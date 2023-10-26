use core::ffi::CStr;
use libloading::{Library, Symbol};
use std::{
    collections::HashMap,
    path::{Path, PathBuf},
    ptr::{null, slice_from_raw_parts},
};

#[repr(C)]
struct Value {
    value: i64,
}

trait Sentinel {
    fn is_sentinel(&self) -> bool;
}

#[derive(Debug, PartialEq)]
#[repr(C)]
struct FastFunctionDecl {
    name: &'static CStr,
    fn_ptr: Option<extern "C" fn(&Value) -> Value>,
}

impl FastFunctionDecl {
    fn sentinel() -> Self {
        let name = CStr::from_bytes_with_nul(b"\0").unwrap();
        let fn_ptr = None;
        Self { name, fn_ptr }
    }
}

/// Create a slice from a raw pointer, using a sentinel value to determine the end of the array
///
/// # Safety
///
/// - `data` must be null or point to a C-style span of `T` with alignof T
/// - If `data` is not null
///     - `data + i` must be the sentinel value for some `i` in `0..isize::max`
///     - data pointed to by `data` must have static lifetime
unsafe fn sentinel_array<T: PartialEq>(data: *const T, sentinel: T) -> &'static [T] {
    if data.is_null() {
        panic!("Attempt to create array from null raw array pointer.");
    }
    for len in 0.. {
        if unsafe { &*data.offset(len) } == &sentinel {
            return unsafe { &*slice_from_raw_parts(data, len as usize) };
        }
    }
    panic!("Raw array pointer lacks sentinel, which is undefined behavior.");
}

#[derive(Clone, Debug)]
pub struct Module {
    map: HashMap<String, extern "C" fn(&Value) -> Value>,
}

impl Module {
    fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }

    fn visit(name: &str, path: &Path) -> Option<Module> {
        if path.is_dir() {
            for entry in path.read_dir().unwrap() {
                if let Some(module) = Self::visit(name, entry.unwrap().path().as_path()) {
                    return Some(module);
                }
            }
            None
        } else if path.ends_with(name) {
            let _path = path.canonicalize().unwrap();
            let mut module = Module::new();
            let fast_function_table = unsafe {
                let lib = Library::new(path).ok()?;
                let fast_function_table: Symbol<*mut FastFunctionDecl> =
                    lib.get("fast_function_table".as_bytes()).ok()?;
                sentinel_array(*fast_function_table, FastFunctionDecl::sentinel())
            };
            for decl in fast_function_table {
                module.map.insert(
                    decl.name.to_str().unwrap().to_string(),
                    decl.fn_ptr.unwrap(),
                );
            }
            Some(module)
        } else {
            None
        }
    }

    pub fn find(
        name: &str,
        module_path: &Path,
        include_paths: &Vec<PathBuf>,
        fast_path: &Vec<PathBuf>,
    ) -> Option<Module> {
        if let Some(module) = Self::visit(name, module_path) {
            return Some(module);
        }
        for path in include_paths {
            if let Some(module) = Self::visit(name, path.as_path()) {
                return Some(module);
            }
        }
        for path in fast_path {
            if let Some(module) = Self::visit(name, path.as_path()) {
                return Some(module);
            }
        }
        None
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_fast_function_decl_eq() {
        assert_eq!(FastFunctionDecl::sentinel(), FastFunctionDecl::sentinel());
    }

    #[test]
    fn test_sentinel_array() {
        let x = [1, 2, 3, 0];
        let x1 = x.as_slice().as_ptr();
        assert_eq!(unsafe { sentinel_array(x1, 0) }, &x[..3]);
    }

    #[test]
    #[should_panic]
    fn test_sentinel_array_null() {
        unsafe { sentinel_array(null::<i64>(), 0) };
    }
}
