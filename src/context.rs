use std::{ops::Range, path::PathBuf, collections::HashMap};

use string_interner::{StringInterner, symbol::SymbolU32};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Location {
    source: SourceIx,
    start: usize,
    end: usize,
}

impl From<Location> for Range<usize> {
    fn from(location: Location) -> Self {
        location.start..location.end
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct SourceIx(usize);

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Context {
    store: StringInterner,
    files: HashMap<PathBuf, SourceIx>,
    sources: Vec<String>,
}

pub type Symbol = SymbolU32;

impl Context {
    pub fn new() -> Self {
        let store = StringInterner::default();
        let files = HashMap::new();
        let sources = Vec::new();
        Self { store, files, sources }
    }

    fn insert_source(&mut self, string: String) -> SourceIx {
        let ix = SourceIx(self.sources.len());
        self.sources.push(string);
        ix
    }

    pub fn location(&self, location: Location) -> &str {
        &(&self.sources[location.source.0])[Range::from(location)]
    }

    pub fn file(&mut self, path: &str) -> SourceIx {
        let path = PathBuf::from(path);
        match self.files.get(&path) {
            None => {
                let error = format!("Failed to open file: {:?}", &path);
                let string = std::fs::read_to_string(&path).expect(error.as_str());
                let ix = self.insert_source(string);
                self.files.insert(path, ix);
                ix
            }
            Some(source) => *source,
        }
    }

    pub fn repl(&mut self) -> SourceIx {
        let mut string = String::new();
        std::io::stdin().read_line(&mut string).expect("Failed to read from stdin");
        self.insert_source(string)
    }

    pub fn store(&mut self, location: Location) -> Symbol {
        let string = self.location(location);
        // TODO: Find a way to eliminate the extra allocation
        self.store.get_or_intern(string.to_string())
    }
}