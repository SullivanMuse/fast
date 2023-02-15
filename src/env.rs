use std::borrow::Borrow;
use vec1::Vec1;

pub(crate) trait Env<K, V>: Clone {
    fn insert(&mut self, key: K, value: V);
    fn get<Q>(&self, key: &Q) -> &V
    where
        Q: ?Sized,
        K: Borrow<Q> + PartialEq<Q>;
    fn get_mut<Q>(&mut self, key: &Q) -> &mut V
    where
        Q: ?Sized,
        K: Borrow<Q> + PartialEq<Q>;
    fn push(&mut self);
    fn pop(&mut self);
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct EnvVec<K, V>(Vec1<Vec<(K, V)>>);

impl<K, V> EnvVec<K, V> {
    pub(crate) fn new() -> Self {
        Self(Vec1::new(Vec::new()))
    }
}

impl<K: Clone, V: Clone> Env<K, V> for EnvVec<K, V> {
    fn insert(&mut self, key: K, value: V) {
        self.0.last_mut().push((key, value));
    }

    fn get<Q>(&self, key: &Q) -> &V
    where
        Q: ?Sized,
        K: Borrow<Q> + PartialEq<Q>,
    {
        self.0
            .iter()
            .rev()
            .find_map(|v| v.iter().rev().find(|(k, _)| k == key).map(|(_, v)| v))
            .expect("Attempt to access undeclared variable.")
    }

    fn get_mut<Q>(&mut self, key: &Q) -> &mut V
    where
        Q: ?Sized,
        K: Borrow<Q> + PartialEq<Q>,
    {
        self.0
            .iter_mut()
            .rev()
            .find_map(|v| v.iter_mut().rev().find(|(k, _)| k == key).map(|(_, v)| v))
            .expect("Attempt to access undeclared variable.")
    }

    fn push(&mut self) {
        self.0.push(Vec::new())
    }

    fn pop(&mut self) {
        self.0.pop().expect("More pops than pushes.");
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    #[should_panic]
    fn test_env() {
        let env = EnvVec::<String, usize>::new();
        env.get("Hello");
    }

    #[test]
    fn test_get() {
        let mut env = EnvVec::<String, usize>::new();
        env.insert("x".to_string(), 1);
        assert_eq!(*env.get("x"), 1);
    }

    #[test]
    fn test1() {
        let mut env = EnvVec::new();
        env.insert("x".to_string(), 1);
        *env.get_mut("x") = 2;
        assert_eq!(*env.get("x"), 2);
    }

    #[test]
    fn test_push_pop() {
        let mut env = EnvVec::new();
        env.insert("x".to_string(), 1);
        env.push();
        env.insert("x".to_string(), 3);
        *env.get_mut("x") = 2;
        assert_eq!(*env.get("x"), 2);
        env.pop();
        assert_eq!(*env.get("x"), 1);
    }
}
