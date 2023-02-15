use std::borrow::Borrow;

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
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct EnvVec<K, V>(Vec<(K, V)>);

impl<K, V> EnvVec<K, V> {
    pub(crate) fn new() -> Self {
        Self(Vec::new())
    }
}

impl<K: Clone, V: Clone> Env<K, V> for EnvVec<K, V> {
    fn insert(&mut self, key: K, value: V) {
        self.0.push((key, value));
    }

    fn get<Q>(&self, key: &Q) -> &V
    where
        Q: ?Sized,
        K: Borrow<Q> + PartialEq<Q>,
    {
        self.0
            .iter()
            .rev()
            .find(|(k, _)| k == key)
            .map(|(_, v)| v)
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
            .find(|(k, _)| k == key)
            .map(|(_, v)| v)
            .expect("Attempt to access undeclared variable.")
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
}
