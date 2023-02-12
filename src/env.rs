use std::borrow::Borrow;

pub(crate) trait Env<K, V>: Clone {
    fn insert(&self, key: K, value: V) -> Self;
    fn get<Q>(&self, key: &Q) -> &V
    where
        Q: ?Sized,
        K: Borrow<Q> + PartialEq<Q>;
    fn get_mut<Q>(&mut self, key: &Q) -> &mut V
    where
        Q: ?Sized,
        K: Borrow<Q> + PartialEq<Q>;
}

impl<K: Clone, V: Clone> Env<K, V> for Vec<(K, V)> {
    fn insert(&self, key: K, value: V) -> Self {
        let mut out = self.clone();
        out.push((key, value));
        out
    }

    fn get<Q>(&self, key: &Q) -> &V
    where
        Q: ?Sized,
        K: Borrow<Q> + PartialEq<Q>,
    {
        self.iter()
            .find(|(k, _)| k == key)
            .map(|(_, v)| v)
            .expect("Attempt to access undeclared variable.")
    }

    fn get_mut<Q>(&mut self, key: &Q) -> &mut V
    where
        Q: ?Sized,
        K: Borrow<Q> + PartialEq<Q>,
    {
        self.iter_mut()
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
        let env = Vec::<(String, isize)>::new();
        env.get("Hello");
    }

    #[test]
    fn test_get() {
        let env = Vec::new();
        let env = env.insert("x".to_string(), 1);
        assert_eq!(*env.get("x"), 1);
    }

    #[test]
    fn test1() {
        let mut env = Vec::new().insert("x".to_string(), 1);
        *env.get_mut("x") = 2;
        assert_eq!(*env.get("x"), 2);
    }
}
