use std::rc::Rc;
use std::cell::RefCell;

use std::collections::HashMap;

use super::objects::EvalObject;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Environment {
    pub store: HashMap<String, EvalObject>,
    pub outer: Option<Rc<RefCell<Environment>>>
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            store: HashMap::new(),
            outer: None,
        }
    }

    pub fn get(&self, name: &String) -> Option<EvalObject> {
        match self.store.get(name) {
            Some(value) => Some(value.clone()),
            None => match self.outer {
                Some(ref value) => {
                    value.borrow_mut().get(name)
                },
                None => None,
            }
        }
    }

    pub fn set(&mut self, name: String, value: EvalObject) {
        self.store.insert(name, value);
    }
}

