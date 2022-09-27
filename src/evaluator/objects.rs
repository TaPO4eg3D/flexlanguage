use std::fmt::{Display, Formatter, Result};

pub enum EvalObject {
    Int(i32),
    Boolean(bool),
    Null,
}

impl Display for EvalObject {
   fn fmt(&self, f: &mut Formatter<'_>) -> Result {
       match self {
           EvalObject::Int(i) => {
               write!(f, "{}", i)
           },
           EvalObject::Boolean(b) => {
               write!(f, "{}", b)
           },
           _ => unimplemented!()
       }
    }
}
