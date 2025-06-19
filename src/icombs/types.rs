use std::sync::Arc;

use arcstr::ArcStr;

use crate::par::types::PrimitiveType;

pub struct RuntimeType {
    inner: Arc<RuntimeTypeInner>,
}

struct RuntimeTypeInner {
    variables: Vec<Self>,
    variant: RuntimeTypeVariant,
}

pub enum RuntimeTypeVariant {
    Break,
    Continue,
    Pair(Arc<Self>, Arc<Self>),
    Par(Arc<Self>, Arc<Self>),
    Either(Box<[(ArcStr, Arc<Self>)]>),
    Choice(Box<[(ArcStr, Arc<Self>)]>),
    Self_(usize),
    Forall(Arc<Self>),
    Exists(Arc<Self>),
    Var(usize),
    ChanVar(usize),
    Primitive(PrimitiveType),
    ChanPrimitive(PrimitiveType),
}
