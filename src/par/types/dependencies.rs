use super::super::language::GlobalName;
use super::core::Type;

impl Type {
    pub fn get_dependencies(&self) -> Vec<GlobalName> {
        match self {
            Self::Primitive(_, _) | Self::DualPrimitive(_, _) => vec![],
            Self::Var(_, _) | Self::DualVar(_, _) => vec![],
            Self::Name(_, name, args) | Self::DualName(_, name, args) => {
                let mut deps = vec![name.clone()];
                for arg in args {
                    deps.extend(arg.get_dependencies());
                }
                deps
            }
            Self::Box(_, body) | Self::DualBox(_, body) => body.get_dependencies(),
            Self::Pair(_, t, u) => {
                let mut deps = t.get_dependencies();
                deps.extend(u.get_dependencies());
                deps
            }
            Self::Function(_, t, u) => {
                let mut deps = t.get_dependencies();
                deps.extend(u.get_dependencies());
                deps
            }
            Self::Either(_, branches) => branches
                .iter()
                .flat_map(|(_, typ)| typ.get_dependencies())
                .collect(),
            Self::Choice(_, branches) => branches
                .iter()
                .flat_map(|(_, typ)| typ.get_dependencies())
                .collect(),
            Self::Break(_) => vec![],
            Self::Continue(_) => vec![],
            Self::Recursive { body, .. } => body.get_dependencies(),
            Self::Iterative { body, .. } => body.get_dependencies(),
            Self::Self_(_, _) | Self::DualSelf(_, _) => vec![],
            Self::Exists(_, _, body) => body.get_dependencies(),
            Self::Forall(_, _, body) => body.get_dependencies(),
        }
    }
}
