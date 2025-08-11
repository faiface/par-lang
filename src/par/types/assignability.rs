use crate::location::Span;
use crate::par::language::LocalName;
use crate::par::types::{PrimitiveType, Type, TypeDefs, TypeError};
use std::collections::{BTreeMap, HashMap, HashSet};
use std::rc::Rc;

#[derive(Debug)]
enum FixPointType {
    Recursive,
    Iterative,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
enum Path {
    Empty,
    Node(usize, Rc<Path>),
    NamedNode(LocalName, Rc<Path>),
}
impl Path {
    fn add(&self, index: usize) -> Self {
        Path::Node(index, Rc::new(self.clone()))
    }

    fn add_name(&self, name: LocalName) -> Self {
        Path::NamedNode(name, Rc::new(self.clone()))
    }
}
#[derive(Clone, Debug)]
struct LabelsMap(HashMap<Option<LocalName>, Rc<(Path, Type, FixPointType, LabelsMap)>>);

impl Type {
    pub fn check_assignable(
        &self,
        span: &Span,
        u: &Type,
        type_defs: &TypeDefs,
    ) -> Result<(), TypeError> {
        if !self.is_assignable_to(u, type_defs)? {
            return Err(TypeError::CannotAssignFromTo(
                span.clone(),
                self.clone(),
                u.clone(),
            ));
        }
        Ok(())
    }

    pub fn is_assignable_to(&self, other: &Self, type_defs: &TypeDefs) -> Result<bool, TypeError> {
        self.is_assignable_to_internal(
            other,
            type_defs,
            Path::Empty,
            Path::Empty,
            &mut Default::default(),
            LabelsMap(HashMap::new()),
            LabelsMap(HashMap::new()),
            HashSet::new(),
        )
    }

    /**
    This function checks if `self` <: `other`.

     This algorithm is a generalization of the algorithm presented in `Subtyping recursive types (1993)`,
     to support both least and greatest fixpoint types.

     We believe it to be both complete and sound
     with respect to their definition as an infinite union/intersection respectively.
    */
    fn is_assignable_to_internal(
        &self,
        other: &Self,
        type_defs: &TypeDefs,
        self_path: Path,
        other_path: Path,
        visited: &mut HashSet<(Path, Path)>,
        mut self_labels: LabelsMap,
        mut other_labels: LabelsMap,
        mut cyclic_points: HashSet<(Path, Path)>,
    ) -> Result<bool, TypeError> {
        Ok(match (self, other) {
            (Self::Self_(_, label1), Self::Self_(_, label2)) => {
                let (self_path, self_type, self_fixpoint_type, self_labels) = self_labels
                    .0
                    .get(label1)
                    .expect("Self label not found")
                    .as_ref();
                let (other_path, other_type, other_fixpoint_type, other_labels) = other_labels
                    .0
                    .get(label2)
                    .expect("Other label not found")
                    .as_ref();

                if visited.contains(&(self_path.clone(), other_path.clone())) {
                    if let (FixPointType::Recursive, _) | (_, FixPointType::Iterative) =
                        (self_fixpoint_type, other_fixpoint_type)
                    {
                        return Ok(true);
                    } else {
                        if cyclic_points.contains(&(self_path.clone(), other_path.clone())) {
                            return Ok(false);
                        } else {
                            cyclic_points.insert((self_path.clone(), other_path.clone()));
                        }
                    }
                }
                self_type.is_assignable_to_internal(
                    &other_type,
                    type_defs,
                    self_path.clone(),
                    other_path.clone(),
                    visited,
                    self_labels.clone(),
                    other_labels.clone(),
                    cyclic_points,
                )?
            }

            (Self::Self_(_, label1), t2) => {
                if !self_labels.0.contains_key(label1) {
                    return Ok(false);
                }

                let (self_path, self_type, self_fixpoint_type, self_labels) = self_labels
                    .0
                    .get(label1)
                    .expect("Self label not found")
                    .as_ref();

                if visited.contains(&(self_path.clone(), other_path.clone())) {
                    if let FixPointType::Recursive = self_fixpoint_type {
                        return Ok(true);
                    } else {
                        if cyclic_points.contains(&(self_path.clone(), other_path.clone())) {
                            return Ok(false);
                        } else {
                            cyclic_points.insert((self_path.clone(), other_path.clone()));
                        }
                    }
                }
                self_type.is_assignable_to_internal(
                    t2,
                    type_defs,
                    self_path.clone(),
                    other_path.clone(),
                    visited,
                    self_labels.clone(),
                    other_labels.clone(),
                    cyclic_points,
                )?
            }

            (t1, Self::Self_(_, label2)) => {
                if !self_labels.0.contains_key(label2) {
                    return Ok(false);
                }

                let (other_path, other_type, other_fixpoint_type, other_labels) = other_labels
                    .0
                    .get(label2)
                    .expect("Other label not found")
                    .as_ref();

                if visited.contains(&(self_path.clone(), other_path.clone())) {
                    if let FixPointType::Iterative = other_fixpoint_type {
                        return Ok(true);
                    } else {
                        if cyclic_points.contains(&(self_path.clone(), other_path.clone())) {
                            return Ok(false);
                        } else {
                            cyclic_points.insert((self_path.clone(), other_path.clone()));
                        }
                    }
                }
                t1.is_assignable_to_internal(
                    &other_type,
                    type_defs,
                    self_path.clone(),
                    other_path.clone(),
                    visited,
                    self_labels.clone(),
                    other_labels.clone(),
                    cyclic_points,
                )?
            }
            (Self::Primitive(_, PrimitiveType::Nat), Self::Primitive(_, PrimitiveType::Int)) => {
                true
            }
            (Self::Primitive(_, PrimitiveType::Byte), Self::Primitive(_, PrimitiveType::Bytes)) => {
                true
            }
            (Self::Primitive(_, p1), Self::Primitive(_, p2)) => p1 == p2,
            (
                Self::DualPrimitive(_, PrimitiveType::Int),
                Self::DualPrimitive(_, PrimitiveType::Nat),
            ) => true,
            (
                Self::DualPrimitive(_, PrimitiveType::Bytes),
                Self::DualPrimitive(_, PrimitiveType::Byte),
            ) => true,
            (Self::DualPrimitive(_, p1), Self::DualPrimitive(_, p2)) => p1 == p2,

            (Self::Var(_, name1), Self::Var(_, name2)) => name1 == name2,
            (Self::DualVar(_, name1), Self::DualVar(_, name2)) => name1 == name2,
            (Self::Name(span, name, args), t2) => {
                type_defs.get(span, name, args)?.is_assignable_to_internal(
                    t2,
                    type_defs,
                    self_path,
                    other_path,
                    visited,
                    self_labels,
                    other_labels,
                    cyclic_points,
                )?
            }
            (t1, Self::Name(span, name, args)) => t1.is_assignable_to_internal(
                &type_defs.get(span, name, args)?,
                type_defs,
                self_path,
                other_path,
                visited,
                self_labels,
                other_labels,
                cyclic_points,
            )?,
            (Self::DualName(span, name, args), t2) => type_defs
                .get_dual(span, name, args)?
                .is_assignable_to_internal(
                    t2,
                    type_defs,
                    self_path,
                    other_path,
                    visited,
                    self_labels,
                    other_labels,
                    cyclic_points,
                )?,
            (t1, Self::DualName(span, name, args)) => t1.is_assignable_to_internal(
                &type_defs.get_dual(span, name, args)?,
                type_defs,
                self_path,
                other_path,
                visited,
                self_labels,
                other_labels,
                cyclic_points,
            )?,

            (t1, Self::Box(_, t2)) if t1.is_positive(type_defs)? => t1.is_assignable_to_internal(
                t2,
                type_defs,
                self_path,
                other_path.add(0),
                visited,
                self_labels,
                other_labels,
                cyclic_points,
            )?,
            (Self::DualBox(_, t1), t2) if t1.is_positive(type_defs)? => {
                t1.clone().dual(Span::None).is_assignable_to_internal(
                    t2,
                    type_defs,
                    self_path.add(0),
                    other_path,
                    visited,
                    self_labels,
                    other_labels,
                    cyclic_points,
                )?
            }
            (Self::Box(_, t1), Self::Box(_, t2)) => t1.is_assignable_to_internal(
                t2,
                type_defs,
                self_path.add(0),
                other_path.add(0),
                visited,
                self_labels,
                other_labels,
                cyclic_points,
            )?,
            (Self::Box(_, t1), t2) => t1.is_assignable_to_internal(
                t2,
                type_defs,
                self_path.add(0),
                other_path.add(0),
                visited,
                self_labels,
                other_labels,
                cyclic_points,
            )?,
            (Self::DualBox(_, t1), Self::DualBox(_, t2)) => {
                let t1 = t1.clone().dual(Span::None);
                let t2 = t2.clone().dual(Span::None);
                t1.is_assignable_to_internal(
                    &t2,
                    type_defs,
                    self_path.add(0),
                    other_path.add(0),
                    visited,
                    self_labels,
                    other_labels,
                    cyclic_points,
                )?
            }
            (t1, Self::DualBox(_, t2)) => {
                let t2 = t2.clone().dual(Span::None);
                t1.is_assignable_to_internal(
                    &t2,
                    type_defs,
                    self_path,
                    other_path.add(0),
                    visited,
                    self_labels,
                    other_labels,
                    cyclic_points,
                )?
            }

            (Self::Pair(_, t1, u1), Self::Pair(_, t2, u2)) => {
                t1.is_assignable_to_internal(
                    t2,
                    type_defs,
                    self_path.add(0),
                    other_path.add(0),
                    visited,
                    self_labels.clone(),
                    other_labels.clone(),
                    cyclic_points.clone(),
                )? && u1.is_assignable_to_internal(
                    u2,
                    type_defs,
                    self_path.add(1),
                    other_path.add(1),
                    visited,
                    self_labels,
                    other_labels,
                    cyclic_points,
                )?
            }
            (Self::Function(_, t1, u1), Self::Function(_, t2, u2)) => {
                let t1 = t1.clone().dual(Span::None);
                let t2 = t2.clone().dual(Span::None);
                t1.is_assignable_to_internal(
                    &t2,
                    type_defs,
                    self_path.add(0),
                    other_path.add(0),
                    visited,
                    self_labels.clone(),
                    other_labels.clone(),
                    cyclic_points.clone(),
                )? && u1.is_assignable_to_internal(
                    u2,
                    type_defs,
                    self_path.add(1),
                    other_path.add(1),
                    visited,
                    self_labels,
                    other_labels,
                    cyclic_points,
                )?
            }
            (Self::Either(_, branches1), Self::Either(_, branches2)) => {
                for (branch, t1) in branches1 {
                    let Some(t2) = branches2.get(branch) else {
                        return Ok(false);
                    };
                    if !t1.is_assignable_to_internal(
                        t2,
                        type_defs,
                        self_path.add_name(branch.clone()),
                        other_path.add_name(branch.clone()),
                        visited,
                        self_labels.clone(),
                        other_labels.clone(),
                        cyclic_points.clone(),
                    )? {
                        return Ok(false);
                    }
                }
                true
            }
            (Self::Choice(_, branches1), Self::Choice(_, branches2)) => {
                for (branch, t2) in branches2 {
                    let Some(t1) = branches1.get(branch) else {
                        return Ok(false);
                    };
                    if !t1.is_assignable_to_internal(
                        t2,
                        type_defs,
                        self_path.add_name(branch.clone()),
                        other_path.add_name(branch.clone()),
                        visited,
                        self_labels.clone(),
                        other_labels.clone(),
                        cyclic_points.clone(),
                    )? {
                        return Ok(false);
                    }
                }
                true
            }
            (Self::Break(_), Self::Break(_)) => true,
            (Self::Continue(_), Self::Continue(_)) => true,
            (
                typ,
                t2 @ Self::Recursive {
                    asc: asc2,
                    label,
                    body,
                    ..
                },
            ) => {
                if !asc2.is_empty() {
                    if let Self::Recursive { asc: asc1, .. } = typ {
                        if !asc2.is_subset(asc1) {
                            return Ok(false);
                        }
                    } else {
                        return Ok(false);
                    }
                }
                visited.insert((self_path.clone(), other_path.clone()));
                other_labels.0.insert(
                    label.clone(),
                    Rc::new((
                        other_path.clone(),
                        t2.clone(),
                        FixPointType::Recursive,
                        other_labels.clone(),
                    )),
                );
                typ.is_assignable_to_internal(
                    body,
                    type_defs,
                    self_path,
                    other_path.add(0),
                    visited,
                    self_labels,
                    other_labels,
                    cyclic_points,
                )?
            }

            (t1 @ Self::Recursive { label, body, .. }, typ) => {
                visited.insert((self_path.clone(), other_path.clone()));
                self_labels.0.insert(
                    label.clone(),
                    Rc::new((
                        self_path.clone(),
                        t1.clone(),
                        FixPointType::Recursive,
                        self_labels.clone(),
                    )),
                );
                body.is_assignable_to_internal(
                    typ,
                    type_defs,
                    self_path.add(0),
                    other_path,
                    visited,
                    self_labels,
                    other_labels,
                    cyclic_points,
                )?
            }
            (
                t1 @ Self::Iterative {
                    asc: asc1,
                    label,
                    body,
                    ..
                },
                typ,
            ) => {
                if !asc1.is_empty() {
                    if let Self::Iterative { asc: asc2, .. } = typ {
                        if !asc1.is_subset(asc2) {
                            return Ok(false);
                        }
                    } else {
                        return Ok(false);
                    }
                }
                visited.insert((self_path.clone(), other_path.clone()));
                self_labels.0.insert(
                    label.clone(),
                    Rc::new((
                        self_path.clone(),
                        t1.clone(),
                        FixPointType::Iterative,
                        self_labels.clone(),
                    )),
                );
                body.is_assignable_to_internal(
                    typ,
                    type_defs,
                    self_path.add(0),
                    other_path,
                    visited,
                    self_labels,
                    other_labels,
                    cyclic_points,
                )?
            }
            (typ, t2 @ Self::Iterative { label, body, .. }) => {
                visited.insert((self_path.clone(), other_path.clone()));
                other_labels.0.insert(
                    label.clone(),
                    Rc::new((
                        other_path.clone(),
                        t2.clone(),
                        FixPointType::Iterative,
                        other_labels.clone(),
                    )),
                );
                typ.is_assignable_to_internal(
                    body,
                    type_defs,
                    self_path,
                    other_path.add(0),
                    visited,
                    self_labels,
                    other_labels,
                    cyclic_points,
                )?
            }

            (Self::Exists(loc, name1, body1), Self::Exists(_, name2, body2))
            | (Self::Forall(loc, name1, body1), Self::Forall(_, name2, body2)) => {
                let body2 = body2.clone().substitute(BTreeMap::from([(
                    name2,
                    &Type::Var(loc.clone(), name1.clone()),
                )]))?;
                let mut type_defs = type_defs.clone();
                type_defs.vars.insert(name1.clone());
                body1.is_assignable_to_internal(
                    &body2,
                    &type_defs,
                    self_path.add(0),
                    other_path.add(0),
                    visited,
                    self_labels,
                    other_labels,
                    cyclic_points,
                )?
            }

            _ => false,
        })
    }
}
