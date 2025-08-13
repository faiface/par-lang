use super::super::language::LocalName;
use super::core::Type;
use super::error::TypeError;
use crate::location::Span;
use crate::par::types::visit;
use std::collections::BTreeMap;

impl Type {
    pub fn substitute(self, map: BTreeMap<&LocalName, &Type>) -> Result<Self, TypeError> {
        fn inner(typ: &mut Type, map: &BTreeMap<&LocalName, &Type>) -> Result<(), TypeError> {
            match typ {
                Type::Var(_span, name) if map.contains_key(name) => {
                    *typ = map.get(name).cloned().cloned().unwrap();
                }
                Type::DualVar(_span, name) if map.contains_key(name) => {
                    *typ = map.get(name).cloned().cloned().unwrap().dual(Span::None);
                }
                Type::Exists(_span, name, body) | Type::Forall(_span, name, body) => {
                    let old_name = name.clone();
                    while map.values().any(|t| t.contains_var(&name)) {
                        name.string = arcstr::format!("{}'", name.string);
                    }
                    if &old_name != name {
                        inner(
                            body,
                            &BTreeMap::from([(
                                &old_name,
                                &Type::Var(name.span.clone(), name.clone()),
                            )]),
                        )?;
                    }
                    let mut map = map.clone();
                    map.remove(&old_name);
                    inner(body, &map)?
                }
                _ => {
                    visit::continue_mut(typ, |child: &mut Type| inner(child, map))?;
                }
            }
            Ok(())
        }

        let mut typ = self;
        inner(&mut typ, &map)?;
        Ok(typ)
    }

    pub fn contains_self(&self, label: &Option<LocalName>) -> bool {
        fn inner(result: &mut bool, typ: &Type, target_label: &Option<LocalName>) {
            match typ {
                Type::Self_(_span, label) | Type::DualSelf(_span, label)
                    if label == target_label =>
                {
                    *result = true;
                }
                Type::Recursive { label, .. } | Type::Iterative { label, .. }
                    if label == target_label => {}
                _ => visit::continue_(typ, |child| {
                    inner(result, child, target_label);
                    Ok::<(), ()>(())
                })
                .unwrap(),
            }
        }
        let mut result = false;
        inner(&mut result, self, label);
        result
    }

    pub fn contains_var(&self, var: &LocalName) -> bool {
        fn inner(result: &mut bool, typ: &Type, target_name: &LocalName) -> Result<(), ()> {
            match typ {
                Type::Var(_span, name) | Type::DualVar(_span, name) if name == target_name => {
                    *result = true;
                }
                Type::Forall(_, name, _) | Type::Exists(_, name, _) if name == target_name => {
                    // var is shadowed
                }
                _ => {
                    visit::continue_(typ, |child| inner(result, child, target_name))?;
                }
            }
            Ok(())
        }
        let mut result = false;
        inner(&mut result, self, var).unwrap();
        result
    }
}
