#[cfg(test)]
mod tests {
    use crate::frontend_impl::types::{Type, TypeDefs};

    #[test]
    fn test_iterative_box_choice() {
        let typ = Type::iterative_box_choice(
            None,
            vec![("method1", Type::string()), ("method2", Type::int())],
        );

        match typ {
            Type::Iterative { body, .. } => match body.as_ref() {
                Type::Box(_, inner) => match inner.as_ref() {
                    Type::Choice(_, branches) => {
                        assert_eq!(branches.len(), 2);
                        assert!(branches.contains_key(
                            &crate::frontend_impl::language::LocalName {
                                span: crate::location::Span::None,
                                string: arcstr::ArcStr::from("method1"),
                            }
                        ));
                        assert!(branches.contains_key(
                            &crate::frontend_impl::language::LocalName {
                                span: crate::location::Span::None,
                                string: arcstr::ArcStr::from("method2"),
                            }
                        ));
                    }
                    _ => panic!("Expected Choice type inside Box"),
                },
                _ => panic!("Expected Box type"),
            },
            _ => panic!("Expected Iterative type"),
        }
    }

    #[test]
    fn test_iterative_box_choice_with_label() {
        let typ = Type::iterative_box_choice(
            Some("my_label"),
            vec![("action", Type::function(Type::nat(), Type::break_()))],
        );

        match typ {
            Type::Iterative { label, body, .. } => {
                assert!(label.is_some());
                assert_eq!(label.unwrap().string.as_str(), "my_label");

                match body.as_ref() {
                    Type::Box(_, inner) => match inner.as_ref() {
                        Type::Choice(_, branches) => {
                            assert_eq!(branches.len(), 1);
                        }
                        _ => panic!("Expected Choice type inside Box"),
                    },
                    _ => panic!("Expected Box type"),
                }
            }
            _ => panic!("Expected Iterative type"),
        }
    }

    #[test]
    fn test_iterative_box_choice_equivalent_to_manual() {
        let manual = Type::iterative(
            None,
            Type::box_(Type::choice(vec![("test", Type::string())])),
        );

        let helper = Type::iterative_box_choice(None, vec![("test", Type::string())]);

        match (manual, helper) {
            (Type::Iterative { body: body1, .. }, Type::Iterative { body: body2, .. }) => {
                match (body1.as_ref(), body2.as_ref()) {
                    (Type::Box(_, inner1), Type::Box(_, inner2)) => {
                        match (inner1.as_ref(), inner2.as_ref()) {
                            (Type::Choice(_, branches1), Type::Choice(_, branches2)) => {
                                assert_eq!(branches1.len(), branches2.len());
                            }
                            _ => panic!("Expected Choice types"),
                        }
                    }
                    _ => panic!("Expected Box types"),
                }
            }
            _ => panic!("Expected Iterative types"),
        }
    }

    #[test]
    fn test_empty_either_subtype_of_any() {
        let type_defs = TypeDefs::default();
        let empty_either = Type::either(vec![]);
        let any_type = Type::string();

        assert!(
            empty_either
                .is_assignable_to(&any_type, &type_defs)
                .unwrap()
        );
    }

    #[test]
    fn test_any_subtype_of_empty_choice() {
        let type_defs = TypeDefs::default();
        let any_type = Type::int();
        let empty_choice = Type::choice(vec![]);

        assert!(
            any_type
                .is_assignable_to(&empty_choice, &type_defs)
                .unwrap()
        );
    }
}
