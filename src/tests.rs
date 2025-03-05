fn test_equivalence(
    code: &str,
    equal_names: &[(bool, (&str, Option<&str>), (&str, Option<&str>))],
    tree_eq: &[(bool, (&str, Option<&str>), (&str))],
) -> Result<(), ()> {
    let mut code = String::from(code);
    for (idx, (_, (l, lt), (r, rt))) in equal_names.iter().enumerate() {
        if let Some(lt) = lt {
            code.push_str(&format!("declare left_{idx} : {lt}\n"));
        }
        if let Some(rt) = rt {
            code.push_str(&format!("declare right_{idx} : {rt}\n"));
        }
        code.push_str(&format!(
            "define left_{idx} = {l}\n define right_{idx} = {r}\n"
        ));
    }
    for (idx, (_, (l, lt), _)) in tree_eq.iter().enumerate() {
        if let Some(lt) = lt {
            code.push_str(&format!("declare t_{idx} : {lt}\n"));
        }
        code.push_str(&format!("define t_{idx} = {l}\n"));
    }
    let program = crate::playground::Compiled::from_string(&code);
    let program = match program {
        Ok(program) => program,
        Err(e) => {
            println!("{}", e.display(&code));
            return Err(());
        }
    };
    let ic_compiled = program.checked.unwrap().ic_compiled.unwrap();
    //println!("{}", ic_compiled);
    for (idx, (should_check, (l, _), (r, _))) in equal_names.iter().enumerate() {
        let left = ic_compiled
            .get_with_name(&format!("left_{idx}").into())
            .unwrap();
        let right = ic_compiled
            .get_with_name(&format!("right_{idx}").into())
            .unwrap();
        let mut net = ic_compiled.create_net();
        let ls = left.show();
        let rs = right.show();
        if *should_check != crate::icombs::are_equivalent(&mut net, left, right) {
            eprintln!(
                "error when checking {l} {} {r}.\n\tLHS: {ls}\n\tRHS: {rs}",
                if *should_check { "==" } else { "!=" },
            )
        }
    }

    for (idx, (should_check, (l, lt), r)) in tree_eq.iter().enumerate() {
        let left = ic_compiled
            .get_with_name(&format!("t_{idx}").into())
            .unwrap();
        let right = crate::icombs::parse::parse_net(r)
            .unwrap()
            .ports
            .pop_back()
            .unwrap();
        let mut net = ic_compiled.create_net();
        let ls = left.show();
        if *should_check != crate::icombs::are_equivalent(&mut net, left, right) {
            eprintln!(
                "error when checking {l} {} {r}.\n\tLHS: {ls}",
                if *should_check { "==" } else { "!=" },
            )
        }
    }
    Ok(())
}

#[test]
fn main() {
    test_equivalence(
        r#"
type Bool = either { .true! .false! }
declare true: Bool
declare false: Bool
declare unit: either { .unit! }
define true = .true!
define false = .false!
define unit = .unit!
declare not: [Bool] Bool
define not = [x: Bool] x {
  true? => .false!
  false? => .true!
}
declare not_comm: [Bool] Bool
define not_comm = [x] x {
  false? => .true!
  true? => .false!
}

declare bool_id: [Bool] Bool
define bool_id = [x: Bool] x {
  true? => .true!
  false? => .false!
}

declare compose: [[Bool] Bool] [[Bool] Bool] [Bool] Bool
define compose = [f] [g] [x] f(g(x))

  "#,
        &[
            (true, ("true", None), ("true", None)),
            (false, ("true", None), ("false", None)),
            (true, ("not", None), ("not_comm", None)),
            (true, ("not(true)", None), ("false", None)),
            (true, ("true", None), ("compose(not)(not)(true)", None)),
        ],
        &[
            (true, ("unit", None), "(a (a *))"),
            (true, ("true", None), "(a (* (a *)))"),
            (true, ("false", None), "(a ((a *) *))"),
            (true, ("{ unit => ! }", None), "(a ((a *) *))"),
            (true, ("not", None), "(* *)"),
        ],
    )
    .unwrap()
}
