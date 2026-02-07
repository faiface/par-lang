use std::{
    io::{stdin, stdout, Write},
    sync::Arc,
};

use arcstr::literal;

use par_core::{
    frontend::{process, Definition, Module, ParString, Type},
    runtime::Handle,
};

pub(super) fn external_module() -> Module<Arc<process::Expression<()>>> {
    Module {
        type_defs: vec![],
        declarations: vec![],
        definitions: vec![Definition::external(
            "Open",
            Type::name(None, "Console", vec![]),
            |handle| Box::pin(console_open(handle)),
        )],
    }
}

async fn console_open(mut handle: Handle) {
    loop {
        match handle.case().await.as_str() {
            "close" => {
                handle.break_();
                break;
            }

            "print" => {
                println!("{}", handle.receive().string().await.as_str(),);
            }

            "prompt" => {
                let prompt = handle.receive().string().await;
                print!("{}", prompt.as_str());
                let _ = stdout().flush();
                let mut buf = String::new();
                let result = stdin().read_line(&mut buf);

                handle.send().concurrently(|mut handle| async move {
                    match result {
                        Ok(n) if n > 0 => {
                            let string = ParString::copy_from_slice(
                                buf.trim_end_matches(&['\n', '\r']).as_bytes(),
                            );
                            handle.signal(literal!("ok"));
                            handle.provide_string(string);
                        }
                        _ => {
                            handle.signal(literal!("err"));
                            handle.break_();
                        }
                    }
                });
            }
            _ => unreachable!(),
        }
    }
}
