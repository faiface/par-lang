use std::{
    io::{stdin, stdout, Write},
    sync::Arc,
};

use arcstr::{literal, Substr};

use crate::{
    icombs::readback::Handle,
    par::{
        process,
        program::{Definition, Module},
        types::Type,
    },
};

pub fn external_module() -> Module<Arc<process::Expression<()>>> {
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
                println!("{}", handle.receive().string().await);
            }

            "prompt" => {
                let prompt = handle.receive().string().await;
                print!("{}", prompt);
                let _ = stdout().flush();
                let mut buf = String::new();
                let result = stdin().read_line(&mut buf);

                handle.send().concurrently(|mut handle| async move {
                    match result {
                        Ok(_) => {
                            let string = Substr::from(buf.trim_end_matches(&['\n', '\r']));
                            handle.signal(literal!("ok"));
                            handle.provide_string(string);
                        }
                        Err(_) => {
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
