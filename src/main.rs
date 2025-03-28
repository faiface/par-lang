use std::sync::Arc;
use eframe::egui;
use crate::interact::Handle;
use crate::par::language::Definition;
use crate::par::runtime::Context;
use crate::playground::Compiled;
use crate::spawn::TokioSpawn;

mod interact;
mod par;
mod playground;
mod spawn;
mod language_server;
mod location;
/*
#[tokio::main]
async fn main() {
    let options = eframe::NativeOptions {
        viewport: egui::ViewportBuilder::default().with_inner_size([1000.0, 700.0]),
        ..Default::default()
    };

    par::parse::set_miette_hook();

    eframe::run_native(
        "⅋layground",
        options,
        Box::new(|cc| Ok(Playground::new(cc))),
    )
    .expect("egui crashed");
}*/


fn main() {
    language_server::language_server_main::main();
}



/*fn main() {
    let mut interact: Option<playground::Interact> = None;

    let code = "def main = { .interact => ! }";
    let Ok(compiled) = stacker::grow(32 * 1024 * 1024, || {
        Compiled::from_string(code)
    }) else {
        println!("Compilation failed");
        return;
    };
    let compiled_code = code.into();
    let program = compiled.program.clone();
    let definition = program.definitions[0].clone();

    let runtime = tokio::runtime::Runtime::new().unwrap();
    runtime.block_on(async {
        let options = eframe::NativeOptions {
            viewport: egui::ViewportBuilder::default().with_inner_size([1000.0, 700.0]),
            //run_and_return: false,
            ..Default::default()
        };

        eframe::run_simple_native(
            "⅋layground",
            options,
            move |ctx, _frame| {
                egui::CentralPanel::default().show(ctx, |ui| {
                    let name = definition.name.to_string();
                    if ui.button(&name).clicked() {
                        if let Some(int) = interact.take() {
                            int.handle.lock().expect("lock failed").cancel();
                        }
                        interact = Some(playground::Interact {
                            code: Arc::clone(&compiled_code),
                            handle: Handle::start_expression(
                                Arc::new({
                                    let ctx = ui.ctx().clone();
                                    move || ctx.request_repaint()
                                }),
                                Context::new(
                                    Arc::new(TokioSpawn),
                                    Arc::new(
                                        program
                                            .definitions
                                            .iter()
                                            .map(|Definition { name, expression, .. }| (name.clone(), expression.clone()))
                                            .collect(),
                                    ),
                                ),
                                &definition.expression,
                            ),
                        });
                    }
                });
            }
            // 417, 339, 508
        )
            .expect("egui crashed");
    })
}*/