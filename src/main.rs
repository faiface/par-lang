use eframe::egui;
use playground::Playground;

mod interact;
mod par;
mod playground;
mod spawn;

#[tokio::main]
async fn main() {
    let options = eframe::NativeOptions {
        viewport: egui::ViewportBuilder::default().with_inner_size([1000.0, 700.0]),
        ..Default::default()
    };

    par::parser::set_miette_hook();

    eframe::run_native(
        "⅋layground",
        options,
        Box::new(|cc| Ok(Playground::new(cc))),
    )
    .expect("egui crashed");
}
