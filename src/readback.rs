use crate::icombs::{
    readback::{TypedHandle, TypedReadback},
    Net,
};
use arcstr::{ArcStr, Substr};
use core::fmt::Debug;
use eframe::egui::{self, RichText, Ui};
use futures::{
    channel::oneshot,
    task::{Spawn, SpawnExt},
};
use num_bigint::BigInt;
use std::sync::{Arc, Mutex};

enum Request {
    Nat(String, Box<dyn Send + FnOnce(BigInt)>),
    Int(String, Box<dyn Send + FnOnce(BigInt)>),
    String(String, Box<dyn Send + FnOnce(Substr)>),
    Char(String, Box<dyn Send + FnOnce(char)>),
    Choice(Vec<ArcStr>, Box<dyn Send + FnOnce(ArcStr)>),
}

pub enum Event {
    Times(Arc<Mutex<Element>>),
    Par(Arc<Mutex<Element>>),
    Either(ArcStr),
    Choice(ArcStr),
    Break,
    Continue,
    Nat(BigInt),
    NatRequest(BigInt),
    Int(BigInt),
    IntRequest(BigInt),
    String(Substr),
    StringRequest(Substr),
    Char(char),
    CharRequest(char),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Polarity {
    Positive,
    Negative,
}

impl Event {
    fn polarity(&self) -> Polarity {
        match self {
            Self::Times(_) => Polarity::Positive,
            Self::Par(_) => Polarity::Negative,
            Self::Either(_) => Polarity::Positive,
            Self::Choice(_) => Polarity::Negative,
            Self::Break => Polarity::Positive,
            Self::Continue => Polarity::Negative,
            Self::Nat(_) => Polarity::Positive,
            Self::NatRequest(_) => Polarity::Negative,
            Self::Int(_) => Polarity::Positive,
            Self::IntRequest(_) => Polarity::Negative,
            Self::String(_) => Polarity::Positive,
            Self::StringRequest(_) => Polarity::Negative,
            Self::Char(_) => Polarity::Positive,
            Self::CharRequest(_) => Polarity::Negative,
        }
    }
}

pub struct Element {
    history: Vec<Event>,
    request: Option<Request>,
    net: Arc<Mutex<Net>>,
}

impl Element {
    pub fn new(
        refresh: Arc<dyn Fn() + Send + Sync>,
        spawner: Arc<dyn Spawn + Send + Sync>,
        handle: TypedHandle,
    ) -> Arc<Mutex<Self>> {
        let element = Arc::new(Mutex::new(Self {
            history: vec![],
            request: None,
            net: handle.net(),
        }));

        spawner
            .spawn(handle_coroutine(
                refresh,
                Arc::clone(&spawner),
                handle,
                Arc::clone(&element),
            ))
            .expect("spawn failed");
        element
    }

    pub fn show(&mut self, ui: &mut egui::Ui) {
        ui.vertical(|ui| {
            ui.horizontal(|ui| {
                self.show_content(ui);
            });
            ui.separator();
            self.show_stats(ui);
        });
    }

    pub fn show_stats(&self, ui: &mut egui::Ui) {
        let rewrites = self.net.lock().unwrap().rewrites.clone();
        ui.vertical(|ui| {
            let row = |ui: &mut Ui, s: &str, n: u128| {
                ui.horizontal(|ui| {
                    ui.label(s);
                    ui.label(RichText::from(n.to_string()).strong());
                });
            };
            row(ui, "Annihilate:", rewrites.annihilate);
            row(ui, "Commute:", rewrites.commute);
            row(ui, "Signal", rewrites.signal);
            row(ui, "Erase:", rewrites.era);
            row(ui, "Expand:", rewrites.expand);
            row(ui, "Responds:", rewrites.resp);
            row(ui, "Total:", rewrites.total());
            row(ui, "Busy time (ms):", rewrites.busy_duration.as_millis());
            row(ui, "Total / s:", rewrites.total_per_second());
        });
    }

    pub fn show_content(&mut self, ui: &mut egui::Ui) {
        egui::Frame::default()
            .stroke(egui::Stroke::new(1.0, egui::Color32::GRAY))
            .inner_margin(egui::Margin::same(4))
            .outer_margin(egui::Margin::same(2))
            .show(ui, |ui| {
                ui.vertical(|ui| {
                    Self::show_history(ui, &self.history);

                    if let Some(request) = self.request.take() {
                        match request {
                            Request::Nat(mut input, callback) => {
                                let input_number = BigInt::parse_bytes(input.as_bytes(), 10);
                                let entered = ui
                                    .horizontal(|ui| {
                                        ui.add(
                                            egui::TextEdit::singleline(&mut input)
                                                .hint_text("Type a natural number..."),
                                        );
                                        let button = ui.add_enabled(
                                            input_number.is_some()
                                                && input_number.as_ref().unwrap() >= &BigInt::ZERO,
                                            egui::Button::small(egui::Button::new("OK")),
                                        );
                                        button.clicked() && input_number.is_some()
                                    })
                                    .inner;
                                if entered {
                                    let number = input_number.unwrap();
                                    self.history.push(Event::NatRequest(number.clone()));
                                    callback(number);
                                } else {
                                    self.request = Some(Request::Nat(input, callback));
                                }
                            }

                            Request::Int(mut input, callback) => {
                                let input_number = BigInt::parse_bytes(input.as_bytes(), 10);
                                let entered = ui
                                    .horizontal(|ui| {
                                        ui.add(
                                            egui::TextEdit::singleline(&mut input)
                                                .hint_text("Type an integer..."),
                                        );
                                        let button = ui.add_enabled(
                                            input_number.is_some(),
                                            egui::Button::small(egui::Button::new("OK")),
                                        );
                                        button.clicked() && input_number.is_some()
                                    })
                                    .inner;
                                if entered {
                                    let number = input_number.unwrap();
                                    self.history.push(Event::IntRequest(number.clone()));
                                    callback(number);
                                } else {
                                    self.request = Some(Request::Int(input, callback));
                                }
                            }

                            Request::String(mut input, callback) => {
                                let entered = ui
                                    .horizontal(|ui| {
                                        ui.add(
                                            egui::TextEdit::multiline(&mut input)
                                                .desired_rows(1)
                                                .hint_text("Type a string..."),
                                        );
                                        ui.add(egui::Button::small(egui::Button::new("OK")))
                                            .clicked()
                                    })
                                    .inner;
                                if entered {
                                    let string = Substr::from(input);
                                    self.history.push(Event::StringRequest(string.clone()));
                                    callback(string);
                                } else {
                                    self.request = Some(Request::String(input, callback));
                                }
                            }

                            Request::Char(mut input, callback) => {
                                let input_char = Some(&input)
                                    .filter(|s| s.chars().count() == 1)
                                    .and_then(|s| s.chars().next());
                                let entered = ui
                                    .horizontal(|ui| {
                                        ui.add(
                                            egui::TextEdit::singleline(&mut input)
                                                .hint_text("Type a single character..."),
                                        );
                                        let button = ui.add_enabled(
                                            input_char.is_some(),
                                            egui::Button::small(egui::Button::new("OK")),
                                        );
                                        button.clicked() && input_char.is_some()
                                    })
                                    .inner;
                                if entered {
                                    let character = input_char.unwrap();
                                    self.history.push(Event::CharRequest(character));
                                    callback(character);
                                } else {
                                    self.request = Some(Request::Char(input, callback));
                                }
                            }

                            Request::Choice(signals, callback) => {
                                let mut chosen = None;
                                ui.vertical(|ui| {
                                    for signal in &signals {
                                        if ui
                                            .button(RichText::new(signal.to_string()).strong())
                                            .clicked()
                                        {
                                            chosen = Some(signal.clone());
                                        }
                                    }
                                });
                                if let Some(chosen) = chosen {
                                    self.history.push(Event::Choice(chosen.clone()));
                                    callback(chosen);
                                } else {
                                    self.request = Some(Request::Choice(signals, callback));
                                }
                            }
                        }
                    }
                });
            });
    }

    fn show_history<'h>(ui: &mut egui::Ui, events: &'h [Event]) {
        let mut events = events;
        ui.vertical(|ui| {
            while !events.is_empty() {
                events = Self::show_history_line(ui, events);
            }
        });
    }

    fn show_history_line<'h>(ui: &mut egui::Ui, events: &'h [Event]) -> &'h [Event] {
        let mut polarity = None::<Polarity>;
        let mut events = events;

        ui.horizontal(|ui| {
            while let Some(event) = events.get(0) {
                if polarity.map_or(false, |p| p != event.polarity()) {
                    return events;
                }

                if polarity == None {
                    match event.polarity() {
                        Polarity::Positive => {
                            ui.label(RichText::from(">").code());
                        }
                        Polarity::Negative => {
                            ui.label(RichText::from("<").code());
                        }
                    }
                }

                polarity = Some(event.polarity());
                events = &events[1..];

                match event {
                    Event::Times(child) | Event::Par(child) => {
                        child.lock().unwrap().show_content(ui);
                        return events;
                    }
                    Event::Either(name) | Event::Choice(name) => {
                        ui.label(RichText::from(name.to_string()).strong());
                    }
                    Event::Break | Event::Continue => {
                        ui.label(RichText::from("!").strong().code());
                    }
                    Event::Nat(i) | Event::NatRequest(i) => {
                        ui.label(RichText::from(format!("{}", i)).strong().code());
                    }
                    Event::Int(i) | Event::IntRequest(i) => {
                        ui.label(RichText::from(format!("{}", i)).strong().code());
                    }
                    Event::String(s) | Event::StringRequest(s) => {
                        ui.label(RichText::from(format!("{:?}", s)).strong().code());
                    }
                    Event::Char(s) | Event::CharRequest(s) => {
                        ui.label(RichText::from(format!("{:?}", s)).strong().code());
                    }
                }
            }

            &[]
        })
        .inner
    }
}

async fn handle_coroutine(
    refresh: Arc<dyn Fn() + Send + Sync>,
    spawner: Arc<dyn Spawn + Send + Sync>,
    handle: TypedHandle,
    element: Arc<Mutex<Element>>,
) {
    let mut handle = handle;

    loop {
        match handle.readback().await {
            TypedReadback::Nat(value) => {
                let mut lock = element.lock().expect("lock failed");
                lock.history.push(Event::Nat(value));
                refresh();
                break;
            }

            TypedReadback::NatRequest(callback) => {
                let mut lock = element.lock().expect("lock failed");
                lock.request = Some(Request::Nat(String::new(), callback));
                refresh();
                break;
            }

            TypedReadback::Int(value) => {
                let mut lock = element.lock().expect("lock failed");
                lock.history.push(Event::Int(value));
                refresh();
                break;
            }

            TypedReadback::IntRequest(callback) => {
                let mut lock = element.lock().expect("lock failed");
                lock.request = Some(Request::Int(String::new(), callback));
                refresh();
                break;
            }

            TypedReadback::String(value) => {
                let mut lock = element.lock().expect("lock failed");
                lock.history.push(Event::String(value));
                refresh();
                break;
            }

            TypedReadback::StringRequest(callback) => {
                let mut lock = element.lock().expect("lock failed");
                lock.request = Some(Request::String(String::new(), callback));
                refresh();
                break;
            }

            TypedReadback::Char(value) => {
                let mut lock = element.lock().expect("lock failed");
                lock.history.push(Event::Char(value));
                refresh();
                break;
            }

            TypedReadback::CharRequest(callback) => {
                let mut lock = element.lock().expect("lock failed");
                lock.request = Some(Request::Char(String::new(), callback));
                refresh();
                break;
            }

            TypedReadback::Times(handle1, handle2) => {
                let mut lock = element.lock().expect("lock failed");
                lock.history.push(Event::Times(Element::new(
                    Arc::clone(&refresh),
                    Arc::clone(&spawner),
                    handle1,
                )));
                handle = handle2;
                refresh();
            }

            TypedReadback::Par(handle1, handle2) => {
                let mut lock = element.lock().expect("lock failed");
                lock.history.push(Event::Par(Element::new(
                    Arc::clone(&refresh),
                    Arc::clone(&spawner),
                    handle1,
                )));
                handle = handle2;
                refresh();
            }

            TypedReadback::Either(chosen, handle1) => {
                let mut lock = element.lock().expect("lock failed");
                lock.history.push(Event::Either(chosen));
                handle = handle1;
                refresh();
            }

            TypedReadback::Choice(signals, callback) => {
                let rx = {
                    let (tx, rx) = oneshot::channel();
                    let mut lock = element.lock().expect("lock failed");
                    lock.request = Some(Request::Choice(
                        signals,
                        Box::new(move |chosen| {
                            let handle = callback(chosen);
                            tx.send(handle).ok().unwrap();
                        }),
                    ));
                    rx
                };
                handle = rx.await.unwrap();
                refresh();
            }

            TypedReadback::Break => {
                let mut lock = element.lock().expect("lock failed");
                lock.history.push(Event::Break);
                refresh();
                break;
            }

            TypedReadback::Continue => {
                let mut lock = element.lock().expect("lock failed");
                lock.history.push(Event::Continue);
                refresh();
                break;
            }
        }
    }
}
