#![cfg(test)]

use std::fs;
use std::path::PathBuf;
use std::time::{SystemTime, UNIX_EPOCH};

use par_core::frontend::process::{Command, Expression, Process};
use par_core::frontend::{DefinitionBody, Type};

use crate::check;

const LARGE_TEST_STACK_SIZE: usize = 6 * 1024 * 1024;

fn run_with_large_stack<T>(f: impl FnOnce() -> T + Send + 'static) -> T
where
    T: Send + 'static,
{
    let handle = std::thread::Builder::new()
        .name("par-large-stack-test".to_string())
        .stack_size(LARGE_TEST_STACK_SIZE)
        .spawn(f)
        .expect("failed to spawn large-stack test thread");
    match handle.join() {
        Ok(result) => result,
        Err(panic) => std::panic::resume_unwind(panic),
    }
}

#[derive(Default, Debug)]
struct DepthMetrics {
    expression_current: usize,
    expression_flattened: usize,
    process_current: usize,
    process_flattened: usize,
    type_current: usize,
    type_flattened: usize,
}

impl DepthMetrics {
    fn observe_expression<S>(&mut self, expression: &Expression<Type<S>, S>)
    where
        S: Clone,
    {
        self.expression_current = self.expression_current.max(expression.current_depth());
        self.expression_flattened = self.expression_flattened.max(expression.flattened_depth());

        match expression {
            Expression::Global(_, _, typ)
            | Expression::Primitive(_, _, typ)
            | Expression::External(_, typ) => self.observe_type(typ),
            Expression::Variable(_, _, typ, _) => self.observe_type(typ),
            Expression::Box(_, _, inner, typ) => {
                self.observe_type(typ);
                self.observe_expression(inner);
            }
            Expression::Chan {
                chan_annotation,
                chan_type,
                expr_type,
                process,
                ..
            } => {
                if let Some(annotation) = chan_annotation {
                    self.observe_type(annotation);
                }
                self.observe_type(chan_type);
                self.observe_type(expr_type);
                self.observe_process(process);
            }
        }
    }

    fn observe_process<S>(&mut self, process: &Process<Type<S>, S>)
    where
        S: Clone,
    {
        self.process_current = self.process_current.max(process.current_depth());
        self.process_flattened = self.process_flattened.max(process.flattened_depth());

        match process {
            Process::Let {
                annotation,
                typ,
                value,
                then,
                ..
            } => {
                if let Some(annotation) = annotation {
                    self.observe_type(annotation);
                }
                self.observe_type(typ);
                self.observe_expression(value);
                self.observe_process(then);
            }
            Process::Do { typ, command, .. } => {
                self.observe_type(typ);
                self.observe_command(command);
            }
            Process::Poll {
                clients,
                name_typ,
                then,
                else_,
                ..
            } => {
                self.observe_type(name_typ);
                for client in clients {
                    self.observe_expression(client);
                }
                self.observe_process(then);
                self.observe_process(else_);
            }
            Process::Submit { values, .. } => {
                for value in values {
                    self.observe_expression(value);
                }
            }
            Process::Telltypes(_, process) => self.observe_process(process),
            Process::Block(_, _, body, then) => {
                self.observe_process(body);
                self.observe_process(then);
            }
            Process::Goto(..) | Process::Unreachable(..) => {}
        }
    }

    fn observe_command<S>(&mut self, command: &Command<Type<S>, S>)
    where
        S: Clone,
    {
        match command {
            Command::Link(expression) => self.observe_expression(expression),
            Command::Send(argument, process) => {
                self.observe_expression(argument);
                self.observe_process(process);
            }
            Command::Receive(_, annotation, typ, process, _) => {
                if let Some(annotation) = annotation {
                    self.observe_type(annotation);
                }
                self.observe_type(typ);
                self.observe_process(process);
            }
            Command::Signal(_, process)
            | Command::Continue(process)
            | Command::ReceiveType(_, process) => self.observe_process(process),
            Command::SendType(argument, process) => {
                self.observe_type(argument);
                self.observe_process(process);
            }
            Command::Case(_, processes, else_process) => {
                for process in processes.iter() {
                    self.observe_process(process);
                }
                if let Some(process) = else_process {
                    self.observe_process(process);
                }
            }
            Command::Break | Command::Loop(..) => {}
            Command::Begin { body, .. } => self.observe_process(body),
        }
    }

    fn observe_type<S>(&mut self, typ: &Type<S>)
    where
        S: Clone,
    {
        self.type_current = self.type_current.max(typ.current_depth());
        self.type_flattened = self.type_flattened.max(typ.flattened_depth());
    }
}

#[test]
fn check_all_examples() -> Result<(), String> {
    run_with_large_stack(|| {
        // Check the examples package as a whole.
        let mut d = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        d.push("examples");
        eprintln!("Checking {:?}", d);
        check(d)
    })
}

#[test]
fn test_all_files() -> Result<(), String> {
    run_with_large_stack(|| {
        let mut d = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        d.push("tests");
        eprintln!("Testing {:?}", d);
        if crate::test_runner::run_tests(d, None, None, 10_000) {
            Ok(())
        } else {
            Err("Some tests failed".to_string())
        }
    })
}

fn temp_package(name: &str, source: &str) -> PathBuf {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("system time before unix epoch")
        .as_nanos();
    let root = std::env::temp_dir().join(format!("par-lang-{name}-{unique}"));
    fs::create_dir_all(root.join("src")).expect("failed to create temp package");
    fs::write(root.join("Par.toml"), "[package]\nname = \"tmp\"\n")
        .expect("failed to write manifest");
    fs::write(root.join("src").join("Main.par"), source).expect("failed to write source");
    root
}

fn missing_external_error(package: &PathBuf) -> String {
    let error = match crate::build_runtime_package(package, 10_000) {
        Ok(_) => panic!("link should fail"),
        Err(error) => error,
    };
    error.display()
}

#[test]
fn check_reports_missing_external_registration() {
    let package = temp_package("check-external", "module Main\n\ndef Main : ! = external\n");
    let error = check(package).expect_err("check should fail");
    assert!(
        error.contains("Missing external registration for"),
        "unexpected error: {error}"
    );
}

#[test]
fn runtime_link_reports_missing_external_registration() {
    let package = temp_package("link-external", "module Main\n\ndef Main : ! = external\n");
    let display = missing_external_error(&package);
    assert!(
        display.contains("Missing external registration for"),
        "unexpected error: {display}"
    );
}
