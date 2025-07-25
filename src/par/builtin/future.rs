use crate::{
    icombs::readback::Handle,
    par::{
        process,
        program::{Definition, Module},
        types::Type,
    },
};
use arcstr::literal;
use futures::future;
use futures::future::Either;
use std::future::Future;
use std::sync::Arc;

pub fn external_module() -> Module<Arc<process::Expression<()>>> {
    Module {
        type_defs: vec![],
        declarations: vec![],
        definitions: vec![Definition::external(
            "Select",
            Type::name(None, "Select", vec![]),
            |handle| Box::pin(select(handle)),
        )],
    }
}

enum Direction {
    Left,
    Right,
}

impl Direction {
    fn opposite(&self) -> Self {
        match self {
            Direction::Left => Direction::Right,
            Direction::Right => Direction::Left,
        }
    }
}

enum SelectState {
    Syn,
    SynAck,
}

async fn select(mut handle: Handle) {
    let a = handle.receive();
    let b = handle.receive();
    let res = handle;
    let mut state_future1 = Box::pin(map_future(a));
    let mut state_future2 = Box::pin(map_future(b));
    let mut dir = Direction::Left;
    loop {
        let (state, mut chosen, other_future) =
            match future::select(state_future1, state_future2).await {
                Either::Left(((chosen_handle, chosen_state), other_future)) => {
                    (chosen_state, chosen_handle, other_future)
                }
                Either::Right(((chosen_handle, chosen_state), other_future)) => {
                    dir = dir.opposite();
                    (chosen_state, chosen_handle, other_future)
                }
            };

        match state {
            SelectState::Syn => {
                chosen.signal(literal!("syn_ack"));
                match chosen.case().await.as_str() {
                    "ack" => {
                        send_result(chosen, res, other_future, dir).await;
                        break;
                    }
                    "rst" => {
                        state_future1 = Box::pin(map_future(chosen));
                        state_future2 = other_future;
                    }
                    _ => unreachable!(),
                }
            }
            SelectState::SynAck => {
                chosen.signal(literal!("ack"));
                send_result(chosen, res, other_future, dir).await;
                break;
            }
        }
    }
}

async fn map_future(mut handle: Handle) -> (Handle, SelectState) {
    match handle.case().await.as_str() {
        "syn" => (handle, SelectState::Syn),
        "swap" => {
            handle.signal(literal!("syn"));
            match handle.case().await.as_str() {
                "syn_ack" => (handle, SelectState::SynAck),
                _ => unreachable!(),
            }
        }
        _ => unreachable!(),
    }
}

async fn send_result(
    mut chosen: Handle,
    mut res: Handle,
    unchosen_state_future: impl Future<Output = (Handle, SelectState)> + Sized,
    dir: Direction,
) {
    match dir {
        Direction::Left => {
            res.signal(literal!("left"));
            res.send().link(chosen);
            let other = res.send();
            res.break_();
            let (unchosen, unchosen_state) = unchosen_state_future.await;
            reset(unchosen, other, unchosen_state).await;
        }
        Direction::Right => {
            res.signal(literal!("right"));
            let other = res.send();
            res.send().link(chosen);
            res.break_();
            let (unchosen, unchosen_state) = unchosen_state_future.await;
            reset(unchosen, other, unchosen_state).await;
        }
    }
}

async fn reset(mut handle: Handle, mut res: Handle, state: SelectState) {
    match state {
        SelectState::Syn => {
            res.signal(literal!("syn"));
            res.link(handle);
        }
        SelectState::SynAck => {
            handle.signal(literal!("rst"));
            res.signal(literal!("swap"));
            res.link(handle);
        }
    }
}
