use std::time::Duration;

#[derive(Default, Clone)]
pub struct Rewrites {
    pub r#continue: u64,
    pub receive: u64,
    pub r#match: u64,
    pub fanout: u64,
    pub instantiate: u64,
    pub ext_call: u64,
    pub ext_send: u64,
}

impl Rewrites {
    fn total(&self) -> u64 {
        self.r#continue
            + self.receive
            + self.r#match
            + self.fanout
            + self.instantiate
            + self.ext_call
            + self.ext_send
    }
    pub fn show(&self, elapsed: Duration) -> String {
        format!(
            "\
            \tContinue: {}\n\
            \tReceive: {}\n\
            \tMatch: {}\n\
            \tInstantiate: {}\n\
            \tExternal Call: {}\n\
            \tExternal Send: {}\n\
            \tTotal: {}\n\
            \tTime (ms): {}\n\
            \tPer second: {}\n\
        ",
            self.r#continue,
            self.receive,
            self.r#match,
            self.instantiate,
            self.ext_call,
            self.ext_send,
            self.total(),
            elapsed.as_millis(),
            (self.total() as f64 / elapsed.as_secs_f64()) as u64,
        )
    }
}
