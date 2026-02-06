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
    pub share_sync: u64,
    pub share_async: u64,
    pub net_duration: Duration,
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
            + self.share_sync
            + self.share_async
    }
    pub fn show(&self, elapsed: Duration) -> String {
        let _ = elapsed;
        let per_second = if self.net_duration.is_zero() {
            0
        } else {
            (self.total() as f64 / self.net_duration.as_secs_f64()) as u64
        };
        format!(
            "\
            \tContinue: {}\n\
            \tReceive: {}\n\
            \tMatch: {}\n\
            \tInstantiate: {}\n\
            \tFanout: {}\n\
            \tExternal Call: {}\n\
            \tExternal Send: {}\n\
            \tShare Sync: {}\n\
            \tShare Async: {}\n\
            \tTotal reductions: {}\n\
            \tTotal time: {}\n\
            \tNet time (ms): {}\n\
            \tNet reductions per second: {}\n\
        ",
            self.r#continue,
            self.receive,
            self.r#match,
            self.instantiate,
            self.fanout,
            self.ext_call,
            self.ext_send,
            self.share_sync,
            self.share_async,
            self.total(),
            elapsed.as_millis(),
            self.net_duration.as_millis(),
            per_second,
        )
    }
}
