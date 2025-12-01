use crate::{
    icombs::readback::Handle,
    par::{
        process,
        program::{Definition, Module},
        types::Type,
    },
};
use num_bigint::BigInt;
use num_traits::ToPrimitive;
use std::sync::Arc;

pub fn external_module() -> Module<Arc<process::Expression<()>>> {
    Module {
        type_defs: vec![],
        declarations: vec![],
        definitions: vec![
            Definition::external(
                "Now",
                Type::function(Type::break_(), Type::nat()),
                |handle| Box::pin(time_now(handle)),
            ),
            Definition::external(
                "WaitAndNow",
                Type::function(Type::nat(), Type::nat()),
                |handle| Box::pin(time_wait_and_now(handle)),
            ),
        ],
    }
}

async fn time_now(mut handle: Handle) {
    // return current time in milliseconds since epoch
    handle.receive().continue_();
    let now = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap()
        .as_millis();
    handle.provide_nat(BigInt::from(now));
}

async fn time_wait_and_now(mut handle: Handle) {
    // Receive the milliseconds to wait
    let milliseconds = handle.receive().nat().await;
    
    // Convert BigInt to u64 for Duration
    // Cap at u64::MAX (approximately 584 million years) for safety
    // This ensures compatibility with all platforms and processors
    // For modern computers (last 10 years): fully supported on Windows, Linux, macOS
    let ms_u64 = milliseconds
        .to_u64()
        .unwrap_or(u64::MAX)
        .min(u64::MAX);
    
    // Sleep for the specified duration
    // tokio::time::sleep is cross-platform (Windows, Linux, macOS, etc.)
    // Sleep precision varies by OS but is acceptable for typical use cases:
    //   - Windows: ~15ms precision
    //   - Linux/macOS: sub-millisecond precision
    tokio::time::sleep(tokio::time::Duration::from_millis(ms_u64)).await;
    
    // Get the current time after waiting and return it
    // SystemTime::now() and duration_since(UNIX_EPOCH) are cross-platform
    // as_millis() returns u128, which is safe for timestamps well into the future
    // Note: unwrap() is safe here as modern systems (last 10+ years) always have
    // clocks set after 1970-01-01 (UNIX epoch). This matches Time.Now's implementation.
    let now = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap()
        .as_millis();
    handle.provide_nat(BigInt::from(now));
}
