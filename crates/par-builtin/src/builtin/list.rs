use par_runtime::readback::Handle;
use std::future::Future;

pub(super) async fn readback_list<T, F>(
    mut handle: Handle,
    mut readback_item: impl FnMut(Handle) -> F,
) -> Vec<T>
where
    F: Future<Output = T>,
{
    let mut items = Vec::new();
    loop {
        match handle.case().await.as_str() {
            "end" => {
                handle.continue_();
                return items;
            }
            "item" => {
                let item = readback_item(handle.receive()).await;
                items.push(item);
            }
            _ => unreachable!(),
        }
    }
}
