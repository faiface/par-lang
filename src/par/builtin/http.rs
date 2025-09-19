use std::{io, sync::Arc, time::Duration};

use arcstr::{literal, Substr};
use futures::{
    channel::{mpsc, oneshot},
    StreamExt,
};
use num_bigint::BigInt;

use crate::{
    icombs::readback::Handle,
    par::{
        builtin::list::readback_list,
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
            "Fetch",
            Type::function(
                Type::name(None, "Request", vec![]),
                Type::either(vec![
                    ("err", Type::name(None, "Error", vec![])),
                    ("ok", Type::name(None, "Response", vec![])),
                ]),
            ),
            |handle| Box::pin(http_fetch(handle)),
        )],
    }
}

async fn http_fetch(mut handle: Handle) {
    let mut request = handle.receive();

    let method = request.receive().string().await;
    let url = request.receive().string().await;

    let header_pairs = readback_list(request.receive(), |mut handle| async move {
        let name = handle.receive().string().await;
        let value = handle.string().await;
        (name, value)
    })
    .await;

    let body_reader = request;

    let (tx, rx) = mpsc::unbounded::<Result<bytes::Bytes, std::io::Error>>();
    let (body_done_tx, body_done_rx) = oneshot::channel::<Result<(), Substr>>();

    body_reader.concurrently(move |handle| async move {
        consume_http_reader(handle, tx, body_done_tx).await;
    });

    let client = match reqwest::Client::builder()
        .connect_timeout(Duration::from_secs(10))
        .read_timeout(Duration::from_secs(10))
        .build()
    {
        Ok(c) => c,
        Err(err) => {
            handle.signal(literal!("err"));
            return handle.provide_string(Substr::from(err.to_string()));
        }
    };

    let method = reqwest::Method::from_bytes(method.as_bytes()).unwrap_or(reqwest::Method::GET);

    let mut headers = reqwest::header::HeaderMap::new();
    for (name, value) in header_pairs.iter() {
        if let (Ok(hn), Ok(hv)) = (
            reqwest::header::HeaderName::from_bytes(name.as_bytes()),
            reqwest::header::HeaderValue::from_str(value),
        ) {
            headers.append(hn, hv);
        }
    }

    let request = client.request(method, url.as_str());
    let request = request
        .headers(headers)
        .body(reqwest::Body::wrap_stream(rx));

    let response_result = request.send().await;
    let body_result = body_done_rx.await.unwrap_or(Ok(()));

    let response = match response_result {
        Ok(response) => {
            if let Err(body_err) = body_result {
                handle.signal(literal!("err"));
                return handle.provide_string(body_err);
            }
            response
        }
        Err(err) => {
            handle.signal(literal!("err"));
            if let Err(body_err) = body_result {
                return handle.provide_string(body_err);
            }
            return handle.provide_string(Substr::from(err.to_string()));
        }
    };

    handle.signal(literal!("ok"));
    handle
        .send()
        .provide_nat(BigInt::from(response.status().as_u16()));
    provide_headers_list(handle.send(), response.headers()).await;
    provide_body_reader(handle, response).await;
}

async fn provide_body_reader(mut handle: Handle, response: reqwest::Response) {
    let mut stream = response.bytes_stream();
    loop {
        match handle.case().await.as_str() {
            "close" => {
                handle.signal(literal!("ok"));
                return handle.break_();
            }
            "read" => match stream.next().await {
                Some(Ok(bytes)) => {
                    handle.signal(literal!("ok"));
                    handle.signal(literal!("chunk"));
                    handle.send().provide_bytes(bytes);
                    continue;
                }
                Some(Err(err)) => {
                    handle.signal(literal!("err"));
                    return handle.provide_string(Substr::from(err.to_string()));
                }
                None => {
                    handle.signal(literal!("ok"));
                    handle.signal(literal!("end"));
                    return handle.break_();
                }
            },
            _ => unreachable!(),
        }
    }
}

async fn provide_headers_list(mut handle: Handle, headers: &reqwest::header::HeaderMap) {
    for (name, value) in headers {
        handle.signal(literal!("item"));
        let (name, value) = (
            Substr::from(name.as_str()),
            Substr::from(value.to_str().unwrap_or_default()),
        );
        handle.send().concurrently(|mut handle| async {
            handle.send().provide_string(name);
            handle.provide_string(value);
        });
    }
    handle.signal(literal!("end"));
    handle.break_();
}

async fn consume_http_reader(
    mut handle: Handle,
    mut tx: mpsc::UnboundedSender<Result<bytes::Bytes, std::io::Error>>,
    done: oneshot::Sender<Result<(), Substr>>,
) {
    let mut done = Some(done);

    loop {
        handle.signal(literal!("read"));
        match handle.case().await.as_str() {
            "ok" => match handle.case().await.as_str() {
                "chunk" => {
                    let chunk = handle.receive().bytes().await;
                    if chunk.is_empty() {
                        continue;
                    }
                    if tx.unbounded_send(Ok(chunk)).is_err() {
                        let result = close_reader(handle).await;
                        if let Some(done) = done.take() {
                            let _ = done.send(result);
                        }
                        return;
                    }
                }
                "end" => {
                    handle.continue_();
                    tx.disconnect();
                    if let Some(done) = done.take() {
                        let _ = done.send(Ok(()));
                    }
                    return;
                }
                _ => unreachable!(),
            },
            "err" => {
                let err = handle.string().await;
                let io_err = io::Error::new(io::ErrorKind::Other, err.to_string());
                let _ = tx.unbounded_send(Err(io_err));
                if let Some(done) = done.take() {
                    let _ = done.send(Err(err));
                }
                return;
            }
            _ => unreachable!(),
        }
    }
}

async fn close_reader(mut handle: Handle) -> Result<(), Substr> {
    handle.signal(literal!("close"));
    match handle.case().await.as_str() {
        "ok" => {
            handle.continue_();
            Ok(())
        }
        "err" => {
            let err = handle.string().await;
            Err(err)
        }
        _ => unreachable!(),
    }
}
