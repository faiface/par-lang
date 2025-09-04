use std::sync::Arc;
use std::time::Duration;

use arcstr::{literal, Substr};
use bytes::Bytes;
use futures::{channel::mpsc, StreamExt};
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
            "Request",
            Type::function(
                // method
                Type::string(),
                Type::function(
                    // url
                    Type::string(),
                    Type::function(
                        // headers: List<(String) String>
                        Type::name(
                            Some("List"),
                            "List",
                            vec![Type::pair(Type::string(), Type::string())],
                        ),
                        Type::function(
                            // body writer closure: [Http.Writer] Result<Http.Error, !>
                            Type::function(
                                Type::name(
                                    Some("Bytes"),
                                    "Writer",
                                    vec![
                                        Type::either(vec![]),                      // errIn = either{}
                                        Type::name(Some("Http"), "Error", vec![]), // errOut = Http.Error
                                    ],
                                ),
                                Type::either(vec![
                                    ("ok", Type::break_()),
                                    ("err", Type::name(Some("Http"), "Error", vec![])),
                                ]),
                            ),
                            // return: either { .err Http.Error, .ok ((Nat) (List<(String) String>) Http.Reader) }
                            Type::either(vec![
                                ("err", Type::name(Some("Http"), "Error", vec![])),
                                (
                                    "ok",
                                    Type::pair(
                                        Type::nat(),
                                        Type::pair(
                                            Type::name(
                                                Some("List"),
                                                "List",
                                                vec![Type::pair(Type::string(), Type::string())],
                                            ),
                                            Type::name(
                                                Some("Bytes"),
                                                "Reader",
                                                vec![
                                                    Type::either(vec![]),                      // errIn
                                                    Type::name(Some("Http"), "Error", vec![]), // errOut
                                                ],
                                            ),
                                        ),
                                    ),
                                ),
                            ]),
                        ),
                    ),
                ),
            ),
            |handle| Box::pin(http_request(handle)),
        )],
    }
}

async fn http_request(mut handle: Handle) {
    let method = handle.receive().string().await;
    let url = handle.receive().string().await;

    let header_pairs = readback_list(handle.receive(), |mut handle| async move {
        let name = handle.receive().string().await;
        let value = handle.string().await;
        (name, value)
    })
    .await;

    let mut body_closure = handle.receive();

    // Channel for streaming request body
    let (tx, rx) = mpsc::unbounded::<Result<bytes::Bytes, std::io::Error>>();
    body_closure.send().concurrently(move |handle| async move {
        provide_http_writer(handle, tx).await;
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
    let response = match request.send().await {
        Ok(r) => r,
        Err(err) => {
            handle.signal(literal!("err"));
            return handle.provide_string(Substr::from(err.to_string()));
        }
    };

    // Wait for the closure result; if it's an error, return .err
    match body_closure.case().await.as_str() {
        "ok" => body_closure.continue_(),
        "err" => {
            let err = body_closure.string().await;
            handle.signal(literal!("err"));
            return handle.provide_string(err);
        }
        _ => unreachable!(),
    }

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
                // Only 'ok' is possible for errIn = either{}
                handle.receive().concurrently(|mut h| async move {
                    match h.case().await.as_str() {
                        "ok" => h.continue_(),
                        _ => unreachable!(),
                    }
                });
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

async fn provide_http_writer(
    mut handle: Handle,
    mut tx: mpsc::UnboundedSender<Result<bytes::Bytes, std::io::Error>>,
) {
    loop {
        match handle.case().await.as_str() {
            "close" => {
                // Only 'ok' is possible for errIn = either{}
                handle.receive().concurrently(|mut h| async move {
                    match h.case().await.as_str() {
                        "ok" => h.continue_(),
                        _ => unreachable!(),
                    }
                });

                // Closing the channel signals end of body
                tx.disconnect();
                handle.signal(literal!("ok"));
                return handle.break_();
            }
            "flush" => {
                // Nothing to flush in channel-based streaming
                handle.signal(literal!("ok"));
                continue;
            }
            "write" => {
                let bytes = handle.receive().bytes().await;
                let res = tx.unbounded_send(Ok(bytes));
                match res {
                    Ok(()) => {
                        handle.signal(literal!("ok"));
                        continue;
                    }
                    Err(_) => {
                        handle.signal(literal!("err"));
                        return handle.provide_string(Substr::from("request body stream closed"));
                    }
                }
            }
            "writeString" => {
                let s = handle.receive().string().await;
                let res = tx.unbounded_send(Ok(Bytes::copy_from_slice(s.as_bytes())));
                match res {
                    Ok(()) => {
                        handle.signal(literal!("ok"));
                        continue;
                    }
                    Err(_) => {
                        handle.signal(literal!("err"));
                        return handle.provide_string(Substr::from("request body stream closed"));
                    }
                }
            }
            _ => unreachable!(),
        }
    }
}
