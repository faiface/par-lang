use std::{
    io,
    net::SocketAddr,
    str::FromStr,
    sync::{
        atomic::{AtomicBool, Ordering as AtomicOrdering},
        Arc,
    },
    time::Duration,
};

use arcstr::literal;
use bytes::Bytes;
use futures::{
    channel::{mpsc, oneshot},
    SinkExt, StreamExt,
};
use http_body::Frame;
use http_body_util::{self as body_util, BodyExt, Full, StreamBody};
use hyper::{
    body::Incoming,
    http::{header::HOST, HeaderName, HeaderValue, StatusCode},
    service::service_fn,
    Request, Response,
};
use hyper_util::rt::TokioIo;
use num_bigint::BigInt;
use tokio::{net::TcpListener, signal, sync::Notify};
use url::Url as ParsedUrl;

use crate::{
    par::{
        builtin::{list::readback_list, url::provide_url_value},
        primitive::ParString,
        process,
        program::{Definition, Module},
        types::Type,
    },
    runtime::Handle,
};

pub fn external_module() -> Module<Arc<process::Expression<()>>> {
    Module {
        type_defs: vec![],
        declarations: vec![],
        definitions: vec![
            Definition::external(
                "Fetch",
                Type::function(
                    Type::name(None, "Request", vec![]),
                    Type::either(vec![
                        ("err", Type::name(None, "Error", vec![])),
                        ("ok", Type::name(None, "Response", vec![])),
                    ]),
                ),
                |handle| Box::pin(http_fetch(handle)),
            ),
            Definition::external(
                "Listen",
                Type::function(
                    Type::string(),
                    Type::recursive(
                        None,
                        Type::either(vec![
                            (
                                "shutdown",
                                Type::either(vec![
                                    ("ok", Type::break_()),
                                    ("err", Type::name(None, "Error", vec![])),
                                ]),
                            ),
                            (
                                "incoming",
                                Type::pair(
                                    Type::name(None, "Request", vec![]),
                                    Type::pair(
                                        Type::function(
                                            Type::name(None, "Response", vec![]),
                                            Type::either(vec![
                                                ("ok", Type::break_()),
                                                ("err", Type::name(None, "Error", vec![])),
                                            ]),
                                        ),
                                        Type::self_(None),
                                    ),
                                ),
                            ),
                        ]),
                    ),
                ),
                |handle| Box::pin(http_listen(handle)),
            ),
        ],
    }
}

// ----------

async fn http_fetch(mut handle: Handle) {
    let mut request = handle.receive().await;

    let method = request.receive().await.string().await;

    let mut url_handle = request.receive().await;
    url_handle.signal(literal!("full")).await;
    let url = url_handle.string().await;

    let header_pairs = readback_list(request.receive().await, |mut handle| async move {
        let name = handle.receive().await.string().await;
        let value = handle.string().await;
        (name, value)
    })
    .await;

    let body_reader = request;

    let (tx, rx) = mpsc::unbounded::<Result<bytes::Bytes, std::io::Error>>();
    let (body_done_tx, body_done_rx) = oneshot::channel::<Result<(), ParString>>();

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
            handle.signal(literal!("err")).await;
            return handle
                .provide_string(ParString::from(err.to_string()))
                .await;
        }
    };

    let method = reqwest::Method::from_bytes(&method.as_bytes()).unwrap_or(reqwest::Method::GET);

    let mut headers = reqwest::header::HeaderMap::new();
    for (name, value) in header_pairs.iter() {
        if let (Ok(hn), Ok(hv)) = (
            reqwest::header::HeaderName::from_bytes(&name.as_bytes()),
            reqwest::header::HeaderValue::from_bytes(&value.as_bytes()),
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
                handle.signal(literal!("err")).await;
                return handle.provide_string(ParString::from(body_err)).await;
            }
            response
        }
        Err(err) => {
            handle.signal(literal!("err")).await;
            if let Err(body_err) = body_result {
                return handle.provide_string(ParString::from(body_err)).await;
            }
            return handle
                .provide_string(ParString::from(err.to_string()))
                .await;
        }
    };

    handle.signal(literal!("ok")).await;
    handle
        .send()
        .await
        .provide_nat(BigInt::from(response.status().as_u16()))
        .await;
    provide_headers_list(handle.send().await, response.headers()).await;
    provide_body_reader(handle, response).await;
}

async fn provide_body_reader(mut handle: Handle, response: reqwest::Response) {
    let mut stream = response.bytes_stream();
    loop {
        match handle.case().await.as_str() {
            "close" => {
                handle.signal(literal!("ok")).await;
                return handle.break_().await;
            }
            "read" => match stream.next().await {
                Some(Ok(bytes)) => {
                    handle.signal(literal!("ok")).await;
                    handle.signal(literal!("chunk")).await;
                    handle.send().await.provide_bytes(bytes).await;
                    continue;
                }
                Some(Err(err)) => {
                    handle.signal(literal!("err")).await;
                    return handle
                        .provide_string(ParString::from(err.to_string()))
                        .await;
                }
                None => {
                    handle.signal(literal!("ok")).await;
                    handle.signal(literal!("end")).await;
                    return handle.break_().await;
                }
            },
            _ => unreachable!(),
        }
    }
}

async fn provide_headers_list(mut handle: Handle, headers: &reqwest::header::HeaderMap) {
    for (name, value) in headers {
        handle.signal(literal!("item")).await;
        let (name, value) = (
            ParString::copy_from_slice(name.as_str().as_bytes()),
            Bytes::copy_from_slice(value.as_bytes()),
        );
        handle.send().await.concurrently(|mut handle| async {
            handle.send().await.provide_string(name).await;
            handle.provide_bytes(value).await;
        });
    }
    handle.signal(literal!("end")).await;
    handle.break_().await;
}

async fn consume_http_reader(
    mut handle: Handle,
    mut tx: mpsc::UnboundedSender<Result<bytes::Bytes, std::io::Error>>,
    done: oneshot::Sender<Result<(), ParString>>,
) {
    let mut done = Some(done);

    loop {
        handle.signal(literal!("read")).await;
        match handle.case().await.as_str() {
            "ok" => match handle.case().await.as_str() {
                "chunk" => {
                    let chunk = handle.receive().await.bytes().await;
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
                    handle.continue_().await;
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
                let io_err = io::Error::new(io::ErrorKind::Other, err.as_str().to_string());
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

async fn close_reader(mut handle: Handle) -> Result<(), ParString> {
    handle.signal(literal!("close")).await;
    match handle.case().await.as_str() {
        "ok" => {
            handle.continue_().await;
            Ok(())
        }
        "err" => Err(handle.string().await),
        _ => unreachable!(),
    }
}

// ----------

async fn http_listen(mut handle: Handle) {
    let address = handle.receive().await.string().await;
    match start_listener(address.as_str().to_string()).await {
        Ok(state) => provide_listener_value(handle, state).await,
        Err(err) => {
            handle.signal(literal!("shutdown")).await;
            handle.signal(literal!("err")).await;
            handle.provide_string(err).await;
        }
    }
}

type ResponseBody = body_util::combinators::BoxBody<Bytes, BodyError>;

struct ListenerState {
    events: mpsc::UnboundedReceiver<ListenerEvent>,
}

#[derive(Clone)]
struct ListenerControl {
    sender: mpsc::UnboundedSender<ListenerEvent>,
    notify: Arc<Notify>,
    shutdown: Arc<AtomicBool>,
    bind_host: String,
}

enum ListenerEvent {
    Incoming(IncomingRequest),
    Shutdown(Result<(), String>),
}

struct IncomingRequest {
    method: String,
    url: ParsedUrl,
    headers: Vec<(String, String)>,
    body: Incoming,
    responder: oneshot::Sender<Result<Response<ResponseBody>, BodyError>>,
}

#[derive(Debug, Clone)]
struct BodyError(ParString);

impl std::fmt::Display for BodyError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.as_str())
    }
}

impl std::error::Error for BodyError {}

impl ListenerControl {
    fn new(sender: mpsc::UnboundedSender<ListenerEvent>, bind_host: String) -> Self {
        Self {
            sender,
            notify: Arc::new(Notify::new()),
            shutdown: Arc::new(AtomicBool::new(false)),
            bind_host,
        }
    }

    async fn trigger_shutdown(&mut self, result: Result<(), String>) {
        if !self.shutdown.swap(true, AtomicOrdering::SeqCst) {
            let _ = self.sender.send(ListenerEvent::Shutdown(result)).await;
            self.notify.notify_waiters();
        }
    }

    fn is_shutdown(&self) -> bool {
        self.shutdown.load(AtomicOrdering::SeqCst)
    }
}

impl ListenerState {
    fn new(receiver: mpsc::UnboundedReceiver<ListenerEvent>) -> Self {
        Self { events: receiver }
    }

    async fn next_event(&mut self) -> ListenerEvent {
        match self.events.next().await {
            Some(event) => event,
            None => ListenerEvent::Shutdown(Ok(())),
        }
    }
}

async fn start_listener(address: String) -> Result<ListenerState, ParString> {
    let socket_addr: SocketAddr = address
        .parse()
        .map_err(|err: std::net::AddrParseError| err.to_string())?;

    let listener = TcpListener::bind(socket_addr)
        .await
        .map_err(|err| err.to_string())?;

    let (event_tx, event_rx) = mpsc::unbounded();
    let mut control = ListenerControl::new(event_tx, address);

    let mut accept_control = control.clone();
    tokio::spawn(async move {
        if let Err(err) = run_accept_loop(listener, accept_control.clone()).await {
            accept_control.trigger_shutdown(Err(err)).await;
        }
    });

    tokio::spawn(async move {
        match signal::ctrl_c().await {
            Ok(()) => control.trigger_shutdown(Ok(())).await,
            Err(err) => control.trigger_shutdown(Err(err.to_string())).await,
        }
    });

    Ok(ListenerState::new(event_rx))
}

async fn run_accept_loop(
    listener: TcpListener,
    mut control: ListenerControl,
) -> Result<(), String> {
    loop {
        tokio::select! {
            _ = control.notify.notified() => {
                break Ok(());
            }
            accept_res = listener.accept() => {
                match accept_res {
                    Ok((stream, _)) => {
                        let control = control.clone();
                        tokio::spawn(async move {
                            handle_connection(stream, control).await;
                        });
                    }
                    Err(err) => {
                        let message = err.to_string();
                        control.trigger_shutdown(Err(message.clone())).await;
                        break Err(message);
                    }
                }
            }
        }
    }
}

async fn handle_connection(stream: tokio::net::TcpStream, control: ListenerControl) {
    let io = TokioIo::new(stream);
    let service = service_fn(move |req| handle_request(req, control.clone()));

    if hyper::server::conn::http1::Builder::new()
        .serve_connection(io, service)
        .with_upgrades()
        .await
        .is_err()
    {
        // Ignore individual connection errors; they are reported per-request.
    }
}

async fn handle_request(
    req: Request<Incoming>,
    control: ListenerControl,
) -> Result<Response<ResponseBody>, hyper::Error> {
    if control.is_shutdown() {
        return Ok(simple_response(
            StatusCode::SERVICE_UNAVAILABLE,
            "server shutting down",
        ));
    }

    let (parts, body) = req.into_parts();
    let method = parts.method.as_str().to_string();

    let headers_vec = parts
        .headers
        .iter()
        .filter_map(|(name, value)| {
            value
                .to_str()
                .ok()
                .map(|v| (name.as_str().to_string(), v.to_string()))
        })
        .collect::<Vec<_>>();

    let host = parts
        .headers
        .get(HOST)
        .and_then(|value| value.to_str().ok())
        .map(|s| s.to_string())
        .unwrap_or_else(|| control.bind_host.clone());

    let path = parts
        .uri
        .path_and_query()
        .map(|pq| pq.as_str())
        .unwrap_or("/");

    let scheme = parts.uri.scheme_str().unwrap_or("http");
    let full_url = format!("{}://{}{}", scheme, host, path);

    let parsed_url = match ParsedUrl::parse(&full_url) {
        Ok(url) => url,
        Err(_) => {
            return Ok(simple_response(
                StatusCode::BAD_REQUEST,
                "invalid request url",
            ));
        }
    };

    let (resp_tx, resp_rx) = oneshot::channel();
    let incoming = IncomingRequest {
        method,
        url: parsed_url,
        headers: headers_vec,
        body,
        responder: resp_tx,
    };

    if control
        .sender
        .clone()
        .send(ListenerEvent::Incoming(incoming))
        .await
        .is_err()
    {
        return Ok(simple_response(
            StatusCode::SERVICE_UNAVAILABLE,
            "listener dropped",
        ));
    }

    match resp_rx.await {
        Ok(Ok(response)) => Ok(response),
        Ok(Err(err)) => Ok(simple_response(
            StatusCode::INTERNAL_SERVER_ERROR,
            err.to_string(),
        )),
        Err(_) => Ok(simple_response(
            StatusCode::INTERNAL_SERVER_ERROR,
            "response cancelled",
        )),
    }
}

async fn provide_listener_value(mut handle: Handle, mut state: ListenerState) {
    match state.next_event().await {
        ListenerEvent::Incoming(request) => {
            handle.signal(literal!("incoming")).await;
            let IncomingRequest {
                method,
                url,
                headers,
                body,
                responder,
            } = request;

            handle.send().await.concurrently(|handle| {
                provide_http_request_value(handle, method, url, headers, body)
            });
            handle
                .send()
                .await
                .concurrently(|handle| provide_responder_function(handle, responder));
            Box::pin(provide_listener_value(handle, state)).await;
        }

        ListenerEvent::Shutdown(result) => {
            handle.signal(literal!("shutdown")).await;
            match result {
                Ok(()) => {
                    handle.signal(literal!("ok")).await;
                    handle.break_().await;
                }
                Err(err) => {
                    handle.signal(literal!("err")).await;
                    handle.provide_string(ParString::from(err)).await;
                }
            }
        }
    }
}

async fn provide_http_request_value(
    mut handle: Handle,
    method: String,
    url: ParsedUrl,
    headers: Vec<(String, String)>,
    body: Incoming,
) {
    handle
        .send()
        .await
        .provide_string(ParString::from(method))
        .await;
    provide_url_value(handle.send().await, url).await;
    provide_header_list_value(handle.send().await, headers).await;
    provide_request_body_reader(handle, body).await;
}

async fn provide_header_list_value(mut handle: Handle, headers: Vec<(String, String)>) {
    for (name, value) in headers {
        handle.signal(literal!("item")).await;
        handle.send().await.concurrently(|mut pair| async move {
            pair.send()
                .await
                .provide_string(ParString::from(name))
                .await;
            pair.provide_bytes(Bytes::from(value)).await;
        });
    }
    handle.signal(literal!("end")).await;
    handle.break_().await;
}

async fn provide_request_body_reader(mut handle: Handle, mut body: Incoming) {
    loop {
        match handle.case().await.as_str() {
            "close" => {
                handle.signal(literal!("ok")).await;
                return handle.break_().await;
            }
            "read" => match body.frame().await {
                Some(Ok(frame)) => {
                    match frame.into_data() {
                        Ok(chunk) => {
                            if chunk.is_empty() {
                                continue;
                            }
                            handle.signal(literal!("ok")).await;
                            handle.signal(literal!("chunk")).await;
                            handle.send().await.provide_bytes(chunk).await;
                        }
                        Err(_) => {
                            // Skip non-data frames such as trailers.
                            continue;
                        }
                    }
                }
                Some(Err(err)) => {
                    handle.signal(literal!("err")).await;
                    handle
                        .provide_string(ParString::from(err.to_string()))
                        .await;
                    return;
                }
                None => {
                    handle.signal(literal!("ok")).await;
                    handle.signal(literal!("end")).await;
                    return handle.break_().await;
                }
            },
            _ => unreachable!(),
        }
    }
}

async fn provide_responder_function(
    mut handle: Handle,
    responder: oneshot::Sender<Result<Response<ResponseBody>, BodyError>>,
) {
    match build_response(handle.receive().await).await {
        Ok(response) => {
            let _ = responder.send(Ok(response));
            handle.signal(literal!("ok")).await;
            handle.break_().await;
        }
        Err(err) => {
            let _ = responder.send(Err(BodyError(err.clone())));
            handle.signal(literal!("err")).await;
            handle.provide_string(err).await;
        }
    }
}

async fn build_response(mut handle: Handle) -> Result<Response<ResponseBody>, ParString> {
    use num_traits::ToPrimitive;

    let status = handle
        .receive()
        .await
        .nat()
        .await
        .to_u16()
        .map(StatusCode::from_u16)
        .and_then(Result::ok)
        .unwrap_or(StatusCode::INTERNAL_SERVER_ERROR);

    let headers = readback_list(handle.receive().await, |mut handle| async {
        let key = handle.receive().await.string().await;
        let val = handle.bytes().await;
        (key, val)
    })
    .await;

    let mut response = Response::builder()
        .status(status)
        .body(BodyExt::boxed(reader_to_body(handle)))
        .map_err(|err| Bytes::from(err.to_string()))?;

    for (name, value) in headers {
        let Ok(header_name) = HeaderName::from_str(name.as_str()) else {
            continue;
        };
        let Ok(header_value) = HeaderValue::from_bytes(&value) else {
            continue;
        };
        response.headers_mut().append(header_name, header_value);
    }

    Ok(response)
}

fn reader_to_body(reader: Handle) -> StreamBody<mpsc::Receiver<Result<Frame<Bytes>, BodyError>>> {
    let (mut tx, rx) = mpsc::channel(1);

    reader.concurrently(|mut reader| async move {
        loop {
            reader.signal(literal!("read")).await;
            match reader.case().await.as_str() {
                "ok" => match reader.case().await.as_str() {
                    "chunk" => {
                        let bytes = reader.receive().await.bytes().await;
                        if tx.send(Ok(Frame::data(bytes))).await.is_err() {
                            reader.signal(literal!("close")).await;
                            match reader.case().await.as_str() {
                                "ok" => reader.continue_().await,
                                "err" => {
                                    let _ = reader.string().await;
                                }
                                _ => unreachable!(),
                            }
                            return;
                        }
                        continue;
                    }
                    "end" => {
                        reader.continue_().await;
                        return;
                    }
                    _ => unreachable!(),
                },
                "err" => {
                    let err = reader.string().await;
                    let _ = tx.send(Err(BodyError(err))).await;
                    return;
                }
                _ => unreachable!(),
            }
        }
    });

    StreamBody::new(rx)
}

fn simple_response(status: StatusCode, message: impl Into<String>) -> Response<ResponseBody> {
    let text = message.into();
    let body = Full::new(Bytes::from(text))
        .map_err(|infallible| match infallible {})
        .boxed();
    Response::builder()
        .status(status)
        .body(body)
        .expect("valid response")
}
