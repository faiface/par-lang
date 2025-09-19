use std::sync::Arc;

use arcstr::{literal, Substr};
use url::Url as ParsedUrl;

use crate::{
    icombs::readback::Handle,
    par::{
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
            "FromString",
            Type::function(
                Type::string(),
                Type::either(vec![
                    ("ok", Type::name(None, "Url", vec![])),
                    ("err", Type::name(None, "Error", vec![])),
                ]),
            ),
            |handle| Box::pin(url_from_string(handle)),
        )],
    }
}

async fn url_from_string(mut handle: Handle) {
    let input = handle.receive().string().await;
    match ParsedUrl::parse(input.as_str()) {
        Ok(url) => {
            handle.signal(literal!("ok"));
            provide_url(handle, Arc::new(url));
        }
        Err(err) => {
            handle.signal(literal!("err"));
            handle.provide_string(Substr::from(err.to_string()));
        }
    }
}

fn provide_url(handle: Handle, url: Arc<ParsedUrl>) {
    handle.provide_box(move |mut handle| {
        let url = url.clone();
        async move {
            loop {
                match handle.case().await.as_str() {
                    "full" => {
                        let serialized = url.as_str().to_string();
                        handle.provide_string(Substr::from(serialized));
                        return;
                    }
                    "protocol" => {
                        let scheme = url.scheme().to_string();
                        handle.provide_string(Substr::from(scheme));
                        return;
                    }
                    "host" => {
                        let mut host = url.host_str().unwrap_or("").to_string();
                        if let Some(port) = url.port() {
                            if !host.is_empty() {
                                host.push(':');
                                host.push_str(&port.to_string());
                            } else {
                                host = port.to_string();
                            }
                        }
                        handle.provide_string(Substr::from(host));
                        return;
                    }
                    "path" => {
                        let path = url.path().to_string();
                        handle.provide_string(Substr::from(path));
                        return;
                    }
                    "query" => {
                        for (key, value) in url.query_pairs() {
                            handle.signal(literal!("item"));
                            let key = key.into_owned();
                            let value = value.into_owned();
                            handle.send().concurrently(|mut pair| async move {
                                pair.send().provide_string(Substr::from(key));
                                pair.provide_string(Substr::from(value));
                            });
                        }
                        handle.signal(literal!("end"));
                        handle.break_();
                        return;
                    }
                    "appendPath" => {
                        let segment = handle.receive().string().await;
                        let updated = append_path(url.as_ref(), segment.as_str());
                        return provide_url(handle, Arc::new(updated));
                    }
                    "addQuery" => {
                        let key = handle.receive().string().await;
                        let value = handle.receive().string().await;
                        let updated = add_query(url.as_ref(), key.as_str(), value.as_str());
                        return provide_url(handle, Arc::new(updated));
                    }
                    _ => unreachable!(),
                }
            }
        }
    });
}

fn append_path(current: &ParsedUrl, segment: &str) -> ParsedUrl {
    if segment.is_empty() {
        return current.clone();
    }

    let mut new_url = current.clone();
    let parts: Vec<&str> = segment.split('/').filter(|part| !part.is_empty()).collect();
    if new_url.cannot_be_a_base() {
        let mut path = new_url.path().to_string();
        if path.is_empty() || !path.ends_with('/') {
            path.push('/');
        }
        let appended = parts.join("/");
        path.push_str(&appended);
        new_url.set_path(&path);
    } else {
        let mut segments_mut = new_url.path_segments_mut().expect("base URL");
        for part in parts.iter() {
            segments_mut.push(part);
        }
    }
    if new_url.path().is_empty() {
        new_url.set_path("/");
    }
    new_url
}

fn add_query(current: &ParsedUrl, key: &str, value: &str) -> ParsedUrl {
    let mut new_url = current.clone();
    new_url.query_pairs_mut().append_pair(key, value);
    new_url
}
