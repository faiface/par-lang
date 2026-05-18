//package: core
use std::sync::Arc;

use crate::builtin::list::readback_list;
use arcstr::literal;
use indexmap::IndexMap;
use par_runtime::primitive::ParString;
use par_runtime::readback::Handle;
use par_runtime::registry::{DefinitionRef, ExternalDef, PackageRef};

inventory::submit!(ExternalDef {
    path: DefinitionRef {
        package: PackageRef::Special("core"),
        path: &[],
        module: "Json",
        name: "Encode"
    },
    f: |handle| Box::pin(json_encode(handle)),
});

inventory::submit!(ExternalDef {
    path: DefinitionRef {
        package: PackageRef::Special("core"),
        path: &[],
        module: "Json",
        name: "Decode"
    },
    f: |handle| Box::pin(json_decode(handle)),
});

#[derive(Clone, Debug)]
enum SerJsonValue {
    Null,
    Bool(bool),
    String(ParString),
    Number(f64),
    List(Vec<SerJsonValue>),
    // when serializing, we only need to provide a sequence of entries, so we can just use a vec
    Object(Vec<(ParString, SerJsonValue)>),
}

impl serde::Serialize for SerJsonValue {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match self {
            SerJsonValue::Null => serializer.serialize_unit(),
            SerJsonValue::Bool(v) => serializer.serialize_bool(*v),
            SerJsonValue::String(v) => serializer.serialize_str(v.as_str()),
            SerJsonValue::Number(v) => serializer.serialize_f64(*v),
            SerJsonValue::List(values) => serializer.collect_seq(values),
            SerJsonValue::Object(entries) => {
                serializer.collect_map(entries.iter().map(|(k, v)| (k, v)))
            }
        }
    }
}

#[derive(Clone, Debug)]
enum DeJsonValue {
    Null,
    Bool(bool),
    String(ParString),
    Number(f64),
    List(Vec<DeJsonValue>),
    // when deserializing, we need to provide a map interface, so we need an actual map
    Object(IndexMap<ParString, DeJsonValue>),
}

impl<'de> serde::Deserialize<'de> for DeJsonValue {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        struct Visitor;

        impl<'de> serde::de::Visitor<'de> for Visitor {
            type Value = DeJsonValue;

            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                formatter.write_str("any valid JSON value")
            }

            #[inline]
            fn visit_bool<E>(self, value: bool) -> Result<DeJsonValue, E> {
                Ok(DeJsonValue::Bool(value))
            }

            #[inline]
            fn visit_i64<E>(self, value: i64) -> Result<DeJsonValue, E> {
                Ok(DeJsonValue::Number(value as f64))
            }

            fn visit_i128<E>(self, value: i128) -> Result<DeJsonValue, E> {
                Ok(DeJsonValue::Number(value as f64))
            }

            #[inline]
            fn visit_u64<E>(self, value: u64) -> Result<DeJsonValue, E> {
                Ok(DeJsonValue::Number(value as f64))
            }

            fn visit_u128<E>(self, value: u128) -> Result<DeJsonValue, E> {
                Ok(DeJsonValue::Number(value as f64))
            }

            #[inline]
            fn visit_f64<E>(self, value: f64) -> Result<DeJsonValue, E> {
                Ok(DeJsonValue::Number(value))
            }

            #[inline]
            fn visit_str<E>(self, value: &str) -> Result<DeJsonValue, E> {
                Ok(DeJsonValue::String(value.to_owned().into()))
            }

            #[inline]
            fn visit_string<E>(self, value: String) -> Result<DeJsonValue, E> {
                Ok(DeJsonValue::String(value.into()))
            }

            #[inline]
            fn visit_none<E>(self) -> Result<DeJsonValue, E> {
                Ok(DeJsonValue::Null)
            }

            #[inline]
            fn visit_some<D>(self, deserializer: D) -> Result<DeJsonValue, D::Error>
            where
                D: serde::Deserializer<'de>,
            {
                serde::Deserialize::deserialize(deserializer)
            }

            #[inline]
            fn visit_unit<E>(self) -> Result<DeJsonValue, E> {
                Ok(DeJsonValue::Null)
            }

            #[inline]
            fn visit_seq<V>(self, mut visitor: V) -> Result<DeJsonValue, V::Error>
            where
                V: serde::de::SeqAccess<'de>,
            {
                let mut vec = Vec::new();

                while let Some(elem) = visitor.next_element()? {
                    vec.push(elem);
                }

                Ok(DeJsonValue::List(vec))
            }

            fn visit_map<V>(self, mut visitor: V) -> Result<DeJsonValue, V::Error>
            where
                V: serde::de::MapAccess<'de>,
            {
                let mut values = IndexMap::new();

                while let Some((key, value)) = visitor.next_entry()? {
                    values.insert(key, value);
                }

                Ok(DeJsonValue::Object(values))
            }
        }

        deserializer.deserialize_any(Visitor)
    }
}

async fn json_encode(mut handle: Handle) {
    let json = readback_json(handle.receive()).await;
    let encoded = serde_json::to_string(&json).expect("JSON encoding should not fail");
    handle.provide_string(ParString::from(encoded));
}

async fn json_decode(mut handle: Handle) {
    let string = handle.receive().string().await;
    match serde_json::from_str::<DeJsonValue>(string.as_str()) {
        Ok(json) => {
            handle.signal(literal!("ok"));
            provide_json_value(handle, json);
        }
        Err(err) => {
            handle.signal(literal!("err"));
            handle.provide_string(ParString::from(err.to_string()));
        }
    }
}

async fn readback_json(mut handle: Handle) -> SerJsonValue {
    match handle.case().await.as_str() {
        "null" => {
            handle.continue_();
            SerJsonValue::Null
        }
        "bool" => SerJsonValue::Bool(readback_bool(handle).await),
        "string" => SerJsonValue::String(handle.string().await),
        "number" => SerJsonValue::Number(handle.float().await),
        "list" => SerJsonValue::List(
            readback_list(handle, |handle| Box::pin(readback_json(handle))).await,
        ),
        "object" => SerJsonValue::Object(
            readback_list(handle, async |mut handle| {
                let mut pair = handle.receive();
                let key = pair.receive().string().await;
                let value = Box::pin(readback_json(pair)).await;
                (key, value)
            })
            .await,
        ),
        _ => unreachable!(),
    }
}

async fn readback_bool(mut handle: Handle) -> bool {
    match handle.case().await.as_str() {
        "true" => {
            handle.continue_();
            true
        }
        "false" => {
            handle.continue_();
            false
        }
        _ => unreachable!(),
    }
}

fn provide_json_value(mut handle: Handle, value: DeJsonValue) {
    match value {
        DeJsonValue::Null => {
            handle.signal(literal!("null"));
            handle.break_();
        }
        DeJsonValue::Bool(value) => {
            handle.signal(literal!("bool"));
            provide_bool(handle, value);
        }
        DeJsonValue::String(value) => {
            handle.signal(literal!("string"));
            handle.provide_string(value.into());
        }
        DeJsonValue::Number(value) => {
            handle.signal(literal!("number"));
            handle.provide_float(value);
        }
        DeJsonValue::List(values) => {
            handle.signal(literal!("list"));
            for value in values {
                handle.signal(literal!("item"));
                provide_json_value(handle.send(), value);
            }
            handle.signal(literal!("end"));
            handle.break_();
        }
        DeJsonValue::Object(entries) => {
            handle.signal(literal!("object"));
            provide_object(handle, entries);
        }
    }
}

fn provide_object(handle: Handle, entries: IndexMap<ParString, DeJsonValue>) {
    let entries = Arc::new(entries);
    handle.provide_box(move |mut handle| {
        let entries = entries.clone();
        async move {
            match handle.case().await.as_str() {
                "size" => handle.provide_nat(entries.len().into()),
                "keys" => {
                    for key in entries.keys() {
                        handle.signal(literal!("item"));
                        handle.send().provide_string(ParString::from(key.clone()));
                    }
                    handle.signal(literal!("end"));
                    handle.break_();
                }
                "list" => {
                    for (key, value) in entries.iter() {
                        handle.signal(literal!("item"));
                        let mut pair = handle.send();
                        pair.send().provide_string(ParString::from(key.clone()));
                        provide_json_value(pair, value.clone());
                    }
                    handle.signal(literal!("end"));
                    handle.break_();
                }
                "get" => {
                    let key = handle.receive().string().await;
                    match entries.get(&key) {
                        Some(value) => {
                            handle.signal(literal!("some"));
                            provide_json_value(handle, value.clone());
                        }
                        None => {
                            handle.signal(literal!("none"));
                            handle.break_();
                        }
                    }
                }
                _ => unreachable!(),
            }
        }
    })
}

fn provide_bool(mut handle: Handle, value: bool) {
    if value {
        handle.signal(literal!("true"));
    } else {
        handle.signal(literal!("false"));
    }
    handle.break_();
}
