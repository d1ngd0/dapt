use criterion::{black_box, criterion_group, criterion_main, Criterion};
use dapt::query::{QueryResult, WhereClause};
use dapt::Dapt;

const JSON: &str = r#"{"version":1,"url":"https://www.elastic.co/guide/en/ecs/current/index.html","ecs":{"version":"1.x"},"fields":{"@timestamp":{"type":"datetime","required":true,"index":0,"url":"https://www.elastic.co/guide/en/ecs/current/ecs-base.html","comment":["Field order, as specified by 'index', is RECOMMENDED.","ECS loggers must implement field order unless the logging framework makes that impossible."]},"log.level":{"type":"string","required":true,"index":1,"top_level_field":true,"url":"https://www.elastic.co/guide/en/ecs/current/ecs-log.html","comment":["This field SHOULD NOT be a nested object field but at the top level with a dot in the property name.","This is to make the JSON logs more human-readable.","Loggers MAY indent the log level so that the `message` field always starts at the exact same offset,","no matter the number of characters the log level has.","For example: `'DEBUG'` (5 chars) will not be indented, whereas ` 'WARN'` (4 chars) will be indented by one space character."]},"message":{"type":"string","required":false,"index":2,"url":"https://www.elastic.co/guide/en/ecs/current/ecs-base.html","comment":["A message field is typically included in all log records, but some logging libraries allow records with no message.","That's typically the case for libraries that allow for structured logging."]},"ecs.version":{"type":"string","required":true,"top_level_field":true,"url":"https://www.elastic.co/guide/en/ecs/current/ecs-ecs.html","comment":["This field SHOULD NOT be a nested object field but at the top level with a dot in the property name.","This is to make the JSON logs more human-readable."]},"labels":{"type":"object","required":false,"url":"https://www.elastic.co/guide/en/ecs/current/ecs-base.html","sanitization":{"key":{"replacements":[".","*","\\"],"substitute":"_"}}},"trace.id":{"type":"string","required":false,"url":"https://www.elastic.co/guide/en/ecs/current/ecs-tracing.html","comment":"When APM agents add this field to the context, ecs loggers should pick it up and add it to the log event."},"transaction.id":{"type":"string","required":false,"url":"https://www.elastic.co/guide/en/ecs/current/ecs-tracing.html","comment":"When APM agents add this field to the context, ecs loggers should pick it up and add it to the log event."},"service.name":{"type":"string","required":false,"url":"https://www.elastic.co/guide/en/ecs/current/ecs-service.html","comment":["Configurable by users.","When an APM agent is active, it should auto-configure this field if not already set."]},"service.node.name":{"type":"string","required":false,"url":"https://www.elastic.co/guide/en/ecs/current/ecs-service.html","comment":["Configurable by users.","When an APM agent is active and `service_node_name` is manually configured, the agent should auto-configure this field if not already set."]},"service.version":{"type":"string","required":false,"url":"https://www.elastic.co/guide/en/ecs/current/ecs-service.html#field-service-version","comment":["Configurable by users.","When an APM agent is active, it should auto-configure it if not already set."]},"event.dataset":{"type":"string","required":false,"url":"https://www.elastic.co/guide/en/ecs/current/ecs-event.html","default":"${service.name} OR ${service.name}.${appender.name}","comment":["Configurable by users.","If the user manually configures the service name,","the logging library should set `event.dataset=${service.name}` if not explicitly configured otherwise.","","When agents auto-configure the app to use an ECS logger,","they should set `event.dataset=${service.name}.${appender.name}` if the appender name is available in the logging library.","Otherwise, agents should also set `event.dataset=${service.name}`","","The field helps to filter for different log streams from the same pod, for example and is required for log anomaly detection."]},"service.environment":{"type":"string","required":false,"url":"https://www.elastic.co/guide/en/ecs/current/ecs-service.html#field-service-environment","comment":["Configurable by users.","When an APM agent is active, it should auto-configure it if not already set."]},"process.thread.name":{"type":"string","required":false,"url":"https://www.elastic.co/guide/en/ecs/current/ecs-process.html"},"log.logger":{"type":"string","required":false,"url":"https://www.elastic.co/guide/en/ecs/current/ecs-log.html"},"log.origin.file.line":{"type":"integer","required":false,"url":"https://www.elastic.co/guide/en/ecs/current/ecs-log.html","comment":"Should be opt-in as it requires the logging library to capture a stack trace for each log event."},"log.origin.file.name":{"type":"string","required":false,"url":"https://www.elastic.co/guide/en/ecs/current/ecs-log.html","comment":"Should be opt-in as it requires the logging library to capture a stack trace for each log event."},"log.origin.function":{"type":"string","required":false,"url":"https://www.elastic.co/guide/en/ecs/current/ecs-log.html","comment":"Should be opt-in as it requires the logging library to capture a stack trace for each log event."},"error.type":{"type":"string","required":false,"url":"https://www.elastic.co/guide/en/ecs/current/ecs-error.html","comment":"The exception type or class, such as `java.lang.IllegalArgumentException`."},"error.message":{"type":"string","required":false,"url":"https://www.elastic.co/guide/en/ecs/current/ecs-error.html","comment":"The message of the exception."},"error.stack_trace":{"type":"string","required":false,"url":"https://www.elastic.co/guide/en/ecs/current/ecs-error.html","comment":"The stack trace of the exception as plain text."}}}"#;

fn bench_json_deserialize(s: &str) -> Dapt {
    serde_json::from_str(s).unwrap()
}

fn bench_cbor_deserialize(s: &[u8]) -> Dapt {
    serde_cbor::from_slice(s).unwrap()
}

fn bench_yaml_deserialize(s: &str) -> Dapt {
    serde_yaml::from_str(s).unwrap()
}

fn bench_json_value_deserialize(s: &str) -> serde_json::Value {
    serde_json::from_str(s).unwrap()
}

fn bench_filter(d: &Dapt, f: &WhereClause) -> QueryResult<bool> {
    f.filter(d)
}

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("deserialize_json", |b| {
        black_box(b.iter(|| bench_json_deserialize(black_box(JSON))))
    });

    c.bench_function("deserialize_json_value", |b| {
        black_box(b.iter(|| bench_json_value_deserialize(black_box(JSON))))
    });

    let cbor = serde_cbor::to_vec(&serde_json::from_str::<Dapt>(JSON).unwrap()).unwrap();
    c.bench_function("deserialize_cbor", |b| {
        black_box(b.iter(|| bench_cbor_deserialize(black_box(&cbor))))
    });

    let yaml = serde_yaml::to_string(&serde_json::from_str::<Dapt>(JSON).unwrap()).unwrap();
    c.bench_function("deserialize_yaml", |b| {
        black_box(b.iter(|| bench_yaml_deserialize(black_box(&yaml))))
    });

    let d = serde_json::from_str::<Dapt>(JSON).unwrap();
    let f = WhereClause::new("WHERE \"~.type\" == 'string'").unwrap();
    c.bench_function("recursive_filter", |b| {
        black_box(b.iter(|| bench_filter(black_box(&d), black_box(&f))))
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
