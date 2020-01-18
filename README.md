# Erlang OpenTelemetry API

This is the API portion of [OpenTelemetry](https://opentelemetry.io/) for Erlang and Elixir applications.

This is a library, it does not start any processes, and should be the only OpenTelemetry dependency of Erlang/Elixir applications. 

The end user of your application can then choose to include the [OpenTelemetry implementation](https://github.com/open-telemetry/opentelemetry-erlang) application. If the implementation application is not in the final release the OpenTelemetry instrumentation will all be noops. This means no processes started, no ETS tables created and nothing added to the process dictionary.

This separation is done so you should feel comfortable instrumenting your Erlang/Elixir application with OpenTelemetry and not worry that a complicated dependency is being forced on your users.

## Use

When instrumenting an application to be used as a dependency of other projects it is best practice to register a `Tracer` with a name and a version using the application's name and version.

If it is a runnable application then this registration should happen in `start/2`, example below is adding `Tracer` registration to the Postgres library [pgo](https://github.com/erleans/pgo):

``` erlang
start(_StartType, _StartArgs) ->
...
    {ok, Vsn} = application:get_key(pgo, vsn),
    opentelemetry:register_tracer(pgo, Vsn),
...
```

Then when the spans are started and finished in the application's code the `Tracer` is fetched with `get_tracer/1` and passed to `with_span/3` or `start_span/3`:

``` erlang
Tracer = opentelemetry:get_tracer(pgo),
ot_tracer:with_span(Tracer, <<"pgo:query/3">>, fun() -> ... end).
```

Naming the `Tracers` allows the user of your application to disable the traces from the dependency if it is needed.

If the application does not have a `start/2` there may be another function that is always called before the library would create any spans. For example, the [Elli](https://github.com/elli-lib/elli) middleware for OpenTelemetry instrumentation registers the `Tracer` during Elli startup:

``` erlang
handle_event(elli_startup, _Args, _Config) ->
    {ok, Vsn} = application:get_key(opentelemetry_elli, vsn),
    _ = opentelemetry:register_tracer(opentelemetry_elli, Vsn),
    ok;
```

When there is no startup of any kind to hook into in the library itself it should export a function `register_tracer/0` to be used by any application that depends on it to do the registration:

``` erlang
-module(mylib).

-export([register_tracer/0]).

register_tracer() ->
    {ok, Vsn} = application:get_key(mylib, vsn),
    _ = opentelemetry:register_tracer(mylib, Vsn),
    ok.
```

Not registering does not cause any issues or crashes, OpenTelemetry simply will fallback to the default `Tracer` if `get_tracer/1` is called with a name that is not registered.


