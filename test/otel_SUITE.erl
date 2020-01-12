-module(otel_SUITE).

-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

-include("opentelemetry.hrl").

all() ->
    [with_span].

init_per_suite(Config) ->
    application:load(opentelemetry_api),
    Config.

end_per_suite(_Config) ->
    ok.

with_span(_Config) ->
    SpanCtx1 = otel:start_span(<<"span1">>),

    %% Returns execution value of passed function
    ?assertEqual(foo, otel:with_span(<<"span">>, fun (_Ctx) -> foo end)),
    %% Resets span context to te one before the call
    ?assertEqual(SpanCtx1, otel:current_span_ctx()).
