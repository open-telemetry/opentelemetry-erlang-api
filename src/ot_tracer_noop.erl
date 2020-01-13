%%%------------------------------------------------------------------------
%% Copyright 2019, OpenTelemetry Authors
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% @doc
%% @end
%%%-------------------------------------------------------------------------
-module(ot_tracer_noop).

-behaviour(ot_tracer).

-export([start_span/3,
         set_span/2,
         end_span/2,
         span_module/1,
         current_span_ctx/1,
         get_binary_format/1,
         get_http_text_format/1]).

-include("opentelemetry.hrl").

-define(NOOP_SPAN_CTX, #span_ctx{trace_id=0,
                                 span_id=0,
                                 trace_flags=0,
                                 tracestate=[],
                                 is_valid=false,
                                 is_recorded=false}).

-spec start_span(opentelemetry:tracer(), opentelemetry:span_name(), ot_span:start_opts()) -> opentelemetry:span_ctx().
start_span(_, _Name, _) ->
    ?NOOP_SPAN_CTX.

-spec set_span(opentelemetry:tracer(), opentelemetry:span_ctx()) -> ok.
set_span(_, _SpanCtx) ->
    ok.

-spec current_span_ctx(opentelemetry:tracer()) -> opentelemetry:span_ctx().
current_span_ctx(_) ->
    ?NOOP_SPAN_CTX.

span_module(_) ->
    ot_span_noop.

-spec end_span(opentelemetry:tracer(), opentelemetry:span_ctx()) -> ok.
end_span(_, _) ->
    ok.

-spec get_binary_format(opentelemetry:tracer()) -> binary().
get_binary_format(_) ->
    <<>>.

-spec get_http_text_format(opentelemetry:tracer()) -> opentelemetry:http_headers().
get_http_text_format(_) ->
    [].
