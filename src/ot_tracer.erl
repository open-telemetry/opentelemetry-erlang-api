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
-module(ot_tracer).

-export([start_span/3,
         set_span/2,
         current_span_ctx/1,
         end_span/1,
         end_span/2]).

%% tracer access functions
-export([span_module/1]).

-include("opentelemetry.hrl").

-callback start_span(opentelemetry:tracer(),
                     opentelemetry:span_name(),
                     ot_span:start_opts()) -> opentelemetry:span_ctx().
-callback with_span(opentelemetry:tracer(), opentelemetry:span_ctx()) -> ok.
-callback with_span(opentelemetry:tracer(), opentelemetry:span_ctx(), fun()) -> ok.
-callback end_span(opentelemetry:tracer(), opentelemetry:span_ctx()) -> ok.
-callback current_span_ctx(opentelemetry:tracer()) -> opentelemetry:span_ctx().
-callback span_module(opentelemetry:tracer()) -> module().

-spec start_span(opentelemetry:tracer(), opentelemetry:span_name(),
                 ot_span:start_opts()) -> opentelemetry:span_ctx().
start_span(Tracer={Module, _}, Name, Opts) ->
    Module:start_span(Tracer, Name, Opts).

-spec set_span(opentelemetry:tracer(), opentelemetry:span_ctx()) -> ok.
set_span(Tracer={Module, _}, SpanCtx) ->
    Module:set_span(Tracer, SpanCtx).

-spec end_span(opentelemetry:tracer()) -> ok.
end_span(Tracer={Module, _}) ->
    end_span(Tracer, Module:current_span_ctx(Tracer)).

-spec end_span(opentelemetry:tracer(), opentelemetry:span_ctx()) -> ok.
end_span(Tracer={Module, _}, SpanCtx) ->
    Module:end_span(Tracer, SpanCtx).

current_span_ctx(Tracer={Module, _}) ->
    Module:current_span_ctx(Tracer).

%% tracer access functions

span_module(Tracer={Module, _}) ->
    Module:span_module(Tracer).
