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
-module(ot_updown_counter).

-behaviour(ot_instrument).

-export([new/2,
         new/3,
         instrument_config/0,
         add/2,
         add/4,
         measurement/2]).

-include("meter.hrl").

-spec new(opentelemetry:meter(), ot_meter:name()) -> boolean().
new(Meter, Name) ->
    new(Meter, Name, #{}).

-spec new(opentelemetry:meter(), ot_meter:name(), ot_meter:instrument_opts()) -> boolean().
new(Meter, Name, Opts) ->
    ot_meter:new_instrument(Meter, Name, ?KIND_UPDOWN_COUNTER, Opts).

-spec instrument_config() -> ot_meter:instrument_config().
instrument_config() ->
    #{monotonic => false,
      synchronous => true}.

-spec add(ot_meter:bound_instrument(), number()) -> ok.
add(BoundInstrument, Number) ->
    ot_meter:record(BoundInstrument, Number).

-spec add(opentelemetry:meter(), ot_meter:name(), number(), ot_meter:label_set()) -> ok.
add(Meter, Name, Number, LabelSet) ->
    ot_meter:record(Meter, Name, Number, LabelSet).

-spec measurement(ot_meter:bound_instrument() | ot_meter:name(), number())
                 -> {ot_meter:bound_instrument() | ot_meter:name(), number()}.
measurement(NameOrInstrument, Number) ->
    {NameOrInstrument, Number}.
