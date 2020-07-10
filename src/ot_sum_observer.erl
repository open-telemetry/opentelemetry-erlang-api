%%%------------------------------------------------------------------------
%% Copyright 2020, OpenTelemetry Authors
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
-module(ot_sum_observer).

-behaviour(ot_instrument).
-behaviour(ot_observer).

-export([new/2,
         new/3,
         instrument_config/0,
         set_callback/3,
         observe/3]).

-include("meter.hrl").

-spec new(opentelemetry:meter(), ot_meter:name()) -> boolean().
new(Meter, Name) ->
    new(Meter, Name, #{}).

-spec new(opentelemetry:meter(), ot_meter:name(), ot_meter:instrument_opts()) -> boolean().
new(Meter, Name, Opts) ->
    ot_meter:new_instrument(Meter, Name, ?KIND_SUM_OBSERVER, Opts).

-spec instrument_config() -> ot_meter:instrument_config().
instrument_config() ->
    #{monotonic => true,
      synchronous => false}.

-spec set_callback(opentelemetry:meter(), ot_meter:name(), ot_observer:callback()) -> ok.
set_callback(Meter, Observer, Callback) ->
    ot_meter:set_observer_callback(Meter, Observer, Callback).

-spec observe(ot_observer:instrument(), number(), ot_meter:labels()) -> ok.
observe(ObserverInstrument, Number, LabelSet) ->
    ot_meter:observe(ObserverInstrument, Number, LabelSet).
