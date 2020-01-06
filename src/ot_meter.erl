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
-module(ot_meter).

-callback new_instruments(opentelemetry:meter(), [instrument_opts()]) -> ok.

-callback labels(opentelemetry:meter(), list() | map()) -> label_set().

-callback record(opentelemetry:meter(), bound_instrument(), number()) -> ok.
-callback record(opentelemetry:meter(), instrument(), number(), label_set()) -> ok.

-callback record_batch(opentelemetry:meter(), [{instrument(), number()}], label_set()) -> ok.

-callback bind(opentelemetry:meter(), instrument(), label_set()) -> bound_instrument().
-callback release(opentelemetry:meter(), bound_instrument()) -> ok.

-export([new_instruments/2,
         bind/3,
         release/2,
         record/3,
         record/4,
         record_batch/2]).

-type name() :: unicode:unicode_binary().
-type description() :: unicode:unicode_binary().
-type metric_kind() :: counter | gauge | measure.
-type unit() :: atom().
-type instrument_mode() :: monotonic | non_monotonic | absolute | non_absolute.
-type input_type() :: integer | float.

-type instrument_opts() :: #{name := name(),
                             description => description(),
                             kind := metric_kind(),
                             type => input_type(),
                             label_keys => [label_key()],
                             monotonic => boolean(),
                             absolute => boolean(),
                             unit => unit()}.

-type instrument() :: term().
-type bound_instrument() :: term().

-type label_key() :: unicode:unicode_binary().
-type label_value() :: unicode:unicode_binary().
-type label_set() :: #{label_key() => label_value()}.

-type measurement() :: {bound_instrument() | instrument(), number()}.

-export_type([name/0,
              description/0,
              metric_kind/0,
              input_type/0,
              unit/0,
              instrument_mode/0,
              measurement/0,
              label_set/0]).

-spec new_instruments(opentelemetry:meter(), [instrument_opts()]) -> boolean().
new_instruments(Meter={Module, _}, List) ->
    Module:new_instruments(Meter, List).

-spec bind(opentelemetry:meter(), instrument(), label_set()) -> bound_instrument().
bind(Meter={Module, _}, Instrument, LabelSet) ->
    Module:bind(Meter, Instrument, LabelSet).

-spec release(opentelemetry:meter(), bound_instrument()) -> ok.
release(Meter={Module, _}, BoundInstrument) ->
    Module:release(Meter, BoundInstrument).

-spec record(opentelemetry:meter(), instrument(), number(), label_set()) -> ok.
record(Meter={Module, _}, Instrument, Number, LabelSet) ->
    Module:record(Meter, Instrument, Number, LabelSet).

-spec record(opentelemetry:meter(), bound_instrument(), number()) -> ok.
record(Meter={Module, _}, BoundInstrument, Number) ->
    Module:record(Meter, BoundInstrument, Number).

-spec record_batch(opentelemetry:meter(), [measurement()]) -> ok.
record_batch(Meter={Module, _}, Measurements) ->
    Module:record_batch(Meter, Measurements).
