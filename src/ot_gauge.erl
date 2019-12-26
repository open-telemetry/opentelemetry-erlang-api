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
-module(ot_gauge).

-export([set/3,
         set/4,
         measurement/3]).

-spec set(opentelemetry:meter(), ot_meter:name(), number(), ot_meter:label_set()) -> ok.
set(Meter, Name, Number, LabelSet) ->
    ot_meter:record(Meter, ot_meter:bind(Meter, Name, LabelSet), Number).

-spec set(opentelemetry:meter(), ot_meter:bound_instrument(), number()) -> ok.
set(Meter, BoundInstrument, Number) ->
    ot_meter:record(Meter, BoundInstrument, Number).

-spec measurement(opentelemetry:meter(), ot_meter:bound_instrument() | ot_meter:name(), number())
                 -> {ot_meter:bound_instrument() | ot_meter:name(), number()}.
measurement(_Meter, NameOrInstrument, Number) ->
    {NameOrInstrument, Number}.
