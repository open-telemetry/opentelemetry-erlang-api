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
-module(ot_observer).

-export([set_callback/3,
         observe/3]).

%% function called with an `observer_result' argument to update observer
-type callback() :: fun((observer_result()) -> ok).

%% value containing all information needed by the SDK to record an update
-type observer_result() :: term().

-export_type([callback/0]).

-spec set_callback(opentelemetry:meter(), ot_meter:name(), callback()) -> boolean().
set_callback(Meter, Observer, Callback) ->
    ot_meter:set_observer_callback(Meter, Observer, Callback).

-spec observe(observer_result(), number(), ot_meter:label_set()) -> ok.
observe(ObserverResult, Number, LabelSet) ->
    ot_meter:observe(ObserverResult, Number, LabelSet).
