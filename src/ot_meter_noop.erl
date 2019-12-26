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
-module(ot_meter_noop).

-export([new_instruments/2,
         labels/2,
         record/3,
         record/4,
         record_batch/3,
         bind/3,
         release/2]).

new_instruments(_, _) ->
    [].

labels(_, _) ->
    #{}.

record(_, _, _) ->
    ok.

record(_, _, _, _) ->
    ok.

record_batch(_, _, _) ->
    ok.

bind(_, _, _) ->
    [].

release(_, _) ->
    ok.
