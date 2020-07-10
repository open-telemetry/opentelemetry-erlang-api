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
-module(ot_instrument).

-callback new(opentelemetry:meter(), ot_meter:name()) -> boolean().
-callback new(opentelemetry:meter(), ot_meter:name(), ot_meter:instrument_opts()) -> boolean().

-callback instrument_config() -> ot_meter:instrument_config().

-callback add(ot_meter:bound_instrument(), number()) -> ok.
-callback add(opentelemetry:meter(), ot_meter:name(), number(), ot_meter:labels()) -> ok.

-callback record(ot_meter:bound_instrument(), number()) -> ok.
-callback record(opentelemetry:meter(), ot_meter:name(), number(), ot_meter:labels()) -> ok.

-callback measurement(ot_meter:bound_instrument() | ot_meter:name(), number()) ->
    {ot_meter:bound_instrument() | ot_meter:name(), number()}.

-optional_callbacks([add/2,
                     add/4,
                     record/2,
                     record/4,
                     measurement/2]).
