%% Copyright 2021 Travelping GmbH <info@travelping.com>
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(prometheus_diameter_collector_SUITE).

%%% ==================================================================
%%% Common Tests Callbacks Exports
%%% ==================================================================

-export([
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1
]).

%%% ==================================================================
%%% Common Tests Info Callbacks Exports
%%% ==================================================================

-export([
    diameter_applications/0,
    diameter_connections/0,
    diameter_messages/0,
    gather_statistics/0
]).

%%% ==================================================================
%%% Common Tests Exports
%%% ==================================================================

-export([
    diameter_applications/1,
    diameter_connections/1,
    diameter_messages/1,
    gather_statistics/1
]).

%%% ==================================================================
%%% Includes
%%% ==================================================================

-include_lib("common_test/include/ct.hrl").

%%% ==================================================================
%%% Macros
%%% ==================================================================

-define(match(Guard, Expr),
    ((fun () ->
        case (Expr) of
            Guard ->
                ok;
            V ->
                ct:pal("MISMATCH(~s:~b, ~s)~nExpected: ~p~nActual:   ~p~n",
                [?FILE, ?LINE, ??Expr, ??Guard, V]),
                error(badmatch)
        end
    end)())).

%%% ==================================================================
%%% Common Tests Callbacks
%%% ==================================================================

all() ->
  [
    {group, collector}
  ].

groups() ->
    [
        {collector, [sequence], [
            diameter_applications,
            diameter_connections,
            diameter_messages,
            gather_statistics
        ]}
    ].

init_per_suite(Config) ->
    ok = diameter:start(),
    ok = prometheus:start(),
    ok = prometheus_registry:register_collector(pdc, prometheus_diameter_collector),
    Config.

end_per_suite(Config) ->
    ok = diameter:stop(),
    ok = prometheus:stop(),
    Config.

%%% ==================================================================
%%% Common Tests
%%% ==================================================================

%% -------------------------------------------------------------------
diameter_applications() ->
    [{doc, "Check diameter applications"}].
diameter_applications(_Config) ->
    Metrics = prometheus_text_format:format(pdc),
    ?match({match, _}, re:run(Metrics, "diameter_applications")).

%% -------------------------------------------------------------------
diameter_connections() ->
    [{doc, "Check diameter connections"}].
diameter_connections(_Config) ->
    Metrics = prometheus_text_format:format(pdc),
    ?match({match, _}, re:run(Metrics, "diameter_connections")).

%% -------------------------------------------------------------------
diameter_messages() ->
    [{doc, "Check diameter messages"}].
diameter_messages(_Config) ->
    Metrics = prometheus_text_format:format(pdc),
    ?match({match, _}, re:run(Metrics, "diameter_messages")).

%% -------------------------------------------------------------------
gather_statistics() ->
    [{doc, "Check gather statistics - verify the stat collection"}].
gather_statistics(_Config) ->
    S = [{{{0,257,1},send},1},
         {{{unknown,0},recv,discarded},2618},
         {{{0,257,0},recv,{'Result-Code',2001}},1}],
    R = #{messages => #{
        [{svc,testsvc}, {peer,<<"testpeer">>}, {direction,recv}, {type,answer}, {msg,'CEA'}, {rc,2001}] => 1,
        [{svc,testsvc}, {peer,<<"testpeer">>}, {direction,recv}, {type,answer}, {msg,unknown}, {rc,discarded}] => 2618,
        [{svc,testsvc}, {peer,<<"testpeer">>}, {direction,send}, {type,request}, {msg,'CER'}] => 1}},
    ?match(R, prometheus_diameter_collector:gather_statistics(testsvc, <<"testpeer">>, S, [], #{})).
