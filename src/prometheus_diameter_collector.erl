%% Copyright 2019 Travelping GmbH <info@travelping.com>
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

-module(prometheus_diameter_collector).
-behaviour(prometheus_collector).

-include_lib("eunit/include/eunit.hrl").
-include_lib("prometheus/include/prometheus.hrl").

-export([
	 deregister_cleanup/1,
	 collect_mf/2, collect_metrics/2,
	 gather/0
	]).

-import(prometheus_model_helpers, [create_mf/5,
				   gauge_metric/2]).

-define(METRIC_NAME_PREFIX, "diameter_").

-define(METRICS, [{applications, gauge, "Number of installed DIAMETER applications."},
		  {connections, gauge, "Number of connections to peers."},
		  {messages, gauge, "Number of requests."}]).

%%====================================================================
%% Collector API
%%====================================================================

deregister_cleanup(_) -> ok.

collect_mf(_Registry, Callback) ->
    Stats = gather(),
    [mf(Callback, Metric, Stats) || Metric <- ?METRICS],
    ok.

mf(Callback, {Name, Type, Help}, Stats) ->
    Callback(create_mf(?METRIC_NAME(Name), Help, Type, ?MODULE,
		       {Type, fun(S) -> maps:get(Name, S, undefined) end, Stats})),
    ok.

collect_metrics(_, {Type, Fun, Stats}) ->
    case Fun(Stats) of
	M when is_map(M) ->
	    [metric(Type, Labels, Value) || {Labels, Value} <- maps:to_list(M)];
	_ ->
	    undefined
    end.

metric(gauge, Labels, Value) ->
    gauge_metric(Labels, Value).

%%%===================================================================
%%% Internal functions
%%%===================================================================

add([Key], Value, Stats) ->
    maps:update_with(Key, fun(X) -> X + Value end, Value, Stats);
add([Key|Next], Value, Stats) ->
    maps:update_with(Key, fun(S) -> add(Next, Value, S) end, add(Next, Value, #{}), Stats);
add(Key, Value, Stats) ->
    maps:update_with(Key, fun(X) -> X + Value end, Value, Stats).

gather() ->
    Services = diameter:services(),
    lists:foldl(
      fun(SvcName, Stats) ->
	      Info = diameter:service_info(SvcName, [applications, peers]),
	      gather_service(SvcName, Info, Stats)
      end, #{}, Services).

gather_service(SvcName, Info, Stats) ->
    lists:foldl(
      fun({applications, Apps}, S0) ->
	      add([applications, [{svc, SvcName}]], length(Apps), S0);
	 ({peers, Peers}, S0) ->
	      Apps = proplists:get_value(applications, Info, []),
	      lists:foldl(
		fun({PeerName, Peer}, S1) ->
			gather_peer(SvcName, PeerName, Peer, Apps, S1)
		end, S0, Peers)
      end, Stats, Info).

gather_peer(SvcName, Peer, Info, Apps, Stats) ->
    lists:foldl(
      fun({connections, C}, S0) ->
	      lists:foldl(
		fun(X, S1) ->
			gather_connection(SvcName, Peer, X, S1)
		end, S0, C);
	 ({statistics, S}, S0) ->
	      gather_statistics(SvcName, Peer, S, Apps, S0)
      end, Stats, Info).

gather_connection(SvcName, Peer, Values, Stats) ->
    {_, _, State} = proplists:get_value(watchdog, Values, {undefine, undefined, unknown}),
    Type = case proplists:get_value(type, Values, unknown) of
	       accept  -> responder;
	       connect -> initiator;
	       Other -> Other
	   end,
    Port = proplists:get_value(port, Values, []),
    Connection = case proplists:get_value(module, Port, unknown) of
		     diameter_tcp -> tcp;
		     diameter_sctp -> sctp;
		     OtherTP -> OtherTP
		 end,
    add([connections, [{svc, SvcName}, {peer, Peer}, {type, Type},
		       {state, State}, {protocol, Connection}]], 1, Stats).

gather_statistics(SvcName, Peer, S, Apps, Stats) ->
    lists:foldl(
      fun({{{_, _, 1} = Msg, Direction}, Cnt}, S1) ->
	      add([messages, [{svc, SvcName}, {peer, Peer},
			      {direction, Direction},
			      {type, msg_type(Msg)},
			      {msg, msg_name(Msg, Apps)}]], Cnt, S1);
	 ({{Msg, Direction, {'Result-Code', RC}}, Cnt}, S1) ->
	      add([messages, [{svc, SvcName}, {peer, Peer},
			      {direction, Direction},
			      {type, msg_type(Msg)},
			      {msg, msg_name(Msg, Apps)},
			      {rc, RC}]], Cnt, S1);
	 ({{Msg, Direction, Result}, Cnt}, S1) when is_atom(Result) ->
	      add([messages, [{svc, SvcName}, {peer, Peer},
			      {direction, Direction},
			      {type, msg_type(Msg)},
			      {msg, msg_name(Msg, Apps)},
			      {rc, Result}]], Cnt, S1);
	 (_, S1) ->
	      S1
      end, Stats, S).

msg_type({_, 0}) -> answer;
msg_type({_, 1}) -> request;
msg_type({_, _, 0}) -> answer;
msg_type({_, _, 1}) -> request.

try_dict(Dict, {_, CmdCode, Rbit} = Cmd) ->
    case code:is_loaded(Dict) of
	{file, _} ->
	    try Dict:msg_name(CmdCode, Rbit =:= 1) of
		'' -> Cmd;
		Name when is_atom(Name) -> Name;
		_ -> Cmd
	    catch
		_:_ ->
		    Cmd
	    end;
	_ ->
	    Cmd
    end.

msg_name({0, _, _} = Cmd, _Apps) ->
    case try_dict(diameter_gen_base_rfc6733, Cmd) of
	Name when is_atom(Name) ->
	    Name;
	_ ->
	    try_dict(diameter_gen_base_rfc3588, Cmd)
    end;
msg_name({ApplId, _, _} = Cmd, Apps) ->
    case lists:filter(
	   fun(E) -> ApplId =:= proplists:get_value(id, E, -1) end, Apps) of
	[App|_] ->
	    try_dict(proplists:get_value(dictionary, App, undefined), Cmd);
	_ ->
	    Cmd
    end;
msg_name(_, _Apps) -> 
    unknown.

%%%===================================================================
%%% A small eunit test to verify the stat collection
%%%===================================================================

gather_statistics_test() ->
    S = [{{{0,257,1},send},1},
	 {{{unknown,0},recv,discarded},2618},
	 {{{0,257,0},recv,{'Result-Code',2001}},1}],
    R = #{messages => #{
	[{svc,testsvc}, {peer,<<"testpeer">>}, {direction,recv}, {type,answer}, {msg,unknown}, {rc,discarded}] => 2618,
	[{svc,testsvc}, {peer,<<"testpeer">>}, {direction,recv}, {type,answer}, {msg,{0,257,0}}, {rc,2001}] => 1,
	[{svc,testsvc}, {peer,<<"testpeer">>}, {direction,send}, {type,request},{msg,{0,257,1}}] => 1
    }},
    R = gather_statistics(testsvc, <<"testpeer">>, S, [], #{}).
