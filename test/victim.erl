%%%-------------------------------------------------------------------
%%% @author <vjache@gmail.com>
%%% @copyright (C) 2011, Vyacheslav Vorobyov.  All Rights Reserved.
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%% http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%% @doc
%%%    TODO: Document it.
%%% @end
%%% Created : Nov 20, 2011
%%%-------------------------------------------------------------------------------
-module(victim).

%%
%% Include files
%%
-include_lib("elips/include/elips.hrl").
-include("victim.hrl").
-include("log.hrl"). 

%%
%% Exported Functions
%%
-export([start/0, notify/1]).
-export([init/1,
         handle_pattern/3,
         handle_event/3,
         handle_info/2, 
         code_change/3, 
         terminate/2]).
 
-record(state,{}).

%% 
%% API Functions
%%

start() ->
    elips:start({local, ?MODULE}, ?MODULE, [], []).

notify(Event) ->
    elips:notify(?MODULE, Event).

%%=============================================================
%% ELIPS bahavior callbacks
%%=============================================================

% @doc
% Initialize ELIPS state & insert some data to Working Memory (WM).
% @end
-spec init( Args :: term() ) -> elips:ok_reply() | ignore.
init(_Args) ->
%%     Self=self(),
    State=#state{},
    {ok, 
     State, []}. 
%%      [{assert, {Self, is_a, elips} },
%%       {assert, {Self, is_a, process} },
%%       {assert, {Self, started_at, now()} }]}.

% @doc
% Handle pattern matched at Working Memory of this elips process.
% @end
-spec handle_pattern(Token :: [term()], WMO :: elips:wmo(term()), State :: term() ) ->  elips:ok_reply() | noop.
handle_pattern( 
     [{_A,has,_B},
      {_B,has,_C},
      {_C,is_a,dog}]=P, {assert,_}=_WMO, _State) ->
    ?ECHO({'HANDLE-PATTERN-ASSERT',_WMO, P}),
    noop;
handle_pattern(
     [#person{age=Age},
      {_B,has,_C},
      {_C,is_a,dog}]=P, _, _State) when Age > 3 ->
    ?ECHO({'HANDLE-PATTERN', P}), 
    noop.

% @doc
% Handle an event & do some Working Memory updates.
% @end
-spec handle_event(Event :: term(), FromPid :: pid(), State :: term() ) ->  elips:ok_reply() | noop.
handle_event({'-',_Event}, _FromPid, _State) ->
    ?ECHO({'-HANDLE-EVENT', _Event}),
    {ok,_State,[{retire, _Event}]};
handle_event(_Event, _FromPid, _State) ->
    ?ECHO({'HANDLE-EVENT', _Event}),
    {ok,_State,[{assert, _Event}]}. 

% @doc
% Handle a raw message & do some Working Memory updates.
% @end
-spec handle_info(Msg :: term(), State :: term() ) ->  elips:ok_reply() | noop.
handle_info(_Msg, _State) ->
    noop.

% @doc
% Analogous to gen_server:terminate/2 .
% @end
terminate(_Reason, _State) ->
    ?ECHO({'STACK-TRACE', erlang:get_stacktrace()}),
    ok.

% @doc
% Analogous to gen_server:code_change/2 .
% @end
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%=============================================================
%% Local Functions
%%=============================================================


