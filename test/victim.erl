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
-include_lib("eunit/include/eunit.hrl").

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
 
-record(state,{reply_to}).

%% 
%% API Functions
%%

start() ->
    elips:start({local, ?MODULE}, ?MODULE, tester, []).

notify(Event) ->
    elips:notify(?MODULE, Event).

%%=============================================================
%% ELIPS behavior callbacks
%%=============================================================

% @doc
% Initialize ELIPS state & insert some data to Working Memory (WM).
% @end
-spec init( Args :: term() ) -> elips:ok_reply() | ignore.
init(ReplyPid) ->
    State=#state{reply_to=ReplyPid},
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
      {_C,is_a,dog}]=P, 
     #assert{}, #state{reply_to=ReplyPid}=_State) ->
    ReplyPid ! P,
    noop;
handle_pattern( 
     [{_A, on, _B},
      {_B, on, _C},
      {_D, near, _E}]=P, 
     #assert{}, #state{reply_to=ReplyPid}=_State) ->
    ReplyPid ! P,
    noop;
handle_pattern(
    P, #retire{}, #state{reply_to=ReplyPid}=_State) -> 
    ReplyPid ! {retire, P},
    noop. 

% @doc
% Handle an event & do some Working Memory updates.
% @end
-spec handle_event(Event :: term(), FromPid :: pid(), State :: term() ) ->  elips:ok_reply() | noop.
handle_event({'-',_Event}, _FromPid, _State) ->
    {ok,_State,[{retire, _Event}]};
handle_event(_Event, _FromPid, _State) ->
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

%%=============================================================
%% EUnit Functions
%%=============================================================

init_per_testcase() ->
    {ok,SrvPid}=start(),SrvPid.
end_per_testcase(_InitResult) ->
    elips:shutdown(?MODULE).

check_victim_alive() ->
    timer:sleep(30),
    ?assert(erlang:is_process_alive(whereis(?MODULE))).

receive_result() ->
    receive
        Result -> Result
        after
            100 ->  throw(timeout)
    end.

%% ensure_mbox_empty() ->
%%     receive
%%         Result -> throw({mbox_not_empty, Result})
%%         after
%%             1 ->  ok
%%     end.

basic_test_() ->
    {foreach,fun init_per_testcase/0, fun end_per_testcase/1,
     [% Test rule with 'connected-by-var' fact patterns
      fun() ->
              check_victim_alive(),
              
              % Register self as a receiver of 'victim's messages
              register(tester, self()),
              
              %% Do ASSERTs
              
              % Send facts to an agent
              notify({a, has, b}),
              notify({b, has, c}),
              notify({c, is_a, dog}),
              ?assertMatch([{_A,has,_B},{_B,has,_C},{_C,is_a,dog}], 
                           receive_result()),
              
              % Send again & the same. Ensure rule will not be activated
              notify({a, has, b}),
              notify({b, has, c}),
              notify({c, is_a, dog}),
              ?assertThrow(timeout, receive_result()),
              
              % Send facts to an agent for the same pattern but in 'arbitrar' ordered
              notify({c1, is_a, dog}),
              notify({a1, has, b1}),
              notify({b1, has, c1}),
              ?assertMatch([{_A,has,_B},{_B,has,_C},{_C,is_a,dog}], 
                           receive_result()),
              
              % Send fact that must trigger(activate) a rule due to reuse of data already in WM
              notify({a2, has, b1}),
              ?assertMatch([{a2,has,_B},{_B,has,_C},{_C,is_a,dog}], 
                           receive_result()),
              
              %% Do RETIREs
              
              % Send fact that must be removed from WM and trigger(activate) a rule due to 
              notify({'-', {a2, has, b1}}),
              ?assertMatch({retire, [{a2,has,_B},{_B,has,_C},{_C,is_a,dog}]}, 
                           receive_result()),
              
              % Ensure that previous operation being repeated has no effect
              notify({'-', {a2, has, b1}}),
              ?assertThrow(timeout, receive_result()),
              
              % Ensure that some 'garbage' operations no effect
              notify({g, g, g, g}),
              ?assertThrow(timeout, receive_result()),
              
              notify({'-', {g1, g1, g1, g1} }),
              ?assertThrow(timeout, receive_result())
      end,
      % Test rule with 'disconnected-by-var' fact patterns
      fun() ->
              check_victim_alive(),
              
              % Register self as a receiver of 'victim's messages
              register(tester, self()),
              
              %% Do ASSERTs
              
              % Send facts to an agent
              notify({a, on, b}),
              notify({b, on, c}),
              notify({d, near, e}),
              ?assertMatch([{_A, on, _B},{_B, on, _C},{_D, near, _E}], receive_result())
      end]}.

