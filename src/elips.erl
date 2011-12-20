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
%%%
%%% The ELIPS agent behavior. The idea behind this module is analogous to 
%%% 'gen_server' and 'gen_event'. Hence it starts a process that receives 
%%% a messages (events) and handles them in some way using a provided user 
%%% callback module. Most of callback functions looks like ones in 
%%% 'gen_server' or 'gen_event' except 'handle_pattern'. This callback 
%%% called when in a working memory appears a set of facts that matches one 
%%% of the rules. More over, rules are actually clauses of handle_pattern.  
%%%
%%% The user module should export:
%%%
%%%   init(Args)  
%%%     ==> {ok, State}
%%%         {ok, State, WorkingMemoryOps}
%%%         {ok, State, WorkingMemoryOps, Timeout}
%%%         ignore
%%%
%%%   handle_event(Event, FromPid, State)
%%%
%%%    ==>  {ok, State}
%%%         {ok, State, WorkingMemoryOps}
%%%         {ok, State, WorkingMemoryOps, Timeout}
%%%         noop
%%%
%%%   handle_info(Info, State) Info is e.g. {'EXIT', P, R}, {nodedown, N}, ...
%%%
%%%    ==>  {ok, State}
%%%         {ok, State, WorkingMemoryOps}
%%%         {ok, State, WorkingMemoryOps, Timeout}
%%%         noop
%%%
%%%   handle_pattern(Pattern, WorkingMemoryOp, State)
%%%
%%%    ==>  {ok, State}
%%%         {ok, State, WorkingMemoryOps}
%%%         noop
%%%
%%%   terminate(Reason, State) Let the user module clean up
%%%        always called when server terminates
%%%
%%%    ==> ok
%%%
%%%   The main concept behind these callback functions is:
%%%     1. client code interacts with ELIPS agent sending events to it using elips:notify/2
%%%     2. ELIPS agent accepts and interprets events with CallbackModule:handle_event and a 
%%%       result of such an interpretation may be a set of asserts and/or retires (WMOs)
%%%     3. The WMOs obtained at the previous step are applied to a working memory
%%%     4. If some rules match with working memory they become activated and 
%%%       handled with CallbackModule:handle_pattern. The CallbackModule:handle_pattern may
%%%       result in its turn with a set of WMOs (asserts and/or retires) again, in this case 
%%%       all things repeated starting from step 3. etc..
%%%     5. The CallbackModule:handle_info is like CallbackModule:handle_even but designed 
%%%        mainly for handling system events like {'EXIT', P, R}, {nodedown, N}, {nodeup, N} etc. 
%%%        or for those messages that sent not through elips:notify but just with '!'.  
%%%
%%% @end
%%% Created : Nov 20, 2011
%%%-------------------------------------------------------------------------------
-module(elips).

-behaviour(gen_server).

%%
%% Include files
%%
-include_lib("stdlib/include/ms_transform.hrl").

-include("log.hrl").
-include("rete.hrl").
-include("public.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([start_link/4,
         start/4,
         notify/2,
         shutdown/1,
         behaviour_info/1]).

%% gen_server callbacks
-export([init/1, 
         handle_call/3, 
         handle_cast/2, 
         handle_info/2, 
         terminate/2, 
         code_change/3]).

%
-record(state, 
        {estate :: any(), 
         emodule :: atom(), 
         wm_ets :: ets:tab()}).

%
% Export Types
%
-export_type([wmo/0, ok_reply/0]).

% Working Memory Operation
-type wmo() :: #assert{} | #retire{} .
% A standard reply of some behavior functions
-type ok_reply() :: {ok, State :: term()} | 
                    {ok, State :: term(), WMOs :: [wmo()]} | 
                    {ok, State :: term(), WMOs :: [wmo()], Timeout :: non_neg_integer() | infinity}.

%% ====================================================================
%% External functions
%% ====================================================================

-spec behaviour_info(atom()) -> 'undefined' | [{atom(), arity()}].

behaviour_info(callbacks) ->
    [{init,1},
     {handle_pattern,3},
     {handle_event,3}, 
     {handle_info,2},
     {terminate,2},
     {code_change,3},
     % Generated functions
     {alpha_nodes,0},
     {beta_node,1}];
behaviour_info(_Other) ->
    undefined.

%% @doc
%%  Starts an elips agent with specified behavior and links it with process caller.
%% @end
start_link(ServerName,
           ElipsBehavior,
           Args,
           Opts) ->
    gen_server:start_link(ServerName, ?MODULE, {ElipsBehavior, Args}, Opts).

%% @doc
%%  Starts an elips agent with specified behavior.
%% @end
start(ServerName,
           ElipsBehavior,
           Args,
           Opts) ->
    gen_server:start(ServerName, ?MODULE, {ElipsBehavior, Args}, Opts).

%% @doc
%%  Shuts down an elips agent process.
%% @end
shutdown(ServerRef) ->
    ok=gen_server:call(ServerRef, shutdown).

%% @doc
%%  Send event to an elips agent.
%% @end
notify(ServerRef, Event) ->
    ok=gen_server:cast(ServerRef, {'$elips_event', self(), Event}).

%% ====================================================================
%% Server functions
%% ====================================================================

init({Module, Args}) ->
    WMEts=ets:new(working_memory, [bag]),
    try 
        case Module:init(Args) of
            {ok, EState} ->
                {ok, #state{estate=EState,emodule=Module,wm_ets=WMEts}};
            {ok, EState, Data} ->
                EState1=handle_data(EState, Module, WMEts, Data),
                {ok, #state{estate=EState1,emodule=Module,wm_ets=WMEts}};
            {ok, EState, Data, Timeout} ->
                EState1=handle_data(EState, Module, WMEts, Data),
                {ok, #state{estate=EState1,emodule=Module,wm_ets=WMEts}, Timeout};
            ignore ->
                ignore;
            BadRet ->
                {stop, {unexpected_return_value, {Module, init, [Args]}, BadRet}}
        end
    catch
        _: Reason ->
            {stop, {error,[{reason, Reason}, {stacktrace, erlang:get_stacktrace()}]}}
    end.

handle_call(shutdown, _From, State) ->
    {stop, shutdown, ok, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({'$elips_event', FromPid, Msg}, 
            #state{estate=EState,emodule=Module}=State) when is_pid(FromPid) ->
    handle_({Module, handle_event, [Msg, FromPid, EState]}, State);
handle_cast(_Msg, State) -> {noreply, State}.

handle_info(Info, #state{estate=EState,emodule=Module}=State) ->
    handle_({Module, handle_info, [Info, EState]}, State);
handle_info(_Info, State) -> {noreply, State}.

terminate(Reason, #state{estate=EState,emodule=Module}=_State) ->
    Module:terminate(Reason, EState).

code_change(OldVsn, #state{estate=EState,emodule=Module}=State, Extra) ->
    {ok,EState1}=Module:code_change(OldVsn, EState, Extra),
    {ok, State#state{estate=EState1} }.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
handle_({Module, Function, Args} = MFA, State) ->
    try 
        case apply(Module, Function, Args) of
            noop ->
                {noreply, State};
            {ok, EState1} ->
                {noreply, State#state{estate=EState1} };
            {ok, EState1, Data} ->
                EState2=handle_data(EState1, Module, State#state.wm_ets, Data),
                {noreply, State#state{estate=EState2} };
            {ok, EState1, Data, Timeout} ->
                EState2=handle_data(EState1, Module, State#state.wm_ets, Data),
                {noreply, State#state{estate=EState2}, Timeout };
            BadRet ->
                {stop, {unexpected_return_value, MFA, BadRet}}
        end
    catch
        throw:normal ->
            {stop, normal, State};
        throw:shutdown ->
            {stop, shutdown, State};
        _:Reason ->
            {stop, {error,[{reason, Reason}, {stacktrace, erlang:get_stacktrace()}]}, State}
    end.

handle_data(EState0, _Module, _WMEts, []) ->
    EState0;
handle_data(EState0, Module, WMEts, WMOList) ->
    ModifyEntry=
        fun(Op, Entry) ->
                EntryExists=([[]]==ets:match(WMEts, Entry)),
                case {Op,EntryExists} of
                    {assert, false} -> ets:insert(WMEts, Entry);
                    {retire, true} -> ets:delete_object(WMEts, Entry);
                    _ -> false
                end
        end,
    % Create enivronment function for engine logic
    Env=fun(get_bnode, [BNodeId])->
                Module:beta_node(BNodeId); % Generated by parse transform
           (get_pnode, [PNodeId])->
                Module:p_node(PNodeId); % Generated by parse transform
           (get_anodes, []) ->
                Module:alpha_nodes(); % Generated by parse transform
           (fetch_left_index, [BNodeId, {}]) ->
                #bnode{bnode_ids=BNodeIds}=Module:beta_node(terminal),
                case lists:member(BNodeId, BNodeIds) of
                    true -> [ [ ] ];
                    false -> [ ]
                end;
           (fetch_pnode_tokens, [PNodeId, BNodeId]) ->
                try ets:lookup_element(WMEts, {p,PNodeId, BNodeId}, 2) catch _:badarg -> [] end;
           (fetch_left_index, [BNodeId, Key]) ->
                try ets:lookup_element(WMEts, {li,BNodeId,Key}, 2) catch _:badarg -> [] end;
           (fetch_right_index, [BNodeId, Key]) ->
                try ets:lookup_element(WMEts, {ri,BNodeId,Key}, 2) catch _:badarg -> [] end;
           (modify_pnode_tokens, [PNodeId, FromBNodeId, Op, Token]) ->
                Entry={ {p,PNodeId,FromBNodeId}, Token },
                ModifyEntry(Op, Entry);
           (modify_left_index, [BNodeId, Key, assert, Token]) ->
                Entry={ {li,BNodeId,Key}, Token},
                ets:insert(WMEts, Entry),ok;
           (modify_left_index, [BNodeId, Key, retire, Token]) ->
                Entry={ {li,BNodeId,Key}, Token},
                ets:delete_object(WMEts, Entry),ok;
           (modify_right_index, [BNodeId, Key, Op, WME]) ->
                Entry={ {ri,BNodeId,Key} , WME},
                ModifyEntry(Op, Entry);
           (activate_pnode, [_BNodeId, _Op, _PNode, {cart, _TokenFormat, _LLT}=Cart]) ->
                Cart;
           (activate_pnode, [_BNodeId, _Op, _PNode, Token]) ->
                {token, Token}
        end,
    FoldToken=
        fun(Token,WMO, {EStateAcc, WMOAcc})-> 
                % Pass activated token to behavior handle_pattern
                case Module:handle_pattern(Token, WMO, EStateAcc) of
                    noop -> {EStateAcc, WMOAcc};
                    {ok, EState1} -> {EState1, WMOAcc};
                    {ok, EState1, WMOData} -> {EState1, [WMOData, WMOAcc] }
                end
        end,
    {EStateZ, WMOListZ}=
        lists:foldl( % Fold each Working Memory Operation in WMOList
          fun(WMO, {EStateAcc0, WMOAcc0}) -> % In case of WMO type is assert
                  % Do assert with engine
                  case elips_engine:handle_wmo(WMO, Env) of
                      [] -> % No tokens activated/deactivated
                          {EStateAcc0, WMOAcc0};
                      ATokens -> % There are tokens activated
                          lists:foldl( % Fold each activated token into elips state
                            fun({token, Token}, Acc) ->
                                    FoldToken(Token, WMO, Acc);
                               ({cart, TokenFormat, LLT}, Acc) ->
                                    foldl_cart(
                                      fun(LT,Acc1)->
                                              Token=TokenFormat(LT),
                                              FoldToken(Token, WMO, Acc1)
                                      end,Acc,LLT)
                            end, {EStateAcc0, WMOAcc0}, ATokens)
                  end
          end, {EState0, []}, WMOList),
    handle_data(EStateZ, Module, WMEts, lists:flatten(WMOListZ) ).

foldl_cart(F,Acc0,LL)->
    foldl_cart(F,Acc0,LL,[]).
foldl_cart(F,Acc0,[],Token) ->
    F(lists:reverse(Token),Acc0);
foldl_cart(F,Acc0,[L|T],Token) ->
    lists:foldl(
      fun(E,Acc)->
              foldl_cart(F,Acc,T, [E|Token])
      end,Acc0,L).

