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
-module(elips).

-behaviour(gen_server).

%%
%% Include files
%%

-include("log.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([start_link/4,
         start/4,
         notify/2,
         behaviour_info/1]).

%% gen_server callbacks
-export([init/1, 
         handle_call/3, 
         handle_cast/2, 
         handle_info/2, 
         terminate/2, 
         code_change/3]).

-record(state, {estate, emodule, wm_ets}).

%
% Types
%

-export_type([assert/1,retire/1, wmo/1, ok_reply/0]).

-type assert(T) :: {assert | '+', T}.
-type retire(T) :: {retire | '-', T}.
-type wmo(T) :: assert(T) | retire(T) .

-type ok_reply() :: {ok, State :: term()} | 
                    {ok, State :: term(), WMOs :: [wmo(T::term())]} | 
                    {ok, State :: term(), WMOs :: [wmo(T::term())], Timeout :: non_neg_integer() | infinity}.

%% ====================================================================
%% External functions
%% ====================================================================

-spec behaviour_info(atom()) -> 'undefined' | [{atom(), arity()}].

behaviour_info(callbacks) ->
    [{init,1},
     {handle_pattern,2},
     {handle_event,3}, 
     {handle_info,2},
     {terminate,2},
     {code_change,3},
     % Generated functions
     {alpha_nodes,0},
     {beta_node,1}];
behaviour_info(_Other) ->
    undefined.

start_link(ServerName,
           ElipsBehavior,
           Args,
           Opts) ->
    gen_server:start_link(ServerName, ?MODULE, {ElipsBehavior, Args}, Opts).

start(ServerName,
           ElipsBehavior,
           Args,
           Opts) ->
    gen_server:start(ServerName, ?MODULE, {ElipsBehavior, Args}, Opts).


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
    % Create enivronment function for engine logic
    Env=fun(get_bnode, [BNodeId])->
                Module:beta_node(BNodeId); % Generated by parse transform
           (get_anodes, []) ->
                Module:alpha_nodes(); % Generated by parse transform
           (fetch_left_index, [_BNodeId, {}]) ->
                [ [ ] ];
           (fetch_left_index, [BNodeId, Key]) ->
                try ets:lookup_element(WMEts, {li,BNodeId,Key}, 2) catch _:badarg -> [] end;
           (fetch_right_index, [BNodeId, Key]) ->
                try ets:lookup_element(WMEts, {ri,BNodeId,Key}, 2) catch _:badarg -> [] end;
           (store_left_index, [BNodeId, Key, Token]) ->
                ets:insert(WMEts, { {li,BNodeId,Key}, Token}),ok;
           (store_right_index, [BNodeId, Key, WME]) ->
                ets:insert(WMEts, { {ri,BNodeId,Key}, WME}),ok;
           (activate_pnode, [_PNode, Token]) ->
                Token
        end,
    {EStateZ, WMOListZ}=
        lists:foldl( % Fold each Working Memory Operation in WMOList
          fun({assert, WME}, {EStateAcc0, WMOAcc0}) -> % In case of WMO type is assert
                  % Do assert with engine
                  case elips_engine:assert(WME, Env) of
                      [] -> % No tokens activated
                          {EStateAcc0, WMOAcc0};
                      ATokens -> % There are tokens activated
                          lists:foldl( % Fold each activated token into elips state
                            fun({atoken, _BNodeId, Token}, {EStateAcc, WMOAcc}) ->
                                    % Pass activated token to behavior handle_pattern
                                    case Module:handle_pattern(Token,EStateAcc) of
                                        noop -> {EStateAcc, WMOAcc};
                                        {ok, EState1} -> {EState1, WMOAcc};
                                        {ok, EState1, WMOData} -> {EState1, [WMOData, WMOAcc] }
                                    end
                            end, {EStateAcc0, WMOAcc0}, ATokens)
                  end;
             ({retire, _WME}, _) ->
                  throw(not_supported_yet)
          end, {EState0, []}, WMOList),
    handle_data(EStateZ, Module, WMEts, lists:flatten(WMOListZ) ).