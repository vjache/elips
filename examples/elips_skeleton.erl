-module(elips_skeleton).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("elips/include/elips.hrl").

%%--------------------------------------------------------------------
%% Exported Functions
%%--------------------------------------------------------------------

% Agent API functions export
-export([start_link/0,start/0,notify/1]).
% ELIPS behavior functions export
-export([init/1, handle_pattern/3, handle_event/3, handle_info/2, terminate/2, code_change/3]).

% Agent state
-record(state,{}).

%% ====================================================================
%% API Functions
%% ====================================================================
start_link() ->
    elips:start_link({local, ?MODULE}, ?MODULE, _InitArgs=[], []).

start() ->
    elips:start({local, ?MODULE}, ?MODULE, _InitArgs=[], []).

notify(Event) ->
    elips:notify(?MODULE, Event).

%% ====================================================================
%% ELIPS Behavior Callback Functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initialize ELIPS state & insert some data to Working 
%%              Memory (WM).
%% Returns: {ok, State}          |
%%          {ok, State, [#assert{}|#retire{}]} |
%%          {ok, State, [#assert{}|#retire{}], Timeout} | ignore
%% --------------------------------------------------------------------
-spec init( InitArgs :: term() ) -> elips:ok_reply() | ignore.
init(_InitArgs) ->
    State=#state{},
    {ok, State, []}.

%% --------------------------------------------------------------------
%% Function: handle_pattern/3
%% Description: Handling pattern matched against a facts at Working 
%%              Memory of an agent.
%% updates.
%% Returns: {ok, State}          |
%%          {ok, State, [#assert{}|#retire{}]} |
%%          {ok, State, [#assert{}|#retire{}], Timeout} | noop
%% --------------------------------------------------------------------
-spec handle_pattern(Token :: [term()], WMO :: elips:wmo(), State :: term() ) ->  elips:ok_reply() | noop.
handle_pattern([{_A, predicate,_B}=Fact]=_Pattern, 
               #assert{}, _State) ->
    Fact1={_B, predicate_1, _A},
    {ok,_State,[#retire{fact=Fact},#assert{fact=Fact1}]}.

%% --------------------------------------------------------------------
%% Function: handle_event/3
%% Description: Handling elips:notify/2 messages & do some Working Memory 
%%              updates.
%% Returns: {ok, State}          |
%%          {ok, State, [#assert{}|#retire{}]} |
%%          {ok, State, [#assert{}|#retire{}], Timeout} | noop
%% --------------------------------------------------------------------
-spec handle_event(Event :: term(), FromPid :: pid(), State :: term() ) ->  elips:ok_reply() | noop.
handle_event({'-',_Event}, _FromPid, _State) ->
    {ok, _State, [#retire{fact=_Event}]};
handle_event({'-+',OldEvent, NewEvent}, _FromPid, _State) ->
    {ok, _State, [#retire{fact=OldEvent}, 
                  #assert{fact=NewEvent}]};
handle_event(_Event, _FromPid, _State) ->
    {ok, _State, [#assert{fact=_Event}]}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non elips:notify/2 messages & do some 
%%              Working Memory updates. This function handles system messages like 
%%              'EXIT' of some linked process if trapexit true were set in init/1, 
%%              or else node up and down if node monitoring started.
%% Returns: {ok, State}          |
%%          {ok, State, [#assert{}|#retire{}]} |
%%          {ok, State, [#assert{}|#retire{}], Timeout} | noop
%% --------------------------------------------------------------------
-spec handle_info(Msg :: term(), State :: term() ) ->  elips:ok_reply() | noop.
handle_info(_Msg, _State) ->
    noop.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the agent
%% Returns: any (ignored by ELIPS host process)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

