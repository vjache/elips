-module(janitor).

-include_lib("elips/include/elips.hrl").

-export([handle_pattern/3,
         handle_event/3,
         init/1]).


-define(DISTANCE(V1,V2), 
        ((V1#coord.x-V1#coord.y)*(V1#coord.x-V1#coord.y) + 
             (V2#coord.x-V2#coord.y)*(V2#coord.x-V2#coord.y)) ).

%
% Records
%
-record(coord, {x,y,z}).
-record(state,{}).

%
% Functions
%

init(_Args) ->
    State=#state{},
    {ok, 
     State, [#assert{fact= {me, status, stay} },
             #assert{fact= {me, located_at, #coord{x=0,y=0,z=0}} }]}. 

% Accepts a raw events and converts it to Working Memory Operations.
handle_event(_Event, _FromPid, _State) ->
    {ok,_State,[#assert{fact=_Event}]}.

% Here a pattern handling function for automatic janitor behaviour.
% For simplicity we suppose a discrete nature of coordinates 

%
% Handle pattern occured at Working Memory.
%

% Garbage detected -> move to it
handle_pattern(
  [{G, is_a, garbage},
   {G, located_at, C },
   {me, located_at, C1 },
   {me, status, stay }=OldStatus], 
  #assert{}, _State) when C=/=C1 ->
    % Start move to position C
    NewStatus={me, status, {moving_to, C} },
    start_move(C,NewStatus),
    % Update Working Memory
    [#retire{fact=OldStatus},
     #assert{fact=NewStatus}];
% Garbage reached -> collect it
handle_pattern(
  [{G, is_a, garbage},
   {G, located_at, C},
   {me, located_at, C }], 
  #assert{}, _State) ->
    % Start collect garbage
    NewStatus={me, status, collecting_garbage},
    start_collect_garbage(G,NewStatus),
    % Update Working Memory
    #assert{fact=NewStatus}.

%
% Spawns an Erlang process which simulates a process (in common sense) 
% of moving. When it finish it must send a status update to ELIPS agent process.
%
start_move(_DestCoord,MotionStatus) ->
    AgentPid=self(),
    spawn(fun()->
                  do_move,
                  elips:notify(AgentPid, #retire{fact=MotionStatus}),
                  % TODO: It will be quite useful to have an automatic ability 
                  % to link some fact(s) to some process and when th process 
                  % finishes automatically send #retire!
                  NewMotionStatus={me, status, stay},
                  elips:notify(AgentPid, #assert{fact=NewMotionStatus})
          end).

start_collect_garbage(_Garbage, GCStatus) ->
    AgentPid=self(),
    spawn(fun()->
                  do_gc,
                  elips:notify(AgentPid, #retire{fact=GCStatus})
          end).
