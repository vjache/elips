-module(victim_0).

-include("elips.hrl").

-record(person,{sex}).

-export([handle_pattern/3]).


%%=============================
%% Functions
%%=============================

%% handle_pattern(
%%   [{X, has_father, Y},
%%    {Y, has_father, Z}], _, State) when X>2 ->
%%     assert({X, has_grand_father, Z});
%% handle_pattern(
%%   [{X, has_father, Y},
%%    {Y, has_mother, Z}]=P, _, State)->
%%     assert({X, has_grand_mother, Z});
%% handle_pattern(
%%   [{X, has_father, Y},
%%    {Y, has_child, #person{sex=S}=Z}], _, State) when X=/=Z ->
%%     if S == male ->
%%            assert({X, has_brother, Z});
%%        S == female ->
%%            assert({X, has_sister, Z})
%%     end.

handle_pattern(
  [{X, has_father, Y},
   {W, has_father, Z},
   {Y, has_father, M},
   {W, has_child, Z1}], _, State) ->
    noop.

assert(_) ->
    ok.

%
% Generated
%

%% alpha_nodes() ->
%%     [fun({X, has_father, Y})-> [2,3,4] end,
%%      fun({X, has_mother, Y})-> [5] end,
%%      fun({X, has_child, Y})-> [6] end].
%% 
%% beta_node(2) ->
%%     #bnode{bnode_ids=[3],
%%            left_key_fun=fun([])->[] end,
%%            right_key_fun=fun({X, has_father, Y})->[] end,
%%            token_constraint=fun([{X, has_father, Y}])-> true end};
%% beta_node(3) ->
%%     #bnode{bnode_ids=[],
%%            left_key_fun=fun([{_X, has_father, _Y}])->[_Y] end,
%%            right_key_fun=fun({_Y, has_father, _Z})->[_Y] end,
%%            token_constraint=fun([{X, has_father, Y},{Y, has_father, Z}])-> true end}.


