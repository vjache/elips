-module(example_0).

-include_lib("elips/include/elips.hrl").

-export([handle_pattern/3]).

%
% Records
%

-record(person,{fname,lname,sex}).



%
% Functions
%

handle_pattern(
  [{X, has_father, Y},
   {Y, has_father, Z}], _, _State)->
    assert({X, has_grand_father, Z});
handle_pattern(
  [{X, has_father, Y},
   {Y, has_mother, Z}], _, _State)->
    assert({X, has_grand_mother, Z});
handle_pattern(
  [{X, has_father, Y},
   {Y, has_child, #person{sex=S}=Z}], _, _State) when X=/=Z ->
    if S == male ->
           assert({X, has_brother, Z});
       S == female ->
           assert({X, has_sister, Z})
    end.

assert(_)-> ok.

