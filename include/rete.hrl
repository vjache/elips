%%=======================================================================
%% Rete data structures
%%=======================================================================

% An alpha node definition
-record(anode, 
        {id :: integer(),
         wme_constraint :: fun( (term()) -> boolean() ), 
         bnode_ids :: [integer()]}).
% A beta node definition
-record(bnode, 
        {id :: integer(),
         token_constraint :: fun( (term()) -> boolean() ),
         left_key_fun :: fun( (term()) -> term() ),
         right_key_fun :: fun( (term()) -> term() ),
         pnodes=[]:: [integer()],
         bnode_ids :: [integer()] }).
% An output node definition
-record(pnode,
        {id :: integer(), 
         token_format :: fun( ([ [T] ]) -> [T] ),
         bnode_ids :: [integer()]}).
% A rete spec
-record(rete, 
        {anodes=[]:: [#anode{}],
         bnodes=[]:: [#bnode{}],
         pnodes=[]:: [#pnode{}]}).