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
%%% Created : Nov 27, 2011
%%%-------------------------------------------------------------------------------
-module(elips_builder).

%%
%% Include files
%%
-include("private.hrl").
-include("log.hrl").

%%
%% Exported Functions
%%
-export([make/1]).

-export([traverse_test/0, 
         clause_to_rete/1, 
         clause_to_rete_test/0, 
         merge_into_tree_test/0, 
         merge_two_rete_test/0,
         equimatch_test/0,
         make_test/0]).

-define(FUN(Line,Clauses),{'fun',Line,{clauses, Clauses}}).

%%
%% API Functions
%%

make(#function{name=handle_pattern,arity=3,clauses=ClauseList}) ->
    ClauseList1=filter_equimatchable_clauses(ClauseList),
    ClauseList2=filter_incoventional_clauses(ClauseList1),
    ReteTotal=merge_deep(ClauseList2),
    {make_alpha_layer(ReteTotal), make_beta_layer(ReteTotal), make_p_layer(ReteTotal), ReteTotal}.

merge_deep(ClauseList) ->
    ReteDeepList=[clause_to_rete(Clause) || Clause <- ClauseList],
    ReteList=lists:flatten(ReteDeepList),
    lists:foldl(
      fun(Rete, ReteAcc) ->
              merge(Rete, ReteAcc)
      end, #rete{}, ReteList).

filter_incoventional_clauses(ClauseList) ->
    IfConventional=
        fun(#clause{args=[#var{}|_]}) ->
                false;
           (_) ->
                true
        end,
    [Clause || Clause <- ClauseList, IfConventional(Clause)].
filter_equimatchable_clauses(ClauseList) ->
    lists:reverse(
      lists:foldl(
        fun(#clause{args=Args}=C, UniqueR)->
                case lists:all(fun(A)-> not equimatch(A, Args) end, UniqueR) of
                    true -> [C | UniqueR];
                    false -> UniqueR
                end
        end, [], ClauseList)).

-spec clause_to_rete(ClauseForm::#clause{}) -> #rete{}.
clause_to_rete(#clause{args=[#match{left=L,right=R}, _LastWMO, _StateForm]}=C) ->
    case {L,R} of
        {#cons{}, #var{}} -> Cons=L;
        {#var{}, #cons{}} -> Cons=R
    end,
    clause_to_rete(C#clause{args=[Cons, _LastWMO, _StateForm]});
clause_to_rete(#clause{args=[#cons{}=ConsForm, _LastWMO, _StateForm],guards=Guards}) ->
    % Create a build fun at the beginning to avoid unwanted occasional clash with local variables
    BuildFun=fun(AlphaForm, {#rete{anodes=ANodes,
                                   bnodes=[#bnode{bnode_ids=BIds}=BNode| Tail ]}, 
                             AlphaFormsAcc}) when is_list(BIds) ->
                     % Allocate ids for anode and bnode in advance
                     ANodeIdNew=next(anode_ids),
                     BNodeIdNew=next(bnode_ids),
                     % Create alpha node
                     ANodeNew=#anode{id=ANodeIdNew,
                                     bnode_ids=[BNodeIdNew],% New alpha node must refer to a new beta node created bellow
                                     wme_constraint=make_wme_constraint(
                                                      AlphaForm,Guards,ANodeIdNew) % Fun form (for alpha layer)
                                    },
                     % Extract common variables of a token at left and WME at right
                     CommonVars=lists:sort(
                                  sets:to_list(
                                    sets:intersection(
                                      sets:from_list(get_variables(AlphaForm)),
                                      sets:from_list([V || F <- AlphaFormsAcc, V <- get_variables(F)])))),
                     % Ensure common variables not an empty set
                     case CommonVars of
                         [] when BNode#bnode.id=/=terminal -> 
                             throw({no_common_vars, CommonVars, AlphaForm, AlphaFormsAcc});
                         _ -> ok
                     end,
                     AlphaFormsAcc1=[AlphaForm | AlphaFormsAcc],
                     %Create beta node
                     BNodeNew=#bnode{id=BNodeIdNew,
                                     bnode_ids=[],
                                     left_key_fun=make_left_key_fun(lists:reverse(AlphaFormsAcc),CommonVars), % Fun form
                                     right_key_fun=make_right_key_fun(AlphaForm,CommonVars), % Fun form
                                     token_constraint=make_token_constraint(
                                                        lists:reverse(AlphaFormsAcc1),
                                                        Guards,BNodeIdNew) % Fun form (for beta layer)
                                    },
                     % Accumulate all stuff ...
                     {#rete{anodes=[ANodeNew|ANodes],
                            bnodes=[BNodeNew|[BNode#bnode{bnode_ids=[BNodeIdNew|BIds]}|Tail]]},
                      AlphaFormsAcc1}
             end,
    AlphaForms=cons_to_list(ConsForm),
    case separate_connected_by_var_components(AlphaForms) of
%%         % There is only one component (all forms connected by vars)
%%         [AlphaFormsOredred] ->
%%             {#rete{anodes=ANodeList,
%%                    bnodes=[H|T]}, _ } = 
%%                 lists:foldl(BuildFun, 
%%                             {#rete{anodes=[], bnodes=[make_terminal_bnode()]},[]}, 
%%                             AlphaFormsOredred),
%%             #rete{anodes=lists:reverse(ANodeList),
%%                   bnodes=lists:reverse([H#bnode{pnodes=true}|T])};
        % Thre are multiple components
        Components when length(Components) > 0 ->
            % Build a list of rete's one per component
            RList=
                [begin
                     {R, _ } = 
                         lists:foldl(BuildFun, 
                                     {#rete{bnodes=[make_terminal_bnode()]},[]}, 
                                     Component), R
                 end || Component <- Components ],
            % Create pnode with incoming bnode ids
            HeadBNodeIds=[ BNodeId || #rete{bnodes=[#bnode{id=BNodeId}|_]} <- RList],
            PNodeIdNew=next(pnode_ids),
            PNode=#pnode{id=PNodeIdNew,
                   token_format=make_token_format(Components, ConsForm),
                   bnode_ids=HeadBNodeIds},
            % Reverse lists and insert pnode id
            [#rete{anodes=lists:reverse(ANodeList),
                   bnodes=lists:reverse([H#bnode{pnodes=[PNodeIdNew]}|T]),
                   pnodes=[PNode]} || #rete{anodes=ANodeList, bnodes=[H|T]} <- RList]
    end;
clause_to_rete(#clause{args=[#var{}|_]}) ->
    #rete{}.

separate_connected_by_var_components(Forms) ->
    separate_connected_by_var_components(Forms, []).

separate_connected_by_var_components([], Components) ->
    Components;
separate_connected_by_var_components([StartForm|_]=Forms, Components) ->
    GetNeighFun=
        fun(F)->
                Vars=get_variables(F),
                [AF || AF <- Forms, 
                       AF=/=F, 
                       lists:any(fun(V)-> lists:member(V, Vars) end, get_variables(AF))]
        end,
    % Order alpha forms in such a way that each form in list have 
    % a common variables with preceding alpha forms. Such an order 
    % have better performance versus arbitrary one.
    Component=traverse(StartForm, GetNeighFun, breadth),
    separate_connected_by_var_components(
      [F || F <- Forms, not lists:member(F, Component)], 
      [Component | Components]).
    

%
% Merge left to right
%

-spec merge(#rete{}, #rete{}) -> #rete{} .

merge(#rete{bnodes=[],anodes=[]}=_Path,
      #rete{bnodes=[],anodes=[]}=_Tree) ->
    _Tree;
merge(#rete{}=Path,
      #rete{bnodes=[],anodes=[]}=Tree) ->
    merge(Path,Tree#rete{bnodes=[make_terminal_bnode()]});
merge(#rete{bnodes=BNodesLeft,anodes=ANodesLeft,pnodes=PNodesLeft},
      #rete{bnodes=[Root|_]=BNodesRight,anodes=ANodesRight,pnodes=PNodesRight}=Tree) ->
    %% Merge beta layers
    GetChildrenFun=
        fun(#bnode{bnode_ids=ChildrenIds})->
                [BNode || BNode <- BNodesRight, lists:member(BNode#bnode.id, ChildrenIds)]
        end,
    EqualFun=
        fun(#bnode{token_constraint=?FUN(_,[#clause{args=ArgsCons1},_])},
            #bnode{token_constraint=?FUN(_,[#clause{args=ArgsCons2},_])})->
                equimatch(ArgsCons1, ArgsCons2);
           (#bnode{id=terminal},
            #bnode{id=terminal})->
                true
        end,
    case merge_into_tree(BNodesLeft, Root, GetChildrenFun, EqualFun) of
        % In this case we just mark that tree node have an output (pnodes)
        {[#bnode{id=Id,pnodes=PIds}=H|_], []} ->
            #bnode{id=IdL,pnodes=PIdsL}=lists:last(BNodesLeft),
            % Update pnodes bnode_ids: replace an old bnode id (IdL) with a new one (Id)
            PNodesLeft1=[ P#pnode{bnode_ids=lists_replace(IdL, Id, BIds)} || #pnode{bnode_ids=BIds}=P <-PNodesLeft],
            BNodesRight1=lists:keyreplace(Id, #bnode.id, BNodesRight, H#bnode{pnodes=umerge_lists(PIds,PIdsL)}),
            %% Filter out alpha nodes that point to bnodes trunceted after merge of beta layer
            ANodesRight1=ANodesRight;
        % In this case we tailoring a new branch by fixing ids of a tree node
        {[#bnode{id=Id1,bnode_ids=Ids}=H|_], [#bnode{id=Id}|_]=LeftRest} ->
            PNodesLeft1=PNodesLeft,
            BNodesRight1=lists:keyreplace(Id1, #bnode.id, BNodesRight, H#bnode{bnode_ids=[Id|Ids]})++LeftRest,
            %% Filter out alpha nodes that point to bnodes trunceted after merge of beta layer
            % Dou to alpha node may have multiple pointers to bnodes we firstly remove that 
            % pointers(Ids) form anone.bnode_ids list
            ANodesLeft1=[ ANL#anode{bnode_ids=[BId||BId <- BIds, lists:keymember(BId, #bnode.id, LeftRest)]} || 
                           #anode{bnode_ids=BIds}=ANL <- ANodesLeft],
            % Then filter out an alpha nodes with empty lists of bnode pointers
            ANodesRight1= ANodesRight ++ [ N || N <- ANodesLeft1, length(N#anode.bnode_ids) > 0 ]
    end,
    %% Merge alpha layers
    % 2. Consider each pair of alpha nodes and construct equivalence grpah 
    % where each edge of a graph connects two equimatching alpha nodes
    G=digraph:new(),
    [digraph:add_vertex(G, N) || N <- ANodesRight1],
    [begin
         E={N1,N2},
         E=digraph:add_edge(G, E, N1, N2, "equimatchable")
     end|| 
        #anode{wme_constraint=?FUN(_,[#clause{args=ArgsL},_])}=N1 <- ANodesRight1, 
        #anode{wme_constraint=?FUN(_,[#clause{args=ArgsR},_])}=N2 <- ANodesRight1,
        N1 =/= N2,
        equimatch(ArgsL, ArgsR)],
    % 3. Merge equivalent groups of components
    EquimatchComponents=digraph_utils:components(G),
%%     ?ECHO({'COMPONENTS',[[{I,Is,A}||#anode{id=I,bnode_ids=Is,wme_constraint=#clause{args=A}}<-Cmp]||Cmp<-EquimatchComponents]}),
    ANodesRight2=
        [lists:foldl(
           fun(#anode{bnode_ids=Bids},#anode{bnode_ids=AccBids}=Acc)->
                   Acc#anode{bnode_ids=umerge_lists(Bids, AccBids)}
           end,HAN,Tail) || [HAN|Tail]<-EquimatchComponents],
    % 4. Return resulting rete
    Tree#rete{bnodes=BNodesRight1,anodes=ANodesRight2,pnodes=umerge_lists(PNodesLeft1,PNodesRight)}.

lists_replace(OldElem, NewElem, List) ->
    [if OldElem == Elem ->
            NewElem;
        true -> Elem
     end || Elem <- List].

umerge_lists(L1,L2) ->
    lists:umerge(lists:sort(L1), lists:sort(L2)).

make_terminal_bnode() ->
    #bnode{id=terminal,bnode_ids=[]}.

make_wme_constraint(AlphaForm,Guards,ANodeIdNew) when 
  is_integer(ANodeIdNew),is_list(Guards) ->
    Clause=
        #clause{args=[AlphaForm],
            guards=[],
            body=[#atom{name=true}]},
    Clause_=
        #clause{args=[#var{name='_'}],
            guards=[],
            body=[#atom{name=false}]},
    underscore_vars(?FUN(0,[Clause,Clause_])).

make_token_constraint(TokenForms,Guards,BNodeIdNew) when 
  is_integer(BNodeIdNew),is_list(Guards),is_list(TokenForms) ->
    Clause=
        #clause{args=[list_to_cons(TokenForms)],
            guards=[],
            body=[#atom{name=true}]},
    Clause_=
        #clause{args=[#var{name='_'}],
            guards=[],
            body=[#atom{name=false}]},
    underscore_vars(?FUN(0,[Clause,Clause_])).

make_token_format(Components, #cons{}=ConsForm) when is_list(Components) ->
    Clause=
        #clause{args=[list_to_cons([ list_to_cons(Component) || Component <- Components])],
            guards=[],
            body=[ConsForm]},
    underscore_vars(?FUN(0,[Clause])).

make_left_key_fun(TokenForms,CommonVars) ->
    Clause=
        #clause{args=[list_to_cons(TokenForms)],
                guards=[],
                body=[#tuple{elements=[#var{name=V} || V <- CommonVars]}]},
    underscore_vars(?FUN(0,[Clause])).

make_right_key_fun(AlphaForm,CommonVars) ->
    Clause=
        #clause{args=[AlphaForm],
                guards=[],
                body=[#tuple{elements=[#var{name=V} || V <- CommonVars]}]},
    underscore_vars(?FUN(0,[Clause])).

-spec list_to_cons([Form :: term()]) -> #cons{} | #nil{}.
list_to_cons([]=_Forms) ->
    #nil{};
list_to_cons([Form|Tail]=_Forms) ->
    #cons{term=Form,next=list_to_cons(Tail)}.

-spec cons_to_list(Cons :: #cons{}) -> [term()].
cons_to_list(Cons) ->
    cons_to_list(Cons,[]).

cons_to_list(#nil{},L) ->
    lists:reverse(L);
cons_to_list(#cons{term=Form,next=Other},L) when not is_record(Other,cons), not is_record(Other,nil) ->
    {lists:reverse([Form|L]), Other};
cons_to_list(#cons{term=Form,next=Next},L) ->
    cons_to_list(Next, [Form|L]).

%%=========================================
%% Local Functions
%%=========================================

%
% Extracts variables from a form.
%
get_variables(Form) ->
    lists:umerge([lists:flatten(get_variables_(Form))]).
get_variables_(#nil{}) ->
    [];
get_variables_(#atom{}) ->
    [];
get_variables_(#cons{term=Term,next=Elem}) ->
    [get_variables_(Term),get_variables_(Elem)];
get_variables_(#tuple{elements=Elems}) ->
    [get_variables_(Elem) || Elem <- Elems];
get_variables_(#record{fields=Fiels}) ->
    [ get_variables_(Value) || #record_field{value=Value} <- Fiels];
get_variables_(#match{left=F1,right=F2}) ->
    get_variables_(F1) ++ get_variables_(F2); 
get_variables_(#var{name=Name}) ->
    [Name].

map_form(Fun, #cons{term=Term,next=Elem}=T) ->
    Fun(T#cons{term=map_form(Fun, Term),next=map_form(Fun, Elem)});
map_form(Fun, #tuple{elements=Elems}=T) ->
    Fun(T#tuple{elements=[map_form(Fun, Elem) || Elem <- Elems]});
map_form(Fun, #record{fields=Fields}=T) ->
    Fun(T#record{fields=[ R#record_field{value=map_form(Fun,Value)} || R=#record_field{value=Value} <- Fields]});
map_form(Fun, #match{left=F1,right=F2}=T) ->
    Fun(T#match{left=map_form(Fun, F1),right=map_form(Fun, F2)});
map_form(Fun, #clause{args=Args,guards=Guards,body=Body}=T) ->
    Fun(T#clause{args=[map_form(Fun,A)||A<-Args],guards=[map_form(Fun,G)||G<-Guards],body=[map_form(Fun,B)||B<-Body]});
map_form(Fun, #function{clauses=Clauses}=T) ->
    Fun(T#function{clauses=[map_form(Fun,C)||C<-Clauses]});
map_form(Fun, ?FUN(Line,Clauses)) ->
    Fun(?FUN(Line,[map_form(Fun,C)||C<-Clauses]));
map_form(Fun, #var{}=V) ->
    Fun(V);
map_form(Fun, #atom{}=T) ->
    Fun(T);
map_form(Fun, #nil{}=T) ->
    Fun(T);
map_form(Fun, T) ->
    Fun(T).

underscore_vars(Form) ->
    prefix_vars("_", Form).

prefix_vars(Prefix, Form) ->
    map_form(fun(#var{name='_'}=V)->
                     V;
                (#var{name=Name}=V)->
                     V#var{name=list_to_atom(Prefix ++ atom_to_list(Name))};
                (_T) -> _T
             end,Form).

%% blank_vars(Form) ->
%%     map_form(fun(#var{name=_}=V)->
%%                      V#var{name='_'};
%%                 (_T) -> _T
%%              end,Form).

%
% Helper function to traverse a graph nodes.
%
-spec traverse(StartVertex::T, 
               GetNeighFun :: fun((T) -> [T]), 
               Strategy:: breadth | depth | {custom, MergeFun::fun(([T],[T])->[T])}) -> [T].
traverse(StartVertex, GetNeighFun, Strategy) when is_function(GetNeighFun, 1) ->
    traverse(GetNeighFun, [StartVertex], [], Strategy).
    
traverse(_GetNeighFun, []=_Stack, Visited, _Strategy) ->
    lists:reverse(Visited);
traverse(GetNeighFun, [V|Tail]=_Stack, Visited, Strategy) ->
    case [ N || N <- GetNeighFun(V), 
                not lists:member(N, Visited), 
                not lists:member(N, Tail)] of
        [] ->
            traverse(GetNeighFun, Tail, [V|Visited], Strategy);
        Neigh ->
            Stack1=case Strategy of 
                       breadth -> lists:append(Tail,Neigh); 
                       depth -> lists:append(Neigh,Tail); 
                       {custom, MergeFun} -> MergeFun(Neigh,Tail) 
                   end, 
            traverse(GetNeighFun, Stack1, [V|Visited], Strategy)
    end.

%
% Helper function to prepare path merge into tree.
%
-spec merge_into_tree(Path :: [T],
                RootNode :: [T1],
                GetChildrenFun :: fun((T1)->[T1]),
                EqualFun :: fun( (T,T1) -> boolean() ) ) -> {ForkNodePathRev::[T1], PathRest::[T]}.
merge_into_tree(Path, Root, GetChildrenFun, EqualFun) ->
    follow_path(Path, [Root], GetChildrenFun, EqualFun, []).

follow_path([]=_Path, _Roots, _GetChildrenFun, _EqualFun, Acc) ->
    {Acc, []};
follow_path([H|T]=Path, Roots, GetChildrenFun, EqualFun, Acc) ->
    case [ C || C <- Roots, EqualFun(H,C)] of
        [Next] -> follow_path(T, GetChildrenFun(Next), GetChildrenFun, EqualFun, [Next|Acc]);
        [] -> {Acc, Path}
    end.

%
% Equimatch comparator
%

equimatch(L1,L2) when is_list(L1),is_list(L2) ->
    lists:all(
      fun({F1,F2})->
              equimatch(F1,F2)
      end, lists:zip(L1, L2));
equimatch(#tuple{elements=E1},#tuple{elements=E2}) when length(E1)==length(E2) ->
    equimatch(E1, E2);
equimatch(#record{name=Name,fields=Fields1},
          #record{name=Name,fields=Fields2}) when length(Fields1)==length(Fields2) ->
    equimatch(Fields1, Fields2);
equimatch(#record_field{name=#atom{name=RF},value=V1},
          #record_field{name=#atom{name=RF},value=V2}) ->
    equimatch(V1,V2);
equimatch(#atom{name=Name},#atom{name=Name}) ->
    true;
equimatch(#cons{}=Cons1,#cons{}=Cons2) ->
    case {cons_to_list(Cons1),cons_to_list(Cons2)} of
        {{L1,T1},{L2,T2}} ->
            equimatch(L1,L2) andalso equimatch(T1,T2); 
        {L1,L2} -> 
            equimatch(L1,L2)
    end;
equimatch(#match{left=L,right=R},Form) ->
    equimatch(L, Form) andalso equimatch(R, Form);
equimatch(Form,#match{left=L,right=R}) ->
    equimatch(L, Form) andalso equimatch(R, Form);
equimatch(#nil{},#nil{}) ->
    true;
equimatch(#var{},#var{}) ->
    true;
equimatch(_,_) ->
    false.

%
% Sequence
%
next(SequenceName) ->
    case get(SequenceName) of
        undefined ->
            put(SequenceName,2),1;
        N -> put(SequenceName,N+1)
    end.

%
% Beta node to form
%
bnode_to_form(#bnode{id=terminal,
                     bnode_ids=BIds,
                     pnodes=_PNodes,
                     left_key_fun=LKF,
                     right_key_fun=RKF,
                     token_constraint=TC}) ->
    #record{name=bnode,
            fields=[#record_field{name=atomf(id),value=#atom{name=terminal}},
                    #record_field{name=atomf(bnode_ids),value=list_to_cons([#integer{value=I}||I<-BIds])},
                    #record_field{name=atomf(pnodes),value=#nil{}},
                    #record_field{name=atomf(left_key_fun),value=#atom{name=LKF}},
                    #record_field{name=atomf(right_key_fun),value=#atom{name=RKF}},
                    #record_field{name=atomf(token_constraint),value=#atom{name=TC}}]};    
bnode_to_form(#bnode{id=Id,
                     bnode_ids=BIds,
                     pnodes=PNodes,
                     left_key_fun=LKF,
                     right_key_fun=RKF,
                     token_constraint=TC}) ->
    #record{name=bnode,
            fields=[#record_field{name=atomf(id),value=#integer{value=Id}},
                    #record_field{name=atomf(bnode_ids),value=list_to_cons([#integer{value=I}||I<-BIds])},
                    #record_field{name=atomf(pnodes),value=list_to_cons([#integer{value=I}||I<-PNodes])},
                    #record_field{name=atomf(left_key_fun),value=LKF},
                    #record_field{name=atomf(right_key_fun),value=RKF},
                    #record_field{name=atomf(token_constraint),value=TC}]}.

atomf(A) when is_atom(A) ->
    #atom{name=A}.

anode_to_form(#anode{id=Id,bnode_ids=BIds,wme_constraint=WMEC}) ->
    #record{name=anode,
            fields=[#record_field{name=atomf(id),value=#integer{value=Id}},
                    #record_field{name=atomf(bnode_ids),value=list_to_cons([#integer{value=I}||I<-BIds])},
                    #record_field{name=atomf(wme_constraint),value=WMEC}]}.

pnode_to_form(#pnode{id=Id,bnode_ids=BIds,token_format=TF}) ->
    #record{name=pnode,
            fields=[#record_field{name=atomf(id),value=#integer{value=Id}},
                    #record_field{name=atomf(bnode_ids),value=list_to_cons([ #integer{value=I} || I <- BIds ])},
                    #record_field{name=atomf(token_format),value=TF}]}.
    

make_beta_layer(#rete{bnodes=BNodes}) ->
    #function{name=beta_node,
              arity=1,
              clauses=[#clause{args=[if is_integer(Id) -> 
                                           #integer{value=Id}; 
                                       Id==terminal -> #atom{name=Id} end],
                               guards=[],
                               body=[bnode_to_form(BNode)]} || #bnode{id=Id}=BNode <- BNodes]}.

make_alpha_layer(#rete{anodes=ANodes}) ->
    #function{name=alpha_nodes,
              arity=0,
              clauses=[#clause{args=[],
                               guards=[],
                               body=[list_to_cons([anode_to_form(ANode) || ANode <- ANodes])]}]}.

make_p_layer(#rete{pnodes=PNodes}) ->
     #function{name=p_node,
              arity=1,
              clauses=[#clause{args=[#integer{value=Id}],
                               guards=[],
                               body=[pnode_to_form(PNode)]} || #pnode{id=Id}=PNode <- PNodes]}.

%%=========================================
%% Test Functions
%%=========================================

traverse_test() ->
    GetNeighFun=
        fun(a)-> [b,c,g];
           (b)-> [a,c];
           (c)->[a,b,d];
           (d)->[c,e,f];
           (f)-> [d];
           (e)->[d]; 
           (g)->[a,k];
           (k)->[g,l];
           (l)->[k,m];
           (m)->[l] end,
    {traverse(a,GetNeighFun,breadth),traverse(a,GetNeighFun,depth)}.

clause_to_rete_test() ->
    try
    {ok,B}=file:read_file("src/examples/handle_pattern"),
    S=erlang:binary_to_list(B),
    ?ECHO("HERE"),
    {ok,T,_}=erl_scan:string(S),
    ?ECHO("HERE"),
    {ok,#function{clauses=[C1|_]}}=erl_parse:parse_form(T),
    ?ECHO("HERE"),
    clause_to_rete(C1)
    catch
        _:Reason ->
            io:format("**ERROR**~n\treason: ~p~n\tstack_trace: ~p~n~n",[Reason,erlang:get_stacktrace()])
    end.

merge_into_tree_test()->
    RootNode={tn,a}, % tn - stands for 'tree node'
    GetChildrenFun=
        fun({tn,a})-> [{tn,b1},{tn,b},{tn,b2}];
           ({tn,b})-> [{tn,c1},{tn,c},{tn,c2},{tn,c3}];
           ({tn,a1})-> [{tn,b11}]; 
           (_) -> [] end,
    EqualFun=fun(X,{tn,Y})-> X==Y end,
    {[{tn,c},{tn,b},{tn,a}],[d,e]}=merge_into_tree([a,b,c,d,e], RootNode, GetChildrenFun, EqualFun),
    {[{tn,c},{tn,b},{tn,a}],[]}=merge_into_tree([a,b,c], RootNode, GetChildrenFun, EqualFun),
    ok.

equimatch_test() ->
    F1=#tuple{elements=[#var{name='X'},#atom{name=has_father},#var{name='Y'}]},
    F2=#tuple{elements=[#var{name='X'},#atom{name=has_mother},#var{name='Y'}]},
    F3=#tuple{elements=[#var{name='A'},#atom{name=has_father},#var{name='B'}]},
    false=equimatch(F1,F2),
    true=equimatch(F1,F3).

merge_two_rete_test() ->
    try
        ?ECHO("start"),
        erase(),
        #function{clauses=[C1,C2|_]}=load_example_form(),
        ?ECHO("form loded"),
        Rete1=clause_to_rete(C1),
        Rete2=clause_to_rete(C2),
        merge(Rete1, Rete2)
    catch
        _:Reason ->
            io:format("**ERROR**~n\treason: ~p~n\tstack_trace: ~p~n~n",[Reason,erlang:get_stacktrace()])
    end.

load_example_form()->
    {ok,B}=file:read_file("src/examples/handle_pattern"),
    S=erlang:binary_to_list(B),
    {ok,T,_}=erl_scan:string(S),
    {ok,F=#function{name=handle_pattern,arity=3}}=erl_parse:parse_form(T),
    F.

make_test() ->
    try
        ?ECHO("start"),
        erase(),
        FunctionForm=load_example_form(),
        ?ECHO("form loded"),
        {ALayer,BLayer,_}=make(FunctionForm),
%%         ALayerAST=erl_syntax:revert_forms([ALayer]),
        ALayerStringRep=erl_prettypr:format(ALayer),
        io:format("== Alpha layer source ==~n~n~s~n",[ALayerStringRep]),
%%         io:format("== Beta layer ==~n~n~s~n",[BLayer]),
        BLayerStringRep=erl_prettypr:format(BLayer),
        io:format("== Beta layer source ==~n~n~s~n",[BLayerStringRep])
    catch
        _:Reason ->
            io:format("**ERROR**~n\treason: ~p~n",[Reason]),
            {stack_trace,Reason,erlang:get_stacktrace()}
    end.

                






















