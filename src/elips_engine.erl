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
-module(elips_engine).

%%
%% Include files
%%

-include("private.hrl").
-include("log.hrl").

%%
%% Exported Functions
%%
-export([handle_wmo/2]).

%%
%% API Functions
%%

-spec handle_wmo({assert | retire, WME :: term()}, Env :: term()) -> [term()] .
handle_wmo({_Op,WME}=WMO, Env) ->
    % Get matching nodes
    case match_anodes(WME, Env) of
        [] -> [];
        MatchedANodes ->
            % For each matched a-node, get bnodes and do right join
            lists:flatten(
              [begin
                   BNode=get_bnode(BNodeId ,Env),
                   join_right_bnode(BNode, WMO, Env)
               end || #anode{bnode_ids=BNodeIds} <- MatchedANodes, BNodeId <- BNodeIds])
    end.

join_right_bnode(#bnode{id=Id,right_key_fun=RightKeyFun}=BNode, {_Op,WME}=WMO, Env) ->
    Key=RightKeyFun(WME),
    case modify_right_index(Id, Key, WMO, Env) of
        false -> % WM not modified
            [];
        true -> % WM modified
            FetchedTokens=fetch_left_index(Id, Key, Env),
            NewTokens=[ new_token(WME, T) || T <- FetchedTokens],
            downstream_tokens(BNode, _Op, NewTokens, Env)
    end.

join_left_bnode(#bnode{id=Id,left_key_fun=LeftKeyFun}=BNode, _Op, Token, Env) ->
    Key=LeftKeyFun(Token),
    modify_left_index(Id, Key, _Op, Token, Env),
    FetchedWMEs=fetch_right_index(Id, Key, Env),
    NewTokens=[ new_token(WME, Token) || WME <- FetchedWMEs],
    downstream_tokens(BNode, _Op, NewTokens, Env).

new_token(WME, Token) ->
    Token ++ [WME].

downstream_tokens(#bnode{id=Id,bnode_ids=BNodeIds,pnodes=PNodeIds}=BNode, _Op, Tokens, Env) ->
    % Filter tokens with b-node match critera (e.g. check additional guards before output)
    MatchedTokens=[ T || T <- Tokens, match_token(BNode, T)],
    % Each matched token pass to the next b-nodes (i.e. downstreaming)
    ActResults=
        [begin
             NextBNode=get_bnode(BNodeId,Env),
             join_left_bnode(NextBNode, _Op, Token, Env)
         end || Token <- MatchedTokens, BNodeId <- BNodeIds],
    % Each matched token pass to output nodes (p-nodes) if any
    ActResult=[push_token_to_pnode(Id, _Op, PNodeId, Token, Env) || Token <- MatchedTokens, PNodeId <- PNodeIds],
    [ActResult, ActResults].

push_token_to_pnode(FromBNodeId, WMO, PNodeId, Token, Env) ->
    PNode=#pnode{bnode_ids=BNodeIds,token_format=TokenFormat}=get_pnode(PNodeId,Env),
    case BNodeIds of
        [FromBNodeId] -> % Simple case
            activate_pnode(FromBNodeId, WMO, PNode, TokenFormat([Token]), Env);
        [_,_|_] -> % Cartesian product case
            % 1. Add pnode tokens for bnode data received from
            IsModified=modify_pnode_tokens(PNodeId, FromBNodeId, WMO, Token, Env), % TODO: It seems we don't 
                                                                                   % need to analyze whether 
                                                                                   % the data were modified, 
                                                                                   % because of this is already 
                                                                                   % done at alpha layer.   
            case IsModified of
                % 2a. In case modification occured read all components for product
                true ->
                    % TODO: Optimize. If some fetch results with [] then stop & return []
                    LLT=[if BNodeId==FromBNodeId -> [Token];
                            true -> fetch_pnode_tokens(PNodeId, BNodeId, Env)
                         end || BNodeId <- BNodeIds ],
                    case lists:any(fun(L)-> L==[] end, LLT) of
                        true -> [];
                        false -> activate_pnode(FromBNodeId, WMO, PNode, {cart, TokenFormat, LLT}, Env)
                    end;
                % 2b. Token have no effect
                false -> []
            end
    end.

match_anodes(WME, Env) ->
    [ ANode || ANode <- get_anodes(Env), match_anode(ANode, WME)].

match_anode(#anode{wme_constraint=Constr}, WME) when is_function(Constr, 1) ->
    Constr(WME).

match_token(#bnode{token_constraint=Constr,id=_BNodeId}, Token) when is_function(Constr, 1) ->
    Constr(Token).

    
%%
%% Local Functions
%%

%%==========================================
%% Environment calls
%%==========================================

get_bnode(BNodeId,Env) ->
    Env(get_bnode,[BNodeId]).

get_anodes(Env) ->
    Env(get_anodes,[]).

get_pnode(PNodeId,Env) ->
    Env(get_pnode,[PNodeId]).

fetch_left_index(BNodeId, Key, Env) ->
    Env(fetch_left_index, [BNodeId, Key]).

fetch_right_index(BNodeId, Key, Env) ->
    Env(fetch_right_index, [BNodeId, Key]).

fetch_pnode_tokens(PNodeId, BNodeId, Env) ->
    Env(fetch_pnode_tokens, [PNodeId, BNodeId]).

modify_left_index(BNodeId, Key, Op, Token, Env) ->
    ok=Env(modify_left_index, [BNodeId, Key, Op, Token]).

modify_right_index(BNodeId, Key, {Op, WME}, Env) ->
    Env(modify_right_index, [BNodeId, Key, Op, WME]).

modify_pnode_tokens(PNodeId, FromBNodeId, Op, Token, Env) ->
    Env(modify_pnode_tokens, [PNodeId, FromBNodeId, Op, Token]).

activate_pnode(Id, _Op, PNode, Token, Env) ->
    Env(activate_pnode, [Id, _Op, PNode, Token]).

