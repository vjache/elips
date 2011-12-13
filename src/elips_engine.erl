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
    ?ECHO(join_right_bnode),
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
    ?ECHO(join_left_bnode),
    Key=LeftKeyFun(Token),
    modify_left_index(Id, Key, _Op, Token, Env),
    FetchedWMEs=fetch_right_index(Id, Key, Env),
    NewTokens=[ new_token(WME, Token) || WME <- FetchedWMEs],
    downstream_tokens(BNode, _Op, NewTokens, Env).

new_token(WME, Token) ->
    Token ++ [WME].

downstream_tokens(#bnode{id=Id,bnode_ids=BNodeIds,pnodes=PNodes}=BNode, _Op, Tokens, Env) ->
    ?ECHO({downstream_tokens, Tokens}),
    % Filter tokens with b-node match critera (e.g. check additional guards before output)
    MatchedTokens=[ T || T <- Tokens, match_token(BNode, T)],
    % Each matched token pass to the next b-nodes (i.e. downstreaming)
    ActResults=
        [begin
             NextBNode=get_bnode(BNodeId,Env),
             join_left_bnode(NextBNode, _Op, Token, Env)
         end || Token <- MatchedTokens, BNodeId <- BNodeIds],
    % Each matched token pass to output nodes (p-nodes) if any
    ActResult=
        if PNodes ->
               [activate_pnode(Id, _Op, true, Token, Env) || Token <- MatchedTokens];
           true ->
               []
        end,
    [ActResult, ActResults].

match_anodes(WME, Env) ->
    [ ANode || ANode <- get_anodes(Env), match_anode(ANode, WME)].

match_anode(#anode{wme_constraint=Constr}, WME) when is_function(Constr, 1) ->
    Constr(WME).

match_token(#bnode{token_constraint=Constr,id=BNodeId}, Token) when is_function(Constr, 1) ->
    R=Constr(Token),
    ?ECHO({match_token, BNodeId, Token, R}),
    R.

    
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

fetch_left_index(BNodeId, Key, Env) ->
    ?ECHO({fetch_left_index, BNodeId, Key}),
    Env(fetch_left_index, [BNodeId, Key]).

fetch_right_index(BNodeId, Key, Env) ->
    ?ECHO({fetch_right_index, BNodeId, Key}),
    Env(fetch_right_index, [BNodeId, Key]).

modify_left_index(BNodeId, Key, Op, Token, Env) ->
    ok=Env(modify_left_index, [BNodeId, Key, Op, Token]).

modify_right_index(BNodeId, Key, {Op, WME}, Env) ->
    Env(modify_right_index, [BNodeId, Key, Op, WME]).

activate_pnode(Id, _Op, PNode, Token, Env) ->
    Env(activate_pnode, [Id, _Op, PNode, Token]).

