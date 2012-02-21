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
%%% Created : Nov 23, 2011
%%%-------------------------------------------------------------------------------
-module(elips_transform).

%%
%% Include files
%%

-include("private.hrl").

%%
%% Exported Functions
%%
-export([parse_transform/2]).

%%
%% API Functions
%%

parse_transform(Forms, _Options) ->
%%     io:format("AST-BEFORE: ~p ~n",[Forms]),
    {Forms1, Acc1}=lists:mapfoldl(fun handle_form/2, dict:new(), Forms),
    Forms2=final_handle(Forms1,Acc1),
%%     io:format("AST-AFTER: ~p ~n",[Forms2]),
    io:format("== RESULT CODE ==~n~n~s~n",
              [erl_prettypr:format(erl_syntax:form_list(Forms2))]),
    Forms2.

% Export alpha_nodes/0 & beta_node/1
handle_form(#attribute{type=export,value=List}=Form, Acc) ->
    case lists:member({handle_pattern,3}, List) of 
        true ->
            {Form#attribute{type=export,value=List++[{alpha_nodes,0},{beta_node,1},{p_node,1}]},Acc};
        false -> 
            {Form,Acc}
    end;
% Analyze handle_pattern/2
handle_form(#function{name=handle_pattern, arity=3}=Form, Acc) ->
    {Form, dict:store(make_result, elips_builder:make(Form), Acc)};
handle_form(Form, Acc) ->
    {Form, Acc}.

final_handle(Forms,Acc) ->
    {value, #eof{line=EofLine}, Forms2}=lists:keytake(eof, 1, Forms),
    case dict:find(make_result, Acc) of
        {ok, {FuncAlpha,FuncBeta,FuncP,_} } ->
            Forms2 ++ [FuncAlpha, FuncBeta, FuncP, #eof{line=EofLine}];
        error ->
            throw(not_elips_behavior)
    end.

%%
%% Local Functions
%%
