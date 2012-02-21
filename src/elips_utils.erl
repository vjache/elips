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
%%% Created : Feb 21, 2012
%%%-------------------------------------------------------------------------------
-module(elips_utils).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([monitor_name/1]).

%%
%% API Functions
%%

monitor_name(Reg) ->
    CallaerPid=self(),
    spawn_link(
      fun()->
              monitor_name_init(Reg, CallaerPid)
      end).

%%
%% Local Functions
%%

resolve({local,    Name}) ->
    whereis(Name);
resolve({global,    Name}) ->
    global:whereis_name(Name).

monitor_name_init(Reg, Pid) when is_pid(Pid) ->
    process_flag(trap_exit, true), 
    monitor_name_loop(Reg, Pid, undefined).

monitor_name_loop(Reg, Pid, undefined) ->
    timer:sleep(500),
    monitor_name_loop(Reg, Pid, resolve(Reg));

monitor_name_loop(Reg, Pid, Obj) ->
    Pid ! {reg, Reg},
    link(Obj),
    receive 
        {'EXIT', Obj, _Reason} ->
            Pid ! {unreg, Reg}
    end,
    monitor_name_loop(Reg, Pid, undefined).