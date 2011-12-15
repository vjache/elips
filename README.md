Erlang Language Integrated Production System (ELIPS).
----------------------------------------------------------------------------------

## Intro ##

This library is a tool to create a reactive rule based agents. The rules of behavior 
of an agent specified via a special function handle_pattern/3, where each clause 
declare one rule. Each such clause have its Left Hand Side (LHS) which is a pattern 
specification and Right Hand Side (RHS) which define a reaction. LHS in its turn 
consists of pattern itself and guards. Pattern is a list of term (fact) patterns. 
RHS is an ordinary Erlang code optionally returning a set of asserts or retires.
Example:
    TBD


## Background ##

The name of this library and basic concept inspired by CLIPS (http://clipsrules.sourceforge.net/), 
nevertheless, at the current state ELIPS have a different ideology than CLIPS and probably could 
not be compared directly (e.g. like JESS and CLIPS). ELIPS is a really integrated to an Erlang 
language. In contrast with CLIPS, ELIPS have no its own language to represent rules but it 
successfully uses the Erlang language itself slightly changing (augmenting) semantics of pattern 
matching, fortunately Erlang already pattern matching language. And fortunately Erlang have a 
"parse transform" mechanism to be able to hack a language:) 

## Design & implementation notes ##

This library allows to create a so called ELIPS agents. One such an agent is represented by 
its module which is a callback module for an ELIPS behavior. Also, such a module must use parse 
transform to be augmented with a couple of functions generated as a result of analysis of rules 
declared in an agent module. At runtime this module is plugged into a special host process 
(see elips.erl). This process is capable to accept a messages (or events if you want). 
The accepted messages are translated to an operations like ``assert`` and ``retire``. These 
operations modify a working memory of an agent. Working memory implemented using a core Erlang
module ``ets``. Each agent process have its separated ``ets`` based working memory. When working 
memory accumulates a data that matches some rule the rule is activated and RHS is executed.    

## Usage ##

### Integrate library to your project ###

This is a rebar'ized project, so, if you are already using rebar, just insert a reference 
to this git repo at your rebar.config. Otherwise clone this repo, and run ``erl -make``. 
To compile your project, ELIPS library must be on ``ERL_LIBS`` to compile your agents or use 
``-pa`` for ``erlc``:
 *   ``env ERL_LIBS=path\to\the\elips erlc my_proj\src\myagent.erl``
 *   ``erlc -pa path\to\the\elips\ebin my_proj\src\myagent.erl``

### Implementing your own agent ###

To create your own ELIPS agent you must create a module say ``myagent``. This module MUST have a 
set of functions satisfying to some conventions. In Erlang world we say that it is a callback 
module implementing a behavior ``elips``. Also, module ``myagent`` must use a special parse transform 
to be augmented with auto generated hidden functions. On practice you must follow these steps:

 *   first of all insert in ``myagent`` directive ``-include_lib("elips/include/elips.hrl").``
 *   implement functions:
  *   ``init/1`` - initialize a state of an agent and working memory (WM)
  *   ``handle_event/3`` - accepts an event sent with elips:notify/2 optionally changes the state and WM  
  *   ``handle_info/2`` - similar to handle_event/3 but it accepts a system messages and those sent not with elips:notify/2
  *   ``handle_pattern/3`` - handles a patterns matched against WM i.e. it may be considered as a set of rules
  *   ``terminate/3`` - handles a termination of an agent process
  *   ``code_change/3`` - handles an event of code change

To run an agent it is enough to say something like ``elips:start(myagent_1,myagent,[],[])``.
Copy a ``examples/elips_skeleton.erl`` and rename to ``your_agent_module.erl`` to implement a new agent. 

### Runtime ###

After you perform ``elips:start(myagent_1,myagent,[],[])`` the new process is created and 
registered with name ``myagent_1`` and initialized with ``myagent``. This process ready 
to accept events and handle them using ``myagent`` callback module. To send events use 
``elips:notify(myagent_1, {some_event, with, some, params})``. It is quite similar to 
standard OTP behavior ``gen_event``. So, each agent is a separate Erlang process.

## Examples ##

TBD