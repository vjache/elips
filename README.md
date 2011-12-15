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

TBD 

## Usage ##

TBD

## Examples ##

TBD