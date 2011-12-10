-include("rete.hrl").
%%=======================================================================
%% Parse transform segment
%%=======================================================================
-record(attribute,{line=0,type,value}).
-record(clause, {line=0, args, guards=[], body}).
-record(function, {line=0, name :: atom(), arity :: integer(), clauses :: [#clause{}]}).
-record(atom, {line=0, name}).
-record(integer, {line=0, value :: integer()}).
-record(cons, {line=0, term, next}).
-record(nil, {line=0}).
-record(record_field, {line=0, name :: #atom{}, value}).
-record(record, {line=0, name, fields :: [#record_field{}]}).
-record(tuple, {line=0, elements :: []}).
-record(var, {line=0, name :: atom()}).
-record(call, {line=0, func_name, args}).
-record(remote, {line=0,module,function}).
-record(match, {line=0,left,right}).
-record(eof, {line=0}).

-record(class, {type :: atom| list| tuple ,arity,props}).