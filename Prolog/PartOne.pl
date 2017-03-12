% Boniface Sindala
% Andrew Markley
% CS401
% Assignment 3


:- dynamic pair/2.

pair(x,y).

is_a(X,Y) :- pair(X,Y).
is_a(X,Z) :- pair(Y,Z), is_a(X,Y).

start :-
  write('You may begin to type facts and queries in the form'),
  nl,
  write('_ is a _.'),
  nl,
  write('A _ is a _.'),
  nl,
  write('Is _ a _?'),
  nl,
  write('Type "exit." to quit'),
  nl,
  repeat,
    (
    read_string(user_input, "\n", "\r\t", _, X),nl,
    clean_string(X, Y),
    assess(Y),
    fail
    ).

assess(X) :-
  (
  last(X, 'exit.') -> write('Quitting...'), nl, retractall(pair(,));
  is_statement(X) -> make_fact(X);
  is_query(X) -> find_answer(X)
  ).

clean_string(W,A) :-
  split_string(W, ' ', '', X),
  maplist(downcase_atom, X, Y),
  subtract(Y, [is,a,an], Z),
  separate_end(Z,A).

find_answer([X|[Y|_]]) :-
  is_a(X,Y) -> write('Yes!'), nl;
  write('unknown'), nl.

make_fact([X|[Y|_]]) :-
  assert(pair(X,Y)),
  write('Ok.'), nl.

separate_end(X,Result) :-
  last(X,Y),
  nth0(0,X,First,_),
  atom_length(Y,Length),
  is( Decrement, -(Length, 1)),
  sub_atom(Y, 0, Decrement, 1, Word),
  sub_atom(Y, _, 1, 0, Punc),
  create_list(First, Word, Punc, Result).

create_list(X, Y, Z, Return) :- append([X, Y], [Z], Return).

is_statement(X) :- last(X,'.').
is_query(X) :- last(X,?).
