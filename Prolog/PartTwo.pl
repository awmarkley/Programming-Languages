% Boniface Sindala
% Andrew Markley
% CS401
% Assignment 3

:-initialization(run).

family(	wife	).
family(	husband	).
family(	brother	).
family(	sister	).
family(	mother	).
family(	grandfather	).
family(	father	).
family(	dad	).
family(	mom	).
family(	son	).
family(	daughter	).
family(	grandson	).
family(	uncle	).
family(	aunt	).
family(	cousin	).
family(	grandmother	).
family(	granddaughter	).

greeting( hello) .
greeting( hi ).
greeting( yo ).
greeting( sup ).

health(	shivers	).
health(	cold	).
health(	cough	).
health(	fever	).
health(	vomiting	).
health(	sweats	).
health(	sleepiness	).
health(	exhaustion	).
health(	health	).
health(	frostbite	).
health(	soreness	).
health( cramps ).

question( why ).
question( what ).
question( how ).

query( name ).

run :-
  nl,nl,
  write('HAL 9000: Tell me about your problems.'),nl,
  write('HAL 9000: I understand many things about your family and your health.'),
  nl, nl,
  repeat,
    (
    read_string(user_input, "\n", "\r\t.?", _, X),
    clean_string(X, Y),
    assess(Y),
    fail
    ).

clean_string(W,Y) :-
  split_string(W, ' ', '', X),
  maplist(downcase_atom, X, Y).

make_fact([X|[Y|_]]) :-
  assert(pair(X,Y)).

assess(X) :-
  is_generic(greeting,X) -> greeting_reply(X);
  is_generic(query,X) -> query_reply(X);
  is_generic(family,X) -> family_reply(X);
  is_generic(health,X) -> health_reply(X);
  is_generic(question,X, Res) -> question_reply(Res);
  write('HAL 9000: I see. Please continue.'),nl,nl.

is_generic(Fact, X) :-
  get_list( Fact, Z ),
  find( X, Z, Res),
  Res \== [].

is_generic(Fact, X, Res) :-
  get_list( Fact, Z ),
  find( X, Z, Res),
  Res \== [].

get_list(Fn, X) :- findall(A, call(Fn,A), X).

family_reply([]).
family_reply([H|T]) :-
  family(H) -> write('HAL 9000: I want to hear more about your '),
  write(H), write('.'),nl,nl;
  family_reply(T).

health_reply([]).
health_reply([H|T]) :-
  health(H) -> write('HAL 9000: Please tell me more about your '),
  write(H), write('.'),nl,nl;
  health_reply(T).

question_reply(X) :-
  X = [how] -> write('HAL 9000: I will not make it that easy for you.'),nl,nl;
  X = [what] -> write('HAL 9000: I am not really sure I want to answer that.'),nl,nl;
  X = [why] -> write('HAL 9000: Humans worry too much about the why of things.'),nl,nl.

query_reply(X) :-
  write('HAL 9000: I am a Heuristically programmed ALgorithmic computer.'),nl,
  write('HAL 9000: You may call me HAL for short.'),nl,nl.

greeting_reply([H|T]) :- write('HAL 9000: Hello there. How are you?'),nl,nl.

find(A,B, Res) :-
  findall(X, (nth0(_, A, X), member(X, B)), Res).
