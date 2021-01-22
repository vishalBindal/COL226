% helper  
% contains(X, Ls) <-> Ls contains X
contains(X, [X|_]).
contains(X, [_|T]) :- contains(X, T).

% isUnion(X,Y,Un) <-> X U Y = Un
isUnion([X|Y], [X|Z], [X|U]) :- isUnion(Y, Z, U).
isUnion([A|X], Y, [A|U]) :- 
    isUnion(X, Yleft, U),
    takeout(A, Y, Yleft).
isUnion([], B, B).

% augment(G1, G2, G) <-> G1[G2] = G
augment(G1, [], G1). 
augment(G1, [(X,T)|Y], [(X,T)|G]) :-
    contains((X,Tx), G1),
    takeout((X,Tx), G1, G1left),
    augment(G1left, Y, G).
augment(G1, [(X,T)|Y], [(X,T)|G]) :-
    not(contains((X,_), G1)),
    augment(G1, Y, G).

% takeout(X, Ls1, Ls2) <-> Ls2 = Ls1 - {X}
takeout(X, [X|Y], Y).
takeout(X, [Y|Z], [Y|W]) :-
    not(X==Y),
    takeout(X, Z, W).
takeout(_, [], []).
% -----------------------------------------------------

% constants
hasType(_, true, tbool).
hasType(_, false, tbool).
hasType(_, unit, tunit).
hasType(_, num(_), tint).

% variables
hasType(Gamma, var(X), T) :- contains((X,T), Gamma).
hasType(Gamma, var(X), typeVar(X)) :- 
    not(contains((X,_), Gamma)).

% arithmetic operations over numerical expressions
hasType(Gamma, abs(A), tint) :-
    hasType(Gamma, A, tint).
hasType(Gamma, add(A,B), tint) :-
    hasType(Gamma, A, tint),
    hasType(Gamma, B, tint).
hasType(Gamma, sub(A,B), tint) :-
    hasType(Gamma, A, tint),
    hasType(Gamma, B, tint).
hasType(Gamma, mult(A,B), tint) :-
    hasType(Gamma, A, tint),
    hasType(Gamma, B, tint).
hasType(Gamma, div(A,B), tint) :-
    hasType(Gamma, A, tint),
    hasType(Gamma, B, tint).

% comparison operations over numerical expressions
hasType(Gamma, gt(A,B), tbool):- % greater than
    hasType(Gamma, A, tint),
    hasType(Gamma, B, tint).
hasType(Gamma, lt(A,B), tbool):- % less than
    hasType(Gamma, A, tint),
    hasType(Gamma, B, tint).
hasType(Gamma, gte(A,B), tbool):- % greater than or equal to
    hasType(Gamma, A, tint),
    hasType(Gamma, B, tint).
hasType(Gamma, lte(A,B), tbool):- % less than or equal to
    hasType(Gamma, A, tint),
    hasType(Gamma, B, tint).

% boolean operations over boolean expressions
hasType(Gamma, not(A), tbool) :-
    hasType(Gamma, A, tbool).
hasType(Gamma, and(A,B), tbool) :-
    hasType(Gamma, A, tbool),
    hasType(Gamma, B, tbool).
hasType(Gamma, or(A,B), tbool) :-
    hasType(Gamma, A, tbool),
    hasType(Gamma, B, tbool).

% equality over arbitrary expressions
hasType(Gamma, equal(A,B), tbool) :-
    hasType(Gamma, A, T),
    hasType(Gamma, B, T).

% conditional expressions
hasType(Gamma, if_then_else(C, A, B), T) :-
    hasType(Gamma, C, tbool),
    hasType(Gamma, A, T),
    hasType(Gamma, B, T).

% qualified expressions
hasType(Gamma, let_in_end(D, E), T) :-
    typeElaborates(Gamma, D, G1),
    augment(Gamma, G1, Ga),
    hasType(Ga, E, T).

% function abstraction
hasType(Gamma, lambda(X, E), arrow(Tx, Te)) :-
    hasType(Gamma, X, Tx),
    hasType(Gamma, E, Te).

% function application
hasType(Gamma, apply(M, E), T2) :-
    hasType(Gamma, M, arrow(T1, T2)),
    hasType(Gamma, E, T1).

% n-tuples
hasType(_, tup(0, []), tunit).
hasType(Gamma, tup(1, [X]), T) :-
    hasType(Gamma, X, T).
hasType(Gamma, tup(2, [X,Y]), tcp(2, [T1, T2])) :-
    hasType(Gamma, X, T1),
    hasType(Gamma, Y, T2).
hasType(Gamma, tup(N, [X|Ls]), tcp(N, [T|Tls])) :-
    N1 is N-1,
    hasType(Gamma, tup(N1, Ls), tcp(N1, Tls)),
    hasType(Gamma, X, T).

% projection on tuples
hasType(Gamma, proj(1, tup(_, [X|_])), T) :-
    hasType(Gamma, X, T).
hasType(Gamma, proj(K, tup(N, [_|Ls])), T) :-
    K1 is K-1,
    N1 is N-1,
    hasType(Gamma, proj(K1, tup(N1, Ls)), T).

% ----------------------------------------------

% simple definitions
typeElaborates(_, def(tup(0, []), tup(0, [])), []).
typeElaborates(Gamma, def(tup(N, [var(X)|Xls]), tup(N, [E|Els])), [(X,T)|Ls]) :-
    hasType(Gamma, E, T),
    N1 is N-1,
    typeElaborates(Gamma, def(tup(N1, Xls), tup(N1, Els)), Ls).

typeElaborates(Gamma, def(var(X),E), [(X,T)]) :- 
    hasType(Gamma, E, T).


% D1 || D2
typeElaborates(Gamma, parallel(D1, D2), U) :-
    typeElaborates(Gamma, D1, G1),
    typeElaborates(Gamma, D2, G2),
    isUnion(G1, G2, U),!.

% D1 ; D2
typeElaborates(Gamma, seq(D1, D2), Ga2) :-
    typeElaborates(Gamma, D1, G1),
    augment(Gamma, G1, Ga1),
    typeElaborates(Ga1, D2, G2),
    augment(G1, G2, Ga2),!.

% local D1 in D2 end
typeElaborates(Gamma, local_in_end(D1, D2), G2) :-
    typeElaborates(Gamma, D1, G1),
    augment(Gamma, G1, Ga1),
    typeElaborates(Ga1, D2, G2),!.
