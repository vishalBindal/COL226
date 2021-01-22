man(luke).
man(socrates). 
man(X) :- alive(X). 
alive(chewbacca). 
mortal(X) :- man(X). 
father(anakin, luke).
great(Y) :-  man(X), father(Y,X).
sisters(X,Y) :- mother(Z,X), mother(Z,Y), father(A,X), father(A,Y), female(X), female(Y), older(X,Y).
female(ria).
female(lee).
female(emma).
mother(ria,lee).
mother(ria,emma).
father(andrew,lee).
father(andrew,ben).
father(andrew,emma).
older(andrew,ria).
older(ria,emma).
older(emma,lee).
