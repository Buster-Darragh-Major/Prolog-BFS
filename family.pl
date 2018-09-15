
% DEFINITIONS:

% motherOf
% MotherOf(x, y) = ChildOf(y, x) and Female(x)
motherOf(Mother, Person) :- childOf(Person, Mother), female(Mother).

% sisterOf
% SisterOf(x, y) = E(p)ChildOf(x, p) and ChildOf(y, p) and Female(x) and x not y
sisterOf(Sister, Person) :- childOf(Sister, P1), 
			childOf(Person, P1), 
			female(Sister), 
			not(Sister = Person).

	
parentOf(a, b).
parentOf(b, c).
parentOf(c, d).
parentOf(c, e).
parentOf(c, f).
parentOf(f, g).
parentOf(g, h).

parentOf(x, y).
parentOf(y, z).
parentOf(x, x1).
parentOf(x, y2).
	

inSameTree(Node1, Node2) :-
	(ancestorOfOrSelf(X, Node1), ancestorOfOrSelf(X, Node2)).
	

% Helper method for finding direct ancestors
ancestorOfOrSelf(Ancestor, Descendant) :-
	(Ancestor = Descendant);
	parentOf(Ancestor, Descendant);
	(parentOf(ParentX, Descendant), ancestorOfOrSelf(Ancestor, ParentX)).