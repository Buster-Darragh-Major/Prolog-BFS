
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
	getRootNode(Node1, Root1),
	getRootNode(Node2, Root2),
	Root1 = Root2.
	
getRootNode(RootNode, RootNode) :-
	not(parentOf(_, RootNode)).
getRootNode(Node, RootNode) :-
	parentOf(NextNode, Node),
	getRootNode(NextNode, RootNode).