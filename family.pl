
% DEFINITIONS:

% motherOf
motherOf(Mother, Person) :- childOf(Person, Mother), female(Mother).

% sisterOf
% 
sisterOf(Sister, Person) :- childOf(Sister, P1), 
			childOf(Person, P1), 
			female(Sister), 
			not(Sister = Person).
	

% inSameTree 
inSameTree(Node1, Node2) :-
	getRootNode(Node1, Root1),
	getRootNode(Node2, Root2),
	Root1 = Root2.
	
getRootNode(RootNode, RootNode) :-
	not(parentOf(_, RootNode)).
getRootNode(Node, RootNode) :-
	parentOf(NextNode, Node),
	getRootNode(NextNode, RootNode).