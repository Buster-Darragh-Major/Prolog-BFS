:- consult(counter).
:- consult(eightPuzzle).
:- consult(queues).
:- use_module(library(lists)).

breadthFirstSearch(Input, Solution, Statistics) :-
	
	% Initialise counters
	initialiseCounter(_, Generated),	% Initialise counter for all g-levels
	initialiseCounter(_, Duplicated),	% Initialise counter for all duplicated per g-level
	initialiseCounter(_, Expanded),		% Initialise counter for all expanded per g-level
	
	make_queue(ClosedQueue),
	make_queue(FreshOpenQueue),
	join_queue(Input, FreshOpenQueue, OpenQueue),
	assert(node([], Input, 0),			% Store first node with initial input at g-level 0.
	recurse(OpenQueue, []).
	
% Base Case
recurse(OpenQueue, ClosedList) :-
	
	
recurse(OpenQueue, ClosedList) :-
	serve_queue(OpenQueue, State, RemainingQ),				% Extract first node
	succ8(State, NeighbourTuples),							% Expand neighbouring states
	
	unpackStates(NeighbourTuples, [], NeighbourStates),		% Unpack just the states from the tuples
	node(_, State, G),										% Find g state of current 
	storeNodes(State, NeighbourStates, G + 1),
	
	
% Also assert node objects with the state, the g value and the parent node.
% Predicate for implementing a closed list in terms of state - use asserts
closed(State).





% Saves a list of states as a node along with a reference 
% to a parent node and a g-value
saveNodes(ParentState, [], Gvalue).
saveNodes(ParentState, [State|T], Gvalue) :-
	assert(node(ParentState, State, Gvalue).

% Unpacks a set of states from their tuples into a list 
% of only the second tuple.
unpackStates([], OutList, OutList).

unpackStates([(_, State)|T], DelegateList, OutList) :-
	append(DelegateList, [State], MidList),
	unpackStates(T, MidList, OutList).