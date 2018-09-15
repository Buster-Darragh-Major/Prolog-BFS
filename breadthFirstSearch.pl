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
	recurse(OpenQueue, 0).
	
% Change of tack - pass in a queue and iterate through all of them expanding each set and adding to the queue,
% removing closed sets as you go, hah
	
% Base Case TODO
recurse(StateQueue, Gvalue) :-
	
	
recurse(StateQueue, Gvalue) :-
	
	expandStates(StateQueue, [], ExpandedStates),
	
	join_queue(NeighbourStates, OpenQueue, NewOpenQueue),


% Unpacks a set of states from their tuples into a list 
% of only the second tuple.
unpackStates([], OutList, OutList).
unpackStates([(_, State)|T], DelegateList, OutList) :-
	append(DelegateList, [State], MidList),
	unpackStates(T, MidList, OutList).
	
	
% Takes a list of states and perform a check that they can be added to the expanded
% states list (i.e. not a repeat of the current list or already closed.), If so then
% they are added to the output list and marked as a node
appendState([], OutList, OutList, ParentState).
appendState([State|T], DelegateList, OutList, ParentState) :-
	(((not(closed(State)), not(node(_, State, _))) ->	(		% Only add a state if it isnt closed or already exisitng node
		node(_, ParentState, Gvalue),							% Get the g-value of the parent node
		assert(node(ParentState, State, Gvalue))				% Register current state as an existing node
		append(OutList, State, MidList)))),						
	appendState(T, MidList, OutList, ParentState).
	
	
% Passed a list of state tuples and outputs a list of those tuples expanded states
% with no repeats or closed states added, all state tuples passed in will be marked 
% as closed.
expandStates([], OutList, OutList).
expandStates([State|T], DelegateList, OutList) :-
	succ8(State, NeighbourTuples),											% Expand neighbouring states
	assert(closed(State)),													% Close the statebecause expanded
	
	unpackStates(NeighbourTuples, [], NeighbourStates),						% Unpack just the states from the tuples
	appendState(NeighbourStates, DelegateList, MidList, ParentState)		% take a list of states to add, the current List of expanded states and output the uptaded expanded list
	expandStates(T, MidList, OutList).										% Recurse