:- consult(counter).
:- consult(eightPuzzle).
:- consult(queues).
:- use_module(library(lists)).

:- dynamic closed/1.
:- dynamic node/3.
:- dynamic stats/4.

breadthFirstSearch(Input, Solution, Statistics) :-

	retractall(closed(_)),
	retractall(node(_,_,_)),
	retractall(stats(_,_,_,_)),
	
	% Initialise counters
	initialiseCounter(_, generated),		% Initialise counter for all g-levels
	initialiseCounter(_, duplicated),		% Initialise counter for all duplicated per g-level
	initialiseCounter(_, expanded),			% Initialise counter for all expanded per g-level
	
	incrementCounter(0, expanded),			% Gvalue 0 is expanded once
	make_queue(OpenQueue),
	assert(node([], Input, 0)),				% Store first node with initial input at g-level 0.
	recurse(Input, OpenQueue, EndState, 1),	% Recurse on level 1
	getSolution(EndState, [], Solution),

	node(_, EndState, Gvalue),
	unpackStatistics(Gvalue),
	wrapper(Statistics).

incr(X, X1) :-
	X1 is X+1.
	

decr(X, X1) :-
	X1 is X-1.

	
% list_join_queue doesnt do what it says it should so this does.
appendQueue([], OutQueue, OutQueue).
appendQueue([State|T], InQueue, OutQueue) :-
	join_queue(State, InQueue, MidQueue),
	appendQueue(T, MidQueue, OutQueue).
	
	
% Base Case
recurse(State, _, State, _) :- goal8(State).
recurse(State, StateQueue, Solution, Gvalue) :-
	expandStates(State, ExpandedStates, Gvalue),
	
	incrementCounter(Gvalue, expanded),		% Node has been expanded.
	assert(closed(State)),
	
	appendQueue(ExpandedStates, StateQueue, JoinedQueue),
	serve_queue(JoinedQueue, NextState, NextQueue),
	
	incr(Gvalue, IncrG),
	recurse(NextState, NextQueue, Solution, IncrG).


% Unpacks a set of states from their tuples into a list 
% of only the second tuple. TESTED
unpackStates([], OutList, OutList).
unpackStates([(_, State)|T], DelegateList, OutList) :-
	append(DelegateList, [State], MidList),
	unpackStates(T, MidList, OutList).
	
	
% Takes a list of states and perform a check that they can be added to the expanded
% states list (i.e. not a repeat of the current list or already closed.), If so then
% they are added to the output list and marked as a node TESTED
filterState([], OutList, OutList, _, _).
filterState([State|T], DelegateList, OutList, ParentState, Gvalue) :-

	incrementCounter(Gvalue, generated),							% Every time this enters a new generated state is being filtered
	
	((not(closed(State)), not(node(_, State, _))) -> (			% Only add a state if it isnt closed or already exisitng node
		incr(Gvalue, IncrG),
		assert(node(ParentState, State, IncrG)),				% Register current state as an existing node
		append(DelegateList, [State], MidList),

		filterState(T, MidList, OutList, ParentState, Gvalue)))
	;
		incr(Gvalue, IncrG),
		incrementCounter(IncrG, duplicated),					% Every time this is called state is duplicate
		filterState(T, DelegateList, OutList, ParentState, Gvalue).						
	
	
	
% Passed a list of state tuples and outputs a list of those tuples expanded states
% with no repeats or closed states added, all state tuples passed in will be marked 
% as closed.
expandStates(State, OutList, Gvalue) :-
	succ8(State, NeighbourTuples),									% Expand neighbouring states
	unpackStates(NeighbourTuples, [], NeighbourStates),				% Unpack just the states from the tuples
	filterState(NeighbourStates, [], OutList, State, Gvalue).		% take a list of states to add, the current List of expanded states and output the uptaded expanded list

	
% Walks up the inheritance tree and creates a list of states from the goal state
% to the root starting state. TESTED
getSolution([], SolutionList, SolutionList).
getSolution(LeafState, DelegateList, SolutionList) :-
	append([LeafState], DelegateList, NewSolutionList),
	node(ParentState, LeafState, _),
	getSolution(ParentState, NewSolutionList, SolutionList).
	

unpackStatistics(-1).
unpackStatistics(Gvalue) :-
	getValueCounter(Gvalue, generated, GeneratedValue),
	getValueCounter(Gvalue, duplicated, DuplicatedValue),
	getValueCounter(Gvalue, expanded, ExpandedValue),

	assert(wrapper(stats(Gvalue, GeneratedValue, DuplicatedValue, ExpandedValue))),
	
	decr(Gvalue, DecrG),
	unpackStatistics(DecrG).