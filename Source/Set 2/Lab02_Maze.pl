:-['Lab02_SEARCH_LIBRARY.pl'].
/** Author: Nikolaos Lintas from University of Sheffield */
%%% Maze Problem definition

/* GENERAL COMMENTS
1. A state is represented by a LIST that contains the 
path from the initial square to the "current" square. The "current" 
square is the one that actually belongs to the FrontierSet. The rest
of the list forms the Path that has been followed till the current square.
So a state has the form:
 state((Xn,Yn),[...,(X2,Y2),(X1,Y1)],[Blocked_SQ]) 
where (Xn,Yn) is the "current" square and (Xn-1,Yn-1) - (X1,X2) the path.
Of course the solution is presented in the reverse order. 
*/ 

:- dynamic start_point/1, end_point/1, blocked_square/1, limit/1.



%%% Table representation.
%%% Representing only blocked squares. 

%%% blocked_square/1
%%% blocked_square((X,Y)).
%%% (X,Y) the blocked Square Co-ordinates.
%%% Original Problem Presented in the slides. 
blocked_square((1,1)).
blocked_square((1,2)).
blocked_square((1,3)).
blocked_square((1,4)).
blocked_square((1,5)).
blocked_square((1,6)).
blocked_square((1,7)).
blocked_square((1,8)).
blocked_square((1,9)).
blocked_square((1,10)).

blocked_square((2,1)).

blocked_square((3,1)).
blocked_square((3,3)).
blocked_square((3,4)).
blocked_square((3,5)).
blocked_square((3,7)).
blocked_square((3,8)).
blocked_square((3,9)).

blocked_square((4,1)).
blocked_square((4,5)).
blocked_square((4,7)).

blocked_square((5,1)).
blocked_square((5,3)).
blocked_square((5,4)).
blocked_square((5,5)).
blocked_square((5,7)).
blocked_square((5,8)).
blocked_square((5,9)).

blocked_square((6,1)).
blocked_square((6,9)).

blocked_square((7,1)).
blocked_square((7,4)).
blocked_square((7,5)).
blocked_square((7,6)).
blocked_square((7,7)).
blocked_square((7,9)).

blocked_square((8,1)).
blocked_square((8,4)).
blocked_square((8,5)).
blocked_square((8,6)).
blocked_square((8,7)).
blocked_square((8,9)).

blocked_square((9,1)).
blocked_square((9,4)).
blocked_square((9,5)).
blocked_square((9,6)).
blocked_square((9,7)).
blocked_square((9,9)).

end_point((4,10)).
start_point((10,1)).
limit(10).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Mazze Problem Generator
%%% Unfortunately the problems generated do not guarantee to
%%% have a solution in order to evaluate the algorithms in their 
%%% full capabilities. (completeness)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% shows information about the problem
show_problem:-
	listing(blocked_square/1),
	listing(end_point/1),
	listing(start_point/1),
	listing(limit/1).
	
%%% main Problem generating predicate.
%%% Density is the percentage of squares of the mazze that will be 
%%% blocked. 

generate_problem(Size,Density):-
	clean_up,
	assert(start_point((1,1))),
	assert(end_point((Size,Size))),
	assert(limit(Size)),
	write('Initial point: '), write((1,1)),nl,
	write('End point: '), write((Size,Size)),nl,
	write('Density: '), write(Density),nl,nl,
	NumBlocked is round(Size*Size*(Density/100)),
	generate_table(Size,Size,TempCandidates),
	subtract(TempCandidates,[(1,1),(Size,Size)],Candidates),
	generate_blocked(NumBlocked,Candidates).
	

%%% Generates the complete mazze table, of a given size.
generate_table(0,_,[]).

generate_table(RowNum,Size,Table):-
	gen_row(RowNum,Size,Row),
	RowNum > 0,
	NewRowNum is RowNum - 1,
	generate_table(NewRowNum,Size,RestTable),
	append(RestTable,Row,Table).
	
%%% Auxiliary predicate that generates a row.	
%%gen_row(RowNum,0,[(RowNum,0)]).
gen_row(_RowNum,0,[]).

gen_row(RowNum,Size,[(RowNum,Size)|RestOfRow]):-
	Size > 0, NewSize is Size - 1,
	gen_row(RowNum,NewSize,RestOfRow).	
		
%%% Clean up actions for dynamic goals.	
clean_up:-
	abolish(start_point/1),
	abolish(end_point/1),
	abolish(blocked_square/1),
	abolish(limit/1).

%%% generate_blocked squares and asserts the correspoding facts.
generate_blocked(0,_).

generate_blocked(Blocked,Candidates):-
	Blocked > 0,
	NewBlocked is Blocked - 1,
	length(Candidates,Len),
	PickBlock is random(Len),
	nth0(PickBlock,Candidates,B),
	assert(blocked_square(B)),
	select(B,Candidates,RestCand),
	generate_blocked(NewBlocked,RestCand).	
	
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% goal/1.
%%% The goal state, ie. the coordinates of the 
%%% final block (exit).

final_state(state(P,_PATH,_)):-
	end_point(P).

%%% initial/1.
%%% The initial state, of the mazze. 

initial_state(state(P,[],Blocked_sq)):-
	findall(X,blocked_square(X),Blocked_sq),
	start_point(P).
	
%%% same state predicate relarion.
same_state(state(AT,_,_),state(AT,_,_)).

%%% operator/2.
%%% operator(Parent,Child).
%%% Generates from the current square one legal square 
%%% that we can move to. 
 
operator(state(Parent,Path,BLOCKED),state(Child,[Parent|Path],BLOCKED)):-
	can_go(Parent,Child),
	not(member(Child,BLOCKED)),
	not(member(Child,Path)), %%% checks for loops in a single path. 
	valid(Child).

%%% can_go/2
%%% can_go((X,Y),(NX,NY))
%%% Generates a square to which we can move to, 
%%% even an illegal one. Note: on backtracking it creates 
%%% all possible squares. 

can_go((X,Y),(NX,Y)):- NX is X + 1.
can_go((X,Y),(NX,Y)):- NX is X - 1.
can_go((X,Y),(X,NY)):- NY is Y + 1.
can_go((X,Y),(X,NY)):- NY is Y - 1.

%%% valid/1.
%%% valid((X,Y)).
%%% Succeeds if the square (X,Y) is a square that belongs 
%%% to the table. 

valid((X,Y)):-
	X>0,Y>0,
	limit(L),
	Y=<L,X =< L.

%%% costOf/2
%%% costOf(State,Cost)
costOf(state(_,Path,_),Cost):-
	length(Path,Cost).

%%% Heuristic Functions 
%%% Manhattan Distance
heuristic_Func(state((X,Y),_,_),HV,bestf,manhattan):-
	final_state(state((XF,YF),_,_)),
	HV is abs(X-XF) + abs(Y-YF).

%%% astar for maze problem.
%%% Manhattan Distance
heuristic_Func(State,HV,astar,manhattan):-
	costOf(State,C),
	heuristic_Func(State,H,bestf,manhattan),
	HV is C + H.
	
%%% hc for maze problem.
%%% Manhattan Distance
heuristic_Func(state((X,Y),_,_),HV,hc,manhattan):-
	final_state(state((XF,YF),_,_)),
	HV is abs(X-XF) + abs(Y-YF).