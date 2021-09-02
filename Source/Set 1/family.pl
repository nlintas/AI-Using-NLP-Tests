:-['Lab01_SEARCH_LIBRARY.pl'].
/** Author: Nikolaos Lintas from University of Sheffield */
/*------------------------------------------------------------------
  THE FAMILY CROSSING THE BRIDGE PROBLEM DEFINITION 
------------------------------------------------------------------*/

initial_state(state([g,f,m,s,b],[],t_left,0,[],[])).

final_state(state([],_,t_right,_,_,_)).

operator(state(L,R,t_left,D,P,PathSoFar),state(NL,NR,t_right,ND,[(X,Y,Pace)|P],[pass(X,Y,left_to_right,Pace)|PathSoFar])) :-
	select(X,L,TL),
	select(Y,TL,NL),
	append([X,Y],R,NR),
	pace(X,P1),
	pace(Y,P2),
	max(P1,P2,Pace),
	ND is D+Pace.

operator(state(L,R,t_right,D,P,PathSoFar),state(NL,NR,t_left,ND,[(X,Pace)|P],[pass(X,right_to_left,Pace)|PathSoFar])) :-
	select(X,R,NR),
	append([X],L,NL),
	pace(X,Pace),
	ND is D+Pace.

pace(g,12).
pace(f,8).
pace(m,6).
pace(s,3).
pace(b,1).

max(X,Y,X):-X>=Y,!.
max(_,Y,Y).

same_state(state(L1,R1,_,D,_,_),state(L2,R2,_,D,_,_)):-
	foreach(member(X,L1),member(X,L2)),
	foreach(member(Y,R1),member(Y,R2)).
	
costOf(state(_,_,_,D,_,_),D).

