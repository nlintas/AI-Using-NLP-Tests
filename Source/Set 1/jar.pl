:-['Lab01_SEARCH_LIBRARY.pl'].
/** Author: Nikolaos Lintas from University of Sheffield */
/*------------------------------------------------------------------
  THE GLASS PROBLEM DEFINITION 
------------------------------------------------------------------*/

initial_state(state(0,0,[])).

final_state(state(_,40,_)).
final_state(state(40,_,_)).

glass1(70).
glass2(50).

% 
operator(state(V1,V2,P),state(Vsum,0,[' Empty B in A '|P])) :- 
	V2 > 0, 
	Vsum is V1 + V2, 
	glass1(G1),
 	Vsum =< G1. 

% 
operator(state(V1,V2,P),state(0,Vsum,[' Empty A in B '|P])) :- 
	V1 > 0, 
	Vsum is V1 + V2, 
	glass2(G2),
	Vsum =< G2. 

% 
operator(state(V1,V2,P),state(G1,Vdiff,[' Empty B in A until A is full '|P])) :- 
	V2 > 0, 
	V1 >= 0, 
	glass1(G1), 
	V1 < G1,
	Vdiff is V2 - ( G1 - V1 ), 
	Vdiff > 0.

% 
operator(state(V1,V2,P),state(Vdiff,G2,[' Empty A in B until B is full '|P])) :- 
	V1 > 0, 
	V2 >= 0, 
	glass2(G2), 
	V2 < G2,
 	Vdiff is V1 - ( G2 - V2 ), 
 	Vdiff > 0.

% 
operator(state(V1,V2,P),state(G1,V2,[' Fill A (if not full) '|P])) :- glass1(G1), V1\=G1.

% 
operator(state(V1,V2,P),state(V1,G2,[' Fill B (if not full) '|P])) :- glass2(G2), V2\=G2.

% 
operator(state(V1,V2,P),state(0,V2,[' Empty A '|P])):- V1 \=0, V2\=0.

% 
operator(state(V1,V2,P),state(V1,0,[' Empty B '|P])):- V2 \=0, V1\=0.

same_state(state(V1,V2,_),state(V1,V2,_)).

costOf(state(_,_,Path),Cost):-
	length(Path,Cost).

