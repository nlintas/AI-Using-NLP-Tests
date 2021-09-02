%%% Simple equations example
/** Author: Nikolaos Lintas from University of Sheffield */

:-use_module(library(bounds)).
	
solve_eq([X,Y,Z]):-
	[X,Y,Z] in 1..6,
	X + Y #= 2*Z,
	X #> 3,
	X + Z #> 5,
	all_different([X,Y,Z]),
	labeling([],[X,Y,Z]).