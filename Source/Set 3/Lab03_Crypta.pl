/** Author: Nikolaos Lintas from University of Sheffield */
/* Solve the operation:                                                  */
/*                                                                       */
/*      S E N D                                                          */
/*  +   M O R E                                                          */
/*  --------------                                                       */
/*  = M O N E Y                                                          */
/*                                                                       */
/*  Solution:                                                             */
/*  [S,E,N,D,M,O,R,Y]                                                    */
/*  [9,5,6,7,1,0,8,2]                                                    */
/*-----------------------------------------------------------------------*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TO RUN:
%%% run(prolog). For Simple Prolog Solution
%%% run(normal). For CLP normal mode
%%% run(ff).     For CLP Fail-First heuristic
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:-use_module(library(bounds)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
run(Mode):-
    format(' ** SEND+MORE=MONEY Puzzle ~w labeling :: ~n ',[Mode]),
	statistics(cputime,Start),
    sendmore(LD,Mode),nl,
	statistics(cputime,End),
    write(' SOLUTION ::: '),write(LD),nl,
	TIME is End - Start,
	write(TIME),write(' secs').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Constraint Version
%%% The constraint version Mode can have two labelings (ff & normal)

sendmore(LD,Mode):-
	LD=[S,E,N,D,M,O,R,Y],
	LD in 0..9,
	%%% ADD YOUR CONSTRAINTS HERE
	% cannot be zero because it would not be a 4-digit number
	S #\= 0,
	M #\= 0,
	(S+M)*1000 + (E+O)*100 + (N+R)*10 + D + E #= M*10000 + O*1000 + N*100 + E*10 + Y,
	all_different([S,E,N,D,M,O,R,Y]),
	lab(Mode,LD).

lab(normal,L):-  
	labeling([],L).

lab(ff,L):-
	labeling([ff],L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Simple Prolog Solution 
%%% This is invoked by using "prolog" in the Mode argument. 

sendmore(LD,prolog):-
	LD=[S,E,N,D,M,O,R,Y],
	!,
	numlist(0,9,Nums),
	assign_doms(LD,Nums),
	S \= 0,
	M \= 0,
	is_set(LD), %% only unique elements
	          1000*S + 100*E + 10*N + D + 
	          1000*M + 100*O + 10*R + E =:= 
	10000*M + 1000*O + 100*N + 10*E + Y.
	
%%% Generating a solution to the problem.

assign_doms([],_).
assign_doms([X|Rest],List):-
	select(X,List,NewList),
	assign_doms(Rest,NewList).