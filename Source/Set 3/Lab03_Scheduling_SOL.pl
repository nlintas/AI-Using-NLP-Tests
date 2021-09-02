/** Author: Nikolaos Lintas from University of Sheffield */
:-use_module(library(bounds)).

%%% A Simple Scheduling problem
%%% Jobs definitions
jobs([job(1,12),job(2,10),job(3,8),
	 job(4,14),job(5,20),job(6,9),job(7,8),job(8,3),job(9,7)]).
%%% Ordering Constraints
before([(1,3),(5,7),(8,7),(2,4),(9,1)]).
	 
%%% Find the maximum possible start time
%%% of each task.
max_start_time(MAX):-
	jobs(JOBS),
	findall(D,member(job(_,D),JOBS),DURATIONS),
	sumlist(DURATIONS,MAX).

solve(STARTS):-
	jobs(JOBS),
	max_start_time(MAX),
	length(JOBS,L),
	length(STARTS,L),
	STARTS in 0..MAX,
	all_different(STARTS),
	findall(D,member(job(_,D),JOBS),DURATIONS),
	serialized(STARTS,DURATIONS),
	before(CONS),
	state_cons(CONS,STARTS),
	labeling([ff],STARTS).

%%% Stating constraints
state_cons([],_).
state_cons([(P,N)|Rest],STARTS):-
	nth1(P,STARTS,PSTART),
	nth1(N,STARTS,NSTART),
	PSTART #< NSTART,
	state_cons(Rest,STARTS).