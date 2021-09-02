:-['Lab01_SEARCH_LIBRARY.pl'].
/** Author: Nikolaos Lintas from University of Sheffield */
:-dynamic worker/2.

%%% Initial job allocation problem.
%%% Worker Cost-Job table.
%%% Cost is 1 if worker is allocated the first job, 2 for the second etc.

worker(1,[4,1,3,5]).
worker(2,[6,3,5,2]).
worker(3,[8,4,5,7]).
worker(4,[3,7,8,9]).
worker(5,[7,1,5,6]).
worker(6,[8,4,7,9]).
worker(7,[5,6,7,4]).
worker(8,[2,6,8,3]).
worker(9,[1,3,9,6]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Generating random Problem Instances
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% random problem generator
%%% Creates a new problem instance of size N, and asserts corresponding 
%%% facts into Prolog memory. The number of jobs assigned to each worker is 
%%% N //2 (subset of the jobs he/she can do), where N is the number of jobs, 
%%% which is equal to the number of workers.       

generate_problem(N):-
	abolish(worker/2),
	numlist(1,N,Jobs),
	JobsNum is N // 2,
	create_workers(N,JobsNum,Jobs).

%%% create_workers/3
%%% create_workers(NumOfWorkers,NumofJobs,Jobs).
%%% Creates and asserts facts concering the workers. Numofoworkers
%%% is the number of workers (equal to the number of total jobs), Numofjobs is the 
%%% number of jobs assigned to each worker (half the number of total jobs), and Jobs
%%% is the list of all jobs. 

create_workers(0,_,_):-!.
create_workers(N,JobsNum,Jobs):-
	create_worker_list(JobsNum,Jobs,WList),
	write(worker(N,WList)),nl,
	asserta(worker(N,WList)),
	NN is N - 1, 
	rotate(Jobs,NewJobs),
	create_workers(NN,JobsNum,NewJobs).

%%% rotates the list to create a bit of diversity
rotate([X|Jobs],NJobs):-
	append(Jobs,[X],NJobs),
	!.

%%% create_worker_list/2
%%% create_worker_list(JobsNum,Jobs,WList)
%%% Creates a list of JobsNum jobs for a worker, which is a subset 
%%% of Jobs and unifies the result with WList.
create_worker_list(JobsNum,Jobs,WList):-
	append(WorkerJobs,_,Jobs),
	length(WorkerJobs,JobsNum),
	random_list(WorkerJobs,WList),!.

%%% random/2
%%% random(List,RandomLIST).
%%% randomizes the elements of the list List and 
%%% unifies the result with RandomLIST/
random_list([],[]):-!.
random_list(Jobs,[X|Rest]):-
	length(Jobs,N),
	Pos is random(N),
	nth0(Pos,Jobs,X),
	select(X,Jobs,Remaining),
	random_list(Remaining,Rest).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% State representation 




%%% initial_state/1

%%% final_state/1

%%% operator/2




%%% costOf/2
costOf(state(_,_,Cost),Cost).

%%% same_state/2
same_state(State,State).



