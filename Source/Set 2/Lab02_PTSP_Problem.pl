:-['Lab02_SEARCH_LIBRARY.pl'].
/** Author: Nikolaos Lintas from University of Sheffield */

%%%% TSP Problem
:-dynamic city_distance/3. 

%%% An initial Problem instance with 8 cities.
city_distance(1, 2, 95).
city_distance(1, 3, 46).
city_distance(1, 4, 59).
city_distance(1, 5, 91).
city_distance(1, 6, 71).
city_distance(1, 7, 61).
city_distance(1, 8, 20).
city_distance(2, 3, 57).
city_distance(2, 4, 76).
city_distance(2, 5, 75).
city_distance(2, 6, 21).
city_distance(2, 7, 86).
city_distance(2, 8, 82).
city_distance(3, 4, 67).
city_distance(3, 5, 67).
city_distance(3, 6, 30).
city_distance(3, 7, 52).
city_distance(3, 8, 25).
city_distance(4, 5, 41).
city_distance(4, 6, 15).
city_distance(4, 7, 61).
city_distance(4, 8, 70).
city_distance(5, 6, 46).
city_distance(5, 7, 25).
city_distance(5, 8, 3).
city_distance(6, 7, 31).
city_distance(6, 8, 15).
city_distance(7, 8, 55).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Distance predicate
%%% Use these for the implementation of your operator.

city_dis(Next,Current,ArcCost):-
	city_distance(Next,Current,ArcCost).

city_dis(Next,Current,ArcCost):-
	city_distance(Current,Next,ArcCost).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Problem generator for random fully connected TSP problems
%%% of varying size. 
%%% generate_problem(NumberOfCities)
generate_problem(Cities):-	
	abolish(city_distance/3),
	findall(X,between(1,Cities,X),CList),
	generate_distance_facts(CList).
	
generate_distance_facts([_]).
generate_distance_facts([City|Rest]):-
	forall(member(C,Rest),(
			Dis is random(100),
			assertz(city_distance(City,C,Dis))
		)),
	generate_distance_facts(Rest).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
%%% state description
%%% state(CitiesLeft,Path,Cost)
%%% initial_state(State)
initial_state(state(CitiesLeft,[1],0)):-
	setof(X,Y^D^city_distance(X,Y,D),[1|CitiesLeft]).

%%% final_state(State)
final_state(state([],_,_)).

%%% operator(State,Next)
operator( state(Cleft,[Current|Path],Cost), state(NewCleft,[Next,Current|Path],NewCost) ):-
	select(Next,Cleft,NewCleft),
	city_dis(Next,Current,ArcCost),
	NewCost is Cost + ArcCost.

%%% Returns the cost of a state.
%%% costOf(State,Cost)	
costOf(state(_,_,Cost),Cost).

%%% Two states are identical.
same_state(S,S).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Heuristic Functions 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% heuristic_Func for bestfs


heuristic_Func(state(_,[_],Cost),Cost,_AnyHeuristicAlgorithm,distance).

heuristic_Func(state(_,[X,Y|_Path],_Cost),HV,bestf,distance):-
	city_dis(X,Y,HV).

%%% heuristic_Func for astar

heuristic_Func(state(_,[X,Y|_Path],Cost),HV,astar,distance):-
	city_dis(X,Y,D),
	HV is D+Cost.

