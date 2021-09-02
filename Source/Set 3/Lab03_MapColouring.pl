/** Author: Nikolaos Lintas from University of Sheffield */
/*--------------------------------------------------------------
  Map Colouring
  --------------------------------------------------------------*/
/*--------------------------------------------------------------
map/1:(-/+)
1st argument: The solution of the map colouring problem
--------------------------------------------------------------*/

map(Map):-
	colours(C),
	template_map(Map),
	solve_map(Map,C).

template_map([(a,_),(b,_),(c,_),(d,_),(e,_),(f,_),(g,_),(h,_)]).

colours([red, green, blue, white, yellow, orange]).

/*--------------------------------------------------------------
            The list of Neighbouring Countries
--------------------------------------------------------------*/
next(a,b).
next(b,h).
next(b,d).
next(a,c).
next(a,d).
next(h,d).
next(d,g).
next(c,e).
next(e,g).
next(e,f).
next(f,g).
next(d,c).
next(d,e).
next(g,h).

neighbourgh(X,Y):-next(X,Y),!.
neighbourgh(X,Y):-next(Y,X),!.

/*--------------------------------------------------------------
     Pick up any of the available colours and check constraint
--------------------------------------------------------------*/

solve_map([],_).
solve_map([(Country,Colour)|Rest],AllColours):-
	solve_map(Rest,AllColours),
	member(Colour,AllColours),
	not(conflict((Country,Colour),Rest)).

/*--------------------------------------------------------------
      Constraint: Neighbouring countries have the same colour
--------------------------------------------------------------*/

% There is a conflict if a neighboughing country has the same colour

conflict((Country1,Colour),[(Country2,Colour)|_]):-
	neighbourgh(Country1,Country2),!.

% There is a conflict if there is a conflict in the rest of the list

conflict((Country,Colour),[_|Rest]):-
	conflict((Country,Colour),Rest).

