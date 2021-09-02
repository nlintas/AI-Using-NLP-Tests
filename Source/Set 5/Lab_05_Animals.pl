:-['Lab05_ID3_ALGORITHM.pl'].
/** Author: Nikolaos Lintas from University of Sheffield */

/*-------------------------------------------------------------------------------
  Example for applying ID3 in order to produce if-then rules.

  example/3:
  - number id for sample
  - outcome (the "then" part of the resulting if-then rule)
  - [attr1=val1, attr2=val2, ...] all attributes and their corresponding values
---------------------------------------------------------------------------------*/

example(1, mammal,   [has_covering=hair,     milk=t, homeothermic=t, habitat=land, eggs=f, gills=f]).
example(2, mammal,   [has_covering=none,     milk=t, homeothermic=t, habitat=sea,  eggs=f, gills=f]).
example(3, mammal,   [has_covering=hair,     milk=t, homeothermic=t, habitat=sea,  eggs=t, gills=f]).
example(4, mammal,   [has_covering=hair,     milk=t, homeothermic=t, habitat=air,  eggs=f, gills=f]).
example(5, fish,     [has_covering=scales,   milk=f, homeothermic=f, habitat=sea,  eggs=t, gills=t]).
example(6, reptile,  [has_covering=scales,   milk=f, homeothermic=f, habitat=land, eggs=t, gills=f]).
example(7, reptile,  [has_covering=scales,   milk=f, homeothermic=f, habitat=sea,  eggs=t, gills=f]).
example(8, bird,     [has_covering=feathers, milk=f, homeothermic=t, habitat=air,  eggs=t, gills=f]).
example(9, bird,     [has_covering=feathers, milk=f, homeothermic=t, habitat=land, eggs=t, gills=f]).
example(10,amphibian,[has_covering=none,     milk=f, homeothermic=f, habitat=land, eggs=t, gills=f]).

/*-------------------------------------------------------------------------------
  son/2:
  - the value (one by one)
  - ?
---------------------------------------------------------------------------------*/

son(hair,?).
son(none,?).
son(scales,?).
son(feathers,?).

son(land,?).
son(sea,?).
son(air,?).

son(t,?).
son(f,?).

