:-['Lab04_FRAMES_LIBRARY.pl'].
/** Author: Nikolaos Lintas from University of Sheffield */

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% frame(name(<Frame_Name>),
%%%       isa([<parent_frames_in_hierarchy>]),
%%%       ako([<parent_frames_in_hierarchy>]),
%%%       [<slots>]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* 
All animals are either birds or mammals and in principle they cannot fly. Birds have 2 legs and can fly, except penguins which cannot fly. Mammals have 4 legs, except bats which have 2 legs and can fly. Cats are mammals and live up to 5 years. Garfield is a 4 year old cat. Happyfeet is a penguin. Garfield and Happyfeet are friends. Happyfeet likes Duffy Duck, which is a cartoon that resembles a bird which talks, something which normally other birds, except parrots, cannot do.
*/

frame(name(animal),
	isa([]),
	ako([]),
	[fly(no)]).

frame(name(mammal),
    isa([animal]),
    ako([]),
    [fly(no)]).


% PUT THE REST OF THE REPRESENTATION HERE