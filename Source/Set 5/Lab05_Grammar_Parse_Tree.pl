:-[lab05_lexical_analyser].
/** Author: Nikolaos Lintas from University of Sheffield */

parse_tree:-
	lexical_analysis(S),
	append(X,['.'],S),
	write('The list of tokens for the syntax check (parsing) is:'),nl, write(X),nl, nl,
	sentence(P,X,[]),!,
	write('The sentence is parsed correctly!!'),nl,
	write('The Parse Tree is:'),nl, write(P), nl, nl.

parse_tree:-
	write('Syntax Error: The sentence failed to comply with grammar!!'),nl.

sentence(s(NP,VP)) --> noun_phrase(NP), verb_phrase(VP).
noun_phrase( np( A, N ) ) --> article( A ), noun( N ).
verb_phrase( vp( V, NP ) ) --> verb( V ), noun_phrase( NP ).

noun(n(cat))-->[cat].
noun(n(dog))-->[dog].
noun(n(man))-->[man].
noun(n(woman))-->[woman].
noun(n(monkey))-->[monkey].
noun(n(banana))-->[banana].

verb(v(chases))-->[chases].
verb(v(climbs))-->[climbs].
verb(v(eats))-->[eats].
verb(v(kicks))-->[kicks].

article(art(a))-->[a].
article(art(the))-->[the].
