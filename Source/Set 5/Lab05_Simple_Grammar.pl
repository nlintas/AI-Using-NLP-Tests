:-[lab05_lexical_analyser].
/** Author: Nikolaos Lintas from University of Sheffield */

parse:-
	lexical_analysis(S),
	append(X,['.'],S),
	write('The list of tokens for the syntax check (parsing) is:'),nl, write(X),nl, nl,
	sentence(X,[]),!,
	write('The sentence is parsed correctly!!'),nl.
parse:-
	write('Syntax Error: The sentence failed to comply with grammar!!'),nl.


sentence --> noun_phrase, verb_phrase.

noun_phrase-->article,noun.

verb_phrase-->verb,noun_phrase.

noun-->[cat].
noun-->[dog].
noun-->[man].
noun-->[woman].
noun-->[monkey].
noun-->[banana].

verb-->[chases].
verb-->[climbs].
verb-->[eats].
verb-->[kicks].

article-->[a].
article-->[the].
