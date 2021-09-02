/*------------------------------------------------------*/
/*  Induction of Decision Trees                         */
/*------------------------------------------------------*/
/*  Author: (C) 1994 Zdravko Markov                     */
/*  Updated: Sping-2005                                 */
/*  Editor: Nikolaos Lintas from University of Sheffield*/
/*------------------------------------------------------*/
/* Example format: example(ID, Class, [A=V,...]).       */
/* Use:                                                 */
/*   Create a decision tree: ?- id3.                    */
/*   Print tree:             ?- showtree.               */
/*   Print rules:            ?- showrules.              */
/*------------------------------------------------------*/   
/* Additional procedures for experimenting:             */
/*   ?- entropy(List_of_example_IDs, Entropy).          */
/*   ?- distr(List_of_example_IDs, [C1/N1, C2/N2,...]). */
/*   (where Ni is the number of occurences of Ci)       */
/*   Get the set of attributes:          ?- attr(L).    */
/*   Get the set of values for attr. A:  ?- vals(A,L).  */ 
/*------------------------------------------------------*/

/*------------------------------------------------------*/
?-  op(100,fx,if).
?-  op(99,xfy,then).
/*------------------------------------------------------*/

id3 :- 
	id3(1),  % for compatibility with old versions
	write('The decision tree is:'),nl,
	showtree, nl,
	write('The decision IF-THEN ruels are:'),nl,
	showrules,nl,!.

id3(Tr) :-
    retractall(node(_,_,_)),
    retractall(if _ then _),
    findall(N,example(N,_,_),E),
    example(_,_,L), !,
    get_attributes(L,A),
    idt(E,root,A,Tr),
    assert_rules, !.

idt(E,Parent,_,Tr) :-
    length(E,Len),
    Len=<Tr,
    distr(E, Distr),
    assertz(node(leaf,Distr,Parent)), !.
idt(E,Parent,_,_) :-
    distr(E, [C]),
    assertz(node(leaf,[C],Parent)).
idt(Es,Parent,As,Tr) :- 
    choose_attribute(Es,As,A,Values,Rest), !,
    partition(Values,A,Es,Parent,Rest,Tr).
idt(E,Parent,_,_) :- !,
    node(Parent,Test,_),
    write('Inconsistent data: cannot split '), write(E), write(' at node '), writeln(Test).

get_attributes([],[]) :- !.
get_attributes([A=_|T],[A|W]) :-
    get_attributes(T,W).

partition([],_,_,_,_,_) :- !.
partition([V|Vs],A,Es,Parent,Rest,Tr) :-
    get_subset(Es,A=V,Ei), !,
    gen_name(Node), 
    assertz(node(Node,A=V,Parent)),
    idt(Ei,Node,Rest,Tr), !,
    partition(Vs,A,Es,Parent,Rest,Tr).

choose_attribute(Es,As,A,Values,Rest) :-
    length(Es,LenEs),
    information_content(Es,LenEs,I), !,
    findall((A-Values)/Gain, 
            (member(A,As),
             get_values(Es,A,[],Values),
             split_into_subsets(Values,Es,A,Ess),
             residual_information(Ess,LenEs,R),
             Gain is I - R),
            All),
    maximum(All,(A-Values)/_),
    efface(A,As,Rest), !.

split_into_subsets([],_,_,[]) :- !.
split_into_subsets([V|Vs],Es,A,[Ei|Rest]) :-
    get_subset(Es,A=V,Ei), !,
    split_into_subsets(Vs,Es,A,Rest).

residual_information([],_,0) :- !.
residual_information([Ei|Es],Len,Res) :-
    length(Ei,LenEi),
    information_content(Ei,LenEi,I), !,
    residual_information(Es,Len,R),
    Res is R + I*LenEi/Len.

information_content(Es,Len,I) :-
    setof(C,E^L^(member(E,Es),example(E,C,L)),Classes), !,
    sum_terms(Classes,Es,Len,I).

sum_terms([],_,_,0) :- !.
sum_terms([C|Cs],Es,Len,Info) :-
    findall(E,(member(E,Es),example(E,C,_)),InC),
    length(InC,N),
    sum_terms(Cs,Es,Len,I),
    Info is I - (N/Len)*(log(N/Len)/log(2)).

get_values([],_,Values,Values) :- !.
get_values([E|Es],A,Vs,Values) :-
    example(E,_,L),
    member(A=V,L), !,
    (member(V,Vs), !, get_values(Es,A,Vs,Values);
     get_values(Es,A,[V|Vs],Values)
    ).

get_subset([],_,[]) :- !.
get_subset([E|Es],A,[E|W]) :-
    example(E,_,L),
    member(A,L), !,
    get_subset(Es,A,W).
get_subset([_|Es],A,W) :-
    get_subset(Es,A,W).

assert_rules :-
    path(root,Path,Conclusion),
    assertz(if Path then Conclusion),
    fail.
assert_rules.

path(Parent,[],Class) :-
    node(leaf,Class,Parent), !.
path(Parent,[A|Path],Leaf) :-
    node(Son,A,Parent),
    path(Son,Path,Leaf).

distr(S,Dist) :-
    setof(C,X^L^(member(X,S),example(X,C,L)),Cs),
    countc(Cs,S,Dist).

countc([],_,[]) :- !.
countc([C|L],E,[C/N|T]) :-
    findall(X,(member(X,E),example(X,C,_)),W),
    length(W,N), !,
    countc(L,E,T).

/*--------------------- Show tree ---------------------------*/
showrules :-
    listing(if).

/*--------------------- Show tree ---------------------------*/
showtree :-
    showtree(root,0).

showtree(Parent,_) :- 
    node(leaf,Class,Parent), !,
    write(' => '),write(Class).
showtree(Parent,Pos) :-
    findall(Son,node(Son,_,Parent),L),
    Pos1 is Pos+2,
    show_list(L,Pos1).

show_list([],_) :- !.
show_list([N|T],Pos) :-
    node(N,Label,_),
    nl, tab(Pos), write(Label),
    showtree(N,Pos),
    show_list(T,Pos).

/*------------------- Auxiliary --------------------------*/
gen_name(M) :-
   retract(nam(N)),
   M is N+1,
   assert(nam(M)), !.
gen_name(1) :-
   assert(nam(1)).

efface(X,[X|T],T) :- !.
efface(X,[Y|T],[Y|Z]) :-
   efface(X,T,Z).

subset([],_) :- !.
subset([X|T],L) :-
   member(X,L), !,
   subset(T,L).

maximum([X],X) :- !.
maximum([X/M|T],Y/N) :-
    maximum(T,Z/K),
    (M>K,Y/N=X/M ; Y/N=Z/K), !.

/*------------------- For experimenting------------------------*/
model(C,M) :-
    atom(C), C\=[], !,
    setof(N,H^L^W^U^(if H then [C/U],example(N,W,L),covers(H,L)),M).
model(H,M) :-
    findall(N,(example(N,_,L),covers(H,L)),M).

covers(H1,H2) :-
     subset(H1,H2).

sem_covers(H1,H2) :-
    model(H1,M1),
    model(H2,M2),
    subset(M2,M1).

entropy(E,I) :-
    length(E,N),
    information_content(E,N,I).

attr(As) :-
    setof(A,X^C^L^V^(example(X,C,L),member(A=V,L)),As).

vals(A,AVs) :-
    setof(A=V,X^C^L^(example(X,C,L),member(A=V,L)),AVs).

/*------------------- End id3.pl --------------------------*/