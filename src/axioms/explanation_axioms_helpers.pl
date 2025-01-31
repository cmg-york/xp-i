:-module(explanation_axioms_helpers,[has_occurred/2,
							not_occurred/2,
							is_prefix/3,
							only_preferred_alt/2,
							only_feasible_alt/2,
							write_list/3,
							term_to_string/2
							]).
:-dynamic pre_depth/1.

%
% EXPLANATION AXIOMS - HELPERS
%



/** 
 * has_occurred(+X,+S) is det
 * X is a fluent or action, S is a situation. 
 * True if X holds for any prefix of the situation, or is any action of the situation. 
 *
 */
has_occurred(X,s0) :- \+ task(X),
						get_fluent(X,Xf),
						holds(Xf,s0),!.
has_occurred(X,do(A,S)) :- (task(X),X = A; \+task(X),get_fluent(X,Xf),holds(Xf,do(A,S)); has_occurred(X,S)),!.


/** 
 * not_occurred(+X,+S) is det
 * X is a fluent or action, S a situation
 * Holds if X is not observed in the situation, prints a message and fails. 
 *
 */
not_occurred(X,S):- 
				\+ has_occurred(X,S),term_string(X,Xs),
				string_concat(Xs,' did not occur in episode',J),
				ansi_format([bold,fg(red)], '~w.', J).

/** 
 * is_prefix(+X,+S,-Sp) is det
 * X is an action or fluent, S, Sp are situations.
 * Returns in Sp a prefix do(A,S') of S from s0, in which X has not been observed (holds or occurs) up to S' and is observed at do(A,S'). True if X holds at s0. Fails if no prefix is found.
 */
is_prefix(X,s0,s0):- \+ task(X),
						get_fluent(X,Xf),
						holds(Xf,s0),!.
is_prefix(X,do(A,S),Sp):- is_prefix(X,S,Sp),! ;
							((task(X), A = X ; (\+ task(X),
												   get_fluent(X,Xf),
												   holds(Xf,do(A,S)),
												   \+holds(Xf,S))),Sp = S),!.





%
% P R E F E R E N C E 
%


/** 
 * only_preferred_alt(+Explanatum,+Situation)
 * Explanatum has alternatives but it is the only preferred one.
 */
only_preferred_alt(X,S):- 
		bagof(Y,or_sibling(Y,X,S),R), %Get the siblings
		get_feasible(R,S,Feasible),
		Feasible \= [],
		only_preferred(X,[X|Feasible]).

/** 
 * only_preferred(+X,+L) 
 * true if X is the only preferred among elements in L. For use by only_preferred_alt(+Explanatum,+Situation)
 */
only_preferred(X,L) :- bagof(Y,preferred(Y),Preferred),
					intersection(L,Preferred,Inter),
					length(Inter,1),
					memberchk(X,Inter).
					


%! 
/** 
 * preferred(?X) element X is preferred if it contributes positivelly to a soft-goal that is promoted 
 * and does not contribute negativelly to a goal that is not promoted.
 *
 */
preferred(X) :- contr(X,Q1,plus),
				promote(Q1),
				\+ (contr(X,Q2,minus),
				promote(Q2)).
%


%
% F E A S I B I L I T Y
%

/** 
 * only_feasible_alt(+X,+Situation) true if none of the siblings of X are feasible in Situation.
 * Collects all or_siblings and check if all of them are infeasible.
 */
only_feasible_alt(X,S):- bagof(Y,or_sibling(Y,X,S),R),none_feasible(R,S).

/** 
 * get_feasible(+List,+Situation,-Feasible) Feasible is a sublist of List with items that are 
 * feasible in Situation.
 *
 */
get_feasible([],_,[]):-!.
get_feasible([H|T],S,[H|R]):- feas(H,S),get_feasible(T,S,R),!.
get_feasible([H|T],S,R):- \+ feas(H,S),get_feasible(T,S,R),!.


/** 
 * none_feasible(+List,+Situation) true if there is no action in the List that is feasible in Situation.
 *
 */
none_feasible([],_).
none_feasible([H|T],S):- \+ feas(H,S),none_feasible(T,S).



%
%   M I S C 
%


/** 
 * write_list(N,L1,L2) is det.
 * Prints the elements of L1 and L2 one each line in the format "n: l1 - l2", where n is the position in the list starting from N, and l1 and l2 are the elements of L1 and L2 with the corresponding position.
 * Assumes the lenghts of the list are equal.
 */
write_list(_,[],[]).
write_list(N,[H1|T1],[H2|T2]):- write(N),write(': '),write(H1), write(' - '), write(H2),
								nl,M is N+1, write_list(M,T1,T2).



/** 
 * term_to_string(+Input, -Output) is det.
 * Converts Input to string if it is a term, leaves it as is otherwise.
 */
term_to_string(Input, Input) :- string(Input),!.
term_to_string(Input, Output) :- \+ string(Input), term_string(Input, Output).






%
% D E P R E C I A T E D
%


%
% Getting a situation prefix - it is only for actions...
%

list_to_sit([],s0).
list_to_sit([H|T],do(H,R)) :- list_to_sit(T,R).

sit_to_list(s0,[]).
sit_to_list(do(A,S),[A|R]) :- sit_to_list(S,R).

get_pref_list(Ac,Sl,Pl):- nth0(N,Sl,Ac),!,prefix(Pl,Sl),length(Pl,N),!. 


%! get_sit_prefix(+A,+S,-Res): returns the prefix of siutation S, up to and EXCLUDING action A.

get_sit_prefix(_,s0,s0):-!.
get_sit_prefix(A,S,Res):-sit_to_list(S,L0),reverse(L0,L),
						get_pref_list(A,L,ResList),reverse(ResList,RL),
						list_to_sit(RL,Res).



%! any_feasible(+List,+S) true if there is an action in the List that is feasible in S.
any_feasible([],_):-fail.
any_feasible([H|T],S):- feas(H,S);any_feasible(T,S).

