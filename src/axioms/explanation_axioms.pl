:-module(explanation_axioms,[explains_tel/4,
							explains_can/4,
							explains_noalt/4,
							explains_only_preferred/4]).
:-use_module('explanation_axioms_helpers.pl').
:-dynamic pre_depth/1.

%
% EXPLANATION AXIOMS
%

%
% Configuration
%
pre_depth(7).
max_depth(20).

/**    
 * explains_tel(-Y:Explanation,+X:Explanatum,+S:Situation,-J:Justification) is nondet
 * Predicate Y explains X if X has occured in S, and X operatinalizes Y (is a task that refines goal Y).
 * X must be a task and Y a goal.
 * Occurence of Y is S is not checked.
 * (Axiom 1)
 */
explains_tel(Y,X,S,J):- 
		has_occurred(X,S),operationalizes(X,Y),J = "operationalization (axiom 1)";
		not_occurred(X,S).
		
		

/** 
 * explains_tel(-Y:Explanation,+X:Explanatum,+S:Situation,-J:Justification) is nondet
 * (Axiom 2) Y explains X if X has occured in S, and X refines Y (is a goal that refines goal Y).
 * X, Y must both be goals.
 * Occurence of Y is S is not checked.
 */
explains_tel(Y,X,S,'refinement (axiom 2)'):- 
		has_occurred(X,S),refines(X,Y).


/** 
 * explains_tel(-Y:Explanation,+X:Explanatum,+S:Situation,-J:Justification) is nondet.
 * (Axiom 3) Y explains X if X has occured in S and X is the result of delegation from Y.
 * No checks on the type of X and Y are performed.
 * Occurence of Y is S is not checked.
 */
explains_tel(Y,X,S,'dependency (axiom 3)') :- has_occurred(X,S),dependency(_,_,Y,X), holds(hasDelegated(_,_,Y,X),S).


/** 
 * explains_tel(-Y:Explanation,+X:Explanatum,+S:Situation,-J:Justification) is nondet.
 * (Axiom 4) Y explains X if X has occured in S and X somehow enables Y.
 * Enablement depth to be set by a pre_depth(+N) assertion.
 * Run max_depth(-N), for maximum depth allowed.
 * No checks on the type of X and Y are performed.
 * Occurence of Y is S is not checked.
 */
explains_can(Y,X,S,'enablement (axiom 4)'):- has_occurred(X,S),pre_depth(N),enables_strongly_bound(X,Y,N,S).


/** 
 * explains_noalt(-Y,+X,+S) is nondet
 * (Axiom 5) X ia explained if it has no recursive OR-sibling (e.g. including OR-aunt) that is feasible in the prefix of S where X is about to occur.
 * Will always be true if X does not have an OR-sibling or OR-aunt
 * Occurence of Y is S is not checked.
 */
explains_noalt('...',X,S,'only feasible (axiom 5)'):- (\+ or_sibling(_,X,S),!);
						(is_prefix(X,S,Res),only_feasible_alt(X,Res)).



/** 
 * explains_only_preferred(-Y,+X,+S) is nondet
 * (Axiom 6) X ia explained if it has recursive OR-siblings (e.g. including OR-aunt) that that are feasible in the prefix of S where X is about to occur, BUT none of them is preferred.
 * Will always be true if X does not have an OR-sibling or OR-aunt
 * Occurence of Y is S is not checked.
 */
explains_only_preferred('...',X,S,'only preferred from feasible (axiom 6)'):-
						is_prefix(X,S,SPre),
						only_preferred_alt(X,SPre).
	

