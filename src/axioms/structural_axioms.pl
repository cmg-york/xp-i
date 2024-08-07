:-module(structural_axioms,[operationalizes/2,
							refines/2,
							ancestor/2,
							or_sibling/3,
							enables_strongly_bound/4]).
							
%
% STRUCTURAL AXIOMS
%


%
% Tasks and goals
%

operationalizes(X,Y):-parent(Y,X),task(X).
refines(X,Y):-parent(Y,X),goal(X).

%
% Siblings and anchestors
%

% Generalized parent. For or parenthood and or_siblings, dependency is treated as parenthood.
%! g_parent(?A,?D) A is a parent of D or the origin of a dependency towards D.
%! parent(D,A): A is the parent of D.
g_parent(A,D) :- parent(A,D);dependency(_,_,A,D).

% As above. Checks also for delegation action.
%! g_parent(?A,?D, +S) A is a parent of D or the origin of a dependency towards D. Dependency was activated in S.
g_parent(A,D,S) :- parent(A,D);(dependency(_,_,A,D),
								holds(hasDelegated(_,_,A,D),S)).


%! ancestor(?A,?D): A is an anchestor of D.
ancestor(A,D) :- g_parent(A,D).
ancestor(A,D) :- g_parent(Z,D),ancestor(A,Z).

%! ancestor(?A,?D,X,+S): A is an Nth degree anchestor of D. Validates that dependencies are activated in S.
ancestor(A,D,1,S) :- g_parent(A,D,S).
ancestor(A,D,N,S) :- g_parent(Z,D,S),ancestor(A,Z,X,S), N is X + 1.

%! or_ancestor(?A,?D): A is an ancestor of D, that is also an OR decomposition. 
or_ancestor(A,D) :- g_parent(A,D),or_node(A).
or_ancestor(A,D) :- g_parent(Z,D),or_ancestor(A,Z).

%! or_ancestor(?A,?D,S): A is an ancestor of D, that is also an OR decomposition. Validates that dependencies are activated in S.
or_ancestor(A,D,S) :- g_parent(A,D,S),or_node(A).
or_ancestor(A,D,S) :- g_parent(Z,D,S),or_ancestor(A,Z,S).


%! or_sibling(?A,?D): A is an OR-sibling or OR-aunt of D. The latter means that some parent of D has an OR-child that is not an ancestor of D.
or_sibling(A,D):- 
	or_ancestor(Z,D), g_parent(Z,A), 
	\+ ancestor(A,D), A \== D.

%! or_sibling(?A,?D,S): A is an OR-sibling/aunt of D. 
or_sibling(A,D,S):- 
	or_ancestor(Z,D,S), g_parent(Z,A,S), 
	\+ ancestor(A,D,_,S), A \== D.


%
% Enables axioms
%

% Bounded strong enablement
%! enables_stronlgy(Y,X,N): Y enables X within N steps in the graph (ancestry + the pre).
enables_strongly(Y,X,1,_) :- pre(Y,X).
enables_strongly(Y,X,N,S) :- ancestor(Z,Y,N1,S), pre(Z,X), N is N1 + 1.
enables_strongly_bound(Y,X,N,S):- enables_strongly(Y,X,M,S),M =< N.









% ######                                                                
% #     # ###### #####  #####  ######  ####    ##   ##### ###### #####  
% #     # #      #    # #    # #      #    #  #  #    #   #      #    # 
% #     # #####  #    # #    # #####  #      #    #   #   #####  #    # 
% #     # #      #####  #####  #      #      ######   #   #      #    # 
% #     # #      #      #   #  #      #    # #    #   #   #      #    # 
% ######  ###### #      #    # ######  ####  #    #   #   ###### #####  


%! enables_bound(Y,X,N): Y enables X within N steps in the graph (ancestry + the pre).
%enables_strongly(Y,X,1,_,_) :- pre(Y,X).
%enables_strongly(Y,X,N,S,F) :- ancestor(Z,Y,N1),enables_strongly(Z,X,N2,S,F),N is N1+N2.
%enables_strongly(Y,X,N,S,F) :- ancestor(Z,Y,N1), pre(Z,X), N is N1 + 1.
%enables_strongly(Y,X,N,S,F) :- F \= bakaliki, holds(hasDelegated(_,_,Z,Y),S),!,enables_strongly(Z,X,N2,S,bakaliki),N is 1 + N2.
%enables_strongly_bound(Y,X,N,S):- enables_strongly(Y,X,M,S,foo_flag),M =< N.


 

%! sibling(?S1,?S2): S1 is a (direct) sibling of S2, for an OR-, AND-decomposition or delegation.
%! sibling(?S1,?S2,+Sit): S1 is a (direct) sibling of S2. 
sibling(S1,S2):-g_parent(Y,S1),g_parent(Y,S2),S1 \= S2. 
sibling(S1,S2,Sit):-g_parent(Y,S1,Sit),g_parent(Y,S2,Sit),S1 \= S2. 


%
%! ancestor(A,D, N): A is an Nth degree anchestor of (parent: N=1, grantparent: N=2, etc.). 
%ancestor(A,D,1) :- parent(A,D).
%ancestor(A,D,N) :- parent(Z,D),ancestor(A,Z,X), N is X + 1.

ancestor(A,D,1) :- g_parent(A,D).
ancestor(A,D,N) :- g_parent(Z,D),ancestor(A,Z,X), N is X + 1.

%
%  Weaker enablement (NOT USED)
%

%! enables(Y,X): Y enables X.
%! pre(Y,X): Y precedes X.
enables_(Y,X) :- pre(Y,X).
enables_(Y,X) :- ancestor(W,X),enables(Y,W).
enables_(Y,X) :- ancestor(Z,Y),enables(Z,X).


% Bounded enablement
%! enables_bound(Y,X,N): Y enables X within N steps in the graph (ancestry + the pre).
enables_(Y,X,1) :- pre(Y,X).
enables_(Y,X,N) :- ancestor(W,X,N1),enables(Y,W,N2),N is N1+N2.
enables_(Y,X,N) :- ancestor(Z,Y,N1),enables(Z,X,N2),N is N1+N2.
enables_bound(Y,X,N):- enables(Y,X,M),M =< N.



% Bounded strong enablement
%! enables_bound(Y,X,N): Y enables X within N steps in the graph (ancestry + the pre). Situation independent.
enables_strongly(Y,X,1) :- pre(Y,X).
enables_strongly(Y,X,N) :- ancestor(Z,Y,N1),enables_strongly(Z,X,N2),N is N1+N2.
enables_strongly_bound(Y,X,N):- enables_strongly(Y,X,M),M =< N.



