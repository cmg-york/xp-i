:-use_module("../axioms/structural_axioms.pl").
:-consult("../axioms/explanation_axioms_helpers.pl").
:-use_module("../axioms/explanation_axioms.pl").
:-consult("axioms_test_fixture.pl").
:-consult("../golog/golog.pl").
:-dynamic(feas/2).

alltests:-ancestry,
			%and_ancestry,
			strong_enablement,
			enablement,
			is_prefix_tests, % pass
			all_preference_tests,!, % pass
			only_feasible_alt_tests,!, % pass
			all_feasibility_tests,!, % Pass
			get_sit_prefix_tests,!. %pass

%
% Plain Ancestry
%

anc1:-bagof(X, ancestor(X,t16),[g15,g11,g5,g1]).
anc2:-bagof(X, ancestor(X,t13),[g11,g5,g1]).
anc3:-bagof(X, ancestor(X,t11),Cs),(Cs == [g10,g4,g1]).
anc4:-bagof(X, ancestor(X,t7),Cs),(Cs == [g8,g3,g1]).
anc5:-bagof(X, ancestor(X,t1),Cs),(Cs == [g2,g1]),!.
anc5:-print("anc5 fails"),fail.

ancestry:-anc1,anc2,anc3,anc4,anc5.



%
% AND Ancestry - DEPRECATED
%


andanc(ts1):-bagof(X, and_ancestor(X,t5),_),!.
andanc(ts1):-!.
andanc(ts2):-bagof(X, and_ancestor(X,t7),Cs),(Cs == [g8,g3,g1]),!.
andanc(ts3):-bagof(X, and_ancestor(X,t18),Cs),(Cs == [g13]),!.
andanc(ts4):-bagof(X, and_ancestor(X,t12),Cs),(Cs == [g10,g4,g1]),!.
andanc(ts5):- bagof(X, and_ancestor(X,t13),_),!.
andanc(ts5):-!.
andanc(ts6):-bagof(X, and_ancestor(X,g10),Cs),(Cs == [g4,g1]),!.
andanc(ts7):-bagof(X, and_ancestor(X,g4),Cs),(Cs == [g1]),!.
andanc(ts8):-bagof(X, and_ancestor(X,g8),Cs),(Cs == [g3,g1]),!.
andanc(ts9):-bagof(X, and_ancestor(X,t15),Cs),(Cs == [g12]),!.

andanc(X):-write("Fails: "),print(X),fail.

and_ancestry:-andanc(ts1),andanc(ts2),andanc(ts3),
				andanc(ts4),andanc(ts5),andanc(ts6),
				andanc(ts7),andanc(ts8),andanc(ts9).
				
%
% Bounded Enablement
%			
%enab(ts1):-setof(X, enables_bound(t1,X,1),_),!.
%enab(ts1):-!.
%enab(ts2):-setof(X, enables_bound(t2,X,2),C), (C == [g3,g4]),!.
%enab(ts3):-setof(X, enables_bound(t2,X,3),C), (C == [g10, g3, g4, g7, g8, g9, t3, t4]),!.


%enablement:- enab(ts1),enab(ts2),enab(ts3).
enablement.

%
% Bounded Strong Enablement
%			
senab(ts1):-setof(X, enables_strongly_bound(t1,X,1,_),_),!.
senab(ts1):-!.
senab(ts2):-setof(X, enables_strongly_bound(t2,X,2,_),C), (C == [g3,g4]),!.
senab(ts3):-setof(X, enables_strongly_bound(t2,X,3,_),C), (C == [g3, g4]),!.
senab(ts4):-setof(X, enables_strongly_bound(t2,X,6,_),C), (C == [g3, g4]),!.
senab(ts5):-setof(X, enables_strongly_bound(t17,X,1,_),C), (C == [t6]),!.
senab(ts6):-setof(X, enables_strongly_bound(t17,X,2,_),C), (C == [t6,t8]),!.
senab(ts7):-setof(X, enables_strongly_bound(t17,X,3,_),C), (C == [g8,t6,t8]),!.
senab(ts8):-setof(X, enables_strongly_bound(t17,X,4,_),C), (C == [g5,g8,t6,t8]),!.
senab(ts9):-setof(X, enables_strongly_bound(t17,X,5,_),C), (C == [g5,g8,t6,t8]),!.


strong_enablement:- senab(ts1),senab(ts2),senab(ts3),senab(ts4),senab(ts5),senab(ts6),senab(ts7),senab(ts8),senab(ts9).


%
% Siblings
%

or_sibling_tests:-
	findall(X,or_sibling(X,t21),[t19,t20,g11]),
	findall(X,or_sibling(X,t20),[t19,t21,g11]),
	findall(X,or_sibling(X,t16),[t14,t13,t22,g12]),
	findall(X,or_sibling(X,t14),[t16,t13,t22,g12]),
	findall(X,or_sibling(X,t12),[]).


or_ancestor_tests :- 
bagof(A,or_ancestor(A,t13),[g11,g5]),
bagof(A,or_ancestor(A,g11),[g5]),
bagof(A,or_ancestor(A,t15),[g5]),
bagof(A,or_ancestor(A,t17),[g9]),
bagof(A,or_ancestor(A,t6),[g7]),
bagof(A,or_ancestor(A,t2),[g2]),
bagof(A,or_ancestor(A,t21),[g14,g5]),
bagof(A,or_ancestor(A,t16),[g15,g11,g5]),
findall(A,or_ancestor(A,t12),[]).




%
% Preference tests
%


all_preference_tests :- only_preferred,
				preferred,
				explains_only_preferred_tests,!.



explains_only_preferred_tests:- %case1_teardown,case2_teardown,case3_teardown,
	\+ explains_only_preferred(_,t14,do(t14,s0),_),
	case1_setup, explains_only_preferred(_,t14,do(t14,s0),_), case1_teardown,
	case2_setup,explains_only_preferred(_,t14,do(t14,s0),_),case2_teardown,
	case3_setup,\+ explains_only_preferred(_,t14,do(t14,s0),_),case3_teardown,!.
explains_only_preferred_tests:- case1_teardown,case2_teardown,case3_teardown,fail.

case1_setup :- assertz(feas(t16,s0)).
case1_teardown :- retract(feas(t16,s0)).

case2_setup :- assertz(feas(t16,s0)),assertz(feas(t13,s0)).
case2_teardown :- retract(feas(t16,s0)),retract(feas(t13,s0)).

case3_setup :- assertz(feas(g12,s0)),assertz(feas(t16,s0)).
case3_teardown :- retract(feas(g12,s0)),retract(feas(t16,s0)).

only_preferred :- 	explanation_axioms_helpers:only_preferred(a,[a,b,c]),
					explanation_axioms_helpers:only_preferred(a,[a]),
					explanation_axioms_helpers:only_preferred(a,[a,c]),
					\+ (explanation_axioms_helpers:only_preferred(a,[a,d])),
					\+ (explanation_axioms_helpers:only_preferred(a,[a,c,d])),
					\+ (explanation_axioms_helpers:only_preferred(b,[a,b,c])),
					\+ (explanation_axioms_helpers:only_preferred(b,[b])),
					\+ (explanation_axioms_helpers:only_preferred(b,[])).

preferred :- explanation_axioms_helpers:preferred(a),
			\+ (explanation_axioms_helpers:preferred(b)), 
			\+ (explanation_axioms_helpers:preferred(c)), 
			explanation_axioms_helpers:preferred(d).




%
% Feasibility tests
%





only_feasible_alt_tests:-only_f1,only_f2,only_f3.


explains_noalt_tests:-
	explains_noalt(t19,s0),
	explains_noalt(t20,s0),
	explains_noalt(t16,s0),
	explains_noalt(t11,s0),
	assertz(feas(t22,s0)),
	assertz(feas(g11,s0)),
	\+ explains_noalt(t13,s0),
	\+ explains_noalt(t14,s0),
	\+ explains_noalt(t16,s0),
	\+ explains_noalt(g12,s0),
	retract(feas(t22,s0)),
	retract(feas(g11,s0)).

only_f3 :- assertz(feas(t22,s0)),
			\+ only_feasible_alt(t13,s0),
			\+ only_feasible_alt(t14,s0),
			\+ only_feasible_alt(t16,s0),
			\+ only_feasible_alt(g15,s0),
			only_feasible_alt(t19,s0),
			only_feasible_alt(t20,s0),
			retract(feas(t22,s0)).

only_f2 :- assertz(feas(t20,s0)),
			\+ only_feasible_alt(t19,s0),
			\+ only_feasible_alt(t21,s0),
			only_feasible_alt(t14,s0),
			retract(feas(t20,s0)).

only_f1 :- only_feasible_alt(t19,s0),
			only_feasible_alt(t20,s0),
			only_feasible_alt(t22,s0),
			only_feasible_alt(t16,s0).



all_feasibility_tests :- get_feasible_test,none_feasible_test.


get_feasible_test :- 
	explanation_axioms_helpers:get_feasible([a,b,d],s0,[a,d]),
	explanation_axioms_helpers:get_feasible([b,e],s0,[]),
	explanation_axioms_helpers:get_feasible([b,e,c],s0,[c]),
	\+ (explanation_axioms_helpers:get_feasible([a,b,d],s0,[a])).

none_feasible_test :- 
	explanation_axioms_helpers:none_feasible([e,f,g],s0),
	explanation_axioms_helpers:none_feasible([],s0),
	\+ (explanation_axioms_helpers:none_feasible([e,f,a],s0)),
	\+ (explanation_axioms_helpers:none_feasible([a,e,f],s0)),
	\+ (explanation_axioms_helpers:none_feasible([a,e,f,d],s0)).



%
% Prefix tests
%
get_sit_prefix_tests:-
	explanation_axioms_helpers:get_sit_prefix(b,do(b,s0),s0),
	explanation_axioms_helpers:get_sit_prefix(b,do(b,s0),s0),
	explanation_axioms_helpers:get_sit_prefix(b,do(a,do(c,do(b,s0))),s0),
	explanation_axioms_helpers:get_sit_prefix(c,do(a,do(c,do(b,s0))),do(b,s0)),
	explanation_axioms_helpers:get_sit_prefix(a,do(a,do(c,do(b,s0))),do(c,do(b,s0))),
	explanation_axioms_helpers:get_sit_prefix(c,do(c,do(a,do(c,do(b,s0)))), do(b, s0)),
	\+ (explanation_axioms_helpers:get_sit_prefix(f,do(c,do(a,do(c,do(b,s0)))),_)),
	explanation_axioms_helpers:get_sit_prefix(f,s0,s0).





%
% Prefix 2 tests
%

goal(property1).
goal(property2).
goal(property3).


holds(sat_property1,s0).
holds(sat_property2,do(b,do(a,s0))).
holds(sat_property3,do(c,(do(b,do(a,s0))))).
holds(sat_property1,do(c,(do(b,do(a,s0))))).

is_prefix_tests:-is_prefix_tests_actions,is_prefix_tests_fluents.


is_prefix_tests_actions :- 
	% Action related
	\+ is_prefix(a,s0,_),
	is_prefix(a,do(b,do(a,s0)),s0),
	is_prefix(b,do(b,do(a,s0)),do(a,s0)),
	\+ is_prefix(c,do(b,do(a,s0)),_),
	is_prefix(b,do(b,do(c,do(b,do(a,s0)))),do(a,s0)),
	is_prefix(b,do(b,do(b,do(b,do(a,s0)))),do(a,s0)).
	% Fluent related
is_prefix_tests_fluents :- is_prefix(property1,s0,s0),
	\+ is_prefix(property2,s0,_),
	\+ is_prefix(property3,s0,_),
	is_prefix(property1,do(a,s0),s0),
	\+ is_prefix(property2,do(a,s0),_),
	\+ is_prefix(property3,do(a,s0),_),
	is_prefix(property1,do(b,do(a,s0)),s0),
	is_prefix(property2,do(b,do(a,s0)),do(a,s0)),
	\+ is_prefix(property3,do(b,do(a,s0)),_),
	is_prefix(property1,do(c,do(b,do(a,s0))),s0),
	is_prefix(property2,do(c,do(b,do(a,s0))),do(a,s0)),
	is_prefix(property3,do(c,do(b,do(a,s0))),do(b,do(a,s0))),
	is_prefix(property1,do(d,do(c,do(b,do(a,s0)))),s0),
	is_prefix(property2,do(d,do(c,do(b,do(a,s0)))),do(a,s0)),
	is_prefix(property3,do(d,do(c,do(b,do(a,s0)))),do(b,do(a,s0))).
	
	
	



%
% has_occured
%
has_occured_tests :-
					\+ has_occured(property1,s0),
					has_occured(property2,s0),
					has_occured(property1,do(b,do(a,s0))),
					\+ has_occured(property1,do(c,do(a,s0))),
					\+ has_occured(property1,do(b,do(c,s0))),
					\+ has_occured(property1,do(a,do(b,s0))),
					has_occured(property1,do(c,do(b,do(a,s0)))),
					\+ has_occured(property1,do(b,do(c,do(a,s0)))).