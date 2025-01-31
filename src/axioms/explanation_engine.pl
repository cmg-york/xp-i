:-module(explanation_engine,[why/4,
							why/2,
							why/1,
							why_interactive/1]).
:-use_module('explanation_axioms.pl').
:-use_module('explanation_axioms_helpers.pl').


%
% E X P L A N A T I O N   E N G I N E  
%


why_teleo(X) :- d(S),!,explains_tel(Y,X,S),write('Teleological Explanation: Because '),writeln(Y).

why_canon(X) :- d(S),!,explains_can(Y,X,S),write('Canonical Explanation: Because '),writeln(Y).

why_alt(X) :- d(S),!,explains_noalt(_,X,S),write('Choice mandatory, no other feasible alternative.').

why_pref(X) :-d(S),!,only_preferred_alt(_,X,S),write('From the available choices this is the preferred one.'). 

why_template(X) :- d(S),!,explains_template(Y,X,S,J),write('Template Explanation: Because '),writeln(Y).

/** 
 * why(-Explanation,+Explanatum,+Situation,-Justification) is nondet.
 * Returns explanations of various types for Explanatum in Situation. Explanation is the formal explanation and Justification is the type of explanation (e.g., dependency, refinment etc.).
 */	
why(Y,X,S,J):- (
				explains_template(Y,X,S,J);
				explains_tel(Y,X,S,J);
				explains_can(Y,X,S,J);
				explains_noalt(Y,X,S,J);
				explains_only_preferred(Y,X,S,J)
			   ).

/** 
 * why(+Explanatum,+Situation) is nondet.
 * Prints explanations of various types for Explanatum in Situation.
 */	
why(X,S) :- not_occurred(X,S);(
				explains_template(Y,X,S,J),write('Template Explanation: '),sub_string(Y, 6, _, 0, Yout),write(Yout),
					write(' ('),ansi_format([bold,fg(blue)],'~w', "..."),write(') '),
					write(' ['),write("template explanation"),writeln(']');
				explains_tel(Y,X,S,J),write('Teleological Explanation: Because '),
					descr(Y,Yd),write(Yd),
					write(' ('),ansi_format([bold,fg(blue)],'~w', Y),write(') '),
					write(' ['),write(J),writeln(']');
				explains_can(Y,X,S,J),write('Canonical Explanation: Because '),
					descr(Y,Yd),write(Yd),write(' ('),ansi_format([bold,fg(blue)],'~w', Y),write(') '),
					write(' ['),write(J),writeln(']');
				explains_noalt(Y,X,S,J),write('Choice mandatory, no other feasible alternative.'),
					write(' ('),ansi_format([bold,fg(blue)],'~w', Y),write(') '),
					write(' ['),write(J),writeln(']');
				explains_only_preferred(Y,X,S,J),
					write('From the available choices this is the only preferred one.'),
					write(' ('),ansi_format([bold,fg(blue)],'~w', Y),write(') '),
					write(' ['),write(J),writeln(']')
			).



/** 
 * why(+Explanatum) is nondet
 * Simulates an arbitrary execution S and invokes why(+Explanatum,+S) for it.
 */
why(X) :- d(S),!,why(X,S).


/** 
 * why_interactive(+Explanatum) is nondet
 * Enters explanation loop, whereby each explanation can be recursivelly chosen as the next explanatum.
 */
why_interactive(X):-d(S),!,has_occurred(X,S),why_loop(X,S).
why_interactive(X):-d(S),!,not_occurred(X,S).




why_loop(rootGoal(_),_).
why_loop(X,S):-  bagof(Y,J^why(Y,X,S,J),L),bagof(J,Y^why(Y,X,S,J),Lj),
				write('Explanantia summary for '),ansi_format([bg(yellow)], '~w:', X),
				nl,write_list(1,L,Lj),write('Explain (e to exit):'),read(U),nl,
				((U = 'e',true,!);
/*				  (nth1(U,L,Next),((Next = '...',write('Option not explainable.'),fail,!);
				                   (Next \= '...',write('Explaining: '),writeln(Next),why_loop(Next,S))) */
				  (nth1(U,L,Next),term_to_string(Next,NextT),
								   ((sub_string(NextT, 0, _, _, "..."),writeln('Option not explainable.'),nl,nl,why_loop(X,S),!);
								   (\+ sub_string(NextT, 0, _, _, "..."),write('Explaining: '),writeln(Next),why_loop(Next,S))) 

				   )
				).

