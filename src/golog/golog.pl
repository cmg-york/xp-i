:-consult("golog_swi.pl").
:-dynamic(current_sit/1).

%% MAIN CALL HELPER %%

retractSucc :- retract(current_sit(X)),!.
retractSucc.

do(X) :- do(X,s0,R),prS(R),retractSucc,assertz(current_sit(R)).
d(S) :- \+ current_sit(S),writeln('No history in buffer.'),!.
d(S) :- current_sit(S).

%d(S):- do(main,s0,S),nl,prS(S),nl,nl.


%% PRINT SITUTATION %%

prS(s0):- pS(s0,1).
prS(do(A,S)):- sit_length(do(A,S),N),pS(do(A,S),N).

pS(s0,N):- format('~2d',[N]),write(": "),write(s0).
pS(do(A,S),N):- M is N-1,pS(S,M),nl,format('~2D',[N]),write(": "),write(A).

sit_length(s0,1).
sit_length(do(_,S),N):- sit_length(S,M),N is M+1.

printSit :- \+ current_sit(S),writeln('No history in buffer.'),!,fail.
printSit :- d(S),prS(S),!.

%
% Helpers
%

primitive_action(X):-task(X).

get_fluent(Pred,SatPred) :- goal(Pred),term_string(Pred,SPred),
								string_concat('sat_',SPred,SPredSat),term_string(SatPred,SPredSat),!;
							task(Pred),term_string(Pred,SPred),
								string_concat('perf_',SPred,SPredSat),term_string(SatPred,SPredSat),!.