%:-consult("../golog/golog.pl").
%:-use_module("../axioms/structural_axioms.pl").
%:-use_module("../axioms/explanation_axioms.pl").
%:-use_module("../axioms/explanation_engine.pl").
%:-use_module("../axioms/explanation_axioms_helpers.pl").
:-consult("../axioms/explain_main.pl").
:-multifile primitive_action/1.
:-style_check(-discontiguous).
:-style_check(-singleton).
:-set_prolog_flag(toplevel_print_anon, false).

%Run with: do(main).



%
% I N I T I A L I Z A T I O N
%

%student(xing).
%departmentalSecretary(amr).
%instructor(naya).
%departmentalChair(matilda).


student(student).
departmentalSecretary(secretary).
instructor(instructor).
instructorAvailable(instructor).
%instructorAvailable(instructor).
departmentalChair(chair).
facultyCommitteeOfficer(fCO).
facultyCommittee(fC).
appealsCommitteeChair(acc).
senateAppealsCommittee(sac).

requestWithinYear(student).
requestWithin30days(foo).
requestsCPS(student).
physicianStatement(foo).

attendingUniversityForFirstTime(student).
coursesCompletedWithinFisrtYear(foo).



cpsWaived(T):- attendingUniversityForFirstTime(T),coursesCompletedWithinFisrtYear(T),requestWithinYear(T).




% #     #                                #####                                                        
% ##   ##  ####  #####  ###### #        #     # ##### #####  #    #  ####  ##### #    # #####  ###### 
% # # # # #    # #    # #      #        #         #   #    # #    # #    #   #   #    # #    # #      
% #  #  # #    # #    # #####  #         #####    #   #    # #    # #        #   #    # #    # #####  
% #     # #    # #    # #      #              #   #   #####  #    # #        #   #    # #####  #      
% #     # #    # #    # #      #        #     #   #   #   #  #    # #    #   #   #    # #   #  #      
% #     #  ####  #####  ###### ######    #####    #   #    #  ####   ####    #    ####  #    # ###### 
  

% Goals
%% Initiator
goal(rootGoal(_)). % RESERVED
task(foo).
task(delegate(_,_,_,_)). %RESERVED



%
%  Student
%

% Goals and Tasks
goal(beWithdrawnFromCourseAfterDeadeline(_)).
    goal(haveLateWithdrawalRequested(_)).
		goal(withdrawalRequestSubmitted(_)).
			goal(coursePerformanceSummaryObtained(_)).
			task(meetWithAcademicAdvisor(_)).
			goal(fullWithdrawalRequestSubmitted(_)).
		goal(withdrawalRequestWithoutCPSSubmitted(_)).
	goal(considerPetitioning(_)).
		goal(lateWithdrawalPetitioned(_)).
		task(noPetitionNeeded(_)).
	goal(considerAppeal(_)).
		goal(lateWithdrawalDecisionAppealed(_)).
		task(noAppealNeeded(_)).
task(submitAppealDocumentation(_,_)).

or_node(beWithdrawFromCourseAfterDeadeline(_)).
or_node(haveLateWithdrawalRequested(_)).
or_node(considerPetitioning(_)).
or_node(considerAppeal(_)).


% Precedences
pre(coursePerformanceSummaryObtained(T),fullWithdrawalRequestSubmitted(T)).
pre(meetWithAcademicAdvisor(T),fullWithdrawalRequestSubmitted(T)).

% Hierarchy
parent(rootGoal(beWithdrawnFromCourseAfterDeadeline),beWithdrawnFromCourseAfterDeadeline(_)).
parent(beWithdrawnFromCourseAfterDeadeline(T),haveLateWithdrawalRequested(T)).
parent(beWithdrawnFromCourseAfterDeadeline(T),considerPetitioning(T)).
parent(beWithdrawnFromCourseAfterDeadeline(T),considerAppeal(T)).
parent(considerPetitioning(T),lateWithdrawalPetitioned(T)).
parent(considerAppeal(T),lateWithdrawalDecisionAppealed(T)).
parent(haveLateWithdrawalRequested(T),withdrawalRequestSubmitted(T)).
parent(haveLateWithdrawalRequested(T),withdrawalRequestWithoutCPSSubmitted(T)).
parent(withdrawalRequestSubmitted(T),meetWithAcademicAdvisor(T)).
parent(withdrawalRequestSubmitted(T),coursePerformanceSummaryObtained(T)).
parent(withdrawalRequestSubmitted(T),fullWithdrawalRequestSubmitted(T)).


% Dependencies
dependency(T,I,
           coursePerformanceSummaryObtained(T),handleCPSRequest(I,T)):-
		   student(T),instructor(I).
dependency(T,FO,
           lateWithdrawalPetitioned(T),studentPetitionHandledByOfficer(FO,T)):-
		   student(T),facultyCommitteeOfficer(FO).
dependency(T,ACC,
           lateWithdrawalDecisionAppealed(T),lateWithdrawalAppealHandled(ACC,T)):-
		   student(T),appealsCommitteeChair(ACC).
dependency(T,S,
           fullWithdrawalRequestSubmitted(T),lateWithdrawalRequestHandled(S,T)):-
		   student(T),departmentalSecretary(S).
dependency(T,S,
           withdrawalRequestWithoutCPSSubmitted(T),lateWithdrawalRequestHandled(S,T)):-
		   student(T),departmentalSecretary(S).

% Contributions
contr(foo,bar,plus).


%
%  Instructor
%

% Goals and Tasks
goal(handleCPSRequest(_,_)).
    task(writeAndSubmitCPS(_,_)).
	goal(delegateCPSRequest(_,_)).

parent(handleCPSRequest(I,T),writeAndSubmitCPS(I,T)).
parent(handleCPSRequest(I,T),delegateCPSRequest(I,T)).


or_node(handleCPSRequest(_,_)).

% Dependencies
dependency(I,C,
           delegateCPSRequest(I,T),handleCPSRequest(C,I,T)):-
		   instructor(I),departmentalChair(C).


%
%  Departmental Chair
%

% Goals and Tasks
goal(handleCPSRequest(_,_,_)).
    task(writeAndSubmitCPS(_,_,_)).
goal(decideUponWithdrawalRequest(_,_,_)).
    task(rejectCourseWithdrawalRequest(_,_,_)).
	task(grantCourseWithdrawalRequest(_,_,_)).


or_node(decideUponWithdrawalRequest(_,_,_)).
	
% Hierarchy
parent(handleCPSRequest(C,I,T),writeAndSubmitCPS(C,I,T)).
parent(decideUponWithdrawalRequest(C,E,T),rejectCourseWithdrawalRequest(C,E,T)).
parent(decideUponWithdrawalRequest(C,E,T),grantCourseWithdrawalRequest(C,E,T)).


%
%  Departmental Secretary
%

% Goals and Tasks
goal(lateWithdrawalRequestHandled(_,_)).
	goal(haveChairAdjudicate(_,_)).
    task(deskRejectCourseWithdrawalRequest(_,_)).
    task(grantCourseWithdrawalRequest(_,_)).
	
or_node(lateWithdrawalRequestHandled(_,_)).

% Hierarchy
parent(lateWithdrawalRequestHandled(E,T),haveChairAdjudicate(E,T)).
parent(lateWithdrawalRequestHandled(E,T),deskRejectCourseWithdrawalRequest(E,T)).
parent(lateWithdrawalRequestHandled(E,T),grantCourseWithdrawalRequest(E,T)).

dependency(E,C,
           haveChairAdjudicate(E,T),decideUponWithdrawalRequest(C,E,T)):-
		   departmentalSecretary(E),departmentalChair(C).

%
% Faculty Committee Officer
%

% Goals and Tasks
goal(studentPetitionHandledByOfficer(_,_)).
	goal(forwardToPetitionCommittee(_,_)).
    task(deskRejectPetition(_,_)).

or_node(studentPetitionHandledByOfficer(_,_)).

parent(studentPetitionHandledByOfficer(FO,T),forwardToPetitionCommittee(FO,T)).
parent(studentPetitionHandledByOfficer(FO,T),deskRejectPetition(FO,T)).

dependency(FO,FC,
           forwardToPetitionCommittee(FO,FC),petitionHandledByCommittee(FO,FC)):-
		   facultyCommitteeOfficer(FO),facultyCommittee(FC).

%
% Faculty Committee
%

% Goals and Tasks
goal(petitionHandledByCommittee(_,_)).
	task(grantPetition(_,_)).
    task(rejectPetition(_,_)).

or_node(petitionHandledByCommittee(_,_)).

parent(petitionHandledByCommittee(FC,T),grantPetition(FC,T)).
parent(petitionHandledByCommittee(FC,T),rejectPetition(FC,T)).


%
% Appeals Committee Chair
%

goal(lateWithdrawalAppealHandled(_,_)).
	goal(appealDocumentationReceived(_,_)).
	goal(completeLateWithdrawalAppealHandled(_,_)).
		task(doNotAdvanceProceeding(_,_)).
		goal(haveProceedingAdvanced(_,_)).
task(submitAppealDocumentation(_,_)).

or_node(completeLateWithdrawalAppealHandled(_,_)).

parent(lateWithdrawalAppealHandled(ACC,T),appealDocumentationReceived(ACC,T)).
parent(lateWithdrawalAppealHandled(ACC,T),completeLateWithdrawalAppealHandled(ACC,T)).
parent(completeLateWithdrawalAppealHandled(ACC,T),doNotAdvanceProceeding(ACC,T)).
parent(completeLateWithdrawalAppealHandled(ACC,T),haveProceedingAdvanced(ACC,T)).

pre(appealDocumentationReceived(ACC,T),completeLateWithdrawalAppealHandled(ACC,T)).

dependency(ACC,SAC,
           haveProceedingAdvanced(ACC,T),appealForRejectedLateWithdrawalHandled(SAC,T)):-
		   appealsCommitteeChair(ACC),senateAppealsCommittee(SAC).

dependency(ACC,T,
           appealDocumentationReceived(ACC,T),submitAppealDocumentation(T,ACC)):-
		   appealsCommitteeChair(ACC),student(T).

%
% Senate Appeals Committee
%

goal(appealForRejectedLateWithdrawalHandled(_,_)).
	task(dismissAppealWithoutHearing(_,_)).
	goal(appealHearingHandled(_,_)).
		goal(participationArranged(_,_)).
			task(inviteStudentToMeeting(_,_)).
			task(omitStudentInvitationToMeeting(_,_)).
		task(conductAppealHearing(_,_)).
		goal(appealDecisionMade(_,_)).
			task(denyAppeal(_,_)).
			task(grantRelief(_,_)).	
		goal(responsedIssued(_,_)).
			task(issueWrittenResponse(_,_)).
			task(issueOralResponse(_,_)).
		task(disposeAppealFile(_,_)).


pre(participationArranged(SAC,T),conductAppealHearing(SAC,T)).
pre(conductAppealHearing(SAC,T),appealDecisionMade(SAC,T)).
pre(appealDecisionMade(SAC,T),responsedIssued(SAC,T)).
pre(responsedIssued(SAC,T),disposeAppealFile(SAC,T)).

or_node(appealForRejectedLateWithdrawalHandled(_,_)).
or_node(participationArranged(_,_)).
or_node(appealDecisionMade(_,_)).
or_node(responsedIssued(_,_)).


parent(appealForRejectedLateWithdrawalHandled(SAC,T),dismissAppealWithoutHearing(SAC,T)).
parent(appealForRejectedLateWithdrawalHandled(SAC,T),appealHearingHandled(SAC,T)).
parent(appealHearingHandled(SAC,T),participationArranged(SAC,T)).
parent(appealHearingHandled(SAC,T),conductAppealHearing(SAC,T)).
parent(appealHearingHandled(SAC,T),appealDecisionMade(SAC,T)).
parent(appealHearingHandled(SAC,T),responsedIssued(SAC,T)).
parent(appealHearingHandled(SAC,T),disposeAppealFile(SAC,T)).

parent(participationArranged(SAC,T),inviteStudentToMeeting(SAC,T)).
parent(participationArranged(SAC,T),omitStudentInvitationToMeeting(SAC,T)).

parent(appealDecisionMade(SAC,T),denyAppeal(SAC,T)).
parent(appealDecisionMade(SAC,T),grantRelief(SAC,T)).

parent(responsedIssued(SAC,T),issueWrittenResponse(SAC,T)).
parent(responsedIssued(SAC,T),issueOralResponse(SAC,T)).





%    #                                   #######                                   
%   # #    ####  ##### #  ####  #    #      #    #    # ######  ####  #####  #   # 
%  #   #  #    #   #   # #    # ##   #      #    #    # #      #    # #    #  # #  
% #     # #        #   # #    # # #  #      #    ###### #####  #    # #    #   #   
% ####### #        #   # #    # #  # #      #    #    # #      #    # #####    #   
% #     # #    #   #   # #    # #   ##      #    #    # #      #    # #   #    #   
% #     #  ####    #   #  ####  #    #      #    #    # ######  ####  #    #   #   
 



%
% PRIMITIVE ACTIONS LIST
%

primitive_action(X):-task(X).


%
% PROCEDURES, ATTEMPT, AND SATISFACTION FORMULAE
%



%
% Student
%

proc(main,
	pi(t, ?(student(t)) : beWithdrawnFromCourseAfterDeadeline(t))
).



% Conditinoal OR Decomposition -- TO AUGMENT
proc(beWithdrawnFromCourseAfterDeadeline(T), 
	haveLateWithdrawalRequested(T) : considerPetitioning(T) : considerAppeal(T)
).

proc(considerPetitioning(T), 
	lateWithdrawalPetitioned(T) # noPetitionNeeded(T)
).

proc(considerAppeal(T), 
	lateWithdrawalDecisionAppealed(T) # noAppealNeeded(T)
).


proc(haveLateWithdrawalRequested(T), 
	withdrawalRequestSubmitted(T) # withdrawalRequestWithoutCPSSubmitted(T)
).


proc(withdrawalRequestSubmitted(T), 
	coursePerformanceSummaryObtained(T):meetWithAcademicAdvisor(T):fullWithdrawalRequestSubmitted(T)
).

proc(withdrawalRequestSubmitted(T), 
	meetWithAcademicAdvisor(T):coursePerformanceSummaryObtained(T):fullWithdrawalRequestSubmitted(T)
).


% Student dependencies

proc(coursePerformanceSummaryObtained(T), 
	pi(i, ?(instructor(i)) : delegate(T,i,coursePerformanceSummaryObtained(T), handleCPSRequest(i,T)) : 
							handleCPSRequest(i,T))
).


proc(fullWithdrawalRequestSubmitted(T), 
	pi(e, ?(departmentalSecretary(e)) : delegate(T,e,fullWithdrawalRequestSubmitted(T), lateWithdrawalRequestHandled(e,T)): 								lateWithdrawalRequestHandled(e,T))
).

proc(withdrawalRequestWithoutCPSSubmitted(T), 
	pi(e, ?(departmentalSecretary(e)) : delegate(T,e,withdrawalRequestWithoutCPSSubmitted(T), lateWithdrawalRequestHandled(e,T)): 
										lateWithdrawalRequestHandled(e,T))
).

proc(lateWithdrawalPetitioned(T), 
	pi(fO, ?(facultyCommitteeOfficer(fO)) : delegate(T,fO,lateWithdrawalPetitioned(T),
	studentPetitionHandledByOfficer(fO,T)): 
										studentPetitionHandledByOfficer(fO,T))
).


proc(lateWithdrawalDecisionAppealed(T), 
	pi(acc, ?(appealsCommitteeChair(acc)) : delegate(T,acc,lateWithdrawalDecisionAppealed(T),
	lateWithdrawalAppealHandled(acc,T)): lateWithdrawalAppealHandled(acc,T))
).


%
% Instructor
%

proc(handleCPSRequest(I,T), 
	writeAndSubmitCPS(I,T) # delegateCPSRequest(I,T)
).

% Instructor dependencies
proc(delegateCPSRequest(I,T), 
	pi(c, ?(departmentalChair(c)) : delegate(I,c,delegateCPSRequest(I,T),
	handleCPSRequest(c,I,T)) : handleCPSRequest(c,I,T))
).



%
% Departmental Chair
%

proc(handleCPSRequest(C,I,T), 
	writeAndSubmitCPS(C,I,T)
).

proc(decideUponWithdrawalRequest(C,E,T), 
	rejectCourseWithdrawalRequest(C,E,T) # grantCourseWithdrawalRequest(C,E,T)
).



%
%  Departmental Secretary
%

proc(lateWithdrawalRequestHandled(E,T),
	haveChairAdjudicate(E,T) # deskRejectCourseWithdrawalRequest(E,T) # grantCourseWithdrawalRequest(E,T)
).


proc(haveChairAdjudicate(E,T), 
	pi(c, ?(departmentalChair(c)) : delegate(E,c,haveChairAdjudicate(E,T),
	decideUponWithdrawalRequest(c,E,T)) : decideUponWithdrawalRequest(c,E,T))
).



%
%  Faculty Committee Officer
%

proc(studentPetitionHandledByOfficer(FO,T),
	forwardToPetitionCommittee(FO,T) # deskRejectPetition(FO,T)
).


proc(forwardToPetitionCommittee(FO,T),
	pi(fc, ?(facultyCommittee(fc)) : delegate(FO,fc,forwardToPetitionCommittee(FO,T),petitionHandledByCommittee(fc,T)) : petitionHandledByCommittee(fc,T))
).

%
%  Faculty Comittee
%


proc(petitionHandledByCommittee(FC,T),
	grantPetition(FC,T) # rejectPetition(FC,T)
).


%
% Appeals Committee Chair
%

proc(lateWithdrawalAppealHandled(ACC,T),
	appealDocumentationReceived(ACC,T) :
	completeLateWithdrawalAppealHandled(ACC,T)
).

proc(appealDocumentationReceived(ACC,T),
	delegate(ACC,T,appealDocumentationReceived(ACC,T),submitAppealDocumentation(T,ACC)) : 
	submitAppealDocumentation(T,ACC)
).

proc(completeLateWithdrawalAppealHandled(ACC,T),
	doNotAdvanceProceeding(ACC,T) #
	haveProceedingAdvanced(ACC,T)
).


proc(haveProceedingAdvanced(ACC,T),
	pi(sac, ?(senateAppealsCommittee(sac)) : 
	delegate(ACC,sac,haveProceedingAdvanced(ACC,T),appealForRejectedLateWithdrawalHandled(sac,T)) :
	appealForRejectedLateWithdrawalHandled(sac,T))
).


%
% Senate Appeals Committee
%

proc(appealForRejectedLateWithdrawalHandled(SAC,T),
	dismissAppealWithoutHearing(SAC,T) # appealHearingHandled(SAC,T)
).


proc(appealHearingHandled(SAC,T),
	participationArranged(SAC,T) :
	conductAppealHearing(SAC,T) :
	appealDecisionMade(SAC,T) :
	responsedIssued(SAC,T) :
	disposeAppealFile(SAC,T)
).


proc(participationArranged(SAC,T),
	inviteStudentToMeeting(SAC,T) # 
	omitStudentInvitationToMeeting(SAC,T)
).

proc(appealDecisionMade(SAC,T),
	denyAppeal(SAC,T) # 
	grantRelief(SAC,T)
).

proc(responsedIssued(SAC,T),
	issueWrittenResponse(SAC,T) # 
	issueOralResponse(SAC,T)
).







%
% ACTION PRECONDITION AXIOMS
%

poss(foo,S). % RESERVED
poss(delegate(_,_,_,_),S). % RESERVED

poss(noPetitionNeeded(T),S):- departmentalRequestGranted(T,S).
poss(noAppealNeeded(T),S) :- departmentalRequestGranted(T,S); facultyPetitionGranted(T,S).

poss(meetWithAcademicAdvisor(_),S).
poss(writeAndSubmitCPS(I,T),S):- requestsCPS(T),instructor(I),instructorAvailable(I).
poss(writeAndSubmitCPS(_,_,T),S):- requestsCPS(T).

%Secretary
poss(deskRejectCourseWithdrawalRequest(_,T),S):- \+ requestWithinYear(T).
poss(grantCourseWithdrawalRequest(_,T),S):- requestWithin30days(T),(cpsAttached(T,S);cpsWaived(T)).



%Chair
poss(rejectCourseWithdrawalRequest(_,_,_),S).
poss(grantCourseWithdrawalRequest(_,_,_),S):-(cpsAttached(T,S);cpsWaived(T)).


% Petition stage
poss(grantPetition(_,T),S) :- departmentalRequestRejected(T,S).
poss(rejectPetition(_,T),S) :- departmentalRequestRejected(T,S).
poss(deskRejectPetition(_,_),S).

poss(doNotAdvanceProceeding(ACC,T),S). % :- sat_appealDocumentationReceived(ACC,T,S).
poss(submitAppealDocumentation(_,_),S).

poss(dismissAppealWithoutHearing(_,_),S).
poss(inviteStudentToMeeting(_,T),S):-facultyPetitionRejected(T,S).
poss(omitStudentInvitationToMeeting(_,T),S):-facultyPetitionRejected(T,S).
poss(conductAppealHearing(SAC,T),S):-facultyPetitionRejected(T,S),sat_participationArranged(SAC,T,S).
poss(denyAppeal(SAC,T),S):-facultyPetitionRejected(T,S), perf_conductAppealHearing(SAC,T,S).
poss(grantRelief(SAC,T),S):-facultyPetitionRejected(T,S), perf_conductAppealHearing(SAC,T,S).
poss(issueWrittenResponse(SAC,T),S):-facultyPetitionRejected(T,S), sat_appealDecisionMade(SAC,T,S).
poss(issueOralResponse(SAC,T),S):-facultyPetitionRejected(T,S), sat_appealDecisionMade(SAC,T,S).
poss(disposeAppealFile(SAC,T),S):-facultyPetitionRejected(T,S), sat_responsedIssued(SAC,T,S).


%
% SATISFACTION AXIOMS
%


%
% Student
%


% Update this one TEMPORARY
sat_beWithdrawnFromCourseAfterDeadeline(T,S) :- 
		sat_haveLateWithdrawalRequested(T,S),sat_considerPetitioning(T,S),sat_considerAppeal(T,S).
sat_considerPetitioning(T,S):- sat_lateWithdrawalPetitioned(T,S);perf_noPetitionNeeded(T,S).
sat_considerAppeal(T,S):- sat_lateWithdrawalDecisionAppealed(T,S);perf_noAppealNeeded(T,S).
		
sat_lateWithdrawalDecisionAppealed(T,S) :- sat_lateWithdrawalAppealHandled(ACC,T,S),
		appealsCommitteeChair(ACC),student(T).

sat_lateWithdrawalPetitioned(T,S) :- sat_studentPetitionHandledByOfficer(FO,T,S),facultyCommitteeOfficer(FO).
		
	
sat_haveLateWithdrawalRequested(T,S) :- 
		sat_withdrawalRequestSubmitted(T,S).
sat_haveLateWithdrawalRequested(T,S) :- 
		sat_fullWithdrawalRequestSubmitted(T,S).
		
sat_withdrawalRequestSubmitted(T,S):- 
	sat_coursePerformanceSummaryObtained(T,S),perf_meetWithAcademicAdvisor(T,S),sat_fullWithdrawalRequestSubmitted(T,S).
								

sat_fullWithdrawalRequestSubmitted(T,S) :- sat_lateWithdrawalRequestHandled(E,T,S),departmentalSecretary(E).
sat_withdrawalRequestWithoutCPSSubmitted(T,S) :- sat_lateWithdrawalRequestHandled(E,T,S),departmentalSecretary(E).

sat_coursePerformanceSummaryObtained(T,S):- sat_handleCPSRequest(I,T,S),instructor(I).


%
% Instructor
%


sat_handleCPSRequest(I,T,S):- 
	perf_writeAndSubmitCPS(I,T,S).
sat_handleCPSRequest(I,T,S):- 	
	sat_delegateCPSRequest(I,T,S).
	
sat_delegateCPSRequest(I,T,S):- sat_handleCPSRequest(C,I,T,S),departmentalChair(C).


%
% Departmental Chair
%


sat_handleCPSRequest(C,I,T,S) :- perf_writeAndSubmitCPS(C,I,T,S).


sat_decideUponWithdrawalRequest(C,E,T,S):-
    perf_rejectCourseWithdrawalRequest(C,E,T,S);
	perf_grantCourseWithdrawalRequest(C,E,T,S).


%
% Departmental Secretary
%


sat_lateWithdrawalRequestHandled(E,T,S):-
    sat_haveChairAdjudicate(E,T,S) ;
	perf_deskRejectCourseWithdrawalRequest(E,T,S) ;
	perf_grantCourseWithdrawalRequest(E,T,S).
sat_haveChairAdjudicate(E,T,S) :- sat_decideUponWithdrawalRequest(C,E,T,S),departmentalChair(C).


sat_studentPetitionHandledByOfficer(FO,T,S) :- 
	sat_forwardToPetitionCommittee(FO,T,S) ; perf_deskRejectCourseWithdrawalRequest(FO,T,S).

sat_forwardToPetitionCommittee(FO,T,S):-
	sat_petitionHandledByCommittee(FC,T,S),facultyCommittee(FC).
	
%
% Faculty Committee
%
	
sat_petitionHandledByCommittee(FC,T,S):-
	perf_grantPetition(FC,T,S) ; perf_rejectPetition(FC,T,S).



%
% Faculty Committee Officer
%


sat_studentPetitionHandledByOfficer(FO,T,S):-
	sat_forwardToPetitionCommittee(FO,T,S) ; 
	perf_deskRejectPetition(FO,T,S).


sat_forwardToPetitionCommittee(_,T,S) :- sat_petitionHandledByCommittee(_,T,S).

%
% Senate Appeals Chair
%

sat_lateWithdrawalAppealHandled(ACC,T,S):-
	sat_appealDocumentationReceived(ACC,T,S),
	sat_completeLateWithdrawalAppealHandled(ACC,T,S).

sat_completeLateWithdrawalAppealHandled(ACC,T,S):-
	perf_doNotAdvanceProceeding(ACC,T,S);
	sat_haveProceedingAdvanced(ACC,T,S).

sat_appealDocumentationReceived(ACC,T,S) :- perf_submitAppealDocumentation(T,ACC,S).

sat_haveProceedingAdvanced(ACC,T,S) :- sat_appealForRejectedLateWithdrawalHandled(SAC,T,S),
				senateAppealsCommittee(SAC),
				appealsCommitteeChair(ACC).



%
% Senate Appeals Committee
%


sat_appealForRejectedLateWithdrawalHandled(SAC,T,S):-
	perf_dismissAppealWithoutHearing(SAC,T,S);
	sat_appealHearingHandled(SAC,T,S).

sat_appealHearingHandled(SAC,T,S):-
	sat_participationArranged(SAC,T,S),
	perf_conductAppealHearing(SAC,T,S),
	sat_appealDecisionMade(SAC,T,S),
	sat_responsedIssued(SAC,T,S),
	perf_disposeAppealFile(SAC,T,S).

sat_participationArranged(SAC,T,S):-
	perf_inviteStudentToMeeting(SAC,T,S);
	perf_omitStudentInvitationToMeeting(SAC,T,S).
	
sat_appealDecisionMade(SAC,T,S) :-
	perf_denyAppeal(SAC,T,S);
	perf_grantRelief(SAC,T,S).	
	
sat_responsedIssued(SAC,T,S) :-
	perf_issueWrittenResponse(SAC,T,S);
	perf_issueOralResponse(SAC,T,S).
	

%
% ORDINARY SATISFACTION AXIOMS
%


departmentalRequestRejected(T,S):- departmentalChair(C),departmentalSecretary(E),perf_rejectCourseWithdrawalRequest(C,E,T,S),!.
departmentalRequestRejected(T,S):- departmentalSecretary(E),perf_deskRejectCourseWithdrawalRequest(E,T,S),!.


departmentalRequestGranted(T,S):- departmentalChair(C),departmentalSecretary(E),perf_grantCourseWithdrawalRequest(C,E,T,S),!.
departmentalRequestGranted(T,S):- departmentalSecretary(E),perf_grantCourseWithdrawalRequest(E,T,S),!.

facultyPetitionRejected(T,S):- perf_rejectPetition(_,T,S);perf_deskRejectPetition(_,T,S).

facultyPetitionGranted(T,S):- perf_grantPetition(_,T,S).

cpsAttached(T,S):-perf_writeAndSubmitCPS(I,T,S),instructor(I),!.
cpsAttached(T,S):-perf_writeAndSubmitCPS(C,I,T,S),instructor(I),departmentalChair(C).


%
% FEASIBILITY AXIOMS
%

feas(havePPCalled(O,P),S):- secretary(T),feas(haveSecretaryCallPP(O,T,P),S).

% In case of richer decompositions, simply form the disjunction of all tasks.
feas(haveSecretaryCallPP(O,T,P),S) :- poss(collectConstraintsByPhone(O,T,P),S). 

% In case of richer decompositions, simply form the disjunction of all tasks.
feas(collectFromCallendar(O,P),S):- poss(collectFromCallendar(O,P),S).



%
% TASKS SUCCESSOR STATE AXIOMS
%

% RESERVED
hasDelegated(Delegator,Delegatee,DelegatorGoal,DelegateeGoal,do(A,S)):- 
									hasDelegated(Delegator,Delegatee,DelegatorGoal,DelegateeGoal,S);
									A = delegate(Delegator,Delegatee,DelegatorGoal,DelegateeGoal).


perf_noPetitionNeeded(Agent,do(A,S)):-perf_noPetitionNeeded(Agent,S);
									A = noPetitionNeeded(Agent).
perf_noAppealNeeded(Agent,do(A,S)):-perf_noAppealNeeded(Agent,S);
									A = noAppealNeeded(Agent).
perf_meetWithAcademicAdvisor(Agent,do(A,S)):-perf_meetWithAcademicAdvisor(Agent,S);
									A = meetWithAcademicAdvisor(Agent).

perf_writeAndSubmitCPS(Agent,T,do(A,S)) :- perf_writeAndSubmitCPS(Agent,T,S);
									A = writeAndSubmitCPS(Agent,T).

perf_writeAndSubmitCPS(Agent,I,T,do(A,S)) :- perf_writeAndSubmitCPS(Agent,I,T,S);
									A = writeAndSubmitCPS(Agent,I,T).

perf_rejectCourseWithdrawalRequest(Agent,E,T,do(A,S)) :- 
									perf_rejectCourseWithdrawalRequest(Agent,E,T,S);
									A = rejectCourseWithdrawalRequest(Agent,E,T).

perf_grantCourseWithdrawalRequest(Agent,E,T,do(A,S)) :- 
									perf_grantCourseWithdrawalRequest(Agent,E,T,S);
									A = grantCourseWithdrawalRequest(Agent,E,T).


perf_grantCourseWithdrawalRequest(Agent,E,T,do(A,S)) :- 
									perf_grantCourseWithdrawalRequest(Agent,E,T,S);
									A = grantCourseWithdrawalRequest(Agent,E,T).




perf_deskRejectCourseWithdrawalRequest(Agent,T,do(A,S)) :- 
									perf_deskRejectCourseWithdrawalRequest(Agent,T,S);
									A = deskRejectCourseWithdrawalRequest(Agent,T).

perf_grantCourseWithdrawalRequest(Agent,T,do(A,S)) :- 
									perf_grantCourseWithdrawalRequest(Agent,T,S);
									A = grantCourseWithdrawalRequest(Agent,T).

perf_deskRejectCourseWithdrawalRequest(Agent,T,do(A,S)) :- 
									perf_deskRejectCourseWithdrawalRequest(Agent,T,S);
									A = deskRejectCourseWithdrawalRequest(Agent,T).


perf_grantPetition(Agent,T,do(A,S)) :- 
									perf_grantPetition(Agent,T,S);
									A = grantPetition(Agent,T).

perf_rejectPetition(Agent,T,do(A,S)) :- 
									perf_rejectPetition(Agent,T,S);
									A = rejectPetition(Agent,T).

perf_deskRejectPetition(Agent,T,do(A,S)) :- 
									perf_deskRejectPetition(Agent,T,S);
									A = deskRejectPetition(Agent,T).



perf_doNotAdvanceProceeding(Agent,T,do(A,S)) :- 
									perf_doNotAdvanceProceeding(Agent,T,S);
									A = doNotAdvanceProceeding(Agent,T).

perf_submitAppealDocumentation(Agent,T,do(A,S)) :- 
									perf_submitAppealDocumentation(Agent,T,S);
									A = submitAppealDocumentation(Agent,T).



perf_dismissAppealWithoutHearing(Agent,T,do(A,S)) :- 
									perf_dismissAppealWithoutHearing(Agent,T,S);
									A = dismissAppealWithoutHearing(Agent,T).

perf_inviteStudentToMeeting(Agent,T,do(A,S)) :- 
									perf_inviteStudentToMeeting(Agent,T,S);
									A = inviteStudentToMeeting(Agent,T).

perf_omitStudentInvitationToMeeting(Agent,T,do(A,S)) :- 
									perf_omitStudentInvitationToMeeting(Agent,T,S);
									A = omitStudentInvitationToMeeting(Agent,T).

perf_conductAppealHearing(Agent,T,do(A,S)) :- 
									perf_conductAppealHearing(Agent,T,S);
									A = conductAppealHearing(Agent,T).

perf_denyAppeal(Agent,T,do(A,S)) :- 
									perf_denyAppeal(Agent,T,S);
									A = denyAppeal(Agent,T).


perf_grantRelief(Agent,T,do(A,S)) :- 
									perf_grantRelief(Agent,T,S);
									A = grantRelief(Agent,T).

perf_issueWrittenResponse(Agent,T,do(A,S)) :- 
									perf_issueWrittenResponse(Agent,T,S);
									A = issueWrittenResponse(Agent,T).

perf_issueOralResponse(Agent,T,do(A,S)) :- 
									perf_issueOralResponse(Agent,T,S);
									A = issueOralResponse(Agent,T).

perf_disposeAppealFile(Agent,T,do(A,S)) :- 
									perf_disposeAppealFile(Agent,T,S);
									A = disposeAppealFile(Agent,T).


%
% ORDINARY SUCCESSOR STATE AXIOMS
%


%departmentalRequestRejected(Student,do(A,S)) :- 
%									departmentalRequestRejected(Student,S);
%									A = rejectCourseWithdrawalRequest(_,_,Student);
%									A = deskRejectCourseWithdrawalRequest(_,Student)
%									.
									
%departmentalRequestGranted(Student,do(A,S)) :- 
%									departmentalRequestGranted(Student,S);
%									A = grantCourseWithdrawalRequest(_,_,Student);
%									A = grantCourseWithdrawalRequest(_,Student)
%									.

%facultyPetitionRejected(Student,do(A,S)) :- 
%									facultyPetitionRejected(Student,S);
%									A = rejectPetition(_,Student);
%									A = deskRejectPetition(_,Student).
									
%facultyPetitionGranted(Student,do(A,S)) :- 
%									facultyPetitionGranted(Student,S);
%									A = grantPetition(_,_,Student).



%
% Argument Restoration - Helpers
%


% Reserved
restoreSitArg(hasDelegated(Delegator,Delegatee,DelegatorGoal,DelegateeGoal),S,hasDelegated(Delegator,Delegatee,DelegatorGoal,DelegateeGoal,S)).

% Ordinary Fluents


% Task Fluents

restoreSitArg(perf_meetWithAcademicAdvisor(A),S,perf_meetWithAcademicAdvisor(A,S)).
restoreSitArg(perf_writeAndSubmitCPS(A,T),S,perf_writeAndSubmitCPS(A,T,S)).
restoreSitArg(perf_writeAndSubmitCPS(A,I,T),S,perf_writeAndSubmitCPS(A,I,T,S)).
restoreSitArg(perf_rejectCourseWithdrawalRequest(A,E,T),S,perf_rejectCourseWithdrawalRequest(A,E,T,S)).
restoreSitArg(perf_grantCourseWithdrawalRequest(A,E,T),S,perf_grantCourseWithdrawalRequest(A,E,T,S)).
restoreSitArg(perf_deskRejectCourseWithdrawalRequest(A,T),S,perf_deskRejectCourseWithdrawalRequest(A,T,S)).
restoreSitArg(perf_grantCourseWithdrawalRequest(A,T),S,perf_grantCourseWithdrawalRequest(A,T,S)).

restoreSitArg(perf_deskRejectPetition(FCO,T),S,perf_deskRejectPetition(FCO,T,S)).
restoreSitArg(perf_grantPetition(FC,T),S,perf_grantPetition(FC,T,S)).
restoreSitArg(perf_rejectPetition(FC,T),S,perf_rejectPetition(FC,T,S)).


restoreSitArg(perf_noAppealNeeded(T),S,perf_noAppealNeeded(T,S)).
restoreSitArg(perf_noPetitionNeeded(T),S,perf_noPetitionNeeded(T,S)).


restoreSitArg(perf_dismissAppealWithoutHearing(SAC,T),S,perf_dismissAppealWithoutHearing(SAC,T,S)).
restoreSitArg(perf_inviteStudentToMeeting(SAC,T),S,perf_inviteStudentToMeeting(SAC,T,S)).
restoreSitArg(perf_omitStudentInvitationToMeeting(SAC,T),S,perf_omitStudentInvitationToMeeting(SAC,T,S)).
restoreSitArg(perf_conductAppealHearing(SAC,T),S,perf_conductAppealHearing(SAC,T,S)).
restoreSitArg(perf_denyAppeal(SAC,T),S,perf_denyAppeal(SAC,T,S)).
restoreSitArg(perf_grantRelief(SAC,T),S,perf_grantRelief(SAC,T,S)).	
restoreSitArg(perf_issueWrittenResponse(SAC,T),S,perf_issueWrittenResponse(SAC,T,S)).
restoreSitArg(perf_issueOralResponse(SAC,T),S,perf_issueOralResponse(SAC,T,S)).
restoreSitArg(perf_disposeAppealFile(SAC,T),S,perf_disposeAppealFile(SAC,T,S)).
restoreSitArg(perf_doNotAdvanceProceeding(ACC,T),S,perf_doNotAdvanceProceeding(ACC,T,S)).
restoreSitArg(perf_submitAppealDocumentation(ACC,T),S,perf_submitAppealDocumentation(ACC,T,S)).

restoreSitArg(cpsAttached(T),S,cpsAttached(T,S)).


% Goal satisfaction Fluents
restoreSitArg(sat_beWithdrawnFromCourseAfterDeadeline(T),S,sat_beWithdrawnFromCourseAfterDeadeline(T,S)).
restoreSitArg(sat_considerPetitioning(T),S,sat_considerPetitioning(T,S)).
restoreSitArg(sat_considerAppeal(T),S,sat_considerAppeal(T,S)).
restoreSitArg(sat_haveLateWithdrawalRequested(T),S,sat_haveLateWithdrawalRequested(T,S)).
restoreSitArg(sat_withdrawalRequestSubmitted(T),S,sat_withdrawalRequestSubmitted(T,S)).


restoreSitArg(sat_fullWithdrawalRequestSubmitted(T),S,sat_fullWithdrawalRequestSubmitted(T,S)).
restoreSitArg(sat_withdrawalRequestWithoutCPSSubmitted(T),S,sat_withdrawalRequestWithoutCPSSubmitted(T,S)).

%restoreSitArg(sat_submitWithdrawalRequest(A),S,sat_submitWithdrawalRequest(A,S)).
%restoreSitArg(sat_submitWithdrawalRequestWithoutCPS(A),S,sat_submitWithdrawalRequestWithoutCPS(A,S)).
restoreSitArg(sat_handleCPSRequest(I,T),S,sat_handleCPSRequest(I,T,S)).
restoreSitArg(sat_delegateCPSRequest(I,T),S,sat_delegateCPSRequest(I,T,S)).
restoreSitArg(sat_handleCPSRequest(C,I,T),S,sat_handleCPSRequest(C,I,T,S)).
restoreSitArg(sat_decideUponWithdrawalRequest(C,E,T),S,sat_decideUponWithdrawalRequest(C,E,T,S)).
restoreSitArg(sat_lateWithdrawalRequestHandled(E,T),S,sat_lateWithdrawalRequestHandled(E,T,S)).
restoreSitArg(sat_haveChairAdjudicate(E,T),S,sat_haveChairAdjudicate(E,T,S)).
restoreSitArg(sat_coursePerformanceSummaryObtained(T),S,sat_coursePerformanceSummaryObtained(T,S)).
%restoreSitArg(sat_appealLateWithdrawalDecision(T),S,sat_appealLateWithdrawalDecision(T,S)).

restoreSitArg(sat_lateWithdrawalDecisionAppealed(T),S,sat_lateWithdrawalDecisionAppealed(T,S)).
restoreSitArg(sat_lateWithdrawalPetitioned(T),S,sat_lateWithdrawalPetitioned(T,S)).


restoreSitArg(sat_studentPetitionHandledByOfficer(FO,T),S,sat_studentPetitionHandledByOfficer(FO,T,S)).

restoreSitArg(sat_forwardToPetitionCommittee(FO,T),S,sat_forwardToPetitionCommittee(FO,T,S)).
restoreSitArg(sat_petitionHandledByCommittee(FC,T),S,sat_petitionHandledByCommittee(FC,T,S)).

restoreSitArg(sat_lateWithdrawalAppealHandled(ACC,T),S,sat_lateWithdrawalAppealHandled(ACC,T,S)).
restoreSitArg(sat_appealDocumentationReceived(ACC,T),S,sat_appealDocumentationReceived(ACC,T,S)).
restoreSitArg(sat_completeLateWithdrawalAppealHandled(ACC,T),S,sat_completeLateWithdrawalAppealHandled(ACC,T,S)).
restoreSitArg(sat_haveProceedingAdvanced(ACC,T),S,sat_haveProceedingAdvanced(ACC,T,S)).

restoreSitArg(sat_appealForRejectedLateWithdrawalHandled(SAC,T),S,sat_appealForRejectedLateWithdrawalHandled(SAC,T,S)).
restoreSitArg(sat_appealHearingHandled(SAC,T),S,sat_appealHearingHandled(SAC,T,S)).
restoreSitArg(sat_participationArranged(SAC,T),S,sat_participationArranged(SAC,T,S)).
restoreSitArg(sat_appealDecisionMade(SAC,T),S,sat_appealDecisionMade(SAC,T,S)).
restoreSitArg(sat_responsedIssued(SAC,T),S,sat_responsedIssued(SAC,T,S)).



%
% User friendly descriptions
%

descr2(X,Y):- descr(X,D),term_string(X,Xt),atomic_list_concat([D,' (',Xt,')'],Y).


%descr(decideMeetingDetails(X),Y):- 
%		term_string(X,Xs),string_concat(Xs,' wanted to decide meeting details.',Y).
descr(haveMeetingOrganized(I),Y):-
		atomic_list_concat(["initiator ",I," wanted to have the meeting organized."],Y),!.
descr(haveSchedulingPerformed(I),Y):-
		atomic_list_concat(["initiator ",I," wanted to have the scheduling of the meeting performed."],Y),!.
descr(haveSchedulingPerformedByOrganizer(I,O),Y):-
		atomic_list_concat(["initiator ",I," wanted to have the scheduling of the meeting performed by organizer ",O,"."],Y),!.
descr(meetingAttended(I),Y):-
		atomic_list_concat(["initiator ",I," wanted to have the meeting attended by participants."],Y),!.
descr(meetingAttendedByParticipant(I,P),Y):-
		atomic_list_concat(["initiator ",I," wanted to have the meeting attended by participant ",P,"."],Y),!.
		descr(decideMeetingDetails(I),Y):- 
		atomic_list_concat(["initiator ",I," wanted to decide meeting details (purpose, potential participants, quality preferences etc.)."],Y),!.
		
descr(timetableCollected(O,P),Y):- 
		atomic_list_concat(["organizer ",O," wanted to collect ",P,"'s timetable."],Y),!.
descr(chooseTimeandDate(O),Y):- 
		atomic_list_concat(["organizer ",O," wanted to choose time and date for the meeting."],Y),!.
descr(haveSecretaryCallPP(O,S,P),Y):-
		atomic_list_concat(["organizer ",O," wanted to have secretary ",S," call participant ",P," to collect their constraints."],Y),!.
descr(meetingAttended(O),Y):-
		atomic_list_concat(["organizer ",O," wanted to have the meeting attended."],Y),!.
descr(haveMeetingScheduled(O),Y):-
		atomic_list_concat(["organizer ",O," wanted to have the meeting that was assigned to them scheduled."],Y),!.
descr(timetablesCollected(O),Y):- 
		atomic_list_concat(["organizer ",O," wanted to collect timetables of all potential participants."],Y),!.
descr(havePPCalled(O,P),Y):- 
		atomic_list_concat(["organizer ",O," wanted to have ", P,"'s constraints collected by phone."],Y),!.
descr(meetingAnnounced(O),Y):- 
		atomic_list_concat(["organizer ",O," wanted to have the meeting announced."],Y),!.
descr(collectFromCallendar(O,P),Y):- 
		atomic_list_concat(["organizer ",O," wanted to collect ",P ,"'s constraints by looking at their online callendars."],Y),!.
descr(announceMeeting(O,P),Y):- 
		atomic_list_concat(["organizer ",O," wanted to announce the meeting to participant ", P, "."],Y),!.

descr(collectConstraintsByPhone(O,T,P),Y):- 
		atomic_list_concat(["secretary ",T," collected ",P,"'s constraints by phone on behalf of ",O,"."],Y),!.

descr(participate(P),Y):- 
		atomic_list_concat(["intitee ",P," wanted to participate to the meeting."],Y),!.

descr(X,X).



 %#######                                                                    #######                                                        
 %#       #    # #####  #        ##   #    #   ##   ##### #  ####  #    #       #    ###### #    # #####  #        ##   ##### ######  ####  
 %#        #  #  #    # #       #  #  ##   #  #  #    #   # #    # ##   #       #    #      ##  ## #    # #       #  #    #   #      #      
 %#####     ##   #    # #      #    # # #  # #    #   #   # #    # # #  #       #    #####  # ## # #    # #      #    #   #   #####   ####  
 %#         ##   #####  #      ###### #  # # ######   #   # #    # #  # #       #    #      #    # #####  #      ######   #   #           # 
 %#        #  #  #      #      #    # #   ## #    #   #   # #    # #   ##       #    #      #    # #      #      #    #   #   #      #    # 
 %####### #    # #      ###### #    # #    # #    #   #   #  ####  #    #       #    ###### #    # #      ###### #    #   #   ######  ####  



%
% Instructor
%


instructorCPSHanldingExplanation(I,"Request not received in written and signed"):- instructorAvailable(I).
instructorCPSHanldingExplanation(I,"Instructor unavailable"):- \+ instructorAvailable(I).
explains_templ(Y,delegateCPSRequest(I,T),_,J):- instructorCPSHanldingExplanation(I,J),atomic_list_concat(["instructor ", I, " delegated student '",T, "''s CSP request to departmental chair with justification: ", J],Y).
explains_templ(Y,writeAndSubmitCPS(C,I,T),_,J):- instructorCPSHanldingExplanation(I,J),atomic_list_concat(["instructor ", I, " delegated student '",T, "''s CSP request to departmental chair with justification: ", J],Y).


%
% Departmental Chair
%

withdrawalRequestChairsRationalle(C,T,_,"Health challenge claims not backed by physician's statement"):- \+ physicianStatement(T).
withdrawalRequestChairsRationalle(C,T,S,"CPS not attached but required"):- \+ cpsAttached(T,S),\+ cpsWaived(T).
chairCPSRationalle(_,I,_,"Instructor unavailable"):- \+ instructorAvailable(I).
% Decision Explanation
explains_templ(Y,rejectCourseWithdrawalRequest(C,_,T),S,J):- withdrawalRequestChairsRationalle(C,T,S,J),atomic_list_concat(["departmental chair [", C, "] rejected student [",T, "]'s request for late withdrawal with justification: ", J],Y).
% Delegation Explanation
explains_templ(Y,rejectCourseWithdrawalRequest(C,E,T),_,J):- secreatryDecisionRationalle(E,T,J),atomic_list_concat(["departmental chair [",C,"] made the decision at the request of secreatary [",E, "]. Delegation was justifed as: ", J],Y).
% CPS Explanation
explains_templ(Y,writeAndSubmitCPS(C,I,T),_,J):- chairCPSRationalle(C,I,T,J),atomic_list_concat(["departmental chair [",C,"] completed the CPS on the stead of instructor [",I, "]. Delegation was justifed as: ", J],Y).


%
% Departmental Secretary
%
secreatryDecisionRationalle(E,T,"request within 1 year of course ending but beyond 30 days - chair to adjudicate"):- requestWithinYear(T),\+ requestWithin30days(T).
secreatryDecisionRationalle(E,T,"request after 1 year threshold from the time course ended - department cannot adjudicate") :- \+ requestWithinYear(T).
secreatryDecisionRationalle(_,T,S,"CPS not attached but required."):- \+(cpsAttached(T,S);cpsWaived(T)).
explains_templ(Y,deskRejectCourseWithdrawalRequest(E,T),_,J):- secreatryDecisionRationalle(E,T,J),atomic_list_concat(["departmental secretary [",E,"] made the decision to desk rejecte late withdrawal request by student [",T,"]. Desk rejection was justifed as: ", J],Y).
explains_templ(Y,haveChairAdjudicate(E,T),_,J):- secreatryDecisionRationalle(E,T,J),atomic_list_concat(["departmental secretary [",E,"] made the decision to delegate to chair the late withdrawal request by student [",T,"]. Delegation was justifed as: ", J],Y).





%
% Faculty Committee & Officer
%

petitionOutcomeExplanation("15 percent of coursework not completed by withdrawal deadline").
explains_templ(Y,rejectPetition(FC,T),_,J):- petitionOutcomeExplanation(J),atomic_list_concat(["committee rejected petition of student [",T, "] with justification: [", J, "]"],Y).

deskRejectPetitionRationalle(_,T,S,"paperwork incomplete: Course Performance Summary (CPS) missing when required"):- \+ cpsAttached(T,S),\+ cpsWaived(T).
deskRejectPetitionRationalle(_,T,S,"departmental request already granted"):- departmentalRequestGranted(T,S).
deskRejectPetitionRationalle(_,T,S,"forms incomplete or information correctness issue"):- departmentalRequestRejected(T,S),(cpsAttached(T,S);cpsWaived(T)).
explains_templ(Y,deskRejectPetition(FO,T),S,J):- deskRejectPetitionRationalle(FO,T,S,J),atomic_list_concat(["faculty committee officer [",FO,"] made the decision to desk reject petition by student [",T,"]. Desk rejection was justifed as: ", J],Y).



%
% Apeals Committee & Chair
%

appealOutcomeExplanation("committee judged that the appellant's circumstances do not justify course performance").
omitInvitationRationale(T,"presence not requested by appealant and not deemed necessary by committee"):-requestsPresenceInHearing(T).
oralResponseRationale("writen response avoided to protect case confidentiality").

explains_templ(Y,denyAppeal(SAC,T),_,J):- appealOutcomeExplanation(J),atomic_list_concat(["Senate appeals committee rejected appeal of student [",T, "] with justification: [", J, "]"],Y).
explains_templ(Y,omitStudentInvitationToMeeting(SAC,T),_,J):- omitInvitationRationale(T,J),atomic_list_concat(["Senate appeals committee did not invite student [",T, "] in hearing with justification: [", J, "]"],Y).
explains_templ(Y,issueOralResponse(SAC,T),_,J):- oralResponseRationale(J),atomic_list_concat(["Senate appeals committee did not invite student [",T, "] in hearing with justification: [", J, "]"],Y).


doNotAdvanceRationalle(ACC,T,S,J) XXX complete
dismissWithoutHearingRationalle(SAC,T,S,J) XXX complete


explains_templ(Y,dismissAppealWithoutHearing(SAC,T),S,J):- dismissWithoutHearingRationalle(SAC,T,S,J),atomic_list_concat(["Senate appeals committee dismissed appeal by student [",T,"] without hearing on the following grounds: ", J],Y).
explains_templ(Y,doNotAdvanceProceeding(ACC,T),S,J):- doNotAdvanceRationalle(ACC,T,S,J),atomic_list_concat(["Senate appeals committee chair did not advance appeal by student [",T,"] with justification: ", J],Y).
