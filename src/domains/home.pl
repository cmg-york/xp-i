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


room(kitchen).
occupancyRoom(livingRoom).
room(hallway).
occupancyRoom(bedroom1).
room(bedroom2).
exteriorArea(frontYard).
exteriorArea(backYard).

lightController(lc).
occupantExperienceAgent(oea).
energyEfficiencyAgent(eea).
securityAndSafetyAgent(ssa).

occupied(null).

lightOff(livingRoom,s0).
lightOff(hallway,s0).
lightOff(bedroom1,s0).
lightOff(bedroom2,s0).
lightOff(frontYard,s0).
lightOff(backYard,s0).
lightOn(kitchen,s0).

room(X):- occupancyRoom(X).
area(X):- room(X);exteriorArea(X).


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
task(idle(_)).
task(idle(_,_)).
pre(foo,bar).


feas(_,_).
promote(_).

%
%  Security and Safety Agent
%

goal(homeProtectedDuringStorm(_)).
    goal(stormProtectionActuated(_,_)).
		goal(haveAllDoorsAndWindowsClosed(_,_)).
		goal(protecteElectricalSystem(_,_)).
			goal(powerOffRequested(_,_)).
		
goal(unexpectedMotionRespondedTo(_)).
	goal(intruderDeterenceActuated(_,_)).
		goal(allDoorsAndWindowsClosedAndLocked(_,_)).
				goal(haveAllDoorsAndWindowsClosedActuated(_,_)).
				goal(haveAllDoorsAndWindowsLockedActuated(_,_)).
		goal(outsideLightsOn(_,_,_)).
		goal(occupancyLightsOn(_,_,_)).


or_node(homeProtectedDuringStorm(_)).
or_node(unexpectedMotionRespondedTo(_)).
or_node(protectElectricalSystem(_)).


feas(intruderDeterenceActuated


parent(rootGoal(controlHouse),homeProtectedDuringStorm(_)).
parent(rootGoal(controlHouse),homeProtectedforIntruders(_)).

parent(homeProtectedDuringStorm(SSA),stormProtectionActuated(SSA,J)).
parent(homeProtectedDuringStorm(SSA),idle(SSA)).

parent(stormProtectionActuated(SSA,J),protecteElectricalSystem(SSA,J)).
parent(stormProtectionActuated(SSA,J),haveAllDoorsAndWindowsClosed(SSA,J,stormProtectionActuated)).

parent(protecteElectricalSystem(SSA,J),powerOffRequested(SSA,J)).
parent(protecteElectricalSystem(SSA,J),idle(SSA,J)).

parent(unexpectedMotionRespondedTo(SSA),intruderDeterenceActuated(SSA,J)).
parent(unexpectedMotionRespondedTo(SSA),idle(SSA,unexpectedMotionRespondedTo)).

parent(intruderDeterenceActuated(SSA,J),allDoorsAndWindowsClosedAndLocked(SSA,J,[intruderDeterenceActuated])).
parent(intruderDeterenceActuated(SSA,J),outsideLightsOn(SSA,J,[intruderDeterenceActuated])).
parent(intruderDeterenceActuated(SSA,J),occupancyLightsOn(SSA,J,[intruderDeterenceActuated])).

parent(allDoorsAndWindowsClosedAndLocked(SSA,J,G),haveAllDoorsAndWindowsClosed(SSA,J,G)).
parent(allDoorsAndWindowsClosedAndLocked(SSA,J,G),haveAllDoorsAndWindowsLockedActuated(SSA,J,G)).

dependency(SSA,EPC,
           powerOffRequested(SSA,J),cutPowerOff(EPC,SSA,J)):-
		   securityAndSafetyAgent(SSA),electricalPowerController(EPC).

dependency(SSA,DWC,
           haveAllDoorsAndWindowsClosed(SSA,J,G),allDoorsAndWindowsClosed(DWC,J,G)):-
		   securityAndSafetyAgent(SSA),doorAndWindowController(DWC).

dependency(SSA,DWC,
           haveAllDoorsAndWindowsClosed(SSA,J,G),allDoorsAndWindowsClosed(DWC,J,G)):-
		   securityAndSafetyAgent(SSA),doorAndWindowController(DWC).

dependency(SSA,LC,
           outsideLightsOn(SSA,J,G),outsideLightsSwitchedOn(LC,J,[outsideLightsOn|G])):-
		   securityAndSafetyAgent(SSA),lightController(LC).

dependency(SSA,LC, occupancyLightsOn(SSA,J,G),occupancyLightsSwitchedOn(LC,J,[occupancyLightsOn|G])):-
		   securityAndSafetyAgent(SSA),lightController(LC).


%
%  Electrical Power Controller
%

task(cutPowerOff(_,_,_)).


%
%  Door and Window Controller
%

goal(allDoorsAndWindowsClosed(_,_)).
	goal(haveAllDoorsClosed(_,_)).
		task(closeDoor(_,_,_)).
	goal(haveAllWindowsClosed(_,_)).
		task(closeWindow(_,_,_)).

goal(allDoorsAndWindowsLocked(_,_)).
	goal(haveAllDoorsLocked(_,_)).
		task(lockDoor(_,_,_)).
	goal(haveAllWindowsLocked(_,_)).
		task(lockWindow(_,_,_)).

parent(allDoorsAndWindowsClosed(DWC,J,G),haveAllDoorsClosed(DWC,J,G)).
parent(allDoorsAndWindowsClosed(DWC,J,G),haveAllWindowsClosed(DWC,J,G)).

parent(haveAllDoorsClosed(DWC,J,G),closeDoor(DWC,J,G,_)).
parent(haveAllWindowsClosed(DWC,J,G),closeWindow(DWC,J,G,_)).

parent(allDoorsAndWindowsLocked(DWC,J,G),haveAllDoorsLocked(DWC,J,G)).
parent(allDoorsAndWindowsLocked(DWC,J,G),haveAllWindowsLocked(DWC,J,G)).

parent(haveAllDoorsLocked(DWC,J,G),lockDoor(DWC,J,G,_)).
parent(haveAllWindowsLocked(DWC,J,G),lockWindow(DWC,J,G,_)).


%
%  Energy Efficiency Agent
%

goal(turnUnneededLightsOff(_)).
	goal(haveLightTurnedOff(_,_,_,_)).

parent(rootGoal(controlHouse),turnUnneededLightsOff(EAA)).
parent(turnUnneededLightsOff(EEA),haveLightTurnedOff(EEA,_,_,_)).


dependency(EEA,LC,haveLightTurnedOff(EEA,R,J,G),lightIsOff(LC,R,J,[haveLightTurnedOff|G])):-
		   energyEfficiencyAgent(EEA),lightController(LC).


or_node(energyCostsMinimized(_,_)).

%
%  Light Controller
%

goal(outsideLightsSwitchedOn(_,_,_)).
goal(occupancyLightsSwitchedOn(_,_,_)).
	goal(lightIsOn(_,_,_,_)).
		task(turnLightOn(_,_,_,_)).

goal(lightIsOff(_,_,_,_)).
	task(turnLightOff(_,_,_,_)).

parent(outsideLightsSwitchedOn(LC,J,G),lightIsOn(LC,R,J,[outsideLightsSwitchedOn|G])).
parent(occupancyLightsSwitchedOn(LC,J,G),lightIsOn(LC,R,J,[occupancyLightsSwitchedOn|G])).

parent(lightIsOn(LC,R,J,G),turnLightOn(LC,R,J,[lightIsOn|G])).

parent(lightIsOff(LC,R,J,G),turnLightOff(LC,R,J,[lightIsOff|G])).



%
%  Occupant Experience Agent
%


parent(rootGoal(controlHouse),lightSwitchRequestRespondedTo(_,_)).
parent(rootGoal(controlHouse),lightScheduleActuated(_,_)).


goal(lightScheduleActuated(_,_)).
goal(lightSwitchRequestRespondedTo(_,_)).
	goal(haveLightOff(_,_,_,_)).
	goal(haveLightOn(_,_,_,_)).

parent(lightSwitchRequestRespondedTo(OEA,R),haveLightOff(OEA,R,_,[lightSwitchRequestRespondedTo])).
parent(lightSwitchRequestRespondedTo(OEA,R),haveLightOn(OEA,R,_,[lightSwitchRequestRespondedTo])).

parent(lightScheduleActuated(OEA,R),haveLightOff(OEA,R,_,[lightScheduleActuated])).
parent(lightScheduleActuated(OEA,R),haveLightOn(OEA,R,_,[lightScheduleActuated])).


dependency(OEA,LC,
           haveLightOn(OEA,R,J,G),lightIsOn(LC,R,J,[haveLightOn|G])):-
		   occupantExperienceAgent(OEA),lightController(LC).

dependency(OEA,LC,
           haveLightOff(OEA,R,J,G),lightIsOff(LC,R,J,[haveLightOff|G])):-
		   occupantExperienceAgent(OEA),lightController(LC).



% Contributions
contr(foo,bar,plus).




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
% Main
%

proc(main,
	eeaAct:oeaAct:ssaAct
).



%
% Security and Safety Agent
%

proc(ssaAct,
	unexpectedMotionRespondedTo(ssa)
).


proc(unexpectedMotionRespondedTo(SSA),
	intruderDeterenceActuated(SSA,eraseme) # idle(SSA,unexpectedMotionRespondedTo)
).


proc(intruderDeterenceActuated(SSA,J),
	outsideLightsOn(SSA,J,[intruderDeterenceActuated]) : occupancyLightsOn(SSA,J,[intruderDeterenceActuated])
).

proc(outsideLightsOn(SSA,J,G),
	pi(lc, ?(lightController(lc)) : delegate(SSA,lc,outsideLightsOn(SSA,J,G),outsideLightsSwitchedOn(lc,J,[outsideLightsOn|G])):outsideLightsSwitchedOn(lc,J,[outsideLightsOn|G]) )
).

proc(occupancyLightsOn(SSA,J,G),
	pi(lc, ?(lightController(lc)) : delegate(SSA,lc,occupancyLightsOn(SSA,J,G),occupancyLightsSwitchedOn(lc,J,[occupancyLightsOn|G])):occupancyLightsSwitchedOn(lc,J,[occupancyLightsOn|G]))
).



%
% Energy Efficiency Agent
%

proc(eeaAct,
	turnUnneededLightsOff(eea)
).

proc(turnUnneededLightsOff(EEA),
	while( some(r, area(r) & lightOn(r) & -occupied(r)),
		pi(r, ?(area(r) & lightOn(r) & -occupied(r)) : haveLightTurnedOff(EEA,r,forgetit,[turnUnneededLightsOff]))
	)
).

proc(haveLightTurnedOff(EEA,R,J,G),
	pi(lc, ?(lightController(lc)) : delegate(EEA,lc,haveLightTurnedOff(EEA,R,J,G),lightIsOff(lc,R,J,[haveLightTurnedOff|G])):lightIsOff(lc,R,J,[haveLightTurnedOff|G]))
).


%
% Occupant Experience Agent
%


proc(oeaAct,
	lightSwitchRequestRespondedTo(oea,kitchen) # lightScheduleActuated(oea,kitchen)
).

proc(lightScheduleActuated(OEA,R), haveLightOn(OEA,R,somejustif,[lightScheduleActuated]) # haveLightOff(OEA,R,somejustif,[lightScheduleActuated])
).

proc(lightSwitchRequestRespondedTo(OEA,R), haveLightOn(OEA,R,somejustif,[lightSwitchRequestRespondedTo]) # haveLightOff(OEA,R,somejustif,[lightSwitchRequestRespondedTo])
).


proc(haveLightOn(OEA,R,J,G),
	pi(lc, ?(lightController(lc)) : delegate(OEA,lc,haveLightOn(OEA,R,J,G),lightIsOn(lc,R,J,[haveLightOn|G])):lightIs_On(lc,R,J,G) )
).

proc(lightIs_On(A,R,J,G),lightIsOn(A,R,J,[haveLightOn|G])).



proc(haveLightOff(OEA,R,J,G),
	pi(lc, ?(lightController(lc)) : delegate(OEA,lc,haveLightOff(OEA,R,J,G),lightIsOff(lc,R,J,[haveLightOff|G])):lightIs_Off(lc,R,J,G) )
).

proc(lightIs_Off(A,R,J,G),lightIsOff(A,R,J,[haveLightOff|G])).





%
% Light Controller
%


proc(occupancyLightsSwitchedOn(LC,J,G),
	while( some(r, occupancyRoom(r) & (-sat_lightIsOn(LC,r,_,_))),
		  pi(r, ?(occupancyRoom(r) & -sat_lightIsOn(LC,r,_,_)) : lightIsOn(LC,r,J,[occupancyLightsSwitchedOn|G]))
		  )
).

proc(outsideLightsSwitchedOn(LC,J,G),
	while( some(r, exteriorArea(r) & (-sat_lightIsOn(LC,r,_,_))),
		  pi(r, ?(exteriorArea(r) & -sat_lightIsOn(LC,r,_,_)) : lightIsOn(LC,r,J,[outsideLightsSwitchedOn|G]))
		  )
).


proc(lightIsOn(LC,R,J,G),
	turnLightOn(LC,R,J,[lightIsOn|G])).

proc(lightIsOff(LC,R,J,G),
	turnLightOff(LC,R,J,[lightIsOff|G])).



%
% ACTION PRECONDITION AXIOMS
%

poss(foo,S). % RESERVED
poss(delegate(_,_,_,_),S). % RESERVED
poss(idle(_,_),S).

poss(turnLightOn(_,R,_,_),S) :- lightOff(R,S).
poss(turnLightOff(_,R,_,_),S) :- lightOn(R,S).


%
% SATISFACTION AXIOMS
%

sat_turnUnneededLightsOff(EEA,S):- area(R), (occupied(R);lightOff(R,S)).

sat_haveLightTurnedOff(EEA,R,J,G,S):-sat_lightIsOff(_,R,J,[haveLightTurnedOff|G],S),energyEfficiencyAgent(EEA).

sat_haveLightOn(OEA,R,J,G,S) :- sat_lightIsOn(_,R,J,[haveLightOn|G],S),occupantExperienceAgent(OEA).
sat_haveLightOff(OEA,R,J,G,S) :- sat_lightIsOff(_,R,J,[haveLightOff|G],S),occupantExperienceAgent(OEA).

sat_lightSwitchRequestRespondedTo(OEA,R,S):- sat_haveLightOn(OEA,R,_,[lightSwitchRequestRespondedTo|_],S).
sat_lightSwitchRequestRespondedTo(OEA,R,S):- sat_haveLightOff(OEA,R,_,[lightSwitchRequestRespondedTo|_],S).

sat_lightScheduleActuated(OEA,R,S):- sat_haveLightOn(OEA,R,_,[lightScheduleActuated|_],S).
sat_lightScheduleActuated(OEA,R,S):- sat_haveLightOff(OEA,R,_,[lightScheduleActuated|_],S).


sat_lightIsOn(LC,R,J,G,S) :- perf_turnLightOn(LC,R,J,[lightIsOn|G],S).





sat_lightIsOff(LC,R,J,G,S) :- perf_turnLightOff(LC,R,J,[lightIsOff|G],S).
lightOff(R,S) :- perf_turnLightOff(LC,R,_,_,S),lightController(LC),area(R).

sat_occupancyLightsSwitchedOn(LC,J,G,S):- \+ ((occupancyRoom(R),lightOff(R,S))).
sat_outsideLightsSwitchedOn(LC,J,G,S):- \+ ((exteriorArea(R),lightOff(R,S))).

sat_outsideLightsOn(SSA,J,G,S) :- sat_outsideLightsSwitchedOn(LC,J,G,S), lightController(LC), securityAndSafetyAgent(SSA).
sat_occupancyLightsOn(SSA,J,G,S) :- sat_occupancyLightsSwitchedOn(LC,J,G,S), lightController(LC), securityAndSafetyAgent(SSA).


sat_intruderDeterenceActuated(SSA,J,S):- sat_outsideLightsOn(SSA,J,_,S), sat_occupancyLightsOn(SSA,J,_,S).

sat_unexpectedMotionRespondedTo(SSA,S) :- sat_intruderDeterenceActuated(SSA,_,S);perf_idle(SSA,unexpectedMotionRespondedTo,S).


%
% FEASIBILITY AXIOMS
%


%
% SUCCESSOR STATE AXIOMS
%

% RESERVED
hasDelegated(Delegator,Delegatee,DelegatorGoal,DelegateeGoal,do(A,S)):- 
									hasDelegated(Delegator,Delegatee,DelegatorGoal,DelegateeGoal,S);
									A = delegate(Delegator,Delegatee,DelegatorGoal,DelegateeGoal).


perf_turnLightOn(LC,R,J,G,do(A,S)):-perf_turnLightOn(LC,R,J,G,S);
									A = turnLightOn(LC,R,J,G).

perf_turnLightOff(LC,R,J,G,do(A,S)):-perf_turnLightOff(LC,R,J,G,S);
									A = turnLightOff(LC,R,J,G).

perf_idle(A,I,do(A,S)):-perf_idle(A,I,S);
									A = idle(A,I).

lightOn(R,do(A,S)):- (lightOn(R,S), A \= turnLightOff(_,R,_,_));
						(lightOff(R,S), A = turnLightOn(_,R,_,_)).

lightOff(R,do(A,S)):- (lightOff(R,S), A \= turnLightOn(_,R,_,_));
						(lightOn(R,S), A = turnLightOff(_,R,_,_)).



%lightOn(R,S) :- perf_turnLightOn(LC,R,_,_,S),lightController(LC),area(R).
%lightOff(R,S) :- perf_turnLightOff(LC,R,_,_,S),lightController(LC),area(R).

%
% Argument Restoration - Helpers
%


% Reserved
restoreSitArg(hasDelegated(Delegator,Delegatee,DelegatorGoal,DelegateeGoal),S,hasDelegated(Delegator,Delegatee,DelegatorGoal,DelegateeGoal,S)).

% Ordinary Fluents


% Task Fluents

restoreSitArg(perf_turnLightOn(LC,R,J,G),S,perf_turnLightOn(LC,R,J,G,S)).
restoreSitArg(perf_turnLightOff(LC,R,J,G),S,perf_turnLightOff(LC,R,J,G,S)).
restoreSitArg(perf_idle(A,I),S,perf_idle(A,I,S)).


restoreSitArg(sat_turnUnneededLightsOff(EEA),S,sat_turnUnneededLightsOff(EEA,S)).

restoreSitArg(sat_haveLightTurnedOff(EEA,R,J,G),S,sat_haveLightTurnedOff(EEA,R,J,G,S)).

restoreSitArg(sat_haveLightOn(LC,R,J,G),S,sat_haveLightOn(LC,R,J,G,S)).
restoreSitArg(sat_haveLightOff(LC,R,J,G),S,sat_haveLightOff(LC,R,J,G,S)).

restoreSitArg(sat_lightIsOn(LC,R,J,G),S,sat_lightIsOn(LC,R,J,G,S)).
restoreSitArg(sat_lightIsOff(LC,R,J,G),S,sat_lightIsOff(LC,R,J,G,S)).

restoreSitArg(lightOn(R),S,lightOn(R,S)).
restoreSitArg(lightOff(R),S,lightOff(R,S)).


restoreSitArg(sat_occupancyLightsSwitchedOn(LC,J,G),S,sat_occupancyLightsSwitchedOn(LC,J,G,S)).
restoreSitArg(sat_outsideLightsSwitchedOn(LC,J,G),S,sat_outsideLightsSwitchedOn(LC,J,G,S)).

restoreSitArg(sat_occupancyLightsOn(SSA,J,G),S,sat_occupancyLightsOn(SSA,J,G,S)).
restoreSitArg(sat_outsideLightsOn(SSA,J,G),S,sat_outsideLightsOn(SSA,J,G,S)).
restoreSitArg(sat_intruderDeterenceActuated(SSA,J),S,sat_intruderDeterenceActuated(SSA,J,S)).
restoreSitArg(unexpectedMotionRespondedTo(SSA),S,unexpectedMotionRespondedTo(SSA,S)).

restoreSitArg(sat_lightSwitchRequestRespondedTo(OEA,R),S,sat_lightSwitchRequestRespondedTo(OEA,R,S)).
restoreSitArg(sat_lightScheduleActuated(OEA,R),S,sat_lightScheduleActuated(OEA,R,S)).




%
% User friendly descriptions
%

descr2(X,Y):- descr(X,D),term_string(X,Xt),atomic_list_concat([D,' (',Xt,')'],Y).


%descr(decideMeetingDetails(X),Y):- 
%		term_string(X,Xs),string_concat(Xs,' wanted to decide meeting details.',Y).
descr(lightIsOn(LC,R,_,_),Y):-
		atomic_list_concat(["lights controller ",LC," wanted to have lights on in room ",R],Y),!.
descr(lightIsOff(LC,R,_,_),Y):-
		atomic_list_concat(["lights controller ",LC," wanted to have lights off in room ",R],Y),!.
descr(outsideLightsSwitchedOn(LC,_,_),Y):- 
		atomic_list_concat(["lights controller ",LC," wanted to have all outside lights turn on"],Y),!.
descr(occupancyLightsSwitchedOn(LC,_,_),Y):- 
		atomic_list_concat(["lights controller ",LC," wanted to have all interior occupancy outside lights turn on"],Y),!.
descr(haveLightOn(OEA,R,_,_),Y):-
		atomic_list_concat(["occupant experience agent ",OEA," wanted to have lights in room ",R, " on"],Y),!.
descr(haveLightOff(OEA,R,_,_),Y):-
		atomic_list_concat(["occupant experience agent ",OEA," wanted to have lights in room ",R, " off"],Y),!.
descr(lightSwitchRequestRespondedTo(OEA,R),Y):-
		atomic_list_concat(["occupant experience agent ",OEA," wanted to respond to an occupant request"],Y),!.
descr(lightScheduleActuated(OEA,R),Y):-
		atomic_list_concat(["occupant experience agent ",OEA," wanted to actuate a scheduled event"],Y),!.

descr(outsideLightsOn(SSA,_,_),Y):- 
		atomic_list_concat(["security and safety agent ",SSA," wanted to have all outside lights turn on"],Y),!.
descr(occupancyLightsOn(SSA,_,_),Y):- 
		atomic_list_concat(["security and safety agent ",SSA," wanted to have all interior occupancy outside lights turn on"],Y),!.
descr(intruderDeterenceActuated(SSA,_),Y):- 
		atomic_list_concat(["security and safety agent ",SSA," had intruder deterence measures actuated"],Y),!.
descr(unexpectedMotionRespondedTo(SSA),Y):- 
		atomic_list_concat(["security and safety agent ",SSA," wanted to respond to an unexpected motion that was detected"],Y),!.



descr(turnUnneededLightsOff(EEA),Y):- 
		atomic_list_concat(["energy efficiency agent ",EEA," wanted to turn lights off in areas of no occupancy."],Y),!.
descr(haveLightTurnedOff(EEA,R,_,_),Y):- 
		atomic_list_concat(["energy efficiency agent ",EEA," wanted to turn light off in ", R, " on account of it not being occupied."],Y),!.




descr(X,X).



 %#######                                                                    #######                                                        
 %#       #    # #####  #        ##   #    #   ##   ##### #  ####  #    #       #    ###### #    # #####  #        ##   ##### ######  ####  
 %#        #  #  #    # #       #  #  ##   #  #  #    #   # #    # ##   #       #    #      ##  ## #    # #       #  #    #   #      #      
 %#####     ##   #    # #      #    # # #  # #    #   #   # #    # # #  #       #    #####  # ## # #    # #      #    #   #   #####   ####  
 %#         ##   #####  #      ###### #  # # ######   #   # #    # #  # #       #    #      #    # #####  #      ######   #   #           # 
 %#        #  #  #      #      #    # #   ## #    #   #   # #    # #   ##       #    #      #    # #      #      #    #   #   #      #    # 
 %####### #    # #      ###### #    # #    # #    #   #   #  ####  #    #       #    ###### #    # #      ###### #    #   #   ######  ####  

explains_templ(_,_,_,_):-fail.

