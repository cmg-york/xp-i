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

% Agents
lightController(lc).
occupantExperienceAgent(oea).
energyEfficiencyAgent(eea).
securityAndSafetyAgent(ssa).

%Rooms
rooms([kitchen, hallway, bedroom1, bedroom2, familyRoom, study, staircase]).
occupancyRooms([livingRoom, bedroom1]).
exteriorAreas([frontYard,backYard]).



%Events
onRequested([kitchen,bedroom2]).
offRequested([hallway]).
mustBeOns([hallway,familyRoom]).
mustBeOffs([frontYard]).
occupieds([]).

%Initial State

lightOffs([livingRoom, bedroom1, frontYard, bedroom2, familyRoom]).
lightOns([hallway, kitchen, backYard, study, staircase]).

%lightOff(livingRoom,s0). lightOff(bedroom1,s0). lightOff(frontYard,s0). lightOff(bedroom2,s0). lightOff(familyRoom,s0).
%lightOn(hallway,s0). lightOn(kitchen,s0). lightOn(backYard,s0). lightOn(study,s0). lightOn(staircase,s0). 

energySaverActivated.
isAfterHours.
motionDetected(frontYard).



%
% Rules and helpers
%
room(X) :- rooms(R),member(X,R).
occupancyRoom(X) :- occupancyRooms(R),member(X,R).
exteriorArea(X) :- exteriorAreas(R),member(X,R).
onRequest(X) :- onRequested(R),member(X,R).
offRequest(X) :- offRequested(R),member(X,R).
mustBeOn(X) :- mustBeOns(R),member(X,R).
mustBeOff(X) :- mustBeOffs(R),member(X,R).
occupied(X) :- occupieds(R),member(X,R).

lightOff(X,s0):- lightOffs(R),member(X,R).
lightOn(X,s0):- lightOns(R),member(X,R).

room(X):- occupancyRoom(X).
area(X):- room(X);exteriorArea(X).

% Conditions (shorthands)

intruderDeterenceCondition :- motionDetected(X),exteriorArea(X),isAfterHours.
intruderDeterenceCondition(R) :- intruderDeterenceCondition, (occupancyRoom(R);exteriorArea(R)).

intruderDeterenceActuatedJustitication(" motion was detected in an exterior area and it is after hours ").
intruderDeterenceNotActuatedJustitication(" motion was not detected in an exterior area during late hours ").

eeTurnOffCondition(R,S):- area(R), lightOn(R,S),!,\+ occupied(R),\+ onRequest(R).
eeTurnOffJustification(" room not occupied and energy saver program is activated ").

scheduleLightOffCondition(R,S):- lightOn(R,S), mustBeOff(R), !, \+ onRquest(R),\+ intruderDeterenceCondition(R).
scheduleLightOnJustification(" room's light is on, schedule says it should be off, no occupant has requested it on and no intruder detection has been activated in which the room is part of ").

scheduleLightOnCondition(R,S):- lightOff(R,S), mustBeOn(R).
scheduleLightOnJustification(" room's light is off and schedule says it should be on ").

requestLightOffCondition(R,S):- lightOn(R,S), offRequest(R),!,\+ (intruderDeterenceCondition(R)).
requestLightOffJustification(" room's light is on, there is an off request and no intruder detection has been activated which demands the light to be on ").







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
	goal(intruderDeterenceActuated(_)).
		goal(allDoorsAndWindowsClosedAndLocked(_)).
				goal(haveAllDoorsAndWindowsClosedActuated(_)).
				goal(haveAllDoorsAndWindowsLockedActuated(_)).
		goal(outsideLightsOn(_,_)).
		goal(occupancyLightsOn(_,_)).


or_node(homeProtectedDuringStorm(_)).
or_node(unexpectedMotionRespondedTo(_)).
or_node(protectElectricalSystem(_)).



parent(rootGoal(controlHouse),homeProtectedDuringStorm(_)).
parent(rootGoal(controlHouse),unexpectedMotionRespondedTo(_)).

parent(homeProtectedDuringStorm(SSA),stormProtectionActuated(SSA)).
parent(homeProtectedDuringStorm(SSA),idle(SSA)).

parent(stormProtectionActuated(SSA,J),protecteElectricalSystem(SSA)).
parent(stormProtectionActuated(SSA,J),haveAllDoorsAndWindowsClosed(SSA,stormProtectionActuated)).

parent(protecteElectricalSystem(SSA,J),powerOffRequested(SSA,J)).
parent(protecteElectricalSystem(SSA,J),idle(SSA,J)).



parent(unexpectedMotionRespondedTo(SSA),intruderDeterenceActuated(SSA)).
parent(unexpectedMotionRespondedTo(SSA),idle(SSA,unexpectedMotionRespondedTo)).

parent(intruderDeterenceActuated(SSA),allDoorsAndWindowsClosedAndLocked(SSA,[intruderDeterenceActuated])).
parent(intruderDeterenceActuated(SSA),outsideLightsOn(SSA,[intruderDeterenceActuated])).
parent(intruderDeterenceActuated(SSA),occupancyLightsOn(SSA,[intruderDeterenceActuated])).

parent(allDoorsAndWindowsClosedAndLocked(SSA,G),haveAllDoorsAndWindowsClosed(SSA,G)).
parent(allDoorsAndWindowsClosedAndLocked(SSA,G),haveAllDoorsAndWindowsLockedActuated(SSA,G)).

dependency(SSA,EPC,
           powerOffRequested(SSA,J),cutPowerOff(EPC,SSA)):-
		   securityAndSafetyAgent(SSA),electricalPowerController(EPC).

dependency(SSA,DWC,
           haveAllDoorsAndWindowsClosed(SSA,G),allDoorsAndWindowsClosed(DWC,G)):-
		   securityAndSafetyAgent(SSA),doorAndWindowController(DWC).

dependency(SSA,DWC,
           haveAllDoorsAndWindowsClosed(SSA,G),allDoorsAndWindowsClosed(DWC,G)):-
		   securityAndSafetyAgent(SSA),doorAndWindowController(DWC).

dependency(SSA,LC,
           outsideLightsOn(SSA,G),outsideLightsSwitchedOn(LC,[outsideLightsOn|G])):-
		   securityAndSafetyAgent(SSA),lightController(LC).

dependency(SSA,LC, occupancyLightsOn(SSA,G),occupancyLightsSwitchedOn(LC,[occupancyLightsOn|G])):-
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

parent(allDoorsAndWindowsClosed(DWC,G),haveAllDoorsClosed(DWC,G)).
parent(allDoorsAndWindowsClosed(DWC,G),haveAllWindowsClosed(DWC,G)).

parent(haveAllDoorsClosed(DWC,G),closeDoor(DWC,G,_)).
parent(haveAllWindowsClosed(DWC,G),closeWindow(DWC,G,_)).

parent(allDoorsAndWindowsLocked(DWC,G),haveAllDoorsLocked(DWC,G)).
parent(allDoorsAndWindowsLocked(DWC,G),haveAllWindowsLocked(DWC,G)).

parent(haveAllDoorsLocked(DWC,G),lockDoor(DWC,G,_)).
parent(haveAllWindowsLocked(DWC,G),lockWindow(DWC,G,_)).


%
%  Energy Efficiency Agent
%

goal(turnUnneededLightsOff(_)).
	goal(haveLightTurnedOff(_,_,_)).

parent(rootGoal(controlHouse),turnUnneededLightsOff(EAA)).
parent(turnUnneededLightsOff(EEA),haveLightTurnedOff(EEA,_,_)).


dependency(EEA,LC,haveLightTurnedOff(EEA,R,G),lightIsOff(LC,R,[haveLightTurnedOff|G])):-
		   energyEfficiencyAgent(EEA),lightController(LC).


%
%  Light Controller
%

goal(outsideLightsSwitchedOn(_,_)).
goal(occupancyLightsSwitchedOn(_,_)).
	goal(lightIsOn(_,_,_)).
		task(turnLightOn(_,_,_)).

goal(lightIsOff(_,_,_)).
	task(turnLightOff(_,_,_)).

parent(outsideLightsSwitchedOn(LC,G),lightIsOn(LC,R,[outsideLightsSwitchedOn|G])).
parent(occupancyLightsSwitchedOn(LC,G),lightIsOn(LC,R,[occupancyLightsSwitchedOn|G])).

parent(lightIsOn(LC,R,G),turnLightOn(LC,R,[lightIsOn|G])).

parent(lightIsOff(LC,R,G),turnLightOff(LC,R,[lightIsOff|G])).



%
%  Occupant Experience Agent
%


parent(rootGoal(controlHouse),lightSwitchRequestsProcessed(_)).
parent(rootGoal(controlHouse),lightScheduledEventsProcessed(_)).


goal(lightScheduleActuated(_,_)).
goal(lightSwitchRequestRespondedTo(_,_)).
	goal(haveLightOff(_,_,_)).
	goal(haveLightOn(_,_,_)).

parent(lightSwitchRequestRespondedTo(OEA,R),haveLightOff(OEA,R,[lightSwitchRequestRespondedTo])).
parent(lightSwitchRequestRespondedTo(OEA,R),haveLightOn(OEA,R,[lightSwitchRequestRespondedTo])).

parent(lightScheduleActuated(OEA,R),haveLightOff(OEA,R,[lightScheduleActuated])).
parent(lightScheduleActuated(OEA,R),haveLightOn(OEA,R,[lightScheduleActuated])).

goal(lightSwitchRequestsProcessed(_)).
	goal(lightOnRequestsProcessed(_)).
	goal(lightOffRequestsProcessed(_)).
	
goal(lightScheduledEventsProcessed(_)).
	goal(lightOnScheduledEventsProcessed(_)).
	goal(lightOffScheduledEventsProcessed(_)).
	
	
	
parent(rootGoal(controlHouse),lightSwitchRequestsProcessed(_)).
parent(lightSwitchRequestsProcessed(OEA),lightOnRequestsProcessed(OEA)).
parent(lightSwitchRequestsProcessed(OEA),lightOffRequestsProcessed(OEA)).

parent(rootGoal(controlHouse),lightScheduleEventsProcessed(_)).
parent(lightScheduledEventsProcessed(OEA),lightOnScheduledEventsProcessed(OEA)).
parent(lightScheduledEventsProcessed(OEA),lightOffScheduledEventsProcessed(OEA)).


parent(lightOffScheduledEventsProcessed(OEA),haveLightOff(OEA,_,[lightOffScheduledEventsProcessed])).
parent(lightOnScheduledEventsProcessed(OEA),haveLightOn(OEA,_,[lightOnScheduledEventsProcessed])).

parent(lightOffRequestsProcessed(OEA),haveLightOff(OEA,_,[lightOffRequestsProcessed])).
parent(lightOnRequestsProcessed(OEA),haveLightOn(OEA,_,[lightOnRequestsProcessed])).



dependency(OEA,LC,
           haveLightOn(OEA,R,G),lightIsOn(LC,R,[haveLightOn|G])):-
		   occupantExperienceAgent(OEA),lightController(LC).

dependency(OEA,LC,
           haveLightOff(OEA,R,G),lightIsOff(LC,R,[haveLightOff|G])):-
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
	ssaAct:oeaAct:eeaAct
).

bulb(off).

proc(something,
	?(bulb(on)):idle(yo,you) # idle(ya,yo)
).


%
% Security and Safety Agent
%

proc(ssaAct,
	unexpectedMotionRespondedTo(ssa)
).


proc(unexpectedMotionRespondedTo(SSA),
	?(intruderDeterenceCondition): intruderDeterenceActuated(SSA) # ?(-intruderDeterenceCondition):idle(SSA,unexpectedMotionRespondedTo)
).


proc(intruderDeterenceActuated(SSA),
	outsideLightsOn(SSA,[intruderDeterenceActuated]) : occupancyLightsOn(SSA,[intruderDeterenceActuated])
).

proc(outsideLightsOn(SSA,G),
	pi(lc, ?(lightController(lc)) : delegate(SSA,lc,outsideLightsOn(SSA,G),outsideLightsSwitchedOn(lc,[outsideLightsOn|G])):outsideLightsSwitchedOn(lc,[outsideLightsOn|G]) )
).

proc(occupancyLightsOn(SSA,G),
	pi(lc, ?(lightController(lc)) : delegate(SSA,lc,occupancyLightsOn(SSA,G),occupancyLightsSwitchedOn(lc,[occupancyLightsOn|G])):occupancyLightsSwitchedOn(lc,[occupancyLightsOn|G]))
).



%
% Energy Efficiency Agent
%

proc(eeaAct,
	turnUnneededLightsOff(eea)
).

proc(turnUnneededLightsOff(EEA),
	while( some(r, area(r) & lightOn(r) & -occupied(r) & -onRequest(r) & -intruderDeterenceCondition(r) & -mustBeOn(r)),
		pi(r, ?(area(r) & lightOn(r) & -occupied(r) & -onRequest(r) & -intruderDeterenceCondition(r) & -mustBeOn(r)) : haveLightTurnedOff(EEA,r,[turnUnneededLightsOff]))
	)
).

proc(haveLightTurnedOff(EEA,R,G),
	pi(lc, ?(lightController(lc)) : delegate(EEA,lc,haveLightTurnedOff(EEA,R,G),lightIsOff(lc,R,[haveLightTurnedOff|G])):lightIsOff(lc,R,[haveLightTurnedOff|G]))
).


%
% Occupant Experience Agent
%

proc(oeaAct,
	lightScheduledEventsProcessed(oea) : lightSwitchRequestsProcessed(oea)
).

proc(lightScheduledEventsProcessed(OEA),
	lightOnScheduledEventsProcessed(OEA) : lightOffScheduledEventsProcessed(OEA)
).

proc(lightSwitchRequestsProcessed(OEA),
	 lightOnRequestsProcessed(OEA)  : lightOffRequestsProcessed(OEA)
).

proc(lightOnScheduledEventsProcessed(OEA),
	while( some(r, area(r) & lightOff(r) & mustBeOn(r) & -offRequest(r)),
		pi(r, ?(area(r) & lightOff(r) & mustBeOn(r) & -offRequest(r)) : haveLightOn(OEA,R,[lightOnScheduledEventsProcessed]))
	)
).


% TODO: this does not work for whatever reason!!!!
% do(haveLightOff(oea,staircase,[lightOffScheduledEventsProcessed]):lightOnRequestsProcessed(oea)).
proc(lightOnRequestsProcessed(OEA),
	while( some(r, area(r) & lightOff(r) & onRequest(r)),
		pi(r, ?(area(r) & lightOff(r) & onRequest(r)) : haveLightOn(OEA,R,[lightOnRequestsProcessed]))
	)
).

% Conditional AND Decomposition
proc(meetingAnnounced(C), 
	while( some(p, potential_participant(p) & (-perf_announceMeeting(C,p))),
		  pi(p, ?(potential_participant(p) & -perf_announceMeeting(C,p)) : announceMeeting(C,p))
		  )
).


proc(lightOffScheduledEventsProcessed(OEA),
	while( some(r, area(r) & lightOn(r) & mustBeOff(r) & -onRequest(r) & -intruderDeterenceCondition(r)),
		pi(r, ?(area(r) & lightOn(r) & mustBeOff(r) & -onRequest(r) & -intruderDeterenceCondition(r)) : haveLightOff(OEA,R,[lightOffScheduledEventsProcessed]))
	)
).


proc(lightOffRequestsProcessed(OEA),
	while( some(r, area(r) & lightOn(r) & offRequest(r) & -intruderDeterenceCondition(r)),
		pi(r, ?(area(r) & lightOn(r) & offRequest(r) & -intruderDeterenceCondition(r)) : haveLightOff(OEA,R,[lightOffRequestsProcessed]))
	)
).


%proc(oeaAct,
%	lightSwitchRequestRespondedTo(oea,kitchen) # lightScheduleActuated(oea,kitchen)
%).

proc(lightScheduleActuated(OEA,R), haveLightOn(OEA,R,[lightScheduleActuated]) # haveLightOff(OEA,R,[lightScheduleActuated])
).

proc(lightSwitchRequestRespondedTo(OEA,R), haveLightOn(OEA,R,[lightSwitchRequestRespondedTo]) # haveLightOff(OEA,R,[lightSwitchRequestRespondedTo])
).


proc(haveLightOn(OEA,R,G),
	pi(lc, ?(lightController(lc)) : delegate(OEA,lc,haveLightOn(OEA,R,G),lightIsOn(lc,R,[haveLightOn|G])):lightIs_On(lc,R,G) )
).

proc(lightIs_On(A,R,G),lightIsOn(A,R,[haveLightOn|G])).



proc(haveLightOff(OEA,R,G),
	pi(lc, ?(lightController(lc)) : delegate(OEA,lc,haveLightOff(OEA,R,G),lightIsOff(lc,R,[haveLightOff|G])):lightIs_Off(lc,R,G) )
).

proc(lightIs_Off(A,R,G),lightIsOff(A,R,[haveLightOff|G])).





%
% Light Controller
%


proc(occupancyLightsSwitchedOn(LC,G),
	while( some(r, occupancyRoom(r) & lightOff(r)),
		  pi(r, ?(occupancyRoom(r) & lightOff(r)) : lightIsOn(LC,r,[occupancyLightsSwitchedOn|G]))
		  )
).

proc(outsideLightsSwitchedOn(LC,G),
	while( some(r, exteriorArea(r) & lightOff(r)),
		  pi(r, ?(exteriorArea(r) & lightOff(r)) : lightIsOn(LC,r,[outsideLightsSwitchedOn|G]))
		  )
).


proc(lightIsOn(LC,R,G),
	turnLightOn(LC,R,[lightIsOn|G])).

proc(lightIsOff(LC,R,G),
	turnLightOff(LC,R,[lightIsOff|G])).



%
% ACTION PRECONDITION AXIOMS
%

poss(foo,S). % RESERVED
poss(delegate(_,_,_,_),S). % RESERVED
poss(idle(_,_),S).

poss(turnLightOn(_,R,_),S) :- lightOff(R,S).
poss(turnLightOff(_,R,_),S) :- lightOn(R,S).


%
% SATISFACTION AXIOMS
%

sat_turnUnneededLightsOff(EEA,S):- area(R), (occupied(R);lightOff(R,S)).

sat_haveLightTurnedOff(EEA,R,G,S):-sat_lightIsOff(_,R,[haveLightTurnedOff|G],S),energyEfficiencyAgent(EEA).

sat_haveLightOn(OEA,R,G,S) :- sat_lightIsOn(_,R,[haveLightOn|G],S),occupantExperienceAgent(OEA).
sat_haveLightOff(OEA,R,G,S) :- sat_lightIsOff(_,R,[haveLightOff|G],S),occupantExperienceAgent(OEA).

sat_lightSwitchRequestRespondedTo(OEA,R,S):- sat_haveLightOn(OEA,R,[lightSwitchRequestRespondedTo|_],S).
sat_lightSwitchRequestRespondedTo(OEA,R,S):- sat_haveLightOff(OEA,R,[lightSwitchRequestRespondedTo|_],S).

sat_lightScheduleActuated(OEA,R,S):- sat_haveLightOn(OEA,R,[lightScheduleActuated|_],S).
sat_lightScheduleActuated(OEA,R,S):- sat_haveLightOff(OEA,R,[lightScheduleActuated|_],S).


sat_lightSwitchRequestsProcessed(OEA,S):-sat_lightOnRequestsProcessed(OEA,S),sat_lightOffRequestsProcessed(OEA,S).
sat_lightOffRequestsProcessed(OEA,S):- \+ ((area(R),lightOn(R,S), offRequest(R), \+ intruderDeterenceCondition(R)), \+ sat_haveLightOff(OEA,R,[lightOffRequestsProcessed|_],S)).
sat_lightOnRequestsProcessed(OEA,S):- \+ ((area(R),lightOff(R,S),onRequest(R)), \+ sat_haveLightOn(OEA,R,[lightOnRequestsProcessed|_],S)).


%
% TODO - SHOULD be satisfied for ALL rooms that meet the contidion not just one. See above.
%
sat_lightScheduledEventsProcessed(OEA,S):- sat_lightOnScheduledEventsProcessed(OEA,S),sat_lightOffScheduledEventsProcessed(OEA,S).
sat_lightOnScheduledEventsProcessed(OEA,S):- \+ ((area(R), lightOff(R,S) , mustBeOn(R) , \+ offRequest(R)), \+ sat_haveLightOn(OEA,R,[lightOnScheduledEventsProcessed|_],S)).
sat_lightOffScheduledEventsProcessed(OEA,S):- \+ ((area(R),lightOn(R,S),mustBeOff(R),\+ onRequest(R), \+ intruderDeterenceCondition(R)),\+ sat_haveLightOff(OEA,R,[lightOffScheduledEventsProcessed|_],S)).
%sat_lightOnScheduledEventsProcessed(_,_).
%sat_lightOffScheduledEventsProcessed(_,_).

sat_lightIsOn(LC,R,G,S) :- perf_turnLightOn(LC,R,[lightIsOn|G],S).

sat_lightIsOff(LC,R,G,S) :- perf_turnLightOff(LC,R,[lightIsOff|G],S).
lightOff(R,S) :- perf_turnLightOff(LC,R,_,S),lightController(LC),area(R).

sat_occupancyLightsSwitchedOn(LC,G,S):- \+ ((occupancyRoom(R),lightOff(R,S))).
sat_outsideLightsSwitchedOn(LC,G,S):- \+ ((exteriorArea(R),lightOff(R,S))).

sat_outsideLightsOn(SSA,G,S) :- sat_outsideLightsSwitchedOn(LC,G,S), lightController(LC), securityAndSafetyAgent(SSA).
sat_occupancyLightsOn(SSA,G,S) :- sat_occupancyLightsSwitchedOn(LC,G,S), lightController(LC), securityAndSafetyAgent(SSA).


sat_intruderDeterenceActuated(SSA,S):- sat_outsideLightsOn(SSA,_,S), sat_occupancyLightsOn(SSA,_,S).

sat_unexpectedMotionRespondedTo(SSA,S) :- sat_intruderDeterenceActuated(SSA,S);perf_idle(SSA,unexpectedMotionRespondedTo,S).


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


perf_turnLightOn(LC,R,G,do(A,S)):-perf_turnLightOn(LC,R,G,S);
									A = turnLightOn(LC,R,G).

perf_turnLightOff(LC,R,G,do(A,S)):-perf_turnLightOff(LC,R,G,S);
									A = turnLightOff(LC,R,G).

perf_idle(A,I,do(A,S)):-perf_idle(A,I,S);
									A = idle(A,I).

lightOn(R,do(A,S)):- (lightOn(R,S), A \= turnLightOff(_,R,_));
						(lightOff(R,S), A = turnLightOn(_,R,_)).

lightOff(R,do(A,S)):- (lightOff(R,S), A \= turnLightOn(_,R,_));
						(lightOn(R,S), A = turnLightOff(_,R,_)).



%lightOn(R,S) :- perf_turnLightOn(LC,R,_,_,S),lightController(LC),area(R).
%lightOff(R,S) :- perf_turnLightOff(LC,R,_,_,S),lightController(LC),area(R).

%
% Argument Restoration - Helpers
%


% Reserved
restoreSitArg(hasDelegated(Delegator,Delegatee,DelegatorGoal,DelegateeGoal),S,hasDelegated(Delegator,Delegatee,DelegatorGoal,DelegateeGoal,S)).

% Ordinary Fluents


% Task Fluents

restoreSitArg(perf_turnLightOn(LC,R,G),S,perf_turnLightOn(LC,R,G,S)).
restoreSitArg(perf_turnLightOff(LC,R,G),S,perf_turnLightOff(LC,R,G,S)).
restoreSitArg(perf_idle(A,I),S,perf_idle(A,I,S)).


restoreSitArg(sat_turnUnneededLightsOff(EEA),S,sat_turnUnneededLightsOff(EEA,S)).

restoreSitArg(sat_haveLightTurnedOff(EEA,R,G),S,sat_haveLightTurnedOff(EEA,R,G,S)).

restoreSitArg(sat_haveLightOn(LC,R,G),S,sat_haveLightOn(LC,R,G,S)).
restoreSitArg(sat_haveLightOff(LC,R,G),S,sat_haveLightOff(LC,R,G,S)).

restoreSitArg(sat_lightIsOn(LC,R,G),S,sat_lightIsOn(LC,R,G,S)).
restoreSitArg(sat_lightIsOff(LC,R,G),S,sat_lightIsOff(LC,R,G,S)).

restoreSitArg(lightOn(R),S,lightOn(R,S)).
restoreSitArg(lightOff(R),S,lightOff(R,S)).


restoreSitArg(sat_occupancyLightsSwitchedOn(LC,G),S,sat_occupancyLightsSwitchedOn(LC,G,S)).
restoreSitArg(sat_outsideLightsSwitchedOn(LC,G),S,sat_outsideLightsSwitchedOn(LC,G,S)).

restoreSitArg(sat_occupancyLightsOn(SSA,G),S,sat_occupancyLightsOn(SSA,G,S)).
restoreSitArg(sat_outsideLightsOn(SSA,G),S,sat_outsideLightsOn(SSA,G,S)).
restoreSitArg(sat_intruderDeterenceActuated(SSA),S,sat_intruderDeterenceActuated(SSA,S)).
restoreSitArg(sat_unexpectedMotionRespondedTo(SSA),S,sat_unexpectedMotionRespondedTo(SSA,S)).



restoreSitArg(sat_lightSwitchRequestRespondedTo(OEA,R),S,sat_lightSwitchRequestRespondedTo(OEA,R,S)).
restoreSitArg(sat_lightScheduleActuated(OEA,R),S,sat_lightScheduleActuated(OEA,R,S)).


restoreSitArg(sat_lightOffRequestsProcessed(OEA),S,sat_lightOffRequestsProcessed(OEA,S)).
restoreSitArg(sat_lightOnRequestsProcessed(OEA),S,sat_lightOnRequestsProcessed(OEA,S)).
restoreSitArg(sat_lightSwitchRequestsProcessed(OEA),S,sat_lightSwitchRequestsProcessed(OEA,S)).

restoreSitArg(sat_lightOnScheduledEventsProcessed(OEA),S,sat_lightOnScheduledEventsProcessed(OEA,S)).
restoreSitArg(sat_lightOffScheduledEventsProcessed(OEA),S,sat_lightOffScheduledEventsProcessed(OEA,S)).
restoreSitArg(sat_lightScheduledEventsProcessed(OEA),S,sat_lightScheduledEventsProcessed(OEA,S)).




%
% User friendly descriptions
%

descr2(X,Y):- descr(X,D),term_string(X,Xt),atomic_list_concat([D,' (',Xt,')'],Y).


%descr(decideMeetingDetails(X),Y):- 
%		term_string(X,Xs),string_concat(Xs,' wanted to decide meeting details.',Y).
descr(lightIsOn(LC,R,_),Y):-
		atomic_list_concat(["lights controller ",LC," wanted to have lights on in room ",R],Y),!.
descr(lightIsOff(LC,R,_),Y):-
		atomic_list_concat(["lights controller ",LC," wanted to have lights off in room ",R],Y),!.
descr(outsideLightsSwitchedOn(LC,_),Y):- 
		atomic_list_concat(["lights controller ",LC," wanted to have all outside lights turn on"],Y),!.
descr(occupancyLightsSwitchedOn(LC,_),Y):- 
		atomic_list_concat(["lights controller ",LC," wanted to have all interior occupancy outside lights turn on"],Y),!.
descr(haveLightOn(OEA,R,_),Y):-
		atomic_list_concat(["occupant experience agent ",OEA," wanted to have lights in room ",R, " on"],Y),!.
descr(haveLightOff(OEA,R,_),Y):-
		atomic_list_concat(["occupant experience agent ",OEA," wanted to have lights in room ",R, " off"],Y),!.
descr(lightSwitchRequestRespondedTo(OEA,R),Y):-
		atomic_list_concat(["occupant experience agent ",OEA," wanted to respond to an occupant request"],Y),!.
descr(lightScheduleActuated(OEA,R),Y):-
		atomic_list_concat(["occupant experience agent ",OEA," wanted to actuate a scheduled event"],Y),!.
descr(lightOnScheduledEventsProcessed(OEA),Y):-
		atomic_list_concat(["occupant experience agent ",OEA," wanted to process all light on events in the schedule"],Y),!.
descr(lightOffScheduledEventsProcessed(OEA),Y):-
		atomic_list_concat(["occupant experience agent ",OEA," wanted to process all light off events in the schedule"],Y),!.
descr(lightOnRequestsProcessed(OEA),Y):-
		atomic_list_concat(["occupant experience agent ",OEA," wanted to process all light on occupant requests"],Y),!.
descr(lightOffRequestsProcessed(OEA),Y):-
		atomic_list_concat(["occupant experience agent ",OEA," wanted to process all light off occupant requests"],Y),!.
descr(lightScheduledEventsProcessed(OEA),Y):-
		atomic_list_concat(["occupant experience agent ",OEA," wanted to process the light switch events schedule"],Y),!.
descr(lightSwitchRequestsProcessed(OEA),Y):-
		atomic_list_concat(["occupant experience agent ",OEA," wanted to process the light switch requests of occupants"],Y),!.




descr(outsideLightsOn(SSA,_),Y):- 
		atomic_list_concat(["security and safety agent ",SSA," wanted to have all outside lights turn on"],Y),!.
descr(occupancyLightsOn(SSA,_),Y):- 
		atomic_list_concat(["security and safety agent ",SSA," wanted to have all interior occupancy outside lights turn on"],Y),!.
descr(intruderDeterenceActuated(SSA),Y):- 
		atomic_list_concat(["security and safety agent ",SSA," had intruder deterence measures actuated"],Y),!.
descr(unexpectedMotionRespondedTo(SSA),Y):- 
		atomic_list_concat(["security and safety agent ",SSA," wanted to respond to an unexpected motion that was detected"],Y),!.



descr(turnUnneededLightsOff(EEA),Y):- 
		atomic_list_concat(["energy efficiency agent ",EEA," wanted to turn lights off in areas of no occupancy."],Y),!.
descr(haveLightTurnedOff(EEA,R,_),Y):- 
		atomic_list_concat(["energy efficiency agent ",EEA," wanted to turn light off in ", R, " on account of it not being occupied."],Y),!.


descr(X,X).



 %#######                                                                    #######                                                        
 %#       #    # #####  #        ##   #    #   ##   ##### #  ####  #    #       #    ###### #    # #####  #        ##   ##### ######  ####  
 %#        #  #  #    # #       #  #  ##   #  #  #    #   # #    # ##   #       #    #      ##  ## #    # #       #  #    #   #      #      
 %#####     ##   #    # #      #    # # #  # #    #   #   # #    # # #  #       #    #####  # ## # #    # #      #    #   #   #####   ####  
 %#         ##   #####  #      ###### #  # # ######   #   # #    # #  # #       #    #      #    # #####  #      ######   #   #           # 
 %#        #  #  #      #      #    # #   ## #    #   #   # #    # #   ##       #    #      #    # #      #      #    #   #   #      #    # 
 %####### #    # #      ###### #    # #    # #    #   #   #  ####  #    #       #    ###### #    # #      ###### #    #   #   ######  ####  



explains_templ(Y,intruderDeterenceActuated(SSA),_,J) :- 
						intruderDeterenceCondition, intruderDeterenceActuatedJustitication(J), atomic_list_concat([" Security and Safety agent ", SSA ," activated intruder deterence with justification: ", J],Y).
explains_templ(Y,idle(SSA,unexpectedMotionRespondedTo),_,J) :- 
						\+ intruderDeterenceCondition, intruderDeterenceNotActuatedJustitication(J), atomic_list_concat([" Security and Safety agent ", SSA ," did not activate intruder deterence with justification: ", J],Y).
explains_templ(Y,haveLightTurnedOff(EEA,R,_),S,J) :- 
						 eeTurnOffCondition(R,S),eeTurnOffJustification(J),atomic_list_concat([" Energy efficiency agent ", EEA ," turns off the light in room ", R ," with justification: ", J],Y).