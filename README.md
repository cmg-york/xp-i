# Xp-i: extracting explanations from $i^+$ models.

Xp-i is a set of Prolog routines for extracting explanations from $i^+$ models. The latter need to be appropriately translated to a Golog-compatible specification. The toolset allows generation of random action histories from the $i^+$ models, and subsequently posing explanation questions with regards to each action included in the history. A manual and an interactive explanation extraction mode is currently supported.

The routines require Golog in order to produce simulated action histories. We have no premission to redistribute the Golog interpreter, but it can be downloaded from the [University of Toronto's Cognitive Robotics Lab pages](https://www.cs.toronto.edu/cogrobo/main/systems/index.html).


## Installation

1. Download or clone the src directory.
2. Download the Golog interpreter from [here](http://www.cs.toronto.edu/cogrobo/Systems/golog_swi.pl).
3. Place `golog_swi.pl` inside the src/golog directory.
4. Install [SWI Prolog](https://www.swi-prolog.org/) in your system.

## Examples (MS Windows)

The examples below use the Meeting Scheduling domain model. A graphical representation of the model can be found [here](https://github.com/anonymous-author-1000/xp-i/blob/main/src/domains/mtg.drawio) (select download and save as a DRAWIO file). File opens with [Draw.io](https://www.drawio.com/). Alternativelly you can view [the PDF version](https://github.com/anonymous-author-1000/xp-i/blob/main/src/domains/mtg.pdf)

### Manual mode

1. In command prompt, go to `src/domains/`
2. `swipl -l mtg.pl` to enter the SWI Prolog environment with the rules and domain loaded.
4. `do(main)` to obtain a simulated history. The command simply asks Golog to generate a situation for the top goal.
    - Press `;` to move to the next history or `.` to end.
    - The last history displayed will be the one that will be stored in the history buffer and on which explanations will be based.
5. `why([predicate])`, where `[predicate]` is any of the predicates appearing in the history currently in the buffer. This will produce explanations for `[predicate]`
    - Press `;` to move to the next explanation or `.` to end.
    
```
?- why(announceMeeting(matilda,xing)).
   Teleological Explanation: Because meetingAnnounced(matilda) (meetingAnnounced(matilda)) [operationalization (axiom 1)]
   true ;
   Canonical Explanation: Because organizer abdul wanted to have the meeting attended. (meetingAttended(abdul))  [enablement (axiom 4)]
   true ;
   Choice mandatory, no other feasible alternative. (...)  [only feasible (axiom 5)]
   true .
```    
    
6. Back to Step 5 for selecting a different explanandum.
7. Back to Step 4 for selecting a different history.

### Interactive mode
1. In command prompt, go to `src/domains/`
2. `swipl -l mtg.pl` to enter the SWI Prolog environment with the rules and domain loaded.
4. `do(main)` to obtain a simulated history. The command simply asks Golog to generate a situation for the top goal.
    - Press `;` to move to the next history or `.` to end.
    - The last history displayed will be the one that will be stored in the history buffer and on which explanations will be based.
5. `why_interactive([predicate])`, where `[predicate]` is any of the predicates appearing in the history currently in the buffer. This will produce a one-line list of all explanations for `[predicate]`. For example:


```
?- why_interactive(announceMeeting(matilda,xing)).

   Explanantia summary for announceMeeting(matilda,xing):
   1: meetingAnnounced(matilda) - operationalization (axiom 1)
   2: meetingAttended(abdul) - enablement (axiom 4)
   3: ... - only feasible (axiom 5)
   Explain (e to exit):
```
6. Select which explanation to adopt as the explanandum for the next round by entering the corresponding number following by a full-stop - e.g. `1.` for explaining `meetingAnnounced(matilda)` above.
7. Enter `e.` (notice the full-stop `.` at the end) to end the interactive loop.


Please send issues and bugs to [liaskos@yorku.ca](mailto:liaskos@yorku.ca?subject=Question about xp-i).
## Troubleshooting

## Additional resources
The accompanying paper can be found [here](https://github.com/anonymous-author-1000/xp-i/blob/main/doc/ER2024-Explain-Long.pdf)

