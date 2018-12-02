/************************************************************
    Program:  process_events.i
    Created:  TN    4 Sep 99
Description:

Last change:  TN    4 Sep 99    4:40 pm
************************************************************/

/* Bruker avbryter. */
PROCESS EVENTS.
if wAvbryt then
  do:
    message "Skal overf›ring stoppes? " view-as alert-box buttons YES-NO
             set wStop.
    if wStop = true then
      leave {&BlokkLabel}.
  end.

