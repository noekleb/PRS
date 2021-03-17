/************************************************************
    Program:  finnjobb.p
    Created:  TN   24 May 98
Description:  finner f›rste ledige jobbnummmer fra
              sekvens JobbNr.

  TN   28/3-99  Laget loop rundt det † hente neste ledige
                jobbnummer.

Last change:  TN   25 Oct 100    3:47 pm
************************************************************/

DEF INPUT-OUTPUT PARAMETER wJobbNr  as INT NO-UNDO.
DEF INPUT-OUTPUT PARAMETER wDivData as CHAR NO-UNDO.
DEF       OUTPUT PARAMETER wStatus  as CHAR NO-UNDO.


LOOPEN:
do while true:
  ASSIGN
    wJobbNr = NEXT-VALUE(JobbNr).

  /* Ved eksport/import glemmes ofte eksport/import av sekvenser. */
  /* Dette fikser det.                                            */
  if not can-find(Jobb where Jobb.JobbNr = wJobbNr) then
    DO:
      /* Mangler det jobb-record i forhold til jobblinjer, skal nummeret */
      /* markeres osm opptatt.                                           */
      if CAN-FIND(FIRST JobbLinje WHERE JobbLinje.JobbNr = wJobbNr) then
        NEXT LOOPEN.

      /* Hurra - et ledig nummer. */
      leave LOOPEN.
    END.
end. /* LOOPEN */


IF wJobbNr <> ? then
  DO:
    wStatus = "Ok":U.
  END.
ELSE
  wStatus = "AVBRYT":U.

