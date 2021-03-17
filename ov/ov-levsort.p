/************************************************************
    Program:  ov-LevSort.p
    Created:  TN   13 Sep 99
Description:  Overf›ring av LevSort til sentral database.

              run ov-LevSort.p (input input RS-2).

Last change:  TN   17 Sep 99    9:02 pm
************************************************************/

DEF INPUT parameter wRS-2 as INT NO-UNDO.

DEF shared VAR wAvbryt as LOG INITIAL FALSE NO-UNDO.

DEF VAR wStop as LOG NO-UNDO.

DISABLE TRIGGERS FOR LOAD OF SentralDB.levsort.


ASSIGN wStop = FALSE.

LevSort:
for each SkoTex.LevSort no-lock:

  {process_events.i &BlokkLabel = "LevSort"}

  FIND SentralDB.LevSort OF SkoTex.LevSort exclusive-lock NO-ERROR.
  IF NOT AVAILABLE SentralDB.LevSort then
    DO:
      CREATE SentralDB.LevSort.
      BUFFER-COPY SkoTex.LevSort to SentralDB.LevSort.
    END.
  ELSE IF AVAILABLE SentralDB.LevSort then
    DO:
      IF wRS-2 = 2 then /* Bruker har kun valgt † oppdatere nye poster. */
        NEXT LevSort.
      BUFFER-COPY SkoTex.LevSort to SentralDB.LevSort.
    END.

end. /* LevSort */

IF wStop then
  RETURN "AVBRYT".
ELSE
  RETURN "OK".


