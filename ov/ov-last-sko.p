/************************************************************
    Program:  ov-Last-Sko.p
    Created:  TN   13 Sep 99
Description:  Overf›ring av Last-Sko til sentral database.

              run ov-Last-Sko.p (input input RS-1).

Last change:  TN   15 Sep 99    7:50 pm
************************************************************/

DEF INPUT parameter wRS-1 as INT NO-UNDO.

DEF shared VAR wAvbryt as LOG INITIAL FALSE NO-UNDO.

DEF VAR wStop as LOG NO-UNDO.

DISABLE TRIGGERS FOR LOAD OF SentralDB.last-sko.

ASSIGN wStop = FALSE.

Last-Sko:
for each SkoTex.Last-Sko no-lock:

  {process_events.i &BlokkLabel = "Last-Sko"}

  FIND SentralDB.Last-Sko OF SkoTex.Last-Sko exclusive-lock NO-ERROR.
  IF NOT AVAILABLE SentralDB.Last-Sko then
    DO:
      CREATE SentralDB.Last-Sko.
      BUFFER-COPY SkoTex.Last-Sko to SentralDB.Last-Sko.
    END.
  ELSE IF AVAILABLE SentralDB.Last-Sko then
    DO:
      IF wRS-1 = 2 then /* Bruker har kun valgt † oppdatere nye poster. */
        NEXT Last-Sko.
      BUFFER-COPY SkoTex.Last-Sko to SentralDB.Last-Sko.
    END.

end. /* Last-Sko */

IF wStop then
  RETURN "AVBRYT".
ELSE
  RETURN "OK".


