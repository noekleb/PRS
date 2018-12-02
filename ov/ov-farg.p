/************************************************************
    Program:  ov-farg.p
    Created:  TN   13 Sep 99
Description:  Overf›ring av farg til sentral database.

              run ov-farg.p (input input RS-1).

Last change:  TN   15 Sep 99    4:44 pm
************************************************************/

DEF INPUT parameter wRS-1 as INT NO-UNDO.

DEF shared VAR wAvbryt as LOG INITIAL FALSE NO-UNDO.

DEF VAR wStop as LOG NO-UNDO.

ASSIGN wStop = FALSE.

DISABLE TRIGGERS FOR LOAD OF SentralDB.Farg.

FARG:
for each SkoTex.farg no-lock:

  {process_events.i &BlokkLabel = "farg"}

  FIND SentralDB.farg OF SkoTex.farg exclusive-lock NO-ERROR.
  IF NOT AVAILABLE SentralDB.farg then
    DO:
      CREATE SentralDB.farg.
      BUFFER-COPY SkoTex.farg to SentralDB.farg.
    END.
  ELSE IF AVAILABLE SentralDB.farg then
    DO:
      IF wRS-1 = 2 then /* Bruker har kun valgt † oppdatere nye poster. */
        NEXT farg.
      BUFFER-COPY SkoTex.farg to SentralDB.farg.
    END.

end. /* farg */

IF wStop then
  RETURN "AVBRYT".
ELSE
  RETURN "OK".


