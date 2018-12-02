/************************************************************
    Program:  ov-SlitSula.p
    Created:  TN   13 Sep 99
Description:  Overf›ring av SlitSula til sentral database.

              run ov-SlitSula.p (input input RS-1).

Last change:  TN   15 Sep 99    7:55 pm
************************************************************/

DEF INPUT parameter wRS-1 as INT NO-UNDO.

DEF shared VAR wAvbryt as LOG INITIAL FALSE NO-UNDO.

DEF VAR wStop as LOG NO-UNDO.

DISABLE TRIGGERS FOR LOAD OF SentralDB.slitsula.

ASSIGN wStop = FALSE.

SlitSula:
for each SkoTex.SlitSula no-lock:

  {process_events.i &BlokkLabel = "SlitSula"}

  FIND SentralDB.SlitSula OF SkoTex.SlitSula exclusive-lock NO-ERROR.
  IF NOT AVAILABLE SentralDB.SlitSula then
    DO:
      CREATE SentralDB.SlitSula.
      BUFFER-COPY SkoTex.SlitSula to SentralDB.SlitSula.
    END.
  ELSE IF AVAILABLE SentralDB.SlitSula then
    DO:
      IF wRS-1 = 2 then /* Bruker har kun valgt † oppdatere nye poster. */
        NEXT SlitSula.
      BUFFER-COPY SkoTex.SlitSula to SentralDB.SlitSula.
    END.

end. /* SlitSula */

IF wStop then
  RETURN "AVBRYT".
ELSE
  RETURN "OK".


