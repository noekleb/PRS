/************************************************************
    Program:  ov-foder.p
    Created:  TN   13 Sep 99
Description:  Overf›ring av Foder til sentral database.

              run ov-Foder.p (input input RS-1).

Last change:  TN   15 Sep 99    7:46 pm
************************************************************/

DEF INPUT parameter wRS-1 as INT NO-UNDO.

DEF shared VAR wAvbryt as LOG INITIAL FALSE NO-UNDO.

DISABLE TRIGGERS FOR LOAD OF SentralDB.foder.

DEF VAR wStop as LOG NO-UNDO.

ASSIGN wStop = FALSE.

Foder:
for each SkoTex.Foder no-lock:

  {process_events.i &BlokkLabel = "Foder"}

  FIND SentralDB.Foder OF SkoTex.Foder exclusive-lock NO-ERROR.
  IF NOT AVAILABLE SentralDB.Foder then
    DO:
      CREATE SentralDB.Foder.
      BUFFER-COPY SkoTex.Foder to SentralDB.Foder.
    END.
  ELSE IF AVAILABLE SentralDB.Foder then
    DO:
      IF wRS-1 = 2 then /* Bruker har kun valgt † oppdatere nye poster. */
        NEXT Foder.
      BUFFER-COPY SkoTex.Foder to SentralDB.Foder.
    END.

end. /* Foder */

IF wStop then
  RETURN "AVBRYT".
ELSE
  RETURN "OK".


