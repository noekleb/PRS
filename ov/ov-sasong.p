/************************************************************
    Program:  ov-Sasong.p
    Created:  TN   13 Sep 99
Description:  Overf›ring av Sasong til sentral database.

              run ov-Sasong.p (input input RS-1).

Last change:  TN   15 Sep 99    7:54 pm
************************************************************/

DEF INPUT parameter wRS-1 as INT NO-UNDO.

DEF shared VAR wAvbryt as LOG INITIAL FALSE NO-UNDO.

DEF VAR wStop as LOG NO-UNDO.

DISABLE TRIGGERS FOR LOAD OF SentralDB.sasong.


ASSIGN wStop = FALSE.

Sasong:
for each SkoTex.Sasong no-lock:

  {process_events.i &BlokkLabel = "Sasong"}

  FIND SentralDB.Sasong OF SkoTex.Sasong exclusive-lock NO-ERROR.
  IF NOT AVAILABLE SentralDB.Sasong then
    DO:
      CREATE SentralDB.Sasong.
      BUFFER-COPY SkoTex.Sasong to SentralDB.Sasong.
    END.
  ELSE IF AVAILABLE SentralDB.Sasong then
    DO:
      IF wRS-1 = 2 then /* Bruker har kun valgt † oppdatere nye poster. */
        NEXT Sasong.
      BUFFER-COPY SkoTex.Sasong to SentralDB.Sasong.
    END.

end. /* Sasong */

IF wStop then
  RETURN "AVBRYT".
ELSE
  RETURN "OK".


