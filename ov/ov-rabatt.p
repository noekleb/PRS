/************************************************************
    Program:  ov-Rabatt.p
    Created:  TN   13 Sep 99
Description:  Overf›ring av Rabatt til sentral database.

              run ov-Rabatt.p (input input RS-1).

Last change:  TN   15 Sep 99    7:54 pm
************************************************************/

DEF INPUT parameter wRS-1 as INT NO-UNDO.

DEF shared VAR wAvbryt as LOG INITIAL FALSE NO-UNDO.

DEF VAR wStop as LOG NO-UNDO.

DISABLE TRIGGERS FOR LOAD OF SentralDB.rabatt.

ASSIGN wStop = FALSE.

Rabatt:
for each SkoTex.Rabatt no-lock:

  {process_events.i &BlokkLabel = "Rabatt"}

  FIND SentralDB.Rabatt OF SkoTex.Rabatt exclusive-lock NO-ERROR.
  IF NOT AVAILABLE SentralDB.Rabatt then
    DO:
      CREATE SentralDB.Rabatt.
      BUFFER-COPY SkoTex.Rabatt to SentralDB.Rabatt.
    END.
  ELSE IF AVAILABLE SentralDB.Rabatt then
    DO:
      IF wRS-1 = 2 then /* Bruker har kun valgt † oppdatere nye poster. */
        NEXT Rabatt.
      BUFFER-COPY SkoTex.Rabatt to SentralDB.Rabatt.
    END.

end. /* Rabatt */

IF wStop then
  RETURN "AVBRYT".
ELSE
  RETURN "OK".


