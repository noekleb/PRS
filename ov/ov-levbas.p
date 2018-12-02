/************************************************************
    Program:  ov-LevBas.p
    Created:  TN   13 Sep 99
Description:  Overf›ring av LevBas til sentral database.

              run ov-LevBas.p (input input RS-2).

Last change:  TN   17 Sep 99    9:42 pm
************************************************************/

DEF INPUT parameter wRS-2 as INT NO-UNDO.

DEF shared VAR wAvbryt as LOG INITIAL FALSE NO-UNDO.

DEF VAR wStop as LOG NO-UNDO.

DISABLE TRIGGERS FOR LOAD OF SentralDB.levbas.                                              

ASSIGN wStop = FALSE.

LevBas:
for each SkoTex.LevBas no-lock:

  {process_events.i &BlokkLabel = "LevBas"}

  FIND SentralDB.LevBas OF SkoTex.LevBas exclusive-lock NO-ERROR.
  IF NOT AVAILABLE SentralDB.LevBas then
    DO:
      CREATE SentralDB.LevBas.
      BUFFER-COPY SkoTex.LevBas to SentralDB.LevBas.
    END.
  ELSE IF AVAILABLE SentralDB.LevBas then
    DO:
      IF wRS-2 = 2 then /* Bruker har kun valgt † oppdatere nye poster. */
        NEXT LevBas.
      BUFFER-COPY SkoTex.LevBas to SentralDB.LevBas.
    END.

end. /* LevBas */

IF wStop then
  RETURN "AVBRYT".
ELSE
  RETURN "OK".


