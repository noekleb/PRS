/************************************************************
    Program:  ov-LevSAnt.p
    Created:  TN   13 Sep 99
Description:  Overf›ring av LevSAnt til sentral database.

              run ov-LevSAnt.p (input input RS-2).

Last change:  TN   17 Sep 99    9:03 pm
************************************************************/

DEF INPUT parameter wRS-2 as INT NO-UNDO.

DEF shared VAR wAvbryt as LOG INITIAL FALSE NO-UNDO.

DEF VAR wStop as LOG NO-UNDO.

DISABLE TRIGGERS FOR LOAD OF SentralDB.levsant.


ASSIGN wStop = FALSE.

LevSAnt:
for each SkoTex.LevSAnt no-lock:

  {process_events.i &BlokkLabel = "LevSAnt"}

  FIND SentralDB.LevSAnt OF SkoTex.LevSAnt exclusive-lock NO-ERROR.
  IF NOT AVAILABLE SentralDB.LevSAnt then
    DO:
      CREATE SentralDB.LevSAnt.
      BUFFER-COPY SkoTex.LevSAnt to SentralDB.LevSAnt.
    END.
  ELSE IF AVAILABLE SentralDB.LevSAnt then
    DO:
      IF wRS-2 = 2 then /* Bruker har kun valgt † oppdatere nye poster. */
        NEXT LevSAnt.
      BUFFER-COPY SkoTex.LevSAnt to SentralDB.LevSAnt.
    END.

end. /* LevSAnt */

IF wStop then
  RETURN "AVBRYT".
ELSE
  RETURN "OK".


