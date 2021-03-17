/************************************************************
    Program:  ov-VareMerke.p
    Created:  TN   13 Sep 99
Description:  Overf›ring av VareMerke til sentral database.

              run ov-VareMerke.p (input input RS-2).

Last change:  TN   17 Sep 99    9:04 pm
************************************************************/

DEF INPUT parameter wRS-2 as INT NO-UNDO.

DEF shared VAR wAvbryt as LOG INITIAL FALSE NO-UNDO.

DEF VAR wStop as LOG NO-UNDO.

DISABLE TRIGGERS FOR LOAD OF SentralDB.varemerk.

ASSIGN wStop = FALSE.

VareMerke:
for each SkoTex.VareMerke no-lock:

  {process_events.i &BlokkLabel = "VareMerke"}

  FIND SentralDB.VareMerke OF SkoTex.VareMerke exclusive-lock NO-ERROR.
  IF NOT AVAILABLE SentralDB.VareMerke then
    DO:
      CREATE SentralDB.VareMerke.
      BUFFER-COPY SkoTex.VareMerke to SentralDB.VareMerke.
    END.
  ELSE IF AVAILABLE SentralDB.VareMerke then
    DO:
      IF wRS-2 = 2 then /* Bruker har kun valgt † oppdatere nye poster. */
        NEXT VareMerke.
      BUFFER-COPY SkoTex.VareMerke to SentralDB.VareMerke.
    END.

end. /* VareMerke */

IF wStop then
  RETURN "AVBRYT".
ELSE
  RETURN "OK".


