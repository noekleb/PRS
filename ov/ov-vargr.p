/************************************************************
    Program:  ov-VarGr.p
    Created:  TN   13 Sep 99
Description:  Overf›ring av VarGr til sentral database.

              run ov-VarGr.p (input input RS-1).

Last change:  TN   15 Sep 99    7:58 pm
************************************************************/

DEF INPUT parameter wRS-1 as INT NO-UNDO.

DEF shared VAR wAvbryt as LOG INITIAL FALSE NO-UNDO.

DEF VAR wStop as LOG NO-UNDO.

DISABLE TRIGGERS FOR LOAD OF SentralDB.vargr.

ASSIGN wStop = FALSE.

VarGr:
for each SkoTex.VarGr no-lock:

  {process_events.i &BlokkLabel = "VarGr"}

  FIND SentralDB.VarGr OF SkoTex.VarGr exclusive-lock NO-ERROR.
  IF NOT AVAILABLE SentralDB.VarGr then
    DO:
      CREATE SentralDB.VarGr.
      BUFFER-COPY SkoTex.VarGr to SentralDB.VarGr.
    END.
  ELSE IF AVAILABLE SentralDB.VarGr then
    DO:
      IF wRS-1 = 2 then /* Bruker har kun valgt † oppdatere nye poster. */
        NEXT VarGr.
      BUFFER-COPY SkoTex.VarGr to SentralDB.VarGr.
    END.

end. /* VarGr */

IF wStop then
  RETURN "AVBRYT".
ELSE
  RETURN "OK".


