/************************************************************
    Program:  ov-Huvgr.p
    Created:  TN   13 Sep 99
Description:  Overf›ring av Huvgr til sentral database.

              run ov-Huvgr.p (input input RS-1).

Last change:  TN   15 Sep 99    7:48 pm
************************************************************/

DEF INPUT parameter wRS-1 as INT NO-UNDO.

DISABLE TRIGGERS FOR LOAD OF SentralDB.huvgr.

DEF shared VAR wAvbryt as LOG INITIAL FALSE NO-UNDO.

DEF VAR wStop as LOG NO-UNDO.

ASSIGN wStop = FALSE.

Huvgr:
for each SkoTex.Huvgr no-lock:

  {process_events.i &BlokkLabel = "Huvgr"}

  FIND SentralDB.Huvgr OF SkoTex.Huvgr exclusive-lock NO-ERROR.
  IF NOT AVAILABLE SentralDB.Huvgr then
    DO:
      CREATE SentralDB.Huvgr.
      BUFFER-COPY SkoTex.Huvgr to SentralDB.Huvgr.
    END.
  ELSE IF AVAILABLE SentralDB.Huvgr then
    DO:
      IF wRS-1 = 2 then /* Bruker har kun valgt † oppdatere nye poster. */
        NEXT Huvgr.
      BUFFER-COPY SkoTex.Huvgr to SentralDB.Huvgr.
    END.

end. /* Huvgr */

IF wStop then
  RETURN "AVBRYT".
ELSE
  RETURN "OK".


