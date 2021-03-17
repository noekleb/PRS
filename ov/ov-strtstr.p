/************************************************************
    Program:  ov-StrTStr.p
    Created:  TN   13 Sep 99
Description:  Overf›ring av StrTStr til sentral database.

              run ov-StrTStr.p (input input RS-1).

Last change:  TN   15 Sep 99    7:57 pm
************************************************************/

DEF INPUT parameter wRS-1 as INT NO-UNDO.

DEF shared VAR wAvbryt as LOG INITIAL FALSE NO-UNDO.

DISABLE TRIGGERS FOR LOAD OF SentralDB.strtstr.

DEF VAR wStop as LOG NO-UNDO.

ASSIGN wStop = FALSE.

StrTStr:
for each SkoTex.StrTStr no-lock:

  {process_events.i &BlokkLabel = "StrTStr"}

  FIND SentralDB.StrTStr OF SkoTex.StrTStr exclusive-lock NO-ERROR.
  IF NOT AVAILABLE SentralDB.StrTStr then
    DO:
      CREATE SentralDB.StrTStr.
      BUFFER-COPY SkoTex.StrTStr to SentralDB.StrTStr.
    END.
  ELSE IF AVAILABLE SentralDB.StrTStr then
    DO:
      IF wRS-1 = 2 then /* Bruker har kun valgt † oppdatere nye poster. */
        NEXT StrTStr.
      BUFFER-COPY SkoTex.StrTStr to SentralDB.StrTStr.
    END.

end. /* StrTStr */

IF wStop then
  RETURN "AVBRYT".
ELSE
  RETURN "OK".


