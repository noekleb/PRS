/************************************************************
    Program:  ov-StrType.p
    Created:  TN   13 Sep 99
Description:  Overf›ring av StrType til sentral database.

              run ov-StrType.p (input input RS-1).

Last change:  TN   15 Sep 99    7:56 pm
************************************************************/

DEF INPUT parameter wRS-1 as INT NO-UNDO.

DEF shared VAR wAvbryt as LOG INITIAL FALSE NO-UNDO.

DISABLE TRIGGERS FOR LOAD OF SentralDB.strtype.


DEF VAR wStop as LOG NO-UNDO.

ASSIGN wStop = FALSE.

StrType:
for each SkoTex.StrType no-lock:

  {process_events.i &BlokkLabel = "StrType"}

  FIND SentralDB.StrType OF SkoTex.StrType exclusive-lock NO-ERROR.
  IF NOT AVAILABLE SentralDB.StrType then
    DO:
      CREATE SentralDB.StrType.
      BUFFER-COPY SkoTex.StrType to SentralDB.StrType.
    END.
  ELSE IF AVAILABLE SentralDB.StrType then
    DO:
      IF wRS-1 = 2 then /* Bruker har kun valgt † oppdatere nye poster. */
        NEXT StrType.
      BUFFER-COPY SkoTex.StrType to SentralDB.StrType.
    END.

end. /* StrType */

IF wStop then
  RETURN "AVBRYT".
ELSE
  RETURN "OK".


