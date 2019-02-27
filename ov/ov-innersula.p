/************************************************************
    Program:  ov-Innersula.p
    Created:  TN   13 Sep 99
Description:  Overf›ring av Innersula til sentral database.

              run ov-Innersula.p (input input RS-1).

Last change:  TN   15 Sep 99    7:48 pm
************************************************************/

DEF INPUT parameter wRS-1 as INT NO-UNDO.

DEF shared VAR wAvbryt as LOG INITIAL FALSE NO-UNDO.

DISABLE TRIGGERS FOR LOAD OF SentralDB.innersula.

DEF VAR wStop as LOG NO-UNDO.

ASSIGN wStop = FALSE.

Innersula:
for each SkoTex.Innersula no-lock:

  {process_events.i &BlokkLabel = "Innersula"}

  FIND SentralDB.Innersula OF SkoTex.Innersula exclusive-lock NO-ERROR.
  IF NOT AVAILABLE SentralDB.Innersula then
    DO:
      CREATE SentralDB.Innersula.
      BUFFER-COPY SkoTex.Innersula to SentralDB.Innersula.
    END.
  ELSE IF AVAILABLE SentralDB.Innersula then
    DO:
      IF wRS-1 = 2 then /* Bruker har kun valgt † oppdatere nye poster. */
        NEXT Innersula.
      BUFFER-COPY SkoTex.Innersula to SentralDB.Innersula.
    END.

end. /* Innersula */

IF wStop then
  RETURN "AVBRYT".
ELSE
  RETURN "OK".


