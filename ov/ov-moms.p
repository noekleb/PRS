/************************************************************
    Program:  ov-Moms.p
    Created:  TN   13 Sep 99
Description:  Overf›ring av Moms til sentral database.

              run ov-Moms.p (input input RS-1).

Last change:  TN   15 Sep 99    7:52 pm
************************************************************/

DEF INPUT parameter wRS-1 as INT NO-UNDO.

DEF shared VAR wAvbryt as LOG INITIAL FALSE NO-UNDO.

DEF VAR wStop as LOG NO-UNDO.

DISABLE TRIGGERS FOR LOAD OF SentralDB.moms.

ASSIGN wStop = FALSE.

Moms:
for each SkoTex.Moms no-lock:

  {process_events.i &BlokkLabel = "Moms"}

  FIND SentralDB.Moms OF SkoTex.Moms exclusive-lock NO-ERROR.
  IF NOT AVAILABLE SentralDB.Moms then
    DO:
      CREATE SentralDB.Moms.
      BUFFER-COPY SkoTex.Moms to SentralDB.Moms.
    END.
  ELSE IF AVAILABLE SentralDB.Moms then
    DO:
      IF wRS-1 = 2 then /* Bruker har kun valgt † oppdatere nye poster. */
        NEXT Moms.
      BUFFER-COPY SkoTex.Moms to SentralDB.Moms.
    END.

end. /* Moms */

IF wStop then
  RETURN "AVBRYT".
ELSE
  RETURN "OK".


