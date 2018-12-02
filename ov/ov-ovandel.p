/************************************************************
    Program:  ov-Ovandel.p
    Created:  TN   13 Sep 99
Description:  Overf›ring av Ovandel til sentral database.

              run ov-Ovandel.p (input input RS-1).

Last change:  TN   15 Sep 99    7:53 pm
************************************************************/

DEF INPUT parameter wRS-1 as INT NO-UNDO.

DEF shared VAR wAvbryt as LOG INITIAL FALSE NO-UNDO.

DEF VAR wStop as LOG NO-UNDO.

DISABLE TRIGGERS FOR LOAD OF SentralDB.ovandel.


ASSIGN wStop = FALSE.

Ovandel:
for each SkoTex.Ovandel no-lock:

  {process_events.i &BlokkLabel = "Ovandel"}

  FIND SentralDB.Ovandel OF SkoTex.Ovandel exclusive-lock NO-ERROR.
  IF NOT AVAILABLE SentralDB.Ovandel then
    DO:
      CREATE SentralDB.Ovandel.
      BUFFER-COPY SkoTex.Ovandel to SentralDB.Ovandel.
    END.
  ELSE IF AVAILABLE SentralDB.Ovandel then
    DO:
      IF wRS-1 = 2 then /* Bruker har kun valgt † oppdatere nye poster. */
        NEXT Ovandel.
      BUFFER-COPY SkoTex.Ovandel to SentralDB.Ovandel.
    END.

end. /* Ovandel */

IF wStop then
  RETURN "AVBRYT".
ELSE
  RETURN "OK".


