/************************************************************
    Program:  ov-Prov.p
    Created:  TN   13 Sep 99
Description:  Overf›ring av Prov til sentral database.

              run ov-Prov.p (input input RS-1).

Last change:  TN   15 Sep 99    7:53 pm
************************************************************/

DEF INPUT parameter wRS-1 as INT NO-UNDO.

DEF shared VAR wAvbryt as LOG INITIAL FALSE NO-UNDO.

DEF VAR wStop as LOG NO-UNDO.

DISABLE TRIGGERS FOR LOAD OF SentralDB.prov.

ASSIGN wStop = FALSE.

Prov:
for each SkoTex.Prov no-lock:

  {process_events.i &BlokkLabel = "Prov"}

  FIND SentralDB.Prov OF SkoTex.Prov exclusive-lock NO-ERROR.
  IF NOT AVAILABLE SentralDB.Prov then
    DO:
      CREATE SentralDB.Prov.
      BUFFER-COPY SkoTex.Prov to SentralDB.Prov.
    END.
  ELSE IF AVAILABLE SentralDB.Prov then
    DO:
      IF wRS-1 = 2 then /* Bruker har kun valgt † oppdatere nye poster. */
        NEXT Prov.
      BUFFER-COPY SkoTex.Prov to SentralDB.Prov.
    END.

end. /* Prov */

IF wStop then
  RETURN "AVBRYT".
ELSE
  RETURN "OK".


