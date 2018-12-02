/************************************************************
    Program:  ov-Produsent.p
    Created:  TN   13 Sep 99
Description:  Overf›ring av Produsent til sentral database.

              run ov-Produsent.p (input input RS-2).

Last change:  TN   17 Sep 99    9:03 pm
************************************************************/

DEF INPUT parameter wRS-2 as INT NO-UNDO.

DEF shared VAR wAvbryt as LOG INITIAL FALSE NO-UNDO.

DEF VAR wStop as LOG NO-UNDO.

DISABLE TRIGGERS FOR LOAD OF SentralDB.produsent.

ASSIGN wStop = FALSE.

Produsent:
for each SkoTex.Produsent no-lock:

  {process_events.i &BlokkLabel = "Produsent"}

  FIND SentralDB.Produsent OF SkoTex.Produsent exclusive-lock NO-ERROR.
  IF NOT AVAILABLE SentralDB.Produsent then
    DO:
      CREATE SentralDB.Produsent.
      BUFFER-COPY SkoTex.Produsent to SentralDB.Produsent.
    END.
  ELSE IF AVAILABLE SentralDB.Produsent then
    DO:
      IF wRS-2 = 2 then /* Bruker har kun valgt † oppdatere nye poster. */
        NEXT Produsent.
      BUFFER-COPY SkoTex.Produsent to SentralDB.Produsent.
    END.

end. /* Produsent */

IF wStop then
  RETURN "AVBRYT".
ELSE
  RETURN "OK".


