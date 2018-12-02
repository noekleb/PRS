/************************************************************
    Program:  ov-Valuta.p
    Created:  TN   13 Sep 99
Description:  Overf›ring av Valuta til sentral database.

              run ov-Valuta.p (input input RS-1).

Last change:  TN   17 Sep 99    9:43 pm
************************************************************/

DEF INPUT parameter wRS-1 as INT NO-UNDO.

DEF shared VAR wAvbryt as LOG INITIAL FALSE NO-UNDO.

DEF VAR wStop as LOG NO-UNDO.

DISABLE TRIGGERS FOR LOAD OF SentralDB.Valuta.

ASSIGN wStop = FALSE.

on CREATE OF SentralDB.Valuta override
  DO:

  END.
on WRITE OF SentralDB.Valuta override
  DO:

  END.

Valuta:
for each SkoTex.Valuta no-lock:

  {process_events.i &BlokkLabel = "Valuta"}

  FIND SentralDB.Valuta OF SkoTex.Valuta exclusive-lock NO-ERROR.
  IF NOT AVAILABLE SentralDB.Valuta then
    DO:
      CREATE SentralDB.Valuta.
      BUFFER-COPY SkoTex.Valuta to SentralDB.Valuta.
    END.
  ELSE IF AVAILABLE SentralDB.Valuta then
    DO:
      IF wRS-1 = 2 then /* Bruker har kun valgt † oppdatere nye poster. */
        NEXT Valuta.
      BUFFER-COPY SkoTex.Valuta to SentralDB.Valuta.
    END.

end. /* Valuta */

IF wStop then
  RETURN "AVBRYT".
ELSE
  RETURN "OK".




