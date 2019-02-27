/************************************************************
    Program:  ov-anv-kod.p
    Created:  TN   13 Sep 99
Description:  Overf›ring av Anv-Kod til sentral database.

              run ov-anv-kod.p (input input RS-1, output wStop).

Last change:  TN   15 Sep 99    4:42 pm
************************************************************/

DEF INPUT parameter wRS-1 as INT NO-UNDO.

DEF shared VAR wAvbryt as LOG INITIAL FALSE NO-UNDO.

DEF VAR wStop as LOG NO-UNDO.

ASSIGN wStop = FALSE.

ANV-KOD:
for each SkoTex.Anv-Kod no-lock:

  {process_events.i &BlokkLabel = "ANV-KOD"}

  FIND SentralDB.Anv-Kod OF SkoTex.Anv-Kod exclusive-lock NO-ERROR.
  IF NOT AVAILABLE SentralDB.Anv-Kod then
    DO:
      CREATE SentralDB.Anv-Kod.
      BUFFER-COPY SkoTex.Anv-Kod to SentralDB.Anv-Kod.
    END.
  ELSE IF AVAILABLE SentralDB.Anv-Kod then
    DO:
      IF wRS-1 = 2 then /* Bruker har kun valgt † oppdatere nye poster. */
        NEXT Anv-Kod.
      BUFFER-COPY SkoTex.Anv-Kod to SentralDB.Anv-Kod.
    END.

end. /* ANV-KOD */

IF wStop then
  RETURN "AVBRYT".
ELSE
  RETURN "OK".


