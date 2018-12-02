/************************************************************
    Program:  ov-Kategori.p
    Created:  TN   13 Sep 99
Description:  Overf›ring av Kategori til sentral database.

              run ov-Kategori.p (input input RS-1).

Last change:  TN   15 Sep 99    7:49 pm
************************************************************/

DEF INPUT parameter wRS-1 as INT NO-UNDO.

DEF shared VAR wAvbryt as LOG INITIAL FALSE NO-UNDO.

DEF VAR wStop as LOG NO-UNDO.

DISABLE TRIGGERS FOR LOAD OF SentralDB.kategori.

ASSIGN wStop = FALSE.

Kategori:
for each SkoTex.Kategori no-lock:

  {process_events.i &BlokkLabel = "Kategori"}

  FIND SentralDB.Kategori OF SkoTex.Kategori exclusive-lock NO-ERROR.
  IF NOT AVAILABLE SentralDB.Kategori then
    DO:
      CREATE SentralDB.Kategori.
      BUFFER-COPY SkoTex.Kategori to SentralDB.Kategori.
    END.
  ELSE IF AVAILABLE SentralDB.Kategori then
    DO:
      IF wRS-1 = 2 then /* Bruker har kun valgt † oppdatere nye poster. */
        NEXT Kategori.
      BUFFER-COPY SkoTex.Kategori to SentralDB.Kategori.
    END.

end. /* Kategori */

IF wStop then
  RETURN "AVBRYT".
ELSE
  RETURN "OK".


