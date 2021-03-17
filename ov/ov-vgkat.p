/************************************************************
    Program:  ov-VgKat.p
    Created:  TN   13 Sep 99
Description:  Overf›ring av VgKat til sentral database.

              run ov-VgKat.p (input input RS-1).

Last change:  TN   15 Sep 99    8:20 pm
************************************************************/

DEF INPUT parameter wRS-1 as INT NO-UNDO.

DEF shared VAR wAvbryt as LOG INITIAL FALSE NO-UNDO.

DEF VAR wStop as LOG NO-UNDO.

DISABLE TRIGGERS FOR LOAD OF SentralDB.vgkat.

ASSIGN wStop = FALSE.

VgKat:
for each SkoTex.VgKat no-lock:

  {process_events.i &BlokkLabel = "VgKat"}

  FIND SentralDB.VgKat OF SkoTex.VgKat exclusive-lock NO-ERROR.
  IF NOT AVAILABLE SentralDB.VgKat then
    DO:
      CREATE SentralDB.VgKat.
      BUFFER-COPY SkoTex.VgKat to SentralDB.VgKat.
    END.
  ELSE IF AVAILABLE SentralDB.VgKat then
    DO:
      IF wRS-1 = 2 then /* Bruker har kun valgt † oppdatere nye poster. */
        NEXT VgKat.
      BUFFER-COPY SkoTex.VgKat to SentralDB.VgKat.
    END.

end. /* VgKat */

IF wStop then
  RETURN "AVBRYT".
ELSE
  RETURN "OK".


