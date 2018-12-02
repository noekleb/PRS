/************************************************************
    Program:  ov-Material.p
    Created:  TN   13 Sep 99
Description:  Overf›ring av Material til sentral database.

              run ov-Material.p (input input RS-1).

Last change:  TN   15 Sep 99    7:51 pm
************************************************************/

DEF INPUT parameter wRS-1 as INT NO-UNDO.

DEF shared VAR wAvbryt as LOG INITIAL FALSE NO-UNDO.

DISABLE TRIGGERS FOR LOAD OF SentralDB.material.

DEF VAR wStop as LOG NO-UNDO.

ASSIGN wStop = FALSE.

Material:
for each SkoTex.Material no-lock:

  {process_events.i &BlokkLabel = "Material"}

  FIND SentralDB.Material OF SkoTex.Material exclusive-lock NO-ERROR.
  IF NOT AVAILABLE SentralDB.Material then
    DO:
      CREATE SentralDB.Material.
      BUFFER-COPY SkoTex.Material to SentralDB.Material.
    END.
  ELSE IF AVAILABLE SentralDB.Material then
    DO:
      IF wRS-1 = 2 then /* Bruker har kun valgt † oppdatere nye poster. */
        NEXT Material.
      BUFFER-COPY SkoTex.Material to SentralDB.Material.
    END.

end. /* Material */

IF wStop then
  RETURN "AVBRYT".
ELSE
  RETURN "OK".


