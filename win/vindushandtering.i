/************************************************************
    Program:  vindushandtering.i
    Created:  TN   12 Nov 98
Description:

Last change:  TN    5 Nov 99   10:03 am
************************************************************/

PROCEDURE GetWinPos :
/*------------------------------------------------------------------------------
  Purpose:     Finner og setter posisjon for vinduer
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT  PARAMETER wh-Vindu AS WIDGET  NO-UNDO.

DEF VAR wRow AS DECI NO-UNDO INIT 1.
DEF VAR wCol AS DECI NO-UNDO INIT 1.

DEF VAR wchRow AS CHAR NO-UNDO.
DEF VAR wchCol AS CHAR NO-UNDO.

/* Skal vindusposisjon huskes? */
IF wHuskPos THEN
GetWinPos: DO:
  IF wMappeLokalInifil <> ? THEN DO:
     LOAD wLokalIniFil DIR wMappeLokalInifil BASE-KEY "INI" NO-ERROR.
     IF ERROR-STATUS:ERROR THEN DO:
        LOAD wLokalIniFil DIR wMappeLokalInifil NEW BASE-KEY "INI" NO-ERROR.
        IF ERROR-STATUS:ERROR THEN LEAVE GetWinPos.
     END.
           
     USE wLokalIniFil NO-ERROR.

     IF NOT ERROR-STATUS:ERROR THEN DO:
        GET-KEY-VALUE SECTION wSystemNavn + " " + wh-Vindu:NAME KEY "Row" VALUE wchRow.
        GET-KEY-VALUE SECTION wSystemNavn + " " + wh-Vindu:NAME KEY "Col" VALUE wchCol.
        IF int(wchRow) > 20 THEN
            wchRow = '1'.
        IF int(wchCol) > 164 THEN
            wchCol = '1'.
            
        UNLOAD wLokalIniFil NO-ERROR.

     END.
   END.
END.     

ASSIGN 
  wRow = DECI(wchRow)
  wCol = DECI(wchCol) NO-ERROR.

IF wRow = 0 THEN wRow = ?.
IF wCol = 0 THEN wCol = ?.  

IF wRow <> ? THEN ASSIGN wh-Vindu:ROW = wRow NO-ERROR.
IF wCol <> ? THEN ASSIGN wh-Vindu:COL = wCol NO-ERROR.

END PROCEDURE.

PROCEDURE SaveWinPos:
/*------------------------------------------------------------------------------
  Purpose:     Lagrer vindusposisjon ved avslutning     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER wh-Vindu AS WIDGET NO-UNDO.
DEF VAR i           AS INTE   NO-UNDO.
DEF VAR wEntrySatt  AS LOGI   NO-UNDO.
DEF VAR wh          AS WIDGET NO-UNDO.

  /* Skal vindusposisjon huskes? */
  IF wHuskPos THEN
  SaveWinPos: DO:
    IF wMappeLokalInifil <> ? THEN DO:
      LOAD wLokalIniFil DIR wMappeLokalInifil BASE-KEY "INI" NO-ERROR.
      IF ERROR-STATUS:ERROR THEN DO:
         LOAD wLokalIniFil DIR wMappeLokalInifil NEW BASE-KEY "INI" NO-ERROR.
         IF ERROR-STATUS:ERROR THEN LEAVE SaveWinPos.
      END.
           
      USE wLokalIniFil NO-ERROR.

      IF NOT ERROR-STATUS:ERROR THEN DO:
         PUT-KEY-VALUE SECTION wSystemNavn + " " + wh-Vindu:NAME KEY "Row" VALUE STRING(wh-Vindu:ROW) NO-ERROR.
         PUT-KEY-VALUE SECTION wSystemNavn + " " + wh-Vindu:NAME KEY "Col" VALUE STRING(wh-Vindu:COL) NO-ERROR.
            
         UNLOAD wLokalIniFil NO-ERROR.

     END.
    END.
  END.
END PROCEDURE.

/* Sl†r husking av vindusposisjonen av/p† */
PROCEDURE HuskPos:
  if wHuskPos = TRUE then
    wHuskPos = FALSE.
  else
    wHuskPos = TRUE.
END procedure.

/* Minimerer alle aktive loggede vinduer. */
PROCEDURE Minimer:
  DEF INPUT PARAMETER wCallProgHandle as HANDLE NO-UNDO.
  DEF INPUT PARAMETER wSensFlagg      as LOG    NO-UNDO. /* True = Windu settes insensetive */

  FOR EACH ProgramListe:
    if VALID-HANDLE(ProgramListe.ProgramHandle) AND valid-handle(ProgramListe.WindowHandle) then
      DO:
        if ProgramListe.WindowHandle:WINDOW-STATE <> 2 then
          assign
            ProgramListe.WindowHandle:WINDOW-STATE = 2
            ProgramListe.WindowHandle:Sensitive    = if wSensFlagg
                                                        then false
                                                        else ProgramListe.WindowHandle:Sensitive.
      END.
  END.

END PROCEDURE.

/* Maksimerer alle aktive loggede vinduer. */
PROCEDURE Maksimer:
  DEF INPUT PARAMETER wCallProgHandle as HANDLE NO-UNDO.
  DEF INPUT PARAMETER wSensFlagg      as LOG    NO-UNDO. /* True = Windu settes sensitive */

  FOR EACH ProgramListe:
    if VALID-HANDLE(ProgramListe.ProgramHandle) AND valid-handle(ProgramListe.WindowHandle) then
      DO:
        if ProgramListe.WindowHandle:WINDOW-STATE <> 3 then
          assign
            ProgramListe.WindowHandle:WINDOW-STATE = 3
            ProgramListe.WindowHandle:Sensitive    = if wSensFlagg
                                                        then true
                                                        else ProgramListe.WindowHandle:Sensitive.
      END.
  END.

END PROCEDURE.

