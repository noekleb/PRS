PROCEDURE InitBrowseSettings :
/*------------------------------------------------------------------------------
  Purpose:     Initierer innstillinger for en browse
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT  PARAMETER wBrowse  AS WIDGET NO-UNDO.
  DEF INPUT  PARAMETER wCaller  AS HANDLE NO-UNDO. 
  DEF INPUT  PARAMETER wBrowse# AS INTE   NO-UNDO.
  RUN CreateBrowsePopup(wBrowse,wCaller,wBrowse#).
  RUN GetBrowseSettings(wBrowse,wCaller,"").
  RETURN RETURN-VALUE.
END PROCEDURE.

PROCEDURE SaveBrowseSettings :
/*------------------------------------------------------------------------------
  Purpose:     Lagrer innstillingene for en browse     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER wh-Browse AS WIDGET NO-UNDO.
  DEF INPUT PARAMETER wStd      AS CHAR   NO-UNDO.
  DEF INPUT PARAMETER wCaller   AS HANDLE NO-UNDO.

  DEF VAR wId  AS CHAR   NO-UNDO.
  DEF VAR wh   AS HANDLE NO-UNDO.
  DEF VAR ws   AS CHAR   NO-UNDO.
  DEF VAR wX   AS HANDLE NO-UNDO.
  ASSIGN wId = wh-Browse:NAME + " " + wCaller:FILE-NAME
         wh  = wh-Browse:FIRST-COLUMN
         wX  = wh-Browse:CURRENT-COLUMN
         ws  = STRING(wh-Browse:ROW-HEIGHT-PIXELS) 
             + "," 
             + STRING(wh-Browse:SEPARATORS,"J/N") 
             + ","
             + (IF wh-Browse:SEPARATOR-FGCOLOR = ? THEN "?" ELSE STRING(wh-Browse:SEPARATOR-FGCOLOR))
             + ","
             + STRING(wh-Browse:NUM-LOCKED-COLUMNS)
             + ","
             + STRING(wX:NAME)
             + "|".
  DO WHILE VALID-HANDLE(wh):
     ASSIGN ws = ws 
               + wh:NAME 
               + "," 
               + STRING(wh:WIDTH-PIXELS) 
               + "," 
               + (IF wh:COLUMN-FGCOLOR = ? THEN "?" ELSE STRING(wh:COLUMN-FGCOLOR))
               + "," 
               + (IF wh:COLUMN-BGCOLOR = ? THEN "?" ELSE STRING(wh:COLUMN-BGCOLOR))
               + "|"
            wh = wh:NEXT-COLUMN. 
  END.

  ASSIGN ws = SUBSTR(ws,1,LENGTH(ws) - 1). /* Fjerner siste skilletegn */
  
  SaveSettings: DO:
    IF wMappeLokalInifil <> ? THEN DO:
       LOAD wLokalIniFil DIR wMappeLokalInifil BASE-KEY "INI" NO-ERROR.
       IF ERROR-STATUS:ERROR THEN DO:
          LOAD wLokalIniFil DIR wMappeLokalInifil NEW BASE-KEY "INI" NO-ERROR.
          IF ERROR-STATUS:ERROR THEN LEAVE SaveSettings.
       END.
           
       USE wLokalIniFil NO-ERROR.
       IF NOT ERROR-STATUS:ERROR THEN DO:
          IF ws = wStd THEN
               PUT-KEY-VALUE SECTION wSystemNavn + " BROWSE " + wId KEY ?          VALUE ?  NO-ERROR.
          ELSE PUT-KEY-VALUE SECTION wSystemNavn + " BROWSE " + wId KEY "Settings" VALUE ws NO-ERROR.
          UNLOAD wLokalIniFil NO-ERROR.
       END.
    END.
  END.
END PROCEDURE.

PROCEDURE StdBrowseSettings :
/*------------------------------------------------------------------------------
  Purpose:     Setter standarinnstillinger for en browse
  Parameters:  se under
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER wBrowse   AS WIDGET NO-UNDO.
  DEF INPUT PARAMETER wSettings AS CHAR   NO-UNDO.
  DEF INPUT PARAMETER wCaller   AS HANDLE NO-UNDO.
  
  DEF VAR wBrowse_frame AS WIDGET NO-UNDO.
  DEF VAR wLocked       AS INTE   NO-UNDO.
  
  ASSIGN wBrowse_frame = wBrowse:FRAME.
  
/*  RUN LockWindowUpdate (wBrowse_frame:HWND, OUTPUT wLocked). */

  RUN GetBrowseSettings (wBrowse,wCaller,wSettings).

/*  IF wLocked <> 0 THEN
    RUN LockWindowUpdate ( 0, OUTPUT wLocked). */
END PROCEDURE.

PROCEDURE GetBrowseSettings :
/*------------------------------------------------------------------------------
  Purpose:     Finner og setter innstillingene for en browse.
  Parameters:  se under
               NB! Returnerer innstillingene browseren har før endring.
  Notes:       Hvis inputparameteret wSettings er oppgitt benyttes dette.
               Hvis ikke, hentes innstillinger fra ini-fila.
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER wBrowse   AS WIDGET NO-UNDO.
  DEF INPUT PARAMETER wCaller   AS HANDLE NO-UNDO.
  DEF INPUT PARAMETER wSettings AS CHAR   NO-UNDO.
  
  DEF VAR wh  AS HANDLE NO-UNDO.
  DEF VAR ws  AS CHAR   NO-UNDO. /* Fra ini-fila */
  DEF VAR wf  AS CHAR   NO-UNDO. /* Standardinnstillinger */
  DEF VAR i   AS INTE   NO-UNDO INIT 2.
  DEF VAR j   AS INTE   NO-UNDO.
  DEF VAR wId AS CHAR   NO-UNDO.
  
  ASSIGN wId = wBrowse:NAME + " " + wCaller:FILE-NAME.
  
  IF wSettings = "" THEN 
  GetSettings: DO:
    IF wMappeLokalInifil <> ? THEN DO:
       LOAD wLokalIniFil DIR wMappeLokalInifil BASE-KEY "INI" NO-ERROR.
       IF ERROR-STATUS:ERROR THEN DO:
          LOAD wLokalIniFil DIR wMappeLokalInifil NEW BASE-KEY "INI" NO-ERROR.
          IF ERROR-STATUS:ERROR THEN LEAVE GetSettings.
       END.
           
       USE wLokalIniFil NO-ERROR.
            
       IF NOT ERROR-STATUS:ERROR THEN DO:
          GET-KEY-VALUE SECTION wSystemNavn + " BROWSE " + wId KEY "Settings"  VALUE ws.
        
          UNLOAD wLokalIniFil NO-ERROR.
       END.
     END.
      
     ASSIGN wh  = wBrowse:FIRST-COLUMN
            wf  = (IF wBrowse:ROW-HEIGHT-PIXELS = 0 THEN "13" ELSE STRING(wBrowse:ROW-HEIGHT-PIXELS)) 
                + ","
                + STRING(wBrowse:SEPARATORS,"J/N")
                + ","
                + (IF wBrowse:SEPARATOR-FGCOLOR = ? THEN "?" ELSE STRING(wBrowse:SEPARATOR-FGCOLOR))
                + ","
                + STRING(wBrowse:NUM-LOCKED-COLUMNS)
                + ","
                + wh:NAME
                + "|".
 
     DO WHILE VALID-HANDLE(wh):
        ASSIGN wf  = wf 
                   + wh:NAME 
                   + "," 
                   + (IF wh:WIDTH-PIXELS = ? THEN "?" ELSE STRING(wh:WIDTH-PIXELS)) 
                   + ","
                   + (IF wh:COLUMN-FGCOLOR = ? THEN "?" ELSE STRING(wh:COLUMN-FGCOLOR))
                   + ","
                   + (IF wh:COLUMN-BGCOLOR = ? THEN "?" ELSE STRING(wh:COLUMN-BGCOLOR))
                   + "|"
               wh  = wh:NEXT-COLUMN.
     END.
  END.
  
  ELSE ASSIGN ws = wSettings.

  IF ws <> "" AND ws <> ? THEN DO: 
     ASSIGN wBrowse:ROW-HEIGHT-PIXELS  = INT(ENTRY(1,ENTRY(1,ws,"|")))
            wBrowse:SEPARATORS         = ENTRY(2,ENTRY(1,ws,"|")) = "J"
            wBrowse:SEPARATOR-FGCOLOR  = INT(ENTRY(3,ENTRY(1,ws,"|")))
            wBrowse:NUM-LOCKED-COLUMNS = INT(ENTRY(4,ENTRY(1,ws,"|"))).
         
     DO j = 2 TO NUM-ENTRIES(ws,"|"):
        ASSIGN wh = wBrowse:FIRST-COLUMN
               i  = 0.

        DO WHILE VALID-HANDLE(wh):
           ASSIGN i = i + 1.
           IF wh:NAME = ENTRY(1,ENTRY(j,ws,"|")) THEN DO:
              IF wh:NAME = ENTRY(5,ENTRY(1,ws,"|")) THEN
                  wBrowse:CURRENT-COLUMN = wh.
              wBrowse:MOVE-COLUMN(i,j - 1).
              ASSIGN wh:WIDTH-PIXELS   = INT(ENTRY(2,ENTRY(j,ws,"|"))) 
                     wh:COLUMN-FGCOLOR = INT(ENTRY(3,ENTRY(j,ws,"|")))
                     wh:COLUMN-BGCOLOR = INT(ENTRY(4,ENTRY(j,ws,"|"))) NO-ERROR.
              LEAVE.
           END.   
           
           ASSIGN wh  = wh:NEXT-COLUMN.
                  
        END.       

     END.
  END.  
  IF wSettings <> "" AND wBrowse:EXPANDABLE THEN
       ASSIGN wBrowse:EXPANDABLE = NO
              wBrowse:EXPANDABLE = YES.
  RETURN SUBSTR(wf,1,LENGTH(wf) - 1).
END PROCEDURE.

PROCEDURE CreateBrowsePopup :
/*------------------------------------------------------------------------------
  Purpose:     Lager en popupmeny for innstilling av en browse
  Parameters:  se under
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT  PARAMETER wBrowse  AS WIDGET NO-UNDO.
  DEF INPUT  PARAMETER wCaller  AS HANDLE NO-UNDO. 
  DEF INPUT  PARAMETER wBrowse# AS INTE   NO-UNDO.
  
  DEF VAR wBrowse_frame        AS WIDGET NO-UNDO.
  DEF VAR wBrowse_popup        AS WIDGET NO-UNDO.
  DEF VAR wBrowse_popupItem    AS WIDGET NO-UNDO.
  
  ASSIGN wBrowse_popup = wBrowse:POPUP-MENU.
  
  IF VALID-HANDLE(wBrowse_popup) THEN DO:
     ASSIGN wBrowse:POPUP-MENU = ?.
     CREATE MENU-ITEM wBrowse_popupItem
         ASSIGN PARENT  = wBrowse_popup
                SUBTYPE = "RULE".
  END.
  ELSE CREATE MENU wBrowse_popup
          ASSIGN POPUP-ONLY = YES.

   CREATE MENU-ITEM wBrowse_popupItem
     ASSIGN PARENT    = wBrowse_popup
            LABEL     = "&Innstillinger..."
            SENSITIVE = YES
     TRIGGERS:
        ON CHOOSE PERSISTENT RUN d-browseprop.w(wBrowse,wCaller,wBrowse#).
     END TRIGGERS.
    
   CREATE MENU-ITEM wBrowse_popupItem
     ASSIGN PARENT    = wBrowse_popup
            LABEL     = "&Standardinnstillinger!"
            SENSITIVE = YES
     TRIGGERS:
        ON CHOOSE PERSISTENT RUN VALUE("StdBrowseSettings0" + TRIM(STRING(wBrowse#,"zzzz"))) IN wCaller.
     END TRIGGERS.

   ASSIGN wBrowse:POPUP-MENU = wBrowse_popup.
END PROCEDURE.
