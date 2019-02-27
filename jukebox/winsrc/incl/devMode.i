/*
IF CAN-DO("MULTI-SESSION-AGENT,APPSERVER,WEBSPEED",SESSION:CLIENT-TYPE) AND C-Win = ? THEN
  CREATE FRAME C-Win
         ASSIGN HIDDEN = YES.
*/  
&IF DEFINED(UIB_is_Running) NE 0 &THEN
  RUN JBoxLoadLib.p ("JBoxASlib.p,JBoxUIlib.p,ResizeLib.p"
                   + (IF PROVERSION BEGINS "1" THEN ",JBoxFUlib.p" ELSE "")
                   + (IF SEARCH("controls.dll") NE ? THEN ",controls.p" ELSE "")
                     ).

  IF NOT CAN-DO("JBoxDynMenu.w,JBoxJlwDynMenu.w,JBoxRibbonMenu.p",THIS-PROCEDURE:FILE-NAME) THEN DO:
    SUBSCRIBE TO "StartChildWindow" ANYWHERE.
    SUBSCRIBE TO "StartTabWindow"   ANYWHERE.
    SUBSCRIBE TO "StartChildTab"    ANYWHERE.
  END.
  
  DYNAMIC-FUNCTION("NewObject",SESSION,SESSION,"session").
  DYNAMIC-FUNCTION("setAttribute",SESSION,"UIB_is_Running","yes").
  
  DEF VAR hChildProcDev AS HANDLE NO-UNDO EXTENT 10.
  DEF VAR cChildProcDev AS CHAR   NO-UNDO EXTENT 10.

  PROCEDURE StartChildWindow:

    DEF INPUT  PARAM icProcName      AS CHAR   NO-UNDO.
    DEF INPUT  PARAM icWinTitle     AS CHAR   NO-UNDO.
    DEF INPUT  PARAM ihParent       AS HANDLE NO-UNDO.
    DEF INPUT  PARAM ibNew          AS LOG    NO-UNDO.
    DEF OUTPUT PARAM ohWinHandle    AS HANDLE NO-UNDO.

    DEF VAR ix         AS INT    NO-UNDO.
    DEF VAR hChildProc AS HANDLE NO-UNDO.

    IF SEARCH(icProcName) = ? AND SEARCH(SUBSTR(icProcName,1,LENGTH(icProcName) - 1) + "r") = ? THEN
      RETURN.

    DO ix = 1 TO 10:
      IF cChildProcDev[ix] = icProcName THEN DO:
        hChildProc = hChildProcDev[ix].
        LEAVE.
      END.
    END.

    IF NOT VALID-HANDLE(hChildProc) OR ibNew THEN DO:
      IF ix = 10 THEN
        DO ix = 1 TO 10:
          IF NOT VALID-HANDLE(hChildProcDev[ix]) THEN LEAVE.
        END.

      RUN VALUE(icProcName) PERSIST SET hChildProc.

      IF CAN-DO(hChildProc:INTERNAL-ENTRIES,"setParent") THEN 
        DYNAMIC-FUNCTION("setParent" IN hChildProc,ihParent).

      IF CAN-DO(hChildProc:INTERNAL-ENTRIES,"InitializeObject") THEN DO:
        IF VALID-HANDLE(ihParent) AND CAN-DO(ihParent:INTERNAL-ENTRIES,"InitializeChild") THEN
          RUN InitializeChild IN ihParent (hChildProc).
        RUN InitializeObject IN hChildProc NO-ERROR.
      END. 

      IF CAN-DO(hChildProc:INTERNAL-ENTRIES,"ChangeCompany") THEN
        DYNAMIC-FUNCTION("setCompanyHeader",hChildProc:CURRENT-WINDOW).
      ELSE
        hChildProc:CURRENT-WINDOW:TITLE = IF icWinTitle NE "" THEN icWinTitle ELSE hChildProc:CURRENT-WINDOW:TITLE.
    END.
    ELSE 
      hChildProc:CURRENT-WINDOW:TITLE = IF icWinTitle NE "" THEN icWinTitle ELSE hChildProc:CURRENT-WINDOW:TITLE.

    IF CAN-DO(hChildProc:INTERNAL-ENTRIES,"MoveToTop") THEN 
      RUN MoveToTop IN hChildProc.
    ELSE DO:
      hChildProc:CURRENT-WINDOW:WINDOW-STATE = 3.
      hChildProc:CURRENT-WINDOW:MOVE-TO-TOP().
    END.

    ohWinHandle = hChildProc.
  END PROCEDURE.
/*end.*/
  PROCEDURE StartTabWindow:
    DEF INPUT  PARAM icProcName      AS CHAR   NO-UNDO.
    DEF INPUT  PARAM icWinTitle     AS CHAR   NO-UNDO.
    DEF INPUT  PARAM ihParent       AS HANDLE NO-UNDO.
    DEF INPUT  PARAM ibNew          AS LOG    NO-UNDO.
    DEF INPUT  PARAM ibCloseTab     AS LOG    NO-UNDO.
    DEF OUTPUT PARAM ohWinHandle    AS HANDLE NO-UNDO.
    
    RUN StartChildWindow (icProcName,icWinTitle,ihParent,ibNew,OUTPUT ohWinHandle).        
  END PROCEDURE.
  
  PROCEDURE StartChildTab:
    DEF INPUT  PARAM icProcName     AS CHAR   NO-UNDO.
    DEF INPUT  PARAM icWinTitle     AS CHAR   NO-UNDO.
    DEF INPUT  PARAM ihParent       AS HANDLE NO-UNDO.
    DEF INPUT  PARAM ibNew          AS LOG    NO-UNDO.
    DEF INPUT  PARAM ibCloseTab     AS LOG    NO-UNDO.
    DEF INPUT  PARAM icDirection    AS CHAR   NO-UNDO.
    DEF OUTPUT PARAM ohWinHandle    AS HANDLE NO-UNDO.
    
    RUN StartChildWindow (icProcName,icWinTitle,ihParent,ibNew,OUTPUT ohWinHandle).        
  END PROCEDURE.
&ENDIF
