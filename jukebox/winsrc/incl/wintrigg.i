/* wintrigg.i Common triggers + includes for customisation */

/*{incl/initproc.i}*/
 
/* ON 'F1':U OF FRAME {&FRAME-NAME} ANYWHERE DO:  */
/*   RUN Help NO-ERROR.                           */
/* END.                                           */
/*                                                                                                               */
ON "WINDOW-RESIZED" OF {&WINDOW-NAME} DO:
  DYNAMIC-FUNCTION("ResizeKeepsWindowLocked",THIS-PROCEDURE:CURRENT-WINDOW) NO-ERROR.
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"resize","").
  DYNAMIC-FUNCTION("ResizeKeepsWindowLocked",?) NO-ERROR.
END.




   
ON "SHIFT-CTRL-ALT-T" OF FRAME {&FRAME-NAME} ANYWHERE DO:
  DEF VAR hJBoxTranMan AS HANDLE NO-UNDO.
  
  hJBoxTranMan = SESSION:FIRST-PROCEDURE.
  REPEAT WHILE VALID-HANDLE(hJBoxTranMan):
    IF hJBoxTranMan:FILE-NAME = "JBoxTranMan.w" THEN DO:
      APPLY "close" TO hJBoxTranMan.
      hJBoxTranMan = ?.
      LEAVE.
    END.
    hJBoxTranMan = hJBoxTranMan:NEXT-SIBLING.
  END.
  IF NOT VALID-HANDLE(hJBoxTranMan) THEN DO:
    RUN JBoxTranMan.w PERSIST SET hJBoxTranMan.
    IF VALID-HANDLE(hJBoxTranMan) THEN
      hJBoxTranMan:CURRENT-WINDOW:MOVE-TO-TOP().
  END.
  ELSE hJBoxTranMan:CURRENT-WINDOW:MOVE-TO-TOP().

  PUBLISH "BuildScreenObjects" (THIS-PROCEDURE:CURRENT-WINDOW).
END.

ON "SHIFT-CTRL-T" OF FRAME {&FRAME-NAME} ANYWHERE
  PUBLISH "BuildScreenObjects" (THIS-PROCEDURE:CURRENT-WINDOW).

ON "SHIFT-CTRL-ALT-F2" OF FRAME {&FRAME-NAME} ANYWHERE
  MESSAGE PROGRAM-NAME(1) SKIP  
          VIEW-AS ALERT-BOX.

ON "SHIFT-CTRL-ALT-F3" ANYWHERE DO:
  DEF VAR hSessProc AS HANDLE NO-UNDO.
  
  hSessProc = SESSION:FIRST-PROCEDURE.
  REPEAT WHILE VALID-HANDLE(hSessProc):
    IF hSessProc:FILE-NAME = "protools/_ppmgr.w" THEN DO:
      APPLY "close" TO hSessProc.
      hSessProc = ?.
      LEAVE.
    END.
    hSessProc = hSessProc:NEXT-SIBLING.
  END.
  IF hSessProc = ? AND SEARCH("protools/_ppmgr.r") NE ? THEN DO:
    RUN protools/_ppmgr.w PERSIST SET hSessProc.
    hSessProc:CURRENT-WINDOW:MOVE-TO-TOP().
  END.
  ELSE IF VALID-HANDLE(hSessProc) THEN hSessProc:CURRENT-WINDOW:MOVE-TO-TOP().
END.

ON "SHIFT-CTRL-ALT-F4" ANYWHERE 
  DYNAMIC-FUNCTION("dumpMemInfo").

ON "SHIFT-CTRL-ALT-F5" ANYWHERE DO:
  DEF VAR hJBoxObjectViewer AS HANDLE NO-UNDO.
  
  hJBoxObjectViewer = SESSION:FIRST-PROCEDURE.
  REPEAT WHILE VALID-HANDLE(hJBoxObjectViewer):
    IF hJBoxObjectViewer:FILE-NAME = "JBoxObjectViewer.w" THEN DO:
      APPLY "close" TO hJBoxObjectViewer.
      hJBoxObjectViewer = ?.
      LEAVE.
    END.
    hJBoxObjectViewer = hJBoxObjectViewer:NEXT-SIBLING.
  END.
  IF hJBoxObjectViewer = ? THEN DO:
    RUN JBoxObjectViewer.w PERSIST SET hJBoxObjectViewer.
    hJBoxObjectViewer:CURRENT-WINDOW:MOVE-TO-TOP().
  END.
  ELSE hJBoxObjectViewer:CURRENT-WINDOW:MOVE-TO-TOP().
END.

ON "SHIFT-CTRL-ALT-F6" ANYWHERE DO:
  DEF VAR hDictView AS HANDLE NO-UNDO.
  
  IF SEARCH("dictview.w") = ? THEN RETURN NO-APPLY.

  hDictView = SESSION:FIRST-PROCEDURE.
  REPEAT WHILE VALID-HANDLE(hDictView):
    IF hDictView:FILE-NAME = "dictview.w" THEN DO:
      APPLY "close" TO hDictView.
      hDictView = ?.
      LEAVE.
    END.
    hDictView = hDictView:NEXT-SIBLING.
  END.
  IF hDictView = ? THEN DO:
    RUN dictview.w PERSIST SET hDictView.
    hDictView:CURRENT-WINDOW:MOVE-TO-TOP().
  END.
  ELSE hDictView:CURRENT-WINDOW:MOVE-TO-TOP().
END.

ON "SHIFT-CTRL-ALT-F7" ANYWHERE DO:
  DEF VAR bServerLogs AS LOG NO-UNDO.
  MESSAGE "Log queries and transactions to ascii-files on server" 
          VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE bServerLogs.

  IF bServerLogs THEN
    DYNAMIC-FUNCTION("setASlibBehaviour",
                      "QueryLogFile|ServerQuery.log," +   
                      "TransLogFile|ServerTrans.log").
  ELSE
    DYNAMIC-FUNCTION("setASlibBehaviour",
                      "QueryLogFile|," +   
                      "TransLogFile|"). 
END.

ON "SHIFT-CTRL-ALT-F8" ANYWHERE DO:
  DEF VAR hAppComp AS HANDLE NO-UNDO.
  
  IF SEARCH("AppComp.w") = ? THEN RETURN NO-APPLY.

  hAppComp = SESSION:FIRST-PROCEDURE.
  REPEAT WHILE VALID-HANDLE(hAppComp):
    IF hAppComp:FILE-NAME = "AppComp.w" THEN DO:
      APPLY "close" TO hAppComp.
      hAppComp = ?.
      LEAVE.
    END.
    hAppComp = hAppComp:NEXT-SIBLING.
  END.
  IF hAppComp = ? THEN DO:
    RUN AppComp.w PERSIST SET hAppComp.
    hAppComp:CURRENT-WINDOW:MOVE-TO-TOP().
  END.
  ELSE hAppComp:CURRENT-WINDOW:MOVE-TO-TOP().
END.

ON "SHIFT-CTRL-ALT-F9" ANYWHERE DO:
  DEF VAR hLogMethods AS HANDLE NO-UNDO.
  
  IF SEARCH("LogMethods.w") = ? THEN RETURN NO-APPLY.

  hLogMethods = SESSION:FIRST-PROCEDURE.
  REPEAT WHILE VALID-HANDLE(hLogMethods):
    IF hLogMethods:FILE-NAME = "LogMethods.w" THEN DO:
      APPLY "close" TO hLogMethods.
      hLogMethods = ?.
      LEAVE.
    END.
    hLogMethods = hLogMethods:NEXT-SIBLING.
  END.
  IF hLogMethods = ? THEN DO:
    RUN LogMethods.w PERSIST SET hLogMethods.
    hLogMethods:CURRENT-WINDOW:MOVE-TO-TOP().
  END.
  ELSE hLogMethods:CURRENT-WINDOW:MOVE-TO-TOP().
END.
