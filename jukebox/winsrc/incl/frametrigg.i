/* frametrigg.i Common triggers - for Dialog Boxes */

ON "SHIFT-CTRL-T" OF FRAME {&FRAME-NAME} ANYWHERE
  PUBLISH "BuildScreenObjects" (FRAME {&FRAME-NAME}:HANDLE).

ON "SHIFT-CTRL-ALT-F2" OF FRAME {&FRAME-NAME} ANYWHERE
  MESSAGE PROGRAM-NAME(1) SKIP  
          VIEW-AS ALERT-BOX.

ON "SHIFT-CTRL-ALT-F3" ANYWHERE DO:
  DEF VAR hSessProc AS HANDLE NO-UNDO.
  
  hSessProc = SESSION:FIRST-PROCEDURE.
  REPEAT WHILE VALID-HANDLE(hSessProc):
    IF hSessProc:FILE-NAME = "protools/_ppmgr.w" THEN DO:
      APPLY "close" TO hSessProc.
      LEAVE.
    END.
    hSessProc = hSessProc:NEXT-SIBLING.
  END.
  IF hSessProc = ? THEN
    RUN protools/_ppmgr.w PERSIST.END.
END.
