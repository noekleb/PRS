
/*------------------------------------------------------------------------
    File        : jbserv_loadserverlib.p
    Purpose     : 

    Syntax      :

    Description : (re)load server library

    Author(s)   : 
    Created     : Thu Jan 23 16:17:56 CET 2014
    Notes       :
  ----------------------------------------------------------------------*/
DEF INPUT PARAM icSessionId AS CHAR NO-UNDO.

DEF VAR hProc AS HANDLE NO-UNDO.

hProc = SESSION:FIRST-PROCEDURE.
REPEAT WHILE VALID-HANDLE(hProc):
  IF hProc:FILE-NAME = "jbserv_api_for_server.p" AND SEARCH("jbserv_api_for_server.r") NE ? THEN DO:
    FILE-INFO:FILE-NAME = SEARCH("jbserv_api_for_server.r").
    IF FILE-INFO:FILE-MOD-DATE > TODAY - 1 THEN DO:
      DELETE PROCEDURE hProc.
      hProc = ?.
      LEAVE.
    END.
    ELSE RETURN.
  END.
  hProc = hProc:NEXT-SIBLING.
END.
IF NOT VALID-HANDLE(hProc) THEN DO:
  RUN jbserv_api_for_server.p PERSIST SET hProc (icSessionId).
  SESSION:ADD-SUPER-PROC(hProc).
END.  