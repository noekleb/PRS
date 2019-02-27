/* Run an ad-hoc ABL report
-----------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR cReportProg  AS CHAR   NO-UNDO.
DEF VAR cProgParam   AS CHAR   NO-UNDO.
DEF VAR cFileName    AS CHAR   NO-UNDO.
DEF VAR hJbAPI       AS HANDLE NO-UNDO.
DEF VAR cRepLine     AS CHAR   NO-UNDO.

cReportProg = ENTRY(1,icParam,";").

FUNCTION getFileName RETURNS CHARACTER():
  RETURN cFileName.
END FUNCTION.

FUNCTION setFileName RETURNS LOGICAL(INPUT icFileName AS CHAR):
  cFileName = icFileName.
  RETURN YES.
END FUNCTION.

DYNAMIC-FUNCTION("startASlib" IN SOURCE-PROCEDURE).
hJbAPI = DYNAMIC-FUNCTION("getAsLibHandle" IN SOURCE-PROCEDURE).

FUNCTION getAsLibHandle RETURNS HANDLE():
  RETURN hJbAPI.
END FUNCTION.


FUNCTION startASlib RETURNS LOGICAL ():
  SOURCE-PROCEDURE:ADD-SUPER-PROCEDURE(hJbAPI).
  RETURN YES.
END.

IF NUM-ENTRIES(icParam,";") > 1 THEN
  cProgParam = ENTRY(2,icParam,";").

IF SEARCH(SUBSTR(cReportProg,1,R-INDEX(cReportProg,".")) + "p") NE ? OR 
   SEARCH(SUBSTR(cReportProg,1,R-INDEX(cReportProg,".")) + "r") NE ? OR 
   SEARCH(SUBSTR(cReportProg,1,R-INDEX(cReportProg,".")) + "w") NE ? THEN DO:

  RUN VALUE(cReportProg) (cProgParam) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN
    RUN VALUE(cReportProg).

  cFileName = DYNAMIC-FUNCTION("getReportFileName").
  IF cFileName = "" OR SEARCH(cFileName) = ? THEN
    ocReturn = "Report file name not set in " + cReportProg.
  ELSE DO:
    INPUT FROM VALUE(cFileName).
    REPEAT:
      IMPORT UNFORMATTED cRepLine.
      ihBuffer:BUFFER-CREATE().
      ihBuffer:BUFFER-FIELD("cLine"):BUFFER-VALUE = cRepLine.
    END.
    INPUT CLOSE.
    
    OS-DELETE VALUE(cFileName).
  END.
END.
ELSE ocReturn = "Missing program " + cReportProg.

obOK = ocReturn = "".
IF obOK THEN
  ocReturn = ENTRY(NUM-ENTRIES(cFileName,"."),cFileName,".").
