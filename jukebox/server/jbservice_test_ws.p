/* Run an ad-hoc ABL report
-----------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR cWsProg      AS CHAR     NO-UNDO.
DEF VAR cWsParam     AS CHAR     NO-UNDO.
DEF VAR lcResult     AS LONGCHAR NO-UNDO.
DEF VAR cFileName    AS CHAR     NO-UNDO.
DEF VAR hJbAPI       AS HANDLE   NO-UNDO.
DEF VAR cRepLine     AS CHAR     NO-UNDO.

ASSIGN cWsProg  = ENTRY(1,icParam,";")
       cWsParam = ENTRY(2,icParam,";")
       .

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
  cWsParam = ENTRY(2,icParam,";").

IF SEARCH(SUBSTR(cWsProg,1,R-INDEX(cWsProg,".")) + "p") NE ? OR 
   SEARCH(SUBSTR(cWsProg,1,R-INDEX(cWsProg,".")) + "r") NE ? OR 
   SEARCH(SUBSTR(cWsProg,1,R-INDEX(cWsProg,".")) + "w") NE ? THEN DO:

  IF cWsProg = "NifuOrgEndr.p" THEN
    RUN VALUE(cWsProg) (DATE(cWsParam),"report","",OUTPUT lcResult,OUTPUT obOK,OUTPUT ocReturn) NO-ERROR.
  ELSE
    RUN VALUE(cWsProg) (cWsParam + "|report","","",OUTPUT lcResult,OUTPUT obOK,OUTPUT ocReturn) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN
    RUN VALUE(cWsProg).

  IF obOK THEN DO:
    IF cFileName = "" THEN 
      cFileName = DYNAMIC-FUNCTION("getUniqueFileName") + ".txt".
    OUTPUT TO VALUE(cFileName).
    EXPORT lcResult.
    OUTPUT CLOSE.
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
ELSE ocReturn = "Missing program " + cWsProg.

obOK = ocReturn = "".
IF obOK THEN
  ocReturn = ENTRY(NUM-ENTRIES(cFileName,"."),cFileName,".").
