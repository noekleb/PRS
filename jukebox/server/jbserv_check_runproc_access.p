DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF INPUT  PARAM icFileName  AS CHAR NO-UNDO.
DEF INPUT  PARAM iiCompanyId AS INT  NO-UNDO.
DEF INPUT  PARAM icUserId    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obAccess    AS LOG  NO-UNDO INIT YES.


DEF VAR cUserLevel           AS CHAR   NO-UNDO.
DEF VAR bOk                  AS LOG    NO-UNDO.
DEF VAR hBuffFunction        AS HANDLE NO-UNDO.

CREATE BUFFER hBuffFunction FOR TABLE "JBoxFunction" NO-ERROR.
IF NOT VALID-HANDLE(hBuffFunction) THEN RETURN.

RUN jbadmin_getuserlevel.p (icSessionId,iiCompanyId,OUTPUT cUserLevel,OUTPUT bOK).
IF cUserLevel NE "super" THEN DO:
  bOk = hBuffFunction:FIND-FIRST("WHERE cServerFileName = '" + icFileName + "'",NO-LOCK) NO-ERROR.
  
  IF bOk THEN 
    RUN jbserv_check_runproc_access2.p (hBuffFunction:BUFFER-FIELD("iJBoxFunctionId"):BUFFER-VALUE,icUserId,OUTPUT obAccess) NO-ERROR.
END.

DELETE OBJECT hBuffFunction NO-ERROR.


