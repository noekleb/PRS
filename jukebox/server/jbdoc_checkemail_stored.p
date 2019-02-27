/* Check if a list of emails have been stored in the database
   Parameters: 
             - Message count on email server: Use when emails should not be deleted from mailserver 
             - Table of emails read from email server 
   
   Note: Custimize this procedure to check against the current document journal      
              
   Created: 24.08.09 by brynjar@chemistry.no                  
  Modified: 16.11.10 by brynjar
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR hQuery        AS HANDLE NO-UNDO.
DEF VAR cUserId       AS CHAR   NO-UNDO.
DEF VAR bOk           AS LOG    NO-UNDO.
DEF VAR iLastCount    AS INT    NO-UNDO.
DEF VAR iCompanyId    AS INT    NO-UNDO.
DEF VAR cCompanyId    AS CHAR   NO-UNDO.
DEF VAR hBuffSysParam AS HANDLE NO-UNDO.
DEF VAR dDate         AS DATE   NO-UNDO.
DEF VAR iTime         AS INT    NO-UNDO.
DEF VAR cFileName     AS CHAR   NO-UNDO.

obOK = YES.

IF icParam NE "" THEN DO:
  iLastCount = INTEGER(icParam) NO-ERROR.
  IF NOT ERROR-STATUS:ERROR THEN DO:
    CREATE BUFFER hBuffSysParam FOR TABLE "JBoxSysParam" NO-ERROR.
    IF VALID-HANDLE(hBuffSysParam) THEN DO TRANSACTION:
      iCompanyId = DYNAMIC-FUNCTION("getCompanyId" IN SOURCE-PROCEDURE) NO-ERROR.
      IF NOT ERROR-STATUS:ERROR THEN cCompanyId = STRING(iCompanyId).
      bOk = hBuffSysParam:FIND-FIRST("WHERE iJBoxCompanyId = " + cCompanyId 
                                    + " AND cSysParamName = 'LastEmailCount'",EXCLUSIVE-LOCK,NO-WAIT) NO-ERROR.
      IF bOk THEN
        hBuffSysParam:BUFFER-FIELD("iSysParamIntValue"):BUFFER-VALUE = iLastCount.
      ELSE IF NOT hBuffSysParam:LOCKED THEN DO:
        hBuffSysParam:BUFFER-CREATE().
        ASSIGN hBuffSysParam:BUFFER-FIELD("iJBoxCompanyId"):BUFFER-VALUE = iCompanyId
               hBuffSysParam:BUFFER-FIELD("cSysParamName"):BUFFER-VALUE  = "LastEmailCount"
               hBuffSysParam:BUFFER-FIELD("iSysParamIntValue"):BUFFER-VALUE = iLastCount
               .
      END.
    END.
    IF VALID-HANDLE(hBuffSysParam) AND hBuffSysParam:AVAILABLE THEN
      hBuffSysParam:FIND-CURRENT(NO-LOCK).
  END.
  IF VALID-HANDLE(hBuffSysParam) THEN
    DELETE OBJECT hBuffSysParam NO-ERROR.
END.

cUserId = DYNAMIC-FUNCTION("getAsUserId" IN SOURCE-PROCEDURE).

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " BY dDateSent BY iTimeSent").
hQuery:QUERY-OPEN().
hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
      
  /* Customize from here: */

  ASSIGN dDate      = ihBuffer:BUFFER-FIELD("dDateSent"):BUFFER-VALUE
         iTime      = ihBuffer:BUFFER-FIELD("iTimeSent"):BUFFER-VALUE
         cFileName  = ihBuffer:BUFFER-FIELD("cFileName"):BUFFER-VALUE
         .

  FOR EACH JBoxDocument FIELDS(cFileName iJBoxDocumentId) NO-LOCK
      WHERE JBoxDocument.cFileName BEGINS cFileName:
    ihBuffer:BUFFER-FIELD("cEntityId"):BUFFER-VALUE = STRING(JBoxDocument.iJBoxDocumentId).
  END.

  hQuery:GET-NEXT().
END.

DELETE OBJECT hQuery.

obOk = ocReturn = "".


