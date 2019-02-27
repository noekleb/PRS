/* jbserv_getusermessage.p
   Lookup user message.
   For server-side usage the procedure should be called via jbserv_domessage.p
   From the client the procedure is automatically invoked from the DoMessage function in JBoxUIlib.p
   
   Created 16.08.10 by brynjar@chemistry.no
--------------------------------------------------------------------------*/   
DEF INPUT  PARAM icParam         AS CHAR   NO-UNDO.
DEF INPUT  PARAM ihBuffer        AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId     AS CHAR   NO-UNDO.
DEF OUTPUT PARAM ocReturn        AS CHAR   NO-UNDO.
DEF OUTPUT PARAM obOk            AS LOG    NO-UNDO.

DEF VAR cMsgNum          AS CHAR   NO-UNDO.
DEF VAR iMsgNo           AS INT    NO-UNDO.
DEF VAR cMessage         AS CHAR   NO-UNDO.
DEF VAR cTitle           AS CHAR   NO-UNDO.
DEF VAR cData            AS CHAR   NO-UNDO.
DEF VAR cLanguageCode    AS CHAR   NO-UNDO.
DEF VAR ix               AS INT    NO-UNDO.
DEF VAR hBuffJBoxMessage AS HANDLE NO-UNDO.

ASSIGN cMsgNum       = ENTRY(1,icParam,"|")
       iMsgNo        = INT(cMsgNum)
       cMessage      = ENTRY(2,icParam,"|")
       cTitle        = ENTRY(3,icParam,"|")                                                         
       cLanguageCode = DYNAMIC-FUNCTION("getLanguageCode" IN SOURCE-PROCEDURE)       
       NO-ERROR.

IF NOT ERROR-STATUS:ERROR THEN DO:
  IF iMsgNo NE 0 THEN DO:
    IF cLanguageCode = "" THEN cLanguageCode = "NO".
    CREATE BUFFER hBuffJBoxMessage FOR TABLE "JBoxMessage" NO-ERROR.
    IF VALID-HANDLE(hBuffJBoxMessage) THEN DO:
      hBuffJBoxMessage:FIND-FIRST("WHERE iMsgNo = " + cMsgNum + " AND cLanguage = '" + cLanguageCode + "'", NO-LOCK) NO-ERROR.
      IF hBuffJBoxMessage:AVAIL AND hBuffJBoxMessage:BUFFER-FIELD("cMessage"):BUFFER-VALUE NE "" THEN
        ocReturn = hBuffJBoxMessage:BUFFER-FIELD("cMessage"):BUFFER-VALUE + "  ( " + cMsgNum + " )" + "|" 
                 + hBuffJBoxMessage:BUFFER-FIELD("cTitle"):BUFFER-VALUE
                   .
      ELSE IF cLanguageCode NE "NO" THEN DO:
        cLanguageCode = "NO".
        hBuffJBoxMessage:FIND-FIRST("WHERE iMsgNo = " + cMsgNum + " AND cLanguage = '" + cLanguageCode + "'", NO-LOCK) NO-ERROR.
        IF hBuffJBoxMessage:AVAIL AND hBuffJBoxMessage:BUFFER-FIELD("cMessage"):BUFFER-VALUE NE "" THEN
          ocReturn = hBuffJBoxMessage:BUFFER-FIELD("cMessage"):BUFFER-VALUE + "  ( " + cMsgNum + " )" + "|" 
                   + hBuffJBoxMessage:BUFFER-FIELD("cTitle"):BUFFER-VALUE
                     .
      END.
      DELETE OBJECT hBuffJBoxMessage NO-ERROR.
    END.
  END.
END.

IF ocReturn = "" THEN ocReturn = cMessage + "|" + cTitle.
   

