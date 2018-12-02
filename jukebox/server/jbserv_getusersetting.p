/* Get a user setting. 
   Created: 07.08.06 By brynjar@chemistry.no
----------------------------------------------------------------------------------------*/
DEF INPUT  PARAM icSessionId        AS CHAR NO-UNDO. 
DEF INPUT  PARAM icSourceFile       AS CHAR NO-UNDO.
DEF INPUT  PARAM icObjectName       AS CHAR NO-UNDO.
DEF INPUT  PARAM icContext          AS CHAR NO-UNDO.
DEF INPUT  PARAM icSettingName      AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn           AS CHAR NO-UNDO.

DEF VAR hBuffer      AS HANDLE NO-UNDO.
DEF VAR bOk          AS LOG    NO-UNDO INIT YES.
DEF VAR hQuery       AS HANDLE NO-UNDO.
DEF VAR cSettingList AS CHAR   NO-UNDO.

{incl/validatesession.i}

CREATE BUFFER hBuffer FOR TABLE "JBoxUserSetting" NO-ERROR.
IF ERROR-STATUS:ERROR THEN RETURN "No setting table".

IF icSourceFile = "" AND icObjectName = "" AND icContext = "" AND icSettingName = "" THEN
  RETURN "Invalid qualifier".

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(hBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + hBuffer:NAME + " USE-INDEX idxUserId "
                    + " WHERE cSourceFile  = '" + icSourceFile + "'"
                    + "   AND cObjectName  = '" + icObjectName + "'"
                    + "   AND cContext     = '" + icContext + "'"
                    + "   AND CAN-DO('" + icSettingName + "',cSettingName)"
                    + "   AND cJBoxUserId  = '" + cCurrUserId + "'") NO-ERROR.
IF ERROR-STATUS:GET-MESSAGE(1) = "" THEN DO:
  hQuery:QUERY-OPEN().
  hQuery:GET-FIRST().
  REPEAT WHILE NOT hQuery:QUERY-OFF-END:
    ASSIGN ocReturn     = ocReturn + hBuffer:BUFFER-FIELD("cSetting"):BUFFER-VALUE + CHR(1)
           cSettingList = cSettingList + hBuffer:BUFFER-FIELD("cSettingName"):BUFFER-VALUE + ",".
    hQuery:GET-NEXT().
  END.
END.
ELSE bOk = NO.

ASSIGN ocReturn     = SUBSTR(ocReturn,1,LENGTH(ocReturn) - 1)
       cSettingList = TRIM(cSettingList,","). 

IF hQuery:NUM-RESULTS > 1 THEN
  ocReturn = cSettingList + "|" + ocReturn.

DELETE OBJECT hQuery.
DELETE OBJECT hBuffer.

IF NOT bOk THEN
  RETURN "Invalid query for user setting".
ELSE IF NUM-ENTRIES(ocReturn,"|") = 1 THEN
  RETURN cSettingList.
ELSE RETURN "".
