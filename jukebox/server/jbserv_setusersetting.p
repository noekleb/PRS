/* Set a user setting. 
   Created: 07.08.06 By brynjar@chemistry.no
----------------------------------------------------------------------------------------*/
DEF INPUT  PARAM icSessionId        AS CHAR NO-UNDO. 
DEF INPUT  PARAM icSourceFile       AS CHAR NO-UNDO.
DEF INPUT  PARAM icObjectName       AS CHAR NO-UNDO.
DEF INPUT  PARAM icContext          AS CHAR NO-UNDO.
DEF INPUT  PARAM icSettingName      AS CHAR NO-UNDO.
DEF INPUT  PARAM icSetting          AS CHAR NO-UNDO.

DEF VAR hBuffer    AS HANDLE NO-UNDO.
DEF VAR bOk        AS LOG    NO-UNDO.
DEF VAR ocReturn   AS CHAR   NO-UNDO.
DEF VAR ix         AS INT    NO-UNDO.

{incl/validatesession.i}

CREATE BUFFER hBuffer FOR TABLE "JBoxUserSetting" NO-ERROR.
IF ERROR-STATUS:ERROR THEN RETURN "No setting table".

IF icSourceFile = "" AND icObjectName = "" AND icContext = "" AND icSettingName = "" THEN
  RETURN "".

DO TRANSACTION ON ERROR UNDO,LEAVE:
  DO ix = 1 TO NUM-ENTRIES(icSettingName):
    bOk = hBuffer:FIND-FIRST("WHERE cSourceFile  = '" + icSourceFile + "'"
                            + " AND cObjectName  = '" + icObjectName + "'"
                            + " AND cContext     = '" + icContext + "'"
                            + " AND cSettingName = '" + ENTRY(ix,icSettingName) + "'"
                            + " AND cJBoxUserId  = '" + cCurrUserId + "'",EXCLUSIVE-LOCK) NO-ERROR.
    IF bOk AND icSetting = "delete_setting" THEN
      hBuffer:BUFFER-DELETE().
    ELSE IF icSetting NE "delete_setting" THEN DO:
      IF NOT bOk THEN DO:
        hBuffer:BUFFER-CREATE().
        ASSIGN hBuffer:BUFFER-FIELD("cSourceFile"):BUFFER-VALUE  = icSourceFile
               hBuffer:BUFFER-FIELD("cObjectName"):BUFFER-VALUE  = icObjectName
               hBuffer:BUFFER-FIELD("cContext"):BUFFER-VALUE     = icContext
               hBuffer:BUFFER-FIELD("cSettingName"):BUFFER-VALUE = ENTRY(ix,icSettingName)
               hBuffer:BUFFER-FIELD("cJBoxUserId"):BUFFER-VALUE  = cCurrUserId
               hBuffer:BUFFER-FIELD("dCreated"):BUFFER-VALUE     = TODAY
               hBuffer:BUFFER-FIELD("cCreatedBy"):BUFFER-VALUE   = cCurrUserId
               .
      END.
      ELSE ASSIGN
        hBuffer:BUFFER-FIELD("dModified"):BUFFER-VALUE     = TODAY
        hBuffer:BUFFER-FIELD("cModifiedBy"):BUFFER-VALUE   = cCurrUserId.
      
      hBuffer:BUFFER-FIELD("cSetting"):BUFFER-VALUE = ENTRY(ix,icSetting,CHR(1)).
    END.
  END.
END.

DELETE OBJECT hBuffer.

RETURN "".
