DEF INPUT        PARAM iiCompanyId         AS INT  NO-UNDO.
DEF INPUT        PARAM icEventType         AS CHAR NO-UNDO.
DEF INPUT        PARAM icEventText         AS CHAR NO-UNDO.
DEF INPUT        PARAM icUserId            AS CHAR NO-UNDO.
DEF INPUT        PARAM icEntityIdFields    AS CHAR NO-UNDO.
DEF INPUT        PARAM icEntityIdValues    AS CHAR NO-UNDO.
DEF INPUT        PARAM icTableName         AS CHAR NO-UNDO.
DEF INPUT        PARAM idEventDate         AS DATE NO-UNDO.
DEF INPUT        PARAM iiEventTime         AS INT  NO-UNDO.
DEF INPUT        PARAM icExtra             AS CHAR NO-UNDO.
DEF INPUT-OUTPUT PARAM ioiEventLogHdrId    AS INT  NO-UNDO.

DEF VAR hBuffLogHeader  AS HANDLE NO-UNDO.
DEF VAR hFldLogHeaderId AS HANDLE NO-UNDO.

hFldLogHeaderId = BUFFER JBoxEventLog:BUFFER-FIELD("iJBoxEventLogHeaderId") NO-ERROR.

FIND FIRST JBoxEventLogType NO-LOCK
     WHERE JBoxEventLogType.cEventLogType = icEventType
     NO-ERROR.
IF NOT AVAIL JBoxEventLogType THEN DO:
  CREATE JBoxEventLogType.
  ASSIGN JBoxEventLogType.cEventLogType     = icEventType
         JBoxEventLogType.cEventLogTypeText = icEventType
         JBoxEventLogType.dCreated          = TODAY
         JBoxEventLogType.cCreatedBy        = icUserId
         .
END.

CREATE JBoxEventLog.
ASSIGN JBoxEventLog.iJBoxCompanyId        = iiCompanyId
       JBoxEventLog.cEventLogType         = icEventType
       JBoxEventLog.cEventText            = icEventText
       JBoxEventLog.cEntityIdFields       = icEntityIdFields
       JBoxEventLog.cEntityId             = icEntityIdValues
       JBoxEventLog.cEntityTable          = icTableName
       JBoxEventLog.dEventDate            = idEventDate
       JBoxEventLog.iEventTime            = iiEventTime
       JBoxEventLog.dCreated              = TODAY
       JBoxEventLog.cCreatedBy            = icUserId
       JBoxEventLog.cEventStatus          = "NEW"
       .

IF VALID-HANDLE(hFldLogHeaderId) THEN DO:
  IF ioiEventLogHdrId = 0 THEN DO:
    CREATE BUFFER hBuffLogHeader FOR TABLE "JBoxEventLogHeader" NO-ERROR.
    IF VALID-HANDLE(hBuffLogHeader) THEN DO:
      hBuffLogHeader:BUFFER-CREATE().
      ASSIGN hBuffLogHeader:BUFFER-FIELD("cEventHeaderText"):BUFFER-VALUE = icEventText
             hBuffLogHeader:BUFFER-FIELD("cExtraText"):BUFFER-VALUE       = icExtra
             ioiEventLogHdrId              = hBuffLogHeader:BUFFER-FIELD("iJBoxEventLogHeaderId"):BUFFER-VALUE
             hFldLogHeaderId:BUFFER-VALUE  = ioiEventLogHdrId
             .
      hBuffLogHeader:BUFFER-COPY(BUFFER JBoxEventLog:HANDLE).
    END.
  END.
  ELSE IF ioiEventLogHdrId NE ? THEN
    hFldLogHeaderId:BUFFER-VALUE = ioiEventLogHdrId.
END.

RETURN STRING(JBoxEventLog.iJBoxEventLogId).
