/* Find related record for relation from JBoxProcess to event-type, etc (records that may relate to a specific workflow)
   Created 04.01.12 by brynjar@chemistry.no
-----------------------------------------------------------*/   

DEF INPUT PARAM  irProcessRel AS ROWID NO-UNDO.
DEF INPUT PARAM  icSessionId  AS CHAR  NO-UNDO.
DEF OUTPUT PARAM ocReturn     AS CHAR  NO-UNDO.

DEF VAR hBuffContext AS HANDLE NO-UNDO.
DEF VAR bOk          AS LOG    NO-UNDO.

FIND JBoxProcessRel NO-LOCK
     WHERE ROWID(JBoxProcessRel) = irProcessRel
     NO-ERROR.
IF AVAIL JBoxProcessRel THEN DO:
  CREATE BUFFER hBuffContext FOR TABLE JBoxProcessRel.cContext NO-ERROR.
  IF VALID-HANDLE(hBuffContext) THEN 
    CASE JBoxProcessRel.cContext:
      WHEN "JBoxEmailAccount" THEN DO:
        bOk = hBuffContext:FIND-FIRST("WHERE iJBoxEmailAccountId = " + JBoxProcessRel.cEntityId,NO-LOCK) NO-ERROR.
        IF bOk THEN
          ocReturn = hBuffContext:BUFFER-FIELD("cEmailAddress"):BUFFER-VALUE.
      END.
      WHEN "JBoxEventLogType" THEN DO:
        bOk = hBuffContext:FIND-FIRST("WHERE cEventLogType = '" + JBoxProcessRel.cEntityId + "'",NO-LOCK) NO-ERROR.
        IF bOk THEN
          ocReturn = hBuffContext:BUFFER-FIELD("cEventLogTypeText"):BUFFER-VALUE.
      END.
    END CASE.

END.

