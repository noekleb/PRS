/* Create an event log entry. 
   Created: 04.01.07 By goo@thinc.no
----------------------------------------------------------------------------------------*/
DEF INPUT PARAM icSessionId        AS CHAR NO-UNDO. 
DEF INPUT PARAM iiEventId          AS INT  NO-UNDO.
DEF INPUT PARAM icDescription      AS CHAR NO-UNDO.
DEF INPUT PARAM icGroup            AS CHAR NO-UNDO.
DEF INPUT PARAM icType             AS CHAR NO-UNDO.
DEF INPUT PARAM icSource           AS CHAR NO-UNDO.
DEF INPUT PARAM iccategory         AS CHAR NO-UNDO.

DEF VAR hBuffer        AS HANDLE NO-UNDO.
DEF VAR bOk            AS LOG    NO-UNDO.
DEF VAR ocReturn       AS CHAR   NO-UNDO.

DEF VAR fTypeObj       AS DEC    NO-UNDO.
DEF VAR fGroupObj      AS DEC    NO-UNDO.
DEF VAR fSourceObj     AS DEC    NO-UNDO.
DEF VAR fCategoryObj   AS DEC    NO-UNDO.
DEF VAR cTableList     AS CHAR   NO-UNDO.
DEF VAR i              AS INT    NO-UNDO.
DEF VAR bh             AS HANDLE NO-UNDO.
DEF VAR cValueList     AS CHAR   NO-UNDO.


{incl/validatesession.i}


CREATE BUFFER hBuffer FOR TABLE "JBoxEventLine" NO-ERROR.
IF ERROR-STATUS:ERROR THEN RETURN.

cTableList = 'Group,Type,Source,Category'.
DO i = 1 TO NUM-ENTRIES(cTableList):
  CREATE BUFFER bh FOR TABLE 'JBoxEvent' + ENTRY(i,cTableList) NO-ERROR.
  IF VALID-HANDLE(bh) THEN
  DO:
    CASE ENTRY(i,cTableList):
      WHEN 'Group'    THEN bh:FIND-FIRST('where EventGroupName='    + QUOTER(icGroup),NO-LOCK)    NO-ERROR.
      WHEN 'Type'     THEN bh:FIND-FIRST('where EventTypeName='     + QUOTER(icType),NO-LOCK)     NO-ERROR.
      WHEN 'Source'   THEN bh:FIND-FIRST('where EventSourceName='   + QUOTER(icSource),NO-LOCK)   NO-ERROR.
      WHEN 'Category' THEN bh:FIND-FIRST('where EventCategoryName=' + QUOTER(icCategory),NO-LOCK) NO-ERROR.
    END CASE.
    cValueList = cValueList + '|' + (IF bh:AVAIL THEN STRING(bh:BUFFER-FIELD('Event' + ENTRY(i,cTableList) + 'Obj'):BUFFER-VALUE) ELSE '0').
  END.
  DELETE OBJECT bh.
END.
cValueList = TRIM(cValueList,'|').

DO TRANSACTION ON ERROR UNDO,LEAVE:
  hBuffer:BUFFER-CREATE().
  ASSIGN 
    hBuffer:BUFFER-FIELD("EventLineDate"):BUFFER-VALUE        = TODAY 
    hBuffer:BUFFER-FIELD("EventLineTime"):BUFFER-VALUE        = TIME
    hBuffer:BUFFER-FIELD("EventLineId"):BUFFER-VALUE          = iiEventId
    hBuffer:BUFFER-FIELD("EventLineDescription"):BUFFER-VALUE = icDescription           
    hBuffer:BUFFER-FIELD("cJBoxUserId"):BUFFER-VALUE          = cCurrUserId
    hBuffer:BUFFER-FIELD("iJBoxCompanyId"):BUFFER-VALUE       = iCurrCompanyId
    hBuffer:BUFFER-FIELD("EventGroupObj"):BUFFER-VALUE        = DEC(ENTRY(1,cValueList,'|'))
    hBuffer:BUFFER-FIELD("EventTypeObj"):BUFFER-VALUE         = DEC(ENTRY(2,cValueList,'|'))
    hBuffer:BUFFER-FIELD("EventSourceObj"):BUFFER-VALUE       = DEC(ENTRY(3,cValueList,'|'))
    hBuffer:BUFFER-FIELD("EventCategoryObj"):BUFFER-VALUE     = DEC(ENTRY(4,cValueList,'|'))
  NO-ERROR.
END.

DELETE OBJECT hBuffer.

