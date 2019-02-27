/* Use to create JBoxEventLog from client
   Parameters, pipe-separated: 
   - EventType (mandatory)
   - EventText 
   - EventDate (date, event)
   - EventTime (time, event)
   - Name (source table)
   - ROWID (source record)
   - Field names for reference
   - Extra  
   
   Can also be used with table input. Required fields are
   - RowIdent1 (CHARACTER)
   If the table contains events for more than one link-source table these field must also be included
   - cSourceTable
   - cKeyFieldList
   Optionally the event type, text and extra text can be included in the table using these columns
   - cEventType
   - cEventText
   - cEventExtraText
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR   NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR   NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR   NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG    NO-UNDO. 

DEF VAR cEventType     AS CHAR   NO-UNDO.
DEF VAR cEventText     AS CHAR   NO-UNDO.
DEF VAR dEventDate     AS DATE   NO-UNDO.
DEF VAR iEventTime     AS INT    NO-UNDO.
DEF VAR cSourceTable   AS CHAR   NO-UNDO.
DEF VAR cRowId         AS CHAR   NO-UNDO.
DEF VAR cKeyFieldList  AS CHAR   NO-UNDO.
DEF VAR cFieldValues   AS CHAR   NO-UNDO.
DEF VAR cExtra         AS CHAR   NO-UNDO.
DEF VAR ix             AS INT    NO-UNDO.
DEF VAR bOk            AS LOG    NO-UNDO.
DEF VAR hBuffer        AS HANDLE NO-UNDO.
DEF VAR hField         AS HANDLE NO-UNDO EXTENT 10.
DEF VAR hQuery         AS HANDLE NO-UNDO.
DEF VAR hRowIdent1Fld  AS HANDLE NO-UNDO.
DEF VAR hEventTypeFld  AS HANDLE NO-UNDO.
DEF VAR hEventTextFld  AS HANDLE NO-UNDO.
DEF VAR hEventExtraFld AS HANDLE NO-UNDO.
DEF VAR hSourceTabFld  AS HANDLE NO-UNDO.
DEF VAR hKeyFldListFld AS HANDLE NO-UNDO.
DEF VAR iEventLogHdrId AS INT    NO-UNDO.

DO ix = 1 TO NUM-ENTRIES(icParam,"|"):
  CASE ix:
    WHEN 1 THEN cEventType     = ENTRY(ix,icParam,"|") NO-ERROR.
    WHEN 2 THEN cEventText     = ENTRY(ix,icParam,"|") NO-ERROR.
    WHEN 3 THEN dEventDate     = DATE(ENTRY(ix,icParam,"|")) NO-ERROR.
    WHEN 4 THEN iEventTime     = INT(ENTRY(ix,icParam,"|")) NO-ERROR.
    WHEN 5 THEN cSourceTable   = ENTRY(ix,icParam,"|") NO-ERROR.
    WHEN 6 THEN cRowId         = ENTRY(ix,icParam,"|") NO-ERROR.
    WHEN 7 THEN cKeyFieldList  = ENTRY(ix,icParam,"|") NO-ERROR.
    WHEN 8 THEN cExtra         = ENTRY(ix,icParam,"|") NO-ERROR.
    WHEN 9 THEN iEventLogHdrId = INTEGER(ENTRY(ix,icParam,"|")) NO-ERROR.
  END CASE.
  IF ERROR-STATUS:GET-MESSAGE(1) NE "" THEN DO:
    ocReturn = "Invalid parameter for event-log entry: " + ENTRY(ix,icParam,"|") + CHR(10) + PROGRAM-NAME(1).
    RETURN.
  END.
END.
IF dEventDate = ? THEN dEventDate = TODAY.
IF iEventTime = 0 THEN iEventTime = TIME.

IF cEventType = "" THEN
  ocReturn = "Event-type mandatory when creating an event log entry: " + PROGRAM-NAME(1).

IF cSourceTable NE "" THEN DO:
  CREATE BUFFER hBuffer FOR TABLE cSourceTable NO-ERROR.
  IF NOT VALID-HANDLE(hBuffer) THEN DO:
    ocReturn = "Invalid table reference: " + cSourceTable + CHR(10) +
               PROGRAM-NAME(1).
    RETURN.
  END. 
END.
    
IF cSourceTable NE "" AND cRowId NE "" THEN DO:
  bOk = hBuffer:FIND-BY-ROWID(TO-ROWID(cRowId),NO-LOCK) NO-ERROR.  
  IF bOk THEN DO ix = 1 TO NUM-ENTRIES(cKeyFieldList):
    hField[ix] = hBuffer:BUFFER-FIELD(ENTRY(ix,cKeyFieldList)) NO-ERROR.
    IF NOT VALID-HANDLE(hField[ix]) THEN DO:
      ocReturn = "Invalid field reference: " + ENTRY(ix,cKeyFieldList) + CHR(10) +
                 PROGRAM-NAME(1).
      LEAVE.
    END.
    cFieldValues = cFieldValues + (IF hField[ix]:BUFFER-VALUE = ? THEN "?" ELSE STRING(hField[ix]:BUFFER-VALUE)) + "|".
  END.
  ELSE ocReturn = "Record not found for creation of event-log entry for table: " + cSourceTable.

  IF cFieldValues NE "" THEN
    cFieldValues = SUBSTR(cFieldValues,1,LENGTH(cFieldValues) - 1).

  IF ocReturn = "" THEN DO:
    RUN jbadmin_cre_eventlog.p (DYNAMIC-FUNCTION("getCompanyId" IN SOURCE-PROCEDURE),
                                cEventType,
                                cEventText,
                                DYNAMIC-FUNCTION("getASuserId" IN SOURCE-PROCEDURE),
                                cKeyFieldList,
                                cFieldValues,
                                cSourceTable,
                                dEventDate,
                                iEventTime,
                                cExtra,
                                INPUT-OUTPUT iEventLogHdrId
                                ).
  END.
END.
ELSE IF VALID-HANDLE(ihBuffer) THEN 
  Oppdatering:
  DO TRANSACTION ON ERROR UNDO,LEAVE:
    CREATE QUERY hQuery.
    hQuery:SET-BUFFERS(ihBuffer).
    hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " BY cSourceTable") NO-ERROR.
    IF ERROR-STATUS:GET-MESSAGE(1) NE "" THEN
      hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME) NO-ERROR.
    hQuery:QUERY-OPEN().
    hQuery:GET-FIRST().
  
    hRowIdent1Fld  = ihBuffer:BUFFER-FIELD("RowIdent1") NO-ERROR.
    hEventTypeFld  = ihBuffer:BUFFER-FIELD("cEventType") NO-ERROR.
    hEventTextFld  = ihBuffer:BUFFER-FIELD("cEventText") NO-ERROR.
    hEventExtraFld = ihBuffer:BUFFER-FIELD("cEventExtraText") NO-ERROR.
    hSourceTabFld  = ihBuffer:BUFFER-FIELD("cSourceTable") NO-ERROR.
    hKeyFldListFld = ihBuffer:BUFFER-FIELD("cKeyFieldList") NO-ERROR.
  
    IF cSourceTable = "" AND NOT VALID-HANDLE(hSourceTabFld) THEN
      ocReturn = "Invalid source table reference in eventlog source table".
    IF NOT VALID-HANDLE(hRowIdent1Fld) THEN
      ocReturn = "Invalid reference for mandatory field RowIdent1 in eventlog source table".
         
    IF ocReturn = "" THEN
      REPEAT WHILE NOT hQuery:QUERY-OFF-END:
        IF VALID-HANDLE(hSourceTabFld) AND hSourceTabFld:BUFFER-VALUE NE cSourceTable THEN DO:
          cSourceTable = hSourceTabFld:BUFFER-VALUE.
          DELETE OBJECT hBuffer NO-ERROR.  
          CREATE BUFFER hBuffer FOR TABLE cSourceTable NO-ERROR.
          IF NOT VALID-HANDLE(hBuffer) THEN DO:
            ocReturn = "Invalid table reference: " + cSourceTable + CHR(10) +
                       PROGRAM-NAME(1).
            UNDO, LEAVE Oppdatering.
          END. 
          IF NOT VALID-HANDLE(hKeyFldListFld) THEN DO:
            ocReturn = "Missing key field reference in event source for table: " + cSourceTable + CHR(10) +
                       PROGRAM-NAME(1).
            UNDO, LEAVE Oppdatering.
          END.
          IF hKeyFldListFld:BUFFER-VALUE NE "" THEN
            cKeyFieldList = hKeyFldListFld:BUFFER-VALUE.

          DO ix = 1 TO NUM-ENTRIES(cKeyFieldList):
            hField[ix] = hBuffer:BUFFER-FIELD(ENTRY(ix,cKeyFieldList)) NO-ERROR.
            IF NOT VALID-HANDLE(hField[ix]) THEN DO:
              ocReturn = "Invalid field reference: " + ENTRY(ix,cKeyFieldList) + CHR(10) +
                         PROGRAM-NAME(1).
              UNDO, LEAVE Oppdatering.
            END.
          END.
        END.
    
        bOk = hBuffer:FIND-BY-ROWID(TO-ROWID(hRowIdent1Fld:BUFFER-VALUE),NO-LOCK) NO-ERROR.  
    
        cFieldValues = "".
        IF bOk THEN DO ix = 1 TO NUM-ENTRIES(cKeyFieldList):
          cFieldValues = cFieldValues + (IF hField[ix]:BUFFER-VALUE = ? THEN "?" ELSE STRING(hField[ix]:BUFFER-VALUE)) + "|".
        END.
        ELSE DO:
          ocReturn = "Record not found for creation of event-log entry for table: " + cSourceTable.
          LEAVE.
        END. 
  
        IF cFieldValues NE "" THEN
          cFieldValues = SUBSTR(cFieldValues,1,LENGTH(cFieldValues) - 1).
    
        RUN jbadmin_cre_eventlog.p (DYNAMIC-FUNCTION("getCompanyId" IN SOURCE-PROCEDURE),
                                    IF VALID-HANDLE(hEventTypeFld) AND hEventTypeFld:BUFFER-VALUE NE "" THEN hEventTypeFld:BUFFER-VALUE ELSE cEventType,
                                    IF VALID-HANDLE(hEventTextFld) AND hEventTextFld:BUFFER-VALUE NE "" THEN hEventTextFld:BUFFER-VALUE ELSE cEventText,
                                    DYNAMIC-FUNCTION("getASuserId" IN SOURCE-PROCEDURE),
                                    cKeyFieldList,
                                    cFieldValues,
                                    cSourceTable,
                                    dEventDate,
                                    iEventTime,
                                    IF VALID-HANDLE(hEventExtraFld) THEN hEventExtraFld:BUFFER-VALUE ELSE cExtra,
                                    INPUT-OUTPUT iEventLogHdrId
                                    ).
    
        hQuery:GET-NEXT().
      END.
  END.
        
obOk = ocReturn = "". 
IF obOK THEN ocReturn = IF iEventLogHdrId NE ? THEN STRING(iEventLogHdrId) ELSE STRING(RETURN-VALUE).

DELETE OBJECT hBuffer NO-ERROR.
DELETE OBJECT hQuery  NO-ERROR.
