/* Create a structure table for documents */
   
DEF INPUT   PARAM icDocIdList AS CHAR   NO-UNDO.

DEF VAR ix              AS INT    NO-UNDO.
DEF VAR cUserid         AS CHAR   NO-UNDO.
DEF VAR bOk             AS LOG    NO-UNDO.
DEF VAR hBuffDocLink    AS HANDLE NO-UNDO.
DEF VAR iMainDoc        AS INT    NO-UNDO.

DEF TEMP-TABLE ttSort
    FIELD iDocId AS INT.

cUserid = DYNAMIC-FUNCTION("getAsUserId" IN SOURCE-PROCEDURE) NO-ERROR.

CREATE BUFFER hBuffDocLink FOR TABLE "JBoxDocLink" NO-ERROR.

icDocIdList = TRIM(icDocIdList,",").
    
IF VALID-HANDLE(hBuffDocLink) THEN DO:
  DO ix = 1 TO NUM-ENTRIES(icDocIdList):
    CREATE ttSort.
    ttSort.iDocId = INTEGER(ENTRY(ix,icDocIdList)).
  END.

  FOR EACH ttSort BY ttSort.iDocId DESC:
    IF iMainDoc = 0 THEN iMainDoc = ttSort.iDocId.

    bOk = hBuffDocLink:FIND-FIRST("WHERE iToDocumentId = " + STRING(iMainDoc)
                                + "  AND iFromDocumentId = " + STRING(ttSort.iDocId)
                                  ,NO-LOCK) NO-ERROR.
    IF NOT bOk THEN DO TRANSACTION:
      hBuffDocLink:BUFFER-CREATE().
      ASSIGN hBuffDocLink:BUFFER-FIELD("iToDocumentId"):BUFFER-VALUE   = iMainDoc
             hBuffDocLink:BUFFER-FIELD("iFromDocumentId"):BUFFER-VALUE = ttSort.iDocId
             hBuffDocLink:BUFFER-FIELD("cCreatedBy"):BUFFER-VALUE      = cUserid
             hBuffDocLink:BUFFER-FIELD("dCreated"):BUFFER-VALUE        = TODAY
             .
    END.
  END.
  
  DELETE OBJECT hBuffDocLink NO-ERROR.
END.


