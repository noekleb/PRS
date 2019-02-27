/* Delete the revision table for documents */
   
DEF INPUT   PARAM icDocIdList AS CHAR   NO-UNDO.

DEF VAR ix              AS INT    NO-UNDO.
DEF VAR bOk             AS LOG    NO-UNDO.
DEF VAR hBuffDocRev     AS HANDLE NO-UNDO.
DEF VAR hQuery          AS HANDLE NO-UNDO.

CREATE BUFFER hBuffDocRev FOR TABLE "JBoxDocRev" NO-ERROR.

icDocIdList = TRIM(icDocIdList,",").
    
IF VALID-HANDLE(hBuffDocRev) THEN DO TRANSACTION:
  CREATE QUERY hQuery.
  hQuery:SET-BUFFERS(hBuffDocRev).
  DO ix = 1 TO NUM-ENTRIES(icDocIdList):
    hQuery:QUERY-PREPARE("FOR EACH " + hBuffDocRev:NAME + " EXCLUSIVE-LOCK WHERE iJBoxDocumentId = " + ENTRY(ix,icDocIdList)).
    hQuery:QUERY-OPEN().
    hQuery:GET-FIRST().
    REPEAT WHILE NOT hQuery:QUERY-OFF-END:
      hBuffDocRev:BUFFER-DELETE().
      hQuery:GET-NEXT().
    END.
  END.
  DELETE OBJECT hQuery.
  DELETE OBJECT hBuffDocRev NO-ERROR.
END.


