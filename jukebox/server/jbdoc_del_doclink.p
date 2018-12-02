/* Delete the structure table for documents */
   
DEF INPUT   PARAM icDocIdList AS CHAR   NO-UNDO.

DEF VAR ix              AS INT    NO-UNDO.
DEF VAR bOk             AS LOG    NO-UNDO.
DEF VAR hBuffDocLink    AS HANDLE NO-UNDO.
DEF VAR hQuery          AS HANDLE NO-UNDO.

CREATE BUFFER hBuffDocLink FOR TABLE "JBoxDocLink" NO-ERROR.

icDocIdList = TRIM(icDocIdList,",").
    
IF VALID-HANDLE(hBuffDocLink) THEN DO TRANSACTION:
  CREATE QUERY hQuery.
  hQuery:SET-BUFFERS(hBuffDocLink).
  DO ix = 1 TO NUM-ENTRIES(icDocIdList):
    hQuery:QUERY-PREPARE("FOR EACH " + hBuffDocLink:NAME + " EXCLUSIVE-LOCK WHERE iFromDocumentId = " + ENTRY(ix,icDocIdList)).
    hQuery:QUERY-OPEN().
    hQuery:GET-FIRST().
    REPEAT WHILE NOT hQuery:QUERY-OFF-END:
      hBuffDocLink:BUFFER-DELETE().
      hQuery:GET-NEXT().
    END.
  END.
  DELETE OBJECT hQuery.
  DELETE OBJECT hBuffDocLink NO-ERROR.
END.


