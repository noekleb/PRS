DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO. 
DEF INPUT  PARAM icParam      AS CHAR NO-UNDO.
DEF OUTPUT PARAM TABLE-HANDLE hTempTable.
DEF OUTPUT PARAM ocReturn     AS CHAR NO-UNDO.

DEF VAR cDBlist     AS CHAR   NO-UNDO.
DEF VAR cBufferName AS CHAR   NO-UNDO.
DEF VAR cQueryWhere AS CHAR   NO-UNDO.
DEF VAR hBuffer     AS HANDLE NO-UNDO.
DEF VAR httBuffer   AS HANDLE NO-UNDO.
DEF VAR hQuery      AS HANDLE NO-UNDO.
DEF VAR hField      AS HANDLE NO-UNDO.

DEF VAR ixFileNum   AS INT NO-UNDO.
DEF VAR ixIdxNum    AS INT NO-UNDO.
DEF VAR ixFieldNum  AS INT NO-UNDO.
DEF VAR ix          AS INT NO-UNDO.

{incl/validatesession.i}

{incl/ttdatadict.i NEW}

ASSIGN cBufferName = ENTRY(1,icParam,";")
       cDBlist     = ENTRY(2,icParam,";")
       cQueryWhere = ENTRY(3,icParam,";").        

REPEAT ix = 1 TO NUM-DBS:
  IF NOT CAN-DO(cDBlist,LDBNAME(ix)) THEN NEXT.

  DELETE ALIAS dictdb.
  CREATE ALIAS dictdb FOR DATABASE VALUE (LDBNAME(ix)).
  RUN jbquery_getdbdatadict.p (LDBNAME(ix),
                       INPUT-OUTPUT ixFileNum,
                       INPUT-OUTPUT ixIdxNum,
                       INPUT-OUTPUT ixFieldNum).
END.
DELETE ALIAS dictdb.
CREATE ALIAS dictdb FOR DATABASE VALUE (LDBNAME(1)).

CREATE TEMP-TABLE hTempTable.
hTempTable:CREATE-LIKE(IF cBufferName = "_file" THEN "tt_file" ELSE "tt_field"). 
hTempTable:TEMP-TABLE-PREPARE("tt" + cBufferName).

CREATE BUFFER hBuffer FOR TABLE IF cBufferName = "_file" THEN "tt_file" ELSE "tt_field".

httBuffer = hTempTable:DEFAULT-BUFFER-HANDLE.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(hBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + hBuffer:NAME + " " + cQueryWhere).
hQuery:QUERY-OPEN().

hQuery:GET-FIRST().

REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  httBuffer:BUFFER-CREATE.
  httBuffer:BUFFER-COPY(hBuffer).
  hQuery:GET-NEXT().
END.

DELETE OBJECT hBuffer.
DELETE OBJECT hQuery.
DELETE OBJECT hTempTable.

