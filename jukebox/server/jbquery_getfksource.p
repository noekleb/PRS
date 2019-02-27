DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO. 
DEF INPUT  PARAM icBufferName AS CHAR NO-UNDO.
DEF OUTPUT PARAM TABLE-HANDLE hTempTable.
DEF OUTPUT PARAM ocReturn     AS CHAR NO-UNDO.

DEF VAR hBuffer    AS HANDLE NO-UNDO.
DEF VAR httBuffer  AS HANDLE NO-UNDO.
DEF VAR hQuery     AS HANDLE NO-UNDO.
DEF VAR hField     AS HANDLE NO-UNDO.

DEF VAR ix         AS INT NO-UNDO.

{incl/validatesession.i}

{incl/ttfksource.i NEW}

FOR EACH JBoxQForeignKey NO-LOCK:
  CREATE ttForeignKey.
  BUFFER-COPY JBoxQForeignKey TO ttForeignKey.
END.

CREATE TEMP-TABLE hTempTable.
hTempTable:CREATE-LIKE(icBufferName). 
hTempTable:TEMP-TABLE-PREPARE(icBufferName).

CREATE BUFFER hBuffer FOR TABLE icBufferName.

httBuffer = hTempTable:DEFAULT-BUFFER-HANDLE.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(hBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + hBuffer:NAME).
hQuery:QUERY-OPEN().

hQuery:GET-FIRST().

REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  httBuffer:BUFFER-CREATE.
  httBuffer:BUFFER-COPY(hBuffer).
  hQuery:GET-NEXT().
END.

DELETE OBJECT hBuffer.
DELETE OBJECT hQuery.


