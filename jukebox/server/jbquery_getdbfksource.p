{incl/ttfksource.i}

DEF VAR hQuery   AS HANDLE NO-UNDO.
DEF VAR hBuffer  AS HANDLE NO-UNDO.
DEF VAR hBuffer2 AS HANDLE NO-UNDO.

DEF SHARED TEMP-TABLE ttJBoxQForeignKey LIKE JBoxQForeignKey.  


CREATE BUFFER hBuffer  FOR TABLE "_file".
CREATE BUFFER hBuffer2 FOR TABLE "_field".
CREATE QUERY hQuery.
hQuery:SET-BUFFERS(hBuffer,hBuffer2).
hQuery:QUERY-PREPARE("FOR EACH _file WHERE _tbl-type = 'T' NO-LOCK, EACH _field OF _file NO-LOCK").
hQuery:QUERY-OPEN().
hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  FIND FIRST ttJBoxQForeignKey
       WHERE ttJBoxQForeignKey.cDBtable = STRING(hBuffer:BUFFER-FIELD("_file-name"):BUFFER-VALUE)
         AND ttJBoxQForeignKey.cDBfield = STRING(hBuffer2:BUFFER-FIELD("_field-name"):BUFFER-VALUE)
        NO-ERROR. 
  IF AVAIL ttJBoxQForeignKey THEN DO:
    CREATE ttForeignKey.
    BUFFER-COPY ttJBoxQForeignKey TO ttForeignKey.
  END.
  ELSE DO:
    CREATE ttSource.
    ASSIGN ttSource.cDBtable = hBuffer:BUFFER-FIELD("_file-name"):BUFFER-VALUE
           ttSource.cDBfield = hBuffer2:BUFFER-FIELD("_field-name"):BUFFER-VALUE
           ttSource.cDesc    = hBuffer2:BUFFER-FIELD("_desc"):BUFFER-VALUE
           .
  END.
  hQuery:GET-NEXT().
END.
