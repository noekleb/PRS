DEF INPUT PARAM icClient AS CHAR NO-UNDO.

DEF VAR hBuffer AS HANDLE NO-UNDO.
DEF VAR hQuery  AS HANDLE NO-UNDO.
DEF VAR hField  AS HANDLE NO-UNDO.
DEF VAR ix      AS INT    NO-UNDO.

FOR EACH _file WHERE _tbl-type = "T" NO-LOCK:
  
  OUTPUT TO VALUE("c:\temp\" + icClient + "_" + _file-name + ".dmp").

  CREATE BUFFER hBuffer FOR TABLE _file-name.

  CREATE QUERY hQuery.
  hQuery:SET-BUFFERS(hBuffer).
  hQuery:QUERY-PREPARE("for each " + _file-name).
  hQuery:QUERY-OPEN().
  hQuery:GET-FIRST().
  REPEAT WHILE NOT hQuery:QUERY-OFF-END:
    PUT UNFORMATTED icClient "|".
    DO ix = 1 TO hBuffer:NUM-FIELDS:
      PUT UNFORMATTED REPLACE(hBuffer:BUFFER-FIELD(ix):BUFFER-VALUE,CHR(10),"") "|".
    END.
    PUT CHR(10).
    hQuery:GET-NEXT().
  END.

  DELETE OBJECT hQuery.
  DELETE OBJECT hBuffer.

  OUTPUT CLOSE.
END.

