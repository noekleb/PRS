DEF VAR hBuffer AS HANDLE NO-UNDO.
DEF VAR hQuery  AS HANDLE NO-UNDO.
FOR EACH _file WHERE _tbl-type = "T" NO-LOCK:
    DISP _file-name.
    CREATE BUFFER hBuffer FOR TABLE _file-name.
    CREATE QUERY hQuery.
    hQuery:SET-BUFFERS(hBuffer).
    hQuery:QUERY-PREPARE("for each " + _file-name).
    hQuery:QUERY-OPEN().
    hQuery:GET-FIRST().
    REPEAT TRANSACTION WHILE NOT hQuery:QUERY-OFF-END:
        hBuffer:FIND-CURRENT(EXCLUSIVE-LOCK).
        hBuffer:BUFFER-DELETE().
        hQuery:GET-NEXT().
    END.
END.
