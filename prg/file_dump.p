DEF INPUT PARAMETER cFile-Name  AS CHAR NO-UNDO.
DEF INPUT PARAMETER cFilKatalog AS CHAR NO-UNDO.

DEF VAR cDumpFil   AS CHAR NO-UNDO.
DEF VAR cDump-Name AS CHAR NO-UNDO.
DEF VAR cDesc      AS CHAR NO-UNDO.
DEF VAR bOk        AS LOG  NO-UNDO.
DEF VAR hQuery     AS HANDLE NO-UNDO.
DEF VAR hBuffer    AS HANDLE NO-UNDO.
DEF VAR hBuffer2   AS HANDLE NO-UNDO.
DEF VAR cString    AS CHAR NO-UNDO.
DEF VAR hField     AS HANDLE NO-UNDO.
DEF VAR bh         AS HANDLE NO-UNDO.
DEF VAR ix         AS INT NO-UNDO.

DEF STREAM Ut.

cFilKatalog = TRIM(cFilKatalog).
IF cFilKatalog = '' THEN
    {syspara.i 1 1 60 cFilKatalog}

DO ix = 1 TO NUM-DBS:
  hQuery:QUERY-CLOSE NO-ERROR.
  DELETE OBJECT hQuery NO-ERROR.
  DELETE OBJECT hBuffer NO-ERROR.

  CREATE BUFFER hBuffer  FOR TABLE LDBNAME(ix) + '._file'.
  CREATE QUERY hQuery.
  
  ASSIGN 
    cString = 'FOR EACH ' + LDBNAME(ix) + '._file WHERE _file-name = "' + cFile-name + '"'.
  
  hQuery:SET-BUFFERS(hBuffer) NO-ERROR. 
  hQuery:QUERY-PREPARE(cString) NO-ERROR.
  hQuery:QUERY-OPEN() NO-ERROR.
  hQuery:GET-FIRST(NO-LOCK) NO-ERROR.
  
  hBuffer:FIND-FIRST(cString,NO-LOCK) NO-ERROR.
  
  IF hBuffer:AVAIL THEN
   LEAVE.
END. 

ASSIGN
  cFile-Name  = hQuery:GET-BUFFER-HANDLE(1):BUFFER-FIELD(1):BUFFER-VALUE
  cDump-Name  = hBuffer:BUFFER-FIELD(24):BUFFER-VALUE
  cDump-Name  = IF cDump-Name = ? THEN cFile-Name ELSE cDump-Name 
  cDesc       = hBuffer:BUFFER-FIELD(14):BUFFER-VALUE.

hBuffer:BUFFER-RELEASE().
hQuery:QUERY-CLOSE().

/* Sikrer at katalogen finnes. */
ASSIGN cFilKatalog = RIGHT-TRIM(cFilKatalog,'\').
OS-CREATE-DIR VALUE(cFilKatalog) SILENT NO-WAIT.

cDumpFil = cFilKatalog + '\' + cDump-Name + '.d'.

CREATE BUFFER hBuffer FOR TABLE cFile-Name .

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(hBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + hBuffer:NAME).
hQuery:QUERY-OPEN().

OUTPUT STREAM Ut TO VALUE(cDumpFil) NO-ECHO.

hQuery:GET-FIRST(NO-LOCK).
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  
  DO ix = 1 TO hQuery:GET-BUFFER-HANDLE(1):NUM-FIELDS:
      IF hQuery:GET-BUFFER-HANDLE(1):BUFFER-FIELD(ix):DATA-TYPE = "CHARACTER" THEN
        PUT STREAM ut UNFORMATTED '"' + STRING(hQuery:GET-BUFFER-HANDLE(1):BUFFER-FIELD(ix):BUFFER-VALUE) + '" '.
      ELSE
        PUT STREAM ut UNFORMATTED STRING(hQuery:GET-BUFFER-HANDLE(1):BUFFER-FIELD(ix):BUFFER-VALUE) ' '.
  END.
  PUT STREAM ut SKIP.
  hQuery:GET-NEXT(NO-LOCK).
END.
DELETE OBJECT hQuery.

OUTPUT STREAM Ut CLOSE.
bOk = TRUE.
