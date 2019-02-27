DEF VAR hQuery  AS HANDLE NO-UNDO. 
DEF VAR hBuffer AS HANDLE NO-UNDO.
DEF VAR hField  AS HANDLE NO-UNDO.
DEF VAR ix      AS INT NO-UNDO.
DEF VAR bReport AS LOG NO-UNDO INIT TRUE.

MESSAGE "Kun rapport" UPDATE bReport.

DEF TEMP-TABLE ttLength
    FIELD cTableName  AS CHAR
    FIELD cFieldName  AS CHAR
    FIELD iWidth      AS INT
    FIELD iMax        AS INT.

FOR EACH _file NO-LOCK
    WHERE _file._Tbl-Type = "T" /* << User tables */
    :

  CREATE BUFFER hBuffer FOR TABLE _file-name.
  CREATE QUERY hQuery.
  hQuery:SET-BUFFERS(hBuffer).
  hQuery:QUERY-PREPARE("FOR EACH " + _file-name + " NO-LOCK").
  hQuery:QUERY-OPEN().
  hQuery:GET-FIRST().

  EMPTY TEMP-TABLE ttLength.

  REPEAT WHILE NOT hQuery:QUERY-OFF-END AND ix < 10:
    DO ix = 1 TO hBuffer:NUM-FIELDS:
      hField = hBuffer:BUFFER-FIELD(ix).
      IF hField:DATA-TYPE = "CHARACTER" THEN DO:

        FIND FIRST _field OF _file
             WHERE _field._Field-Name = hField:NAME 
             NO-LOCK.

        IF LENGTH(hField:BUFFER-VALUE) > _field._Width THEN DO:
          FIND FIRST ttLength
               WHERE ttLength.cFieldName = hField:NAME
               NO-ERROR.
          IF NOT AVAIL ttLength THEN DO:
            CREATE ttLength.
            ASSIGN ttLength.cTableName = _file-name
                   ttLength.cFieldName = hField:NAME
                   ttLength.iWidth     = _width
                   ttLength.iMax       = LENGTH(hField:BUFFER-VALUE)
                   .
          END.
          ELSE IF LENGTH(hField:BUFFER-VALUE) > ttLength.iMax THEN
            ttLength.iMax       = LENGTH(hField:BUFFER-VALUE).
        END.
      END.
    END.
    hQuery:GET-NEXT().
  END.
  IF bReport THEN DO:
    OUTPUT TO VALUE(".\DUMP\sqlwidth.txt") APPEND.
    FOR EACH ttLength:
      DISP ttLength.cTableName FORMAT "x(30)"
           ttLength.cFieldName FORMAT "x(30)"
           ttLength.iWidth
           ttLength.iMax
           WITH WIDTH 100 STREAM-IO.
    END.
    OUTPUT CLOSE.
  END.
  ELSE FOR EACH ttLength:
    FIND FIRST _field OF _file
         WHERE _field._Field-Name = ttLength.cFieldName
         NO-ERROR.
    IF AVAIL _field THEN
      _field._Width = ttLength.iMax + 5.
  END.
END.

