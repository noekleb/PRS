/* Change characters in all text fields, except primary keys */

CURRENT-WINDOW:WIDTH-CHARS = 180.
                      
DEF VAR cPKlist       AS CHAR   NO-UNDO.
DEF VAR cCharFields   AS CHAR   NO-UNDO.
DEF VAR ix            AS INT    NO-UNDO.
DEF VAR hField        AS HANDLE NO-UNDO.

RUN getPkList (OUTPUT cPkList).
MESSAGE cPkList
  VIEW-AS ALERT-BOX INFO BUTTONS OK.

DEF VAR hBuffer AS HANDLE NO-UNDO.
DEF VAR hQuery  AS HANDLE NO-UNDO.
FOR EACH _file NO-LOCK 
    WHERE _tbl-type = "T" 
    AND _file-name = "PART_MASTER"
    :
    cCharFields = "".
    CREATE BUFFER hBuffer FOR TABLE _file-name.

    DO ix = 1 TO hBuffer:NUM-FIELDS:
      hField = hBuffer:BUFFER-FIELD(ix).
      IF hField:DATA-TYPE = "CHARACTER" 
         AND NOT CAN-DO(cPkList,hField:NAME) 
         AND INT(SUBSTR(hField:FORMAT,3,R-INDEX(hField:FORMAT,")") - 3)) > 12
         THEN 
        cCharFields = cCharFields + (IF cCharFields NE "" THEN "," ELSE "") + hField:NAME.
    END.
    DISP _file-name 
         cCharFields FORMAT "x(120)"
         WITH FRAME f1 DOWN WIDTH 180.
    DOWN WITH FRAME f1.

    IF cCharFields NE "" THEN DO:
      CREATE QUERY hQuery.
      hQuery:SET-BUFFERS(hBuffer).
      hQuery:QUERY-PREPARE("for each " + _file-name).
      hQuery:QUERY-OPEN().
      hQuery:GET-FIRST().
      REPEAT TRANSACTION WHILE NOT hQuery:QUERY-OFF-END:
        hBuffer:FIND-CURRENT(EXCLUSIVE-LOCK).
        DO ix = 1 TO NUM-ENTRIES(cCharFields):
          ASSIGN hField = hBuffer:BUFFER-FIELD(ENTRY(ix,cCharFields))
            hField:BUFFER-VALUE = REPLACE(hField:BUFFER-VALUE,CHR(15),"Å")
            hField:BUFFER-VALUE = REPLACE(hField:BUFFER-VALUE,CHR(27),"ø")
            hField:BUFFER-VALUE = REPLACE(hField:BUFFER-VALUE,CHR(29),"Ø")
            hField:BUFFER-VALUE = REPLACE(hField:BUFFER-VALUE,CHR(127),"Å")
            hField:BUFFER-VALUE = REPLACE(hField:BUFFER-VALUE,CHR(143),"å")
            hField:BUFFER-VALUE = REPLACE(hField:BUFFER-VALUE,CHR(145),"æ")
            hField:BUFFER-VALUE = REPLACE(hField:BUFFER-VALUE,CHR(146),"Æ")
            hField:BUFFER-VALUE = REPLACE(hField:BUFFER-VALUE,CHR(155),"ø")
            hField:BUFFER-VALUE = REPLACE(hField:BUFFER-VALUE,CHR(157),"ø")
            hField:BUFFER-VALUE = REPLACE(hField:BUFFER-VALUE,CHR(167),"ø")
            hField:BUFFER-VALUE = REPLACE(hField:BUFFER-VALUE,CHR(168),"ø")
            hField:BUFFER-VALUE = REPLACE(hField:BUFFER-VALUE,CHR(176),"ø")
            hField:BUFFER-VALUE = REPLACE(hField:BUFFER-VALUE,CHR(176),"ø")
            hField:BUFFER-VALUE = REPLACE(hField:BUFFER-VALUE,CHR(181),"æ")
            hField:BUFFER-VALUE = REPLACE(hField:BUFFER-VALUE,CHR(207),"Ø")
            hField:BUFFER-VALUE = REPLACE(hField:BUFFER-VALUE,CHR(210),"å")
            hField:BUFFER-VALUE = REPLACE(hField:BUFFER-VALUE,CHR(211),"ø")
            hField:BUFFER-VALUE = REPLACE(hField:BUFFER-VALUE,CHR(213),"å")
            hField:BUFFER-VALUE = REPLACE(hField:BUFFER-VALUE,CHR(218),"é")
            hField:BUFFER-VALUE = REPLACE(hField:BUFFER-VALUE,CHR(227),"Æ")
            hField:BUFFER-VALUE = REPLACE(hField:BUFFER-VALUE,CHR(236),"²")
            hField:BUFFER-VALUE = REPLACE(hField:BUFFER-VALUE,CHR(253),"²")
            .
        END.
        hQuery:GET-NEXT().
      END.
    END.
END.

PROCEDURE getPKlist:
  DEF OUTPUT PARAM ocPkList AS CHAR NO-UNDO.

  FOR EACH _file NO-LOCK
      WHERE _tbl-type = "T"
     ,EACH _index OF _file NO-LOCK
           WHERE _index._unique
     ,EACH _index-field OF _index NO-LOCK
     ,FIRST _field OF _index-field NO-LOCK
            WHERE _field._data-type = "CHARACTER"
      :
    IF NOT CAN-DO(ocPkList,_field._field-name) THEN
      ocPkList = ocPkList + (IF ocPkList NE "" THEN "," ELSE "") + _field._field-name.
  END.

END PROCEDURE.
