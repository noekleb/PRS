DEF INPUT PARAM icLDBname   AS CHAR NO-UNDO.
DEF INPUT PARAM icTableName AS CHAR NO-UNDO.

DEF SHARED VAR cFields AS CHAR NO-UNDO.
DEF VAR ix AS INT NO-UNDO.

FOR EACH dictdb._file NO-LOCK
    WHERE dictdb._file._tbl-type = "T"
      AND dictdb._file._file-name = icTableName
    ,EACH dictdb._field NO-LOCK 
          OF dictdb._file:
  IF dictdb._field._Extent = 0 THEN            
    cFields = cFields + (IF cFields NE "" THEN ";" ELSE "") + dictdb._field._field-name.
  ELSE DO ix = 1 TO dictdb._field._Extent:
    cFields = cFields + (IF cFields NE "" THEN ";" ELSE "") + dictdb._field._field-name + "[" + STRING(ix) + "]".
  END.   
END.

 
