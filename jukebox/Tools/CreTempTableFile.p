DEF INPUT PARAM icLDBname AS CHAR NO-UNDO.
DEF INPUT-OUTPUT PARAM ixFileNum  AS INT NO-UNDO.
DEF INPUT-OUTPUT PARAM ixIdxNum   AS INT NO-UNDO.
DEF INPUT-OUTPUT PARAM ixFieldNum AS INT NO-UNDO.

DEF SHARED TEMP-TABLE tt_file LIKE _file
    FIELD cLDBname AS CHAR
    FIELD iFileNum AS INT
    INDEX FileNum IS UNIQUE iFileNum.
DEF SHARED TEMP-TABLE tt_field LIKE _field
    FIELD iFileNum  AS INT
    FIELD iFieldNum AS INT
    INDEX FileNum iFileNum
    INDEX FieldNum IS UNIQUE iFieldNum.
DEF SHARED TEMP-TABLE tt_index LIKE _index
    FIELD iFileNum AS INT
    FIELD iIdxNum  AS INT
    FIELD cCode    AS CHAR
    INDEX FileNum iFileNum
    INDEX IdxNum IS UNIQUE iIdxNum.
DEF SHARED TEMP-TABLE tt_index-field LIKE _index-field
    FIELD iFileNum   AS INT
    FIELD iIdxNum    AS INT
    FIELD iFieldNum  AS INT
    FIELD cFieldName AS CHAR
    INDEX FileNum iFileNum
    INDEX FieldNum iFieldNum
    INDEX IdxNum iIdxNum.

FOR EACH dictdb._file NO-LOCK:
  IF NOT CAN-FIND (FIRST tt_file WHERE 
                   tt_file._Db-recid  = _file._Db-recid AND
                   tt_file._File-name = _file._File-name AND
                   tt_file._Owner     = _file._Owner) THEN DO: 
    CREATE tt_file.
    BUFFER-COPY dictdb._file TO tt_file.
    ASSIGN tt_file.cLDBname = icLDBname
           ixFileNum        = ixFileNum + 1
           tt_file.iFileNum = ixFileNum.
    
    FOR EACH dictdb._field OF dictdb._file NO-LOCK:
      CREATE tt_field.
      BUFFER-COPY dictdb._field TO tt_field. 
      ASSIGN tt_field.iFileNum  = ixFileNum
             ixFieldNum         = ixFieldNum + 1
             tt_field.iFieldNum = ixFieldNum.
    END.
    FOR EACH dictdb._index OF dictdb._file NO-LOCK
        BY dictdb._index._index-name:
        
      CREATE tt_index.
      BUFFER-COPY dictdb._index TO tt_index.
      ASSIGN tt_index.iFileNum = ixFileNum
             ixIdxNum          = ixIdxNum + 1
             tt_index.iIdxNum  = ixIdxNum
             tt_index.cCode    = (IF RECID(dictdb._index) = dictdb._file._prime-index THEN "P" 
                                  ELSE " ")
                               + (IF dictdb._index._UNIQUE THEN "U" ELSE "") 
             .
      
      FOR EACH dictdb._index-field OF dictdb._index NO-LOCK,
          EACH dictdb._field OF dictdb._index-field NO-LOCK:
        CREATE tt_index-field.
        BUFFER-COPY dictdb._index-field TO tt_index-field.
        ASSIGN tt_index-field.iFileNum   = ixFileNum
               tt_index-field.iIdxNum    = ixIdxNum
               tt_index-field.cFieldName = dictdb._field._field-name.
        FOR FIRST tt_field WHERE tt_field.iFileNum = ixFileNum AND
            tt_field._field-name = dictdb._field._field-name NO-LOCK:
          tt_index-field.iFieldNum = tt_field.iFieldNum.
        END.
      END.
    END.
  END.
END.

 
