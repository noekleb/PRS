DEF INPUT PARAM icLDBname AS CHAR NO-UNDO.
DEF INPUT-OUTPUT PARAM ixFileNum  AS INT NO-UNDO.
DEF INPUT-OUTPUT PARAM ixIdxNum   AS INT NO-UNDO.
DEF INPUT-OUTPUT PARAM ixFieldNum AS INT NO-UNDO.

{incl/ttdatadict.i}

FOR EACH dictdb._file 
    WHERE dictdb._file._tbl-type = 'T'
    NO-LOCK:

  IF NOT CAN-FIND (FIRST tt_file WHERE 
                   tt_file.cFileName = _file._File-name 
                   ) THEN DO: 
    CREATE tt_file.
    ASSIGN tt_file.cFileName = _file._File-Name
           tt_file.cFileDesc = _file._Desc
           tt_file.cLDBname  = icLDBname
           ixFileNum         = ixFileNum + 1
           tt_file.iFileNum  = ixFileNum.
    
    FOR EACH dictdb._field OF dictdb._file
            NO-LOCK:
      CREATE tt_field.
      ASSIGN tt_field.cFileName  = _file._File-Name
             tt_field.cFieldName = _field._Field-Name
             tt_field.cLabel     = _field._Label
             tt_field.cDesc      = _field._Desc
             tt_field.cDataType  = _field._Data-Type
             tt_field.cFormat    = _field._Format
             tt_field.iFileNum   = ixFileNum
             ixFieldNum          = ixFieldNum + 1
             tt_field.iFieldNum  = ixFieldNum
             tt_field.iIndexSeq  = 0
             .
      SjekkIdx:
      FOR EACH _index-field OF dictdb._field
          NO-LOCK
          BY _index-field._index-seq:
        tt_field.iIndexSeq  = _index-field._index-seq.
        LEAVE SjekkIdx.
      END.
    END.
  END.
END.

 
