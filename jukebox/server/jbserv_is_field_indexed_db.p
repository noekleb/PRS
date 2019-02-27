/* Deterimine if a database field is indexed
   Created: 18.03.09 by brynjar@chemistry.no
----------------------------------------------------------------------------*/
DEF INPUT  PARAM icLDBname   AS CHAR NO-UNDO.
DEF INPUT  PARAM icFileName  AS CHAR NO-UNDO.
DEF INPUT  PARAM icFieldName AS CHAR NO-UNDO.
DEF OUTPUT PARAM obIndexed   AS LOG  NO-UNDO.


FOR EACH dictdb._file NO-LOCK
    WHERE dictdb._file._tbl-type = "T"
      AND dictdb._file._File-Name = icFileName
    ,EACH dictdb._field NO-LOCK 
          OF dictdb._file
          WHERE dictdb._field._Field-Name = icFieldName
    ,EACH dictdb._index-field NO-LOCK
          OF dictdb._field
          WHERE dictdb._index-field._Index-Seq = 1
    :
  obIndexed = YES.
END.

 
