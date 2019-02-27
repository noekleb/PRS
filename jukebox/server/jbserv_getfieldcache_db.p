DEF INPUT PARAM icLDBname AS CHAR NO-UNDO.

DEF SHARED TEMP-TABLE tt_field
    FIELD Db-Name          AS CHAR
    FIELD Table-Name       AS CHAR 
    FIELD Field-Name       AS CHAR
    FIELD Field-Type       AS CHAR
    FIELD Field-Label      AS CHAR
    FIELD Field-Format     AS CHAR
    FIELD Field-Initial    AS CHAR
    FIELD Field-Help       AS CHAR
    FIELD Field-Order      AS INT
    FIELD Field-Extent     AS INT
    FIELD Table-Can-Read   AS CHAR
    FIELD Table-Can-Write  AS CHAR
    FIELD Table-Can-Create AS CHAR
    FIELD Table-Can-Delete AS CHAR
    FIELD Field-Can-Read   AS CHAR
    FIELD Field-Can-Write  AS CHAR
    INDEX idxFileField Table-Name Field-Name
    INDEX idxField Field-Name
    .

FOR EACH dictdb._file NO-LOCK
    WHERE dictdb._file._tbl-type = "T"
    ,EACH dictdb._field NO-LOCK 
          OF dictdb._file:
  CREATE tt_field.
  ASSIGN tt_field.Db-name          = icLDBname
         tt_field.Table-Name       = dictdb._file._file-name
         tt_field.Table-Can-Read   = dictdb._file._Can-Read
         tt_field.Table-Can-Write  = dictdb._file._Can-Write
         tt_field.Table-Can-Create = dictdb._file._Can-Create
         tt_field.Table-Can-Delete = dictdb._file._Can-Delete
         tt_field.Field-Name       = dictdb._field._field-name
         tt_field.Field-Type       = dictdb._field._Data-Type
         tt_field.Field-Label      = dictdb._field._Label
         tt_field.Field-Format     = dictdb._field._Format
         tt_field.Field-Initial    = dictdb._field._Initial
         tt_field.Field-Help       = dictdb._field._Help
         tt_field.Field-Order      = dictdb._field._Order
         tt_field.Field-Can-Read   = dictdb._field._Can-Read
         tt_field.Field-Can-Write  = dictdb._field._Can-Write
         tt_field.Field-Extent     = dictdb._field._Extent
         .
END.

 
