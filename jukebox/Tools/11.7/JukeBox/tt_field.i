DEF {1} SHARED TEMP-TABLE tt_field LIKE dictdb._Field
    FIELD cTableName AS CHAR 
    FIELD cDbName    AS CHAR
    INDEX idxDbTable cDbName cTableName
    INDEX idxTable cTableName
    .
DEF BUFFER btt_field FOR tt_field.

DEF VAR htt_field AS HANDLE NO-UNDO.
htt_field = BUFFER tt_field:HANDLE.
