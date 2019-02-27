/* Temp-table def's for schema tables */

DEF {1} SHARED TEMP-TABLE tt_file
    FIELD cFileName  AS CHAR
    FIELD cFileDesc  AS CHAR
    FIELD cLDBname   AS CHAR
    FIELD iFileNum   AS INT
    INDEX FileNum IS UNIQUE iFileNum
    INDEX idxFileName cFileName
    .

DEF {1} SHARED TEMP-TABLE tt_field
    FIELD cFileName  AS CHAR
    FIELD cFieldName AS CHAR
    FIELD cLabel     AS CHAR
    FIELD cDesc      AS CHAR
    FIELD cDataType  AS CHAR
    FIELD cFormat    AS CHAR
    FIELD iFileNum   AS INT
    FIELD iFieldNum  AS INT
    FIELD iIndexSeq  AS INT 
    INDEX FileNum iFileNum
    INDEX idxFileName cFileName
    INDEX FieldNum IS UNIQUE iFieldNum.
