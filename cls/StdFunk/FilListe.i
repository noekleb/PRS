DEFINE TEMP-TABLE tmpFiler NO-UNDO 
    FIELD FilId AS DECIMAL FORMAT ">>>>>>>>>9"
    FIELD File-Name AS CHARACTER FORMAT "x(30)" 
    FIELD File-Size AS INTEGER 
    FIELD File-Type AS CHARACTER FORMAT "x(5)"
    FIELD Full-Path-Name AS CHARACTER FORMAT "x(40)"
    FIELD PathName AS CHARACTER FORMAT "x(40)"
    FIELD File-Create-Date AS DATE FORMAT "99/99/9999" 
    FIELD File-Create-Time AS INTEGER 
    FIELD File-Mod-Date AS DATE FORMAT "99/99/9999"
    FIELD File-Mod-time AS INTEGER 
    FIELD AntLinjer AS INTEGER 
    FIELD DatoTidOpprettet AS DATETIME
    FIELD DatoTidEndret AS DATETIME
    FIELD Ekstent AS CHARACTER 
    INDEX idxFullPath Full-Path-Name 
    INDEX idxAlder DatoTidOpprettet 
    INDEX idxEndret DatoTidEndret 
    .

