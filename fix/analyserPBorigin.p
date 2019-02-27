
DEF VAR cDir AS CHAR NO-UNDO.
DEF VAR cFile AS CHAR NO-UNDO.
DEF VAR cString AS CHAR FORMAT "x(40)"NO-UNDO.
DEFINE VARIABLE lDTBRFunnet AS LOGICAL    NO-UNDO.
DEFINE VARIABLE lLDFunnet   AS LOGICAL    NO-UNDO.
DEFINE VARIABLE iTst AS INTEGER    NO-UNDO.
ASSIGN cDir = "F:\Home\Pressbyran\Ankommet\origin\".
DEFINE TEMP-TABLE OriginDir
    FIELD oDir AS CHAR.

DEFINE TEMP-TABLE FilInfo
    FIELD oDir AS CHAR
    FIELD cFile AS CHAR
    FIELD cFnamn AS CHAR
    FIELD FirstRad AS CHAR
    FIELD Filstorlek AS INTE
    FIELD FilTyp AS CHAR
    FIELD cButId AS CHAR.

INPUT FROM OS-DIR(cDir) NO-ECHO.
REPEAT:
    SET cFile.
    IF cFile = "." OR cFile = ".." THEN
        NEXT.
    FILE-INFO:FILE-NAME = cDir + cFile.
    ASSIGN iTst = INT(cFile) NO-ERROR.
    IF NOT ERROR-STATUS:ERROR AND FILE-INFO:FILE-TYPE MATCHES "*D*" THEN DO:
        CREATE OriginDir.
        ASSIGN OriginDir.oDir = FILE-INFO:FULL-PATHNAME + "\".
    END.
END.
INPUT CLOSE.
RELEASE OriginDir.
FOR EACH OriginDir:
    ASSIGN lDTBRFunnet = FALSE
           lLDFunnet   = FALSE.
    INPUT FROM OS-DIR(OriginDir.oDir) NO-ECHO.
    REPEAT:
        SET cFile FORMAT "x(40)".
        FILE-INFO:FILE-NAME = OriginDir.oDir + TRIM(cFile).
        IF NOT FILE-INFO:FILE-TYPE MATCHES "*F*" THEN
            NEXT.
        IF cFile BEGINS "MD" OR cFile BEGINS "dtbr" OR cFile BEGINS "LD" THEN DO:
            IF cFile BEGINS "DTBR" AND lDTBRFunnet = TRUE THEN
                NEXT.
            ELSE IF cFile BEGINS "LD" AND lLDFunnet = TRUE THEN
                NEXT.
            IF cFile BEGINS "DTBR" THEN
                lDTBRFunnet = TRUE.
            IF cFile BEGINS "LD" THEN
                lLDFunnet = TRUE.
            CREATE FilInfo.
            ASSIGN FilInfo.oDir = OriginDir.oDir
                   FilInfo.cFile = OriginDir.oDir + cFile
                   FilInfo.cFnamn = cFile
                   Filinfo.Filstorlek = FILE-INFO:FILE-SIZE
                   FilInfo.FilTyp = SUBSTR(cFile,1,2).
        END.
    END.
    INPUT CLOSE.
END.

FOR EACH FilInfo WHERE FilInfo.cFnamn BEGINS "MD" AND Filinfo.Filstorlek > 0:
    INPUT FROM VALUE(FilInfo.cFile).
    IMPORT UNFORMATTED cString.
    ASSIGN FilInfo.FirstRad = cString
           FilInfo.cButId = IF FilInfo.cFile MATCHES "*.txt" THEN ENTRY(8,cString," ") ELSE ENTRY(8,cString,";").
    INPUT CLOSE.
END.
OUTPUT TO "c:\temp\OriginFiler.txt".
PUT UNFORMATTED "Butdir" CHR(9) "ButIDiFil" CHR(9) "Fil" CHR(9) "Filtyp" CHR(9) "FilStorlek" CHR(9) "kassadir" chr(9)"Datarad" SKIP.
FOR EACH FilInfo BY FilInfo.oDir BY FilInfo.cFile:
    IF Filtyp = "MD" THEN
        FIND FIRST kasse WHERE Kasse.KvitteringKatalog = TRIM(REPLACE(Filinfo.oDir,"origin\",""),"\") NO-LOCK NO-ERROR.
    ELSE
        FIND FIRST kasse WHERE Kasse.ElJournalKatalog = TRIM(REPLACE(Filinfo.oDir,"origin\",""),"\") NO-LOCK NO-ERROR.
    PUT UNFORMATTED ENTRY(6,FilInfo.cFile,"\") chr(9) FilInfo.cButId chr(9) FilInfo.cFile CHR(9) Filinfo.Filtyp chr(9) Filinfo.Filstorlek chr(9) AVAIL kasse chr(9) FilInfo.FirstRad SKIP.
END.
OUTPUT CLOSE.

