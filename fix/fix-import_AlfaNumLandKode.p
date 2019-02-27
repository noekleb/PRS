/* fix-import_NumLandKode.p */

DEF VAR cLinje AS CHAR NO-UNDO.
DEF VAR cFil   AS CHAR NO-UNDO.
DEF VAR iInt   AS INT  NO-UNDO.
DEF VAR cChar2 AS CHAR NO-UNDO.
DEF VAR cChar3 AS CHAR NO-UNDO.

ASSIGN
    cFil = 'C:\Polygon\PRS Dokumentasjon\Grunndata\InternasjonaleLandKoder-ISO3361-1\LandKoder_Alfanumerisk-ISO3166.csv'.

DEF STREAM Inn.

INPUT STREAM Inn FROM VALUE(cFil) NO-ECHO.
REPEAT:
    IMPORT STREAM inn UNFORMATTED
        cLinje.
    cLinje = TRIM(cLinje).
    IF cLinje = '' THEN
        NEXT.
    IF NUM-ENTRIES(cLinje,';') < 3 THEN
        NEXT.

    ASSIGN
        cChar2 = TRIM(ENTRY(1,cLinje,';'))
        cChar3 = TRIM(ENTRY(2,cLinje,';'))
        iInt   = INT(ENTRY(3,cLinje,';')).

    FIND AlfaLandKode NO-LOCK WHERE
        AlfaLandKode.AlfaKode3 = cChar3 NO-ERROR.
    IF NOT AVAILABLE AlfaLandKode THEN
    DO:
        CREATE AlfaLandKode.
        ASSIGN
            AlfaLandKode.NumLandKode = iInt
            AlfaLandKode.AlfaKode2   = cChar2
            AlfaLandKode.AlfaKode3   = cChar3
            .
        RELEASE AlfaLandKode.
    END.
END.
INPUT STREAM Inn CLOSE.
