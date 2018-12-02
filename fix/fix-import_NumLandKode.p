/* fix-import_NumLandKode.p */

DEF VAR cLinje AS CHAR NO-UNDO.
DEF VAR cFil   AS CHAR NO-UNDO.
DEF VAR iInt   AS INT  NO-UNDO.
DEF VAR cLand  AS CHAR NO-UNDO.

ASSIGN
    cFil = 'C:\Polygon\PRS Dokumentasjon\Grunndata\InternasjonaleLandKoder-ISO3361-1\LandKoder_Numeriske-ISO3166.csv'.

DEF STREAM Inn.

INPUT STREAM Inn FROM VALUE(cFil) NO-ECHO.
REPEAT:
    IMPORT STREAM inn UNFORMATTED
        cLinje.
    cLinje = TRIM(cLinje).
    IF cLinje = '' THEN
        NEXT.
    IF NUM-ENTRIES(cLinje,';') < 2 THEN
        NEXT.

    ASSIGN
      iInt  = INT(ENTRY(1,cLinje,';'))
      cLand = ENTRY(2,cLinje,';').

    FIND NumLandKode NO-LOCK WHERE
        NumLandKode.NumLandKode = iInt NO-ERROR.
    IF NOT AVAILABLE NumLandKode THEN
    DO:
        CREATE NumLandKode.
        ASSIGN
            NumLandKode.NumLandKode = iInt.
            NumLandKode.Land        = cLand.
        RELEASE NumLandKode.
    END.
END.
INPUT STREAM Inn CLOSE.
