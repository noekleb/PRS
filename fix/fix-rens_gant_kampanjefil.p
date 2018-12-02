/* fix-rens_gant_kampanjefil.p*/

DEF VAR cLinje AS CHAR NO-UNDO.
DEF VAR cInnFil AS CHAR NO-UNDO.
DEF VAR cUtFil AS CHAR NO-UNDO.

DEF STREAM Inn.
DEF STREAM Ut.

CURRENT-WINDOW:WIDTH = 200.

ASSIGN
    cInnFil = 'C:\Polygon\PRS\wrk\PRSKamp.csv'
    cUtFil  = 'C:\Polygon\PRS\wrk\PRSKamp_tn.csv'
    .

INPUT STREAM Inn FROM VALUE(cInnFil) NO-ECHO.
REPEAT:
    IMPORT STREAM Inn UNFORMATTED cLinje.

    DISPLAY
        ASC(SUBSTRING(cLinje,LENGTH(cLinje) - 3,1))
        ASC(SUBSTRING(cLinje,LENGTH(cLinje) - 2,1))
        ASC(SUBSTRING(cLinje,LENGTH(cLinje) - 1,1))
        ASC(SUBSTRING(cLinje,LENGTH(cLinje),1))
        LOOKUP(cLinje,CHR(10))
        cLinje FORMAT "x(60)"
    WITH WIDTH 200.
END.
INPUT STREAM Inn CLOSE.

