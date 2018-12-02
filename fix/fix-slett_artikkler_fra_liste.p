CURRENT-WINDOW:WIDTH = 250.
DEF VAR lArtikkelNr AS DEC FORMAT ">>>>>>>>>>>>>9" NO-UNDO.

DEF VAR cFilNavn AS CHAR NO-UNDO.
DEF VAR cLinje AS CHAR FORMAT "x(50)" NO-UNDO.

DEF STREAM Inn.

ASSIGN
    cFilNavn = 'c:\home\lindbak\ankommet\ArtiklerSlettes120301.csv'
    .

INPUT STREAM Inn FROM VALUE(cFilNavn) NO-ECHO.

REPEAT:
    IMPORT STREAM Inn UNFORMATTED cLinje.
    ASSIGN
        lArtikkelNr = DEC(ENTRY(2,cLinje,';')) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        NEXT.
    ELSE DO:

    /*
    IF CAN-FIND(ArtBAs WHERE ArtBas.ArtikkelNr = lArtikkelNr) THEN
        RUN slettartbasbatch.w (lArtikkelNr).
    */
    DISPLAY
        cLinje
        lArtikkelNr
        WITH WIDTH 250.

    END.
END.

INPUT STREAM Inn CLOSE.

