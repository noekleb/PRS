CURRENT-WINDOW:WIDTH = 200.

DEF VAR cFilNavn AS CHAR NO-UNDO.
DEF VAR cEAN AS CHAR FORMAT "x(15)" NO-UNDO.
DEF VAR cTekst AS CHAR FORMAT "x(60)" NO-UNDO.

DEF STREAM Inn.

ASSIGN
      cFilNavn = 'c:\HOME\Bildereferanser_Bergans2.csv'.

INPUT STREAM Inn FROM VALUE(cFilNAvn) NO-ECHO.

REPEAT:
    IMPORT STREAM Inn DELIMITER ';'
        cEAN
        cTekst
        .

    IF NUM-ENTRIES(cTekst,'.') < 2 THEN 
        cTekst = cTekst + '.jpg'.
    cEan = TRIM(cEAN).

    FIND Strekkode NO-LOCK WHERE
        Strekkode.Kode = cEAN NO-ERROR.
    IF AVAILABLE STrekkode THEN
        FIND ArtBAs OF Strekkode EXCLUSIVE-LOCK NO-ERROR.
    
    IF NUM-ENTRIES(cTekst,'.') = 2 THEN
        ArtBas.VPIBildeKode = cTekst.
    DISPLAY 
        cEan cTekst 
        ArtBas.ArtikkelNr WHEN AVAILABLE ArtBAs
        ArtBas.VPIBildekode WHEN AVAILABLE ArtBAs FORMAT "x(60)"
    WITH WIDTH 200.
END.

INPUT STREAM Inn CLOSE.
