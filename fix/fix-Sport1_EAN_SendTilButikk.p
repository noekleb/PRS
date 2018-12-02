CURRENT-WINDOW:WIDTH = 250.

DEF VAR cFil   AS CHAR NO-UNDO.
DEF VAR cLinje AS CHAR FORMAT "x(50)" NO-UNDO.

DEF STREAM Inn.

DEF VAR cEAN AS CHAR FORMAT "x(30)" NO-UNDO.

ASSIGN
    cFil = 'c:\home\lindbak\ankommet\rygge_ean.txt'.

INPUT STREAM Inn FROM VALUE(cFil) NO-ECHO.

REPEAT:
    IMPORT STREAM Inn UNFORMATTED cEAN.
    cEAN = TRIM(cEAN).

    IF LENGTH(cEAN) > 6 THEN
      RUN bibl_chkean.p (INPUT-OUTPUT cEAN).

    IF AVAILABLE ArtBas THEN 
        RELEASE ArtBAs.
    FIND Strekkode NO-LOCK WHERE
        STrekkode.Kode = cEAN NO-ERROR.
    IF AVAILABLE STrekkode THEN
        FIND ArtBas OF Strekkode NO-LOCK.

    IF AVAILABLE ArtBas THEN
    DO:
        /* Overfører ArtBas til VPI register. */
        RUN artbas_til_vpi.p (1,ArtBas.ArtikkelNr).
        RUN create_elogg.p ('VPIArtBas','POS-123','1' + CHR(1) + STRING(ArtBas.ArtikkelNr)).
    END.
END.

DEF VAR iAntSlett AS INT NO-UNDO.
DEF VAR iAntNyEndre AS INT NO-UNDO.
RUN vpieksport.w ('POS-123',
                      '325', /* Ekstent skal være butikk som har sendt filen. */
                      0, /* Bildedata skal sendes med */
                      OUTPUT iAntSlett,
                      OUTPUT iAntNyEndre).

INPUT STREAM Inn CLOSE.
