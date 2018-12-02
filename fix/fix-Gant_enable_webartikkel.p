

CURRENT-WINDOW:WIDTH = 300.

DEF VAR cFilNavn AS CHAR NO-UNDO.
DEF VAR cLinje AS CHAR NO-UNDO.
DEF VAR cEAN   AS CHAR NO-UNDO.
DEF VAR iAnt AS INT NO-UNDO.

ASSIGN
    cFilNavn = "C:\ArkivDokument\Kunder\GANT\Nettbutikk\export_no_simple_enabled.csv".

DEF STREAM Inn.

FOR EACH ArtBAs EXCLUSIVE-LOCK WHERE 
    ArtBas.WebButikkArtikkel = TRUE:
    ASSIGN
        ArtBas.PubliserINettbutikk = FALSE
        ArtBas.WebButikkArtikkel   = FALSE
        .
END.

INPUT STREAM Inn FROM VALUE(cFilNAvn) NO-ECHO.

REPEAT:
    IF AVAILABLE ArtBas THEN RELEASE ArtBAs.
    IF AVAILABLE Lager THEN RELEASE Lager.

    IMPORT STREAM Inn UNFORMATTED cLinje.
    iAnt = iAnt + 1.
    IF iAnt <= 1 THEN
        NEXT.

    ASSIGN
        iAnt = iAnt + 1
        cEAN = TRIM(ENTRY(2,cLinje,';'),'"')
    .

    RUN bibl_chkean.p (INPUT-OUTPUT cEAN).
    FIND Strekkode NO-LOCK WHERE
        Strekkode.Kode = cEAN NO-ERROR.
    IF AVAILABLE Strekkode THEN
        FIND ArtBas OF Strekkode EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE ArtBAs THEN
    DO:
        ASSIGN
            ArtBas.PubliserINettbutikk = TRUE
            ArtBas.WebButikkArtikkel   = TRUE
            .
        FOR EACH Lager EXCLUSIVE-LOCK WHERE
            Lager.ArtikkelNr = ArtBAs.ArtikkelNr:
            ASSIGN
                Lager.ETid = TIME.
        END.
    END.
        
    PAUSE 0 BEFORE-HIDE.
    DISPLAY
        cEAN FORMAT "x(20)"
        AVAILABLE Strekkode
        AVAILABLE ArtBas
        AVAILABLE Lager
    WITH WIDTH 300.
END.
INPUT STREAM Inn CLOSE.
