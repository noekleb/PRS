CURRENT-WINDOW:WIDTH = 200.

DEF VAR cFilNavn AS CHAR NO-UNDO.

DEF STREAM Ut.

ASSIGN
    cFilNavn = "Strekkode_000.csv"
    .

OUTPUT STREAM Ut TO VALUE(cFilNavn) NO-ECHO.

PUT STREAM Ut UNFORMATTED
    "Strekkode;ArtikkelNr;LEv.art.nr;Varetekst;Lev.fargekode;Bestillingsnr;SE nr."
    SKIP.

FOR EACH Strekkode NO-LOCK WHERE
    substring(Strekkode.Kode,10,3) = "000":
    FIND ArtBas OF Strekkode NO-LOCK NO-ERROR.
    /* ----------------------
    DISPLAY
        Strekkode.Kode
        Strekkode.ArtikkelNr
        ArtBAs.LevKod WHEN AVAILABLE ArtBas
        ArtBAs.Beskr  WHEN AVAILABLE ArtBas
        ArtBAs.LevFargKod WHEN AVAILABLE ArtBas
        Strekkode.Bestillingsnummer
        STRING(Strekkode.ArtikkelNr) + "000" FORMAT "x(13)" COLUMN-LABEL "SE nr."
        WITH WIDTH 248
        .
    ---------------- */    
    PUT STREAM Ut UNFORMATTED
        Strekkode.Kode ";"
        Strekkode.ArtikkelNr ";"
        IF AVAILABLE ArtBas THEN ArtBAs.LevKod ELSE "** Ukjent artikkel" ";"
        IF AVAILABLE ArtBas THEN ArtBAs.Beskr  ELSE "" ";"
        IF AVAILABLE ArtBas THEN ArtBAs.LevFargKod ELSE "" ";"
        Strekkode.Bestillingsnummer ";"
        STRING(Strekkode.ArtikkelNr) + "000" FORMAT "x(13)" ";"
        SKIP.
END.

OUTPUT STREAM Ut CLOSE.
