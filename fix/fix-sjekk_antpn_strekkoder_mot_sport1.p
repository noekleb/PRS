CURRENT-WINDOW:WIDTH = 300.

DEF STREAM Inn.
DEF STREAM Ut.

DEF VAR X AS INT NO-UNDO.

DEF VAR cFilNavn  AS CHAR NO-UNDO.
DEF VAR cLinje    AS CHAR FORMAT "x(50)" NO-UNDO.
DEF VAR cEAN      AS CHAR FORMAT "x(15)" NO-UNDO.
DEF VAR cResultat AS CHAR NO-UNDO.
DEF VAR cTillegg  AS CHAR NO-UNDO.

ASSIGN
    cFilNavn  = "Q:\Appdir\Sport1HK\AntonStrekkoder05092008\AntonRegister.txt"
    cResultat = "AntonSport1_Strekkodesjekk.txt"
    .

INPUT STREAM Inn FROM VALUE(cFilNavn).
OUTPUT STREAM Ut TO VALUE(cResultat).

/* Overskrift */
PUT STREAM Ut UNFORMATTED
    "Anton.ArtikkelNr;" +
    "Anton.Kode;" +
    "Anton.Bestillingsnummer;" +
    "Anton.LevKod;" +
    "Anton.Beskr;" +
    "Anton.LEvFargKod;" +
    "Anton.StrKode;" +
    "Anton.Storl;" +
    "Anton.RegistrertDato;" +
    "Sport1.ArtikkelNr;" +
    "Sport1.Kode;" +
    "Sport1.Bestillingsnummer;" +
    "Sport1.LevKod;" +
    "Sport1.Beskr;" +
    "Sport1.LEvFargKod;" +
    "Sport1.StrKode;" +
    "Sport1.Storl;" +
    "Sport1.RegistrertDato"
    SKIP.

LOOPEN:
REPEAT:
    IMPORT STREAM Inn UNFORMATTED
        cLinje.

    ASSIGN
        X = X + 1
        cTillegg  = ";;;;;;;;"
        .
    IF X = 1 THEN NEXT LOOPEN.

    ASSIGN
        cEAN = ENTRY(2,cLinje,";")
        .

    FIND STrekkode NO-LOCK WHERE
        Strekkode.Kode = cEAN NO-ERROR.

    IF AVAILABLE Strekkode THEN DO:

        FIND ArtBas OF Strekkode NO-LOCK NO-ERROR.
        FIND StrKonv WHERE
            StrKonv.StrKode = Strekkode.StrKode NO-ERROR.

        ASSIGN
            X = X + 1
            ENTRY(1,cTillegg,";") = STRING(Strekkode.ArtikkelNr)
            ENTRY(2,cTillegg,";") = STRING(Strekkode.Kode)
            ENTRY(3,cTillegg,";") = STRING(Strekkode.Bestillingsnummer)
            ENTRY(7,cTillegg,";") = STRING(Strekkode.StrKode)
            .
       IF AVAILABLE ArtBas THEN
           ASSIGN
             ENTRY(4,cTillegg,";") = STRING(ArtBas.LEvKod)
             ENTRY(5,cTillegg,";") = STRING(ArtBas.Beskr)
             ENTRY(6,cTillegg,";") = STRING(ArtBas.LevFargKod)
             ENTRY(9,cTillegg,";") = STRING(ArtBas.RegistrertDato)
             .
       IF AVAILABLE StrKonv THEN
           ASSIGN
             ENTRY(8,cTillegg,";") = STRING(StrKonv.Storl)
             .
        /*
        DISPLAY
            cEAN
            ENTRY(1,cLinje,";")
            Strekkode.Kode WHEN AVAILABLE Strekkode
            Strekkode.ArtikkelNr WHEN AVAILABLE Strekkode
        WITH WIDTH 300.
        */
    END.

    PUT STREAM Ut UNFORMATTED
        cLinje ";"
        cTillegg
        SKIP.

    /*IF X > 1000 THEN LEAVE LOOPEN.*/
END. /* LOOPEN */
OUTPUT STREAM Ut CLOSE.
INPUT STREAM Inn CLOSE.
