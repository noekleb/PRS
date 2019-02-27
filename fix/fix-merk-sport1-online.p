/* Sett Sport 1 Online merke
 Artikler som er merket i VG for aktivering i Sport 1 Online, 
 er ikke merket inne i SE. Fil med aktuelle artikler er eksportert 
 fra VG og benyttes som underlag for å merke opp artiklene i SE.
*/

DEF VAR cFilVG       AS CHAR FORMAT "x(40)"  NO-UNDO.
DEF VAR cFilPrisDiff AS CHAR FORMAT "x(40)"  NO-UNDO.
DEF VAR cRecord      AS CHAR FORMAT "x(200)" NO-UNDO.

DEF VAR dArtikkelNr AS DEC  NO-UNDO.
DEF VAR cArtikkelNr AS CHAR FORMAT "x(15)" NO-UNDO.
DEF VAR dVarekost   AS DEC  FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEF VAR dDiff       AS DEC  FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEF VAR cBruk       AS CHAR NO-UNDO.
DEF VAR iAnv-Id     AS INT  NO-UNDO.

ASSIGN
    cFilVG = "C:\ArkivDokument\LRS\Kunder & Prospects\Norge\Kunder\Sport 1 Gruppen\Messe Håkonshall aug 06\sykkelkatalog.csv"
    .

DEF STREAM Inn.
DEF STREAM Ut.

DEF BUFFER bAnv-Kod FOR Anv-Kod.

CURRENT-WINDOW:WIDTH = 220.

INPUT STREAM inn FROM VALUE (cFilVg) NO-ECHO.

REPEAT:
    IMPORT STREAM Inn UNFORMATTED cRecord.

    ASSIGN
        cArtikkelNr = TRIM(ENTRY(2,cRecord,";"))
        cArtikkelNr = SUBSTRING(cArtikkelNr,1,LENGTH(cArtikkelNr) - 3)
        dArtikkelNr = DEC(cArtikkelNr)
        cBruk       = trim(SUBSTRING(TRIM(ENTRY(12,cRecord,";")),1,1)).
        .

    /* Legger opp brukskoden */
    IF cBruk <> "" THEN
    DO:
        cBruk = cBruk + " Sport 1 Online".
        FIND FIRST Anv-Kod NO-LOCK WHERE
            Anv-Kod.AnvBeskr = cBruk NO-ERROR.
        IF NOT AVAILABLE Anv-Kod THEN
        DO:
            FIND LAST bAnv-Kod NO-ERROR.
            IF AVAILABLE bAnv-Kod THEN
                iAnv-Id = bAnv-Kod.Anv-Id + 1.
            ELSE
                iAnv-Id = 1.
            CREATE Anv-Kod.
            ASSIGN
                Anv-Kod.Anv-Id   = iAnv-Id
                Anv-Kod.AnvBeskr = cBruk 
                NO-ERROR.
            IF AVAILABLE Anv-Kod THEN 
                RELEASE Anv-Kod.
        END.
        ELSE
            iAnv-Id = Anv-Kod.Anv-Id.
    END.
    ELSE
        iAnv-Id = 0.

    FIND ArtBas Exclusive-LOCK WHERE
        ArtBas.ArtikkelNr = dArtikkelNr NO-ERROR.
    IF AVAILABLE ArtBas THEN
        FIND FIRST ArtPris OF ArtBas NO-ERROR.
    IF AVAILABLE ArtPris THEN
    DO:
        ASSIGN
            dVarekost = DEC(ENTRY(15,cRecord,";"))
            dDiff = ArtPris.Varekost[1] - dVarekost
            ArtBas.Anv-Id = 1
            .
        DISPLAY
            cArtikkelNr
            ArtBas.Beskr
            "*" WHEN CAN-FIND(ArtBas WHERE artBas.ArtikkelNr = dArtikkelNr)
            ArtPris.Varekost[1] WHEN AVAILABLE ArtPris
            ArtPris.Pris[1] WHEN AVAILABLE ArtPris
            "|"
            ENTRY(9,cRecord,";") FORMAT "x(30)"
            ENTRY(13,cRecord,";") FORMAT "x(15)" COLUMN-LABEL "Innpris"
            ENTRY(14,cRecord,";") FORMAT "x(15)" COLUMN-LABEL "Selvkost"
            ENTRY(15,cRecord,";") FORMAT "x(15)" COLUMN-LABEL "Utpris"
            "*Diff*" WHEN dDiff <> 0
            dDiff WHEN dDiff <> 0
            WITH WIDTH 220.
    END.

END.
