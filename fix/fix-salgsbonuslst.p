/* 
Genererer en excel fil (csv) som inneholder salgstransaksjoner
for de artikler som ligger i artikkellisten.
*/

CURRENT-WINDOW:WIDTH = 200.

DEF VAR dDato1   AS DATE NO-UNDO.
DEF VAR dDato2   AS DATE NO-UNDO.
DEF VAR cArtLst  AS CHAR NO-UNDO.
DEF VAR cFilNavn AS CHAR NO-UNDO.
DEF VAR piLoop   AS INT  NO-UNDO.

DEF STREAM Ut.

ASSIGN
    cArtLst  = "2106510,2106509,2104106,2105064,2104908,2105053,2106158,2105547"
    dDato1   = 05/27/2005
    dDato2   =06/15/2005
    cFilNavn = "c:\tmp\BonusRap.txt"
    .

OUTPUT STREAM ut TO VALUE(cFilNavn) NO-ECHO.

PUT STREAM ut UNFORMATTED
    "ArtikkelNr;Varetekst;Dato;Antall;Pris;RabKr;Rab%;Varekost;Db%;Selger;Navn;Kasserer;Navn" SKIP
    .

DO piLoop = 1 TO NUM-ENTRIES(cArtLst):
    FIND ArtBas NO-LOCK WHERE
        ArtBas.ArtikkelNr = DEC(ENTRY(piLoop,cArtLst)) NO-ERROR.

    FOR EACH TransLogg NO-LOCK WHERE
        Translogg.ArtikkelNr  = ArtBas.ArtikkelNr AND
        TransLogg.Dato       >=  dDato1 AND
        TransLogg.Dato       <=  dDato2 AND
        CAN-DO("1,10",STRING(Translogg.TTId)):

        IF AVAILABLE Forsalj THEN
            RELEASE Forsalj.
        IF TransLogg.ForsNr > 0 THEN
            FIND Forsalj NO-LOCK WHERE
                Forsalj.ForsNr = int(TransLogg.ForsNr) NO-ERROR.
        IF AVAILABLE Selger THEN
            RELEASE Selger.
        IF Translogg.SelgerNr > 0 THEN FIND Selger NO-LOCK WHERE
            Selger.SelgerNr = Translogg.SelgerNr NO-ERROR.

        PUT STREAM Ut UNFORMATTED
            ArtBAs.ArtikkelNr ";" 
            ArtBas.Beskr ";"
            Translogg.Dato ";"
            Translogg.Antall ";"
            (TransLogg.Pris - TransLogg.RabKr) * Translogg.Antall ";"
            TransLogg.RabKr ";"
            ROUND((((Translogg.RabKr * Translogg.Antall) / (Translogg.Pris * Translogg.Antall)) * 100),1) ";"
            Translogg.VVArekost * Translogg.Antall ";"
            ROUND((((Translogg.Pris - TransLogg.Mva - Translogg.RabKr) - Translogg.VVArekost) * Translogg.Antall) / (Translogg.Pris - TransLogg.Mva - Translogg.RabKr) * Translogg.Antall * 100,1) ";"
            Translogg.SelgerNr ";"
            (IF AVAILABLE Selger
               THEN Selger.Navn
                ELSE "") ";"
            Translogg.ForsNr ";"
            (IF AVAILABLE Forsalj
               THEN Forsalj.FoNamn
                ELSE "")
            SKIP.

    END.
END.

OUTPUT STREAM ut CLOSE.

