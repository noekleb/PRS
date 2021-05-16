/* lagerlisltePrEan.p
   9/5-21 TN 
*/

DEF VAR cButLst AS CHAR NO-UNDO.
DEF VAR iLoop AS INT NO-UNDO.
DEF VAR cFilNavn AS CHAR NO-UNDO.

DEF STREAM Ut.

ASSIGN 
    cButLst = '11'
    cFilNavn = 'konv\lagerlisteEan' + STRING(TODAY,"99999999") + '.csv'
    .

DEF TEMP-TABLE ttLagerEAN
    FIELD ButNr AS INT FORMAT ">>>>>9"
    FIELD EAN AS CHAR FORMAT "x(15)"
    FIELD Varetekst AS CHAR FORMAT "x(40)"
    FIELD ModellNr AS CHAR FORMAT "x(25)"
    FIELD Varemerke AS CHAR FORMAT "x(25)"
    FIELD kategori1 AS CHAR FORMAT "x(15)"
    FIELD Kategori2 AS CHAR FORMAT "x(15)"
    FIELD Fargekode AS CHAR FORMAT "x(15)"
    FIELD Storrelse AS CHAR FORMAT "x(15)"
    FIELD LagerAntall AS INT FORMAT "->>,>>>,>>9"
    FIELD Pris AS INT FORMAT "->>,>>>,>>9"
    FIELD VVarekost AS INT FORMAT "->>,>>>,>>9"
    FIELD Varekost AS INT FORMAT "->>,>>>,>>9"
    FIELD InnkjPrist AS INT FORMAT "->>,>>>,>>9"
    FIELD Rabatt AS INT FORMAT "->>,>>>,>>9"
    FIELD LC AS INT FORMAT "->>,>>>,>>9"
    INDEX idxButEan AS PRIMARY UNIQUE ButNr Ean.

CURRENT-WINDOW:WIDTH = 350.


BUTLOOP:
DO iLoop = 1 TO NUM-ENTRIES(cButLst):
    FIND Butiker NO-LOCK WHERE 
        Butiker.Butik = INT(ENTRY(iLoop,cButLst)) NO-ERROR.
    IF NOT AVAILABLE Butiker THEN
        NEXT.
    RUN byggListe (INT(ENTRY(iLoop,cButLst))).
END. /* BUTLOOP */

IF CAN-FIND(FIRST ttLagerEan) THEN
    RUN SkrivRapport.

RETURN.


PROCEDURE ByggListe:
    DEF INPUT PARAMETER piButNr AS INT NO-UNDO.

    LAGERLOOP:
    FOR EACH Lager NO-LOCK WHERE 
        Lager.Butik = piButNr AND 
        Lager.LagAnt > 0,
        FIRST ArtBAs OF Lager NO-LOCK,
        EACH ArtLAg OF Lager NO-LOCK WHERE 
        ArtLag.LagAnt > 0:

        FIND ArtPris NO-LOCK WHERE 
            ArtPris.Artikkelnr = Lager.ArtikkelNr AND 
            ArtPris.ProfilNr   = Butiker.ProfilNr NO-ERROR.
        IF NOT AVAILABLE ArtPris THEN
        FIND ArtPris NO-LOCK WHERE 
            ArtPris.Artikkelnr = Lager.ArtikkelNr AND 
            ArtPris.ProfilNr   = 1 NO-ERROR.
        FIND StrKonv NO-LOCK WHERE 
            StrKonv.Storl = ArtLag.Storl NO-ERROR.
        IF AVAILABLE StrKonv THEN 
            FIND Strekkode NO-LOCK WHERE
                Strekkode.ArtikkelNr = Lager.ArtikkelNr AND 
                Strekkode.StrKode = StrKonv.StrKode NO-ERROR.

        IF AVAILABLE Strekkode THEN
        STREKKODEBLOKK:
        DO:
            FIND Anv-Kod OF ArtBas NO-LOCK NO-ERROR.
            FIND HovedKategori OF ArtBas NO-LOCK NO-ERROR.
            FIND ttLagerEan WHERE 
                ttLagerEan.ButNr = Lager.Butik AND 
                ttLagerEan.Ean   = Strekkode.Kode NO-ERROR.
            IF NOT AVAILABLE ttLagerEan THEN
            DO:
                CREATE ttLagerEan.
                ASSIGN
                    ttLagerEan.ButNr = Lager.Butik
                    ttLagerEan.Ean   = Strekkode.Kode
                    .
            END.
            ASSIGN 
                ttLagerEan.Varetekst = ArtBas.Beskr
                ttLagerEan.ModellNr = ArtBas.LevKod
                ttLagerEan.Varemerke = (IF AVAILABLE Varemerke THEN Varemerke.Beskrivelse ELSE '')
                ttLagerEan.kategori1 = STRING(ArtBas.Anv-Id) + ' ' + (IF AVAILABLE Anv-Kod THEN Anv-Kod.AnvBeskr ELSE '')
                ttLagerEan.Kategori2 = STRING(ArtBas.HovedKatNr) +  ' ' + (IF AVAILABLE Hovedkategori THEN Hovedkategori.HovedKatTekst ELSE '')
                ttLagerEan.Fargekode = ArtBas.LevFargKod
                ttLagerEan.Storrelse = ArtLag.Storl
                ttLagerEan.LagerAntall = ArtLag.Lagant
                ttLagerEan.Pris = (IF AVAILABLE ArtPris THEN artPris.Pris[1] ELSE 0)
                ttLagerEan.VVarekost = Lager.VVareKost
                ttLagerEan.Varekost = (IF AVAILABLE ArtPris THEN artPris.InnkjopsPris[1] ELSE 0)
                ttLagerEan.Rabatt = (IF AVAILABLE ArtPris THEN artPris.Rab1%[1] ELSE 0)
                ttLagerEan.InnkjPrist = (IF AVAILABLE ArtPris THEN artPris.Varekost[1] ELSE 0)
                ttLagerEan.LC = ArtBas.KjedeInnkPris
                .

        END. /* STREKKODEBLOKK */
    END. /* LAGERLOOP */

END PROCEDURE.

PROCEDURE SkrivRapport:

    OUTPUT STREAM Ut TO VALUE(cFilNavn).
    PUT STREAM Ut UNFORMATTED
        'ButNr;'
        'Ean;'
        'Varetekst;'
        'ModellNr;'
        'Varemerke;'
        'kategori1;'
        'Kategori2;'
        'Fargekode;'
        'Storrelse;'
        'LagerAntall;'
        'Pris;'
        'VVarekost;'
        'Varekost;'
        'Rabatt;'
        'InnkjPrist;'
        'LC'
        SKIP.
    
    UTSKRIFT:
    FOR EACH ttLagerEan:
        PUT STREAM Ut UNFORMATTED
            ttLagerEan.ButNr ';'
            ttLagerEan.Ean ';'
            ttLagerEan.Varetekst ';'
            ttLagerEan.ModellNr ';'
            ttLagerEan.Varemerke ';'
            ttLagerEan.kategori1 ';'
            ttLagerEan.Kategori2 ';'
            ttLagerEan.Fargekode ';'
            ttLagerEan.Storrelse ';'
            ttLagerEan.LagerAntall ';'
            ttLagerEan.Pris ';'
            ttLagerEan.VVarekost ';'
            ttLagerEan.Varekost ';'
            ttLagerEan.Rabatt ';'
            ttLagerEan.InnkjPrist ';'
            ttLagerEan.LC
            SKIP.

    END. /* UTSKRIFT */
    OUTPUT STREAM Ut CLOSE.

END PROCEDURE.

