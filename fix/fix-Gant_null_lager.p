

DEF VAR iBatchNr   AS DEC  NO-UNDO.
DEF VAR piSeqNr    AS INT NO-UNDO.
DEF VAR piTransNr  AS INT NO-UNDO.
DEF VAR lAnt     AS DEC NO-UNDO.

DEF VAR lAntall AS DEC NO-UNDO.
DEF VAR cLogg AS CHAR NO-UNDO.

cLogg = 'fix-Gant_null_lager.p' + REPLACE(STRING(TODAY),'/','').

DEF BUFFER bufArtPris FOR ArtPris.
DEF BUFFER bufArtLag FOR ArtLag.

CURRENT-WINDOW:WIDTH = 350.


RUN bibl_logg.p (cLogg, '').
RUN bibl_logg.p (cLogg, 'Start.').

RUN batchlogg.p ("Korreksjon 9/11-16: ",
                   "Nullstill 16" +
                   string(today) +
                   " " +
                   string(TIME,"HH:MM") +
                   " " +
                   USERID("dictdb"),
                   OUTPUT iBatchNr).

FOR EACH PkSdlHode NO-LOCK, 
    EACH PkSdlMottak NO-LOCK WHERE 
         PkSdlMottak.PkSdlId     = PkSdlHode.PkSdlId AND
         PkSdlMottak.MotTattDato = 11/09/2016,
    EACH PkSdlLinje OF PkSdlHode NO-LOCK WHERE
         PkSdlLinje.ButikkNr = 16,
    FIRST ArtPris EXCLUSIVE-LOCK WHERE
        ArtPris.ArtikkelNr = PkSdlLinje.ArtikkelNr AND
        ArtPris.ProfilNr = PkSdlLinje.butikkNr:

    FIND bufArtPris NO-LOCK WHERE
        bufArtPris.ArtikkelNr = ArtPris.ArtikkelNr AND
        bufArtPris.ProfilNr = 1 NO-ERROR.
    
    /* Skaper transaksjoner for å trekke lager til 0. */
    LAGERLOOP:
    FOR EACH ArtLag NO-LOCK WHERE
        ArtLag.ArtikkelNr = PkSdlLinje.ArtikkelNr AND
        ArtLag.Butik      = PkSdlLinje.butikkNr AND
        ArtLag.StrKode    = PkSdlLinje.StrKode:

        /* Her skal det ikke gjøres noe. */
        IF ArtLag.LagAnt = 0 THEN
            NEXT.

        FIND bufArtLag NO-LOCK WHERE
            bufArtLag.ArtikkelNr = PkSdlLinje.ArtikkelNr AND
            bufArtLag.Butik      = 50 AND
            bufArtLag.StrKode    = PkSdlLinje.StrKode NO-ERROR.

        /* Er lagerteller lik, skal det ikke gjøres noe. */
        IF AVAILABLE bufArtLag AND ArtLag.LagAnt >= bufArtLag.LagAnt THEN
            NEXT.
        IF NOT AVAILABLE bufArtLag THEN
            NEXT.
        IF bufArtLag.LagAnt <= 0 THEN
            NEXT.
        FIND ArtBAs NO-LOCK WHERE
            ArtBas.ArtikkelNr = PkSdlLinje.ArtikkelNr NO-ERROR.
        FIND Lager NO-LOCK WHERE
            Lager.ArtikkelNr = PkSdlLinje.ArtikkelNr AND
            Lager.butik      = PkSdlLinje.butikkNr NO-ERROR.

        /* Beregner antall som skal overføres. */
        lAnt = bufArtLag.LagAnt - ArtLag.LagAnt.
        IF lAnt <= 0 THEN
            NEXT.

        /* Setter transaksjonsnummer  */
        IF piTransNr = 0 THEN
        DO:
            FIND LAST TransLogg WHERE
                TransLogg.Butik = ArtLag.Butik
                 USE-INDEX TransLogg NO-ERROR.
            IF AVAILABLE TransLogg THEN
                piTransNr = TransLogg.TransNr + 1.
            ELSE
                piTransNr = 1.
        END.
        ELSE
            piTransNr = piTransNr + 1.

        TRANSLOGGEN:
        DO:
            /* Oppretter TransLogg */    
            CREATE TransLogg.
            NYTRANSLOGG:
            DO WHILE TRUE ON ERROR UNDO, RETRY:
                ASSIGN TransLogg.Butik        = 50
                       TransLogg.TransNr      = piTransNr
                       TransLogg.SeqNr        = 1
                       TransLogg.Ovbutik      = 16
                       NO-ERROR.
                IF ERROR-STATUS:ERROR THEN
                    piTransNr = piTransNr + 1.
                ELSE LEAVE NYTRANSLOGG.
            END. /* NYTRANSLOGG */

            ASSIGN
                   TransLogg.BatchNr      = iBatchNr
                   TransLogg.TTId         = 6 /* Overføring. */
                   TransLogg.TBId         = 1
                   TransLogg.ArtikkelNr   = ArtLag.ArtikkelNr
                   TransLogg.Vg           = ArtBas.Vg
                   TransLogg.LopNr        = ArtBas.LopNr
                   TransLogg.Antall       = lAnt
                   TransLogg.Pris         = Lager.VVareKost
                   TransLogg.RabKr        = 0

                   TransLogg.LevNr        = IF AVAILABLE ArtBas
                                              THEN ArtBas.LevNr
                                              ELSE 0
                   TransLogg.ForsNr       = 1
                   TransLogg.Plukket      = TRUE 
                   TransLogg.Dato         = TODAY 
                   TransLogg.Tid          = 0
                   TransLogg.SelgerNr     = 1
                   TransLogg.BestNr       = 0
                   TransLogg.Postert      = FALSE
                   Translogg.PostertDato = ?
                   Translogg.BongTekst    = ArtBas.Beskr
                   TransLogg.VVareKost    = Lager.VVareKost
                   TransLogg.VVareKost    = IF TransLogg.VVAreKost = ? THEN 0 ELSE Translogg.VVareKost
                   TransLogg.SattVVarekost = FALSE
                   TransLogg.KalkylePris  = IF AVAILABLE ArtPris
                                              THEN ArtPris.Pris[IF ArtPris.Tilbud THEN 2 ELSE 1]
                                              ELSE Translogg.KalkylePris
                   TransLogg.Varekost     = Lager.VVareKost
                   TransLogg.Pris         = TransLogg.Varekost 
                   .
              ASSIGN
                TransLogg.Pris          = TransLogg.Varekost
                TransLogg.RabKr         = 0
                TransLogg.SubtotalRab   = 0
                TransLogg.VVareKost     = TransLogg.Varekost
                TransLogg.SattVVarekost = TRUE
                TransLogg.Mva           = 0
                TransLogg.Mva%          = 0
                TransLogg.Storl        = ArtLag.Storl
                TransLogg.TilStorl     = Artlag.Storl
                Translogg.RefTekst     = 'fix-Gant_null_lager'
                .

        END. /* TRANSLOGGEN */


    END. /* LAGERLOOP */

    /*
    DISPLAY
        PkSdlHode.PkSdlNr
        PksdlMottak.MotTattDato
        PkSdlLinje.butikkNr
        PkSdlLinje.ArtikkelNr
        ArtPris.InnkjopsPris[1]
        ArtPris.Rab1%[1]
        ArtPris.Pris[1]
        '|'
        bufArtPris.InnkjopsPris[1]
        bufArtPris.Rab1%[1]
        bufArtPris.Pris[1]
    WITH WIDTH 350.
    */

END.

RUN bibl_logg.p (cLogg, 'Slutt.').


