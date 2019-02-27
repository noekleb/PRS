DEFINE VARIABLE iNettButNr   AS INTEGER NO-UNDO.
DEFINE VARIABLE iNettLagerNr AS INTEGER NO-UNDO.
DEFINE VARIABLE iBatchNr     AS INTEGER NO-UNDO.

DEF VAR dDato AS DATE NO-UNDO.

{syspara.i 150 1 2 iNettButNr INT}
{syspara.i 150 1 3 iNettLagerNr INT}

FUNCTION FixStorl RETURNS CHARACTER 
( pcStorl AS CHAR ) FORWARD.


FORM
    WITH FRAME Gurre DOWN.


DO dDato = 03/01/2015 TO TODAY:
    FOR EACH BongHode NO-LOCK WHERE
        BongHode.ButikkNr = 15 AND
        BongHode.Dato = dDato USE-INDEX ButDato:

        DISPLAY
            BongHode.Dato
            WITH FRAME Gurre DOWN.

        FOR EACH BongLinje EXCLUSIVE-LOCK WHERE
          BongLinje.B_Id = BongHode.B_Id AND
          BongLinje.LinjeNr >= 0 AND 
          BongLinje.TTId = 10:

          RUN posterOverforing.

          DISPLAY
            BongHode.ButikkNR
            BongHode.Dato
            BongLinje.TTID
            BongLinje.Antall
          WITH FRAME Gurre DOWN.
          DOWN WITH FRAME Gurre.
          PAUSE 0.
        END.
    END.
END.

/* Flagger batchen klar for oppdatering. */
IF iBatchNr > 0 THEN 
    RUN batchstatus.p (iBatchNr, 2).


PROCEDURE posterOverforing:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE piTransNr AS INTEGER NO-UNDO.    
    
    /* Batch for TransLogg */
    IF iBatchNr = 0 THEN 
        RUN batchlogg.p (PROGRAM-NAME(1),
            "RETUR fra kundeordre " +
            string(TODAY) +
            " " +
            string(TIME,"HH:MM") +
            " " +
            USERID("dictdb"),
            OUTPUT iBatchNr).
 
    IF AVAILABLE ArtBas THEN RELEASE ArtBas.
    IF AVAILABLE ArtPris THEN RELEASE ArtPris.
    
    FIND ArtBas NO-LOCK WHERE 
        ArtBas.ArtikkelNr = DEC(BongLinje.ArtikkelNr) NO-ERROR.
    IF AVAILABLE ArtBas THEN 
    DO:
        FIND ArtPris NO-LOCK WHERE 
            ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND 
            ArtPris.ProfilNr   = Butiker.ProfilNr NO-ERROR.
        IF NOT AVAILABLE ArtPris THEN 
            FIND FIRST ArtPris WHERE 
                ArtPris.ArtikkelNR = ArtBas.ArtikkelNr NO-ERROR.
    END. 
    
    /* Setter transaksjonsnummer  */
    IF piTransNr = 0 THEN
    DO:
        FIND LAST TransLogg WHERE 
            TransLogg.Butik = BongLinje.ButikkNr
            USE-INDEX TransLogg NO-ERROR.
        IF AVAILABLE TransLogg THEN
            piTransNr = TransLogg.TransNr + 1.
        ELSE
            piTransNr = 1.
    END.
    ELSE
        piTransNr = piTransNr + 1.

    /* Oppretter TransLogg */    
    CREATE TransLogg.
    NYTRANSLOGG:
    DO WHILE TRUE ON ERROR UNDO, RETRY:
        ASSIGN 
            TransLogg.Butik     = BongLinje.ButikkNr
            TransLogg.TransNr   = piTransNr
            TransLogg.SeqNr     = 1
            TransLogg.OvButik   = iNettLagerNr
            /* Setter inn pekere på transaksjonene */
            BongLinje.TransNr   = piTransNr
            BongLinje.SeqNr     = 1
                   NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            piTransNr = piTransNr + 1.
        ELSE LEAVE NYTRANSLOGG.
    END. /* NYTRANSLOGG */

    ASSIGN
        TransLogg.BatchNr       = iBatchNr
        TransLogg.TTId          = 6 /* Overføring */
        TransLogg.TBId          = 1
        TransLogg.ArtikkelNr    = IF AVAILABLE ArtBas
                                          THEN ArtBas.ArtikkelNr
                                          ELSE 0
        TransLogg.Vg            = BongLinje.VareGr
        TransLogg.LopNr         = BongLinje.LopeNr
        TransLogg.Antall        = BongLinje.Antall
        TransLogg.KundNr        = BongHode.KundeNr

        TransLogg.LevNr         = IF AVAILABLE ArtBas
                                          THEN ArtBas.LevNr
                                          ELSE 0
        TransLogg.OvTransNr     = TransLogg.TransNr
        TransLogg.BongId        = BongLinje.BongNr
        TransLogg.BongLinjeNr   = BongLinje.LinjeNr
        TransLogg.KassaNr       = BongLinje.KasseNr
        TransLogg.ForsNr        = BongHode.KassererNr
        TransLogg.Plukket       = IF BongLinje.TTId = 6 THEN TRUE ELSE FALSE
        TransLogg.Dato          = BongLinje.TransDato
        TransLogg.Tid           = BongLinje.TransTid
        TransLogg.SelgerNr      = BongHode.SelgerNr
        TransLogg.BestNr        = 0
        TransLogg.Postert       = FALSE
        TransLogg.KortNr        = (IF BongHode.KortType = 2
                                           THEN BongHode.KundeKort
                                           ELSE BongHode.MedlemsKort)
        TransLogg.KundNr        = BongHode.KundeNr
        TransLogg.MedlemsNr     = BongHode.MedlemsNr
        TransLogg.KortType      = BongHode.KortType
        TransLogg.RefNr         = BongLinje.RefNr
        TransLogg.RefTekst      = BongLinje.RefTekst
        Translogg.Kode          = Bonglinje.Strekkode
        Translogg.BongTekst     = BongLinje.BongTekst
        TransLogg.VVareKost     = BongLinje.VVareKost / ABS(BongLinje.Antall)
        TransLogg.VVareKost     = IF TransLogg.VVareKost = ? THEN 0 ELSE Translogg.VVareKost
        TransLogg.SattVVarekost = (IF AVAILABLE ArtBas AND ArtBas.Lager = TRUE 
                                            THEN FALSE 
                                          ELSE IF CAN-DO("1,3,10",STRING(Translogg.TTId))
                                            THEN TRUE /* Skal ikke regnes om ved opp. av statistikker. */
                                          ELSE FALSE)
        TransLogg.KalkylePris   = IF AVAILABLE ArtPris
                                          THEN ArtPris.Pris[IF ArtPris.Tilbud THEN 2 ELSE 1]
                                          ELSE Translogg.KalkylePris
        TransLogg.Varekost      = IF AVAILABLE ArtPris
                                          THEN ArtPris.Varekost[IF ArtPris.Tilbud THEN 2 ELSE 1]
                                          ELSE TransLogg.Varekost
        TransLogg.Pris          = TransLogg.vVarekost
        TransLogg.RabKr         = 0
        TransLogg.SubtotalRab   = 0
        TransLogg.Mva           = 0
        TransLogg.Mva%          = 0
        .
    ASSIGN 
        TransLogg.Storl    = FixStorl(BongLinje.Storrelse)
        TransLogg.TilStorl = TransLogg.Storl
        .
END PROCEDURE.

FUNCTION FixStorl RETURNS CHARACTER 
	( pcStorl AS CHAR ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/	
    ASSIGN
        pcStorl = TRIM(pcStorl)
        pcStorl = CAPS(pcStorl)
        pcStorl = IF (LENGTH(pcStorl) = 1 OR 
                 LENGTH(pcStorl) = 3
                 ) 
                then " " + pcStorl
                else pcStorl.          

    /* Bytter ut eventuelle comma med punkt. */
    IF INDEX(pcStorl,",") <> 0 THEN
        OVERLAY(pcStorl, INDEX(pcStorl,","), 1, "CHARACTER") = ".".

    RETURN pcStorl.   /* Function return value. */
END FUNCTION.

