DEF    VAR      cArtLst     AS CHAR    NO-UNDO.
DEF    VAR      lArtikkelNr AS DEC     NO-UNDO.
DEF    VAR      iLoop       AS INT     NO-UNDO.
DEFINE VARIABLE iBatchNr    AS INTEGER NO-UNDO.
DEFINE VARIABLE ibutNr      AS INTEGER NO-UNDO.
DEFINE VARIABLE ibufTransNr AS INTEGER NO-UNDO.
DEFINE VARIABLE iTransNr    AS INTEGER NO-UNDO.

CURRENT-WINDOW:WIDTH = 350.

DEF    BUFFER bufTranslogg FOR TransLogg.
DEFINE BUFFER cpTranslogg  FOR TransLogg.

ASSIGN
    ibutNr  = 40
    /*cArtLst = '9828686,9828689,9828680,9828669,9828661,9828660,9828653,9828652'*/
    cArtLst = '' /*  */
    .

/* Setter batchNr */
RUN batchlogg.p (PROGRAM-NAME(1),
    "Fiks av varemottak som er postert med feil kalkyle - motpostering",
    OUTPUT iBatchNr).

DO iLoop = 1 TO NUM-ENTRIES(cArtLst):
    ASSIGN 
        lArtikkelNr = DEC(ENTRY(iLoop,cArtLst)).

    FIND ArtBAs WHERE 
        ArtBas.ArtikkelNr = lArtikkelNr NO-LOCK NO-ERROR.
    FIND ArtPris NO-LOCK WHERE 
        ArtPris.ArtikkelNr = artBas.ArtikkelNr AND
        ArtPris.ProfilNr = 2 NO-ERROR.
        
    DISPLAY
        ArtBas.ArtikkelNr
        ArtBas.Beskr
        ArtBas.LevKod
        ArtPris.InnkjopsPris[1]
        ArtPris.Rab1%[1]
        ArtPris.VareKost[1]
        ArtPris.Pris[1]
        WITH WIDTH 350.

    FIND LAST bufTransLogg WHERE
        bufTransLogg.Butik = iButNr
        USE-INDEX TransLogg NO-ERROR.
    IF AVAILABLE bufTransLogg THEN
        iTransNr = bufTransLogg.TransNr + 1.
    ELSE
        iTransNr = 1.

    FOR EACH Translog NO-LOCK WHERE
        TransLogg.ArtikkelNr = lArtikkelNr AND
        Translogg.butik = ibutNr AND 
        Translogg.Dato  = 10/05/2017 AND
        TransLogg.TTId = 5 AND
        Translogg.VVareKost < 0:
     
        /* --------------------
        /* Kopierer varemottaks transaksjonen */
        CREATE cpTransLogg.
        BUFFER-COPY TransLogg EXCEPT TransNr BatchNr TO  cpTransLogg
            ASSIGN
            cpTransLogg.TransNr     = iTransNr
            cpTransLogg.BatchNr     = iBatchNr
            cpTransLogg.Antall      = TransLogg.Antall * -1
            cpTransLogg.Postert     = FALSE
            cpTransLogg.PostertDato = ?
            cpTransLogg.PostertTid  = 0
            cpTransLogg.FeilKode    = 0
            cpTransLogg.Plukket     = TRUE 
            iTransNr                = iTransNr + 1.
        .

        /* Kopierer varemottaks transaksjonen */
        CREATE cpTransLogg.
        BUFFER-COPY TransLogg EXCEPT TransNr BatchNr TO  cpTransLogg
            ASSIGN
            cpTransLogg.TransNr     = iTransNr
            cpTransLogg.BatchNr     = iBatchNr
            cpTransLogg.Antall      = TransLogg.Antall
            cpTransLogg.Postert     = FALSE
            cpTransLogg.PostertDato = ?
            cpTransLogg.PostertTid  = 0
            cpTransLogg.FeilKode    = 0
            cpTransLogg.Plukket     = TRUE 
            cpTransLogg.VVarekost   = ArtPris.VareKost[1]
            cpTransLogg.Pris        = ArtPris.VareKost[1]
            iTransNr                = iTransNr + 1
            .
        ------------------------- */
        
        DISPLAY
            TransLogg.BatchNr FORMAT ">>>>>>>9"
            Translogg.TTId
            TransLogg.TBId
            Translogg.Dato
            ArtBas.ArtikkelNr
            ArtBas.Beskr
            ArtBas.LevKod
            Translogg.VVareKost
            Translogg.Antall
            Translogg.VVarekost
            TransLogg.Pris
            WITH WIDTH 350.
        
    BUFTRANSLOGG_LOOP:
    FOR EACH bufTranslogg NO-LOCK WHERE
        bufTransLogg.ArtikkelNr = TransLogg.ArtikkelNr AND
        CAN-DO('20,40',STRING(bufTransLogg.Butik)) /*TransLogg.Butik*/ AND
        bufTransLogg.Storl      = TransLogg.Storl AND
        bufTransLogg.Dato      >= TransLogg.Dato AND 
        bufTransLogg.Postert    = TRUE:
                
        IF bufTranslogg.TTId = 5 THEN 
            NEXT.

        IF RECID(bufTransLogg) = RECID(TransLogg) THEN
            NEXT.
            
        IF (bufTransLogg.Pris < 0 OR bufTransLogg.VVarekost < 0) THEN . /* Gjør ingenting */
        ELSE NEXT.

        FIND Lager NO-LOCK WHERE 
            Lager.ArtikkelNr = TransLogg.ArtikkelNr AND
            Lager.Butik = Translogg.butik NO-ERROR.

        iTransNr = iTransNr + 1.
    
        /* Kopierer varemottaks transaksjonen */
        CREATE cpTransLogg.
        BUFFER-COPY bufTransLogg EXCEPT TransNr BatchNr TO  cpTransLogg
            ASSIGN
            cpTransLogg.TransNr     = iTransNr
            cpTransLogg.BatchNr     = iBatchNr
            cpTransLogg.Antall      = bufTransLogg.Antall * -1
            cpTransLogg.Postert     = FALSE
            cpTransLogg.PostertDato = ?
            cpTransLogg.PostertTid  = 0
            cpTransLogg.FeilKode    = 0
            cpTransLogg.Plukket     = TRUE 
            iTransNr                = iTransNr + 1.
        .

        /* Kopierer varemottaks transaksjonen */
        CREATE cpTransLogg.
        BUFFER-COPY bufTransLogg EXCEPT TransNr BatchNr TO  cpTransLogg
            ASSIGN
            cpTransLogg.TransNr     = iTransNr
            cpTransLogg.BatchNr     = iBatchNr
            cpTransLogg.Antall      = bufTransLogg.Antall
            cpTransLogg.Postert     = FALSE
            cpTransLogg.PostertDato = ?
            cpTransLogg.PostertTid  = 0
            cpTransLogg.FeilKode    = 0
            cpTransLogg.Plukket     = TRUE 
            cpTransLogg.VVarekost   = ArtPris.VareKost[1]
            cpTransLogg.Pris        = ArtPris.Pris[1]
            iTransNr                = iTransNr + 1
            .
        
        DISPLAY
            TransLogg.BatchNr FORMAT ">>>>>>>9"
            Translogg.TTId
            Translogg.Dato
            ArtBas.ArtikkelNr
            ArtBas.Beskr
            ArtBas.LevKod
            Translogg.VVareKost
            Lager.VVAreKost
            '|'
            bufTransLogg.butik
            bufTransLogg.TTId
            bufTransLogg.Dato
            bufTransLogg.Antall
            bufTranslogg.VVareKost
            bufTransLogg.Pris
            WITH WIDTH 350.
    END. /* BUFTRANSLOGG_LOOP */
    END.
    PAUSE.
END.




