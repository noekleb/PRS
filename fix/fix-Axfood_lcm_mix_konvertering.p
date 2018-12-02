CURRENT-WINDOW:WIDTH = 300.

DEF VAR cEAN            AS CHAR NO-UNDO.
DEF VAR lArtikkelNr     AS DEC FORMAT ">>>>>>>>>>>>9" NO-UNDO.
DEF VAR iKamptilbTypeId AS INT NO-UNDO.
DEF VAR bGrenseAnt      AS LOG NO-UNDO.
DEF VAR lKampTilbArtSeq AS INT NO-UNDO.

/* Oppretter Eier */
IF NOT CAN-FIND(FIRST KampanjeEier) THEN
DO TRANSACTION:
    CREATE KampanjeEier.
    ASSIGN
        KampanjeEier.KampEierId = 1
        KampanjeEier.KampEierNavn = 'Aut. opprettet'
        .
    RELEASE KampanjeEier.    
END.
FIND FIRST kampanjeEier NO-LOCK.

FOR EACH Mix NO-LOCK WHERE
    CAN-DO("1,2,17",STRING(mix.ButNr)) AND
    Mix.TilDat > 02/04/2013
    BREAK BY Mix.FraDato
          BY Mix.TilDato
          BY Mix.MixNr
          BY Mix.ProfNr
          BY Mix.ButNr:

    IF FIRST-OF(Mix.FraDato) OR 
        NOT CAN-FIND(KampanjeMixMatch WHERE 
                     KampanjeMixMatch.KampId = DEC(Mix.MixNr)) THEN
    DO TRANSACTION:
        /* Oppretter KampanjeMixMatch. */
        FIND KampanjeMixMatch NO-LOCK WHERE
            KampanjeMixMatch.KampId = DEC(Mix.MixNr) NO-ERROR.
        IF NOT AVAILABLE KampanjeMixMatch THEN
        DO:
            CREATE KampanjeMixMatch.
            ASSIGN
                KampanjeMixMatch.KampId        = DEC(Mix.MixNr)
                KampanjeMixMatch.KampNavn      = TRIM(mix.Mixtekst + ' ' + mix.mixtekst2)
                KampanjeMixMatch.KampKlar      = TRUE
                KampanjeMixMatch.KampEierId    = KampanjeEier.KampEierId
                KampanjeMixMatch.KampStartDato = mix.fraDato
                KampanjeMixMatch.KampStartTid  = mix.fraTid
                KampanjeMixMatch.KampSluttDato = mix.TilDato
                KampanjeMixMatch.KampSluttTid  = mix.TilTid
                KampanjeMixMatch.KampanjeNotat = mix.mixnotat
                .
            FIND CURRENT KampanjeMixMatch NO-LOCK.
        END. 
    END. /* TRANSACTION */
    IF NOT AVAILABLE KampanjeMixMatch THEN
        FIND KampanjeMixMatch NO-LOCK WHERE 
            KampanjeMixMatch.KampId = DEC(Mix.MixNr).

    /* Kampanjetype */
    CASE mix.mixtype:
        WHEN 1 THEN ASSIGN iKamptilbTypeId = 10
                           bGrenseAnt      = TRUE.
        OTHERWISE ASSIGN iKamptilbTypeId = 10
                         bGrenseAnt      = TRUE.
    END CASE.

    FIND KampanjeTilbud NO-LOCK WHERE
        KampanjeTilbud.KampId     = KampanjeMixMatch.KampId AND
        KampanjeTilbud.KampTilbId = INT(Mix.MixNr) NO-ERROR.
    IF NOT AVAILABLE KampanjeTilbud THEN
    DO TRANSACTION:
        CREATE KampanjeTilbud.
        ASSIGN
            KampanjeTilbud.KampId                   = KampanjeMixMatch.KampId 
            KampanjeTilbud.KampTilbId               = INT(Mix.MixNr)
            KampanjeTilbud.KamptilbNavn             = TRIM(mix.Mixtekst + ' ' + mix.mixtekst2)
            KampanjeTilbud.KamptilbTypeId           = iKamptilbTypeId                      
            KampanjeTilbud.KampTilbKvitteringsTekst = KampanjeTilbud.KamptilbNavn 
            KampanjeTilbud.KamptilbPopUpTekstBruk   = FALSE 
            KampanjeTilbud.KampTilbOkning           = FALSE
            KampanjeTilbud.KampTilbBelop            = mix.utpris                       
            KampanjeTilbud.KampTilbGrenseAntall     = mix.antall         
            KampanjeTilbud.HapHourId                = 0                               
            KampanjeTilbud.KamptilbGrenseAntallBruk = bGrenseAnt 
            KampanjeTilbud.KamptilbNotat            = TRIM(mix.Mixtekst + ' ' + mix.mixtekst2)
            .
   
        FIND CURRENT KampanjeTilbud NO-LOCK.
    END. /* TRANSACTION */
    /*    
    DISPLAY
        mix.MixNr
        mix.mixtype
        mix.ProfNr
        mix.butNr
        mix.Mixtekst
        mix.antall
        mix.utpris
        Mix.Subtotal
        mix.fraDato
        STRING(mix.FraTid,"HH:MM")
        mix.tildato
        STRING(mix.TilTid,"HH:MM")
        mix.medlem
        mix.Modellmix
        mix.antgrp
        mix.mixtekst2
        mix.mixnotat FORMAT "x(50)"
        WITH FRAME X WIDTH 300.
    */
    
    LOOPEN:
    FOR EACH MixRad OF Mix NO-LOCK:
        FIND Vare OF MixRad NO-LOCK.

        cEAN = STRING(MixRad.EAN).
        RUN bibl_chkean.p (INPUT-OUTPUT cEAN).
        FIND Strekkode NO-LOCK WHERE
            Strekkode.Kode = cEAN NO-ERROR.
        IF AVAILABLE Strekkode THEN
            lArtikkelNr = Strekkode.ArtikkelNr.
        ELSE NEXT LOOPEN.

        FIND FIRST KampanjeTilbArtikkel NO-LOCK WHERE
            KampanjeTilbArtikkel.KampId        = KampanjeMixMatch.KampId AND
            KampanjeTilbArtikkel.KampTilbId    = KampanjeTilbud.KampTilbId AND
            KampanjeTilbArtikkel.KampTilbArtId = lArtikkelNr NO-ERROR.
        IF NOT AVAILABLE KampanjeTilbArtikkel THEN
        DO TRANSACTION:
            lKampTilbArtSeq = 0.
            FIND LAST KampanjeTilbArtikkel NO-LOCK USE-INDEX KampTilbArtSeq NO-ERROR.
            IF AVAILABLE KampanjeTilbArtikkel THEN
                lKampTilbArtSeq = KampanjeTilbArtikkel.KampTilbArtSeq + 1.
            ELSE 
                lKampTilbArtSeq = 1.
       
            CREATE KampanjeTilbArtikkel.
            ASSIGN
                KampanjeTilbArtikkel.KampId               = KampanjeMixMatch.KampId 
                KampanjeTilbArtikkel.KampTilbId           = KampanjeTilbud.KampTilbId 
                KampanjeTilbArtikkel.KampTilbArtId        = lArtikkelNr
                KampanjeTilbArtikkel.KampTilbArtSeq       = lKampTilbArtSeq           
                KampanjeTilbArtikkel.KampTilbArtBelop     = MixRad.Utpris         
                KampanjeTilbArtikkel.KampTilbArtMinAntall = MixRad.Antall     
                KampanjeTilbArtikkel.KampRabattTypeId     = 0         
                KampanjeTilbArtikkel.ProdFamId            = 0         
                .
       
            FIND CURRENT KampanjeTilbArtikkel NO-LOCK.
        END. /* TRANSACTION */

        /* ButikkKobling */
        FOR EACH Butiker NO-LOCK WHERE
            Butiker.Butik = MixRad.ButNr:
            FIND KampanjeButikker NO-LOCK WHERE
                KampanjeButikker.KampId = KampanjeMixMatch.KampId AND
                KampanjeButikker.Butik  = Butiker.Butik NO-ERROR.
            IF NOT AVAILABLE KampanjeButikker THEN
            DO TRANSACTION:
                CREATE KampanjeButikker.
                ASSIGN
                    KampanjeButikker.KampId = KampanjeMixMatch.KampId
                    KampanjeButikker.Butik  = Butiker.Butik
                    .
                FIND CURRENT KampanjeButikker NO-LOCK.
            END. /* TRANSACTION */
        END.
        /*
        DISPLAY
            MixRad.MixNr
            MixRad.ProfNr
            MixRad.ButNr
            lArtikkelNr
            MixRad.EAN
            Vare.VareTekst
            MixRad.Antall
            MixRad.Utpris
            MixRad.ModellMix
            MixRad.GrpNr SKIP
            WITH FRAME G DOWN WITH WIDTH 300.
        DOWN WITH FRAME G.
        */
    END. /* LOOPEN */
    
END.
