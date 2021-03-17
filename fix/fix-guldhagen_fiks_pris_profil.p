CURRENT-WINDOW:WIDTH = 300.

DEF VAR iButNr AS INT NO-UNDO.
DEF VAR iProfNr AS INT NO-UNDO.
DEF VAR iLokProfilNr AS INT NO-UNDO.

DEF BUFFER bufArtPris FOR ArtPris.

ASSIGN
    iButNr       = 1
    iProfNr      = 1
    iLokProfilNr = 1000001
    .

FOR EACH ArtPris EXCLUSIVE-LOCK WHERE
    ArtPris.ArtikkelNr = 2105 AND  
    ArtPris.ProfilNr = 1 /*AND 
    ArtPris.EDAto = ?*/,
    FIRST ArtBas OF ArtPris NO-LOCK,
    FIRST Strekkode OF ArtBas NO-LOCK:

    FIND vare WHERE vare.ean = DECI(Strekkode.Kode) NO-LOCK NO-ERROR.
    IF NOT AVAIL vare THEN 
    DO:
        FIND tandem WHERE tandem.tandemean = DECI(Strekkode.Kode) NO-LOCK NO-ERROR.
        IF AVAIL tandem THEN
            FIND vare WHERE vare.ean = tandem.ean NO-LOCK NO-ERROR.
    END.



    IF AVAILABLE Vare THEN
      FIND FIRST Pris NO-LOCK WHERE
        Pris.EAN    = Vare.EAN AND
        Pris.ProfNr = iProfNr AND
        Pris.ButNr  = iButNr NO-ERROR.
    IF AVAILABLE Vare THEN
        FIND vargr WHERE vargr.vg = vare.hgr NO-LOCK NO-ERROR.
    IF AVAILABLE VarGr THEN
        FIND moms OF vargr NO-LOCK.

    IF AVAILABLE Pris AND
        (Pris.EngrosN <> ArtPris.VareKost[1] OR 
         Pris.UtprisN <> ArtPris.Pris[1]) THEN
    KORREKSJON:
    DO:

        DISPLAY
            ArtPris.ArtikkelNr
            ArtBas.LevKod
            ArtBas.Beskr FORMAT "x(40)"
            ArtPris.EDato
            ArtPris.Mva%[1]
            Moms.MomsProc
            ArtPris.VareKost[1]
            ArtPris.Pris[1]
            '|'
            Pris.ButNr WHEN AVAILABLE Pris
            Pris.ProfNr WHEN AVAILABLE Pris
            Pris.EngrosN WHEN AVAILABLE Pris
            '*' WHEN AVAILABLE Pris AND Pris.EngrosN <> ArtPris.VareKost[1]
            Pris.UtPrisN WHEN AVAILABLE Pris
            '*' WHEN AVAILABLE Pris AND Pris.UtPrisN <> ArtPris.Pris[1]
            /*
            Pris.Rab1N WHEN AVAILABLE Pris
            Pris.Rab2N WHEN AVAILABLE Pris
            Pris.Rab3N WHEN AVAILABLE Pris
            */
        WITH WIDTH 300.

        FIND bufArtPris EXCLUSIVE-LOCK WHERE
            bufArtPris.ArtikkelNr  = ArtBas.ArtikkelNr AND
            bufArtPris.ProfilNr    = iLokProfilNr NO-ERROR.
        IF NOT AVAILABLE bufArtPris THEN
        DO:
            CREATE bufArtPris.
            BUFFER-COPY ArtPris EXCEPT ProfilNr
                TO bufArtPris
                ASSIGN
                bufArtPris.ProfilNr = iLokProfilNr.
        END.
        ELSE 
            BUFFER-COPY ArtPris EXCEPT ProfilNr
                TO bufArtPris
                ASSIGN
                bufArtPris.ProfilNr = iLokProfilNr.

        /* Korr av innkjøpspris */
        IF Pris.EngrosN <> ArtPris.VareKost[1] THEN
        INNKJOPSPRIS:
        DO:
            ASSIGN
                bufArtPris.ValPris[1]   = pris.engrosn
                bufArtPris.Rab1Kr[1]    = 0
                bufArtPris.Rab2Kr[1]    = 0
                bufArtPris.Rab3Kr[1]    = 0
                bufArtPris.Rab1%[1]     = 0
                bufArtPris.Rab2%[1]     = 0
                bufArtPris.Rab3%[1]     = 0
                bufArtPris.Mva%[1]      = Moms.MomsProc
                bufArtPris.InnkjopsPris[1] = pris.engrosn
                bufArtPris.VareKost[1]  = pris.engrosn
                bufArtPris.Mvakr[1]     = IF bufArtPris.Mva%[1] = 0 THEN 0 ELSE ROUND(bufArtPris.Pris[1] * bufArtPris.Mva%[1] / (100 + bufArtPris.Mva%[1]),2)
                bufArtPris.dbkr[1]      = bufArtPris.pris[1] - bufArtPris.mvakr[1] - bufArtPris.Varekost[1]
                bufArtPris.db%[1]       = ROUND(bufArtPris.dbkr[1] / (bufArtPris.pris[1] - bufArtPris.mvakr[1]) * 100,2)
                .
        END. /* INNKJOPSPRIS */

        /* Korr av utpris */
        IF Pris.UtPrisN <> ArtPris.Pris[1] THEN
        UTPRIS:
        DO:
            ASSIGN
                bufArtPris.Mva%[1]  = Moms.MomsProc
                bufArtPris.Pris[1]  = Pris.UtprisN
                bufArtPris.Mvakr[1] = IF bufArtPris.Mva%[1] = 0 THEN 0 ELSE ROUND(bufArtPris.Pris[1] * bufArtPris.Mva%[1] / (100 + bufArtPris.Mva%[1]),2)
                bufArtPris.dbkr[1]  = bufArtPris.pris[1] - bufArtPris.mvakr[1] - bufArtPris.Varekost[1]
                bufArtPris.db%[1]   = ROUND(bufArtPris.dbkr[1] / (bufArtPris.pris[1] - bufArtPris.mvakr[1]) * 100,2)
                .
                DISPLAY
                    bufArtPris.Pris[1]
                    WITH WIDTH 300.

        END. /* UTPRIS */
        
        DISPLAY
            bufArtPris.ProfilNr
            bufArtPris.VareKost[1]
            Pris.EngrosN <> ArtPris.VareKost[1]
            Pris.UtPrisN <> ArtPris.Pris[1]
            WITH WIDTH 300.

    END. /* KORREKSJON */
    
END.
