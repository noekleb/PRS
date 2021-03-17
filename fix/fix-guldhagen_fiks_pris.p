CURRENT-WINDOW:WIDTH = 300.

DEF VAR iButNr AS INT NO-UNDO.
DEF VAR iProfNr AS INT NO-UNDO.
DEF VAR iLokProfilNr AS INT NO-UNDO.

DEF BUFFER bufArtPris FOR ArtPris.

ASSIGN
    iButNr       = 2
    iProfNr      = 2
    iLokProfilNr = 1
    .

FOR EACH ArtPris EXCLUSIVE-LOCK WHERE
    /*ArtPris.ArtikkelNr = 2454 AND*/  
    ArtPris.ProfilNr = iLokProfilNr AND 
    ArtPris.EDAto = ?,
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
            Pris.Rab1N WHEN AVAILABLE Pris
            Pris.Rab2N WHEN AVAILABLE Pris
            Pris.Rab3N WHEN AVAILABLE Pris
        WITH WIDTH 300.

        /* Korr av innkjøpspris */
        IF Pris.EngrosN <> ArtPris.VareKost[1] THEN
        INNKJOPSPRIS:
        DO:
            ASSIGN
                ArtPris.ValPris[1]   = pris.engrosn
                ArtPris.Rab1Kr[1]    = 0
                ArtPris.Rab2Kr[1]    = 0
                ArtPris.Rab3Kr[1]    = 0
                ArtPris.Rab1%[1]     = 0
                ArtPris.Rab2%[1]     = 0
                ArtPris.Rab3%[1]     = 0
                ArtPris.Mva%[1]      = Moms.MomsProc
                ArtPris.InnkjopsPris[1] = pris.engrosn
                ArtPris.VareKost[1]  = pris.engrosn
                Artpris.Mvakr[1]     = IF ArtPris.Mva%[1] = 0 THEN 0 ELSE ROUND(ArtPris.Pris[1] * ArtPris.Mva%[1] / (100 + ArtPris.Mva%[1]),2)
                Artpris.dbkr[1]      = Artpris.pris[1] - Artpris.mvakr[1] - Artpris.Varekost[1]
                Artpris.db%[1]       = ROUND(Artpris.dbkr[1] / (Artpris.pris[1] - Artpris.mvakr[1]) * 100,2)
                .
        END. /* INNKJOPSPRIS */

        /* Korr av utpris */
        IF Pris.UtPrisN <> ArtPris.Pris[1] THEN
        UTPRIS:
        DO:
            ASSIGN
                ArtPris.Mva%[1]  = Moms.MomsProc
                Artpris.Pris[1]  = Pris.UtprisN
                Artpris.Mvakr[1] = IF ArtPris.Mva%[1] = 0 THEN 0 ELSE ROUND(ArtPris.Pris[1] * ArtPris.Mva%[1] / (100 + ArtPris.Mva%[1]),2)
                Artpris.dbkr[1]  = Artpris.pris[1] - Artpris.mvakr[1] - Artpris.Varekost[1]
                Artpris.db%[1]   = ROUND(Artpris.dbkr[1] / (Artpris.pris[1] - Artpris.mvakr[1]) * 100,2)
                .

        END. /* UTPRIS */
        
    END. /* KORREKSJON */
    
END.
