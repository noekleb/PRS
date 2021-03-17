DEFINE VARIABLE lPrisRab%   AS DECIMAL   NO-UNDO.
DEFINE VARIABLE lforhRab%   AS DECIMAL   NO-UNDO.
DEFINE VARIABLE cEDB-System AS CHARACTER INITIAL 'Gant Global' NO-UNDO.
DEFINE VARIABLE fMvaKr      AS DECIMAL   NO-UNDO.
DEFINE VARIABLE fDbKr       AS DECIMAL   NO-UNDO.
DEF VAR iAnt AS INT NO-UNDO.

DEF BUFFER bufArtPris FOR ArtPris.
DEF BUFFER buf2ArtPris FOR ArtPris.

CURRENT-WINDOW:WIDTH = 350.

FIX:
FOR EACH ArtPris WHERE ProfilNr = 16 AND 
    (ArtPris.Pris[1] = ? OR ArtPris.Pris[1] = 0):

    FIND bufArtPris NO-LOCK WHERE 
        bufArtPris.ArtikkelNr = ArtPris.ArtikkelNr AND 
        bufArtPris.ProfilNr = 1 NO-ERROR.

    IF AVAILABLE bufArtPris AND 
        bufArtPris.Pris[1] > 0 THEN
    DO:
        /* ---------------- Her hentes Outlet'ens rabatter ----------------------*/
        FIND FIRST ImpKonv NO-LOCK WHERE 
              ImpKonv.EDB-System = cEDB-System AND 
              ImpKonv.Tabell     = 'Def.Rab%' AND 
              ImpKonv.EksterntId = '16' NO-ERROR.
        IF AVAILABLE ImpKonv 
              THEN ASSIGN 
                  lforhRab%      = DEC(ImpKonv.Merknad)
                  lPrisRab%      = DEC(ImpKonv.InterntId).
        
        /* Legger over Outlet kalkyle */ 
        BUFFER-COPY bufArtPris
            EXCEPT ProfilNr Tilbud
            TO ArtPris.

        /*---------------------------------
          ASSIGN 
            ArtPris.Pris[1]         = ROUND(ArtPris.Pris[1] - (ArtPris.Pris[1] * lPrisRab% / 100),2) 
            ArtPris.InnkjopsPris[1] = ArtPris.InnkjopsPris[1]
            ArtPris.Rab1%[1]        = lforhRab%
            ArtPris.Varekost[1]     = ROUND(ArtPris.InnkjopsPris[1] - (ArtPris.InnkjopsPris[1] * lforhRab% / 100),2)
            fMvaKr                  = ArtPris.Pris[1] - (ArtPris.Pris[1] / (1 + (ArtPris.Mva%[1] / 100)))
            fDbKr                   = ArtPris.Pris[1] - fMvaKr - ArtPris.Varekost[1]                   
            ArtPris.Db%[1]          = ROUND((fDbKr * 100) / (ArtPris.Pris[1] - fMvaKr),2)
            ArtPris.Db%[1]          = IF ArtPris.Db%[1] = ? THEN 0 ELSE ArtPris.Db%[1]
            .
        ----------------------------------*/
        
        DISPLAY
            lforhRab%
            lPrisRab%
            '|'
            ArtPris.ArtikkelNr
            ArtPris.ProfilNr
            ArtPris.Pris[1]
            ArtPris.VareKost[1]
            ArtPris.EDato
            WITH WIDTH 350.
        IF AVAILABLE bufArtPris THEN
        DISPLAY
            bufArtPris.ArtikkelNr
            bufArtPris.ProfilNr
            bufArtPris.Pris[1]
            bufArtPris.VareKost[1]
            bufArtPris.EDato
            WITH WIDTH 350.
        iAnt = iAnt + 1.
    END.

    IF iAnt > 100000 THEN
        LEAVE fix.
END.
