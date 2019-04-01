DEF VAR lPris AS DEC NO-UNDO.
DEF VAR lPris2 AS DEC NO-UNDO.

CURRENT-WINDOW:WIDTH = 350.

DEF BUFFER bufArtPris FOR ArtPris.

ON write OF artbas OVERRIDE DO: END.

FOR EACH ArtPris WHERE 
    ArtPris.ProfilNr = 1 AND ArtPris.InnkjopsPris[1] = 0:
    
    FIND bufArtPris NO-LOCK WHERE 
        bufArtPris.ArtikkelNr = ArtPris.ArtikkelNr AND
        bufArtPris.ProfilNr   = 2 NO-ERROR.
    FIND ArtBas OF ArtPris NO-LOCK NO-ERROR.
    
    IF AVAILABLE bufArtPris AND 
        ArtPris.Pris[1] > 0 AND 
        bufArtPris.InnkjopsPris[1] <> ? AND 
        bufArtPris.InnkjopsPris[1] > 0 THEN
    DO:
        lPris = bufArtPris.Pris[1] / 0.7.
        lPris2 = ROUND(lPris,0).
        IF lPris <> lPris2 THEN
            lPris = bufArtPris.Pris[1] / 0.6.

        ASSIGN 
            ArtPris.Pris[1]         = lPris
            ArtPris.MvaKr[1]        =  ArtPris.Pris[1] - ArtPris.Pris[1] / (1 + ArtPris.Mva%[1] / 100)
            ArtPris.ValPris[1]      = bufArtPris.InnkjopsPris[1]
            ArtPris.InnkjopsPris[1] = bufArtPris.InnkjopsPris[1]
            ArtPris.Rab1%[1]        = 10
            ArtPris.Rab1Kr[1]       =  ArtPris.InnkjopsPris[1] * ArtPris.Rab1%[1] / 100
            ArtPris.VareKost[1]     =  ArtPris.InnkjopsPris[1] - ArtPris.Rab1Kr[1]
            ArtPris.DbKr[1]         =  ArtPris.Pris[1] - ArtPris.MvaKr[1] - ArtPris.VareKost[1]
            ArtPris.Db%[1]          =  round(ArtPris.DbKr[1] * 100 / (ArtPris.Pris[1] - ArtPris.MvaKr[1]),2)
            .

        DISPLAY
            ArtPris.ProfilNr
            ArtPris.ArtikkelNr
            ArtPris.RegistrertDato
            ArtBas.LevKod
            ArtBas.LevFargKod

            '|'
            ArtPris.ValPris[1]
            ArtPris.InnkjopsPris[1]
            ArtPris.VareKost[1]
            ArtPris.Rab1%[1]
            ArtPris.Pris[1]
            lPris
            '|'
            bufArtPris.ProfilNr WHEN AVAILABLE bufArtPris
            bufArtPris.ValPris[1] WHEN AVAILABLE bufArtPris
            bufArtPris.InnkjopsPris[1] WHEN AVAILABLE bufArtPris
            bufArtPris.Varekost[1] WHEN AVAILABLE bufArtPris
            bufArtPris.Rab1%[1] WHEN AVAILABLE bufArtPris
            bufArtPris.Pris[1] WHEN AVAILABLE bufArtPris
        WITH WIDTH 350.
        /*
        BUFFER-COPY bufArtPris 
            EXCEPT ProfilNr
            TO ArtPris
            .
        */
    END.
END.
