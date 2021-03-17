CURRENT-WINDOW:WIDTH = 300.

DEF BUFFER bArtPris FOR ArtPris.

FOR EACH TelleHode NO-LOCK WHERE
    TelleHode.ButikkListe = '170':

    FOR EACH TelleLinje NO-LOCK OF TelleHode:
        
        FIND ArtBAs NO-LOCK WHERE
            ArtBas.ArtikkelNr = TelleLinje.ArtikkelNr NO-ERROR.
        FIND FIRST ArtPris OF ArtBas NO-LOCK NO-ERROR.


        FIND bArtPris EXCLUSIVE-LOCK WHERE
            bArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND 
            bArtPris.ProfilNr   = 170 NO-ERROR.
        IF NOT AVAILABLE bArtPris THEN
        DO:
            CREATE bArtPris.
            BUFFER-COPY ArtPris 
                EXCEPT ProfilNr
                TO bArtPris
                ASSIGN
                    bArtPris.ProfilNr = 170.
        END.

        ASSIGN
            bArtPris.InnkjopsPris[1] = ArtPris.InnkjopsPris[1] - ROUND(((ArtPris.InnkjopsPris[1] * 10) / 100),2) 
            bArtPris.ValPris[1]      = bArtPris.InnkjopsPris[1]
            bArtPris.Rab1Kr[1]       = ROUND(((bArtPris.InnkjopsPris[1] * bArtPris.Rab1%[1]) / 100),2)
            bArtPris.VareKost[1]     = bArtPris.InnkjopsPris[1] - bArtPris.Rab1Kr[1]
            bArtPris.DBKr[1]         = bArtPris.Pris[1] - bArtPris.MvaKr[1] - bArtPris.VareKost[1]
            bArtPris.Db%[1]          = ROUND(bArtpris.dbkr[1] / (bArtpris.pris[1] - bArtpris.mvakr[1]) * 100,2)
            .

        DISPLAY
            TelleHode.TelleNr
            TelleHode.ButikkListe FORMAT "x(5)"
            ArtBas.ArtikkelNr
            ArtPris.ProfilNr
            bArtPris.ProfilNr
            ArtPris.InnkjopsPris[1]
            bArtPris.InnkjopsPris[1]
            bArtPris.Rab1Kr[1]
            bArtPris.Rab1%[1]
            bArtPris.VareKost[1]
            bArtPris.DbKr[1]
            bArtPris.Db%[1]
            bArtPris.Pris[1]
            
            WITH WIDTH 300.
    END.
   

END.
