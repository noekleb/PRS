DEFINE BUFFER bArtPris FOR ArtPris.

FOR EACH ArtPris WHERE 
    ArtPris.ProfilNr = 100:
    FIND ArtBas OF ArtPris EXCLUSIVE-LOCK.
    
  ArtBas.EDato = TODAY.
  ArtBas.ETid  = TIME.
  
  IF ArtBas.KjedeInnkPris <= 0 THEN
    DO:
      FIND bArtPris NO-LOCK WHERE 
        bArtPris.ArtikkelNr = ArtPris.ArtikkelNr AND 
        bArtPris.ProfilNr = 1.
      ASSIGN 
        ArtBas.KjedeInnkPris = ROUND((bArtPris.InnkjopsPris[1] * 45) / 100,0)
        .
    END.     
  ASSIGN             
    ArtPris.InnkjopsPris[1] = ArtBas.KjedeInnkPris
    Artpris.ValPris[1]      = ArtBas.KjedeInnkPris
    ArtPris.Rab1Kr[1]       = 0
    ArtPris.Rab1Kr[1]       = 0
    ArtPris.VareKost[1]     = 0
    ArtPris.MvaKr[1]        = ArtPris.Pris[1] - ROUND((ArtPris.Pris[1] / (1 + (25 / 100))),2)
    ArtPris.DbKr[1]         = ArtPris.Pris[1] - ArtPris.MvaKr[1] - ArtPris.VareKost[1]
    ArtPris.Db%[1]          = ROUND(
                                    (ArtPris.DbKr[1] * 100) / (ArtPris.Pris[1] - ArtPris.MvaKr[1]),2) 
    ArtPris.Db%[1]          = IF ArtPris.Db%[1] = ? THEN 0 ELSE ArtPris.Db%[1]
    .

            
            
    DISPLAY
        ArtPris.ArtikkelNr
        ArtBas.LevKod
        ArtPris.InnkjopsPris[1]
        ArtBas.KjedeInnkPris
        .

END.
