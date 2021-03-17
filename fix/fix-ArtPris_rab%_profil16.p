DEF BUFFER ArtPris FOR ArtPris.

CURRENT-WINDOW:WIDTH = 350.

FOR EACH ArtPris EXCLUSIVE-LOCK WHERE 
    ArtPris.ProfilNr = 16 AND 
    ArtPris.Tilbud = TRUE,
    FIRST ArtBas OF ArtPris NO-LOCK:
    

      ASSIGN
            ArtPris.Rab1%[2]        = 40
            ArtPris.Rab1Kr[2]       = ROUND((ArtPris.InnkjopsPris[2] * ArtPris.Rab1%[2])/ 100,2) 
            ArtPris.Rab1Kr[1]       = IF ArtPris.Rab1Kr[2] = ? THEN 0 ELSE ArtPris.Rab1Kr[2] 
            ArtPris.Pris[2]         = ROUND(ArtPris.Pris[1] - ((ArtPris.Pris[1] * ArtPris.Rab1%[2])/ 100),0) 
            ArtPris.MvaKr[2]        = ArtPris.Pris[2] - (ArtPris.Pris[2] / (1 + (ArtPris.Mva%[2] / 100)))
            ArtPris.DbKr[2]         = ArtPris.Pris[2] - ArtPris.MvaKr[2] - ArtPris.VareKost[2]
            ArtPris.DB%[1]          = ROUND((ArtPris.DbKr[2] * 100) / (ArtPris.VareKost[2] + ArtPris.DbKr[2]),2)
            ArtPris.DB%[1]          = IF ArtPris.DB%[2] = ? THEN 0 ELSE ArtPris.DB%[2]
        .

      DISPLAY
          ArtBas.ArtikkelNr
          ArtBas.Beskr
          ArtBas.LevKod
          ArtPris.Rab1%[2]
          ArtPris.Pris[2]
      WITH WIDTH 350.
END.
