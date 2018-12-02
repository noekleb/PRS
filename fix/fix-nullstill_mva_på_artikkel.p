
/* Nullstille mva på en artikkel. */

    
FOR EACH ArtPris WHERE 
    ArtPris.ArtikkelNr =  58980:
     
  /* Utregning av ny kalkyle */
  ASSIGN
    ArtPris.MvaKr[1] = 0
    ArtPris.Mva%[1]  = 0
    ArtPris.DbKr[1]  = ArtPris.Pris[1] - ArtPris.MvaKr[1] - ArtPris.Varekost[1]
    ArtPris.Db%[1]   = ROUND((ArtPris.DbKr[1] * 100)/ (ArtPris.Pris[1] - ArtPris.MvaKr[1]),2)
    ArtPris.Db%[1]   = IF ArtPris.Db%[1] = ? THEN 0 ELSE ArtPris.Db%[1]
    .

end.
