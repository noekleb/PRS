current-window:width = 320.
def var x as int no-undo.

for each ArtBas where ArtBas.EDato = today - 2:
  
  find first ArtPris of ArtBas.
  /*
  display
  ArtBas.ArtikkelNr
  ArtBas.LevKod
  ArtBas.Beskr
  ArtBas.LevFargKod
  ArtPRis.EDato
  ArtPris.Tilbud
  ArtPris.ValPris[1]
  ArtPris.InnkjopsPris[1]
  ArtPris.Rab1Kr[1]
  ArtPris.Rab1%[1]
  ArtPris.Rab2Kr[1]
  ArtPris.Rab2%[1]
  ArtPris.Rab3Kr[1]
  ArtPris.Rab3%[1]
  ArtPris.Frakt[1]
  ArtPris.Frakt%[1]
  ArtPris.DivKostKr[1]
  ArtPris.DivKost%[1]
  ArtPris.Varekost[1]
  ArtPris.DbKr[1]
  ArtPris.Db%[1]
  ArtPris.MvaKr[1]
  ArtPris.Mva%[1]
  ArtPris.Pris[1]
  ArtPris.EuroPris[1]
  with width 320.
  */
  for first HPrisKo of ArtPris where
    HPrisKo.EDato < ArtBas.EDato:
    /*
    display 
      HPrisKo.ArtikkelNR
      HPrisKo.ValPris
      HPrisKo.Innkjopspris
      HPrisKo.Rab1Kr
      HPrisKo.Rab1%
      HPrisKo.Rab2Kr
      HPrisKo.Rab2%
      HPrisKo.Rab3Kr
      HPrisKo.Rab3%
      HPrisKo.Frakt
      HPrisKo.Frakt%
      HPrisKo.DivKostKr
      HPrisKo.DivKost%
      HPrisKo.VareKost
      HPrisKo.DbKr
      HPrisKo.Db%
      HPrisKo.MvaKr
      HPrisKo.Mva%
      HPrisKo.Pris
      HPrisKo.EuroPris
      HPrisKo.EDato
      with width 320. 
      */
      assign
        ArtPris.ValPris[1]      = HPrisKo.ValPris
        ArtPris.InnkjopsPris[1] = HPrisKo.InnkjopsPris
        ArtPris.Rab1Kr[1]       = HPrisko.Rab1Kr
        ArtPris.Rab1%[1]        = HPrisKo.Rab1%
        ArtPris.Rab2Kr[1]       = HPrisKo.Rab2Kr
        ArtPris.Rab2%[1]        = HPrisKo.Rab2%
        ArtPris.Rab3Kr[1]       = HPrisKo.Rab3Kr
        ArtPris.Rab3%[1]        = HPrisko.Rab3%
        ArtPris.Frakt[1]        = HPrisKo.Frakt
        ArtPris.Frakt%[1]       = HPrisKo.Frakt%
        ArtPris.DivKostKr[1]    = HPrisKo.DivKostKr
        ArtPris.DivKost%[1]     = HPrisKo.DivKost%
        ArtPris.Varekost[1]     = HPrisKo.VareKost
        ArtPris.DbKr[1]         = HPrisKo.DbKr
        ArtPris.Db%[1]          = HPrisKo.Db%
        ArtPris.MvaKr[1]        = HPrisKo.MvaKr
        ArtPris.Mva%[1]         = HPrisKo.Mva%
        ArtPris.Pris[1]         = HPrisKo.Pris
        ArtPris.EuroPris[1]     = HPrisKo.EuroPris
      .     
  end.

  x = x + 1.
end.
display x.
