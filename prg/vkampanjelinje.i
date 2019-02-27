  FIELD AktivFraDato LIKE ArtPris.AktivFraDato VALIDATE ~
  FIELD AktivFraTid LIKE ArtPris.AktivFraTid VALIDATE ~
  FIELD ArtikkelNr LIKE ArtPris.ArtikkelNr VALIDATE ~
  FIELD BrukerID LIKE ArtPris.BrukerID VALIDATE ~
  FIELD DB%1 LIKE ArtPris.DB%[1] VALIDATE ~
  FIELD DB%2 LIKE ArtPris.DB%[2] VALIDATE ~
  FIELD DBKr1 LIKE ArtPris.DBKr[1] VALIDATE ~
  FIELD DBKr2 LIKE ArtPris.DBKr[2] VALIDATE ~
  FIELD DivKost%1 LIKE ArtPris.DivKost%[1] VALIDATE ~
  FIELD DivKost%2 LIKE ArtPris.DivKost%[2] VALIDATE ~
  FIELD DivKostKr1 LIKE ArtPris.DivKostKr[1] VALIDATE ~
  FIELD DivKostKr2 LIKE ArtPris.DivKostKr[2] VALIDATE ~
  FIELD EDato LIKE ArtPris.EDato VALIDATE ~
  FIELD ETid LIKE ArtPris.ETid VALIDATE ~
  FIELD EuroManuel LIKE ArtPris.EuroManuel VALIDATE ~
  FIELD EuroPris1 LIKE ArtPris.EuroPris[1] VALIDATE ~
  FIELD EuroPris2 LIKE ArtPris.EuroPris[2] VALIDATE ~
  FIELD Frakt1 LIKE ArtPris.Frakt[1] VALIDATE ~
  FIELD Frakt2 LIKE ArtPris.Frakt[2] VALIDATE ~
  FIELD Frakt%1 LIKE ArtPris.Frakt%[1] VALIDATE ~
  FIELD Frakt%2 LIKE ArtPris.Frakt%[2] VALIDATE ~
  FIELD InnkjopsPris1 LIKE ArtPris.InnkjopsPris[1] VALIDATE ~
  FIELD InnkjopsPris2 LIKE ArtPris.InnkjopsPris[2] VALIDATE ~
  FIELD LevNr LIKE ArtPris.LevNr VALIDATE ~
  FIELD Mva%1 LIKE ArtPris.Mva%[1] VALIDATE ~
  FIELD Mva%2 LIKE ArtPris.Mva%[2] VALIDATE ~
  FIELD MvaKr1 LIKE ArtPris.MvaKr[1] VALIDATE ~
  FIELD MvaKr2 LIKE ArtPris.MvaKr[2] VALIDATE ~
  FIELD Pris1 LIKE ArtPris.Pris[1] VALIDATE ~
  FIELD Pris2 LIKE ArtPris.Pris[2] VALIDATE ~
  FIELD ProfilNr LIKE ArtPris.ProfilNr VALIDATE ~
  FIELD Rab1%1 LIKE ArtPris.Rab1%[1] VALIDATE ~
  FIELD Rab1%2 LIKE ArtPris.Rab1%[2] VALIDATE ~
  FIELD Rab1Kr1 LIKE ArtPris.Rab1Kr[1] VALIDATE ~
  FIELD Rab1Kr2 LIKE ArtPris.Rab1Kr[2] VALIDATE ~
  FIELD Rab2%1 LIKE ArtPris.Rab2%[1] VALIDATE ~
  FIELD Rab2%2 LIKE ArtPris.Rab2%[2] VALIDATE ~
  FIELD Rab2Kr1 LIKE ArtPris.Rab2Kr[1] VALIDATE ~
  FIELD Rab2Kr2 LIKE ArtPris.Rab2Kr[2] VALIDATE ~
  FIELD Rab3%1 LIKE ArtPris.Rab3%[1] VALIDATE ~
  FIELD Rab3%2 LIKE ArtPris.Rab3%[2] VALIDATE ~
  FIELD Rab3Kr1 LIKE ArtPris.Rab3Kr[1] VALIDATE ~
  FIELD Rab3Kr2 LIKE ArtPris.Rab3Kr[2] VALIDATE ~
  FIELD RegistrertAv LIKE ArtPris.RegistrertAv VALIDATE ~
  FIELD RegistrertDato LIKE ArtPris.RegistrertDato VALIDATE ~
  FIELD RegistrertTid LIKE ArtPris.RegistrertTid VALIDATE ~
  FIELD Tilbud LIKE ArtPris.Tilbud VALIDATE ~
  FIELD TilbudFraDato LIKE ArtPris.TilbudFraDato VALIDATE ~
  FIELD TilbudFraTid LIKE ArtPris.TilbudFraTid VALIDATE ~
  FIELD TilbudTilDato LIKE ArtPris.TilbudTilDato VALIDATE ~
  FIELD TilbudTilTid LIKE ArtPris.TilbudTilTid VALIDATE ~
  FIELD TilbudTimeStyrt LIKE ArtPris.TilbudTimeStyrt VALIDATE ~
  FIELD ValPris1 LIKE ArtPris.ValPris[1] VALIDATE ~
  FIELD ValPris2 LIKE ArtPris.ValPris[2] VALIDATE ~
  FIELD VareKost1 LIKE ArtPris.VareKost[1] VALIDATE ~
  FIELD VareKost2 LIKE ArtPris.VareKost[2] VALIDATE ~
  FIELD FuInnkjopsPris AS DECIMAL FORMAT "->,>>>,>>9.99" LABEL "Innkjøpspris"~
  FIELD FuRab1Kr AS DECIMAL FORMAT "->>,>>9.99" LABEL "Rabatt 1 (-)"~
  FIELD FuRab1% AS DECIMAL FORMAT "->>9.99" LABEL "%Rabatt 1"~
  FIELD FuRab2Kr AS DECIMAL FORMAT "->>,>>9.99" LABEL "Rabatt 2 (-)"~
  FIELD FuRab2% AS DECIMAL FORMAT "->>9.99" LABEL "%Rabatt 2"~
  FIELD FuFrakt AS DECIMAL FORMAT "->>,>>9.99" LABEL "Frakt (+)"~
  FIELD FuFrakt% AS DECIMAL FORMAT "->>,>>9.99" LABEL "Frakt%"~
  FIELD FuDivKostKr AS DECIMAL FORMAT "->>,>>9.99" LABEL "Div.kost (+)"~
  FIELD FuDivKost% AS DECIMAL FORMAT "->>,>>9.99" LABEL "Div.kost%"~
  FIELD FuRab3Kr AS DECIMAL FORMAT "->>,>>9.99" LABEL "Rabatt 3 (-)"~
  FIELD FuRab3% AS DECIMAL FORMAT "->>9.99" LABEL "%Rabatt 3"~
  FIELD FuVareKost AS DECIMAL FORMAT "->>,>>9.99" LABEL "VareKost"~
  FIELD FuDBKr AS DECIMAL FORMAT "->>,>>9.99" LABEL "Db (+)"~
  FIELD FuDB% AS DECIMAL FORMAT "->>,>>9.99" LABEL "DB%"~
  FIELD FuMvaKr AS DECIMAL FORMAT "->>,>>9.99" LABEL "Mva (+)"~
  FIELD FuMva% AS DECIMAL FORMAT "->>,>>9.99" LABEL "Mva%"~
  FIELD FuPris AS DECIMAL FORMAT "->,>>>,>>9.99" LABEL "Pris"~
  FIELD FuEuroPris AS DECIMAL FORMAT "->,>>>,>>9.99" LABEL "Pris (Euro)"~
  FIELD FuValPris AS DECIMAL FORMAT "->>>,>>>,>>9.99" LABEL "Valutapris"~
  FIELD FuHarTilbudsPris AS LOGICAL FORMAT "J/N" LABEL "HarTilbudpris"
