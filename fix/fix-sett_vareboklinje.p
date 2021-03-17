
DEF VAR hvlb AS HANDLE NO-UNDO.

FOR EACH VareBokLinje WHERE VarebokLinje.VareBokNr = 10000001:
    FIND ArtBas OF VareBokLinje.
    FIND FIRST ArtPris OF ArtBas.
  ASSIGN
      hvlb = BUFFER VareBokLinje:handle
      VarebokLinje.InnkjopsPris = ArtPris.InnkjopsPris[1]
      VarebokLinje.Varekost     = ArtPris.Varekost[1]
      VarebokLinje.DB%          = ArtPris.Db%[1]
      VarebokLinje.supVarekost  = ArtPris.Rab1%[1]
      VarebokLinje.Pris         = ArtPris.Pris[1]
      VarebokLinje.KampanjePris = ArtPris.Pris[1]
      .
      
    run vareboklinje_kalkuler.p (hvlb,"Varekost").
    run vareboklinje_kalkuler.p (hvlb,"supVarekost").
END.
