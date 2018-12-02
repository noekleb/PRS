DEF VAR lEuKurs AS DEC NO-UNDO.
current-window:width = 300.

DEF VAR iAnt AS INT NO-UNDO.

{syspara.i 2 1 1 lEuKurs DEC}
IF lEuKurs = 0 THEN lEuKurs = 1.

LOOPEN:
for each ArtPris where 
    ArtPris.Innkjopspris[1] = 0 and ArtPris.ValPris[1] > 0:

  find ArtBas of ArtPris.
  IF ArtBas.Opris THEN NEXT LOOPEN.

  find lager EXCLUSIVE-LOCK where
  Lager.ArtikkelNr = ArtBas.ArtikkelNr and
  Lager.Butik = 550 no-error.


  FIND Valuta NO-LOCK WHERE
      Valuta.ValKod = ArtBAs.ValKod NO-ERROR.

  iAnt = iAnt + 1.
  IF iant > 5 THEN
      LEAVE LOOPEN.

  ASSIGN
      ArtPris.InnkjopsPris[1] = ArtPris.ValPris[1] * Valuta.ValKurs
      ArtPris.Rab1Kr          = ROUND((ArtPris.InnkjopsPris[1] * ArtPris.Rab1%[1])/ 100,2) 
      ArtPris.Rab1Kr          = if ArtPris.Rab1Kr = ? then 0 else ArtPris.Rab1Kr 
      ArtPris.VareKost[1]     = ArtPris.InnkjopsPris[1] - ArtPris.Rab1Kr[1]
      ArtPris.MvaKr[1]        = ArtPris.Pris[1] - (ArtPris.Pris[1] / (1 + (ArtPris.Mva%[1] / 100)))
      ArtPris.DbKr[1]         = ArtPris.Pris[1] - ArtPris.MvaKr[1] - ArtPris.VareKost[1]
      ArtPris.DB%[1]          = ROUND((ArtPris.DbKr[1] * 100) / (ArtPris.VareKost[1] + ArtPris.DbKr[1]),2)
      ArtPris.DB%[1]          = IF ArtPris.DB%[1] = ? THEN 0 ELSE ArtPris.DB%[1]
  .
  IF AVAILABLE Lager THEN 
      Lager.VVAreKost = ArtPris.VareKost[1].


  display
  ArtBAs.ArtikkelNr
  ArtBas.LevKod
  ArtBAs.Beskr
  ArtPris.ValPris[1]
  ArtPris.InnkjopsPris[1]
  ArtPris.Rab1%[1]
  ArtPris.VareKost[1]
  ArtPris.MvaKr[1]
  ArtPris.Db%[1]
  ArtPris.Pris[1]
  '|'
  Lager.VVareKost when available Lager
  with width 300.
end.
