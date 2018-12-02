
def var wCl       as int no-undo.
def var wVareKost as dec no-undo.

current-window:width = 180.

assign wCl = 20.

find butiker no-lock where
  Butiker.Butik = wCl no-error.

ARTBAS:
for each ArtBas no-lock where
  ArtBas.Vg >= 1 and
  ArtBas.Vg <= 999 and
  ArtBas.LopNr >= 1000 and
  ArtBas.LopNr <= 2999:
  /*
  ArtBas.Vg = 50 and ArtBas.LopNr = 1002:
  */
  
  
  find ArtPris no-lock where
    ArtPris.ArtikkelNr = ArtBas.ArtikkelNr and
    ArtPris.ProfilNr = Butiker.ProfilNr no-error.
  if not available ArtPris then
    next ARTBAS.
  
  assign
    wVareKost = ArtPris.VareKost[if ArtPris.Tilbud then 2 else 1].
    
    
  if wVareKost = 0 then
    display 
      ArtBAs.Vg
      ArtBAs.LopNr 
      wVareKost
    with width 178.
  /*  
  for each LAger of ArtBas no-lock:
    display 
      ArtBas.Vg
      ArtBas.LopNr
      wVareKost
      Lager.Butik
      LAger.vVareKost
      Lager.LAgAnt
    with width 178.
  end.
  */
    
end.  /* ARTBAS */
