
def var wCl       as int no-undo.
def var wVareKost as dec no-undo.
def var wSvar as log.

message "Korreksjon av varekost i lager" skip
        "Skal rutinen starte?" view-as alert-box buttons yes-no
        update wSvar.
if wSvar <> true then
  return.        

assign wCl = 2.

find butiker no-lock where
  Butiker.Butik = wCl no-error.

ARTBAS:
for each ArtBas no-lock where
  ArtBas.Vg >= 1 and
  ArtBas.Vg <= 999 and
  ArtBas.LopNr >= 1 and
  ArtBas.LopNr <= 9999:
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
    
  for each Lager of ArtBas:
    pause 0.
    
    assign Lager.VVareKost = wVAreKost.
    
    display 
      ArtBas.Vg
    with frame gurre.
  end.
end.  /* ARTBAS */
