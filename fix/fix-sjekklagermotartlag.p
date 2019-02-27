def var wLagSum as int no-undo.
def stream err.

output stream err to value("error.svd").

for each ArtBas no-lock,
    each Lager of ArtBas no-lock:
 
  wLagSum = 0. 
  for each ArtLAg no-lock where
    ArtLag.Butik = Lager.Butik and
    ArtLAg.Vg = ArtBas.Vg and
    ArtLag.LopNr = ArtBas.LopNr:
    wLagSum = wLAgSum + ArtLag.LagAnt.
  end.
  if Lager.LagAnt <> wLagSum then
    export stream err delimiter ";" 
      ArtBas.Vg ArtBas.LopNr ArtBas.ARtikkelNr
      Lager.Lagant wLagSum.
end.
output stream err close.
