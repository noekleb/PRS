def var wLoop as int no-undo.

form
  "Antall behandlede poster:" wLoop skip
  "Behandler artikkel      :" ArtBAs.Vg space(0) "/" space(0) ArtBAs.LopNr 
                              "(" space(0) ArtBAs.ArtikkelNr space(0) ")" skip
with frame Gurre overlay centered row 4 no-labels.

for each ArtBas no-lock:
  
  for each ArtLAg exclusive-lock where
    ArtLAg.Vg    = ArtBas.Vg and
    ArtLAg.LopNr = ArtBas.LopNr:
    assign
      wLoop = wLoop + 1
      ArtLag.ArtikkelNr = ArtBas.ArtikkelNr.
  end.
  pause 0 before-hide.
  display
    wLoop
    ArtBAs.Vg
    ArtBAs.LopNr 
    ArtBAs.ArtikkelNr
  with frame Gurre.

end.

pause.
