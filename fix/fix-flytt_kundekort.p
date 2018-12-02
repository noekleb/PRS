current-window:width = 250.
def var lKundeNR as dec no-undo.

def buffer bufKunde for Kunde.

assign
lKundeNr = 12600978.
find bufKunde where bufKunde.KundeNR = lKundeNr.

for each KundeKort where
  int(KundeKort.KortNr) >= 251 and
  int(kundeKort.KortNr) <= 500,
  each MedlemsKort where
  Medlemskort.InterntKKortId = KundeKort.InterntKKortId,
  each Medlem of MEdlemsKort:
  
  display
  bufKunde.Navn
  KundeKort.KundENr
  KundEKort.KortNr
  KundEKort.Innehaver
  KundeKort.InterntKKortId
  MEdlemSkort.InterntKKortId
  MedlemsKort.MedlemsNr
  MedlemsKOrt.Innehaver
  Medlem.KundeNr
  '*' when Medlem.KundeNr <> KundeKort.KundeNr
  with width 250.
  
  assign
  KundeKort.Kundenr = lKundeNR
  Medlem.KundeNr = lKundeNr
  .
end.
