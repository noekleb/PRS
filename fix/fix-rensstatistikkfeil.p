/*
ARTIKKEL
AVDELING
BUTSTAT
HOVEDGR
VAREGR
KUNDSTAT
LEVERAN
MEDLEM
MEDLEMTOT
SELGER
SELGERSTAT
*/

for each StLinje where
StLinje.StType = "SELGERSTAT":
  if not can-find(Forsalj where
  Forsalj.ForsNr = int(StLinje.DataObjekt)) or
  int(StLinje.DataObjekt) = 0 then
  do:
    pause 0.
    display
    StLinje.StType
    StLinje.DataObjekt
    .
    delete StLinje.
  end.
end.


for each StLinje where
StLinje.StType = "SELGER":
  if not can-find(Selger where
  Selger.SelgerNr = int(StLinje.DataObjekt)) or
  int(StLinje.DataObjekt) = 0 then
  do:
    pause 0.
    display
    StLinje.StType
    StLinje.DataObjekt
    .
    delete StLinje.
  end.
end.


for each StLinje where
StLinje.StType = "MEDLEMTOT":
  if not can-find(Medlem where
  Medlem.MedlemsNr = int(StLinje.DataObjekt)) then
  do:
    pause 0.
    display
    StLinje.StType
    StLinje.DataObjekt
    .
    delete StLinje.
  end.
end.


for each StLinje where
StLinje.StType = "MEDLEM":
  if not can-find(Medlem where
  Medlem.MedlemsNr = int(StLinje.DataObjekt)) then
  do:
    pause 0.
    display
    StLinje.StType
    StLinje.DataObjekt
    .
    delete StLinje.
  end.
end.


for each StLinje where
StLinje.StType = "LEVERAN":
  if not can-find(LevBas where
  LevBas.LevNr = int(StLinje.DataObjekt)) then
  do:
    pause 0.
    display
    StLinje.StType
    StLinje.DataObjekt
    .
    delete StLinje.
  end.
end.


for each StLinje where
StLinje.StType = "KUNDSTAT":
  if not can-find(Kunde where
  Kunde.KundeNr = int(StLinje.DataObjekt)) then
  do:
    pause 0.
    display
    StLinje.StType
    StLinje.DataObjekt
    .
    delete StLinje.
  end.
end.


for each StLinje where
StLinje.StType = "VAREGR":
  if not can-find(VarGr where
  VarGr.VG = int(StLinje.DataObjekt)) then
  do:
    pause 0.
    display
    StLinje.StType
    StLinje.DataObjekt
    .
    delete StLinje.
  end.
end.

for each StLinje where
StLinje.StType = "HOVEDGR":
  if not can-find(HuvGr where
  HuvGr.Hg = int(StLinje.DataObjekt)) then
  do:
    pause 0.
    display
    StLinje.StType
    StLinje.DataObjekt
    .
    delete StLinje.
  end.
end.
for each StLinje where
StLinje.StType = "ARTIKKEL":
  if not can-find(ArtBas where
  ArtBas.ArtikkelNr = dec(StLinje.DataObjekt)) then
  do:
    pause 0.
    display
    StLinje.StType
    StLinje.DataObjekt
    .
    delete StLinje.
  end.
end.
for each StLinje where
StLinje.StType = "AVDELING":
  if not can-find(Avdeling where
  Avdeling.AvdelingNr = int(StLinje.DataObjekt)) then
  do:
    pause 0.
    display
    StLinje.StType
    StLinje.DataObjekt
    .
    delete StLinje.
  end.
end.
for each StLinje where
StLinje.StType = "BUTSTAT":
  if not can-find(Butiker where
  Butiker.Butik = int(StLinje.DataObjekt)) then
  do:
    pause 0.
    display
    StLinje.StType
    StLinje.DataObjekt
    .
    delete StLinje.
  end.
end.
