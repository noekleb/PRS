/*----
current-window:width = 250.
for each Varebehlinje where
  Varebehlinje.VareBehNr  = 9000003:
  
  find vargr of Varebehlinje.
  find HuvGr of vargr.
  find Avdeling of huvgr.
  
  if HuvGr.Hg <> Varebehlinje.Hg or Avdeling.AvdelingNr <> Varebehlinje.AvdelingNr
  then do:
    display
      Varebehlinje.ArtikkelNr
      Varebehlinje.Besk
      Varebehlinje.vg
      Varebehlinje.Hg
      Varebehlinje.Avdelingnr
      "*" when HuvGr.Hg <> Varebehlinje.Hg
      HuvGr.Hg
      "* " when Avdeling.AvdelingNr <> Varebehlinje.AvdelingNr
      Avdeling.Avdelingnr
    with width 250.
    
    assign
      Varebehlinje.Hg = VarGr.Hg
      Varebehlinje.AvdelingNr = Avdeling.AVdelingNr
      .
  end.

  
end.  
----*/


for each Vareboklinje where
  Vareboklinje.VareBokNr  = 9000003:
  
  find vargr of VareBokLinje.
  find HuvGr of vargr.
  find Avdeling of huvgr.
  
  if HuvGr.Hg <> VareBokLinje.Hg or Avdeling.AvdelingNr <> VarebokLinje.AvdelingNr
  then do:
    display
      VareBokLinje.ArtikkelNr
      VareBokLinje.Besk
      VarebokLinje.vg
      VareBokLinje.Hg
      VareBokLinje.Avdelingnr
      "*" when HuvGr.Hg <> VareBokLinje.Hg
      HuvGr.Hg
      "* " when Avdeling.AvdelingNr <> VarebokLinje.AvdelingNr
      Avdeling.Avdelingnr
    with width 250.
    
    assign
      VareBokLinje.Hg = VarGr.Hg
      VareBokLinje.AvdelingNr = Avdeling.AVdelingNr
      .
  end.

  
end.
  
