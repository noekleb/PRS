def buffer bufTelleLinje  for TelleLinje.

current-window:width = 300.

for each Tellelinje exclusive-lock where
  TelleLinje.TelleNr = 27 and
  can-find(bufTelleLinje where 
           bufTelleLinje.TelleNr = 12 and
           bufTelleLinje.ArtikkelNr = TelleLinje.ArtikkelNr and
           bufTelleLinje.Butik      = TelleLinje.Butik and
           bufTelleLinje.Storl      = TelleLinje.Storl):
  /*         
  display
    TelleLinje.TelleNr
    TelleLinje.ArtikkelNr
    TelleLinje.Butik
    TelleLinje.Storl
    TelleLinje.AntallTalt  
  with width 300.
  */
  delete TelleLinje.
end.  
