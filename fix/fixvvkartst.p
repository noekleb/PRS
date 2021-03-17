def var x as int no-undo.

for each StLinje where
  StLinje.StType = "ARTIKKEL":
  
  find Lager no-lock where
    Lager.ArtikkelNr = int(StLinje.DataObjekt) and
    LAger.Butik      = StLinje.Butik no-error.
  if not available LAger then
    next.
    
  assign
    x = x + 1
    StLinje.VVareKost = Lager.VVareKost * StLinje.AntSolgt. 
  
  pause 0.
  display 
    
    StLinje.DataObjekt
    StLinje.AntSolgt
    StLinje.VVareKost
    (StLinje.VVareKost / StLinje.AntSolgt)
    Lager.VVareKost
  with frame G.
end.  
