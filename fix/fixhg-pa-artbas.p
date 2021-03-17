for each artbas:
  find VarGr of ArtBas no-lock.
  
  if ArtBas.Hg <> VarGr.Hg then 
    do:
      /*
      pause 0.
      display artbas.vg artbas.lopnr artbas.hg vargr.hg.
      */
      ArtBas.Hg = VarGr.Hg.
    end.

end.
