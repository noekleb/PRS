for each VarGr no-lock:
  find first VgKat no-lock where
    VgKat.Vg    = VarGr.Vg and
    VgKAt.VgKAt = 1 no-error.
  if not available VgKat then
    do:
      create VgKAt.
      assign
        VgKat.Vg = VarGr.Vg
        VgKAt.VgKAt = 1 
        VgKAt.KatNr = 1.
    end.
  
end.
