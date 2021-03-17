def var x as int.
for each vargr no-lock:
  do x = 1 to 5:
    find VgKat where
      VgKat.Vg    = varGr.Vg and
      VgKat.VgKat = x and
      vgkat.katnr = x no-error.
    if not available vgkat then
      do:
        create vgkat.
        assign 
          VgKat.Vg    = varGr.Vg 
          VgKat.VgKat = x 
          vgkat.katnr = x.
      end.
  end.
end.
