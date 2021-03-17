/* Setter korrekt hg på varegruppene. */

for each vargr:

  display vargr.vg vargr.vgBeskr vargr.Hg.   
  
  if Vargr.vg >= 1 and
     VarGr.Vg <= 19 then VarGr.Hg = 1.

  else if Vargr.vg >= 20 and
     VarGr.Vg <= 29 then VarGr.Hg = 3.

  else if Vargr.vg >= 30 and
     VarGr.Vg <= 49 then VarGr.Hg = 2.

  else if Vargr.vg >= 50 and
     VarGr.Vg <= 69 then VarGr.Hg = 4.
  
  else 
    VarGr.Hg = 5.
end.
