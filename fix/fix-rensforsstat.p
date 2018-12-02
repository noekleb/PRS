def  var i as int.
for each stlinje.
  assign    StLinje.AntSolgt      = 0
            StLinje.VerdiSolgt    = 0
            StLinje.VVareKost     = 0
            StLinje.MvaVerdi      = 0
            StLinje.AntRab        = 0
            StLinje.VerdiRabatt   = 0
            StLinje.GjenkjopAnt   = 0
            StLinje.GjenkjopVerdi = 0. 
  i = i + 1.
  if i modulo 1000 = 0 then display i.
  pause 0.

end.

