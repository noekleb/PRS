def var wVareBokNr like varebokhode.VareBokNr no-undo.
def var cDato as char no-undo.
def var cTekst as char no-undo.

assign
  wVareBokNr = 90000080
  cTekst     = string(Today,"99/99/9999")
  cDato      = string(wVareBokNr) + "-"
  cDato      = cDato + entry(1,cTekst,'/') + entry(2,cTekst,'/') + entry(3,cTekst,'/')
  cDato      = cDato + "-" + string(time)
  .

message wVareBokNr skip
        cDato
        view-as alert-box.

output to value("varebokHode" + string(cDato) +  ".d") no-echo.
for each VareBokHode no-lock where
  VareBokHode.VareBokNr = wVareBokNr:
  export VareBokHode.
end.
output close.

output to value("varebokLinje" + string(cDato) + ".d") no-echo.
for each VareBokLinje no-lock where
  VareBokLinje.VareBokNr = wVareBokNr:
  export VareBokLinje.
end.
output close.
