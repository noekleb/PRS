output to value("Dagsoppgjor.log").
  export "Eksport av Dagsoppgjor startet." today string(time,"HH:MM:SS").
output close.

output to value("akt_rapp.d").
for each Akt_Rapp no-lock where     
     Akt_Rapp.Dato >= 01/01/2008:
  export Akt_Rapp.
end.
output close.

output to value("dags_rap.d").
for each dags_rap no-lock where     
     dags_rap.Dato >= 01/01/2008:
  export dags_rap.
end.
output close.

output to value("kas_rap.d").
for each kas_rap no-lock where     
     kas_rap.Dato >= 01/01/2008:
  export kas_rap.
end.
output close.

output to value("kort_spe.d").
for each Kort_Spes no-lock where     
     Kort_Spes.Dato >= 01/01/2008:
  export Kort_Spes.
end.
output close.

output to value("bokforin.d").
for each BokforingsBilag no-lock where     
     BokforingsBilag.OmsetningsDato >= 01/01/2008:
  export BokforingsBilag.
end.
output close.

output to value("Kassbila.d").
for each KassererBilag no-lock where     
     KassererBilag.Dato >= 01/01/2008:
  export KassererBilag.
end.
output close.

output to value("kassdag.d").
for each KassererDag no-lock where     
     KassererDag.Dato >= 01/01/2008:
  export KassererDag.
end.
output close.

output to value("kasskont.d").
for each KassererKontanter no-lock where     
     KassererKontanter.Dato >= 01/01/2008:
  export KassererKontanter.
end.
output close.

output to value("kasserer.d").
for each KassererOppgj no-lock where     
     KassererOppgj.Dato >= 01/01/2008:
  export KassererOppgj.
end.
output close.

output to value("kassval.d").
for each KassererValuta no-lock where     
     KassererValuta.Dato >= 01/01/2008:
  export KassererValuta.
end.
output close.

output to value("Dagsoppgjor.log") append.
  export "Eksport av Dagsoppgjor ferdig." today string(time,"HH:MM:SS").
output close.

