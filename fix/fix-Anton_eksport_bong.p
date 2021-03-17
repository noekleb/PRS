output to value("bonghode.log").
  export "Eksport av bongdata startet." today string(time,"HH:MM:SS").
output close.

output to value("bonghode.d").
for each Butiker no-lock,
  each Kasse no-lock where Kasse.ButikkNr = Butiker.Butik,
  each BongHode where
     BongHode.ButikkNr = Butiker.Butik  and
     BongHode.Gruppe = 1 and
     BongHode.KAsseNr = Kasse.KasseNr and      
     BongHode.Dato >= 01/01/2008:
  export BongHode.
end.
output close.

output to value("bonglinj.d").
for each Butiker no-lock,
  each Kasse no-lock where Kasse.ButikkNr = Butiker.Butik,
  each BongHode where
     BongHode.ButikkNr = Butiker.Butik  and
     BongHode.Gruppe = 1 and
     BongHode.KAsseNr = Kasse.KasseNr and      
     BongHode.Dato >= 01/01/2008:
  for each BongLinje no-lock where
    BongLinje.B_Id = BongHode.B_Id:
      export BongLinje.
  end.
end.
output close.

output to value("datasett.d").
for each Butiker no-lock,
  each Datasett where
     DataSett.ButikkNr = Butiker.Butik and 
     DataSett.Dato >= 01/01/2008:
  export DataSett.
end.
output close.

output to value("bonghode.log") append.
  export "Eksport av bongdata ferdig." today string(time,"HH:MM:SS").
output close.

