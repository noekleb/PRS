current-window:width = 200.
for each BongHode where BongHode.BongSTatus < 6:

  display
  BongHode.ButikkNr
  BongHode.KasseNr
  BongHode.DAto
  BongHode.BongNr
  BongHode.Gradering
  with width 200.
  BongHode.Gradering = 0.
  find DataSEtt of BongHode.
  display 
  DataSett.Datasettid
  DataSett.SettStatus
  DataSEtt.Behandlet
  with width 200.
  DataSett.Behandlet = 3.
end.
