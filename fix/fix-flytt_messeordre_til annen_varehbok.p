current-window:width = 250.
for each VarebehLinjeTHode where
  VareBehLinjeTHode.VareBehNr = 9000004 and
  VareBehLinjeTHode.ButikkNr  = 195:
  
  display
  VareBehLinjeTHode.VareBehNR
  VareBehLinjeTHode.ButikkNr
  with width 250.
  
  for each VareBehLinjeTrans of VareBehLinjeTHode:
    display 
    VareBehLinjeTrans.VareBehNr
    VareBehLinjeTrans.ButikkNr
    VarebehLinjeTrans.ArtikkelNr
    VareBehLinjeTrans.Kode 
    VareBehLinjeTrans.RegistrertDato
    Bestilt1
    Bestilt2
    Bestilt3
    Bestilt4
    with width 250.
    /*
    assign
    VareBehLinjeTrans.VareBehNr = 9000003.*/
  end.
  
  
  
  
end.  
