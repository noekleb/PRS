def stream Data1.
def stream Data2.
def stream Data3.

output stream Data1 to value("faktura1.d").
output stream Data2 to value("KOrdreLinje.d").
for each KOrdreHode no-lock where
  KOrdreHode.Dato >= 01/01/2008:
  export stream Data1 KOrdreHode.
  
  for each KOrdreLinje of KOrdreHode no-lock:
    export stream Data2 KOrdreLinje.
  end.
end.
output stream Data2 close.
output stream Data1 close.

output stream Data1 to value("kunderek.d").
for each KundeResKobling no-lock where
  KundeResKobling.Dato >= 01/01/2008:
  export stream Data1 KundeResKobling.
end.
output stream Data1 close.

output stream Data1 to value("fakturah.d").
output stream Data2 to value("kunderes.d").
output stream Data3 to value("faktural.d").
for each FakturaHode no-lock where
  FakturaHode.Dato >= 01/01/2008:
  export stream Data1 FakturaHode.

  for each FakturaLinje of FakturaHode no-lock:
    export stream Data3 FakturaLinje.
  end.
  for each Kundereskontr no-lock where Kundereskontr.FakturaNr = FakturaHode.FakturaNr:
    export stream Data2 Kundereskontr.
  end.
end.
output stream Data3 close.
output stream Data2 close.
output stream Data1 close.

output stream Data1 to value("kundetra.d").
for each KundeTrans no-lock where
  KundeTrans.Dato >= 01/01/2008:
  export stream Data1 KundeTrans.
end.
output stream Data1 close.

output stream Data1 to value("medetran.d").
for each MedTrans no-lock where
  MedTrans.Dato >= 01/01/2008:
  export stream Data1 MedTrans.
end.
output stream Data1 close.

output stream Data1 to value("kundesal.d").
for each KundeSaldo no-lock:
  export stream Data1 KundeSaldo.
end.
output stream Data1 close.

output stream Data1 to value("medlemsa.d").
for each MedlemSaldo no-lock:
  export stream Data1 MedlemSaldo.
end.
output stream Data1 close.

