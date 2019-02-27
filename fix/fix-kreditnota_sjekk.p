current-window:width = 200.
def stream Ut.
Output Stream Ut to value("kreditnota.xls") no-echo.
for each FakturaHode no-lock where 
  FakturaHode.FakturaNr >= 0 and
  FakturaHode.BTTekst = "KreditNota"
  Break By FakturaHode.FakturaNr:
  put stream ut unformatted
    FakturaHode.FakturaNr chr(9)
    FakturaHode.KundeNr chr(9)
    FakturaHode.Navn chr(9)
    Fakturahode.ButikkNr chr(9)
    FakturaHode.Dato chr(9)
    Fakturahode.BilagsType chr(9)
    Fakturahode.BTTekst chr(9)
    FakturaHode.Totalt
    skip.
end.  
