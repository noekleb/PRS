DEF STREAM Ut.

FIND Butiker NO-LOCK WHERE 
    Butiker.butik = 10 NO-ERROR.

OUTPUT STREAM Ut TO VALUE('konv\fakturaliste_' + STRING(Butiker.butik) + '_02112020.csv').
    
FOR EACH FakturaHode NO-LOCK WHERE 
    FakturaHode.FakturertDato >= 01/01/2020 AND 
    FakturaHode.FakturertDato <= 01/10/2020 AND 
    FakturaHode.PkSdlNr > '' /*
    FakturaHode.KundeNr = Butiker.KundeNr*/
    :
    
    PUT STREAM Ut UNFORMATTED 
        FakturaHode.FakturaNr ';'
        FakturaHode.FakturertDato ';'
        FakturaHode.ButikkNr ';'
        FakturaHode.KundeNr ';'
        SKIP.
END.

OUTPUT STREAM Ut CLOSE.
