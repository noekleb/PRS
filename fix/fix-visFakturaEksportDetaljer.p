CURRENT-WINDOW:WIDTH = 350.
FOR EACH FakturaHode EXCLUSIVE-LOCK WHERE 
    FakturaHode.FakturertDato >= 09/05/2020:
    
    FIND Kunde OF FakturaHode NO-LOCK NO-ERROR.
    
    DISPLAY
    FakturaHode.FakturaNr
    FakturaHode.butikkNR
    Kunde.butikkNr
    WITH WIDTH 350.
    
    FakturaHode.EksportertDato = ?.
    
END.
