CURRENT-WINDOW:WIDTH = 350.
FOR EACH FakturaHode NO-LOCK WHERE 
    FakturaHode.Opphav >= 0 AND 
    FakturaHode.FakturertDato >= 11/17/2020 AND 
    FakturaHode.PkSdlNr > '':
    
    FIND Kunde OF FakturaHode NO-LOCK NO-ERROR.
    
    FIND LAST PkSdlHode NO-LOCK WHERE        
        PkSdlHode.PkSdlStatus = 20 AND 
        PkSdlHode.PkSdlNr = FakturaHode.PkSdlNr AND 
        PkSdlHode.ButikkNr = Kunde.ButikkNr NO-ERROR.
    
    IF AVAILABLE PkSdlHode THEN
    DO:
        DISPLAY
            FakturaHode.FakturaNr
            FakturaHode.PkSdlNr
            FakturaHode.KundeNr
            FakturaHode.FakturertDato
            FakturaHode.EDato
            STRING(FakturaHode.ETid,"HH:MM:SS")
            '|'
            Kunde.KundeNr
            Kunde.ButikkNr
            '|'
            PkSdlHode.PkSdlNr  WHEN AVAILABLE PkSdlHode
            PkSdlHode.ButikkNr WHEN AVAILABLE PkSdlHode
            PkSdlHode.FakturaNr WHEN AVAILABLE PkSdlHode
            PkSdlHode.EDato
            STRING(PkSdlHode.ETid,"HH:MM:SS")            
        WITH WIDTH 350.
    END.
END.    
