CURRENT-WINDOW:WIDTH = 350.

DEF BUFFER bufPkSdlHode FOR PkSdlhode.

FOR EACH FakturaHode NO-LOCK WHERE 
    FakturaHode.ButikkNr = 20 AND 
    Fakturahode.FakturaNr >= 20000082 AND 
    FakturaHode.FakturertDato >= 01/01/2020,
    FIRST Kunde OF FakturaHode NO-LOCK WHERE 
        Kunde.butikkNr = 10
    BREAK BY FakturaHode.ButikkNr
          BY FakturaHode.FakturaNr:
    
    
    FIND LAST PkSdlHode NO-LOCK WHERE 
        PkSdlHode.FakturaNr = FakturaHode.FakturaNr NO-ERROR.
    IF NOT AVAILABLE PkSdlHode THEN
        FIND LAST bufPkSdlHode NO-LOCK WHERE 
            bufPkSdlHode.PkSdlNr = FakturaHode.PkSdlNr NO-ERROR.
    DO:
        
    END.
    DISPLAY
        FakturaHode.FakturaNr
        FakturaHode.FakturertDato FORMAT "99/99/9999"
        FakturaHode.KundeNr
        Kunde.butikkNr
        FakturaHode.Totalt
        FakturaHode.PkSdlNr
        PkSdlHode.PkSdlNr WHEN AVAILABLE PkSdlHode
        bufPkSdlHode.PkSdlNr WHEN AVAILABLE bufPkSdlHode
    WITH WIDTH 350.
END.
