DEF BUFFER bufKunde FOR Kunde.

CURRENT-WINDOW:WIDTH = 350.

FOR EACH FakturaHode NO-LOCK:
    FIND Butiker NO-LOCK WHERE 
        Butiker.Butik = FakturaHode.butikkNr NO-ERROR.
    FIND Kunde OF FakturaHode NO-LOCK NO-ERROR.
    FIND bufKunde OF Butiker NO-LOCK NO-ERROR.
    FIND Butiker NO-LOCK WHERE butiker.butik = FakturaHode.butikkNr NO-ERROR.
    FIND LAST PkSdlHode NO-LOCK WHERE 
        PkSdlHode.PkSdlNr = FakturaHode.PkSdlNr AND 
        PkSdlHode.PkSdlStatus = 20 NO-ERROR.
    DISPLAY
        FakturaHode.ButikkNr
        FakturaHode.KundeNr
        FakturaHode.FakturaNr
        FakturaHode.Faktura_Id
        FakturaHode.bilagsType
        FakturaHode.Opphav
        FakturaHode.PkSdlNr
        FakturaHode.TotalRabatt%
        FakturaHode.TotalRabattKr
        FakturaHode.Totalt
        FakturaHode.SendingsNr
        FakturaHode.EksportertDato
        FakturaHode.EksportertAv
        /*
        FakturaHode.RegistrertDato
        */
        Kunde.butikkNr
        '|'
        Butiker.KundeNr
        bufKunde.ButikkNr
        /*
        '|'
        PkSdlHode.FakturaNr WHEN AVAILABLE PkSdlHode
        FakturaHode.FNotat FORMAT "x(40)"
        */
    WITH WIDTH 350.
END.
