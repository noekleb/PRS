DEF VAR iAntFakt AS INT NO-UNDO.
DEF VAR iAntLev AS INT NO-UNDO.

CURRENT-WINDOW:WIDTH = 350.
CURRENT-WINDOW:HEIGHT = 45. 
FOR EACH PkSdlHode EXCLUSIVE-LOCK WHERE 
    PkSdlHode.ButikkNr = 40 AND 
    PkSdlHode.PkSdlStatus = 20 /*AND 
    PkSdlHode.PkSdlOpphav = 4*/ ,
    FIRST PkSdlMottak OF PkSdlHode NO-LOCK WHERE 
        PkSdlMottak.MottattDato >= TODAY - 30,
    FIRST Butiker NO-LOCK WHERE 
        Butiker.butik = PkSdlHode.ButikkNr:
    
    RELEASE FakturaHode.
    IF PkSdlHode.FakturaNr <> ? THEN
        FIND LAST fakturahode NO-LOCK WHERE 
            FakturaHode.FakturaNr = PkSdlHode.fakturaNr NO-ERROR.
    ELSE DO:
        FIND LAST FakturaHode NO-LOCK WHERE 
            FakturaHode.PkSdlNr = PkSdlHode.PkSdlNr AND 
            FakturaHode.FakturertDato = PkSdlMottak.MottattDato AND 
            FakturaHode.KundeNr = Butiker.KundeNr NO-ERROR.
    END.
    
    iAntLEv = 0.
    FOR EACH PkSdlLinje OF PkSdlHode NO-LOCK:
        iAntLEv = iAntLEv + PkSdlLinje.AntLevert.
    END.
    
    iAntFakt = 0.
    IF AVAILABLE FakturaHode THEN
    DO:
        FOR EACH FakturaLinje OF FakturaHode NO-LOCK:
            iAntFakt = iAntFakt + FakturaLinje.Antall.
        END.
    END.
    
    /*
    IF AVAILABLE FakturaHode AND PkSdlHode.FakturaNr = ? THEN
        PkSdlHode.FakturaNr = FakturaHode.FakturaNr.
    */    
    
    /*IF iAntLev <> iAntFakt THEN*/
    DISPLAY
        PkSdlHode.ButikkNr
        PkSdlHode.PkSdlNr
        PkSdlMottak.MottattDato
        STRING(PkSdlMottak.MottattTid,"HH:MM:SS")
        PkSdlHode.PkSdlOpphav
        PkSdlHode.OrdreType
        PkSdlHode.FakturaNr
        Butiker.KundeNr
        iantLev
        '|'
        FakturaHode.FakturaNr WHEN AVAILABLE Fakturahode
        FakturaHode.Opphav WHEN AVAILABLE Fakturahode
        FakturaHode.totalt WHEN AVAILABLE FakturaHode
        FakturaHode.EksportertDato WHEN AVAILABLE FakturaHode
        FakturaHode.KundeNr WHEN AVAILABLE FakturaHode
        iAntFakt
        '|'
        (IF iAntLev <> iAntFakt THEN '*' ELSE '')
        (IF AVAILABLE FakturaHode AND (Butiker.KundeNr <> FakturaHode.KundeNr) THEN '*' ELSE '')
    WITH WIDTH 350.
    
    
END.
