CURRENT-WINDOW:WIDTH = 350.
CURRENT-WINDOW:HEIGHT = 40.

FOR EACH FakturaEksport NO-LOCK WHERE 
    DATE(FakturaEksport.EksporterDatoTid) >= 09/04/2020:
    DISPLAY
        FakturaEksport.SendingsNr
        FakturaEksport.EksporterDatoTid
        FakturaEksport.EksportertAv
        FakturaEksport.Merknad
        FakturaEksport.Opphav
    WITH WIDTH 350.
    FOR EACH FakturaHode NO-LOCK WHERE 
        FakturaHode.SendingsNr = STRING(FakturaEksport.SendingsNr):
        
        DISPLAY
            FakturaHode.FakturaNr
            FakturaHode.FakturertDato
            FakturaHode.EksportertDato
        WITH WIDTH 350.
    END.
END.
