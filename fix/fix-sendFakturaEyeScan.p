DEF VAR bDisp AS LOG NO-UNDO.

CURRENT-WINDOW:WIDTH = 350.
CURRENT-WINDOW:HEIGHT = 40.

DEF BUFFER bFakturaHode FOR FakturaHode.

ASSIGN 
    bDisp = TRUE
    .

FOR EACH FakturaHode NO-LOCK WHERE 
    FakturaHode.FakturertDato >= 09/5/2020 AND 
    FakturaHode.FakturertDato <= 09/7/2020:
    IF AVAILABLE Butiker THEN 
        RELEASE butiker.
    FIND Kunde NO-LOCK WHERE
        Kunde.KundeNr = FakturaHode.KundeNr NO-ERROR.
    IF AVAILABLE Kunde THEN
    DO:
        FIND FIRST Butiker NO-LOCK WHERE 
            Butiker.KundeNr = Kunde.KundeNr NO-ERROR.
    END.

    IF bDisp AND AVAILABLE Butiker THEN
    DO:
        /* Disse skal ikke med. */
        IF CAN-DO('848,849',STRING(Butiker.butik)) THEN
            NEXT.
            
        DISPLAY
            FakturaHode.Faktura_Id
            FakturaHode.ButikkNr
            FakturaHode.FakturaNr
            FakturaHode.FakturertDato
            '|'
            FakturaHode.EksportertDato
            FakturaHode.EksportertAv  
            FakturaHode.SendingsNr    
            '|'
            FakturaHode.RegistrertDato
            STRING(FakturaHode.RegistrertTid,"HH:MM:SS")
            '|'
            Kunde.ButikkNr WHEN AVAILABLE Kunde
            Butiker.Butik WHEN AVAILABLE Butiker
            Kunde.KundeNr  WHEN AVAILABLE Kunde
            Kunde.Navn WHEN AVAILABLE Kunde
        WITH WIDTH 350.
        
        
        RUN sendFakturaEMail.p ( FakturaHode.Faktura_Id ).
        
    END.
      
END.
