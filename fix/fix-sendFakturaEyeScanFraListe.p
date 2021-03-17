DEF VAR bDisp AS LOG NO-UNDO.
DEFINE VARIABLE cLst AS CHARACTER NO-UNDO.
DEFINE VARIABLE lFakturaNr AS DECIMAL NO-UNDO.
DEFINE VARIABLE iLoop AS INTEGER NO-UNDO.

CURRENT-WINDOW:WIDTH = 350.
CURRENT-WINDOW:HEIGHT = 40.

DEF BUFFER bFakturaHode FOR FakturaHode.

ASSIGN 
    bDisp = TRUE
    cLst = '2001017,2001016,2001015,' + 
           '2001011,' +
           '2001002,' +
           '2001001,' +
           '2001000,' +
           '2000999,' +
           '2000998,' +
           '2000997,' +
           '2000996,' +
           '2000995,' +
           '2000994,' +
           '2000993,' +
           '2000992,' +
           '2000991,' +
           '2000989,' +
           '2000988,' +
           '2000987,' +
           '2000986,' +
           '2000985,' +
           '2000984,' +
           '2000983,' +
           '2000982,' +
           '2000981'
    .

DO iLoop = 1 TO NUM-ENTRIES(cLst):
  lFakturaNr = DEC(ENTRY(iLoop,cLst)).
  
  FIND FakturaHode NO-LOCK WHERE 
    FakturaHode.FakturaNr = lFakturaNr NO-ERROR.
  IF AVAILABLE FakturaHode THEN 
  DO:
    FIND Kunde NO-LOCK WHERE
        Kunde.KundeNr = FakturaHode.KundeNr NO-ERROR.
    IF AVAILABLE Kunde THEN
    DO:
        FIND FIRST Butiker NO-LOCK WHERE 
            Butiker.KundeNr = Kunde.KundeNr NO-ERROR.
        IF AVAILABLE Butiker THEN
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
  END.
END.

