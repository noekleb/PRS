CURRENT-WINDOW:WIDTH = 350.
FORM 
WITH FRAME A DOWN.
FORM
WITH FRAME B DOWN.

FOR EACH FakturaHode NO-LOCK WHERE 
    FakturaHode.ButikkNr = 20 AND 
    FakturaHode.FakturertDato >= 01/22/2020 AND 
    FakturaHode.PkSdlNr > '' AND 
    FakturaHode.FakturaNr <= 20000067,
    FIRST Kunde OF FakturaHode NO-LOCK WHERE 
      CAN-DO('10,40',STRING(Kunde.butikkNr)):
  FOR EACH PkSdlHode EXCLUSIVE-LOCK WHERE 
        PkSdlHode.PkSdlNr = FakturaHode.PkSdlNr AND 
        PkSdlHode.PkSdlStatus = 20 AND
        PkSdlHode.OrdreType >= '' AND 
        CAN-DO('1,3',STRING(PkSdlHode.PkSdlOpphav)) AND 
        PkSdlHode.FakturaNr = FakturaHode.FakturaNr:
      /*
      ASSIGN
        PkSdlHode.FakturaNr = FakturaHode.FakturaNr
        .
      */
      
      FIND Butiker NO-LOCK WHERE 
          Butiker.Butik = FakturaHode.ButikkNr NO-ERROR.
      DISPLAY
      FakturaHode.FakturaNr
      FakturaHode.PkSdlNr
      FakturaHode.FirmaBankkonto
      FakturaHode.FakturertDato
      FakturaHode.ButikkNr
      Butiker.RAPPrinter
      '|'
      Kunde.KundeNr WHEN AVAILABLE Kunde
      Kunde.butikkNr WHEN AVAILABLE Kunde
      '|'
      PkSdlHode.FakturaNr WHEN AVAILABLE PkSdlHode
      PkSdlHode.butikkNr WHEN AVAILABLE PkSdlHode
      PkSdlHode.PkSdlOpphav WHEN AVAILABLE PkSdlHode  
      PkSdlHode.OrdreType WHEN AVAILABLE PkSdlHode
      WITH FRAME A WIDTH 350 DOWN.
      DOWN 1 WITH FRAME A.
      
      /*
      FOR EACH FakturaLinje OF FakturaHode NO-LOCK:
        DISPLAY
            FakturaLinje.VareNr
            FakturaLinje.VareTekst FORMAT "x(60)"
            FakturaLinje.Storl
        WITH FRAME B WIDTH 200 DOWN.
        DOWN WITH FRAME B.
      END.
      */
  END.
END.
