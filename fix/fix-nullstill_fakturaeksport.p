DEF VAR bDisp AS LOG NO-UNDO.

CURRENT-WINDOW:WIDTH = 350.
CURRENT-WINDOW:HEIGHT = 40.

DEF BUFFER bFakturaHode FOR FakturaHode.

ASSIGN 
    bDisp = TRUE
    .

FOR EACH FakturaHode NO-LOCK WHERE 
      FakturaHode.FakturaNr >= 20100377 AND 
      FakturaHode.FakturaNr <= 20100378
      /*FakturaHode.FakturertDato >= 09/14/2020*/
      :
      
    FIND Kunde NO-LOCK WHERE
        Kunde.KundeNr = FakturaHode.KundeNr NO-ERROR.
    IF NOT CAN-DO('1,20,10100,10110,10120,10130,10140',STRING(Kunde.ButikkNr)) THEN
        NEXT.

    IF bDisp THEN
    DO:
        DISPLAY
            FakturaHode.Faktura_Id
            FakturaHode.ButikkNr
            FakturaHode.FakturaNr
            FakturaHode.FakturertDato
            TODAY - FakturaHode.FakturertDato COLUMN-LABEL 'Ant dager'
            '|'
            FakturaHode.EksportertDato
            FakturaHode.EksportertAv  
            FakturaHode.SendingsNr    
            '|'
            FakturaHode.RegistrertDato
            STRING(FakturaHode.RegistrertTid,"HH:MM:SS")
            '|'
            Kunde.ButikkNr WHEN AVAILABLE Kunde
            Kunde.KundeNr  WHEN AVAILABLE Kunde
            Kunde.Navn WHEN AVAILABLE Kunde
        WITH WIDTH 350.
    END.
    
    DO TRANSACTION:
        FIND bFakturaHode WHERE 
            ROWID(bFakturaHode) = ROWID(FakturaHode) EXCLUSIVE-LOCK.
        ASSIGN   
            bFakturaHode.EksportertDato = ?
            bFakturaHode.EksportertAv   = ''
            bFakturaHode.SendingsNr     = ''
            .
        RELEASE bFakturaHode.    
    END.
    
END.
