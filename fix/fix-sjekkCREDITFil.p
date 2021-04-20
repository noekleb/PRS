DEF VAR cFilNavn AS CHAR FORMAT "x(40)" NO-UNDO.
DEF VAR cRecord AS CHAR FORMAT "x(110)" NO-UNDO.
DEF VAR lFakturaNr AS DEC FORMAT ">>>>>>>>>>>>9" NO-UNDO.
DEF VAR cSalgsKonto AS CHAR FORMAT "x(10)" NO-UNDO.
DEF VAR cMvaKonto AS CHAR FORMAT "x(10)" NO-UNDO.
DEF VAR cRabattKonto AS CHAR FORMAT "x(10)" NO-UNDO.
DEF VAR cNetto AS CHAR FORMAT "x(10)" NO-UNDO.
DEF VAR cMva AS CHAR FORMAT "x(10)" NO-UNDO.
DEF VAR cRab AS CHAR FORMAT "x(10)" NO-UNDO.
DEF VAR cDato AS CHAR FORMAT "x(10)" NO-UNDO.
DEF VAR cKlient AS CHAR FORMAT "x(10)" NO-UNDO.
DEF VAR cTotalt AS CHAR FORMAT "x(10)" NO-UNDO.


DEF STREAM Inn.

CURRENT-WINDOW:WIDTH = 350.
CURRENT-WINDOW:HEIGHT = 40.

ASSIGN 
    cFilNavn = 'konv\CREDIT0309.txt'
    .

INPUT STREAM Inn FROM VALUE(cFilNavn).
REPEAT:
    IMPORT STREAM Inn UNFORMATTED
        cRecord.
        
    ASSIGN 
        cDato = (TRIM(ENTRY(1,cRecord,','),'"')) 
        lFakturaNr = DEC(TRIM(ENTRY(2,cRecord,','),'"'))
        cKlient = (TRIM(ENTRY(3,cRecord,','),'"')) 
        cTotalt = (TRIM(ENTRY(4,cRecord,','),'"')) 
        cSalgsKonto = (TRIM(ENTRY(5,cRecord,','),'"')) 
        cNetto = (TRIM(ENTRY(6,cRecord,','),'"')) 
        cMvaKonto  = (TRIM(ENTRY(7,cRecord,','),'"')) 
        cMva = (TRIM(ENTRY(8,cRecord,','),'"')) 
        cRabattKonto  = (TRIM(ENTRY(9,cRecord,','),'"')) 
        cRab = (TRIM(ENTRY(10,cRecord,','),'"')) 
        NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
    DO:
        NEXT.         
    END.
    FIND FIRST FakturaHode NO-LOCK WHERE 
         FakturaHode.KundeNr >= 0 AND 
        FakturaHode.FakturaNr = lFakturaNr NO-ERROR.
    IF AVAILABLE FakturaHode THEN
    DO:
        FIND Kunde WHERE 
            Kunde.KundeNr = FakturaHode.KundeNr NO-LOCK NO-ERROR.             
    END.
    FIND Butiker NO-LOCK WHERE 
        Butiker.Butik = FakturaHode.ButikkNr NO-ERROR.

    DISPLAY
        cDato
        lFakturaNr
        cKlient
        FakturaHode.ButikkNr WHEN AVAILABLE FakturaHode
        Butiker.ButNamn  WHEN AVAILABLE Butiker
        Kunde.butikkNr  WHEN AVAILABLE Kunde
        Kunde.KundeNr  WHEN AVAILABLE Kunde
        Kunde.Navn   WHEN AVAILABLE Kunde
        cTotalt
        cSalgsKonto
        cNetto
        cMvaKonto
        cMva
        cRabattKonto
        cRab
        /*cRecord*/
    WITH WIDTH 350.
END.
INPUT STREAM Inn CLOSE.
