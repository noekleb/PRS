DEF VAR iLoop AS INT NO-UNDO.
DEF VAR cFakturaLst AS CHAR NO-UNDO.
DEF VAR lFakturaNr AS DEC NO-UNDO.
DEF VAR cSkipLst AS CHAR NO-UNDO.

DEF BUFFER bufKunde FOR Kunde.

CURRENT-WINDOW:WIDTH = 350.

ASSIGN 
    cFakturaLst = ''
    cSkipLst    = '20000068,' +
                  '20000067,' +
                  '20000066,' +
                  '20000065,' +
                  '20000064,' +
                  '20000063,' +
                  '20000062,' +
                  '20000061,' +
                  '20000060,' +
                  '20000059,' +
                  '20000058,' +
                  '20000056,' +
                  '20000047,' +
                  '20000040,' +
                  '20000029,' 

    .
FAKTURALOOP:
FOR EACH FakturaHode NO-LOCK WHERE 
    FakturaHode.ButikkNr = 20 AND 
    FakturaHode.KundeNr = 100105 AND 
    FakturaHode.FakturertDato >= 01/01/2020,
    FIRST kunde OF FakturaHode NO-LOCK:

    /* Skipper disse */
    IF CAN-DO(cSkipLst,STRING(FakturaHode.FakturaNr)) THEN
        NEXT FAKTURALOOP.

    DISPLAY
        FakturaHode.FakturaNr
        FakturaHode.FakturertDato
        FakturaHode.ButikkNr
        FakturaHode.KundeNr
        Kunde.butikkNr
    WITH WIDTH 350.

    
    RUN sendFakturaEMail.p ( FakturaHode.Faktura_Id ).
    

END. /* FAKTURALOOP */



