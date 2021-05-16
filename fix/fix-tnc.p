DEF VAR cPkSdlLst AS CHAR NO-UNDO.

CURRENT-WINDOW:WIDTH = 350.

FOR EACH PkSdlHode  WHERE 
    PkSdlHode.butikkNr = 40 AND 
    PkSdlHode.PkSdlStatus = 20 AND 
    CAN-FIND(FIRST PkSdlMottak OF PkSdlHode WHERE 
             PkSdlMottak.MottattDato >= 01/01/2018) AND 
    PkSdlHode.FakturaNr = ?
    :

    ASSIGN 
        cPkSdlLst = cPkSdlLst + (IF cPkSdlLst = '' THEN '' ELSE ',') + 
        STRING(PkSdlHode.PkSdlNr).
        
    FOR EACH FakturaHode NO-LOCK WHERE 
        FakturaHode.PkSdlNr = PkSdlHode.PkSdlNr /*AND 
        FakturaHode.ButikkNr = 20 */ :
        
    IF AVAILABLE FakturaHode THEN
    DO:
        FIND Kunde OF FakturaHode NO-LOCK NO-ERROR.
        
    END.

    /*
    DISPLAY
    PkSdlHode.PkSdlId 
    PkSdlHode.pkSdlNr 
    PkSdlHode.FakturaNr
    FakturaHode.FakturaNr WHEN AVAILABLE FakturaHode
    fakturaHode.butikkNr WHEN AVAILABLE FakturaHode
    FakturaHode.KundeNr WHEN AVAILABLE FakturaHode
    FakturaHode.FakturertDato  WHEN AVAILABLE FakturaHode
    Kunde.ButikkNr WHEN AVAILABLE Kunde
    WITH WIDTH 350 DOWN.
    DOWN 1.
    */
    IF AVAILABLE FakturaHode THEN
    DO:
        
    END.
    END.
END.
OUTPUT TO 'pksdl.txt'.
EXPORT cPkSdlLst.
