CURRENT-WINDOW:WIDTH = 350.

FOR EACH PkSdlHode EXCLUSIVE-LOCK WHERE
    PkSdlHode.PkSdlStatus = 10,
    FIRST PkSdlLinje OF PkSdlHode NO-LOCK WHERE 
    PkSdlLinje.Beskr MATCHES '*Vareprøve*':

    IF PkSdlHode.SendtDato > 07/01/2016 THEN
        NEXT.

    DISPLAY
        PkSdlHode.PkSdlId
        PkSdlHode.PkSdlNr
        PkSdlHode.PkSdlStatus        
        pkSdlHode.SendtDato
        PkSdlLinje.butikkNr
        PkSdlLinje.Beskr
    WITH WIDTH 350.
    

END.
