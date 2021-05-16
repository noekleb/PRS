ON CREATE OF KOrdreHode OVERRIDE DO: END.
ON WRITE OF KOrdreHode  OVERRIDE DO: END.

CURRENT-WINDOW:WIDTH = 350.

FOR EACH KOrdreHode EXCLUSIVE-LOCK WHERE 
    KOrdreHode.LevFNr = 8 AND 
    KOrdreHode.RegistrertDato >= 01/01/2020:

    IF KOrdreHode.LevFNr = 8 THEN 
        FIND Butiker NO-LOCK WHERE 
            Butiker.LevPostNr = KOrdreHode.LevPostNr NO-ERROR.
    ELSE 
        RELEASE Butiker.
        
    IF AVAILABLE Butiker AND KOrdreHode.Butik = 0 THEN
    DO:
        ASSIGN 
            KOrdreHode.Butik = Butiker.butik
            .
    END.
    /*    
    DISPLAY
        KOrdreHode.KOrdre_Id
        KOrdreHode.EkstOrdreNr
        KOrdrEHode.LevFNr
        KOrdreHode.LevPostNr
        KOrdreHode.Butik
        Butiker.Butik WHEN AVAILABLE Butiker
        Butiker.ButNamn WHEN AVAILABLE Butiker FORMAT "x(60)"
    WITH WIDTH 350.
    */
END.

