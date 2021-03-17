CURRENT-WINDOW:WIDTH = 350.
DEF VAR iAnt AS INT NO-UNDO.

FOR EACH KOrdreHode NO-LOCK WHERE 
    KOrdrEHode.RegistrertDato >= 06/05/2020 AND 
    NUM-ENTRIES(KOrdreHode.EkstOrdreNr,' ') = 1:

    IF KOrdreHode.ekstORdreNr = '500123803' THEN
    DO:
        NEXT.    
    END.

    
    FIND FIRST OvBunt NO-LOCK WHERE 
        Ovbunt.Merknad = "Importert fra nettbutikk: " + KOrdreHode.EkstOrdreNr NO-ERROR.
    
    DISPLAY
        KOrdreHode.ekstORdreNr
        KOrdreHode.RegistrertDato
        STRING(KORdrEHode.RegistrertTid,"HH:MM:SS")
        OvBunt.Merknad WHEN AVAILABLE Ovbunt FORMAT "x(45)"
     WITH WIDTH 350.
END.
