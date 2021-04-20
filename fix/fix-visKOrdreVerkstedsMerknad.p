CURRENT-WINDOW:WIDTH = 350.
CURRENT-WINDOW:HEIGHT = 40.

FOR EACH KORdreHode NO-LOCK WHERE 
    KORdreHode.LevStatus = '50' AND 
    KORdreHode.RegistrertDato >= 09/02/2020 AND 
    KORdreHode.RegistrertDato <= 09/16/2020 AND 
    KOrdreHode.VerkstedMerknad MATCHES '*Retur fra butikk*' AND
    ENTRY(NUM-ENTRIES(KOrdreHode.VerkstedMerknad,' '),KOrdreHode.VerkstedMerknad,' ') <> '16.'
    :
    
    DISPLAY
        KORdre_Id
        EkstOrdreNr
        ENTRY(NUM-ENTRIES(KOrdreHode.VerkstedMerknad,' '),KOrdreHode.VerkstedMerknad,' ')
        EDato
        STRING(ETid,"HH:MM:SS")
        SendingsNr
        ReturNr
        VerkstedMerknad 
    WITH WIDTH 350.
    
    
END.

