CURRENT-WINDOW:WIDTH = 350.
FOR EACH KOrdreHode EXCLUSIVE-LOCK WHERE 
    /* KOrdreHode.EkstOrdreNr = '500128907'   */
    
    KOrdreHode.RegistrertDato >= TODAY - 100 AND 
    KOrdreHode.LevStatus = '42'
    :
    
    
    KOrdreHode.LevStatus = '40'.
    
    DISPLAY
        KOrdreHode.ekstORdreNr
        KOrdreHode.KOrdre_Id
        KORdrEHode.LEvStatus
        KOrdreHode.SendingsNr
        KOrdreHode.ReturNr
        KOrdreHode.RegistrertDato
        KOrdreHode.EDato
        KOrdreHode.AntPPEti
    WITH WIDTH 350.
END.
