CURRENT-WINDOW:WIDTH = 350.

FOR EACH TransLogg NO-LOCK WHERE
    Translogg.Butik = 2 AND 
    TransLogg.Dato = 04/28/2020
    USE-INDEX ButDatotid:

    FIND FIRST Kas_Rap WHERE 
             Kas_Rap.Dato = TransLogg.Dato AND 
             Kas_Rap.Butik = TransLogg.butik AND 
             Kas_Rap.Kasse = TransLogg.KassaNr AND 
             kas_rap.KassererNr = INT(TransLogg.ForsNr) NO-ERROR.

    DISPLAY
        TransLogg.butik
        TransLogg.Dato
        TransLogg.TTId
        TransLogg.TBId
        STRING(TransLogg.Tid,"HH:MM:SS")
        CAN-FIND(FIRST Kas_Rap WHERE 
                 Kas_Rap.Dato = TransLogg.Dato AND 
                 Kas_Rap.Butik = TransLogg.butik AND 
                 Kas_Rap.Kasse = TransLogg.KassaNr AND 
                 kas_rap.KassererNr = INT(TransLogg.ForsNr)) 
        kas_rap.Vekselbeholdning WHEN AVAILABLE Kas_Rap
    WITH WIDTH 350.

END.
