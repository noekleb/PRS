CURRENT-WINDOW:WIDTH = 350.
ON write of KORdreHode OVERRIDE  
DO:

END.
FOR EACH KOrdreHode WHERE 
    KORdreHode.ButikkNr = 15 AND
    KOrdreHode.LevStatus = '30' AND 
    KOrdreHode.LeveringsDato <= 01/01/2017:

    DISPLAY
        KORdreHode.ButikkNr 
        KOrdreHode.LevStatus
        KOrdreHode.LeveringsDato 
        VerkstedMerknad FORMAT "x(40)"
    WITH WIDTH 350.

    levStatus = '60'.

END.
