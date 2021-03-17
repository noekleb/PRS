CURRENT-WINDOW:WIDTH = 350.

DEF BUFFER bufKOrdreLinje FOR KOrdreLinje.

FOR EACH KOrdreHode NO-LOCK WHERE 
    KordreHode.KOrdre_Id = 1170006252 AND
    KOrdreHode.ButikkNr = 15 AND 
    KOrdreHode.RegistrertDato >= 01/01/2017,
    LAST KOrdreLinje OF KOrdreHode NO-LOCK WHERE 
        KORdreLinje.VareNr = 'BETALT':

    IF NOT CAN-DO('amex,visa,mastercard',KORdreLinje.VareTekst) THEN
        NEXT.
    ELSE DO:
        FOR EACH bufKOrdreLinje OF KOrdreHode EXCLUSIVE-LOCK WHERE
            bufKOrdreLinje.VareNr <> 'BETALT':
            ASSIGN
                bufKOrdreLinje.UtleverButikk = 2 /* Stortingsgaten */
                .
        END.
        
        
        DISPLAY
            KOrdreHode.RegistrertDato
            KOrdreHode.butikkNr
            KORdreLinje.VareNr        
            KORdreLinje.Varetekst     
            KOrdreLinje.NettoPris
            KOrdreLinje.NettoLinjesum 
            KOrdreLinje.BruttoPris    
            KOrdreLinje.Pris          
            KOrdreLinje.Linjesum      
            KOrdreLinje.UtleverButikk
        WITH WIDTH 350.
        
    END.

END.
