CURRENT-WINDOW:WIDTH = 350.

FIND KOrdrEHode WHERE KOrdrEHode.KOrdre_Id = 1190000007.
DISPLAY
    KOrdrEHode.KORdre_Id
WITH WIDTH 350.
FOR EACH KOrdreLinje OF KOrdrEHode:
    DISPLAY
        KOrdreLinje.KOrdre_Id     
        KOrdreHode.Totalt
        KOrdrEHode.Mva
        KOrdreHode.Avrund
        /*
        KOrdrEHode.AvgPlSalg
        KOrdreHode.AvgFriSalg
        */
        '|'
        KOrdreLinje.KOrdreLinjeNr
        KORdreLinje.VareNr
        KOrdreLinje.Varetekst
        KOrdreLinje.Antall        
        KOrdreLinje.nettolinjesum 
        KOrdreLinje.NettoPris     
        KOrdreLinje.MvaKr         
        KOrdreLinje.Mva%          
        KOrdreLinje.BruttoPris    
        KOrdreLinje.Pris          
        KOrdreLinje.Linjesum      
        KOrdreLinje.Leveringsdato 
        KOrdreLinje.Faktura_Id    
    WITH WIDTH 350.
END.
