CURRENT-WINDOW:WIDTH = 350.
FOR EACH varebehLinjeTrans WHERE 
VareBehNr = 1720000003 /*AND ButikkNr = 176*/:
    DISPLAY
        VarebehLinjeTrans.VareBehNr
        ButikkNr
        ArtikkelNr
        VarebehLinjeTrans.Bestilt1
        Bestilt2
        Bestilt3
        Bestilt4
        WITH WIDTH 350.
    /*DELETE VareBehLinjeTrans.*/
END.
