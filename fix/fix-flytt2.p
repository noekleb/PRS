
CURRENT-WINDOW:WIDTH = 320.

DEF BUFFER bufVareBehLinjeTrans FOR VareBehLinjeTrans.

FOR EACH VareBehLinjeTHode EXCLUSIVE-LOCK WHERE
    VareBehLinjeTHode.VareBehNr = 9000009 AND
    VareBehLinjeTHode.ButikkNr = 268:

    DISPLAY
        VareBehLinjeTHode
        WITH WIDTH 320.

    FOR EACH VareBehLinjeTrans OF VareBehLinjeTHode EXCLUSIVE-LOCK /*WHERE
        (VareBehLinjeTrans.Bestilt1 + 
             VareBehLinjeTrans.Bestilt2 + 
             VareBehLinjeTrans.Bestilt3 + 
             VareBehLinjeTrans.Bestilt4) > 0 */:

        
        DISPLAY
            VareBehLinjeTrans.ArtikkelNr            
            (VareBehLinjeTrans.Bestilt1 + 
             VareBehLinjeTrans.Bestilt2 + 
             VareBehLinjeTrans.Bestilt3 + 
             VareBehLinjeTrans.Bestilt4)
            .

        DELETE varebehlinjeTrans.
        
    END.
    DELETE varebehlinjeTHode.
END.

