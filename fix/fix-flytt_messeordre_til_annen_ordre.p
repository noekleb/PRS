CURRENT-WINDOW:WIDTH = 320.

DEF BUFFER bufVareBehLinjeTrans FOR VareBehLinjeTrans.

FOR EACH VareBehLinjeTHode EXCLUSIVE-LOCK WHERE
    VareBehLinjeTHode.VareBehNr = 9000009 AND
    VareBehLinjeTHode.ButikkNr = 329:

    DISPLAY
        VareBehLinjeTHode
        WITH WIDTH 320.

    FOR EACH VareBehLinjeTrans OF VareBehLinjeTHode EXCLUSIVE-LOCK WHERE
        (VareBehLinjeTrans.Bestilt1 + 
             VareBehLinjeTrans.Bestilt2 + 
             VareBehLinjeTrans.Bestilt3 + 
             VareBehLinjeTrans.Bestilt4) > 0:

        FIND bufVareBehLinjeTrans EXCLUSIVE-LOCK WHERE
            bufVareBehLinjeTrans.VareBehNr  = 9000008 AND
            bufVareBehLinjeTrans.ButikkNr   = VareBehLinjeTrans.ButikkNr AND
            bufVareBehLinjeTrans.Kode       = VareBehLinjeTrans.Kode AND
            bufVareBehLinjeTrans.ArtikkelNr = VareBehLinjeTrans.ArtikkelNr NO-ERROR.
        DISPLAY
            VareBehLinjeTrans.ArtikkelNr
            (VareBehLinjeTrans.Bestilt1 + 
             VareBehLinjeTrans.Bestilt2 + 
             VareBehLinjeTrans.Bestilt3 + 
             VareBehLinjeTrans.Bestilt4)
            bufVareBehLinjeTrans.ArtikkelNr WHEN AVAILABLE bufVareBehLinjeTrans
            (bufVareBehLinjeTrans.Bestilt1 + 
             bufVareBehLinjeTrans.Bestilt2 + 
             bufVareBehLinjeTrans.Bestilt3 + 
             bufVareBehLinjeTrans.Bestilt4)  WHEN AVAILABLE bufVareBehLinjeTrans
            .

        /* Tar bort tomme linjer */
        IF AVAILABLE bufVareBehLinjeTrans THEN
        DO:
            IF (bufVareBehLinjeTrans.Bestilt1 + 
                 bufVareBehLinjeTrans.Bestilt2 + 
                 bufVareBehLinjeTrans.Bestilt3 + 
                 bufVareBehLinjeTrans.Bestilt4) = 0 THEN
                DELETE bufVareBehLinjeTrans.
        END.

        ASSIGN
            VareBehLinjeTrans.VareBehNr = 9000008 NO-ERROR.
    END.
    ASSIGN
        VarebehLinjeTHode.VareBehNr = 9000008 NO-ERROR.
END.
