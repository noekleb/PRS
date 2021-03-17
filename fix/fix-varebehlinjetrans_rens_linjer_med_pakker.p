DEF VAR lDec AS DEC NO-UNDO.
CURRENT-WINDOW:WIDTH = 250.    
FOR EACH VarebehLinje NO-LOCK WHERE
    VareBehLinje.VareBehNr = 9000006 AND
    VareBehLinje.LevKod     = 'TN840':

    FOR EACH VareBehLinjeTrans OF VareBehLinje EXCLUSIVE-LOCK WHERE
        NOT CAN-FIND(ArtSort WHERE
                     ArtSort.ArtikkelNr = VareBehLinjeTrans.ArtikkelNr AND
                     ArtSort.SortId     = VareBehLinjeTrans.Kode):

        ASSIGN
            lDec = DEC(VareBehLinjeTrans.Kode) 
            NO-ERROR.
        IF ERROR-STATUS:ERROR = TRUE THEN DO:
            DISPLAY 
                VareBehLinje.VareBehNr
                VareBehLinje.LevNr
                VareBehLinje.ArtikkelNr
                VareBehLinje.LevKod
                VareBehLinje.Beskr
                VareBehLinje.LEvFargKod
                VareBehLinjetrans.ArtikkelNr
                Varebehlinjetrans.butikknr
                VareBehLinjeTrans.Kode
                Varebehlinjetrans.bestilt1
                Varebehlinjetrans.bestilt2
                Varebehlinjetrans.bestilt3
                Varebehlinjetrans.bestilt4
            WITH WIDTH 250.
            /*
            DELETE VareBehLinjetrans. /* DØDEN */
            */
        END.
    END.
END.
