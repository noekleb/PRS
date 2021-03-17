CURRENT-WINDOW:WIDTH = 350.
FOR EACH BatchLogg WHERE 
    BatchLogg.OppdStatus < 4,
    EACH Translogg OF BatchLogg:

    FIND ArtBas NO-LOCK WHERE 
        ArtBas.ArtikkelNr = TransLogg.ArtikkelNr NO-ERROR.

    /*IF TransLogg.TTId = 6 AND 
        TransLogg.TBId = 2 THEN */
    DO:
        DISPLAY
            BatchLogg.BatchNr
            BatchLogg.OppdStatus
            BatchLogg.RegistrertDato
            BatchLogg.EDato
            BatchLogg.Oppdatert
            BatchLogg.Opphav
            '|'
            TransLogg.TTID
            TransLogg.TBId
            TransLogg.Dato
            Translogg.Postert
            Translogg.ArtikkelNr
            TransLogg.Feilkode
            Translogg.butik
            TransLogg.OvBut
            '|'
            ArtBas.ArtikkelNr WHEN AVAILABLE ArtBas
            ArtBas.Beskr WHEN AVAILABLE ArtBas
            ArtBas.LevKod WHEN AVAILABLE ArtBas
            ArtBas.LevFargKod WHEN AVAILABLE ArtBas

        WITH WIDTH 350.
        BatchLogg.OppdStatus = 2.
        /*
        TransLogg.OvBut = 20.
        translogg.tbid = 1.
        */
    END.
END.
