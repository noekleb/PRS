CURRENT-WINDOW:WIDTH = 200.

DEF  VAR wRecid AS RECID NO-UNDO.
DEF BUFFER bufTranslogg FOR Translogg.

FOR EACH TelleHode WHERE
    int(TelleHode.ButikkListe) = 1 TRANSACTION:
    
    PAUSE 0.
    DISPLAY
        TelleHode.TelleNr
        TelleHode.TTId
        TelleHode.OrdreNr
        TelleHode.PkSdlNr
        TelleHode.BatchNr
        .
    MOTPOSTER:
    FOR EACH TransLogg WHERE
        TransLogg.BatchNr = TelleHode.BatchNr AND
        Translogg.Postert = TRUE:

        

        /*
        DISPLAY
            TransLogg.BatchNr
            TransLogg.TTId
            TransLogg.ArtikkelNr
            Translogg.Bongtekst
            TransLogg.SeqNr FORMAT ">>>>>>>9"
            WITH WIDTH 200.
        */

        
        create BufTransLogg.

        {translogg.i bufTransLogg TransLogg " + 1" true}

        /* Hvis transaksjonen ikke er oppdatert når den motposteres, flagges den */
        /* direkte som ferdigbehandlet.                                          */
        if TransLogg.Postert = false then
          do:
            assign
              wRecid = recid(TransLogg)
              TransLogg.Postert        = true
              TransLogg.PostertDato    = today
              TransLogg.PostertTid     = time
              TransLogg.Plukket        = true
              bufTransLogg.Postert     = true
              bufTransLogg.PostertDato = today
              bufTransLogg.PostertTid  = time
              bufTransLogg.Plukket     = true.
          end.
        
    END. /* MOTPOSTER */

    FIND BatchLogg WHERE
        BatchLogg.BatchNr = TelleHode.BatchNr.
    BatchLogg.OppdStatus = 2.
    
    /* TELLEHODE */
    ASSIGN
        TelleHode.ButikkListe = "6"
        TelleHode.Oppdatert   = ?
        .
    FOR EACH TelleLinje OF TelleHode:
        ASSIGN
            TelleLinje.butik     = 6
            TelleLinje.Oppdatert = false
            .
    END.
    run oppdaterTelling.p (TelleHode.TelleNr, THIS-PROCEDURE).

END.
