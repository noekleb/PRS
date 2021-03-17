DEF VAR lNr AS DEC FORMAT "99999999999999999999" NO-UNDO.
DEF VAR lRNr AS DEC FORMAT "99999999999999999999" NO-UNDO.
lNr = 00370726206963162518. 
lrNr = lNr + 1.

FOR EACH KOrdreHode EXCLUSIVE-LOCK: 

    IF NOT Sendingsnr MATCHES '*RETUR*' THEN
        ASSIGN  
            SendingsNr = STRING(lNr)
            ReturNr    = STRING(lrNr)
            .
    /*LevStatus = '47'.*/
    lNr = lNr + 1.
    lrNr = lrNr + 1.
              

    ASSIGN 
        KOrdreHode.AntPPEti = 1
        KOrdrEHode.AntApnet = 1
        KOrdreHode.LevStatus = '40'
        .
END. 
