DEF VAR iNr AS INT NO-UNDO.
iNr = 11234.
FOR EACH KOrdreHode:

    IF NOT Sendingsnr MATCHES '*RETUR*' THEN
        ASSIGN 
            SendingsNr = STRING(iNr)
            ReturNr    = STRING(iNr + 1)
            .
    /*LevStatus = '47'.*/
    iNr = iNr + 2.
END.
