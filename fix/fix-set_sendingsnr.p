DEF VAR iNr AS INT NO-UNDO.
iNr = 1.
FOR EACH KOrdreHode:

    ASSIGN 
        SendingsNr = STRING(iNr)
        ReturNr    = STRING(iNr + 1)
        .
    /*LevStatus = '30'.*/
    iNr = iNr + 2.
END.
