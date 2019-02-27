CURRENT-WINDOW:WIDTH = 250.

DEF VAR wBAtchNr AS INT NO-UNDO.

DEF BUFFER bufBatchLogg FOR BatchLogg.

FOR EACH bufBatchLogg WHERE
    /*bufBatchLogg.BatchNr = 79495 AND*/ 
    bufBatchLogg.Beskrivelse BEGINS "Plukklager" /* AND
    bufBatchLogg.StatusOppdatert < 11/01/2005*/:

    DISPLAY
        bufBatchLogg.BatchNr
        bufBatchLogg.OppdStatus
        bufBatchLogg.StatusOppdatert
        bufBatchLogg.Beskrivelse
        WITH WIDTH 250.


    ASSIGN
        wBatchNr = bufBatchLogg.BatchNr
        .
    run x-motposterbatch.p (input-output wBatchNr).

    FOR EACH Translogg OF bufBatchLogg:
      ASSIGN        
        TransLogg.Postert = FALSE
        Translogg.TilStorl = Translogg.Storl
        .
    END.
    bufBatchLogg.OppdStatus = 2.

END.
