CURRENT-WINDOW:WIDTH = 250.

DEF VAR wBAtchNr AS INT NO-UNDO.

DEF BUFFER bufBatchLogg FOR BatchLogg.

FOR EACH bufBatchLogg WHERE
    /*bufBatchLogg.BatchNr = 79495 AND*/ 
    bufBatchLogg.Beskrivelse BEGINS "Overføringer bunt: " AND
    bufBatchLogg.StatusOppdatert >= 11/01/2005:

    DISPLAY
        bufBatchLogg.BatchNr
        bufBatchLogg.OppdStatus
        bufBatchLogg.StatusOppdatert
        bufBatchLogg.Beskrivelse
        WITH WIDTH 250.

    /*
    FOR EACH bufTranslogg OF BatchLogg:
        DISPLAY
            Translogg.BatchNr 
            Translogg.TTID
            Translogg.ArtikkelNr
            TransLogg.BongTekst
            Translogg.Antall
            WITH WIDTH 250.
    END.
    */

    ASSIGN
        wBatchNr = bufBatchLogg.BatchNr
        .
    run x-motposterbatch.p (input-output wBatchNr).
END.
