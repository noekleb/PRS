DEFINE VARIABLE iBatchNr AS INTEGER NO-UNDO.
DEFINE VARIABLE iSeqNr   AS INTEGER NO-UNDO.

CURRENT-WINDOW:WIDTH = 350.

DEF BUFFER bTransLogg FOR TransLogg.


/* Oppretter batch som skal balangsere PLU'er og ikke lagerstyrte varer */
IF iBatchNr = 0 THEN 
   RUN batchlogg.p (PROGRAM-NAME(1),
            "KORR Motpostert 848 overføringer " +
            string(TODAY) +
            " " +
            string(TIME,"HH:MM") +
            " " +
            USERID("dictdb"),
            OUTPUT iBatchNr).


FOR EACH TransLogg NO-LOCK WHERE
    Translogg.Dato >= 06/01/2015 AND 
    Translogg.TTId  = 5 AND
    Translogg.Butik = 848:

    IF Translogg.Ovbut = 0 THEN
        NEXT.

    /*
    DISPLAY
        TransLogg.butik
        Translogg.OvBut COLUMN-LABEL 'OvBut'
        TransLogg.TTId
        TransLogg.Dato
        TransLogg.Antall

    WITH WIDTH 350.
    */
    
    MOTPOSTER:
    DO:
        iSeqNr = TransLogg.SeqNr + 3.
        CREATE bTransLogg.
        BUFFER-COPY TransLogg
            EXCEPT SeqNr BatchNr Butik
            TO bTranslogg
            ASSIGN
                bTransLogg.Butik       = 16
                bTransLogg.BatchNr     = iBatchNr
                bTranslogg.SeqNr       = iSeqNr
                bTransLogg.Antall      = TransLogg.Antall * -1
                bTransLogg.Postert     = FALSE
                bTransLogg.PostertDato = ?
                bTransLogg.PostertTid  = 0
                .
        RELEASE bTransLogg.
    END.
   
END.


