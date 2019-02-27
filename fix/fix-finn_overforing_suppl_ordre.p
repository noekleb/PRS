DEF VAR iBatchNr AS INT NO-UNDO.
DEF VAR iAntPoster AS INT NO-UNDO.

CURRENT-WINDOW:WIDTH = 350.

DEF BUFFER buffTransLogg FOR TransLogg.

RUN batchlogg.p ("KORR supl.ordre: ",
                     "KORR Fra butikk " +
                     '20 til butikk 15 ' +
                     string(today) +
                     " " +
                     string(TIME,"HH:MM") +
                     " " +
                     USERID("dictdb"),
                     OUTPUT iBatchNr).

FOR EACH TransLogg NO-LOCK WHERE
    TransLogg.TTId = 6 AND
    TransLogg.RefNr = 6:

    /* Motpostering */
    CREATE buffTranslogg.
    BUFFER-COPY TransLogg
        TO buffTranslogg
        ASSIGN
          buffTransLogg.BatchNr = iBatchNr
          BuffTransLogg.SeqNr   = TransLogg.SeqNr + 5
          BuffTransLogg.Antall  = TransLogg.Antall * -1  
          buffTransLogg.Postert = FALSE
          buffTransLogg.PostertDato = ?
          .
    /*
    DISPLAY
        TransLogg.butik
        TransLogg.TTId
        Translogg.Dato
        TransLogg.RefNr
        TransLogg.RefTekst
    WITH WIDTH 350.
    */
END.
