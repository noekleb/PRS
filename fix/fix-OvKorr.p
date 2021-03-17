DEF VAR iBatchNr AS INT NO-UNDO.

DEF BUFFER bBatchLogg FOR BatchLogg.
DEF BUFFER bTranslogg FOR TransLogg.

/* Batch for TransLogg */
run batchlogg.p (program-name(1),
                 "FIX OvKorr " +
                 string(today) +
                 " " +
                 string(time,"HH:MM") +
                 " " +
                 userid("dictdb"),
                 output iBatchNr).
FIND bBatchLogg WHERE BatchNr = iBatchNr.
DISPLAY bBatchLogg.BatchNr.

FOR EACH BatchLogg WHERE
    BatchLogg.Merknad begins "Overføringer bunt" AND
    BatchLogg.RegistrertDato >= 01/01/2006:

    FOR EACH Translogg OF BatchLogg:
        DISPLAY
            Translogg.ArtikkelNr
            TransLogg.Antall
            Translogg.TTID
            .
        CREATE bTransLogg.
        BUFFER-COPY TransLogg TO bTranslogg
            ASSIGN
            bTranslogg.BatchNr = iBatchNr
            bTransLogg.SeqNr   = translogg.SeqNr + 1
            bTransLogg.Antall  = TransLogg.Antall * -1
            bTranslogg.Postert = FALSE
            bTranslogg.PostertDato = ?
            bTranslogg.PostertTid  = 0
            .
    END.


END.
