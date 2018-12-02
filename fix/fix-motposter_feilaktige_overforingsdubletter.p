CURRENT-WINDOW:WIDTH = 350.

DEF VAR iAnt AS INT NO-UNDO.
DEF VAR wBatchNr LIKE BatchLogg.BatchNr NO-UNDO.

DEF STREAM Ut.

OUTPUT STREAM Ut TO VALUE('c:\appdir\tn\dobleBatcher.csv').

PUT STREAM Ut CHR(10)
    '*** START ****'
    SKIP.

FOR EACH BatchLogg EXCLUSIVE-LOCK WHERE 
    BatchLogg.RegistrertDato >= 01/01/2017 AND
    BatchLogg.OppHav BEGINS 'Poster',
    FIRST Translogg NO-LOCK WHERE 
        TransLogg.BatchNr = BatchLogg.BatchNr AND
        TransLogg.RefTekst BEGINS 'Ordrenr. nettbutikk: RETUR'
    BREAK BY BatchLogg.RegistrertDato
          BY TransLogg.RefTekst:
    IF FIRST-OF(TransLogg.RefTekst) THEN
    DO:
        iAnt = 0.
    END.
    iant = iant + 1.

    IF iAnt > 1 THEN
    DO:
        DISPLAY
            NOW
            BatchLogg.BatchNr
            BatchLogg.RegistrertDato
            STRING(BatchLogg.RegistrertTid,"HH:MM:SS")
            BatchLogg.Opphav FORMAT "x(60)"
            BatchLogg.OppdStatus
            StatusOppdatert
            TransLogg.ArtikkelNr
            TransLogg.RefTekst
        WITH WIDTH 350.

        PUT STREAM Ut
            NOW      ';'
            BatchLogg.BatchNr ';'
            BatchLogg.RegistrertDato ';'
            STRING(BatchLogg.RegistrertTid,"HH:MM:SS") ';'
            BatchLogg.Opphav ';'
            BatchLogg.OppdStatus ';'
            StatusOppdatert       ';'
            TransLogg.ArtikkelNr  ';'
            TransLogg.RefTekst ';'
            iAnt
        SKIP.

        ASSIGN 
            wBatchNr = BatchLogg.BatchNr
            BatchLogg.Opphav = 'KORR ' + BatchLogg.Opphav
            .
        run x-motposterbatch.p (input-output wBatchNr).
    END.

    IF LAST-OF(TransLogg.RefTekst) THEN
    DO:
        IF iAnt > 1 THEN
        DISPLAY
        iAnt
        WITH WIDTH 350.
    END.

END.
OUTPUT STREAM ut CLOSE.
