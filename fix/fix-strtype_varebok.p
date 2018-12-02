CURRENT-WINDOW:WIDTH = 300.

DEF VAR lModellFarge AS DEC FORMAT ">>>>>>>>>>>>9" NO-UNDO. 
DEF VAR cTekst AS CHAR FORMAT "x(20)" NO-UNDO.
DEF VAR iStrTypeId AS INT NO-UNDO.

DEF VAR cStrListe AS CHAR FORMAT "x(40)" NO-UNDO.
DEF VAR cMangler  AS CHAR FORMAT "x(40)" NO-UNDO.

DEF BUFFER bufArtBas FOR ArtBas.

FOR EACH  VarebokLinje NO-LOCK WHERE 
    VareBokLinje.VareBokNr = 9000040:

    FIND ArtBas OF VareBokLinje EXCLUSIVE-LOCK.
    FIND StrType OF ArtBAs NO-LOCK NO-ERROR.

    ASSIGN 
    cStrListe  = ''
    cMangler   = ''
    iStrTypeId = 0.

    FOR EACH Strekkode OF ArtBas NO-LOCK:
        FIND StrKonv OF Strekkode NO-LOCK.
        ASSIGN
            cStrListe = cStrListe + (IF cStrListe = '' THEN '' ELSE '|') + 
                        StrKonv.Storl.
        IF NOT CAN-FIND(StrTStr WHERE
                        StrTStr.StrTypeId = ArtBas.StrTypeId AND
                        StrTStr.SoStorl   = StrKonv.Storl) THEN
            ASSIGN
            cMangler = cMangler + (IF cMangler = '' THEN '' ELSE '|') + 
                        StrKonv.Storl.
    END.

    IF cMangler <> '' THEN
    DO:
        IF ArtBas.ModellFarge > 0 
            THEN lModellFarge = ArtBas.ModellFarge.
        ELSE lModellFarge = ArtBas.ArtikkelNr.

        cTekst = 'Modell ' + STRING(lModellFarge).
        
        FOR EACH Strekkode OF ArtBas NO-LOCK:
            FIND StrKonv NO-LOCK WHERE
                StrKonv.StrKode = Strekkode.StrKode NO-ERROR.
            RUN opprett_strtype2.p (cTekst, StrKonv.Storl, ArtBas.Vg, OUTPUT iStrTypeId). 
        END.

        IF lModellFarge > 0 AND iStrTypeId > 0 THEN
        DO:
            ArtBas.StrTypeId = iStrTypeId.
            IF ArtBas.ModellFarge > 0 THEN
              FOR EACH bufArtBas WHERE
                BufArtBas.ModellFarge = lModellFarge:
                ASSIGN
                  bufArtBas.StrTypeId = iStrTypeId.
              END.
        END.
        /*
        DISPLAY
            lModellFarge COLUMN-LABEL 'Mal'
            iStrTypeId COLUMN-LABEL 'Ny'
            VareBokLinje.ArtikkelNr
            ArtBas.ArtikkelNr
            ArtBas.ModellFarge
            VareBokLinje.Beskr
            ArtBas.StrTypeId
            cMangler
            WITH WIDTH 300
            .
        */
    END.
END.
