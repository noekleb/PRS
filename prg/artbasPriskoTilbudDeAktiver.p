DEF INPUT PARAMETER lArtikkelNr LIKE ArtBas.ArtikkelNr NO-UNDO.

DEFINE BUFFER bufPrisKo FOR PrisKo.

/* Slår av kampanje på alle aktive profiler. */
FOR EACH PrisKo NO-LOCK WHERE
    PrisKo.ArtikkelNr = lArtikkelNr AND
    PrisKo.Tilbud     = TRUE AND 
    PrisKo.TYPE       = 3:

    DO FOR bufPrisKo TRANSACTION:
        FIND bufPrisko EXCLUSIVE-LOCK WHERE 
            RECID(bufPrisKo) = RECID(PrisKo) NO-WAIT NO-ERROR.
        IF AVAILABLE bufPrisKo THEN 
        DO:
            ASSIGN
                bufPrisKo.AktiveresDato = TODAY
                bufPrisKo.AktiveresTid  = TIME
                NO-ERROR.
            IF AVAILABLE bufPrisKo THEN 
                RELEASE bufPrisKo.
        END.
    END. /* TRANSACTION */
END.
