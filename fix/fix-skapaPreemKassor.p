DEFINE VARIABLE iButikNr AS INTEGER    NO-UNDO.
ASSIGN iButikNr = 14902???.

DEFINE BUFFER bCopyFromKassa FOR Kasse.

FOR EACH bCopyFromKassa WHERE bCopyFromKassa.ButikkNr = iButikNr NO-LOCK.
    FOR EACH butiker WHERE butiker.butik <> iButikNr:
        IF NOT CAN-FIND(gruppe WHERE gruppe.butikknr = butiker.butik AND gruppe.gruppenr = 1) THEN DO:
            CREATE gruppe.
            ASSIGN gruppe.butikknr = butiker.butik
                   gruppe.gruppenr = 1 
                   gruppe.navn     = "Gruppe 1" NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
                DELETE gruppe.
        END.
        IF NOT CAN-FIND(Kasse WHERE kasse.butikknr = butiker.butik AND Kasse.gruppenr = 1 AND kasse.kassenr = bCopyFromKassa.KasseNr) THEN DO:
            CREATE Kasse.
            BUFFER-COPY bCopyFromKassa EXCEPT bCopyFromKassa.ButikkNr TO Kasse
                ASSIGN Kasse.ButikkNr = butiker.butik NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
                DELETE Kasse.
        END.
        ELSE DO:
            FIND Kasse WHERE kasse.butikknr = butiker.butik AND Kasse.gruppenr = 1 AND kasse.kassenr = bCopyFromKassa.KasseNr NO-ERROR.
            IF AVAIL Kasse THEN DO:
                BUFFER-COPY bCopyFromKassa EXCEPT bCopyFromKassa.ButikkNr TO Kasse
                    ASSIGN Kasse.ButikkNr = butiker.butik NO-ERROR.
            END.
        END.
        IF AVAIL Kasse THEN DO:
            ASSIGN Kasse.Navn = REPLACE(Kasse.Navn,STRING(bCopyFromKassa.ButikkNr),STRING(Kasse.ButikkNr))
                   Kasse.ElJournal[2] = STRING(Kasse.ButikkNr)
                   Kasse.ElJournalId  = REPLACE(Kasse.ElJournalId,STRING(bCopyFromKassa.ButikkNr),STRING(Kasse.ButikkNr))
                   Kasse.ElJournalKatalog = REPLACE(Kasse.ElJournalKatalog,STRING(bCopyFromKassa.ButikkNr),STRING(Kasse.ButikkNr)).
            RELEASE kasse.
        END.
    END.
END. 
