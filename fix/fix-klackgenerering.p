FOR EACH ArtBAs NO-LOCK:
    IF NOT CAN-FIND(klack WHERE
                    klack.klack-Id = artbas.klack) THEN
    DO:
        CREATE Klack.
        ASSIGN
            Klack.Klack-id     = ArtBas.Klack
            Klack.Beskrivning = ""
            .
    END.
END.
