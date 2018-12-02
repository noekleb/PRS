DEFINE BUFFER bArtbas FOR Artbas.

DEFINE TEMP-TABLE tt_artikkler NO-UNDO
    FIELD artikkelnr AS DECI FORMAT ">>>>>>>>>>>>9"
    INDEX artikkelnr artikkelnr.

    
FOR EACH artbas NO-LOCK:
    FOR EACH artlag WHERE artlag.artikkelnr = artbas.artikkelnr NO-LOCK:
        IF artlag.vg <> artbas.vg OR artlag.lopnr <> artbas.lopnr THEN DO:
            FIND bArtbas WHERE bArtbas.vg = artlag.vg AND bArtbas.lopnr = artlag.lopnr NO-LOCK NO-ERROR.
            IF AVAIL bArtBas THEN DO:
                IF NOT CAN-FIND(FIRST tt_artikkler WHERE tt_artikkler.artikkelnr = bartbas.artikkelnr) THEN DO:
                    CREATE tt_artikkler.
                    ASSIGN tt_artikkler.artikkelnr = bartbas.artikkelnr.
                END.
            END.
            ELSE DO:
                IF NOT CAN-FIND(FIRST tt_artikkler WHERE tt_artikkler.artikkelnr = artbas.artikkelnr) THEN DO:
                    CREATE tt_artikkler.
                    ASSIGN tt_artikkler.artikkelnr = artbas.artikkelnr.
                END.
            END.
        END.
    END.
    FOR EACH artlag WHERE artlag.vg = artbas.vg AND artlag.lopnr = artbas.lopnr NO-LOCK:
        IF artlag.artikkelnr <> artbas.artikkelnr THEN DO:
            IF NOT CAN-FIND(FIRST tt_artikkler WHERE tt_artikkler.artikkelnr = artlag.artikkelnr) THEN DO:
                CREATE tt_artikkler.
                ASSIGN tt_artikkler.artikkelnr = artlag.artikkelnr.
            END.
        END.
    END.
END.

FOR EACH tt_artikkler:
    RUN fix-lager-og-artlag.p (tt_artikkler.ArtikkelNR).
END.
/* FOR EACH tt_artikkler:                     */
/*     MESSAGE tt_artikkler.artikkelnr        */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK. */
/* END.                                       */

