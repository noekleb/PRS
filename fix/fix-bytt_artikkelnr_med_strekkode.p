/* Bytter artikkelnr med strekkodenr. */

DEF VAR lDummy AS DEC NO-UNDO.

DEF BUFFER bStrekkode FOR Strekkode.

FOR EACH Strekkode NO-LOCK WHERE
    Strekkode.ArtikkelNr > 0 AND
    LENGTH(Strekkode.Kode) >= 13
    BREAK BY Strekkode.ArtikkelNr:

    IF LAST-OF(Strekkode.ArtikkelNr) THEN
    DO:
        ASSIGN lDummy = DEC(Strekkode.Kode) NO-ERROR.
        IF NOT ERROR-STATUS:ERROR AND 
            NOT CAN-FIND(FIRST ArtBas WHERE ArtBas.ArtikkelNr = dec(Strekkode.Kode)) THEN
        DO:
            FOR EACH ArtBas EXCLUSIVE-LOCK WHERE
                ArtBas.ArtikkelNr = Strekkode.ArtikkelNr:
                ArtBas.ArtikkelNr = DEC(Strekkode.Kode).
            END.
            FOR EACH ArtPris EXCLUSIVE-LOCK WHERE
                ArtPris.ArtikkelNr = Strekkode.ArtikkelNr:
                ArtPris.ArtikkelNr = DEC(Strekkode.Kode).
            END.
            FOR EACH BongLinje EXCLUSIVE-LOCK WHERE
                BongLinje.ArtikkelNr = STRING(Strekkode.ARtikkelNr):
                BongLinje.ArtikkelNr = Strekkode.Kode.
            END.
            FOR EACH KampanjeTilbArt EXCLUSIVE-LOCK WHERE
                KampanjeTilbArt.KampTilbArtId = DEC(Strekkode.ArtikkelNr):
                KampanjeTilbArt.KampTilbArtId = DEC(STrekkode.Kode).
            END.
            FOR EACH bStrekkode EXCLUSIVE-LOCK WHERE
                bStrekkode.ArtikkelNr = Strekkode.ArtikkelNr:
                ASSIGN
                    bStrekkode.ArtikkelNr = DEC(Strekkode.Kode).
            END.
        END.
    END.
END.
