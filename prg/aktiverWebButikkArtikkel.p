/* aktiverWebButikkArtikkel.p */

DEF INPUT PARAMETER lArtikkelNr LIKE ArtBas.ArtikkelNr NO-UNDO.

DO  TRANSACTION:
    FIND ArtBas EXCLUSIVE-LOCK WHERE
        ArtBas.ArtikkelNr = lArtikkelNr NO-ERROR.
    IF ArtBas.WebButikkArtikkel = FALSE THEN
        ASSIGN
            ArtBas.WebButikkArtikkel = TRUE
            ArtBas.Publiser          = TRUE.
    IF AVAILABLE ArtBas THEN
        RELEASE ArtBAs.
END. /* TRANSACTION */
