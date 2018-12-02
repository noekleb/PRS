/* flaggArtBasWebArtikkel.p */

DEF INPUT PARAMETER lArtikkelNr LIKE ArtBas.ArtikkelNr NO-UNDO.

DO FOR ArtBas TRANSACTION:
    FIND ArtBas EXCLUSIVE-LOCK WHERE 
        ArtBas.ArtikkelNr = lArtikkelNr NO-ERROR NO-WAIT.
    IF AVAILABLE ArtBas AND NOT LOCKED ArtBas THEN
        ASSIGN
        ArtBas.WebButikkArtikkel = TRUE
        .
    IF AVAILABLE ArtBas THEN
        RELEASE ArtBas.
END.
