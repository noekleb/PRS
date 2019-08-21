DEFINE VARIABLE rArtPrisKalkyle AS cls.Artikkel.ArtPrisKalkyle NO-UNDO.

DEF VAR iFraProfilNr AS INT NO-UNDO.
DEF VAR iTilProfilNr AS INT NO-UNDO.
DEF VAR cLogg AS CHAR NO-UNDO.

DEF BUFFER bufArtPris FOR ArtPris.

rArtPrisKalkyle  = NEW cls.Artikkel.ArtPrisKalkyle( cLogg ) NO-ERROR.

ASSIGN 
    iFraProfilNr = 16
    itilProfilNr = 1
    cLogg = 'fix-ArtPris.cls' + REPLACE(STRING(TODAY),'/','')
    .

FIND ArtPris NO-LOCK WHERE 
    ArtPris.ArtikkelNr = 9855960 AND 
    ArtPris.ProfilNr   = iFraProfilNr NO-ERROR.

IF AVAILABLE ArtPris THEN
DO:
    IF CAN-FIND(bufArtPris WHERE 
                bufArtPris.ArtikkelNr =  ArtPris.ArtikkelNr AND 
                bufArtPris.ProfilNr = iTilProfilNr) THEN
        rArtPrisKalkyle:KopierArtPris(ArtPris.ArtikkelNr, iFraProfilNr, iTilProfilNr).
    ELSE 
        rArtPrisKalkyle:NyArtPris(ArtPris.ArtikkelNr, iFraProfilNr, iTilProfilNr).
END.


