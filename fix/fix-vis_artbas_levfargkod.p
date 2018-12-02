CURRENT-WINDOW:WIDTH = 350.
FOR EACH ArtBas WHERE 
    ArtBas.LevFargKod > '':

    IF NUM-ENTRIES(ArtBas.LevFargKod,'/') = 1 THEN
        NEXT.
    FIND Farg OF ArtBas NO-LOCK NO-ERROR.

    ArtBas.LevFargKod = ENTRY(1,ArtBas.LevFargKod,'/').
    DISPLAY
        ArtBas.ArtikkelNr
        ArtBas.Beskr
        ArtBas.LevFargKod
        Farg.FarBeskr
        ArtBas.Sasong
        ArtBas.EDato
        ArtBas.RegistrertDato
    WITH WIDTH 350.
END.
