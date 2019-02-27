CURRENT-WINDOW:WIDTH = 350.
FOR EACH ArtBas EXCLUSIVE-LOCK WHERE RegistrertDato >= 01/01/2000:
    IF NUM-ENTRIES(ArtBas.LevFargKod,'/') > 1 THEN
    DISPLAY
        ArtikkelNr
        Beskr
        LevKod
        LevFargKod
        ENTRY(1,ArtBas.LevFargKod,'/')

        WITH WIDTH 350.
    ArtBas.LevFargKod = ENTRY(1,ArtBas.LevFargKod,'/').
END.
