DEF VAR lFlag AS LOG NO-UNDO.

CURRENT-WINDOW:WIDTH = 350.

FOR EACH ArtBas NO-LOCK WHERE 
    ArtBas.RegistrertDato >= 01/01/2017:
    ASSIGN 
        lFlag = FALSE.

    /* Flagger at det finnes bedriftsinterne strekkoder. */
    FOR EACH Strekkode OF ArtBas NO-LOCK:
        IF Strekkode.Kode BEGINS '02' AND 
            LENGTH(Strekkode.Kode) = 13 THEN
            lFlag = TRUE.
    END.

    /* slår av flagget hvis det finnes andre koder på artikkelen. */
    FOR EACH Strekkode OF ArtBas NO-LOCK:
        IF NOT Strekkode.Kode BEGINS '02' AND 
            LENGTH(Strekkode.Kode) = 13 THEN
            lFlag = FALSE.
    END.

    IF lFlag THEN
        DISPLAY
        ArtBas.ArtikkelNr
        ArtBas.Beskr
        ArtBas.LevFargKod
        ArtBas.LevKod
        ArtBas.RegistrertDato
        WITH WIDTH 350.
END.
