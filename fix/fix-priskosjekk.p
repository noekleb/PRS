DEF VAR iLoop AS INT.
DEF VAR iAntPaa AS INT.
DEF VAR iAntAv  AS INT.
        
/*OUTPUT TO VALUE("PrisKoSjekk.txt").*/
LOOPEN:
FOR EACH ArtBas NO-LOCK 
    /*
    WHERE
    ArtBAs.Vg     = 1 AND
    ArtBas.LopNr >= 3000 AND
    ArtBas.LopNr <= 3999
    */
    :

    iLoop = 0.

    FIND ArtPris OF ArtBas NO-LOCK WHERE
        ArtPris.ProfilNr = 1.
    IF ArtPris.Tilbud = TRUE THEN
        NEXT LOOPEN.

    FOR EACH PrisKo OF ArtBas WHERE
        PrisKo.ProfilNr = 1 AND
        PrisKo.TYPE = 3 AND
        PrisKo.AktiveresDato >= TODAY:

        iLoop = iLoop + 1
            .

    END.

    IF iLoop = 1 THEN
    DO:
        FIND ArtPris OF ArtBas EXCLUSIVE-LOCK WHERE
            ArtPris.ProfilNr = 1.
        ASSIGN
            ArtPris.Tilbud = TRUE
            .
        /*
        MESSAGE ArtBas.Vg ArtBas.LopNr ArtPris.Tilbud
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        */
        /*
        EXPORT DELIMITER ";"
        ArtBas.Vg
        ArtBas.LopNr
        .
        */
    END.
END. /* LOOPEN */
/*OUTPUT CLOSE.*/
