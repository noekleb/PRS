DEF VAR cTekst AS CHAR NO-UNDO.
DEF VAR iOtype AS INT NO-UNDO.
DEF VAR lPris AS DEC NO-UNDO.

DEF BUFFER bufArtPris FOR ArtPris.

CURRENT-WINDOW:WIDTH = 350.
FOR EACH PkSdlHode NO-LOCK WHERE
    CAN-FIND(FIRST PkSdlLinje OF PkSdlHode WHERE 
             PkSdlLinje.ButikkNr = 40) AND
    PkSdlHode.PkSdlStatus = 20,
    EACH PkSdlPris OF PksdlHode NO-LOCK:

    IF NUM-ENTRIES(PkSdlHode.MeldingFraLEv,CHR(10)) > 1 THEN
    FIX_PRIS:
    DO:
        FIND ArtBas NO-LOCK WHERE
            ArtBas.ArtikkelNr = PkSdlPris.ArtikkelNr NO-ERROR.
        FIND ArtPris EXCLUSIVE-LOCK WHERE 
            ArtPris.ArtikkelNr = PkSdlPris.ArtikkelNr AND
            ArtPris.ProfilNr   = 1 NO-ERROR.
        FIND bufArtPris NO-LOCK WHERE 
            bufArtPris.ArtikkelNr = PkSdlPris.ArtikkelNr AND
            bufArtPris.ProfilNr   = 2 NO-ERROR.
        cTekst = ENTRY(1,PkSdlHode.MeldingFraLEv,CHR(10)).
        ASSIGN iOType = INT(ENTRY(2,cTekst,' ')).

        IF NOT CAN-DO('1,12',STRING(iOType)) THEN
            LEAVE FIX_PRIS.

        IF AVAILABLE bufArtPris AND ArtPris.Pris[1] = bufArtPris.Pris[1] AND 
            NUM-ENTRIES(STRING(ROUND(artPris.Pris[1] / 0.7,2)),',') = 1 THEN
        DO:
            DISPLAY
                ArtBas.ArtikkelNr
                ArtBas.Beskr
                ArtBas.LevKod
                ArtBas.LevFargKod
                IF AVAILABLE ArtPris THEN artPris.eDato ELSE ?
                IF AVAILABLE bufArtPris THEN bufartPris.eDato ELSE ?
                '|'
                PkSdlHode.PkSdlNr
                PkSdlHode.MeldingFraLev
                PkSdlPris.NyPris
                '|'
                IF AVAILABLE ArtPris THEN STRING(artPris.ProfilNr) ELSE ''
                IF AVAILABLE ArtPris THEN STRING(artPris.Pris[1]) ELSE '*'
                ROUND(artPris.Pris[1] / 0.7,2)
                IF NUM-ENTRIES(STRING(ROUND(artPris.Pris[1] / 0.7,2)),',') > 1 
                    THEN  ENTRY(2,STRING(ROUND(artPris.Pris[1] / 0.7,2)),',')
                    ELSE ''
                '|'
                IF AVAILABLE bufArtPris THEN STRING(bufartPris.ProfilNr) ELSE ''
                IF AVAILABLE bufArtPris THEN STRING(bufartPris.Pris[1]) ELSE '*'
            WITH WIDTH 350.

            ASSIGN
                lPris            = ROUND(artPris.Pris[1] / 0.7,2)
                ArtPris.Pris[1]  = lPris
                ArtPris.Mva%[1]  = 25
                ArtPris.MvaKr[1] = lPris - ROUND((lPris / (1 + (25 / 100))),2)
                ArtPris.DbKr[1]  = lPris - ArtPris.MvaKr[1] - ArtPris.VareKost[1]
                ArtPris.Db%[1]   = ROUND((ArtPris.DbKr[1] * 100) / (lPris - ArtPris.MvaKr[1]),2) 
            .

        END.
    END. /* FIX_PRIS */
END.
