DEF VAR iAnt AS INT NO-UNDO.
DEF VAR lPris AS DEC NO-UNDO.
DEFINE VARIABLE lRab% AS DECIMAL NO-UNDO.

DEF BUFFER bufartPris FOR ArtPris.

ASSIGN 
    lRab% = 0
    .

CURRENT-WINDOW:WIDTH = 350.
FOR EACH ArtPris EXCLUSIVE-LOCK WHERE 
    ArtPris.ProfilNr    = 16 AND 
    ArtPris.ArtikkelNr >= 0 AND
    ArtPris.Tilbud = TRUE AND 
    ArtPris.EDato = TODAY,
    EACH PrisKo EXCLUSIVE-LOCK WHERE
        PrisKo.ArtikkelNr = ArtPris.ArtikkelNr AND
        PrisKo.ProfilNr   = ArtPris.ProfilNr :

    FIND ArtBas OF ArtPris NO-LOCK NO-ERROR.


    IF ArtBas.LevKod <> '802201' THEN
        NEXT.
    IF ArtBas.LevFargKod <> '410' THEN
        NEXT.

    FIND bufArtPris NO-LOCK WHERE
        bufArtPris.ArtikkelNr = ArtPris.ArtikkelNr AND
        bufArtPris.ProfilNr   = ArtPris.ProfilNr NO-ERROR.

    ASSIGN 
        iant = iant + 1
        lPris = bufArtPris.Pris[1] - ((bufArtPris.Pris[1] * lRab%) / 100)
        .

    /*IF ArtPris.Pris[2] <> lPris THEN*/
    DO:
        
        DISPLAY
            ArtPris.Profilnr
            ArtPris.ArtikkelNr
            artPris.tilbud
            ArtBas.Beskr FORMAT "x(40)"
            ArtBas.LevKod
            ArtBas.LevFargKod
            ArtPris.EDato
            '|'
            ArtPris.tilbudFraDato
            ArtPris.tilbudtilDato
            ArtPris.Pris[2] COLUMN-LABEL 'tilb.pris'
            lPris            COLUMN-LABEL 'Pris'
            (ArtPris.Pris[2] - lPris) COLUMN-LABEL 'Avvik'
            '|'
            /*
            bufArtPris.tilbudFraDato WHEN AVAILABLE bufArtPris
            bufArtPris.tilbudtilDato WHEN AVAILABLE bufArtPris
            bufArtPris.Pris[1] WHEN AVAILABLE bufArtPris
            */
            PrisKo.Type
            PrisKo.AktiveresDato
            PrisKo.Tilbud

        WITH WIDTH 350.
        
        /* Korrigerer ArtPris.
        ASSIGN
            ArtPris.Pris[2]  = lPris
            ArtPris.Mva%[1]  = 25
            ArtPris.MvaKr[1] = lPris - ROUND((lPris / (1 + (25 / 100))),2)
            ArtPris.DbKr[1]  = lPris - ArtPris.MvaKr[1] - ArtPris.VareKost[1]
            ArtPris.Db%[1]   = ROUND((ArtPris.DbKr[1] * 100) / (lPris - ArtPris.MvaKr[1]),2) 
        NO-ERROR.
        */
        /*
        /* Korrigerer Priskø post. */
        ASSIGN
            PrisKo.Pris  = ArtPris.Pris[2]
            PrisKo.Mva%  = ArtPris.Mva%[1]
            Prisko.MvaKr = ArtPris.MvaKr[1]
            PrisKo.DbKr  = ArtPris.DbKr[1]
            PrisKo.Db%   = ArtPris.Db%[1]
        NO-ERROR.
        */
    END.

END.

MESSAGE iant
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
