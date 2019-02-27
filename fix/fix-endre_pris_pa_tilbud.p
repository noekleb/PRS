DEF VAR lPris AS DEC FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEF VAR lMvaKr AS DEC FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEF VAR lDbKr AS DEC FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEF VAR lDb% AS DEC FORMAT "->>,>>9.9" NO-UNDO.
DEF VAR iant AS INT NO-UNDO.

CURRENT-WINDOW:WIDTH = 350.

FOR EACH ArtPris EXCLUSIVE-LOCK  WHERE
    ArtPris.ProfilNr = 16 AND
    ArtPris.Tilbud = TRUE:

    FIND FIRST PrisKo WHERE
        PrisKo.ArtikkelNr = ArtPris.ArtikkelNr AND
        PrisKo.ProfilNr = ArtPris.ProfilNr AND
        PrisKo.TYPE     = 3 AND 
        PrisKo.tilbud = TRUE
        NO-ERROR.

    IF AVAILABLE PrisKo THEN
    DO:
        ASSIGN
            iAnt = iAnt + 1
            lPris  = (ArtPris.Pris[1] * 70) / 100
            lMvaKr = lPris - ROUND((lPris / (1 + (25 / 100))),2)
            lDbKr  = lPris - lMvaKr - ArtPris.VareKost[1]
            lDb%   = ROUND((lDbKr * 100) / (lPris - lMvaKr),2) 
            .
        /*----
        ASSIGN
            ArtPris.tilbud = TRUE
            ArtPris.Pris[2]  = lPris
            ArtPris.MvaKr[2] = lMvaKr
            ArtPris.DbKr[2]  = lDbKr
            ArtPris.Db%[2]   = lDb%
            ArtPris.TilbudTilDAto = 09/30/2018
            PrisKo.aktiveresDato  = 09/30/2018
            PrisKo.gyldigtilDato = 09/30/2018
            .
        ----*/
        
        DISPLAY
            ArtPris.ArtikkelNr
            ArtPris.Tilbud
            ArtPris.Varekost[1]
            ArtPris.DbKr[1]
            ArtPris.Db%[1]
            ArtPris.MvaKr[1]
            ArtPris.Mva%[1]
            ArtPris.Pris[1]
            ArtPris.Pris[2]
            '|'
            lDbKr
            lDb%
            lMvaKr
            lPris
            ArtPris.TilbudFraDato 
            ArtPris.TilbudtilDato
            /*
            '|'
            PrisKo.TYPE WHEN AVAILABLE PrisKo
            PrisKo.tilbud WHEN AVAILABLE PrisKo
            */
        WITH WIDTH 350.
        
    END.

END.
MESSAGE iAnt
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
