/*
    På prisprofil 2 skal det være 40 rabatt i Rab1% og 
    30% rabatt på utpris.
*/

DEF VAR iAnt AS INT NO-UNDO.


DEF VAR lVareKost AS DEC FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEF VAR lPris     AS DEC FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEF VAR lMvaKr    AS DEC FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEF VAR lDbKr     AS DEC FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEF VAR lRab1Kr   AS DEC FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEF VAR lRab1%    AS DEC FORMAT "->>9.9"          NO-UNDO.


CURRENT-WINDOW:WIDTH = 350.
DEF BUFFER bufArtPris FOR ArtPris.

FOR EACH ArtPris EXCLUSIVE-LOCK WHERE 
    ProfilNr = 2 AND 
    ArtPris.Rab1%[1] = 0,
    FIRST bufArtPris  NO-LOCK WHERE 
        bufArtPris.ArtikkelNr = ArtPris.ArtikkelNr AND
        bufArtPris.ProfilNr   = 1 AND
        bufArtPris.Pris[1] = ArtPris.Pris[1]:

    iant = iAnt + 1.
    
    ASSIGN
        lPris     = ROUND((bufArtPris.Pris[1] * 70) / 100,2)
        lVareKost = ROUND((bufArtPris.InnkjopsPris[1] * 60) / 100,2)
        lMvaKr    = lPris - (lPris / (1 + (ArtPris.Mva%[1] / 100)))
        lDbKr     = lPris - lMvaKr - lVareKost    
        lRab1Kr   = ArtPris.Innkjopspris[1] - lVareKost
        .

    ASSIGN
        ArtPris.Rab1%[1]    = 40
        ArtPris.Rab1Kr[1]   = lRab1Kr
        ArtPris.VareKost[1] = lVareKost
        ArtPris.MvaKr[1]    = lMvaKr
        ArtPris.Pris[1]     = lPris
        .

    /*
    FIND ArtBas OF ArtPris NO-LOCK.
    DISPLAY
        ArtPris.ArtikkelNr
        ArtBas.Beskr
        ArtBas.LevKod
        ArtPris.Innkjopspris[1]
        ArtPris.Rab1%[1]
        ArtPris.MvaKr[1]
        ArtPris.Mva%[1]
        ArtPris.Pris[1]
        ArtPris.EDato
        '|'
        lVareKost
        lRab1%
        lPris
        '|'
        bufArtPris.Innkjopspris[1]
        bufArtPris.Rab1%[1]
        bufArtPris.Pris[1]
        bufArtPris.EDato
    WITH WIDTH 350.
    */
END.
/*
MESSAGE iAnt
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
*/    
