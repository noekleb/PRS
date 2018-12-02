DEF VAR lInnkjopsPris AS DEC FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEF VAR lVareKost AS DEC FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEF VAR lPris     AS DEC FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEF VAR lMvaKr    AS DEC FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEF VAR lDbKr     AS DEC FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEF VAR lRab1Kr   AS DEC FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEF VAR lRab1%    AS DEC FORMAT "->>9.9"          NO-UNDO.
DEF VAR lDb%    AS DEC FORMAT "->>9.9"          NO-UNDO.

CURRENT-WINDOW:WIDTH = 350.

DEF BUFFER bufArtPris FOR ArtPris.


FOR EACH PkSdlHode EXCLUSIVE-LOCK WHERE
    PkSdlHode.PkSdlStatus = 10 AND
    PkSdlHode.PkSdlOpphav = 1 AND
    PkSdlHode.Merknad = 'Via fakturamodul',
    FIRST PkSdlLinje OF PkSdlHode NO-LOCK WHERE 
        CAN-DO('10,40',STRING(PkSdlLinje.butikkNr)),
    EACH PkSdlPris OF PkSdlHode EXCLUSIVE-LOCK:

    ASSIGN PkSdlHode.PkSdlOpphav = 5.

    IF PkSdlPris.NyRab1% <> 50 THEN
    DO:
        FIND ArtPris EXCLUSIVE-LOCK WHERE
            ArtPris.ArtikkelNr = PkSdlLinje.ArtikkelNr AND
            ArtPris.ProfilNr   = 2 NO-ERROR.

        ASSIGN
            lInnkjopsPris = PkSdlPris.NyInnkjopsPris
            lRab1%    = 50
            lPris     = PkSdlPris.NyPris
            lVareKost = ROUND((PkSdlPris.NyInnkjopsPris * lRab1%) / 100,2)
            lMvaKr    = lPris - (lPris / (1 + (25 / 100)))
            lDbKr     = lPris - lMvaKr - lVareKost    
            lRab1Kr   = PkSdlPris.NyInnkjopsPris - lVareKost
            lDB%      = ROUND((lDbKr * 100) / (lVareKost + lDbKr),2)
            .

        IF AVAILABLE ArtPris THEN
        DO:
            ASSIGN
                Artpris.Rab1Kr[1]   = lRab1Kr
                ArtPris.Rab1%[1]    = lRab1%
                Artpris.Varekost[1] = lVarekost
                ArtPris.DbKr[1]     = lDbKr
                ArtPris.Db%[1]      = lDb%
                ArtPris.MvaKr[1]    = lMvaKr
                .

        END.
        ASSIGN
            PkSdlPris.NyRab1%        = lRab1%
            PkSdlPris.NyVarekost     = lVareKost
            PkSdlPris.NyDb%          = lDb%
            .

        /*
        DISPLAY
                PkSdlHode.PkSdlNr
                PkSdlHode.PkSdlStatus        
                pkSdlHode.SendtDato
                PkSdlHode.Merknad FORMAT "x(10)"
                PkSdlLinje.butikkNr WHEN AVAILABLE PkSdlLinje
                /*PkSdlLinje.Beskr    WHEN AVAILABLE PkSdlLinje*/
            '|'
            PkSdlPris.NyInnkjopsPris  WHEN AVAILABLE PkSdlPris
            PkSdlPris.NyRab1%         WHEN AVAILABLE PkSdlPris
            PkSdlPris.NyVarekost      WHEN AVAILABLE PkSdlPris
            PkSdlPris.NyDb%          WHEN AVAILABLE PkSdlPris
            PkSdlPris.NyPris          WHEN AVAILABLE PkSdlPris        
            '|'
            lInnkjopsPris
            lRab1%   
            lVareKost
            lDb%    
            lPris    
        WITH WIDTH 350.
        */
    END.
END.
