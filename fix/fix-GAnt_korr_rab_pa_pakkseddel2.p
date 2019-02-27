DEF VAR fMvaKr AS DEC NO-UNDO.
DEF VAR fNyDbKr  AS DEC NO-UNDO.
DEF VAR fNyPris AS DEC NO-UNDO.
DEF VAR fNyVareKost AS DEC NO-UNDO.
DEF VAR fNyRab% AS DEC NO-UNDO.
DEF VAR fNyDb% AS DEC NO-UNDO.
DEF VAR fInnkjopsPris AS DEC NO-UNDO.
DEF VAR fPris AS DEC NO-UNDO.

DEF VAR fMva%  AS DEC INITIAL 25 NO-UNDO.

CURRENT-WINDOW:WIDTH = 300.
FOR EACH PkSdlHode NO-LOCK WHERE
    PkSdlHode.PkSdlStatus = 10,
    FIRST PksdlLinje OF PkSdlHode NO-LOCK WHERE
    PkSdlLinje.ButikkNr = 10,
    EACH PkSdlPris OF PkSdlHode WHERE PkSdlPris.NyRab1% = 40:

    FIND FIRST ArtPris WHERE ArtPris.ArtikkelNR = PkSdlPris.ArtikkelNr.

    /*
                      fMvaKr          = PkSdlPris.NyPris - (PkSdlPris.NyPris / (1 + (lokArtPris.Mva%[1] / 100)))
                      fDbKr           = PkSdlPris.NyPris - fMvaKr - PkSdlPris.NyVarekost
                      PkSdlPris.NyDB% = ROUND((fDbKr * 100) / (PkSdlPris.NyVarekost + fDbKr),2)
                      PkSdlPris.NyDB% = IF PkSdlPris.NyDB% = ? THEN 0 ELSE PkSdlPris.NyDB%
    */

    ASSIGN
        fNyRab%  = 30
        fInnkjopsPris = IF ArtPris.InnkjopsPris[1] = 0 THEN PkSdlPris.NyInnkjopsPris ELSE ArtPris.InnkjopsPris[1]
        fNyVareKost =  fInnkjopsPris - ROUND((fInnkjopsPris * fNyRab%) / 100,2)
        fNyPris     = ArtPris.Pris[1] - ROUND((ArtPris.Pris[1] * fNyRab%) / 100,2)
        fMvaKr      = fNyPris - ROUND((fNyPris / (1 + (fMva% / 100))),2)
        fNyDbKr     = fNyPris - fMvaKr - fNyVareKost
        fNyDb%      = ROUND((fNyDbKr * 100) / (fNyPris - fMvaKr),2) 
        fNyDb%      = IF fNyDb% = ? THEN 0 ELSE fNyDb%
        .
/*    
DISPLAY
        /*
        pksdlhode.PkSdlStatus
        */
        PkSdlHode.PksdlNr FORMAT "x(8)"
        pkSdlLinje.ButikkNr
        PkSdlPris.ArtikkelNr
        PkSdlPris.Beskr FORMAT "x(10)"
        PkSdlPris.LevKod FORMAT "x(10)"
        /*
        PkSdlPris.LevFargKod
        PkSdlPris.InnkjopsPris
        PkSdlPris.Rab1%
        PkSdlPris.VareKost
        PkSdlPris.Db%
        PkSdlPris.Pris
        */
        '|'
        ArtPris.InnkjopsPris[1]
        ArtPris.Pris[1]
        '|'
        PkSdlPris.NyInnkjopsPris
        PkSdlPris.NyRab1%
        PkSdlPris.NyVareKost
        PkSdlPris.NyDb%
        PkSdlPris.NyPris
        '|'
        fInnkjopsPris
        fNyVareKost
        fNyRab%
        fNyVareKost
        fNyDb%
        fNyPris


        WITH WIDTH 300.
*/
    ASSIGN
        PkSdlPris.NyInnkjopsPris = fInnkjopsPris
        PkSdlPris.NyRab1%        = fNyRab%
        PkSdlPris.NyVareKost     = fNyVareKost
        PkSdlPris.NyDb%          = fNyDb%
        PkSdlPris.NyPris         = fNyPris
        .
END.
