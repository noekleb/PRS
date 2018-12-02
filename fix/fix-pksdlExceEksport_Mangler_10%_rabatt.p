/* 
Legger på 10% rabatt på alle pakkseddel linjer som ikke har
dette fra før. Dvs. de linjene som står med 0% i rabatt. 
*/

CURRENT-WINDOW:WIDTH = 300.

DEF VAR fRab1% AS DEC NO-UNDO.
DEF VAR fMvaKr AS DEC NO-UNDO.
DEF VAR fDbKr  AS DEC NO-UNDO.
DEF VAR fDb%   AS DEC NO-UNDO.
DEF VAR fRabKr AS DEC NO-UNDO.
DEF VAR cButikkListe AS CHAR NO-UNDO.
DEF VAR piLoop AS INT NO-UNDO.
DEF VAR cFilNavn AS CHAR NO-UNDO.

DEF VAR fVareKost AS DEC NO-UNDO.

ASSIGN
    cFilNavn     = 'c:\appdir\pakkseddel_10%manko.csv'
    cButikkListe = '2,3,4,5,6,8,9,11,2,15'
    fRab1% = 10.

DEF STREAM Ut.

OUTPUT STREAM Ut TO VALUE(cFilNavn) NO-ECHO.

PUT STREAM Ut UNFORMATTED
    'PkSdlHode.SendtDato'
    ';PkSdlLinje.ButikkNr'
    ';PkSdlHode.PkSdlNr'
    ';PkSdlHode.EkstId'
    ';PkSdlPris.ArtikkelNr'
    ';PkSdlLinje.LevKod'
    ';PkSdlLinje.Beskr'
    ';PkSdlLinje.LevFargKod'
    ';Størrelse'
    ';PkSdlPris.NyInnkjopsPris'
    ';PkSdlPris.NyRab1%' 
    ';PkSdlPris.NyVareKost'
    ';PkSdlPris.NyDb%'
    ';PkSdlPris.NyPris'
    ';|'
    ';fRab1%'
    ';fVareKost'
    ';fDb%'
    ';Diff vareKost' 
    ';Antall'
    ';Tot Diff'
    SKIP.

BUTIKKER:
FOR EACH Butiker NO-LOCK WHERE 
    CAN-DO(cButikkListe,STRING(Butiker.Butik)):
    PAKKSEDDEL:
    FOR EACH PkSdlHode NO-LOCK WHERE
        PkSdlHode.PkSdlStatus = 20 AND 
        CAN-FIND(FIRST PkSdlLinje OF PkSdlHode WHERE PkSdlLinje.ButikkNr = Butiker.Butik),
        EACH PkSdlPris OF PkSdlHode EXCLUSIVE-LOCK WHERE
             PkSdlPris.NyRab1% = 0,
        FIRST ArtBas OF PkSdlPris NO-LOCK,
        FIRST ArtPris EXCLUSIVE-LOCK WHERE
              ArtPris.ArtikkelNr = pkSdlPris.ArtikkelNr,
        EACH PkSdlLinje OF PkSdlHode NO-LOCK WHERE
             PkSdlLinje.ArtikkelNr = PkSdlPris.ArtikkelNr,
        FIRST StrKonv OF PkSdlLinje NO-LOCK:


        ASSIGN
            fRabKr = ((PkSdlPris.NyInnkjopsPris * fRab1%) / 100)
            fVareKost = PkSdlPris.NyInnkjopsPris - ((PkSdlPris.NyInnkjopsPris * fRab1%) / 100)
            fMvaKr = PkSdlPris.NyPris - (PkSdlPris.NyPris / (1 + (ArtPris.Mva%[1] / 100)))
            fDbKr  = PkSdlPris.NyPris - fMvaKr - fVareKost                   
            fDB%   = ROUND((fDbKr * 100) / (PkSdlPris.NyPris - fMvaKr),2)
            fDB%   = IF fDB% = ? THEN 0 ELSE fDB%
            .
        
        /*
        DISPLAY
            PkSdlHode.SendtDato 
            PkSdlHode.PkSdlNr 
            /*PkSdlHode.EkstId*/
            PkSdlLinje.ButikkNr 
            PkSdlPris.ArtikkelNr 
            PkSdlLinje.LevKod
            PkSdlLinje.Beskr
            PkSdlLinje.LevFargKod
            PkSdlPris.NyInnkjopsPris
            /*PkSdlPris.NyRab1% */
            PkSdlPris.NyVareKost
            PkSdlPris.NyDb%
            PkSdlPris.NyPris
            '|'
            fRab1%
            fVareKost
            fDb%
            PkSdlPris.NyVareKost - fVareKost COLUMN-LABEL 'DIFF'
            WITH WIDTH 300.
        */

        /*
        ASSIGN
            PkSdlPris.NyRab1%    = fRab1%
            PkSdlPris.NyVareKost = fVareKost
            PkSdlPris.NyDb%      = fDb%
            .
        
        IF ArtPris.Rab1%[1] = 0 THEN
        DO:
            ASSIGN
                ArtPris.Rab1%[1]    = fRab1%
                ArtPris.Rab1Kr[1]   = fRabKr
                ArtPris.VareKost[1] = fVareKost
                ArtPris.DbKr[1]     = fDbKr
                ArtPRis.Db%[1]      = fDb%
                .
        END.
        */
        PUT STREAM Ut unformatted
            PkSdlHode.SendtDato ';'
            PkSdlLinje.ButikkNr ';'
            PkSdlHode.PkSdlNr ';'
            PkSdlHode.EkstId ';'
            PkSdlPris.ArtikkelNr ';'
            PkSdlLinje.LevKod ';'
            PkSdlLinje.Beskr ';'
            PkSdlLinje.LevFargKod ';'
            StrKonv.Storl ';'
            PkSdlPris.NyInnkjopsPris ';'
            PkSdlPris.NyRab1% ';' 
            PkSdlPris.NyVareKost ';'
            PkSdlPris.NyDb% ';'
            PkSdlPris.NyPris ';'
            '|' ';'
            fRab1% ';'
            fVareKost ';'
            fDb% ';'
            PkSdlPris.NyVareKost - fVareKost ';'
            PkSdlLinje.AntLevert ';'
            PkSdlLinje.AntLevert * (PkSdlPris.NyVareKost - fVareKost) 
            SKIP.


    END. /* PAKKSEDDEL */

END. /* BUTIKKER */

OUTPUT STREAM Ut CLOSE.
