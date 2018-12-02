 
 /*------------------------------------------------------------------------
    File        : HentTelleliste
    Purpose     : 
    Syntax      : 
    Description : Henter tellelister fra BxLogistics.
    Author(s)   : tny
    Created     : Fri Dec 29 11:16:10 CET 2017
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.


CLASS cls.BxLog.HentTelleliste: 

    DEFINE VARIABLE cSystemName AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cLogg   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE bOk     AS LOG       NO-UNDO.
    DEFINE VARIABLE bTest   AS LOG       NO-UNDO.
    DEFINE VARIABLE ctekst  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iLoop   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cDatoTid AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hDataset AS HANDLE NO-UNDO.
    DEFINE VARIABLE iMaksAntLister AS INTEGER NO-UNDO.
    DEFINE VARIABLE cKatalog AS CHARACTER NO-UNDO.

    DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.
    DEFINE VARIABLE rLesSkrivBxEngine AS cls.BxLog.LesSkrivBxEngine NO-UNDO.
/*    DEFINE VARIABLE rTelleliste AS cls.Telling.Telleliste NO-UNDO.*/
    
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    {cls\bxLog\tmpTblStockCount.i}
    {cls\BxLog\tmpDsStockCount.i}
        
    CONSTRUCTOR PUBLIC HentTelleliste ( INPUT pcLogg AS CHARACTER ):
        SUPER ().

        ASSIGN 
            cKatalog = 'konv\Tellefiler'
            cLogg = pcLogg
            bTest = TRUE
            iMaksAntLister = 10 /* Maks antall tellelsiter som hentes pr. oppslag */
            cDatoTid = REPLACE(STRING(TODAY),'/','') + REPLACE(STRING(TIME,"HH:MM:SS"),':','')
            .

        rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.
        rLesSkrivBxEngine = NEW cls.BxLog.LesSkrivBxEngine( INPUT cLogg ) NO-ERROR.
/*        rTelleliste  = NEW cls.Telling.Telleliste( INPUT cLogg ) NO-ERROR.*/

        cSystemName = rStandardFunksjoner:SystemName.

        rStandardFunksjoner:opprettKatalog(cKatalog).
        
        /* Kobler opp Sql server. */
        bOk = rLesSkrivBxEngine:oppkoblingSqlServer( ).

        /* Henter alle tilgjengelige tellelister (Henter maks 10 for hvert kall). */
        IF bOk THEN 
        EVIGHETEN:
        DO WHILE TRUE:
            EMPTY TEMP-TABLE tmpStockCount.

            /* Teller antall lister som blir hentet. */
            ASSIGN 
                iLoop = iLoop + 1
                bOk = FALSE 
                .

            /* Leser første ikke overførte telleliste inn i temp-tabell. */
            rLesSkrivBxEngine:lesStockCount( INPUT iMaksAntLister, 
                                         INPUT-OUTPUT DATASET dsStockCount ).        
    
            /* Er returen tom, avsluttes kjøringen. */
            IF NOT CAN-FIND(FIRST tmpStockCount) THEN 
                LEAVE EVIGHETEN.
            /* Ellers går vi videre og henter linjene i tellelistene, of flagger listene som hentet. */
            ELSE 
            HENT_LISTELINJER:
            DO:
                /* Henter listelinjene til alle mottatte lister. */
                rLesSkrivBxEngine:lesStockCountLine( INPUT-OUTPUT DATASET dsStockCount ).

                /* Posterer mottatte tellelister og flagger at tellelisten er mottatt. */
                /*                rTelleliste:posterTelleliste( INPUT-OUTPUT DATASET dsStockCount ).*/
                RUN cls\bxLog\hentTellelistePRSCl.p ( cLogg, INPUT-OUTPUT DATASET dsStockCount ).
                                
                /* Flagger listene som er mottatt som sendt. */
                rLesSkrivBxEngine:oppdStockCount( INPUT-OUTPUT DATASET dsStockCount ).
            END. /* HENT_LISTELINJER */

            IF bTest THEN 
            DO:
                cTekst = cKatalog + '\StockCount' + cDatoTid + '_' + STRING(iLoop) + '.JSon'.
                DATASET dsStockCount:WRITE-JSON('file', cTekst, TRUE).
                rStandardFunksjoner:SkrivTilLogg(cLogg,
                    '  Dataset skrevet til fil: ' + cTekst 
                    ).    
            END. 

            /* Sikrer at det ikke går i evig loop. */
            IF iLoop > 10 THEN 
                LEAVE EVIGHETEN.
        END. /* EVIGHETEN */

        /* Kobler ned SQL server. */
        bOk = rLesSkrivBxEngine:nedkoblingSqlServer(  ).
        IF NOT bOk THEN 
        DO:
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '  ** Feil ved nedkobling av Sql Server.' 
                ).    
            RETURN.
        END.
        
    END CONSTRUCTOR.

END CLASS.