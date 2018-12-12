 
 /*------------------------------------------------------------------------
    File        : HentArticle
    Purpose     : 
    Syntax      : 
    Description : Henter artikkeldata fra UNI2.
    Author(s)   : tny
    Created     : Fri oct 8 11:16:10 CET 2018
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.


CLASS cls.UNI2.HentUNI2Data: 

    DEFINE VARIABLE cSystemName AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cLogg   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE bOk     AS LOG       NO-UNDO.
    DEFINE VARIABLE bTest   AS LOG       NO-UNDO.
    DEFINE VARIABLE ctekst  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDatoTid AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hDataset AS HANDLE NO-UNDO.
    DEFINE VARIABLE cKatalog AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cEDB-System        LIKE ImpKonv.EDB-System NO-UNDO.

    DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.
    DEFINE VARIABLE rLesSkrivUNI2 AS cls.UNI2.LesSkrivUNI2 NO-UNDO.
    
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    {cls\UNI2\tmpTblvArticle_NO.i}
    {cls\UNI2\tmpDsvArticle_NO.i}
    {cls\UNI2\tmpTblvArticles.i}
    {cls\UNI2\tmpDsvArticles.i}
    {cls\UNI2\tmpTblSeasons.i}
    {cls\UNI2\tmpDsSeasons.i}
    {cls\UNI2\tmpTblvSupplier.i}
    {cls\UNI2\tmpDsvSupplier.i}
    {cls\UNI2\tmpTblregArticles.i}
    {cls\UNI2\tmpDsregArticles.i}
    {cls\UNI2\tmpTblregArtSKU.i}
    {cls\UNI2\tmpDsregArtSKU.i}
    {cls\UNI2\tmpTblregEanSKU.i}
    {cls\UNI2\tmpDsregEanSKU.i}
    {cls\UNI2\tmpTblArtEan.i}
    {cls\UNI2\tmpDsArtEan.i}
        
    CONSTRUCTOR PUBLIC HentUNI2Data ( INPUT pcLogg AS CHARACTER ):
        SUPER ().

        ASSIGN  
            cKatalog = 'konv\Article'
            cLogg    = pcLogg
            bTest    = TRUE
            cDatoTid = REPLACE(STRING(TODAY),'/','') + REPLACE(STRING(TIME,"HH:MM:SS"),':','')
            cEDB-System = 'UNI2'
            .

        rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.
        rLesSkrivUNI2 = NEW cls.UNI2.LesSkrivUNI2( INPUT cLogg ) NO-ERROR.

        cSystemName = rStandardFunksjoner:SystemName.

        rStandardFunksjoner:opprettKatalog(cKatalog).
        

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC VOID byggtmpArtEan( OUTPUT DATASET dsArtEan ):
        
        EMPTY TEMP-TABLE tmpArtEan.
        
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            '  Start byggtmpArtEan().' 
            ).    
        FOR EACH tmpregArticles, 
            EACH tmpregArtSKU WHERE 
                tmpregArtSKU.nArtKey = tmpregArticles.nArtKey,
            EACH tmpregEanSku WHERE 
                tmpregEanSku.nSKU = tmpregArtSKU.nSKU
                BY tmpregArticles.nArtKey
                BY tmpregArticles.cArtNo
                BY tmpregArtSKU.nArtKey:
                
                 
            IF NOT CAN-FIND(FIRST tmpArtEan WHERE 
                            tmpArtEan.nArtKey = tmpregArticles.nArtkey AND 
                            tmpArtEan.cArtNo  = tmpregArticles.cArtNo AND 
                            tmpArtEan.nEan    = tmpregEanSKU.nEan) THEN 
            DO:
                FIND FIRST tmpvArticles WHERE 
                    tmpvArticles.nArtKey = tmpregArticles.nArtkey AND  
                    tmpvArticles.cArtNo  = tmpregArticles.cArtNo
                    NO-ERROR.
                CREATE tmpArtEan.
                ASSIGN 
                    tmpArtEan.nArtKey = tmpregArticles.nArtkey 
                    tmpArtEan.cArtNo  = tmpregArticles.cArtNo 
                    tmpArtEan.nEan    = tmpregEanSKU.nEan
                    tmpArtEan.nSKU    = tmpregEanSKU.nSKU 
                    .
                IF AVAILABLE tmpvArticles THEN 
                    ASSIGN 
                        tmpArtEan.dPriceFOB       = tmpvArticles.dPriceFOB          
                        tmpArtEan.dPriceLC        = tmpvArticles.dPriceLC       
                        tmpArtEan.dPriceWholesale = tmpvArticles.dPriceWholesale 
                        tmpArtEan.dPriceRetail    = tmpvArticles.dPriceRetail   
                        .
            END.  
        END.        

        rStandardFunksjoner:SkrivTilLogg(cLogg,
            '  Ferdig byggtmpArtEan().' 
            ).    
        
        IF bTest THEN 
        DO:
            cDatoTid = REPLACE(STRING(TODAY),'/','') + REPLACE(STRING(TIME,"HH:MM:SS"),':','').
            cTekst = cKatalog + '\ArtEan' + cDatoTid + '.JSon'.
            DATASET dsArtEan:WRITE-JSON('file', cTekst, TRUE).
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '  Dataset skrevet til fil fra byggtmpArtEan(): ' + cTekst 
                ).    
        END.
        
        RETURN.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC VOID hentvArticlesData(  OUTPUT DATASET dsvArticles ):
        EMPTY TEMP-TABLE tmpvArticles.

        /* Henter hele tabellen fra SQL basen og legger den i en temp-tabell. */
        rLesSkrivUNI2:lesvArticles( OUTPUT DATASET dsvArticles ).

        IF bTest THEN 
        DO:
            cDatoTid = REPLACE(STRING(TODAY),'/','') + REPLACE(STRING(TIME,"HH:MM:SS"),':','').
            cTekst = cKatalog + '\vArticles' + cDatoTid + '.JSon'.
            DATASET dsvArticles:WRITE-JSON('file', cTekst, TRUE).
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '  Dataset skrevet til fil fra hentvArticlesData(): ' + cTekst 
                ).    
        END.
        
        RETURN.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC VOID hentvArticles_NOData( OUTPUT DATASET dsvArticle_NO ):
        EMPTY TEMP-TABLE tmpvArticle_NO.
  
        /* Henter hele tabellen fra SQL basen og legger den i en temp-tabell. */
        rLesSkrivUNI2:lesvArticles_NO( OUTPUT DATASET dsvArticle_NO ).

        IF bTest THEN 
        DO:
            cDatoTid = REPLACE(STRING(TODAY),'/','') + REPLACE(STRING(TIME,"HH:MM:SS"),':','').
            cTekst = cKatalog + '\Articles_NO' + cDatoTid + '.JSon'.
            DATASET dsvArticle_NO:WRITE-JSON('file', cTekst, TRUE).
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '  Dataset skrevet til fil hentvArticles_NOData(): ' + cTekst 
                ).    
        END.
        
        RETURN.

    END METHOD.


    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC VOID hentregArtSKUData( OUTPUT DATASET dsregArtSKU ):
        EMPTY TEMP-TABLE tmpregArtSKU.

        /* Henter hele tabellen fra SQL basen og legger den i en temp-tabell. */
        rLesSkrivUNI2:lesregArtSKU( OUTPUT DATASET dsregArtSKU ).
        IF bTest THEN 
        DO:
            cDatoTid = REPLACE(STRING(TODAY),'/','') + REPLACE(STRING(TIME,"HH:MM:SS"),':','').
            cTekst = cKatalog + '\regArtSKU' + cDatoTid + '.JSon'.
            DATASET dsregArtSKU:WRITE-JSON('file', cTekst, TRUE).
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '  Dataset skrevet til fil fra hentregArtSKUData(): ' + cTekst 
                ).    
        END.
        
        RETURN.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC VOID hentregEanSKUData( OUTPUT DATASET dsregEanSKU ):
        EMPTY TEMP-TABLE tmpregEanSKU.

        /* Henter hele tabellen fra SQL basen og legger den i en temp-tabell. */
        rLesSkrivUNI2:lesregEanSKU( OUTPUT DATASET dsregEanSKU ).

        IF bTest THEN 
        DO:
            cDatoTid = REPLACE(STRING(TODAY),'/','') + REPLACE(STRING(TIME,"HH:MM:SS"),':','').
            cTekst = cKatalog + '\regEanSKU' + cDatoTid + '.JSon'.
            DATASET dsregEanSKU:WRITE-JSON('file', cTekst, TRUE).
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '  Dataset skrevet til fil fra hentregEanSKUData(): ' + cTekst 
                ).    
        END.
        
        RETURN.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:  12/12-18 Denne rutinen kalles ikke lenger. Manglende felt er 
                      nå lagt inn i vArticles_NO.
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC VOID hentregArticlesData( OUTPUT DATASET DsregArticles ):
        
        EMPTY TEMP-TABLE tmpregArticles.

        /* Henter hele tabellen fra SQL basen og legger den i en temp-tabell. */
        rLesSkrivUNI2:lesregArticles( OUTPUT DATASET dsregArticles ).

        IF bTest THEN 
        DO:
            cDatoTid = REPLACE(STRING(TODAY),'/','') + REPLACE(STRING(TIME,"HH:MM:SS"),':','').
            cTekst = cKatalog + '\regArtticles' + cDatoTid + '.JSon'.
            DATASET dsregArticles:WRITE-JSON('file', cTekst, TRUE).
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '  regArticles Dataset skrevet til fil fra hentregArticlesData(): ' + cTekst 
                ).    
        END.

        RETURN.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

/*    METHOD PUBLIC VOID hentregArtSupplierData( OUTPUT DATASET dsregArtSupplier  ):             */
/*        EMPTY TEMP-TABLE tmpregArtSupplier.                                                    */
/*                                                                                               */
/*        /* Henter hele tabellen fra SQL basen og legger den i en temp-tabell. */               */
/*        rLesSkrivUNI2:lesregArtSupplier( OUTPUT DATASET dsregArtSupplier ).                    */
/*                                                                                               */
/*        IF bTest THEN                                                                          */
/*        DO:                                                                                    */
/*            cDatoTid = REPLACE(STRING(TODAY),'/','') + REPLACE(STRING(TIME,"HH:MM:SS"),':','').*/
/*            cTekst = cKatalog + '\regArtSupplier' + cDatoTid + '.JSon'.                        */
/*            DATASET dsregArtSupplier:WRITE-JSON('file', cTekst, TRUE).                         */
/*            rStandardFunksjoner:SkrivTilLogg(cLogg,                                            */
/*                '  regArtSupplier Dataset skrevet til fil fra oppdaterLC(): ' + cTekst         */
/*                ).                                                                             */
/*        END.                                                                                   */
/*                                                                                               */
/*        RETURN.                                                                                */
/*                                                                                               */
/*    END METHOD.                                                                                */

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC VOID hentSeasonData( OUTPUT DATASET dsSeasons ):
        EMPTY TEMP-TABLE tmpSeasons.

        /* Henter hele tabellen fra SQL basen og legger den i en temp-tabell. */
        rLesSkrivUNI2:lesSeasons( OUTPUT DATASET dsSeasons ).

        IF bTest THEN 
        DO:
            cDatoTid = REPLACE(STRING(TODAY),'/','') + REPLACE(STRING(TIME,"HH:MM:SS"),':','').
            cTekst = cKatalog + '\vSeason' + cDatoTid + '.JSon'.
            DATASET dsSeasons:WRITE-JSON('file', cTekst, TRUE).
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '  Dataset skrevet til fil fra hentSeasonData(): ' + cTekst 
                ).    
        END.
         
        RETURN.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC VOID hentvSupplierData( OUTPUT DATASET DsvSupplier ):
        EMPTY TEMP-TABLE tmpvSupplier.

        /* Henter hele tabellen fra SQL basen og legger den i en temp-tabell. */
        rLesSkrivUNI2:lesvSupplier( OUTPUT DATASET DsvSupplier ).

        IF bTest THEN 
        DO:
            cDatoTid = REPLACE(STRING(TODAY),'/','') + REPLACE(STRING(TIME,"HH:MM:SS"),':','').
            cTekst = cKatalog + '\vSupplier' + cDatoTid + '.JSon'.
            DATASET dsvSupplier:WRITE-JSON('file', cTekst, TRUE).
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '  Dataset skrevet til fil fra hentvSupplierData(): ' + cTekst 
                ).    
        END.
        
        RETURN.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC VOID nedkoblingSQL( OUTPUT pbOk AS LOG ):

        /* Kobler ned SQL server. */
        pbOk = rLesSkrivUNI2:nedkoblingSqlServer(  ).
        IF NOT pbOk THEN 
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '  ** Feil ved nedkobling av Sql Server.' 
                ).
        ELSE     
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '  SQL server nedkoblet.' 
                ).    

        
        RETURN.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose: NB: Brukes ikke foreløpig.....
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC VOID oppdaterLC( INPUT DATASET dsvArticle_NO ):

        DEFINE VARIABLE piAntLC AS INTEGER NO-UNDO.
        DEFINE VARIABLE piAntSasong AS INTEGER NO-UNDO.
        
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            '  Start oppdater LC.'
            ).    

        FOR EACH tmpvArticle_NO WHERE
            tmpvArticle_NO.cArtno   >= '' AND 
            tmpvArticle_NO.nColCode >= 0 AND 
            tmpvArticle_NO.nSeason  >= 0 AND 
            tmpvArticle_NO.dtLastChanged >= DATETIME(TODAY - 30)
            USE-INDEX idxBreakBy
            BREAK BY tmpvArticle_NO.cArtno
                  BY tmpvArticle_NO.nColCode
                  BY tmpvArticle_NO.nSeason:
                      
            /* Endrer bare på siste sesong. Vi skal ha den siste sesongen i tabell v_articles_NO */
            /* Den tabellen synkes når det sendes ordre til PRS bla.                             */
            IF LAST-OF(tmpvArticle_NO.nSeason) THEN 
            BRYTPUNKT:
            DO:
                FIND StrekKode NO-LOCK WHERE 
                    StrekKode.Kode = tmpvArticle_NO.cEAN NO-ERROR.
                IF AVAILABLE StrekKode THEN 
                DO TRANSACTION:
                    FIND ArtBas OF StrekKode EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
                    IF AVAILABLE ArtBas AND NOT LOCKED ArtBas THEN 
                    DO:
                        IF tmpvArticle_NO.dPriceLC > 0 AND ArtBas.KjedeInnkPris <> tmpvArticle_NO.dPriceLC THEN
                        DO:                     
                            rStandardFunksjoner:SkrivTilLogg(cLogg,
                                '  Endret LC på artikkel ' + StrekKode.Kode + ' ' + ArtBas.LevKod + ' ' + ArtBas.Beskr + ' ' + ArtBas.LevFargKod + 
                                ' fra ' + STRING(ArtBas.KjedeInnkPris) + ' til ' + STRING(tmpvArticle_NO.dPriceLC) + '.'
                                ).    
                            ASSIGN 
                                piAntLC               = piAntLC + 1
                                ArtBas.KjedeInnkPris = tmpvArticle_NO.dPriceLC
                                .
                        END.
                        
                        IF ArtBas.SaSong <> tmpvArticle_NO.nSeason THEN
                        DO:
                            rStandardFunksjoner:SkrivTilLogg(cLogg,
                                '  Endret Sesong på artikkel ' + StrekKode.Kode + ' ' + ArtBas.LevKod + ' ' + ArtBas.Beskr + ' ' + ArtBas.LevFargKod + 
                                ' fra ' + STRING(ArtBas.Sasong) + ' til ' + STRING(tmpvArticle_NO.nSeason) + '.'
                                ).    
                            
                            ASSIGN 
                                piAntSasong   = piAntSasong + 1
                                ArtBas.SaSong = tmpvArticle_NO.nSeason
                                .
                        END.
                        
                        IF TRIM(ArtBas.Lokasjon) = '' AND tmpvArticle_NO.MadeInCountry <> '' THEN
                        DO: 
                            rStandardFunksjoner:SkrivTilLogg(cLogg,
                                '  Lagt inn opphavsland på artikkel ' + StrekKode.Kode + ' ' + ArtBas.LevKod + ' ' + ArtBas.Beskr + ' ' + ArtBas.LevFargKod + 
                                ' fra ' + STRING(ArtBas.Lokasjon) + ' til ' + STRING(tmpvArticle_NO.MadeInCountry) + '.'
                                ).    
                            ArtBas.Lokasjon = tmpvArticle_NO.MadeInCountry. 
                        END.
    
                        /* Logger mapping av artikkel og vSuppliers til ImpKonv. */
                        setImpKonvSupplier().                     
                            
                        /* Logger mapping av artikkel og vSeasons til ImpKonv. */
                        setImpKonvSeasons().    
                            
                        RELEASE ArtBas.
                    END.
                END. /* TRANSACTION */
            END. /* BRYTPUNKT */
        END.

        rStandardFunksjoner:SkrivTilLogg(cLogg,
            '  Ferdig oppdater LC. Antall endret LC: ' + STRING(piAntLC) + ' Antall endret sesong: ' + STRING(piAntSasong) + '.'
            ).    
        
        RETURN.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC VOID oppdaterSeasons( INPUT DATASET DsSeasons ):
        
        /* Oppdaterer alltid med siste sesongens navn. */
        FOR EACH tmpSeasons TRANSACTION
                BREAK BY tmpSeasons.nSeason:
            IF LAST-OF(tmpSeasons.nSeason) THEN 
            DO:
                FIND SaSong EXCLUSIVE-LOCK WHERE
                    SaSong.Sasong = nSeason NO-ERROR.
                IF NOT AVAILABLE SaSong THEN 
                DO:
                    CREATE SaSong.
                    ASSIGN 
                        SaSong.Sasong = tmpSeasons.nSeason
                        .
                END. 
                ASSIGN 
                    SaSong.SasBeskr = tmpSeasons.cSeasonName 
                    .
            END.
        END. /* TRANSACTION */
        
        RETURN.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC VOID oppkoblingSQL( OUTPUT pbOk AS LOG ):

        /* Kobler opp Sql server. */
        pbOk = rLesSkrivUNI2:oppkoblingSqlServer( ).
        IF NOT pbOk THEN 
        DO:
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '  ** Feil ved oppkobling av Sql Server.' 
                ).    
            RETURN.
        END.
        ELSE 
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '  SQL server oppkoblet.' 
                ).    
        
        RETURN.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC VOID setImpKonvSeasons(  ):
        DEFINE VARIABLE cImpTabell AS CHARACTER NO-UNDO.
        
        ASSIGN 
            cImpTabell = 'vSeasons'
            .
        IF AVAILABLE ImpKonv THEN 
            RELEASE ImpKonv.
            
        FIND FIRST ImpKonv NO-LOCK WHERE 
            ImpKonv.EDB-System = cEDB-System AND 
            ImpKonv.Tabell     = cImpTabell AND 
            ImpKonv.InterntID  = tmpvArticle_NO.cEAN AND  
            ImpKonv.EksterntId = STRING(tmpvArticle_NO.nSeason)
            NO-ERROR.

        IF NOT AVAILABLE ImpKonv THEN 
        UNI2: 
        DO:
            CREATE ImpKonv.
            ASSIGN 
                ImpKonv.EDB-System = cEDB-System
                ImpKonv.Tabell     = cImpTabell
                ImpKonv.InterntID  = tmpvArticle_NO.cEAN
                ImpKonv.EksterntId = STRING(tmpvArticle_NO.nSeason)
                ImpKonv.Merknad    = tmpvArticle_NO.cSeasonName 
                .
        END. /* UNI2 */
        
        RETURN.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC VOID setImpKonvSupplier(  ):
        DEFINE VARIABLE cImpTabell AS CHARACTER NO-UNDO.
        
        ASSIGN 
            cImpTabell = 'vSupplier'
            .
        IF AVAILABLE ImpKonv THEN 
            RELEASE ImpKonv.
            
        FIND FIRST ImpKonv NO-LOCK WHERE 
            ImpKonv.EDB-System = cEDB-System AND 
            ImpKonv.Tabell     = cImpTabell AND 
            ImpKonv.InterntID  = tmpvArticle_NO.cEAN AND  
            ImpKonv.EksterntId = STRING(tmpvArticle_NO.nSeason) + '|' + STRING(tmpvArticle_NO.nSupplierCode)
            NO-ERROR.

        IF NOT AVAILABLE ImpKonv THEN 
        UNI2: 
        DO:
            FIND FIRST tmpvSupplier WHERE 
                tmpvSupplier.nSeason       = tmpvArticle_NO.nSeason AND  
                tmpvSupplier.nSupplierCode = tmpvArticle_NO.nSupplierCode 
                NO-ERROR.
            IF NOT AVAILABLE tmpvSupplier THEN 
                FIND FIRST tmpvSupplier WHERE 
                    tmpvSupplier.nSupplierCode = tmpvArticle_NO.nSupplierCode 
                    NO-ERROR.
            CREATE ImpKonv.
            ASSIGN 
                ImpKonv.EDB-System = cEDB-System
                ImpKonv.Tabell     = cImpTabell
                ImpKonv.InterntID  = tmpvArticle_NO.cEAN
                ImpKonv.EksterntId = STRING(tmpvArticle_NO.nSeason) + '|' + STRING(tmpvArticle_NO.nSupplierCode)
                ImpKonv.Merknad    = IF tmpvArticle_NO.cSupplierName <> '' 
                                        THEN tmpvArticle_NO.cSupplierName 
                                        ELSE '* Ukjent regSupplier'
/*                ImpKonv.Merknad    = IF AVAILABLE tmpvSupplier         */
/*                                        THEN tmpvSupplier.cSupplierName*/
/*                                        ELSE '* Ukjent regSupplier'    */
                .
        END. /* UNI2 */
        
        RETURN.

    END METHOD.

    DESTRUCTOR PUBLIC HentUNI2Data ( ):

    END DESTRUCTOR.
END CLASS.