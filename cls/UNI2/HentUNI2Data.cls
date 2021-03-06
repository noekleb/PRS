 
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
    DEFINE VARIABLE iX AS INTEGER NO-UNDO.
    DEFINE VARIABLE cVg AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cEDB-System        LIKE ImpKonv.EDB-System NO-UNDO.

    DEFINE BUFFER bufArtBas FOR ArtBas.
    
    DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.
    DEFINE VARIABLE rLesSkrivUNI2 AS cls.UNI2.LesSkrivUNI2 NO-UNDO.

    /* Endringer her skal ikke utl�se ny ELogg post og resending av ordre. */    
    ON CREATE OF ArtBas OVERRIDE DO: END.
    ON WRITE  OF ArtBas OVERRIDE DO: END.
    ON CREATE OF ArtBas OVERRIDE DO: END.
    
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
            bTest    = FALSE
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

  METHOD PUBLIC VOID hentvArticles_NOVarGrData( INPUT pcVgLst AS CHARACTER,
                                                OUTPUT DATASET dsvArticle_NO ):
        EMPTY TEMP-TABLE tmpvArticle_NO.
  
        /* Henter hele tabellen fra SQL basen og legger den i en temp-tabell. */
        rLesSkrivUNI2:lesvArticles_NOVarGr( INPUT pcVgLst, OUTPUT DATASET dsvArticle_NO ).

        IF bTest THEN 
        DO:
            cDatoTid = REPLACE(STRING(TODAY),'/','') + REPLACE(STRING(TIME,"HH:MM:SS"),':','').
            cTekst = cKatalog + '\Articles_NOVarGrData' + cDatoTid + '.JSon'.
            DATASET dsvArticle_NO:WRITE-JSON('file', cTekst, TRUE).
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '  Dataset skrevet til fil hentvArticles_NOVarGrData(): ' + cTekst 
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
            cTekst = cKatalog + '\Articles_NOData' + cDatoTid + '.JSon'.
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
                      n� lagt inn i vArticles_NO.
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
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC VOID oppdaterHovedGr(  ):

    FIND HuvGr EXCLUSIVE-LOCK WHERE 
      HuvGr.Hg = tmpvArticle_NO.nMainGroup NO-ERROR.
    IF NOT AVAILABLE HuvGr AND NOT LOCKED HuvGr THEN 
    DO:
      CREATE HuvGr.
      ASSIGN 
        HuvGr.Hg         = tmpvArticle_NO.nMainGroup
        HuvGr.HgBeskr    = tmpvArticle_NO.cMainGroup
        HuvGr.AvdelingNr = IF tmpvArticle_NO.cArtGroup = 'HOME' THEN 2 ELSE 1
        .  
      rStandardFunksjoner:SkrivTilLogg(cLogg,
          '  Ny hovedgruppe: ' + STRING(tmpvArticle_NO.nMainGroup) + ' ' + tmpvArticle_NO.cMainGroup 
          ).    
    END.
    IF AVAILABLE HuvGr AND NOT LOCKED HuvGr AND 
      HuvGr.HgBeskr <> tmpvArticle_NO.cMainGroup THEN 
    DO:
      rStandardFunksjoner:SkrivTilLogg(cLogg,
          '  Endret hovedgruppe: ' + STRING(tmpvArticle_NO.nMainGroup) + 
          ' Fra ' + HuvGr.HgBeskr +  
          ' Til ' + tmpvArticle_NO.cMainGroup 
          ).    
      ASSIGN 
        HuvGr.HgBeskr    = tmpvArticle_NO.cMainGroup
        HuvGr.AvdelingNr = IF tmpvArticle_NO.cMainGroup = 'HOME' THEN 2 ELSE 1
        .
    END.  
        
    RETURN.

  END METHOD.

    /*------------------------------------------------------------------------------
     Purpose: NB: Brukes ikke forel�pig.....
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC VOID oppdaterLC( INPUT DATASET dsvArticle_NO ):

        DEFINE VARIABLE piAntLC AS INTEGER NO-UNDO.
        DEFINE VARIABLE piAntSasong AS INTEGER NO-UNDO.
        DEFINE VARIABLE piantLand AS INTEGER NO-UNDO.
        DEFINE VARIABLE piLopNr AS INTEGER NO-UNDO.
        DEFINE VARIABLE piLoop AS INTEGER NO-UNDO.
        
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            '  Start oppdater LC. Data i tmpvArticle_NO: ' + STRING(CAN-FIND(FIRST tmpvArticle_NO))
            ).    

        FOR EACH tmpvArticle_NO /* WHERE
            tmpvArticle_NO.cArtno   >= '' AND 
            tmpvArticle_NO.nColCode >= 0 AND 
            tmpvArticle_NO.nSeason  >= 0 AND 
            tmpvArticle_NO.dtLastChanged >= DATETIME(TODAY - 30)*/
/*            USE-INDEX idxBreakBy            */
/*            BREAK BY tmpvArticle_NO.cArtno  */
/*                  BY tmpvArticle_NO.nColCode*/
/*                  BY tmpvArticle_NO.nSeason:*/
              USE-INDEX idxEanSesong
              BREAK BY tmpvArticle_NO.cEan 
                    BY tmpvArticle_NO.nSeason:

            /* Endrer bare p� siste sesong. Vi skal ha den siste sesongen i tabell v_articles_NO */
            /* Den tabellen synkes n�r det sendes ordre til PRS bla.                             */
            IF LAST-OF(tmpvArticle_NO.nSeason) THEN
            BRYTPUNKT:
            DO:
/*                rStandardFunksjoner:SkrivTilLogg(cLogg, */
/*                    '  Behandler artikkel: ' +          */
/*                       tmpvArticle_NO.cEan + ' ' +      */
/*                       cArtName +                       */
/*                       ' cArtno: ' + cArtno +           */
/*                       ' nColCode:' + STRING(nColCode) +*/
/*                       ' nSeason:' + STRING(nSeason) +  */
/*                       ' nArtGroup:' + STRING(nArtGroup)*/
/*                    ).                                  */
                
                FIND Strekkode NO-LOCK WHERE 
                  Strekkode.Kode = tmpvArticle_NO.cEAN NO-ERROR.
                IF NOT AVAILABLE Strekkode THEN 
                  LEAVE BRYTPUNKT.     
                /* Fikser alle artikler som h�rer til denne. */
                FIND ArtBas EXCLUSIVE-LOCK WHERE 
                  ArtBas.ArtikkelNr = Strekkode.ArtikkelNr NO-WAIT NO-ERROR.
                IF AVAILABLE ArtBas AND NOT LOCKED ArtBas THEN 
                ARTFIKS:
                DO:                
                    IF tmpvArticle_NO.dPriceLC > 0 AND tmpvArticle_NO.dPriceLC <> ? AND ArtBas.KjedeInnkPris <> tmpvArticle_NO.dPriceLC THEN
                    DO:                     
                        rStandardFunksjoner:SkrivTilLogg(cLogg,
                            '    Endret LC p� artikkel ' + tmpvArticle_NO.cEan + ' ' + ArtBas.LevKod + ' ' + ArtBas.Beskr + ' ' + ArtBas.LevFargKod + 
                            ' fra ' + STRING(ArtBas.KjedeInnkPris) + ' til ' + STRING(tmpvArticle_NO.dPriceLC) + '.'
                            ).    
                        ASSIGN 
                            piAntLC               = piAntLC + 1
                            ArtBas.KjedeInnkPris = tmpvArticle_NO.dPriceLC
                            .
                    END.
                    
                    /* TN 12/11-19 Det hender at det kommer inn h�yere sesonger i pakkseddlene en det vi f�r tak i UNI2. */
                    /*    Ref. tlf med Are. N� blilr pakkseddel master p� import av pakkseddler.                         */
                    IF (ArtBas.SaSong <= 99999 AND ArtBas.SaSong < tmpvArticle_NO.nSeason) THEN
                    DO:
                        rStandardFunksjoner:SkrivTilLogg(cLogg,
                            '    Endret Sesong p� artikkel ' + tmpvArticle_NO.cEan + ' ' + ArtBas.LevKod + ' ' + ArtBas.Beskr + ' ' + ArtBas.LevFargKod + 
                            ' fra ' + STRING(ArtBas.Sasong) + ' til ' + STRING(tmpvArticle_NO.nSeason) + '.'
                            ).    
                        
                        ASSIGN 
                            piAntSasong   = piAntSasong + 1
                            ArtBas.SaSong = tmpvArticle_NO.nSeason
                            .
                    END.
                    
                    IF TRIM(ArtBas.AlfaKode2) = '' AND tmpvArticle_NO.MadeInCountry <> '' THEN
                    DO: 
                        IF AVAILABLE NumLandKode THEN RELEASE NumLandKode.
                        IF AVAILABLE AlfaLandKode THEN RELEASE AlfaLandKode.
                        FIND FIRST NumLandKode WHERE NumLandKode.Land MATCHES '*' + tmpvArticle_NO.MadeInCountry + '*' NO-ERROR.
                        IF AVAILABLE NumLandKode THEN
                            FIND FIRST AlfaLandKode WHERE 
                                AlfaLandKode.NumLandKode = NumLandKode.NumLandKode NO-ERROR.
                        
                        
                        rStandardFunksjoner:SkrivTilLogg(cLogg,
                            '    Lagt inn opphavslandskode p� artikkel ' + tmpvArticle_NO.cEan + ' ' + ArtBas.LevKod + ' ' + ArtBas.Beskr + ' ' + ArtBas.LevFargKod + 
                            ' fra ' + STRING(ArtBas.AlfaKode2) + ' til ' + IF AVAILABLE AlfaLandKode THEN AlfaLandKode.AlfaKode2 ELSE ArtBas.AlfaKode2  + ' Land: ' + STRING(tmpvArticle_NO.MadeInCountry) + '.'
                            ).    
                        ASSIGN
                            piAntLand = piantLand + 1 
                            ArtBas.AlfaKode2 = IF AVAILABLE AlfaLandKode THEN AlfaLandKode.AlfaKode2 ELSE ArtBas.AlfaKode2
                            ArtBas.Lokasjon  = tmpvArticle_NO.MadeInCountry
                            . 
                    END.                    

                    /* Varegruppen m� legges under hovedgruppen. Den kan forekomme flere ganger. */
                    cVg = STRING(tmpvArticle_NO.nMainGroup) + FILL('0',4 - LENGTH(STRING(tmpvArticle_NO.nArtGroup))) + STRING(tmpvArticle_NO.nArtGroup).
                    
                    /* Oppdaterer hovedgruppe */
                    oppdaterHovedGr().

                    /* Oppdaterer varegruppe. */
                    oppdaterVareGr().
                    
                    /* Oppdaterer brukskoder. */
                    oppdaterAnv-Kod().

                    /* Oppretter hovedkategori hvis den mangler. */
                    oppdaterHovedkategori().


                    /* Sjekker varegruppe p� artikkelen. */
/*                    IF ArtBas.Vg <> tmpvArticle_NO.nArtGroup THEN*/
                    IF ArtBas.Vg <> INT(cVg) THEN
                    DO:
                      rStandardFunksjoner:SkrivTilLogg(cLogg,
                          '    Endret Varegruppe p� artikkel ' + tmpvArticle_NO.cEan +  
                             ' fra ' + STRING(ArtBas.Vg) + 
                             ' til ' + cVg + 
                             ' MainGrp: ' + STRING(tmpvArticle_NO.nMainGroup)
                          ).    
                      ASSIGN 
                        ArtBas.Vg    = INT(cVg)
                        ArtBas.LopNr = ?
                        ArtBas.Hg    = tmpvArticle_NO.nMainGroup
                        ArtBas.HovedKatNr = tmpvArticle_NO.nArtGroup
                        NO-ERROR.
                      IF ERROR-STATUS:ERROR THEN 
                      DO:
                        DO ix = 1 TO ERROR-STATUS:NUM-MESSAGES:
                            cTekst = STRING(ERROR-STATUS:GET-NUMBER(ix)) + ' '+ 
                                     ERROR-STATUS:GET-MESSAGE(ix). 
                            rStandardFunksjoner:SkrivTilLogg(cLogg, 
                                '     ** Feil: ' + cTekst 
                            ). 
                        END.                      END.
                      ELSE 
                        DO piLoop = 1 TO 10:   
                          piLopNr = 0.
                          RUN SettLopNr.p (artbas.vg,"F",OUTPUT piLopNr).
                          ASSIGN 
                            ArtBas.LopNr = piLopnr NO-ERROR.
                          IF ERROR-STATUS:ERROR THEN 
                          DO:
                            rStandardFunksjoner:SkrivTilLogg(cLogg,
                                '    Feilet med l�penr ' + String(piLopNr) + ' loop: ' + STRING(piLoop)  
                                ).    
                            NEXT.
                          END.
                          ELSE DO:
                            rStandardFunksjoner:SkrivTilLogg(cLogg,
                                '    Endret l�penr til ' + String(piLopNr)  
                                ).    
                            
                            LEAVE.
                          END.
                        END.
                      
                    END.

                    /* TN 29/(1-20 Denne skal alltid sjekkes. */
                    IF ArtBas.HovedKatNr <> tmpvArticle_NO.nArtGroup THEN 
                    DO:
                      ASSIGN 
                        ArtBas.HovedKatNr = tmpvArticle_NO.nArtGroup
                        .
                    END.
                    
                    IF ArtBas.Anv-Id <> tmpvArticle_NO.nMainGroup THEN 
                    DO:
                      ASSIGN 
                        ArtBas.Anv-Id = tmpvArticle_NO.nMainGroup
                        .
                    END.

                    /* Logger mapping av artikkel og vSuppliers til ImpKonv. */
                    setImpKonvSupplier().                     
                        
                    /* Logger mapping av artikkel og vSeasons til ImpKonv. */
                    setImpKonvSeasons().    
                END. /* ARTFIKS */
                    
            END. /* BRYTPUNKT */
        END.

        rStandardFunksjoner:SkrivTilLogg(cLogg,
            '  Ferdig oppdater LC. Antall endret LC: ' + STRING(piAntLC) + 
                ' Antall endret sesong: ' + STRING(piAntSasong) +
                ' Antall endret landkoder: ' + STRING(piantLand) +  
                '.'
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

  METHOD PUBLIC VOID oppdaterVareGr(  ):
    
    FIND VarGr WHERE
/*        VarGr.Vg = tmpvArticle_NO.nArtGroup EXCLUSIVE-LOCK NO-WAIT NO-ERROR.*/
        VarGr.Vg = INT(cVg) EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
    IF NOT AVAILABLE VarGr AND NOT LOCKED VarGr THEN 
      DO:
          CREATE VarGr.
          ASSIGN
/*              VarGr.Vg         = tmpvArticle_NO.nArtGroup*/
              VarGr.Vg         = INT(cVg)
              VarGr.VgBeskr    = tmpvArticle_NO.cArtGroup
              VarGr.Hg         = tmpvArticle_NO.nMainGroup
              VarGr.MomsKod    = 1
              VarGr.Kost_Proc  = 65
              NO-ERROR.
          rStandardFunksjoner:SkrivTilLogg(cLogg,
              '  Ny varegruppe: ' + STRING(tmpvArticle_NO.nArtGroup) + ' ' + tmpvArticle_NO.cArtGroup + ' Hg: ' + STRING(tmpvArticle_NO.nMainGroup) 
              ).    
              
          FOR EACH Kategori NO-LOCK WHERE
              Kategori.KatNr <= 4:
              IF NOT CAN-FIND(FIRST VgKat WHERE
                              VgKat.Vg    = VarGr.Vg AND
                              VgKat.VgKat = Kategori.KatNr) THEN
              DO:
                  CREATE VgKat.
                  ASSIGN
                  VgKat.Vg    = VarGr.Vg
                  VgKat.VgKat = Kategori.KatNr
                  VgKat.KatNr = Kategori.KatNr
                  NO-ERROR.
              END.
          END.
      END.
    IF NOT LOCKED VarGr AND VarGr.VgBeskr <>  tmpvArticle_NO.cArtGroup THEN
    DO: 
      rStandardFunksjoner:SkrivTilLogg(cLogg,
          '  Endret varegruppe: ' + STRING(tmpvArticle_NO.nArtGroup) +
          ' Fra ' + VarGr.VgBeskr + 
          ' Til ' + tmpvArticle_NO.cArtGroup + 
          ' Hg: ' + STRING(tmpvArticle_NO.nMainGroup) 
          ).    
      ASSIGN 
        VarGr.VgBeskr    = tmpvArticle_NO.cArtGroup
        VarGr.Hg         = tmpvArticle_NO.nMainGroup
        NO-ERROR.
      rStandardFunksjoner:SkrivTilLogg(cLogg,
          '  Endret varegruppe: ' + STRING(tmpvArticle_NO.nArtGroup) + ' ' + tmpvArticle_NO.cArtGroup + ' Hg: ' + STRING(tmpvArticle_NO.nMainGroup) 
          ).    
    END.    
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

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC VOID oppdaterAnv-Kod(  ):
    
  FIND Anv-Kod WHERE
      Anv-Kod.Anv-Id = tmpvArticle_NO.nMainGroup EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
  IF NOT AVAILABLE Anv-Kod AND NOT LOCKED Anv-Kod THEN 
    DO:
        CREATE Anv-Kod.
        ASSIGN
            Anv-Kod.Anv-Id   = tmpvArticle_NO.nMainGroup
            Anv-Kod.AnvBeskr = tmpvArticle_NO.cMainGroup
            NO-ERROR.
    END.
    
    RETURN.
  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC VOID oppdaterHovedkategori(  ):
    IF NOT CAN-FIND(HovedKategori WHERE 
                    HovedKategori.HovedKatNr = tmpvArticle_NO.nArtGroup) THEN 
    DO:
        CREATE HovedKategori.
        ASSIGN 
            HovedKategori.HovedKatNr    = tmpvArticle_NO.nArtGroup
            HovedKategori.HovedKatTekst = tmpvArticle_NO.cArtGroup
            .
    END.
    
    RETURN.

  END METHOD.

    DESTRUCTOR PUBLIC HentUNI2Data ( ):

    END DESTRUCTOR.
END CLASS.