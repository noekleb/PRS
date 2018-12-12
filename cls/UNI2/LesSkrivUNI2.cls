 
/*------------------------------------------------------------------------
    File        : LesSkrivBxEngine.cls
    Purpose     : 
    Syntax      : 
    Description : Posterer data fra temp-tabell i bxengine databasen.
    Author(s)   : tny
    Created     : Thu Nov 09 11:33:41 CET 2017
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING System.Data.SqlClient.*.
USING System.Data.*.
/*USING System.Data.SqlClient.SqlConnection.*.*/

/*ROUTINE-LEVEL ON ERROR UNDO, THROW.*/

CLASS cls.UNI2.LesSkrivUNI2: 
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.

    DEFINE VARIABLE cLogg    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE bTest    AS LOG       NO-UNDO.
    DEFINE VARIABLE cDatoTid AS CHARACTER NO-UNDO.  
    DEFINE VARIABLE cTekst   AS CHARACTER NO-UNDO.  
    DEFINE VARIABLE iX       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE bOk      AS LOG       NO-UNDO.

    /* Kommunikasjonsparametre */
    DEFINE VARIABLE cPwd    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cUserId AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cServer AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDbName AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDataSource AS CHARACTER NO-UNDO.

    /* Oppkobling mot server. */
    DEFINE VARIABLE cSQL      AS CHARACTER                    NO-UNDO.
    DEFINE VARIABLE ConString AS CHARACTER                    NO-UNDO.
    DEFINE VARIABLE Conn      AS System.Data.SqlClient.SqlConnection NO-UNDO.
    DEFINE VARIABLE Cmd       AS SqlCommand                          NO-UNDO.
    DEFINE VARIABLE CmdRead   AS SqlCommand                          NO-UNDO.
    DEFINE VARIABLE Rdr       AS SqlDataReader                       NO-UNDO.
    DEFINE VARIABLE SqlCred   AS SqlCredential                       NO-UNDO.
    DEFINE VARIABLE SeqString AS System.Security.SecureString        NO-UNDO.

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
               
    CONSTRUCTOR PUBLIC LesSkrivUNI2 ( INPUT pcLogg AS CHARACTER ):
        SUPER ().

        ASSIGN 
            bTest    = TRUE 
            cLogg    = pcLogg
            cDatoTid = REPLACE(STRING(TODAY),'/','') /*+ REPLACE(STRING(TIME,"HH:MM:SS"),':','')*/
            .

        rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.
                
        /* Kommunikasjonsparametre */ 
        IF SEARCH('tnc.txt') <> ? THEN 
            ASSIGN
                cPwd    = 'bxengine'
                cUserId = 'bxengine'
                cServer = 'localhost'
                cDbName = 'StageDB_Universal'
                cDataSource = 'SP1TOMN-14'
                .
        ELSE  
            ASSIGN 
                cPwd        = 'Uhdsa67RT'
                cUserId     = 'QlickView'
                cServer     = '192.168.100.30'
                cDbName     = 'StageDB_Universal'
                cDataSource = 'NORGE0047'
                .
                    
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC LOGICAL lesregArtSKU( OUTPUT DATASET dsregArtSKU ):
        
        DEFINE VARIABLE bResult AS LOGICAL NO-UNDO.
        DEFINE VARIABLE iAntRecord AS INTEGER NO-UNDO.
        DEFINE VARIABLE piSKU      AS INT64 NO-UNDO.

        ASSIGN 
            cSQL = 'SELECT [nSKU]
                          ,[nArtKey]
                          ,[nColCode]
                          ,[nSizeKey]
                          ,[dtLastChanged]
                          ,[dtLastChanged_Stg]
                      FROM [StageDB_Universal].[dbo].[regArtSKU];'.
                      
        CmdRead:CommandText = cSQL.

        Rdr = CmdRead:ExecuteReader().

        ix = 0.
        LOOPEN:
        DO WHILE Rdr:Read() ON ERROR UNDO, LEAVE:
            ASSIGN 
                piSKU = INT(STRING(Rdr["nSKU"]))                
                .
                
            IF INT(STRING(Rdr["nSKU"])) > 0 AND  
               NOT CAN-FIND(FIRST tmpregArtSKU WHERE tmpregArtSKU.nSKU = piSKU) THEN 
            DO:
                CREATE tmpregArtSKU.
                ASSIGN 
                    tmpregArtSKU.nSKU =  INT(STRING(Rdr["nSKU"]))
                    tmpregArtSKU.nArtKey =  INT(STRING(Rdr["nArtKey"]))
                    tmpregArtSKU.nColCode =  INT(STRING(Rdr["nColCode"]))
                    tmpregArtSKU.nSizeKey =  INT(STRING(Rdr["nSizeKey"]))
                    tmpregArtSKU.dtLastChanged  = Rdr["dtLastChanged"]
                    tmpregArtSKU.dtLastChanged_Stg  = Rdr["dtLastChanged_Stg"]
                    iAntRecord                = iAntRecord + 1
                    .
            END.
        END. /* LOOPEN */
        
        Rdr:Close().
        
        IF iAntRecord > 0 THEN 
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '  Antall poster lest: ' + STRING(iAntRecord) + '.' 
                ).
        
        bResult = TRUE.
        RETURN bResult.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC LOGICAL lesregEanSKU( OUTPUT DATASET dsregEanSKU ):
        
        DEFINE VARIABLE bResult AS LOGICAL NO-UNDO.
        DEFINE VARIABLE iAntRecord AS INTEGER NO-UNDO.
        DEFINE VARIABLE piSKU      AS INT64 NO-UNDO.

        ASSIGN 
            cSQL = 'SELECT [nSKU]
                          ,[nEan]
                          ,[dtLastChanged] 
                          ,[dtLastChanged_Stg]
                      FROM [StageDB_Universal].[dbo].[regEanSKU];'.
                      
        CmdRead:CommandText = cSQL.

        Rdr = CmdRead:ExecuteReader().

        ix = 0.
        LOOPEN:
        DO WHILE Rdr:Read() ON ERROR UNDO, LEAVE:
            ASSIGN 
                piSKU = DEC(STRING(Rdr["nSKU"]))                
                .
                
            IF INT(STRING(Rdr["nSKU"])) > 0 AND  
               NOT CAN-FIND(FIRST tmpregEanSKU WHERE tmpregEanSKU.nSKU = piSKU) THEN 
            DO:
                CREATE tmpregEanSKU.
                ASSIGN 
                    tmpregEanSKU.nSKU =  DEC(STRING(Rdr["nSKU"]))
                    tmpregEanSKU.nEan =  DEC(STRING(Rdr["nEan"]))
                    tmpregEanSKU.dtLastChanged  = Rdr["dtLastChanged"]
                    tmpregEanSKU.dtLastChanged_Stg  = Rdr["dtLastChanged_Stg"]
                    iAntRecord                = iAntRecord + 1
                    .
            END.
            CATCH e3 AS Progress.Lang.Error:
                DO ix = 1 TO e3:NumMessages:
                    rStandardFunksjoner:SkrivTilLogg(cLogg,
                        '  ** Feil: ' + e3:GetMessage(ix) 
                        ).    
                END.
            END CATCH.
        END. /* LOOPEN */
        
        Rdr:Close().
        
        IF iAntRecord > 0 THEN 
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '  Antall poster lest: ' + STRING(iAntRecord) + '.' 
                ).
        
        bResult = TRUE.
        RETURN bResult.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC LOGICAL lesvArticles(  OUTPUT DATASET dsvArticles  ):
        DEFINE VARIABLE bResult AS LOGICAL NO-UNDO.
        DEFINE VARIABLE iAntRecord AS INTEGER NO-UNDO.
        DEFINE VARIABLE inArtKey AS INT64 NO-UNDO.
        DEFINE VARIABLE inSeason AS INT64 NO-UNDO.
        ASSIGN 
            cSQL = 'SELECT *
                    FROM "StageDB_Universal".dbo."vArticles";'.
                    
        CmdRead:CommandText = cSQL.

        Rdr = CmdRead:ExecuteReader().

        ix = 0.
        LOOPEN:
        DO WHILE Rdr:Read() ON ERROR UNDO, LEAVE:
            ASSIGN 
                inArtKey = Rdr["nArtKey"]                
                inSeason = Rdr["nSeason"]                
                .
                
/*            rStandardFunksjoner:SkrivTilLogg(cLogg,*/
/*                '  Ean: ' + pcEAN                  */
/*                ).                                 */
                
            IF NOT CAN-FIND(FIRST tmpvArticles WHERE 
                            tmpvArticles.nArtKey = inArtKey AND 
                            tmpvArticles.nSeason = inSeason) THEN 
            DO:
                CREATE tmpvArticles.
                ASSIGN 
                    tmpvArticles.nArtKey = Rdr["nArtKey"]
                    tmpvArticles.cArtno = STRING(Rdr["cArtno"])
                    tmpvArticles.cArtName = STRING(Rdr["cArtName"])
                    tmpvArticles.nSeason = Rdr["nSeason"]
                    tmpvArticles.cConcept = STRING(Rdr["cConcept"])
                    tmpvArticles.MadeInCountry = STRING(Rdr["MadeInCountry"])
                    tmpvArticles.dPriceFOB = DEC(STRING(Rdr["dPriceFOB"]))
                    tmpvArticles.dPriceLC = DEC(STRING(Rdr["dPriceLC"]))
                    tmpvArticles.dPriceWholesale = DEC(STRING(Rdr["dPriceWholesale"]))
                    tmpvArticles.dPriceRetail = DEC(STRING(Rdr["dPriceRetail"]))
                    tmpvArticles.dtLastChanged = Rdr["dtLastChanged"] 
                    tmpvArticles.cDesc = STRING(Rdr["cDesc"])
                    tmpvArticles.cShortDesc = STRING(Rdr["cShortDesc"])
                    tmpvArticles.cSupplierArtno = STRING(Rdr["cSupplierArtno"])
                    tmpvArticles.nArtStructId = Rdr["nArtStructId"]
                    tmpvArticles.cArtStructName = STRING(Rdr["cArtStructName"])
                    tmpvArticles.nSupplierCode = Rdr["nSupplierCode"]
                    tmpvArticles.ShippedFromCountry = STRING(Rdr["ShippedFromCountry"])
                    tmpvArticles.cShippedFrom = STRING(Rdr["cShippedFrom"])
                    tmpvArticles.nOrder = Rdr["nOrder"]
                    tmpvArticles.nWeight = Rdr["nWeight"]
                    tmpvArticles.isLocked = Rdr["isLocked"]
                    tmpvArticles.isGlobalOut = Rdr["isGlobalOut"]
                    tmpvArticles.isLocalOut = Rdr["isLocalOut"]
                    tmpvArticles.isRemoved = Rdr["isRemoved"]
                    tmpvArticles.nArtGroup = Rdr["nArtGroup"]
                    tmpvArticles.cArtGroup = STRING(Rdr["cArtGroup"])
                    tmpvArticles.nSubGroup = Rdr["nSubGroup"]
                    tmpvArticles.cSubGroup = STRING(Rdr["cSubGroup"])
                    tmpvArticles.nMainGroup = Rdr["nMainGroup"]
                    tmpvArticles.cMainGroup = STRING(Rdr["cMainGroup"])
                    tmpvArticles.nConcept = Rdr["nConcept"]
                    tmpvArticles.dPriceFOBconfirmed = Rdr["dPriceFOBconfirmed"] 
                    tmpvArticles.nDesignerId = Rdr["nDesignerId"]
                    tmpvArticles.cFirstName = STRING(Rdr["cFirstName"])
                    tmpvArticles.cLastName = STRING(Rdr["cLastName"])
                    tmpvArticles.ArtSeasonstatus = STRING(Rdr["ArtSeasonstatus"])
                    tmpvArticles.isDeleted = Rdr["isDeleted"]
                    tmpvArticles.cAttributeName1 = STRING(Rdr["cAttributeName1"])
                    tmpvArticles.cAttributeName2 = STRING(Rdr["cAttributeName2"])
                    tmpvArticles.nArtStatus = Rdr["nArtStatus"]
                    tmpvArticles.cArtStatus_name = STRING(Rdr["cArtStatus_name"])
                    tmpvArticles.cArtStatus_shortname = STRING(Rdr["cArtStatus_shortname"])
                    tmpvArticles.nPackingMode = Rdr["nPackingMode"]
                    tmpvArticles.cCurrency = STRING(Rdr["cCurrency"])
                    tmpvArticles.BoxType1 = STRING(Rdr["BoxType1"])
                    tmpvArticles.BoxSize1 = STRING(Rdr["BoxSize1"])
                    tmpvArticles.BoxQty1 = STRING(Rdr["BoxQty1"])
                    tmpvArticles.BoxType2 = STRING(Rdr["BoxType2"])
                    tmpvArticles.BoxSize2 = STRING(Rdr["BoxSize2"])
                    tmpvArticles.BoxQty2 = STRING(Rdr["BoxQty2"])
                    tmpvArticles.BoxType3 = STRING(Rdr["BoxType3"])
                    tmpvArticles.BoxSize3 = STRING(Rdr["BoxSize3"])
                    tmpvArticles.BoxQty3 = STRING(Rdr["BoxQty3"])
                    tmpvArticles.cPackConfig = STRING(Rdr["cPackConfig"])
                    tmpvArticles.isBRP = Rdr["isBRP"]
                    tmpvArticles.nMinBuy = Rdr["nMinBuy"]
                    iAntRecord                = iAntRecord + 1
                    .
            END.
            CATCH e3 AS Progress.Lang.Error:
                DO ix = 1 TO e3:NumMessages:
                    rStandardFunksjoner:SkrivTilLogg(cLogg,
                        '  ** Feil: ' + e3:GetMessage(ix) 
                        ).    
                END.
            END CATCH.
        END. /* LOOPEN */ 
        
        Rdr:Close().
        
        IF iAntRecord > 0 THEN 
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '  Antall poster lest: ' + STRING(iAntRecord) + '.' 
                ).
        
        bResult = TRUE.
        RETURN bResult.

        CATCH e1 AS Progress.Lang.AppError:
            DO ix = 1 TO e1:NumMessages:
                rStandardFunksjoner:SkrivTilLogg(cLogg,
                    '  ** Feil: ' + e1:GetMessage(ix) 
                    ).    
            END.
    
            IF e1:ReturnValue > "" THEN
                rStandardFunksjoner:SkrivTilLogg(cLogg,
                    '  Returverdi: ' + e1:ReturnValue 
                    ).    
        END CATCH.
        CATCH e2 AS Progress.Lang.Error:
            DO ix = 1 TO e2:NumMessages:
                rStandardFunksjoner:SkrivTilLogg(cLogg,
                    '  ' + e2:GetMessage(ix) 
                    ).    
            END.
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC LOGICAL lesvArticles_NO( OUTPUT DATASET dsvArticle_NO ):
        
        DEFINE VARIABLE bResult AS LOGICAL NO-UNDO.
        DEFINE VARIABLE iAntRecord AS INTEGER NO-UNDO.
        DEFINE VARIABLE piEAN AS INT64 NO-UNDO.
        DEFINE VARIABLE pcEan AS CHARACTER NO-UNDO.

        ASSIGN 
            cSQL = 'SELECT a.*
                    FROM "StageDB_Universal".dbo."vArticles_NO" a
                    INNER JOIN (
                        SELECT nEAN, MAX(nSeason) Season
                        FROM "StageDB_Universal".dbo."vArticles_NO" b
                        GROUP BY nEAN
                    ) b ON a.nEAN = b.nEAN AND a.nSeason = b.Season;'.
                    
        CmdRead:CommandText = cSQL.

        Rdr = CmdRead:ExecuteReader().

        ix = 0.
        LOOPEN:
        DO WHILE Rdr:Read() ON ERROR UNDO, LEAVE:
            ASSIGN 
                pcEAN = STRING(Rdr["nEAN"])                
                .
                
/*            rStandardFunksjoner:SkrivTilLogg(cLogg,*/
/*                '  Ean: ' + pcEAN                  */
/*                ).                                 */
                
            IF DEC(STRING(Rdr["dPriceLC"])) > 0 AND  
               NOT CAN-FIND(FIRST tmpvArticle_NO WHERE tmpvArticle_NO.cEAN = pcEAN) THEN 
            DO:
                CREATE tmpvArticle_NO.
                ASSIGN 
                    tmpvArticle_NO.cEan           = pcEan
                    tmpvArticle_NO.cArtno         = STRING(Rdr["cArtno"])
                    tmpvArticle_NO.nArtKey        = Rdr["nArtKey"]
                    tmpvArticle_NO.cArtName       = STRING(Rdr["cArtName"])
                    tmpvArticle_NO.nSupplierCode  = Rdr["nSupplierCode"] 
                    tmpvArticle_NO.cSupplierName  = STRING(Rdr["cSupplierName"])
                    tmpvArticle_NO.MadeInCountry   = STRING(Rdr["MadeInCountry"])
                    tmpvArticle_NO.nSeason        = Rdr["nSeason"]
                    tmpvArticle_NO.cSeasonName    = STRING(Rdr["cSeasonName"])
                    tmpvArticle_NO.nArtGroup      = Rdr["nArtGroup"]
                    tmpvArticle_NO.cArtGroup      = STRING(Rdr["cArtGroup"])
                    tmpvArticle_NO.nSubGroup      = Rdr["nSubGroup"]
                    tmpvArticle_NO.cSubGroup      = STRING(Rdr["cSubGroup"])
                    tmpvArticle_NO.nMainGroup     = Rdr["nMainGroup"]
                    tmpvArticle_NO.cMainGroup     = STRING(Rdr["cMainGroup"])
                    tmpvArticle_NO.nConcept       = Rdr["nConcept"]
                    tmpvArticle_NO.cConcept       = STRING(Rdr["cConcept"])
                    tmpvArticle_NO.nDesignerId    = Rdr["nDesignerId"]
                    tmpvArticle_NO.cArtStructName = STRING(Rdr["cArtStructName"])
                    tmpvArticle_NO.nColCode       = Rdr["nColCode"]
                    tmpvArticle_NO.cColName       = STRING(Rdr["cColName"])
                    tmpvArticle_NO.cCode1         = STRING(Rdr["cCode1"])
                    tmpvArticle_NO.cCode2         = STRING(Rdr["cCode2"])
                    tmpvArticle_NO.cCode3         = STRING(Rdr["cCode3"])
                    tmpvArticle_NO.dPriceFOB      = DEC(STRING(Rdr["dPriceFOB"]))
                    tmpvArticle_NO.dPriceLC       = DEC(STRING(Rdr["dPriceLC"]))
                    tmpvArticle_NO.dPriceWholesale = DEC(STRING(Rdr["dPriceWholesale"]))
                    tmpvArticle_NO.dPriceRetail   = DEC(STRING(Rdr["dPriceRetail"]))
                    tmpvArticle_NO.isDeleted      = Rdr["isDeleted"]
                    tmpvArticle_NO.isLocked       = Rdr["isLocked"]
                    tmpvArticle_NO.isGlobalOut    = Rdr["isGlobalOut"]
                    tmpvArticle_NO.isLocalOut     = Rdr["isLocalOut"]
                    tmpvArticle_NO.isRemoved      = Rdr["isRemoved"]
                    tmpvArticle_NO.dtLastChanged  = Rdr["dtLastChanged"]
                    tmpvArticle_NO.ArtSeasonstatus = STRING(Rdr["ArtSeasonstatus"])
                    iAntRecord                = iAntRecord + 1
                    .
            END.
            CATCH e3 AS Progress.Lang.Error:
                DO ix = 1 TO e3:NumMessages:
                    rStandardFunksjoner:SkrivTilLogg(cLogg,
                        '  ** Feil: ' + e3:GetMessage(ix) 
                        ).    
                END.
            END CATCH.
        END. /* LOOPEN */ 
        
        Rdr:Close().
        
        IF iAntRecord > 0 THEN 
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '  Antall poster lest: ' + STRING(iAntRecord) + '.' 
                ).
        
        bResult = TRUE.
        RETURN bResult.

        CATCH e1 AS Progress.Lang.AppError:
            DO ix = 1 TO e1:NumMessages:
                rStandardFunksjoner:SkrivTilLogg(cLogg,
                    '  ** Feil: ' + e1:GetMessage(ix) 
                    ).    
            END.
    
            IF e1:ReturnValue > "" THEN
                rStandardFunksjoner:SkrivTilLogg(cLogg,
                    '  Returverdi: ' + e1:ReturnValue 
                    ).    
        END CATCH.
        CATCH e2 AS Progress.Lang.Error:
            DO ix = 1 TO e2:NumMessages:
                rStandardFunksjoner:SkrivTilLogg(cLogg,
                    '  ' + e2:GetMessage(ix) 
                    ).    
            END.
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC LOGICAL lesregArticles( OUTPUT DATASET DsregArticles ):
        
        DEFINE VARIABLE bResult AS LOGICAL NO-UNDO.
        DEFINE VARIABLE iAntRecord AS INTEGER NO-UNDO.
        DEFINE VARIABLE piKey1 AS INT64 NO-UNDO.
        DEFINE VARIABLE pcKey1 AS CHARACTER NO-UNDO.

        ASSIGN 
            cSQL = 'SELECT 
                     [nArtKey]
                    ,[cArtno]
                    ,[dtLastChanged]
                   FROM [StageDB_Universal].[dbo].[regArticles];'.
                                  
        CmdRead:CommandText = cSQL.

        Rdr = CmdRead:ExecuteReader().

        ix = 0.
        LOOPEN:
        DO WHILE Rdr:Read() ON ERROR UNDO, LEAVE:
            ASSIGN 
                piKey1 = INT(STRING(Rdr["nArtKey"]))                
                .
                
            IF INT(STRING(Rdr["nArtKey"])) <> 0 AND
               NOT CAN-FIND(FIRST tmpregArticles WHERE 
                            tmpregArticles.nArtKey = piKey1) THEN 
            DO: 
                CREATE tmpregArticles.
                ASSIGN 
                    iAntRecord                = iAntRecord + 1
                    tmpregArticles.nArtKey = INT(STRING(Rdr["nArtKey"]))
                    tmpregArticles.cArtNo = STRING(Rdr["cArtNo"])
                    tmpregArticles.dtLastChanged = Rdr["dtLastChanged"]                   
                    .
            END.
            CATCH e3 AS Progress.Lang.Error:
                DO ix = 1 TO e3:NumMessages:
                    rStandardFunksjoner:SkrivTilLogg(cLogg,
                        '  ** Feil: ' + e3:GetMessage(ix) 
                        ).    
                END.
            END CATCH.
        END. /* LOOPEN */
        
        Rdr:Close().
        
        IF iAntRecord > 0 THEN 
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '  Antall poster lest: ' + STRING(iAntRecord) + '.' 
                ).
        
        bResult = TRUE.
        RETURN bResult.

        CATCH e1 AS Progress.Lang.AppError:
            DO ix = 1 TO e1:NumMessages:
                rStandardFunksjoner:SkrivTilLogg(cLogg,
                    '  ** Feil: ' + e1:GetMessage(ix) 
                    ).    
            END.
    
            IF e1:ReturnValue > "" THEN
                rStandardFunksjoner:SkrivTilLogg(cLogg,
                    '  Returverdi: ' + e1:ReturnValue 
                    ).    
        END CATCH.
        CATCH e2 AS Progress.Lang.Error:
            DO ix = 1 TO e2:NumMessages:
                rStandardFunksjoner:SkrivTilLogg(cLogg,
                    '  ' + e2:GetMessage(ix) 
                    ).    
            END.
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

/*    METHOD PUBLIC LOGICAL lesregArtSupplier( OUTPUT DATASET dsregArtSupplier ):         */
/*                                                                                        */
/*        DEFINE VARIABLE bResult AS LOGICAL NO-UNDO.                                     */
/*        DEFINE VARIABLE iAntRecord AS INTEGER NO-UNDO.                                  */
/*        DEFINE VARIABLE piKey1 AS INT64 NO-UNDO.                                        */
/*        DEFINE VARIABLE piKey2 AS INT64 NO-UNDO.                                        */
/*        DEFINE VARIABLE pcKey1 AS CHARACTER NO-UNDO.                                    */
/*        DEFINE VARIABLE pcKey2 AS CHARACTER NO-UNDO.                                    */
/*                                                                                        */
/*        ASSIGN                                                                          */
/*                                                                                        */
/*            cSQL = 'SELECT [nArtKey]                                                    */
/*                          ,[nSupplierCode]                                              */
/*                          ,[nStatus]                                                    */
/*                          ,[dtLastChanged]                                              */
/*                          ,[dtLastChanged_Stg]                                          */
/*                          ,[isDeleted]                                                  */
/*                      FROM [StageDB_Universal].[dbo].[regArtSupplier];'.                */
/*                                                                                        */
/*        CmdRead:CommandText = cSQL.                                                     */
/*                                                                                        */
/*        Rdr = CmdRead:ExecuteReader().                                                  */
/*                                                                                        */
/*        ix = 0.                                                                         */
/*        LOOPEN:                                                                         */
/*        DO WHILE Rdr:Read() ON ERROR UNDO, LEAVE:                                       */
/*            ASSIGN                                                                      */
/*                piKey1 = INT(STRING(Rdr["nArtKey"]))                                    */
/*                piKey2 = INT(STRING(Rdr["nSupplierCode"]))                              */
/*                .                                                                       */
/*                                                                                        */
/*            IF INT(STRING(Rdr["nArtKey"])) <> 0 AND                                     */
/*               INT(STRING(Rdr["nSupplierCode"])) <> 0 AND                               */
/*               NOT CAN-FIND(FIRST tmpregArtSupplier WHERE                               */
/*                            tmpregArtSupplier.nArtKey = piKey1 AND                      */
/*                            tmpregArtSupplier.nSupplierCode = piKey2) THEN              */
/*            DO:                                                                         */
/*                CREATE tmpregArtSupplier.                                               */
/*                ASSIGN                                                                  */
/*                    tmpregArtSupplier.nArtKey =  INT(STRING(Rdr["nArtKey"]))            */
/*                    tmpregArtSupplier.nSupplierCode =  INT(STRING(Rdr["nSupplierCode"]))*/
/*                    tmpregArtSupplier.nStatus =  INT(STRING(Rdr["nStatus"]))            */
/*                    tmpregArtSupplier.dtLastChanged = Rdr["dtLastChanged"]              */
/*                    tmpregArtSupplier.dtLastChanged_Stg = Rdr["dtLastChanged_Stg"]      */
/*                    iAntRecord                = iAntRecord + 1                          */
/*                    .                                                                   */
/*            END.                                                                        */
/*            CATCH e3 AS Progress.Lang.Error:                                            */
/*                DO ix = 1 TO e3:NumMessages:                                            */
/*                    rStandardFunksjoner:SkrivTilLogg(cLogg,                             */
/*                        '  ** Feil: ' + e3:GetMessage(ix)                               */
/*                        ).                                                              */
/*                END.                                                                    */
/*            END CATCH.                                                                  */
/*        END. /* LOOPEN */                                                               */
/*                                                                                        */
/*        Rdr:Close().                                                                    */
/*                                                                                        */
/*        IF iAntRecord > 0 THEN                                                          */
/*            rStandardFunksjoner:SkrivTilLogg(cLogg,                                     */
/*                '  Antall poster lest: ' + STRING(iAntRecord) + '.'                     */
/*                ).                                                                      */
/*                                                                                        */
/*        bResult = TRUE.                                                                 */
/*        RETURN bResult.                                                                 */
/*                                                                                        */
/*        CATCH e1 AS Progress.Lang.AppError:                                             */
/*            DO ix = 1 TO e1:NumMessages:                                                */
/*                rStandardFunksjoner:SkrivTilLogg(cLogg,                                 */
/*                    '  ** Feil: ' + e1:GetMessage(ix)                                   */
/*                    ).                                                                  */
/*            END.                                                                        */
/*                                                                                        */
/*            IF e1:ReturnValue > "" THEN                                                 */
/*                rStandardFunksjoner:SkrivTilLogg(cLogg,                                 */
/*                    '  Returverdi: ' + e1:ReturnValue                                   */
/*                    ).                                                                  */
/*        END CATCH.                                                                      */
/*        CATCH e2 AS Progress.Lang.Error:                                                */
/*            DO ix = 1 TO e2:NumMessages:                                                */
/*                rStandardFunksjoner:SkrivTilLogg(cLogg,                                 */
/*                    '  ' + e2:GetMessage(ix)                                            */
/*                    ).                                                                  */
/*            END.                                                                        */
/*        END CATCH.                                                                      */
/*                                                                                        */
/*    END METHOD.                                                                         */

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC LOGICAL lesSeasons( OUTPUT DATASET dsSeasons ):

        DEFINE VARIABLE bResult AS LOGICAL NO-UNDO.
        DEFINE VARIABLE iAntRecord AS INTEGER NO-UNDO.
        DEFINE VARIABLE piSeason AS INT64 NO-UNDO.
        DEFINE VARIABLE pcSeason AS CHARACTER NO-UNDO.

        ASSIGN 

            cSQL = 'SELECT [nSeason]
                          ,[cSeasonName]
                          ,[nCompSeason]
                          ,[cCompSeasonName]
                          ,[dtLastChanged]
                      FROM [StageDB_Universal].[dbo].[vSeasons];'.
                      
        CmdRead:CommandText = cSQL.

        Rdr = CmdRead:ExecuteReader().

        ix = 0.
        LOOPEN:
        DO WHILE Rdr:Read() ON ERROR UNDO, LEAVE:
            ASSIGN 
                piSeason = INT(STRING(Rdr["nSeason"]))                
                .
                
            IF INT(STRING(Rdr["nSeason"])) > 0 AND  
               NOT CAN-FIND(FIRST tmpSeasons WHERE tmpSeasons.nSeason = piSeason) THEN 
            DO:
                CREATE tmpSeasons.
                ASSIGN 
                    tmpSeasons.nSeason =  DEC(STRING(Rdr["nSeason"])) 
                    tmpSeasons.cSeasonName    = STRING(Rdr["cSeasonName"])
                    tmpSeasons.nCompSeason =  DEC(STRING(Rdr["nCompSeason"])) 
                    tmpSeasons.cCompSeasonName = STRING(Rdr["cCompSeasonName"])
                    tmpSeasons.dtLastChanged  = Rdr["dtLastChanged"]
                    iAntRecord                = iAntRecord + 1
                    .
            END.
            CATCH e3 AS Progress.Lang.Error:
                DO ix = 1 TO e3:NumMessages:
                    rStandardFunksjoner:SkrivTilLogg(cLogg,
                        '  ** Feil: ' + e3:GetMessage(ix) 
                        ).    
                END.
            END CATCH.
        END. /* LOOPEN */
        
        Rdr:Close().
        
        IF iAntRecord > 0 THEN 
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '  Antall poster lest: ' + STRING(iAntRecord) + '.' 
                ).
        
        bResult = TRUE.
        RETURN bResult.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC LOGICAL lesvSupplier( OUTPUT DATASET DsvSupplier ):
        
        DEFINE VARIABLE bResult AS LOGICAL NO-UNDO.
        DEFINE VARIABLE iAntRecord AS INTEGER NO-UNDO.
        DEFINE VARIABLE piKey1 AS INT64 NO-UNDO.
        DEFINE VARIABLE piKey2 AS INT64 NO-UNDO.
        DEFINE VARIABLE pcKey1 AS CHARACTER NO-UNDO.
        DEFINE VARIABLE pcKey2 AS CHARACTER NO-UNDO.

        ASSIGN 
            cSQL = 'SELECT *
                    FROM [StageDB_Universal].[dbo].[vSuppliers];'.
                                  
        CmdRead:CommandText = cSQL.

        Rdr = CmdRead:ExecuteReader().

        ix = 0.
        LOOPEN:
        DO WHILE Rdr:Read() ON ERROR UNDO, LEAVE:
            ASSIGN
                piKey1 = INT(STRING(Rdr["nSupplierCode"]))
                .

            IF INT(STRING(Rdr["nSupplierCode"])) <> 0 AND
               NOT CAN-FIND(FIRST tmpvSupplier WHERE
                            tmpvSupplier.nSupplierCode = piKey1) THEN
            DO:
                CREATE tmpvSupplier.
                ASSIGN
                    iAntRecord                = iAntRecord + 1
                    tmpvSupplier.nSeason = INT(STRING(Rdr["nSeason"]))
                    tmpvSupplier.nSupplierCode = INT(STRING(Rdr["nSupplierCode"]))
                    tmpvSupplier.cSupplierName = STRING(Rdr["cSupplierName"])
                    tmpvSupplier.cSupplierShort = STRING(Rdr["cSupplierShort"])
                    tmpvSupplier.cVatNo = STRING(Rdr["cVatNo"])
                    tmpvSupplier.cNotes = STRING(Rdr["cNotes"])
                    tmpvSupplier.cCurrency = STRING(Rdr["cCurrency"])
                    tmpvSupplier.cRemarkLocal = STRING(Rdr["cRemarkLocal"])
                    tmpvSupplier.cRemarkPublic = STRING(Rdr["cRemarkPublic"])

                    tmpvSupplier.nDivider = INT(STRING(Rdr["nDivider"]))
                    tmpvSupplier.nMinTotalBuy = INT(STRING(Rdr["nMinTotalBuy"]))
                    tmpvSupplier.nMinTotalBuyPerColour = INT(STRING(Rdr["nMinTotalBuyPerColour"]))
                    tmpvSupplier.nDocCharge = INT(STRING(Rdr["nDocCharge"]))
                    tmpvSupplier.nDuty = INT(STRING(Rdr["nDuty"]))
                    tmpvSupplier.nFinance = INT(STRING(Rdr["nFinance"]))
                    tmpvSupplier.nFreight = INT(STRING(Rdr["nFreight"]))
                    tmpvSupplier.nGoetz = INT(STRING(Rdr["nGoetz"]))
                    tmpvSupplier.nInsurance = INT(STRING(Rdr["nInsurance"]))
                    tmpvSupplier.nRoyalty = INT(STRING(Rdr["nRoyalty"]))

                    tmpvSupplier.nSampleUpcharge = INT(STRING(Rdr["nSampleUpcharge"]))
                    tmpvSupplier.cAddress1 = STRING(Rdr["cAddress1"])
                    tmpvSupplier.cAddress2 = STRING(Rdr["cAddress2"])
                    tmpvSupplier.cAddress3 = STRING(Rdr["cAddress3"])
                    tmpvSupplier.cZip = STRING(Rdr["cZip"])
                    tmpvSupplier.cCity = STRING(Rdr["cCity"])
                    tmpvSupplier.cState = STRING(Rdr["cState"])
                    tmpvSupplier.cCountry = STRING(Rdr["cCountry"])

                    tmpvSupplier.cPhone = STRING(Rdr["cPhone"])
                    tmpvSupplier.cFax = STRING(Rdr["cFax"])
                    tmpvSupplier.cFirstname = STRING(Rdr["cFirstname"])
                    tmpvSupplier.cMiddlename = STRING(Rdr["cMiddlename"])
                    tmpvSupplier.cLastName = STRING(Rdr["cLastName"])
                    tmpvSupplier.cEmail = STRING(Rdr["cEmail"])
                    tmpvSupplier.dtLastChanged = Rdr["dtLastChanged"]
                    .
            END.
            CATCH e3 AS Progress.Lang.Error:
                DO ix = 1 TO e3:NumMessages:
                    rStandardFunksjoner:SkrivTilLogg(cLogg,
                        '  ** Feil: ' + e3:GetMessage(ix)
                        ).
                END.
            END CATCH.
        END. /* LOOPEN */

        Rdr:Close().

        IF iAntRecord > 0 THEN
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '  Antall poster lest: ' + STRING(iAntRecord) + '.'
                ).

        bResult = TRUE.
        RETURN bResult.

        CATCH e1 AS Progress.Lang.AppError:
            DO ix = 1 TO e1:NumMessages:
                rStandardFunksjoner:SkrivTilLogg(cLogg,
                    '  ** Feil: ' + e1:GetMessage(ix)
                    ).
            END.

            IF e1:ReturnValue > "" THEN
                rStandardFunksjoner:SkrivTilLogg(cLogg,
                    '  Returverdi: ' + e1:ReturnValue
                    ).
        END CATCH.
        CATCH e2 AS Progress.Lang.Error:
            DO ix = 1 TO e2:NumMessages:
                rStandardFunksjoner:SkrivTilLogg(cLogg,
                    '  ' + e2:GetMessage(ix)
                    ).
            END.
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC LOGICAL nedkoblingSqlServer(  ):
        DEFINE VARIABLE result AS LOGICAL NO-UNDO.
        result = FALSE.

        /* kobler ned forbindelse til SqlServer databasen. */
        Conn:Close() NO-ERROR.
        IF NOT ERROR-STATUS:ERROR THEN
        DO: 
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '  Koblet ned forbindelse til Sql server : ' + ConString + '.'
                ).
            RESULT = TRUE.
        END.    
        
        RETURN result.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC LOGICAL oppkoblingSqlServer(  ):
        
        DEFINE VARIABLE result AS LOGICAL NO-UNDO.
        result = FALSE.

        /* passord -> sec.string */
        SeqString = NEW System.Security.SecureString().
        DO ix = 1 TO LENGTH(cPwd):
            SeqString:AppendChar(SUBSTR(cPwd,ix,1)).
        END.
        SeqString:MakeReadOnly().

        /* brukernavn, pwd */
        SqlCred = NEW SqlCredential(cUserId,SeqString).

        ConString = "Server=" + cServer + ",1433".
        ConString = ConString + ";Data Source=" + cDataSource +  ";Database=" + cDbName .

        rStandardFunksjoner:SkrivTilLogg(cLogg,
            '  User         : ' + cUserId + CHR(10) /*+
            '  Pwd          : ' + cPwd + CHR(10)*/ +
            '  SQL ConString: ' + ConString + CHR(10) 
            ).

        Conn = NEW SqlConnection(ConString,SqlCred).

        Conn:Open() NO-ERROR.
        IF ERROR-STATUS:ERROR THEN 
        DO:
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '    ' + ERROR-STATUS:GET-MESSAGE(1) 
                ).    
        END.
        ELSE 
        DO: 
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '  Oppkoblet mot Sql server: ' + ConString + '.'
                ).
            result = TRUE.
        END.   
        
        CmdRead = NEW SqlCommand('', Conn).

        RETURN result.
        
        CATCH zeroError AS Progress.Lang.AppError:
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '** ENDFeil: ' + zeroError:GetMessage(1) 
                ).
        END CATCH.
        CATCH oneError AS Progress.Lang.SysError:
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '** ENDFeil: ' + oneError:GetMessage(1) 
                ).
        END CATCH.                
        CATCH twoError AS Progress.Lang.ProError:
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '** ENDFeil: ' + twoError:GetMessage(1) 
                ).
        END CATCH.    

    END METHOD.

    DESTRUCTOR PUBLIC LesSkrivUni2 ( ):
        /* kobler ned forbindelse til SqlServer databasen hvis det ikke er gjort før. */
        /* nedkoblingSqlServer(  ). */

    END DESTRUCTOR.

END CLASS.
