 
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
USING System.Data.SqlClient.SqlConnection.*.
USING System.Data.SqlClient.*.
USING System.Data.*.

/*ROUTINE-LEVEL ON ERROR UNDO, THROW.*/

CLASS cls.BxLog.LesSkrivBxEngine: 
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

    /* Dataset og .Net håndtering */
    DEFINE VARIABLE DotNetDs    AS System.Data.DataSet.    
    DEFINE VARIABLE oServer     AS "Microsoft.SqlServer.Management.Smo.Server".
    DEFINE VARIABLE oConnection AS Microsoft.SqlServer.Management.Common.ServerConnection.
    DEFINE VARIABLE oDataBase   AS Microsoft.SqlServer.Management.Smo.Database.
    DEFINE VARIABLE PDataset    AS HANDLE.

    
    {cls\BxLog\tempTabeller.i}    
    {cls\BxLog\tempDatasett.i}
    {cls\bxLog\tmpTblStockCount.i}
    {cls\BxLog\tmpDsStockCount.i}
    {cls\bxLog\tmpTblBxPickinglist.i}
    {cls\BxLog\tmpDsBxPickinglist.i}        
               
    CONSTRUCTOR PUBLIC LesSkrivBxEngine ( INPUT pcLogg AS CHARACTER ):
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
                cDbName = 'bxengine'
                cDataSource = 'SP1TOMN-14'
                .
        ELSE  
            ASSIGN 
                cPwd        = 'BxWarehouse'
                cUserId     = 'BxEngine'
                cServer     = '192.168.200.186'
                cDbName     = 'BxEngine'
                cDataSource = 'PRS-PGM1'
                .

                    
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC LOGICAL lesBxPickinglist( INPUT piMaksAntLister AS INTEGER, 
                                            INPUT-OUTPUT DATASET dsBxPickinglist ):
        
        DEFINE VARIABLE bResult      AS LOGICAL   NO-UNDO.
        DEFINE VARIABLE iAntRecord   AS INTEGER   NO-UNDO.
        DEFINE VARIABLE pcWarehouseno AS CHARACTER NO-UNDO.
        DEFINE VARIABLE piOrderno     AS INT64  NO-UNDO.

        ASSIGN 
            cSQL    = "SELECT 
                       [Id]
                       ,[Warehouse]
                       ,[Orderno]
                       ,[Date]
                       ,[DelDate]
                       ,[Customerno]
                       ,[Customername]
                       ,[Projectname] 
                    FROM [BxPickinglist] 
                    WHERE Orderstatus = 'Picked' 
                    ORDER BY Id"
            .
        /*Cmd = NEW SqlCommand(cSql, Conn).*/
        CmdRead:CommandText = cSQL.

        Rdr = CmdRead:ExecuteReader().

        ix = 0.
        LOOPEN:
        DO WHILE Rdr:Read() ON ERROR UNDO, LEAVE:
            ASSIGN 
                pcWarehouseno = Rdr["Warehouse"]
                piOrderno     = Rdr["Orderno"]
                .

            IF NOT CAN-FIND(FIRST tmpBxPickinglist WHERE 
                                  tmpBxPickinglist.Warehouseno = pcWarehouseno AND 
                                  tmpBxPickinglist.Orderno     = piOrderno) THEN 
            DO:
                CREATE tmpBxPickinglist.

                ASSIGN 
                    tmpBxPickinglist.Id           = INT(STRING(Rdr["Id"]))
                    tmpBxPickinglist.Warehouseno  = Rdr["Warehouse"]
                    tmpBxPickinglist.Orderno      = INT(STRING(Rdr["Orderno"]))
                    tmpBxPickinglist.dDate        = Rdr["Date"]
                    tmpBxPickinglist.DelDate      = Rdr["DelDate"]
                    tmpBxPickinglist.Customerno   = INT(STRING(Rdr["Customerno"]))
                    tmpBxPickinglist.Customername = Rdr["Customername"]
                    tmpBxPickinglist.Projectname  = Rdr["Projectname"]
                    iAntRecord              = iAntRecord + 1
                    .

                IF iantRecord >= piMaksAntLister THEN 
                    LEAVE LOOPEN.    
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
                '  lesBxPickinglist Antall poster lest: ' + STRING(iAntRecord) + '.' 
                ).
        
        bResult = TRUE.
        RETURN bResult.

        CATCH e1 AS Progress.Lang.AppError:
            DO ix = 1 TO e1:NumMessages:
                rStandardFunksjoner:SkrivTilLogg(cLogg,
                    '  ** Feil lesBxPickinglist: ' + e1:GetMessage(ix) 
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
                    '  lesBxPickinglist' + e2:GetMessage(ix) 
                    ).    
            END.
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC LOGICAL lesBxPickinglistline( INPUT-OUTPUT DATASET dsBxPickinglist ):
        
        DEFINE VARIABLE bResult AS LOGICAL NO-UNDO.
        DEFINE VARIABLE iAntRecord    AS INTEGER   NO-UNDO.
        DEFINE VARIABLE pcWarehouseno AS CHARACTER NO-UNDO.
        DEFINE VARIABLE piOrderno AS INT64 NO-UNDO.
        DEFINE VARIABLE piOrderline AS INT64     NO-UNDO.
        
        LES_ALLE_LISTER:
        FOR EACH tmpBxPickinglist:

            ASSIGN 
                cSQL = "SELECT 
                         [Id]
                        ,[Orderno]
                        ,[Orderline]
                        ,[Productname]
                        ,[Location]
                        ,[Warehouse]
                        ,[DelDate]
                        ,[Price]
                        ,[PickingQuantity]
                        ,[PickedQuantity]
                        ,[Unitname]
                        ,[GTIN]
                      FROM [BxPickinglistLine]
                      WHERE Warehouse = '" + tmpBxPickinglist.Warehouseno + "' AND orderno = '" + STRING(tmpBxPickinglist.Orderno) + "'"
                .         
            CmdRead:CommandText = cSQL.

            Rdr = CmdRead:ExecuteReader().
                
            ix = 0.
            LOOPEN:
            DO WHILE Rdr:Read() ON ERROR UNDO, LEAVE:
                ASSIGN 
                    pcWarehouseno = Rdr["Warehouse"]
                    piOrderno     = Rdr["Orderno"]
                    piOrderline   = Rdr["Orderline"]
                    .

                IF NOT CAN-FIND(FIRST tmpBxPickinglistline WHERE 
                    tmpBxPickinglistline.Warehouseno = pcWarehouseno AND 
                    tmpBxPickinglistline.Orderno     = piOrderno AND 
                    tmpBxPickinglistline.Orderline   = piOrderline) THEN  
                DO:
                    CREATE tmpBxPickinglistline.

                    ASSIGN 
                        tmpBxPickinglistline.Id              = INT(STRING(Rdr["Id"]))
                        tmpBxPickinglistline.Warehouseno     = Rdr["Warehouse"]
                        tmpBxPickinglistline.Orderno         = INT(STRING(Rdr["Orderno"]))
                        tmpBxPickinglistline.Orderline       = INT(STRING(Rdr["Orderline"]))
                        tmpBxPickinglistline.Productname     = Rdr["Productname"]
                        tmpBxPickinglistline.Location        = Rdr["Location"]
                        tmpBxPickinglistline.DelDate         = Rdr["DelDate"]
                        tmpBxPickinglistline.Price           = STRING(Rdr["Price"])
                        tmpBxPickinglistline.PickingQuantity = STRING(Rdr["PickingQuantity"])
                        tmpBxPickinglistline.PickedQuantity  = STRING(Rdr["PickedQuantity"])                        
                        tmpBxPickinglistline.Unitname        = Rdr["Unitname"]
                        tmpBxPickinglistline.GTIN            = Rdr["GTIN"]
                        iAntRecord                           = iAntRecord + 1
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
        END. /* LES_ALLE_LISTER */
                
        bResult = TRUE.
        RETURN bResult.

        CATCH e1 AS Progress.Lang.AppError:
            DO ix = 1 TO e1:NumMessages:
                rStandardFunksjoner:SkrivTilLogg(cLogg,
                    '  ** Feil lesBxPickinglist: ' + e1:GetMessage(ix) 
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
                    '  lesBxPickinglist' + e2:GetMessage(ix) 
                    ).    
            END.
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC LOGICAL lesStockCount( INPUT piMaksAntLister AS INTEGER, 
                                         INPUT-OUTPUT DATASET dsStockCount ):
        
        DEFINE VARIABLE bResult AS LOGICAL NO-UNDO.
        DEFINE VARIABLE iAntRecord AS INTEGER NO-UNDO.
        DEFINE VARIABLE cName AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cPrefix AS CHARACTER NO-UNDO.

        /* Ikke gjør dette herfra. Bedre å ha kontrollen fra kallende program. */
        /* EMPTY TEMP-TABLE tmpStockCount.                                     */
        
        ASSIGN 
            cPrefix = 'Device'
            cSQL = "SELECT 
                        Id
                        ,Name
                        ,Source
                        ,Locked
                        ,[User]
                        ,WareHouse
                        ,Received
                        ,Company
                        ,Child
                        ,Status
                        ,ImportId
                        ,Exported 
                    FROM StockCount 
                    WHERE Status = 'Counted' 
                    ORDER BY Id"
            .
        /*Cmd = NEW SqlCommand(cSql, Conn).*/
        CmdRead:CommandText = cSQL.

        Rdr = CmdRead:ExecuteReader().

        ix = 0.
        LOOPEN:
        DO WHILE Rdr:Read() ON ERROR UNDO, LEAVE:
            ASSIGN 
                cName = Rdr["Name"]
                .
            /* Hvis første entry = 'Device', er ikke overføring fra håndterminalen klar. */ 
            /* Navnet på listen har mønsteret '«<navn>_<id>_<timestamp>'. Første entry   */
            /* endres når overføring av alle linjene fra håndterminalen er klar.         */   
            IF ENTRY(1,cName,'_') = cPrefix THEN 
                NEXT.
                
            IF NOT CAN-FIND(FIRST tmpStockCount WHERE tmpStockCount.Name = cName) THEN 
            DO:
                CREATE tmpStockCount.
                ASSIGN 
                    tmpStockCount.cId       = STRING(Rdr["Id"])
                    tmpStockCount.Name      = Rdr["Name"]
                    tmpStockCount.Source    = Rdr["Source"]
                    tmpStockCount.bLocked   = Rdr["Locked"]
                    tmpStockCount.cUser     = STRING(Rdr["User"])
                    tmpStockCount.WareHouse = Rdr["WareHouse"]
                    tmpStockCount.Received  = Rdr["Received"]
                    tmpStockCount.cCompany  = STRING(Rdr["Company"])
                    tmpStockCount.cChild    = STRING(Rdr["Child"])
                    tmpStockCount.cStatus   = Rdr["Status"]
                    tmpStockCount.ImportId  = Rdr["ImportId"]
                    tmpStockCount.Exported  = Rdr["Exported"]
                    iAntRecord              = iAntRecord + 1
                    .
                IF iantRecord >= piMaksAntLister THEN 
                    LEAVE LOOPEN.    
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

    METHOD PUBLIC LOGICAL lesStockCountLine( INPUT-OUTPUT DATASET dsStockCount ):
        
        DEFINE VARIABLE bResult    AS LOGICAL   NO-UNDO.
        DEFINE VARIABLE iAntRecord AS INTEGER NO-UNDO.
        DEFINE VARIABLE c2Id       AS CHARACTER NO-UNDO.

        HOVEDLOOP:
        FOR EACH tmpStockCount:
            ASSIGN 
                cSQL = "SELECT [Id]
                      ,[Productno]
                      ,[Quantity]
                      ,[Warehouse]
                      ,[Unit]
                      ,[Location]
                      ,[StockCount]
                      ,[ExpectedQuantity]
                      ,[CountedTime]
                      ,[Productname]
                      ,[Unitcost]
                      ,[Unitfactor]
                      ,[Unitname]
                      ,[ImportId]
                      ,[GTIN]
                      ,[Status]
                  FROM [bxengine].[dbo].[StockCountLine]
                  WHERE [StockCount] = " + tmpStockCount.cId 
                .
        
            CmdRead:CommandText = cSQL.

            Rdr = CmdRead:ExecuteReader().
        
            iAntRecord = 0.
            LOOPEN:
            DO WHILE Rdr:Read() ON ERROR UNDO, LEAVE:
                
                c2Id = STRING(Rdr["Id"]).

                IF NOT CAN-FIND(FIRST tmpStockCountLine WHERE 
                    tmpStockCountLine.cStockCount = tmpStockCount.cId AND 
                    tmpstockCountLine.cId = c2Id ) THEN 
                DO:
                    CREATE tmpStockCountLine.
                    ASSIGN 
                        tmpStockCountLine.cId               = STRING(Rdr["Id"])
                        tmpStockCountLine.cStockCount       = STRING(Rdr["StockCount"])
    
                        tmpStockCountLine.cProductno        = Rdr["Productno"]
                        tmpStockCountLine.cQuantity         = Rdr["Quantity"]
                        tmpStockCountLine.cWarehouse        = STRING(Rdr["Warehouse"])
                        tmpStockCountLine.cUnit             = STRING(Rdr["Unit"])
                        tmpStockCountLine.cWareHouse        = Rdr["WareHouse"]
                        tmpStockCountLine.cLocation         = STRING(Rdr["Location"])
                        tmpStockCountLine.cExpectedQuantity = STRING(Rdr["ExpectedQuantity"])
                        tmpStockCountLine.CountedTime       = Rdr["CountedTime"]
                        tmpStockCountLine.cProductname      = Rdr["Productname"]
                        tmpStockCountLine.cUnitcost         = STRING(Rdr["Unitcost"])
                        tmpStockCountLine.cUnitfactor       = STRING(Rdr["Unitfactor"])
                        tmpStockCountLine.cUnitname         = STRING(Rdr["Unitname"])
                        tmpStockCountLine.cImportId         = STRING(Rdr["ImportId"])
                        tmpStockCountLine.cGTIN             = STRING(Rdr["GTIN"])
                        tmpStockCountLine.cStatus           = STRING(Rdr["Status"])
                        iAntRecord                          = iAntRecord + 1
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
            
            /* Viktig å lukke leseren, slik at vi kan ta neste tellefil også. */
            Rdr:Close().
        
            /* Setter antall tellelinjer i listens hode. */
            ASSIGN 
                tmpStockcount.NumLines = iAntRecord.
        END. /* HOVEDLOOP */
        
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
     Purpose: Flegger plukklistene som hentet.
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC LOGICAL oppdBxPickingList( INPUT-OUTPUT DATASET dsBxPickinglist ):
        
        DEFINE VARIABLE iId        AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cSqlUpdate AS CHARACTER NO-UNDO.
        DEFINE VARIABLE bResult    AS LOG       NO-UNDO.
        
        DEFINE VARIABLE iBuffer    AS INTEGER   NO-UNDO.
        DEFINE VARIABLE hBuffer    AS HANDLE    NO-UNDO EXTENT 40.
        DEFINE VARIABLE hQuery     AS HANDLE    NO-UNDO EXTENT 40.
        DEFINE VARIABLE dDatoTid   AS DATETIME  NO-UNDO.

        ASSIGN 
            bResult  = FALSE 
            dDatoTid = NOW
            .

        FLAGGER_LISTEN:
        FOR EACH tmpBxPickingList WHERE 
            tmpBxPickingList.PlListeStatus = 30:

            cSqlUpdate =
                "UPDATE [BxPickinglist]
             SET [Orderstatus]=@Orderstatus
             OUTPUT INSERTED.ID
             WHERE Id= " + STRING(tmpBxPickingList.Id).
        
            Cmd = Conn:CreateCommand().
            Cmd:Parameters:Clear().
            Cmd:Parameters:AddWithValue("@Orderstatus",STRING(tmpBxPickingList.Orderstatus)).
            
            Cmd:CommandText = cSqlUpdate.
                
            iId = Cmd:ExecuteScalar().
        END. /* FLAGGER_LISTEN */

        RETURN bResult.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose: Flagger tellelistene som hentet.
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC LOGICAL oppdStockCount( INPUT-OUTPUT DATASET dsStockCount ):
        
        DEFINE VARIABLE iId        AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cSqlUpdate AS CHARACTER NO-UNDO.
        DEFINE VARIABLE bResult    AS LOG       NO-UNDO.
        
        DEFINE VARIABLE iBuffer    AS INTEGER   NO-UNDO.
        DEFINE VARIABLE hBuffer    AS HANDLE    NO-UNDO EXTENT 40.
        DEFINE VARIABLE hQuery     AS HANDLE    NO-UNDO EXTENT 40.
        DEFINE VARIABLE dDatoTid   AS DATETIME  NO-UNDO.

        ASSIGN 
            bResult  = FALSE 
            dDatoTid = NOW
            .
        FLAGGER_LISTEN:
        FOR EACH tmpStockCount WHERE 
            tmpStockCount.cStatus = 'Sendt':
        
            cSqlUpdate =     
                "UPDATE [StockCount]        
                 SET  Status=@Status
                     ,Exported=@Exported
                 OUTPUT INSERTED.ID
                 WHERE Id= " + cId.
        
            Cmd = Conn:CreateCommand().
            Cmd:Parameters:Clear().
            Cmd:Parameters:AddWithValue("@Status",tmpStockCount.cStatus).
            Cmd:Parameters:AddWithValue("@Exported",dDatoTid).
            
            Cmd:CommandText = cSqlUpdate.
                
            iId = Cmd:ExecuteScalar().
            IF (iId <> ?) THEN
                tmpStockCount.Exported = dDatotid.
        END. /* FLAGGER_LISTEN */

        RETURN bResult.

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
            '  User         : ' + cUserId + CHR(10) +
            '  Pwd          : ' + cPwd + CHR(10) +
            '  SQL ConString: ' + ConString 
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

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC LOGICAL posterBxcustomer( INPUT-OUTPUT hDataset AS HANDLE ):
        
        DEFINE VARIABLE iId        AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cSqlUpdate AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cSqlInsert AS CHARACTER NO-UNDO.
        DEFINE VARIABLE bResult    AS LOG       NO-UNDO.
        
        DEFINE VARIABLE iBuffer    AS INTEGER   NO-UNDO.
        DEFINE VARIABLE hBuffer    AS HANDLE    NO-UNDO EXTENT 40.
        DEFINE VARIABLE hQuery     AS HANDLE    NO-UNDO EXTENT 40.

        ASSIGN 
            bResult = FALSE 
            .
        
        IF bTest THEN 
        DO:
            cTekst = 'konv\BxCustomerSkriv' + cDatoTid + '.JSon'.
            hDataset:WRITE-JSON('file', cTekst, TRUE).
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '  Dataset: ' + cTekst 
                ).    
        END. 
    
        cSqlUpdate =     
            "UPDATE [Bxcustomer]        
         SET  Customername=@Customername
          OUTPUT INSERTED.Id
          where  Customerno= @Customerno".
        
        cSqlInsert =     
            "INSERT INTO [Bxcustomer]
               ([Customerno]
               ,[Customername])
         OUTPUT INSERTED.ID
         VALUES
               (@Customerno
               ,@Customername)".
                
        Cmd = Conn:CreateCommand().

        /* Loop through dataset buffers */
        LOOPEN:
        DO iBuffer = 1 TO hDataset:NUM-BUFFERS:
            hBuffer[iBuffer] = hDataset:GET-BUFFER-HANDLE(iBuffer).
            
            CREATE QUERY hQuery[iBuffer].
            hQuery[iBuffer]:SET-BUFFERS(hBuffer[iBuffer]).
            hQuery[iBuffer]:QUERY-PREPARE("FOR EACH " + hBuffer[iBuffer]:NAME).
            hQuery[iBuffer]:QUERY-OPEN().
            hQuery[iBuffer]:GET-FIRST().
      
            /* Loop through the records in each buffer */
            DO WHILE NOT hQuery[iBuffer]:QUERY-OFF-END:
    
    
                Cmd:Parameters:Clear().
                Cmd:Parameters:AddWithValue("@Customerno",hBuffer[iBuffer]:BUFFER-FIELD('Customerno'):BUFFER-VALUE()).
                Cmd:Parameters:AddWithValue("@Customername",hBuffer[iBuffer]:BUFFER-FIELD('Customername'):BUFFER-VALUE()).
            
                Cmd:CommandText = cSqlUpdate.
                iId = Cmd:ExecuteScalar().
                IF (iId = ?) THEN
                DO:
                    Cmd:CommandText = cSqlInsert.
                    iId = Cmd:ExecuteScalar().
                END.
                
                /* Tar vare på butikkens tildelte ID */
                ASSIGN 
                    hBuffer[iBuffer]:BUFFER-FIELD('Id'):BUFFER-VALUE() = iId
                    . 

                hQuery[iBuffer]:GET-NEXT().
            END.
        END. /* LOOPEN*/

        /* Flagger at det har gått bra. */
        IF ERROR-STATUS:ERROR THEN 
        DO ix = 1 TO ERROR-STATUS:NUM-MESSAGES:        
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '** FeilNr: '+ STRING(ERROR-STATUS:GET-NUMBER(ix)) + ' ' + ERROR-STATUS:GET-MESSAGE(ix) 
                ).
        END.
        ELSE bResult = TRUE. 
        
        RETURN bResult.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC LOGICAL posterBxPickinglist( INPUT DATASET dsBxPickinglist ):
        
        DEFINE VARIABLE bResult AS LOGICAL NO-UNDO.
        DEFINE VARIABLE pbOk AS LOG NO-UNDO.
        DEFINE VARIABLE iId        AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cSqlUpdate AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cSqlInsert AS CHARACTER NO-UNDO.
        DEFINE VARIABLE iNye       AS INTEGER   NO-UNDO.
        DEFINE VARIABLE iEndret    AS INTEGER   NO-UNDO.
        DEFINE VARIABLE itotalt    AS INTEGER   NO-UNDO.
        
        ASSIGN 
            bResult = FALSE 
            .
        
        FIND FIRST tmpBxPickinglist NO-ERROR.
        IF NOT AVAILABLE tmpBxPickinglist THEN
        DO: 
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '  Tabellen er tom: BxPickinglist.' 
                ).    
            RETURN bResult.
        END.
            
        IF bTest THEN 
        DO:
            cTekst = 'konv\BxPickinglistSkriv' + cDatoTid + '.JSon'.
            DATASET dsBxPickinglist:WRITE-JSON('file', cTekst, TRUE).
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '  Dataset: ' + cTekst 
                ).    
        END. 

        cSqlUpdate =
            "UPDATE [BxPickinglist]
         SET [Date]=@Date
             ,DelDate=@DelDate
             ,Customerno=@Customerno
             ,Customername=@Customername
             ,Projectname=@Projectname
             ,Orderstatus=@Orderstatus
             ,Ordertype=@Ordertype
          OUTPUT INSERTED.ID
          where Warehouse= @Warehouse AND Orderno= @Orderno".

        cSqlInsert =
            "INSERT INTO [BxPickinglist]
               ([Warehouse]
               ,[Orderno]
               ,[Date]
               ,[DelDate]
               ,[Customerno]
               ,[Customername]
               ,[Projectname]
               ,[Orderstatus]
               ,[Ordertype])
         OUTPUT INSERTED.ID
         VALUES
               (@Warehouse
               ,@Orderno
               ,@Date
               ,@DelDate
               ,@Customerno
               ,@customername
               ,@Projectname
               ,@Orderstatus
               ,@Ordertype)".
        
        Cmd = Conn:CreateCommand() NO-ERROR.
        /* Fanger eventuelle feilmeldinger.. */
        IF ERROR-STATUS:ERROR THEN 
        DO ix = 1 TO ERROR-STATUS:NUM-MESSAGES:        
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '** CreateCommand - FeilNr: '+ STRING(ERROR-STATUS:GET-NUMBER(ix)) + ' ' + ERROR-STATUS:GET-MESSAGE(ix) 
                ).
        END.

        /* Loop through dataset buffers */
        LOOPEN:
        FOR EACH tmpBxPickinglist ON ERROR UNDO, LEAVE:
                itotalt = itotalt + 1.

                IF ( tmpBxPickinglist.Warehouseno = ? OR
                     tmpBxPickinglist.Orderno = ? OR
                     tmpBxPickinglist.dDate = ? OR
                     tmpBxPickinglist.DelDate = ? OR
                     tmpBxPickinglist.Customerno = ? OR
                     tmpBxPickinglist.customername = ? OR
                     tmpBxPickinglist.Projectname = ? OR  
                     tmpBxPickinglist.Orderstatus = ? OR  
                     tmpBxPickinglist.Ordertype = ?  
                    ) THEN 
                DO:
                    rStandardFunksjoner:SkrivTilLogg(cLogg,
                        '  Warehouse: ' + (IF tmpBxPickinglist.Warehouseno = ? THEN '?' ELSE tmpBxPickinglist.Warehouseno) 
                        ).    
                    rStandardFunksjoner:SkrivTilLogg(cLogg,
                        '  Productno: ' + (IF tmpBxPickinglist.Orderno = ? THEN '?' ELSE STRING(tmpBxPickinglist.Orderno)) 
                        ).    
                    rStandardFunksjoner:SkrivTilLogg(cLogg,
                        '  ProductName: ' + (IF tmpBxPickinglist.dDate = ? THEN '?' ELSE STRING(tmpBxPickinglist.dDate)) 
                        ).    
                    rStandardFunksjoner:SkrivTilLogg(cLogg,
                        '  GTIN: ' + (IF tmpBxPickinglist.DelDate = ? THEN '?' ELSE STRING(tmpBxPickinglist.DelDate)) 
                        ).    
                    rStandardFunksjoner:SkrivTilLogg(cLogg,
                        '  Price: ' + (IF tmpBxPickinglist.Customerno = ? THEN '?' ELSE STRING(tmpBxPickinglist.Customerno)) 
                        ).    
                    rStandardFunksjoner:SkrivTilLogg(cLogg,
                        '  Cost: ' + (IF tmpBxPickinglist.Projectname = ? THEN '?' ELSE tmpBxPickinglist.Projectname) 
                        ).    
                    rStandardFunksjoner:SkrivTilLogg(cLogg,
                        '  Location: ' + (IF tmpBxPickinglist.customername = ? THEN '?' ELSE tmpBxPickinglist.customername) 
                        ).    
                    rStandardFunksjoner:SkrivTilLogg(cLogg,
                        '  Orderstatus: ' + (IF tmpBxPickinglist.Orderstatus = ? THEN '?' ELSE tmpBxPickinglist.Orderstatus) 
                        ).    
                    rStandardFunksjoner:SkrivTilLogg(cLogg,
                        '  Ordertype: ' + (IF tmpBxPickinglist.Ordertype = ? THEN '?' ELSE tmpBxPickinglist.Ordertype) 
                        ).    
                END. 

                Cmd:Parameters:Clear().
                Cmd:Parameters:AddWithValue("@Warehouse",tmpBxPickinglist.Warehouseno) NO-ERROR.
                Cmd:Parameters:AddWithValue("@Orderno",tmpBxPickinglist.Orderno) NO-ERROR.
                Cmd:Parameters:AddWithValue("@Date",tmpBxPickinglist.dDate) NO-ERROR.
                Cmd:Parameters:AddWithValue("@DelDate",tmpBxPickinglist.DelDate) NO-ERROR.
                Cmd:Parameters:AddWithValue("@Customerno",tmpBxPickinglist.Customerno) NO-ERROR.
                Cmd:Parameters:AddWithValue("@customername",tmpBxPickinglist.customername) NO-ERROR.
                Cmd:Parameters:AddWithValue("@Projectname",tmpBxPickinglist.Projectname) NO-ERROR.
                Cmd:Parameters:AddWithValue("@Orderstatus",tmpBxPickinglist.Orderstatus) NO-ERROR.
                Cmd:Parameters:AddWithValue("@Ordertype",tmpBxPickinglist.Ordertype) NO-ERROR.

                Cmd:CommandText = cSqlUpdate NO-ERROR.
                /* Fanger eventuelle feilmeldinger.. */
                IF ERROR-STATUS:ERROR THEN 
                DO ix = 1 TO ERROR-STATUS:NUM-MESSAGES:        
                    rStandardFunksjoner:SkrivTilLogg(cLogg,
                        '** CommandText - FeilNr: '+ STRING(ERROR-STATUS:GET-NUMBER(ix)) + ' ' + ERROR-STATUS:GET-MESSAGE(ix) 
                        ).
                END.
                
                iId = Cmd:ExecuteScalar() NO-ERROR.
                IF ERROR-STATUS:ERROR THEN 
                DO:
                    DO ix = 1 TO ERROR-STATUS:NUM-MESSAGES:        
                        rStandardFunksjoner:SkrivTilLogg(cLogg,
                            '** ExecuteScalar - FeilNr: '+ STRING(ERROR-STATUS:GET-NUMBER(ix)) + ' ' + ERROR-STATUS:GET-MESSAGE(ix) 
                            ).
                    END.
                    rStandardFunksjoner:SkrivTilLogg(cLogg,
                        '** cSqlUpdate: ' + cSqlUpdate 
                        ).
                END.
                IF (iId = ?) THEN
                DO:
                    iNye = iNye + 1.
                    Cmd:CommandText = cSqlInsert.
                    iId = Cmd:ExecuteScalar().
                END.
                ELSE iEndret = iEndret + 1.

                /* Tar vare på leverandørens tildelte id */
                ASSIGN 
                tmpBxPickinglist.Id = iId
                    .
                
                /* Posterer ordrelinjene til plukklisten. */
                pbOk = posterBxPickingListLine(INPUT tmpBxPickingList.Warehouseno,
                                               INPUT tmpBxPickingList.Orderno).
                
                CATCH zeroError AS Progress.Lang.AppError:
                    DO ix = 1 TO zeroError:NumMessages.
                        rStandardFunksjoner:SkrivTilLogg(cLogg,
                            '** Feil: ' + zeroError:GetMessage(ix) 
                            ).
                    END.
                END CATCH.
                CATCH oneError AS Progress.Lang.SysError:
                    DO ix = 1 TO oneError:NumMessages.
                        rStandardFunksjoner:SkrivTilLogg(cLogg,
                            '** Feil: ' + oneError:GetMessage(ix) 
                            ).
                    END.
                END CATCH.                
                CATCH twoError AS Progress.Lang.ProError:
                    DO ix = 1 TO twoError:NumMessages.
                        rStandardFunksjoner:SkrivTilLogg(cLogg,
                            '** Feil: ' + twoError:GetMessage(ix) 
                        ).
                    END.
                END CATCH.    
        END. /* LOOPEN*/

        /* Fanger eventuelle feilmeldinger.. */
        IF ERROR-STATUS:ERROR THEN 
        DO ix = 1 TO ERROR-STATUS:NUM-MESSAGES:        
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '** BxPickinglist - FeilNr: '+ STRING(ERROR-STATUS:GET-NUMBER(ix)) + ' ' + ERROR-STATUS:GET-MESSAGE(ix) 
                ).
        END.
        ELSE bResult = TRUE. 

        rStandardFunksjoner:SkrivTilLogg(cLogg,
            '  Nye   : ' + STRING(iNye) + '.'
            ).
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            '  Endret: ' + STRING(iEndret) + '.'
            ).
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            '  Totalt: ' + STRING(iTotalt) + '.'
            ).
        
        /* Flagger at det har gått bra. */
        IF NOT ERROR-STATUS:ERROR THEN 
            bResult = TRUE. 
        
        RETURN bResult.

        CATCH zeroError AS Progress.Lang.AppError:
            DO ix = 1 TO oneError:NumMessages.
                rStandardFunksjoner:SkrivTilLogg(cLogg,
                    '** ENDFeil: ' + oneError:GetMessage(ix) 
                    ).
            END.
        END CATCH.
        CATCH oneError AS Progress.Lang.SysError:
            DO ix = 1 TO oneError:NumMessages.
                rStandardFunksjoner:SkrivTilLogg(cLogg,
                    '** ENDFeil: ' + oneError:GetMessage(ix) 
                    ).
            END.
        END CATCH.                
        CATCH twoError AS Progress.Lang.ProError:
            DO ix = 1 TO twoError:NumMessages.
                rStandardFunksjoner:SkrivTilLogg(cLogg,
                    '** ENDFeil: ' + twoError:GetMessage(ix) 
                    ).
            END.
        END CATCH.    

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC LOGICAL posterBxPickingListLine( INPUT pcWarehouseno AS CHARACTER,
                                                   INPUT piOrderno AS INT64 ):
        
        DEFINE VARIABLE bResult AS LOGICAL NO-UNDO.
        DEFINE VARIABLE iId        AS INT64   NO-UNDO.
        DEFINE VARIABLE cSqlUpdate AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cSqlInsert AS CHARACTER NO-UNDO.

        ASSIGN 
            bResult = FALSE 
            .
        cSqlUpdate =
            "UPDATE [BxPickinglistLine]
            SET [Productno]=@Productno
               ,[Productname]=@Productname
               ,[Location]=@Location
               ,[Warehouse]=@Warehouse
               ,[DelDate]=@DelDate
               ,[Price]=@Price
               ,[PickingQuantity]=@PickingQuantity
               ,[Unit]=@Unit
               ,[Unitname]=@Unitname
               ,[GTIN]=@GTIN
           OUTPUT INSERTED.ID
           WHERE Warehouse= @Warehouse AND Orderno= @Orderno AND Orderline= @Orderline".

        cSqlInsert =
            "INSERT INTO [BxPickinglistLine]
               ([Warehouse]
               ,[Orderno]
               ,[Orderline]
               ,[Productno]
               ,[Productname]
               ,[Location]
               ,[DelDate]
               ,[Price]
               ,[PickingQuantity]
               ,[Unit]
               ,[Unitname]
               ,[GTIN])
         OUTPUT INSERTED.ID
         VALUES
               (@Warehouse
               ,@Orderno
               ,@Orderline
               ,@Productno
               ,@Productname
               ,@Location
               ,@DelDate
               ,@Price
               ,@PickingQuantity
               ,@Unit
               ,@Unitname
               ,@GTIN)".
        
        LINJELOOPEN:
        FOR EACH tmpBxPickinglistLine WHERE 
            tmpBxPickinglistLine.Warehouseno = pcWarehouseno AND
            tmpBxPickinglistLine.Orderno     = piOrderno:

            Cmd:Parameters:Clear().
            Cmd:Parameters:AddWithValue("@Warehouse",tmpBxPickinglistLine.Warehouseno) NO-ERROR.
            Cmd:Parameters:AddWithValue("@Orderno",tmpBxPickinglistLine.Orderno) NO-ERROR.
            Cmd:Parameters:AddWithValue("@Orderline",tmpBxPickinglistLine.Orderline) NO-ERROR.
            Cmd:Parameters:AddWithValue("@Productno",tmpBxPickinglistLine.Productno) NO-ERROR.
            Cmd:Parameters:AddWithValue("@Productname",tmpBxPickinglistLine.Productname) NO-ERROR.
            Cmd:Parameters:AddWithValue("@Location",tmpBxPickinglistLine.Location) NO-ERROR.
            Cmd:Parameters:AddWithValue("@DelDate",tmpBxPickinglistLine.DelDate) NO-ERROR.
            Cmd:Parameters:AddWithValue("@Price",DEC(tmpBxPickinglistLine.Price)) NO-ERROR.
            Cmd:Parameters:AddWithValue("@PickingQuantity",DEC(tmpBxPickinglistLine.PickingQuantity)) NO-ERROR.
            Cmd:Parameters:AddWithValue("@Unit",tmpBxPickinglistLine.Unit) NO-ERROR.
            Cmd:Parameters:AddWithValue("@Unitname",tmpBxPickinglistLine.Unitname) NO-ERROR.
            Cmd:Parameters:AddWithValue("@GTIN",tmpBxPickinglistLine.GTIN) NO-ERROR.

            Cmd:CommandText = cSqlUpdate NO-ERROR.
            /* Fanger eventuelle feilmeldinger.. */
            IF ERROR-STATUS:ERROR THEN 
            DO ix = 1 TO ERROR-STATUS:NUM-MESSAGES:        
                rStandardFunksjoner:SkrivTilLogg(cLogg,
                    '** CommandTextLinje - FeilNr: '+ STRING(ERROR-STATUS:GET-NUMBER(ix)) + ' ' + ERROR-STATUS:GET-MESSAGE(ix) 
                    ).
            END.
                
            iId = Cmd:ExecuteScalar() NO-ERROR.
            IF ERROR-STATUS:ERROR THEN 
            DO:
                DO ix = 1 TO ERROR-STATUS:NUM-MESSAGES:        
                    rStandardFunksjoner:SkrivTilLogg(cLogg,
                        '** ExecuteScalarLinje - FeilNr: '+ STRING(ERROR-STATUS:GET-NUMBER(ix)) + ' ' + ERROR-STATUS:GET-MESSAGE(ix) 
                        ).
                END.
                rStandardFunksjoner:SkrivTilLogg(cLogg,
                    '** cSqlUpdate: ' + cSqlUpdate 
                    ).
            END.
            IF (iId = ?) THEN
            DO:
                Cmd:CommandText = cSqlInsert.
                iId = Cmd:ExecuteScalar().
            END.

            /* Tar vare på leverandørens tildelte id */
            ASSIGN 
                tmpBxPickinglistLine.Id = iId
                .
            
        END. /* LINJELOOPEN */

        RETURN bResult.

        CATCH zeroError AS Progress.Lang.AppError:
            DO ix = 1 TO zeroError:NumMessages.
                rStandardFunksjoner:SkrivTilLogg(cLogg,
                    '** ENDFeil: ' + zeroError:GetMessage(ix) 
                    ).
            END.
        END CATCH.
        CATCH oneError AS Progress.Lang.SysError:
            DO ix = 1 TO oneError:NumMessages.
                rStandardFunksjoner:SkrivTilLogg(cLogg,
                    '** ENDFeil: ' + oneError:GetMessage(ix) 
                    ).
            END.
        END CATCH.                
        CATCH twoError AS Progress.Lang.ProError:
            DO ix = 1 TO twoError:NumMessages.
                rStandardFunksjoner:SkrivTilLogg(cLogg,
                    '** ENDFeil: ' + twoError:GetMessage(ix) 
                    ).
            END.
        END CATCH.    

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC LOGICAL posterBxProduct( INPUT hDataset AS HANDLE ):
        
        DEFINE VARIABLE iId        AS INT64   NO-UNDO.
        DEFINE VARIABLE cSqlUpdate AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cSqlInsert AS CHARACTER NO-UNDO.
        DEFINE VARIABLE bResult    AS LOG       NO-UNDO.
        DEFINE VARIABLE iNye       AS INTEGER NO-UNDO.
        DEFINE VARIABLE iEndret    AS INTEGER NO-UNDO.
        DEFINE VARIABLE itotalt    AS INTEGER NO-UNDO.
        
        DEFINE VARIABLE iBuffer    AS INTEGER   NO-UNDO.
        DEFINE VARIABLE hBuffer    AS HANDLE    NO-UNDO EXTENT 40.
        DEFINE VARIABLE hQuery     AS HANDLE    NO-UNDO EXTENT 40.

        ASSIGN 
            bResult = FALSE 
            .
        
        IF bTest THEN 
        DO:
            cTekst = 'konv\BxProductSkriv' + cDatoTid + '.JSon'.
            hDataset:WRITE-JSON('file', cTekst, TRUE).
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '  Dataset: ' + cTekst 
                ).    
        END. 

        cSqlUpdate =
            "UPDATE [BxProduct]
         SET ProductName=@ProductName
             ,GTIN=@GTIN
             ,Price=@Price
             ,Cost=@Cost
             ,Location=@Location
             ,Unit=@Unit
             ,UnitName=@UnitName
          OUTPUT INSERTED.ID
          where Warehouse= @Warehouse AND Productno= @Productno".

        cSqlInsert =
            "INSERT INTO [BxProduct]
               ([Warehouse]
               ,[Productno]
               ,[ProductName]
               ,[GTIN]
               ,[Price]
               ,[Cost]
               ,[Location]
               ,[Unit]
               ,[UnitName])
         OUTPUT INSERTED.ID
         VALUES
               (@Warehouse
               ,@Productno
               ,@ProductName
               ,@GTIN
               ,@Price
               ,@Cost
               ,@Location
               ,@Unit
               ,@UnitName)".

        Cmd = Conn:CreateCommand() NO-ERROR.
        /* Fanger eventuelle feilmeldinger.. */
        IF ERROR-STATUS:ERROR THEN 
        DO ix = 1 TO ERROR-STATUS:NUM-MESSAGES:        
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '** CreateCommand - FeilNr: '+ STRING(ERROR-STATUS:GET-NUMBER(ix)) + ' ' + ERROR-STATUS:GET-MESSAGE(ix) 
                ).
        END.

        IF bTest THEN 
        DO:
            cTekst = '    BxProduktEtter cmd Create'.
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '  Dataset: ' + cTekst 
                ).    
        END. 

        /* Loop through dataset buffers */
        LOOPEN:
        DO iBuffer = 1 TO hDataset:NUM-BUFFERS ON ERROR UNDO, LEAVE:
            hBuffer[iBuffer] = hDataset:GET-BUFFER-HANDLE(iBuffer).
            CREATE QUERY hQuery[iBuffer].
            hQuery[iBuffer]:SET-BUFFERS(hBuffer[iBuffer]).
            hQuery[iBuffer]:QUERY-PREPARE("FOR EACH " + hBuffer[iBuffer]:NAME).
            hQuery[iBuffer]:QUERY-OPEN().
            hQuery[iBuffer]:GET-FIRST().

            /* Loop through the records in each buffer */
            DO WHILE NOT hQuery[iBuffer]:QUERY-OFF-END ON ERROR UNDO, LEAVE:
                itotalt = itotalt + 1.

                IF bTest AND (
                              hBuffer[iBuffer]:BUFFER-FIELD('Warehouse'):BUFFER-VALUE() = ? OR 
                              hBuffer[iBuffer]:BUFFER-FIELD('Productno'):BUFFER-VALUE() = ? OR
                              hBuffer[iBuffer]:BUFFER-FIELD('ProductName'):BUFFER-VALUE() = ? OR
                              hBuffer[iBuffer]:BUFFER-FIELD('GTIN'):BUFFER-VALUE() = ? OR
                              hBuffer[iBuffer]:BUFFER-FIELD('Price'):BUFFER-VALUE() = ? OR
                              hBuffer[iBuffer]:BUFFER-FIELD('Cost'):BUFFER-VALUE() = ? OR
                              hBuffer[iBuffer]:BUFFER-FIELD('Location'):BUFFER-VALUE() = ? OR
                              hBuffer[iBuffer]:BUFFER-FIELD('Unit'):BUFFER-VALUE() = ? OR 
                              hBuffer[iBuffer]:BUFFER-FIELD('UnitName'):BUFFER-VALUE() = ?
                              ) THEN 
                DO:
                    rStandardFunksjoner:SkrivTilLogg(cLogg,
                        '  Warehouse: ' + hBuffer[iBuffer]:BUFFER-FIELD('Warehouse'):BUFFER-VALUE() 
                        ).    
                    rStandardFunksjoner:SkrivTilLogg(cLogg,
                        '  Productno: ' + hBuffer[iBuffer]:BUFFER-FIELD('Productno'):BUFFER-VALUE() 
                        ).    
                    rStandardFunksjoner:SkrivTilLogg(cLogg,
                        '  ProductName: ' + hBuffer[iBuffer]:BUFFER-FIELD('ProductName'):BUFFER-VALUE() 
                        ).    
                    rStandardFunksjoner:SkrivTilLogg(cLogg,
                        '  GTIN: ' + hBuffer[iBuffer]:BUFFER-FIELD('GTIN'):BUFFER-VALUE() 
                        ).    
                    rStandardFunksjoner:SkrivTilLogg(cLogg,
                        '  Price: ' + hBuffer[iBuffer]:BUFFER-FIELD('Price'):BUFFER-VALUE() 
                        ).    
                    rStandardFunksjoner:SkrivTilLogg(cLogg,
                        '  Cost: ' + hBuffer[iBuffer]:BUFFER-FIELD('Cost'):BUFFER-VALUE() 
                        ).    
                    rStandardFunksjoner:SkrivTilLogg(cLogg,
                        '  Location: ' + hBuffer[iBuffer]:BUFFER-FIELD('Location'):BUFFER-VALUE() 
                        ).    
                    rStandardFunksjoner:SkrivTilLogg(cLogg,
                        '  Unit: ' + hBuffer[iBuffer]:BUFFER-FIELD('Unit'):BUFFER-VALUE() 
                        ).    
                    rStandardFunksjoner:SkrivTilLogg(cLogg,
                        '  Unitname: ' + hBuffer[iBuffer]:BUFFER-FIELD('Unit'):BUFFER-VALUE() 
                        ).    
                END. 

                Cmd:Parameters:Clear().
                Cmd:Parameters:AddWithValue("@Warehouse",hBuffer[iBuffer]:BUFFER-FIELD('Warehouse'):BUFFER-VALUE()) NO-ERROR.
                Cmd:Parameters:AddWithValue("@Productno",hBuffer[iBuffer]:BUFFER-FIELD('Productno'):BUFFER-VALUE()) NO-ERROR.
                Cmd:Parameters:AddWithValue("@ProductName",hBuffer[iBuffer]:BUFFER-FIELD('ProductName'):BUFFER-VALUE()) NO-ERROR.
                Cmd:Parameters:AddWithValue("@GTIN",hBuffer[iBuffer]:BUFFER-FIELD('GTIN'):BUFFER-VALUE()) NO-ERROR.
                Cmd:Parameters:AddWithValue("@Price",hBuffer[iBuffer]:BUFFER-FIELD('Price'):BUFFER-VALUE()) NO-ERROR.
                Cmd:Parameters:AddWithValue("@Cost",hBuffer[iBuffer]:BUFFER-FIELD('Cost'):BUFFER-VALUE()) NO-ERROR.
                Cmd:Parameters:AddWithValue("@Location",hBuffer[iBuffer]:BUFFER-FIELD('Location'):BUFFER-VALUE()) NO-ERROR.
                Cmd:Parameters:AddWithValue("@Unit",hBuffer[iBuffer]:BUFFER-FIELD('Unit'):BUFFER-VALUE()) NO-ERROR.
                Cmd:Parameters:AddWithValue("@UnitName",hBuffer[iBuffer]:BUFFER-FIELD('UnitName'):BUFFER-VALUE()) NO-ERROR.

                Cmd:CommandText = cSqlUpdate NO-ERROR.
                /* Fanger eventuelle feilmeldinger.. */
                IF ERROR-STATUS:ERROR THEN 
                DO ix = 1 TO ERROR-STATUS:NUM-MESSAGES:        
                    rStandardFunksjoner:SkrivTilLogg(cLogg,
                        '** CommandText - FeilNr: '+ STRING(ERROR-STATUS:GET-NUMBER(ix)) + ' ' + ERROR-STATUS:GET-MESSAGE(ix) 
                        ).
                END.
                
                iId = Cmd:ExecuteScalar() NO-ERROR.
                IF ERROR-STATUS:ERROR THEN 
                DO:
                    DO ix = 1 TO ERROR-STATUS:NUM-MESSAGES:        
                        rStandardFunksjoner:SkrivTilLogg(cLogg,
                            '** ExecuteScalar - FeilNr: '+ STRING(ERROR-STATUS:GET-NUMBER(ix)) + ' ' + ERROR-STATUS:GET-MESSAGE(ix) 
                            ).
                    END.
                    rStandardFunksjoner:SkrivTilLogg(cLogg,
                        '** cSqlUpdate: ' + cSqlUpdate 
                        ).
                END.
                IF (iId = ?) THEN
                DO:
                    iNye = iNye + 1.
                    Cmd:CommandText = cSqlInsert.
                    iId = Cmd:ExecuteScalar().
                END.
                ELSE iEndret = iEndret + 1.

                /* Tar vare på leverandørens tildelte id */
                ASSIGN 
                    hBuffer[iBuffer]:BUFFER-FIELD('Id'):BUFFER-VALUE() = iId
                    .
                hQuery[iBuffer]:GET-NEXT().
                
                CATCH zeroError AS Progress.Lang.AppError:
                    rStandardFunksjoner:SkrivTilLogg(cLogg,
                        '** Feil: ' + zeroError:GetMessage(1) 
                        ).
                END CATCH.
                CATCH oneError AS Progress.Lang.SysError:
                    rStandardFunksjoner:SkrivTilLogg(cLogg,
                        '** Feil: ' + oneError:GetMessage(1) 
                        ).
                END CATCH.                
                CATCH twoError AS Progress.Lang.ProError:
                    rStandardFunksjoner:SkrivTilLogg(cLogg,
                        '** Feil: ' + twoError:GetMessage(1) 
                        ).
                END CATCH.    
            END.
        END. /* LOOPEN*/

        /* Fanger eventuelle feilmeldinger.. */
        IF ERROR-STATUS:ERROR THEN 
        DO ix = 1 TO ERROR-STATUS:NUM-MESSAGES:        
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '** BxProduct - FeilNr: '+ STRING(ERROR-STATUS:GET-NUMBER(ix)) + ' ' + ERROR-STATUS:GET-MESSAGE(ix) 
                ).
        END.
        ELSE bResult = TRUE. 

        rStandardFunksjoner:SkrivTilLogg(cLogg,
            '  Nye   : ' + STRING(iNye) + '.'
            ).
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            '  Endret: ' + STRING(iEndret) + '.'
            ).
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            '  Totalt: ' + STRING(iTotalt) + '.'
            ).
        
        /* Flagger at det har gått bra. */
        IF NOT ERROR-STATUS:ERROR THEN 
            bResult = TRUE. 
        
        RETURN bResult.

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

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC LOGICAL posterBxUnit( INPUT hDataset AS HANDLE ):
        
        DEFINE VARIABLE iId        AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cSqlUpdate AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cSqlInsert AS CHARACTER NO-UNDO.
        DEFINE VARIABLE bResult    AS LOG       NO-UNDO.
        
        DEFINE VARIABLE iBuffer    AS INTEGER   NO-UNDO.
        DEFINE VARIABLE hBuffer    AS HANDLE    NO-UNDO EXTENT 40.
        DEFINE VARIABLE hQuery     AS HANDLE    NO-UNDO EXTENT 40.

        ASSIGN 
            bResult = FALSE 
            .
        
        IF bTest THEN 
        DO:
            cTekst = 'konv\BxUnitSkriv' + cDatoTid + '.JSon'.
            hDataset:WRITE-JSON('file', cTekst, TRUE).
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '  Dataset: ' + cTekst 
                ).    
        END. 
    
        cSqlUpdate =
            "UPDATE [BxUnit]
            SET  GTIN=@GTIN
              ,Price=@Price
              ,Unit= @Unit
              ,Unitname=@Unitname
            OUTPUT INSERTED.Id
            WHERE Productno= @Productno and Unit= @Unit".

        cSqlInsert =
            "INSERT INTO [BxUnit]
               ([Productno]
               ,[Unit]
               ,[GTIN]
               ,[Price]
               ,[Unitname])
           OUTPUT INSERTED.ID
           VALUES
               (@Productno
               ,@Unit
               ,@GTIN
               ,@Price
               ,@Unitname)".
        
        Cmd = Conn:CreateCommand().

        /* Loop through dataset buffers */
        LOOPEN:
        DO iBuffer = 1 TO hDataset:NUM-BUFFERS ON ERROR UNDO, LEAVE:
            hBuffer[iBuffer] = hDataset:GET-BUFFER-HANDLE(iBuffer).
            CREATE QUERY hQuery[iBuffer].
            hQuery[iBuffer]:SET-BUFFERS(hBuffer[iBuffer]).
            hQuery[iBuffer]:QUERY-PREPARE("FOR EACH " + hBuffer[iBuffer]:NAME).
            hQuery[iBuffer]:QUERY-OPEN().
            hQuery[iBuffer]:GET-FIRST().
      
            /* Loop through the records in each buffer */
            DO WHILE NOT hQuery[iBuffer]:QUERY-OFF-END:
    
                Cmd:Parameters:Clear().
                Cmd:Parameters:AddWithValue("@Productno",hBuffer[iBuffer]:BUFFER-FIELD('Productno'):BUFFER-VALUE()).
                Cmd:Parameters:AddWithValue("@Unit",hBuffer[iBuffer]:BUFFER-FIELD('Unit'):BUFFER-VALUE()).
                Cmd:Parameters:AddWithValue("@GTIN",hBuffer[iBuffer]:BUFFER-FIELD('GTIN'):BUFFER-VALUE()).
                Cmd:Parameters:AddWithValue("@Price",hBuffer[iBuffer]:BUFFER-FIELD('Price'):BUFFER-VALUE()).
                Cmd:Parameters:AddWithValue("@UnitName",hBuffer[iBuffer]:BUFFER-FIELD('Unitname'):BUFFER-VALUE()).
            
                Cmd:CommandText = cSqlUpdate.
                
                iId = Cmd:ExecuteScalar().
                IF (iId = ?) THEN
                DO:
                    Cmd:CommandText = cSqlInsert.
                    iId = Cmd:ExecuteScalar().
                END.
                
                /* Tar vare på butikkens tildelte ID */
                ASSIGN 
                    hBuffer[iBuffer]:BUFFER-FIELD('Id'):BUFFER-VALUE() = iId
                    . 
                hQuery[iBuffer]:GET-NEXT().
            END.
        END. /* LOOPEN*/

        /* Fanger eventuelle feilmeldinger.. */
        IF ERROR-STATUS:ERROR THEN 
        DO ix = 1 TO ERROR-STATUS:NUM-MESSAGES:        
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '** BxUnit - FeilNr: '+ STRING(ERROR-STATUS:GET-NUMBER(ix)) + ' ' + ERROR-STATUS:GET-MESSAGE(ix) 
                ).
        END.
        ELSE bResult = TRUE. 
        
        RETURN bResult.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC LOGICAL posterClientProfile( INPUT-OUTPUT hDataset AS HANDLE ):
        
        DEFINE VARIABLE iId        AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cSqlUpdate AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cSqlInsert AS CHARACTER NO-UNDO.
        DEFINE VARIABLE bResult    AS LOG       NO-UNDO.
        
        DEFINE VARIABLE iBuffer    AS INTEGER   NO-UNDO.
        DEFINE VARIABLE hBuffer    AS HANDLE    NO-UNDO EXTENT 40.
        DEFINE VARIABLE hQuery     AS HANDLE    NO-UNDO EXTENT 40.

        ASSIGN 
            bResult = FALSE 
            .
        
        IF bTest THEN 
        DO:
            cTekst = 'konv\ClientProfileSkriv' + cDatoTid + '.JSon'.
            hDataset:WRITE-JSON('file', cTekst, TRUE).
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '  Dataset: ' + cTekst 
                ).    
        END. 
    
        cSqlUpdate =
            "UPDATE [ClientProfile]
            SET  Profilename=@Profilename
              ,Comment=@Comment
              ,Language=@Language
            OUTPUT INSERTED.Id
            WHERE CompanyId= @CompanyId".

        cSqlInsert =
            "INSERT INTO [ClientProfile]
               ([CompanyId]
               ,[Profilename]
               ,[Comment]
               ,[Language])
           OUTPUT INSERTED.ID
           VALUES
               (@CompanyId
               ,@Profilename
               ,@Comment
               ,@Language)".
        
        Cmd = Conn:CreateCommand().

        /* Loop through dataset buffers */
        LOOPEN:
        DO iBuffer = 1 TO hDataset:NUM-BUFFERS:
            hBuffer[iBuffer] = hDataset:GET-BUFFER-HANDLE(iBuffer).
            CREATE QUERY hQuery[iBuffer].
            hQuery[iBuffer]:SET-BUFFERS(hBuffer[iBuffer]).
            hQuery[iBuffer]:QUERY-PREPARE("FOR EACH " + hBuffer[iBuffer]:NAME).
            hQuery[iBuffer]:QUERY-OPEN().
            hQuery[iBuffer]:GET-FIRST().
      
            /* Loop through the records in each buffer */
            DO WHILE NOT hQuery[iBuffer]:QUERY-OFF-END:
    
                Cmd:Parameters:Clear().
                Cmd:Parameters:AddWithValue("@CompanyId",hBuffer[iBuffer]:BUFFER-FIELD('CompanyId'):BUFFER-VALUE()).
                Cmd:Parameters:AddWithValue("@Profilename",hBuffer[iBuffer]:BUFFER-FIELD('Profilename'):BUFFER-VALUE()).
                Cmd:Parameters:AddWithValue("@Comment",hBuffer[iBuffer]:BUFFER-FIELD('Comment'):BUFFER-VALUE()).
                Cmd:Parameters:AddWithValue("@Language",hBuffer[iBuffer]:BUFFER-FIELD('Language'):BUFFER-VALUE()).
            
                Cmd:CommandText = cSqlUpdate.
                IF ERROR-STATUS:ERROR THEN 
                DO ix = 1 TO ERROR-STATUS:NUM-MESSAGES:        
                    rStandardFunksjoner:SkrivTilLogg(cLogg,
                        '** SQLUpdate: '+ STRING(ERROR-STATUS:GET-NUMBER(ix)) + ' ' + ERROR-STATUS:GET-MESSAGE(ix) 
                        ).
                END.
                
                iId = Cmd:ExecuteScalar() NO-ERROR.
                IF ERROR-STATUS:ERROR THEN 
                DO ix = 1 TO ERROR-STATUS:NUM-MESSAGES:        
                    rStandardFunksjoner:SkrivTilLogg(cLogg,
                        '** SQLUpdateExecute: '+ STRING(ERROR-STATUS:GET-NUMBER(ix)) + ' ' + ERROR-STATUS:GET-MESSAGE(ix) 
                        ).
                END.

                IF (iId = ?) THEN
                DO:
                    Cmd:CommandText = cSqlInsert NO-ERROR.
                    IF ERROR-STATUS:ERROR THEN 
                    DO ix = 1 TO ERROR-STATUS:NUM-MESSAGES:        
                        rStandardFunksjoner:SkrivTilLogg(cLogg,
                            '** SQLInsert: '+ STRING(ERROR-STATUS:GET-NUMBER(ix)) + ' ' + ERROR-STATUS:GET-MESSAGE(ix) 
                            ).
                    END.
                    
                    iId = Cmd:ExecuteScalar() NO-ERROR.
                    IF ERROR-STATUS:ERROR THEN 
                    DO ix = 1 TO ERROR-STATUS:NUM-MESSAGES:        
                        rStandardFunksjoner:SkrivTilLogg(cLogg,
                            '** SQLInsertExecute: '+ STRING(ERROR-STATUS:GET-NUMBER(ix)) + ' ' + ERROR-STATUS:GET-MESSAGE(ix) 
                            ).
                    END.
                END.
                
                /* Tar vare på butikkens tildelte ID */
                ASSIGN 
                    hBuffer[iBuffer]:BUFFER-FIELD('Id'):BUFFER-VALUE() = iId
                    . 
                hQuery[iBuffer]:GET-NEXT().
            END.
        END. /* LOOPEN*/

        /* Flagger at det har gått bra. */
        IF ERROR-STATUS:ERROR THEN 
        DO ix = 1 TO ERROR-STATUS:NUM-MESSAGES:        
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '** FeilNr: '+ STRING(ERROR-STATUS:GET-NUMBER(ix)) + ' ' + ERROR-STATUS:GET-MESSAGE(ix) 
                ).
        END.
        ELSE bResult = TRUE. 
        
        RETURN bResult.

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

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:    IKKE oppdater denne tabellen foreløpig.
               Avklar med Bxsoftware hvor butikkene skal legges. 
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC LOGICAL posterCompany( INPUT-OUTPUT hDataset AS HANDLE ):
        
        DEFINE VARIABLE iId        AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cSqlUpdate AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cSqlInsert AS CHARACTER NO-UNDO.
        DEFINE VARIABLE bResult    AS LOG       NO-UNDO.
        
        DEFINE VARIABLE iBuffer    AS INTEGER   NO-UNDO.
        DEFINE VARIABLE hBuffer    AS HANDLE    NO-UNDO EXTENT 40.
        DEFINE VARIABLE hQuery     AS HANDLE    NO-UNDO EXTENT 40.

        ASSIGN 
            bResult = FALSE 
            .
        
        IF bTest THEN 
        DO:
            cTekst = 'konv\CompanySkriv' + cDatoTid + '.JSon'.
            hDataset:WRITE-JSON('file', cTekst, TRUE).
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '  Dataset: ' + cTekst 
                ).    
        END. 
    
        cSqlUpdate =     
            "UPDATE [Company]        
         SET  CompanyName=@CompanyName
          OUTPUT INSERTED.Id
          where  Warehouseno= @Warehouseno".
        
        cSqlInsert =     
            "INSERT INTO [Company]
               ([CompanyName]
               ,[Warehouseno])
         OUTPUT INSERTED.ID
         VALUES
               (@CompanyName
               ,@Warehouseno)".
                
        Cmd = Conn:CreateCommand().

        /* Loop through dataset buffers */
        LOOPEN:
        DO iBuffer = 1 TO hDataset:NUM-BUFFERS:
            hBuffer[iBuffer] = hDataset:GET-BUFFER-HANDLE(iBuffer).
            
            CREATE QUERY hQuery[iBuffer].
            hQuery[iBuffer]:SET-BUFFERS(hBuffer[iBuffer]).
            hQuery[iBuffer]:QUERY-PREPARE("FOR EACH " + hBuffer[iBuffer]:NAME).
            hQuery[iBuffer]:QUERY-OPEN().
            hQuery[iBuffer]:GET-FIRST().
      
            /* Loop through the records in each buffer */
            DO WHILE NOT hQuery[iBuffer]:QUERY-OFF-END:
    
    
                Cmd:Parameters:Clear().
                Cmd:Parameters:AddWithValue("@CompanyName",hBuffer[iBuffer]:BUFFER-FIELD('CompanyName'):BUFFER-VALUE()).
                Cmd:Parameters:AddWithValue("@Warehouseno",hBuffer[iBuffer]:BUFFER-FIELD('Warehouseno'):BUFFER-VALUE()).
            
                Cmd:CommandText = cSqlUpdate.
                iId = Cmd:ExecuteScalar().
                IF (iId = ?) THEN
                DO:
                    Cmd:CommandText = cSqlInsert.
                    iId = Cmd:ExecuteScalar().
                END.
                
                /* Tar vare på butikkens tildelte ID */
                ASSIGN 
                    hBuffer[iBuffer]:BUFFER-FIELD('Id'):BUFFER-VALUE() = iId
                    . 

                hQuery[iBuffer]:GET-NEXT().
            END.
        END. /* LOOPEN*/

        /* Flagger at det har gått bra. */
        IF ERROR-STATUS:ERROR THEN 
        DO ix = 1 TO ERROR-STATUS:NUM-MESSAGES:        
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '** FeilNr: '+ STRING(ERROR-STATUS:GET-NUMBER(ix)) + ' ' + ERROR-STATUS:GET-MESSAGE(ix) 
                ).
        END.
        ELSE bResult = TRUE. 
        
        RETURN bResult.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC LOGICAL posterBxSupplier( INPUT-OUTPUT hDataset AS HANDLE ):
    
        DEFINE VARIABLE iId AS INTEGER NO-UNDO.
        DEFINE VARIABLE cSqlUpdate AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cSqlInsert AS CHARACTER NO-UNDO.
        DEFINE VARIABLE bResult AS LOG NO-UNDO.
        
        DEFINE VARIABLE iBuffer AS INTEGER NO-UNDO.
        DEFINE VARIABLE hBuffer AS HANDLE NO-UNDO EXTENT 40.
        DEFINE VARIABLE hQuery AS HANDLE NO-UNDO EXTENT 40.

        ASSIGN 
            bResult = FALSE 
            .
        
        IF bTest THEN 
        DO:
            cTekst = 'konv\BxSupplierSkriv' + cDatoTid + '.JSon'.
            hDataset:WRITE-JSON('file', cTekst, TRUE).
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '  Dataset: ' + cTekst 
                ).    
        END. 
    
        cSqlUpdate =     
        "UPDATE [BxSupplier]        
         SET  Suppliername=@Suppliername
             ,Address=@Address
             ,Postcode=@Postcode
             ,Country=@Country
             ,City=@City
             ,DelAddress=@DelAddress
             ,DelPostcode=@DelPostcode
             ,DelCity=@DelCity
             ,DelCountry=@DelCountry
         OUTPUT INSERTED.ID
         WHERE Supplierno= @Supplierno".
        
        cSqlInsert =     
                "INSERT INTO [BxSupplier]
               ([Supplierno]
               ,[Suppliername]
               ,[Address]
               ,[Postcode]
               ,[Country]
               ,[City]
               ,[DelAddress]
               ,[DelPostcode]
               ,[DelCity]
               ,[DelCountry])
         OUTPUT INSERTED.ID
         VALUES
               (@Supplierno
               ,@Suppliername
               ,@Address
               ,@Postcode
               ,@Country
               ,@City
               ,@DelAddress
               ,@DelPostcode
               ,@DelCity
               ,@DelCountry)".
                
        Cmd = Conn:CreateCommand().

        /* Loop through dataset buffers */
        LOOPEN:
        DO iBuffer = 1 TO hDataset:NUM-BUFFERS:
            hBuffer[iBuffer] = hDataset:GET-BUFFER-HANDLE(iBuffer).
            CREATE QUERY hQuery[iBuffer].
            hQuery[iBuffer]:SET-BUFFERS(hBuffer[iBuffer]).
            hQuery[iBuffer]:QUERY-PREPARE("FOR EACH " + hBuffer[iBuffer]:NAME).
                hQuery[iBuffer]:QUERY-OPEN().
                hQuery[iBuffer]:GET-FIRST().
      
                /* Loop through the records in each buffer */
            DO WHILE NOT hQuery[iBuffer]:QUERY-OFF-END:
    
                Cmd:Parameters:Clear().
                Cmd:Parameters:AddWithValue("@Supplierno",hBuffer[iBuffer]:BUFFER-FIELD('Supplierno'):BUFFER-VALUE()).
                Cmd:Parameters:AddWithValue("@Suppliername",hBuffer[iBuffer]:BUFFER-FIELD('Suppliername'):BUFFER-VALUE()).
                Cmd:Parameters:AddWithValue("@Address",hBuffer[iBuffer]:BUFFER-FIELD('Address'):BUFFER-VALUE()).
                Cmd:Parameters:AddWithValue("@Postcode",hBuffer[iBuffer]:BUFFER-FIELD('Postcode'):BUFFER-VALUE()).
                Cmd:Parameters:AddWithValue("@Country",hBuffer[iBuffer]:BUFFER-FIELD('Country'):BUFFER-VALUE()).
                Cmd:Parameters:AddWithValue("@City",hBuffer[iBuffer]:BUFFER-FIELD('City'):BUFFER-VALUE()).
                Cmd:Parameters:AddWithValue("@DelAddress",hBuffer[iBuffer]:BUFFER-FIELD('DelAddress'):BUFFER-VALUE()).
                Cmd:Parameters:AddWithValue("@DelPostcode",hBuffer[iBuffer]:BUFFER-FIELD('DelPostcode'):BUFFER-VALUE()).
                Cmd:Parameters:AddWithValue("@DelCity",hBuffer[iBuffer]:BUFFER-FIELD('DelCity'):BUFFER-VALUE()).
                Cmd:Parameters:AddWithValue("@DelCountry",hBuffer[iBuffer]:BUFFER-FIELD('DelCountry'):BUFFER-VALUE()).
            
                Cmd:CommandText = cSqlUpdate.
                iId = Cmd:ExecuteScalar().
                IF (iId = ?) THEN
                DO:
                    Cmd:CommandText = cSqlInsert.
                    iId = Cmd:ExecuteScalar().
                END.

                /* Tar vare på butikkens tildelte ID */
                ASSIGN 
                    hBuffer[iBuffer]:BUFFER-FIELD('Id'):BUFFER-VALUE() = iId
                    . 
                
                hQuery[iBuffer]:GET-NEXT().
            END.
        END. /* LOOPEN*/
        
        /* Flagger at det har gått bra. */
        IF NOT ERROR-STATUS:ERROR THEN 
            bResult = TRUE. 
        
        RETURN bResult.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC LOGICAL posterUser( INPUT-OUTPUT hDataset AS HANDLE ):
        
        DEFINE VARIABLE iId        AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cSqlUpdate AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cSqlInsert AS CHARACTER NO-UNDO.
        DEFINE VARIABLE bResult    AS LOG       NO-UNDO.
        
        DEFINE VARIABLE iBuffer    AS INTEGER   NO-UNDO.
        DEFINE VARIABLE hBuffer    AS HANDLE    NO-UNDO EXTENT 40.
        DEFINE VARIABLE hQuery     AS HANDLE    NO-UNDO EXTENT 40.

        ASSIGN 
            bResult = FALSE 
            .
        
        IF bTest THEN 
        DO:
            cTekst = 'konv\UserSkriv' + cDatoTid + '.JSon'.
            hDataset:WRITE-JSON('file', cTekst, TRUE).
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '  Dataset: ' + cTekst 
                ).    
        END. 
        cSqlUpdate =
            "UPDATE [User]
            SET  ProfileId=@ProfileId
                ,Password=@Password
                ,Fullname=@Fullname
                ,Enabled=@Enabled
                ,UserType=@UserType
                ,Employee=@Employee
                ,Employeename=@Employeename
            OUTPUT INSERTED.Id
            WHERE Warehouse=@Warehouse and Username= @Username".

        cSqlInsert =
            "INSERT INTO [User]
               ([Username]
               ,[Warehouse]
               ,[ProfileId]
               ,[Password]
               ,[Fullname]
               ,[Enabled]
               ,[UserType]
               ,[Employee])
           OUTPUT INSERTED.Id
           VALUES
               (@Username
               ,@Warehouse
               ,@ProfileId
               ,@Password
               ,@Fullname
               ,@Enabled
               ,@UserType
               ,@Employee
               ,@Employeename)".
        Cmd = Conn:CreateCommand().
        /* Loop through dataset buffers */
        LOOPEN:
        DO iBuffer = 1 TO hDataset:NUM-BUFFERS  ON ERROR UNDO, LEAVE:
            hBuffer[iBuffer] = hDataset:GET-BUFFER-HANDLE(iBuffer).
            CREATE QUERY hQuery[iBuffer].
            hQuery[iBuffer]:SET-BUFFERS(hBuffer[iBuffer]).
            hQuery[iBuffer]:QUERY-PREPARE("FOR EACH " + hBuffer[iBuffer]:NAME).
            hQuery[iBuffer]:QUERY-OPEN().
            hQuery[iBuffer]:GET-FIRST().
            /* Loop through the records in each buffer */
            DO WHILE NOT hQuery[iBuffer]:QUERY-OFF-END ON ERROR UNDO, LEAVE:

                IF bTest AND (
                    hBuffer[iBuffer]:BUFFER-FIELD('Username'):BUFFER-VALUE() = ? OR 
                    hBuffer[iBuffer]:BUFFER-FIELD('Warehouse'):BUFFER-VALUE() = ? OR
                    hBuffer[iBuffer]:BUFFER-FIELD('ProfileId'):BUFFER-VALUE() = ? OR
                    hBuffer[iBuffer]:BUFFER-FIELD('Password'):BUFFER-VALUE() = ? OR
                    hBuffer[iBuffer]:BUFFER-FIELD('Fullname'):BUFFER-VALUE() = ? OR
                    hBuffer[iBuffer]:BUFFER-FIELD('Enabled'):BUFFER-VALUE() = ? OR
                    hBuffer[iBuffer]:BUFFER-FIELD('UserType'):BUFFER-VALUE() = ? OR 
                    hBuffer[iBuffer]:BUFFER-FIELD('Employee'):BUFFER-VALUE() = ? OR 
                    hBuffer[iBuffer]:BUFFER-FIELD('Employeename'):BUFFER-VALUE() = ?
                    ) THEN 
                DO:
                    rStandardFunksjoner:SkrivTilLogg(cLogg,
                        '  Username: ' + hBuffer[iBuffer]:BUFFER-FIELD('Username'):BUFFER-VALUE() 
                        ).    
                    rStandardFunksjoner:SkrivTilLogg(cLogg,
                        '  Warehouse: ' + hBuffer[iBuffer]:BUFFER-FIELD('Warehouse'):BUFFER-VALUE() 
                        ).    
                    rStandardFunksjoner:SkrivTilLogg(cLogg,
                        '  ProfileId: ' + hBuffer[iBuffer]:BUFFER-FIELD('ProfileId'):BUFFER-VALUE() 
                        ).    
                    rStandardFunksjoner:SkrivTilLogg(cLogg,
                        '  Password: ' + hBuffer[iBuffer]:BUFFER-FIELD('Password'):BUFFER-VALUE() 
                        ).    
                    rStandardFunksjoner:SkrivTilLogg(cLogg,
                        '  Fullname: ' + hBuffer[iBuffer]:BUFFER-FIELD('Fullname'):BUFFER-VALUE() 
                        ).    
                    rStandardFunksjoner:SkrivTilLogg(cLogg,
                        '  Enabled: ' + hBuffer[iBuffer]:BUFFER-FIELD('Enabled'):BUFFER-VALUE() 
                        ).    
                    rStandardFunksjoner:SkrivTilLogg(cLogg,
                        '  UserType: ' + hBuffer[iBuffer]:BUFFER-FIELD('UserType'):BUFFER-VALUE() 
                        ).    
                    rStandardFunksjoner:SkrivTilLogg(cLogg,
                        '  Employee: ' + hBuffer[iBuffer]:BUFFER-FIELD('Employee'):BUFFER-VALUE() 
                        ).    
                    rStandardFunksjoner:SkrivTilLogg(cLogg,
                        '  Employeename: ' + hBuffer[iBuffer]:BUFFER-FIELD('Employeename'):BUFFER-VALUE() 
                        ).    
                END.
       
                Cmd:Parameters:Clear().
                Cmd:Parameters:AddWithValue("@Username",hBuffer[iBuffer]:BUFFER-FIELD('Username'):BUFFER-VALUE()).
                Cmd:Parameters:AddWithValue("@Warehouse",hBuffer[iBuffer]:BUFFER-FIELD('Warehouse'):BUFFER-VALUE()).
                Cmd:Parameters:AddWithValue("@ProfileId",hBuffer[iBuffer]:BUFFER-FIELD('ProfileId'):BUFFER-VALUE()).
                Cmd:Parameters:AddWithValue("@Password",hBuffer[iBuffer]:BUFFER-FIELD('Password'):BUFFER-VALUE()).
                Cmd:Parameters:AddWithValue("@Fullname",hBuffer[iBuffer]:BUFFER-FIELD('Fullname'):BUFFER-VALUE()).
                Cmd:Parameters:AddWithValue("@Enabled",hBuffer[iBuffer]:BUFFER-FIELD('Enabled'):BUFFER-VALUE()).
                Cmd:Parameters:AddWithValue("@UserType",hBuffer[iBuffer]:BUFFER-FIELD('UserType'):BUFFER-VALUE()).
                Cmd:Parameters:AddWithValue("@Employee",hBuffer[iBuffer]:BUFFER-FIELD('Employee'):BUFFER-VALUE()).
                Cmd:Parameters:AddWithValue("@Employeename",hBuffer[iBuffer]:BUFFER-FIELD('Employeename'):BUFFER-VALUE()).

                Cmd:CommandText = cSqlUpdate NO-ERROR.
                /* Fanger eventuelle feilmeldinger.. */
                IF ERROR-STATUS:ERROR THEN 
                DO ix = 1 TO ERROR-STATUS:NUM-MESSAGES:        
                    rStandardFunksjoner:SkrivTilLogg(cLogg,
                        '** CommandText - FeilNr: '+ STRING(ERROR-STATUS:GET-NUMBER(ix)) + ' ' + ERROR-STATUS:GET-MESSAGE(ix) 
                        ).
                END.

                iId = Cmd:ExecuteScalar() NO-ERROR.
                IF ERROR-STATUS:ERROR THEN 
                DO ix = 1 TO ERROR-STATUS:NUM-MESSAGES:        
                    rStandardFunksjoner:SkrivTilLogg(cLogg,
                        '** ExecuteScalar - FeilNr: '+ STRING(ERROR-STATUS:GET-NUMBER(ix)) + ' ' + ERROR-STATUS:GET-MESSAGE(ix) 
                        ).
                END.
                
                /* Hvis Update feiler, skal insert kjøres istedenfor. */
                IF (iId = ?) THEN
                DO:
                    Cmd:CommandText = cSqlInsert.
                    iId = Cmd:ExecuteScalar() NO-ERROR.
                END.
                
                /* Tar vare på butikkens tildelte ID */
                ASSIGN 
                    hBuffer[iBuffer]:BUFFER-FIELD('Id'):BUFFER-VALUE() = iId
                    . 
                hQuery[iBuffer]:GET-NEXT().

                CATCH zeroError AS Progress.Lang.AppError:
                    rStandardFunksjoner:SkrivTilLogg(cLogg,
                        '** Feil: ' + zeroError:GetMessage(1) 
                        ).
                END CATCH.
                CATCH oneError AS Progress.Lang.SysError:
                    rStandardFunksjoner:SkrivTilLogg(cLogg,
                        '** Feil: ' + oneError:GetMessage(1) 
                        ).
                END CATCH.                
                CATCH twoError AS Progress.Lang.ProError:
                    rStandardFunksjoner:SkrivTilLogg(cLogg,
                        '** Feil: ' + twoError:GetMessage(1) 
                        ).
                END CATCH.    
            END. 
        END. /* LOOPEN*/

        /* Flagger at det har gått bra. */
        IF ERROR-STATUS:ERROR THEN 
        DO ix = 1 TO ERROR-STATUS:NUM-MESSAGES:        
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '** FeilNr: '+ STRING(ERROR-STATUS:GET-NUMBER(ix)) + ' ' + ERROR-STATUS:GET-MESSAGE(ix) 
                ).
        END.
        ELSE bResult = TRUE. 
        
        RETURN bResult.

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

    DESTRUCTOR PUBLIC LesSkrivBxEngine ( ):
        /* kobler ned forbindelse til SqlServer databasen hvis det ikke er gjort før. */
        /* nedkoblingSqlServer(  ). */

    END DESTRUCTOR.

END CLASS.
