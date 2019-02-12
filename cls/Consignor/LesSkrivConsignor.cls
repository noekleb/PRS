 
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

CLASS cls.Consignor.LesSkrivConsignor: 
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

    {cls\Consignor\tmpTblvShipmentPackages.i}
    {cls\Consignor\tmpDsvShipmentPackages.i}
               
    CONSTRUCTOR PUBLIC LesSkrivConsignor ( INPUT pcLogg AS CHARACTER ):
        SUPER ().

        ASSIGN 
            bTest    = TRUE 
            cLogg    = pcLogg
            cDatoTid = REPLACE(STRING(TODAY),'/','') /*+ REPLACE(STRING(TIME,"HH:MM:SS"),':','')*/
            .

        rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.
                
       ASSIGN
/*         cPwd         = 'IndexGant'*/
          cPwd        = 'polygon09PRS'
          cUserId     = 'PRS'
          cServer     = '192.168.100.29'
          cDbName     = 'Consignor'
          cDataSource = 'GANTSQL01'
          .
                    
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC LOGICAL lesvShipmentPackages( INPUT cOrderNumber AS CHARACTER,
                                                OUTPUT DATASET dsvShipmentPackages ):
        
        DEFINE VARIABLE bResult AS LOGICAL NO-UNDO.
        DEFINE VARIABLE iAntRecord AS INTEGER NO-UNDO.
        DEFINE VARIABLE ishi_ID AS INT64 NO-UNDO.

        EMPTY TEMP-TABLE tmpvShipmentPackages.
        
        ASSIGN cSQL =   
            "SELECT [shi_ID]
                  ,[shi_ref_OrderNumber]
                  ,[shi_number]
                  ,[pac_parcelNumber]
              FROM [Consignor].[dbo].[vShipmentPackages]
              WHERE shi_ref_OrderNumber = '" + cOrderNumber + "'"
                    
        CmdRead:CommandText = cSQL.

        Rdr = CmdRead:ExecuteReader().

        ix = 0.
        LOOPEN:
        DO WHILE Rdr:Read() ON ERROR UNDO, LEAVE:
            ASSIGN 
                ishi_ID = INT(STRING(Rdr["shi_ID"]))                
                .
                
            IF INT(STRING(Rdr["shi_ID"])) > 0 AND  
               NOT CAN-FIND(FIRST tmpvShipmentPackages WHERE tmpvShipmentPackages.shi_ID = ishi_ID) THEN 
            DO:
                CREATE tmpvShipmentPackages.
                ASSIGN 
                    tmpvShipmentPackages.shi_ID              = ishi_ID
                    tmpvShipmentPackages.shi_ref_OrderNumber = STRING(Rdr["shi_ref_OrderNumber"])
                    tmpvShipmentPackages.shi_number          = STRING(Rdr["shi_number"])
                    tmpvShipmentPackages.pac_parcelNumber    = STRING(Rdr["pac_parcelNumber"])
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
        
        IF iAntRecord > 0 AND bTest THEN 
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '  Antall shipment poster lest: ' + STRING(iAntRecord) + '.' 
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
            IF bTest THEN 
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

        IF bTest THEN 
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
            IF bTest THEN  
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

    DESTRUCTOR PUBLIC LesSkrivConsignor ( ):
        /* kobler ned forbindelse til SqlServer databasen hvis det ikke er gjort før. */
        /* nedkoblingSqlServer(  ). */

    END DESTRUCTOR.

END CLASS.
