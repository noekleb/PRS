USING Progress.Lang.*.
USING System.Data.SqlClient.SqlConnection.*.
USING System.Data.SqlClient.*.
USING System.Data.*.

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

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.
rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner() NO-ERROR.

ASSIGN
    cLogg = 'gurresSjekk'
    .

/* Kommunikasjonsparametre */ 
IF SEARCH('tnc.txt') <> ? THEN 
    ASSIGN 
        cPwd        = 'bxengine'
        cUserId     = 'bxengine'
        cServer     = 'localhost'
        cDbName     = 'bxengine'
        cDataSource = 'SP1TOMN-14'
        .
ELSE  
/*    ASSIGN                             */
/*        cPwd        = 'BxWarehouse'    */
/*        cUserId     = 'bxengine'       */
/*        cServer     = '192.168.200.186'*/
/*        cDbName     = 'BxEngine'       */
/*        cDataSource = 'PRS-PGM1'       */
/*        .                              */
      ASSIGN 
        cPwd        = 'Uhdsa67RT'
        cUserId     = 'QlickView'
        cServer     = '192.168.100.30'
        cDbName     = 'StageDB_Universal'
        cDataSource = 'NORGE0047' 
        .
/*       ASSIGN                           */
/*         cPwd        = 'IndexGant'      */
/*          cUserId     = 'administrator' */
/*          cServer     = '192.168.100.29'*/
/*          cDbName     = 'Consignor'     */
/*          cDataSource = 'GANTSQL01'     */
/*          .                             */
DO:
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  TEST-1'
        ).

    /* passord -> sec.string */
    SeqString = NEW System.Security.SecureString().
    DO ix = 1 TO LENGTH(cPwd):
        SeqString:AppendChar(SUBSTR(cPwd,ix,1)).
    END.
    SeqString:MakeReadOnly().

    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  TEST-2'
        ).

        /* brukernavn, pwd */
    SqlCred = NEW SqlCredential(cUserId,SeqString).

    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  TEST-3'
        ).
    
    ConString = "Server=" + cServer + ",1433".
    ConString = ConString + ";Data Source=" + cDataSource +  ";Database=" + cDbName .

    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  User         : ' + cUserId + CHR(10) +
        '                    Pwd          : ' + cPwd + CHR(10) +
        '                    SQL ConString: ' + ConString 
        ).

    Conn = NEW SqlConnection(ConString,SqlCred).

    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  TEST-4'
        ).

    Conn:Open() NO-ERROR.
    IF ERROR-STATUS:ERROR THEN 
    DO:
        DO ix = 1 TO ERROR-STATUS:NUM-MESSAGES:        
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '** Conn - FeilNr: '+ STRING(ERROR-STATUS:GET-NUMBER(ix)) + ' ' + ERROR-STATUS:GET-MESSAGE(ix) 
                ).
        END.
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            '** Feil ved oppkobling av SQL server.'
            ).
    END.
    ELSE DO:
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            'Vellykket oppkobling av SQL server: ' + ConString
            ).
    END.
    
    CmdRead = NEW SqlCommand('', Conn).

    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  TEST-6'
        ).
END.
        
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
