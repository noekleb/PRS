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
        cPwd    = 'bxengine'
        cUserId = 'bxengine'
        cServer = 'localhost'
        cDbName = 'bxengine'
        .
ELSE  
    ASSIGN 
        cPwd    = 'BxEngine'
        cUserId = 'bxengine'
        cServer = '192.168.200.186'
        cDbName = 'BxEngine'
        .
DO:
    /* passord -> sec.string */
    SeqString = NEW System.Security.SecureString().
    DO ix = 1 TO LENGTH(cPwd):
        SeqString:AppendChar(SUBSTR(cPwd,ix,1)).
    END.
    SeqString:MakeReadOnly().

    /* brukernavn, pwd */
    SqlCred = NEW SqlCredential(cUserId,SeqString).

    ConString = "Server=" + cServer + ",1433".
    ConString = ConString + ";Database=" + cDbName .

    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  User         : ' + cUserId + CHR(10) +
        '                    Pwd          : ' + cPwd + CHR(10) +
        '                    SQL ConString: ' + ConString 
        ).

    Conn = NEW SqlConnection(ConString,SqlCred).

    Conn:Open() NO-ERROR.
    IF ERROR-STATUS:ERROR THEN 
    DO:
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            '    ' + ERROR-STATUS:GET-MESSAGE(1) 
            ).    
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            '  ** Feil ved oppkobling mot Sql server: ' + ConString + '.'
            ).
    END.
    ELSE 
    DO: 
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            '  Oppkoblet mot Sql server: ' + ConString + '.'
            ).
    END.   
    
    CmdRead = NEW SqlCommand('', Conn).
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
