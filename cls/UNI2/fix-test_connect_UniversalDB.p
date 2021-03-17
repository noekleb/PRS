/* fix-test_connect_UniversalDB.p */

USING Progress.Lang.*.
USING System.Data.SqlClient.*.
USING System.Data.*.

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

DEFINE VARIABLE cLogg    AS CHARACTER NO-UNDO.
DEFINE VARIABLE bTest    AS LOG       NO-UNDO.
DEFINE VARIABLE cDatoTid AS CHARACTER NO-UNDO.  
DEFINE VARIABLE cTekst   AS CHARACTER NO-UNDO.  
DEFINE VARIABLE iX       AS INTEGER   NO-UNDO.
DEFINE VARIABLE bOk      AS LOG       NO-UNDO.

/* Kommunikasjonsparametre */ 
ASSIGN 
    cPwd        = 'Uhdsa67RT'
    cUserId     = 'QlickView'
    cServer     = '192.168.100.30'
    cDbName     = 'UniversalDB'
    cDataSource = 'NORGE0047'
    .

RUN oppkoblingSqlServer.

IF bOk THEN
    RUN nedkoblingSqlServer.

PROCEDURE nedkoblingSqlServer:
    bok = FALSE.

    /* kobler ned forbindelse til SqlServer databasen. */
    Conn:Close() NO-ERROR.
    IF NOT ERROR-STATUS:ERROR THEN
    DO: 
        MESSAGE 
            '  Koblet ned forbindelse til Sql server : ' + ConString + '.'
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        bOk = TRUE.
    END.    
        
END PROCEDURE.

PROCEDURE oppkoblingSqlServer:

    bOk = FALSE.
    
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

    Conn = NEW SqlConnection(ConString,SqlCred).

    Conn:Open() NO-ERROR.
    IF ERROR-STATUS:ERROR THEN 
    DO:
        MESSAGE 
            ERROR-STATUS:GET-MESSAGE(1) 
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
    END.
    ELSE 
    DO: 
        MESSAGE 
            'Oppkoblet mot Sql server: ' + ConString + '.'
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        bOk = TRUE.
    END.   

    CmdRead = NEW SqlCommand('', Conn).

    RETURN.

END PROCEDURE.
