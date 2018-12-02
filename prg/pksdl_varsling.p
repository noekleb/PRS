
/*------------------------------------------------------------------------
    File        : kordre_varsling.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Sat Feb 21 18:16:05 CET 2015
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER lPkSdlId AS DECIMAL NO-UNDO.

DEFINE VARIABLE cFilNavn AS CHARACTER NO-UNDO.
DEFINE VARIABLE iCl      AS INTEGER   NO-UNDO.
DEFINE VARIABLE iButNr   AS INTEGER   NO-UNDO.
DEFINE VARIABLE cButLst  AS CHARACTER NO-UNDO.

/* smtpoppsett */
DEFINE VARIABLE cMailhub  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cDoAUTH   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cAuthType AS CHARACTER NO-UNDO.
DEFINE VARIABLE cUser     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPassword AS CHARACTER NO-UNDO.

DEFINE VARIABLE cWebButEmail AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMailTo      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cUtfilNavn   AS CHARACTER NO-UNDO.

DEFINE VARIABLE hTempTable   AS HANDLE    NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

{syspara.i 5 1 1 iCl INT}
{syspara.i 50 50 1 cMailhub }
{syspara.i 50 50 2 cDoAUTH  }
{syspara.i 50 50 3 cAuthType}
{syspara.i 50 50 4 cUser    }
{syspara.i 50 50 5 cPassword}
{syspara.i 50 50 33 cMailTo}
{syspar2.i 50 50 33 cButLst}

ASSIGN 
    cUtfilNavn = 'Pakkseddel varemottak'.

/* Ingen varsling satt opp */
IF cButLst = '' THEN
    RETURN.

/*RUN bibl_loggDbFri.p (cUtfilNavn,                                       */
/*    ' kordre_varsling.p (Trigger procedure) - varsling av varemottak.').*/

FIND PkSdlHode NO-LOCK WHERE
  PkSdlHode.PksdlId = lPkSdlId NO-ERROR.  
IF NOT AVAILABLE PkSdlHode THEN 
DO:
    RUN bibl_loggDbFri.p (cUtfilNavn, 
        ' Finner ikke pakkseddel med PksdlId: ' + STRING(lPkSdlId) + '.'). 
    RETURN.
END.

FIND FIRST PkSdlLinje OF PkSdlHode NO-LOCK NO-ERROR.
IF NOT AVAILABLE PksdlLinje THEN 
DO:
    RUN bibl_loggDbFri.p (cUtfilNavn, 
        ' Pakkseddel har ingen varelinjer - PkSdlId ' + STRING(lPkSdlId) + ' OrdreNr ' + PkSdlHode.EkstId + ' PkSdlNr ' + PkSdlHode.PksdlNr + '.'). 
    RETURN.    
END.

iButNr = PkSdlLinje.ButikkNr.
IF NOT CAN-DO(cButLst,STRING(iButNr)) THEN
DO: 
    RUN bibl_loggDbFri.p (cUtfilNavn, 
        ' Butikk ' + STRING(iButNr) + ' finnes ikke i varslingsliste ' + cButLst + '.'). 
    RETURN.
END.  

FIND Butiker NO-LOCK WHERE 
  Butiker.Butik = iCl NO-ERROR.
IF NOT AVAILABLE Butiker THEN 
DO:
    RUN bibl_loggDbFri.p (cUtfilNavn, 
        ' Sentrallager ikke satt opp i syspara 5 1 1, eller ugyldig butikknr er angitt ( ' + STRING(iCL) + ').'). 
    RETURN.
END.
cWebButEmail = Butiker.ePostAdresse.
IF cWebButEmail = '' OR NUM-ENTRIES(cWebButEmail,"@") <> 2 THEN 
DO:
    RUN bibl_loggDbFri.p (cUtfilNavn, 
        ' Avsender eMail ikke satt opp, eller er ugyldig på Sentrallager ( ' + cWebButEmail + ').'). 
    RETURN.
END.

RUN bibl_loggDbFri.p (cUtfilNavn, 
    'Varemottak pakkseddel - PkSdlId ' + STRING(lPkSdlId) + ' OrdreNr ' + PkSdlHode.EkstId + ' PkSdlNr ' + PkSdlHode.PksdlNr + ' fra butikk ' + STRING(iButNr)).


/*-----------------
BUFFER-HANDLE PkSdlHode.
RUN sendEMail.

PROCEDURE SendEmail :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    
    DEFINE VARIABLE lMailOK  AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.

    RUN prssmtpmailv5_7a.p (
    /*mailhub    */   cMailhub,
    /*EmailTo    */   cMailTo,
    /*EmailFrom  */   cWebButEmail,
    /*EmailCC    */   "",
    /*Attachments*/   "" ,
    /*LocalFiles */   "",
    /*Subject    */   'Varemottak pakkseddel - PkSdlId ' + STRING(lPkSdlId) + ' OrdreNr ' + PkSdlHode.EkstId + ' PkSdlNr ' + PkSdlHode.PksdlNr + ' fra butikk ' + STRING(iButNr),
    /*Body       */   "",
    /*MIMEHeader */   "",
    /*BodyType   */   "",
    /*Importance */   0,
    /*L_DoAUTH   */   IF cDoAUTH = "1" THEN TRUE ELSE FALSE,
    /*C_AuthType */   cAuthType,
    /*C_User     */   cUser,
    /*C_Password */   cPassword,
    /*oSuccessful*/  OUTPUT lMailOK,
    /*vMessage   */  OUTPUT cMessage) NO-ERROR.
    
    RUN bibl_loggDbFri.p (cUtfilNavn, 
        ' Mail sendt ' + STRING(lMailOk) + '.'). 
    RUN bibl_loggDbFri.p (cUtfilNavn, 
        ' Mail message: ' + cMessage). 
        
    IF lMailOK = TRUE AND cFilNavn <> "" THEN
        OS-DELETE VALUE(cFilNavn).
END PROCEDURE.
---------------*/
