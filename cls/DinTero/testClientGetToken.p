
/*------------------------------------------------------------------------
    File        : tetClientGetToken.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : tny
    Created     : Tue Nov 06 10:11:11 CET 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE VARIABLE rClientGetToken AS CLASS cls.DinTero.ClientDinTero NO-UNDO.

DEFINE VARIABLE iStatusCode AS INTEGER NO-UNDO.
DEFINE VARIABLE cStatusReason AS CHARACTER NO-UNDO.
DEFINE VARIABLE cToken AS CHARACTER NO-UNDO.
DEFINE VARIABLE iType AS INTEGER NO-UNDO.
DEFINE VARIABLE iX AS INTEGER NO-UNDO.
DEFINE VARIABLE cErorTekst AS CHARACTER NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
rClientGetToken = NEW cls.DinTero.ClientDinTero().

/* Loggfil navn. */
ASSIGN 
    rClientGetToken:cLogg       = 'ClientGetToken' + REPLACE(STRING(TODAY),'/','')
    .

/* Parametre for kall. */
ASSIGN 
    rClientGetToken:cProtocoll  = 'https'    
    rClientGetToken:cPath       = '/v1/accounts/T11112542/auth/token'
    rClientGetToken:cUserDomain = 'tomcat'
    rClientGetToken:cDomainName = 'api.dintero.com'
    rClientGetToken:cUser       = 'c0fab10f-ecc5-4c8d-a152-93fd0ff314ab'
    rClientGetToken:cPwd        = 'b8ceaab2-9051-46cb-8407-76ca7eb96235'
    .

rClientGetToken:getToken( OUTPUT iStatusCode,
                          OUTPUT cStatusReason ) NO-ERROR.
                          
IF iStatusCode = 200 THEN 
    cToken = rClientGetToken:cToken.
ELSE  
    cToken = ''.                              

MESSAGE
    'iStatusCode:' iStatusCode SKIP
    'cStatusReason:' cStatusReason SKIP(1)
    'cToken:' cToken
VIEW-AS ALERT-BOX.
