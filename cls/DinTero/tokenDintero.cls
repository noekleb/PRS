 
 /*------------------------------------------------------------------------
    File        : tokenDintero
    Purpose     : Isolerer koden til denne klassen, og la den gjenbrukes der hvor det er behov for token.
    Syntax      : 
    Description : Henter og lagrer token fra Dintero.
    Author(s)   : tomn
    Created     : Wed Nov 11 13:13:09 CET 2020
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING OpenEdge.Net.HTTP.*.
USING Progress.Json.ObjectModel.JsonObject.

CLASS cls.DinTero.tokenDintero: 
  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/
  DEFINE VARIABLE cUserDomain AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cFullPath AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cUser AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cPwd AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
  DEFINE VARIABLE bTest AS LOG NO-UNDO.
  DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.
  
  DEFINE VARIABLE lcJsonString AS LONGCHAR NO-UNDO.
    
  DEFINE VARIABLE oClient AS IHttpClient NO-UNDO.
  DEFINE VARIABLE oCredentials AS Credentials NO-UNDO.
  DEFINE VARIABLE oJsonRequest AS JsonObject NO-UNDO.
  DEFINE VARIABLE oHttpRequest AS IHttpRequest NO-UNDO.
  DEFINE VARIABLE oHttpResponse AS IHttpResponse NO-UNDO.
  
  DEFINE PROPERTY cAccess_Token AS CHARACTER NO-UNDO 
      GET. 
      SET.
  DEFINE PROPERTY cToken_Type AS CHARACTER NO-UNDO 
      GET. 
      SET.
  DEFINE PROPERTY cAid AS CHARACTER NO-UNDO 
      GET. 
      SET.
  DEFINE PROPERTY dtRead AS DATETIME NO-UNDO 
      GET. 
      SET.
  DEFINE PROPERTY dtExpires AS DATETIME NO-UNDO 
      GET. 
      SET.

  DEFINE TEMP-TABLE ttRequest NO-UNDO 
    FIELD grant_type AS CHARACTER
    FIELD audience   AS CHARACTER.

  DEFINE TEMP-TABLE ttToken NO-UNDO
    FIELD access_token AS CHARACTER FORMAT "x(40)"
    FIELD expires_in AS INTEGER FORMAT ">>>>>>>9"
    FIELD token_type AS CHARACTER FORMAT "x(15)" 
    FIELD ReadDateTime AS DATETIME FORMAT "99/99/9999 HH:MM:SS" 
    FIELD ExpiresDateTime AS DATETIME FORMAT "99/99/9999 HH:MM:SS" 
    .

  DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO. 
    
  CONSTRUCTOR PUBLIC tokenDintero (  ):
    SUPER ().

    rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner() NO-ERROR.

    
    ASSIGN 
      bTest = FALSE
      cLogg = 'tokenDintero.cls' + REPLACE(STRING(TODAY,"99/99/9999"),'/','')
      .
      
     /* Henter systemparameter oppsettet. */
     getSysParaOppkobling().
          
  END CONSTRUCTOR.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC LOGICAL getToken( OUTPUT piStatusCode AS INTEGER,
                                  OUTPUT pcStatusReason AS CHARACTER ):
    
    DEFINE VARIABLE bResult AS LOGICAL INITIAL FALSE NO-UNDO.

    EMPTY TEMP-TABLE ttRequest.
    EMPTY TEMP-TABLE ttToken.

    /* Er token fortsatt gyldig, hentes ikke nytt token. */
    IF sjekkSysParaToken() THEN 
      DO:
        ASSIGN 
          piStatusCode   = 200
          pcStatusReason = 'Ok. (Bruker tidligere hentet token)'
          bResult        = TRUE
          .
        RETURN bResult.
      END.

    /* Henter oppdatert systemparametre for oppkobling. */
    IF NOT getSysparaOppkobling() THEN 
      DO:
        ASSIGN 
          piStatusCode   = 0
          pcStatusReason = 'Manglende systemparameteroppsett.'
          bResult        = FALSE
          .
        RETURN bResult.
      END.
          
    /* Creating a Client. */
    oClient = ClientBuilder:Build()
              :KeepCookies(CookieJarBuilder:Build()
              :CookieJar):Client
              .

    /* Creating a request. */
    oCredentials = NEW Credentials(cUserDomain, cUser, cPwd).
    
    CREATE ttRequest. 
    ASSIGN 
        ttRequest.grant_type = "client_credentials"
        ttRequest.audience   = "https://api.dintero.com/v1/accounts/" + cAid.

    /* Legger ut request payload på en fil. */
    IF bTest THEN 
    DO:      
       rStandardFunksjoner:SkrivTilLogg(cLogg,
         '  REquest: '  
         ).
       rStandardFunksjoner:SkrivTilLogg(cLogg,
         '    cFullPath: ' + cFullPath 
         ).
       rStandardFunksjoner:SkrivTilLogg(cLogg,
         '    audience: ' + ttRequest.audience 
         ).
       rStandardFunksjoner:SkrivTilLogg(cLogg,
         '    cAid: ' + cAid 
         ).
       rStandardFunksjoner:SkrivTilLogg(cLogg,
         '    cUserDomain: ' + cUserDomain 
         ).
       rStandardFunksjoner:SkrivTilLogg(cLogg,
         '    cUser: ' + cUser 
         ).
       rStandardFunksjoner:SkrivTilLogg(cLogg,
         '    cPwd: ' + cPwd 
         ).
      
    END.

    oJsonRequest = NEW JsonObject().
    FOR EACH ttRequest: 
        oJSonRequest:add("grant_type",ttRequest.grant_type). 
        oJSonRequest:add("audience",ttRequest.audience). 
    END.

    IF bTest THEN 
      CAST(oJSonRequest, JSONObject):WriteFile('konv\GetTokenPayload' + STRING(ETIME) + '.json').
    
    oHttpRequest = RequestBuilder:Post(cFullPath) /* URL og JSon object */
                    :UsingBasicAuthentication(oCredentials) /* Sender inn domain, brukerid og passord. */
                    :AcceptJson() /* we want to get JSON back */
                    :AddJSonData(oJSonRequest) /* Her legges JSon meldingen inn */
                    :Request.

    /* Bygger respons objekstet. Det er tomt her, men fylles når forespørselen eksekverees. */
    oHttpResponse = ResponseBuilder:Build():Response.

    /* Execute the request.                                                              */
    /* Sender forespørselen til API'et med preppet request object og tom respons object. */
    oClient:Execute(oHttpRequest, oHttpResponse).     

    /* Processing the responce                     */
    /* Henter ut status koden fra responsobjectet. */
    ASSIGN 
        piStatusCode   = oHttpResponse:StatusCode
        pcStatusReason = oHttpResponse:StatusReason
        cAccess_Token  = ''  
        cToken_Type    = ''
        dtRead         = ?
        dtExpires      = ?  
        .

    IF bTest THEN 
    DO:
       rStandardFunksjoner:SkrivTilLogg(cLogg,
         '  Response:' 
         ).
       rStandardFunksjoner:SkrivTilLogg(cLogg,
         '    piStatusCode: ' + STRING(piStatusCode) 
         ).
       rStandardFunksjoner:SkrivTilLogg(cLogg,
         '    pcStatusReason: ' + pcStatusReason 
         ).
    END.


    IF piStatusCode = 200 THEN
    STATUS200: 
    DO:
      /* Leser og tolker svaret som er en JSon melding og legger det i en longchar variabel. */
      /* Deretter må de enkelte nodene leses ut en for en av JSon meldingen.                 */
      CAST(oHttpResponse:Entity, JsonObject):Write(lcJsonString, TRUE).

      /* Legger ut respons JSon meldingen på en fil. */
      IF bTest THEN 
        CAST(oHttpResponse:Entity, JSONObject):WriteFile('konv\GetTokenRespons' + STRING(ETIME) + '.json').
        
      /* Legger JSon meldingen inn i Token tabellen. */
      TEMP-TABLE ttToken:READ-JSON ("longchar", lcJsonString,"EMPTY").
        
      FIND FIRST ttToken NO-ERROR.
      IF AVAILABLE ttToken THEN 
      DO:
        ASSIGN 
          ttToken.ReadDateTime    = NOW
          ttToken.ExpiresDateTime = NOW + (ttToken.expires_in * 1000)
          .

        ASSIGN
          dtRead        = ttToken.ReadDateTime
          dtExpires     = ttToken.ExpiresDateTime
          cAccess_Token = ttToken.access_token
          cToken_Type   = ttToken.token_type
          bResult       = TRUE
          .
        /* Oppdaterer systemparametrene. */
        setSyspara().
                  
      END.              
    END. /* STATUS200 */ 
    ELSE DO:
      ASSIGN 
        bResult = FALSE 
        .
    END.

    DELETE OBJECT oJsonRequest.
    RETURN bResult.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose: Legger token på plass i SysPara tabellen.
   
   Notes:   Andre rutiner som trenger token henter dem derfra.    
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC LOGICAL setSyspara(  ):
    
    DEFINE VARIABLE bResult AS LOGICAL.

    DEFINE BUFFER bSysPara FOR SysPara.

    DO FOR bSysPara TRANSACTION:
      {setSysPara.i 14 303 10 cAccess_token "" "b" "" }
      {setSysPara.i 14 303 11 cToken_type "" "b" "" }
      {setSysPara.i 14 303 12 STRING(dtRead) "" "b" "" }
      {setSysPara.i 14 303 13 STRING(dtExpires) "" "b" "" }
      bResult = TRUE.
    END. /* TRANSACTION */

   IF bTest THEN 
     rStandardFunksjoner:SkrivTilLogg(cLogg,
       'setSyspara( ): ' + 'Lagret Token. Resultat: ' + CAPS(STRING(bResult)) + '.'
       ).    
    
    RETURN bResult.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose: Henter data for oppkobling mot Dintero for henting av nytt token.
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC LOGICAL getSysParaOppkobling(  ):
    
    DEFINE VARIABLE bResult AS LOGICAL NO-UNDO.

    {syspara.i 14 303 1 cUserDomain}
    {syspara.i 14 303 2 cUser}
    {syspara.i 14 303 3 cPwd}
    {syspara.i 14 303 4 cAid}
    {syspara.i 14 303 5 cFullPath}
    cFullPath = REPLACE(cFullPath,'&Aid',cAid).
    
    IF (cUserDomain = '' OR 
        cUser = '' OR 
        cPwd = '' OR 
        cAid = '' OR 
        cFullPath = ''
        ) THEN 
      DO: 
       IF bTest THEN
       DO: 
         rStandardFunksjoner:SkrivTilLogg(cLogg,
           'getSysParaOppkobling( ): ' + 'FEIL ved henting av oppkoblingsparametre.'
           ).
         rStandardFunksjoner:SkrivTilLogg(cLogg,
           '  cUserDomain: ' + cUserDomain 
           ).
         rStandardFunksjoner:SkrivTilLogg(cLogg,
           '  cUser: ' + cUser 
           ).
         rStandardFunksjoner:SkrivTilLogg(cLogg,
           '  cPwd: ' + cPwd 
           ).
         rStandardFunksjoner:SkrivTilLogg(cLogg,
           '  cAid: ' + cAid 
           ).
         rStandardFunksjoner:SkrivTilLogg(cLogg,
           '  cFullPath: ' + cUserDomain 
           ).
        END.    
        bResult = FALSE.
      END.
     ELSE DO:
       IF bTest THEN
       DO: 
         rStandardFunksjoner:SkrivTilLogg(cLogg,
           'getSysParaOppkobling( ): ' + 'Oppkoblingsparametre hentet.'
           ).
         rStandardFunksjoner:SkrivTilLogg(cLogg,
           '  cUserDomain: ' + cUserDomain 
           ).
         rStandardFunksjoner:SkrivTilLogg(cLogg,
           '  cUser: ' + cUser 
           ).
         rStandardFunksjoner:SkrivTilLogg(cLogg,
           '  cPwd: ' + cPwd 
           ).
         rStandardFunksjoner:SkrivTilLogg(cLogg,
           '  cAid: ' + cAid 
           ).
         rStandardFunksjoner:SkrivTilLogg(cLogg,
           '  cFullPath: ' + cUserDomain 
           ).
        END.    
        bResult = TRUE.     
      END.
/*      cUserDomain = 'tomcat'                                                  */
/*      cFullPath   = 'https://api.dintero.com/v1/accounts/T11112542/auth/token'*/
/*      cUser       = 'c0fab10f-ecc5-4c8d-a152-93fd0ff314ab'                    */
/*      cPwd        = 'b8ceaab2-9051-46cb-8407-76ca7eb96235'                    */

    RETURN bResult.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose: Henter data for oppkobling og bruk av API hos Dintero.
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC LOGICAL sjekkSysParaToken(  ):
    
    DEFINE VARIABLE bResult AS LOGICAL NO-UNDO.
    DEFINE VARIABLE piResultCode AS INTEGER NO-UNDO.
    DEFINE VARIABLE pcResultMessage AS CHARACTER NO-UNDO.
    {syspara.i 14 303 10 cAccess_Token}
    {syspara.i 14 303 11 cToken_Type}
    {syspara.i 14 303 12 dtRead DATETIME}
    {syspara.i 14 303 13 dtExpires DATETIME}
    
    IF (cAccess_Token = '' OR 
        cToken_Type   = '' OR 
        dtExpires     = ?        
       ) THEN 
      DO:
        IF bTest THEN 
          rStandardFunksjoner:SkrivTilLogg(cLogg,
            'sjekkSysParaToken( ): ' + 'FEIL i oppkoblingsparametre for API (Token).'
            ).    
         bResult = FALSE.
      END.
    ELSE DO: 
      /* Er token foreldet, skal det hente spå nytt. */ 
      IF (dtExpires <= NOW) THEN 
        DO:
          IF bTest THEN 
            rStandardFunksjoner:SkrivTilLogg(cLogg,
              'sjekkSysParaToken( ): ' + 'Token foreldet.'
              ).  
            
          bResult = FALSE.
        END.  
      ELSE DO: 
        IF bTest THEN 
          rStandardFunksjoner:SkrivTilLogg(cLogg,
            'sjekkSysParaToken( ): ' + 'OK i oppkoblingsparametre for API (Token).'
            ).    
        bResult = TRUE.
      END.
    END.

    RETURN bResult.

  END METHOD.

  DESTRUCTOR PUBLIC tokenDintero ( ):

  END DESTRUCTOR.

END CLASS.