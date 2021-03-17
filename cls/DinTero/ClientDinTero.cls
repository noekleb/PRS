 
 /*------------------------------------------------------------------------
    File        : ClientDinTero
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : tny
    Created     : Tue Nov 06 09:42:35 CET 2018
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING OpenEdge.Net.HTTP.*.
USING OpenEdge.Net.URI.
USING Progress.Json.ObjectModel.JsonObject.
USING Progress.Json.ObjectModel.JsonArray.
USING OpenEdge.Core.String.
USING OpenEdge.Core.WidgetHandle.
USING OpenEdge.Net.HTTP.Lib.ClientLibraryBuilder.

CLASS cls.DinTero.ClientDinTero: 
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    DEFINE VARIABLE oClient AS IHttpClient NO-UNDO.
    DEFINE VARIABLE oURI AS URI NO-UNDO.
    DEFINE VARIABLE o2URI AS URI NO-UNDO.
    DEFINE VARIABLE oCredentials AS Credentials NO-UNDO.
    DEFINE VARIABLE oRequest AS IHttpRequest NO-UNDO.
    DEFINE VARIABLE oResponse AS IHttpResponse NO-UNDO.
    DEFINE VARIABLE lcJsonString AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE oRequestBody AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcExpires_In AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcToken_StatusReason AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE iLoop AS INTEGER NO-UNDO.
    DEFINE VARIABLE cRecord AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cLabel AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cValue AS CHARACTER NO-UNDO.
/*    DEFINE VARIABLE oLib AS IHttpClientLibrary NO-UNDO.*/
    /* TN 17/8-20 */
    DEFINE VARIABLE oJson AS JsonObject NO-UNDO.
    DEFINE VARIABLE oEntity AS OBJECT NO-UNDO.
    DEFINE VARIABLE httpUrl AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cJSon AS CHARACTER FORMAT "x(500)" NO-UNDO.
    DEFINE VARIABLE hXmlDoc AS HANDLE NO-UNDO.
    DEFINE VARIABLE lcHTML AS LONGCHAR NO-UNDO.

    {cls/DinTero/clientDinTero.i}
    {cls\DinTero\ttOrder.i}
    {cls\DinTero\dsOrder.i}

    DEFINE STREAM Inn.
    
    DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.clStandardFunksjoner NO-UNDO. 

    DEFINE PROPERTY cJsonString AS LONGCHAR NO-UNDO 
        GET. 
        SET.
    DEFINE PROPERTY cToken_Entity AS LONGCHAR NO-UNDO 
        GET. 
        SET.
    DEFINE PROPERTY dtToken_Entity AS DATETIME NO-UNDO 
        GET. 
        SET.
    DEFINE PROPERTY cAccess_Token AS CHARACTER NO-UNDO 
        GET. 
        SET.
    DEFINE PROPERTY cExpires_In AS CHARACTER NO-UNDO 
        GET. 
        SET.
    DEFINE PROPERTY cToken_Type AS CHARACTER NO-UNDO 
        GET. 
        SET.
    DEFINE PROPERTY cToken_StatusReason AS CHARACTER NO-UNDO 
        GET. 
        SET.
    DEFINE PROPERTY cLogg AS CHARACTER NO-UNDO 
        GET. 
        SET(INPUT pcLogg AS CHARACTER ):
            ASSIGN 
                cLogg = pcLogg
                .
        END SET.
    DEFINE PROPERTY cDomainName AS CHARACTER NO-UNDO 
        GET.
        SET.
    DEFINE PROPERTY cPath AS CHARACTER NO-UNDO
        GET.
        SET.
    DEFINE PROPERTY cFullPath AS CHARACTER NO-UNDO
        GET.
        SET.
    DEFINE PROPERTY cProtocoll AS CHARACTER NO-UNDO
        GET.
        SET.
    DEFINE PROPERTY cUserDomain AS CHARACTER NO-UNDO 
        GET.
        SET.
    DEFINE PROPERTY cUser AS CHARACTER NO-UNDO
        GET.
        SET.
    DEFINE PROPERTY cPwd AS CHARACTER NO-UNDO
        GET.
        SET.

    CONSTRUCTOR PUBLIC ClientDinTero (  ):
        SUPER ().
        
      rStandardFunksjoner  = NEW cls.StdFunk.clStandardFunksjoner() NO-ERROR.
        
      ASSIGN 
        cLogg       = 'ClientGetToken' + REPLACE(STRING(TODAY),'/','')
        cProtocoll  = 'https'    
        cUserDomain = 'tomcat'
        .
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
      {
        "access_token": "eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCIsImtpZCI6ImFwaS5kaW50ZXJvLmNvbS8wMWUyY2I2ZWZiMTVlNTAyNmQ1OTU3Njc3MjkwZDVkMDY0ZDc1N2ZmIn0.eyJpc3MiOiJodHRwczovL2FwaS5kaW50ZXJvLmNvbSIsImF1ZCI6Imh0dHBzOi8vVDExMTEyNTQyQGFwaS5kaW50ZXJvLmNvbS92MS9hY2NvdW50cy9UMTExMTI1NDIiLCJzdWIiOiJjMGZhYjEwZi1lY2M1LTRjOGQtYTE1Mi05M2ZkMGZmMzE0YWIiLCJzY29wZXMiOiJ3cml0ZTpjaGVja291dCByZWFkOmNoZWNrb3V0IHdyaXRlOnJlY2VpcHRzIHdyaXRlOm5vdGlmaWNhdGlvbnMgd3JpdGU6ZGlzY291bnRzOi9hdmFpbGFibGVfZm9yX3JlY2VpcHQiLCJ2ZXJzaW9uIjoyLCJpYXQiOjE2MDE4MDEwNjcsImV4cCI6MTYwMTgxNTQ2N30.OUc9JvFi8vuCGPhYmj4MkN9jhIfqKzdHA-buSLIaFmix2M25E08kRLUz4hr3o_wjPjIrk9yOrMSEpQmaQzZ42-v-F3vpMVLl22g7OKACn6cVuHKGXOZQoFf_hfGbDa_o5HG2r6CfjQhU1hfCWqqrY7jQ0IHV5LHoNSHVAG-CgS0NckCfGAXQu7unC_2XohrXF4OA3ApJsT1rxYXZcblkF7wKxxaxaQtnxfyR6kFsK_NyltlCRTjEn9fbk6CGk_esPXfbFzFXEVxtRFEDc3rxOOJTOfCuPzWReXzA9NQnCLEsQXpNExwRvazC-86rP7EPTlP61GgfCY0jmmbwoNfZJA",
        "expires_in": 14400,
        "token_type": "Bearer"
      }

    ------------------------------------------------------------------------------*/

    METHOD PUBLIC VOID getToken( OUTPUT piStatusCode AS INTEGER,
                                 OUTPUT pcStatusReason AS CHARACTER ):
        /* Gjør oppsett og bygger JSon meldingen som forespørselen ligger i.                                          */
        /* Skal bare bygges ved første oppkall. Kalles den flere ganger, benyttes samme instans av objectet om igjen. */
        IF NOT CAN-FIND(FIRST ttOJSon) THEN 
        DO:
          /* Creating a Client. */
          oClient = ClientBuilder:Build()
                    :KeepCookies(CookieJarBuilder:Build()
                    :CookieJar):Client
                    .

          /* Creating a request. */
          oCredentials = NEW Credentials(cUserDomain, cUser, cPwd).
          
          CREATE ttoJSon. 
          ASSIGN 
              ttoJson.grant_type = "client_credentials"
              ttoJSon.audience   = "https://api.dintero.com/v1/accounts/T11112542".
          oJson = NEW JsonObject().
          FOR EACH ttoJson: 
              oJSon:add("grant_type",ttoJSon.grant_type). 
              oJSon:add("audience",ttoJSon.audience). 
          END.
        END.
        oRequest = RequestBuilder:Post(cFullPath) /* URL og JSon object */
                        :UsingBasicAuthentication(oCredentials) /* Sender inn domain, brukerid og passord. */
                        :AcceptJson() /* we want to get JSON back */
                        :AddJSonData(oJson) /* Her legges JSon meldingen inn */
                        :Request.

        /* Bygger respons objekstet. Det er tomt her, men fylles når forespørselen eksekverees. */
        oResponse = ResponseBuilder:Build():Response.

        /* Execute the request.                                                              */
        /* Sender forespørselen til API'et med preppet request object og tom respons object. */
        oClient:Execute(oRequest, oResponse).     

        /* Processing the responce                     */
        /* Henter ut status koden fra responsobjectet. */
        ASSIGN 
            piStatusCode   = oResponse:StatusCode
            pcStatusReason = oResponse:StatusReason
            cAccess_Token  = ''  
            cExpires_In    = ''  
            cToken_Type    = ''  
            .
        EMPTY TEMP-TABLE ttToken.

        IF piStatusCode = 200 THEN 
        DO:

            /* Leser og tolker svaret som er en JSon melding og legger det i en longchar variabel. */
            /* Deretter må de enkelte nodene leses ut en for en av JSon meldingen.                 */
            CAST(oResponse:Entity, JsonObject):Write(lcJsonString, TRUE).

            /* Legger ut respons JSon meldingen på en fil. */
            CAST(oResponse:Entity, JSONObject):WriteFile('konv\entityGetToken' + STRING(ETIME) + '.json').
            
            /* Legger JSon meldingen inn i Token tabellen. */
            TEMP-TABLE ttToken:READ-JSON ("longchar", lcJsonString,"EMPTY").
            FIND FIRST ttToken NO-ERROR.
            IF AVAILABLE ttToken THEN 
            DO:
              ASSIGN
                ttToken.dtRead      = NOW
                dtToken_Entity      = ttToken.dtRead
                ttToken.dtExpires   = NOW + (ttToken.expires_in * 1000)
                cToken_Entity       = STRING(lcJsonString)
                cToken_StatusReason = pcStatusReason 
                cAccess_Token       = ttToken.access_token
                cExpires_In         = STRING(ttToken.expires_in)
                cToken_Type         = ttToken.token_type
                .

              TEMP-TABLE ttToken:WRITE-JSON('file', 'konv\ttToken' + STRING(ETIME) + '.json', TRUE).
            END.              
        END. 
              
        RETURN.
        
    END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC VOID rensJSon( INPUT-OUTPUT lcPostData AS LONGCHAR ):
    
    lcPostData = TRIM(lcPostData). 
    lcPostData = LEFT-TRIM(lcPostData, '~{'). 
    lcPostData = RIGHT-TRIM(lcPostdata, '}').       
    lcPostData = LEFT-TRIM(lcPostData, CHR(13)). 
    lcPostData = RIGHT-TRIM(lcPostdata, CHR(13)). 
    lcPostData = LEFT-TRIM(lcPostData, CHR(10)). 
    lcPostData = RIGHT-TRIM(lcPostdata, CHR(10)). 
    
    RETURN.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC VOID listSession( INPUT  pcParameters AS CHARACTER,
                                  OUTPUT piStatusCode AS INTEGER,
                                  OUTPUT pcStatusReason AS CHARACTER,
                                  OUTPUT pcJSonString AS LONGCHAR,
                                  OUTPUT pcJSonFileName AS CHARACTER,
                                  OUTPUT pcType AS CHARACTER ):

    DEFINE VARIABLE pcFullPath AS CHARACTER NO-UNDO.

    /* Creating a Client. */
    oClient = ClientBuilder:Build()
              :KeepCookies(CookieJarBuilder:Build()
              :CookieJar):Client
              .
    
    /* Legger inn parametrene i URL'en. */
    /* Parameter form '<Para1>=<Value1>?<Para1>=<Value1>... */
    ASSIGN 
      pcFullPath = cFullPath + 
                  (IF pcParameters <> ? THEN 
                     ('?' + pcParameters) 
                   ELSE 
                     '').
                     
    oRequest = RequestBuilder:Get(pcFullPath) /* URL */
                    :AddHeader('Authorization', cToken_Type + ' ' + cAccess_Token) /* cToken_Type = 'Bearer' */
                    :AcceptJson() /* we want to get JSON back */
                    :Request.
    /* Bygger respons objekstet. Det er tomt her, men fylles når forespørselen eksekverees. */
    oResponse = ResponseBuilder:Build():Response.

    /* Execute the request.                                                              */
    /* Sender forespørselen til API'et med preppet request object og tom respons object. */
    oClient:Execute(oRequest, oResponse).   

    oEntity = oResponse:Entity.  

    ASSIGN 
      piStatusCode   = oResponse:StatusCode
      pcStatusReason = oResponse:StatusReason
      .

/*    IF piStatusCode = 200 THEN*/
    DO:      
      castObject(INPUT 'ListSessionsResponse', OUTPUT pcJSonString, OUTPUT pcJSonFileName, OUTPUT pcType).      
    END.       
      
    RETURN.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes: 
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC VOID createSession( INPUT  pcParameters AS CHARACTER,
                                    INPUT poJSon AS CLASS JsonObject,
                                   OUTPUT piStatusCode AS INTEGER,
                                   OUTPUT pcStatusReason AS CHARACTER,
                                   OUTPUT pcJSonString AS LONGCHAR,
                                   OUTPUT pcJSonFileName AS CHARACTER,
                                   OUTPUT pcType AS CHARACTER ):

    DEFINE VARIABLE pcFullPath AS CHARACTER NO-UNDO.

    /* Legger inn parametrene i URL'en. */
    /* Parameter form '<Para1>=<Value1>?<Para1>=<Value1>... */
    ASSIGN 
      pcFullPath = cFullPath + 
                  (IF pcParameters <> ? THEN 
                     ('?' + pcParameters) 
                   ELSE 
                     '').

    /* Creating a Client. */
    oClient = ClientBuilder:Build()
              :KeepCookies(CookieJarBuilder:Build()
              :CookieJar):Client
              .

    oRequest = RequestBuilder:Post(pcFullPath) /* URL */
                    :AddHeader('Authorization', cToken_Type + ' ' + cAccess_Token) /* cToken_Type = 'Bearer' */
                    :AcceptJson() /* we want to get JSON back */
                    :AddJSonData(poJson) /* Her legges JSon meldingen inn */
                    :Request.

    /* Bygger respons objekstet. Det er tomt her, men fylles når forespørselen eksekverees. */
    oResponse = ResponseBuilder:Build():Response.
    
    /* Execute the request.                                                              */
    /* Sender forespørselen til API'et med preppet request object og tom respons object. */
    oClient:Execute(oRequest, oResponse).   
    
    oEntity = oResponse:Entity.  
    
    ASSIGN 
      piStatusCode   = oResponse:StatusCode
      pcStatusReason = oResponse:StatusReason
      .

/*    IF piStatusCode = 200 THEN*/
    DO:      
      CAST(poJson, JSONObject):WriteFile('konv\CreateSession' + STRING(ETIME) + '.json').
      /* Her er det oEntity objectet som behandles. */
      castObject(INPUT 'CreateSessionResponse', OUTPUT pcJSonString, OUTPUT pcJSonFileName, OUTPUT pcType).
    END.       
    
    RETURN.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC VOID listProfiles( INPUT  pcParameters AS CHARACTER,
                                   OUTPUT piStatusCode AS INTEGER,
                                   OUTPUT pcStatusReason AS CHARACTER,
                                   OUTPUT pcJSonString AS LONGCHAR,
                                   OUTPUT pcJSonFileName AS CHARACTER,
                                   OUTPUT pcType AS CHARACTER ):
    DEFINE VARIABLE pcFullPath AS CHARACTER NO-UNDO.
    
    /* Creating a Client. */
    oClient = ClientBuilder:Build()
              :KeepCookies(CookieJarBuilder:Build()
              :CookieJar):Client
              .
    
    /* Legger inn parametrene i URL'en. */
    /* Parameter form '<Para1>=<Value1>?<Para1>=<Value1>... */
    ASSIGN 
      pcFullPath = cFullPath + 
                  (IF pcParameters <> ? THEN 
                     ('?' + pcParameters) 
                   ELSE 
                     '').

    /* RequestBuilder kan ikke legge inn 'RequestBuilder:AddParameters'. Men parametre kan legges direkte i URL'en. Slik som vist under. */
    /* rClientDinTero:cFullPath = 'https://api.dintero.com/checkouts/v1/admin/session/profiles?limit=1&starting_after=T11112542.4XopX7PCwvdZzWxu5anRmd'*/
                                    
    oRequest = RequestBuilder:Get(pcFullPath) /* URL */
                    :AddHeader('Authorization', cToken_Type + ' ' + cAccess_Token) /* cToken_Type = 'Bearer' */
                    :AcceptJson() /* we want to get JSON back */
                    :Request.

    /* Bygger respons objekstet. Det er tomt her, men fylles når forespørselen eksekverees. */
    oResponse = ResponseBuilder:Build():Response.
    
    /* Execute the request.                                                              */
    /* Sender forespørselen til API'et med preppet request object og tom respons object. */
    oClient:Execute(oRequest, oResponse).   
    
    oEntity = oResponse:Entity.  
    
    ASSIGN 
      piStatusCode   = oResponse:StatusCode
      pcStatusReason = oResponse:StatusReason
      .

/*    IF piStatusCode = 200 THEN*/
    DO:      
      castObject(INPUT 'ListProfilesResponse', OUTPUT pcJSonString, OUTPUT pcJSonFileName, OUTPUT pcType).
    END.       
    
    RETURN.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC VOID castObject( INPUT  pcFilName AS CHARACTER, 
                                 OUTPUT pcJSonString AS LONGCHAR,
                                 OUTPUT pcJSonFileName AS CHARACTER,
                                 OUTPUT pcType AS CHARACTER ):
    
    pcJSonFileName = REPLACE('konv/&FileName','&FileName', pcFilName) + STRING(ETIME) + '.&Ekst'.

    IF TYPE-OF(oEntity, JsonObject) THEN
    DO:
      pcType = 'JSonObject'.
      pcJSonFileName = REPLACE(pcJSonFileName,'&Ekst','json').
      CAST(oEntity, JSONObject):Write(pcJsonString).
      CAST(oEntity, JSONObject):WriteFile(pcJSonFileName).
    END.
    ELSE IF TYPE-OF(oEntity, JsonArray) THEN
    DO:
      pcType = 'JSonArray'.
      pcJSonFileName = REPLACE(pcJSonFileName,'&Ekst','json').
      CAST(oEntity, JSONArray):Write(pcJsonString).
      CAST(oEntity, JSONArray):WriteFile(pcJSonFileName).
    END.
    ELSE IF TYPE-OF(oEntity, WidgetHandle) THEN
    DO:
      pcType = 'WidgetHandle'.
      pcJSonFileName = REPLACE(pcJSonFileName,'&Ekst','xml').
      hXmlDoc = CAST(oEntity, WidgetHandle):Value.
      hXmlDoc:SAVE('file', pcJSonFileName).
    END.
    ELSE 
    DO:
      pcType = 'String'.
      IF TYPE-OF(oEntity, String) THEN
         lcHTML = CAST(oEntity, String):Value.
      ELSE
         lcHTML = oEntity:ToString().

      /* Change extension per the Response's ContentType */
      CASE oResponse:ContentType:        
        WHEN 'application/json' THEN
            pcJSonFileName = REPLACE(pcJSonFileName,'&Ekst','json').
        WHEN 'text/html' THEN
            pcJSonFileName = REPLACE(pcJSonFileName,'&Ekst','html').
        OTHERWISE
            pcJSonFileName = REPLACE(pcJSonFileName,'&Ekst','txt').
      END CASE.
      COPY-LOB lcHTML TO FILE pcJSonFileName.
    END.
    
    RETURN.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC VOID listTransactions( INPUT  pcParameters AS CHARACTER,
                                  OUTPUT piStatusCode AS INTEGER,
                                  OUTPUT pcStatusReason AS CHARACTER,
                                  OUTPUT pcJSonString AS LONGCHAR,
                                  OUTPUT pcJSonFileName AS CHARACTER,
                                  OUTPUT pcType AS CHARACTER ):
    
    DEFINE VARIABLE pcFullPath AS CHARACTER NO-UNDO.

    /* Creating a Client. */
    oClient = ClientBuilder:Build()
              :KeepCookies(CookieJarBuilder:Build()
              :CookieJar):Client
              .
    
    /* Legger inn parametrene i URL'en. */
    /* Parameter form '<Para1>=<Value1>?<Para1>=<Value1>... */
    ASSIGN 
      pcFullPath = cFullPath + 
                  (IF pcParameters <> ? THEN 
                     ('?' + pcParameters) 
                   ELSE 
                     '').
                     
    oRequest = RequestBuilder:Get(pcFullPath) /* URL */
                    :AddHeader('Authorization', cToken_Type + ' ' + cAccess_Token) /* cToken_Type = 'Bearer' */
                    :AcceptJson() /* we want to get JSON back */
                    :Request.
    /* Bygger respons objekstet. Det er tomt her, men fylles når forespørselen eksekverees. */
    oResponse = ResponseBuilder:Build():Response.

    /* Execute the request.                                                              */
    /* Sender forespørselen til API'et med preppet request object og tom respons object. */
    oClient:Execute(oRequest, oResponse).   

    oEntity = oResponse:Entity.  

    ASSIGN 
      piStatusCode   = oResponse:StatusCode
      pcStatusReason = oResponse:StatusReason
      .

/*    IF piStatusCode = 200 THEN*/
    DO:      
      castObject(INPUT 'ListTransactionsResponse', OUTPUT pcJSonString, OUTPUT pcJSonFileName, OUTPUT pcType).      
    END.       
      
    RETURN.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC VOID getTransaction( INPUT  pcParameters AS CHARACTER,
                                  OUTPUT piStatusCode AS INTEGER,
                                  OUTPUT pcStatusReason AS CHARACTER,
                                  OUTPUT pcJSonString AS LONGCHAR,
                                  OUTPUT pcJSonFileName AS CHARACTER,
                                  OUTPUT pcType AS CHARACTER ):
    
    DEFINE VARIABLE pcFullPath AS CHARACTER NO-UNDO.

    /* Creating a Client. */
    oClient = ClientBuilder:Build()
              :KeepCookies(CookieJarBuilder:Build()
              :CookieJar):Client
              .
    
    /* Legger inn parametrene i URL'en. */
    /* Parameter form '<Para1>=<Value1>?<Para1>=<Value1>... */
    ASSIGN
      pcFullPath = cFullPath +
                  (IF pcParameters <> ? THEN
                     ('?' + pcParameters)
                   ELSE
                     '').
    MESSAGE 'getTransaction pcFullPath:' pcFullPath
    VIEW-AS ALERT-BOX.                     
    oRequest = RequestBuilder:Get(pcFullPath) /* URL */
                    :AddHeader('Authorization', cToken_Type + ' ' + cAccess_Token) /* cToken_Type = 'Bearer' */
                    :AcceptJson() /* we want to get JSON back */
                    :Request.
    /* Bygger respons objekstet. Det er tomt her, men fylles når forespørselen eksekverees. */
    oResponse = ResponseBuilder:Build():Response.

    /* Execute the request.                                                              */
    /* Sender forespørselen til API'et med preppet request object og tom respons object. */
    oClient:Execute(oRequest, oResponse).   

    oEntity = oResponse:Entity.  

    ASSIGN 
      piStatusCode   = oResponse:StatusCode
      pcStatusReason = oResponse:StatusReason
      .

/*    IF piStatusCode = 200 THEN*/
    DO:      
      castObject(INPUT 'GetTransactionResponse', OUTPUT pcJSonString, OUTPUT pcJSonFileName, OUTPUT pcType).      
    END.       
      
    RETURN.


  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC VOID voidTransaction( INPUT  pcParameters AS CHARACTER,
                                   OUTPUT piStatusCode AS INTEGER,
                                   OUTPUT pcStatusReason AS CHARACTER,
                                   OUTPUT pcJSonString AS LONGCHAR,
                                   OUTPUT pcJSonFileName AS CHARACTER,
                                   OUTPUT pcType AS CHARACTER ):

    DEFINE VARIABLE pcFullPath AS CHARACTER NO-UNDO.

    /* Legger inn parametrene i URL'en. */
    /* Parameter form '<Para1>=<Value1>?<Para1>=<Value1>... */
    ASSIGN 
      pcFullPath = cFullPath + 
                  (IF pcParameters <> ? THEN 
                     ('?' + pcParameters) 
                   ELSE 
                     '').

    /* Creating a Client. */
    oClient = ClientBuilder:Build()
              :KeepCookies(CookieJarBuilder:Build()
              :CookieJar):Client
              .

    oRequest = RequestBuilder:Post(pcFullPath) /* URL */
                    :AddHeader('Authorization', cToken_Type + ' ' + cAccess_Token) /* cToken_Type = 'Bearer' */
                    :AcceptJson() /* we want to get JSON back */
                    :Request.

    /* Bygger respons objekstet. Det er tomt her, men fylles når forespørselen eksekverees. */
    oResponse = ResponseBuilder:Build():Response.
    
    /* Execute the request.                                                              */
    /* Sender forespørselen til API'et med preppet request object og tom respons object. */
    oClient:Execute(oRequest, oResponse).   
    
    oEntity = oResponse:Entity.  
    
    ASSIGN 
      piStatusCode   = oResponse:StatusCode
      pcStatusReason = oResponse:StatusReason
      .

/*    IF piStatusCode = 200 THEN*/
    DO:      
      castObject(INPUT 'VoidTransactionResponse', OUTPUT pcJSonString, OUTPUT pcJSonFileName, OUTPUT pcType).
    END.       
    
    RETURN.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC VOID refundTransaction( INPUT  pcParameters AS CHARACTER,
                                    INPUT poJSon AS CLASS JsonObject,
                                   OUTPUT piStatusCode AS INTEGER,
                                   OUTPUT pcStatusReason AS CHARACTER,
                                   OUTPUT pcJSonString AS LONGCHAR,
                                   OUTPUT pcJSonFileName AS CHARACTER,
                                   OUTPUT pcType AS CHARACTER ):
    
    DEFINE VARIABLE pcFullPath AS CHARACTER NO-UNDO.

    /* Legger inn parametrene i URL'en. */
    /* Parameter form '<Para1>=<Value1>?<Para1>=<Value1>... */
    ASSIGN 
      pcFullPath = cFullPath + 
                  (IF pcParameters <> ? THEN 
                     ('?' + pcParameters) 
                   ELSE 
                     '').

    /* Creating a Client. */
    oClient = ClientBuilder:Build()
              :KeepCookies(CookieJarBuilder:Build()
              :CookieJar):Client
              .

    oRequest = RequestBuilder:Post(pcFullPath) /* URL */
                    :AddHeader('Authorization', cToken_Type + ' ' + cAccess_Token) /* cToken_Type = 'Bearer' */
                    :AcceptJson() /* we want to get JSON back */
                    :AddJSonData(poJson) /* Her legges JSon meldingen inn */
                    :Request.

    /* Bygger respons objekstet. Det er tomt her, men fylles når forespørselen eksekverees. */
    oResponse = ResponseBuilder:Build():Response.
    
    /* Execute the request.                                                              */
    /* Sender forespørselen til API'et med preppet request object og tom respons object. */
    oClient:Execute(oRequest, oResponse).   
    
    oEntity = oResponse:Entity.  
    
    ASSIGN 
      piStatusCode   = oResponse:StatusCode
      pcStatusReason = oResponse:StatusReason
      .

/*    IF piStatusCode = 200 THEN*/
    DO:      
      CAST(poJson, JSONObject):WriteFile('konv\RefundTransaction' + STRING(ETIME) + '.json').
      castObject(INPUT 'RefundTransactionResponse', OUTPUT pcJSonString, OUTPUT pcJSonFileName, OUTPUT pcType) NO-ERROR.
    END.       

    RETURN.
    
  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC VOID getGetSessionDetails( INPUT  pcParameters AS CHARACTER,
                                  OUTPUT piStatusCode AS INTEGER,
                                  OUTPUT pcStatusReason AS CHARACTER,
                                  OUTPUT pcJSonString AS LONGCHAR,
                                  OUTPUT pcJSonFileName AS CHARACTER,
                                  OUTPUT pcType AS CHARACTER ):
    
    DEFINE VARIABLE pcFullPath AS CHARACTER NO-UNDO.

    /* Creating a Client. */
    oClient = ClientBuilder:Build()
              :KeepCookies(CookieJarBuilder:Build()
              :CookieJar):Client
              .
    
    /* Legger inn parametrene i URL'en. */
    /* Parameter form '<Para1>=<Value1>?<Para1>=<Value1>... */
    ASSIGN
      pcFullPath = cFullPath +
                  (IF pcParameters <> ? THEN
                     ('?' + pcParameters)
                   ELSE
                     '').
    oRequest = RequestBuilder:Get(pcFullPath) /* URL */
                    :AddHeader('Authorization', cToken_Type + ' ' + cAccess_Token) /* cToken_Type = 'Bearer' */
                    :AcceptJson() /* we want to get JSON back */
                    :Request.
    /* Bygger respons objekstet. Det er tomt her, men fylles når forespørselen eksekverees. */
    oResponse = ResponseBuilder:Build():Response.

    /* Execute the request.                                                              */
    /* Sender forespørselen til API'et med preppet request object og tom respons object. */
    oClient:Execute(oRequest, oResponse).   

    oEntity = oResponse:Entity.  

    ASSIGN 
      piStatusCode   = oResponse:StatusCode
      pcStatusReason = oResponse:StatusReason
      .

/*    IF piStatusCode = 200 THEN*/
    DO:
      MESSAGE '  pcFullPath: ' + pcFullPath.      
      castObject(INPUT 'getGetSessionDetailsResponse', OUTPUT pcJSonString, OUTPUT pcJSonFileName, OUTPUT pcType).      
    END.       

    RETURN.

  END METHOD.

  DESTRUCTOR PUBLIC ClientDinTero ( ):

    EMPTY TEMP-TABLE ttoJSon. 
    EMPTY TEMP-TABLE ttOrder.
    EMPTY TEMP-TABLE ttRefund.
    EMPTY TEMP-TABLE ttUrl.
    EMPTY TEMP-TABLE ttItems.
    EMPTY TEMP-TABLE ttGroups.

  END DESTRUCTOR.

END CLASS.

