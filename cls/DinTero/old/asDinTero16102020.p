
/*------------------------------------------------------------------------
    File        : asDinTero.p
    Purpose     : 

    Syntax      :

    Description : Tillater klienten å kalle serverDinTero klassen for igjennom den ha tilgang til DinTero API'et.

    Author(s)   : Tom Nøkleby
    Created     : Sun Oct 11 09:09:03 CEST 2020
    Notes       :
  ----------------------------------------------------------------------*/

USING Progress.Json.ObjectModel.JsonObject.
USING Progress.Json.ObjectModel.JsonArray.

/* ***************************  Definitions  ************************** */
DEFINE INPUT  PARAMETER icParam     AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ihBuffer    AS HANDLE NO-UNDO.
DEFINE INPUT  PARAMETER icSessionId AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER ocReturn    AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER obOK        AS LOG NO-UNDO.

DEFINE VARIABLE cJSonString   AS LONGCHAR  NO-UNDO.
DEFINE VARIABLE cJSonFileName AS CHARACTER NO-UNDO.
DEFINE VARIABLE iStatusCode   AS INTEGER   NO-UNDO.
DEFINE VARIABLE cStatusReason AS CHARACTER NO-UNDO.
DEFINE VARIABLE cToken        AS CHARACTER NO-UNDO.
DEFINE VARIABLE iType         AS INTEGER   NO-UNDO.
DEFINE VARIABLE cErorTekst    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cType         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cParametre    AS CHARACTER NO-UNDO.

{cls/DinTero/clientDinTero.i}

DEFINE VARIABLE rClientDinTero AS cls.DinTero.ClientDinTero NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEFINE VARIABLE cLogg          AS CHARACTER                 NO-UNDO.
DEFINE VARIABLE cEndPointCall  AS CHARACTER                 NO-UNDO.

rClientDinTero = NEW cls.DinTero.ClientDinTero().

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.

ASSIGN 
  obOk          = TRUE
  cEndPointCall = ENTRY(1,icParam,'|')
  cLogg         = 'asDinTero' + REPLACE(STRING(TODAY),'/','') 
  .

rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.
rStandardFunksjoner:SkrivTilLogg(cLogg,
  'Start' 
  ).    
rStandardFunksjoner:SkrivTilLogg(cLogg,
  '  Parametre: ' 
  ).    
rStandardFunksjoner:SkrivTilLogg(cLogg,
  '    icParam: ' + icParam 
  ).    
rStandardFunksjoner:SkrivTilLogg(cLogg,
  '    cEndPointCall: ' + cEndPointCall 
  ).    

CASE cEndPointCall:
  WHEN 'getToken' THEN RUN getToken.
  WHEN 'listSessions' THEN RUN listSessions.
  WHEN 'listProfiles' THEN RUN listProfiles.
  WHEN 'createSession' THEN RUN createSession.
  WHEN 'listTransactions' THEN RUN listTransactions.
  WHEN 'getTransaction' THEN RUN getTransaction.
  WHEN 'voidTransaction' THEN RUN voidTransaction.
  WHEN 'refundTransaction' THEN RUN refundTransaction.
  OTHERWISE 
  DO:
    obOk = FALSE.
    ocReturn = 'Ukjent metodekall (' + cEndPointCall + ').'.
  END.    
END CASE.

rStandardFunksjoner:SkrivTilLogg(cLogg,
  'Slutt' 
  ).    



/* **********************  Internal Procedures  *********************** */

PROCEDURE getToken:
  /*------------------------------------------------------------------------------
   Purpose:
   Notes: Response   
     {
      "access_token": "eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCIsImtpZCI6ImFwaS5kaW50ZXJvLmNvbS8wMWUyY2I2ZWZiMTVlNTAyNmQ1OTU3Njc3MjkwZDVkMDY0ZDc1N2ZmIn0.eyJpc3MiOiJodHRwczovL2FwaS5kaW50ZXJvLmNvbSIsImF1ZCI6Imh0dHBzOi8vVDExMTEyNTQyQGFwaS5kaW50ZXJvLmNvbS92MS9hY2NvdW50cy9UMTExMTI1NDIiLCJzdWIiOiJjMGZhYjEwZi1lY2M1LTRjOGQtYTE1Mi05M2ZkMGZmMzE0YWIiLCJzY29wZXMiOiJ3cml0ZTpjaGVja291dCByZWFkOmNoZWNrb3V0IHdyaXRlOnJlY2VpcHRzIHdyaXRlOm5vdGlmaWNhdGlvbnMgd3JpdGU6ZGlzY291bnRzOi9hdmFpbGFibGVfZm9yX3JlY2VpcHQiLCJ2ZXJzaW9uIjoyLCJpYXQiOjE2MDE4MDEwNjcsImV4cCI6MTYwMTgxNTQ2N30.OUc9JvFi8vuCGPhYmj4MkN9jhIfqKzdHA-buSLIaFmix2M25E08kRLUz4hr3o_wjPjIrk9yOrMSEpQmaQzZ42-v-F3vpMVLl22g7OKACn6cVuHKGXOZQoFf_hfGbDa_o5HG2r6CfjQhU1hfCWqqrY7jQ0IHV5LHoNSHVAG-CgS0NckCfGAXQu7unC_2XohrXF4OA3ApJsT1rxYXZcblkF7wKxxaxaQtnxfyR6kFsK_NyltlCRTjEn9fbk6CGk_esPXfbFzFXEVxtRFEDc3rxOOJTOfCuPzWReXzA9NQnCLEsQXpNExwRvazC-86rP7EPTlP61GgfCY0jmmbwoNfZJA",
      "expires_in": 14400,
      "token_type": "Bearer"
     }
     
  ------------------------------------------------------------------------------*/

  /* Parametre for kall. */
  ASSIGN 
    rClientDinTero:cFullPath = 'https://api.dintero.com/v1/accounts/T11112542/auth/token'
    rClientDinTero:cUser     = 'c0fab10f-ecc5-4c8d-a152-93fd0ff314ab'
    rClientDinTero:cPwd      = 'b8ceaab2-9051-46cb-8407-76ca7eb96235'
    ocReturn                 = '||||||' /* 7 entries. */
    .
    
  rClientDinTero:getToken( OUTPUT iStatusCode,
    OUTPUT cStatusReason ) NO-ERROR.

  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    Request: ' + cEndPointCall + ' StatusCode: ' + STRING(iStatusCode) + ' StatusReason: ' + cStatusReason + '.' 
    ).    
                            
  IF iStatusCode = 200 THEN
  DO: 
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '    Entity: ' + STRING(rClientDinTero:cToken_Entity) 
      ).    
    ASSIGN 
      ENTRY(1,ocReturn,'|') = STRING(iStatusCode)
      ENTRY(2,ocReturn,'|') = cStatusReason
      ENTRY(3,ocReturn,'|') = rClientDinTero:cAccess_Token
      ENTRY(4,ocReturn,'|') = STRING(INT(rClientDinTero:cExpires_in),"HH:MM:SS")
      ENTRY(5,ocReturn,'|') = rClientDinTero:ctoken_type
      ENTRY(6,ocReturn,'|') = STRING(rClientDinTero:dtToken_Entity)
      ENTRY(7,ocReturn,'|') = STRING(rClientDinTero:cToken_Entity)
      cToken                = STRING(rClientDinTero:cAccess_Token)
      obOk                  = TRUE
      .
  END.
  ELSE 
  DO:  
    ASSIGN 
      ENTRY(1,ocReturn,'|') = STRING(iStatusCode)
      ENTRY(2,ocReturn,'|') = rClientDinTero:cToken_StatusReason
      cToken                = ''
      obOk                  = FALSE
      .
  END.    
  
  RETURN.
  
END PROCEDURE.

PROCEDURE listSessions:
  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    cAccess_Token: ' + ENTRY(2,icParam,'|') 
    ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    cToken_Type: ' + ENTRY(3,icParam,'|') 
    ).    

  /* Parametre for kall. */
  ASSIGN
    /*    cParametre = 'limit=1&starting_after=T11112542.4XCQ9MC8L2pKYfpD1bUTMv'*/
    rClientDinTero:cFullPath     = 'https://api.dintero.com/checkouts/v1/sessions'
    rClientDinTero:cAccess_Token = ENTRY(2,icParam,'|')  
    rClientDinTero:cToken_Type   = ENTRY(3,icParam,'|')
    cParametre                   = ENTRY(4,icParam,'|')
    ocReturn                     = '||||'
    .

  rClientDinTero:listSession( INPUT  cParametre ,
    OUTPUT iStatusCode,
    OUTPUT cStatusReason,
    OUTPUT cJSonString,
    OUTPUT cJSonFileName,
    OUTPUT cType ) NO-ERROR.
  IF iStatusCode = 200 THEN
  DO:
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '    Entity: ' + STRING(rClientDinTero:cToken_Entity)
      ).
    ASSIGN
      ENTRY(1,ocReturn,'|') = STRING(iStatusCode)
      ENTRY(2,ocReturn,'|') = cStatusReason
      ENTRY(3,ocReturn,'|') = cType
      ENTRY(4,ocReturn,'|') = STRING(cJSonFileName)
      ENTRY(5,ocReturn,'|') = STRING(cJSonString)
      obOk                  = TRUE
      .
  END.
  ELSE 
  DO:
    ASSIGN
      ENTRY(1,ocReturn,'|') = STRING(iStatusCode)
      ENTRY(2,ocReturn,'|') = rClientDinTero:cToken_StatusReason
      cToken                = ''
      obOk                  = FALSE
      .
  END.

END PROCEDURE.

PROCEDURE listProfiles:
  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    cAccess_Token: ' + ENTRY(2,icParam,'|') 
    ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    cToken_Type: ' + ENTRY(3,icParam,'|') 
    ).    


  /* Parametre for kall. */
  ASSIGN
    /*    cParametre = 'limit=1&starting_after=T11112542.4XopX7PCwvdZzWxu5anRmd'*/
    rClientDinTero:cFullPath     = 'https://api.dintero.com/checkouts/v1/admin/session/profiles'
    rClientDinTero:cAccess_Token = ENTRY(2,icParam,'|')  
    rClientDinTero:cToken_Type   = ENTRY(3,icParam,'|')
    cParametre                   = ENTRY(4,icParam,'|')
    ocReturn                     = '||||'
    .
      
  rClientDinTero:listProfiles( INPUT  cParametre,  
    OUTPUT iStatusCode,
    OUTPUT cStatusReason,
    OUTPUT cJSonString, 
    OUTPUT cJSonFileName,
    OUTPUT cType ) NO-ERROR.

  IF iStatusCode = 200 THEN
  DO:
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '    Entity: ' + STRING(rClientDinTero:cToken_Entity)
      ).
    ASSIGN
      ENTRY(1,ocReturn,'|') = STRING(iStatusCode)
      ENTRY(2,ocReturn,'|') = cStatusReason
      ENTRY(3,ocReturn,'|') = cType
      ENTRY(4,ocReturn,'|') = STRING(cJSonFileName)
      ENTRY(5,ocReturn,'|') = STRING(cJSonString)
      obOk                  = TRUE
      .
  END.
  ELSE 
  DO:
    ASSIGN
      ENTRY(1,ocReturn,'|') = STRING(iStatusCode)
      ENTRY(2,ocReturn,'|') = rClientDinTero:cToken_StatusReason
      cToken                = ''
      obOk                  = FALSE
      .
  END.

END PROCEDURE.

PROCEDURE createSession:
  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/
  DEFINE VARIABLE poJsonObject AS JsonObject NO-UNDO. 

  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    cAccess_Token: ' + ENTRY(2,icParam,'|') 
    ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    cToken_Type: ' + ENTRY(3,icParam,'|') 
    ).    


  /* Oppretter payload objectet for opprettelse av session. */ 
  /* Inneholder kundeid, email, mobilnr, kunde_id og tgSms. */   
  RUN getKOrdreSession(ENTRY(4,icParam,'|'),
    ENTRY(5,icParam,'|'),
    ENTRY(6,icParam,'|'),
    ENTRY(7,icParam,'|'),
    ENTRY(8,icParam,'|'),
    OUTPUT poJsonObject).

  /* Parametre for kall. */
  ASSIGN
    cParametre                   = '' 
    rClientDinTero:cFullPath     = 'https://api.dintero.com/checkouts/v1/sessions-profile'
    rClientDinTero:cAccess_Token = ENTRY(2,icParam,'|')  
    rClientDinTero:cToken_Type   = ENTRY(3,icParam,'|')
    ocReturn                     = '||||'
    .
      
  rClientDinTero:createSession( INPUT  cParametre,
    INPUT  poJsonObject,  
    OUTPUT iStatusCode,
    OUTPUT cStatusReason,
    OUTPUT cJSonString, 
    OUTPUT cJSonFileName,
    OUTPUT cType ) NO-ERROR.

  IF iStatusCode = 200 THEN
  DO:
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '    Entity: ' + STRING(rClientDinTero:cToken_Entity)
      ).
    ASSIGN
      ENTRY(1,ocReturn,'|') = STRING(iStatusCode)
      ENTRY(2,ocReturn,'|') = cStatusReason
      ENTRY(3,ocReturn,'|') = cType
      ENTRY(4,ocReturn,'|') = STRING(cJSonFileName)
      ENTRY(5,ocReturn,'|') = STRING(cJSonString)
      obOk                  = TRUE
      .
  END.
  ELSE 
  DO:
    ASSIGN
      ENTRY(1,ocReturn,'|') = STRING(iStatusCode)
      ENTRY(2,ocReturn,'|') = rClientDinTero:cToken_StatusReason
      cToken                = ''
      obOk                  = FALSE
      .
  END.


END PROCEDURE.

PROCEDURE getKOrdreSession:
  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER piId AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pieMail AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER piPhoneNumber AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER piKOrdre_Id AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER piSms AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER poJSon AS CLASS JsonObject NO-UNDO.

  DEF    VAR      cJSonString  AS LONGCHAR   NO-UNDO.
  /*    DEFINE VARIABLE poJson        AS JsonObject NO-UNDO.*/
  DEFINE VARIABLE poUrl        AS JsonObject NO-UNDO. 
  DEFINE VARIABLE poOrder      AS JsonObject NO-UNDO. 
  DEFINE VARIABLE poItemsArray AS JsonArray  NO-UNDO.
  DEFINE VARIABLE poCustomer   AS JsonObject NO-UNDO. 
    
  CREATE ttUrl.
  ASSIGN 
    return_url = "https://checkout.dintero.com/result"
    .
  
  FIND KOrdreHode NO-LOCK WHERE 
    KOrdreHode.KOrdre_Id = DEC(piKOrdre_Id) NO-ERROR.
      
  CREATE ttOrder.
  ASSIGN 
    ttOrder.amount             = 0 
    ttOrder.currency           = 'NOK' 
    ttOrder.merchant_reference = 'string' 
    .
    
  CREATE ttItems.    
  ASSIGN 
    ttItems.id          = "chair-1"
    ttItems.line_id     = "1"
    ttItems.description = "StablestolTNC"
    ttItems.quantity    = 1
    ttItems.amount      = 290
    ttItems.vat_amount  = 60
    ttItems.vat         = 25
    ttOrder.amount      = ttOrder.amount + ttItems.amount
    .
    
  CREATE ttItems.    
  ASSIGN 
    ttItems.id          = "chair-2"
    ttItems.line_id     = "2"
    ttItems.description = "StablestolTNC"
    ttItems.quantity    = 1
    ttItems.amount      = 290
    ttItems.vat_amount  = 60
    ttItems.vat         = 25
    ttOrder.amount      = ttOrder.amount + ttItems.amount
    .
    
  CREATE ttCustomer.
  ASSIGN 
    ttCustomer.customer_id  = piId
    ttCustomer.email        = pieMail
    ttCustomer.phone_number = piPhoneNumber
    /*      ttCustomer.customer_id = 'Are Eikrem'*/
    /*      ttCustomer.email = 'are@gant.no'     */
    /*      ttCustomer.phone_number = '48004066' */
    .   
    
    
  /* Create new JsonObjects */
  poJson       = NEW JsonObject().
  poUrl        = NEW JsonObject().
  poOrder      = NEW JsonObject().
  poItemsArray = NEW JsonArray().
  poCustomer   = NEW JsonObject().
    
  /* Bygger opp JSon objektet.                                            */
  /* Rekkefølgen det settes sammen på er viktig for å få ønsket resultat. */
  poUrl:Add("return_url",ttUrl.return_url).

  poCustomer:ADD("customer_id", ttCustomer.customer_id).
  poCustomer:ADD("email", ttCustomer.email).
  poCustomer:ADD("phone_number", ttCustomer.phone_number).
    
  poOrder:ADD("amount", ttOrder.amount).
  poOrder:ADD("currency", ttOrder.currency).
  poOrder:ADD("merchant_reference", ttOrder.merchant_reference).
    
  poOrder:Add("items", poItemsArray).
  poItemsArray:READ(TEMP-TABLE ttItems:HANDLE).
    
  poJson:Add("url", poUrl).
  poJson:Add("customer", poCustomer).
  poJson:Add("order", poOrder).
/*  poJson:Add("profile_id","T11112542.4XopPVLQtJfeUWbDHUPP8U"). /* Vipps */*/
  poJson:Add("profile_id","T11112542.4XopX7PCwvdZzWxu5anRmd"). /* Vipps */  

END PROCEDURE.

PROCEDURE listTransactions:
  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    cAccess_Token: ' + ENTRY(2,icParam,'|') 
    ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    cToken_Type: ' + ENTRY(3,icParam,'|') 
    ).    

  /* Parametre for kall. */
  ASSIGN
    /*    cParametre = 'limit=1&starting_after=T11112542.4XCQ9MC8L2pKYfpD1bUTMv'*/
    rClientDinTero:cFullPath     = 'https://api.dintero.com/checkouts/v1/transactions'
    rClientDinTero:cAccess_Token = ENTRY(2,icParam,'|')  
    rClientDinTero:cToken_Type   = ENTRY(3,icParam,'|')
    cParametre                   = ENTRY(4,icParam,'|')
    ocReturn                     = '||||'
    .

  rClientDinTero:listTransactions( INPUT  cParametre ,
    OUTPUT iStatusCode,
    OUTPUT cStatusReason,
    OUTPUT cJSonString,
    OUTPUT cJSonFileName,
    OUTPUT cType ) NO-ERROR.
  IF iStatusCode = 200 THEN
  DO:
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '    Entity: ' + STRING(rClientDinTero:cToken_Entity)
      ).
    ASSIGN
      ENTRY(1,ocReturn,'|') = STRING(iStatusCode)
      ENTRY(2,ocReturn,'|') = cStatusReason
      ENTRY(3,ocReturn,'|') = cType
      ENTRY(4,ocReturn,'|') = STRING(cJSonFileName)
      ENTRY(5,ocReturn,'|') = STRING(cJSonString)
      obOk                  = TRUE
      .
  END.
  ELSE 
  DO:
    ASSIGN
      ENTRY(1,ocReturn,'|') = STRING(iStatusCode)
      ENTRY(2,ocReturn,'|') = rClientDinTero:cToken_StatusReason
      cToken                = ''
      obOk                  = FALSE
      .
  END.


END PROCEDURE.

PROCEDURE getTransaction:
  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    cAccess_Token: ' + ENTRY(2,icParam,'|') 
    ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    cToken_Type: ' + ENTRY(3,icParam,'|') 
    ).    

  /* Parametre for kall. */
  ASSIGN
    /*    cParametre = 'limit=1&starting_after=T11112542.4XCQ9MC8L2pKYfpD1bUTMv'*/
    cParametre                   = ENTRY(4,icParam,'|')
    rClientDinTero:cFullPath     = 'https://api.dintero.com/checkouts/v1/transactions/' + ENTRY(2,cParametre,'=')
    rClientDinTero:cAccess_Token = ENTRY(2,icParam,'|')  
    rClientDinTero:cToken_Type   = ENTRY(3,icParam,'|')
    cParametre                   = ENTRY(4,icParam,'|')
    ocReturn                     = '||||'
    .

  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    FullPath: ' + rClientDinTero:cFullPath 
    ).    
 
  rClientDinTero:GetTransaction( INPUT  cParametre ,
    OUTPUT iStatusCode,
    OUTPUT cStatusReason,
    OUTPUT cJSonString,
    OUTPUT cJSonFileName,
    OUTPUT cType ) NO-ERROR.

  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    iStatusCode: ' + STRING(iStatusCode) 
    ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    cStatusReason: ' + cStatusReason 
    ).    
                              
  IF iStatusCode = 200 THEN
  DO:
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '    Entity: ' + STRING(rClientDinTero:cToken_Entity)
      ).
    ASSIGN
      ENTRY(1,ocReturn,'|') = STRING(iStatusCode)
      ENTRY(2,ocReturn,'|') = cStatusReason
      ENTRY(3,ocReturn,'|') = cType
      ENTRY(4,ocReturn,'|') = STRING(cJSonFileName)
      ENTRY(5,ocReturn,'|') = STRING(cJSonString)
      obOk                  = TRUE
      .
  END.
  ELSE 
  DO:
    ASSIGN
      ENTRY(1,ocReturn,'|') = STRING(iStatusCode)
      ENTRY(2,ocReturn,'|') = rClientDinTero:cToken_StatusReason
      cToken                = ''
      obOk                  = FALSE
      .
  END.



END PROCEDURE.

PROCEDURE voidTransaction:
  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    cAccess_Token: ' + ENTRY(2,icParam,'|') 
    ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    cToken_Type: ' + ENTRY(3,icParam,'|') 
    ).    

  /* Parametre for kall. */
  ASSIGN
    cParametre                   = ENTRY(4,icParam,'|')
    rClientDinTero:cFullPath     = 'https://api.dintero.com/checkouts/v1/transactions/T11112542/void'
    rClientDinTero:cAccess_Token = ENTRY(2,icParam,'|')  
    rClientDinTero:cToken_Type   = ENTRY(3,icParam,'|')
    cParametre                   = ENTRY(4,icParam,'|')
    ocReturn                     = '||||'
    .

  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    FullPath: ' + rClientDinTero:cFullPath 
    ).    
 
  rClientDinTero:VoidTransaction( INPUT  cParametre ,
    OUTPUT iStatusCode,
    OUTPUT cStatusReason,
    OUTPUT cJSonString,
    OUTPUT cJSonFileName,
    OUTPUT cType ) NO-ERROR.

  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    iStatusCode: ' + STRING(iStatusCode) 
    ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    cStatusReason: ' + cStatusReason 
    ).    
                              
  IF iStatusCode = 200 THEN
  DO:
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '    Entity: ' + STRING(rClientDinTero:cToken_Entity)
      ).
    ASSIGN
      ENTRY(1,ocReturn,'|') = STRING(iStatusCode)
      ENTRY(2,ocReturn,'|') = cStatusReason
      ENTRY(3,ocReturn,'|') = cType
      ENTRY(4,ocReturn,'|') = STRING(cJSonFileName)
      ENTRY(5,ocReturn,'|') = STRING(cJSonString)
      obOk                  = TRUE
      .
  END.
  ELSE 
  DO:
    ASSIGN
      ENTRY(1,ocReturn,'|') = STRING(iStatusCode)
      ENTRY(2,ocReturn,'|') = rClientDinTero:cToken_StatusReason
      cToken                = ''
      obOk                  = FALSE
      .
  END.


END PROCEDURE.

PROCEDURE refundTransaction:
  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/
  DEFINE VARIABLE poJsonObject AS JsonObject NO-UNDO. 

  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    cAccess_Token: ' + ENTRY(2,icParam,'|') 
    ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    cToken_Type: ' + ENTRY(3,icParam,'|') 
    ).    

  /* Oppretter payload objectet for opprettelse av session. */    
  RUN getKOrdreRefund(ENTRY(4,icParam,'|'),
    '',
    '',
    OUTPUT poJsonObject).

  /* Parametre for kall. */
  ASSIGN
    cParametre                   = ENTRY(4,icParam,'|')
    rClientDinTero:cFullPath     = 'https://api.dintero.com/checkouts/v1/transactions/T11112542/refund'
    rClientDinTero:cAccess_Token = ENTRY(2,icParam,'|')  
    rClientDinTero:cToken_Type   = ENTRY(3,icParam,'|')
    cParametre                   = ENTRY(4,icParam,'|')
    ocReturn                     = '||||'
    .

  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    FullPath: ' + rClientDinTero:cFullPath 
    ).    
 
  rClientDinTero:refundTransaction( INPUT  cParametre ,
    INPUT  poJsonObject,  
    OUTPUT iStatusCode,
    OUTPUT cStatusReason,
    OUTPUT cJSonString,
    OUTPUT cJSonFileName,
    OUTPUT cType ) NO-ERROR.

  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    iStatusCode: ' + STRING(iStatusCode) 
    ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    cStatusReason: ' + cStatusReason 
    ).    
                              
  IF iStatusCode = 200 THEN
  DO:
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '    Entity: ' + STRING(rClientDinTero:cToken_Entity)
      ).
    ASSIGN
      ENTRY(1,ocReturn,'|') = STRING(iStatusCode)
      ENTRY(2,ocReturn,'|') = cStatusReason
      ENTRY(3,ocReturn,'|') = cType
      ENTRY(4,ocReturn,'|') = STRING(cJSonFileName)
      ENTRY(5,ocReturn,'|') = STRING(cJSonString)
      obOk                  = TRUE
      .
  END.
  ELSE 
  DO:
    ASSIGN
      ENTRY(1,ocReturn,'|') = STRING(iStatusCode)
      ENTRY(2,ocReturn,'|') = rClientDinTero:cToken_StatusReason
      cToken                = ''
      obOk                  = FALSE
      .
  END.

END PROCEDURE.

PROCEDURE getKOrdreRefund:
  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
     
  {
      "amount": 0,
      "reason": "string",
      "items": [
          {
              "id": "item_01",
              "groups": [
                  {
                      "id": "B234",
                      "name": "Stol"
                  }
              ],
              "line_id": "1",
              "description": "Stablestol",
              "quantity": 1,
              "amount": 29990,
              "vat_amount": 6000,
              "vat": 25
          }
      ]
  }   

  /*    poRefund:ADD("amount", ttRefund.amount).*/
  /*    poJson:Read( TEMP-TABLE ttRefund:DEFAULT-BUFFER-HANDLE, TRUE).*/
  /*    poJson:Read( DATASET dsItems:HANDLE, TRUE).                   */
  /*    poItemsArray:READ(DATASET dsItems:HANDLE).*/
  ------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piId AS CHARACTER NO-UNDO.
  DEFINE INPUT  PARAMETER pieMail AS CHARACTER NO-UNDO.
  DEFINE INPUT  PARAMETER piPhoneNumber AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER poJSon AS CLASS JsonObject NO-UNDO.

  DEFINE VARIABLE piLoop       AS INTEGER    NO-UNDO.
  DEFINE VARIABLE piAnt        AS INTEGER    NO-UNDO.
  DEF    VAR      cJSonString  AS LONGCHAR   NO-UNDO.
  DEFINE VARIABLE poItemsArray AS JsonArray  NO-UNDO.
  DEFINE VARIABLE poItems      AS JsonObject NO-UNDO. 
  DEFINE VARIABLE poGroupArray AS JsonArray  NO-UNDO.
  DEFINE VARIABLE poGroup      AS JsonObject NO-UNDO. 
  DEFINE VARIABLE pcObject     AS LONGCHAR   NO-UNDO.
    
  CREATE ttRefund.
  ASSIGN 
    ttRefund.amount = 0 
    ttRefund.reason = '14 Feil farge' 
    .
    
  DO piant = 1 TO 5:
    CREATE ttItems.    
    ASSIGN 
      ttItems.id          = "chair-" + STRING(piant)
      ttItems.line_id     = "1"
      ttItems.Group_Id    = "B234"
      ttItems.description = "StablestolTNC"
      ttItems.quantity    = 1
      ttItems.amount      = 290
      ttItems.vat_amount  = 60
      ttItems.vat         = 25
      ttRefund.amount     = ttRefund.amount + ttItems.amount
      .
  END.

  CREATE ttGroups.
  ASSIGN 
    ttGroups.Group_Id  = "B234"
    ttGroups.GroupName = "Stol"
    .
    
  /*    DATASET dsItems:WRITE-JSON('file','konv\ttitems' + STRING(ETIME) + '.json',TRUE,'UTF-8',FALSE,TRUE).*/

  /* Create new JsonObjects */
  poJson       = NEW JsonObject().
  poItems      = NEW JsonObject().
  poItemsArray = NEW JsonArray().
  poGroupArray = NEW JsonArray(1).
  poGroup      = NEW JsonObject().
    
  piLoop = 0.
  ITEMBLOKK:
  FOR EACH ttItems, 
    FIRST ttGroups OF ttItems:
      
    poGroup:Read( TEMP-TABLE ttGroups:DEFAULT-BUFFER-HANDLE, TRUE).
    poGroupArray:ADD(poGroup). /* Skal bare ha en ekstent i denne arrayen. */
    poGroupArray:remove(1).
             
    piLoop = piLoop + 1.
    poItems:Read( TEMP-TABLE ttItems:DEFAULT-BUFFER-HANDLE, TRUE).
    poItems:Add("groups", poGroupArray).
      
    poItemsArray:ADD(poItems).
  END. /* ITEMBLOKK */

  /* Bygger opp JSon retur objektetet.                                            */
  poJSon:ADD("amount", ttRefund.amount).
  poJSon:ADD("reason", ttRefund.reason).
  poJSon:Add("items", poItemsArray).
   
  /* For sjekk av det genererte dataobjektet. */
  poJson:Write(pcObject, TRUE).
  OUTPUT TO value('konv\test' + STRING(ETIME) + '.json').
  PUT UNFORMATTED 
    STRING(pcObject)
    SKIP.
  OUTPUT CLOSE.

END PROCEDURE.

FINALLY :

  EMPTY TEMP-TABLE ttoJSon. 
  EMPTY TEMP-TABLE ttOrder.
  EMPTY TEMP-TABLE ttRefund.
  EMPTY TEMP-TABLE ttUrl.
  EMPTY TEMP-TABLE ttItems.
  EMPTY TEMP-TABLE ttGroups.

END FINALLY.