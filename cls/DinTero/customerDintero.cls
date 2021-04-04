 
/*------------------------------------------------------------------------
   File        : customerDintero
   Purpose     : 
   Syntax      : 
   Description : Inneholder kall til customer API'et hos Dintero.
   Author(s)   : tomn
   Created     : Sun Nov 01 06:40:55 CET 2020
   Notes       : TN 27/3-21
 ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Json.ObjectModel.ObjectModelParser.
USING Progress.Json.ObjectModel.JsonObject.
USING Progress.Json.ObjectModel.JsonArray.
/*USING OpenEdge.Net.HTTP.Lib.ClientLibraryBuilder.*/
USING OpenEdge.Net.HTTP.*.
USING OpenEdge.Net.*.

CLASS cls.DinTero.customerDintero: 
  /*------------------------------------------------------------------------------
   Purpose:
   Notes:

    rStandardFunksjoner:SkrivTilLogg(cLogg,
      'Start' 
      ).    
  ------------------------------------------------------------------------------*/
  {cls\dintero\ttMedlem.i}
  {cls\dintero\dsMedlem.i}
  {cls\dintero\ttBong.i}
  {cls\dintero\dsBong.i}
  {cls\dintero\ttPOSBong.i}
  {cls\dintero\dsPOSBong.i}
  {cls\Dintero\ttCategory.i}
  {cls\Dintero\dsCategory.i}
  {cls\dintero\ttCustomerObj.i}
  {cls\dintero\dsCustomerObj.i}
  {cls\dintero\ttReceipts.i}
  {cls\dintero\dsReceipts.i}
 
  DEFINE VARIABLE cLogg               AS CHARACTER                      NO-UNDO.
  DEFINE VARIABLE cAid                AS CHARACTER                      NO-UNDO.
  DEFINE VARIABLE cToken_Type         AS CHARACTER                      NO-UNDO.
  DEFINE VARIABLE cAccess_Token       AS CHARACTER                      NO-UNDO.
  DEFINE VARIABLE iStatusCode         AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE cStatusReason       AS CHARACTER                      NO-UNDO.
  DEFINE VARIABLE bTest               AS LOG                            NO-UNDO.
  
  DEFINE VARIABLE lcMedlemJSon        AS LONGCHAR                       NO-UNDO.
  DEFINE VARIABLE oClient             AS IHttpClient                    NO-UNDO.
  DEFINE VARIABLE oRequest            AS IHttpRequest                   NO-UNDO.
  DEFINE VARIABLE oResponse           AS IHttpResponse                  NO-UNDO.
  DEFINE VARIABLE oEntity             AS OBJECT                         NO-UNDO.
  DEFINE VARIABLE myParser            AS ObjectModelParser              NO-UNDO.
  DEFINE VARIABLE oURL                AS URI                            NO-UNDO.
    
  DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.
  DEFINE VARIABLE rTokenDintero       AS cls.Dintero.tokenDintero       NO-UNDO.
      
  CONSTRUCTOR PUBLIC customerDintero (  ):
    SUPER ().

    /* very useful information to understand what is being sent and received under 
        the covers */
/*    SESSION:ERROR-STACK-TRACE = TRUE.                                                                     */
/*    SESSION:DEBUG-ALERT       = TRUE.                                                                     */
/*    LOG-MANAGER:LOGFILE-NAME  = 'LOG-ManagerCustomerDintero.cls' + REPLACE(STRING(TODAY),'/','') + '.log'.*/
/*    LOG-MANAGER:LOGGING-LEVEL = 4.                                                                        */
/*    LOG-MANAGER:CLEAR-LOG().                                                                              */

    rStandardFunksjoner = NEW cls.StdFunk.StandardFunksjoner( ) NO-ERROR.
    rTokenDintero = NEW cls.Dintero.TokenDintero( ) NO-ERROR.
    
    ASSIGN 
      bTest = FALSE
      cLogg = 'customerDintero' + REPLACE(REPLACE(REPLACE(STRING(NOW,"99/99/9999"),'/',''),':',''),' ','_')
      /*      cAid          = 'T11112542'                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  */
      /*      cToken_Type   = 'Bearer'                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     */
      /*      cAccess_Token = 'eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCIsImtpZCI6ImFwaS5kaW50ZXJvLmNvbS8wMWUyY2I2ZWZiMTVlNTAyNmQ1OTU3Njc3MjkwZDVkMDY0ZDc1N2ZmIn0.eyJpc3MiOiJodHRwczovL2FwaS5kaW50ZXJvLmNvbSIsImF1ZCI6Imh0dHBzOi8vVDExMTEyNTQyQGFwaS5kaW50ZXJvLmNvbS92MS9hY2NvdW50cy9UMTExMTI1NDIiLCJzdWIiOiJjMGZhYjEwZi1lY2M1LTRjOGQtYTE1Mi05M2ZkMGZmMzE0YWIiLCJzY29wZXMiOiJyZWFkOmNoZWNrb3V0IHdyaXRlOmNoZWNrb3V0IHdyaXRlOmRpc2NvdW50czovYXZhaWxhYmxlX2Zvcl9yZWNlaXB0IHdyaXRlOm5vdGlmaWNhdGlvbnMgd3JpdGU6cmVjZWlwdHMgYWRtaW46Y3VzdG9tZXJzIiwidmVyc2lvbiI6MiwiaWF0IjoxNjA1MDk2NTE4LCJleHAiOjE2MDUxMTA5MTh9.fzPBQB-5q_Cg0C5fQkx0TirzvN2wuwBGabRyeke9f3dgkx0HbXrb8uGymMTW6FY76_MbmZv4XOMcDFqVqjiGyO9DCj8LnQ1W5cJl8HfTSeFp9K2tONpY2z34xIlqs3Qsy-ntD-ykg_qCwZ5PWFAX6jmXke0X78_P1AGBjWHJQCez8jICT-gO4VCR2QmnhKSdiprBPI01UIDUHIXGGaa3y-dKOOkPcZCGNaLLnzqe9BrA1tgqytWgzND84pdCJJhIwbLxOKsIN7yqCqK8cRZaOWHX8oyRYaOonKz9S6RnVrbq1U5H_6_7aS7dIEzMue7Vmh595dq-vuVxNbytQ2yXrg'*/
      .
    
  END CONSTRUCTOR.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes: Datasettet skal bare inneholde et medlem. 
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC LOGICAL CreateNewCustomer( INPUT DATASET dsMedlem,
    OUTPUT pcReturn AS CHARACTER ):

    DEFINE VARIABLE pcFullPath     AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE pcJsonRespons  AS LONGCHAR   NO-UNDO.
    DEFINE VARIABLE pcResponsType  AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE pcStatusReason AS CHARACTER  NO-UNDO.
    
    DEFINE VARIABLE bResult        AS LOG        NO-UNDO.
    DEFINE VARIABLE poJsonObject   AS JsonObject NO-UNDO.

    FIND FIRST ttMedlem.

    IF AVAILABLE ttMedlem THEN 
    DO:    
      /* Er token fortsatt gyldig, hentes ikke nytt token */
      IF NOT rtokenDintero:sjekkSysParaToken() THEN 
      DO:
        IF NOT rtokenDintero:getToken(OUTPUT iStatusCode, OUTPUT pcStatusReason )THEN 
        DO:
          ASSIGN 
            bResult = FALSE
            .
          RETURN bResult.
        END.
      END.
      
      IF bTest THEN
      DO: 
        rStandardFunksjoner:SkrivTilLogg(cLogg,
          '  CreateNewCustomer: mottatt medlem som skal opprettes ' + STRING(ttMedlem.MedlemsNr) 
          ).    
      END.
      /* Konverterer datasett til JSon og sender det nye medlemmet. */
      IF datasetMedlemToJSon ( INPUT DATASET dsMedlem, OUTPUT poJSonObject) THEN
      SENDOBJECT: 
      DO:
        rStandardFunksjoner:SkrivTilLogg(cLogg,
          '    CreateNewCustomer: JSon melding opprettet ' + STRING(ttMedlem.MedlemsNr) 
          ).    

        /* Skriver respons til fil for debug. */
        IF bTest THEN
          CAST(poJSonObject, JSONObject):WriteFile('konv\CreateNewCustomerPayload' + STRING(ETIME) + '.json').

        ASSIGN 
          pcFullPath = 'https://api.dintero.com/v1/accounts/' + rTokenDintero:cAid + '/customers/users'.
      
        /* Creating a Client. */
        oClient = ClientBuilder:Build()
                    :KeepCookies(CookieJarBuilder:Build()
                    :CookieJar):Client
          .
        oRequest = RequestBuilder:Post(pcFullPath) /* URL */
                          :AddHeader('Authorization', rtokenDintero:cToken_Type + ' ' + rtokenDintero:cAccess_Token) /* cToken_Type = 'Bearer' */
                          :AcceptJson() /* we want to get JSON back */
                          :AddJSonData(poJSonObject) /* Her legges JSon meldingen inn */
                          :Request.
        /* Bygger respons objekstet. Det er tomt her, men fylles når forespørselen eksekverees. */
        oResponse = ResponseBuilder:Build():Response.
        /* Execute the request.                                                              */
        /* Sender forespørselen til API'et med preppet request object og tom respons object. */
        oClient:Execute(oRequest, oResponse).   
        oEntity = oResponse:Entity.
        ASSIGN 
          iStatusCode   = oResponse:StatusCode
          cStatusReason = oResponse:StatusReason
          .
        DO:      
          /* Her er det oEntity objectet som behandles. */
          castObject(INPUT 'CreateNewCustomerResponse', OUTPUT pcJsonRespons, OUTPUT pcResponsType).
        END.       

        /* Nå må medlemmet oppdateres med Dintero customer_id. */
        /* --------------------------------------------------- */
        IF setCustomerIdMedlem( INPUT ttMedlem.MedlemsNr, INPUT pcJsonRespons, INPUT pcResponsType ) THEN 
          bResult = TRUE.
      /* --------------------------------------------------- */
                    
      /* Opprydding */
      /*          DELETE OBJECT poJSonObject.*/
          
      END. /* SENDOBJECT */
      ELSE 
      DO:
        rStandardFunksjoner:SkrivTilLogg(cLogg,
          '  CreateNewCustomer: Feilet ved konvertering til JSon.' 
          ).    
      END.
      bResult = TRUE.
    END.
    ELSE 
    DO:
      rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  CreateNewCustomer: mottatt tomt datasett' 
        ).    
    END.
    
    RETURN bResult.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose: Sjekker om medlemmet finnes hos Dintero.  Sjekken kan gjøres 
            med eMail ellet mobilnr.
                        
   Notes:   Respons er 200 hvis bruker finnes. 
            JSon melding som returneres, inneholder eventuelle feil hvis det 
            ikke ble gitt en 200 kode som respons.
            
            Metoden returnerer OK hvis medlemmet finnes.
            
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC LOGICAL UserExist( INPUT piType AS INTEGER,
    INPUT pcData AS CHARACTER,
    OUTPUT pcStatusReason AS CHARACTER,
    OUTPUT piStatus AS INTEGER ):
      
    DEFINE VARIABLE bResult       AS LOG        NO-UNDO.
    DEFINE VARIABLE piLoop        AS INTEGER    NO-UNDO.
    DEFINE VARIABLE pcTekst       AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE pcFullPath    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE plcJsonString AS LONGCHAR   NO-UNDO.
    DEFINE VARIABLE poJsonObject  AS JsonObject NO-UNDO.
    DEFINE VARIABLE poEntityArray AS JsonArray  NO-UNDO.
    DEFINE VARIABLE pcCustomer_Id AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE piAntall AS INTEGER NO-UNDO.
    DEFINE VARIABLE pcReturn AS CHARACTER NO-UNDO.
     
    /* Antall ekstenter angis ikke. Fastsettes når variabelen fylles første gang. */
    DEFINE VARIABLE pcEntityArray AS CHARACTER  EXTENT NO-UNDO. 

    DEFINE BUFFER bufMedlem FOR Medlem.

    IF AVAILABLE Medlem THEN 
      RELEASE Medlem.

    /* Er token fortsatt gyldig, hentes ikke nytt token */
    IF NOT rtokenDintero:sjekkSysParaToken() THEN 
    DO:
      IF NOT rtokenDintero:getToken(OUTPUT iStatusCode, OUTPUT pcStatusReason )THEN 
      DO:
        ASSIGN 
          bResult = FALSE
          .
        RETURN bResult.
      END.
    END.

    /* EMail */
    IF piType = 1 THEN
    EMAILSJEKK: 
    DO:
      ASSIGN 
        pcFullPath = 'https://api.dintero.com/v1/accounts/' + rTokenDintero:cAid + '/customers/users?email=' + pcData
        .
      /* Sjekker om medlemmet finnes lokalt. */
      FIND LAST medlem NO-LOCK WHERE 
        Medlem.ePostAdresse = pcData NO-ERROR.
    END. /* EMAILSJEKK */
    ELSE IF piType = 2 OR piType = 3 THEN 
      MOBILSJEKK:
      DO:
        prepMobilNr( 1, INPUT-OUTPUT pcData ).
        ASSIGN 
          pcFullPath = 'https://api.dintero.com/v1/accounts/' + rTokenDintero:cAid + '/customers/users?phone_number=' + pcData
          .
      /* Sjekker om medlemmet finnes lokalt. */
      FIND LAST medlem NO-LOCK WHERE 
        Medlem.MobilTlf = pcData NO-ERROR.
      END. /* MOBILSJEKK */
    ELSE 
      DO:
        ASSIGN 
          bResult        = FALSE
          pcStatusReason = 'Ukjent søk type. Bruk 1-MobilNr, 2-eMail.'
          .
        RETURN bResult.
      END.
      
    /* Medlemmet er funnet ved lokalt oppslag. Da returerer vi herfra */
    IF AVAILABLE Medlem THEN 
      DO:
        FIND FIRST MedlemsKort OF Medlem NO-LOCK NO-ERROR.
        ASSIGN   
          bResult        = TRUE
          iStatusCode    = 201
          pcStatusReason = STRING(Medlem.MedlemsNr) + ';' +
                           STRING(MedlemsKort.KortNr) + ';' +  
                           STRING(poJsonObject:GetJsonText("first_name")) + ';' +
                           STRING(poJsonObject:GetJsonText("last_name")) + ';' +
                           STRING(poJsonObject:GetJsonText("email")) + ';' +
                           STRING(poJsonObject:GetJsonText("phone_number"))
          .
        RETURN bResult.
      END.
      
    /*
      TN 15/11-20 Løsningen på problemet med '+' er beskrevet her.
      Benytter nå oURL objektet, med ecode satt til false.
      Ref: https://community.progress.com/s/article/requestbuilder-is-decoding-url
    */
    oURL = URI:Parse(pcFullPath, FALSE).
     
    /* Creating a Client. */
    oClient = ClientBuilder:Build()
              :KeepCookies(CookieJarBuilder:Build()
              :CookieJar):Client
      .
    oRequest = RequestBuilder:Get(oURL) /* URL */
                    :AddHeader('Authorization', rTokenDintero:cToken_Type + ' ' + rTokenDintero:cAccess_Token) /* cToken_Type = 'Bearer' */
                    :AcceptJson() /* we want to get JSON back */
                    :Request.
    /* Viser URL'en slik den blir sendt til API'et. Etter at den er encoded.*/ 
    /*    MESSAGE 'URL3:' oRequest:URI:ToString()*/
    /*    VIEW-AS ALERT-BOX.                     */
    
    /* Bygger respons objekstet. Det er tomt her, men fylles når forespørselen eksekverees. */
    oResponse = ResponseBuilder:Build():Response.
    
    /* Execute the request.                                                              */
    /* Sender forespørselen til API'et med preppet request object og tom respons object. */
    oClient:Execute(oRequest, oResponse).   
    
    oEntity = oResponse:Entity.  
    
    ASSIGN 
      iStatusCode    = oResponse:StatusCode
      pcStatusReason = STRING(iStatusCode) + ' ' + oResponse:StatusReason
      piStatus       = iStatusCode
      .
 
    IF iStatusCode = 200 THEN
    STATUS200: 
    DO:
      
      /* Rresponsen er en Array hvis oppslaget fant et medlem. */
      poEntityArray  = CAST(oResponse:Entity,JsonArray).
      /* Det ligger bare et object i array'en. */
      poJsonObject = poEntityArray:GETJSONOBJECT(1) NO-ERROR.
      /* Tom Array' Søk misslykkes. */
      IF ERROR-STATUS:ERROR THEN 
      DO:
        ASSIGN 
          bResult = FALSE
          .
        IF piType = 1 THEN 
          pcStatusReason = STRING(iStatusCode) + ' FEIL Ukjent eMail adresse (' + pcData + ').'.
        ELSE IF piType = 2 THEN 
            pcStatusReason = STRING(iStatusCode) + ' FEIL Ukjent mobilnr. (med landkode) (' + pcData + ').'.
          ELSE IF piType = 3 THEN 
              pcStatusReason = STRING(iStatusCode) + ' FEIL Ukjent mobilnr. (uten landkode) (' + pcData + ').'.
        
        RETURN bResult.
      END.
       
      /* Her plukkes navnene på feltene på øverste nivå ut. */        
      pcEntityArray = poJsonObject:GetNames() NO-ERROR.
      
      ASSIGN 
        bResult       = TRUE
        pcCustomer_Id = STRING(poJsonObject:GetJsonText("customer_id"))
        .

      /* Oppdaterer/oppretter medlemmet hvis det ikke finnes. */  
      IF CustomerCollection(INPUT pcCustomer_Id, 
           OUTPUT piAntall,
           OUTPUT pcReturn ) THEN 
        RUN cls\dintero\customerToMedlem.p (INPUT DATASET dsCustCustomer, OUTPUT pcReturn).
      FIND LAST Medlem NO-LOCK WHERE 
        Medlem.EksterntMedlemSnr = pcCustomer_Id NO-ERROR. 
      IF AVAILABLE Medlem THEN 
      DO:
        FIND FIRST MedlemsKort OF Medlem NO-LOCK NO-ERROR.
        ASSIGN   
          pcStatusReason = STRING(Medlem.MedlemsNr) + ';' +
                           STRING(MedlemsKort.KortNr) + ';' +  
                           STRING(poJsonObject:GetJsonText("first_name")) + ';' +
                           STRING(poJsonObject:GetJsonText("last_name")) + ';' +
                           STRING(poJsonObject:GetJsonText("email")) + ';' +
                           STRING(poJsonObject:GetJsonText("phone_number"))
          .
      END.  
    END. /* STATUS200 */ 
    ELSE 
    DO:
      ASSIGN 
        bResult = FALSE 
        .
    END.
      
    RETURN bResult.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose: Henter over alle medlemmer fra Dintero.
   Notes:   Medlemmene som hentes over, opprettes/oppdateres mot medlemstabellen 
            i PRS og adderes til temp tabellen.
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC LOGICAL CustomerCollection(INPUT pcSearch AS CHARACTER,
    OUTPUT piAntall AS INTEGER,
    OUTPUT pcStatusReason AS CHARACTER ):

    DEFINE VARIABLE piLimit       AS INTEGER    NO-UNDO.
    DEFINE VARIABLE pcStarting_after AS CHARACTER NO-UNDO.
    DEFINE VARIABLE bResult       AS LOG        NO-UNDO.
    DEFINE VARIABLE pcReturn      AS CHARACTER  NO-UNDO. 
    DEFINE VARIABLE piLoop        AS INTEGER    NO-UNDO.
    DEFINE VARIABLE pcTekst       AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE pcFullPath    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE plcJsonString AS LONGCHAR   NO-UNDO.
    DEFINE VARIABLE poJsonObject  AS JsonObject NO-UNDO.
    DEFINE VARIABLE poEntityArray AS JsonArray  NO-UNDO.
    DEFINE VARIABLE pcCustomer_Id AS CHARACTER  NO-UNDO. 
    /* Antall ekstenter angis ikke. Fastsettes når variabelen fylles første gang. */
    DEFINE VARIABLE pcEntityArray AS CHARACTER  EXTENT NO-UNDO. 

    DEFINE BUFFER bufMedlem FOR Medlem.
    
    /* Skal ha med alle medlemmer. */
    ASSIGN 
      piLimit = 0 /* 0 gir alle */
      piLimit = 99 /* Henter maks antall */
      pcStarting_after = '' /* '' gir alle */
      .
      
    /* Er token fortsatt gyldig, hentes ikke nytt token */
    IF NOT rtokenDintero:sjekkSysParaToken() THEN 
    DO:
      IF NOT rtokenDintero:getToken(OUTPUT iStatusCode, OUTPUT pcStatusReason )THEN 
      DO:
        ASSIGN 
          bResult = FALSE
          .
        RETURN bResult.
      END.
    END.

    IF pcSearch > '' AND pcSearch BEGINS '+' THEN 
      prepMobilNr( 1, pcSearch).
         
    IF pcStarting_after = '' AND piLimit = 0 AND pcSearch = '' THEN 
      ASSIGN 
        pcFullPath = 'https://api.dintero.com/v1/accounts/' + rTokenDintero:cAid + '/customers/users'
        .
    ELSE 
    DO:
      pcFullPath = 'https://api.dintero.com/v1/accounts/' + rTokenDintero:cAid + '/customers/users?'.
          
      IF pcStarting_after > '' THEN 
        pcFullPath = pcFullPath + 'starting_after=' + pcStarting_after + '&'.
        
      IF piLimit > 0 THEN 
        pcFullPath = pcFullPath + 'limit=' + STRING(piLimit) + '&'.
        
      IF pcSearch > '' THEN 
        pcFullPath = pcFullPath + 'search=' + pcSearch.
      
      pcFullPath = RIGHT-TRIM(pcFullPath,'&').
    END.
    
    oURL = URI:Parse(pcFullPath, FALSE).
    oClient = ClientBuilder:Build()
              :KeepCookies(CookieJarBuilder:Build()
              :CookieJar):Client
      .
    oRequest = RequestBuilder:Get(oURL) /* URL */
                    :AddHeader('Authorization', rTokenDintero:cToken_Type + ' ' + rTokenDintero:cAccess_Token) /* cToken_Type = 'Bearer' */
                    :AcceptJson() /* we want to get JSON back */
                    :Request.
    oResponse = ResponseBuilder:Build():Response.
    
    oClient:Execute(oRequest, oResponse).   
    oEntity = oResponse:Entity.  

    ASSIGN 
      iStatusCode    = oResponse:StatusCode
      pcStatusReason = STRING(iStatusCode) + ' ' + oResponse:StatusReason
      .

    IF iStatusCode = 200 THEN
    STATUS200: 
    DO:
      /* Rresponsen er en Array. Inni ligger de medlemmmene som ble funnet i pooslaget. */
      poEntityArray  = CAST(oResponse:Entity,JsonArray).
      /* Sjekker antall elementer i arrayen. */
      piAntall = poEntityArray:LENGTH. 
      IF piAntall = 0 THEN 
        DO:
          ASSIGN 
            bResult  = TRUE
            piAntall = 0
            .
          pcStatusReason = STRING(iStatusCode) + 'Ingen medlemmer funnet.'.
          
          RETURN bResult.
        END.
      IF bTest THEN 
       CAST(poEntityArray, JsonArray):WriteFile('konv\customerCollectRespons' + STRING(ETIME) + '.json').

      rStandardFunksjoner:SkrivTilLogg(cLogg,
        'CustomerCollection: Hentet ' + STRING(poEntityArray:LENGTH) + ' medlemmer.'
        ).    
      /* Oppdaterer/oppretter medlemmene. */
      DO piLoop = 1 TO poEntityArray:LENGTH:
        EMPTY TEMP-TABLE ttCustcustomer.
        poJsonObject = poEntityArray:GETJSONOBJECT(piLoop).

        IF bTest THEN 
         CAST(poJsonObject, JsonObject):WriteFile('konv\customerCollectResponsObject_' + STRING(piLoop) + '_' + STRING(ETIME) + '.json').
        
        RUN cls\dintero\customerToDataset.p(poJsonObject, OUTPUT DATASET dsCustCustomer ).
        IF bTest THEN 
          DATASET dsCustCustomer:WRITE-JSON('file',
                                            'konv\customerCollectResponsCustomer_' + 
                                              STRING(piLoop) + '_' + 
                                              STRING(ETIME) + 
                                              '.json',
                                              TRUE).
        
        RUN cls\dintero\customerToMedlem.p(INPUT DATASET dsCustCustomer, OUTPUT pcReturn ).
      END.
    END. /* STATUS200 */ 
    ELSE 
    DO:
      ASSIGN 
        bResult = FALSE 
        .
    END.
    
    RETURN bResult. 

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC VOID CustomerDetails(  ):
    
    RETURN.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose: Oppdaterer medlemsinfo fra Medlemstabellen mot dintero.
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC LOGICAL UpdateCustomer( DATASET dsMedlem,  
    OUTPUT pcReturn AS CHARACTER ):
    DEFINE VARIABLE pcFullPath     AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE pcJsonRespons  AS LONGCHAR   NO-UNDO.
    DEFINE VARIABLE pcResponsType  AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE pcStatusReason AS CHARACTER  NO-UNDO.
    
    DEFINE VARIABLE bResult        AS LOG        NO-UNDO.
    DEFINE VARIABLE poJsonObject   AS JsonObject NO-UNDO.

    FIND FIRST ttMedlem.

    IF AVAILABLE ttMedlem THEN 
    DO:    
      /* Er token fortsatt gyldig, hentes ikke nytt token */
      IF NOT rtokenDintero:sjekkSysParaToken() THEN 
      DO:
        IF NOT rtokenDintero:getToken(OUTPUT iStatusCode, OUTPUT pcStatusReason )THEN 
        DO:
          ASSIGN 
            bResult = FALSE
            .
          RETURN bResult.
        END.
      END.
      
      IF bTest THEN
      DO: 
        rStandardFunksjoner:SkrivTilLogg(cLogg,
          '  UpdateCustomer: mottatt medlem som skal oppdateres ' + STRING(ttMedlem.MedlemsNr) 
          ).    
      END.
      /* Konverterer datasett til JSon og sender det nye medlemmet. */
      IF datasetMedlemToJSon ( INPUT DATASET dsMedlem, OUTPUT poJSonObject) THEN
      SENDOBJECT: 
      DO:
        rStandardFunksjoner:SkrivTilLogg(cLogg,
          '    UpdateCustomer: JSon melding oppdater ' + STRING(ttMedlem.MedlemsNr) 
          ).    

        /* Skriver respons til fil for debug. */
        IF bTest THEN
          CAST(poJSonObject, JSONObject):WriteFile('konv\UpdateCustomerPayload' + STRING(ETIME) + '.json').

        ASSIGN 
          pcFullPath = 'https://api.dintero.com/v1/accounts/' + rTokenDintero:cAid + '/customers/users/' + ttMedlem.EksterntMedlemsNr
      
          oURL       = URI:Parse(pcFullPath, FALSE).
      
        /* Creating a Client. */
        oClient = ClientBuilder:Build()
                    :KeepCookies(CookieJarBuilder:Build()
                    :CookieJar):Client
          .
        oRequest = RequestBuilder:Put(oURL, poJSonObject) /* URL */
                          :AddHeader('Authorization', rtokenDintero:cToken_Type + ' ' + rtokenDintero:cAccess_Token) /* cToken_Type = 'Bearer' */
                          :AcceptJson() /* we want to get JSON back */
                          :Request.
        /* Bygger respons objekstet. Det er tomt her, men fylles når forespørselen eksekverees. */
        oResponse = ResponseBuilder:Build():Response.
        /* Execute the request.                                                              */
        /* Sender forespørselen til API'et med preppet request object og tom respons object. */
        oClient:Execute(oRequest, oResponse).   
        oEntity = oResponse:Entity.
        ASSIGN 
          iStatusCode   = oResponse:StatusCode
          cStatusReason = oResponse:StatusReason
          .
        DO:      
          /* Her er det oEntity objectet som behandles. */
          castObject(INPUT 'UpdateCustomerResponse', OUTPUT pcJsonRespons, OUTPUT pcResponsType).
        END.       

      /* Opprydding */
      /*          DELETE OBJECT poJSonObject.*/
          
      END. /* SENDOBJECT */
      ELSE 
      DO:
        rStandardFunksjoner:SkrivTilLogg(cLogg,
          '  UpdateCustomer: Feilet ved konvertering til JSon.' 
          ).    
      END.
      bResult = TRUE.
    END.
    ELSE 
    DO:
      rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  UpdateCustomer: mottatt tomt datasett' 
        ).    
    END.
    
    RETURN bResult.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC VOID DeleteCustomer(  ):
    
    RETURN.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC VOID ChangeCustomer_id(  ):
    
    RETURN.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC VOID emptyMedlem(  ):

    DATASET dsMedlem:EMPTY-DATASET ().
    
    /*    EMPTY TEMP-TABLE ttMedlem.       */
    /*    EMPTY TEMP-TABLE ttMedlemskort.  */
    /*    EMPTY TEMP-TABLE ttMedlemsaldo.  */
    /*    EMPTY TEMP-TABLE ttMedlemsgruppe.*/
    /*    EMPTY TEMP-TABLE ttMedlemsklubb. */
    /*    EMPTY TEMP-TABLE ttMedlemstype.  */
    
    RETURN.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:

  ------------------------------------------------------------------------------*/

  METHOD PUBLIC LOGICAL datasetMedlemToJSon( INPUT DATASET dsMedlem, OUTPUT poCustCustomer AS JSonObject):
    
    DEFINE VARIABLE bResult                 AS LOGICAL    NO-UNDO.
    DEFINE VARIABLE pcMobilNr               AS CHARACTER  NO-UNDO.
  
    DEFINE VARIABLE poJSonObject            AS JsonObject NO-UNDO. 
    /*    DEFINE VARIABLE poCustCustomer    AS JsonObject NO-UNDO.*/
    DEFINE VARIABLE poCustEnrolled_by       AS JsonObject NO-UNDO.
    DEFINE VARIABLE poCustMarketing_consent AS JsonObject NO-UNDO.
    DEFINE VARIABLE poCustEMail             AS JsonObject NO-UNDO.
    DEFINE VARIABLE poCustSMS               AS JsonObject NO-UNDO.
    DEFINE VARIABLE poCustMetadata          AS JsonObject NO-UNDO.
    DEFINE VARIABLE poCustAddresses         AS JsonObject NO-UNDO. 
    DEFINE VARIABLE poCustAdressArray       AS JsonArray  NO-UNDO.

    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '    datasetMedlemToJSon:'  
      ).    
    
    /* Klargjør for bygging av nytt object. */
    EMPTY TEMP-TABLE ttCustCustomer.
    EMPTY TEMP-TABLE ttCustMetadata.
    EMPTY TEMP-TABLE ttCustAttributes.
    EMPTY TEMP-TABLE ttCustEnrolled_by.
    EMPTY TEMP-TABLE ttCustMarketing_consent.
    EMPTY TEMP-TABLE ttCustSms.
    EMPTY TEMP-TABLE ttCustEMail.
    EMPTY TEMP-TABLE ttCustAddresses.
    EMPTY TEMP-TABLE ttCustTerm.
    EMPTY TEMP-TABLE ttCustCompany.
    
    FIND FIRST ttMedlem NO-ERROR.
    IF AVAILABLE ttMEdlem THEN 
    MEDLEMKONV:
    DO:
      DATASET dsCustCustomer:EMPTY-DATASET ().

      pcMobilNr = ttMedlem.MobilTlf.
      IF pcMobilNr <> '' THEN 
        prepMobilNr( 2, INPUT-OUTPUT pcMobilNr ).
      
      /* 1. Bygger opp datasettet. */
      /*------------------------------*/
      CREATE ttCustCustomer.
      ASSIGN 
        ttCustCustomer.customer_id    = IF ttMedlem.EksterntMedlemsNr <> '' THEN 
                                       STRING(ttMedlem.EksterntMedlemsNr)
                                     ELSE /* Nytt medlem får midlertidig nummer. */ 
                                       STRING(ttMedlem.MedlemsNr)
        ttCustCustomer.custType       = 'customer'
        ttCustCustomer.gender         = IF ttMedlem.Kjonn = TRUE THEN 'Mann' ELSE IF ttMedlem.Kjonn = FALSE THEN 'Kvinne' ELSE ''
        ttCustCustomer.date_of_birth  = STRING(DATE(YEAR(ttMedlem.FodselsDato))) + '-' +
                                       STRING(MONTH(ttMedlem.FodselsDato)) + '-' +
                                       STRING(DAY(ttMedlem.FodselsDato))
        /*        ttCustCustomer.date_of_birth = (IF ttCustCustomer.date_of_birth = ? THEN '' ELSE ttCustCustomer.date_of_birth)*/
        ttCustCustomer.first_name     = ttMedlem.forNavn
        ttCustCustomer.last_name      = ttMedlem.EtterNavn
        ttCustCustomer.email          = ttMedlem.ePostAdresse
        ttCustCustomer.phone_number   = pcMobilNr
        ttCustCustomer.custStatus     = 'Aktiv'
        ttCustCustomer.favorite_store = STRING(ttMedlem.ButikkNr)
        .
      CREATE ttCustEnrolled_by.
      ASSIGN
        ttCustEnrolled_by.customer_id = ttCustCustomer.customer_id 
        ttCustEnrolled_by.enrType     = 'store'
        ttCustEnrolled_by.enrValue    = STRING(ttMedlem.ButikkNr) 
        .
      CREATE ttCustMetadata.
      ASSIGN 
        ttCustMetadata.customer_id = ttCustCustomer.customer_id
        ttCustMetadata.dob_year    = YEAR(ttMedlem.FodselsDato) 
        .
      FIND Post OF ttMedlem NO-LOCK NO-ERROR.
      CREATE ttCustAddresses.
      ASSIGN 
        ttCustAddresses.customer_id    = ttCustCustomer.customer_id
        ttCustAddresses.addrType       = 'home'
        ttCustAddresses.address_line   = ttMedlem.Adresse1
        ttCustAddresses.address_line_2 = ttMedlem.Adresse2
        ttCustAddresses.postal_code    = STRING(ttMedlem.PostNr)
        ttCustAddresses.postal_place   = IF AVAILABLE Post THEN Post.Beskrivelse ELSE ''
        ttCustAddresses.country        = 'NOR'
        ttCustAddresses.custom_type    = ''
        ttCustAddresses.comment        = ''
        .
      CREATE ttCustMarketing_consent.
      ASSIGN 
        ttCustMarketing_consent.customer_id = ttCustCustomer.customer_id
        ttCustMarketing_consent.consent_id  = '1'
        .
      CREATE ttCustEMail.
      ASSIGN
        ttCustEMail.customer_id = ttCustCustomer.customer_id
        ttCustEMail.consent_id  = '1'
        ttCustEMail.consent     = TRUE 
        .
      
      /* Create new JsonObjects */
      poJSonObject            = NEW JsonObject().
      poCustCustomer          = NEW JsonObject().
      poCustEnrolled_by       = NEW JsonObject().
      poCustMarketing_consent = NEW JSonObject().
      poCustSMS               = NEW JSonObject().
      poCustEMail             = NEW JSonObject().
      poCustMetadata          = NEW JSonObject().
      poCustAddresses         = NEW JSonObject().
        
      poCustAdressArray        = NEW JsonArray().
  
      IF AVAILABLE ttCustCustomer THEN
      DO: 
        poCustCustomer:ADD("customer_id", ttCustCustomer.customer_id).
        poCustCustomer:ADD("first_name", ttCustCustomer.first_name).
        poCustCustomer:ADD("last_name", ttCustCustomer.last_name).
        IF ttCustCustomer.email <> '' THEN poCustCustomer:ADD("email", ttCustCustomer.email).
        IF ttCustCustomer.phone_number > '' THEN poCustCustomer:ADD("phone_number", ttCustCustomer.phone_number).
        IF ttCustCustomer.custStatus > '' THEN poCustCustomer:ADD("custStatus", ttCustCustomer.custStatus).
        IF ttCustCustomer.favorite_store <> '' THEN poCustCustomer:ADD("favorite_store", ttCustCustomer.favorite_store).
        IF ttCustCustomer.custType <> '' THEN poCustCustomer:ADD("type", ttCustCustomer.custType).
        IF ttCustCustomer.gender > '' THEN poCustCustomer:ADD("gender", ttCustCustomer.gender).
        IF ttCustCustomer.date_of_birth <> '' THEN poCustCustomer:ADD("date_of_birth", ttCustCustomer.date_of_birth).
      END.  
      IF AVAILABLE ttCustEnrolled_by THEN 
      DO:
        poCustEnrolled_by:Add("type", ttCustEnrolled_by.enrType).
        poCustEnrolled_by:Add("value", ttCustEnrolled_by.enrValue).
      END.
      IF AVAILABLE ttCustMarketing_consent THEN 
      DO:
        IF AVAILABLE ttCustSMS THEN 
        DO:
          poCustSMS:Add("consent", ttCustSMS.consent).
          poCustMarketing_consent:Add("sms", poCustSMS).
        END.    
        IF AVAILABLE ttCustEMail THEN 
        DO:
          poCustEMail:Add("consent", ttCustEMail.consent).
          poCustMarketing_consent:Add("email", poCustEMail).
        END.    
      END.
      IF AVAILABLE ttCustMetadata THEN 
      DO:
        poCustMetadata:Add("dob_year", ttCustMetadata.dob_year).
      END.
      IF CAN-FIND(FIRST ttCustAddresses) THEN 
      DO:
        poCustAdressArray:READ(TEMP-TABLE ttCustAddresses:HANDLE).
      END.
        
      /* Setter sammen objectet. */
      /*      poJSonObject:Add("customer", poCustCustomer).*/
      poCustCustomer:Add("enrolled_by", poCustEnrolled_by).
      poCustCustomer:Add("marketing_consent", poCustMarketing_consent).
      poCustCustomer:Add("metadata", poCustMetadata).
      poCustCustomer:Add("addresses", poCustAdressArray).
    END. /* MEDLEMKONV */

    /* Skriver ogjectet til fil. */
    /*    CAST(poCustCustomer, JSONObject):WriteFile('konv\poCustCustomer' + STRING(ETIME) + '.json') NO-ERROR.*/
    /* skriver objectet til longchar. */
    /*    CAST(poCustCustomer, JSONObject):Write(pcMedlemJSon).*/

    /* Rydder opp */
    /*    DATASET dsCustCustomer:EMPTY-DATASET ().*/
    /* TN 15/11-20 Dette kan ikke gjøres. Da feiler bruk av det returnerte objectet i kallende rutine. */
    /*    DELETE OBJECT poJSonObject.*/
    /*    DELETE OBJECT poCustCustomer.*/
    /*    DELETE OBJECT poCustEnrolled_by.      */
    /*    DELETE OBJECT poCustMarketing_consent.*/
    /*    DELETE OBJECT poCustSMS.              */
    /*    DELETE OBJECT poCustEMail.            */
    /*    DELETE OBJECT poCustMetadata.         */
    /*    DELETE OBJECT poCustAddresses.        */
    /*    DELETE OBJECT poCustAdressArray.      */

    /* Satser på at dette gikk bra og returnerer. */
    bResult = TRUE.
    RETURN bResult.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC VOID castObject( INPUT  pcFilName AS CHARACTER, 
    OUTPUT pcJSonString AS LONGCHAR,
    OUTPUT pcType AS CHARACTER ):

    DEFINE VARIABLE pcJSonFileName AS CHARACTER NO-UNDO.
    
    pcJSonFileName = REPLACE('konv/CAST&FileName','&FileName', pcFilName) + STRING(ETIME) + '.&Ekst' NO-ERROR.

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
    
    RETURN.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose: Medlem er opprettet hos dintero. I responsen ligger customer_Id som 
            nå skal settes inn på medlemmet.
            NB: DB Trigger skal overstyres her, slik at det ikke sendes en 
                ny oppdatering til Dintero.
   Notes: JSon respons etter opprettelse av medlem. 
   ------------------------------------------------
    {
        "id": "f4e5ffa0-e835-4d82-a3fe-671307a68f53",
        "customer_id": "f4e5ffa0-e835-4d82-a3fe-671307a68f53",
        "first_name": "Gunn",
        "last_name": "Rita B Strømnes",
        "email": "grb@basutleie.no",
        "favorite_store": "15",
        "enrolled_by": {
            "type": "store",
            "value": "15"
        },
        "addresses": [
            {
                "type": "home",
                "comment": "",
                "country": "NOR",
                "latitude": 0,
                "longitude": 0,
                "custom_type": "",
                "postal_code": "2406",
                "address_line": "Ilderveien 29",
                "postal_place": "ELVERUM",
                "address_line_2": " "
            }
        ],
        "phone_number": "93293320",
        "gender": "Mann",
        "marketing_consent": {
            "email": {
                "consent": true,
                "updated_at": "2020-11-08T18:58:25.344Z"
            }
        },
        "created_at": "2020-11-08T18:58:25.348Z",
        "created_by": "c0fab10f-ecc5-4c8d-a152-93fd0ff314ab",
        "updated_at": "2020-11-08T18:58:25.348Z",
        "metadata": {},
        "type": "customer"
    }     
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC LOGICAL setCustomerIdMedlem( INPUT plMedlemsNr AS DECIMAL, 
    INPUT pcJasonResponse AS LONGCHAR, 
    INPUT pcResponstype AS CHARACTER ):
    
    DEFINE VARIABLE bResult          AS LOGICAL           NO-UNDO.
    DEFINE VARIABLE pcCustomer_Id    AS CHARACTER         NO-UNDO.
    DEFINE VARIABLE pc2JasonResponse AS LONGCHAR          NO-UNDO.
    DEFINE VARIABLE pcCode           AS CHARACTER         NO-UNDO.
    DEFINE VARIABLE pcMessage        AS CHARACTER         NO-UNDO.

    DEFINE VARIABLE poNestObject     AS JsonObject        NO-UNDO.
    DEFINE VARIABLE poJsonRespons    AS JsonObject        NO-UNDO.
    DEFINE VARIABLE myParser         AS ObjectModelParser NO-UNDO.    
    
    DEFINE BUFFER bufMedlem FOR Medlem.

    FIX-CODEPAGE(pc2JasonResponse) = 'UTF-8'.

    ASSIGN 
      pc2JasonResponse = pcJasonResponse
      .
    
    poNestObject  = NEW JsonObject().
    poJsonRespons = NEW JsonObject().
    myParser      = NEW ObjectModelParser().
    
    /* Ikke vellykket */
    IF pc2JasonResponse MATCHES '*"error":*' THEN
    ERRORBLOKK: 
    DO:
      poJsonRespons = CAST(myParser:Parse(pc2JasonResponse), JsonObject).
      poNestObject = poJsonRespons:GetJsonObject('error').
  
      ASSIGN
        pcMessage = poNestObject:GetJsonText('message')
        .
      rStandardFunksjoner:SkrivTilLogg(cLogg,
        '    CreateNewCustomer: setCustomerIdMedlem: FEIL ved setting av ID på medlem ' + STRING(plMedlemsNr)  + ' ' + pcMessage  
        ).    
    END. /* ERRORBLOKK */            
    ELSE 
    VELLYKKETBLOKK:
    DO:    
      /* Legger på navn på første objektet. Det er ikke satt fra Dintero. */
      ASSIGN 
        pc2JasonResponse = '~{"response":' + pc2JasonResponse + '~}'
        .
      poJsonRespons = CAST(myParser:Parse(pc2JasonResponse), JsonObject).
      poNestObject = poJsonRespons:GetJsonObject('response').
  
      ASSIGN
        pcCustomer_Id = poNestObject:GetJsonText('customer_id')
        .
        
      IF pcCustomer_Id <> '' THEN 
      DO FOR bufMedlem TRANSACTION:
        FIND bufMedlem EXCLUSIVE-LOCK WHERE 
          bufMedlem.MedlemsNr = plMedlemsNr NO-ERROR NO-WAIT.
        IF AVAILABLE bufMedlem THEN 
        DO:
          ASSIGN 
            bufMedlem.EksterntMedlemsNr = pcCustomer_Id
            . 
          rStandardFunksjoner:SkrivTilLogg(cLogg,
            '    CreateNewCustomer: setCustomerIdMedlem: Satt customer_id på medlem ' + STRING(plMedlemsNr) 
            ).    
          RELEASE bufMedlem.
        END.
      END. /* TRANSACTION */
    END. /* VELLYKKETBLOKK */
    
    DELETE OBJECT poNestObject.
    DELETE OBJECT poJsonRespons.
    DELETE OBJECT myParser.            

    bResult = TRUE.
    RETURN bResult.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC VOID prepMobilNr( INPUT piAction AS INTEGER, INPUT-OUTPUT pcMobilNr AS CHARACTER ):
    
    ASSIGN 
      pcMobilNr = REPLACE(pcMobilNr,'0047','+47')
      pcMobilNr = IF NOT pcMobilNr BEGINS '~+47' THEN '+47' + pcMobilNr ELSE pcMobilNr
      .

    IF piAction = 1 THEN 
      ASSIGN 
        pcMobilNr = REPLACE(pcMobilNr,'+','%2B')
        .
    
    RETURN.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC LOGICAL CreateNewReceipts( INPUT DATASET dsBong,
    OUTPUT pcReturn AS CHARACTER  ):
  
    DEFINE VARIABLE pcFullPath     AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE pcJsonRespons  AS LONGCHAR   NO-UNDO.
    DEFINE VARIABLE pcResponsType  AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE pcStatusReason AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE pcJSonPayLoad  AS LONGCHAR   NO-UNDO.

    DEFINE VARIABLE bResult        AS LOG        NO-UNDO.
    DEFINE VARIABLE poJsonArray    AS JsonArray  NO-UNDO.
    DEFINE VARIABLE poJsonObject   AS JsonObject NO-UNDO.
    DEFINE VARIABLE poEntity       AS OBJECT     NO-UNDO.

    DEFINE BUFFER bufBongCRMLogg FOR BongCRMLogg.


    FIND FIRST ttBongHode.

    IF AVAILABLE ttBongHode THEN
    DO:
      /* Er token fortsatt gyldig, hentes ikke nytt token */
      IF NOT rtokenDintero:sjekkSysParaToken() THEN
      DO:
        IF NOT rtokenDintero:getToken(OUTPUT iStatusCode, OUTPUT pcStatusReason )THEN
        DO:
          ASSIGN
            bResult = FALSE
            .
          RETURN bResult.
        END.
      END.

      IF bTest THEN
      DO:
        rStandardFunksjoner:SkrivTilLogg(cLogg,
          '  CreateNewReceipts: mottatt bong som skal opprettes ' + STRING(ttBongHode.B_Id)
          ).

        FIND FIRST ttBongHode NO-LOCK.
        IF bTest THEN 
          DATASET dsBong:WRITE-JSON ('file', 'konv\Bong' + STRING(ttBongHode.B_Id) + '.json', TRUE).
      END.
      /* Konverterer datasett til JSon og sender den nye bongen. */
      IF datasetBongToJSon ( INPUT DATASET dsBong, OUTPUT poJSonArray) THEN
      SENDOBJECT:
      DO:
        rStandardFunksjoner:SkrivTilLogg(cLogg,
          '    CreateNewReceipts: JSon melding opprettet ' + STRING(ttBongHode.B_Id)
          ).

        /* Skriver ferdig bygget receipts til fil for debug. */
        IF bTest THEN
          CAST(poJSonArray, JsonArray):WriteFile('konv\CreateNewReceiptsPayload' + STRING(ETIME) + '.json').

        ASSIGN
          pcFullPath = 'https://api.dintero.com/v1/accounts/' + rTokenDintero:cAid + '/receipts'.

        oURL = URI:Parse(pcFullPath, FALSE).
        poEntity = poJSonArray.

        /* Creating a Client. */
        oClient = ClientBuilder:Build()
                    :KeepCookies(CookieJarBuilder:Build()
                    :CookieJar):Client
          .
        oRequest = RequestBuilder:Post(oURL, poJSonArray) /* URL */
                          :AddHeader('Authorization', rtokenDintero:cToken_Type + ' ' + rtokenDintero:cAccess_Token) /* cToken_Type = 'Bearer' */
                          :AcceptJson() /* we want to get JSON back */
                          :Request.
        /* Bygger respons objekstet. Det er tomt her, men fylles når forespørselen eksekverees. */
        oResponse = ResponseBuilder:Build():Response.
        /* Execute the request.                                                              */
        /* Sender forespørselen til API'et med preppet request object og tom respons object. */
        oClient:Execute(oRequest, oResponse).
        oEntity = oResponse:Entity.
        ASSIGN
          iStatusCode   = oResponse:StatusCode
          cStatusReason = oResponse:StatusReason
          pcReturn      = STRING(oResponse:StatusCode) + ' ' + oResponse:StatusReason
          .
            
        /* Her er det oEntity objectet som behandles og leges i en longchar. */
        IF bTest THEN 
          castObject(INPUT 'CreateNewReceiptsResponse', OUTPUT pcJsonRespons, OUTPUT pcResponsType).
        
        IF NOT pcJsonRespons MATCHES '*duplicate*' THEN 
        /* Logger at bongen er sendt */
        SKRIVLOGG:
        DO FOR bufBongCRMLogg TRANSACTION:
          FIND FIRST bufBongCRMLogg EXCLUSIVE-LOCK WHERE
            bufBongCRMLogg.B_Id = ttBongHode.B_Id NO-ERROR.
          IF NOT AVAILABLE bufBongCRMLogg THEN  
            FIND FIRST bufBongCRMLogg EXCLUSIVE-LOCK WHERE
              bufBongCRMLogg.ButikkNr = ttBongHode.butikkNr AND
              bufBongCRMLogg.GruppeNr = ttBongHode.GruppeNr AND
              bufBongCRMLogg.KasseNr = ttBongHode.KasseNr AND
              bufBongCRMLogg.Dato = ttBongHode.Dato AND
              bufBongCRMLogg.BongNr = ttBongHode.BongNr NO-ERROR.
          IF NOT AVAILABLE bufBongCRMLogg THEN
          DO:
            CREATE bufBongCRMLogg.
            ASSIGN
              bufBongCRMLogg.B_Id              = ttBongHode.B_Id
              bufBongCRMLogg.ButikkNr          = ttBongHode.butikkNr
              bufBongCRMLogg.GruppeNr          = ttBongHode.GruppeNr
              bufBongCRMLogg.KasseNr           = ttBongHode.KasseNr
              bufBongCRMLogg.Dato              = ttBongHode.Dato
              bufBongCRMLogg.BongNr            = ttBongHode.BongNr
              bufBongCRMLogg.RegistrertDatoTid = NOW
              .
          END.
          IF AVAILABLE bufBongCRMLogg AND NOT LOCKED bufBongCRMLogg THEN
          DO:
            ASSIGN
              bufBongCRMLogg.SendtCRM = NOW
              bufBongCRMLogg.B_Id     = ttBongHode.B_Id
              .
            /* Er ikke dete lagt inn før, legges det inn her. */
            IF STRING(bufBongCRMLogg.JsonPayLoad) = '' OR
              STRING(bufBongCRMLogg.JsonPayLoad) = ? THEN
            DO:
              CAST(poJSonArray, JsonArray):Write(pcJSonPayLoad) NO-ERROR.
              bufBongCRMLogg.JsonPayLoad = pcJSonPayLoad.
            END.
          END.
          IF AVAILABLE bufBongCRMLogg THEN
            RELEASE bufBongCRMLogg.
        END. /* SKRIVLOGG */
        
        bResult = TRUE.
      END. /* SENDOBJECT */
      ELSE 
      DO:
        rStandardFunksjoner:SkrivTilLogg(cLogg,
          '  CreateNewReceipts: Feilet ved konvertering til JSon.'
          ).
        bResult = FALSE.
      END.

    END. /* ttBongHode */
    ELSE 
    DO:
      rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  CreateNewReceipts: mottatt tomt datasett'
        ).
    END.

    RETURN bResult.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose: Klargjør JSon melding som skal legges inn i logg for senere å
            overføres til Dintero når hele bongen er klar og lest inn i
            BongHode/BongLinje.
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC LOGICAL datasetBongToJSon( INPUT DATASET dsBong, OUTPUT poReceipArray AS JSonArray ):

    DEFINE VARIABLE pcJSonPayLoad AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE bResult       AS LOGICAL  NO-UNDO.

    DEFINE BUFFER bufBongCRMLogg FOR BongCRMLogg.
    DEFINE BUFFER bufButiker     FOR Butiker.

    /* Sikrer at datasettet er tomt før det fylles opp. */
    DATASET dsCustCustomer:EMPTY-DATASET ().
    DATASET dsReceipts:EMPTY-DATASET ().

    /* Setter riktig codepage. */
    FIX-CODEPAGE(pcJSonPayLoad) = 'UTF-8'.

    FIND FIRST ttBongHode NO-ERROR.
    IF AVAILABLE ttBongHode THEN
    BONGKONV:
    DO:
      /* Sjekker butikk */
      FIND bufButiker NO-LOCK WHERE
        bufButiker.Butik = ttBongHode.butikkNr NO-ERROR.
      IF NOT AVAILABLE bufButiker THEN
        LEAVE BONGKONV.

      /* Ligger bongen i bongCRMLoggen, hentes data ut derfra. Den har alltid et bilde av bongen slik */
      /* den så ut før betaling. Bongen ligger da i loggen uten b_id.                                 */
      FIND FIRST bufBongCRMLogg WHERE 
        bufBongCRMLogg.B_Id = ttBongHode.b_id NO-ERROR.
      IF NOT AVAILABLE bufBongCRMLogg THEN 
        FIND FIRST bufBongCRMLogg NO-LOCK WHERE
          bufBongCRMLogg.ButikkNr = ttBongHode.butikkNr AND
          bufBongCRMLogg.GruppeNr = ttBongHode.GruppeNr AND
          bufBongCRMLogg.KasseNr  = ttBongHode.KasseNr AND
          bufBongCRMLogg.Dato     = ttBongHode.Dato AND
          bufBongCRMLogg.BongNr   = ttBongHode.BongNr NO-ERROR.
      /* Bongen er logget. dsReceipt bygges fra JSon meldingen som ligger der. */
      IF AVAILABLE bufBongCRMLogg THEN
      DO:
        /* Henter inn respons fra dintero's bongkalkulasjon */
        pcJSonPayLoad = bufBongCRMLogg.JSonPayLoad.
        /* Bygger første del...                             */
        /* Bygger opp dsReceipt fra bongen som skal sendes. */
        RUN cls\dintero\byggReceiptFraBong.p (INPUT DATASET dsBong, 
          INPUT-OUTPUT DATASET dsReceipts BY-REFERENCE,
          INPUT-OUTPUT DATASET dsCustCustomer BY-REFERENCE).
        /* Supplere dsReceipts datasettet med mva og betalingsinfo fra dsBong. */
        RUN cls\dintero\supplerReceiptFraBong.p ( DATASET dsBong, 
          INPUT-OUTPUT DATASET dsReceipts BY-REFERENCE,
          INPUT-OUTPUT DATASET dsCustCustomer BY-REFERENCE).
        /* Legger på rabattlinjer i receipts. Hentet fra Dintero's respons på bongkalkulasjon.   */
        /* Den responsen ligger i BongCRMLogg.cPayload.                                          */
        /* NB: Rabatt på bong og rabatt på linjene, legges på som longchar på respektive record. */
        /*     Feltene is_changed og net_amount oppdateres også fra Dintero's respons.           */
        IF pcJSonPayLoad > '' THEN  
          RUN cls\dintero\supplerReceiptFraJson.p ( pcJSonPayLoad, 
            INPUT-OUTPUT DATASET dsReceipts BY-REFERENCE).
      END.
      ELSE
      /* Hele datasettet må bygges fra dsBong. Nå skal det ikke lages Discount objekter. */
      DO:
        /* Bygger første del... */
        RUN cls\dintero\byggReceiptFraBong.p (INPUT DATASET dsBong, 
          INPUT-OUTPUT DATASET dsReceipts BY-REFERENCE,
          INPUT-OUTPUT DATASET dsCustCustomer BY-REFERENCE).
        /* Supplere datasettet med mva og betalingsinfo fra dsBong. */
        RUN cls\dintero\supplerReceiptFraBong.p ( DATASET dsBong, 
          INPUT-OUTPUT DATASET dsReceipts BY-REFERENCE,
          INPUT-OUTPUT DATASET dsCustCustomer BY-REFERENCE).
      END.

      /* Setter sammen dsReceipt, dsCustomer og supplerer med rabatter hentet fra JsonMelding i */
      /* BongCRMLogg.cPayLoad. til en komplett JSon melding for Bong. Denne legges så inn i     */
      /* en JSonArray som leveres til kallende rutine via poReceipArray.                        */
      /* BongCRMLogg.cPayload inneholder respons fra rekalkulkulering av bong gjort tidligere.  */
      RUN cls\dintero\receiptsToJson.p ( DATASET dsReceipts  BY-REFERENCE
        , DATASET dsCustCustomer BY-REFERENCE
        , OUTPUT poReceipArray ).
      bResult = TRUE.
    END. /* BONGKONV */

    RETURN bResult.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC LOGICAL PostReceiptForDiscount( INPUT-OUTPUT DATASET dsPOSBong,
    OUTPUT pcReturn AS CHARACTER ):
    
    DEFINE VARIABLE pcFullPath     AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE pcJsonRespons  AS LONGCHAR   NO-UNDO.
    DEFINE VARIABLE pcResponsType  AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE pcStatusReason AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE pcJSonPayLoad  AS LONGCHAR   NO-UNDO.
    
    DEFINE VARIABLE bResult        AS LOG        NO-UNDO.
    DEFINE VARIABLE poJsonArray    AS JsonArray  NO-UNDO.
    DEFINE VARIABLE poJsonObject   AS JsonObject NO-UNDO.
    DEFINE VARIABLE poJsonTest     AS JsonObject NO-UNDO.
    DEFINE VARIABLE pbOk           AS LOG        NO-UNDO.

    FIND FIRST ttPOSBongHode.

    IF AVAILABLE ttPOSBongHode THEN 
    DO:    
      /* Er token fortsatt gyldig, hentes ikke nytt token */
      IF NOT rtokenDintero:sjekkSysParaToken() THEN 
      DO:
        IF NOT rtokenDintero:getToken(OUTPUT iStatusCode, OUTPUT pcStatusReason )THEN 
        DO:
          ASSIGN 
            bResult = FALSE
            .
          RETURN bResult.
        END.
      END.

      IF bTest THEN
      DO: 
        rStandardFunksjoner:SkrivTilLogg(cLogg,
          '  PostReceiptForDiscount: mottatt bong som skal opprettes ' + STRING(ttPOSBongHode.B_Id) 
          ).    
      END.
      /* Konverterer datasett til JSon og sender den nye bongen. */
      IF datasetPOSBongToJSon ( INPUT DATASET dsPOSBong, OUTPUT poJsonObject) THEN
      SENDOBJECT: 
      DO:
        rStandardFunksjoner:SkrivTilLogg(cLogg,
          '    PostReceiptForDiscount: JSon melding opprettet ' + STRING(ttPOSBongHode.B_Id) 
          ).    

        /* Skriver respons til fil for debug. */
        IF bTest THEN
          CAST(poJsonObject, JsonObject):WriteFile('konv\PostReceiptForDiscount' + STRING(ETIME) + '.json').

        ASSIGN 
          pcFullPath = 'https://api.dintero.com/v1/accounts/' + rTokenDintero:cAid + '/discounts/available_for_receipt'.
      
         rStandardFunksjoner:SkrivTilLogg(cLogg,
           '  pcFullPath: ' + pcFullPath 
           ).
      
        oURL = URI:Parse(pcFullPath, FALSE).
      
        oRequest = RequestBuilder:Post(oURL, poJsonObject) /* URL */
                          :AddHeader('Authorization', rtokenDintero:cToken_Type + ' ' + rtokenDintero:cAccess_Token) /* cToken_Type = 'Bearer' */
                          :AcceptJson() /* we want to get JSON back */
                          :Request.
        /* Bygger respons objektet. Det er tomt her, men fylles når forespørselen eksekverees. */
        oResponse = ResponseBuilder:Build():Response.

        /* Creating a Client. */
        oClient = ClientBuilder:Build()
                    :KeepCookies(CookieJarBuilder:Build()
                    :CookieJar):Client
          .

        /* Execute the request.                                                              */
        /* Sender forespørselen til API'et med preppet request object og tom respons object. */
        oClient:Execute(oRequest, oResponse).   
        oEntity = oResponse:Entity.
        ASSIGN 
          iStatusCode   = oResponse:StatusCode
          cStatusReason = oResponse:StatusReason
          .
        IF iStatusCode = 200 THEN   
        RESPONSBLOKK:
        DO:      
          /* Her er det oEntity objectet som behandles og legges i en longchar. */
          castObject(INPUT 'PostReceiptForDiscountResponse', OUTPUT pcJsonRespons, OUTPUT pcResponsType).
            
          /* Lagrer respons i bongloggen. */
          lagreBongKalkulasjon (INPUT DATASET dsPOSBong, pcJsonRespons).
            
          DATASET dsReceipts:EMPTY-DATASET ().
          /* Bygger opp datasettet dsReceipts. */
          RUN cls\dintero\receiptToDataset.p (pcJsonRespons, OUTPUT DATASET dsReceipts).
          FIND FIRST ttReceipts NO-ERROR.
          IF bTest THEN
          DO: 
            rStandardFunksjoner:SkrivTilLogg(cLogg,
              '  PostReceiptForDiscount: receipt available ' + STRING(AVAILABLE ttReceipts) + 
              ' Is-Changed: ' + (IF AVAILABLE ttReceipts THEN STRING(ttReceipts.is_changed) ELSE '') 
              ).    
          END.
          IF AVAILABLE ttReceipts AND 
            ttReceipts.is_changed = TRUE THEN 
          REKALKULER_BONG:
          DO:
            IF bTest THEN
            DO: 
              rStandardFunksjoner:SkrivTilLogg(cLogg,
                '  PostReceiptForDiscount: receipt available ' + STRING(AVAILABLE ttReceipts) + 
                ' REKALKULER_BONG ' 
                ).    
            END.
            pbOk = FALSE.
            /* sjekker rabatt totaler, rekalkulere hvis nødvendig og sender korrigert bong til kassen. */
            /* Returnerer bare FALSE hvis det feilet med rekalkulering. Den er ellers TRUE selv om det */
            /* ikke er gitt rabatt fra Dintero.                                                        */
            RUN cls\dintero\rekalkulerBong.p (INPUT-OUTPUT DATASET dsPOSBong BY-REFERENCE, INPUT-OUTPUT DATASET dsReceipts BY-REFERENCE, OUTPUT bResult).

            /* Legge på spesifikasjons linjer i Bong ut fra Discount recordene. */                
/*            adderDiscountLinesBong().*/
                
            /* Tilgjengeligjør for videre behandling. */
            FIND FIRST ttPOSBongHode NO-ERROR.
          END. /* REKALKULER_BONG */
          ELSE 
          INGEN_REKALKULERING: DO:
            bResult = TRUE.
          END. /* INGEN_REKALKULERING */
        END. /* RESPONSBLOKK */       
        ELSE 
        DO:
          /* Her er det oEntity objectet som behandles og legges i en longchar. */
          castObject(INPUT 'PostReceiptForDiscountResponse', OUTPUT pcJsonRespons, OUTPUT pcResponsType).
          bResult = FALSE.
          pcReturn = STRING(iStatusCode) + ' ' + cStatusReason.
        END.
      END. /* SENDOBJECT */
      ELSE 
      DO:
        rStandardFunksjoner:SkrivTilLogg(cLogg,
          '  PostReceiptForDiscount: Feilet ved konvertering til JSon.' 
          ).    
      END.      
    END. /* ttPOSBongHode */
    ELSE 
    DO:
      rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  PostReceiptForDiscount: mottatt tomt datasett' 
        ).    
    END.
    
    RETURN bResult.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose: Klargjør bongen for rekalkulering hos Dintero. Bongen har her ikke 
            fått påført betalingslinjer. Bare varelinjer.
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC LOGICAL datasetPOSBongToJSon( INPUT DATASET dsPOSBong, OUTPUT poReceipt AS JSonObject  ):
    
    DEFINE VARIABLE bResult              AS LOGICAL    NO-UNDO.
    DEFINE VARIABLE plMvaKr              AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE piAntLinjer          AS INTEGER    NO-UNDO.
    DEFINE VARIABLE plRabKr              AS DECIMAL    NO-UNDO.
    
    DEFINE VARIABLE poJsonObject         AS JsonObject NO-UNDO.
    DEFINE VARIABLE poStore              AS JsonObject NO-UNDO.
    DEFINE VARIABLE pcJSonString         AS LONGCHAR   NO-UNDO.
    DEFINE VARIABLE poItemArray          AS JsonArray  NO-UNDO.
    DEFINE VARIABLE poPaymentArray       AS JsonArray  NO-UNDO.
    DEFINE VARIABLE poItem               AS JsonObject NO-UNDO.
    DEFINE VARIABLE poItemGroupArray     AS JsonArray  NO-UNDO.
    DEFINE VARIABLE poItemDimension      AS JsonObject NO-UNDO.
    DEFINE VARIABLE poCustCustomer       AS JSonObject NO-UNDO.
    DEFINE VARIABLE poCustAddresses      AS JsonObject NO-UNDO.
    DEFINE VARIABLE poCustAddressesArray AS JsonArray  NO-UNDO.
    DEFINE VARIABLE lBelop AS DECIMAL NO-UNDO.
    
    DEFINE BUFFER bufButiker FOR Butiker.

    DATASET dsReceipts:EMPTY-DATASET ().
    FIX-CODEPAGE(pcJSonString) = 'UTF-8'.
     
    FIND FIRST ttPOSBongHode NO-ERROR.
    IF AVAILABLE ttPOSBongHode THEN 
    BONGKONV: 
    DO:
      FIND bufButiker NO-LOCK WHERE 
        bufButiker.Butik = ttPOSBongHode.butikkNr NO-ERROR.
      IF NOT AVAILABLE bufButiker THEN 
        LEAVE BONGKONV.
         
      /* Summerer MVA for å finne nettobeløpet. */
      plMvaKr = 0.
      piAntLinjer = 0.
      plRabKr = 0.
      FOR EACH ttPOSBongLinje WHERE 
        ttPOSBongLinje.B_Id = ttPOSBongHode.B_Id:
          
        plMvaKr = plMvaKr + (ttPOSBongLinje.MvaKr).        
        plRabKr = plRabKr + (ttPOSBongLinje.LinjeRab + ttPOSBongLinje.SubtotalRab).
        /* Varelinjer. */
        IF (ttPOSBongLinje.TTId >= 1 AND ttPOSBongLinje.TTId <= 11) THEN 
          piAntLinjer = piantLinjer + 1.
      /* Gavekort ut */
      /* Andre typer varer... */    
      END.

      IF ttPOSBongHode.MedlemsNr > 0 THEN 
        FIND medlem NO-LOCK WHERE 
          Medlem.MedlemsNr = ttPOSBongHode.MedlemsNr NO-ERROR.
      ELSE 
        RELEASE Medlem.
        
      /* Endrer datoformatet her slik at vi får riktig format. Settes tilbake lenger nede. */
      SESSION:DATE-FORMAT = 'ymd'.
      DATOFORMATBLOKK:
      DO:
        ASSIGN 
          lBelop = ttPOSBongHode.Belop
          .  
        
        /* Summerer opp linjene. */  
        IF lBelop = 0 THEN 
          DO:
            FOR EACH ttPOSBongLinje WHERE 
              ttPOSBongLinje.B_Id = ttPOSBongHode.B_Id:
              IF NOT CAN-DO('1,3,10',STRING(ttPOSBongLinje.TTId)) THEN 
                NEXT.
              lBelop = lBelop + ttPOSBongLinje.LinjeSum.
            END.
          END.        
        
        CREATE ttReceipts.
        ASSIGN 
          ttReceipts.receipt_id        = STRING(ttPOSBongHode.b_id)
          ttReceipts.gross_amount      = lBelop * 100 /* Beløpet kunden betaler FLR rabatt */
          ttReceipts.net_amount        = lBelop * 100 /* Beløpet kunden betaler etter rabatt.*/   
          ttReceipts.round_off_to_coin = ttReceipts.gross_amount - ABS(ttReceipts.gross_amount) 
          ttReceipts.currency          = 'NOK'
          ttReceipts.purchase_at       = STRING(DATETIME(ttPOSBongHode.Dato, ttPOSBongHode.Tid * 1000),"9999-99-99THH:MM:SS") + 'Z' 
          ttReceipts.customer_id       = (IF AVAILABLE Medlem THEN Medlem.EksterntMedlemsNr ELSE '') 
          ttReceipts.no_of_items       = piantLinjer
          ttReceipts.total_discount    = plRabKr * 100
          ttReceipts.operator_id       = STRING(ttPOSBongHode.KassererNr)
          ttReceipts.operator_name     = ttPOSBongHode.KassererNavn
          ttReceipts.salesperson_id    = STRING(ttPOSBongHode.SelgerNr)
          ttReceipts.salesperson_name  = ttPOSBongHode.SelgerNavn
          ttReceipts.comment           = ""
          .
          
        IF AVAILABLE Medlem THEN
        DO:
          CREATE ttCustCustomer.
          ASSIGN
            ttCustCustomer.customer_id    = Medlem.EksterntMedlemsNr 
            ttCustCustomer.custType       = IF Medlem.MedType = 3 THEN 'customer' 
                                            ELSE IF Medlem.MedType = 2 THEN 'company'
                                            ELSE 'customer' 
            ttCustCustomer.date_of_birth  = IF Medlem.FodselsDato <> ? THEN 
                                              STRING(Medlem.FodselsDato,"9999-99-99")
                                            ELSE ''
            ttCustCustomer.first_name     = Medlem.ForNavn
            ttCustCustomer.last_name      = Medlem.EtterNavn
            ttCustCustomer.email          = Medlem.ePostAdresse 
            ttCustCustomer.phone_number   = Medlem.MobilTlf
            ttCustCustomer.custStatus     = IF Medlem.Aktiv THEN 'yes' ELSE 'no'
            ttCustCustomer.favorite_store = STRING(Medlem.ButikkNr)
            ttCustCustomer.gender         = IF Medlem.Kjonn THEN 'male' ELSE 'female' 
            .          
          CREATE ttCustAddresses.
          FIND Post NO-LOCK WHERE 
            Post.PostNr = Medlem.PostNr NO-ERROR.
          ASSIGN 
            ttCustAddresses.customer_id    = Medlem.EksterntMedlemsNr
            ttCustAddresses.addrType       = 'customer'
            ttCustAddresses.address_line   = Medlem.Adresse1
            ttCustAddresses.address_line_2 = Medlem.Adresse2
            ttCustAddresses.postal_code    = Medlem.PostNr
            ttCustAddresses.postal_place   = (IF AVAILABLE Post THEN Post.Beskrivelse ELSE '')
            ttCustAddresses.country        = Medlem.Land
            .
        END.  
      END. /* DATOFORMATBLOKK */
      SESSION:DATE-FORMAT = 'dmy'.
      
      CREATE ttStore.
      ASSIGN 
        ttStore.receipt_id = STRING(ttPOSBongHode.B_Id)
        ttStore.id         = STRING(bufButiker.Butik)
        ttStore.cName      = bufbutiker.ButNamn
        .

      /* Create new JsonObjects */
      poJSonObject   = NEW JsonObject().
      poReceipt      = NEW JsonObject().
      poStore        = NEW JsonObject().
      poItemArray    = NEW JsonArray().
      poPaymentArray = NEW JsonArray().
      myParser       = NEW ObjectModelParser().
      
      IF AVAILABLE ttReceipts THEN
      DO: 
        poReceipt:ADD("receipt_id", ttReceipts.receipt_id).
        poReceipt:ADD("gross_amount", ttReceipts.gross_amount).
        poReceipt:ADD("net_amount", ttReceipts.net_amount).
        IF ttReceipts.round_off_to_coin <> 0 THEN 
          poReceipt:ADD("round_off_to_coin", ttReceipts.round_off_to_coin).
        poReceipt:ADD("currency", ttReceipts.currency).
        poReceipt:ADD("purchase_at", ttReceipts.purchase_at).
        IF ttReceipts.no_of_items > 0 THEN 
          poReceipt:ADD("no_of_items", ttReceipts.no_of_items).
        IF ttReceipts.total_discount > 0 THEN 
          poReceipt:ADD("total_discount", ttReceipts.total_discount).
        IF ttReceipts.customer_id > '' THEN  
          poReceipt:ADD("customer_id", ttReceipts.customer_id).
        IF ttReceipts.operator_id > '' THEN 
          poReceipt:ADD("operator_id", ttReceipts.operator_id).
        IF ttReceipts.operator_name > '' THEN 
          poReceipt:ADD("operator_name", ttReceipts.operator_name).
        IF ttReceipts.salesperson_id > '' THEN 
          poReceipt:ADD("salesperson_id", ttReceipts.salesperson_id).
        IF ttReceipts.salesperson_name > '' THEN 
          poReceipt:ADD("salesperson_name", ttReceipts.salesperson_name).
        IF ttReceipts.comment > '' THEN 
          poReceipt:ADD("comment", ttReceipts.comment).
      END.  
      IF AVAILABLE ttStore THEN 
      DO:
        poStore:ADD("id", ttStore.id).
        poStore:ADD("name", ttStore.cName).
      END.

      /* Legger på medlem. */
      IF AVAILABLE ttCustCustomer THEN 
      DO:
        poCustCustomer = NEW JsonObject().
        /* Legger på feltene. */
        poCustCustomer:ADD("type", ttCustCustomer.custType).
        IF (ttCustCustomer.date_of_birth <> '' AND ttCustCustomer.date_of_birth <> ?) THEN 
          poCustCustomer:ADD("date_of_birth", ttCustCustomer.date_of_birth).  
        poCustCustomer:ADD("first_name", ttCustCustomer.first_name).     
        poCustCustomer:ADD("last_name", ttCustCustomer.last_name).      
        poCustCustomer:ADD("email", ttCustCustomer.email).          
        poCustCustomer:ADD("phone_number", ttCustCustomer.phone_number).   
        poCustCustomer:ADD("status", ttCustCustomer.custStatus).     
        poCustCustomer:ADD("favorite_store", ttCustCustomer.favorite_store). 
        poCustCustomer:ADD("gender", ttCustCustomer.gender).         
               
        FIND FIRST ttCustAddresses WHERE 
          ttCustAddresses.customer_id = ttCustCustomer.customer_id NO-ERROR.
        IF AVAILABLE ttCustAddresses THEN 
        DO:
          poCustAddressesArray = NEW JsonArray().
              
          FOR EACH ttCustAddresses WHERE 
            ttCustAddresses.customer_id = ttCustCustomer.customer_id:
            poCustAddresses = NEW JsonObject().
                
            poCustAddresses:ADD("type", ttCustAddresses.addrType).
            poCustAddresses:ADD("address_line", ttCustAddresses.address_line).
            poCustAddresses:ADD("address_line_2", ttCustAddresses.address_line_2).
            poCustAddresses:ADD("postal_code", ttCustAddresses.postal_code).
            poCustAddresses:ADD("postal_place", ttCustAddresses.postal_place).
            poCustAddresses:ADD("country", ttCustAddresses.country).
                
            poCustAddressesArray:ADD(poCustAddresses).
          END.
          poCustCustomer:ADD("adresses",poCustAddressesArray).
        END.
      END.

      /* Legger opp varelinjene. */  
      VARELINJEBLOKK:
      FOR EACH ttPOSBongLinje WHERE 
        ttPOSBongLinje.B_Id = ttPOSBongHode.B_Id AND 
        ttPOSBongLinje.Antall <> 0:

        /* Bare varelinjene har varegruppe påført. */
        IF NOT ttPOSBongLinje.VareGr > 0 THEN
          NEXT VARELINJEBLOKK. 

        FIND ArtBas NO-LOCK WHERE 
          ArtBas.ArtikkelNr = DEC(ttPOSBongLinje.ArtikkelNr) NO-ERROR.

        CREATE ttItem.
        ASSIGN 
          ttItem.receipt_id                 = STRING(ttPOSBongHode.B_Id)
          ttItem.line_id                    = ttPOSBongLinje.LinjeNr
          /*          ttItem.item_id = (IF AVAILABLE ArtBas THEN ArtBas.LevKod ELSE '')*/
          ttItem.item_id                    = (IF AVAILABLE ArtBas THEN (ArtBas.LevKod + ArtBas.LevFargKod) ELSE '')
          ttItem.quantity                   = ttPOSBongLinje.Antall
          ttItem.unit                       = (IF AVAILABLE ArtBas THEN ArtBas.SalgsEnhet ELSE '')
          ttItem.description                = (IF AVAILABLE ArtBAs THEN ArtBas.Beskr ELSE ttPOSBongLinje.BongTekst)
          ttItem.description_alias          = ttPOSBongLinje.BongTekst
          /*          ttItem.net_amount = (ttPOSBongLinje.Linjesum - ttPOSBongLinje.MvaKr) * 100*/
          ttItem.net_amount                 = ttPOSBongLinje.Linjesum * 100
          ttItem.gross_amount               = ttPOSBongLinje.Linjesum * 100
          ttItem.tax_percent                = ttPOSBongLinje.Mva%
          ttItem.barcode                    = ttPOSBongLinje.Strekkode
          ttItem.cost_price                 = ttPOSBongLinje.VVarekost * 100
          ttItem.voided                     = ttPOSBongLinje.Makulert
          ttItem.included_in_total_discount = (ttPOSBongLinje.LinjeRab + ttPOSBongLinje.SubtotalRab) > 0
          ttItem.is_return_item             = ttPOSBongLinje.Antall < 0
          ttItem.is_virtual_product         = (IF AVAILABLE ArtBas THEN ArtBas.OPris ELSE FALSE)
          /*          ttItem.comment = 'Farge: ' + (IF AVAILABLE ArtBas THEN ArtBas.LevFargKod ELSE '') +*/
          /*                           ' Str: ' + ttPOSBongLinje.Storrelse                               */
          .
        /* Rabatt skal gis ? */
        IF AVAILABLE ArtBas AND 
          (ArtBas.ManRabIKas = TRUE OR ArtBas.KundeRabatt = TRUE OR ArtBas.Bonus_Givende = TRUE) THEN       
          ttItem.eligible_for_discount = TRUE.
        /* Er varen makulert, skal den ikke gi rabatt */
        IF ttPOSBongLinje.Makulert THEN  
          ttItem.eligible_for_discount = FALSE.
          
        /* Rydder opp gruppene slik at bare dette Item'ets grupper vil finnes. */
        EMPTY TEMP-TABLE ttItemGroups.
        EMPTY TEMP-TABLE ttItemDimension.
        
        /* Oppretter grupper og dimension hvis det er lagt inn på artikkelen. */
        IF AVAILABLE ArtBas THEN 
        DO:
          CREATE ttItemDimension.
          ASSIGN 
            ttItemDimension.receipt_id = ttItem.receipt_id  
            ttItemDimension.line_id    = ttItem.line_id
            ttItemDimension.cColor     = ArtBas.LevFargKod
            ttItemDimension.cSize      = ttPOSBongLinje.Storrelse 
            ttItemDimension.cVariant   = ArtBas.LevKod
            .
            
          FIND Anv-Kod NO-LOCK WHERE 
            Anv-Kod.anv-Id = ArtBas.anv-id NO-ERROR.
          IF AVAILABLE Anv-Kod THEN 
          DO:
            CREATE ttItemGroups.
            ASSIGN
              ttItemGroups.receipt_id = ttItem.receipt_id  
              ttItemGroups.line_id    = ttItem.line_id
              ttItemGroups.group_id   = STRING(Anv-Kod.anv-Id)
              ttItemGroups.group_name = Anv-Kod.AnvBeskr
              .
          END.
          FIND HovedKategori NO-LOCK WHERE 
            HovedKategori.HovedKatNr = ArtBas.HovedKatNr NO-ERROR.
          IF AVAILABLE HovedKategori THEN 
          DO:
            CREATE ttItemGroups.
            ASSIGN
              ttItemGroups.receipt_id = ttItem.receipt_id
              ttItemGroups.line_id    = ttItem.line_id
              ttItemGroups.group_id   = STRING(HovedKategori.HovedKatNr)
              ttItemGroups.group_name = HovedKategori.HovedKatTekst
              .
          END. 
        END.

        /* Oppretter og tilordner Items objectet. */
        poItem = NEW JsonObject().         

        /* På Item må det legges inn felt for felt */
        poItem:ADD("line_id", ttItem.line_id).
        poItem:ADD("id", ttItem.item_id).
        poItem:ADD("quantity", ttItem.quantity).
        poItem:ADD("unit", ttItem.unit).
        poItem:ADD("description", ttItem.description).
        poItem:ADD("description_alias", ttItem.description_alias).
        poItem:ADD("net_amount", ttItem.net_amount).
        poItem:ADD("gross_amount", ttItem.gross_amount).
        poItem:ADD("tax_percent", ttItem.tax_percent).
        poItem:ADD("barcode", ttItem.barcode).
        poItem:ADD("cost_price", ttItem.cost_price).
        /*          poItem:ADD("voided", ttItem.voided).*/
        poItem:ADD("eligible_for_discount", ttItem.eligible_for_discount).
        poItem:ADD("included_in_total_discount", ttItem.included_in_total_discount).
        poItem:ADD("is_return_item", ttItem.is_return_item).
        poItem:ADD("is_virtual_product", ttItem.is_virtual_produc).
        poItem:ADD("comment", ttItem.comment).
          
        /* Legger på farge og størrelse. */
        IF CAN-FIND(FIRST ttItemDimension) THEN 
        DO:              
          poItemDimension = NEW JsonObject().
          /* Legger på feltene */
          poItemDimension:ADD("color", ttItemDimension.cColor).
          poItemDimension:ADD("size", ttItemDimension.cSize).
          poItemDimension:ADD("variant", ttItemDimension.cVariant).
          /* Legger dimension inn på item objectet. */
          poItem:ADD('dimension',poItemDimension).
        END.
          
        /* Setter samme groups objectet. */
        IF CAN-FIND(FIRST ttItemGroups) THEN 
        DO:
          poItemGroupArray = NEW JsonArray().
          /* Legger alle groups recordene inn i en array. */
          poItemGroupArray:READ(TEMP-TABLE ttItemGroups:HANDLE).
          /* Legger groups array'en inn på item objectet. */ 
          poItem:ADD('groups',poItemGroupArray).
        END.
        /* Legger item objectet inn i item array'en. */
        poItemArray:Add(poItem).
          
      END. /* VARELINJEBLOKK */

      /* Legger opp betalingslinjene. */  
      BETALLINJEBLOKK:
      FOR EACH ttPOSBongLinje WHERE 
        ttPOSBongLinje.B_Id = ttPOSBongHode.B_Id:

        /* Bare varelinjene har varegruppe påført. */
        IF NOT ttPOSBongLinje.VareGr = 0 THEN
          NEXT BETALLINJEBLOKK. 
        /* Ikke bongInfo linjer. */
        IF ttPOSBongLinje.TTId = 95 THEN 
          NEXT BETALLINJEBLOKK.

        FIND TransType NO-LOCK WHERE 
          TransType.TTId = ttPOSBongLinje.TTId NO-ERROR.
        FIND TransBeskr NO-LOCK WHERE 
          TransBeskr.TTId = ttPOSBongLinje.TTId AND 
          TransBeskr.TBId = (IF ttPOSBongLinje.TBId = 0 THEN 1 ELSE ttPOSBongLinje.TBId) NO-ERROR.
        CREATE ttPayment.
        ASSIGN 
          ttPayment.receipt_id   = STRING(ttPOSBongHode.B_Id)          
          ttPayment.line_id      = ttPOSBongLinje.LinjeNr
          ttPayment.amount       = ttPOSBongLinje.LinjeSum * 100
          ttPayment.voided       = ttPOSBongLinje.Makulert
          ttPayment.cDescription = (IF AVAILABLE TransType THEN TransType.Beskrivelse ELSE '') + '/' + 
                                   (IF AVAILABLE TransBeskr THEN TransBeskr.Beskrivelse ELSE '')
          .
        IF ttPOSBongLinje.TTId = 95 THEN
          ttPayment.cDescription = ttPOSBongLinje.BongTekst. 
      END. /* BETALLINJEBLOKK */

      /* Legger opp info linjene. */  
      INFOLINJEBLOKK:
      FOR EACH ttPOSBongLinje WHERE 
        ttPOSBongLinje.B_Id = ttPOSBongHode.B_Id AND 
        ttPOSBongLinje.TTId = 95:

        FIND TransType NO-LOCK WHERE 
          TransType.TTId = ttPOSBongLinje.TTId NO-ERROR.
        FIND TransBeskr NO-LOCK WHERE 
          TransBeskr.TTId = ttPOSBongLinje.TTId AND 
          TransBeskr.TBId = (IF ttPOSBongLinje.TBId = 0 THEN 1 ELSE ttPOSBongLinje.TBId) NO-ERROR.
        CREATE ttExtra_info.
        ASSIGN 
          ttExtra_info.receipt_id = STRING(ttPOSBongHode.B_Id)          
          ttExtra_info.cKey       = (IF AVAILABLE Transtype THEN TransType.Beskrivelse ELSE '')
          ttExtra_info.cValue     = ttPOSBongLinje.BongTekst
          ttExtra_info.value_type = 'INFO'
          . 
          
      END. /* INFOLINJEBLOKK */
        
      /* Setter sammen objectet og array. */
      poReceipt:Add("store", poStore).
      IF AVAILABLE ttCustCustomer THEN 
        poReceipt:Add("customer", poCustCustomer).
      IF CAN-FIND(FIRST ttItem) THEN
      DO:   
        /*          poItemArray:READ(TEMP-TABLE ttItem:HANDLE).*/
        poReceipt:Add("items", poItemArray).
      END.
      IF CAN-FIND(FIRST ttPayment) THEN
      DO:   
        poPaymentArray:READ(TEMP-TABLE ttPayment:HANDLE).
        poReceipt:Add("payments", poPaymentArray).
      END.

      CAST(poReceipt, JsonObject):WriteFile('konv\poReceipt' + STRING(ttPOSBongHode.B_Id) + STRING(ETIME) + '.json').
     
      bResult = TRUE.  
    END. /* BONGKONV */

    RETURN bResult.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC LOGICAL adderDiscountLinesBong(  ):
    
    DEFINE VARIABLE bResult   AS LOGICAL NO-UNDO.
    DEFINE VARIABLE piLinjeNr AS INTEGER NO-UNDO.

    FIND LAST ttPOSBongLinje USE-INDEX idxB_IdLinje NO-ERROR.
    ASSIGN 
      piLinjeNr = ttPOSBongLinje.LinjeNr
      .
    FIND FIRST ttPOSBongHode NO-ERROR.
    IF AVAILABLE ttPOSBongHode THEN
    DO:
      FOR EACH ttDiscounts, 
        FIRST ttRefs OF ttDiscounts, 
        FIRST ttReward WHERE 
        ttReward.receipt_id = ttDiscounts.receipt_id AND 
        ttReward.discounts_id = ttDiscounts.discounts_id:
            
        piLinjeNr = piLinjeNr + 1.
        CREATE ttPOSBongLinje.
        ASSIGN 
          ttPOSBongLinje.b_id     = ttPOSBongHode.B_Id 
          ttPOSBongLinje.ButikkNr = ttPOSBongHode.ButikkNr 
          ttPOSBongLinje.GruppeNr = ttPOSBongHode.GruppeNr 
          ttPOSBongLinje.KasseNr  = ttPOSBongHode.KasseNr 
          ttPOSBongLinje.Dato     = ttPOSBongHode.Dato 
          ttPOSBongLinje.BongNr   = ttPOSBongHode.BongNr 
          ttPOSBongLinje.LinjeNr  = piLinjeNr
          ttPOSBongLinje.TTId     = 95 /* Fritekst */
          ttPOSBongLinje.TBId     = 1
          NO-ERROR. 
        IF ttDiscounts.receipt_text <> '' THEN 
          ttPOSBongLinje.BongTekst = ttDiscounts.receipt_text.
        ELSE                            
          ttPOSBongLinje.BongTekst = ttReward.cType  + ' ' + STRING(ttReward.iValue).
        ttPOSBongLinje.LinjeSum = (ttRefs.amount / 100).     
      END.
      IF bTest THEN 
      DO:
        DATASET dsPOSBong:WRITE-JSON('file', 'konv\sdPOSBong' + 
          STRING(ttPOSBongHode.ButikkNr) + '_' + 
          STRING(ttPOSBongHode.BongNr) +
          '.Json',TRUE).
      END.
    END.
    bResult = TRUE.
    RETURN bResult.


  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC LOGICAL postNewCategory( INPUT-OUTPUT DATASET dsCategory,
    OUTPUT pcReturn AS CHARACTER ):
    
    DEFINE VARIABLE bResult        AS LOGICAL    NO-UNDO.

    DEFINE VARIABLE pcFullPath     AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE pcJsonRespons  AS LONGCHAR   NO-UNDO.
    DEFINE VARIABLE pcResponsType  AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE pcStatusReason AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE pcJSonPayLoad  AS LONGCHAR   NO-UNDO.
    
    DEFINE VARIABLE poJsonArray    AS JsonArray  NO-UNDO.
    DEFINE VARIABLE poJsonObject   AS JsonObject NO-UNDO.
    DEFINE VARIABLE poJsonTest     AS JsonObject NO-UNDO.
    DEFINE VARIABLE pbOk           AS LOG        NO-UNDO.

    DEFINE BUFFER bufBongCRMLogg FOR BongCRMLogg.
    
    FIND FIRST ttCategory.

    IF AVAILABLE ttCategory THEN 
    DO:    
      /* Er token fortsatt gyldig, hentes ikke nytt token */
      IF NOT rtokenDintero:sjekkSysParaToken() THEN 
      DO:
        IF NOT rtokenDintero:getToken(OUTPUT iStatusCode, OUTPUT pcStatusReason )THEN 
        DO:
          ASSIGN 
            bResult = FALSE
            .
          RETURN bResult.
        END.
      END.
      
      IF bTest THEN
      DO: 
        rStandardFunksjoner:SkrivTilLogg(cLogg,
          '  PostNewCategory: mottatt kategori som skal opprettes ' + STRING(ttCategory.category_id) 
          ).    
      END.
      /* Konverterer datasett til JSon og sender den nye bongen. */
      IF datasetCategoryToJSon ( INPUT DATASET dsCategory, OUTPUT poJsonObject) THEN
      SENDOBJECT: 
      DO:
        rStandardFunksjoner:SkrivTilLogg(cLogg,
          '    PostNewCategory: JSon melding opprettet ' + STRING(ttCategory.category_id) 
          ).    

        /* Skriver respons til fil for debug. */
        IF bTest THEN
          CAST(poJsonObject, JsonObject):WriteFile('konv\PostNewCategory' + STRING(ETIME) + '.json').

        ASSIGN 
          pcFullPath = 'https://api.dintero.com/v1/accounts/' + rTokenDintero:cAid + '/products/categories'.
      
        oURL = URI:Parse(pcFullPath, FALSE).
      
        oRequest = RequestBuilder:Post(oURL, poJsonObject) /* URL */
                          :AddHeader('Authorization', rtokenDintero:cToken_Type + ' ' + rtokenDintero:cAccess_Token) /* cToken_Type = 'Bearer' */
                          :AcceptJson() /* we want to get JSON back */
                          :Request.
        /* Bygger respons objektet. Det er tomt her, men fylles når forespørselen eksekverees. */
        oResponse = ResponseBuilder:Build():Response.

        /* Creating a Client. */
        oClient = ClientBuilder:Build()
                    :KeepCookies(CookieJarBuilder:Build()
                    :CookieJar):Client
          .

        /* Execute the request.                                                              */
        /* Sender forespørselen til API'et med preppet request object og tom respons object. */
        oClient:Execute(oRequest, oResponse).   
        oEntity = oResponse:Entity.
        ASSIGN 
          iStatusCode   = oResponse:StatusCode
          cStatusReason = oResponse:StatusReason
          .
        IF iStatusCode = 200 THEN   
        RESPONSBLOKK:
        DO:      
          /* Her er det oEntity objectet som behandles og legges i en longchar. */
          castObject(INPUT 'PostNewCategory', OUTPUT pcJsonRespons, OUTPUT pcResponsType).
        END. /* RESPONSBLOKK */       
        ELSE 
        DO:
          /* Her er det oEntity objectet som behandles og legges i en longchar. */
          castObject(INPUT 'PostNewCategory', OUTPUT pcJsonRespons, OUTPUT pcResponsType).
          bResult = FALSE.
          pcReturn = STRING(iStatusCode) + ' ' + cStatusReason.
        END.
      END. /* SENDOBJECT */
      ELSE 
      DO:
        rStandardFunksjoner:SkrivTilLogg(cLogg,
          '  PosNewCategory: Feilet ved konvertering til JSon.' 
          ).    
      END.
      
    END. /* ttCategory */
    ELSE 
    DO:
      rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  PostNewCategory: mottatt tomt datasett' 
        ).    
    END.

    RETURN bResult.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC LOGICAL datasetCategoryToJSon( INPUT DATASET dsCategory, OUTPUT poJSonObject AS JSonObject ):
    
    DEFINE VARIABLE bResult      AS LOGICAL  NO-UNDO.
    DEFINE VARIABLE pcJsonString AS LONGCHAR NO-UNDO.    
    
    FIX-CODEPAGE(pcJSonString) = 'UTF-8'.
    
    poJSonObject = NEW JsonObject().
    FIND FIRST ttCategory NO-ERROR.
    IF AVAILABLE ttCategory THEN 
    DO:
      poJSonObject:ADD("id", ttCategory.category_id).
      poJSonObject:ADD("name", ttCategory.category_name).
      bResult = TRUE.
    END.

    RETURN bResult.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC VOID lagreBongKalkulasjon(  INPUT DATASET dsPOSBong, pcJsonRespons AS LONGCHAR ):
    
    DEFINE BUFFER bufBongCRMLogg FOR BongCRMLogg.
    /* Her logges bongen. Den ligger ikke i bongdatabasen ennå. Men logges her med den */
    /* lange nøkkelen, og med en JSonformatert bong som bare mangler betalingsrecorden */
    /* Den må legges på senere når bongen sendes over til Dintero.                     */
    SKRIVLOGG:
    DO FOR bufBongCRMLogg TRANSACTION:
      FIND FIRST bufBongCRMLogg EXCLUSIVE-LOCK WHERE
        bufBongCRMLogg.B_Id = ttPOSBongHode.B_Id NO-ERROR.
      IF NOT AVAILABLE bufBongCRMLogg THEN 
      DO:
        CREATE bufBongCRMLogg.
        ASSIGN 
          bufBongCRMLogg.B_Id              = ttPOSBongHode.B_Id
          bufBongCRMLogg.RegistrertDatoTid = NOW
          NO-ERROR.
        ASSIGN  /* Splittet i to pga. dårlige testdata. */
          bufBongCRMLogg.ButikkNr          = ttPOSBongHode.butikkNr  
          bufBongCRMLogg.GruppeNr          = ttPOSBongHode.GruppeNr    
          bufBongCRMLogg.KasseNr           = ttPOSBongHode.KasseNr  
          bufBongCRMLogg.Dato              = ttPOSBongHode.Dato 
          bufBongCRMLogg.BongNr            = ttPOSBongHode.BongNr
          NO-ERROR.
      END.
      IF NOT LOCKED bufBongCRMLogg THEN 
      DO:
        bufBongCRMLogg.JsonPayLoad = pcJsonRespons. /* Legger inn respons fra Dintero. */
          
        IF bTest THEN 
        DO:
          OUTPUT TO VALUE('konv\payloadLagret' + STRING(ttPOSBongHode.B_Id) + '.json').
          pcJsonRespons = bufBongCRMLogg.JsonPayLoad.
          PUT UNFORMATTED STRING(pcJsonRespons).
          OUTPUT CLOSE.
        END.
      END.
      IF AVAILABLE bufBongCRMLogg THEN 
        RELEASE bufBongCRMLogg.
    END. /* SKRIVLOGG */
    
    RETURN.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC LOGICAL CreateNewCatalog( INPUT pcCatalog_Id AS CHARACTER, INPUT pcCatalog_Name AS CHARACTER ):

    DEFINE VARIABLE pcFullPath     AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE pcJsonRespons  AS LONGCHAR   NO-UNDO.
    DEFINE VARIABLE pcResponsType  AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE pcStatusReason AS CHARACTER  NO-UNDO.
    
    DEFINE VARIABLE bResult        AS LOG        NO-UNDO.
    DEFINE VARIABLE poJsonObject   AS JsonObject NO-UNDO.

    /* Er token fortsatt gyldig, hentes ikke nytt token */
    IF NOT rtokenDintero:sjekkSysParaToken() THEN 
    DO:
      IF NOT rtokenDintero:getToken(OUTPUT iStatusCode, OUTPUT pcStatusReason )THEN 
      DO:
        ASSIGN 
          bResult = FALSE
          .
        RETURN bResult.
      END.
    END.
    
    IF bTest THEN
    DO: 
      rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  CreateNewCatalog: mottatt katalog som skal opprettes ' + pcCatalog_Id + ' ' + pcCatalog_Name 
        ).    
    END.
    
    /* Oppretter payload objectet. */
    poJsonObject = NEW JsonObject().
    poJsonObject:ADD("catalog_id", pcCatalog_Id).
    poJsonObject:ADD("catalog_name", pcCatalog_Name).
    
    SENDOBJECT: 
    DO:
      rStandardFunksjoner:SkrivTilLogg(cLogg,
        '    CreateNewCatalog: JSon melding opprettet.' 
        ).    

      /* Skriver payload til fil for debug. */
      IF bTest THEN
        CAST(poJSonObject, JSONObject):WriteFile('konv\CreateNewCatalogPayload' + STRING(ETIME) + '.json').

      ASSIGN 
        pcFullPath = 'https://api.dintero.com/v1/accounts/' + rTokenDintero:cAid + '/products/catalogs'.
    
      /* Creating a Client. */
      oClient = ClientBuilder:Build()
                  :KeepCookies(CookieJarBuilder:Build()
                  :CookieJar):Client
        .
      oRequest = RequestBuilder:Post(pcFullPath) /* URL */
                        :AddHeader('Authorization', rtokenDintero:cToken_Type + ' ' + rtokenDintero:cAccess_Token) /* cToken_Type = 'Bearer' */
                        :AcceptJson() /* we want to get JSON back */
                        :AddJSonData(poJSonObject) /* Her legges JSon meldingen inn */
                        :Request.
      /* Bygger respons objekstet. Det er tomt her, men fylles når forespørselen eksekverees. */
      oResponse = ResponseBuilder:Build():Response.
      /* Execute the request.                                                              */
      /* Sender forespørselen til API'et med preppet request object og tom respons object. */
      oClient:Execute(oRequest, oResponse).   
      oEntity = oResponse:Entity.
      ASSIGN 
        iStatusCode   = oResponse:StatusCode
        cStatusReason = oResponse:StatusReason
        .
      DO:      
        /* Her er det oEntity objectet som behandles. */
        castObject(INPUT 'CreateNewCatalogResponse', OUTPUT pcJsonRespons, OUTPUT pcResponsType).
      END.       
      bResult = TRUE.
    END. /* SENDOBJECT */
    
    RETURN bResult.

  END METHOD.

  DESTRUCTOR PUBLIC customerDintero ( ):

  END DESTRUCTOR.

END CLASS.