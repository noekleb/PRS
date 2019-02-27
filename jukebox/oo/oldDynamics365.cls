 
/*------------------------------------------------------------------------
   File        : dynamics
   Purpose     : 
   Syntax      : 
   Description : 
   Author(s)   : goo_000
   Created     : Tue Nov 07 14:37:48 CET 2017
   Notes       : 
 ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Microsoft.IdentityModel.Clients.ActiveDirectory.*.

USING System.*.
USING System.Collections.Generic.*.
USING System.IO.*.
USING System.Linq.*.
USING System.Net.*.
USING System.Runtime.Serialization.*.
USING System.Runtime.Serialization.Json.*.
USING System.Text.*.
USING OpenEdge.Net.HTTP.*.
USING Progress.Json.ObjectModel.JsonObject FROM PROPATH.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS Dynamics365 INHERITS JSON:
  DEFINE PROPERTY WebApiBaseUrl AS CHARACTER NO-UNDO GET. SET.
  DEFINE PROPERTY Username      AS CHARACTER NO-UNDO GET. SET. 
  DEFINE PROPERTY Password      AS CHARACTER NO-UNDO 
    PRIVATE GET. SET.
  DEFINE PROPERTY ClientId      AS CHARACTER NO-UNDO GET. SET.
  DEFINE PROPERTY Token         AS CHARACTER NO-UNDO GET. SET. 
  
  DEFINE PROPERTY oAuthParam    AS CLASS "System.Threading.Tasks.Task<Microsoft.IdentityModel.Clients.ActiveDirectory.AuthenticationParameters>" NO-UNDO GET. 
    PRIVATE SET.
  DEFINE PROPERTY oUri          AS CLASS Uri NO-UNDO GET. 
    PRIVATE SET.
  DEFINE PROPERTY oAuthContext  AS CLASS Microsoft.IdentityModel.Clients.ActiveDirectory.AuthenticationContext NO-UNDO GET. 
    PRIVATE SET.
  
  DEFINE PROPERTY oClientCred   AS CLASS Microsoft.IdentityModel.Clients.ActiveDirectory.UserCredential NO-UNDO GET. 
    PRIVATE SET.

  define property SendingsStatus as char no-undo get. set. 
  
  DEFINE VARIABLE wc         AS System.Net.WebClient                 NO-UNDO.
  DEFINE VARIABLE wcRequest  AS System.Net.WebRequest                NO-UNDO.
  DEFINE VARIABLE wcResponse AS System.Net.WebResponse               NO-UNDO.

  DEFINE VARIABLE oClient    AS OpenEdge.Net.HTTP.IHttpClient        NO-UNDO.
  DEFINE VARIABLE oRequest   AS OpenEdge.Net.HTTP.IHttpRequest       NO-UNDO.
  DEFINE VARIABLE oResponse  AS OpenEdge.Net.HTTP.IHttpResponse      NO-UNDO.
  DEFINE VARIABLE myJson     AS Progress.Json.ObjectModel.JsonObject NO-UNDO.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/
    
  CONSTRUCTOR PUBLIC Dynamics365 (ipUri AS CHARACTER, ipUser AS CHARACTER, ipPwd AS CHARACTER,ipClient AS CHARACTER):
    SUPER ().
    ASSIGN 
      WebApiBaseUrl = ipUri
      UserName      = ipUser
      Password      = ipPwd
      ClientId      = ipClient
      .  
    oUri = NEW Uri(webApiBaseUrl).
    PAUSE 2 NO-MESSAGE.
    oAuthParam = AuthenticationParameters:CreateFromResourceUrlAsync(oUri).
    if not valid-object(oAuthParam) then do:
      SendingsStatus = 'Authentication failed...'.
      return.
    end.
    PAUSE 2 NO-MESSAGE.
    oAuthContext = NEW AuthenticationContext(oAuthParam:Result:Authority).
    if not valid-object(oAuthContext) then do:
      SendingsStatus = 'Authentication Context failed...'.
      return.
    end.
    PAUSE 2 NO-MESSAGE.
    oClientCred = NEW Microsoft.IdentityModel.Clients.ActiveDirectory.UserCredential(username,password).
    if not valid-object(oClientCred) then do:
      SendingsStatus = 'ClientCred failed...'.
      return.
    end.
       PAUSE 2 NO-MESSAGE.
    Token = oAuthContext:AcquireToken(oAuthParam:Result:Resource,clientId,oClientCred):AccessToken.

    CATCH eAny AS Progress.Lang.Error :
        MESSAGE eAny:GetMessage(1)
        VIEW-AS ALERT-BOX.
    END CATCH.
  END CONSTRUCTOR.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
{"acan_Kontakt@odata.bind":"\/contacts(184552ef-1c7e-e711-80ec-3863bb349b40)","acan_name":"Innboforsikring", "acan_forsikringtype":"123"}
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC LOGICAL addInsurance(ipContactGUID AS CHARACTER, ipProduktnr AS INTEGER, ipForsikring AS CHARACTER ):
    
    DEFINE VARIABLE cQuery      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lcResponse  AS LONGCHAR  NO-UNDO.
    DEFINE VARIABLE oJsonObject AS CLASS     JsonObject NO-UNDO.
    DEFINE VARIABLE cString     AS CHARACTER NO-UNDO.
    
    oJsonObject = NEW JsonObject().
    oJsonObject:add('acan_Kontakt@odata.bind','/contacts(' + ipContactGUID + ')').
    oJsonObject:add('acan_name',ipForsikring).
    oJsonObject:add('acan_forsikringskode',STRING(ipProduktnr)).
    fix-codepage(lcResponse) = 'UTF-8'.
    cQuery = "".
    wc = getWebClient().
    cString = oJsonObject:GetJsonText().
    LOG-MANAGER:WRITE-MESSAGE ('addInsurance:' + cString).    
    wc:UploadString('acan_forsikrings','POST',cString).    

    RETURN TRUE.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC LONGCHAR getContact(ipContactGUID AS CHARACTER):
    DEFINE VARIABLE cQuery     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lcResponse AS LONGCHAR  NO-UNDO.
    
    fix-codepage(lcResponse) = 'UTF-8'.
    cQuery = "contacts?$select=contactid&$filter=contactid eq '" + string(ipContactGUID) /*+ "'&$top=1"*/.
    wc = getWebClient().
    lcResponse = wc:DownloadString(cQuery).    

    RETURN lcResponse.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
var insuranceQuery = $"acan_forsikrings?$select=acan_forsikringid,acan_name,acan_forsikringskode&$filter=_acan_kontakt_value eq {contact.contactid.Value.ToString("D")}";
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC LONGCHAR getInsurance(ipContactGUID AS CHARACTER):
    DEFINE VARIABLE cQuery     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lcResponse AS LONGCHAR  NO-UNDO.
    /*    def var wc as class System.Net.WebClient no-undo.*/
    fix-codepage(lcResponse) = 'UTF-8'.
    IF ipContactGUID = '' THEN cQuery = "acan_forsikrings?$select=acan_forsikringid,acan_name,acan_forsikringskode".
    ELSE cQuery = "acan_forsikrings?$select=acan_forsikringid,acan_name,acan_forsikringskode&$filter=_acan_kontakt_value eq " + string(ipContactGUID) /*+ "&$top=1"*/.
    wc = getWebClient().
    lcResponse = wc:DownloadString(cQuery).    
    RETURN lcResponse.
  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC LONGCHAR getMedlem(INPUT ipMedlemsnr AS INTEGER ):
    DEFINE VARIABLE cQuery     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lcResponse AS LONGCHAR  NO-UNDO.
    MESSAGE 'getMedlem..'
    VIEW-AS ALERT-BOX.
    fix-codepage(lcResponse) = 'UTF-8'.
    IF ipMedlemsNr = 0 THEN cQuery = "contacts?$select=contactid,acan_medlemsnummer".
    ELSE cQuery = "contacts?$select=contactid,acan_medlemsnummer&$filter=acan_medlemsnummer eq '" + string(ipMedlemsNr) + "'&$top=1".
    wc = getWebClient().
    lcResponse = wc:DownloadString(cQuery).    
    RETURN lcResponse.
  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC CLASS System.Net.WebClient getWebClient():
    wc = NEW WebClient().
    wc:Encoding = Encoding:UTF8.
    wc:BaseAddress = webApiBaseUrl.

    wc:Headers:Add("OData-MaxVersion", "4.0").
    wc:Headers:Add("OData-Version", "4.0").
    wc:Headers:Add("Accept", "application/json").
    wc:Headers:Add("Content-Type", "application/json;charset=utf-8").
    wc:Headers:Add("Authorization", "Bearer " + Token).
    RETURN wc.
  END METHOD.

END CLASS.