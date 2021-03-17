USING Progress.Json.ObjectModel.JsonObject.
USING Progress.Json.ObjectModel.JsonArray . 

DEF VAR cJSonString AS LONGCHAR NO-UNDO.

DEFINE VARIABLE oJson        AS JsonObject NO-UNDO. 
DEFINE VARIABLE oUrl         AS JsonObject NO-UNDO. 
DEFINE VARIABLE oOrder       AS JsonObject NO-UNDO. 
DEFINE VARIABLE oItemsArray  AS JsonArray NO-UNDO. 

DEF TEMP-TABLE ttUrl
    FIELD return_url AS CHAR 
    .

DEF TEMP-TABLE ttOrder
    field amount AS DEC 
    field currency AS CHAR 
    field merchant_reference AS CHAR 
    .

DEF TEMP-TABLE ttItems
    FIELD id AS CHAR 
    FIELD line_id AS CHAR  
    FIELD description AS CHAR 
    FIELD quantity AS INT 
    FIELD amount AS DEC  
    FIELD vat_amount AS DEC 
    FIELD vat AS DEC 
    .

CREATE ttUrl.
ASSIGN 
    return_url = "https://checkout.dintero.com/result"
    .

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
    ttItems.amount      = 29990
    ttItems.vat_amount  = 6000
    ttItems.vat         = 25
    ttOrder.amount      = ttOrder.amount + ttItems.amount
    .

CREATE ttItems.    
ASSIGN 
    ttItems.id          = "chair-2"
    ttItems.line_id     = "2"
    ttItems.description = "StablestolTNC"
    ttItems.quantity    = 1
    ttItems.amount      = 29990
    ttItems.vat_amount  = 6000
    ttItems.vat         = 25
    ttOrder.amount      = ttOrder.amount + ttItems.amount
    .

/* Create new JsonObjects */
oJson       = NEW JsonObject().
oUrl        = NEW JsonObject().
oOrder      = NEW JsonObject().
oItemsArray = NEW JsonArray().

/* Bygger opp JSon objektet.                                            */
/* Rekkefølgen det settes sammen på er viktig for å få ønsket resultat. */
oUrl:Add("return_url",ttUrl.return_url).

oOrder:ADD("amount", ttOrder.amount).
oOrder:ADD("currency", ttOrder.currency).
oOrder:ADD("merchant_reference", ttOrder.merchant_reference).

oOrder:Add("items", oItemsArray).
oItemsArray:READ(TEMP-TABLE ttItems:HANDLE).

oJson:Add("url", oUrl).
oJson:Add("order", oOrder).
oJson:Add("profile_id","T11112542.4XopPVLQtJfeUWbDHUPP8U").

/* Write the JSON string to a character variable */
oJson:Write(cJSonString,TRUE). 

MESSAGE STRING(cJSonString) SKIP(1)
    VIEW-AS ALERT-BOX.


/* {                                                       */
/*   "url": {                                              */
/*     "return_url": "https://checkout.dintero.com/result" */
/*   },                                                    */
/*   "order": {                                            */
/*     "amount": 29990,                                    */
/*     "currency": "NOK",                                  */
/*     "merchant_reference": "string",                     */
/*     "items":[                                           */
/*       {                                                 */
/*         "id": "chair-1",                                */
/*         "line_id": "1",                                 */
/*         "description": "Stablestol",                    */
/*         "quantity": 1,                                  */
/*         "amount": 29990,                                */
/*         "vat_amount": 6000,                             */
/*         "vat": 25                                       */
/*       }                                                 */
/*     ]                                                   */
/*   },                                                    */
/*   "profile_id": "T11112542.4XopX7PCwvdZzWxu5anRmd"      */
/* }                                                       */

