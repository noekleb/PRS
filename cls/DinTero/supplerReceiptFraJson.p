
/*------------------------------------------------------------------------
    File        : supplerReceiptFraJson.p
    Purpose     : 

    Syntax      :

    Description : Fyller på bongen med rabattlinjene hentet fra json melding i bongCRMLogg.

    Author(s)   : Tom Nøkleby
    Created     : Sun Jan 24 12:51:59 CET 2021
    Notes       : TN 27/3-21
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
USING Progress.Json.ObjectModel.ObjectModelParser.
USING Progress.Json.ObjectModel.JsonObject.
USING Progress.Json.ObjectModel.JsonArray.

BLOCK-LEVEL ON ERROR UNDO, THROW.

{cls\dintero\ttCustomerObj.i}
{cls\dintero\dsCustomerObj.i}
{cls\dintero\ttReceipts.i}
{cls\dintero\dsReceipts.i}

DEFINE INPUT        PARAMETER pcReceipts AS LONGCHAR NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsReceipts.

DEFINE VARIABLE bTest AS LOG NO-UNDO.

DEFINE VARIABLE myParser  AS ObjectModelParser NO-UNDO.
DEFINE VARIABLE oReceipts AS JsonObject NO-UNDO.
DEFINE VARIABLE oDiscountsArray AS JsonArray NO-UNDO.
DEFINE VARIABLE oItemsArray AS JsonArray NO-UNDO.

DEFINE VARIABLE cLabelsReceipts AS CHARACTER EXTENT NO-UNDO. /* NB: Ikke angi antall ekstenter. */
DEFINE VARIABLE cReceipt_id AS CHARACTER NO-UNDO.
DEFINE VARIABLE iEkst AS INTEGER NO-UNDO.
DEFINE VARIABLE iLoop AS INTEGER NO-UNDO.
DEFINE VARIABLE lcLongChar AS LONGCHAR NO-UNDO.


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
myParser = NEW ObjectModelParser().

bTest = FALSE.

IF bTest THEN 
DO:
  OUTPUT TO VALUE('konv\ByggReceiptFraJsonPayload.json').
  PUT UNFORMATTED STRING(pcReceipts).
  OUTPUT CLOSE.
END.

/* Oppretter JsonObjectet fra longchar variabelens innhold. */
oReceipts = CAST(myParser:Parse(pcReceipts), JsonObject).

/* Henter bongens Id. Brukes for å hente ttReceipts. */
cReceipt_id = oReceipts:GETCHARACTER("receipt_id").

/* Her hentes feltene is_changed og net_amount ut og oppdateres i ttReceipts recorden. */
FIND FIRST ttReceipts WHERE 
  ttReceipts.receipt_id = cReceipt_id NO-ERROR.
IF AVAILABLE ttReceipts THEN 
  DO:
    ASSIGN
      ttReceipts.is_changed = oReceipts:GetLogical("is_changed"). 
/*      ttReceipts.net_amount = oReceipts:GetInteger("net_amount"). Blir ikke endret av Dintero! */
      .
  END. 

/* Sjekker om bong er endred. Dvs. at Dintero har lagt på rabatter som nå skal legges inn på bongen. */
/* Er bongen ikke endret, trenger vi ikke gjøre noe her.                                             */
IF oReceipts:GetLogical("is_changed") = FALSE THEN
  RETURN.

/* Henter navn på alle felt i receipts. Antall ekstenter settes også nå når variabelen først brukes. */
cLabelsReceipts =  oReceipts:GetNames().
/* Henter antall felt i objektet. */
iEkst = EXTENT(cLabelsReceipts).

/* Looper igjennom ekstentene, og plukker ut det vi skal behandle videre. */
/* Arrayene må behandles etter at vi har fullført loopen. Dette skyldes   */
/* at vi ikke kjenner rekkefølgen på feltene, og må ha tak i receipt_id   */
/* før behandlingen av arrayene kan startes.                              */
LOOPBLOKK:
DO iLoop = 1 TO iEkst:
  CASE cLabelsReceipts[iLoop]:
    /* Her hentes Discount array'en ut og behandles. */ 
    WHEN "discounts" THEN 
      oDiscountsArray = oReceipts:GetJsonArray("discounts").
    /* Her hentes Items arrayen ut og behandles. */      
    WHEN "items" THEN 
      oItemsArray = oReceipts:GetJsonArray("items"). 
  END CASE. 
END. /* LOOPBLOKK */

/* Pakker ut Discounts arrayen. */
RUN pakkUtDiscounts (oDiscountsArray).

/* Pakker ut Items arrayen. */ 
RUN pakkUtItems (oItemsArray).
 
/* Rydder opp. */ 
DELETE OBJECT myParser. 
DELETE OBJECT oDiscountsArray.
DELETE OBJECT oItemsArray.

RETURN.

/* **********************  Internal Procedures  *********************** */

PROCEDURE pakkUtDiscounts:
/*------------------------------------------------------------------------------
 Purpose:
 Notes: "discounts": [
                      {
                          "id": "86258083-868a-4cb6-9a0a-fe1916d95b1f",
                          "requirement": {
                              "purchase_to": "2021-10-24T12:33:00Z",
                              "purchase_from": "2020-10-29T11:44:04Z"
                          },
                          "limitation": {
                              "discount_repeat_usage": -1
                          },
                          "reward": {
                              "type": "discount_percent",
                              "value": 10
                          },
                          "created_by": "ba5d6d2c-3b59-4454-b614-207e3a63f8cd",
                          "created_at": "2020-11-18T11:26:17.423Z",
                          "updated_at": "2020-11-18T11:26:17.423Z",
                          "type": "receipt",
                          "links": [],
                          "metadata": {},
                          "active": true,
                          "refs": [
                              {
                                  "id": "72ef48f5-f34d-41ca-9340-abb7e93800ee",
                                  "usage": 1,
                                  "amount": 20500,
                                  "items": [
                                      {
                                          "line_id": 20,
                                          "amount": 11500
                                      },
                                      {
                                          "line_id": 22,
                                          "amount": 9000
                                      }
                                  ],
                                  "statistics": {
                                      "usage": 1,
                                      "amount": 20500
                                  }
                              }
                          ]
                      }
                  ],   
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER poDiscountsArray AS JsonArray NO-UNDO.

  /* Henter kvitteringen. */
  FIND FIRST ttReceipts WHERE 
    ttReceipts.receipt_id = cReceipt_id NO-ERROR.
  IF AVAILABLE ttReceipts THEN 
    DO:
      /* Her lagres hele discount arrayen på kvitteringen, slik at dette senere      */
      /* kan hentes opp og 'knappes inn' på kvitteringen når den sendes til Dintero. */
      /* Må her gå veien om en longchar variabel.                                    */
      poDiscountsArray:WRITE(lcLongChar).
      ASSIGN 
        ttReceipts.lcDiscounts = lcLongChar
        .
    END.
  
  RETURN.

END PROCEDURE.

PROCEDURE pakkUtItems:
/*------------------------------------------------------------------------------
 Purpose: Pakker ut Items. 
          NB: - ttItem er allerede opprettet. Her må det bare hentes slik at 
                det kan bekreftes. Receipt_id og line_id er nøkkel.
              - Legger så hele JsonArray'en inn i temp-tabell recorden. 
 Notes: "items": [
                  {
                      "line_id": 20,
                      "id": "440029989\/DARK INDIGO",
                      "quantity": 1,
                      "unit": "Stk",
                      "description": "Varegruppe 7322623315273",
                      "description_alias": "A-SHAPED DENIM MINI SKIRT",
                      "net_amount": 115000,
                      "gross_amount": 115000,
                      "tax_percent": 25,
                      "barcode": "7322623315235",
                      "cost_price": 36000,
                      "eligible_for_discount": true,
                      "included_in_total_discount": false,
                      "is_return_item": false,
                      "is_virtual_product": false,
                      "comment": "",
                      "dimension": {
                          "color": "989\/DARK INDIGO",
                          "size": "34",
                          "variant": "440029"
                      },
                      "groups": [
                          {
                              "id": "30",
                              "name": "BABY"
                          },
                          {
                              "id": "761",
                              "name": "ACC-BABY GIRLS"
                          }
                      ],
                      "discount_lines": [
                          {
                              "amount": 11500,
                              "description": "",
                              "discount_id": "86258083-868a-4cb6-9a0a-fe1916d95b1f",
                              "discount_type": "external",
                              "line_id": 0
                          }
                      ],
                      "is_changed": true
                  },
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER poItemsArray AS JsonArray NO-UNDO.
 
  DEFINE VARIABLE poItems AS JsonObject.
  DEFINE VARIABLE poDiscountLinesArray AS JsonArray.
  DEFINE VARIABLE poDiscountLinesObj AS JsonObject.

  DEFINE VARIABLE piArrayLoop AS INTEGER NO-UNDO.
  DEFINE VARIABLE piLoop AS INTEGER NO-UNDO.
  DEFINE VARIABLE pcLabelsItems AS CHARACTER EXTENT NO-UNDO.
  DEFINE VARIABLE piLine_id AS INTEGER NO-UNDO. /* Dintero */
  
  ARRAYLOOP:
  DO piArrayLoop = 1 TO poItemsArray:LENGTH:
  
    /* Henter ut objectet. */
    poItems = poItemsArray:GetJsonObject(piArrayLoop).
    
    /* Henter ut linjenummer på itemobjectet. */
    piLine_id = poItems:GETINTEGER ("line_id").

    /* Henter kvitteringslinjen. */
    FIND FIRST ttItem WHERE 
      ttItem.receipt_id = cReceipt_id AND 
      ttItem.line_id    = piLine_Id NO-ERROR.
    IF NOT AVAILABLE ttItem THEN 
      FIND FIRST ttItem WHERE 
        ttItem.receipt_id = cReceipt_id AND 
        ttItem.line_id    = piArrayLoop NO-ERROR.

    IF AVAILABLE ttItem THEN 
      DO:
        /* Henter ut rabattarrayen som skal ligge på kvitteringslinjen. */
        poDiscountLinesArray = poItems:GetJsonArray("discount_lines").
        /* Må gå om en longchar. */
        poDiscountLinesArray:WRITE(lcLongChar).
        /* Lagrer rabatt arrayen på kvitteringslinjen. */
        ASSIGN
          ttItem.lcDiscount_Lines = lcLongChar
          .
/*        /* Her plukkes rabatten ut. For å korrigere nettosun i bonghode. */                        */
/*        DISCOUNTLOOP:                                                                              */
/*        DO piLoop = 1 TO poDiscountLinesArray:LENGTH:                                              */
/*          poDiscountLinesObj = poDiscountLinesArray:GetJsonObject(piLoop).                         */
/*          ttReceipts.net_amount = ttReceipts.net_amount - poDiscountLinesObj:GETINTEGER ("amount").*/
/*        END. /* DISCOUNTLOOP */                                                                    */
      END.
  END. /* ARRAYLOOP */
  
  RETURN.

END PROCEDURE.

