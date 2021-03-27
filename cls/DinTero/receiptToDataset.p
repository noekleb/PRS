
/*------------------------------------------------------------------------
    File        : receiptToDataset.p
    Purpose     : Tar håndom konvertering og omregning av bong etter 
                  rabattkalkulasjonen er gjort.

    Syntax      :

    Description : Tar receipt fra rabattkalkulasjon over til dsPOSBong oppsettet.

    Author(s)   : Tom Nøkleby
    Created     : Sat Nov 21 09:36:28 CET 2020 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
USING Progress.Json.ObjectModel.ObjectModelParser.
USING Progress.Json.ObjectModel.JsonObject.
USING Progress.Json.ObjectModel.JsonArray.

{cls\dintero\ttPOSBong.i}
{cls\dintero\dsPOSBong.i}
{cls\dintero\ttReceipts.i}
{cls\dintero\dsReceipts.i}

DEFINE INPUT PARAMETER pcReceipt AS LONGCHAR NO-UNDO.
DEFINE OUTPUT PARAMETER DATASET FOR dsReceipts.

DEFINE VARIABLE cReceiptFile              AS CHARACTER         NO-UNDO.
DEFINE VARIABLE bOk                       AS LOG               NO-UNDO.
DEFINE VARIABLE iLoop                     AS INTEGER           NO-UNDO.
DEFINE VARIABLE iLoop2                    AS INTEGER           NO-UNDO.  
DEFINE VARIABLE iLoop3                    AS INTEGER           NO-UNDO.  
DEFINE VARIABLE iLoop4                    AS INTEGER           NO-UNDO.  
DEFINE VARIABLE iLoop5                    AS INTEGER           NO-UNDO.  
DEFINE VARIABLE cTekst                    AS CHARACTER         NO-UNDO.
DEFINE VARIABLE bTest                     AS LOG               NO-UNDO.  

/* NB: Når antall ekstenter ikke angis på en variabel, settes dette antallet første gang variablelen benyttes. */    
     
DEFINE VARIABLE oJSonObject               AS JsonObject        NO-UNDO.
DEFINE VARIABLE myLabels                  AS CHARACTER         EXTENT NO-UNDO. /* NB: Ikke angi antall ekstenter. */

DEFINE VARIABLE oJsonArrayDiscounts       AS JSonArray         NO-UNDO.
DEFINE VARIABLE oJsonObjectDiscount       AS JsonObject        NO-UNDO.
DEFINE VARIABLE myLabelsDiscount          AS CHARACTER         EXTENT NO-UNDO. /* NB: Ikke angi antall ekstenter. */

DEFINE VARIABLE oJsonObjectStore          AS JsonObject        NO-UNDO.
DEFINE VARIABLE myLabelsStore             AS CHARACTER         EXTENT NO-UNDO. /* NB: Ikke angi antall ekstenter. */

DEFINE VARIABLE oJSonArrayItems           AS JsonArray         NO-UNDO.
DEFINE VARIABLE oJSonObjectItems          AS JsonObject        NO-UNDO.

DEFINE VARIABLE oJsonArrayDiscount_lines  AS JsonArray         NO-UNDO.
DEFINE VARIABLE oJSonObjectDiscount_lines AS JsonObject        NO-UNDO.
DEFINE VARIABLE myLabelsDiscount_lines    AS CHARACTER         EXTENT NO-UNDO. /* NB: Ikke angi antall ekstenter. */

DEFINE VARIABLE oJsonObjectRequirement    AS JsonObject        NO-UNDO.
DEFINE VARIABLE myLabelsRequirement       AS CHARACTER         EXTENT NO-UNDO. /* NB: Ikke angi antall ekstenter. */

DEFINE VARIABLE oJsonObjectLimitation     AS JsonObject        NO-UNDO.
DEFINE VARIABLE myLabelsLimitation        AS CHARACTER         EXTENT NO-UNDO. /* NB: Ikke angi antall ekstenter. */

DEFINE VARIABLE oJsonObjectReward         AS JsonObject        NO-UNDO.
DEFINE VARIABLE myLabelsReward            AS CHARACTER         EXTENT NO-UNDO. /* NB: Ikke angi antall ekstenter. */

DEFINE VARIABLE oJsonArrayRefs            AS JsonArray         NO-UNDO.
DEFINE VARIABLE oJsonObjectRefs           AS JsonObject        NO-UNDO.
DEFINE VARIABLE myLabelsRefs              AS CHARACTER         EXTENT NO-UNDO. /* NB: Ikke angi antall ekstenter. */
DEFINE VARIABLE oJsonObjectstatistics     AS JsonObject        NO-UNDO. 
DEFINE VARIABLE oJsonArrayDiscountItems   AS JsonArray         NO-UNDO.
DEFINE VARIABLE oJsonObjectDiscountItems  AS JsonObject        NO-UNDO.

DEFINE VARIABLE oParser                   AS ObjectModelParser NO-UNDO.
 
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */



ASSIGN 
  bTest = FALSE 
  .

oParser = NEW ObjectModelParser().
oJsonArrayDiscounts = NEW JsonArray().
oJsonObjectStore = NEW JsonObject().

/* Legger Longchar variabelen inn i et object.                                */
/* Gjør det denne veien, fordi det kommer som varibel når TEST over tas bort. */ 
oJsonObject = CAST(oParser:Parse(pcReceipt), JsonObject).

/* Legger objectet inn i longchar variabelen. */  
CAST(oJsonObject, JSONObject):Write(pcReceipt).

/* Leser ut lablene på alle entryene i hovedobjektet. */
myLabels = oJsonObject:GetNames().

/* Skaper record for lagring av data. */
CREATE ttReceipts.

/* Løper igjennom og henter ut feltene fra root noden. */
OBJECTLOOP:
DO iLoop = 1 TO EXTENT(myLabels):
  /* Henter ut verdien i feltet. */
  cTekst = oJsonObject:getJsonText(myLabels[iLoop]).
   
  /*  MESSAGE "Property : " myLabels[iLoop] " : " cTekst SKIP(1)       */
  /*      "Number of Properties in the object : " EXTENT(myLabels) SKIP*/
  /*      "Current Property : " iLoop SKIP                             */
  /*      VIEW-AS ALERT-BOX TITLE "ProcessObject" .                    */
      
  /* Legger inn data i feltene i recorden. */    
  CASE myLabels[iLoop]:
    /* Tar hånd om feltene */
    WHEN 'is_changed' THEN 
      ttReceipts.is_changed = IF cTekst = 'true' THEN TRUE ELSE FALSE.
    WHEN 'receipt_id' THEN 
      ttReceipts.receipt_id = cTekst. 
    WHEN 'gross_amount' THEN 
      ttReceipts.gross_amount = INTEGER(cTekst).
    WHEN 'net_amount' THEN 
      ttReceipts.net_amount = INTEGER(cTekst).
    WHEN 'round_off_to_coin' THEN 
      ttReceipts.round_off_to_coin = INT(cTekst).
    WHEN 'currency' THEN 
      ttReceipts.currency = cTekst.
    WHEN 'purchase_at' THEN 
      ttReceipts.purchase_at = cTekst.
    WHEN 'receipt_text' THEN 
      ttReceipts.receipt_text = cTekst. 
    WHEN 'no_of_items' THEN 
      ttReceipts.no_of_items = INTEGER(cTekst).
    WHEN 'total_discount' THEN 
      ttReceipts.total_discount = INTEGER(cTekst).
    WHEN 'operator_id' THEN 
      ttReceipts.operator_id = cTekst.
    WHEN 'operator_name' THEN 
      ttReceipts.operator_name = cTekst.
    WHEN 'salesperson_id' THEN 
      ttReceipts.salesperson_id = cTekst.
    WHEN 'salesperson_name' THEN 
      ttReceipts.salesperson_name = cTekst.
    WHEN 'comment' THEN 
      ttReceipts.comment = cTekst.
    WHEN 'customer_id' THEN 
      ttReceipts.customer_id = cTekst.
    /* Tar hånd om objectene */   
    WHEN 'store' THEN 
      DO:
        oJsonObjectStore = oJsonObject:GetJsonObject("store").
        myLabelsStore = oJsonObjectStore:GetNames().
      /* Pakkes ut i engen blok for enkelthets skyld :). */
      END.
    WHEN 'discounts' THEN 
      DO:
        oJsonArrayDiscounts = oJsonObject:GetJsonArray("discounts").
      /* Pakker ut denne i egen blokk under. da den har 4 nivåer. */
      END.
    WHEN 'items' THEN 
      DO:
        oJsonArrayItems = oJsonObject:GetJsonArray("items").
      END.
  END CASE.
      
END. /* OBJECTLOOP */

/* Tar hånd om varelinjene. */
IF oJsonArrayItems:LENGTH > 0 THEN 
ITEMSBLOKK:
DO:
  ITEMARRAYBLOKK:
  DO iLoop = 1 TO oJsonArrayItems:LENGTH:
    oJsonObjectItems = oJsonArrayItems:GetJsonObject(iLoop).
    CREATE ttItem.
    ASSIGN 
      ttItem.receipt_id = ttReceipts.receipt_id
      ttItem.line_id    = INT(oJsonObjectItems:getJsonText('line_id'))
      .
    /* Løper igjennom feltene i recorden. */
    RUN itemsBlokk.
    
  END. /* ITEMARRAYBLOKK */
END. /* ITEMSBLOKK */

/* Tar hånd om Store recorden. */ 
IF EXTENT(myLabelsStore) > 1 THEN
STOREBLOKK: 
DO:
  CREATE ttStore.
  ASSIGN 
    ttStore.receipt_id = ttReceipts.receipt_id
    .
  DO iLoop = 1 TO EXTENT(myLabelsStore):
    cTekst = oJsonObjectStore:getJsonText(myLabelsStore[iLoop]).
    CASE myLabelsStore[iLoop]:
      WHEN 'id' THEN 
        ttStore.id = cTekst.
      WHEN 'name' THEN 
        ttStore.cName = cTekst.
    END CASE.
  END.
END. /* STOREBLOKK */

/* Tar hånd om rabattene. */
IF oJsonArrayDiscounts:LENGTH > 0 THEN 
DISCOUNTSBLOKK:
DO:
  DISCOUNTARRAYBLOKK:
  DO iLoop = 1 TO oJsonArrayDiscounts:LENGTH:
    oJsonObjectDiscount = oJsonArrayDiscounts:GetJsonObject(iLoop).
    myLabelsDiscount = oJsonObjectDiscount:GetNames().
    CREATE ttDiscounts.
    ASSIGN 
      ttDiscounts.receipt_id = ttReceipts.receipt_id
      .
    /* Løper igjennom feltene i recorden. */
    DISCOUNTBLOKK:  
    DO iLoop2 = 1 TO EXTENT(myLabelsDiscount):
      /* Leser ut felt verdi. */
      cTekst = oJsonObjectDiscount:getJsonText(myLabelsDiscount[iLoop2]).
      CASE myLabelsDiscount[iLoop2]:
        WHEN 'id' THEN 
          ttDiscounts.discounts_id = cTekst.  
        WHEN 'created_at' THEN 
          ttDiscounts.created_at = cTekst. 
        WHEN 'created_by' THEN 
          ttDiscounts.created_by = cTekst. 
        WHEN 'updated_at' THEN 
          ttDiscounts.updated_at = cTekst. 
        WHEN 'deleted_by' THEN 
          ttDiscounts.deleted_by = cTekst. 
        WHEN 'deleted_at' THEN 
          ttDiscounts.deleted_at = cTekst. 
        WHEN 'campaign_id' THEN 
          ttDiscounts.campaign_id = cTekst. 
        WHEN 'active' THEN 
          ttDiscounts.bActive = IF cTekst = 'true' THEN TRUE ELSE FALSE.
        WHEN 'private' THEN 
          ttDiscounts.bPrivate =  IF cTekst = 'true' THEN TRUE ELSE FALSE.
        WHEN 'updated_by' THEN 
          ttDiscounts.updated_by = cTekst.
        WHEN 'type' THEN 
          ttDiscounts.cType = cTekst.
        WHEN 'name' THEN 
          ttDiscounts.cName = cTekst.
        WHEN 'receipt_text' THEN 
          ttDiscounts.receipt_text = cTekst.
        WHEN 'visible_from' THEN 
          ttDiscounts.visible_from = cTekst.
        WHEN 'description' THEN 
          ttDiscounts.cDescription = cTekst.  
        WHEN 'requirement' THEN 
          REQUIRMENTBLOKK:
          DO:
            oJsonObjectRequirement = oJsonObjectDiscount:GetJsonObject('requirement').
            myLabelsRequirement = oJsonObjectRequirement:GetNames().
            CREATE ttRequirement.
            ASSIGN 
              ttRequirement.receipt_id   = ttReceipts.receipt_id
              ttRequirement.discounts_id = ttDiscounts.discounts_id 
              .              
            DO iLoop3 = 1 TO EXTENT(myLabelsRequirement):
              cTekst = oJsonObjectRequirement:getJsonText(myLabelsRequirement[iLoop3]).
              CASE myLabelsRequirement[iLoop3]:
                WHEN 'purchase_to' THEN 
                  ttRequirement.purchase_to = cTekst.
                WHEN 'purchase_from' THEN 
                  ttRequirement.purchase_from = cTekst.
              END CASE.  
            END.
          END. /* REQUIRMENTBLOKK */  
        WHEN 'limitation' THEN 
          LIMITATIONBLOKK:
          DO:
            oJsonObjectLimitation = oJsonObjectDiscount:GetJsonObject('limitation').
            myLabelsLimitation = oJsonObjectLimitation:GetNames().
            CREATE ttLimitation.
            ASSIGN 
              ttLimitation.receipt_id   = ttReceipts.receipt_id
              ttLimitation.discounts_id = ttDiscounts.discounts_id 
              .              
            DO iLoop3 = 1 TO EXTENT(myLabelsLimitation):
              cTekst = oJsonObjectLimitation:getJsonText(myLabelsLimitation[iLoop3]).
              CASE myLabelsLimitation[iLoop3]:
                WHEN 'discount_repeat_usage' THEN 
                  ttLimitation.discount_repeat_usage = INTEGER(cTekst).  
              END CASE.  
            END.
          END. /* LIMITATIONBLOKK */  
        WHEN 'reward' THEN 
          REWARDBLOKK:
          DO:
            oJsonObjectReward = oJsonObjectDiscount:GetJsonObject('reward').
            myLabelsReward = oJsonObjectReward:GetNames().
            CREATE ttReward.
            ASSIGN 
              ttReward.receipt_id   = ttDiscounts.receipt_id
              ttReward.discounts_id = ttDiscounts.discounts_id 
              .              
            DO iLoop3 = 1 TO EXTENT(myLabelsReward):
              cTekst = oJsonObjectReward:getJsonText(myLabelsReward[iLoop3]).
              CASE myLabelsReward[iLoop3]:
                WHEN 'type' THEN 
                  ttReward.cType    = cTekst.
                WHEN 'value' THEN 
                  ttReward.iValue = INTEGER(cTekst).
              END CASE.  
            END.
          END. /* REWARDBLOKK */  
        WHEN 'refs' THEN
          REFSBLOKK: 
          DO:
            oJsonArrayRefs = oJsonObjectDiscount:GetJsonArray('refs').
            IF oJsonArrayRefs:LENGTH > 0 THEN 
            DO:
              DO iLoop3 = 1 TO oJsonArrayRefs:LENGTH:
                oJsonObjectRefs = oJsonArrayRefs:GetJsonObject(iLoop3).

                myLabelsRefs = oJsonObjectRefs:GetNames().
                CREATE ttRefs.
                ASSIGN 
                  ttRefs.receipt_id   = ttReceipts.receipt_id
                  ttRefs.discounts_id = ttDiscounts.discounts_id 
                  .              
                DO iLoop4 = 1 TO EXTENT(myLabelsRefs):
                  cTekst = oJsonObjectRefs:getJsonText(myLabelsRefs[iLoop4]).
                  CASE myLabelsRefs[iLoop4]:
                    WHEN 'id' THEN 
                      ttRefs.refs_id = cTekst.
                    WHEN 'usage' THEN 
                      ttRefs.usage = INTEGER(cTekst).
                    WHEN 'amount' THEN 
                      ttRefs.amount = INTEGER(cTekst).
                    WHEN 'items' THEN
                      DO:
                        oJsonArrayDiscountItems = oJsonObjectRefs:GetJsonArray('items').
                        IF oJsonArrayDiscountItems:LENGTH > 0 THEN
                        DO iLoop5 = 1 TO oJsonArrayDiscountItems:LENGTH: 
                          CREATE ttDiscountItems.
                          ASSIGN 
                            ttDiscountItems.receipt_id   = ttRefs.receipt_id
                            ttDiscountItems.discounts_id = ttRefs.discounts_id 
                            ttDiscountItems.refs_id      = ttRefs.refs_id 
                            .
                          oJsonObjectDiscountItems = oJsonArrayDiscountItems:GetJsonObject(iLoop5).
                          cTekst = oJsonObjectDiscountItems:getJsonText('line_id').
                          ttDiscountItems.line_id = INTEGER(cTekst). 
                          cTekst = oJsonObjectDiscountItems:getJsonText('amount').
                          ttDiscountItems.amount = INTEGER(cTekst). 
                        END.
                      END.
                    WHEN 'statistics' THEN
                      DO:
                        CREATE ttStatistics.
                        ASSIGN 
                          ttStatistics.receipt_id   = ttRefs.receipt_id
                          ttStatistics.discounts_id = ttRefs.discounts_id 
                          ttStatistics.refs_id      = ttRefs.refs_id 
                          .
                        oJsonObjectstatistics = oJsonObjectRefs:GetJsonObject('statistics').
                        cTekst = oJsonObjectstatistics:getJsonText('usage').
                        ttStatistics.usage = INTEGER(cTekst). 
                        cTekst = oJsonObjectstatistics:getJsonText('amount').
                        ttStatistics.amount = INTEGER(cTekst). 
                      END.
                  END CASE.  
                END.
              END.
            END.
          END. /* REFSBLOKK */  
      END CASE.
    END. /* DISCOUNTBLOKK */
  END. /* DISCOUNTARRAYBLOKK */
END. /* DISCOUNTSBLOKK */

IF bTest THEN 
DO:
  DATASET dsReceipts:WRITE-JSON('file', 'konv\dsReceipt' + ttReceipts.receipt_id + '.json', TRUE).
  FIND FIRST ttReceipts NO-ERROR.
END.

RETURN.

/* **********************  Internal Procedures  *********************** */

PROCEDURE itemsBlokk:
  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/
  DEFINE VARIABLE myLabelsItems             AS CHARACTER         EXTENT NO-UNDO. /* NB: Ikke angi antall ekstenter. */

  myLabelsItems = oJsonObjectItems:GetNames().

  ITEMSLABELBLOKK:  
  DO iLoop2 = 1 TO EXTENT(myLabelsItems):
    /* Leser ut felt verdi. */
    cTekst = oJsonObjectItems:getJsonText(myLabelsItems[iLoop2]).
      
    CASE myLabelsItems[iLoop2]:
      WHEN 'id' THEN 
        ttItem.item_id = cTekst.
      /*        WHEN 'line_id' THEN ttItem.line_id = INTEGER(cTekst).*/
      WHEN 'unit' THEN 
        ttItem.unit = cTekst.
      WHEN 'description' THEN 
        ttItem.description = cTekst.
      WHEN 'barcode' THEN 
        ttItem.barcode = cTekst.
      WHEN 'comment' THEN 
        ttItem.comment = cTekst.
      WHEN 'description_alias' THEN 
        ttItem.description_alias = cTekst.
      WHEN 'quantity' THEN 
        ttItem.quantity = INTEGER(cTekst).
      WHEN 'net_amount' THEN 
        ttItem.net_amount = INTEGER(cTekst).
      WHEN 'gross_amount' THEN 
        ttItem.gross_amount = INTEGER(cTekst).
      WHEN 'tax_percent' THEN 
        ttItem.tax_percent = INTEGER(cTekst).
      WHEN 'cost_price' THEN 
        ttItem.quantity = INTEGER(cTekst).
      WHEN 'voided' THEN 
        ttItem.voided = IF cTekst = 'true' THEN TRUE ELSE FALSE.
      WHEN 'eligible_for_discount' THEN 
        ttItem.eligible_for_discount = IF cTekst = 'true' THEN TRUE ELSE FALSE.
      WHEN 'included_in_total_discount' THEN 
        ttItem.included_in_total_discount = IF cTekst = 'true' THEN TRUE ELSE FALSE.
      WHEN 'is_return_item' THEN 
        ttItem.is_return_item = IF cTekst = 'true' THEN TRUE ELSE FALSE.
      WHEN 'is_virtual_product' THEN 
        ttItem.is_virtual_product = IF cTekst = 'true' THEN TRUE ELSE FALSE.
      WHEN 'discount_lines' THEN 
        DISCOUNTLINESBLOKK:
        DO:
          oJsonArrayDiscount_lines = oJsonObjectItems:GetJsonArray('discount_lines').
          IF oJsonArrayDiscount_lines:LENGTH > 0 THEN 
          DISCOUNTLINESBLOKK:
          DO iLoop3 = 1 TO oJsonArrayDiscount_lines:LENGTH:
            oJSonObjectDiscount_lines = oJsonArrayDiscount_lines:GetJsonObject(iLoop3).
            myLabelsDiscount_lines = oJsonObjectDiscount_lines:GetNames().
            CREATE ttDiscount_lines.
            ASSIGN 
              ttDiscount_lines.receipt_id = ttItem.receipt_id
              ttDiscount_lines.line_id    = ttItem.line_id
              .

            /* Løper igjennom feltene i recorden. */
            DISCOUNTLINESBLOKK:  
            DO iLoop4 = 1 TO EXTENT(myLabelsDiscount_lines):
              /* Leser ut felt verdi. */
              cTekst = oJSonObjectDiscount_lines:getJsonText(myLabelsDiscount_lines[iLoop4]).
              CASE myLabelsDiscount_lines[iLoop4]:
                WHEN 'line_id' THEN 
                  ttDiscount_lines.discount_line_id = INTEGER(cTekst).
                WHEN 'description' THEN 
                  ttDiscount_lines.description = cTekst.
                WHEN 'discount_id' THEN 
                  ttDiscount_lines.discount_id = cTekst.
                WHEN 'discount_type' THEN 
                  ttDiscount_lines.discount_type = cTekst.
                WHEN 'amount' THEN 
                  ttDiscount_lines.amount = INTEGER(cTekst).
              END CASE.
            END. /* DISCOUNTLINESBLOKK */
          END. /* DISCOUNTLINESBLOKK */            
        END. /* DISCOUNTLINESBLOKK */  
    END CASE.  
  END. /* ITEMSLABELBLOKK */      

END PROCEDURE.


