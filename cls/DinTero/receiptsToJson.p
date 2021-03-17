
/*------------------------------------------------------------------------
    File        : receiptsToJson.p
    Purpose     : 

    Syntax      :

    Description : Legger datasettet dsRecipts over i en JsonArray.

    Author(s)   : Tom Nøkleby
    Created     : Sun Jan 24 13:00:11 CET 2021
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

USING Progress.Json.ObjectModel.ObjectModelParser.
USING Progress.Json.ObjectModel.JsonObject.
USING Progress.Json.ObjectModel.JsonArray.

{cls\dintero\ttCustomerObj.i}
{cls\dintero\dsCustomerObj.i}
{cls\dintero\ttReceipts.i}
{cls\dintero\dsReceipts.i}

DEFINE INPUT  PARAMETER DATASET FOR dsReceipts.
DEFINE INPUT  PARAMETER DATASET FOR dsCustCustomer.
DEFINE OUTPUT PARAMETER poReceipArray AS JSonArray NO-UNDO.

DEFINE VARIABLE poStore AS JsonObject NO-UNDO.
DEFINE VARIABLE poStoreAdress AS JSonObject NO-UNDO.
DEFINE VARIABLE poReceipt AS JsonObject NO-UNDO.
DEFINE VARIABLE poCustCustomer AS JsonObject NO-UNDO.
DEFINE VARIABLE poCustAddresses AS JsonObject NO-UNDO.
DEFINE VARIABLE poCustAddressesArray AS JsonArray NO-UNDO.
DEFINE VARIABLE poTax_Lines AS JsonObject NO-UNDO.
DEFINE VARIABLE poTax_LinesArray AS JsonArray NO-UNDO.
DEFINE VARIABLE poExtra_info AS JsonObject NO-UNDO.
DEFINE VARIABLE poExtra_infoArray AS JsonArray NO-UNDO.
DEFINE VARIABLE poItem AS JsonObject NO-UNDO.
DEFINE VARIABLE poItemGroup AS JsonObject NO-UNDO.
DEFINE VARIABLE poItemGroupArray AS JsonArray NO-UNDO.
DEFINE VARIABLE poItemArray AS JsonArray NO-UNDO.
DEFINE VARIABLE poPayMent AS JsonObject NO-UNDO.
DEFINE VARIABLE poPaymentArray AS JsonArray NO-UNDO.
DEFINE VARIABLE poInfoArray AS JsonArray NO-UNDO.
DEFINE VARIABLE poItemDimension AS JsonObject NO-UNDO.
DEFINE VARIABLE poItemTax_Lines AS JsonObject NO-UNDO.
DEFINE VARIABLE poItemTax_LinesArray AS JsonArray NO-UNDO.
DEFINE VARIABLE poDiscountsArray AS JsonArray NO-UNDO.
DEFINE VARIABLE poDiscountsLinesArray AS JsonArray NO-UNDO.
DEFINE VARIABLE poCard_Info AS JsonObject NO-UNDO.
DEFINE VARIABLE myParser  AS ObjectModelParser NO-UNDO.

DEFINE VARIABLE lcLongChar AS LONGCHAR NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
myParser = NEW ObjectModelParser().

/* Prepper variabel som skal ha JSon meldingen. */
DEFINE VARIABLE pcJSonString AS LONGCHAR NO-UNDO.
FIX-CODEPAGE(pcJSonString)  = 'UTF-8'.
FIX-CODEPAGE(lcLongChar)  = 'UTF-8'.

/* Leser alle bonger i datasettet. */
LESDATASETT:
FOR EACH ttReceipts:
  RECEIPTSBLOKK:
  DO: 
    poReceipArray = NEW JSonArray().
    
    /* Oppretter objectet for bongen. */
    poReceipt      = NEW JsonObject().
  
    /* Legger inn feltene fra bongen. */
    poReceipt:ADD("is_changed", ttReceipts.is_changed).
    poReceipt:ADD("receipt_id", ttReceipts.receipt_id).
    poReceipt:ADD("round_off_to_coin", ttReceipts.round_off_to_coin).
    poReceipt:ADD("currency", ttReceipts.currency).
    poReceipt:ADD("purchase_at", ttReceipts.purchase_at).
    poReceipt:ADD("no_of_items", ttReceipts.no_of_items).
    poReceipt:ADD("total_discount", ttReceipts.total_discount).
    poReceipt:ADD("customer_id", ttReceipts.customer_id).
    poReceipt:ADD("operator_id", ttReceipts.operator_id).
    poReceipt:ADD("operator_name", ttReceipts.operator_name).
    poReceipt:ADD("salesperson_id", ttReceipts.salesperson_id).
    poReceipt:ADD("salesperson_name", ttReceipts.salesperson_name).
    poReceipt:ADD("comment", ttReceipts.comment).
    poReceipt:ADD("gross_amount", ttReceipts.gross_amount).
    poReceipt:ADD("net_amount", ttReceipts.net_amount).
    /* Legger på discounts hvis dette er lagt inn. */
    IF LENGTH(ttReceipts.lcDiscounts) > 1 THEN 
      DO:
        /* Legger inn discount objectet på bongen. */
        lcLongChar = ttReceipts.lcDiscounts.
        poDiscountsArray = CAST(myParser:Parse(lcLongChar), JsonArray).
        poReceipt:ADD("discounts", poDiscountsArray).
      END.
  END. /* RECEIPTSBLOKK */

  FIND FIRST ttStore WHERE 
    ttStore.receipt_id = ttReceipts.receipt_id NO-ERROR.    
  IF AVAILABLE ttStore THEN 
    STOREBLOKK:
    DO:
      poStore = NEW JsonObject().

      poStore:ADD("id", ttStore.id).
      poStore:ADD("name", ttStore.cName).
      poStore:ADD("organization_number", ttStore.organization_number).
            
      /* Legger adressen på butikken. */
      FIND FIRST ttStoreAdress WHERE 
        ttStoreAdress.receipt_id = ttStore.receipt_id AND 
        ttStoreAdress.store_id = ttStore.Id NO-ERROR.
      IF AVAILABLE ttStoreAdress THEN
        STOREADRESSBLOKK: 
        DO:
          poStoreAdress = NEW JsonObject().
          
          poStoreAdress:ADD("address_line", ttStoreAdress.address_line).
          poStoreAdress:ADD("address_line_2", ttStoreAdress.address_line_2).
          poStoreAdress:ADD("postal_code", ttStoreAdress.postal_code).
          poStoreAdress:ADD("postal_place", ttStoreAdress.postal_place).
          poStoreAdress:ADD("country", ttStoreAdress.country).
          
          poStore:ADD("adress", poStoreAdress).
        END. /* STOREADRESSBLOKK */
        
      poReceipt:ADD("store", poStore).
        
    END. /* STOREBLOKK */
    
  FIND FIRST ttCustCustomer WHERE 
    ttCustCustomer.customer_id = ttReceipts.customer_id NO-ERROR.
  IF AVAILABLE ttCustCustomer THEN
    CUSTOMERBLOKK: 
    DO:
      poCustCustomer = NEW JsonObject().
      
      poCustCustomer:ADD("customer_id", ttCustCustomer.customer_id).
      poCustCustomer:ADD("type", ttCustCustomer.custType).
      IF ttCustCustomer.date_of_birth <> '' THEN 
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
          poCustCustomer:ADD("addresses",poCustAddressesArray).
        END.
            
      poReceipt:ADD("customer", poCustCustomer).      
    END. /* CUSTOMERBLOKK */ 

  FIND FIRST ttTax_Lines WHERE 
    ttTax_Lines.receipt_id = ttReceipts.receipt_id NO-ERROR.
  IF AVAILABLE ttTax_Lines THEN 
    TAX_LINESBLOKK:
    DO:
      poTax_LinesArray = NEW JsonArray().
 
      FOR EACH ttTax_Lines WHERE 
        ttTax_Lines.receipt_id = ttReceipts.receipt_id:
        poTax_Lines = NEW JsonObject().
        
        poTax_Lines:ADD("receipt_id", ttTax_Lines.receipt_id).
        poTax_Lines:ADD("tax_code", ttTax_Lines.tax_code).
        poTax_Lines:ADD("amount", ttTax_Lines.amount).
        poTax_Lines:ADD("exempt", ttTax_Lines.exempt).
        poTax_Lines:ADD("included_in_price", ttTax_Lines.included_in_price).
        poTax_Lines:ADD("percentage", ttTax_Lines.percentage).
        poTax_Lines:ADD("tax_basis", ttTax_Lines.tax_basis).
        poTax_Lines:ADD("tax_group", ttTax_Lines.tax_group).
        
        poTax_LinesArray:ADD(poTax_Lines).
      END.  

       /* Legger Array inn på bongen. */
      poReceipt:ADD("tax_lines", poTax_LinesArray).
    END. /* TAX_LINESBLOKK */    

    FIND FIRST ttItem NO-ERROR.
    IF AVAILABLE ttItem THEN 
      DO:
        poItemArray = NEW JsonArray().
        
        /* Legger opp varelinjene. */  
        VARELINJEBLOKK:
        FOR EACH ttItem WHERE 
          ttItem.receipt_Id = ttReceipts.receipt_Id:
    
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
          /* Legger på discounts hvis dette er lagt inn. */
          IF LENGTH(ttItem.lcDiscount_lines) > 1 THEN 
            DO:
              lcLongChar = ttItem.lcDiscount_lines.
              poDiscountsLinesArray = CAST(myParser:Parse(lcLongChar), JsonArray).
              poItem:ADD("discount_lines", poDiscountsLinesArray).
            END.
          
          /* Legger på farge og størrelse. */
          FIND FIRST ttItemDimension WHERE
            ttItemDimension.receipt_id = ttItem.receipt_id AND
            ttItemDimension.line_id    = ttItem.line_id NO-ERROR.
          IF AVAILABLE ttItemDimension THEN
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
          FIND FIRST ttItemGroups WHERE 
            ttItemGroups.receipt_id = ttItem.receipt_id AND   
            ttItemGroups.line_id    = ttItem.line_id NO-ERROR.
          IF AVAILABLE ttItemGroups THEN 
            DO:
              poItemGroupArray = NEW JsonArray().
              
              FOR EACH ttItemGroups WHERE 
                ttItemGroups.receipt_id = ttItem.receipt_id AND 
                ttItemGroups.line_id    = ttItem.line_id:
                  
                poItemGroup = NEW JsonObject().
                
                poItemGroup:ADD("id", ttItemGroups.group_id).
                poItemGroup:ADD("name", ttItemGroups.group_name).
                
                poItemGroupArray:ADD(poItemGroup).
              END.
              
              /* Legger groups array'en inn på item objectet. */
              poItem:ADD('groups',poItemGroupArray).
              
            END.
            
          /* Legger på tax lines. */
          FIND FIRST ttItemTax_Lines WHERE
            ttItemTax_Lines.receipt_id = ttItem.receipt_id AND
            ttItemTax_Lines.Line_Id = ttItem.line_id NO-ERROR.
          IF AVAILABLE ttItemTax_Lines THEN 
            DO:   
              poItemTax_LinesArray = NEW JsonArray().
              
              FOR EACH ttItemTax_Lines WHERE 
                ttItemTax_Lines.receipt_id = ttItem.receipt_id AND
                ttItemTax_Lines.Line_Id = ttItem.line_id:
                  
                poItemTax_Lines = NEW JsonObject().

                poItemTax_Lines:ADD("tax_code", ttItemTax_Lines.tax_code).
                  
                poItemTax_Lines:ADD("amount", ttItemTax_Lines.amount). 
                poItemTax_Lines:ADD("exempt", ttItemTax_Lines.exempt). 
                poItemTax_Lines:ADD("included_in_price", ttItemTax_Lines.included_in_price).
                poItemTax_Lines:ADD("percentage", ttItemTax_Lines.percentage). 
                poItemTax_Lines:ADD("tax_basis", ttItemTax_Lines.tax_basis). 
                poItemTax_Lines:ADD("tax_group", ttItemTax_Lines.tax_group). 
                  
                poItemTax_LinesArray:ADD(poItemTax_Lines).
              END.
              
              poItem:ADD('tax_lines',poItemTax_LinesArray).
            END.
            
          /* Legger item objectet inn i item array'en. */
          poItemArray:Add(poItem).            
        END. /* VARELINJEBLOKK */
       /* Legger Array inn på bongen. */
        poReceipt:ADD("items", poItemArray).
      END.
    
    FIND FIRST ttPayment WHERE 
      ttPayment.receipt_id = ttReceipts.receipt_id NO-ERROR.
    IF AVAILABLE ttPayment THEN                  
    PAYMENTBLOKK:
    DO:
      poPaymentArray = NEW JsonArray().
      
      FOR EACH ttPayMent WHERE 
        ttPayment.receipt_id = ttReceipts.receipt_id:
        poPayMent = NEW JsonObject().

/*        poPayMent:ADD("receipt_id", ttPayment.receipt_id).*/
        poPayMent:ADD("line_id", ttPayment.line_id).
        poPayMent:ADD("amount", ttPayment.amount).
        poPayMent:ADD("voided", ttPayment.voided).
        poPayMent:ADD("description", ttPayment.cDescription).

        FOR EACH ttCard_Info WHERE 
          ttCard_Info.receipt_id = ttReceipts.receipt_id AND 
          ttCard_Info.line_id = ttPayMent.line_id:
            
          poCard_Info = NEW JsonObject().
            
          poCard_Info:ADD("balance_amount", ttCard_Info.balance_amount).
          poCard_Info:ADD("card_amount", ttCard_Info.card_amount).
          poCard_Info:ADD("issuer_id", ttCard_Info.issuer_id).
          poCard_Info:ADD("issuer_name", ttCard_Info.issuer_name).
          poCard_Info:ADD("session_id", ttCard_Info.session_id).
          poCard_Info:ADD("terminal_id", ttCard_Info.terminal_id).
          poCard_Info:ADD("card_number", ttCard_Info.card_number).
          poCard_Info:ADD("card_type", ttCard_Info.card_type).
          poCard_Info:ADD("receipt", ttCard_Info.receipt).
            
          poPayMent:ADD("card_info", poCard_Info).
        END.
          
        poPaymentArray:ADD(poPayMent).
      END.
        
       /* Legger Array inn på bongen. */
      poReceipt:ADD("payments", poPaymentArray).
    END. /* PAYMENTBLOKK*/ 

  /* Legger opp info linjene. */
  FIND FIRST ttExtra_info WHERE 
    ttExtra_info.receipt_id = ttReceipts.receipt_id NO-ERROR.  
  IF AVAILABLE ttExtra_info THEN
    INFOLINJEBLOKK: 
    DO:
      poExtra_infoArray = NEW JsonArray().

      FOR EACH ttExtra_info WHERE 
        ttExtra_info.receipt_id = ttReceipts.receipt_id:
        poExtra_info = NEW JsonObject().
            
/*        poExtra_info:ADD("receipt_id", ttExtra_info.receipt_id).*/
/*        poExtra_info:ADD("line_id", ttExtra_info.line_id).      */
        poExtra_info:ADD("key", ttExtra_info.cKey).
        poExtra_info:ADD("value", ttExtra_info.cValue).
        poExtra_info:ADD("value_type", ttExtra_info.value_type).

        poExtra_infoArray:ADD(poExtra_info).
      END.
      
       /* Legger Array inn på bongen. */
      poReceipt:ADD("Extra_info", poExtra_infoArray).
    END. /* INFOLINJEBLOKK */

  /* Legger bongen til Array'en. */    
  poReceipArray:ADD(poReceipt).
END.

        