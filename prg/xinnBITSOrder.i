DEFINE {&NEW} SHARED TEMP-TABLE TT_Order NO-UNDO
    FIELD path AS CHARACTER
    FIELD calias AS CHARACTER
    FIELD customer AS CHARACTER
    FIELD KundeNr AS CHARACTER
    FIELD OrdreNr AS CHARACTER 
    FIELD cuser AS CHARACTER
    FIELD creationDate AS CHARACTER
    FIELD viewedOn AS CHARACTER
    FIELD cancelledOn AS CHARACTER
    FIELD inProcessOn AS CHARACTER
    FIELD pendingOn AS CHARACTER
    FIELD readyForShippingOn AS CHARACTER
    FIELD partlyDispatchedOn AS CHARACTER
    FIELD dispatchedOn AS CHARACTER
    FIELD shippedOn AS CHARACTER
    FIELD partlyPaidOn AS CHARACTER
    FIELD paidOn AS CHARACTER
    FIELD closedOn AS CHARACTER
    FIELD archivedOn AS CHARACTER
    FIELD partlyInvoicedOn AS CHARACTER
    FIELD invoicedOn AS CHARACTER
	FIELD internalComment AS CHARACTER 
	FIELD customerComment AS CHARACTER 
    INDEX  OrderSeq IS PRIMARY KundeNr OrdreNr. 

/* Denne inneholder også productLineItem, taxlinjeItem */ 
DEFINE {&NEW} SHARED TEMP-TABLE TT_lineItemContainer NO-UNDO
    FIELD localeID AS CHARACTER
    FIELD languageCode AS CHARACTER
    FIELD currencyID AS CHARACTER
    FIELD taxArea AS CHARACTER
    FIELD taxAreaName AS CHARACTER
    FIELD taxModel AS CHARACTER
    FIELD grandTotal AS CHARACTER
    FIELD totalBeforeTax AS CHARACTER
    FIELD totalTax AS CHARACTER
    INDEX  lineItemContainerSeq IS PRIMARY localeID. 

/* Det kan komme flere productLineItem */
DEFINE {&NEW} SHARED TEMP-TABLE TT_productLineItem NO-UNDO
    FIELD name AS CHARACTER
    FIELD SKU AS CHARACTER
    FIELD Artikkelnr AS CHARACTER
    FIELD Modellfarge AS CHARACTER 
    FIELD product AS CHARACTER 
    FIELD StrKode AS CHARACTER
    FIELD taxClass AS CHARACTER
    FIELD lineItemPrice AS CHARACTER
    FIELD basePrice AS CHARACTER
    FIELD quantity AS CHARACTER
    FIELD discount AS CHARACTER
    FIELD taxRate AS CHARACTER
    FIELD taxAmount AS CHARACTER
    FIELD SeqNr AS INTEGER 
    INDEX  productLineItemSeq IS PRIMARY ArtikkelNr StrKode. 

/* Det kan komme mer enn en taxlinjeItem. */
DEFINE {&NEW} SHARED TEMP-TABLE TT_taxLineItem NO-UNDO
    FIELD name AS CHARACTER
    FIELD taxArea AS CHARACTER
    FIELD taxMatrix AS CHARACTER
    FIELD taxClass AS CHARACTER
    FIELD MomsKod AS CHARACTER
    FIELD lineItemPrice AS CHARACTER
    FIELD basePrice AS CHARACTER
    FIELD quantity AS CHARACTER
    FIELD discount AS CHARACTER
    FIELD taxRate AS CHARACTER
    FIELD taxAmount AS CHARACTER
    INDEX  taxLineItemSeq IS PRIMARY Name. 
    
/* Det kommer bare en PaymentLineItem */
DEFINE {&NEW} SHARED TEMP-TABLE TT_paymentLineItem NO-UNDO
    FIELD name AS CHARACTER
    FIELD paymentMethod AS CHARACTER
    FIELD paymentType AS CHARACTER
    FIELD taxClass AS CHARACTER
    FIELD lineItemPrice AS CHARACTER
    FIELD basePrice AS CHARACTER
    FIELD quantity AS CHARACTER
    FIELD discount AS CHARACTER
    FIELD taxRate AS CHARACTER
    FIELD taxAmount AS CHARACTER
    FIELD bbsEpayment AS CHARACTER 
    INDEX  paymentLineItemSeq IS PRIMARY Name. 

/* Det kommer bare en PaymentLineItem */
DEFINE {&NEW} SHARED TEMP-TABLE TT_shippingLineItem NO-UNDO
    FIELD name AS CHARACTER
    FIELD shippingMethod AS CHARACTER
    FIELD lineItemPrice AS CHARACTER
    FIELD basePrice AS CHARACTER
    FIELD quantity AS CHARACTER
    FIELD discount AS CHARACTER
    FIELD taxRate AS CHARACTER
    FIELD taxAmount AS CHARACTER
    INDEX  shippingLineItemSeq IS PRIMARY Name. 


DEFINE {&NEW} SHARED TEMP-TABLE TT_BillingAddress NO-UNDO
    FIELD Oppdatert AS LOG 
    FIELD cAlias AS CHARACTER
    FIELD KundeNr AS CHARACTER  
    FIELD displayName AS CHARACTER
    FIELD addressExtension AS CHARACTER  
    FIELD street AS CHARACTER 
    FIELD zipcode AS CHARACTER 
    FIELD city AS CHARACTER 
    FIELD state AS CHARACTER 
    FIELD countryID AS CHARACTER 
    FIELD EMail AS CHARACTER 
    FIELD phone AS CHARACTER 
    FIELD fax AS CHARACTER 
    FIELD salutation AS CHARACTER 
    FIELD cTitle AS CHARACTER 
    FIELD firstName AS CHARACTER 
    FIELD middleName AS CHARACTER 
    FIELD lastName AS CHARACTER 
    FIELD EMailPrivate AS CHARACTER 
    FIELD EMailBusiness AS CHARACTER 
    FIELD phonePrivate AS CHARACTER 
    FIELD phoneBusiness AS CHARACTER 
    FIELD phoneCell AS CHARACTER 
    FIELD gender AS CHARACTER 
    FIELD company AS CHARACTER 
    FIELD department AS CHARACTER 
    FIELD jobTitle AS CHARACTER 
    FIELD birthday AS CHARACTER 
    FIELD VATID AS CHARACTER 
    FIELD bankCode AS CHARACTER 
    FIELD bankName AS CHARACTER 
    FIELD bankAccountNo AS CHARACTER 
    FIELD cURL AS CHARACTER 
    INDEX  BillingAdressSeq IS PRIMARY KundeNr Street.       
      
DEFINE {&NEW} SHARED TEMP-TABLE TT_ShippingAdress NO-UNDO
    FIELD Oppdatert AS LOG 
    FIELD cAlias AS CHARACTER
    FIELD KundeNr AS CHARACTER  
    FIELD displayName AS CHARACTER
    FIELD addressExtension AS CHARACTER  
    FIELD street AS CHARACTER 
    FIELD zipcode AS CHARACTER 
    FIELD city AS CHARACTER 
    FIELD state AS CHARACTER 
    FIELD countryID AS CHARACTER 
    FIELD EMail AS CHARACTER 
    FIELD phone AS CHARACTER 
    FIELD fax AS CHARACTER 
    FIELD salutation AS CHARACTER 
    FIELD cTitle AS CHARACTER 
    FIELD firstName AS CHARACTER 
    FIELD middleName AS CHARACTER 
    FIELD lastName AS CHARACTER 
    FIELD EMailPrivate AS CHARACTER 
    FIELD EMailBusiness AS CHARACTER 
    FIELD phonePrivate AS CHARACTER 
    FIELD phoneBusiness AS CHARACTER 
    FIELD phoneCell AS CHARACTER 
    FIELD gender AS CHARACTER 
    FIELD company AS CHARACTER 
    FIELD department AS CHARACTER 
    FIELD jobTitle AS CHARACTER 
    FIELD birthday AS CHARACTER 
    FIELD VATID AS CHARACTER 
    FIELD bankCode AS CHARACTER 
    FIELD bankName AS CHARACTER 
    FIELD bankAccountNo AS CHARACTER 
    FIELD URL AS CHARACTER 
    INDEX shippingAdressSeq IS PRIMARY KundeNr Street.

