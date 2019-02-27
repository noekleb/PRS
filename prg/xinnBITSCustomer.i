DEFINE {&NEW} SHARED TEMP-TABLE TT_Customer NO-UNDO
    FIELD Oppdatert AS LOG 
    FIELD cPath AS CHARACTER
    FIELD cAlias AS CHARACTER 
    FIELD KundeNr AS CHARACTER 
    FIELD customerGroup AS CHARACTER 
    FIELD taxArea AS CHARACTER 
    FIELD taxModel AS CHARACTER 
    FIELD isHtmlEMailAllowed AS CHARACTER 
    FIELD isDoOrderAllowed AS CHARACTER 
    FIELD comment AS CHARACTER
    INDEX CustomerSeq IS PRIMARY UNIQUE KundeNr.
    
DEFINE {&NEW} SHARED TEMP-TABLE TT_User NO-UNDO
    FIELD Oppdatert AS LOG 
	FIELD cPath AS CHARACTER 
	FIELD cAlias AS CHARACTER 
	FIELD KundeNr AS CHARACTER 
	FIELD BrukerId AS CHARACTER 
	FIELD localeID AS CHARACTER 
	FIELD LanguageCode AS CHARACTER 
	FIELD currencyID AS CHARACTER  
	FIELD isActivated AS CHARACTER 
	FIELD deleteConfirmation AS CHARACTER 
	FIELD isLoginCookieAllowed AS CHARACTER 
	FIELD isHtmlEMailAllowed AS CHARACTER 
	FIELD reminderQuestion AS CHARACTER 
	FIELD challengePhrase AS CHARACTER 
	FIELD EMail AS CHARACTER 
	FIELD UserName AS CHARACTER 
    INDEX CustomerSeq IS PRIMARY UNIQUE KundeNr BrukerId.   

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
    FIELD MottaeMailUtsendelser AS CHARACTER 
    FIELD MedlemsKlubb AS CHARACTER 
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

DEFINE {&NEW} SHARED TEMP-TABLE TT_UserBillingAddress NO-UNDO
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
      
DEFINE {&NEW} SHARED TEMP-TABLE TT_UserShippingAdress NO-UNDO
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
    INDEX shippingAdressSeq IS PRIMARY KundeNr Street.
      