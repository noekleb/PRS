
/*------------------------------------------------------------------------
    File        : ttCustomerObj.i
    Purpose     : Ved å legge inn denne includen, har proceduren tilgang til alle tabeller som CustomerObjectet består av.  

    Syntax      :

    Description : Definisjon av alle tabeller i Customer objectet.

    Author(s)   : Tom Nøkleby
    Created     : Sun Nov 01 04:52:05 CET 2020
    Notes       :
      
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttCustCustomer NO-UNDO SERIALIZE-NAME 'customer'
  FIELD UUid AS CHARACTER  FORMAT "x(40)" SERIALIZE-NAME 'id' 
  FIELD customer_id AS CHARACTER FORMAT "x(40)" 
  FIELD custType AS CHARACTER FORMAT "x20" SERIALIZE-NAME 'type'
  FIELD gender AS CHARACTER FORMAT "x20"
  FIELD date_of_birth AS CHARACTER /* YMD */
  FIELD created_at AS DATETIME  
  FIELD created_by AS DATETIME 
  FIELD updated_at AS DATETIME 
  FIELD deleted_by AS CHARACTER FORMAT "x(40)"
  FIELD deleted_at AS DATETIME
  FIELD first_name AS CHARACTER FORMAT "x(30)"
  FIELD last_name AS CHARACTER FORMAT "x(40)"
  FIELD email AS CHARACTER FORMAT "x(30)"
  FIELD phone_number AS CHARACTER FORMAT "x(20)"
  FIELD custStatus AS CHARACTER FORMAT "x(15)" SERIALIZE-NAME 'status'
  FIELD favorite_store AS CHARACTER FORMAT "x(10)"
  INDEX idxttCustCustomer AS PRIMARY UNIQUE customer_id
  . 

DEFINE TEMP-TABLE ttCustMetadata NO-UNDO SERIALIZE-NAME 'metadata'
  FIELD customer_id AS CHARACTER FORMAT "x(40)" SERIALIZE-HIDDEN 
  FIELD dob_year AS INTEGER FORMAT "9999"
  INDEX idxttCustMetadata customer_id 
  .
  
DEFINE TEMP-TABLE ttCustAttributes NO-UNDO SERIALIZE-NAME 'attributes'
  FIELD customer_id AS CHARACTER FORMAT "x(40)" SERIALIZE-HIDDEN 
  FIELD EksterntKundeNr AS CHARACTER FORMAT "x(30)"
  FIELD MedlemsNr AS CHARACTER FORMAT "x(20)" 
  INDEX idxttCustAttributes AS PRIMARY customer_id 
  .    

DEFINE TEMP-TABLE ttCustEnrolled_by NO-UNDO SERIALIZE-NAME 'enrolled_by'
  FIELD customer_id AS CHARACTER FORMAT "x(40)" SERIALIZE-HIDDEN
  FIELD enrType AS CHARACTER FORMAT "x(20)" SERIALIZE-NAME 'type'
  FIELD enrValue AS CHARACTER FORMAT "x(20)" SERIALIZE-NAME 'value'
  INDEX idxCustEnrolled_by AS PRIMARY UNIQUE customer_id enrType 
  .

DEFINE TEMP-TABLE ttCustMarketing_consent NO-UNDO SERIALIZE-NAME 'marketing_consent'
  FIELD customer_id AS CHARACTER FORMAT "x(40)" SERIALIZE-HIDDEN
  FIELD consent_id AS CHARACTER FORMAT "x(10)"
  FIELD consent AS LOG 
  INDEX idxCustMarketing_consent AS PRIMARY UNIQUE customer_id consent_id
  .
    
DEFINE TEMP-TABLE ttCustSms NO-UNDO SERIALIZE-NAME 'sms'
  FIELD customer_id AS CHARACTER FORMAT "x(40)" SERIALIZE-HIDDEN
  FIELD consent_id AS CHARACTER FORMAT "x(10)" SERIALIZE-HIDDEN
  FIELD consent AS LOG FORMAT "true/false"
  FIELD updated_at AS DATETIME 
  INDEX idxCustSms AS PRIMARY customer_id consent_id
  .
       
DEFINE TEMP-TABLE ttCustEMail NO-UNDO SERIALIZE-NAME 'email'
  FIELD customer_id AS CHARACTER FORMAT "x(40)" SERIALIZE-HIDDEN
  FIELD consent_id AS CHARACTER FORMAT "x(10)" SERIALIZE-HIDDEN
  FIELD consent AS LOG FORMAT "true/false"
  FIELD updated_at AS DATETIME 
  INDEX idxCustEMail AS PRIMARY customer_id consent_id
  .
    
DEFINE TEMP-TABLE ttCustAddresses NO-UNDO SERIALIZE-NAME 'addresses'
  FIELD customer_id AS CHARACTER FORMAT "x(40)" SERIALIZE-HIDDEN
  FIELD addrType AS CHARACTER FORMAT "x(20)" SERIALIZE-NAME 'type'
  FIELD address_line AS CHARACTER FORMAT "x(40)"
  FIELD address_line_2 AS CHARACTER FORMAT "x(40)"
  FIELD postal_code AS CHARACTER FORMAT "x(10)"
  FIELD postal_place AS CHARACTER FORMAT "x(30)"
  FIELD country AS CHARACTER FORMAT "x(10)"
  FIELD latitude AS DEC FORMAT "99.999999"
  FIELD longitude AS DEC FORMAT "99.999999"
  FIELD custom_type AS CHARACTER FORMAT "x(20)"
  FIELD comment AS CHARACTER FORMAT "x(40)"
  INDEX idxCustAddresses AS PRIMARY UNIQUE customer_id addrType
  . 

DEFINE TEMP-TABLE ttCustTerm NO-UNDO SERIALIZE-NAME 'term'
  FIELD customer_id AS CHARACTER FORMAT "x(40)" SERIALIZE-HIDDEN
  FIELD id AS CHARACTER FORMAT "x(20)"
  FIELD accepted_at AS DATETIME 
  INDEX idxCustTerm AS PRIMARY UNIQUE customer_id id 
  .  

DEFINE TEMP-TABLE ttCustCompany NO-UNDO SERIALIZE-NAME 'company'
  FIELD customer_id AS CHARACTER FORMAT "x(40)" SERIALIZE-HIDDEN
  FIELD bussiness_name AS CHARACTER FORMAT "x(40)"
  FIELD organization_number AS CHARACTER FORMAT "x(20)"
  FIELD department AS CHARACTER FORMAT "x(20)"
  FIELD industry AS CHARACTER FORMAT "x(20)"
  FIELD website AS CHARACTER FORMAT "x(40)"
  FIELD number_of_employees AS CHARACTER FORMAT "x(10)"
  INDEX idxCustCompany AS PRIMARY UNIQUE customer_id bussiness_name
  .  
  

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
