
/*------------------------------------------------------------------------
    File        : dsCustomerObj.i
    Purpose     : 

    Syntax      :

    Description : Definerer datasettet for customer objectet.

    Author(s)   : Tom Nøkleby
    Created     : Sun Nov 01 06:06:13 CET 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
/*{cls\dintero\ttcustomerObj.i}*/

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEFINE DATASET dsCustCustomer SERIALIZE-HIDDEN
  FOR ttCustCustomer, ttCustMetadata, ttCustAttributes, ttCustMarketing_consent, ttCustSms, ttCustEMail, ttCustAddresses, ttCustTerm, ttCustCompany
  DATA-RELATION drCustMetadata FOR ttCustCustomer, ttCustMetadata RELATION-FIELDS (customer_id, customer_id) NESTED
  DATA-RELATION drCustAttributes FOR ttCustCustomer, ttCustAttributes RELATION-FIELDS (customer_id, customer_id) NESTED
  DATA-RELATION drCustMarketing_consent FOR ttCustCustomer, ttCustMarketing_consent RELATION-FIELDS (customer_id, customer_id) NESTED
  DATA-RELATION drCustSms FOR ttCustMarketing_consent, ttCustSms RELATION-FIELDS (customer_id, customer_id, consent_id, consent_id) NESTED
  DATA-RELATION drCustEMail FOR ttCustMarketing_consent, ttCustEMail RELATION-FIELDS (customer_id, customer_id, consent_id, consent_id) NESTED
  DATA-RELATION drCustAddresses FOR ttCustCustomer, ttCustAddresses RELATION-FIELDS (customer_id, customer_id) NESTED
  DATA-RELATION drCustTerm FOR ttCustCustomer, ttCustTerm RELATION-FIELDS (customer_id, customer_id) NESTED
  DATA-RELATION drCustCompany FOR ttCustCustomer, ttCustCompany RELATION-FIELDS (customer_id, customer_id) NESTED
  .
  
  