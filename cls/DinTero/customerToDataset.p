
/*------------------------------------------------------------------------
    File        : customerToDataset.p
    Purpose     : Tar hånd om konvertering av customer object til customer
                  dataset. 

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

{cls\dintero\ttCustomerObj.i}
{cls\dintero\dsCustomerObj.i}

DEFINE INPUT PARAMETER oCustomer AS JsonObject NO-UNDO.
DEFINE OUTPUT PARAMETER DATASET FOR dsCustCustomer.

DEFINE VARIABLE cCustomer AS CHARACTER NO-UNDO.
DEFINE VARIABLE bOk AS LOG NO-UNDO.
DEFINE VARIABLE iLoop AS INTEGER   NO-UNDO.
DEFINE VARIABLE iLoop2 AS INTEGER   NO-UNDO.
DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.
DEFINE VARIABLE bTest AS LOG NO-UNDO.  

/* NB: Når antall ekstenter ikke angis på en variabel, settes dette antallet første gang variablelen benyttes. */       
DEFINE VARIABLE myLabelsCustomer AS CHARACTER EXTENT NO-UNDO. /* NB: Ikke angi antall ekstenter. */

DEFINE VARIABLE oMarketing_consent AS JsonObject NO-UNDO.
DEFINE VARIABLE oSms AS JsonObject NO-UNDO.
DEFINE VARIABLE oEMail AS JsonObject NO-UNDO.
DEFINE VARIABLE myLabelsmarketing_consent AS CHARACTER EXTENT NO-UNDO. /* NB: Ikke angi antall ekstenter. */

DEFINE VARIABLE ometadata AS JsonObject NO-UNDO.
DEFINE VARIABLE myLabelsmetadata AS CHARACTER EXTENT NO-UNDO. /* NB: Ikke angi antall ekstenter. */

DEFINE VARIABLE oArrayAddresses AS JSonArray NO-UNDO.
DEFINE VARIABLE oAddresses AS JsonObject NO-UNDO.
DEFINE VARIABLE myLabelsAddresses AS CHARACTER EXTENT NO-UNDO. /* NB: Ikke angi antall ekstenter. */

 
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

ASSIGN 
  bTest = TRUE 
  .

/* Leser ut lablene på alle entryene i hovedobjektet. */
myLabelsCustomer = ocustomer:GetNames().

/* Skaper record for lagring av data. */
CREATE ttCustCustomer.

/* Løper igjennom og henter ut feltene fra root noden. */
OBJECTLOOP:
DO iLoop = 1 TO EXTENT(myLabelsCustomer):
  /* Henter ut verdien i feltet. */
  cTekst = oCustomer:getJsonText(myLabelsCustomer[iLoop]).
   
  /* Legger inn data i feltene i recorden. */    
  CASE myLabelsCustomer[iLoop]:
    /* Tar hånd om feltene */
    WHEN 'id' THEN ttCustcustomer.UUId = cTekst.
    WHEN 'customer_id' THEN ttCustcustomer.customer_id = cTekst.
    WHEN 'first_name' THEN ttCustcustomer.first_name = cTekst.
    WHEN 'first_name' THEN ttCustcustomer.first_name = cTekst.
    WHEN 'last_name' THEN ttCustcustomer.last_name = cTekst.
    WHEN 'email' THEN ttCustcustomer.email = cTekst.
    WHEN 'phone_number' THEN ttCustcustomer.phone_number = cTekst.
    WHEN 'custStatus' THEN ttCustcustomer.custStatus = cTekst.
    WHEN 'favorite_store' THEN ttCustcustomer.favorite_store = cTekst.
    WHEN 'type' THEN ttCustcustomer.custType = cTekst.
    WHEN 'gender' THEN ttCustcustomer.gender = cTekst.
    WHEN 'date_of_birth' THEN ttCustcustomer.gender = IF cTekst = ? THEN '' ELSE cTekst.
    /* Tar hånd om objectene */   
    WHEN 'marketing_consent' THEN 
      DO:
        oMarketing_consent = oCustomer:GetJsonObject("marketing_consent").
        myLabelsmarketing_consent = oMarketing_consent:GetNames().
        /* Inneholder maks 2 objecter. */
      END.
    WHEN 'metadata' THEN 
      DO:
        ometadata = oCustomer:GetJsonObject("metadata").
        myLabelsmetadata = ometadata:GetNames().
        /* Pakker ut denne i egen blokk under. da den har 4 nivåer. */
      END.
    WHEN 'addresses' THEN 
      DO:
        oArrayAddresses = oCustomer:GetJsonArray("addresses").
      END.
  END CASE.
END. /* OBJECTLOOP */

/* Tar hånd om Marketing_Consent recorden. */
/*"marketing_consent": {                                  */
/*                  "sms": {                              */
/*                    "consent": true,                    */
/*                    "updated_at": "2018-01-12T13:42:00Z"*/
/*                    },                                  */
/*                  "email": {                            */
/*                    "consent": true,                    */
/*                    "updated_at": "2018-01-12T13:42:00Z"*/
/*                    }                                   */
/*                  }                                     */
IF  EXTENT(myLabelsmarketing_consent) > 0 THEN
MARKETINGCONSENTBLOKK: 
DO:
  DO iLoop = 1 TO EXTENT(myLabelsmarketing_consent):
    cTekst = oMarketing_consent:getJsonText(myLabelsmarketing_consent[iLoop]).
    
    CASE myLabelsmarketing_consent[iLoop]:
      WHEN 'sms' THEN 
        DO:
          CREATE ttCustMarketing_consent.
          ASSIGN 
            ttCustMarketing_consent.customer_id = ttCustCustomer.customer_id
            ttCustMarketing_consent.consent_id = myLabelsmarketing_consent[iLoop]
            oSms = oMarketing_consent:GetJsonObject("sms").
            ttCustMarketing_consent.consent = IF oSms:GetJsonText('consent') = 'TRUE' THEN TRUE ELSE FALSE
            . 
        END.
      WHEN 'email' THEN 
        DO:
          CREATE ttCustMarketing_consent.
          ASSIGN 
            ttCustMarketing_consent.customer_id = ttCustCustomer.customer_id
            ttCustMarketing_consent.consent_id = myLabelsmarketing_consent[iLoop]
            oEMail = oMarketing_consent:GetJsonObject("email").
            ttCustMarketing_consent.consent = IF oEMail:GetJsonText('consent') = 'TRUE' THEN TRUE ELSE FALSE
            . 
        END.
    END CASE.
  END.
END. /* MARKETINGCONSENTBLOKK */

/* Tar hånd om adressene. */
IF VALID-OBJECT(oArrayAddresses) THEN 
ADRESSESBLOKK:
DO:
  ADRESSARRAYBLOKK:
  DO iLoop = 1 TO oArrayAddresses:LENGTH:
    oAddresses = oArrayAddresses:GetJsonObject(iLoop).
    myLabelsAddresses = oAddresses:GetNames().
    cTekst = oAddresses:getJsonText(myLabelsAddresses[1]).
    
    FIND ttCustAddresses WHERE 
      ttCustAddresses.customer_id = ttCustCustomer.customer_id AND 
      ttCustAddresses.addrType    = cTekst NO-ERROR.
    IF NOT AVAILABLE ttCustAddresses THEN 
      DO:
        CREATE ttCustAddresses.
        ASSIGN 
          ttCustAddresses.customer_id = ttCustCustomer.customer_id
          ttCustAddresses.addrType    = cTekst
          .
      END.
    /* Løper igjennom feltene i recorden. */
    ITEMSBLOKK:  
    DO iLoop2 = 1 TO EXTENT(myLabelsAddresses):
      /* Leser ut felt verdi. */
      cTekst = oAddresses:getJsonText(myLabelsAddresses[iLoop2]).
      CASE myLabelsAddresses[iLoop2]:
        WHEN 'address_line' THEN ttCustAddresses.address_line = cTekst.
        WHEN 'address_line_2' THEN ttCustAddresses.address_line_2 = cTekst.
        WHEN 'postal_code' THEN ttCustAddresses.postal_code = cTekst.
        WHEN 'postal_place' THEN ttCustAddresses.postal_place = cTekst.
        WHEN 'country' THEN ttCustAddresses.country = cTekst.
        WHEN 'latitude' THEN ttCustAddresses.latitude = DEC(cTekst).
        WHEN 'longitude' THEN ttCustAddresses.longitude = DEC(cTekst).
        WHEN 'custom_type' THEN ttCustAddresses.custom_type = cTekst.
        WHEN 'comment' THEN ttCustAddresses.comment = cTekst.
      END CASE.  
    END. /* ITEMSBLOKK */      
  END. /* ADRESSARRAYBLOKK */
END. /* ADRESSESBLOKK */

/* Tar hånd om Marketing_Consent recorden. */
IF EXTENT(myLabelsmetadata) > 0 THEN
MARKETINGCONSENTBLOKK: 
DO:
  CREATE ttCustMetadata.
  ASSIGN 
    ttCustMetadata.customer_id = ttCustCustomer.customer_id
    .
  DO iLoop = 1 TO EXTENT(myLabelsmarketing_consent):
    cTekst = oMarketing_consent:getJsonText(myLabelsmarketing_consent[iLoop]).
    CASE myLabelsmarketing_consent[iLoop]:
      WHEN 'dob_year' THEN ttCustMetadata.dob_year = INT(cTekst).
    END CASE.
  END.
END. /* MARKETINGCONSENTBLOKK */
