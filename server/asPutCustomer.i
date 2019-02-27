/* asPutCustomer.i */

/*
DEFINE TEMP-TABLE tt_Kunde NO-UNDO
    FIELD EksterntKundeNr AS CHARACTER /* ID           */
    FIELD ePostAdresse    AS CHARACTER /* eMailAddress */
    FIELD MobilTlf        AS CHARACTER /* mobile       */
    FIELD Telefon         AS CHARACTER /* work         */
    FIELD Fornavn         AS CHARACTER /* givenName + ' ' + middleName */
    FIELD Etternavn       AS CHARACTER /* familyName   */
    /* shipto */
    FIELD LevAdresse1     AS CHARACTER /* addressLine  */ 
    FIELD LevLand         AS CHARACTER /* countryCode  */
    FIELD LevPostNr       AS CHARACTER /* postalCode   */
    FIELD LevPostSted     AS CHARACTER /* Cityname     */
    /* billing */
    FIELD FaktAdresse1    AS CHARACTER /* addressLine  */ 
    FIELD FaktLand        AS CHARACTER /* countryCode  */
    FIELD FaktPostNr      AS CHARACTER /* postalCode   */
    FIELD FaktPostSted    AS CHARACTER /* Cityname     */
    /* Nye felt */
    FIELD KundeKlubb      AS LOG       /* loyaltyProgramMembership */
    FIELD Nyhetsbrev      AS LOG       /* newsletterIndicator      */
    INDEX idxorderhead IS PRIMARY UNIQUE EksterntKundeNr.
*/

DEFINE TEMP-TABLE tt_Customer NO-UNDO /* SERIALIZE-NAME "Customerhode" */
    FIELD internnr                     AS INTE                  
    FIELD customerId                   AS CHAR                  
    FIELD givenName                    AS CHAR                  
    FIELD middleName                   AS CHAR                  
    FIELD familyName                   AS CHAR                  
    FIELD sh_addressLine               AS CHAR /* sh = ship */  
    FIELD sh_addressLine2              AS CHAR /* sh = ship */  
    FIELD sh_cityName                  AS CHAR                  
    FIELD sh_countrySubDivisionCode    AS CHAR                  
    FIELD sh_countryCode               AS CHAR                  
    FIELD sh_postalCode                AS CHAR                  
    FIELD bi_addressLine               AS CHAR                  
    FIELD bi_addressLine2              AS CHAR                  
    FIELD bi_cityName                  AS CHAR                  
    FIELD bi_countrySubDivisionCode    AS CHAR                  
    FIELD bi_countryCode               AS CHAR                  
    FIELD bi_postalCode                AS CHAR                  
    FIELD home_countryDialingCode      AS CHAR
    FIELD home_dialNumber              AS CHAR                  
    FIELD work_countryDialingCode      AS CHAR                  
    FIELD work_dialNumber              AS CHAR                  
    FIELD mobile_countryDialingCode    AS CHAR                  
    FIELD mobile_dialNumber            AS CHAR                  
    FIELD home_eMailAddress            AS CHAR                  
    FIELD home_htmlPreferenceIndicator AS LOG                   
    FIELD home_newsletterIndicator     AS LOG
    FIELD work_eMailAddress            AS CHAR
    FIELD work_htmlPreferenceIndicator AS LOG
    FIELD work_newsletterIndicator     AS LOG
    FIELD loyaltyProgramMembership     AS LOG
    FIELD preferredSalutationCode      AS CHAR
    INDEX internnr IS PRIMARY UNIQUE internnr.

