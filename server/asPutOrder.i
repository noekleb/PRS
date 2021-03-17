/* asPutOrder.i */
/*
DEFINE TEMP-TABLE tt_KOrdreHode NO-UNDO
    FIELD EkstOrdreNr     AS CHARACTER /* orderID      */
    FIELD orderTyp        AS CHARACTER /* orderTyp     */ 
    FIELD orderstatus     AS CHARACTER /* orderstatus  */ 
    FIELD EksterntKundeNr AS CHARACTER /* customerID   */
    /* shipto */
    FIELD DeresRef1       AS CHARACTER /* name         */ 
    FIELD LevAdresse1     AS CHARACTER /* addressLine  */ 
    FIELD LevLand         AS CHARACTER /* countryCode  */
    FIELD LevPostNr       AS CHARACTER /* postalCode   */
    FIELD LevPostSted     AS CHARACTER /*  Cityname    */
    /* billTo */
    FIELD DeresRef2       AS CHARACTER /* name         */ 
    FIELD FaktAdresse1    AS CHARACTER /* addressLine  */ 
    FIELD FaktLand        AS CHARACTER /* countryCode  */
    FIELD FaktPostNr      AS CHARACTER /* postalCode   */
    FIELD FaktPostSted    AS CHARACTER /* Cityname     */
    INDEX idxorderhead IS PRIMARY UNIQUE EkstOrdreNr.

DEFINE TEMP-TABLE tt_KOrdreLinje NO-UNDO
    FIELD EkstOrdreNr     AS CHARACTER /* orderID      */
    FIELD KOrdreLinjeNr   AS INTEGER   /* ** FINNES IKKE I JSon */
    FIELD Notat           AS CHARACTER /* note         */ 
    FIELD VareNr          AS CHARACTER /* upcid        */
    FIELD Varetekst       AS CHARACTER /*DESCRIPTION   */
    FIELD Antall          AS DECIMAL   /* quantity     */                
    FIELD Pris            AS DECIMAL   /* amount       */
    FIELD ValKod          AS CHARACTER /* currencyCode */
    FIELD LinjeSum        AS DECIMAL   /* totalAmount  */
    FIELD Mva%            AS DECIMAL   /* taxRate      */
    FIELD MvaKr           AS DECIMAL   /* taxAmount    */
    FIELD Leveringsdato   AS DATE      /* requiredDeliveryDateTime */
    FIELD LinjeRab%       AS DECIMAL   /* discountPercent */
    INDEX idxorderline IS PRIMARY  EkstOrdreNr KOrdreLinjeNr. 
*/

DEFINE TEMP-TABLE tt_orderHeader NO-UNDO /* SERIALIZE-NAME "Orderhode" */
    FIELD internnr                  AS INTE 
    FIELD orderId                   AS CHAR
    FIELD orderType                 AS CHAR
    FIELD orderStatus               AS CHAR
    FIELD opprettetdt               AS DATETIME
    FIELD endretdt                  AS DATETIME
    FIELD customerId                AS CHAR
    FIELD note                      AS CHAR
    FIELD shipmentServiceLevelCode  AS CHARACTER /* Ny */
    FIELD sh_name                   AS CHAR
    FIELD sh_addressLine            AS CHAR /* sh = ship */
    FIELD sh_addressLine2           AS CHAR /* sh = ship */
    FIELD sh_cityName               AS CHAR
    FIELD sh_countrySubDivisionCode AS CHAR
    FIELD sh_countryCode            AS CHAR
    FIELD sh_postalCode             AS CHAR
    FIELD bi_name                   AS CHAR /* bi = bill */
    FIELD bi_addressLine            AS CHAR
    FIELD bi_addressLine2           AS CHAR
    FIELD bi_cityName               AS CHAR
    FIELD bi_countrySubDivisionCode AS CHAR
    FIELD bi_countryCode            AS CHAR
    FIELD bi_postalCode             AS CHAR
    FIELD carrierCode               AS CHARACTER /* Ny */
    FIELD carrierName               AS CHARACTER /* Ny */
    FIELD giftWrapping              AS CHARACTER /* Ny */
    INDEX internnr IS PRIMARY UNIQUE internnr.

DEFINE TEMP-TABLE tt_orderLine NO-UNDO /* SERIALIZE-NAME "Orderlinje" */
    FIELD internnr                 AS INTE
    FIELD orderID                  AS CHAR 
    FIELD lineId                   AS CHARACTER /* Ny */
    FIELD TYPE                     AS CHARACTER /* Ny */
    FIELD transactionType          AS CHARACTER /* Ny */
    FIELD opprettetdt              AS DATETIME 
    FIELD note                     AS CHAR
    FIELD upcid                    AS CHAR /* required */
    FIELD description              AS CHAR
    FIELD season                   AS CHAR
    FIELD quantity                 AS INTE /* req */
    FIELD amount                   AS DECI /* req */
    FIELD currencyCode             AS CHAR /* req */
    FIELD totalAmount              AS DECI
    FIELD taxRate                  AS DECI
    FIELD taxSystemCode            AS CHAR
    FIELD taxModelCode             AS CHAR
    FIELD taxAmount                AS DECI
    FIELD requiredDeliveryDateTime AS CHAR
    FIELD discountPercent          AS DECI
    FIELD discountAmount           AS DECIMAL FORMAT "->>,>>>,>>9.99" /* Rabattbelø på ordrelinje. */
    FIELD discountDescription      AS CHARACTER /* Forklaring til rabatt. */
    INDEX internnr IS PRIMARY internnr orderID.

DEFINE TEMP-TABLE tt_payments NO-UNDO /* SERIALIZE-NAME "Orderlinje" */
    FIELD internnr     AS INTE
    FIELD orderID      AS CHAR 
    FIELD amount       AS DECI
    FIELD currencyCode AS CHAR
    FIELD TYPE         AS CHAR
    FIELD referenceId  AS CHAR
    INDEX internnr IS PRIMARY internnr orderID.
DEFINE TEMP-TABLE tmp_payments LIKE tt_payments.





    
    
