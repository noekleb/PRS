/* asPutCancelOrder.i */

DEFINE TEMP-TABLE tt_CancelKOrdreHode NO-UNDO
    FIELD EkstOrdreNr     AS CHARACTER /* orderID      */
    FIELD orderTyp        AS CHARACTER /* orderTyp - Sett 'CANCEL' her. */ 
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

DEFINE TEMP-TABLE tt_CancelKOrdreLinje NO-UNDO
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
