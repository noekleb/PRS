DEFINE TEMP-TABLE tt_payments NO-UNDO SERIALIZE-NAME "tt_payments" /* SERIALIZE-NAME "Orderlinje" */
    FIELD internnr       AS INTE
    FIELD orderID       AS CHAR 
    FIELD amount       AS DECI
    FIELD currencyCode AS CHAR
    FIELD TYPE  AS CHAR
    FIELD referenceId    AS CHAR
    INDEX internnr IS PRIMARY internnr orderID.

DEFINE TEMP-TABLE tt_order NO-UNDO SERIALIZE-NAME "Order"
    FIELD cwebbutnr            AS CHAR
    FIELD id                   AS INTE
    FIELD parent_id            AS INTE
    FIELD cStatus              AS CHAR
    FIELD order_key            AS CHAR
    FIELD number               AS INTE
    FIELD currency             AS CHAR
    FIELD cVersion             AS CHAR
    FIELD prices_include_tax   AS LOG
    FIELD date_created         AS CHAR
    FIELD date_modified        AS CHAR
    FIELD opprettetdt         AS DATETIME
    FIELD endretdt            AS DATETIME
    FIELD customer_id          AS INTE
    FIELD discount_total       AS CHAR
    FIELD discount_tax         AS CHAR
    FIELD shipping_total       AS CHAR
    FIELD shipping_tax         AS CHAR
    FIELD cart_tax             AS CHAR
    FIELD total                AS CHAR
    FIELD total_tax            AS CHAR
    FIELD payment_method       AS CHAR
    FIELD payment_method_title AS CHAR
    FIELD transaction_id       AS CHAR
    FIELD customer_ip_address  AS CHAR
    FIELD customer_user_agent  AS CHAR
    FIELD created_via          AS CHAR
    FIELD customer_note        AS CHAR
    FIELD date_completed       AS CHAR
    FIELD date_paid            AS CHAR
    FIELD personnummer         AS CHAR
    FIELD medleminfo           AS CHAR
    FIELD bi_first_name        AS CHAR
    FIELD bi_last_name         AS CHAR
    FIELD bi_company           AS CHAR
    FIELD bi_address_1         AS CHAR
    FIELD bi_address_2         AS CHAR
    FIELD bi_city              AS CHAR
    FIELD bi_state             AS CHAR
    FIELD bi_postcode          AS CHAR
    FIELD bi_country           AS CHAR
    FIELD bi_email             AS CHAR
    FIELD bi_phone             AS CHAR
    FIELD sh_first_name        AS CHAR
    FIELD sh_last_name         AS CHAR
    FIELD sh_company           AS CHAR
    FIELD sh_address_1         AS CHAR
    FIELD sh_address_2         AS CHAR
    FIELD sh_city              AS CHAR
    FIELD sh_state             AS CHAR
    FIELD sh_postcode          AS CHAR
    FIELD sh_country           AS CHAR
    INDEX number IS PRIMARY number.

DEFINE TEMP-TABLE tt_line_items NO-UNDO SERIALIZE-NAME "LineItems"
    FIELD number  AS INTE
    FIELD NAME    AS CHAR
    FIELD sku     AS CHAR
    FIELD product_id AS INTE
    FIELD variation_id AS INTE
    FIELD quantity     AS INTE
    FIELD tax_class    AS CHAR
    FIELD price        AS DECI
    FIELD subtotal     AS DECI
    FIELD subtotal_tax AS DECI
    FIELD dTotal       AS DECI
    FIELD total_tax    AS DECI
    FIELD pris         AS DECI
    FIELD rabatt       AS DECI
    FIELD mvakr        AS DECI
    INDEX number IS PRIMARY number.

DEFINE TEMP-TABLE tt_tax_lines NO-UNDO
    FIELD number       AS INTE
    FIELD rate_code AS CHAR
    FIELD rate_id    AS CHAR
    FIELD compound        AS CHAR
    FIELD tax_total    AS CHAR
    FIELD shipping_tax_total    AS CHAR
    INDEX number IS PRIMARY number.

DEFINE TEMP-TABLE tt_shipping_lines NO-UNDO SERIALIZE-NAME "Shippinglines"
    FIELD number       AS INTE
    FIELD method_title AS CHAR
    FIELD method_id    AS CHAR
    FIELD instance_id  AS CHAR
    FIELD dTotal        AS DECI
    FIELD total_tax    AS DECI
    INDEX number IS PRIMARY number.


DEFINE DATASET dsttWooComOrder SERIALIZE-NAME "WooComOrder" FOR tt_order, tt_line_items, tt_shipping_lines 
   DATA-RELATION drOrderRad FOR tt_order, tt_line_items RELATION-FIELDS (number,number) NESTED
   DATA-RELATION drShipRad FOR tt_order, tt_shipping_lines RELATION-FIELDS (number,number) NESTED.

