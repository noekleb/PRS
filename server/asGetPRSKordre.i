DEFINE TEMP-TABLE tt_shippingheader NO-UNDO
    FIELD orderId AS CHAR
    FIELD note    AS CHAR
    INDEX orderId IS PRIMARY UNIQUE orderId.

DEFINE TEMP-TABLE tt_shippinglines NO-UNDO
    FIELD orderId    AS CHAR
    FIELD trackingId AS CHAR
    FIELD kode       AS CHAR
    FIELD antall     AS INTE
    FIELD note       AS CHAR
    INDEX orderId IS PRIMARY orderId trackingId.
