DEFINE TEMP-TABLE tt_returnheader NO-UNDO
    FIELD orderId AS CHAR
    INDEX orderId IS PRIMARY UNIQUE orderId.

DEFINE TEMP-TABLE tt_returnlines NO-UNDO
    FIELD orderId       AS CHAR
    FIELD kode          AS CHAR
    FIELD antall        AS INTE
    FIELD orsak         AS INTE
    FIELD phoenix_orsak AS CHAR
    INDEX orderId IS PRIMARY orderId.
