/* ------ JSon Start ---------------------- 
[
    {
        "upcid": "7332972566202",
        "facilities": [
            {
                "id": "cbrstore_1",
                "availableQuantity": 1,
            },
            {
                "id": "cbrstore_2",
                "availableQuantity": 3,
            },
            {
                "id": "cbrstore_3",
                "availableQuantity": 6,
            },
            {
                "id": "cbrstore_4",
                "availableQuantity": 2,
            }
        ]
    }
]
--------------- JSon Slutt ------------- */

DEFINE TEMP-TABLE tt_stockbutiker NO-UNDO
    FIELD butik AS INTE
    FIELD butnamn AS CHAR
    FIELD kortnavn AS CHAR
    INDEX butik IS PRIMARY UNIQUE butik.

DEFINE TEMP-TABLE tt_stock NO-UNDO
    FIELD butik AS INTE
    FIELD kode AS CHAR
    FIELD lagant AS INTE
    INDEX butik IS PRIMARY butik.
