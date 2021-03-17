
/* ------------ JSon Start -----------------------------        
{
    "priceListHeader": {
        "id": "1",
        "currencyCode": "GBP"
    },
    "priceListLines": [
        {
            "item": {
                "upcid": "7332972566202"
            },
            "unitPrice": {
                "unitAmount": 20.95,
                "description": {
                    "isTaxInclusive": true
                }
            },
            "priceBreak": {
                "priceBreakAmount": 9.95
            },
            "extension": {
                "unitCost": {
                    "unitAmount": 5.00
                },
                "tax": {
                    "calculation": {
                        "rateNumeric": 20
                    }
                }
            }
        },
    ]
}
------------ JSon Slutt ----------------------------- */

DEFINE TEMP-TABLE tt_artprisbutiker NO-UNDO
    FIELD butik    AS INTE
    FIELD butnamn  AS CHAR
    FIELD kortnavn AS CHAR
    FIELD profilnr AS INTEGER 
    INDEX butik IS PRIMARY UNIQUE butik.

DEFINE TEMP-TABLE tt_artpris NO-UNDO
    FIELD Butik AS INTEGER 
    FIELD Kode    AS CHARACTER 
    FIELD Varekost AS DECIMAL 
    FIELD Mva% AS DECIMAL 
    FIELD Pris    AS DECIMAL 
    FIELD TilbPris AS DECIMAL 
    INDEX artpris IS PRIMARY butik kode.
