/* tt_kolinjer.i */

DEFINE TEMP-TABLE tt_linjer NO-UNDO SERIALIZE-NAME "kolinjer"
    FIELD artikkelnr AS CHAR
    FIELD linjenr AS INTE
    FIELD ean AS CHAR
    FIELD varetekst AS CHAR
    FIELD antall AS INTE
    FIELD levfargkod AS CHAR
    FIELD storl AS CHAR
    FIELD kundpris AS DECI
    FIELD feilkode AS INTE
    FIELD used AS LOG. /* används på kassasidan */

