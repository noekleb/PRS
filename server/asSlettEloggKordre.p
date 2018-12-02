DEFINE INPUT  PARAMETER cElogglist AS CHARACTER   NO-UNDO.

DEFINE VARIABLE ii AS INTEGER     NO-UNDO.

DO  ii = 1 TO NUM-ENTRIES(cElogglist):
    FIND ELogg WHERE ELogg.TabellNavn = "KOrdreHode" AND
        ELogg.EksterntSystem          = "WEBBUT" AND
        ELogg.verdier                 = ENTRY(ii,cElogglist) EXCLUSIVE NO-ERROR.
                                 
    IF AVAIL Elogg THEN
        DELETE ELogg NO-ERROR.
END.
