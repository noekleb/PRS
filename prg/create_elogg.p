DEF INPUT PARAMETER cTabell    AS CHAR NO-UNDO.
DEF INPUT PARAMETER cEDBSystem AS CHAR NO-UNDO.
DEF INPUT PARAMETER cVerdier   AS CHAR NO-UNDO.


LOGG:
DO FOR ELogg:
    FIND ELogg WHERE 
         ELogg.TabellNavn     = cTabell    AND
         ELogg.EksterntSystem = cEDBSystem AND
         ELogg.Verdier        = cVerdier   NO-ERROR.
    IF NOT AVAIL Elogg THEN DO:
        CREATE Elogg.
        ASSIGN ELogg.TabellNavn     = cTabell
               ELogg.EksterntSystem = cEDBSystem  
               ELogg.Verdier        = cVerdier.
    END.
    ASSIGN ELogg.EndringsType = 1
           ELogg.Behandlet    = FALSE.
    RELEASE ELogg.
END. /* LOGG */
