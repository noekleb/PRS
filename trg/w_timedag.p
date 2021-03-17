TRIGGER PROCEDURE FOR WRITE OF timedag.

DEFINE VARIABLE bStatTilHK  AS LOG       NO-UNDO.
DEFINE VARIABLE cTekst      AS CHARACTER NO-UNDO.

/* Skal det sendes artikkelstatistikk? */
{syspara.i 3 4 1 cTekst}
IF (cTekst = "1" OR cTekst = "") THEN
    bStatTilHK = TRUE.
ELSE
    bStatTilHK = FALSE.

IF bStatTilHK THEN  
LOGGSTAT:
DO:
    FIND ELogg WHERE 
         ELogg.TabellNavn     = "timedag" AND
         ELogg.EksterntSystem = "HK"    AND
         ELogg.Verdier        = STRING(timedag.butnr) + CHR(1) + STRING(timedag.dato) NO-ERROR.
    IF NOT AVAIL Elogg THEN DO:
        CREATE Elogg.
        ASSIGN ELogg.TabellNavn     = "timedag"
               ELogg.EksterntSystem = "HK"   
               ELogg.Verdier        = STRING(timedag.butnr) + CHR(1) + STRING(timedag.dato).
    END.
    ASSIGN ELogg.EndringsType = 1
           ELogg.Behandlet    = FALSE.
    RELEASE ELogg.
END.

