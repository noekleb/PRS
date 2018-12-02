TRIGGER PROCEDURE FOR WRITE OF varedag.

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
         ELogg.TabellNavn     = "varedag" AND
         ELogg.EksterntSystem = "HK"    AND
         ELogg.Verdier        = STRING(varedag.butnr) + CHR(1) + STRING(varedag.ean) + CHR(1) + STRING(varedag.dato) NO-ERROR.
    IF NOT AVAIL Elogg THEN DO:
        CREATE Elogg.
        ASSIGN ELogg.TabellNavn     = "varedag"
               ELogg.EksterntSystem = "HK"   
               ELogg.Verdier        = STRING(varedag.butnr) + CHR(1) + STRING(varedag.ean) + CHR(1) + STRING(varedag.dato).
    END.
    ASSIGN ELogg.EndringsType = 1
           ELogg.Behandlet    = FALSE.
    RELEASE ELogg.
END.

