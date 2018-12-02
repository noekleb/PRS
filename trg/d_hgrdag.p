TRIGGER PROCEDURE FOR DELETE OF hgrdag.
    
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
         ELogg.TabellNavn     = "hgrdag" AND
         ELogg.EksterntSystem = "HK"    AND
         ELogg.Verdier        = STRING(hgrdag.butnr) + CHR(1) + STRING(hgrdag.hg) + CHR(1) + STRING(hgrdag.dato) NO-ERROR.
    IF NOT AVAIL Elogg THEN DO:
        CREATE Elogg.
        ASSIGN ELogg.TabellNavn     = "hgrdag"
               ELogg.EksterntSystem = "HK"   
               ELogg.Verdier        = STRING(hgrdag.butnr) + CHR(1) + STRING(hgrdag.hg) + CHR(1) + STRING(hgrdag.dato).
    END.
    ASSIGN ELogg.EndringsType = 3
           ELogg.Behandlet    = FALSE.
    RELEASE ELogg.
END.

