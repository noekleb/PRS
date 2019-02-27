TRIGGER PROCEDURE FOR WRITE OF StLager.

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
         ELogg.TabellNavn     = "StLager" AND
         ELogg.EksterntSystem = "HK"    AND
         ELogg.Verdier        = StLager.StTypeId      + CHR(2) + 
                                STRING(StLager.Butik) + CHR(2) + 
                                StLager.DataObjekt NO-ERROR.
    IF NOT AVAIL Elogg THEN DO:
        CREATE Elogg.
        ASSIGN ELogg.TabellNavn     = "StLager"
               ELogg.EksterntSystem = "HK"   
               ELogg.Verdier        = StLager.StTypeId      + CHR(2) + 
                                      STRING(StLager.Butik) + CHR(2) + 
                                      StLager.DataObjekt NO-ERROR.
    END.
    ASSIGN ELogg.EndringsType = 1
           ELogg.Behandlet    = FALSE.
    RELEASE ELogg.
END.

