TRIGGER PROCEDURE FOR WRITE OF kas_rap.

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
         ELogg.TabellNavn     = "kas_rap" AND
         ELogg.EksterntSystem = "HK"    AND
         ELogg.Verdier        = STRING(kas_rap.dato) + CHR(1) + STRING(kas_rap.butikk) + CHR(1) + STRING(kas_rap.kasse) + CHR(1) + STRING(kas_rap.z_nummer) NO-ERROR.
    IF NOT AVAIL Elogg THEN DO:
        CREATE Elogg.
        ASSIGN ELogg.TabellNavn     = "kas_rap"
               ELogg.EksterntSystem = "HK"   
               ELogg.Verdier        = STRING(kas_rap.dato) + CHR(1) + STRING(kas_rap.butikk) + CHR(1) + STRING(kas_rap.kasse) + CHR(1) + STRING(kas_rap.z_nummer).
    END.
    ASSIGN ELogg.EndringsType = 1
           ELogg.Behandlet    = FALSE.
    RELEASE ELogg.
END.



