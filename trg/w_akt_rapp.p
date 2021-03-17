TRIGGER PROCEDURE FOR WRITE OF akt_rapp.

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
       ELogg.TabellNavn     = "akt_rapp" AND
       ELogg.EksterntSystem = "HK"    AND
       ELogg.Verdier        = STRING(akt_rapp.dato) + CHR(1) + STRING(akt_rapp.butik) + CHR(1) + STRING(akt_rapp.kasse) + CHR(1) + STRING(akt_rapp.tid) NO-ERROR.
  IF NOT AVAIL Elogg THEN DO:
      CREATE Elogg.
      ASSIGN ELogg.TabellNavn     = "akt_rapp"
             ELogg.EksterntSystem = "HK"   
             ELogg.Verdier        = STRING(akt_rapp.dato) + CHR(1) + STRING(akt_rapp.butik) + CHR(1) + STRING(akt_rapp.kasse) + CHR(1) + STRING(akt_rapp.tid).
  END.
  ASSIGN ELogg.EndringsType = 1
         ELogg.Behandlet    = FALSE.
  RELEASE ELogg.
END.


