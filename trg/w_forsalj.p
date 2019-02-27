TRIGGER PROCEDURE FOR WRITE OF SkoTex.Forsalj.

DEFINE VARIABLE bStatTilHK  AS LOG       NO-UNDO.
DEFINE VARIABLE cTekst      AS CHARACTER NO-UNDO.

ASSIGN
  SkoTex.Forsalj.EDato    = TODAY
  SkoTex.Forsalj.ETid     = TIME
  SkoTex.Forsalj.BrukerId = USERID("skotex").

IF Skotex.Forsalj.ForsaljAktiv = TRUE THEN DO:
  FIND ELogg WHERE 
       ELogg.TabellNavn     = "Forsalj" AND
       ELogg.EksterntSystem = "POS"    AND
       ELogg.Verdier        = STRING(Forsalj.ForsNr) NO-ERROR.
  IF NOT AVAIL Elogg THEN DO:
      CREATE Elogg.
      ASSIGN ELogg.TabellNavn     = "Forsalj"
             ELogg.EksterntSystem = "POS"   
             ELogg.Verdier        = STRING(Forsalj.ForsNr).
  END.
  ASSIGN ELogg.EndringsType = 1
         ELogg.Behandlet    = FALSE.
  RELEASE ELogg.
END.

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
         ELogg.TabellNavn     = "Forsalj" AND
         ELogg.EksterntSystem = "HK"    AND
         ELogg.Verdier        = STRING(Forsalj.ForsNr) NO-ERROR.
    IF NOT AVAIL Elogg THEN DO:
        CREATE Elogg.
        ASSIGN ELogg.TabellNavn     = "Forsalj"
               ELogg.EksterntSystem = "HK"   
               ELogg.Verdier        = STRING(Forsalj.ForsNr) NO-ERROR.
    END.
    ASSIGN ELogg.EndringsType = 1
           ELogg.Behandlet    = FALSE.
    RELEASE Elogg.
END. /* LOGGSTAT */

