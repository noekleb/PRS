DEF INPUT  PARAM icBuffer    AS CHAR NO-UNDO.
DEF INPUT  PARAM icRowid     AS CHAR NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocError     AS CHAR NO-UNDO.

FIND ProduktFamilie WHERE ROWID(ProduktFamilie) = TO-ROWID(icRowid) NO-LOCK NO-ERROR.
IF AVAIL ProduktFamilie THEN 
DO:
  IF NOT CAN-FIND(FIRST KampanjeTilbArtikkel OF ProduktFamilie) THEN
  DO:
    FOR EACH ProduktFamMedlem EXCLUSIVE-LOCK OF ProduktFamilie:
      DELETE ProduktFamMedlem.
    END.
    IF ERROR-STATUS:ERROR THEN
        ocError = "Feil oppsto ved sletting av ProduktFamMedlem.".
    ELSE
      DELETE ProduktFamilie.
  END.
  ELSE
  DO:
    FIND FIRST KampanjeTilbArtikkel OF ProduktFamilie NO-LOCK NO-ERROR.
    IF AVAIL KampanjeTilbArtikkel THEN
        ocError = 'Kan ikke slette produkt familien, benyttes i kampanje ' + STRING(KampanjeTilbArtikkel.KampId). 
  END.
END.
