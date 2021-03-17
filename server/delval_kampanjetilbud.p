DEF INPUT  PARAM icBuffer    AS CHAR NO-UNDO.
DEF INPUT  PARAM icRowid     AS CHAR NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocError     AS CHAR NO-UNDO.

FIND KampanjeTilbud WHERE ROWID(KampanjeTilbud) = TO-ROWID(icRowid) NO-LOCK NO-ERROR.
IF AVAIL KampanjeTilbud THEN 
DO:
    FOR EACH KampanjeTilbArtikkel EXCLUSIVE-LOCK OF KampanjeTilbud:
      DELETE KampanjeTilbArtikkel.
    END.
    FOR EACH KampTilbTemplate EXCLUSIVE-LOCK OF KampanjeTilbud:
      DELETE KampTilbTemplate.
    END.
    IF ERROR-STATUS:ERROR THEN
        ocError = "Feil oppsto ved sletting av vareboklinje(r).".
    ELSE
      DELETE KampanjeTilbud.
END.
