DEF INPUT  PARAM icBuffer    AS CHAR NO-UNDO.
DEF INPUT  PARAM icRowid     AS CHAR NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocError     AS CHAR NO-UNDO.

FIND KampanjeTilbArtikkel WHERE ROWID(KampanjeTilbArtikkel) = TO-ROWID(icRowid) NO-LOCK NO-ERROR.
IF AVAIL KampanjeTilbArtikkel THEN 
DO:
/*   FOR EACH ProduktFamilie EXCLUSIVE-LOCK OF KampanjeTilbArtikkel: */
/*     FOR EACH ProduktFamMedlem EXCLUSIVE-LOCK OF ProduktFamilie:   */
/*       DELETE ProduktFamMedlem.                                    */
/*     END.                                                          */
/*     DELETE ProduktFamilie.                                        */
/*   END.                                                            */
/*   IF ERROR-STATUS:ERROR THEN                                      */
/*       ocError = "Feil oppsto ved sletting av vareboklinje(r).".   */
/*   ELSE                                                            */
    DELETE KampanjeTilbArtikkel.
END.
