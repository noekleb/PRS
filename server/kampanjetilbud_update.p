/*
   Opprettet: 29.09.2007 Geir Otto Olsen
 -----------------------------------------------------------------------------------*/
 DEF INPUT  PARAM icRowid             AS CHAR NO-UNDO.
 DEF INPUT  PARAM icFields            AS CHAR NO-UNDO.
 DEF INPUT  PARAM icValues            AS CHAR NO-UNDO.
 DEF INPUT  PARAM icSessionId         AS CHAR NO-UNDO.
 DEF OUTPUT PARAM ocReturn            AS CHAR NO-UNDO.  

  &SCOPED-DEFINE useTable KampanjeTilbud
  FIND {&useTable} WHERE ROWID({&useTable}) = TO-ROWID(icRowId) EXCLUSIVE-LOCK NO-ERROR.
  
  IF NOT AVAIL {&useTable} THEN
  DO:
   ocReturn = 'Posten for {&useTable} ble ikke funnet'.
   RETURN.
  END.
/*   MESSAGE                                                                                     */
/*     DEC(ENTRY(LOOKUP('KampTilbBelop',icFields),icValues,"|")) <> KampanjeTilbud.KampTilbBelop */
/*     DEC(ENTRY(LOOKUP('KampTilbBelop',icFields),icValues,"|"))                                 */
/*                                                                                               */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                        */
  IF DEC(ENTRY(LOOKUP('KampTilbBelop',icFields),icValues,"|")) <> KampanjeTilbud.KampTilbBelop THEN
  DO:
    IF DEC(ENTRY(LOOKUP('KampTilbBelop',icFields),icValues,"|")) GT 0 THEN
    FOR EACH kampanjeTilbArtikkel OF KampanjeTilbud EXCLUSIVE-LOCK:
      KampanjeTilbArtikkel.KampTilbArtBelop = 0.
    END.
  END.

