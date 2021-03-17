/*   batchlogg_post_delete.p
   
-----------------------------------------------------------------------------------------*/   
DEF INPUT  PARAM ihBuffer       AS HANDLE NO-UNDO.  /* Handle to current buffer. Her: PkSdlHode */
DEF INPUT  PARAM icAction       AS CHAR   NO-UNDO.  /* Create or Update */
DEF INPUT  PARAM icSessionId    AS CHAR   NO-UNDO.
DEF OUTPUT PARAM ocValue        AS CHAR   NO-UNDO.  /* Error message. If <> blank the transaction is backed out */

FIND BatchLogg WHERE BatchLogg.BatchNr = ihBuffer:BUFFER-FIELD("BatchNr"):BUFFER-VALUE
  NO-LOCK NO-ERROR.
IF NOT AVAIL BatchLogg THEN DO:
  ocValue = "".
  RETURN.
END.
ELSE DO:
  FOR EACH TransLogg OF BatchLogg EXCLUSIVE-LOCK:
    DELETE TransLogg.
  END.
END.
