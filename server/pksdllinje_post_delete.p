/*   
   
-----------------------------------------------------------------------------------------*/   
DEF INPUT  PARAM ihBuffer       AS HANDLE NO-UNDO.  /* Handle to current buffer. Her: PkSdlHode */
DEF INPUT  PARAM icAction       AS CHAR   NO-UNDO.  /* Create or Update */
DEF INPUT  PARAM icSessionId    AS CHAR   NO-UNDO.
DEF OUTPUT PARAM ocValue        AS CHAR   NO-UNDO.  /* Error message. If <> blank the transaction is backed out */

FIND PkSdlLinje WHERE PkSdlLinje.PkSdlId = ihBuffer:BUFFER-FIELD("PkSdlId"):BUFFER-VALUE AND 
  PkSdlLinje.PkSdlLinjeId = ihBuffer:BUFFER-FIELD("PkSdlLinjeId"):BUFFER-VALUE
  NO-LOCK NO-ERROR.
IF NOT AVAIL PkSdlLinje THEN DO:
  ocValue = "".
  RETURN.
END.
ELSE DO:
  IF INT(DYNAMIC-FUNCTION("getCurrentValues" IN SOURCE-PROCEDURE)) = 0 THEN 
    DELETE PkSdlLinje.
END.
