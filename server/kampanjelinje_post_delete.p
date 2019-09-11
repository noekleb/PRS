/*   
   
-----------------------------------------------------------------------------------------*/   
DEF INPUT  PARAM ihBuffer       AS HANDLE NO-UNDO.  /* Handle to current buffer. Her: PkSdlHode */
DEF INPUT  PARAM icAction       AS CHAR   NO-UNDO.  /* Create or Update */
DEF INPUT  PARAM icSessionId    AS CHAR   NO-UNDO.
DEF OUTPUT PARAM ocValue        AS CHAR   NO-UNDO.  /* Error message. If <> blank the transaction is backed out */

FIND KampanjeLinje WHERE KampanjeLinje.KampanjeId = ihBuffer:BUFFER-FIELD("KampanjeId"):BUFFER-VALUE AND 
  Kampanjelinje.Vg = ihBuffer:BUFFER-FIELD("Vg"):BUFFER-VALUE AND 
  KampanjeLinje.LopNr =  ihBuffer:BUFFER-FIELD("LopNr"):BUFFER-VALUE 
  NO-LOCK NO-ERROR.
IF NOT AVAIL KampanjeLinje THEN DO:
  ocValue = "".
  RETURN.
END.
ELSE DO:
  DELETE KampanjeLinje.
END.
