/* Kjøres etter oppdatering. Se prosedyre MySaveBrowseFillIn i kallende rutine  
   
-----------------------------------------------------------------------------------------*/   
DEF INPUT  PARAM ihBuffer       AS HANDLE NO-UNDO.  /* Handle to current buffer. Her: Ovbunt */
DEF INPUT  PARAM icAction       AS CHAR   NO-UNDO.  /* Create or Update */
DEF INPUT  PARAM icSessionId    AS CHAR   NO-UNDO.
DEF OUTPUT PARAM ocValue        AS CHAR   NO-UNDO.  /* Error message. If <> blank the transaction is backed out */

DEF VAR cFields        AS CHAR  NO-UNDO.  /* Last modified field */
DEF VAR cFieldsParam   AS CHAR  NO-UNDO.   
DEFINE VARIABLE iButNr AS INTEGER NO-UNDO.
DEFINE VARIABLE cOutletLst AS CHARACTER NO-UNDO.

{syspara.i 150 1 3 iButNr INT}
/*cFields = DYNAMIC-FUNCTION("getCurrentValueFields" IN SOURCE-PROCEDURE) NO-ERROR.*/

ASSIGN 
  ihBuffer:BUFFER-FIELD("ButikkNr"):BUFFER-VALUE    = iButNr
  ihBuffer:BUFFER-FIELD("PkSdlStatus"):BUFFER-VALUE = 10
  ihBuffer:BUFFER-FIELD("OrdreType"):BUFFER-VALUE = '90'
  ihBuffer:BUFFER-FIELD("Merknad"):BUFFER-VALUE = 'Opprettet fra eCom ' + STRING(NOW,"99/99/99 HH:MM:SS")
  .
IF CAN-DO(cOutletLst,STRING(ihBuffer:BUFFER-FIELD("ButikkNr"):BUFFER-VALUE)) THEN 
  ihBuffer:BUFFER-FIELD("PkSdlOpphav"):BUFFER-VALUE = 3.
ELSE 
  ihBuffer:BUFFER-FIELD("PkSdlOpphav"):BUFFER-VALUE = 1.

ocValue = "".
RETURN.

