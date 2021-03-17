/* Kjøres etter oppdatering. Se prosedyre MySaveBrowseFillIn i kallende rutine  
   pksdlhode_post_update.p
-----------------------------------------------------------------------------------------*/   
DEF INPUT  PARAM ihBuffer       AS HANDLE NO-UNDO.  /* Handle to current buffer. Her: Ovbunt */
DEF INPUT  PARAM icAction       AS CHAR   NO-UNDO.  /* Create or Update */
DEF INPUT  PARAM icSessionId    AS CHAR   NO-UNDO.
DEF OUTPUT PARAM ocValue        AS CHAR   NO-UNDO.  /* Error message. If <> blank the transaction is backed out */

DEF VAR cFields        AS CHAR  NO-UNDO.  /* Last modified field */
DEF VAR cFieldsParam   AS CHAR  NO-UNDO.   

/*cFields = DYNAMIC-FUNCTION("getCurrentValueFields" IN SOURCE-PROCEDURE) NO-ERROR.*/

/*ASSIGN                                                  */
/*  ihBuffer:BUFFER-FIELD("PkSdlOpphav"):BUFFER-VALUE = 8 */
/*  ihBuffer:BUFFER-FIELD("ButikkNr"):BUFFER-VALUE    = 16*/
/*  ihBuffer:BUFFER-FIELD("PkSdlStatus"):BUFFER-VALUE = 10*/
/*  .                                                     */

ocValue = "".
RETURN.

