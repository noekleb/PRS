/*   
   
-----------------------------------------------------------------------------------------*/   
DEF INPUT  PARAM ihBuffer       AS HANDLE NO-UNDO.  /* Handle to current buffer. Her: PkSdlHode */
DEF INPUT  PARAM icAction       AS CHAR   NO-UNDO.  /* Create or Update */
DEF INPUT  PARAM icSessionId    AS CHAR   NO-UNDO.
DEF OUTPUT PARAM ocValue        AS CHAR   NO-UNDO.  /* Error message. If <> blank the transaction is backed out */

DEF VAR cFields       AS CHAR  NO-UNDO.  /* Last modified field */
DEF VAR cFieldValues  AS CHAR  NO-UNDO.   
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
DEFINE VARIABLE lVVareKost AS DECIMAL NO-UNDO.

ASSIGN 
  cLogg = 'pksdl_post_delete' + REPLACE(STRING(TODAY),'/','')
  .  

cFields      = DYNAMIC-FUNCTION("getCurrentValueFields" IN SOURCE-PROCEDURE).
cFieldValues = DYNAMIC-FUNCTION("getCurrentValues" IN SOURCE-PROCEDURE).

FIND PkSdlHode WHERE PkSdlHode.PkSdlId = INT(ihBuffer:BUFFER-FIELD("PkSdlId"):BUFFER-VALUE)
  NO-LOCK NO-ERROR.
IF NOT AVAIL PkSdlHode THEN DO:
  ocValue = "".
  RETURN.
END.
ELSE DO:
  IF INT(cFieldValues) = 0 THEN 
    DELETE PkSdlHode.
END.
