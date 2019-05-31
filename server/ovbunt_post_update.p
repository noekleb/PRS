/* Kjøres etter oppdatering. Se prosedyre MySaveBrowseFillIn i kallende rutine  
   
-----------------------------------------------------------------------------------------*/   
DEF INPUT  PARAM ihBuffer       AS HANDLE NO-UNDO.  /* Handle to current buffer. Her: Ovbunt */
DEF INPUT  PARAM icAction       AS CHAR   NO-UNDO.  /* Create or Update */
DEF INPUT  PARAM icSessionId    AS CHAR   NO-UNDO.
DEF OUTPUT PARAM ocValue        AS CHAR   NO-UNDO.  /* Error message. If <> blank the transaction is backed out */

DEF VAR cField        AS CHAR  NO-UNDO.  /* Last modified field */
DEF VAR cFieldParam   AS CHAR  NO-UNDO.   
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.

ASSIGN 
  cLogg = 'ovbunt_post_update' + REPLACE(STRING(TODAY),'/','')
  .  

cField = DYNAMIC-FUNCTION("getCurrentValueFields" IN SOURCE-PROCEDURE) NO-ERROR.
IF NOT ERROR-STATUS:ERROR THEN
  cField = ENTRY(1,cField).

RUN bibl_loggDbFri(cLogg,'cField: ' + cField).
         
cFieldParam = DYNAMIC-FUNCTION("getInputParam" IN SOURCE-PROCEDURE) NO-ERROR.
IF cFieldParam NE "" AND (cField = ? OR cField = "") THEN
  cField = cFieldParam.

RUN bibl_loggDbFri(cLogg,'cFieldParam: ' + cFieldParam).

FIND OvBunt WHERE OvBunt.BuntNr = INT(ihBuffer:BUFFER-FIELD("BuntNr"):BUFFER-VALUE) NO-LOCK NO-ERROR.
IF NOT AVAIL Ovbunt THEN DO:
  ocValue = "".
  RETURN.
END.
ELSE DO:

  ASSIGN 
    OvBunt.Merknad = 'Nettbutikk lager'
    OvBunt.opphav  = 5 /* Nettbutikk lager */ 
    .
END.
