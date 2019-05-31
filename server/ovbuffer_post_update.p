/* Kjøres etter oppdatering. Se prosedyre MySaveBrowseFillIn i kallende rutine  
   
-----------------------------------------------------------------------------------------*/   
DEF INPUT  PARAM ihBuffer       AS HANDLE NO-UNDO.  /* Handle to current buffer. Her: Ovbuffer */
DEF INPUT  PARAM icAction       AS CHAR   NO-UNDO.  /* Create or Update */
DEF INPUT  PARAM icSessionId    AS CHAR   NO-UNDO.
DEF OUTPUT PARAM ocValue        AS CHAR   NO-UNDO.  /* Error message. If <> blank the transaction is backed out */

DEF VAR cFields       AS CHAR  NO-UNDO.  /* Last modified field */
DEF VAR cFieldValues  AS CHAR  NO-UNDO.   
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.

ASSIGN 
  cLogg = 'ovBuffer_post_update' + REPLACE(STRING(TODAY),'/','')
  .  

cFields      = DYNAMIC-FUNCTION("getCurrentValueFields" IN SOURCE-PROCEDURE).
cFieldValues = DYNAMIC-FUNCTION("getCurrentValues" IN SOURCE-PROCEDURE).

RUN bibl_loggDbFri(cLogg,'BuntNr: ' + ihBuffer:BUFFER-FIELD("BuntNr"):BUFFER-VALUE).
RUN bibl_loggDbFri(cLogg,'LinjeNr: ' + ihBuffer:BUFFER-FIELD("LinjeNr"):BUFFER-VALUE).
RUN bibl_loggDbFri(cLogg,'Frabut: ' + ihBuffer:BUFFER-FIELD("ButikkNrFra"):BUFFER-VALUE).
RUN bibl_loggDbFri(cLogg,'TilBut: ' + ihBuffer:BUFFER-FIELD("ButikkNrTil"):BUFFER-VALUE).
RUN bibl_loggDbFri(cLogg,'Fields: ' + cFields).
RUN bibl_loggDbFri(cLogg,'FieldValues: ' + cFieldValues).

FIND OvBuffer WHERE OvBuffer.BuntNr = INT(ihBuffer:BUFFER-FIELD("BuntNr"):BUFFER-VALUE) AND 
  OvBuffer.LinjeNr = INT(ihBuffer:BUFFER-FIELD("LinjeNr"):BUFFER-VALUE) 
  NO-LOCK NO-ERROR.
IF NOT AVAIL OvBuffer THEN DO:
  ocValue = "".
  RETURN.
END.
ELSE DO:
  ASSIGN 
    .
END.
