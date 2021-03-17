/* Oppretter plListeHode post 
 -----------------------------------------------------------------------------------*/
DEF INPUT  PARAM hBuffer     AS HANDLE NO-UNDO.
DEF INPUT  PARAM icFields    AS CHAR NO-UNDO.
DEF INPUT  PARAM icValues    AS CHAR NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.

DEFINE VARIABLE lBokforingsId AS DECIMAL NO-UNDO.
DEFINE VARIABLE iLinjeNr AS INTEGER NO-UNDO.
DEFINE VARIABLE cBrukerId AS CHARACTER NO-UNDO.


lBokforingsId = DEC(DYNAMIC-FUNCTION("getValue" IN SOURCE-PROCEDURE,"BokforingsId")).
cBrukerId     = DYNAMIC-FUNCTION("getValue" IN SOURCE-PROCEDURE,"EBrukerId").

FIND Bokforingsbilag NO-LOCK WHERE 
  bokforingsbilag.BokforingsId = lBokforingsId NO-ERROR.
IF /* BokforingsBilag.EODMottatt = FALSE OR */ BokforingsBilag.SendtRegnskap = TRUE THEN 
DO:
/*  ocReturn = 'Bokforingsbilag med denne status kan ikke endres. Det er derfor ikke tillatt å registrere korreksjonsbilag.'.*/
  ocReturn = 'Bokforingsbilag er sendt regnskap og kan ikke endres.'.
  RETURN.
END. 

FIND LAST BokforingsKorrBilag NO-LOCK WHERE 
  BokforingsKorrBilag.BokForingsId = lBokforingsId NO-ERROR.
IF AVAILABLE BokforingsKorrbilag THEN 
  iLinjeNr = BokforingsKorrbilag.LinjeNr + 1.
ELSE 
  iLinjeNr = 1.
hBuffer:BUFFER-FIELD("BokforingsId"):BUFFER-VALUE =lBokforingsId.
hBuffer:BUFFER-FIELD("LinjeNr"):BUFFER-VALUE = iLinjeNr.
hBuffer:BUFFER-FIELD("TTId"):BUFFER-VALUE = INT(DYNAMIC-FUNCTION("getValue" IN SOURCE-PROCEDURE,"TTId")).
hBuffer:BUFFER-FIELD("TBId"):BUFFER-VALUE = INT(DYNAMIC-FUNCTION("getValue" IN SOURCE-PROCEDURE,"TBId")).
hBuffer:BUFFER-FIELD("BrukerId"):BUFFER-VALUE = cBrukerId /* DYNAMIC-FUNCTION("getASuserId" IN SOURCE-PROCEDURE) */.
hBuffer:BUFFER-FIELD("DatoTid"):BUFFER-VALUE = NOW.
