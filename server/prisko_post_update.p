/* Kalkuler nytt DB% etter endring av i priskø
-----------------------------------------------------------------------------------------*/   
DEF INPUT  PARAM ihBuffer       AS HANDLE NO-UNDO.  /* Handle to current buffer */
DEF INPUT  PARAM icAction       AS CHAR   NO-UNDO.  /* Create or Update */
DEF INPUT  PARAM icSessionId    AS CHAR   NO-UNDO.
DEF OUTPUT PARAM ocValue        AS CHAR   NO-UNDO.  /* Error message. If <> blank the transaction is backed out */


DEF VAR fPrisExMVA  AS DEC   NO-UNDO.

/* To get a spesific value from current update: */
/* cValues = DYNAMIC-FUNCTION("getValue" IN SOURCE-PROCEDURE,"<field name>"). */

ASSIGN fPrisExMVA  = ihBuffer:BUFFER-FIELD("Pris"):BUFFER-VALUE / (1 + ihBuffer:BUFFER-FIELD("Mva%"):BUFFER-VALUE / 100)
       ihBuffer:BUFFER-FIELD("DBKr"):BUFFER-VALUE = fPrisExMVA - ihBuffer:BUFFER-FIELD("Varekost"):BUFFER-VALUE 
       ihBuffer:BUFFER-FIELD("DB%"):BUFFER-VALUE  = ihBuffer:BUFFER-FIELD("DBKr"):BUFFER-VALUE / fPrisExMVA * 100
       .

