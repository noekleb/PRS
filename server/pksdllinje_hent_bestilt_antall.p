/* Henter bestilt antall ved nyregistrering av pakkseddellinje. 
   Parametere: 
   
   Opprettet: 26.08.13 av TN   
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEFINE VARIABLE hQuery AS HANDLE NO-UNDO.
/*
CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " NO-LOCK").
hQuery:QUERY-OPEN().

DO:
  hQuery:GET-FIRST().
  REPEAT WHILE NOT hQuery:QUERY-OFF-END TRANSACTION:
    ocReturn = ocReturn + 
               (IF ocReturn = '' THEN '' ELSE ',') +
               STRING(STRING(ihBuffer:BUFFER-FIELD("ButikkNr"):BUFFER-VALUE)).
    hQuery:GET-NEXT().
  END.
END.

DELETE OBJECT hQuery.
*/

IF ocReturn = "" THEN obOk = TRUE.
