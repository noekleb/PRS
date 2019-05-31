/* Registrer 
   Parameter:  
   Opprettet:             
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR hQuery       AS HANDLE NO-UNDO.
/*DEFINE VARIABLE hJbApi AS HANDLE NO-UNDO.*/

DEFINE VARIABLE bTest AS LOG NO-UNDO.
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
DEFINE VARIABLE ibuntNr AS INTEGER NO-UNDO.
DEFINE VARIABLE hdovbunt AS HANDLE NO-UNDO.
{overforing.i}

ASSIGN
    bTest = TRUE 
    cLogg = 'OvBunt_Oppdater' + REPLACE(STRING(TODAY),'/','')
    .
CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " NO-LOCK").
hQuery:QUERY-OPEN().
hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  ASSIGN 
    ocReturn = ''
    obOk     = TRUE.

  IF ihBuffer:AVAILABLE THEN 
    RUN oppdaterOvBuffer (INT(ihBuffer:BUFFER-FIELD('BuntNr'):BUFFER-VALUE))).
  hQuery:GET-NEXT().
END.

ERROR-STATUS:ERROR = FALSE.

EMPTY TEMP-TABLE tmpOverfor.

RETURN.  

/* **********************  Internal Procedures  *********************** */

PROCEDURE oppdaterOvBuffer:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER piBuntNr AS INTEGER NO-UNDO.
  
  FIND OvBunt NO-LOCK WHERE
    OvBunt.BuntNr = piBuntNr NO-ERROR.
  IF NOT AVAILABLE OvBunt OR 
    OvBunt.DatoOppdatert <> ? THEN 
    RETURN.
  ELSE 
  OPPDATER:
  DO:
    
    RUN dovbunt.w PERSISTENT SET hdovbunt.
    RUN OppdaterTransLogg IN hdovbunt (OvBunt.BuntNr).
    IF VALID-HANDLE(hdovbunt) THEN 
      DELETE PROCEDURE hdovbunt.
    
  END. /* OPPDATER */

END PROCEDURE.
