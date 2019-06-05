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
DEFINE VARIABLE iBuntNr AS INTEGER NO-UNDO.
DEFINE VARIABLE hdovbunt AS HANDLE NO-UNDO.
{overforing.i}

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.

ASSIGN
    bTest = TRUE 
    cLogg = 'OvBunt_Oppdater' + REPLACE(STRING(TODAY),'/','')
    .

rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.
rStandardFunksjoner:SkrivTilLogg(cLogg,
    'Start OvBunt_Oppdater' 
    ).    
rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    Parametre: ' + icParam 
    ).    

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " NO-LOCK").
hQuery:QUERY-OPEN().
hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  ASSIGN 
    ocReturn = ''
    obOk     = TRUE.

  rStandardFunksjoner:SkrivTilLogg(cLogg,
      '    Avail: ' + STRING(ihBuffer:AVAILABLE) + '.'
      ).    

  IF ihBuffer:AVAILABLE THEN 
  DO:
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '    Kjører: oppdaterOvBuffer.'
        ).  
    RUN oppdaterOvBuffer (INT(ihBuffer:BUFFER-FIELD('BuntNr'):BUFFER-VALUE)).
  END.
  hQuery:GET-NEXT().
END.

ERROR-STATUS:ERROR = FALSE.

EMPTY TEMP-TABLE tmpOverfor.

rStandardFunksjoner:SkrivTilLogg(cLogg,
    'Slutt OvBunt_Oppdater' 
    ).    

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
    
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '    Funnet ovbunt: ' + STRING(AVAILABLE OvBunt)
        ).    
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '    Funnet ovbunt: ' + STRING(OvBunt.DatoOppdatert)
        ).    
    
  IF NOT AVAILABLE OvBunt OR 
    OvBunt.DatoOppdatert <> ? THEN 
    RETURN.
  ELSE 
  OPPDATER:
  DO:    
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '    start OPPDATER'
        ).    
    RUN dovbunt.w PERSISTENT SET hdovbunt.
    RUN OppdaterTransLogg IN hdovbunt (OvBunt.BuntNr).
    IF VALID-HANDLE(hdovbunt) THEN 
      DELETE PROCEDURE hdovbunt.
    
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '    slutt OPPDATER'
        ).    
  END. /* OPPDATER */

END PROCEDURE.
