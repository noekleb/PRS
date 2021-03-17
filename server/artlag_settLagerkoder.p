/* Registrer 
   Parameter:  
   Opprettet:             
-----------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER icParam     AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ihBuffer    AS HANDLE NO-UNDO.
DEFINE INPUT  PARAMETER icSessionId AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER ocReturn    AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER obOK        AS LOG NO-UNDO.

DEFINE VARIABLE cLagerKoder AS CHARACTER NO-UNDO.

DEFINE VARIABLE hQuery       AS HANDLE NO-UNDO.

ASSIGN 
  cLagerkoder = ENTRY(1,icParam,'|')
  .

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " NO-LOCK").
hQuery:QUERY-OPEN().

hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END ON ERROR UNDO, LEAVE: 
  ASSIGN 
    ocReturn = ''
    obOk     = TRUE.

  FIND ArtBas WHERE 
      ArtBas.ArtikkelNr = DEC(ihBuffer:BUFFER-FIELD('ArtikkelNr'):BUFFER-VALUE)
      EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
  IF AVAILABLE ArtBas THEN 
    ArtBas.LagerKoder = cLagerKoder.

  IF AVAILABLE ArtBas THEN RELEASE ArtBas.
  hQuery:GET-NEXT().
END.

