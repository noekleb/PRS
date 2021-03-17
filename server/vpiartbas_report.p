/* Registrer Slett VPIArtBas record
   Parameter:  
   Opprettet: 25.11.2007             
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.


DEF VAR hQuery          AS HANDLE NO-UNDO.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " NO-LOCK").
hQuery:QUERY-OPEN().

hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  ASSIGN 
    ocReturn = ''
    obOk     = TRUE
  .

  DO TRANSACTION:
    FIND FIRST VPIArtBas WHERE VPIArtBas.EkstVPILevNr = INT(ihBuffer:BUFFER-FIELD('EkstVPILevNr'):BUFFER-VALUE)
                           AND VPIArtBas.VareNr       = STRING(ihBuffer:BUFFER-FIELD('VareNr'):BUFFER-VALUE)
                         EXCLUSIVE-LOCK NO-ERROR.
    
    IF AVAIL VPIArtBas THEN
    DO:
    END.
  END.
  IF AVAIL VPIArtBas THEN RELEASE VPIArtBas.
  hQuery:GET-NEXT().
END.

