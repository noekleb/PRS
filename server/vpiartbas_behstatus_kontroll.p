/* Registrer vpiartbas_behstatus_kontroll.p
   Parameter:  
   Opprettet:       
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEFINE VARIABLE ihHandle AS HANDLE NO-UNDO.
DEFINE VARIABLE bOppdInnpris AS LOG NO-UNDO.

DEF VAR hQuery          AS HANDLE NO-UNDO.

ASSIGN 
    ocReturn = ''
    obOk     = TRUE
    .

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " NO-LOCK").
hQuery:QUERY-OPEN().

hQuery:GET-FIRST().

BLOKKEN:
REPEAT WHILE NOT hQuery:QUERY-OFF-END:

    FIND FIRST VPIArtBas WHERE VPIArtBas.EkstVPILevNr = INT(ihBuffer:BUFFER-FIELD('EkstVPILevNr'):BUFFER-VALUE)
                           AND VPIArtBas.VareNr       = STRING(ihBuffer:BUFFER-FIELD('VareNr'):BUFFER-VALUE)
                         NO-LOCK NO-ERROR.
    
    IF AVAIL VPIArtBas THEN
    DO:
      IF VPIArtBas.BehStatus >= 10 THEN 
        DO:
          ASSIGN 
            obOk = FALSE
            ocReturn = 'Det finnes artikler med behandlet status >= 10 i utvalget.'.
          LEAVE BLOKKEN.  
        END.
    END.
  hQuery:GET-NEXT().
END. /* BLOKKEN */

