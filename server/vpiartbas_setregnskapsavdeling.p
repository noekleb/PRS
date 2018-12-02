/* Registrer innleveranse fra pakkseddel
   Parameter:  
   Opprettet: 25.11.2007             
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.


DEF VAR hQuery          AS HANDLE NO-UNDO.
DEF VAR iEkstVPILevNr   AS INT    NO-UNDO.
DEF VAR iStatus         AS INT    NO-UNDO.

ASSIGN
  iEkstVPILevNr  = INT(ENTRY(1,icParam,';'))
  iStatus        = INT(ENTRY(2,icParam,';'))
.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " NO-LOCK").
hQuery:QUERY-OPEN().

hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:

  /*Ved feil, skal alle feil samles, og sendes etter alle er lest. Gi da også stat på 
hvor mange som ble oppettet i artbas og vareboklinje*/
  DO TRANSACTION:
    FIND FIRST VPIArtBas WHERE VPIArtBas.EkstVPILevNr = iEkstVPILevNr
                           AND VPIArtBas.VareNr       = STRING(ihBuffer:BUFFER-FIELD('varenr'):BUFFER-VALUE)
                   EXCLUSIVE-LOCK NO-ERROR.
    
    IF AVAIL VPIArtBas THEN
    DO:
      VPIArtBas.RAvdNr = iStatus.
    END.
  END.
  IF AVAIL VPIArtBas THEN RELEASE VPIArtBas.
  hQuery:GET-NEXT().
END.
ASSIGN 
  ocReturn = ''
  obOk     = TRUE
.

