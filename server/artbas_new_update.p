/* Registrer innleveranse fra pakkseddel
   Parameter:  Artikkelnr|VarebehNr
   Opprettet: 25.11.2007             
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.


DEF VAR iEkstVPILevNr   AS INT    NO-UNDO.
DEF VAR cFieldList      AS CHAR   NO-UNDO.
DEF VAR i               AS INT    NO-UNDO.
DEF VAR iNewArt         AS INT    NO-UNDO.
DEF VAR iUpdate         AS INT    NO-UNDO.
DEF VAR iNewVL          AS INT    NO-UNDO.
DEF VAR hQuery          AS HANDLE NO-UNDO.

ASSIGN
  iEkstVPILevNr  = INT(ENTRY(1,icParam,';'))
  cFieldList     = ENTRY(2,icParam,';')
.


CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " NO-LOCK").
hQuery:QUERY-OPEN().

hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:

  /*Ved feil, skal alle feil samles, og sendes etter alle er lest. Gi da også stat på 
hvor mange som ble oppettet i artbas og vareboklinje*/
  FIND VPIArtBas WHERE VPIArtBas.EkstVPILevNr = iEkstVPILevNr
                   AND VPIArtBas.VareNr       = STRING(ihBuffer:BUFFER-FIELD('varenr'):BUFFER-VALUE)
                 NO-LOCK NO-ERROR.
  IF AVAIL VPIArtBas THEN
  DO:
    RUN artbas_new.p (STRING(VPIArtBas.EkstVPILevNr) + ';' 
                        + cFieldList + ';' 
                        + STRING(ihBuffer:BUFFER-FIELD('varenr'):BUFFER-VALUE), 
                      ihBuffer,
                      icSessionid,
                      OUTPUT ocReturn,
                      OUTPUT obOk).
    
    IF obOk THEN
    DO TRANSACTION:
      iNewArt = iNewArt + 1.
      FIND vpiartbas WHERE vpiartbas.EkstVPILevNr = iEkstVPILevNr AND vpiartbas.varenr = STRING(ihBuffer:BUFFER-FIELD('varenr'):BUFFER-VALUE) EXCLUSIVE-LOCK NO-ERROR.
      IF AVAIL vpiartbas THEN
      DO:
        vpiartbas.ArtStatus = 90. /*Behandlet*/
        ocReturn = 'Det ble endret ' + STRING(iNewArt) + ' poster i artikkelregisteret'. 
        FIND CURRENT VPIArtBas NO-LOCK.
      END.
    END.
    hQuery:GET-NEXT().
  END.
  ELSE
  DO:
    hQuery:GET-NEXT().
  END.
END.



