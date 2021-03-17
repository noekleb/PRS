
/* Registrer innleveranse fra pakkseddel
   Parameter:  
   Opprettet: 08.01.2008             
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR fArtikkelNr     AS DEC    NO-UNDO.
DEF VAR i               AS INT    NO-UNDO.
DEF VAR iNewArt         AS INT    NO-UNDO.
DEF VAR iUpdate         AS INT    NO-UNDO.
DEF VAR iNewVL          AS INT    NO-UNDO.
DEF VAR hQuery          AS HANDLE NO-UNDO.
DEFINE VARIABLE bOpprettArtBas AS LOG NO-UNDO.
DEFINE VARIABLE cFieldList     AS CHARACTER NO-UNDO.
DEFINE VARIABLE hArtPris       AS HANDLE NO-UNDO.
DEFINE BUFFER bVPIArtBAs FOR VPIArtBas.

IF NUM-ENTRIES(icParam,';') > 1 THEN
  ASSIGN
    cFieldList = ENTRY(2,icParam,';')
    icParam    = ENTRY(1,icParam,';')
    .

ASSIGN
  bOpprettArtBas = CAN-DO('1,j,Ja,Y,Yes,true',ENTRY(2,icParam,'|')).

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " NO-LOCK").
hQuery:QUERY-OPEN().

hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:

  /*Ved feil, skal alle feil samles, og sendes etter alle er lest. Gi da også stat på 
    hvor mange som ble oppettet i artbas og vareboklinje*/
  FIND bVPIArtBas WHERE bVPIArtBas.EkstVPILevNr = INTEGER(ihBuffer:BUFFER-FIELD('EkstVPILevNr'):BUFFER-VALUE) AND  
                        bVPIArtBas.VareNr       = STRING(ihBuffer:BUFFER-FIELD('VareNr'):BUFFER-VALUE) 
                        NO-LOCK NO-ERROR.
  
  IF AVAIL bVPIArtBas THEN
    fArtikkelNr = DECIMAL(bVPIArtBas.VareNr).
  ELSE
  DO:
    /*VPIArtBas finnes ikke, og er derfor gunn til å avslutte*/ 
    hQuery:GET-NEXT().
    NEXT.
  END.
  
  /* Oppretter ArtBas hvis den ikke finnes og man har valgt at den skal opprettes. */
  IF (bOpprettArtBas AND NOT CAN-FIND(ArtBas WHERE ArtBas.ArtikkelNr = DECIMAL(bVPIArtBas.VareNr))) OR 
     cFieldList <> '' THEN   
    DO:
      RUN artbas_new.p (STRING(bVPIArtBas.EkstVPILevNr) + ';' + cFieldList + ';' + bVPIArtBas.VareNr, 
                        ihBuffer, 
                        icSessionid, 
                        OUTPUT ocReturn, 
                        OUTPUT obOk).
      obOk = FALSE.      
    END.
  
  /* Priskø skal bare legges opp på de postene hvor artbas finnes. */
  IF CAN-FIND(ArtBas WHERE ArtBas.ArtikkelNr = DECIMAL(bVPIArtBas.VareNr)) THEN
  DO:
    FIND FIRST VPIArtPris OF bVPIArtBas NO-LOCK NO-ERROR.
    IF AVAILABLE VPIArtPris THEN 
    DO:
      hArtPris = BUFFER VPIArtPris:HANDLE.
      RUN vpiartbas_pris.p (icParam,hArtPris,icSessionId,OUTPUT ocReturn,OUTPUT obOk).
    END.
  END.
  IF obOk THEN
  DO TRANSACTION:
    FIND CURRENT bVPIArtBas EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL bVPIArtBas THEN
    DO:
      ASSIGN 
        iUpdate              = iUpdate + 1
        bVPIArtBas.behStatus = 90 /*Behandlet*/
        ocReturn             = 'Prisendringer er overført til priskø på valgt aktvieringsdato.'
      .
    END.
  END.
  ELSE obOk = TRUE.
  
  hQuery:GET-NEXT().
END.




