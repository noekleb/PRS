
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


CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " NO-LOCK").
hQuery:QUERY-OPEN().

hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:

  /*Ved feil, skal alle feil samles, og sendes etter alle er lest. Gi da også stat på 
hvor mange som ble oppettet i artbas og vareboklinje*/
  FIND FIRST VPImottak WHERE VPImottak.VPImottakId = DEC(ihBuffer:BUFFER-FIELD('VPImottakId'):BUFFER-VALUE) NO-LOCK NO-ERROR.
  
  IF AVAIL VPImottak THEN
    fArtikkelNr = VPImottak.ArtikkelNr.
  ELSE
  DO:
    /*VPImottak finnes ikke, og er derfor gunn til å avslutte*/ 
    hQuery:GET-NEXT().
    NEXT.
  END.
  RUN vpimottak_pris.p ('',ihBuffer,icSessionId,OUTPUT ocReturn,OUTPUT obOk).
  IF obOk THEN
  DO TRANSACTION:
    FIND CURRENT VPImottak EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL VPImottak THEN
    DO:
      ASSIGN 
        iUpdate             = iUpdate + 1
        VPImottak.behStatus = 90 /*Behandlet*/
        ocReturn            = 'Det ble lagt inn ' + STRING(iNewArt) + ' poster i artikkelregisteret, ' + STRING(iNewVL) + ' i varebokregisteret. ' + STRING(iUpdate) + ' varebok artikkler ble oppdatert.'
      .
    END.
  END.
  hQuery:GET-NEXT().
END.




