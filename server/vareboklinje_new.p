/* Registrer innleveranse fra pakkseddel
   Parameter:  Artikkelnr|VarebehNr
   Opprettet: 25.11.2007             
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.


DEF VAR fVarebokNr      AS DEC    NO-UNDO.
DEF VAR iEkstVPILevNr   AS INT    NO-UNDO.
DEF VAR cVareNrListe    AS CHAR   NO-UNDO.
DEF VAR cFieldList      AS CHAR   NO-UNDO.
DEF VAR fArtikkelNr     AS DEC    NO-UNDO.
DEF VAR i               AS INT    NO-UNDO.
DEF VAR iNewArt         AS INT    NO-UNDO.
DEF VAR iUpdate         AS INT    NO-UNDO.
DEF VAR iNewVL          AS INT    NO-UNDO.

ASSIGN
  fVarebokNr     = DEC(ENTRY(1,icParam,';'))
  iEkstVPILevNr  = INT(ENTRY(2,icParam,';'))
  cFieldList     = ENTRY(3,icParam,';')
  cVareNrListe   = ENTRY(4,icParam,';')
.

DO i = 1 TO NUM-ENTRIES(cVareNrListe):
/*Ved feil, skal alle feil samles, og sendes etter alle er lest. Gi da også stat på 
hvor mange som ble oppettet i artbas og vareboklinje*/
  FIND VPIArtBas WHERE VPIArtBas.EkstVPILevNr = iEkstVPILevNr
                   AND VPIArtBas.VareNr       = ENTRY(i,cVareNrListe)
                 NO-LOCK NO-ERROR.
  IF AVAIL VPIArtBas THEN
    fArtikkelNr = VPIArtBas.ArtikkelNr.
  ELSE
  DO:
    /*VPIArtBas finnes ikke, og er derfor gunn til å avslutte*/ 
  END.

  IF NOT CAN-FIND(FIRST ArtBas WHERE ArtBas.artikkelnr = fArtikkelNr) THEN
  DO:
    /*Opprett artikkel i ArtBas tabell*/
    RUN artbas_new.p ('VPIArtBas' + ';' + STRING(VPIArtBas.EkstVPILevNr) + ';' + VPIArtBas.Varenr, ihBuffer, icSessionid, OUTPUT ocReturn, OUTPUT obOk).
    IF obOk THEN iNewArt = iNewArt + 1.
  END.
  
  /*Opprett artikkel i VarebokLinje*/
  IF NOT CAN-FIND(FIRST VarebokLinje WHERE Vareboklinje.vareboknr = fVarebokNr AND VarebokLinje.artikkelnr = fArtikkelNr) THEN
  DO:
    RUN art_to_varebok.p (STRING(fVarebokNr) + ',' + 'ARTNR' + ',' + STRING(fArtikkelNr)
                          ,?
                          ,icSessionId
                          ,OUTPUT ocReturn
                          ,OUTPUT obOk).
    IF obOk THEN iNewVL = iNewVL + 1.
  END.
  ELSE
  DO:
    RUN update_varebok_from_vpiartbas.p ('VPIArtBas' + ',' + STRING(iEkstVPILevNr) + ',' + ENTRY(i,cVareNrListe) + "," + STRING(fVarebokNr) + "," + cFieldList,
                                      ?,
                                      icSessionId,
                                      OUTPUT ocReturn,
                                      OUTPUT obOk).
    IF obOk THEN iUpdate = iUpdate + 1.
  END.
  IF obOk THEN
    ocReturn = 'Det ble lagt inn ' + STRING(iNewArt) + ' poster i artikkelregisteret, ' + STRING(iNewVL) + ' i varebokregisteret. ' + STRING(iUpdate) + ' varebok artikkler ble oppdatert.'. 
END.



