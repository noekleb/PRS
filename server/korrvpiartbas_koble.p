/* Registrer innleveranse fra pakkseddel
   Parameter: 
   Opprettet:             
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.


DEF VAR iEkstVPILevNr   AS INT    NO-UNDO.
DEF VAR fArtikkelNr     AS DEC    NO-UNDO.
DEF VAR fKorrArtikkelNr AS DEC    NO-UNDO.
DEF VAR cFieldList      AS CHAR   NO-UNDO.
DEF VAR i               AS INT    NO-UNDO.
DEF VAR iNewArt         AS INT    NO-UNDO.
DEF VAR iUpdate         AS INT    NO-UNDO.
DEF VAR iNewVL          AS INT    NO-UNDO.
DEF VAR hQuery          AS HANDLE NO-UNDO.

ASSIGN
  iEkstVPILevNr   = INT(ENTRY(1,icParam,';'))
  fArtikkelNr     = DEC(ENTRY(2,icParam,';'))
  fKorrArtikkelNr = DEC(ENTRY(3,icParam,';'))
.


FIND VPIArtBas WHERE VPIArtBas.EkstVPILevNr = iEkstVPILevNr
                 AND VPIArtBas.VareNr       = STRING(fArtikkelNr)
               EXCLUSIVE-LOCK NO-ERROR.
IF AVAIL VPIArtBas THEN
DO:
  
  ASSIGN 
    vpiArtBas.KorrArtikkelNr = fKorrArtikkelNr
    vpiartbas.KorrStatus     = 10 /*Koblet*/
    ocReturn = ''
    obOk     = TRUE
  . 
END.
ELSE
DO:
  ASSIGN 
    ocReturn = ERROR-STATUS:GET-MESSAGE(1).
    obOk = FALSE
  .

END.


