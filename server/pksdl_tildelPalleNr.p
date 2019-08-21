/* Tildeler nytt pallenr til pakkseddel
   Parameter:  <PkSdlId>
   Opprettet: 07.09.07 av BHa              
-----------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER icParam     AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ihBuffer    AS HANDLE NO-UNDO.
DEFINE INPUT  PARAMETER icSessionId AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER ocReturn    AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER obOK        AS LOG NO-UNDO.

DEFINE VARIABLE hQuery   AS HANDLE NO-UNDO.
DEFINE VARIABLE iPalleNr AS INTEGER NO-UNDO.

DEFINE BUFFER bufPkSdlHode FOR PkSdlHode.

ASSIGN 
    ocReturn  = ""
    .

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " WHERE TRUE").
hQuery:QUERY-OPEN().

hQuery:GET-FIRST().

BLOKKEN:
REPEAT WHILE NOT hQuery:QUERY-OFF-END TRANSACTION:

  RUN bibl_loggDbFri.p ('pksdl_tildelPalleNr', 'PkSdlId: ' + STRING(ihBuffer:BUFFER-FIELD("PkSdlId"):BUFFER-VALUE)). 

  FIND PkSdlHode EXCLUSIVE-LOCK WHERE
    PkSdlHode.PkSdlId = DECIMAL(ihBuffer:BUFFER-FIELD("PkSdlId"):BUFFER-VALUE) NO-ERROR.
  FIND LAST bufPkSdlHode NO-LOCK USE-INDEX idx_PalleNr NO-ERROR.  
  IF AVAILABLE bufPkSdlHode AND bufPksdlHode.PalleNr > 0 THEN 
    iPalleNr = bufPkSdlHode.PalleNr + 1.
  ELSE 
    iPalleNr = 1.
  
  IF AVAILABLE PkSdlHode AND PkSdlHode.PkSdlStatus = 10 AND iPalleNr > 1 THEN 
      ASSIGN 
        PkSdlHode.PalleNr = iPalleNr.
        .

  hQuery:GET-NEXT().
END. /* BLOKKEN */


DELETE OBJECT hQuery NO-ERROR.

obOk = TRUE.
ocReturn = ''.
