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

{syspara.i 22 5 4 iPalleNr INT}
IF iPalleNr = 0 THEN
DO: 
  RUN finn_og_sett_sistePalleNr.p.
END.
iPalleNr = iPalleNr + 1.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " WHERE TRUE").
hQuery:QUERY-OPEN().

hQuery:GET-FIRST().

BLOKKEN:
REPEAT WHILE NOT hQuery:QUERY-OFF-END TRANSACTION:

  FIND PkSdlHode EXCLUSIVE-LOCK WHERE
    PkSdlHode.PkSdlId = DECIMAL(ihBuffer:BUFFER-FIELD("PkSdlId"):BUFFER-VALUE) NO-ERROR.
  IF AVAILABLE PkSdlHode AND TRIM(PkSdlHode.cPalleNr) = '' THEN 
  DO:
    ASSIGN
      PkSdlHode.cPalleNr = STRING(iPalleNr) 
      ihBuffer:BUFFER-FIELD("PkSdlId"):BUFFER-VALUE = STRING(iPalleNr)
      .
    FIND SysPara EXCLUSIVE-LOCK WHERE
        SysPara.SysHId = 22 AND
        SysPara.SysGr  = 5 AND
        SysPara.ParaNr = 4 NO-ERROR NO-WAIT.
    IF AVAILABLE SysPara AND NOT LOCKED SysPara THEN 
      SysPara.Parameter1 = STRING(iPalleNr).
    iPalleNr = iPalleNr + 1.
    RUN bibl_loggDbFri.p ('pksdl_tildelPalleNr', 
                          'PkSdlId: ' + STRING(ihBuffer:BUFFER-FIELD("PkSdlId"):BUFFER-VALUE) + ' PalleNr: ' + STRING(iPalleNr)). 
  END.


  hQuery:GET-NEXT().
END. /* BLOKKEN */


DELETE OBJECT hQuery NO-ERROR.

obOk = TRUE.
ocReturn = ''.
