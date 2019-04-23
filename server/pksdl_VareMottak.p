/* Varemottak av en eller flere pakksedler. 
   Parameter:  <PkSdlId>
   Opprettet: 07.09.07 av BHa              
-----------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER icParam     AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ihBuffer    AS HANDLE NO-UNDO.
DEFINE INPUT  PARAMETER icSessionId AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER ocReturn    AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER obOK        AS LOG NO-UNDO.

DEFINE VARIABLE ihBufPkSdlLinje AS HANDLE NO-UNDO.
DEFINE VARIABLE iAnt AS INTEGER NO-UNDO.
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
DEFINE VARIABLE cOutletLst AS CHARACTER NO-UNDO.
DEFINE VARIABLE iCl AS INTEGER NO-UNDO.
DEFINE VARIABLE hQuery          AS HANDLE NO-UNDO.
DEFINE VARIABLE iLagereCom AS INTEGER NO-UNDO.

{syspara.i 150 1 3 iLagereCom INT}

ASSIGN 
  cLogg     = ENTRY(1,icParam,'|')  
  cLogg     = IF cLogg = '' THEN 'PkSdlBehandling' + REPLACE(STRING(TODAY),'/','') ELSE cLogg
  ocReturn  = ""
  iAnt      = 0
  .

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " WHERE TRUE").
hQuery:QUERY-OPEN().
hQuery:GET-FIRST().

BLOKKEN:
REPEAT WHILE NOT hQuery:QUERY-OFF-END TRANSACTION:
    iAnt = iAnt + 1.
    /* bare tillatt å levere inn en pakkseddel ad gangen. */
    IF iAnt > 1 THEN 
        LEAVE BLOKKEN.

    FIND PkSdlHode NO-LOCK WHERE
      PkSdlHode.PkSdlId = DECIMAL(ihBuffer:BUFFER-FIELD("PkSdlId"):BUFFER-VALUE) NO-ERROR.
    IF NOT AVAILABLE PkSdlHode THEN 
    DO:
        ASSIGN 
            obOk     = FALSE 
            ocReturn = 'Ukjent pakkseddelid: ' + STRING(ihBuffer:BUFFER-FIELD("PkSdlId"):BUFFER-VALUE) + '.'
            .
        RETURN.
    END.
    IF CAN-FIND(FIRST PkSdlLinje OF PkSdlHode WHERE PkSdlLinje.MottaksId > 0) THEN 
    DO:
        ASSIGN 
            obOk     = FALSE 
            ocReturn = 'Pakkseddel ' + STRING(ihBuffer:BUFFER-FIELD("PkSdlId"):BUFFER-VALUE) + ' er allerede mottatt.' 
            .
        RETURN.
    END.
    IF PkSdlHode.PkSdlStatus <> 10 THEN 
    DO:
        ASSIGN 
            obOk     = FALSE 
            ocReturn = 'Pakkseddel ' + STRING(ihBuffer:BUFFER-FIELD("PkSdlId"):BUFFER-VALUE) + ' har behandlingsstatus ' +  STRING(ihBuffer:BUFFER-FIELD("PkSdlStatus"):BUFFER-VALUE) + '. Pakkseddel er allerede innlevert.' 
            .
        RETURN.
    END.

    FIND FIRST PkSdlLinje OF PkSdlHode NO-LOCK NO-ERROR.
    IF NOT AVAILABLE PkSdlHode THEN 
    DO:
        ASSIGN 
            obOk     = FALSE 
            ocReturn = 'Pakkseddel ' + STRING(ihBuffer:BUFFER-FIELD("PkSdlId"):BUFFER-VALUE) + ' har ingen varelinjer.' 
            .
        RETURN.
    END.

    /* Innleverer pakkseddelen */
    RUN asPakkseddel.p(
        iLagereCom,
        PkSdlHode.PkSdlNr,
        NO,
        NO,
        OUTPUT obOk,
        OUTPUT ocReturn
        ).

    IF obOk THEN 
        RUN bibl_loggDbFri.p (cLogg, 'Innlevering av pakkseddel (PkSdlId/PkSdlNr): ' 
            + STRING(ihBuffer:BUFFER-FIELD("PkSdlId"):BUFFER-VALUE) + '/'  
            + STRING(ihBuffer:BUFFER-FIELD("PkSdlNr"):BUFFER-VALUE) + ' for butikk '  
            + STRING(PkSdlLinje.butikkNr) + '.'
            ).
    ELSE 
        RUN bibl_loggDbFri.p (cLogg, '** Innlevering feilet for pakkseddel (PkSdlId/PkSdlNr): ' 
            + STRING(ihBuffer:BUFFER-FIELD("PkSdlId"):BUFFER-VALUE) + '/'  
            + STRING(ihBuffer:BUFFER-FIELD("PkSdlNr"):BUFFER-VALUE) + ' for butikk '  
            + STRING(PkSdlLinje.butikkNr) + '.'
            ).

  hQuery:GET-NEXT().
END. /* BLOKKEN */

DELETE OBJECT hQuery NO-ERROR.



/* **********************  Internal Procedures  *********************** */

PROCEDURE getPkSdlLogfileName:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER pcLogg AS CHARACTER NO-UNDO.
  
  ASSIGN 
    pcLogg = cLogg
    .

END PROCEDURE.
