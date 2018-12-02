/* Oppretter VPI datasett poster for alle eksterne VPI leverandører.
   Parameter:  
   Opprettet: 18/7-08             
*/

DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR iOpprettet AS INT NO-UNDO.

OPPRETT_DATASETT:
FOR EACH EkstVPILev NO-LOCK WHERE
    EkstVPILev.LevNr <= 99999 AND
    NOT CAN-FIND(FIRST VPIDataSett WHERE
                 VPIDataSett.EkstVPILevNr = EkstVPILev.EkstVPILevNr):

    CREATE VPIDataSett.
    ASSIGN
        VPIDataSett.EkstVPILevNr = EkstVPILev.EkstVPILevNr
        iOpprettet               = iOpprettet + 1
        .
END. /* OPPRETT_DATASETT */

ASSIGN
    ocReturn = IF iOpprettet > 0
                 THEN 'Det ble opprettet ' + STRING(iOpprettet) + ' poster'
                 ELSE 'Ingen poster ble opprettet'
    obOk     = TRUE
    .

