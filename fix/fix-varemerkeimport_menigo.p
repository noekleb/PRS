/* fix-varemerkeimport_menigo.p */

DEF VAR cFilNavn AS CHAR NO-UNDO.
DEF VAR cLinje   AS CHAR NO-UNDO.

DEF VAR iVmId     AS INT  NO-UNDO.
DEF VAR cERPNr    AS CHAR NO-UNDO.
DEF VAR cKortNavn AS CHAR NO-UNDO.


ASSIGN
    cFilNavn = 'C:\Home\Lindbak\ANKOMMET\Varemerker_Menigo.csv'    
    .

DEF STREAM Inn.

INPUT STREAM Inn FROM VALUE(cFilNavn) NO-ECHO.
LOOPEN:
REPEAT :
    IMPORT STREAM Inn 
        cLinje.

    FIND LAST Varemerke NO-LOCK NO-ERROR.

    ASSIGN
        iVmId     = (IF AVAILABLE VareMerke THEN Varemerke.VmId + 1 ELSE 1)
        cERPNr    = ENTRY(1,cLinje,";")
        cKortNavn = ENTRY(2,cLinje,";")
        NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        NEXT LOOPEN.

    FIND VareMerke EXCLUSIVE-LOCK WHERE
        VareMerke.VmId = iVmId NO-ERROR.
    IF NOT AVAILABLE Varemerke THEN
    DO:
        CREATE Varemerke.
        ASSIGN
            VareMerke.VmId        = iVmId
            VareMerke.KortNavn    = cKortNavn
            Varemerke.Beskrivelse = cKortNavn + "(" + cERPNr + ")"
            .
    END.
    ASSIGN
        VareMerke.KortNavn    = cKortNavn
        Varemerke.Beskrivelse = cKortNavn + " (" + cERPNr + ")"
        .

END. /* LOOPEN */
INPUT STREAM Inn CLOSE.
