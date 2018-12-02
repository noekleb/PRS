/* ---------------------------------------------------------------------- */
/* ---- Procedure Start Appserver "PRSPosFileDistrib" Scheduled job  ---- */
/* ---------------------------------------------------------------------- */

DEFINE INPUT PARAMETER ipcHost AS CHAR NO-UNDO. 
DEFINE INPUT PARAMETER ipcPort AS INT NO-UNDO. 

DEFINE VARIABLE ghServer              AS HANDLE  NO-UNDO.
DEFINE VARIABLE lok                   AS LOGICAL NO-UNDO. 
DEFINE VARIABLE gcAppserverConnection AS CHAR    NO-UNDO. 

/*10.125.250.28, port 3100 */

gcAppserverConnection = "-DirectConnect -N tcp -H " + ipcHost + " -S " + STRING(ipcPort).

CREATE SERVER ghServer.
lOk = ghServer:CONNECT(gcAppserverConnection) NO-ERROR.
             
IF lok THEN  
DO:
    RUN VALUE("runSchTask.p") ON ghserver  (INPUT "PRSPosFileDistrib").
    ghServer:DISCONNECT(). 
END.
DELETE OBJECT ghServer NO-ERROR. 

