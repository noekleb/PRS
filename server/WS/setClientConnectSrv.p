/* ---------------------------------------- (c) 2012 CHSO --------------- */
/* ---- Program Name : setClientConnectSrv.p                         ---- */
/* ---- Description  :                                               ---- */
/* ----                                                              ---- */
/* ---- Author       : Curt H. Oldenborg                             ---- */
/* ---- Date Started : 2013-04-2            
        Used for deployment of new code - callback to appserver 
        for registring some information regarding the "release" 
        Called from InfoPos 
------------------------------------------------------------------------- */

DEFINE INPUT PARAMETER ipcLocalIpAddress AS CHAR NO-UNDO. 
DEFINE INPUT PARAMETER ipcClientConnectionId AS CHAR NO-UNDO. 
DEFINE INPUT PARAMETER ipcComputerName AS CHAR NO-UNDO. 
DEFINE INPUT PARAMETER ipcType AS CHAR NO-UNDO. 
DEFINE INPUT PARAMETER ipcChain AS CHAR NO-UNDO. 
DEFINE INPUT PARAMETER ipiStoreNumber AS INT NO-UNDO. 
DEFINE INPUT PARAMETER ipcStoreName AS CHAR NO-UNDO. 
DEFINE INPUT PARAMETER ipcProgressVersion AS CHAR NO-UNDO. 
DEFINE INPUT PARAMETER ipcVersionNumber AS CHAR NO-UNDO. 
DEFINE INPUT PARAMETER ipdVersionDate AS DATE NO-UNDO. 

DEFINE VARIABLE MyUUID AS RAW NO-UNDO.
ASSIGN  MyUUID = GENERATE-UUID.  

CREATE ClientConnect.
ClientConnect.ClientConnectUID = GUID(MyUUID).
ClientConnect.ClientIPAddress =  ipcLocalIpAddress.
ClientConnect.ClientComputerName = ipcComputerName.
ClientConnect.ConnectType = ipcType. 
ClientConnect.ConnectDateTime = NOW.
ClientConnect.ConnectDateTimePrev = NOW.
ClientConnect.ClientConnectionId = ipcClientConnectionId.
ClientConnect.StoreChain = ipcChain.
ClientConnect.StoreNumber = ipiStoreNumber.
ClientConnect.StoreName = ipcStoreName.
ClientConnect.ProgressVersion  = ipcProgressVersion.           
ClientConnect.ClientAppVersion = ipcVersionNumber.           
ClientConnect.ClientAppVersionDate = ipdVersionDate.

