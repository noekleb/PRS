

DEFINE INPUT PARAMETER ipcType AS CHAR INIT "PATCH-ANALYZE" NO-UNDO. 


PROCEDURE gethostname EXTERNAL "wsock32.dll" :
  DEFINE OUTPUT       PARAMETER p-Hostname      AS CHARACTER.
  DEFINE INPUT        PARAMETER p-Length        AS LONG.
  DEFINE RETURN       PARAMETER p-Return        AS LONG.
END PROCEDURE.
 
PROCEDURE gethostbyname EXTERNAL "wsock32.dll" :
  DEFINE INPUT        PARAMETER p-Name          AS CHARACTER.
  DEFINE RETURN       PARAMETER p-Hostent       AS LONG.
END PROCEDURE.
 
PROCEDURE inet_ntoa EXTERNAL "wsock32.dll" :
  DEFINE INPUT        PARAMETER p-AddrStruct    AS LONG.
  DEFINE RETURN       PARAMETER p-AddrString    AS MEMPTR.
END PROCEDURE.
 
PROCEDURE WSAStartup EXTERNAL "wsock32.dll" :
  DEFINE INPUT        PARAMETER p-VersionReq    AS SHORT.
  DEFINE INPUT        PARAMETER ptr-WsaData     AS LONG.
  DEFINE RETURN       PARAMETER p-Return        AS LONG.
END PROCEDURE.
 
PROCEDURE WSACleanup EXTERNAL "wsock32":
  DEFINE RETURN       PARAMETER p-Return        AS LONG.
END PROCEDURE.


&SCOPED-DEFINE WSADESCRIPTION_LEN       256
&SCOPED-DEFINE WSASYS_STATUS_LEN        128
 
&SCOPED-DEFINE WSADATA_VERSION_LOW        1    /* WORD(2)  */
&SCOPED-DEFINE WSADATA_VERSION_HIGH       3    /* WORD(2)  */
&SCOPED-DEFINE WSADATA_DESCRIPTION        5    /* CHAR(WSADESCRIPTION_LEN + 1) */ 
&SCOPED-DEFINE WSADATA_SYSTEM_STATUS    262    /* CHAR(WSASYS_STATUS_LEN + 1)  */ 
&SCOPED-DEFINE WSADATA_MAX_SOCKETS      391    /* SHORT(4) */ 
&SCOPED-DEFINE WSADATA_MAX_UDP          395    /* SHORT(4) */ 
&SCOPED-DEFINE WSADATA_VENDOR_INFO      399    /* CHAR*(4) */ 
&SCOPED-DEFINE WSADATA_LENGTH           403   
 
&SCOPED-DEFINE HOSTENT_NAME               1    /* CHAR*(4)  */
&SCOPED-DEFINE HOSTENT_ALIASES            5    /* CHAR**(4) */ 
&SCOPED-DEFINE HOSTENT_ADDR_TYPE          9    /* SHORT(2)  */ 
&SCOPED-DEFINE HOSTENT_ADDR_LENGTH       11    /* SHORT(2)  */ 
&SCOPED-DEFINE HOSTENT_ADDR_LIST         13    /* CHAR**(4) */ 
&SCOPED-DEFINE HOSTENT_LENGTH            16
 
 

PROCEDURE getTcpInfo:
    DEFINE OUTPUT PARAMETER p-TcpName      AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER p-TcpAddr      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE         w-TcpName      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE         w-Length       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE         w-Return       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE         ptr-WsaData    AS MEMPTR    NO-UNDO.
    DEFINE VARIABLE         w-Hostent      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE         ptr-Hostent    AS MEMPTR    NO-UNDO.
    DEFINE VARIABLE         ptr-AddrString AS MEMPTR    NO-UNDO.
    DEFINE VARIABLE         ptr-AddrList   AS MEMPTR    NO-UNDO.
    DEFINE VARIABLE         ptr-ListEntry  AS MEMPTR    NO-UNDO.
    DEFINE VARIABLE         w-TcpLong      AS INTEGER   NO-UNDO.
    
    ASSIGN
        p-TcpName = ?
        p-TcpAddr = ? .
    
    SET-SIZE(ptr-WsaData) = {&WSADATA_LENGTH}.
    RUN WSAStartup (INPUT  257,INPUT  GET-POINTER-VALUE(ptr-WsaData),OUTPUT w-Return).
    SET-SIZE(ptr-WsaData) = 0.
    
    IF w-Return NE 0 THEN RETURN ERROR "Error accessing WINSOCK support.". 
    ASSIGN 
        w-Length  = 100
        w-TcpName = FILL(" ", w-Length).
    RUN gethostname (OUTPUT w-TcpName,INPUT  w-Length,OUTPUT w-Return).
    IF w-Return NE 0 THEN 
    DO:
        RUN WSACleanup (OUTPUT w-Return).
        RETURN ERROR "Error getting tcp name.".
    END.
    p-TcpName = ENTRY(1,w-TcpName,CHR(0)).
    RUN gethostbyname (INPUT  w-TcpName,OUTPUT w-Hostent).
    IF w-Hostent EQ 0 THEN 
    DO:
        RUN WSACleanup (OUTPUT w-Return).
        RETURN ERROR "Error resolving host name.".
    END.
    
    SET-POINTER-VALUE(ptr-Hostent)   = w-Hostent.
    SET-POINTER-VALUE(ptr-AddrList)  = GET-LONG(ptr-Hostent,{&HOSTENT_ADDR_LIST}).
    SET-POINTER-VALUE(ptr-ListEntry) = GET-LONG(ptr-AddrList, 1).
    w-TcpLong                        = GET-LONG(ptr-ListEntry, 1).
    
    RUN inet_ntoa (INPUT  w-TcpLong,OUTPUT ptr-AddrString).
    p-TcpAddr = GET-STRING(ptr-AddrString, 1).
    RUN WSACleanup (OUTPUT w-Return).
END PROCEDURE.




DEFINE VARIABLE hServer AS HANDLE NO-UNDO. 
DEFINE VARIABLE lConnected AS LOG NO-UNDO. 
DEFINE VARIABLE cConnectionString AS CHAR NO-UNDO.
DEFINE VARIABLE cLocalIpAddress AS CHAR NO-UNDO. 
DEFINE VARIABLE cComputerName AS CHAR NO-UNDO. 
DEFINE VARIABLE iButikknr AS INT NO-UNDO. 
DEFINE VARIABLE cButikkNr AS CHAR NO-UNDO. 
DEFINE VARIABLE cKjede AS CHAR NO-UNDO. 
DEFINE VARIABLE cButikkNavn AS CHAR NO-UNDO. 
DEFINE VARIABLE cVersionNumber AS CHAR NO-UNDO. 
DEFINE VARIABLE dVersionDate AS DATE NO-UNDO. 


DEFINE VARIABLE cStoreNumber AS CHAR NO-UNDO. 
{syspara.i 5 1 1 cStoreNumber}

cConnectionString = "-URL http://appfarm.netextend.no/aia/Aia?AppService=PRSTrans".

CREATE SERVER hServer. 
lConnected = hServer:CONNECT(cConnectionString) NO-ERROR .

IF lConnected THEN
DO:
    RUN getTcpInfo (OUTPUT cComputerName, OUTPUT cLocalIpaddress) NO-ERROR.
    IF cComputername = ? OR 
       trim(cComputername) = ""  THEN
       cComputerName = OS-GETENV("COMPUTERNAME") NO-ERROR.     

    iButikkNr = INT(OS-GETENV("STORENR")) NO-ERROR.
    IF iButikkNr = ? THEN
       iButikkNr = INT(cStoreNumber) NO-ERROR. 
    
    cButikkNr = STRING(iButikknr,"999999") NO-ERROR. 
    
    FIND FIRST butiker WHERE  
               butiker.butik = iButikkNr NO-LOCK NO-ERROR. 
    
    IF NOT AVAIL butiker THEN
      FIND FIRST butiker NO-LOCK NO-ERROR. 
    
    FIND FIRST kjede NO-LOCK NO-ERROR. 
    IF AVAIL kjede THEN cKjede = TRIM(kjede.kjedeknavn). 
    
    IF cKjede = "" THEN cKjede = "Sport1". 
    
    cButikkNr = STRING(butiker.butik,"999999") NO-ERROR.
    cButikkNavn = cButikkNr + "-" + butiker.ButNamn NO-ERROR. 

    RUN skoversion.w (OUTPUT cVersionNumber,OUTPUT dVersionDate) NO-ERROR. 

    RUN setClientConnectSrv.p ON hServer 
        (cLocalIpAddress,
         hServer:CLIENT-CONNECTION-ID,
         cComputerName, 
         ipcType, 
         cKjede,
         iButikknr,
         cButikkNavn,
         PROVERSION,
         cVersionNumber, 
         dVersionDate) NO-ERROR. 
                                                          
    hServer:DISCONNECT() NO-ERROR.
END. 
DELETE OBJECT hServer NO-ERROR. 
