

/* ---------------------------------------------------------------------- */    
/*  Windows system calls                                                  */                                                                            
/* ---------------------------------------------------------------------- */

&SCOPE MAX_PATH 260
&SCOPE FILE_ATTRIBUTE_NORMAL  128
&SCOPE INTERNET_OPEN_TYPE_PRECONFIG    0
&SCOPE INTERNET_FLAG_EXISITING_CONNECT 536870912
&SCOPE INTERNET_FLAG_PASSIVE           134217728
&SCOPE FTP_TRANSFER_TYPE_ASCII  1   
&SCOPE FTP_TRANSFER_TYPE_BINARY 2   
&SCOPE INTERNET_DEFAULT_FTP_PORT     21
&SCOPE INTERNET_DEFAULT_GOPHER_PORT  70
&SCOPE INTERNET_DEFAULT_HTTP_PORT    80
&SCOPE INTERNET_DEFAULT_HTTPS_PORT  443
&SCOPE INTERNET_DEFAULT_SOCKS_PORT 1080

&SCOPE INTERNET_SERVICE_FTP    1
&SCOPE INTERNET_SERVICE_GOPHER 2
&SCOPE INTERNET_SERVICE_HTTP   3

PROCEDURE InternetConnectA EXTERNAL "wininet.dll" PERSISTENT :
  DEFINE INPUT PARAMETER  hInternetSession  AS LONG.
  DEFINE INPUT PARAMETER  lpszServerName    AS CHAR.
  DEFINE INPUT PARAMETER  nServerPort       AS LONG.
  DEFINE INPUT PARAMETER  lpszUserName      AS CHAR.
  DEFINE INPUT PARAMETER  lpszPassword      AS CHAR.
  DEFINE INPUT PARAMETER  dwService         AS LONG.
  DEFINE INPUT PARAMETER  dwFlags           AS LONG.
  DEFINE INPUT PARAMETER  dwContext         AS LONG.
  DEFINE RETURN PARAMETER hInternetConnect  AS LONG.
END.

PROCEDURE InternetGetLastResponseInfoA EXTERNAL "wininet.dll" PERSISTENT:
  DEFINE OUTPUT PARAMETER lpdwError              AS LONG.
  DEFINE OUTPUT PARAMETER lpszBuffer             AS CHAR.
  DEFINE INPUT-OUTPUT PARAMETER lpdwBufferLength AS LONG.
  DEFINE RETURN PARAMETER iResultCode            AS LONG.
END.

PROCEDURE InternetOpenUrlA EXTERNAL "wininet.dll" PERSISTENT:
  DEFINE INPUT PARAMETER  hInternetSession  AS LONG.
  DEFINE INPUT PARAMETER  lpszUrl           AS CHAR.
  DEFINE INPUT PARAMETER  lpszHeaders       AS CHAR.
  DEFINE INPUT PARAMETER  dwHeadersLength   AS LONG.
  DEFINE INPUT PARAMETER  dwFlags           AS LONG.
  DEFINE INPUT PARAMETER  dwContext         AS LONG.
  DEFINE RETURN PARAMETER iResultCode       AS LONG.
END.

PROCEDURE InternetOpenA EXTERNAL "wininet.dll" PERSISTENT:
  DEFINE INPUT PARAMETER  sAgent            AS CHAR.
  DEFINE INPUT PARAMETER  lAccessType       AS LONG.
  DEFINE INPUT PARAMETER  sProxyName        AS CHAR.
  DEFINE INPUT PARAMETER  sProxyBypass      AS CHAR.
  DEFINE INPUT PARAMETER  lFlags            AS LONG.
  DEFINE RETURN PARAMETER iResultCode       AS LONG.
END.

PROCEDURE InternetCloseHandle EXTERNAL "wininet.dll" PERSISTENT:
  DEFINE INPUT PARAMETER  hInet             AS LONG.
  DEFINE RETURN PARAMETER iResultCode       AS LONG.
END.

PROCEDURE FtpOpenFileA EXTERNAL "wininet.dll" PERSISTENT:
    DEFINE INPUT PARAMETER  hFtpSession  AS LONG.
    DEFINE INPUT PARAMETER  lpszFileName AS LONG.
    DEFINE INPUT PARAMETER  dwAccess     AS LONG.
    DEFINE INPUT PARAMETER  dwFlags      AS LONG.
    DEFINE INPUT PARAMETER  dwContext    AS LONG.
    DEFINE RETURN PARAMETER iRetCode AS LONG.
END PROCEDURE.

PROCEDURE FtpPutFileA EXTERNAL "wininet.dll" PERSISTENT:
    DEFINE INPUT PARAMETER  hFtpSession       AS LONG.
    DEFINE INPUT PARAMETER  lpszLocalFile     AS LONG.
    DEFINE INPUT PARAMETER  lpszNewRemoteFile AS LONG.
    DEFINE INPUT PARAMETER  dwFlags           AS LONG.
    DEFINE INPUT PARAMETER  dwContext         AS LONG.
    DEFINE RETURN PARAMETER iRetCode          AS LONG.
END PROCEDURE.


PROCEDURE FtpRenameFileA EXTERNAL "wininet.dll" PERSISTENT:
    DEFINE INPUT PARAMETER  hFtpSession          AS LONG.
    DEFINE INPUT PARAMETER  lpszExisting         AS LONG.
    DEFINE INPUT PARAMETER  lpszNew              AS LONG.
    DEFINE RETURN PARAMETER iRetCode             AS LONG.
END PROCEDURE.

PROCEDURE GetLastError external "kernel32.dll" :
  DEFINE RETURN PARAMETER dwMessageID AS LONG. 
END PROCEDURE.

/* ---------------------------------------------------------------------- */


DEFINE STREAM outStream. 
DEFINE VARIABLE gcFTPSessionLogFileName AS CHAR NO-UNDO. 
DEFINE VARIABLE ghInternetSession AS INTEGER  NO-UNDO.
DEFINE VARIABLE ghFTPSession AS INTEGER  NO-UNDO.
DEFINE VARIABLE glStatus AS LOGICAL INIT TRUE NO-UNDO. 

FUNCTION CloseInternetConnection RETURNS LOGICAL (INPUT phInternetSession AS INT )  FORWARD.
FUNCTION ConnectWinInet RETURNS LOGICAL ()  FORWARD.
FUNCTION FTPConnect RETURNS LOGICAL ( INPUT pcURL AS CHAR,INPUT ipcUser AS CHAR,INPUT ipcPassword AS CHAR) FORWARD.
FUNCTION FtpDeleteFile RETURNS LOGICAL (INPUT ipcFilenName AS CHAR)  FORWARD.
FUNCTION FtpPutFile RETURNS LOGICAL (INPUT pcLocalFile AS CHAR,INPUT pcRemoteFile AS CHAR)  FORWARD.
FUNCTION InternetGetLastResponseInfo RETURNS CHARACTER ()  FORWARD.

FUNCTION ERROR RETURNS LOGICAL (INPUT ipcErrorString AS CHAR):
    OUTPUT STREAM OutStream TO VALUE(gcFTPSessionLogFileName) APPEND. 
    PUT STREAM OutStream UNFORMATTED 
        SUBSTRING(STRING(YEAR(TODAY),'9999'),3,2) + '-' +
        STRING(MONTH(TODAY),'99') + '-' +
        STRING(DAY(TODAY),'99') + ' ' + STRING(TIME,'HH:MM:SS')
        ' ERR: '
        ipcErrorString  SKIP. 
    OUTPUT STREAM OutStream CLOSE. 
    glStatus = FALSE. 
END. 

FUNCTION LOG RETURNS LOGICAL (INPUT ipcLogString AS CHAR):

    OUTPUT STREAM OutStream TO VALUE(gcFTPSessionLogFileName) APPEND. 
    PUT STREAM OutStream UNFORMATTED 
       
        SUBSTRING(STRING(YEAR(TODAY),'9999'),3,2) + '-' +
        STRING(MONTH(TODAY),'99') + '-' +
        STRING(DAY(TODAY),'99') + ' ' + STRING(TIME,'HH:MM:SS')
        ' LOG: ' 
        ipcLogString  SKIP. 

    OUTPUT STREAM OutStream CLOSE. 
END. 


FUNCTION SetMemptr RETURNS MEMPTR (INPUT ipcString AS CHAR):
    DEFINE VARIABLE lpMemptr AS MEMPTR  NO-UNDO.
    ASSIGN
        SET-SIZE(lpMemptr)     = length(ipcString) + 1
        PUT-STRING(lpMemptr,1) = ipcString.

    RETURN lpMemptr. 
END. 


FUNCTION InternetGetLastResponseInfo RETURNS CHARACTER (  ) :
    DEFINE VARIABLE cBuffer     AS CHAR NO-UNDO.
    DEFINE VARIABLE iBufferSz   AS INT INIT 4096 NO-UNDO.
    DEFINE VARIABLE iResultCode AS INT NO-UNDO.
    DEFINE VARIABLE iTemp       AS INT NO-UNDO.
  
    cBuffer = FILL(' ', iBufferSz).
    
    RUN InternetGetLastResponseInfoA (OUTPUT iResultCode,OUTPUT cBuffer,INPUT-OUTPUT iBufferSz,OUTPUT iTemp).
    ERROR( substitute('Error (&1):  &2', iResultCode,substr(cBuffer,1,iBufferSz))).
                       
    RETURN "".   
END FUNCTION.


FUNCTION CloseInternetConnection RETURNS LOGICAL
  ( input pghInternetSession as integer ) :
  
    DEFINE VARIABLE iRetCode AS INTEGER NO-UNDO.
                                                                        
    RUN InternetCloseHandle(input pghInternetSession, output iRetCode).
    RETURN iRetCode > 0.  

END FUNCTION.


FUNCTION ConnectWinInet RETURNS LOGICAL () :
  
    RUN InternetOpenA ('WebBasedAgent',{&INTERNET_OPEN_TYPE_PRECONFIG},'','',0,OUTPUT ghInternetSession).
    IF ghInternetSession = 0 THEN ERROR('Network connection is not working!').
    RETURN ghInternetSession <> 0. 
END FUNCTION.


FUNCTION FTPConnect RETURNS LOGICAL
    ( INPUT ipcURL AS CHAR,
      INPUT ipcUser AS CHAR,
      INPUT ipcPassword AS CHAR):
  
    DEFINE VARIABLE iError AS INTEGER NO-UNDO.
    
    RUN InternetConnectA(ghInternetSession,ipcURL,{&INTERNET_DEFAULT_FTP_PORT},ipcUser,ipcPassword,{&INTERNET_SERVICE_FTP},0,0,OUTPUT ghFTPSession).

    IF ghFTPSession = 0 THEN
    DO:
        RUN GetLastError(output iError).
        ERROR('InternetConnectA Failed:  ' + STRING(iError)).
        InternetGetLastResponseInfo().
        RETURN FALSE.
    END.
    
    RETURN TRUE.   
END FUNCTION.


FUNCTION FtpPutFile RETURNS LOGICAL
  ( INPUT ipcLocalFileName AS CHAR,
    INPUT ipcRemoteFileName AS CHAR) :

    DEFINE VARIABLE lpLocalFile        AS  MEMPTR  NO-UNDO.
    DEFINE VARIABLE lpNewRemoteFile    AS  MEMPTR  NO-UNDO.
    DEFINE VARIABLE fOverwirte         AS  LOG     NO-UNDO.
    DEFINE VARIABLE iRetCode           AS  INTEGER NO-UNDO.
      
    lpLocalFile = SetMemptr(ipcLocalFileName). 
    lpNewRemoteFile = SetMemptr(ipcRemoteFileName). 

    RUN FtpPutFileA(ghFTPSession,GET-POINTER-VALUE(lpLocalFile),GET-POINTER-VALUE(lpNewRemoteFile),{&FTP_TRANSFER_TYPE_BINARY},0,OUTPUT iRetCode).

    ASSIGN
        SET-SIZE(lpNewRemoteFile)     = 0
        SET-SIZE(lpLocalFile)         = 0.

    IF iRetCode = 0 THEN
    DO:
        InternetGetLastResponseInfo().
        RETURN FALSE. 
    END.
    
    LOG('*Success - File:' + ipcLocalFileName + ' sent. Remote file:' +  ipcRemoteFileName).
    RETURN TRUE.  
END FUNCTION.


FUNCTION FtpRenameFile RETURNS LOGICAL ( INPUT ipcRemoteFileName AS CHAR,
                                         INPUT ipcRemoteNewFileName AS CHAR) :

    DEFINE VARIABLE lpRemoteFile    AS  MEMPTR  NO-UNDO.
    DEFINE VARIABLE lpRemoteNewFile AS  MEMPTR  NO-UNDO.
    DEFINE VARIABLE iRetCode        AS  INTEGER NO-UNDO.
    
    ASSIGN
        lpRemoteFile = SetMemptr(ipcRemoteFileName) 
        lpRemoteNewFile = SetMemptr(ipcRemoteNewFileName). 

    RUN FtpRenameFileA(ghFTPSession,GET-POINTER-VALUE(lpRemoteFile),GET-POINTER-VALUE(lpRemoteNewFile),OUTPUT iRetCode).

    SET-SIZE(lpRemoteFile) = 0.
    SET-SIZE(lpRemoteNewFile) = 0.

    IF iRetCode = 0 THEN 
    DO:
        InternetGetLastResponseInfo().
        RETURN FALSE. 
    END.

    LOG('File Renamed from:' + ipcRemoteFileName + ' to ' + ipcRemoteNewFileName).
    RETURN TRUE. 
END FUNCTION.


/* ---------------------------------------------------------------------- */
/* ---------------------------------------------------------------------- */

DEFINE INPUT PARAMETER ipcHost     AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER ipcUser     AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER ipcPassword AS CHAR NO-UNDO. 
DEFINE INPUT PARAMETER ipcLocalFileName AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER ipcTransferFileName AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER ipcRenameToFileName AS CHAR NO-UNDO. 
DEFINE INPUT PARAMETER ipcFTPSessionLogFileName AS CHAR NO-UNDO. 
DEFINE OUTPUT PARAMETER oplStatus AS LOGICAL INIT TRUE NO-UNDO. 

DEFINE VARIABLE lFtpPutFileSuccess AS LOGICAL NO-UNDO.
DEFINE VARIABLE lFtpRenameFileSuccess AS LOGICAL NO-UNDO. 
DEFINE VARIABLE cLocalFileName AS CHAR NO-UNDO. 

IF ipcFTPSessionLogFileName = '' OR ipcFTPSessionLogFileName = ?  THEN
   ipcFTPSessionLogFileName = ENTRY(1,(SUBSTRING(THIS-PROCEDURE:FILE-NAME,R-INDEX(THIS-PROCEDURE:FILE-NAME,CHR(92)) + 1)),'.')  + '.log'.

gcFTPSessionLogFileName = ipcFTPSessionLogFileName.

DO ON ERROR UNDO, LEAVE: 
    LOG('FTP Session Starting...').

    ConnectWinInet().
    cLocalFileName = ipcLocalFileName.

    FILE-INFO:FILE-NAME = ipcLocalFileName. 
    cLocalFileName = FILE-INFO:FULL-PATHNAME. 
    ipcRenameToFileName = TRIM(ipcRenameToFileName). 
    ipcTransferFileName = TRIM(ipcTransferFileName).

    IF ipcTransferFileName = "" THEN
       ipcTransferFileName = SUBSTRING(cLocalFileName,R-INDEX(cLocalFileName,CHR(92)) + 1).

    IF FTPConnect(ipcHost,ipcUser,ipcPassword) AND ConnectWinInet() AND 
        cLocalFileName NE ? THEN
    DO:
        lFtpPutFileSuccess = FtpPutFile(cLocalFileName,ipcTransferFileName).
        IF lFtpPutFileSuccess AND ipcRenameToFileName NE "" THEN
        lFtpRenameFileSuccess = FtpRenameFile(ipcTransferFileName,ipcRenameToFileName).    
    END.
    ELSE IF cLocalFileName = ? THEN ERROR('Local file:' + ipcLocalFileName + ' does not exist!'). 
END. 

CloseInternetConnection(ghInternetSession).  
LOG('FTP Session Closed...').
RELEASE EXTERNAL PROCEDURE "wininet.dll" NO-ERROR.

oplStatus = glStatus. 

/* ---------------------------------------------------------------------- */
