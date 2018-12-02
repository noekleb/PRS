
&SCOPED-DEFINE PackageName com.integration.ftp
ROUTINE-LEVEL ON ERROR UNDO, THROW . 

CLASS {&PackageName}.ftpSession  INHERITS {&PackageName}.Utility USE-WIDGET-POOL: 
 
    DEFINE VARIABLE ftpProcedure AS CHAR INIT "winftp.p" NO-UNDO. 
    DEFINE VARIABLE SearchResult AS CHAR NO-UNDO.
    DEFINE VARIABLE hFTP AS HANDLE NO-UNDO. 
    DEFINE VARIABLE lStatus AS LOGICAL NO-UNDO. 

    CONSTRUCTOR PUBLIC ftpSession( INPUT ipcHostName AS CHAR,INPUT ipcUserName AS CHAR,INPUT ipcPassword AS CHAR,OUTPUT plOk AS LOGICAL ):
        
        SUPER(lStatus). /* Start utility */ 
        
        plOk = FALSE. 

        IF NOT lStatus THEN 
        DO:
            AddErrorMessage("SUPER() constructor failed").
            RETURN.
        END.

        ASSIGN SearchResult = SEARCH(ftpProcedure).
        IF SearchResult = ? THEN
            ASSIGN SearchResult = SEARCH(PackagePath() + "/" + ftpProcedure).

        IF SearchResult = ? THEN
        DO:
            ASSIGN SearchResult = SEARCH(REPLACE(PackagePath() + "/" + ftpProcedure, ".p",".r")).
        END.

        IF SearchResult = ? THEN 
        DO:
            AddErrorMessage(SUBSTITUTE("Unable to run result parser '&1'", ftpProcedure)).
            RETURN.
        END.

        RUN VALUE(SearchResult) PERSISTENT SET hFTP (INPUT THIS-OBJECT) NO-ERROR.
                    
        IF ERROR-STATUS:ERROR THEN 
        DO:
            AddErrorMessage(SUBSTITUTE("Unable to run result parser '&1'", ftpProcedure)).
            RETURN.
        END.
                                   
        lStatus = DYNAMIC-FUNCTION('ConnectWinInet' IN hFTP ). 
        IF NOT lStatus THEN
        DO: 
            AddErrorMessage('Network connection is not working!').
             
            RETURN.
        END.

        lStatus = DYNAMIC-FUNCTION('FTPConnect'  IN hFTP ,ipcHostName,ipcUserName,ipcPassword). 
        IF NOT lStatus THEN
        DO: 
            AddErrorMessage('FTPConnect is not working!').
            RETURN.
        END.

        plOk = TRUE. 

    END CONSTRUCTOR.
        
    
    METHOD PUBLIC VOID PutFile(INPUT cLocalFileName AS CHAR,
                               INPUT cExternalPath AS CHAR,
                               INPUT cExternalFileName AS CHAR, 
                               OUTPUT oplOk AS LOG):
            
        DEFINE VARIABLE cTransferFilename AS CHAR  NO-UNDO.
        DEFINE VARIABLE cRenameToFileName AS CHAR  NO-UNDO. 
        DEFINE VARIABLE MyUUID            AS RAW   NO-UNDO.
        DEFINE VARIABLE vGUID             AS CHARACTER NO-UNDO. 
        DEFINE VARIABLE lStatus AS LOGICAL NO-UNDO. 

        oplOk = false.

        FILE-INFO:FILE-NAME = cLocalFileName. 
        cLocalFileName = FILE-INFO:FULL-PATHNAME.

        IF cLocalFileName = ? THEN 
        DO:
            AddErrorMessage('Local file not found! ' + cLocalFileName).
            RETURN. 
        END.
      
        ASSIGN 
            MyUUID            = GENERATE-UUID
            vGUID             = GUID(MyUUID)
            cExternalPath     = IF cExternalPath NE '' THEN cExternalPath + CHR(92) ELSE ''
            cTransferFileName = cExternalPath + 'ftp.' + vguid + '.file'
            cRenameToFileName = cExternalPath + SUBSTRING(cLocalFileName,R-INDEX(cLocalFileName,CHR(92)) + 1). 
 
        lStatus = DYNAMIC-FUNCTION('ftpPutFile'  IN hFTP ,cLocalFileName,cTransferFileName). 

        IF NOT lstatus THEN
        DO:
            AddErrorMessage('FTP transf. failed! ' + cLocalFileName).
            RETURN. 
        END.

        lStatus = DYNAMIC-FUNCTION('ftpRenameFile'  IN hFTP ,cTransferFileName,cRenameToFileName).
        IF NOT lstatus THEN
        DO:
            /* Delete temp-file - or do something else !! */ 
            DYNAMIC-FUNCTION('ftpDeleteFile'  IN hFTP ,cTransferFileName).
            AddErrorMessage('FTP file rename failed! ' + cTransferFileName).
            RETURN. 
        END.

        oplOK = TRUE. 

    END METHOD.
    
    DEFINE TEMP-TABLE ttTrans 
        FIELD FileName AS CHAR
        FIELD ExternalPath AS CHAR 
        FIELD ExternalFileName AS CHAR
        FIELD TransDate AS DATETIME 
        FIELD TransStatus  AS LOGICAL 
        FIElD TransMessage AS CHAR. 

    METHOD PUBLIC VOID TransferFiles(INPUT-OUTPUT TABLE ttTrans):
            
         FOR EACH ttTrans: 
              PutFile(ttTrans.fileName,ExternalPath,ExternalFileName,OUTPUT TransStatus).
              IF NOT TransStatus THEN
                  TransMessage = getErrorMessage().
              transDate = NOW. 
         END. 
         DYNAMIC-FUNCTION('ftpCloseConnection'  IN hFTP).
    END. 


    DESTRUCTOR PUBLIC ftpSession():
        DestroyFtpService().
    END DESTRUCTOR.

    METHOD PRIVATE VOID DestroyFtpService():
       RUN DestroyFTPService IN hFTP NO-ERROR. 
       DELETE OBJECT hFTP NO-ERROR. 
    END METHOD.
    
END CLASS.
