&SCOPED-DEFINE PackageName com.integration.ftp

USING {&PackageName}.ftpSession.

                          
DEFINE VARIABLE ftp AS ftpSession.
DEFINE VARIABLE lOK AS LOGICAL.
                            
DEFINE TEMP-TABLE ttTrans 
    FIELD FileName AS CHAR
    FIELD ExternalPath AS CHAR 
    FIELD ExternalFileName AS CHAR
    FIELD TransDate AS DATETIME 
    FIELD TransStatus  AS LOGICAL 
    FIElD TransMessage AS CHAR. 

DEFINE TEMP-TABLE FileInfo NO-UNDO 
    FIELD FileName AS CHAR FORMAT "X(30)"
    FIELD FullPath AS CHAR 
    FIELD PathName AS CHAR 
    FIELD FileSize AS INT 
    FIELD FileType AS CHAR 
    FIELD CreateDateTime AS DATETIME
    FIELD CreateTime AS INT 
    FIELD ModDateTime AS DATETIME
    FIELD ModTime AS INT
    FIELD Compress AS LOGICAL 
    FIELD CompressFileName AS CHAR
    FIELD Processed AS LOGICAL 
    INDEX idx1 CreateDateTime .

DEFINE STREAM in-stream. 


PROCEDURE ImportFiles :  
    DEFINE INPUT PARAMETER  ipcLoadPath AS CHAR NO-UNDO. 
    DEFINE INPUT PARAMETER  ipcFileSelect AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  iplMoveFile AS LOGICAL NO-UNDO. 
    DEFINE INPUT PARAMETER  ipcTargetDirectory AS CHAR NO-UNDO. 
    DEFINE INPUT PARAMETER  iplCompress AS LOGICAL NO-UNDO. 
    DEFINE OUTPUT PARAMETER oplSucess AS LOGICAL NO-UNDO. 

    DEFINE VARIABLE ipcCodePage AS CHAR NO-UNDO. 
    DEFINE VARIABLE iPathDelimiter AS INT NO-UNDO.
    DEFINE VARIABLE cFileName AS CHAR NO-UNDO. 
    DEFINE VARIABLE lSucess AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lError AS LOGICAL NO-UNDO. 

    EMPTY TEMP-TABLE FileInfo.

    ASSIGN
        iPathDelimiter = IF OPSYS = "UNIX" THEN 47 ELSE 92 
        FILE-INFO:FILE-NAME = ipcLoadPath
        ipcLoadPath         = FILE-INFO:FULL-PATHNAME. 

    LOG-MANAGER:WRITE-MESSAGE ("Loading files from:" + ipcLoadPath + CHR(iPathDelimiter) + ipcFileSelect,"EXT").
    IF ipcLoadPath = ? THEN 
        RETURN ERROR ('ImportDirectory not found!  Create Directory:' + ipcLoadPath). 
    
    INPUT STREAM in-stream FROM OS-DIR(ipcLoadPath) NO-ATTR-LIST. 
    FileLoadLoop:
    REPEAT: 
        IMPORT STREAM in-stream cFileName. 
        ipcFileSelect = IF ipcFileSelect = ? OR ipcFileSelect = '' THEN '*' ELSE ipcFileSelect.
        IF NOT (cFileName MATCHES ipcFileSelect) THEN NEXT FileLoadLoop. 
        FILE-INFO:FILE-NAME = ipcLoadPath + CHR(iPathDelimiter) + cFileName.
        
        cFileName = FILE-INFO:FULL-PATHNAME.
        
        RUN checkOSFileStatus (cFileName,OUTPUT lError).
        IF lError = true THEN 
        DO:
            LOG-MANAGER:WRITE-MESSAGE("OS-FILE:" + RETURN-VALUE,"ERR").
            NEXT FileLoadLoop.
        END.
        
        CREATE FileInfo. 
        ASSIGN
            FileInfo.Filename = cFileName
            FileInfo.FullPath = FILE-INFO:FULL-PATHNAME 
            FileInfo.PathName = FILE-INFO:PATHNAME
            FileInfo.FileType = FILE-INFO:FILE-TYPE
            FileInfo.FileSize = FILE-INFO:FILE-SIZE
            FileInfo.ModTime = FILE-INFO:FILE-MOD-TIME
            FileInfo.CreateTime = FILE-INFO:FILE-CREATE-TIME 
            FileInfo.CreateDateTime = DATETIME(FILE-INFO:FILE-CREATE-DATE,FileInfo.CreateTime * 1000)
            FileInfo.ModDateTime = DATETIME(FILE-INFO:FILE-MOD-DATE,FileInfo.ModTime * 1000).
        
        oplSucess = IF lSucess THEN TRUE ELSE oplSucess. 
        CATCH eAppError AS Progress.Lang.AppError:
                UNDO, THROW eAppError. 
        END CATCH.
        CATCH eSysError AS Progress.Lang.SysError:
               UNDO, THROW eSysError.
        END CATCH.
    END. 

    FOR EACH FileInfo:
              
        RUN ProcessSingleFile(FileInfo.Filename,iplMoveFile,ipcCodePage,ipcTargetDirectory,iplCompress,OUTPUT lSucess).
        
        oplSucess = IF lSucess THEN TRUE ELSE oplSucess. 

        CATCH eAppError AS Progress.Lang.AppError:
                UNDO, THROW eAppError. 
        END CATCH.
        CATCH eSysError AS Progress.Lang.SysError:
               UNDO, THROW eSysError.
        END CATCH.
    END.

    CATCH eAppError AS Progress.Lang.AppError:
            UNDO, THROW eAppError. 
    END CATCH.

    CATCH eSysError AS Progress.Lang.SysError:
           UNDO, THROW eSysError.
    END CATCH.
END. 






LOG-MANAGER:LOGFILE-NAME = "Ftp.log".
LOG-MANAGER:LOGGING-LEVEL = 3. 
LOG-MANAGER:CLEAR-LOG(). 


DO ON ERROR UNDO, LEAVE ON QUIT UNDO, LEAVE ON STOP UNDO, LEAVE:


                     /*
    RUN importFiles (gcUploadDirectory,
                     cFileSelector,
                     true,
                     cProcessedDirectory,
                     lCompressFile,
                     OUTPUT oplSucess). 
                       */

    ftp = NEW ftpSession("10.0.0.220","anonymous","testing", OUTPUT lOK) .

    ftp:log("testing"). 



    IF NOT lOK OR ERROR-STATUS:ERROR THEN 
        message ftp:getErrorMessage() view-as alert-box.

    ftp:PutFile('c:\tmp\test.txt','','',OUTPUT lok).

    MESSAGE  
        "Status:" lOk 
        ftp:getErrorMessage()
        
        VIEW-AS ALERT-BOX.
     
    END.

LOG-MANAGER:CLOSE-LOG(). 

DELETE OBJECT ftp NO-ERROR.
