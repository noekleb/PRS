

&SCOPED-DEFINE PackageName com.filetools
ROUTINE-LEVEL ON ERROR UNDO, THROW . 


CLASS {&PackageName}.fileutil INHERITS com.Utility USE-WIDGET-POOL: 
 
    DEFINE VARIABLE lStatus AS LOGICAL NO-UNDO. 

    CONSTRUCTOR PUBLIC fileUtil( OUTPUT plOk AS LOGICAL):
        SUPER(lStatus).
        
        IF NOT lStatus THEN 
        DO:
            AddErrorMessage("SUPER() constructor failed").
            RETURN.
        END.
        
        ASSIGN plOK = TRUE.

        RETURN.

    END CONSTRUCTOR.

    METHOD PUBLIC LOGICAL checkOSFileStatus (INPUT  ipcFileName AS CHAR):

        DEFINE VARIABLE cStatus AS CHAR NO-UNDO. 
        DEFINE VARIABLE cTmpFile AS CHAR NO-UNDO. 
        
        DEFINE VARIABLE cOSErrorCodes AS CHAR EXTENT 20 
            INIT ["Not owner","No such file or directory","Interrupted system call","I/O error","Bad file number","No more processes",
                  "Not enough core memory","Permission denied","Bad address","File exists","No such device","Is a directory",
                  "File table overflow","Too many open files","File too large","No space left on device","Directory not empty"].
    
       
        FILE-INFO:FILE-NAME = ipcFileName .
        
        IF FILE-INFO:FULL-PATHNAME = ? THEN 
        DO:
            AddErrorMessage("Can't read file, file = " + QUOTER(ipcFilename ) + " is not available!").
            RETURN FALSE.
        END. 
                                                    
        IF FILE-INFO:FILE-TYPE NE "FRW" THEN 
        DO:
            AddErrorMessage("Can't read file, file-type = " + QUOTER(FILE-INFO:FILE-TYPE)).
            RETURN FALSE. 
        END. 

        ipcFileName = FILE-INFO:FULL-PATHNAME.
        cTmpFile  = ipcFileName + "." + getUniqueId().
        
        OS-RENAME VALUE(ipcFileName) VALUE(cTmpFIle). 
        IF OS-ERROR NE 0 THEN 
        DO:
            IF OS-ERROR GT 0 AND OS-ERROR LE 18 THEN
                 cStatus = "Can't read file or directory:" + ipcFileName + " ," + cOSErrorCodes[OS-ERROR].
            ELSE cStatus = "Can't read file:" + ipcFileName + ", unknow error code: " + STRING(OS-ERROR). 
            
            AddErrorMessage(cStatus).
            RETURN FALSE. 
        END. 
       
        OS-RENAME VALUE(cTmpFile) VALUE(ipcFileName).
        IF OS-ERROR NE 0 THEN 
        DO:
            IF OS-ERROR GT 0 AND OS-ERROR LE 18 THEN
                 cStatus = "Can't read file or directory:" + cTmpFile + " ," + cOSErrorCodes[OS-ERROR].
            ELSE cStatus = "Can't read file:" + cTmpFile + ", unknow error code: " + STRING(OS-ERROR). 
            
             AddErrorMessage(cStatus).
             RETURN FALSE. 
        END. 
    
        RETURN TRUE. 
    END. 


    METHOD PUBLIC LOGICAL RenameOsFile (INPUT ipcFromFileName AS CHAR,INPUT ipcToFileName AS CHAR): 
        
        DEFINE VARIABLE cStatus AS CHAR NO-UNDO. 
    
        DEFINE VARIABLE cOSErrorCodes AS CHAR EXTENT 20 
        INIT ["Not owner","No such file or directory","Interrupted system call","I/O error","Bad file number","No more processes",
              "Not enough core memory","Permission denied","Bad address","File exists","No such device","Is a directory",
              "File table overflow","Too many open files","File too large","No space left on device","Directory not empty"].
    
        FILE-INFO:FILE-NAME = ipcFromFileName .
        
        IF FILE-INFO:FULL-PATHNAME = ? THEN 
        DO:
            AddErrorMessage("Can't read file, file = " + QUOTER(ipcFromFileName ) + " is not available!").
            RETURN FALSE.
        END. 
        
        IF FILE-INFO:FILE-TYPE NE "FRW" THEN 
        DO:
            AddErrorMessage("Can't read file, file-type = " + QUOTER(FILE-INFO:FILE-TYPE)).
            RETURN FALSE. 
        END. 
             
        OS-RENAME VALUE(ipcFromFileName) VALUE(ipcToFileName).
        IF OS-ERROR NE 0 THEN 
        DO:
            IF OS-ERROR GT 0 AND OS-ERROR LE 18 THEN
                 cStatus = "Can't read file or directory:" + ipcFromFileName + " ," + cOSErrorCodes[OS-ERROR].
            ELSE cStatus = "Can't read file:" + ipcFromFileName + ", unknow error code: " + STRING(OS-ERROR). 
        
            AddErrorMessage(cStatus).
            RETURN FALSE. 
        END.   
        
        RETURN TRUE. 
    END. 

    METHOD PRIVATE CHAR PathDelimiter() : 
        RETURN IF OPSYS = "UNIX" THEN CHR(47) ELSE CHR(92). 
    END.

    METHOD PUBLIC CHAR getFileName (INPUT ipcFileName AS CHAR) : 
        IF TRIM(ipcFileName) = ""  THEN RETURN ?.
        IF  R-INDEX(ipcFileName,PathDelimiter()) = 0 THEN RETURN ipcFilename. 
        ipcFilename = SUBSTRING(ipcFileName,R-INDEX(ipcFileName,PathDelimiter()) + 1).
        IF TRIM(ipcFileName) = ""  THEN RETURN ?. ELSE RETURN ipcFileName.
    END. 
    
    METHOD PUBLIC CHAR getFilePath (INPUT ipcFileName AS CHAR) : 
        IF TRIM(ipcFileName) = ""  THEN RETURN ?.
        IF  R-INDEX(ipcFileName,PathDelimiter()) = 0 THEN RETURN "". 
        ipcFilename = SUBSTRING(ipcFileName,1,R-INDEX(ipcFileName,PathDelimiter())).
        IF TRIM(ipcFileName) = ""  THEN RETURN ?. ELSE RETURN ipcFileName.
    END. 


    METHOD PUBLIC LOGICAL CompressFile (INPUT ipcFileName AS CHAR):

        DEFINE VARIABLE cGzip AS CHAR NO-UNDO. 
        
        FILE-INFO:FILE-NAME = ipcFileName.
        LOG("Compress File:" + QUOTER(FILE-INFO:FILE-NAME) + " Size:" + QUOTER(FILE-INFO:FILE-SIZE)).
        
        FILE-INFO:FILE-NAME = "gzip.exe".
        cGzip = FILE-INFO:FULL-PATHNAME. 
        
        LOG("Compressing using:" + QUOTER(cGzip) + ' File:' + ipcFileName).
        OS-COMMAND SILENT VALUE(cGzip + ' ' + ipcFileName + ' -Nq1'). 
        FILE-INFO:FILE-NAME = ipcFileName + '.gz'. 
        ipcFileName = FILE-INFO:FULL-PATHNAME.  
        IF ipcFileName = ? THEN RETURN FALSE. 
    END. 


    METHOD PUBLIC LOGICAL CheckOsFile (INPUT ipcFileName AS CHAR):
        DEFINE VARIABLE cFileName AS CHAR NO-UNDO. 
        DEFINE VARIABLE cFullPath AS CHAR NO-UNDO. 
        
        cfilename = SEARCH(ipcFileName) .
        cfilename = IF cfilename = ? THEN ipcFileName 
         ELSE cfilename .
        
        FILE-INFO:FILE-NAME = cfilename.
        cfullpath = FILE-INFO:FULL-PATHNAME.
        
        IF cFullPath = ?  THEN RETURN FALSE. 
        RETURN TRUE. 
    END. 


    METHOD PUBLIC CHAR getOsFileFullPath (INPUT ipcFileName AS CHAR):

        DEFINE VARIABLE cFileName AS CHAR NO-UNDO. 
        DEFINE VARIABLE cFullPath AS CHAR NO-UNDO. 
    
        cfilename = SEARCH(ipcFileName) .
        cfilename = IF cfilename = ? THEN ipcFileName ELSE cfilename .
    
        FILE-INFO:FILE-NAME = cfilename.
        cfullpath = FILE-INFO:FULL-PATHNAME.
        IF cFullPath = ?  THEN RETURN ?. 
        RETURN cFullPath. 
    END. 


    METHOD PUBLIC CHAR getOsDirectoryFullPath (INPUT ipcDirectory AS CHAR):

        DEFINE VARIABLE cFullPath AS CHAR NO-UNDO. 
    
    
        FILE-INFO:FILE-NAME = ipcDirectory.
        cfullpath = FILE-INFO:FULL-PATHNAME.
        IF cFullPath = ?  THEN RETURN ?. 
        RETURN cFullPath. 
    END. 


    DESTRUCTOR PUBLIC fileutil():
        DestroySOAPService().
    END DESTRUCTOR.
    

    METHOD PRIVATE VOID DestroySOAPService():
    END METHOD /* DestroySOAPService() */.
    
END CLASS.

