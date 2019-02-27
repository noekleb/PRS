    
&SCOPED-DEFINE PackageName com.filetools
ROUTINE-LEVEL ON ERROR UNDO, THROW . 
        
USING {&PackageName}.filelistdir.


CLASS {&PackageName}.filexmlimport INHERITS {&PackageName}.filelistdir USE-WIDGET-POOL: 
 
DEFINE VARIABLE dir AS filelistdir.
    
    DEFINE PRIVATE TEMP-TABLE FileInfo NO-UNDO 
       FIELD id AS CHAR FORMAT "X(30)"         
       FIELD FileName AS CHAR FORMAT "X(30)"
       FIELD FullPath AS CHAR FORMAT "X(30)"
       FIELD FilePath AS CHAR FORMAT "X(30)"
       FIELD FileSize AS INT 
       FIELD FileType AS CHAR 
       FIELD CreateDateTime AS DATETIME
       FIELD CreateTime AS INT 
       FIELD ModDateTime AS DATETIME
       FIELD ModTime AS INT
       FIELD signature AS CHAR 
       INDEX idx0 IS PRIMARY CreateDateTime   
       INDEX idx1 ModDateTime
       INDEX idx2 FileName
       INDEX idx3 id
       INDEX idx4 signature. 

    DEFINE VARIABLE hFileSpoolCP AS HANDLE. 
    DEFINE VARIABLE bfhFileSpoolCP AS HANDLE. 
    DEFINE VARIABLE bfhFileSpool AS HANDLE. 
    DEFINE VARIABLE lStatus AS LOGICAL NO-UNDO. 

    DEFINE VARIABLE fileListDir AS filelistdir.
    DEFINE VARIABLE lOK AS LOGICAL NO-UNDO. 
    DEFINE VARIABLE hXMLDataSet AS HANDLE NO-UNDO. 


    CONSTRUCTOR PUBLIC filexmlimport(INPUT ipcDirectory AS CHARACTER,
                                     INPUT ipcFileMask AS CHARACTER, 
                                     INPUT-OUTPUT hBuffer AS HANDLE,
                                     INPUT ipcXSDfile AS CHAR, 
                                     OUTPUT plok AS LOGICAL):
        SUPER(lstatus).
        
        plOk = FALSE. 

        ipcXSDfile = (IF ipcXSDfile = "" THEN  ? ELSE ipcXSDfile). 

        IF NOT lStatus THEN 
        DO:
            AddErrorMessage("SUPER() constructor failed").
            RETURN.
        END.

        fileListDir = NEW fileListDir(OUTPUT lOK) .

        IF NOT lok THEN 
        DO:
            AddErrorMessage("NEW constructor failed").
            RETURN. 
        END. 
        fileListDir:getOSFileList(ipcDirectory,ipcFileMask,OUTPUT TABLE FileInfo,OUTPUT lok). 

        IF NOT lok THEN 
        DO: 
            RETURN. 
        END.

        IF NOT VALID-HANDLE(hBuffer) THEN CREATE DATASET hXMLDataSet. 

        FOR EACH FileInfo : 
            Log("Loading xml file:" + fileinfo.fullpath + " ....").

            /* IF fileInfo.fileSize = 0 OR fileInfo.fileSIZE = ? THEN NEXT. */

            IF NOT VALID-HANDLE(hBuffer) THEN ASSIGN lok =  hxmlDataset:READ-XML('file',FileInfo.fullPath,'APPEND',ipcXSDFile,?,?,'ignore') NO-ERROR.
            ELSE  ASSIGN lok =  hBuffer:READ-XML('file',FileInfo.fullPath,'APPEND',ipcXSDFile,?,?,'ignore') NO-ERROR.
 
            IF ERROR-STATUS:ERROR OR NOT lok THEN
            DO:
                LOG("Xml file load for file:" + fileinfo.fullpath + " failed!").
            END. 
        END. 

        IF NOT VALID-HANDLE(hBuffer) THEN hBuffer = hxmlDataset:GET-BUFFER-HANDLE(1).

        EMPTY TEMP-TABLE fileInfo. 
        plOk = TRUE. 
    END. 



    CONSTRUCTOR PUBLIC filexmlimport (fileDir AS CLASS {&PackageName}.filelistdir,
                                      INPUT-OUTPUT hBuffer AS HANDLE,
                                      INPUT ipcXSDfile AS CHAR, 
                                      OUTPUT plok AS LOGICAL):
                                      
		SUPER (lok).
        plOk = FALSE. 

        fileDir:getOSFileListTable(OUTPUT TABLE FileInfo,OUTPUT lok).

        IF NOT lok THEN 
        DO: 
            RETURN. 
        END.
 
        IF NOT VALID-HANDLE(hBuffer) THEN CREATE DATASET hXMLDataSet. 

        FOR EACH FileInfo : 
            Log("Loading xml file:" + fileinfo.fullpath + " ....").

            /* IF fileInfo.fileSize = 0 OR fileInfo.fileSIZE = ? THEN NEXT. */

            IF NOT VALID-HANDLE(hBuffer) THEN ASSIGN lok =  hxmlDataset:READ-XML('file',FileInfo.fullPath,'APPEND',ipcXSDFile,?,?,'ignore') NO-ERROR.
            ELSE  ASSIGN lok =  hBuffer:READ-XML('file',FileInfo.fullPath,'APPEND',ipcXSDFile,?,?,'ignore') NO-ERROR.
 
            IF ERROR-STATUS:ERROR OR NOT lok THEN
            DO:
                LOG("Xml file load for file:" + fileinfo.fullpath + " failed!").
            END. 
        END. 

        IF NOT VALID-HANDLE(hBuffer) THEN hBuffer = hxmlDataset:GET-BUFFER-HANDLE(1).

        EMPTY TEMP-TABLE fileInfo. 
        DELETE OBJECT filedir. 
        plOk = TRUE. 

	END CONSTRUCTOR.


END. 
