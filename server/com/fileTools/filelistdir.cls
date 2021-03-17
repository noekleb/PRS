
&SCOPED-DEFINE PackageName com.filetools
ROUTINE-LEVEL ON ERROR UNDO, THROW . 

USING {&PackageName}.fileinfo.

CLASS {&PackageName}.filelistdir INHERITS {&PackageName}.fileutil USE-WIDGET-POOL: 
 
    DEFINE TEMP-TABLE FileInfo NO-UNDO 
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

    DEFINE VARIABLE ListFilesProcedure AS CHAR INIT "filelistdir_win.p" NO-UNDO. 
    DEFINE VARIABLE SearchResult AS CHAR NO-UNDO.
    DEFINE VARIABLE hFileList AS HANDLE NO-UNDO. 
    DEFINE VARIABLE lStatus AS LOGICAL NO-UNDO. 


    CONSTRUCTOR PUBLIC filelistdir(OUTPUT plOk AS LOGICAL):
        SUPER(lstatus).

        plOk = FALSE. 

        IF NOT lStatus THEN 
        DO:
            AddErrorMessage("SUPER() constructor failed").
            RETURN.
        END.
        
        ASSIGN SearchResult = SEARCH(ListFilesProcedure).
        IF SearchResult = ? THEN
            ASSIGN SearchResult = SEARCH(PackagePathRelative('{&PackageName}') + "/" + ListFilesProcedure).
        
        IF SearchResult = ? THEN
        DO:
            ASSIGN SearchResult = SEARCH(REPLACE(PackagePathRelative('{&PackageName}') + "/" + ListFilesProcedure, ".p",".r")).
        END.
        
        IF SearchResult = ? THEN 
        DO:
            AddErrorMessage(SUBSTITUTE("Unable to run list directory files procedure: '&1'", ListFilesProcedure)).
            RETURN.
        END.
        
        RUN VALUE(SearchResult) PERSISTENT SET hFileList (INPUT THIS-OBJECT) NO-ERROR.
        
        IF ERROR-STATUS:ERROR THEN 
        DO:
            AddErrorMessage(SUBSTITUTE("Unable to set persistent proc, list directory proc:'&1'", ListFilesProcedure)).
            RETURN.
        END.
        plOk = TRUE. 

    END CONSTRUCTOR.


    CONSTRUCTOR PUBLIC filelistdir(INPUT ipcDirectory AS CHAR , INPUT ipcFileMask AS CHAR,OUTPUT plok AS LOGICAL):
        SUPER(lstatus).

        plOk = FALSE. 

        IF NOT lStatus THEN 
        DO:
            AddErrorMessage("SUPER() constructor failed").
            RETURN.
        END.
        
        ASSIGN SearchResult = SEARCH(ListFilesProcedure).
        IF SearchResult = ? THEN
            ASSIGN SearchResult = SEARCH(PackagePathRelative('{&PackageName}') + "/" + ListFilesProcedure).
        
        IF SearchResult = ? THEN
        DO:
            ASSIGN SearchResult = SEARCH(REPLACE(PackagePathRelative('{&PackageName}') + "/" + ListFilesProcedure, ".p",".r")).
        END.
        
        IF SearchResult = ? THEN 
        DO:
            AddErrorMessage(SUBSTITUTE("Unable to run list directory files procedure: '&1'", ListFilesProcedure)).
            RETURN.
        END.
        
        RUN VALUE(SearchResult) PERSISTENT SET hFileList (INPUT THIS-OBJECT) NO-ERROR.
        
        IF ERROR-STATUS:ERROR THEN 
        DO:
            AddErrorMessage(SUBSTITUTE("Unable to set persistent proc, list directory proc:'&1'", ListFilesProcedure)).
            RETURN.
        END.

        getOSFileList(ipcDirectory, ipcFileMask, OUTPUT TABLE FileInfo,OUTPUT plok). 

        
    END CONSTRUCTOR.


    METHOD PUBLIC VOID AddFileInfo (INPUT  ipcLoadPath AS CHAR,INPUT  ipcFileName AS CHAR,INPUT  ipdDate AS DATE,INPUT  ipiTime AS INT ): 

        DEFINE VARIABLE fileInfo AS fileInfo. 
        DEFINE VARIABLE lok AS LOGICAL NO-UNDO. 

        fileinfo = NEW fileinfo(ipcLoadPath + '/' + getFileName(ipcFileName),lok).
        
        IF NOT lOK THEN 
        DO:
             AddErrorMessage("NEW fileinfo failed").
             DELETE OBJECT fileinfo NO-ERROR. 
             RETURN.
        END.

        IF NOT fileinfo:type BEGINS "F" THEN 
        DO:
            DELETE OBJECT fileinfo. 
            RETURN. 
        END. 

        LOG("Adding FileInfo:" + fileinfo:fullPath). 

        CREATE FileInfo. 
        ASSIGN
            FileInfo.id             = getUniqueId()
            FileInfo.Filename       = fileinfo:filename
            FileInfo.FullPath       = fileinfo:fullPath 
            FileInfo.FilePath       = getFilePath(FileInfo.FullPath)
            FileInfo.FileType       = fileinfo:type
            FileInfo.FileSize       = fileinfo:size
            FileInfo.CreateDateTime = fileinfo:createDateTime
            FileInfo.ModDateTime    = fileinfo:moddatetime
            FileInfo.Signature      = fileinfo:signature.

            IF FileInfo.CreateDateTime GT FileInfo.ModDateTime THEN FileInfo.CreateDateTime = FileInfo.ModDateTime. 

        DELETE OBJECT fileinfo. 
        RETURN. 
    END. 


    METHOD PUBLIC VOID getOSFileList (INPUT ipcDirectory AS CHAR , INPUT ipcFileMask AS CHAR,OUTPUT TABLE FileInfo, OUTPUT plok AS LOGICAL):

        DEFINE VARIABLE lok AS LOGICAL NO-UNDO. 
        RUN ListDirectory IN hFileList (ipcDirectory,ipcFileMask, OUTPUT lok). 

        IF NOT lOK THEN 
        DO:
             AddErrorMessage("ListDirectory failed!").
             plOk = FALSE. 
             RETURN.
        END.
        plOk = true. 
        RETURN. 
    END. 


    METHOD PUBLIC VOID getOSFileListTable (OUTPUT TABLE FileInfo, OUTPUT plok AS LOGICAL):
        plOk = true. 
        RETURN. 
    END. 


    DESTRUCTOR PUBLIC filelistDir():
        DestroySOAPService().
    END DESTRUCTOR.
    

    METHOD PRIVATE VOID DestroySOAPService():
    
    END METHOD /* DestroySOAPService() */.

END CLASS.
