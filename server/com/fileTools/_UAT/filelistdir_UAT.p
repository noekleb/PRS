          

&SCOPED-DEFINE PackageName com.filetools

USING {&PackageName}.filelistdir.

DEFINE VARIABLE dir AS filelistdir.
DEFINE VARIABLE lOK AS LOGICAL INIT FALSE .

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

DEFINE VARIABLE oplStatus AS LOGICAL NO-UNDO. 

LOG-MANAGER:LOGFILE-NAME = "filelistdir.log".
LOG-MANAGER:LOGGING-LEVEL = 3. 
LOG-MANAGER:CLEAR-LOG().                        



DO ON ERROR UNDO, LEAVE ON QUIT UNDO, LEAVE ON STOP UNDO, LEAVE:
              
    dir = NEW fileListDir(OUTPUT lOK) .

    IF NOT lok THEN message dir:getErrorMessage() view-as alert-box. 

    dir:getOSFileList("c:\tmp","*.txt",OUTPUT TABLE FileInfo,OUTPUT oplStatus). 

          
END.

DELETE OBJECT Dir NO-ERROR.


