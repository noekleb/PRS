&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/* ***************************  Definitions  ************************** */

               
DEFINE VARIABLE ipcLoadFromDirectory AS CHAR NO-UNDO. 
{syspara.i 1 1 52 ipcLoadFromDirectory}


DEFINE VARIABLE cErrorMessage       AS CHAR NO-UNDO. 
DEFINE VARIABLE lError              AS LOGICAL NO-UNDO. 
DEFINE VARIABLE lStatus             AS LOG NO-UNDO. 
DEFINE VARIABLE cBkuFileName        AS CHAR NO-UNDO. 
DEFINE VARIABLE hdlParent           AS HANDLE NO-UNDO. 
DEFINE VARIABLE giCounter           AS INT NO-UNDO. 

DEFINE TEMP-TABLE FileInfo NO-UNDO 
    FIELD id AS CHAR FORMAT "X(30)"         
    FIELD FileName AS CHAR FORMAT "X(30)"
    FIELD FullPath AS CHAR FORMAT "X(30)"
    FIELD FilePath AS CHAR FORMAT "X(30)"
    FIELD FileSize AS INT 
    FIELD FileType AS CHAR 
    FIELD CreateDate AS DATE 
    FIELD CreateTime AS INT 
    FIELD ModDate AS DATE
    FIELD ModTime AS INT
    INDEX idx0 IS PRIMARY CreateDate CreateTime  
    INDEX idx1 ModDate ModTime
    INDEX idx2 FileName
    INDEX idx3 id.

/* hard coded to look for PATCH*.zip files */ 

DEFINE VARIABLE cFileDirectory AS CHAR EXTENT 3 NO-UNDO. 
DEFINE VARIABLE cFileMask      AS CHAR EXTENT 3 NO-UNDO. 
DEFINE VARIABLE cAction        AS CHAR EXTENT 3 NO-UNDO. 

ASSIGN 
    cFileDirectory = IF ipcLoadFromDirectory = '' THEN  "c:\home\lindbak\ankommet" ELSE ipcLoadFromDirectory 
    cFileMask  = "PATCH*.zip" 
    cAction    = "zipUncompress".

hdlParent = SOURCE-PROCEDURE:HANDLE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-getFileName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFileName Procedure 
FUNCTION getFileName RETURNS CHARACTER
    (INPUT ipcFileName AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getFilePath) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFilePath Procedure 
FUNCTION getFilePath RETURNS CHARACTER
  (INPUT ipcFileName AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getUniqueId) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getUniqueId Procedure 
FUNCTION getUniqueId RETURNS CHARACTER () FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PathDelimiter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD PathDelimiter Procedure 
FUNCTION PathDelimiter RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

                                                                         
/* Add more code here to loop through all array elements and preform  actions - like unpacking and running code 
   and start processes - or other stuff  */ 
   
RUN prg/getOsFiles.p (cFileDirectory[1], cFileMask[1],'CreateFileEntry',THIS-PROCEDURE) NO-ERROR. 

FOR EACH FileInfo NO-LOCK BY CreateDate BY CreateTime:

    RUN VALUE('prg/' + cAction[1] + '.p') (FileInfo.FullPath, '', OUTPUT lStatus) NO-ERROR. 
    
    IF ERROR-STATUS:ERROR THEN 
    ASSIGN 
        lError = TRUE 
        cErrorMessage = RETURN-VALUE.
    
    ELSE lError = FALSE.
  
    cBkuFileName = FileInfo.FilePath + 'bku' + PathDelimiter() + FileInfo.FILENAME + '.' + 
                   '[' + STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") 
                       + "@" + REPLACE(REPLACE(STRING(TIME,"HH:MM:SS"),':',''),'.','+') + ']'.
        
    OS-RENAME VALUE(FileInfo.FullPath) VALUE(cBkuFileName).

    IF VALID-HANDLE(hdlParent) THEN 
    IF CAN-DO(hdlParent:INTERNAL-ENTRIES,"ShowStatus") THEN  
       RUN ShowStatus IN hdlParent ("Release completed!",FileInfo.FILENAME,cAction[1]).

END. 

IF VALID-HANDLE(hdlParent) THEN 
IF CAN-DO(hdlParent:INTERNAL-ENTRIES,"ShowStatus") THEN  
   RUN ShowStatus IN hdlParent ("Release completed!",'','').

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-CreateFileEntry) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CreateFileEntry Procedure 
PROCEDURE CreateFileEntry :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ipcLoadPath AS CHAR NO-UNDO. 
DEFINE INPUT PARAMETER ipcFileName AS CHAR NO-UNDO. 
DEFINE INPUT PARAMETER ipdDate AS DATE NO-UNDO. 
DEFINE INPUT PARAMETER ipiTime AS INT NO-UNDO. 

DEFINE VARIABLE cFileName AS CHAR NO-UNDO. 

  ASSIGN
        FILE-INFO:FILE-NAME = ipcLoadPath
        ipcLoadPath = FILE-INFO:FULL-PATHNAME. 
            
        FILE-INFO:FILE-NAME = ipcLoadPath + PathDelimiter() + ipcFileName.
        
        IF FILE-INFO:FILE-TYPE BEGINS "D"                     THEN RETURN.
        IF FILE-INFO:FILE-SIZE = 0 OR FILE-INFO:FILE-SIZE = ? THEN RETURN. 

        cFileName = FILE-INFO:FULL-PATHNAME.
        
        CREATE FileInfo. 
        ASSIGN
            FileInfo.id             = getUniqueId().
        assign     
            FileInfo.FILENAME       = getFileName(cFileName).
        assign
            FileInfo.FullPath       = FILE-INFO:FULL-PATHNAME 
            FileInfo.FilePath       = getFilePath(FILE-INFO:PATHNAME)
            FileInfo.FileType       = FILE-INFO:FILE-TYPE
            FileInfo.FileSize       = FILE-INFO:FILE-SIZE
            FileInfo.ModTime        = FILE-INFO:FILE-MOD-TIME
            FileInfo.CreateTime     = FILE-INFO:FILE-CREATE-TIME 
            FileInfo.CreateDate     = FILE-INFO:FILE-CREATE-DATE
            FileInfo.ModDate    = FILE-INFO:FILE-MOD-DATE.
            
        IF FileInfo.CreateDate GT FileInfo.ModDate THEN 
                ASSIGN 
                FileInfo.CreateDate = FileInfo.ModDate
                FileInfo.CreateTime = FileInfo.ModTime . 

        IF VALID-HANDLE(hdlParent) THEN 
           IF CAN-DO(hdlParent:INTERNAL-ENTRIES,"ShowStatus") THEN  
              RUN ShowStatus IN hdlParent ("Laster",FileInfo.FILENAME,'Patch file lasting').


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-getFileName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFileName Procedure 
FUNCTION getFileName RETURNS CHARACTER
    (INPUT ipcFileName AS CHAR) : 
    IF TRIM(ipcFileName) = ""  THEN RETURN ?.
    IF  R-INDEX(ipcFileName,PathDelimiter()) = 0 THEN RETURN ipcFilename. 
    ipcFilename = SUBSTRING(ipcFileName,R-INDEX(ipcFileName,PathDelimiter()) + 1).
    IF TRIM(ipcFileName) = ""  THEN RETURN ?. ELSE RETURN ipcFileName.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getFilePath) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFilePath Procedure 
FUNCTION getFilePath RETURNS CHARACTER
  (INPUT ipcFileName AS CHAR) : 
    IF TRIM(ipcFileName) = ""  THEN RETURN ?.
    IF  R-INDEX(ipcFileName,PathDelimiter()) = 0 THEN RETURN "". 
    ipcFilename = SUBSTRING(ipcFileName,1,R-INDEX(ipcFileName,PathDelimiter())).
    IF TRIM(ipcFileName) = ""  THEN RETURN ?. ELSE RETURN ipcFileName.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getUniqueId) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getUniqueId Procedure 
FUNCTION getUniqueId RETURNS CHARACTER ():
  
     giCounter = giCounter + 1. 
     RETURN "P" + STRING(giCounter,"9999999").

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PathDelimiter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION PathDelimiter Procedure 
FUNCTION PathDelimiter RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
RETURN IF OPSYS = "UNIX" THEN CHR(47) ELSE CHR(92). 
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

