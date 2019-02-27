&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/* ***************************  Definitions  ************************** */

DEFINE INPUT PARAMETER iParamGruppeId AS INT INIT 1 NO-UNDO. 

DEFINE STREAM instr.

DEFINE VARIABLE cFileDirectory     AS CHAR NO-UNDO. 
DEFINE VARIABLE cFileMask          AS CHAR NO-UNDO. 
DEFINE VARIABLE cLastRunDateTime   AS CHAR NO-UNDO. 
DEFINE VARIABLE dLastRunDate       AS DATE NO-UNDO. 
DEFINE VARIABLE iLastRunTime       AS INT  NO-UNDO. 
DEFINE VARIABLE cEmailToAddress    AS CHAR NO-UNDO. 
DEFINE VARIABLE cEmailFromAddress  AS CHAR NO-UNDO. 

{syspara.i 102 iParamGruppeId 1 cFileMask}  
{syspar2.i 102 iParamGruppeId 1 cLastRunDateTime}   /*  Leser dato/tid - Pipe separert. */
{syspara.i 102 iParamGruppeId 100 cEmailToAddress}  
{syspara.i 1 1 59 cFileDirectory}  
{syspara.i 50 50 4 cEmailFromAddress}
  

ASSIGN 
    dLastRunDate = DATE(ENTRY(1,cLastRunDateTime,"|")) 
    iLastRunTime = INT(ENTRY(2,cLastRunDateTime,"|"))   
    NO-ERROR. 


DEFINE VARIABLE cErrorMessage       AS CHAR NO-UNDO. 
DEFINE VARIABLE lError              AS LOGICAL NO-UNDO. 
DEFINE VARIABLE lStatus             AS LOG NO-UNDO. 
DEFINE VARIABLE cBkuFileName        AS CHAR NO-UNDO. 
DEFINE VARIABLE giCounter           AS INT NO-UNDO. 
DEFINE VARIABLE giImportLineCounter AS INT NO-UNDO. 
DEFINE VARIABLE glSendMail          AS LOG INIT TRUE NO-UNDO. 


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

DEFINE TEMP-TABLE ImportFile NO-UNDO 
    FIELD ImportFileName AS CHAR 
    FIELD ImportLineNr AS INT 
    FIELD ImportLine AS CHAR
    INDEX idx0 IS PRIMARY ImportLineNr.

/* Default values */
ASSIGN 
    cFileDirectory    = IF cFileDirectory = ''    THEN "c:\appdir\se\log" ELSE cFileDirectory 
    cFileMask         = IF cFileMask = ''         THEN "VPILog_*.txt"     ELSE cFileMask 
    cEmailToAddress   = IF cEmailToAddress = ''   THEN "tomn@polygonsoftware.no"      ELSE cEmailToAddress
    cEmailFromAddress = IF cEmailFromAddress = '' THEN "test@polygon.no"  ELSE cEmailFromAddress.

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

DEFINE VARIABLE cTempFile AS CHAR  NO-UNDO. 
DEFINE VARIABLE oplStatus AS CHAR NO-UNDO. 
DEFINE VARIABLE dLastCreateDate AS DATE NO-UNDO. 
DEFINE VARIABLE iLastCreateTime AS INT NO-UNDO. 
                                                                        
RUN prg/getOsFiles.p (cFileDirectory, cFileMask,'CreateFileEntry',THIS-PROCEDURE) NO-ERROR. 

FOR EACH FileInfo NO-LOCK 
         BY CreateDate BY CreateTime:

    RUN getFileContent(FileInfo.FullPath) NO-ERROR.
    
    IF ERROR-STATUS:ERROR THEN 
    ASSIGN 
        lError = TRUE 
        cErrorMessage = RETURN-VALUE.
    
    ELSE lError = FALSE.
    
    ASSIGN 
        dLastCreateDate = FileInfo.CreateDate 
        iLastCreateTime = FileInfo.CreateTime. 
END. 


IF dLastCreateDate NE ? THEN
DO:
    cLastRunDateTime = STRING(dLastCreateDate) + '|' + STRING(iLastCreateTime).
    {setsyspar2.i 102 1 1 cLastRunDateTime}  
    /* Set Create date/time - for last import file */

    /* Dump content to file */ 
    cTempFile = "TMP" + TRIM(TRIM(STRING(THIS-PROCEDURE)) + TRIM(STRING(TODAY,"999999")) + TRIM(STRING(TIME))) + ".txt".
    OUTPUT STREAM instr TO VALUE(cTempFile). 
    FOR EACH ImportFile : 
        PUT STREAM instr UNFORMATTED ImportFile.ImportLine SKIP. 
    END.
    
    FILE-INFO:FILENAME = cTempFile. 
    cTempFile = FILE-INFO:FULL-PATHNAME. 
    
    IF glSendMail THEN
       RUN SendMail (cEmailToAddress,cEmailFromAddress,"Log File","Informasjon vedlagt",cTempFile, OUTPUT oplStatus).
    
    OS-DELETE VALUE(cTempFile) NO-ERROR.   
END.

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
        
        IF FILE-INFO:FILE-CREATE-DATE LT dLastRunDate THEN RETURN.  

        IF FILE-INFO:FILE-CREATE-DATE =  dLastRunDate AND 
           FILE-INFO:FILE-CREATE-TIME LE iLastRunTime THEN RETURN.  

        CREATE FileInfo. 
        ASSIGN
            FileInfo.id             = getUniqueId()
            FileInfo.FILENAME       = getFileName(cFileName)
            FileInfo.FullPath       = FILE-INFO:FULL-PATHNAME 
            FileInfo.FilePath       = getFilePath(FILE-INFO:PATHNAME)
            FileInfo.FileType       = FILE-INFO:FILE-TYPE
            FileInfo.FileSize       = FILE-INFO:FILE-SIZE
            FileInfo.ModTime        = FILE-INFO:FILE-MOD-TIME
            FileInfo.CreateTime     = FILE-INFO:FILE-CREATE-TIME 
            FileInfo.CreateDate     = FILE-INFO:FILE-CREATE-DATE
            FileInfo.ModDate        = FILE-INFO:FILE-MOD-DATE.
            /*
           IF FileInfo.CreateDate GT FileInfo.ModDate THEN 
                ASSIGN 
                FileInfo.CreateDate = FileInfo.ModDate
                FileInfo.CreateTime = FileInfo.ModTime . 
           */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getFileContent) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getFileContent Procedure 
PROCEDURE getFileContent :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ipcFileName AS CHAR NO-UNDO. 

INPUT STREAM instr FROM VALUE(ipcFileName). 
REPEAT: 
    giImportLineCounter = giImportLineCounter + 1 . 
    CREATE ImportFile. 
    ImportFile.ImportFileName = ipcFileName. 
    ImportFile.ImportLineNr = giImportLineCounter. 
    IMPORT STREAM instr UNFORMATTED ImportFile.ImportLine. 

END.
INPUT STREAM instr CLOSE. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SendMail) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendMail Procedure 
PROCEDURE SendMail :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcMailTo AS CHAR NO-UNDO.  
    DEFINE INPUT PARAMETER ipcMailFrom AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER ipcMailSubject AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER ipcMailBody AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER ipcMailAttachmentFiles AS CHAR NO-UNDO. 
    DEFINE OUTPUT PARAMETER oplStatus AS LOG NO-UNDO. 

    DEFINE VARIABLE cSMTPserver       AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cMailAuthorize    AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cMailAuthType     AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cMailUser         AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cMailPwd          AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cMailContentType  AS CHARACTER INIT 'CharSet=iso8859-1'  NO-UNDO.
    DEFINE VARIABLE cMailReceiver    AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cMailCC          AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cMailAttachments AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cMailFiles       AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE iMailImportance  AS INTEGER     NO-UNDO.
    DEFINE VARIABLE cReturn          AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE bOk              AS LOGICAL     NO-UNDO.
    
    {syspara.i 50 50 1 cSMTPserver }
    {syspara.i 50 50 2 cMailAuthorize  }
    {syspara.i 50 50 3 cMailAuthType }
    {syspara.i 50 50 4 cMailUser }
    {syspara.i 50 50 5 cMailPwd } 
       
    /*
    ASSIGN
        cSMTPserver = "mail.polygonsoftware.no:587"
        cMailAuthorize = "1"
        cMailAuthType = "base64"
        cMailUser = "prssport1@polygonsoftware.no"
        cMailPwd = "2009Sport1".
    */
     
     ASSIGN
     cMailAttachments = "Log_file.txt".
     cMailFiles       =  ipcMailAttachmentFiles.
     
     RUN prssmtpmailv5_7a.p (
            /*mailhub    */   cSMTPserver,
            /*EmailTo    */   ipcMailTo,
            /*EmailFrom  */   ipcMailFrom,
            /*EmailCC    */   cMailCC,
            /*Attachments*/   cMailAttachments,
            /*LocalFiles */   cMailFiles,
            /*Subject    */   ipcMailSubject,
            /*Body       */   ipcMailBody,
            /*MIMEHeader */   cMailContentType,
            /*BodyType   */   "",
            /*Importance */   iMailImportance,
            /*L_DoAUTH   */   IF cMailAuthorize = '1' THEN 'yes' ELSE 'no',
            /*C_AuthType */   cMailAuthType,
            /*C_User     */   cMailUser,
            /*C_Password */   cMailPwd,
            /*oSuccessful*/  OUTPUT bOk,
            /*vMessage   */  OUTPUT cReturn) NO-ERROR.
     
    oplstatus = bok. 


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


