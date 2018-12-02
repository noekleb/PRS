&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ClientFile NO-UNDO
    FIELD FILESEQ    AS INT 
    FIELD FileID     AS CHAR 
    FIELD FILENAME   AS CHAR FORMAT "x(60)"
    FIELD FILEPATH   AS CHAR FORMAT "x(50)"
    FIELD DestinationDirectory AS CHAR 
    FIELD fileWriteFullPath AS CHAR 
    FIELD FileWriteStatus AS LOG 
    FIELD FileWriteError AS CHAR 
    FIELD FileObject AS BLOB . 


DEFINE TEMP-TABLE ServerFile  NO-UNDO LIKE ClientFile . 

DEFINE INPUT         PARAMETER ipcType   AS CHAR NO-UNDO. 
DEFINE OUTPUT        PARAMETER opcResult AS CHAR EXTENT 2 NO-UNDO. 
DEFINE INPUT-OUTPUT  PARAMETER TABLE FOR ClientFile .


DEFINE VARIABLE cFullPath   AS CHAR NO-UNDO. 
DEFINE VARIABLE cFileName   AS CHAR NO-UNDO. 
DEFINE VARIABLE lWriteError AS LOGICAL INIT FALSE. 
DEFINE VARIABLE giCnt       AS INT NO-UNDO. 
DEFINE VARIABLE lok         AS LOGICAL NO-UNDO. 
DEFINE VARIABLE Tmp_ClientFileName AS CHAR NO-UNDO. 
DEFINE VARIABLE OS-Error-Status AS INT NO-UNDO. 
DEFINE VARIABLE cDestinationDirectory AS CHAR INIT "c:\temp\data\" NO-UNDO.


DEFINE VARIABLE  cDropServerDestination AS CHAR NO-UNDO.
DEFINE VARIABLE  cVareTellingPushFilNavn AS CHAR NO-UNDO.
DEFINE VARIABLE  cVareTellingServerDestination AS CHAR NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-getFileExtention) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFileExtention Procedure 
FUNCTION getFileExtention RETURNS CHAR (INPUT ipcFileName AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getFileName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFileName Procedure 
FUNCTION getFileName RETURNS CHAR (INPUT ipcFileName AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getFilenamePrefix) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFilenamePrefix Procedure 
FUNCTION getFilenamePrefix RETURNS CHAR (INPUT ipcFileName AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getFilePath) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFilePath Procedure 
FUNCTION getFilePath RETURNS CHAR (INPUT ipcFileName AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getParameter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getParameter Procedure 
FUNCTION getParameter RETURNS CHAR
      (INPUT  piSysHId AS INT ,
       INPUT  piSysGr  AS INT  ,
       INPUT  piParaNr AS INT ) FORWARD.

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
         HEIGHT             = 21.71
         WIDTH              = 126.4.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* 51, 1 - drop 2, - varetelling 1,klilent filnavn, 2, serverdestination. */

{syspara.i 51 1 2 cDropServerDestination}
{syspara.i 51 2 1 cVareTellingPushFilNavn}
{syspara.i 51 2 2 cVareTellingServerDestination}


IF ipcType = "VARETELLINGParam" THEN
DO:
    opcResult[1] = cVareTellingPushFilNavn. 
    opcResult[2] = cVareTellingServerDestination.
    RETURN. 
END.

IF ipcType = "FileDropParam" THEN
DO:
    opcResult[1] = "". 
    opcResult[2] = cDropServerDestination.
    RETURN. 
END.



IF LOG-MANAGER:LOGFILE-NAME NE ? THEN
LOG-MANAGER:WRITE-MESSAGE("Receive Files:" + ipcType,"RECEIVE-DATA").

DEF VAR pathDelimiter AS CHAR NO-UNDO. 
    pathDelimiter = IF OPSYS = "UNIX" THEN CHR(47) ELSE CHR(92).


FOR EACH ClientFile BY ClientFile.FileSeq: 

    lWriteError = FALSE. 

    IF ClientFile.DestinationDirectory NE "" THEN
    DO:
        FILE-INFO:FILE-NAME = ClientFile.DestinationDirectory. 
        cDestinationDirectory = FILE-INFO:FULL-PATHNAME + pathDelimiter.
    END. 

    IF cDestinationDirectory NE ""  THEN 
    DO:
        FILE-INFO:FILE-NAME = cDestinationDirectory. 
        cDestinationDirectory = FILE-INFO:FULL-PATHNAME + pathDelimiter.
    END.

    IF cDestinationDirectory = ? THEN 
       cDestinationDirectory = SESSION:TEMP-DIR . 

    IF LOG-MANAGER:LOGFILE-NAME NE ? THEN
    LOG-MANAGER:WRITE-MESSAGE("File Seq# " + STRING(ClientFile.FileSeq) + " " + QUOTER(cDestinationDirectory + ClientFile.FILENAME),"RECEIVE-DATA").

    Tmp_ClientFileName = "_" + ClientFile.FILENAME.
 
    COPY-LOB ClientFile.FileObject TO FILE cDestinationDirectory + Tmp_ClientFileName NO-ERROR.
    FILE-INFO:FILENAME = cDestinationDirectory + Tmp_ClientFileName. 
    
    IF ERROR-STATUS:ERROR OR FILE-INFO:FULL-PATHNAME = ?  THEN 
    DO:
       ClientFile.FileWriteError = "Can't write file to:" + QUOTER(cDestinationDirectory) + ClientFile.FILENAME.
       IF LOG-MANAGER:LOGFILE-NAME NE ? THEN
       LOG-MANAGER:WRITE-MESSAGE(ClientFile.FileWriteError,"ERRROR").
       lWriteError = TRUE.
    END.
    ELSE 
    DO: 
        /* if file exists rename-file and write over again */
        FILE-INFO:FILENAME = cDestinationDirectory + ClientFile.FILENAME.
        IF FILE-INFO:FULL-PATHNAME NE ? THEN
        DO:
            OS-RENAME VALUE(cDestinationDirectory + ClientFile.FILENAME) VALUE(cDestinationDirectory + getFileNamePrefix(ClientFile.FILENAME) + '' +                                    "[" +
                            STRING(YEAR(TODAY) - 2000 ,"99") +
                            STRING(MONTH(TODAY),"99")        +
                            STRING(DAY(TODAY),"99")          +
                            STRING(MTIME,"999999999") + "]."  + getFileExtention(ClientFile.FILENAME)).
        END.

        OS-RENAME VALUE(cDestinationDirectory + Tmp_ClientFileName) VALUE(cDestinationDirectory + ClientFile.FILENAME) .
        os-error-status = OS-ERROR. 
        IF os-error-status NE 0 THEN
        DO:
            ClientFile.FileWriteError = "Can't rename file - OS error#" + STRING(OS-ERROR,"99").
            IF LOG-MANAGER:LOGFILE-NAME NE ? THEN
               LOG-MANAGER:WRITE-MESSAGE(ClientFile.FileWriteError,"ERRROR").
            lWriteError = TRUE.
        END. 
    END. 

    IF lWriteError THEN 
    DO:
        IF LOG-MANAGER:LOGFILE-NAME NE ? THEN
        LOG-MANAGER:WRITE-MESSAGE("Canceling write to:" + QUOTER(cDestinationDirectory + ClientFile.FILENAME),"ERRROR").
        ClientFile.FileWriteStatus = FALSE. 
    END. 
    ELSE 
    DO:
        ClientFile.FileWriteFullPath = FILE-INFO:FULL-PATHNAME.
        ClientFile.FileWriteStatus = TRUE. 
    END. 

    MESSAGE ClientFile.FileWriteStatus VIEW-AS ALERT-BOX. 
END.


IF LOG-MANAGER:LOGFILE-NAME NE ? THEN
LOG-MANAGER:WRITE-MESSAGE("Completed","RECEIVE-DATA").

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-getFileExtention) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFileExtention Procedure 
FUNCTION getFileExtention RETURNS CHAR (INPUT ipcFileName AS CHAR) :

    ipcFilename = getFileName(ipcfilename). 
    IF  R-INDEX(ipcFileName,".") = 0 THEN RETURN ipcFilename.

    ipcFilename = SUBSTRING(ipcFileName,R-INDEX(ipcFileName,".") + 1).
    RETURN (TRIM(ipcFilename)). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getFileName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFileName Procedure 
FUNCTION getFileName RETURNS CHAR (INPUT ipcFileName AS CHAR) :

    DEF VAR pathDelimiter AS CHAR NO-UNDO. 
    pathDelimiter = IF OPSYS = "UNIX" THEN CHR(47) ELSE CHR(92).
    IF TRIM(ipcFileName) = ""  THEN RETURN ?.
    IF  R-INDEX(ipcFileName,PathDelimiter) = 0 THEN RETURN ipcFilename. 
    ipcFilename = SUBSTRING(ipcFileName,R-INDEX(ipcFileName,PathDelimiter) + 1).
    IF TRIM(ipcFileName) = ""  THEN RETURN ?. ELSE RETURN ipcFileName.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getFilenamePrefix) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFilenamePrefix Procedure 
FUNCTION getFilenamePrefix RETURNS CHAR (INPUT ipcFileName AS CHAR) :

    DEF VAR tFilename AS CHAR NO-UNDO. 
    tFilename   = getFileName(ipcFileName). 
    ipcFilename = getFileExtention(ipcfilename). 
        
    tFileName = REPLACE(tFilename,"." + ipcFilename,""). 
    RETURN tFileName. 
     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getFilePath) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFilePath Procedure 
FUNCTION getFilePath RETURNS CHAR (INPUT ipcFileName AS CHAR) :
    DEF VAR pathDelimiter AS CHAR NO-UNDO. 
    pathDelimiter = IF OPSYS = "UNIX" THEN CHR(47) ELSE CHR(92).
    IF TRIM(ipcFileName) = ""  THEN RETURN ?.
    IF  R-INDEX(ipcFileName,PathDelimiter) = 0 THEN RETURN "". 
    ipcFilename = SUBSTRING(ipcFileName,1,R-INDEX(ipcFileName,PathDelimiter)).
    IF TRIM(ipcFileName) = ""  THEN RETURN ?. ELSE RETURN ipcFileName.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getParameter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getParameter Procedure 
FUNCTION getParameter RETURNS CHAR
      (INPUT  piSysHId AS INT ,
       INPUT  piSysGr  AS INT  ,
       INPUT  piParaNr AS INT ):

  
   

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

