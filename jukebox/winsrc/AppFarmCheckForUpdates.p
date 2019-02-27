&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : AppFarmCheckForUpdates.p
    Purpose     : Batch script to get application updates from AppFarm database

    Syntax      :

    Description :

    Author(s)   : Brynjar Hasle
    Created     : 21.May.2012
    Notes       : Obtained from SESSION:PARAMETER:
                  ENTRY(1,";"): User name (mandatory)
                  ENTRY(2,";"): Appserver connect string (optional)
                  ENTRY(3,";"): Name of logfile (optional)
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEF VAR ix                AS INT    NO-UNDO.
DEF VAR bOk               AS LOG    NO-UNDO.
DEF VAR bTest             AS LOG    NO-UNDO.
DEF VAR cRoot             AS CHAR   NO-UNDO INIT ".\".
DEF VAR cProPath          AS CHAR   NO-UNDO.
DEF VAR bRestart          AS LOG    NO-UNDO.
DEF VAR hServer           AS HANDLE NO-UNDO.
DEF VAR cOSfile           AS CHAR   NO-UNDO.
DEF VAR cSignature        AS CHAR   NO-UNDO.
DEF VAR cLatestPackage    AS CHAR   NO-UNDO.
DEF VAR cSessionId        AS CHAR   NO-UNDO.
DEF VAR cReturnTrans      AS CHAR   NO-UNDO.
DEF VAR iNumPackages      AS INT    NO-UNDO.
DEF VAR httPackages       AS HANDLE NO-UNDO.
DEF VAR httPackagesBuffer AS HANDLE NO-UNDO.
DEF VAR httPackagesQuery  AS HANDLE NO-UNDO.
DEF VAR cAppTitle         AS CHAR   NO-UNDO.
DEF VAR cLanguages        AS CHAR   NO-UNDO.
DEF VAR cBaseLanguage     AS CHAR   NO-UNDO.
DEF VAR cPrefLanguage     AS CHAR   NO-UNDO.
DEF VAR cBehaviour        AS CHAR   NO-UNDO.
DEF VAR cUserId           AS CHAR   NO-UNDO.
DEF VAR cTemp             AS CHAR   NO-UNDO.
DEF VAR cOrgProPath       AS CHAR   NO-UNDO.
DEF VAR hOldAppservice    AS HANDLE NO-UNDO.
DEF VAR hNewAppservice    AS HANDLE NO-UNDO.
DEF VAR cOldSessionId     AS CHAR   NO-UNDO.
DEF VAR bWcUpgrade        AS LOG    NO-UNDO.
DEF VAR bViperUpgrade     AS LOG    NO-UNDO.
DEF VAR bAUIUpgrade       AS LOG    NO-UNDO.
DEF VAR iOsError          AS INT    NO-UNDO.
DEF VAR iFileSize         AS INT    NO-UNDO.
/* DEF VAR cVirtualStore     AS CHAR   NO-UNDO.                 */
/* DEF VAR cVirtualRoot      AS CHAR   NO-UNDO.                 */
DEF VAR cRenamePattern    AS CHAR   NO-UNDO INIT "*jll;dll".
DEF VAR bRunAsAdmin       AS LOG    NO-UNDO.
/* DEF VAR hWindow           AS HANDLE NO-UNDO.  */
/* DEF VAR hFrame            AS HANDLE NO-UNDO.  */
/* DEF VAR hBgFrame          AS HANDLE NO-UNDO.  */
DEF VAR cLogFile          AS CHAR   NO-UNDO.
DEF VAR cAppFarmUser      AS CHAR   NO-UNDO.

DEF VAR httDoc          AS HANDLE NO-UNDO.
DEF TEMP-TABLE ttDoc NO-UNDO
    FIELD cFileName       AS CHAR
    FIELD cFullPathName   AS CHAR
    FIELD cFileType       AS CHAR
    FIELD cDescription    AS CHAR
    FIELD iDocSize        AS INT
    FIELD blDocument      AS BLOB
    FIELD dFileCreateDate AS DATE
    FIELD iFileCreateTime AS INT
    FIELD dFileModDate    AS DATE
    FIELD iFileModTime    AS INT
    FIELD cCreatedBy      AS CHAR
    FIELD dCreated        AS DATE
    FIELD cContext        AS CHAR
    FIELD cEntityId       AS CHAR
    .
httDoc = BUFFER ttDoc:HANDLE:TABLE-HANDLE.

DEF STREAM strFile.

DEF VAR bChunkInProgress AS LOG    NO-UNDO.
DEF VAR httDocInfoBuf    AS HANDLE NO-UNDO.

DEF TEMP-TABLE ttDocInfo NO-UNDO
    FIELD iJBoxDocumentId AS INT
    FIELD iDocSize        AS INT
    FIELD cFileName       AS CHAR
    FIELD cFileType       AS CHAR
    FIELD cDescription    AS CHAR
    FIELD dFileModDate    AS DATE
    FIELD cCat            AS CHAR
    FIELD blChunk         AS BLOB
    .
httDocInfoBuf = BUFFER ttDocInfo:HANDLE.

DEF STREAM sLog.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-CheckIfAdmin) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CheckIfAdmin Procedure 
FUNCTION CheckIfAdmin RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CleanUpDir) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CleanUpDir Procedure 
FUNCTION CleanUpDir RETURNS LOGICAL
  ( INPUT icCleanUpDir AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ConnectAppFarm) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ConnectAppFarm Procedure 
FUNCTION ConnectAppFarm RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CreateInstallDir) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CreateInstallDir Procedure 
FUNCTION CreateInstallDir RETURNS LOGICAL
  ( INPUT icSaveDir AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getPackageList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getPackageList Procedure 
FUNCTION getPackageList RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-InstallPackage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD InstallPackage Procedure 
FUNCTION InstallPackage RETURNS LOGICAL
  ( INPUT icPackageId AS CHAR,
    INPUT icSaveDir   AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LogThis) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LogThis Procedure 
FUNCTION LogThis RETURNS LOGICAL
  ( INPUT icLogText   AS CHAR,
    INPUT iiSkipLines AS INT)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ProcessesStopped) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ProcessesStopped Procedure 
FUNCTION ProcessesStopped RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setVariable) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setVariable Procedure 
FUNCTION setVariable RETURNS LOGICAL
  ( INPUT icVarName AS CHAR,
    INPUT icValue   AS CHAR )  FORWARD.

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
   Other Settings: CODE-ONLY
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
IF VALID-HANDLE(SOURCE-PROCEDURE) THEN 
  ASSIGN cRoot          = DYNAMIC-FUNCTION("getRootDir" IN SOURCE-PROCEDURE)
         bTest          = TRUE
         cOrgProPath    = PROPATH
         hOldAppservice = DYNAMIC-FUNCTION("getAppServiceHandle")
         cOldSessionId  = DYNAMIC-FUNCTION("getSessionId")
         .

cAppFarmUser = ENTRY(1,SESSION:PARAMETER,";").
IF cAppFarmUser = "" THEN
  cAppFarmUser = OS-GETENV("USERNAME").

IF NUM-ENTRIES(SESSION:PARAMETER,";") > 2 THEN
  cLogFile = ENTRY(3,SESSION:PARAMETER,";").
ELSE
  cLogFile = cRoot + "AppFarmUpdateLog.log".

/* bRunAsAdmin = CheckIfAdmin(). */

IF SEARCH(cRoot + "signature.txt") NE ? THEN DO:

  INPUT FROM VALUE(cRoot + "signature.txt").
  IMPORT UNFORMATTED cSignature.
  INPUT CLOSE.
  cLatestPackage = ENTRY(2,cSignature,"|").
  
  IF NUM-ENTRIES(cSignature,"|") GE 3 THEN cAppTitle     = ENTRY(3,cSignature,"|").
  IF NUM-ENTRIES(cSignature,"|") GE 4 THEN cLanguages    = ENTRY(4,cSignature,"|").
  IF NUM-ENTRIES(cSignature,"|") GE 5 THEN cBaseLanguage = ENTRY(5,cSignature,"|").

  IF ConnectAppFarm() THEN DO:
    IF GetPackageList() AND ProcessesStopped() THEN 
      RUN GetUpdates.
    hServer:DISCONNECT() NO-ERROR.
  END.
  ELSE DO:
    IF NUM-ENTRIES(SESSION:PARAMETER,";") > 1 THEN
      LogThis("Could not connect appfarm database. Connect-string: " + ENTRY(2,SESSION:PARAMETER,";"),1).
    ELSE 
      LogThis("Could not connect appfarm database",1).
    QUIT.
  END.
  
  LogThis("Number of packages retrieved: " + STRING(iNumPackages),0).

  IF iNumPackages > 0 AND SEARCH("gzip.exe") NE ? THEN
    OS-COMMAND SILENT VALUE(SEARCH("gzip.exe") + " -dfr *.*").
    
  RUN RenameFiles (cRoot).

  IF SEARCH(cRoot + "new_jboxstartup.r") NE ? THEN DO:    
    FILE-INFO:FILE-NAME = SEARCH("jboxstartup.r").
    iFileSize = FILE-INFO:FILE-SIZE.
    FILE-INFO:FILE-NAME = SEARCH(cRoot + "new_jboxstartup.r").
    IF iFileSize NE FILE-INFO:FILE-SIZE THEN
      bRestart = TRUE.

    OS-DELETE VALUE(SEARCH("jboxstartup.r")) NO-ERROR.
    OS-COPY   VALUE(SEARCH("new_jboxstartup.r")) VALUE(cRoot + "jboxstartup.r").
    OS-DELETE VALUE(SEARCH("new_jboxstartup.r")).
  END.
  IF SEARCH(cRoot + "new_progress.ini") NE ? THEN DO:    
    FILE-INFO:FILE-NAME = SEARCH("progress.ini").
    iFileSize = FILE-INFO:FILE-SIZE.
    FILE-INFO:FILE-NAME = SEARCH(cRoot + "new_progress.ini").
    IF iFileSize NE FILE-INFO:FILE-SIZE THEN
      bRestart = TRUE.

    OS-DELETE VALUE(SEARCH("progress.ini")) NO-ERROR.
    OS-COPY   VALUE(SEARCH("new_progress.ini")) VALUE(cRoot + "progress.ini").
    OS-DELETE VALUE(SEARCH("new_progress.ini")).
  END.
  IF SEARCH(cRoot + "new_startup.pf") NE ? THEN DO:    
    FILE-INFO:FILE-NAME = SEARCH("startup.pf").
    iFileSize = FILE-INFO:FILE-SIZE.
    FILE-INFO:FILE-NAME = SEARCH(cRoot + "new_startup.pf").
    IF iFileSize NE FILE-INFO:FILE-SIZE THEN
      bRestart = TRUE.

    OS-DELETE VALUE(SEARCH("startup.pf")) NO-ERROR.
    OS-COPY   VALUE(SEARCH("new_startup.pf")) VALUE(cRoot + "startup.pf").
    OS-DELETE VALUE(SEARCH("new_startup.pf")).
  END.
  
  IF SEARCH(cRoot + "new_webclient\progress.cfg") NE ? THEN 
    ASSIGN bRestart   = YES
           bWcUpgrade = YES.

  IF SEARCH(cRoot + "new_viper\viper.key") NE ? THEN 
    ASSIGN bRestart      = YES
           bViperUpgrade = YES.

  IF SEARCH(cRoot + "new_assemblies\assemblies.xml") NE ? THEN 
    ASSIGN bRestart    = YES
           bAUIUpgrade = YES.

  IF bRestart THEN DO:

    OUTPUT TO .\restart.bat.
    PUT UNFORMATTED "@ECHO OFF" SKIP.
    IF bWcUpgrade THEN DO:
      IF SEARCH(".\new_webclient\bin\sleep.exe") NE ? THEN
        PUT UNFORMATTED ".\new_webclient\bin\sleep 2" SKIP.
      PUT UNFORMATTED "DEL /Q .\WebClient\bin\*.*" SKIP.
      PUT UNFORMATTED "DEL /Q .\WebClient\*.*" SKIP.
      PUT UNFORMATTED "COPY /Y .\new_webclient\bin\*.* .\WebClient\bin" SKIP.
      PUT UNFORMATTED "COPY /Y .\new_webclient\*.* .\WebClient" SKIP.
      PUT UNFORMATTED "RMDIR /S /Q .\new_webclient" SKIP.
    END.
    IF bViperUpgrade THEN DO:
      PUT UNFORMATTED "DEL /Q .\Viper\*.*" SKIP.
      PUT UNFORMATTED "COPY /Y .\new_viper\*.* .\Viper" SKIP.
      PUT UNFORMATTED "RMDIR /S /Q .\new_viper" SKIP.
    END.
    IF bAUIUpgrade THEN DO:
      PUT UNFORMATTED "DEL /Q .\Assemblies\*.*" SKIP.
      PUT UNFORMATTED "COPY /Y .\new_assemblies\*.* .\Assemblies" SKIP.
      PUT UNFORMATTED "RMDIR /S /Q .\new_assemblies" SKIP.
    END.
    PUT UNFORMATTED ".\WebClient\bin\prowc.exe -basekey ini -pf startup.pf -T " + SESSION:TEMP-DIR SKIP.
    PUT UNFORMATTED "EXIT" SKIP.
    OUTPUT CLOSE.
        
    OS-COMMAND NO-WAIT VALUE("CMD /C .\restart.bat").
    QUIT.
  END.
END.
ELSE LogThis("ERROR: Application unavailable (missing signature file)",1).
  

/* Housekeeping: */
INPUT FROM OS-DIR(SESSION:TEMP-DIRECTORY).
REPEAT:
  IMPORT cOSfile.
  IF cOSfile BEGINS "lbi0" 
     OR cOSfile BEGINS "srt0"
     OR cOSfile BEGINS "DBI0"
     OR cOSfile MATCHES "*.jpg"
     OR cOSfile MATCHES "*.bmp"
     OR cOSfile MATCHES "*.tif"
     OR cOSfile MATCHES "*.txt"
     OR cOSfile MATCHES "*.pdf"
     OR cOSfile MATCHES "*.doc"
     OR cOSfile MATCHES "*.csv"
     OR cOSfile MATCHES "*.tmp"
     THEN DO:

    ASSIGN cOSfile = SESSION:TEMP-DIRECTORY + cOSfile
           FILE-INFO:FILE-NAME = cOSfile
           . 

    IF FILE-INFO:FILE-MOD-DATE LT TODAY - 5 THEN
      OS-DELETE VALUE(cOSfile) NO-ERROR.
  END.
END.
INPUT CLOSE.

IF NOT bTest THEN QUIT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ChunkDownload) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ChunkDownload Procedure 
PROCEDURE ChunkDownload :
/*------------------------------------------------------------------------------
  Purpose:     Analyse a download of docs if files need to be chunked or batched in a number of files at a time  
  Parameters:  <none>
  Notes:       Done to get past timeout limits and bugs in the java classes used by Aia
------------------------------------------------------------------------------*/
DEF INPUT PARAM  icPackageId AS CHAR NO-UNDO.
DEF INPUT PARAM  icSaveDir   AS CHAR NO-UNDO.
DEF OUTPUT PARAM obChunk     AS LOG  NO-UNDO.
DEF OUTPUT PARAM obChunkOk   AS LOG  NO-UNDO.

DEF VAR hTempTable     AS HANDLE NO-UNDO.
DEF VAR hTempBuffer    AS HANDLE NO-UNDO.
DEF VAR hTempQuery     AS HANDLE NO-UNDO.
DEF VAR iTotSmallSize  AS INT    NO-UNDO.
DEF VAR iTotSize       AS INT    NO-UNDO.
DEF VAR iSizeLimit     AS INT    NO-UNDO INIT 1000000.
DEF VAR iCount         AS INT    NO-UNDO.
DEF VAR iStartDocBatch AS INT    NO-UNDO.
DEF VAR cOutFile       AS CHAR   NO-UNDO.
DEF VAR bUseOrgName    AS LOG    NO-UNDO.
DEF VAR mpTarget       AS MEMPTR NO-UNDO.
DEF VAR iDocsize       AS INT    NO-UNDO.
DEF VAR iStartChunk    AS INT    NO-UNDO.
DEF VAR iChunkSize     AS INT    NO-UNDO.
DEF VAR cChunkFileName AS CHAR   NO-UNDO.
DEF VAR cReturn        AS CHAR   NO-UNDO.

SESSION:SET-WAIT-STATE("general").

IF VALID-HANDLE(hServer) NE ? THEN DO:
  RUN jbdoc_getdocinfo.p
      ON hServer
     (cSessionId,
      "JBoxPackage|" + icPackageId,
      OUTPUT TABLE-HANDLE hTempTable,
      OUTPUT cReturn,
      OUTPUT bOk)
      NO-ERROR.
  IF ERROR-STATUS:ERROR THEN DO:
    MESSAGE "File-list unavaliable for package: " STRING(icPackageId )
            VIEW-AS ALERT-BOX ERROR.
    RETURN.
  END.
END.
ELSE 
  RUN jbdoc_getdocinfo.p
     (cSessionId,
      "JBoxPackage|" + icPackageId,
      OUTPUT TABLE-HANDLE hTempTable,
      OUTPUT cReturn,
      OUTPUT bOk).

EMPTY TEMP-TABLE ttDocInfo.

hTempBuffer = hTempTable:DEFAULT-BUFFER-HANDLE.
CREATE QUERY hTempQuery.
hTempQuery:SET-BUFFERS(hTempBuffer).
hTempQuery:QUERY-PREPARE("FOR EACH " + hTempBuffer:NAME).
hTempQuery:QUERY-OPEN().
hTempQuery:GET-FIRST().

REPEAT WHILE NOT hTempQuery:QUERY-OFF-END:
  CREATE ttDocInfo.
  httDocInfoBuf:BUFFER-COPY(hTempBuffer).

  IF hTempBuffer:BUFFER-FIELD("iDocSize"):BUFFER-VALUE > iSizeLimit THEN
    ttDocInfo.cCat = "big".

  ASSIGN iTotSize = iTotSize + hTempBuffer:BUFFER-FIELD("iDocSize"):BUFFER-VALUE
         iCount   = iCount + 1.

  hTempQuery:GET-NEXT().
END.

/* Now we know if the total download exceeds max in one call
   and the single files that exceeds max. */

IF iTotSize > iSizeLimit THEN DO:
  ASSIGN bChunkInProgress = YES
         obChunk          = YES
         .
  IF icSaveDir = "" THEN icSaveDir = SESSION:TEMP-DIR.
  ELSE 
    ASSIGN icSaveDir   = TRIM(icSaveDir,"\") + "\"
           bUseOrgName = TRUE.

  /* First process the files under size limit in batches that are less than limit - 
     Approximately: Includes the current doc that is the one that pushes over the limit: */  

  FOR EACH ttDocInfo
      WHERE ttDocInfo.cCat NE "big"
      BY ttDocInfo.iJBoxDocumentId:
    iTotSmallSize = iTotSmallSize + ttDocInfo.iDocSize.

    IF iTotSmallSize > iSizeLimit THEN DO:
      bChunkInProgress = YES.
      obChunkOk = InstallPackage(
                  icPackageId
                + "||"
                + STRING(iStartDocBatch) + "¤" + STRING(ttDocInfo.iJBoxDocumentId) + "¤" + STRING(iSizeLimit),
                  icSaveDir).      
      ASSIGN iStartDocBatch = ttDocInfo.iJBoxDocumentId
             iTotSmallSize  = 0.
    END.
  END.
  IF iTotSmallSize > 0 THEN DO:      
    bChunkInProgress = YES.
    obChunkOk = InstallPackage(
               icPackageId
              + "||"
              + STRING(iStartDocBatch) + "¤999999999¤" + STRING(iSizeLimit),
                icSaveDir).      
  END.

  /* Now process the documents exceeding max size indivdually by chunks: */

  DELETE OBJECT hTempTable NO-ERROR.  
  FOR EACH ttDocInfo
      WHERE ttDocInfo.cCat = "big"
      BY ttDocInfo.iJBoxDocumentId:

    ASSIGN iChunkSize       = iSizeLimit
           iStartChunk      = 1
           ix               = 0
           .

    SET-SIZE(mpTarget) = 0.
    SET-SIZE(mpTarget) = ttDocInfo.iDocSize.
    
    GetChunks:
    REPEAT:
      ASSIGN iChunkSize = MIN(ttDocInfo.iDocSize - iStartChunk + 1,iChunkSize)
             ix = ix + 1.
      IF VALID-HANDLE(hServer) THEN 
        RUN jbdoc_getdocchunk.p
            ON hServer
           (cSessionId,
            ttDocInfo.iJBoxDocumentId,
            iChunkSize,
            iStartChunk,
            OUTPUT TABLE-HANDLE hTempTable,
            OUTPUT iDocsize,
            OUTPUT cChunkFileName,  /* Same as ttDocInfo.cFileName */
            OUTPUT cReturn,
            OUTPUT bOk)
            NO-ERROR.
      ELSE 
        RUN jbdoc_getdocchunk.p
           (cSessionId,
            ttDocInfo.iJBoxDocumentId,
            iChunkSize,
            iStartChunk,
            OUTPUT TABLE-HANDLE hTempTable,
            OUTPUT iDocsize,
            OUTPUT cChunkFileName,
            OUTPUT cReturn,
            OUTPUT bOk).

      IF ix > 100 OR NOT bOk THEN DO:
        IF NOT bOk THEN
          MESSAGE PROGRAM-NAME(1) SKIP
                  cReturn VIEW-AS ALERT-BOX ERROR.
        DELETE OBJECT hTempTable NO-ERROR.  
        SET-SIZE(mpTarget) = 0.
        RETURN.
      END.

      hTempBuffer = hTempTable:DEFAULT-BUFFER-HANDLE.
      bOk = hTempBuffer:FIND-FIRST() NO-ERROR.
      IF bOk THEN DO:
        COPY-LOB FROM OBJECT hTempBuffer:BUFFER-FIELD("blChunk"):BUFFER-VALUE                 
                 TO OBJECT mpTarget
                 OVERLAY AT iStartChunk
                 NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:
          SET-SIZE(mpTarget) = ttDocInfo.iDocSize + 1.
          COPY-LOB FROM OBJECT hTempBuffer:BUFFER-FIELD("blChunk"):BUFFER-VALUE                 
                   TO OBJECT mpTarget
                   OVERLAY AT iStartChunk
                   NO-ERROR.
          IF ERROR-STATUS:ERROR THEN DO:
            MESSAGE ERROR-STATUS:GET-MESSAGE(1) SKIP
                   "iDocsize: " iDocsize SKIP
                   "ttDocInfo.iDocSize: " ttDocInfo.iDocSize SKIP
                   "iChunkSize: " iChunkSize SKIP
                   "iStartChunk: " iStartChunk SKIP
                   "GET-SIZE(mpTarget): " GET-SIZE(mpTarget) SKIP(1)
                   "Upgrade failed. Report this error to Sys.admin"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RETURN.
          END.
        END.

        iStartChunk = iStartChunk + iChunkSize.

        IF iStartChunk > iDocSize THEN DO:

          IF NOT CreateInstallDir(icSaveDir) THEN QUIT.
          icSaveDir = TRIM(icSaveDir,"\") + "\".
/* /*           OS-COMMAND SILENT mkdir VALUE('"' + icSaveDir + '"').  */                   */
/*                                                                                          */
/*           OS-CREATE-DIR VALUE(icSaveDir).                                                */
/*           iOsError = OS-ERROR.                                                           */
/*           IF iOsError NE 0 THEN DO:                                                      */
/*             MESSAGE "Cannot create directory " icSaveDir SKIP                            */
/*                     "Check your permissions or try running the program as administrator" */
/*                     VIEW-AS ALERT-BOX ERROR.                                             */
/*             QUIT.                                                                        */
/*           END.                                                                           */

          OUTPUT STREAM strFile TO VALUE(icSaveDir + cChunkFileName) NO-MAP BINARY NO-CONVERT.
          EXPORT STREAM strFile mpTarget.
          OUTPUT STREAM strFile CLOSE.

          DELETE OBJECT hTempTable NO-ERROR.  
          LEAVE GetChunks.
        END.
      END.
    END.
    DELETE OBJECT hTempTable NO-ERROR.  
  END.
  SET-SIZE(mpTarget) = 0.
END.

ELSE obChunk = NO.

SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetUpdates) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetUpdates Procedure 
PROCEDURE GetUpdates :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR fTotSize         AS DEC    NO-UNDO.
DEF VAR hRectangle       AS HANDLE NO-UNDO.
DEF VAR ix               AS INT    NO-UNDO.

httPackagesBuffer = httPackages:DEFAULT-BUFFER-HANDLE.
CREATE QUERY httPackagesQuery NO-ERROR.
httPackagesQuery:SET-BUFFERS(httPackagesBuffer) NO-ERROR.
httPackagesQuery:QUERY-PREPARE("FOR EACH " + httPackagesBuffer:NAME 
                               + " WHERE cSignature GT '" + ENTRY(2,cSignature,"|") + "'"
                               + " BY iInstallSeq").
httPackagesQuery:QUERY-OPEN.

iNumPackages = 0.
httPackagesQuery:GET-FIRST().
REPEAT WHILE NOT httPackagesQuery:QUERY-OFF-END:
  ASSIGN iNumPackages = iNumPackages + 1
         fTotSize     = fTotSize + httPackagesBuffer:BUFFER-FIELD("fTotSize"):BUFFER-VALUE
         .
  httPackagesQuery:GET-NEXT().
END.


ix = 0.
httPackagesQuery:GET-FIRST().
REPEAT WHILE NOT httPackagesQuery:QUERY-OFF-END:
  ix = ix + 1.        

  IF httPackagesBuffer:BUFFER-FIELD("bComplete"):BUFFER-VALUE THEN 
    CleanUpDir(cRoot + LEFT-TRIM(httPackagesBuffer:BUFFER-FIELD("cRelativePath"):BUFFER-VALUE,".\")).

  IF httPackagesBuffer:BUFFER-FIELD("bRestart"):BUFFER-VALUE THEN
    bRestart = TRUE.

  InstallPackage(STRING(httPackagesBuffer:BUFFER-FIELD("iJBoxPackageId"):BUFFER-VALUE),
                 cRoot + LEFT-TRIM(httPackagesBuffer:BUFFER-FIELD("cRelativePath"):BUFFER-VALUE,".\")
                 ).
  IF httPackagesBuffer:BUFFER-FIELD("cSignature"):BUFFER-VALUE > cLatestPackage THEN
    cLatestPackage = httPackagesBuffer:BUFFER-FIELD("cSignature"):BUFFER-VALUE.
  httPackagesQuery:GET-NEXT().
END.
  
OUTPUT TO VALUE(cRoot + "signature.txt").
PUT UNFORMATTED ENTRY(1,cSignature,"|") + "|" + cLatestPackage + "|" + cAppTitle + "|" + cLanguages + "|" + cBaseLanguage SKIP.
OUTPUT CLOSE.

IF NOT bRestart THEN DO:
  httPackagesQuery:QUERY-PREPARE("FOR EACH " + httPackagesBuffer:NAME 
                                 + " WHERE bProPath"
                                 + " BY iProPathSeq").
  httPackagesQuery:QUERY-OPEN.
  httPackagesQuery:GET-FIRST().
  
  /* cProPath = PROPATH. */
  REPEAT WHILE NOT httPackagesQuery:QUERY-OFF-END:
    cProPath = cProPath + 
               cRoot + LEFT-TRIM(httPackagesBuffer:BUFFER-FIELD("cRelativePath"):BUFFER-VALUE,".\") + ";".
    httPackagesQuery:GET-NEXT().
  END.
  
  PROPATH = cProPath + PROPATH.
  OUTPUT TO VALUE(cRoot + "propath.txt").
  PUT UNFORMATTED cProPath SKIP.
  OUTPUT CLOSE.
END.

DELETE OBJECT httPackagesQuery.
DELETE OBJECT httPackages.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-RenameFiles) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RenameFiles Procedure 
PROCEDURE RenameFiles :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icCat AS CHAR NO-UNDO.

DEF VAR cTmpFileName     AS CHAR NO-UNDO.
DEF VAR cTmpFileFullName AS CHAR NO-UNDO.
DEF VAR cNewName         AS CHAR NO-UNDO.
DEF VAR cNewFullName     AS CHAR NO-UNDO.
DEF VAR cReplaceString   AS CHAR NO-UNDO.
DEF VAR cDirList         AS CHAR NO-UNDO.
DEF VAR ix               AS INT  NO-UNDO.

FILE-INFO:FILE-NAME = icCat.
IF FILE-INFO:FULL-PATHNAME = ? THEN RETURN.

INPUT FROM OS-DIR(icCat).
REPEAT:
  IMPORT cTmpFileName.
  FILE-INFO:FILE-NAME = RIGHT-TRIM(icCat,"\") + "\" + cTmpFileName.
  cTmpFileFullName = FILE-INFO:FULL-PATHNAME.
  IF cTmpFileName BEGINS "." THEN NEXT.

  IF FILE-INFO:FILE-TYPE = "drw" THEN DO:
    cDirList = cDirList + (IF cDirList NE "" THEN "," ELSE "") + cTmpFileFullName.
    NEXT.
  END.

  bOk = FALSE.
  DO ix = 1 TO NUM-ENTRIES(cRenamePattern):
    IF cTmpFileName MATCHES ENTRY(1,ENTRY(ix,cRenamePattern),";") THEN DO:
      ASSIGN cReplaceString = REPLACE(ENTRY(1,ENTRY(ix,cRenamePattern),";"),"*","")
             cNewName = REPLACE(cTmpFileName,cReplaceString,ENTRY(2,ENTRY(ix,cRenamePattern),";"))  
             FILE-INFO:FILE-NAME = cNewName
             cNewFullName = REPLACE(cTmpFileFullName,cTmpFileName,cNewName)
             .
  
      OS-RENAME VALUE(cTmpFileFullName) VALUE(cNewFullName).
    END.
  END.
END.
INPUT CLOSE.

DO ix = 1 TO NUM-ENTRIES(cDirList):
  RUN RenameFiles(ENTRY(ix,cDirList)).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-CheckIfAdmin) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CheckIfAdmin Procedure 
FUNCTION CheckIfAdmin RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cInput AS CHAR NO-UNDO.
DEF VAR bAdmin AS LOG  NO-UNDO.
OS-COMMAND SILENT VALUE("whoami /groups /fo csv /nh > " + SESSION:TEMP-DIRECTORY + "whoami.txt").
INPUT FROM VALUE(SESSION:TEMP-DIRECTORY + "whoami.txt").
REPEAT:
  IMPORT UNFORMATTED cInput.
  IF CAN-DO(cInput,'"S-1-16-12288"') THEN DO:
    bAdmin = YES.
    LEAVE.
  END. 
END.
INPUT CLOSE.
RETURN bAdmin.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CleanUpDir) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CleanUpDir Procedure 
FUNCTION CleanUpDir RETURNS LOGICAL
  ( INPUT icCleanUpDir AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
icCleanUpDir = TRIM(icCleanUpDir,"\").

DEF VAR bMon AS LOG.
FILE-INFO:FILE-NAME = icCleanUpDir.
IF FILE-INFO:FILE-TYPE = ? THEN DO:
  OS-COMMAND SILENT mkdir VALUE('"' + icCleanUpDir + '"') NO-ERROR.
  FILE-INFO:FILE-NAME = icCleanUpDir.
  IF FILE-INFO:FILE-TYPE = ? THEN RETURN NO.
END.
INPUT FROM OS-DIR(icCleanUpDir) CONVERT TARGET "UTF-8" SOURCE "ISO8859-1".
icCleanUpDir = TRIM(icCleanUpDir,"\") + "\".
REPEAT:
  IMPORT cOSfile NO-ERROR.
  OS-DELETE VALUE(icCleanUpDir + cOSfile) NO-ERROR.
END.
INPUT CLOSE.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ConnectAppFarm) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ConnectAppFarm Procedure 
FUNCTION ConnectAppFarm RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF bTest AND PROGRESS = "Full" THEN RETURN TRUE.

{incl/startupconnect.i}
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CreateInstallDir) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CreateInstallDir Procedure 
FUNCTION CreateInstallDir RETURNS LOGICAL
  ( INPUT icSaveDir AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cDirComp AS CHAR NO-UNDO.
DEF VAR cTmp     AS CHAR NO-UNDO.
DEF VAR ix       AS INT  NO-UNDO.

icSaveDir = TRIM(icSaveDir,"\").

DO ix = 1 TO NUM-ENTRIES(icSaveDir,"\"):
  cTmp = cTmp + (IF cTmp NE "" THEN "\" ELSE "") + ENTRY(ix,icSaveDir,"\").
  IF LENGTH(TRIM(cTmp,"\") + "\") > LENGTH(cRoot) THEN DO:
    cDirComp = cDirComp + (IF cDirComp NE "" THEN "\" ELSE "") + ENTRY(ix,icSaveDir,"\").
                                                             
    OS-CREATE-DIR VALUE(cDirComp).
    iOsError = OS-ERROR.
    IF iOsError NE 0 THEN DO:
      MESSAGE "Cannot create directory " icSaveDir SKIP
              "Check your permissions or try running the program as administrator"
              VIEW-AS ALERT-BOX ERROR.
      RETURN NO.
    END.
  END.
END.
  
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getPackageList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getPackageList Procedure 
FUNCTION getPackageList RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
bOK = SESSION:SET-WAIT-STATE("general").

IF VALID-HANDLE(hServer) THEN DO:
  RUN jbappfarm_get_package_list.p
      ON hServer
     (ENTRY(1,cSignature,"|"),
      OS-GETENV("COMPUTERNAME"),
      cAppFarmUser,
      OUTPUT TABLE-HANDLE httPackages,
      OUTPUT cReturnTrans,
      OUTPUT cSessionId)
      NO-ERROR.
  IF ERROR-STATUS:ERROR THEN DO:
    LogThis("Package list unavaliable for signature: " + cSignature,1).
    RETURN FALSE.
  END.
END.
ELSE 
  RUN jbappfarm_get_package_list.p
     (ENTRY(1,cSignature,"|"),
      OS-GETENV("COMPUTERNAME"),
      cAppFarmUser,
      OUTPUT TABLE-HANDLE httPackages,
      OUTPUT cReturnTrans,
      OUTPUT cSessionId)
      .

bOK = SESSION:SET-WAIT-STATE("").

IF cSessionId NE "" THEN DO:
  ASSIGN cAppTitle     = ENTRY(1,cReturnTrans,"|")
         cLanguages    = ENTRY(2,cReturnTrans,"|")
         cBaseLanguage = ENTRY(3,cReturnTrans,"|")
         .
  RETURN TRUE.
END.
ELSE DO:
  LogThis(cReturnTrans,0).
  RETURN FALSE.
END.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-InstallPackage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION InstallPackage Procedure 
FUNCTION InstallPackage RETURNS LOGICAL
  ( INPUT icPackageId AS CHAR,
    INPUT icSaveDir   AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hTempTable  AS HANDLE NO-UNDO.
DEF VAR hTempBuffer AS HANDLE NO-UNDO.
DEF VAR hTempQuery  AS HANDLE NO-UNDO.
DEF VAR cOutFile    AS CHAR   NO-UNDO.
DEF VAR mpDocument  AS MEMPTR NO-UNDO.
DEF VAR bChunk      AS LOG    NO-UNDO.
DEF VAR bChunkOk    AS LOG    NO-UNDO.

IF NOT bChunkInProgress THEN DO:
  RUN ChunkDownload(icPackageId,icSaveDir,OUTPUT bChunk,OUTPUT bChunkOk).
  IF bChunk THEN 
    RETURN bChunkOk.
END.

bChunkInProgress = NO.

bOK = SESSION:SET-WAIT-STATE("general").

IF VALID-HANDLE(hServer) THEN DO:
  RUN jbdoc_getdoc.p
      ON hServer
     (cSessionId,
      "JBoxPackage|" + icPackageId,
      OUTPUT TABLE-HANDLE hTempTable,
      OUTPUT cReturnTrans,
      OUTPUT bOk)
      NO-ERROR.
  IF ERROR-STATUS:ERROR THEN DO:
    MESSAGE "File-list unavaliable for package: " icPackageId 
            VIEW-AS ALERT-BOX ERROR.
    RETURN FALSE.
  END.
END.
ELSE 
  RUN jbdoc_getdoc.p
     (cSessionId,
      "JBoxPackage|" + icPackageId,
      OUTPUT TABLE-HANDLE hTempTable,
      OUTPUT cReturnTrans,
      OUTPUT bOk)
      .

IF NOT CreateInstallDir(icSaveDir) THEN QUIT.
icSaveDir = TRIM(icSaveDir,"\") + "\".


hTempBuffer = hTempTable:DEFAULT-BUFFER-HANDLE.
CREATE QUERY hTempQuery.
hTempQuery:SET-BUFFERS(hTempBuffer).
hTempQuery:QUERY-PREPARE("FOR EACH " + hTempBuffer:NAME + " BY cFileType DESC"). /* So that .bat files comes last */
hTempQuery:QUERY-OPEN().
hTempQuery:GET-FIRST().

REPEAT WHILE NOT hTempQuery:QUERY-OFF-END:
  cOutFile = icSaveDir + hTempBuffer:BUFFER-FIELD("cFileName"):BUFFER-VALUE.
  SET-SIZE(mpDocument) = hTempBuffer:BUFFER-FIELD('iDocSize'):BUFFER-VALUE.
  COPY-LOB FROM OBJECT hTempBuffer:BUFFER-FIELD('blDocument'):BUFFER-VALUE TO OBJECT mpDocument NO-ERROR.
  IF ERROR-STATUS:ERROR THEN DO:
    MESSAGE ERROR-STATUS:GET-MESSAGE(1) SKIP
            hTempBuffer:BUFFER-FIELD('iDocSize'):BUFFER-VALUE SKIP(1)
            "Upgrade failed. Report this error to sys.admin"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN NO.
  END.
  OUTPUT STREAM strFile TO VALUE(cOutFile) NO-MAP BINARY NO-CONVERT.
  EXPORT STREAM strFile mpDocument.
  OUTPUT STREAM strFile CLOSE.

  IF hTempBuffer:BUFFER-FIELD('cFileType'):BUFFER-VALUE = "bat" AND NOT cOutFile MATCHES "*webclient*" THEN
    OS-COMMAND SILENT VALUE(cOutFile).

  hTempQuery:GET-NEXT().
END.

DELETE OBJECT hTempQuery.
DELETE OBJECT hTempTable.
SET-SIZE(mpDocument) = 0.

bOK = SESSION:SET-WAIT-STATE("").

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LogThis) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LogThis Procedure 
FUNCTION LogThis RETURNS LOGICAL
  ( INPUT icLogText   AS CHAR,
    INPUT iiSkipLines AS INT) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF cLogFile NE "" THEN DO:
  OUTPUT STREAM sLog TO VALUE(cLogFile) APPEND.
  PUT STREAM sLog UNFORMATTED TODAY ", " STRING(TIME,"HH:MM:SS") icLogText AT 30.
  CASE iiSkipLines:
    WHEN 0 THEN PUT STREAM sLog SKIP.
    WHEN 1 THEN PUT STREAM sLog SKIP(1).
    WHEN 2 THEN PUT STREAM sLog SKIP(2).
    WHEN 3 THEN PUT STREAM sLog SKIP(3).
    WHEN 4 THEN PUT STREAM sLog SKIP(4).
    WHEN 5 THEN PUT STREAM sLog SKIP(5).
  END CASE.
  OUTPUT STREAM sLog CLOSE.
  IF icLogText MATCHES "*Etime*" THEN ETIME(TRUE).
END.
RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ProcessesStopped) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ProcessesStopped Procedure 
FUNCTION ProcessesStopped RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cRunningProcesses AS CHAR NO-UNDO.
DEF VAR cStoppedProcesses AS CHAR NO-UNDO.

bOK = SESSION:SET-WAIT-STATE("general").

IF VALID-HANDLE(hServer) THEN DO:
  RUN jbappfarm_check_process_status.p
      ON hServer
     (ENTRY(1,cSignature,"|"),
      cAppFarmUser,
      cSessionId,
      OUTPUT cRunningProcesses,
      OUTPUT cStoppedProcesses,
      OUTPUT cReturnTrans)
      NO-ERROR.
  IF ERROR-STATUS:ERROR THEN DO:
    LogThis("Could not check process status for " + ENTRY(1,cSignature,"|") + ", " + cAppFarmUser,1).
    RETURN FALSE.
  END.
END.
ELSE 
  RUN jbappfarm_check_process_status.p
     (ENTRY(1,cSignature,"|"),
      OS-GETENV("COMPUTERNAME"),
      cAppFarmUser,
      cSessionId,
      OUTPUT cRunningProcesses,
      OUTPUT cStoppedProcesses,
      OUTPUT cReturnTrans)
      .

bOK = SESSION:SET-WAIT-STATE("").

IF cReturnTrans NE "" THEN DO:
  LogThis("Error when checking process status for " + ENTRY(1,cSignature,"|") + ", " + cAppFarmUser + ": " + cReturnTrans,1).
  RETURN FALSE.                                                                                           
END.

IF cRunningProcesses NE "" THEN
  LogThis("Updates could not be retrieved due to running processes " + cRunningProcesses,0).

RETURN cRunningProcesses = "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setVariable) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setVariable Procedure 
FUNCTION setVariable RETURNS LOGICAL
  ( INPUT icVarName AS CHAR,
    INPUT icValue   AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
CASE icVarName:
  WHEN "cRenamePattern" THEN cRenamePattern = icValue.
END CASE.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

