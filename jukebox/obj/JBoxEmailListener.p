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

DEFINE VARIABLE oEmailReader AS JBoxEMailOpenPop NO-UNDO.
DEFINE VARIABLE o2Pdf        AS JBox2PdfBullZip  NO-UNDO.

DEFINE VARIABLE cMailServer         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPassword           AS CHARACTER NO-UNDO.
DEFINE VARIABLE cUserName           AS CHARACTER NO-UNDO.
DEFINE VARIABLE iAuthMethod         AS INTEGER   NO-UNDO.
DEFINE VARIABLE iPort               AS INTEGER   NO-UNDO.

DEFINE VARIABLE bConvHtmlToText     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE bFirstSuspend       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE bSuspend            AS LOGICAL   NO-UNDO.
DEFINE VARIABLE bUploadAttAsync     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cDocLoadContext     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMethod             AS CHARACTER NO-UNDO.
DEFINE VARIABLE hParent             AS HANDLE    NO-UNDO.
DEFINE VARIABLE iChrPrLineHtmlToTxt AS INTEGER   NO-UNDO.
DEFINE VARIABLE iLastMsgCount       AS INTEGER   NO-UNDO.
DEFINE VARIABLE cLastLoadedDocIds   AS CHARACTER NO-UNDO.

DEFINE VARIABLE iLastMsgId          AS INTEGER   NO-UNDO INIT 0. /*Used to deal with retrievals.*/
DEFINE VARIABLE bSingleMsg          AS LOGICAL   INIT FALSE.
DEFINE VARIABLE hGetMsgBtn          AS HANDLE    NO-UNDO.
DEFINE VARIABLE iThreadHandle       AS INTEGER   NO-UNDO.
DEFINE VARIABLE iMsgCount           AS INTEGER   NO-UNDO.
DEFINE VARIABLE iSize               AS MEMPTR    NO-UNDO.
DEFINE VARIABLE ix                  AS INTEGER   NO-UNDO.
DEFINE VARIABLE iReturn             AS INTEGER   NO-UNDO.
DEFINE VARIABLE cCheckStoredTable   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCheckStoredFromFld AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCheckStoredDateFld AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCheckStoredTimeFld AS CHARACTER NO-UNDO.
DEFINE VARIABLE bDeleteIfStored     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cTargetGroupIdField AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTargetGroupId      AS CHARACTER NO-UNDO.
DEFINE VARIABLE bLoadBlobs          AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cActionProcs        AS CHAR      NO-UNDO.
DEFINE VARIABLE cActionParams       AS CHAR      NO-UNDO.
DEFINE VARIABLE cConvToPdfTypes     AS CHAR      NO-UNDO.
DEFINE VARIABLE cLogFile            AS CHAR      NO-UNDO.
DEFINE VARIABLE chWord              AS COM-HANDLE NO-UNDO.

DEFINE STREAM s1.
DEFINE STREAM sLog.

DEF TEMP-TABLE ttEmail NO-UNDO
    FIELD dDateSent       AS DATE 
    FIELD iTimeSent       AS INT
    FIELD cTimeSent       AS CHAR FORMAT "x(5)"
    FIELD cSubject        AS CHAR FORMAT "x(80)"
    FIELD cFrom           AS CHAR FORMAT "x(40)"
    FIELD cDisplayFrom    AS CHAR FORMAT "x(40)"
    FIELD cTo             AS CHAR FORMAT "x(40)"
    FIELD cDisplayTo      AS CHAR FORMAT "x(40)"
    FIELD cCc             AS CHAR FORMAT "x(40)"
    FIELD cDisplayCc      AS CHAR FORMAT "x(40)"
    FIELD cBcc            AS CHAR FORMAT "x(40)"
    FIELD cDisplayBcc     AS CHAR FORMAT "x(40)"
    FIELD cBody           AS CHAR FORMAT "x(256)"
    FIELD blBody          AS BLOB 
    FIELD iBodySize       AS INT
    FIELD iPriority       AS INT
    FIELD iAttachCount    AS INT
    FIELD cAttachments    AS CHAR FORMAT "x(80)"
    FIELD cAttachFiles    AS CHAR
    FIELD iMsgId          AS INT
    FIELD bBodyRetrieved  AS LOG
    FIELD iMsgType        AS INT        /*Determines type of message, 1 = Plain 2 = HTML*/
    FIELD cFileName       AS CHAR FORMAT "x(40)"
    FIELD cFullPathName   AS CHAR FORMAT "x(60)"
    FIELD RowIdent1       AS CHAR
    .
DEF VAR httEmailBuffer AS HANDLE NO-UNDO.
DEF VAR httEmail       AS HANDLE NO-UNDO.
httEmailBuffer = BUFFER ttEmail:HANDLE.
httEmail       = httEmailBuffer:TABLE-HANDLE.

DEF TEMP-TABLE ttNewEmail NO-UNDO
    FIELD dDateSent       AS DATE
    FIELD iTimeSent       AS INT
    FIELD cTimeSent       AS CHAR FORMAT "x(5)"
    FIELD cSubject        AS CHAR
    FIELD cFrom           AS CHAR FORMAT "x(40)"
    FIELD cDisplayFrom    AS CHAR FORMAT "x(40)"
    FIELD cTo             AS CHAR FORMAT "x(40)"
    FIELD cDisplayTo      AS CHAR FORMAT "x(40)"
    FIELD cCc             AS CHAR FORMAT "x(40)"
    FIELD cDisplayCc      AS CHAR FORMAT "x(40)"
    FIELD cBcc            AS CHAR FORMAT "x(40)"
    FIELD cDisplayBcc     AS CHAR FORMAT "x(40)"
    FIELD cBody           AS CHAR
    FIELD blBody          AS BLOB
    FIELD iBodySize       AS INT 
    FIELD iPriority       AS INT
    FIELD iAttachCount    AS INT
    FIELD cAttachments    AS CHAR
    FIELD cAttachFiles    AS CHAR
    FIELD iMsgId          AS INT
    FIELD bBodyRetrieved  AS LOG
    FIELD iMsgType        AS INT        /*Determines type of message, 1 = Plain 2 = HTML*/
    FIELD cEntityId       AS CHAR
    FIELD cFileName       AS CHAR FORMAT "x(40)"
    INDEX idxMsgId AS PRIMARY iMsgId.

DEF VAR httNewEmail       AS HANDLE NO-UNDO.
DEF VAR httNewEmailBuffer AS HANDLE NO-UNDO.
httNewEmailBuffer = BUFFER ttNewEmail:HANDLE.
httNewEmail       = httNewEmailBuffer:TABLE-HANDLE.

DEF TEMP-TABLE ttAttachment NO-UNDO
    FIELD iMsgId          AS INT 
    FIELD cFileName       AS CHAR
    FIELD cFileType       AS CHAR
    FIELD cDescription    AS CHAR
    FIELD cFullPathName   AS CHAR 
    FIELD blAttachment    AS BLOB
    FIELD iAttachmentSize AS INT
    FIELD dAttachCreated  AS DATE
    FIELD iAttachCreTime  AS INT
    FIELD dAttachModified AS DATE
    FIELD iAttachModTime  AS INT
    .

DEF VAR httAttachment     AS HANDLE NO-UNDO.
DEF VAR httAttachBuffer   AS HANDLE NO-UNDO.
httAttachBuffer = BUFFER ttAttachment:HANDLE.
httAttachment   = httAttachBuffer:TABLE-HANDLE.

DEF TEMP-TABLE ttSubjectList
    FIELD iMsgId   AS INTEGER
    FIELD cSubject AS CHARACTER
    .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-ConvertHtmlToText) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ConvertHtmlToText Procedure 
FUNCTION ConvertHtmlToText RETURNS LOGICAL
  ( INPUT ibConvHtmlToText AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CreLog) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CreLog Procedure 
FUNCTION CreLog RETURNS LOGICAL
  ( INPUT icLogText AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DeleteEmail) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD DeleteEmail Procedure 
FUNCTION DeleteEmail RETURNS LOGICAL
  ( INPUT iiMsgId AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getEmailServer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getEmailServer Procedure 
FUNCTION getEmailServer RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getEmailTT) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getEmailTT Procedure 
FUNCTION getEmailTT RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getEmailUser) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getEmailUser Procedure 
FUNCTION getEmailUser RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getLastLoadedDocIds) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getLastLoadedDocIds Procedure 
FUNCTION getLastLoadedDocIds RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTTAttachBuffer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTTAttachBuffer Procedure 
FUNCTION getTTAttachBuffer RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTTEmailBuffer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTTEmailBuffer Procedure 
FUNCTION getTTEmailBuffer RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTTNewEmailBuffer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTTNewEmailBuffer Procedure 
FUNCTION getTTNewEmailBuffer RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ImportAttachments) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ImportAttachments Procedure 
FUNCTION ImportAttachments RETURNS CHARACTER
  ( INPUT iiMsgId AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OpenAttachment) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD OpenAttachment Procedure 
FUNCTION OpenAttachment RETURNS LOGICAL
  ( INPUT iiMsgId          AS INT,
    INPUT icAttachmentList AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ReleaseWordHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ReleaseWordHandle Procedure 
FUNCTION ReleaseWordHandle RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SaveMailAsHTML) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SaveMailAsHTML Procedure 
FUNCTION SaveMailAsHTML RETURNS CHARACTER
  ( INPUT iiMsgId   AS INT)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setActionProcsAndParams) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setActionProcsAndParams Procedure 
FUNCTION setActionProcsAndParams RETURNS LOGICAL
  ( INPUT icActionProcs  AS CHAR,
    INPUT icActionParams AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setCheckStoredExp) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setCheckStoredExp Procedure 
FUNCTION setCheckStoredExp RETURNS LOGICAL
  ( INPUT icCheckStoredTable   AS CHAR,
    INPUT icCheckStoredFromFld AS CHAR,
    INPUT icCheckStoredDateFld AS CHAR,
    INPUT icCheckStoredTimeFld AS CHAR,
    INPUT icTargetGroupIdField AS CHAR,
    INPUT icTargetGroupId      AS CHAR,
    INPUT ibDeleteIfStored     AS LOG)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setChrPrLineHtmlToTxt) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setChrPrLineHtmlToTxt Procedure 
FUNCTION setChrPrLineHtmlToTxt RETURNS LOGICAL
  ( INPUT iiChrPrLineHtmlToTxt AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setDocLoadContext) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setDocLoadContext Procedure 
FUNCTION setDocLoadContext RETURNS LOGICAL
  ( INPUT icDocLoadContext AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setLoadBlobs) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setLoadBlobs Procedure 
FUNCTION setLoadBlobs RETURNS LOGICAL
  ( INPUT ibLoadBlobs AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setLogFile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setLogFile Procedure 
FUNCTION setLogFile RETURNS LOGICAL
  ( INPUT icLogFile AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setMailServerProperties) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setMailServerProperties Procedure 
FUNCTION setMailServerProperties RETURNS LOGICAL
  ( INPUT icMailServer AS CHARACTER,
    INPUT iiPort       AS INTEGER,
    INPUT icUserName   AS CHARACTER,
    INPUT icPassword   AS CHARACTER,
    INPUT iiAuthMethod AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setSaveBodyMethod) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setSaveBodyMethod Procedure 
FUNCTION setSaveBodyMethod RETURNS LOGICAL
  ( INPUT icMethod AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setSubjectList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setSubjectList Procedure 
FUNCTION setSubjectList RETURNS LOGICAL
  ( INPUT icSubjectList AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setUploadAttAsync) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setUploadAttAsync Procedure 
FUNCTION setUploadAttAsync RETURNS LOGICAL
  ( INPUT ibUploadAttAsync AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SignOff) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SignOff Procedure 
FUNCTION SignOff RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-UploadMailToDb) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD UploadMailToDb Procedure 
FUNCTION UploadMailToDb RETURNS LOGICAL
  ( INPUT iiMsgId      AS INT,
    INPUT icEntity     AS CHAR,
    INPUT icEntityId   AS CHAR,
    INPUT icEntityText AS CHAR,
    INPUT ibDeleteMail AS LOG )  FORWARD.

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
         HEIGHT             = 27.33
         WIDTH              = 56.6.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

ON 'close':U OF THIS-PROCEDURE DO:
  SignOff().
  DELETE PROCEDURE THIS-PROCEDURE.
END.

oEmailReader = NEW JBoxEMailOpenPop().

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-getEmail) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getEmail Procedure 
PROCEDURE getEmail :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE cAttachments    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cBcc            AS CHARACTER NO-UNDO.
DEFINE VARIABLE cBody           AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCC             AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFrom           AS CHARACTER NO-UNDO.
DEFINE VARIABLE cReplyTo        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSubject        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTo             AS CHARACTER NO-UNDO.
DEFINE VARIABLE iAttachCount    AS INTEGER   NO-UNDO.
DEFINE VARIABLE iPriority       AS INTEGER   NO-UNDO.
DEFINE VARIABLE dDateSent       AS DATE      NO-UNDO.
DEFINE VARIABLE iTimeSent       AS INTEGER   NO-UNDO.
DEFINE VARIABLE iMsgCount       AS INTEGER   NO-UNDO.
DEFINE VARIABLE iy              AS INTEGER   NO-UNDO.
DEFINE VARIABLE iCntNewMsg      AS INTEGER   NO-UNDO.
DEFINE VARIABLE hBufMessages    AS HANDLE    NO-UNDO.
DEFINE VARIABLE hQueryMessages  AS HANDLE    NO-UNDO.
DEFINE VARIABLE bStored         AS LOGICAL   NO-UNDO.


IF bSuspend THEN RETURN.


iMsgCount = oEmailReader:getEmailMessageCount().

/* The batch server (JBoxEmailImport.w) will keep track of last count */
PUBLISH "getLastEmailCount" (iMsgCount,OUTPUT iLastMsgCount).
  
IF iMsgCount > iLastMsgCount THEN DO:

  EMPTY TEMP-TABLE ttNewEmail.
  ix = iLastMsgCount.

  hBufMessages = oEmailReader:getEmailMessages().

  CREATE QUERY hQueryMessages.
  hQueryMessages:SET-BUFFERS(hBufMessages).
  hQueryMessages:QUERY-PREPARE("FOR EACH " + hBufMessages:NAME).
  hQueryMessages:QUERY-OPEN().
  hQueryMessages:GET-FIRST().
  REPEAT WHILE NOT hQueryMessages:QUERY-OFF-END:
    CREATE ttNewEmail.
    ASSIGN ttNewEmail.cSubject       = hBufMessages:BUFFER-FIELD("cSubject"):BUFFER-VALUE
           ttNewEmail.cFrom          = hBufMessages:BUFFER-FIELD("cFrom"):BUFFER-VALUE
           ttNewEmail.cDisplayFrom   = IF hBufMessages:BUFFER-FIELD("cDisplayFrom"):BUFFER-VALUE NE "" THEN
                                         hBufMessages:BUFFER-FIELD("cDisplayFrom"):BUFFER-VALUE   
                                       ELSE hBufMessages:BUFFER-FIELD("cFrom"):BUFFER-VALUE
           ttNewEmail.cTo            = hBufMessages:BUFFER-FIELD("cTo"):BUFFER-VALUE
           ttNewEmail.cDisplayTo     = IF hBufMessages:BUFFER-FIELD("cDisplayTo"):BUFFER-VALUE NE "" THEN 
                                         hBufMessages:BUFFER-FIELD("cDisplayTo"):BUFFER-VALUE
                                       ELSE hBufMessages:BUFFER-FIELD("cTo"):BUFFER-VALUE
           ttNewEmail.cCc            = hBufMessages:BUFFER-FIELD("cCc"):BUFFER-VALUE
           ttNewEmail.cDisplayCc     = IF hBufMessages:BUFFER-FIELD("cDisplayCc"):BUFFER-VALUE NE "" THEN
                                         hBufMessages:BUFFER-FIELD("cDisplayCc"):BUFFER-VALUE
                                       ELSE hBufMessages:BUFFER-FIELD("cCc"):BUFFER-VALUE
           ttNewEmail.cBcc           = hBufMessages:BUFFER-FIELD("cBcc"):BUFFER-VALUE
           ttNewEmail.cDisplayBcc    = IF hBufMessages:BUFFER-FIELD("cDisplayBcc"):BUFFER-VALUE NE "" THEN
                                         hBufMessages:BUFFER-FIELD("cDisplayBcc"):BUFFER-VALUE
                                       ELSE hBufMessages:BUFFER-FIELD("cBcc"):BUFFER-VALUE
           ttNewEmail.cAttachments   = hBufMessages:BUFFER-FIELD("cAttachments"):BUFFER-VALUE
           ttNewEmail.cAttachFiles   = hBufMessages:BUFFER-FIELD("cAttachFiles"):BUFFER-VALUE
           ttNewEmail.cBody          = hBufMessages:BUFFER-FIELD("cPlainText"):BUFFER-VALUE
           ttNewEmail.iAttachCount   = hBufMessages:BUFFER-FIELD("iAttachCount"):BUFFER-VALUE
           ttNewEmail.dDateSent      = DATE(hBufMessages:BUFFER-FIELD("dtDate"):BUFFER-VALUE)
           ttNewEmail.iTimeSent      = INTEGER(TRUNC(MTIME(hBufMessages:BUFFER-FIELD("dtDate"):BUFFER-VALUE) / 1000,0))
           ttNewEmail.cTimeSent      = STRING(ttNewEmail.iTimeSent,"HH:MM")
           ttNewEmail.blBody         = hBufMessages:BUFFER-FIELD("blBody"):BUFFER-VALUE
           ttNewEmail.iMsgType       = hBufMessages:BUFFER-FIELD("iMsgType"):BUFFER-VALUE
           ttNewEmail.iMsgId         = hBufMessages:BUFFER-FIELD("iMsgId"):BUFFER-VALUE
           ix                        = ix + 1
           ttNewEmail.iPriority      = iPriority
           ttNewEmail.bBodyRetrieved = YES
           .

    FIND FIRST ttSubjectList
         WHERE ttSubjectList.iMsgId = ttNewEmail.iMsgId
           AND ttSubjectList.cSubject MATCHES "*" + ttNewEmail.cSubject
         NO-ERROR.
    IF AVAIL ttSubjectList THEN
      ttNewEmail.cSubject = ttSubjectList.cSubject.
    hQueryMessages:GET-NEXT().
  END.

  FOR EACH ttNewEmail 
      BY iMsgId DESC:
    IF cCheckStoredTable NE "" THEN
      bStored = DYNAMIC-FUNCTION("getFieldValues",cCheckStoredTable,
                                 "WHERE " + cCheckStoredFromFld + " = '" + ttNewEmail.cFrom + "'"
                               + " AND " + cCheckStoredDateFld + " = DATE('" + STRING(ttNewEmail.dDateSent) + "')"
                               + " AND " + cCheckStoredTimeFld + " = INTEGER('" + STRING(ttNewEmail.iTimeSent) + "')"
                               + (IF cTargetGroupIdField NE "" THEN
                                   " AND " + cTargetGroupIdField + " = " + cTargetGroupId
                                  ELSE "")
                                 ,cCheckStoredFromFld) NE ?.

    FIND FIRST ttEmail
         WHERE ttEmail.cFrom     = ttNewEmail.cFrom
           AND ttEmail.dDateSent = ttNewEmail.dDateSent
           AND ttEmail.iTimeSent = ttNewEmail.iTimeSent
         NO-ERROR.
    IF AVAIL ttEmail THEN DO:
      ttEmail.iMsgId = ttNewEmail.iMsgId.
      DELETE ttNewEmail.
      IF bStored THEN DO:
        FOR EACH ttAttachment
            WHERE ttAttachment.iMsgId = ttEmail.iMsgId:
          DELETE ttAttachment.
        END.
        DELETE ttEmail.  
      END. 
    END.
    ELSE DO:
      IF NOT bStored THEN DO:
        ttNewEmail.cFileName = ttNewEmail.cFrom 
                             + "_" + STRING(YEAR(ttNewEmail.dDateSent)) + STRING(MONTH(ttNewEmail.dDateSent),"99") + STRING(DAY(ttNewEmail.dDateSent)) 
                             + "_" + STRING(ttNewEmail.iTimeSent)
                             + (IF ttNewEmail.iMsgType = 2 THEN ".htm" ELSE ".txt")
                             .
  
        CREATE ttEmail.  
        BUFFER-COPY ttNewEmail TO ttEmail.
        IF bLoadBlobs THEN DO:
          ttEmail.cFullPathName = SaveMailAsHTML(ttEmail.iMsgId).
          ImportAttachments(ttEmail.iMsgId).
        END.
        iCntNewMsg = iCntNewMsg + 1.
      END.
      ELSE IF bDeleteIfStored THEN 
        DeleteEmail(ttNewEmail.iMsgId).
    END.
  END.
END.
  
PUBLISH "NewEmailCount" (iCntNewMsg).
PUBLISH "RefreshEmailQuery".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getEmailTable) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getEmailTable Procedure 
PROCEDURE getEmailTable :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF OUTPUT PARAM ohttEmailBuffer AS HANDLE NO-UNDO.
DEF OUTPUT PARAM ohListener      AS HANDLE NO-UNDO.

ASSIGN ohttEmailBuffer = httEmailBuffer
       ohListener      = THIS-PROCEDURE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-InitializeObject) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeObject Procedure 
PROCEDURE InitializeObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR hField AS HANDLE NO-UNDO.

IF oEMailReader:setEmailServerProperties(cMailServer,cUserName,cPassword,iPort,
                                      "","","","",0)
  THEN DO:
/* IF oEMailReader:EmailServerLogin() THEN DO: */
  SUBSCRIBE TO "SuspendEmailListener" ANYWHERE.
  DO ix = 1 TO httEmailBuffer:NUM-FIELDS:
    hField = httEmailBuffer:BUFFER-FIELD(ix).
    CASE hField:NAME:
      WHEN "dDateSent" THEN hField:LABEL = IF DYNAMIC-FUNCTION("Scandinavian") THEN "Dato" ELSE "Date".
      WHEN "cTimeSent" THEN hField:LABEL = IF DYNAMIC-FUNCTION("Scandinavian") THEN "Kl"   ELSE "At".
      WHEN "cSubject"  THEN hField:LABEL = IF DYNAMIC-FUNCTION("Scandinavian") THEN "Emne" ELSE "Subject".
      WHEN "cFrom"     THEN hField:LABEL = IF DYNAMIC-FUNCTION("Scandinavian") THEN "Fra"  ELSE "From".
      WHEN "cTo"       THEN hField:LABEL = IF DYNAMIC-FUNCTION("Scandinavian") THEN "Til"  ELSE "To".
      WHEN "cCc"       THEN hField:LABEL = "CC".
      WHEN "cBcc"      THEN hField:LABEL = "BCC".
      WHEN "cBody"     THEN hField:LABEL = IF DYNAMIC-FUNCTION("Scandinavian") THEN "Tekst" ELSE "Text".
    END CASE.
  END.

  IF CAN-DO(cActionProcs,"2pdf") THEN DO:
    o2Pdf = NEW JBox2PdfBullZip().
    IF NUM-ENTRIES(cActionProcs) = NUM-ENTRIES(cActionParams,";") THEN
      cConvToPdfTypes = ENTRY(LOOKUP("2pdf",cActionProcs),cActionParams,";").
    IF cConvToPdfTypes NE "" THEN
      o2Pdf:setConvFileTypes(cConvToPdfTypes).

    chWord = DYNAMIC-FUNCTION("getWordHandle").
    IF VALID-HANDLE(chWord) THEN
      o2Pdf:setWordHandle(chWord).
  END.

END.
ELSE DO:
  IF cLogFile NE "" THEN 
    CreLog("Login for email server " + cMailServer + "/" + cUserName + " failed!").
  ELSE
    MESSAGE "Login for email server failed"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
  APPLY "close" TO THIS-PROCEDURE.  
  RETURN.
END. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SaveEmailMessage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SaveEmailMessage Procedure 
PROCEDURE SaveEmailMessage :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE bOk            AS LOG     NO-UNDO.
DEFINE VARIABLE cDocDesc       AS CHAR    NO-UNDO.
DEFINE VARIABLE cBody          AS CHAR    NO-UNDO.
DEFINE VARIABLE cEventId       AS CHAR    NO-UNDO.
DEFINE VARIABLE cEventContext  AS CHAR    NO-UNDO.
DEFINE VARIABLE cEventText     AS CHAR    NO-UNDO.
DEFINE VARIABLE cEventEntity   AS CHAR    NO-UNDO.
DEFINE VARIABLE cEventEntityId AS CHAR    NO-UNDO.

IF AVAIL ttNewEmail THEN DO:

  IF bConvHtmlToText THEN 
    cBody = DYNAMIC-FUNCTION("ConvHtmlToText",ttNewEmail.cBody,"text","text",iChrPrLineHtmlToTxt,"standard"). 
  ELSE cBody = ttNewEmail.cBody.

  IF AVAIL ttEmail AND ttEmail.iMsgId = ttNewEmail.iMsgId THEN
    ttEmail.cBody = cBody.

  IF CAN-DO(hParent:INTERNAL-ENTRIES,cMethod) THEN
    RUN VALUE(cMethod) IN hParent (ttNewEmail.cEntityId,ttNewEmail.cSubject,cBody,ttNewEmail.iMsgType,OUTPUT cDocDesc,OUTPUT bOk) NO-ERROR.

  IF NOT bOk AND INDEX(ttNewEmail.cSubject,"[") > 0 AND R-INDEX(ttNewEmail.cSubject,"]") > R-INDEX(ttNewEmail.cSubject,"[") THEN DO:
    cEventId = SUBSTR(ttNewEmail.cSubject,R-INDEX(ttNewEmail.cSubject,"[") + 1,R-INDEX(ttNewEmail.cSubject,"]") - R-INDEX(ttNewEmail.cSubject,"[") - 1).

    cEventContext = DYNAMIC-FUNCTION("getFieldValues",
                                     "FIRST JBoxEventLog",
                                     "WHERE iJBoxEventLogHeaderId > 0 AND iJBoxEventLogHeaderId = " + cEventId + " AND iJBoxCompanyId = " + DYNAMIC-FUNCTION("getCompany"),
                                     "cEntityTable,cEntityId,cEventText").
    IF cEventContext = ? THEN
      cEventContext = DYNAMIC-FUNCTION("getFieldValues",
                                       "FIRST JBoxEventLog",
                                       "WHERE iJBoxEventLogId = " + cEventId + " AND iJBoxCompanyId = " + DYNAMIC-FUNCTION("getCompany"),
                                       "cEntityTable,cEntityId,cEventText").
    IF cEventContext NE ? THEN DO:
      ASSIGN cEventEntity   = ENTRY(1,cEventContext,"|")
             cEventEntityId = ENTRY(2,cEventContext,"|")
             cEventText     = ENTRY(3,cEventContext,"|")
             .        
      bOk = cEventEntityId NE "".

      IF bOk THEN DO:
        DYNAMIC-FUNCTION("setLoadDocsAsync",YES).  
        UploadMailToDb(ttNewEmail.iMsgId,cEventEntity,cEventEntityId,cEventText,YES).  
      END. 

    END.
  END.
  IF NOT bOk AND cMethod NE "" THEN
    PUBLISH cMethod (ttNewEmail.cEntityId,ttNewEmail.cSubject,cBody,ttNewEmail.iMsgType,OUTPUT cDocDesc,OUTPUT bOk).
  
  PUBLISH "RefreshEmailQuery".
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SuspendEmailListener) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SuspendEmailListener Procedure 
PROCEDURE SuspendEmailListener :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ibSuspend AS LOG NO-UNDO.

bSuspend = ibSuspend.

IF ibSuspend THEN bFirstSuspend = YES.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-ConvertHtmlToText) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ConvertHtmlToText Procedure 
FUNCTION ConvertHtmlToText RETURNS LOGICAL
  ( INPUT ibConvHtmlToText AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
bConvHtmlToText = ibConvHtmlToText.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CreLog) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CreLog Procedure 
FUNCTION CreLog RETURNS LOGICAL
  ( INPUT icLogText AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF cLogFile NE "" THEN DO:
  OUTPUT STREAM sLog TO VALUE(cLogFile) APPEND.
  PUT STREAM sLog UNFORMATTED TODAY " " STRING(TIME,"hh:mm:ss") " " icLogText SKIP.
  OUTPUT STREAM sLog CLOSE.
END.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DeleteEmail) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION DeleteEmail Procedure 
FUNCTION DeleteEmail RETURNS LOGICAL
  ( INPUT iiMsgId AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR iReturn AS INT NO-UNDO.
DEF VAR ix      AS INT NO-UNDO.

IF oEmailReader:DeleteMsg(iiMsgId) THEN DO:

  FIND FIRST ttEmail 
       WHERE ttEmail.iMsgId = iiMsgId
       NO-ERROR.
  IF AVAIL ttEmail THEN DO: 
    DO ix = 1 TO NUM-ENTRIES(ttEmail.cAttachFiles,";"):
      IF SEARCH(ENTRY(ix,ttEmail.cAttachFiles,";")) NE ? THEN
        OS-DELETE VALUE(ENTRY(ix,ttEmail.cAttachFiles,";")).
    END.
    DELETE ttEmail.
  END.

  FIND FIRST ttNewEmail 
       WHERE ttNewEmail.iMsgId = iiMsgId
       NO-ERROR.
  IF AVAIL ttNewEmail THEN DELETE ttNewEmail.

  FOR EACH ttEmail WHERE ttEmail.iMsgId > iiMsgId:
    ttEmail.iMsgId = ttEmail.iMsgId - 1.
  END.
  FOR EACH ttNewEmail WHERE ttNewEmail.iMsgId > iiMsgId:
    ttNewEmail.iMsgId = ttNewEmail.iMsgId - 1.
  END.

  FOR EACH ttAttachment
      WHERE ttAttachment.iMsgId = iiMsgId:
    DELETE ttAttachment.
  END.

  FOR EACH ttAttachment
      WHERE ttAttachment.iMsgId > iiMsgId:
    ttAttachment.iMsgId = ttAttachment.iMsgId - 1.
  END.


  RETURN YES.
END.

RETURN NO.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getEmailServer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getEmailServer Procedure 
FUNCTION getEmailServer RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN cMailServer.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getEmailTT) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getEmailTT Procedure 
FUNCTION getEmailTT RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN httEmail.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getEmailUser) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getEmailUser Procedure 
FUNCTION getEmailUser RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN cUserName.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getLastLoadedDocIds) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getLastLoadedDocIds Procedure 
FUNCTION getLastLoadedDocIds RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN cLastLoadedDocIds.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTTAttachBuffer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTTAttachBuffer Procedure 
FUNCTION getTTAttachBuffer RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN httAttachBuffer.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTTEmailBuffer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTTEmailBuffer Procedure 
FUNCTION getTTEmailBuffer RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
RETURN httEmailBuffer.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTTNewEmailBuffer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTTNewEmailBuffer Procedure 
FUNCTION getTTNewEmailBuffer RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
RETURN httNewEmailBuffer.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ImportAttachments) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ImportAttachments Procedure 
FUNCTION ImportAttachments RETURNS CHARACTER
  ( INPUT iiMsgId AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR ix                AS INT  NO-UNDO.
DEF VAR cNewName          AS CHAR NO-UNDO.
DEF VAR cFileName         AS CHAR NO-UNDO.
DEF VAR cFileType         AS CHAR NO-UNDO.
DEF VAR cConvFileName     AS CHAR NO-UNDO.
DEF VAR cNoCompressTypes  AS CHAR NO-UNDO.

DEF BUFFER bttEmail FOR ttEmail.

FIND FIRST bttEmail
     WHERE bttEmail.iMsgId = iiMsgId
     NO-ERROR.
IF AVAIL bttEmail THEN DO:
  cNoCompressTypes = DYNAMIC-FUNCTION("getNoCompressTypes").  

  FOR EACH ttAttachment 
      WHERE ttAttachment.iMsgId = iiMsgId:
    DELETE ttAttachment.
  END.

  DO ix = 1 TO NUM-ENTRIES(bttEmail.cAttachments,";"):
    ASSIGN cFileName = ENTRY(ix,bttEmail.cAttachFiles,";")
           cFileType = ""
           .

    CreLog("Processing attachment: " + cFileName + " Actions: " + cActionProcs).

    IF CAN-DO(cActionProcs,"2pdf") AND NOT cFileName MATCHES "*.pdf" THEN DO:
      cConvFileName = o2Pdf:convert2Pdf(cFileName,cFileName + ".pdf").


      IF cConvFileName NE "" THEN DO:
        CreLog("pdf conversion done for attachment " + cFileName).
        ASSIGN cFileName = cConvFileName
               cFileType = "pdf".        
      END.
      ELSE CreLog("No pdf conversion for attachment " + cFileName).
    END.
    ELSE IF cFileName MATCHES "*.pdf" THEN
      cFileType = "pdf".

    FILE-INFO:FILE-NAME = cFileName. 
    CREATE ttAttachment.
    ASSIGN ttAttachment.iMsgId          = iiMsgId
           ttAttachment.cFileName       = ENTRY(ix,bttEmail.cAttachments,";") + (IF cFileType NE "" AND 
                                                                                    NOT ENTRY(ix,bttEmail.cAttachments,";") MATCHES "*." + cFileType  THEN 
                                                                                  "." + cFileType 
                                                                                 ELSE "")
           ttAttachment.cFullPathName   = FILE-INFO:FULL-PATHNAME
           ttAttachment.cDescription    = ttAttachment.cFileName
           ttAttachment.iAttachmentSize = FILE-INFO:FILE-SIZE
           ttAttachment.dAttachCreated  = FILE-INFO:FILE-CREATE-DATE
           ttAttachment.iAttachCreTime  = FILE-INFO:FILE-CREATE-TIME             
           ttAttachment.dAttachModified = FILE-INFO:FILE-MOD-DATE
           ttAttachment.iAttachModTime  = FILE-INFO:FILE-MOD-TIME
           ttAttachment.cFileType       = IF cFileType NE "" THEN cFileType 
                                          ELSE SUBSTR(ENTRY(ix,bttEmail.cAttachments,";"),R-INDEX(ENTRY(ix,bttEmail.cAttachments,";"),".") + 1)
           .
    IF SEARCH("gzip.exe") NE ? AND NOT CAN-DO(cNoCompressTypes,ttAttachment.cFileType) THEN DO:
      OS-COMMAND SILENT VALUE(SEARCH("gzip.exe") + ' "' + FILE-INFO:FULL-PATHNAME + '"').
      cNewName = FILE-INFO:FULL-PATHNAME + ".gz".
      IF SEARCH(cNewName) NE ? THEN DO:
        FILE-INFO:FILE-NAME = cNewName.
        ASSIGN ttAttachment.cFileName       = ttAttachment.cFileName + ".gz"
               ttAttachment.cFullPathName   = ttAttachment.cFullPathName + ".gz"
               ttAttachment.iAttachmentSize = FILE-INFO:FILE-SIZE
               .
      END.
    END.
    ELSE cNewName = ttAttachment.cFullPathName.
      
    COPY-LOB FROM FILE cNewName TO OBJECT ttAttachment.blAttachment.
  END.

END.
  
RETURN "". 

END FUNCTION.

/*

  IF bCompressFiles AND cGZIP NE ? AND NOT CAN-DO(cNoCompressTypes,SUBSTR(ENTRY(ix,icFileNames,";"),R-INDEX(ENTRY(ix,icFileNames,";"),".") + 1)) THEN DO:
    cFileName = SESSION:TEMP-DIR + ttDoc.cFileName.
    OS-COPY VALUE(FILE-INFO:FULL-PATHNAME) VALUE(cFileName).
    OS-COMMAND SILENT VALUE(SEARCH("gzip.exe") + ' "' + cFileName + '"').
    cFileName = cFileName + ".gz".
    IF SEARCH(cFileName) NE ? THEN DO:
      FILE-INFO:FILE-NAME = cFileName.
      ASSIGN bCompress = TRUE
             ttDoc.cFileName = ttDoc.cFileName + ".gz"
             ttDoc.iDocSize  = FILE-INFO:FILE-SIZE
             .
    END.
    ELSE cFileName = FILE-INFO:FULL-PATHNAME.
  END.
  ELSE cFileName = FILE-INFO:FULL-PATHNAME.
*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OpenAttachment) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION OpenAttachment Procedure 
FUNCTION OpenAttachment RETURNS LOGICAL
  ( INPUT iiMsgId          AS INT,
    INPUT icAttachmentList AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  icAttachmentList should be ; separated
------------------------------------------------------------------------------*/
DEF VAR ix        AS INT     NO-UNDO.
DEF VAR cNewName  AS CHAR    NO-UNDO.


DEF BUFFER bttEmail FOR ttEmail.

FIND FIRST bttEmail
     WHERE bttEmail.iMsgId = iiMsgId
     NO-ERROR.
IF NOT AVAIL bttEmail THEN RETURN NO.

IF AVAIL bttEmail THEN DO ix = 1 TO NUM-ENTRIES(bttEmail.cAttachments,";"):
  IF LOOKUP(ENTRY(ix,bttEmail.cAttachments,";"),icAttachmentList,";") > 0 THEN DO:
    cNewName = SESSION:TEMP-DIRECTORY 
             + ENTRY(NUM-ENTRIES(ENTRY(ix,bttEmail.cAttachFiles,";"),"\"),ENTRY(ix,bttEmail.cAttachFiles,";"),"\") + "_"
             + ENTRY(ix,bttEmail.cAttachments,";").
    OS-RENAME VALUE(ENTRY(ix,bttEmail.cAttachFiles,";")) VALUE(cNewName).
    DYNAMIC-FUNCTION("setWebDoc","",cNewName).
  END.
END.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ReleaseWordHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ReleaseWordHandle Procedure 
FUNCTION ReleaseWordHandle RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF VALID-HANDLE(chWord) THEN DO:
  RELEASE OBJECT chWord NO-ERROR.  
  RETURN YES.
END.
ELSE
  RETURN NO.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SaveMailAsHTML) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SaveMailAsHTML Procedure 
FUNCTION SaveMailAsHTML RETURNS CHARACTER
  ( INPUT iiMsgId   AS INT) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cBody       AS CHAR NO-UNDO.
DEF VAR cReturnChar AS CHAR NO-UNDO.
DEF VAR iSuccess    AS INT  NO-UNDO.
DEF VAR cLF         AS CHAR NO-UNDO.
DEF VAR cFileName   AS CHAR NO-UNDO.
DEF VAR cBlobFile   AS CHAR NO-UNDO.
DEF VAR cBodyFile   AS CHAR NO-UNDO.

DEF BUFFER bttEmail FOR ttEmail.

FIND FIRST bttEmail
     WHERE bttEmail.iMsgId = iiMsgId
     NO-ERROR.
IF AVAIL bttEmail THEN DO:
    
  IF bttEmail.iMsgType = 1 THEN  /* Text */
    cLF   = CHR(10).
  ELSE 
    cLF = "<br>".

  cFileName = SESSION:TEMP-DIRECTORY + bttEmail.cFileName.
        
  IF cFileName = "" THEN RETURN "".

  cBodyFile = SESSION:TEMP-DIRECTORY + "body_" + bttEmail.cFileName.

  IF bttEmail.iBodySize = 0 THEN DO:
    OUTPUT STREAM s1 TO VALUE(cFileName).
    EXPORT STREAM s1 bttEmail.blBody.
    OUTPUT STREAM s1 CLOSE.

    OUTPUT STREAM s1 TO VALUE(cBodyFile).
  
    PUT STREAM s1 UNFORMATTED 
        (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Dato: " ELSE "Date: ") 
           + STRING(bttEmail.dDateSent) + " " + STRING(bttEmail.iTimeSent,"HH:MM") + cLF
      + (IF bttEmail.iMsgType = 2 THEN "<strong>" ELSE cLF)
        + bttEmail.cSubject 
        + (IF bttEmail.iMsgType = 2 THEN "</strong>" ELSE cLF) + cLF
      + (IF bttEmail.cFrom NE "" THEN (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Fra: " ELSE "From: ")
                                         + bttEmail.cDisplayFrom 
                                         + (IF bttEmail.cFrom NE bttEmail.cDisplayFrom THEN "<" + bttEmail.cFrom + ">" ELSE "")
                                         + cLF 
                                       ELSE "") 
      + (IF bttEmail.cTo NE "" THEN (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Til: " ELSE "  To: ") + bttEmail.cDisplayTo + cLF ELSE "") 
      + (IF bttEmail.cCc NE "" THEN (IF DYNAMIC-FUNCTION("Scandinavian") THEN " CC: " ELSE "  CC: ") + bttEmail.cDisplayCc + cLF ELSE "")
      + (IF bttEmail.iMsgType = 2 THEN "<hr>" ELSE cLF)
      + (IF bttEmail.iMsgType = 1 THEN bttEmail.cBody ELSE "")
        .
    OUTPUT STREAM s1 CLOSE.
  
    IF bttEmail.iMsgType = 2 THEN DO:
      INPUT FROM VALUE(cFileName).
      IMPORT UNFORMATTED cBlobFile.
      INPUT CLOSE.
      cBlobFile = SESSION:TEMP-DIRECTORY + TRIM(cBlobFile,'"').
      OS-APPEND VALUE(cBlobFile) VALUE(cBodyFile).
    END.
  
    FILE-INFO:FILE-NAME = cBodyFile.
    bttEmail.iBodySize = FILE-INFO:FILE-SIZE.
    IF bLoadBlobs THEN DO:
      /*
      IF SEARCH("gzip.exe") NE ? AND SEARCH(cBodyFile) NE ? THEN DO:
        DO  ix = 1 TO 1000000:
        END.
        OS-COMMAND SILENT VALUE(SEARCH("gzip.exe") + ' "' + cBodyFile + '"').
        DO  ix = 1 TO 1000000:
        END.
        IF SEARCH(cBodyFile + ".gz") NE ? THEN 
          ASSIGN bttEmail.cFileName = bttEmail.cFileName + ".gz"
                 cBodyFile = cBodyFile + ".gz".
      END.
      */
      COPY-LOB FROM FILE cBodyFile TO OBJECT bttEmail.blBody.
    END.
  END.

END.
  
RETURN cBodyFile.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setActionProcsAndParams) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setActionProcsAndParams Procedure 
FUNCTION setActionProcsAndParams RETURNS LOGICAL
  ( INPUT icActionProcs  AS CHAR,
    INPUT icActionParams AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
ASSIGN cActionProcs  = icActionProcs
       cActionParams = icActionParams
       .

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setCheckStoredExp) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setCheckStoredExp Procedure 
FUNCTION setCheckStoredExp RETURNS LOGICAL
  ( INPUT icCheckStoredTable   AS CHAR,
    INPUT icCheckStoredFromFld AS CHAR,
    INPUT icCheckStoredDateFld AS CHAR,
    INPUT icCheckStoredTimeFld AS CHAR,
    INPUT icTargetGroupIdField AS CHAR,
    INPUT icTargetGroupId      AS CHAR,
    INPUT ibDeleteIfStored     AS LOG) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
ASSIGN cCheckStoredTable   = icCheckStoredTable
       cCheckStoredFromFld = icCheckStoredFromFld
       cCheckStoredDateFld = icCheckStoredDateFld
       cCheckStoredTimeFld = icCheckStoredTimeFld
       cTargetGroupIdField = icTargetGroupIdField
       cTargetGroupId      = icTargetGroupId
       bDeleteIfStored     = ibDeleteIfStored
       .

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setChrPrLineHtmlToTxt) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setChrPrLineHtmlToTxt Procedure 
FUNCTION setChrPrLineHtmlToTxt RETURNS LOGICAL
  ( INPUT iiChrPrLineHtmlToTxt AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
iChrPrLineHtmlToTxt = iiChrPrLineHtmlToTxt.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setDocLoadContext) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setDocLoadContext Procedure 
FUNCTION setDocLoadContext RETURNS LOGICAL
  ( INPUT icDocLoadContext AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
cDocLoadContext = icDocLoadContext.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setLoadBlobs) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setLoadBlobs Procedure 
FUNCTION setLoadBlobs RETURNS LOGICAL
  ( INPUT ibLoadBlobs AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
bLoadBlobs = ibLoadBlobs.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setLogFile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setLogFile Procedure 
FUNCTION setLogFile RETURNS LOGICAL
  ( INPUT icLogFile AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
cLogFile = icLogFile.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setMailServerProperties) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setMailServerProperties Procedure 
FUNCTION setMailServerProperties RETURNS LOGICAL
  ( INPUT icMailServer AS CHARACTER,
    INPUT iiPort       AS INTEGER,
    INPUT icUserName   AS CHARACTER,
    INPUT icPassword   AS CHARACTER,
    INPUT iiAuthMethod AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

IF icMailServer NE "" THEN
  cMailServer = icMailServer.

ASSIGN iPort       = iiPort
       cUserName   = icUserName
       cPassword   = icPassword
       iAuthMethod = iiAuthMethod
       .

IF iiPort = 0 THEN iiPort = 110.
IF iiAuthMethod = 0 THEN iAuthMethod = 1.
  
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setSaveBodyMethod) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setSaveBodyMethod Procedure 
FUNCTION setSaveBodyMethod RETURNS LOGICAL
  ( INPUT icMethod AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose: Set the timer method 
    Notes:  
------------------------------------------------------------------------------*/

cMethod = icMethod.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setSubjectList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setSubjectList Procedure 
FUNCTION setSubjectList RETURNS LOGICAL
  ( INPUT icSubjectList AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR ix AS INT NO-UNDO.

FOR EACH ttSubjectList:
  DELETE ttSubjectList.
END.

DO ix = 1 TO NUM-ENTRIES(icSubjectList,"|") BY 2:
  IF ENTRY(ix + 1,icSubjectList,"|") MATCHES "*å*" THEN DO:
    CREATE ttSubjectList.
    ASSIGN ttSubjectList.iMsgId   = INTEGER(ENTRY(ix,icSubjectList,"|"))
           ttSubjectList.cSubject = ENTRY(ix + 1,icSubjectList,"|")
           .
  END.

END.
  
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setUploadAttAsync) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setUploadAttAsync Procedure 
FUNCTION setUploadAttAsync RETURNS LOGICAL
  ( INPUT ibUploadAttAsync AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
bUploadAttAsync = ibUploadAttAsync.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SignOff) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SignOff Procedure 
FUNCTION SignOff RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
oEmailReader:SignOff(). /* A must to commit deletes */
DELETE OBJECT oEmailReader NO-ERROR.

IF VALID-OBJECT(o2Pdf) THEN DO:
  o2pdf:resetPrinter().
  DELETE OBJECT o2Pdf NO-ERROR.
END.
  
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-UploadMailToDb) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION UploadMailToDb Procedure 
FUNCTION UploadMailToDb RETURNS LOGICAL
  ( INPUT iiMsgId      AS INT,
    INPUT icEntity     AS CHAR,
    INPUT icEntityId   AS CHAR,
    INPUT icEntityText AS CHAR,
    INPUT ibDeleteMail AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cFileList AS CHAR    NO-UNDO.
DEF VAR mBlob     AS MEMPTR  NO-UNDO.  
DEF VAR iSuccess  AS INT     NO-UNDO.


DEF BUFFER bttEmail FOR ttEmail.

FIND FIRST bttEmail 
     WHERE bttEmail.iMsgId = iiMsgId
     NO-ERROR.
IF AVAIL bttEmail THEN DO:
  
  cFileList = SaveMailAsHTML(bttEmail.iMsgId). 

  IF cFileList = "" THEN DO:
    MESSAGE "Error in mail definition" SKIP
            "No temporary file name spesified"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN NO.
  END.

  SET-SIZE(mBlob) = 8.

  DO ix = 1 TO NUM-ENTRIES(bttEmail.cAttachments,";"):
    RUN SaveRecievedAttachment(iThreadHandle
                              ,ix       /*Attachment index*/
                              ,1        /*Save type. 1 = file, 2 = blob.*/
                              ,SESSION:TEMP-DIRECTORY + ENTRY(ix,bttEmail.cAttachments,";")
                              ,mBlob
                              ,0
                              ,OUTPUT iSuccess
                              ).
    IF iSuccess = 0 THEN
      cFileList = cFileList + (IF cFileList NE "" THEN ";" ELSE "") + SESSION:TEMP-DIRECTORY + ENTRY(ix,bttEmail.cAttachments,";").
  END.
  IF cFileList NE "" THEN DO:

    DYNAMIC-FUNCTION("setDocLoadFileDate",bttEmail.dDateSent,bttEmail.iTimeSent) NO-ERROR.
    DYNAMIC-FUNCTION("setDocLoadParam","linkdocs").

    IF DYNAMIC-FUNCTION("LoadDocs",cFileList,icEntity,icEntityId,icEntityText) THEN DO:
      cLastLoadedDocIds = DYNAMIC-FUNCTION("getOutParam").
      IF ibDeleteMail THEN
        DeleteEmail(iiMsgId).  
    END.
    ELSE ASSIGN iSuccess          = -1
                cLastLoadedDocIds = ""
                .
  END.
END.
  
SET-SIZE(mBlob) = 0.

RETURN iSuccess = 0.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

