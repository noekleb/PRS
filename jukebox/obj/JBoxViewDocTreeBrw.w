&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description:        JukeBox suppressed window template
                      This template is for use with a tabfolder or viewer object
                      to parent the suppressed window

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author:            brynjar@chemistry.no

  Created:           30.march.2008

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEF VAR bOk                 AS LOG    NO-UNDO.
DEF VAR ix                  AS INT    NO-UNDO.
DEF VAR hBrowse             AS HANDLE NO-UNDO.
DEF VAR hToolbar            AS HANDLE NO-UNDO.
DEF VAR hParent             AS HANDLE NO-UNDO.
DEF VAR hParentQueryObject  AS HANDLE NO-UNDO.
DEF VAR hBuffer             AS HANDLE NO-UNDO.


DEF VAR cEntityId           AS CHAR   NO-UNDO.
DEF VAR cContext            AS CHAR   NO-UNDO.
DEF VAR cDocDescription     AS CHAR   NO-UNDO.
DEF VAR hFillInDescription  AS HANDLE NO-UNDO.                           
DEF VAR cBaseQuery          AS CHAR   NO-UNDO.
DEF VAR bEnglish            AS LOG    NO-UNDO.
DEF VAR hPlusMinus          AS HANDLE NO-UNDO.

DEF VAR chEditWord          AS COM-HANDLE NO-UNDO.
DEF VAR chViewWord          AS COM-HANDLE NO-UNDO.
DEF VAR chActiveDoc         AS COM-HANDLE NO-UNDO.
DEF VAR chActiveWin         AS COM-HANDLE NO-UNDO.

DEF VAR cEditFile           AS CHAR NO-UNDO.
DEF VAR cEditKey            AS CHAR NO-UNDO.
DEF VAR cEditDesc           AS CHAR NO-UNDO.
DEF VAR cEditType           AS CHAR NO-UNDO.
DEF VAR dLastModDate        AS DATE NO-UNDO.
DEF VAR iLastModTime        AS INT  NO-UNDO.
DEF VAR iSaveOption         AS INT  NO-UNDO.
DEF VAR cContainerType      AS char NO-UNDO.
DEF VAR hColPlusMinus       AS HANDLE NO-UNDO.

DEF VAR cLinkLabelList      AS CHAR   NO-UNDO.
DEF VAR cLinkContextList    AS CHAR   NO-UNDO.
DEF VAR cLinkIdList         AS CHAR   NO-UNDO.
DEF VAR cLinkTargets        AS CHAR   NO-UNDO.
DEF VAR bDocMetaInstalled   AS LOG    NO-UNDO.
DEF VAR bDownLoadToTempDir  AS LOG    NO-UNDO.
DEF VAR cDocDownloadList    AS CHAR   NO-UNDO.
DEF VAR hDescDropDown       AS HANDLE NO-UNDO.

DEF TEMP-TABLE ttLinkTargets
    FIELD cLinkTarget AS CHAR
    FIELD hLinkTarget AS HANDLE
    .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS brwDocument rectToolBar 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AdjustBrowseColumns C-Win 
FUNCTION AdjustBrowseColumns RETURNS LOGICAL
  ( INPUT ihBrowse AS HANDLE,
    INPUT icBrowseName AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFrameHandle C-Win 
FUNCTION getFrameHandle RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD InitializeResize C-Win 
FUNCTION InitializeResize RETURNS LOGICAL
  ( )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setContainerType C-Win 
FUNCTION setContainerType RETURNS LOGICAL
  ( INPUT icContainerType AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setDocContext C-Win 
FUNCTION setDocContext RETURNS LOGICAL
  ( INPUT icEntityId       AS CHAR,
    INPUT icContext        AS CHAR,
    INPUT icDocDescription AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setEditAndRevisionMenu C-Win 
FUNCTION setEditAndRevisionMenu RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setFocus C-Win 
FUNCTION setFocus RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setKeywordDropDown C-Win 
FUNCTION setKeywordDropDown RETURNS LOGICAL
  ( INPUT icDropDownList AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setParent C-Win 
FUNCTION setParent RETURNS LOGICAL
  ( INPUT ihParent AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setPlusMinus C-Win 
FUNCTION setPlusMinus RETURNS LOGICAL
  ( INPUT icPlusMinus AS CHAR,
    INPUT iiDocId     AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setQuery C-Win 
FUNCTION setQuery RETURNS LOGICAL
  ( INPUT ihParentQueryObject AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE RECTANGLE brwDocument
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 85 BY 4.05.

DEFINE RECTANGLE rectToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 28 BY .95.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     brwDocument AT ROW 2.43 COL 2
     rectToolBar AT ROW 1.14 COL 1.8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 87 BY 5.76.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* SUPPRESS Window definition (used by the UIB) 
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "JukeBox plain suppressed window"
         HEIGHT             = 5.67
         WIDTH              = 86.6
         MAX-HEIGHT         = 28.57
         MAX-WIDTH          = 160
         VIRTUAL-HEIGHT     = 28.57
         VIRTUAL-WIDTH      = 160
         SHOW-IN-TASKBAR    = no
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
                                                                        */
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME
ASSIGN C-Win = CURRENT-WINDOW.


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win 
/* ************************* Included-Libraries *********************** */

{incl/devmode.i}
{incl/custdevmode.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* JukeBox plain suppressed window */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* JukeBox plain suppressed window */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
  /* The viewer or tabfolder object will see to that the Disable_UI is run
     and the container will make sure that all dynamic objects + resize settings are deleted
     also for the suppressed windows */
  DEF VAR iReturn       AS INT NO-UNDO.
  DEF VAR hSaveOptions  AS HANDLE NO-UNDO.

  IF DYNAMIC-FUNCTION("getIsOfficeComHandleActive",chEditWord) AND SEARCH(cEditFile) NE ? THEN DO:
    FILE-INFO:FILE-NAME = cEditFile.

    IF (FILE-INFO:FILE-MOD-DATE = dLastModDate AND FILE-INFO:FILE-MOD-TIME GT iLastModTime) OR
       FILE-INFO:FILE-MOD-DATE GT dLastModDate THEN DO:       

      RUN JBoxDocSaveOptions.w PERSIST SET hSaveOptions.
      RUN InitializeObject IN hSaveOptions.
      RUN MoveToTop IN hSaveOptions.
      IF iSaveOption = 0 THEN RETURN NO-APPLY.
    END.
    ELSE DO:
      RUN JBoxDDocSaveOnClose.w (OUTPUT iReturn).
      IF iReturn LE 1 THEN RETURN NO-APPLY.
      ELSE IF iReturn = 2 THEN
        DYNAMIC-FUNCTION("DoDelete","JBoxDocRev","ignore",
                      "iJBoxDocumentId,iDocRevNo",
                      cEditKey + "|-1",
                      YES).
    END.
  END.
  PUBLISH "InvalidateHandle".
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.

  &IF DEFINED(UIB_is_Running) NE 0 &THEN
    RUN InitializeObject.
  &ENDIF

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

{incl/supptrigg.i hBrowse}  /* <- To be able to capture keys (insert, delete..) on the browse */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DefaultActionBrowse C-Win 
PROCEDURE DefaultActionBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN OpenDocsRecord.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DeleteRecord C-Win 
PROCEDURE DeleteRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cLinkedDocTitles AS CHAR NO-UNDO.
DEF VAR cLinkedDocIds    AS CHAR NO-UNDO.
DEF VAR cParentDocIds    AS CHAR NO-UNDO.
DEF VAR cDocIdList       AS CHAR NO-UNDO.
DEF VAR cOtherRelToDocs  AS CHAR NO-UNDO.
DEF VAR cDocRelIdList    AS CHAR NO-UNDO.
DEF VAR iReturn          AS INT  NO-UNDO.
DEF VAR cDeleteParam     AS CHAR NO-UNDO.

DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
  IF hBrowse:FETCH-SELECTED-ROW(ix) THEN DO:
    cParentDocIds = cParentDocIds + (IF cParentDocIds NE "" THEN "," ELSE "") + STRING(hBuffer:BUFFER-FIELD("iJBoxDocumentId"):BUFFER-VALUE).
    IF hBuffer:BUFFER-FIELD("cPlusMinus"):BUFFER-VALUE = "+" THEN 
      RUN getLinkedDocs(hBuffer:BUFFER-FIELD("iJBoxDocumentId"):BUFFER-VALUE,INPUT-OUTPUT cLinkedDocTitles,INPUT-OUTPUT cLinkedDocIds,INPUT-OUTPUT cDocRelIdList).      
    ELSE
      ASSIGN cLinkedDocIds = cLinkedDocIds + (IF cLinkedDocIds NE "" THEN "," ELSE "") + STRING(hBuffer:BUFFER-FIELD("iJBoxDocumentId"):BUFFER-VALUE)
             cDocRelIdList = cDocRelIdList + (IF cDocRelIdList NE "" THEN "," ELSE "") + STRING(hBuffer:BUFFER-FIELD("iJBoxDocRelId"):BUFFER-VALUE)
             .
    IF hBuffer:BUFFER-FIELD("cOtherLinks"):BUFFER-VALUE NE "" THEN
      cOtherRelToDocs = cOtherRelToDocs + (IF cOtherRelToDocs NE "" THEN "," ELSE "") + hBuffer:BUFFER-FIELD("cOtherLinks"):BUFFER-VALUE.
  END.
END.

IF cParentDocIds = "" THEN DO:
  DYNAMIC-FUNCTION("DoMessage",0,0,IF DYNAMIC-FUNCTION("Scandinavian") THEN "Ingen rader er valgt" ELSE "No rows selected","","").
  RETURN.
END.

IF cLinkedDocTitles NE "" OR cOtherRelToDocs NE "" THEN DO:
  IF cLinkedDocTitles NE "" AND cOtherRelToDocs = "" THEN DO:
    IF DYNAMIC-FUNCTION("DoMessage",0,4,
                        (IF DYNAMIC-FUNCTION("Scandinavian") THEN
                          "Det er også tilknyttede dokumenter som vil bli slettet. Fortsett?"
                        ELSE
                          "There are also linked documents which will be deleted. Continue?") + CHR(10) + cLinkedDocTitles,
                         "","") = 6 THEN
      cDocIdList = cLinkedDocIds.
    ELSE RETURN.
  END.
  IF cOtherRelToDocs NE "" THEN DO:
    iReturn = DYNAMIC-FUNCTION("DoMessage",0,3,
                        (IF DYNAMIC-FUNCTION("Scandinavian") THEN
                          "Det finnes knytninger til andre begreper for dokument(er)." + CHR(10)
                        + "JA: Dokument(er) slettes sammen med alle knytninger" + CHR(10)
                        + "NEI: Dokument(er) og andre knytninger beholdes men denne knytningen (til " + cContext + ") slettes" + CHR(10)
                        + "AVBRYT: Ingen sletting"
                        ELSE
                          "There are links to other entities for document(s)." + CHR(10)
                        + "YES: Document(s) and all links to other entities are deleted" + CHR(10)
                        + "NO: Document(s) and other links are kept but this link (to " + cContext + ") is deleted" + CHR(10)
                        + "CANCEL: Nothing is deleted")
                        ,"","").
    IF iReturn = 2 THEN RETURN.
    IF iReturn = 6 THEN DO:
      cDeleteParam = "strong".  
      IF cLinkedDocTitles NE "" THEN DO:
        IF DYNAMIC-FUNCTION("DoMessage",0,4,
                            (IF DYNAMIC-FUNCTION("Scandinavian") THEN
                              "Det er også tilknyttede dokumenter (vedlegg) som vil bli slettet. Fortsett?"
                            ELSE
                              "There are also linked documents (attachments) that will be deleted. Continue?") + CHR(10) + cLinkedDocTitles,
                             "","") = 6 THEN
          cDocIdList = cLinkedDocIds.
        ELSE RETURN.
      END.
    END. 
    ELSE cDeleteParam = "weak".
  END.
END.
ELSE IF DYNAMIC-FUNCTION("DoMessage",0,4,
                      (IF DYNAMIC-FUNCTION("Scandinavian") THEN
                        "Slett valgte dokumenter?"
                      ELSE
                        "Delete seleced documents?"),
                       "","") = 6 THEN
  ASSIGN cDocIdList   = cParentDocIds
         cDeleteParam = "strong".
ELSE RETURN.    

IF cDeleteParam = "strong" THEN
  DO ix = 1 TO NUM-ENTRIES(cDocIdList):
    IF NOT DYNAMIC-FUNCTION("runproc","jbdoc_delete_one_doc.p",ENTRY(ix,cDocIdList),?) THEN DO:
      DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
      LEAVE.
    END.
  END.

ELSE IF cDeleteParam = "weak" THEN DO:
  DO ix = 1 TO NUM-ENTRIES(cDocRelIdList):
    DYNAMIC-FUNCTION("DoDelete","JBoxDocRel","ignore",
                     "iJBoxDocRelId",
                      ENTRY(ix,cDocRelIdList),
                      NO).
  END.
  DYNAMIC-FUNCTION("DoCommit",YES).
END.

PUBLISH "DocumentAddDelete" ("delete").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME DEFAULT-FRAME.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplayRecord C-Win 
PROCEDURE DisplayRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DYNAMIC-FUNCTION("DoLockWindow",THIS-PROCEDURE:CURRENT-WINDOW).
DYNAMIC-FUNCTION("setAttribute",SESSION,"userKeepsWindowLocked","yes").
RUN SUPER.
IF VALID-HANDLE(hFillInDescription) THEN
  hFillInDescription:SENSITIVE = NO.
ELSE IF VALID-HANDLE(hDescDropDown) THEN
  hDescDropDown:SENSITIVE = NO.
    
setEditAndRevisionMenu().
DYNAMIC-FUNCTION("setAttribute",SESSION,"userKeepsWindowLocked","").
DYNAMIC-FUNCTION("DoLockWindow",?).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DocumentAddDelete C-Win 
PROCEDURE DocumentAddDelete :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icAction AS CHAR NO-UNDO.

RUN StartQuery.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DropFileNotifyBrowse C-Win 
PROCEDURE DropFileNotifyBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF cContext = "" OR cEntityId = "" THEN DO:
  DYNAMIC-FUNCTION("DoMessage",513,0,"Dokument kunne ikke lastes inn på grunn av manglende koblingsbegrep","","").
  RETURN.
END.
DO ix = 1 TO hBrowse:NUM-DROPPED-FILES:
  DYNAMIC-FUNCTION("LoadDocs",hBrowse:GET-DROPPED-FILE(ix),cContext,cEntityId,cDocDescription).
END.

PUBLISH "DocumentAddDelete" ("add").

IF SEARCH("addDocRef.p") NE ? OR SEARCH("addDocRef.r") NE ? THEN
  RUN addDocRef.p (cContext,cEntityId,DYNAMIC-FUNCTION("getOutParam")). /* cOutParam: list of loaded docs */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EditDocRecord C-Win 
PROCEDURE EditDocRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cFileName     AS CHAR NO-UNDO.

IF hBrowse:NUM-SELECTED-ROWS > 1 OR DYNAMIC-FUNCTION("getIsOfficeComHandleActive",chEditWord) THEN DO:
  DYNAMIC-FUNCTION("DoMessage",0,0,
                   IF bEnglish THEN "You may only edit one document at a time" ELSE "Du kan bare editere et dokument om gangen",
                   "","").
  RETURN.
END.

ASSIGN cEditKey     = STRING(hBuffer:BUFFER-FIELD("iJBoxDocumentId"):BUFFER-VALUE)
       cEditType    = hBuffer:BUFFER-FIELD("cFileType"):BUFFER-VALUE
       cEditDesc    = hBuffer:BUFFER-FIELD("cDescription"):BUFFER-VALUE
       .

DYNAMIC-FUNCTION("ViewDocs","JBoxDocument",cEditKey,NO,"").

cFileName = DYNAMIC-FUNCTION("getTmpDocFileNames").
FILE-INFO:FILE-NAME = cFileName.

ASSIGN cEditFile    = FILE-INFO:FULL-PATHNAME
       dLastModDate = FILE-INFO:FILE-MOD-DATE
       iLastModTime = FILE-INFO:FILE-MOD-TIME
       .

IF cEditType MATCHES "*doc*" THEN DO:
  CREATE "Word.Application" chEditWord.
  
  chEditWord:ENABLE-EVENTS("WordEvents").
  
  chEditWord:Documents:OPEN(cFileName,,0).
  ASSIGN chEditWord:WindowState = 0
         chEditWord:Visible     = TRUE
         chActiveDoc            = chEditWord:ActiveDocument
         chActiveWin            = chEditWord:ActiveWindow
         .
END.
ELSE DO:
  CREATE "Excel.Application" chEditWord.
  
  chEditWord:ENABLE-EVENTS("ExcelEvents").
  
  chEditWord:Workbooks:OpenText(cFileName,2,,,,,TRUE).
  ASSIGN 
/*          chEditWord:WindowState = 0 */
         chEditWord:Visible     = TRUE
/*          chActiveDoc            = chEditWord:ActiveDocument */
         chActiveWin            = chEditWord:ActiveWindow
         .
END.

IF DYNAMIC-FUNCTION("getFieldValues","JBoxDocRev",
                    "WHERE iJBoxDocumentId = " + cEditKey
                  + " AND iDocRevNo = -1","cRevCreatedBy") = ? THEN
  DYNAMIC-FUNCTION("DoCreate","JBoxDocRev","ignore",
                   "iJBoxDocumentId,iDocRevNo,cRevCreatedBy,dRevCreated,iRevCreTime",
                   cEditKey + "|-1|" + DYNAMIC-FUNCTION("getASuserId") + "|" + STRING(TODAY) + "|" + STRING(TIME),
                   YES).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EditRecord C-Win 
PROCEDURE EditRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF VALID-HANDLE(hFillInDescription) THEN DO:
  hFillInDescription:SENSITIVE = YES.
  APPLY "entry" TO hFillInDescription.
END.
ELSE IF VALID-HANDLE(hDescDropDown) THEN DO:
  hDescDropDown:SENSITIVE = YES.
  APPLY "entry" TO hDescDropDown.
END.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EmailRecord C-Win 
PROCEDURE EmailRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEF VAR oJboxEmail AS JBoxEmailViaDefaultClient NO-UNDO.
DEF VAR cToList    AS CHAR NO-UNDO.
DEF VAR cCcList    AS CHAR NO-UNDO.
DEF VAR cBccList   AS CHAR NO-UNDO.
DEF VAR cSubject   AS CHAR NO-UNDO.
DEF VAR cBody      AS CHAR NO-UNDO.

PUBLISH "getEmailContext" (OUTPUT cToList,OUTPUT cCcList,OUTPUT cBccList,OUTPUT cSubject,OUTPUT cBody).

bDownLoadToTempDir = YES.
RUN SaveToDisc.
oJBoxEmail = NEW JBoxEmailViaDefaultClient(cSubject,cBody,cDocDownloadList).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  ENABLE brwDocument rectToolBar 
      WITH FRAME DEFAULT-FRAME.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExcelEvents.WorkbookDeactivate C-Win 
PROCEDURE ExcelEvents.WorkbookDeactivate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER Wb AS COM-HANDLE NO-UNDO.

DEF VAR hSaveOptions AS HANDLE NO-UNDO.

IF SEARCH(cEditFile) NE ? THEN DO:
  FILE-INFO:FILE-NAME = cEditFile.
  
  IF (FILE-INFO:FILE-MOD-DATE = dLastModDate AND FILE-INFO:FILE-MOD-TIME GT iLastModTime) OR
     FILE-INFO:FILE-MOD-DATE GT dLastModDate THEN DO:

    THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = NO.

    RUN JBoxDocSaveOptions.w PERSIST SET hSaveOptions.
    RUN InitializeObject IN hSaveOptions.
    RUN MoveToTop IN hSaveOptions.
    DYNAMIC-FUNCTION("setDescription" IN hSaveOptions,cEditDesc).
  END.
  ELSE   
    DYNAMIC-FUNCTION("DoDelete","JBoxDocRev","ignore",
      "iJBoxDocumentId,iDocRevNo",
      cEditKey + "|-1",
      YES).
END.
ELSE THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = YES.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getLinkedDocs C-Win 
PROCEDURE getLinkedDocs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT        PARAM icParentDocId  AS CHAR NO-UNDO.
DEF INPUT-OUTPUT PARAM ocDocDescList  AS CHAR NO-UNDO.
DEF INPUT-OUTPUT PARAM ocDocIdList    AS CHAR NO-UNDO.
DEF INPUT-OUTPUT PARAM ocDocRelIdList AS CHAR NO-UNDO.

DEF VAR hBuffLinkedDocs AS HANDLE NO-UNDO.
DEF VAR hQuery          AS HANDLE NO-UNDO.

CREATE BUFFER hBuffLinkedDocs FOR TABLE hBuffer.
CREATE QUERY  hQuery.
hQuery:SET-BUFFERS(hBuffLinkedDocs).
hQuery:QUERY-PREPARE("FOR EACH " + hBuffLinkedDocs:NAME + " WHERE iToDocumentId = " + icParentDocId).
hQuery:QUERY-OPEN().
hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  ASSIGN ocDocDescList  = ocDocDescList + CHR(10) + hBuffLinkedDocs:BUFFER-FIELD("cDescription"):BUFFER-VALUE
         ocDocIdList    = ocDocIdList + (IF ocDocIdList NE "" THEN "," ELSE "") + STRING(hBuffLinkedDocs:BUFFER-FIELD("iFromDocumentId"):BUFFER-VALUE)
         ocDocRelIdList = ocDocRelIdList + (IF ocDocRelIdList NE "" THEN "," ELSE "") + STRING(hBuffLinkedDocs:BUFFER-FIELD("iJBoxDocRelId"):BUFFER-VALUE)
         .
  hQuery:GET-NEXT().
END.

DELETE OBJECT hQuery.
DELETE OBJECT hBuffLinkedDocs.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeObject C-Win 
PROCEDURE InitializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Create any dynamic objects, initialize drop-down lists etc
  Parameters:  <none>
  Notes:       The dynamic query object must be linked to the THIS-PROCEDURE
------------------------------------------------------------------------------*/

DO WITH FRAME {&FRAME-NAME}:

  ASSIGN cLinkTargets      = DYNAMIC-FUNCTION("getFieldValues","JBoxSysParam",
                                    "WHERE cSysParamName = 'DocumentLinkTarget'(codemaster)","cSysParamCharValue")
         bDocMetaInstalled = DYNAMIC-FUNCTION("getIsTableInstalled","JBoxDocMeta")
         bEnglish = NOT DYNAMIC-FUNCTION("Scandinavian")
         .

  IF cLinkTargets = ? THEN cLinkTargets = "".

  SUBSCRIBE TO "DocumentAddDelete" ANYWHERE.

  DYNAMIC-FUNCTION("NewObject",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE,"procedure").

  hBrowse = DYNAMIC-FUNCTION("NewBrowse"
    ,brwDocument:HANDLE
    ,100
    ,"multiple"
    ,"temp-table"
     + ";cPlusMinus|CHARACTER| x||+"
     + ";iJBoxDocumentId|INTEGER|>>>>>>9||Id"
     + ";cDescription|CHARACTER|x(50)||" + (IF bEnglish THEN 
                                              (IF bDocMetaInstalled THEN "Keywords" ELSE "Description")
                                            ELSE 
                                              (IF bDocMetaInstalled THEN "Nøkkelord" ELSE "Beskrivelse")
                                            )
     + ";cFileName|CHARACTER|x(50)||" + (IF bEnglish THEN "File name" ELSE "Filnavn")
     + ";cFileType|CHARACTER|x(5)||" + (IF bEnglish THEN "File type" ELSE "Filtype")
     + ";dFileCreateDate|DATE|99/99/9999||" + (IF bEnglish THEN "File created" ELSE "Fil opprettet")
     + ";cFileCreateTime|CHARACTER|x(8)||" + (IF bEnglish THEN "File cre.time" ELSE "Fil oppr.tid")
     + ";!iFileCreateTime|INTEGER|>>>>>>9||" + (IF bEnglish THEN "File cre.time" ELSE "Fil oppr.tid")
     + ";dFileModDate|DATE|99/99/9999||" + (IF bEnglish THEN "File modified" ELSE "Fil endret")
     + ";cFileModTime|CHARACTER|x(8)||" + (IF bEnglish THEN "File mod.time" ELSE "Fil mod.tid")
     + ";!iFileModTime|INTEGER|>>>>>>9||" + (IF bEnglish THEN "File mod.time" ELSE "Fil mod.tid")
     + ";dCreated|DATE|99/99/9999||" + (IF bEnglish THEN "Load-date" ELSE "Lastet dato")
     + ";cCreatedBy|CHARACTER|x(10)||" + (IF bEnglish THEN "Loaded by" ELSE "Lastet av")
     + ";dModified|DATE|99/99/9999||" + (IF bEnglish THEN "Modified date" ELSE "Endret dato")
     + ";cModifiedBy|CHARACTER|x(10)||" + (IF bEnglish THEN "Modified by" ELSE "Endret av")
     + ";iDocSize|INTEGER|>>>>>>>>>9||" + (IF bEnglish THEN "Size" ELSE "Str")
     + ";cFullPathName|CHARACTER|x(80)||" + (IF bEnglish THEN "Loaded from" ELSE "Lastet fra")
     + ";!DocRowident|CHARACTER|x(20)||"

     + ";cOtherLinks|CHARACTER|x(50)||" + (IF bEnglish THEN "Document links" ELSE "Dokumentknytninger")                             
     + ";!cOtherLinkKeys|CHARACTER|x(30)||Link keys"

     + ";!iJBoxDocRelId|INTEGER|>>>>>>9||Rel.id"
     + ";!cEntityId|CHARACTER|x(10)||Entity id"
     + ";!cContext|CHARACTER|x(20)||Context"
    
     + ";!iToDocumentId|INTEGER|>>>>>>9||To doc.id"
     + ";!iFromDocumentId|INTEGER|>>>>>>9||From doc.id"

     + ";!cDocRevList|CHARACTER|x(80)||Doc rev list"  /* for right-click menu */
     + ";!cLockInfo|CHARACTER|x(40)||Lock info"
    ,"WHERE false"
    ,"").

  DYNAMIC-FUNCTION("createObjectLink",hBrowse,THIS-PROCEDURE).

  hBuffer = hBrowse:QUERY:GET-BUFFER-HANDLE(1).

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"fieldnamedeletewarning","cFileName").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"noMoveColumns","cPlusMinus").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"windowsBrowse","yes").
  DYNAMIC-FUNCTION("setSortString",hBrowse,"iJBoxDocumentId;desc").


  hToolbar = DYNAMIC-FUNCTION("NewToolBar",
                    rectToolBar:HANDLE,
                    IF cContainerType = "window" THEN (IF bEnglish THEN "File" ELSE "Fil") ELSE "",            
                    "LoadFiles;" + (IF bEnglish THEN "Load files" ELSE "Last inn filer") + "ctrl-L;;;bmp/open16e.bmp¤enable"
                  + ",Edit;"  + (IF bEnglish THEN "Edit &description" ELSE "Endre &beskrivelse")
                  + ",Delete" + (IF NOT bEnglish THEN ";Slett" ELSE "")
                  + ",Refresh;" + (IF bEnglish THEN "Refresh" ELSE "Oppdater")
                  + ",Excel"
                  + ",OpenDocs;"
                    + (IF bEnglish THEN
                         "Open document;Open selected documents"
                       ELSE
                         "Åpne dokument;Åpne valgte dokumenter")
                    + ";OpenDocsRecord;bmp/folderopen.bmp"
                  + ",SaveToDisc;"
                    + (IF bEnglish THEN
                        "SaveFiles;Save selected files to disc"
                       ELSE
                        "Hent filer;Hent filer til eget filområde")
                    + ";SaveToDisc;bmp/save16e.bmp"         
                  + ",email"                                             
                    ,"maxborder").

  IF DYNAMIC-FUNCTION("Scandinavian") THEN
    hBrowse:TOOLTIP = "Dobbelklikk for å åpne dokument" +
                      (IF NOT CAN-DO(DYNAMIC-FUNCTION("getSecDisabledActions",""),"save") THEN
                        CHR(10) + "Dra og slipp her for å laste dokument"
                       ELSE "").
  ELSE 
    hBrowse:TOOLTIP = "Doubleclick to open document" +
                      (IF NOT CAN-DO(DYNAMIC-FUNCTION("getSecDisabledActions",""),"save") THEN
                        CHR(10) + "Drop new documents here"
                       ELSE "").

  IF NOT CAN-DO(DYNAMIC-FUNCTION("getSecDisabledActions",""),"save") AND
     DYNAMIC-FUNCTION("getIsEventAllowed",hToolbar,"save") THEN
    hBrowse:DROP-TARGET = TRUE.

  hFillInDescription = DYNAMIC-FUNCTION("NewBrowseFillIn",hBrowse,
                                        "cDescription","",
                                        "","","","").
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hFillInDescription,"cDescription").
  
  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hToolbar).


  IF CAN-DO(hParent:INTERNAL-ENTRIES,"setDocTreeResize") THEN
    DYNAMIC-FUNCTION("setDocTreeResize" IN hParent,FRAME {&FRAME-NAME}:HANDLE,hBrowse).
  ELSE
  do:
    DYNAMIC-FUNCTION("setNoMoveY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,"+,brwDocument," + hBrowse:NAME + "," + DYNAMIC-FUNCTION("getToolBarNames",hToolbar,"")).
    DYNAMIC-FUNCTION("setNoMoveX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,"+,brwDocument," + hBrowse:NAME + "," + DYNAMIC-FUNCTION("getToolBarNames",hToolbar,"")).    
  end.   
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LinkDocument C-Win 
PROCEDURE LinkDocument :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icEntity       AS CHAR NO-UNDO.
DEF INPUT PARAM icEntityIdList AS CHAR NO-UNDO.

DEF VAR cLastLoadedDocIds  AS CHAR NO-UNDO.
DEF VAR cEmailIdList       AS CHAR NO-UNDO.
DEF VAR cReturn            AS CHAR NO-UNDO.
DEF VAR cRowIdList         AS CHAR NO-UNDO.
DEF VAR cFromList          AS CHAR NO-UNDO.
DEF VAR cContact           AS CHAR NO-UNDO.
DEF VAR ix                 AS INT  NO-UNDO.
DEF VAR cCalcParam         AS CHAR NO-UNDO.

DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
  IF hBrowse:FETCH-SELECTED-ROW(ix) THEN DO:
    cReturn = DYNAMIC-FUNCTION("getFieldList","JBoxDocLink;iFromDocumentId",
                               "WHERE iToDocumentId = " + STRING(hBuffer:BUFFER-FIELD("iJBoxDocumentId"):BUFFER-VALUE)).
    IF cReturn = "" THEN
      cReturn = STRING(hBuffer:BUFFER-FIELD("iJBoxDocumentId"):BUFFER-VALUE).
  
    ASSIGN cLastLoadedDocIds = cLastLoadedDocIds + (IF cLastLoadedDocIds NE "" THEN "," ELSE "") + REPLACE(cReturn,"|",",")
           cRowIdList        = cRowIdList + (IF cRowIdList NE "" THEN "," ELSE "") + hBuffer:BUFFER-FIELD("DocRowident"):BUFFER-VALUE           
           .
  END.
END.
IF cLastLoadedDocIds = "" THEN DO:
  DYNAMIC-FUNCTION("DoMessage",1,0,"Ingen rader er valgt","","").
  RETURN.
END.

IF NOT DYNAMIC-FUNCTION("runProc","jbdoc_cre_docrel.p",
                 icEntity + "¤" + icEntityIdList + "¤" + cLastLoadedDocIds
/*                  icEntity + "¤" + SUBSTR(icEntityIdList,INDEX(icEntityIdList,"|") + 1) + "¤" + cLastLoadedDocIds */
                ,?) THEN
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
ELSE 
  RUN StartQuery.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LinkRecord C-Win 
PROCEDURE LinkRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cReturn         AS CHAR NO-UNDO.
DEF VAR hLinkTargetProc AS HANDLE NO-UNDO.
  
IF NUM-ENTRIES(cLinkTargets,";") > 2 THEN DO:
  RUN JBoxDSimpleSelectList.w (REPLACE(cLinkTargets,";","|"),?,OUTPUT cReturn).
  IF cReturn = ? THEN RETURN.
END.
ELSE IF NUM-ENTRIES(cLinkTargets,";") = 2 THEN
  cReturn = ENTRY(2,cLinkTargets,";").

IF SEARCH("LinkDocTo" + cReturn + ".w") = ? AND SEARCH("LinkDocTo" + cReturn + ".r") = ? AND SEARCH("LinkDocTo" + cReturn + ".p") = ? THEN DO:
  DYNAMIC-FUNCTION("DoMessage",0,0,"Mangler program for knytning av dokument til " + cReturn,"","").
  RETURN.  
END.

IF hBrowse:NUM-SELECTED-ROWS = 0 THEN DO:
  DYNAMIC-FUNCTION("DoMessage",1,0,"Ingen rad er valgt","","").
  RETURN.
END.

FIND FIRST ttLinkTargets 
     WHERE ttLinkTargets.cLinkTarget = cReturn
     NO-ERROR.
IF AVAIL ttLinkTargets AND VALID-HANDLE(ttLinkTargets.hLinkTarget) THEN
  hLinkTargetProc = ttLinkTargets.hLinkTarget.

IF SEARCH("LinkDocTo" + cReturn + ".p") NE ? THEN
  RUN VALUE("LinkDocTo" + cReturn + ".p") ("link","docmgr","",INPUT-OUTPUT hLinkTargetProc).
ELSE
  RUN VALUE("LinkDocTo" + cReturn + ".w") ("link","docmgr","",INPUT-OUTPUT hLinkTargetProc).

IF VALID-HANDLE(hLinkTargetProc) THEN DO:
  FIND FIRST ttLinkTargets
       WHERE ttLinkTargets.cLinkTarget = cReturn
       NO-ERROR.
  IF NOT AVAIL ttLinkTargets THEN DO:
    CREATE ttLinkTargets.
    ttLinkTargets.cLinkTarget = cReturn.
  END.
  ttLinkTargets.hLinkTarget = hLinkTargetProc.

  IF CAN-DO(hLinkTargetProc:INTERNAL-ENTRIES,"MoveToTop") THEN
    RUN MoveToTop IN hLinkTargetProc.
  ELSE hLinkTargetProc:CURRENT-WINDOW:MOVE-TO-TOP().
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LoadFilesRecord C-Win 
PROCEDURE LoadFilesRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cFileNames AS CHAR NO-UNDO.

IF cContext = "" OR cEntityId = "" THEN DO:
  DYNAMIC-FUNCTION("DoMessage",513,0,"Dokument kunne ikke lastes inn på grunn av manglende koblingsbegrep","","").
  RETURN.
END.

cFileNames = DYNAMIC-FUNCTION("SelectFileNames",
                              IF bEnglish THEN "All files|*.*" ELSE "Alle filer|*.*",?,
                              IF bEnglish THEN "Select files" ELSE "Velg filer").

IF cFileNames NE "" THEN DO:

  DYNAMIC-FUNCTION("LoadDocs",cFileNames,cContext,cEntityId,cDocDescription).
  
  IF SEARCH("addDocRef.p") NE ? OR SEARCH("addDocRef.r") NE ? THEN
    RUN addDocRef.p (cContext,cEntityId,DYNAMIC-FUNCTION("getOutParam")). /* cOutParam: list of loaded docs */

  PUBLISH "DocumentAddDelete" ("add").    
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LookupRecord C-Win 
PROCEDURE LookupRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cLookupLabel    AS CHAR   NO-UNDO.
DEF VAR cLookupContext  AS CHAR   NO-UNDO.
DEF VAR cLookupIdList   AS CHAR   NO-UNDO.
DEF VAR hMenuItem       AS HANDLE NO-UNDO.
DEF VAR hLinkTargetProc AS HANDLE NO-UNDO.
DEF VAR ix              AS INT    NO-UNDO.

ASSIGN hMenuItem      = DYNAMIC-FUNCTION("getCurrentWidget")
       cLookupLabel   = hMenuItem:LABEL
       cLookupContext = ENTRY(LOOKUP(cLookupLabel,cLinkLabelList),cLinkContextList)
       .
DO ix = 1 TO NUM-ENTRIES(cLinkContextList):
  IF ENTRY(ix,cLinkContextList) = cLookupContext THEN
    cLookupIdList = cLookupIdList + (IF cLookupIdList NE "" THEN "¤" ELSE "") + ENTRY(ix,cLinkIdList,"¤").
END.

FIND FIRST ttLinkTargets 
     WHERE ttLinkTargets.cLinkTarget = cLookupContext
     NO-ERROR.
IF AVAIL ttLinkTargets AND VALID-HANDLE(ttLinkTargets.hLinkTarget) THEN
  hLinkTargetProc = ttLinkTargets.hLinkTarget.

IF SEARCH("LinkDocTo" + cLookupContext + ".p") NE ? THEN
  RUN VALUE("LinkDocTo" + cLookupContext + ".p") ("lookup","email",cLookupContext + "," + cLookupIdList,INPUT-OUTPUT hLinkTargetProc).
ELSE
  RUN VALUE("LinkDocTo" + cLookupContext + ".w") ("lookup","email",cLookupContext + "," + cLookupIdList,INPUT-OUTPUT hLinkTargetProc).

IF VALID-HANDLE(hLinkTargetProc) THEN DO:
  FIND FIRST ttLinkTargets
       WHERE ttLinkTargets.cLinkTarget = cLookupContext
       NO-ERROR.
  IF NOT AVAIL ttLinkTargets THEN DO:
    CREATE ttLinkTargets.
    ttLinkTargets.cLinkTarget = cLookupContext.
  END.
  ttLinkTargets.hLinkTarget = hLinkTargetProc.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MoveToTop C-Win 
PROCEDURE MoveToTop :
/*------------------------------------------------------------------------------
  Purpose:     Mak
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FRAME {&FRAME-NAME}:MOVE-TO-TOP().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MySaveBrowseDropDown C-Win 
PROCEDURE MySaveBrowseDropDown :
DEF INPUT  PARAM ihDropDown AS HANDLE NO-UNDO.
DEF INPUT  PARAM ihBuffer   AS HANDLE NO-UNDO.
DEF OUTPUT PARAM obOk       AS LOG    NO-UNDO.

IF ihDropDown = hDescDropDown THEN
  obOk = DYNAMIC-FUNCTION("DoUpdate","JBoxDocument","ignore" + (IF bDocMetaInstalled THEN ",jbdoc_post_update.p" ELSE ""),
                  "",
                  hBuffer:BUFFER-FIELD("DocRowident"):BUFFER-VALUE,
                  "cDescription",
                  hDescDropDown:SCREEN-VALUE,
                  YES).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MySaveBrowseFillIn C-Win 
PROCEDURE MySaveBrowseFillIn :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT  PARAM ihFillIn AS HANDLE NO-UNDO.
DEF INPUT  PARAM ihBuffer AS HANDLE NO-UNDO.
DEF OUTPUT PARAM obOk     AS LOG    NO-UNDO INIT YES.

IF ihFillIn = hFillInDescription THEN
  obOk = DYNAMIC-FUNCTION("DoUpdate","JBoxDocument","ignore" + (IF bDocMetaInstalled THEN ",jbdoc_post_update.p" ELSE ""),
                  "",
                  hBuffer:BUFFER-FIELD("DocRowident"):BUFFER-VALUE,
                  "cDescription",
                  hFillInDescription:SCREEN-VALUE,
                  YES).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenDocs C-Win 
PROCEDURE OpenDocs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icDocIdList AS CHAR NO-UNDO.

IF icDocIdList = "" THEN
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN
      DYNAMIC-FUNCTION("ViewDocs","JBoxDocument",STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("iJBoxDocumentId"):BUFFER-VALUE),TRUE,"").
  END.
ELSE DO ix = 1 TO NUM-ENTRIES(icDocIdList):
  DYNAMIC-FUNCTION("ViewDocs","JBoxDocument",ENTRY(ix,icDocIdList),TRUE,"").
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenDocsRecord C-Win 
PROCEDURE OpenDocsRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cLinkedDocTitles AS CHAR NO-UNDO.
DEF VAR cLinkedDocIds    AS CHAR NO-UNDO.
DEF VAR cParentDocIds    AS CHAR NO-UNDO.
DEF VAR cDocRelIdList    AS CHAR NO-UNDO. /* Not in use here */

DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
  IF hBrowse:FETCH-SELECTED-ROW(ix) THEN DO:
    cParentDocIds = cParentDocIds + (IF cParentDocIds NE "" THEN "," ELSE "") + STRING(hBuffer:BUFFER-FIELD("iJBoxDocumentId"):BUFFER-VALUE).
    IF hBuffer:BUFFER-FIELD("cPlusMinus"):BUFFER-VALUE = "+" THEN 
      RUN getLinkedDocs(hBuffer:BUFFER-FIELD("iJBoxDocumentId"):BUFFER-VALUE,INPUT-OUTPUT cLinkedDocTitles,INPUT-OUTPUT cLinkedDocIds,INPUT-OUTPUT cDocRelIdList).      
    ELSE cLinkedDocIds = cLinkedDocIds + (IF cLinkedDocIds NE "" THEN "," ELSE "") + STRING(hBuffer:BUFFER-FIELD("iJBoxDocumentId"):BUFFER-VALUE).
  END.
END.

IF cLinkedDocTitles NE "" THEN DO:
  IF DYNAMIC-FUNCTION("DoMessage",0,4,
                      (IF DYNAMIC-FUNCTION("Scandinavian") THEN
                        "Åpne alle tilknyttede dokumenter?"
                      ELSE
                        "Open all linked documents?") + CHR(10) + cLinkedDocTitles,
                       "","") = 6 THEN
    RUN OpenDocs(cLinkedDocIds).
  ELSE RUN OpenDocs(cParentDocIds).
END.
ELSE
  RUN OpenDocs(cParentDocIds).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RefreshRecord C-Win 
PROCEDURE RefreshRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN StartQuery.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReleaseLockRecord C-Win 
PROCEDURE ReleaseLockRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF hBuffer:AVAIL AND hBrowse:NUM-SELECTED-ROWS = 1 THEN DO:
  IF DYNAMIC-FUNCTION("DoDelete","JBoxDocRev","ignore",
                      "iJBoxDocumentId,iDocRevNo",
                      STRING(hBuffer:BUFFER-FIELD("iJBoxDocumentId"):BUFFER-VALUE) + "|-1",
                      YES) THEN
    RUN StartQuery.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReplaceDocRecord C-Win 
PROCEDURE ReplaceDocRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cFileName AS CHAR NO-UNDO.

IF NOT hBuffer:AVAIL THEN RETURN.

IF hBrowse:NUM-SELECTED-ROWS > 1 THEN DO:
  DYNAMIC-FUNCTION("DoMessage",0,0,
                   IF bEnglish THEN "You may only replace one document at a time" ELSE "Du kan bare erstatte et dokument om gangen",
                   "","").
  RETURN.
END.
      
SYSTEM-DIALOG GET-FILE cFileName 
       TITLE (IF bEnglish THEN "Select file" ELSE "Velg fil")
       FILTERS (IF bEnglish THEN "All files" ELSE "Alle filer") "*.*"
       UPDATE bOk.

IF bOk THEN DO:
  DYNAMIC-FUNCTION("setDocLoadParam","replacedoc").
  DYNAMIC-FUNCTION("LoadDocs",cFileName,"",
                   STRING(hBuffer:BUFFER-FIELD("iJBoxDocumentId"):BUFFER-VALUE),
                   hBuffer:BUFFER-FIELD("cDescription"):BUFFER-VALUE).

  IF DYNAMIC-FUNCTION("DoDelete","JBoxDocRev","ignore",
                      "iJBoxDocumentId,iDocRevNo",
                      STRING(hBuffer:BUFFER-FIELD("iJBoxDocumentId"):BUFFER-VALUE) + "|-1",
                      YES) THEN
    RUN StartQuery.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RowEntry C-Win 
PROCEDURE RowEntry :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iMousePos AS INT NO-UNDO.
DEF VAR rRepos    AS ROWID NO-UNDO.
DEF VAR iCurrRow  AS INT   NO-UNDO.

IF hBuffer:AVAIL AND hBuffer:BUFFER-FIELD("cPlusMinus"):BUFFER-VALUE NE "" THEN DO:
  iMousePos = DYNAMIC-FUNCTION("getMousePosition",FRAME {&FRAME-NAME}:HANDLE,"x").
  IF iMousePos LT hBrowse:X + 13 THEN DO:
    ASSIGN rRepos = hBuffer:ROWID
           iCurrRow = hBrowse:FOCUSED-ROW
           .
    setPlusMinus(hBuffer:BUFFER-FIELD("cPlusMinus"):BUFFER-VALUE,hBuffer:BUFFER-FIELD("iJBoxDocumentId"):BUFFER-VALUE).
    bOK = hBuffer:FIND-BY-ROWID(rRepos).
    IF bOk THEN DO:
      hBrowse:SET-REPOSITIONED-ROW(iCurrRow,"conditional").
      hBrowse:QUERY:REPOSITION-TO-ROWID(rRepos).
    END.
    RETURN.
  END.
END.
RUN SUPER.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SaveChangedDoc C-Win 
PROCEDURE SaveChangedDoc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM iiSaveOption AS INT  NO-UNDO.
DEF INPUT PARAM icEditDesc   AS CHAR NO-UNDO.

DEF VAR rRepos  AS ROWID NO-UNDO.
DEF VAR bRepos  AS LOG   NO-UNDO.

THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = YES.

iSaveOption = iiSaveOption.

IF iiSaveOption < 4 THEN
  DYNAMIC-FUNCTION("DoDelete","JBoxDocRev","ignore",
      "iJBoxDocumentId,iDocRevNo",
      cEditKey + "|-1",
      YES).

IF iiSaveOption GT 0 AND iiSaveOption LE 2 THEN DO:

  DYNAMIC-FUNCTION("setDocLoadParam","replacedoc" + (IF iiSaveOption = 2 THEN ",saveoldversion" ELSE "")).

  IF NOT DYNAMIC-FUNCTION("LoadDocs",cEditFile,"",cEditKey,IF icEditDesc NE ? THEN icEditDesc ELSE cEditDesc) THEN 
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getOutParam"),"","").

END.

IF hBuffer:AVAIL AND cEditKey = STRING(hBuffer:BUFFER-FIELD("iJBoxDocumentId"):BUFFER-VALUE) THEN
  bRepos = YES.

RUN StartQuery.

IF bRepos THEN DO:
  bOk = hBuffer:FIND-FIRST("WHERE iJBoxDocumentId = " + cEditKey) NO-ERROR.  
  IF bOk THEN DO:
    hBrowse:QUERY:REPOSITION-TO-ROWID(hBuffer:ROWID).
    RUN InvokeMethod(hBrowse,"DisplayRecord").
  END.
END.


IF cEditType MATCHES "doc*" OR cEditType MATCHES "xls*" THEN
  RELEASE OBJECT chEditWord NO-ERROR.

THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SaveDocs C-Win 
PROCEDURE SaveDocs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icDocIdList AS CHAR NO-UNDO.

DEF VAR cDir             AS CHAR NO-UNDO.

IF icDocIdList = "" THEN RETURN.

cDocDownloadList = "".

IF NOT bDownLoadToTempDir THEN DO:
  SYSTEM-DIALOG GET-DIR cDir.
  IF cDir = "" THEN RETURN.
END.
ELSE cDir = SESSION:TEMP-DIR. 

IF icDocIdList = "" THEN
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN DO:
      IF DYNAMIC-FUNCTION("ViewDocs","JBoxDocument",STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("iJBoxDocumentId"):BUFFER-VALUE),FALSE,cDir) THEN
        cDocDownloadList = cDocDownloadList + (IF cDocDownloadList NE "" THEN ";" ELSE "") + DYNAMIC-FUNCTION("getTmpDocFileNames").
    END.  
  END.
ELSE DO ix = 1 TO NUM-ENTRIES(icDocIdList):
  IF DYNAMIC-FUNCTION("ViewDocs","JBoxDocument",ENTRY(ix,icDocIdList),FALSE,cDir) THEN
    cDocDownloadList = cDocDownloadList + (IF cDocDownloadList NE "" THEN ";" ELSE "") + DYNAMIC-FUNCTION("getTmpDocFileNames").
END.

IF NOT bDownLoadToTempDir AND DYNAMIC-FUNCTION("DoMessage",0,4,
                    IF bEnglish THEN
                      "Explore directory " + cDir
                    ELSE
                      "Vis filområde " + cDir
                    ,"","") = 6 THEN
  DYNAMIC-FUNCTION("ExploreDirectory",cDir).

bDownLoadToTempDir = NO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SaveToDisc C-Win 
PROCEDURE SaveToDisc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cLinkedDocTitles AS CHAR NO-UNDO.
DEF VAR cLinkedDocIds    AS CHAR NO-UNDO.
DEF VAR cParentDocIds    AS CHAR NO-UNDO.
DEF VAR cDocRelIdList    AS CHAR NO-UNDO. /* Not in use here */

cDocDownloadList = "".

DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
  IF hBrowse:FETCH-SELECTED-ROW(ix) THEN DO:
    cParentDocIds = cParentDocIds + (IF cParentDocIds NE "" THEN "," ELSE "") + STRING(hBuffer:BUFFER-FIELD("iJBoxDocumentId"):BUFFER-VALUE).
    IF hBuffer:BUFFER-FIELD("cPlusMinus"):BUFFER-VALUE = "+" THEN 
      RUN getLinkedDocs(hBuffer:BUFFER-FIELD("iJBoxDocumentId"):BUFFER-VALUE,INPUT-OUTPUT cLinkedDocTitles,INPUT-OUTPUT cLinkedDocIds,INPUT-OUTPUT cDocRelIdList).      
    ELSE cLinkedDocIds = cLinkedDocIds + (IF cLinkedDocIds NE "" THEN "," ELSE "") + STRING(hBuffer:BUFFER-FIELD("iJBoxDocumentId"):BUFFER-VALUE).
  END.
END.

IF cLinkedDocTitles NE "" THEN DO:
  IF DYNAMIC-FUNCTION("DoMessage",0,4,
                      (IF DYNAMIC-FUNCTION("Scandinavian") THEN
                        "Hent alle tilknyttede dokumenter?"
                      ELSE
                        "Fetch all linked documents?") + CHR(10) + cLinkedDocTitles,
                       "","") = 6 THEN
    RUN SaveDocs(cLinkedDocIds).
  ELSE RUN SaveDocs(cParentDocIds).
END.
ELSE
  RUN SaveDocs(cParentDocIds).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartQuery C-Win 
PROCEDURE StartQuery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF VALID-HANDLE(hBuffer) THEN DO:
  hBuffer:EMPTY-TEMP-TABLE() NO-ERROR.

  DYNAMIC-FUNCTION("getMyTempTable","jbdoc_getdoctree.p",
                   IF cBaseQuery NE "" THEN cBaseQuery ELSE cContext + "|" + cEntityId,
                   hBuffer).

  setPlusMinus("-",0).
END.  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartSearch C-Win 
PROCEDURE StartSearch :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF hBrowse:CURRENT-COLUMN:NAME = "cPlusMinus" THEN DO:
  setPlusMinus(TRIM(hBrowse:CURRENT-COLUMN:LABEL),0).
  IF hBrowse:CURRENT-COLUMN:LABEL = " +" THEN
    hBrowse:CURRENT-COLUMN:LABEL = " -".
  ELSE
    hBrowse:CURRENT-COLUMN:LABEL = " +".
END.
ELSE DO:
  setPlusMinus("-",0).
  RUN SUPER.  
END. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ViewOlderVersionRecord C-Win 
PROCEDURE ViewOlderVersionRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cVersionLabel AS CHAR   NO-UNDO.
DEF VAR hMenuItem     AS HANDLE NO-UNDO.

ASSIGN hMenuItem     = DYNAMIC-FUNCTION("getCurrentWidget")
       cVersionLabel = hMenuItem:LABEL
       .

DYNAMIC-FUNCTION("ViewDocs","JBoxDocRev",
                 STRING(hBuffer:BUFFER-FIELD("iJBoxDocumentId"):BUFFER-VALUE) + "|" + ENTRY(1,cVersionLabel,"-"),
                 TRUE,"").


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WordEvents.Quit C-Win 
PROCEDURE WordEvents.Quit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR hSaveOptions AS HANDLE NO-UNDO.

IF SEARCH(cEditFile) NE ? THEN DO:
  FILE-INFO:FILE-NAME = cEditFile.
  
  IF (FILE-INFO:FILE-MOD-DATE = dLastModDate AND FILE-INFO:FILE-MOD-TIME GT iLastModTime) OR
     FILE-INFO:FILE-MOD-DATE GT dLastModDate THEN DO:

    THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = NO.

    RUN JBoxDocSaveOptions.w PERSIST SET hSaveOptions.
    RUN InitializeObject IN hSaveOptions.
    RUN MoveToTop IN hSaveOptions.
    DYNAMIC-FUNCTION("setDescription" IN hSaveOptions,cEditDesc).
  END.
  ELSE   
    DYNAMIC-FUNCTION("DoDelete","JBoxDocRev","ignore",
      "iJBoxDocumentId,iDocRevNo",
      cEditKey + "|-1",
      YES).

END.
ELSE THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = YES.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AdjustBrowseColumns C-Win 
FUNCTION AdjustBrowseColumns RETURNS LOGICAL
  ( INPUT ihBrowse AS HANDLE,
    INPUT icBrowseName AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hColumn AS HANDLE NO-UNDO.

hPlusMinus = DYNAMIC-FUNCTION("getBrowseColumn",ihBrowse,"cPlusMinus").
ASSIGN hPlusMinus:WIDTH-PIXELS = 12
       hPlusMinus:LABEL = " +"
       hPlusMinus:FONT = 6
       .
  
hColumn = DYNAMIC-FUNCTION("getBrowseColumn",ihBrowse,"cDescription").
hColumn:WIDTH-PIXELS = 200.

hColumn = DYNAMIC-FUNCTION("getBrowseColumn",ihBrowse,"cFileName").
hColumn:WIDTH-PIXELS = 200.

hColumn = DYNAMIC-FUNCTION("getBrowseColumn",ihBrowse,"cFileType").
hColumn:WIDTH-PIXELS = 40.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFrameHandle C-Win 
FUNCTION getFrameHandle RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose: Make the frame of the child procedure known to the tabfolder or viewer object
    Notes: The procedure is mandatory
------------------------------------------------------------------------------*/

  RETURN FRAME {&FRAME-NAME}:HANDLE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION InitializeResize C-Win 
FUNCTION InitializeResize RETURNS LOGICAL
  ( ) :
/*------------------------------------------------------------------------------
  Purpose:
------------------------------------------------------------------------------*/

DO WITH FRAME {&FRAME-NAME}:
  DYNAMIC-FUNCTION("setNoMoveX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,"DEFAULT-FRAME,rectToolBar,brwDocument").    
  DYNAMIC-FUNCTION("setNoMoveY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,"rectToolBar,brwDocument").    
END.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setContainerType C-Win 
FUNCTION setContainerType RETURNS LOGICAL
  ( INPUT icContainerType AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
cContainerType = icContainerType.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setDocContext C-Win 
FUNCTION setDocContext RETURNS LOGICAL
  ( INPUT icEntityId       AS CHAR,
    INPUT icContext        AS CHAR,
    INPUT icDocDescription AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR ix         AS INT  NO-UNDO.

hPlusMinus:LABEL = " +".

IF NUM-ENTRIES(icEntityId,";") > 1 THEN
  DO ix = 1 TO NUM-ENTRIES(icEntityId,";"):
    cBaseQuery = cBaseQuery + (IF cBaseQuery NE "" THEN " OR (cEntityId = '" ELSE "(cEntityId = '")
               + ENTRY(ix,icEntityId,";") + "' AND cContext = '" + ENTRY(ix,icContext,";") + "')".
  END.

ASSIGN cEntityId       = ENTRY(1,icEntityId,";")
       cContext        = ENTRY(1,icContext,";")
       cDocDescription = icDocDescription.

RUN StartQuery.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setEditAndRevisionMenu C-Win 
FUNCTION setEditAndRevisionMenu RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR bCanEdit      AS LOG  NO-UNDO.
DEF VAR bCanReplace   AS LOG  NO-UNDO.
DEF VAR cVersionMenu  AS CHAR NO-UNDO.
DEF VAR ix            AS INT  NO-UNDO.
DEF VAR iy            AS INT  NO-UNDO.
DEF VAR hOlderVerMenu AS HANDLE NO-UNDO.

DEF VAR cLookupMenu   AS CHAR   NO-UNDO.
DEF VAR hLookupMenu   AS HANDLE NO-UNDO.
DEF VAR cLinkLabel    AS CHAR   NO-UNDO.
DEF VAR bAddMenu      AS LOG    NO-UNDO.

DELETE OBJECT hBrowse:POPUP-MENU NO-ERROR.

IF NOT hBuffer:AVAIL THEN RETURN NO.

ASSIGN cLinkLabelList   = ""
       cLinkContextList = ""
       cLinkIdList      = ""
       .

bCanEdit = CAN-DO("doc,docx,xls,xlsx",hBuffer:BUFFER-FIELD("cFileType"):BUFFER-VALUE)
           AND NOT DYNAMIC-FUNCTION("getIsOfficeComHandleActive",chEditWord)
           AND hBuffer:BUFFER-FIELD("cLockInfo"):BUFFER-VALUE = "" 
           .

bCanReplace = ENTRY(1,hBuffer:BUFFER-FIELD("cLockInfo"):BUFFER-VALUE,"|") = DYNAMIC-FUNCTION("getASuserId").

DO ix = 1 TO NUM-ENTRIES(hBuffer:BUFFER-FIELD("cDocRevList"):BUFFER-VALUE):
  cVersionMenu = cVersionMenu 
               + (IF cVersionMenu NE "" THEN "," ELSE "")
               + "ViewOlderVersion;"
               + ENTRY(ix,hBuffer:BUFFER-FIELD("cDocRevList"):BUFFER-VALUE)
               .
END.

DO ix = 1 TO NUM-ENTRIES(hBuffer:BUFFER-FIELD("cOtherLinks"):BUFFER-VALUE):
  IF LOOKUP(ENTRY(ix,hBuffer:BUFFER-FIELD("cOtherLinks"):BUFFER-VALUE),cLinkTargets,";") MOD 2 = 0 THEN
    cLinkLabel = ENTRY(LOOKUP(ENTRY(ix,hBuffer:BUFFER-FIELD("cOtherLinks"):BUFFER-VALUE),cLinkTargets,";") - 1,cLinkTargets,";") NO-ERROR.
  ELSE IF LOOKUP(ENTRY(ix,hBuffer:BUFFER-FIELD("cOtherLinks"):BUFFER-VALUE),cLinkTargets,";") > 0 THEN
    cLinkLabel = ENTRY(LOOKUP(ENTRY(ix,hBuffer:BUFFER-FIELD("cOtherLinks"):BUFFER-VALUE),cLinkTargets,";"),cLinkTargets,";").
  ELSE
    CASE ENTRY(ix,hBuffer:BUFFER-FIELD("cOtherLinks"):BUFFER-VALUE):
      WHEN "Dokument" THEN cLinkLabel = IF bEnglish THEN "Case" ELSE "Sak".
      OTHERWISE cLinkLabel = ENTRY(ix,hBuffer:BUFFER-FIELD("cOtherLinks"):BUFFER-VALUE).
    END CASE.

  ASSIGN cLinkLabelList   = cLinkLabelList + (IF cLinkLabelList NE "" THEN "," ELSE "") + cLinkLabel
         cLinkContextList = cLinkContextList + (IF cLinkContextList NE "" THEN "," ELSE "") + ENTRY(ix,hBuffer:BUFFER-FIELD("cOtherLinks"):BUFFER-VALUE)
         cLinkIdList      = cLinkIdList + (IF cLinkIdList NE "" THEN "¤" ELSE "") + ENTRY(ix,hBuffer:BUFFER-FIELD("cOtherLinkKeys"):BUFFER-VALUE,"¤")
         bAddMenu         = YES
         .
  DO iy = 1 TO NUM-ENTRIES(cLookupMenu):
    IF ENTRY(2,ENTRY(iy,cLookupMenu),";") = cLinkLabel THEN bAddMenu = NO.
  END.
  IF bAddMenu THEN
     cLookupMenu  = cLookupMenu 
                  + (IF cLookupMenu NE "" THEN "," ELSE "")
                  + "Lookup;"
                  + cLinkLabel.
                  
END.

DYNAMIC-FUNCTION("NewMenuBand",hBrowse
                ,"MultiSortBrowse;" + (IF bEnglish THEN "Sort on multiple columns" ELSE "Sorter på flere kolonner")
              + (IF cLinkTargets NE "" THEN
                   ",link;" + (IF bEnglish THEN "Add reference for document" ELSE "Legg til referanse for dokument")
                 ELSE "")
              + (IF bCanEdit THEN
                  ",EditDoc;" + (IF bEnglish THEN "Edit document" ELSE "Rediger dokument")
                 ELSE "")
              + (IF bCanReplace THEN
                  ",ReplaceDoc;" + (IF bEnglish THEN "Replace document" ELSE "Erstatt dokument")
                 ELSE "")
              + (IF cVersionMenu NE "" THEN 
                  ",|" + (IF bEnglish THEN "Older versions" ELSE "Tidligere versjoner")
                 ELSE "")
              + (IF DYNAMIC-FUNCTION("getAttribute",SESSION,"userlevel") MATCHES "*super"
                    AND hBuffer:BUFFER-FIELD("cLockInfo"):BUFFER-VALUE NE "" THEN
                  ",ReleaseLock;" + (IF bEnglish THEN "Release document lock" ELSE "Opphev lås for dokument")
                 ELSE "")
              + (IF cLookupMenu NE "" THEN 
                  ",|" + (IF bEnglish THEN "View" ELSE "Vis")
                 ELSE "")
                ,"").

IF cVersionMenu NE "" THEN DO:
  hOlderVerMenu = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hBrowse,"placeholder1")) NO-ERROR.
  IF VALID-HANDLE(hOlderVerMenu) THEN
    DYNAMIC-FUNCTION("NewMenuBand",hOlderVerMenu
    ,cVersionMenu
    ,"").
END.

IF cLookupMenu NE "" THEN DO:
  hLookupMenu = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hBrowse,
                                               "placeholder" + (IF cVersionMenu NE "" THEN "2" ELSE "1"))) NO-ERROR.
  IF VALID-HANDLE(hLookupMenu) THEN
    DYNAMIC-FUNCTION("NewMenuBand",hLookupMenu
    ,cLookupMenu
    ,"").
END.

RETURN YES.

END FUNCTION.

/*

  
DEF VAR cLookupMenu   AS CHAR   NO-UNDO.
DEF VAR ix            AS INT    NO-UNDO.
DEF VAR hLookupMenu   AS HANDLE NO-UNDO.
DEF VAR cLinkLabel    AS CHAR   NO-UNDO.

DELETE OBJECT hBrowse:POPUP-MENU NO-ERROR.
ASSIGN cLinkLabelList   = ""
       cLinkContextList = ""
       cLinkIdList      = ""
       .

IF NOT hBuffer:AVAIL THEN RETURN NO.

DO ix = 1 TO NUM-ENTRIES(hBuffer:BUFFER-FIELD("cOtherLinks"):BUFFER-VALUE):
  IF LOOKUP(ENTRY(ix,hBuffer:BUFFER-FIELD("cOtherLinks"):BUFFER-VALUE),cLinkTargets,";") MOD 2 = 0 THEN
    cLinkLabel = ENTRY(LOOKUP(ENTRY(ix,hBuffer:BUFFER-FIELD("cOtherLinks"):BUFFER-VALUE),cLinkTargets,";") - 1,cLinkTargets,";").
  ELSE IF LOOKUP(ENTRY(ix,hBuffer:BUFFER-FIELD("cOtherLinks"):BUFFER-VALUE),cLinkTargets,";") > 0 THEN
    cLinkLabel = ENTRY(LOOKUP(ENTRY(ix,hBuffer:BUFFER-FIELD("cOtherLinks"):BUFFER-VALUE),cLinkTargets,";"),cLinkTargets,";").
  ELSE
    CASE ENTRY(ix,hBuffer:BUFFER-FIELD("cOtherLinks"):BUFFER-VALUE):
      WHEN "Dokument" THEN cLinkLabel = IF bScand THEN "Sak" ELSE "Case".
      OTHERWISE cLinkLabel = ENTRY(ix,hBuffer:BUFFER-FIELD("cOtherLinks"):BUFFER-VALUE).
    END CASE.

  ASSIGN cLinkLabelList   = cLinkLabelList + (IF cLinkLabelList NE "" THEN "," ELSE "") + cLinkLabel
         cLinkContextList = cLinkContextList + (IF cLinkContextList NE "" THEN "," ELSE "") + ENTRY(ix,hBuffer:BUFFER-FIELD("cOtherLinks"):BUFFER-VALUE)
         cLinkIdList      = cLinkIdList + (IF cLinkIdList NE "" THEN "¤" ELSE "") + ENTRY(ix,hBuffer:BUFFER-FIELD("OtherLinkKeys"):BUFFER-VALUE,"¤")
         cLookupMenu      = cLookupMenu 
                          + (IF cLookupMenu NE "" THEN "," ELSE "")
                          + "Lookup;"
                          + cLinkLabel
         .
END.
  
*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setFocus C-Win 
FUNCTION setFocus RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
APPLY "entry" TO hBrowse.

RETURN hBrowse.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setKeywordDropDown C-Win 
FUNCTION setKeywordDropDown RETURNS LOGICAL
  ( INPUT icDropDownList AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:
------------------------------------------------------------------------------*/
DYNAMIC-FUNCTION("DeleteObject",hFillInDescription).

hDescDropDown = DYNAMIC-FUNCTION("NewBrowseDropDown",hBrowse,"cDescription","cDescription"
              ,"","",icDropDownList). 
DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hDescDropDown,"cDescription").

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setParent C-Win 
FUNCTION setParent RETURNS LOGICAL
  ( INPUT ihParent AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose: Set the handle to the parent procedure for the suppressed window
    Notes: This function is not mandatory but is called if it exists
------------------------------------------------------------------------------*/
hParent = ihParent.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setPlusMinus C-Win 
FUNCTION setPlusMinus RETURNS LOGICAL
  ( INPUT icPlusMinus AS CHAR,
    INPUT iiDocId     AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hBuffPlusMinus AS HANDLE NO-UNDO.
DEF VAR hQuery         AS HANDLE NO-UNDO.
DEF VAR bAvailChild    AS LOG    NO-UNDO.
DEF VAR bChild         AS LOG    NO-UNDO.

CREATE BUFFER hBuffPlusMinus FOR TABLE hBuffer.
CREATE QUERY  hQuery.
hQuery:SET-BUFFERS(hBuffPlusMinus).
hQuery:QUERY-PREPARE("FOR EACH " + hBuffPlusMinus:NAME + " WHERE "
                   + (IF iiDocId NE 0 THEN "iToDocumentId = " + STRING(iiDocId) ELSE "true"))
                     .
hQuery:QUERY-OPEN().
hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  bAvailChild = hBuffer:FIND-FIRST("WHERE iToDocumentId = " + STRING(hBuffPlusMinus:BUFFER-FIELD("iToDocumentId"):BUFFER-VALUE)
                                 + " AND iFromDocumentId NE " + STRING(hBuffPlusMinus:BUFFER-FIELD("iToDocumentId"):BUFFER-VALUE))
                                   NO-ERROR.
  bChild = hBuffPlusMinus:BUFFER-FIELD("iFromDocumentId"):BUFFER-VALUE NE hBuffPlusMinus:BUFFER-FIELD("iToDocumentId"):BUFFER-VALUE.

  IF icPlusMinus = "+" AND bChild THEN 
    hBuffPlusMinus:BUFFER-FIELD("cPlusMinus"):BUFFER-VALUE = "".
  ELSE IF icPlusMinus = "+" AND bAvailChild THEN
    hBuffPlusMinus:BUFFER-FIELD("cPlusMinus"):BUFFER-VALUE = "-".
  ELSE IF icPlusMinus = "-" AND bChild THEN
    hBuffPlusMinus:BUFFER-FIELD("cPlusMinus"):BUFFER-VALUE = "0".
  ELSE IF icPlusMinus = "-" AND bAvailChild THEN
    hBuffPlusMinus:BUFFER-FIELD("cPlusMinus"):BUFFER-VALUE = "+".

  hQuery:GET-NEXT().
END.

DELETE OBJECT hQuery.
DELETE OBJECT hBuffPlusMinus.

DYNAMIC-FUNCTION("setAttribute",hBrowse,"queryFilter",
                 "WHERE cPlusMinus NE '0'"   
                 ).

RUN InvokeMethod(hBrowse,"OpenQuery").

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setQuery C-Win 
FUNCTION setQuery RETURNS LOGICAL
  ( INPUT ihParentQueryObject AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose: Set the handle of the parent (or oneToOne navigation query/browse) so
           it is available for the child object 
    Notes: This function is not mandatory but is called if it exists 
------------------------------------------------------------------------------*/
hParentQueryObject = ihParentQueryObject.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

