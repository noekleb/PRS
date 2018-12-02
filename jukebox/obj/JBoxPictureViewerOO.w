&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

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
&IF "{1}" = "Developer_Studio_is_Running" &THEN
  &SCOPED-DEFINE UIB_is_Running 1 
&ENDIF   
/* Uncomment to enable use of .Net components: */
&SCOPED-DEFINE AdvGuiWin

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{windows.i}

DEF VAR bOK                 AS LOG     NO-UNDO.
DEF VAR ix                  AS INT     NO-UNDO.
DEF VAR hToolbar            AS HANDLE  NO-UNDO.
DEF VAR cContext            AS CHAR    NO-UNDO.
DEF VAR cKey                AS CHAR    NO-UNDO.
DEF VAR cTmpFileNames       AS CHAR    NO-UNDO.
DEF VAR cOrgFileNames       AS CHAR    NO-UNDO.
DEF VAR cFileDates          AS CHAR    NO-UNDO.
DEF VAR cFileDescs          AS CHAR    NO-UNDO.
DEF VAR cFileIds            AS CHAR    NO-UNDO.
DEF VAR cFileTypes          AS CHAR    NO-UNDO.
DEF VAR iHorSize            AS INT     NO-UNDO INIT 130.
DEF VAR iVerSize            AS INT     NO-UNDO INIT 118.
DEF VAR iMinHorSize         AS INT     NO-UNDO INIT 130.
DEF VAR iMinVerSize         AS INT     NO-UNDO INIT 118.
DEF VAR cCurrSmallPicture   AS CHAR    NO-UNDO.
DEF VAR hZoomIn             AS HANDLE  NO-UNDO.
DEF VAR hZoomOut            AS HANDLE  NO-UNDO.
DEF VAR oForm               AS JBoxWrapperForm NO-UNDO.
DEF VAR bMdiChild           AS LOG     NO-UNDO.
DEF VAR iStartRow           AS INT     NO-UNDO INIT 1.
DEF VAR hBtnFirst           AS HANDLE  NO-UNDO.
DEF VAR hBtnPrev            AS HANDLE  NO-UNDO.
DEF VAR hBtnNext            AS HANDLE  NO-UNDO.
DEF VAR hBtnLast            AS HANDLE  NO-UNDO.
DEF VAR cPictureLoadMode    AS CHAR    NO-UNDO.
DEF VAR iViewCount          AS INT     NO-UNDO.
DEF VAR iColumns            AS INT     NO-UNDO.
DEF VAR iRows               AS INT     NO-UNDO.
DEF VAR bScand              AS LOG     NO-UNDO.
DEF VAR oStatusBar          AS JBoxStatusBar   NO-UNDO.
DEF VAR cLinkTargets        AS CHAR    NO-UNDO.
DEF VAR bDownLoadToTempDir  AS LOG     NO-UNDO.
DEF VAR cDocDownloadList    AS CHAR    NO-UNDO.
DEF VAR oJboxEmail          AS JBoxEmailViaDefaultClient NO-UNDO.
DEF VAR cStrongOrWeakDelete AS CHARACTER NO-UNDO.

DEF TEMP-TABLE ttPicture
    FIELD iPictureNum  AS INT
    FIELD bSelected    AS LOG
    FIELD bView        AS LOG
    FIELD iDocId       AS INT
    FIELD cFileName    AS CHAR
    FIELD cTmpFileName AS CHAR
    FIELD dDate        AS DATE
    FIELD cDesc        AS CHAR
    .
DEF VAR hbttPicture AS HANDLE NO-UNDO.
hbttPicture = BUFFER ttPicture:HANDLE.    

DEF TEMP-TABLE ttPictureInView
    FIELD iPictureNum       AS INT
    FIELD hRect             AS HANDLE
    FIELD hSmallPicture     AS HANDLE
    FIELD hButton           AS HANDLE
    FIELD hMenuOpen         AS HANDLE
    FIELD hPopupMenu        AS HANDLE
    FIELD hMenuEditText     AS HANDLE
    FIELD hMenuDeleteImage  AS HANDLE
    FIELD hLargePicture     AS HANDLE
    FIELD hToggle           AS HANDLE
    .
    
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
&Scoped-Define ENABLED-OBJECTS rectTB cmbSort fiSearch 
&Scoped-Define DISPLAYED-OBJECTS cmbSort fiSearch 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CleanUp C-Win 
FUNCTION CleanUp RETURNS LOGICAL
  (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDocIdList C-Win 
FUNCTION getDocIdList RETURNS CHARACTER
  ( )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getPictureDesc C-Win 
FUNCTION getPictureDesc RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getSelectedDocIdList C-Win 
FUNCTION getSelectedDocIdList RETURNS CHARACTER
  ( )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LoadPictures C-Win 
FUNCTION LoadPictures RETURNS LOGICAL
  ( INPUT icFileNames AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setSelectCount C-Win 
FUNCTION setSelectCount RETURNS LOGICAL
  (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setStrongOrWeakDelete C-Win 
FUNCTION setStrongOrWeakDelete RETURNS LOGICAL
  ( INPUT icStrongOrWeakDelete AS CHAR) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ViewPictures C-Win 
FUNCTION ViewPictures RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE cmbSort AS CHARACTER FORMAT "X(256)":U 
     LABEL "Sortering" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "","",
                     "Stigende dato","date<",
                     "Synkende dato","date>"
     DROP-DOWN-LIST
     SIZE 18.8 BY 1 NO-UNDO.

DEFINE VARIABLE fiSearch AS CHARACTER FORMAT "X(256)":U 
     LABEL "Søk" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 TOOLTIP "Søk beskrivelse" NO-UNDO.

DEFINE RECTANGLE rectTB
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 9 BY .95
     BGCOLOR 12 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     cmbSort AT ROW 1.1 COL 73 COLON-ALIGNED WIDGET-ID 2
     fiSearch AT ROW 1.1 COL 97.4 COLON-ALIGNED
     rectTB AT ROW 1.1 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 110.8 BY 16 DROP-TARGET.

DEFINE FRAME frmImage
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.4 ROW 2.19
         SIZE 109.6 BY 14.76 DROP-TARGET WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert window title>"
         HEIGHT             = 16
         WIDTH              = 110.8
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 110.8
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 110.8
         MIN-BUTTON         = no
         MAX-BUTTON         = no
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
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win 
/* ************************* Included-Libraries *********************** */

{incl\devmode.i}
{incl\custdevmode.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* REPARENT FRAME */
ASSIGN FRAME frmImage:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 16
       FRAME DEFAULT-FRAME:WIDTH            = 110.8.

/* SETTINGS FOR FRAME frmImage
   NOT-VISIBLE                                                          */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME frmImage
/* Query rebuild information for FRAME frmImage
     _Query            is NOT OPENED
*/  /* FRAME frmImage */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* <insert window title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <insert window title> */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME DEFAULT-FRAME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DEFAULT-FRAME C-Win
ON DROP-FILE-NOTIFY OF FRAME DEFAULT-FRAME
DO:
  DEF VAR cFileNames AS CHAR   NO-UNDO.

  DO ix = 1 TO SELF:NUM-DROPPED-FILES:
    cFileNames = cFileNames + SELF:GET-DROPPED-FILE(ix) + ";".
  END.
  IF cFileNames NE "" THEN LoadPictures(TRIM(cFileNames,";")).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME frmImage
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL frmImage C-Win
ON DROP-FILE-NOTIFY OF FRAME frmImage
DO:
  DEF VAR cFileNames AS CHAR   NO-UNDO.

  DO ix = 1 TO SELF:NUM-DROPPED-FILES:
    cFileNames = cFileNames + SELF:GET-DROPPED-FILE(ix) + ";".
  END.
  IF cFileNames NE "" THEN LoadPictures(TRIM(cFileNames,";")).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL frmImage C-Win
ON MOUSE-SELECT-UP OF FRAME frmImage
DO:
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmbSort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbSort C-Win
ON VALUE-CHANGED OF cmbSort IN FRAME DEFAULT-FRAME /* Sortering */
DO:
  RUN SortPictures(cmbSort:SCREEN-VALUE).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiSearch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiSearch C-Win
ON RETURN OF fiSearch IN FRAME DEFAULT-FRAME /* Søk */
DO:
  IF fiSearch:MODIFIED THEN RUN SortPictures(cmbSort:SCREEN-VALUE).  
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
  CleanUp().  
  PUBLISH "InvalidateHandle" (THIS-PROCEDURE).
  RUN disable_UI.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

{incl\wintrigg.i}

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:


  &IF DEFINED(UIB_is_Running) NE 0 &THEN
    RUN InitializeObject.
    RUN MoveToTop.
    IF DYNAMIC-FUNCTION("getAttribute",THIS-PROCEDURE,"advGuiWin") = "YES" THEN
      RUN WaitForForm NO-ERROR.
  &ENDIF

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.


ON 'window-resized':U OF {&WINDOW-NAME} DO:
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"resize","").
  IF bMdiChild THEN DO:
    iStartRow = 1.
    FRAME frmImage:SCROLLABLE = NO.
    ViewPictures().
  END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CloseFromPictureView C-Win 
PROCEDURE CloseFromPictureView :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DeleteImageRecord C-Win 
PROCEDURE DeleteImageRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icDocId        AS CHAR   NO-UNDO.
DEF INPUT PARAM icSmallPicture AS CHAR NO-UNDO.

DEF VAR iy AS INT NO-UNDO.

cCurrSmallPicture = icSmallPicture.

FIND FIRST ttPicture WHERE ttPicture.iDocId = INT(icDocId)
     NO-ERROR.

IF AVAIL ttPicture AND DYNAMIC-FUNCTION("DoMessage",0,4,
                                        IF bScand THEN "Slett bilde?" ELSE "Delete image?"
                                       ,"","") = 6 THEN DO:
  IF NOT DYNAMIC-FUNCTION("runproc","jbdoc_delete_one_doc.p",icDocId,?) THEN
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
  ELSE DO:
    DELETE ttPicture.
    
    RUN SortPictures (cmbSort:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
  END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DeleteRecord C-Win 
PROCEDURE DeleteRecord :
/*------------------------------------------------------------------------------
 Purpose:
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
DEF VAR ix               AS INT NO-UNDO.
DEF VAR oJBoxDocRel      AS JBoxBuffer NO-UNDO. 

oJBoxDocRel = NEW JBoxBuffer("JBoxDocRel").

FOR EACH ttPicture WHERE ttPicture.bSelected:
  ASSIGN ix = ix + 1
         cDocIdList = cDocIdList + (IF cDocIdList NE "" THEN "," ELSE "" + STRING(ttPicture.iDocId)).
  IF oJBoxDocRel:Find("WHERE iJBoxDocumentId = " + STRING(ttPicture.iDocId) + " AND cContext NE '" + cContext + "'") THEN      
    cOtherRelToDocs = cOtherRelToDocs + (IF cOtherRelToDocs NE "" THEN "," ELSE "") + STRING(oJBoxDocRel:BUFFER-HANDLE::iJBoxDocRelId).

  cDocRelIdList = cDocRelIdList + (IF cDocRelIdList NE "" THEN "," ELSE "") 
                + JBoxServerAPI:Instance:FieldValues("JBoxDocRel",
                                                      "WHERE iJBoxDocumentId = " + STRING(ttPicture.iDocId) + " AND cContext = '" + cContext + "'",
                                                      "iJBoxDocRelId"). 
END.  
     
IF ix = 0 THEN DO:
  DYNAMIC-FUNCTION("DoMessage",0,0,IF bScand THEN "Ingen bilder valgt for sletting" ELSE "No images selected for deletion","","").
  RETURN.
END.       

IF cStrongOrWeakDelete = "" AND cOtherRelToDocs NE "" THEN DO:
  iReturn = DYNAMIC-FUNCTION("DoMessage",0,3,
                        (IF DYNAMIC-FUNCTION("Scandinavian") THEN
                          "Det finnes knytninger til andre begreper for bilde(r)." + CHR(10)
                        + "JA: Bilde(r) slettes sammen med alle knytninger" + CHR(10)
                        + "NEI: Bilde(r) og andre knytninger beholdes men denne knytningen (til " + cContext + ") slettes" + CHR(10)
                        + "AVBRYT: Ingen sletting"
                        ELSE
                          "There are links to other entities for document(s)." + CHR(10)
                        + "YES: Image(s) and all links to other entities are deleted" + CHR(10)
                        + "NO: Image(s) and other links are kept but this link (to " + cContext + ") is deleted" + CHR(10)
                        + "CANCEL: Nothing is deleted")
                        ,"","").
  IF iReturn = 2 THEN RETURN.
  IF iReturn = 6 THEN 
    cDeleteParam = "strong".  
  ELSE cDeleteParam = "weak".
END.
ELSE IF DYNAMIC-FUNCTION("DoMessage",0,4,
                      (IF DYNAMIC-FUNCTION("Scandinavian") THEN
                        "Slett valgte bilder?"
                      ELSE
                        "Delete seleced images?"),
                       "","") = 6 THEN
  cDeleteParam = IF cStrongOrWeakDelete NE "" THEN cStrongOrWeakDelete ELSE "strong".
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
    JBoxServerAPI:Instance:Delete("JBoxDocRel",
                                  "iJBoxDocRelId",
                                  ENTRY(ix,cDocRelIdList),
                                  "NO",
                                  "").
  END.
END.

PUBLISH "DocumentAddDelete" ("delete").


FOR EACH ttPicture
    WHERE ttPicture.bSelected
    :
  DELETE ttPicture.    
END.

RUN SortPictures (cmbSort:SCREEN-VALUE IN FRAME {&FRAME-NAME}).


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
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EditImageTextRecord C-Win 
PROCEDURE EditImageTextRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       When called from JboxLargePictureOO.w (after edit of desc) 
               the desc is passed in as icSmallPicture.
------------------------------------------------------------------------------*/
DEF INPUT PARAM icDocId  AS CHAR   NO-UNDO.
DEF INPUT PARAM icSmallPicture AS CHAR NO-UNDO.

DEF VAR ocReturn AS CHAR NO-UNDO.

IF LOOKUP(icSmallPicture,cTmpFileNames,"|") > 0 THEN DO:
  RUN JBoxDEditPictureDesc.w (icDocId,OUTPUT ocReturn).
  cCurrSmallPicture = icSmallPicture.
END.
ELSE ocReturn = icSmallPicture.  

FIND FIRST ttPicture WHERE ttPicture.iPictureNum = LOOKUP(icDocId,cFileIds) NO-ERROR.
IF AVAIL ttPicture THEN DO:
  ttPicture.cDesc = ocReturn.  

  FIND FIRST ttPictureInView
       WHERE ttPictureInView.iPictureNum = ttPicture.iPictureNum
       NO-ERROR. 
  
  IF AVAIL ttPictureInView THEN DO:
    ASSIGN ENTRY(LOOKUP(icDocId,cFileIds),cFileDescs,"|") = ocReturn
           ttPictureInView.hButton:TOOLTIP = ttPicture.cFileName + " - " + STRING(ttPicture.dDate) + CHR(10) + ocReturn.
    IF LOOKUP(icSmallPicture,cTmpFileNames,"|") > 0 AND VALID-HANDLE(ttPictureInView.hLargePicture) THEN
      DYNAMIC-FUNCTION("setPictureDesc" IN ttPictureInView.hLargePicture,ocReturn).
  END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EmailRecord C-Win 
PROCEDURE EmailRecord :
DEF VAR cToList    AS CHAR NO-UNDO.
DEF VAR cCcList    AS CHAR NO-UNDO.
DEF VAR cBccList   AS CHAR NO-UNDO.
DEF VAR cSubject   AS CHAR NO-UNDO.
DEF VAR cBody      AS CHAR NO-UNDO.

PUBLISH "getEmailContext" (OUTPUT cToList,OUTPUT cCcList,OUTPUT cBccList,OUTPUT cSubject,OUTPUT cBody).

bDownLoadToTempDir = YES.
RUN SaveToDisc.

IF VALID-OBJECT(oJboxEmail) THEN DELETE OBJECT oJboxEmail.

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
  DISPLAY cmbSort fiSearch 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE rectTB cmbSort fiSearch 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  {&OPEN-BROWSERS-IN-QUERY-frmImage}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExcelRecord C-Win 
PROCEDURE ExcelRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DYNAMIC-FUNCTION("setAttribute",hbttPicture,"excelquery","WHERE bView").
DYNAMIC-FUNCTION ("ToExcelViaFile",hbttPicture,0).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FirstRecord C-Win 
PROCEDURE FirstRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
iStartRow = 1.

ViewPictures().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HideScrollbars C-Win 
PROCEDURE HideScrollbars :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
/* purpose : hide both scrollbars */
   DEFINE INPUT PARAMETER hFrame AS HANDLE.
 
   DEFINE VARIABLE retval AS INTEGER NO-UNDO.
   RUN ScrollUpperLeft(hFrame).
   RUN ShowScrollBar IN hpApi (hFrame:HWND, 
                               {&SB_BOTH},
                               0,
                               OUTPUT retval).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeObject C-Win 
PROCEDURE InitializeObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN enable_UI.

oStatusBar = NEW JBoxStatusBar().

bScand = DYNAMIC-FUNCTION ("Scandinavian").

SUBSCRIBE TO "ShowLargePicture" ANYWHERE.

DO WITH FRAME {&FRAME-NAME}:
  ASSIGN cLinkTargets      = DYNAMIC-FUNCTION("getFieldValues","JBoxSysParam",
                                    "WHERE cSysParamName = 'DocumentLinkTarget'(codemaster)","cSysParamCharValue")
         .

  IF cLinkTargets = ? THEN cLinkTargets = "Sak;dokument".
  
  IF NOT bScand THEN
    ASSIGN cmbSort:LABEL = "Sorting"
           cmbSort:LIST-ITEM-PAIRS = ",,Date,date<,Desc.date,date>"
           fiSearch:LABEL = "Find"
           fiSearch:TOOLTIP = "Search image description"
           .
  
  hToolbar = DYNAMIC-FUNCTION("NewToolBar",
                    rectTB:HANDLE,             /* Rectangle to define coordinates for toolbar */
                    "Bilde",                          /* Corresponding menu label - no menu if blank */
                    "LoadFromDisc;"
                     + (IF bScand THEN "Last inn bilder;Last inn bilder fra disk"
                        ELSE "Load images;Load images from disk")
                     + ";LoadFromDisc;bmp/open16e.bmp"   
                  + ",Delete;" + (IF bScand THEN "Slett valgte bilder" ELSE "Delete selected images")
                  + ",Excel"
                  + ",SaveToDisc;" 
                    + (IF bScand THEN "Lagre bilder;Lagre bilder til disk"
                       ELSE "Save images;Save images to disk")
                    + ";SaveToDisc;bmp/save16e.bmp"                                                    
                  + ",OpenPictures;"
                    + (IF bScand THEN "Åpne valgte bilder;Åpne bilder med standard behandlingsprogram"
                       ELSE "Open selected images;Open selected images in default application")
                    + ";OpenPictures;bmp/folderopen.bmp"
                  + ",Link;" + (IF bScand THEN "Lag kryssreferanse;" ELSE "Create cross-ref;") + ";;bmp\links.bmp"
                  + ",Email"
                  + ",first|,prev|,next|,last|"
                  + ",Select;"  
                    + (IF bScand THEN "Marker bilder;Marker/fjern markering av alle bilder"
                       ELSE "Select;Select/deselect images")
                    ,"").  

  ASSIGN hBtnFirst = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hToolbar,"buttonFirst"))
         hBtnPrev  = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hToolbar,"buttonPrev"))
         hBtnNext  = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hToolbar,"buttonNext"))
         hBtnLast  = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hToolbar,"buttonLast"))
         .

  DYNAMIC-FUNCTION("setAttribute",hToolbar,"disabledEvents","ZoomOut").
  DYNAMIC-FUNCTION("setToolbar",hToolbar,"avail").

  DYNAMIC-FUNCTION("setResizeTypes",THIS-PROCEDURE:CURRENT-WINDOW,"frame").
  DYNAMIC-FUNCTION("setAddMoveY",THIS-PROCEDURE:CURRENT-WINDOW,FRAME frmImage:HANDLE,"btnVisFlere").  
  DYNAMIC-FUNCTION("setOrgWinSize", THIS-PROCEDURE:CURRENT-WINDOW,500,1200,0,0).  

  RUN getFormObject(OUTPUT oForm).

  bMdiChild = DYNAMIC-FUNCTION("getIsMDIchild").
  
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LastRecord C-Win 
PROCEDURE LastRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

iStartRow = iRows - iViewCount / iColumns.

IF (iStartRow - 1) * iColumns + iViewCount < NUM-ENTRIES(cTmpFileNames,"|") THEN
  iStartRow = iStartRow + 1.

ViewPictures().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LinkDocument C-Win 
PROCEDURE LinkDocument :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEF INPUT PARAM icEntity       AS CHAR NO-UNDO.
DEF INPUT PARAM icEntityIdList AS CHAR NO-UNDO.

DEF VAR cDocIdList         AS CHAR NO-UNDO.
DEF VAR cLinkedDoc         AS CHAR NO-UNDO.
DEF VAR cContact           AS CHAR NO-UNDO.
DEF VAR ix                 AS INT  NO-UNDO.
DEF VAR cCalcParam         AS CHAR NO-UNDO.


FOR EACH ttPicture WHERE ttPicture.bSelected:
  ASSIGN ix = ix + 1
         cDocIdList = cDocIdList + (IF cDocIdList NE "" THEN "," ELSE "" + STRING(ttPicture.iDocId))
         cLinkedDoc = JBoxServerAPI:Instance:FieldValues("JBoxDocLink",
                                                  "WHERE iToDocumentId = " + STRING(ttPicture.iDocId) + " AND iFromDocumentId NE " + STRING(ttPicture.iDocId),
                                                  "iFromDocumentId").
  IF cLinkedDoc NE ? THEN cDocIdList = cDocIdList + "," + cLinkedDoc.                                                  
END.  

IF ix = 0 THEN DO:
  DYNAMIC-FUNCTION("DoMessage",0,0,IF bScand THEN "Ingen bilder valgt" ELSE "No images selected","","").
  RETURN.
END.    
/*
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
*/
IF NOT DYNAMIC-FUNCTION("runProc","jbdoc_cre_docrel.p",
                 icEntity + "¤" + icEntityIdList + "¤" + cDocIdList
/*                  icEntity + "¤" + SUBSTR(icEntityIdList,INDEX(icEntityIdList,"|") + 1) + "¤" + cLastLoadedDocIds */
                ,?) THEN
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LinkRecord C-Win 
PROCEDURE LinkRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEF VAR cReturn         AS CHAR NO-UNDO.
DEF VAR hLinkTargetProc AS HANDLE NO-UNDO.

IF NOT CAN-FIND(FIRST ttPicture WHERE ttPicture.bSelected) THEN DO:
  DYNAMIC-FUNCTION("DoMessage",0,0,IF bScand THEN "Ingen bilder valgt" ELSE "No images selected","","").
  RETURN.
END.
  
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LoadFromDisc C-Win 
PROCEDURE LoadFromDisc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cFileNames AS CHAR NO-UNDO.

PUBLISH "SuspendJBoxTimer" (YES).

cFileNames = DYNAMIC-FUNCTION("SelectFileNames",
                              IF bScand THEN "Alle filer|*.*" ELSE "All files|*.*",
                              ?,
                              IF bScand THEN "Velg bildefil(er) for opplasting" ELSE "Select image files for upload").
IF cFileNames NE "" THEN LoadPictures(cFileNames).

PUBLISH "SuspendJBoxTimer" (NO).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MailRecord C-Win 
PROCEDURE MailRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DYNAMIC-FUNCTION("sendOutlookMail","brynjar@chemistry.no","","",
                 THIS-PROCEDURE:CURRENT-WINDOW:TITLE,"",
                 REPLACE(cTmpFileNames,",",";")).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MoveToTop C-Win 
PROCEDURE MoveToTop :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF DYNAMIC-FUNCTION("getAttribute",THIS-PROCEDURE,"advGuiWin") = "YES" THEN 
  RUN ShowForm("").

THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = NO.
THIS-PROCEDURE:CURRENT-WINDOW:WINDOW-STATE = 3.
THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
FRAME {&FRAME-NAME}:MOVE-TO-TOP().
FRAME frmImage:MOVE-TO-TOP().

 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NextRecord C-Win 
PROCEDURE NextRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

iStartRow = iStartRow + 1.

ViewPictures().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenImageRecord C-Win 
PROCEDURE OpenImageRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM iIx           AS INT  NO-UNDO.
DEF INPUT PARAM icSmallPicture AS CHAR NO-UNDO.

FIND FIRST ttPictureInView
     WHERE ttPictureInView.iPictureNum = iIx 
     NO-ERROR.

IF AVAIL ttPictureInView AND VALID-HANDLE(ttPictureInView.hLargePicture) THEN DO:    
  ttPictureInView.hLargePicture:CURRENT-WINDOW:WINDOW-STATE = 3.
  ttPictureInView.hLargePicture:CURRENT-WINDOW:MOVE-TO-TOP().
  RETURN.
END.

FIND FIRST ttPicture
     WHERE ttPicture.iPictureNum = iIx
     NO-ERROR.
IF NOT AVAIL ttPicture THEN RETURN.     

cCurrSmallPicture = icSmallPicture.
        
DYNAMIC-FUNCTION("ViewDocs",cContext,cKey + "|" + ttPicture.cFileName,FALSE,""). 

RUN JBoxLargePictureOO.w PERSIST SET ttPictureInView.hLargePicture.
DYNAMIC-FUNCTION("setParent" IN ttPictureInView.hLargePicture,THIS-PROCEDURE).
ttPictureInView.hLargePicture:CURRENT-WINDOW:TITLE = ttPicture.cFileName + " - " + STRING(ttPicture.dDate).

RUN InitializeObject IN ttPictureInView.hLargePicture.

DYNAMIC-FUNCTION("LoadPictureFile" IN ttPictureInView.hLargePicture,ENTRY(1,DYNAMIC-FUNCTION("getTmpDocFileNames"))). 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenPictures C-Win 
PROCEDURE OpenPictures :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DYNAMIC-FUNCTION ("setSuppressOpenDocError",YES).
FOR EACH ttPicture 
    WHERE ttPicture.bSelected:
      
  DYNAMIC-FUNCTION("ViewDocs",cContext,
                   cKey + "|" + ttPicture.cFileName  
                  ,TRUE,"").
END.                  
DYNAMIC-FUNCTION ("ViewOpenDocError",YES).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrevRecord C-Win 
PROCEDURE PrevRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
iStartRow = iStartRow - 1.
ViewPictures().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SaveToDisc C-Win 
PROCEDURE SaveToDisc :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEF VAR cDir AS CHAR NO-UNDO.

IF NOT bDownLoadToTempDir THEN DO:
  SYSTEM-DIALOG GET-DIR cDir.
  IF cDir = "" THEN RETURN.
END.
ELSE cDir = SESSION:TEMP-DIR.

cDocDownloadList = "".
 
IF cDir NE "" THEN DO:
  FOR EACH ttPicture 
      WHERE ttPicture.bSelected:

    DYNAMIC-FUNCTION("ViewDocs",cContext,
                     cKey + "|" + ttPicture.cFileName
/*                   + (IF cPictureLoadMode NE "small" THEN "|!small_" ELSE "")*/
                    ,FALSE,cDir).
    cDocDownloadList = cDocDownloadList + (IF cDocDownloadList NE "" THEN ";" ELSE "") + DYNAMIC-FUNCTION("getTmpDocFileNames").
  END.                  
  
  IF NOT bDownLoadToTempDir AND DYNAMIC-FUNCTION("DoMessage",0,4,
                    IF NOT bScand THEN
                      "Explore directory " + cDir
                    ELSE
                      "Vis filområde " + cDir
                    ,"","") = 6 THEN
    DYNAMIC-FUNCTION("ExploreDirectory",cDir).

  END.

bDownLoadToTempDir = NO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ScrollUpperLeft C-Win 
PROCEDURE ScrollUpperLeft :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
/* purpose : move both scrollbars to their 0% position */
   DEFINE INPUT PARAMETER hFrame AS HANDLE.
 
   DEFINE VARIABLE wParam AS INTEGER NO-UNDO.
   DEFINE VARIABLE nPos AS INTEGER   NO-UNDO.
   DEFINE VARIABLE RetVal AS INTEGER NO-UNDO.
   nPos = 0.                       
   wParam = (nPos * 256) + {&SB_THUMBPOSITION}.
   RUN SendMessage{&A} IN hpApi (hFrame:HWND, {&WM_HSCROLL}, wParam, 0, OUTPUT RetVal).
   RUN SendMessage{&A} IN hpApi (hFrame:HWND, {&WM_VSCROLL}, wParam, 0, OUTPUT RetVal).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SelectImage C-Win 
PROCEDURE SelectImage :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihToggle     AS HANDLE NO-UNDO.
DEF INPUT PARAM iiPictureNum AS INT NO-UNDO.

FOR FIRST ttPictureInView
    WHERE ttPictureInView.iPictureNum = iiPictureNum
   ,FIRST ttPicture 
          WHERE ttPicture.iPictureNum = iiPictureNum:
     
  ttPicture.bSelected = ttPictureInView.hToggle:CHECKED.
END.    

setSelectCount().   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SelectRecord C-Win 
PROCEDURE SelectRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEF VAR cSelectStatus AS CHAR NO-UNDO.

FOR EACH ttPictureInView:
  IF ttPictureInView.hToggle:CHECKED THEN 
    cSelectStatus = "all".
  ELSE IF cSelectStatus NE "" THEN DO:
    cSelectStatus = "some".
    LEAVE.
  END.    
END.  

FOR EACH ttPictureInView
   ,FIRST ttPicture 
          WHERE ttPicture.iPictureNum = ttPictureInView.iPictureNum
    :
  ASSIGN ttPictureInView.hToggle:CHECKED = cSelectStatus NE "all"
         ttPicture.bSelected = ttPictureInView.hToggle:CHECKED
         .           
END.  

setSelectCount().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetPictures C-Win 
PROCEDURE SetPictures :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icContext AS CHAR NO-UNDO. 
DEF INPUT PARAM icKey     AS CHAR NO-UNDO.

DEF VAR cWrongTypes AS CHAR NO-UNDO.

ASSIGN cContext       = icContext
       cKey           = icKey
       CURRENT-WINDOW = THIS-PROCEDURE:CURRENT-WINDOW.
       
EMPTY TEMP-TABLE ttPicture.       

IF DYNAMIC-FUNCTION("ViewDocs",icContext,icKey + "|small_",FALSE,"") THEN DO WITH FRAME {&FRAME-NAME}: 

  ASSIGN 
    cTmpFileNames = DYNAMIC-FUNCTION("getTmpDocFileNames")
    cOrgFileNames = DYNAMIC-FUNCTION("getOrgDocFileNames")
    cFileDates    = DYNAMIC-FUNCTION("getDocFileDates")
    cFileDescs    = DYNAMIC-FUNCTION("getDocFileDescs")
    cFileIds      = DYNAMIC-FUNCTION("getDocFileIds")
    cFileTypes    = DYNAMIC-FUNCTION("getDocFileTypes")
    .

  DO ix = 1 TO NUM-ENTRIES(cFileTypes):
    IF NOT CAN-DO("bmp,jpg,jpeg,pcx,tif,tiff",ENTRY(ix,cFileTypes)) THEN
      cWrongTypes = cWrongTypes + STRING(ix) + ",".
  END.
  cWrongTypes = TRIM(cWrongTypes,",").
  DO ix = 1 TO NUM-ENTRIES(cWrongTypes):
    ASSIGN cFileIds      = DYNAMIC-FUNCTION("DeleteListItem",cFileIds,"",ix,"")
           cTmpFileNames = DYNAMIC-FUNCTION("DeleteListItem",cTmpFileNames,"",ix,"|")
           cOrgFileNames = DYNAMIC-FUNCTION("DeleteListItem",cOrgFileNames,"",ix,"|")
           cFileDates    = DYNAMIC-FUNCTION("DeleteListItem",cFileDates,"",ix,"")
           cFileDescs    = DYNAMIC-FUNCTION("DeleteListItem",cFileDescs,"",ix,"|")
           cFileTypes    = DYNAMIC-FUNCTION("DeleteListItem",cFileTypes,"",ix,"")
           .
  END.
  
  DO ix = 1 TO NUM-ENTRIES(cFileIds):
    CREATE ttPicture.
    ASSIGN ttPicture.iPictureNum  = ix
           ttPicture.iDocId       = INT(ENTRY(ix,cFileIds))
           ttPicture.cFileName    = IF ENTRY(ix,cOrgFileNames,"|") BEGINS "small_" THEN                   
                                      SUBSTR(ENTRY(ix,cOrgFileNames,"|"),7)
                                    ELSE
                                      ENTRY(ix,cOrgFileNames,"|")
           ttPicture.cTmpFileName = ENTRY(ix,cTmpFileNames,"|")
           ttPicture.dDate        = DATE(ENTRY(ix,cFileDates))
           ttPicture.cDesc        = ENTRY(ix,cFileDescs,"|")
           ttPicture.bView        = YES
           .
  END.

  ViewPictures().

END.
ELSE CleanUp().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ShowLargePicture C-Win 
PROCEDURE ShowLargePicture :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM  icSmallPicture AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOk           AS LOG NO-UNDO.

bOk = FALSE.
FIND FIRST ttPictureInView 
     WHERE ttPictureInView.hSmallPicture = SOURCE-PROCEDURE
     NO-ERROR.
IF NOT AVAIL ttPictureInView THEN RETURN.

IF VALID-HANDLE(ttPictureInView.hLargePicture) THEN DO:   
   ttPictureInView.hLargePicture:CURRENT-WINDOW:WINDOW-STATE = 3.
   ttPictureInView.hLargePicture:CURRENT-WINDOW:MOVE-TO-TOP().
   obOk = YES.
   RETURN.
END.

FIND FIRST ttPicture WHERE ttPicture.iPictureNum = ttPictureInView.iPictureNum.

cCurrSmallPicture = icSmallPicture.

DYNAMIC-FUNCTION("ViewDocs",cContext,cKey + "|" + ttPicture.cFileName, 
/*                 (IF ENTRY(LOOKUP(icSmallPicture,cTmpFileNames,"|"),cOrgFileNames,"|") BEGINS "small_" THEN*/
/*                   SUBSTR(ENTRY(LOOKUP(icSmallPicture,cTmpFileNames,"|"),cOrgFileNames,"|"),7)             */
/*                  ELSE                                                                                     */
/*                   ENTRY(LOOKUP(icSmallPicture,cTmpFileNames,"|"),cOrgFileNames,"|")),                     */
                 FALSE,SESSION:TEMP-DIR). 

&IF DEFINED(UIB_is_Running) NE 0 &THEN
  IF NOT VALID-HANDLE(hLargePicture) THEN
    RUN JBoxLargePictureOO.w PERSIST SET ttPictureInView.hLargePicture.
&ELSE
  PUBLISH "StartChildWindow" ("JBoxLargePictureOO.w", ttPicture.cFileName + " - " + STRING(ttPicture.dDate),
/*                              (IF ENTRY(LOOKUP(cCurrSmallPicture,cTmpFileNames,"|"),cOrgFileNames,"|") BEGINS "small_" THEN*/
/*                                 SUBSTR(ENTRY(LOOKUP(cCurrSmallPicture,cTmpFileNames,"|"),cOrgFileNames,"|"),7)            */
/*                               ELSE ENTRY(LOOKUP(cCurrSmallPicture,cTmpFileNames,"|"),cOrgFileNames,"|"))                  */
/*                            + " - " + ENTRY(LOOKUP(cCurrSmallPicture,cTmpFileNames,"|"),cFileDates),                       */
                              THIS-PROCEDURE,
                              YES,
                              OUTPUT ttPictureInView.hLargePicture).  
&ENDIF

DYNAMIC-FUNCTION("LoadPictureFile" IN ttPictureInView.hLargePicture,
                  ENTRY(1,DYNAMIC-FUNCTION("getDocFileIds")),
                  cContext,
                  ENTRY(1,DYNAMIC-FUNCTION("getDocFileDescs"),"|"),
                  ENTRY(1,DYNAMIC-FUNCTION("getOrgDocFileNames"),"|")).

/*DYNAMIC-FUNCTION("LoadPictureFile" IN ttPictureInView.hLargePicture,ENTRY(1,DYNAMIC-FUNCTION("getTmpDocFileNames"),"|")).*/

obOk = TRUE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SortPictures C-Win 
PROCEDURE SortPictures :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEF INPUT PARAM icSort AS CHAR NO-UNDO.

DEF VAR ix AS INT NO-UNDO.
DEF VAR cSearch AS CHAR NO-UNDO.

cSearch = fiSearch:SCREEN-VALUE IN FRAME {&FRAME-NAME}.
IF cSearch NE "" AND INDEX(cSearch,"*") = 0 THEN cSearch = "*" + cSearch + "*".

FOR EACH ttPicture:
  ttPicture.bView = cSearch = "" OR ttPicture.cDesc MATCHES cSearch.
  IF NOT ttPicture.bView THEN ttPicture.bSelected = NO.
END.

CASE icSort:
  WHEN "date<" THEN
    FOR EACH ttPicture WHERE ttPicture.bView BY ttPicture.dDate:
      ix = ix + 1.
      ttPicture.iPictureNum = ix.
    END.
  WHEN "date>" THEN
    FOR EACH ttPicture WHERE ttPicture.bView BY ttPicture.dDate DESC:
      ix = ix + 1.
      ttPicture.iPictureNum = ix.
    END.
  OTHERWISE  
    FOR EACH ttPicture WHERE ttPicture.bView BY ttPicture.iDocId:
      ix = ix + 1.
      ttPicture.iPictureNum = ix.
    END.
END CASE.

iStartRow = 1.

ViewPictures().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ZoomInRecord C-Win 
PROCEDURE ZoomInRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
ASSIGN iHorSize = iHorSize * 1.3
       iVerSize = iVerSize * 1.3
/*       slZoom:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(INTEGER(slZoom:SCREEN-VALUE) + 1)*/
       .
ViewPictures().
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ZoomOutRecord C-Win 
PROCEDURE ZoomOutRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
ASSIGN iHorSize = iHorSize / 1.3
       iVerSize = iVerSize / 1.3
       .

ViewPictures().
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CleanUp C-Win 
FUNCTION CleanUp RETURNS LOGICAL
  (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
FOR EACH ttPictureInView:
  DELETE OBJECT ttPictureInView.hRect NO-ERROR.
  DELETE OBJECT ttPictureInView.hButton NO-ERROR.
  DELETE OBJECT ttPictureInView.hMenuOpen NO-ERROR.
  DELETE OBJECT ttPictureInView.hMenuEditText NO-ERROR.
  DELETE OBJECT ttPictureInView.hMenuDeleteImage NO-ERROR.
  DELETE OBJECT ttPictureInView.hPopupMenu NO-ERROR.
  DELETE OBJECT ttPictureInView.hToggle NO-ERROR.
  IF VALID-HANDLE(ttPictureInView.hSmallPicture) THEN
    APPLY "close" TO ttPictureInView.hSmallPicture.
  DELETE ttPictureInView NO-ERROR.
END.     
IF VALID-OBJECT(oJboxEmail) THEN DELETE OBJECT oJboxEmail.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDocIdList C-Win 
FUNCTION getDocIdList RETURNS CHARACTER
  ( ) :
/*------------------------------------------------------------------------------
  Purpose:
------------------------------------------------------------------------------*/
DEF VAR cDocIdList AS CHAR NO-UNDO.

FOR EACH ttPicture
    BY ttPicture.iDocId
    :
  cDocIdList = cDocIdList + (IF cDocIdList NE "" THEN "," ELSE "") + STRING(ttPicture.iDocId).    
END.

RETURN cDocIdList.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getPictureDesc C-Win 
FUNCTION getPictureDesc RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN ENTRY(LOOKUP(cCurrSmallPicture,cTmpFileNames,"|"),cFileDescs,"|").  

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getSelectedDocIdList C-Win 
FUNCTION getSelectedDocIdList RETURNS CHARACTER
  ( ) :
/*------------------------------------------------------------------------------
  Purpose: Fetch list of selected doc id's
------------------------------------------------------------------------------*/
DEF VAR cDocIdList AS CHAR NO-UNDO.

FOR EACH ttPicture
    WHERE ttPicture.bSelected
    BY ttPicture.iDocId
    :
  cDocIdList = cDocIdList + (IF cDocIdList NE "" THEN "," ELSE "") + STRING(ttPicture.iDocId).    
END.

RETURN cDocIdList.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LoadPictures C-Win 
FUNCTION LoadPictures RETURNS LOGICAL
  ( INPUT icFileNames AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF INDEX(icFileNames,",") > 0 THEN DO:
  DYNAMIC-FUNCTION("DoMessage",0,0,"Filnavn kan ikke inneholde komma","","").
  RETURN NO.
END.

RUN JBoxPictureScaleOO.w (INPUT-OUTPUT icFileNames,"").

DYNAMIC-FUNCTION("LoadDocs",icFileNames,
                 cContext,cKey,"").
    
RUN SetPictures (cContext,cKey).

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setSelectCount C-Win 
FUNCTION setSelectCount RETURNS LOGICAL
  (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEF VAR ix AS INT NO-UNDO.

FOR EACH ttPicture WHERE ttPicture.bSelected:
  ix = ix + 1.
END.

oStatusBar:RecordSelectText = STRING(ix).

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setStrongOrWeakDelete C-Win 
FUNCTION setStrongOrWeakDelete RETURNS LOGICAL
  ( INPUT icStrongOrWeakDelete AS CHAR):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

cStrongOrWeakDelete = icStrongOrWeakDelete.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ViewPictures C-Win 
FUNCTION ViewPictures RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR iy         AS INT   NO-UNDO.
DEF VAR iRow       AS INT   NO-UNDO.
DEF VAR iCount     AS INT   NO-UNDO.
DEF VAR iStartView AS INT   NO-UNDO.

DYNAMIC-FUNCTION("DoLockWindow",THIS-PROCEDURE:CURRENT-WINDOW).
DYNAMIC-FUNCTION("setAttribute",SESSION,"userkeepswindowlocked","yes").

DO WITH FRAME {&FRAME-NAME}:
  CURRENT-WINDOW = THIS-PROCEDURE:CURRENT-WINDOW.
  SESSION:SET-WAIT-STATE("general").

  DYNAMIC-FUNCTION("setToolbar",hToolbar,"avail").

  CleanUp().  
    
  IF bMdiChild THEN
    ASSIGN iColumns = TRUNC(THIS-PROCEDURE:CURRENT-WINDOW:WIDTH-PIXELS / (iHorSize + 5),0)
           iRows    = MAX(NUM-ENTRIES(cTmpFileNames,"|") / iColumns + (IF NUM-ENTRIES(cTmpFileNames,"|") / iColumns > 1 AND 
                                                                   NUM-ENTRIES(cTmpFileNames,"|") MOD iColumns < 5 THEN 1 ELSE 0),1)
           .
  ELSE DO:          
    ASSIGN iColumns = MIN(NUM-ENTRIES(cTmpFileNames,"|"),(SESSION:WIDTH-PIXELS - 100) / (iHorSize + 5))
           iRows    = NUM-ENTRIES(cTmpFileNames,"|") / iColumns + (IF NUM-ENTRIES(cTmpFileNames,"|") / iColumns > 1 AND 
                                                                   NUM-ENTRIES(cTmpFileNames,"|") MOD iColumns < 5 THEN 1 ELSE 0)
           .
    
    IF VALID-OBJECT(oForm) THEN
      oForm:Size = NEW System.Drawing.Size(iColumns * (iHorSize + 5) + 20 + 17,
                                           MIN(SESSION:HEIGHT-PIXELS - 70,iRows * (iVerSize + 17)) + 22 + 36 + oForm:iDeltaYWinMenu).

    ASSIGN THIS-PROCEDURE:CURRENT-WINDOW:WIDTH-PIXELS  = iColumns * (iHorSize + 5) + 20
           THIS-PROCEDURE:CURRENT-WINDOW:HEIGHT-PIXELS = MIN(SESSION:HEIGHT-PIXELS - 70,iRows * (iVerSize + 17)) + 22
           .

    APPLY "window-resized" TO {&WINDOW-NAME}.
  END.
    
  ASSIGN hBtnPrev:SENSITIVE  = iStartRow > 1
         hBtnFirst:SENSITIVE = hBtnPrev:SENSITIVE
         iViewCount = 0
         .

  DO iy = 1 TO iRows:
    iRow = iy - iStartRow + 1.
      
    IF bMdiChild THEN DO:
      IF iRow + (iRow - 1) * (iVerSize + 22) + iVerSize GT FRAME frmImage:HEIGHT-PIXELS THEN
        LEAVE.
    END.
    ELSE DO:      
      IF iRow + (iRow - 1) * (iVerSize + 5) + iVerSize GT FRAME frmImage:HEIGHT-PIXELS THEN
        LEAVE.
    END.    
    DO ix = 1 TO iColumns:
      iCount = iCount + 1.
      FIND FIRST ttPicture 
           WHERE ttPicture.iPictureNum = iCount
             AND ttPicture.bView
           NO-ERROR.
       
      IF iy GE iStartRow AND AVAIL ttPicture THEN DO:
        iViewCount = iViewCount + 1.
        IF iStartView = 0 THEN iStartView = iCount.
        CREATE ttPictureInView.
        ttPictureInView.iPictureNum = iCount.
        
        CREATE RECTANGLE ttPictureInView.hRect
        ASSIGN FRAME            = FRAME frmImage:HANDLE
               NAME             = "rect-" + STRING(iRows) + "-" + STRING(iColumns)
               X                = ix + (ix - 1) * (iHorSize + 5)
               Y                = iRow + (iRow - 1) * (iVerSize + 15) /* + (IF iy > 1 THEN 22 ELSE 0) */ 
               WIDTH-PIXELS     = iHorSize
               HEIGHT-PIXELS    = iVerSize
               VISIBLE          = TRUE
               TOOLTIP          = ENTRY(iCount,cOrgFileNames,"|")             
               .
  
        RUN JBoxSmallPictureOO.w PERSIST SET ttPictureInView.hSmallPicture.
        RUN InitializeObject IN ttPictureInView.hSmallPicture (ttPictureInView.hRect).
        DYNAMIC-FUNCTION("LoadPictureFile" IN ttPictureInView.hSmallPicture,ttPicture.cTmpFileName).
        DYNAMIC-FUNCTION("SetPictureDesc"  IN ttPictureInView.hSmallPicture,ttPicture.cDesc).

        CREATE TOGGLE-BOX ttPictureInView.hToggle
        ASSIGN FRAME            = FRAME frmImage:HANDLE
               X                = ix + (ix - 1) * (iHorSize + 5)
               Y                = iRow * iVerSize + iRow + (iRow - 1) * 15 - 1 /* + (IF iy > 1 THEN 22 ELSE 0) */
               VISIBLE          = TRUE
               SENSITIVE        = TRUE
               TOOLTIP          = IF bScand THEN "Markér" ELSE "Select"
               CHECKED          = ttPicture.bSelected
         TRIGGERS:
           ON VALUE-CHANGED PERSISTENT RUN SelectImage IN THIS-PROCEDURE (SELF,iCount).
         END TRIGGERS.
                
        CREATE BUTTON ttPictureInView.hButton
        ASSIGN FRAME            = FRAME frmImage:HANDLE
               X                = ix + (ix - 1) * (iHorSize + 5) + 13
               Y                = iRow * iVerSize + iRow + (iRow - 1) * 15 /* + (IF iy > 1 THEN 22 ELSE 0) */
               FONT             = 3
               WIDTH-PIXELS     = iHorSize - 12
               HEIGHT-PIXELS    = 14
               VISIBLE          = TRUE
               SENSITIVE        = TRUE
               TOOLTIP          = ttPicture.cFileName + " " + STRING(ttPicture.dDate) + CHR(10) + ttPicture.cDesc
               LABEL            = STRING(iCount) + " " + ttPicture.cFileName
         TRIGGERS:
           ON CHOOSE PERSISTENT RUN EditImageTextRecord IN THIS-PROCEDURE (ENTRY(iCount,cFileIds),ENTRY(iCount,cTmpFileNames,"|")).
         END TRIGGERS.
             
  
         CREATE MENU ttPictureInView.hPopupMenu
                ASSIGN POPUP-ONLY = TRUE.
  
         ttPictureInView.hButton:POPUP-MENU = ttPictureInView.hPopupMenu.
  
         CREATE MENU-ITEM ttPictureInView.hMenuOpen
               ASSIGN PARENT     = ttPictureInView.hPopupMenu
                      LABEL      = IF bScand THEN "Åpne bilde" ELSE "Open image"
                      NAME       = "OpenImage" 
                      TRIGGERS:
                        ON CHOOSE PERSISTENT RUN OpenImageRecord IN THIS-PROCEDURE (iCount,ttPicture.cTmpFileName).
                      END TRIGGERS.
  
         CREATE MENU-ITEM ttPictureInView.hMenuEditText
               ASSIGN PARENT     = ttPictureInView.hPopupMenu
                      LABEL      = IF bScand THEN "Rediger bildetekst" ELSE "Edit description"
                      NAME       = "EditImageText" 
                      TRIGGERS:
                        ON CHOOSE PERSISTENT RUN EditImageTextRecord IN THIS-PROCEDURE (STRING(ttPicture.iDocId),ttPicture.cTmpFileName).
                      END TRIGGERS.
  
         CREATE MENU-ITEM ttPictureInView.hMenuDeleteImage
               ASSIGN PARENT     = ttPictureInView.hPopupMenu
                      LABEL      = IF bScand THEN "Slett bilde" ELSE "Delete image"
                      NAME       = "DeleteImage" 
                      TRIGGERS:
                        ON CHOOSE PERSISTENT RUN DeleteImageRecord IN THIS-PROCEDURE (STRING(ttPicture.iDocId),ttPicture.cTmpFileName).
                      END TRIGGERS.
      END.
      IF NOT AVAIL ttPicture THEN LEAVE.  
    END.
    IF NOT AVAIL ttPicture THEN LEAVE.  
  END.

  ix = 0.
  iy = 0.
  FOR EACH ttPicture:
    ix = ix + 1.
    IF ttPicture.bView THEN iy = iy + 1.
  END.

  oStatusBar:RecordCountText = STRING(ix).
  oStatusBar:HelpText = (IF bScand THEN "Viser bilde " ELSE "Pictures ") + STRING(iStartView) + " - " + STRING(iStartView + iViewCount - 1) + " / " + STRING(iy).

  setSelectCount().

  SESSION:SET-WAIT-STATE("").
  THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = FALSE.
  THIS-PROCEDURE:CURRENT-WINDOW:WINDOW-STATE = 3.
  THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
  FRAME frmImage:MOVE-TO-TOP().
  
  ASSIGN hBtnNext:SENSITIVE = iStartView + iViewCount - 1 NE iy /* iCount NE ix */
         hBtnLast:SENSITIVE = hBtnNext:SENSITIVE.
  
END.
  
FRAME {&FRAME-NAME}:MOVE-TO-TOP().
FRAME frmImage:MOVE-TO-TOP().

DYNAMIC-FUNCTION("setAttribute",SESSION,"userkeepswindowlocked","").
DYNAMIC-FUNCTION("DoLockWindow",?).

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

