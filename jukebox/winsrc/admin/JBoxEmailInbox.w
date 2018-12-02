&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description:        Container for a JukeBox window program

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author:            brynjar@chemistry.no

  Created:           18.oct.2006

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
&SCOPED-DEFINE PureABLWin 0
/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEF VAR bOk              AS LOG    NO-UNDO.
DEF VAR ix               AS INT    NO-UNDO.
DEF VAR hBrowse          AS HANDLE NO-UNDO.
DEF VAR hQuery           AS HANDLE NO-UNDO.
DEF VAR hToolbar         AS HANDLE NO-UNDO.
DEF VAR hFieldMap        AS HANDLE NO-UNDO.
DEF VAR httEmailBuffer   AS HANDLE NO-UNDO.
DEF VAR hBrwAttach       AS HANDLE NO-UNDO.
DEF VAR hBuffAttach      AS HANDLE NO-UNDO.
DEF VAR hIe              AS HANDLE NO-UNDO.
DEF VAR cCurrAccountId   AS CHAR   NO-UNDO.
DEF VAR hHelpText        AS HANDLE NO-UNDO.
DEF VAR bScand           AS LOG    NO-UNDO.
DEF VAR hLinkEmail       AS HANDLE NO-UNDO.
DEF VAR bExternalCaller  AS LOG    NO-UNDO.
DEF VAR cLinkTargets     AS CHAR   NO-UNDO.
DEF VAR cToList          AS CHAR   NO-UNDO.
DEF VAR cFromList        AS CHAR   NO-UNDO.
DEF VAR bEmailLink       AS LOG    NO-UNDO.

DEF VAR cLinkLabelList   AS CHAR   NO-UNDO.
DEF VAR cLinkContextList AS CHAR   NO-UNDO.
DEF VAR cLinkIdList      AS CHAR   NO-UNDO.

DEF VAR hSubject         AS HANDLE NO-UNDO.

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
&Scoped-Define ENABLED-OBJECTS btnSplitBarY brwEmail tbEmail brwAttachments ~
rectIE cmbAccount cmbView dDateSent cTimeSent cSubject cDisplayFrom ~
cDisplayTo 
&Scoped-Define DISPLAYED-OBJECTS cmbAccount cmbView dDateSent cTimeSent ~
cSubject cDisplayFrom cDisplayTo 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AdjustBrowseColumns C-Win 
FUNCTION AdjustBrowseColumns RETURNS LOGICAL
  ( INPUT ihBrowse     AS HANDLE,
    INPUT icBrowseName AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LocalTranslation C-Win 
FUNCTION LocalTranslation RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setDocRefLookupMenu C-Win 
FUNCTION setDocRefLookupMenu RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setExternalCaller C-Win 
FUNCTION setExternalCaller RETURNS LOGICAL
  ( INPUT ibExternalCaller AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnSplitBarY 
     IMAGE-UP FILE "bmp/tabdown.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Button 2" 
     SIZE 154.6 BY .29.

DEFINE VARIABLE cmbAccount AS CHARACTER FORMAT "X(256)":U 
     LABEL "Konto" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 65 BY 1 NO-UNDO.

DEFINE VARIABLE cmbView AS CHARACTER FORMAT "X(256)":U 
     LABEL "Combo 1" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 31.2 BY 1 NO-UNDO.

DEFINE VARIABLE cDisplayFrom AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fra" 
     VIEW-AS FILL-IN 
     SIZE 80 BY 1 NO-UNDO.

DEFINE VARIABLE cDisplayTo AS CHARACTER FORMAT "X(256)":U 
     LABEL "Til" 
     VIEW-AS FILL-IN 
     SIZE 80 BY 1 NO-UNDO.

DEFINE VARIABLE cSubject AS CHARACTER FORMAT "X(256)":U 
     LABEL "Emne" 
     VIEW-AS FILL-IN 
     SIZE 80 BY 1 NO-UNDO.

DEFINE VARIABLE cTimeSent AS CHARACTER FORMAT "X(5)":U 
     VIEW-AS FILL-IN 
     SIZE 10.2 BY 1 NO-UNDO.

DEFINE VARIABLE dDateSent AS DATE FORMAT "99/99/9999":U 
     LABEL "Dato" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE RECTANGLE brwAttachments
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 59 BY 4.57.

DEFINE RECTANGLE brwEmail
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 154 BY 10.95.

DEFINE RECTANGLE rectIE
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 154 BY 9.33.

DEFINE RECTANGLE tbEmail
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 9.8 BY .91.

DEFINE BUTTON btnSplitBarX 
     IMAGE-UP FILE "bmp/tableft_small.bmp":U NO-FOCUS FLAT-BUTTON NO-CONVERT-3D-COLORS
     LABEL "" 
     SIZE .8 BY 4.52.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnSplitBarY AT ROW 13.48 COL 1.4
     cmbAccount AT ROW 1.24 COL 10 COLON-ALIGNED
     cmbView AT ROW 1.24 COL 122.4 COLON-ALIGNED
     dDateSent AT ROW 14.1 COL 13 COLON-ALIGNED
     cTimeSent AT ROW 14.1 COL 31.4 COLON-ALIGNED NO-LABEL
     cSubject AT ROW 15.14 COL 13 COLON-ALIGNED
     cDisplayFrom AT ROW 16.19 COL 13 COLON-ALIGNED
     cDisplayTo AT ROW 17.24 COL 13 COLON-ALIGNED
     brwEmail AT ROW 2.52 COL 2
     tbEmail AT ROW 1.29 COL 77.8
     brwAttachments AT ROW 14.05 COL 97
     rectIE AT ROW 18.86 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 155.6 BY 27.29.

DEFINE FRAME frmSplitBarX
     btnSplitBarX AT ROW 1 COL 33.2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 62.6 ROW 14.1
         SIZE 63.2 BY 4.52.


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
         TITLE              = "Arkiver epost"
         HEIGHT             = 27.29
         WIDTH              = 155.8
         MAX-HEIGHT         = 55.91
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 55.91
         VIRTUAL-WIDTH      = 320
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

{incl/devmode.i}
{incl/custdevmode.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* REPARENT FRAME */
ASSIGN FRAME frmSplitBarX:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
ASSIGN 
       btnSplitBarY:MOVABLE IN FRAME DEFAULT-FRAME          = TRUE.

/* SETTINGS FOR FRAME frmSplitBarX
                                                                        */
ASSIGN 
       btnSplitBarX:MOVABLE IN FRAME frmSplitBarX          = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Arkiver epost */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Arkiver epost */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frmSplitBarX
&Scoped-define SELF-NAME btnSplitBarX
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSplitBarX C-Win
ON END-MOVE OF btnSplitBarX IN FRAME frmSplitBarX
DO:
  DYNAMIC-FUNCTION("SetSplitBarX",THIS-PROCEDURE:CURRENT-WINDOW,btnSplitBarX:HANDLE IN FRAME frmSplitBarX,NO).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME btnSplitBarY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSplitBarY C-Win
ON END-MOVE OF btnSplitBarY IN FRAME DEFAULT-FRAME /* Button 2 */
DO:
  DYNAMIC-FUNCTION("SetSplitBarY",THIS-PROCEDURE:CURRENT-WINDOW,btnSplitBarY:HANDLE:HANDLE IN FRAME {&FRAME-NAME},NO).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmbAccount
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbAccount C-Win
ON VALUE-CHANGED OF cmbAccount IN FRAME DEFAULT-FRAME /* Konto */
DO:
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"baseQuery",
                   "WHERE iJBoxEmailAccountId = " + (IF cmbAccount:SCREEN-VALUE NE ? THEN cmbAccount:SCREEN-VALUE ELSE "0")).

  RUN InvokeMethod(hBrowse,"OpenQuery").
  IF hBrowse:QUERY:NUM-RESULTS > 0 THEN DO:
    APPLY "entry" TO hBrowse.
    hBrowse:SELECT-FOCUSED-ROW().
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmbView
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbView C-Win
ON VALUE-CHANGED OF cmbView IN FRAME DEFAULT-FRAME /* Combo 1 */
DO:
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcParamcOtherLinks",SELF:SCREEN-VALUE).
  RUN InvokeMethod(hBrowse,"OpenQuery").
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
  IF NOT DYNAMIC-FUNCTION("DoCleanUpObjects",THIS-PROCEDURE:CURRENT-WINDOW) THEN RETURN NO-APPLY.
  DYNAMIC-FUNCTION("SetCleanUpResize",THIS-PROCEDURE:CURRENT-WINDOW).
  PUBLISH "InvalidateHandle" (THIS-PROCEDURE).
  IF VALID-HANDLE(hIe) THEN APPLY "close" TO hIe.
  FOR EACH ttLinkTargets:
    IF VALID-HANDLE(ttLinkTargets.hLinkTarget) THEN APPLY "close" TO ttLinkTargets.hLinkTarget.
  END.
  RUN disable_UI.
END.

{incl/wintrigg.i}

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  &IF DEFINED(UIB_is_Running) NE 0 &THEN
    RUN InitializeObject.
    IF VALID-HANDLE(THIS-PROCEDURE:CURRENT-WINDOW) THEN
      RUN MoveToTop.
    ELSE MESSAGE "Press ctrl-break"
                 VIEW-AS ALERT-BOX.
  &ENDIF

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

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
IF DYNAMIC-FUNCTION("getCurrentObject") = hBrwAttach THEN 
  DYNAMIC-FUNCTION("ViewDocs",
                    "JBoxDocument",
                    STRING(hBuffAttach:BUFFER-FIELD("iFromDocumentId"):BUFFER-VALUE),
                    YES,
                    "").
ELSE DO:
  RUN SUPER.
  hSubject:READ-ONLY = NO.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DeleteAttachment C-Win 
PROCEDURE DeleteAttachment :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cAttachList AS CHAR NO-UNDO.

DO ix = 1 TO hBrwAttach:NUM-SELECTED-ROWS:
  IF hBrwAttach:FETCH-SELECTED-ROW(ix) THEN
    cAttachList = cAttachList + (IF cAttachList NE "" THEN "," ELSE "") + STRING(hBrwAttach:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("iFromDocumentId"):BUFFER-VALUE).
END.
IF cAttachList NE "" THEN DO:
  IF DYNAMIC-FUNCTION("DoMessage",0,4,IF bScand THEN "Slett valgt(e) vedlegg?" ELSE "Delete selected attachment(s)","","") = 6 THEN DO:
    DO ix = 1 TO NUM-ENTRIES(cAttachList):
      IF NOT DYNAMIC-FUNCTION("runProc","jbdoc_delete_one_doc.p",ENTRY(ix,cAttachList),?) THEN DO:
        DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
        LEAVE.
      END.
    END.
    RUN InvokeMethod(hBrowse,"DisplayRecord").
  END.
END.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplayRecord C-Win 
PROCEDURE DisplayRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

RUN SUPER.

IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowse 
   AND hFieldMap:AVAIL THEN DO:
       
  cSubject:SCREEN-VALUE IN FRAME {&FRAME-NAME} = hFieldMap:BUFFER-FIELD("cSubject"):BUFFER-VALUE.

  IF VALID-HANDLE(hIe) THEN DO:
    IF DYNAMIC-FUNCTION("ViewDocs",
                        "JBoxDocument",
                        STRING(hFieldMap:BUFFER-FIELD("iDocRelMasterDocId"):BUFFER-VALUE),
                        NO,
                        "") THEN 
      DYNAMIC-FUNCTION("NavigateToURL" IN hIe,
                       DYNAMIC-FUNCTION("getTmpDocFileNames")).
  
    setDocRefLookupMenu().
  END.
END.

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
APPLY "default-action" TO hBrowse.
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
  DISPLAY cmbAccount cmbView dDateSent cTimeSent cSubject cDisplayFrom 
          cDisplayTo 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE btnSplitBarY brwEmail tbEmail brwAttachments rectIE cmbAccount cmbView 
         dDateSent cTimeSent cSubject cDisplayFrom cDisplayTo 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  ENABLE btnSplitBarX 
      WITH FRAME frmSplitBarX IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-frmSplitBarX}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeObject C-Win 
PROCEDURE InitializeObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       Refer to the <jukebox>\winsrc\samples for working examples for Sports2000
------------------------------------------------------------------------------*/
RUN enable_UI.

DYNAMIC-FUNCTION("CreateStatusBar",THIS-PROCEDURE:CURRENT-WINDOW,"",0,YES,?).

hHelpText = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",THIS-PROCEDURE:CURRENT-WINDOW,"HelpTextWidget")).

DO WITH FRAME {&FRAME-NAME}:

  ASSIGN cmbAccount:DELIMITER  = "|"
         cmbAccount:LIST-ITEM-PAIRS = RIGHT-TRIM("|0|" + DYNAMIC-FUNCTION("getFieldList",
                                                       "JBoxEmailAccount;cAccountName|cEmailAddress;iJBoxEmailAccountId",
                                                       "where bActive AND cUsage NE 'out' and iJBoxCompanyId = " + DYNAMIC-FUNCTION("getCompany")
                                                         ),"|")
         cmbAccount:SCREEN-VALUE = "0"
         bScand       = DYNAMIC-FUNCTION("Scandinavian")
         cLinkTargets = DYNAMIC-FUNCTION("getFieldValues","JBoxSysParam",
                                         "WHERE cSysParamName = 'DocumentLinkTarget'(codemaster)","cSysParamCharValue")
         cmbView:LIST-ITEM-PAIRS = (IF bScand THEN
                                      "Alle,,Arkivert,skipWhenNoOtherRel,Ikke arkivert,skipWhenOtherRel"
                                    ELSE
                                      "All,,Archived,skipWhenNoOtherRel,Not archived,skipWhenOtherRel"
                                    )
         cmbView:LABEL = (IF bScand THEN "Vis" ELSE "View")
         cmbView:SCREEN-VALUE = "skipWhenOtherRel"
         .
  
  hBrowse = DYNAMIC-FUNCTION("NewBrowse"
      ,brwEmail:HANDLE
      ,10000
      ,"multiple"
      ,"JBoxEmailInbox"
      + ";dDateSent|" + (IF bScand THEN "Dato" ELSE "Date")
      + ";+cTimeSent|CHARACTER|x(5)|jb_hhmm(iTimeSent)|" + (IF bScand THEN "Tid" ELSE "Time")
      + ";cSubject|" + (IF bScand THEN "Emne" ELSE "Subject")
      + ";cDisplayFrom|" + (IF bScand THEN "Fra" ELSE "From")
      + ";cDisplayTo|" + (IF bScand THEN "Til" ELSE "To")
      + ";cDisplayCc|" + (IF bScand THEN "Kopi" ELSE "CC")
      + ";cDisplayBCc|" + (IF bScand THEN "Blind kopi" ELSE "BCC")
      + ";!cFrom|" + (IF bScand THEN "Fra" ELSE "From")
      + ";!cTo|" + (IF bScand THEN "Til" ELSE "To")
      + ";!cCc|" + (IF bScand THEN "Kopi" ELSE "CC")
      + ";!iJBoxEmailInboxId"
    + ",JBoxDocRel"
      + ";!+iDocRelMasterDocId|INTEGER|>>>>>>>9|find_masterdoc_id(iJBoxDocumentId)|Masterdoc"
      + ";+cOtherLinks|CHARACTER|x(50)|other_docrel(ROWID)|" + (IF bScand THEN "Dokumentknytninger" ELSE "Document links")                             
      + ";+OtherLinkKeys|CHARACTER|x(30)|other_docrel_keys"
      ,"WHERE false"
      + ",EACH JBoxDocRel NO-LOCK WHERE cContext = 'JBoxEmailInbox' AND cEntityId = STRING(JBoxEmailInbox.iJBoxEmailInboxId)" 
      ,"").

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"shadedRows","yes").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcFieldProc","jbdoc_browsecalc.p").
  DYNAMIC-FUNCTION("setSortString",hBrowse,"dDateSent;desc,cTimeSent;desc").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcParamcOtherLinks","skipWhenOtherRel").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"windowsBrowse","yes").

  DYNAMIC-FUNCTION("NewMenuBand",hBrowse
                  ,"MultiSortBrowse;Sorter på flere kolonner"
                  ,"").

  hSubject  = DYNAMIC-FUNCTION("NewBrowseFillIn",hBrowse,"cSubject","cSubject"
              ,"","","","").
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hSubject,"cSubject").

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"enableOnDblClick","yes").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"setReadOnlyOnReturn","yes").

  hBrowse:TOOLTIP = (IF bScand THEN "Dobbeltklikk for å redigere emne" ELSE "Doubleclick to edit subject").

  hFieldMap = DYNAMIC-FUNCTION("NewFieldMap"
      ,hBrowse:QUERY
      ,FRAME {&FRAME-NAME}:HANDLE
      ,"",""
      ,"dDateSent,cTimeSent,cDisplayFrom,cDisplayTo,cSubject",""
      ,"").

  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hFieldMap).
  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"customDeleteValProc","=jbdoc_del_email_inbox.p").

  hToolbar = DYNAMIC-FUNCTION("NewToolBar"
      ,tbEmail:HANDLE
      ,IF bScand THEN "Fil" ELSE "File"
      ,"Refresh;" + (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Oppdater (F5)" ELSE "Refresh (F5)")
     + ",Edit;" + (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Rediger emne" ELSE "Edit subject")
/*      + ",Undo"                                                                                  */
/*      + ",Save"                                                                                  */
     + ",Delete;" + (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Slett" ELSE "Delete")
     + ",Filter"
     + ",excel;" + (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Eksporter til E&xcel" ELSE "Export to Excel")
     + ",browseconfig;" + (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Kolonneoppsett" ELSE "Column setup")
     + (IF NOT bExternalCaller THEN 
          ",Link;" + (IF bScand THEN "Arkiverctrl-a;Arkiver" ELSE "Archive emailctrl-a;Archive email") 
                   + ";;bmp\links.bmp"
        ELSE "")
      ,"maxborder").

  DYNAMIC-FUNCTION("CreateObjectLink",hToolbar,hBrowse).
/*   DYNAMIC-FUNCTION("CreateObjectLink",hToolbar,hFieldMap).  */

  hBrwAttach = DYNAMIC-FUNCTION("NewBrowse"
      ,brwAttachments:HANDLE
      ,100
      ,"multiple"
      ,"JBoxDocLink"
       + ";+cFileName|CHARACTER|x(50)|doclink_filename(ROWIDskipmaster)|" + (IF bScand THEN "Vedlegg" ELSE "Attachments")                             
       + ";!iToDocumentId"
       + ";!iFromDocumentId"
      ,"WHERE false"
      ,"").

  DYNAMIC-FUNCTION("setAttribute",hBrwAttach,"viewRecordCount","no").
  DYNAMIC-FUNCTION("setAttribute",hBrwAttach,"calcFieldProc","jbdoc_browsecalc.p").

  hBuffAttach = hBrwAttach:QUERY:GET-BUFFER-HANDLE(1).

  DYNAMIC-FUNCTION("CreateParentLink",hBrwAttach,hBrowse,"iToDocumentId;iDocRelMasterDocId").

  DYNAMIC-FUNCTION("NewMenuBand",hBrwAttach
     ,"OpenAttachments;" + (IF bScand THEN "Åpne vedlegg" ELSE "Open attachment")
   + ",Delete;" + (IF bScand THEN "Slett vedlegg" ELSE "Delete attachment") + ";DeleteAttachment"
     ,"").

  DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,
                   "brwEmail," + hBrowse:NAME).
  DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,
                   "brwEmail,brwAttachments,frmSplitBarX," + hBrowse:NAME + "," + hBrwAttach:NAME).
  DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME frmSplitBarX:HANDLE,
                   "frmSplitBarX").
  DYNAMIC-FUNCTION("setNoMoveY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME frmSplitBarX:HANDLE,
                   "btnSplitBarX").

  DYNAMIC-FUNCTION("setAddResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,
                   "cDisplayFrom,cSubject,cDisplayTo").
  DYNAMIC-FUNCTION("setNoMoveX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,
                   "cDisplayFrom,cSubject,cDisplayTo").
  DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,
                   "cDisplayFrom,cSubject,cDisplayTo").
  
  DYNAMIC-FUNCTION("SetSplitBarX",THIS-PROCEDURE:CURRENT-WINDOW,btnSplitBarX:HANDLE IN FRAME frmSplitBarX,NO).
  DYNAMIC-FUNCTION("setFollowSplitBarX",THIS-PROCEDURE:CURRENT-WINDOW,btnSplitBarX:HANDLE IN FRAME frmSplitBarX,
                   STRING(cDisplayFrom:HANDLE) + "," +
                   STRING(cDisplayTo:HANDLE) + "," +
                   STRING(cSubject:HANDLE) + "," +
                   STRING(brwAttachments:HANDLE) + "," + 
                   STRING(hBrwAttach)
                   ).
  
  IF SEARCH("controls.dll") NE ? THEN DO:
    RUN JBoxJlwSupIe.w PERSISTENT SET hIe.
    DYNAMIC-FUNCTION("setNavigateAction" IN hIe,"setWebDoc").
  END.
  ELSE
    RUN JBoxSupIe.w PERSISTENT SET hIe.
  RUN InitializeObject IN hIe (rectIE:HANDLE,"").
  rectIE:WIDTH-PIXELS = rectIE:WIDTH-PIXELS - 2.
  SUBSCRIBE TO "InvalidateHandle" IN hIe.

  DYNAMIC-FUNCTION("SetSplitBarY",THIS-PROCEDURE:CURRENT-WINDOW,btnSplitBarY:HANDLE:HANDLE IN FRAME {&FRAME-NAME},NO).
  DYNAMIC-FUNCTION("setFollowSplitBarY",THIS-PROCEDURE:CURRENT-WINDOW,btnSplitBarY:HANDLE IN FRAME {&FRAME-NAME},
                   STRING(BrwEmail:HANDLE) + "," +
                   STRING(hBrowse) + "," +
                   STRING(brwAttachments:HANDLE) + "," +
                   STRING(hBrwAttach) + "," +
                   STRING(FRAME frmSplitBarX:HANDLE) + "," +
                   STRING(cDisplayFrom:HANDLE) + "," +
                   STRING(cDisplayTo:HANDLE) + "," +
                   STRING(dDateSent:HANDLE) + "," +
                   STRING(cTimeSent:HANDLE) + "," +
                   STRING(cSubject:HANDLE) + "," +
                   STRING(rectIe:HANDLE) + "," + 
                   DYNAMIC-FUNCTION("getFrameHandleList" IN hIe)
                   ).
  DYNAMIC-FUNCTION("setSplitBarYlimits",btnSplitBarY:HANDLE IN FRAME {&FRAME-NAME},150,150).
  
  DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,550,200,0,0).

  LocalTranslation().

  DYNAMIC-FUNCTION("InitTranslation",THIS-PROCEDURE:CURRENT-WINDOW).

  IF cmbAccount:NUM-ITEMS = 2 AND cmbAccount:SCREEN-VALUE = ? THEN DO:
    cmbAccount:SCREEN-VALUE = cmbAccount:ENTRY(2).
    APPLY "value-changed" TO cmbAccount.
  END.
  ELSE
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"baseQuery","WHERE iJBoxEmailAccountId = 0").
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InvalidateHandle C-Win 
PROCEDURE InvalidateHandle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihChild AS HANDLE NO-UNDO.
IF ihChild = hIe THEN APPLY "close" TO THIS-PROCEDURE.

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
                               "WHERE iToDocumentId = " + STRING(hFieldMap:BUFFER-FIELD("iDocRelMasterDocId"):BUFFER-VALUE)).
    IF cReturn = "" THEN
      cReturn = STRING(hFieldMap:BUFFER-FIELD("iDocRelMasterDocId"):BUFFER-VALUE).
  
    ASSIGN cLastLoadedDocIds = cLastLoadedDocIds + (IF cLastLoadedDocIds NE "" THEN "," ELSE "") + REPLACE(cReturn,"|",",")
           cRowIdList        = cRowIdList + (IF cRowIdList NE "" THEN "," ELSE "") + hFieldMap:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE           
           cEmailIdList      = cEmailIdList + (IF cEmailIdList NE "" THEN "," ELSE "") + STRING(hFieldMap:BUFFER-FIELD("iJBoxEmailInboxId"):BUFFER-VALUE)
           .
    IF NOT CAN-DO(cFromList,hFieldMap:BUFFER-FIELD("cFrom"):BUFFER-VALUE) THEN
      cFromList = cFromList + (IF cFromList NE "" THEN "," ELSE "") + hFieldMap:BUFFER-FIELD("cFrom"):BUFFER-VALUE.
  END.
END.
IF cLastLoadedDocIds = "" THEN DO:
  DYNAMIC-FUNCTION("DoMessage",512,0,"Ingen meldinger er valgt for arkivering","","").
  RETURN.
END.

IF icEntity = "Dokument" AND NUM-ENTRIES(icEntityIdList,"|") = 1 THEN DO:    
  IF NUM-ENTRIES(cFromList) > 1 AND
     DYNAMIC-FUNCTION("DoMessage",511,1,"Du har valgt meldinger fra flere avsendere for arkivering under samme journalpost. Er du sikker på at du vil gjøre dette?",
                      "Flere avsendere","") = 2 THEN RETURN.

  IF DYNAMIC-FUNCTION("getFieldValues","Dokument","WHERE iDokumentId = " + icEntityIdList,"cAvsMott") = "" THEN DO:
    /* Sjekk om avsender er en ekstern kontakt: */
    cReturn = DYNAMIC-FUNCTION("getFieldValues","JBoxContact",
                                "WHERE iJBoxCompanyId = " + DYNAMIC-FUNCTION("getCompany")
                              + " AND cEmail = '" + hFieldMap:BUFFER-FIELD("cFrom"):BUFFER-VALUE + "'"
                               ,"cContactName,cAddress1,cPostalCode,cCity,iJBoxContactId").
   
    IF cReturn NE ? THEN DO:
      DO ix = 1 TO 4:
        IF ENTRY(ix,cReturn,"|") NE "" THEN
          cContact = cContact + (IF cContact NE "" AND ix NE 4 THEN ", " ELSE IF ix > 1 THEN " " ELSE "") 
                              + ENTRY(ix,cReturn,"|").
      END.
      DYNAMIC-FUNCTION("DoUpdate","Dokument","ignore",
                       "iDokumentId",
                       icEntityIdList,
                       "cAvsMott,iJBoxContactId,iPersAvsMott",
                       cContact + "|" + ENTRY(5,cReturn,"|") + "|-1",
                       YES).
    END.
    /* Hvis ikke, sjekk om avsender er i personregister: */
    ELSE DO:
      cReturn = DYNAMIC-FUNCTION("getFieldValues","Person",
                                 "WHERE iJBoxCompanyId = " + DYNAMIC-FUNCTION("getCompany")
                               + " AND cEpost = '" + hFieldMap:BUFFER-FIELD("cFrom"):BUFFER-VALUE + "'"
                                 ,"iPersonId").  
      IF cReturn NE ? THEN
        DYNAMIC-FUNCTION("DoUpdate","Dokument","ignore",
                         "iDokumentId",
                         icEntityIdList,
                         "iPersAvsMott",
                         cReturn,
                         YES).
    END.
  END.
END.
/* Oppretter nye dokumenter (journalposter) for sak og gjør knytning i samme slengen: */

/* Denne er nødvendig ellers blir ikke radene frisket opp.. */
DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcParamcOtherLinks","").

IF icEntity = "Sak" THEN DO:
  IF NOT DYNAMIC-FUNCTION("runProc","sak_opprett_dok_fra_epost.p",
                   SUBSTR(icEntityIdList,INDEX(icEntityIdList,"|") + 1) + "¤" + cEmailIdList
                  ,?) THEN
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
  ELSE DYNAMIC-FUNCTION("RefreshRowids",hBrowse,cRowIdList).
END.
ELSE IF cLastLoadedDocIds NE "" THEN DO:
  IF NOT DYNAMIC-FUNCTION("runProc","jbdoc_cre_docrel.p",
                   icEntity + "¤" + SUBSTR(icEntityIdList,INDEX(icEntityIdList,"|") + 1) + "¤" + cLastLoadedDocIds
                  ,?) THEN
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
  ELSE DYNAMIC-FUNCTION("RefreshRowids",hBrowse,cRowIdList).
END.

DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcParamcOtherLinks",cmbView:SCREEN-VALUE IN FRAME {&FRAME-NAME}).

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

IF NOT VALID-HANDLE(hLinkEmail) THEN DO:
  
  bEmailLink = YES.
  IF NUM-ENTRIES(cLinkTargets,";") > 2 THEN DO:
    RUN JBoxDSimpleSelectList.w (REPLACE(cLinkTargets,";","|"),?,OUTPUT cReturn).
    IF cReturn = ? THEN RETURN.
  END.
  ELSE IF NUM-ENTRIES(cLinkTargets,";") = 2 THEN
    cReturn = ENTRY(2,cLinkTargets,";").
  ELSE DO:
    DYNAMIC-FUNCTION("DoMessage",0,0,
                      IF DYNAMIC-FUNCTION("Scandinavian") THEN 
                        "Mangler parameteroppsett for arkivering av epost"
                      ELSE
                        "Missing parameter configuration for storing of emails"
                      ,"","").
    RETURN.  
  END.
  bEmailLink = NO.
  
  IF SEARCH("LinkDocTo" + cReturn + ".w") = ? AND SEARCH("LinkDocTo" + cReturn + ".r") = ? AND SEARCH("LinkDocTo" + cReturn + ".p") = ? THEN DO:
    DYNAMIC-FUNCTION("DoMessage",0,0,"Mangler program for arkivering av epost til " + cReturn,"","").
    RETURN.  
  END.
  
  IF hBrowse:NUM-SELECTED-ROWS = 0 THEN DO:
    DYNAMIC-FUNCTION("DoMessage",0,0,IF DYNAMIC-FUNCTION("Scandinavian") THEN
                                       "Ingen epost-rad er valgt for arkivering"
                                     ELSE "No email-row selected","","").
    RETURN.
  END.

  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN 
      ASSIGN cToList   = cToList   + (IF cToList   NE "" THEN ";" ELSE "") + hFieldMap:BUFFER-FIELD("cTo"):BUFFER-VALUE
             cFromList = cFromList + (IF cFromList NE "" THEN ";" ELSE "") + hFieldMap:BUFFER-FIELD("cFrom"):BUFFER-VALUE
             .
  END.

  FIND FIRST ttLinkTargets 
       WHERE ttLinkTargets.cLinkTarget = cReturn
       NO-ERROR.
  IF AVAIL ttLinkTargets AND VALID-HANDLE(ttLinkTargets.hLinkTarget) THEN
    hLinkTargetProc = ttLinkTargets.hLinkTarget.

  IF SEARCH("LinkDocTo" + cReturn + ".p") NE ? THEN
    RUN VALUE("LinkDocTo" + cReturn + ".p") ("link","email",cFromList + "|" + cToList,INPUT-OUTPUT hLinkTargetProc).
  ELSE
    RUN VALUE("LinkDocTo" + cReturn + ".w") ("link","email",cFromList + "|" + cToList,INPUT-OUTPUT hLinkTargetProc).

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
DEF VAR cLookupId       AS CHAR   NO-UNDO.
DEF VAR hMenuItem       AS HANDLE NO-UNDO.
DEF VAR hLinkTargetProc AS HANDLE NO-UNDO.

ASSIGN hMenuItem      = DYNAMIC-FUNCTION("getCurrentWidget")
       cLookupLabel   = hMenuItem:LABEL
       cLookupContext = ENTRY(LOOKUP(cLookupLabel,cLinkLabelList),cLinkContextList)
       cLookupId      = ENTRY(LOOKUP(cLookupLabel,cLinkLabelList),cLinkIdList,"¤")
       .

FIND FIRST ttLinkTargets 
     WHERE ttLinkTargets.cLinkTarget = cLookupContext
     NO-ERROR.
IF AVAIL ttLinkTargets AND VALID-HANDLE(ttLinkTargets.hLinkTarget) THEN
  hLinkTargetProc = ttLinkTargets.hLinkTarget.

IF SEARCH("LinkDocTo" + cLookupContext + ".p") NE ? THEN
  RUN VALUE("LinkDocTo" + cLookupContext + ".p") ("lookup","email",cLookupContext + "," + cLookupId,INPUT-OUTPUT hLinkTargetProc).
ELSE
  RUN VALUE("LinkDocTo" + cLookupContext + ".w") ("lookup","email",cLookupContext + "," + cLookupId,INPUT-OUTPUT hLinkTargetProc).

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
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = NO.
THIS-PROCEDURE:CURRENT-WINDOW:WINDOW-STATE = 3.
THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().

APPLY "entry" TO hBrowse.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenAttachmentsRecord C-Win 
PROCEDURE OpenAttachmentsRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cDocIdList AS CHAR NO-UNDO.
DO ix = 1 TO hBrwAttach:NUM-SELECTED-ROWS:
  IF hBrwAttach:FETCH-SELECTED-ROW(ix) THEN
    cDocIdList = cDocIdList + (IF cDocIdList NE "" THEN "|" ELSE "") + STRING(hBuffAttach:BUFFER-FIELD("iFromDocumentId"):BUFFER-VALUE).
END.
IF cDocIdList NE "" THEN
  DYNAMIC-FUNCTION("ViewDocs",
                    "JBoxDocument",
                    cDocIdList,
                    YES,
                    "").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setSimpleSelectAttributes C-Win 
PROCEDURE setSimpleSelectAttributes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihSelection AS HANDLE NO-UNDO.

IF DYNAMIC-FUNCTION("Scandinavian") THEN 
  ihSelection:FRAME:TITLE = (IF bEmailLink THEN "Velg knytning for epost" ELSE "Velg tilleggsknytning").
ELSE
  ihSelection:FRAME:TITLE = (IF bEmailLink THEN "Select link target for email" ELSE "Create additional link").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AdjustBrowseColumns C-Win 
FUNCTION AdjustBrowseColumns RETURNS LOGICAL
  ( INPUT ihBrowse     AS HANDLE,
    INPUT icBrowseName AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hColumn AS HANDLE NO-UNDO.

IF icBrowseName = "brwEmail" THEN DO:
  hColumn = DYNAMIC-FUNCTION("getBrowseColumn",ihBrowse,"cDisplayTo").
  hColumn:WIDTH-PIXELS = 100.
  hColumn = DYNAMIC-FUNCTION("getBrowseColumn",ihBrowse,"cDisplayFrom").
  hColumn:WIDTH-PIXELS = 100.
  hColumn = DYNAMIC-FUNCTION("getBrowseColumn",ihBrowse,"cDisplayCc").
  hColumn:WIDTH-PIXELS = 100.
  hColumn = DYNAMIC-FUNCTION("getBrowseColumn",ihBrowse,"cDisplayBcc").
  hColumn:WIDTH-PIXELS = 100.
END.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LocalTranslation C-Win 
FUNCTION LocalTranslation RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF NOT bScand THEN DO WITH FRAME {&FRAME-NAME}:
  ASSIGN cmbAccount:LABEL   = "Account"
         dDateSent:LABEL    = "Date"
         cDisplayFrom:LABEL = "From"
         cSubject:LABEL     = "Subject"
         cDisplayTo:LABEL   = "To"
         THIS-PROCEDURE:CURRENT-WINDOW:TITLE = "Archive email"
         .
END.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setDocRefLookupMenu C-Win 
FUNCTION setDocRefLookupMenu RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cLookupMenu   AS CHAR   NO-UNDO.
DEF VAR ix            AS INT    NO-UNDO.
DEF VAR hLookupMenu   AS HANDLE NO-UNDO.
DEF VAR cLinkLabel    AS CHAR   NO-UNDO.

DELETE OBJECT hBrowse:POPUP-MENU NO-ERROR.
ASSIGN cLinkLabelList   = ""
       cLinkContextList = ""
       cLinkIdList      = ""
       .

IF NOT hFieldMap:AVAIL THEN RETURN NO.

DO ix = 1 TO NUM-ENTRIES(hFieldMap:BUFFER-FIELD("cOtherLinks"):BUFFER-VALUE):
  IF LOOKUP(ENTRY(ix,hFieldMap:BUFFER-FIELD("cOtherLinks"):BUFFER-VALUE),cLinkTargets,";") MOD 2 = 0 THEN
    cLinkLabel = ENTRY(LOOKUP(ENTRY(ix,hFieldMap:BUFFER-FIELD("cOtherLinks"):BUFFER-VALUE),cLinkTargets,";") - 1,cLinkTargets,";").
  ELSE IF LOOKUP(ENTRY(ix,hFieldMap:BUFFER-FIELD("cOtherLinks"):BUFFER-VALUE),cLinkTargets,";") > 0 THEN
    cLinkLabel = ENTRY(LOOKUP(ENTRY(ix,hFieldMap:BUFFER-FIELD("cOtherLinks"):BUFFER-VALUE),cLinkTargets,";"),cLinkTargets,";").
  ELSE
    CASE ENTRY(ix,hFieldMap:BUFFER-FIELD("cOtherLinks"):BUFFER-VALUE):
      WHEN "Dokument" THEN cLinkLabel = IF bScand THEN "Sak" ELSE "Case".
      OTHERWISE cLinkLabel = ENTRY(ix,hFieldMap:BUFFER-FIELD("cOtherLinks"):BUFFER-VALUE).
    END CASE.

  ASSIGN cLinkLabelList   = cLinkLabelList + (IF cLinkLabelList NE "" THEN "," ELSE "") + cLinkLabel
         cLinkContextList = cLinkContextList + (IF cLinkContextList NE "" THEN "," ELSE "") + ENTRY(ix,hFieldMap:BUFFER-FIELD("cOtherLinks"):BUFFER-VALUE)
         cLinkIdList      = cLinkIdList + (IF cLinkIdList NE "" THEN "¤" ELSE "") + ENTRY(ix,hFieldMap:BUFFER-FIELD("OtherLinkKeys"):BUFFER-VALUE,"¤")
         cLookupMenu      = cLookupMenu 
                          + (IF cLookupMenu NE "" THEN "," ELSE "")
                          + "Lookup;"
                          + cLinkLabel
         .
END.

DYNAMIC-FUNCTION("NewMenuBand",hBrowse
                ,"MultiSortBrowse;" + (IF NOT bScand THEN "Sort on multiple columns" ELSE "Sorter på flere kolonner")
              + (IF cLookupMenu NE "" THEN 
                  ",|" + (IF bScand THEN "Vis" ELSE "View")
                 ELSE "")
                ,"").

IF cLookupMenu NE "" THEN DO:
  hLookupMenu = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hBrowse,"placeholder1")) NO-ERROR.
  IF VALID-HANDLE(hLookupMenu) THEN
    DYNAMIC-FUNCTION("NewMenuBand",hLookupMenu
    ,cLookupMenu
    ,"").
END.

RETURN YES.

END FUNCTION.


/*

      + ";+cOtherLinks|CHARACTER|x(50)|other_docrel(ROWID)|" + (IF bScand THEN "Dokumentknytninger" ELSE "Document links")                             
      + ";!+OtherLinkKeys|CHARACTER|x(30)|other_docrel_keys"
*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setExternalCaller C-Win 
FUNCTION setExternalCaller RETURNS LOGICAL
  ( INPUT ibExternalCaller AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
bExternalCaller = ibExternalCaller.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

