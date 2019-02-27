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

DEF VAR bOk             AS LOG    NO-UNDO.
DEF VAR ix              AS INT    NO-UNDO.
DEF VAR hBrowse         AS HANDLE NO-UNDO.
DEF VAR hQuery          AS HANDLE NO-UNDO.
DEF VAR hToolbar        AS HANDLE NO-UNDO.
DEF VAR hFieldMap       AS HANDLE NO-UNDO.
DEF VAR httEmailBuffer  AS HANDLE NO-UNDO.
DEF VAR hEmailListener  AS HANDLE NO-UNDO.
DEF VAR hBrwAttach      AS HANDLE NO-UNDO.
DEF VAR hBuffAttach     AS HANDLE NO-UNDO.
DEF VAR hIe             AS HANDLE NO-UNDO.
DEF VAR cCurrAccountId  AS CHAR   NO-UNDO.
DEF VAR iMailCount      AS INT    NO-UNDO.
DEF VAR iMailRetrieved  AS INT    NO-UNDO.
DEF VAR hHelpText       AS HANDLE NO-UNDO.
DEF VAR bScand          AS LOG    NO-UNDO.
DEF VAR hLinkEmail      AS HANDLE NO-UNDO.
DEF VAR bExternalCaller AS LOG    NO-UNDO.
DEF VAR cLinkTargets    AS CHAR   NO-UNDO.
DEF VAR cToList         AS CHAR   NO-UNDO.
DEF VAR cFromList       AS CHAR   NO-UNDO.
DEF VAR bEmailLink      AS LOG    NO-UNDO.

DEF TEMP-TABLE ttSortMsg NO-UNDO
    FIELD iMsgId    AS INT
    FIELD cSubject  AS CHAR
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
&Scoped-Define ENABLED-OBJECTS btnSplitBarY brwEmail tbEmail brwAttachments ~
rectIE cmbAccount dDateSent cTimeSent cSubject cFrom cTo 
&Scoped-Define DISPLAYED-OBJECTS cmbAccount dDateSent cTimeSent cSubject ~
cFrom cTo 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LocalTranslation C-Win 
FUNCTION LocalTranslation RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setCountText C-Win 
FUNCTION setCountText RETURNS LOGICAL
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
     SIZE 155.6 BY .29.

DEFINE VARIABLE cmbAccount AS CHARACTER FORMAT "X(256)":U 
     LABEL "Konto" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 70 BY 1 NO-UNDO.

DEFINE VARIABLE cFrom AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fra" 
     VIEW-AS FILL-IN 
     SIZE 80 BY 1 NO-UNDO.

DEFINE VARIABLE cSubject AS CHARACTER FORMAT "X(256)":U 
     LABEL "Emne" 
     VIEW-AS FILL-IN 
     SIZE 80 BY 1 NO-UNDO.

DEFINE VARIABLE cTimeSent AS CHARACTER FORMAT "X(5)":U 
     VIEW-AS FILL-IN 
     SIZE 10.2 BY 1 NO-UNDO.

DEFINE VARIABLE cTo AS CHARACTER FORMAT "X(256)":U 
     LABEL "Til" 
     VIEW-AS FILL-IN 
     SIZE 80 BY 1 NO-UNDO.

DEFINE VARIABLE dDateSent AS DATE FORMAT "99/99/9999":U 
     LABEL "Dato" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE RECTANGLE brwAttachments
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 59 BY 4.33.

DEFINE RECTANGLE brwEmail
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 153 BY 10.95.

DEFINE RECTANGLE rectIE
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 154 BY 9.38.

DEFINE RECTANGLE tbEmail
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 9.8 BY .91.

DEFINE BUTTON btnSplitBarX 
     IMAGE-UP FILE "bmp/tableft_small.bmp":U NO-FOCUS FLAT-BUTTON NO-CONVERT-3D-COLORS
     LABEL "" 
     SIZE .8 BY 4.24.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnSplitBarY AT ROW 13.48 COL 1.4
     cmbAccount AT ROW 1.24 COL 10 COLON-ALIGNED
     dDateSent AT ROW 14.1 COL 13 COLON-ALIGNED
     cTimeSent AT ROW 14.1 COL 31.4 COLON-ALIGNED NO-LABEL
     cSubject AT ROW 15.14 COL 13 COLON-ALIGNED
     cFrom AT ROW 16.19 COL 13 COLON-ALIGNED
     cTo AT ROW 17.24 COL 13 COLON-ALIGNED
     brwEmail AT ROW 2.52 COL 3
     tbEmail AT ROW 1.29 COL 83.4
     brwAttachments AT ROW 14.05 COL 97
     rectIE AT ROW 18.67 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 156 BY 27.29.

DEFINE FRAME frmSplitBarX
     btnSplitBarX AT ROW 1 COL 33.2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 62.8 ROW 14.1
         SIZE 63.2 BY 4.29.


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
         WIDTH              = 156
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
       FRAME DEFAULT-FRAME:HEIGHT           = 27.29
       FRAME DEFAULT-FRAME:WIDTH            = 156.

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
  RUN SwitchAccount(cmbAccount:SCREEN-VALUE).
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
  IF VALID-HANDLE(hEmailListener) THEN APPLY "close" TO hEmailListener.
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
  DYNAMIC-FUNCTION("OpenAttachment" IN hEmailListener,
                   hFieldMap:BUFFER-FIELD("iMsgId"):BUFFER-VALUE,
                   hBuffAttach:BUFFER-FIELD("cAttachment"):BUFFER-VALUE).
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
DEF VAR cMsg AS CHAR NO-UNDO.

EMPTY TEMP-TABLE ttSortMsg.
DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
  IF hBrowse:FETCH-SELECTED-ROW(ix) THEN DO:
    CREATE ttSortMsg.
    ASSIGN ttSortMsg.iMsgId = hFieldMap:BUFFER-FIELD("iMsgId"):BUFFER-VALUE
           ttSortMsg.cSubject = hFieldMap:BUFFER-FIELD("cSubject"):BUFFER-VALUE
           cMsg = cMsg + (IF cMsg NE "" THEN CHR(10) ELSE "") + hFieldMap:BUFFER-FIELD("cSubject"):BUFFER-VALUE.
  END.
END.


IF DYNAMIC-FUNCTION("DoMessage",0,4,IF DYNAMIC-FUNCTION("Scandinavian") THEN
                                      "Slett epost fra epost-server" + CHR(10) + cMsg
                                    ELSE
                                      "Delete email from mailserver" + CHR(10) + cMsg,
                                    "","") = 6 THEN DO:
  FOR EACH ttSortMsg BY ttSortMsg.iMsgId DESC:
    IF NOT DYNAMIC-FUNCTION("DeleteEmail" IN hEmailListener,ttSortMsg.iMsgId) THEN
      DYNAMIC-FUNCTION("DoMessage",0,0,IF DYNAMIC-FUNCTION("Scandinavian") THEN
                                         "Sletting feilet for" + CHR(10) + ttSortMsg.cSubject
                                       ELSE
                                         "Delete failed for" + CHR(10) + ttSortMsg.cSubject
                                      ,"","").
  END.
END.
RUN RefreshEmailQuery.

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
IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowse AND hFieldMap:AVAIL THEN DO:
  hBuffAttach:EMPTY-TEMP-TABLE().
  DO ix = 1 TO NUM-ENTRIES(hFieldMap:BUFFER-FIELD("cAttachments"):BUFFER-VALUE,";"):
    hBuffAttach:BUFFER-CREATE().
    ASSIGN hBuffAttach:BUFFER-FIELD("cAttachment"):BUFFER-VALUE = ENTRY(ix,hFieldMap:BUFFER-FIELD("cAttachments"):BUFFER-VALUE,";")
           hBuffAttach:BUFFER-FIELD("iMsgId"):BUFFER-VALUE = hFieldMap:BUFFER-FIELD("iMsgId"):BUFFER-VALUE
           .
  END.
END.

RUN SUPER.

IF VALID-HANDLE(hIe) AND VALID-HANDLE(hEmailListener) AND hFieldMap:AVAIL THEN
  DYNAMIC-FUNCTION("NavigateToURL" IN hIe,
                   DYNAMIC-FUNCTION("SaveMailAsHTML" IN hEmailListener,hFieldMap:BUFFER-FIELD("iMsgId"):BUFFER-VALUE)
                   ).
ELSE IF VALID-HANDLE(hIe) THEN
  DYNAMIC-FUNCTION("ViewText" IN hIe,"").


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EmailRecord C-Win 
PROCEDURE EmailRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
ASSIGN iMailRetrieved = 0
       iMailCount     = 0.

IF VALID-HANDLE(hEmailListener) THEN
  RUN getEmail IN hEmailListener.

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
  DISPLAY cmbAccount dDateSent cTimeSent cSubject cFrom cTo 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE btnSplitBarY brwEmail tbEmail brwAttachments rectIE cmbAccount 
         dDateSent cTimeSent cSubject cFrom cTo 
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

PUBLISH "getEmailTable" (OUTPUT httEmailBuffer,OUTPUT hEmailListener).

DO WITH FRAME {&FRAME-NAME}:

  ASSIGN cmbAccount:DELIMITER  = "|"
         cmbAccount:LIST-ITEM-PAIRS = RIGHT-TRIM("|0|" + DYNAMIC-FUNCTION("getFieldList",
                                                       "JBoxEmailAccount;cAccountName|cEmailAddress;iJBoxEmailAccountId",
                                                       "where true(codemaster)"
                                                         ),"|")
         bScand       = DYNAMIC-FUNCTION("Scandinavian")
         cLinkTargets = DYNAMIC-FUNCTION("getFieldValues","JBoxSysParam","WHERE cSysParamName = 'DocumentLinkTarget'(codemaster)","cSysParamCharValue")
         .

  IF VALID-HANDLE(hEmailListener) AND VALID-HANDLE(httEmailBuffer) THEN DO:
    SUBSCRIBE TO "RefreshEmailQuery" IN hEmailListener.
    SUBSCRIBE TO "NewEmailCount" IN hEmailListener.
    cmbAccount:SCREEN-VALUE = DYNAMIC-FUNCTION("getEmailUser" IN hEmailListener).
  END.
  ELSE cmbAccount:SCREEN-VALUE = "0".
  
  SUBSCRIBE TO "UploadEmailToDb" ANYWHERE.

  hBrowse = DYNAMIC-FUNCTION("NewBrowse"
      ,brwEmail:HANDLE
      ,10000
      ,"multiple"
      ,"TEMP-TABLE"
     + ";dDateSent|DATE|99/99/9999||" + (IF bScand THEN "Dato" ELSE "Date")
     + ";cTimeSent|CHARACTER|x(5)||" + (IF bScand THEN "Tid" ELSE "Time")
     + ";cSubject|CHARACTER|x(80)||" + (IF bScand THEN "Emne" ELSE "Subject")
     + ";cFrom|CHARACTER|x(50)||" + (IF bScand THEN "Fra" ELSE "From")
     + ";cTo|CHARACTER|x(50)||" + (IF bScand THEN "Til" ELSE "To")
     + ";!cCc|CHARACTER|x(50)||" + (IF bScand THEN "Kopi" ELSE "CC")
     + ";!cBCc|CHARACTER|x(50)||" + (IF bScand THEN "Blind kopi" ELSE "BCC")
     + ";iPriority|INTEGER|9||Pri"
     + ";!cBody|CHARACTER|x(256)||Tekst"
     + ";!cFromEmail|CHARACTER|x(40)||From"
     + ";!iMsgType|INTEGER|9||Msg.type"
     + ";iAttachCount|INTEGER|>9||Att.cnt"
     + ";cAttachments|CHARACTER|x(80)||Attachments"
     + ";!iMsgId|INTEGER|>>>>9||MsgId"
     + ";!RowCount|INTEGER|>>9||Cnt"
      ,"WHERE false"
      ,"").

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"useLocalData","yes").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"shadedRows","yes").

  hFieldMap = DYNAMIC-FUNCTION("NewFieldMap"
    ,hBrowse:QUERY
    ,FRAME {&FRAME-NAME}:HANDLE
    ,"",""
    ,"dDateSent,cTimeSent,cFrom,cSubject,cTo",""
    ,"").

  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hFieldMap).

  hToolbar = DYNAMIC-FUNCTION("NewToolBar"
    ,tbEmail:HANDLE
    ,IF bScand THEN "Fil" ELSE "File"
    ,"Email;" + (IF bScand THEN "Hent epostctrl-g;Hent epost" ELSE "Get emailctrl-g;Get email") 
              + ";;gif\ei0021-16.gif"
              + "¤enable"
   + ",Delete;" + (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Slett" ELSE "Delete")
/*    + ",save;" + (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Lagre" ELSE "Save") */
   + ",excel;" + (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Eksporter til E&xcel" ELSE "Export to Excel")
   + ",browseconfig;" + (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Kolonneoppsett" ELSE "Column setup")
   + (IF NOT bExternalCaller THEN 
        ",Link;" + (IF bScand THEN "Arkiverctrl-a;Arkiver" ELSE "Archive emailctrl-a;Archive email") 
                 + ";;bmp\links.bmp"
      ELSE "")
    ,"maxborder").

  DYNAMIC-FUNCTION("CreateObjectLink",hToolbar,hBrowse).

  hBrwAttach = DYNAMIC-FUNCTION("NewBrowse"
    ,brwAttachments:HANDLE
    ,100
    ,"multiple"
    ,"temp-table"
     + ";cAttachment|CHARACTER|x(50)||" + (IF bScand THEN "Vedlegg" ELSE "Attachments")
     + ";!iMsgId|INTEGER|>>>9"
    ,"WHERE false"
    ,"").

  DYNAMIC-FUNCTION("setAttribute",hBrwAttach,"viewRecordCount","no").

  hBuffAttach = hBrwAttach:QUERY:GET-BUFFER-HANDLE(1).

  DYNAMIC-FUNCTION("CreateParentLink",hBrwAttach,hBrowse,"iMsgId").

  DYNAMIC-FUNCTION("NewMenuBand",hBrwAttach
     ,"OpenAttachments;" + IF bScand THEN "Åpne vedlegg" ELSE "Open attachment"
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
                   "cFrom,cSubject,cTo").
  DYNAMIC-FUNCTION("setNoMoveX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,
                   "cFrom,cSubject,cTo").
  DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,
                   "cFrom,cSubject,cTo").
  
  DYNAMIC-FUNCTION("SetSplitBarX",THIS-PROCEDURE:CURRENT-WINDOW,btnSplitBarX:HANDLE IN FRAME frmSplitBarX,NO).
  DYNAMIC-FUNCTION("setFollowSplitBarX",THIS-PROCEDURE:CURRENT-WINDOW,btnSplitBarX:HANDLE IN FRAME frmSplitBarX,
                   STRING(cFrom:HANDLE) + "," +
                   STRING(cTo:HANDLE) + "," +
                   STRING(cSubject:HANDLE) + "," +
/*                    STRING(brwEmail:HANDLE) + "," + */
/*                    STRING(hBrowse) + "," +         */
                   STRING(brwAttachments:HANDLE) + "," + 
                   STRING(hBrwAttach)
                   ).
  
  IF SEARCH("controls.dll") NE ? THEN 
    RUN JBoxJlwSupIe.w PERSISTENT SET hIe.
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
                   STRING(cFrom:HANDLE) + "," +
                   STRING(cTo:HANDLE) + "," +
                   STRING(dDateSent:HANDLE) + "," +
                   STRING(cTimeSent:HANDLE) + "," +
                   STRING(cSubject:HANDLE) + "," +
                   STRING(rectIe:HANDLE) + "," + 
                   DYNAMIC-FUNCTION("getFrameHandleList" IN hIe)
                   ).
  DYNAMIC-FUNCTION("setSplitBarYlimits",btnSplitBarY:HANDLE IN FRAME {&FRAME-NAME},150,150).
  
  DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,500,200,0,0).

  LocalTranslation().

  DYNAMIC-FUNCTION("InitTranslation",THIS-PROCEDURE:CURRENT-WINDOW).

  IF cmbAccount:NUM-ITEMS = 2 AND cmbAccount:SCREEN-VALUE = ? THEN DO:
    cmbAccount:SCREEN-VALUE = cmbAccount:ENTRY(2).
    APPLY "value-changed" TO cmbAccount.
  END.
  ELSE IF cmbAccount:SCREEN-VALUE NE ? THEN DO:
    RUN EmailRecord.
    IF iMailCount = 0 THEN RUN RefreshEmailQuery.
  END.

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
                        "Mangler oppsett for arkivering av epost"
                      ELSE
                        "Missing configuration for storing of emails"
                      ,"","").
    RETURN.  
  END.
  bEmailLink = NO.
  
  IF SEARCH("LinkEmailTo" + cReturn + ".w") = ? AND SEARCH("LinkEmailTo" + cReturn + ".r") = ? AND SEARCH("LinkEmailTo" + cReturn + ".p") = ? THEN DO:
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

  IF SEARCH("LinkEmailTo" + cReturn + ".p") NE ? THEN
    RUN VALUE("LinkEmailTo" + cReturn + ".p") (cFromList + "|" + cToList,INPUT-OUTPUT hLinkTargetProc).
  ELSE
    RUN VALUE("LinkEmailTo" + cReturn + ".w") (cFromList + "|" + cToList,INPUT-OUTPUT hLinkTargetProc).

  IF VALID-HANDLE(hLinkTargetProc) THEN DO:
    FIND FIRST ttLinkTargets
         WHERE ttLinkTargets.cLinkTarget = cReturn
         NO-ERROR.
    IF NOT AVAIL ttLinkTargets THEN DO:
      CREATE ttLinkTargets.
      ttLinkTargets.cLinkTarget = cReturn.
    END.
    ttLinkTargets.hLinkTarget = hLinkTargetProc.
  END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NewEmailCount C-Win 
PROCEDURE NewEmailCount :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM iiMailCount AS INT NO-UNDO.

iMailCount = iiMailCount.

IF iMailCount > 0 THEN DO:
  cmbAccount:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
  DYNAMIC-FUNCTION("setAttribute",hToolbar,"disabledEvents","*").
END.

setCountText().

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
DEF VAR cFileList AS CHAR NO-UNDO.
DO ix = 1 TO hBrwAttach:NUM-SELECTED-ROWS:
  IF hBrwAttach:FETCH-SELECTED-ROW(ix) THEN
    cFileList = cFileList + (IF cFileList NE "" THEN ";" ELSE "") + hBuffAttach:BUFFER-FIELD("cAttachment"):BUFFER-VALUE.
END.
IF cFileList NE "" THEN
  DYNAMIC-FUNCTION("OpenAttachment" IN hEmailListener,
                   hFieldMap:BUFFER-FIELD("iMsgId"):BUFFER-VALUE,
                   cFileList).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RefreshBrowseRecord C-Win 
PROCEDURE RefreshBrowseRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowse THEN
  RUN EmailRecord.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RefreshEmailQuery C-Win 
PROCEDURE RefreshEmailQuery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR hEmailQuery AS HANDLE NO-UNDO.

hFieldMap:EMPTY-TEMP-TABLE().

IF SOURCE-PROCEDURE:FILE-NAME NE THIS-PROCEDURE:FILE-NAME THEN DO:
  iMailRetrieved = iMailRetrieved + 1.
  setCountText().
END.

CREATE QUERY hEmailQuery.
hEmailQuery:SET-BUFFERS(httEmailBuffer).
hEmailQuery:QUERY-PREPARE("FOR EACH " + httEmailBuffer:NAME).
hEmailQuery:QUERY-OPEN().
hEmailQuery:GET-FIRST().
REPEAT WHILE NOT hEmailQuery:QUERY-OFF-END:
  hFieldMap:BUFFER-CREATE().
  hFieldMap:BUFFER-COPY(hEmailQuery:GET-BUFFER-HANDLE(1)).
  hEmailQuery:GET-NEXT().
END.

DELETE OBJECT hEmailQuery.

RUN InvokeMethod(hBrowse,"OpenQuery").
IF hFieldMap:AVAIL THEN DO:
  APPLY "entry" TO hBrowse.
  hBrowse:SELECT-ROW(hBrowse:FOCUSED-ROW).
END.
setCountText().


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SwitchAccount C-Win 
PROCEDURE SwitchAccount :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icAccountId AS CHAR NO-UNDO.

DEF VAR cMailServer AS CHAR NO-UNDO.

IF icAccountId = cCurrAccountId OR icAccountId = "0" OR icAccountId = ? THEN RETURN.

cCurrAccountId = icAccountId.

IF VALID-HANDLE(hEmailListener) THEN
  APPLY "close" TO hEmailListener.

cMailServer = DYNAMIC-FUNCTION("getFieldValues","JBoxEmailAccount",
                                "WHERE iJBoxEmailAccountId = " + icAccountId,
                                "cMailServer,iPort,cUserName,cPassword,iAuthMethod").


/* RUN JBoxEmailListener.p PERSIST SET hEmailListener.  */

RUN JBoxJlwEmailListener.w PERSIST SET hEmailListener.

DYNAMIC-FUNCTION("setMailServerProperties" IN hEmailListener,
                 ENTRY(1,cMailServer,"|"),
                 INTEGER(ENTRY(2,cMailServer,"|")),
                 ENTRY(3,cMailServer,"|"),
                 ENTRY(4,cMailServer,"|"),
                 INTEGER(ENTRY(5,cMailServer,"|"))
                 ).
                 
RUN InitializeObject IN hEmailListener.
IF VALID-HANDLE(hEmailListener) THEN DO:
  SUBSCRIBE TO "RefreshEmailQuery" IN hEmailListener.
  SUBSCRIBE TO "NewEmailCount" IN hEmailListener.
  httEmailBuffer = DYNAMIC-FUNCTION("getTTEmailBuffer" IN hEmailListener).
  RUN RefreshEmailQuery.
  RUN EmailRecord.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UploadEmailToDb C-Win 
PROCEDURE UploadEmailToDb :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icEntity       AS CHAR NO-UNDO.
DEF INPUT PARAM icEntityIdList AS CHAR NO-UNDO.

DEF VAR iReturn            AS INT  NO-UNDO.
DEF VAR cReturn            AS CHAR NO-UNDO.
DEF VAR cMsg               AS CHAR NO-UNDO.
DEF VAR cLastLoadedDocIds  AS CHAR NO-UNDO.

IF NOT hFieldMap:AVAIL THEN DO:
  DYNAMIC-FUNCTION("DoMessage",0,0,IF DYNAMIC-FUNCTION("Scandinavian") THEN "Ingen epost valgt" ELSE "No email selected","","").
  RETURN.  
END.

EMPTY TEMP-TABLE ttSortMsg.
DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
  IF hBrowse:FETCH-SELECTED-ROW(ix) THEN DO:
    CREATE ttSortMsg.
    ASSIGN ttSortMsg.iMsgId = hFieldMap:BUFFER-FIELD("iMsgId"):BUFFER-VALUE
           ttSortMsg.cSubject = hFieldMap:BUFFER-FIELD("cSubject"):BUFFER-VALUE
           cMsg = cMsg + (IF cMsg NE "" THEN CHR(10) ELSE "") + hFieldMap:BUFFER-FIELD("cSubject"):BUFFER-VALUE.
  END.
END.

iReturn = DYNAMIC-FUNCTION("DoMessage",0,3,IF DYNAMIC-FUNCTION("Scandinavian") THEN
                                      "Arkivering av epost" + CHR(10) + cMsg + CHR(10) + CHR(10) + "Skal epost slettes fra epost-server etter arkivering?"
                                    ELSE
                                      "Upload of email" + CHR(10) + cMsg + CHR(10) + CHR(10) + "Delete message from email-server after upload?",
                                    "","").

IF iReturn = 2 THEN RETURN.

FOR EACH ttSortMsg BY ttSortMsg.iMsgId DESC:
  IF NOT DYNAMIC-FUNCTION("UploadMailToDb" IN hEmailListener,
                          ttSortMsg.iMsgId,icEntity,ENTRY(1,icEntityIdList,"|"),ttSortMsg.cSubject,iReturn = 6) THEN DO:
    DYNAMIC-FUNCTION("DoMessage",0,0,
                              IF DYNAMIC-FUNCTION("Scandinavian") THEN
                                "Feil ved lasting av epost dokument til database"
                              ELSE
                                "Error when uploading email document to database"
                             ,"","").      
    LEAVE.
  END.
  ELSE cLastLoadedDocIds = cLastLoadedDocIds + (IF cLastLoadedDocIds NE "" THEN "," ELSE "") + DYNAMIC-FUNCTION("getOutParam").
END.

IF cLastLoadedDocIds NE "" AND NUM-ENTRIES(icEntityIdList,"|") > 1 THEN DO:
  IF NOT DYNAMIC-FUNCTION("runProc","jbdoc_cre_docrel.p",
                   icEntity + "¤" + SUBSTR(icEntityIdList,INDEX(icEntityIdList,"|") + 1) + "¤" + cLastLoadedDocIds
                  ,?) THEN
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
END.

IF cLastLoadedDocIds NE "" AND NUM-ENTRIES(cLinkTargets,";") > 2 THEN REPEAT:
  RUN JBoxDSimpleSelectList.w (REPLACE(cLinkTargets,";","|"),?,OUTPUT cReturn).
  IF cReturn = ? THEN LEAVE.
END.

RUN RefreshEmailQuery.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LocalTranslation C-Win 
FUNCTION LocalTranslation RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF NOT bScand THEN DO WITH FRAME {&FRAME-NAME}:
  ASSIGN cmbAccount:LABEL = "Account"
         dDateSent:LABEL  = "Date"
         cFrom:LABEL      = "From"
         cSubject:LABEL   = "Subject"
         cTo:LABEL        = "To"
         THIS-PROCEDURE:CURRENT-WINDOW:TITLE = "Archive email"
         .
END.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setCountText C-Win 
FUNCTION setCountText RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF DYNAMIC-FUNCTION("Scandinavian") THEN
  hHelpText:SCREEN-VALUE = "Hentet " + STRING(iMailRetrieved) + " av " + STRING(iMailCount) + " meldinger".
ELSE
  hHelpText:SCREEN-VALUE = "Fetched " + STRING(iMailRetrieved) + " of " + STRING(iMailCount) + " messages".

hBrowse:HELP = hHelpText:SCREEN-VALUE.

IF iMailRetrieved = iMailCount OR iMailCount = 0 THEN DO:
  cmbAccount:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
  DYNAMIC-FUNCTION("setAttribute",hToolbar,"disabledEvents","").
END.

RETURN YES.

END FUNCTION.

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

