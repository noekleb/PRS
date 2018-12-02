&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
/* Procedure Description
"Basic Window Template

Use this template to create a new window. Alter this default template or create new ones to accomodate your needs for different default sizes and/or attributes."
*/
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*********************************************************************
* Copyright (C) 2001 by Progress Software Corporation ("PSC"),       *
* 14 Oak Park, Bedford, MA 01730, and other contributors as listed   *
* below.  All Rights Reserved.                                       *
*                                                                    *
* The Initial Developer of the Original Code is PSC.  The Original   *
* Code is Progress IDE code released to open source December 1, 2000.*
*                                                                    *
* The contents of this file are subject to the Possenet Public       *
* License Version 1.0 (the "License"); you may not use this file     *
* except in compliance with the License.  A copy of the License is   *
* available as of the date of this notice at                         *
* http://www.possenet.org/license.html                               *
*                                                                    *
* Software distributed under the License is distributed on an "AS IS"*
* basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. You*
* should refer to the License for the specific language governing    *
* rights and limitations under the License.                          *
*                                                                    *
* Contributors:                                                      *
*                                                                    *
*********************************************************************/
/*------------------------------------------------------------------------

  File:             CustOrders.w

  Description:      JukeBox sample program for nav.query and detail window usage with tab-folder

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author:           Brynjar Hasle, Chemistry as

  Created:          06.10.05

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unNavnd pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEF VAR bOK             AS LOG NO-UNDO.
DEF VAR ix              AS INT NO-UNDO.

DEF VAR hToolbar        AS HANDLE NO-UNDO.
DEF VAR hBrowse         AS HANDLE NO-UNDO.
DEF VAR hSearchField    AS HANDLE NO-UNDO.
DEF VAR hTabFolder      AS HANDLE NO-UNDO.
 
DEF VAR hCurrTabProc    AS HANDLE NO-UNDO.
DEF VAR hCurrTabFrame   AS HANDLE NO-UNDO.
DEF VAR hCurrTabQuery   AS HANDLE NO-UNDO.
DEF VAR iCurrTab        AS INT NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS KOrdreFolder KOrdreNavBrowse ~
KOrdreNavToolbar searchField ButikkNr 
&Scoped-Define DISPLAYED-OBJECTS ButikkNr 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD InitializeResize C-Win 
FUNCTION InitializeResize RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD TabChanged C-Win 
FUNCTION TabChanged RETURNS LOGICAL
  ( INPUT iiTab AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE ButikkNr AS CHARACTER 
     LABEL "Butikk" 
     VIEW-AS COMBO-BOX INNER-LINES 20
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN AUTO-COMPLETION UNIQUE-MATCH
     SIZE 36 BY 1 TOOLTIP "Trykk ENTER for å skifte butikk" NO-UNDO.

DEFINE RECTANGLE KOrdreFolder
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 146.4 BY 27.14.

DEFINE RECTANGLE KOrdreNavBrowse
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 38.6 BY 27.24.

DEFINE RECTANGLE KOrdreNavToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 15.4 BY .95.

DEFINE RECTANGLE searchField
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 19 BY .95.

DEFINE BUTTON btnSplitBarX 
     IMAGE-UP FILE "bmp/tabright.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE .8 BY 27.14
     BGCOLOR 12 FGCOLOR 12 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     ButikkNr AT ROW 1.24 COL 44 COLON-ALIGNED HELP
          "Trykk ENTER for å skifte butikk"
     KOrdreFolder AT ROW 2.67 COL 42.6
     KOrdreNavBrowse AT ROW 2.57 COL 1.2
     KOrdreNavToolbar AT ROW 1.24 COL 22
     searchField AT ROW 1.24 COL 2.2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 188.4 BY 28.95.

DEFINE FRAME frSplitBarX
     btnSplitBarX AT ROW 1 COL 9.2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 31.6 ROW 2.62
         SIZE 16.4 BY 27.19.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window Template
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Registrering av kundeordre"
         HEIGHT             = 28.95
         WIDTH              = 188.4
         MAX-HEIGHT         = 52.38
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 52.38
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

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT C-Win:LOAD-ICON("ico/cntrlhry.ico":U) THEN
    MESSAGE "Unable to load icon: ico/cntrlhry.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
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
ASSIGN FRAME frSplitBarX:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* SETTINGS FOR FRAME frSplitBarX
                                                                        */
ASSIGN 
       btnSplitBarX:MOVABLE IN FRAME frSplitBarX          = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME frSplitBarX
/* Query rebuild information for FRAME frSplitBarX
     _Query            is NOT OPENED
*/  /* FRAME frSplitBarX */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Registrering av kundeordre */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Registrering av kundeordre */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* Registrering av kundeordre */
DO:
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"Resize","").

  RUN MoveToTop IN hCurrTabProc NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frSplitBarX
&Scoped-define SELF-NAME btnSplitBarX
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSplitBarX C-Win
ON END-MOVE OF btnSplitBarX IN FRAME frSplitBarX
DO:
  DYNAMIC-FUNCTION("setSplitBarX" , THIS-PROCEDURE:CURRENT-WINDOW, btnSplitBarX:HANDLE IN FRAME frSplitBarX,NO).
  RUN MoveToTop IN hCurrTabProc NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME ButikkNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ButikkNr C-Win
ON VALUE-CHANGED OF ButikkNr IN FRAME DEFAULT-FRAME /* Butikk */
DO:
  DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
  RUN OpenQuery.
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
  IF NOT DYNAMIC-FUNCTION("DoCleanUpObjects",THIS-PROCEDURE:CURRENT-WINDOW) THEN
    RETURN NO-APPLY.
  PUBLISH "InvalidateHandle" (THIS-PROCEDURE).
  DYNAMIC-FUNCTION("setCleanUpResize",THIS-PROCEDURE:CURRENT-WINDOW).
  RUN disable_UI.
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

{incl/wintrigg.i}
{incl/conttrigg.i hCurrTabQuery}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  DISPLAY ButikkNr 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE KOrdreFolder KOrdreNavBrowse KOrdreNavToolbar searchField ButikkNr 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  ENABLE btnSplitBarX 
      WITH FRAME frSplitBarX IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-frSplitBarX}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExtraFlatViewRecord C-Win 
PROCEDURE ExtraFlatViewRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihFlatView   AS HANDLE NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO INIT TRUE.

DYNAMIC-FUNCTION("setUseLocalData" IN ihFlatView,TRUE).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FlatViewRecord C-Win 
PROCEDURE FlatViewRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR hFlatView AS HANDLE NO-UNDO.
DEF VAR hFlatBrw  AS HANDLE NO-UNDO.

RUN SUPER.

hFlatView = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hToolBar,"flatviewhandle")) NO-ERROR.
IF NOT VALID-HANDLE(hFlatView) THEN RETURN.

hFlatBrw  = DYNAMIC-FUNCTION("getBrowseHandle" IN hFlatView).


{incl/dynfilterlookups.i hBrowse}
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
DEF VAR hPageObject AS HANDLE NO-UNDO.
DEF VAR cTabList    AS CHAR   NO-UNDO.
DEF VAR iy          AS INT    NO-UNDO.

DYNAMIC-FUNCTION("NewObject",SESSION,"session").

DO WITH FRAME {&FRAME-NAME}:
  DYNAMIC-FUNCTION("NewObject",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE,"container").

  DYNAMIC-FUNCTION("setAttribute",SESSION,"ButikkNr",
                   DYNAMIC-FUNCTION("getFieldValues","Bruker","WHERE BrukerId = '" + DYNAMIC-FUNCTION("getASuserId") + "'","ButikkNr")).

  ASSIGN ButikkNr:DELIMITER = "|"
         ButikkNr:LIST-ITEM-PAIRS = RIGHT-TRIM("|0|" + DYNAMIC-FUNCTION("GetFieldList","Butiker;Butik|ButNamn;Butik","WHERE true BY ButNamn"),"|")
         ButikkNr:SCREEN-VALUE = DYNAMIC-FUNCTION("getAttribute",SESSION,"ButikkNr")
         .

  hBrowse = DYNAMIC-FUNCTION("NewBrowse",
                    KOrdreNavBrowse:HANDLE,   
                    100,                 
                    "",                  
                    "KOrdreHode"
                    + ";Navn"
                    + ";KOrdre_Id|Ordrenr"
                    + ";KundeNr"
                    + ";LevStatus"
                    ,"WHERE false"
/*                     ,"WHERE LevStatus < '3'" */
                    ,"sort|KOrdre_Id desc").
  hBrowse:GET-BROWSE-COLUMN(1):WIDTH-PIXELS = 100.
  hBrowse:GET-BROWSE-COLUMN(2):WIDTH-PIXELS = 60.

  hSearchField = DYNAMIC-FUNCTION("NewBrowseSearchField",SearchField:HANDLE,hBrowse,1).
  DYNAMIC-FUNCTION("CreateObjectLink",hSearchField,hBrowse).

  DYNAMIC-FUNCTION("createObjectLink",hBrowse,THIS-PROCEDURE).

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"filterlookupfields_Salesrep","Salesrep;Salesrep;RepNavn").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"filterlookupquery_Salesrep","where false").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"filterlookupreturnfield_Salesrep","Salesrep").

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"flatviewjointype","outer-join").

  hTabFolder = DYNAMIC-FUNCTION("NewTabFolder",KOrdreFolder:HANDLE).
  
  /* X and Y limits for move of widget are not yet set for the window. 
     Since we want the tabs to resize according to the window size these values must be set here and
     they must be exact the same as in setOrwWinSize (see InitResize function) 
     Here the values are set to not give any automatic move of widgets */
     
  DYNAMIC-FUNCTION("setMinXYmove",2000,1200). 

  DYNAMIC-FUNCTION("InitPages" IN hTabFolder,"Ordre|KOrdreView.w|Kunderegister|KundeView.w|Faktura|Faktura.w",hBrowse).
  
  DYNAMIC-FUNCTION("buildFolder" IN hTabFolder).

  hToolbar = DYNAMIC-FUNCTION("NewToolBar",
                    KOrdreNavToolbar:HANDLE,             
                    "Fil",                          
                    "filter,flatview,excel",                                                    
                    "").

  DYNAMIC-FUNCTION("createObjectLink",hBrowse,hToolbar).

  InitializeResize().

END.

DYNAMIC-FUNCTION("initTranslation",THIS-PROCEDURE:CURRENT-WINDOW).

DYNAMIC-FUNCTION("TabFolderChanged" IN hTabFolder,1).

APPLY "value-changed" TO ButikkNr.

APPLY "entry" TO hSearchField.

THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = FALSE.

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
{&WINDOW-NAME}:WINDOW-STATE = 3.
{&WINDOW-NAME}:MOVE-TO-TOP().
APPLY "entry" TO hSearchField.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenQuery C-Win 
PROCEDURE OpenQuery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DYNAMIC-FUNCTION("setAttribute",hBrowse,"basequery","WHERE ButikkNr = " + ButikkNr:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
RUN SUPER.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION InitializeResize C-Win 
FUNCTION InitializeResize RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  - Create the splitbar and grab the widgets that should follow it's move.
            - Set resize rules for this frame 
            - Set resize rules for window (and load previous settings for size and pos)
    Notes:  
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  DYNAMIC-FUNCTION("setSplitBarX" , THIS-PROCEDURE:CURRENT-WINDOW, btnSplitBarX:HANDLE IN FRAME frSplitBarX,NO).

  DYNAMIC-FUNCTION("setFollowSplitBarX",THIS-PROCEDURE:CURRENT-WINDOW,btnSplitBarX:HANDLE IN FRAME frSplitBarX, 
                    DYNAMIC-FUNCTION("getWidgetsByLasso",KOrdreNavBrowse:HANDLE IN FRAME {&FRAME-NAME},0,"frame,control-frame,rectangle,browse") + "," +
                    DYNAMIC-FUNCTION("getWidgetsByLasso",KOrdreFolder:HANDLE IN FRAME {&FRAME-NAME},0,"frame,browse,control-frame,rectangle,editor,button,toggle-box,combo-box,fill-in")
                    ).

  DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "KOrdreNavBrowse,searchField," + hBrowse:NAME).
  DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "KOrdreNavToolbar").

  DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,2000,1200,0,550).
END.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION TabChanged C-Win 
FUNCTION TabChanged RETURNS LOGICAL
  ( INPUT iiTab AS INT ) :
/*------------------------------------------------------------------------------
  Purpose: We don't want to sync all tabfolders when changing the tab-folder
           Therefore we delete and re-establish links accordingly 
           
    Notes: Deleting all links from the navigation browse will also delete the  
           link to to the toolbar. That doesn't matter since the link is two-ways and we keep the link from the toolbar to the browse.
           For tab 1 we use a OneToOne link type rather than Parent. This way syncronization of updates are handled automatically 
------------------------------------------------------------------------------*/
IF iCurrTab NE 0 THEN
  DYNAMIC-FUNCTION("DeleteObjectLink",hBrowse,DYNAMIC-FUNCTION("getPageQuery" IN hTabFolder,iCurrTab)).

ASSIGN hCurrTabQuery = DYNAMIC-FUNCTION("getPageQuery" IN hTabFolder,iiTab)
       hCurrTabProc  = DYNAMIC-FUNCTION("getPageHandle" IN hTabFolder,iiTab)
       hCurrTabFrame = DYNAMIC-FUNCTION("getPageFrame" IN hTabFolder,iiTab)
       iCurrTab      = iiTab.


IF iiTab = 1 THEN
  DYNAMIC-FUNCTION("CreateOneToOneLink",hCurrTabQuery,hBrowse,"KOrdre_id").
ELSE IF hCurrTabProc:FILE-NAME = "KundeView.w" THEN
  DYNAMIC-FUNCTION("CreateParentLink",hCurrTabQuery,hBrowse,"KundeNr").
ELSE 
  DYNAMIC-FUNCTION("CreateParentLink",hCurrTabQuery,hBrowse,"KOrdre_id").


DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
RUN DisplayRecord.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

