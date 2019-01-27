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
DEF VAR hWinToolbar     AS HANDLE NO-UNDO.
 
DEF VAR hCurrTabProc    AS HANDLE NO-UNDO.
DEF VAR hCurrTabFrame   AS HANDLE NO-UNDO.
DEF VAR hCurrTabQuery   AS HANDLE NO-UNDO.
DEF VAR iCurrTab        AS INT    NO-UNDO.

DEF VAR cFollowSpList   AS CHAR   NO-UNDO.
DEF VAR iApneNettbutikk AS INT NO-UNDO.
DEF VAR iNettButikk     AS INT NO-UNDO.

DEFINE VARIABLE hbcKOrdre_Id AS HANDLE NO-UNDO.
DEFINE VARIABLE hbfKOrdre_Id AS HANDLE NO-UNDO.
DEFINE VARIABLE bTest AS LOG NO-UNDO.
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.

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
KOrdreNavToolbar searchField rectWinToolBar ButikkNr cmbModus Strekkode 
&Scoped-Define DISPLAYED-OBJECTS ButikkNr cmbModus Strekkode 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AdjustBrowseColumns C-Win 
FUNCTION AdjustBrowseColumns RETURNS LOGICAL
  ( INPUT ihBrowse     AS HANDLE,
    INPUT icBrowseName AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getButikkNr C-Win 
FUNCTION getButikkNr RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getKundeNr C-Win 
FUNCTION getKundeNr RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD InitializeResize C-Win 
FUNCTION InitializeResize RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setBaseQuery C-Win 
FUNCTION setBaseQuery RETURNS LOGICAL
  (  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setFocus C-Win 
FUNCTION setFocus RETURNS LOGICAL
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

DEFINE VARIABLE cmbModus AS CHARACTER FORMAT "X(256)":U INITIAL "1" 
     LABEL "Modus" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Salg","1",
                     "Verksted","2",
                     "Nettbutikk","3",
                     "Kasse","5",
                     "<Alle>","9"
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE Strekkode AS CHARACTER FORMAT "X(256)":U 
     LABEL "Strekkode" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE RECTANGLE KOrdreFolder
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 146.4 BY 30.48.

DEFINE RECTANGLE KOrdreNavBrowse
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 58.8 BY 30.48.

DEFINE RECTANGLE KOrdreNavToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 15.4 BY .95.

DEFINE RECTANGLE rectWinToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 4.8 BY 1.

DEFINE RECTANGLE searchField
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 19 BY .95.

DEFINE BUTTON btnSplitBarX 
     IMAGE-UP FILE "bmp/tabright.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE .8 BY 30.24
     BGCOLOR 12 FGCOLOR 12 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     ButikkNr AT ROW 1.14 COL 102.6 COLON-ALIGNED HELP
          "Trykk ENTER for å skifte butikk"
     cmbModus AT ROW 1.14 COL 148 COLON-ALIGNED
     Strekkode AT ROW 1.14 COL 176.6 COLON-ALIGNED
     KOrdreFolder AT ROW 2.67 COL 61
     KOrdreNavBrowse AT ROW 2.67 COL 1.2
     KOrdreNavToolbar AT ROW 1.19 COL 22
     searchField AT ROW 1.19 COL 2.2
     rectWinToolBar AT ROW 1.19 COL 203.2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 207.8 BY 32.14.

DEFINE FRAME frSplitBarX
     btnSplitBarX AT ROW 1 COL 1.4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 60 ROW 2.62
         SIZE 3.8 BY 30.29.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window Template
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Registrering av kundeordre"
         HEIGHT             = 32.14
         WIDTH              = 207.8
         MAX-HEIGHT         = 57.14
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 57.14
         VIRTUAL-WIDTH      = 320
         RESIZE             = YES
         SCROLL-BARS        = NO
         STATUS-AREA        = YES
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = YES
         THREE-D            = YES
         MESSAGE-AREA       = NO
         SENSITIVE          = YES.
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
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 32.14
       FRAME DEFAULT-FRAME:WIDTH            = 207.8.

/* SETTINGS FOR FRAME frSplitBarX
                                                                        */
ASSIGN 
       btnSplitBarX:MOVABLE IN FRAME frSplitBarX          = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = YES.

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


&Scoped-define FRAME-NAME frSplitBarX
&Scoped-define SELF-NAME btnSplitBarX
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSplitBarX C-Win
ON END-MOVE OF btnSplitBarX IN FRAME frSplitBarX
DO:
  DYNAMIC-FUNCTION("ResizeKeepsWindowLocked",THIS-PROCEDURE:CURRENT-WINDOW) NO-ERROR.
  DYNAMIC-FUNCTION("setSplitBarX" , THIS-PROCEDURE:CURRENT-WINDOW, btnSplitBarX:HANDLE IN FRAME frSplitBarX,NO).
  RUN MoveToTop IN hCurrTabProc NO-ERROR.
  DYNAMIC-FUNCTION("ResizeKeepsWindowLocked",?) NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME ButikkNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ButikkNr C-Win
ON VALUE-CHANGED OF ButikkNr IN FRAME DEFAULT-FRAME /* Butikk */
DO:
  setBaseQuery().

  DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
  RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmbModus
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbModus C-Win
ON VALUE-CHANGED OF cmbModus IN FRAME DEFAULT-FRAME /* Modus */
DO:
  setBaseQuery().

  DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
  RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Strekkode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Strekkode C-Win
ON DELETE-CHARACTER OF Strekkode IN FRAME DEFAULT-FRAME /* Strekkode */
DO:
  APPLY LASTKEY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Strekkode C-Win
ON RETURN OF Strekkode IN FRAME DEFAULT-FRAME /* Strekkode */
DO:
  IF Strekkode:SCREEN-VALUE NE "" THEN 
    DYNAMIC-FUNCTION("setPreScanQuery",
                     "Strekkode WHERE kode = '" + Strekkode:SCREEN-VALUE + "'"
                   + ",FIRST ArtBas NO-LOCK OF Strekkode"
                   + ",EACH KOrdreLinje WHERE Varenr = STRING(ArtBas.ArtikkelNr) NO-LOCK"
                   + ",FIRST KOrdreHode NO-LOCK OF KOrdreLinje").
  ELSE
    DYNAMIC-FUNCTION("setPreScanQuery","").

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
  RUN Dummy.

  IF NOT DYNAMIC-FUNCTION("DoCleanUpObjects",THIS-PROCEDURE:CURRENT-WINDOW) THEN
    RETURN NO-APPLY.
  PUBLISH "InvalidateHandle" (THIS-PROCEDURE).
  DYNAMIC-FUNCTION("setCleanUpResize",THIS-PROCEDURE:CURRENT-WINDOW).
  RUN disable_UI.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

{incl/wintrigg.i}

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

    ASSIGN
        bTest = TRUE 
        cLogg = 'KOrdreUtlever' + REPLACE(STRING(TODAY),'/','')
        .

  RUN enable_UI.
  &IF DEFINED(UIB_is_Running) NE 0 &THEN
    RUN InitializeObject.
  &ENDIF
  
/*   DYNAMIC-FUNCTION("setDebugResize",YES).  */

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

{incl/conttrigg.i hCurrTabQuery}

ON 'window-resized':U OF {&WINDOW-NAME} DO:
  DYNAMIC-FUNCTION("ResizeKeepsWindowLocked",THIS-PROCEDURE:CURRENT-WINDOW) NO-ERROR.
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"Resize","").

  RUN MoveToTop IN hCurrTabProc NO-ERROR.
  DYNAMIC-FUNCTION("ResizeKeepsWindowLocked",?) NO-ERROR.
END.

ON 'alt-s':U OF FRAME {&FRAME-NAME} ANYWHERE DO:
  IF hCurrTabProc:FILE-NAME = "KOrdreView.w" THEN
    PUBLISH "AltSKundeOrdre" ("kundeordre").
END.

ON 'delete-character' OF FRAME {&FRAME-NAME} ANYWHERE 
  RETURN.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addFollowSplitBar C-Win 
PROCEDURE addFollowSplitBar :
/*------------------------------------------------------------------------------
  Purpose:    Legg til liste av handler som skal følge splitbar men som ikke 
              blir fanget av den automatiske rutinen (getWidgetsByLasso, se InitializeResize)
              Fill-ins og comboer er ikke med i utgangspunktet men de som skal resizes bør også følge splitbaren 
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihWindow       AS HANDLE NO-UNDO.
DEF INPUT PARAM icFollowSpList AS CHAR   NO-UNDO.

IF ihWindow NE THIS-PROCEDURE:CURRENT-WINDOW THEN RETURN.

cFollowSpList = icFollowSpList.

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
DYNAMIC-FUNCTION("DoLockWindow",THIS-PROCEDURE:CURRENT-WINDOW).
DYNAMIC-FUNCTION("setAttribute",SESSION,"userkeepswindowlocked","yes").
RUN SUPER.
IF NOT hBrowse:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN
  PUBLISH "HideOverlays" (THIS-PROCEDURE:CURRENT-WINDOW).


IF Strekkode:SCREEN-VALUE IN FRAME {&FRAME-NAME} NE "" THEN 
  PUBLISH "FinnStrekkode" (Strekkode:SCREEN-VALUE).
DYNAMIC-FUNCTION("setAttribute",SESSION,"userkeepswindowlocked","").
DYNAMIC-FUNCTION("DoLockWindow",?).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Dummy C-Win 
PROCEDURE Dummy :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR ix AS INT NO-UNDO.

ix = ix + 1.

RETURN.

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
  DISPLAY ButikkNr cmbModus Strekkode 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE KOrdreFolder KOrdreNavBrowse KOrdreNavToolbar searchField 
         rectWinToolBar ButikkNr cmbModus Strekkode 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  ENABLE btnSplitBarX 
      WITH FRAME frSplitBarX IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-frSplitBarX}
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

IF iCurrTab = 1 THEN
  DYNAMIC-FUNCTION("CreateParentLink",DYNAMIC-FUNCTION("getOrdreLinjeQuery" IN hCurrTabProc),hBrowse,"KOrdre_Id").

RUN SUPER.

hFlatView = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hToolBar,"flatviewhandle")) NO-ERROR.
IF NOT VALID-HANDLE(hFlatView) THEN RETURN.

hFlatBrw  = DYNAMIC-FUNCTION("getBrowseHandle" IN hFlatView).

DYNAMIC-FUNCTION("setAttribute",hFlatBrw,"availdistinctcolumns",
                 "Navn,KOrdre_Id,KundeNr,LevStatus,ProdStatus,ProduksjonsDato,AnsvVerksted,LeveringsDato,FakturertDato,Utsendelsesdato|Levert dato,RegistrertDato,RegistrertAv,Adresse1,DeresRef,VaarRef,Referanse,VerkstedMerknad,ButikkNr,VareNr,VareTekst").
DYNAMIC-FUNCTION("setAttribute",hFlatBrw,"availaccumfields",
                 "NettoLinjesum,SumEksMvaKr,DbKr").

IF iCurrTab = 1 THEN
  DYNAMIC-FUNCTION("DeleteObjectLink",DYNAMIC-FUNCTION("getOrdreLinjeQuery" IN hCurrTabProc),hBrowse).


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
DEFINE VARIABLE cButikslista AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iBrGrpNr AS INTEGER NO-UNDO.
DEFINE VAR bUserFilter AS LOG NO-UNDO.

DYNAMIC-FUNCTION("NewObject",SESSION,SESSION,"session").

SUBSCRIBE TO "addFollowSplitBar" ANYWHERE.

DO WITH FRAME {&FRAME-NAME}:
  DYNAMIC-FUNCTION("NewObject",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE,"container").

  
  cButikslista     = DYNAMIC-FUNCTION("getFieldValues","SysPara",
                                                      "WHERE SysHId = 19 and SysGr = 14 and ParaNr = 1",
                                                      "Parameter1").

  iNettButikk     = INT(DYNAMIC-FUNCTION("getFieldValues","SysPara",
                                                      "WHERE SysHId = 150 and SysGr = 1 and ParaNr = 2",
                                                      "Parameter1")).
  iApneNettbutikk = IF CAN-DO('1,J,Ja,Yes,True',
                              STRING(DYNAMIC-FUNCTION("getFieldValues","SysPara",
                                                      "WHERE SysHId = 19 and SysGr = 9 and ParaNr = 3",
                                                      "Parameter1"))
                             )
                      THEN 1
                      ELSE 0.
/*   DYNAMIC-FUNCTION("setAttribute",SESSION,"windowsbrowse","yes").  */

  DYNAMIC-FUNCTION("setAttribute",SESSION,"SeButikkNr",
                   DYNAMIC-FUNCTION("getFieldValues","Bruker","WHERE BrukerId = '" + DYNAMIC-FUNCTION("getASuserId") + "'","ButikkNr")).
  iBrGrpNr = DYNAMIC-FUNCTION("getFieldValues","Bruker","WHERE BrukerId = '" + DYNAMIC-FUNCTION("getASuserId") + "'","BrGrpNr").

  ButikkNr:DELIMITER = "|".
  IF cButikslista = "" THEN DO:
      IF INT(DYNAMIC-FUNCTION("getAttribute",SESSION,"SeButikkNr")) <> 0 THEN 
      DO:
        ASSIGN ButikkNr:LIST-ITEM-PAIRS = DYNAMIC-FUNCTION("getFieldList","butikktilgang,butiker;butnamn|butik;butik"
                                                   ,"WHERE brgrpnr = " + STRING(iBrGrpNr) + ", first Butiker where Butiker.Butik = ButikkTilgang.Butik")
               ButikkNr:SCREEN-VALUE = DYNAMIC-FUNCTION("getAttribute",SESSION,"SeButikkNr").
      END.
      ELSE DO: 
        ASSIGN ButikkNr:LIST-ITEM-PAIRS = RIGHT-TRIM("|0|" + DYNAMIC-FUNCTION("GetFieldList","Butiker;Butik|ButNamn;Butik","WHERE true BY ButNamn"),"|")
               ButikkNr:SCREEN-VALUE = DYNAMIC-FUNCTION("getAttribute",SESSION,"ButikkNr").
               
      END.
  END.
  ELSE
      ASSIGN ButikkNr:LIST-ITEM-PAIRS = "|0|" + cButikslista.
  IF iNettButikk > 0 AND LOOKUP(STRING(iNettButikk),ButikkNr:LIST-ITEM-PAIRS,'|') > 0 THEN
      ASSIGN ButikkNr:SCREEN-VALUE = STRING(iNettButikk) NO-ERROR.

  hBrowse = DYNAMIC-FUNCTION("NewBrowse",
                    KOrdreNavBrowse:HANDLE,   
                    100,                 
                    "MULTIPLE",                  
                    "KOrdreHode"
                    + ";Navn|Navn|x(20)@1"
                    + ";KOrdre_Id|Ordrenr|>>>>>>>>>>>>9@2"
                    + ";+LevFTekst|CHARACTER|x(15)|kordre_LevFTekst|Lev.måte@4"
                    + ";EkstOrdreNr|Ekst.ordrenr|x(15)@5"
                    + ";Totalt|Ordreverdi"
                    + ";KundeNr@23"
                    + ";ProduksjonsDato|Ferdig verksted@9"
                    + ";AnsvVerksted@10"
                    + ";LeveringsDato@11"
                    + ";ShipmentSendt@12"
                    + ";FakturertDato@13"
                    + ";Utsendelsesdato|Levert dato@14"
                    + ";LevStatus|O.stat|99@15"
                    + ";ProdStatus|P.stat|99@16"
                    + ";RegistrertDato@17"
                    + ";RegistrertAv@18"
                    + ";Adresse1@19"
                    + ";FaktPostNr|FaktPostNr" 
                    + ";FaktPoststed"
                    + ";DeresRef@20"  
                    + ";VaarRef@21"
                    + ";Referanse@22"
                    + ";VerkstedMerknad|Merknad@23"
                    + ";ButikkNr"
                    + ";Opphav"
                    + ";LevFNr"
                    + ";Sendingsnr"
                    + ";ReturNr"
                  + ",SysPara"
                    + ";Parameter1|Ordrestatus|x(12)@3"
                 + ",buf1_SysPara"
                   + ";Parameter1|Prod.status|x(12)@7"
                    ,"WHERE false"
                   + ",FIRST SysPara WHERE SysPara.SysHId = 19 and SysPara.SysGr = 1 AND SysPara.ParaNr = INT(KOrdreHode.LevStatus) OUTER-JOIN"
                   + ",FIRST buf1_SysPara WHERE buf1_SysPara.SysHId = 19 and buf1_SysPara.SysGr = 2 AND buf1_SysPara.ParaNr = INT(KOrdreHode.ProdStatus) OUTER-JOIN"
                    ,"SORT|KORdre_Id DESCENDING").

  DYNAMIC-FUNCTION("createObjectLink",hBrowse,THIS-PROCEDURE).

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcfieldproc","kordre_brwcalc.p").

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"filterlookupfields_KundeNr","Kunde;KundeNr;Navn").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"filterlookupquery_KundeNr","WHERE true").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"filterlookupreturnfield_KundeNr","KundeNr").

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"filterlookupfields_RegistrertAv","Bruker;BrukerId;Navn").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"filterlookupquery_RegistrertAv","WHERE true").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"filterlookupreturnfield_RegistrertAv","BrukerId").

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"filterlookupfields_AnsvVerksted","Bruker;BrukerId;Navn").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"filterlookupquery_AnsvVerksted","WHERE true").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"filterlookupreturnfield_AnsvVerksted","BrukerId").

  DYNAMIC-FUNCTION("setSortString",hBrowse,ENTRY(1,DYNAMIC-FUNCTION("getAttribute",hBrowse,"currviewfields"))).
/*   DYNAMIC-FUNCTION("setAttribute",hBrowse,"querysort",ENTRY(1,DYNAMIC-FUNCTION("getAttribute",hBrowse,"currviewfields"))). */
/*   DYNAMIC-FUNCTION("setAttribute",hBrowse,"localsort",ENTRY(1,DYNAMIC-FUNCTION("getAttribute",hBrowse,"currviewfields"))). */
  
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"filterdropdownvaluelist_LevStatus",DYNAMIC-FUNCTION("getFieldList","SysPara;ParaNr|Parameter1;ParaNr","WHERE SysHId = 19 and SysGr = 1")).
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"filterdropdownvaluelist_ProdStatus",DYNAMIC-FUNCTION("getFieldList","SysPara;ParaNr|Parameter1;ParaNr","WHERE SysHId = 19 and SysGr = 2")).

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"getrecordcount","yes").
  
  hSearchField = DYNAMIC-FUNCTION("NewBrowseSearchField",SearchField:HANDLE,hBrowse,1).
  DYNAMIC-FUNCTION("CreateObjectLink",hSearchField,hBrowse).

  hTabFolder = DYNAMIC-FUNCTION("NewTabFolder",KOrdreFolder:HANDLE).
  
  /* X and Y limits for move of widget are not yet set for the window. 
     Since we want the tabs to resize according to the window size these values must be set here and
     they must be exact the same as in setOrwWinSize (see InitResize function) 
     Here the values are set to not give any automatic move of widgets */
     
  DYNAMIC-FUNCTION("setMinXYmove",2000,1200). 

  DYNAMIC-FUNCTION("InitPages" IN hTabFolder,"Ordre|KOrdreView.w|Kunderegister|KundeView.w|Notater kunde|Kundekommentar.w|Faktura|Faktura.w|Reskontro|Kundereskontr.w|Kundetranser|KundeTrans.w",hBrowse).
  
  DYNAMIC-FUNCTION("buildFolder" IN hTabFolder).

  bUserFilter = DYNAMIC-FUNCTION("LoadUserFilter",hBrowse,THIS-PROCEDURE).

  hToolbar = DYNAMIC-FUNCTION("NewToolBar",
                    KOrdreNavToolbar:HANDLE,             
                    "Fil",                          
                    "filter,excel,FlatView,BrowseConfig;Kolonneoppsett,Refresh,PostPakke,Pakkseddel,Lever"    
                  + ",|Innstillinger"
                    ,"maxborder").

  DYNAMIC-FUNCTION("createObjectLink",hBrowse,hToolbar).

  hWinToolbar = DYNAMIC-FUNCTION("NewToolBar",
                    rectWinToolBar:HANDLE,             /* Rectangle to define coordinates for toolbar */
                    "Fil",                          /* Corresponding menu label - no menu if blank */
                    "Close;Avslutt,Help|Hjelp",
                                                    /* Buttons / Menu items: action;label;tooltip;Method;image,action;label.. 
                                                       Any number of properties accepted (one ok - if predef. action) */
                    "enable,right").                        /* Misc - for something I might need in next version.. */

  InitializeResize().
END.

DYNAMIC-FUNCTION("TabFolderChanged" IN hTabFolder,1).

IF iApneNettbutikk = 1 THEN
    ASSIGN cmbModus:SCREEN-VALUE = '3'.

setBaseQuery().
IF bUserFilter THEN 
  RUN InvokeMethod(hBrowse,"OpenQuery").
ELSE
  DYNAMIC-FUNCTION("InitDynFilter",hBrowse,"LevStatus,Levstatus",">=,<=","10|40","try").

DYNAMIC-FUNCTION("initTranslation",THIS-PROCEDURE:CURRENT-WINDOW).

APPLY "entry" TO hSearchField.

THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = FALSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LeverRecord C-Win 
PROCEDURE LeverRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iReturn     AS INT  NO-UNDO.
DEF VAR ocValue     AS CHAR NO-UNDO.
DEF VAR cStatusList AS CHAR NO-UNDO.
DEF VAR cRowIdList  AS CHAR NO-UNDO.

  RUN JBoxBrowseMsgUpdateVal.w ("Levere kundeordre ?",
                                hBrowse:NUM-SELECTED-ROWS,
                                IF INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount")) LT INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"rowstobatch")) THEN
                                  INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount"))
                                ELSE 99999,
                                "",
                                OUTPUT ocValue, 
                                OUTPUT iReturn).

IF bTest THEN 
DO:
    RUN Bibl_LoggDbFri.p(cLogg,'Start kundeordre.w -LeverRecord').
    RUN Bibl_LoggDbFri.p(cLogg,'    iReturn: ' + STRING(iReturn) + '.').
END.

IF iReturn = 1 THEN DO:
    MESSAGE 'Marker en eller flere ordre for utskrift. Maks 20.'
    VIEW-AS ALERT-BOX.
    RETURN.
    
/*    bOk = DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"kordrehode_lever.p",'').                                           */
/*                                                                                                                      */
/*    IF NOT bOK THEN                                                                                                   */
/*      DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil i sending av informasjon ","").*/
END.
ELSE IF iReturn = 2 THEN
DO:
    IF hBrowse:NUM-SELECTED-ROWS > 20 THEN 
    DO:
        MESSAGE "Maks 20 ordre kan velges for utskrift på en gang."
        VIEW-AS ALERT-BOX.
        RETURN.
    END.
    
    
    IF NOT DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowse,"kordrehode_lever.p",'') THEN
      DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil i sending av informasjon ","").
    ELSE
        RUN InvokeMethod(hBrowse,"OpenQuery").
END.
ELSE
  LEAVE.

IF bTest THEN 
DO:
    RUN Bibl_LoggDbFri.p(cLogg,'Ferdig kundeordre.w - LeverRecord').
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
{&WINDOW-NAME}:WINDOW-STATE = 3.
{&WINDOW-NAME}:MOVE-TO-TOP().
APPLY "entry" TO hSearchField.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PakkseddelRecord C-Win 
PROCEDURE PakkseddelRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iReturn     AS INT  NO-UNDO.
DEF VAR ocValue     AS CHAR NO-UNDO.
DEF VAR cStatusList AS CHAR NO-UNDO.
DEF VAR cRowIdList  AS CHAR NO-UNDO.

  RUN JBoxBrowseMsgUpdateVal.w ("Skrive pakkseddel ?",
                                hBrowse:NUM-SELECTED-ROWS,
                                IF INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount")) LT INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"rowstobatch")) THEN
                                  INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount"))
                                ELSE 99999,
                                "",
                                OUTPUT ocValue, 
                                OUTPUT iReturn).

IF iReturn = 1 THEN
DO:
    MESSAGE 'Marker en eller flere ordre for utskrift. Maks 20.'
    VIEW-AS ALERT-BOX.
    RETURN.
    
/*  bOk = DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"kordrehode_pakkseddel.p",'').*/
END.
ELSE IF iReturn = 2 THEN
DO:
    IF hBrowse:NUM-SELECTED-ROWS > 20 THEN 
    DO:
        MESSAGE "Maks 20 ordre kan velges for utskrift på en gang."
        VIEW-AS ALERT-BOX.
        RETURN.
    END.
 
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN
      cRowIdList = cRowIdList + hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('RowIdent1'):BUFFER-VALUE + ','.
  END.
  bOk = DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowse,"kordrehode_pakkseddel.p",'').
END.
ELSE
  LEAVE.
IF NOT bOK THEN
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil i sending av informasjon ",""). 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PlukklisteRecord C-Win 
PROCEDURE PlukklisteRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iReturn     AS INT  NO-UNDO.
DEF VAR ocValue     AS CHAR NO-UNDO.
DEF VAR cStatusList AS CHAR NO-UNDO.
DEF VAR cRowIdList  AS CHAR NO-UNDO.

  RUN JBoxBrowseMsgUpdateVal.w ("Skrive ut plukkliste ?",
                                hBrowse:NUM-SELECTED-ROWS,
                                IF INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount")) LT INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"rowstobatch")) THEN
                                  INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount"))
                                ELSE 99999,
                                "",
                                OUTPUT ocValue, 
                                OUTPUT iReturn).

IF iReturn = 1 THEN
  bOk = DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"kordrehode_plukkliste.p",'').
ELSE IF iReturn = 2 THEN
DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN
      cRowIdList = cRowIdList + hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('RowIdent1'):BUFFER-VALUE + ','.
  END.
  bOk = DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowse,"kordrehode_plukkliste.p",'').
END.
ELSE
  LEAVE.
IF NOT bOK THEN
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil i sending av informasjon ",""). 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PostPakkeRecord C-Win 
PROCEDURE PostPakkeRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iReturn     AS INT  NO-UNDO.
DEF VAR ocValue     AS CHAR NO-UNDO.
DEF VAR cStatusList AS CHAR NO-UNDO.
DEF VAR cRowIdList  AS CHAR NO-UNDO.

  RUN JBoxBrowseMsgUpdateVal.w ("Skrive ut postpakke etikett ?",
                                hBrowse:NUM-SELECTED-ROWS,
                                IF INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount")) LT INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"rowstobatch")) THEN
                                  INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount"))
                                ELSE 99999,
                                "",
                                OUTPUT ocValue, 
                                OUTPUT iReturn).
IF iReturn = 1 THEN
DO:
    MESSAGE 'Marker en eller flere ordre for utskrift. Maks 20.'
    VIEW-AS ALERT-BOX.
    RETURN.
    
    
/*  bOk = DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"kordrehode_postpakke.p",'').*/
END.
ELSE IF iReturn = 2 THEN
DO:
    IF hBrowse:NUM-SELECTED-ROWS > 20 THEN 
    DO:
        MESSAGE "Maks 20 ordre kan velges for utskrift på en gang."
        VIEW-AS ALERT-BOX.
        RETURN.
    END.
    
    
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN
      cRowIdList = cRowIdList + hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('RowIdent1'):BUFFER-VALUE + ','.
  END.
  bOk = DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowse,"kordrehode_postpakke.p",'').
END.
ELSE
  LEAVE.
IF NOT bOK THEN
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil i sending av informasjon ",""). 


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RefreshCurrentRow C-Win 
PROCEDURE RefreshCurrentRow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
APPLY "value-changed" TO hBrowse.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RefreshRecord C-Win 
PROCEDURE RefreshRecord :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/

DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
RUN SUPER.

RUN OpenQuery.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RowDisplayBrowse C-Win 
PROCEDURE RowDisplayBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE cGave AS CHARACTER   NO-UNDO.
DEFINE VARIABLE dKOrdre_Id AS DECI   NO-UNDO.
DEFINE VARIABLE iLevFnr AS INTEGER     NO-UNDO.
    dKOrdre_Id = hbfKOrdre_Id:BUFFER-VALUE.
    cGave   = DYNAMIC-FUNCTION("getFieldValues","KOrdreHode","WHERE KOrdre_id = " + STRING(dKOrdre_Id),"cOpt1").
    iLevFnr = INT(DYNAMIC-FUNCTION("getFieldValues","KOrdreHode","WHERE KOrdre_id = " + STRING(dKOrdre_Id),"LevFnr")).
    IF cGave <> "" OR iLevFnr = 8 THEN DO:
        hbcKOrdre_id:BGCOLOR = IF cGave = "" THEN 10 ELSE IF iLevFnr <> 8 THEN 14 ELSE 12.
                                 /* Gul */                   /* Grön */           /* Båda = röd */
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TabFromBrowse C-Win 
PROCEDURE TabFromBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DYNAMIC-FUNCTION("setReturnNoApplyMethod","MoveToTop",hCurrTabProc).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AdjustBrowseColumns C-Win 
FUNCTION AdjustBrowseColumns RETURNS LOGICAL
  ( INPUT ihBrowse     AS HANDLE,
    INPUT icBrowseName AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEFINE VARIABLE ix AS INTEGER     NO-UNDO.
DO ix = 1 TO ihBrowse:NUM-COLUMNS:
    IF ihBrowse:GET-BROWSE-COLUMN(ix):NAME = "KOrdre_Id" THEN
        hbcKOrdre_Id = ihBrowse:GET-BROWSE-COLUMN(ix).
        hbfKOrdre_Id = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('KOrdre_Id').

END.
/*
ASSIGN ihBrowse:GET-BROWSE-COLUMN(1):WIDTH-PIXELS = 100
       ihBrowse:GET-BROWSE-COLUMN(2):WIDTH-PIXELS = 60.
*/
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getButikkNr C-Win 
FUNCTION getButikkNr RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN ButikkNr:SCREEN-VALUE IN FRAME {&FRAME-NAME}.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getKundeNr C-Win 
FUNCTION getKundeNr RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF hBrowse:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN
  RETURN hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("KundeNr"):BUFFER-VALUE.
ELSE
  RETURN 0.00.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
                    TRIM(
                    DYNAMIC-FUNCTION("getWidgetsByLasso",KOrdreNavBrowse:HANDLE IN FRAME {&FRAME-NAME},0,
                                     "frame,control-frame,rectangle,browse") + "," +
                    DYNAMIC-FUNCTION("getWidgetsByLasso",KOrdreFolder:HANDLE IN FRAME {&FRAME-NAME},0,
                                     "frame,browse,control-frame,rectangle,editor") + "," +
                    cFollowSpList
                    ,",")
                    ).

  DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "KOrdreNavBrowse,searchField," + hBrowse:NAME).
  DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "KOrdreNavToolbar").
  DYNAMIC-FUNCTION("setAddMoveX",  THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, 
                                   "Strekkode," 
                                 + DYNAMIC-FUNCTION("getToolbarNames",hWinToolbar,"button")
                                   ).

  DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,2000,1200,0,550).
END.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setBaseQuery C-Win 
FUNCTION setBaseQuery RETURNS LOGICAL
  (  ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"basequery",
                    (IF ButikkNr:SCREEN-VALUE NE ? AND ButikkNr:SCREEN-VALUE NE "" THEN
                      "WHERE ButikkNr = " + ButikkNr:SCREEN-VALUE
                     ELSE "WHERE true")
/*                   + (IF cmbModus:SCREEN-VALUE = "2" THEN                                     */
/*                       " AND KasseNr = " + DYNAMIC-FUNCTION("getAttribute",SESSION,"SEKASSE") */
/*                      ELSE "")                                                                */
                  + (IF cmbModus:SCREEN-VALUE = "1" THEN 
                      " AND Verkstedordre = false and Opphav = 1"
                     ELSE "")
                  + (IF cmbModus:SCREEN-VALUE = "2" THEN 
                      " AND Verkstedordre and (Opphav = 1 or Opphav = 5)"
                     ELSE "")
                  + (IF cmbModus:SCREEN-VALUE = "3" THEN 
                      " AND Opphav = 10"
                     ELSE "")
                  + (IF cmbModus:SCREEN-VALUE = "9" THEN 
                      ""
                     ELSE "")
                    ).
END.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setFocus C-Win 
FUNCTION setFocus RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
APPLY "entry" TO hBrowse.

RETURN YES.

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
ELSE
  DYNAMIC-FUNCTION("DeleteObjectLink",hBrowse,DYNAMIC-FUNCTION("getPageQuery" IN hTabFolder,2)).

ASSIGN hCurrTabQuery = DYNAMIC-FUNCTION("getPageQuery" IN hTabFolder,iiTab)
       hCurrTabProc  = DYNAMIC-FUNCTION("getPageHandle" IN hTabFolder,iiTab)
       hCurrTabFrame = DYNAMIC-FUNCTION("getPageFrame" IN hTabFolder,iiTab)
       iCurrTab      = iiTab.


IF iiTab = 1 THEN
  DYNAMIC-FUNCTION("CreateOneToOneLink",hCurrTabQuery,hBrowse,"KOrdre_id").
ELSE IF CAN-DO("Kundereskontr.w,KundeView.w,Faktura.w,KundeTrans.w,Kundekommentar.w",hCurrTabProc:FILE-NAME) THEN
  DYNAMIC-FUNCTION("CreateParentLink",hCurrTabQuery,hBrowse,"KundeNr").
ELSE 
  DYNAMIC-FUNCTION("CreateParentLink",hCurrTabQuery,hBrowse,"KOrdre_id").

IF CAN-DO(hCurrTabProc:INTERNAL-ENTRIES,"setFilter") THEN 
  DYNAMIC-FUNCTION("setFilter" IN hCurrTabProc).
IF CAN-DO("Faktura.w,KOrdre.w,Kundereskontr.w",hCurrTabProc:FILE-NAME) THEN 
  DYNAMIC-FUNCTION("TabChanged" IN hCurrTabProc,1).

DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
RUN DisplayRecord.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

