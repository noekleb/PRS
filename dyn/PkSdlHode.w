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

DEF VAR bOK              AS LOG NO-UNDO.
DEF VAR ix               AS INT NO-UNDO.

DEF VAR hToolbar         AS HANDLE NO-UNDO.
DEF VAR hBrowse          AS HANDLE NO-UNDO.
DEF VAR hFieldMap          AS HANDLE NO-UNDO.
DEF VAR hTabFolder       AS HANDLE NO-UNDO.
DEF VAR hWinToolbar      AS HANDLE NO-UNDO.
 
DEF VAR hCurrTabProc     AS HANDLE NO-UNDO.
DEF VAR hCurrTabFrame    AS HANDLE NO-UNDO.
DEF VAR hCurrTabQuery    AS HANDLE NO-UNDO.
DEF VAR iCurrTab         AS INT    NO-UNDO.

DEF VAR cFollowSpList    AS CHAR   NO-UNDO.
DEF VAR cNotFollowSpList AS CHAR   NO-UNDO.
DEF VAR hEtikettVindu    AS HANDLE NO-UNDO.
DEF VAR hSourceBrowse    AS HANDLE NO-UNDO.
DEF VAR bOverstyrKol     AS LOG    NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS PkSdlFolder PkSdlNavToolbar rectWinToolBar ~
SendtDato EkstId PkSdlNr Parameter1 MeldingFraLev 
&Scoped-Define DISPLAYED-OBJECTS SendtDato EkstId PkSdlNr Parameter1 ~
MeldingFraLev 

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getOverstyrKol C-Win 
FUNCTION getOverstyrKol RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getParentBrowse C-Win 
FUNCTION getParentBrowse RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTabFolderHandle C-Win 
FUNCTION getTabFolderHandle RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD InitializeResize C-Win 
FUNCTION InitializeResize RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setFocus C-Win 
FUNCTION setFocus RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setTab C-Win 
FUNCTION setTab RETURNS LOGICAL
  ( INPUT iiTab AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SkrivEtikett C-Win 
FUNCTION SkrivEtikett RETURNS LOGICAL
  ( INPUT icListe AS CHAR )  FORWARD.

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
DEFINE VARIABLE EkstId AS CHARACTER FORMAT "X(15)" 
     LABEL "Ekst.ordre" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE MeldingFraLev AS CHARACTER FORMAT "X(256)" 
     LABEL "Lev.mrk" 
     VIEW-AS FILL-IN 
     SIZE 20.4 BY 1.

DEFINE VARIABLE Parameter1 AS CHARACTER FORMAT "X(256)":U 
     LABEL "St" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE PkSdlNr AS CHARACTER FORMAT "X(20)" 
     LABEL "Pk.sdl.nr" 
     VIEW-AS FILL-IN 
     SIZE 17.2 BY 1.

DEFINE VARIABLE SendtDato AS DATE FORMAT "99/99/99" 
     LABEL "Sendt" 
     VIEW-AS FILL-IN 
     SIZE 13.4 BY 1.

DEFINE RECTANGLE PkSdlFolder
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 195 BY 27.14.

DEFINE RECTANGLE PkSdlNavToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 15.4 BY .95.

DEFINE RECTANGLE rectWinToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 4.8 BY 1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     SendtDato AT ROW 1.24 COL 55 COLON-ALIGNED HELP
          "Dato da varene er sendt fra leverandør."
     EkstId AT ROW 1.24 COL 83 COLON-ALIGNED HELP
          "Kobllingsfelt for å koble til ekstern ordre."
     PkSdlNr AT ROW 1.24 COL 111 COLON-ALIGNED HELP
          "Pakkseddelnummer"
     Parameter1 AT ROW 1.24 COL 132 COLON-ALIGNED
     MeldingFraLev AT ROW 1.24 COL 162.2 COLON-ALIGNED HELP
          "Merknad fra leverandør."
     PkSdlFolder AT ROW 2.43 COL 2
     PkSdlNavToolbar AT ROW 1.24 COL 2.8
     rectWinToolBar AT ROW 1.29 COL 191.4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 196.6 BY 28.81.


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
         TITLE              = "Mottakshåndtering"
         HEIGHT             = 28.71
         WIDTH              = 196.6
         MAX-HEIGHT         = 57.14
         MAX-WIDTH          = 384
         VIRTUAL-HEIGHT     = 57.14
         VIRTUAL-WIDTH      = 384
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
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 28.81
       FRAME DEFAULT-FRAME:WIDTH            = 196.6.

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

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Mottakshåndtering */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Mottakshåndtering */
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

DYNAMIC-FUNCTION("EmbedMe" IN SOURCE-PROCEDURE,THIS-PROCEDURE) NO-ERROR.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
  IF NOT DYNAMIC-FUNCTION("DoCleanUpObjects",THIS-PROCEDURE:CURRENT-WINDOW) THEN
    RETURN NO-APPLY "cancel".
  PUBLISH "InvalidateHandle" (THIS-PROCEDURE).
  DYNAMIC-FUNCTION("setCleanUpResize",THIS-PROCEDURE:CURRENT-WINDOW).
  IF VALID-HANDLE(hEtikettVindu) THEN APPLY "close" TO hEtikettVindu.
  RUN disable_UI.
  RETURN.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

{incl/wintrigg.i}

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

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
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"Resize","").

  RUN MoveToTop IN hCurrTabProc NO-ERROR.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DeleteRecord C-Win 
PROCEDURE DeleteRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  RUN SUPER.

  /* Hvordan gjøres dette ?????? */
  /* Slår av alarmknappen        */
  FIND FIRST PkSdlHode NO-LOCK WHERE
      PkSdlHode.PkSdlStatus = 10 NO-ERROR.
  IF NOT AVAILABLE PkSdlHode THEN
      PUBLISH "setViewBtnEvents" (FALSE).

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
PUBLISH "setPkslButikkListe" (IF hBrowse:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("PkSdlId"):BUFFER-VALUE ELSE 0).
RUN SUPER.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EditPkSdl C-Win 
PROCEDURE EditPkSdl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT  PARAM ihWindow   AS HANDLE NO-UNDO.
DEF OUTPUT PARAM bEditPkSdl AS LOG    NO-UNDO.

IF ihWindow = THIS-PROCEDURE:CURRENT-WINDOW THEN
  bEditPkSdl = hFieldMap:AVAIL AND hFieldMap:BUFFER-FIELD("PkSdlStatus"):BUFFER-VALUE LE 10.

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
  DISPLAY SendtDato EkstId PkSdlNr Parameter1 MeldingFraLev 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE PkSdlFolder PkSdlNavToolbar rectWinToolBar SendtDato EkstId PkSdlNr 
         Parameter1 MeldingFraLev 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EtikettRecord C-Win 
PROCEDURE EtikettRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF NOT hFieldMap:AVAIL THEN RETURN.

/* IF NOT DYNAMIC-FUNCTION("runproc","pksdl_etiketter.p",STRING(hFieldMap:BUFFER-FIELD("PkSdlId"):BUFFER-VALUE)  */
/*                                                   + ";" + DYNAMIC-FUNCTION("getASuserId")                     */
/*                                                     ,?) THEN DO:                                              */
/*   DYNAMIC-FUNCTION("DoMessage",0,0,                                                                           */
/*                    DYNAMIC-FUNCTION("getTransactionMessage"),"","").                                          */
/* END.                                                                                                          */
/* ELSE                                                                                                          */
/*   SkrivEtikett(DYNAMIC-FUNCTION("getTransactionMessage")).                                                    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getPkSdlButNr C-Win 
PROCEDURE getPkSdlButNr :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER piPkSdlButNr AS INTEGER NO-UNDO.
    
    DEFINE VARIABLE piPkSdlId LIKE PkSdlHode.PkSdlId NO-UNDO.
    
    ASSIGN 
        piPkSdlId = (IF hBrowse:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN 
                        hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("PkSdlId"):BUFFER-VALUE
                        ELSE 0)
        .
    IF piPkSdlId <> 0 THEN 
    DO:
        FIND FIRST PkSdlLinje NO-LOCK WHERE 
            PkSdlLinje.PkSdlId = piPkSdlId NO-ERROR.
        IF AVAILABLE PkSdlLinje THEN 
            ASSIGN 
                piPkSdlButNr = PkSdlLinje.ButikkNr
                .
    END.

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
DEF VAR hTab1       AS HANDLE NO-UNDO.

DYNAMIC-FUNCTION("NewObject",SESSION,SESSION,"session").

SUBSCRIBE TO "EditPkSdl"     ANYWHERE.
SUBSCRIBE TO "getPkSdlButNr" ANYWHERE.
  
DO WITH FRAME {&FRAME-NAME}:
  DYNAMIC-FUNCTION("NewObject",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE,"container").

  bOverstyrKol = LOGICAL(DYNAMIC-FUNCTION("getFieldValues","syspara",
                                          "WHERE SysPara.SysHId = 5 AND SysPara.SysGr = 27 and SysPara.ParaNr = 1",
                                          "Parameter1")) NO-ERROR.
  IF bOverstyrKol = ? THEN bOverstyrKol = YES.

  IF SEARCH("controls.dll") NE ? THEN
    DYNAMIC-FUNCTION("setAttribute",PkSdlFolder:HANDLE,"tabFolderProg","JBoxJlwTabFolder.w").
  hTabFolder = DYNAMIC-FUNCTION("NewTabFolder",PkSdlFolder:HANDLE).
  
  /* X and Y limits for move of widget are not yet set for the window. 
     Since we want the tabs to resize according to the window size these values must be set here and
     they must be exact the same as in setOrwWinSize (see InitResize function) 
     Here the values are set to not give any automatic move of widgets */
     
  DYNAMIC-FUNCTION("setMinXYmove",2000,1200). 
/*   IF SEARCH("controls.dll") NE ? THEN                                                                      */
/*     DYNAMIC-FUNCTION("setImageList" IN hTabFolder,"newbmp\debt.bmp;newbmp\accept.bmp;newbmp\account.bmp"). */

  DYNAMIC-FUNCTION("InitPages" IN hTabFolder,"Pakksedler|PkSdlBrw.w|Varer|PkSdlLinje.w|Priser|PkSdlPris.w",hBrowse).  /* |Detaljer|PkSdlView.w */

  DYNAMIC-FUNCTION("buildFolder" IN hTabFolder).

  hBrowse = DYNAMIC-FUNCTION("getPageQuery" IN hTabFolder,1).
  DYNAMIC-FUNCTION("setParentQuery" IN DYNAMIC-FUNCTION("getPageHandle" IN hTabFolder,2),hBrowse).
  DYNAMIC-FUNCTION("setParentQuery" IN DYNAMIC-FUNCTION("getPageHandle" IN hTabFolder,3),hBrowse).
/*   DYNAMIC-FUNCTION("setParentQuery" IN DYNAMIC-FUNCTION("getPageHandle" IN hTabFolder,4),hBrowse).  */
  hTab1 = DYNAMIC-FUNCTION("getPageHandle" IN hTabFolder,1).

  hWinToolbar = DYNAMIC-FUNCTION("NewToolBar",
                    rectWinToolBar:HANDLE,      
                    "Fil",                      
                    "Close;Avslutt"
                  + ",|Innstillinger"
                  + ",Help|Hjelp",
                    "enable,right").

  hToolbar = DYNAMIC-FUNCTION("NewToolBar",
                    PkSdlNavToolbar:HANDLE,             
                    "", 
                    "Print,Delete;Slett"
                  + ",-,First,Prev,Next,Last"
                    ,"maxborder").

  DYNAMIC-FUNCTION("createObjectLink",hBrowse,hToolbar).

  DYNAMIC-FUNCTION("AddToToolbar" IN hTab1,hToolbar).

  hFieldMap = DYNAMIC-FUNCTION("NewFieldMap"
            ,hBrowse:QUERY
            ,FRAME {&FRAME-NAME}:HANDLE
            ,"",""
            ,"PkSdlNr,SendtDato,EkstId,Parameter1,MeldingFraLev",""
            ,"").
  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"customDeleteValProc","=pksdlhode_delete.p").
  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hFieldMap).

  IF NOT bOverstyrKol THEN
    ASSIGN /*
           SendtDato:HIDDEN     = YES
           PkSdlNr:HIDDEN       = YES
           */
           Parameter1:HIDDEN    = YES
           MeldingFraLev:HIDDEN = YES
           .
  InitializeResize().

  DYNAMIC-FUNCTION("initTranslation",THIS-PROCEDURE:CURRENT-WINDOW).

END.

&IF DEFINED(UIB_is_Running) NE 0 &THEN
  THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = FALSE.
&ENDIF

DYNAMIC-FUNCTION("TabFolderChanged" IN hTabFolder,1).

DYNAMIC-FUNCTION("setAttribute",hBrowse,"basequery","").
RUN StartQuery IN hTab1.
/*
RUN MoveToTop IN hTab1.
*/

APPLY "value-change" TO hBrowse.

/* DYNAMIC-FUNCTION("InitDynFilter",hBrowse,"PkSdlStatus,PkSdlStatus",">=,<=","10|10","try").  */

/* APPLY "end-move" TO btnSplitBarX IN FRAME frSplitBarX.  */

/* APPLY "entry" TO hSearchField.  */

/* DYNAMIC-FUNCTION("TabFolderChanged" IN hTabFolder,1). */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InnlevRecord C-Win 
PROCEDURE InnlevRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR cMsg AS CHAR NO-UNDO.

/* IF DYNAMIC-FUNCTION("DoMessage",0,4,"Registrer innleveranse og prisoppdatering for varer (husk kontroll av antall og pris først)?","","") = 6 THEN DO:  */
/*   PUBLISH "InnlevRecord". /* <- denne finnes i pksdllinje.w */                                                                                          */
/*   IF RETURN-VALUE NE "Error" THEN DO:                                                                                                                   */
/*     SkrivEtikett(RETURN-VALUE).                                                                                                                         */
/*                                                                                                                                                         */
/*     DYNAMIC-FUNCTION("RefreshRowids",hBrowse,hFieldMap:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE).                                                         */
/*     APPLY "value-changed" TO hBrowse.                                                                                                                   */
/*                                                                                                                                                         */
/*     IF VALID-HANDLE(hSourceBrowse) THEN APPLY "value-changed" TO hSourceBrowse.                                                                         */
/*   END.                                                                                                                                                  */
/* END.                                                                                                                                                    */
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
THIS-PROCEDURE:CURRENT-WINDOW:VISIBLE = YES.
{&WINDOW-NAME}:WINDOW-STATE = 3.
{&WINDOW-NAME}:MOVE-TO-TOP().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrintRecord C-Win 
PROCEDURE PrintRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN skrivpakkseddel.p (hFieldMap:BUFFER-FIELD("PkSdlId"):STRING-VALUE + "|",FALSE,"",1,"",1).

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setQuery C-Win 
PROCEDURE setQuery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icCrit         AS CHAR NO-UNDO.
DEF INPUT PARAM ihSourceBrowse AS HANDLE NO-UNDO.

hSourceBrowse = ihSourceBrowse.

DYNAMIC-FUNCTION("setAttribute",hBrowse,"querywhere",
                 (IF DYNAMIC-FUNCTION("getAttribute",hBrowse,"basequery") NE "" 
                    OR DYNAMIC-FUNCTION("getAttribute",hBrowse,"queryfilter") NE "" THEN
                   " AND "
                  ELSE "WHERE ") +
                  "PkSdlId = " + icCrit).

DYNAMIC-FUNCTION("TabFolderChanged" IN hTabFolder,1).
DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
RUN OpenQuery.

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
ASSIGN ihBrowse:GET-BROWSE-COLUMN(1):WIDTH-PIXELS = 80
       ihBrowse:GET-BROWSE-COLUMN(2):WIDTH-PIXELS = 60
       ihBrowse:GET-BROWSE-COLUMN(3):WIDTH-PIXELS = 18
       .

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getOverstyrKol C-Win 
FUNCTION getOverstyrKol RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN bOverstyrKol.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getParentBrowse C-Win 
FUNCTION getParentBrowse RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN hBrowse.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTabFolderHandle C-Win 
FUNCTION getTabFolderHandle RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN hTabFolder.   /* Function return value. */

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
DEF VAR cRightSpList    AS CHAR NO-UNDO.
DEF VAR cNewRightSpList AS CHAR NO-UNDO.


DO WITH FRAME {&FRAME-NAME}:
  cRightSpList = DYNAMIC-FUNCTION("getWidgetsByLasso",PkSdlFolder:HANDLE IN FRAME {&FRAME-NAME},0,
                                     "frame,browse,control-frame,rectangle,editor").
  DO ix = 1 TO NUM-ENTRIES(cRightSpList):
    IF NOT CAN-DO(cNotFollowSpList,ENTRY(ix,cRightSpList)) THEN
      cNewRightSpList = cNewRightSpList + ENTRY(ix,cRightSpList) + ",".
  END.
  cNewRightSpList = TRIM(cNewRightSpList,",").

  DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "searchField").
  DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "PkSdlNavToolbar").
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setTab C-Win 
FUNCTION setTab RETURNS LOGICAL
  ( INPUT iiTab AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF iiTab NE 0 THEN
  DYNAMIC-FUNCTION("TabFolderChanged" IN hTabFolder,iiTab).

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SkrivEtikett C-Win 
FUNCTION SkrivEtikett RETURNS LOGICAL
  ( INPUT icListe AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cArtikkelEti    AS CHAR NO-UNDO.
DEF VAR cEtiketter      AS CHAR NO-UNDO.
DEF VAR cAntallEti      AS CHAR NO-UNDO.
DEF VAR cIndividNr      AS CHAR NO-UNDO.
DEF VAR cPris           AS CHAR NO-UNDO.
DEF VAR iCount          AS INT  NO-UNDO.
DEF VAR cArtEANlist     AS CHAR NO-UNDO.
DEF VAR cArtANTlist     AS CHAR NO-UNDO.
DEF VAR cArtINDlist     AS CHAR NO-UNDO.
DEF VAR cArtPRISList    AS CHAR NO-UNDO.
DEF VAR iTotAnt         AS INT  NO-UNDO.

IF NUM-ENTRIES(icListe,"|") < 5 THEN RETURN FALSE.

ASSIGN icListe      = SUBSTR(icListe,8)
       cArtikkelEti = ENTRY(1,icListe,"|")
       cEtiketter   = ENTRY(2,icListe,"|")
       cAntallEti   = ENTRY(3,icListe,"|")
       cIndividNr   = ENTRY(4,icListe,"|")
       cPris        = ENTRY(5,icListe,"|")
       .

/* MESSAGE                                */
/*     "cArtikkelEti"  cArtikkelEti SKIP  */
/*     "cEtiketter  "  cEtiketter   SKIP  */
/*     "cAntallEti  "  cAntallEti   SKIP  */
/*     "cIndividNr  "  cIndividNr   SKIP  */
/*     "cPris"         cPris SKIP         */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK. */

IF cArtikkelEti <> "" THEN DO:
  DO ix = 1 TO NUM-ENTRIES(cArtikkelEti,CHR(1)):
    ASSIGN cArtEANlist  = ENTRY(ix,cEtiketter,CHR(1))
           cArtANTlist  = ENTRY(ix,cAntallEti,CHR(1))
           cArtINDlist  = ENTRY(ix,cIndividNr,CHR(1))
           cArtPRISList = ENTRY(ix,cPris,CHR(1))
           .
    IF cArtEANlist <> "" THEN 
      DO iCount = 1 TO NUM-ENTRIES(cArtANTlist):
        IF ENTRY(iCount,cArtEANlist) <> "" AND INT(ENTRY(iCount,cArtANTlist)) > 0 THEN
          iTotAnt = iTotAnt + INT(ENTRY(iCount,cArtANTlist)).
      END.
  END.

  IF iTotAnt LE 0 THEN RETURN FALSE.

  DO ix = 1 TO NUM-ENTRIES(cArtikkelEti,CHR(1)):
    ASSIGN cArtEANlist  = ENTRY(ix,cEtiketter,CHR(1))
           cArtANTlist  = ENTRY(ix,cAntallEti,CHR(1))
           cArtINDlist  = ENTRY(ix,cIndividNr,CHR(1))
           cArtPRISList = ENTRY(ix,cPris,CHR(1))
           .
    IF cArtEANlist <> "" THEN DO:
      IF NOT VALID-HANDLE(hEtikettVindu) THEN
          RUN w-TmpEtikett.w PERSISTENT SET hEtikettVindu (THIS-PROCEDURE:CURRENT-WINDOW).
      IF VALID-HANDLE(hEtikettVindu) THEN 
        DO iCount = 1 TO NUM-ENTRIES(cArtANTlist):
          IF ENTRY(iCount,cArtEANlist) <> "" AND INT(ENTRY(iCount,cArtANTlist)) > 0 THEN
            RUN NyEtikettPakkseddel IN hEtikettVindu (
                ENTRY(iCount,cArtEANlist),
                INT(ENTRY(iCount,cArtANTlist)),
                INT(ENTRY(iCount,cArtINDlist)),
                INT(ENTRY(iCount,cArtPRISList))
                ).
        END.
    END.
  END.
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
ELSE
  DYNAMIC-FUNCTION("DeleteObjectLink",hBrowse,DYNAMIC-FUNCTION("getPageQuery" IN hTabFolder,2)).

IF iiTab = 1 THEN DO:
  IF hBrowse:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN
    DYNAMIC-FUNCTION("RefreshRowids",hBrowse,hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("RowIdent1"):BUFFER-VALUE).
  RETURN NO.   
END. 

ASSIGN hCurrTabQuery = DYNAMIC-FUNCTION("getPageQuery" IN hTabFolder,iiTab)
       hCurrTabProc  = DYNAMIC-FUNCTION("getPageHandle" IN hTabFolder,iiTab)
       hCurrTabFrame = DYNAMIC-FUNCTION("getPageFrame" IN hTabFolder,iiTab)
       iCurrTab      = iiTab.


IF iCurrTab NE 4 THEN
  DYNAMIC-FUNCTION("CreateParentLink",hCurrTabQuery,hBrowse,"PkSdlId").
ELSE
  DYNAMIC-FUNCTION("CreateOneToOneLink",hCurrTabQuery,hBrowse,"PkSdlId").

IF CAN-DO(hCurrTabProc:INTERNAL-ENTRIES,"setFilter") THEN 
  DYNAMIC-FUNCTION("setFilter" IN hCurrTabProc).

DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
RUN DisplayRecord.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

