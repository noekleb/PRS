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

/* Parameters Definitions ---                                           */
DEF INPUT PARAM ihParent       AS HANDLE NO-UNDO.
DEF INPUT PARAM ihBrowse       AS HANDLE NO-UNDO. /* browse or query */
DEF INPUT PARAM ihConfigWidget AS HANDLE NO-UNDO.

/* Local Variable Definitions ---                                       */
DEF VAR ix                   AS INT    NO-UNDO.
DEF VAR bOk                  AS LOG    NO-UNDO.

DEF VAR hBrowse              AS HANDLE NO-UNDO.
DEF VAR hQuery               AS HANDLE NO-UNDO.
DEF VAR hBuffer              AS HANDLE NO-UNDO.
DEF VAR hToolbar             AS HANDLE NO-UNDO.
DEF VAR hWinToolbar          AS HANDLE NO-UNDO.
DEF VAR hSaveFilterMenu      AS HANDLE NO-UNDO.
DEF VAR bConfigEdit          AS LOG    NO-UNDO.
DEF VAR iColor               AS INT    NO-UNDO.
DEF VAR iOrgColor            AS INT    NO-UNDO.
DEF VAR bReset               AS LOG    NO-UNDO.
DEF VAR iNumLockedCols       AS INT    NO-UNDO.
DEF VAR iViewNumLockedCols   AS INT    NO-UNDO.
                            
DEF VAR hViewFieldOverlay    AS HANDLE NO-UNDO.
DEF VAR hEditFieldOverlay    AS HANDLE NO-UNDO.
DEF VAR hNameField           AS HANDLE NO-UNDO.
DEF VAR hViewField           AS HANDLE NO-UNDO.
DEF VAR hEditField           AS HANDLE NO-UNDO.
DEF VAR hMandField           AS HANDLE NO-UNDO.
DEF VAR hPinButton           AS HANDLE NO-UNDO.
DEF VAR hMenuItemSaveSetting AS HANDLE NO-UNDO.
DEF VAR hMenuItemAdvanced    AS HANDLE NO-UNDO.

DEF VAR cActivePinButton     AS CHAR   NO-UNDO.
DEF VAR cPassivePinButton    AS CHAR   NO-UNDO.

DEF VAR rRepos               AS ROWID  NO-UNDO.
DEF VAR iMaxSeq              AS INT    NO-UNDO INIT -1000.

DEF VAR cAllViewFields       AS CHAR   NO-UNDO.
DEF VAR cBrowseColumn        AS CHAR   NO-UNDO.
DEF VAR cMandFields          AS CHAR   NO-UNDO.

DEF VAR iFontWingdings       AS INT    NO-UNDO.
iFontWingdings = DYNAMIC-FUNCTION("setAppFont","Wingdings, size=11 Script=symbol","").

DEF VAR cNoMoveColumns       AS CHAR   NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rectWinToolbar rectToolbar rectBrowse 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setFieldColor C-Win 
FUNCTION setFieldColor RETURNS INTEGER
  ( INPUT iRGBcolor AS INT)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ViewTotals C-Win 
FUNCTION ViewTotals RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE RECTANGLE rectBrowse
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 58.2 BY 18.1.

DEFINE RECTANGLE rectToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 14.6 BY .91.

DEFINE RECTANGLE rectWinToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 14.6 BY .91.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     rectWinToolbar AT ROW 1.33 COL 45.6
     rectToolbar AT ROW 1.33 COL 2
     rectBrowse AT ROW 2.67 COL 1.8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 59.8 BY 19.81.


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
         TITLE              = "Kolonneoppsett"
         HEIGHT             = 19.81
         WIDTH              = 59.8
         MAX-HEIGHT         = 52.38
         MAX-WIDTH          = 142.6
         VIRTUAL-HEIGHT     = 52.38
         VIRTUAL-WIDTH      = 142.6
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



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
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

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Kolonneoppsett */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Kolonneoppsett */
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
  DYNAMIC-FUNCTION("DoCleanUpObjects",THIS-PROCEDURE:CURRENT-WINDOW).
  DYNAMIC-FUNCTION("SetCleanUpResize",THIS-PROCEDURE:CURRENT-WINDOW).
  RUN disable_UI.

  PUBLISH "EndJBoxEvent" (?,ihParent,?,"JBoxWindowClose").
  IF VALID-HANDLE(ihBrowse) THEN APPLY "entry" TO ihBrowse.
END.

{incl/wintrigg.i}

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  RUN InitializeObject.

  IF THIS-PROCEDURE:CURRENT-WINDOW:ICON = "" THEN
    THIS-PROCEDURE:CURRENT-WINDOW:LOAD-ICON("ico\app16.ico") NO-ERROR.

  THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = FALSE.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

ON 'window-resized':U OF {&WINDOW-NAME} DO:
  DEF VAR hColumn          AS HANDLE NO-UNDO.
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"resize","").

  hColumn = hBrowse:GET-BROWSE-COLUMN(1).
  APPLY "end-resize" TO hColumn.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AdvancedSetupRecord C-Win 
PROCEDURE AdvancedSetupRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN JBoxDAdvancedBrowseConfig.w (ihBrowse,OUTPUT bOk).
        
hMenuItemAdvanced:CHECKED = bOk.
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ColorRecord C-Win 
PROCEDURE ColorRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iColNum AS INT NO-UNDO.

RUN JBoxDSelectColor.w ("Tekst",INPUT-OUTPUT iColor,OUTPUT iColNum,OUTPUT bOK).

IF bOK THEN 
  DYNAMIC-FUNCTION("setBrwOverlayBGcolNum",hBrowse,iColNum).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CopyToUsersRecord C-Win 
PROCEDURE CopyToUsersRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cRowIdList  AS CHAR NO-UNDO.
DEF VAR cIdList     AS CHAR NO-UNDO.  
DEF VAR bOk         AS LOG  NO-UNDO.
DEF VAR cKeepOpen   AS CHAR NO-UNDO.

THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
RUN JBoxSelector.w (THIS-PROCEDURE,0,
                    "JBoxUser"      
                    + ";cJBoxUserId"  
                    + ";cUserName"
                    ,"where cJBoxUserId NE '" + DYNAMIC-FUNCTION("getASuserId") + "'",
                    INPUT-OUTPUT cRowIdList,
                    "cJBoxUserId", /* Primary key */
                    INPUT-OUTPUT cIdList,
                    "","",
                    OUTPUT bOK).
THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

IF bOk THEN DO:
  cKeepOpen = DYNAMIC-FUNCTION("getAttribute",ihBrowse,"keepconfigopen").
  DYNAMIC-FUNCTION("setAttribute",ihBrowse,"keepconfigopen","YES").
  RUN SetBrowseRecord.
  DYNAMIC-FUNCTION("setAttribute",ihBrowse,"keepconfigopen",cKeepOpen).
  IF NOT DYNAMIC-FUNCTION("runProc","jbserv_copy_usersettings.p",
                          DYNAMIC-FUNCTION("getObjectSourceFile",ihBrowse) + "¤"
                        + DYNAMIC-FUNCTION("getObjectName",ihBrowse) + "¤"
                        + DYNAMIC-FUNCTION("getAttribute",ihBrowse,"usersettingcontext") + "¤"
                        + "widthpixelslist,overlaybgcolor,noeditfields,currviewfields" + "¤"
                        + cRowIdList
                         ,?) THEN
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
END.
THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().

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
DEF VAR hColumn          AS HANDLE NO-UNDO.
DEF VAR cCurrToggle      AS CHAR   NO-UNDO.

IF hBuffer:AVAIL AND CAN-DO(cNoMoveColumns,hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE) THEN
  DYNAMIC-FUNCTION("setAttribute",hToolbar,"disabledEvents","MoveDown,MoveUp").
ELSE
  DYNAMIC-FUNCTION("setAttribute",hToolbar,"disabledEvents","").

RUN SUPER.

IF hBuffer:AVAIL THEN DO:
  IF hBuffer:BUFFER-FIELD("hEditField"):BUFFER-VALUE = ? THEN
    hEditFieldOverlay:VISIBLE = NO.
  IF CAN-DO(cNoMoveColumns,hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE) THEN
    hViewFieldOverlay:VISIBLE = NO.
END.

/*
DYNAMIC-FUNCTION("setCurrentObject",hBrowse).

/* DYNAMIC-FUNCTION("DoLockWindow",THIS-PROCEDURE:CURRENT-WINDOW). */
/* DYNAMIC-FUNCTION("DeleteObject",hViewFieldOverlay).             */
/* DYNAMIC-FUNCTION("DoLockWindow",?).                             */
DYNAMIC-FUNCTION("DeleteObjectLink",hBrowse,hViewFieldOverlay).

RUN SUPER.

IF hBuffer:AVAIL THEN DO:
/*   DYNAMIC-FUNCTION("DeleteObjectLink",hBrowse,hViewFieldOverlay).  */

  IF NOT CAN-DO(cMandFields,hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE) THEN DO:
    hViewFieldOverlay = DYNAMIC-FUNCTION("NewBrowseToggle",
                      hBrowse,                    
                      "ViewField",                   
                      "ViewField",                   
                      "").
    DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hViewFieldOverlay,"ViewField"). /* Link the dropdown to the browse with column-name info */
    ASSIGN hViewFieldOverlay:CHECKED  = hBuffer:BUFFER-FIELD("ViewField"):BUFFER-VALUE
           hViewFieldOverlay:MODIFIED = FALSE
           hViewFieldOverlay:BGCOLOR  = hViewField:BGCOLOR
           cCurrToggle               = STRING(hViewFieldOverlay)
           NO-ERROR.
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"currentoverlaywidget",STRING(hViewFieldOverlay)).
  END.

  hColumn = hBrowse:GET-BROWSE-COLUMN(1).
  APPLY "end-resize" TO hColumn.
END.

DYNAMIC-FUNCTION("setAttribute",hBrowse,"currbrowsetoggles",cCurrToggle).
/* RUN SUPER.                               */
/*                                          */
/* hColumn = hBrowse:GET-BROWSE-COLUMN(1).  */
/* APPLY "end-resize" TO hColumn.           */

*/
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
  ENABLE rectWinToolbar rectToolbar rectBrowse 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HideRecord C-Win 
PROCEDURE HideRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
  IF hBrowse:FETCH-SELECTED-ROW(ix) THEN DO:
    IF NOT CAN-DO(cMandFields,hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE) THEN
      hBuffer:BUFFER-FIELD("ViewField"):BUFFER-VALUE = NO.
  END.
END.
DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
RUN OpenQuery.

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
DEF VAR hSubMenuSettings      AS HANDLE NO-UNDO.
DEF VAR cCurrViewFields       AS CHAR   NO-UNDO.
DEF VAR cDataType             AS CHAR   NO-UNDO.
DEF VAR iy                    AS INT    NO-UNDO.
DEF VAR cKeepConfigOpen       AS CHAR   NO-UNDO.
DEF VAR iNumViewRows          AS INT    NO-UNDO.
DEF VAR bUserSettingInstalled AS LOG    NO-UNDO.
DEF VAR bSuperUser            AS LOG    NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  ASSIGN cActivePinButton      = DYNAMIC-FUNCTION("getAttribute",SESSION,"ActivePinButton")
         cPassivePinButton     = DYNAMIC-FUNCTION("getAttribute",SESSION,"PassivePinButton")
         cKeepConfigOpen       = DYNAMIC-FUNCTION("getAttribute",ihBrowse,"keepconfigopen")
         bConfigEdit           = DYNAMIC-FUNCTION("getAttribute",ihBrowse,"configeditfields") = "yes"
         bUserSettingInstalled = DYNAMIC-FUNCTION("getIsUserSettingInstalled") AND ihParent:FILE-NAME NE "JBoxDataBrw.w"
         bSuperUser            = LOGICAL(DYNAMIC-FUNCTION("getFieldValues","JBoxUser","WHERE cJBoxUserId = '" + DYNAMIC-FUNCTION("getASuserId") + "'","bSuperuser"))        
         NO-ERROR.
    
  IF cActivePinButton  = "" THEN cActivePinButton = "gif\pushout.gif".
  IF cPassivePinButton = "" THEN cPassivePinButton = "gif\pushin.gif".

  bConfigEdit = DYNAMIC-FUNCTION("getLinkedObject",ihBrowse,"browseoverlay","from") NE ?.

  hToolbar = DYNAMIC-FUNCTION("NewToolbar",
                             rectToolbar:HANDLE,
                             IF DYNAMIC-FUNCTION("Scandinavian") THEN "Fil" ELSE "File",
                             "MoveUp;"
                             + (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Flytt &opp" ELSE "Move &up")
                           + ",MoveDown;"
                             + (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Flytt &ned" ELSE "Move &down")
                           + (IF bConfigEdit THEN
                              ",Color;" + (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Sett bakgrunnsfarge for redigering" ELSE "Set backgroundcolor for edit")
                              ELSE "")
/*                            + (IF bConfigEdit THEN                                                                                                         */
/*                               ",AdvancedSetup;" + (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Avanserte innstillinger" ELSE "Advanced setup") + ";toggle"  */
/*                               ELSE "")                                                                                                                    */
                           + ",Reset;"
                             + (IF DYNAMIC-FUNCTION("Scandinavian") THEN "&Tilbakestill" ELSE "Reset")
                           + ",SetBrowse;&OK"
                           + (IF bUserSettingInstalled THEN 
                               (IF bSuperUser THEN ",COPYTOUSERS;" + (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Kopier oppsett til andre brukere¤MENU" ELSE "Copy setup to other users¤MENU") ELSE "")
                             + ",|" + (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Innstillinger" ELSE "Settings")
                              ELSE "")
                             ,"maxborder,enable").
    
  hMenuItemAdvanced = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hToolbar,"menu-itemAdvancedSetup")) NO-ERROR.

  DYNAMIC-FUNCTION("DeleteObject",hBrowse).
  DYNAMIC-FUNCTION("DeleteObject",hViewFieldOverlay).
  rRepos = ?.

  IF NOT DYNAMIC-FUNCTION("Scandinavian") THEN THIS-PROCEDURE:CURRENT-WINDOW:TITLE = "Column setup".

  hBrowse = DYNAMIC-FUNCTION("NewBrowse",rectBrowse:HANDLE,1,"MULTIPLE",
                             "temp-table"
                           + ";+ColumnLabel|CHARACTER|x(50)||" + (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Felt"   ELSE "Field") 
                           + ";+ViewField|LOGICAL|yes/no||"        + (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Vis"    ELSE "View") 
                           + ";+" + (IF bConfigEdit THEN "" ELSE "!")
                             + "EditField|LOGICAL|yes/no||"        + (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Rediger"    ELSE "Edit") 
                           + ";+!ColumnName|CHARACTER" 
                           + ";+!DataType|CHARACTER" 
                           + ";+!Format|CHARACTER" 
                           + ";+!Seq|INTEGER" 
                           + ";+!OrgSeq|INTEGER" 
                           + ";+!MandField|LOGICAL" 
                           + ";+!hEditField|HANDLE"
                            ,"where false"
                            ,"").

  DYNAMIC-FUNCTION("setNoColumnSort",hBrowse,"ColumnLabel,ViewField,EditField").

  DYNAMIC-FUNCTION("NewMenuBand",
                   hBrowse,
                   "View;" + (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Vis" ELSE "View")
                 + ",Hide;" + (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Skjul" ELSE "Hide")
                 + ",Excel;" + (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Eksporter til Excel" ELSE "Export to Excel")
                   ,"").

  ASSIGN 
         hBrowse:GET-BROWSE-COLUMN(1):WIDTH-PIXELS = 180
         hViewField                                = hBrowse:GET-BROWSE-COLUMN(2)
         hEditField                                = IF bConfigEdit THEN hBrowse:GET-BROWSE-COLUMN(3) ELSE ?
         hBuffer                                   = hBrowse:QUERY:GET-BUFFER-HANDLE(1)
         hQuery                                    = hBrowse:QUERY
         cCurrViewFields                           = DYNAMIC-FUNCTION("getAttribute",ihBrowse,"currviewfields")
         cAllViewFields                            = DYNAMIC-FUNCTION("getAttribute",ihBrowse,"allviewfields")
         cMandFields                               = DYNAMIC-FUNCTION("getAttribute",ihBrowse,"mandatoryfields")
         cNoMoveColumns                            = DYNAMIC-FUNCTION("getAttribute",ihBrowse,"noMoveColumns")
         iNumLockedCols                            = ihBrowse:NUM-LOCKED-COLUM
         iViewNumLockedCols                        = iNumLockedCols
         .

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"uselocaldata","yes").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"select1strow","yes").
  DYNAMIC-FUNCTION("CreateObjectLink",hToolbar,hBrowse).

  DO ix = 1 TO ihBrowse:NUM-COLUMNS:
    IF CAN-DO("jbCountDistinct,jbAverage",ihBrowse:GET-BROWSE-COLUMN(ix):NAME) THEN NEXT.

    ASSIGN cBrowseColumn = ihBrowse:GET-BROWSE-COLUMN(ix):NAME
           cDataType     = ihBrowse:GET-BROWSE-COLUMN(ix):DATA-TYPE.

    iNumViewRows = iNumViewRows + 1.

    hBuffer:BUFFER-CREATE().
    ASSIGN hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE  = cBrowseColumn
           hBuffer:BUFFER-FIELD("hEditField"):BUFFER-VALUE  = DYNAMIC-FUNCTION("getLinkedObjectByInfo",ihBrowse,"browseoverlay",cBrowseColumn)
           hBuffer:BUFFER-FIELD("ViewField"):BUFFER-VALUE   = CAN-DO(cCurrViewFields,cBrowseColumn)
           hBuffer:BUFFER-FIELD("EditField"):BUFFER-VALUE   = DYNAMIC-FUNCTION("getLinkedObjectByInfo",ihBrowse,"browseoverlay",cBrowseColumn) NE ?
                                                              AND NOT CAN-DO(DYNAMIC-FUNCTION("getAttribute",ihBrowse,"noeditfields"),cBrowseColumn)
           hBuffer:BUFFER-FIELD("MandField"):BUFFER-VALUE   = CAN-DO(cMandFields,cBrowseColumn)
           hBuffer:BUFFER-FIELD("DataType"):BUFFER-VALUE    = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD(cBrowseColumn):DATA-TYPE
           hBuffer:BUFFER-FIELD("Format"):BUFFER-VALUE      = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD(cBrowseColumn):FORMAT
           hBuffer:BUFFER-FIELD("ColumnLabel"):BUFFER-VALUE = DYNAMIC-FUNCTION("getStrippedSortLabel",ihBrowse:GET-BROWSE-COLUMN(ix))
           .
    IF CAN-DO(cCurrViewFields,cBrowseColumn) THEN
      hBuffer:BUFFER-FIELD("Seq"):BUFFER-VALUE            = ix.
    ELSE hBuffer:BUFFER-FIELD("Seq"):BUFFER-VALUE         = ix + 200.

    hBuffer:BUFFER-FIELD("OrgSeq"):BUFFER-VALUE           = ix.
  END.

  hViewFieldOverlay = DYNAMIC-FUNCTION("NewBrowseToggle",
                    hBrowse,                    
                    "ViewField",                   
                    "ViewField",                   
                    "").
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hViewFieldOverlay,"ViewField").
  
  hEditFieldOverlay = DYNAMIC-FUNCTION("NewBrowseToggle",
                    hBrowse,                    
                    "EditField",                   
                    "EditField",                   
                    "").
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hEditFieldOverlay,"EditField").

  iOrgColor = INT(DYNAMIC-FUNCTION("getUserSetting",DYNAMIC-FUNCTION("getObjectSourceFile",ihBrowse),DYNAMIC-FUNCTION("getObjectName",ihBrowse),"","overlaybgcolor")).
  IF iOrgColor NE 0 THEN DO:
    iColor = iOrgColor.
    DYNAMIC-FUNCTION("setBrwOverlayBGcolNum",hBrowse,setFieldColor(iOrgColor)).
  END.


  DYNAMIC-FUNCTION("setAttribute",ihBrowse,"keepconfigopen",cKeepConfigOpen).

  DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"querysort","Seq").
  RUN OpenQuery.

  IF bUserSettingInstalled THEN DO:
    hSubMenuSettings = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hToolbar,"placeholder1")).
    DYNAMIC-FUNCTION("NewMenuBand",
                     hSubMenuSettings,
                     "SaveSettings;"
                     + (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Lagre innstillinger" ELSE "Save settings")
                     + ";SaveSettings;toggle"
                     ,"").
    hMenuItemSaveSetting  = DYNAMIC-FUNCTION("getEventWidget",hToolbar,"SaveSettings","menu-item").
  
    hMenuItemSaveSetting:CHECKED = NOT DYNAMIC-FUNCTION("getAttribute",ihBrowse,"savebrowseconfigsettings") = "no".
  END.

  hWinToolbar = DYNAMIC-FUNCTION("NewToolbar",
                             rectWinToolbar:HANDLE,
                             IF DYNAMIC-FUNCTION("Scandinavian") THEN "Fil" ELSE "File",
                             "Close;E&xit;,Pin;;;pinWindow;" + cPassivePinButton,
                             "right,enable").  

  hPinButton     = DYNAMIC-FUNCTION("getEventWidget",hWinToolbar,"Pin","").
END.

DYNAMIC-FUNCTION("setMyEnableSaveSettingsMenu",NO) NO-ERROR.
DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "rectToolbar").
DYNAMIC-FUNCTION("setOrgWinSize", THIS-PROCEDURE:CURRENT-WINDOW,200,150,250,150).

THIS-PROCEDURE:CURRENT-WINDOW:HEIGHT-PIXELS = MIN(SESSION:HEIGHT-PIXELS - 100,70 + iNumViewRows * (hBrowse:ROW-HEIGHT-PIXELS + 4)).
APPLY "window-resized" TO {&WINDOW-NAME}.

APPLY "value-changed" TO hBrowse.
DYNAMIC-FUNCTION("InitTranslation",THIS-PROCEDURE:CURRENT-WINDOW).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MoveDownRecord C-Win 
PROCEDURE MoveDownRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR rBuffer  AS ROWID NO-UNDO.
DEF VAR iSeq     AS INT   NO-UNDO.
DEF VAR iNextSeq AS INT   NO-UNDO.

IF NOT hBuffer:AVAIL THEN RETURN.

IF CAN-DO(cNoMoveColumns,hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE) THEN RETURN.

ASSIGN rBuffer = hBuffer:ROWID
       iSeq    = hBuffer:BUFFER-FIELD("Seq"):BUFFER-VALUE.

IF hQuery:GET-NEXT() THEN DO:
  ASSIGN iNextSeq = hBuffer:BUFFER-FIELD("Seq"):BUFFER-VALUE
         hBuffer:BUFFER-FIELD("Seq"):BUFFER-VALUE = iSeq
         .
  hQuery:GET-PREV().
  hBuffer:BUFFER-FIELD("Seq"):BUFFER-VALUE = iNextSeq.

  hQuery:QUERY-OPEN().
  hQuery:REPOSITION-TO-ROWID(rBuffer).
  hBrowse:SELECT-ROW(hBrowse:FOCUSED-ROW).
  APPLY "value-changed" TO hBrowse.
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
THIS-PROCEDURE:CURRENT-WINDOW:WINDOW-STATE = 3.
THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
APPLY "entry" TO hBrowse.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MoveUpRecord C-Win 
PROCEDURE MoveUpRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR rBuffer  AS ROWID NO-UNDO.
DEF VAR iSeq     AS INT   NO-UNDO.
DEF VAR iPrevSeq AS INT   NO-UNDO.

IF NOT hBuffer:AVAIL THEN RETURN.

ASSIGN rBuffer = hBuffer:ROWID
       iSeq    = hBuffer:BUFFER-FIELD("Seq"):BUFFER-VALUE.

IF hQuery:GET-PREV() THEN DO:
  IF CAN-DO(cNoMoveColumns,hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE) THEN DO:
    hQuery:GET-NEXT().
    RETURN.      
  END. 

  ASSIGN iPrevSeq = hBuffer:BUFFER-FIELD("Seq"):BUFFER-VALUE
         hBuffer:BUFFER-FIELD("Seq"):BUFFER-VALUE = iSeq
         .
  hQuery:GET-NEXT().
  hBuffer:BUFFER-FIELD("Seq"):BUFFER-VALUE = iPrevSeq.

  hQuery:QUERY-OPEN().
  hQuery:REPOSITION-TO-ROWID(rBuffer).
  hBrowse:SELECT-ROW(hBrowse:FOCUSED-ROW).
  APPLY "value-changed" TO hBrowse.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PinWindow C-Win 
PROCEDURE PinWindow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF DYNAMIC-FUNCTION("getAttribute",ihBrowse,"keepconfigopen") = "yes" THEN DO:
  DYNAMIC-FUNCTION("setAttribute",ihBrowse,"keepconfigopen","").
  hPinButton:LOAD-IMAGE(cPassivePinButton).
END.
ELSE DO:
  DYNAMIC-FUNCTION("setAttribute",ihBrowse,"keepconfigopen","yes").
  hPinButton:LOAD-IMAGE(cActivePinButton).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ResetRecord C-Win 
PROCEDURE ResetRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
hQuery:GET-FIRST().

REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  ASSIGN hBuffer:BUFFER-FIELD("ViewField"):BUFFER-VALUE = YES
         hBuffer:BUFFER-FIELD("EditField"):BUFFER-VALUE = DYNAMIC-FUNCTION("getLinkedObjectByInfo",ihBrowse,"browseoverlay",hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE) NE ?
         hBuffer:BUFFER-FIELD("Seq"):BUFFER-VALUE       = LOOKUP(hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE,cAllViewFields)
         .
  hQuery:GET-NEXT().
END.
hQuery:GET-FIRST().

bReset = YES.

IF iOrgColor NE 0 THEN
  DYNAMIC-FUNCTION("setBrwOverlayBGcolNum",hBrowse,setFieldColor(iOrgColor)).
ELSE DO:
  iColor = 0.
  DYNAMIC-FUNCTION("setBrwOverlayBGcolNum",hBrowse,setFieldColor(iColor)).
END.

IF VALID-HANDLE(hMenuItemSaveSetting) AND NOT DYNAMIC-FUNCTION("getAttribute",ihBrowse,"disablesavebrowseconfig") = "yes" AND hMenuItemSaveSetting:CHECKED THEN
  DYNAMIC-FUNCTION("setUserSetting",
                   DYNAMIC-FUNCTION("getObjectSourceFile",ihBrowse),
                   DYNAMIC-FUNCTION("getObjectName",ihBrowse),
                   DYNAMIC-FUNCTION("getAttribute",ihBrowse,"usersettingcontext"),
                   "widthpixelslist,overlaybgcolor,noeditfields,currviewfields,orgviewfields",
                   "delete_setting").


DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
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
IF hBuffer:BUFFER-FIELD("MandField"):BUFFER-VALUE THEN
  hViewField:BGCOLOR = 8.

hViewField:FONT = iFontWingdings.
hViewField:FORMAT = CHR(254) + "/"  + CHR(168).

/* IF hBuffer:BUFFER-FIELD("Seq"):BUFFER-VALUE LE iViewNumLockedCols THEN  */
/*   hViewField:BGCOLOR = 7.                                               */

IF VALID-HANDLE(hEditField) THEN DO:
  IF hBuffer:BUFFER-FIELD("hEditField"):BUFFER-VALUE NE ? THEN DO:
    hEditField:FONT = iFontWingdings.
    hEditField:FORMAT = CHR(254) + "/"  + CHR(168).
  END.
  ELSE DO:
    hEditField:FONT = ?.
    hEditField:FORMAT = "*/".
  END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SaveSettings C-Win 
PROCEDURE SaveSettings :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF hMenuItemSaveSetting:CHECKED THEN
  DYNAMIC-FUNCTION("setAttribute",ihBrowse,"savebrowseconfigsettings","").
ELSE
  DYNAMIC-FUNCTION("setAttribute",ihBrowse,"savebrowseconfigsettings","no").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetBrowseRecord C-Win 
PROCEDURE SetBrowseRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:      
------------------------------------------------------------------------------*/
DEF VAR cViewFields      AS CHAR   NO-UNDO.
DEF VAR cNoEditFields    AS CHAR   NO-UNDO.
DEF VAR cButton          AS CHAR   NO-UNDO.
DEF VAR cWidthPixelsList AS CHAR   NO-UNDO.
DEF VAR hToolbar         AS HANDLE NO-UNDO.
DEF VAR hAccum           AS HANDLE NO-UNDO.
DEF VAR hFilter          AS HANDLE NO-UNDO.

hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  IF hBuffer:BUFFER-FIELD("ViewField"):BUFFER-VALUE THEN 
    cViewFields = cViewFields + hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE + ",".

  IF DYNAMIC-FUNCTION("getLinkedObjectByInfo",ihBrowse,"browseoverlay",hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE) NE ? 
     AND NOT hBuffer:BUFFER-FIELD("EditField"):BUFFER-VALUE THEN
    cNoEditFields = cNoEditFields + hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE + ",".
  hQuery:GET-NEXT().
END.

ASSIGN cViewFields   = TRIM(cViewFields,",")
       cNoEditFields = TRIM(cNoEditFields,",").

IF cViewFields = "" THEN DO:
  RUN ResetRecord.
  RETURN.
END.

DO ix = 1 TO ihBrowse:NUM-COLUMNS:
  IF ihBrowse:GET-BROWSE-COLUMN(ix):VISIBLE THEN
    cWidthPixelsList = cWidthPixelsList + ihBrowse:GET-BROWSE-COLUMN(ix):NAME + ";" + STRING(ihBrowse:GET-BROWSE-COLUMN(ix):WIDTH-PIXELS) + ",".
END.

IF VALID-HANDLE(hMenuItemSaveSetting) AND NOT DYNAMIC-FUNCTION("getAttribute",ihBrowse,"disablesavebrowseconfig") = "yes" AND hMenuItemSaveSetting:CHECKED AND NOT bReset THEN
  DYNAMIC-FUNCTION("setUserSetting",
                   DYNAMIC-FUNCTION("getObjectSourceFile",ihBrowse),
                   DYNAMIC-FUNCTION("getObjectName",ihBrowse),
                   DYNAMIC-FUNCTION("getAttribute",ihBrowse,"usersettingcontext"),
                   "widthpixelslist",
                   cWidthPixelsList).

IF bConfigEdit AND NOT bReset THEN DO:
  DYNAMIC-FUNCTION("setBrwOverlayBGcolNum",ihBrowse,setFieldColor(iColor)).
  DYNAMIC-FUNCTION("setAttribute",ihBrowse,"noeditfields",cNoEditFields).

  IF VALID-HANDLE(hMenuItemSaveSetting) AND NOT DYNAMIC-FUNCTION("getAttribute",ihBrowse,"disablesavebrowseconfig") = "yes" AND hMenuItemSaveSetting:CHECKED THEN 
    DYNAMIC-FUNCTION("setUserSetting",
                     DYNAMIC-FUNCTION("getObjectSourceFile",ihBrowse),
                     DYNAMIC-FUNCTION("getObjectName",ihBrowse),
                     DYNAMIC-FUNCTION("getAttribute",ihBrowse,"usersettingcontext"),
                     "overlaybgcolor,noeditfields",
                     STRING(iColor) + CHR(1) + cNoEditFields).
END.

IF cViewFields = DYNAMIC-FUNCTION("getAttribute",ihBrowse,"allviewfields") AND bReset THEN
  DYNAMIC-FUNCTION("setAttribute",ihBrowse,"widthpixelslist",DYNAMIC-FUNCTION("getAttribute",ihBrowse,"orgwidthpixelslist")).

DYNAMIC-FUNCTION("setAttribute",ihBrowse,"currviewfields",cViewFields).
DYNAMIC-FUNCTION("setBrowseColumns",ihBrowse,cViewFields,NO).

IF VALID-HANDLE(hMenuItemSaveSetting) AND NOT DYNAMIC-FUNCTION("getAttribute",ihBrowse,"disablesavebrowseconfig") = "yes" AND hMenuItemSaveSetting:CHECKED AND NOT bReset THEN
  DYNAMIC-FUNCTION("setUserSetting",
                   DYNAMIC-FUNCTION("getObjectSourceFile",ihBrowse),
                   DYNAMIC-FUNCTION("getObjectName",ihBrowse),
                   DYNAMIC-FUNCTION("getAttribute",ihBrowse,"usersettingcontext"),
                   "currviewfields,orgviewfields",
                   cViewFields + CHR(1) + DYNAMIC-FUNCTION("getAttribute",ihBrowse,"allViewFields")).

bReset = NO.

DYNAMIC-FUNCTION("InitToolbarWidgets",DYNAMIC-FUNCTION("getObjectByEvent",ihConfigWidget),ihBrowse).

hToolbar = DYNAMIC-FUNCTION("GetLinkedObject",ihBrowse,"toolbar","from").
IF VALID-HANDLE(hToolbar) THEN DO:
  hAccum = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hToolbar,"accumhandle")) NO-ERROR.
  IF VALID-HANDLE(hAccum) THEN 
    APPLY "close" TO hAccum.
  hFilter = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hToolbar,"filterhandle")) NO-ERROR.
  IF VALID-HANDLE(hFilter) THEN 
    APPLY "close" TO hFilter.
END.

IF CAN-DO(ihParent:INTERNAL-ENTRIES,"ExtraSetBrowseRecord") THEN
  RUN ExtraSetBrowseRecord IN ihParent (ihBrowse).

IF DYNAMIC-FUNCTION("getAttribute",ihBrowse,"keepconfigopen") = "yes" THEN 
  APPLY "entry" TO hBrowse.
ELSE APPLY "close" TO THIS-PROCEDURE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValChngBrowseToggle C-Win 
PROCEDURE ValChngBrowseToggle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF NOT CAN-DO(cMandFields,hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE) THEN DO:
  DYNAMIC-FUNCTION("DoLockWindow",THIS-PROCEDURE:CURRENT-WINDOW).
  DYNAMIC-FUNCTION("setAttribute",SESSION,"keepwindowlocked","yes").

  RUN SUPER.
  IF hBuffer:BUFFER-FIELD("Seq"):BUFFER-VALUE LE iNumLockedCols THEN DO:
    IF hBuffer:BUFFER-FIELD("ViewField"):BUFFER-VALUE THEN
      iViewNumLockedCols = iViewNumLockedCols - 1.
    ELSE
      iViewNumLockedCols = iViewNumLockedCols + 1.
  END.

  DYNAMIC-FUNCTION("setAttribute",SESSION,"keepwindowlocked","").
  DYNAMIC-FUNCTION("DoLockWindow",?).
END.
ELSE hViewFieldOverlay:CHECKED = YES.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ViewRecord C-Win 
PROCEDURE ViewRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
  IF hBrowse:FETCH-SELECTED-ROW(ix) THEN 
    hBuffer:BUFFER-FIELD("ViewField"):BUFFER-VALUE = YES.
END.
DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
RUN OpenQuery.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setFieldColor C-Win 
FUNCTION setFieldColor RETURNS INTEGER
  ( INPUT iRGBcolor AS INT) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR iy AS INT NO-UNDO.

IF iRGBcolor = 0 THEN RETURN 15.

IF iRGBcolor NE 0 THEN DO iy = 0 TO COLOR-TABLE:NUM-ENTRIES:
  IF COLOR-TABLE:GET-RGB-VALUE(iy) = iRGBcolor THEN RETURN iy.
END.

ASSIGN iy = COLOR-TABLE:NUM-ENTRIES
       COLOR-TABLE:NUM-ENTRIES = iy + 1.

IF iy = 256 THEN
  MESSAGE PROGRAM-NAME(1) SKIP
          256 SKIP
          VIEW-AS ALERT-BOX.

COLOR-TABLE:SET-DYNAMIC(iy, yes).
COLOR-TABLE:SET-RGB-VALUE(iy,iRGBcolor).

RETURN iy.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ViewTotals C-Win 
FUNCTION ViewTotals RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  hBuffer:BUFFER-FIELD("Total"):BUFFER-VALUE = 
    DEC(DYNAMIC-FUNCTION("getAttribute",ihBrowse,"statvalue" + hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE)).

  hQuery:GET-NEXT().
END.
RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

