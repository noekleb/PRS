&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
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

/* Local Variable Definitions ---                                       */

DEF VAR bOK             AS LOG NO-UNDO.
DEF VAR ix              AS INT NO-UNDO.

DEF VAR hToolbar        AS HANDLE NO-UNDO.
DEF VAR hBrowse         AS HANDLE NO-UNDO.
DEF VAR hFieldMap       AS HANDLE NO-UNDO.
DEF VAR hBrwColEANSerieAktiv AS HANDLE NO-UNDO.
DEF VAR hBrwColAntLedig      AS HANDLE NO-UNDO.
DEF VAR hBuffer         AS HANDLE NO-UNDO.

DEF VAR cTekst          AS CHAR   NO-UNDO.
DEF VAR cEANType        AS CHAR   NO-UNDO.
DEF VAR cEANLandKode    AS CHAR   NO-UNDO.
DEF VAR cAntSiffer      AS CHAR   NO-UNDO.
DEF VAR cAntSiffer8     AS CHAR   NO-UNDO.
DEF VAR cMsg            AS CHAR   NO-UNDO.
DEF VAR rRowid          AS ROWID  NO-UNDO.
DEF VAR hSearchField    AS HANDLE NO-UNDO.

DEF VAR piJBoxCompanyId as int NO-UNDO.
DEF VAR piCL            as int NO-UNDO.
DEF VAR cSprak              AS CHAR        NO-UNDO.
DEF VAR cSprakLst           AS CHAR  INITIAL 'SE,SVE' NO-UNDO.

DEF TEMP-TABLE ttVerdier
    FIELD cNavn   AS CHAR
    FIELD cVerdi  AS CHAR
    .
    
{buildfunction.i}
    
DEF VAR httVerdier  AS HANDLE NO-UNDO.
httVerdier = BUFFER ttVerdier:TABLE-HANDLE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rectBrowse rectToolBar rectWinToolbar RECT-1 ~
searchField BT-Blank CB-EDB-System CB-Tabell EDB-System Tabell InterntId ~
EksterntId Merknad 
&Scoped-Define DISPLAYED-OBJECTS CB-EDB-System CB-Tabell EDB-System Tabell ~
InterntId EksterntId Merknad 

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD HentVerdier C-Win 
FUNCTION HentVerdier RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD InitializeResize C-Win 
FUNCTION InitializeResize RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BT-Blank 
     LABEL "Blank filter" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE CB-EDB-System AS CHARACTER FORMAT "X(256)":U 
     LABEL "EDB System" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 55.8 BY 1 NO-UNDO.

DEFINE VARIABLE CB-Tabell AS CHARACTER FORMAT "X(256)":U 
     LABEL "Tabell" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 41 BY 1 NO-UNDO.

DEFINE VARIABLE EDB-System AS CHARACTER FORMAT "X(12)":U 
     LABEL "EDB-System" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 TOOLTIP "Navn på eksternt datasystem" NO-UNDO.

DEFINE VARIABLE EksterntId AS CHARACTER FORMAT "X(255)":U 
     LABEL "Eksternt id" 
     VIEW-AS FILL-IN 
     SIZE 39 BY 1 TOOLTIP "Navn på eksternt datasystem" NO-UNDO.

DEFINE VARIABLE InterntId AS CHARACTER FORMAT "X(255)":U 
     LABEL "Internt id" 
     VIEW-AS FILL-IN 
     SIZE 39 BY 1 TOOLTIP "Internt id" NO-UNDO.

DEFINE VARIABLE Merknad AS CHARACTER FORMAT "X(256)":U 
     LABEL "Merknad" 
     VIEW-AS FILL-IN 
     SIZE 39 BY 1 NO-UNDO.

DEFINE VARIABLE Tabell AS CHARACTER FORMAT "X(12)":U 
     LABEL "Tabell" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 TOOLTIP "Databasetabell som konverteringen gjelder" NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 59 BY 5.71.

DEFINE RECTANGLE rectBrowse
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 144 BY 24.05.

DEFINE RECTANGLE rectToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 28 BY .95.

DEFINE RECTANGLE rectWinToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 9.8 BY .91.

DEFINE RECTANGLE searchField
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 15 BY .95.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BT-Blank AT ROW 2.38 COL 190
     CB-EDB-System AT ROW 2.43 COL 45 COLON-ALIGNED
     CB-Tabell AT ROW 2.43 COL 114 COLON-ALIGNED
     EDB-System AT ROW 4.19 COL 162 COLON-ALIGNED HELP
          "Navn på eksternt datasystem"
     Tabell AT ROW 5.19 COL 162 COLON-ALIGNED HELP
          "Tabellnavn på den tabell som konverteringen gjelder"
     InterntId AT ROW 6.19 COL 162 COLON-ALIGNED
     EksterntId AT ROW 7.19 COL 162 COLON-ALIGNED
     Merknad AT ROW 8.19 COL 162 COLON-ALIGNED
     rectBrowse AT ROW 3.86 COL 2
     rectToolBar AT ROW 1.14 COL 1.8
     rectWinToolbar AT ROW 1.1 COL 195.6
     RECT-1 AT ROW 3.86 COL 146
     searchField AT ROW 2.43 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 204.8 BY 27.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Import konverteringsregister"
         HEIGHT             = 27
         WIDTH              = 204.8
         MAX-HEIGHT         = 57.14
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 57.14
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
ON END-ERROR OF C-Win /* Import konverteringsregister */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Import konverteringsregister */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* Import konverteringsregister */
DO:
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"Resize","").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BT-Blank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BT-Blank C-Win
ON CHOOSE OF BT-Blank IN FRAME DEFAULT-FRAME /* Blank filter */
DO:
        ASSIGN
          CB-EDB-system:SCREEN-VALUE = ENTRY(1,CB-EDB-system:list-items,'|')
          CB-Tabell:SCREEN-VALUE     = ENTRY(1,CB-Tabell:list-items,'|').
        APPLY 'VALUE-CHANGED' TO CB-EDB-System.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-EDB-System
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-EDB-System C-Win
ON value-changed OF CB-EDB-System IN FRAME DEFAULT-FRAME /* EDB System */
DO:
  DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
  RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-Tabell
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Tabell C-Win
ON value-changed OF CB-Tabell IN FRAME DEFAULT-FRAME /* Tabell */
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
  IF NOT DYNAMIC-FUNCTION("DoCleanUpObjects",THIS-PROCEDURE:CURRENT-WINDOW) THEN RETURN NO-APPLY.
  DYNAMIC-FUNCTION("setCleanUpResize",THIS-PROCEDURE:CURRENT-WINDOW).
  PUBLISH "InvalidateHandle" (THIS-PROCEDURE).
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

  RUN enable_UI.

  &IF DEFINED(UIB_is_Running) NE 0 &THEN
    RUN InitializeObject.
  &ENDIF
 
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BlankRecord C-Win 
PROCEDURE BlankRecord :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DO WITH FRAME Default-Frame:

        ASSIGN
            EDB-System:SCREEN-VALUE = CB-EDB-System:SCREEN-VALUE
            Tabell:SCREEN-VALUE     = CB-Tabell:SCREEN-VALUE
                .      
    END.
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
  DEF VAR iReturn  AS INT NO-UNDO.

  IF CAN-DO(cSprakLst,cSprak) THEN DO:
      RUN JBoxBrowseSelectMsg.w ("Skall posterna tas bort",
                               hBrowse:NUM-SELECTED-ROWS,
                               INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"recordCount")),
                               OUTPUT iReturn).
  END.
  ELSE DO:
      RUN JBoxBrowseSelectMsg.w ("Skal posten(e) slettes",
                               hBrowse:NUM-SELECTED-ROWS,
                               INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"recordCount")),
                               OUTPUT iReturn).
  END.

  IF iReturn = 0 THEN RETURN.
  ELSE IF iReturn = 1 THEN
    DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"impkonv_delete.p","").
  ELSE IF iReturn = 2 THEN
    DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowse,"impkonv_delete.p","").

  RUN InvokeMethod(hBrowse,"OpenQuery").

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
    DEFINE VARIABLE cValg AS CHARACTER NO-UNDO.
    
    DO WITH FRAME Default-Frame:

        RUN SUPER.

        ASSIGN
            EDB-System:SENSITIVE    = DYNAMIC-FUNCTION('getToolbarState',hToolbar) = 'New'
            Tabell:SENSITIVE        = DYNAMIC-FUNCTION('getToolbarState',hToolbar) = 'New'
            CB-EDB-System:SENSITIVE = DYNAMIC-FUNCTION('getToolbarState',hToolbar) <> 'New'
            CB-Tabell:SENSITIVE     = DYNAMIC-FUNCTION('getToolbarState',hToolbar) <> 'New'
            .
    END.
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
  DISPLAY CB-EDB-System CB-Tabell EDB-System Tabell InterntId EksterntId Merknad 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE rectBrowse rectToolBar rectWinToolbar RECT-1 searchField BT-Blank 
         CB-EDB-System CB-Tabell EDB-System Tabell InterntId EksterntId Merknad 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeObject C-Win 
PROCEDURE InitializeObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:      InitializeObject is called automatically when using JBoxDynMenu.w 
------------------------------------------------------------------------------*/
DO WITH FRAME  {&FRAME-NAME}.
  ASSIGN 
    CB-EDB-System:DELIMITER = "|"
    CB-EDB-System:LIST-ITEMS = "|" + DYNAMIC-FUNCTION("getFieldList","ImpHode;EDB-System","where EDB-System > ''")
    /*  CB-EDB-System:LIST-ITEMS = "|" + DYNAMIC-FUNCTION("getFieldList","EkstVPILev;KortNavn","where KortNavn > ''")*/
    CB-Tabell:DELIMITER = "|"
    CB-Tabell:LIST-ITEMS = "|" + DYNAMIC-FUNCTION("getFieldList","EkstVPITabell;TabellNavn","where EkstVPILevNr = 1")
    .
  cSprak      = DYNAMIC-FUNCTION("getFieldValues","Bruker","WHERE brukerid = '" + DYNAMIC-FUNCTION("getASuserId") + "'","Lng"). 
   
  hBrowse = DYNAMIC-FUNCTION("NewBrowse",          /* Create a browse object */
                    rectBrowse:HANDLE,              /* Rectangle to define coordinates for browse */
                    100,                            /* Rows to batch */
                    "multiple",                     /* Browse properties, ie MULTIPLE,NUM-LOCKED-COLUMNS, etc */
                    "ImpKonv"
                    + ";EDB-System"
                    + ";Tabell" 
                    + ";EksterntId|EksterntId|x(25)"
                    + ";Merknad" 
                    + ";InterntId|InterntId|x(25)"
                    + ";EDato"
                   ,"WHERE false"
                    ,"sort|EDB-System").             /* Initial sort column */
  hBuffer = hBrowse:QUERY:GET-BUFFER-HANDLE(1).

  hFieldMap = DYNAMIC-FUNCTION("NewFieldMap",      /* A fieldmap object holds extra info for display and input fields (fill-ins) 
                                                       and their corresponding buffer columns return handle equals the buffer handle */
                    hBrowse:QUERY,
                    FRAME {&FRAME-NAME}:HANDLE,     /* Frame for the input/display fields (might not be the same frame as the browse) */
                    "EDB-System,Tabell,InterntId,EksterntId,Merknad",   /* Update columns in buffer */
                      "",                           /* Corresponding input fields (fill-in..). blank if equal to update columns */
                      "",        /* Additional buffer and displ.fields - not updateable*/
                      "",                           /* Corresponding fill-ins */
                    "").                            /* other input widgets and lookup buttons for update fill-ins */

  /*DYNAMIC-FUNCTION('setAttribute',hBrowse,'calcfieldproc','eannrserie_brwcalc.p').*/
  DYNAMIC-FUNCTION("CreateObjectLink",hFieldMap,hBrowse).
  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"NoDeleteWarning","yes").
  
  hSearchField = DYNAMIC-FUNCTION("NewBrowseSearchField",SearchField:HANDLE,hBrowse,1).
  DYNAMIC-FUNCTION("CreateObjectLink",hSearchField,hBrowse).

  hToolbar = DYNAMIC-FUNCTION("NewToolBar",
                    rectToolBar:HANDLE,             /* Rectangle to define coordinates for toolbar */
                    "File",                         /* Corresponding menu label - no menu if blank */
                    "new,undo,delete,save"
                    + ",excel;Eksporter til E&xcel,Rule" /* Buttons / Menu items: action;label;tooltip;Method;image,action;label.. 
                                                       Any number of properties accepted (one ok - if predef. action) */
                    ,"maxborder").                  /* Misc - enable, maxborder.. */

  DYNAMIC-FUNCTION("CreateObjectLink",hFieldMap,hToolbar).
  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hToolbar).

  DYNAMIC-FUNCTION("NewToolBar",
                    rectWinToolBar:HANDLE,          /* Rectangle to define coordinates for toolbar */
                    "File",                         /* Corresponding menu label - no menu if blank */
                    "close",
                    "right,enable").                      /* Misc - enable, maxborder.. */

  assign
    CB-EDB-System:screen-value = entry(1,CB-EDB-System:list-items,'|')
    CB-Tabell:screen-value = entry(1,CB-Tabell:list-items,'|')
    .
  RUN InvokeMethod (hBrowse,"OpenQuery").

  APPLY "value-changed" TO hBrowse.
  /*APPLY 'entry' TO hBrowse.*/

END.

DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "RECT-1").
DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "RECT-1").
DYNAMIC-FUNCTION("setOrgWinSize", THIS-PROCEDURE:CURRENT-WINDOW,200,450,0,250).

DYNAMIC-FUNCTION("initTranslation",THIS-PROCEDURE:CURRENT-WINDOW).

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
 IF VALID-HANDLE(hBrowse) THEN APPLY "entry" TO hBrowse.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NewRecord C-Win 
PROCEDURE NewRecord :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DO WITH FRAME Default-Frame:

      RUN SUPER.

      ASSIGN
        EDB-System:SENSITIVE    = DYNAMIC-FUNCTION('getToolbarState',hToolbar) = 'New'
        Tabell:SENSITIVE        = DYNAMIC-FUNCTION('getToolbarState',hToolbar) = 'New'
        CB-EDB-System:SENSITIVE = DYNAMIC-FUNCTION('getToolbarState',hToolbar) <> 'New'
        CB-Tabell:SENSITIVE     = DYNAMIC-FUNCTION('getToolbarState',hToolbar) <> 'New'
        .
      IF DYNAMIC-FUNCTION('getToolbarState',hToolbar) = 'New' THEN
      DO:
        ASSIGN
            EDB-System:SCREEN-VALUE = CB-EDB-System:SCREEN-VALUE
            Tabell:SCREEN-VALUE     = CB-Tabell:SCREEN-VALUE
            .      
      END.
    END.
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
  def var cWhere as char no-undo.
  
  DO WITH FRAME Default-Frame:
      DYNAMIC-FUNCTION("setAttribute",hBrowse,"QueryFilter",''). 
    
      if CB-EDB-System:screen-value <> '' then
         cWhere = (if cWhere = '' then '' else ',') + buildFilter(cWhere,CB-EDB-System:HANDLE,'EDB-System','BEGINS').
      if CB-Tabell:screen-value <> '' then    
         cWhere = cWhere +
                   
                  buildFilter(cWhere,CB-Tabell:HANDLE,'Tabell','BEGINS').

      DYNAMIC-FUNCTION("setAttribute",hBrowse,"QueryFilter",cWhere). 

      ASSIGN
          CB-EDB-System:MODIFIED = FALSE 
          CB-Tabell:MODIFIED     = FALSE  
          .

      RUN SUPER.
      
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppdaterFeltRecord C-Win 
PROCEDURE OppdaterFeltRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DYNAMIC-FUNCTION("RefreshRowids",hBrowse,hBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE).

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
  IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowse THEN
  DO:
      /*
      /* Ikke initierte nummerserier er turkis. */
      IF NOT hFieldMap:buffer-field('EANSerieAktiv'):BUFFER-VALUE() AND 
          int(hFieldMap:buffer-field('AntLedig'):BUFFER-VALUE()) = 0
          THEN hBrwColEANSerieAktiv:BGCOLOR = 11.

      /* Aktive og ledige nummerserier er grønne. */
      IF hFieldMap:buffer-field('EANSerieAktiv'):BUFFER-VALUE() AND
          int(hFieldMap:buffer-field('AntLedig'):BUFFER-VALUE()) > 0
          THEN hBrwColEANSerieAktiv:BGCOLOR = 10.
      
      /* Aktive fulle nummerserier er røde. */
      IF hFieldMap:buffer-field('EANSerieAktiv'):BUFFER-VALUE() AND
          int(hFieldMap:buffer-field('AntLedig'):BUFFER-VALUE()) = 0
          THEN hBrwColEANSerieAktiv:BGCOLOR = 12.
      */    
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SaveRecord C-Win 
PROCEDURE SaveRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR cFinnes AS CHAR NO-UNDO.

  DO WITH FRAME Default-Frame:
    IF EDB-System:SCREEN-VALUE = '' THEN
    DO:
        MESSAGE "Det er ikke angitt EDB-System. " SKIP
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN.
    END.
    IF Tabell:SCREEN-VALUE = '' THEN
      DO:
          MESSAGE "Det er ikke angitt tabellnavn. " SKIP
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN.
    END.
    IF InterntId:SCREEN-VALUE = '' THEN
      DO:
          MESSAGE "Det er ikke angitt internt id. " SKIP
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN.
    END.
    IF EksterntId:SCREEN-VALUE = '' THEN
      DO:
          MESSAGE "Det er ikke angitt eksternt id. " SKIP
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN.
    END.

    ASSIGN
        cFinnes = DYNAMIC-FUNCTION("getFieldValues","ImpKonv",
                                             "WHERE EDB-System = '" + EDB-System:SCREEN-VALUE + "'" +
                                                " and Tabell = '"  + Tabell:SCREEN-VALUE + "'" + 
                                                " and InterntId = '"  + InterntId:SCREEN-VALUE + "'" +
                                                " and EksterntId = '"  + EksterntId:SCREEN-VALUE + "'",
                                             "EDB-System")
        cFinnes = IF cFinnes = ? OR cFinnes = '?' THEN '' ELSE cFinnes
        .
    IF cFinnes <> '' AND Tabell:SENSITIVE = TRUE THEN
    DO:
        MESSAGE "Det er allerede lagt opp en konverteringspost for dette interne Id'et." SKIP                
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN.
    END.
    
    RUN SUPER.

  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValueChangedField C-Win 
PROCEDURE ValueChangedField :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER cField AS CHAR NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
      CASE cField:
          when 'CB-EDB-System' then 
            RUN InvokeMethod (hBrowse,"OpenQuery").
          when 'CB-Tabell' then 
            RUN InvokeMethod (hBrowse,"OpenQuery").
      END CASE.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AdjustBrowseColumns C-Win 
FUNCTION AdjustBrowseColumns RETURNS LOGICAL
  ( INPUT ihBrowse     AS HANDLE,
    INPUT icBrowseName AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose: Hook fra NewBrowse slik at kolonner kan flyttes regelbasert.
           Gjør også justeringer av kolonnebredder her slik at disse 
           blir tatt vare på.
    Notes:  
------------------------------------------------------------------------------*/
IF icBrowseName = "rectBrowse" THEN DO:  

  /*
  hBrwColEANSerieAktiv = ihBrowse:GET-BROWSE-COLUMN(2).
  hBrwColAntLedig      = ihBrowse:GET-BROWSE-COLUMN(10).
  */
  
/*   ASSIGN ihBrowse:GET-BROWSE-COLUMN(2):WIDTH-PIXELS  = 70                                           */
/*          ihBrowse:GET-BROWSE-COLUMN(3):WIDTH-PIXELS  = 110                                          */
/*          ihBrowse:GET-BROWSE-COLUMN(24):WIDTH-PIXELS  = 40                                          */
/*          .                                                                                          */
/*                                                                                                     */
/*   DO ix = 1 TO 15:                                                                                  */
/*     ihBrowse:GET-BROWSE-COLUMN(ix):WIDTH-PIXELS = ihBrowse:GET-BROWSE-COLUMN(ix):WIDTH-PIXELS - 15. */
/*     IF ihBrowse:GET-BROWSE-COLUMN(ix):NAME = "Beskr" THEN                                           */
/*       hBrwColArtBeskr = ihBrowse:GET-BROWSE-COLUMN(ix).                                             */
/*   END.                                                                                              */

END.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION HentVerdier C-Win 
FUNCTION HentVerdier RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hField AS HANDLE NO-UNDO.

EMPTY TEMP-TABLE ttVerdier.
hField = FRAME {&FRAME-NAME}:FIRST-CHILD:FIRST-CHILD.

REPEAT WHILE VALID-HANDLE(hField):
  IF CAN-DO("fill-in,combo-box",hField:TYPE) THEN DO:
    CREATE ttVerdier.
    ASSIGN ttVerdier.cNavn  = hField:NAME
           ttVerdier.cVerdi = hField:INPUT-VALUE
           .
  END.
  hField = hField:NEXT-SIBLING.
END.

RETURN httVerdier.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION InitializeResize C-Win 
FUNCTION InitializeResize RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "searchField").

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

