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
DEF INPUT PARAM ihParent       AS HANDLE NO-UNDO. /* window-handel */
DEF INPUT PARAM ihQueryObject  AS HANDLE NO-UNDO. /* browse or query */
DEF INPUT PARAM icFeltListe    AS CHARACTER NO-UNDO.
DEF INPUT PARAM icReturnType   AS CHARACTER NO-UNDO. /* 'ROW' eller "FIELD" */
DEF INPUT-OUTPUT PARAM ocListe       AS CHARACTER NO-UNDO.
DEF OUTPUT PARAM oiReturnValue AS INTEGER NO-UNDO.   /* 0 = 'cancel' 1 = 'alle' 2 = 'valgte' */

/* Local Variable Definitions ---                                       */
DEF VAR ix               AS INT NO-UNDO.
DEF VAR bOk              AS LOG NO-UNDO.

DEF VAR hFilter          AS HANDLE NO-UNDO.
DEF VAR hBrowse          AS HANDLE NO-UNDO.
DEF VAR hBuffer          AS HANDLE NO-UNDO.
DEF VAR hToolbar         AS HANDLE NO-UNDO.
DEF VAR hWinToolbar      AS HANDLE NO-UNDO.

DEF VAR hOperatorOverlay AS HANDLE NO-UNDO.
DEF VAR hValueOverlay    AS HANDLE NO-UNDO.
DEF VAR hFieldOpOverlay  AS HANDLE NO-UNDO.
DEF VAR httCopy          AS HANDLE NO-UNDO.

DEF VAR hMenuItemAdv     AS HANDLE NO-UNDO.
DEF VAR hMenuItemViewQ   AS HANDLE NO-UNDO.
DEF VAR bAdv             AS LOG    NO-UNDO.
DEF VAR rRepos           AS ROWID  NO-UNDO.
DEF VAR hFieldGroup      AS HANDLE NO-UNDO.
DEF VAR cBaseTable       AS CHAR   NO-UNDO.
DEF VAR cPreselected     AS CHAR NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rectWinToolbar rectToolbar rectBrowse ~
RS-Custom 
&Scoped-Define DISPLAYED-OBJECTS RS-Custom 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FillFromCopy C-Win 
FUNCTION FillFromCopy RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE RS-Custom AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Alle", 1,
"Utvalg", 2
     SIZE 22 BY .95 NO-UNDO.

DEFINE RECTANGLE rectBrowse
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 70.2 BY 18.33.

DEFINE RECTANGLE rectToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 14.6 BY .91.

DEFINE RECTANGLE rectWinToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 14.6 BY .91.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     RS-Custom AT ROW 1.24 COL 24 NO-LABEL
     rectWinToolbar AT ROW 1.29 COL 57.2
     rectToolbar AT ROW 1.29 COL 2
     rectBrowse AT ROW 2.43 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 71.6 BY 19.81.


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
         TITLE              = "Felter"
         HEIGHT             = 19.81
         WIDTH              = 71.8
         MAX-HEIGHT         = 52.38
         MAX-WIDTH          = 142.6
         VIRTUAL-HEIGHT     = 52.38
         VIRTUAL-WIDTH      = 142.6
         ALWAYS-ON-TOP      = yes
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
IF NOT C-Win:LOAD-ICON("ico/prospy9.ico":U) THEN
    MESSAGE "Unable to load icon: ico/prospy9.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 19.81
       FRAME DEFAULT-FRAME:WIDTH            = 71.6.

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
ON END-ERROR OF C-Win /* Felter */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Felter */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* Felter */
DO:
  DEF VAR hColumn          AS HANDLE NO-UNDO.
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"resize","").

  hColumn = hBrowse:GET-BROWSE-COLUMN(1).
  APPLY "end-resize" TO hColumn.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RS-Custom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RS-Custom C-Win
ON VALUE-CHANGED OF RS-Custom IN FRAME DEFAULT-FRAME
DO:
    DEFINE VARIABLE iBrowseCol AS INTEGER    NO-UNDO.
    ASSIGN INPUT RS-Custom.
    IF RS-Custom = 1 THEN DO:
        hValueOverlay:HIDDEN = TRUE.
        DYNAMIC-FUNCTION("DeleteObjectLink",hBrowse,hValueOverlay). /* Link the dropdown to the browse with column-name info */
    END.
    ELSE
        DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hValueOverlay,"ColumnValue"). /* Link the dropdown to the browse with column-name info */
    hBrowse:QUERY:GET-FIRST().
    REPEAT WHILE NOT hBrowse:QUERY:QUERY-OFF-END:
        iBrowseCol = hBuffer:BUFFER-FIELD("BrowseCol"):BUFFER-VALUE.
        hBuffer:BUFFER-FIELD("ColumnValue"):BUFFER-VALUE = RS-Custom = 1 OR CAN-DO(cPreselected,STRING(iBrowseCol)).
        hBrowse:QUERY:GET-NEXT().
    END.
    DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
    RUN OpenQuery.
    APPLY "ENTRY" TO hBrowse.
    IF RS-Custom = 2 THEN
        APPLY "ENTRY" TO hValueOverlay.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}
       CURRENT-WINDOW:X = ihParent:X
       CURRENT-WINDOW:Y = ihParent:Y
       .

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
  DYNAMIC-FUNCTION("DoCleanUpObjects",THIS-PROCEDURE:CURRENT-WINDOW).
  DYNAMIC-FUNCTION("SetCleanUpResize",THIS-PROCEDURE:CURRENT-WINDOW).
  RUN disable_UI.
END.
ASSIGN cPreselected = ocListe
       ocListe      = "".
{incl/wintrigg.i}

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  oiReturnValue = 0. /* vid cancel */
  RUN enable_UI.
  RUN InitializeObject.
  THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = FALSE.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

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
  DISPLAY RS-Custom 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE rectWinToolbar rectToolbar rectBrowse RS-Custom 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitFilter C-Win 
PROCEDURE InitFilter :
/*------------------------------------------------------------------------------
  Purpose:     This is where the programmer makes the change
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cFilterFields   AS CHAR  NO-UNDO.
DEF VAR iy              AS INT   NO-UNDO.
DEF VAR cKeepFilterOpen AS CHAR  NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:

  hBrowse = DYNAMIC-FUNCTION("NewBrowse",rectBrowse:HANDLE,1,(IF bAdv THEN "MULTIPLE" ELSE ""),
                             "_file"
                           + ";+BrowseCol|INTEGER|>9||Col"
                           + ";+ColumnLabel|CHARACTER|x(20)||Felt"
                           + ";+ColumnValue|Logical|J/||Valgt"
                           + ";+!ColumnName|CHARACTER"
                             ,
                             "where false",
                             "SORT|BrowseCol")
                            .
  ASSIGN hBrowse:GET-BROWSE-COLUMN(1):WIDTH-PIXELS = 40
         hBrowse:GET-BROWSE-COLUMN(2):WIDTH-PIXELS = 150
         hBrowse:GET-BROWSE-COLUMN(3):WIDTH-PIXELS = 35
/*          hFieldGroup = hBrowse:GET-BROWSE-COLUMN(2) */
/*          hBrowse:GET-BROWSE-COLUMN(4):WIDTH-PIXELS = 120 */
         .

  hBuffer = hBrowse:QUERY:GET-BUFFER-HANDLE(1).

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"uselocaldata","yes").

  IF NOT VALID-HANDLE(ihQueryObject) THEN DO:
    DO ix = 1 TO NUM-ENTRIES(icFeltListe):
      hBuffer:BUFFER-CREATE().
      ASSIGN hBuffer:BUFFER-FIELD("BrowseCol"):BUFFER-VALUE = ix
             hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE = REPLACE(ENTRY(ix,icFeltListe)," ","")
             hBuffer:BUFFER-FIELD("ColumnLabel"):BUFFER-VALUE = ENTRY(ix,icFeltListe)
             hBuffer:BUFFER-FIELD("ColumnValue"):BUFFER-VALUE = TRUE
             .
    END.
  END.
  hValueOverlay = DYNAMIC-FUNCTION("NewBrowseToggle",
                    hBrowse,                      /* Handle to browse */
                    "ColumnValue",                   /* Browse column (display) */
                    "ColumnValue",                   /* Buffer column (to update - foreign key. Maps to value - under) */
                    "").   

  DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"nocolumnsort","BrowseCol,ColumnLabel,ColumnValue").
  RUN OpenQuery.
  IF cPreselected <> "" THEN DO:
      RS-Custom:SCREEN-VALUE = "2".
      APPLY "VALUE-CHANGED" TO RS-Custom.
  END.
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
DEF VAR hSubMenuAdv AS HANDLE NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  hToolbar = DYNAMIC-FUNCTION("NewToolbar",
                             rectToolbar:HANDLE,
                             "File",
                             "OK;&OK,|Innstillinger",
                             "maxborder,enable").
    
  RUN InitFilter.

  hWinToolbar = DYNAMIC-FUNCTION("NewToolbar",
                             rectWinToolbar:HANDLE,
                             "File",
                             "Close;Exit;;",
/*                              "Close;Exit,Pin;;;pinWindow;gif/pushout.gif", */
                             "right,enable").  

END.

/* DYNAMIC-FUNCTION("CreateObjectLink",hSaveFilterMenu,hFilter).  */
DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "rectToolbar").
DYNAMIC-FUNCTION("setOrgWinSize", THIS-PROCEDURE:CURRENT-WINDOW,200,150,250,150).

APPLY "value-changed" TO hBrowse.
DYNAMIC-FUNCTION("InitTranslation",THIS-PROCEDURE:CURRENT-WINDOW).

THIS-PROCEDURE:CURRENT-WINDOW:HEIGHT-PIXELS = MIN(SESSION:HEIGHT-PIXELS - 100,150 + ix * 15).
APPLY "window-resized" TO {&WINDOW-NAME}.
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

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OKrecord C-Win 
PROCEDURE OKrecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN oiReturnValue = RS-Custom.
    IF oiReturnValue = 2 THEN DO:
        /* här tar vi hand om de valda posterna */
        hBrowse:QUERY:GET-FIRST().
        REPEAT WHILE NOT hBrowse:QUERY:QUERY-OFF-END:
            IF hBuffer:BUFFER-FIELD("ColumnValue"):BUFFER-VALUE = TRUE THEN
                ASSIGN ocListe = ocListe + (IF ocListe <> "" THEN "," ELSE "") + 
                (IF icReturnType = "ROW" THEN STRING(hBuffer:BUFFER-FIELD("BrowseCol"):BUFFER-VALUE) ELSE hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE).
            hBrowse:QUERY:GET-NEXT().
        END.
        IF ocListe = "" THEN
            ASSIGN oiReturnValue = 0.
    END.
    APPLY "CLOSE" TO THIS-PROCEDURE.
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
    RUN SUPER.
    IF RS-Custom = 2 THEN
        APPLY "ENTRY" TO hValueOverlay.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FillFromCopy C-Win 
FUNCTION FillFromCopy RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hCopyQuery AS HANDLE NO-UNDO.
DEF VAR bFilled    AS LOG NO-UNDO.

CREATE QUERY hCopyQuery.
hCopyQuery:SET-BUFFERS(httCopy:DEFAULT-BUFFER-HANDLE).
hCopyQuery:QUERY-PREPARE("FOR EACH " + httCopy:DEFAULT-BUFFER-HANDLE:NAME).
hCopyQuery:QUERY-OPEN().
hCopyQuery:GET-FIRST().
REPEAT WHILE NOT hCopyQuery:QUERY-OFF-END:
  hBuffer:BUFFER-CREATE().
  hBuffer:BUFFER-COPY(hCopyQuery:GET-BUFFER-HANDLE(1)).
  hCopyQuery:GET-NEXT().
  bFilled = TRUE.
  IF rRepos = ? AND hBuffer:BUFFER-FIELD("Operator"):BUFFER-VALUE NE "" THEN 
    rRepos = hBuffer:ROWID.
END.

DELETE OBJECT hCopyQuery.

RETURN bFilled.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

