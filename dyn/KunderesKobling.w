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

/* Local Variable Definitions ---                                       */

DEF VAR bOk               AS LOG  NO-UNDO.
DEF VAR ix                AS INT  NO-UNDO.
DEF VAR iReturn           AS INT  NO-UNDO.
DEF VAR hParent           AS HANDLE NO-UNDO.

DEF VAR hParentBuffer     AS HANDLE NO-UNDO.
DEF VAR hBrowse           AS HANDLE NO-UNDO.
DEF VAR hFieldMap         AS HANDLE NO-UNDO.
DEF VAR hToolbar          AS HANDLE NO-UNDO.
DEF VAR hTabFolder        AS HANDLE NO-UNDO.

DEF VAR hCurrTabProc      AS HANDLE NO-UNDO.
DEF VAR hCurrTabFrame     AS HANDLE NO-UNDO.
DEF VAR iCurrTab          AS INT NO-UNDO.


DEF VAR hLinjeNrOverlay   AS HANDLE NO-UNDO.
DEF VAR hVareNrOverlay    AS HANDLE NO-UNDO.
DEF VAR hVareTekstOverlay AS HANDLE NO-UNDO.
DEF VAR hAntallOverlay    AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME frmResKobling

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rectBrowse rectToolbar 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFrameHandle C-Win 
FUNCTION getFrameHandle RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getParentBrowse C-Win 
FUNCTION getParentBrowse RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setParent C-Win 
FUNCTION setParent RETURNS LOGICAL
  ( INPUT ihParent AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setQuery C-Win 
FUNCTION setQuery RETURNS LOGICAL
  (INPUT ihQuery AS HANDLE)  FORWARD.

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
DEFINE RECTANGLE rectBrowse
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 130.8 BY 3.48.

DEFINE RECTANGLE rectToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 17 BY 1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME frmResKobling
     rectBrowse AT ROW 2.43 COL 1.2
     rectToolbar AT ROW 1.19 COL 1.6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 131.8 BY 5.48.


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
         TITLE              = "<insert window title>"
         HEIGHT             = 5.48
         WIDTH              = 131.8
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
/* SETTINGS FOR FRAME frmResKobling
   FRAME-NAME                                                           */
ASSIGN 
       FRAME frmResKobling:HEIGHT           = 5.48
       FRAME frmResKobling:WIDTH            = 131.8.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME frmResKobling
/* Query rebuild information for FRAME frmResKobling
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME frmResKobling */
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


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
  PUBLISH "InvalidateHandle".
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

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.
{incl/supptrigg.i hFieldMap}

ON 'alt-s':U OF FRAME {&FRAME-NAME} ANYWHERE DO:
  MESSAGE PROGRAM-NAME(1) SKIP
           SKIP
          VIEW-AS ALERT-BOX.
  PUBLISH "AltSKundeOrdre" ("kundeordre").
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
  /* Hide all frames. */
  HIDE FRAME frmResKobling.
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
  ENABLE rectBrowse rectToolbar 
      WITH FRAME frmResKobling.
  {&OPEN-BROWSERS-IN-QUERY-frmResKobling}
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
DO WITH FRAME {&FRAME-NAME}:

  DYNAMIC-FUNCTION("NewObject",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE,"procedure").

  /* Create the browse: */
   hBrowse = DYNAMIC-FUNCTION("NewBrowse",
                           rectBrowse:HANDLE,        
                           100,                      
                           "",                       
                           "KunderesKobling"             
                           + ";DReskontro_Id"
                           + ";KReskontro_Id"
                           + ";Dato"
                           + ";Belop"
                           + ";RegistrertDato"
                           + ";RegistrertAv"
                         + ",Kundereskontr"
                           + ";BilagsType"
                         + ",Bilagstype"
                           + ";BTTekst"
/*                          + ",buf1_Kundereskontr" */
/*                            + ";BilagsType"       */
/*                          + ",buf1_Bilagstype"    */
/*                            + ";BTTekst"          */
                           ,"WHERE false"
                          + ",FIRST Kundereskontr WHERE Reskontro_id = DReskontro_Id NO-LOCK"
                          + ",FIRST BilagsType OF Kundereskontr NO-LOCK"
/*                           + ",FIRST buf1_Kundereskontr WHERE Reskontro_id = KReskontro_Id NO-LOCK"                                       */
/*                           + ",FIRST buf1_BilagsType WHERE buf1_BilagsType.BilagsType = buf1_Kundereskontr.BilagsType NO-LOCK OUTER-JOIN" */
                           ,"").         
  DYNAMIC-FUNCTION("createObjectLink",hBrowse,THIS-PROCEDURE).
  hBrowse:NAME = "brwKunderesKobling".
  hBrowse:MOVE-COLUMN(7,2).
  hBrowse:MOVE-COLUMN(8,3).
  
  hToolbar = DYNAMIC-FUNCTION("NewToolbar",
                            rectToolbar:HANDLE,
                            "",
                            "excel",
                            "maxborder").

  DYNAMIC-FUNCTION("setToolbar",hToolbar,"disable").
  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hToolbar).
  
  DYNAMIC-FUNCTION("setAddMoveY",THIS-PROCEDURE:CURRENT-WINDOW,FRAME {&FRAME-NAME}:HANDLE,"frmResKobling").
  DYNAMIC-FUNCTION("setNoMoveY",THIS-PROCEDURE:CURRENT-WINDOW,FRAME {&FRAME-NAME}:HANDLE,"rectToolbar,brwKunderesKobling,rectBrowse,"
                   + DYNAMIC-FUNCTION("getToolbarNames",hToolbar,"")).
  DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,FRAME {&FRAME-NAME}:HANDLE,"frmResKobling,rectToolbar,brwKunderesKobling,rectBrowse").
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
DEF VAR hColumn AS HANDLE NO-UNDO.
hColumn = hBrowse:GET-BROWSE-COLUMN(1).
APPLY "end-resize" TO hColumn.
FRAME {&FRAME-NAME}:MOVE-TO-TOP().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFrameHandle C-Win 
FUNCTION getFrameHandle RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN FRAME {&FRAME-NAME}:HANDLE. 

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setParent C-Win 
FUNCTION setParent RETURNS LOGICAL
  ( INPUT ihParent AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  hParent = ihParent.

  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setQuery C-Win 
FUNCTION setQuery RETURNS LOGICAL
  (INPUT ihQuery AS HANDLE) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
hParentBuffer = ihQuery:QUERY:GET-BUFFER-HANDLE(1).
RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION TabChanged C-Win 
FUNCTION TabChanged RETURNS LOGICAL
  ( INPUT iiTab AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DYNAMIC-FUNCTION("DeleteLinksFrom",hBrowse).

DYNAMIC-FUNCTION("setAttribute",hToolbar,"disabledevents","").
DYNAMIC-FUNCTION("CreateParentLink",DYNAMIC-FUNCTION("getPageQuery" IN hTabFolder,iiTab),hBrowse,"FakturaNr").

hCurrTabProc  = DYNAMIC-FUNCTION("getPageHandle" IN hTabFolder,iiTab).
hCurrTabFrame = DYNAMIC-FUNCTION("getPageFrame" IN hTabFolder,iiTab).

DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
RUN DisplayRecord.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

