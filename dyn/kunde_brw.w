&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description:        JukeBox suppressed window template
                      This template is for use with a tabfolder or viewer object
                      to parent the suppressed window

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author:            brynjar@chemistry.no

  Created:           30.march.2008

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

DEF VAR bOk                 AS LOG    NO-UNDO.
DEF VAR ix                  AS INT    NO-UNDO.
DEF VAR hBrowse             AS HANDLE NO-UNDO.
DEF VAR hToolbar            AS HANDLE NO-UNDO.
DEF VAR hParent             AS HANDLE NO-UNDO.
DEF VAR hParentQueryObject  AS HANDLE NO-UNDO.
DEF VAR hBuffer             AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS brwKunde rectToolBar searchField 

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD InitializeResize C-Win 
FUNCTION InitializeResize RETURNS LOGICAL
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
  ( INPUT ihParentQueryObject AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE RECTANGLE brwKunde
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 119 BY 15.71.

DEFINE RECTANGLE rectToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 15.4 BY .95.

DEFINE RECTANGLE searchField
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 19 BY .95.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     brwKunde AT ROW 2.67 COL 2
     rectToolBar AT ROW 1.19 COL 22
     searchField AT ROW 1.19 COL 2.2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 120.8 BY 17.91.


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
         TITLE              = "JukeSox plain suppressed window"
         HEIGHT             = 17.76
         WIDTH              = 120.8
         MAX-HEIGHT         = 28.57
         MAX-WIDTH          = 160
         VIRTUAL-HEIGHT     = 28.57
         VIRTUAL-WIDTH      = 160
         SHOW-IN-TASKBAR    = no
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
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* JukeSox plain suppressed window */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* JukeSox plain suppressed window */
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
  /* The viewer or tabfolder object will see to that the Disable_UI is run
     and the container will make sure that all dynamic objects + resize settings are deleted
     also for the suppressed windows */
  PUBLISH "InvalidateHandle".
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

{incl/supptrigg.i hBrowse}  /* <- To be able to capture keys (insert, delete..) on the browse */

ON 'alt-s':U OF FRAME {&FRAME-NAME} ANYWHERE
  PUBLISH "AltSKundeOrdre" (THIS-PROCEDURE:FILE-NAME).

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
  HIDE FRAME DEFAULT-FRAME.
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
DYNAMIC-FUNCTION("setHeading" IN hParent,hBuffer).
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
  ENABLE brwKunde rectToolBar searchField 
      WITH FRAME DEFAULT-FRAME.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeObject C-Win 
PROCEDURE InitializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Create any dynamic objects, initialize drop-down lists etc
  Parameters:  <none>
  Notes:       The dynamic query object must be linked to the THIS-PROCEDURE
------------------------------------------------------------------------------*/
DEF VAR hSearchField AS HANDLE NO-UNDO.
DEF VAR cBrukerTypeBut AS CHAR  NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  DYNAMIC-FUNCTION("NewObject",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE,"procedure").
  ASSIGN cBrukerTypeBut = DYNAMIC-FUNCTION("getFieldValues","Bruker","WHERE BrukerId = '" + DYNAMIC-FUNCTION("getASuserId") + "'","BrukerType,ButikkNr")

  hBrowse = DYNAMIC-FUNCTION("NewBrowse",
                    brwKunde:HANDLE,   
                    100,                 
                    "",                  
                    "Kunde"
                    + ";Navn"
                    + ";Butikknr|Butikk"
                    + ";Kundenr"
                    + ";EksterntKundeNr|Ekst.kndnr"
                    + ";Aktiv|Aktiv|J/N"
                    + ";KundeSaldo"
                    + ";ForsteKjop"
                    + ";SisteKjop"
                    + ";GruppeId|Knd.grp|>>>>>9"
                    + ";TypeId|Knd.type|>>>>>9"
                    + ";Telefon"
                    + ";MobilTlf"
                    + ";ePostAdresse"
                    + ";Kilde"
                    + ";TilgKilde"
                    + ";Adresse1"
                    + ";Adresse2"
                    + ";Postnr"
                    + ";FaktAdresse1"
                    + ";FaktAdresse2"
                    + ";FaktPostNr"
                    + ";OrgNr"
                    + ";KontNavn"
                    + ";KontTelefon"
                    + ";KontMobilTlf"
                    + ";KontE-post"
                    + ";Privat"
                    + ";RegistrertDato|Opprettet"
                    + ";EDato|Endret"
                    + ";Hovedkunde"
                    + ";KobletTilKunde|Hovedkunde nr"
                  + ",Post"
                    + ";Beskrivelse"
                  + ",buf1_Post"
                    + ";Beskrivelse"
                  + ",buf2_kunde"
                    + ";Navn|Hovedkunde navn"
                  + ",KundeGruppe"
                    + ";Beskrivelse|Kunde gruppe|x(15)@10"
                  + ",KundeType"
                    + ";Beskrivelse|Kunde type|x(15)@12"
                    ,"WHERE FALSE"
                    + ",FIRST Post OF Kunde NO-LOCK OUTER-JOIN"
                    + ",FIRST buf1_Post OF Kunde NO-LOCK OUTER-JOIN"
                    + ",FIRST buf2_Kunde WHERE buf2_Kunde.KundeNr = Kunde.KobletTilKunde NO-LOCK OUTER-JOIN"
                    + ",FIRST KundeGruppe OF Kunde NO-LOCK OUTER-JOIN"
                    + ",FIRST KundeType OF Kunde  NO-LOCK OUTER-JOIN"
                    ,"sort|Navn").

  hBrowse:NAME = "brwKunde". /* This name is neccessary because the browser is due to special treatment during resize */
  hBrowse:GET-BROWSE-COLUMN(1):WIDTH-PIXELS = 100.
  hBrowse:GET-BROWSE-COLUMN(2):WIDTH-PIXELS = 60.

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"prescanqueryBuf2_Kunde","EACH Kunde WHERE Kunde.KobletTilKunde = buf2_kunde.KundeNr").

  hSearchField = DYNAMIC-FUNCTION("NewBrowseSearchField",SearchField:HANDLE,hBrowse,1).
  DYNAMIC-FUNCTION("CreateObjectLink",hSearchField,hBrowse).

  DYNAMIC-FUNCTION("createObjectLink",hBrowse,THIS-PROCEDURE).

  hBuffer = hBrowse:QUERY:GET-BUFFER-HANDLE(1).

  hToolbar = DYNAMIC-FUNCTION("NewToolBar",
                    rectToolBar:HANDLE,             
                    "Fil", 
                    (IF ENTRY(1,cBrukerTypeBut,"|") <> "2"
                      THEN "filter,excel"
                    ELSE "excel")
                  + ",|Innkreving/Innbetaling,|Rapporter�enable"
                   ,"enable,maxborder").
  DYNAMIC-FUNCTION("createObjectLink",hBrowse,hToolbar).

END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MoveToTop C-Win 
PROCEDURE MoveToTop :
/*------------------------------------------------------------------------------
  Purpose:     Mak
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FRAME {&FRAME-NAME}:MOVE-TO-TOP().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFrameHandle C-Win 
FUNCTION getFrameHandle RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose: Make the frame of the child procedure known to the tabfolder or viewer object
    Notes: The procedure is mandatory
------------------------------------------------------------------------------*/

  RETURN FRAME {&FRAME-NAME}:HANDLE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION InitializeResize C-Win 
FUNCTION InitializeResize RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose: This function is invoked automatically from the viewer or tabfolder object before
           InitializeObject and also before the suppressed window is resized to fit the size of the 
           graphics placeholder.
  Notes:   Note that if the suppressed window contains a searchfield or toolbar object
           the placeholders for these must have resize settings here since the objects themselves have not been 
           initialized. (When a searchfield or tabfolder is created the resize is automatically taken care of).
           
           This function is not mandatory but is called if it exists
------------------------------------------------------------------------------*/

DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,"SearchField").
DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,"SearchField,rectToolbar").
  
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setParent C-Win 
FUNCTION setParent RETURNS LOGICAL
  ( INPUT ihParent AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose: Set the handle to the parent procedure for the suppressed window
    Notes: This function is not mandatory but is called if it exists
------------------------------------------------------------------------------*/
hParent = ihParent.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setQuery C-Win 
FUNCTION setQuery RETURNS LOGICAL
  ( INPUT ihParentQueryObject AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose: Set the handle of the parent (or oneToOne navigation query/browse) so
           it is available for the child object 
    Notes: This function is not mandatory but is called if it exists 
------------------------------------------------------------------------------*/
hParentQueryObject = ihParentQueryObject.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

