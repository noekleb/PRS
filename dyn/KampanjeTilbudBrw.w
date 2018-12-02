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
                          
DEF VAR hToolbar          AS HANDLE NO-UNDO.
DEF VAR hBrowse           AS HANDLE NO-UNDO.
 
DEF VAR hParent           AS HANDLE NO-UNDO.
DEF VAR hParentQuery      AS HANDLE NO-UNDO.

DEF VAR hChild            AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rectBrowse rectToolBar 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD addFromTemplate C-Win 
FUNCTION addFromTemplate RETURNS LOGICAL
    ( INPUT ifKampId AS DEC,
      INPUT iiKampTilbId AS INT)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD adjustBrowseColumns C-Win 
FUNCTION adjustBrowseColumns RETURNS LOGICAL
  ( INPUT ihBrowse     AS HANDLE,
    INPUT icBrowseName AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  ( INPUT ihQuery AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE RECTANGLE rectBrowse
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.4 BY 6.43.

DEFINE RECTANGLE rectToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 28 BY .95.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     rectBrowse AT ROW 2.43 COL 2.2
     rectToolBar AT ROW 1.24 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 49 BY 7.95.


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
         TITLE              = "Tilbud"
         HEIGHT             = 7.95
         WIDTH              = 48.4
         MAX-HEIGHT         = 57.14
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 57.14
         VIRTUAL-WIDTH      = 320
         MAX-BUTTON         = no
         RESIZE             = no
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
ON END-ERROR OF C-Win /* Tilbud */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Tilbud */
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
  IF VALID-HANDLE(hChild) THEN APPLY "close" TO hChild.
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

{incl/supptrigg.i hBrowse}

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
IF NOT VALID-HANDLE(hChild) THEN
DO:
  RUN kampanjetilbud.w PERSISTENT SET hChild.
  IF VALID-HANDLE(hChild) THEN
  DO:
    /*setParent etc...*/
    RUN initializeObject IN hChild.
    RUN MoveToTop IN hChild.
    APPLY 'value-changed' TO hBrowse.
    RUN getInfo IN hChild.
  END.
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

DEF VAR iTmp AS INT NO-UNDO.
  iTmp = INT(DYNAMIC-FUNCTION('getFieldValues','KampTilbTemplate','WHERE KampTilbTemplate.kampid = DEC(' + STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('kampid'):BUFFER-VALUE) + ')'
                                                                   + ' AND KampTilbTemplate.kamptilbid = INT(' + STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('KampTilbId'):BUFFER-VALUE) + ')'
                                                                   ,'KampTilbTempNr')).
  IF iTmp GT 0 THEN
  DO:
    MESSAGE 'Template nr.: ' STRING(iTmp) ' finnes knyttet til dette tilbudet.' SKIP 'Om posten blir slettet, vil også template knyttet til posten bli slettet.'
      VIEW-AS ALERT-BOX INFO BUTTONS YES-NO UPDATE bOk.

    IF bOk THEN RUN SUPER.
  END.
  ELSE RUN SUPER.
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
  /* Hide all frames. */
  HIDE FRAME DEFAULT-FRAME.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
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
  RUN DefaultActionBrowse.
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
  ENABLE rectBrowse rectToolBar 
      WITH FRAME DEFAULT-FRAME.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getTemplateRecord C-Win 
PROCEDURE getTemplateRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR cReturnValues   AS CHAR NO-UNDO.

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxLookup.w (THIS-PROCEDURE,50,
                    "KampTilbTemplate"
                      + ";KampTilbTempNr;!KampId;!KampTilbId"
                    + ",KampanjeTilbud"
                      + ";KampTilbNavn"
                    ,"WHERE true, FIRST KampanjeTilbud OF KampTilbTemplate"
                    ,""                                                  
                    ,"KampId,KampTilbId",   /* <- return values for these fields */
                      OUTPUT cReturnValues,
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.
  IF bOk AND cReturnValues NE "" THEN 
  DO:
    DYNAMIC-FUNCTION('addFromTemplate',DEC(ENTRY(1,cReturnValues,'|')),INT(ENTRY(2,cReturnValues,'|'))).
    DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
    RUN OpenQuery.
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
DO WITH FRAME {&FRAME-NAME}:

  DYNAMIC-FUNCTION("NewObject",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE,"procedure").
  hBrowse = DYNAMIC-FUNCTION("NewBrowse",          /* Create a browse object */
                    rectBrowse:HANDLE,              /* Rectangle to define coordinates for browse */
                    100,                            /* Rows to batch */
                    "",                             /* Browse properties, ie MULTIPLE,NUM-LOCKED-COLUMNS, etc */
                    "KampanjeTilbud"
                    + ";KampTilbId"
                    + ";!KampId"
                    + ";KampTilbNavn"
                    + ";KampTilbPropBetalFor"
                    + ";KamptilbPopUpTekstBruk"
                    + ";KamptilbPopUpTekst"
                    + ";KampTilbOkning"
                    + ";KampTilbKvitteringstekst"
                    + ";KamptilbGrenseAntallBruk"
                    + ";KamptilbGrenseAntall"
                    + ";KampTilbBelop"
                    + ";!HapHourId"
                    + ";!KampTilbTypeId"
                    + ";!RegistrertDato;!RegistrertTid;!RegistrertAv;!ETid;!EDato;!BrukerID"
                   + ",KampanjeTilbType"
                    + ";!KampTilbTypeId;KampTilbTypeNavn"
                   ,"WHERE false, " 
                   + ",FIRST KampanjeTilbType OF KampanjeTilbud NO-LOCK"
                    ,"sort|KampTilbId").             /* Initial sort column */
  hBrowse:NAME = 'brwKT'.
  DYNAMIC-FUNCTION("createObjectLink",hBrowse,THIS-PROCEDURE).
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"customDeleteValProc","=delval_kampanjetilbud.p").
  
  hToolbar = DYNAMIC-FUNCTION("NewToolBar",
                    rectToolBar:HANDLE,             /* Rectangle to define coordinates for toolbar */
                    "File",                         /* Corresponding menu label - no menu if blank */
                    "new,edit,Delete"
                    + ",rule,excel;Eksporter til E&xcel,BrowseConfig" /* Buttons / Menu items: action;label;tooltip;Method;image,action;label.. 
                                                       Any number of properties accepted (one ok - if predef. action) */
/*                     + ",rule,getTemplate;Kopier tilbud¤ENABLE,newTemplate;Lag tilbudsmal" */
                    ,"maxborder").                  /* Misc - enable, maxborder.. */

  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hToolbar).    

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
FRAME {&FRAME-NAME}:MOVE-TO-TOP().
APPLY "entry" TO hBrowse.
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
DEF VAR hTmp AS HANDLE NO-UNDO.

IF NOT VALID-HANDLE(hChild) THEN
DO:
  RUN kampanjetilbud.w PERSISTENT SET hChild.
  IF VALID-HANDLE(hChild) THEN
  DO:
    /*setParent etc...*/
    RUN initializeObject IN hChild.
    RUN MoveToTop IN hChild.

    hTmp = DYNAMIC-FUNCTION('getLinkedObject',hBrowse,'OneToOne','To').
    hTmp = DYNAMIC-FUNCTION('getLinkedObject',hTmp,'Toolbar','From').
    DYNAMIC-FUNCTION('setCurrentObject',hTmp).
    RUN SUPER.
    DYNAMIC-FUNCTION('setButton' IN hChild,FALSE).
  END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE newTemplateRecord C-Win 
PROCEDURE newTemplateRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iKeyId    AS INT   NO-UNDO.

    IF hBrowse:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN
    iKeyId = DYNAMIC-FUNCTION('getFieldValues','KampTilbTemplate','WHERE kampid = DEC(' + STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('kampid'):BUFFER-VALUE) + ')'
                                                                 + ' AND kamptilbid = INT(' + STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('kamptilbid'):BUFFER-VALUE) + ')','KampTilbTempNr').
    
    IF iKeyId NE 0 AND iKeyId NE ? THEN
    DO:
      MESSAGE 'Allerede registrert !'
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
      LEAVE.
    END.
    DO WITH FRAME {&FRAME-NAME}:
      bOk = DYNAMIC-FUNCTION('runProc','kamptilbtemplate_create.p',STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('kampid'):BUFFER-VALUE) 
                                                                  + ';' + STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('kamptilbid'):BUFFER-VALUE),?).
      IF NOT bOk THEN DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("GetTransactionMessage"),"Feil","").

/*       DYNAMIC-FUNCTION('applyEvent',hBrowse,'value-changed'). */

    END. /*FRAME*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION addFromTemplate C-Win 
FUNCTION addFromTemplate RETURNS LOGICAL
    ( INPUT ifKampId AS DEC,
      INPUT iiKampTilbId AS INT) :
  /*------------------------------------------------------------------------------
    Purpose: Legg til eller endre artikkel. Kalles fra artbassok.w 
      Notes:  
  ------------------------------------------------------------------------------*/
/* DEF VAR iKeyId    AS INT   NO-UNDO.                                                                                                                        */
/*                                                                                                                                                            */
/* iKeyId = DYNAMIC-FUNCTION('getFieldValues','KampanjeTilbud',                                                                                               */
/*                           'WHERE KampanjeTilbud.kampid = DEC(' + STRING(hParentQuery:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('KampId'):BUFFER-VALUE) + ')' */
/*                         + ' AND KampanjeTilbud.kamptilbid = INT(' + STRING(iiKampTilbId) + ')'                                                             */
/*                               ,'KampTilbId').                                                                                                              */
/*                                                                                                                                                            */
/* IF iKeyId NE 0 AND iKeyId NE ? THEN                                                                                                                        */
/* DO:                                                                                                                                                        */
/*   MESSAGE 'Allerede registrert !'                                                                                                                          */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                                                                                     */
/*   LEAVE.                                                                                                                                                   */
/* END.                                                                                                                                                       */
DO WITH FRAME {&FRAME-NAME}:
  bok =  DYNAMIC-FUNCTION('runProc','kampanjetilbud_template_create.p',STRING(hParentQuery:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('KampId'):BUFFER-VALUE) + ';' + STRING(ifKampId) + ';' + STRING(iiKampTilbId),?).
  DYNAMIC-FUNCTION('applyEvent',hBrowse,'value-changed').

  RETURN bOk.    
END. /*FRAME*/

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION adjustBrowseColumns C-Win 
FUNCTION adjustBrowseColumns RETURNS LOGICAL
  ( INPUT ihBrowse     AS HANDLE,
    INPUT icBrowseName AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose: Hook fra NewBrowse slik at kolonner kan flyttes regelbasert.
           Gjør også justeringer av kolonnebredder her slik at disse 
           blir tatt vare på.
    Notes:  
------------------------------------------------------------------------------*/
  ihBrowse:GET-BROWSE-COLUMN(1):WIDTH-PIXELS = 60.
  ihBrowse:GET-BROWSE-COLUMN(2):WIDTH-PIXELS = 200.
  RETURN TRUE.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setQuery C-Win 
FUNCTION setQuery RETURNS LOGICAL
  ( INPUT ihQuery AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
hParentQuery = ihQuery.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

