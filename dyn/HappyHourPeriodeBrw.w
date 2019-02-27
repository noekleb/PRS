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
DEF VAR hFieldMap         AS HANDLE NO-UNDO.

DEF VAR hChild            AS HANDLE NO-UNDO.


DEF VAR cStartTid       AS CHAR NO-UNDO.
DEF VAR cStoppTid       AS CHAR NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rectToolBar rectBrowse HapHourPerID ~
HapHourPerStartTid HapHourPerSluttTid tgl_2 tgl_4 tgl_6 tgl_1 tgl_3 tgl_5 ~
tgl_7 tglAll 
&Scoped-Define DISPLAYED-OBJECTS HapHourPerID HapHourPerStartTid ~
HapHourPerSluttTid tgl_2 tgl_4 tgl_6 tgl_1 tgl_3 tgl_5 tgl_7 tglAll 

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fillTime C-Win 
FUNCTION fillTime RETURNS CHARACTER
  (INPUT iphCombo         AS HANDLE,
   INPUT ipcTimeRangeFrom AS CHAR,
   INPUT ipcTimeRangeTo   AS CHAR,
   INPUT ipiTimeStep      AS INT)  FORWARD.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getWeekdayList C-Win 
FUNCTION getWeekdayList RETURNS CHARACTER
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
DEFINE VARIABLE HapHourPerSluttTid AS INTEGER FORMAT ">>>>>>9":U INITIAL 0 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "00:00",0
     DROP-DOWN-LIST
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE HapHourPerStartTid AS INTEGER FORMAT ">>>>>>9":U INITIAL 0 
     LABEL "Start" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "00:00",0
     DROP-DOWN-LIST
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE HapHourPerID AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Id" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE RECTANGLE rectBrowse
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 59 BY 6.14.

DEFINE RECTANGLE rectToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 28 BY .95.

DEFINE VARIABLE tglAll AS LOGICAL INITIAL no 
     LABEL "Alle dager" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE tgl_1 AS LOGICAL INITIAL no 
     LABEL "Søndag" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE tgl_2 AS LOGICAL INITIAL no 
     LABEL "Mandag" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE tgl_3 AS LOGICAL INITIAL no 
     LABEL "Tirsdag" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY .81 NO-UNDO.

DEFINE VARIABLE tgl_4 AS LOGICAL INITIAL no 
     LABEL "Onsdag" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE tgl_5 AS LOGICAL INITIAL no 
     LABEL "Torsdag" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE tgl_6 AS LOGICAL INITIAL no 
     LABEL "Fredag" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE tgl_7 AS LOGICAL INITIAL no 
     LABEL "Lørdag" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     HapHourPerID AT ROW 2.62 COL 7 COLON-ALIGNED
     HapHourPerStartTid AT ROW 3.62 COL 7 COLON-ALIGNED
     HapHourPerSluttTid AT ROW 3.62 COL 20 COLON-ALIGNED NO-LABEL
     tgl_2 AT ROW 4.91 COL 9.4
     tgl_4 AT ROW 4.91 COL 22.4
     tgl_6 AT ROW 4.91 COL 34.4
     tgl_1 AT ROW 4.91 COL 46
     tgl_3 AT ROW 5.62 COL 9.4
     tgl_5 AT ROW 5.62 COL 22.4
     tgl_7 AT ROW 5.62 COL 34.4
     tglAll AT ROW 5.62 COL 46
     rectToolBar AT ROW 1.24 COL 3
     rectBrowse AT ROW 7.48 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 59.8 BY 12.71.


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
         TITLE              = "Perioder"
         HEIGHT             = 12.81
         WIDTH              = 59.6
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
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 12.71
       FRAME DEFAULT-FRAME:WIDTH            = 59.8.

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
ON END-ERROR OF C-Win /* Perioder */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Perioder */
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
DEF VAR i AS INT NO-UNDO.

RUN SUPER.
DO WITH FRAME {&FRAME-NAME}:
  
  ASSIGN
    tglAll:CHECKED = FALSE
    tgl_1:CHECKED  = FALSE
    tgl_2:CHECKED  = FALSE
    tgl_3:CHECKED  = FALSE
    tgl_4:CHECKED  = FALSE
    tgl_5:CHECKED  = FALSE
    tgl_6:CHECKED  = FALSE
    tgl_7:CHECKED  = FALSE
  .
  IF hFieldMap:AVAIL THEN
  DO i = 1 TO NUM-ENTRIES(hFieldMap:BUFFER-FIELD('HapHourPerUkedagListe'):BUFFER-VALUE):
    CASE ENTRY(i,hFieldMap:BUFFER-FIELD('HapHourPerUkedagListe'):BUFFER-VALUE):
      WHEN '1' THEN tgl_1:CHECKED = TRUE.
      WHEN '2' THEN tgl_2:CHECKED = TRUE.
      WHEN '3' THEN tgl_3:CHECKED = TRUE.
      WHEN '4' THEN tgl_4:CHECKED = TRUE.
      WHEN '5' THEN tgl_5:CHECKED = TRUE.
      WHEN '6' THEN tgl_6:CHECKED = TRUE.
      WHEN '7' THEN tgl_7:CHECKED = TRUE.
    END CASE.
  END.
  DYNAMIC-FUNCTION('setAttribute',hFieldMap,'CheckModified',NO).
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
  DISPLAY HapHourPerID HapHourPerStartTid HapHourPerSluttTid tgl_2 tgl_4 tgl_6 
          tgl_1 tgl_3 tgl_5 tgl_7 tglAll 
      WITH FRAME DEFAULT-FRAME.
  ENABLE rectToolBar rectBrowse HapHourPerID HapHourPerStartTid 
         HapHourPerSluttTid tgl_2 tgl_4 tgl_6 tgl_1 tgl_3 tgl_5 tgl_7 tglAll 
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
                    "HappyHourPeriode"
                    + ";!HapHourId;!HapHourPerStartTid;!HapHourPerSluttTid"
                    + ";HapHourPerId|PerId|>>>9;+cStartTid|character|x(5)|jb_hhmmss (HapHourPerStartTid)|Start;+cStoppTid|character|x(5)|jb_hhmmss (HapHourPerSluttTid)|Slutt;!HapHourPerUkedagListe;!RegistrertDato;!RegistrertTid;!RegistrertAv;!ETid;!EDato;!BrukerID"
                   ,"WHERE false "
                    ,"sort|HapHourPerId").             /* Initial sort column */
  hBrowse:NAME = 'brwHHP'.
  hFieldMap = DYNAMIC-FUNCTION("NewFieldMap",      /* A fieldmap object holds extra info for display and input fields (fill-ins) 
                                                       and their corresponding buffer columns return handle equals the buffer handle */
                    hBrowse:QUERY,
                    FRAME {&FRAME-NAME}:HANDLE,     /* Frame for the input/display fields (might not be the same frame as the browse) */
                    "HapHourPerStartTid,HapHourPerSluttTid",   /* Update columns in buffer */
                      "",                           /* Corresponding input fields (fill-in..). blank if equal to update columns */
                      "HapHourPerId",        /* Additional buffer and displ.fields - not updateable*/
                      "",                           /* Corresponding fill-ins */
                    "tgl_1,tgl_2,tgl_3,tgl_4,tgl_5,tgl_6,tgl_7,tglAll").                            /* other input widgets and lookup buttons for update fill-ins */

  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,THIS-PROCEDURE).
  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"BufferExtraFields","HapHourId,HapHourPerUkedagListe").
  DYNAMIC-FUNCTION("CreateObjectLink",hFieldMap,hBrowse).
  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"customcreateproc","happyhourperiode_create.p").

  fillTime(HapHourPerStartTid:HANDLE,'00:00','23:59',30).
  fillTime(HapHourPerSluttTid:HANDLE,'00:00','23:59',30).

  hToolbar = DYNAMIC-FUNCTION("NewToolBar",
                    rectToolBar:HANDLE,             /* Rectangle to define coordinates for toolbar */
                    "File",                         /* Corresponding menu label - no menu if blank */
                    "new,undo,delete,save"
                    + "" /* Buttons / Menu items: action;label;tooltip;Method;image,action;label.. 
                                                       Any number of properties accepted (one ok - if predef. action) */
                    ,"MaxBorder").                  /* Misc - enable, maxborder.. */

  DYNAMIC-FUNCTION("CreateObjectLink",hFieldMap,hToolbar).
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
RUN SUPER.

IF WIDGET-HANDLE(DYNAMIC-FUNCTION('getAttribute',THIS-PROCEDURE:CURRENT-WINDOW,'CurrentToolbar')) = hToolbar THEN
DO WITH FRAME {&FRAME-NAME}:
  HapHourPerStartTid:SCREEN-VALUE = ENTRY(2,HapHourPerStartTid:LIST-ITEM-PAIRS).
  HapHourPerSluttTid:SCREEN-VALUE = ENTRY(2,HapHourPerSluttTid:LIST-ITEM-PAIRS).
  ASSIGN
    HapHourPerID:SCREEN-VALUE = '0'
    tglAll:CHECKED = FALSE
    tgl_1:CHECKED  = FALSE
    tgl_2:CHECKED  = FALSE
    tgl_3:CHECKED  = FALSE
    tgl_4:CHECKED  = FALSE
    tgl_5:CHECKED  = FALSE
    tgl_6:CHECKED  = FALSE
    tgl_7:CHECKED  = FALSE
  .
  DYNAMIC-FUNCTION('setAttribute',hFieldMap,'CheckModified',NO).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SaveRecord C-Win 
PROCEDURE SaveRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"BufferExtraValues", STRING(hParentQuery:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('HapHourId'):BUFFER-VALUE)
                                                                  + '|' + getWeekdayList()).
  RUN SUPER.

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
DEF INPUT PARAM ipcWName AS CHAR NO-UNDO.

IF ipcWName = 'tglAll' THEN
DO WITH FRAME {&FRAME-NAME}:
  ASSIGN 
    tgl_1:CHECKED = tglAll:CHECKED
    tgl_2:CHECKED = tglAll:CHECKED
    tgl_3:CHECKED = tglAll:CHECKED
    tgl_4:CHECKED = tglAll:CHECKED
    tgl_5:CHECKED = tglAll:CHECKED
    tgl_6:CHECKED = tglAll:CHECKED
    tgl_7:CHECKED = tglAll:CHECKED
  . 
END.
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
  ihBrowse:GET-BROWSE-COLUMN(2):WIDTH-PIXELS = 60.
  ihBrowse:GET-BROWSE-COLUMN(2):WIDTH-PIXELS = 60.
  RETURN TRUE.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fillTime C-Win 
FUNCTION fillTime RETURNS CHARACTER
  (INPUT iphCombo         AS HANDLE,
   INPUT ipcTimeRangeFrom AS CHAR,
   INPUT ipcTimeRangeTo   AS CHAR,
   INPUT ipiTimeStep      AS INT) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR iTimeFrom AS INT NO-UNDO.
DEF VAR iTimeTo   AS INT NO-UNDO.
DEF VAR iTime     AS INT NO-UNDO.
DEF VAR iTimeStep AS INT NO-UNDO.

/*Bygges om for å kunne legge inn faste klokkeslett som 00:00*/
ASSIGN 
  iTimeFrom = 0
  iTimeTo   = (23 * 3600) + (59 * 60)
  iTimeStep = ipiTimeStep * 60.
.
DO iTime = 1 TO iTimeTo:
  IF iTime GE iTimeFrom AND (iTime MOD iTimeStep = 0 OR iTime = 1) THEN 
    iphCombo:ADD-LAST(STRING(iTime,'hh:mm'),iTime).
   /*do: DISP string(iTime,'hh:mm') '....' iTimeFrom '...' iTimeTo. PAUSE 1. END.*/
END.
iphCombo:DELETE(1).  

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getWeekdayList C-Win 
FUNCTION getWeekdayList RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR cWeekdayList AS CHAR   NO-UNDO.
  DEF VAR hObject       AS HANDLE NO-UNDO.
  DEF VAR i             AS INT    NO-UNDO.

  hObject = FRAME {&FRAME-NAME}:FIRST-CHILD.
  hObject = hObject:FIRST-CHILD.

  DO WHILE VALID-HANDLE(hObject):
    IF hObject:NAME BEGINS 'tgl_' THEN
    DO:
      IF hObject:CHECKED THEN
        cWeekdayList = cWeekdayList + ',' + SUBSTRING(hObject:NAME,5,1).
    END.
    hObject = hObject:NEXT-SIBLING.
  END.
  hObject = ?.
  RETURN TRIM(cWeekdayList,',').
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

