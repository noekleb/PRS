&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description:        Container for a JukeBox window program

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author:            brynjar@chemistry.no

  Created:           18.oct.2006

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

DEF VAR bOk         AS LOG    NO-UNDO.
DEF VAR ix          AS INT    NO-UNDO.
DEF VAR hBrowse     AS HANDLE NO-UNDO.
DEF VAR hQuery      AS HANDLE NO-UNDO.
DEF VAR hToolbar    AS HANDLE NO-UNDO.
DEF VAR hFieldMap   AS HANDLE NO-UNDO.
DEF VAR hParent     AS HANDLE NO-UNDO.

DEF VAR iSysHid     AS INT    NO-UNDO.
DEF VAR iSysGr      AS INT    NO-UNDO.
DEF VAR tth         AS HANDLE NO-UNDO.
DEF VAR cAction     AS CHAR   NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS perid Beskrivelse Hjelpetekst BtnAction ~
BtnAvbryt 
&Scoped-Define DISPLAYED-OBJECTS sysGr perid Beskrivelse Hjelpetekst ~
lblHjelpetekst 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnAction DEFAULT 
     LABEL "Lagre" 
     SIZE 14 BY 1.43
     BGCOLOR 8 .

DEFINE BUTTON BtnAvbryt DEFAULT 
     LABEL "Avbryt" 
     SIZE 14 BY 1.43
     BGCOLOR 8 .

DEFINE VARIABLE perid AS CHARACTER FORMAT "X(256)":U 
     LABEL "Periode" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Dag","Uke","Måned","År" 
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE Hjelpetekst AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 39 BY 2.14 NO-UNDO.

DEFINE VARIABLE Beskrivelse AS CHARACTER FORMAT "X(256)":U 
     LABEL "Beskrivelse" 
     VIEW-AS FILL-IN 
     SIZE 39 BY 1 NO-UNDO.

DEFINE VARIABLE lblHjelpetekst AS CHARACTER FORMAT "X(256)":U INITIAL "Hjelpetekst:" 
      VIEW-AS TEXT 
     SIZE 12 BY .62 NO-UNDO.

DEFINE VARIABLE sysGr AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "RapportId" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     sysGr AT ROW 2.67 COL 15 COLON-ALIGNED
     perid AT ROW 3.81 COL 15 COLON-ALIGNED
     Beskrivelse AT ROW 4.81 COL 15 COLON-ALIGNED
     Hjelpetekst AT ROW 5.76 COL 17 NO-LABEL
     BtnAction AT ROW 8.86 COL 17
     BtnAvbryt AT ROW 8.86 COL 31
     lblHjelpetekst AT ROW 5.76 COL 3 COLON-ALIGNED NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 60.4 BY 10.14.


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
         TITLE              = "Lagre ABC rapport"
         HEIGHT             = 10.14
         WIDTH              = 60.4
         MAX-HEIGHT         = 23.52
         MAX-WIDTH          = 114.2
         VIRTUAL-HEIGHT     = 23.52
         VIRTUAL-WIDTH      = 114.2
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
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
/* SETTINGS FOR FILL-IN lblHjelpetekst IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       lblHjelpetekst:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN sysGr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Lagre ABC rapport */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Lagre ABC rapport */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnAction
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnAction C-Win
ON CHOOSE OF BtnAction IN FRAME DEFAULT-FRAME /* Lagre */
DO:
  IF cAction = 'Delete' THEN 
  DO:
    DYNAMIC-FUNCTION('DoDelete','sysGruppe','=delval_sysgruppe.p','',hFieldMap:BUFFER-FIELD('rowident1'):BUFFER-VALUE,YES).
    RUN BlankRecord IN hParent.
  END.
  ELSE 
  DO:
    RUN InvokeMethod(hFieldMap,"SaveRecord").
    DYNAMIC-FUNCTION('setsysGr' IN hParent, INT(iSysGr)).
    DYNAMIC-FUNCTION('receiveParameterTable' IN hParent,tth).
    RUN myDisplayRecord IN hParent. 
    RUN MoveToTop IN hParent.
  END.
  APPLY 'close' TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnAvbryt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnAvbryt C-Win
ON CHOOSE OF BtnAvbryt IN FRAME DEFAULT-FRAME /* Avbryt */
DO:
  APPLY 'close' TO THIS-PROCEDURE.
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
  DYNAMIC-FUNCTION("SetCleanUpResize",THIS-PROCEDURE:CURRENT-WINDOW).
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
  
  hParent = SOURCE-PROCEDURE.
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
  DISPLAY sysGr perid Beskrivelse Hjelpetekst lblHjelpetekst 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE perid Beskrivelse Hjelpetekst BtnAction BtnAvbryt 
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
  Notes:       Refer to the <jukebox>\winsrc\samples for working examples for Sports2000
------------------------------------------------------------------------------*/
  DEF VAR cReturn AS CHAR NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    
    hQuery = DYNAMIC-FUNCTION("NewQuery"
    ,100
    ,""
    ,"sysGruppe"
     + ";!sysGr;beskrivelse;!syshid;Hjelpetekst"
    ,'WHERE syshid = ' + STRING(iSysHid) + ' AND sysGr = ' + STRING(iSysGr)
    ,"").
    
    hFieldMap = DYNAMIC-FUNCTION("NewFieldMap",      /* A fieldmap object holds extra info for display and input fields (fill-ins) 
                                                         and their corresponding buffer columns return handle equals the buffer handle */
                      hQuery,
                      FRAME {&FRAME-NAME}:HANDLE,     /* Frame for the input/display fields (might not be the same frame as the browse) */
                      "Beskrivelse,Hjelpetekst",   /* Update columns in buffer */
                        "",                           /* Corresponding input fields (fill-in..). blank if equal to update columns */
                      "sysGr",        /* Additional buffer and displ.fields - not updateable*/
                        "",                           /* Corresponding fill-ins */
                      "").                            /* other input widgets and lookup buttons for update fill-ins */
  
    DYNAMIC-FUNCTION("setAttribute",hFieldMap,"bufferextrafields","sysHid,sysGr").
    DYNAMIC-FUNCTION('setAttribute',hFieldMap,'CustomDeleteValProc','=delval_sysgruppe.p').

    DYNAMIC-FUNCTION("CreateObjectLink",hFieldMap,hQuery).
    RUN InvokeMethod(hQuery,"OpenQuery").
    
    IF cAction = "New" THEN
    DO:
      RUN InvokeMethod(hFieldMap,"NewRecord").
      DYNAMIC-FUNCTION("setObjectState",hFieldMap,'New').
    END.
    IF cAction = 'Delete' THEN
    DO:
      THIS-PROCEDURE:CURRENT-WINDOW:TITLE = 'Slett ABC rapport'.
      btnAction:LABEL = 'Slett'.
      ASSIGN
        Beskrivelse:SENSITIVE = FALSE
        Hjelpetekst:SENSITIVE = FALSE
        perid:SENSITIVE       = FALSE 
        sysGr:SENSITIVE       = FALSE
      .
    END.                        
    
    cReturn = DYNAMIC-FUNCTION("getFieldValues","sysPara","WHERE sysHid = " + STRING(iSysHid) + ' AND sysGr = ' + STRING(iSysGr) + ' AND parameter1="stlinje.perid"',"parameter2").
    IF cReturn NE ? THEN
      perid:SCREEN-VALUE = cReturn.
    /*   DYNAMIC-FUNCTION("setAttribute",hFieldMap,"postUpdateProc","sysgruppe_postupdate.p"). */
  
  END.

  DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,200,110,0,0).
  THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = NO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initValues C-Win 
PROCEDURE initValues :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM TABLE-HANDLE itth.
DEF INPUT PARAM icAction AS CHAR NO-UNDO.
DEF INPUT PARAM icsysHid AS CHAR NO-UNDO.
DEF INPUT PARAM icSysGr  AS CHAR NO-UNDO.

  ASSIGN 
    tth     = itth
    iSysHid = INTEGER(icsysHid)
    iSysGr  = INTEGER(icSysGr)
    cAction = icAction
  .

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
APPLY 'entry' TO perid IN FRAME {&FRAME-NAME}.
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
  DEF VAR iNextSysGr   AS INTEGER NO-UNDO.
  DEF VAR iNextsysPara AS INTEGER NO-UNDO.
  DEF VAR bh           AS HANDLE  NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:  
  /*Bør det være en trigger for sysgruppe?*/
  IF DYNAMIC-FUNCTION("getObjectState",hFieldMap) = 'New' THEN
    ASSIGN 
      iNextSysGr = INTEGER(DYNAMIC-FUNCTION("getFieldValues","LAST sysGruppe","WHERE syshid = " + STRING(isyshid),"sysGr"))
      iNextSysGr = IF iNextSysGr = ? THEN 1 ELSE iNextSysGr + 1
    .
  ELSE 
    iNextSysGr = isysGr.
  
  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"bufferextravalues",STRING(iSysHid) + '|' + STRING(iNextSysGr)).
  
  RUN SUPER.

  bh = tth:DEFAULT-BUFFER-HANDLE.
  bh:FIND-FIRST('WHERE parameter1 = ' + QUOTER('stlinje.perid')) NO-ERROR.
  IF bh:AVAILABLE THEN
  DO:
    bh:BUFFER-FIELD('parameter2'):BUFFER-VALUE = perid:SCREEN-VALUE.
  END.
  ELSE
  DO:
    bh:FIND-LAST('where true').
    iNextSysPara = IF bh:AVAIL THEN bh:BUFFER-FIELD('paranr'):BUFFER-VALUE ELSE 0.
    bh:BUFFER-CREATE().                                     
    ASSIGN 
      iNextSysPara = iNextSysPara + 1
      bh:BUFFER-FIELD('sysHid'):BUFFER-VALUE     = STRING(isysHid)
      bh:BUFFER-FIELD('sysGr'):BUFFER-VALUE      = iNextSysGr
      bh:BUFFER-FIELD('paranr'):BUFFER-VALUE     = STRING(iNextSysPara)
      bh:BUFFER-FIELD('parameter1'):BUFFER-VALUE = 'stlinje.perid'
      bh:BUFFER-FIELD('parameter2'):BUFFER-VALUE = perid:SCREEN-VALUE
    .
  END.
  DYNAMIC-FUNCTION("runProc","sysgruppe_postaction.p",STRING(isyshid) + '|' + STRING(iNextSysGr) + '|' + DYNAMIC-FUNCTION("getObjectState",hFieldMap),tth).
  iSysGr = iNextSysGr. /*For å sende riktig data tilbake ved NEW*/
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

