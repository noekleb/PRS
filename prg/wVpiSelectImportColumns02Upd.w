&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

CREATE WIDGET-POOL.

DEF VAR bOk         AS LOG    NO-UNDO.
DEF VAR ix          AS INT    NO-UNDO.
DEF VAR hBrowse     AS HANDLE NO-UNDO.
DEF VAR hQuery      AS HANDLE NO-UNDO.
DEF VAR hToolbar    AS HANDLE NO-UNDO.
DEF VAR hFieldMap   AS HANDLE NO-UNDO.
DEF VAR hParent     AS HANDLE NO-UNDO.

DEFINE VARIABLE cAction               AS CHAR NO-UNDO.
DEFINE VARIABLE fldSysHid             AS INT NO-UNDO. 
DEFINE VARIABLE fldSysGr              AS INT NO-UNDO. 
DEFINE VARIABLE fldSysParaNr          AS INT INIT ? NO-UNDO.
DEFINE VARIABLE fldSysParaBeskrivelse AS CHAR NO-UNDO. 
DEFINE VARIABLE fldSysParaHjelpeTekst AS CHAR NO-UNDO.
DEFINE VARIABLE fldSysParaParameter1  AS CHAR NO-UNDO.
DEFINE VARIABLE fldSysParaParameter2  AS CHAR NO-UNDO.

DEFINE VARIABLE hQueryBf AS HANDLE NO-UNDO. 

hParent = SOURCE-PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Beskrivelse Hjelpetekst1 BtnAction BtnAvbryt 
&Scoped-Define DISPLAYED-OBJECTS VPILeverandor sysGr Beskrivelse ~
Hjelpetekst1 lblHjelpetekst 

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
     SIZE 24 BY 1.43
     BGCOLOR 8 .

DEFINE BUTTON BtnAvbryt DEFAULT 
     LABEL "Avbryt" 
     SIZE 24 BY 1.43
     BGCOLOR 8 .

DEFINE VARIABLE Hjelpetekst1 AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 54 BY 1.91 NO-UNDO.

DEFINE VARIABLE Beskrivelse AS CHARACTER FORMAT "X(256)":U 
     LABEL "Beskrivelse" 
     VIEW-AS FILL-IN 
     SIZE 54 BY 1 NO-UNDO.

DEFINE VARIABLE lblHjelpetekst AS CHARACTER FORMAT "X(256)":U INITIAL "Hjelpetekst:" 
      VIEW-AS TEXT 
     SIZE 12 BY .62 NO-UNDO.

DEFINE VARIABLE sysGr AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "Import nr" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE VPILeverandor AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     LABEL "VPI Levrandør" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     VPILeverandor AT ROW 1.48 COL 15 COLON-ALIGNED 
     sysGr AT ROW 3.38 COL 15 COLON-ALIGNED
     Beskrivelse AT ROW 4.33 COL 15 COLON-ALIGNED
     Hjelpetekst1 AT ROW 5.29 COL 17 NO-LABEL
     BtnAction AT ROW 8.14 COL 21
     BtnAvbryt AT ROW 8.14 COL 47
     lblHjelpetekst AT ROW 5.29 COL 3 COLON-ALIGNED NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 87.8 BY 9.86.


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
         TITLE              = "Lagre VPI import kolonne def"
         HEIGHT             = 9.86
         WIDTH              = 87.8
         MAX-HEIGHT         = 47.91
         MAX-WIDTH          = 384
         VIRTUAL-HEIGHT     = 47.91
         VIRTUAL-WIDTH      = 384
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
/* SETTINGS FOR FILL-IN VPILeverandor IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Lagre VPI import kolonne def */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Lagre VPI import kolonne def */
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

        DO TRANSACTION : 
           FIND syspara WHERE ROWID(syspara) =  TO-ROWID(hFieldMap:BUFFER-FIELD('rowident1'):BUFFER-VALUE) EXCLUSIVE-LOCK. 
           DELETE syspara. 
           RELEASE syspara. 
        END.
        /*  - dette fungerer ikke så bra ... 
        DYNAMIC-FUNCTION('DoDelete','sysPara','=delval_sysPara.p','',hFieldMap:BUFFER-FIELD('rowident1'):BUFFER-VALUE,YES).
        */
        RUN BlankRecord IN hParent.

    END.
    ELSE 
    DO:
        RUN InvokeMethod(hFieldMap,"SaveRecord").
        DYNAMIC-FUNCTION('setsysGr' IN hParent, INT(fldSysGr)).
        /*
        DYNAMIC-FUNCTION('receiveParameterTable' IN hParent,tth).
        RUN myDisplayRecord IN hParent. 
        */
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
  DISPLAY VPILeverandor sysGr Beskrivelse Hjelpetekst1 lblHjelpetekst 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE Beskrivelse Hjelpetekst1 BtnAction BtnAvbryt 
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
  DEFINE VARIABLE cReturn AS CHAR NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    
    IF fldSysparanr = ? THEN cAction = "new" .

    hQuery = DYNAMIC-FUNCTION("NewQuery"
    ,100
    ,""
    ,"sysPara"
     + ";!sysGr;beskrivelse;!syshid;Hjelpetekst1;!paranr;!parameter1;!parameter2;"
    ,'WHERE syshid = ' + STRING(fldSysHid) + ' AND sysGr = ' + STRING(fldSysGr) + ' AND paranr = ' + STRING(fldSysparanr)
    ,"").
    
    hQueryBf = hQuery:GET-BUFFER-HANDLE(1).

    hFieldMap = DYNAMIC-FUNCTION("NewFieldMap",      /* A fieldmap object holds extra info for display and input fields (fill-ins) 
                                                        and their corresponding buffer columns return handle equals the buffer handle */
                      hQuery,
                      FRAME {&FRAME-NAME}:HANDLE,    /* Frame for the input/display fields (might not be the same frame as the browse) */
                      "Beskrivelse,Hjelpetekst1",     /* Update columns in buffer */
                      "",                            /* Corresponding input fields (fill-in..). blank if equal to update columns */
                      "",                           /* Additional buffer and displ.fields - not updateable*/
                      "",                           /* Corresponding fill-ins */
                      "").                           /* other input widgets and lookup buttons for update fill-ins */
  
    DYNAMIC-FUNCTION("setAttribute",hFieldMap,"bufferextrafields","syshid,sysgr,paranr,parameter1,parameter2").
    DYNAMIC-FUNCTION('setAttribute',hFieldMap,'CustomDeleteValProc','=delval_sysPara.p').

    /*
    IF hQueryBf:AVAILABLE THEN
    MESSAGE hQueryBf::syshid 
            hQueryBf::sysgr 
            hQueryBf::paranr
            hQueryBf::beskrivelse
            hQueryBf::parameter1  
            hQueryBf::parameter2
            VIEW-AS ALERT-BOX. 
    */

    DYNAMIC-FUNCTION("CreateObjectLink",hFieldMap,hQuery).
    RUN InvokeMethod(hQuery,"OpenQuery").
    
    IF cAction = "New" THEN
    DO:
          RUN InvokeMethod(hFieldMap,"NewRecord").
          DYNAMIC-FUNCTION("setObjectState",hFieldMap,'New').
    END.

    IF cAction = 'Delete' THEN
    DO:
      THIS-PROCEDURE:CURRENT-WINDOW:TITLE = 'Slett'.
      btnAction:LABEL = 'Slett'.
      ASSIGN
        Beskrivelse:SENSITIVE    = FALSE
        Hjelpetekst1:SENSITIVE   = FALSE
        vpileverandor:SENSITIVE  = FALSE 
        sysGr:SENSITIVE          = FALSE
      .
    END.                        
    
    cReturn = DYNAMIC-FUNCTION("getFieldValues","sysPara","WHERE sysHid = " + STRING(fldSysHid) + ' AND sysGr = ' + STRING(fldSysGr) + ' AND parameter1="VPILeverandor"',"parameter2").
    IF cReturn NE ? THEN
      vpileverandor:SCREEN-VALUE = cReturn.
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
/* DEF INPUT PARAM TABLE-HANDLE itth. */
DEFINE INPUT PARAM icAction    AS CHAR NO-UNDO.
DEFINE INPUT PARAM icsysHid    AS INT NO-UNDO.
DEFINE INPUT PARAM icSysGr     AS INT NO-UNDO.
DEFINE INPUT PARAM icSysParaNr AS INT NO-UNDO.
DEFINE INPUT PARAM icfldSysParaParameter1 AS CHAR NO-UNDO.
DEFINE INPUT PARAM icfldSysParaParameter2 AS CHAR NO-UNDO.


ASSIGN 
    cAction = icAction
    fldSysHid = icsyshid 
    fldSysGr = icsysgr
    fldSysParaNr = icSysParaNr
    fldSysParaParameter1 = icfldSysParaParameter1  
    fldSysParaParameter2 = icfldSysParaParameter2 .

DO WITH FRAME {&FRAME-NAME}:
  vpiLeverandor:SCREEN-VALUE = STRING(fldSysGr).
  IF icAction = "New" THEN 
       sysgr:SCREEN-VALUE = ?.
  ELSE sysgr:SCREEN-VALUE = STRING(icSysParaNr).
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
APPLY 'entry' TO beskrivelse IN FRAME {&FRAME-NAME}.
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
DEFINE VARIABLE iNextsysParanr AS INTEGER NO-UNDO.


DO WITH FRAME {&FRAME-NAME}:  
  
  IF DYNAMIC-FUNCTION("getObjectState",hFieldMap) = 'New' THEN
  DO:
  
    ASSIGN 
      iNextSysParaNr = INTEGER(DYNAMIC-FUNCTION("getFieldValues","LAST sysPara",
                                                'WHERE syshid = ' + STRING(fldSysHid) + ' AND sysGr = ' + STRING(fldSysGr)  /* + ' AND paranr = ' + STRING(fldSysparanr) */
                                               ,"paranr"))
      iNextSysParaNr = IF iNextSysParaNr = ? THEN 1 ELSE iNextSysParaNr + 1.

     
     hQueryBf:BUFFER-CREATE().                                     
     ASSIGN 
       hQueryBf:BUFFER-FIELD('sysHid'):BUFFER-VALUE     = STRING(fldSyshid)
       hQueryBf:BUFFER-FIELD('sysGr'):BUFFER-VALUE      = STRING(fldSysgr)
       hQueryBf:BUFFER-FIELD('paranr'):BUFFER-VALUE     = STRING(iNextSysParaNr)
       hQueryBf:BUFFER-FIELD('parameter1'):BUFFER-VALUE = fldSysParaParameter1
       hQueryBf:BUFFER-FIELD('parameter2'):BUFFER-VALUE = fldSysParaParameter2
       hQueryBf:BUFFER-FIELD('beskrivelse'):BUFFER-VALUE = beskrivelse:SCREEN-VALUE
       hQueryBf:BUFFER-FIELD('hjelpetekst1'):BUFFER-VALUE = hjelpetekst1:SCREEN-VALUE.
     
     DYNAMIC-FUNCTION("setAttribute",hFieldMap,"bufferextravalues",STRING(fldSysHid) + '|' + STRING(fldSysgr) + "|" + STRING(iNextSysParaNr) + 
                      "|" + fldSysParaparameter1 + "|" + fldSysParaparameter2).

  END. 
  ELSE 
      
      DYNAMIC-FUNCTION("setAttribute",hFieldMap,"bufferextravalues",STRING(fldSysHid) + '|' + STRING(fldSysgr) + "|" + STRING(fldSysParaNr) + 
                      "|" + fldSysParaparameter1 + "|" + fldSysParaparameter2).

  RUN SUPER.
  

END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

