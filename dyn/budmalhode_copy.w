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

DEF VAR wlibhandle  AS HANDLE NO-UNDO.
DEF VAR cUserId     AS CHAR   NO-UNDO.
DEF VAR iBrGrpNr    AS INT    NO-UNDO.
DEF VAR iMalId      AS INT    NO-UNDO.
DEF VAR ipMalId     AS INT    NO-UNDO.
DEF VAR cTekst      AS CHAR NO-UNDO.
DEF VAR piLoop      AS INT NO-UNDO.

DEF VAR iDefaultButikkNr AS INT NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Aar MalId MalBeskrivelse ButikkNr MalNotat ~
BtnOK BtnCancel AarFra ButikkNrFra RECT-57 RECT-58 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-2 Aar MalId MalBeskrivelse ~
ButikkNr MalNotat AarFra ButikkNrFra FILL-IN-3 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getQuery C-Win 
FUNCTION getQuery RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnCancel AUTO-END-KEY DEFAULT 
     LABEL "Avbryt" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON BtnOK AUTO-GO DEFAULT 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE Aar AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "År" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "Item 1",0
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE AarFra AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "År" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "Item 1",0
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE ButikkNr AS CHARACTER FORMAT "X(256)":U 
     LABEL "Butikk" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 34.8 BY 1 NO-UNDO.

DEFINE VARIABLE ButikkNrFra AS CHARACTER FORMAT "X(256)":U 
     LABEL "Butikk" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 34.8 BY 1 NO-UNDO.

DEFINE VARIABLE MalNotat AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 98 BY 8.57 NO-UNDO.

DEFINE VARIABLE FILL-IN-2 AS CHARACTER FORMAT "X(256)":U INITIAL "Fra" 
      VIEW-AS TEXT 
     SIZE 44.8 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FILL-IN-3 AS CHARACTER FORMAT "X(256)":U INITIAL "Til" 
      VIEW-AS TEXT 
     SIZE 43.6 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE MalBeskrivelse AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN NATIVE 
     SIZE 71 BY 1 NO-UNDO.

DEFINE VARIABLE MalId AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Mal" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-57
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 49.6 BY 3.1.

DEFINE RECTANGLE RECT-58
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 49.6 BY 3.1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     FILL-IN-2 AT ROW 2.67 COL 1.2 COLON-ALIGNED NO-LABEL WIDGET-ID 14
     Aar AT ROW 5.1 COL 60.8 COLON-ALIGNED WIDGET-ID 2
     MalId AT ROW 1.57 COL 13 COLON-ALIGNED
     MalBeskrivelse AT ROW 1.57 COL 27 COLON-ALIGNED NO-LABEL
     ButikkNr AT ROW 3.81 COL 60.8 COLON-ALIGNED
     MalNotat AT ROW 6.95 COL 2 NO-LABEL
     BtnOK AT ROW 15.76 COL 1.4
     BtnCancel AT ROW 15.76 COL 85.4
     AarFra AT ROW 5.1 COL 13.2 COLON-ALIGNED WIDGET-ID 4
     ButikkNrFra AT ROW 3.81 COL 13.2 COLON-ALIGNED WIDGET-ID 6
     FILL-IN-3 AT ROW 2.67 COL 51.4 COLON-ALIGNED NO-LABEL WIDGET-ID 16
     RECT-57 AT ROW 3.29 COL 1.6 WIDGET-ID 10
     RECT-58 AT ROW 3.29 COL 51 WIDGET-ID 12
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 100.2 BY 16.05
         DEFAULT-BUTTON BtnOK CANCEL-BUTTON BtnCancel.


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
         TITLE              = "Kopier budsjettmal"
         HEIGHT             = 16.05
         WIDTH              = 100.2
         MAX-HEIGHT         = 23.52
         MAX-WIDTH          = 114.2
         VIRTUAL-HEIGHT     = 23.52
         VIRTUAL-WIDTH      = 114.2
         MIN-BUTTON         = NO
         MAX-BUTTON         = NO
         RESIZE             = NO
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
   FRAME-NAME Custom                                                    */
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 16.05
       FRAME DEFAULT-FRAME:WIDTH            = 100.2.

/* SETTINGS FOR FILL-IN FILL-IN-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-3 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       MalNotat:RETURN-INSERTED IN FRAME DEFAULT-FRAME  = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = YES.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Kopier budsjettmal */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Kopier budsjettmal */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnCancel C-Win
ON CHOOSE OF BtnCancel IN FRAME DEFAULT-FRAME /* Avbryt */
DO:
   APPLY "CLOSE" TO THIS-PROCEDURE.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnOK C-Win
ON CHOOSE OF BtnOK IN FRAME DEFAULT-FRAME /* OK */
DO:
  DEF VAR hParentBrowse AS HANDLE NO-UNDO.
  
  IF MalBeskrivelse:SCREEN-VALUE = "" THEN DO:
      MESSAGE "Angi beskrivelse"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY "ENTRY" TO MalBeskrivelse.
      RETURN NO-APPLY.
  END.
  RUN InvokeMethod(hFieldMap,"SaveRecord").
  IF DYNAMIC-FUNCTION("getTransactionMessage") NE '' THEN
  DO:
    MESSAGE DYNAMIC-FUNCTION("getTransactionMessage")
       VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
  END.
  ELSE DO:
      bOK = DYNAMIC-FUNCTION("RunProc","sbudmalhode_copy.p",
                             hFieldMap:BUFFER-FIELD('MalId'):BUFFER-VALUE + '|' + STRING(ipMalId) + '|' + STRING(Aar:SCREEN-VALUE IN FRAME DEFAULT-FRAME),
                             ?).
  END.

/*   hParentBrowse = DYNAMIC-FUNCTION("getParentBrowse" IN hParent).                                         */
/*   IF VALID-HANDLE(hParentBrowse) THEN                                                                     */
/*   DO:                                                                                                     */
/*     MESSAGE program-name(1)                                                                               */
/*        VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                                 */
/*     IF DYNAMIC-FUNCTION("getObjectState",hFieldMap) = 'New' THEN                                          */
/*     DO:                                                                                                   */
/*       RUN InvokeMethod(hParentBrowse,"OpenQuery").                                                        */
/*       hParentBrowse:QUERY:REPOSITION-TO-ROWID(hFieldMap:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE) NO-ERROR. */
/*     END.                                                                                                  */
/*                                                                                                           */
/*     DYNAMIC-FUNCTION("RefreshRowids",hParentBrowse,hFieldMap:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE).     */
/*                                                                                                           */
/*   END.    
                                                                                                  */
  bOK = TRUE.
  APPLY "CLOSE" TO THIS-PROCEDURE.
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
  DYNAMIC-FUNCTION("SetWindowSensitive" IN hParent,TRUE) NO-ERROR.
  RUN OppdaterSumRecord IN hParent NO-ERROR.
  DYNAMIC-FUNCTION("friskOppRad" IN hParent,bOK) NO-ERROR.
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
  hParent = SOURCE-PROCEDURE.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplayRecord C-Win 
PROCEDURE DisplayRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN SUPER.
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            MalId:SENSITIVE       = FALSE
            .

        /*ED-FullTxt:HIDDEN = TelleType:SCREEN-VALUE <> "1".*/
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
  DISPLAY FILL-IN-2 Aar MalId MalBeskrivelse ButikkNr MalNotat AarFra 
          ButikkNrFra FILL-IN-3 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE Aar MalId MalBeskrivelse ButikkNr MalNotat BtnOK BtnCancel AarFra 
         ButikkNrFra RECT-57 RECT-58 
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
DEF VAR cButikkLst AS CHAR NO-UNDO.
DEF VAR iCl        AS INT  NO-UNDO.

iCl = DYNAMIC-FUNCTION("getFieldValues","SysPara",
                       "WHERE SysHId = 5 and SysGr = 1 and ParaNr = 1","Parameter1").

DO WITH FRAME {&FRAME-NAME}:
    
  hQuery = DYNAMIC-FUNCTION("NewQuery",1,
                            "",
                            "SBudMalHode"
                           + ";MalId"
                           + ";MalBeskrivelse"
                           + ";ButikkNr"
                           + ";Aar"
                           + ";MalNotat"
                            ,"where false"
                            ,"").
  .
  ASSIGN 
    iBrGrpNr         = INT(DYNAMIC-FUNCTION("getFieldValues","bruker","WHERE brukerid = " + QUOTER(DYNAMIC-FUNCTION("getASuserId")),"BrGrpNr"))
/*     iDefaultButikkNr = INT(DYNAMIC-FUNCTION("getFieldValues","bruker","WHERE brukerid = " + QUOTER(DYNAMIC-FUNCTION("getASuserId")),"ButikkNr")) */
/*     iDefaultButikkNr = IF iDefaultButikkNr = 0 THEN 1 ELSE iDefaultButikkNr                                                                      */
    iDefaultButikkNr    = int(DYNAMIC-FUNCTION("getButikkNr" IN hParent)).
    IF iDefaultButikkNr = 0 THEN
        iDefaultButikkNr = INT(DYNAMIC-FUNCTION("getFieldValues","bruker","WHERE brukerid = " + QUOTER(DYNAMIC-FUNCTION("getASuserId")),"ButikkNr"))
  .
  ASSIGN
  cButikkLst = DYNAMIC-FUNCTION("getFieldList","butiker;butnamn|butik;butik,butikktilgang"
                                               ,"WHERE true, first butikktilgang where brgrpnr = " + STRING(iBrGrpNr)).
      
  IF cButikkLst = '' OR cButikkLst = ? THEN
  cButikkLst = DYNAMIC-FUNCTION("getFieldList","butiker;butnamn|butik;butik,butikktilgang"
                                               ,"WHERE true, first butikktilgang where true").
  /* Er bruker ikke satt opp mot butikk, skal sentrallager benyttes */
  IF cButikkLst = '' OR cButikkLst = ? THEN
  cButikkLst = DYNAMIC-FUNCTION("getFieldList","butiker;butnamn|butik;butik"
                                               ,"WHERE Butiker.Butik = " + STRING(iCl)).
  
  /* Bygger liste med aktuelle årstall */
  DO piLoop = 2000 TO YEAR(TODAY) + 1:
      cTekst = cTekst + 
               (IF cTekst <> '' THEN '|' ELSE '') + 
               STRING(piLoop) + '|' + STRING(piLoop).
  END.

  ASSIGN 
    Aar:DELIMITER       = "|"
    Aar:LIST-ITEM-PAIRS = cTekst
    Aar:SCREEN-VALUE    = STRING(YEAR(TODAY))
    ButikkNr:DELIMITER  = "|"
    ButikkNr:LIST-ITEM-PAIRS = cButikkLst
    ButikkNr:SCREEN-VALUE    = STRING(iDefaultButikkNr)
    AarFra:DELIMITER       = "|"
    AarFra:LIST-ITEM-PAIRS = cTekst
    AarFra:SCREEN-VALUE    = STRING(YEAR(TODAY))
    ButikkNrFra:DELIMITER  = "|"
    ButikkNrFra:LIST-ITEM-PAIRS = cButikkLst
    ButikkNrFra:SCREEN-VALUE    = STRING(iDefaultButikkNr)
    /*ButikkNr:SCREEN-VALUE    = ButikkNr:ENTRY(ButikkNr:LOOKUP(STRING(iDefaultButikkNr)))*/
  .
  
  hFieldMap = DYNAMIC-FUNCTION("NewFieldMap",
                             hQuery,                    
                             FRAME {&FRAME-NAME}:HANDLE,
                             "MalId,MalBeskrivelse,ButikkNr,Aar,MalNotat",""
                             ,"",""
                             ,"").
  DYNAMIC-FUNCTION("CreateObjectLink",hQuery,hFieldMap).
  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"customUpdateValProc","ignore").

  ButikkNr:SENSITIVE = TRUE.

END.

DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,200,200,0,0).
THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = NO.
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
APPLY 'entry' TO MalId IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE myNewRecord C-Win 
PROCEDURE myNewRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM iButikkNr   AS INT NO-UNDO.
  
  DEF VAR cMalId AS CHAR NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:

    DYNAMIC-FUNCTION("setObjectState",hFieldMap,'New').
    RUN InvokeMethod(hFieldMap,"NewRecord").

    IF DYNAMIC-FUNCTION("runproc","get_sbudmalid.p",'',?) THEN.
    cMalId = DYNAMIC-FUNCTION("getTransactionMessage").

    ASSIGN
        MalId:SCREEN-VALUE    = cMalId
        MalId:SENSITIVE       = FALSE
        ButikkNr:SCREEN-VALUE = ButikkNr:ENTRY(ButikkNr:LOOKUP(STRING(iButikkNr)))
        ButikkNr:SENSITIVE    = TRUE
        MalBeskrivelse:SCREEN-VALUE = '' 
        .


    APPLY "ENTRY" TO MalBeskrivelse.
  
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
  
  IF DYNAMIC-FUNCTION("getObjectState",hFieldMap) = 'New' THEN 
  DO:
    /*
    DYNAMIC-FUNCTION("setAttribute",hFieldMap,"BufferExtraFields","ttid,TelleType").
    DYNAMIC-FUNCTION("setAttribute",hFieldMap,"BufferExtraValues","9|" + STRING(TelleType:SCREEN-VALUE IN FRAME {&FRAME-NAME})).
    */
  END.
  RUN SUPER.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setDefaultVerdier C-Win 
PROCEDURE setDefaultVerdier :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER cPara AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
        MalBeskrivelse:SCREEN-VALUE = ENTRY(4,cPara,'|')
        Aar:SCREEN-VALUE            = ENTRY(3,cPara,'|')
        ButikkNr:SCREEN-VALUE       = ENTRY(2,cPara,'|')
        AarFra:SCREEN-VALUE         = ENTRY(3,cPara,'|')
        AarFra:SENSITIVE            = FALSE
        ButikkNrFra:SCREEN-VALUE    = ENTRY(2,cPara,'|')
        ButikkNrFra:SENSITIVE       = FALSE
        ipMalId                     = INT(ENTRY(1,cPara,'|'))
        MalNotat:SCREEN-VALUE       = 'Kopi av mal ' + STRING(ipMalId) + ' ' + 
                                      ENTRY(4,cPara,'|') + '.' + CHR(10) + 
                                      'Kopiert ' + STRING(TODAY) + ' ' + 
                                      STRING(TIME,"HH:MM:SS") + ' ' +
                                      USERID("SkoTex")
        .
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getQuery C-Win 
FUNCTION getQuery RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN hQuery.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

