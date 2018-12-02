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
DEF VAR cSBudLst    AS CHAR   NO-UNDO.

DEF VAR wlibhandle  AS HANDLE NO-UNDO.
DEF VAR cUserId     AS CHAR   NO-UNDO.
DEF VAR iBrGrpNr    AS INT    NO-UNDO.
DEF VAR iMalId      AS INT    NO-UNDO.
DEF VAR cTekst      AS CHAR NO-UNDO.
DEF VAR piLoop      AS INT NO-UNDO.
DEF VAR BNyFlagg    AS LOG    NO-UNDO.

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
&Scoped-Define ENABLED-OBJECTS RECT-5 SBudId SBudBeskrivelse ButikkNr Aar ~
Salgbudsjett ProsentPM DbBudsjett DB% Aktiv SBudNotat BtnOk BtnCancel 
&Scoped-Define DISPLAYED-OBJECTS SBudId SBudBeskrivelse ButikkNr Aar ~
Salgbudsjett ProsentPM DbBudsjett DB% MalId AktivertAv AktivertDato Aktiv ~
SBudNotat FILL-IN-8 FILL-IN-9 FILL-IN-3 FILL-IN-5 

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ValueChangedWarning C-Win 
FUNCTION ValueChangedWarning RETURNS LOGICAL
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

DEFINE BUTTON BtnOk 
     LABEL "Ok" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnSBudMal  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE Aar AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "År" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "Item 1",0
     DROP-DOWN-LIST
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE ButikkNr AS CHARACTER FORMAT "X(256)":U 
     LABEL "Butikk" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 76.4 BY 1 NO-UNDO.

DEFINE VARIABLE SBudNotat AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 95 BY 7.05 NO-UNDO.

DEFINE VARIABLE AktivertAv AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN NATIVE 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE AktivertDato AS DATE FORMAT "99/99/99":U 
     LABEL "Aktivert" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE DB% AS DECIMAL FORMAT "->>,>>9.99%Db":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE DbBudsjett AS DECIMAL FORMAT "->>,>>>,>>9.99":U INITIAL 0 
     LABEL "Db budsjett" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-3 AS CHARACTER FORMAT "X(256)":U INITIAL "Alle beløp inkl. mva." 
      VIEW-AS TEXT 
     SIZE 44 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FILL-IN-5 AS CHARACTER FORMAT "X(256)":U INITIAL "Mal velges når budsjettet oppdateres" 
      VIEW-AS TEXT 
     SIZE 44 BY .62 NO-UNDO.

DEFINE VARIABLE FILL-IN-8 AS CHARACTER FORMAT "X(256)":U INITIAL "Budsjettets salg og db beløp kan bare endres når budsjettet ikke er aktivt" 
      VIEW-AS TEXT 
     SIZE 79.6 BY .62 NO-UNDO.

DEFINE VARIABLE FILL-IN-9 AS CHARACTER FORMAT "X(256)":U INITIAL "Angi øknkng/minskning i %" 
      VIEW-AS TEXT 
     SIZE 36 BY .62 NO-UNDO.

DEFINE VARIABLE MalId AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "MalId" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE ProsentPM AS DECIMAL FORMAT "->>,>>9.99%":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE Salgbudsjett AS DECIMAL FORMAT "->>,>>>,>>9.99":U INITIAL 0 
     LABEL "Salgsbudsjett" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE SBudBeskrivelse AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN NATIVE 
     SIZE 62 BY 1 NO-UNDO.

DEFINE VARIABLE SBudId AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Budsjett id" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 95 BY 3.81.

DEFINE VARIABLE Aktiv AS LOGICAL INITIAL no 
     LABEL "Aktiv" 
     VIEW-AS TOGGLE-BOX
     SIZE 48.2 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnSBudMal AT ROW 10.14 COL 34 WIDGET-ID 10 NO-TAB-STOP 
     SBudId AT ROW 1.71 COL 17.6 COLON-ALIGNED
     SBudBeskrivelse AT ROW 1.71 COL 31.8 COLON-ALIGNED NO-LABEL
     ButikkNr AT ROW 2.81 COL 17.6 COLON-ALIGNED
     Aar AT ROW 4.1 COL 17.6 COLON-ALIGNED WIDGET-ID 2
     Salgbudsjett AT ROW 6.43 COL 17.6 COLON-ALIGNED
     ProsentPM AT ROW 6.48 COL 44 COLON-ALIGNED NO-LABEL
     DbBudsjett AT ROW 7.62 COL 17.6 COLON-ALIGNED
     DB% AT ROW 7.62 COL 44 COLON-ALIGNED NO-LABEL
     MalId AT ROW 10.14 COL 18 COLON-ALIGNED
     AktivertAv AT ROW 10.14 COL 32.4 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     AktivertDato AT ROW 11.24 COL 18 COLON-ALIGNED NO-TAB-STOP 
     Aktiv AT ROW 11.38 COL 49.2 WIDGET-ID 18
     SBudNotat AT ROW 12.52 COL 3 NO-LABEL
     BtnOk AT ROW 20.05 COL 3 WIDGET-ID 32
     BtnCancel AT ROW 20.19 COL 85
     FILL-IN-8 AT ROW 5.33 COL 2.2 COLON-ALIGNED NO-LABEL
     FILL-IN-9 AT ROW 6.71 COL 59 COLON-ALIGNED NO-LABEL
     FILL-IN-3 AT ROW 8.86 COL 18 COLON-ALIGNED NO-LABEL
     FILL-IN-5 AT ROW 10.38 COL 47.2 COLON-ALIGNED NO-LABEL
     RECT-5 AT ROW 6 COL 3 WIDGET-ID 24
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 100.2 BY 20.48
         CANCEL-BUTTON BtnCancel.


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
         TITLE              = "Budsjett"
         HEIGHT             = 20.48
         WIDTH              = 100.2
         MAX-HEIGHT         = 23.52
         MAX-WIDTH          = 114.2
         VIRTUAL-HEIGHT     = 23.52
         VIRTUAL-WIDTH      = 114.2
         MIN-BUTTON         = no
         MAX-BUTTON         = no
         RESIZE             = no
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
   FRAME-NAME L-To-R                                                    */
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 20.48
       FRAME DEFAULT-FRAME:WIDTH            = 100.2.

/* SETTINGS FOR FILL-IN AktivertAv IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN AktivertDato IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnSBudMal IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       btnSBudMal:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-3 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-5 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-8 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-9 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN MalId IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       SBudNotat:RETURN-INSERTED IN FRAME DEFAULT-FRAME  = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Budsjett */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Budsjett */
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


&Scoped-define SELF-NAME BtnOk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnOk C-Win
ON CHOOSE OF BtnOk IN FRAME DEFAULT-FRAME /* Ok */
DO:
  DEF VAR hParentBrowse AS HANDLE NO-UNDO.
  
  IF SBudBeskrivelse:SCREEN-VALUE = "" THEN DO:
      MESSAGE "Angi beskrivelse"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY "ENTRY" TO SBudBeskrivelse.
      RETURN NO-APPLY.
  END.
  IF INT(ButikkNr:SCREEN-VALUE) = ? THEN DO:
      MESSAGE "Angi butikk"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY "ENTRY" TO ButikkNr.
      RETURN NO-APPLY.
  END.
  IF INT(Aar:SCREEN-VALUE) = ? THEN DO:
      MESSAGE "Angi år som budsjettet gjelder for"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY "ENTRY" TO ButikkNr.
      RETURN NO-APPLY.
  END.
  IF DEC(SalgBudsjett:SCREEN-VALUE) = 0 THEN DO:
    MESSAGE "Angi budsjettert salgsverdi"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    APPLY "ENTRY" TO SalgBudsjett.
    RETURN NO-APPLY.
  END.
  IF DEC(DbBudsjett:SCREEN-VALUE) = 0 THEN DO:
    MESSAGE "Angi budsjettert dekningsbidrag"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    APPLY "ENTRY" TO DbBudsjett.
    RETURN NO-APPLY.
  END.
  IF DEC(SalgBudsjett:SCREEN-VALUE) < DEC(DbBudsjett:SCREEN-VALUE) THEN DO:
    MESSAGE "Dekningsbidraget er større en budsjettert omsetning."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    APPLY "ENTRY" TO DbBudsjett.
    RETURN NO-APPLY.
  END.

  /*IF NOT Aktiv:CHECKED IN FRAME DEFAULT-FRAME THEN */
  DO:
      RUN InvokeMethod(hFieldMap,"SaveRecord").

      RUN oppdaterRad IN hParent NO-ERROR.
  END.

  IF DYNAMIC-FUNCTION("getTransactionMessage") NE '' THEN
  DO:
/*     MESSAGE DYNAMIC-FUNCTION("getTransactionMessage") */
/*        VIEW-AS ALERT-BOX INFO BUTTONS OK.             */
    RETURN.
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


&Scoped-define SELF-NAME btnSBudMal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSBudMal C-Win
ON CHOOSE OF btnSBudMal IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF MalId
DO:
  DEF VAR cLookupValue AS CHAR NO-UNDO.

  cLookupValue = "MalId".

  RUN JBoxDLookup.w ("SBudMalHode;MalId;MalBeskrivelse;Aar", 
                     "WHERE true",
                     INPUT-OUTPUT cLookupValue).

  IF cLookupValue NE "" THEN DO:
    ASSIGN 
      MalId:SCREEN-VALUE   = ENTRY(1,cLookupValue,"|")
    .
  END.
  ELSE APPLY "entry" TO MalId.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME DB%
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DB% C-Win
ON LEAVE OF DB% IN FRAME DEFAULT-FRAME
DO:
    IF SELF:MODIFIED THEN
    ASSIGN
      DbBudsjett:SCREEN-VALUE = STRING( ROUND((DYNAMIC-FUNCTION("calkNettoFraBruttoOms",DEC(SalgBudsjett:SCREEN-VALUE),0)/ 100 ) * DEC(REPLACE(DB%:SCREEN-VALUE,'%Db','')),2))
      DbBudsjett:SCREEN-VALUE = IF DbBudsjett:SCREEN-VALUE = ? THEN '' ELSE DbBudsjett:SCREEN-VALUE
      .

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DB% C-Win
ON VALUE-CHANGED OF DB% IN FRAME DEFAULT-FRAME
DO:
    SELF:MODIFIED = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME DbBudsjett
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DbBudsjett C-Win
ON LEAVE OF DbBudsjett IN FRAME DEFAULT-FRAME /* Db budsjett */
DO:
  IF SELF:MODIFIED THEN
  DO:
      RUN beregnOppNed (0).
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DbBudsjett C-Win
ON VALUE-CHANGED OF DbBudsjett IN FRAME DEFAULT-FRAME /* Db budsjett */
DO:
    SELF:MODIFIED = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ProsentPM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ProsentPM C-Win
ON LEAVE OF ProsentPM IN FRAME DEFAULT-FRAME
DO:
    IF SELF:MODIFIED THEN
    DO:
        RUN beregnOppNed (DEC(REPLACE(ProsentPM:SCREEN-VALUE,'%',''))).
        ProsentPM:SCREEN-VALUE = '0'.
    END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ProsentPM C-Win
ON VALUE-CHANGED OF ProsentPM IN FRAME DEFAULT-FRAME
DO:
    SELF:MODIFIED = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Salgbudsjett
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Salgbudsjett C-Win
ON LEAVE OF Salgbudsjett IN FRAME DEFAULT-FRAME /* Salgsbudsjett */
DO:
    IF SELF:MODIFIED THEN
    DO:
        RUN beregnOppNed (0).
    END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Salgbudsjett C-Win
ON VALUE-CHANGED OF Salgbudsjett IN FRAME DEFAULT-FRAME /* Salgsbudsjett */
DO:
  SELF:MODIFIED = TRUE.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE beregnOppNed C-Win 
PROCEDURE beregnOppNed :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER lPM% AS DEC FORMAT '->>>>9.99' NO-UNDO.

  DEF VAR lDec  AS DEC FORMAT "->>>>>>>9.99" NO-UNDO.
  DEF VAR lDiff AS DEC FORMAT "->>>>>>>9.99" NO-UNDO. 
                              
  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN
          lDec  = DEC(SalgBudsjett:SCREEN-VALUE)
          lDiff =  ROUND((lDec * ABS(lPM%)) / 100,2)
          lDiff = IF lDiff = ? THEN 0 ELSE lDiff
          lDiff = IF lPM% < 0 THEN lDiff * -1 ELSE lDiff
          .

      IF lPM% <> 0 AND lDec <> 0 THEN 
          ASSIGN
          SalgBudsjett:SCREEN-VALUE = STRING(lDec + lDiff)
          ProsentPM:SCREEN-VALUE    = '0'
          Db%:SCREEN-VALUE = STRING(ROUND((DEC(DbBudsjett:SCREEN-VALUE) / DYNAMIC-FUNCTION("calkNettoFraBruttoOms",DEC(SalgBudsjett:SCREEN-VALUE),0) ) * 100,2))
          Db%:SCREEN-VALUE = IF Db%:SCREEN-VALUE = ? THEN '' ELSE Db%:SCREEN-VALUE
          .
      ELSE IF lPM% = 0 THEN
          ASSIGN
          Db%:SCREEN-VALUE = STRING(ROUND((DEC(DbBudsjett:SCREEN-VALUE) / DYNAMIC-FUNCTION("calkNettoFraBruttoOms",DEC(SalgBudsjett:SCREEN-VALUE),0) ) * 100,2))
          Db%:SCREEN-VALUE = IF Db%:SCREEN-VALUE = ? THEN '' ELSE Db%:SCREEN-VALUE
          .
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
            SBudId:SENSITIVE       = FALSE
            SalgBudsjett:SENSITIVE = NOT Aktiv:CHECKED  
            DbBudsjett:SENSITIVE   = NOT Aktiv:CHECKED  
            ProsentPM:SENSITIVE    = NOT Aktiv:CHECKED  
            Db%:SENSITIVE          = NOT Aktiv:CHECKED 
            ButikkNr:SENSITIVE     = bNyFlagg  
            aAR:SENSITIVE          = bNyFlagg
            
            Db%:SCREEN-VALUE = STRING((DEC(DbBudsjett:SCREEN-VALUE) / DYNAMIC-FUNCTION("calkNettoFraBruttoOms",DEC(SalgBudsjett:SCREEN-VALUE),0) ) * 100)
/*             Db%:SCREEN-VALUE = STRING((DEC(DbBudsjett:SCREEN-VALUE) / DEC(SalgBudsjett:SCREEN-VALUE) ) * 100) Ändrad 160226 - ken1 */
            Db%:SCREEN-VALUE = IF Db%:SCREEN-VALUE = ? THEN '' ELSE Db%:SCREEN-VALUE
            /*
            SalgBudsjett           = DEC(Salgbudsjett:SCREEN-VALUE)
            DbBudsjett             = DEC(DbBudsjett:SCREEN-VALUE)
            */
            ProsentPM              = DEC(REPLACE(ProsentPM:SCREEN-VALUE,'%',''))
            Db%                    = DEC(REPLACE(Db%:SCREEN-VALUE,'%Db',''))
            SalgBudsjett:MODIFIED  = FALSE
            DbBudsjett:MODIFIED    = FALSE
            ProsentPM:MODIFIED     = FALSE
            Db%:MODIFIED           = FALSE
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
  DISPLAY SBudId SBudBeskrivelse ButikkNr Aar Salgbudsjett ProsentPM DbBudsjett 
          DB% MalId AktivertAv AktivertDato Aktiv SBudNotat FILL-IN-8 FILL-IN-9 
          FILL-IN-3 FILL-IN-5 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-5 SBudId SBudBeskrivelse ButikkNr Aar Salgbudsjett ProsentPM 
         DbBudsjett DB% Aktiv SBudNotat BtnOk BtnCancel 
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
                            "SBudHode"
                           + ";SBudId"
                           + ";SBudBeskrivelse"
                           + ";ButikkNr"
                           + ";Aar"
                           + ";MalId"
                           + ";Aktiv"
                           + ";AktivertDato"
                           + ";AktivertAv"
                           + ";SalgBudsjett"
                           + ";DbBudsjett"
                           + ";SBudNotat"
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
  DO piLoop = 2000 TO YEAR(TODAY) + 2:
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
    /*ButikkNr:SCREEN-VALUE    = ButikkNr:ENTRY(ButikkNr:LOOKUP(STRING(iDefaultButikkNr)))*/
  .
  
  hFieldMap = DYNAMIC-FUNCTION("NewFieldMap",
                             hQuery,                    
                             FRAME {&FRAME-NAME}:HANDLE,
                             "SBudId,SBudBeskrivelse,ButikkNr,Aar,MalId,Aktiv,AktivertDato,AktivertAv,SalgBudsjett,DbBudsjett,SBudNotat"
                             ,""
                             ,""
                             ,""
                             ,"").
  DYNAMIC-FUNCTION("CreateObjectLink",hQuery,hFieldMap).
  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"customUpdateValProc","ignore").
  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"postUpdateProc","sbudhode_post_update.p").  

  ButikkNr:SENSITIVE = TRUE.
  APPLY 'ENTRY' TO SBudBeskrivelse.
END.

DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,200,200,0,0).
THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = NO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LeaveOfField C-Win 
PROCEDURE LeaveOfField :
DEF INPUT PARAM icFieldName AS CHAR NO-UNDO.

DEF VAR cReturnValue AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
    CASE icFieldName:
      WHEN "DbBudsjett" THEN DO WITH FRAME {&FRAME-NAME}:
          IF DbBudsjett:MODIFIED THEN 
          DO:
            RUN beregnOppNed (0).
          END.
          DbBudsjett:MODIFIED = FALSE.
      END.
      WHEN "SalgBudsjett" THEN DO WITH FRAME {&FRAME-NAME}:
          IF SalgBudsjett:MODIFIED THEN 
          DO:
            RUN beregnOppNed (0).
          END.
          SalgBudsjett:MODIFIED = FALSE.
      END.
    END CASE.
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
APPLY 'entry' TO SBudId IN FRAME {&FRAME-NAME}.

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
  
  DEF VAR cSBudId AS CHAR NO-UNDO.

  ASSIGN
      BNyFlagg = TRUE.

  DO WITH FRAME {&FRAME-NAME}:

    DYNAMIC-FUNCTION("setObjectState",hFieldMap,'New').
    RUN InvokeMethod(hFieldMap,"NewRecord").

    IF DYNAMIC-FUNCTION("runproc","get_sbudid.p",'',?) THEN.
    cSBudId = DYNAMIC-FUNCTION("getTransactionMessage").

    ASSIGN
        SBudId:SCREEN-VALUE   = cSBudId
        SBudId:SENSITIVE      = FALSE
        ButikkNr:SCREEN-VALUE = ButikkNr:ENTRY(ButikkNr:LOOKUP(STRING(iButikkNr)))
        ButikkNr:SENSITIVE    = TRUE
        aAR:SENSITIVE         = TRUE
        SBudBeskrivelse:SCREEN-VALUE = '' 
        .


    APPLY "ENTRY" TO SBudBeskrivelse.
  
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
  DO WITH FRAME {&FRAME-NAME}:
      IF DYNAMIC-FUNCTION("getObjectState",hFieldMap) = 'New' THEN 
      DO:
        /*
        DYNAMIC-FUNCTION("setAttribute",hFieldMap,"BufferExtraFields","ttid,TelleType").
        DYNAMIC-FUNCTION("setAttribute",hFieldMap,"BufferExtraValues","9|" + STRING(TelleType:SCREEN-VALUE IN FRAME {&FRAME-NAME})).
        */
      END.
  END.

  RUN SUPER.

  DO WITH FRAME {&FRAME-NAME}:
      /*
      IF bNyFlagg = FALSE THEN
          RUN sbudhode_regen.p (INT(SBudId:SCREEN-VALUE)).
      */
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
DEF INPUT PARAM icFieldName AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  cSBudLst = ''.
  FOR EACH SBudHode NO-LOCK WHERE
      SBudHode.Aar      = INT(Aar:SCREEN-VALUE) AND 
      SBudHode.ButikkNr = INT(ButikkNr:SCREEN-VALUE) AND 
      SBudHode.Aktiv    = TRUE:

      IF SBudHode.SBudId = INT(SBudId:SCREEN-VALUE) THEN
          NEXT.

      cSBudLst = cSBudLst + 
                 (IF cSBudLst <> '' THEN ',' ELSE '') + 
                 STRING(SBudHode.SBudId).
  END.

  CASE icFieldName:
    WHEN "Aktiv" THEN 
        DO WITH FRAME DEFAULT-FRAME:
          IF (   INT(MalId:SCREEN-VALUE) = 0
              OR INT(ButikkNr:SCREEN-VALUE) = 0
              OR INT(Aar:SCREEN-VALUE) = 0
              )  
              AND SELF:CHECKED = TRUE  THEN
          DO:
              MESSAGE 'Beskrivelse, butikknr., malid og år må være angitt.' SKIP
                      'Budsjettet kan ikke aktiveres før det er bygget og en mal er koblet til det.' SKIP
                      'Marker budsjettet, og velg "Oppdater budsjett". Velg en mal og trykk OK.'
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
              ASSIGN
                  SELF:CHECKED = FALSE
                  AktivertDato:SCREEN-VALUE = ?
                  AktivertAv:SCREEN-VALUE   = ''
                  SalgBudsjett:SENSITIVE = NOT Aktiv:CHECKED  
                  DbBudsjett:SENSITIVE   = NOT Aktiv:CHECKED  
                  Db%:SENSITIVE          = NOT Aktiv:CHECKED   
                  .
          END.
          ELSE IF cSBudLst <> '' THEN
          DO:
              MESSAGE 'Det finnes allerede et aktivt budsjett for år ' + Aar:SCREEN-VALUE + '.' SKIP
                      'Budsjett ' + cSBudLst + ' er aktivt.' SKIP(1)
                      'Du må av aktivere det andre budsjettet før du kan aktivere dette.'
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
              ASSIGN Aktiv:CHECKED = FALSE.
              RETURN NO-APPLY.
          END.
          ELSE DO:
              IF SELF:CHECKED THEN
                ASSIGN
                  AktivertDato:SCREEN-VALUE = STRING(TODAY)
                  AktivertAv:SCREEN-VALUE   = USERID("SkoTex")
                  SalgBudsjett:SENSITIVE = NOT Aktiv:CHECKED  
                  DbBudsjett:SENSITIVE   = NOT Aktiv:CHECKED  
                  Db%:SENSITIVE          = NOT Aktiv:CHECKED   
                  .
              ELSE 
          END.
        END.
  END CASE.
  /*RUN SaveRecord.*/
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ValueChangedWarning C-Win 
FUNCTION ValueChangedWarning RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
      IF INT(MalId:SCREEN-VALUE) > 0 THEN
      DO:
          bOk = FALSE.
          MESSAGE 'Budsjettet er allerede oppdatert med budsjettmal. ' SKIP 
                  'Hvis omsetningsbeløp eller dekningsbidrag endres, må budsjettet oppdateres på nytt. Dette gjøres ved å kjøre funksjonen for kobling mot busjettmal en gang til.' SKIP(1)
                  'Omsetning og dekningsbidrag fordeles når budsjettmal kobles mot budsjettet. Når dette gjøres vil budsjettet bli nullstilt og ny fordeling gjort ut fra budsjettmalen.' SKIP(1)
                  'Er du sikker på at du vil endre på budsjettet?'
              VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE bOk.
          IF bOk <> TRUE THEN
          DO:
              ASSIGN
                  Salgbudsjett:SCREEN-VALUE = STRING(Salgbudsjett)
                  DbBudsjett:SCREEN-VALUE   = STRING(DbBudsjett)
                  ProsentPM:SCREEN-VALUE    = STRING(ProsentPM)
                  DB%:SCREEN-VALUE          = STRING(DB%)
                  .
              RETURN FALSE.
          END.
          ELSE RETURN TRUE.
      END.
      ELSE RETURN FALSE. 
  END.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

