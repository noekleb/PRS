&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

update_varebok_from_artbas.p
vareboklinjeDvelgfelter.w           
vareboklinje_refresh_all.p           
           
           
           
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
DEF VAR hParentBrowse     AS HANDLE NO-UNDO.
DEF VAR hSearchField      AS HANDLE NO-UNDO.

DEF VAR hChild            AS HANDLE NO-UNDO.
DEF VAR hDetail           AS HANDLE NO-UNDO.
DEF VAR hBuffer           AS HANDLE NO-UNDO.
DEF VAR hTransferer       AS HANDLE NO-UNDO.

DEF VAR cRowidList        AS CHAR NO-UNDO.
DEF VAR cButLst           AS CHAR NO-UNDO. 
/*Valg av poster*/
DEF VAR cDato   AS CHAR NO-UNDO.
DEF VAR iReturn AS INT  NO-UNDO.
DEF VAR hUtvalg AS HANDLE NO-UNDO.
DEF VAR cButikkIdList    AS CHAR NO-UNDO.
DEF VAR cButikkRowidList AS CHAR NO-UNDO.
DEF VAR iBrGrpNr     AS INT NO-UNDO.
DEF VAR iBrukerType  AS INT NO-UNDO.
DEF VAR iButNr       AS INT NO-UNDO.
DEF VAR cUser        AS CHAR NO-UNDO.
DEF VAR hTeamCombo   AS HANDLE NO-UNDO.
DEF VAR hSourceBrw   AS HANDLE NO-UNDO.

DEF VAR iCurrentButikkNr AS INT NO-UNDO.

DEF VAR hbcAktiv   AS HANDLE NO-UNDO.
DEF VAR hbfAktiv   AS HANDLE NO-UNDO.

DEF VAR iGreen    AS INT INIT 10 NO-UNDO.
DEF VAR iRed      AS INT INIT 12 NO-UNDO.
DEF VAR iYellow   AS INT INIT 14 NO-UNDO.
DEF VAR iLtYellow AS INT INIT 16 NO-UNDO.
DEF VAR iLtRed    AS INT INIT 17 NO-UNDO.
DEF VAR iLtGreen  AS INT INIT 18 NO-UNDO.

/* DEF VAR iFontWingdings    AS INT    NO-UNDO.                                                    */
/* iFontWingdings = DYNAMIC-FUNCTION("setAppFont","Wingdings, size=11 Script=symbol","") NO-ERROR. */

{buildfunction.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS SalgBudsjettTotal CB-Aar CB-Aktiv ~
butikkliste btnButikk DbBudsjettTotal DbSnitt rectBrowse rectToolBar ~
searchField 
&Scoped-Define DISPLAYED-OBJECTS SalgBudsjettTotal CB-Aar CB-Aktiv ~
butikkliste DbBudsjettTotal DbSnitt 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AdjustBrowseColumns C-Win 
FUNCTION AdjustBrowseColumns RETURNS LOGICAL
  ( INPUT ihBrowse     AS HANDLE,
    INPUT icBrowseName AS CHAR) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD friskOppRad C-Win 
FUNCTION friskOppRad RETURNS LOGICAL
  ( INPUT obOk AS LOG /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getButikkNr C-Win 
FUNCTION getButikkNr RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getSBudId C-Win 
FUNCTION getSBudId RETURNS INTEGER
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
  ( INPUT ihQuery AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setWindowSensitive C-Win 
FUNCTION setWindowSensitive RETURNS LOGICAL
  (INPUT ipSensitive AS LOG)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnButikk  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE CB-Aar AS CHARACTER FORMAT "X(256)":U 
     LABEL "År" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE CB-Aktiv AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Aktiv" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Alle",0,
                     "Aktive",1,
                     "Ikke aktive",2
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE butikkliste AS CHARACTER FORMAT "x(1034)":U 
     LABEL "Butikk" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 109 BY 1 NO-UNDO.

DEFINE VARIABLE DbBudsjettTotal AS DECIMAL FORMAT "->>,>>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN NATIVE 
     SIZE 29.8 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE DbSnitt AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN NATIVE 
     SIZE 17 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE SalgBudsjettTotal AS DECIMAL FORMAT "->>,>>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN NATIVE 
     SIZE 29.8 BY 1
     FONT 6 NO-UNDO.

DEFINE RECTANGLE rectBrowse
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 197.6 BY 13.1.

DEFINE RECTANGLE rectToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 28 BY .95.

DEFINE RECTANGLE searchField
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 15 BY .95.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     SalgBudsjettTotal AT ROW 17.91 COL 79.2 COLON-ALIGNED NO-LABEL
     CB-Aar AT ROW 3.38 COL 24 COLON-ALIGNED WIDGET-ID 10
     CB-Aktiv AT ROW 3.38 COL 51.8 COLON-ALIGNED WIDGET-ID 6
     butikkliste AT ROW 3.38 COL 83 COLON-ALIGNED
     btnButikk AT ROW 3.38 COL 193.8 NO-TAB-STOP 
     DbBudsjettTotal AT ROW 17.91 COL 109.8 COLON-ALIGNED NO-LABEL
     DbSnitt AT ROW 17.91 COL 140.2 COLON-ALIGNED NO-LABEL
     rectBrowse AT ROW 4.57 COL 1.4
     rectToolBar AT ROW 1.24 COL 2
     searchField AT ROW 3.43 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 198.6 BY 18.05.


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
/* SUPPRESS Window definition (used by the UIB) 
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Budsjett hode"
         HEIGHT             = 18.05
         WIDTH              = 198.6
         MAX-HEIGHT         = 57.14
         MAX-WIDTH          = 384
         VIRTUAL-HEIGHT     = 57.14
         VIRTUAL-WIDTH      = 384
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
   FRAME-NAME Custom                                                    */
ASSIGN 
       butikkliste:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       DbBudsjettTotal:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       DbSnitt:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       SalgBudsjettTotal:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

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
ON END-ERROR OF C-Win /* Budsjett hode */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Budsjett hode */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnButikk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnButikk C-Win
ON CHOOSE OF btnButikk IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF butikkliste
DO:
  cButikkIdList = REPLACE(cButLst,',','|').





  cButikkRowidList =
     DYNAMIC-FUNCTION("getRowIdList","butiker","",
              "WHERE can-do('" + cButLst + "',STRING(Butiker.butik))").
  
  /* Invoke the selector: */
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "Butiker;Butik;ButNamn;+!TeamList|character|x(255)|butikkteam.p(rowid" + cUser + "¤2" + ")"
                      ,"where true"
                      ,INPUT-OUTPUT cButikkRowidList,
                      "Butik",
                      INPUT-OUTPUT cButikkIdList,
                      "","",
                      OUTPUT bOK).
  ASSIGN 
      cButLst                  = REPLACE(cButikkIdList,'|',',')
      butikkliste:SCREEN-VALUE = cButLst.
  
  IF cButikkIdList <> '' THEN
      IF VALID-HANDLE(hBrowse) THEN RUN InvokeMethod (hBrowse,'OpenQuery').   

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME butikkliste
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL butikkliste C-Win
ON LEAVE OF butikkliste IN FRAME DEFAULT-FRAME /* Butikk */
DO:
  /*
  DEF VAR ix AS INT NO-UNDO.
  ASSIGN 
    ix = int(SELF:SCREEN-VALUE)
    NO-ERROR.
  IF ERROR-STATUS:ERROR THEN
  DO:
    MESSAGE "Ugyldig tegn i butikk, må være heltall"
       VIEW-AS ALERT-BOX INFO BUTTONS OK.
    SELF:AUTO-ZAP = TRUE.
    RETURN NO-APPLY.
  END.
  ELSE
    IF SELF:MODIFIED AND VALID-HANDLE(hBrowse) THEN RUN InvokeMethod (hBrowse,'OpenQuery').
  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL butikkliste C-Win
ON TAB OF butikkliste IN FRAME DEFAULT-FRAME /* Butikk */
OR RETURN OF ButikkListe
DO:
    RUN InvokeMethod (hBrowse,'OpenQuery').
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-Aar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Aar C-Win
ON VALUE-CHANGED OF CB-Aar IN FRAME DEFAULT-FRAME /* År */
DO:
    IF VALID-HANDLE(hBrowse) THEN RUN InvokeMethod (hBrowse,'OpenQuery').       
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-Aktiv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Aktiv C-Win
ON VALUE-CHANGED OF CB-Aktiv IN FRAME DEFAULT-FRAME /* Aktiv */
DO:
    IF VALID-HANDLE(hBrowse) THEN RUN InvokeMethod (hBrowse,'OpenQuery').     
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

  IF VALID-HANDLE(hUtvalg) THEN APPLY "close" TO hUtvalg.
  IF VALID-HANDLE(hChild) THEN APPLY "close" TO hChild.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  
  DEF NEW SHARED VAR wCurrLng   AS CHAR   INITIAL "DES" NO-UNDO.
  DEF NEW SHARED VAR wLngHandle AS HANDLE NO-UNDO.

  RUN enable_UI.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

{incl/supptrigg.i hBrowse}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CopyRecord C-Win 
PROCEDURE CopyRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF VAR cParam AS CHAR NO-UNDO.

  DO WITH FRAME Default-Frame:
    ASSIGN
        cParam = STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('SBudId'):BUFFER-VALUE) + '|' + 
                 STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('ButikkNr'):BUFFER-VALUE) + '|' + 
                 STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('Aar'):BUFFER-VALUE) + '|' + 
                 STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('SBudBeskrivelse'):BUFFER-VALUE) 
        .

    RUN Budhode_copy.w PERSISTENT SET hDetail.
    RUN InitializeObject IN hDetail.
    DYNAMIC-FUNCTION("CreateOneToOneLink",DYNAMIC-FUNCTION("getQuery" IN hDetail),hBrowse,"SBudId").
    
    RUN myNewRecord IN hDetail (INT(ButikkListe:screen-value)).
    IF RETURN-VALUE = "AVBRYT" THEN 
        APPLY "CLOSE" TO hDetail.
    ELSE DO:
      RUN MoveToTop IN hDetail.
      RUN setDefaultVerdier IN hDetail (cParam).
      DYNAMIC-FUNCTION("SetWindowSensitive",FALSE).
    END.
  END. /* Default-Frame */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DefaultActionBrowse C-Win 
PROCEDURE DefaultActionBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*   IF hBrowse:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN */
/*     RUN openVisAvvik.                              */

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

/*   MESSAGE 'Skal budsjettet slettes?'                                                                                                */
/*      VIEW-AS ALERT-BOX WARNING BUTTONS YES-NO UPDATE bOk.                                                                           */
/*   IF NOT bOk THEN                                                                                                                   */
/*     RETURN.                                                                                                                         */
/*   bOk = DYNAMIC-FUNCTION("runProc","sBudSlett.p",STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('SBudId'):BUFFER-VALUE),?). */
/*   IF bOk THEN                                                                                                                       */
/*       RUN FriskOppBrowsRecord.                                                                                                      */

  iReturn = 0.
  RUN JBoxBrowseSelectMsg.w ("Eksportere budsjett for valgte poster?",
      hBrowse:NUM-SELECTED-ROWS,
      IF INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount")) LT INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"rowstobatch")) THEN
      INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount"))
      ELSE 99999,
      OUTPUT iReturn).  /*1=Alle,2=Valgte*/

IF iReturn = 0 THEN RETURN NO-APPLY.

  IF iReturn = 1 OR hBrowse:NUM-SELECTED-ROWS = 0 THEN
    DO:
      /* Alle poster */
      DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"sBudSlett.p","").
    END.
  ELSE DO: 
      /* Valgte poster */
      DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowse,"sBudSlett.p","").
  END.
  RUN FriskOppBrowsRecord.


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
  DEF VAR cHentLokasjon   AS CHAR NO-UNDO.
  DEF VAR cDisabledEvents AS CHAR NO-UNDO.
  DEF VAR cUtvalg         AS CHAR NO-UNDO.

  IF hBuffer:AVAIL THEN DO:
      /*
      IF (hBuffer:BUFFER-FIELD("TelleType"):BUFFER-VALUE = 2 OR hBuffer:BUFFER-FIELD("Oppdatert"):BUFFER-VALUE <> ?) 
          THEN cHentLokasjon = 'HentLokasjon'.
      ELSE ASSIGN cHentLokasjon = ''.

      IF (hBuffer:BUFFER-FIELD("Oppdatert"):BUFFER-VALUE <> ?) 
          THEN cUtvalg = 'Utvalg'.
      ELSE ASSIGN cUtvalg = ''.

      ASSIGN
          cDisabledEvents = cHentLokasjon + (IF cHentLokasjon <> '' THEN ',' ELSE '') +
                            cUtvalg                            
          .
      DYNAMIC-FUNCTION("setAttribute",hToolbar,"DisabledEvents",cDisabledEvents).
      btnOppdater:SENSITIVE IN FRAME {&FRAME-NAME} = IF (hBuffer:BUFFER-FIELD("TelleType"):BUFFER-VALUE = 1 AND 
                                                         hBuffer:BUFFER-FIELD("Oppdatert"):BUFFER-VALUE = ?) 
                                                       THEN TRUE
                                                       ELSE FALSE.
      btnOppdater:SENSITIVE IN FRAME {&FRAME-NAME} = TRUE.
      */
  END.

  RUN SUPER.

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
  RUN sbudhode_upd.w PERSISTENT SET hDetail.
  RUN InitializeObject IN hDetail.
  
  DYNAMIC-FUNCTION("CreateOneToOneLink",DYNAMIC-FUNCTION("getQuery" IN hDetail),hBrowse,"SBudId").
  
  RUN InvokeMethod(hBrowse,"DisplayRecord").
  RUN MoveToTop IN hDetail.
  DYNAMIC-FUNCTION("SetWindowSensitive",FALSE).


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
  DISPLAY SalgBudsjettTotal CB-Aar CB-Aktiv butikkliste DbBudsjettTotal DbSnitt 
      WITH FRAME DEFAULT-FRAME.
  ENABLE SalgBudsjettTotal CB-Aar CB-Aktiv butikkliste btnButikk 
         DbBudsjettTotal DbSnitt rectBrowse rectToolBar searchField 
      WITH FRAME DEFAULT-FRAME.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExcelRecord C-Win 
PROCEDURE ExcelRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  iReturn = 0.
  RUN JBoxBrowseSelectMsg.w ("Eksportere budsjett for valgte poster?",
      hBrowse:NUM-SELECTED-ROWS,
      IF INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount")) LT INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"rowstobatch")) THEN
      INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount"))
      ELSE 99999,
      OUTPUT iReturn).  /*1=Alle,2=Valgte*/

IF iReturn = 0 THEN RETURN NO-APPLY.

  IF iReturn = 1 OR hBrowse:NUM-SELECTED-ROWS = 0 THEN
    DO:
      /* Alle poster */
      DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"SBudHode_til_Excel.p","").
    END.
  ELSE DO: 
      /* Valgte poster */
      DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowse,"SBudHode_til_Excel.p","").
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FargeTotaler C-Win 
PROCEDURE FargeTotaler :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF hBuffer:AVAIL THEN
  DO:
    DYNAMIC-FUNCTION("getLocalQueryStat",hBrowse,"").
    DYNAMIC-FUNCTION("RefreshRowids",hBrowse,hBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE). 
    SalgBudsjettTotal:SCREEN-VALUE IN FRAME {&FRAME-NAME} = DYNAMIC-FUNCTION("getAttribute",hBrowse,"LocalStatValueSalgBudsjett").
    DbBudsjettTotal:SCREEN-VALUE IN FRAME {&FRAME-NAME} = DYNAMIC-FUNCTION("getAttribute",hBrowse,"LocalStatValueDbBudsjett").
    
    DbSnitt:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(DEC(DYNAMIC-FUNCTION("getAttribute",hBrowse,"LocalStatValuetmpDbProc")) / DEC(DYNAMIC-FUNCTION("getAttribute",hBrowse,"LocalStatValueiAntbud"))).
    IF DEC(DYNAMIC-FUNCTION("getAttribute",hBrowse,"LocalStatValueiAntBud")) = 0 THEN
        DbSnitt:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ''.


  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FriskOppBrowsRecord C-Win 
PROCEDURE FriskOppBrowsRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF VALID-HANDLE(hBrowse) THEN RUN InvokeMethod (hBrowse,'OpenQuery').
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
  
  DEF VAR iDefaultButikkNr AS INT NO-UNDO.
  DEF VAR hColumn          AS HANDLE NO-UNDO.
  DEF VAR piLoop           AS INT NO-UNDO.
  DEF VAR cTekst           AS CHAR NO-UNDO.

  cUser = DYNAMIC-FUNCTION('getASUserId').

  /* Er brukeren brukertype 2 og det er satt butikknr. på bruker, skal restriksjoner tre i kraft. */
  ASSIGN iBrGrpNr    = INT(DYNAMIC-FUNCTION('getFieldValues','Bruker','WHERE BrukerId = ' + QUOTER(cUser),'BrGrpNr')) NO-ERROR.
  ASSIGN iButNr      = INT(DYNAMIC-FUNCTION("getFieldValues","Bruker","WHERE brukerid = '" + DYNAMIC-FUNCTION("getASuserId") + "'","ButikkNr")) NO-ERROR.
  ASSIGN iBrukerType = INT(DYNAMIC-FUNCTION("getFieldValues","Bruker","WHERE brukerid = '" + DYNAMIC-FUNCTION("getASuserId") + "'","BrukerType")) NO-ERROR.

  /* Bygger liste med aktuelle årstall */
  cTekst = 'Alle|0'.
  DO piLoop = YEAR(TODAY) TO 2000 BY -1:
      cTekst = cTekst + 
               (IF cTekst <> '' THEN '|' ELSE '') + 
               STRING(piLoop) + '|' + STRING(piLoop).
  END.

  ASSIGN 
    CB-Aar:DELIMITER       = "|"
    CB-Aar:LIST-ITEM-PAIRS = cTekst
    CB-Aar:SCREEN-VALUE    = '0'.

  IF iBrukertype = 2 AND iButNr <> 0 THEN 
      ASSIGN
        ButikkListe:SENSITIVE    = FALSE
        btnButikk:SENSITIVE      = FALSE
        cButLst                  = STRING(iButNr)
        ButikkListe:SCREEN-VALUE = STRING(iButNr).
        .

  DYNAMIC-FUNCTION("NewObject",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE,"procedure").
  hBrowse = DYNAMIC-FUNCTION("NewBrowse",
                              rectBrowse:HANDLE,
                              100,
                              "MULTIPLE",
                              "SBudHode"                               
                               + ";SBudId"
                               + ";ButikkNr"
                               + ";Aar|År"
                               + ";SBudBeskrivelse|Beskrivelse|x(30)"
                               + ";MalId|MalId"
                               + ";SalgBudsjett|Omsetning      |->>><>>><>>9"
                               + ";DbBudsjett|Db|->>><>>><>>9"
                               + ";+tmpDbProc|dec|->>>9.99|tmpDbProc(ROWID)|Db%"
                               + ";Aktiv|Aktiv|*/"
                               + ";AktivertDato"
                               + ";!AktivertTid"
                               + ";AktivertAv"
                               + ";EDato"
                               + ";!ETid"
                               + ";Brukerid"
                               + ";RegistrertDato"
                               + ";!RegistrertTid"
                               + ";RegistrertAv"
                               + ";!+iAntBud|integer|->>>9|iAntBud(ROWID)|AntBud"
                             ,"WHERE false"
                             ,"").
                             
  hColumn = DYNAMIC-FUNCTION("getBrowseColumn",hBrowse,"SalgBudsjett").
  hColumn:WIDTH-PIXELS = 150.
  hColumn = DYNAMIC-FUNCTION("getBrowseColumn",hBrowse,"DbBudsjett").
  hColumn:WIDTH-PIXELS = 150.
  hColumn = DYNAMIC-FUNCTION("getBrowseColumn",hBrowse,"tmpDbProc").
  hColumn:WIDTH-PIXELS = 80.

  hBuffer = hBrowse:QUERY:GET-BUFFER-HANDLE(1).
  hSearchField = DYNAMIC-FUNCTION("NewBrowseSearchField",SearchField:HANDLE,hBrowse,1).
  DYNAMIC-FUNCTION("CreateObjectLink",hSearchField,hBrowse).
    
  DYNAMIC-FUNCTION("setSortString",hBrowse,"SBudId;desc").   
    
  DYNAMIC-FUNCTION('setAttribute',hBrowse,'baseQuery','where true').
    
  DYNAMIC-FUNCTION("setAttribute",hBrowse,
                "queryStatFields","SalgBudsjett,DbBudsjett,iAntBud,tmpDbProc").

/*   DYNAMIC-FUNCTION('setAttribute',hBrowse,'calcfieldproc','vpiartbas_varebok_brwcalc.p'). */
  DYNAMIC-FUNCTION("createObjectLink",hBrowse,THIS-PROCEDURE).
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"customDeleteValProc","=delval_sbudhode.p").
/*   DYNAMIC-FUNCTION("setAttribute",hBrowse,"customDeleteValProc","ignore"). */
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"FieldNameDeleteWarning","SBudId").
  DYNAMIC-FUNCTION('setAttribute',hBrowse,'calcfieldproc','sbudhode_brwcalc.p').
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"postUpdateProc","sbudhode_post_update.p").  
  
  hToolbar = DYNAMIC-FUNCTION("NewToolBar",
                    rectToolBar:HANDLE,             /* Rectangle to define coordinates for toolbar */
                    "Fil",                         /* Corresponding menu label - no menu if blank */
                    "New;&Ny,Edit;&Endre,Copy,delete;Slette,Print;S&kriv,refresh,Excel,rule"
                    + ",Oppdater;Koble budsjett mot mal og oppdater"
                    ,"maxborder").                  /* Misc - enable, maxborder.. */

  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hToolbar).  
  /*SUBSCRIBE TO 'setButikkNr' ANYWHERE.*/
  
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InvalidateHandle C-Win 
PROCEDURE InvalidateHandle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DYNAMIC-FUNCTION('setCurrentObject',hBrowse).  
DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hToolbar).  
hChild = ?.
  APPLY 'value-changed' TO hBrowse.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE myTeam C-Win 
PROCEDURE myTeam :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihSourceBrw AS HANDLE NO-UNDO.
DEF INPUT PARAM ihDropDown  AS HANDLE NO-UNDO.

IF ihDropDown:SCREEN-VALUE = "" THEN
  DYNAMIC-FUNCTION("setAttribute",ihSourceBrw,"QueryFilter","").
ELSE
  DYNAMIC-FUNCTION("setAttribute",ihSourceBrw,"QueryFilter"," AND CAN-DO(teamlist," + QUOTER(ihDropDown:SCREEN-VALUE) + ')').

DYNAMIC-FUNCTION("setCurrentObject",ihSourceBrw).

RUN openQuery.

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
  
      RUN sbudhode_upd.w PERSISTENT SET hDetail.

      RUN InitializeObject IN hDetail.
      
      DYNAMIC-FUNCTION("CreateOneToOneLink",DYNAMIC-FUNCTION("getQuery" IN hDetail),hBrowse,"SBudId").
      
      RUN myNewRecord IN hDetail (INT(ButikkListe:screen-value)).
      IF RETURN-VALUE = "AVBRYT" THEN DO:
          APPLY "CLOSE" TO hDetail.
      END.
      ELSE DO:
          RUN MoveToTop IN hDetail.
          DYNAMIC-FUNCTION("SetWindowSensitive",FALSE).
      END.
     
  END. /* Default-Frame */
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
  DEF VAR cWhere AS CHAR NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:

    DYNAMIC-FUNCTION("setAttribute",hBrowse,"QueryFilter",'').


    cWhere = ' AND ' + 
             (IF CB-Aktiv:SCREEN-VALUE = '0' 
                 THEN " TRUE " 
              ELSE IF CB-Aktiv:SCREEN-VALUE = '1' 
                 THEN " SBudHode.Aktiv = TRUE  "
               ELSE " SBudHode.Aktiv = FALSE ")
              . 

    IF cButLst <> '' THEN
        cWhere = cWhere + 
                 " AND CAN-DO('" + cButLst + "',STRING(SBudHode.ButikkNr)) ". 
    

    IF CB-Aar:SCREEN-VALUE <> '0' THEN
        cWhere = cWhere + 
                  " AND SBudHode.Aar = '" + CB-Aar:SCREEN-VALUE + "' ". 

    DYNAMIC-FUNCTION("setAttribute",hBrowse,"QueryFilter",cWhere).

  END.

  RUN SUPER.

  RUN fargeTotaler.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdaterRad C-Win 
PROCEDURE oppdaterRad :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DYNAMIC-FUNCTION("RefreshRowids",hBrowse,hBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE).
  
  hbcAktiv:BGCOLOR =  IF hbfAktiv:BUFFER-VALUE = TRUE
                           THEN iLtGreen
                         ELSE ?.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppdaterRecord C-Win 
PROCEDURE OppdaterRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR piMalId AS INT FORMAT ">>>9" NO-UNDO.
  DEF VAR cLookupValue AS CHAR NO-UNDO.
  DEF VAR bSvar AS LOG FORMAT "J/N" NO-UNDO.
  
  IF hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('MalId'):BUFFER-VALUE <> 0  THEN 
  DO:
      bSvar = FALSE.
      MESSAGE 'Budsjettet er allerede tildelt en mal. Velger du å oppdatere med en ny mal, vil ' + 
              'alle verdier på budsjettets måned og dag linjer bli nullstilt og beregnet på nytt.'
          VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE bSvar.
      IF NOT bSvar THEN
          RETURN NO-APPLY.
  END.

  IF hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('ButikkNr'):BUFFER-VALUE = 0 OR 
      hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('Aar'):BUFFER-VALUE = 0 THEN 
  DO:
      MESSAGE 'Butikknr. og år må være angitt før budsjettet kan oppdateres.'
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
  END.
  
  cLookupValue = 'MalId'.
  RUN JBoxDLookup.w ("SBudMalHode;MalId;MalBeskrivelse;Aar;ButikkNr", 
                   "WHERE ButikkNr = '" + STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('ButikkNr'):BUFFER-VALUE) +
                     "' AND Aar = '" + STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('Aar'):BUFFER-VALUE) + "'",
                   INPUT-OUTPUT cLookupValue).
  IF cLookupValue <> '' THEN   
  BLOKKEN:
  DO ON STOP UNDO, RETRY:
    IF RETRY THEN 
    DO:
        MESSAGE 'Finner ikke programmet sbudhode_generer.p.'
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        LEAVE BLOKKEN.
    END.
    
    RUN sbudhode_generer.p (hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('SBudId'):BUFFER-VALUE,
                            DEC(cLookupValue),
                            hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('ButikkNr'):BUFFER-VALUE,
                            hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('Aar'):BUFFER-VALUE,
                            hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('SalgBudsjett'):BUFFER-VALUE,
                            hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('DbBudsjett'):BUFFER-VALUE
                            ) NO-ERROR.
    
    DYNAMIC-FUNCTION("RefreshRowids",hBrowse,hBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE).
  END. /* BLOKKEN */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppdaterSumRecord C-Win 
PROCEDURE OppdaterSumRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 IF hBuffer:AVAILABLE THEN
      DO:
/*      SESSION:SET-WAIT-STATE("GENERAL"). */
        IF DYNAMIC-FUNCTION("runproc","sbudhode_oppdatersum.p",STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('rowident1'):BUFFER-VALUE),?) 
            THEN. /* Gjør ingenting. */
        DYNAMIC-FUNCTION("RefreshRowids",hBrowse,hBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE).

/*      RUN InvokeMethod(hBrowse,"OpenQuery"). */
/*      SESSION:SET-WAIT-STATE(""). */
      END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrintRecord C-Win 
PROCEDURE PrintRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /*
  RUN printTellelisteX.p (TO-ROWID(hBrowse:QUERY:get-buffer-handle(1):BUFFER-FIELD('RowIdent1'):BUFFER-VALUE),"").
  */
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
    hbcAktiv:BGCOLOR =  IF hbfAktiv:BUFFER-VALUE = TRUE
                             THEN iLtGreen
                           ELSE ?.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setButikkNr C-Win 
PROCEDURE setButikkNr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  Denne metoden blir publisert fra varetelling.w 
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipButikkNr AS INT NO-UNDO.

  iCurrentButikkNr = ipButikkNr.
  DO WITH FRAME {&FRAME-NAME}:
    butikkliste:SCREEN-VALUE = STRING(iCurrentButikkNr).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setSelectorAttributes C-Win 
PROCEDURE setSelectorAttributes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF INPUT PARAM ihSourceBrw   AS HANDLE NO-UNDO.
DEF INPUT PARAM ihTargetBrw   AS HANDLE NO-UNDO.

DEF VAR   cList AS CHAR NO-UNDO.
DEF VAR   hFilterToolbar AS HANDLE NO-UNDO.
DEF VAR   hFilterButton  AS HANDLE NO-UNDO.

DEF VAR hSourceBuffer  AS HANDLE NO-UNDO.
DEF VAR hTargetBuffer  AS HANDLE NO-UNDO.
DEF VAR ix             AS INT    NO-UNDO.
DEF VAR cSelectedRows  AS CHAR   NO-UNDO.

  /*Finn filter toolbar, disable filter knapp og skjul den */
  hFilterToolbar = DYNAMIC-FUNCTION("getLinkedObject",ihSourceBrw,'Toolbar','FROM').
  DYNAMIC-FUNCTION("setAttribute",hFilterToolbar,'DisabledEvents','Filter').
  hFilterButton = DYNAMIC-FUNCTION("getEventWidget",hFilterToolbar,'Filter','').
  IF VALID-HANDLE(hFilterButton) THEN  hFilterButton:VISIBLE = FALSE.

  cList = DYNAMIC-FUNCTION("getFieldList",'ButikkTeam;TeamNr|Beskrivelse;+rButikkTeam|butikkteam_rowid.p',
                           'WHERE BrGrpNr = ' + STRING(iBrGrpNr) + 
                           '  AND TeamTypeId = "2"' /*Hardkodet tallet 3 (2 inntil det er lagt opp egen verdi for kampanje*/
                           ).
  IF cList = '' THEN cList = '|'.
  ELSE cList = '||' + cList.  
  CREATE COMBO-BOX hTeamCombo 
    ASSIGN DELIMITER        = "|"
           DATA-TYPE        = "CHARACTER"
           FORMAT           = "x(256)"
           NAME             = "cbTeam"
           SUBTYPE          = "DROP-DOWN-LIST"
           LIST-ITEM-PAIRS  = cList
           INNER-LINES      = 50
           FRAME            = ihSourceBrw:FRAME
           X                = 90
           Y                = 5 
           WIDTH-PIXELS     = 200
           VISIBLE          = TRUE
           SENSITIVE        = TRUE
           TRIGGERS:
             ON VALUE-CHANGED PERSISTENT RUN myTeam IN THIS-PROCEDURE (INPUT ihSourceBrw, INPUT hTeamCombo).
  /*            ON TAB           PERSISTENT RUN DoProcessEvent (SOURCE-PROCEDURE,"leave"). */
  /*            ON BACK-TAB      PERSISTENT RUN DoProcessEvent (SOURCE-PROCEDURE,"leave"). */
  /*            ON RETURN        PERSISTENT RUN DoProcessEvent (SOURCE-PROCEDURE,"leave"). */
  /*            ON ENTRY         PERSISTENT RUN DoProcessEvent (SOURCE-PROCEDURE,"entry"). */
             ON END-ERROR     PERSISTENT RUN DoProcessEvent (SOURCE-PROCEDURE,"end-error").
           END TRIGGERS.
   hSourceBrw = ihSourceBrw.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AdjustBrowseColumns C-Win 
FUNCTION AdjustBrowseColumns RETURNS LOGICAL
  ( INPUT ihBrowse     AS HANDLE,
    INPUT icBrowseName AS CHAR):
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  IF icBrowseName = 'RectBrowse' THEN
  DO ix = 1 TO ihBrowse:NUM-COLUMNS:
    CASE ihBrowse:GET-BROWSE-COLUMN(ix):NAME:
      WHEN 'Aktiv' THEN 
        ASSIGN
          hbcAktiv = ihBrowse:GET-BROWSE-COLUMN(ix)
          hbfAktiv = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('Aktiv')                                                                            
        .
    END CASE.
  END.

  RETURN TRUE.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION friskOppRad C-Win 
FUNCTION friskOppRad RETURNS LOGICAL
  ( INPUT obOk AS LOG /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  IF obOK = TRUE THEN
      DYNAMIC-FUNCTION("RefreshRowids",hBrowse,hBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE).
  APPLY "LEAVE" TO butikkliste IN FRAME DEFAULT-FRAME.
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getButikkNr C-Win 
FUNCTION getButikkNr RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN butikkliste:SCREEN-VALUE IN FRAME {&FRAME-NAME}.   /* Function return value. */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getSBudId C-Win 
FUNCTION getSBudId RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  RETURN hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('SBudId'):BUFFER-VALUE.
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
  DYNAMIC-FUNCTION("setAddMoveY", 
                   THIS-PROCEDURE:CURRENT-WINDOW, 
                   FRAME {&FRAME-NAME}:HANDLE, 
                   "SalgBudsjettTotal,DbBudsjettTotal,DbSnitt").

  DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "searchField,rectButton").
  DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "rectButton").

  DYNAMIC-FUNCTION("setAddMoveX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,
  "btnOppdater,rectButton").
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
hParentBrowse = ihQuery.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setWindowSensitive C-Win 
FUNCTION setWindowSensitive RETURNS LOGICAL
  (INPUT ipSensitive AS LOG) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = ipSensitive.
  RETURN TRUE.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

