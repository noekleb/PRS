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

DEF VAR cRowidList           AS CHAR   NO-UNDO.
/*Valg av poster*/
DEF VAR cDato   AS CHAR NO-UNDO.
DEF VAR iReturn AS INT  NO-UNDO.
DEF VAR hUtvalg AS HANDLE NO-UNDO.
DEF VAR bUtvalgIsMaster AS LOG NO-UNDO.

DEF VAR iCurrentButikkNr AS INT NO-UNDO.

DEF VAR hbcTelleType AS HANDLE NO-UNDO.
DEF VAR hbfTelleType AS HANDLE NO-UNDO.
DEF VAR hbfOppdatert AS HANDLE NO-UNDO.

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
&Scoped-Define ENABLED-OBJECTS btnOppdater butikkliste btnButikk rectBrowse ~
rectToolBar searchField 
&Scoped-Define DISPLAYED-OBJECTS butikkliste 

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getVareTellingNr C-Win 
FUNCTION getVareTellingNr RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD InitializeResize C-Win 
FUNCTION InitializeResize RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD InitUtvalgToTelling C-Win 
FUNCTION InitUtvalgToTelling RETURNS LOGICAL
  ( INPUT ihUtvalg         AS HANDLE,
    INPUT ibUtvalgIsMaster AS LOG )  FORWARD.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD UtvalgIsMaster C-Win 
FUNCTION UtvalgIsMaster RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnButikk  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnOppdater 
     LABEL "Oppdater og avslutt telling" 
     SIZE 36 BY 1.19
     FGCOLOR 18 FONT 6.

DEFINE BUTTON btnTransfer  NO-FOCUS
     LABEL "Overf�r utvalg til telleliste" 
     SIZE 36 BY 1.19.

DEFINE VARIABLE butikkliste AS CHARACTER FORMAT "xxxx":U 
     LABEL "Butikk" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE RECTANGLE rectBrowse
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 185 BY 14.29.

DEFINE RECTANGLE rectButton
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 40 BY 2
     BGCOLOR 12 FGCOLOR 12 .

DEFINE RECTANGLE rectToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 28 BY .95.

DEFINE RECTANGLE searchField
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 15 BY .95.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnTransfer AT ROW 2.67 COL 109 NO-TAB-STOP 
     btnOppdater AT ROW 2.71 COL 148
     butikkliste AT ROW 3.43 COL 45.2 COLON-ALIGNED
     btnButikk AT ROW 3.43 COL 56.2 NO-TAB-STOP 
     rectBrowse AT ROW 4.57 COL 1
     rectToolBar AT ROW 1.24 COL 2
     searchField AT ROW 3.43 COL 2
     rectButton AT ROW 2.29 COL 146
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 186.2 BY 18.05.


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
         TITLE              = "Tellehode"
         HEIGHT             = 18.05
         WIDTH              = 186.2
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
   FRAME-NAME Custom                                                    */
/* SETTINGS FOR BUTTON btnTransfer IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       btnTransfer:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR RECTANGLE rectButton IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
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
ON END-ERROR OF C-Win /* Tellehode */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Tellehode */
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
  DEF VAR cLookupValue AS CHAR NO-UNDO.

  cLookupValue = "butik;butnamn".

  RUN JBoxDLookup.w ("butiker;butik;butnamn", 
                     "WHERE true",
                     INPUT-OUTPUT cLookupValue).

  IF cLookupValue NE "" THEN DO:
    ASSIGN 
      butikkliste:SCREEN-VALUE   = ENTRY(1,cLookupValue,"|")
    .
/*       SasBeskr:SCREEN-VALUE = ENTRY(2,cLookupValue,"|"). */
    IF butikkliste:MODIFIED AND VALID-HANDLE(hBrowse) THEN RUN InvokeMethod (hBrowse,'OpenQuery').   
  END.
  ELSE APPLY "entry" TO butikkliste.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOppdater
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOppdater C-Win
ON CHOOSE OF btnOppdater IN FRAME DEFAULT-FRAME /* Oppdater og avslutt telling */
DO:
  RUN OppdaterRecord.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnTransfer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnTransfer C-Win
ON CHOOSE OF btnTransfer IN FRAME DEFAULT-FRAME /* Overf�r utvalg til telleliste */
DO:  
  RUN TransferUtvalg.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME butikkliste
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL butikkliste C-Win
ON LEAVE OF butikkliste IN FRAME DEFAULT-FRAME /* Butikk */
DO:
  DEF VAR ix AS INT NO-UNDO.
  ASSIGN 
    ix = int(SELF:SCREEN-VALUE)
    NO-ERROR.
  IF ERROR-STATUS:ERROR THEN
  DO:
    MESSAGE "Ugyldig tegn i butikk, m� v�re heltall"
       VIEW-AS ALERT-BOX INFO BUTTONS OK.
    SELF:AUTO-ZAP = TRUE.
    RETURN NO-APPLY.
  END.
  ELSE
    IF SELF:MODIFIED AND VALID-HANDLE(hBrowse) THEN RUN InvokeMethod (hBrowse,'OpenQuery').
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
  DEF VAR iKoblet AS INT NO-UNDO.

  iKoblet = DYNAMIC-FUNCTION("getFieldValues","tellehode","WHERE koblettiltellenr = " 
                   + string(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('tellenr'):BUFFER-VALUE) 
                   ,"tellenr").
  IF iKoblet NE ? AND iKoblet NE 0 THEN
  DO:
    MESSAGE 'Det finnes lokasjonslister koblet til denne telleliste, de vil ogs� bli slettet. �nsker du fremdeles � slette listen?'
       VIEW-AS ALERT-BOX WARNING BUTTONS YES-NO UPDATE bOk.
    IF NOT bOk THEN
      RETURN.      
  END.
  ELSE DO:
      MESSAGE 'Skal tellelisten slettes?'
         VIEW-AS ALERT-BOX WARNING BUTTONS YES-NO UPDATE bOk.
      IF NOT bOk THEN
        RETURN.      
  END.

  /*RUN SUPER.*/

  bOk = DYNAMIC-FUNCTION("runProc","slettvaretelling.p",STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('tellenr'):BUFFER-VALUE),?).
  IF bOk THEN
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
  RUN tellehode_upd.w PERSISTENT SET hDetail.
  RUN InitializeObject IN hDetail.
  
  DYNAMIC-FUNCTION("CreateOneToOneLink",DYNAMIC-FUNCTION("getQuery" IN hDetail),hBrowse,"tellenr").
  
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
  DISPLAY butikkliste 
      WITH FRAME DEFAULT-FRAME.
  ENABLE btnOppdater butikkliste btnButikk rectBrowse rectToolBar searchField 
      WITH FRAME DEFAULT-FRAME.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FullListe C-Win 
PROCEDURE FullListe :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE bOK AS LOGICAL     NO-UNDO.
  DO:
      bOK = DYNAMIC-FUNCTION("RunProc","art_to_telling.p",
                             hBuffer:BUFFER-FIELD('TelleNr'):BUFFER-VALUE + "|ALLE,",
                              ?).
    IF NOT bOK THEN
      DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil i overf�ring til Telling",""). 
  END.
  RUN OppdaterSumRecord.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HentLokasjonRecord C-Win 
PROCEDURE HentLokasjonRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cLokasjonRowIdList AS CHAR  NO-UNDO.
DEF VAR cLokasjonidList    AS CHAR  NO-UNDO.
DEF VAR rRowid             AS ROWID NO-UNDO.
DEF VAR iTellenr           AS INT   NO-UNDO.

  IF hBuffer:BUFFER-FIELD("TelleType"):BUFFER-VALUE = 2 THEN
  DO:
      MESSAGE "Du kan ikke hente inn lokasjonslister i en lokasjonsliste." SKIP
              "Marker en telleliste f�r du velger denne funksjonen."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN.
  END.

  bOk = FALSE.
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  DO WITH FRAME {&FRAME-NAME}:
    RUN JBoxSelector.w (THIS-PROCEDURE,0,
                        "Tellehode;TelleNr;Beskrivelse;TTId;Oppdatert;ButikkListe",
                        "where TTId = 9 and Oppdatert = ? and and TelleType = 2 and TelleNr <> " + string(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('tellenr'):BUFFER-VALUE)
                        + " and ButikkListe = '" + butikkliste:SCREEN-value + "'",
                        INPUT-OUTPUT cLokasjonRowIdList,
                        "TelleNr",
                        INPUT-OUTPUT cLokasjonIdList,
                        "","",
                        OUTPUT bOK).
    THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.
    
    IF cLokasjonIdList = "" THEN
      RETURN.
      
    MESSAGE "Skal tellelistene " + cLokasjonIdList + " oppdateres mot �pen telleliste?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE bOk.
    
    IF NOT bOk THEN
        RETURN.
    IF butikkliste:SCREEN-VALUE = '' THEN 
    DO:
        
      MESSAGE PROGRAM-NAME(1) 'Butikkliste ikke satt !!! '
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN.
    END.

    THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
    
    RUN oppdatlokasjon.p (INPUT hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('tellenr'):BUFFER-VALUE,
                          INPUT butikkliste:SCREEN-VALUE,
                          INPUT cLokasjonIdList).
    THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

/*     SESSION:SET-WAIT-STATE("GENERAL"). */
    ASSIGN
      iTellenr = hBuffer:BUFFER-FIELD('tellenr'):BUFFER-VALUE
      rRowid = TO-ROWID(hBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE)
    .
    DYNAMIC-FUNCTION("runproc","tellehode_oppdatersum.p",STRING(rRowid),?).
    RUN InvokeMethod(hBrowse,"OpenQuery").
    hBuffer:FIND-FIRST('WHERE tellenr = ' + STRING(iTellenr),NO-LOCK) NO-ERROR.
    hBrowse:QUERY:REPOSITION-TO-ROWID(hBuffer:ROWID).

/*     SESSION:SET-WAIT-STATE(""). */
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HTsetupRecord C-Win 
PROCEDURE HTsetupRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE wTypeId AS INTEGER NO-UNDO.
  
  RUN d-bht-type.w (INPUT-OUTPUT wTypeId).
  RETURN NO-APPLY.

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
DEF VAR iBrGrpNr AS INT NO-UNDO.
DEF VAR iDefaultButikkNr AS INT NO-UNDO.
  DYNAMIC-FUNCTION("NewObject",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE,"procedure").
  hBrowse = DYNAMIC-FUNCTION("NewBrowse",
                              rectBrowse:HANDLE,
                              100,
                              "",
                              "tellehode"                               
                               + ";TelleNr"
                               + ";ButikkListe|Butikk|x(7)"
                               + ";!TelleType"
                               + ";Beskrivelse|Beskrivelse|x(50)"
                               + ";StartDato"
                               + ";Oppdatert"
                               + ";BatchNr|Batch"
                               + ";AntLinjer|Ant.linjer|-zzzzzz9"

                               + ";AntallPar|Ant.lager|-zzzzz9" 
                               + ";AntallTalt|Ant.talt|-zzzzz9"
                               + ";AntallDiff|Ant.Diff|-zzzzzz9" 
                               + ";AntallPosDiff|Ant.pos Diff|-zzzzzz9"
                               + ";AntallNegDiff|Ant.neg Diff|-zzzzzz9"

                               + ";OpprVerdi|Oppr.verdi|-zzzzzzzzzz9.99"
                               + ";OpptVerdi|Oppt.verdi|-zzzzzzzzzz9.99"
                               + ";VerdiDiff|Verdi diff|-zzzzzzzzzz9.99"
                               + ";VerdiPosDiff|Verdi pos diff|-zzzzzzzzzz9.99"
                               + ";VerdiNegDiff|Verdi neg diff|-zzzzzzzzzz9.99"

                               + ";!OrdreNr"
                               + ";!PkSdlNr"
                               + ";!TilButikk"
                               + ";RegistrertDato"
                               + ";!RegistrertTid"
                               + ";RegistrertAv"
                               + ";EDato"
                               + ";!ETid"
                               + ";BrukerID"
                               + ";LokasjonsId"
                               + ";KobletTilTelleNr"
                               + ";FilDatoPDA"
                               + ";FilTidPDA"
                               + ";BrukerIdPDA"
                               + ";TTId"
                               + ";!TBId"
                               + ";!Notat"
                               + ";Filid"
                               + ",sysPara;Parameter1|Listetype|x(15)@2"
                             ,"WHERE false"
                               + ", FIRST sysPara OUTER-JOIN WHERE sysPara.sysHid = 4 AND sysPara.sysGr = 2 AND sysPara.ParaNr = TelleHode.TelleType NO-LOCK"
                             ,"sort|TelleNr DESC").
                             
  hBuffer = hBrowse:QUERY:GET-BUFFER-HANDLE(1).
  hSearchField = DYNAMIC-FUNCTION("NewBrowseSearchField",SearchField:HANDLE,hBrowse,1).
  DYNAMIC-FUNCTION("CreateObjectLink",hSearchField,hBrowse).
    
    
  DYNAMIC-FUNCTION('setAttribute',hBrowse,'baseQuery','where TTId = 9 and KobletTilTelleNr = 0').
    
/*   DYNAMIC-FUNCTION('setAttribute',hBrowse,'calcfieldproc','vpiartbas_varebok_brwcalc.p'). */
  DYNAMIC-FUNCTION("createObjectLink",hBrowse,THIS-PROCEDURE).
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"customDeleteValProc","=delval_tellehode.p").
/*   DYNAMIC-FUNCTION("setAttribute",hBrowse,"customDeleteValProc","ignore"). */
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"FieldNameDeleteWarning","Tellenr").

  
  hToolbar = DYNAMIC-FUNCTION("NewToolBar",
                    rectToolBar:HANDLE,             /* Rectangle to define coordinates for toolbar */
                    "Fil",                         /* Corresponding menu label - no menu if blank */
                    "New;&Ny,Edit;&Endre,delete;Slette,Print;S&kriv,refresh,Excel,rule"
                    + ",Utvalg,HentLokasjon;Hent lokasjon,OppdaterSum;Oppdater sum felt p� valgt linje"
                    /*+ ",FriskOppBrows;Frisk opp vindu"*/
                    + ",HTsetup;H�ndterminal oppsett"
                    + ",TilHT;Eksporter vareregister til h�ndterminal"
                    ,"maxborder").                  /* Misc - enable, maxborder.. */

  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hToolbar).  
  SUBSCRIBE TO 'setButikkNr' ANYWHERE.
  SUBSCRIBE TO 'VaretellingOppdaterSumRecord' ANYWHERE RUN-PROCEDURE 'OppdaterSumRecord'. 
  
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Kontroll C-Win 
PROCEDURE Kontroll :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       Dette er et notat.
------------------------------------------------------------------------------*/
  DEFINE VARIABLE bOK AS LOGICAL     NO-UNDO.
  
  MESSAGE "Ved kontrolltelling, kan tellelisten initieres for deler av varelageret. Dette gj�res via artikkelutvalget." SKIP
              "�nsker du � gj�re dette n�, eller vil du gj�re dette senere? " SKIP
              "Bruk funksjonen 'Utvalg' for � gj�re det senere." SKIP
              "Bygg utvalg (Ja). Vent til senere med � bygge telleliste (Nei)."
          VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL UPDATE svar AS LOGICAL.
  IF svar = ? THEN
      RETURN NO-APPLY.
  ELSE IF svar = TRUE THEN DO:
      SESSION:SET-WAIT-STATE("general").
      IF NOT VALID-HANDLE(hUtvalg) THEN DO:
        RUN wtmpartbas.w PERSIST SET hUtvalg.
        DYNAMIC-FUNCTION("setTellingIsMaster" IN hUtvalg,TRUE).
        RUN InitializeObject IN hUtvalg.
    /*     InitUtvalgToTelling(hUtvalg,NO). /* NO: Utvalg er IKKE master */  */
      END.
      DYNAMIC-FUNCTION("InitFromTelling" IN hUtvalg, THIS-PROCEDURE).
  END.
  RUN OppdaterSumRecord.
  SESSION:SET-WAIT-STATE("").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Lokasjon C-Win 
PROCEDURE Lokasjon :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

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
  DEF VAR iTelleType    AS INT NO-UNDO.
  DEF VAR iOldTelleType AS INT NO-UNDO.
  DEF VAR bOk           AS LOG NO-UNDO.

  DO WITH FRAME Default-Frame:
  RUN velgtelletype_dialog.w (INPUT '', OUTPUT iTelleType, INPUT INT(ButikkListe:screen-value)).
  IF iTelleType = 0 THEN
      RETURN.
  END.
  iOldTelleType = iTelletype.

  IF iTelletype = 5 THEN
  DO:
      bOk = FALSE.
      MESSAGE "Denne funksjonen skal bare benyttes i forbindelse med korreksjon av en varetelling, " + 
              "og da bare i sammarbeide med support. Er du sikker p� at du vil gj�re dette?" 
          VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE bOk.
      IF bOk = FALSE THEN
          RETURN NO-APPLY.
  END.

  RUN tellehode_upd.w PERSISTENT SET hDetail.

  RUN InitializeObject IN hDetail.
  
  DYNAMIC-FUNCTION("CreateOneToOneLink",DYNAMIC-FUNCTION("getQuery" IN hDetail),hBrowse,"tellenr").
  
  RUN myNewRecord IN hDetail (INPUT iTelleType,9,INT(ButikkListe:screen-value)).
  IF RETURN-VALUE = "AVBRYT" THEN DO:
      APPLY "CLOSE" TO hDetail.
  END.
  ELSE DO:
      RUN MoveToTop IN hDetail.
      DYNAMIC-FUNCTION("SetWindowSensitive",FALSE).
  END.

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
DEF VAR cWhere   AS CHAR NO-UNDO.
DEF VAR cVPIDato AS CHAR NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:

/*     ASSIGN                                                                                                                */
/*         cVPIDato = '�' +                                                                                                  */
/*                    (IF INPUT FraVPIDato <> ? THEN STRING(INPUT FraVPIDato) ELSE '') + '�' +                               */
/*                    (IF INPUT TilVPIDato <> ? THEN STRING(INPUT TilVPIDato) ELSE '')                                       */
/*         .                                                                                                                 */
/*                                                                                                                           */
/*     DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcparamChkArtReg",tbAvvikArtBas:SCREEN-VALUE).                             */
/*     DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcparamChkVarebok",STRING(VarebokNr) + "�" + tbAvvikVarebok:SCREEN-VALUE). */
/*     DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcparamChkPris",STRING(VarebokNr) + "�" + tbAvvikPris:SCREEN-VALUE).       */
/*     DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcparamChkArtInfo",STRING(VarebokNr) + "�" + tbAvvikArtInfo:SCREEN-VALUE). */
/*     DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcparamChkStrek",tbAvvikStrekKode:SCREEN-VALUE).                           */
/*     DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcparamChkAnonseArtikkel",tbAnonseArtikkel:SCREEN-VALUE).                  */
/*     DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcparamChkKontroll",IF cbcKontroll:SCREEN-VALUE = ?                        */
/*                                                                      THEN ''                                              */
/*                                                                      ELSE (cbcKontroll:SCREEN-VALUE + cVPIDato) ).        */
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"QueryFilter",'').

    cWhere = buildFilter(cWhere,butikkliste:HANDLE,'butikkliste','=').

    DYNAMIC-FUNCTION("setAttribute",hBrowse,"QueryFilter",cWhere).

/*     hParentBrowse = DYNAMIC-FUNCTION("getLinkedObject",hBrowse,"parent","from").                                    */
/*     IF hParentBrowse:QUERY:GET-BUFFER-HANDLE(1):AVAIL AND                                                           */
/*        Strekkode:SCREEN-VALUE NE "" THEN                                                                            */
/*       DYNAMIC-FUNCTION("setAttribute",hBrowse,"prescanQueryFilter",                                                 */
/*                        "VPIstrekkode WHERE VPIStrekkode.EkstVPILevNr = "                                            */
/*                        + STRING(hParentBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("EkstVPILevNr"):BUFFER-VALUE) */
/*                        + " and VPIstrekkode.Kode = '" + Strekkode:SCREEN-VALUE + "'"                                */
/*                        + ",FIRST VPIartBas OF VPIstrekkode NO-LOCK").                                               */
/*     ELSE                                                                                                            */
/*       DYNAMIC-FUNCTION("setAttribute",hBrowse,"prescanQueryFilter","").                                             */
/*                                                                                                                     */

  END.

  RUN SUPER.
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

  IF hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('AntLinjer'):BUFFER-VALUE = 0 THEN
  DO:
      MESSAGE "Tom telleliste kan ikke oppdateres." SKIP
              "Legg inn tellelinjer, eller slett tellelisten."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN.
  END.

  bOk = FALSE.
  MESSAGE "Skal oppdatering av lager og statistikker startes?" SKIP
          "Denne rutinen starter oppdaterer lager og statistikker."
          VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Bekreft oppdatering"
          UPDATE bOk.

  IF bOk THEN
  DO:
    RUN oppdaterTelling.p (hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('TelleNr'):BUFFER-VALUE, THIS-PROCEDURE).

    DYNAMIC-FUNCTION("RefreshRowids",hBrowse,hBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE).

    /*RUN InvokeMethod(hBrowse,"OpenQuery").*/
  END.
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
        IF DYNAMIC-FUNCTION("runproc","tellehode_oppdatersum.p",STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('rowident1'):BUFFER-VALUE),?) 
            THEN. /* Gj�r ingenting. */
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
  RUN printTellelisteX.p (TO-ROWID(hBrowse:QUERY:get-buffer-handle(1):BUFFER-FIELD('RowIdent1'):BUFFER-VALUE),"").
  
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
  DEF VAR iGreen  AS INT INIT 10 NO-UNDO.
  DEF VAR iRed    AS INT INIT 12 NO-UNDO.
  DEF VAR iYellow AS INT INIT 14 NO-UNDO.
  DEF VAR iBlue   AS INT INIT 11 NO-UNDO.

  IF hbfOppdatert:BUFFER-VALUE = ? THEN
  DO:
    IF VALID-HANDLE(hbcTelleType) THEN 
      hbcTelleType:BGCOLOR = IF      hbfTelleType:BUFFER-VALUE = 1 THEN iBlue
                             ELSE IF hbfTelleType:BUFFER-VALUE = 2 THEN iYellow
                             ELSE ?.
      
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Rullerende C-Win 
PROCEDURE Rullerende :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE bOK AS LOGICAL     NO-UNDO.
  
  DEF VAR cLevResultat AS CHAR NO-UNDO.
  DEF VAR cVgResultat  AS CHAR NO-UNDO.

  
  RUN init_rullerende_telleliste.w (OUTPUT cVgResultat, OUTPUT cLevResultat).

  IF cVgResultat <> '' THEN
      bOK = DYNAMIC-FUNCTION("RunProc","art_to_telling.p",
                             hBuffer:BUFFER-FIELD('TelleNr'):BUFFER-VALUE + "|VG," + '|' + cVgResultat,
                              ?).
  IF cLevResultat <> '' THEN
      bOK = DYNAMIC-FUNCTION("RunProc","art_to_telling.p",
                             hBuffer:BUFFER-FIELD('TelleNr'):BUFFER-VALUE + "|LEV," + '|' + cLEvResultat,
                              ?).

  RUN OppdaterSumRecord.
  SESSION:SET-WAIT-STATE("").
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setTransferBtn C-Win 
PROCEDURE setTransferBtn :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipParent AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER ipEnable AS LOGICAL NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN 
      hTransferer           = ipParent
      btnTransfer:SENSITIVE = ipEnable
      btnTransfer:HIDDEN    = NOT ipEnable
    .
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TilHTrecord C-Win 
PROCEDURE TilHTrecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iTypeId LIKE HT-Type.TypeId NO-UNDO.
    DEFINE VARIABLE hArtbas AS HANDLE      NO-UNDO.
    DEFINE VARIABLE bOk AS LOG NO-UNDO.

    RUN d-velgHT.w (OUTPUT iTypeId).
    IF iTypeId = 0 THEN DO:
        MESSAGE "Ingen h�ndterminaltype ble valgt."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN.
    END.
    
    MESSAGE 'Starte utlegg av ALLE varer (med EAN koder) til h�ndterminal?' + CHR(10) 
            + 'Utlegget kan ta opptil noen minutter.'
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
        UPDATE bOk.
    IF bOk = FALSE THEN
        RETURN.
    ELSE DO:
        FIND HT-Type WHERE HT-Type.TypeId = iTypeId NO-LOCK.
        ASSIGN hArtbas = BUFFER Artbas:HANDLE.
        
        RUN eksportHTfil.p (hArtbas,
                            iTypeId,
                            HT-Type.Eksportkatalog,
                            HT-Type.EkspFilPrefix,
                            butikkliste:SCREEN-VALUE IN FRAME DEFAULT-FRAME
                            ).
        
        MESSAGE "Utlegg til h�ndterminal klar."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Transaksjon C-Win 
PROCEDURE Transaksjon :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TransferUtvalg C-Win 
PROCEDURE TransferUtvalg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF hBuffer:AVAIL AND hBuffer:BUFFER-FIELD('oppdatert'):BUFFER-VALUE = ?
    AND VALID-HANDLE(hTransferer) THEN
  DO:
    RUN OverfUtvalgTilTelling IN hTransferer (hBuffer:BUFFER-FIELD('tellenr'):BUFFER-VALUE, OUTPUT bOK).
/*     APPLY "choose" TO B-Linjer IN FRAME {&FRAME-NAME}. */
  END.
  ELSE IF NOT VALID-HANDLE(hTransferer) THEN
    DYNAMIC-FUNCTION("DoMessage",0,0,"Utvalg ikke tilgjengelig","","").
  ELSE IF hBuffer:AVAILABLE THEN 
    DYNAMIC-FUNCTION("DoMessage",0,0,"Telling &1 er oppdatert og kan ikke tilf�res flere artikler","",STRING(hBuffer:BUFFER-FIELD('tellenr'):BUFFER-VALUE)).
  ELSE 
    DYNAMIC-FUNCTION("DoMessage",0,0,"Ingen varetelling valgt","","").


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UtvalgRecord C-Win 
PROCEDURE UtvalgRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE bOK AS LOGICAL     NO-UNDO.
  IF hBuffer:BUFFER-FIELD('TelleType'):BUFFER-VALUE = "1" THEN 
      MESSAGE "Ved � velge Full varetelling (Ja), bygges komplett telleliste. " SKIP
              "Bygging av full telleliste g�r hurtig. Alternativt kan man bygge " SKIP
              "telleliste p� deler av artikkelregisteret (Nei)." SKIP
              "Ved bygging av telleliste p� deler av artikkelregisteret, benyttes artikkelutvalget." SKIP(1)
              "Bygg fullstendig telleliste (Ja), bruk artikkelutvalget til � bygge telleliste (Nei)."
          VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL UPDATE svar AS LOGICAL.
  IF svar = ? THEN
      RETURN NO-APPLY.
  ELSE IF svar = TRUE THEN DO:
      bOK = DYNAMIC-FUNCTION("RunProc","art_to_telling.p",
                             hBuffer:BUFFER-FIELD('TelleNr'):BUFFER-VALUE + "|ALLE,",
                              ?).
    IF NOT bOK THEN
      DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil i overf�ring til Telling",""). 
      SESSION:SET-WAIT-STATE("general").
  END.
  ELSE DO:
      SESSION:SET-WAIT-STATE("general").
      IF NOT VALID-HANDLE(hUtvalg) THEN DO:
        RUN wtmpartbas.w PERSIST SET hUtvalg.
        DYNAMIC-FUNCTION("setTellingIsMaster" IN hUtvalg,TRUE).
        RUN InitializeObject IN hUtvalg.
    /*     InitUtvalgToTelling(hUtvalg,NO). /* NO: Utvalg er IKKE master */  */
      END.
      DYNAMIC-FUNCTION("InitFromTelling" IN hUtvalg, THIS-PROCEDURE).
  END.
  RUN OppdaterSumRecord.
  SESSION:SET-WAIT-STATE("").
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

  DO ix = 1 TO ihBrowse:NUM-COLUMNS:
    CASE ihBrowse:GET-BROWSE-COLUMN(ix):NAME:
      WHEN 'Parameter1' THEN /*Navn p� kolonnen som representerer telletype*/
        ASSIGN
          hbcTelleType = ihBrowse:GET-BROWSE-COLUMN(ix)
          hbfTelleType = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('TelleType')
        .
      WHEN 'Oppdatert' THEN
        ASSIGN
          hbcTelleType = ihBrowse:GET-BROWSE-COLUMN(ix)
          hbfOppdatert = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('Oppdatert')
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getVareTellingNr C-Win 
FUNCTION getVareTellingNr RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  RETURN hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('tellenr'):BUFFER-VALUE.
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

  DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "searchField,rectButton").
  DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "rectButton").

  DYNAMIC-FUNCTION("setAddMoveX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,
  "btnOppdater,rectButton").
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION InitUtvalgToTelling C-Win 
FUNCTION InitUtvalgToTelling RETURNS LOGICAL
  ( INPUT ihUtvalg         AS HANDLE,
    INPUT ibUtvalgIsMaster AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose: Enable / disable buttons according to who is master
    Notes:  
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  ASSIGN 
    hUtvalg         = ihUtvalg
    bUtvalgIsMaster = ibUtvalgIsMaster
 .                             
 IF NOT bUtvalgIsMaster THEN  DYNAMIC-FUNCTION("setAttribute",hToolbar,"disabledevents","Utvalg").
 DYNAMIC-FUNCTION("RefreshRowids",hBrowse,hBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE).


END.
  
RETURN TRUE.   /* Function return value. */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION UtvalgIsMaster C-Win 
FUNCTION UtvalgIsMaster RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN bUtvalgIsMaster.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

