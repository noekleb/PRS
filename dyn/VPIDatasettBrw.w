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

DEF VAR bOk              AS LOG  NO-UNDO.
DEF VAR ix               AS INT  NO-UNDO.
                          
DEF VAR hToolbar         AS HANDLE NO-UNDO.
DEF VAR hBrowse          AS HANDLE NO-UNDO.
 
DEF VAR hParent          AS HANDLE NO-UNDO.
DEF VAR hParentBrowse    AS HANDLE NO-UNDO.

DEF VAR hChild           AS HANDLE NO-UNDO.

DEF VAR hSearchField     AS HANDLE NO-UNDO.

DEF VAR hbcChkAktiv       AS HANDLE NO-UNDO.
DEF VAR hbfChkAktiv       AS HANDLE NO-UNDO.

DEF VAR hbcChkAktivLev    AS HANDLE NO-UNDO.
DEF VAR hbfChkAktivLev    AS HANDLE NO-UNDO.
DEF VAR bHarVPI           AS LOG NO-UNDO.
DEF VAR bSkjulFilter     AS LOG    NO-UNDO.
DEF VAR bVisEnkelToolbar AS LOG    NO-UNDO.
DEF VAR cBrukerId AS CHAR NO-UNDO.
DEF VAR iBrukerType AS INT NO-UNDO.
DEFINE VARIABLE lFilId AS DECIMAL NO-UNDO.
DEFINE VARIABLE iAntLinjer AS INTEGER NO-UNDO.

DEFINE VARIABLE bVisButikker AS LOG NO-UNDO.

DEFINE VARIABLE rPricat AS CLASS cls.vpi.Pricat NO-UNDO.

DEF VAR iFontWingdings   AS INT    NO-UNDO.
iFontWingdings = DYNAMIC-FUNCTION("setAppFont","Wingdings, size=11 Script=symbol","") NO-ERROR.

{windows.i}

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
&Scoped-Define ENABLED-OBJECTS btnStartUtvalg rectBrowse rectToolBar ~
searchField TG-Filter fraImportDato tilImportDato tbAktivLev fiVPILevNr ~
tbAktiv btnCalFraDato btnCalTilDato 
&Scoped-Define DISPLAYED-OBJECTS TG-Filter fraImportDato tilImportDato ~
tbAktivLev fiVPILevNr tbAktiv 

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LoadRowsToBatchSettings C-Win 
FUNCTION LoadRowsToBatchSettings RETURNS LOGICAL
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
DEFINE BUTTON btnCalFraDato 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 3.8 BY 1.

DEFINE BUTTON btnCalTilDato 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 3.8 BY 1.

DEFINE BUTTON btnStartUtvalg  NO-FOCUS
     LABEL "Start søk" 
     SIZE 13 BY 1.

DEFINE VARIABLE fiVPILevNr AS INTEGER FORMAT ">>>>>>9":U INITIAL 0 
     LABEL "VPILev.nr." 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fraImportDato AS DATE FORMAT "99/99/99":U 
     LABEL "Importert dato" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE tilImportDato AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE rectBrowse
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 180.8 BY 14.05.

DEFINE RECTANGLE rectToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 28 BY .95.

DEFINE RECTANGLE searchField
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 24 BY .95.

DEFINE VARIABLE tbAktiv AS LOGICAL INITIAL NO 
     LABEL "Har VPI" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY .81 NO-UNDO.

DEFINE VARIABLE tbAktivLev AS LOGICAL INITIAL NO 
     LABEL "Kun aktive" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY .81 NO-UNDO.

DEFINE VARIABLE TG-Filter AS LOGICAL INITIAL NO 
     LABEL "Filter" 
     VIEW-AS TOGGLE-BOX
     SIZE 12.2 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnStartUtvalg AT ROW 3.67 COL 169.4 NO-TAB-STOP 
     TG-Filter AT ROW 2.43 COL 169.8 WIDGET-ID 2
     fraImportDato AT ROW 2.67 COL 43.2 COLON-ALIGNED
     tilImportDato AT ROW 2.67 COL 61.2 COLON-ALIGNED NO-LABEL
     tbAktivLev AT ROW 2.76 COL 4
     fiVPILevNr AT ROW 3.67 COL 43.2 COLON-ALIGNED
     tbAktiv AT ROW 4 COL 4
     btnCalFraDato AT ROW 2.71 COL 59.2
     btnCalTilDato AT ROW 2.67 COL 77.2
     rectBrowse AT ROW 6.24 COL 2.2
     rectToolBar AT ROW 1.24 COL 2
     searchField AT ROW 5.19 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 182.2 BY 19.76.


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
         TITLE              = "VPI Leverandører"
         HEIGHT             = 19.76
         WIDTH              = 182.2
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
ON END-ERROR OF C-Win /* VPI Leverandører */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* VPI Leverandører */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalFraDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalFraDato C-Win
ON CHOOSE OF btnCalFraDato IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF fraImportDato
DO:
  RUN Cal.w (fraImportDato:HANDLE).
  IF fraImportDato:MODIFIED AND VALID-HANDLE(hBrowse) THEN RUN InvokeMethod (hBrowse,'OpenQuery').  

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalTilDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalTilDato C-Win
ON CHOOSE OF btnCalTilDato IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF tilImportDato
DO:
  RUN Cal.w (tilImportDato:HANDLE).
  IF tilImportDato:MODIFIED AND VALID-HANDLE(hBrowse) THEN RUN InvokeMethod (hBrowse,'OpenQuery').  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnStartUtvalg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnStartUtvalg C-Win
ON CHOOSE OF btnStartUtvalg IN FRAME DEFAULT-FRAME /* Start søk */
DO:
  DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
  RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fraImportDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fraImportDato C-Win
ON LEAVE OF fraImportDato IN FRAME DEFAULT-FRAME /* Importert dato */
DO:
/*   IF SELF:MODIFIED AND VALID-HANDLE(hBrowse) THEN RUN InvokeMethod (hBrowse,'OpenQuery'). */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbAktiv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbAktiv C-Win
ON VALUE-CHANGED OF tbAktiv IN FRAME DEFAULT-FRAME /* Har VPI */
DO:
  RUN InvokeMethod (hBrowse,'OpenQuery').
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbAktivLev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbAktivLev C-Win
ON VALUE-CHANGED OF tbAktivLev IN FRAME DEFAULT-FRAME /* Kun aktive */
DO:
   RUN InvokeMethod (hBrowse,'OpenQuery').
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-Filter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-Filter C-Win
ON VALUE-CHANGED OF TG-Filter IN FRAME DEFAULT-FRAME /* Filter */
DO:
  RUN HideViewFilter.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tilImportDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tilImportDato C-Win
ON LEAVE OF tilImportDato IN FRAME DEFAULT-FRAME
DO:
/*   IF SELF:MODIFIED AND VALID-HANDLE(hBrowse) THEN RUN InvokeMethod (hBrowse,'OpenQuery'). */
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
  rPricat = NEW cls.vpi.Pricat().

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

{incl/supptrigg.i hBrowse}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE clearFilterRecord C-Win 
PROCEDURE clearFilterRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN 
      fraImportDato:SCREEN-VALUE = '' 
      tilImportDato:SCREEN-VALUE = ''
      tbAktiv:CHECKED            = FALSE
      tbAktivLev:CHECKED         = FALSE
      fraImportDato:MODIFIED     = FALSE 
      tilImportDato:MODIFIED     = FALSE
      tbAktiv:MODIFIED           = FALSE
      tbAktivLev:MODIFIED        = FALSE
    .
  END.
  IF VALID-HANDLE(hBrowse) THEN RUN InvokeMethod (hBrowse,'OpenQuery').  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deleteRecord C-Win 
PROCEDURE deleteRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN VelgButikkRecord.

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

  DO WITH FRAME {&FRAME-NAME}:
    RUN HideViewFilter.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE eksportVpiToPricat C-Win
PROCEDURE eksportVpiToPricatRecord:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cRowIdList AS CHARACTER NO-UNDO.
    DEFINE VARIABLE piEkstVpiLevNr AS INTEGER NO-UNDO.
    DEFINE VARIABLE piAntPoster AS INTEGER NO-UNDO.
    DEFINE VARIABLE pcfilNavn AS CHARACTER NO-UNDO.
    DEFINE VARIABLE pbEksport AS LOG NO-UNDO.
    
    /* En av leverandørene i listen må være markert. */
    DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
      IF hBrowse:FETCH-SELECTED-ROW(ix) THEN
        cRowIdList = cRowIdList + hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('RowIdent1'):BUFFER-VALUE + ','.
    END.
    cRowIdList = RIGHT-TRIM(cRowIdList,',').
    IF cRowIdList = "" THEN DO:
        MESSAGE "Ingen rader er valgt." SKIP
            "Marker den VPI leverandør det skal eksporteres varedata fra før eksportfunksjon startes."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.

    MESSAGE 'Skal data fra VPI leverandør ' + (hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("EkstVPILevNr"):BUFFER-VALUE) + 
            ' eksporteres til pricat fil?'
    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL UPDATE pbEksport.
    IF pbEksport = FALSE THEN 
        RETURN.
        
    piEkstVpiLevNr = INTEGER(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("EkstVPILevNr"):BUFFER-VALUE).

    IF piEkstVpiLevNr >= 1 THEN 
    DO:
        {sww.i}
        rPricat:eksporterVpiLev( 1 ).
        {swn.i}
        ASSIGN 
            pcFilNavn = rPricat:ppcPricatFil
            piAntPoster = rPricat:ppiAntPoster
            .
            
        IF SEARCH(pcFilnavn) <> ? THEN
        DO:
            DEF VAR hInstance AS INT.
        
            RUN ShellExecute{&A} IN hpApi(0,
                                          "open",
                                          "notepad.exe",
                                          SEARCH(pcFilnavn),
                                          "",
                                          1,
                                          OUTPUT hInstance).
        
        END.
        
        MESSAGE "Prikatfil: " pcFilnavn + "." SKIP
                "Antall varelinjer eksportert " + STRING(piAntPoster) + "."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

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
  DISPLAY TG-Filter fraImportDato tilImportDato tbAktivLev fiVPILevNr tbAktiv 
      WITH FRAME DEFAULT-FRAME.
  ENABLE btnStartUtvalg rectBrowse rectToolBar searchField TG-Filter 
         fraImportDato tilImportDato tbAktivLev fiVPILevNr tbAktiv 
         btnCalFraDato btnCalTilDato 
      WITH FRAME DEFAULT-FRAME.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getRIGALFilRecord C-Win 
PROCEDURE getRIGALFilRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR opcFieldList AS CHAR NO-UNDO.
DEF VAR opcLabelList AS CHAR NO-UNDO.
DEF VAR opiReturn    AS INT  NO-UNDO.

DEF VAR iReturn     AS INT    NO-UNDO.
DEF VAR cVareNr     AS CHAR   NO-UNDO.
DEF VAR cDummy      AS CHAR   NO-UNDO.
DEF VAR cButList    AS CHAR   NO-UNDO.
DEF VAR cFileName   AS CHAR   NO-UNDO.
DEF VAR cKatalog    AS CHAR   NO-UNDO.
DEF VAR cFilter     AS CHAR   NO-UNDO.
DEF VAR cRowIdList  AS CHAR   NO-UNDO.
DEF VAR cVPIFilNavn AS CHAR   NO-UNDO.
DEF VAR bAktiv      AS LOG    NO-UNDO.

/* En av leverandørene i listen må være markert. */
DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
  IF hBrowse:FETCH-SELECTED-ROW(ix) THEN
    cRowIdList = cRowIdList + hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('RowIdent1'):BUFFER-VALUE + ','.
END.
cRowIdList = RIGHT-TRIM(cRowIdList,',').
IF cRowIdList = "" THEN DO:
    MESSAGE "Ingen rader er valgt." SKIP
        "Marker den VPI leverandør det skal importeres fra før importfunksjon startes."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN NO-APPLY.
END.

/* Setter opp filmaske m.m. */
ASSIGN
    cKatalog    = DYNAMIC-FUNCTION("getFieldValues","SysPara",
                   "WHERE SysHId = 1 and SysGr = 1 and ParaNr = 52","Parameter1")
    cKatalog    = RIGHT-TRIM(cKatalog,'\')
    cVPIFilNavn = DYNAMIC-FUNCTION("getFieldValues","EkstVPIFil",
                   "WHERE EkstVPILevNr =  '" + 
                   STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("EkstVPILevNr"):BUFFER-VALUE) + 
                   "' and VPIFilNavn begins 'V'","VPIFilNavn")

    cFilter     = cVPIFilNavn + '*.' + 
                  DYNAMIC-FUNCTION("getFieldValues","EkstVPIFil",
                   "WHERE EkstVPILevNr =  '" + 
                   STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("EkstVPILevNr"):BUFFER-VALUE) + 
                   "' and VPIFilNavn begins 'V'","VPIEkst") 
    
    bAktiv      = hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("chkAktivLev"):BUFFER-VALUE
    .

/* Leverandør må være aktivert før det kan importeres filer på den. */
IF bAktiv = FALSE THEN
DO:
    MESSAGE "Valgt VPILeverandør er ikke aktiv." SKIP
            "VPILeverandøren må aktiveres før det kan importeres VPI filer."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN NO-APPLY.
END.

bOk = FALSE.
/* Henter VPI fil fra disk */
SYSTEM-DIALOG GET-FILE cFileName 
              FILTERS "VPI filer: " + cFilter cFilter 
              INITIAL-DIR cKatalog
              RETURN-TO-START-DIR
              MUST-EXIST
              UPDATE bOk.

IF bOk = FALSE THEN
    RETURN NO-APPLY.
ELSE 
    MESSAGE "Skal import av VPI fil starte?" SKIP
            "VPI fil som skal importeres:" cFileName
        VIEW-AS ALERT-BOX INFO BUTTONS YES-NO UPDATE bOk.
IF bOk = FALSE THEN
    RETURN NO-APPLY.

bOk = DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"vpiartbas_importer_pricat_fil.p"
                        ,STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("EkstVPILevNr"):BUFFER-VALUE) + ';' + cFileName).
  
IF NOT bOK THEN
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil ved import av fil: " + cFileName,""). 
ELSE 
    DYNAMIC-FUNCTION("RefreshRowids",hBrowse,TRIM(cRowIdList,",")).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getVPIFilRecord C-Win 
PROCEDURE getVPIFilRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR opcFieldList AS CHAR NO-UNDO.
DEF VAR opcLabelList AS CHAR NO-UNDO.
DEF VAR opiReturn    AS INT  NO-UNDO.

DEF VAR iReturn     AS INT    NO-UNDO.
DEF VAR cVareNr     AS CHAR   NO-UNDO.
DEF VAR cDummy      AS CHAR   NO-UNDO.
DEF VAR cButList    AS CHAR   NO-UNDO.
DEF VAR cFileName   AS CHAR   NO-UNDO.
DEF VAR cKatalog    AS CHAR   NO-UNDO.
DEF VAR cFilter     AS CHAR   NO-UNDO.
DEF VAR cRowIdList  AS CHAR   NO-UNDO.
DEF VAR cVPIFilNavn AS CHAR   NO-UNDO.
DEF VAR bAktiv      AS LOG    NO-UNDO.

/* En av leverandørene i listen må være markert. */
DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
  IF hBrowse:FETCH-SELECTED-ROW(ix) THEN
    cRowIdList = cRowIdList + hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('RowIdent1'):BUFFER-VALUE + ','.
END.
cRowIdList = RIGHT-TRIM(cRowIdList,',').
IF cRowIdList = "" THEN DO:
    MESSAGE "Ingen rader er valgt." SKIP
        "Marker den VPI leverandør det skal importeres fra før importfunksjon startes."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN NO-APPLY.
END.

/* Setter opp filmaske m.m. */
ASSIGN
/*    cKatalog    = DYNAMIC-FUNCTION("getFieldValues","SysPara",                   */
/*                   "WHERE SysHId = 1 and SysGr = 1 and ParaNr = 52","Parameter1")*/

    cKatalog = DYNAMIC-FUNCTION("getFieldValues","EkstVPIFil",
                   "WHERE EkstVPILevNr =  '" + 
                   STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("EkstVPILevNr"):BUFFER-VALUE) + 
                   "' and VPIFilNr = '1'","VPIKatalog")
    cKatalog    = RIGHT-TRIM(cKatalog,'\')

/*    cVPIFilNavn = DYNAMIC-FUNCTION("getFieldValues","EkstVPIFil",                                        */
/*                   "WHERE EkstVPILevNr =  '" +                                                           */
/*                   STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("EkstVPILevNr"):BUFFER-VALUE) +*/
/*                   "' and (VPIFilNavn begins 'PRSPricat' or VPIFilNavn begins 'VPI')","VPIFilNavn")      */
    cVPIFilNavn = DYNAMIC-FUNCTION("getFieldValues","EkstVPIFil",
                   "WHERE EkstVPILevNr =  '" + 
                   STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("EkstVPILevNr"):BUFFER-VALUE) + 
                   "' and VPIFilNr = '1'","VPIFilNavn")
    

/*    cFilter     = cVPIFilNavn + '*.' +                                                                   */
/*                  DYNAMIC-FUNCTION("getFieldValues","EkstVPIFil",                                        */
/*                   "WHERE EkstVPILevNr =  '" +                                                           */
/*                   STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("EkstVPILevNr"):BUFFER-VALUE) +*/
/*                   "' and (VPIFilNavn begins 'PRSPricat' or VPIFilNavn begins 'VPI')","VPIEkst")         */

    cFilter     = cVPIFilNavn + '*.' + 
                  DYNAMIC-FUNCTION("getFieldValues","EkstVPIFil",
                   "WHERE EkstVPILevNr =  '" + 
                   STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("EkstVPILevNr"):BUFFER-VALUE) + 
                   "' and VPIFilNr = '1'","VPIEkst")
    cFilter     = cFilter + (IF cFilter <> '' THEN ',' ELSE '') + cVPIFilNavn + '*.xls'
    cFilter     = cFilter + (IF cFilter <> '' THEN ',' ELSE '') + cVPIFilNavn + '*.csv'

    bAktiv      = hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("chkAktivLev"):BUFFER-VALUE
    .

/* Leverandør må være aktivert før det kan importeres filer på den. */
/*IF bAktiv = FALSE THEN                                                      */
/*DO:                                                                         */
/*    MESSAGE "Valgt VPILeverandør er ikke aktiv." SKIP                       */
/*            "VPILeverandøren må aktiveres før det kan importeres VPI filer."*/
/*        VIEW-AS ALERT-BOX INFO BUTTONS OK.                                  */
/*    RETURN NO-APPLY.                                                        */
/*END.                                                                        */

/* Filmaske er ikke satt opp. */
IF (cVPIFilNavn = ? OR cVPIFilNavn = '') THEN
DO:
    MESSAGE "Det er ikke satt opp VPI fil på valgt leverandør." SKIP
            "VPI filens navn må begynne med bokstavene 'VPI' eller 'PRSPricat'."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN NO-APPLY.
END.

bOk = FALSE.
/* Henter VPI fil fra disk */
SYSTEM-DIALOG GET-FILE cFileName 
              FILTERS "VPI filer: " + cFilter cFilter  
              INITIAL-DIR cKatalog
              RETURN-TO-START-DIR
              MUST-EXIST
              UPDATE bOk.

IF bOk = FALSE THEN
    RETURN NO-APPLY.
ELSE 
    MESSAGE "Skal import av VPI fil starte?" SKIP
            "VPI fil som skal importeres:" cFileName
        VIEW-AS ALERT-BOX INFO BUTTONS YES-NO UPDATE bOk.
IF bOk = FALSE THEN
    RETURN NO-APPLY.

IF cFileName MATCHES '*GS1Pricat*' THEN 
DO:
    lFilId = 0.
    /* Oppretter filhode. */
    DO TRANSACTION:
        FILE-INFO:FILE-NAME = cFileName.
        FIND LAST VPIFilHode NO-LOCK NO-ERROR.
        IF AVAILABLE VPIFilHode
            THEN lFilId = VPIFilHode.FilId + 1.
        ELSE lfilId = 1.
        CREATE VPIFilHode.
        ASSIGN
            VPIFilHode.FilId        = lFilId
            VPIFilHode.FilNavn      = ENTRY(NUM-ENTRIES(cFileName,"\")
                                        ,cFileName,"\")
            VPIFilHode.Katalog      = RIGHT-TRIM(REPLACE(cFileName,VPIFilHode.FilNavn,""),"\")
            VPIFilHode.Dato         = FILE-INFO:FILE-CREATE-DATE
            VPIFilHode.Kl           = STRING(FILE-INFO:FILE-CREATE-TIME,"HH:MM:SS")
            VPIFilHode.Storrelse    = FILE-INFO:FILE-SIZE
            VPIFilHode.VPIFilType   = 1
            VPIFilHode.VPIFilStatus = 7
            VPIFilHode.EkstVPILevNr = hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("EkstVPILevNr"):BUFFER-VALUE
            lFilId                  = VPIFilHode.FilId
        NO-ERROR.
        IF AVAILABLE VPIFilHode THEN
            RELEASE VPIFilHode.
    END. /* TRANSACTION */

    /* Starter import */
    IF lFilId > 0 THEN DO: 
        RUN xGS1v3_4_Til_PRSPricat.p (lFilId, ?, OUTPUT iAntLinjer).
        bOk = TRUE.
        MESSAGE 'VPI fil ' + cFileName + ' importert. ' + CHR(10) + STRING(iAntLinjer) + ' linjer lest.'
        VIEW-AS ALERT-BOX.
    END.
END.
ELSE 
bOk = DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"vpiartbas_importer_pricat_fil.p"
                        ,STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("EkstVPILevNr"):BUFFER-VALUE) + ';' + cFileName).
  
IF NOT bOK THEN
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil ved import av fil: " + cFileName,""). 
ELSE 
    DYNAMIC-FUNCTION("RefreshRowids",hBrowse,TRIM(cRowIdList,",")).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getVPIPakkeRecord C-Win 
PROCEDURE getVPIPakkeRecord :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
DEF VAR opcFieldList AS CHAR NO-UNDO.
DEF VAR opcLabelList AS CHAR NO-UNDO.
DEF VAR opiReturn    AS INT  NO-UNDO.

DEF VAR iReturn     AS INT    NO-UNDO.
DEF VAR cVareNr     AS CHAR   NO-UNDO.
DEF VAR cDummy      AS CHAR   NO-UNDO.
DEF VAR cButList    AS CHAR   NO-UNDO.
DEF VAR cFileName   AS CHAR   NO-UNDO.
DEF VAR cKatalog    AS CHAR   NO-UNDO.
DEF VAR cFilter     AS CHAR   NO-UNDO.
DEF VAR cRowIdList  AS CHAR   NO-UNDO.
DEF VAR cVPIFilNavn AS CHAR   NO-UNDO.
DEF VAR bAktiv      AS LOG    NO-UNDO.

/* En av leverandørene i listen må være markert. */
DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
  IF hBrowse:FETCH-SELECTED-ROW(ix) THEN
    cRowIdList = cRowIdList + hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('RowIdent1'):BUFFER-VALUE + ','.
END.
cRowIdList = RIGHT-TRIM(cRowIdList,',').
IF cRowIdList = "" THEN DO:
    MESSAGE "Ingen rader er valgt." SKIP
        "Marker den VPI leverandør det skal importeres fra før importfunksjon startes."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN NO-APPLY.
END.

/* Setter opp filmaske m.m. */
ASSIGN
    cKatalog    = DYNAMIC-FUNCTION("getFieldValues","SysPara",
                   "WHERE SysHId = 1 and SysGr = 1 and ParaNr = 52","Parameter1")
    cKatalog    = RIGHT-TRIM(cKatalog,'\')
    cVPIFilNavn = DYNAMIC-FUNCTION("getFieldValues","EkstVPIFil",
                   "WHERE EkstVPILevNr =  '887' and VPIFilNavn begins 'VPIPK'","VPIFilNavn")

    cFilter     = cVPIFilNavn + '*.' + 
                  DYNAMIC-FUNCTION("getFieldValues","EkstVPIFil",
                   "WHERE EkstVPILevNr =  '887' and VPIFilNavn begins 'VPIPK'","VPIEkst") 
    
    bAktiv      = hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("chkAktivLev"):BUFFER-VALUE
    .

/* Leverandør må være aktivert før det kan importeres filer på den. */
IF bAktiv = FALSE THEN
DO:
    MESSAGE "Valgt VPILeverandør er ikke aktiv." SKIP
            "VPILeverandøren må aktiveres før det kan importeres VPI Pakkefiler."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN NO-APPLY.
END.

bOk = FALSE.
/* Henter VPI fil fra disk */
SYSTEM-DIALOG GET-FILE cFileName 
              FILTERS "VPI Pakke filer: " + cFilter cFilter 
              INITIAL-DIR cKatalog
              RETURN-TO-START-DIR
              MUST-EXIST
              UPDATE bOk.

IF bOk = FALSE THEN
    RETURN NO-APPLY.
ELSE 
    MESSAGE "Skal import av VPI Pakke fil starte?" SKIP
            "VPI Pakke fil som skal importeres:" cFileName
        VIEW-AS ALERT-BOX INFO BUTTONS YES-NO UPDATE bOk.
IF bOk = FALSE THEN
    RETURN NO-APPLY.

bOk = DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"vpiartbas_importer_pricat_fil.p"
                        ,'887' + ';' + cFileName).
  
IF NOT bOK THEN
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil ved import av fil: " + cFileName,""). 
ELSE 
    DYNAMIC-FUNCTION("RefreshRowids",hBrowse,TRIM(cRowIdList,",")).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HideViewFilter C-Win 
PROCEDURE HideViewFilter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME DEFAULT-FRAME:
  ASSIGN
      tbAktivLev:HIDDEN = TG-Filter:CHECKED
      tbAktiv:HIDDEN = TG-Filter:CHECKED
      fraImportDato:HIDDEN = TG-Filter:CHECKED
      btnCalFraDato:HIDDEN = TG-Filter:CHECKED
      tilImportDato:HIDDEN = TG-Filter:CHECKED
      btnCalTilDato:HIDDEN = TG-Filter:CHECKED
      fiVPILevNr:HIDDEN = TG-Filter:CHECKED
      btnStartUtvalg:HIDDEN = TG-Filter:CHECKED
      .
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
DEFINE VARIABLE pcTekst AS CHARACTER NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  cBrukerId   = DYNAMIC-FUNCTION("getASuserId").
  iBrukerType = DYNAMIC-FUNCTION("getFieldValues","Bruker","WHERE BrukerId = '" + cBrukerId + "'","BrukerType").

  pcTekst      = DYNAMIC-FUNCTION("getFieldValues","SysPara","WHERE SysHId = 50 and SysGr =  25 and ParaNr = 1","Parameter1").
  IF CAN-DO('1,Ja,True,Yes,J,Y',pcTekst) THEN 
      bSkjulFilter = TRUE.
  pcTekst      = DYNAMIC-FUNCTION("getFieldValues","SysPara","WHERE SysHId = 50 and SysGr =  25 and ParaNr = 3","Parameter1").
  IF CAN-DO('1,Ja,True,Yes,J,Y',pcTekst) THEN 
      bVisEnkelToolbar = TRUE.
  
  pcTekst = DYNAMIC-FUNCTION("getFieldValues","SysPara",
                             "WHERE SysHId = 50 and SysGr = 25 and ParaNr = 4","Parameter1").
  bHarVPI = IF CAN-DO('1,j,ja,true,y,yes',pcTekst) THEN TRUE ELSE FALSE.

  pcTekst = DYNAMIC-FUNCTION("getFieldValues","SysPara",
                             "WHERE SysHId = 50 and SysGr = 15 and ParaNr = 8","Parameter1").
  bVisButikker = IF CAN-DO('1,j,ja,true,y,yes',pcTekst) THEN TRUE ELSE FALSE.
  
  IF iBrukerType <= 1 THEN
      ASSIGN
      bSkjulFilter     = FALSE
      bVisEnkelToolbar = FALSE
      bHarVPI          = FALSE

      .

  DYNAMIC-FUNCTION("NewObject",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE,"procedure").
  hBrowse = DYNAMIC-FUNCTION("NewBrowse",
                              rectBrowse:HANDLE,
                              100,
                              "", /*!FIT-LAST-COLUMN|!EXPANDABLE*/
                              "vpidatasett"
                             + ";!FilId"
                             + ";EkstVPILevNr"
                             + ";ImportDato"
                             + ";+ImportKL|CHARACTER|x(5)|jb_hhmm(ImportKL)"
                             + ";Beskrivelse|Beskrivelse|x(200)"
                             + ";!DatasettStatus;!AntallArtikler;!AntallKoblet;!BrukerId"
                             + ";+chkAktivLev|logical|yes/no|chkAktivLev(ROWID)|Akt"
                             + ";+chkAktiv|logical|y/n|chkAktiv(ROWID)|VPI"
/*                              + ";OppdatertDato;+OppdatertKL|CHARACTER|x(5)|jb_hhmm(OppdatertTid)" */
                             + ",EkstVPILev;!LevNr;!EkstVPILevNr;KortNavn@2;!AktivLev|Aktiv"
                             + ",LevBas;LevNr;levnamn"
                             ,"WHERE FALSE "
                             + ", FIRST EkstVPILev OUTER-JOIN WHERE EkstVPILev.EkstVPILevNr = VPIDatasett.EkstVPILevNr NO-LOCK "
                             + ", FIRST LevBas OUTER-JOIN WHERE LevBas.LevNr = EkstVPILev.LevNr NO-LOCK "
                             ,"").
  
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"querysort","ImportDato ").
  IF bVisButikker THEN 
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"basequery","WHERE EkstVPILevNr lt 9999999 ").
  ELSE 
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"basequery","WHERE EkstVPILevNr lt 99999 ").
  
  DYNAMIC-FUNCTION("createObjectLink",hBrowse,THIS-PROCEDURE).
  DYNAMIC-FUNCTION('setAttribute',hBrowse,'calcfieldproc','vpidatasett_brwcalc.p').

/*   DYNAMIC-FUNCTION("setAttribute",hBrowse,"flatviewjointype","outer-join"). */

  hSearchField = DYNAMIC-FUNCTION("NewBrowseSearchField",SearchField:HANDLE,hBrowse,1).
  DYNAMIC-FUNCTION("CreateObjectLink",hSearchField,hBrowse).
  
  IF bVisEnkelToolbar THEN 
      hToolbar = DYNAMIC-FUNCTION("NewToolBar",
                   rectToolBar:HANDLE,             /* Rectangle to define coordinates for toolbar */
                   "Fil",                         /* Corresponding menu label - no menu if blank */
                   "excel;Eksporter til E&xcel"
                    + ",clearFilter;Blank &filter¤ENABLE"
                   ,"maxborder").                  /* Misc - enable, maxborder.. */
  ELSE hToolbar = DYNAMIC-FUNCTION("NewToolBar",
                    rectToolBar:HANDLE,             /* Rectangle to define coordinates for toolbar */
                    "Fil",                         /* Corresponding menu label - no menu if blank */
                    "excel;Eksporter til E&xcel¤enable,BrowseConfig"
                     + ",oppdVPIFilListe;&Oppd. liste¤enable"
                     + ",getVPIFil;&Hent VPI fil¤enable" 
                     + ",getRIGALFil;&Hent RIGAL fil¤enable"
                     + ",getVPIPakke;&Hent VPI Pakkefil¤enable"
                     + ",clearFilter;Blank &filter¤ENABLE"
                     + ",RowsToBatch;Antall rader i resultatsett¤enable"
                     + ",eksportVpiToPricat;&Eksporter til Pricat¤enable" 

                    ,"maxborder").                  /* Misc - enable, maxborder.. */
  LoadRowsToBatchSettings().

  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hToolbar).

  ASSIGN 
      TG-Filter:CHECKED IN FRAME DEFAULT-FRAME = bSkjulFilter
      tbAktiv:CHECKED IN FRAME DEFAULT-FRAME   = bHarVPI.



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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenQuery C-Win 
PROCEDURE OpenQuery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cWhere AS CHAR NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcparamChkAktiv",STRING(tbAktiv:CHECKED)).
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcparamChkAktivLev",STRING(tbAktivLev:CHECKED)).

    cWhere = buildFilter(cWhere,fraImportDato:HANDLE,'ImportDato','GE').      
    cWhere = cWhere + buildFilter(cWhere,tilImportDato:HANDLE,'ImportDato','LE').      
    cWhere = cWhere + buildFilter(cWhere,fiVPILevNr:HANDLE,'EkstVPILevNr','EQ').      
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"QueryFilter",cWhere).

    ASSIGN 
      fraImportDato:MODIFIED     = FALSE 
      tilImportDato:MODIFIED     = FALSE
      tbAktiv:MODIFIED           = FALSE
      tbAktivLev:MODIFIED        = FALSE
      fiVPILevNr:MODIFIED        = FALSE 
    .

  END.
  
  RUN SUPER.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdVPIFilListeRecord C-Win 
PROCEDURE oppdVPIFilListeRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR opcFieldList AS CHAR NO-UNDO.
DEF VAR opcLabelList AS CHAR NO-UNDO.
DEF VAR opiReturn    AS INT  NO-UNDO.

DEF VAR iReturn    AS INT    NO-UNDO.
DEF VAR cVareNr    AS CHAR   NO-UNDO.
DEF VAR cDummy     AS CHAR   NO-UNDO.
DEF VAR cButList   AS CHAR   NO-UNDO.
DEF VAR cFileName  AS CHAR   NO-UNDO.
DEF VAR cKatalog   AS CHAR   NO-UNDO.
DEF VAR cFilter    AS CHAR   NO-UNDO.
DEF VAR cRowIdList AS CHAR   NO-UNDO.

bOk = DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"vpiartbas_oppdater_vpidatasett_liste.p"
                      ,STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("EkstVPILevNr"):BUFFER-VALUE)).

IF NOT bOK THEN
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil ved oppdatering av liste ",""). 
ELSE DO:
      DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Oppdatering av liste ","").
      RUN InvokeMethod (hBrowse,'OpenQuery').
  END.

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
  IF VALID-HANDLE(hbcChkAktiv) THEN
      ASSIGN 
        hbcChkAktiv:FONT      = iFontWingdings
        hbcChkAktiv:FORMAT    = CHR(254) + "/"  + CHR(168)
        .
  IF VALID-HANDLE(hbcChkAktivLev) THEN
      ASSIGN 
        hbcChkAktivLev:FONT      = iFontWingdings
        hbcChkAktivLev:FORMAT    = CHR(254) + "/"  + CHR(168)
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RowsToBatchRecord C-Win 
PROCEDURE RowsToBatchRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cRowsToBatch AS CHAR NO-UNDO.

RUN JBoxDSelectRowsToBatch.w (DYNAMIC-FUNCTION("getAttribute",hBrowse,"rowsToBatch"), OUTPUT cRowsToBatch).

IF cRowsToBatch NE "" THEN 
DO:
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"rowsToBatch",cRowsToBatch).
  DYNAMIC-FUNCTION("setCustomWinSettings",THIS-PROCEDURE:CURRENT-WINDOW,"<StartRowsToBatch>|brwLinje|" + cRowsToBatch + "|<EndRowsToBatch>").
END.
RUN InvokeMethod (hBrowse,'OpenQuery').
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setEkstVPILev C-Win 
PROCEDURE setEkstVPILev :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER piEkstVPILevNr AS INT NO-UNDO.

DO WITH FRAME Default-Frame:
  ASSIGN
      fiVPILevNr:SCREEN-VALUE = STRING(piEkstVPILevNr)
      .
  DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
  RUN OpenQuery.

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
/* DEF INPUT PARAM icField AS CHAR NO-UNDO. */
/* DO WITH FRAME {&FRAME-NAME}:             */
/*   CASE icField:                          */
/*     WHEN '' THEN                         */
/*     DO:                                  */
/*     END.                                 */
/*   END CASE.                              */
/* END.                                     */
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
   ihBrowse:MOVE-COLUMN(8,3).          
   ihBrowse:MOVE-COLUMN(9,4).         
   ihBrowse:MOVE-COLUMN(8,5).          
   ihBrowse:MOVE-COLUMN(9,6).
  
  ASSIGN
    ihBrowse:GET-BROWSE-COLUMN(1):WIDTH-PIXELS = 50
    ihBrowse:GET-BROWSE-COLUMN(5):WIDTH-PIXELS = 20
    ihBrowse:GET-BROWSE-COLUMN(6):WIDTH-PIXELS = 20
    ihBrowse:GET-BROWSE-COLUMN(9):WIDTH-PIXELS = 200 /* 8 */
    hbcChkAktivLev = ihBrowse:GET-BROWSE-COLUMN(5)                                   /* Ny */
    hbfChkAktivLev = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('ChkAktivLev') /* Ny */
    hbcChkAktiv    = ihBrowse:GET-BROWSE-COLUMN(6)  /*5*/
    hbfChkAktiv    = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('ChkAktiv')
  .

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LoadRowsToBatchSettings C-Win 
FUNCTION LoadRowsToBatchSettings RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR iy           AS INT NO-UNDO.
DEF VAR cWinSettings AS CHAR NO-UNDO.
  
  cWinSettings = DYNAMIC-FUNCTION("getCustomWinSettings",THIS-PROCEDURE:CURRENT-WINDOW).
  
  DO ix = 1 TO NUM-ENTRIES(cWinSettings,"|"):
    IF ENTRY(ix,cWinSettings,"|") = "<StartRowsToBatch>" THEN
      DO iy = ix + 1 TO NUM-ENTRIES(cWinSettings,"|") BY 2:
        IF ENTRY(iy,cWinSettings,"|") = "<EndRowsToBatch>" THEN LEAVE.
        ELSE DYNAMIC-FUNCTION("setAttribute",hBrowse,"rowsToBatch",ENTRY(iy + 1,cWinSettings,"|")).
      END.
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

