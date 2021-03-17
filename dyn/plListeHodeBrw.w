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

DEFINE VARIABLE bOk                AS LOGICAL   NO-UNDO.
DEFINE VARIABLE bVisButikker       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE hbcChkAktiv        AS HANDLE    NO-UNDO.
DEFINE VARIABLE hbcChkAktivLev     AS HANDLE    NO-UNDO.
DEFINE VARIABLE hbfChkAktiv        AS HANDLE    NO-UNDO.
DEFINE VARIABLE hbfChkAktivLev     AS HANDLE    NO-UNDO.
DEFINE VARIABLE hBrwHode           AS HANDLE    NO-UNDO.
DEFINE VARIABLE hBrwModell         AS HANDLE    NO-UNDO.
DEFINE VARIABLE hBrwArtikkel       AS HANDLE    NO-UNDO.
DEFINE VARIABLE hBrwLinje          AS HANDLE    NO-UNDO.
DEFINE VARIABLE hChild             AS HANDLE    NO-UNDO.
DEFINE VARIABLE hParent            AS HANDLE    NO-UNDO.
DEFINE VARIABLE hParentBrowse      AS HANDLE    NO-UNDO.
DEFINE VARIABLE hSearchFieldHode   AS HANDLE    NO-UNDO.
DEFINE VARIABLE hSearchFieldModell AS HANDLE    NO-UNDO.
DEFINE VARIABLE hToolbar           AS HANDLE    NO-UNDO.
DEFINE VARIABLE ix                 AS INTEGER   NO-UNDO.
DEFINE VARIABLE hgenpllisteordre   AS HANDLE    NO-UNDO.
DEFINE VARIABLE cBrukerId          AS CHAR      NO-UNDO.
DEFINE VARIABLE iBrukerButikk      AS INT       NO-UNDO.
DEFINE VARIABLE hPlListeView       AS HANDLE    NO-UNDO.
DEFINE VARIABLE hBuffer            AS HANDLE    NO-UNDO.
DEFINE VARIABLE iBrukerType        AS INT       NO-UNDO.        

DEF VAR iFontWingdings   AS INT    NO-UNDO.
iFontWingdings = DYNAMIC-FUNCTION("setAppFont","Wingdings, size=11 Script=symbol","") NO-ERROR.

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
&Scoped-Define ENABLED-OBJECTS btnCalFraDato BrwPlListeHode rectToolBar ~
searchFieldHode cbButikk cbOrdreStatus fraSendtDato tilSendtDato ~
btnCalTilDato 
&Scoped-Define DISPLAYED-OBJECTS cbButikk cbOrdreStatus fraSendtDato ~
tilSendtDato 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AdjustBrowseColumns C-Win 
FUNCTION AdjustBrowseColumns RETURNS LOGICAL
  ( INPUT ihBrwHode     AS HANDLE,
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

DEFINE VARIABLE cbButikk AS CHARACTER FORMAT "X(256)":U 
     LABEL "Butikk" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 29 BY 1 TOOLTIP "Filter på butikk" NO-UNDO.

DEFINE VARIABLE cbOrdreStatus AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "Ordrestatus" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "Item 1",0
     DROP-DOWN-LIST
     SIZE 29 BY 1 TOOLTIP "Filter på ordrestatus" NO-UNDO.

DEFINE VARIABLE fraSendtDato AS DATE FORMAT "99/99/99":U 
     LABEL "Sendt dato" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE tilSendtDato AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE BrwPlListeHode
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 187.4 BY 14.05.

DEFINE RECTANGLE rectToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 28 BY .95.

DEFINE RECTANGLE searchFieldHode
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 19 BY .95.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnCalFraDato AT ROW 4.38 COL 154
     cbButikk AT ROW 3.38 COL 90 COLON-ALIGNED
     cbOrdreStatus AT ROW 4.33 COL 90 COLON-ALIGNED
     fraSendtDato AT ROW 4.33 COL 138 COLON-ALIGNED
     tilSendtDato AT ROW 4.33 COL 156 COLON-ALIGNED NO-LABEL
     btnCalTilDato AT ROW 4.33 COL 172
     BrwPlListeHode AT ROW 6.48 COL 1.6
     rectToolBar AT ROW 1.24 COL 2
     searchFieldHode AT ROW 5.29 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 189.2 BY 19.76.


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
         WIDTH              = 189.2
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
OR F10 OF fraSendtDato
DO:
  RUN Cal.w (fraSendtDato:HANDLE).
  IF fraSendtDato:MODIFIED AND VALID-HANDLE(hBrwHode) THEN RUN InvokeMethod (hBrwHode,'OpenQuery').  

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalTilDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalTilDato C-Win
ON CHOOSE OF btnCalTilDato IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF tilSendtDato
DO:
  RUN Cal.w (tilSendtDato:HANDLE).
  IF tilSendtDato:MODIFIED AND VALID-HANDLE(hBrwHode) THEN RUN InvokeMethod (hBrwHode,'OpenQuery').  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbButikk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbButikk C-Win
ON VALUE-CHANGED OF cbButikk IN FRAME DEFAULT-FRAME /* Butikk */
DO:
  DYNAMIC-FUNCTION("setCurrentObject",hBrwHode).
  RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbOrdreStatus
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbOrdreStatus C-Win
ON VALUE-CHANGED OF cbOrdreStatus IN FRAME DEFAULT-FRAME /* Ordrestatus */
DO:
  DYNAMIC-FUNCTION("setCurrentObject",hBrwHode).
  RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fraSendtDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fraSendtDato C-Win
ON LEAVE OF fraSendtDato IN FRAME DEFAULT-FRAME /* Sendt dato */
DO:
/*   IF SELF:MODIFIED AND VALID-HANDLE(hBrwHode) THEN RUN InvokeMethod (hBrwHode,'OpenQuery'). */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fraSendtDato C-Win
ON TAB OF fraSendtDato IN FRAME DEFAULT-FRAME /* Sendt dato */
DO:
  DYNAMIC-FUNCTION("setCurrentObject",hBrwHode).
  RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tilSendtDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tilSendtDato C-Win
ON LEAVE OF tilSendtDato IN FRAME DEFAULT-FRAME
DO:
/*   IF SELF:MODIFIED AND VALID-HANDLE(hBrwHode) THEN RUN InvokeMethod (hBrwHode,'OpenQuery'). */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tilSendtDato C-Win
ON TAB OF tilSendtDato IN FRAME DEFAULT-FRAME
DO:
  DYNAMIC-FUNCTION("setCurrentObject",hBrwHode).
  RUN OpenQuery.
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
  IF VALID-HANDLE(hgenpllisteordre) THEN APPLY "close" TO hgenpllisteordre.
  IF VALID-HANDLE(hPlListeView) THEN APPLY "close" TO hPlListeView.
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

{incl/supptrigg.i hBrwHode}

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
      fraSendtDato:SCREEN-VALUE  = '' 
      tilSendtDato:SCREEN-VALUE  = ''
      cbOrdreStatus:SCREEN-VALUE = ''
      cbButikk:SCREEN-VALUE      = ''
      fraSendtDato:MODIFIED      = FALSE 
      tilSendtDato:MODIFIED      = FALSE
      cbOrdreStatus:MODIFIED     = FALSE 
      cbButikk:MODIFIED          = FALSE 
    .
  END.
  IF VALID-HANDLE(hBrwHode) THEN RUN InvokeMethod (hBrwHode,'OpenQuery').  
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
RUN EditRecord.

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
DEF VAR iReturn     AS INT  NO-UNDO.
DEF VAR ocValue     AS CHAR NO-UNDO.
DEFINE VARIABLE cRowIdList AS CHARACTER NO-UNDO.

RUN JBoxBrowseMsgUpdateVal.w ('Skal ordreforslag(ene) slettes?',
                              hBrwHode:NUM-SELECTED-ROWS,
                              IF INT(DYNAMIC-FUNCTION("getAttribute",hBrwHode,"Totalcount")) LT INT(DYNAMIC-FUNCTION("getAttribute",hBrwHode,"rowstobatch")) THEN
                                INT(DYNAMIC-FUNCTION("getAttribute",hBrwHode,"Totalcount"))
                              ELSE 99999,
                              "",
                              OUTPUT ocValue,
                              OUTPUT iReturn).

IF iReturn = 0 THEN RETURN "cancel".

IF iReturn = 1 THEN
  bOk = DYNAMIC-FUNCTION("ProcessQuery",hBrwHode,"pllistehode_delete.p",STRING(hBrwHode:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("plListeId"):BUFFER-VALUE) + ';' + ocValue).
ELSE IF iReturn = 2 THEN
DO:
  DO ix = 1 TO hBrwHode:NUM-SELECTED-ROWS:
    IF hBrwHode:FETCH-SELECTED-ROW(ix) THEN
      cRowIdList = cRowIdList + hBrwHode:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('RowIdent1'):BUFFER-VALUE + ','.
  END.
/*   bOk = DYNAMIC-FUNCTION("ProcessSelectedRows",hBrwHode,"pllistehode_delete.p",STRING(hBrwHode:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("plListeId"):BUFFER-VALUE) + ';' + ocValue). */
  bOk = DYNAMIC-FUNCTION("ProcessSelectedRows",hBrwHode,"pllistehode_delete.p",'').
END.
ELSE
  LEAVE.
  
IF NOT bOK THEN
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil i sending av informasjon ",""). 

RUN InvokeMethod (hBrwHode,'OpenQuery').
RUN InvokeMethod (hBrwHode,'DisplayRecord').

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
IF hBuffer:AVAIL AND hBuffer:BUFFER-FIELD("OverfortDato"):BUFFER-VALUE NE ? THEN
  DYNAMIC-FUNCTION("setAttribute",hToolbar,"disabledEvents",""). /* new,edit,delete */
ELSE
  DYNAMIC-FUNCTION("setAttribute",hToolbar,"disabledEvents","").  /* new,edit,delete */

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
IF NOT VALID-HANDLE(hPlListeView) THEN DO:
  RUN plListeView.w PERSIST SET hPlListeView.
  RUN InitializeObject IN hPlListeView.
  DYNAMIC-FUNCTION("setParentQuery" IN hPlListeView,hBrwHode).
END.

RUN MoveToTop IN hPlListeView.
RUN InvokeMethod(hBrwHode,"DisplayRecord").
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
  DISPLAY cbButikk cbOrdreStatus fraSendtDato tilSendtDato 
      WITH FRAME DEFAULT-FRAME.
  ENABLE btnCalFraDato BrwPlListeHode rectToolBar searchFieldHode cbButikk 
         cbOrdreStatus fraSendtDato tilSendtDato btnCalTilDato 
      WITH FRAME DEFAULT-FRAME.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GenOrdreforslagRecord C-Win 
PROCEDURE GenOrdreforslagRecord :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/

  IF NOT VALID-HANDLE(hgenpllisteordre) THEN
    RUN genpllisteordre.w PERSISTENT SET hgenpllisteordre.
  IF VALID-HANDLE(hgenpllisteordre) THEN 
    RUN setParentHandle IN hgenpllisteordre (THIS-PROCEDURE).
  
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
DO ix = 1 TO hBrwHode:NUM-SELECTED-ROWS:
  IF hBrwHode:FETCH-SELECTED-ROW(ix) THEN
    cRowIdList = cRowIdList + hBrwHode:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('RowIdent1'):BUFFER-VALUE + ','.
END.
cRowIdList = RIGHT-TRIM(cRowIdList,',').
IF cRowIdList = "" THEN DO:
    MESSAGE "Ingen rader er valgt." SKIP
        "Marker den VPI leverandør det skal importeres på før importfunksjon startes."
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
                   STRING(hBrwHode:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("EkstVPILevNr"):BUFFER-VALUE) + 
                   "' and VPIFilNavn begins 'VPI'","VPIFilNavn")

    cFilter     = cVPIFilNavn + '*.' + 
                  DYNAMIC-FUNCTION("getFieldValues","EkstVPIFil",
                   "WHERE EkstVPILevNr =  '" + 
                   STRING(hBrwHode:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("EkstVPILevNr"):BUFFER-VALUE) + 
                   "' and VPIFilNavn begins 'VPI'","VPIEkst") 
    
    bAktiv      = hBrwHode:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("chkAktivLev"):BUFFER-VALUE
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

bOk = DYNAMIC-FUNCTION("ProcessQuery",hBrwHode,"vpiartbas_importer_pricat_fil.p"
                        ,STRING(hBrwHode:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("EkstVPILevNr"):BUFFER-VALUE) + ';' + cFileName).
  
IF NOT bOK THEN
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil ved import av fil: " + cFileName,""). 
ELSE 
    DYNAMIC-FUNCTION("RefreshRowids",hBrwHode,TRIM(cRowIdList,",")).

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

  iBrukerButikk = INT(DYNAMIC-FUNCTION("getFieldValues","Bruker",
                                       "WHERE Bruker.BrukerId  = '" + userid('SkoTex') + "'","ButikkNr")).
  iBrukerType   = INT(DYNAMIC-FUNCTION("getFieldValues","Bruker",
                                       "WHERE Bruker.BrukerId  = '" + userid('SkoTex') + "'","BrukerType")).

  ASSIGN 
    cbOrdreStatus:DELIMITER = "|"
    cbOrdreStatus:LIST-ITEM-PAIRS = "|0|" + DYNAMIC-FUNCTION("getFieldList","sysPara;ParaNr|Parameter1;ParaNr","where sysHId = 5 and sysGr = 7") 
    cbButikk:DELIMITER = "|"
    cbButikk:LIST-ITEM-PAIRS = "|0|" + DYNAMIC-FUNCTION("getFieldList","Butiker;Butik|ButNamn;Butik","where  ApningsDato > '01/01/1980'") 
  . 

  hBrwHode = DYNAMIC-FUNCTION("NewBrowse",
                              BrwPlListeHode:HANDLE,
                              100,
                              "MULTIPLE", /*!FIT-LAST-COLUMN|!EXPANDABLE*/
                              "PlListeHode"
                             + ";PlListeId|Ordreforsl"
                             + ";LevNr|Lev.nr"
                             + ";FraButikkNr|Butikk"
                             + ";PlNavn|Beskrivelse" 
                             + ";!PlMerknad" 
                             + ";RegistrertDato"
                             + ";!SendtPda|Sendt"
                             + ";OverfortDato|Sendt dato" 
                             + ";!PlLType"
                             + ";!plListeStatus"
                             + ";RegistrertAv| "
                            + ",Butiker"
                             + ";ButNamn@6"
                            + ",LevBas"
                             + ";LevNamn|Lev.navn@3"
                            + ",sysPara;Parameter1|Status|x(15)@4"
                             ,"WHERE FALSE "
                             + ", FIRST Butiker OUTER-JOIN WHERE Butiker.butik = FraButikkNr NO-LOCK "
                             + ", FIRST LevBas  OUTER-JOIN WHERE LevBas.LevNr = LevNr NO-LOCK "
                             + ", FIRST sysPara OUTER-JOIN WHERE sysPara.sysHid = 5 AND sysPara.sysGr = 7 AND sysPara.ParaNr = plListeHode.PlListeStatus NO-LOCK"
                             ,"").
  
  hBuffer = hBrwHode:QUERY:GET-BUFFER-HANDLE(1).

  DYNAMIC-FUNCTION("setSortString",hBrwHode,"PlListeId;desc").
  DYNAMIC-FUNCTION("setAttribute",hBrwHode,"baseQuery","WHERE PlLType = 2").
  
  DYNAMIC-FUNCTION("createObjectLink",hBrwHode,THIS-PROCEDURE).

  hSearchFieldHode = DYNAMIC-FUNCTION("NewBrowseSearchField",SearchFieldHode:HANDLE,hBrwHode,1).
  DYNAMIC-FUNCTION("CreateObjectLink",hSearchFieldHode,hBrwHode).
  
  hToolbar = DYNAMIC-FUNCTION("NewToolBar",
                    rectToolBar:HANDLE,             /* Rectangle to define coordinates for toolbar */
                    "Fil",                         /* Corresponding menu label - no menu if blank */
                    "New;Ny,Edit;Endre,Delete;Slette,rule,excel;Eksporter til E&xcel"
                  + ",BrowseConfig,rule"
                  + ",GenOrdreforslag;&Generer ordreforslag¤Enable"
                    ,"maxborder").                  /* Misc - enable, maxborder.. */

  DYNAMIC-FUNCTION("CreateObjectLink",hBrwHode,hToolbar).

  ASSIGN
    cbOrdreStatus:SCREEN-VALUE IN FRAME Default-Frame = '1'
    cbButikk:SCREEN-VALUE IN FRAME Default-Frame = STRING(iBrukerbutikk)
    .
  IF iBrukerButikk <> ? AND iBrukerbutikk > 0 AND iBrukerType > 1 THEN
  DO:
      ASSIGN 
        cbButikk:SENSITIVE IN FRAME Default-Frame = FALSE.
  END.

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
APPLY "entry" TO hBrwHode.
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
IF NOT VALID-HANDLE(hPlListeView) THEN DO:
  RUN plListeView.w PERSIST SET hPlListeView.
  RUN InitializeObject IN hPlListeView.
  DYNAMIC-FUNCTION("setParentQuery" IN hPlListeView,hBrwHode).
END.

RUN MoveToTop IN hPlListeView.
RUN NewListe IN hPlListeView.

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
    cWhere = buildFilter(cWhere,fraSendtDato:HANDLE,'OverfortDato','GE').      
    cWhere = cWhere + buildFilter(cWhere,tilSendtDato:HANDLE,'OverfortDato','LE').      
    cWhere = cWhere + buildFilter(cWhere,cbOrdreStatus:HANDLE,'plListeStatus','EQ').
    IF INTEGER(cbButikk:SCREEN-VALUE IN FRAME Default-Frame) > 0 THEN 
      cWhere = cWhere + buildFilter(cWhere,cbButikk:HANDLE,'FraButikkNr','EQ').
    
    DYNAMIC-FUNCTION("setAttribute",hBrwHode,"QueryFilter",cWhere).
    
    ASSIGN 
      fraSendtDato:MODIFIED     = FALSE 
      tilSendtDato:MODIFIED     = FALSE
    .

  END.
  
  RUN SUPER.
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
/*   IF VALID-HANDLE(hbcChkAktiv) THEN                            */
/*       ASSIGN                                                   */
/*         hbcChkAktiv:FONT      = iFontWingdings                 */
/*         hbcChkAktiv:FORMAT    = CHR(254) + "/"  + CHR(168)     */
/*         .                                                      */
/*   IF VALID-HANDLE(hbcChkAktivLev) THEN                         */
/*       ASSIGN                                                   */
/*         hbcChkAktivLev:FONT      = iFontWingdings              */
/*         hbcChkAktivLev:FORMAT    = CHR(254) + "/"  + CHR(168)  */
/*         .                                                      */
RUN SUPER.

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

RUN JBoxDSelectRowsToBatch.w (DYNAMIC-FUNCTION("getAttribute",hBrwHode,"rowsToBatch"), OUTPUT cRowsToBatch).

IF cRowsToBatch NE "" THEN 
DO:
  DYNAMIC-FUNCTION("setAttribute",hBrwHode,"rowsToBatch",cRowsToBatch).
  DYNAMIC-FUNCTION("setCustomWinSettings",THIS-PROCEDURE:CURRENT-WINDOW,"<StartRowsToBatch>|brwLinje|" + cRowsToBatch + "|<EndRowsToBatch>").
END.
RUN InvokeMethod (hBrwHode,'OpenQuery').
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE runOpenQueryInBrowse C-Win 
PROCEDURE runOpenQueryInBrowse :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/

  DYNAMIC-FUNCTION("setCurrentObject",hBrwHode).
  RUN OpenQuery.
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
  ( INPUT ihBrwHode     AS HANDLE,
    INPUT icBrowseName AS CHAR):
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
/*  
  ASSIGN
    ihBrwHode:GET-BROWSE-COLUMN(1):WIDTH-PIXELS = 50
    ihBrwHode:GET-BROWSE-COLUMN(5):WIDTH-PIXELS = 20
    ihBrwHode:GET-BROWSE-COLUMN(6):WIDTH-PIXELS = 20
    ihBrwHode:GET-BROWSE-COLUMN(9):WIDTH-PIXELS = 200 /* 8 */
    hbcChkAktivLev = ihBrwHode:GET-BROWSE-COLUMN(5)                                   /* Ny */
    hbfChkAktivLev = ihBrwHode:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('ChkAktivLev') /* Ny */
    hbcChkAktiv    = ihBrwHode:GET-BROWSE-COLUMN(6)  /*5*/
    hbfChkAktiv    = ihBrwHode:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('ChkAktiv')
  .
*/
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

  RETURN hBrwHode.

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
        ELSE DYNAMIC-FUNCTION("setAttribute",hBrwHode,"rowsToBatch",ENTRY(iy + 1,cWinSettings,"|")).
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

