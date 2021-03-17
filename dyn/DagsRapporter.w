&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*********************************************************************
* Copyright (C) 2001 by Progress Software Corporation ("PSC"),       *
* 14 Oak Park, Bedford, MA 01730, and other contributors as listed   *
* below.  All Rights Reserved.                                       *
*                                                                    *
* The Initial Developer of the Original Code is PSC.  The Original   *
* Code is Progress IDE code released to open source December 1, 2000.*
*                                                                    *
* The contents of this file are subject to the Possenet Public       *
* License Version 1.0 (the "License"); you may not use this file     *
* except in compliance with the License.  A copy of the License is   *
* available as of the date of this notice at                         *
* http://www.possenet.org/license.html                               *
*                                                                    *
* Software distributed under the License is distributed on an "AS IS"*
* basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. You*
* should refer to the License for the specific language governing    *
* rights and limitations under the License.                          *
*                                                                    *
* Contributors:                                                      *
*                                                                    *
*********************************************************************/
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

DEFINE VARIABLE bOK             AS LOGICAL             NO-UNDO.
DEFINE VARIABLE ix              AS INTEGER             NO-UNDO.

DEFINE VARIABLE hToolbar        AS HANDLE              NO-UNDO.
DEFINE VARIABLE hBrowse         AS HANDLE              NO-UNDO.
DEFINE VARIABLE hFieldMap       AS HANDLE              NO-UNDO.
DEFINE VARIABLE hViewRecCnt     AS HANDLE              NO-UNDO.
DEFINE VARIABLE hBuffer         AS HANDLE              NO-UNDO.

DEFINE VARIABLE iBrukerType     AS INTEGER             NO-UNDO.
DEFINE VARIABLE cFilNavn        AS CHARACTER           NO-UNDO.

/* To variabler for å håndtere knappepanelet. */
DEFINE VARIABLE hBtnPanel       AS HANDLE              NO-UNDO.
DEFINE VARIABLE hPanelFrame     AS HANDLE              NO-UNDO.

DEFINE VARIABLE cCL             AS CHAR                NO-UNDO.

DEFINE VARIABLE cStandard       AS CHAR                NO-UNDO.
DEFINE VARIABLE cMaxSidor       AS CHAR INIT "1,2,3,4" NO-UNDO.
DEFINE VARIABLE lKasserer       AS LOGICAL             NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnfraDato rectBrowse rectToolBar ~
btnfraDato-2 rectWinToolbar BtnPanel RECT-1 RECT-2 btnfraDato-3 RECT-3 ~
cmbButikk btnfraDato-4 fiFraDato2 fiTilDato2 btnBlank T-PrKasserer fiButNr ~
fiFraDato fiTilDato 
&Scoped-Define DISPLAYED-OBJECTS cmbButikk fiFraDato2 fiTilDato2 ~
T-PrKasserer fiButNr fiFraDato fiTilDato 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnBlank 
     LABEL "<Blank>" 
     SIZE 10 BY 1.

DEFINE BUTTON btnfraDato 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 3.8 BY 1.

DEFINE BUTTON btnfraDato-2 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 3.8 BY 1.

DEFINE BUTTON btnfraDato-3 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 3.8 BY 1.

DEFINE BUTTON btnfraDato-4 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 3.8 BY 1.

DEFINE VARIABLE cmbButikk AS CHARACTER FORMAT "X(256)":U INITIAL "0" 
     LABEL "Butikk" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 41.4 BY 1 NO-UNDO.

DEFINE VARIABLE fiButNr AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     LABEL "Butikk" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE fiFraDato AS DATE FORMAT "99/99/99":U 
     LABEL "Fra dato" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE fiFraDato2 AS DATE FORMAT "99/99/99":U 
     LABEL "Fra dato" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE fiTilDato AS DATE FORMAT "99/99/99":U 
     LABEL "Til dato" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE fiTilDato2 AS DATE FORMAT "99/99/99":U 
     LABEL "Til dato" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE RECTANGLE BtnPanel
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 39 BY 13.95.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 41.2 BY 3.33.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 41.2 BY 20.48.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 141.8 BY 1.33.

DEFINE RECTANGLE rectBrowse
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 100.8 BY 23.81.

DEFINE RECTANGLE rectToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 28 BY .95.

DEFINE RECTANGLE rectWinToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 9.8 BY .91.

DEFINE VARIABLE T-PrKasserer AS LOGICAL INITIAL NO 
     LABEL "Samlingsrapport per kassör" 
     VIEW-AS TOGGLE-BOX
     SIZE 60 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnfraDato AT ROW 1.24 COL 90.2 NO-TAB-STOP 
     btnfraDato-2 AT ROW 1.24 COL 126.8 NO-TAB-STOP 
     btnfraDato-3 AT ROW 4.67 COL 137.6 NO-TAB-STOP 
     cmbButikk AT ROW 1.24 COL 5.8
     btnfraDato-4 AT ROW 5.76 COL 137.6 NO-TAB-STOP 
     fiFraDato2 AT ROW 1.24 COL 68.2 COLON-ALIGNED
     fiTilDato2 AT ROW 1.24 COL 105.8 COLON-ALIGNED
     btnBlank AT ROW 1.24 COL 132.2
     T-PrKasserer AT ROW 2.67 COL 69
     fiButNr AT ROW 3.76 COL 116.2 COLON-ALIGNED
     fiFraDato AT ROW 4.76 COL 116.2 COLON-ALIGNED
     fiTilDato AT ROW 5.76 COL 116.2 COLON-ALIGNED
     rectBrowse AT ROW 3.62 COL 1.2
     rectToolBar AT ROW 2.52 COL 1.8
     rectWinToolbar AT ROW 2.52 COL 132.4
     BtnPanel AT ROW 7.05 COL 103.2
     RECT-1 AT ROW 3.62 COL 102
     RECT-2 AT ROW 6.95 COL 102
     RECT-3 AT ROW 1.05 COL 1.2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 142.6 BY 26.62.


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
         TITLE              = "Salgsenhets register"
         HEIGHT             = 26.52
         WIDTH              = 142.8
         MAX-HEIGHT         = 57.14
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 57.14
         VIRTUAL-WIDTH      = 320
         RESIZE             = YES
         SCROLL-BARS        = NO
         STATUS-AREA        = NO
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
   FRAME-NAME                                                           */
/* SETTINGS FOR COMBO-BOX cmbButikk IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = YES.

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
ON END-ERROR OF C-Win /* Salgsenhets register */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Salgsenhets register */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* Salgsenhets register */
DO:
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"Resize","").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnBlank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnBlank C-Win
ON CHOOSE OF btnBlank IN FRAME DEFAULT-FRAME /* <Blank> */
DO:
  DO WITH FRAME Default-Frame:
      ASSIGN
          fiFraDato2:SCREEN-VALUE = ?
          fiTilDato2:SCREEN-VALUE = ?
          cmbButikk:SCREEN-VALUE  = (IF cmbButikk:SENSITIVE THEN ' ' ELSE cmbButikk:SCREEN-VALUE)
          .
      RUN InvokeMethod(hBrowse,"OpenQuery").  
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnfraDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnfraDato C-Win
ON CHOOSE OF btnfraDato IN FRAME DEFAULT-FRAME /* ... */
DO:

  RUN Cal.w (fiFraDato2:HANDLE).
  RUN InvokeMethod(hBrowse,"OpenQuery").  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnfraDato-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnfraDato-2 C-Win
ON CHOOSE OF btnfraDato-2 IN FRAME DEFAULT-FRAME /* ... */
DO:

  RUN Cal.w (fiTilDato2:HANDLE).
  RUN InvokeMethod(hBrowse,"OpenQuery").  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnfraDato-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnfraDato-3 C-Win
ON CHOOSE OF btnfraDato-3 IN FRAME DEFAULT-FRAME /* ... */
DO:

  RUN Cal.w (fiFraDato:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnfraDato-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnfraDato-4 C-Win
ON CHOOSE OF btnfraDato-4 IN FRAME DEFAULT-FRAME /* ... */
DO:

  RUN Cal.w (fiTilDato:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmbButikk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbButikk C-Win
ON VALUE-CHANGED OF cmbButikk IN FRAME DEFAULT-FRAME /* Butikk */
DO:
  IF INT(cmbButikk:SCREEN-VALUE) > 0 THEN
    fiButNr:SCREEN-VALUE = cmbButikk:SCREEN-VALUE.
  ELSE
    fiButNr:SCREEN-VALUE = '0'.

  ASSIGN
      fiButNr:SENSITIVE = fiButNr:SCREEN-VALUE = '0'.

  RUN InvokeMethod(hBrowse,"OpenQuery").  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiFraDato2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiFraDato2 C-Win
ON TAB OF fiFraDato2 IN FRAME DEFAULT-FRAME /* Fra dato */
OR RETURN OF fiFraDato2
DO:
    RUN InvokeMethod(hBrowse,"OpenQuery").  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiTilDato2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiTilDato2 C-Win
ON TAB OF fiTilDato2 IN FRAME DEFAULT-FRAME /* Til dato */
OR RETURN OF fiTilDato2
DO:
    RUN InvokeMethod(hBrowse,"OpenQuery").  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-PrKasserer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-PrKasserer C-Win
ON VALUE-CHANGED OF T-PrKasserer IN FRAME DEFAULT-FRAME /* Samlingsrapport per kassör */
DO:
  IF T-PrKasserer:CHECKED = YES THEN
      ASSIGN lKasserer = TRUE.
   ELSE
      ASSIGN lKasserer = FALSE.
/*  MESSAGE 'ghjg' T-PrKasserer:CHECKED
      VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

DYNAMIC-FUNCTION("EmbedMe" IN SOURCE-PROCEDURE,THIS-PROCEDURE) NO-ERROR.
/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
  IF NOT DYNAMIC-FUNCTION("DoCleanUpObjects",THIS-PROCEDURE:CURRENT-WINDOW) THEN RETURN NO-APPLY.
  DYNAMIC-FUNCTION("setCleanUpResize",THIS-PROCEDURE:CURRENT-WINDOW).
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
    DEF VAR cDummyType AS CHAR NO-UNDO.
    RUN InitStringTranslation (THIS-PROCEDURE:CURRENT-WINDOW,INPUT-OUTPUT cDummyType).
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

  DO WITH FRAME Default-Frame:
      IF hBuffer:AVAILABLE THEN
      DO:
        ASSIGN
          fiFraDato:SCREEN-VALUE = STRING(hBuffer:BUFFER-FIELD("OmsetningsDato"):BUFFER-VALUE)
          fiTilDato:SCREEN-VALUE = STRING(hBuffer:BUFFER-FIELD("OmsetningsDato"):BUFFER-VALUE)
          fiButNr:SCREEN-VALUE   = STRING(hBuffer:BUFFER-FIELD("ButikkNr"):BUFFER-VALUE)
          .
      END.
      ELSE
          ASSIGN
            fiFraDato:SCREEN-VALUE = ?
            fiTilDato:SCREEN-VALUE = ?
            fiButNr:SCREEN-VALUE   = '0'
            .
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
  DISPLAY cmbButikk fiFraDato2 fiTilDato2 T-PrKasserer fiButNr fiFraDato 
          fiTilDato 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE btnfraDato rectBrowse rectToolBar btnfraDato-2 rectWinToolbar BtnPanel 
         RECT-1 RECT-2 btnfraDato-3 RECT-3 cmbButikk btnfraDato-4 fiFraDato2 
         fiTilDato2 btnBlank T-PrKasserer fiButNr fiFraDato fiTilDato 
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
  Notes:      InitializeObject is called automatically when using JBoxDynMenu.w 
------------------------------------------------------------------------------*/
DEFINE VARIABLE cButNr AS CHARACTER   NO-UNDO.
DEF VAR iBrGrpNr     AS INT         NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  SUBSCRIBE TO "InitStringTranslation" ANYWHERE.

  DYNAMIC-FUNCTION("CreateStatusBar",THIS-PROCEDURE:CURRENT-WINDOW,"",0,YES,?).
  hViewRecCnt = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",THIS-PROCEDURE:CURRENT-WINDOW,"RecordCountWidget")) NO-ERROR.
  iBrukerType = int(DYNAMIC-FUNCTION("getFieldValues","bruker","WHERE brukerid = '" + DYNAMIC-FUNCTION("getASuserId") + "'","BrukerType")).

  cCL = DYNAMIC-FUNCTION("getFieldValues","SysPara",
                         "WHERE SysHId = 5 and SysGr = 1 and ParaNr = 1","Parameter1").
  cButNr = DYNAMIC-FUNCTION("getFieldValues","bruker","WHERE brukerid = '" + DYNAMIC-FUNCTION("getASuserId") + "'","ButikkNr").
  iBrGrpNr = DYNAMIC-FUNCTION("getFieldValues","Bruker","WHERE BrukerId = '" + DYNAMIC-FUNCTION("getASuserId") + "'","BrGrpNr").

  ASSIGN cmbButikk:DELIMITER = "|".
  /*
  IF iBrukerType = 2 THEN
      ASSIGN
         cmbButikk:LIST-ITEM-PAIRS = (IF iBrukertype <> 2 THEN '||' ELSE '') +
                                     DYNAMIC-FUNCTION("getFieldList","Butiker;butik|butnamn;butik",
                                                          "WHERE " + (IF cButNr = cCL THEN "true" ELSE "butik = '" + cButNr + "'") + " BY butik").
  ELSE */
      ASSIGN
         cmbButikk:LIST-ITEM-PAIRS = RIGHT-TRIM(DYNAMIC-FUNCTION("getFieldList","Butikktilgang,Butiker;ButNamn|Butik;Butik"
                                                ,"WHERE BrGrpnr = " + STRING(iBrGrpNr) + ", FIRST Butiker where Butiker.Butik = ButikkTilgang.Butik"),"|").

  ASSIGN
     cmbButikk:SCREEN-VALUE = ENTRY(2,cmbButikk:LIST-ITEM-PAIRS,'|') 
     cmbButikk:SENSITIVE    = IF (NUM-ENTRIES(cmbButikk:LIST-ITEM-PAIRS,'|') > 2 OR (cButNr = cCL)) 
                                THEN TRUE
                                ELSE FALSE
     cmbButikk:BGCOLOR      = 15
     .
  IF iBrukerType = 2 THEN DO:
      ASSIGN
      fiButNr:SCREEN-VALUE = cButNr
      fiButNr:SENSITIVE    = FALSE
      .
      IF cButNr <> "0" THEN DO:
          FIND Syspara WHERE SysPara.SysHId = 210 AND
                             SysPara.SysGr  = 270 AND 
                             SysPara.ParaNr = INT(fiButNr:SCREEN-VALUE) NO-LOCK NO-ERROR.
          IF AVAIL Syspara THEN
              cStandard = TRIM(SysPara.Parameter1).
          IF NOT AVAIL Syspara OR cStandard = "" THEN
              cStandard = cMaxSidor.
      END.
      ELSE
          cStandard = cMaxSidor.
  END.
  ELSE
      ASSIGN
          fiButNr:SENSITIVE = fiButNr:SCREEN-VALUE = '0'.


  hBrowse = DYNAMIC-FUNCTION("NewBrowse",          /* Create a browse object */
                    rectBrowse:HANDLE,              /* Rectangle to define coordinates for browse */
                    100,                            /* Rows to batch */
                    "",                             /* Browse properties, ie MULTIPLE,NUM-LOCKED-COLUMNS, etc */
                    "BokforingsBilag"
                    + ";OmsetningsDato|Salgsdato"
                    + ";ButikkNr"
                    + ";+DummyFelt|CHAR|x(2)|| "
                   ,"WHERE false"
                    ,"").             /* Initial sort column */

  DYNAMIC-FUNCTION("setSortString",hBrowse,"OmsetningsDato;DESC,ButikkNr").
  
  /* Henter buffer handle for å kunne lese ut verdier fra feltene i bufferet. */
  hBuffer = hBrowse:QUERY:GET-BUFFER-HANDLE(1).

  /*
  hFieldMap = DYNAMIC-FUNCTION("NewFieldMap",      /* A fieldmap object holds extra info for display and input fields (fill-ins) 
                                                       and their corresponding buffer columns return handle equals the buffer handle */
                    hBrowse:QUERY,
                    FRAME {&FRAME-NAME}:HANDLE,     /* Frame for the input/display fields (might not be the same frame as the browse) */
                    "GodkjentAv",   /* Update columns in buffer */
                      "",                           /* Corresponding input fields (fill-in..). blank if equal to update columns */
                    "",        /* Additional buffer and displ.fields - not updateable*/
                      "",                           /* Corresponding fill-ins */
                    "").                            /* other input widgets and lookup buttons for update fill-ins */

  DYNAMIC-FUNCTION("CreateObjectLink",hFieldMap,hBrowse).
  */
  hToolbar = DYNAMIC-FUNCTION("NewToolBar",
                    rectToolBar:HANDLE,             /* Rectangle to define coordinates for toolbar */
                    "File",                         /* Corresponding menu label - no menu if blank */
                    "Excel" + /* Buttons / Menu items: action;label;tooltip;Method;image,action;label.. 
                                                       Any number of properties accepted (one ok - if predef. action) */
                    ",Standard;Standard"
                    ,"maxborder").                  /* Misc - enable, maxborder.. */
  
  /*
  DYNAMIC-FUNCTION("CreateObjectLink",hFieldMap,hToolbar).
  */
  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hToolbar).

  DYNAMIC-FUNCTION("NewToolBar",
                    rectWinToolBar:HANDLE,          /* Rectangle to define coordinates for toolbar */
                    "File",                         /* Corresponding menu label - no menu if blank */
                    "close",
                    "right,enable").                      /* Misc - enable, maxborder.. */


  RUN InvokeMethod(hBrowse,"OpenQuery").
END.

DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "rectToolBar,rect-1,rect-3").
DYNAMIC-FUNCTION("setNoResizex", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "rect-1,rect-2").


DYNAMIC-FUNCTION("setOrgWinSize", THIS-PROCEDURE:CURRENT-WINDOW,200,150,0,250).

DYNAMIC-FUNCTION("initTranslation",THIS-PROCEDURE:CURRENT-WINDOW).

THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = FALSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitStringTranslation C-Win 
PROCEDURE InitStringTranslation :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
DEF INPUT PARAM        ihWindow    AS HANDLE NO-UNDO.
DEF INPUT-OUTPUT PARAM iocTypeList AS CHAR   NO-UNDO.

DEF VAR cRapportAlle        AS CHAR   NO-UNDO.
DEF VAR cRapportSamling     AS CHAR   NO-UNDO.
DEF VAR cRapportBokfBilag   AS CHAR   NO-UNDO.
DEF VAR cRapportSpes        AS CHAR   NO-UNDO.
DEF VAR cRapportKontRapp    AS CHAR   NO-UNDO.
DEF VAR cRapportBongRapp    AS CHAR   NO-UNDO.
DEF VAR cRapportAlleTT      AS CHAR   NO-UNDO.
DEF VAR cRapportSamlingTT   AS CHAR   NO-UNDO.
DEF VAR cRapportBokfBilagTT AS CHAR   NO-UNDO.
DEF VAR cRapportSpesTT      AS CHAR   NO-UNDO.
DEF VAR cRapportKontRappTT  AS CHAR   NO-UNDO.
DEF VAR cRapportBongRappTT  AS CHAR   NO-UNDO.
DEF VAR cRapportSpesTilbBet    AS CHAR   NO-UNDO.
DEF VAR cRapportSpesTilbBetTT  AS CHAR   NO-UNDO.

IF ihWindow NE THIS-PROCEDURE:CURRENT-WINDOW THEN RETURN.

/* For translation: Init special translation type and corresponding values: */
IF NOT CAN-DO(iocTypeList,"BUTTON-RapportAlle") THEN
  iocTypeList = iocTypeList + ",BUTTON-RapportAlle".
IF NOT CAN-DO(iocTypeList,"BUTTON-RapportSamling") THEN
  iocTypeList = iocTypeList + ",BUTTON-RapportSamling".
IF NOT CAN-DO(iocTypeList,"BUTTON-RapportBokfBilag") THEN
  iocTypeList = iocTypeList + ",BUTTON-RapportBokfBilag".
IF NOT CAN-DO(iocTypeList,"BUTTON-RapportSpes") THEN
  iocTypeList = iocTypeList + ",BUTTON-RapportSpes".
IF NOT CAN-DO(iocTypeList,"BUTTON-RapportKontRapp") THEN
  iocTypeList = iocTypeList + ",BUTTON-RapportKontRapp".
IF NOT CAN-DO(iocTypeList,"BUTTON-RapportBongRapp") THEN
  iocTypeList = iocTypeList + ",BUTTON-RapportBongRapp".

IF NOT CAN-DO(iocTypeList,"TOOLTIP-RapportAlle") THEN
  iocTypeList = iocTypeList + ",TOOLTIP-RapportAlle".
IF NOT CAN-DO(iocTypeList,"TOOLTIP-RapportSamling") THEN
  iocTypeList = iocTypeList + ",TOOLTIP-RapportSamling".
IF NOT CAN-DO(iocTypeList,"TOOLTIP-RapportBokfBilag") THEN
  iocTypeList = iocTypeList + ",TOOLTIP-RapportBokfBilag".
IF NOT CAN-DO(iocTypeList,"TOOLTIP-RapportSpes") THEN
  iocTypeList = iocTypeList + ",TOOLTIP-RapportSpes".
IF NOT CAN-DO(iocTypeList,"TOOLTIP-RapportKontRapp") THEN
  iocTypeList = iocTypeList + ",TOOLTIP-RapportKontRapp".
IF NOT CAN-DO(iocTypeList,"TOOLTIP-RapportBongRapp") THEN
  iocTypeList = iocTypeList + ",TOOLTIP-RapportBongRapp".


ASSIGN cRapportAlle        = DYNAMIC-FUNCTION("getStringTranslation",THIS-PROCEDURE:FILE-NAME,"BUTTON-RapportAlle","Alle rapporter")
       cRapportSamling     = DYNAMIC-FUNCTION("getStringTranslation",THIS-PROCEDURE:FILE-NAME,"BUTTON-RapportSamling","Samlingsrapport")
       cRapportBokfBilag   = DYNAMIC-FUNCTION("getStringTranslation",THIS-PROCEDURE:FILE-NAME,"BUTTON-RapportBokfBilag","Bokføringsbilag")
       cRapportSpes        = DYNAMIC-FUNCTION("getStringTranslation",THIS-PROCEDURE:FILE-NAME,"BUTTON-RapportSpes","Spesifikasjoner")
       cRapportKontRapp    = DYNAMIC-FUNCTION("getStringTranslation",THIS-PROCEDURE:FILE-NAME,"BUTTON-RapportKontRapp","Konteringsrapport")
       cRapportBongRapp    = DYNAMIC-FUNCTION("getStringTranslation",THIS-PROCEDURE:FILE-NAME,"BUTTON-RapportBongRapp","Bongrapport")
       cRapportSpesTilbBet = DYNAMIC-FUNCTION("getStringTranslation",THIS-PROCEDURE:FILE-NAME,"BUTTON-RapportSpestilbBetp","Spes. tilbakebetaling")
       
       cRapportAlleTT      = DYNAMIC-FUNCTION("getStringTranslation",THIS-PROCEDURE:FILE-NAME,"TOOLTIP-RapportAlle","Skriver ut alle rapporter samtidig")
       cRapportSamlingTT   = DYNAMIC-FUNCTION("getStringTranslation",THIS-PROCEDURE:FILE-NAME,"TOOLTIP-RapportSamling","Skriver ut samlingsrapport")
       cRapportBokfBilagTT = DYNAMIC-FUNCTION("getStringTranslation",THIS-PROCEDURE:FILE-NAME,"TOOLTIP-RapportBokfBilag","Skriver ut bokføringsbilag")
       cRapportSpesTT      = DYNAMIC-FUNCTION("getStringTranslation",THIS-PROCEDURE:FILE-NAME,"TOOLTIP-RapportSpes","Skriver ut spesifikasjoner")
       cRapportKontRappTT  = DYNAMIC-FUNCTION("getStringTranslation",THIS-PROCEDURE:FILE-NAME,"TOOLTIP-RapportKontRapp","Skriver ut konteringsrapport")
       cRapportBongRappTT  = DYNAMIC-FUNCTION("getStringTranslation",THIS-PROCEDURE:FILE-NAME,"TOOLTIP-RapportBongRapp","Skriver ut bongrapport")
       cRapportBongRappTT  = DYNAMIC-FUNCTION("getStringTranslation",THIS-PROCEDURE:FILE-NAME,"TOOLTIP-RapportSpesTilbBet","Skriver ut spes. tilbakebetaling")
       .

IF NOT VALID-HANDLE(hBtnPanel) THEN 
DO WITH FRAME Default-Frame:
  /* Oppsett av rapportknapper. */
  hBtnPanel = DYNAMIC-FUNCTION("NewPanel"
            ,BtnPanel:HANDLE
            ,"",
             "RapportAlle;" + cRapportAlle + ";" + cRapportAlleTT + ";;bmp\bullet_triangle_blue.bmp"
          + ",RapportSamling;" + cRapportSamling + ";" + cRapportSamlingTT + ";;bmp\bullet_triangle_blue.bmp"
          + ",RapportBokfBilag;" + cRapportBokfBilag + ";" + cRapportBokfBilagTT + ";;bmp\bullet_triangle_blue.bmp" 
          + ",RapportSpes;" + cRapportSpes + ";" + cRapportSpesTT + ";;bmp\bullet_triangle_blue.bmp" 
          + ",RapportKontRapp;" + cRapportKontRapp + ";" + cRapportKontRappTT + ";;bmp\bullet_triangle_blue.bmp" 
          + ",RapportBongRapp;" + cRapportBongRapp + ";" + cRapportBongRappTT + ";;bmp\bullet_triangle_blue.bmp" 
          + ",RapportSpesTilbBet;" + cRapportSpesTilbBet + ";" + cRapportSpesTilbBetTT + ";;bmp\bullet_triangle_blue.bmp" 
            ,0,36     
            ,"").
  DYNAMIC-FUNCTION("CreateObjectLink",hBtnPanel,hBrowse).
  hPanelFrame = DYNAMIC-FUNCTION("getPanelFrame" IN hBtnPanel).
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
{&WINDOW-NAME}:WINDOW-STATE = 3.
{&WINDOW-NAME}:MOVE-TO-TOP().

hPanelFrame:MOVE-TO-TOP().

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
DEF VAR cFilter AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  ASSIGN cmbButikk fiFraDato2 fiTilDato2.

  cFilter = "WHERE true " +
           (IF fiFraDato2 NE ? THEN
            " AND OmsetningsDato GE DATE('" + fiFraDato2:SCREEN-VALUE + "')"
            ELSE "") +
           (IF fiTilDato2 NE ? THEN
            " AND OmsetningsDato LE DATE('" + fiTilDato2:SCREEN-VALUE + "')"
            ELSE "") +
           
           (IF cmbButikk:SCREEN-VALUE NE "" AND cmbButikk:SCREEN-VALUE NE ? THEN
             " AND ButikkNr = " + cmbButikk:SCREEN-VALUE
            ELSE "")
            .
  END.

  DYNAMIC-FUNCTION("SetAttribute",hBrowse,"queryfilter",cFilter).
  DYNAMIC-FUNCTION("SetCurrentObject",hBrowse).

  RUN SUPER.

  APPLY "entry" TO hBrowse.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RapportAlleRecord C-Win 
PROCEDURE RapportAlleRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DO WITH FRAME Default-Frame:
    ASSIGN
        fiButNr fiFraDato fiTilDato
        bOk = FALSE.
    RUN dagsrapp_utskrift.p ("1,2,3,4", fiButNr, fiFraDato, fiTilDato, bOk, OUTPUT cFilnavn).
    IF SEARCH(cfilnavn) <> ? THEN
      RUN browse2pdf\viewxmldialog.w (cFilNavn,"Rapport").    
    RUN skrivbongrap.p (fiButNr,fiFraDato,TRUE,TRUE).    
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RapportBokfBilagRecord C-Win 
PROCEDURE RapportBokfBilagRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DO WITH FRAME Default-Frame:
    ASSIGN
        fiButNr fiFraDato fiTilDato
        bOk = FALSE.
    RUN dagsrapp_utskrift.p ("2", fiButNr, fiFraDato, fiTilDato, bOk, OUTPUT cFilnavn).
    IF SEARCH(cfilnavn) <> ? THEN 
      RUN browse2pdf\viewxmldialog.w (cFilNavn,"Rapport").    
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RapportBongRappRecord C-Win 
PROCEDURE RapportBongRappRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DO WITH FRAME Default-Frame:
    ASSIGN
        fiButNr fiFraDato fiTilDato
        bOk = FALSE.
    RUN skrivbongrap.p (fiButNr,fiFraDato,TRUE,TRUE).    
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RapportKontRappRecord C-Win 
PROCEDURE RapportKontRappRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DO WITH FRAME Default-Frame:
    ASSIGN
        fiButNr fiFraDato fiTilDato
        bOk = FALSE.
    RUN dagsrapp_utskrift.p ("4", fiButNr, fiFraDato, fiTilDato, bOk, OUTPUT cfilnavn).
    IF SEARCH(cfilnavn) <> ? THEN 
      RUN browse2pdf\viewxmldialog.w (cFilNavn,"Rapport").    
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RapportSamlingRecord C-Win 
PROCEDURE RapportSamlingRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE iKasserer AS INTEGER FORMAT ">>>>>9" NO-UNDO.
  
DO WITH FRAME Default-Frame:
    ASSIGN
        fiButNr fiFraDato fiTilDato
        bOk = FALSE.
    
/*  MESSAGE "Ange KassörsNr. 0 för ALLA " SET iKasserer.*/
/*  IF iKasserer <> 0 THEN*/
  IF lKasserer = TRUE THEN
  DO:
/*    FIND FIRST Forsalj WHERE Forsalj.ForsNr = iKasserer NO-LOCK.
    IF NOT AVAILABLE Forsalj THEN
      RUN dagsrapp_utskrift.p ("1", fiButNr, fiFraDato, fiTilDato, bOk, OUTPUT cfilnavn).
    ELSE*/
    ASSIGN iKasserer = 1.
    RUN dagsrapp_utskrift_kasserer.p ("1", fiButNr, iKasserer, fiFraDato, fiTilDato, bOk, OUTPUT cfilnavn).
  END.
  ELSE
    RUN dagsrapp_utskrift.p ("1", fiButNr, fiFraDato, fiTilDato, bOk, OUTPUT cfilnavn).
    IF SEARCH(cfilnavn) <> ? THEN 
      RUN browse2pdf\viewxmldialog.w (cFilNavn,"Rapport").    
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RapportSpesRecord C-Win 
PROCEDURE RapportSpesRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DO WITH FRAME Default-Frame:
    ASSIGN
        fiButNr fiFraDato fiTilDato
        bOk = FALSE.
    RUN dagsrapp_utskrift.p ("3", fiButNr, fiFraDato, fiTilDato, bOk, OUTPUT cfilnavn).
    IF SEARCH(cfilnavn) <> ? THEN 
      RUN browse2pdf\viewxmldialog.w (cFilNavn,"Rapport").    
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME





&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RapportSpesTilbBetRecord C-Win
PROCEDURE RapportSpesTilbBetRecord:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DO WITH FRAME Default-Frame:
    ASSIGN
        fiButNr fiFraDato fiTilDato
        bOk = FALSE.
    RUN dagsrapp_utskrift.p ("6", fiButNr, fiFraDato, fiTilDato, bOk, OUTPUT cfilnavn).
    IF SEARCH(cfilnavn) <> ? THEN 
      RUN browse2pdf\viewxmldialog.w (cFilNavn,"Rapport").    
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
  
  DO WITH FRAME Default-Frame:
      IF hBuffer:AVAILABLE THEN
      DO:
        ASSIGN
          fiFraDato:SCREEN-VALUE = STRING(hBuffer:BUFFER-FIELD("OmsetningsDato"):BUFFER-VALUE)
          fiTilDato:SCREEN-VALUE = STRING(hBuffer:BUFFER-FIELD("OmsetningsDato"):BUFFER-VALUE)
          .
      END.
      ELSE
          ASSIGN
            fiFraDato:SCREEN-VALUE = ?
            fiTilDato:SCREEN-VALUE = ?
            .
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Standardrecord C-Win 
PROCEDURE Standardrecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cKun   AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE iReturnValue AS INTEGER     NO-UNDO.
    IF iBrukerType <> 2 THEN
        RETURN NO-APPLY.
    cKun = cStandard.
    RUN JBoxDynFieldChoose.w (THIS-PROCEDURE:CURRENT-WINDOW,?,cMaxSidor,"ROW",INPUT-OUTPUT cKun,OUTPUT iReturnValue).
    IF iReturnValue = 1 THEN
        cKun = cMaxSidor.
    IF (iReturnvalue = 1 OR iReturnValue = 2) AND cKun <> "" AND cKun <> cStandard THEN DO:
      FIND Syspara WHERE SysPara.SysHId = 210 AND
                         SysPara.SysGr  = 270 AND
                         SysPara.ParaNr = INT(fiButNr:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-ERROR.
      IF NOT AVAIL Syspara THEN DO:
          CREATE SysPara.
          ASSIGN SysPara.SysHId = 210
                 SysPara.SysGr  = 270
                 SysPara.ParaNr = INT(fiButNr:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-ERROR.
          IF ERROR-STATUS:ERROR THEN
              DELETE SysPara.
      END.
      IF AVAIL SysPara THEN DO:
          ASSIGN cStandard = cKun.
          ASSIGN SysPara.Parameter1 = cStandard.
          RELEASE SysPara.
      END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

