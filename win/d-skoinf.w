&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------
  Forfatter:
  Beskrivelse:  
  Parametere:
  Endringer:
------------------------------------------------------------------------*/

CREATE WIDGET-POOL.
{windows.i}
/* ***************************  Definitions  ************************** */

/* Parameter Definisjoner ---                                           */
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
&ELSE
&ENDIF

/* Preprossessor direktiver ---                                         */

/* Buffer og Temp-Table Definisjoner ---                                */

/* Lokale variabler ---                                                 */
DEF VAR retur-verdi AS CHAR INITIAL "AVBRYT" NO-UNDO.

DEFINE VAR m-ja              AS LOGICAL NO-UNDO.
DEFINE VAR m-i               AS INTEGER NO-UNDO.
DEFINE VAR m-x               AS CHARACTER NO-UNDO.
DEFINE VAR m-handle          AS HANDLE NO-UNDO.
DEFINE VAR m-wh              AS WIDGET-HANDLE NO-UNDO.

DEF VAR wVerNr               AS CHAR NO-UNDO.
DEF VAR wVerDato             AS DATE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BUTTON-SkoTexWeb BUTTON-Web RECT-10 RECT-8 ~
RECT-9 Btn_OK BUTTON-VisWin FILL-IN-Versjon 
&Scoped-Define DISPLAYED-OBJECTS FI-Firma FI-SkoTex FI-VerInfo ~
FILL-IN-Versjon 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "Avslutt" 
     SIZE 15 BY 1.1
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-SkoTexWeb  NO-FOCUS
     LABEL "www.polygon.se" 
     SIZE 39 BY 1.19.

DEFINE BUTTON BUTTON-VisWin 
     LABEL "Vis Windows info" 
     SIZE 37 BY 1.14.

DEFINE BUTTON BUTTON-Web  NO-FOCUS
     LABEL "www.polygon.se" 
     SIZE 39 BY 1.19.

DEFINE VARIABLE FI-Firma AS CHARACTER FORMAT "X(256)":U INITIAL "             Polygon Communications AB" 
      VIEW-AS TEXT 
     SIZE 85 BY 1.19
     BGCOLOR 9 FGCOLOR 15 FONT 8 NO-UNDO.

DEFINE VARIABLE FI-SkoTex AS CHARACTER FORMAT "X(256)":U INITIAL "PRS (Polygon Retail Solution)" 
      VIEW-AS TEXT 
     SIZE 74 BY 1.19
     FGCOLOR 9 FONT 8 NO-UNDO.

DEFINE VARIABLE FI-VerInfo AS CHARACTER FORMAT "X(256)":U 
     LABEL "Version" 
      VIEW-AS TEXT 
     SIZE 35 BY .62 NO-UNDO.

DEFINE VARIABLE FILL-IN-Versjon AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 69 BY .62 NO-UNDO.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 89 BY 2.86.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 43 BY 8.57.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 43 BY 8.57.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BUTTON-SkoTexWeb AT ROW 15.52 COL 5
     BUTTON-Web AT ROW 15.52 COL 51
     Btn_OK AT ROW 18.38 COL 3
     BUTTON-VisWin AT ROW 18.38 COL 52
     FI-Firma AT ROW 2.67 COL 3 COLON-ALIGNED NO-LABEL
     FI-SkoTex AT ROW 5.29 COL 8 COLON-ALIGNED NO-LABEL
     FI-VerInfo AT ROW 6.71 COL 37 COLON-ALIGNED
     FILL-IN-Versjon AT ROW 17.43 COL 15 COLON-ALIGNED NO-LABEL
     "+47 41223320" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 11.71 COL 18
          FONT 6
     "Måttbandsvägen 12" VIEW-AS TEXT
          SIZE 36 BY .62 AT ROW 9.81 COL 51
          FONT 6
     "+46 8  85 90 49" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 12.67 COL 64
          FONT 6
     "Polygon Communications AS" VIEW-AS TEXT
          SIZE 37 BY .62 AT ROW 8.86 COL 5
          FONT 6
     "Sverige" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 7.91 COL 51
          FONT 6
     "Telefon:" VIEW-AS TEXT
          SIZE 12 BY .62 AT ROW 11.71 COL 5
          FONT 6
     "Polygon Communications AB" VIEW-AS TEXT
          SIZE 37 BY .62 AT ROW 8.86 COL 51
          FONT 6
     "E-Mail:" VIEW-AS TEXT
          SIZE 13 BY .62 AT ROW 14.1 COL 51
          FONT 6
     "+46 8  85 19 13" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 11.71 COL 64
          FONT 6
     "Fax:" VIEW-AS TEXT
          SIZE 27 BY .62 AT ROW 12.62 COL 51
          FONT 6
     "info@polygon.se" VIEW-AS TEXT
          SIZE 27 BY .62 AT ROW 14.1 COL 64
          FONT 6
     "Telefon:" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 11.71 COL 51
          FONT 6
     "Norge" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 7.91 COL 5
          FONT 6
     "0182 Oslo" VIEW-AS TEXT
          SIZE 36 BY .62 AT ROW 10.76 COL 5
          FONT 6
     "polygon@polygon.se" VIEW-AS TEXT
          SIZE 35 BY .62 AT ROW 14.57 COL 8.8
          FONT 6
     "E-Mail:" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 13.91 COL 5.6
          FONT 6
     "Brenneriveien 11" VIEW-AS TEXT
          SIZE 37 BY .62 AT ROW 9.81 COL 5
          FONT 6
     "187 66 Täby" VIEW-AS TEXT
          SIZE 38 BY .62 AT ROW 10.76 COL 51
          FONT 6
     RECT-10 AT ROW 1.71 COL 3
     RECT-8 AT ROW 8.38 COL 3
     RECT-9 AT ROW 8.38 COL 49
     SPACE(4.39) SKIP(2.99)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Om PRS"
         DEFAULT-BUTTON Btn_OK.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME                                                           */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN FI-Firma IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-SkoTex IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-VerInfo IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Om PRS */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SkoTexWeb
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SkoTexWeb Dialog-Frame
ON CHOOSE OF BUTTON-SkoTexWeb IN FRAME Dialog-Frame /* www.polygon.se */
DO:
  APPLY "ENTRY" TO Btn_Ok.
  RUN OpenWeb(SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-VisWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-VisWin Dialog-Frame
ON CHOOSE OF BUTTON-VisWin IN FRAME Dialog-Frame /* Vis Windows info */
DO:
  RUN shellabout.p.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Web
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Web Dialog-Frame
ON CHOOSE OF BUTTON-Web IN FRAME Dialog-Frame /* www.polygon.se */
DO:
  APPLY "ENTRY" TO Btn_Ok.
  RUN OpenWeb(SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT EQ ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.
ASSIGN FRAME {&FRAME-NAME}:TITLE = "Om PRS".

ON HELP ANYWHERE BELL.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN FILL-IN-Versjon = string(FILL-IN-Versjon:HANDLE) 
                              + " Progress versjon " 
                              + PROVERSION 
                              + "  (" 
                              + PROGRESS 
                              + "), serienr. " 
                              + STRING(_Serial).
     IF BUTTON-Web:LOAD-MOUSE-POINTER("GLOVE") THEN.
     IF BUTTON-WEB:MOVE-TO-BOTTOM() THEN.
  END.          
    
  RUN skoversion.w (OUTPUT wVerNr, OUTPUT wVerDato).
  ASSIGN
    FI-VerInfo = wVerNr + "  " + string(wVerDato). 

  /*{gradient.i &COLOR = "'BLUE'"}*/
  {lng.i} RUN enable_UI.
  
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

&IF DEFINED(UIB_IS_RUNNING) EQ 0 &THEN
 RETURN retur-verdi.
&else
 MESSAGE retur-verdi VIEW-AS ALERT-BOX.
&endif

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
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
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
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
  DISPLAY FI-Firma FI-SkoTex FI-VerInfo FILL-IN-Versjon 
      WITH FRAME Dialog-Frame.
  ENABLE BUTTON-SkoTexWeb BUTTON-Web RECT-10 RECT-8 RECT-9 Btn_OK BUTTON-VisWin 
         FILL-IN-Versjon 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenWeb Dialog-Frame 
PROCEDURE OpenWeb :
/*------------------------------------------------------------------------------
  Purpose:     Åpner webside for SD
  Parameters:  se under
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAMETER document AS CHAR.

  DEF VAR executable AS CHAR.
  DEF VAR hInstance AS INTEGER.
  
  /* find the associated executable in registry */  
  Executable = fill("x", 255). /* =allocate memory */
  RUN FindExecutable{&A} IN hpApi (document,
                                   "",
                                   INPUT-OUTPUT Executable,
                                   OUTPUT hInstance).

  /* if not found, show the OpenAs dialog from the Explorer */
  IF hInstance>=0 AND hInstance<=32 AND hInstance <> 2 THEN 
     RUN ShellExecute{&A} IN hpApi(0,
                                  "open",
                                  "rundll32.exe",
                                  "shell32.dll,OpenAs_RunDLL " + document,
                                  "",
                                  1,
                                  OUTPUT hInstance).

  /* now open the document. If the user canceled the OpenAs dialog,
     this ShellExecute call will silently fail */
  RUN ShellExecute{&A} IN hpApi  (0,
                                  "open",
                                  document,
                                  "",
                                  "",
                                  1,
                                  OUTPUT hInstance).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

