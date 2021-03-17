&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
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

DEFINE VARIABLE iDayDelay AS INTEGER    NO-UNDO.
DEFINE VARIABLE dFirstDate AS DATE       NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BROWSE-1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES SysPara

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 SysPara.ParaNr getButnamn() ~
SysPara.Parameter1 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1 
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH SysPara ~
      WHERE SysPara.SysHId = 210 ~
 AND SysPara.SysGr = 255 NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY BROWSE-1 FOR EACH SysPara ~
      WHERE SysPara.SysHId = 210 ~
 AND SysPara.SysGr = 255 NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 SysPara
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 SysPara


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BROWSE-1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-65 BROWSE-1 FI-Rappdir ~
FI-ButikkNr FI-Navn FI-Dato FI-MuligFraDato FI-MuligTilDato B-Refresh ~
FI-Ftphost FI-Ftpbruker FI-Ftppassord 
&Scoped-Define DISPLAYED-OBJECTS FI-Rappdir FI-ButikkNr FI-Navn FI-Dato ~
FI-MuligFraDato FI-MuligTilDato FI-Ftphost TG-Ftp FI-Ftpbruker ~
FI-Ftppassord 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getButnamn C-Win 
FUNCTION getButnamn RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Refresh 
     LABEL "Refresh" 
     SIZE 16.8 BY 1.14.

DEFINE VARIABLE FI-ButikkNr AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     LABEL "Butikk" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1.

DEFINE VARIABLE FI-Dato AS CHARACTER FORMAT "X(8)" 
     LABEL "Siste rapport" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1.

DEFINE VARIABLE FI-Ftpbruker AS CHARACTER FORMAT "X(256)":U 
     LABEL "Bruker" 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Ftphost AS CHARACTER FORMAT "X(256)":U 
     LABEL "Ftp-host" 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Ftppassord AS CHARACTER FORMAT "X(256)":U 
     LABEL "Passord" 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE FI-MuligFraDato AS CHARACTER FORMAT "X(8)" 
     LABEL "Ikke kjørte fra/til" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1.

DEFINE VARIABLE FI-MuligTilDato AS CHARACTER FORMAT "X(8)" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1.

DEFINE VARIABLE FI-Navn AS CHARACTER FORMAT "X(40)" 
     LABEL "Navn" 
     VIEW-AS FILL-IN 
     SIZE 36 BY 1.

DEFINE VARIABLE FI-Rappdir AS CHARACTER FORMAT "X(256)":U 
     LABEL "Rapportmappe" 
     VIEW-AS FILL-IN 
     SIZE 36 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 64 BY 14.38.

DEFINE RECTANGLE RECT-65
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 64 BY 4.05.

DEFINE VARIABLE TG-Ftp AS LOGICAL INITIAL no 
     LABEL "Ftp" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      SysPara SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 C-Win _STRUCTURED
  QUERY BROWSE-1 NO-LOCK DISPLAY
      SysPara.ParaNr COLUMN-LABEL "Butikk" FORMAT ">>>>>9":U
      getButnamn() COLUMN-LABEL "Navn" FORMAT "x(30)":U
      SysPara.Parameter1 COLUMN-LABEL "Siste rapportdato" FORMAT "X(8)":U
            WIDTH 11.8
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 60 BY 18.81 ROW-HEIGHT-CHARS .62 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BROWSE-1 AT ROW 2.67 COL 3.4
     FI-Rappdir AT ROW 3.33 COL 84 COLON-ALIGNED NO-TAB-STOP 
     FI-ButikkNr AT ROW 4.57 COL 84 COLON-ALIGNED HELP
          "Parameterets Id" NO-TAB-STOP 
     FI-Navn AT ROW 5.62 COL 84 COLON-ALIGNED HELP
          "Parameter 1" NO-TAB-STOP 
     FI-Dato AT ROW 6.71 COL 84 COLON-ALIGNED HELP
          "Parameter 1" NO-TAB-STOP 
     FI-MuligFraDato AT ROW 9.57 COL 84 COLON-ALIGNED HELP
          "Parameter 1" NO-TAB-STOP 
     FI-MuligTilDato AT ROW 9.57 COL 102.2 COLON-ALIGNED HELP
          "Parameter 1" NO-LABEL NO-TAB-STOP 
     B-Refresh AT ROW 12.43 COL 73
     FI-Ftphost AT ROW 18 COL 84 COLON-ALIGNED
     TG-Ftp AT ROW 18.14 COL 113
     FI-Ftpbruker AT ROW 19.14 COL 84 COLON-ALIGNED
     FI-Ftppassord AT ROW 20.29 COL 84 COLON-ALIGNED
     RECT-1 AT ROW 2.81 COL 65.2
     RECT-65 AT ROW 17.67 COL 65.2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 131 BY 21.1.


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
         TITLE              = "Lentus kupong statistik"
         HEIGHT             = 21.1
         WIDTH              = 131
         MAX-HEIGHT         = 31.05
         MAX-WIDTH          = 138.2
         VIRTUAL-HEIGHT     = 31.05
         VIRTUAL-WIDTH      = 138.2
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
/* BROWSE-TAB BROWSE-1 RECT-65 DEFAULT-FRAME */
ASSIGN 
       FI-ButikkNr:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       FI-Dato:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       FI-Ftpbruker:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       FI-Ftphost:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       FI-Ftppassord:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       FI-MuligFraDato:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       FI-MuligTilDato:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       FI-Navn:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       FI-Rappdir:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR TOGGLE-BOX TG-Ftp IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-1
/* Query rebuild information for BROWSE BROWSE-1
     _TblList          = "SkoTex.SysPara"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "SkoTex.SysPara.SysHId = 210
 AND SkoTex.SysPara.SysGr = 255"
     _FldNameList[1]   > SkoTex.SysPara.ParaNr
"SysPara.ParaNr" "Butikk" ">>>>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > "_<CALC>"
"getButnamn()" "Navn" "x(30)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > SkoTex.SysPara.Parameter1
"SysPara.Parameter1" "Siste rapportdato" "X(8)" "character" ? ? ? ? ? ? no "" no no "11.8" yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Lentus kupong statistik */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Lentus kupong statistik */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Refresh
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Refresh C-Win
ON CHOOSE OF B-Refresh IN FRAME DEFAULT-FRAME /* Refresh */
DO:
    DEFINE VARIABLE rRowId AS ROWID      NO-UNDO.
    rRowId = ROWID(SysPara).
    {&OPEN-QUERY-{&BROWSE-NAME}}
    APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.
    REPOSITION {&BROWSE-NAME} TO ROWID rRowid.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-1
&Scoped-define SELF-NAME BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 C-Win
ON VALUE-CHANGED OF BROWSE-1 IN FRAME DEFAULT-FRAME
DO:
    DEFINE VARIABLE dDato AS DATE       NO-UNDO.
    ASSIGN FI-ButikkNr:SCREEN-VALUE = STRING(Syspara.ParaNr)
           FI-ButikkNr
           FI-Navn:SCREEN-VALUE     = DYNAMIC-FUNCTION('getButnamn':U)
           FI-Dato:SCREEN-VALUE     = Syspara.parameter1.
    ASSIGN dDato = DATE(FI-Dato:SCREEN-VALUE) NO-ERROR.
    IF ERROR-STATUS:ERROR OR dDato >= TODAY - iDayDelay THEN DO:
        ASSIGN FI-MuligFraDato:SCREEN-VALUE = ""
               FI-MuligTilDato:SCREEN-VALUE = "". 
    END.
    ELSE DO:
        ASSIGN FI-MuligFraDato:SCREEN-VALUE = STRING(dDato + 1)
               FI-MuligTilDato:SCREEN-VALUE = STRING(TODAY - iDayDelay). 
    END.

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
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    RUN GetSyspara.
    RUN KontrollerSysPara.
  RUN enable_UI.
  APPLY "VALUE-CHANGED" TO BROWSE {&BROWSE-NAME}.
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
  DISPLAY FI-Rappdir FI-ButikkNr FI-Navn FI-Dato FI-MuligFraDato FI-MuligTilDato 
          FI-Ftphost TG-Ftp FI-Ftpbruker FI-Ftppassord 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-1 RECT-65 BROWSE-1 FI-Rappdir FI-ButikkNr FI-Navn FI-Dato 
         FI-MuligFraDato FI-MuligTilDato B-Refresh FI-Ftphost FI-Ftpbruker 
         FI-Ftppassord 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetSyspara C-Win 
PROCEDURE GetSyspara :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cTG-Ftp AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cDato AS CHARACTER  NO-UNDO.
    {syspara.i 210 254 1 FI-Rappdir}
    {syspara.i 210 254 2 iDayDelay INT}
    {syspara.i 210 254 3 cDato}
    IF NUM-ENTRIES(cDato,"/") = 3 AND LENGTH(cDato) = 10 THEN
        dFirstDate = DATE(INT(SUBSTR(cDato,6,2)),INT(SUBSTR(cDato,9,2)),INT(SUBSTR(cDato,1,4))).
/*     {syspara.i 210 201 5 cTG-Ftp}        */
/*     {syspara.i 210 201 6 FI-Ftphost}     */
/*     {syspara.i 210 201 10 FI-Ftpbruker}  */
/*     {syspar2.i 210 201 10 FI-Ftppassord} */
/*     TG-Ftp = cTG-Ftp = "1".              */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KontrollerSysPara C-Win 
PROCEDURE KontrollerSysPara :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*     FOR EACH butiker NO-LOCK WHERE CAN-FIND(FIRST Kasse WHERE Kasse.butik = butiker.butik AND Kasse.aktiv = TRUE):  */
/*         FIND Syspara WHERE SysPara.SysHId = 210 AND                                                                 */
/*                            SysPara.SysGr  = 255 AND                                                                 */
/*                            SysPara.ParaNr = butiker.butik NO-LOCK NO-ERROR.                                         */
/*         IF NOT AVAIL SysPara THEN DO:                                                                               */
/*             CREATE SysPara.                                                                                         */
/*             ASSIGN SysPara.SysHId = 210                                                                             */
/*                    SysPara.SysGr  = 255                                                                             */
/*                    SysPara.ParaNr = butiker.butik                                                                   */
/*                    SysPara.Beskrivelse = butiker.butnamn                                                            */
/*                    SysPara.Parameter1  = STRING(dFirstDate - 1) .                                                   */
/*         END.                                                                                                        */
/*     END.                                                                                                            */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Rapport C-Win 
PROCEDURE Rapport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iplUpdateSyspara AS LOGICAL    NO-UNDO.
    DEFINE INPUT  PARAMETER ipButikknr       AS INTEGER    NO-UNDO.
    DEFINE INPUT  PARAMETER ipDato           AS DATE       NO-UNDO.
/*     RUN xartstat2timegrip.p (FI-Rappdir:SCREEN-VALUE IN FRAME {&FRAME-NAME},ipButikkNr,ipDato,TG-Ftp:CHECKED,FI-Ftphost,FI-Ftpbruker,FI-Ftppassord). */
    IF iplUpdateSyspara = TRUE THEN DO:
        FIND SysPara WHERE SysPara.SysHId = 210 AND 
                           SysPara.SysGr  = 200 AND 
                           SysPara.ParaNr = ipButikkNr.
        ASSIGN SysPara.Parameter1 = STRING(ipDato).
        FIND CURRENT SysPara NO-LOCK.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getButnamn C-Win 
FUNCTION getButnamn RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND butiker WHERE butiker.butik = syspara.paranr NO-LOCK NO-ERROR.
  RETURN IF AVAIL butiker THEN butiker.butnamn ELSE "Ukjent".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

