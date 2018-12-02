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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-74 B-Send B-Avsluta FI-Artikkelnr 
&Scoped-Define DISPLAYED-OBJECTS FI-Artikkelnr FI-Txt FI-Error 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Avsluta 
     LABEL "Avsluta" 
     SIZE 15 BY 1.14.

DEFINE BUTTON B-Send 
     LABEL "Sänd" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE FI-Artikkelnr AS DECIMAL FORMAT ">>>>>>>>>>>>9":U INITIAL 0 
     LABEL "Artikelnr" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Error AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 56 BY 1.43
     FGCOLOR 12 FONT 6 NO-UNDO.

DEFINE VARIABLE FI-Txt AS CHARACTER FORMAT "X(256)":U INITIAL "Artikeln kommer att tas bort från kassan" 
      VIEW-AS TEXT 
     SIZE 40 BY 1.19 NO-UNDO.

DEFINE RECTANGLE RECT-74
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 60 BY 5.95.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     B-Send AT ROW 1.76 COL 65
     B-Avsluta AT ROW 3.43 COL 65
     FI-Artikkelnr AT ROW 4.33 COL 19 COLON-ALIGNED
     FI-Txt AT ROW 2.19 COL 7 COLON-ALIGNED NO-LABEL
     FI-Error AT ROW 5.76 COL 4 COLON-ALIGNED NO-LABEL
     RECT-74 AT ROW 1.71 COL 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80.8 BY 7.29.


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
         TITLE              = "Ta bort från kassan"
         HEIGHT             = 7.29
         WIDTH              = 80.8
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 80.8
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 80.8
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
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN FI-Error IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Txt IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Ta bort från kassan */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Ta bort från kassan */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Avsluta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Avsluta C-Win
ON CHOOSE OF B-Avsluta IN FRAME DEFAULT-FRAME /* Avsluta */
DO:
  APPLY "CLOSE" TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Send
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Send C-Win
ON CHOOSE OF B-Send IN FRAME DEFAULT-FRAME /* Sänd */
DO:
    ASSIGN INPUT FI-Artikkelnr.
  IF FI-Artikkelnr > 0 THEN DO:
      RUN fixElogg.
  END.
  APPLY "ENTRY" TO FI-Artikkelnr.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Artikkelnr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Artikkelnr C-Win
ON ANY-KEY OF FI-Artikkelnr IN FRAME DEFAULT-FRAME /* Artikelnr */
DO:
  IF INPUT FI-Artikkelnr = 0 THEN
      fi-Error:SCREEN-VALUE = "".
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
  RUN enable_UI.
  APPLY "ENTRY" TO FI-Artikkelnr.
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
  DISPLAY FI-Artikkelnr FI-Txt FI-Error 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-74 B-Send B-Avsluta FI-Artikkelnr 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fixElogg C-Win 
PROCEDURE fixElogg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

       IF CAN-FIND(ELogg WHERE 
           ELogg.TabellNavn     = "ArtBas" AND
           ELogg.EksterntSystem = "POS"    AND
           ELogg.Verdier        = STRING(FI-Artikkelnr)) THEN DO:
           FI-Error:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "Ändring som inte behandlats finns, vänta...".
           RETURN.
       END.
       IF NOT CAN-FIND(ELogg WHERE 
           ELogg.TabellNavn     = "RedeleteArtBas" AND
           ELogg.EksterntSystem = "POS"    AND
           ELogg.Verdier        = STRING(FI-Artikkelnr)) THEN DO:
           CREATE ELogg.
           ASSIGN ELogg.TabellNavn     = "RedeleteArtBas"
                  ELogg.EksterntSystem = "POS"   
                  ELogg.Verdier        = STRING(FI-Artikkelnr)
                  ELogg.EndringsType = 3
                  ELogg.Behandlet    = FALSE NO-ERROR.

           FIND CURRENT ELogg NO-LOCK.
       END.
       RELEASE ELogg.
       FI-Artikkelnr = 0.
       FI-Artikkelnr:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(FI-Artikkelnr).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

