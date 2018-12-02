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
DEFINE INPUT  PARAMETER h_Parent AS HANDLE      NO-UNDO.
DEFINE INPUT  PARAMETER hScanner AS HANDLE      NO-UNDO.
/* Local Variable Definitions ---                                       */

DEFINE VARIABLE iXcenter AS INTEGER     NO-UNDO.
DEFINE VARIABLE iYcenter AS INTEGER     NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 FI-Strekkode Btn_OK FI-Txt 
&Scoped-Define DISPLAYED-OBJECTS FI-Strekkode FI-Vg FI-Lopnr FI-storlek ~
FI-Ean FI-Txt 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "Avslutt" 
     SIZE 15 BY 1.15.

DEFINE VARIABLE FI-Ean AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 38.86 BY 1.19
     BGCOLOR 15 FONT 8 NO-UNDO.

DEFINE VARIABLE FI-Lopnr AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 12.86 BY 1.19
     BGCOLOR 15 FONT 8 NO-UNDO.

DEFINE VARIABLE FI-storlek AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 12.86 BY 1.19
     BGCOLOR 15 FONT 8 NO-UNDO.

DEFINE VARIABLE FI-Strekkode AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 47 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Txt AS CHARACTER FORMAT "X(256)":U INITIAL "Senast registrerade" 
      VIEW-AS TEXT 
     SIZE 45 BY 1.31
     FONT 8 NO-UNDO.

DEFINE VARIABLE FI-Vg AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 12.86 BY 1.19
     BGCOLOR 15 FONT 8 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 50 BY 1.38.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     FI-Strekkode AT ROW 1.23 COL 3 NO-LABEL
     FI-Vg AT ROW 3.96 COL 1 COLON-ALIGNED NO-LABEL
     FI-Lopnr AT ROW 3.96 COL 14.14 COLON-ALIGNED NO-LABEL
     FI-storlek AT ROW 3.96 COL 27.43 COLON-ALIGNED NO-LABEL
     FI-Ean AT ROW 4.69 COL 9 COLON-ALIGNED NO-LABEL
     Btn_OK AT ROW 5.35 COL 3
     FI-Txt AT ROW 2.54 COL 3 NO-LABEL
     RECT-1 AT ROW 1.04 COL 1.14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS TOP-ONLY NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 50.86 BY 5.69.


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
         TITLE              = "Strekkoderegistrering"
         HEIGHT             = 5.65
         WIDTH              = 50.86
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 80
         MIN-BUTTON         = no
         MAX-BUTTON         = no
         ALWAYS-ON-TOP      = yes
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
/* SETTINGS FOR FILL-IN FI-Ean IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Lopnr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-storlek IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Strekkode IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FI-Txt IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FI-Vg IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _Query            is NOT OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Strekkoderegistrering */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Strekkoderegistrering */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK C-Win
ON CHOOSE OF Btn_OK IN FRAME DEFAULT-FRAME /* Avslutt */
DO:
  APPLY "CLOSE" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Strekkode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Strekkode C-Win
ON TAB OF FI-Strekkode IN FRAME DEFAULT-FRAME
OR "F10"  OF FI-Strekkode OR "RETURN":U OF FI-Strekkode
DO:
    DEFINE VARIABLE cc AS CHARACTER   NO-UNDO.
    IF VALID-HANDLE(hScanner) THEN DO:
        /* Disabla scanner */
        RUN ScanRelease IN hScanner.

    END.
  RUN SetskjermVerdier IN h_Parent
      (
        INPUT INPUT FI-Strekkode
      ).
  IF RETURN-VALUE = "AVBRYT" THEN DO:
      ASSIGN
          FI-Strekkode:SCREEN-VALUE = "".
      IF VALID-HANDLE(hScanner) THEN DO:
          /* Eanbla scanner */
         RUN Claim IN hScanner.
      END.
      RETURN NO-APPLY.
  END.
  ELSE DO:
    RUN UpdateRecord IN h_Parent.
    DO WITH FRAME gDialog:
      if return-value <> "AVBRYT" then DO:
        cc = FI-Strekkode:SCREEN-VALUE.
        IF LENGTH(cc) = 12 THEN DO:
            FI-Vg:SCREEN-VALUE = LEFT-TRIM(SUBSTR(cc,1,3),"0").
            FI-Lopnr:SCREEN-VALUE = LEFT-TRIM(SUBSTR(cc,4,4),"0").
            FI-Storlek:SCREEN-VALUE = SUBSTR(cc,9,3) + "." + SUBSTR(cc,12).
            FI-Storlek:SCREEN-VALUE = TRIM(TRIM(FI-Storlek:SCREEN-VALUE,"0"),".").
        END.
        ELSE IF LENGTH(cc) = 13 THEN DO:
            FI-Ean:SCREEN-VALUE = cc.
            FI-EAN:HIDDEN = FALSE.
            FI-EAN:MOVE-TO-TOP().
        END.
      END.
      ASSIGN
          FI-Strekkode:SCREEN-VALUE = ""
          .
      APPLY "ENTRY":U TO FI-Strekkode.
    END.
/*     RUN AddRecord IN h_Parent.  */
/*     RUN VisButiker IN h_Parent. */
    IF VALID-HANDLE(hScanner) THEN DO:
        /* Eanbla scanner */
/*         RUN ClearInput in hScanner. */
/*         RUN ClearOutput in hScanner. */
        RUN Claim IN hScanner.
    END.
    RETURN NO-APPLY.
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
ON CLOSE OF THIS-PROCEDURE DO:
    IF VALID-HANDLE(hScanner) THEN
        RUN ScanRelease IN hScanner.
    RUN disable_UI.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    RUN SendX_Y IN h_Parent (OUTPUT iXcenter,OUTPUT iYcenter) NO-ERROR.
    IF iXcenter <> 0 THEN DO:
        {&WINDOW-NAME}:X = iXcenter - {&WINDOW-NAME}:WIDTH-PIXELS / 2.
        {&WINDOW-NAME}:Y = iYcenter - {&WINDOW-NAME}:HEIGHT-PIXELS / 2.
    END.
    IF VALID-HANDLE(hScanner) THEN DO:
        RUN Claim IN hScanner.
        SUBSCRIBE TO "STRECKKOD" IN hScanner.
    END.
    FI-EAN:X = FI-Vg:X.
    FI-EAN:Y = FI-Vg:Y.
  RUN enable_UI.
  FI-Ean:HIDDEN = TRUE.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.
RUN cancelRecord IN h_Parent.

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
  DISPLAY FI-Strekkode FI-Vg FI-Lopnr FI-storlek FI-Ean FI-Txt 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-1 FI-Strekkode Btn_OK FI-Txt 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE STRECKKOD C-Win 
PROCEDURE STRECKKOD :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER cStreckkod AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER cFrom      AS CHARACTER   NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:
      FI-Strekkode:SCREEN-VALUE = cStreckkod.
      APPLY "RETURN" TO Fi-Strekkode.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

