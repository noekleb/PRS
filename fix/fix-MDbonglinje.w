&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win

/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE TT_Butiker NO-UNDO LIKE Butiker
       FIELD StartTid AS CHARACTER
       FIELD FerdigTid AS CHARACTER.


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

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BROWSE-TT_Butiker

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES TT_Butiker

/* Definitions for BROWSE BROWSE-TT_Butiker                             */
&Scoped-define FIELDS-IN-QUERY-BROWSE-TT_Butiker TT_Butiker.Butik ~
TT_Butiker.ButNamn StartTid FerdigTid 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-TT_Butiker 
&Scoped-define QUERY-STRING-BROWSE-TT_Butiker FOR EACH TT_Butiker NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-TT_Butiker OPEN QUERY BROWSE-TT_Butiker FOR EACH TT_Butiker NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-TT_Butiker TT_Butiker
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-TT_Butiker TT_Butiker


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BROWSE-TT_Butiker}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-TT_Butiker B-Start FI-Exportfil 
&Scoped-Define DISPLAYED-OBJECTS FI-Exportfil 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Start 
     LABEL "Starta" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE FI-Exportfil AS CHARACTER FORMAT "X(256)":U 
     LABEL "Exportfil" 
     VIEW-AS FILL-IN 
     SIZE 54 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-TT_Butiker FOR 
      TT_Butiker SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-TT_Butiker
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-TT_Butiker C-Win _STRUCTURED
  QUERY BROWSE-TT_Butiker NO-LOCK DISPLAY
      TT_Butiker.Butik FORMAT ">>>>>9":U
      TT_Butiker.ButNamn FORMAT "x(20)":U WIDTH 29.4
      StartTid COLUMN-LABEL "Start" FORMAT "x(8)":U WIDTH 9
      FerdigTid COLUMN-LABEL "Färdig" FORMAT "x(8)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 61 BY 12.38 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BROWSE-TT_Butiker AT ROW 1.95 COL 3
     B-Start AT ROW 2.91 COL 65
     FI-Exportfil AT ROW 15.05 COL 8 COLON-ALIGNED
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 16.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: TT_Butiker T "?" NO-UNDO skotex Butiker
      ADDITIONAL-FIELDS:
          FIELD StartTid AS CHARACTER
          FIELD FerdigTid AS CHARACTER
      END-FIELDS.
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Fixa fel i MD-inläsning"
         HEIGHT             = 16
         WIDTH              = 80
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 80
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
/* BROWSE-TAB BROWSE-TT_Butiker 1 DEFAULT-FRAME */
ASSIGN 
       FI-Exportfil:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-TT_Butiker
/* Query rebuild information for BROWSE BROWSE-TT_Butiker
     _TblList          = "Temp-Tables.TT_Butiker"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   = Temp-Tables.TT_Butiker.Butik
     _FldNameList[2]   > Temp-Tables.TT_Butiker.ButNamn
"TT_Butiker.ButNamn" ? ? "character" ? ? ? ? ? ? no ? no no "29.4" yes no no "U" "" ""
     _FldNameList[3]   > "_<CALC>"
"StartTid" "Start" "x(8)" ? ? ? ? ? ? ? no ? no no "9" yes no no "U" "" ""
     _FldNameList[4]   > "_<CALC>"
"FerdigTid" "Färdig" "x(8)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BROWSE-TT_Butiker */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Fixa fel i MD-inläsning */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Fixa fel i MD-inläsning */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Start
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Start C-Win
ON CHOOSE OF B-Start IN FRAME DEFAULT-FRAME /* Starta */
DO:
  FOR EACH TT_Butiker:
      ASSIGN TT_Butiker.StartTid = STRING(TIME,"HH:MM:SS").
      REPOSITION BROWSE-TT_Butiker TO ROWID ROWID(TT_Butiker) NO-ERROR.
      BROWSE BROWSE-TT_Butiker:REFRESH().
      RUN FixaBonglinje (TT_Butiker.Butik).
      ASSIGN TT_Butiker.FerdigTid = STRING(TIME,"HH:MM:SS").
      BROWSE BROWSE-TT_Butiker:REFRESH().
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-TT_Butiker
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
  RUN SkapaTT_Butiker.
  ASSIGN FI-Exportfil = SESSION:TEMP-DIRECTORY + "fixbonglinjeMD.txt".
  RUN enable_UI.
  BROWSE BROWSE-TT_Butiker:SET-REPOSITIONED-ROW(BROWSE BROWSE-TT_Butiker:DOWN,"ALWAYS").
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
  DISPLAY FI-Exportfil 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE BROWSE-TT_Butiker B-Start FI-Exportfil 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FixaBonglinje C-Win 
PROCEDURE FixaBonglinje :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEFINE INPUT  PARAMETER ipButik  LIKE Butiker.Butik NO-UNDO.
 OUTPUT TO VALUE(FI-Exportfil) APPEND.
 DO TRANSACTION:
   For each bonglinje where butikk = ipButik AND (TTId = 1 OR TTId = 10):
        If hovedgr = 1905 then do:
            EXPORT bonglinje.
             Assign Linjesum  = linjesum
                    Bongpris   = ABS(antall) * bongpris
                    VVarekost = 0
                    Ttid      = 62
                    Antall    = 0.
       End.
       Else if abs(antall) > 1 then do:
           EXPORT bonglinje.
           Assign Bongpris   = ABS(antall) * vvarekost
                  VVarekost  =   Bongpris.
       End.
   END.
 END.
 OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapaTT_Butiker C-Win 
PROCEDURE SkapaTT_Butiker :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH Butiker WHERE CAN-FIND(FIRST kasse WHERE kasse.butik = butiker.butik AND
                 Kasse.ModellNr = 30) NO-LOCK:
        BUFFER-COPY Butiker USING butik butnamn TO TT_Butiker.
        RELEASE TT_Butiker.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

