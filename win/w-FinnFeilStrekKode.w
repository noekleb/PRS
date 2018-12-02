&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win

/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE TT_FeilStrekKode NO-UNDO LIKE StrekKode
       FIELD StrTypeId LIKE StrType.StrTypeID
       FIELD Beskr LIKE ArtBas.Beskr.


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
DEFINE VARIABLE iStrTypeId LIKE StrType.StrTypeId    NO-UNDO.

DEFINE BUFFER bStrekKode FOR StrekKode.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME B-FeilStrek

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES TT_FeilStrekKode StrType StrTStr

/* Definitions for BROWSE B-FeilStrek                                   */
&Scoped-define FIELDS-IN-QUERY-B-FeilStrek TT_FeilStrekKode.ArtikkelNr ~
TT_FeilStrekKode.Beskr TT_FeilStrekKode.Kode TT_FeilStrekKode.StrKode 
&Scoped-define ENABLED-FIELDS-IN-QUERY-B-FeilStrek 
&Scoped-define OPEN-QUERY-B-FeilStrek OPEN QUERY B-FeilStrek FOR EACH TT_FeilStrekKode NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-B-FeilStrek TT_FeilStrekKode
&Scoped-define FIRST-TABLE-IN-QUERY-B-FeilStrek TT_FeilStrekKode


/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 StrType.Beskrivelse ~
StrType.Intervall StrTStr.SoStorl 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2 
&Scoped-define OPEN-QUERY-BROWSE-2 OPEN QUERY BROWSE-2 FOR EACH StrType ~
      WHERE StrType.StrTypeID = iStrTypeId NO-LOCK, ~
      EACH StrTStr OF StrType NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-2 StrType StrTStr
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 StrType
&Scoped-define SECOND-TABLE-IN-QUERY-BROWSE-2 StrTStr


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-B-FeilStrek}~
    ~{&OPEN-QUERY-BROWSE-2}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS B-FeilStrek B-Velg BROWSE-2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Velg 
     LABEL "Velg størrelse" 
     SIZE 15 BY 1.14.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY B-FeilStrek FOR 
      TT_FeilStrekKode SCROLLING.

DEFINE QUERY BROWSE-2 FOR 
      StrType, 
      StrTStr SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE B-FeilStrek
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS B-FeilStrek C-Win _STRUCTURED
  QUERY B-FeilStrek NO-LOCK DISPLAY
      TT_FeilStrekKode.ArtikkelNr FORMAT "zzzzzzzzzzzz9":U
      TT_FeilStrekKode.Beskr COLUMN-LABEL "Beskrivelse" FORMAT "X(25)":U
            WIDTH 30
      TT_FeilStrekKode.Kode FORMAT "X(20)":U
      TT_FeilStrekKode.StrKode FORMAT ">>9":U WIDTH 14.6
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 87 BY 12.38 EXPANDABLE.

DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 C-Win _STRUCTURED
  QUERY BROWSE-2 NO-LOCK DISPLAY
      StrType.Beskrivelse FORMAT "X(30)":U
      StrType.Intervall FORMAT "x(12)":U
      StrTStr.SoStorl FORMAT "x(4)":U WIDTH 10
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 58 BY 4.52 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     B-FeilStrek AT ROW 1.95 COL 7
     B-Velg AT ROW 15.29 COL 69
     BROWSE-2 AT ROW 15.48 COL 7
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: TT_FeilStrekKode T "?" NO-UNDO skotex StrekKode
      ADDITIONAL-FIELDS:
          FIELD StrTypeId LIKE StrType.StrTypeID
          FIELD Beskr LIKE ArtBas.Beskr
      END-FIELDS.
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Strekkoder med feil størrelse"
         HEIGHT             = 19.86
         WIDTH              = 94.4
         MAX-HEIGHT         = 26.71
         MAX-WIDTH          = 158.8
         VIRTUAL-HEIGHT     = 26.71
         VIRTUAL-WIDTH      = 158.8
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
   Size-to-Fit                                                          */
/* BROWSE-TAB B-FeilStrek 1 DEFAULT-FRAME */
/* BROWSE-TAB BROWSE-2 B-Velg DEFAULT-FRAME */
ASSIGN 
       FRAME DEFAULT-FRAME:SCROLLABLE       = FALSE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE B-FeilStrek
/* Query rebuild information for BROWSE B-FeilStrek
     _TblList          = "Temp-Tables.TT_FeilStrekKode"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   = Temp-Tables.TT_FeilStrekKode.ArtikkelNr
     _FldNameList[2]   > "_<CALC>"
"TT_FeilStrekKode.Beskr" "Beskrivelse" "X(25)" ? ? ? ? ? ? ? no ? no no "30" yes no no "U" "" ""
     _FldNameList[3]   = Temp-Tables.TT_FeilStrekKode.Kode
     _FldNameList[4]   > Temp-Tables.TT_FeilStrekKode.StrKode
"StrKode" ? ? "integer" ? ? ? ? ? ? no ? no no "14.6" yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE B-FeilStrek */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _TblList          = "skotex.StrType,skotex.StrTStr OF skotex.StrType"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "skotex.StrType.StrTypeID = iStrTypeId"
     _FldNameList[1]   = skotex.StrType.Beskrivelse
     _FldNameList[2]   = skotex.StrType.Intervall
     _FldNameList[3]   > skotex.StrTStr.SoStorl
"StrTStr.SoStorl" ? ? "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BROWSE-2 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Strekkoder med feil størrelse */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Strekkoder med feil størrelse */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME B-FeilStrek
&Scoped-define SELF-NAME B-FeilStrek
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-FeilStrek C-Win
ON VALUE-CHANGED OF B-FeilStrek IN FRAME DEFAULT-FRAME
DO:
  ASSIGN iStrTypeId = TT_FeilStrekKode.StrTypeId.
{&OPEN-QUERY-BROWSE-2}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Velg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Velg C-Win
ON CHOOSE OF B-Velg IN FRAME DEFAULT-FRAME /* Velg størrelse */
DO:
  IF AVAIL StrTstr THEN DO:
      FIND StrKonv WHERE StrKonv.Storl = StrTstr.SoStorl NO-LOCK NO-ERROR.
      IF AVAIL StrKonv THEN DO:
          MESSAGE "Overfør til Artikkel?"
              VIEW-AS ALERT-BOX INFO BUTTONS OK UPDATE l_Ok AS LOGICAL.
          IF l_Ok THEN DO:
              RUN UpdateStrekKode.
              BROWSE B-FeilStrek:REFRESH().
              BROWSE B-FeilStrek:SELECT-NEXT-ROW().
              APPLY "VALUE-CHANGED" TO BROWSE B-FeilStrek.
          END.
      END.
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
  RUN FinnFeil.
  RUN enable_UI.
  IF BROWSE B-FeilStrek:FOCUSED-ROW <> ? THEN
      APPLY "VALUE-CHANGED" TO B-FeilStrek.
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
  ENABLE B-FeilStrek B-Velg BROWSE-2 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FinnFeil C-Win 
PROCEDURE FinnFeil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH ArtBas WHERE ArtBas.Pakke = FALSE NO-LOCK:
        FIND strtype OF artbas NO-LOCK NO-ERROR.
        IF NOT AVAIL Strtype THEN
            NEXT.
        FOR EACH strekkode OF ArtBas NO-LOCK WHERE StrekKode.KodeType = 1 AND 
                NOT StrekKode.Kode BEGINS "02" AND
                NOT CAN-DO(StrType.Fordeling,STRING(StrekKode.StrKode)):
            CREATE TT_FeilStrekKode.
            ASSIGN TT_FeilStrekKode.ArtikkelNr = ArtBas.ArtikkelNr
                   TT_FeilStrekKode.Kode       = StrekKode.Kode
                   TT_FeilStrekKode.StrTypeId  = StrType.StrTypeId
                   TT_FeilStrekKode.Beskr      = ArtBas.Beskr.
        END.
    END.
    RELEASE TT_FeilStrekKode.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UpdateStrekKode C-Win 
PROCEDURE UpdateStrekKode :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FIND bStrekKode WHERE bStrekKode.Kode = TT_FeilStrekKode.Kode EXCLUSIVE NO-ERROR.
    IF AVAIL bStrekKode THEN
        ASSIGN bStrekKode.StrKode = StrKonv.StrKode
               TT_FeilStrekKode.StrKode = StrKonv.StrKode.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

