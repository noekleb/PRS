&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win

/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE TT_StrekKode NO-UNDO LIKE StrekKode
       FIELD Beskr AS CHAR
       FIELD Strtypeid AS INTE
       FIELD Vg AS INTE
       FIELD Storl AS CHAR.


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
DEFINE INPUT  PARAMETER hParentWin AS HANDLE     NO-UNDO.
DEFINE INPUT  PARAMETER hParentProc AS HANDLE     NO-UNDO.
/* Local Variable Definitions ---                                       */
DEFINE VARIABLE iButik        AS INTEGER    NO-UNDO.
DEFINE VARIABLE hFrameHandle AS HANDLE     NO-UNDO.
DEFINE VARIABLE cSkjul AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lSkjul AS LOGICAL    NO-UNDO.
DEFINE VARIABLE dByttObject AS DECIMAL    NO-UNDO.
DEFINE VARIABLE lInlev AS LOGICAL    NO-UNDO.
{etikettlogg.i &NEW=NEW}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BROWSE-Ean

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES TT_StrekKode

/* Definitions for BROWSE BROWSE-Ean                                    */
&Scoped-define FIELDS-IN-QUERY-BROWSE-Ean TT_StrekKode.ArtikkelNr ~
TT_StrekKode.Kode TT_Strekkode.Beskr TT_StrekKode.StrtypeId ~
TT_Strekkode.Storl TT_Strekkode.Vg 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-Ean 
&Scoped-define QUERY-STRING-BROWSE-Ean FOR EACH TT_StrekKode NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-Ean OPEN QUERY BROWSE-Ean FOR EACH TT_StrekKode NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-Ean TT_StrekKode
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-Ean TT_StrekKode


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BROWSE-Ean}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-64 BROWSE-Ean B-Sok B-Slett ArtikkelNr ~
Beskr StrTypeID Vg B-Flytt 
&Scoped-Define DISPLAYED-OBJECTS ArtikkelNr Beskr StrTypeID Vg 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SjekkStr C-Win 
FUNCTION SjekkStr RETURNS LOGICAL
  ( INPUT iStrtypeid AS INTEGER,OUTPUT cStrKoder AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Flytt 
     LABEL "Flytt" 
     SIZE 15 BY 1.14.

DEFINE BUTTON B-Slett 
     LABEL "Slett" 
     SIZE 15 BY 1.14.

DEFINE BUTTON B-Sok 
     LABEL "Søk artikkel" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE ArtikkelNr LIKE ArtBas.ArtikkelNr
     VIEW-AS FILL-IN 
     SIZE 20.2 BY 1 NO-UNDO.

DEFINE VARIABLE Beskr LIKE ArtBas.Beskr
     VIEW-AS FILL-IN 
     SIZE 30.8 BY 1 NO-UNDO.

DEFINE VARIABLE StrTypeID LIKE ArtBas.StrTypeID
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1 NO-UNDO.

DEFINE VARIABLE Vg LIKE ArtBas.Vg
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-64
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 49.6 BY 6.29.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-Ean FOR 
      TT_StrekKode SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-Ean
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-Ean C-Win _STRUCTURED
  QUERY BROWSE-Ean NO-LOCK DISPLAY
      TT_StrekKode.ArtikkelNr FORMAT "zzzzzzzzzzzz9":U
      TT_StrekKode.Kode COLUMN-LABEL "Ean" FORMAT "X(15)":U WIDTH 18
      TT_Strekkode.Beskr COLUMN-LABEL "Beskr" FORMAT "x(30)":U
            WIDTH 25
      TT_StrekKode.StrtypeId COLUMN-LABEL "Strtype" FORMAT ">>>>>9":U
            WIDTH 8
      TT_Strekkode.Storl COLUMN-LABEL "Størrelse" FORMAT "X(5)":U
            WIDTH 10
      TT_Strekkode.Vg COLUMN-LABEL "Vg" FORMAT ">>>>>9":U WIDTH 5.6
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 89.6 BY 9.05 ROW-HEIGHT-CHARS .62 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BROWSE-Ean AT ROW 1.14 COL 1.4
     B-Sok AT ROW 1.24 COL 92
     B-Slett AT ROW 2.67 COL 92
     ArtikkelNr AT ROW 5 COL 106.8 COLON-ALIGNED HELP
          ""
     Beskr AT ROW 5.95 COL 106.8 COLON-ALIGNED HELP
          "Kort beskrivelse av artikkelen"
     StrTypeID AT ROW 7 COL 106.8 COLON-ALIGNED HELP
          "Størrelsestype"
     Vg AT ROW 8 COL 106.8 COLON-ALIGNED HELP
          "'varegruppenummer"
     B-Flytt AT ROW 8.05 COL 122
     RECT-64 AT ROW 4 COL 92
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 141.2 BY 9.57.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: TT_StrekKode T "?" NO-UNDO SkoTex StrekKode
      ADDITIONAL-FIELDS:
          FIELD Beskr AS CHAR
          FIELD Strtypeid AS INTE
          FIELD Vg AS INTE
          FIELD Storl AS CHAR
      END-FIELDS.
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Flytt Ean"
         HEIGHT             = 9.57
         WIDTH              = 141.2
         MAX-HEIGHT         = 12.95
         MAX-WIDTH          = 144.4
         VIRTUAL-HEIGHT     = 12.95
         VIRTUAL-WIDTH      = 144.4
         SHOW-IN-TASKBAR    = NO
         MIN-BUTTON         = NO
         MAX-BUTTON         = NO
         ALWAYS-ON-TOP      = YES
         RESIZE             = NO
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



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
/* BROWSE-TAB BROWSE-Ean RECT-64 DEFAULT-FRAME */
/* SETTINGS FOR FILL-IN ArtikkelNr IN FRAME DEFAULT-FRAME
   LIKE = SkoTex.ArtBas. EXP-HELP EXP-SIZE                              */
/* SETTINGS FOR FILL-IN Beskr IN FRAME DEFAULT-FRAME
   LIKE = SkoTex.ArtBas. EXP-HELP EXP-SIZE                              */
/* SETTINGS FOR FILL-IN StrTypeID IN FRAME DEFAULT-FRAME
   LIKE = SkoTex.ArtBas. EXP-HELP                                       */
/* SETTINGS FOR FILL-IN Vg IN FRAME DEFAULT-FRAME
   LIKE = SkoTex.ArtBas. EXP-HELP                                       */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-Ean
/* Query rebuild information for BROWSE BROWSE-Ean
     _TblList          = "Temp-Tables.TT_StrekKode"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   = Temp-Tables.TT_StrekKode.ArtikkelNr
     _FldNameList[2]   > Temp-Tables.TT_StrekKode.Kode
"TT_StrekKode.Kode" "Ean" "X(15)" "character" ? ? ? ? ? ? no ? no no "18" yes no no "U" "" ""
     _FldNameList[3]   > "_<CALC>"
"TT_Strekkode.Beskr" "Beskr" "x(30)" ? ? ? ? ? ? ? no ? no no "25" yes no no "U" "" ""
     _FldNameList[4]   > "_<CALC>"
"TT_StrekKode.StrtypeId" "Strtype" ">>>>>9" ? ? ? ? ? ? ? no ? no no "8" yes no no "U" "" ""
     _FldNameList[5]   > "_<CALC>"
"TT_Strekkode.Storl" "Størrelse" "X(5)" ? ? ? ? ? ? ? no ? no no "10" yes no no "U" "" ""
     _FldNameList[6]   > "_<CALC>"
"TT_Strekkode.Vg" "Vg" ">>>>>9" ? ? ? ? ? ? ? no ? no no "5.6" yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BROWSE-Ean */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Flytt Ean */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON PARENT-WINDOW-CLOSE OF C-Win /* Flytt Ean */
DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Flytt Ean */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESTORED OF C-Win /* Flytt Ean */
DO:
  MESSAGE "window-restored"
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ArtikkelNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ArtikkelNr C-Win
ON TAB OF ArtikkelNr IN FRAME DEFAULT-FRAME /* Artikkelnummer */
OR RETURN OF ArtikkelNr DO:
    IF INPUT Artikkelnr <> 0 THEN
        RUN HentArtBas (INPUT INPUT Artikkelnr).
    IF Artikkelnr:SCREEN-VALUE = "0" THEN
        APPLY "ENTRY" TO Artikkelnr.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Flytt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Flytt C-Win
ON CHOOSE OF B-Flytt IN FRAME DEFAULT-FRAME /* Flytt */
DO:
  FIND artbas WHERE artbas.artikkelnr = DECI(Artikkelnr:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-LOCK.
  FOR EACH TT_StrekKode:
      FIND Strekkode WHERE Strekkode.kode = TT_Strekkode.kode NO-ERROR.
      IF AVAIL StrekKode THEN
          ASSIGN Strekkode.artikkelnr = DECI(Artikkelnr:SCREEN-VALUE IN FRAME {&FRAME-NAME})
                 Strekkode.bestillingsnummer = Artbas.Levkod.
  END.
  dByttObject = DECI(Artikkelnr:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
  APPLY "CLOSE" TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Slett
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Slett C-Win
ON CHOOSE OF B-Slett IN FRAME DEFAULT-FRAME /* Slett */
DO:
  IF BROWSE {&BROWSE-NAME}:FOCUSED-ROW <> ? THEN DO:
      BROWSE {&BROWSE-NAME}:SELECT-FOCUSED-ROW().
      DELETE TT_Strekkode.
      BROWSE {&BROWSE-NAME}:REFRESH().
  END.
  IF BROWSE {&BROWSE-NAME}:FOCUSED-ROW = ? THEN DO:
      ASSIGN ArtikkelNr:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
             Beskr:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
             StrTypeID:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
             Vg:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
  END.
      ASSIGN B-Flytt:SENSITIVE = FALSE
             B-Sok:SENSITIVE = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Sok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Sok C-Win
ON CHOOSE OF B-Sok IN FRAME DEFAULT-FRAME /* Søk artikkel */
DO:
    DEFINE VARIABLE ipdArtikkelnr AS DECIMAL    NO-UNDO.
    RUN d-hsok.w (OUTPUT ipdArtikkelnr,"").
    IF ipdArtikkelnr <> 0 THEN
        RUN HentArtBas (ipdArtikkelnr).
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-Ean
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
    RUN disable_UI.
    IF dByttObject <> 0 THEN
        RUN ByttArtikkel IN hParentProc (dByttObject).
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.
/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  {syspara.i 5 4 14 cSkjul}
  RUN enable_UI.
  RUN WinPosition.
  B-Flytt:SENSITIVE = FALSE.
  {lng.i}
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Avslutt C-Win 
PROCEDURE Avslutt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    MESSAGE "Etikettvindu avsluttes"
        VIEW-AS ALERT-BOX INFO BUTTONS YES-NO UPDATE choice AS LOGICAL.
    IF choice THEN
        APPLY "CLOSE" TO THIS-PROCEDURE.
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
  DISPLAY ArtikkelNr Beskr StrTypeID Vg 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-64 BROWSE-Ean B-Sok B-Slett ArtikkelNr Beskr StrTypeID Vg B-Flytt 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HentArtBas C-Win 
PROCEDURE HentArtBas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipdArtikkelnr AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE cStrKoder AS CHARACTER  NO-UNDO.
    IF CAN-FIND(FIRST TT_Strekkode WHERE TT_Strekkode.artikkelnr = ipdArtikkelnr) THEN DO:
        Artikkelnr:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "0".
        RETURN.
    END.
    IF ipdArtikkelnr <> 0 THEN DO:
        FIND ArtBas WHERE ArtBas.Artikkelnr = ipdArtikkelnr NO-LOCK NO-ERROR.
        IF AVAIL ArtBas THEN DO:
            /* Strekkoder skal kunne flyttes også til plu artikler.
            IF Artbas.artikkelnr = ArtBas.Vg THEN DO:
                MESSAGE "Kan ikke flyttes til PLU-artikkel"
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
                Artikkelnr:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "0".
                RETURN.
            END.
            */
            /*IF SjekkStr(ArtBas.Strtypeid,OUTPUT cStrKoder) THEN */
            DO:
                ASSIGN ArtikkelNr:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ArtBas.Artikkelnr)
                       Beskr:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ArtBas.Beskr
                       StrTypeID:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ArtBas.StrtypeId)
                       Vg:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ArtBas.Vg).
                /* Skal kunne flyttes uavhengig av størrelsestype
                IF cStrKoder <> "" THEN DO:
                    MESSAGE "Størrelse " SKIP
                            cStrKoder SKIP
                            "finnes ikke i størrelsestype for " ipdArtikkelnr
                        VIEW-AS ALERT-BOX INFO BUTTONS OK.
                    ASSIGN ArtikkelNr:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "0"
                           Beskr:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
                           StrTypeID:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "0"
                           Vg:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "0".
                    RETURN.
                END.
                */
                B-Flytt:SENSITIVE = TRUE.
            END.
            /*
            ELSE DO:
                MESSAGE "Finner ikke størrelsestype på artikkel " ipdArtikkelnr
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
                Artikkelnr:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "0".
            END.
            */
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NyEan C-Win 
PROCEDURE NyEan :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER dArtikkelnr AS DECIMAL    NO-UNDO.
  DEFINE INPUT  PARAMETER cStrekKode  LIKE StrekKode.Kode NO-UNDO.
  DEFINE INPUT  PARAMETER iStrKode    AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER cStorl      AS CHARACTER  NO-UNDO.
  IF B-Flytt:SENSITIVE IN FRAME {&FRAME-NAME} THEN
      RETURN.
  FIND StrekKode WHERE StrekKode.kode = cStrekKode NO-LOCK NO-ERROR.
  IF NOT AVAIL StrekKode THEN
      RETURN.
  IF CAN-FIND(TT_StrekKode WHERE TT_StrekKode.kode = cStrekKode) THEN
      RETURN.
  FIND FIRST TT_StrekKode NO-ERROR.
  IF AVAIL TT_StrekKode AND TT_StrekKode.Artikkelnr <> dArtikkelNr THEN
      RETURN.
  IF StrekKode.StrKode > 0 THEN
  FIND StrKonv OF StrekKode NO-LOCK NO-ERROR. 
  FIND ArtBas OF StrekKode NO-LOCK NO-ERROR.
  IF NOT AVAIL ArtBas THEN
      RETURN.

  ELSE DO:
      CREATE TT_Strekkode.
      ASSIGN TT_Strekkode.Artikkelnr = dArtikkelnr
             TT_Strekkode.kode       = cStrekkode
             TT_Strekkode.beskr      = ArtBas.Beskr
             TT_Strekkode.StrtypeId  = ArtBas.StrTypeId
             TT_Strekkode.Vg         = ArtBas.Vg
             TT_Strekkode.Strkode    = iStrKode
             TT_StrekKode.Storl       = cStorl.
      B-Sok:SENSITIVE = TRUE.
  END.
  {&OPEN-QUERY-{&BROWSE-NAME}}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WinPosition C-Win 
PROCEDURE WinPosition :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iX AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iY AS INTEGER    NO-UNDO.
  ASSIGN hFrameHandle = FRAME DEFAULT-FRAME:HANDLE.
/*          {&WINDOW-NAME}:WIDTH-PIXELS  = hFrameHandle:WIDTH-PIXELS   */
/*          {&WINDOW-NAME}:HEIGHT-PIXELS = hFrameHandle:HEIGHT-PIXELS. */
  ASSIGN iX = hParentWin:X + hParentWin:WIDTH-PIXELS - 
                                     hFrameHandle:WIDTH-PIXELS
         iX = IF iX + hFrameHandle:WIDTH-PIXELS > SESSION:WIDTH-PIXELS THEN 
                        SESSION:WIDTH-PIXELS - hFrameHandle:WIDTH-PIXELS ELSE iX
         iX = IF iX < 1 THEN 1 ELSE iX
         iY = hParentWin:Y
         iY = IF hParentWin:Y < 1 THEN 1 ELSE iY
         iY = IF iY + hFrameHandle:HEIGHT-PIXELS > SESSION:HEIGHT-PIXELS THEN 
                        SESSION:HEIGHT-PIXELS - hFrameHandle:HEIGHT-PIXELS ELSE iY.

  ASSIGN THIS-PROCEDURE:CURRENT-WINDOW:X = iX
         THIS-PROCEDURE:CURRENT-WINDOW:Y = iY.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SjekkStr C-Win 
FUNCTION SjekkStr RETURNS LOGICAL
  ( INPUT iStrtypeid AS INTEGER,OUTPUT cStrKoder AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
/*   DEFINE VARIABLE cStrKoder AS CHARACTER  NO-UNDO. */
  cStrkoder = "".
  FIND StrType WHERE Strtype.strtypeId = istrtypeId NO-LOCK NO-ERROR.
  IF AVAIL Strtype THEN DO:
      FOR EACH TT_Strekkode:
          IF NOT CAN-DO(StrType.Fordeling,STRING(TT_StrekKode.StrKode)) THEN
              ASSIGN cStrkoder = cStrkoder + (IF cStrkoder <> "" THEN "," ELSE "") + 
                    STRING(TT_StrekKode.StrKode).
      END.
  END.
  IF NOT AVAIL StrType THEN
      RETURN FALSE.   /* Function return value. */
  ELSE
      RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

