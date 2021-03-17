&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          data             PROGRESS
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BR-prchdata

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES prCHData prFSData prBSData

/* Definitions for BROWSE BR-prchdata                                   */
&Scoped-define FIELDS-IN-QUERY-BR-prchdata prCHData.ButikkNr prCHData.Dato ~
prCHData.Hg prCHData.Vg prCHData.ArtikkelNr prCHData.AntSolgt "" 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BR-prchdata 
&Scoped-define QUERY-STRING-BR-prchdata FOR EACH prCHData ~
      WHERE prCHData.ButikkNr = FI-ButikkNr ~
 AND prCHData.Dato = FI-DatoFr NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BR-prchdata OPEN QUERY BR-prchdata FOR EACH prCHData ~
      WHERE prCHData.ButikkNr = FI-ButikkNr ~
 AND prCHData.Dato = FI-DatoFr NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BR-prchdata prCHData
&Scoped-define FIRST-TABLE-IN-QUERY-BR-prchdata prCHData


/* Definitions for BROWSE BR-prfsdata                                   */
&Scoped-define FIELDS-IN-QUERY-BR-prfsdata prFSData.ButikkNr prFSData.Dato ~
prFSData.AvdelingsNr prFSData.Hg prFSData.Vg prFSData.fsgnto prFSData.MvaKr ~
prFSData.DbKr prFSData.AntSolgtNto prFSData.AntSolgtPLU "" 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BR-prfsdata 
&Scoped-define QUERY-STRING-BR-prfsdata FOR EACH prFSData ~
      WHERE prFSData.ButikkNr = FI-ButikkNr ~
 AND prFSData.Dato = FI-DatoFr NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BR-prfsdata OPEN QUERY BR-prfsdata FOR EACH prFSData ~
      WHERE prFSData.ButikkNr = FI-ButikkNr ~
 AND prFSData.Dato = FI-DatoFr NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BR-prfsdata prFSData
&Scoped-define FIRST-TABLE-IN-QUERY-BR-prfsdata prFSData


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BR-prchdata}~
    ~{&OPEN-QUERY-BR-prfsdata}
&Scoped-define QUERY-STRING-DEFAULT-FRAME FOR EACH prBSData ~
      WHERE prBSData.ButikkNr = FI-ButikkNr ~
 AND prBSData.Dato = FI-DatoFr NO-LOCK
&Scoped-define OPEN-QUERY-DEFAULT-FRAME OPEN QUERY DEFAULT-FRAME FOR EACH prBSData ~
      WHERE prBSData.ButikkNr = FI-ButikkNr ~
 AND prBSData.Dato = FI-DatoFr NO-LOCK.
&Scoped-define TABLES-IN-QUERY-DEFAULT-FRAME prBSData
&Scoped-define FIRST-TABLE-IN-QUERY-DEFAULT-FRAME prBSData


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BUTTON-Next FI-ButikkNr FI-DatoFr BUTTON-1 ~
BR-prchdata BR-prfsdata BUTTON-Prev 
&Scoped-Define DISPLAYED-OBJECTS FI-ButikkNr FI-ButNamn FI-DatoFr 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-1 
     LABEL "Visa data" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-Next 
     IMAGE-UP FILE "icon/next.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Neste" 
     SIZE 4.6 BY 1.05 TOOLTIP "Neste post (Alt-PilNed)".

DEFINE BUTTON BUTTON-Prev 
     IMAGE-UP FILE "icon/prev.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Forrige" 
     SIZE 4.6 BY 1.05 TOOLTIP "Forrige post (Alt-PilOpp)".

DEFINE VARIABLE FI-ButikkNr AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     LABEL "Butikknummer" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-ButNamn AS CHARACTER FORMAT "x(20)" 
     VIEW-AS FILL-IN 
     SIZE 41 BY 1.

DEFINE VARIABLE FI-DatoFr AS DATE FORMAT "99/99/99" 
     LABEL "Datum" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BR-prchdata FOR 
      prCHData SCROLLING.

DEFINE QUERY BR-prfsdata FOR 
      prFSData SCROLLING.

DEFINE QUERY DEFAULT-FRAME FOR 
      prBSData SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BR-prchdata
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BR-prchdata C-Win _STRUCTURED
  QUERY BR-prchdata NO-LOCK DISPLAY
      prCHData.ButikkNr FORMAT ">>>>>9":U
      prCHData.Dato FORMAT "99/99/99":U
      prCHData.Hg FORMAT ">>>9":U
      prCHData.Vg FORMAT ">>>>>9":U
      prCHData.ArtikkelNr FORMAT "X(20)":U
      prCHData.AntSolgt FORMAT "->>>,>>9.999":U WIDTH 18.4
      ""
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 80.6 BY 2.71 EXPANDABLE.

DEFINE BROWSE BR-prfsdata
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BR-prfsdata C-Win _STRUCTURED
  QUERY BR-prfsdata NO-LOCK DISPLAY
      prFSData.ButikkNr FORMAT ">>>>>9":U
      prFSData.Dato FORMAT "99/99/99":U
      prFSData.AvdelingsNr FORMAT ">>>>>9":U
      prFSData.Hg FORMAT ">>>9":U
      prFSData.Vg FORMAT ">>>>>9":U
      prFSData.fsgnto FORMAT "->>>,>>9.999":U
      prFSData.MvaKr FORMAT "->>>,>>9.99":U
      prFSData.DbKr FORMAT "->>>,>>9.99":U
      prFSData.AntSolgtNto FORMAT "->>>,>>9.999":U
      prFSData.AntSolgtPLU FORMAT "->>>,>>9.999":U WIDTH 15
      ""
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 115 BY 21.91 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BUTTON-Next AT ROW 3.62 COL 36.6 NO-TAB-STOP 
     FI-ButikkNr AT ROW 2.19 COL 16 COLON-ALIGNED HELP
          "Butikknummer."
     FI-ButNamn AT ROW 2.19 COL 31 COLON-ALIGNED HELP
          "Butikkens navn" NO-LABEL
     FI-DatoFr AT ROW 3.62 COL 15.4 COLON-ALIGNED HELP
          "Dato for utstedelse av bongen"
     BUTTON-1 AT ROW 3.62 COL 49
     BR-prchdata AT ROW 6.48 COL 3
     BR-prfsdata AT ROW 9.81 COL 3
     BUTTON-Prev AT ROW 3.62 COL 32 NO-TAB-STOP 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 170 BY 31.57.

DEFINE FRAME FRAME-VisData
     prBSData.ButikkNr AT ROW 1.95 COL 21 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     prBSData.Dato AT ROW 2.95 COL 21 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     prBSData.TotSalg AT ROW 3.95 COL 21 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 21.6 BY 1
     prBSData.MvaKr AT ROW 4.95 COL 21 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 21.6 BY 1
     prBSData.AntKvittoInne AT ROW 5.95 COL 21 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     prBSData.AntKvittoUte AT ROW 6.95 COL 21 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     prBSData.TotDrvm AT ROW 7.95 COL 21 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 21.6 BY 1
     prBSData.MvaDrm AT ROW 8.95 COL 21 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 21.6 BY 1
     prBSData.DrivmInne AT ROW 9.95 COL 21 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 21.6 BY 1
     prBSData.bm01 AT ROW 10.95 COL 21 COLON-ALIGNED
          LABEL "Cash"
          VIEW-AS FILL-IN 
          SIZE 21.6 BY 1
     prBSData.bm01Ant AT ROW 11.95 COL 21 COLON-ALIGNED
          LABEL "Cash antal"
          VIEW-AS FILL-IN 
          SIZE 23 BY 1
     prBSData.bm02 AT ROW 12.95 COL 21 COLON-ALIGNED
          LABEL "Card"
          VIEW-AS FILL-IN 
          SIZE 21.6 BY 1
     prBSData.bm02ant AT ROW 13.95 COL 21 COLON-ALIGNED
          LABEL "Card antal"
          VIEW-AS FILL-IN 
          SIZE 23 BY 1
     prBSData.bm03 AT ROW 14.95 COL 21 COLON-ALIGNED
          LABEL "Other credit"
          VIEW-AS FILL-IN 
          SIZE 21.6 BY 1
     prBSData.bm03ant AT ROW 15.95 COL 21 COLON-ALIGNED
          LABEL "Other credit antal"
          VIEW-AS FILL-IN 
          SIZE 23 BY 1
     prBSData.bm04 AT ROW 16.95 COL 21 COLON-ALIGNED
          LABEL "Drive off"
          VIEW-AS FILL-IN 
          SIZE 21.6 BY 1
     prBSData.bm04and AT ROW 17.95 COL 21 COLON-ALIGNED
          LABEL "Drive off antal"
          VIEW-AS FILL-IN 
          SIZE 23 BY 1
     prBSData.bm05 AT ROW 18.95 COL 21 COLON-ALIGNED
          LABEL "In/Out tendering"
          VIEW-AS FILL-IN 
          SIZE 21.6 BY 1
     prBSData.bm05ant AT ROW 19.95 COL 21 COLON-ALIGNED
          LABEL "In/Out antal"
          VIEW-AS FILL-IN 
          SIZE 23 BY 1
     prBSData.bm06 AT ROW 20.95 COL 21 COLON-ALIGNED
          LABEL "Station credit"
          VIEW-AS FILL-IN 
          SIZE 21.6 BY 1
     prBSData.bm06ant AT ROW 21.95 COL 21 COLON-ALIGNED
          LABEL "Station cred antal"
          VIEW-AS FILL-IN 
          SIZE 23 BY 1
     prBSData.bm07 AT ROW 22.95 COL 21 COLON-ALIGNED
          LABEL "Round off"
          VIEW-AS FILL-IN 
          SIZE 21.6 BY 1
     prBSData.bm07ant AT ROW 23.95 COL 21 COLON-ALIGNED
          LABEL "Round off antal"
          VIEW-AS FILL-IN 
          SIZE 23 BY 1
     prBSData.bm08 AT ROW 24.95 COL 21 COLON-ALIGNED
          LABEL "Loyalty card"
          VIEW-AS FILL-IN 
          SIZE 21.6 BY 1
     prBSData.bm08ant AT ROW 25.95 COL 21 COLON-ALIGNED
          LABEL "Loyalty antal"
          VIEW-AS FILL-IN 
          SIZE 23 BY 1
     prBSData.bm09 AT ROW 26.95 COL 21 COLON-ALIGNED
          LABEL "Cash back"
          VIEW-AS FILL-IN 
          SIZE 21.6 BY 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 121 ROW 1.48
         SIZE 49 BY 30.24.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME FRAME-VisData
     prBSData.bm09ant AT ROW 27.95 COL 21 COLON-ALIGNED
          LABEL "Cash back antal"
          VIEW-AS FILL-IN 
          SIZE 23 BY 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 121 ROW 1.48
         SIZE 49 BY 30.24
         TITLE "Betalmedel".


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
         TITLE              = "Visa VS-data"
         HEIGHT             = 31.57
         WIDTH              = 170
         MAX-HEIGHT         = 31.57
         MAX-WIDTH          = 204.8
         VIRTUAL-HEIGHT     = 31.57
         VIRTUAL-WIDTH      = 204.8
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
/* REPARENT FRAME */
ASSIGN FRAME FRAME-VisData:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME FRAME-VisData:MOVE-BEFORE-TAB-ITEM (FI-ButikkNr:HANDLE IN FRAME DEFAULT-FRAME)
/* END-ASSIGN-TABS */.

/* BROWSE-TAB BR-prchdata BUTTON-1 DEFAULT-FRAME */
/* BROWSE-TAB BR-prfsdata BR-prchdata DEFAULT-FRAME */
/* SETTINGS FOR FILL-IN FI-ButNamn IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME FRAME-VisData
                                                                        */
ASSIGN 
       FRAME FRAME-VisData:SENSITIVE        = FALSE.

/* SETTINGS FOR FILL-IN prBSData.bm01 IN FRAME FRAME-VisData
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN prBSData.bm01Ant IN FRAME FRAME-VisData
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN prBSData.bm02 IN FRAME FRAME-VisData
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN prBSData.bm02ant IN FRAME FRAME-VisData
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN prBSData.bm03 IN FRAME FRAME-VisData
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN prBSData.bm03ant IN FRAME FRAME-VisData
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN prBSData.bm04 IN FRAME FRAME-VisData
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN prBSData.bm04and IN FRAME FRAME-VisData
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN prBSData.bm05 IN FRAME FRAME-VisData
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN prBSData.bm05ant IN FRAME FRAME-VisData
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN prBSData.bm06 IN FRAME FRAME-VisData
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN prBSData.bm06ant IN FRAME FRAME-VisData
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN prBSData.bm07 IN FRAME FRAME-VisData
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN prBSData.bm07ant IN FRAME FRAME-VisData
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN prBSData.bm08 IN FRAME FRAME-VisData
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN prBSData.bm08ant IN FRAME FRAME-VisData
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN prBSData.bm09 IN FRAME FRAME-VisData
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN prBSData.bm09ant IN FRAME FRAME-VisData
   EXP-LABEL                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BR-prchdata
/* Query rebuild information for BROWSE BR-prchdata
     _TblList          = "data.prCHData"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "data.prCHData.ButikkNr = FI-ButikkNr
 AND data.prCHData.Dato = FI-DatoFr"
     _FldNameList[1]   = data.prCHData.ButikkNr
     _FldNameList[2]   = data.prCHData.Dato
     _FldNameList[3]   = data.prCHData.Hg
     _FldNameList[4]   = data.prCHData.Vg
     _FldNameList[5]   = data.prCHData.ArtikkelNr
     _FldNameList[6]   > data.prCHData.AntSolgt
"prCHData.AntSolgt" ? ? "decimal" ? ? ? ? ? ? no ? no no "18.4" yes no no "U" "" ""
     _FldNameList[7]   > "_<CALC>"
"""""" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BR-prchdata */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BR-prfsdata
/* Query rebuild information for BROWSE BR-prfsdata
     _TblList          = "data.prFSData"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "data.prFSData.ButikkNr = FI-ButikkNr
 AND data.prFSData.Dato = FI-DatoFr"
     _FldNameList[1]   = data.prFSData.ButikkNr
     _FldNameList[2]   = data.prFSData.Dato
     _FldNameList[3]   = data.prFSData.AvdelingsNr
     _FldNameList[4]   = data.prFSData.Hg
     _FldNameList[5]   = data.prFSData.Vg
     _FldNameList[6]   = data.prFSData.fsgnto
     _FldNameList[7]   = data.prFSData.MvaKr
     _FldNameList[8]   = data.prFSData.DbKr
     _FldNameList[9]   = data.prFSData.AntSolgtNto
     _FldNameList[10]   > data.prFSData.AntSolgtPLU
"prFSData.AntSolgtPLU" ? ? "decimal" ? ? ? ? ? ? no ? no no "15" yes no no "U" "" ""
     _FldNameList[11]   > "_<CALC>"
"""""" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BR-prfsdata */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _TblList          = "data.prBSData"
     _Options          = "NO-LOCK"
     _Where[1]         = "data.prBSData.ButikkNr = FI-ButikkNr
 AND data.prBSData.Dato = FI-DatoFr"
     _Query            is OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Visa VS-data */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Visa VS-data */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 C-Win
ON CHOOSE OF BUTTON-1 IN FRAME DEFAULT-FRAME /* Visa data */
DO:
    DEFINE VARIABLE dDatoLoop AS DATE       NO-UNDO.
    ASSIGN FI-ButikkNr 
           FI-DatoFr.
    FIND Butiker WHERE Butiker.butik = FI-ButikkNr NO-LOCK NO-ERROR.
    IF NOT AVAIL Butiker THEN DO:
        MESSAGE
            "Finner inte butiken"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN.
    END.
    FI-ButNamn:SCREEN-VALUE = Butiker.butnamn.
    PROCESS EVENTS.
    IF FI-DatoFr = ? THEN DO:
        MESSAGE 
             "Fel dato"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.
    {&OPEN-QUERY-BR-prbsdata}
    {&OPEN-QUERY-BR-prchdata}
    {&OPEN-QUERY-BR-prfsdata}
/*     {&OPEN-QUERY-DEFAULT-FRAME} */
        FIND prBSdata WHERE prBSdata.butikknr = FI-butikknr AND prBSData.dato = FI-Datofr NO-LOCK NO-ERROR.
        IF AVAILABLE prBSData THEN 
      DISPLAY prBSData.ButikkNr prBSData.Dato prBSData.TotSalg prBSData.MvaKr 
            prBSData.AntKvittoInne prBSData.AntKvittoUte prBSData.TotDrvm 
            prBSData.MvaDrm prBSData.DrivmInne prBSData.bm01 prBSData.bm01Ant 
            prBSData.bm02 prBSData.bm02ant prBSData.bm03 prBSData.bm03ant 
            prBSData.bm04 prBSData.bm04and prBSData.bm05 prBSData.bm05ant 
            prBSData.bm06 prBSData.bm06ant prBSData.bm07 prBSData.bm07ant 
            prBSData.bm08 prBSData.bm08ant prBSData.bm09 prBSData.bm09ant 
        WITH FRAME Frame-Visdata IN WINDOW C-Win.
        ELSE
            CLEAR FRAME Frame-Visdata.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Next
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Next C-Win
ON CHOOSE OF BUTTON-Next IN FRAME DEFAULT-FRAME /* Neste */
DO:
  /* Det er ikke lov å bytte artikkel hvis artikkelen ikke har en ArtPrisPost. */
    ASSIGN FI-DatoFr.
    IF FI-DatoFr <> ? THEN DO:
        FI-DatoFr = FI-DatoFr + 1.
        DISP FI-DatoFr WITH FRAME DEFAULT-FRAME.
        APPLY "CHOOSE" TO BUTTON-1.
    END.
  RETURN NO-APPLY.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Prev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Prev C-Win
ON CHOOSE OF BUTTON-Prev IN FRAME DEFAULT-FRAME /* Forrige */
DO:
  /* Det er ikke lov å bytte artikkel hvis artikkelen ikke har en ArtPrisPost. */
    ASSIGN FI-DatoFr.
    IF FI-DatoFr <> ? THEN DO:
        FI-DatoFr = FI-DatoFr - 1.
        DISP FI-DatoFr WITH FRAME DEFAULT-FRAME.
        APPLY "CHOOSE" TO BUTTON-1.
    END.
  RETURN NO-APPLY.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BR-prchdata
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
  APPLY "ENTRY" TO FI-Butikknr IN FRAME DEFAULT-FRAME.
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

  {&OPEN-QUERY-DEFAULT-FRAME}
  GET FIRST DEFAULT-FRAME.
  DISPLAY FI-ButikkNr FI-ButNamn FI-DatoFr 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE BUTTON-Next FI-ButikkNr FI-DatoFr BUTTON-1 BR-prchdata BR-prfsdata 
         BUTTON-Prev 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  IF AVAILABLE prBSData THEN 
    DISPLAY prBSData.ButikkNr prBSData.Dato prBSData.TotSalg prBSData.MvaKr 
          prBSData.AntKvittoInne prBSData.AntKvittoUte prBSData.TotDrvm 
          prBSData.MvaDrm prBSData.DrivmInne prBSData.bm01 prBSData.bm01Ant 
          prBSData.bm02 prBSData.bm02ant prBSData.bm03 prBSData.bm03ant 
          prBSData.bm04 prBSData.bm04and prBSData.bm05 prBSData.bm05ant 
          prBSData.bm06 prBSData.bm06ant prBSData.bm07 prBSData.bm07ant 
          prBSData.bm08 prBSData.bm08ant prBSData.bm09 prBSData.bm09ant 
      WITH FRAME FRAME-VisData IN WINDOW C-Win.
  ENABLE prBSData.ButikkNr prBSData.Dato prBSData.TotSalg prBSData.MvaKr 
         prBSData.AntKvittoInne prBSData.AntKvittoUte prBSData.TotDrvm 
         prBSData.MvaDrm prBSData.DrivmInne prBSData.bm01 prBSData.bm01Ant 
         prBSData.bm02 prBSData.bm02ant prBSData.bm03 prBSData.bm03ant 
         prBSData.bm04 prBSData.bm04and prBSData.bm05 prBSData.bm05ant 
         prBSData.bm06 prBSData.bm06ant prBSData.bm07 prBSData.bm07ant 
         prBSData.bm08 prBSData.bm08ant prBSData.bm09 prBSData.bm09ant 
      WITH FRAME FRAME-VisData IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-VisData}
  FRAME FRAME-VisData:SENSITIVE = NO.
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

