&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE TT_ArtBas NO-UNDO LIKE ArtBas.



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

DEFINE INPUT  PARAMETER ipArtikkelNr LIKE ArtBas.ArtikkelNr    NO-UNDO.
DEFINE INPUT  PARAMETER cGenEan       AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER wForslagLopNr AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER lOppdater     AS LOGICAL    NO-UNDO.
DEFINE VARIABLE dArtikkelnr LIKE ArtBas.ArtikkelNr  NO-UNDO.
DEFINE VARIABLE cTekst AS CHARACTER  NO-UNDO.
DEFINE VARIABLE bOk    AS LOG NO-UNDO.

DEFINE BUFFER bArtBas FOR ArtBas.
DEFINE BUFFER bArtPris FOR ArtPris.
DEFINE BUFFER bTT_ArtBas FOR TT_ArtBas.
DEFINE BUFFER bbArtBas FOR ArtBas.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BROWSE-TT_Varianter

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES TT_ArtBas ArtBas Farg Material

/* Definitions for BROWSE BROWSE-TT_Varianter                           */
&Scoped-define FIELDS-IN-QUERY-BROWSE-TT_Varianter TT_ArtBas.ArtikkelNr ~
TT_ArtBas.Beskr TT_ArtBas.BongTekst TT_ArtBas.Farg getFarg() ~
TT_ArtBas.LevKod TT_ArtBas.LevFargKod 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-TT_Varianter 
&Scoped-define QUERY-STRING-BROWSE-TT_Varianter FOR EACH TT_ArtBas NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-TT_Varianter OPEN QUERY BROWSE-TT_Varianter FOR EACH TT_ArtBas NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-TT_Varianter TT_ArtBas
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-TT_Varianter TT_ArtBas


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BROWSE-TT_Varianter}

/* Definitions for FRAME FRAME-Hoved                                    */
&Scoped-define FIELDS-IN-QUERY-FRAME-Hoved ArtBas.ArtikkelNr ArtBas.Beskr ~
ArtBas.LevKod ArtBas.LevFargKod ArtBas.inn_dato ArtBas.Utgatt ArtBas.IKasse ~
ArtBas.Farg Farg.FarBeskr ArtBas.MatKod Material.MatKod ArtBas.BongTekst ~
ArtBas.LevDato1 ArtBas.LevDato2 
&Scoped-define ENABLED-FIELDS-IN-QUERY-FRAME-Hoved ArtBas.ArtikkelNr ~
ArtBas.Beskr ArtBas.LevKod ArtBas.LevFargKod ArtBas.inn_dato ArtBas.Utgatt ~
ArtBas.IKasse ArtBas.Farg Farg.FarBeskr ArtBas.MatKod Material.MatKod ~
ArtBas.BongTekst ArtBas.LevDato1 ArtBas.LevDato2 
&Scoped-define ENABLED-TABLES-IN-QUERY-FRAME-Hoved ArtBas Farg Material
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-FRAME-Hoved ArtBas
&Scoped-define SECOND-ENABLED-TABLE-IN-QUERY-FRAME-Hoved Farg
&Scoped-define THIRD-ENABLED-TABLE-IN-QUERY-FRAME-Hoved Material
&Scoped-define QUERY-STRING-FRAME-Hoved FOR EACH ArtBas ~
      WHERE ArtBas.ArtikkelNr = ipArtikkelNr NO-LOCK, ~
      EACH Farg OF ArtBas NO-LOCK, ~
      EACH Material OF ArtBas NO-LOCK
&Scoped-define OPEN-QUERY-FRAME-Hoved OPEN QUERY FRAME-Hoved FOR EACH ArtBas ~
      WHERE ArtBas.ArtikkelNr = ipArtikkelNr NO-LOCK, ~
      EACH Farg OF ArtBas NO-LOCK, ~
      EACH Material OF ArtBas NO-LOCK.
&Scoped-define TABLES-IN-QUERY-FRAME-Hoved ArtBas Farg Material
&Scoped-define FIRST-TABLE-IN-QUERY-FRAME-Hoved ArtBas
&Scoped-define SECOND-TABLE-IN-QUERY-FRAME-Hoved Farg
&Scoped-define THIRD-TABLE-IN-QUERY-FRAME-Hoved Material


/* Definitions for FRAME FRAME-Variant                                  */
&Scoped-define FIELDS-IN-QUERY-FRAME-Variant TT_ArtBas.ArtikkelNr ~
TT_ArtBas.Beskr TT_ArtBas.LevKod TT_ArtBas.LevFargKod TT_ArtBas.inn_dato ~
TT_ArtBas.Utgatt TT_ArtBas.IKasse TT_ArtBas.Farg TT_ArtBas.MatKod ~
TT_ArtBas.BongTekst TT_ArtBas.LevDato1 TT_ArtBas.LevDato2 
&Scoped-define ENABLED-FIELDS-IN-QUERY-FRAME-Variant TT_ArtBas.Beskr ~
TT_ArtBas.LevKod TT_ArtBas.LevFargKod TT_ArtBas.Utgatt TT_ArtBas.IKasse ~
TT_ArtBas.Farg TT_ArtBas.MatKod TT_ArtBas.BongTekst TT_ArtBas.LevDato1 ~
TT_ArtBas.LevDato2 
&Scoped-define ENABLED-TABLES-IN-QUERY-FRAME-Variant TT_ArtBas
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-FRAME-Variant TT_ArtBas
&Scoped-define QUERY-STRING-FRAME-Variant FOR EACH TT_ArtBas NO-LOCK
&Scoped-define OPEN-QUERY-FRAME-Variant OPEN QUERY FRAME-Variant FOR EACH TT_ArtBas NO-LOCK.
&Scoped-define TABLES-IN-QUERY-FRAME-Variant TT_ArtBas
&Scoped-define FIRST-TABLE-IN-QUERY-FRAME-Variant TT_ArtBas


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-27 BUTTON-Angre RECT-28 RECT-56 RECT-57 ~
BUTTON-Lagre B-SlettFraModell BUTTON-Ok BUTTON-HentArtBas BUTTON-Slett ~
BUTTON-Ny BROWSE-TT_Varianter FI-FargeTxt FI-HuvTxt 
&Scoped-Define DISPLAYED-OBJECTS FI-FargeTxt FI-HuvTxt 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFarg C-Win 
FUNCTION getFarg RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-SlettFraModell 
     LABEL "Slett fra modell" 
     SIZE 19 BY 1.14.

DEFINE BUTTON Btn_Help DEFAULT 
     IMAGE-UP FILE "icon\e-help":U NO-FOCUS FLAT-BUTTON
     LABEL "&Help" 
     SIZE 4.6 BY 1.05
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-Aktiver 
     LABEL "Aktiver nye" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-Angre 
     IMAGE-UP FILE "icon/reset.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "&Angre" 
     SIZE 4.6 BY 1.05 TOOLTIP "Angre (Alt-A)".

DEFINE BUTTON BUTTON-HentArtBas 
     LABEL "Hent artikkler..." 
     SIZE 17 BY 1.14.

DEFINE BUTTON BUTTON-Lagre 
     IMAGE-UP FILE "icon/saverec.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "&Lagra" 
     SIZE 4.6 BY 1.05 TOOLTIP "Lagre post (Alt-L)".

DEFINE BUTTON BUTTON-Ny 
     IMAGE-UP FILE "icon/add.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "&Ny" 
     SIZE 4.6 BY 1.05 TOOLTIP "Ny post (Alt-N)".

DEFINE BUTTON BUTTON-Ok 
     IMAGE-UP FILE "icon\e-exit":U NO-FOCUS FLAT-BUTTON
     LABEL "&Ok" 
     SIZE 4.6 BY 1.05 TOOLTIP "Lagre og avslutt".

DEFINE BUTTON BUTTON-Slett 
     IMAGE-UP FILE "icon/deleterec.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Radera" 
     SIZE 4.6 BY 1.05 TOOLTIP "Slett post (Alt-D)".

DEFINE VARIABLE FI-FargeTxt AS CHARACTER FORMAT "X(256)":U INITIAL " Fargevariant" 
      VIEW-AS TEXT 
     SIZE 17.4 BY .62
     FGCOLOR 9 FONT 6 NO-UNDO.

DEFINE VARIABLE FI-HuvTxt AS CHARACTER FORMAT "X(256)":U INITIAL " Hovedartikkel farge" 
      VIEW-AS TEXT 
     SIZE 25 BY .62
     FGCOLOR 9 FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-27
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 138.6 BY .1.

DEFINE RECTANGLE RECT-28
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 138.6 BY .1.

DEFINE RECTANGLE RECT-56
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 68 BY 13.57.

DEFINE RECTANGLE RECT-57
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 68 BY 13.57.

DEFINE BUTTON BUTTON-SokFarge 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokMaterial 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-TT_Varianter FOR 
      TT_ArtBas SCROLLING.

DEFINE QUERY FRAME-Hoved FOR 
      ArtBas, 
      Farg, 
      Material SCROLLING.

DEFINE QUERY FRAME-Variant FOR 
      TT_ArtBas SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-TT_Varianter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-TT_Varianter C-Win _STRUCTURED
  QUERY BROWSE-TT_Varianter NO-LOCK DISPLAY
      TT_ArtBas.ArtikkelNr FORMAT "zzzzzzzzzzzz9-":U
      TT_ArtBas.Beskr FORMAT "x(30)":U
      TT_ArtBas.BongTekst FORMAT "X(20)":U
      TT_ArtBas.Farg FORMAT ">>>>9":U WIDTH 8
      getFarg() COLUMN-LABEL "Beskr" FORMAT "x(20)":U WIDTH 15.6
      TT_ArtBas.LevKod FORMAT "x(20)":U
      TT_ArtBas.LevFargKod FORMAT "X(15)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 138 BY 5.81 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BUTTON-Angre AT ROW 15.67 COL 84.6 NO-TAB-STOP 
     Btn_Help AT ROW 15.76 COL 129.6 NO-TAB-STOP 
     BUTTON-Lagre AT ROW 15.67 COL 79.6 NO-TAB-STOP 
     B-SlettFraModell AT ROW 13.14 COL 42
     BUTTON-Ok AT ROW 15.76 COL 134.4 NO-TAB-STOP 
     BUTTON-HentArtBas AT ROW 15.67 COL 20
     BUTTON-Slett AT ROW 15.67 COL 74.6 NO-TAB-STOP 
     BUTTON-Aktiver AT ROW 15.67 COL 91
     BUTTON-Ny AT ROW 15.67 COL 69.6 NO-TAB-STOP 
     BROWSE-TT_Varianter AT ROW 17.38 COL 1.4
     FI-FargeTxt AT ROW 1.24 COL 95 NO-LABEL
     FI-HuvTxt AT ROW 1.29 COL 19 NO-LABEL
     RECT-27 AT ROW 15.52 COL 1
     RECT-28 AT ROW 16.86 COL 1
     RECT-56 AT ROW 1.52 COL 1
     RECT-57 AT ROW 1.52 COL 71
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 138.6 BY 22.29.

DEFINE FRAME FRAME-Variant
     BUTTON-SokFarge AT ROW 8.24 COL 29 NO-TAB-STOP 
     TT_ArtBas.ArtikkelNr AT ROW 1.24 COL 18 COLON-ALIGNED FORMAT "zzzzzzzzzzzz9-"
          VIEW-AS FILL-IN 
          SIZE 21.2 BY 1
     TT_ArtBas.Beskr AT ROW 2.24 COL 18 COLON-ALIGNED FORMAT "x(30)"
          VIEW-AS FILL-IN 
          SIZE 29.4 BY 1
     TT_ArtBas.LevKod AT ROW 3.24 COL 18 COLON-ALIGNED FORMAT "x(200)"
          VIEW-AS FILL-IN 
          SIZE 44 BY 1
     TT_ArtBas.LevFargKod AT ROW 4.24 COL 18 COLON-ALIGNED FORMAT "X(200)"
          VIEW-AS FILL-IN 
          SIZE 44 BY 1
     TT_ArtBas.inn_dato AT ROW 5.24 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     TT_ArtBas.Utgatt AT ROW 6.33 COL 20
          VIEW-AS TOGGLE-BOX
          SIZE 18 BY .81
     TT_ArtBas.IKasse AT ROW 7.24 COL 20
          VIEW-AS TOGGLE-BOX
          SIZE 19 BY .81
     TT_ArtBas.Farg AT ROW 8.24 COL 18 COLON-ALIGNED FORMAT ">>>>9"
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     Farg.FarBeskr AT ROW 8.24 COL 31.6 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 29.4 BY 1
     TT_ArtBas.MatKod AT ROW 9.24 COL 18 COLON-ALIGNED
          LABEL "Material"
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     Material.MatBeskr AT ROW 9.24 COL 31.6 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 29.4 BY 1
     TT_ArtBas.BongTekst AT ROW 10.24 COL 18 COLON-ALIGNED FORMAT "X(20)"
          VIEW-AS FILL-IN 
          SIZE 43 BY 1
     TT_ArtBas.LevDato1 AT ROW 11.24 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     TT_ArtBas.LevDato2 AT ROW 12.24 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     BUTTON-SokMaterial AT ROW 9.24 COL 29 NO-TAB-STOP 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 72 ROW 2
         SIZE 66 BY 12.62.

DEFINE FRAME FRAME-Hoved
     ArtBas.ArtikkelNr AT ROW 1.24 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 21.2 BY 1
     ArtBas.Beskr AT ROW 2.24 COL 18 COLON-ALIGNED FORMAT "x(30)"
          VIEW-AS FILL-IN 
          SIZE 29.4 BY 1
     ArtBas.LevKod AT ROW 3.24 COL 18 COLON-ALIGNED FORMAT "x(200)"
          VIEW-AS FILL-IN 
          SIZE 44 BY 1
     ArtBas.LevFargKod AT ROW 4.24 COL 18 COLON-ALIGNED FORMAT "X(200)"
          VIEW-AS FILL-IN 
          SIZE 44 BY 1
     ArtBas.inn_dato AT ROW 5.24 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     ArtBas.Utgatt AT ROW 6.33 COL 20
          VIEW-AS TOGGLE-BOX
          SIZE 13.4 BY .81
     ArtBas.IKasse AT ROW 7.24 COL 20.2
          VIEW-AS TOGGLE-BOX
          SIZE 20 BY .81
     ArtBas.Farg AT ROW 8.24 COL 18 COLON-ALIGNED FORMAT ">>>>9"
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     Farg.FarBeskr AT ROW 8.24 COL 27 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 35 BY 1
     ArtBas.MatKod AT ROW 9.24 COL 18 COLON-ALIGNED
          LABEL "Material"
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     Material.MatKod AT ROW 9.24 COL 27 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 35 BY 1
     ArtBas.BongTekst AT ROW 10.24 COL 18 COLON-ALIGNED FORMAT "X(20)"
          VIEW-AS FILL-IN 
          SIZE 44 BY 1
     ArtBas.LevDato1 AT ROW 11.24 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     ArtBas.LevDato2 AT ROW 12.24 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 2
         SIZE 66 BY 12.62.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: TT_ArtBas T "?" NO-UNDO skotex ArtBas
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Registrer modell-farge"
         HEIGHT             = 22.29
         WIDTH              = 138.6
         MAX-HEIGHT         = 22.29
         MAX-WIDTH          = 139
         VIRTUAL-HEIGHT     = 22.29
         VIRTUAL-WIDTH      = 139
         MIN-BUTTON         = no
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
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME FRAME-Hoved:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME FRAME-Variant:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB BROWSE-TT_Varianter BUTTON-Ny DEFAULT-FRAME */
/* SETTINGS FOR BUTTON Btn_Help IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-Aktiver IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-FargeTxt IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FI-HuvTxt IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR FRAME FRAME-Hoved
                                                                        */
ASSIGN 
       FRAME FRAME-Hoved:SENSITIVE        = FALSE.

/* SETTINGS FOR FILL-IN ArtBas.Beskr IN FRAME FRAME-Hoved
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN ArtBas.BongTekst IN FRAME FRAME-Hoved
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN ArtBas.Farg IN FRAME FRAME-Hoved
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN ArtBas.LevFargKod IN FRAME FRAME-Hoved
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN ArtBas.LevKod IN FRAME FRAME-Hoved
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN ArtBas.MatKod IN FRAME FRAME-Hoved
   EXP-LABEL                                                            */
/* SETTINGS FOR FRAME FRAME-Variant
                                                                        */
/* SETTINGS FOR FILL-IN TT_ArtBas.ArtikkelNr IN FRAME FRAME-Variant
   NO-ENABLE EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN TT_ArtBas.Beskr IN FRAME FRAME-Variant
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN TT_ArtBas.BongTekst IN FRAME FRAME-Variant
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN Farg.FarBeskr IN FRAME FRAME-Variant
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN TT_ArtBas.Farg IN FRAME FRAME-Variant
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN TT_ArtBas.inn_dato IN FRAME FRAME-Variant
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN TT_ArtBas.LevFargKod IN FRAME FRAME-Variant
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN TT_ArtBas.LevKod IN FRAME FRAME-Variant
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN Material.MatBeskr IN FRAME FRAME-Variant
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN TT_ArtBas.MatKod IN FRAME FRAME-Variant
   EXP-LABEL                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-TT_Varianter
/* Query rebuild information for BROWSE BROWSE-TT_Varianter
     _TblList          = "Temp-Tables.TT_ArtBas"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.TT_ArtBas.ArtikkelNr
"ArtikkelNr" ? "zzzzzzzzzzzz9-" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.TT_ArtBas.Beskr
"Beskr" ? "x(30)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.TT_ArtBas.BongTekst
"BongTekst" ? "X(20)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.TT_ArtBas.Farg
"Farg" ? ">>>>9" "integer" ? ? ? ? ? ? no ? no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > "_<CALC>"
"getFarg()" "Beskr" "x(20)" ? ? ? ? ? ? ? no ? no no "15.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   = Temp-Tables.TT_ArtBas.LevKod
     _FldNameList[7]   = Temp-Tables.TT_ArtBas.LevFargKod
     _Query            is OPENED
*/  /* BROWSE BROWSE-TT_Varianter */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-Hoved
/* Query rebuild information for FRAME FRAME-Hoved
     _TblList          = "skotex.ArtBas,skotex.Farg OF skotex.ArtBas,skotex.Material OF skotex.ArtBas"
     _Options          = "NO-LOCK"
     _Where[1]         = "skotex.ArtBas.ArtikkelNr = ipArtikkelNr"
     _Query            is OPENED
*/  /* FRAME FRAME-Hoved */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-Variant
/* Query rebuild information for FRAME FRAME-Variant
     _TblList          = "Temp-Tables.TT_ArtBas"
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME FRAME-Variant */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Registrer modell-farge */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Registrer modell-farge */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SlettFraModell
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SlettFraModell C-Win
ON CHOOSE OF B-SlettFraModell IN FRAME DEFAULT-FRAME /* Slett fra modell */
DO:
    DEFINE VARIABLE lSlett      AS LOGICAL    NO-UNDO.
    DEFINE VARIABLE lOpploes    AS LOGICAL    NO-UNDO.
    DEFINE VARIABLE dArtikkelNr AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE dNyHovedVare AS DECIMAL    NO-UNDO.
    DEFINE BUFFER bArtBas FOR ArtBas.
    IF CAN-FIND(FIRST TT_ArtBas WHERE TT_ArtBas.ArtikkelNr < 0) THEN DO:
        MESSAGE "Aktiver/slett nye"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.
    IF BROWSE {&BROWSE-NAME}:MAX-DATA-GUESS = 1 THEN DO:
        MESSAGE "Ved sletting fra modell, oppløses modellen." SKIP
                "Oppløs modell?"
          VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lOpploes.
        IF NOT lOpploes THEN
            RETURN NO-APPLY.
        ASSIGN dArtikkelNr  = ArtBas.ArtikkelNr.
    END.
    ELSE IF BROWSE {&BROWSE-NAME}:MAX-DATA-GUESS > 1 THEN DO:
        MESSAGE "Slett fra modell?" SKIP
          VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lSlett.
        IF NOT lSlett THEN
            RETURN NO-APPLY.
        FIND FIRST TT_ArtBas WHERE TT_ArtBas.ArtikkelNr > 0.
        ASSIGN dArtikkelNr  = ArtBas.ArtikkelNr
               dNyHovedVare = TT_ArtBas.ArtikkelNr.
    END.
    IF lOpploes THEN DO:
        FOR EACH bArtBas WHERE bArtBas.ModellFarge = dArtikkelNr:
            ASSIGN bArtBas.ModellFarge = 0
                   bArtBas.HovedModellFarge = FALSE.
        END.
    END.
    ELSE IF lSlett THEN DO:
        FOR EACH bArtBas WHERE bArtBas.ModellFarge = dArtikkelNr:
            IF bArtBas.ArtikkelNr = dArtikkelNr THEN
                ASSIGN bArtBas.ModellFarge = 0
                       bArtBas.HovedModellFarge = FALSE.
            ELSE IF bArtBas.ArtikkelNr = dNyHovedVare THEN
                ASSIGN bArtBas.ModellFarge = dNyHovedVare
                       bArtBas.HovedModellFarge = TRUE.
            ELSE
                ASSIGN bArtBas.ModellFarge = dNyHovedVare
                       bArtBas.HovedModellFarge = FALSE.
        END.
    END.
    FIND CURRENT Artbas NO-LOCK.
    ASSIGN lOppdater = TRUE.
    APPLY "CLOSE" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-TT_Varianter
&Scoped-define SELF-NAME BROWSE-TT_Varianter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-TT_Varianter C-Win
ON VALUE-CHANGED OF BROWSE-TT_Varianter IN FRAME DEFAULT-FRAME
DO:
    DO WITH FRAME FRAME-Variant:
        IF AVAIL TT_ArtBas THEN DO:
            DISPLAY {&FIELDS-IN-QUERY-FRAME-Variant}.
            /*ASSIGN BUTTON-Slett:SENSITIVE = TT_ArtBas.ArtikkelNr < 0.*/
            FIND Farg OF TT_ArtBas NO-LOCK NO-ERROR.
            ASSIGN Farg.FarBeskr:SCREEN-VALUE = IF AVAIL Farg THEN Farg.FarBeskr ELSE "".
            FIND Material OF TT_ArtBas NO-LOCK NO-ERROR.
            ASSIGN Material.MatBeskr:SCREEN-VALUE = IF AVAIL Material THEN Material.MatBeskr ELSE "".
        END.
        ELSE DO:
            CLEAR FRAME FRAME-Variant.
/*             ASSIGN BUTTON-Slett:SENSITIVE = FALSE. */
        END.
        ASSIGN BUTTON-Slett:SENSITIVE        = AVAIL TT_ArtBas
               BUTTON-Ny:SENSITIVE           = TRUE 
               BUTTON-Lagre:SENSITIVE        = FALSE
               BUTTON-Angre:SENSITIVE        = FALSE
               FRAME FRAME-Variant:SENSITIVE = AVAIL TT_ArtBas.

/*         GAMMAL, ovan testas nya versionen med enabling av endra nya                         */
/*         ASSIGN FRAME FRAME-Variant:SENSITIVE = AVAIL TT_ArtBas AND TT_ArtBas.ArtikkelNr < 0 */
/*                BUTTON-Slett:SENSITIVE = FRAME FRAME-Variant:SENSITIVE                       */
/*                BUTTON-Lagre:SENSITIVE = FRAME FRAME-Variant:SENSITIVE.                      */
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help C-Win
ON CHOOSE OF Btn_Help IN FRAME DEFAULT-FRAME /* Help */
OR HELP OF FRAME DEFAULT-FRAME
DO: /* Call Help Function (or a simple message). */
/*    {winhlp.i} */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Aktiver
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Aktiver C-Win
ON CHOOSE OF BUTTON-Aktiver IN FRAME DEFAULT-FRAME /* Aktiver nye */
DO:
  RUN Aktiver.
  ASSIGN SELF:SENSITIVE = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Angre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Angre C-Win
ON CHOOSE OF BUTTON-Angre IN FRAME DEFAULT-FRAME /* Angre */
DO:
    APPLY "VALUE-CHANGED" TO BROWSE {&BROWSE-NAME}.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-HentArtBas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-HentArtBas C-Win
ON CHOOSE OF BUTTON-HentArtBas IN FRAME DEFAULT-FRAME /* Hent artikkler... */
DO:
    DEFINE VARIABLE bOK               AS LOGICAL    NO-UNDO.
    DEFINE VARIABLE cArtBasRowIdList  AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cArtBasIdList     AS CHARACTER  NO-UNDO.

    RUN JBoxSelector.w (THIS-PROCEDURE,0,
                         "ArtBas;Artikkelnr;Beskr;!Vg;!LevNr;!ModellFarge",
                         "where ArtBas.Vg = " + 
                                STRING(ArtBas.Vg) + 
                                " AND ArtBas.LevNr = " + 
                                STRING(ArtBas.LevNr) + " 
                                AND ArtBas.ArtikkelNr <> " + 
                                STRING(ArtBas.Artikkelnr) + 
                                " AND ArtBas.ModellFarge = 0",
                         INPUT-OUTPUT cArtBasRowIdList,
                         "ArtikkelNr",
                         INPUT-OUTPUT cArtBasIdList,
                         "","",
                         OUTPUT bOK).
  IF bOK = FALSE OR cArtBasIdList = "" THEN
        RETURN NO-APPLY.
  RUN AddArtBasToModell (cArtBasIdList).
/*   assign */
/*     FI-ArtBasNr:SCREEN-VALUE = IF cArtBasIdList = ""                          */
/*                       then cAlle                                              */
/*                     else "( " + STRING(NUM-ENTRIES(cArtBasIdList,"|")) + " )" */
/*     FI-ArtBasNr     = if cArtBasIdList = ""                                   */
/*                       then "*"                                                */
/*                       else REPLACE(cArtBasIdList,"|",",")                     */
/*     FI-ArtBasNr:TOOLTIP = IF FI-ArtBasNr = "*" THEN "" ELSE FI-ArtBasNr       */
/*     FI-ArtBasNr:PRIVATE-DATA = cArtBasRowIdList.                              */

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Lagre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Lagre C-Win
ON CHOOSE OF BUTTON-Lagre IN FRAME DEFAULT-FRAME /* Lagra */
DO:
    DO WITH FRAME FRAME-Variant:
        /*IF CAN-FIND(FIRST bTT_ArtBas WHERE bTT_ArtBas.ArtikkelNr <> TT_ArtBas.ArtikkelNr
                    AND bTT_ArtBas.Farg = INPUT TT_ArtBas.Farg) OR 
            ArtBas.Farg = INPUT TT_ArtBas.Farg THEN DO:
            MESSAGE "Fargen er brukt tidligere."
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
            APPLY "ENTRY" TO TT_ArtBas.Farg.
            RETURN NO-APPLY.
        END.
        ELSE */ IF NOT CAN-FIND(Farg WHERE Farg.Farg = INPUT TT_ArtBas.Farg) THEN DO:
            MESSAGE "Farge finnes ikke."
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
            APPLY "ENTRY" TO TT_ArtBas.Farg.
            RETURN NO-APPLY.
        END.
        IF TT_ArtBas.BongTekst:SCREEN-VALUE = "" THEN
            ASSIGN TT_ArtBas.BongTekst:SCREEN-VALUE = SUBSTR(TT_ArtBas.BongTekst:SCREEN-VALUE,1,20).
        ASSIGN INPUT TT_ArtBas.Utgatt 
               INPUT TT_ArtBas.Beskr 
               TT_ArtBas.BongTekst = IF INPUT TT_ArtBas.BongTekst = "" THEN
                         SUBSTR(TT_ArtBas.Beskr:SCREEN-VALUE,1,20) ELSE INPUT TT_ArtBas.BongTekst
               INPUT TT_ArtBas.Farg 
               INPUT TT_ArtBas.IKasse 
               INPUT TT_ArtBas.LevDato1 
               INPUT TT_ArtBas.LevDato2 
               INPUT TT_ArtBas.LevFargKod 
               INPUT TT_ArtBas.LevKod 
               INPUT TT_ArtBas.MatKod.
        ASSIGN BUTTON-Aktiver:SENSITIVE = CAN-FIND(FIRST TT_ArtBas WHERE TT_ArtBas.ArtikkelNr < 0)
               BUTTON-Ny:SENSITIVE      = TRUE
               SELF:SENSITIVE           = FALSE.
        APPLY "VALUE-CHANGED" TO BROWSE {&BROWSE-NAME}.
        BROWSE {&BROWSE-NAME}:REFRESH().
        IF TT_ArtBas.ArtikkelNr > 0 THEN
            RUN LagreIdb.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Ny
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Ny C-Win
ON CHOOSE OF BUTTON-Ny IN FRAME DEFAULT-FRAME /* Ny */
DO:
    DEFINE VARIABLE rRowId AS ROWID      NO-UNDO.
    RUN SkapaModellFarge (OUTPUT rRowId).
    IF rRowId <> ? THEN DO:
        ASSIGN BUTTON-Aktiver:SENSITIVE = FALSE
               BUTTON-Lagre:SENSITIVE   = TRUE.
        {&OPEN-QUERY-{&BROWSE-NAME}}
        REPOSITION {&BROWSE-NAME} TO ROWID rRowId.
        APPLY "VALUE-CHANGED" TO BROWSE {&BROWSE-NAME}.
        ASSIGN FRAME FRAME-Variant:SENSITIVE = TRUE.
        APPLY "ENTRY" TO TT_ArtBas.LevKod IN FRAME FRAME-Variant.
        SELF:SENSITIVE = FALSE.
        RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Ok C-Win
ON CHOOSE OF BUTTON-Ok IN FRAME DEFAULT-FRAME /* Ok */
DO:
    IF ArtBas.ModellFarge = ArtBas.ArtikkelNr AND NOT CAN-FIND(FIRST TT_Artbas WHERE 
                                                        TT_ArtBas.ArtikkelNr > 0) THEN DO:
            FIND CURRENT ArtBas EXCLUSIVE.
            ASSIGN ArtBas.ModellFarge      = 0
                   Artbas.HovedModellFarge = FALSE
                   lOppdater               = TRUE.
    END.
    APPLY "CLOSE":U TO THIS-PROCEDURE.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Slett
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Slett C-Win
ON CHOOSE OF BUTTON-Slett IN FRAME DEFAULT-FRAME /* Radera */
DO:
    DO TRANSACTION:
        IF tt_ArtBas.ArtikkelNr > 0 THEN DO:
            MESSAGE "Ønsker du å slette denne varen fram modellen?"
                VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lSlett AS LOGICAL.
            IF NOT lSlett THEN
                RETURN NO-APPLY.
            FIND bbArtBas EXCLUSIVE-LOCK WHERE
                bbArtBAs.ArtikkelNr = tt_ArtBas.ArtikkelNr NO-ERROR.
/*             IF bbArtBAs.HovedModellFarge THEN                                      */
/*             DO:                                                                    */
/*                 MESSAGE "Kan ikke frikoble artikkel som hovedartikkel fra modell." */
/*                     VIEW-AS ALERT-BOX WARNING BUTTONS OK.                          */
/*                 RETURN NO-APPLY.                                                   */
/*             END.                                                                   */
            IF AVAILABLE bbArtBas THEN
            DO:
                ASSIGN
                    bbArtBas.ModellFarge = 0
                    bbArtBAs.HovedModellFarge = FALSE
                    lOppdater            = TRUE
                    .
                RELEASE bbArtBas.
            END.
        END.
        DELETE TT_ArtBas.
        BROWSE {&BROWSE-NAME}:DELETE-CURRENT-ROW().
        APPLY "ENTRY" TO BROWSE {&BROWSE-NAME}.
        APPLY "VALUE-CHANGED" TO BROWSE {&BROWSE-NAME}.
        ASSIGN BUTTON-Aktiver:SENSITIVE = 
                    CAN-FIND(FIRST TT_ArtBas WHERE TT_ArtBas.ArtikkelNr < 0)
               BUTTON-Ny:SENSITIVE = TRUE.
        IF BROWSE {&BROWSE-NAME}:FOCUSED-ROW = ? THEN DO:
            FIND CURRENT ArtBas.
            ASSIGN ArtBas.ModellFarge      = 0
                   ArtBAs.HovedModellFarge = FALSE.
            FIND CURRENT ArtBas NO-LOCK.
        END.
    END.
/*            FRAME FRAME-Variant:SENSITIVE = BROWSE {&BROWSE-NAME}:FOCUSED-ROW <> ?. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-Variant
&Scoped-define SELF-NAME BUTTON-SokFarge
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokFarge C-Win
ON CHOOSE OF BUTTON-SokFarge IN FRAME FRAME-Variant /* ... */
or F10 of TT_ArtBas.Farg
DO:
    DO WITH FRAME FRAME-Variant:
      cTekst = "Farg,FarBeskr".
      THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
      RUN JBoxLookup.w (THIS-PROCEDURE,50,
                    "Farg;FarBeskr;Farg|Farge|>>>>9"
                    ,"WHERE TRUE"
                    ,""                                                  
                    ,"Farg,FarBeskr",   /* <- return values for these fields */
                    OUTPUT cTekst,
                    OUTPUT bOK).
      THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

      IF cTekst = '' THEN DO:
          APPLY "ENTRY" TO TT_ArtBas.Farg.      
          RETURN NO-APPLY.
      END.
      IF NUM-ENTRIES(cTekst,'|') >= 2 THEN DO:
          /* Legger opp verdier I de aktuelle feltene */
          IF ENTRY(1,cTekst,'|') = TT_ArtBas.Farg:SCREEN-VALUE THEN
              RETURN NO-APPLY.
          IF INT(ENTRY(1,cTekst,'|')) = ArtBas.Farg OR
                 CAN-FIND(FIRST TT_ArtBas WHERE TT_ArtBas.Farg = INT(ENTRY(1,cTekst,'|'))) THEN DO:
              MESSAGE "Fargen er brukt tidligere. Ønsker du fargen?"
                  VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lSamme AS LOGICAL.
              APPLY "ENTRY" TO TT_ArtBas.Farg.
              IF NOT lSamme THEN
                  RETURN NO-APPLY.
          END.
          ASSIGN TT_ArtBas.Farg:SCREEN-VALUE = ENTRY(1,cTekst,'|')
                 Farg.FarBeskr:SCREEN-VALUE  = ENTRY(2,cTekst,'|').
          IF TT_ArtBas.Farg:SCREEN-VALUE <> STRING(TT_ArtBas.Farg) THEN
              APPLY "VALUE-CHANGED" TO TT_ArtBas.Farg.
      END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokMaterial
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokMaterial C-Win
ON CHOOSE OF BUTTON-SokMaterial IN FRAME FRAME-Variant /* ... */
or F10 of TT_ArtBas.MatKod
DO:
    DO WITH FRAME FRAME-Variant:
      THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
      RUN JBoxLookup.w (THIS-PROCEDURE,50,
                    "Material;MatBeskr;MatKod|Material|>9"
                    ,"WHERE TRUE"
                    ,""                                                  
                    ,"MatKod,Matbeskr",   /* <- return values for these fields */
                    OUTPUT cTekst,
                    OUTPUT bOK).
      THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

      IF cTekst = '' THEN DO:
          APPLY "ENTRY" TO TT_ArtBas.MatKod.
          RETURN NO-APPLY.
      END.
      IF NUM-ENTRIES(cTekst,'|') >= 2 THEN DO:
          /* Legger opp verdier I de aktuelle feltene */
          ASSIGN
            TT_ArtBas.MatKod:SCREEN-VALUE   = ENTRY(1,cTekst,'|')
            Material.MatBeskr:SCREEN-VALUE IN FRAME FRAME-Variant = ENTRY(2,cTekst,'|').
            IF TT_ArtBas.MatKod:SCREEN-VALUE <> STRING(TT_ArtBas.MatKod) THEN
              APPLY "VALUE-CHANGED" TO TT_ArtBas.MatKod.
      END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
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

ON "VALUE-CHANGED" OF FRAME {&FRAME-NAME} ANYWHERE DO:
    ASSIGN BUTTON-Lagre:SENSITIVE IN FRAME DEFAULT-FRAME = TRUE.
    IF TT_ArtBas.ArtikkelNr > 0 THEN
        ASSIGN BUTTON-Ny:SENSITIVE = FALSE
               BUTTON-Angre:SENSITIVE = TRUE.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    RUN Init_TT.
  RUN enable_UI.
  B-SlettFraModell:MOVE-TO-TOP().
  {lng.i} /* Oversettelse */
  APPLY "VALUE-CHANGED" TO BROWSE {&BROWSE-NAME}.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AddArtBasToModell C-Win 
PROCEDURE AddArtBasToModell :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT  PARAMETER cArtBasList AS CHARACTER  NO-UNDO.
   DEFINE        VARIABLE  iCount      AS INTEGER    NO-UNDO.
   DEFINE        VARIABLE  lNy         AS LOGICAL    NO-UNDO.
   DO TRANSACTION:
       DO iCount = 1 TO NUM-ENTRIES(cArtBasList,"|"):
           FIND bArtBas WHERE bArtBas.ArtikkelNr = DECI(ENTRY(iCount,cArtBasList,"|")).
           ASSIGN bArtBas.ModellFarge = ArtBas.ArtikkelNr
                  lNy = TRUE.
           IF ArtBas.ModellFarge <> 0 THEN DO:
               RELEASE TT_ArtBas.
               BUFFER-COPY bArtBas TO TT_ArtBas.
               RELEASE TT_ArtBas.
           END.
       END.
       RELEASE bArtBas.
       IF lNy AND ArtBas.ModellFarge = 0 THEN DO:
           FIND CURRENT ArtBas.
           ASSIGN ArtBas.ModellFarge = ArtBas.Artikkelnr
                  ArtBas.HovedModellFarge = TRUE.
           FIND CURRENT ArtBas NO-LOCK.
           RUN Init_TT.
       END.
       IF lNy THEN
           ASSIGN lOppdater = TRUE.
   END.
   {&OPEN-QUERY-{&BROWSE-NAME}}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Aktiver C-Win 
PROCEDURE Aktiver :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iLopNr AS INTEGER INIT ?   NO-UNDO.
    DO TRANSACTION:
        IF ArtBas.ModellFarge <> ArtBas.ArtikkelNr THEN DO:
            FIND CURRENT ArtBas EXCLUSIVE.
            ASSIGN ArtBas.ModellFarge      = ArtBas.ArtikkelNr
                   Artbas.HovedModellFarge = TRUE.
        END.
        FOR EACH TT_ArtBas WHERE TT_ArtBas.ArtikkelNr < 0.
            RELEASE bArtBas.
            BUFFER-COPY TT_ArtBas EXCEPT ArtikkelNr LopNr BildNr TO bArtBas.
            IF wForslagLopNr <> "" THEN
                RUN SettLopNr.p (bArtBas.Vg,wForslagLopNr,OUTPUT iLopNr).
            ASSIGN bArtBas.LopNr = iLopNr
                   TT_ArtBas.ArtikkelNr = bArtBas.ArtikkelNr
                   .
            FOR EACH ArtPris OF ArtBas NO-LOCK:
                RELEASE bArtPris.
                BUFFER-COPY ArtPris EXCEPT ArtikkelNr TO bArtPris.
                ASSIGN bArtPris.ArtikkelNr = bArtBas.ArtikkelNr.
            END.
            if bArtBas.Lager = TRUE THEN
                for each Butiker no-lock break by Butiker.ProfilNr:
                    create Lager.
                    ASSIGN Lager.ArtikkelNr = bArtBas.ArtikkelNr
                           Lager.Butik      = Butiker.Butik.                    
                    RELEASE Lager.
            end. /* BUTIKKLOOP */
/*             IF cGenEan = "yes" OR CAN-FIND(StrTstr WHERE StrTypeId = bArtBas.StrTypeId) THEN */
            IF cGenEan = "yes" THEN
                RUN genStrekKode.p (bArtBas.ArtikkelNr,1,"").
            ELSE
                RUN genStrekKode.p (bArtBas.ArtikkelNr,0,"").
            ASSIGN lOppdater = TRUE.
            BROWSE {&BROWSE-NAME}:REFRESH().
        END.
        APPLY "VALUE-CHANGED" TO BROWSE {&BROWSE-NAME}.
    END.
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
  DISPLAY FI-FargeTxt FI-HuvTxt 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-27 BUTTON-Angre RECT-28 RECT-56 RECT-57 BUTTON-Lagre 
         B-SlettFraModell BUTTON-Ok BUTTON-HentArtBas BUTTON-Slett BUTTON-Ny 
         BROWSE-TT_Varianter FI-FargeTxt FI-HuvTxt 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}

  {&OPEN-QUERY-FRAME-Hoved}
  GET FIRST FRAME-Hoved.
  IF AVAILABLE ArtBas THEN 
    DISPLAY ArtBas.ArtikkelNr ArtBas.Beskr ArtBas.LevKod ArtBas.LevFargKod 
          ArtBas.inn_dato ArtBas.Utgatt ArtBas.IKasse ArtBas.Farg ArtBas.MatKod 
          ArtBas.BongTekst ArtBas.LevDato1 ArtBas.LevDato2 
      WITH FRAME FRAME-Hoved IN WINDOW C-Win.
  IF AVAILABLE Farg THEN 
    DISPLAY Farg.FarBeskr 
      WITH FRAME FRAME-Hoved IN WINDOW C-Win.
  IF AVAILABLE Material THEN 
    DISPLAY Material.MatKod 
      WITH FRAME FRAME-Hoved IN WINDOW C-Win.
  ENABLE ArtBas.ArtikkelNr ArtBas.Beskr ArtBas.LevKod ArtBas.LevFargKod 
         ArtBas.inn_dato ArtBas.Utgatt ArtBas.IKasse ArtBas.Farg Farg.FarBeskr 
         ArtBas.MatKod Material.MatKod ArtBas.BongTekst ArtBas.LevDato1 
         ArtBas.LevDato2 
      WITH FRAME FRAME-Hoved IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-Hoved}
  FRAME FRAME-Hoved:SENSITIVE = NO.
  IF AVAILABLE Farg THEN 
    DISPLAY Farg.FarBeskr 
      WITH FRAME FRAME-Variant IN WINDOW C-Win.
  IF AVAILABLE Material THEN 
    DISPLAY Material.MatBeskr 
      WITH FRAME FRAME-Variant IN WINDOW C-Win.
  IF AVAILABLE TT_ArtBas THEN 
    DISPLAY TT_ArtBas.ArtikkelNr TT_ArtBas.Beskr TT_ArtBas.LevKod 
          TT_ArtBas.LevFargKod TT_ArtBas.inn_dato TT_ArtBas.Utgatt 
          TT_ArtBas.IKasse TT_ArtBas.Farg TT_ArtBas.MatKod TT_ArtBas.BongTekst 
          TT_ArtBas.LevDato1 TT_ArtBas.LevDato2 
      WITH FRAME FRAME-Variant IN WINDOW C-Win.
  ENABLE BUTTON-SokFarge TT_ArtBas.Beskr TT_ArtBas.LevKod TT_ArtBas.LevFargKod 
         TT_ArtBas.Utgatt TT_ArtBas.IKasse TT_ArtBas.Farg TT_ArtBas.MatKod 
         TT_ArtBas.BongTekst TT_ArtBas.LevDato1 TT_ArtBas.LevDato2 
         BUTTON-SokMaterial 
      WITH FRAME FRAME-Variant IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-Variant}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Init_TT C-Win 
PROCEDURE Init_TT :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.
    RELEASE TT_ArtBas.
    FOR EACH bArtBas NO-LOCK WHERE bArtBas.ModellFarge = ipArtikkelNr AND
                  bArtBas.ArtikkelNr <> ipArtikkelNr AND NOT CAN-FIND(TT_artBas WHERE TT_ArtBas.Artikkelnr = bArtBas.Artikkelnr):
        BUFFER-COPY bArtBas TO TT_ArtBas NO-ERROR.
        RELEASE TT_ArtBas.
        ASSIGN iCount = iCount + 1.
    END.
/*     BROWSE {&BROWSE-NAME}:MAX-DATA-GUESS = iCount. */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LagreIdb C-Win 
PROCEDURE LagreIdb :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE BUFFER bArtBas FOR ArtBas.
    FIND bArtBas WHERE bArtBas.ArtikkelNr = TT_ArtBas.ArtikkelNr NO-ERROR.
    IF AVAIL bArtBas THEN DO:
        BUFFER-COPY TT_ArtBas USING Utgatt Beskr BongTekst Farg IKasse LevDato1 
                                    LevDato2 LevFargKod LevKod MatKod TO bArtBas.
        RELEASE bArtBas.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapaModellFarge C-Win 
PROCEDURE SkapaModellFarge :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER rRowId AS ROWID      NO-UNDO.
    dArtikkelNr = dArtikkelNr - 1.
    RELEASE TT_ArtBas.
    CREATE TT_ArtBas.
    BUFFER-COPY ArtBas EXCEPT ArtikkelNr HovedModellFarge Farg LopNr inn_dato LevFargKod TO TT_ArtBas.
    ASSIGN TT_ArtBas.ArtikkelNr  = dArtikkelNr
           TT_ArtBas.ModellFarge = ArtBas.ArtikkelNr
           TT_ArtBas.Farg        = 0
           TT_ArtBas.LopNr       = ?
           TT_ArtBas.inn_dato    = ?
           rRowId = ROWID(TT_ArtBas).
    RELEASE TT_ArtBas.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFarg C-Win 
FUNCTION getFarg RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND Farg WHERE Farg.Farg = TT_ArtBas.Farg NO-LOCK NO-ERROR.
  RETURN IF AVAIL Farg THEN Farg.FarBeskr ELSE "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

