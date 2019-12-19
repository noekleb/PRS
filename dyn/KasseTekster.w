&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: Basert på C:\NSoft\JukeBox\winsrc\samples\ExtentFieldUsage.w 

  Description:        Container for a JukeBox window program

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author:            tomn@polygon.se

  Created:           28.nov.2019

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
&IF "{1}" = "Developer_Studio_is_Running" &THEN
  &SCOPED-DEFINE UIB_is_Running 1 
&ENDIF   

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
/* &SCOPED-DEFINE PureABLWin 1  */

DEF VAR bOk         AS LOG    NO-UNDO.
DEF VAR ix          AS INT    NO-UNDO.
DEF VAR hBrowse     AS HANDLE NO-UNDO.
DEF VAR hQuery      AS HANDLE NO-UNDO.
DEF VAR hToolbar    AS HANDLE NO-UNDO.
DEF VAR hFieldMap   AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS brwKasseTekster tbKasseTekster ButikkNr ~
KasseNavn GruppeNr KasseNr TekstHode_1 TekstHStil_1 TekstHode_2 ~
TekstHStil_2 TekstHode_3 TekstHStil_3 TekstHode_4 TekstHStil_4 TekstHode_5 ~
TekstHStil_5 TekstHode_6 TekstHStil_6 TekstHode_7 TekstHStil_7 TekstHode_8 ~
TekstHStil_8 TekstHode_9 TekstHStil_9 TekstHode_10 TekstHStil_10 ~
TekstSlutt_1 TekstSStil_1 TekstSlutt_2 TekstSStil_2 TekstSlutt_3 ~
TekstSStil_3 TekstSlutt_4 TekstSStil_4 TekstSlutt_5 TekstSStil_5 ~
TekstSlutt_6 TekstSStil_6 TekstSlutt_7 TekstSStil_7 TekstSlutt_8 ~
TekstSStil_8 TekstSlutt_9 TekstSStil_9 TekstSlutt_10 TekstSStil_10 
&Scoped-Define DISPLAYED-OBJECTS ButikkNr KasseNavn GruppeNr KasseNr ~
TekstHode_1 TekstHStil_1 TekstHode_2 TekstHStil_2 TekstHode_3 TekstHStil_3 ~
TekstHode_4 TekstHStil_4 TekstHode_5 TekstHStil_5 TekstHode_6 TekstHStil_6 ~
TekstHode_7 TekstHStil_7 TekstHode_8 TekstHStil_8 TekstHode_9 TekstHStil_9 ~
TekstHode_10 TekstHStil_10 TekstSlutt_1 TekstSStil_1 TekstSlutt_2 ~
TekstSStil_2 TekstSlutt_3 TekstSStil_3 TekstSlutt_4 TekstSStil_4 ~
TekstSlutt_5 TekstSStil_5 TekstSlutt_6 TekstSStil_6 TekstSlutt_7 ~
TekstSStil_7 TekstSlutt_8 TekstSStil_8 TekstSlutt_9 TekstSStil_9 ~
TekstSlutt_10 TekstSStil_10 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE ButikkNr AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     LABEL "Butikknr" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 TOOLTIP "Butikknummer".

DEFINE VARIABLE GruppeNr AS INTEGER FORMAT ">9":U INITIAL 1 
     LABEL "Gruppenr" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 TOOLTIP "Gruppenummer" NO-UNDO.

DEFINE VARIABLE KasseNavn AS CHARACTER FORMAT "x(50)" 
     VIEW-AS FILL-IN 
     SIZE 51 BY 1 TOOLTIP "Navn på kassen".

DEFINE VARIABLE KasseNr AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Kassenr" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 TOOLTIP "Kassenummer" NO-UNDO.

DEFINE VARIABLE TekstHode_1 AS CHARACTER FORMAT "X(60)":U 
     LABEL "Linje 1" 
     VIEW-AS FILL-IN 
     SIZE 65.8 BY 1 NO-UNDO.

DEFINE VARIABLE TekstHode_10 AS CHARACTER FORMAT "X(60)":U 
     LABEL "Linje 10" 
     VIEW-AS FILL-IN 
     SIZE 65.8 BY 1 NO-UNDO.

DEFINE VARIABLE TekstHode_2 AS CHARACTER FORMAT "X(60)":U 
     LABEL "Linje 2" 
     VIEW-AS FILL-IN 
     SIZE 65.8 BY 1 NO-UNDO.

DEFINE VARIABLE TekstHode_3 AS CHARACTER FORMAT "X(60)":U 
     LABEL "Linje 3" 
     VIEW-AS FILL-IN 
     SIZE 65.8 BY 1 NO-UNDO.

DEFINE VARIABLE TekstHode_4 AS CHARACTER FORMAT "X(60)":U 
     LABEL "Linje 4" 
     VIEW-AS FILL-IN 
     SIZE 65.8 BY 1 NO-UNDO.

DEFINE VARIABLE TekstHode_5 AS CHARACTER FORMAT "X(60)":U 
     LABEL "Linje 5" 
     VIEW-AS FILL-IN 
     SIZE 65.8 BY 1 NO-UNDO.

DEFINE VARIABLE TekstHode_6 AS CHARACTER FORMAT "X(60)":U 
     LABEL "Linje 6" 
     VIEW-AS FILL-IN 
     SIZE 65.8 BY 1 NO-UNDO.

DEFINE VARIABLE TekstHode_7 AS CHARACTER FORMAT "X(60)":U 
     LABEL "Linje 7" 
     VIEW-AS FILL-IN 
     SIZE 65.8 BY 1 NO-UNDO.

DEFINE VARIABLE TekstHode_8 AS CHARACTER FORMAT "X(60)":U 
     LABEL "Linje 8" 
     VIEW-AS FILL-IN 
     SIZE 65.8 BY 1 NO-UNDO.

DEFINE VARIABLE TekstHode_9 AS CHARACTER FORMAT "X(60)":U 
     LABEL "Linje 9" 
     VIEW-AS FILL-IN 
     SIZE 65.8 BY 1 NO-UNDO.

DEFINE VARIABLE TekstSlutt_1 AS CHARACTER FORMAT "X(60)":U 
     LABEL "Linje 1" 
     VIEW-AS FILL-IN 
     SIZE 65.8 BY 1 NO-UNDO.

DEFINE VARIABLE TekstSlutt_10 AS CHARACTER FORMAT "X(60)":U 
     LABEL "Linje 10" 
     VIEW-AS FILL-IN 
     SIZE 65.8 BY 1 NO-UNDO.

DEFINE VARIABLE TekstSlutt_2 AS CHARACTER FORMAT "X(60)":U 
     LABEL "Linje 2" 
     VIEW-AS FILL-IN 
     SIZE 65.8 BY 1 NO-UNDO.

DEFINE VARIABLE TekstSlutt_3 AS CHARACTER FORMAT "X(60)":U 
     LABEL "Linje 3" 
     VIEW-AS FILL-IN 
     SIZE 65.8 BY 1 NO-UNDO.

DEFINE VARIABLE TekstSlutt_4 AS CHARACTER FORMAT "X(60)":U 
     LABEL "Linje 4" 
     VIEW-AS FILL-IN 
     SIZE 65.8 BY 1 NO-UNDO.

DEFINE VARIABLE TekstSlutt_5 AS CHARACTER FORMAT "X(60)":U 
     LABEL "Linje 5" 
     VIEW-AS FILL-IN 
     SIZE 65.8 BY 1 NO-UNDO.

DEFINE VARIABLE TekstSlutt_6 AS CHARACTER FORMAT "X(60)":U 
     LABEL "Linje 6" 
     VIEW-AS FILL-IN 
     SIZE 65.8 BY 1 NO-UNDO.

DEFINE VARIABLE TekstSlutt_7 AS CHARACTER FORMAT "X(60)":U 
     LABEL "Linje 7" 
     VIEW-AS FILL-IN 
     SIZE 65.8 BY 1 NO-UNDO.

DEFINE VARIABLE TekstSlutt_8 AS CHARACTER FORMAT "X(60)":U 
     LABEL "Linje 8" 
     VIEW-AS FILL-IN 
     SIZE 65.8 BY 1 NO-UNDO.

DEFINE VARIABLE TekstSlutt_9 AS CHARACTER FORMAT "X(60)":U 
     LABEL "Linje 9" 
     VIEW-AS FILL-IN 
     SIZE 65.8 BY 1 NO-UNDO.

DEFINE VARIABLE TekstHStil_1 AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N", 1,
"F", 2,
"Bmp", 3
     SIZE 23.6 BY 1 TOOLTIP "N=Normal, F=Fetstil, Bmp=Bitmap." NO-UNDO.

DEFINE VARIABLE TekstHStil_10 AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N", 1,
"F", 2,
"Bmp", 3
     SIZE 23.6 BY 1 TOOLTIP "N=Normal, F=Fetstil, Bmp=Bitmap." NO-UNDO.

DEFINE VARIABLE TekstHStil_2 AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N", 1,
"F", 2,
"Bmp", 3
     SIZE 23.6 BY 1 TOOLTIP "N=Normal, F=Fetstil, Bmp=Bitmap." NO-UNDO.

DEFINE VARIABLE TekstHStil_3 AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N", 1,
"F", 2,
"Bmp", 3
     SIZE 23.6 BY 1 TOOLTIP "N=Normal, F=Fetstil, Bmp=Bitmap." NO-UNDO.

DEFINE VARIABLE TekstHStil_4 AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N", 1,
"F", 2,
"Bmp", 3
     SIZE 23.6 BY 1 TOOLTIP "N=Normal, F=Fetstil, Bmp=Bitmap." NO-UNDO.

DEFINE VARIABLE TekstHStil_5 AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N", 1,
"F", 2,
"Bmp", 3
     SIZE 23.6 BY 1 TOOLTIP "N=Normal, F=Fetstil, Bmp=Bitmap." NO-UNDO.

DEFINE VARIABLE TekstHStil_6 AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N", 1,
"F", 2,
"Bmp", 3
     SIZE 23.6 BY 1 TOOLTIP "N=Normal, F=Fetstil, Bmp=Bitmap." NO-UNDO.

DEFINE VARIABLE TekstHStil_7 AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N", 1,
"F", 2,
"Bmp", 3
     SIZE 23.6 BY 1 TOOLTIP "N=Normal, F=Fetstil, Bmp=Bitmap." NO-UNDO.

DEFINE VARIABLE TekstHStil_8 AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N", 1,
"F", 2,
"Bmp", 3
     SIZE 23.6 BY 1 TOOLTIP "N=Normal, F=Fetstil, Bmp=Bitmap." NO-UNDO.

DEFINE VARIABLE TekstHStil_9 AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N", 1,
"F", 2,
"Bmp", 3
     SIZE 23.6 BY 1 TOOLTIP "N=Normal, F=Fetstil, Bmp=Bitmap." NO-UNDO.

DEFINE VARIABLE TekstSStil_1 AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N", 1,
"F", 2,
"Bmp", 3
     SIZE 23.6 BY 1 TOOLTIP "N=Normal, F=Fetstil, Bmp=Bitmap." NO-UNDO.

DEFINE VARIABLE TekstSStil_10 AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N", 1,
"F", 2,
"Bmp", 3
     SIZE 23.6 BY 1 TOOLTIP "N=Normal, F=Fetstil, Bmp=Bitmap." NO-UNDO.

DEFINE VARIABLE TekstSStil_2 AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N", 1,
"F", 2,
"Bmp", 3
     SIZE 23.6 BY 1 TOOLTIP "N=Normal, F=Fetstil, Bmp=Bitmap." NO-UNDO.

DEFINE VARIABLE TekstSStil_3 AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N", 1,
"F", 2,
"Bmp", 3
     SIZE 23.6 BY 1 TOOLTIP "N=Normal, F=Fetstil, Bmp=Bitmap." NO-UNDO.

DEFINE VARIABLE TekstSStil_4 AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N", 1,
"F", 2,
"Bmp", 3
     SIZE 23.6 BY 1 TOOLTIP "N=Normal, F=Fetstil, Bmp=Bitmap." NO-UNDO.

DEFINE VARIABLE TekstSStil_5 AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N", 1,
"F", 2,
"Bmp", 3
     SIZE 23.6 BY 1 TOOLTIP "N=Normal, F=Fetstil, Bmp=Bitmap." NO-UNDO.

DEFINE VARIABLE TekstSStil_6 AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N", 1,
"F", 2,
"Bmp", 3
     SIZE 23.6 BY 1 TOOLTIP "N=Normal, F=Fetstil, Bmp=Bitmap." NO-UNDO.

DEFINE VARIABLE TekstSStil_7 AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N", 1,
"F", 2,
"Bmp", 3
     SIZE 23.6 BY 1 TOOLTIP "N=Normal, F=Fetstil, Bmp=Bitmap." NO-UNDO.

DEFINE VARIABLE TekstSStil_8 AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N", 1,
"F", 2,
"Bmp", 3
     SIZE 23.6 BY 1 TOOLTIP "N=Normal, F=Fetstil, Bmp=Bitmap." NO-UNDO.

DEFINE VARIABLE TekstSStil_9 AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N", 1,
"F", 2,
"Bmp", 3
     SIZE 23.6 BY 1 TOOLTIP "N=Normal, F=Fetstil, Bmp=Bitmap." NO-UNDO.

DEFINE RECTANGLE brwKasseTekster
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88 BY 25.29.

DEFINE RECTANGLE tbKasseTekster
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 9 BY .95.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     ButikkNr AT ROW 2.67 COL 103 COLON-ALIGNED HELP
          "Angi butikknummer"
     KasseNavn AT ROW 2.67 COL 117.2 COLON-ALIGNED HELP
          "Please enter the Name of the Salesperson." NO-LABEL
     GruppeNr AT ROW 3.71 COL 103 COLON-ALIGNED
     KasseNr AT ROW 4.76 COL 103 COLON-ALIGNED
     TekstHode_1 AT ROW 6.86 COL 103.2 COLON-ALIGNED
     TekstHStil_1 AT ROW 6.86 COL 172 NO-LABEL WIDGET-ID 4
     TekstHode_2 AT ROW 7.86 COL 103.2 COLON-ALIGNED
     TekstHStil_2 AT ROW 7.86 COL 172 NO-LABEL WIDGET-ID 16
     TekstHode_3 AT ROW 8.86 COL 103.2 COLON-ALIGNED
     TekstHStil_3 AT ROW 8.86 COL 172 NO-LABEL WIDGET-ID 22
     TekstHode_4 AT ROW 9.86 COL 103.2 COLON-ALIGNED
     TekstHStil_4 AT ROW 9.86 COL 172 NO-LABEL WIDGET-ID 28
     TekstHode_5 AT ROW 10.86 COL 103.2 COLON-ALIGNED
     TekstHStil_5 AT ROW 10.86 COL 172 NO-LABEL WIDGET-ID 34
     TekstHode_6 AT ROW 11.86 COL 103.2 COLON-ALIGNED
     TekstHStil_6 AT ROW 11.86 COL 172 NO-LABEL WIDGET-ID 40
     TekstHode_7 AT ROW 12.86 COL 103.2 COLON-ALIGNED
     TekstHStil_7 AT ROW 12.86 COL 172 NO-LABEL WIDGET-ID 46
     TekstHode_8 AT ROW 13.86 COL 103.2 COLON-ALIGNED
     TekstHStil_8 AT ROW 13.86 COL 172 NO-LABEL WIDGET-ID 52
     TekstHode_9 AT ROW 14.86 COL 103.2 COLON-ALIGNED
     TekstHStil_9 AT ROW 14.86 COL 172 NO-LABEL WIDGET-ID 58
     TekstHode_10 AT ROW 15.86 COL 103.2 COLON-ALIGNED
     TekstHStil_10 AT ROW 15.86 COL 172 NO-LABEL WIDGET-ID 64
     TekstSlutt_1 AT ROW 17.95 COL 103.2 COLON-ALIGNED
     TekstSStil_1 AT ROW 17.95 COL 172 NO-LABEL WIDGET-ID 8
     TekstSlutt_2 AT ROW 18.95 COL 103.2 COLON-ALIGNED
     TekstSStil_2 AT ROW 18.95 COL 172 NO-LABEL WIDGET-ID 72
     TekstSlutt_3 AT ROW 19.95 COL 103.2 COLON-ALIGNED
     TekstSStil_3 AT ROW 19.95 COL 172 NO-LABEL WIDGET-ID 78
     TekstSlutt_4 AT ROW 20.95 COL 103.2 COLON-ALIGNED
     TekstSStil_4 AT ROW 20.95 COL 172 NO-LABEL WIDGET-ID 84
     TekstSlutt_5 AT ROW 21.95 COL 103.2 COLON-ALIGNED
     TekstSStil_5 AT ROW 21.95 COL 172 NO-LABEL WIDGET-ID 90
     TekstSlutt_6 AT ROW 22.86 COL 103.2 COLON-ALIGNED
     TekstSStil_6 AT ROW 22.86 COL 172 NO-LABEL WIDGET-ID 96
     TekstSlutt_7 AT ROW 23.86 COL 103.2 COLON-ALIGNED
     TekstSStil_7 AT ROW 23.86 COL 172 NO-LABEL WIDGET-ID 102
     TekstSlutt_8 AT ROW 24.86 COL 103.2 COLON-ALIGNED
     TekstSStil_8 AT ROW 24.86 COL 172 NO-LABEL WIDGET-ID 108
     TekstSlutt_9 AT ROW 25.86 COL 103.2 COLON-ALIGNED
     TekstSStil_9 AT ROW 25.86 COL 172 NO-LABEL WIDGET-ID 114
     TekstSlutt_10 AT ROW 26.86 COL 103.2 COLON-ALIGNED
     TekstSStil_10 AT ROW 26.86 COL 172 NO-LABEL WIDGET-ID 120
     "Tekster på bunnen av bongen" VIEW-AS TEXT
          SIZE 64.2 BY .62 AT ROW 17.24 COL 105.8 WIDGET-ID 68
          FONT 6
     "Tekster på toppen av bongen" VIEW-AS TEXT
          SIZE 64.2 BY .62 AT ROW 6.05 COL 105.8 WIDGET-ID 12
          FONT 6
     brwKasseTekster AT ROW 2.62 COL 2
     tbKasseTekster AT ROW 1.29 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 196.2 BY 27.24.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Vedlikehold av kassens topp og bunntekster."
         HEIGHT             = 27.14
         WIDTH              = 196.2
         MAX-HEIGHT         = 31.14
         MAX-WIDTH          = 198.2
         VIRTUAL-HEIGHT     = 31.14
         VIRTUAL-WIDTH      = 198.2
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
ASSIGN 
       GruppeNr:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "1".

ASSIGN 
       TekstHStil_1:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "1".

ASSIGN 
       TekstHStil_10:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "1".

ASSIGN 
       TekstHStil_2:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "1".

ASSIGN 
       TekstHStil_3:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "1".

ASSIGN 
       TekstHStil_4:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "1".

ASSIGN 
       TekstHStil_5:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "1".

ASSIGN 
       TekstHStil_6:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "1".

ASSIGN 
       TekstHStil_7:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "1".

ASSIGN 
       TekstHStil_8:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "1".

ASSIGN 
       TekstHStil_9:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "1".

ASSIGN 
       TekstSStil_1:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "1".

ASSIGN 
       TekstSStil_10:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "1".

ASSIGN 
       TekstSStil_2:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "1".

ASSIGN 
       TekstSStil_3:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "1".

ASSIGN 
       TekstSStil_4:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "1".

ASSIGN 
       TekstSStil_5:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "1".

ASSIGN 
       TekstSStil_6:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "1".

ASSIGN 
       TekstSStil_7:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "1".

ASSIGN 
       TekstSStil_8:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "1".

ASSIGN 
       TekstSStil_9:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "1".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = YES.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Vedlikehold av kassens topp og bunntekster. */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Vedlikehold av kassens topp og bunntekster. */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
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
  IF NOT DYNAMIC-FUNCTION("DoCleanUpObjects",THIS-PROCEDURE:CURRENT-WINDOW) THEN RETURN NO-APPLY.
  DYNAMIC-FUNCTION("SetCleanUpResize",THIS-PROCEDURE:CURRENT-WINDOW).
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

  &IF DEFINED(UIB_is_Running) NE 0 &THEN
    RUN InitializeObject.
    RUN MoveToTop.
    &IF DEFINED(PureABLWin) = 1 &THEN
      IF PROVERSION GE "10.2" THEN RUN WaitForForm.
    &ENDIF
  &ENDIF

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BeforeDynAccum C-Win 
PROCEDURE BeforeDynAccum :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihBrowse AS HANDLE NO-UNDO. 
DEF INPUT PARAM icAction AS CHAR NO-UNDO.

IF ihBrowse NE hBrowse THEN RETURN.

IF icAction = "accum" THEN DO:
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"shadedRows","yes").
  hBrowse:SEPARATORS = NO.
END.
ELSE DO:
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"shadedRows","no").
  hBrowse:SEPARATORS = YES.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplayRecord C-Win 
PROCEDURE DisplayRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN SUPER.
ButikkNr:READ-ONLY IN FRAME {&FRAME-NAME} = YES.
GruppeNr:READ-ONLY IN FRAME {&FRAME-NAME} = YES.
KasseNr:READ-ONLY IN FRAME {&FRAME-NAME} = YES.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DynAccumDone C-Win 
PROCEDURE DynAccumDone :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihBrowse AS HANDLE NO-UNDO. 
DEF INPUT PARAM icAction AS CHAR NO-UNDO.

IF ihBrowse NE hBrowse THEN RETURN.

IF icAction = "accum" THEN DO:
  DYNAMIC-FUNCTION("setAttribute",hToolbar,"disabledEvents","new,copy,delete,save,undo").
  DYNAMIC-FUNCTION("DeleteObjectLink",hBrowse,hFieldMap).
  DYNAMIC-FUNCTION("ViewHideFieldMap",hFieldMap,NO).
END.
ELSE DO:
  DYNAMIC-FUNCTION("setAttribute",hToolbar,"disabledEvents","").
  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hFieldMap).
  DYNAMIC-FUNCTION("ViewHideFieldMap",hFieldMap,YES).
END.

RUN InvokeMethod(hBrowse,"DisplayRecord").
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
  DISPLAY ButikkNr KasseNavn GruppeNr KasseNr TekstHode_1 TekstHStil_1 
          TekstHode_2 TekstHStil_2 TekstHode_3 TekstHStil_3 TekstHode_4 
          TekstHStil_4 TekstHode_5 TekstHStil_5 TekstHode_6 TekstHStil_6 
          TekstHode_7 TekstHStil_7 TekstHode_8 TekstHStil_8 TekstHode_9 
          TekstHStil_9 TekstHode_10 TekstHStil_10 TekstSlutt_1 TekstSStil_1 
          TekstSlutt_2 TekstSStil_2 TekstSlutt_3 TekstSStil_3 TekstSlutt_4 
          TekstSStil_4 TekstSlutt_5 TekstSStil_5 TekstSlutt_6 TekstSStil_6 
          TekstSlutt_7 TekstSStil_7 TekstSlutt_8 TekstSStil_8 TekstSlutt_9 
          TekstSStil_9 TekstSlutt_10 TekstSStil_10 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE brwKasseTekster tbKasseTekster ButikkNr KasseNavn GruppeNr KasseNr 
         TekstHode_1 TekstHStil_1 TekstHode_2 TekstHStil_2 TekstHode_3 
         TekstHStil_3 TekstHode_4 TekstHStil_4 TekstHode_5 TekstHStil_5 
         TekstHode_6 TekstHStil_6 TekstHode_7 TekstHStil_7 TekstHode_8 
         TekstHStil_8 TekstHode_9 TekstHStil_9 TekstHode_10 TekstHStil_10 
         TekstSlutt_1 TekstSStil_1 TekstSlutt_2 TekstSStil_2 TekstSlutt_3 
         TekstSStil_3 TekstSlutt_4 TekstSStil_4 TekstSlutt_5 TekstSStil_5 
         TekstSlutt_6 TekstSStil_6 TekstSlutt_7 TekstSStil_7 TekstSlutt_8 
         TekstSStil_8 TekstSlutt_9 TekstSStil_9 TekstSlutt_10 TekstSStil_10 
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
  Notes:       Refer to the <jukebox>\winsrc\samples for working examples for Sports2000
------------------------------------------------------------------------------*/
RUN enable_UI.

DYNAMIC-FUNCTION("CreateStatusBar",THIS-PROCEDURE:CURRENT-WINDOW,"",0,YES,?).

DO WITH FRAME {&FRAME-NAME}:

  SUBSCRIBE TO "DynAccumDone" ANYWHERE.
  SUBSCRIBE TO "BeforeDynAccum" ANYWHERE.

  hBrowse = DYNAMIC-FUNCTION("NewBrowse"
  ,brwKasseTekster:HANDLE
  ,100
  ,""
  ,"KasseTekster"
   + ";ButikkNr"
   + ";GruppNr"
   + ";KasseNr"
   + ";KasseNavn"
   + ";TekstHode[1]|Linje 1"
   + ";TekstHode[2]|Linje 2"
   + ";TekstHode[3]|Linje 3"
   + ";TekstHode[4]|Linje 4"
   + ";TekstHode[5]|Linje 5"
   + ";TekstHode[6]|Linje 6"
   + ";TekstHode[7]|Linje 7"
   + ";TekstHode[8]|Linje 8"
   + ";TekstHode[9]|Linje 9"
   + ";TekstHode[10]|Linje 10"
   + ";TekstSlutt[1]|Linje 1"
   + ";TekstSlutt[2]|Linje 2"
   + ";TekstSlutt[3]|Linje 3"
   + ";TekstSlutt[4]|Linje 4"
   + ";TekstSlutt[5]|Linje 5"
   + ";TekstSlutt[6]|Linje 6"
   + ";TekstSlutt[7]|Linje 7"
   + ";TekstSlutt[8]|Linje 8"
   + ";TekstSlutt[9]|Linje 9"
   + ";TekstSlutt[10]|Linje 10"
   
   + ";TekstHStil[1]"
   + ";TekstHStil[2]"
   + ";TekstHStil[3]"
   + ";TekstHStil[4]"
   + ";TekstHStil[5]"
   + ";TekstHStil[6]"
   + ";TekstHStil[7]"
   + ";TekstHStil[8]"
   + ";TekstHStil[9]"
   + ";TekstHStil[10]"
   
   + ";TekstSStil[1]"
   + ";TekstSStil[2]"
   + ";TekstSStil[3]"
   + ";TekstSStil[4]"
   + ";TekstSStil[5]"
   + ";TekstSStil[6]"
   + ";TekstSStil[7]"
   + ";TekstSStil[8]"
   + ";TekstSStil[9]"
   + ";TekstSStil[10]"
  ,"WHERE false"
  ,"").

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"accumDataTypes","integer,decimal").

  DYNAMIC-FUNCTION("NewMenuBand",hBrowse
      ,"MultiSortBrowse;Sort on multiple columns"
      ,"").

  hToolbar = DYNAMIC-FUNCTION("NewToolBar"
    ,tbKasseTekster:HANDLE
    ,"File"
    ,"new,copy,undo,delete,save"
   + ",Filter,excel;E&xcel"
    ,"maxborder").

  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hToolbar).

  hFieldMap = DYNAMIC-FUNCTION("NewFieldMap"
  ,hBrowse:QUERY
  ,FRAME {&FRAME-NAME}:HANDLE
  ,"ButikkNr,KasseNavn,GruppeNr,KasseNr" +
     ",TekstHode[1],TekstHode[2],TekstHode[3],TekstHode[4],TekstHode[5],TekstHode[6],TekstHode[7],TekstHode[8],TekstHode[9],TekstHode[10]" + 
     ",TekstSlutt[1],TekstSlutt[2],TekstSlutt[3],TekstSlutt[4],TekstSlutt[5],TekstSlutt[6],TekstSlutt[7],TekstSlutt[8],TekstSlutt[9],TekstSlutt[10]" +
     ",TekstHStil[1],TekstHStil[2],TekstHStil[3],TekstHStil[4],TekstHStil[5],TekstHStil[6],TekstHStil[7],TekstHStil[8],TekstHStil[9],TekstHStil[10]" + 
     ",TekstSStil[1],TekstSStil[2],TekstSStil[3],TekstSStil[4],TekstSStil[5],TekstSStil[6],TekstSStil[7],TekstSStil[8],TekstSStil[9],TekstSStil[10]",
   "ButikkNr,KasseNavn,GruppeNr,KasseNr" + 
     ",TekstHode_1,TekstHode_2,TekstHode_3,TekstHode_4,TekstHode_5,TekstHode_6,TekstHode_7,TekstHode_8,TekstHode_9,TekstHode_10" + 
     ",TekstSlutt_1,TekstSlutt_2,TekstSlutt_3,TekstSlutt_4,TekstSlutt_5,TekstSlutt_6,TekstSlutt_7,TekstSlutt_8,TekstSlutt_9,TekstSlutt_10" + 
     ",TekstHStil_1,TekstHStil_2,TekstHStil_3,TekstHStil_4,TekstHStil_5,TekstHStil_6,TekstHStil_7,TekstHStil_8,TekstHStil_9,TekstHStil_10" + 
     ",TekstSStil_1,TekstSStil_2,TekstSStil_3,TekstSStil_4,TekstSStil_5,TekstSStil_6,TekstSStil_7,TekstSStil_8,TekstSStil_9,TekstSStil_10"
  ,"",""
  ,"").

  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hFieldmap).
  DYNAMIC-FUNCTION("CreateObjectLink",hToolbar,hFieldmap).

  DYNAMIC-FUNCTION("BrwOrQryToFMapTranslation",hBrowse) NO-ERROR.
  
  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"customUpdateValProc","ignore"). 
  
END.


DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,200,100,0,0).

RUN InvokeMethod(hBrowse,"OpenQuery").

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
&IF DEFINED(PureABLWin) = 1 &THEN
  IF PROVERSION GE "10.2" THEN RUN ShowForm ("").
&ENDIF

THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = NO.
THIS-PROCEDURE:CURRENT-WINDOW:WINDOW-STATE = 3.
THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().

APPLY "entry" TO hBrowse.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

