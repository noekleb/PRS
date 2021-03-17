&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: Etikettutskrift NO db

  Notes: Genväg/Snarvei: 
  "C:\Program\Progress Software\WebClient\bin\prowc.exe" -p w-etikettNOdb.w  -param ".\etikett.tmpdb,LPT4,.\kom\pgm\etisend1.bat"

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
DEF VAR wTekst1      AS CHAR NO-UNDO.
DEF VAR wTekst2      AS CHAR NO-UNDO.
def var wCl          as int  no-undo.
def var wSeqNr       as int  no-undo.
def var wLayout      as char no-undo.
def var wBatch       as char no-undo.
def var wEtikett_Fil as char INIT "etikett.dat" no-undo.
def var wFirma       as char no-undo.
def var Linje        as char extent 50.
def var BLinje       as char extent 50.
def var TStorlek     as char format "x(4)".
def var Teller       as int  no-undo.

/* Diverse flagg. */
def var i              as int  no-undo.

/* ENV-variabler (utan c) */
def var cETIKETTSEND   AS char INIT ".\kom\pgm\etisend1.bat" NO-UNDO.
DEF VAR cETIKETTPORT   AS CHAR INIT "COM1"                   NO-UNDO.
DEF VAR cETIKETTDB     AS CHARACTER INIT ".\etikett.tmpdb"   NO-UNDO.

DEF VAR cPrisgruppfil  AS CHAR INIT ".\prisgrupper.txt" NO-UNDO.
DEF VAR cArtikeltyper  AS CHAR NO-UNDO.

DEF VAR cPRINTER       AS CHARACTER NO-UNDO.

/* temp-table som vi skriver ut från */
def temp-table TT_EtikettLogg NO-UNDO
       FIELD EAN AS CHAR FORMAT "x(13)"
      field Vg        AS INTE
      field LopNr     AS INTE
      field Ant       as int
      field Storl     as char
      field bongtekst as char
      field Pris      as dec format "-zzz,zz9.99"
      field pris2     as dec format "-zzz,zz9.99"
      FIELD individ   AS DEC DECIMALS 0
      FIELD Plunr     AS INTE
      field SeqNr     as int
      INDEX Seqnr IS PRIMARY Seqnr.

/* db på disk in i temp-table */
DEFINE TEMP-TABLE TT_Artikkel NO-UNDO
    FIELD kode     AS DECI FORMAT ">>>>>>>>>>>>9"
    FIELD vg       AS INTE
    FIELD lopnr    AS INTE
    FIELD storlek  AS CHAR
    FIELD pris     AS DECI
    FIELD bongtekst   AS CHAR
    FIELD antal    AS INTE
    FIELD levinnr  AS INTE
    INDEX vglop IS PRIMARY vg lopnr
    INDEX kode  kode.


DEFINE TEMP-TABLE TT_prisgrupper NO-UNDO
    FIELD Artikeltyp AS CHAR
    FIELD PluNr      AS INTE
    FIELD Pris       AS DECI
    INDEX ArtPris IS PRIMARY UNIQUE Artikeltyp Pris.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BROWSE-1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES TT_EtikettLogg

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 EAN Vg LopNr Storl Ant /* field bongtekst as char */ Pris /* field pris2 as dec */ /* FIELD individ AS DEC DECIMALS 0 */ /* field SeqNr as int. */   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1   
&Scoped-define SELF-NAME BROWSE-1
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH TT_EtikettLogg
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY {&SELF-NAME} FOR EACH TT_EtikettLogg.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 TT_EtikettLogg
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 TT_EtikettLogg


/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 EAN Vg LopNr Storl Ant /* field bongtekst as char */ Pris Plunr /* field pris2 as dec */ /* FIELD individ AS DEC DECIMALS 0 */ /* field SeqNr as int. */   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2   
&Scoped-define SELF-NAME BROWSE-2
&Scoped-define QUERY-STRING-BROWSE-2 FOR EACH TT_EtikettLogg
&Scoped-define OPEN-QUERY-BROWSE-2 OPEN QUERY {&SELF-NAME} FOR EACH TT_EtikettLogg.
&Scoped-define TABLES-IN-QUERY-BROWSE-2 TT_EtikettLogg
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 TT_EtikettLogg


/* Definitions for FRAME FRAME-Typ1                                     */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-Typ1 ~
    ~{&OPEN-QUERY-BROWSE-1}

/* Definitions for FRAME FRAME-Typ2                                     */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-Typ2 ~
    ~{&OPEN-QUERY-BROWSE-2}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BUTTON-Ok RECT-49 RECT-50 B-Print RS-EtiTyp 
&Scoped-Define DISPLAYED-OBJECTS RS-EtiTyp 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FixChkEan C-Win 
FUNCTION FixChkEan RETURNS CHARACTER
    ( INPUT cKode AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Print 
     IMAGE-UP FILE "icon/e-print":U
     LABEL "Print" 
     SIZE 4.6 BY 1.1 TOOLTIP "Utskrift av etiketter".

DEFINE BUTTON BUTTON-Ok 
     IMAGE-UP FILE "icon\e-exit":U NO-FOCUS FLAT-BUTTON
     LABEL "&Ok" 
     SIZE 4.6 BY 1.05 TOOLTIP "Avsluta".

DEFINE VARIABLE RS-EtiTyp AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Etikett 1", 1,
"Etikett 2", 2
     SIZE 29.8 BY 1.19 NO-UNDO.

DEFINE RECTANGLE RECT-49
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 98 BY .05.

DEFINE RECTANGLE RECT-50
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 98 BY .05.

DEFINE VARIABLE FI-Ant AS INTEGER FORMAT "zzz9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-EAN AS DECIMAL FORMAT ">>>>>>>>>>>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 21.8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-LopNr AS INTEGER FORMAT "zzz9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Pris AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Storl AS CHARACTER FORMAT "X(4)":U 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Vg AS INTEGER FORMAT "zz9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-10 AS CHARACTER FORMAT "X(256)":U INITIAL "Antall" 
      VIEW-AS TEXT 
     SIZE 14 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FILL-IN-11 AS CHARACTER FORMAT "X(256)":U INITIAL "Pris" 
      VIEW-AS TEXT 
     SIZE 14 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FILL-IN-12 AS CHARACTER FORMAT "X(256)":U INITIAL "EAN" 
      VIEW-AS TEXT 
     SIZE 7 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FILL-IN-7 AS CHARACTER FORMAT "X(256)":U INITIAL "VgNr" 
      VIEW-AS TEXT 
     SIZE 7 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FILL-IN-8 AS CHARACTER FORMAT "X(256)":U INITIAL "LøpNr" 
      VIEW-AS TEXT 
     SIZE 8 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FILL-IN-9 AS CHARACTER FORMAT "X(256)":U INITIAL "Storl" 
      VIEW-AS TEXT 
     SIZE 7 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE RS-Typ AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "EAN", 1,
"Vg/Löpnr", 2
     SIZE 34 BY 1.19 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 3 GRAPHIC-EDGE  NO-FILL   
     SIZE 81 BY 3.33.

DEFINE VARIABLE CB-Artikeltyp AS CHARACTER FORMAT "X(256)":U 
     LABEL "Art.typ" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Ant2 AS INTEGER FORMAT "zzz9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-EAN2 AS DECIMAL FORMAT ">>>>>>>>>>>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 21.8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-LopNr2 AS INTEGER FORMAT "zzz9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Pris2 AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-prisgrupp AS CHARACTER FORMAT "X(256)":U 
     LABEL "Plufil" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Storl2 AS CHARACTER FORMAT "X(4)":U 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Vg2 AS INTEGER FORMAT "zz9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE RS-Typ2 AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "EAN", 1,
"Vg/Löpnr", 2
     SIZE 34 BY 1.19 NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 3 GRAPHIC-EDGE  NO-FILL   
     SIZE 96 BY 3.33.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      TT_EtikettLogg SCROLLING.

DEFINE QUERY BROWSE-2 FOR 
      TT_EtikettLogg SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 C-Win _FREEFORM
  QUERY BROWSE-1 DISPLAY
      EAN    FORMAT "x(17)"
      Vg 
      LopNr  LABEL "Löpnr"
      Storl
      Ant
/*       field bongtekst as char */
      Pris
/*       field pris2     as dec format "-zzz,zz9.99" */
/*       FIELD individ   AS DEC DECIMALS 0           */
/*       field SeqNr     as int.                     */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 81 BY 13.1 FIT-LAST-COLUMN.

DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 C-Win _FREEFORM
  QUERY BROWSE-2 DISPLAY
      EAN    FORMAT "x(17)"
      Vg 
      LopNr  LABEL "Löpnr"
      Storl
      Ant
/*       field bongtekst as char */
      Pris
      Plunr LABEL "PLU"
/*       field pris2     as dec format "-zzz,zz9.99" */
/*       FIELD individ   AS DEC DECIMALS 0           */
/*       field SeqNr     as int.                     */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 95 BY 13.1 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BUTTON-Ok AT ROW 1.38 COL 93.6 NO-TAB-STOP 
     B-Print AT ROW 1.38 COL 2.2
     RS-EtiTyp AT ROW 3.14 COL 3.2 NO-LABEL 
     RECT-49 AT ROW 1.19 COL 1.4
     RECT-50 AT ROW 2.62 COL 1.2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 99 BY 33.

DEFINE FRAME FRAME-Typ2
     RS-Typ2 AT ROW 1.24 COL 2 NO-LABEL 
     CB-Artikeltyp AT ROW 1.48 COL 41 COLON-ALIGNED
     FI-prisgrupp AT ROW 1.48 COL 73.8 COLON-ALIGNED
     FI-EAN2 AT ROW 4.1 COL 1.8 COLON-ALIGNED NO-LABEL
     FI-Vg2 AT ROW 4.1 COL 24.6 COLON-ALIGNED NO-LABEL
     FI-LopNr2 AT ROW 4.1 COL 32.6 COLON-ALIGNED NO-LABEL
     FI-Storl2 AT ROW 4.1 COL 41.6 COLON-ALIGNED NO-LABEL
     FI-Ant2 AT ROW 4.1 COL 49.6 COLON-ALIGNED NO-LABEL
     FI-Pris2 AT ROW 4.1 COL 64.6 COLON-ALIGNED NO-LABEL
     BROWSE-2 AT ROW 6.62 COL 2
     FILL-IN-12 AT ROW 3.38 COL 1.8 COLON-ALIGNED NO-LABEL FORMAT "X(256)":U
           VIEW-AS TEXT 
          SIZE 7 BY .62
          FONT 6
     FILL-IN-7 AT ROW 3.38 COL 24.6 COLON-ALIGNED NO-LABEL FORMAT "X(256)":U
           VIEW-AS TEXT 
          SIZE 7 BY .62
          FONT 6
     FILL-IN-8 AT ROW 3.38 COL 32.6 COLON-ALIGNED NO-LABEL FORMAT "X(256)":U
           VIEW-AS TEXT 
          SIZE 8 BY .62
          FONT 6
     FILL-IN-9 AT ROW 3.38 COL 41.6 COLON-ALIGNED NO-LABEL FORMAT "X(256)":U
           VIEW-AS TEXT 
          SIZE 7 BY .62
          FONT 6
     FILL-IN-10 AT ROW 3.38 COL 49.6 COLON-ALIGNED NO-LABEL FORMAT "X(256)":U
           VIEW-AS TEXT 
          SIZE 14 BY .62
          FONT 6
     FILL-IN-11 AT ROW 3.38 COL 64.6 COLON-ALIGNED NO-LABEL FORMAT "X(256)":U
           VIEW-AS TEXT 
          SIZE 14 BY .62
          FONT 6
     RECT-2 AT ROW 2.91 COL 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 4.76
         SIZE 97.6 BY 20 .

DEFINE FRAME FRAME-Typ1
     RS-Typ AT ROW 1.24 COL 2 NO-LABEL 
     FI-EAN AT ROW 4.1 COL 1.8 COLON-ALIGNED NO-LABEL 
     FI-Vg AT ROW 4.1 COL 24.6 COLON-ALIGNED NO-LABEL 
     FI-LopNr AT ROW 4.1 COL 32.6 COLON-ALIGNED NO-LABEL 
     FI-Storl AT ROW 4.1 COL 41.6 COLON-ALIGNED NO-LABEL 
     FI-Ant AT ROW 4.1 COL 49.6 COLON-ALIGNED NO-LABEL 
     FI-Pris AT ROW 4.1 COL 64.6 COLON-ALIGNED NO-LABEL 
     BROWSE-1 AT ROW 6.62 COL 2 
     FILL-IN-12 AT ROW 3.38 COL 1.8 COLON-ALIGNED NO-LABEL 
     FILL-IN-7 AT ROW 3.38 COL 24.6 COLON-ALIGNED NO-LABEL 
     FILL-IN-8 AT ROW 3.38 COL 32.6 COLON-ALIGNED NO-LABEL 
     FILL-IN-9 AT ROW 3.38 COL 41.6 COLON-ALIGNED NO-LABEL 
     FILL-IN-10 AT ROW 3.38 COL 49.6 COLON-ALIGNED NO-LABEL 
     FILL-IN-11 AT ROW 3.38 COL 64.6 COLON-ALIGNED NO-LABEL 
     RECT-1 AT ROW 2.91 COL 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 4.76
         SIZE 97.6 BY 20 .


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
         TITLE              = "Manuell utskrift av etiketter"
         HEIGHT             = 23.86
         WIDTH              = 99.2
         MAX-HEIGHT         = 36.05
         MAX-WIDTH          = 188.2
         VIRTUAL-HEIGHT     = 36.05
         VIRTUAL-WIDTH      = 188.2
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
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* REPARENT FRAME */
ASSIGN FRAME FRAME-Typ1:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME FRAME-Typ2:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
   NOT-VISIBLE FRAME-NAME                                               */
/* SETTINGS FOR FRAME FRAME-Typ1
                                                                        */
/* BROWSE-TAB BROWSE-1 FI-Pris FRAME-Typ1 */
/* SETTINGS FOR FILL-IN FILL-IN-10 IN FRAME FRAME-Typ1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-11 IN FRAME FRAME-Typ1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-12 IN FRAME FRAME-Typ1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-7 IN FRAME FRAME-Typ1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-8 IN FRAME FRAME-Typ1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-9 IN FRAME FRAME-Typ1
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME FRAME-Typ2
                                                                        */
/* BROWSE-TAB BROWSE-2 FI-Pris2 FRAME-Typ2 */
/* SETTINGS FOR FILL-IN FI-prisgrupp IN FRAME FRAME-Typ2
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-10 IN FRAME FRAME-Typ2
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-11 IN FRAME FRAME-Typ2
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-12 IN FRAME FRAME-Typ2
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-7 IN FRAME FRAME-Typ2
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-8 IN FRAME FRAME-Typ2
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-9 IN FRAME FRAME-Typ2
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-1
/* Query rebuild information for BROWSE BROWSE-1
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH TT_EtikettLogg.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH TT_EtikettLogg.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-2 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Manuell utskrift av etiketter */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Manuell utskrift av etiketter */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Print
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Print C-Win
ON CHOOSE OF B-Print IN FRAME DEFAULT-FRAME /* Print */
DO:
    DEFINE VARIABLE lIgen AS LOGICAL     NO-UNDO.
    find first TT_EtikettLogg no-lock no-error.
    if not available TT_EtikettLogg THEN do:
        message "Ingen etiketter å skrive ut!"
          view-as alert-box message title "Melding".
        return no-apply.
    end.
    ELSE DO:
        RUN Initiering.
        REPEAT:
            RUN Utskrift.
            MESSAGE "Vill du skriva ut igen?"
                VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lIgen.
            IF lIgen = FALSE THEN
                LEAVE.
        END.
/*         RUN Sendfil. */
    /*     APPLY "CLOSE" TO {&BROWSE-NAME}. */
        RUN EmptyReg.
    END.
    IF RS-EtiTyp = 1 THEN DO WITH FRAME FRAME-Typ1:
        IF RS-Typ:SCREEN-VALUE = "1" THEN
            APPLY "ENTRY" TO FI-Ean.
        ELSE
            APPLY "ENTRY" TO FI-Vg.
    END.
    ELSE IF RS-EtiTyp = 2 THEN DO WITH FRAME FRAME-Typ2:
        IF RS-Typ2:SCREEN-VALUE = "1" THEN
            APPLY "ENTRY" TO FI-Ean2.
        ELSE
            APPLY "ENTRY" TO FI-Vg2.
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-1
&Scoped-define FRAME-NAME FRAME-Typ1
&Scoped-define SELF-NAME BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 C-Win
ON MOUSE-SELECT-CLICK OF BROWSE-1 IN FRAME FRAME-Typ1
DO:
    IF BROWSE {&BROWSE-NAME}:FOCUSED-ROW = ? THEN
        RETURN NO-APPLY.
  IF BROWSE {&BROWSE-NAME}:FOCUSED-ROW-SELECTED THEN DO:
      IF TT_etikettlogg.ean <> "" THEN DO:
          FI-EAN:SCREEN-VALUE = TT_etikettlogg.EAN.
          APPLY "TAB" TO FI-EAN.
          FI-Ant:SCREEN-VALUE = STRING(TT_EtikettLogg.Ant).
          APPLY "ENTRY" TO FI-Ant.
      END.
      ELSE DO:
          ASSIGN FI-VG:SCREEN-VALUE    = string(TT_etikettlogg.vg)
                 FI-lopnr:SCREEN-VALUE = string(TT_etikettlogg.lopnr)
                 fi-storl:SCREEN-VALUE = TT_etikettlogg.storl
                 fi-ant:SCREEN-VALUE = STRING(TT_etikettlogg.ant)
                 fi-pris:SCREEN-VALUE = STRING(TT_etikettlogg.pris).
          APPLY "ENTRY" TO FI-ant.
      END.
  END.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 C-Win
ON MOUSE-SELECT-DBLCLICK OF BROWSE-1 IN FRAME FRAME-Typ1
DO:
    IF BROWSE {&BROWSE-NAME}:FOCUSED-ROW <> ? THEN DO:
        MESSAGE "Önskar du att ta bort registrerad post?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lTabort AS LOG.
        IF lTabort THEN DO:
            DELETE TT_EtikettLogg.
            BROWSE {&BROWSE-NAME}:DELETE-SELECTED-ROWS().
            IF BROWSE {&BROWSE-NAME}:FOCUSED-ROW <> ? THEN
                BROWSE {&BROWSE-NAME}:DESELECT-FOCUSED-ROW().
        END.
        ELSE
            BROWSE {&BROWSE-NAME}:DESELECT-FOCUSED-ROW().
    END.
    RUN ClearFelt.
    RUN ApplyEntry.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME BUTTON-Ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Ok C-Win
ON CHOOSE OF BUTTON-Ok IN FRAME DEFAULT-FRAME /* Ok */
DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-Typ2
&Scoped-define SELF-NAME CB-Artikeltyp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Artikeltyp C-Win
ON VALUE-CHANGED OF CB-Artikeltyp IN FRAME FRAME-Typ2 /* Art.typ */
DO:
    ASSIGN CB-Artikeltyp.
    IF RS-Typ2:SCREEN-VALUE = "1" THEN DO:
        FI-EAN2:SENSITIVE = TRUE.
        apply "entry":U to FI-EAN2 in fram {&FRAME-NAME}.
    END.
    ELSE DO:
        FI-EAN2:SENSITIVE = FALSE.
        apply "entry":U to FI-Vg2 in fram {&FRAME-NAME}.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-Typ1
&Scoped-define SELF-NAME FI-Ant
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Ant C-Win
ON RETURN OF FI-Ant IN FRAME FRAME-Typ1
DO:
  APPLY "TAB" TO SELF.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Ant C-Win
ON TAB OF FI-Ant IN FRAME FRAME-Typ1
DO:
  if input FI-Ant <= 0 then
    do:
      message "Antall må angis!"
              view-as alert-box message title "Melding".
      return no-apply.
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-Typ2
&Scoped-define SELF-NAME FI-Ant2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Ant2 C-Win
ON RETURN OF FI-Ant2 IN FRAME FRAME-Typ2
DO:
  APPLY "TAB" TO SELF.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Ant2 C-Win
ON TAB OF FI-Ant2 IN FRAME FRAME-Typ2
DO:
  if input FI-Ant2 <= 0 then
    do:
      message "Antall må angis!"
              view-as alert-box message title "Melding".
      return no-apply.
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-Typ1
&Scoped-define SELF-NAME FI-EAN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-EAN C-Win
ON RETURN OF FI-EAN IN FRAME FRAME-Typ1
DO:
  APPLY "TAB" TO SELF.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-EAN C-Win
ON TAB OF FI-EAN IN FRAME FRAME-Typ1
DO:
    DEFINE VARIABLE cEan AS CHARACTER   NO-UNDO.
    ASSIGN INPUT FI-EAN.
    do:
        cEan = STRING(SELF:SCREEN-VALUE,"9999999999999").
        IF cEan <> DYNAMIC-FUNCTION('FixChkEan':U,SUBSTR(cEan,1,12))THEN DO:
            MESSAGE "Fel checksiffra"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RETURN NO-APPLY.
        END.
        FIND TT_Artikkel WHERE TT_Artikkel.kode = FI-Ean NO-ERROR.
        IF AVAIL TT_Artikkel THEN DO:
            ASSIGN FI-Vg:SCREEN-VALUE = STRING(TT_Artikkel.Vg)
                   FI-LopNr:SCREEN-VALUE = STRING(TT_Artikkel.Lopnr)
                   FI-Storl:SCREEN-VALUE = TT_Artikkel.Storl
                   FI-Pris:SCREEN-VALUE = STRING(TT_Artikkel.Pris).
            APPLY "ENTRY" TO FI-Ant.
            RETURN NO-APPLY.
        END.
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-Typ2
&Scoped-define SELF-NAME FI-EAN2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-EAN2 C-Win
ON RETURN OF FI-EAN2 IN FRAME FRAME-Typ2
DO:
  APPLY "TAB" TO SELF.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-EAN2 C-Win
ON TAB OF FI-EAN2 IN FRAME FRAME-Typ2
DO:
    DEFINE VARIABLE cEan AS CHARACTER   NO-UNDO.
    ASSIGN INPUT FI-EAN2.
    DO WITH FRAME FRAME-Typ2:
        cEan = STRING(SELF:SCREEN-VALUE,"9999999999999").
        IF cEan <> DYNAMIC-FUNCTION('FixChkEan':U,SUBSTR(cEan,1,12))THEN DO:
            MESSAGE "Fel checksiffra"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RETURN NO-APPLY.
        END.
        FIND TT_Artikkel WHERE TT_Artikkel.kode = FI-Ean2 NO-ERROR.
        IF AVAIL TT_Artikkel THEN DO:
            ASSIGN FI-Vg2:SCREEN-VALUE = STRING(TT_Artikkel.Vg)
                   FI-LopNr2:SCREEN-VALUE = STRING(TT_Artikkel.Lopnr)
                   FI-Storl2:SCREEN-VALUE = TT_Artikkel.Storl.
/*                    FI-Pris2:SCREEN-VALUE = STRING(TT_Artikkel.Pris). */
            APPLY "ENTRY" TO FI-Ant2.
            RETURN NO-APPLY.
        END.
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-Typ1
&Scoped-define SELF-NAME FI-LopNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-LopNr C-Win
ON RETURN OF FI-LopNr IN FRAME FRAME-Typ1
DO:
  APPLY "TAB" TO SELF.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-LopNr C-Win
ON TAB OF FI-LopNr IN FRAME FRAME-Typ1
DO:
  find FIRST TT_artikkel no-lock where
    TT_artikkel.Vg = input FI-Vg and
    TT_artikkel.LopNr = input FI-LopNr no-error.
  if available TT_artikkel THEN do:
      DISP TT_Artikkel.Pris @ FI-Pris
        with frame {&FRAME-NAME}.
  end.
  ELSE
      DISP 0 @ FI-Pris
        with frame {&FRAME-NAME}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-Typ2
&Scoped-define SELF-NAME FI-LopNr2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-LopNr2 C-Win
ON RETURN OF FI-LopNr2 IN FRAME FRAME-Typ2
DO:
  APPLY "TAB" TO SELF.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-LopNr2 C-Win
ON TAB OF FI-LopNr2 IN FRAME FRAME-Typ2
DO:
/*   find TT_artikkel no-lock where                  */
/*     TT_artikkel.Vg = input FI-Vg2 and             */
/*     TT_artikkel.LopNr = input FI-LopNr2 no-error. */
/*   if available TT_artikkel THEN do:               */
/*       DISP TT_Artikkel.Pris @ FI-Pris2            */
/*         with frame {&FRAME-NAME}.                 */
/*   end.                                            */
/*   ELSE                                            */
      DISP 0 @ FI-Pris2
        with frame FRAME-Typ2.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-Typ1
&Scoped-define SELF-NAME FI-Pris
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Pris C-Win
ON TAB OF FI-Pris IN FRAME FRAME-Typ1
or "return":U of FI-Pris 
DO:
    DEFINE VARIABLE rr AS ROWID       NO-UNDO.
  assign frame {&FRAME-NAME}
    FI-Vg
    FI-LopNr
    FI-Storl
    FI-Ant
    FI-Pris = int(input FI-Pris).    

/*   if available ArtBas then                                  */
/*     do:                                                     */
/*       assign                                                */
/*         ED-TransListe = string(ArtBas.Vg,"zz9") + "/" +     */
/*                         string(ArtBas.LopNr,"zzz9") + " " + */
/*                         ArtBas.Bongtekst + " " +            */
/*                         FI-Storl + " " +                    */
/*                         string(input FI-Ant)+ " " +         */
/*                         string(input FI-Pris)  + chr(13) +  */
/*                         ED-TransListe.                      */
/*     end.                                                    */
/*   else                                                      */
  ASSIGN INPUT FI-Ean
         INPUT FI-vg
         INPUT Fi-lopnr
         INPUT FI-pris
         INPUT FI-Storl
         INPUT FI-Ant.
  FIND TT_Etikettlogg WHERE TT_etikettlogg.EAN   = (IF FI-EAN:SCREEN-VALUE <> "0" THEN FI-EAN:SCREEN-VALUE ELSE "") AND
                            TT_etikettlogg.Vg    = FI-Vg  AND 
                            TT_Etikettlogg.Lopnr = FI-Lopnr AND
                            TT_Etikettlogg.Storl = FI-Storl NO-ERROR.
  IF AVAIL TT_Etikettlogg THEN DO:
      ASSIGN 
      TT_EtikettLogg.Ant       = input FI-Ant
      TT_EtikettLogg.Pris      = FI-Pris.
  END.
  ELSE DO:
      create TT_EtikettLogg.
      assign
        wSeqNr                = wSeqNr + 1
        TT_EtikettLogg.EAN = IF FI-EAN:SCREEN-VALUE <> "0" THEN FI-EAN:SCREEN-VALUE ELSE ""
        TT_EtikettLogg.Vg        = FI-Vg
        TT_EtikettLogg.LopNr     = FI-LopNr
        TT_EtikettLogg.Ant       = input FI-Ant
        TT_EtikettLogg.Storl     = input FI-Storl
    /*     TT_EtikettLogg.Bongtekst = if available ArtBas  */
    /*                               then ArtBas.Bongtekst */
    /*                               else ""               */
        TT_EtikettLogg.Pris      = FI-Pris
        TT_EtikettLogg.SeqNr     = wSeqNr.
  END.
  rr = ROWID(TT_Etikettlogg).
  IF FI-Ean:SCREEN-VALUE = "0" THEN DO:
      FIND tt_artikkel WHERE tt_artikkel.vg = fi-vg AND
                             tt_artikkel.lopnr = fi-lopnr NO-ERROR.
  END.
  ELSE
      FIND tt_artikkel WHERE tt_artikkel.kode = fi-Ean NO-ERROR.
  IF NOT AVAIL tt_artikkel THEN DO:
      CREATE tt_artikkel.
      ASSIGN tt_artikkel.kode = FI-Ean
             tt_artikkel.vg = FI-Vg
             tt_artikkel.lopnr = FI-Lopnr
             tt_artikkel.storl = IF FI-Ean <> 0 THEN FI-Storl ELSE ""
             tt_artikkel.pris = FI-Pris.
      RELEASE tt_artikkel.
  END.
  ELSE DO:
      IF FI-Ean <> 0 THEN
          ASSIGN tt_artikkel.vg = FI-Vg
                 tt_artikkel.lopnr = FI-Lopnr
                 tt_artikkel.storl = FI-Storl.
      ASSIGN tt_artikkel.pris = FI-Pris.
  END.
  RUN ClearFelt.
  {&OPEN-QUERY-BROWSE-1}
  FIND TT_Etikettlogg NO-ERROR.
  IF NOT AVAIL TT_EtikettLogg THEN
  REPOSITION BROWSE-1 TO ROWID rr.
  BROWSE BROWSE-1:DESELECT-FOCUSED-ROW().
/*   BROWSE {&BROWSE-NAME}:REFRESH(). */
  RUN ApplyEntry.
  return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-Typ2
&Scoped-define SELF-NAME FI-Pris2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Pris2 C-Win
ON TAB OF FI-Pris2 IN FRAME FRAME-Typ2
or "return":U of FI-Pris2 
DO:
    DEFINE VARIABLE rr AS ROWID       NO-UNDO.
    DO WITH FRAME FRAME-Typ2:
        
  ASSIGN
    FI-Vg2
    FI-LopNr2
    FI-Storl2
    FI-Ant2
    FI-Pris2 = int(input FI-Pris2).    

/*   if available ArtBas then                                  */
/*     do:                                                     */
/*       assign                                                */
/*         ED-TransListe = string(ArtBas.Vg,"zz9") + "/" +     */
/*                         string(ArtBas.LopNr,"zzz9") + " " + */
/*                         ArtBas.Bongtekst + " " +            */
/*                         FI-Storl + " " +                    */
/*                         string(input FI-Ant)+ " " +         */
/*                         string(input FI-Pris)  + chr(13) +  */
/*                         ED-TransListe.                      */
/*     end.                                                    */
/*   else                                                      */
  ASSIGN INPUT FI-Ean2
         INPUT FI-vg2
         INPUT Fi-lopnr2
         INPUT FI-pris2.
  FIND TT_Prisgrupper WHERE TT_Prisgrupper.Artikeltyp = CB-Artikeltyp AND
                            TT_Prisgrupper.Pris = FI-Pris2 NO-ERROR.
  IF NOT AVAIL TT_Prisgrupper THEN DO:
      MESSAGE "Finner ingen prisgrupp '" CB-Artikeltyp "' med pris '" FI-Pris2 "'"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY "ENTRY" TO FI-Pris2.
      RETURN NO-APPLY.
  END.
  create TT_EtikettLogg.
  assign
    wSeqNr                = wSeqNr + 1
    TT_EtikettLogg.EAN = FI-EAN2:SCREEN-VALUE
    TT_EtikettLogg.Vg        = FI-Vg2
    TT_EtikettLogg.LopNr     = FI-LopNr2
    TT_EtikettLogg.Ant       = input FI-Ant2
    TT_EtikettLogg.Storl     = input FI-Storl2
/*     TT_EtikettLogg.Bongtekst = if available ArtBas  */
/*                               then ArtBas.Bongtekst */
/*                               else ""               */
    TT_EtikettLogg.Pris      = FI-Pris2
    TT_EtikettLogg.Plunr     = TT_Prisgrupper.Plunr
    TT_EtikettLogg.SeqNr     = wSeqNr.
  rr = ROWID(TT_Etikettlogg).
/*   IF FI-Ean2:SCREEN-VALUE = "0" THEN DO:                                */
/*       FIND tt_artikkel WHERE tt_artikkel.vg = fi-vg2 AND                */
/*                              tt_artikkel.lopnr = fi-lopnr2 NO-ERROR.    */
/*   END.                                                                  */
/*   ELSE                                                                  */
/*       FIND tt_artikkel WHERE tt_artikkel.kode = fi-Ean2 NO-ERROR.       */
/*   IF NOT AVAIL tt_artikkel THEN DO:                                     */
/*       CREATE tt_artikkel.                                               */
/*       ASSIGN tt_artikkel.kode = FI-Ean2                                 */
/*              tt_artikkel.vg = FI-Vg2                                    */
/*              tt_artikkel.lopnr = FI-Lopnr2                              */
/*              tt_artikkel.storl = IF FI-Ean2 <> 0 THEN FI-Storl2 ELSE "" */
/*              tt_artikkel.pris = FI-Pris2.                               */
/*       RELEASE tt_artikkel.                                              */
/*   END.                                                                  */
/*   ELSE DO:                                                              */
/*       IF FI-Ean <> 0 THEN                                               */
/*           ASSIGN tt_artikkel.vg = FI-Vg2                                */
/*                  tt_artikkel.lopnr = FI-Lopnr2                          */
/*                  tt_artikkel.storl = FI-Storl2.                         */
/*       ASSIGN tt_artikkel.pris = FI-Pris2.                               */
/*   END.                                                                  */
  {&OPEN-QUERY-BROWSE-2}
  FIND TT_Etikettlogg NO-ERROR.
  IF NOT AVAIL TT_EtikettLogg THEN
  REPOSITION Browse-2 TO ROWID rr.
/*   BROWSE {&BROWSE-NAME}:REFRESH(). */
  IF RS-Typ2:SCREEN-VALUE IN FRAME FRAME-Typ2 = "1" THEN
      apply "entry":U to FI-EAN2.
  ELSE
      apply "entry":U to FI-Vg2.
    END.
  return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-Typ1
&Scoped-define SELF-NAME FI-Storl
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Storl C-Win
ON RETURN OF FI-Storl IN FRAME FRAME-Typ1
DO:
  APPLY "TAB" TO SELF.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Storl C-Win
ON TAB OF FI-Storl IN FRAME FRAME-Typ1
DO:
  assign frame {&FRAME-NAME}
    FI-Storl.

    run FiksStorl in THIS-PROCEDURE (input-output FI-Storl).
    
  display 
    FI-Storl 
  with frame {&FRAME-NAME}.
  IF FI-Ean:SCREEN-VALUE = "0" THEN DO:
      FIND tt_etikettlogg WHERE tt_etikettlogg.vg = INPUT FI-Vg AND
                                tt_etikettlogg.Lopnr = INPUT fi-lopnr AND
                                tt_etikettlogg.storl = INPUT fi-storl NO-ERROR.
      IF avail tt_etikettlogg THEN
          ASSIGN FI-Ant:SCREEN-VALUE = STRING(tt_etikettlogg.ant)
                 FI-Pris:SCREEN-VALUE = STRING(TT_etikettlogg.Pris).
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-Typ2
&Scoped-define SELF-NAME FI-Storl2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Storl2 C-Win
ON RETURN OF FI-Storl2 IN FRAME FRAME-Typ2
DO:
  APPLY "TAB" TO SELF.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Storl2 C-Win
ON TAB OF FI-Storl2 IN FRAME FRAME-Typ2
DO:
  assign frame {&FRAME-NAME}
    FI-Storl2.

    run FiksStorl in THIS-PROCEDURE (input-output FI-Storl2).
    
  display 
    FI-Storl2 
  with frame {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-Typ1
&Scoped-define SELF-NAME FI-Vg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Vg C-Win
ON RETURN OF FI-Vg IN FRAME FRAME-Typ1
DO:
  APPLY "TAB" TO SELF.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-Typ2
&Scoped-define SELF-NAME FI-Vg2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Vg2 C-Win
ON RETURN OF FI-Vg2 IN FRAME FRAME-Typ2
DO:
  APPLY "TAB" TO SELF.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME RS-EtiTyp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RS-EtiTyp C-Win
ON VALUE-CHANGED OF RS-EtiTyp IN FRAME DEFAULT-FRAME
DO:
  ASSIGN RS-EtiTyp.
  IF RS-EtiTyp = 1 THEN DO:
      FRAME FRAME-Typ1:MOVE-TO-TOP().
      APPLY "VALUE-CHANGED" TO RS-Typ IN FRAME FRAME-Typ1.
  END.
  ELSE IF RS-EtiTyp = 2 THEN DO:
      IF FI-prisgrupp:SCREEN-VALUE IN FRAME FRAME-Typ2 = "" THEN DO:
              MESSAGE "Finner inte prisgruppfil (" + cPrisgruppfil + ")"
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
              RS-EtiTyp:SCREEN-VALUE = "1".
              RETURN NO-APPLY.
      END.
      ELSE IF NOT CAN-FIND(FIRST TT_prisgrupper) THEN DO:
          MESSAGE "Filen " cPrisgruppfil " innehåller inga PLU-priser"
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RS-EtiTyp:SCREEN-VALUE = "1".
          RETURN NO-APPLY.
      END.
      FRAME FRAME-Typ2:MOVE-TO-TOP().
      APPLY "VALUE-CHANGED" TO RS-Typ2 IN FRAME FRAME-Typ2.
  END.
  RUN EmptyReg.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-Typ1
&Scoped-define SELF-NAME RS-Typ
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RS-Typ C-Win
ON VALUE-CHANGED OF RS-Typ IN FRAME FRAME-Typ1
DO:
    IF CAN-FIND(FIRST TT_Etikettlogg) THEN DO:
        MESSAGE "Du har registrerat koder." SKIP
                "Vill du skriva ut dom först?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE skrivut AS LOG.
        IF skrivut THEN
            APPLY "CHOOSE" TO B-Print IN FRAME DEFAULT-FRAME.
        ELSE
            RUN EmptyReg.
    END.
    IF RS-Typ:SCREEN-VALUE = "1" THEN DO:
        FI-EAN:SENSITIVE = TRUE.
        apply "entry":U to FI-EAN in fram {&FRAME-NAME}.
    END.
    ELSE DO:
        FI-EAN:SENSITIVE = FALSE.
        apply "entry":U to FI-Vg in fram {&FRAME-NAME}.
    END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-Typ2
&Scoped-define SELF-NAME RS-Typ2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RS-Typ2 C-Win
ON VALUE-CHANGED OF RS-Typ2 IN FRAME FRAME-Typ2
DO:
    DO WITH FRAME FRAME-Typ1:
        IF CAN-FIND(FIRST TT_Etikettlogg) THEN DO:
            MESSAGE "Du har registrerat koder." SKIP
                    "Vill du skriva ut dom först?"
                VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE skrivut AS LOG.
            IF skrivut THEN
                APPLY "CHOOSE" TO B-Print IN FRAME DEFAULT-FRAME.
            ELSE
                RUN EmptyReg.
        END.
        IF RS-Typ2:SCREEN-VALUE = "1" THEN DO:
            FI-EAN2:SENSITIVE = TRUE.
            apply "entry":U to FI-EAN2 in fram {&FRAME-NAME}.
        END.
        ELSE DO:
            FI-EAN2:SENSITIVE = FALSE.
            apply "entry":U to FI-Vg2 in fram {&FRAME-NAME}.
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
ON CLOSE OF THIS-PROCEDURE DO:
    RUN DumpaDB.
    RUN disable_UI.
END.
ON 'ALT-P':U ANYWHERE
DO:
    APPLY "CHOOSE" TO B-Print IN FRAME {&FRAME-NAME}.
    RETURN.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  RUN initEtikettVars.
  IF SEARCH(cPrisgruppfil) <> ? THEN DO:
      RUN initPrisgrupp.
      FI-prisgrupp = cPrisgruppfil.
  END.
  RUN enable_UI.
  FI-prisgrupp:SCREEN-VALUE = FI-prisgrupp.
  APPLY "VALUE-CHANGED" TO RS-EtiTyp IN FRAME DEFAULT-FRAME.
  BROWSE BROWSE-1:SET-REPOSITIONED-ROW (BROWSE BROWSE-1:DOWN ,"ALWAYS").
  BROWSE BROWSE-1:SET-REPOSITIONED-ROW (BROWSE BROWSE-1:DOWN ,"ALWAYS").
  RUN initDB.
  RUN ApplyEntry.
  /*   APPLY "ENTRY" TO fi-ean. */
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ApplyEntry C-Win 
PROCEDURE ApplyEntry :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
      IF RS-EtiTyp = 1 THEN DO WITH FRAME FRAME-Typ1:
          IF RS-Typ:SCREEN-VALUE = "1" THEN DO:
              FI-EAN:SCREEN-VALUE = "".
              apply "entry":U to FI-EAN.
          END.
          ELSE
              apply "entry":U to FI-Vg.
      END.
      ELSE IF RS-EtiTyp = 2 THEN DO WITH FRAME FRAME-Typ2:
          IF RS-Typ2:SCREEN-VALUE = "1" THEN DO:
              FI-EAN2:SCREEN-VALUE = "".
              apply "entry":U to FI-EAN2.
          END.
          ELSE
              apply "entry":U to FI-Vg2.
      END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ClearFelt C-Win 
PROCEDURE ClearFelt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF RS-EtiTyp = 1 THEN DO WITH FRAME FRAME-Typ1:
      ASSIGN FI-EAN:SCREEN-VALUE = "0"
             FI-Vg:SCREEN-VALUE = "0"
             FI-LopNr:SCREEN-VALUE = "0" 
             FI-Storl:SCREEN-VALUE = ""
             FI-Ant:SCREEN-VALUE = "0" 
             FI-Pris:SCREEN-VALUE = "0". 
   END.
   ELSE IF RS-EtiTyp = 2 THEN DO WITH FRAME FRAME-Typ2:
      ASSIGN FI-EAN2:SCREEN-VALUE = "0"
             FI-Vg2:SCREEN-VALUE = "0"
             FI-LopNr2:SCREEN-VALUE = "0" 
             FI-Storl2:SCREEN-VALUE = ""
             FI-Ant2:SCREEN-VALUE = "0" 
             FI-Pris2:SCREEN-VALUE = "0". 
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Conv C-Win 
PROCEDURE Conv :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input-output parameter navn as char no-undo.

SORRY:
repeat on error undo, retry:
    if index(navn,"~{") <> 0 then
    navn = substring(navn,1,index(navn,"~{") - 1)
             + chr(126)
             + "132"
             + substring(navn,index(navn,"~{") + 1,length(navn)).
    if index(navn,"~|") <> 0 then
    navn = substring(navn,1,index(navn,"~|") - 1)
             + chr(126)
             + "148"
             + substring(navn,index(navn,"~|") + 1,length(navn)).
    if index(navn,"~}") <> 0 then
    navn = substring(navn,1,index(navn,"~}") - 1)
             + chr(126)
             + "134"
             + substring(navn,index(navn,"~}") + 1,length(navn)).
    if index(navn,"~[") <> 0 then
    navn = substring(navn,1,index(navn,"~[") - 1)
             + chr(126)
             + "142"
             + substring(navn,index(navn,"~[") + 1,length(navn)).
    if index(navn,"~\") <> 0 then
    navn = substring(navn,1,index(navn,"~\") - 1)
             + chr(126)
             + "153"
             + substring(navn,index(navn,"~\") + 1,length(navn)).
    if index(navn,"~]") <> 0 then
    navn = substring(navn,1,index(navn,"~]") - 1)
             + chr(126)
             + "143"
             + substring(navn,index(navn,"~]") + 1,length(navn)).
    if index(navn,"~{") <> 0 or
       index(navn,"~|") <> 0 or
       index(navn,"~}") <> 0 or
       index(navn,"~[") <> 0 or
       index(navn,"~\") <> 0 or
       index(navn,"~]") <> 0 then
    do:
        next SORRY.
    end.
    else leave SORRY.
end.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DumpaDB C-Win 
PROCEDURE DumpaDB :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    OUTPUT TO value(cETIKETTDB).
    FOR EACH tt_artikkel:
        EXPORT tt_artikkel.
    END.
    OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EmptyReg C-Win 
PROCEDURE EmptyReg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO:
      EMPTY TEMP-TABLE TT_Etikettlogg.
      IF RS-EtiTyp = 1 THEN DO WITH FRAME FRAME-Typ1:
          ASSIGN FI-EAN:SCREEN-VALUE = "0"
                 FI-Vg:SCREEN-VALUE = "0"
                 FI-LopNr:SCREEN-VALUE = "0" 
                 FI-Storl:SCREEN-VALUE = ""
                 FI-Ant:SCREEN-VALUE = "0" 
                 FI-Pris:SCREEN-VALUE = "0". 
          {&OPEN-QUERY-BROWSE-1}
      END.
      ELSE IF RS-EtiTyp = 2 THEN DO WITH FRAME FRAME-Typ2:
          ASSIGN FI-EAN2:SCREEN-VALUE = "0"
                 FI-Vg2:SCREEN-VALUE = "0"
                 FI-LopNr2:SCREEN-VALUE = "0" 
                 FI-Storl2:SCREEN-VALUE = ""
                 FI-Ant2:SCREEN-VALUE = "0" 
                 FI-Pris2:SCREEN-VALUE = "0". 
          {&OPEN-QUERY-BROWSE-2}
      END.
  END.


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
  DISPLAY RS-EtiTyp 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE BUTTON-Ok RECT-49 RECT-50 B-Print RS-EtiTyp 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  DISPLAY RS-Typ FI-EAN FI-Vg FI-LopNr FI-Storl FI-Ant FI-Pris FILL-IN-12 
          FILL-IN-7 FILL-IN-8 FILL-IN-9 FILL-IN-10 FILL-IN-11 
      WITH FRAME FRAME-Typ1 IN WINDOW C-Win.
  ENABLE RECT-1 RS-Typ FI-EAN FI-Vg FI-LopNr FI-Storl FI-Ant FI-Pris BROWSE-1 
      WITH FRAME FRAME-Typ1 IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-Typ1}
  DISPLAY RS-Typ2 CB-Artikeltyp FI-prisgrupp FI-EAN2 FI-Vg2 FI-LopNr2 FI-Storl2 
          FI-Ant2 FI-Pris2 FILL-IN-12 FILL-IN-7 FILL-IN-8 FILL-IN-9 FILL-IN-10 
          FILL-IN-11 
      WITH FRAME FRAME-Typ2 IN WINDOW C-Win.
  ENABLE RECT-2 RS-Typ2 CB-Artikeltyp FI-EAN2 FI-Vg2 FI-LopNr2 FI-Storl2 
         FI-Ant2 FI-Pris2 BROWSE-2 
      WITH FRAME FRAME-Typ2 IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-Typ2}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FiksStorl C-Win 
PROCEDURE FiksStorl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def INPUT-output parameter wStorl as char NO-UNDO.

  DEF VAR wDecimaler as CHAR NO-UNDO.

/*   {syspara.i 1 1 16 wDecimaler} */
  wDecimaler = "5".
  assign
     wStorl = trim(wStorl)
     wStorl = caps(wStorl)
     wStorl = if (length(wStorl) = 1 or
                  length(wStorl) = 3
                  )
                 then " " + wStorl
                 else wStorl.

  /* Bytter ut eventuelle comma med punkt. */
  if index(wStorl,",") <> 0 then
    OVERLAY(wStorl, index(wStorl,","), 1, "CHARACTER") = ".".

  /* Sjekker om det er benyttet gyldige tegn i halvnummer. */
  /* Er det ikke det, tas halvnummeret bort.               */
  if NUM-ENTRIES(wStorl,".") = 2 then
    DO:
      if NOT CAN-DO(wDecimaler,ENTRY(2,wStorl,".")) then
        wStorl = ENTRY(1,wStorl,".").
    END.

  RETURN wStorl.   /* Function return value. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitDB C-Win 
PROCEDURE InitDB :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF SEARCH(cETIKETTDB) = ? THEN
    RETURN NO-APPLY.
INPUT FROM value(cETIKETTDB) NO-ECHO.
REPEAT:
    CREATE tt_Artikkel.
    SET tt_artikkel NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        DELETE tt_artikkel.
END.
IF AVAIL tt_Artikkel AND
         tt_Artikkel.vg = 0 AND
         tt_artikkel.kode = 0 THEN
    DELETE tt_artikkel.
INPUT CLOSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initEtikettVars C-Win 
PROCEDURE initEtikettVars :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE cPara AS CHARACTER   NO-UNDO.
cPara = SESSION:PARAMETER.
IF cPara <> "" THEN DO:
    cPRINTER = ENTRY(1,cPara).
/*     cETIKETTDB   = ENTRY(1,cPara). */
/*     cETIKETTPORT = ENTRY(2,cPara). */
/*     cETIKETTSEND = ENTRY(3,cPara). */
END.
/* ELSE DO:                                     */
/*     cETIKETTDB   = OS-GETENV("ETIKETTDB").   */
/*     cETIKETTPORT = OS-GETENV("ETIKETTPORT"). */
/*     cETIKETTSEND = OS-GETENV("ETIKETTSEND"). */
/* END.                                         */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Initiering C-Win 
PROCEDURE Initiering :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
    assign
    linje[1]  = "F,1,A,R,G,500,288,""VANLIG""|" + chr(10)
    linje[2]  = "T,1,6,V,75,275,0,1,3,2,O,R,0,2|" + chr(10)     /* kronor    */
    linje[3]  = "T,2,2,V,55,65,0,1,2,1,O,R,0,2|" + chr(10)      /* |ren      */
    linje[4]  = "B,3,12,V,80,28,3,12,60,8,L,0|" + chr(10)       /* I 2 of 5 streckkod */
    linje[5] = "T,4,8,V,190,265,0,1,2,1,B,R,0,2|" + chr(10)     /* VG/LPNR   */
    linje[6] = "T,5,3,V,180,130,1,2,1,1,B,L,0,2|" + chr(10)     /* "stl"     */
    linje[7] = "T,6,4,V,190,100,0,1,2,1,B,L,0,2|" + chr(10)     /* STLK      */
    linje[8]  = "T,7,8,V,60,300,0,1,2,1,B,C,0,2|" + chr(10)    /* huvudgrupp */
    linje[9] = "T,8,8,V,120,225,1,1,2,1,B,R,0,3|" + chr(10).     /* PLU   */
    IF RS-Typ:SCREEN-VALUE IN FRAME FRAME-Typ1 = "1" OR 
       RS-Typ2:SCREEN-VALUE IN FRAME FRAME-Typ2 = "1"THEN
        linje[4]  = "B,3,13,F,132,230,7,2,60,7,L,2|" + chr(10).       /* EAN streckkod */

    /* Etikettens layout */
    wlayout = "~{".
    do i = 1 to 9:
        wlayout = wlayout + linje[i].
    end.
    wlayout = wlayout + "~}".
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initPrisgrupp C-Win 
PROCEDURE initPrisgrupp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE cStr AS CHARACTER   NO-UNDO.
/* DEFINE TEMP-TABLE TT_prisgrupper NO-UNDO             */
/*     FIELD Artikeltyp AS CHAR                         */
/*     FIELD PluNr      AS INTE                         */
/*     FIELD Pris       AS DECI                         */
/*     INDEX ArtPris IS PRIMARY UNIQUE Artikeltyp Pris. */
/* cArtikeltyper */
    INPUT FROM VALUE(cPrisgruppfil).
    REPEAT:
        IMPORT UNFORMATTED cStr.
        cStr = TRIM(cStr).
        IF cStr = "" OR NUM-ENTRIES(cStr,";") <> 5 THEN
            NEXT.
        CREATE TT_Prisgrupper.
        ASSIGN Artikeltyp = TRIM(ENTRY(1,cStr,";"))
               PluNr      = INT(ENTRY(2,cStr,";"))
               Pris       = DECI(ENTRY(5,cStr,";")) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            DELETE TT_prisgrupper.
        ELSE IF NOT CAN-DO(cArtikeltyper,TT_Prisgrupper.Artikeltyp) THEN
            cArtikeltyper = cArtikeltyper + (IF cArtikeltyper <> "" THEN "," ELSE "") + 
                                            TT_Prisgrupper.Artikeltyp.

    END.
    IF cArtikeltyper <> "" THEN
        ASSIGN CB-Artikeltyp:LIST-ITEMS IN FRAME FRAME-Typ2 = cArtikeltyper.
               CB-Artikeltyp = ENTRY(1,CB-Artikeltyp:LIST-ITEMS).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitType C-Win 
PROCEDURE InitType :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
    MESSAGE "EAN eller Vg/Löpnr (Ja=EAN)"
          VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE RSEan AS logi.
    IF RS-EtiTyp = 1 THEN DO WITH FRAME FRAME-Typ1:
      IF RSEan = TRUE THEN DO:
          RS-Typ:SCREEN-VALUE = "1".
          FI-EAN:SENSITIVE = TRUE.
          apply "entry":U to FI-EAN.
      END.
      ELSE DO:
          RS-Typ:SCREEN-VALUE = "2".
          FI-EAN:SENSITIVE = FALSE.
          apply "entry":U to FI-Vg.
      END.
    END.
    ELSE IF RS-EtiTyp = 2 THEN DO WITH FRAME FRAME-Typ2:
      IF RSEan = TRUE THEN DO:
          RS-Typ2:SCREEN-VALUE = "1".
          FI-EAN2:SENSITIVE = TRUE.
          apply "entry":U to FI-EAN2.
      END.
      ELSE DO:
          RS-Typ2:SCREEN-VALUE = "2".
          FI-EAN2:SENSITIVE = FALSE.
          apply "entry":U to FI-Vg2.
      END.
    END.
END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendFil C-Win 
PROCEDURE SendFil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE lIgen AS LOGICAL     NO-UNDO.
REPEAT:
    os-command SILENT value(cETIKETTSEND) 
                      value(wEtikett_Fil)
                      value(cETIKETTPORT).
    MESSAGE "Vill du skriva ut igen?"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lIgen.
    IF lIgen = FALSE THEN
        LEAVE.
END.


/*   run x-etikettsend.w (input wValgtSkriver).                             */
/*                                                                          */
/*   if search(wEtikett_Fil) <> ? then                                      */
/*     do:                                                                  */
/*       message "Etikettfil ikke sendt. Skal den slettes?"                 */
/*         view-as alert-box question BUTTONS YES-NO-CANCEL title "Bekreft" */
/*         update wSvar as log.                                             */
/*       if wSvar= ? then /* Angre og vente litt til */                     */
/*         do:                                                              */
/*           os-delete value(wEtikett_Fil).                                 */
/*           return no-apply "AVBRYT" .                                     */
/*         end.                                                             */
/*       else if wSvar then /* Slette fil. */                               */
/*        os-delete value(wEtikett_Fil).                                    */
/*     end.                                                                 */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Utskrift C-Win 
PROCEDURE Utskrift :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cTeller AS CHARACTER  NO-UNDO.
/*   if search("teller") <> ? THEN do:                     */
/*       input from teller no-echo.                        */
/*       IMPORT UNFORMATTED cTeller.                       */
/*       ASSIGN teller = INT(cTeller).                     */
/* /*             set teller. */                           */
/*       input close.                                      */
/*       if (teller = 0 or teller > 9999) then teller = 1. */
/*   end.                                                  */
/*   else                                                  */
/*       teller = 1.                                       */
  /*Ut med alle etikettene i filen. */
/*   output to value(wEtikett_Fil) no-echo. */
  OUTPUT TO PRINTER VALUE (cPRINTER).
    put control wLayout.
/*   output close. */
  for each TT_EtikettLogg
     break by TT_EtikettLogg.vg
           by TT_EtikettLogg.lopnr
           by TT_EtikettLogg.Storl:

        /* Konverterer st|rrelsen.                   */
        /* Nb: Nullstiller alfanumeriske st|rrelser. */
        tstorlek = "".
        do i = 1 to length(TT_EtikettLogg.Storl):
            if substring(TT_EtikettLogg.Storl,i,1) = " " then
              TStorlek = TStorlek + "0". /* Space */
            else if substring(TT_EtikettLogg.Storl,i,1) = "." then
              NEXT. /* Punktum strippes bort. */
            else if (substring(TT_EtikettLogg.Storl,i,1) < "0" or
                     substring(TT_EtikettLogg.Storl,i,1) > "9") then
              TStorlek = TStorlek + "0". /* Alfanumeriske st|rrelser. */
            else
              TStorlek = TStorlek + substring(TT_EtikettLogg.Storl,i,1). /* Tar vare p} verdien */
        end.
        /*
        " 3.5"  "035"
        " 3"    "03"
        "35"    "35"
        "35.5"  "355"
        */
        ASSIGN TStorlek = TRIM(TStorlek) /* Trimmer bort ledende space og gjør lengden = 4. */
               TStorlek = (if LENGTH(TStorlek) = 2 then TStorlek + "0" else TStorlek)
               TStorlek = "0" + TStorlek.
        /* Initierer telleverk */

        assign
             blinje[1]  = "B,1,N," + string(TT_EtikettLogg.Ant) + "|" + chr(10)
             blinje[2]  = "1," + "~"" + substring(string(TT_EtikettLogg.pris,"zzz9.99"),1,4) + "~"" + "|" + chr(10)
             blinje[3]  = "2," + "~"" + substring(string(TT_EtikettLogg.pris,"zzz9.99"),6,2) + "~"" + "|" + chr(10)
             blinje[4]  = "3," + "~"" + string(TT_EtikettLogg.vg,"9999") + string(TT_EtikettLogg.lopnr,"9999") + TStorlek + "~"" + "|" + chr(10)
             blinje[5]  = "4," + "~"" + string(TT_EtikettLogg.vg) + "-" + string(TT_EtikettLogg.lopnr) + "~"" + "|" + chr(10)
             blinje[6]  = "5,""stl""|" + chr(10)
             blinje[7]  = "6," + "~"" + TT_EtikettLogg.Storl  + "~"" + "|" + chr(10)
             blinje[8]  = "7," + "~"" + " " + "~"" + "|" + chr(10) /* txt9 = string(hg,"9") */
             blinje[9]  = "8," + "~"" + string(TT_EtikettLogg.plunr,"zzzz") + "~"" + "|" + chr(10).
        IF RS-EtiTyp = 1 AND RS-Typ:SCREEN-VALUE IN FRAME FRAME-Typ1 = "1" THEN
                blinje[4]  = "3," + "~"" + string(tt_etikettlogg.EAN) + "~"" + "|" + chr(10).
        ELSE IF RS-EtiTyp = 2 AND RS-Typ2:SCREEN-VALUE IN FRAME FRAME-Typ2 = "1" THEN
                blinje[4]  = "3," + "~"" + string(tt_etikettlogg.EAN) + "~"" + "|" + chr(10).
        /* Ut med dr.... */
        do:
            /* wBatch informasjon. */
            wBatch = "~{".
            do i = 1 to 9:
                wBatch = wBatch + blinje[i].
            end.
            wBatch = wBatch + "~}".            

/*             /* ]pner stream */                            */
/*             output to value(wEtikett_Fil) no-echo append. */

            /* Skriver etikett. */
            put control wBatch.

/*             /* Lukker stream */ */
/*             output close.       */
        end.

        /* Resetter teller */
/*         teller = teller + TT_EtikettLogg.Ant. */
  end.
  OUTPUT CLOSE.
/*   output to teller no-echo. */
/*   export teller.            */
/*   output close.             */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UtskriftOld C-Win 
PROCEDURE UtskriftOld :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cTeller AS CHARACTER  NO-UNDO.
  /*Ut med alle etikettene i filen. */
  output to value(wEtikett_Fil) no-echo append.
    put control wLayout.
  output close.
  for each TT_EtikettLogg
     break by TT_EtikettLogg.vg
           by TT_EtikettLogg.lopnr
           by TT_EtikettLogg.Storl:

        /* Konverterer st|rrelsen.                   */
        /* Nb: Nullstiller alfanumeriske st|rrelser. */
        tstorlek = "".
        do i = 1 to length(TT_EtikettLogg.Storl):

            if substring(TT_EtikettLogg.Storl,i,1) = " " then
              TStorlek = TStorlek + "0". /* Space */

            else if substring(TT_EtikettLogg.Storl,i,1) = "." then
              NEXT. /* Punktum strippes bort. */

            else if (substring(TT_EtikettLogg.Storl,i,1) < "0" or
                     substring(TT_EtikettLogg.Storl,i,1) > "9") then
              TStorlek = TStorlek + "0". /* Alfanumeriske st|rrelser. */

            else
              TStorlek = TStorlek + substring(TT_EtikettLogg.Storl,i,1). /* Tar vare p} verdien */
        end.

        /*
        " 3.5"  "035"
        " 3"    "03"
        "35"    "35"
        "35.5"  "355"
        */

        ASSIGN /* Trimmer bort ledende space og gjør lengden = 4. */
          TStorlek = TRIM(TStorlek)
          TStorlek = (if LENGTH(TStorlek) = 2
                        then TStorlek + "0"
                        else TStorlek)
          TStorlek = "0" + TStorlek.

        /* Initierer telleverk */
        if search("teller") <> ? then
        do:
            input from teller no-echo.
            IMPORT UNFORMATTED cTeller.
            ASSIGN teller = INT(cTeller).
/*             set teller. */
            input close.
            if (teller = 0 or teller > 9999) then teller = 1.
        end.
        else teller = 1.

        blinje[1]  = "B,1,N," + string(TT_EtikettLogg.Ant) + "|" + chr(10).
        blinje[2]  = "1," + """"
                          + string((teller),"9999")
                          + """"
                          + "|" + chr(10).
        blinje[3]  = "2," + """"
                          + string((teller),"9999")
                          + """"
                          + "|" + chr(10).
        blinje[4]  = "3," + """"
                          + string(TT_EtikettLogg.pris,"zzzz9.99")
                          + """"
                          + "|" + chr(10).
        blinje[5]  = "4," + """"
                          + string(TT_EtikettLogg.pris,"zzzz9.99")
                          + """"
                          + "|" + chr(10).
        blinje[6]  = "5," + """"
                          + string(TT_EtikettLogg.vg,"999")
                          + string(TT_EtikettLogg.lopnr,"9999")
                          + "0"
                          + tstorlek
                          + """"
                          + "|" + chr(10).
        blinje[7]  = "6," + """"
                          + string(TT_EtikettLogg.vg,"999")
                          + string(TT_EtikettLogg.lopnr,"9999")
                          + "0"
                          + tstorlek
                          + """"
                          + "|" + chr(10).
        blinje[8]  = "7," + """"
                          + string(TT_EtikettLogg.vg)
                          + "-"
                          + string(TT_EtikettLogg.lopnr)
                          + """"
                          + "|" + chr(10).
        blinje[9]  = "8," + """"
                          + string(TT_EtikettLogg.vg)
                          + "-"
                          + string(TT_EtikettLogg.lopnr)
                          + """"
                          + "|" + chr(10).
        blinje[10]  = "9," + """"
                          + TT_EtikettLogg.storl
                          + """"
                          + "|" + chr(10).
        blinje[11]  = "10," + """"
                          + TT_EtikettLogg.storl
                          + """"
                          + "|" + chr(10).
                          

        /* Ut med dr.... */
        do:
            /* wBatch informasjon. */
            wBatch = "~{".
            do i = 1 to 11:
                wBatch = wBatch + blinje[i].
            end.
            wBatch = wBatch + "~}".            

            /* ]pner stream */
            output to value(wEtikett_Fil) no-echo append.

            /* Skriver etikett. */
            put control wBatch.

            /* Lukker stream */
            output close.

        end.

        /* Resetter teller */
        do:
            teller = teller + TT_EtikettLogg.Ant.
            output to teller no-echo.
            export teller.
            output close.
        end.
  end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FixChkEan C-Win 
FUNCTION FixChkEan RETURNS CHARACTER
    ( INPUT cKode AS CHARACTER ) :
  /*------------------------------------------------------------------------------
    Purpose:  Räknar ut checksiffra för ean EAN-kod - parameter utan chksiffra
              i.e 12 lång
      Notes:  
  ------------------------------------------------------------------------------*/
      DEF VAR iCount1 AS INTE NO-UNDO.
      DEF VAR iMulti  AS INTE INIT 1 NO-UNDO.
      DEF VAR iSum AS INTE NO-UNDO.
      DO iCount1 = LENGTH(cKode) TO 1 BY -1:  
          ASSIGN iMulti = IF iMulti = 1 THEN 3 ELSE 1
                 iSum = iSum + INT(SUBSTR(cKode,iCount1,1)) * iMulti.
      END.
      RETURN cKode + string((10 - iSum MODULO 10) MODULO 10).
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

