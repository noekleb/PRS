&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Parameteroppsett
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Parameteroppsett 
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
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
&ELSE
&ENDIF

/* Local Variable Definitions ---                                       */
def var wOk                as log    no-undo.
def var wOldRecid          as recid  no-undo.
def var hHandle            as handle no-undo.
def var hLabel             as handle no-undo.
def var wSubWin            as handle no-undo.
def var wLapTop            as log    no-undo.
def var wBekreft           as log    no-undo.
def var wFeil              as log    no-undo.
DEF VAR wSvar              AS LOG    NO-UNDO.

/* Variabler for håndtering av Tab-Stip. */
DEFINE VAR chTabs            AS COM-HANDLE NO-UNDO.
DEFINE VAR chTab             AS COM-HANDLE NO-UNDO.
define var chTab1Side        as com-handle no-undo.
DEFINE VAR wAktivFlip        AS INT        INIT 1 NO-UNDO. /* !! parameter !! */
DEFINE VAR wAktivFrame       AS INTE INIT  1.
DEFINE VAR wTabTekst         AS CHAR INIT "System,Artikkelreg,Lager og statistikk,Varetelling" NO-UNDO.
DEFINE VAR wTabHnd           AS HANDLE EXTENT 4 NO-UNDO.
DEFINE VAR wBrowseHandle     AS HANDLE NO-UNDO.

/* Buffere */
def buffer bMedlem for Medlem.
DEF BUFFER ledMedlem FOR Medlem.
def temp-table tmpChild 
  field wChild as handle.
{runlib.i}

DEF VAR cFileName        AS CHAR  NO-UNDO. 
DEF VAR cExcEkstent      AS CHAR  NO-UNDO.
DEF VAR cKunde           AS CHAR  NO-UNDO.
DEF VAR cPOS             AS CHAR  NO-UNDO.

DEF STREAM sExportFile.
{methodexcel.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn_Help B-Lagre B-Angre BUTTON-Eksport ~
BUTTON-Ok RECT-27 RECT-28 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Parameteroppsett AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE SUB-MENU m_Fil 
       MENU-ITEM m_Avslutt      LABEL "&Avslutt"       ACCELERATOR "ALT-F4".

DEFINE MENU MENU-BAR-C-ArtKort MENUBAR
       SUB-MENU  m_Fil          LABEL "&Fil"          .


/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE TabStrip AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chTabStrip AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Angre 
     IMAGE-UP FILE "icon/e-undo.bmp":U
     LABEL "&Angre" 
     SIZE 4.4 BY 1.1.

DEFINE BUTTON B-Lagre 
     IMAGE-UP FILE "icon/e-save.bmp":U
     LABEL "&Lagre" 
     SIZE 4.4 BY 1.1.

DEFINE BUTTON Btn_Help DEFAULT 
     IMAGE-UP FILE "icon\e-help":U NO-FOCUS FLAT-BUTTON
     LABEL "&Help" 
     SIZE 4.6 BY 1.05
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-Eksport 
     IMAGE-UP FILE "icon/excel.bmp":U
     LABEL "&Eksport" 
     SIZE 4.6 BY 1.1 TOOLTIP "Slett".

DEFINE BUTTON BUTTON-Ok 
     IMAGE-UP FILE "icon\e-exit":U NO-FOCUS FLAT-BUTTON
     LABEL "&Ok" 
     SIZE 4.6 BY 1.05 TOOLTIP "Lagre og avslutt".

DEFINE RECTANGLE RECT-27
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 159.2 BY .1.

DEFINE RECTANGLE RECT-28
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 159.4 BY .1.

DEFINE VARIABLE FI-DbId AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE FI-TekstDbId AS CHARACTER FORMAT "X(256)":U INITIAL "Database ID:" 
      VIEW-AS TEXT 
     SIZE 27 BY .62 NO-UNDO.

DEFINE VARIABLE FI-TekstHK AS CHARACTER FORMAT "X(256)":U INITIAL "HK innstallasjon:" 
      VIEW-AS TEXT 
     SIZE 27 BY .62 NO-UNDO.

DEFINE VARIABLE FI-TekstLogVPI AS CHARACTER FORMAT "X(256)":U INITIAL "Aktiver logging av VPI til HK" 
      VIEW-AS TEXT 
     SIZE 27 BY .62 NO-UNDO.

DEFINE VARIABLE T-HKInstall AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE T-LogVpi AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE CB-VPI AS INTEGER FORMAT ">9":U INITIAL 0 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1",0
     DROP-DOWN-LIST
     SIZE 40 BY 1 TOOLTIP "Åpne for sending av vare og prisinformasjon til HK" NO-UNDO.

DEFINE VARIABLE FI-TekstBildeIKasse AS CHARACTER FORMAT "X(256)":U INITIAL "Bilde i kasse" 
      VIEW-AS TEXT 
     SIZE 32 BY .62 NO-UNDO.

DEFINE VARIABLE FI-TekstDirO AS CHARACTER FORMAT "X(256)":U INITIAL "Direkte oppdatering av priser:" 
      VIEW-AS TEXT 
     SIZE 32 BY .62 NO-UNDO.

DEFINE VARIABLE FI-TekstGenEAN AS CHARACTER FORMAT "X(256)":U INITIAL "Generer EAN ved ny vare" 
      VIEW-AS TEXT 
     SIZE 32 BY .62 NO-UNDO.

DEFINE VARIABLE FI-TekstHk1 AS CHARACTER FORMAT "X(256)":U INITIAL "HK nummerserie:" 
      VIEW-AS TEXT 
     SIZE 32 BY .62 NO-UNDO.

DEFINE VARIABLE FI-TekstHk2 AS CHARACTER FORMAT "X(256)":U INITIAL "HK ekstranummerserie:" 
      VIEW-AS TEXT 
     SIZE 31 BY .62 NO-UNDO.

DEFINE VARIABLE FI-TekstHKInfo AS CHARACTER FORMAT "X(256)":U INITIAL "9000001-9999999" 
      VIEW-AS TEXT 
     SIZE 21 BY .62 NO-UNDO.

DEFINE VARIABLE FI-TekstHKInfo2 AS CHARACTER FORMAT "X(256)":U INITIAL "8500001-8999999" 
      VIEW-AS TEXT 
     SIZE 21 BY .62 TOOLTIP "Ekstranummerserie for VareId i en HK installasjon" NO-UNDO.

DEFINE VARIABLE FI-TekstLokalVre AS CHARACTER FORMAT "X(256)":U INITIAL "Tillat opprettelse av lokale varer" 
      VIEW-AS TEXT 
     SIZE 32 BY .62 NO-UNDO.

DEFINE VARIABLE FI-TekstNrs AS CHARACTER FORMAT "X(256)":U INITIAL "VareId nummerserier" 
      VIEW-AS TEXT 
     SIZE 26 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-TekstOLager AS CHARACTER FORMAT "X(256)":U INITIAL "On-Line lageroppdatering:" 
      VIEW-AS TEXT 
     SIZE 32 BY .62 NO-UNDO.

DEFINE VARIABLE FI-TekstOPris AS CHARACTER FORMAT "X(256)":U INITIAL "Åpen pris i kasse:" 
      VIEW-AS TEXT 
     SIZE 32 BY .62 NO-UNDO.

DEFINE VARIABLE FI-TekstVPI AS CHARACTER FORMAT "X(256)":U INITIAL "Sende VPI til HK" 
      VIEW-AS TEXT 
     SIZE 32 BY .62 NO-UNDO.

DEFINE RECTANGLE RECT-29
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 75 BY 2.62.

DEFINE VARIABLE T-BildeIKasse AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 5.6 BY .81 TOOLTIP "Defaultverdi på parameter Bilde i kasse" NO-UNDO.

DEFINE VARIABLE T-DirOppdat AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 5.6 BY .81 TOOLTIP "Ved innlegging av ny pris i priskø, vil priskø bli klargjort direkte." NO-UNDO.

DEFINE VARIABLE T-GenEANNyVre AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 5.6 BY .81 TOOLTIP "Defaultverdi på parameter Bilde i kasse" NO-UNDO.

DEFINE VARIABLE T-HKEkstra AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY .81 NO-UNDO.

DEFINE VARIABLE T-HKNrSerie AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY .81 TOOLTIP "Nummerserie for VareID i en HK installasjon" NO-UNDO.

DEFINE VARIABLE T-LokalVre AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 5.6 BY .81 TOOLTIP "Defaultverdi på parameter Bilde i kasse" NO-UNDO.

DEFINE VARIABLE T-OLager AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 5.6 BY .81 TOOLTIP "Defaultverdi på parameter On-Line lageroppdatering" NO-UNDO.

DEFINE VARIABLE T-OPris AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 5.6 BY .81 TOOLTIP "Defaultverdi på parameter Åpen pris i kasse" NO-UNDO.

DEFINE VARIABLE FI-StorsteButNr AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Største butikknummer" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE FI-TransTyper AS CHARACTER FORMAT "X(256)":U 
     LABEL "Tillatte transtyper i varetelling" 
     VIEW-AS FILL-IN 
     SIZE 39 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     Btn_Help AT ROW 1.33 COL 150.4 NO-TAB-STOP 
     B-Lagre AT ROW 1.24 COL 2.4
     B-Angre AT ROW 1.24 COL 7
     BUTTON-Eksport AT ROW 1.24 COL 12
     BUTTON-Ok AT ROW 1.33 COL 155.4 NO-TAB-STOP 
     RECT-27 AT ROW 1.1 COL 1.4
     RECT-28 AT ROW 2.43 COL 1.2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 159.8 BY 26.81.

DEFINE FRAME FRAME-3
     FI-StorsteButNr AT ROW 1.48 COL 41 COLON-ALIGNED
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 4.33
         SIZE 156 BY 23.1.

DEFINE FRAME FRAME-4
     FI-TransTyper AT ROW 1.71 COL 32 COLON-ALIGNED
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 4.33
         SIZE 156 BY 23.1.

DEFINE FRAME FRAME-1
     FI-DbId AT ROW 1.71 COL 31 COLON-ALIGNED NO-LABEL
     T-HKInstall AT ROW 2.91 COL 33
     T-LogVpi AT ROW 3.86 COL 33
     FI-TekstDbId AT ROW 1.81 COL 3 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     FI-TekstHK AT ROW 2.95 COL 3 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     FI-TekstLogVPI AT ROW 3.95 COL 3 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 4.33
         SIZE 156 BY 23.1.

DEFINE FRAME FRAME-2
     T-DirOppdat AT ROW 2.1 COL 41
     T-HKNrSerie AT ROW 4.1 COL 41
     T-HKEkstra AT ROW 5.1 COL 41
     T-OPris AT ROW 6.95 COL 41
     T-OLager AT ROW 7.91 COL 41
     T-BildeIKasse AT ROW 8.86 COL 41
     CB-VPI AT ROW 9.81 COL 39 COLON-ALIGNED NO-LABEL
     T-LokalVre AT ROW 11 COL 41
     T-GenEANNyVre AT ROW 12 COL 41
     FI-TekstDirO AT ROW 2.19 COL 5 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     FI-TekstNrs AT ROW 3.24 COL 5 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     FI-TekstHk1 AT ROW 4.1 COL 5 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     FI-TekstHk2 AT ROW 5.1 COL 5 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     FI-TekstOPris AT ROW 7 COL 5 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     FI-TekstOLager AT ROW 7.95 COL 5 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     FI-TekstBildeIKasse AT ROW 8.91 COL 5 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     FI-TekstVPI AT ROW 10 COL 5 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     FI-TekstLokalVre AT ROW 11 COL 5 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     FI-TekstGenEAN AT ROW 12 COL 5 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     FI-TekstHKInfo AT ROW 4.14 COL 43 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     FI-TekstHKInfo2 AT ROW 5.14 COL 43 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     RECT-29 AT ROW 3.62 COL 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 4.33
         SIZE 156 BY 23.1.


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
  CREATE WINDOW C-Parameteroppsett ASSIGN
         HIDDEN             = YES
         TITLE              = "Parameteroppsett"
         HEIGHT             = 26.81
         WIDTH              = 159.8
         MAX-HEIGHT         = 26.81
         MAX-WIDTH          = 159.8
         VIRTUAL-HEIGHT     = 26.81
         VIRTUAL-WIDTH      = 159.8
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

ASSIGN {&WINDOW-NAME}:MENUBAR    = MENU MENU-BAR-C-ArtKort:HANDLE.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Parameteroppsett
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* REPARENT FRAME */
ASSIGN FRAME FRAME-1:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME FRAME-2:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME FRAME-3:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME FRAME-4:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
ASSIGN 
       FRAME DEFAULT-FRAME:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME FRAME-1
   L-To-R,COLUMNS                                                       */
/* SETTINGS FOR FILL-IN FI-TekstDbId IN FRAME FRAME-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-TekstHK IN FRAME FRAME-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-TekstLogVPI IN FRAME FRAME-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME FRAME-2
   L-To-R,COLUMNS                                                       */
/* SETTINGS FOR FILL-IN FI-TekstBildeIKasse IN FRAME FRAME-2
   NO-ENABLE                                                            */
ASSIGN 
       FI-TekstBildeIKasse:READ-ONLY IN FRAME FRAME-2        = TRUE.

/* SETTINGS FOR FILL-IN FI-TekstDirO IN FRAME FRAME-2
   NO-ENABLE                                                            */
ASSIGN 
       FI-TekstDirO:READ-ONLY IN FRAME FRAME-2        = TRUE.

/* SETTINGS FOR FILL-IN FI-TekstGenEAN IN FRAME FRAME-2
   NO-ENABLE                                                            */
ASSIGN 
       FI-TekstGenEAN:READ-ONLY IN FRAME FRAME-2        = TRUE.

/* SETTINGS FOR FILL-IN FI-TekstHk1 IN FRAME FRAME-2
   NO-ENABLE                                                            */
ASSIGN 
       FI-TekstHk1:READ-ONLY IN FRAME FRAME-2        = TRUE.

/* SETTINGS FOR FILL-IN FI-TekstHk2 IN FRAME FRAME-2
   NO-ENABLE                                                            */
ASSIGN 
       FI-TekstHk2:READ-ONLY IN FRAME FRAME-2        = TRUE.

/* SETTINGS FOR FILL-IN FI-TekstHKInfo IN FRAME FRAME-2
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-TekstHKInfo2 IN FRAME FRAME-2
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-TekstLokalVre IN FRAME FRAME-2
   NO-ENABLE                                                            */
ASSIGN 
       FI-TekstLokalVre:READ-ONLY IN FRAME FRAME-2        = TRUE.

/* SETTINGS FOR FILL-IN FI-TekstNrs IN FRAME FRAME-2
   NO-ENABLE                                                            */
ASSIGN 
       FI-TekstNrs:READ-ONLY IN FRAME FRAME-2        = TRUE.

/* SETTINGS FOR FILL-IN FI-TekstOLager IN FRAME FRAME-2
   NO-ENABLE                                                            */
ASSIGN 
       FI-TekstOLager:READ-ONLY IN FRAME FRAME-2        = TRUE.

/* SETTINGS FOR FILL-IN FI-TekstOPris IN FRAME FRAME-2
   NO-ENABLE                                                            */
ASSIGN 
       FI-TekstOPris:READ-ONLY IN FRAME FRAME-2        = TRUE.

/* SETTINGS FOR FILL-IN FI-TekstVPI IN FRAME FRAME-2
   NO-ENABLE                                                            */
ASSIGN 
       FI-TekstVPI:READ-ONLY IN FRAME FRAME-2        = TRUE.

/* SETTINGS FOR FRAME FRAME-3
   L-To-R,COLUMNS                                                       */
/* SETTINGS FOR FRAME FRAME-4
   L-To-R,COLUMNS                                                       */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Parameteroppsett)
THEN C-Parameteroppsett:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-1
/* Query rebuild information for FRAME FRAME-1
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME FRAME-1 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-2
/* Query rebuild information for FRAME FRAME-2
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME FRAME-2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-3
/* Query rebuild information for FRAME FRAME-3
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME FRAME-3 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-4
/* Query rebuild information for FRAME FRAME-4
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME FRAME-4 */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME TabStrip ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 2.91
       COLUMN          = 2
       HEIGHT          = 24.91
       WIDTH           = 158
       HIDDEN          = no
       SENSITIVE       = yes.
      TabStrip:NAME = "TabStrip":U .
/* TabStrip OCXINFO:CREATE-CONTROL from: {1EFB6596-857C-11D1-B16A-00C0F0283628} type: TabStrip */
      TabStrip:MOVE-AFTER(BUTTON-Eksport:HANDLE IN FRAME DEFAULT-FRAME).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Parameteroppsett
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Parameteroppsett C-Parameteroppsett
ON END-ERROR OF C-Parameteroppsett /* Parameteroppsett */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Parameteroppsett C-Parameteroppsett
ON WINDOW-CLOSE OF C-Parameteroppsett /* Parameteroppsett */
DO:

  if can-find(first tmpChild where
               valid-handle(tmpChild.wChild)) then
    do:
      wBekreft = false.
      message 'Det er startet andre programmer fra dette vinduet.' skip
              'Avsluttes dette vinduet, vil alle underliggende programmer' skip
              'også bli avsluttet.'
              view-as alert-box warning buttons yes-no title 'Bekreft avsluttning'
              update wBekreft
              .
    end.
  else wBekreft = true.
  if wBekreft <> true then 
  return no-apply.
  
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  /* RETURN NO-APPLY. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Angre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Angre C-Parameteroppsett
ON CHOOSE OF B-Angre IN FRAME DEFAULT-FRAME /* Angre */
DO:
  wSvar = FALSE.
  MESSAGE "Alle endringer (siden siste lagring) vil bli opphevet?" SKIP
          "Gjelder også endringer utført på de andre flikene."
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Bekreft angre"
      UPDATE wSvar.
  IF wSvar = FALSE THEN
      RETURN NO-APPLY.
  RUN VisPost.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Lagre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Lagre C-Parameteroppsett
ON CHOOSE OF B-Lagre IN FRAME DEFAULT-FRAME /* Lagre */
DO:
  RUN LagrePost.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help C-Parameteroppsett
ON CHOOSE OF Btn_Help IN FRAME DEFAULT-FRAME /* Help */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  run WinHlp.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Eksport
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Eksport C-Parameteroppsett
ON CHOOSE OF BUTTON-Eksport IN FRAME DEFAULT-FRAME /* Eksport */
DO:
  RUN Utskrift.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Ok C-Parameteroppsett
ON CHOOSE OF BUTTON-Ok IN FRAME DEFAULT-FRAME /* Ok */
DO:
  if can-find(first tmpChild where
               valid-handle(tmpChild.wChild)) then
    do:
      wBekreft = false.
      message 'Det er startet andre programmer fra dette vinduet.' skip
              'Avsluttes dette vinduet, vil alle underliggende programmer' skip
              'også bli avsluttet.'
              view-as alert-box warning buttons yes-no title 'Bekreft avsluttning'
              update wBekreft
              .
    end.
  else wBekreft = true.
  if wBekreft <> true then
    return no-apply.

  APPLY "choose":U TO B-Lagre.
  
  APPLY "CLOSE":U TO THIS-PROCEDURE.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-2
&Scoped-define SELF-NAME T-BildeIKasse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-BildeIKasse C-Parameteroppsett
ON RETURN OF T-BildeIKasse IN FRAME FRAME-2
DO:
    APPLY "TAB" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-DirOppdat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-DirOppdat C-Parameteroppsett
ON RETURN OF T-DirOppdat IN FRAME FRAME-2
DO:
    APPLY "TAB" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-GenEANNyVre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-GenEANNyVre C-Parameteroppsett
ON RETURN OF T-GenEANNyVre IN FRAME FRAME-2
DO:
    APPLY "TAB" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-HKEkstra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-HKEkstra C-Parameteroppsett
ON VALUE-CHANGED OF T-HKEkstra IN FRAME FRAME-2
DO:
  IF T-HkEkstra:SCREEN-VALUE = "yes" THEN
      T-HkNrSerie:SENSITIVE = FALSE.
  ELSE
      T-HkNrSerie:SENSITIVE = true.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-1
&Scoped-define SELF-NAME T-HKInstall
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-HKInstall C-Parameteroppsett
ON VALUE-CHANGED OF T-HKInstall IN FRAME FRAME-1
DO:
  IF SELF:SCREEN-VALUE = "yes" THEN
      ASSIGN
      T-HKNrSerie:SCREEN-VALUE IN FRAME FRAME-2 = "yes"
      .
  ELSE
      ASSIGN
      T-HKNrSerie:SCREEN-VALUE IN FRAME FRAME-2 = "no"
      T-HKEkstra:SCREEN-VALUE IN FRAME FRAME-2 = "no"
      .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-2
&Scoped-define SELF-NAME T-HKNrSerie
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-HKNrSerie C-Parameteroppsett
ON VALUE-CHANGED OF T-HKNrSerie IN FRAME FRAME-2
DO:
  IF T-HkNrSerie:SCREEN-VALUE = "yes" THEN
      T-HkEkstra:SENSITIVE = TRUE.
  ELSE
      T-HkEkstra:SENSITIVE = false.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-1
&Scoped-define SELF-NAME T-LogVpi
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-LogVpi C-Parameteroppsett
ON VALUE-CHANGED OF T-LogVpi IN FRAME FRAME-1
DO:
  IF SELF:SCREEN-VALUE = "yes" THEN
      ASSIGN
      T-HKNrSerie:SCREEN-VALUE IN FRAME FRAME-2 = "yes"
      .
  ELSE
      ASSIGN
      T-HKNrSerie:SCREEN-VALUE IN FRAME FRAME-2 = "no"
      T-HKEkstra:SCREEN-VALUE IN FRAME FRAME-2 = "no"
      .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-2
&Scoped-define SELF-NAME T-LokalVre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-LokalVre C-Parameteroppsett
ON RETURN OF T-LokalVre IN FRAME FRAME-2
DO:
    APPLY "TAB" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-OLager
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-OLager C-Parameteroppsett
ON RETURN OF T-OLager IN FRAME FRAME-2
DO:
    APPLY "TAB" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-OPris
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-OPris C-Parameteroppsett
ON RETURN OF T-OPris IN FRAME FRAME-2
DO:
    APPLY "TAB" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME TabStrip
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TabStrip C-Parameteroppsett OCX.Click
PROCEDURE TabStrip.TabStrip.Click .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/

IF wAktivFlip <> INT(chTabStrip:SelectedItem:Index) THEN 
DO:
    ASSIGN wAktivFlip = INT(chTabStrip:SelectedItem:Index).
    RUN ByttFrame. /* Bytter tab */
    RETURN NO-APPLY.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Parameteroppsett 


/* ***************************  Main Block  *************************** */
/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* Biblotek, inn/utmelding og ON CLOSE OF THIS... */
{genlib.i 
  &NoLibCall      = "Nei"
  &WindowName     = "MedlemsKlubbStatus"
  &PostIClose     = "IF VALID-HANDLE(chTabStrip) THEN
                        RELEASE OBJECT chTabStrip NO-ERROR.
                    IF VALID-HANDLE(chTabs) THEN
                        RELEASE OBJECT chTabs NO-ERROR.
                    IF VALID-HANDLE(chTab) THEN
                        RELEASE OBJECT chTab NO-ERROR.
                    IF VALID-HANDLE(chTab1Side) THEN
                        RELEASE OBJECT chTab1Side NO-ERROR.
                    IF VALID-HANDLE(TabStrip) THEN
                        DELETE OBJECT TabStrip NO-ERROR.
                    ASSIGN TabStrip    = ?
                           chTabStrip  = ?
                           chTabs      = ?
                           chTab       = ?
                           chTab1Side  = ?.

                    "
}
/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* HotKeySøk - DYYYYRT */
on "CTRL-TAB":U anywhere
  do:
    chTab1Side = chTabs:Item ((IF wAktivFlip = NUM-ENTRIES(wTabTekst)
                      THEN 1 ELSE wAktivFlip + 1) BY-VARIANT-POINTER).
    chTabStrip:SelectedItem = chTab1Side.
    RETURN NO-APPLY.
  end.

{syspara.i 1 1  10 cExcEkstent}
cExcEkstent = if cExcEkstent = "" then "sdv" else cExcEkstent.   
{syspara.i 1 10  1 cKunde}
{syspara.i 1 10  2 cPOS}

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  RUN enable_UI.
  {lng.i} /* Oversettelse */
  
  ASSIGN {&WINDOW-NAME}:HIDDEN = NO
         wTabHnd[1] = FRAME FRAME-1:HANDLE
         wTabHnd[2] = FRAME FRAME-2:HANDLE
         wTabHnd[3] = FRAME FRAME-3:HANDLE
         wTabHnd[4] = FRAME FRAME-4:HANDLE
         .
  RUN SetCB-VPI.
  run ByttFrame. /* Legger opp f›rste fane. */ 
  RUN VisPost.
/*   assign chTab1Side = chCtrlFrame:TabStrip. /* COM-Handl 1. side */ */
  assign chTab1Side = chTabStrip. /* COM-Handl 1. side */
  
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByttFrame C-Parameteroppsett 
PROCEDURE ByttFrame :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF VAR wInputRecid AS RECID NO-UNDO.

  ASSIGN
      wInputRecid = ?.
  /*
  if valid-handle(wSubWin) then
            delete procedure wSubWin.
  */
  IF wAktivFlip <> INT(chTabStrip:SelectedItem:Index) OR 
     INT(chTabStrip:SelectedItem:Index) = 1 THEN
  DO:
    FRAME DEFAULT-FRAME:MOVE-TO-TOP().
    wTabHnd[wAktivFlip]:MOVE-TO-TOP().
    APPLY "ENTRY" TO FI-DbId IN FRAME Frame-1.
  END.
  IF wAktivFlip = 2 THEN 
    DO:
      FRAME DEFAULT-FRAME:MOVE-TO-TOP().
      wTabHnd[wAktivFlip]:MOVE-TO-TOP().
      APPLY "ENTRY" TO T-DirOppdat IN FRAME Frame-2.
      IF T-HKInstall:SCREEN-VALUE IN FRAME FRAME-1 = "yes" THEN
      DO:
        IF T-HkNrSerie:SCREEN-VALUE = "yes" THEN
        DO:
            T-HkEkstra:SENSITIVE = TRUE.
            IF T-HkEkstra:SCREEN-VALUE = "yes" THEN
                T-HkNrSerie:SENSITIVE = false.
        END.
        ELSE DO:
            T-HkEkstra:SENSITIVE = FALSE.
        END.
      END.
      ELSE DO:
          ASSIGN
              T-HkNrSerie:SENSITIVE = false
              T-HkEkstra:SENSITIVE  = FALSE
              .
      END.

      /*
      run w-stlinje.w persistent set wSubWin (wInputRecid,"9999999999999","MEDLEMTOT","MANED",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE).
      */
    END.
  IF wAktivFlip = 3 THEN 
    DO:
      FRAME DEFAULT-FRAME:MOVE-TO-TOP().
      wTabHnd[wAktivFlip]:MOVE-TO-TOP().
      APPLY "ENTRY" TO FI-StorsteButNr IN FRAME Frame-3.
      /*
      run w-btotmedtrans.w persistent set wSubWin (THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE).
      */
    END.
  IF wAktivFlip = 4 THEN
    DO:
      FRAME DEFAULT-FRAME:MOVE-TO-TOP().
      wTabHnd[wAktivFlip]:MOVE-TO-TOP().
      APPLY "ENTRY" TO FI-TransTyper IN FRAME Frame-4.

    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load C-Parameteroppsett  _CONTROL-LOAD
PROCEDURE control_load :
/*------------------------------------------------------------------------------
  Purpose:     Load the OCXs    
  Parameters:  <none>
  Notes:       Here we load, initialize and make visible the 
               OCXs in the interface.                        
------------------------------------------------------------------------------*/

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN
DEFINE VARIABLE UIB_S    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE OCXFile  AS CHARACTER  NO-UNDO.

OCXFile = SEARCH( "w-parameteroppsett.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chTabStrip = TabStrip:COM-HANDLE
    UIB_S = chTabStrip:LoadControls( OCXFile, "TabStrip":U)
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "w-parameteroppsett.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DelTmpChild C-Parameteroppsett 
PROCEDURE DelTmpChild :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    for each tmpChild:
        if valid-handle(tmpChild.wChild) then do:
            RUN DelTmpChild IN tmpChild.wChild NO-ERROR.
            delete procedure tmpChild.wChild.
        end.
    end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Parameteroppsett  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Parameteroppsett)
  THEN DELETE WIDGET C-Parameteroppsett.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Parameteroppsett  _DEFAULT-ENABLE
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
  RUN control_load.
  ENABLE Btn_Help B-Lagre B-Angre BUTTON-Eksport BUTTON-Ok RECT-27 RECT-28 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Parameteroppsett.
  VIEW FRAME DEFAULT-FRAME IN WINDOW C-Parameteroppsett.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  DISPLAY FI-DbId T-HKInstall T-LogVpi FI-TekstDbId FI-TekstHK FI-TekstLogVPI 
      WITH FRAME FRAME-1 IN WINDOW C-Parameteroppsett.
  ENABLE FI-DbId T-HKInstall T-LogVpi 
      WITH FRAME FRAME-1 IN WINDOW C-Parameteroppsett.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-1}
  DISPLAY T-DirOppdat T-HKNrSerie T-HKEkstra T-OPris T-OLager T-BildeIKasse 
          CB-VPI T-LokalVre T-GenEANNyVre FI-TekstDirO FI-TekstNrs FI-TekstHk1 
          FI-TekstHk2 FI-TekstOPris FI-TekstOLager FI-TekstBildeIKasse 
          FI-TekstVPI FI-TekstLokalVre FI-TekstGenEAN FI-TekstHKInfo 
          FI-TekstHKInfo2 
      WITH FRAME FRAME-2 IN WINDOW C-Parameteroppsett.
  ENABLE T-DirOppdat T-HKNrSerie T-HKEkstra T-OPris T-OLager T-BildeIKasse 
         CB-VPI T-LokalVre T-GenEANNyVre RECT-29 
      WITH FRAME FRAME-2 IN WINDOW C-Parameteroppsett.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-2}
  DISPLAY FI-StorsteButNr 
      WITH FRAME FRAME-3 IN WINDOW C-Parameteroppsett.
  ENABLE FI-StorsteButNr 
      WITH FRAME FRAME-3 IN WINDOW C-Parameteroppsett.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-3}
  DISPLAY FI-TransTyper 
      WITH FRAME FRAME-4 IN WINDOW C-Parameteroppsett.
  ENABLE FI-TransTyper 
      WITH FRAME FRAME-4 IN WINDOW C-Parameteroppsett.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-4}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initialize-Controls C-Parameteroppsett 
PROCEDURE initialize-Controls :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR wCount AS INTE NO-UNDO.
  ASSIGN chTabStrip = chTabStrip:TabStrip
         chTabs     = chTabStrip:Tabs
         chTabs:Item(1):Caption = ENTRY(1,wTabTekst).
  DO wCount = 2 TO NUM-ENTRIES(wTabTekst):
      chTabs:Add(wCount,,ENTRY(wCount,wTabTekst)).
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LagreParameter C-Parameteroppsett 
PROCEDURE LagreParameter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piSysHId LIKE SysPara.SysHId      NO-UNDO.
  DEF INPUT PARAMETER piSysGr  LIKE SysPara.SysGr       NO-UNDO.
  DEF INPUT PARAMETER piParaNr LIKE SysPara.ParaNr      NO-UNDO.
  DEF INPUT PARAMETER pcParaTx LIKE SysPara.Beskrivelse NO-UNDO.
  DEF INPUT PARAMETER piHandle AS HANDLE                NO-UNDO.

  DEF var pcVerdi  AS CHAR NO-UNDO.
  DEF var pcLabel  AS CHAR NO-UNDO.

  DEF BUFFER pSysPara FOR SysPara.

  ASSIGN
      pcVerdi = piHandle:SCREEN-VALUE
      pcLabel = piHandle:LABEL
      .

  DO TRANSACTION:
    FIND pSysPara EXCLUSIVE-LOCK WHERE
        pSysPara.SysHId   = piSysHId   AND
        pSysPara.SysGr    = piSysGr    AND
        pSysPara.ParaNr   = piParaNr   NO-ERROR.
    IF NOT AVAILABLE pSysPara THEN
    DO:
        CREATE pSysPara.
        ASSIGN
            pSysPara.SysHId      = piSysHId  
            pSysPara.SysGr       = piSysGr   
            pSysPara.ParaNr      = piParaNr
            pSysPara.Beskrivelse = (IF pcParaTx <> ""
                                      THEN pcParaTx
                                      ELSE pcLabel)
            .
    END.
    ASSIGN
        pSysPara.Parameter1 = pcVerdi
        .

  END. /* TRANSACTION */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LagrePost C-Parameteroppsett 
PROCEDURE LagrePost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
     Parametre legges under SysHID = 100. 
     Flik  1 - Gruppe 10 
     Flik  2 - Gruppe 11 
     Flik  3 - Gruppe 12 
     Flik  4 - Gruppe 13 
    
------------------------------------------------------------------------------*/
/*
piSysHId
piSysGr 
piParaNr
pcVerdi 
pcLabel 

*/
  DEF VAR pcTekst AS CHAR NO-UNDO.  

  /* Parametre legges under SysHID = 100. */
  RUN SysHode (100, "Systemvedlikehold").

  /* Flik  1 */
  DO WITH FRAME Frame-1:
    RUN SysGruppe (100, 10, "Systemparametre - oppstart/inn").
    RUN LagreParameter (  1, 1, 14, 'Database ID',  FI-DbId:handle).
    RUN LagreParameter (  1, 1, 18, 'HK innstallasjon', T-HkInstall:handle).
    RUN LagreParameter (  1, 1, 22, 'Aktiver logging av VPI til HK', T-LogVpi:handle).
  END.

  /* Flik  2 */
  DO WITH FRAME Frame-2:
    ASSIGN
        pcTekst = ((IF T-HKNrSerie:SCREEN-VALUE = 'yes'
                      THEN FI-TekstHKInfo:SCREEN-VALUE
                      ELSE FI-TekstHKInfo:SCREEN-VALUE) + 
                   (IF T-HKEkstra:SCREEN-VALUE = 'yes'
                      THEN ',' 
                      ELSE '') +
                   (IF T-HKEkstra:SCREEN-VALUE = 'yes'
                      THEN FI-TekstHKInfo2:SCREEN-VALUE
                      ELSE '')
                  )
        .
    RUN SysGruppe      (  2,  1, "Kalulasjonsparametre").
    RUN LagreParameter (  2,  1, 10, 'Direkte oppdatering av priser', T-DirOppdat:handle).
    RUN LagreParameter (  1,  1, 19, 'HK nummerserie', T-HkNrSerie:handle).
    RUN LagreParameter (  1,  1, 20, 'HK ekstranummerseie', T-HkEkstra:handle).
    RUN LagreParameter (  1,  1, 21, 'HK Nummerserie', FI-TekstHKInfo:handle).
    RUN LagreParameter (  2,  4,  2, 'Åpen pris i kasse', T-OPris:handle).
    RUN LagreParameter (  2,  4,  3, 'On-Line lageroppdatering', T-OLager:handle).
    RUN LagreParameter (  2,  4,  4, 'Bilde i kasse', T-BildeIKasse:handle).
    {setsyspara.i 1 1 21 pcTekst} /* Nummerserier */
    RUN LagreParameter (  2,  4,  6, 'Sende VPI til HK', CB-VPI:handle).
    RUN LagreParameter (  2,  4,  7, 'Tillat opprettelse av lokale varer', T-LokalVre:handle).
    RUN LagreParameter (  2,  4,  8, 'Generer EAN ved ny vare', T-GenEANNyVre:handle).
  END.

  /* Flik  3 */
  DO WITH FRAME Frame-3:
    RUN SysGruppe (  3, 1, "Lager og statistikkoppdatering").
    RUN LagreParameter (  3,  1,  2, 'Største butikknummer', FI-StorsteButNr:handle).
  END.
  
  /* Flik  4 - Gruppe 13 */
  DO WITH FRAME Frame-4:
    RUN SysGruppe (  4,  1, "Varetelling").
    RUN LagreParameter (  4,  1, 1, 'Tillatte transtyper i varetelling', FI-TransTyper:handle).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NullStill C-Parameteroppsett 
PROCEDURE NullStill :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  CLEAR FRAME Frame-1.
  CLEAR FRAME Frame-2.
  CLEAR FRAME Frame-3.
  CLEAR FRAME Frame-4.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetCB-VPI C-Parameteroppsett 
PROCEDURE SetCB-VPI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR pcTekst AS CHAR NO-UNDO.

{syspara.i 2 4 5 pcTekst}
IF pcTekst = "" THEN
DO:
    pcTekst = "Ingen rapportering,0,Manuell rapportering,1,Automatisk rapportering,2".
    {setsyspara.i 2 4 5 pcTekst '' '' 'Valg for send VPI til HK'}
END.
ASSIGN
    CB-VPI:LIST-ITEM-PAIRS IN FRAME FRAME-2 = pcTekst
    CB-VPI:SCREEN-VALUE IN FRAME FRAME-2 = ENTRY(2,pcTekst)
    .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapaTmpChild C-Parameteroppsett 
PROCEDURE SkapaTmpChild :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipChild AS HANDLE NO-UNDO.
    IF VALID-HANDLE(ipChild) THEN DO:
        CREATE tmpChild.
        ASSIGN tmpChild.wChild = ipChild.
        RELEASE tmpChild.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SysGruppe C-Parameteroppsett 
PROCEDURE SysGruppe :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piSysHId      AS INT NO-UNDO.
  DEF INPUT PARAMETER piSysGr       AS INT NO-UNDO.
  DEF INPUT PARAMETER pcBeskrivelse AS CHAR NO-UNDO.

  DEF BUFFER pSysGruppe FOR SysGruppe.

  DO TRANSACTION:
      FIND pSysGruppe EXCLUSIVE-LOCK WHERE
          pSysGruppe.SysHId   = piSysHId  and
          pSysGruppe.SysGr    = piSysGr   NO-ERROR.
      IF NOT AVAILABLE pSysGruppe THEN
      DO:
          CREATE pSysGruppe.
          ASSIGN
            pSysGruppe.SysHId      = piSysHId 
            pSysGruppe.SysGr       = piSysGr
            pSysGruppe.Beskrivelse = pcBeskrivelse.
            .
      END.
  END. /* TRANSACTION */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SysHode C-Parameteroppsett 
PROCEDURE SysHode :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piSysHId      AS INT NO-UNDO.
  DEF INPUT PARAMETER pcBeskrivelse AS CHAR NO-UNDO.

  DEF BUFFER pSysHode FOR SysHode.

  DO TRANSACTION:
      FIND pSysHode EXCLUSIVE-LOCK WHERE
          pSysHode.SysHId   = piSysHId NO-ERROR.
      IF NOT AVAILABLE pSysHode THEN
      DO:
          CREATE pSysHode.
          ASSIGN
            pSysHode.SysHId   = piSysHId 
            pSysHode.Beskrivelse = pcBeskrivelse.
            .
      END.
  END. /* TRANSACTION */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Utskrift C-Parameteroppsett 
PROCEDURE Utskrift :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DEF VAR piAntPoster AS INT  NO-UNDO.
  DEF VAR pcChr10     AS CHAR NO-UNDO.
  DEF VAR pcTekst     AS CHAR NO-UNDO.

  ASSIGN
    pcChr10 = CHR(10)
    .

  {sww.i}
  
  /* Finner temporært filnavn. */
  if valid-handle(wLibHandle) then
    run GetTempFileName in wLibHandle ("Para", cExcEkstent, output cFileName). 
  
  /* Åpner stream */
  OUTPUT STREAM sExportFile TO VALUE(cFileName) NO-ECHO.
  
  /* Legger ut overskrifter. */
  STATUS DEFAULT "Eksporterer data...".
  EXPORT STREAM sExportFile DELIMITER ";"
    "SYSTEMPARAMETRE"  
    " "
    " "
    " "
    " "
    ""
    SKIP.                                 
  EXPORT STREAM sExportFile DELIMITER ";"
    "Parametergruppe"
    "SysHId"
    "SysGr"
    "ParaNr"
    "Beskrivelse"
    "Parameter1"
    "Parameter2"
    "Hjelpetekst1"
    "HjelpeTekst"
    SKIP.                                 
                                  
  /* Eksporterer data */
  EKSPORT:
  FOR EACH SysPara NO-LOCK WHERE
      SysPara.SysHId = 100 AND
      SysPara.SysGr >= 10 AND
      SysPara.SysGr <= 99
      BREAK BY SysPara.SysHId
            BY SysPara.SysGr
            BY SysPara.ParaNr:

    FIND SysGruppe NO-LOCK WHERE
      SysGruppe.SysHId   = SysPara.SysHId AND
      SysGruppe.SysGr    = SysPara.SysGr NO-ERROR.

    ASSIGN
        piAntPoster = piAntPoster + 1
        .

    EXPORT STREAM sExportFile DELIMITER ";"
      (IF AVAILABLE SysGruppe
         THEN SysGruppe.Beskrivelse
         ELSE "")
      SysPara.SysHId
      SysPara.SysGr
      SysPara.ParaNr
      SysPara.Beskrivelse
      SysPara.Parameter1
      SysPara.Parameter2
      SysPara.Hjelpetekst1
      SysPara.HjelpeTekst2
      .
                                
  END. /* EKSPORT */
                                  
  /* Lukker stream */
  OUTPUT STREAM sExportFile CLOSE.
  
  STATUS DEFAULT "Importerer data i Excel...".
  CREATE "Excel.Application" chExcelApplication.  
  chExcelApplication:Visible = FALSE.                                     
  chWorkbooks = chExcelApplication:Workbooks:OpenText(cFileName,2,1,1,1,1,FALSE,TRUE,FALSE,FALSE,FALSE).
 
  STATUS DEFAULT "Setter aktivt ark...".
  chWorkSheets = chExcelApplication:Sheets:Item(1).
 
  STATUS DEFAULT "Setter overskrift...".
  chWorkSheets:Range("A1:AI2"):Font:Bold = TRUE.
  chWorkSheets:Range("A1:AI2"):Font:Italic = TRUE.

  STATUS DEFAULT "Blokkinndeling...".
  chWorkSheets:Range("C:C"):borders(10):LineStyle     = 9. /*** Dobbelt linje ****/

  STATUS DEFAULT "Setter nummerformat...".
  chWorkSheets:Range("E:I"):NumberFormat = "@".
  /*
  chWorkSheets:Range("F:F"):NumberFormat = "# ##0,0".
  chWorkSheets:Range("G:G"):NumberFormat = "# ##0,00".
  */

  STATUS DEFAULT "Setter overskrift...".
  /* chWorkSheets:Range("A1:B1"):Merge(). */
  chWorkSheets:Range("A1:B1"):HorizontalAlignment = 3.
  chWorkSheets:Range("E1:F1"):HorizontalAlignment = 3.

  /*      
  chWorkSheets:Range("C2:C2"):HorizontalAlignment = 4.
  chWorkSheets:Range("E2:E2"):HorizontalAlignment = 4.
  chWorkSheets:Range("E2:E2"):HorizontalAlignment = 4.   
  
  chWorkSheets:Range("F2:F2"):HorizontalAlignment = 4.   
  
  chWorkSheets:Range("G2:G2"):HorizontalAlignment = 4.
  
  chWorkSheets:Range("H2:H2"):HorizontalAlignment = 4.   

  chWorkSheets:Range("I2:I2"):HorizontalAlignment = 4.
  
  chWorkSheets:Range("J2:J2"):HorizontalAlignment = 4.   
  */

  STATUS DEFAULT "Setter AutoFit...".
  chWorkSheets:Columns("A:AI"):AutoFit().

  STATUS DEFAULT "Setter Kriterier...".
  chWorkSheets:PageSetup:PrintTitleRows = "A1:AI2".  
  chWorkSheets:PageSetup:LeftHeader     = "Kriterier - <Blank>".
  chWorkSheets:PageSetup:RightHeader    = "Antall poster: " + String(piAntPoster).
  chWorkSheets:PageSetup:LeftFooter     = cKunde.
  chWorkSheets:PageSetup:RightFooter    = cPOS.
  chWorksheets:PageSetup:PrintArea      = "A:AI".
  chWorkSheets:PageSetup:Orientation    = 2. /*LAndscape */
  chWorkSheets:PageSetup:FitToPagesWide = 1.
  
  STATUS DEFAULT "Setter FreezePanes...".
  chWorkSheets:Range("D3"):Select().
  chExcelApplication:ActiveWindow:FreezePanes = True.
  
  /* Legger inn sumlinjer. */                                        
  /* Excel macro som gjør jobben.
  Range("A4").Select
    Selection.Subtotal GroupBy:=1, Function:=xlSum, TotalList:=Array(5, 7, 9), _
        Replace:=True, PageBreaks:=True, SummaryBelowData:=True  
  */
  STATUS DEFAULT "Setter summeringer...".
  /*chWorkSheets:Range("E4:M50"):Subtotal(1 ,1 ,"5 , 7, 9" ,TRUE ,TRUE ,TRUE ).*/   
  
  chExcelApplication:Visible = TRUE.
  
  RELEASE OBJECT chWorksheets NO-ERROR.            /* release com-handles */
  RELEASE OBJECT chWorkbooks NO-ERROR.             /* release com-handles */
  RELEASE OBJECT chExcelApplication NO-ERROR.      /* release com-handles */
  ASSIGN chWorksheets       = ?
         chWorkbooks        = ?
         chExcelApplication = ?.
  STATUS DEFAULT "".

  {swn.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisParametre C-Parameteroppsett 
PROCEDURE VisParametre :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piSysHId LIKE SysPara.SysHId NO-UNDO.
  DEF INPUT PARAMETER piSysGr  LIKE SysPara.SysGr  NO-UNDO.
  DEF INPUT PARAMETER piParaNr LIKE SysPara.ParaNr NO-UNDO.
  DEF INPUT PARAMETER piHandle AS HANDLE           NO-UNDO.

  DEF BUFFER pSysPara FOR SysPara.

  DO:
    FIND pSysPara NO-LOCK WHERE
        pSysPara.SysHId   = piSysHId   AND
        pSysPara.SysGr    = piSysGr    AND
        pSysPara.ParaNr   = piParaNr   NO-ERROR.
    IF AVAILABLE pSysPara THEN
    ASSIGN
        piHandle:SCREEN-VALUE = pSysPAra.Parameter1
        .

  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisPost C-Parameteroppsett 
PROCEDURE VisPost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
     Parametre vises under SysHID = 100. 
     Flik  1 - Gruppe 10 
     Flik  2 - Gruppe 11 
     Flik  3 - Gruppe 12 
     Flik  4 - Gruppe 13 
    
------------------------------------------------------------------------------*/
    
    /* Renser skjermen før nye verdier legges opp. */    
    RUN Nullstill.

    /* Parametre hentes under SysHID = 100. */
    /* Flik  1 */
    DO WITH FRAME Frame-1:
        DISPLAY
            FI-TekstHK
            FI-TekstDbId
            FI-TekstLogVpi
            .
      RUN VisParametre (  1,   1, 14, FI-DbId:handle).
      RUN VisParametre (  1,  1, 18, T-HkInstall:handle).
      RUN VisParametre (  1,  1, 22, T-LogVpi:handle).
    END.

    /* Flik  2 */
    DO WITH FRAME Frame-2:
      RUN VisParametre (  2,  1, 10, T-DirOppdat:handle).
      RUN VisParametre (  1,  1, 19, T-HkNrSerie:handle).
      RUN VisParametre (  1,  1, 20, T-HkEkstra:handle).
      RUN VisParametre (  1,  1, 21, FI-TekstHKInfo:handle).
      RUN VisParametre (  2,  4,  2, T-OPris:handle).
      RUN VisParametre (  2,  4,  3, T-OLager:handle).
      RUN VisParametre (  2,  4,  4, T-BildeIKasse:handle).
      RUN VisParametre (  2,  4,  6, CB-VPI:handle).
      RUN VisParametre (  2,  4,  7, T-LokalVre:handle).
      RUN VisParametre (  2,  4,  8, T-GenEANNyVre:handle).
      DISPLAY
          FI-TekstDirO
          FI-TekstNrs
          FI-TekstHK1
          FI-TekstHK2          
          FI-TekstOPris
          FI-TekstOLager
          FI-TekstBildeIKasse
          FI-TekstVPI
          FI-TekstLokalVre
          FI-TekstGenEAN
          .
      IF NUM-ENTRIES(FI-TekstHKInfo:SCREEN-VALUE) > 1 THEN
          FI-TekstHKInfo:SCREEN-VALUE = ENTRY(1,FI-TekstHKInfo:SCREEN-VALUE).
      IF FI-TekstHKInfo:SCREEN-VALUE = '' THEN
          FI-TekstHKInfo:SCREEN-VALUE = '9000001-9999999'.
      IF FI-TekstHKInfo2:SCREEN-VALUE = '' THEN
          FI-TekstHKInfo2:SCREEN-VALUE = '8500001-8999999'.

      IF T-HKInstall:SCREEN-VALUE IN FRAME FRAME-1 = "yes" THEN
      DO:
        IF T-HkNrSerie:SCREEN-VALUE = "yes" THEN
        DO:
            T-HkEkstra:SENSITIVE = TRUE.
            IF T-HkEkstra:SCREEN-VALUE = "yes" THEN
                T-HkNrSerie:SENSITIVE = false.
        END.
        ELSE DO:
            T-HkEkstra:SENSITIVE = FALSE.
        END.
      END.
      ELSE DO:
          ASSIGN
              T-HkNrSerie:SENSITIVE = false
              T-HkEkstra:SENSITIVE  = FALSE
              .
      END.
    END.

    /* Flik  3 */
    DO WITH FRAME Frame-3:
      RUN VisParametre (  3,  1,  2, FI-StorsteButNr:handle).
    END.
    
    /* Flik  3  */
    DO WITH FRAME Frame-4:
      RUN VisParametre (  4,  1, 1, FI-TransTyper:handle).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WinHlp C-Parameteroppsett 
PROCEDURE WinHlp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {winhlp.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

