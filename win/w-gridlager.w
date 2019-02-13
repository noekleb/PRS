&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE tLevSort NO-UNDO LIKE BestSort.
DEFINE TEMP-TABLE tLevSortx NO-UNDO LIKE BestStr
       FIELD SortID   LIKE LevSort.SortID
       FIELD Antal    AS INTE FORMAT ">>9" LABEL "Antal"
       FIELD Fordel   AS CHAR FORMAT "x(40)" LABEL "Fördelning"
       FIELD StrlIntv AS CHAR FORMAT "x(10)" LABEL "Str.intervall"
       FIELD Storlekar AS CHAR FORMAT "x(50)" LABEL "Storlekar".
DEFINE NEW SHARED TEMP-TABLE TT_OvBuffer NO-UNDO LIKE OvBuffer.



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
  def var wArtBasRecid   as recid no-undo.
  def var wModus         as char  no-undo. /* NY, Endre, Slette */
  find first ArtBas no-lock where ArtBas.ArtikkelNr = 62 no-error.
  if not available ArtBas then
    find first ArtBas WHERE ArtBas.Lager = TRUE.
  if available ArtBas then
    assign wArtBasRecid = recid(ArtBas).
&ELSE
  def input parameter wArtBasRecid          as recid no-undo.
  def input parameter wModus                as char  no-undo.
&ENDIF

/* Local Variable Definitions ---                                       */
def var wOk       as log    no-undo.
def var hHandle   as handle no-undo.
def var hLabel    as handle no-undo.
def var wBildNr   as int    no-undo.
def var wFilNavn  as char   no-undo.
def var wFilExt   as char   no-undo.
DEF VAR wMDrow    AS INTE   NO-UNDO.
DEF VAR wMDcol    AS INTE   NO-UNDO.
def var wBatchNr  as int  initial ?  no-undo.
def var wEDB-System      as char no-undo.
def var wTabell          as char no-undo.
def var wSvar            as log  no-undo.
def var wOldRecid as recid  no-undo.
DEF VAR iOpphav   AS INTE INIT 3 NO-UNDO.
DEFINE VARIABLE iWidthPix  AS INTEGER    NO-UNDO.
DEFINE VARIABLE iHeightPix AS INTEGER    NO-UNDO.
DEFINE VARIABLE hSetGetNr  AS HANDLE     NO-UNDO.
DEFINE VARIABLE cOKButikker AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iOKbutBGcol AS INTEGER INIT 8585090   NO-UNDO.
DEFINE VARIABLE cFraCL AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cCL AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cBrukteStr AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lVisTG AS LOGICAL    NO-UNDO.
DEFINE VARIABLE bBrukTBId2 AS LOG NO-UNDO.
DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.

/* Om vi har okænd storlek ex, (NS), som har behållning  */
/* Skall denna justeras innan vi kan gøra øverføringar   */
/* i Main test på lHarUkjentMantall, sætts i initalize...*/
DEFINE VARIABLE lHarUkjentMantall AS LOGICAL    NO-UNDO.
/* Temp-Table Definitions ---                            */
DEFINE NEW SHARED TEMP-TABLE Overfor
        FIELD FraButik AS CHAR LABEL "Från butik"
        FIELD TilButik AS CHAR LABEL "Till butik"
        FIELD FraStrl  AS CHAR LABEL "Från Strl"
        FIELD TilStrl  AS CHAR LABEL "Till Strl"
        FIELD Antal    AS INTE LABEL "Antal"
        FIELD FraRow   AS INTE
        FIELD FraCol   AS INTE
        FIELD TilRow   AS INTE
        FIELD TilCol   AS INTE
        INDEX FraButik IS PRIMARY FraButik ASCENDING
                                  FraStrl  ASCENDING
        INDEX TilButik TilButik ASCENDING
                       TilStrl  ASCENDING.

DEFINE NEW SHARED TEMP-TABLE tmpLager NO-UNDO LIKE tmpLager.
/*  FIELD ArtikkelNr  AS DECI DECIMALS 2 FORMAT ">>>>>>>>>>>>9"
  FIELD Antall      AS CHAR EXTENT 48 FORMAT "X(6)"
  FIELD Butik       AS CHAR FORMAT "X(8)"
  FIELD SumAntall   AS CHAR FORMAT "X(10)"
  FIELD SumVerdi    AS CHAR FORMAT "X(10)"
  FIELD DivAntall   AS CHAR FORMAT "X(10)".
*/

DEFINE NEW SHARED TEMP-TABLE Fri-Str
    FIELD SoAnt    LIKE LevSant.SoAnt FORMAT ">>>>9-"
    FIELD SoStorl  LIKE LevSant.SoStorl
    FIELD Min      AS INTE LABEL "Min" FORMAT ">>>9-"
    FIELD SeqNr    LIKE LevSant.SeqNr
    INDEX SeqNr IS PRIMARY SeqNr ASCENDING.
    
DEFINE VAR wCentralLager LIKE Butiker.Butik NO-UNDO.
DEFINE VAR wStorlekar    AS CHAR NO-UNDO. /* Storlekar i matris */
DEFINE VAR wKassCell     AS INTE EXTENT 99 NO-UNDO. /* Storlekar i matris */
DEFINE VAR wFstRow       AS INTE NO-UNDO.
DEFINE VAR wLstRow       AS INTE NO-UNDO.
DEFINE VAR wFstCol       AS INTE NO-UNDO.
DEFINE VAR wLstCol       AS INTE NO-UNDO.
DEFINE VAR wBest         AS INTE NO-UNDO. /* För ändring av tLevsort */
DEFINE VAR wKassNy       AS INTE NO-UNDO. /* För ändring av tLevsort */
DEFINE VAR wFriAnt       AS INTE NO-UNDO.
DEFINE VAR wStatus       AS CHAR NO-UNDO.
DEFINE VAR wFriSort      AS LOGI NO-UNDO.
DEFINE var wStrTypeLst   as char no-undo.
DEFINE VAR wArtikkelNr   AS DEC  NO-UNDO.
/* DEFINE VAR dia-Best LIKE tLevSort.Best NO-UNDO. */
def var wNegInnLev        as log  no-undo.

/* Buffere */
def buffer bArtBas for ArtBas.
def buffer bBestStr for BestStr.
def buffer btLevSort for tLevSort.
def buffer bBestPris for BestPris.
{incl/DevMode.i}
{incl/CustDevMode.i}
{runlib.i}

{overforing.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn_Done RECT-3 RECT-4 RECT-2 RECT-1 ~
Btn_Help TG-VisSkjul 
&Scoped-Define DISPLAYED-FIELDS ArtBas.VgKat ArtBas.Beskr ArtBas.LevNr ~
LevBas.levnamn ArtBas.LevKod ArtBas.LevFargKod 
&Scoped-define DISPLAYED-TABLES ArtBas LevBas
&Scoped-define FIRST-DISPLAYED-TABLE ArtBas
&Scoped-define SECOND-DISPLAYED-TABLE LevBas
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-Vgr-Lopnr TG-VisSkjul 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FiksStorl C-Win 
FUNCTION FiksStorl RETURNS CHARACTER
  ( input wStorl as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetFarg C-Win 
FUNCTION GetFarg RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD OkSortering C-Win 
FUNCTION OkSortering RETURNS LOGICAL
  ( INPUT wType AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD PositivtLager C-Win 
FUNCTION PositivtLager RETURNS LOGICAL
  ( INPUT iMouseRow AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE SUB-MENU m_Fil 
       MENU-ITEM m_Avslutt      LABEL "&Avslutt"       ACCELERATOR "ALT-F4".

DEFINE MENU MENU-BAR-C-Win MENUBAR
       SUB-MENU  m_Fil          LABEL "&Fil"          .


/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE Grid AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chGrid AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE Image-Sko AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chImage-Sko AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Done 
     IMAGE-UP FILE "icon\e-exit":U NO-FOCUS FLAT-BUTTON
     LABEL "&Ok" 
     SIZE 4.6 BY 1.05 TOOLTIP "Lagre og avslutt".

DEFINE BUTTON Btn_Help DEFAULT 
     IMAGE-UP FILE "icon\e-help":U NO-FOCUS FLAT-BUTTON
     LABEL "&Help" 
     SIZE 4.6 BY 1.05
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-Angre 
     IMAGE-UP FILE "icon/e-undo.bmp":U NO-FOCUS
     LABEL "&Angre" 
     SIZE 4.6 BY 1.05 TOOLTIP "Angre".

DEFINE BUTTON BUTTON-Browse 
     IMAGE-UP FILE "adeicon\brws":U NO-FOCUS
     LABEL "Button 1" 
     SIZE 6.4 BY 1.52 TOOLTIP "Lista överföringar".

DEFINE BUTTON BUTTON-Lagra 
     IMAGE-UP FILE "icon/e-save.bmp":U NO-FOCUS
     LABEL "&Lagre og avslutte" 
     SIZE 4.6 BY 1.05 TOOLTIP "Lagre".

DEFINE BUTTON BUTTON-Skift 
     LABEL "&Skift artikkel ..." 
     SIZE 23 BY 1.05.

DEFINE VARIABLE FILL-IN-Vgr-Lopnr AS CHARACTER FORMAT "X(10)":U 
     LABEL "VGr/Löpnr" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 152.4 BY .1.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 152.4 BY .1.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 68 BY 5.24.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 29 BY 5.24.

DEFINE VARIABLE TG-VisSkjul AS LOGICAL INITIAL no 
     LABEL "Vis alle str" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     Btn_Done AT ROW 1.24 COL 148.2 NO-TAB-STOP 
     BUTTON-Skift AT ROW 1.24 COL 12.8
     FILL-IN-Vgr-Lopnr AT ROW 3 COL 20 COLON-ALIGNED
     ArtBas.VgKat AT ROW 3 COL 34.8 COLON-ALIGNED NO-LABEL FORMAT "z9"
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
     ArtBas.Beskr AT ROW 3 COL 40.4 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 25.6 BY 1
     ArtBas.LevNr AT ROW 4.14 COL 20 COLON-ALIGNED FORMAT "zzzzz9"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     LevBas.levnamn AT ROW 4.14 COL 34.6 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 31.4 BY 1
     ArtBas.LevKod AT ROW 5.29 COL 20 COLON-ALIGNED
          LABEL "LevArtNr/Farge"
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     ArtBas.LevFargKod AT ROW 5.29 COL 42.6 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 23.4 BY 1
     Btn_Help AT ROW 1.24 COL 143.2 NO-TAB-STOP 
     BUTTON-Angre AT ROW 1.24 COL 7.6 NO-TAB-STOP 
     TG-VisSkjul AT ROW 6.95 COL 129
     BUTTON-Lagra AT ROW 1.24 COL 2.6 NO-TAB-STOP 
     BUTTON-Browse AT ROW 8.14 COL 146.6
     RECT-3 AT ROW 2.57 COL 2
     RECT-4 AT ROW 2.57 COL 71.4
     RECT-2 AT ROW 2.33 COL 1
     RECT-1 AT ROW 1.05 COL 1
     SPACE(0.00) SKIP(29.13)
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
      TABLE: tLevSort T "?" NO-UNDO SkoTex BestSort
      TABLE: tLevSortx T "?" NO-UNDO skotex BestStr
      ADDITIONAL-FIELDS:
          FIELD SortID   LIKE LevSort.SortID
          FIELD Antal    AS INTE FORMAT ">>9" LABEL "Antal"
          FIELD Fordel   AS CHAR FORMAT "x(40)" LABEL "Fördelning"
          FIELD StrlIntv AS CHAR FORMAT "x(10)" LABEL "Str.intervall"
          FIELD Storlekar AS CHAR FORMAT "x(50)" LABEL "Storlekar"
      END-FIELDS.
      TABLE: TT_OvBuffer T "NEW SHARED" NO-UNDO skotex OvBuffer
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Lager artikkel:"
         HEIGHT             = 29.48
         WIDTH              = 152.4
         MAX-HEIGHT         = 57.14
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 57.14
         VIRTUAL-WIDTH      = 320
         TOP-ONLY           = yes
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

ASSIGN {&WINDOW-NAME}:MENUBAR    = MENU MENU-BAR-C-Win:HANDLE.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME DEFAULT-FRAME:SCROLLABLE       = FALSE.

/* SETTINGS FOR FILL-IN ArtBas.Beskr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-Angre IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-Browse IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-Lagra IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-Skift IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BUTTON-Skift:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-Vgr-Lopnr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ArtBas.LevFargKod IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ArtBas.LevKod IN FRAME DEFAULT-FRAME
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN LevBas.levnamn IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ArtBas.LevNr IN FRAME DEFAULT-FRAME
   NO-ENABLE EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN ArtBas.VgKat IN FRAME DEFAULT-FRAME
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME Image-Sko ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 2.81
       COLUMN          = 72.6
       HEIGHT          = 4.76
       WIDTH           = 26
       HIDDEN          = no
       SENSITIVE       = yes.

CREATE CONTROL-FRAME Grid ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 8.14
       COLUMN          = 2
       HEIGHT          = 22.14
       WIDTH           = 144
       HIDDEN          = no
       SENSITIVE       = yes.
/* Image-Sko OCXINFO:CREATE-CONTROL from: {9A93B740-C96B-11D0-8883-444553540000} type: Picbuf */
/* Grid OCXINFO:CREATE-CONTROL from: {0F026C11-5A66-4c2b-87B5-88DDEBAE72A1} type: VSFlexGrid */
      Image-Sko:MOVE-AFTER(BUTTON-Skift:HANDLE IN FRAME DEFAULT-FRAME).
      Grid:MOVE-AFTER(TG-VisSkjul:HANDLE IN FRAME DEFAULT-FRAME).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Lager artikkel: */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Lager artikkel: */
DO:
  /* This event will close the window and terminate the procedure.  */
  
  IF BUTTON-Lagra:SENSITIVE IN FRAME {&FRAME-NAME} = YES THEN DO:
      MESSAGE "Vill du lagra innan du avslutar"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL
                    TITLE "" UPDATE choice AS LOGICAL.
      CASE choice:
         WHEN TRUE THEN /* Yes */
              APPLY "CHOOSE" TO BUTTON-Lagra IN FRAME {&FRAME-NAME}.
         WHEN ? THEN /* No */
             RETURN NO-APPLY.
      END CASE.
  END.
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* Lager artikkel: */
DO:
    IF {&WINDOW-NAME}:WIDTH-PIXELS < iWidthPix THEN
            {&WINDOW-NAME}:WIDTH-PIXELS = iWidthPix.
    DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"Resize","").  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Done C-Win
ON CHOOSE OF Btn_Done IN FRAME DEFAULT-FRAME /* Ok */
DO:
  IF BUTTON-Lagra:SENSITIVE IN FRAME {&FRAME-NAME} = YES THEN DO:
      MESSAGE "Regstrering startet, ønsker du å avslutte?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL
                    TITLE "" UPDATE choice AS LOGICAL.
      CASE choice:
         WHEN FALSE THEN /* no */
             RETURN NO-APPLY.
/*               APPLY "CHOOSE" TO BUTTON-Lagra IN FRAME {&FRAME-NAME}. */
         WHEN ? THEN /* No */
             RETURN NO-APPLY.
      END CASE.
  END.
  APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help C-Win
ON CHOOSE OF Btn_Help IN FRAME DEFAULT-FRAME /* Help */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  {WinHlp.i}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Angre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Angre C-Win
ON CHOOSE OF BUTTON-Angre IN FRAME DEFAULT-FRAME /* Angre */
DO:
  for each Overfor: Delete Overfor. End.
  RUN Btn-Ena-Dis.
/*   assign                                                   */
/*     BUTTON-Bytt:SENSITIVE  IN FRAME {&FRAME-NAME} = true   */
/*     BUTTON-Angre:SENSITIVE IN FRAME {&FRAME-NAME} = false  */
/*     BUTTON-Lagra:SENSITIVE IN FRAME {&FRAME-NAME} = false. */
  RUN InitLager.
  RUN control_load.
  RUN VisArtBas.
  run VisBilde (1).

  apply "ENTRY":U to ArtBas.LevKod in frame {&FRAME-NAME}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Browse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Browse C-Win
ON CHOOSE OF BUTTON-Browse IN FRAME DEFAULT-FRAME /* Button 1 */
DO:
  RUN d-boverfor.w(THIS-PROCEDURE,chGrid,wArtBasRecid).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Lagra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Lagra C-Win
ON CHOOSE OF BUTTON-Lagra IN FRAME DEFAULT-FRAME /* Lagre og avslutte */
DO:
  DEFINE VARIABLE iBuntNr AS INTEGER INIT ? NO-UNDO.
  DEFINE VARIABLE cBuntTxt AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE lOppdatDirekte AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE lUtskrift AS LOGICAL  NO-UNDO.
  DEFINE VARIABLE ihBuffer AS HANDLE NO-UNDO.
  DEFINE VARIABLE cReturn AS CHAR NO-UNDO.
  DEFINE VARIABLE bOk AS LOG NO-UNDO.

  IF VALID-HANDLE(hSetGetNr) THEN
      RUN SetGetBuntNr IN hSetGetNr (INPUT-OUTPUT iBuntNr,"GET").
  IF iBuntNr = 0 THEN
      ASSIGN iBuntNr = ?.
  ASSIGN cBuntTxt = "J" + CHR(1) + "Artikkeloverføring " + STRING(TODAY) + STRING(TIME,"HH:MM").

  RUN d-velgovbunt.w (INPUT-OUTPUT iBuntNr,INPUT-OUTPUT cBuntTxt).

  IF RETURN-VALUE <> "AVBRYT" THEN 
  DO:
      RUN SkapaTT_OvBuffer.
      
      IF cBuntTxt = "" THEN
          ASSIGN cBuntTxt = "N" + CHR(1) + "Artikkeloverføring " + STRING(TODAY) + STRING(TIME,"HH:MM").
      IF NUM-ENTRIES(cBuntTxt,CHR(1)) >= 2 THEN 
           ASSIGN lOppdatDirekte = ENTRY(1,cBuntTxt,CHR(1)) = "J".
      IF NUM-ENTRIES(cBuntTxt,CHR(1)) = 3 AND ENTRY(3,cBuntTxt,CHR(1)) = "J" THEN
          lUtskrift = TRUE.
      ASSIGN
          cBuntTxt = (IF lOppdatDirekte
                          THEN "J"
                          ELSE "N") + chr(1) + ENTRY(2,cBuntTxt,CHR(1)).          

      IF bBrukTBId2 AND lOppdatDirekte THEN
      NY_OPPDATERING_VIA_PKSDL:
      DO:
         FOR EACH TT_Ovbuffer
             BREAK BY tt_Ovbuffer.BuntNr
                   BY tt_OvBuffer.ButikkNrFra
                   BY tt_Ovbuffer.ButikkNrTil:
             
             CREATE tmpOverfor.
             ASSIGN
                 tmpOverfor.ArtikkelNr = TT_OvBuffer.ArtikkelNr
                 tmpOverfor.Vg         = TT_OvBuffer.Vg
                 tmpOverfor.LopNr      = TT_OvBuffer.LopNr
                 tmpOverfor.FraBut     = TT_OvBuffer.ButikkNrFra
                 tmpOverfor.TilBut     = TT_OvBuffer.ButikkNrTil
                 tmpOverfor.FraStorl   = TT_OvBuffer.Storl
                 tmpOverfor.TilStorl   = TT_OvBuffer.TilStorl
                 tmpOverfor.Antall     = TT_OvBuffer.Antall
                 tmpOverfor.BuntNr     = TT_OvBuffer.BuntNr /* Bruker Dummy */
                 .
             /* Oppretter bong. pr. overføring */
             IF LAST-OF(tt_Ovbuffer.ButikkNrTil) THEN
             DO:
                 ihBuffer = BUFFER tmpOverfor:HANDLE.
                 RUN ovbunt_overfor.p (STRING(tt_Ovbuffer.ButikkNrFra) + '|' + STRING(tt_Ovbuffer.ButikkNrTil),
                                    ihBuffer,
                                    '',
                                    OUTPUT cReturn,
                                    OUTPUT bOk
                                   ).
                 EMPTY TEMP-TABLE tmpOverfor.
             END.
         END.
      END. /* NY_OPPDATERING_VIA_PKSDL */
      ELSE 
      GAMMEL_OPPDATERING:
      DO:
          IF CAN-FIND(FIRST TT_OvBuffer) THEN DO:
              RUN LagraOvBuffer.p (INPUT-OUTPUT iBuntNr,
                                   ArtBas.ArtikkelNr,
                                   cBuntTxt,
                                   wEDB-System,wTabell,iOpphav).
              EMPTY TEMP-TABLE TT_OvBuffer NO-ERROR.
              EMPTY TEMP-TABLE Overfor NO-ERROR.
              EMPTY TEMP-TABLE tmpLager NO-ERROR.
          END.
          IF VALID-HANDLE(hSetGetNr) THEN
              RUN SetGetBuntNr IN hSetGetNr (INPUT-OUTPUT iBuntNr,"SET").
          IF lUtskrift = TRUE THEN 
          do:
              RUN printoverforX.p (iBuntNr).
          end.
      END. /* GAMMEL_OPPDATERING */
  END.    
  ELSE DO:
      MESSAGE "Lagring avbrutt"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
  END.
  
  if wModus   <> "LAGER" then
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  ELSE DO:
      RELEASE ArtBas.
      ASSIGN wArtBasRecid = ?
             wOldRecid    = ?
             wStorlekar   = "".
      RUN control_load.
      RUN VisArtBas.
      RUN Btn-Ena-Dis.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Skift
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Skift C-Win
ON CHOOSE OF BUTTON-Skift IN FRAME DEFAULT-FRAME /* Skift artikkel ... */
DO:
    if wModus   <> "LAGER" then
      return no-apply.

    ASSIGN wOldRecid = wArtBasRecid
           wArtBasRecid = ?.
    run d-hsok.w (output wArtikkelNr,"").
    if wArtikkelNr = ? THEN 
        do:
          wArtBasRecid = wOldRecid.
/*         return no-apply. */
        end.
    ELSE DO:
        find ArtBas no-lock where
        ArtBas.ArtikkelNr = wArtikkelNr no-error.
        IF AVAILABLE ArtBas THEN
            wArtBasRecid = RECID(ArtBas).
        RUN Sjekk_Art.
        IF RETURN-VALUE = "AVBRYT" THEN DO:
            RELEASE ArtBas.
        END.
        RUN Sjekk_Laas.
        IF RETURN-VALUE = "AVBRYT" THEN DO:
            RELEASE ArtBas.
        END.
        IF NOT AVAIL ArtBas THEN
            ASSIGN wArtBasRecid = wOldRecid.
    END.
    IF wArtBasRecid <> ? THEN DO:
      find ArtBas no-lock where
      recid(ArtBas) = wArtBasRecid no-error.
      RUN InitLager.
      RUN control_load.
      RUN VisArtBas.
      run VisBilde (1).
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Grid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Grid C-Win OCX.MouseDown
PROCEDURE Grid.VSFlexGrid.MouseDown .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  Required for OCX.
    Button
    Shift
    X
    Y
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER p-Button AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER p-Shift  AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER p-X      AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER p-Y      AS DECIMAL NO-UNDO.
DEFINE VARIABLE cMDButik        AS CHARACTER  NO-UNDO.
    IF p-Button = 2 THEN
        RETURN.
    IF chGrid:MouseRow < chGrid:FixedRows THEN
        RETURN.
    IF chGrid:MouseCol > 0 AND chGrid:MouseCol < chGrid:FixedRows THEN DO:
        RETURN.
    END.
    ELSE IF chGrid:MouseCol = 0 AND NOT PositivtLager(chGrid:MouseRow) THEN
        RETURN.
    ASSIGN cMDButik = IF NUM-ENTRIES(TRIM(chGrid:Cell(0,chGrid:MouseRow,0,chGrid:MouseRow,0))," ") = 2 THEN
        TRIM(ENTRY(2,TRIM(chGrid:Cell(0,chGrid:MouseRow,0,chGrid:MouseRow,0))," ")) ELSE TRIM(chGrid:Cell(0,chGrid:MouseRow,0,chGrid:MouseRow,0)).

    IF NOT CAN-DO(cOKButikker,cMDButik) THEN
        RETURN.
    if chGrid:MouseCol >= chGrid:FixedRows AND 
              INT(chGrid:Cell(0,chGrid:MouseRow,chGrid:MouseCol,chGrid:MouseRow,chGrid:MouseCol)) < 0 then
      do:
        message "Du kan ikke overføre et negativt antall!" skip
                "Overfør fra en butikk som har lager til den butikk som har negativt lager."
                view-as alert-box message title "Melding".
        ASSIGN chGrid:MousePointer = 0.
        return no-apply.
      end.
    IF chGrid:MouseCol > 0 AND (chGrid:MouseRow <= chGrid:FixedRows - 1 OR
       chGrid:MouseCol <= chGrid:FixedCols - 1 OR
       INT(chGrid:TextMatrix(chGrid:MouseRow,chGrid:MouseCol)) = 0) THEN
        RETURN.
    ASSIGN wMDRow = chGrid:MouseRow
           wMDCol = chGrid:MouseCol
           chGrid:MousePointer = 5.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Grid C-Win OCX.MouseUp
PROCEDURE Grid.VSFlexGrid.MouseUp .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  Required for OCX.
    Button
    Shift
    X
    Y
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER p-Button  AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER p-Shift   AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER p-X       AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER p-Y       AS DECIMAL NO-UNDO.
DEFINE VAR             wMUrow    AS INTEGER NO-UNDO.
DEFINE VAR             wMUcol    AS INTEGER NO-UNDO.
DEFINE VAR             w-Antal   AS INTEGER NO-UNDO.
DEFINE VAR             wFraButik AS CHAR    NO-UNDO.
DEFINE VAR             wFraStrl  AS CHAR    NO-UNDO.
DEFINE VAR             wTilButik AS CHAR    NO-UNDO.
DEFINE VAR             wTilStrl  AS CHAR    NO-UNDO.
DEFINE VAR             iCount    AS INTEGER NO-UNDO.
    ASSIGN wMURow = chGrid:MouseRow
           wMUCol = chGrid:MouseCol.
    IF chGrid:MousePointer = 0 OR (wMDRow = wMURow and wMDCol = wMUCol) OR wMURow < 0 OR wMUCol < 0 THEN DO:
        ASSIGN chGrid:MousePointer = 0.
        RETURN NO-APPLY.
    END.
    IF wMDCol = 0 THEN DO:
        IF wMUCol <> 0 THEN DO:
            ASSIGN chGrid:MousePointer = 0.
            RETURN NO-APPLY.
        END.
    END.
    IF wMURow = chGrid:FixedRows - 1 OR wMUCol = chGrid:FixedCols - 1 THEN DO:
        ASSIGN chGrid:MousePointer = 0.
        RETURN NO-APPLY.
    END.
    IF TRIM(chGrid:Cell(0,0,wMUCol,0,wMUCol)) BEGINS("(") THEN DO:
        MESSAGE "Overføring til ukjent størrelse ikke tillatt."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        ASSIGN chGrid:MousePointer = 0.
        RETURN NO-APPLY.
    END.
    ASSIGN w-Antal   = INT(chGrid:Cell(0,wMDRow,wMDCol,wMDRow,wMDCol))
           wFraButik = TRIM(chGrid:Cell(0,wMDRow,0,wMDRow,0))
           wFraStrl  = FiksStorl(chGrid:Cell(0,0,wMDCol,0,wMDCol))
           wTilButik = TRIM(chGrid:Cell(0,wMURow,0,wMURow,0))
           wTilStrl  = FiksStorl(chGrid:Cell(0,0,wMUCol,0,wMUCol))
           wFraButik = IF NUM-ENTRIES(TRIM(wFraButik)," ") = 2 THEN ENTRY(2,TRIM(wFraButik)," ")
                                              ELSE wFraButik
           wTilButik = IF NUM-ENTRIES(TRIM(wTilButik)," ") = 2 THEN ENTRY(2,TRIM(wTilButik)," ")
                                              ELSE wTilButik.
/*     MESSAGE wTilButik                      */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK. */
    IF cOKButikker <> "*" THEN DO:
        IF wFraButik = wTilButik AND wFraButik = cCL THEN DO:
            ASSIGN chGrid:MousePointer = 0.
            RETURN NO-APPLY.
        END.
        IF wFraButik = cCL AND NOT CAN-DO(cOKButikker,wTilButik) THEN DO:
            ASSIGN chGrid:MousePointer = 0.
            RETURN NO-APPLY.
        END.
        IF wFraButik <> cCL AND NOT CAN-DO(cOKButikker,wTilButik) AND CAN-FIND(FIRST Overfor WHERE Overfor.FraButik = cCl) THEN DO:
            ASSIGN chGrid:MousePointer = 0.
            RETURN NO-APPLY.
        END.
/*         IF wFraButik <> wTilButik */
    END.
    IF wMUCol = 0 THEN DO:
        IF CAN-FIND(FIRST Overfor WHERE Overfor.TilButik = wFraButik) THEN DO:
            MESSAGE "Det finnes overføringer registrert til butikken du ønsker flytte fra."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
        END.
        ELSE DO:
            MESSAGE "Ønsker du å overføre hele lageret?" SKIP
                    "(Negative antall overføres ikke!)" SKIP
                    VIEW-AS ALERT-BOX MESSAGE TITLE "Melding" UPDATE lOK AS LOGICAL FORMAT "J/N".
            IF lOK THEN DO:
                DO iCount = chGrid:FixedCols TO chGrid:Cols - 1:
                    ASSIGN w-Antal = INT(chGrid:TextMatrix(wMDRow,iCount)).
                    IF w-Antal <= 0 THEN
                        NEXT.
                    ASSIGN wFraStrl = FiksStorl(chGrid:TextMatrix(0,iCount))
                           wTilStrl = wFraStrl
                           chGrid:TextMatrix(wMURow,iCount) =
                                   STRING(INT(chGrid:TextMatrix(wMURow,iCount)) + w-Antal) + " "
                           chGrid:TextMatrix(wMURow,chGrid:FixedCols - 1) =
                                       STRING(INT(chGrid:TextMatrix(wMURow,chGrid:FixedCols - 1)) + w-Antal) + " "
                           chGrid:TextMatrix(wMDRow,iCount) =
                                 IF INT(chGrid:TextMatrix(wMDRow,iCount)) - w-Antal = 0 THEN "" ELSE
                                   STRING(INT(chGrid:TextMatrix(wMDRow,iCount)) - w-Antal) + " "
                           chGrid:TextMatrix(wMDRow,chGrid:FixedCols - 1) =
                                 IF INT(chGrid:TextMatrix(wMDRow,chGrid:FixedCols - 1)) - w-Antal = 0 THEN "0 " ELSE
                                   STRING(INT(chGrid:TextMatrix(wMDRow,chGrid:FixedCols - 1)) - w-Antal) + " ".
                   FIND Overfor WHERE (Overfor.FraButik = wFraButik  OR
                                       Overfor.FraButik = wTilButik)  AND
                                      (Overfor.TilButik = wTilButik  OR 
                                       Overfor.TilButik = wFraButik)  AND 
                                      Overfor.FraStrl   = wFraStrl AND
                                      Overfor.TilStrl   = wTilStrl NO-ERROR.
                   IF AVAIL Overfor THEN DO:
                       IF Overfor.FraButik = wFraButik THEN DO:
                           ASSIGN Overfor.Antal = Overfor.Antal + w-Antal.
                       END.
                       ELSE DO:
                           IF Overfor.Antal = w-Antal THEN
                               DELETE Overfor.
                           ELSE IF Overfor.Antal > w-Antal THEN
                               ASSIGN Overfor.Antal = Overfor.Antal - w-Antal.
                           ELSE
                               ASSIGN Overfor.FraButik = wFraButik
                                      Overfor.TilButik = wTilButik
                                      Overfor.Antal    = w-Antal - Overfor.Antal
                                      Overfor.FraRow   = wMDRow
                                      Overfor.FraCol   = wMDCol
                                      Overfor.TilRow   = wMURow
                                      Overfor.TilCol   = wMUCol.
                       END.
                   END.
                   ELSE DO:
                       CREATE Overfor.
                       ASSIGN Overfor.FraButik = wFraButik
                              Overfor.FraStrl  = wFraStrl
                              Overfor.TilButik = wTilButik
                              Overfor.TilStrl  = wTilStrl
                              Overfor.Antal    = w-Antal
                              Overfor.FraRow   = wMDRow
                              Overfor.FraCol   = wMDCol
                              Overfor.TilRow   = wMURow
                              Overfor.TilCol   = wMUCol.
                   END.
                END.
            END.
        END.
        ASSIGN chGrid:MousePointer = 0.
    END.
    ELSE DO:
        IF wFraStrl <> wTilStrl AND wFraButik <> wTilButik THEN DO.
            MESSAGE "Överföring från en storlek till en annan" skip
                    "mellan två butiker är inte tillåtet!" VIEW-AS ALERT-BOX.
            ASSIGN chGrid:MousePointer = 0.
            RETURN NO-APPLY.
        END.    
        IF w-Antal > 1 OR wFraButik = wTilButik THEN
            RUN d-antal.w (INPUT-OUTPUT w-Antal,wFraButik,wFraStrl,wTilButik,wTilStrl).
        IF RETURN-VALUE = "<Avbryt>" OR w-Antal = 0 THEN DO:
            ASSIGN chGrid:MousePointer = 0.
            RETURN NO-APPLY.
        END.
        ASSIGN chGrid:TextMatrix(wMURow,wMUCol) =
                       STRING(INT(chGrid:TextMatrix(wMURow,wMUCol)) + w-Antal) + " "
               chGrid:TextMatrix(wMDRow,wMDCol) = 
                     IF INT(chGrid:TextMatrix(wMDRow,wMDCol)) - w-Antal = 0 THEN "" ELSE
                       STRING(INT(chGrid:TextMatrix(wMDRow,wMDCol)) - w-Antal) + " "
               chGrid:MousePointer = 0.
        IF wMURow = wMDRow THEN
            ASSIGN chGrid:TextMatrix(chGrid:FixedRows - 1,wMDCol) = 
                   IF INT(chGrid:TextMatrix(chGrid:FixedRows - 1,wMDCol)) - w-Antal = 0 THEN "" ELSE
                       STRING(INT(chGrid:TextMatrix(chGrid:FixedRows - 1,wMDCol)) - w-Antal) + " "
                   chGrid:TextMatrix(chGrid:FixedRows - 1,wMUCol) = 
                       STRING(INT(chGrid:TextMatrix(chGrid:FixedRows - 1,wMUCol)) + w-Antal) + " ".
        ELSE
            ASSIGN chGrid:TextMatrix(wMDRow,2) = 
                       STRING(INT(chGrid:TextMatrix(wMDRow,2)) - w-Antal) + " "
                   chGrid:TextMatrix(wMURow,2) = 
                       STRING(INT(chGrid:TextMatrix(wMURow,2)) + w-Antal) + " ".

        IF wFraButik = wTilButik THEN DO:
            FIND Overfor WHERE Overfor.FraButik  = wFraButik      AND
                               Overfor.TilButik  = wTilButik      AND 
                               (Overfor.FraStrl  = wFraStrl   OR
                                 Overfor.FraStrl = wTilStrl)  AND
                               (Overfor.TilStrl  = wFraStrl    OR
                                 Overfor.TilStrl = wTilStrl) NO-ERROR.
            IF AVAIL Overfor THEN DO:
                IF Overfor.FraStrl = wFraStrl THEN DO:
                    ASSIGN Overfor.Antal = Overfor.Antal + w-Antal.
                END.
                ELSE DO:
                    IF Overfor.Antal = w-Antal THEN
                        DELETE Overfor.
                    ELSE IF Overfor.Antal > w-Antal THEN
                        ASSIGN Overfor.Antal = Overfor.Antal - w-Antal.
                    ELSE
                        ASSIGN Overfor.FraStrl = wFraStrl
                               Overfor.TilStrl = wTilStrl
                               Overfor.Antal   = w-Antal - Overfor.Antal
                               Overfor.FraRow   = wMDRow
                               Overfor.FraCol   = wMDCol
                               Overfor.TilRow   = wMURow
                               Overfor.TilCol   = wMUCol.
                END.
            END.
            ELSE DO:
                CREATE Overfor.
                ASSIGN Overfor.FraButik = wFraButik
                       Overfor.FraStrl  = wFraStrl
                       Overfor.TilButik = wTilButik
                       Overfor.TilStrl  = wTilStrl
                       Overfor.Antal    = w-Antal
                       Overfor.FraRow   = wMDRow
                       Overfor.FraCol   = wMDCol
                       Overfor.TilRow   = wMURow
                       Overfor.TilCol   = wMUCol.

            END.
        END.
        ELSE DO:
            FIND Overfor WHERE (Overfor.FraButik = wFraButik  OR
                                Overfor.FraButik = wTilButik)  AND
                               (Overfor.TilButik = wTilButik  OR 
                                Overfor.TilButik = wFraButik)  AND 
                               Overfor.FraStrl   = wFraStrl AND
                               Overfor.TilStrl   = wTilStrl NO-ERROR.
            IF AVAIL Overfor THEN DO:
                IF Overfor.FraButik = wFraButik THEN DO:
                    ASSIGN Overfor.Antal = Overfor.Antal + w-Antal.
                END.
                ELSE DO:
                    IF Overfor.Antal = w-Antal THEN
                        DELETE Overfor.
                    ELSE IF Overfor.Antal > w-Antal THEN
                        ASSIGN Overfor.Antal = Overfor.Antal - w-Antal.
                    ELSE
                        ASSIGN Overfor.FraButik = wFraButik
                               Overfor.TilButik = wTilButik
                               Overfor.Antal    = w-Antal - Overfor.Antal
                               Overfor.FraRow   = wMDRow
                               Overfor.FraCol   = wMDCol
                               Overfor.TilRow   = wMURow
                               Overfor.TilCol   = wMUCol.
                END.
            END.
            ELSE DO:
                CREATE Overfor.
                ASSIGN Overfor.FraButik = wFraButik
                       Overfor.FraStrl  = wFraStrl
                       Overfor.TilButik = wTilButik
                       Overfor.TilStrl  = wTilStrl
                       Overfor.Antal    = w-Antal
                       Overfor.FraRow   = wMDRow
                       Overfor.FraCol   = wMDCol
                       Overfor.TilRow   = wMURow
                       Overfor.TilCol   = wMUCol.
            END.
        END.
    END.
    RUN Btn-Ena-Dis.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Image-Sko
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Image-Sko C-Win OCX.DblClick
PROCEDURE Image-Sko.Picbuf.DblClick .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/

  if available ArtBAs then
    do:
      find BildeRegister of ArtBas no-error.
      if available BildeRegister then
        run d-visbil.w (input recid(BildeRegister)).
    end.
  return no-apply.    


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Avslutt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Avslutt C-Win
ON CHOOSE OF MENU-ITEM m_Avslutt /* Avslutt */
DO:
    APPLY "CLOSE" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-VisSkjul
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-VisSkjul C-Win
ON VALUE-CHANGED OF TG-VisSkjul IN FRAME DEFAULT-FRAME /* Vis alle str */
DO:
  RUN SkjulVis(STRING(SELF:CHECKED,"Vis/Skjul")).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.
ASSIGN iWidthPix  = {&WINDOW-NAME}:WIDTH-PIXELS
       iHeightPix = {&WINDOW-NAME}:HEIGHT-PIXELS.
/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
{inutmld.i &Modus = "Opprett"} /* Melder fra at programmet har startet. */

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
{genlib.i
  &NoLibCall      = "Nei"
  &WindowName     = "Gridlager"
  &PreIClose      = " "
  &PostIClose     = "
      IF VALID-HANDLE(chGrid) THEN
          RELEASE OBJECT chGrid NO-ERROR.
      IF VALID-HANDLE(chImage-Sko) THEN
        RELEASE OBJECT chImage-Sko NO-ERROR.
      IF VALID-HANDLE(Grid) THEN
          DELETE OBJECT Grid NO-ERROR.
      IF VALID-HANDLE(Image-Sko) THEN
        DELETE OBJECT Image-Sko NO-ERROR.
      ASSIGN chGrid      = ?
             Grid        = ?
             chImage-Sko = ?
             Image-Sko   = ?.
                     "
  &PostDisable_ui = " "
}

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Bruke TBId = 2 for Varer på vei ved overføringer */
{syspara.i 11 6 1 cTekst}
IF cTekst = '1' THEN 
    bBrukTBId2 = TRUE. 

{syspara.i 5 4 2 wStrTypeLst}
{syspara.i 5 1 1 cCL}
{syspara.i 2 4 16 cFraCL}

/* Kode for låsing av artikkelnummer ved overføring. */
{syspara.i 1 2 3 wEDB-System}
if wEDB-System = "" then
  wEDB-System = "OVERFOR-LOCK".
/* {syspar2.i 1 2 3 wTabell} */
/* if wEDB-System = "" then  */
/*   wEDB-System = "ArtBas". */

  ASSIGN
    wNegInnLev = FALSE.
  IF PROGRAM-NAME(2) BEGINS "gridlager" THEN
    wArtBasRecid = ?.
  IF wArtBasRecid <> ? THEN DO:
    FIND FIRST ArtBas WHERE RECID(ArtBas) = wArtBasRecid NO-LOCK NO-ERROR.
    RUN Sjekk_Art.
    IF RETURN-VALUE = "AVBRYT" THEN DO:
        RUN DISABLE_UI.
        RETURN "AVBRYT".
    END.
   /* sätter upp butiker man kan överföra från */
    FIND Bruker WHERE Bruker.BrukerId = USERID("skotex") NO-LOCK NO-ERROR.
    FIND Butiker WHERE Butiker.Butik = Bruker.ButikkNr NO-LOCK NO-ERROR.
    IF NOT AVAIL Butiker AND Bruker.ButikkNr > 0 THEN DO:
        MESSAGE 
            "Bruker ikke koblet til butikk." SKIP
            "Kontakt systemansvarlig"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        APPLY "CLOSE" TO THIS-PROCEDURE.
        RETURN.
    END.
    ELSE DO:
        /* Henter sentrallager.                                                */
        /* Brukere som er koblet til sentralalgeret og butikere med butikk = 0 */
        /* skal kunne flytte mellom alle butikkene.                            */
        IF NOT AVAILABLE Butiker THEN
            FIND Butiker NO-LOCK WHERE
              Butiker.Butik = INT(cCl) NO-ERROR.

        ASSIGN cOKButikker = IF Butiker.sentrallager = TRUE THEN "*" ELSE STRING(Bruker.ButikkNr).
        IF Butiker.sentrallager = FALSE THEN DO:
            IF TRIM(cFraCL) = "1" THEN DO:
                ASSIGN cOKButikker = cOKButikker + "," + TRIM(cCL).
            END.
        END.
    END.

  /* Sjekker om det er satt overføringslås på artikkelen. */
    RUN Sjekk_Laas.
    IF RETURN-VALUE = "AVBRYT" THEN DO:
        RUN DISABLE_UI.
        RETURN "AVBRYT".
    END.
  END.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  RUN SetWindowTitle.
  IF AVAIL ArtBas THEN
    RUN InitLager.

  RUN enable_UI.
  RUN SkjulVis ("Skjul").
  TG-VisSkjul:HIDDEN = NOT lVisTG.
  RUN SetDivResize.
  {lng.i}
  IF AVAIL ArtBas THEN DO:
    RUN VisArtBas.
    run VisBilde (1).
  END.
  if wModus = "LAGER" then
    do:
      assign
/*         BUTTON-Bytt:hidden    = false */
/*         BUTTON-Bytt:sensitive = true  */
        BUTTON-Skift:hidden    = false
        BUTTON-Skift:sensitive = true.
    end.

  ASSIGN
    c-win:HIDDEN = FALSE.
  IF VALID-HANDLE(SOURCE-PROCEDURE) AND 
           CAN-DO(SOURCE-PROCEDURE:INTERNAL-ENTRIES,"SetGetBuntNr") THEN
    ASSIGN hSetGetNr = SOURCE-PROCEDURE.
  apply "ENTRY":U to BUTTON-Skift in frame {&FRAME-NAME}.
  IF lHarUkjentMantall = TRUE THEN DO:
      MESSAGE "Lager finnes på feil størrelse, størrelser i parentes '( )'" SKIP
              "Justering av lager må gjøres før overføring." 
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN.
  END.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Angra C-Win 
PROCEDURE Angra :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER wRowId AS ROWID NO-UNDO.

    FIND Overfor WHERE ROWID(Overfor) = wRowId.

    ASSIGN chGrid:TextMatrix(Overfor.TilRow,Overfor.TilCol) =
                 IF INT(chGrid:TextMatrix(Overfor.TilRow,Overfor.TilCol)) - Overfor.Antal = 0 THEN "" ELSE
                   STRING(INT(chGrid:TextMatrix(Overfor.TilRow,Overfor.TilCol)) - Overfor.Antal) + " "
           chGrid:TextMatrix(Overfor.FraRow,Overfor.FraCol) = 
                   STRING(INT(chGrid:TextMatrix(Overfor.FraRow,Overfor.FraCol)) + Overfor.Antal) + " ".
    IF Overfor.TilRow = Overfor.FraRow THEN
        ASSIGN chGrid:TextMatrix(chGrid:FixedRows - 1,Overfor.FraCol) = 
                   STRING(INT(chGrid:TextMatrix(chGrid:FixedRows - 1,Overfor.FraCol)) + Overfor.Antal) + " "
               chGrid:TextMatrix(chGrid:FixedRows - 1,Overfor.TilCol) = 
                 IF INT(chGrid:TextMatrix(chGrid:FixedRows - 1,Overfor.TilCol)) - Overfor.Antal = 0 THEN "" ELSE
                   STRING(INT(chGrid:TextMatrix(chGrid:FixedRows - 1,Overfor.TilCol)) - Overfor.Antal) + " ".
    ELSE
        ASSIGN chGrid:TextMatrix(Overfor.FraRow,2) = 
                   STRING(INT(chGrid:TextMatrix(Overfor.FraRow,2)) + Overfor.Antal) + " "
               chGrid:TextMatrix(Overfor.TilRow,2) = 
                   STRING(INT(chGrid:TextMatrix(Overfor.TilRow,2)) - Overfor.Antal) + " ".
    DELETE Overfor.
    RUN Btn-Ena-Dis.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Btn-Ena-Dis C-Win 
PROCEDURE Btn-Ena-Dis :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN BUTTON-Lagra:SENSITIVE  = CAN-FIND(FIRST Overfor)
               BUTTON-Angre:SENSITIVE  = CAN-FIND(FIRST Overfor)
               BUTTON-Browse:SENSITIVE = BUTTON-Lagra:SENSITIVE
/*                BUTTON-Bytt:SENSITIVE   = NOT CAN-FIND(FIRST Overfor) and wModus = "LAGER" */
               BUTTON-Skift:SENSITIVE   = NOT CAN-FIND(FIRST Overfor) and wModus = "LAGER".
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load C-Win  _CONTROL-LOAD
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

OCXFile = SEARCH( "w-gridlager.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chGrid = Grid:COM-HANDLE
    UIB_S = chGrid:LoadControls( OCXFile, "Grid":U)
    Grid:NAME = "Grid":U
    chImage-Sko = Image-Sko:COM-HANDLE
    UIB_S = chImage-Sko:LoadControls( OCXFile, "Image-Sko":U)
    Image-Sko:NAME = "Image-Sko":U
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "w-gridlager.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

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
  RUN control_load.
  DISPLAY FILL-IN-Vgr-Lopnr TG-VisSkjul 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  IF AVAILABLE ArtBas THEN 
    DISPLAY ArtBas.VgKat ArtBas.Beskr ArtBas.LevNr ArtBas.LevKod ArtBas.LevFargKod 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  IF AVAILABLE LevBas THEN 
    DISPLAY LevBas.levnamn 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE Btn_Done RECT-3 RECT-4 RECT-2 RECT-1 Btn_Help TG-VisSkjul 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitButtons C-Win 
PROCEDURE InitButtons :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initialize-controls C-Win 
PROCEDURE initialize-controls :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VAR wIdx        AS INTE NO-UNDO.
DEFINE VAR wRow        AS INTE NO-UNDO.
DEFINE VAR wTotKassPar AS INTE NO-UNDO.
DEFINE VAR wTotCentral AS INTE NO-UNDO.
DEFINE VAR wTotSolgt   AS INTE NO-UNDO.
DEFINE VAR wTotButik   AS INTE NO-UNDO.
DEFINE VAR wTotal      AS INTE NO-UNDO.
DEFINE VAR wFargStr    AS CHAR NO-UNDO.
DEFINE VAR iStartRow   AS INTEGER    NO-UNDO.
ASSIGN chGrid = chGrid:vsFlexGrid.

ASSIGN chGrid:CellPictureAlignment = 1
       chGrid:Redraw = FALSE. /* disable repaint while populating */

chGrid:Clear().

/* !!!! Om wStorlekar innehåller 'REST' skall den trimmas bort */
IF CAN-DO(REPLACE(wStorlekar,";",","),"REST") THEN
    ASSIGN wStorlekar = TRIM(REPLACE(wStorlekar,"REST",""),";").

ASSIGN chGrid:AllowUserResizing = 0    /* Fixed columns/rows */
       chGrid:Enabled           = TRUE /* Updateable grid */
/*       chGrid:AllowBigSelection = FALSE */
       chGrid:AllowSelection    = FALSE
       chGrid:Appearance        = 1 
       chGrid:Rows              = 3
       chGrid:Cols              = NUM-ENTRIES(wStorlekar,";") + 3
       chGrid:FixedRows         = 2
       chGrid:FixedCols         = 3
       chGrid:TextStyle         = 0
       chGrid:TextStyleFixed    = 0
       chGrid:ColWidthMin       = 615
       chGrid:ColWidth(1)       = 615
       chGrid:ColWidth(2)       = 615
   /*    chGrid:TextMatrix(0,0)   = "Butik " */
       chGrid:TextMatrix(0,0)   = " "
       chGrid:TextMatrix(0,1)   = "Solgt".
       chGrid:TextMatrix(0,2)   = "Totalt".
       chGrid:TextMatrix(1,0)   = "Sumlinje".


/* Initiering av storleksrad */
DO wIdx = 1 to NUM-ENTRIES(wStorlekar,";"):
    ASSIGN chGrid:TextMatrix(0,wIdx + 2) = ENTRY(1,ENTRY(wIdx,wStorlekar,";"),"/") + " ".
           chGrid:ColWidth(wIdx + 2) = 510.
END.

/* Finn Butik som är centrallager */
FIND SysPara WHERE SysPara.SysHId = 5 AND
                   SysPara.SysGr  = 1 AND
                   SysPara.ParaNr = 1 NO-LOCK NO-ERROR.
IF NOT AVAIL SysPara THEN DO:
    .
END.
FIND Butiker WHERE Butiker.Butik = INT(SysPara.Parameter1) NO-LOCK NO-ERROR.
IF NOT AVAIL Butiker THEN DO:
    .
END.
ASSIGN wCentralLager = Butiker.Butik
       chGrid:TextMatrix(chGrid:FixedRows,0)   = "CL " + STRING(Butiker.Butik) + " "
       iStartRow = chGrid:FixedRows.
IF CAN-DO(cOKButikker,STRING(Butiker.Butik)) THEN
      chGrid:Cell(6,chGrid:FixedRows,0,chGrid:FixedRows,0) = iOKbutBGcol.

/* Centrallager */
  FIND tmplager WHERE TRIM(tmplager.Butik) = STRING(wCentralLager) NO-ERROR.
  IF AVAIL tmplager THEN DO:
      DO wIdx = 1 to NUM-ENTRIES(wStorlekar,";"):
          ASSIGN wTotCentral = wTotCentral + INT(tmplager.Antall[wIdx])
                 chGrid:TextMatrix(chGrid:FixedRows,wIdx + 2) = TRIM(tmplager.Antall[wIdx]) + " ".
      END.
      wTotSolgt   = INT(tmplager.DivAntall).
  END.
  ASSIGN chGrid:TextMatrix(chGrid:FixedRows,1) = STRING(wTotSolgt)   + " "
         chGrid:TextMatrix(chGrid:FixedRows,2) = STRING(wTotCentral) + " ".
/* END Centrallager */

/* Finn resten av butikerna och det som är beställt */
ASSIGN wRow = chGrid:FixedRows + 1. /* Skall stå här */
FOR EACH tmplager  WHERE TRIM(tmplager.Butik) <> STRING(wCentralLager) AND
                         TRIM(tmplager.Butik) <> "Total" NO-LOCK BY tmplager.Butik:
    ASSIGN chGrid:Rows = wRow + 1
           chGrid:TextMatrix(wRow,0) = TRIM(tmplager.Butik) + " "
           wTotButik = 0.
    IF CAN-DO(cOKButikker,TRIM(tmplager.Butik)) THEN DO:
        chGrid:Cell(6,wRow,0,wRow,0) = iOKbutBGcol.
        IF cOKButikker <> "*" THEN
            ASSIGN iStartRow = wRow.
    END.

    DO wIdx = 1 to NUM-ENTRIES(wStorlekar,";"):
        ASSIGN wTotButik = wTotButik + INT(tmplager.Antall[wIdx])
               chGrid:TextMatrix(wRow,wIdx + 2) = TRIM(tmplager.Antall[wIdx]) + " ".
    END.
    ASSIGN wTotSolgt = wTotSolgt + INT(tmplager.DivAntall)
           chGrid:TextMatrix(wRow,1) = STRING(INT(tmplager.DivAntall)) + " "
           chGrid:TextMatrix(wRow,2) = STRING(wTotButik) + " "
           wRow = wRow + 1.
END. 
FIND tmplager WHERE TRIM(tmplager.Butik) = "Total" NO-LOCK NO-ERROR.
IF AVAIL tmplager THEN DO:
    DO wIdx = 1 to NUM-ENTRIES(wStorlekar,";"):
        IF TRIM(ENTRY(wIdx,wStorlekar,";")) BEGINS "(" AND INT(tmplager.Antall[wIdx]) <> 0 THEN
            ASSIGN lHarUkjentMantall = TRUE.
        ASSIGN wTotal = wTotal + INT(tmplager.Antall[wIdx])
               chGrid:TextMatrix(chGrid:FixedRows - 1,wIdx + 2) = TRIM(tmplager.Antall[wIdx]) + " ".
    END.
    ASSIGN chGrid:TextMatrix(chGrid:FixedRows - 1,1) = STRING(INT(tmplager.DivAntall)) + " "
           chGrid:TextMatrix(chGrid:FixedRows - 1,2) = STRING(wTotal) + " ".
END.
/* Allignment */
ASSIGN chGrid:Cell(2,0,1,0,chGrid:Cols - 1) = 4
       chGrid:Cell(2,1,1,chGrid:Rows - 1,chGrid:Cols - 1) = 7
       chGrid:ROW = iStartRow.
chGrid:AutoSize(1,chGrid:Cols - 1).

ASSIGN chGrid:Redraw = TRUE
/*        chGrid:Row = chGrid:FixedRows */
/*        chGrid:Col = chGrid:FixedCols */
    .

APPLY "ENTRY" TO Grid.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitLager C-Win 
PROCEDURE InitLager :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN bygglager.w (wArtBasRecid,1,OUTPUT wStorlekar, OUTPUT cBrukteStr).
  ASSIGN wFriSort = YES.                                   
  FIND LevBas OF ArtBas NO-LOCK NO-ERROR.
  IF NOT AVAIL LevBas THEN DO:
      MESSAGE "Leverantör saknas." VIEW-AS ALERT-BOX ERROR 
                                   TITLE "Leverantör saknas".
      APPLY "WINDOW-CLOSE" TO THIS-PROCEDURE.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LagreOverforinger C-Win 
PROCEDURE LagreOverforinger :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       IKKE I BRUK /ken1
------------------------------------------------------------------------------*/
/*  
  def var wTransNr like TransLogg.TransNr no-undo.
  
  if not available ArtBas then
    return "AVBRYT".
  
  if wBatchNr = ? then
    run batchlogg.w (program-name(1), 
                     "Overføringer: " + string(ArtBas.Vg) + "/" + string(ArtBas.LopNr),
                     output wBatchNr).

  do TRANSACTION:
    OVERFOR:
    for each Overfor
      break
      by Overfor.TilButik
      by Overfor.FraStrl:
      
      if first-of (Overfor.TilButik) then
        do:
          find last TransLogg no-lock where
            TransLogg.Butik = int(Overfor.FraButik) use-index TransLogg no-error.
          if available TransLogg then
            wTransNr = TransLogg.TransNr + 1.
          else 
            wTransNr = 1.
        end.
              
      /* Sjekker at transnr er ledig */
      if can-find(TransLogg where
                  TransLogg.Butik = int(Overfor.FraButik) and
                  TransLogg.TransNr = wTransNr) then
        NESTE_NR:
        do while true:
          wTransNr = wTransNr + 1.
          if can-find(TransLogg where
                      TransLogg.Butik = int(Overfor.FraButik) and
                      TransLogg.TransNr = wTransNr) then
            next NESTE_NR.
          else
            leave NESTE_NR.
        end. /* NESTE_NR */
        
      /* Benytter varekost i fra butikken. */  
      find Lager no-lock where
        Lager.ArtikkelNr = ArtBas.ArtikkelNr and
        Lager.Butik      = int(Overfor.FraButik) no-error.
        
      create TransLogg.
      assign 
           TransLogg.Butik        = int(Overfor.FraButik)
           TransLogg.TransNr      = wTransNr
           TransLogg.SeqNr        = 1.

      assign
           TransLogg.BatchNr      = wBatchNr
           TransLogg.TTId         = 6 /* Overføring */
           TransLogg.TBId         = 1
           TransLogg.ArtikkelNr   = if AVAILABLE ArtBas
                                      THEN ArtBas.ArtikkelNr
                                      ELSE 0
           TransLogg.Vg           = ArtBas.vg
           TransLogg.LopNr        = ArtBas.LopNr
           TransLogg.Antall       = Overfor.Antal
           TransLogg.Pris         = Lager.VVareKost
           TransLogg.RabKr        = 0
           TransLogg.KundNr       = 0

           TransLogg.LevNr        = if AVAILABLE ArtBas
                                      THEN ArtBas.LevNr
                                      ELSE 0
           TransLogg.OvButik      = int(Overfor.TilButik)
           TransLogg.OvTransNr    = TransLogg.TransNr
           TransLogg.BongId       = 0
           TransLogg.BongLinjeNr  = 0
           TransLogg.KassaNr      = 0
           TransLogg.Plukket      = true /* Skal ikke plukkes */
           TransLogg.Dato         = today
           TransLogg.Tid          = time
           TransLogg.BestNr       = 0
           TransLogg.Postert      = FALSE
           TransLogg.Mva          = 0.
      assign  
           TransLogg.Storl        = FiksStorl(Overfor.FraStrl).
      assign  
           TransLogg.TilStorl     = FiksStorl(Overfor.TilStrl).

    end. /* OVERFOR */

    /* Setter overføringslås på artikkelen. */
    if can-find(first Overfor) then
      do:
        find first KonvReg no-lock where
          KonvReg.EDB-System = wEDB-System and
          KonvReg.Tabell     = wTabell   and
          KonvReg.EkstId     = string(ArtBas.ArtikkelNr) no-error.
        if not available KonvReg then
          do:
            create KonvReg.
            assign
              KonvReg.EDB-System = wEDB-System 
              KonvReg.Tabell     = wTabell   
              KonvReg.EkstId     = string(ArtBas.ArtikkelNr)
              KonvReg.InterntID  = string(ArtBas.ArtikkelNr).
          end.
      end.
  end. /* TRANSACTION */
  if available KonvReg then
    release KonvReg.

  /* Flagger batchen klar for oppdatering. */
  if wBatchNr <> ? then
    run batchstatus.p (wBatchNr, 2).
*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetDivResize C-Win 
PROCEDURE SetDivResize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DYNAMIC-FUNCTION("setNoResizeX",THIS-PROCEDURE:CURRENT-WINDOW,
                                FRAME DEFAULT-FRAME:HANDLE,
                                "Image-Sko,RECT-3,RECT-4").
DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,
                                FRAME DEFAULT-FRAME:HANDLE,
                                "Image-Sko,RECT-1,RECT-2,RECT-3,RECT-4").
DYNAMIC-FUNCTION("setAddMoveX",THIS-PROCEDURE:CURRENT-WINDOW,
                                FRAME DEFAULT-FRAME:HANDLE,
                                 "Btn_Done,Btn_Help,BUTTON-Browse").
/* DYNAMIC-FUNCTION("SetResizeADM2panel",TRUE). */
DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,iWidthPix,iHeightPix,0,0).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetWindowTitle C-Win 
PROCEDURE SetWindowTitle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN CURRENT-WINDOW:HIDDEN = NO
         CURRENT-WINDOW:TITLE = "Lageromflyttning artikel: " + 
          IF AVAIL ArtBas THEN 
              STRING(ArtBas.ArtikkelNr) ELSE "".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Sjekk_Art C-Win 
PROCEDURE Sjekk_Art :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FIND LevBas OF ArtBas NO-LOCK NO-ERROR.
  IF NOT AVAIL LevBas THEN DO:
      MESSAGE "Leverantör saknas." VIEW-AS ALERT-BOX ERROR 
                                   TITLE "Leverantör saknas".
      RETURN "AVBRYT".
  END.
  ELSE IF can-do(wStrTypeLst,string(ArtBas.StrTypeID)) THEN DO:
      MESSAGE "Felaktig storlekstyp." VIEW-AS ALERT-BOX ERROR.
      RETURN "AVBRYT".
  END.
  ELSE if ArtBas.Lager = false then
    do:
      message "Artikkelen har ikke lagerstyring. Overfringer kan ikke registreres."
        view-as alert-box message title "Melding".
      RETURN "AVBRYT".
    end.
  ELSE RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Sjekk_Laas C-Win 
PROCEDURE Sjekk_Laas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF CAN-FIND(FIRST KonvReg WHERE KonvReg.EDB-System = wEDB-System AND
                                  KonvReg.Tabell     = wTabell     AND
                                  KonvReg.EkstId     = STRING(ArtBas.ArtikkelNr)) THEN DO:
    ASSIGN wSvar = FALSE.
    MESSAGE "Artikkelen er låst for registrering av overføringer!" skip
          "Dette skyldes vanlighvis at det ligger overføringstransaksjoner" skip
          "som ikke er oppdatert på denne artikkelen." skip(1)
          "Skal låsen åpnes? " skip
          "(Gjøres bare når du vet at det ikke ligger transaksjoner som ikke er oppdatert)"
          view-as alert-box question buttons YES-NO title "Åpne overføringslås"
          UPDATE wSvar.
     IF NOT wSvar THEN
         RETURN "AVBRYT".
  END.
  RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapaTT_OvBuffer C-Win 
PROCEDURE SkapaTT_OvBuffer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE         iLinjeNr AS INTEGER INIT 1 NO-UNDO.
    FOR EACH TT_OvBuffer:
        DELETE TT_OvBuffer. /* ev metod empty-temp-table */
    END.
    FOR EACH Overfor:
        FIND Lager NO-LOCK WHERE
             Lager.ArtikkelNr = ArtBas.ArtikkelNr AND
             Lager.Butik      = INT(Overfor.FraButik) NO-ERROR.
        CREATE TT_OvBuffer.
        ASSIGN TT_OvBuffer.BuntNr      = 999 /* dummy, kan vara vad som helst */
               TT_OvBuffer.LinjeNr     = iLinjeNr
               TT_OvBuffer.ButikkNrFra = INT(overfor.FraButik)
               TT_OvBuffer.ButikkNrTil = INT(overfor.TilButik)        
               TT_OvBuffer.ArtikkelNr  = ArtBas.ArtikkelNr
               TT_OvBuffer.Vg          = ArtBas.vg   
               TT_OvBuffer.LopNr       = ArtBas.LopNr
               TT_OvBuffer.Antall      = Overfor.Antal
               TT_OvBuffer.Merknad     = "Lagerrutin"
               TT_OvBuffer.Storl       = REPLACE(REPLACE(overfor.FraStrl,"(",""),")","")
               TT_OvBuffer.TilStorl    = overfor.TilStrl
               TT_OvBuffer.Varekost    = IF AVAIL Lager THEN Lager.VVarekost ELSE 0
               iLinjeNr                = iLinjeNr + 1.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkjulVis C-Win 
PROCEDURE SkjulVis :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER cWhat AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE ii AS INTEGER    NO-UNDO.
    IF cWhat = "Skjul" THEN DO:
        DO ii = 1 TO NUM-ENTRIES(wStorlekar,";"):
            chGrid:ColHidden(ii + 2) = ENTRY(ii,cBrukteStr) <> "J".
            IF ENTRY(ii,cBrukteStr) <> "J" THEN
                lVisTG = TRUE.
        END.
    END.
    ELSE DO:
        DO ii = 2 TO chGrid:Cols - 1:
            chGrid:ColHidden(ii) = FALSE.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisArtBas C-Win 
PROCEDURE VisArtBas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def var wTilbud as log no-undo.

do with frame {&FRAME-NAME}:
  find LevBas    of ArtBas no-lock no-error.
  find Farg      of ArtBas no-lock no-error.
  find Sasong    of ArtBas no-lock no-error.

  if available ArtBas THEN DO:
    display ArtBas.VgKat
            ArtBas.Beskr 
            ArtBas.LevNr  
            ArtBas.LevFargKod
            LevBas.LevNamn
            ArtBas.LevKod.
            FILL-IN-Vgr-Lopnr:SCREEN-VALUE = STRING(ArtBas.Vg) + "/" + 
                                   (if (ArtBas.LopNr = ? or ArtBas.LopNr = 0)
                                      then " "
                                      else STRING(ArtBas.LopNr)).
  END.
  ELSE DO:
      ASSIGN ArtBas.VgKat:SCREEN-VALUE      =  " "
             ArtBas.Beskr:SCREEN-VALUE      =  " "
             ArtBas.LevNr  :SCREEN-VALUE    =  " "
             ArtBas.LevFargKod:SCREEN-VALUE =  " "
             LevBas.LevNamn:SCREEN-VALUE    =  " "
             ArtBas.LevKod:SCREEN-VALUE     =  " "
             FILL-IN-Vgr-Lopnr:SCREEN-VALUE =  " ".
        chIMAGE-Sko:CLEAR(2).
  END.
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisBilde C-Win 
PROCEDURE VisBilde :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  if not available ArtBas then
    return.
  {visbilde.i
     &BldOcx = "chIMAGE-Sko"
     &BildNr = "ArtBas.BildNr"
   }
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FiksStorl C-Win 
FUNCTION FiksStorl RETURNS CHARACTER
  ( input wStorl as char ) :
/*------------------------------------------------------------------------------
  Purpose:  Formaterer størrelsen korrekt etter SkoTex standard.
    Notes:  
------------------------------------------------------------------------------*/

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

  RETURN wStorl.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetFarg C-Win 
FUNCTION GetFarg RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR wFarger AS CHAR NO-UNDO.
  FOR EACH SysPara WHERE 
      SysPara.SysHId = 5 and
      SysPara.SysGr  = 10 NO-LOCK:
      ASSIGN wFarger = IF wFarger = "" THEN
                           SysPara.Parameter1
                       ELSE
                           wFarger + "," + SysPara.Parameter1.
  END.
  
  RETURN wFarger.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION OkSortering C-Win 
FUNCTION OkSortering RETURNS LOGICAL
  ( INPUT wType AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VAR wCount1    AS INTE NO-UNDO.
  DEFINE VAR wMatrisIdx AS INTE NO-UNDO.

  if not available tLevSort then
    return FALSE.

  IF wType = "NY" THEN DO:
      DO wCount1 = 1 TO NUM-ENTRIES(tLevSort.Storrelser," "):
          wMatrisIdx =  LOOKUP(ENTRY(wCount1,tLevSort.Storrelser," "),wStorlekar," ").
          IF INT(chGrid:TextMatrix(4,wMatrisIdx + 1)) < INT(ENTRY(wCount1,tLevSort.Fordel," ")) 
          THEN DO:
              RETURN FALSE. 
          END.
      END.
  END.
  ELSE DO:
      DO wCount1 = 1 TO NUM-ENTRIES(tLevSort.Storrelser," "):
          wMatrisIdx =  LOOKUP(ENTRY(wCount1,tLevSort.Storrelser," "),wStorlekar," ").
          IF INT(chGrid:TextMatrix(chGrid:Row,wMatrisIdx + 1)) < INT(ENTRY(wCount1,tLevSort.Fordel," ")) THEN DO:
              RETURN FALSE. 
          END.
      END.
  END.
  RETURN TRUE.   /* Function return value. */
  
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION PositivtLager C-Win 
FUNCTION PositivtLager RETURNS LOGICAL
  ( INPUT iMouseRow AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iCol AS INTEGER    NO-UNDO.
  DEFINE VARIABLE lPositiv AS LOGICAL    NO-UNDO.
  DO iCol = chGrid:FixedCols TO chGrid:Cols - 1:
      IF INT(chGrid:TextMatrix(iMouseRow,iCol)) > 0 THEN DO:
          ASSIGN lPositiv = TRUE.
          LEAVE.
      END.
  END.
  RETURN lPositiv.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

