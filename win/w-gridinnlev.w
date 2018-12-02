&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win


/* Temp-Table and Buffer definitions                                    */
DEFINE NEW SHARED TEMP-TABLE tLevSort NO-UNDO LIKE BestSort.
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
  def var wBestHodeRecid as recid no-undo. /* ? = NY, recid(BestHode) = Endre */
  def var wModus         as char  no-undo. /* NY, Endre, Slette */
  find first ArtBas no-lock where ArtBas.ArtikkelNr = 3.
  if not available ArtBas then
    find first ArtBas.
  if available ArtBas then
    assign wArtBasRecid = recid(ArtBas).
    find last  besthode of artbas no-lock NO-ERROR.
    IF AVAIL BestHode THEN ASSIGN wBestHodeRecid = RECID(BestHode). 
&ELSE
  def input parameter wArtBasRecid          as recid no-undo.
  def input-output parameter wBestHodeRecid as recid no-undo. /* ? = NY, recid(BestHode) = Endre */
  def input parameter wModus                as char  no-undo.
&ENDIF

{windows.i}

/* Local Variable Definitions ---                                       */
def var wOk       as log    no-undo.
def var hHandle   as handle no-undo.
def var hLabel    as handle no-undo.
def var wBildNr   as int    no-undo.
def var wFilNavn  as char   no-undo.
def var wFilExt   as char   no-undo.
def var wBestHLevRec     as recid no-undo.
DEF VAR wLeveringsNr     AS INT  NO-UNDO.

/* Temp-Table Definitions ---                                       */
DEFINE NEW SHARED TEMP-TABLE Fri-Str
    FIELD SoAnt    LIKE LevSant.SoAnt FORMAT ">>>>9-"
    FIELD SoStorl  LIKE LevSant.SoStorl
    FIELD Min      AS INTE LABEL "Min" FORMAT ">>>9-"
    FIELD SeqNr    LIKE LevSant.SeqNr
    INDEX SeqNr IS PRIMARY SeqNr ASCENDING.
    
DEFINE VAR ch_Grid        AS COM-HANDLE NO-UNDO.
DEFINE VAR wFordel        AS CHAR NO-UNDO. /* Fordeling (Ant pr. str) totalt for bestillingen */
DEFINE VAR wCentralLager  LIKE Butiker.Butik NO-UNDO.
DEFINE VAR wStorlekar     AS CHAR NO-UNDO. /* Storlekar i matris */
DEFINE VAR wKassCell      AS INTE EXTENT 50 NO-UNDO. /* Storlekar i matris */
DEFINE VAR wFstRow        AS INTE NO-UNDO.
DEFINE VAR wLstRow        AS INTE NO-UNDO.
DEFINE VAR wFstCol        AS INTE NO-UNDO.
DEFINE VAR wLstCol        AS INTE NO-UNDO.
DEFINE VAR wBest          AS INTE NO-UNDO. /* För ändring av tLevsort */
DEFINE VAR wKassNy        AS INTE NO-UNDO. /* För ändring av tLevsort */
DEFINE VAR wFriAnt        AS INTE NO-UNDO.
DEFINE VAR wStatus        AS CHAR NO-UNDO.
DEFINE VAR wFriSort       AS LOGI NO-UNDO.
DEFINE VAR wAvskrevetCell AS CHAR INIT "Mak" NO-UNDO.
DEFINE VAR wBestString    AS CHAR NO-UNDO.
define var wLapTop        as log  no-undo.
def    var wBekreft       as log  no-undo.
DEF    VAR cPassordkrav   AS CHAR NO-UNDO.
def    var wEDB-System    as char no-undo.
def    var wTabell        as char no-undo.
DEF    VAR iOpphav        AS INTE INIT  2 NO-UNDO. /* ovBunt.Opphav = 2 */
DEFINE VAR wSHC#          AS INTE INIT 11 NO-UNDO. /* Antal hela synliga Cols */
DEFINE VARIABLE cStatusStr    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iMouseMoveRow AS INTEGER    NO-UNDO.
DEFINE VARIABLE cKontrStorl AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cBrukButikEtikett AS CHARACTER  NO-UNDO. /* överstyr etiketter.butik vid ikke direktelev */
DEFINE VARIABLE cManRegEAN AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cInnlevSamlet AS CHARACTER  NO-UNDO.
/* DEFINE VAR dia-Best LIKE tLevSort.Best NO-UNDO. */

/* NY 19/9 */ DEFINE VAR wFriAntal AS INTE EXTENT 99 NO-UNDO. /* CL */

/* Buffere */
def buffer bArtBas for ArtBas.
def buffer bBestStr for BestStr.
def buffer btLevSort for tLevSort.
def buffer bBestPris for BestPris.

{etikettlogg.i &NEW=NEW}

{runlib.i &API="API"}

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
&Scoped-define INTERNAL-TABLES tLevSort

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 tLevSort.AntSort tLevSort.SortID ~
tLevSort.Antall tLevSort.StrInterval tLevSort.Fordeling tLevSort.Storrelser 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1 
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH tLevSort NO-LOCK
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY BROWSE-1 FOR EACH tLevSort NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 tLevSort
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 tLevSort


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BROWSE-1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS BestHode.Merknad 
&Scoped-define ENABLED-TABLES BestHode
&Scoped-define FIRST-ENABLED-TABLE BestHode
&Scoped-Define ENABLED-OBJECTS B-Etiketter RECT-1 RECT-3 RECT-4 RECT-47 ~
RECT-48 B-Print B-PrintInnlev BUTTON-Kalkyl BUTTON-Etiketter Btn_Done ~
Btn_Help BROWSE-1 FILL-IN-2 FILL-IN-1 FILL-IN-KvarL FILL-IN-InnlevL ~
FILL-IN-RestL 
&Scoped-Define DISPLAYED-FIELDS ArtBas.VgKat ArtBas.Beskr ~
BestHode.BestillingsDato ArtBas.LevNr BestHode.LevDato LevBas.levnamn ~
BestPris.VareKost BestHode.TotInnkjVerdi ArtBas.LevKod ArtBas.LevFargKod ~
BestHode.RegistrertAv BestPris.DBKr BestHode.TotDbKr BestHode.DirekteLev ~
BestPris.Pris BestHode.TotSalgsVerdi BestHode.Beskrivelse BestHode.Merknad 
&Scoped-define DISPLAYED-TABLES ArtBas BestHode LevBas BestPris
&Scoped-define FIRST-DISPLAYED-TABLE ArtBas
&Scoped-define SECOND-DISPLAYED-TABLE BestHode
&Scoped-define THIRD-DISPLAYED-TABLE LevBas
&Scoped-define FOURTH-DISPLAYED-TABLE BestPris
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-Vgr-Lopnr FI-OrdreLev ~
TOGGLE-Tillagg FILL-IN-Kvar FILL-IN-Innlev FILL-IN-Rest FILL-IN-2 FILL-IN-1 ~
FILL-IN-KvarL FILL-IN-InnlevL FILL-IN-RestL 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AlleEANok C-Win 
FUNCTION AlleEANok RETURNS CHARACTER
    ( INPUT cStorrelser AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FiksStorl C-Win 
FUNCTION FiksStorl RETURNS CHARACTER
  ( input wStorl as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FixFillIn C-Win 
FUNCTION FixFillIn RETURNS CHARACTER
  ( INPUT wRow AS INTEGER, INPUT wCol AS INTEGER, INPUT wAction AS CHARACTER, INPUT wVerdi AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fLockvindu C-Win 
FUNCTION fLockvindu RETURNS CHARACTER
  ( INPUT lLock AS LOGICAL )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FullInlev C-Win 
FUNCTION FullInlev RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getArtNr C-Win 
FUNCTION getArtNr RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetFarg C-Win 
FUNCTION GetFarg RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getStorl C-Win 
FUNCTION getStorl RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LagString C-Win 
FUNCTION LagString RETURNS CHARACTER
  ( INPUT wVerdi AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD OkClear C-Win 
FUNCTION OkClear RETURNS LOGICAL
  (INPUT wModus AS CHARACTER  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD OkInlev C-Win 
FUNCTION OkInlev RETURNS LOGICAL
  ( )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD OkMak C-Win 
FUNCTION OkMak RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Registrerat C-Win 
FUNCTION Registrerat RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD RegistreratButik C-Win 
FUNCTION RegistreratButik RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

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

DEFINE MENU POPUP-MENU-FRAME-A 
       MENU-ITEM m_Full_inlev_butiker LABEL "Full &inleveranse (butikker)"
       MENU-ITEM m_Full_inlev_butik LABEL "Full &inleveranse (butikk)"
       MENU-ITEM m_Avskriv_storlek LABEL "&Avskriv størrelse(r)"
       RULE
       MENU-ITEM m_clear_butiker LABEL "&Angre registrert (butikker)"
       MENU-ITEM m_clear_butik  LABEL "Angre &registrert (butikk)".


/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE Image-Sko AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chImage-Sko AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Etiketter 
     IMAGE-UP FILE "icon/ean13.jpg":U NO-FOCUS
     LABEL "Etiketter..." 
     SIZE 4.6 BY 1.1 TOOLTIP "Registrer EAN-koder".

DEFINE BUTTON B-Print 
     IMAGE-UP FILE "icon/e-print":U
     LABEL "Button 1" 
     SIZE 4.6 BY 1.1 TOOLTIP "Utskrift av bestillingskort".

DEFINE BUTTON B-PrintInnlev 
     IMAGE-UP FILE "icon/e-print":U
     LABEL "B print 2" 
     SIZE 4.6 BY 1.1 TOOLTIP "Utskrift av innleveransrapport".

DEFINE BUTTON Btn_Done DEFAULT 
     IMAGE-UP FILE "icon/e-exit":U
     LABEL "&Avslutt" 
     SIZE 5 BY 1.1
     BGCOLOR 8 .

DEFINE BUTTON Btn_Help DEFAULT 
     IMAGE-UP FILE "icon/e-help":U NO-FOCUS
     LABEL "&Hjelp" 
     SIZE 4.6 BY 1.1 TOOLTIP "Hjelp"
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-Etiketter 
     LABEL "Etiketter innlev" 
     SIZE 18 BY 1.

DEFINE BUTTON BUTTON-Fullinlev 
     LABEL "Full innlev..." 
     SIZE 16.6 BY 1.

DEFINE BUTTON BUTTON-Kalkyl 
     LABEL "Kalkyle..." 
     SIZE 16.6 BY 1.

DEFINE BUTTON BUTTON-Lagra 
     IMAGE-UP FILE "icon/e-save":U
     LABEL "Lagra" 
     SIZE 4.6 BY 1.1.

DEFINE VARIABLE FI-OrdreLev AS CHARACTER FORMAT "X(256)":U 
     LABEL "Ordreleverandør" 
     VIEW-AS FILL-IN 
     SIZE 41 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-1 AS CHARACTER FORMAT "X(256)":U INITIAL "Totalt" 
      VIEW-AS TEXT 
     SIZE 8 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FILL-IN-2 AS CHARACTER FORMAT "X(256)":U INITIAL "Per enhet" 
      VIEW-AS TEXT 
     SIZE 11 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FILL-IN-Innlev AS INTEGER FORMAT "zzzzzz9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-InnlevL AS CHARACTER FORMAT "X(256)":U INITIAL "Innlev" 
      VIEW-AS TEXT 
     SIZE 9 BY .62 NO-UNDO.

DEFINE VARIABLE FILL-IN-Kvar AS INTEGER FORMAT "zzzzzz9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-KvarL AS CHARACTER FORMAT "X(256)":U INITIAL "Å levere" 
      VIEW-AS TEXT 
     SIZE 11 BY .62 NO-UNDO.

DEFINE VARIABLE FILL-IN-Rest AS INTEGER FORMAT "zzzzzz9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-RestL AS CHARACTER FORMAT "X(256)":U INITIAL "Rest" 
      VIEW-AS TEXT 
     SIZE 9 BY .62 NO-UNDO.

DEFINE VARIABLE FILL-IN-Vgr-Lopnr AS CHARACTER FORMAT "X(10)":U 
     LABEL "VGr/Løpenr" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 62 BY 6.38.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 54 BY 6.38.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 33 BY 6.38.

DEFINE RECTANGLE RECT-47
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 152.4 BY .1.

DEFINE RECTANGLE RECT-48
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 152.4 BY .1.

DEFINE VARIABLE TOGGLE-Tillagg AS LOGICAL INITIAL no 
     LABEL "Tilleggsbest" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .81 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      tLevSort SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 C-Win _STRUCTURED
  QUERY BROWSE-1 DISPLAY
      tLevSort.AntSort COLUMN-LABEL "Best" FORMAT "zzz9":U
      tLevSort.SortID COLUMN-LABEL "Sortiment" FORMAT "X(12)":U
      tLevSort.Antall FORMAT "zzz9":U
      tLevSort.StrInterval COLUMN-LABEL "Str.intervall" FORMAT "X(11)":U
      tLevSort.Fordeling FORMAT "X(45)":U
      tLevSort.Storrelser FORMAT "X(70)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 85.8 BY 4.76 ROW-HEIGHT-CHARS .62.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     B-Etiketter AT ROW 1.19 COL 15.8
     BUTTON-Lagra AT ROW 1.19 COL 2
     B-Print AT ROW 1.19 COL 6.6
     B-PrintInnlev AT ROW 1.19 COL 11.2
     BUTTON-Kalkyl AT ROW 1.24 COL 21.6
     BUTTON-Etiketter AT ROW 1.24 COL 51
     BUTTON-Fullinlev AT ROW 1.24 COL 95
     Btn_Done AT ROW 1.24 COL 147.6
     FILL-IN-Vgr-Lopnr AT ROW 2.91 COL 20 COLON-ALIGNED
     Btn_Help AT ROW 1.24 COL 142
     ArtBas.VgKat AT ROW 2.91 COL 34.4 COLON-ALIGNED NO-LABEL FORMAT "z9"
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
     ArtBas.Beskr AT ROW 2.91 COL 40 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 20.4 BY 1
     BestHode.BestillingsDato AT ROW 2.91 COL 79 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     ArtBas.LevNr AT ROW 4.1 COL 20 COLON-ALIGNED FORMAT "zzzzz9"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     BestHode.LevDato AT ROW 4.1 COL 79 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     LevBas.levnamn AT ROW 4.14 COL 34.6 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     BestPris.VareKost AT ROW 4.33 COL 108 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20.2 BY 1
     BestHode.TotInnkjVerdi AT ROW 4.33 COL 130 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 20.2 BY 1
     ArtBas.LevKod AT ROW 5.29 COL 20 COLON-ALIGNED
          LABEL "LevArtNr/Farg"
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     ArtBas.LevFargKod AT ROW 5.29 COL 42.6 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     BestHode.RegistrertAv AT ROW 5.29 COL 79 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     BestPris.DBKr AT ROW 5.52 COL 108 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     BestHode.TotDbKr AT ROW 5.52 COL 130 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 20.2 BY 1
     FI-OrdreLev AT ROW 6.48 COL 20 COLON-ALIGNED
     BestHode.DirekteLev AT ROW 6.48 COL 66
          VIEW-AS TOGGLE-BOX
          SIZE 13.4 BY .81
     TOGGLE-Tillagg AT ROW 6.48 COL 81
     BestPris.Pris AT ROW 6.71 COL 108 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     BestHode.TotSalgsVerdi AT ROW 6.71 COL 130 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 20.2 BY 1
     BestHode.Beskrivelse AT ROW 7.67 COL 20 COLON-ALIGNED
          LABEL "Merknad"
          VIEW-AS FILL-IN 
          SIZE 41 BY 1
     BROWSE-1 AT ROW 9.05 COL 2
     BestHode.Merknad AT ROW 9.05 COL 89 NO-LABEL
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          SIZE 37 BY 2.86
     FILL-IN-Kvar AT ROW 12.86 COL 87 COLON-ALIGNED NO-LABEL
     FILL-IN-Innlev AT ROW 12.86 COL 99 COLON-ALIGNED NO-LABEL
     FILL-IN-Rest AT ROW 12.86 COL 111 COLON-ALIGNED NO-LABEL
     FILL-IN-2 AT ROW 3.38 COL 112 COLON-ALIGNED NO-LABEL
     FILL-IN-1 AT ROW 3.38 COL 135 COLON-ALIGNED NO-LABEL
     FILL-IN-KvarL AT ROW 12.14 COL 87 COLON-ALIGNED NO-LABEL
     FILL-IN-InnlevL AT ROW 12.14 COL 99 COLON-ALIGNED NO-LABEL
     FILL-IN-RestL AT ROW 12.14 COL 111 COLON-ALIGNED NO-LABEL
     RECT-1 AT ROW 2.57 COL 2
     RECT-3 AT ROW 2.57 COL 99
     RECT-4 AT ROW 2.57 COL 65
     RECT-47 AT ROW 2.33 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 153 BY 30.76.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME DEFAULT-FRAME
     RECT-48 AT ROW 1.05 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 153 BY 30.76.

DEFINE FRAME FRAME-A
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 11 ROW 20
         SIZE 1 BY 2.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: tLevSort T "NEW SHARED" NO-UNDO SkoTex BestSort
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
         TITLE              = "Varemottak artikkel:"
         HEIGHT             = 30.76
         WIDTH              = 152.8
         MAX-HEIGHT         = 33.71
         MAX-WIDTH          = 204.4
         VIRTUAL-HEIGHT     = 33.71
         VIRTUAL-WIDTH      = 204.4
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

ASSIGN {&WINDOW-NAME}:MENUBAR    = MENU MENU-BAR-C-Win:HANDLE.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* REPARENT FRAME */
ASSIGN FRAME FRAME-A:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB BROWSE-1 Beskrivelse DEFAULT-FRAME */
/* SETTINGS FOR FILL-IN ArtBas.Beskr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN BestHode.Beskrivelse IN FRAME DEFAULT-FRAME
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN BestHode.BestillingsDato IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BROWSE-1:NUM-LOCKED-COLUMNS IN FRAME DEFAULT-FRAME     = 4.

/* SETTINGS FOR BUTTON BUTTON-Fullinlev IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-Lagra IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN BestPris.DBKr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX BestHode.DirekteLev IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-OrdreLev IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       FI-OrdreLev:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-Innlev IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-Kvar IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-Rest IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-Vgr-Lopnr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN BestHode.LevDato IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ArtBas.LevFargKod IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ArtBas.LevKod IN FRAME DEFAULT-FRAME
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN LevBas.levnamn IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ArtBas.LevNr IN FRAME DEFAULT-FRAME
   NO-ENABLE EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN BestPris.Pris IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN BestHode.RegistrertAv IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TOGGLE-Tillagg IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN BestHode.TotDbKr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN BestHode.TotInnkjVerdi IN FRAME DEFAULT-FRAME
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN BestHode.TotSalgsVerdi IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN BestPris.VareKost IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ArtBas.VgKat IN FRAME DEFAULT-FRAME
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FRAME FRAME-A
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME FRAME-A:POPUP-MENU       = MENU POPUP-MENU-FRAME-A:HANDLE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-1
/* Query rebuild information for BROWSE BROWSE-1
     _TblList          = "Temp-Tables.tLevSort"
     _FldNameList[1]   > Temp-Tables.tLevSort.AntSort
"tLevSort.AntSort" "Best" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.tLevSort.SortID
"tLevSort.SortID" "Sortiment" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   = Temp-Tables.tLevSort.Antall
     _FldNameList[4]   > Temp-Tables.tLevSort.StrInterval
"tLevSort.StrInterval" "Str.intervall" ? "character" ? ? ? ? ? ? no "Storlekssintervall" no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.tLevSort.Fordeling
"tLevSort.Fordeling" ? "X(45)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.tLevSort.Storrelser
"tLevSort.Storrelser" ? "X(70)" "character" ? ? ? ? ? ? no "Storlekar" no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

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
       ROW             = 9.05
       COLUMN          = 127
       HEIGHT          = 4.76
       WIDTH           = 25
       HIDDEN          = no
       SENSITIVE       = yes.

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 14.19
       COLUMN          = 2
       HEIGHT          = 17.52
       WIDTH           = 149.4
       HIDDEN          = no
       SENSITIVE       = yes.
/* Image-Sko OCXINFO:CREATE-CONTROL from: {9A93B740-C96B-11D0-8883-444553540000} type: Picbuf */
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {0F026C11-5A66-4c2b-87B5-88DDEBAE72A1} type: VSFlexGrid */
      Image-Sko:MOVE-AFTER(BestHode.Merknad:HANDLE IN FRAME DEFAULT-FRAME).
      CtrlFrame:MOVE-AFTER(FILL-IN-Rest:HANDLE IN FRAME DEFAULT-FRAME).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Varemottak artikkel: */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Varemottak artikkel: */
DO:
  /* This event will close the window and terminate the procedure.  */
  DEF VAR wVal AS LOGI INIT TRUE NO-UNDO.
  IF BUTTON-Lagra:SENSITIVE IN FRAME {&FRAME-NAME} = YES AND Registrerat() THEN DO:
      MESSAGE "Vill du lagra innan du avslutar"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL
                    TITLE "" UPDATE wVal.
      CASE wVal:
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


&Scoped-define SELF-NAME FRAME-A
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FRAME-A C-Win
ON GO OF FRAME FRAME-A
DO:
    run Apply-mouse-menu-click(self:handle).
    return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Etiketter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Etiketter C-Win
ON CHOOSE OF B-Etiketter IN FRAME DEFAULT-FRAME /* Etiketter... */
DO:
    RUN OppdStrekkoder.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Print
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Print C-Win
ON CHOOSE OF B-Print IN FRAME DEFAULT-FRAME /* Button 1 */
DO:
  if not available BestHode then
    return.
  IF cInnlevSamlet = "1" THEN
      run bestillingskort.p (recid(BestHode),101,4,no).
  ELSE
      run bestillingskortX.p (recid(BestHode),101,4,no).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-PrintInnlev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-PrintInnlev C-Win
ON CHOOSE OF B-PrintInnlev IN FRAME DEFAULT-FRAME /* B print 2 */
DO:
  if not available BestHode then
    return.
    
  assign wBekreft = false.
/*   message "Skal det skrives ut vedlegg pr. butikk?"                             */
/*     view-as alert-box question buttons yes-no-cancel title "Varemottaksrapport" */
/*     update wBekreft.                                                            */
/*   if wBekreft = ? then                                                          */
/*     return no-apply.                                                            */
/*   else                                                                          */
  IF cInnlevSamlet = "1" THEN
      run bestillingskort.p (recid(BestHode),109,4,wBekreft).
  ELSE
      run bestillingskortX.p (recid(BestHode),109,4,wBekreft).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BestHode.BestillingsDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BestHode.BestillingsDato C-Win
ON LEAVE OF BestHode.BestillingsDato IN FRAME DEFAULT-FRAME /* Bestillingsdato */
DO:
   IF INPUT BestHode.Bestillingsdato < TODAY THEN DO:
       MESSAGE "Fel dato." VIEW-AS ALERT-BOX ERROR TITLE "Fel datum".
       RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Done C-Win
ON CHOOSE OF Btn_Done IN FRAME DEFAULT-FRAME /* Avslutt */
DO:
  if available BestHode then
    do TRANSACTION:
      find current BestHode exclusive-lock.
      ASSIGN /* BestHode.AnonseArtikkel  = INPUT ArtBas.AnonseArtikkel */
         BestHode.Beskrivelse     = INPUT BestHode.Beskrivelse
         BestHode.BestType        = IF TOGGLE-Tillagg:CHECKED THEN 2
                                        ELSE 1
         BestHode.DirekteLev      = INPUT DirekteLev
         BestHode.Merknad         = INPUT BestHode.Merknad
         BestHode.LevFargKod      = INPUT ArtBas.LevFargKod
         BestHode.LevKod          = INPUT ArtBas.LevKod
         BestHode.LevDato         = INPUT BestHode.LevDato.
    end.
  if available BestHode then
      find current BestHode no-lock.

  APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help C-Win
ON CHOOSE OF Btn_Help IN FRAME DEFAULT-FRAME /* Hjelp */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  {winhlp.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Etiketter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Etiketter C-Win
ON CHOOSE OF BUTTON-Etiketter IN FRAME DEFAULT-FRAME /* Etiketter innlev */
DO:
  run SkrivEtiketter.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Fullinlev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Fullinlev C-Win
ON CHOOSE OF BUTTON-Fullinlev IN FRAME DEFAULT-FRAME /* Full innlev... */
DO:
    DEF VAR wOK AS LOGI INIT TRUE NO-UNDO.
  
    if not available ArtBas then
      find ArtBas no-lock where
        recid(ArtBas) = wArtBasRecid no-error.
    if not available ArtBAs then
      do:
        message "Finner ikke artikkel!"
          view-as alert-box.
        return no-apply.
      end.       
    if ArtBas.LopNr = 0 or
       ArtBas.LopNr = ? then
      do:
        MESSAGE "Løpenummer er ikke tildelt artikkelen?" skip
                "Innleveranse kan ikke skje før løpenummer er tildelt."
                VIEW-AS ALERT-BOX MESSAGE 
                        TITLE "Full innlevering".
        RETURN NO-APPLY.        
      end.
  
    /* Sjekker at kalkyle er satt */
    find Butiker no-lock where
      Butiker.Butik = wCentralLager no-error.
    find BestPris no-lock where
      BestPris.BestNr   = BestHode.BestNr and
      BestPris.BestStat = BestHode.BestStat and
      BestPris.ProfilNr = Butiker.ProfilNr no-error.
    if not available BestPris then
      do:
        message "Det er ikke lagt inn kalkyle på bestillingen for sentrallager."
          view-as alert-box message title "Melding".
        return no-apply.
      end.

    assign wOk = false.
    if (BestPris.VareKost <= 0 or BestPris.Pris <= 0) then
      do:
        message "Kalkylen står med 0 i varekos/pris, vil du virkelig levere inn?"
          view-as alert-box question buttons YES-NO title "Ufullstendig kalkyle"
          update wOk. 
      end.
    else 
    MESSAGE "Bekreft full innlevering?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
                    TITLE "Full innlevering" UPDATE wOK.
    IF NOT wOK THEN
        RETURN NO-APPLY.  

    ASSIGN wFstRow = ch_Grid:FixedRows
           wLstRow = ch_Grid:Rows - 1
           wFstCol = 2
           wLstCol = 2.
    RUN PopUpChoose("Fullinlev").
    APPLY "CHOOSE" TO BUTTON-Lagra.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Kalkyl
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Kalkyl C-Win
ON CHOOSE OF BUTTON-Kalkyl IN FRAME DEFAULT-FRAME /* Kalkyle... */
DO:
  RUN d-vbestkalkyle.w (RECID(ArtBas),wBestHodeRecid).
  IF RETURN-VALUE <> "Avbryt" THEN DO:
      FIND CURRENT BestHode EXCLUSIVE.
      FIND Butiker WHERE Butiker.Butik = wCentrallager NO-LOCK.
      FIND BestPris WHERE BestPris.BestNr = BestHode.BestNr AND
                          BestPris.BestStat = BestHode.BestStat AND
                          BestPris.ProfilNr = Butiker.ProfilNr NO-LOCK.
      RUN PrisKalk ("").
      FIND CURRENT BestHode NO-LOCK.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Lagra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Lagra C-Win
ON CHOOSE OF BUTTON-Lagra IN FRAME DEFAULT-FRAME /* Lagra */
DO:
  run LagreBest.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame C-Win OCX.KeyPress
PROCEDURE CtrlFrame.VSFlexGrid.KeyPress .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  Required for OCX.
    KeyAscii
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT-OUTPUT PARAMETER p-KeyAscii    AS INTEGER NO-UNDO.
DEFINE VARIABLE               wCentralValue AS INTE    NO-UNDO.
DEFINE VARIABLE               wCellNy       AS INTE    NO-UNDO.
DEFINE VARIABLE               wE1Value      AS INTE    NO-UNDO.
DEFINE VARIABLE               wE2Value      AS INTE    NO-UNDO.
DEFINE VARIABLE               wTmpValue     AS INTE    NO-UNDO.
DEFINE VARIABLE               wEntry1       AS CHAR    NO-UNDO.
DEFINE VARIABLE               wEntry2       AS CHAR    NO-UNDO.
DEFINE VARIABLE               wAvskrevet    AS LOGI    NO-UNDO.
DO WITH FRAME {&FRAME-NAME}:
IF ch_Grid:TextMatrix(ch_Grid:Row,0) = "" THEN
    RETURN NO-APPLY.
IF NUM-ENTRIES(ch_Grid:Text,"/") <> 2 AND p-KeyAscii <> 13 THEN
    RETURN NO-APPLY.
/* 010522 ken1 */
IF NUM-ENTRIES(ch_Grid:Text,"/") = 2 AND INT(ENTRY(2,ch_Grid:Text,"/")) = 0 AND p-KeyAscii = 48 THEN
    RETURN NO-APPLY.
IF BestHode.BestStat = 6 THEN
    RETURN NO-APPLY.
IF p-KeyAscii = 13 THEN DO:
    IF ch_Grid:Col = ch_Grid:Cols - 1 AND ch_Grid:Row = ch_Grid:Rows - 1 THEN 
        RETURN NO-APPLY.
    IF ch_Grid:Col < ch_Grid:Cols - 1 THEN DO:
      ASSIGN ch_Grid:Col = ch_Grid:Col + 1.
      IF ch_Grid:Col >= wSHC# AND ch_Grid:LeftCol < 
                ch_Grid:Col - wSHC# + 1 + ch_Grid:FixedCols THEN
      ch_Grid:LeftCol = ch_Grid:Col - wSHC# + + 1 + ch_Grid:FixedCols.
    END.
    else IF ch_Grid:Row < ch_Grid:Rows - 1 then
            ASSIGN ch_Grid:Col = ch_Grid:FixedCols
                   ch_Grid:Row = ch_Grid:Row + 1
                   ch_Grid:LeftCol = ch_Grid:FixedCols.
                         
    RETURN NO-APPLY.
END.
ASSIGN wAvskrevet = TRIM(ENTRY(2,ch_Grid:Text,"/")) = wAvskrevetCell
       wE1Value = INT(ENTRY(1,ch_Grid:Text,"/"))
       wE2Value = IF NOT wAvskrevet THEN INT(ENTRY(2,ch_Grid:Text,"/"))
                  ELSE 0.

IF p-KeyAscii = 8 OR p-KeyAscii = 32 OR (p-KeyAscii >= 43 AND p-KeyAscii <= 45) OR
                     (p-KeyAscii >= 48 AND p-KeyAscii <= 57)     THEN DO:
    IF p-KeyAscii = 8 OR p-KeyAscii = 44 THEN DO:                  /* DEL */
        IF wAvskrevet = TRUE THEN DO:
            ASSIGN ch_Grid:Text = ENTRY(1,ch_Grid:Text,"/") + "/        "
                   FILL-In-Rest:SCREEN-VALUE = STRING(INT(FILL-In-Rest:SCREEN-VALUE) + INT(ENTRY(1,ch_Grid:Text,"/"))).
            RETURN NO-APPLY.
        END.
        IF wE2Value = 0 THEN
            RETURN NO-APPLY.
        ASSIGN wEntry2     = ENTRY(2,ch_Grid:Text,"/")
               wCellNy      = INT(SUBSTR(TRIM(wEntry2),1,LENGTH(TRIM(wEntry2)) - 1)).
        ASSIGN ch_Grid:Text = ENTRY(1,ch_Grid:Text,"/") + "/"  + IF wCellNy > 0 THEN
                       FILL(" ", 2 * (4 - LENGTH(STRING(wCellNy)))) + STRING(wCellNy)
                       ELSE "        "
               ch_Grid:TextMatrix(ch_Grid:Row,1) = ENTRY(1,ch_Grid:TextMatrix(ch_Grid:Row,1),"/")
                         + "/" + LagString(INT(ENTRY(2,ch_Grid:TextMatrix(ch_Grid:Row,1),"/")) -
                               (wE2Value - wCellNy)).
                   FixFillIn(ch_Grid:Row,ch_Grid:Col,"-",wE2Value - wCellNy).

    END.
    ELSE IF p-KeyAscii = 32 AND ENTRY(2,ch_Grid:Text,"/") <> "" THEN DO: /* SPACE */
        IF wAvskrevet = TRUE THEN DO:
            ASSIGN ch_Grid:Text = ENTRY(1,ch_Grid:Text,"/") + "/        "

                   FILL-In-Rest:SCREEN-VALUE = STRING(INT(FILL-In-Rest:SCREEN-VALUE) + INT(ENTRY(1,ch_Grid:Text,"/"))).
            RETURN NO-APPLY.
        END.
        ASSIGN wTmpValue = INT(ENTRY(2,ch_Grid:Text,"/"))
               ch_Grid:TextMatrix(ch_Grid:Row,1) = ENTRY(1,ch_Grid:TextMatrix(ch_Grid:Row,1),"/")
                         + "/" + LagString(INT(ENTRY(2,ch_Grid:TextMatrix(ch_Grid:Row,1),"/")) - wTmpValue)
                 ch_Grid:Text = ENTRY(1,ch_Grid:Text,"/") + "/        ". /* sist */
               FixFillIn(ch_Grid:Row,ch_Grid:Col,"-",wTmpValue).
                 
        RETURN NO-APPLY.
    END.
    ELSE IF p-KeyAscii = 43 THEN DO: /* + */
        IF wAvskrevet = TRUE THEN
            RETURN NO-APPLY.
        IF wE2Value + 1 > 9999 THEN
            RETURN NO-APPLY.
        ASSIGN ch_Grid:Text = ENTRY(1,ch_Grid:Text,"/") + "/"  +
                FILL(" ", 2 * (4 - LENGTH(STRING(wE2Value + 1)))) + STRING(wE2Value + 1).
               ch_Grid:TextMatrix(ch_Grid:Row,1) = ENTRY(1,ch_Grid:TextMatrix(ch_Grid:Row,1),"/")
                         + "/" + LagString(INT(ENTRY(2,ch_Grid:TextMatrix(ch_Grid:Row,1),"/")) + 1).
               FixFillIn(ch_Grid:Row,ch_Grid:Col,"+",1).
    END.
    ELSE IF p-KeyAscii = 45 THEN DO: /* - */
        IF wAvskrevet = TRUE THEN
            RETURN NO-APPLY.
        IF INT(ENTRY(2,ch_Grid:Text,"/")) = 0 THEN
            RETURN NO-APPLY.
        ASSIGN ch_Grid:Text = IF wE2Value - 1 = 0 THEN
                       ENTRY(1,ch_Grid:Text,"/") + "/        "
                   ELSE            
                      ENTRY(1,ch_Grid:Text,"/") + "/" +
                          FILL(" ", 2 * (4 - LENGTH(STRING(wE2Value - 1))))  +
                                  STRING(wE2Value - 1)
               ch_Grid:TextMatrix(ch_Grid:Row,1) = ENTRY(1,ch_Grid:TextMatrix(ch_Grid:Row,1),"/")
                         + "/" + LagString(INT(ENTRY(2,ch_Grid:TextMatrix(ch_Grid:Row,1),"/")) - 1).
               FixFillIn(ch_Grid:Row,ch_Grid:Col,"-",1).
    END.
    ELSE IF p-KeyAscii > 48 OR (ch_Grid:Text > "" AND p-KeyAscii = 48) THEN DO:
        IF wAvskrevet = TRUE THEN
            RETURN NO-APPLY.
        ASSIGN wCellNy       = 10 * wE2Value + p-KeyAscii - 48.
          IF wE2Value >= 1000 THEN
            RETURN NO-APPLY.
        ASSIGN ch_Grid:Text =  ENTRY(1,ch_Grid:Text,"/") + "/" +
                                 FILL(" ", 2 * (4 - LENGTH(STRING(wCellNy))))  + STRING(wCellNy).
               ch_Grid:TextMatrix(ch_Grid:Row,1) = ENTRY(1,ch_Grid:TextMatrix(ch_Grid:Row,1),"/") + "/" + 
                         LagString(INT(ENTRY(2,ch_Grid:TextMatrix(ch_Grid:Row,1),"/")) + (wCellNy - wE2Value)).               
               FixFillIn(ch_Grid:Row,ch_Grid:Col,"+",wCellNy - wE2Value).
    END.
    RUN FixInnlev(ch_Grid:Col).
    ASSIGN BUTTON-Lagra:SENSITIVE   IN FRAME {&FRAME-NAME} = YES.
    
END.
ELSE
    RETURN NO-APPLY.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame C-Win OCX.MouseDown
PROCEDURE CtrlFrame.VSFlexGrid.MouseDown .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  Required for OCX.
    Button
    Shift
    x
    y
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER p-Button AS INTEGER         NO-UNDO.
DEFINE INPUT PARAMETER p-Shift  AS INTEGER         NO-UNDO.
DEFINE INPUT PARAMETER p-x      AS INTEGER         NO-UNDO.
DEFINE INPUT PARAMETER p-y      AS INTEGER         NO-UNDO.
DEFINE             VAR wIdx     AS INTEGER         NO-UNDO.
DEFINE             VAR wButik   LIKE Butiker.Butik NO-UNDO.

IF ch_Grid:MouseRow < 0 OR ch_Grid:TextMatrix(ch_Grid:MouseRow,0) = "" THEN
    RETURN NO-APPLY. 
IF p-Button = 1 AND ch_Grid:MouseRow >= ch_Grid:FixedRows - 1 AND ch_Grid:MouseCol = 0 then do:
      ASSIGN wButik = IF  ch_Grid:MouseRow = ch_Grid:FixedRows - 1 THEN ? ELSE
                      IF ch_Grid:MouseRow = ch_Grid:FixedRows THEN
                   wCentrallager ELSE INT(ch_Grid:TextMatrix(ch_Grid:MouseRow,0))
             wBestString = "".
    IF wButik = ? THEN
      do:
        RUN d-visgin.w (wBestHodeRecid,wButik,wStorlekar,wBestString,ch_Grid).
      end.
    ELSE DO:
      DO wIdx = 1 to NUM-ENTRIES(wStorlekar," "):
        FIND BestStr WHERE BestStr.BestNr   = BestHode.BestNr            AND
                           BestStr.Butik    = wButik                     AND
                           BestStr.Storl    = ENTRY(wIdx,wStorlekar," ") AND
                           BestStr.BestStat = BestHode.BestStat NO-LOCK NO-ERROR.
        IF AVAIL BestStr THEN
            ASSIGN wBestString = IF wBestString = " " THEN STRING(BestStr.Bestilt) ELSE
                                    wBestString + " " + STRING(BestStr.Bestilt).
        ELSE
            ASSIGN wBestString = IF wBestString = " " THEN "0" ELSE
                                    wBestString + " 0".
      END.

      RUN d-visgin.w (wBestHodeRecid,wButik,wStorlekar,wBestString,ch_Grid).
    END.
END.
ELSE IF BestHode.BestStat = 6 THEN
    RETURN NO-APPLY.
ELSE IF p-Button = 2 AND ch_Grid:MouseCol > 1 AND
                    ch_Grid:MouseRow > ch_Grid:FixedRows - 1 then do:
    ASSIGN ch_Grid:Col = ch_Grid:MouseCol
           ch_Grid:Row = ch_Grid:MouseRow
           wFstRow     = ch_Grid:Row
           wLstRow     = ch_Grid:Row
           wFstCol     = ch_Grid:Col
           wLstCol     = ch_Grid:Col.
          
    Run FixPopUp("En").
    APPLY "GO" to frame frame-a.
END.
ELSE
    RETURN NO-APPLY.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame C-Win OCX.MouseMove
PROCEDURE CtrlFrame.VSFlexGrid.MouseMove .
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
DEFINE VARIABLE cStatus AS CHARACTER  NO-UNDO.
    IF iMouseMoveRow <> ch_Grid:MouseRow THEN DO:
        ASSIGN iMouseMoveRow = ch_Grid:MouseRow.
               cStatus = IF iMouseMoveRow = 0 THEN "" ELSE ENTRY(iMouseMoveRow,cStatusStr) NO-ERROR.
        STATUS DEFAULT cStatus IN WINDOW C-Win.
    END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame C-Win OCX.MouseUp
PROCEDURE CtrlFrame.VSFlexGrid.MouseUp .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  Required for OCX.
    Button
    Shift
    x
    y
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER p-Button AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER p-Shift  AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER p-x      AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER p-y      AS INTEGER NO-UNDO.

    IF ch_Grid:TextMatrix(5,0) = "" THEN
        RETURN NO-APPLY.
    ELSE IF BestHode.BestStat = 6 THEN
        RETURN NO-APPLY.

    IF ch_Grid:Row = ch_Grid:RowSel AND ch_Grid:Col = ch_Grid:ColSel THEN DO:
        ASSIGN wFstRow = ch_Grid:Row
               wLstRow = ch_Grid:Row.
        Run FixPopUp("En").
    END.
    ELSE DO:
      ASSIGN wFstRow = IF ch_Grid:Row < ch_Grid:RowSel THEN ch_Grid:Row
                                                             ELSE ch_Grid:RowSel
             wLstRow = IF ch_Grid:Row < ch_Grid:RowSel THEN ch_Grid:RowSel
                                                             ELSE ch_Grid:Row
             wFstCol = IF ch_Grid:Col < ch_Grid:ColSel THEN ch_Grid:Col
                                                             ELSE ch_Grid:ColSel
             wLstCol = IF ch_Grid:Col < ch_Grid:ColSel THEN ch_Grid:ColSel
                                                             ELSE ch_Grid:Col.
      Run FixPopUp("Multi").
      APPLY "GO" TO FRAME FRAME-A.
      ASSIGN ch_Grid:ColSel = ch_Grid:Col
             ch_Grid:RowSel = ch_Grid:Row.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame C-Win
ON TAB OF CtrlFrame /* VSFlexGrid */
DO:
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BestHode.DirekteLev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BestHode.DirekteLev C-Win
ON TAB OF BestHode.DirekteLev IN FRAME DEFAULT-FRAME /* Direktelevert */
or "RETURN":U of BestHode.DirekteLev
DO:
  apply "entry":U to TOGGLE-Tillagg in frame DEFAULT-FRAME.
END.

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


&Scoped-define SELF-NAME m_Avskriv_storlek
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Avskriv_storlek C-Win
ON CHOOSE OF MENU-ITEM m_Avskriv_storlek /* Avskriv størrelse(r) */
DO:
    RUN PopUpChoose(SELF:NAME).
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Avslutt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Avslutt C-Win
ON CHOOSE OF MENU-ITEM m_Avslutt /* Avslutt */
DO:
    APPLY "WINDOW-CLOSE" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_clear_butik
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_clear_butik C-Win
ON CHOOSE OF MENU-ITEM m_clear_butik /* Angre registrert (butikk) */
DO:
    RUN PopUpChoose(SELF:NAME).
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_clear_butiker
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_clear_butiker C-Win
ON CHOOSE OF MENU-ITEM m_clear_butiker /* Angre registrert (butikker) */
DO:
    RUN PopUpChoose(SELF:NAME).
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Full_inlev_butik
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Full_inlev_butik C-Win
ON CHOOSE OF MENU-ITEM m_Full_inlev_butik /* Full inleveranse (butikk) */
DO:
    RUN PopUpChoose(SELF:NAME).
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Full_inlev_butiker
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Full_inlev_butiker C-Win
ON CHOOSE OF MENU-ITEM m_Full_inlev_butiker /* Full inleveranse (butikker) */
DO:
    RUN PopUpChoose(SELF:NAME).
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOGGLE-Tillagg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOGGLE-Tillagg C-Win
ON TAB OF TOGGLE-Tillagg IN FRAME DEFAULT-FRAME /* Tilleggsbest */
or "RETURN":U of TOGGLE-Tillagg
DO:
  apply "entry":U to BestHode.Merknad in frame DEFAULT-FRAME.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-1
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
{inutmld.i &Modus = "Opprett"} /* Melder fra at programmet har startet. */

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
{genlib.i
  &NoLibCall      = "Nei"
  &WindowName     = "Innleveranse av bestillinger"
  &PreIClose      = " "
  &PostIClose     = " "
  &PostDisable_ui = "IF VALID-HANDLE(chCtrlFrame) THEN
                        RELEASE OBJECT chCtrlFrame NO-ERROR.
                     IF VALID-HANDLE(CtrlFrame) THEN
                        DELETE OBJECT CtrlFrame NO-ERROR.
                     IF VALID-HANDLE(chImage-Sko) THEN
                        RELEASE OBJECT chImage-Sko NO-ERROR.
                     IF VALID-HANDLE(Image-Sko) THEN
                        DELETE OBJECT Image-Sko NO-ERROR.
                    ASSIGN chCtrlFrame = ?
                           chImage-Sko = ?
                           ch_Grid      = ?. 
                     "
}

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

if valid-handle(wLibHandle) then
  run SjekkLApTop in wLibHandle (output wLapTop).
  
/* Ingen innleveranse kan gjøres på laptop. */
if wLapTop then
  DO:
    message "Inneveranser kan ikke gjøres i innkjøpsmodulen"
            view-as alert-box message title "Melding".
    return.
  end.  

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  FIND FIRST ArtBas WHERE RECID(ArtBas) = wArtBasRecid NO-LOCK NO-ERROR.
  IF NOT AVAILABLE ArtBas THEN DO:
    MESSAGE "Artikkelen finnes ikke." VIEW-AS ALERT-BOX ERROR.
    RETURN NO-APPLY "AVBRYT".
  END.
  IF wBestHodeRecid <> ? THEN DO:
      FIND BestHode WHERE RECID(BestHode) = wBestHodeRecid NO-LOCK NO-ERROR.
      IF NOT AVAIL BestHode THEN DO:
          MESSAGE "Bestillingen finnes ikke." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY "AVBRYT".
      END.
      ELSE IF BestHode.BestStat < 4 THEN DO:
          MESSAGE "Ordre har feil status" VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY "AVBRYT".
      END.
  END.
  {syspara.i 5 2 99 wStatus}
  {syspara.i 5 4 8 cPassordkrav}
  /* Kode for låsing av artikkelnummer ved overføring. */
  {syspara.i 1 2 3 wEDB-System}
  {syspar2.i 5 4 5 cBrukButikEtikett} /* överstyr etiketter.butik vid ikke direktelev */
  if wEDB-System = "" then
    wEDB-System = "OVERFOR-LOCK".
  {syspara.i 5 4 16 cManRegEAN}
  {syspara.i 5 4 17 cInnlevSamlet}

  /* {syspar2.i 1 2 3 wTabell} */
  /* if wEDB-System = "" then  */
  /*   wEDB-System = "ArtBas". */

  RUN SetWindowTitle.
  RUN InitSortiment.

  RUN enable_UI.
  RUN InitFillIns.
  RUN PrisKalk("INIT").
  RUN ButtonEnaDis.
  RUN initStatus.
  ASSIGN BUTTON-Etiketter:SENSITIVE = ArtBas.Etikett <> 0
         TOGGLE-Tillagg:CHECKED =  BestHode.BestType = 2
         ch_Grid:Row = ch_Grid:FixedRows
         wFstRow     = ch_Grid:FixedRows
         wLstRow     = ch_Grid:FixedRows
         ch_Grid:Col = ch_Grid:FixedCols
         wFstCol     = ch_Grid:FixedCols
         wLstCol     = ch_Grid:FixedCols.
/*   IF BestHode.BestStat = 4 THEN         */
/*       RUN GenererEan IN THIS-PROCEDURE. */
  if BestHode.BestStat < 6 then
    Run FixPopUp("En").  /* Initiering av popupmeny */

  RUN VisArtBas.
  run VisBilde(1).
  
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Apply-mouse-menu-click C-Win 
PROCEDURE Apply-mouse-menu-click :
/*------------------------------------------------------------------------------
Purpose:     Programatic click the right mouse button on a widget
Parameters:  Widget-handle on which you want to click
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER  p-wh   AS WIDGET-HANDLE  NO-UNDO.
   DEF VAR ReturnValue AS INTEGER NO-UNDO.
   RUN SendMessageA in wWindows (INPUT p-wh:HWND, 
                                 INPUT 516,
                                 INPUT 2,
                                 INPUT 0,
                                 OUTPUT ReturnValue).
   RUN SendMessageA in wWindows (INPUT p-wh:HWND, 
                                 INPUT 517,
                                 INPUT 0, 
                                 INPUT 0,
                                 OUTPUT ReturnValue).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ButtonEnaDis C-Win 
PROCEDURE ButtonEnaDis :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:

    ASSIGN BUTTON-Fullinlev:SENSITIVE = BestHode.BestStat < 6.
/*            BUTTON-Kalkyl:SENSITIVE = BestHode.BestStat = 4. */
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CenterMouseCursor C-Win 
PROCEDURE CenterMouseCursor :
/*------------------------------------------------------------------------------
Purpose:     Move the mouse cursor to the middle of a widget
Parameters:  the widget-handle
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER  p-wh   AS WIDGET-HANDLE  NO-UNDO.
   DEF VAR lppoint     AS MEMPTR  NO-UNDO.  /* POINT FAR*  */
   DEF VAR ReturnValue AS INTEGER NO-UNDO.   SET-SIZE(lppoint)= 2 * 4.
   PUT-{&INT}(lppoint,1 + 0 * {&INTSIZE})=INTEGER(p-wh:WIDTH-PIXELS / 2).
   PUT-{&INT}(lppoint,1 + 1 * {&INTSIZE})=INTEGER(p-wh:HEIGHT-PIXELS / 2).
   RUN ClientToScreen in hpApi (INPUT p-wh:HWND, 
                                INPUT GET-POINTER-VALUE(lppoint),
                                OUTPUT ReturnValue).
   RUN SetCursorPos in hpApi   (INPUT GET-{&INT}(lppoint,1 + 0 * {&INTSIZE}), 
                                INPUT GET-{&INT}(lppoint,1 + 1 * {&INTSIZE}),
                                OUTPUT ReturnValue).   SET-SIZE(lppoint)= 0.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ChgKasse C-Win 
PROCEDURE ChgKasse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER wAntKasser AS INTE NO-UNDO.
    DEFINE INPUT PARAMETER wInit      AS LOGI NO-UNDO.
    DEFINE VAR             wIdx       AS INTE NO-UNDO.
    DEFINE VAR             wMatrisIdx AS INTE NO-UNDO.
    DEFINE VAR             wFel       AS LOGI NO-UNDO.

/* Kontrollera om vi kan minska antalet kasser */
  IF wAntKasser < 0 THEN DO:
    DO wIdx = 1 to NUM-ENTRIES(tLevSort.Storrelser," "):
        wMatrisIdx =  LOOKUP(ENTRY(wIdx,tLevSort.Storrelser," "),wStorlekar," ").
        IF INT(ENTRY(wIdx,tLevSort.Fordel," ")) * ABSOLUTE(wAntKasser) > 
                               INT(ch_Grid:TextMatrix(4,wMatrisIdx + 1)) THEN DO:
            ASSIGN wFel = YES.
            LEAVE.
        END.
    END.
    IF wFel = YES THEN DO:
        ASSIGN tLevSort.AntSort:SCREEN-VALUE IN BROWSE BROWSE-1 = STRING(wBest).
        MESSAGE "Minskning med " ABSOLUTE(wAntKasser) " kan ikke gjøres."
             VIEW-AS ALERT-BOX ERROR TITLE "Fel".
        RETURN.
    END.
  END.
/* Slut kontroll                               */

    ASSIGN ch_Grid:TextMatrix(1,1) = 
            STRING(INT(ch_Grid:TextMatrix(1,1)) + wAntKasser * tLevSort.Antal) + " "
           ch_Grid:TextMatrix(3,1) = 
            STRING(INT(ch_Grid:TextMatrix(3,1)) + wAntKasser * tLevSort.Antal) + " ".
    IF NOT wInit THEN
        ASSIGN ch_Grid:TextMatrix(4,1) = 
            STRING(INT(ch_Grid:TextMatrix(4,1)) + wAntKasser * tLevSort.Antal) + " ".

    DO wIdx = 1 to NUM-ENTRIES(tLevSort.Storrelser," "):
        wMatrisIdx =  LOOKUP(ENTRY(wIdx,tLevSort.Storrelser," "),wStorlekar," ").
        ASSIGN ch_Grid:TextMatrix(1,wMatrisIdx + 1) =
                 STRING(INT(ch_Grid:TextMatrix(1,wMatrisIdx + 1)) + 
                    INT(ENTRY(wIdx,tLevSort.Fordel," ")) * wAntKasser) + " "
               ch_Grid:TextMatrix(3,wMatrisIdx + 1) =
                 STRING(INT(ch_Grid:TextMatrix(3,wMatrisIdx + 1)) +
                    INT(ENTRY(wIdx,tLevSort.Fordel," ")) * wAntKasser) + " ".
        IF NOT wInit THEN
            ASSIGN ch_Grid:TextMatrix(4,wMatrisIdx + 1) =
                     STRING(INT(ch_Grid:TextMatrix(4,wMatrisIdx + 1)) + 
                        INT(ENTRY(wIdx,tLevSort.Fordel," ")) * wAntKasser) + " ".
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

OCXFile = SEARCH( "w-gridinnlev.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chCtrlFrame = CtrlFrame:COM-HANDLE
    UIB_S = chCtrlFrame:LoadControls( OCXFile, "CtrlFrame":U)
    CtrlFrame:NAME = "CtrlFrame":U
    chImage-Sko = Image-Sko:COM-HANDLE
    UIB_S = chImage-Sko:LoadControls( OCXFile, "Image-Sko":U)
    Image-Sko:NAME = "Image-Sko":U
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "w-gridinnlev.wrx":U SKIP(1)
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
  DISPLAY FILL-IN-Vgr-Lopnr FI-OrdreLev TOGGLE-Tillagg FILL-IN-Kvar 
          FILL-IN-Innlev FILL-IN-Rest FILL-IN-2 FILL-IN-1 FILL-IN-KvarL 
          FILL-IN-InnlevL FILL-IN-RestL 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  IF AVAILABLE ArtBas THEN 
    DISPLAY ArtBas.VgKat ArtBas.Beskr ArtBas.LevNr ArtBas.LevKod ArtBas.LevFargKod 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  IF AVAILABLE BestHode THEN 
    DISPLAY BestHode.BestillingsDato BestHode.LevDato BestHode.TotInnkjVerdi 
          BestHode.RegistrertAv BestHode.TotDbKr BestHode.DirekteLev 
          BestHode.TotSalgsVerdi BestHode.Beskrivelse BestHode.Merknad 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  IF AVAILABLE BestPris THEN 
    DISPLAY BestPris.VareKost BestPris.DBKr BestPris.Pris 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  IF AVAILABLE LevBas THEN 
    DISPLAY LevBas.levnamn 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE B-Etiketter RECT-1 RECT-3 RECT-4 RECT-47 RECT-48 B-Print B-PrintInnlev 
         BUTTON-Kalkyl BUTTON-Etiketter Btn_Done Btn_Help BROWSE-1 
         BestHode.Merknad FILL-IN-2 FILL-IN-1 FILL-IN-KvarL FILL-IN-InnlevL 
         FILL-IN-RestL 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FixInnlev C-Win 
PROCEDURE FixInnlev :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER wIpCol       AS INTE NO-UNDO.
    DEFINE VAR             wCol       AS INTE NO-UNDO.
    DEFINE VAR             wRow       AS INTE NO-UNDO.
    DEFINE VAR             wColInnlev AS INTE NO-UNDO.
    DEFINE VAR             wTotInnlev AS INTE NO-UNDO.
    DO wRow = ch_Grid:FixedRows TO ch_Grid:Rows - 1:
        IF NUM-ENTRIES(ch_Grid:TextMatrix(wRow,wIpCol),"/") <> 2 OR
           TRIM(ENTRY(2,ch_Grid:TextMatrix(wRow,wIpCol),"/")) = wAvskrevetCell OR
           TRIM(ENTRY(2,ch_Grid:TextMatrix(wRow,wIpCol),"/")) = "" THEN
               NEXT.
        ASSIGN wColInnlev = wColInnlev + INT(ENTRY(2,ch_Grid:TextMatrix(wRow,wIpCol),"/")).
    END.
    ASSIGN ch_Grid:TextMatrix(3,wIpCol) = STRING(wColInnlev) + " ".
    DO wCol = ch_Grid:FixedCols TO ch_Grid:Cols - 1:
        ASSIGN wTotInnlev = wTotInnlev + INT(ch_Grid:TextMatrix(3,wCol)).
    END.
    ASSIGN ch_Grid:TextMatrix(3,1)    = STRING(wTotInnlev) + " ".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FixPopUp C-Win 
PROCEDURE FixPopUp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER wMode     AS CHAR NO-UNDO.
    DEFINE VAR             wCount1   AS INTE NO-UNDO.
    DEFINE VAR             wCount2   AS INTE NO-UNDO.
    DEFINE VAR             wOmrData  AS LOGI NO-UNDO.
    DEFINE VAR             wNyStaket AS LOGI NO-UNDO.
    CASE wMode:
        WHEN "En" THEN DO:
            ASSIGN MENU-ITEM m_Full_inlev_butik:SENSITIVE IN MENU POPUP-MENU-FRAME-A   = OkInlev()
                   MENU-ITEM m_Full_inlev_butiker:SENSITIVE IN MENU POPUP-MENU-FRAME-A = NO
                   MENU-ITEM m_clear_butik:SENSITIVE IN MENU POPUP-MENU-FRAME-A   = OkClear("EN")
                   MENU-ITEM m_clear_butiker:SENSITIVE IN MENU POPUP-MENU-FRAME-A = NO
                   MENU-ITEM m_Avskriv_storlek:SENSITIVE IN MENU POPUP-MENU-FRAME-A = OkMak().

        END.
        WHEN "Multi" THEN DO:
            ASSIGN MENU-ITEM m_Full_inlev_butik:SENSITIVE IN MENU POPUP-MENU-FRAME-A   = NO
                   MENU-ITEM m_Full_inlev_butiker:SENSITIVE IN MENU POPUP-MENU-FRAME-A = IF wFstCol = wLstCol THEN
                                                                                OkInlev() ELSE NO
                   MENU-ITEM m_clear_butik:SENSITIVE IN MENU POPUP-MENU-FRAME-A   = NO 
                   MENU-ITEM m_clear_butiker:SENSITIVE IN MENU POPUP-MENU-FRAME-A = IF wFstCol = wLstCol THEN
                                                                                OkClear("FLER") ELSE NO
                   MENU-ITEM m_Avskriv_storlek:SENSITIVE IN MENU POPUP-MENU-FRAME-A = OkMak().
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GenererEan C-Win 
PROCEDURE GenererEan :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters: iKodeType = 3 genererar EAN utifrån BestLevert
              iKodeType = 4 genererar EAN utifrån BestStr
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iKodeType AS INTEGER    NO-UNDO.
    cKontrStorl = AlleEANok(cKontrStorl).
    IF TRIM(cManRegEAN) = "1" AND cKontrStorl <> "" THEN
        RUN StrekKode.w (THIS-PROCEDURE).
    RUN genStrekKode.p (BestHode.BestNr,iKodeType,"").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitFillIns C-Win 
PROCEDURE InitFillIns :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR wRow      AS INTE NO-UNDO.
DEF VAR wTmpCount AS INTE NO-UNDO.
IF BestHode.BestStat > 5 THEN
    RETURN NO-APPLY.
DO WITH FRAME {&FRAME-NAME}:
    DO wRow = ch_Grid:FixedRows TO ch_Grid:Rows - 1:
        ASSIGN wTmpCount = wTmpCount + INT(ENTRY(1,ch_Grid:TextMatrix(wRow,1),"/")).    
    END.
    ASSIGN FILL-IN-Kvar:SCREEN-VALUE = STRING(wTmpCount)
           FILL-IN-InnLev:SCREEN-VALUE = "0"
           FILL-IN-Rest:SCREEN-VALUE = STRING(wTmpCount).
END.
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
DEFINE VAR wCol        AS INTE NO-UNDO.
DEFINE VAR wTotKassPar AS INTE NO-UNDO.
DEFINE VAR wTotCentral AS INTE NO-UNDO.
DEFINE VAR wTotBestilt AS INTE NO-UNDO.
DEFINE VAR wFargStr    AS CHAR NO-UNDO.

ASSIGN ch_Grid = chCtrlFrame:vsFlexGrid.

ASSIGN ch_Grid:CellPictureAlignment = 1
       ch_Grid:Redraw = FALSE. /* disable repaint while populating */

ch_Grid:Clear().

ASSIGN ch_Grid:AllowUserResizing = 0    /* Fixed columns/rows */
       ch_Grid:Enabled           = TRUE /* Updateable grid */
       ch_Grid:AllowBigSelection = FALSE
       ch_Grid:Appearance        = 1 
       ch_Grid:Rows              = 6
       ch_Grid:Cols              = NUM-ENTRIES(wStorlekar," ") + 2
       ch_Grid:FixedRows         = 4
       ch_Grid:FixedCols         = 2
       ch_Grid:TextStyle         = 0
       ch_Grid:TextStyleFixed    = 0
       ch_Grid:ColWidth(1)       = 950
       ch_Grid:TextMatrix(0,1)   = "Total"
       ch_Grid:TextMatrix(2,0)   = "Fri "
       ch_Grid:TextMatrix(1,0)   = "Kasser "
       ch_Grid:TextMatrix(3,0)   = "Innlevering".

/* Initiering av bakgrundsfärg för rad 1 - 5 */

ASSIGN wFargStr = GetFarg().
IF NUM-ENTRIES(wFargStr) = 5 THEN 
DO wRow = 0 TO 4:
    IF INT(ENTRY(wRow + 1,wFargStr)) = 0 THEN
        NEXT.
    DO wIdx = 0 TO ch_Grid:Cols - 1:
        ASSIGN ch_Grid:Row = wRow
               ch_Grid:Col = wIdx.
               ch_Grid:CellBackColor = INT(ENTRY(wRow + 1,wFargStr)).
    END.
END.

/* Initiering av storleksrad */
DO wIdx = 1 to NUM-ENTRIES(wStorlekar," "):
    ASSIGN ch_Grid:TextMatrix(0,wIdx + 1) = ENTRY(wIdx,wStorlekar," ") + " ".
           ch_Grid:ColWidth(wIdx + 1) = 950.
END.

/* Kasser */
FOR EACH tLevSort:
    wTotKassPar = wTotKassPar + tLevSort.AntSort * tLevSort.Antal.
    RUN ChgKasse(tLevSort.AntSort,YES).
END.
ASSIGN ch_Grid:TextMatrix(1,1) = STRING(wTotKassPar) + " ".
/* END Kasser */

/* NY 19/9 Fri */
ASSIGN ch_Grid:TextMatrix(2,ch_Grid:FixedCols - 1) = STRING(wFriAnt) + " ".
DO wIdx = 1 TO NUM-ENTRIES(wStorlekar," "):
    ASSIGN ch_Grid:TextMatrix(2,wIdx + ch_Grid:FixedCols - 1) = STRING( wFriAntal[wIdx]) + " ".
END.  
/* NY 19/9 END Fri */

/* Fri OLD
ASSIGN ch_Grid:TextMatrix(2,1) = STRING(wFriAnt) + " ".
FOR EACH Fri-Str:
    ASSIGN wIdx = LOOKUP(Fri-Str.SoStorl,wStorlekar," ") + 1
           ch_Grid:TextMatrix(2,wIdx) = STRING(Fri-Str.SoAnt) + " ".
    EXPORT fri-str.
END.
    OLD END Fri */

/* Beställningsrad */
DO wIdx = 1 to ch_Grid:Cols - 1:
/*    ASSIGN ch_Grid:TextMatrix(3,wIdx) = STRING(INT(ch_Grid:TextMatrix(1,wIdx)) 
                                                + INT(ch_Grid:TextMatrix(2,wIdx))) + " ". */
    ASSIGN ch_Grid:TextMatrix(3,wIdx) = "0 ".
END.
/* END Beställningsrad */

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
       ch_Grid:TextMatrix(4,0)   = "CL " + STRING(Butiker.Butik) + " ".

/* Centrallager */
IF BestHode.BestStat < 6 THEN DO:
    ASSIGN wRow = ch_Grid:FixedRows.
    DO wIdx = 1 to NUM-ENTRIES(wStorlekar," "):
        FIND BestStr WHERE BestStr.BestNr   = BestHode.BestNr            AND
                           BestStr.Butik    = wCentralLager              AND
                           BestStr.Storl    = ENTRY(wIdx,wStorlekar," ") AND
                           BestStr.BestStat = BestHode.BestStat NO-LOCK NO-ERROR.
        IF AVAIL BestStr THEN DO:
            FIND LAST BestLevert WHERE BestLevert.BestNr = BestHode.BestNr            AND
                                       BestLevert.Butik  = wCentralLager              AND
                                       BestLevert.Storl  = ENTRY(wIdx,wStorlekar," ")
                                                           NO-LOCK NO-ERROR.
            IF AVAIL BestLevert THEN DO:
                ASSIGN wTotCentral = wTotCentral + IF BestLevert.Rest < 0 OR BestLevert.Avskrevet THEN 0
                                                   ELSE BestLevert.Rest
                       ch_Grid:TextMatrix(wRow,wIdx + 1) = IF BestLevert.Avskrevet THEN 
                                        wAvskrevetCell + " " + STRING(BestLevert.Rest)
                                   ELSE IF BestLevert.Rest = 0 THEN "        /        " 
                                   ELSE FILL(" ", 2 * (4 - LENGTH(STRING(BestLevert.Rest)))) +
                                    (IF BestLevert.Rest < 0 THEN " " ELSE "") + 
                                           STRING(BestLevert.Rest) + "/        ".
           END.
           ELSE
               ASSIGN wTotCentral = wTotCentral + BestStr.Bestilt
                      ch_Grid:TextMatrix(wRow,wIdx + 1) = (IF BestStr.Bestilt > 0 THEN
                       FILL(" ", 2 * (4 - LENGTH(STRING(BestStr.Bestilt)))) +
                                                    STRING(BestStr.Bestilt) ELSE
                          "        ") + "/        ".
        END.
        ELSE DO:
            FIND LAST BestLevert WHERE BestLevert.BestNr = BestHode.BestNr            AND
                                       BestLevert.Butik  = wCentralLager            AND
                                       BestLevert.Storl  = ENTRY(wIdx,wStorlekar," ")
                                                       NO-LOCK NO-ERROR.
            IF AVAIL BestLevert THEN DO:
                ASSIGN ch_Grid:TextMatrix(wRow,wIdx + 1) = IF BestLevert.Avskrevet THEN 
                                       wAvskrevetCell + " " + STRING(BestLevert.Rest)
                                   ELSE IF BestLevert.Rest = 0 THEN "        /        "
                                   ELSE FILL(" ", 2 * (4 - LENGTH(STRING(BestLevert.Rest)))) +
                                                  " " +  STRING(BestLevert.Rest) + "/        ".
            END.
            ELSE
                ASSIGN ch_Grid:TextMatrix(wRow,wIdx + 1) = "        /        ".
        END.
    END.
    ASSIGN ch_Grid:TextMatrix(wRow,1) = /* STRING(wTotCentral) + " ". */
          LagString(wTotCentral) + "/" + LagString(0).
    
    
END.
/* END Centrallager */

/* Finn resten av butikerna och det som är beställt */
IF BestHode.BestStat < 6 THEN DO:
    ASSIGN wRow = ch_Grid:FixedRows + 1.
    FOR EACH BestLinje OF BestHode WHERE BestLinje.Butik <> wCentralLager NO-LOCK:
        ASSIGN wTotBestilt                = 0
               ch_Grid:Rows               = wRow + 1
               ch_Grid:TextMatrix(wRow,0) = STRING(BestLinje.Butik) + " ".
       
      DO wIdx = 1 to NUM-ENTRIES(wStorlekar," "):
        FIND BestStr WHERE BestStr.BestNr   = BestHode.BestNr            AND
                           BestStr.Butik    = BestLinje.Butik            AND
                           BestStr.Storl    = ENTRY(wIdx,wStorlekar," ") AND
                           BestStr.BestStat = BestHode.BestStat NO-LOCK NO-ERROR.
        IF AVAIL BestStr THEN DO:
            FIND LAST BestLevert WHERE BestLevert.BestNr = BestHode.BestNr            AND
                                       BestLevert.Butik  = BestLinje.Butik            AND
                                       BestLevert.Storl  = ENTRY(wIdx,wStorlekar," ")
                                                           NO-LOCK NO-ERROR.
            IF AVAIL BestLevert THEN DO:
                ASSIGN wTotBestilt = wTotBestilt + IF BestLevert.Rest < 0 OR BestLevert.Avskrevet THEN 0
                                                   ELSE BestLevert.Rest
                       ch_Grid:TextMatrix(wRow,wIdx + 1) = IF BestLevert.Avskrevet THEN 
                                    wAvskrevetCell + " " + STRING(BestLevert.Rest)
                                   ELSE IF BestLevert.Rest = 0 THEN "        /        " 
                                   ELSE FILL(" ", 2 * (4 - LENGTH(STRING(BestLevert.Rest)))) +
                                    (IF BestLevert.Rest < 0 THEN " " ELSE "") + 
                                           STRING(BestLevert.Rest) + "/        ".
           END.
           ELSE
               ASSIGN wTotBestilt = wTotBestilt + BestStr.Bestilt
                      ch_Grid:TextMatrix(wRow,wIdx + 1) = (IF BestStr.Bestilt > 0 THEN
                       FILL(" ", 2 * (4 - LENGTH(STRING(BestStr.Bestilt)))) +
                                                    STRING(BestStr.Bestilt)
                               ELSE "        ") + "/        ".
        END.
        ELSE DO:
            FIND LAST BestLevert WHERE BestLevert.BestNr = BestHode.BestNr            AND
                                       BestLevert.Butik  = BestLinje.Butik            AND
                                       BestLevert.Storl  = ENTRY(wIdx,wStorlekar," ")
                                                       NO-LOCK NO-ERROR.
            IF AVAIL BestLevert THEN DO:
                ASSIGN ch_Grid:TextMatrix(wRow,wIdx + 1) = IF BestLevert.Avskrevet THEN 
                              wAvskrevetCell + " " + STRING(BestLevert.Rest)
                                   ELSE IF BestLevert.Rest = 0 THEN "        /        "
                                   ELSE FILL(" ", 2 * (4 - LENGTH(STRING(BestLevert.Rest)))) +
                                                  " " +  STRING(BestLevert.Rest) + "/        ".
            END.
            ELSE
                ASSIGN ch_Grid:TextMatrix(wRow,wIdx + 1) = "        /        ".
        END.
      END.
      ASSIGN ch_Grid:TextMatrix(wRow,1) = /* STRING(wTotBestilt) + " " */
             LagString(wTotBestilt) + "/" + LagString(0)
             wRow = wRow + 1.
    END.
    DO wRow = 5 TO ch_Grid:Rows - 1:
        DO wCol = 2 TO ch_Grid:Cols - 1:
            IF ch_Grid:TextMatrix(wRow,wCol) = "" THEN
                ASSIGN ch_Grid:TextMatrix(wRow,wCol) = "        /        ".
        END.    
    END.
END.
IF BestHode.BestStat > 5 THEN
    Run VisFullev.
/* Centrera text i celler */
DO wRow = 0 TO ch_Grid:Rows - 1:
    ASSIGN ch_Grid:Row = wRow.
    DO wCol = 2 TO ch_Grid:Cols - 1:
        ASSIGN ch_Grid:Col = wCol
               ch_Grid:CellAlignment = 4.
    END.
END.
DO wRow = ch_Grid:FixedRows TO ch_Grid:Rows - 1:
    IF wRow MOD 2 = 0 THEN
        ASSIGN ch_Grid:Cell(6,wRow,ch_Grid:FixedCols,wRow,ch_Grid:Cols - 1) = 10813439.
END.
ASSIGN ch_Grid:Redraw = TRUE
       ch_Grid:Row = ch_Grid:FixedRows
       ch_Grid:Col = ch_Grid:FixedCols.
APPLY "ENTRY" TO CtrlFrame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitSortiment C-Win 
PROCEDURE InitSortiment :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR wAntal     AS INTE NO-UNDO.
    DEF VAR wIdx       AS INTE NO-UNDO.
    DEF VAR wStrlIntv  AS CHAR NO-UNDO.
    DEF VAR wStrTmp    AS CHAR NO-UNDO.
    DEF VAR wStrTstr   AS CHAR NO-UNDO. /* Alla storlekar i en StrType */
    DEF VAR wX1        AS INT  NO-UNDO.

    ASSIGN wFriSort = CAN-FIND(FIRST LevSort OF LevBas WHERE LevSort.StrType = ArtBas.StrType AND
                                     LevSort.Fri = YES).
    FIND FIRST BestSort OF BestHode WHERE BestSort.Fri = YES NO-LOCK NO-ERROR.
    IF NOT AVAIL BestSort THEN DO:
          .
    END.
    if available BestSort then 
    DO:
      if NUM-ENTRIES(BestSort.Storrelser," ") <> NUM-ENTRIES(BestSort.Fordeling," ") then
        DO:
          /* Setter opp en 0 maske for alle st›rrelser. */
          wFordel = "".
          DO wX1 = 1 to NUM-ENTRIES(BestSort.Storrelser," "):
            wFordel = wFordel +
                      (if wFordel = ""
                         THEN ""
                         ELSE " ") +
                      "0".
          END.
          /* Sumerer opp for hver enkelt st›rrelse. */
          FOR EACH BestStr NO-LOCK where
            BestStr.BestNr   = BestSort.BestNr and
            BestStr.BestStat = BestHode.BestStat:
            assign
              wIdx = LOOKUP(TRIM(BestStr.Storl),BestSort.Storrelser," ")
              wX1  = INT(ENTRY(wIdx,wFordel," ")) + BestStr.Bestilt
              .
            ENTRY(wIdx,wFordel," ") = STRING(wX1).
          END.
        END.
      ELSE
        wFordel = BestSort.Fordeling.
      DO wIdx = 1 TO NUM-ENTRIES(BestSort.Storrelser," "):
          ASSIGN wStorlekar =
                 IF wStorlekar = "" THEN
                     ENTRY(wIdx,BestSort.Storrelser," ")
                 ELSE
                     wStorlekar + " " + ENTRY(wIdx,BestSort.Storrelser," ").
 /* OLD 
          CREATE Fri-Str.
          ASSIGN Fri-Str.SoAnt    = INT(ENTRY(wIdx,wFordel," "))
                 Fri-Str.SoStorl  = ENTRY(wIdx,BestSort.Storrelser," ")
                 Fri-Str.SeqNr    = wIdx
                 wFriAnt          = wFriAnt + Fri-Str.SoAnt.
 OLD */
      END.
/* NY 19/9 */
      FOR EACH FriButik OF BestHode NO-LOCK:
          DO wIdx = 1 TO NUM-ENTRIES(wStorLekar," "):
              if wIdx > 50 then next.
              ASSIGN wFriAntal[wIdx] = wFriAntal[wIdx] + FriButik.FriAntal[wIdx]
                     wFriAnt         = wFriAnt         + FriButik.FriAntal[wIdx].
          END.
      END.
/* NY 19/9 SLUT */
    END.
    FOR EACH BestSort OF BestHode WHERE BestSort.Fri = NO NO-LOCK:
          RELEASE tLevSort.
          CREATE tLevSort.
          BUFFER-COPY BestSort TO tLevSort.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initStatus C-Win 
PROCEDURE initStatus :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iButik AS INTEGER    NO-UNDO.
    FIND Butiker WHERE Butiker.Butik = wCentralLager NO-LOCK NO-ERROR.
    ASSIGN cStatusStr = ",,,Sentrallager" + " " + Butiker.Butnamn.
    DO iCount = ch_Grid:FixedRows + 1 TO ch_Grid:Rows - 1:
        ASSIGN iButik = INT(ch_Grid:TextMatrix(iCount,0)).
        FIND Butiker WHERE Butiker.Butik = iButik NO-LOCK NO-ERROR.
        ASSIGN cStatusStr = cStatusStr + "," + IF AVAIL Butiker THEN Butiker.butnamn ELSE "".
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LagreBest C-Win 
PROCEDURE LagreBest PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VAR wFriFordeling AS CHAR NO-UNDO. /* antal / storlek */
  DEFINE VAR wStorlek      AS CHAR NO-UNDO. /* strl i find, går ej med ch_Grid */
  DEFINE VAR wButik        LIKE BestStr.Butik NO-UNDO.
  DEFINE VAR wRow          AS INTE NO-UNDO. 
  DEFINE VAR wCol          AS INTE NO-UNDO. 
  DEFINE VAR wLevert       AS INTE NO-UNDO. 
  DEFINE VAR wAvskrevet    AS LOGI NO-UNDO.
  DEFINE VAR wFullInlev    AS LOGI NO-UNDO.
  DEFINE VAR wRest         AS INTE NO-UNDO. 
  DEFINE VAR wCellValue1   AS INTE NO-UNDO. 
  DEFINE VAR wCellValue2   AS INTE NO-UNDO. 
  def var wBatchNr         as int  no-undo.
  def var wLoop            as int  no-undo.
  def var wTransNr         as int  no-undo.
  def var wSkjerm          as char no-undo.
  def var wDirekte         as log  no-undo.
  DEF VAR cUserid          AS CHAR NO-UNDO.
  DEF VAR h_PrisKo         AS HANDLE NO-UNDO.
  DEFINE VARIABLE lEtikettFinns AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE iTotInnLev   LIKE BestHode.TotInnLev   NO-UNDO.
  DEFINE VARIABLE iTotOverLev  LIKE BestHode.TotOverLev  NO-UNDO.
  DEFINE VARIABLE iTotMakulert LIKE BestHode.TotMakulert NO-UNDO.
  DEFINE VARIABLE lRegistreratButik AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE iBuntnr     AS INTEGER          NO-UNDO.
  DEFINE VARIABLE iLinjeNr    AS INTEGER  INIT 1  NO-UNDO.
  DEF VAR iCount       AS INTEGER NO-UNDO.
  DEF VAR dIndividNr   AS DECIMAL    NO-UNDO.
  DEF VAR iIndividBatchNr AS INTEGER    NO-UNDO.
  DEF VAR dArtikkelNr LIKE ArtBas.ArtikkelNr NO-UNDO.
  DEFINE VARIABLE cDummy AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cDirektOppd AS CHARACTER INIT "N"  NO-UNDO.

  EMPTY TEMP-TABLE TT_OvBuffer NO-ERROR.
  
  
  ASSIGN cKontrStorl = "".
  /* Skal følgeseddel ved innleveranse skrives ut. */

  IF NOT Registrerat() THEN DO:
      MESSAGE "Det finnes inget å lagre." VIEW-AS ALERT-BOX INFORMATION.
      ASSIGN SELF:SENSITIVE = NO.
      RETURN NO-APPLY.
  END.
  IF cPassordkrav = "1" THEN DO:
      RUN d-bekreftbruker.w ("Bekreft brukerid").
      IF RETURN-VALUE = "AVBRYT" THEN DO:
          MESSAGE "Lagring avbrutt"
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN NO-APPLY.
      END.
      ELSE
          ASSIGN cUserid = RETURN-VALUE.
  END.
  IF NOT BestHode.DirekteLev THEN DO:
      /* testa om det finns inleverans till andra butiker än CL */
      ASSIGN lRegistreratButik = RegistreratButik().
      IF lRegistreratButik THEN
        RUN d-velgovbunt.w (INPUT-OUTPUT iBuntnr,INPUT-OUTPUT cDummy).
      IF RETURN-VALUE = "AVBRYT" THEN DO:
          MESSAGE "Avbrudd kobling til overføringsordre."
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          ASSIGN lRegistreratButik = FALSE.
      END.
      IF lRegistreratButik AND ENTRY(1,cDummy,CHR(1)) = "J" THEN
          cDirektOppd = "J".
  END.
TRANSBLOKK:
DO TRANSACTION:                    
  IF BestHode.BestStat = 4 THEN DO:
    RUN bytbeststatus.p (wBestHodeRecid,"+",?).
    IF RETURN-VALUE = "Avbryt" THEN 
      UNDO TRANSBLOKK, leave TRANSBLOKK. /* RETURN NO-APPLY */.
  END.
  {sww.i}
  run batchlogg.w (program-name(1), 
                   "Innleveranse av ordre - BestNr: " + string(BestHode.BestNr),
                    output wBatchNr).
  if not available ArtBas then
    find ArtBas no-lock WHERE recid(ArtBas) = wArtBasRecid.       
  IF ArtBas.Inn_Dato = ? THEN DO:
      FIND CURRENT ArtBas EXCLUSIVE-LOCK.
      ASSIGN ArtBAs.Inn_Dato = TODAY.
      FIND CURRENT ArtBas NO-LOCK.
  END.
  /* Oppdaterer kalkyle for alle profiler som det er registrert kalkyle på. */
  PRISOPPDATERING:
  for each BestPris no-lock where
    BestPris.BestNr   = BestHode.BestNr and
    BestPris.BestStat = BestHode.BestStat:
    assign
     wSkjerm = string(BestPris.ValPris)      + ";" +
               string(BestPris.InnkjopsPris) + ";" +
               string(BestPris.Rab1Kr)       + ";" +
               string(BestPris.Rab1%)        + ";" +
               string(BestPris.Rab2Kr)       + ";" +  
               string(BestPris.Rab2%)        + ";" +
               string(BestPris.Frakt)        + ";" +
               string(BestPris.Frakt%)       + ";" + 
               string(BestPris.DivKostKr)    + ";" + 
               string(BestPris.DivKost%)     + ";" +
               string(BestPris.Rab3Kr)       + ";" +
               string(BestPris.Rab3%)        + ";" +
               string(BestPris.VareKost)     + ";" +
               string(BestPris.MvaKr)        + ";" +
               string(BestPris.Mva%)         + ";" + 
               string(BestPris.DBKr)         + ";" +
               string(BestPris.DB%)          + ";" +
               string(BestPris.Pris)         + ";" +
               string(BestPris.EuroPris)     + ";" +
               string(BestPris.EuroManuel)   + ";" + /* 20 */
               string(today)                 + ";" + /* 21 Aktiv fra */
               "0"                           + ";" + /* 22 */
               ""                            + ";" + /* 23 */
               "0"                           + ";" + /* 24 */
               ""                            + ";" + /* 25 */
               "0"                           + ";" + /* 26 */
               ""                            + ";" + /* 27 */
               "no".
    
    IF NOT VALID-HANDLE(h_PrisKo) THEN
        RUN prisko.p PERSISTENT SET h_PrisKo.
    ASSIGN wDirekte = TRUE.
    if wDirekte THEN DO:
        /* Oppdaterer ordinærkalkyle */
        if valid-handle(h_PrisKo) then
          run LagreArtPris in h_PrisKo
            (input wArtBasRecid,
             input BestPris.ProfilNr,
             input-output wSkjerm,
             input false,  /* wTilbud = false - Dvs ordinær kaflkyle.         */
             input true,   /* Direkte oppdatering av prisene som er kalkulert */
             INPUT 1,
             ?).
    END.
    ELSE DO: /* Oppdaterer ordinærkalkyle */
        if valid-handle(h_PrisKo) then
          run LagreArtPris in h_PrisKo
            (input wArtBasRecid,
             input BestPris.ProfilNr,
             input-output wSkjerm,
             input false,  /* wTilbud = false - Dvs ordinær kaflkyle.         */
             input false,  /* Direkte oppdatering av prisene som er kalkulert */
             INPUT 1,
             ?).  
    END.
  END. /* PRISOPPDATERING */
  IF ArtBas.IndividType > 0 THEN DO:
      FIND HuvGr OF ArtBas NO-LOCK.
      FIND LevBas OF ArtBas NO-LOCK.
      FIND Varemerke OF ArtBas NO-LOCK NO-ERROR.
      ASSIGN iIndividBatchNr = wBatchNr
             dArtikkelNr     = ArtBas.ArtikkelNr.
  END.
  Do with frame DEFAULT-FRAME:
    ASSIGN wFullInlev = FullInlev().
    CREATE BestHLev.
    ASSIGN BestHLev.BestNr       = BestHode.BestNr
           BestHLev.LeveringsNr  = NEXT-VALUE(LeveringsNr)
           BestHLev.LevertDato   = TODAY
           BestHLev.LevTidspunkt = time
           BestHLev.LevertAv     = IF cPassordkrav = "1" THEN cUserid 
                                            ELSE USERID("DICTDB")
           wBestHLevRec          = recid(BestHLev)
           wLeveringsNr          = BestHLev.LeveringsNr
           .
    DO wRow = ch_Grid:FixedRows TO ch_Grid:Rows - 1:
        IF ch_Grid:TextMatrix(wRow,0) = "" THEN
            LEAVE.
        ASSIGN wButik = IF wRow = ch_Grid:FixedRows THEN wCentralLager ELSE
                                                    INT(ch_Grid:TextMatrix(wRow,0)).                                                    
        /* Henter kalkylen */
        FIND Butiker WHERE Butiker.Butik    = wButik NO-LOCK.
        FIND BestPris WHERE BestPris.BestNr = BestHode.BestNr AND
          BestPris.BestStat = BestHode.BestStat AND
          BestPris.ProfilNr = Butiker.ProfilNr NO-LOCK no-error.
        if not available BestPris then
          do:
            FIND Butiker WHERE Butiker.Butik    = wCentrallager NO-LOCK.
            FIND BestPris WHERE BestPris.BestNr = BestHode.BestNr AND
                 BestPris.BestStat = BestHode.BestStat AND
                 BestPris.ProfilNr = Butiker.ProfilNr NO-LOCK.
          end.                                            
                                                    
        DO wCol = 2 TO ch_Grid:Cols - 1:
            IF NUM-ENTRIES(ch_Grid:TextMatrix(wRow,wCol),"/") <> 2 OR
               TRIM(ENTRY(2,ch_Grid:TextMatrix(wRow,wCol),"/")) = "" THEN
                NEXT.

            ASSIGN wStorlek = ch_Grid:TextMatrix(0,wCol)
                   wLevert = IF TRIM(ENTRY(2,ch_Grid:TextMatrix(wRow,wCol),"/")) = wAvskrevetCell THEN /* MAK */
                          0 ELSE INT(ENTRY(2,ch_Grid:TextMatrix(wRow,wCol),"/"))
                   wAvskrevet = wLevert = 0
                   wRest = INT(ENTRY(1,ch_Grid:TextMatrix(wRow,wCol),"/"))
                   iTotInnLev   = iTotInnLev + wLevert
                   iTotMakulert = iTotMakulert + IF wAvskrevet = TRUE THEN wRest ELSE 0
                   iTotOverLev  = iTotOverLev  + IF wAvskrevet = TRUE THEN 0 ELSE
                                                 IF wRest     <= 0    THEN wLevert ELSE
                                                 IF wLevert > wRest   THEN wLevert - wRest ELSE 0.

            CREATE BestLevert.
            ASSIGN BestLevert.BestNr      = BestHLev.BestNr
                   BestLevert.Butik       = wButik
                   BestLevert.Storl       = wStorlek
                   BestLevert.LeveringsNr = BestHLev.LeveringsNr
                   BestLevert.Levert      = wLevert
                   BestLevert.Rest        = wRest - wLevert
                   BestLevert.Avskrevet   = wAvskrevet
                   BestLevert.LevertAv    = IF cPassordkrav = "1" THEN cUserid 
                                            ELSE USERID("DICTDB")
                   BestLevert.LevertDato  = TODAY.
         IF NOT CAN-DO(cKontrStorl,TRIM(wStorlek)) AND wAvskrevet = FALSE THEN
             ASSIGN cKontrStorl = cKontrStorl + (IF cKontrStorl <> "" THEN "," ELSE "") + TRIM(wStorlek).

         ASSIGN wStorlek = FiksStorl(wStorlek).
         IF wButik <> wCentralLager AND lRegistreratButik AND wLevert > 0 THEN DO:
           CREATE TT_OvBuffer.
           ASSIGN TT_OvBuffer.BuntNr      = 999 /* dummy */
                  TT_OvBuffer.LinjeNr     = iLinjeNr
                  TT_OvBuffer.ButikkNrFra = wCentralLager
                  TT_OvBuffer.ButikkNrTil = wButik        
                  TT_OvBuffer.ArtikkelNr  = ArtBas.ArtikkelNr
                  TT_OvBuffer.Vg          = ArtBas.vg   
                  TT_OvBuffer.LopNr       = ArtBas.LopNr
                  TT_OvBuffer.Antall      = wLevert
                  TT_OvBuffer.Merknad     = "Best " + STRING(BestHode.BestNr)
                  TT_OvBuffer.Storl       = wStorlek
                  TT_OvBuffer.TilStorl    = wStorlek
                  TT_OvBuffer.Varekost    = if available BestPris
                                                then BestPris.VareKost
                                                else 0
                  iLinjeNr                = iLinjeNr + 1.
           RELEASE TT_OvBuffer.
          END.
                                      
          /* Transaksjonsnummer for butikken. */
          find last TransLogg no-lock where
            TransLogg.Butik = (IF (NOT BestHode.DirekteLev AND lRegistreratButik) 
                                                  THEN wCentralLager 
                                                  ELSE wButik) use-index TransLogg no-error.
          if available TransLogg then
            wTransNr = TransLogg.TransNr + 1.
          else 
            wTransNr = 1.
            /* Oppretter transaksjon */
          IF wLevert > 0 THEN DO iCount = 1 TO (IF ArtBas.IndividType > 0 THEN wLevert ELSE 1):
            LAG_TRANS:
            DO:
              /* Sjekker at transnr er ledig */
              if can-find(TransLogg where
                          TransLogg.Butik   = (IF (NOT BestHode.DirekteLev AND lRegistreratButik) THEN wCentralLager ELSE wButik) and
                          TransLogg.TransNr = wTransNr) then
                NESTE_NR:
                do while true:
                  wTransNr = wTransNr + 1.
                  if can-find(TransLogg where
                              TransLogg.Butik   = (IF (NOT BestHode.DirekteLev AND lRegistreratButik) THEN wCentralLager ELSE wButik) and
                              TransLogg.TransNr = wTransNr) then
                    next NESTE_NR.
                  else
                    leave NESTE_NR.
                end. /* NESTE_NR */
              IF ArtBas.IndividType > 0 THEN DO:
                  FIND StrKonv WHERE StrKonv.Storl = wStorlek NO-LOCK NO-ERROR.
                  IF AVAIL StrKonv THEN
                      RUN SkapaIndivid IN THIS-PROCEDURE (IF (NOT BestHode.DirekteLev AND lRegistreratButik)
                                                THEN wCentralLager ELSE wButik,INPUT wBatchNr,INPUT StrKonv.Strkode,INPUT StrKonv.Storl, OUTPUT dIndividNr).
              END.
              create TransLogg.
              assign TransLogg.Butik        = IF (NOT BestHode.DirekteLev AND lRegistreratButik) 
                                                THEN wCentralLager 
                                                ELSE wButik
                     TransLogg.TransNr      = wTransNr
                     TransLogg.SeqNr        = 1.
              assign TransLogg.BatchNr      = wBatchNr
                     TransLogg.KundNr       = 0
                     TransLogg.TTId         = 5 /* Varekjøp */
                     TransLogg.TBId         = 1
                     TransLogg.ArtikkelNr   = BestHode.ArtikkelNr
                     TransLogg.LevNr        = BestHode.LevNr
                     TransLogg.BongId       = 0
                     TransLogg.BongLinjeNr  = 0
                     TransLogg.KassaNr      = 0
                     TransLogg.Vg           = ArtBas.Vg
                     TransLogg.LopNr        = ArtBas.LopNr
                     TransLogg.Antall       = IF ArtBas.IndividType > 0 THEN 1 ELSE wLevert
                     TransLogg.Pris         = (if available BestPris
                                                then BestPris.VareKost
                                                else 0)
                     TransLogg.RabKr        = 0
                     TransLogg.Mva          = 0
                     TransLogg.Plukket      = true /* Skal ikke ut på plukkliste */
                     TransLogg.Dato         = today
                     TransLogg.Tid          = time
                     TransLogg.BestNr       = BestHode.BestNr
                     TransLogg.Storl        = wStorlek
                     TransLogg.Indivi       = dIndividNr
                     TransLogg.Postert      = false.                                             
            end. /* LAG_TRANS */
            /* Oppretter Etikett */
            IF BUTTON-Etiketter:SENSITIVE THEN ETIKETT:
            do:
              find Etikett exclusive-lock where
                Etikett.LevInNr = BestHode.BestNr AND
                Etikett.Butik = (IF cBrukButikEtikett = "Ja" THEN wButik ELSE IF NOT BestHode.DirekteLev AND lRegistreratButik
                                                  THEN wCentralLager ELSE wButik) and
                Etikett.Vg    = ArtBas.Vg and
                Etikett.LopNr = ArtBas.LopNr and
                Etikett.Storl = FiksStorl(wStorlek) no-error.
              if not available Etikett then
                do:
                  create Etikett.
                  assign
                    Etikett.Butik = IF cBrukButikEtikett = "Ja" THEN wButik ELSE IF NOT BestHode.DirekteLev AND lRegistreratButik
                                       THEN wCentralLager ELSE wButik
                    Etikett.Vg    = ArtBas.Vg
                    Etikett.LopNr = ArtBas.LopNr.
                  assign
                    Etikett.Storl = FiksStorl(wStorlek).
                end.
              assign
                Etikett.Pris    = BestPris.Pris
                Etikett.Texten  = ArtBas.BongTekst
                Etikett.Antal   = IF ArtBas.Etikett = 1 THEN
                                       wLevert ELSE 1
/*                 Etikett.Antal   = IF ArtBas.Etikett = 1 THEN        */
/*                                      Etikett.Antal + wLevert ELSE 1 */
                Etikett.LevInNr = BestHode.BestNr
                Etikett.Rad     = 1
                lEtikettFinns   = TRUE.
            
            end. /* ETIKETT */
          END.
                   
            ASSIGN ch_Grid:TextMatrix(wRow,wCol) = 
                IF wAvskrevet THEN
                    wAvskrevetCell + " " + STRING(BestLevert.Rest) + " "
                ELSE IF BestLevert.Rest <> 0 THEN
                        FILL(" ", 2 * (4 - LENGTH(STRING(BestLevert.Rest)))) +
                        (IF BestLevert.Rest < 0 THEN " " ELSE "") +
                          STRING(BestLevert.Rest) + "/        "
                ELSE "        /        ".
        END.                       
    END.
    FIND CURRENT BestHode EXCLUSIVE NO-ERROR.
    ASSIGN
        BestHode.TotInnLev   = BestHode.TotInnLev   + iTotInnLev
        BestHode.TotMakulert = BestHode.TotMakulert + iTotMakulert
        BestHode.TotOverLev  = BestHode.TotOverLev  + iTotOverLev.
    FIND CURRENT BestHode NO-LOCK NO-ERROR.
    IF wFullInlev = TRUE THEN DO:
        RUN bytbeststatus.p (wBestHodeRecid,"+",?).
        RUN VisFullev.
        ASSIGN FILL-IN-Kvar:SCREEN-VALUE = "0"
               FILL-IN-InnLev:SCREEN-VALUE = "0"
               FILL-IN-Rest:SCREEN-VALUE = "0"
               BUTTON-Fullinlev:SENSITIVE = NO.
    END.
    ELSE DO:
      DO wRow = ch_Grid:FixedRows TO ch_Grid:Rows - 1:
        IF INT(ENTRY(1,ch_Grid:TextMatrix(wRow,1),"/")) = 0 THEN
            NEXT.
        ASSIGN wRest = 0.
        DO wCol = ch_Grid:FixedCols TO ch_Grid:Cols - 1:
            /* Hopper over celler med teksten "Mak N" - Hvor N = antall makulert.*/
            if trim(ch_Grid:TextMatrix(wRow,wCol)) begins wAvskrevetCell then
              next.
              
            ASSIGN wRest = wRest + IF INT(ENTRY(1,ch_Grid:TextMatrix(wRow,wCol),"/")) > 0 THEN
                             INT(ENTRY(1,ch_Grid:TextMatrix(wRow,wCol),"/")) ELSE 0.
        END.
        ASSIGN ch_Grid:TextMatrix(wRow,1) = LagString(wRest) + "/" + LagString(0).
      END.
      RUN InitFillIns.
    END.
    DO wCol = 1 TO ch_Grid:Cols - 1:
        ASSIGN ch_Grid:TextMatrix(3,wCol) = "0 ".
    END.
    /* !!! DENNA SKALL LIGGA SIST */
    FIND CURRENT BestHode NO-LOCK.
    /* !!! */
    RUN SetWindowTitle.
    RUN ButtonEnaDis.
    ASSIGN SELF:SENSITIVE = NO.
    
  end.
  IF CAN-FIND(FIRST TT_OvBuffer) THEN
      RUN LagraOvBuffer.p (INPUT-OUTPUT iBuntNr,ArtBas.ArtikkelNr,cDirektOppd + CHR(1) + "Best " + STRING(BestHode.BestNr),wEDB-System,wTabell,iOpphav).
  run batchstatus.p (wBatchNr, 2).
  {swn.i}
  RUN GenererEAN(3). /* streckkode utifrån BestLevert */
end. /* TRANSBLOKK TRANSACTION */
  RUN Utskrifter(lEtikettFinns,iIndividBatchNr,dArtikkelNr,IF NOT BestHode.DirekteLev AND lRegistreratButik
                                                  THEN wCentralLager ELSE ?).
  IF iIndividBatchNr > 0 THEN
     RUN gIndividSerie.w (INPUT iIndividBatchNr).
  if available Etikett then
    release Etikett.
  if available ArtBas then
    release ArtBas.
  if available TransLogg then
    release TransLogg.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppdStrekkoder C-Win 
PROCEDURE OppdStrekkoder :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cTmpKontrStorl AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cTmpListe AS CHARACTER  NO-UNDO.
    IF NOT AVAIL strtype THEN DO:
        FIND strtype OF artbas NO-LOCK NO-ERROR.
        IF NOT AVAIL strtype THEN
            RETURN.
    END.
    cTmpKontrStorl = cKontrStorl.
    cTmpListe = "".
    /* tar bort ev blanka tkn */
    cTmpListe = REPLACE(StrType.AlfaFordeling," ","").
    FOR EACH BestStr OF BestHode NO-LOCK BREAK BY BestStr.Storl.
        IF FIRST-OF(BestStr.storl) THEN DO:
            IF CAN-DO(cTmpListe,TRIM(BestStr.Storl)) THEN
                cKontrStorl = cKontrStorl + (IF cKontrStorl <> "" THEN "," ELSE "") + TRIM(BestStr.Storl).
        END.
    END.
    RUN StrekKode.w (THIS-PROCEDURE).
    cKontrStorl = cTmpKontrStorl.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PopUpChoose C-Win 
PROCEDURE PopUpChoose :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER wMenyValg AS CHAR NO-UNDO.
    DEF VAR wRow      AS INTE NO-UNDO.
    DEF VAR wCol      AS INTE NO-UNDO.
    DEF VAR wTotTmp   AS INTE NO-UNDO.
    DEF VAR wAntal    AS INTE NO-UNDO.
    DEF VAR wInpAntal AS INTE NO-UNDO. /* Message update */
    DEF VAR wRadSum   AS INTE NO-UNDO.
    DEF VAR wRadE2    AS INTE NO-UNDO.
    DEF VAR wInSum   AS INTE NO-UNDO.
    DEF VAR wEntry1   AS INTE NO-UNDO.
    DEF VAR wEntry2   AS INTE NO-UNDO.
    DEFINE VAR             wMatrisIdx AS INTE NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:
    CASE wMenyValg:
        WHEN "fullinlev" OR WHEN "m_Full_inlev_butiker" OR WHEN "m_Full_inlev_butik" THEN DO:
            DO wRow = wFstRow TO wLstRow:
                /* Hopper over celler med teksten "Mak N" - Hvor N = antall makulert.*/
                if trim(ch_Grid:TextMatrix(wRow,1)) begins wAvskrevetCell then
                  next.

                IF INT(ENTRY(1,ch_Grid:TextMatrix(wRow,1),"/")) = 0
                    THEN NEXT.
                ASSIGN wInSum = 0
                       wRadE2 = INT(ENTRY(2,ch_Grid:TextMatrix(wRow,1),"/")).
                DO wCol = 2 TO ch_Grid:Cols - 1:
                    IF NUM-ENTRIES(ch_Grid:TextMatrix(wRow,wCol),"/") <> 2 OR
                       TRIM(ENTRY(2,ch_Grid:TextMatrix(wRow,wCol),"/")) = wAvskrevetCell OR
                            INT(ENTRY(1,ch_Grid:TextMatrix(wRow,wCol),"/")) < 0 OR
                            INT(ENTRY(1,ch_Grid:TextMatrix(wRow,wCol),"/")) <=
                            INT(ENTRY(2,ch_Grid:TextMatrix(wRow,wCol),"/")) THEN NEXT.
                    ASSIGN wEntry1 = INT(ENTRY(1,ch_Grid:TextMatrix(wRow,wCol),"/"))
                           wEntry2 = INT(ENTRY(2,ch_Grid:TextMatrix(wRow,wCol),"/"))
                           wInSum = wInSum + (wEntry1 - wEntry2)
                           ch_Grid:TextMatrix(wRow,wCol) =
                                          ENTRY(1,ch_Grid:TextMatrix(wRow,wCol),"/") + "/" +
                                          ENTRY(1,ch_Grid:TextMatrix(wRow,wCol),"/").
                    RUN FixInnlev(wCol).
                END.
                ASSIGN ch_Grid:TextMatrix(wRow,1) = ENTRY(1,ch_Grid:TextMatrix(wRow,1),"/") + "/" +
                                           LagString(INT(ENTRY(2,ch_Grid:TextMatrix(wRow,1),"/")) + wInSum)
                       FILL-IN-InnLev:SCREEN-VALUE = STRING(INT(FILL-IN-InnLev:SCREEN-VALUE) + wInSum)
                       FILL-IN-Rest:SCREEN-VALUE = 
                           STRING(   INT(FILL-IN-Rest:SCREEN-VALUE) -
                                    (INT(ENTRY(2,ch_Grid:TextMatrix(wRow,1),"/")) - wRadE2) ).

            END.
        END.
        WHEN "m_Avskriv_storlek" THEN DO:
            DO wRow = wFstRow TO wLstRow:
                IF INT(ENTRY(1,ch_Grid:TextMatrix(wRow,1),"/")) = 0
                    THEN NEXT.
                DO wCol = wFstCol TO wLstCol:
                    IF NUM-ENTRIES(ch_Grid:TextMatrix(wRow,wCol),"/") <> 2 OR
                       TRIM(ENTRY(1,ch_Grid:TextMatrix(wRow,wCol),"/")) = "" OR
                       TRIM(ENTRY(2,ch_Grid:TextMatrix(wRow,wCol),"/")) = wAvskrevetCell OR
                       INT(ENTRY(2,ch_Grid:TextMatrix(wRow,wCol),"/")) > 0 THEN
                           NEXT.
                    ASSIGN ch_Grid:TextMatrix(wRow,wCol) = 
                               ENTRY(1,ch_Grid:TextMatrix(wRow,wCol),"/") + "/" + wAvskrevetCell
                           FILL-IN-Rest:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 
                                   STRING(INT(FILL-IN-Rest:SCREEN-VALUE IN FRAME {&FRAME-NAME}) - 
                                   INT(ENTRY(1,ch_Grid:TextMatrix(wRow,wCol),"/"))).
                    RUN FixInnlev(wCol).
                END.
            END.
        END.
        WHEN "m_clear_butiker" OR WHEN "m_clear_butik" THEN DO:
            DO wRow = wFstRow TO wLstRow:
                ASSIGN wInSum = 0.
                DO wCol = 2 TO ch_Grid:Cols - 1:
                    IF NUM-ENTRIES(ch_Grid:TextMatrix(wRow,wCol),"/") <> 2 OR
                            TRIM(ENTRY(2,ch_Grid:TextMatrix(wRow,wCol),"/")) = "" THEN NEXT.
                    ASSIGN wInSum = wInSum + 
                                   IF INT(ENTRY(1,ch_Grid:TextMatrix(wRow,wCol),"/")) > 0 THEN
                                       INT(ENTRY(1,ch_Grid:TextMatrix(wRow,wCol),"/"))
                                   ELSE 0
                           ch_Grid:TextMatrix(wRow,wCol) =
                                          ENTRY(1,ch_Grid:TextMatrix(wRow,wCol),"/") + "/        ".

                    RUN FixInnlev(wCol).
                END.
                ASSIGN FILL-IN-InnLev:SCREEN-VALUE = STRING(INT(FILL-IN-InnLev:SCREEN-VALUE) - 
                                      INT(ENTRY(2,ch_Grid:TextMatrix(wRow,1),"/")))
                       FILL-IN-Rest:SCREEN-VALUE = STRING(INT(FILL-IN-Rest:SCREEN-VALUE) + wInSum)
                       ch_Grid:TextMatrix(wRow,1) = ENTRY(1,ch_Grid:TextMatrix(wRow,1),"/") + "/" +
                                           LagString(0).
            END.
        END.
    END CASE.
    ASSIGN BUTTON-Lagra:SENSITIVE = YES.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Priskalk C-Win 
PROCEDURE Priskalk :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER wModus AS CHAR NO-UNDO.
   IF wModus = "INIT" THEN DO:
       FIND Butiker WHERE Butiker.Butik = wCentrallager NO-LOCK.
       FIND BestPris WHERE BestPris.BestNr = BestHode.BestNr AND
                       BestPris.BestStat = BestHode.BestStat AND
                       BestPris.ProfilNr = Butiker.ProfilNr NO-LOCK.
    END.
    ELSE
        ASSIGN BestHode.TotInnKjVerdi = BestHode.TotAntPar * BestPris.Varekost
               BestHode.TotDbKr =       BestHode.TotAntPar * BestPris.DbKr
               BestHode.TotSalgsVerdi = BestHode.TotAntPar * BestPris.Pris.
               

    DO WITH FRAME {&FRAME-NAME}:
        DISPLAY BestHode.TotInnKjVerdi
                BestHode.TotDbKr
                BestHode.TotSalgsVerdi.
        if available BestPris then
            DISPLAY BestPris.Varekost
                    BestPris.DbKr
                    BestPris.Pris.
        /*
        IF wBestHodeRecid = ? OR wModus = "INIT" AND AVAIL BestPris THEN
            DISPLAY BestPris.Varekost
                    BestPris.DbKr
                    BestPris.Pris.
        */
    END.
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
         CURRENT-WINDOW:TITLE = "Varemottak artikkel: " + 
           STRING(ArtBas.ArtikkelNr) + " " + " Best.nr: " +
           (IF NOT AVAIL BestHode THEN  "NY" ELSE
           STRING(BestHode.BestNr) + " " + " Status: " + ENTRY(BestHode.BestStat,wStatus)) +
           (IF BestHode.SendtDato <> ? THEN " (Sendt " + STRING(BestHode.SendtDato) + " " +
                STRING(BestHode.SendtTid,"HH:MM") + " " +
                BestHode.SendtAv + ")" ELSE "").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapaIndivid C-Win 
PROCEDURE SkapaIndivid :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER iButikNr  AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER iBatchNr  AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER iStrKode   LIKE StrKonv.StrKode   NO-UNDO.
  DEFINE INPUT  PARAMETER cStorl     LIKE StrKonv.Storl     NO-UNDO.
  DEFINE OUTPUT PARAMETER dIndividNr LIKE Individ.individnr NO-UNDO.
  DEFINE        VARIABLE  dSeqNr      AS DECIMAL            NO-UNDO.
  FIND LAST Individ WHERE Individ.butnr = iButikNr USE-INDEX SeqNr NO-LOCK NO-ERROR.
  ASSIGN dSeqnr = IF NOT AVAIL Individ THEN 1 ELSE Individ.SeqNr + 1.
  CREATE Individ.
  REPEAT:
      ASSIGN dIndividNr         = DECI(STRING(iButikNr) + STRING(dSeqnr))
             Individ.butnr      = iButikNr
             Individ.SeqNr      = dSeqNr
             Individ.ArtikkelNr = ArtBas.ArtikkelNr
             Individ.StrKode    = iStrKode
             Individ.individnr  = dIndividNr NO-ERROR.
      IF NOT ERROR-STATUS:ERROR THEN
          LEAVE.
      ASSIGN dSeqNr = dSeqNr + 1.
  END.
  ASSIGN Individ.AvdelingNr    = HuvGr.AvdelingNr
         Individ.Beskr         = ArtBas.Beskr
         Individ.Hg            = ArtBas.Hg
         Individ.IndividType   = ArtBas.IndividType
         Individ.LevNamn       = LevBas.Levnamn
         Individ.levnr         = ArtBas.LevNr
         Individ.NyVare        = TRUE
         Individ.Storl         = cStorl
         Individ.StrKode       = iStrKode
         Individ.Vg            = ArtBas.Vg
         Individ.VmBeskrivelse = IF AVAIL Varemerke THEN Varemerke.Beskrivelse ELSE ""
         Individ.VMId          = ArtBas.VMId
         Individ.BatchNr       = iBatchNr.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkrivEtiketter C-Win 
PROCEDURE SkrivEtiketter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 def var wSeqNr as int no-undo.
 def var wCl    as int no-undo.
 DEF VAR iLeveringsNr LIKE BestHLev.LeveringsNr          NO-UNDO.
 DEF VAR cPrinterValg AS CHARACTER                       NO-UNDO.
 
 {syspara.i 5 1 1 wCl INT}
 
 if not available BestHode then 
   return.
 IF NOT CAN-FIND(FIRST BestHLev OF BestHode) THEN DO:
     MESSAGE "Ingen innlevering funnet."
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
     RETURN.
 END.
 RUN d-velginnlev.w (INPUT BestHode.BestNr,OUTPUT iLeveringsNr,OUTPUT cPrinterValg).
 IF NOT iLeveringsNr > 0 THEN
     RETURN.
 if not available butiker then
   find Butiker no-lock where 
     Butiker.Butik = wCl no-error.
 if not available ArtBas then
   find ArtBas no-lock where
     ArtBas.ArtikkelNr = BestHode.ArtikkelNr no-error.
 find BestPris no-lock where
   BestPris.BestNr   = BestHode.BestNr and
   BestPris.BestSTat = BestHode.BestStat and
   BestPris.Profil   = Butiker.Profil no-error.
 if not available BestPris then
   do:
     message "Du må først legge opp en kalkyle på artikkelen." 
       view-as alert-box message title "Melding".
     return no-apply.
   end.
 FIND BestHLev OF BestHode WHERE BestHLev.LeveringsNr = iLeveringsNr NO-LOCK NO-ERROR.
 FOR EACH BestLevert OF BestHLev WHERE BestLevert.Levert > 0 NO-LOCK:
         
         
    find Etikett exclusive-lock where
         Etikett.LevInNr = BestHode.BestNr AND
         Etikett.Butik = BestLevert.Butik  and
         Etikett.Vg    = ArtBas.Vg and
         Etikett.LopNr = ArtBas.LopNr and
         Etikett.Storl = FiksStorl(BestLevert.Storl) no-error.
    IF NOT AVAILABLE Etikett THEN DO:
        CREATE Etikett.
        ASSIGN Etikett.Butik = BestLevert.Butik
               Etikett.Vg    = ArtBas.Vg
               Etikett.LopNr = ArtBas.LopNr.
        ASSIGN Etikett.Storl = FiksStorl(BestLevert.Storl).
    END.
    assign Etikett.Pris    = BestPris.Pris
           Etikett.Texten  = ArtBas.BongTekst
           Etikett.Antal   = BestLevert.Levert 
           Etikett.LevInNr = BestHode.BestNr
           Etikett.Rad     = 1.            
 END.

 if not can-find(first Etikett where Etikett.LevInNr = BestHode.BestNr) then
   do:
     message "Ingen etiketter å skrive ut." 
       view-as alert-box message title "Melding".
     return no-apply.
   end.
    run Skrivetiketter2 (input cPrinterValg).
    RUN SlettEtiketter.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkrivEtiketter2 C-Win 
PROCEDURE SkrivEtiketter2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER cPrinterValg AS CHARACTER  NO-UNDO.
  DEFINE        VARIABLE  lFlereBut    AS LOGICAL    NO-UNDO.
  DEFINE        VARIABLE  iButik LIKE Butiker.Butik  NO-UNDO.
  DEFINE        VARIABLE  iSeq   AS INTEGER    NO-UNDO.
  DEFINE        VARIABLE  cInfoRad1 AS CHARACTER  NO-UNDO.
  DEFINE        VARIABLE  cInfoRad2 AS CHARACTER  NO-UNDO.
  DEFINE        VARIABLE  cInfoRad3 AS CHARACTER  NO-UNDO.
  DEFINE        VARIABLE  cInfoRad4 AS CHARACTER  NO-UNDO.
  DEFINE        VARIABLE  iVg       LIKE Artbas.Vg NO-UNDO.
  DEFINE        VARIABLE  iLopnr    LIKE ArtBas.Vg NO-UNDO.
  
  FOR EACH Etikett NO-LOCK WHERE Etikett.LevInNr = BestHode.BestNr BREAK BY Etikett.butik:
      IF FIRST(Etikett.butik) THEN
          ASSIGN iButik = Etikett.butik
                 iVg    = Etikett.Vg
                 iLopNr = Etikett.Lopnr.
      IF LAST(etikett.butik) AND Etikett.butik <> iButik THEN
          ASSIGN lFlereBut = TRUE.
  END.
  FIND ArtBas WHERE ArtBas.Vg = iVg AND ArtBas.Lopnr = iLopnr NO-LOCK NO-ERROR.
  IF INT(ENTRY(2,cPrinterValg,CHR(1))) > 1 THEN DO:
      CREATE EtikettLogg.
      ASSIGN EtikettLogg.Butik = 0
             EtikettLogg.SeqNr = 0
             EtikettLogg.Storl  = "STARTETIKETT"
             EtikettLogg.Ant = INT(ENTRY(2,cPrinterValg,CHR(1))).
  END.
  ASSIGN cInfoRad1 = "Innlev. Best: " + STRING(BestHode.BestNr).
  FOR EACH Etikett NO-LOCK WHERE Etikett.LevInNr = BestHode.BestNr BREAK BY Etikett.butik BY Etikett.Storl:
      IF FIRST-OF(Etikett.butik) AND lFlereBut THEN DO:
          FIND butiker WHERE butiker.butik = Etikett.butik NO-LOCK NO-ERROR.
          ASSIGN cInfoRad3 = IF AVAIL Butiker THEN Butiker.butnamn ELSE ""
                 cInfoRad4 = "SLUTT"
                 iSeq      = iSeq + 1.
          create EtikettLogg.
          assign
            EtikettLogg.Butik     = IF cBrukButikEtikett = "Ja" THEN Etikett.butik ELSE iSeq /* Det skal skrives ut i seqnr ordning. */
            EtikettLogg.Vg        = 0   
            EtikettLogg.LopNr     = 0
            EtikettLogg.Ant       = 0
            EtikettLogg.Storl     = "INFO"
            EtikettLogg.Bongtekst = cInfoRad1 + CHR(1) + cInfoRad2 + CHR(1) + cInfoRad3 + CHR(1) + cInfoRad4
            EtikettLogg.Pris      = 0
            EtikettLogg.Pris2     = 0
            EtikettLogg.SeqNr     = iSeq.
      END.
      FIND StrKonv WHERE StrKonv.Storl = Etikett.Storl NO-LOCK NO-ERROR.
      IF AVAIL StrKonv THEN DO:
          FIND FIRST StrekKode OF ArtBas WHERE StrekKode.KodeType = 1 AND 
                                               StrekKode.StrKode = StrKonv.StrKode AND
                                               NOT StrekKode.Kode BEGINS "02" NO-LOCK NO-ERROR.
          IF NOT AVAIL StrekKode THEN
              FIND FIRST StrekKode OF ArtBas WHERE StrekKode.KodeType = 1 AND 
                                                   StrekKode.StrKode = StrKonv.StrKode NO-LOCK NO-ERROR.
      END.
      IF AVAIL StrekKode THEN DO:
          ASSIGN iSeq = iSeq + 1.
          create EtikettLogg.
          ASSIGN EtikettLogg.Butik     = IF cBrukButikEtikett = "Ja" THEN Etikett.butik ELSE iSeq
                 EtikettLogg.Vg        = Etikett.Vg   
                 EtikettLogg.LopNr     = Etikett.LopNr
                 EtikettLogg.Ant       = Etikett.Antal
                 EtikettLogg.Storl     = StrekKode.Kode
                 EtikettLogg.Bongtekst = Etikett.Texten
                 EtikettLogg.Pris      = Etikett.Pris
                 EtikettLogg.SeqNr     = iSeq.
      END.
      IF LAST-OF(Etikett.butik) AND lFlereBut THEN DO:
          ASSIGN cInfoRad4 = "START"
                 iSeq      = iSeq + 1.
          create EtikettLogg.
          assign
            EtikettLogg.Butik     = IF cBrukButikEtikett = "Ja" THEN Etikett.butik ELSE iSeq /* Det skal skrives ut i seqnr ordning. */
            EtikettLogg.Vg        = 0   
            EtikettLogg.LopNr     = 0
            EtikettLogg.Ant       = 0
            EtikettLogg.Storl     = "INFO"
            EtikettLogg.Bongtekst = cInfoRad1 + CHR(1) + cInfoRad2 + CHR(1) + cInfoRad3 + CHR(1) + cInfoRad4
            EtikettLogg.Pris      = 0
            EtikettLogg.Pris2     = 0
            EtikettLogg.SeqNr     = iSeq.
      END.
  END.
  RUN x-etikettsend.w (INT(ENTRY(1,cPrinterValg,CHR(1)))).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkrivIndividEti C-Win 
PROCEDURE SkrivIndividEti :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER iBatchNr      AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER iArtikkelNr AS DECIMAL    NO-UNDO.
  DEFINE INPUT  PARAMETER cPrinterValg AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER iEtikettBut  AS INTEGER    NO-UNDO.
  DEFINE        VARIABLE  lFlereBut    AS LOGICAL    NO-UNDO.
  DEFINE        VARIABLE  iButik LIKE Butiker.Butik  NO-UNDO.
  DEFINE        VARIABLE  iSeq   AS INTEGER    NO-UNDO.
  DEFINE        VARIABLE  cInfoRad1 AS CHARACTER  NO-UNDO.
  DEFINE        VARIABLE  cInfoRad2 AS CHARACTER  NO-UNDO.
  DEFINE        VARIABLE  cInfoRad3 AS CHARACTER  NO-UNDO.
  DEFINE        VARIABLE  cInfoRad4 AS CHARACTER  NO-UNDO.
  DEFINE        VARIABLE  iVg       LIKE Artbas.Vg NO-UNDO.
  DEFINE        VARIABLE  iLopnr    LIKE ArtBas.Vg NO-UNDO.
  OUTPUT CLOSE.
  FOR EACH Individ NO-LOCK WHERE Individ.BatchNr = iBatchNr BREAK BY Individ.butnr:
      IF FIRST(Individ.butnr) THEN
          ASSIGN iButik = Individ.butnr.
      IF LAST(Individ.butnr) AND Individ.butnr <> iButik THEN
          ASSIGN lFlereBut = TRUE.
  END.
  FIND ArtBas WHERE ArtBas.ArtikkelNr = iArtikkelNr NO-LOCK NO-ERROR.
  IF INT(ENTRY(2,cPrinterValg,CHR(1))) > 1 THEN DO:
      CREATE EtikettLogg.
      ASSIGN EtikettLogg.Butik = 0
             EtikettLogg.SeqNr = 0
             EtikettLogg.Storl  = "STARTETIKETT"
             EtikettLogg.Ant = INT(ENTRY(2,cPrinterValg,CHR(1))).
  END.
  ASSIGN cInfoRad1 = "Innlev. Best: " + STRING(BestHode.BestNr).
  FOR EACH Individ NO-LOCK WHERE Individ.BatchNr = iBatchNr BREAK BY Individ.butnr:
      IF FIRST-OF(Individ.butnr) AND lFlereBut THEN DO:
          FIND butiker WHERE butiker.butik = Individ.butnr NO-LOCK NO-ERROR.
          ASSIGN cInfoRad3 = IF AVAIL Butiker THEN Butiker.butnamn ELSE ""
                 cInfoRad4 = "SLUTT"
                 iSeq      = iSeq + 1.
          create EtikettLogg.
          assign
            EtikettLogg.Butik     = IF cBrukButikEtikett = "Ja" THEN Etikett.butik ELSE iSeq /* Det skal skrives ut i seqnr ordning. */
            EtikettLogg.Vg        = 0   
            EtikettLogg.LopNr     = 0
            EtikettLogg.Ant       = 0
            EtikettLogg.Storl     = "INFO"
            EtikettLogg.Bongtekst = cInfoRad1 + CHR(1) + cInfoRad2 + CHR(1) + cInfoRad3 + CHR(1) + cInfoRad4
            EtikettLogg.Pris      = 0
            EtikettLogg.Pris2     = 0
            EtikettLogg.SeqNr     = iSeq.
      END.
      FIND FIRST StrekKode OF ArtBas WHERE StrekKode.KodeType = 1 AND 
                                           StrekKode.StrKode = StrKonv.StrKode AND
                                           NOT StrekKode.Kode BEGINS "02" NO-LOCK NO-ERROR.
      IF NOT AVAIL StrekKode THEN
          FIND FIRST StrekKode OF ArtBas WHERE StrekKode.KodeType = 1 AND 
                                               StrekKode.StrKode = StrKonv.StrKode NO-LOCK NO-ERROR.
      IF NOT AVAIL StrekKode THEN
          NEXT.
      IF AVAIL StrekKode THEN DO:
          FIND FIRST Etikett WHERE Etikett.LevInNr = BestHode.BestNr AND
                                   Etikett.Butik   = IF iEtikettBut = ? THEN Individ.Butnr ELSE iEtikettBut NO-LOCK NO-ERROR.
          ASSIGN iSeq = iSeq + 1.
          create EtikettLogg.
          ASSIGN EtikettLogg.Butik     = IF cBrukButikEtikett = "Ja" THEN Etikett.butik ELSE iSeq
                 EtikettLogg.Vg        = ArtBas.Vg   
                 EtikettLogg.LopNr     = ArtBas.LopNr
                 EtikettLogg.Ant       = 1
                 EtikettLogg.Storl     = StrekKode.Kode
                 EtikettLogg.Bongtekst = ArtBas.Bongtekst
                 EtikettLogg.Pris      = IF AVAIL Etikett THEN Etikett.Pris ELSE 0
                 EtikettLogg.Individ   = Individ.individnr
                 EtikettLogg.SeqNr     = iSeq.
      END.
      IF LAST-OF(Individ.butnr) AND lFlereBut THEN DO:
          ASSIGN cInfoRad4 = "START"
                 iSeq      = iSeq + 1.
          create EtikettLogg.
          assign
            EtikettLogg.Butik     = IF cBrukButikEtikett = "Ja" THEN Etikett.butik ELSE iSeq /* Det skal skrives ut i seqnr ordning. */
            EtikettLogg.Vg        = 0   
            EtikettLogg.LopNr     = 0
            EtikettLogg.Ant       = 0
            EtikettLogg.Storl     = "INFO"
            EtikettLogg.Bongtekst = cInfoRad1 + CHR(1) + cInfoRad2 + CHR(1) + cInfoRad3 + CHR(1) + cInfoRad4
            EtikettLogg.Pris      = 0
            EtikettLogg.Pris2     = 0
            EtikettLogg.SeqNr     = iSeq.
      END.
  END.
  RUN x-etikettsend.w (INT(ENTRY(1,cPrinterValg,CHR(1)))).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettEtiketter C-Win 
PROCEDURE SlettEtiketter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
do TRANSACTION:
  for each Etikett exclusive-lock where
    Etikett.LevInnr = BestHode.BestNr:
    Delete Etikett.
  end.
end.                

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Utskrifter C-Win 
PROCEDURE Utskrifter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER lEtikettFinns   AS LOGICAL    NO-UNDO.
  DEFINE INPUT  PARAMETER iIndividBatchNr AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER dArtikkelNr AS DECIMAL    NO-UNDO.
  DEFINE INPUT  PARAMETER iEtikettBut  AS INTEGER    NO-UNDO.
  def var wEtiketter       as log  no-undo.
  def var wFolgeseddel     as log  no-undo.
  DEF VAR wTvFolgeseddel   AS LOG  NO-UNDO.
  def var wInnleveranse    as log  no-undo.
  DEF VAR wSkrivFolgeseddel AS LOG NO-UNDO.
  def var wTekst           as char no-undo.
  DEF VAR cPrinterValg        AS CHAR NO-UNDO.
  fLockvindu(TRUE).
  Lockvindublock: DO:
    {syspara.i 5 4 10 wTekst}
    IF trim(wTekst) = "1" THEN
        wSkrivFolgeseddel = TRUE.
    ELSE
        wSkrivFolgeseddel = FALSE.
  
    /* Utskriftshåndtering. */
    ASSIGN wEtiketter = lEtikettFinns. /* 16/1-03 */
    run d-innlevutskrift.w (BestHode.DirekteLev,OUTPUT wTvFolgeseddel,INPUT-OUTPUT wEtiketter,output wFolgeseddel,output wInnleveranse,OUTPUT cPrinterValg).
/*     if return-value = "AVBRYT" then                                             */
/*       do:                                                                       */
/*         /* Utskrift av Innleveranse skal skje uansett*/                         */
/*         ASSIGN                                                                  */
/*             wTvFolgeseddel = TRUE                                               */
/*             .                                                                   */
/*         RUN dummy.p. /* Nullstiller RETURN-VALUE */                             */
/*         IF wSkrivFolgeseddel THEN                                               */
/*           run tvfolgeseddel.p (wBestHLevRec,107,4,wTvFolgeseddel,wLeveringsNr). */
/*         run SlettEtiketter.                                                     */
/*         LEAVE Lockvindublock.                                                   */
/*       end.                                                                      */
/*                                                                                 */
/*     /* Utskrift av Innleveranse */                                              */
/*     if (wTvFolgeseddel AND wSkrivFolgeseddel) then                              */
/*       run tvfolgeseddel.p (wBestHLevRec,107,4,wTvFolgeseddel,wLeveringsNr).     */
/*                                                                                 */
/*     /* Etikettutskrift */                                                       */
    if wEtiketter then
      do:
        find first Etikett no-lock where
          Etikett.LevInNr = BestHode.BestNr no-error.
        if available Etikett THEN DO:
            IF iIndividBatchNr > 0 THEN
                RUN SkrivIndividEti (iIndividBatchNr,dArtikkelNr,cPrinterValg,iEtikettBut).
            ELSE
                RUN SkrivEtiketter2 (cPrinterValg).
        END.
/*           run w-etikettb.w (input BestHode.BestNr). */
      end.
/*     else */
      RUN SlettEtiketter.     
  
    /* Utskrift av Innleveranse */
    if wInnleveranse then DO:
        IF cInnlevSamlet = "1" THEN
            run bestillingskort.p (wBestHLevRec,107,4,wFolgeseddel).
        ELSE
            run bestillingskortX.p (wBestHLevRec,107,4,wFolgeseddel).
    END.
  END.
  fLockvindu(FALSE).
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
  /* Om alt lev ær brukt - gør detta først */
  IF BestHode.LevNr <> ArtBas.LevNr THEN DO:
      FIND Levbas OF BestHode NO-LOCK NO-ERROR.
      IF AVAIL Levbas THEN
          ASSIGN FI-OrdreLev:SCREEN-VALUE = STRING(LevBas.LevNr) + " : " + LevBas.LevNamn.
  END.
  find LevBas    of ArtBas no-lock no-error.
  find Farg      of ArtBas no-lock no-error.
  find Sasong    of ArtBas no-lock no-error.
  find Bilderegister of ArtBas no-lock no-error.
  find StrType   of ArtBas no-lock no-error.

  if available ArtBas then
  display 
    ArtBas.VgKat
    ArtBas.Beskr 
    ArtBas.LevNr 
    ArtBas.LevFargKod
    ArtBas.LevKod
    LevBas.LevNamn.
 ASSIGN ArtBas.LevKod:SCREEN-VALUE     = if available ArtBas then ArtBas.LevKod else BestHode.LevKod
        ArtBas.LevFargKod:SCREEN-VALUE = if available ArtBas then ArtBas.LevFargKod else BestHode.LevFargKod
  FILL-IN-Vgr-Lopnr:SCREEN-VALUE = STRING(ArtBas.Vg) + 
                                   "/" + 
                                   (if (artBas.LopNr = ? or ArtBas.LopNr = 0) 
                                     then " "
                                     else STRING(ArtBas.LopNr)).
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
  {visbilde.i
    &BldOcx = "chIMAGE-Sko"
    &BildNr = "ArtBas.BildNr"
  }
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisFullev C-Win 
PROCEDURE VisFullev :
/*------------------------------------------------------------------------------
  Purpose:     Visar beställt och inlevererat vid status full inleverans (6)
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VAR wRow         AS INTE NO-UNDO.
    DEFINE VAR wCol         AS INTE NO-UNDO.
    DEFINE VAR wIdx         AS INTE NO-UNDO.
    DEFINE VAR wTotBestLev  AS INTE NO-UNDO.
    DEFINE VAR wBestLevStrl AS INTE NO-UNDO.
    DEFINE VAR wTotCentral  AS INTE NO-UNDO.
    DEFINE VAR wTotCentLev  AS INTE NO-UNDO.
    DEFINE VAR wTotButik    AS INTE NO-UNDO.
    DEFINE VAR wTotButikLev AS INTE NO-UNDO.
    ASSIGN wRow                = ch_Grid:FixedRows
           ch_Grid:ColWidth(1) = 950.
    DO wIdx = 1 to NUM-ENTRIES(wStorlekar," "):
        FIND BestStr WHERE BestStr.BestNr   = BestHode.BestNr            AND
                           BestStr.Butik    = wCentralLager              AND
                           BestStr.Storl    = ENTRY(wIdx,wStorlekar," ") AND
                           BestStr.BestStat = BestHode.BestStat NO-LOCK NO-ERROR.
        IF AVAIL BestStr THEN DO:
            ASSIGN wTotCentral = wTotCentral + BestStr.Bestilt.
            FIND LAST BestLevert WHERE BestLevert.BestNr = BestHode.BestNr            AND
                                       BestLevert.Butik  = wCentralLager              AND
                                       BestLevert.Storl  = ENTRY(wIdx,wStorlekar," ")
                                                           NO-LOCK NO-ERROR.
            IF AVAIL BestLevert THEN DO:
                ASSIGN wTotCentLev = wTotCentLev + (BestStr.Bestilt - BestLevert.Rest)
                       ch_Grid:TextMatrix(wRow,wIdx + 1) = LagString(INT(BestStr.Bestilt)) + "/" +
                                                LagString(INT(BestStr.Bestilt - BestLevert.Rest)).
            END.
        END.
        ELSE DO:
            FIND LAST BestLevert WHERE BestLevert.BestNr = BestHode.BestNr            AND
                                       BestLevert.Butik  = wCentralLager            AND
                                       BestLevert.Storl  = ENTRY(wIdx,wStorlekar," ")
                                                       NO-LOCK NO-ERROR.
            IF AVAIL BestLevert THEN DO:
                ASSIGN wTotCentLev = wTotCentLev + (0 - BestLevert.Rest)
                       ch_Grid:TextMatrix(wRow,wIdx + 1) = LagString(0) + "/" +
                                                           LagString(INT(0 - BestLevert.Rest)).
            END.
            ELSE
                ASSIGN ch_Grid:TextMatrix(wRow,wIdx + 1) = "".
        END.
    END.
    ASSIGN ch_Grid:TextMatrix(wRow,1) = LagString(wTotCentral) + "/" + LagString(wTotCentLev)
           wTotBestLev = wTotCentLev.
/* END Centrallager */

/* Finn resten av butikerna och det som är beställt */
    ASSIGN wRow = ch_Grid:FixedRows + 1.
    FOR EACH BestLinje OF BestHode WHERE BestLinje.Butik <> wCentralLager NO-LOCK:
        ASSIGN wTotButik                  = 0
               wTotButikLev               = 0
               ch_Grid:Rows               = wRow + 1
               ch_Grid:TextMatrix(wRow,0) = STRING(BestLinje.Butik) + " ".
       
      DO wIdx = 1 to NUM-ENTRIES(wStorlekar," "):
        FIND BestStr WHERE BestStr.BestNr   = BestHode.BestNr            AND
                           BestStr.Butik    = BestLinje.Butik            AND
                           BestStr.Storl    = ENTRY(wIdx,wStorlekar," ") AND
                           BestStr.BestStat = BestHode.BestStat NO-LOCK NO-ERROR.
        IF AVAIL BestStr THEN DO:
            ASSIGN wTotButik = wTotButik + BestStr.Bestilt.
            FIND LAST BestLevert WHERE BestLevert.BestNr = BestHode.BestNr            AND
                                       BestLevert.Butik  = BestLinje.Butik            AND
                                       BestLevert.Storl  = ENTRY(wIdx,wStorlekar," ")
                                                           NO-LOCK NO-ERROR.
            IF AVAIL BestLevert THEN DO:
                ASSIGN wTotButikLev = wTotButikLev + (BestStr.Bestilt - BestLevert.Rest)
                       ch_Grid:TextMatrix(wRow,wIdx + 1) = LagString(INT(BestStr.Bestilt)) + "/" +
                                                LagString(INT(BestStr.Bestilt - BestLevert.Rest)).
           END.
        END.
        ELSE DO:
            FIND LAST BestLevert WHERE BestLevert.BestNr = BestHode.BestNr            AND
                                       BestLevert.Butik  = BestLinje.Butik            AND
                                       BestLevert.Storl  = ENTRY(wIdx,wStorlekar," ")
                                                       NO-LOCK NO-ERROR.
            IF AVAIL BestLevert THEN DO:
                ASSIGN wTotButikLev = wTotButikLev + (0 - BestLevert.Rest)
                       ch_Grid:TextMatrix(wRow,wIdx + 1) = LagString(0) + "/" +
                                                           LagString(INT(0 - BestLevert.Rest)).
            END.
            ELSE
                ASSIGN ch_Grid:TextMatrix(wRow,wIdx + 1) = "".
        END.
      END.
      ASSIGN ch_Grid:TextMatrix(wRow,1) = LagString(wTotButik) + "/" + LagString(wTotButikLev)
             wTotBestLev = wTotBestLev + wTotButikLev.
             wRow = wRow + 1.
    END.
    ASSIGN ch_Grid:TextMatrix(3,1) = LagString(INT(ch_Grid:TextMatrix(3,1))) + "/" + LagString(wTotBestLev).
    DO wCol = ch_Grid:FixedCols TO ch_Grid:Cols - 1:
        ASSIGN wBestLevStrl = 0.
        DO wRow = ch_Grid:FixedRows TO ch_Grid:Rows - 1:
            wBestLevStrl = wBestLevStrl + IF NUM-ENTRIES(ch_Grid:TextMatrix(wRow,wCol),"/") = 2 THEN
                              INT(ENTRY(2,ch_Grid:TextMatrix(wRow,wCol),"/")) ELSE 0.
        END.
        ASSIGN ch_Grid:TextMatrix(3,wCol) = LagString(INT(ch_Grid:TextMatrix(3,wCol))) + "/" + LagString(wBestLevStrl).
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AlleEANok C-Win 
FUNCTION AlleEANok RETURNS CHARACTER
    ( INPUT cStorrelser AS CHARACTER ) :
  /*------------------------------------------------------------------------------
    Purpose:  
      Notes:  
  ------------------------------------------------------------------------------*/
    DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.
    DEFINE VARIABLE lFunnet AS LOGICAL    NO-UNDO.
    DEFINE VARIABLE cSaknasListe AS CHARACTER  NO-UNDO.
    ASSIGN lFunnet = TRUE.
    DO iCount = 1 TO NUM-ENTRIES(cStorrelser):
        FIND StrKonv WHERE TRIM(StrKonv.Storl) = TRIM(ENTRY(iCount,cStorrelser)) NO-LOCK NO-ERROR.
        IF AVAIL StrKonv THEN DO:
            FIND FIRST StrekKode WHERE StrekKode.ArtikkelNr = BestHode.ArtikkelNr AND
                                       StrekKode.KodeType = 1 AND 
                                       StrekKode.StrKode     = StrKonv.StrKode AND
                                       NOT StrekKode.Kode BEGINS "02" NO-LOCK NO-ERROR.
            IF NOT AVAIL StrekKode THEN DO:
                ASSIGN cSaknasListe = cSaknasListe + (IF cSaknasListe <> "" THEN "," ELSE "") + ENTRY(iCount,cStorrelser).
            END.
        END.
    END.
    RETURN cSaknasListe.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FixFillIn C-Win 
FUNCTION FixFillIn RETURNS CHARACTER
  ( INPUT wRow AS INTEGER, INPUT wCol AS INTEGER, INPUT wAction AS CHARACTER, INPUT wVerdi AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR wKalk AS INTE NO-UNDO.
DO WITH FRAME {&FRAME-NAME}:
  IF wAction = "+" THEN DO:
      ASSIGN wKalk = IF INT(ENTRY(2,ch_Grid:TextMatrix(wRow,wCol),"/")) <=
                     INT(ENTRY(1,ch_Grid:TextMatrix(wRow,wCol),"/")) THEN wVerdi 
                  ELSE IF INT(ENTRY(2,ch_Grid:TextMatrix(wRow,wCol),"/")) -
                          INT(ENTRY(1,ch_Grid:TextMatrix(wRow,wCol),"/")) >= wVerdi THEN 0
                  ELSE INT(ENTRY(1,ch_Grid:TextMatrix(wRow,wCol),"/")) -
                          (INT(ENTRY(2,ch_Grid:TextMatrix(wRow,wCol),"/")) - wVerdi).

      ASSIGN FILL-IN-InnLev:SCREEN-VALUE = STRING(INT(FILL-IN-InnLev:SCREEN-VALUE) + wVerdi)
             FILL-IN-Rest:SCREEN-VALUE = STRING(INT(FILL-IN-Rest:SCREEN-VALUE) - wKalk).
                                        
  END.
  ELSE DO:
      ASSIGN wKalk = IF INT(ENTRY(2,ch_Grid:TextMatrix(wRow,wCol),"/")) >=
                     INT(ENTRY(1,ch_Grid:TextMatrix(wRow,wCol),"/")) THEN 0 
                  ELSE IF INT(ENTRY(1,ch_Grid:TextMatrix(wRow,wCol),"/")) -
                          INT(ENTRY(2,ch_Grid:TextMatrix(wRow,wCol),"/")) >= wVerdi THEN
                          wVerdi
                  ELSE INT(ENTRY(1,ch_Grid:TextMatrix(wRow,wCol),"/")) -
                          INT(ENTRY(2,ch_Grid:TextMatrix(wRow,wCol),"/")).

      ASSIGN FILL-IN-InnLev:SCREEN-VALUE = STRING(INT(FILL-IN-InnLev:SCREEN-VALUE) - wVerdi)
             FILL-IN-Rest:SCREEN-VALUE = STRING(INT(FILL-IN-Rest:SCREEN-VALUE) + wKalk).
  END.
  END.
  RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fLockvindu C-Win 
FUNCTION fLockvindu RETURNS CHARACTER
  ( INPUT lLock AS LOGICAL ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hDetteVindu AS HANDLE     NO-UNDO.
  ASSIGN hDetteVindu = THIS-PROCEDURE:CURRENT-WINDOW
         hDetteVindu:SENSITIVE = NOT lLock.

  RETURN "".   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FullInlev C-Win 
FUNCTION FullInlev RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR wRow AS INT NO-UNDO.
 
/*  DO wRow = ch_Grid:FixedRows TO ch_Grid:Rows - 1:
      IF INT(ch_Grid:TextMatrix(wRow,1)) > 0 THEN
             RETURN FALSE.
  END. */
  IF INT(FILL-IN-Rest:SCREEN-VALUE IN FRAME {&FRAME-NAME}) > 0 THEN
      RETURN FALSE.
  ELSE
      RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getArtNr C-Win 
FUNCTION getArtNr RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN BestHode.ArtikkelNr.   /* Function return value. */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getStorl C-Win 
FUNCTION getStorl RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN cKontrStorl.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LagString C-Win 
FUNCTION LagString RETURNS CHARACTER
  ( INPUT wVerdi AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  RETURN FILL(" ", 2 * (4 - LENGTH(STRING(wVerdi)))) + STRING(wVerdi).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION OkClear C-Win 
FUNCTION OkClear RETURNS LOGICAL
  (INPUT wModus AS CHARACTER  ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VAR wRow AS INTE NO-UNDO.
  DEFINE VAR wCol AS INTE NO-UNDO.
  CASE wModus:
      WHEN "EN" THEN DO:
          DO wCol = 2 TO ch_Grid:Cols - 1:
              IF NUM-ENTRIES(ch_Grid:TextMatrix(ch_Grid:Row,wCol),"/") = 2 AND
                 TRIM(ENTRY(2,ch_Grid:TextMatrix(ch_Grid:Row,wCol),"/")) <> "" THEN
                  RETURN TRUE.
          END.
      END.
      WHEN "FLER" THEN DO:
          DO wRow = wFstRow TO wLstRow:
              DO wCol = 2 TO ch_Grid:Cols - 1:
/*                   IF NUM-ENTRIES(ch_Grid:TextMatrix(ch_Grid:Row,wCol),"/") = 2 AND */
                  IF NUM-ENTRIES(ch_Grid:TextMatrix(wRow,wCol),"/") = 2 AND
                     TRIM(ENTRY(2,ch_Grid:TextMatrix(wRow,wCol),"/")) <> "" THEN
                      RETURN TRUE.
              END.
          END.
      END.
  END CASE.
  RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION OkInlev C-Win 
FUNCTION OkInlev RETURNS LOGICAL
  ( ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VAR wCount1    AS INTE NO-UNDO.
  
  DO wCount1 = wFstRow TO wLstRow:
      IF INT(ENTRY(1,ch_Grid:TextMatrix(wCount1,1),"/")) > 0 THEN
          RETURN TRUE. 
  END.
  RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION OkMak C-Win 
FUNCTION OkMak RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VAR wRow    AS INTE NO-UNDO.
  DEFINE VAR wCol    AS INTE NO-UNDO.
  DO wRow = wFstRow TO wLstRow:
      IF INT(ENTRY(1,ch_Grid:TextMatrix(wRow,1),"/")) = 0 THEN
          NEXT.

      DO wCol = wFstCol TO wLstCol:
            IF NUM-ENTRIES(ch_Grid:TextMatrix(wRow,wCol),"/") = 2 AND
             INT(ENTRY(1,ch_Grid:TextMatrix(wRow,wCol),"/")) > 0 AND
             TRIM(ENTRY(2,ch_Grid:TextMatrix(wRow,wCol),"/")) <> wAvskrevetCell AND
             INT(ENTRY(2,ch_Grid:TextMatrix(wRow,wCol),"/")) = 0 THEN
              RETURN TRUE.
      END.
  END.
  RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Registrerat C-Win 
FUNCTION Registrerat RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR wRow AS INT NO-UNDO.
  DEF VAR wCol AS INT NO-UNDO.
  
  DO wRow = ch_Grid:FixedRows TO ch_Grid:Rows - 1:
      DO wCol = ch_Grid:FixedCols TO ch_Grid:Cols - 1:
          IF NUM-ENTRIES(ch_Grid:TextMatrix(wRow,wCol),"/") = 2 AND
             ENTRY(2,ch_Grid:TextMatrix(wRow,wCol),"/") <> "" THEN
             RETURN TRUE.
      END.
  END.
  
  RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION RegistreratButik C-Win 
FUNCTION RegistreratButik RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  Se om det finns inleverans till andra butiker än CL
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR wRow AS INT NO-UNDO.
  DEF VAR wCol AS INT NO-UNDO.
  
  DO wRow = ch_Grid:FixedRows + 1 TO ch_Grid:Rows - 1:
      DO wCol = ch_Grid:FixedCols TO ch_Grid:Cols - 1:
          IF NUM-ENTRIES(ch_Grid:TextMatrix(wRow,wCol),"/") = 2 AND
             ENTRY(2,ch_Grid:TextMatrix(wRow,wCol),"/") <> "" THEN
             RETURN TRUE.
      END.
  END.
  
  RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

