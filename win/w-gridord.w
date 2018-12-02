&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win


/* Temp-Table and Buffer definitions                                    */
DEFINE NEW SHARED TEMP-TABLE tBestKasse NO-UNDO LIKE BestKasse.
DEFINE TEMP-TABLE tFributik NO-UNDO LIKE Fributik.
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
  def var wBestHodeRecid as recid no-undo. /* ? = NY, recid(BestHode) = Endre */
  def var wModus         as char  no-undo. /* NYDIREKTE, NY, Endre, Slette */
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
def var wOk            AS log    no-undo.
def var hHandle        as handle no-undo.
def var hLabel         as handle no-undo.
def var wBildNr        as int    no-undo.
def var wFilNavn       as char   no-undo.
def var wFilExt        as char   no-undo.
DEF VAR cPassordkrav   AS CHAR   NO-UNDO.
DEF VAR wLeveringsNr   AS INT    NO-UNDO.
DEF VAR cHKinst        AS CHAR   NO-UNDO.
DEF VAR lNyDirektexIn  AS LOG    NO-UNDO.
DEF VAR lLockedUpdate  AS LOG    NO-UNDO.
DEF VAR cTekst         AS CHAR   NO-UNDO.


/* Temp-Table Definitions ---                                       */
/*
DEFINE NEW SHARED TEMP-TABLE Fri-Str
    FIELD SoAnt    LIKE LevSant.SoAnt FORMAT ">>>>9-"
    FIELD SoStorl  LIKE LevSant.SoStorl
    FIELD Min      AS INTE LABEL "Min" FORMAT ">>>9-"
    FIELD Tmp      AS INTE
    FIELD SeqNr    LIKE LevSant.SeqNr
    INDEX SeqNr IS PRIMARY SeqNr ASCENDING.
*/

DEFINE VAR wFriAntal AS INTE EXTENT 50 NO-UNDO. /* CL */

/*    

DEFINE TEMP-TABLE tFriButik
    FIELD BestNr   LIKE BestHode.BestNr
    FIELD Butik    LIKE Butiker.Butik
    FIELD FriAntal AS INTE EXTENT 50
    INDEX .....

DEFINE TEMP-TABLE tBestKasse
    FIELD BestNr   LIKE BestHode.BestNr
    FIELD SortID   LIKE LevSort.SortID
    FIELD Butik    LIKE Butiker.Butik
    FIELD Antal    AS INTE
    INDEX Bsb IS PRIMARY BestNr ASCENDING
                         SortID ASCENDING
                         Butik  ASCENDING.
*/
DEFINE TEMP-TABLE InitBestKasse LIKE BestKasse.
&SCOPE SORTBY-PHRASE BY tLevsort.Antsort DESCENDING
DEFINE BUFFER BtBestKasse FOR tBestKasse.
DEFINE BUFFER CLtFributik FOR tFributik.
DEFINE BUFFER FritLevSort FOR tLevSort.
DEFINE VAR ch_Grid       AS COM-HANDLE NO-UNDO.
DEFINE VAR wCentralLager LIKE Butiker.Butik NO-UNDO.
DEFINE VAR lCanUpdateCL  AS LOGICAL    NO-UNDO.
DEFINE VAR cUpdateButListe AS CHAR NO-UNDO.
DEFINE VAR wStorlekar    AS CHAR NO-UNDO. /* Storlekar i matris */
DEFINE VAR wKassCell     AS INTE EXTENT 50 NO-UNDO. /* Storlekar i matris */
DEFINE VAR wFstRow       AS INTE NO-UNDO.
DEFINE VAR wLstRow       AS INTE NO-UNDO.
DEFINE VAR wFstCol       AS INTE NO-UNDO.
DEFINE VAR wLstCol       AS INTE NO-UNDO.
DEFINE VAR wBest         AS INTE NO-UNDO. /* För ändring av tLevsort */
DEFINE VAR wKassNy       AS INTE NO-UNDO. /* För ändring av tLevsort */
DEFINE VAR wFriAnt       AS INTE NO-UNDO.
DEFINE VAR wStatus       AS CHAR NO-UNDO.
DEFINE VAR wFriSort      AS LOGI NO-UNDO.
DEFINE VAR wStrTypeLst   AS CHAR NO-UNDO.
DEFINE VAR wSHC#         AS INTE INIT 16 NO-UNDO. /* Antal hela synliga Cols */
DEFINE VAR wAktivFriRad  AS INTE NO-UNDO.
DEFINE VAR wOrgFriFGCol  AS INTE NO-UNDO.
DEFINE VAR wFriFGCol     AS INTE NO-UNDO.
def    var wEDB-System    as char no-undo.
def    var wTabell        as char no-undo.
DEF    VAR iOpphav        AS INTE INIT  4 NO-UNDO. /* ovBunt.Opphav = 4 dirinnlev */
def var wCBValg       as char   no-undo. /* Sysparaverdi för [Alle] ändras löpande */
def var winitCBValg   as char   no-undo. /* Sysparaverdi för [Alle] */
def var wBrGrpNr  like BrukerGrp.BrGrpNr NO-UNDO.
def var wBekreft         as log no-undo.
DEFINE VARIABLE iTeamNr LIKE ButikkTeam.TeamNr    NO-UNDO.
DEFINE VARIABLE cStatusStr        AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iMouseMoveRow     AS INTEGER    NO-UNDO.
DEFINE VARIABLE cBrukerButTilgang AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cKontrStorl       AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cBrukButikEtikett AS CHARACTER  NO-UNDO. /* överstyr etiketter.butik vid ikke direktelev */
DEFINE VARIABLE cManRegEAN    AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cInnlevSamlet AS CHARACTER  NO-UNDO.
/* DEFINE VAR dia-Best LIKE tLevSort.Best NO-UNDO. */
def var wNegInnLev        as log  no-undo.
def var wLapTop           as log  no-undo.

/* Buffere */
def buffer bArtBas for ArtBas.
def buffer bBestStr for BestStr.
def buffer btLevSort for tLevSort.
def buffer bBestPris for BestPris.

{etikettlogg.i &NEW=NEW}
{runlib.i}

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
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1 tLevSort.AntSort 
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-1 tLevSort
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-1 tLevSort
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH tLevSort ~
      WHERE tLevSort.Fri = FALSE NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY BROWSE-1 FOR EACH tLevSort ~
      WHERE tLevSort.Fri = FALSE NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 tLevSort
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 tLevSort


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BROWSE-1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS BestHode.BestillingsDato BestHode.LevDato ~
BestHode.BekreftetOrdre BestHode.Beskrivelse BestHode.DirekteLev ~
BestHode.Merknad 
&Scoped-define ENABLED-TABLES BestHode
&Scoped-define FIRST-ENABLED-TABLE BestHode
&Scoped-Define ENABLED-OBJECTS RECT-1 B-Etiketter RECT-2 RECT-3 RECT-47 ~
RECT-48 RECT-68 B-Print B-PrintInnlev Btn_Done BUTTON-Sortiment CB-Lev ~
TOGGLE-Tillagg BROWSE-1 Btn_Help FILL-IN-2 FILL-IN-1 
&Scoped-Define DISPLAYED-FIELDS BestHode.BestillingsDato ArtBas.VgKat ~
ArtBas.Beskr BestHode.LevDato BestPris.VareKost BestHode.TotInnkjVerdi ~
BestHode.BekreftetDato BestPris.DBKr BestHode.TotDbKr ArtBas.LevNr ~
LevBas.levnamn BestHode.BekreftetOrdre ArtBas.LevKod ArtBas.LevFargKod ~
BestHode.EkstId BestPris.Pris BestHode.TotSalgsVerdi BestHode.Beskrivelse ~
BestHode.DirekteLev BestHode.Merknad BestHode.RegistrertAv BestHode.EDato 
&Scoped-define DISPLAYED-TABLES BestHode ArtBas BestPris LevBas
&Scoped-define FIRST-DISPLAYED-TABLE BestHode
&Scoped-define SECOND-DISPLAYED-TABLE ArtBas
&Scoped-define THIRD-DISPLAYED-TABLE BestPris
&Scoped-define FOURTH-DISPLAYED-TABLE LevBas
&Scoped-Define DISPLAYED-OBJECTS T-LapTop FILL-IN-Vgr-Lopnr CB-Lev ~
TOGGLE-Tillagg FILL-IN-2 FILL-IN-1 

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD harBestillt C-Win 
FUNCTION harBestillt RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Modifierad C-Win 
FUNCTION Modifierad RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD OKbyteTeam C-Win 
FUNCTION OKbyteTeam RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD OkDelrad C-Win 
FUNCTION OkDelrad RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD OkDirektLev C-Win 
FUNCTION OkDirektLev RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD OkSortering C-Win 
FUNCTION OkSortering RETURNS LOGICAL
  ( INPUT wType AS CHARACTER )  FORWARD.

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

DEFINE MENU POPUP-MENU-BROWSE-1 
       MENU-ITEM m_NylevSort    LABEL "Nytt sortiment...".

DEFINE MENU POPUP-MENU-FRAME-A 
       MENU-ITEM m_nysortering  LABEL "&Lägg till sortering"
       MENU-ITEM m_n_nysortering LABEL "Lägg till (n) &sortering"
       MENU-ITEM m_delsortering LABEL "&Ta bort sortering"
       RULE
       MENU-ITEM m_nystaket     LABEL "Lägg &till 1 i område"
       MENU-ITEM m_n_nystaket   LABEL "Lägg &till (n) i område"
       MENU-ITEM m_delstaket    LABEL "Ta &bort 1 i område"
       RULE
       MENU-ITEM m_delruta      LABEL "Rensa &aktuell ruta"
       MENU-ITEM m_delraden     LABEL "Rensa &hela raden"
       MENU-ITEM m_delomr       LABEL "Rensa &markerat område".


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
     SIZE 4.4 BY 1.1
     BGCOLOR 8 .

DEFINE BUTTON Btn_Help DEFAULT 
     IMAGE-UP FILE "icon/e-help":U NO-FOCUS
     LABEL "&Hjelp" 
     SIZE 4.6 BY 1.1 TOOLTIP "Hjelp"
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-DirLev 
     LABEL "Direkte lev" 
     SIZE 15 BY 1.

DEFINE BUTTON BUTTON-FraBest 
     LABEL "Fra bestilling" 
     SIZE 18 BY 1.

DEFINE BUTTON BUTTON-Godkann 
     LABEL "&Godkjenn" 
     SIZE 15 BY 1.

DEFINE BUTTON BUTTON-Kalkyl 
     LABEL "Kalkyle..." 
     SIZE 15 BY 1.

DEFINE BUTTON BUTTON-Lagra 
     IMAGE-UP FILE "icon/e-save":U
     LABEL "&Lagra" 
     SIZE 4.6 BY 1.1 TOOLTIP "Alt-L Lagre".

DEFINE BUTTON BUTTON-NegLev 
     LABEL "Negativ innleveranse" 
     SIZE 23 BY 1.

DEFINE BUTTON BUTTON-SokTeam 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-Sortiment 
     LABEL "Sortiment..." 
     SIZE 15 BY 1.

DEFINE VARIABLE CB-Lev AS INTEGER FORMAT "zzzzz9" INITIAL 0 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "0",1
     DROP-DOWN-LIST
     SIZE 40.8 BY 1 NO-UNDO.

DEFINE VARIABLE CB-Team AS CHARACTER FORMAT "X(256)":U 
     LABEL "Team" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 34 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-1 AS CHARACTER FORMAT "X(256)":U INITIAL "Totalt" 
      VIEW-AS TEXT 
     SIZE 8 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FILL-IN-2 AS CHARACTER FORMAT "X(256)":U INITIAL "Per enhet" 
      VIEW-AS TEXT 
     SIZE 11 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FILL-IN-Vgr-Lopnr AS CHARACTER FORMAT "X(10)":U 
     LABEL "VGr/Løpenr" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 62 BY 6.38.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 35 BY 6.38.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 54 BY 5.19.

DEFINE RECTANGLE RECT-47
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 152.4 BY .1.

DEFINE RECTANGLE RECT-48
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 152.4 BY .1.

DEFINE RECTANGLE RECT-68
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 54 BY 1.19.

DEFINE VARIABLE T-LapTop AS LOGICAL INITIAL no 
     LABEL "LapTop" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-Tillagg AS LOGICAL INITIAL no 
     LABEL "Supplering" 
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
  QUERY BROWSE-1 NO-LOCK DISPLAY
      tLevSort.AntSort COLUMN-LABEL "Best" FORMAT "zzz9":U
      tLevSort.SortID COLUMN-LABEL "Sortiment" FORMAT "X(12)":U
      tLevSort.Antall FORMAT "zzz9":U
      tLevSort.StrInterval COLUMN-LABEL "Str.intervall" FORMAT "X(11)":U
      tLevSort.Fordeling FORMAT "X(45)":U
      tLevSort.Storrelser FORMAT "X(70)":U
  ENABLE
      tLevSort.AntSort
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 85.8 BY 4.95 ROW-HEIGHT-CHARS .62.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     B-Etiketter AT ROW 1.19 COL 15.8
     BUTTON-Lagra AT ROW 1.19 COL 2
     B-Print AT ROW 1.19 COL 6.6
     B-PrintInnlev AT ROW 1.19 COL 11.2
     Btn_Done AT ROW 1.19 COL 148
     BUTTON-Sortiment AT ROW 1.24 COL 40.6
     BUTTON-Kalkyl AT ROW 1.24 COL 55.6
     BUTTON-NegLev AT ROW 1.24 COL 70.6
     BUTTON-DirLev AT ROW 1.24 COL 93.6
     BUTTON-Godkann AT ROW 1.24 COL 108.6
     BUTTON-FraBest AT ROW 1.24 COL 123.6
     T-LapTop AT ROW 1.33 COL 22
     CB-Team AT ROW 2.91 COL 20 COLON-ALIGNED
     BestHode.BestillingsDato AT ROW 2.91 COL 79 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     FILL-IN-Vgr-Lopnr AT ROW 4.1 COL 20 COLON-ALIGNED
     ArtBas.VgKat AT ROW 4.1 COL 34.4 COLON-ALIGNED NO-LABEL FORMAT "z9"
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
     ArtBas.Beskr AT ROW 4.1 COL 40 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 20.4 BY 1
     BestHode.LevDato AT ROW 4.1 COL 79 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     BestPris.VareKost AT ROW 4.1 COL 108 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20.2 BY 1
     BestHode.TotInnkjVerdi AT ROW 4.1 COL 130 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 20.2 BY 1
     BestHode.BekreftetDato AT ROW 5.29 COL 79 COLON-ALIGNED
          LABEL "Bekreftet"
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     BestPris.DBKr AT ROW 5.29 COL 108 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     BestHode.TotDbKr AT ROW 5.29 COL 130 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 20.2 BY 1
     ArtBas.LevNr AT ROW 5.33 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     CB-Lev AT ROW 5.33 COL 20 COLON-ALIGNED HELP
          "Leverandørnummer" NO-LABEL
     LevBas.levnamn AT ROW 5.33 COL 34.6 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     BestHode.BekreftetOrdre AT ROW 5.43 COL 94.4
          LABEL ""
          VIEW-AS TOGGLE-BOX
          SIZE 3 BY .81
     ArtBas.LevKod AT ROW 6.48 COL 20 COLON-ALIGNED
          LABEL "LevArtNr/Farge"
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     ArtBas.LevFargKod AT ROW 6.48 COL 42.6 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     BestHode.EkstId AT ROW 6.48 COL 79 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     BestPris.Pris AT ROW 6.48 COL 108 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     BestHode.TotSalgsVerdi AT ROW 6.48 COL 130 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 20.2 BY 1
     BestHode.Beskrivelse AT ROW 7.67 COL 20 COLON-ALIGNED
          LABEL "Merknad"
          VIEW-AS FILL-IN 
          SIZE 40.8 BY 1
     BestHode.DirekteLev AT ROW 7.91 COL 66
          LABEL "Dir.levert"
          VIEW-AS TOGGLE-BOX
          SIZE 13.4 BY .81
     TOGGLE-Tillagg AT ROW 7.91 COL 81
     BROWSE-1 AT ROW 9.05 COL 2.2
     BestHode.Merknad AT ROW 9.05 COL 89 NO-LABEL
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          SIZE 37 BY 4.95
     BUTTON-SokTeam AT ROW 2.91 COL 56
     Btn_Help AT ROW 1.19 COL 143
     FILL-IN-2 AT ROW 3.14 COL 112 COLON-ALIGNED NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1.05
         SIZE 152.4 BY 30.95.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME DEFAULT-FRAME
     FILL-IN-1 AT ROW 3.14 COL 135 COLON-ALIGNED NO-LABEL
     BestHode.RegistrertAv AT ROW 8 COL 115.2 COLON-ALIGNED
          LABEL "Reg./endret av"
           VIEW-AS TEXT 
          SIZE 16 BY .62
     BestHode.EDato AT ROW 8 COL 134 COLON-ALIGNED NO-LABEL
           VIEW-AS TEXT 
          SIZE 16 BY .62
     RECT-1 AT ROW 2.57 COL 2
     RECT-2 AT ROW 2.57 COL 64
     RECT-3 AT ROW 2.57 COL 99
     RECT-47 AT ROW 2.33 COL 1
     RECT-48 AT ROW 1.05 COL 1
     RECT-68 AT ROW 7.76 COL 99
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1.05
         SIZE 152.4 BY 30.95.

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
      TABLE: tBestKasse T "NEW SHARED" NO-UNDO SkoTex BestKasse
      TABLE: tFributik T "?" NO-UNDO SkoTex Fributik
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
         TITLE              = "Bestilling artikkel:"
         HEIGHT             = 31
         WIDTH              = 152.4
         MAX-HEIGHT         = 31
         MAX-WIDTH          = 152.4
         VIRTUAL-HEIGHT     = 31
         VIRTUAL-WIDTH      = 152.4
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
   NOT-VISIBLE FRAME-NAME                                               */
/* BROWSE-TAB BROWSE-1 TOGGLE-Tillagg DEFAULT-FRAME */
ASSIGN 
       FRAME DEFAULT-FRAME:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN BestHode.BekreftetDato IN FRAME DEFAULT-FRAME
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR TOGGLE-BOX BestHode.BekreftetOrdre IN FRAME DEFAULT-FRAME
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN ArtBas.Beskr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN BestHode.Beskrivelse IN FRAME DEFAULT-FRAME
   EXP-LABEL                                                            */
ASSIGN 
       BROWSE-1:POPUP-MENU IN FRAME DEFAULT-FRAME             = MENU POPUP-MENU-BROWSE-1:HANDLE
       BROWSE-1:NUM-LOCKED-COLUMNS IN FRAME DEFAULT-FRAME     = 4.

/* SETTINGS FOR BUTTON BUTTON-DirLev IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-FraBest IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-Godkann IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-Kalkyl IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-Lagra IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-NegLev IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-SokTeam IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BUTTON-SokTeam:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR COMBO-BOX CB-Team IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       CB-Team:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN BestPris.DBKr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX BestHode.DirekteLev IN FRAME DEFAULT-FRAME
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN BestHode.EDato IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN BestHode.EkstId IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-Vgr-Lopnr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ArtBas.LevFargKod IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ArtBas.LevKod IN FRAME DEFAULT-FRAME
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN LevBas.levnamn IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ArtBas.LevNr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BestHode.Merknad:RETURN-INSERTED IN FRAME DEFAULT-FRAME  = TRUE.

/* SETTINGS FOR FILL-IN BestPris.Pris IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN BestHode.RegistrertAv IN FRAME DEFAULT-FRAME
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR TOGGLE-BOX T-LapTop IN FRAME DEFAULT-FRAME
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
     _Options          = "NO-LOCK SORTBY-PHRASE"
     _Where[1]         = "Temp-Tables.tLevSort.Fri = FALSE"
     _FldNameList[1]   > Temp-Tables.tLevSort.AntSort
"tLevSort.AntSort" "Best" ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
       WIDTH           = 26
       HIDDEN          = no
       SENSITIVE       = yes.

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 14.19
       COLUMN          = 2.4
       HEIGHT          = 17.76
       WIDTH           = 150.6
       HIDDEN          = no
       SENSITIVE       = yes.
/* Image-Sko OCXINFO:CREATE-CONTROL from: {9A93B740-C96B-11D0-8883-444553540000} type: Picbuf */
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {0F026C11-5A66-4c2b-87B5-88DDEBAE72A1} type: VSFlexGrid */
      Image-Sko:MOVE-AFTER(BestHode.Merknad:HANDLE IN FRAME DEFAULT-FRAME).
      CtrlFrame:MOVE-AFTER(Image-Sko).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Bestilling artikkel: */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Bestilling artikkel: */
DO:
  /* This event will close the window and terminate the procedure.  */
  
  IF BUTTON-Lagra:SENSITIVE IN FRAME {&FRAME-NAME} = YES OR
     Modifierad() THEN DO:
      MESSAGE "Vil du lagre før du avslutter"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL
                    TITLE "" UPDATE choice AS LOGICAL.
      CASE choice:
         WHEN TRUE THEN /* Yes */
              APPLY "CHOOSE" TO BUTTON-Lagra IN FRAME {&FRAME-NAME}.
         WHEN ? THEN /* Cancel */
             RETURN NO-APPLY.
         WHEN FALSE THEN DO TRANSACTION:
             IF AVAIL BestHode THEN DO:
                 FOR EACH BestKasse WHERE BestKasse.BestNr = BestHode.BestNr EXCLUSIVE:
                     DELETE BestKasse.
                 END.
                 FOR EACH InitBestKasse:
                     BUFFER-COPY InitBestKasse TO BestKasse.
                     RELEASE BestKasse.
                 END.
             END.
         END.
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
/*   message "Skal det skrives ut vedlegg pr. butikk?"                              */
/*     view-as alert-box question buttons yes-no-cancel title "Innleveranserapport" */
/*     update wBekreft.                                                             */
/*   if wBekreft = ? then                                                           */
/*     return no-apply.                                                             */
/*   else                                                                           */
    IF cInnlevSamlet = "1" THEN
        run bestillingskort.p (recid(BestHode),109,4,wBekreft).
    ELSE
        run bestillingskortX.p (recid(BestHode),109,4,wBekreft).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BestHode.BekreftetOrdre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BestHode.BekreftetOrdre C-Win
ON VALUE-CHANGED OF BestHode.BekreftetOrdre IN FRAME DEFAULT-FRAME
DO:
  ASSIGN
      BestHode.BekreftetDato:SCREEN-VALUE = IF INPUT Besthode.BekreftetOrdre = TRUE 
                                              THEN STRING(TODAY)
                                              ELSE ?
      BUTTON-Lagra:SENSITIVE = TRUE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BestHode.Beskrivelse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BestHode.Beskrivelse C-Win
ON VALUE-CHANGED OF BestHode.Beskrivelse IN FRAME DEFAULT-FRAME /* Merknad */
DO:
  IF wBestHodeRecid <> ? THEN 
  DO:
    IF BestHode.BestStat <= 4 THEN
        BUTTON-Lagra:SENSITIVE = TRUE.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BestHode.BestillingsDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BestHode.BestillingsDato C-Win
ON LEAVE OF BestHode.BestillingsDato IN FRAME DEFAULT-FRAME /* Bestillingsdato */
DO:
    IF INPUT BestHode.BestillingsDato = ? THEN DO:
        MESSAGE "Angi dato." VIEW-AS ALERT-BOX INFORMATION TITLE "".
        RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BestHode.BestillingsDato C-Win
ON VALUE-CHANGED OF BestHode.BestillingsDato IN FRAME DEFAULT-FRAME /* Bestillingsdato */
DO:
  IF wBestHodeRecid <> ? THEN 
  DO:
    IF BestHode.BestStat <= 4 THEN
        BUTTON-Lagra:SENSITIVE = TRUE.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-1
&Scoped-define SELF-NAME BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 C-Win
ON DEFAULT-ACTION OF BROWSE-1 IN FRAME DEFAULT-FRAME
DO:
    IF wBestHodeRecid <> ? AND BestHode.BestStat > 4 THEN
        RETURN NO-APPLY.
    IF BROWSE {&BROWSE-NAME}:FOCUSED-ROW <> ? THEN
      ASSIGN tLevSort.AntSort:READ-ONLY IN BROWSE {&BROWSE-NAME} = NO
             wBest = tLevSort.AntSort.

    APPLY "ENTRY" TO tLevSort.AntSort IN BROWSE BROWSE-1.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 C-Win
ON RETURN OF BROWSE-1 IN FRAME DEFAULT-FRAME
DO:
  /*APPLY "LEAVE" TO tLevSort.BestNr IN BROWSE BROWSE-1. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 C-Win
ON START-SEARCH OF BROWSE-1 IN FRAME DEFAULT-FRAME
DO:
  APPLY "END-SEARCH" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 C-Win
ON TAB OF BROWSE-1 IN FRAME DEFAULT-FRAME
DO:
/*  APPLY "LEAVE" TO tLevSort.BestNr IN BROWSE BROWSE-1.
    RETURN NO-APPLY. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 C-Win
ON VALUE-CHANGED OF BROWSE-1 IN FRAME DEFAULT-FRAME
DO:
   IF BestHode.DirekteLev:CHECKED IN FRAME DEFAULT-FRAME THEN
       RUN VisKasser.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Done C-Win
ON CHOOSE OF Btn_Done IN FRAME DEFAULT-FRAME /* Avslutt */
DO:
  if available BestHode AND Modifierad() OR BUTTON-Lagra:SENSITIVE THEN
    APPLY "CHOOSE" TO BUTTON-Lagra.

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


&Scoped-define SELF-NAME BUTTON-DirLev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-DirLev C-Win
ON CHOOSE OF BUTTON-DirLev IN FRAME DEFAULT-FRAME /* Direkte lev */
DO:
    APPLY "ENTRY" TO CtrlFrame.
    if wLapTop then
      do:
        message "Innleveranser kan ikke gjøres i innkjøpsystemet"
                view-as alert-box message title "Melding".
        return no-apply.
      end.
    IF INPUT BestHode.BestillingsDato > INPUT BestHode.LevDato THEN
    DO:
        MESSAGE "Bestillingsdato kan ikke være større enn leveringsdato!"
            VIEW-AS ALERT-BOX ERROR TITLE "Feil".
        RETURN NO-APPLY.
    END.
    if not available ArtBas then
      find ArtBas no-lock where
        recid(ArtBas) = wArtBasRecid no-error.
    if ArtBas.LopNr = 0 or
       ArtBas.LopNr = ? then
      do:
        MESSAGE "Løpenummer er ikke tildelt artikkelen?" skip
                "Innleveranse kan ikke skje før løpenummer er tildelt."
                VIEW-AS ALERT-BOX MESSAGE 
                        TITLE "Full inleverans".
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
        message "Det er ikke lagt inn kalkyle på bestillingen for sentrallageret."
          view-as alert-box message title "Melding".
        return no-apply.
      end.

    ASSIGN wOk = TRUE. /* Har de tryckt på knappen för inleverans bör 
                          default vara TRUE. Vi behöver det i tillägg
                          när vi har passordskrav
                        */
    if (BestPris.VareKost <= 0 or BestPris.Pris <= 0) THEN DO:
        message "Kalkylen står med 0 i varekos/pris, vil du virkelig levere inn?"
          view-as alert-box question buttons YES-NO title "Ufullstendig kalkyle"
          update wOk. 
    END.
/*     ELSE IF cPassordkrav <> "1" AND lNyDirekteIn = FALSE THEN DO: */
    ELSE IF cPassordkrav <> "1" THEN DO:
        MESSAGE IF wNegInnLev = FALSE THEN "Bekreft direkte innlevering?"
                               ELSE  "Bekreft NEGATIV innlevering?"
                VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
                        TITLE "Bekreft" UPDATE wOK.
    END.
    IF wOK THEN DO:
        RUN DirekteInnlev.
        RUN SettModified(FALSE).    
    END.
    
    ASSIGN wNegInnLev = FALSE. /* alltid */
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-FraBest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-FraBest C-Win
ON CHOOSE OF BUTTON-FraBest IN FRAME DEFAULT-FRAME /* Fra bestilling */
DO:

    FIND CURRENT BestHode EXCLUSIVE NO-ERROR.
    IF LOCKED BestHode THEN DO:
        MESSAGE "Posten oppdateras av en annen bruker." VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    MESSAGE "Koble fra bestilling?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
                    TITLE "Fra ordre" UPDATE wOKFra AS LOGICAL.
    IF NOT wOKFra THEN
        RETURN NO-APPLY.  
    {sww.i}
    ASSIGN BestHode.Beskrivelse     = INPUT BestHode.Beskrivelse
           BestHode.BestType        = IF TOGGLE-Tillagg:CHECKED THEN 2
                                       ELSE 1
           BestHode.DirekteLev      = IF BestHode.DirekteLev:SENSITIVE THEN INPUT BestHode.DirekteLev
                                        ELSE BestHode.DirekteLev
           BestHode.Merknad         = INPUT BestHode.Merknad
           BestHode.LevDato         = INPUT BestHode.LevDato
           BestHode.LevFargKod      = INPUT ArtBas.LevFargKod
           BestHode.LevKod          = INPUT ArtBas.LevKod
           BestHode.BestillingsDato = INPUT BestHode.BestillingsDato.
    FIND CURRENT BestHode NO-LOCK.
    RUN bytbeststatus.p (wBestHodeRecid,"-",?).
    FIND CURRENT BestHode NO-LOCK.
    RUN SetWindowTitle.
    RUN InitButtons.
    {swn.i}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Godkann
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Godkann C-Win
ON CHOOSE OF BUTTON-Godkann IN FRAME DEFAULT-FRAME /* Godkjenn */
DO:
  DEFINE VAR wOrdreNr LIKE Ordre.OrdreNr INIT ? NO-UNDO.

  /* Sjekker at kalkyle er satt */
  find Butiker no-lock where
    Butiker.Butik = wCentralLager no-error.
  find BestPris no-lock where
    BestPris.BestNr   = BestHode.BestNr and
    BestPris.BestStat = BestHode.BestStat and
    BestPris.ProfilNr = Butiker.ProfilNr no-error.
  if not available BestPris then
    do:
      message "Det er ikke lagt inn kalkyle på bestillingen."
        view-as alert-box message title "Melding".
      return no-apply.
    end.

  if (BestPris.VareKost <= 0 or BestPris.Pris <= 0) then
    do:
      message "Kalkylen står med 0 i varekos/pris, vil du virkelig godkjenne/koble?"
        view-as alert-box question buttons YES-NO title "Ufullstendig kalkyle"
        update wSvar as log. 
      IF NOT wSvar THEN
         RETURN NO-APPLY.  
    end.

  DO WITH FRAME {&FRAME-NAME}:
    IF BestHode.BestStat > 3 THEN 
        RETURN NO-APPLY.
    IF BestHode.BestStat = 2 THEN DO:
        MESSAGE "Koble til ordre?"
                VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
                        TITLE "Til ordre" UPDATE wOKTil AS LOGICAL.
        IF NOT wOKTil THEN
            RETURN NO-APPLY.  
        RUN d-bbestordre.w (INPUT-OUTPUT wOrdreNr, input BestHode.LevNr).
        IF RETURN-VALUE = "Avbryt" THEN DO:
            MESSAGE "Ordrekobling avbrutt" VIEW-AS ALERT-BOX INFORMATION.
            RETURN NO-APPLY.
        END.
        ELSE DO:
            FIND Ordre WHERE RECID(Ordre) = INT(RETURN-VALUE) NO-LOCK NO-ERROR.
            IF NOT AVAILABLE Ordre THEN DO:
                MESSAGE "Ordrepost mangler" VIEW-AS ALERT-BOX ERROR.
                RETURN NO-APPLY.
            END.
            ASSIGN wOrdreNr = Ordre.OrdreNr.
        END.
    END.
    ELSE IF BestHode.BestStat = 3 THEN DO:
        MESSAGE "Koble fra ordre?"
                VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
                        TITLE "Fra ordre" UPDATE wOKFra AS LOGICAL.
        IF NOT wOKFra THEN
            RETURN NO-APPLY.  
    END.
    FIND CURRENT BestHode EXCLUSIVE NO-ERROR.
    IF LOCKED BestHode THEN DO:
        MESSAGE "Posten oppdateres av en annen bruker." VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ASSIGN BestHode.Beskrivelse     = INPUT BestHode.Beskrivelse
           BestHode.BestType        = IF TOGGLE-Tillagg:CHECKED THEN 2
                                       ELSE 1
           BestHode.DirekteLev      = IF BestHode.DirekteLev:SENSITIVE THEN INPUT BestHode.DirekteLev
                                        ELSE BestHode.DirekteLev
           BestHode.Merknad         = INPUT BestHode.Merknad
           BestHode.LevDato         = INPUT BestHode.LevDato
           BestHode.LevFargKod      = INPUT ArtBas.LevFargKod
           BestHode.LevKod          = INPUT ArtBas.LevKod
           BestHode.BestillingsDato = INPUT BestHode.BestillingsDato.
    FIND CURRENT BestHode NO-LOCK.
    {sww.i}
    RUN bytbeststatus.p (wBestHodeRecid,IF BestHode.BestStat < 3 THEN "+" 
                           ELSE "-",wOrdreNr).
    {swn.i}                           
    FIND CURRENT BestHode NO-LOCK.
    RUN SetWindowTitle.
    RUN InitButtons.
  END.
  APPLY "ENTRY" TO CtrlFrame.
  RETURN NO-APPLY.
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
    DEFINE VAR wFriFordeling AS CHAR NO-UNDO. /* antal / storlek */
    DEFINE VAR wStorlek      AS CHAR NO-UNDO. /* strl i find, går ej med ch_Grid */
    DEFINE VAR wButik        LIKE BestStr.Butik NO-UNDO.
    DEFINE VAR wRow          AS INTE NO-UNDO. 
    DEFINE VAR wCol          AS INTE NO-UNDO. 
    DEFINE VAR wTillbudID    AS INTE NO-UNDO.
    DEFINE VAR wIdx          AS INTE NO-UNDO.
    DEFINE VAR lNyBest       AS LOGI NO-UNDO.
    DEFINE VARIABLE dTmpLevDato AS DATE        NO-UNDO.

    IF INPUT BestHode.BestillingsDato > INPUT BestHode.LevDato THEN
    DO:
        MESSAGE "Bestillingsdato kan ikke være større enn leveringsdato!"
            VIEW-AS ALERT-BOX ERROR TITLE "Feil".
        RETURN NO-APPLY.
    END.

    IF wBestHodeRecid = ? THEN DO:
        IF INPUT BestHode.LevDato <> ? THEN
            dTmpLevDato = INPUT BestHode.LevDato.
/*        CREATE BestHode.
        ASSIGN BestHode.ArtikkelNr      = ArtBas.ArtikkelNr
               BestHode.BestStat        = 1
               BestHode.StrTypeID       = ArtBas.StrTypeID
               BestHode.LevFargKod      = ArtBas.LevFargKod
               BestHode.LevKod          = ArtBas.LevKod
               BestHode.LevNr           = ArtBas.LevNr
               BestHode.LevDato         = INPUT BestHode.LevDato
               BestHode.OrdreNr         = 0
               BestHode.BestillingsDato = INPUT BestHode.BestillingsDato
               BestHode.LapTop          = if wLapTop then true else false. */
        ASSIGN lNyBest = TRUE.
        RUN opprettbestilling.p (RECID(ArtBas),
                                 ?,
                                 ?,
                                 wLapTop,
                                 wCentralLager,
                                 wStorlekar,
                                 wBrGrpNr,
                                 IF CB-Team:SCREEN-VALUE = winitCBValg THEN "" ELSE CB-Team:SCREEN-VALUE,
                                 OUTPUT wBestHodeRecid).
        FIND BestHode where RECID(BestHode) = wBestHodeRecid EXCLUSIVE NO-ERROR.
        DO WITH FRAME {&FRAME-NAME}:
            DISPLAY BestHode.LevDato
                    BestHode.BestillingsDato
                    BestHode.RegistrertAv.
            IF dTmpLevDato <> ? THEN
                BestHode.LevDato:SCREEN-VALUE = STRING(dTmpLevDato).
        END.
        ASSIGN BUTTON-SokTeam:HIDDEN = YES
               CB-Team:HIDDEN        = YES.
    END.
    ELSE DO:
        FIND BestHode where RECID(BestHode) = wBestHodeRecid EXCLUSIVE NO-ERROR.
        IF LOCKED BestHode THEN DO:
            MESSAGE "Posten oppdateres av en annen bruker." VIEW-AS ALERT-BOX.
            RETURN NO-APPLY.
        END.
    END.
    {sww.i}
    IF can-do("JA,YES,TRUE",BestHode.DirekteLev:SCREEN-VALUE) THEN
        BestHode.DirekteLev = TRUE.
    ELSE
        BestHode.DirekteLev = FALSE.
    ASSIGN /* BestHode.AnonseArtikkel  = INPUT ArtBas.AnonseArtikkel */
            BestHode.LevNr           = CB-Lev
            BestHode.Beskrivelse     = INPUT BestHode.Beskrivelse
            BestHode.BestType        = IF TOGGLE-Tillagg:CHECKED THEN 2
                                           ELSE 1
            BestHode.Merknad         = INPUT BestHode.Merknad
            BestHode.TotAntPar       = INT(ch_Grid:TextMatrix(3,ch_Grid:FixedCols - 1))
            BestHode.LevFargKod      = INPUT ArtBas.LevFargKod
            BestHode.LevKod          = INPUT ArtBas.LevKod
            BestHode.BestillingsDato = INPUT BestHode.BestillingsDato
            BestHode.LevDato         = INPUT BestHode.LevDato
            BestHode.BekreftetOrdre  = INPUT BestHode.BekreftetOrdre
            BestHode.BekreftetDato   = INPUT BestHode.BekreftetDato
            .
    FOR EACH btLevSort: /* WHERE btLevSort.AntSort > 0: */
        IF btLevSort.Fri = FALSE AND btLevSort.AntSort = 0 THEN DO:
            FIND BestSort OF BestHode WHERE BestSort.SortID = btLevSort.SortID
                                                            EXCLUSIVE no-error.
            IF AVAIL BestSort THEN
                DELETE BestSort.
        END.
        /* ELSE DO: */
        ELSE IF btLevSort.Fri = FALSE THEN DO:
            FIND BestSort OF BestHode WHERE BestSort.SortID = btLevSort.SortID
                                                            EXCLUSIVE no-error.
            IF NOT AVAIL BestSort THEN DO:
                CREATE BestSort.
                ASSIGN BestSort.BestNr = BestHode.Bestnr. /* från creatat BestHode */
            END.
            BUFFER-COPY btLevSort EXCEPT BestNr TO BestSort.
            RELEASE BestSort. /* notvendig ? */
        END.
    END.
    /* Om vi Har bytt lev måste eventuellt gamla bestsort tas bort */
    FOR EACH BestSort OF BestHode WHERE BestSort.Fri = FALSE:
        IF NOT CAN-FIND(LevSort WHERE LevSort.LevNr  = BestHode.LevNr AND
                        LevSort.SortId = BestSort.SortId) THEN
            DELETE BestSort.
    END.

    /* Om vi är i NY-modus */
    IF lNyBest THEN FOR EACH btLevSort: /* WHERE tLevSort.AntSort > 0: */
        ASSIGN btLevSort.BestNr = BestHode.BestNr.
    END.
    DO wIdx = 1 TO NUM-ENTRIES(wStorLekar," "):
        ASSIGN wFriFordeling = IF wFriFordeling = "" THEN String(wFriAntal[wIdx])
                               ELSE wFriFordeling + " " + String(wFriAntal[wIdx]).
    END.

    /* LOCKHANTERING !!!!! */
    IF lNyBest /* wBestHodeRecid = ? */ THEN DO:
        FOR EACH tBestKasse:
            ASSIGN tBestKasse.BestNr = IF tBestKasse.Antal > 0 THEN BestHode.BestNr ELSE 0.
        END.
        FOR EACH tFriButik:
            ASSIGN tFriButik.BestNr = BestHode.BestNr.
        END.
    END.
/*    ELSE do:
        FIND FIRST BestSort OF BestHode WHERE BestSort.Fri = true no-error.
    end.
    ASSIGN BestSort.Fordeling = wFriFordeling.
*/
   
/* Skapa BestLinje för Centrallager */
    FIND Butiker WHERE Butiker.Butik = wCentralLager NO-LOCK.
/*     IF wBestHodeRecid = ? THEN DO:                                                */
/*         CREATE BestLinje.                                                         */
/*         ASSIGN BestLinje.BestNr = BestHode.BestNr                                 */
/*                BestLinje.Butik  = wCentralLager.                                  */
/*         FIND Butiker WHERE Butiker.Butik = wCentralLager NO-LOCK.                 */
/*         CREATE BestPris.                                                          */
/*         ASSIGN BestPris.BestNr   = BestHode.BestNr                                */
/*                BestPris.BestStat = BestHode.BestStat                              */
/*                BestPris.ProfilNr = Butiker.ProfilNr. /* CENTRALLAGER */           */
/*         FIND ArtPris WHERE ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND             */
/*                            ArtPris.ProfilNr  = Butiker.ProfilNr NO-LOCK NO-ERROR. */
/*         IF AVAIL ArtPris THEN                                                     */
/*             ASSIGN wTillbudID            = IF ArtPris.Tilbud = TRUE THEN 2 ELSE 1 */
/*                    BestPris.DB%          = ArtPris.DB%[wTillbudID]                */
/*                    BestPris.DBKr         = ArtPris.DBKr[wTillbudID]               */
/*                    BestPris.DivKost%     = ArtPris.DivKost%[wTillbudID]           */
/*                    BestPris.DivKostKr    = ArtPris.DivKostKr[wTillbudID]          */
/*                    BestPris.EuroManuel   = ArtPris.EuroManuel                     */
/*                    BestPris.EuroPris     = ArtPris.EuroPris[wTillbudID]           */
/*                    BestPris.Frakt        = ArtPris.Frakt[wTillbudID]              */
/*                    BestPris.Frakt%       = ArtPris.Frakt%[wTillbudID]             */
/*                    BestPris.InnkjopsPris = ArtPris.InnkjopsPris[wTillbudID]       */
/*                    BestPris.Pris         = ArtPris.Pris[wTillbudID]               */
/*                    BestPris.Rab1%        = ArtPris.Rab1%[wTillbudID]              */
/*                    BestPris.Rab1Kr       = ArtPris.Rab1Kr[wTillbudID]             */
/*                    BestPris.Rab2%        = ArtPris.Rab2%[wTillbudID]              */
/*                    BestPris.Rab2Kr       = ArtPris.Rab2Kr[wTillbudID]             */
/*                    BestPris.Rab3%        = ArtPris.Rab3%[wTillbudID]              */
/*                    BestPris.Rab3Kr       = ArtPris.Rab3Kr[wTillbudID]             */
/*                    BestPris.MVAKr        = ArtPris.MVAKr[wTillbudID]              */
/*                    BestPris.MVA%         = ArtPris.MVA%[wTillbudID]               */
/*                    BestPris.ValPris      = ArtPris.ValPris[wTillbudID]            */
/*                    BestPris.VareKost     = ArtPris.VareKost[wTillbudID].          */
/*     END.                                                                          */
/*     ELSE                                                                          */
      FIND BestPris WHERE BestPris.BestNr = BestHode.BestNr AND
                          BestPris.BestStat = BestHode.BestStat AND
                          BestPris.ProfilNr = Butiker.ProfilNr NO-LOCK.

/* Skapa/uppdatera Beststr för Centrallager */
    DO wCol = ch_Grid:FixedCols TO ch_Grid:Cols - 1:
        ASSIGN wStorlek = ch_Grid:TextMatrix(0,wCol).
        FIND BestStr WHERE BestStr.BestNr   = BestHode.BestNr AND
                           BestStr.Butik    = wCentralLager   AND
                           BestStr.Storl    = wStorlek        AND
                           BestStr.BestStat = BestHode.BestStat NO-ERROR.
        IF NOT AVAIL BestStr AND INT(ch_Grid:TextMatrix(4,wCol)) > 0 THEN DO:
            Create Beststr.
            Assign BestStr.BestNr   = BestHode.BestNr
                   BestStr.Butik    = wCentralLager
                   BestStr.Storl    = wStorlek
                   BestStr.BestStat = BestHode.BestStat.
        END.
        IF AVAIL BestStr THEN
            ASSIGN BestStr.Bestilt = INT(ch_Grid:TextMatrix(4,wCol)).
    END.

/* Butiker utom Centrallager */
/* SKAPA BestLinje BestStr för andra butiker */

    DO wRow = 5 TO ch_Grid:Rows - 1:
        IF ch_Grid:TextMatrix(wRow,0) = "" THEN
            LEAVE.
        ASSIGN wButik = INT(ch_Grid:TextMatrix(wRow,0)).
        FIND BestLinje WHERE BestLinje.BestNr = BestHode.BestNr AND
                             BestLinje.Butik  = wButik NO-LOCK NO-ERROR.
        IF NOT AVAIL BestLinje THEN DO:
            CREATE BestLinje.
            ASSIGN BestLinje.BestNr = BestHode.BestNr
                   BestLinje.Butik  = wButik.

 /* Øppna opp vid flera prisprofiler
            FIND Butiker WHERE Butiker.Butik = wButik NO-LOCK.
            IF NOT CAN-FIND(BestPris OF BestHode WHERE BestPris.ProfilNr = Butiker.ProfilNr) THEN DO:
                CREATE BestPris.
                ASSIGN BestPris.BestNr   = BestHode.BestNr
                       BestPris.BestStat = BestHode.BestStat
                       BestPris.ProfilNr = Butiker.ProfilNr. 
            END.
 */
            IF INT(ch_Grid:TextMatrix(wRow,ch_Grid:FixedCols - 1)) = 0 THEN
                NEXT.
        END.

        DO wCol = ch_Grid:FixedCols TO ch_Grid:Cols - 1:
            ASSIGN wStorlek = ch_Grid:TextMatrix(0,wCol).
            FIND BestStr WHERE BestStr.BestNr   = BestHode.BestNr AND
                               BestStr.Butik    = wButik          AND
                               BestStr.Storl    = wStorlek        AND
                               BestStr.BestStat = BestHode.BestStat NO-ERROR.
            IF NOT AVAIL BestStr AND INT(ch_Grid:TextMatrix(wRow,wCol)) > 0 THEN DO:
                Create Beststr.
                Assign BestStr.BestNr   = BestHode.BestNr
                       BestStr.Butik    = wButik
                       BestStr.Storl    = wStorlek
                       BestStr.BestStat = BestHode.BestStat.
            END.
            IF AVAIL BestStr THEN DO:
                ASSIGN BestStr.Bestilt = INT(ch_Grid:TextMatrix(wRow,wCol)).
                RELEASE BestStr.
            END.

        END.
    END.
    /* Lagra BestKasse */
    FOR EACH InitBestKasse:
        DELETE InitBestKasse.
    END.
    FOR EACH tBestKasse WHERE tBestKasse.BestNr = BestHode.BestNr:
        FIND BestKasse WHERE BestKasse.BestNr = tBestKasse.BestNr   AND
                             BestKasse.SortID = tBestKasse.SortID   AND
                             BestKasse.Butik  = tBestKasse.Butik EXCLUSIVE NO-ERROR.
        IF AVAIL BestKasse AND tBestKasse.Antal = 0 THEN
            DELETE BestKasse.
        ELSE IF tBestKasse.Antal > 0 THEN DO:
            BUFFER-COPY tBestKasse TO BestKasse.
            RELEASE BestKasse.
            BUFFER-COPY tBestKasse TO InitBestKasse.
            RELEASE InitBestKasse.
        END.
    END.
    FOR EACH BestKasse OF BestHode WHERE NOT CAN-FIND(Levsort WHERE LevSort.LevNr = BestHode.Levnr AND
                                                      LevSort.SortId = BestKasse.SortId).
        DELETE BestKasse.
    END.
    /* Ordna upp i fributik */
    FOR EACH tFriButik WHERE tFriButik.BestNr = BestHode.BestNr AND
                             tFriButik.TotAntal = 0:
        DELETE tFriButik.
    END.
    FOR EACH FriButik WHERE FriButik.BestNr = BestHode.BestNr:
        DELETE FriButik.
    END.

/*    IF wDirekteLev = TRUE THEN DO:
        FIND tFriButik WHERE tFriButik.Butik = wCentralLager NO-ERROR.
        IF NOT AVAIL tFriButik THEN DO:
        CREATE tFriButik.
            ASSIGN tFriButik.BestNr   = BestHode.BestNr
                   tFriButik.Butik    = wCentralLager.
        END.
        ASSIGN tFriButik.TotAntal = 0.
        DO wIdx = 1 TO NUM-ENTRIES(wStorlekar," "):
            ASSIGN tFriButik.FriAntal[wIdx] = wFriAntal[wIdx]
                   tFriButik.TotAntal = tFriButik.TotAntal + wFriAntal[wIdx].
        END.
    END. */

    FOR EACH tFriButik:
        CREATE FriButik.
        BUFFER-COPY tFriButik TO FriButik.
        RELEASE FriButik.
    END.
    RUN PrisKalk("").    
    /* !!!!!!!!!!!! DENNA SKALL LIGGA SIST */
    FIND CURRENT BestHode NO-LOCK.
/*     ASSIGN wBestHodeRecid = RECID(BestHode). */

    /* !!!!!!!!!!!! */
    RUN SetWindowTitle.
    ASSIGN SELF:SENSITIVE = NO.
    RUN InitButtons.
    RUN SettModified(FALSE).
    {swn.i}
    APPLY "ENTRY" TO CtrlFrame.
/*     IF lNyDirekteIn = TRUE THEN DO:                                   */
/*         IF BUTTON-Godkann:SENSITIVE THEN                              */
/*             APPLY "CHOOSE" TO BUTTON-Godkann.                         */
/*         IF BUTTON-DirLev:SENSITIVE THEN                               */
/*             APPLY "CHOOSE" TO BUTTON-DirLev.                          */
/*         IF lNyDirekteIn = TRUE THEN /* Den kan ha blivit omflaggad */ */
/*             APPLY "CHOOSE" TO Btn_Done.                               */
/*     END.                                                              */
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-NegLev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-NegLev C-Win
ON CHOOSE OF BUTTON-NegLev IN FRAME DEFAULT-FRAME /* Negativ innleveranse */
DO:
  if wLapTop then
    do:
      message "Innleveranser kan ikke gjøres i innkjøpsystemet"
              view-as alert-box message title "Melding".
      return no-apply.
    end.
    IF ArtBas.IndividType > 0 THEN DO:
        MESSAGE "INDIVIDVARE" SKIP
                "Lageret vil bli justert." SKIP
                "Individregisteret påvirkes ikke og må justeres manuellt."
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
    END.
  assign
    wNegInnLev = true.
  apply "choose":U to BUTTON-DirLev.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokTeam
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokTeam C-Win
ON CHOOSE OF BUTTON-SokTeam IN FRAME DEFAULT-FRAME /* ... */
DO:
  DEFINE VARIABLE cTekst AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTeam  AS CHARACTER  NO-UNDO.
    IF NOT OKbyteTeam() THEN DO:
      MESSAGE "Du må slette all registrering på butiker." VIEW-AS ALERT-BOX
                 TITLE "Feil bytte av team".
      RETURN NO-APPLY.
    END.

    RUN gbutikkteam (
      INPUT-OUTPUT cTekst, /* Returstreng - chr(1) separert */
      "", /* Feltliste avgrensningsfelt (kommaseparert) */
      "", /* Feltverdier (chr(1) sep) */ 
      "",
      wBrGrpNr, /* brukergruppe */
      1         /* teamtypeid */
/*       CB-Team:SCREEN-VALUE /* Post markøren skal stå på */ */
      ).
    IF RETURN-VALUE = "AVBRYT" THEN
        RETURN NO-APPLY.
    IF NUM-ENTRIES(cTekst,CHR(1)) >= 3 THEN
    DO:
        /* Legger opp verdier I de aktuelle feltene */
        ASSIGN cTeam = ENTRY(3,cTekst,CHR(1))
               CB-Team:SCREEN-VALUE = cTeam.
        APPLY "VALUE-CHANGED" TO CB-Team.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Sortiment
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Sortiment C-Win
ON CHOOSE OF BUTTON-Sortiment IN FRAME DEFAULT-FRAME /* Sortiment... */
DO:
  DEF VAR wLevSort AS RECID NO-UNDO.
  RUN d-vLevSort.w (INPUT-OUTPUT wLevSort,"NY",RECID(LevBas),ArtBas.StrTypeID).
  IF RETURN-VALUE <> "Avbryt" AND wLevSort <> ? THEN DO:
      FIND LevSort WHERE RECID(LevSort) = wLevSort NO-LOCK.
      RUN SkapatLevSort("VEDLIKEHOLD").
      IF RETURN-VALUE <> "FEIL" THEN DO:
          {&OPEN-QUERY-BROWSE-1}
      END.
      ELSE
          MESSAGE "I sortiment " LevSort.SortID " finnes størrelse(r) som ikke" SKIP
                  "eksisterer i størrelsesdefinisjonen (for denne bestillingen)."
                   view-as alert-box INFORMATION TITLE "".
  END.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-Lev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Lev C-Win
ON VALUE-CHANGED OF CB-Lev IN FRAME DEFAULT-FRAME
DO:
    ASSIGN INPUT CB-Lev
           BUTTON-Lagra:SENSITIVE = TRUE.
    FIND LevBas WHERE LevBas.LevNr = CB-Lev NO-LOCK.
    RUN AltLevChg.
/*   MESSAGE  CB-Lev SKIP                       */
/*            "Här byter vi levsortiment." SKIP */
/*            "och lev på bestilling."          */
/*       VIEW-AS ALERT-BOX INFO BUTTONS OK.     */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-Team
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Team C-Win
ON VALUE-CHANGED OF CB-Team IN FRAME DEFAULT-FRAME /* Team */
DO: 
    IF NOT OKbyteTeam() THEN DO:
/*     IF TRIM(ch_Grid:TextMatrix(ch_Grid:FixedRows - 1,ch_Grid:FixedCols - 1)) <>       */
/*        TRIM(ch_Grid:TextMatrix(ch_Grid:FixedRows - 2,ch_Grid:FixedCols - 1)) THEN DO: */
        MESSAGE "Du må slette all registrering på butiker." VIEW-AS ALERT-BOX
                   TITLE "Feil bytte av team".
        ASSIGN SELF:SCREEN-VALUE = wCBValg.
        RETURN NO-APPLY.
    END.
    ASSIGN wCBValg = SELF:SCREEN-VALUE.
    RUN InitButiker.
    ASSIGN ch_Grid:Cell(2,1,1,ch_Grid:Rows - 1, ch_Grid:Cols - 1) = 7.
    RUN PyjamasFarg.
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
DEFINE VARIABLE               wCellValue    AS INTE    NO-UNDO.
DEFINE VARIABLE               wButik        LIKE Butiker.Butik NO-UNDO.
DEFINE VARIABLE               wIdx          AS INTE    NO-UNDO.
DEFINE VARIABLE               wChange       AS INTE    NO-UNDO.

IF ch_Grid:TextMatrix(5,0) = "" OR BestHode.DirekteLev:CHECKED IN FRAME DEFAULT-FRAME THEN
    RETURN NO-APPLY.
ELSE IF wBestHodeRecid <> ? AND BestHode.BestStat > 4 THEN
    RETURN NO-APPLY.

IF p-KeyAscii = 13 THEN DO:
    IF ch_Grid:Col = ch_Grid:Cols - 1 AND ch_Grid:Row = ch_Grid:Rows - 1 THEN 
        RETURN NO-APPLY.
    IF ch_Grid:Col < ch_Grid:Cols - 1 THEN DO:
      ASSIGN ch_Grid:Col = ch_Grid:Col + 1.
      IF ch_Grid:Col >= wSHC# + ch_Grid:FixedCols AND ch_Grid:LeftCol < 
                ch_Grid:Col - wSHC# + 1 + ch_Grid:FixedCols THEN
      ch_Grid:LeftCol = ch_Grid:Col - wSHC# + 1 + ch_Grid:FixedCols.
    END.
    else IF ch_Grid:Row < ch_Grid:Rows - 1 then
            ASSIGN ch_Grid:Col = ch_Grid:FixedCols
                   ch_Grid:Row = ch_Grid:Row + 1
                   ch_Grid:LeftCol = ch_Grid:FixedCols.
                         
    RETURN NO-APPLY.
END.
IF p-KeyAscii = 8 OR p-KeyAscii = 32 OR (p-KeyAscii >= 43 AND p-KeyAscii <= 45) OR
                     (p-KeyAscii >= 48 AND p-KeyAscii <= 57)     THEN DO:
    IF p-KeyAscii = 8 OR p-KeyAscii = 44 THEN DO:
        IF INT(ch_Grid:Text) = 0 THEN
            RETURN NO-APPLY.
        ASSIGN wCentralValue = INT(ch_Grid:TextMatrix(4,ch_Grid:Col))
               wCellValue    = INT(ch_Grid:Text).
        ASSIGN wChange = TRUNC(INT(TRIM(ch_Grid:Text)) / 10, 0) - INT(TRIM(ch_Grid:Text)).
    END.
    ELSE IF p-KeyAscii = 32 AND ch_Grid:Text <> "" THEN DO:
        RUN PopUpChoose("m_delruta").
        RETURN NO-APPLY.
    END.
    ELSE IF p-KeyAscii = 43 THEN DO: /* + */
        IF INT(ch_Grid:TextMatrix(4,ch_Grid:Col)) = 0 OR INT(ch_Grid:Text) = 9999 THEN 
            RETURN NO-APPLY.
        ASSIGN wChange = 1.
    END.
    ELSE IF p-KeyAscii = 45 THEN DO: /* - */
        IF INT(ch_Grid:Text) = 0 THEN
            RETURN NO-APPLY.
        ASSIGN wChange = -1.
    END.
    ELSE IF p-KeyAscii > 48 OR (ch_Grid:Text > "" AND p-KeyAscii = 48) THEN DO:
        ASSIGN wCentralValue = INT(ch_Grid:TextMatrix(4,ch_Grid:Col))
               wCellValue    = INT(ch_Grid:Text).
        IF 10 * wCellValue - wCellValue + p-KeyAscii - 48 > wCentralValue THEN
            RETURN NO-APPLY.
        ASSIGN wChange = (INT(ch_Grid:Text) * 10 + p-KeyAscii - 48) - INT(ch_Grid:Text).
    END.
    ASSIGN ch_Grid:Text = STRING(INT(ch_Grid:Text) + wChange) + " "
           ch_Grid:Text = IF INT(ch_Grid:Text) = 0 THEN "" ELSE ch_Grid:Text
           ch_Grid:TextMatrix(ch_Grid:Row,ch_Grid:FixedCols - 1) = 
                           STRING(INT(ch_Grid:TextMatrix(ch_Grid:Row,ch_Grid:FixedCols - 1)) + wChange) + " "
           ch_Grid:TextMatrix(4,ch_Grid:Col) = STRING(INT(ch_Grid:TextMatrix(4,ch_Grid:Col)) - wChange) + " "
           ch_Grid:TextMatrix(4,ch_Grid:FixedCols - 1) = STRING(INT(ch_Grid:TextMatrix(4,ch_Grid:FixedCols - 1)) - wChange) + " ".
    ASSIGN BUTTON-Lagra:SENSITIVE   IN FRAME {&FRAME-NAME} = YES
           BUTTON-Kalkyl:SENSITIVE  IN FRAME {&FRAME-NAME} = NO
           BUTTON-Godkann:Sensitive IN FRAME {&FRAME-NAME} = NO.
    ASSIGN CB-Lev:SENSITIVE = NOT (AVAIL BestHode AND BestHode.BestStat > 2 OR 
                               NOT NUM-ENTRIES(CB-Lev:LIST-ITEM-PAIRS) > 2 OR harBestillt()).
END.
ELSE
    RETURN NO-APPLY.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame C-Win OCX.KeyUp
PROCEDURE CtrlFrame.VSFlexGrid.KeyUp .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  Required for OCX.
    KeyCode
    Shift
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT-OUTPUT PARAMETER p-KeyCode AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER p-Shift   AS INTEGER NO-UNDO.
/* Om ALT-L */
IF BUTTON-Lagra:SENSITIVE IN FRAME DEFAULT-FRAME AND p-KeyCode = 76 AND p-Shift = 4 THEN
    APPLY "CHOOSE" TO BUTTON-Lagra.
RETURN NO-APPLY.


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

DEFINE INPUT PARAMETER p-Button AS INTEGER          NO-UNDO.
DEFINE INPUT PARAMETER p-Shift  AS INTEGER          NO-UNDO.
DEFINE INPUT PARAMETER p-x      AS INTEGER          NO-UNDO.
DEFINE INPUT PARAMETER p-y      AS INTEGER          NO-UNDO.
DEFINE             VAR wIdx     AS INTEGER          NO-UNDO.
DEFINE             VAR wButik   LIKE Butiker.Butik  NO-UNDO.
DEFINE             VAR wMin     AS CHAR             NO-UNDO.
DEFINE             VAR wAktuell AS CHAR             NO-UNDO.
IF ch_Grid:MouseRow < 0 OR ch_Grid:TextMatrix(ch_Grid:MouseRow,0) = "" THEN
    RETURN NO-APPLY.
ELSE IF wBestHodeRecid <> ? AND BestHode.BestStat > 4 THEN
    RETURN NO-APPLY.

IF p-Button = 2 AND ch_Grid:MouseCol > ch_Grid:FixedCols - 1 AND
                    ch_Grid:MouseRow > ch_Grid:FixedRows - 1 THEN DO:
    ASSIGN ch_Grid:Col = ch_Grid:MouseCol
           ch_Grid:Row = ch_Grid:MouseRow
           wFstRow     = ch_Grid:Row
           wLstRow     = ch_Grid:Row
           wFstCol     = ch_Grid:Col
           wLstCol     = ch_Grid:Col.
          
    Run FixPopUp("En").
    APPLY "GO" to frame frame-a.
END.
ELSE IF p-Button = 1 AND (ch_Grid:MouseRow = 2 OR (ch_Grid:MouseRow >= 4 AND ch_Grid:MouseCol = 0)) then do:
    IF NOT lCanUpdateCL AND ch_Grid:MouseRow < 5 THEN
        RETURN.
    ELSE IF NOT CAN-DO(cBrukerButTilgang,TRIM(ch_Grid:TextMatrix(ch_Grid:MouseRow,0))) THEN
        RETURN.
    ASSIGN wAktivFriRad = IF ch_Grid:MouseRow = 2 THEN 4 ELSE ch_Grid:MouseRow.
           wButik = IF wAktivFriRad = ch_Grid:FixedRows - 1 THEN wCentrallager 
                                                            ELSE INT(ch_Grid:TextMatrix(ch_Grid:MouseRow,0)).
    IF NOT BestHode.DirekteLev:CHECKED IN FRAME DEFAULT-FRAME THEN DO:
        FIND tFriButik WHERE tFriButik.Butik = wCentrallager NO-ERROR.
        IF NOT AVAIL tFriButik THEN DO:
            CREATE tFriButik.
            ASSIGN tFriButik.BestNr = IF wBestHodeRecid = ? THEN ? ELSE
                                          BestHode.BestNr
                   tFriButik.Butik  = wCentralLager.
        END.
        IF wAktivFriRad = 2 OR wAktivFriRad = ch_Grid:FixedRows - 1 THEN DO:
            DO wIdx = 1 TO NUM-ENTRIES(wStorlekar," "):
                ASSIGN wAktuell = wAktuell + (IF wAktuell = "" THEN "" ELSE " ") + STRING(tFributik.FriAntal[wIdx])
                       wMin     = wMin + (IF wMin = "" THEN "" ELSE " ") + 
                                IF INT(ch_Grid:TextMatrix(4,wIdx + ch_Grid:FixedCols - 1)) >= tFributik.FriAntal[wIdx] THEN "0"
                                ELSE STRING(tFributik.FriAntal[wIdx] - INT(ch_Grid:TextMatrix(4,wIdx + ch_Grid:FixedCols - 1))).
            END.
        END.
        ELSE DO:
            ASSIGN wAktuell = FILL("0 ",NUM-ENTRIES(wStorlekar," ") - 1) + "0"
                   wMin     = FILL("0 ",NUM-ENTRIES(wStorlekar," ") - 1) + "0".
        END.
    END.
    ELSE IF (BestHode.DirekteLev:CHECKED IN FRAME DEFAULT-FRAME /* AND 
             BestHode.DirekteLev:SENSITIVE IN FRAME DEFAULT-FRAME */) AND ch_Grid:MouseRow >= ch_Grid:FixedRows - 1 THEN DO:
        FIND tFriButik WHERE tFriButik.Butik = wButik NO-LOCK NO-ERROR.
        IF NOT AVAIL tFriButik THEN DO:
            CREATE tFriButik.
            ASSIGN tFriButik.BestNr   = IF wBestHodeRecid = ? THEN 1 ELSE BestHode.BestNr
                   tFriButik.Butik    = wButik.
        END.
        DO wIdx = 1 TO NUM-ENTRIES(wStorlekar," "):
           ASSIGN wAktuell = wAktuell + (IF wAktuell = "" THEN "" ELSE " ") + STRING(tFributik.FriAntal[wIdx])
                  wMin     = wMin + (IF wMin = "" THEN "" ELSE " ") + "0".
       END.
    END. 
    ELSE
        LEAVE.
    RUN d-FriGrid.w(THIS-PROCEDURE,wStorlekar,wMin,wAktuell).
/*     APPLY "CHOOSE" TO BUTTON-Lagra. */
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

    IF ch_Grid:Rows = ch_Grid:FixedRows OR ch_Grid:TextMatrix(5,0) = "" THEN
        RETURN NO-APPLY.
    ELSE IF wBestHodeRecid <> ? AND BestHode.BestStat > 4 THEN
        RETURN NO-APPLY.

/*    IF ch_Grid:Row = ch_Grid:RowSel AND ch_Grid:Col = ch_Grid:ColSel THEN DO:
        ASSIGN wFstRow = ch_Grid:Row
               wLstRow = ch_Grid:Row.
        Run FixPopUp("En").
    END.
    ELSE DO: */
    IF ch_Grid:Row <> ch_Grid:RowSel OR ch_Grid:Col <> ch_Grid:ColSel THEN DO:
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
ON TAB OF BestHode.DirekteLev IN FRAME DEFAULT-FRAME /* Dir.levert */
or "RETURN":U of BestHode.DirekteLev
DO:
  apply "entry":U to TOGGLE-Tillagg in frame DEFAULT-FRAME.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BestHode.DirekteLev C-Win
ON VALUE-CHANGED OF BestHode.DirekteLev IN FRAME DEFAULT-FRAME /* Dir.levert */
DO:
    DEF VAR wIdx AS INTE NO-UNDO.
    DEF VAR wRow AS INTE NO-UNDO.
    /*
    IF SELF:CHECKED THEN DO:
        IF NOT OkDirektLev() THEN DO:
            ASSIGN SELF:CHECKED = FALSE.
            MESSAGE "Beställningsmatrisen måste tömmas." VIEW-AS ALERT-BOX.
            RETURN NO-APPLY.
        END.
        FOR EACH tLevSort WHERE tLevSort.Fri = FALSE:
            CREATE tBestKasse.
            ASSIGN tBestKasse.BestNr   = IF wBestHodeRecid = ? THEN ? ELSE BestHode.BestNr
                   tBestKasse.SortID   = tLevSort.SortID
                   tBestKasse.Butik    = wCentralLager
                   tBestKasse.Antal    = tLevSort.AntSort.
            RELEASE tBestKasse.                  
        END.
        IF INT(ch_Grid:TextMatrix(2,2)) > 0 THEN
            ASSIGN ch_Grid:Cell(7,ch_Grid:FixedRows - 1,0,ch_Grid:FixedRows - 1,0) = wFriFGCol.
    END. 
    */
    IF NOT OkDirektLev() THEN DO:
        ASSIGN SELF:CHECKED = NOT SELF:CHECKED.
        MESSAGE "Bestillingsmatrisen må tømmes." VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    IF SELF:CHECKED THEN DO:
        FOR EACH tLevSort WHERE tLevSort.Fri = FALSE:
            CREATE tBestKasse.
            ASSIGN tBestKasse.BestNr   = IF wBestHodeRecid = ? THEN ? ELSE BestHode.BestNr
                   tBestKasse.SortID   = tLevSort.SortID
                   tBestKasse.Butik    = wCentralLager
                   tBestKasse.Antal    = tLevSort.AntSort.
            RELEASE tBestKasse.                  
        END.
        IF INT(ch_Grid:TextMatrix(2,2)) > 0 THEN
            ASSIGN ch_Grid:Cell(7,ch_Grid:FixedRows - 1,0,ch_Grid:FixedRows - 1,0) = wFriFGCol.
    END. 
    ELSE DO:
        FOR EACH tBestKasse:
            DELETE tBestKasse.
        END.
/*
        FOR EACH tFriButik WHERE tFriButik.Butik <> wCentralLager:
            DELETE tFriButik.
        END.
*/
        FOR EACH tFriButik:
            DELETE tFriButik.
        END.
        DO wRow = ch_Grid:FixedRows - 1 TO ch_Grid:Rows - 1:
            ASSIGN ch_Grid:Cell(7,wRow,0,wRow,0) = wOrgFriFGCol.
        END.
        IF INT(ch_Grid:TextMatrix(2,2)) > 0 THEN DO:
            CREATE tFriButik.
            ASSIGN tFriButik.BestNr = IF wBestHodeRecid = ? THEN ? ELSE BestHode.BestNr
                   tFriButik.Butik  = wCentralLager.
            DO wIdx = 1 TO NUM-ENTRIES(wStorLekar," "):
                ASSIGN tFriButik.FriAntal[wIdx] = wFriAntal[wIdx]
                       tFriButik.TotAntal = tFriButik.TotAntal + wFriAntal[wIdx].
            END.
        END.
        RUN VisKasser.
    END.
    BUTTON-Lagra:SENSITIVE IN FRAME DEFAULT-FRAME = TRUE.
    APPLY "VALUE-CHANGED" TO BROWSE {&BROWSE-NAME}.
    RETURN NO-APPLY.
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


&Scoped-define SELF-NAME BestHode.LevDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BestHode.LevDato C-Win
ON TAB OF BestHode.LevDato IN FRAME DEFAULT-FRAME /* Leveringsdato */
OR RETURN OF Besthode.Levdato DO:
    IF BROWSE-1:FOCUSED-ROW <> ? THEN
        APPLY "ENTRY" TO BROWSE-1.
    ELSE
        APPLY "ENTRY" TO BestHode.Beskrivelse.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BestHode.LevDato C-Win
ON VALUE-CHANGED OF BestHode.LevDato IN FRAME DEFAULT-FRAME /* Leveringsdato */
DO:
    IF wBestHodeRecid <> ? THEN 
    DO:
      IF BestHode.BestStat <= 4 THEN
          BUTTON-Lagra:SENSITIVE = TRUE.
    END.
END.

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


&Scoped-define SELF-NAME m_delomr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_delomr C-Win
ON CHOOSE OF MENU-ITEM m_delomr /* Rensa markerat område */
DO:
    RUN PopUpChoose(SELF:NAME).
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_delraden
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_delraden C-Win
ON CHOOSE OF MENU-ITEM m_delraden /* Rensa hela raden */
DO:
    RUN PopUpChoose(SELF:NAME).
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_delruta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_delruta C-Win
ON CHOOSE OF MENU-ITEM m_delruta /* Rensa aktuell ruta */
DO:
    RUN PopUpChoose(SELF:NAME).
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_delsortering
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_delsortering C-Win
ON CHOOSE OF MENU-ITEM m_delsortering /* Ta bort sortering */
DO:
    RUN PopUpChoose(SELF:NAME).
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_delstaket
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_delstaket C-Win
ON CHOOSE OF MENU-ITEM m_delstaket /* Ta bort 1 i område */
DO:
    RUN PopUpChoose(SELF:NAME).
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_NylevSort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_NylevSort C-Win
ON CHOOSE OF MENU-ITEM m_NylevSort /* Nytt sortiment... */
DO:
    APPLY "CHOOSE" TO BUTTON-Sortiment IN FRAME DEFAULT-FRAME.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_nysortering
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_nysortering C-Win
ON CHOOSE OF MENU-ITEM m_nysortering /* Lägg till sortering */
DO:
  RUN PopUpChoose(SELF:NAME).
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_nystaket
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_nystaket C-Win
ON CHOOSE OF MENU-ITEM m_nystaket /* Lägg till 1 i område */
DO:
    RUN PopUpChoose(SELF:NAME).
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_n_nysortering
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_n_nysortering C-Win
ON CHOOSE OF MENU-ITEM m_n_nysortering /* Lägg till (n) sortering */
DO:
    RUN PopUpChoose(SELF:NAME).
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_n_nystaket
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_n_nystaket C-Win
ON CHOOSE OF MENU-ITEM m_n_nystaket /* Lägg till (n) i område */
DO:
    RUN PopUpChoose(SELF:NAME).
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOGGLE-Tillagg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOGGLE-Tillagg C-Win
ON TAB OF TOGGLE-Tillagg IN FRAME DEFAULT-FRAME /* Supplering */
or "RETURN":U of TOGGLE-Tillagg
DO:
  apply "entry":U to BestHode.Merknad in frame DEFAULT-FRAME.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOGGLE-Tillagg C-Win
ON VALUE-CHANGED OF TOGGLE-Tillagg IN FRAME DEFAULT-FRAME /* Supplering */
DO:
    IF wBestHodeRecid <> ? THEN 
    DO:
      IF BestHode.BestStat <= 4 THEN
          BUTTON-Lagra:SENSITIVE = TRUE.
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
/* IF wModus = "NYDIREKTE" THEN    */
/*     ASSIGN wModus       = "NY"  */
/*            lNyDirekteIn = TRUE. */
{inutmld.i &Modus = "Opprett"} /* Melder fra at programmet har startet. */

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
{genlib.i
  &NoLibCall      = "Nei"
  &WindowName     = "Registrering av bestillinger"
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
                            ch_Grid     = ?. 
                     PUBLISH 'RefreshBest' (INPUT wModus).
                   "
}                     
     
        

ON ENTRY OF tLevSort.AntSort IN BROWSE BROWSE-1 DO:
  IF AVAILABLE tLevSort
      THEN ASSIGN wBest = tLevSort.AntSort.
END.
ON LEAVE OF tLevSort.AntSort IN BROWSE BROWSE-1 DO:
    ASSIGN wKassNy = INT(tLevSort.AntSort:SCREEN-VALUE IN BROWSE BROWSE-1).
    IF wKassNy <> wBest THEN DO:
        RUN ChgKasse(wKassNy - wBest,NO).
        IF wBestHodeRecid = ? AND INT(ch_Grid:TextMatrix(3,ch_Grid:FixedCols - 1)) = 0 THEN
            ASSIGN BUTTON-Lagra:SENSITIVE   IN FRAME {&FRAME-NAME} = NO
                   BUTTON-Godkann:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
        ELSE
            ASSIGN BUTTON-Lagra:SENSITIVE   IN FRAME {&FRAME-NAME} = YES                   
                   BUTTON-Kalkyl:SENSITIVE  IN FRAME {&FRAME-NAME} = NO
                   BUTTON-Godkann:SENSITIVE IN FRAME {&FRAME-NAME} = NO
                   BUTTON-FraBest:SENSITIVE IN FRAME {&FRAME-NAME} = NO
                   BUTTON-NegLev:SENSITIVE  IN FRAME {&FRAME-NAME} = NO
                   BUTTON-Dirlev:SENSITIVE  IN FRAME {&FRAME-NAME} = NO.
    END.
    ASSIGN wBest = wKassny.
    ASSIGN CB-Lev:SENSITIVE = NOT (AVAIL BestHode AND BestHode.BestStat > 2 OR 
                           NOT NUM-ENTRIES(CB-Lev:LIST-ITEM-PAIRS) > 2 OR harBestillt()).
END.
/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

{syspara.i 5 4 2 wStrTypeLst}
{syspara.i 5 4 8 cPassordkrav}
{syspara.i 1 2 3 wEDB-System}
{syspara.i 1 1 18 cHKinst}
{syspar2.i 5 4 5 cBrukButikEtikett}
{syspara.i 5 4 16 cManRegEAN}
{syspara.i 5 4 17 cInnlevSamlet}

/* Styrer defaultverdi på direktelevert flagget for lokalt hk. */

{syspara.i 5 4 12 cTekst}
IF CAN-DO('Ja,Yes,1,true',cTekst) THEN
    lNyDirektexIn = TRUE.
ELSE
    lNyDirektexIn = FALSE.
    
if wEDB-System = "" then
  wEDB-System = "OVERFOR-LOCK".
    IF cHKinst = "" THEN
        ASSIGN cHKinst = "no".

/* Sjekker om det kjøres på en LapTop */
if valid-handle(wLibHAndle) then
  run SjekkLapTop in wLibHandle (output wLapTop).

assign
  wNegInnLev = false.

on ALT-L of C-Win anywhere
  do:
    apply "CHOOSE":U to BUTTON-Lagra IN frame DEFAULT-FRAME.
  end.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  IF wBestHodeRecid <> ? AND wModus = "SLETT" THEN DO:
      RUN DelBestHode.
      IF RETURN-VALUE = "OK" THEN
          wBestHodeRecid = ?.
      RETURN NO-APPLY "AVBRYT".
  END.

/*   SUBSCRIBE TO "InitStringTranslation" ANYWHERE.  */

  FIND Bruker WHERE Bruker.BrukerId = USERID("skotex") NO-LOCK.
  IF AVAIL Bruker THEN DO:
      FIND BrukerGrp WHERE BrukerGrp.BrGrpNr = Bruker.BrGrpNr NO-LOCK NO-ERROR.
      IF NOT AVAIL BrukerGrp THEN DO:
          MESSAGE "Feilaktig brukergruppe." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY "AVBRYT".
      END.
      ASSIGN wBrGrpNr = BrukerGrp.BrGrpNr.
  END.
  FIND ArtBas WHERE RECID(ArtBas) = wArtBasRecid NO-LOCK NO-ERROR.
  IF NOT AVAILABLE ArtBas THEN DO:
    MESSAGE "Artikkelen finnes ikke." VIEW-AS ALERT-BOX ERROR.
    RETURN NO-APPLY "AVBRYT".
  END.
/* 1 Gammal */
/*   find LevBas no-lock where                                      */
/*     LevBas.LevNr = ArtBas.LevNr no-error.                        */
/*   IF NOT AVAILABLE LevBas THEN DO:                               */
/*     MESSAGE "Leverandøren finnes ikke." VIEW-AS ALERT-BOX ERROR. */
/*     RETURN NO-APPLY "AVBRYT".                                    */
/*   END.                                                           */
  
  IF can-do(wStrTypeLst,string(ArtBas.StrTypeID)) THEN DO:
      MESSAGE "Feil størrelsestype." skip
              "Størrelsestype " + string(ArtBas.StrTypeId) + 
              " er ikke tillatt." VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY.
  END.
  IF NOT CAN-FIND(FIRST strtstr WHERE strtstr.strtypeid = Artbas.strtypeid) THEN DO:
      MESSAGE "Feil i størrelsetypen," skip
              "størrelser mangler." VIEW-AS ALERT-BOX ERROR 
                                        TITLE "Feil i størrelsetypen".
      RETURN NO-APPLY "AVBRYT".
  END.
  IF wBestHodeRecid <> ? THEN DO:
      FIND BestHode WHERE RECID(BestHode) = wBestHodeRecid NO-LOCK NO-ERROR.
      IF NOT AVAIL BestHode THEN DO:
          MESSAGE "Bestillningen finnes ikke." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY "AVBRYT".
      END.
      assign
        T-LapTop = BestHode.LapTop
        lNyDirektexIn = BestHode.DirekteLev
        .
  END.
  else assign T-LapTop = wLapTop.
  /*  1: Ny */
  find LevBas no-lock where
    LevBas.LevNr = IF AVAIL BestHode THEN BestHode.LevNr ELSE ArtBas.LevNr no-error.
  IF NOT AVAILABLE LevBas THEN DO:
    MESSAGE "Leverandøren finnes ikke." VIEW-AS ALERT-BOX ERROR.
    RETURN NO-APPLY "AVBRYT".
  END.
  display T-LapTop with frame DEFAULT-FRAME.

  {syspara.i 5 2 99 wStatus}
  IF wBestHodeRecid = ? THEN DO:
      RUN InitCB.
      ASSIGN BestHode.BestillingsDato:SCREEN-VALUE = STRING(today).
  END.
  RUN FillCBAltLev.

  /* Finn Butik som är centrallager !! flyttat från initialize-controls. */
  IF wBestHodeRecid = ? AND cHKinst = "yes" THEN DO:
      ASSIGN lCanUpdateCL = TRUE
             cUpdateButListe = "*".
      RUN HK_VelgTeam.
      IF RETURN-VALUE = "AVBRYT" THEN
          RETURN NO-APPLY "AVBRYT".
  END.
  ELSE DO:
      {syspara.i 5 1 1 wCentralLager INT}
      ASSIGN lCanUpdateCL = CAN-FIND(ButikkTilgang WHERE ButikkTilgang.BrGrpNr = wBrGrpNr AND 
                                     ButikkTilgang.Butik   = wCentralLager).
  END.

  RUN SetWindowTitle.
  RUN InitSortiment.
  ASSIGN CB-Lev                    = LevBas.LevNr
         ArtBas.LevKod:TOOLTIP     = ArtBas.LevKod
         ArtBas.LevFargKod:TOOLTIP = ArtBas.LevFargKod
         LevBas.LevNamn:TOOLTIP    = LevBas.LevNamn.
  RUN enable_UI. 
  ASSIGN CB-Lev = LevBas.LevNr.
  CB-Lev:MOVE-TO-TOP().
  IF AVAIL BestHode AND BestHode.BestStat > 2 OR NOT NUM-ENTRIES(CB-Lev:LIST-ITEM-PAIRS) > 2 OR harBestillt() THEN
      ASSIGN CB-Lev:SENSITIVE = FALSE.
  {lng.i}
  view frame DEFAULT-FRAME.
  
  IF AVAIL BestHode AND BestHode.BestStat > 4 THEN
      ASSIGN BestHode.BekreftetOrdre:SENSITIVE = FALSE.

  IF cHKinst = "yes" THEN DO:
      ASSIGN BestHode.DirekteLev:CHECKED   = TRUE
             BestHode.DirekteLev:SENSITIVE = FALSE
             CB-Team:SCREEN-VALUE          = ENTRY(1,CB-Team:LIST-ITEMS)
             CB-Team:SENSITIVE             = FALSE
             BUTTON-SokTeam:SENSITIVE      = FALSE.
      IF wBestHodeRecid = ? THEN
          APPLY "VALUE-CHANGED" TO BestHode.DirekteLev.
  END.
  ELSE DO:
          ASSIGN 
              BestHode.DirekteLev:CHECKED   = lNyDirektexIn
              BestHode.DirekteLev:SENSITIVE = TRUE.

  END.

  IF wBestHodeRecid = ? AND NOT lCanUpdateCL THEN
      ASSIGN BestHode.DirekteLev:CHECKED   = TRUE
             BestHode.DirekteLev:SENSITIVE = FALSE.

  APPLY "VALUE-CHANGED" TO BROWSE {&BROWSE-NAME}.
  IF wBestHodeRecid <> ? THEN DO:
      RUN InitButtons.
      ASSIGN TOGGLE-Tillagg:CHECKED =  BestHode.BestType = 2.
  END.
  ELSE
      ASSIGN BUTTON-Lagra:SENSITIVE IN FRAME DEFAULT-FRAME = TRUE.

  ASSIGN tLevSort.AntSort:READ-ONLY IN BROWSE {&BROWSE-NAME} = YES.
/*   IF lNyDirekteIn = TRUE THEN                    */
/*       ASSIGN BestHode.Direktelev:CHECKED = TRUE. */
 /* Run FixPopUp("En"). */ /* Initiering av popupmeny */
  RUN VisArtBas.
  
  run VisBilde (1).

  IF AVAIL BestHode THEN
      RUN PrisKalk("INIT").  
  IF CAN-FIND(FIRST tLevSort WHERE tLevSort.Fri = FALSE) THEN DO:
      apply "ENTRY":U to BROWSE {&BROWSE-NAME}.
      BROWSE {&BROWSE-NAME}:SELECT-FOCUSED-ROW().
      APPLY "VALUE-CHANGED" TO BROWSE {&BROWSE-NAME}.
  END.
  APPLY "ENTRY" TO BestHode.LevDato.
  RUN SettModified("FALSE").
  IF wModus = "TOM" THEN DO:
      APPLY "CHOOSE" TO BUTTON-Lagra.
      MESSAGE "En tom bestilling er registrert."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY "CLOSE" TO THIS-PROCEDURE.
      RETURN NO-APPLY.
  END.

  IF cHKinst = "yes" AND bruker.brukertype <> 1 THEN DO:
      ASSIGN FRAME DEFAULT-FRAME:SENSITIVE = FALSE.
  END.
  ELSE DO:
      IF NOT lCanUpdateCL AND AVAIL BestHode AND BestHode.DirekteLev = FALSE THEN DO:
          MESSAGE "Bestillingen tillhører sentrallageret. Oppdatering ikke tillatt."
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          APPLY "CLOSE" TO THIS-PROCEDURE.
          RETURN NO-APPLY.
    /*       APPLY "CLOSE" TO THIS-PROCEDURE. */
      END.
      ELSE IF NOT lCanUpdateCL AND AVAIL BestHode AND BestHode.DirekteLev = TRUE THEN DO:
          RUN BrukerButTilgang.
      END.
      ELSE
          ASSIGN cBrukerButTilgang = "*".
  END.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AltLevChg C-Win 
PROCEDURE AltLevChg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH FritLevSort:
        DELETE FritLevSort.
    END.
    FOR EACH tLevSort:
        DELETE tLevSort.
    END.
    FOR EACH tBestKasse:
        DELETE tBestKasse.
    END.
    FOR EACH tFributik:
        DELETE tFributik.
    END.
    RUN InitSortiment.
    {&OPEN-QUERY-{&BROWSE-NAME}}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Apply-mouse-menu-click C-Win 
PROCEDURE Apply-mouse-menu-click :
/*------------------------------------------------------------------------------
Purpose:     Programatic click the right mouse button on a widget
Parameters:  Widget-handle on which you want to click
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER  p-wh   AS WIDGET-HANDLE  NO-UNDO.
   DEF VAR ReturnValue AS INTEGER NO-UNDO.
   RUN SendMessage{&A} in hpApi (INPUT p-wh:HWND, 
                                 INPUT {&WM_RBUTTONDOWN},
                                 INPUT {&MK_RBUTTON},
                                 INPUT 0,
                                 OUTPUT ReturnValue).
   RUN SendMessage{&A} in hpApi (INPUT p-wh:HWND, 
                                 INPUT {&WM_RBUTTONUP},
                                 INPUT 0, 
                                 INPUT 0,
                                 OUTPUT ReturnValue).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BrukerButTilgang C-Win 
PROCEDURE BrukerButTilgang :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FOR EACH ButikkTeam WHERE ButikkTeam.BrGrpNr = wBrGrpNr AND
                            ButikkTeam.TeamTypeId = 1  NO-LOCK.
      FOR EACH ButikkKobling OF ButikkTeam WHERE 
                             ButikkKobling.Butik <> wCentralLager NO-LOCK:
          IF NOT CAN-DO(cBrukerButTilgang,STRING(ButikkKobling.Butik)) THEN
             ASSIGN cBrukerButTilgang = cBrukerButTilgang + 
                                        (IF cBrukerButTilgang <> "" THEN "," ELSE "") + 
                                        STRING(ButikkKobling.Butik).
      END.
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
   DEF VAR ReturnValue AS INTEGER NO-UNDO.   SET-SIZE(lppoint)= 2 * {&INTSIZE}.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ChangeFri C-Win 
PROCEDURE ChangeFri :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER wIdx       AS INTE        NO-UNDO.
    DEFINE INPUT PARAMETER wDiff      AS INTE        NO-UNDO.
    DEFINE VAR             wCount     AS INTE        NO-UNDO.
    DEFINE VAR             wRadEkstra AS INTE        NO-UNDO.
    DEFINE VAR             wGidx      AS INTE        NO-UNDO. /* gridens index */
    ASSIGN wFriAntal[wIdx]          = wFriAntal[wIdx] + wDiff
           tFriButik.FriAntal[wIdx] = tFriButik.FriAntal[wIdx] + wDiff
           tFriButik.TotAntal       = tFriButik.TotAntal + wDiff.
    IF BestHode.DirekteLev:CHECKED IN FRAME DEFAULT-FRAME THEN
        ASSIGN ch_Grid:Cell(7,wAktivFriRad,0,wAktivFriRad,0) = IF tFriButik.TotAntal > 0 THEN
                             wFriFGCol ELSE wOrgFriFGCol.

    DO wCount = 2 TO 4:
           ASSIGN wRadEkstra = if wCount = 4 AND wAktivFriRad > 4 THEN wAktivFriRad - 4 ELSE 0
                  ch_Grid:TextMatrix(wCount + wRadEkstra,ch_Grid:FixedCols - 1) = 
                     STRING(INT(ch_Grid:TextMatrix(wCount + wRadEkstra,ch_Grid:FixedCols - 1)) + wDiff) + " "
                  ch_Grid:TextMatrix(wCount + wRadEkstra,wIdx + ch_Grid:FixedCols - 1) =
                     STRING(INT(ch_Grid:TextMatrix(wCount + wRadEkstra,wIdx + ch_Grid:FixedCols - 1)) + wDiff) + " ".
           IF wRadEkstra > 0 AND INT(ch_Grid:TextMatrix(wCount + wRadEkstra,wIdx + ch_Grid:FixedCols - 1)) = 0 THEN
               ASSIGN ch_Grid:TextMatrix(wCount + wRadEkstra,wIdx + ch_Grid:FixedCols - 1) = "".
    END.
    IF wBestHodeRecid = ? AND INT(ch_Grid:TextMatrix(3,ch_Grid:FixedCols - 1)) = 0 THEN
        ASSIGN BUTTON-Lagra:SENSITIVE   IN FRAME {&FRAME-NAME} = NO
               BUTTON-Godkann:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
    ELSE
        ASSIGN BUTTON-Lagra:SENSITIVE   IN FRAME {&FRAME-NAME} = YES
               BUTTON-Kalkyl:SENSITIVE  IN FRAME {&FRAME-NAME} = NO
               BUTTON-Godkann:SENSITIVE IN FRAME {&FRAME-NAME} = NO
               BUTTON-FraBest:SENSITIVE IN FRAME {&FRAME-NAME} = NO
               BUTTON-Dirlev:SENSITIVE  IN FRAME {&FRAME-NAME} = NO.
    ASSIGN CB-Lev:SENSITIVE = NOT (AVAIL BestHode AND BestHode.BestStat > 2 OR 
                               NOT NUM-ENTRIES(CB-Lev:LIST-ITEM-PAIRS) > 2 OR harBestillt()).

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
    IF BestHode.DirekteLev:CHECKED IN FRAME DEFAULT-FRAME THEN DO:
        FIND tBestKasse WHERE tBestKasse.SortID = tLevSort.SortID AND
                              tBestKasse.Butik  = wCentralLager NO-ERROR.
        IF NOT AVAIL tBestKasse THEN DO:
            CREATE tBestKasse.
            ASSIGN tBestKasse.BestNr   = IF wBestHodeRecid = ? THEN ? ELSE BestHode.BestNr
                    tBestKasse.SortID  = tLevSort.SortID
                    tBestKasse.Butik   = wCentrallager
                    tBestKasse.Antal   = 0.
        END.
        IF tBestKasse.Antal < ABSOLUTE(wAntKasser) THEN
            ASSIGN wFel = YES.
    END.
    ELSE DO wIdx = 1 to NUM-ENTRIES(tLevSort.Storrelser," "):
        ASSIGN wMatrisIdx =  LOOKUP(ENTRY(wIdx,tLevSort.Storrelser," "),wStorlekar," ").
        IF INT(ENTRY(wIdx,tLevSort.Fordel," ")) * ABSOLUTE(wAntKasser) > 
                               INT(ch_Grid:TextMatrix(4,wMatrisIdx + ch_Grid:FixedCols - 1)) THEN DO:
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
    IF NOT wInit AND BestHode.DirekteLev:CHECKED IN FRAME DEFAULT-FRAME THEN DO:
        IF wBestHodeRecid = ? THEN
            FIND tBestKasse WHERE tBestKasse.BestNr = ? AND
                 tBestKasse.SortID = tLevSort.SortID    AND
                 tBestKasse.Butik  = wCentralLager NO-ERROR.
        ELSE
            FIND tBestKasse WHERE tBestKasse.BestNr = BestHode.BestNr AND
                 tBestKasse.SortID = tLevSort.SortID    AND
                 tBestKasse.Butik  = wCentralLager NO-ERROR.
        IF NOT AVAIL tBestKasse THEN DO:
            CREATE tBestKasse.
            ASSIGN tBestKasse.BestNr   = IF wBestHodeRecid = ? THEN ? ELSE BestHode.BestNr
                    tBestKasse.SortID  = tLevSort.SortID
                    tBestKasse.Butik   = wCentrallager
                    tBestKasse.Antal   = 0.
        END.

        ASSIGN tBestKasse.Antal = tBestKasse.Antal + wAntKasser.
        RUN VisKasser.
    END.
    ASSIGN ch_Grid:TextMatrix(1,ch_Grid:FixedCols - 1) = 
            STRING(INT(ch_Grid:TextMatrix(1,ch_Grid:FixedCols - 1)) + wAntKasser * tLevSort.Antal) + " "
           ch_Grid:TextMatrix(3,ch_Grid:FixedCols - 1) =
            STRING(INT(ch_Grid:TextMatrix(3,ch_Grid:FixedCols - 1)) + wAntKasser * tLevSort.Antal) + " ".
    IF NOT wInit THEN
        ASSIGN ch_Grid:TextMatrix(4,ch_Grid:FixedCols - 1) = 
            STRING(INT(ch_Grid:TextMatrix(4,ch_Grid:FixedCols - 1)) + wAntKasser * tLevSort.Antal) + " ".

    DO wIdx = 1 to NUM-ENTRIES(tLevSort.Storrelser," "):
        wMatrisIdx =  LOOKUP(ENTRY(wIdx,tLevSort.Storrelser," "),wStorlekar," ").
        ASSIGN ch_Grid:TextMatrix(1,wMatrisIdx + ch_Grid:FixedCols - 1) =
                 STRING(INT(ch_Grid:TextMatrix(1,wMatrisIdx + ch_Grid:FixedCols - 1)) + 
                    INT(ENTRY(wIdx,tLevSort.Fordel," ")) * wAntKasser) + " "
               ch_Grid:TextMatrix(3,wMatrisIdx + ch_Grid:FixedCols - 1) =
                 STRING(INT(ch_Grid:TextMatrix(3,wMatrisIdx + ch_Grid:FixedCols - 1)) +
                    INT(ENTRY(wIdx,tLevSort.Fordel," ")) * wAntKasser) + " ".
        IF NOT wInit THEN
            ASSIGN ch_Grid:TextMatrix(4,wMatrisIdx + ch_Grid:FixedCols - 1) =
                     STRING(INT(ch_Grid:TextMatrix(4,wMatrisIdx + ch_Grid:FixedCols - 1)) + 
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

OCXFile = SEARCH( "w-gridord.wrx":U ).
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
ELSE MESSAGE "w-gridord.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DelBestHode C-Win 
PROCEDURE DelBestHode :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FIND BestHode WHERE RECID(BestHode) = wBestHodeRecid EXCLUSIVE NO-ERROR.
    IF LOCKED BestHode THEN DO:
        MESSAGE "Du kan ikke ta bort posten," SKIP
                "den oppdateres av en annen bruker." VIEW-AS ALERT-BOX.
        RETURN "LOCKED".
    END.
    
    /*
    if BestHode.BestStat > 4 then
      do:
        message "Bestillinger med status større enn 4 kan ikke slettes" SKIP
                "da disse er oppdatert i statistikkene." 
          view-as alert-box.
        return.
      end.
    */

    FOR EACH BestLinje WHERE BestLinje.BestNr = BestHode.BestNr:
        DELETE BestLinje.
    END.
    FOR EACH BestStr WHERE BestStr.BestNr = BestHode.BestNr:
        DELETE BestStr.
    END.
    FOR EACH BestSort WHERE BestSort.BestNr = BestHode.BestNr:
        DELETE BestSort.
    END.
    FOR EACH BestPris WHERE BestPris.BestNr = BestHode.BestNr:
        DELETE BestPris.
    END.
    FOR EACH BestHLev WHERE BestHLev.BestNr = BestHode.BestNr:
        DELETE BestHLev.
    END.
    FOR EACH BestLevert WHERE BestLevert.BestNr = BestHode.BestNr:
        DELETE BestLevert.
    END.
    FOR EACH BestStr WHERE BestStr.BestNr = BestHode.BestNr:
        DELETE BestStr.
    END.
    FOR EACH BestKasse WHERE BestKasse.BestNr = BestHode.BestNr:
        DELETE BestKasse.
    END.
    FOR EACH FriButik WHERE FriButik.BestNr = BestHode.BestNr:
        DELETE FriButik.
    END.
    
    DELETE BestHode.
    RETURN "OK".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DirekteInnlev C-Win 
PROCEDURE DirekteInnlev :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var wBatchNr          as int    no-undo.
  def var wLoop             as int    no-undo.
  def var wTransNr          as int    no-undo.
  def var wSkjerm           as char   no-undo.
  def var wDirekte          as log    no-undo.
  def var wTekst            as char   no-undo.
  def var wEtiketter        as log    no-undo.
  def var wInnleveranse     as log    no-undo.
  def var wFolgeseddel      as log    no-undo.
  def var wBestHLevRec      as recid  no-undo.
  DEF VAR cUserid           AS CHAR   NO-UNDO.
  DEF VAR wTvFolgeseddel    AS LOG    NO-UNDO.
  DEF VAR wSkrivFolgeseddel AS LOG    NO-UNDO.
  DEF VAR h_PrisKo          AS HANDLE NO-UNDO.
  DEF VAR iTotInnLev        LIKE BestHode.TotInnLev NO-UNDO.
  DEF VAR lRegistreratButik AS LOGICAL    NO-UNDO.
  DEF VAR iBuntnr      AS INTEGER          NO-UNDO.
  DEF VAR iLinjeNr     AS INTEGER  INIT 1  NO-UNDO.
  DEF VAR cPrinterValg AS CHAR NO-UNDO.
  DEF VAR iCount       AS INTEGER NO-UNDO.
  DEF VAR cStorlek     AS CHARACTER NO-UNDO.
  DEF VAR dIndividNr   AS DECIMAL    NO-UNDO.
  DEF VAR iIndividBatchNr AS INTEGER    NO-UNDO.
  DEF VAR dArtikkelNr     AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE cDummy AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cDirektOppd AS CHARACTER INIT "N"  NO-UNDO.
  EMPTY TEMP-TABLE TT_OvBuffer NO-ERROR.
  /* Bestillingsdato og leveringsdato må være angitt for direkte innleveranse. */
  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN
          BestHode.LevDato:SCREEN-VALUE = STRING(TODAY)
          .
      IF INPUT BestHode.Bestillingsdato = ? OR
         INPUT BestHode.LevDato         = ? THEN
      DO:
          MESSAGE "Bestillings- og leveringsdato må angis før direkte innleveranse kan gjøres."
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN "AVBRYT".
      END.
  END.
  ASSIGN cKontrStorl = "".
  {syspara.i 5 4 10 wTekst} /* Skal følgeseddel ved innleveranse skrives ut. */
  IF trim(wTekst) = "1" THEN
      wSkrivFolgeseddel = TRUE.
  ELSE
      wSkrivFolgeseddel = FALSE.

  ASSIGN wLeveringsNr = 0.

  do with frame DEFAULT-FRAME TRANSACTION:
  
    FIND CURRENT BestHode EXCLUSIVE NO-ERROR.
    IF LOCKED BestHode THEN DO:
        MESSAGE "Posten oppdateras av en annen bruker." VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    IF cPassordkrav = "1" THEN DO:
        RUN d-bekreftbruker.w ("Bekreft brukerid").
        IF RETURN-VALUE = "AVBRYT" THEN DO:
            MESSAGE "Lagring avbrutt"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
/*             ASSIGN lNyDirekteIn = FALSE. /* Här måste vi stänga av ny-direkte-in */ */
            RETURN NO-APPLY.
        END.
        ELSE
            ASSIGN cUserid = RETURN-VALUE.
    END.
    IF NOT BestHode.DirekteLev AND NOT wNegInnLev THEN DO:
        /* testa om det finns inleverans till andra butiker än CL */
        ASSIGN lRegistreratButik = RegistreratButik().
        IF lRegistreratButik THEN
          RUN d-velgovbunt.w (INPUT-OUTPUT iBuntnr,INPUT-OUTPUT cDummy).
        IF RETURN-VALUE = "AVBRYT" THEN DO:
            MESSAGE "Avbrudd kobling till overføringsordre."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            ASSIGN lRegistreratButik = FALSE.
        END.
        IF lRegistreratButik AND ENTRY(1,cDummy,CHR(1)) = "J" THEN
            cDirektOppd = "J".
    END.
    ELSE
        ASSIGN lRegistreratButik = FALSE. /* onödigt */
    ASSIGN BestHode.Beskrivelse     = INPUT BestHode.Beskrivelse
           BestHode.BestType        = IF TOGGLE-Tillagg:CHECKED THEN 2
                                       ELSE 1
           BestHode.DirekteLev      = BestHode.DirekteLev:CHECKED
           BestHode.Merknad         = INPUT BestHode.Merknad
           BestHode.LevDato         = TODAY
                                      /* INPUT BestHode.LevDato */
           BestHode.LevFargKod      = INPUT ArtBas.LevFargKod
           BestHode.LevKod          = INPUT ArtBas.LevKod
           BestHode.BestillingsDato = INPUT BestHode.BestillingsDato.
    run batchlogg.w (program-name(1), 
                     "Direkte innlev - BestNr: " + string(BestHode.BestNr),
                     output wBatchNr).
    FIND CURRENT BestHode NO-LOCK.
    if wNegInnLev then
      RUN bytbeststatus.p (wBestHodeRecid,"+5",?).
    else
      RUN bytbeststatus.p (wBestHodeRecid,"+4",?).
    FIND CURRENT BestHode NO-LOCK.
    RUN SetWindowTitle.                                                   
    RUN InitButtons.
    IF ArtBas.IndividType > 0 AND wNegInnLev = FALSE THEN DO:
        FIND HuvGr OF ArtBas NO-LOCK.
        FIND LevBas OF ArtBas NO-LOCK.
        FIND Varemerke OF ArtBas NO-LOCK NO-ERROR.
        ASSIGN iIndividBatchNr = wBatchNr
               dArtikkelNr     = ArtBas.ArtikkelNr.
    END.
    CREATE BestHLev.
    ASSIGN BestHLev.BestNr       = BestHode.BestNr
           BestHLev.LeveringsNr  = NEXT-VALUE(LeveringsNr)
           BestHLev.LevertDato   = TODAY
           BestHLev.LevTidspunkt = time
           BestHLev.LevertAv     = IF cPassordkrav = "1" THEN cUserid ELSE USERID("DICTDB")
           wBestHLevRec          = recid(BestHLev)
           wLeveringsNr           = BestHLev.LeveringsNr.
    FOR EACH BestStr WHERE BestStr.BestNr   = BestHode.BestNr AND
                           BestStr.BestStat = BestHode.BestStat  AND
                           BestStr.Bestilt > 0 NO-LOCK
                           break by BestStr.BestNr
                                 by BestStr.Butik:
            if first-of (BestStr.Butik) THEN do:
                /* Benytter kalkyle for profil hvis denne finnes. */
                FIND Butiker WHERE Butiker.Butik = BestStr.Butik NO-LOCK.
                FIND BestPris WHERE BestPris.BestNr   = BestHode.BestNr AND
                   BestPris.BestStat = BestHode.BestStat AND
                   BestPris.ProfilNr = Butiker.ProfilNr NO-LOCK no-error.    
                /* Finnes ikke kalkylen, benyttes sentrallagerets kalkyle. */            
                IF NOT AVAILABLE BestPris THEN DO:
                    FIND Butiker WHERE Butiker.Butik    = wCentrallager NO-LOCK.
                    FIND BestPris WHERE BestPris.BestNr   = BestHode.BestNr   AND
                                        BestPris.BestStat = BestHode.BestStat AND
                                        BestPris.ProfilNr = Butiker.ProfilNr  NO-LOCK NO-ERROR.
                END.
            END. /* i stf nedan */
            CREATE BestLevert.
            ASSIGN BestLevert.BestNr      = BestHLev.BestNr
                   BestLevert.Butik       = BestStr.Butik
                   BestLevert.Storl       = BestStr.Storl
                   BestLevert.LeveringsNr = BestHLev.LeveringsNr
                   BestLevert.Levert      = BestStr.Bestilt
                   BestLevert.Rest        = 0
                   BestLevert.Avskrevet   = FALSE
                   BestLevert.LevertAv    = IF cPassordkrav = "1" THEN cUserid 
                                            ELSE USERID("DICTDB")
                   BestLevert.LevertDato  = TODAY
                   iTotInnLev             = iTotInnLev + BestLevert.Levert
                   .
            IF NOT CAN-DO(cKontrStorl,TRIM(BestStr.Storl)) THEN
                ASSIGN cKontrStorl = cKontrStorl + (IF cKontrStorl <> "" THEN "," ELSE "") + TRIM(BestStr.Storl).
            /* Lagrer overføringstransaksjoner ut fra sentrallageret. */
            IF BestStr.Butik <> wCentralLager AND lRegistreratButik AND BestStr.Bestilt > 0 THEN DO:
              CREATE TT_OvBuffer.
              ASSIGN TT_OvBuffer.BuntNr      = 999 /* dummy */
                     TT_OvBuffer.LinjeNr     = iLinjeNr
                     TT_OvBuffer.ButikkNrFra = wCentralLager
                     TT_OvBuffer.ButikkNrTil = BestStr.Butik        
                     TT_OvBuffer.ArtikkelNr  = ArtBas.ArtikkelNr
                     TT_OvBuffer.Vg          = ArtBas.vg   
                     TT_OvBuffer.LopNr       = ArtBas.LopNr
                     TT_OvBuffer.Antall      = BestStr.Bestilt
                     TT_OvBuffer.Merknad     = "Best " + STRING(BestHode.BestNr)
                     TT_OvBuffer.Varekost    = IF AVAIL BestPris THEN BestPris.VareKost ELSE 0
                     iLinjeNr                = iLinjeNr + 1.
              ASSIGN TT_OvBuffer.Storl       = FiksStorl(BestStr.Storl)
                     TT_OvBuffer.TilStorl    = FiksStorl(BestStr.Storl).
              RELEASE TT_OvBuffer.
            END.
            find last TransLogg no-lock where
              TransLogg.Butik = (IF (NOT BestHode.DirekteLev AND 
                                            NOT wNegInnLev AND lRegistreratButik) THEN wCentralLager
                                            ELSE BestStr.Butik) use-index TransLogg no-error.
            if available TransLogg then
              wTransNr = TransLogg.TransNr + 1.
            else 
              wTransNr = 1.
/*  lagt in uppe         end. */
        DO iCount = 1 TO (IF ArtBas.IndividType > 0 AND wNegInnlev = FALSE THEN BestStr.Bestilt ELSE 1):
        /* Sjekker at transnr er ledig */
          if can-find(TransLogg WHERE TransLogg.Butik = (IF (NOT BestHode.DirekteLev AND 
                                            NOT wNegInnLev AND lRegistreratButik) THEN wCentralLager
                                            ELSE BestStr.Butik) AND TransLogg.TransNr = wTransNr) then
          NESTE_NR:
          do while true:
            wTransNr = wTransNr + 1.
            if can-find(TransLogg WHERE TransLogg.Butik = (IF (NOT BestHode.DirekteLev AND 
                                         NOT wNegInnLev AND lRegistreratButik) THEN wCentralLager
                                          ELSE BestStr.Butik) AND TransLogg.TransNr = wTransNr) then
              next NESTE_NR.
            else
              leave NESTE_NR.
          end. /* NESTE_NR */
            /* Er det ikke direkte levering, skal innleveransen gjøres mot sentrallager. */
            ASSIGN cStorlek = FiksStorl(BestStr.Storl).
            IF ArtBas.IndividType > 0 AND wNegInnlev = FALSE THEN DO:
                FIND StrKonv WHERE StrKonv.Storl = cStorlek NO-LOCK NO-ERROR.
                IF AVAIL StrKonv THEN
                    RUN SkapaIndivid IN THIS-PROCEDURE (IF (NOT BestHode.DirekteLev AND lRegistreratButik)
                                              THEN wCentralLager ELSE BestStr.Butik,INPUT wBatchNr,INPUT StrKonv.Strkode,INPUT StrKonv.Storl, OUTPUT dIndividNr).
            END.
            create TransLogg.
            assign TransLogg.Butik        = IF (NOT BestHode.DirekteLev AND 
                                                NOT wNegInnLev 
                                                AND lRegistreratButik)
                                              THEN wCentralLager
                                              ELSE BestStr.Butik
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
                   TransLogg.Antall       = IF ArtBas.IndividType > 0 AND wNegInnlev = FALSE THEN 1
                                             ELSE BestStr.Bestilt * (if wNegInnlev 
                                                                 then -1
                                                                 else 1)
                   TransLogg.Pris         = (if available BestPris
                                              then BestPris.VareKost
                                              else 0)
                   TransLogg.RabKr        = 0
                   TransLogg.Mva          = 0
                   TransLogg.Plukket      = true /* Skal ikke ut på plukkliste */
                   TransLogg.Dato         = today
                   TransLogg.Tid          = time
                   TransLogg.BestNr       = BestHode.BestNr
                   TransLogg.Storl        = cStorlek
                   TransLogg.Postert      = false.                               
        END.
            /* Oppretter Etikett */
       IF wNegInnLev = FALSE THEN ETIKETT: /* 16/1-03 */
/*        ETIKETT: */
       do:
         find Etikett exclusive-lock where
           Etikett.LevInNr = BestHode.BestNr AND
           Etikett.Butik = (IF cBrukButikEtikett = "Ja" THEN BestStr.butik ELSE IF NOT BestHode.DirekteLev AND lRegistreratButik THEN wCentralLager ELSE BestStr.Butik) AND
           Etikett.Vg    = ArtBas.Vg and
           Etikett.LopNr = ArtBas.LopNr and
           Etikett.Storl = FiksStorl(BestStr.Storl) no-error.
         if not available Etikett then do:
             create Etikett.
             assign
               Etikett.Butik = IF cBrukButikEtikett = "Ja" THEN BestStr.butik ELSE IF NOT BestHode.DirekteLev AND lRegistreratButik THEN wCentralLager ELSE BestStr.Butik 
               Etikett.Vg    = ArtBas.Vg
               Etikett.LopNr = ArtBas.LopNr.
             assign
               Etikett.Storl = FiksStorl(BestStr.Storl).
         end.
         assign
           Etikett.Pris    = BestPris.Pris
           Etikett.Texten  = ArtBas.BongTekst
           Etikett.Antal   = Etikett.Antal + BestStr.Bestilt
           Etikett.LevInNr = BestHode.BestNr
           Etikett.Rad     = 1.
       
       end. /* ETIKETT */
    END.

    IF ArtBas.Inn_Dato = ? THEN
    DO:
      FIND CURRENT ArtBas exclusive-lock.
      assign
          ArtBas.Inn_Dato = TODAY.
      FIND CURRENT ArtBas no-lock.
    END.
      
    PROFILER_LAGRES:
    for each BestPris no-lock WHERE 
      BestPris.BestNr   = BestHode.BestNr AND
      BestPris.BestStat = BestHode.BestStat:
    
      assign
        wSkjerm =  string(BestPris.ValPris)       + ";" +
                   string(BestPris.InnkjopsPris)  + ";" +
                   string(BestPris.Rab1Kr)        + ";" +
                   string(BestPris.Rab1%)         + ";" +
                   string(BestPris.Rab2Kr)        + ";" +  
                   string(BestPris.Rab2%)         + ";" +
                   string(BestPris.Frakt)         + ";" +
                   string(BestPris.Frakt%)        + ";" + 
                   string(BestPris.DivKostKr)     + ";" + 
                   string(BestPris.DivKost%)      + ";" +
                   string(BestPris.Rab3Kr)        + ";" +
                   string(BestPris.Rab3%)         + ";" +
                   string(BestPris.VareKost)      + ";" +
                   string(BestPris.MvaKr)         + ";" +
                   string(BestPris.Mva%)          + ";" + 
                   string(BestPris.DBKr)          + ";" +
                   string(BestPris.DB%)           + ";" +
                   string(BestPris.Pris)          + ";" +
                   string(BestPris.EuroPris)      + ";" +
                   string(BestPris.EuroManuel)    + ";" + /* 20 */
                   string(today)                  + ";" + /* 21 Aktiv fra */
                   "0"                            + ";" + /* 22 */
                   ""                             + ";" + /* 23 */
                   "0"                            + ";" + /* 24 */
                   ""                             + ";" + /* 25 */
                   "0"                            + ";" + /* 26 */
                   ""                             + ";" + /* 27 */
                   "no".
      /* Parameter som styrer oppdatering av priskø. */
      /*
      {syspara.i 2 1 10 wTekst}
      if can-do("Yes,Ja,True",wTekst) then
        wDirekte = true.
      else
        wDirekte = false.
      */
      ASSIGN wDirekte = TRUE.

      IF NOT VALID-HANDLE(h_PrisKo) THEN
          RUN prisko.p PERSISTENT SET h_PrisKo.

      if wDirekte then
        do:
          /* Oppdaterer ordinærkalkyle */
          if valid-handle(h_PrisKo) then
            run LagreArtPris in h_PrisKo
                (input recid(ArtBas),
                 input BestPris.ProfilNr,
                 input-output wSkjerm,
                 input false,  /* wTilbud = false - Dvs ordinær kaflkyle.         */
                 input true,   /* Direkte oppdatering av prisene som er kalkulert */
                 INPUT 1,
                 ?).  
         end.
      else do:          
        /* Oppdaterer ordinærkalkyle */
        if valid-handle(h_PrisKo) then
          run LagreArtPris in h_PrisKo
              (input recid(ArtBas),
               input BestPris.ProfilNr,
               input-output wSkjerm,
               input false,  /* wTilbud = false - Dvs ordinær kaflkyle.         */
               input false,  /* Direkte oppdatering av prisene som er kalkulert */
               INPUT 1,
               ?). 
      end.
    END. /* PROFILER_LAGRES */
    FIND CURRENT BestHode EXCLUSIVE NO-ERROR.
    ASSIGN BestHode.TotInnLev = iTotInnLev * IF wNegInnLev THEN -1 ELSE 1.
    FIND CURRENT BestHode NO-LOCK NO-ERROR.
    IF VALID-HANDLE(h_PrisKo) THEN
        DELETE PROCEDURE h_PRisKo.
    IF CAN-FIND(FIRST TT_OvBuffer) THEN
      RUN LagraOvBuffer.p (INPUT-OUTPUT iBuntNr,ArtBas.ArtikkelNr,cDirektOppd + CHR(1) + "Best " + STRING(BestHode.BestNr),wEDB-System,wTabell,iOpphav).
    IF wNegInnLev = FALSE THEN
        RUN GenererEan IN THIS-PROCEDURE.
    if available Etikett then
      release Etikett.
    if available ArtBas then
      release ArtBas.
    if available TransLogg then
      release TransLogg.
  end. /* TRANSACTION */

  run batchstatus.p (wBatchNr, 2).
  /* Utskriftshåndtering. */
  ASSIGN wEtiketter = wNegInnLev = FALSE.
  run d-innlevutskrift.w (BestHode.DirekteLev,OUTPUT wTvFolgeseddel,INPUT-OUTPUT wEtiketter,output wFolgeseddel,output wInnleveranse, OUTPUT cPrinterValg).
  /* Etikettutskrift */
  if wEtiketter THEN do:
      find first Etikett no-lock where
        Etikett.LevInNr = BestHode.BestNr no-error.
      if available Etikett THEN DO:
          IF iIndividBatchNr > 0 THEN
              RUN SkrivIndividEti (iIndividBatchNr,dArtikkelNr,cPrinterValg,
                                   IF NOT BestHode.DirekteLev AND lRegistreratButik THEN wCentralLager ELSE ?).
          ELSE
              RUN SkrivEtiketter (cPrinterValg).
      END.
  end.
  run SlettEtiketter.
  /* Utskrift av Innleveranse */
  if wInnleveranse then DO:
      IF cInnlevSamlet = "1" THEN
          run bestillingskort.p (wBestHLevRec,107,4,wFolgeseddel).
      ELSE
          run bestillingskortX.p (wBestHLevRec,107,4,wFolgeseddel).
  END.
  IF iIndividBatchNr > 0 THEN
    RUN gIndividSerie.w (INPUT iIndividBatchNr).

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
  DISPLAY T-LapTop FILL-IN-Vgr-Lopnr CB-Lev TOGGLE-Tillagg FILL-IN-2 FILL-IN-1 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  IF AVAILABLE ArtBas THEN 
    DISPLAY ArtBas.VgKat ArtBas.Beskr ArtBas.LevNr ArtBas.LevKod ArtBas.LevFargKod 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  IF AVAILABLE BestHode THEN 
    DISPLAY BestHode.BestillingsDato BestHode.LevDato BestHode.TotInnkjVerdi 
          BestHode.BekreftetDato BestHode.TotDbKr BestHode.BekreftetOrdre 
          BestHode.EkstId BestHode.TotSalgsVerdi BestHode.Beskrivelse 
          BestHode.DirekteLev BestHode.Merknad BestHode.RegistrertAv 
          BestHode.EDato 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  IF AVAILABLE BestPris THEN 
    DISPLAY BestPris.VareKost BestPris.DBKr BestPris.Pris 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  IF AVAILABLE LevBas THEN 
    DISPLAY LevBas.levnamn 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-1 B-Etiketter RECT-2 RECT-3 RECT-47 RECT-48 RECT-68 B-Print 
         B-PrintInnlev Btn_Done BUTTON-Sortiment BestHode.BestillingsDato 
         BestHode.LevDato CB-Lev BestHode.BekreftetOrdre BestHode.Beskrivelse 
         BestHode.DirekteLev TOGGLE-Tillagg BROWSE-1 BestHode.Merknad Btn_Help 
         FILL-IN-2 FILL-IN-1 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FillCBAltLev C-Win 
PROCEDURE FillCBAltLev :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE BUFFER bLevBas FOR LevBas.
   DEFINE VARIABLE iLevNr LIKE LevBas.LevNr NO-UNDO.
   DO WITH FRAME {&FRAME-NAME}:
       FIND bLevBas OF ArtBas NO-LOCK.
       ASSIGN iLevNr = bLevBas.LevNr
              CB-Lev:LIST-ITEM-PAIRS = STRING(ArtBas.LevNr) + " : " + bLevBas.LevNamn + "," + STRING(bLevBas.LevNr).
       FOR EACH AltLevBas WHERE AltLevBas.ArtikkelNr = Artbas.Artikkelnr AND 
                                AltLevBas.LevNr <> iLevNr NO-LOCK.
           FIND bLevBas OF AltLevBas NO-LOCK NO-ERROR.
           IF NOT AVAIL bLevBas THEN
               NEXT.
           ASSIGN CB-Lev:LIST-ITEM-PAIRS = CB-Lev:LIST-ITEM-PAIRS + "," + STRING(bLevBas.LevNr) + " : " + bLevBas.LevNamn + "," + STRING(bLevBas.LevNr).
       END.
       IF AVAIL BestHode AND BestHode.Levnr <> 0 AND BestHode.Levnr <> ArtBas.LevNr THEN DO:
           IF NOT CAN-FIND(AltLevBas WHERE AltLevBas.ArtikkelNr = Artbas.Artikkelnr AND 
                                AltLevBas.LevNr = BestHode.LevNr) THEN DO:
               FIND bLevBas OF BestHode NO-LOCK NO-ERROR.
               IF AVAIL bLevbas THEN
                   ASSIGN CB-Lev:LIST-ITEM-PAIRS = CB-Lev:LIST-ITEM-PAIRS + "," + STRING(bLevBas.LevNr) + " : " + bLevBas.LevNamn + "," + STRING(bLevBas.LevNr).
           END.
       END.
   END.
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
    DEFINE VAR             wIdx      AS INTE NO-UNDO.
    DEFINE VAR             wDirekteLev  AS LOGI NO-UNDO.
    ASSIGN wDirekteLev = BestHode.DirekteLev:CHECKED IN FRAME DEFAULT-FRAME.
    CASE wMode:
        WHEN "En" THEN
            ASSIGN
                MENU-ITEM m_nysortering:SENSITIVE IN MENU POPUP-MENU-FRAME-A = 
                                                          OkSortering("NY")
                MENU-ITEM m_n_nysortering:SENSITIVE IN MENU POPUP-MENU-FRAME-A = 
                MENU-ITEM m_nysortering:SENSITIVE IN MENU POPUP-MENU-FRAME-A
                                                        /*  OkSortering("NY") */
                MENU-ITEM m_delsortering:SENSITIVE IN MENU POPUP-MENU-FRAME-A = 
                                                          OkSortering("DEL")
                MENU-ITEM m_nystaket:SENSITIVE IN MENU POPUP-MENU-FRAME-A = NO
                MENU-ITEM m_n_nystaket:SENSITIVE IN MENU POPUP-MENU-FRAME-A = NO
                MENU-ITEM m_delstaket:SENSITIVE IN MENU POPUP-MENU-FRAME-A = NO
                MENU-ITEM m_delruta:SENSITIVE IN MENU POPUP-MENU-FRAME-A = 
                     wDirekteLev = FALSE AND ch_Grid:TextMatrix(ch_Grid:Row,ch_Grid:Col) <> ""
                MENU-ITEM m_delraden:SENSITIVE IN MENU POPUP-MENU-FRAME-A = 
                                IF wDirekteLev = TRUE THEN OkDelrad() ELSE
                                ch_Grid:TextMatrix(ch_Grid:Row,ch_Grid:FixedCols - 1) <> "0"
                MENU-ITEM m_delomr:SENSITIVE IN MENU POPUP-MENU-FRAME-A = NO.
        WHEN "Multi" THEN DO:
            OmrData:
            DO wCount1 = wFstRow TO wLstRow:
                IF NOT wDirekteLev THEN DO wCount2 = wFstCol TO wLstCol:
                    IF INT(ch_Grid:TextMatrix(wCount1,wCount2)) <> 0 THEN DO:
                        ASSIGN wOmrData = YES.
                        LEAVE OmrData.
                    END. 
                END.
            END.
            NyStaket:
            DO wCount2 = wFstCol TO wLstCol:
                IF NOT wDirekteLev AND INT(ch_Grid:TextMatrix(4,wCount2)) > 0 THEN DO:
                    ASSIGN wNyStaket = YES.
                    LEAVE NyStaket.
                END. 
            END.

            ASSIGN
                 MENU-ITEM m_nysortering:SENSITIVE IN MENU POPUP-MENU-FRAME-A =
                     IF wFstCol = wLstCol THEN OkSortering("NY") ELSE NO
                 MENU-ITEM m_n_nysortering:SENSITIVE IN MENU POPUP-MENU-FRAME-A = 
                     MENU-ITEM m_nysortering:SENSITIVE IN MENU POPUP-MENU-FRAME-A
                   /*  IF wFstCol = wLstCol THEN OkSortering("NY") ELSE NO */
                 MENU-ITEM m_delsortering:SENSITIVE IN MENU POPUP-MENU-FRAME-A = 
                                                wFstCol = wLstCol AND OkSortering("DEL")
                 MENU-ITEM m_nystaket:SENSITIVE IN MENU POPUP-MENU-FRAME-A = wNyStaket
                 MENU-ITEM m_n_nystaket:SENSITIVE IN MENU POPUP-MENU-FRAME-A = wNyStaket
                 MENU-ITEM m_delstaket:SENSITIVE IN MENU POPUP-MENU-FRAME-A = wOmrData
                 MENU-ITEM m_delruta:SENSITIVE IN MENU POPUP-MENU-FRAME-A = NO
                 MENU-ITEM m_delraden:SENSITIVE IN MENU POPUP-MENU-FRAME-A = NO
                 MENU-ITEM m_delomr:SENSITIVE IN MENU POPUP-MENU-FRAME-A = wOmrData.
        END.
    END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GenererEan C-Win 
PROCEDURE GenererEan :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    cKontrStorl = AlleEANok(cKontrStorl).
    IF TRIM(cManRegEAN) = "1" AND cKontrStorl <> "" THEN
        RUN StrekKode.w (THIS-PROCEDURE).
    RUN genStrekKode.p (BestHode.BestNr,3,"").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HK_VelgTeam C-Win 
PROCEDURE HK_VelgTeam :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cTekst AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTeam  AS CHARACTER  NO-UNDO.
    RUN gbutikkteam (
      INPUT-OUTPUT cTekst, /* Returstreng - chr(1) separert */
      "", /* Feltliste avgrensningsfelt (kommaseparert) */
      "", /* Feltverdier (chr(1) sep) */ 
      "",
      wBrGrpNr, /* brukergruppe */
      1         /* teamtypeid */
/*       CB-Team:SCREEN-VALUE /* Post markøren skal stå på */ */
      ).
    IF RETURN-VALUE = "AVBRYT" THEN
        RETURN "AVBRYT".
    IF NUM-ENTRIES(cTekst,CHR(1)) >= 3 THEN
    DO:
       FIND ButikkTeam WHERE ButikkTeam.TeamNr = INT(ENTRY(2,cTekst,CHR(1))) AND ButikkTeam.TeamTypeId = 1 AND 
                        ButikkTeam.BrGrpNr = wBrGrpNr NO-LOCK NO-ERROR.
       IF AVAIL ButikkTeam THEN DO:
           FOR EACH ButikkKobling OF ButikkTeam NO-LOCK. 
               FIND Butiker WHERE Butiker.Butik = ButikkKobling.Butik NO-LOCK NO-ERROR.
               IF AVAIL Butiker AND Butiker.Sentrallager = TRUE THEN DO:
                   ASSIGN wCentralLager = Butiker.Butik
                          iTeamNr       = ButikkTeam.TeamNr.
                   LEAVE.
               END.
           END.
       END.
       ELSE DO:
           MESSAGE "Feil ved henting av team."
               VIEW-AS ALERT-BOX ERROR BUTTONS OK.
           RETURN "AVBRYT".
       END.
       IF wCentralLager = 0 THEN DO:
           MESSAGE "Finner ikke sentrallager for valgt team."
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
           RETURN "AVBRYT".
       END.
/* Legger opp verdier I de aktuelle feltene */
/*         ASSIGN cTeam = ENTRY(3,cTekst,CHR(1))  */
/*                CB-Team:SCREEN-VALUE = cTeam.   */
/*         APPLY "VALUE-CHANGED" TO CB-Team.      */
    END.
    ELSE
        RETURN "AVBRYT".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitButiker C-Win 
PROCEDURE InitButiker :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR wRow AS INTE INIT 5 NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:                 
    IF iTeamNr <> 0 AND cHKinst = "yes" THEN DO:
      FIND Butiker WHERE Butiker.Butik = wCentralLager NO-LOCK NO-ERROR.
      ASSIGN cStatusStr = ",,,Sentrallager" + " " + Butiker.Butnamn.
      FIND ButikkTeam WHERE ButikkTeam.BrGrpNr = wBrGrpNr AND
                            ButikkTeam.TeamTypeId = 1 AND
                            ButikkTeam.TeamNr = iTeamNr NO-LOCK NO-ERROR.
      IF AVAIL ButikkTeam THEN DO:
        ASSIGN CB-Team:LIST-ITEMS = ButikkTeam.Beskrivelse.
        FOR EACH ButikkKobling OF ButikkTeam WHERE 
                               ButikkKobling.Butik <> wCentralLager NO-LOCK:
          FIND Butiker WHERE Butiker.Butik = ButikkKobling.Butik NO-LOCK NO-ERROR.
          ASSIGN cStatusStr = cStatusStr + "," + IF AVAIL Butiker THEN REPLACE(Butiker.Butnamn,","," ") ELSE ""
                 ch_Grid:Rows = wRow + 1
                 ch_Grid:TextMatrix(wRow,0) = STRING(ButikkKobling.Butik) + " "
                 ch_Grid:TextMatrix(wRow,ch_Grid:FixedCols - 1) = "0 "
                 wRow = wRow + 1.
        END.
      END.
    END.
/*     ELSE IF CB-Team:SCREEN-VALUE = ENTRY(1,CB-Team:LIST-ITEMS) THEN DO:                                         */
/*       FIND Butiker WHERE Butiker.Butik = wCentralLager NO-LOCK NO-ERROR.                                        */
/*       ASSIGN cStatusStr = ",,,Sentrallager" + " " + Butiker.Butnamn.                                            */
/*       FOR EACH ButikkTilgang WHERE ButikkTilgang.BrGrpNr = wBrGrpNr AND                                         */
/*                ButikkTilgang.Butik <> wCentralLager NO-LOCK:                                                    */
/*           FIND Butiker WHERE Butiker.Butik = ButikkTilgang.Butik NO-LOCK NO-ERROR.                              */
/*           ASSIGN cStatusStr = cStatusStr + "," + IF AVAIL Butiker THEN REPLACE(Butiker.Butnamn,","," ") ELSE "" */
/*                  ch_Grid:Rows = wRow + 1                                                                        */
/*                  ch_Grid:TextMatrix(wRow,0) = STRING(ButikkTilgang.Butik) + " "                                 */
/*                  ch_Grid:TextMatrix(wRow,ch_Grid:FixedCols - 1) = "0 "                                          */
/*                  wRow = wRow + 1.                                                                               */
/*       END.                                                                                                      */
/*     END.                                                                                                        */
    ELSE DO:
      FIND ButikkTeam WHERE ButikkTeam.BrGrpNr = wBrGrpNr AND
                            ButikkTeam.TeamTypeId = 1 AND
                            ButikkTeam.Beskrivelse = CB-Team:SCREEN-VALUE NO-LOCK NO-ERROR.
      IF AVAIL ButikkTeam THEN DO:
        FIND Butiker WHERE Butiker.Butik = wCentralLager NO-LOCK NO-ERROR.
        ASSIGN cStatusStr = ",,,Sentrallager" + " " + Butiker.Butnamn.
        FOR EACH ButikkKobling OF ButikkTeam WHERE 
                               ButikkKobling.Butik <> wCentralLager NO-LOCK:
          FIND Butiker WHERE Butiker.Butik = ButikkKobling.Butik NO-LOCK NO-ERROR.
          ASSIGN cStatusStr = cStatusStr + "," + IF AVAIL Butiker THEN REPLACE(Butiker.Butnamn,","," ") ELSE ""
                 ch_Grid:Rows = wRow + 1
                 ch_Grid:TextMatrix(wRow,0) = STRING(ButikkKobling.Butik) + " "
                 ch_Grid:TextMatrix(wRow,ch_Grid:FixedCols - 1) = "0 "
                 wRow = wRow + 1.
        END.
      END.
    END.
  END.
/*   DEF VAR wRow AS INTE INIT 5 NO-UNDO.                                                                          */
/*   DO WITH FRAME {&FRAME-NAME}:                                                                                  */
/*     ASSIGN ch_Grid:rows = wRow.                                                                                 */
/*     IF CB-Team:SCREEN-VALUE = winitCBValg THEN DO:                                                              */
/*       FIND Butiker WHERE Butiker.Butik = wCentralLager NO-LOCK NO-ERROR.                                        */
/*       ASSIGN cStatusStr = ",,,Sentrallager" + " " + Butiker.Butnamn.                                            */
/*       FOR EACH ButikkTilgang WHERE ButikkTilgang.BrGrpNr = wBrGrpNr AND                                         */
/*                ButikkTilgang.Butik <> wCentralLager NO-LOCK:                                                    */
/*           FIND Butiker WHERE Butiker.Butik = ButikkTilgang.Butik NO-LOCK NO-ERROR.                              */
/*           ASSIGN cStatusStr = cStatusStr + "," + IF AVAIL Butiker THEN REPLACE(Butiker.Butnamn,","," ") ELSE "" */
/*                  ch_Grid:Rows = wRow + 1                                                                        */
/*                  ch_Grid:TextMatrix(wRow,0) = STRING(ButikkTilgang.Butik) + " "                                 */
/*                  ch_Grid:TextMatrix(wRow,ch_Grid:FixedCols - 1) = "0 "                                          */
/*                  wRow = wRow + 1.                                                                               */
/*       END.                                                                                                      */
/*     END.                                                                                                        */
/*     ELSE DO:                                                                                                    */
/*       FIND ButikkTeam WHERE ButikkTeam.BrGrpNr = wBrGrpNr AND                                                   */
/*                             ButikkTeam.TeamTypeId = 1 AND                                                       */
/*                             ButikkTeam.Beskrivelse = CB-Team:SCREEN-VALUE NO-LOCK NO-ERROR.                     */
/*       IF AVAIL ButikkTeam THEN DO:                                                                              */
/*         IF cHKinst = "yes" THEN FINNCL: DO:                                                                     */
/*             FOR EACH ButikkKobling OF ButikkTeam  NO-LOCK:                                                      */
/*               FIND Butiker WHERE Butiker.Butik = ButikkKobling.Butik NO-LOCK NO-ERROR.                          */
/*               IF Butiker.SentralLager = TRUE THEN DO:                                                           */
/*                   ASSIGN wCentralLager = Butiker.Butik.                                                         */
/*                          ch_Grid:TextMatrix(4,0)   = "CL " + STRING(wCentralLager) + " ".                       */
/*                   LEAVE FINNCL.                                                                                 */
/*               END.                                                                                              */
/*             END.                                                                                                */
/*         END.                                                                                                    */
/*         FIND Butiker WHERE Butiker.Butik = wCentralLager NO-LOCK NO-ERROR.                                      */
/*         ASSIGN cStatusStr = ",,,Sentrallager" + " " + Butiker.Butnamn.                                          */
/*         FOR EACH ButikkKobling OF ButikkTeam WHERE                                                              */
/*                                ButikkKobling.Butik <> wCentralLager NO-LOCK:                                    */
/*           FIND Butiker WHERE Butiker.Butik = ButikkKobling.Butik NO-LOCK NO-ERROR.                              */
/*           ASSIGN cStatusStr = cStatusStr + "," + IF AVAIL Butiker THEN REPLACE(Butiker.Butnamn,","," ") ELSE "" */
/*                  ch_Grid:Rows = wRow + 1                                                                        */
/*                  ch_Grid:TextMatrix(wRow,0) = STRING(ButikkKobling.Butik) + " "                                 */
/*                  ch_Grid:TextMatrix(wRow,ch_Grid:FixedCols - 1) = "0 "                                          */
/*                  wRow = wRow + 1.                                                                               */
/*         END.                                                                                                    */
/*       END.                                                                                                      */
/*     END.                                                                                                        */
/*     IF wRow = 5 THEN                                                                                            */
/*         ch_Grid:rows = wRow + 1.                                                                                */
/*   END.                                                                                                          */
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
    DO WITH FRAME {&FRAME-NAME}:

        IF AVAIL BestHode THEN
            CB-Lev = BestHode.LevNr.
        ASSIGN CB-Lev:SENSITIVE =
            NOT (AVAIL BestHode AND BestHode.BestStat > 2 OR NOT NUM-ENTRIES(CB-Lev:LIST-ITEM-PAIRS) > 2 OR harBestillt())
               BUTTON-FraBest:SENSITIVE    = BestHode.BestStat = 2
               BUTTON-Kalkyl:SENSITIVE     = TRUE /* BestHode.BestStat < 5 */
               BUTTON-Sortiment:SENSITIVE  = BestHode.BestStat < 5
               MENU-ITEM m_NylevSort:SENSITIVE IN MENU POPUP-MENU-BROWSE-1 =
                                             BestHode.BestStat < 5
               BUTTON-DirLev:SENSITIVE       = cHKinst = "no" AND BestHode.BestStat = 2
               BestHode.DirekteLev:SENSITIVE = cHKinst = "no" AND BestHode.BestStat < 5 AND lCanUpdateCL
               BestHode.LevDato:SENSITIVE    = BestHode.BestStat < 5
               BestHode.BestillingsDato:SENSITIVE = BestHode.BestStat < 5
               BestHode.Beskrivelse:SENSITIVE     = BestHode.BestStat < 5
               BestHode.Merknad:SENSITIVE         = BestHode.BestStat < 6
               BUTTON-Godkann:SENSITIVE           = BestHode.BestStat < 4 AND
                                                 INT(ch_Grid:TextMatrix(3,2)) > 0
               TOGGLE-Tillagg:SENSITIVE           = BestHode.BestStat < 5
               .
         
/*         CASE BestHode.BestStat:                                                                                                        */
/*           WHEN 1 THEN                                                                                                                  */
/*             BUTTON-Godkann:LABEL = DYNAMIC-FUNCTION("getStringTranslation",THIS-PROCEDURE:FILE-NAME,"BUTTON-Godkann","&Godkjenn").     */
/*           WHEN 2 THEN                                                                                                                  */
/*             BUTTON-Godkann:LABEL = DYNAMIC-FUNCTION("getStringTranslation",THIS-PROCEDURE:FILE-NAME,"BUTTON-Godkann","Til &ordre..."). */
/*           WHEN 3 THEN                                                                                                                  */
/*             BUTTON-Godkann:LABEL = DYNAMIC-FUNCTION("getStringTranslation",THIS-PROCEDURE:FILE-NAME,"BUTTON-Godkann","Fra &ordre").    */
/*         END CASE.                                                                                                                      */

        CASE BestHode.BestStat:
            WHEN 1 THEN
                ASSIGN BUTTON-Godkann:LABEL        = "&Godkjenn".
            WHEN 2 THEN
                ASSIGN BUTTON-Godkann:LABEL        = "Til &ordre...".
            WHEN 3 THEN
                ASSIGN BUTTON-Godkann:LABEL        = "Fra &ordre".
        END CASE.
        ASSIGN BUTTON-NegLev:SENSITIVE = BUTTON-Dirlev:SENSITIVE.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitCB C-Win 
PROCEDURE InitCB :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cListItems AS CHARACTER  NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:
    {syspara.i 1 100 1 wCBValg}
/*     ASSIGN winitCBValg = wCBValg                                */
/*            cListItems = IF cHKinst = "no" THEN wCBValg ELSE "". */
    FOR EACH ButikkTeam NO-LOCK WHERE ButikkTeam.BrGrpNr = wBrGrpNr AND
                              ButikkTeam.TeamTypeId = 1 BY ButikkTeam.Beskrivelse.
        ASSIGN cListItems = cListItems + (IF cListItems = "" THEN "" ELSE ",") +
                                      STRING(ButikkTeam.Beskrivelse).
    END.
    ASSIGN CB-Team:LIST-ITEMS = cListItems.
    FOR EACH ButikkTilgang WHERE ButikkTilgang.BrGrpNr = wBrGrpNr NO-LOCK.
        ASSIGN cUpdateButListe = cUpdateButListe + (IF cUpdateButListe <> "" THEN "," ELSE "") + STRING(ButikkTilgang.Butik).
    END.
  END.
  ASSIGN BUTTON-SokTeam:HIDDEN = NO
         BUTTON-SokTeam:SENSITIVE = YES
         CB-Team:HIDDEN = NO
         CB-Team:SENSITIVE = YES
         CB-Team:SCREEN-VALUE = ENTRY(1,CB-Team:LIST-ITEMS).
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
DEFINE VAR wTotBestilt AS INTE NO-UNDO.
DEFINE VAR wFargStr    AS CHAR NO-UNDO.
DEFINE VAR wButik      LIKE Butiker.Butik NO-UNDO.

DEFINE VAR iForstaCol  AS INTE INIT ? NO-UNDO. /* för visning av gris    */
DEFINE VAR iSistaCol   AS INTE        NO-UNDO. /* i.e om vi skal scrolla */

ASSIGN ch_Grid = chCtrlFrame:vsFlexGrid.

ASSIGN ch_Grid:CellPictureAlignment = 1
       ch_Grid:Redraw = FALSE. /* disable repaint while populating */
ch_Grid:Clear().

ASSIGN ch_Grid:AllowUserResizing = 0    /* Fixed columns/rows */
       ch_Grid:Enabled           = TRUE /* Updateable grid */
       ch_Grid:AllowBigSelection = FALSE
       ch_Grid:Appearance        = 1 
       ch_Grid:Rows              = 6 /* tidigare 6 15/1-03 ken1 */
       ch_Grid:Cols              = NUM-ENTRIES(wStorlekar," ") + 3
       ch_Grid:FixedRows         = 5
       ch_Grid:FixedCols         = 3
       ch_Grid:TextStyle         = 0
       ch_Grid:TextStyleFixed    = 0
       ch_Grid:ColWidth(1)       = 615
       ch_Grid:ColWidthMin       = 615
   /*    ch_Grid:TextMatrix(0,0)   = "Butik " */
       ch_Grid:TextMatrix(0,ch_Grid:FixedCols - 2)   = "Kasser "
       ch_Grid:TextMatrix(0,ch_Grid:FixedCols - 1)   = "Total "
       ch_Grid:TextMatrix(2,0)   = "Fri "
       ch_Grid:TextMatrix(1,0)   = "Kasser "
       ch_Grid:TextMatrix(3,0)   = "Bestilt"
       ch_grid:HonorProKeys      = false.
       wOrgFriFGCol              = ch_Grid:Cell(7,0,0,0,0).

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

FIND SysPara WHERE SysPara.SysHId = 5 AND SysPara.SysGr  = 10 AND
                   SysPara.ParaNr = 10 NO-LOCK NO-ERROR.
ASSIGN wFriFGCol = IF AVAIL SysPara AND SysPara.Parameter1 <> "" THEN
                              INT(SysPara.Parameter1)
                          ELSE 255. 
    

/*
DO wIdx = 0 TO ch_Grid:Cols - 1:
    ASSIGN ch_Grid:Row = 0
           ch_Grid:Col = wIdx.
           ch_Grid:CellBackColor = 16777215.
END.
/* Initiering av bakgrundsfärg för rad 4 */
DO wIdx = 0 TO ch_Grid:Cols - 1:
    ASSIGN ch_Grid:Row = 3
           ch_Grid:Col = wIdx.
           ch_Grid:CellBackColor = 16777215.
END.
*/

/* Initiering av storleksrad */
DO wIdx = 1 to NUM-ENTRIES(wStorlekar," "):
    IF wIdx <= 50 THEN
    ASSIGN ch_Grid:TextMatrix(0,wIdx + ch_Grid:FixedCols - 1) = ENTRY(wIdx,wStorlekar," ") + " ".
           ch_Grid:ColWidth(wIdx + ch_Grid:FixedCols - 1) = 510.
END.
/* Kasser */
IF wBestHodeRecid = ? THEN DO:
    DO wIdx = 1 to NUM-ENTRIES(wStorlekar," "):
        IF wIdx <= 50 THEN
        ASSIGN ch_Grid:TextMatrix(1,wIdx + ch_Grid:FixedCols - 1) = "0 ".
    END.
END.
ELSE DO:
    FOR EACH tLevSort WHERE tLevSort.Fri = FALSE:
        wTotKassPar = wTotKassPar + tLevSort.AntSort * tLevSort.Antal.
        RUN ChgKasse(tLevSort.AntSort,YES).
    END.
END.
ASSIGN ch_Grid:TextMatrix(1,ch_Grid:FixedCols - 1) = IF wBestHodeRecid = ? THEN
                                        "0 "
                                    ELSE STRING(wTotKassPar) + " ".
/* END Kasser */

/* TEST 
DISPLAY wStorlekar SKIP wFriAntal WITH FRAME Gurre OVERLAY .
PAUSE.
HIDE FRAME gurre NO-PAUSE.
 TEST Slutt */

/* Fri */
ASSIGN ch_Grid:TextMatrix(2,ch_Grid:FixedCols - 1) = STRING(wFriAnt) + " ".
DO wIdx = 1 TO NUM-ENTRIES(wStorlekar," "):
    IF wIdx <= 50 THEN
    ASSIGN ch_Grid:TextMatrix(2,wIdx + ch_Grid:FixedCols - 1) = STRING( wFriAntal[wIdx]) + " ".
END.  
/* END Fri */
/* Beställningsrad */
DO wIdx = ch_Grid:FixedCols - 1 to ch_Grid:Cols - 1:
    ASSIGN ch_Grid:TextMatrix(3,wIdx) = STRING(INT(ch_Grid:TextMatrix(1,wIdx)) 
                                                + INT(ch_Grid:TextMatrix(2,wIdx))) + " "
           iForstaCol = IF wIdx >= ch_Grid:FixedCols AND 
                                   iForstaCol = ? AND 
                                   INT(ch_Grid:TextMatrix(3,wIdx)) > 0 THEN wIdx ELSE iForstaCol
           iSistaCol  = IF wIdx >= ch_Grid:FixedCols AND 
                                   INT(ch_Grid:TextMatrix(3,wIdx)) > 0 THEN wIdx ELSE iSistaCol
           .
END.
/* END Beställningsrad */
IF cHKinst = "yes" AND wBestHodeRecid <> ? THEN FINNCL: DO:
    FOR EACH BestStr NO-LOCK WHERE BestStr.BestNr   = BestHode.BestNr BREAK BY BestStr.Butik.
        IF FIRST-OF(BestStr.Butik) THEN DO:
            FIND Butiker WHERE Butiker.Butik = BestStr.Butik NO-LOCK NO-ERROR.
            IF AVAIL Butiker AND Butiker.Sentrallager = TRUE THEN DO:
                ASSIGN wCentralLager = Butiker.Butik.
                LEAVE FINNCL.
            END.
        END.
    END.
END.
ASSIGN ch_Grid:TextMatrix(4,0)   = "CL " + STRING(wCentralLager) + " ".

/* Centrallager */
IF wBestHodeRecid = ? THEN DO:
    DO wIdx = ch_Grid:FixedCols - 1 to ch_Grid:Cols - 1:
        ASSIGN ch_Grid:TextMatrix(4,wIdx) = "0 ".
    END.
END.
ELSE DO:
    DO wIdx = 1 to NUM-ENTRIES(wStorlekar," "):
        IF wIdx <= 50 THEN
        DO:
            FIND BestStr WHERE BestStr.BestNr   = BestHode.BestNr            AND
                               BestStr.Butik    = wCentralLager              AND
                               BestStr.Storl    = ENTRY(wIdx,wStorlekar," ") AND
                               BestStr.BestStat = BestHode.BestStat NO-LOCK NO-ERROR.
            IF AVAIL BestStr THEN
                ASSIGN wTotCentral = wTotCentral + BestStr.Bestilt
                       ch_Grid:TextMatrix(4,wIdx + ch_Grid:FixedCols - 1) = STRING(BestStr.Bestilt) + " ".
            ELSE
                ASSIGN ch_Grid:TextMatrix(4,wIdx + ch_Grid:FixedCols - 1) = "0 ".
        END.
    END.
    ASSIGN ch_Grid:TextMatrix(4,ch_Grid:FixedCols - 1) = STRING(wTotCentral) + " ".
END.
/* END Centrallager */

/* Finn resten av butikerna och det som är beställt */
ASSIGN wRow = 5. /* Skall stå här, gäller både för IF och ELSE */
IF wBestHodeRecid = ? THEN DO:
  RUN InitButiker.
END.
ELSE DO:
    FIND Butiker WHERE Butiker.Butik = wCentralLager NO-LOCK NO-ERROR.
    ASSIGN cStatusStr = ",,,Sentrallager" + " " + Butiker.Butnamn.
    FOR EACH BestLinje OF BestHode WHERE BestLinje.Butik <> wCentralLager NO-LOCK:
        FIND Butiker WHERE Butiker.Butik = BestLinje.Butik NO-LOCK NO-ERROR.
        ASSIGN cStatusStr = cStatusStr + "," + IF AVAIL Butiker THEN REPLACE(Butiker.Butnamn,","," ") ELSE ""
               wTotBestilt                = 0
               ch_Grid:Rows               = wRow + 1
               ch_Grid:TextMatrix(wRow,0) = STRING(BestLinje.Butik) + " ".
       
        FOR EACH BestStr OF BestLinje WHERE BestStr.BestStat = BestHode.BestStat NO-LOCK:
            ASSIGN wIdx = LOOKUP(TRIM(BestStr.Storl),wStorlekar," ")
                   wTotBestilt = wTotBestilt + BestStr.Bestilt
                   ch_Grid:TextMatrix(wRow,wIdx + ch_Grid:FixedCols - 1) = 
                   IF BestStr.Bestilt > 0 THEN STRING(BestStr.Bestilt) + " " ELSE "".
        END.
        ASSIGN ch_Grid:TextMatrix(wRow,ch_Grid:FixedCols - 1) = STRING(wTotBestilt) + " "
               wRow = wRow + 1.
    END.
END.
IF AVAIL BestHode AND BestHode.DirekteLev THEN DO:
    IF CAN-FIND(FriButik OF BestHode WHERE FriButik.Butik = wCentralLager) THEN
        ASSIGN ch_Grid:Cell(7,ch_Grid:FixedRows - 1,0,ch_Grid:FixedRows - 1,0) = wFriFGCol.
    DO wRow = ch_Grid:FixedRows TO ch_Grid:Rows - 1:
        ASSIGN wButik = INT(ch_Grid:TextMatrix(wRow,0)).
        IF CAN-FIND(FriButik OF BestHode WHERE FriButik.Butik = wButik) THEN
            ASSIGN ch_Grid:Cell(7,wRow,0,wRow,0) = wFriFGCol.
    END.
END.
/* test CellAlignment */

ASSIGN ch_Grid:Cell(2,1,1,ch_Grid:Rows - 1, ch_Grid:Cols - 1) = 7.

/* end test */
/* sätter en svagt gul färg på varje rad */
RUN PyjamasFarg.
ASSIGN ch_Grid:Redraw = TRUE
       BUTTON-Godkann:SENSITIVE IN FRAME {&FRAME-NAME} = 
               IF INT(ch_Grid:TextMatrix(3,ch_Grid:FixedCols - 1)) > 0 THEN YES ELSE NO
       BUTTON-Dirlev:SENSITIVE IN FRAME {&FRAME-NAME} = AVAIL BestHode AND
                                    BestHode.BestStat > 1
       ch_Grid:Row = ch_Grid:FixedRows
       ch_Grid:Col = ch_Grid:FixedCols.
               
IF iForstaCol <> ? AND iSistaCol > ch_Grid:FixedCols + wSHC# - 1 AND
                       iForstaCol <> ch_Grid:FixedCols THEN DO:
    IF iSistaCol - iForstaCol > wSHC# - 1 THEN
        ASSIGN ch_Grid:LeftCol = iForstaCol.
    ELSE
        ASSIGN ch_Grid:LeftCol = MINIMUM(iSistaCol - wSHC# + 1,iForstaCol).
    ASSIGN ch_Grid:Col = ch_Grid:LeftCol.
END.
ch_Grid:AutoSize(1,ch_Grid:Cols - 1).
APPLY "ENTRY" TO CtrlFrame.

/*----------------------------------
DEFINE VAR wIdx        AS INTE NO-UNDO.
DEFINE VAR wRow        AS INTE NO-UNDO.
DEFINE VAR wTotKassPar AS INTE NO-UNDO.
DEFINE VAR wTotCentral AS INTE NO-UNDO.
DEFINE VAR wTotBestilt AS INTE NO-UNDO.
DEFINE VAR wFargStr    AS CHAR NO-UNDO.
DEFINE VAR wButik      LIKE Butiker.Butik NO-UNDO.

ASSIGN ch_Grid = chCtrlFrame:vsFlexGrid.

ASSIGN ch_Grid:CellPictureAlignment = 1
       ch_Grid:Redraw = FALSE. /* disable repaint while populating */

ch_Grid:Clear().

ASSIGN ch_Grid:AllowUserResizing = 0    /* Fixed columns/rows */
       ch_Grid:Enabled           = TRUE /* Updateable grid */
       ch_Grid:AllowBigSelection = FALSE
       ch_Grid:Appearance        = 1 
       ch_Grid:Rows              = 6
       ch_Grid:Cols              = NUM-ENTRIES(wStorlekar," ") + 3
       ch_Grid:FixedRows         = 5
       ch_Grid:FixedCols         = 3
       ch_Grid:TextStyle         = 0
       ch_Grid:TextStyleFixed    = 0
       ch_Grid:ColWidth(1)       = 615
   /*    ch_Grid:TextMatrix(0,0)   = "Butik " */
       ch_Grid:TextMatrix(0,ch_Grid:FixedCols - 2)   = "Kasser "
       ch_Grid:TextMatrix(0,ch_Grid:FixedCols - 1)   = "Total "
       ch_Grid:TextMatrix(2,0)   = "Fri "
       ch_Grid:TextMatrix(1,0)   = "Kasser "
       ch_Grid:TextMatrix(3,0)   = "Beställt"
       ch_grid:HonorProKeys      = false.
       wOrgFriFGCol              = ch_Grid:Cell(7,0,0,0,0).
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
FIND SysPara WHERE SysPara.SysHId = 5 AND SysPara.SysGr  = 10 AND
                   SysPara.ParaNr = 10 NO-LOCK NO-ERROR.
ASSIGN wFriFGCol = IF AVAIL SysPara AND SysPara.Parameter1 <> "" THEN
                              INT(SysPara.Parameter1)
                          ELSE 16777215. 
    

/*
DO wIdx = 0 TO ch_Grid:Cols - 1:
    ASSIGN ch_Grid:Row = 0
           ch_Grid:Col = wIdx.
           ch_Grid:CellBackColor = 16777215.
END.
/* Initiering av bakgrundsfärg för rad 4 */
DO wIdx = 0 TO ch_Grid:Cols - 1:
    ASSIGN ch_Grid:Row = 3
           ch_Grid:Col = wIdx.
           ch_Grid:CellBackColor = 16777215.
END.
*/

/* Initiering av storleksrad */
DO wIdx = 1 to NUM-ENTRIES(wStorlekar," "):
    ASSIGN ch_Grid:TextMatrix(0,wIdx + ch_Grid:FixedCols - 1) = ENTRY(wIdx,wStorlekar," ") + " ".
           ch_Grid:ColWidth(wIdx + ch_Grid:FixedCols - 1) = 510.
END.
/* Kasser */
IF wBestHodeRecid = ? THEN DO:
    DO wIdx = 1 to NUM-ENTRIES(wStorlekar," "):
        ASSIGN ch_Grid:TextMatrix(1,wIdx + ch_Grid:FixedCols - 1) = "0 ".
    END.
END.
ELSE DO:
    FOR EACH tLevSort WHERE tLevSort.Fri = FALSE:
        wTotKassPar = wTotKassPar + tLevSort.AntSort * tLevSort.Antal.
        RUN ChgKasse(tLevSort.AntSort,YES).
    END.
END.
ASSIGN ch_Grid:TextMatrix(1,ch_Grid:FixedCols - 1) = IF wBestHodeRecid = ? THEN
                                        "0 "
                                    ELSE STRING(wTotKassPar) + " ".
/* END Kasser */

/* TEST 
DISPLAY wStorlekar SKIP wFriAntal WITH FRAME Gurre OVERLAY .
PAUSE.
HIDE FRAME gurre NO-PAUSE.
 TEST Slutt */

/* Fri */
ASSIGN ch_Grid:TextMatrix(2,ch_Grid:FixedCols - 1) = STRING(wFriAnt) + " ".
DO wIdx = 1 TO NUM-ENTRIES(wStorlekar," "):
    ASSIGN ch_Grid:TextMatrix(2,wIdx + ch_Grid:FixedCols - 1) = STRING( wFriAntal[wIdx]) + " ".
END.  
/* END Fri */

/* Beställningsrad */
DO wIdx = ch_Grid:FixedCols - 1 to ch_Grid:Cols - 1:
    ASSIGN ch_Grid:TextMatrix(3,wIdx) = STRING(INT(ch_Grid:TextMatrix(1,wIdx)) 
                                                + INT(ch_Grid:TextMatrix(2,wIdx))) + " ".
END.
/* END Beställningsrad */

ASSIGN ch_Grid:TextMatrix(4,0)   = "CL " + STRING(wCentralLager) + " ".

/* Centrallager */
IF wBestHodeRecid = ? THEN DO:
    /* KASSESTUFF */
/*    FOR EACH tBestKasse: tBestKasse.Butik = wCentralLager. END. */
    /* END KASSESTUFF */

    DO wIdx = ch_Grid:FixedCols - 1 to ch_Grid:Cols - 1:
        ASSIGN ch_Grid:TextMatrix(4,wIdx) = "0 ".
    END.
END.
ELSE DO:
    DO wIdx = 1 to NUM-ENTRIES(wStorlekar," "):
        FIND BestStr WHERE BestStr.BestNr   = BestHode.BestNr            AND
                           BestStr.Butik    = wCentralLager              AND
                           BestStr.Storl    = ENTRY(wIdx,wStorlekar," ") AND
                           BestStr.BestStat = BestHode.BestStat NO-LOCK NO-ERROR.
        IF AVAIL BestStr THEN
            ASSIGN wTotCentral = wTotCentral + BestStr.Bestilt
                   ch_Grid:TextMatrix(4,wIdx + ch_Grid:FixedCols - 1) = STRING(BestStr.Bestilt) + " ".
        ELSE
            ASSIGN ch_Grid:TextMatrix(4,wIdx + ch_Grid:FixedCols - 1) = "0 ".
    END.
    ASSIGN ch_Grid:TextMatrix(4,ch_Grid:FixedCols - 1) = STRING(wTotCentral) + " ".
END.
/* END Centrallager */

/* Finn resten av butikerna och det som är beställt */
ASSIGN wRow = 5. /* Skall stå här, gäller både för IF och ELSE */
IF wBestHodeRecid = ? THEN DO:
  RUN InitButiker.
END.
ELSE DO:
    FOR EACH BestLinje OF BestHode WHERE BestLinje.Butik <> wCentralLager NO-LOCK:
        ASSIGN wTotBestilt                = 0
               ch_Grid:Rows               = wRow + 1
               ch_Grid:TextMatrix(wRow,0) = STRING(BestLinje.Butik) + " ".
       
        FOR EACH BestStr OF BestLinje WHERE BestStr.BestStat = BestHode.BestStat NO-LOCK:
            ASSIGN wIdx = LOOKUP(TRIM(BestStr.Storl),wStorlekar," ")
                   wTotBestilt = wTotBestilt + BestStr.Bestilt
                   ch_Grid:TextMatrix(wRow,wIdx + ch_Grid:FixedCols - 1) = 
                   IF BestStr.Bestilt > 0 THEN STRING(BestStr.Bestilt) + " " ELSE "".
        END.
        ASSIGN ch_Grid:TextMatrix(wRow,ch_Grid:FixedCols - 1) = STRING(wTotBestilt) + " "
               wRow = wRow + 1.
    END.
END.
IF AVAIL BestHode AND BestHode.DirekteLev THEN DO:
    IF CAN-FIND(FriButik OF BestHode WHERE FriButik.Butik = wCentralLager) THEN
        ASSIGN ch_Grid:Cell(7,ch_Grid:FixedRows - 1,0,ch_Grid:FixedRows - 1,0) = wFriFGCol.
    DO wRow = ch_Grid:FixedRows TO ch_Grid:Rows - 1:
        ASSIGN wButik = INT(ch_Grid:TextMatrix(wRow,0)).
        IF CAN-FIND(FriButik OF BestHode WHERE FriButik.Butik = wButik) THEN
            ASSIGN ch_Grid:Cell(7,wRow,0,wRow,0) = wFriFGCol.
    END.
END.
/* test CellAlignment */

ASSIGN ch_Grid:Cell(2,1,1,ch_Grid:Rows - 1, ch_Grid:Cols - 1) = 7.

/* end test */
ASSIGN ch_Grid:Redraw = TRUE
       BUTTON-Godkann:SENSITIVE IN FRAME {&FRAME-NAME} = 
               IF INT(ch_Grid:TextMatrix(3,ch_Grid:FixedCols - 1)) > 0 THEN YES ELSE NO
       BUTTON-Dirlev:SENSITIVE IN FRAME {&FRAME-NAME} = AVAIL BestHode AND
                                    BestHode.BestStat > 1
       ch_Grid:Row = ch_Grid:FixedRows
       ch_Grid:Col = ch_Grid:FixedCols.
APPLY "ENTRY" TO CtrlFrame.
-----------------------------------*/
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
    DEF VAR wLoop1     AS INTE NO-UNDO.
    DEF VAR wIdx       AS INTE NO-UNDO.
    DEF VAR wAntal     AS INTE NO-UNDO.
    DEF VAR wFordel    AS CHAR NO-UNDO.
    DEF VAR wStrlIntv  AS CHAR NO-UNDO.
    DEF VAR wStrTmp    AS CHAR NO-UNDO.
    DEF VAR wStrTstr   AS CHAR NO-UNDO. /* Alla storlekar i en StrType */
    DEF VAR wMsgString AS CHAR NO-UNDO. /* vid fel i levsort */
    DEF VAR wAnt2      AS INT NO-UNDO.
/*  ASSIGN wFriSort = CAN-FIND(FIRST LevSort OF LevBas WHERE LevSort.StrType = ArtBas.StrType AND
                                     LevSort.Fri = YES).
  */
  ASSIGN wFriSort = YES.                                   
  IF wBestHodeRecid = ? THEN DO:
    FIND StrType OF ArtBas NO-LOCK NO-ERROR.
    IF NOT AVAIL StrType THEN DO:
        MESSAGE "Størrelsetype mangler." VIEW-AS ALERT-BOX ERROR 
                                          TITLE "Størrelsetype mangler".
        APPLY "WINDOW-CLOSE" TO THIS-PROCEDURE.
    END.
    
    CREATE FritLevSort.
    ASSIGN FritLevSort.SortID = "FRI"
           FritLevSort.Fri    = TRUE
           wAnt2              = 0.
    STORRELSER:
    FOR EACH StrTstr OF StrType NO-LOCK:
        ASSIGN
            wAnt2 = wAnt2 + 1
            FritLevSort.Storrelser = FritLevSort.Storrelser + 
                  IF FritLevSort.Storrelser = "" THEN
                      left-trim(StrTStr.SoStorl) ELSE " " + left-trim(StrTStr.SoStorl).
        IF wAnt2 > 50 THEN
            LEAVE STORRELSER.
    END. /* STORRELSER */
    ASSIGN wStorlekar = FritLevSort.Storrelser.
    FOR EACH LevSort OF LevBas WHERE LevSort.StrType = ArtBas.StrType AND
                                     LevSort.Fri = NO NO-LOCK:
        IF CAN-FIND(FIRST LevSant OF LevSort WHERE LevSant.SoAnt > 0) THEN
            RUN SkapatLevSort ("INIT").
        IF RETURN-VALUE = "FEIL" THEN 
            ASSIGN wMsgString = wMsgString + (IF wMsgString = "" THEN "" ELSE ",") +
                LevSort.SortID. /* "MESSAGE längre ner. */
    END.
  END. /* END wBestHodeRecid = ? */
  ELSE DO:
      FIND FIRST BestSort OF BestHode WHERE BestSort.Fri = YES NO-LOCK NO-ERROR.
      IF NOT AVAIL BestSort THEN DO:
        message "Mangler BestSort.Fri? (TN)" view-as alert-box.
      END.
      if available BestSort then DO:

          ASSIGN 
              wStorlekar = BestSort.Storrelser.
          CREATE FritLevSort.
          ASSIGN FritLevSort.SortID     = BestSort.SortID
                 FritLevSort.Fri        = BestSort.Fri
                 FritLevSort.Storrelser = BestSort.Storrelser
                 FritLevSort.Fordeling  = BestSort.Fordeling.
      end.
      FOR EACH BestSort OF BestHode WHERE BestSort.Fri = NO NO-LOCK:
          IF NOT CAN-FIND(LevSort WHERE LevSort.LevNr = LevBas.LevNr AND
                          LevSort.SortId = BestSort.SortId) THEN
              NEXT.
          RELEASE tLevSort.
          CREATE tLevSort.
          BUFFER-COPY BestSort TO tLevSort.
          RELEASE tLevSort.
      END.
      FOR EACH LevSort OF LevBas WHERE LevSort.StrType = ArtBas.StrType AND
                                       LevSort.Fri = NO NO-LOCK:
          IF NOT CAN-FIND(FIRST tLevSort WHERE tLevSort.SortID = LevSort.SortID) AND
                 CAN-FIND(FIRST LevSant OF LevSort WHERE LevSant.SoAnt > 0) THEN DO:
              RUN SkapatLevSort ("VEDLIKEHOLD").
              IF RETURN-VALUE = "FEIL" THEN 
                  ASSIGN wMsgString = wMsgString + (IF wMsgString = "" THEN "" ELSE ",") +
                               LevSort.SortID. /* "MESSAGE längre ner. */
          END.
      END.
      IF BestHode.LevNr = CB-Lev AND BestHode.DirekteLev = TRUE THEN DO: 
          FOR EACH BestKasse NO-LOCK WHERE BestKasse.BestNr = BestHode.BestNr AND
              CAN-FIND(LevSort WHERE LevSort.LevNr = LevBas.LevNr AND LevSort.SortId = BestKasse.SortId):
              CREATE tBestKasse.
              BUFFER-COPY BestKasse TO tBestKasse.
              RELEASE tBestKasse.
              BUFFER-COPY BestKasse TO InitBestKasse.
              RELEASE InitBestKasse.
          END.
      END.
      /* inte direketelev -> finns en fributik. wfriantal skall innehålla alla fributikers antal */
      FOR EACH FriButik OF BestHode NO-LOCK:
          IDXLOOP:
          DO wIdx = 1 TO NUM-ENTRIES(wStorLekar," "):
              IF wIdx > 50 THEN LEAVE IDXLOOP.
              ASSIGN wFriAntal[wIdx] = wFriAntal[wIdx] + FriButik.FriAntal[wIdx]
                     wFriAnt         = wFriAnt         + FriButik.FriAntal[wIdx].
          END. /* IDXLOOP */
          CREATE tFriButik.
          BUFFER-COPY FriButik TO tFriButik.
          RELEASE tFriButik.
      END.
  END.
  IF wMsgString <> "" THEN
      MESSAGE "I sortiment " wMsgString " finnes størrelse(r) som ikke" SKIP
              "eksisterer i størrelsesdefinisjonen (for denne bestillingen)."
              view-as alert-box INFORMATION TITLE "".
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitStringTranslation C-Win 
PROCEDURE InitStringTranslation :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM        ihWindow    AS HANDLE NO-UNDO.
DEF INPUT-OUTPUT PARAM iocTypeList AS CHAR   NO-UNDO.

IF ihWindow NE THIS-PROCEDURE:CURRENT-WINDOW THEN RETURN.

/* IF NOT CAN-DO(iocTypeList,"BUTTON-Godkann") THEN */
/*   iocTypeList = iocTypeList + ",BUTTON-Godkann". */
/* IF NOT CAN-DO(iocTypeList,"WINDOWTITLE") THEN    */
/*   iocTypeList = iocTypeList + ",WINDOWTITLE".    */
/* IF NOT CAN-DO(iocTypeList,"GRID") THEN           */
/*   iocTypeList = iocTypeList + ",GRID".           */

/* DYNAMIC-FUNCTION("getStringTranslation",THIS-PROCEDURE:FILE-NAME,"BUTTON-Godkann","&Godkjenn|Til &ordre...|Fra &ordre"). */
  
/* ASSIGN ch_Grid:TextMatrix(0,ch_Grid:FixedCols - 2) = DYNAMIC-FUNCTION("getStringTranslation",THIS-PROCEDURE:FILE-NAME,"GRID","Kasser ") */
/*        ch_Grid:TextMatrix(2,0) = DYNAMIC-FUNCTION("getStringTranslation",THIS-PROCEDURE:FILE-NAME,"GRID","Fri ")                        */
/*        ch_Grid:TextMatrix(1,0) = DYNAMIC-FUNCTION("getStringTranslation",THIS-PROCEDURE:FILE-NAME,"GRID","Kasser ")                     */
/*        ch_Grid:TextMatrix(3,0) = DYNAMIC-FUNCTION("getStringTranslation",THIS-PROCEDURE:FILE-NAME,"GRID","Bestilt")                     */
/*        .                                                                                                                                */

RUN InitButtons.
RUN SetWindowTitle.

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
    DEF VAR wCount1     AS INTE NO-UNDO.
    DEF VAR wCount2     AS INTE NO-UNDO.
    DEF VAR wRow        AS INTE NO-UNDO.
    DEF VAR wTotTmp     AS INTE NO-UNDO.
    DEF VAR wAntal      AS INTE NO-UNDO.
    DEF VAR wIdx        AS INTE NO-UNDO.
    DEF VAR wCellMinus  AS INTE NO-UNDO.
    DEF VAR wButik      LIKE Butiker.Butik NO-UNDO.
    DEF VAR wInpAntal   AS INTE NO-UNDO.
    DEF VAR wDirekteLev AS LOGI NO-UNDO.
    DEF VAR             wMatrisIdx AS INTE NO-UNDO.

    ASSIGN wDirekteLev = BestHode.DirekteLev:CHECKED IN FRAME DEFAULT-FRAME.

    CASE wMenyValg:
       WHEN "m_nysortering"  OR WHEN "m_n_nysortering" THEN DO:
         ASSIGN wInpAntal = 1.
         IF wMenyValg = "m_n_nysortering" THEN DO:
            MESSAGE "Registrer antall sorteringer:" update wInpAntal FORMAT ">9".
            IF wInpAntal = ? OR wInpAntal = 0 THEN
               RETURN.
         END.
         IF wDirekteLev = TRUE THEN
           FIND tBestKasse WHERE tBestKasse.Butik = wCentrallager AND
                                 tBestKasse.SortID   = tLevSort.SortID.
         NYSORT:
         DO wAntal = 1 TO wInpAntal:
           DO wRow = wFstRow TO wLstRow:
             IF wDirekteLev THEN DO:
                IF tBestKasse.Antal = 0 THEN
                   LEAVE NYSORT.
                ASSIGN tBestKasse.Antal = tBestKasse.Antal - 1.
             END.
             ASSIGN wTotTmp = 0.
             IF wDirekteLev THEN DO:
                ASSIGN wButik = INT(ch_Grid:TextMatrix(wRow,0)).
                   FIND BtBestKasse WHERE BtBestKasse.SortID   = tLevSort.SortID AND
                                          BtBestKasse.Butik    = wButik NO-ERROR.
                IF NOT AVAIL BtBestKasse THEN DO:
                   CREATE BtBestKasse.
                   ASSIGN BtBestKasse.BestNr   = IF wBestHodeRecid = ? THEN ? ELSE BestHode.BestNr
                          BtBestKasse.SortID   = tLevSort.SortID
                          BtBestKasse.Butik    = wButik
                          BtBestKasse.Antal    = 0.
                 END.
                 ASSIGN BtBestKasse.Antal  = BtBestKasse.Antal + 1.
             END.
             DO wCount1 = 1 TO NUM-ENTRIES(tLevSort.Storrelser," "):
               wMatrisIdx =  LOOKUP(ENTRY(wCount1,tLevSort.Storrelser," "),wStorlekar," ").
               IF INT(ch_Grid:TextMatrix(4,wMatrisIdx + ch_Grid:FixedCols - 1)) >= INT(ENTRY(wCount1,tLevSort.Fordel," ")) THEN DO:
                  ASSIGN ch_Grid:TextMatrix(wRow,wMatrisIdx + ch_Grid:FixedCols - 1) = 
                                 STRING(INT(ch_Grid:TextMatrix(wRow,wMatrisIdx + ch_Grid:FixedCols - 1)) +
                                        INT(ENTRY(wCount1,tLevSort.Fordel," "))) + " "
                         ch_Grid:TextMatrix(4,wMatrisIdx + ch_Grid:FixedCols - 1) = 
                                 STRING(INT(ch_Grid:TextMatrix(4,wMatrisIdx + ch_Grid:FixedCols - 1)) -
                                        INT(ENTRY(wCount1,tLevSort.Fordel," "))) + " "
                         wTotTmp = wTotTmp + INT(ENTRY(wCount1,tLevSort.Fordel," ")).
               END.
             END.
             ASSIGN ch_Grid:TextMatrix(wRow,ch_Grid:FixedCols - 1) = 
                                   STRING(INT(ch_Grid:TextMatrix(wRow,ch_Grid:FixedCols - 1)) + wTotTmp) + " "
                    ch_Grid:TextMatrix(4,ch_Grid:FixedCols - 1) =
                                   STRING(INT(ch_Grid:TextMatrix(4,ch_Grid:FixedCols - 1)) - wTotTmp) + " ".
           END.
         END.
       END.
       WHEN "m_delsortering" THEN DO:

         FIND tBestKasse WHERE tBestKasse.Butik = wCentrallager AND
                               tBestKasse.SortID   = tLevSort.SortID NO-ERROR.
         IF NOT AVAIL tBestKasse THEN DO:
             CREATE tBestKasse.
             ASSIGN tBestKasse.BestNr   = IF wBestHodeRecid = ? THEN ? ELSE BestHode.BestNr
                    tBestKasse.SortID   = tLevSort.SortID
                    tBestKasse.Butik    = wCentrallager
                    tBestKasse.Antal    = 0.
         END.
         DO wRow = wFstRow TO wLstRow:
           wTotTmp = 0.
           IF wDirekteLev THEN DO:
               ASSIGN wButik = INT(ch_Grid:TextMatrix(wRow,0)).

               FIND BtBestKasse WHERE BtBestKasse.SortID   = tLevSort.SortID AND
                                      BtBestKasse.Butik    = wButik NO-ERROR.
               IF NOT AVAIL BtBestKasse THEN
                   NEXT.
               ASSIGN BtBestKasse.Antal  = BtBestKasse.Antal - 1
                      tBestKasse.Antal   = tBestKasse.Antal + 1.
               IF BtBestKasse.Antal = 0 THEN DO:
                   IF wBestHodeRecid <> ? THEN DO:
                       FIND BestKasse WHERE BestKasse.BestNr   = BtBestKasse.BestNr AND
                                            BestKasse.SortID   = BtBestKasse.SortID AND
                                            BestKasse.Butik    = BtBestKasse.Butik NO-ERROR.
                       IF AVAIL BestKasse THEN
                           DELETE BestKasse.
                   END.
                   DELETE BtBestKasse.
               END.
           END.
           DO wCount1 = 1 TO NUM-ENTRIES(tLevSort.Storrelser," "):
               wMatrisIdx =  LOOKUP(ENTRY(wCount1,tLevSort.Storrelser," "),wStorlekar," ").
               IF INT(ch_Grid:TextMatrix(wRow,wMatrisIdx + ch_Grid:FixedCols - 1)) >= INT(ENTRY(wCount1,tLevSort.Fordel," ")) THEN DO:
                   ASSIGN ch_Grid:TextMatrix(wRow,wMatrisIdx + ch_Grid:FixedCols - 1) = 
                                       STRING(INT(ch_Grid:TextMatrix(wRow,wMatrisIdx + ch_Grid:FixedCols - 1)) -
                                              INT(ENTRY(wCount1,tLevSort.Fordel," "))) + " "
                          ch_Grid:TextMatrix(wRow,wMatrisIdx + ch_Grid:FixedCols - 1) = IF INT(ch_Grid:TextMatrix(wRow,wMatrisIdx + ch_Grid:FixedCols - 1)) = 0 THEN ""
                                                                          ELSE ch_Grid:TextMatrix(wRow,wMatrisIdx + ch_Grid:FixedCols - 1)
                          ch_Grid:TextMatrix(4,wMatrisIdx + ch_Grid:FixedCols - 1) = 
                                       STRING(INT(ch_Grid:TextMatrix(4,wMatrisIdx + ch_Grid:FixedCols - 1)) +
                                              INT(ENTRY(wCount1,tLevSort.Fordel," "))) + " "
                          wTotTmp = wTotTmp + INT(ENTRY(wCount1,tLevSort.Fordel," ")).
               END.
           END.
           ASSIGN ch_Grid:TextMatrix(wRow,ch_Grid:FixedCols - 1) = 
                                       STRING(INT(ch_Grid:TextMatrix(wRow,ch_Grid:FixedCols - 1)) - wTotTmp) + " "
                  ch_Grid:TextMatrix(4,ch_Grid:FixedCols - 1) =
                                       STRING(INT(ch_Grid:TextMatrix(4,ch_Grid:FixedCols - 1)) + wTotTmp) + " ".
          END.
       END.
       WHEN "m_nystaket"  OR WHEN "m_n_nystaket" THEN DO:    /* + 1 i område  */
           ASSIGN wInpAntal = 1.
           IF wMenyValg = "m_n_nystaket" THEN DO:
               MESSAGE "Angi antall i område:" UPDATE wInpAntal FORMAT ">9".
               IF wInpAntal = ? OR wInpAntal = 0 THEN
                   RETURN.
           END.
           DO wAntal = 1 TO wInpAntal:
             DO wCount1 = wFstRow TO wLstRow:
               DO wCount2 = wFstCol TO wLstCol:
                 IF INT(ch_Grid:TextMatrix(4,wCount2)) > 0 THEN
                   ASSIGN ch_Grid:TextMatrix(4,wCount2) = 
                                 STRING(INT(ch_Grid:TextMatrix(4,wCount2)) - 1) + " "
                          ch_Grid:TextMatrix(wCount1,wCount2) = 
                                 STRING(INT(ch_Grid:TextMatrix(wCount1,wCount2)) + 1) + " "
                          ch_Grid:TextMatrix(wCount1,ch_Grid:FixedCols - 1) = 
                                 STRING(INT(ch_Grid:TextMatrix(wCount1,ch_Grid:FixedCols - 1)) + 1) + " "
                          ch_Grid:TextMatrix(4,ch_Grid:FixedCols - 1) =
                                 STRING(INT(ch_Grid:TextMatrix(4,ch_Grid:FixedCols - 1)) - 1) + " ".
               END.
             END.
           END.
       END.
       WHEN "m_delstaket"    THEN DO:    /* Ta &bort 1 i område    */
               DO wCount1 = wFstRow TO wLstRow:
                   DO wCount2 = wFstCol TO wLstCol:
                       IF INT(ch_Grid:TextMatrix(wCount1,wCount2)) > 0 THEN
                           ASSIGN ch_Grid:TextMatrix(4,wCount2) =
                                               STRING(INT(ch_Grid:TextMatrix(4,wCount2)) + 1) + " "
                                  ch_Grid:TextMatrix(wCount1,wCount2) = 
                                      (IF INT(ch_Grid:TextMatrix(wCount1,wCount2)) - 1 > 0 THEN
                                         STRING(INT(ch_Grid:TextMatrix(wCount1,wCount2)) - 1)  + " " ELSE
                                         "")
                                  ch_Grid:TextMatrix(wCount1,ch_Grid:FixedCols - 1) = 
                                         STRING(INT(ch_Grid:TextMatrix(wCount1,ch_Grid:FixedCols - 1)) - 1) + " "
                                  ch_Grid:TextMatrix(4,ch_Grid:FixedCols - 1) =
                                             STRING(INT(ch_Grid:TextMatrix(4,ch_Grid:FixedCols - 1)) + 1) + " ".
                   END.
               END.
           END.
       WHEN "m_delruta"      THEN DO:    /* Rensa &aktuell ruta    */
             ASSIGN ch_Grid:TextMatrix(4,ch_Grid:Col) =
                                     STRING(INT(ch_Grid:TextMatrix(4,ch_Grid:Col)) + 
                                            INT(ch_Grid:Text)) + " "
                    ch_Grid:TextMatrix(4,ch_Grid:FixedCols - 1) = 
                                     STRING(INT(ch_Grid:TextMatrix(4,ch_Grid:FixedCols - 1)) + INT(ch_Grid:Text)) + " "
                    ch_Grid:TextMatrix(ch_Grid:Row,ch_Grid:FixedCols - 1) =
                                     STRING(INT(ch_Grid:TextMatrix(ch_Grid:Row,ch_Grid:FixedCols - 1)) -
                                            INT(ch_Grid:Text)) + " "
                    ch_Grid:Text = "".
           END.
       WHEN "m_delraden"     THEN DELRAD: DO:    /* Rensa &hela raden      */
               IF wDirekteLev THEN DO:
                   ASSIGN wButik = INT(ch_Grid:TextMatrix(ch_Grid:Row,0))
                          wAktivFriRad = ch_Grid:Row.
                   FIND tFriButik WHERE tFriButik.Butik = wButik NO-ERROR.
                   IF AVAIL tFriButik AND tFriButik.TotAntal > 0 THEN DO:
                       DO wIdx = 1 TO NUM-ENTRIES(wStorlekar," "):
                           IF tFriButik.FriAntal[wIdx] > 0 THEN
                               RUN ChangeFri (wIdx,0 - tFriButik.FriAntal[wIdx]).
                       END.
                   END.
                   FOR EACH BtBestKasse WHERE BtBestKasse.Butik = wButik:
                       FIND tBestKasse WHERE tBestKasse.Butik  = wCentrallager AND
                                             tBestKasse.SortID = BtBestKasse.SortID NO-ERROR.
                       IF NOT AVAIL tBestKasse THEN DO:
                           CREATE tBestKasse.
                           ASSIGN tBestKasse.BestNr = IF wBestHodeRecid = ? THEN ? ELSE
                                                          BestHode.BestNr
                                  tBestKasse.SortId = BtBestKasse.SortId
                                  tBestKasse.Butik  = wCentralLager.
                       END.
                       ASSIGN tBestKasse.Antal = tBestKasse.Antal + BtBestKasse.Antal.
                       IF wBestHodeRecid <> ? THEN DO:
                           FIND BestKasse WHERE BestKasse.BestNr   = BtBestKasse.BestNr AND
                                                BestKasse.SortID   = BtBestKasse.SortID AND
                                                BestKasse.Butik    = BtBestKasse.Butik NO-ERROR.
                           IF AVAIL BestKasse THEN
                               DELETE BestKasse.
                       END.
                       DELETE BtBestKasse.
                   END.
                   DO wCount1 = ch_Grid:FixedCols - 1 TO ch_Grid:Cols - 1:
                       ASSIGN ch_Grid:TextMatrix(4,wCount1) = 
                                     STRING(INT(ch_Grid:TextMatrix(4,wCount1)) + 
                                            INT(ch_Grid:TextMatrix(ch_Grid:Row,wCount1))) + " "
                              ch_Grid:TextMatrix(ch_Grid:Row,wCount1) = "".
                   END.
               END.
/* !!!!
               IF wDirekteLev THEN DO:
                   ASSIGN wButik = INT(ch_Grid:TextMatrix(ch_Grid:Row,0)).
                   FIND tBestKasse WHERE tBestKasse.Butik = wCentrallager AND
                                         tBestKasse.SortID   = tLevSort.SortID.
                   FIND BtBestKasse WHERE BtBestKasse.SortID   = tLevSort.SortID AND
                                          BtBestKasse.Butik    = wButik NO-ERROR.
                   IF AVAIL BtBestKasse THEN DO:
                       ASSIGN tBestKasse.Antal   = tBestKasse.Antal + BtBestKasse.Antal
                              BtBestKasse.Antal  = 0.
                       IF wBestHodeRecid <> ? THEN DO:
                           FIND BestKasse WHERE BestKasse.BestNr   = BtBestKasse.BestNr AND
                                                BestKasse.SortID   = BtBestKasse.SortID AND
                                                BestKasse.Butik    = BtBestKasse.Butik NO-ERROR.
                           IF AVAIL BestKasse THEN
                               DELETE BestKasse.
                       END.
                       DELETE BtBestKasse.
                   END.
                   FIND tFriButik WHERE tFriButik.Butik = wButik NO-ERROR.
               END.
 !!!! */     
               ELSE DO wCount1 = ch_Grid:FixedCols TO ch_Grid:Cols - 1:
                   /* IF wDirekteLev = TRUE THEN DO: */
                   IF AVAIL tFriButik AND tFriButik.Butik = wButik THEN DO:
                       ASSIGN wIdx                     = wCount1 - ch_Grid:FixedCols + 1
                              wFriAntal[wIdx]          = wFriAntal[wIdx] + tFriButik.FriAntal[wIdx]
                              tFriButik.FriAntal[wIdx] = 0.
                   END.
                   ASSIGN ch_Grid:TextMatrix(4,wCount1) = 
                                     STRING(INT(ch_Grid:TextMatrix(4,wCount1)) + 
                                            INT(ch_Grid:TextMatrix(ch_Grid:Row,wCount1))) + " "
                          wTotTmp = wTotTmp + INT(ch_Grid:TextMatrix(ch_Grid:Row,wCount1))
                          ch_Grid:TextMatrix(ch_Grid:Row,wCount1) = "".
               END.
               IF AVAIL tFriButik AND tFriButik.Butik = wButik THEN /* wDirekteLev = TRUE THEN */
                   ASSIGN tFriButik.TotAntal = 0.
               ASSIGN ch_Grid:TextMatrix(4,ch_Grid:FixedCols - 1) = 
                                     STRING(INT(ch_Grid:TextMatrix(4,ch_Grid:FixedCols - 1)) + wTotTmp) + " "
                      ch_Grid:TextMatrix(ch_Grid:Row,ch_Grid:FixedCols - 1) =
                                     STRING(INT(ch_Grid:TextMatrix(ch_Grid:Row,ch_Grid:FixedCols - 1)) - wTotTmp) + " ".
           END.
       WHEN "m_delomr"       THEN DO:    /* Rensa &markerat område */
           DO wCount1 = wFstRow TO wLstRow:
               ASSIGN wTotTmp = 0.
               DO wCount2 = wFstCol TO wLstCol:
                   ASSIGN wIdx = wCount2 - ch_Grid:FixedCols + 1.
                   IF INT(ch_Grid:TextMatrix(wCount1,wCount2)) > 0 THEN DO:
                       ASSIGN wCellMinus                          = INT(ch_Grid:TextMatrix(wCount1,wCount2))
                              ch_Grid:TextMatrix(4,wCount2)       = STRING(INT(ch_Grid:TextMatrix(4,wCount2)) + wCellMinus) + " "
                              wTotTmp                             = wTotTmp + wCellMinus
                              ch_Grid:TextMatrix(wCount1,wCount2) = IF INT(ch_Grid:TextMatrix(wCount1,wCount2)) = wCellMinus THEN ""
                                                                    ELSE STRING(INT(ch_Grid:TextMatrix(wCount1,wCount2)) - wCellMinus) + " ".
                   END.
               END.
               ASSIGN ch_Grid:TextMatrix(4,ch_Grid:FixedCols - 1) = 
                                     STRING(INT(ch_Grid:TextMatrix(4,ch_Grid:FixedCols - 1)) + wTotTmp) + " "
                      ch_Grid:TextMatrix(wCount1,ch_Grid:FixedCols - 1) =
                                     STRING(INT(ch_Grid:TextMatrix(wCount1,ch_Grid:FixedCols - 1)) - wTotTmp) + " ".
           END.
       END.
    END CASE.
    IF wDirekteLev THEN
        RUN VisKasser.
    IF wBestHodeRecid = ? AND INT(ch_Grid:TextMatrix(3,ch_Grid:FixedCols - 1)) = 0 THEN
        ASSIGN BUTTON-Lagra:SENSITIVE   IN FRAME {&FRAME-NAME} = NO
               BUTTON-Godkann:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
    ELSE
        ASSIGN BUTTON-Lagra:SENSITIVE   IN FRAME {&FRAME-NAME} = YES
               BUTTON-Kalkyl:SENSITIVE   IN FRAME {&FRAME-NAME} = NO
               BUTTON-Godkann:SENSITIVE IN FRAME {&FRAME-NAME} = NO
               BUTTON-FraBest:SENSITIVE IN FRAME {&FRAME-NAME} = NO
               BUTTON-Dirlev:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
    APPLY "ENTRY" TO CtrlFrame.
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

   IF wModus = "INIT" THEN 
     DO:
       FIND Butiker WHERE Butiker.Butik = wCentrallager NO-LOCK.
       FIND BestPris WHERE BestPris.BestNr = BestHode.BestNr AND
                       BestPris.BestStat = BestHode.BestStat AND
                       BestPris.ProfilNr = Butiker.ProfilNr NO-LOCK.
    END.
    ELSE DO:
        IF NOT AVAILABLE BestPris THEN
        FIND BestPris WHERE BestPris.BestNr = BestHode.BestNr AND
                        BestPris.BestStat = BestHode.BestStat AND
                        BestPris.ProfilNr = Butiker.ProfilNr NO-LOCK.
        IF AVAILABLE BestPris THEN
        ASSIGN BestHode.TotInnKjVerdi = BestHode.TotAntPar * BestPris.Varekost
               BestHode.TotDbKr =       BestHode.TotAntPar * BestPris.DbKr
               BestHode.TotSalgsVerdi = BestHode.TotAntPar * BestPris.Pris.
    END.

    DO WITH FRAME {&FRAME-NAME}:
        DISPLAY BestHode.TotInnKjVerdi
                BestHode.TotDbKr
                BestHode.TotSalgsVerdi
                BestPris.Varekost WHEN AVAIL BestPris
                BestPris.DbKr     WHEN AVAIL BestPris
                BestPris.Pris     WHEN AVAIL BestPris.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PyjamasFarg C-Win 
PROCEDURE PyjamasFarg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR wRow AS INTE NO-UNDO.
    DO wRow = ch_Grid:FixedRows TO ch_Grid:Rows - 1:
        IF wRow MOD 2 = 1 THEN
            ASSIGN ch_Grid:Cell(6,wRow,ch_Grid:FixedCols,wRow,ch_Grid:Cols - 1) = 10813439.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettModified C-Win 
PROCEDURE SettModified :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER wModified AS LOGI NO-UNDO.
  DO WITH FRAME DEFAULT-FRAME:
    ASSIGN BestHode.Beskrivelse:MODIFIED     = wModified
           BestHode.LevDato:MODIFIED         = wModified
           BestHode.BestillingsDato:MODIFIED = wModified
           BestHode.DirekteLev:MODIFIED      = wModified
           BestHode.Merknad:MODIFIED         = wModified
           TOGGLE-Tillagg:MODIFIED           = wModified
           CB-Team:MODIFIED                  = wModified
           CB-Lev:MODIFIED                   = wModified
           BestHode.BekreftetOrdre:MODIFIED  = wModified
           BestHode.BekreftetDato:MODIFIED   = wModified.
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
DEF VAR cTranslation AS CHAR NO-UNDO.

/* cTranslation = DYNAMIC-FUNCTION("getStringTranslation",THIS-PROCEDURE:FILE-NAME,"WINDOWTITLE","Bestilling artikkel: | Best.nr: |NY| Status: | Ordre: | (Sendt "). */
/*                                                                                                                                                                   */
/*   ASSIGN CURRENT-WINDOW:HIDDEN = NO                                                                                                                               */
/*          CURRENT-WINDOW:TITLE = ENTRY(1,cTranslation,"|") +                                                                                                       */
/*            (IF AVAILABLE ArtBas                                                                                                                                   */
/*               THEN STRING(ArtBas.ArtikkelNr)                                                                                                                      */
/*               ELSE "***")  + " " + ENTRY(2,cTranslation,"|") +                                                                                                    */
/*            IF NOT AVAIL BestHode THEN  ENTRY(3,cTranslation,"|") ELSE                                                                                             */
/*            STRING(BestHode.BestNr) + " " + ENTRY(4,cTranslation,"|") + ENTRY(BestHode.BestStat,wStatus) +                                                         */
/*            (IF AVAIL BestHode AND BestHode.OrdreNr <> 0 THEN ENTRY(5,cTranslation,"|") +                                                                          */
/*            STRING(BestHode.OrdreNr) ELSE "") +                                                                                                                    */
/*            (IF BestHode.SendtDato <> ? THEN ENTRY(6,cTranslation,"|") + STRING(BestHode.SendtDato) + " " +                                                        */
/*                        STRING(BestHode.SendtTid,"HH:MM") + " " +                                                                                                  */
/*                        BestHode.SendtAv + ")" ELSE "").                                                                                                           */

  ASSIGN CURRENT-WINDOW:HIDDEN = NO
         CURRENT-WINDOW:TITLE = "Bestilling artikkel: " +
          (IF AVAILABLE ArtBas
             THEN STRING(ArtBas.ArtikkelNr)
             ELSE "***")  + " " + " Best.nr: " +
          IF NOT AVAIL BestHode THEN  "NY" ELSE
          STRING(BestHode.BestNr) + " " + " Status: " + ENTRY(BestHode.BestStat,wStatus) +
          (IF AVAIL BestHode AND BestHode.OrdreNr <> 0 THEN " Order: " +
          STRING(BestHode.OrdreNr) ELSE "") +
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapatLevSort C-Win 
PROCEDURE SkapatLevSort :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER wType AS CHAR NO-UNDO.
    DEF VAR wAntal            AS INTE NO-UNDO.
    DEF VAR wFordel           AS CHAR NO-UNDO.
    DEF VAR wStrlIntv         AS CHAR NO-UNDO.
    DEF VAR wStrTmp           AS CHAR NO-UNDO.
    DEF VAR wStrTstr          AS CHAR NO-UNDO. /* Alla storlekar i en StrType */

    DO:
        ASSIGN wAntal   = 0
               wFordel  = ""
               wStrlIntv = ""
               wStrTmp = "".
        FOR EACH LevsAnt OF LevSort NO-LOCK BY LevSAnt.SortId
                                            BY LevSAnt.SeqNr:
            ASSIGN wAntal     = wAntal + LevsAnt.SoAnt
                   wFordel    = IF wFordel = "" THEN
                                    String(LevSant.SoAnt) + " "
                                ELSE
                                    wFordel + String(LevSant.SoAnt) + " "
                   wStrTmp = IF wStrTmp = "" THEN
                                    left-trim(LevSant.SoStorl)
                                ELSE
                                    wStrTmp + " " + left-trim(LevSant.SoStorl).
            IF NOT CAN-DO(REPLACE(wStorlekar," ",","),left-trim(LevSant.SoStorl)) THEN
                RETURN "FEIL".
        END.
        IF wStrTmp = "" THEN 
            NEXT.
        ASSIGN wStrlIntv = ENTRY(1,wStrTmp," ") + " - " +
                           ENTRY(NUM-ENTRIES(wStrTmp," "),wStrTmp," ").
        CREATE tLevSort.
        ASSIGN tLevSort.AntSort       = 0
               tLevSort.SortID        = LevSort.SortID
               tLevSort.Antall        = wAntal
               tLevSort.Fordeling     = wFordel
               tLevSort.StrInterval   = wStrlIntv
               tLevSort.Storrelser    = wStrTmp.
        IF wType = "VEDLIKEHOLD" AND BestHode.DirekteLev:CHECKED IN FRAME DEFAULT-FRAME THEN DO:
            CREATE tBestKasse.
            ASSIGN tBestKasse.BestNr   = IF wBestHodeRecid = ? THEN ? ELSE BestHode.BestNr
                   tBestKasse.SortID   = tLevSort.SortID
                   tBestKasse.Butik    = wCentralLager
                   tBestKasse.Antal    = tLevSort.AntSort.
            RELEASE tBestKasse.                  

        END.
        RELEASE tLevSort.
    END.
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
  FOR EACH Etikett NO-LOCK WHERE Etikett.LevInNr = BestHode.BestNr BREAK BY Etikett.butik:
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
  DEFINE INPUT  PARAMETER dArtikkelNr AS DECIMAL    NO-UNDO.
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
  FIND ArtBas WHERE ArtBas.ArtikkelNr = dArtikkelNr NO-LOCK NO-ERROR.
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
  FOR EACH Etikettlogg:
      DELETE EtikettLogg.
  END.
end.                

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
DEFINE BUFFER bLevBas FOR LevBas.
do with frame {&FRAME-NAME}:
  find bLevBas    of ArtBas no-lock no-error.
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
    ArtBas.LevKod.
/*     LevBas.LevNamn. */
  IF wBestHodeRecid = ? AND (ArtBas.LevDato1 >= TODAY OR ArtBas.LevDato2 >= TODAY) THEN 
      ASSIGN BestHode.LevDato:SCREEN-VALUE = IF ArtBas.LevDato1 >= TODAY THEN
          STRING(ArtBas.LevDato1) ELSE STRING(ArtBas.LevDato2).
  IF Avail BestHode THEN
      ASSIGN ArtBas.LevKod:SCREEN-VALUE = IF AVAIL ArtBas THEN
                    ArtBas.LevKod ELSE ""
             ArtBas.LevFargKod:SCREEN-VALUE = IF AVAIL ArtBas THEN
                    ArtBas.LevFargKod ELSE "".

  FILL-IN-Vgr-Lopnr:SCREEN-VALUE = STRING(ArtBas.Vg) + 
                                   "/" + 
                                   (if (ArtBas.LopNr = ? or ArtBas.LopNr = 0)
                                      then " "
                                      else STRING(ArtBas.LopNr)).
/*    
    if available Bilderegister then
      do:
        display
          Bilderegister.BildNr @ ArtBas.BildNr 
          BildeRegister.Tekst
        with frame FRAME-ArtInfo.

        run VisBilde (1).
      end.
    else do:
      display 
        "" @ BildeRegister.Tekst
      with frame FRAME-ArtInfo.

      run VisBilde (2).
    end.    */
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisKasser C-Win 
PROCEDURE VisKasser :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VAR wRow AS INTE NO-UNDO.
    DEFINE VAR wButik LIKE Butiker.Butik NO-UNDO.
    DEFINE VAR wFritt AS CHAR NO-UNDO.
    DO WITH FRAME DEFAULT-FRAME:
        FIND FIRST tBestKasse WHERE tBestKasse.SortID = tLevSort.SortID AND
                                    tBestKasse.Butik  = wCentralLager NO-ERROR.
    /*    ASSIGN wFritt = IF CAN-FIND(tFriButik WHERE tFriButik.Butik = wCentralLager AND
                                                    tFriButik.TotAnt > 0) THEN " *" ELSE "". */
        IF AVAIL tBestKasse THEN DO:
            ASSIGN ch_Grid:TextMatrix(4,1) = STRING(tBestKasse.Antal) + IF wFritt <> "" THEN wFritt + " " ELSE " ".
        END.
        ELSE
            ASSIGN ch_Grid:TextMatrix(4,1) = IF wFritt <> "" THEN "  " + wFritt ELSE "".
        DO wRow = 5 TO ch_Grid:Rows - 1:
            ASSIGN wButik = INT(ch_Grid:TextMatrix(wRow,0)).
    /*               wFritt = IF CAN-FIND(tFriButik WHERE tFriButik.Butik = wButik AND
                                                    tFriButik.TotAnt > 0) THEN " *" ELSE "". */
            FIND FIRST BtBestKasse WHERE BtBestKasse.SortID = tLevSort.SortID AND
                                         BtBestKasse.Butik  = wButik NO-ERROR.
            IF AVAIL BtBestKasse THEN
                ASSIGN ch_Grid:TextMatrix(wRow,1) = STRING(BtBestKasse.Antal) + IF wFritt <> "" THEN wFritt + " " ELSE " ".
            ELSE
                ASSIGN ch_Grid:TextMatrix(wRow,1) =  IF wFritt <> "" THEN "  " + wFritt ELSE " ".
        END.
        ASSIGN CB-Lev:SENSITIVE = NOT (AVAIL BestHode AND BestHode.BestStat > 2 OR 
                               NOT NUM-ENTRIES(CB-Lev:LIST-ITEM-PAIRS) > 2 OR harBestillt()).
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
      SysPara.SysGr  = 10 AND
      SysPara.ParaNr < 6 NO-LOCK:
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION harBestillt C-Win 
FUNCTION harBestillt RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN INT(ch_Grid:TextMatrix(3,2)) > 0.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Modifierad C-Win 
FUNCTION Modifierad RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DO WITH FRAME DEFAULT-FRAME:
    RETURN IF BestHode.Beskrivelse:MODIFIED = TRUE OR
              BestHode.LevDato:MODIFIED = TRUE OR
              BestHode.BestillingsDato:MODIFIED = TRUE OR
              BestHode.DirekteLev:MODIFIED = TRUE OR
              BestHode.Merknad:MODIFIED = TRUE OR
              TOGGLE-Tillagg:MODIFIED = TRUE OR
              CB-Team:MODIFIED = TRUE OR
              CB-Lev:MODIFIED = TRUE THEN TRUE ELSE FALSE.
  END.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION OKbyteTeam C-Win 
FUNCTION OKbyteTeam RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    RETURN TRIM(ch_Grid:TextMatrix(ch_Grid:FixedRows - 1,ch_Grid:FixedCols - 1)) =
       TRIM(ch_Grid:TextMatrix(ch_Grid:FixedRows - 2,ch_Grid:FixedCols - 1)).
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION OkDelrad C-Win 
FUNCTION OkDelrad RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR wButik LIKE Butiker.Butik NO-UNDO.
  DEF VAR wAntBestKasse AS INTE NO-UNDO.

  RETURN IF INT(ch_Grid:TextMatrix(ch_Grid:Row,ch_Grid:FixedCols - 1)) > 0 THEN TRUE ELSE FALSE.
/*
  IF INT(ch_Grid:TextMatrix(ch_Grid:Row,ch_Grid:FixedCols - 1)) = 0 OR
         BestHode.DirekteLev:CHECKED IN FRAME DEFAULT-FRAME THEN
      RETURN FALSE.
  ELSE
      RETURN TRUE.
 */
/*
  ASSIGN wButik = INT(ch_Grid:TextMatrix(ch_Grid:Row,0)).
  FOR EACH tBestKasse WHERE tBestKasse.Butik = wButik:
      ASSIGN wAntBestKasse = wAntBestKasse + 1.
  END.
  /* om 0 -> ikke direktlev, hela raden bort
     om 1 -> direktlev, hela raden inkl fributik tas bort.
     popupvalet blir aktivt om det finns kassepost för det sortimentet */
  IF wAntBestKasse = 0 OR (wAntBestKasse = 1 AND INT(ch_Grid:TextMatrix(ch_Grid:Row,1)) <> 0) THEN
      RETURN TRUE.
  /*
    om ingen kasse -> OK
  */
  RETURN FALSE.
*/
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION OkDirektLev C-Win 
FUNCTION OkDirektLev RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR wRow AS INTE NO-UNDO.
  
  DO wRow = ch_Grid:FixedRows TO ch_Grid:Rows - 1:
      IF INT(ch_Grid:TextMatrix(wRow,ch_Grid:FixedCols - 1)) > 0 THEN
          RETURN FALSE.
  END.

  RETURN TRUE.   /* Function return value. */

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
  DEFINE VAR wCount1     AS INTE NO-UNDO.
  DEFINE VAR wRow        AS INTE NO-UNDO.
  DEFINE VAR wMatrisIdx  AS INTE NO-UNDO.
  DEFINE VAR wButik      LIKE Butiker.Butik NO-UNDO.
  DEFINE VAR wOK         AS LOGI NO-UNDO.

  IF NOT AVAIL tLevSort THEN
    RETURN FALSE.

  IF wType = "NY" THEN DO:
      IF BestHode.DirekteLev:CHECKED IN FRAME DEFAULT-FRAME THEN DO:
          IF INT(ch_Grid:TextMatrix(ch_Grid:FixedRows - 1,1)) = 0 THEN
              RETURN FALSE. 
/*        IF wBestHodeRecid = ? THEN
            FIND tBestKasse WHERE tBestKasse.BestNr = ? AND
                 tBestKasse.SortID = tLevSort.SortID    AND
                 tBestKasse.Butik  = wCentralLager.
        ELSE
            FIND tBestKasse WHERE tBestKasse.BestNr = BestHode.BestNr AND
                 tBestKasse.SortID = tLevSort.SortID    AND
                 tBestKasse.Butik  = wCentralLager.
        IF tBestKasse.Antal = 0 THEN
              RETURN FALSE. 
 */
      END.
      ELSE DO wCount1 = 1 TO NUM-ENTRIES(tLevSort.Storrelser," "):
          ASSIGN wMatrisIdx = LOOKUP(ENTRY(wCount1,tLevSort.Storrelser," "),wStorlekar," ").
          IF INT(ch_Grid:TextMatrix(4,wMatrisIdx + ch_Grid:FixedCols - 1)) < INT(ENTRY(wCount1,tLevSort.Fordel," ")) THEN
              RETURN FALSE. 
      END.
  END.
  ELSE DIRLEV: DO:
      IF BestHode.DirekteLev:CHECKED IN FRAME DEFAULT-FRAME THEN DO:
          DO wRow = wFstRow TO wLstRow:
              IF INT(ch_Grid:TextMatrix(wRow,1)) > 0 THEN
                  LEAVE DIRLEV.
          END.
          RETURN FALSE.
      END.
      ELSE DO wRow = wFstRow TO wLstRow: 
          ASSIGN wOK = TRUE.
          DO wCount1 = 1 TO NUM-ENTRIES(tLevSort.Storrelser," "):
              ASSIGN wMatrisIdx = LOOKUP(ENTRY(wCount1,tLevSort.Storrelser," "),wStorlekar," ").
              IF INT(ch_Grid:TextMatrix(wRow,wMatrisIdx + ch_Grid:FixedCols - 1)) < INT(ENTRY(wCount1,tLevSort.Fordel," ")) THEN
                  /* RETURN FALSE. */
                  ASSIGN wOK = FALSE.
          END.
          IF wOK = TRUE THEN
              LEAVE DIRLEV.
      END.
      IF wOK = FALSE THEN
          RETURN FALSE.
  END.
  RETURN TRUE.   /* Function return value. */
  
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
  
  DO wRow = ch_Grid:FixedRows TO ch_Grid:Rows - 1:
      DO wCol = ch_Grid:FixedCols TO ch_Grid:Cols - 1:
          IF INT(ch_Grid:TextMatrix(wRow,wCol)) > 0 THEN
             RETURN TRUE.
      END.
  END.
  
  RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

