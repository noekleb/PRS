&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME C-ArtKort


/* Temp-Table and Buffer definitions                                    */
DEFINE NEW SHARED TEMP-TABLE tmpLager NO-UNDO LIKE tmpLager.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-ArtKort 
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
  DEF VAR wArtBasRecid AS RECID NO-UNDO.
  DEF VAR wModus       AS CHAR  NO-UNDO. /* NY, ENDRE, SLETT */
  ASSIGN 
    wModus = "ENDRE". /* Default */
  FIND FIRST ArtBas NO-LOCK WHERE
    ArtBas.Vg > 0 AND
    ArtBas.LopNr > 0 NO-ERROR.
  IF NOT AVAILABLE ArtBas THEN
    FIND FIRST ArtBas.
  IF AVAILABLE ArtBas THEN
    ASSIGN wArtBasRecid = recid(ArtBas).
&ELSE
  DEF INPUT PARAMETER wArtBasRecid AS RECID NO-UNDO.
  DEF INPUT PARAMETER wModus       AS CHAR  NO-UNDO. /* NY, ENDRE, SLETT */
&ENDIF

/* Local Variable Definitions ---                                       */
DEF VAR wForslagLopNr      AS CHAR   NO-UNDO.
DEF VAR wNyVg              AS INT    NO-UNDO.
DEF VAR wOk                AS LOG    NO-UNDO.
DEF VAR wOldRecid          AS RECID  NO-UNDO.
DEF VAR hParentHandle      AS HANDLE NO-UNDO.
DEF VAR hHandle            AS HANDLE NO-UNDO.
DEF VAR hLabel             AS HANDLE NO-UNDO.
DEF VAR wBildNr            AS INT    NO-UNDO.
DEF VAR wFilNavn           AS CHAR   NO-UNDO.
DEF VAR wFilExt            AS CHAR   NO-UNDO.
DEF VAR wButikk            AS CHAR FORMAT "x(6)" NO-UNDO.
DEF VAR wVisLager          AS RECID  NO-UNDO.
DEF VAR wBestHodeRecid     AS RECID  NO-UNDO.
DEF VAR wRS-Vis            AS RECID  NO-UNDO.
DEF VAR wDataObjekt        AS CHAR   NO-UNDO.
DEF VAR wPerId             AS CHAR   NO-UNDO.
DEF VAR wStTypeId          AS CHAR   NO-UNDO.
DEF VAR  wCl               AS INT    NO-UNDO.
DEF VAR wPerLinTxt         AS CHAR   NO-UNDO.
DEF VAR wHistorikk         AS HANDLE NO-UNDO.
DEF VAR wKalkyle           AS HANDLE NO-UNDO.
DEF VAR wLapTop            AS LOG    NO-UNDO.
DEF VAR wBekreft           AS LOG    NO-UNDO.
DEF VAR wKopi              AS LOG    NO-UNDO.
DEF VAR wTabTekst          AS CHAR   NO-UNDO.
DEF VAR h_PrisKo           AS HANDLE NO-UNDO.
DEF VAR wArtikkelNr        AS DEC    NO-UNDO.
DEF VAR hFocus             AS HANDLE NO-UNDO.


/* Variabler for håndtering av Tab-Stip. */
DEFINE VAR chTabStrip        AS COM-HANDLE NO-UNDO.
DEFINE VAR chTabs            AS COM-HANDLE NO-UNDO.
DEFINE VAR chTab             AS COM-HANDLE NO-UNDO.
DEFINE VAR chTab1Side        AS COM-HANDLE NO-UNDO.
DEFINE VAR wAktivFlip        AS INT        INIT 1 NO-UNDO. /* !! parameter !! */
DEFINE VAR wAktivFrame       AS INTE INIT  1.

/* Buffere */
DEF BUFFER bArtBas   FOR ArtBas.
DEF TEMP-TABLE tmpChild 
  FIELD wChild AS HANDLE.
{runlib.i}

IF NOT VALID-HANDLE(h_PrisKo) THEN
    RUN prisko.p PERSISTENT SET h_PrisKo).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BROWSE-Bestilling

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES BestHode tmpLager

/* Definitions for BROWSE BROWSE-Bestilling                             */
&Scoped-define FIELDS-IN-QUERY-BROWSE-Bestilling BestHode.BestNr ~
BestHode.BestillingsDato BestHode.LevDato BestHode.BestStat BestHode.LevNr ~
BestHode.Beskrivelse BestHode.DirekteLev OrdreNummer() BestHode.TotAntPar ~
BestHode.TotDbKr BestHode.TotInnkjVerdi BestHode.TotSalgsVerdi 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-Bestilling 
&Scoped-define QUERY-STRING-BROWSE-Bestilling FOR EACH BestHode ~
      WHERE BestHode.ArtikkelNr = (if available ArtBas ~
                         then ArtBas.ArtikkelNr ~
                         else -99) NO-LOCK ~
    BY BestHode.BestNr DESCENDING INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-Bestilling OPEN QUERY BROWSE-Bestilling FOR EACH BestHode ~
      WHERE BestHode.ArtikkelNr = (if available ArtBas ~
                         then ArtBas.ArtikkelNr ~
                         else -99) NO-LOCK ~
    BY BestHode.BestNr DESCENDING INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-Bestilling BestHode
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-Bestilling BestHode


/* Definitions for BROWSE BROWSE-Lager                                  */
&Scoped-define FIELDS-IN-QUERY-BROWSE-Lager tmpLager.Butik ~
tmpLager.SumAntall tmpLager.SumVerdi tmpLager.DivAntall tmpLager.Antall[1] ~
tmpLager.Antall[2] tmpLager.Antall[3] tmpLager.Antall[4] tmpLager.Antall[5] ~
tmpLager.Antall[6] tmpLager.Antall[7] tmpLager.Antall[8] tmpLager.Antall[9] ~
tmpLager.Antall[10] tmpLager.Antall[11] tmpLager.Antall[12] ~
tmpLager.Antall[13] tmpLager.Antall[14] tmpLager.Antall[15] ~
tmpLager.Antall[16] tmpLager.Antall[17] tmpLager.Antall[18] ~
tmpLager.Antall[19] tmpLager.Antall[20] tmpLager.Antall[21] ~
tmpLager.Antall[22] tmpLager.Antall[23] tmpLager.Antall[24] ~
tmpLager.Antall[25] tmpLager.Antall[26] tmpLager.Antall[27] ~
tmpLager.Antall[28] tmpLager.Antall[29] tmpLager.Antall[30] ~
tmpLager.Antall[31] tmpLager.Antall[32] tmpLager.Antall[33] ~
tmpLager.Antall[34] tmpLager.Antall[35] tmpLager.Antall[36] ~
tmpLager.Antall[37] tmpLager.Antall[38] tmpLager.Antall[39] ~
tmpLager.Antall[40] tmpLager.Antall[41] tmpLager.Antall[42] ~
tmpLager.Antall[43] tmpLager.Antall[44] tmpLager.Antall[45] ~
tmpLager.Antall[46] tmpLager.Antall[47] tmpLager.Antall[48] 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-Lager 
&Scoped-define QUERY-STRING-BROWSE-Lager FOR EACH tmpLager NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-Lager OPEN QUERY BROWSE-Lager FOR EACH tmpLager NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-Lager tmpLager
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-Lager tmpLager


/* Definitions for FRAME FRAME-Lager                                    */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-Lager ~
    ~{&OPEN-QUERY-BROWSE-Lager}

/* Definitions for FRAME FRAME-Ordre                                    */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-Ordre ~
    ~{&OPEN-QUERY-BROWSE-Bestilling}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BUTTON-Next BUTTON-Prev CB-Sort BUTTON-Trans ~
Btn_Help BUTTON-Ok RECT-27 RECT-28 RECT-34 RECT-35 RECT-5 
&Scoped-Define DISPLAYED-FIELDS ArtBas.ArtikkelNr HuvGr.Hg HuvGr.HgBeskr ~
ArtBas.Vg ArtBas.VgKat ArtBas.LopNr ArtBas.Beskr ArtBas.lager ~
ArtBas.Storrelser ArtBas.Utgatt ArtBas.BildNr Bilderegister.Tekst ~
ArtBas.LevNr ArtBas.LevKod ArtBas.LevFargKod ArtBas.inn_dato 
&Scoped-define DISPLAYED-TABLES ArtBas HuvGr Bilderegister
&Scoped-define FIRST-DISPLAYED-TABLE ArtBas
&Scoped-define SECOND-DISPLAYED-TABLE HuvGr
&Scoped-define THIRD-DISPLAYED-TABLE Bilderegister
&Scoped-Define DISPLAYED-OBJECTS FI-PaTilbud CB-Sort TOGGLE-Annonse ~
FILL-IN-VgBeskr FILL-IN-LevNamn T-LapTop T-10 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD OrdreNummer C-ArtKort 
FUNCTION OrdreNummer RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-ArtKort AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE SUB-MENU m_Fil 
       MENU-ITEM m_Avslutt      LABEL "&Avslutt"       ACCELERATOR "ALT-F4".

DEFINE MENU MENU-BAR-C-ArtKort MENUBAR
       SUB-MENU  m_Fil          LABEL "&Fil"          .

DEFINE MENU POPUP-MENU-BROWSE-Bestilling 
       MENU-ITEM m_Kalkyle      LABEL "&Kalkyle"      
       MENU-ITEM m_Endre        LABEL "&Endre"        
       MENU-ITEM m_Innleveranse LABEL "&Innleveranse" .


/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE IMAGE-Sko AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chIMAGE-Sko AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-ByttVareGr 
     LABEL "Bytt varegruppe..." 
     SIZE 19.6 BY 1.05.

DEFINE BUTTON Btn_Help DEFAULT 
     IMAGE-UP FILE "icon\e-help":U NO-FOCUS FLAT-BUTTON
     LABEL "&Help" 
     SIZE 4.6 BY 1.05
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-Angre 
     IMAGE-UP FILE "icon/reset.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "&Ångra" 
     SIZE 4.6 BY 1.05 TOOLTIP "Angre (Alt-A)".

DEFINE BUTTON BUTTON-Kopier 
     IMAGE-UP FILE "icon/copyrec.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "&Kopiera" 
     SIZE 4.6 BY 1.05 TOOLTIP "Kopier post  (Alt-K)".

DEFINE BUTTON BUTTON-Lagre 
     IMAGE-UP FILE "icon/saverec.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "&Lagra" 
     SIZE 4.6 BY 1.05 TOOLTIP "Lagre post (Alt-L)".

DEFINE BUTTON BUTTON-Next 
     IMAGE-UP FILE "icon/next.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Neste" 
     SIZE 4.6 BY 1.05 TOOLTIP "Neste post (Alt-PilNed)".

DEFINE BUTTON BUTTON-Ny 
     IMAGE-UP FILE "icon/add.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "&Ny" 
     SIZE 4.6 BY 1.05 TOOLTIP "Ny post (Alt-N)".

DEFINE BUTTON BUTTON-Ok 
     IMAGE-UP FILE "icon\e-exit":U NO-FOCUS FLAT-BUTTON
     LABEL "&Ok" 
     SIZE 4.6 BY 1.05 TOOLTIP "Lagre og avslutt".

DEFINE BUTTON BUTTON-Paste 
     IMAGE-UP FILE "icon\e-paste":U NO-FOCUS
     LABEL "" 
     SIZE 4.6 BY 1 TOOLTIP "Henter bilde fra ClipBoard" DROP-TARGET.

DEFINE BUTTON BUTTON-Prev 
     IMAGE-UP FILE "icon/prev.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Forrige" 
     SIZE 4.6 BY 1.05 TOOLTIP "Forrige post (Alt-PilOpp)".

DEFINE BUTTON BUTTON-SettLopNr 
     LABEL "Sett løpenummer..." 
     SIZE 25 BY 1.05.

DEFINE BUTTON BUTTON-Slett 
     IMAGE-UP FILE "icon/deleterec.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Radera" 
     SIZE 4.6 BY 1.05 TOOLTIP "Slett post (Alt-D)".

DEFINE BUTTON BUTTON-SokArt 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokBilde 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1 TOOLTIP "Søk i bilderegister".

DEFINE BUTTON BUTTON-SokLev 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.6 BY 1.05 TOOLTIP "Søk i leverandørregister".

DEFINE BUTTON BUTTON-SokVg 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokVgKat 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-Trans 
     LABEL "Vis transaksjoner..." 
     SIZE 25 BY 1.05.

DEFINE VARIABLE CB-Sort AS CHARACTER FORMAT "X(256)":U INITIAL " 1: Vg/LpNr" 
     VIEW-AS COMBO-BOX INNER-LINES 6
     LIST-ITEMS " 1: Vg/LpNr"," 2: Vg/Beskr"," 3: Vg/LevArtNr"," 4: Lev/LevArtNr"," 5: ArtikkelNummer" 
     DROP-DOWN-LIST
     SIZE 35 BY 1 TOOLTIP "Velg sorteringsordning som skal gjelde for blaing med pil Opp/Ned." NO-UNDO.

DEFINE VARIABLE FI-PaTilbud AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 27 BY .62
     FGCOLOR 12 FONT 6 NO-UNDO.

DEFINE VARIABLE FILL-IN-LevNamn AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-VgBeskr AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 29.4 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-27
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 159.2 BY .1.

DEFINE RECTANGLE RECT-28
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 159.4 BY .1.

DEFINE RECTANGLE RECT-34
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 92 BY 5.95.

DEFINE RECTANGLE RECT-35
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 66 BY 2.43.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 66 BY 3.33.

DEFINE VARIABLE T-10 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 NO-UNDO.

DEFINE VARIABLE T-LapTop AS LOGICAL INITIAL no 
     LABEL "LapTop" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-Annonse AS LOGICAL INITIAL no 
     LABEL "Annonse" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE BUTTON BUTTON-SokBruk 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokDato 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokDato-2 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokFarge 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokInner 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokLalst 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokMaterial 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokOver 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokProdusent 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokProv 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokRabatt 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokSesong 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokSlit 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokStrType 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokValuta 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokVaremerke 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE FI-1 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE FI-2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE FI-3 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE FI-4 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE FI-5 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE FI-6 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE FI-7 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE FI-8 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Tekst-2 AS CHARACTER FORMAT "X(256)":U INITIAL "Leverandørinformasjon" 
      VIEW-AS TEXT 
     SIZE 31 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-Tekst-3 AS CHARACTER FORMAT "X(256)":U INITIAL "Notat" 
      VIEW-AS TEXT 
     SIZE 20 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-Tekst-4 AS CHARACTER FORMAT "X(256)":U INITIAL "Øvrige opplysninger" 
      VIEW-AS TEXT 
     SIZE 29 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-Tekst-5 AS CHARACTER FORMAT "X(256)":U INITIAL "Prisinformasjon" 
      VIEW-AS TEXT 
     SIZE 29 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-Tekst1 AS CHARACTER FORMAT "X(256)":U INITIAL "Grunninformasjon" 
      VIEW-AS TEXT 
     SIZE 20 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FILL-IN-19 AS CHARACTER FORMAT "X(256)":U INITIAL "Vis på ordre" 
      VIEW-AS TEXT 
     SIZE 14 BY .62 NO-UNDO.

DEFINE VARIABLE FILL-IN-EndretInfo AS CHARACTER FORMAT "X(256)":U INITIAL "Endret informasjon" 
      VIEW-AS TEXT 
     SIZE 91 BY .62 NO-UNDO.

DEFINE VARIABLE FILL-IN-InnPris AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Inköpspris" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-SalgsPris AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Normalpris" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-TilbPris AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Kampanjepris" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-UtsFra AS DATE FORMAT "99/99/9999":U 
     LABEL "Kamp.periode" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-UtsTil AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-29
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 59.6 BY 4.57.

DEFINE RECTANGLE RECT-31
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 93 BY 10.71.

DEFINE RECTANGLE RECT-32
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 153 BY 4.48.

DEFINE RECTANGLE RECT-54
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 60 BY 5.24.

DEFINE VARIABLE T-1 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 NO-UNDO.

DEFINE VARIABLE T-2 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 NO-UNDO.

DEFINE VARIABLE T-3 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 NO-UNDO.

DEFINE VARIABLE T-4 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 NO-UNDO.

DEFINE VARIABLE T-5 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 NO-UNDO.

DEFINE VARIABLE T-6 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 NO-UNDO.

DEFINE VARIABLE T-7 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 NO-UNDO.

DEFINE VARIABLE T-8 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 NO-UNDO.

DEFINE VARIABLE T-9 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 NO-UNDO.

DEFINE VARIABLE T-EnableLevInfo AS LOGICAL INITIAL no 
     LABEL "Endre" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .81 NO-UNDO.

DEFINE BUTTON B-Chart 
     IMAGE-UP FILE "icon/e-chart.bmp":U
     LABEL "Button 1" 
     SIZE 4.8 BY 1.14.

DEFINE BUTTON B-OppdLAger 
     LABEL "&Oppdater lagerbilde" 
     SIZE 35 BY 1.14.

DEFINE BUTTON BUTTON-Overfor 
     LABEL "&Overføring..." 
     SIZE 21 BY 1.14.

DEFINE VARIABLE RS-Vis AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Lager", 1,
"Solgt", 2,
"Kjøpt", 3,
"Brekkasje", 4,
"Int.forbr", 5,
"Reklamert", 6,
"Gjennkjøp", 7,
"L.Retur", 8,
"Overført", 9,
"Justert", 10,
"Svinn", 11,
"Nedskrevet", 12,
"Rabatt", 13
     SIZE 153 BY 1.19 NO-UNDO.

DEFINE BUTTON B-OppdBest 
     LABEL "&Oppdate browser" 
     SIZE 29 BY 1.14.

DEFINE BUTTON BUTTON-EndreBest 
     LABEL "&Endre.." 
     SIZE 21 BY 1.14.

DEFINE BUTTON BUTTON-Innleveranse 
     LABEL "&Innleveranse..." 
     SIZE 21 BY 1.14.

DEFINE BUTTON BUTTON-Kalkyle 
     LABEL "&Kalkyle..." 
     SIZE 21 BY 1.14.

DEFINE BUTTON BUTTON-NyBest 
     LABEL "&Ny.." 
     SIZE 21 BY 1.14.

DEFINE BUTTON BUTTON-SlettBest 
     LABEL "&Slette" 
     SIZE 21 BY 1.14.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-Bestilling FOR 
      BestHode SCROLLING.

DEFINE QUERY BROWSE-Lager FOR 
      tmpLager SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-Bestilling
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-Bestilling C-ArtKort _STRUCTURED
  QUERY BROWSE-Bestilling NO-LOCK DISPLAY
      BestHode.BestNr FORMAT ">>>>>>>9":U
      BestHode.BestillingsDato COLUMN-LABEL "BestDato" FORMAT "99/99/99":U
      BestHode.LevDato COLUMN-LABEL "LevDato" FORMAT "99/99/99":U
      BestHode.BestStat FORMAT ">9":U
      BestHode.LevNr FORMAT "zzzzz9":U
      BestHode.Beskrivelse FORMAT "x(30)":U WIDTH 27
      BestHode.DirekteLev FORMAT "yes/no":U
      OrdreNummer() COLUMN-LABEL "OrdreNr" FORMAT "x(7)":U
      BestHode.TotAntPar COLUMN-LABEL "Ant.par" FORMAT "->>,>>9":U
            WIDTH 13.6
      BestHode.TotDbKr COLUMN-LABEL "DbKr" FORMAT "->,>>>,>>9.99":U
            WIDTH 13
      BestHode.TotInnkjVerdi COLUMN-LABEL "Innkj. verdi" FORMAT "->,>>>,>>9.99":U
            WIDTH 15
      BestHode.TotSalgsVerdi COLUMN-LABEL "Salgsverdi" FORMAT "->,>>>,>>9.99":U
            WIDTH 16
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 148 BY 14.76 ROW-HEIGHT-CHARS .62 FIT-LAST-COLUMN.

DEFINE BROWSE BROWSE-Lager
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-Lager C-ArtKort _STRUCTURED
  QUERY BROWSE-Lager NO-LOCK DISPLAY
      tmpLager.Butik FORMAT "x(8)":U
      tmpLager.SumAntall FORMAT "X(10)":U
      tmpLager.SumVerdi FORMAT "X(10)":U
      tmpLager.DivAntall FORMAT "X(10)":U
      tmpLager.Antall[1] FORMAT "X(6)":U
      tmpLager.Antall[2] FORMAT "X(6)":U
      tmpLager.Antall[3] FORMAT "X(6)":U
      tmpLager.Antall[4] FORMAT "X(6)":U
      tmpLager.Antall[5] FORMAT "X(6)":U
      tmpLager.Antall[6] FORMAT "X(6)":U
      tmpLager.Antall[7] FORMAT "X(6)":U
      tmpLager.Antall[8] FORMAT "X(6)":U
      tmpLager.Antall[9] FORMAT "X(6)":U
      tmpLager.Antall[10] FORMAT "X(6)":U
      tmpLager.Antall[11] FORMAT "X(6)":U
      tmpLager.Antall[12] FORMAT "X(6)":U
      tmpLager.Antall[13] FORMAT "X(6)":U
      tmpLager.Antall[14] FORMAT "X(6)":U
      tmpLager.Antall[15] FORMAT "X(6)":U
      tmpLager.Antall[16] FORMAT "X(6)":U
      tmpLager.Antall[17] FORMAT "X(6)":U
      tmpLager.Antall[18] FORMAT "X(6)":U
      tmpLager.Antall[19] FORMAT "X(6)":U
      tmpLager.Antall[20] FORMAT "X(6)":U
      tmpLager.Antall[21] FORMAT "X(6)":U
      tmpLager.Antall[22] FORMAT "X(6)":U
      tmpLager.Antall[23] FORMAT "X(6)":U
      tmpLager.Antall[24] FORMAT "X(6)":U
      tmpLager.Antall[25] FORMAT "X(6)":U
      tmpLager.Antall[26] FORMAT "X(6)":U
      tmpLager.Antall[27] FORMAT "X(6)":U
      tmpLager.Antall[28] FORMAT "X(6)":U
      tmpLager.Antall[29] FORMAT "X(6)":U
      tmpLager.Antall[30] FORMAT "X(6)":U
      tmpLager.Antall[31] FORMAT "X(6)":U
      tmpLager.Antall[32] FORMAT "X(6)":U
      tmpLager.Antall[33] FORMAT "X(6)":U
      tmpLager.Antall[34] FORMAT "X(6)":U
      tmpLager.Antall[35] FORMAT "X(6)":U
      tmpLager.Antall[36] FORMAT "X(6)":U
      tmpLager.Antall[37] FORMAT "X(6)":U
      tmpLager.Antall[38] FORMAT "X(6)":U
      tmpLager.Antall[39] FORMAT "X(6)":U
      tmpLager.Antall[40] FORMAT "X(6)":U
      tmpLager.Antall[41] FORMAT "X(6)":U
      tmpLager.Antall[42] FORMAT "X(6)":U
      tmpLager.Antall[43] FORMAT "X(6)":U
      tmpLager.Antall[44] FORMAT "X(6)":U
      tmpLager.Antall[45] FORMAT "X(6)":U
      tmpLager.Antall[46] FORMAT "X(6)":U
      tmpLager.Antall[47] FORMAT "X(6)":U
      tmpLager.Antall[48] FORMAT "X(6)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 150.4 BY 14.05 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BUTTON-Paste AT ROW 2.91 COL 126.4
     BUTTON-Angre AT ROW 1.33 COL 20.8 NO-TAB-STOP 
     BUTTON-Kopier AT ROW 1.33 COL 6.6 NO-TAB-STOP 
     BUTTON-Lagre AT ROW 1.33 COL 16.2 NO-TAB-STOP 
     BUTTON-Next AT ROW 1.33 COL 30.6 NO-TAB-STOP 
     BUTTON-Ny AT ROW 1.33 COL 2 NO-TAB-STOP 
     BUTTON-Prev AT ROW 1.33 COL 26 NO-TAB-STOP 
     BUTTON-Slett AT ROW 1.33 COL 11.4 NO-TAB-STOP 
     FI-PaTilbud AT ROW 7.81 COL 130 COLON-ALIGNED NO-LABEL
     CB-Sort AT ROW 1.33 COL 34.2 COLON-ALIGNED NO-LABEL
     B-ByttVareGr AT ROW 1.33 COL 72.4
     BUTTON-SettLopNr AT ROW 1.33 COL 93
     BUTTON-Trans AT ROW 1.33 COL 119
     ArtBas.ArtikkelNr AT ROW 2.91 COL 8 COLON-ALIGNED
          LABEL "Art.nr"
          VIEW-AS FILL-IN 
          SIZE 25.8 BY 1
     HuvGr.Hg AT ROW 2.91 COL 34.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     HuvGr.HgBeskr AT ROW 2.91 COL 38.6 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 25 BY 1
     ArtBas.Vg AT ROW 3.91 COL 8 COLON-ALIGNED
          LABEL "Vargr."
          VIEW-AS FILL-IN 
          SIZE 12.6 BY 1
          BGCOLOR 10 FONT 6
     ArtBas.VgKat AT ROW 3.91 COL 23.8 COLON-ALIGNED NO-LABEL FORMAT "z9"
          VIEW-AS FILL-IN 
          SIZE 5.4 BY 1
     ArtBas.LopNr AT ROW 4.91 COL 8 COLON-ALIGNED
          LABEL "Løpenr"
          VIEW-AS FILL-IN 
          SIZE 21.2 BY 1
          BGCOLOR 10 FONT 6
     ArtBas.Beskr AT ROW 4.91 COL 34.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 29.4 BY 1
     ArtBas.lager AT ROW 6.48 COL 4
          LABEL "Lager"
          VIEW-AS TOGGLE-BOX
          SIZE 12 BY .81
     ArtBas.Storrelser AT ROW 7.29 COL 4
          VIEW-AS TOGGLE-BOX
          SIZE 14 BY .81
     ArtBas.Utgatt AT ROW 6.48 COL 36
          VIEW-AS TOGGLE-BOX
          SIZE 14 BY .71
     TOGGLE-Annonse AT ROW 7.29 COL 36
     ArtBas.BildNr AT ROW 2.91 COL 80 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     Bilderegister.Tekst AT ROW 2.91 COL 94.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 29.2 BY 1
     FILL-IN-VgBeskr AT ROW 3.91 COL 34.2 COLON-ALIGNED NO-LABEL
     ArtBas.LevNr AT ROW 3.91 COL 80 COLON-ALIGNED FORMAT "zzzzz9"
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     FILL-IN-LevNamn AT ROW 3.91 COL 95 COLON-ALIGNED NO-LABEL
     ArtBas.LevKod AT ROW 4.91 COL 80 COLON-ALIGNED FORMAT "x(200)"
          VIEW-AS FILL-IN 
          SIZE 44 BY 1
     ArtBas.LevFargKod AT ROW 5.91 COL 80 COLON-ALIGNED FORMAT "X(200)"
          VIEW-AS FILL-IN 
          SIZE 44 BY 1
     ArtBas.inn_dato AT ROW 6.95 COL 80 COLON-ALIGNED
          LABEL "1.innlev."
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     T-LapTop AT ROW 7.1 COL 99.6
     BUTTON-SokBilde AT ROW 2.91 COL 92.4
     BUTTON-SokLev AT ROW 3.86 COL 92.4
     Btn_Help AT ROW 1.33 COL 150.4 NO-TAB-STOP 
     BUTTON-Ok AT ROW 1.33 COL 155.4 NO-TAB-STOP 
     BUTTON-SokVg AT ROW 3.91 COL 21.4
     BUTTON-SokVgKat AT ROW 3.95 COL 31.6
     BUTTON-SokArt AT ROW 4.95 COL 31.6
     T-10 AT ROW 6.19 COL 126.4 NO-TAB-STOP 
     RECT-27 AT ROW 1.1 COL 1.4
     RECT-28 AT ROW 2.43 COL 1.2
     RECT-34 AT ROW 2.67 COL 68
     RECT-35 AT ROW 6.19 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 159.8 BY 26.57.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME DEFAULT-FRAME
     RECT-5 AT ROW 2.71 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 159.8 BY 26.57.

DEFINE FRAME FRAME-Ordre
     BROWSE-Bestilling AT ROW 1.48 COL 2
     B-OppdBest AT ROW 16.71 COL 2
     BUTTON-NyBest AT ROW 16.71 COL 41
     BUTTON-EndreBest AT ROW 16.71 COL 63
     BUTTON-Kalkyle AT ROW 16.71 COL 85
     BUTTON-SlettBest AT ROW 16.71 COL 107
     BUTTON-Innleveranse AT ROW 16.71 COL 129
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 9.81
         SIZE 156 BY 17.34.

DEFINE FRAME FRAME-dummy
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         THREE-D 
         AT COL 3 ROW 9.81
         SIZE 156 BY 17.33.

DEFINE FRAME FRAME-ArtInfo
     FI-Tekst1 AT ROW 1.24 COL 1 COLON-ALIGNED NO-LABEL
     BUTTON-SokDato-2 AT ROW 15.1 COL 146.2
     T-EnableLevInfo AT ROW 2.43 COL 48
     BUTTON-SokProv AT ROW 15.14 COL 23
     BUTTON-SokRabatt AT ROW 16.14 COL 23
     ArtBas.StrTypeID AT ROW 2.24 COL 12.2 COLON-ALIGNED
          LABEL "StrType" FORMAT "zzzzz9"
          VIEW-AS FILL-IN 
          SIZE 9.8 BY 1
     FILL-IN-19 AT ROW 2.52 COL 76 COLON-ALIGNED NO-LABEL
     ArtBas.SaSong AT ROW 3.29 COL 12.2 COLON-ALIGNED FORMAT "zz9"
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     T-9 AT ROW 2.43 COL 97 NO-TAB-STOP 
     ArtBas.Farg AT ROW 4.29 COL 12.2 COLON-ALIGNED FORMAT "zz9"
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     ArtBas.MatKod AT ROW 5.29 COL 12.2 COLON-ALIGNED
          LABEL "Material"
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     ArtBas.Klack AT ROW 6.29 COL 12.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     ArtBas.inner-id AT ROW 7.29 COL 12.2 COLON-ALIGNED
          LABEL "Innersåle"
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     ArtBas.ov-id AT ROW 8.29 COL 12.2 COLON-ALIGNED
          LABEL "Innerfor"
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     ArtBas.slit-id AT ROW 9.29 COL 12.2 COLON-ALIGNED
          LABEL "Slitesåle"
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     ArtBas.last-id AT ROW 10.24 COL 12.2 COLON-ALIGNED
          LABEL "Læst"
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     ArtBas.anv-id AT ROW 11.29 COL 12.2 COLON-ALIGNED
          LABEL "Bruk"
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     FI-1 AT ROW 3.29 COL 46 COLON-ALIGNED NO-LABEL
     FI-2 AT ROW 5.29 COL 46 COLON-ALIGNED NO-LABEL
     FI-3 AT ROW 6.33 COL 46 COLON-ALIGNED NO-LABEL
     FI-4 AT ROW 7.33 COL 46 COLON-ALIGNED NO-LABEL
     FI-5 AT ROW 8.33 COL 46 COLON-ALIGNED NO-LABEL
     FI-6 AT ROW 9.29 COL 46 COLON-ALIGNED NO-LABEL
     FI-7 AT ROW 10.29 COL 46 COLON-ALIGNED NO-LABEL
     FI-8 AT ROW 11.29 COL 46 COLON-ALIGNED NO-LABEL
     ArtBas.BongTekst AT ROW 14.05 COL 12 COLON-ALIGNED FORMAT "X(30)"
          VIEW-AS FILL-IN 
          SIZE 32.8 BY 1
     ArtBas.ProvKod AT ROW 15.05 COL 12 COLON-ALIGNED
          LABEL "ProvKod"
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     ArtBas.RabKod AT ROW 16.1 COL 12 COLON-ALIGNED HELP
          "Angir maks rabbatt som kan gis på artikkelen i kassen"
          LABEL "Maks rab."
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     ArtBas.valkod AT ROW 14.1 COL 57 COLON-ALIGNED
          LABEL "Valuta"
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     SaSong.SasBeskr AT ROW 3.29 COL 22.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 22.2 BY 1
     Farg.FarBeskr AT ROW 4.29 COL 22.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 22.2 BY 1
     Material.MatBeskr AT ROW 5.29 COL 22.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 22.2 BY 1
     InnerSula.InnerBeskr AT ROW 7.29 COL 22.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 22.2 BY 1
     Ovandel.OvBeskr AT ROW 8.29 COL 22.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 22.2 BY 1
     SlitSula.SlitBeskr AT ROW 9.29 COL 22.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 22.2 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 9.81
         SIZE 156 BY 17.33.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME FRAME-ArtInfo
     Last-Sko.LastBeskr AT ROW 10.29 COL 22.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 22.2 BY 1
     Anv-Kod.AnvBeskr AT ROW 11.29 COL 22.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 22.2 BY 1
     Valuta.ValKurs AT ROW 14.14 COL 71 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 21 BY 1
     Prov.ProvProc AT ROW 15.14 COL 26 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
     Rabatt.RabProc AT ROW 16.14 COL 26 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
     ArtBas.VMId AT ROW 15.1 COL 57 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     ArtBas.ProdNr AT ROW 16.1 COL 57 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     ArtBas.LevDato1 AT ROW 14.14 COL 128.2 COLON-ALIGNED
          LABEL "1. Mulige leveringsdato"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     ArtBas.LevDato2 AT ROW 15.05 COL 128.2 COLON-ALIGNED
          LABEL "2. Mulige leveringsdato"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     ArtBas.Notat AT ROW 2.19 COL 100 NO-LABEL
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          SIZE 55 BY 4.76
     StrType.Beskrivelse AT ROW 2.24 COL 26 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 18.8 BY 1
     Varemerke.Beskrivelse AT ROW 15.05 COL 71 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 21 BY 1
     Produsent.Beskrivelse AT ROW 16.1 COL 71 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 21 BY 1
     FILL-IN-SalgsPris AT ROW 9.29 COL 109.6 COLON-ALIGNED
     BUTTON-SokSesong AT ROW 3.29 COL 20.4
     FILL-IN-InnPris AT ROW 8.24 COL 109.6 COLON-ALIGNED
     FILL-IN-TilbPris AT ROW 10.29 COL 109.4 COLON-ALIGNED
     BUTTON-SokFarge AT ROW 4.29 COL 20.4
     FILL-IN-UtsFra AT ROW 11.29 COL 109.4 COLON-ALIGNED
     FILL-IN-UtsTil AT ROW 11.29 COL 127 COLON-ALIGNED NO-LABEL
     BUTTON-SokMaterial AT ROW 5.29 COL 20.4
     BUTTON-SokInner AT ROW 7.33 COL 20.4
     BUTTON-SokOver AT ROW 8.33 COL 20.4
     BUTTON-SokSlit AT ROW 9.33 COL 20.4
     BUTTON-SokLalst AT ROW 10.33 COL 20.4
     BUTTON-SokBruk AT ROW 11.33 COL 20.4
     BUTTON-SokStrType AT ROW 2.19 COL 23.8
     BUTTON-SokVaremerke AT ROW 15.05 COL 68
     BUTTON-SokProdusent AT ROW 16 COL 68
     BUTTON-SokValuta AT ROW 14.14 COL 68
     FILL-IN-EndretInfo AT ROW 17.43 COL 3 NO-LABEL
     T-8 AT ROW 11.52 COL 78.6 NO-TAB-STOP 
     T-7 AT ROW 10.48 COL 78.6 NO-TAB-STOP 
     T-6 AT ROW 9.52 COL 78.6 NO-TAB-STOP 
     T-5 AT ROW 8.57 COL 78.6 NO-TAB-STOP 
     T-4 AT ROW 7.62 COL 78.6 NO-TAB-STOP 
     T-3 AT ROW 6.62 COL 78.6 NO-TAB-STOP 
     T-2 AT ROW 5.52 COL 78.6 NO-TAB-STOP 
     T-1 AT ROW 3.57 COL 78.6 NO-TAB-STOP 
     BUTTON-SokDato AT ROW 14.1 COL 146.2
     FI-Tekst-2 AT ROW 1.24 COL 46 COLON-ALIGNED NO-LABEL
     FI-Tekst-3 AT ROW 1.24 COL 94 COLON-ALIGNED NO-LABEL
     FI-Tekst-4 AT ROW 13.14 COL 1 COLON-ALIGNED NO-LABEL
     FI-Tekst-5 AT ROW 7.43 COL 94 COLON-ALIGNED NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 9.81
         SIZE 156 BY 17.33.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME FRAME-ArtInfo
     RECT-29 AT ROW 8.1 COL 96
     RECT-31 AT ROW 1.95 COL 2
     RECT-32 AT ROW 13.86 COL 2
     RECT-54 AT ROW 1.95 COL 96
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 9.81
         SIZE 156 BY 17.33.

DEFINE FRAME FRAME-Lager
     B-Chart AT ROW 1.1 COL 72.4
     B-OppdLAger AT ROW 1.1 COL 81
     BUTTON-Overfor AT ROW 1.1 COL 130
     BROWSE-Lager AT ROW 2.43 COL 1
     RS-Vis AT ROW 16.71 COL 3 NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 9.81
         SIZE 156 BY 17.34.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: tmpLager T "NEW SHARED" NO-UNDO Temp-DB tmpLager
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-ArtKort ASSIGN
         HIDDEN             = YES
         TITLE              = "Artikelkort"
         HEIGHT             = 26.57
         WIDTH              = 159.8
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

ASSIGN {&WINDOW-NAME}:MENUBAR    = MENU MENU-BAR-C-ArtKort:HANDLE.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-ArtKort
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* REPARENT FRAME */
ASSIGN FRAME FRAME-ArtInfo:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME FRAME-dummy:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME FRAME-Lager:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME FRAME-Ordre:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
   NOT-VISIBLE FRAME-NAME Custom                                        */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME FRAME-Ordre:MOVE-BEFORE-TAB-ITEM (FRAME FRAME-Lager:HANDLE)
/* END-ASSIGN-TABS */.

/* SETTINGS FOR FILL-IN ArtBas.ArtikkelNr IN FRAME DEFAULT-FRAME
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR BUTTON B-ByttVareGr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       B-ByttVareGr:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN ArtBas.Beskr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       ArtBas.Beskr:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN ArtBas.BildNr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       ArtBas.BildNr:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR BUTTON BUTTON-Angre IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-Kopier IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-Lagre IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-Ny IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-Paste IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-SettLopNr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BUTTON-SettLopNr:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON BUTTON-Slett IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-SokArt IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-SokBilde IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-SokLev IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-SokVg IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-SokVgKat IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-PaTilbud IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-LevNamn IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN-LevNamn:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-VgBeskr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN HuvGr.Hg IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN HuvGr.HgBeskr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ArtBas.inn_dato IN FRAME DEFAULT-FRAME
   NO-ENABLE EXP-LABEL                                                  */
ASSIGN 
       ArtBas.inn_dato:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR TOGGLE-BOX ArtBas.lager IN FRAME DEFAULT-FRAME
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN ArtBas.LevFargKod IN FRAME DEFAULT-FRAME
   NO-ENABLE EXP-FORMAT                                                 */
ASSIGN 
       ArtBas.LevFargKod:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN ArtBas.LevKod IN FRAME DEFAULT-FRAME
   NO-ENABLE EXP-FORMAT                                                 */
ASSIGN 
       ArtBas.LevKod:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN ArtBas.LevNr IN FRAME DEFAULT-FRAME
   NO-ENABLE EXP-FORMAT                                                 */
ASSIGN 
       ArtBas.LevNr:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN ArtBas.LopNr IN FRAME DEFAULT-FRAME
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR TOGGLE-BOX ArtBas.Storrelser IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX T-10 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX T-LapTop IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Bilderegister.Tekst IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       Bilderegister.Tekst:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOGGLE-Annonse IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX ArtBas.Utgatt IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ArtBas.Vg IN FRAME DEFAULT-FRAME
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN ArtBas.VgKat IN FRAME DEFAULT-FRAME
   NO-ENABLE EXP-FORMAT                                                 */
/* SETTINGS FOR FRAME FRAME-ArtInfo
   Custom                                                               */
ASSIGN 
       FRAME FRAME-ArtInfo:SENSITIVE        = FALSE.

/* SETTINGS FOR FILL-IN ArtBas.anv-id IN FRAME FRAME-ArtInfo
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Anv-Kod.AnvBeskr IN FRAME FRAME-ArtInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Produsent.Beskrivelse IN FRAME FRAME-ArtInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Varemerke.Beskrivelse IN FRAME FRAME-ArtInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN StrType.Beskrivelse IN FRAME FRAME-ArtInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ArtBas.BongTekst IN FRAME FRAME-ArtInfo
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN Farg.FarBeskr IN FRAME FRAME-ArtInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ArtBas.Farg IN FRAME FRAME-ArtInfo
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN FI-1 IN FRAME FRAME-ArtInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-2 IN FRAME FRAME-ArtInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-3 IN FRAME FRAME-ArtInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-4 IN FRAME FRAME-ArtInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-5 IN FRAME FRAME-ArtInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-6 IN FRAME FRAME-ArtInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-7 IN FRAME FRAME-ArtInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-8 IN FRAME FRAME-ArtInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Tekst-2 IN FRAME FRAME-ArtInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Tekst-3 IN FRAME FRAME-ArtInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Tekst-4 IN FRAME FRAME-ArtInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Tekst-5 IN FRAME FRAME-ArtInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Tekst1 IN FRAME FRAME-ArtInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-19 IN FRAME FRAME-ArtInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-EndretInfo IN FRAME FRAME-ArtInfo
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-InnPris IN FRAME FRAME-ArtInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-SalgsPris IN FRAME FRAME-ArtInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-TilbPris IN FRAME FRAME-ArtInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-UtsFra IN FRAME FRAME-ArtInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-UtsTil IN FRAME FRAME-ArtInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ArtBas.inner-id IN FRAME FRAME-ArtInfo
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN InnerSula.InnerBeskr IN FRAME FRAME-ArtInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ArtBas.last-id IN FRAME FRAME-ArtInfo
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Last-Sko.LastBeskr IN FRAME FRAME-ArtInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ArtBas.LevDato1 IN FRAME FRAME-ArtInfo
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN ArtBas.LevDato2 IN FRAME FRAME-ArtInfo
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Material.MatBeskr IN FRAME FRAME-ArtInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ArtBas.MatKod IN FRAME FRAME-ArtInfo
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN ArtBas.ov-id IN FRAME FRAME-ArtInfo
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Ovandel.OvBeskr IN FRAME FRAME-ArtInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ArtBas.ProvKod IN FRAME FRAME-ArtInfo
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Prov.ProvProc IN FRAME FRAME-ArtInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ArtBas.RabKod IN FRAME FRAME-ArtInfo
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR FILL-IN Rabatt.RabProc IN FRAME FRAME-ArtInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN SaSong.SasBeskr IN FRAME FRAME-ArtInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ArtBas.SaSong IN FRAME FRAME-ArtInfo
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN ArtBas.slit-id IN FRAME FRAME-ArtInfo
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN SlitSula.SlitBeskr IN FRAME FRAME-ArtInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ArtBas.StrTypeID IN FRAME FRAME-ArtInfo
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR TOGGLE-BOX T-1 IN FRAME FRAME-ArtInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX T-2 IN FRAME FRAME-ArtInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX T-3 IN FRAME FRAME-ArtInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX T-4 IN FRAME FRAME-ArtInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX T-5 IN FRAME FRAME-ArtInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX T-6 IN FRAME FRAME-ArtInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX T-7 IN FRAME FRAME-ArtInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX T-8 IN FRAME FRAME-ArtInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX T-9 IN FRAME FRAME-ArtInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ArtBas.valkod IN FRAME FRAME-ArtInfo
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Valuta.ValKurs IN FRAME FRAME-ArtInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME FRAME-dummy
   UNDERLINE                                                            */
ASSIGN 
       FRAME FRAME-dummy:SENSITIVE        = FALSE.

/* SETTINGS FOR FRAME FRAME-Lager
                                                                        */
/* BROWSE-TAB BROWSE-Lager BUTTON-Overfor FRAME-Lager */
/* SETTINGS FOR BUTTON B-OppdLAger IN FRAME FRAME-Lager
   NO-ENABLE                                                            */
ASSIGN 
       B-OppdLAger:HIDDEN IN FRAME FRAME-Lager           = TRUE.

ASSIGN 
       BROWSE-Lager:NUM-LOCKED-COLUMNS IN FRAME FRAME-Lager     = 4.

/* SETTINGS FOR BUTTON BUTTON-Overfor IN FRAME FRAME-Lager
   NO-ENABLE                                                            */
ASSIGN 
       BUTTON-Overfor:HIDDEN IN FRAME FRAME-Lager           = TRUE.

/* SETTINGS FOR FRAME FRAME-Ordre
                                                                        */
/* BROWSE-TAB BROWSE-Bestilling 1 FRAME-Ordre */
/* SETTINGS FOR BUTTON B-OppdBest IN FRAME FRAME-Ordre
   NO-ENABLE                                                            */
ASSIGN 
       B-OppdBest:HIDDEN IN FRAME FRAME-Ordre           = TRUE.

ASSIGN 
       BROWSE-Bestilling:POPUP-MENU IN FRAME FRAME-Ordre             = MENU POPUP-MENU-BROWSE-Bestilling:HANDLE.

/* SETTINGS FOR BUTTON BUTTON-EndreBest IN FRAME FRAME-Ordre
   NO-ENABLE                                                            */
ASSIGN 
       BUTTON-EndreBest:HIDDEN IN FRAME FRAME-Ordre           = TRUE.

/* SETTINGS FOR BUTTON BUTTON-Innleveranse IN FRAME FRAME-Ordre
   NO-ENABLE                                                            */
ASSIGN 
       BUTTON-Innleveranse:HIDDEN IN FRAME FRAME-Ordre           = TRUE.

/* SETTINGS FOR BUTTON BUTTON-Kalkyle IN FRAME FRAME-Ordre
   NO-ENABLE                                                            */
ASSIGN 
       BUTTON-Kalkyle:HIDDEN IN FRAME FRAME-Ordre           = TRUE.

/* SETTINGS FOR BUTTON BUTTON-NyBest IN FRAME FRAME-Ordre
   NO-ENABLE                                                            */
ASSIGN 
       BUTTON-NyBest:HIDDEN IN FRAME FRAME-Ordre           = TRUE.

/* SETTINGS FOR BUTTON BUTTON-SlettBest IN FRAME FRAME-Ordre
   NO-ENABLE                                                            */
ASSIGN 
       BUTTON-SlettBest:HIDDEN IN FRAME FRAME-Ordre           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-ArtKort)
THEN C-ArtKort:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-Bestilling
/* Query rebuild information for BROWSE BROWSE-Bestilling
     _TblList          = "skotex.BestHode"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "skotex.BestHode.BestNr|no"
     _Where[1]         = "BestHode.ArtikkelNr = (if available ArtBas
                         then ArtBas.ArtikkelNr
                         else -99)"
     _FldNameList[1]   = skotex.BestHode.BestNr
     _FldNameList[2]   > skotex.BestHode.BestillingsDato
"BestHode.BestillingsDato" "BestDato" "99/99/99" "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > skotex.BestHode.LevDato
"BestHode.LevDato" "LevDato" "99/99/99" "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   = skotex.BestHode.BestStat
     _FldNameList[5]   = skotex.BestHode.LevNr
     _FldNameList[6]   > skotex.BestHode.Beskrivelse
"BestHode.Beskrivelse" ? "x(30)" "character" ? ? ? ? ? ? no ? no no "27" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   = skotex.BestHode.DirekteLev
     _FldNameList[8]   > "_<CALC>"
"OrdreNummer()" "OrdreNr" "x(7)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > skotex.BestHode.TotAntPar
"BestHode.TotAntPar" "Ant.par" ? "decimal" ? ? ? ? ? ? no ? no no "13.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > skotex.BestHode.TotDbKr
"BestHode.TotDbKr" "DbKr" ? "decimal" ? ? ? ? ? ? no ? no no "13" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > skotex.BestHode.TotInnkjVerdi
"BestHode.TotInnkjVerdi" "Innkj. verdi" ? "decimal" ? ? ? ? ? ? no ? no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > skotex.BestHode.TotSalgsVerdi
"BestHode.TotSalgsVerdi" "Salgsverdi" ? "decimal" ? ? ? ? ? ? no ? no no "16" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BROWSE-Bestilling */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-Lager
/* Query rebuild information for BROWSE BROWSE-Lager
     _TblList          = "Temp-Tables.tmpLager"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   = Temp-Tables.tmpLager.Butik
     _FldNameList[2]   = Temp-Tables.tmpLager.SumAntall
     _FldNameList[3]   = Temp-Tables.tmpLager.SumVerdi
     _FldNameList[4]   > Temp-Tables.tmpLager.DivAntall
"tmpLager.DivAntall" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   = Temp-Tables.tmpLager.Antall[1]
     _FldNameList[6]   = Temp-Tables.tmpLager.Antall[2]
     _FldNameList[7]   = Temp-Tables.tmpLager.Antall[3]
     _FldNameList[8]   = Temp-Tables.tmpLager.Antall[4]
     _FldNameList[9]   = Temp-Tables.tmpLager.Antall[5]
     _FldNameList[10]   = Temp-Tables.tmpLager.Antall[6]
     _FldNameList[11]   = Temp-Tables.tmpLager.Antall[7]
     _FldNameList[12]   = Temp-Tables.tmpLager.Antall[8]
     _FldNameList[13]   = Temp-Tables.tmpLager.Antall[9]
     _FldNameList[14]   = Temp-Tables.tmpLager.Antall[10]
     _FldNameList[15]   = Temp-Tables.tmpLager.Antall[11]
     _FldNameList[16]   = Temp-Tables.tmpLager.Antall[12]
     _FldNameList[17]   = Temp-Tables.tmpLager.Antall[13]
     _FldNameList[18]   = Temp-Tables.tmpLager.Antall[14]
     _FldNameList[19]   = Temp-Tables.tmpLager.Antall[15]
     _FldNameList[20]   = Temp-Tables.tmpLager.Antall[16]
     _FldNameList[21]   = Temp-Tables.tmpLager.Antall[17]
     _FldNameList[22]   = Temp-Tables.tmpLager.Antall[18]
     _FldNameList[23]   = Temp-Tables.tmpLager.Antall[19]
     _FldNameList[24]   = Temp-Tables.tmpLager.Antall[20]
     _FldNameList[25]   = Temp-Tables.tmpLager.Antall[21]
     _FldNameList[26]   = Temp-Tables.tmpLager.Antall[22]
     _FldNameList[27]   = Temp-Tables.tmpLager.Antall[23]
     _FldNameList[28]   = Temp-Tables.tmpLager.Antall[24]
     _FldNameList[29]   = Temp-Tables.tmpLager.Antall[25]
     _FldNameList[30]   = Temp-Tables.tmpLager.Antall[26]
     _FldNameList[31]   = Temp-Tables.tmpLager.Antall[27]
     _FldNameList[32]   = Temp-Tables.tmpLager.Antall[28]
     _FldNameList[33]   = Temp-Tables.tmpLager.Antall[29]
     _FldNameList[34]   = Temp-Tables.tmpLager.Antall[30]
     _FldNameList[35]   = Temp-Tables.tmpLager.Antall[31]
     _FldNameList[36]   = Temp-Tables.tmpLager.Antall[32]
     _FldNameList[37]   = Temp-Tables.tmpLager.Antall[33]
     _FldNameList[38]   = Temp-Tables.tmpLager.Antall[34]
     _FldNameList[39]   = Temp-Tables.tmpLager.Antall[35]
     _FldNameList[40]   = Temp-Tables.tmpLager.Antall[36]
     _FldNameList[41]   = Temp-Tables.tmpLager.Antall[37]
     _FldNameList[42]   = Temp-Tables.tmpLager.Antall[38]
     _FldNameList[43]   = Temp-Tables.tmpLager.Antall[39]
     _FldNameList[44]   = Temp-Tables.tmpLager.Antall[40]
     _FldNameList[45]   = Temp-Tables.tmpLager.Antall[41]
     _FldNameList[46]   = Temp-Tables.tmpLager.Antall[42]
     _FldNameList[47]   = Temp-Tables.tmpLager.Antall[43]
     _FldNameList[48]   = Temp-Tables.tmpLager.Antall[44]
     _FldNameList[49]   = Temp-Tables.tmpLager.Antall[45]
     _FldNameList[50]   = Temp-Tables.tmpLager.Antall[46]
     _FldNameList[51]   = Temp-Tables.tmpLager.Antall[47]
     _FldNameList[52]   = Temp-Tables.tmpLager.Antall[48]
     _Query            is OPENED
*/  /* BROWSE BROWSE-Lager */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-ArtInfo
/* Query rebuild information for FRAME FRAME-ArtInfo
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME FRAME-ArtInfo */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-dummy
/* Query rebuild information for FRAME FRAME-dummy
     _Query            is NOT OPENED
*/  /* FRAME FRAME-dummy */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-Lager
/* Query rebuild information for FRAME FRAME-Lager
     _Query            is NOT OPENED
*/  /* FRAME FRAME-Lager */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-Ordre
/* Query rebuild information for FRAME FRAME-Ordre
     _Query            is NOT OPENED
*/  /* FRAME FRAME-Ordre */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 8.62
       COLUMN          = 1
       HEIGHT          = 18.81
       WIDTH           = 159
       HIDDEN          = no
       SENSITIVE       = yes.

CREATE CONTROL-FRAME IMAGE-Sko ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 2.91
       COLUMN          = 132
       HEIGHT          = 4.81
       WIDTH           = 27
       HIDDEN          = no
       SENSITIVE       = yes.
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {1EFB6596-857C-11D1-B16A-00C0F0283628} type: TabStrip */
/* IMAGE-Sko OCXINFO:CREATE-CONTROL from: {9A93B740-C96B-11D0-8883-444553540000} type: Picbuf */
      IMAGE-Sko:MOVE-AFTER(FRAME FRAME-Lager:HANDLE).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-ArtKort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-ArtKort C-ArtKort
ON END-ERROR OF C-ArtKort /* Artikelkort */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-ArtKort C-ArtKort
ON WINDOW-CLOSE OF C-ArtKort /* Artikelkort */
DO:
  IF CAN-FIND(FIRST tmpChild WHERE
               valid-handle(tmpChild.wChild)) THEN
    DO:
      wBekreft = FALSE.
      MESSAGE 'Det er startet andre programmer fra dette vinduet.' SKIP
              'Avsluttes dette vinduet, vil alle underliggende programmer' SKIP
              'også bli avsluttet.'
              VIEW-AS ALERT-BOX WARNING BUTTONS YES-NO TITLE 'Bekreft avsluttning'
              UPDATE wBekreft
              .
    END.
  ELSE wBekreft = TRUE.
  IF wBekreft <> TRUE THEN
  RETURN NO-APPLY.
  
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  /* RETURN NO-APPLY. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-ArtInfo
&Scoped-define SELF-NAME ArtBas.anv-id
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ArtBas.anv-id C-ArtKort
ON LEAVE OF ArtBas.anv-id IN FRAME FRAME-ArtInfo /* Bruk */
DO:
  FIND Anv-Kod NO-LOCK WHERE
    Anv-Kod.Anv-Id = int(ArtBas.Anv-Id:screen-value IN FRAME FRAME-ArtInfo) NO-ERROR.
  IF AVAILABLE Anv-Kod THEN DISPLAY Anv-Kod.AnvBeskr WITH FRAME FRAME-ArtInfo.
  ELSE DO:
      DISPLAY
        "" @ ArtBas.Anv-Id
        "" @ Anv-Kod.AnvBeskr
      WITH FRAME FRAME-ArtInfo.  
      RETURN NO-APPLY.
  END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME B-ByttVareGr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-ByttVareGr C-ArtKort
ON CHOOSE OF B-ByttVareGr IN FRAME DEFAULT-FRAME /* Bytt varegruppe... */
DO:
    RETURN.
  RUN ByttVg.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-Lager
&Scoped-define SELF-NAME B-Chart
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Chart C-ArtKort
ON CHOOSE OF B-Chart IN FRAME FRAME-Lager /* Button 1 */
DO:
    DEFINE VAR iCount       AS INTEGER    NO-UNDO.
    DEFINE VAR cLabels      AS CHARACTER  NO-UNDO.
    DEFINE VAR hColumn      AS HANDLE     NO-UNDO.
    DEFINE VAR iAntStorl    AS INTEGER    NO-UNDO.
    DEFINE VAR iFirstVerdi  AS INTEGER    NO-UNDO.
    DEFINE VAR iLastVerdi   AS INTEGER    NO-UNDO.
    DEFINE VAR lRest        AS LOGICAL    NO-UNDO.
    DEFINE VAR cColLabel    AS CHAR NO-UNDO.
    DEFINE VAR cRowLabel    AS CHAR NO-UNDO.
    DEFINE VAR cColValues  AS CHARACTER  NO-UNDO.
    DEFINE VAR cTmpVerdier AS CHARACTER  NO-UNDO.
    DEFINE VAR cColEnable    AS CHAR NO-UNDO.      
    ASSIGN hColumn = BROWSE BROWSE-Lager:FIRST-COLUMN.
    REPEAT WHILE VALID-HANDLE(hColumn):
        IF hColumn:LABEL = "" THEN
            LEAVE.
        ASSIGN iAntStorl = iAntStorl + 1.
        IF iAntStorl > 4 THEN DO:
          ASSIGN cLabels = cLabels + (IF cLabels = "" THEN "" ELSE ",") + REPLACE(hColumn:LABEL,",",".").
        END.
        ASSIGN hColumn = hColumn:NEXT-COLUMN.
    END.
    /* antal från 1:a strl till Rest */
    ASSIGN iAntStorl = iAntStorl - 4. 
    /* För att visa minst möjligt antal startar vi vid 1:a storlek som har värde */
    /* och slutar vid sista med värde, inte medräknat rest. */
    /* Har vi rest kommer den med men hoppar över från sista storlek med antal */
    /* till sista. */
    ASSIGN iFirstVerdi = 100
           iLastVerdi  = 0.

    FOR EACH tmpLager:
        DO iCount = 1 TO iAntStorl - 1:
            IF tmpLager.Antall[iCount] <> "" THEN
                ASSIGN iFirstVerdi = IF iCount < iFirstVerdi THEN iCount ELSE iFirstVerdi
                       iLastVerdi  = IF iCount > iLastVerdi  THEN iCount ELSE iLastVerdi.
        END.
        IF tmpLager.Antall[iAntStorl] <> "" THEN
            ASSIGN lRest = TRUE.
    END.
    /* Här bygger vi lablar */
    DO iCount = iFirstVerdi TO iLastVerdi:
        ASSIGN cRowLabel = cRowLabel + (IF cRowLabel = "" THEN "" ELSE ",") + ENTRY(iCount,cLabels).
    END.
    IF lRest = TRUE THEN 
        ASSIGN cRowLabel = cRowLabel + "," + ENTRY(iAntStorl,cLabels).
    /* Här bygger vi värdelistor */
    FOR EACH tmpLager:
        ASSIGN cTmpVerdier = FILL(",",NUM-ENTRIES(cRowLabel) - 1)
               cColLabel   = cColLabel + (IF cColLabel = "" THEN "" ELSE ",")
                                    + TRIM(tmpLager.Butik) + " - (Ant " + STRING(INT(tmpLager.SumAntall)) + ")"
               cColEnable  = cColEnable + (IF cColEnable = "" THEN "" ELSE ",") + "0".
        DO iCount = iFirstVerdi TO iLastVerdi:
            ASSIGN ENTRY(iCount - iFirstVerdi + 1,cTmpVerdier) = TRIM(STRING(INT(tmpLager.Antall[iCount]))).
        END.
        IF lRest = TRUE THEN
            ASSIGN ENTRY(NUM-ENTRIES(cTmpVerdier),cTmpVerdier) = TRIM(STRING(INT(tmpLager.Antall[iAntStorl]))).
        ASSIGN cColValues = cColValues + (IF cColValues = "" THEN "" ELSE ";") + cTmpVerdier.
    END.
    ASSIGN ENTRY(NUM-ENTRIES(cColEnable),cColEnable) = "1".
    IF iLastVerdi = 0 THEN DO:
        MESSAGE "Ingen data til visning"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN.
    END.
    RUN d-mschart.w ("Lagerverdier",ENTRY((2 * INT(RS-Vis:SCREEN-VALUE)) - 1,RS-Vis:RADIO-BUTTONS)
                                          ,cRowLabel,cColLabel,cColValues,cColEnable).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-Ordre
&Scoped-define SELF-NAME B-OppdBest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-OppdBest C-ArtKort
ON CHOOSE OF B-OppdBest IN FRAME FRAME-Ordre /* Oppdate browser */
DO:
  RUN VisArtBas.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-Lager
&Scoped-define SELF-NAME B-OppdLAger
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-OppdLAger C-ArtKort
ON CHOOSE OF B-OppdLAger IN FRAME FRAME-Lager /* Oppdater lagerbilde */
DO:
  wRS-Vis = INPUT RS-Vis.
  RUN VisLager (TRUE).  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME ArtBas.BildNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ArtBas.BildNr C-ArtKort
ON RETURN OF ArtBas.BildNr IN FRAME DEFAULT-FRAME /* Bilde */
OR tab OF ArtBas.BildNr
DO:
  RUN ArtBasBildNr.
  IF RETURN-VALUE = "AVBRYT" THEN
      RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-Bestilling
&Scoped-define FRAME-NAME FRAME-Ordre
&Scoped-define SELF-NAME BROWSE-Bestilling
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-Bestilling C-ArtKort
ON MOUSE-SELECT-DBLCLICK OF BROWSE-Bestilling IN FRAME FRAME-Ordre
DO:
/*   apply "CHOOSE":U to BUTTON-EndreBest in frame FRAME-Ordre. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-Bestilling C-ArtKort
ON ROW-DISPLAY OF BROWSE-Bestilling IN FRAME FRAME-Ordre
DO:
  BestHode.TotInnkjVerdi:SCREEN-VALUE IN BROWSE browse-Bestilling = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-Lager
&Scoped-define FRAME-NAME FRAME-Lager
&Scoped-define SELF-NAME BROWSE-Lager
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-Lager C-ArtKort
ON ROW-DISPLAY OF BROWSE-Lager IN FRAME FRAME-Lager
DO:
  
  IF tmpLager.Butik = "Total" THEN
    RUN SetBrowseRadFont.
  tmpLager.SumVerdi = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help C-ArtKort
ON CHOOSE OF Btn_Help IN FRAME DEFAULT-FRAME /* Help */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
   {winhlp.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Angre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Angre C-ArtKort
ON CHOOSE OF BUTTON-Angre IN FRAME DEFAULT-FRAME /* Ångra */
DO:
  RUN RensFrame.
  RUN VisArtBas.
  ASSIGN wModus = "ENDRE".
  RUN ByttFrame.
  RUN VisLager (TRUE).
  RUN Toolbar.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-Ordre
&Scoped-define SELF-NAME BUTTON-EndreBest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-EndreBest C-ArtKort
ON CHOOSE OF BUTTON-EndreBest IN FRAME FRAME-Ordre /* Endre.. */
DO:
  IF NOT AVAILABLE BestHode THEN
    RETURN NO-APPLY.
  CREATE tmpChild.  
  ASSIGN
    wBestHodeRecid = recid(BestHode).
  RUN w-gridord.w PERSISTENT SET tmpChild.wChild (INPUT wArtBasRecid, INPUT-OUTPUT wBestHodeRecid, "ENDRE").
  FIND BestHode NO-LOCK WHERE
      recid(BestHode) = wBestHodeRecid NO-ERROR.
  RUN VisArtBas.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Innleveranse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Innleveranse C-ArtKort
ON CHOOSE OF BUTTON-Innleveranse IN FRAME FRAME-Ordre /* Innleveranse... */
DO:
  IF ArtBas.LopNr = 0 OR ArtBas.LopNr = ? THEN
    DO:
      MESSAGE Tx("Artikkelen må tildeles løpenummer før innleveranse kan gjøres!",133)
        VIEW-AS ALERT-BOX MESSAGE TITLE Tx("Melding",107).
      RETURN NO-APPLY.
    END.
    
  IF NOT AVAILABLE BestHode THEN
    RETURN NO-APPLY.

  IF BestHode.BestStat < 4  THEN
    DO:
      MESSAGE Tx("Bestilling med denne status kan ikke innleveres!",134)
        VIEW-AS ALERT-BOX TITLE Tx("Melding",107).
      RETURN NO-APPLY.
    END.  
  CREATE tmpChild.
  ASSIGN
    wBestHodeRecid = recid(BestHode).
  RUN w-gridinnlev.w PERSISTENT SET tmpChild.wChild (INPUT wArtBasRecid, INPUT-OUTPUT wBestHodeRecid, "INLEV").
  FIND BestHode NO-LOCK WHERE
      recid(BestHode) = wBestHodeRecid NO-ERROR.
   RUN VisArtBas.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Kalkyle
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Kalkyle C-ArtKort
ON CHOOSE OF BUTTON-Kalkyle IN FRAME FRAME-Ordre /* Kalkyle... */
DO:
  IF NOT AVAILABLE BestHode THEN
    RETURN NO-APPLY.
    
  ASSIGN
    wBestHodeRecid = recid(BestHode).
  RUN d-vbestkalkyle.w (INPUT wArtBasRecid, INPUT wBestHodeRecid).
  FIND BestHode NO-LOCK WHERE
      recid(BestHode) = wBestHodeRecid NO-ERROR.
  RUN VisArtBas.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME BUTTON-Kopier
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Kopier C-ArtKort
ON CHOOSE OF BUTTON-Kopier IN FRAME DEFAULT-FRAME /* Kopiera */
DO:
  /* Lagrer det som er endret. */
  RUN LagreArtBas(1).
  
  /* Bytter til første flik */
  ASSIGN 
    wAktivFlip = 1
    wModus   = "NY"
    wKopi    = TRUE
    .
  RUN ByttFrame.  
  RUN BlankLager.
  RUN Toolbar.
  DISPLAY 
    "" @ ArtBas.ArtikkelNr 
    "" @ ArtBas.LopNr
  WITH FRAME DEFAULT-FRAME.

  {&OPEN-BROWSERS-IN-QUERY-FRAME-Ordre}
  APPLY "ENTRY":U TO ArtBas.Vg IN FRAME DEFAULT-FRAME.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Lagre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Lagre C-ArtKort
ON CHOOSE OF BUTTON-Lagre IN FRAME DEFAULT-FRAME /* Lagra */
DO:
  RUN LagreArtBas (0).
  IF RETURN-VALUE <> "AVBRYT" THEN
    DO:
      ASSIGN wModus = "ENDRE".
      RUN ByttFrame. /* For † enable knapper og felt korrekt */
    END.
  ELSE DO:
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Next
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Next C-ArtKort
ON CHOOSE OF BUTTON-Next IN FRAME DEFAULT-FRAME /* Neste */
DO:
  /* Det er ikke lov å bytte artikkel hvis artikkelen ikke har en ArtPrisPost. */
  DO:
      FIND ArtBas NO-LOCK WHERE
          recid(ArtBas) = wArtBasRecid NO-ERROR.
      IF AVAILABLE ArtBas THEN
      DO:
          IF NOT CAN-FIND(FIRST ArtPris OF ArtBas) THEN
          DO:
              MESSAGE "Det må legges opp en kalkyle på denne artikkelen ("
                       ArtBAs.Vg ArtBAs.LopNr ")."
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
              RETURN "AVBRYT".
          END.
      END.
  END.
  IF VALID-HANDLE(hParentHandle) THEN DO:
      RUN PrevNext IN hParentHandle ("Next").
  END.
  ELSE
      RUN BUTTON-Next.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Ny
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Ny C-ArtKort
ON CHOOSE OF BUTTON-Ny IN FRAME DEFAULT-FRAME /* Ny */
DO:
  DEF VAR wDummy AS LOG INITIAL TRUE NO-UNDO.
  DEF VAR wwArtBasRecid AS INT NO-UNDO.
  RUN BUTTON-Ny.
  
  ASSIGN
    ArtBas.Lager:screen-value      = "J"
    ArtBas.Utgatt:screen-value     = "No"
    ArtBas.Storrelser:screen-value = "Yes"
    T-LapTop     = IF wLapTop THEN TRUE ELSE FALSE.
  DISPLAY
    T-LapTop
  WITH FRAME DEFAULT-FRAME.
  
  RUN RensFrame.

  FIND ArtBas NO-LOCK WHERE
    recid(ArtBas) = wArtBasRecid NO-ERROR.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-Ordre}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-Ordre
&Scoped-define SELF-NAME BUTTON-NyBest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-NyBest C-ArtKort
ON CHOOSE OF BUTTON-NyBest IN FRAME FRAME-Ordre /* Ny.. */
DO:
  RUN NyBest.
  RUN VisArtBas.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME BUTTON-Ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Ok C-ArtKort
ON CHOOSE OF BUTTON-Ok IN FRAME DEFAULT-FRAME /* Ok */
DO:
  IF CAN-FIND(FIRST tmpChild WHERE
               valid-handle(tmpChild.wChild)) THEN
    DO:
      wBekreft = FALSE.
      MESSAGE Tx('Det er startet andre programmer fra dette vinduet.' + chr(13) +
              'Avsluttes dette vinduet, vil alle underliggende programmer' + CHR(13) +
              'også bli avsluttet.',135)
              VIEW-AS ALERT-BOX WARNING BUTTONS YES-NO TITLE Tx('Bekreftelse',105)
              UPDATE wBekreft
              .
    END.
  ELSE wBekreft = TRUE.
  IF wBekreft <> TRUE THEN
  RETURN NO-APPLY.
  
  RUN LagreArtBas (0).
  IF RETURN-VALUE = "OK" THEN
    DO:
      APPLY "CLOSE":U TO THIS-PROCEDURE.  
      RETURN wModus + "," + string(wArtBasRecid).
    END.
  ELSE DO:
    READKEY PAUSE 0.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-Lager
&Scoped-define SELF-NAME BUTTON-Overfor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Overfor C-ArtKort
ON CHOOSE OF BUTTON-Overfor IN FRAME FRAME-Lager /* Overføring... */
DO:
    RETURN.
  RUN Overfor.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME BUTTON-Paste
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Paste C-ArtKort
ON CHOOSE OF BUTTON-Paste IN FRAME DEFAULT-FRAME
DO:
  DO WITH FRAME DEFAULT-FRAME:
    {limfraclipboard.i 
      &BildNr = "input ArtBas.BildNr"
      &Skjerm = "DISPLAY wBildNr @ ArtBas.BildNr WITH FRAME Default-Frame."
    }

  END.
  SELF:PRIVATE-DATA = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Paste C-ArtKort
ON DROP-FILE-NOTIFY OF BUTTON-Paste IN FRAME DEFAULT-FRAME
DO:
    DEFINE VARIABLE cFilnavn AS CHARACTER  NO-UNDO.
    IF SELF:NUM-DROPPED-FILES = 1 THEN DO:
        ASSIGN cFilNavn = SELF:GET-DROPPED-FILE(1).
        IF NOT CAN-DO("bmp,jpg",ENTRY(NUM-ENTRIES(cFilNavn,"."),cFilNavn,".")) THEN
            MESSAGE "Tillatte filtyper: '.bmp,.jpg'"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
        ELSE IF ENTRY(NUM-ENTRIES(cFilNavn,"\"),cFilNavn,"\") BEGINS "mini" THEN DO:
            MESSAGE "Fil med navn 'mini...' kann ikke leses inn."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RETURN.
        END.
        ELSE DO:
            ASSIGN SELF:PRIVATE-DATA = cFilnavn.
            APPLY "CHOOSE" TO SELF.
        END.
    END.
    ELSE DO:
        MESSAGE "Endast 1 fil tillatt!"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Prev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Prev C-ArtKort
ON CHOOSE OF BUTTON-Prev IN FRAME DEFAULT-FRAME /* Forrige */
DO:
  /* Det er ikke lov å bytte artikkel hvis artikkelen ikke har en ArtPrisPost. */
  DO:
      FIND ArtBas NO-LOCK WHERE
          recid(ArtBas) = wArtBasRecid NO-ERROR.
      IF AVAILABLE ArtBas THEN
      DO:
          IF NOT CAN-FIND(FIRST ArtPris OF ArtBas) THEN
          DO:
              MESSAGE "Det må legges opp en kalkyle på denne artikkelen ("
                       ArtBAs.Vg ArtBAs.LopNr ")."
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
              RETURN "AVBRYT".
          END.
      END.
  END.
  IF VALID-HANDLE(hParentHandle) THEN DO:
      RUN PrevNext IN hParentHandle ("Prev").
  END.
  ELSE
      RUN BUTTON-Prev.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SettLopNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SettLopNr C-ArtKort
ON CHOOSE OF BUTTON-SettLopNr IN FRAME DEFAULT-FRAME /* Sett løpenummer... */
DO:
    RETURN.
  RUN SettLopenummer.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Slett
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Slett C-ArtKort
ON CHOOSE OF BUTTON-Slett IN FRAME DEFAULT-FRAME /* Radera */
DO:
  RUN BUTTON-Slett.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-Ordre
&Scoped-define SELF-NAME BUTTON-SlettBest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SlettBest C-ArtKort
ON CHOOSE OF BUTTON-SlettBest IN FRAME FRAME-Ordre /* Slette */
DO:

  RUN SlettBestilling (1).
  RUN VisArtBas.
  RETURN NO-APPLY.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME BUTTON-SokArt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokArt C-ArtKort
ON CHOOSE OF BUTTON-SokArt IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF ArtBas.LopNr
DO:
/*   run SokLopNr. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokBilde
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokBilde C-ArtKort
ON CHOOSE OF BUTTON-SokBilde IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF ArtBas.BildNr
DO:
  RUN SokBilde.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-ArtInfo
&Scoped-define SELF-NAME BUTTON-SokBruk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokBruk C-ArtKort
ON CHOOSE OF BUTTON-SokBruk IN FRAME FRAME-ArtInfo /* ... */
OR F10 OF ArtBas.Anv-Id
DO:
  RUN SokAnvId.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokDato C-ArtKort
ON CHOOSE OF BUTTON-SokDato IN FRAME FRAME-ArtInfo /* ... */
OR F10 OF ArtBas.LevDato1
DO:
  /* Start søkeprogram */
  {soek.i
    &Felt        = ArtBas.LevDato1
    &Program     = kalender.w
    &Frame       = FRAME-ArtInfo
    &ExtraParam  = "Tx('Datosøk',136)"
  }   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokDato-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokDato-2 C-ArtKort
ON CHOOSE OF BUTTON-SokDato-2 IN FRAME FRAME-ArtInfo /* ... */
OR F10 OF ArtBas.LevDato2
DO:
  /* Start søkeprogram */
  {soek.i
    &Felt        = ArtBas.LevDato2
    &Program     = kalender.w
    &Frame       = "FRAME-ArtInfo"
    &ExtraParam  = "Tx('Datosøk',136)"
  }   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokFarge
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokFarge C-ArtKort
ON CHOOSE OF BUTTON-SokFarge IN FRAME FRAME-ArtInfo /* ... */
OR F10 OF ArtBas.Farg
DO:
  RUN SokFarg.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokInner
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokInner C-ArtKort
ON CHOOSE OF BUTTON-SokInner IN FRAME FRAME-ArtInfo /* ... */
OR F10 OF ArtBas.Inner-Id
DO:
  RUN SokInnerId.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokLalst
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokLalst C-ArtKort
ON CHOOSE OF BUTTON-SokLalst IN FRAME FRAME-ArtInfo /* ... */
OR F10 OF ArtBas.Last-Id
DO:
  RUN SokLastId.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME BUTTON-SokLev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokLev C-ArtKort
ON CHOOSE OF BUTTON-SokLev IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF ArtBas.LevNr
DO:
  RUN SokLevNr.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-ArtInfo
&Scoped-define SELF-NAME BUTTON-SokMaterial
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokMaterial C-ArtKort
ON CHOOSE OF BUTTON-SokMaterial IN FRAME FRAME-ArtInfo /* ... */
OR F10 OF ArtBas.MatKod
DO:
  RUN SokMatKod.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokOver
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokOver C-ArtKort
ON CHOOSE OF BUTTON-SokOver IN FRAME FRAME-ArtInfo /* ... */
OR F10 OF ArtBas.Ov-Id
DO:
  RUN SokOverdel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokProdusent
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokProdusent C-ArtKort
ON CHOOSE OF BUTTON-SokProdusent IN FRAME FRAME-ArtInfo /* ... */
OR F10 OF ArtBas.ProdNr
DO:
  /* Start søkeprogram */
  {soek.i
    &Felt        = ArtBas.ProdNr
    &Program     = d-bprodusent.w
    &Frame       = FRAME-ArtInfo
    &PostRun     = "find Produsent no-lock where
                    recid(Produsent) = int(return-value) no-error."
    &OptDisp     = "Produsent.ProdNr when available Produsent @ ArtBas.ProdNr 
                    Produsent.Beskrivelse when available Produsent"
  }   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokProv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokProv C-ArtKort
ON CHOOSE OF BUTTON-SokProv IN FRAME FRAME-ArtInfo /* ... */
OR F10 OF ArtBas.ProvKod
DO:
  RUN SokProvKod.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokRabatt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokRabatt C-ArtKort
ON CHOOSE OF BUTTON-SokRabatt IN FRAME FRAME-ArtInfo /* ... */
OR F10 OF ArtBas.RabKod
DO:
  RUN SokRabKod.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokSesong
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokSesong C-ArtKort
ON CHOOSE OF BUTTON-SokSesong IN FRAME FRAME-ArtInfo /* ... */
OR F10 OF ArtBas.SaSong
DO:
  RUN SokSaSong.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokSlit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokSlit C-ArtKort
ON CHOOSE OF BUTTON-SokSlit IN FRAME FRAME-ArtInfo /* ... */
OR F10 OF ArtBas.Slit-Id
DO:
  RUN SokSlitId.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokStrType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokStrType C-ArtKort
ON CHOOSE OF BUTTON-SokStrType IN FRAME FRAME-ArtInfo /* ... */
OR F10 OF ArtBas.StrTypeId
DO:
  RUN SokStrType.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokValuta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokValuta C-ArtKort
ON CHOOSE OF BUTTON-SokValuta IN FRAME FRAME-ArtInfo /* ... */
OR F10 OF ArtBas.ValKod
DO:
  RUN SokValkod.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokVaremerke
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokVaremerke C-ArtKort
ON CHOOSE OF BUTTON-SokVaremerke IN FRAME FRAME-ArtInfo /* ... */
OR F10 OF ArtBas.VMId
DO:
  /* Start søkeprogram */
  {soek.i
    &Felt        = ArtBas.VMId
    &Program     = d-bvaremerke.w
    &Frame       = FRAME-ArtInfo
    &PostRun     = "find Varemerke no-lock where
                    recid(Varemerke) = int(return-value) no-error."
    &OptDisp     = "VareMerke.VMId when available Varemerke @ ArtBas.VMId 
                    Varemerke.Beskrivelse when available Varemerke"
  }   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME BUTTON-SokVg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokVg C-ArtKort
ON CHOOSE OF BUTTON-SokVg IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF ArtBas.Vg
DO:
  RUN SokVg.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokVgKat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokVgKat C-ArtKort
ON CHOOSE OF BUTTON-SokVgKat IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF ArtBas.VgKat
DO:
  RUN SokVgKat.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Trans
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Trans C-ArtKort
ON CHOOSE OF BUTTON-Trans IN FRAME DEFAULT-FRAME /* Vis transaksjoner... */
DO:
  RUN VisTranser.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame C-ArtKort OCX.Click
PROCEDURE CtrlFrame.TabStrip.Click .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/
  IF wAktivFlip <> INT(chTabStrip:SelectedItem:Index) THEN 
  DO:
     ASSIGN wAktivFlip = INT(chTabStrip:SelectedItem:Index).
     RUN ByttFrame. /* Bytter tab */
  END.   
  ELSE DO:
      /* Bytter frame */
      CASE wAktivFlip:
         WHEN 1 THEN 
           DO WITH FRAME DEFAULT-FRAME:
             IF ArtBas.Vg:Sensitive = TRUE 
               THEN APPLY "entry" TO ArtBas.Vg IN FRAME DEFAULT-FRAME.
             ELSE APPLY "entry" TO ArtBas.Beskr IN FRAME DEFAULT-FRAME.
           END.        
         WHEN 2 THEN 
           DO:
             FRAME FRAME-Dummy:MOVE-TO-TOP().
/*              /* TEST */                                                                                                                      */
/*              IF VALID-HANDLE(wKalkyle) THEN                                                                                                  */
/*                  RUN MoveToTopp IN wKalkyle.                                                                                                 */
/*              /* TEST slutt */                                                                                                                */
/*                                                                                                                                              */
/*              if available ArtBas then                                                                                                        */
/*                do:                                                                                                                           */
/*                  if valid-handle(wKalkyle) then                                                                                              */
/*                    run ByttObjekt in wKalkyle (ArtBas.ArtikkelNr).                                                                           */
/*                  else                                                                                                                        */
/*                    run w-kalkyle.w persistent set wKalkyle (ArtBas.ArtikkelNr,THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE,h_PrisKo,wModus). */
/*                end.                                                                                                                          */
           END.
         WHEN 3 THEN 
           HISTORIKK:
           DO:
             APPLY "entry":U TO BROWSE-Lager IN FRAME FRAME-Lager.
           END. /* HISTORIKK */
         WHEN 4 THEN 
           DO:
             APPLY "entry":u TO BROWSE BROWSE-Bestilling.
           END.
         WHEN 5 THEN 
           DO:
             IF AVAILABLE ArtBas THEN
               DO:
                 IF VALID-HANDLE(wHistorikk) THEN
                     RUN MoveToTopp IN wHistorikk.

                 IF AVAILABLE ArtBas THEN
                   DO:
                     IF valid-handle(wHistorikk) THEN
                       RUN ByttObjekt IN wHistorikk (string(ArtBas.ArtikkelNr,"9999999999999")).  
                     ELSE         
                       RUN w-stlinjeJFSpec.w PERSISTENT SET wHistorikk (wArtBasRecid,string(ArtBas.ArtikkelNr,"9999999999999"),"ARTIKKEL","MANED",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE).  
                   END.
               END.
           END.
      END.   

  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-ArtInfo
&Scoped-define SELF-NAME ArtBas.Farg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ArtBas.Farg C-ArtKort
ON LEAVE OF ArtBas.Farg IN FRAME FRAME-ArtInfo /* Farg */
DO:
  FIND Farg NO-LOCK WHERE
    Farg.Farg = int(ArtBas.Farg:screen-value IN FRAME FRAME-ArtInfo) NO-ERROR.
  IF AVAILABLE Farg THEN
    DISPLAY Farg.FarBeskr WITH FRAME FRAME-ArtInfo.
  ELSE DO:
      DISPLAY
        "" @ ArtBas.Farg
        "" @ Farg.FarBeskr
      WITH FRAME FRAME-ArtInfo.  
      RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME IMAGE-Sko
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL IMAGE-Sko C-ArtKort OCX.DblClick
PROCEDURE IMAGE-Sko.Picbuf.DblClick .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/

  IF AVAILABLE ArtBAs THEN
    DO:
      FIND BildeRegister OF ArtBas NO-ERROR.
      IF AVAILABLE BildeRegister THEN
        RUN d-visbil.w (INPUT recid(BildeRegister)).
    END.
  RETURN NO-APPLY.    


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-ArtInfo
&Scoped-define SELF-NAME ArtBas.inner-id
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ArtBas.inner-id C-ArtKort
ON LEAVE OF ArtBas.inner-id IN FRAME FRAME-ArtInfo /* Innersåle */
DO:
  FIND Innersula NO-LOCK WHERE
    Innersula.Inner-Id = int(ArtBas.Inner-Id:screen-value IN FRAME FRAME-ArtInfo) NO-ERROR.
  IF AVAILABLE InnerSula THEN 
    DISPLAY InnerSula.InnerBeskr WITH FRAME FRAME-ArtInfo.
  ELSE DO:
      DISPLAY
        "" @ ArtBas.Inner-Id
        "" @ InnerSula.InnerBeskr
      WITH FRAME FRAME-ArtInfo.  
      RETURN NO-APPLY.
  END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME ArtBas.lager
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ArtBas.lager C-ArtKort
ON RETURN OF ArtBas.lager IN FRAME DEFAULT-FRAME /* Lager */
DO:
  APPLY "tab":U TO {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-ArtInfo
&Scoped-define SELF-NAME ArtBas.last-id
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ArtBas.last-id C-ArtKort
ON LEAVE OF ArtBas.last-id IN FRAME FRAME-ArtInfo /* Læst */
DO:
  FIND Last-Sko NO-LOCK WHERE
    Last-Sko.Last-Id = int(ArtBas.Last-Id:screen-value IN FRAME FRAME-ArtInfo) NO-ERROR.
  IF AVAILABLE Last-Sko THEN DISPLAY Last-Sko.LastBeskr WITH FRAME FRAME-ArtInfo.
  ELSE DO:
      DISPLAY
        "" @ ArtBas.Last-Id
        "" @ Last-Sko.LastBeskr
      WITH FRAME FRAME-ArtInfo.  
      RETURN NO-APPLY.
  END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME ArtBas.LevFargKod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ArtBas.LevFargKod C-ArtKort
ON TAB OF ArtBas.LevFargKod IN FRAME DEFAULT-FRAME /* LevFargKod */
OR "RETURN":U OF ArtBas.LevFargKod
DO:
  APPLY "ENTRY":U TO ArtBas.StrTypeId IN FRAME FRAME-ArtInfo.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ArtBas.LevNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ArtBas.LevNr C-ArtKort
ON TAB OF ArtBas.LevNr IN FRAME DEFAULT-FRAME /* LevNr */
OR "RETURN":U OF ArtBas.LevNr
DO:
  RUN Sok2LevBas.  
  IF RETURN-VALUE = "AVBRYT" THEN
      RETURN NO-APPLY.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ArtBas.LopNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ArtBas.LopNr C-ArtKort
ON LEAVE OF ArtBas.LopNr IN FRAME DEFAULT-FRAME /* Løpenr */
DO:
  IF (ArtBas.LopNr:screen-value IN FRAME default-frame <> "?" AND
      ArtBas.LopNr:screen-value IN FRAME default-frame <> "")THEN
    DO:
      FIND ArtBas NO-LOCK WHERE
        ArtBas.Vg = int(ArtBas.Vg:screen-value IN FRAME DEFAULT-FRAME) AND
        ArtBas.LopNr = int(ArtBas.LopNr:screen-value IN FRAME default-frame) NO-ERROR.

      IF AVAILABLE ArtBas THEN
        DO:
          MESSAGE "Artikkel med dette varegruppe/löpenummer finnes fra för!" SKIP
                  "Angi et annet varegruppe/löpenummer" VIEW-AS ALERT-BOX 
                  WARNING TITLE "Registreringsfeil".
          RETURN NO-APPLY.
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-ArtInfo
&Scoped-define SELF-NAME ArtBas.MatKod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ArtBas.MatKod C-ArtKort
ON LEAVE OF ArtBas.MatKod IN FRAME FRAME-ArtInfo /* Material */
DO:
  FIND Material NO-LOCK WHERE
    Material.MatKod = int(ArtBas.MatKod:screen-value IN FRAME FRAME-ArtInfo) NO-ERROR.
  IF AVAILABLE Material THEN
    DISPLAY Material.MatBeskr WITH FRAME FRAME-ArtInfo.
  ELSE DO:
      DISPLAY
        "" @ ArtBas.MatKod
        "" @ Material.MatBeskr
      WITH FRAME FRAME-ArtInfo.  
      RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Avslutt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Avslutt C-ArtKort
ON CHOOSE OF MENU-ITEM m_Avslutt /* Avslutt */
DO:
  APPLY "CHOOSE":U TO BUTTON-Ok IN FRAME DEFAULT-FRAME.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ArtBas.Notat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ArtBas.Notat C-ArtKort
ON TAB OF ArtBas.Notat IN FRAME FRAME-ArtInfo /* Øvrig */
DO:
  IF wModus = "NY" THEN
    APPLY "ENTRY":U TO ArtBas.Vg    IN FRAME DEFAULT-FRAME.
  ELSE
    APPLY "ENTRY":U TO ArtBas.VgKat IN FRAME DEFAULT-FRAME.
  RETURN NO-APPLY.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ArtBas.ov-id
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ArtBas.ov-id C-ArtKort
ON LEAVE OF ArtBas.ov-id IN FRAME FRAME-ArtInfo /* Innerfor */
DO:
  FIND Ovandel NO-LOCK WHERE
    Ovandel.Ov-Id = int(ArtBas.Ov-Id:screen-value IN FRAME FRAME-ArtInfo) NO-ERROR.
  IF AVAILABLE Ovandel THEN DISPLAY Ovandel.OvBeskr WITH FRAME FRAME-ArtInfo.
  ELSE DO:
      DISPLAY
        "" @ ArtBas.Ov-Id
        "" @ Ovandel.OvBeskr
      WITH FRAME FRAME-ArtInfo.  
      RETURN NO-APPLY.
  END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ArtBas.ProdNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ArtBas.ProdNr C-ArtKort
ON TAB OF ArtBas.ProdNr IN FRAME FRAME-ArtInfo /* Produsent */
OR "RETURN":U OF ArtBas.ProdNr
DO:
  FIND Produsent NO-LOCK WHERE
    Produsent.ProdNr = INPUT ArtBas.ProdNr NO-ERROR.
  IF NOT AVAILABLE Produsent THEN
    DO:
      DISPLAY
        "" @ ArtBas.ProdNr
        "" @ Produsent.Beskrivelse
      WITH FRAME FRAME-ArtInfo.  
      RETURN NO-APPLY.
    END.
  DISPLAY
    Produsent.Beskrivelse
  WITH FRAME FRAME-ArtInfo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ArtBas.ProvKod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ArtBas.ProvKod C-ArtKort
ON LEAVE OF ArtBas.ProvKod IN FRAME FRAME-ArtInfo /* ProvKod */
DO:
  FIND Prov NO-LOCK WHERE
    Prov.ProvKod = int(ArtBas.ProvKod:screen-value IN FRAME FRAME-ArtInfo) NO-ERROR.
  IF AVAILABLE Prov THEN
    DISPLAY Prov.ProvProc WITH FRAME FRAME-ArtInfo.
  ELSE DO:
      DISPLAY
        "" @ ArtBas.ProvKod
        "" @ Prov.ProvProc
      WITH FRAME FRAME-ArtInfo.  
      RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ArtBas.RabKod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ArtBas.RabKod C-ArtKort
ON LEAVE OF ArtBas.RabKod IN FRAME FRAME-ArtInfo /* Maks rab. */
DO:
  FIND Rabatt NO-LOCK WHERE
    Rabatt.RabKod = int(ArtBas.RabKod:screen-value IN FRAME FRAME-ArtInfo) NO-ERROR.
  IF AVAILABLE Rabatt THEN
    DISPLAY Rabatt.RabProc WITH FRAME FRAME-ArtInfo.
  ELSE DO:
      DISPLAY
        "" @ ArtBas.RabKod
        "" @ Rabatt.RabProc
      WITH FRAME FRAME-ArtInfo.  
      RETURN NO-APPLY.

  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-Lager
&Scoped-define SELF-NAME RS-Vis
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RS-Vis C-ArtKort
ON VALUE-CHANGED OF RS-Vis IN FRAME FRAME-Lager
DO:
  ASSIGN
    wRS-Vis = INPUT RS-Vis.
  RUN VisLager (TRUE).
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-ArtInfo
&Scoped-define SELF-NAME ArtBas.SaSong
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ArtBas.SaSong C-ArtKort
ON BACK-TAB OF ArtBas.SaSong IN FRAME FRAME-ArtInfo /* Sesong */
DO:
  APPLY "ENTRY":U TO SkoTex.ArtBas.LevFargKod IN FRAME DEFAULT-FRAME.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ArtBas.SaSong C-ArtKort
ON LEAVE OF ArtBas.SaSong IN FRAME FRAME-ArtInfo /* Sesong */
DO:
  FIND Sasong NO-LOCK WHERE
    Sasong.Sasong = int(ArtBas.Sasong:screen-value IN FRAME FRAME-ArtInfo) NO-ERROR.
  IF AVAILABLE Sasong THEN DISPLAY Sasong.SasBeskr WITH FRAME FRAME-ArtInfo.
  ELSE DO:
      DISPLAY
        "" @ ArtBas.Sasong
        "" @ Sasong.SasBeskr
      WITH FRAME FRAME-ArtInfo.  
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ArtBas.slit-id
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ArtBas.slit-id C-ArtKort
ON LEAVE OF ArtBas.slit-id IN FRAME FRAME-ArtInfo /* Slitesåle */
DO:
  FIND SlitSula NO-LOCK WHERE
    SlitSula.Slit-Id = int(ArtBas.Slit-Id:screen-value IN FRAME FRAME-ArtInfo) NO-ERROR.
  IF AVAILABLE SlitSula THEN DISPLAY SlitSula.SlitBeskr WITH FRAME FRAME-ArtInfo.
  ELSE DO:
      DISPLAY
        "" @ ArtBas.Slit-Id
        "" @ SlitSula.SlitBeskr
      WITH FRAME FRAME-ArtInfo.  
      RETURN NO-APPLY.
    RETURN NO-APPLY.
  END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME ArtBas.Storrelser
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ArtBas.Storrelser C-ArtKort
ON RETURN OF ArtBas.Storrelser IN FRAME DEFAULT-FRAME /* Størrelser */
DO:
  APPLY "tab":U TO {&SELF-NAME}.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-ArtInfo
&Scoped-define SELF-NAME ArtBas.StrTypeID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ArtBas.StrTypeID C-ArtKort
ON LEAVE OF ArtBas.StrTypeID IN FRAME FRAME-ArtInfo /* StrType */
DO:
  FIND StrType NO-LOCK WHERE
    StrType.StrTypeID = INPUT ArtBas.StrTypeId NO-ERROR.
  IF NOT AVAILABLE StrType THEN
    DO:
      DISPLAY
        "" @ ArtBas.StrType
        "" @ StrType.Beskrivelse
      WITH FRAME FRAME-ArtInfo.  
      RETURN NO-APPLY.
    END.
  ELSE 
  DISPLAY
    StrType.Beskrivelse
  WITH FRAME FRAME-ArtInfo.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-EnableLevInfo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-EnableLevInfo C-ArtKort
ON VALUE-CHANGED OF T-EnableLevInfo IN FRAME FRAME-ArtInfo /* Endre */
DO:
  IF INPUT T-EnableLevInfo = TRUE THEN
      RUN EnableLevInfo(1).
  ELSE
      RUN EnableLevInfo(2).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME TOGGLE-Annonse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOGGLE-Annonse C-ArtKort
ON TAB OF TOGGLE-Annonse IN FRAME DEFAULT-FRAME /* Annonse */
OR "RETURN":U OF TOGGLE-Annonse
DO:
  APPLY "ENTRY":U TO ArtBas.BildNr IN FRAME Default-Frame.
  RETURN NO-APPLY.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ArtBas.Utgatt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ArtBas.Utgatt C-ArtKort
ON RETURN OF ArtBas.Utgatt IN FRAME DEFAULT-FRAME /* Utgått */
DO:
    APPLY "tab":U TO {&SELF-NAME}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-ArtInfo
&Scoped-define SELF-NAME ArtBas.valkod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ArtBas.valkod C-ArtKort
ON TAB OF ArtBas.valkod IN FRAME FRAME-ArtInfo /* Valuta */
OR "RETURN":U OF ArtBas.ValKod
DO:
  FIND Valuta NO-LOCK WHERE
    Valuta.ValKod = INPUT ArtBas.ValKod NO-ERROR.
  IF AVAILABLE Valuta THEN
    DISPLAY 
      caps(INPUT ArtBas.ValKod) @ ArtBas.ValKod
      Valuta.ValKurs WITH FRAME FRAME-ArtInfo.
  ELSE DO:
      DISPLAY
        "" @ ArtBas.ValKod
        "" @ Valuta.ValKurs
      WITH FRAME FRAME-ArtInfo.  
      RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME ArtBas.Vg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ArtBas.Vg C-ArtKort
ON RETURN OF ArtBas.Vg IN FRAME DEFAULT-FRAME /* Vargr. */
OR "TAB" OF ArtBas.Vg
DO:
  FIND VarGr NO-LOCK WHERE
    VarGr.Vg = int(ArtBas.Vg:screen-value IN FRAME DEFAULT-FRAME) NO-ERROR.
  IF AVAILABLE VarGr THEN
    DO:
      DISPLAY 
          VarGr.VgBeskr @ FILL-IN-VgBeskr 
      WITH FRAME default-frame.
      IF wKopi THEN
          DISPLAY 
              VarGr.VgBeskr @ ArtBas.BongTekst 
          WITH FRAME FRAME-ArtInfo.
      IF wModus = "Ny" AND 
         ArtBas.BongTekst:screen-value IN FRAME FRAME-ArtInfo = "" THEN
        DISPLAY VarGr.VgBeskr @ ArtBas.BongTekst WITH FRAME FRAME-ArtInfo.       
      FIND HuvGr OF VarGr NO-LOCK NO-ERROR.
      IF AVAILABLE HuvGr THEN
        DISPLAY
          HuvGr.Hg
          HuvGr.HgBeskr
        WITH FRAME DEFAULT-FRAME.

      END.
  IF NOT AVAILABLE VarGr THEN
  DO:
      MESSAGE "Ukjent varegruppe!"
          VIEW-AS ALERT-BOX ERROR TITLE "Feil".
      RETURN NO-APPLY.
  END.

  IF wForslagLopNr <> "" THEN
  DO:
      RUN ForslagLopNr.  
      ASSIGN
          ArtBas.VgKat:SCREEN-VALUE = "1".
      APPLY "ENTRY":U TO ArtBas.Beskr IN FRAME default-frame.

  END.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ArtBas.VgKat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ArtBas.VgKat C-ArtKort
ON TAB OF ArtBas.VgKat IN FRAME DEFAULT-FRAME /* VgKat */
OR "RETURN":U OF ArtBas.VgKat
DO:
  FIND FIRST VgKat NO-LOCK WHERE
    VgKat.Vg = int(ArtBas.Vg:screen-value IN FRAME DEFAULT-FRAME) AND
    VgKat.VgKat = INPUT ArtBas.VgKat NO-ERROR.
  IF NOT AVAILABLE VgKat THEN
    DO:
      MESSAGE "Ukjent innkjøpskategori!"
        VIEW-AS ALERT-BOX TITLE "Melding".
      RETURN NO-APPLY.
    END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-ArtInfo
&Scoped-define SELF-NAME ArtBas.VMId
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ArtBas.VMId C-ArtKort
ON TAB OF ArtBas.VMId IN FRAME FRAME-ArtInfo /* VareMerke */
OR "RETURN":U OF ArtBas.VMId
DO:
  FIND VareMerke NO-LOCK WHERE
    Varemerke.VMId = INPUT ArtBas.VMId NO-ERROR.
  IF NOT AVAILABLE Varemerke THEN
    DO:
      DISPLAY
        "" @ ArtBas.VmId
        "" @ Varemerke.Beskrivelse
      WITH FRAME FRAME-ArtInfo.  
      RETURN NO-APPLY.
    END.
  DISPLAY
    Varemerke.Beskrivelse
  WITH FRAME FRAME-ArtInfo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BROWSE-Bestilling
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-ArtKort 


/* ***************************  Main Block  *************************** */
/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

RUN InitTx.

/* Biblotek, inn/utmelding og ON CLOSE OF THIS... */
{genlib.i 
  &NoLibCall      = "Nei"
  &WindowName     = "Artikkelkort"
  &PreIClose      = "run VisArtBas."
  &PostIClose    = "
    FIND ArtBas NO-LOCK WHERE
        recid(ArtBas) = wArtBasRecid NO-ERROR.
    IF AVAILABLE ArtBas THEN
    DO:
        IF NOT CAN-FIND(FIRST ArtPris OF ArtBas) THEN
        DO:
            MESSAGE 'Det må legges opp en kalkyle på denne artikkelen ('
                     ArtBAs.Vg ArtBAs.LopNr ').'
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RETURN NO-APPLY 'AVBRYT'.
        END.
    END.
                     if valid-handle(wHistorikk) then
                       delete procedure wHistorikk.
                     if valid-handle(wKalkyle) then
                     DO:
                       RUN SlettPrisKo IN wKalkyle.
                       delete procedure wKalkyle.
                     END.
                     IF VALID-HANDLE(h_PrisKo) THEN
                       DELETE PROCEDURE h_PrisKo.
                     for each tmpChild:
                       if valid-handle(tmpChild.wChild) then
                         delete procedure tmpChild.wChild.
                     end."                     
  &PostDisable_ui = "wModus = 'AVBRYT'." */
}

/* Forslag på løpenummer ver ny og kopiere artikkel */
{syspara.i 2 4 1 wForslagLopNr}
IF NOT CAN-DO("F,N,",wForslagLopNr) THEN
  wForslagLopNr = "".

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* HotKeySøk - DYYYYRT */
ON ALT-F4 OF C-ArtKort ANYWHERE 
  DO:
    APPLY "CHOOSE":U TO BUTTON-Ok IN FRAME DEFAULT-FRAME.
  END.
ON ALT-N OF C-ArtKort ANYWHERE 
  DO:
    APPLY "CHOOSE":U TO BUTTON-Ny IN FRAME DEFAULT-FRAME.
  END.
ON ALT-L OF C-ArtKort ANYWHERE 
  DO:
    APPLY "CHOOSE":U TO BUTTON-Lagre IN FRAME DEFAULT-FRAME.
  END.
ON ALT-K OF C-ArtKort ANYWHERE 
  DO:
    APPLY "CHOOSE":U TO BUTTON-Kopier IN FRAME DEFAULT-FRAME.
  END.
ON ALT-D OF C-ArtKort ANYWHERE 
  DO:
    APPLY "CHOOSE":U TO BUTTON-Slett IN FRAME DEFAULT-FRAME.
  END.
ON ALT-A OF C-ArtKort ANYWHERE 
  DO:
    APPLY "CHOOSE":U TO BUTTON-angre IN FRAME DEFAULT-FRAME.
  END.
ON ALT-CURSOR-UP OF C-ArtKort ANYWHERE 
  DO:
    APPLY "CHOOSE":U TO BUTTON-Prev IN FRAME DEFAULT-FRAME.
  END.
ON ALT-CURSOR-DOWN OF C-ArtKort ANYWHERE 
  DO:
    APPLY "CHOOSE":U TO BUTTON-Next IN FRAME DEFAULT-FRAME.
  END.

ON F2,ALT-S OF C-ArtKort ANYWHERE 
  DO:
    ASSIGN
        hFocus = FOCUS
        .
    IF wModus   = "NY" THEN DO:
        MESSAGE "Den nya posten måste lagras först"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
    END.

    /* Det er ikke lov å bytte artikkel hvis artikkelen ikke har en ArtPrisPost. */
    FIND ArtBas NO-LOCK WHERE
        recid(ArtBas) = wArtBasRecid NO-ERROR.
    IF AVAILABLE ArtBas THEN
    DO:
        IF NOT CAN-FIND(FIRST ArtPris OF ArtBas) THEN
        DO:
            MESSAGE "Det må legges opp en kalkyle på denne artikkelen ("
                     ArtBAs.Vg ArtBAs.LopNr ")."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RETURN "AVBRYT".
        END.
    END.

/*     run LagreArtBas (0). */

    wOldRecid = wArtBasRecid.
    RUN d-hsok.w (OUTPUT wArtikkelNr,"NEI" + CHR(1) + "1"). /* JA = sök mot vpistrekkode */
    IF wArtikkelNr = ? THEN
      DO:
        wArtBasRecid = wOldRecid.
        IF VALID-HANDLE(hFocus) THEN
        DO:
            APPLY "ENTRY" TO hFocus.
            ASSIGN
                hFocus:CURSOR-OFFSET = 1 NO-ERROR.
        END.
        ELSE DO:
            APPLY "ENTRY" TO ArtBas.Beskr IN FRAME DEFAULT-FRAME.
            ASSIGN
                hFocus:CURSOR-OFFSET = 1 NO-ERROR.
        END.
        RETURN NO-APPLY.
      END.

    FIND ArtBas NO-LOCK WHERE
      ArtBAs.ArtikkelNr = wArtikkelNr NO-ERROR.
    wArtBasrecid = RECID(ArtBas).
    FIND HuvGr OF ArtBas NO-LOCK.
    IF NOT AVAILABLE HuvGr THEN RETURN NO-APPLY. 
    RUN ByttFrame. /* For † enable knapper og felt korrekt */
    RUN RensFrame.
    RUN VisArtBas.
    RUN VisLager(TRUE).    
    IF wAktivFlip = 4 THEN
      DO:
        APPLY "entry":U TO BROWSE BROWSE-Bestilling.
        RUN OrdreToolbar.
      END.
  END.

/* Behandler modus */
IF num-entries(wModus,";") > 1 THEN
  ASSIGN
    wBildNr = int(entry(2,wModus,";"))
    wModus  = entry(1,wModus,";").
ELSE
  ASSIGN
    wBildNr = ?.

ON "CTRL-TAB":U ANYWHERE
  DO:
    ASSIGN
      wAktivFlip = wAktivFLip + 1.
    IF wAktivFlip > 5 THEN
      wAktivFlip = 1.
    RUN ByttFrame.
  END.

ASSIGN
  wNyVg                  = ?
  wRS-Vis                = 1. /* Vis Lager */

/* Flagger at systemet kjøres på en LapTop (Innkjøpssystem) */
IF valid-handle(wLibHandle) THEN
  RUN SjekkLapTop IN wLibHandle (OUTPUT wLapTop).

/* Setter sentrallager butikk */
{syspara.i 5 1 1 wCl INT}

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
 IF SOURCE-PROCEDURE:NAME BEGINS "w-bartsokJFSpec" THEN DO:
     hParentHandle = SOURCE-PROCEDURE.
     SUBSCRIBE TO "ByttArtikkel" IN hParentHandle.
 END.

  RUN enable_UI.
  {lng.i} /* Oversettelse */
  APPLY "ENTRY" TO CB-Sort.
  IF CAN-FIND(ArtBas WHERE recid(ArtBas) = wArtBasRecid) THEN
    DO:
      RUN VisArtBas.
      {&OPEN-QUERY-BROWSE-Statistikk}
    END.
    
  RUN ByttFrame. /* Legger opp f›rste fane. */

  ASSIGN chTab1Side = chCtrlFrame:TabStrip. /* COM-Handl 1. side */
  
  IF wModus = "NY" THEN 
    DO:
      /*-- assign ArtBas.StrTypeId:sensitive = true. --*/
      APPLY "CHOOSE":U TO BUTTON-Ny.
    END.
  /*---  
  else
    assign ArtBas.StrTypeId:sensitive = false.
  ---*/

  ASSIGN
    C-ArtKort:hidden = FALSE.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
    
  IF wModus <> "NY" THEN
    FIND ArtBas NO-LOCK WHERE 
      recid(ArtBas) = wArtBasRecid NO-ERROR.
      
  /* Retur verdi */  
  IF AVAILABLE ArtBAs AND wModus <> "NY" THEN
    RETURN string(recid(ArtBas)).
  ELSE 
    RETURN "AVBRYT".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ArtBasBildNr C-ArtKort 
PROCEDURE ArtBasBildNr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME DEFAULT-FRAME:
  FIND Bilderegister NO-LOCK WHERE
      Bilderegister.BildNr = int(string(ArtBas.BildNr:screen-value IN FRAME DEFAULT-FRAME)) NO-ERROR.
  IF AVAILABLE Bilderegister THEN
    DO:
      DISPLAY
        Bilderegister.BildNr @ ArtBas.BildNr 
        BildeRegister.Tekst
      WITH FRAME DEFAULT-FRAME.
      RUN VisBilde (1).
   END.
   ELSE DO:
       DISPLAY
         "" @ ArtBas.BildNr 
         "" @ BildeRegister.Tekst
       WITH FRAME DEFAULT-FRAME.
      RETURN "AVBRYT". 
   END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BlankLager C-ArtKort 
PROCEDURE BlankLager :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  FOR EACH tmpLager:
    DELETE tmpLager.
  END.
  {&OPEN-QUERY-BROWSE-LAger}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BrowseLabel C-ArtKort 
PROCEDURE BrowseLabel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER wStrListe AS CHAR NO-UNDO.
  DEF VAR   wLoop               AS INT  NO-UNDO.
  DEF VAR   wStorl              AS CHAR NO-UNDO.
  
  DO wLoop = 1 TO 48:
    IF num-entries(wStrListe,";") >= wLoop 
      THEN wStorl = entry(wLoop,wStrListe,";") + "  ".
    ELSE wStorl = "".
    
    ASSIGN
      wStorl = fill(" ",6 - length(wStorl)) + wStorl.
  
    CASE wLoop:
      WHEN  1 THEN tmpLager.Antall[ 1]:label IN BROWSE BROWSE-Lager = wStorl.
      WHEN  2 THEN tmpLager.Antall[ 2]:label IN BROWSE BROWSE-Lager = wStorl.
      WHEN  3 THEN tmpLager.Antall[ 3]:label IN BROWSE BROWSE-Lager = wStorl.
      WHEN  4 THEN tmpLager.Antall[ 4]:label IN BROWSE BROWSE-Lager = wStorl.
      WHEN  5 THEN tmpLager.Antall[ 5]:label IN BROWSE BROWSE-Lager = wStorl.
      WHEN  6 THEN tmpLager.Antall[ 6]:label IN BROWSE BROWSE-Lager = wStorl.
      WHEN  7 THEN tmpLager.Antall[ 7]:label IN BROWSE BROWSE-Lager = wStorl.
      WHEN  8 THEN tmpLager.Antall[ 8]:label IN BROWSE BROWSE-Lager = wStorl.
      WHEN  9 THEN tmpLager.Antall[ 9]:label IN BROWSE BROWSE-Lager = wStorl.
      WHEN 10 THEN tmpLager.Antall[10]:label IN BROWSE BROWSE-Lager = wStorl.
      WHEN 11 THEN tmpLager.Antall[11]:label IN BROWSE BROWSE-Lager = wStorl.
      WHEN 12 THEN tmpLager.Antall[12]:label IN BROWSE BROWSE-Lager = wStorl.
      WHEN 13 THEN tmpLager.Antall[13]:label IN BROWSE BROWSE-Lager = wStorl.
      WHEN 14 THEN tmpLager.Antall[14]:label IN BROWSE BROWSE-Lager = wStorl.
      WHEN 15 THEN tmpLager.Antall[15]:label IN BROWSE BROWSE-Lager = wStorl.
      WHEN 16 THEN tmpLager.Antall[16]:label IN BROWSE BROWSE-Lager = wStorl.
      WHEN 17 THEN tmpLager.Antall[17]:label IN BROWSE BROWSE-Lager = wStorl.
      WHEN 18 THEN tmpLager.Antall[18]:label IN BROWSE BROWSE-Lager = wStorl.
      WHEN 19 THEN tmpLager.Antall[19]:label IN BROWSE BROWSE-Lager = wStorl.
      WHEN 20 THEN tmpLager.Antall[20]:label IN BROWSE BROWSE-Lager = wStorl.
      WHEN 21 THEN tmpLager.Antall[21]:label IN BROWSE BROWSE-Lager = wStorl.
      WHEN 22 THEN tmpLager.Antall[22]:label IN BROWSE BROWSE-Lager = wStorl.
      WHEN 23 THEN tmpLager.Antall[23]:label IN BROWSE BROWSE-Lager = wStorl.
      WHEN 24 THEN tmpLager.Antall[24]:label IN BROWSE BROWSE-Lager = wStorl.
      WHEN 25 THEN tmpLager.Antall[25]:label IN BROWSE BROWSE-Lager = wStorl.
      WHEN 26 THEN tmpLager.Antall[26]:label IN BROWSE BROWSE-Lager = wStorl.
      WHEN 27 THEN tmpLager.Antall[27]:label IN BROWSE BROWSE-Lager = wStorl.
      WHEN 28 THEN tmpLager.Antall[28]:label IN BROWSE BROWSE-Lager = wStorl.
      WHEN 29 THEN tmpLager.Antall[29]:label IN BROWSE BROWSE-Lager = wStorl.
      WHEN 30 THEN tmpLager.Antall[30]:label IN BROWSE BROWSE-Lager = wStorl.
      WHEN 31 THEN tmpLager.Antall[31]:label IN BROWSE BROWSE-Lager = wStorl.
      WHEN 32 THEN tmpLager.Antall[32]:label IN BROWSE BROWSE-Lager = wStorl.
      WHEN 33 THEN tmpLager.Antall[33]:label IN BROWSE BROWSE-Lager = wStorl.
      WHEN 34 THEN tmpLager.Antall[34]:label IN BROWSE BROWSE-Lager = wStorl.
      WHEN 35 THEN tmpLager.Antall[35]:label IN BROWSE BROWSE-Lager = wStorl.
      WHEN 36 THEN tmpLager.Antall[36]:label IN BROWSE BROWSE-Lager = wStorl.
      WHEN 37 THEN tmpLager.Antall[37]:label IN BROWSE BROWSE-Lager = wStorl.
      WHEN 38 THEN tmpLager.Antall[38]:label IN BROWSE BROWSE-Lager = wStorl.
      WHEN 39 THEN tmpLager.Antall[39]:label IN BROWSE BROWSE-Lager = wStorl.
      WHEN 40 THEN tmpLager.Antall[40]:label IN BROWSE BROWSE-Lager = wStorl.
      WHEN 41 THEN tmpLager.Antall[41]:label IN BROWSE BROWSE-Lager = wStorl.
      WHEN 42 THEN tmpLager.Antall[42]:label IN BROWSE BROWSE-Lager = wStorl.
      WHEN 43 THEN tmpLager.Antall[43]:label IN BROWSE BROWSE-Lager = wStorl.
      WHEN 44 THEN tmpLager.Antall[44]:label IN BROWSE BROWSE-Lager = wStorl.
      WHEN 45 THEN tmpLager.Antall[45]:label IN BROWSE BROWSE-Lager = wStorl.
      WHEN 46 THEN tmpLager.Antall[46]:label IN BROWSE BROWSE-Lager = wStorl.
      WHEN 47 THEN tmpLager.Antall[47]:label IN BROWSE BROWSE-Lager = wStorl.
      WHEN 48 THEN tmpLager.Antall[48]:label IN BROWSE BROWSE-Lager = wStorl.
    END CASE.
  END. 
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BUTTON-Next C-ArtKort 
PROCEDURE BUTTON-Next :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR wSort AS INT NO-UNDO.
  
  ASSIGN
    wSort = int(entry(1,CB-Sort:screen-value IN FRAME DEFAULT-FRAME,":")).

/*   run LagreArtBas (0).            */
/*   if return-value = "AVBRYT" then */
/*     return no-apply.              */
/*   else                            */
  DO:
    ASSIGN wModus = "ENDRE".
    
    CASE wSort:
      WHEN 1 THEN
        DO:
          FIND NEXT ArtBas NO-LOCK USE-INDEX VgLopNr NO-ERROR.    
          IF NOT AVAILABLE ArtBas THEN
             FIND LAST ArtBas NO-LOCK USE-INDEX VgLopNr  NO-ERROR.
        END.
      WHEN 2 THEN
        DO:
          FIND NEXT ArtBas NO-LOCK USE-INDEX VgBeskr NO-ERROR.    
          IF NOT AVAILABLE ArtBas THEN
             FIND LAST ArtBas NO-LOCK USE-INDEX VgBeskr  NO-ERROR.
        END.
      WHEN 3 THEN
        DO:
          FIND NEXT ArtBas NO-LOCK USE-INDEX VgLevKod NO-ERROR.    
          IF NOT AVAILABLE ArtBas THEN
             FIND LAST ArtBas NO-LOCK USE-INDEX VgLevKod  NO-ERROR.
        END.
      WHEN 4 THEN
        DO:
          FIND NEXT ArtBas NO-LOCK USE-INDEX LevNrLevKod NO-ERROR.    
          IF NOT AVAILABLE ArtBas THEN
             FIND LAST ArtBas NO-LOCK USE-INDEX LevNrLevKod  NO-ERROR.
        END.
      WHEN 5 THEN
        DO:
          FIND NEXT ArtBas NO-LOCK USE-INDEX ArtikkelNr NO-ERROR.    
          IF NOT AVAILABLE ArtBas THEN
             FIND LAST ArtBas NO-LOCK USE-INDEX ArtikkelNr  NO-ERROR.
        END.
    END CASE.   
      
    IF AVAILABLE ArtBas THEN
      DO:
        ASSIGN
          wArtBasRecid = recid(ArtBas).
        RUN ByttFrame. /* For † enable knapper og felt korrekt */ 
        RUN RensFrame. /* Tar bort gammel d.... */
        RUN VisArtBas.
        IF wAktivFlip = 4 THEN
          DO:
            APPLY "entry":U TO BROWSE BROWSE-Bestilling.
            RUN OrdreToolbar.
          END.
        IF wAktivFlip = 5 THEN
        DO:
            IF valid-handle(wHistorikk) THEN
              RUN SetEntry IN wHistorikk.
        END.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BUTTON-Ny C-ArtKort 
PROCEDURE BUTTON-Ny :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF wModus = "ENDRE" THEN
    DO:
      RUN LagreArtBas (1).
      IF RETURN-VALUE = "AVBRYT" THEN
        RETURN NO-APPLY.
    END.
  /* Bytter til f›rste fane. */
  ASSIGN 
    ArtBas.LevNr:SENSITIVE IN FRAME default-frame = TRUE
    BUTTON-SokLev:SENSITIVE IN FRAME default-frame = TRUE
    wAktivFlip = 1 
    TOGGLE-Annonse = FALSE
    wModus   = "NY".

  chTab1Side = chTabs:Item (wAktivFlip BY-VARIANT-POINTER).
  chTabStrip:SelectedItem = chTab1Side.
  RUN ByttFrame.
  RUN RensFrame.
  RUN BlankLager.
  RUN Toolbar.
  APPLY "ENTRY":U TO ArtBas.Vg IN FRAME DEFAULT-FRAME.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BUTTON-Prev C-ArtKort 
PROCEDURE BUTTON-Prev :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR wSort AS INT NO-UNDO.
  
  ASSIGN
    wSort = int(entry(1,CB-Sort:screen-value IN FRAME DEFAULT-FRAME,":")).


/*   run LagreArtBas (0).            */
/*   if return-value = "AVBRYT" then */
/*     return no-apply.              */
/*   else                            */
  DO:
    ASSIGN wModus = "ENDRE".
  
    CASE wSort:
      WHEN 1 THEN
        DO:
          FIND PREV ArtBas NO-LOCK USE-INDEX VgLopNr NO-ERROR.    
          IF NOT AVAILABLE ArtBas THEN
             FIND FIRST ArtBas NO-LOCK USE-INDEX VgLopNr  NO-ERROR.
        END.
      WHEN 2 THEN
        DO:
          FIND PREV ArtBas NO-LOCK USE-INDEX VgBeskr NO-ERROR.    
          IF NOT AVAILABLE ArtBas THEN
             FIND FIRST ArtBas NO-LOCK USE-INDEX VgBeskr  NO-ERROR.
        END.
      WHEN 3 THEN
        DO:
          FIND PREV ArtBas NO-LOCK USE-INDEX VgLevKod NO-ERROR.    
          IF NOT AVAILABLE ArtBas THEN
             FIND FIRST ArtBas NO-LOCK USE-INDEX VgLevKod  NO-ERROR.
        END.
      WHEN 4 THEN
        DO:
          FIND PREV ArtBas NO-LOCK USE-INDEX LevNrLevKod NO-ERROR.    
          IF NOT AVAILABLE ArtBas THEN
             FIND FIRST ArtBas NO-LOCK USE-INDEX LevNrLevKod  NO-ERROR.
        END.
      WHEN 5 THEN
        DO:
          FIND  PREV ArtBas NO-LOCK USE-INDEX ArtikkelNr NO-ERROR.    
          IF NOT AVAILABLE ArtBas THEN
             FIND FIRST ArtBas NO-LOCK USE-INDEX ArtikkelNr  NO-ERROR.
        END.
    END CASE.

    IF AVAILABLE ArtBas THEN
      DO:
        ASSIGN
          wArtBasRecid = recid(ArtBas).
        RUN ByttFrame. /* For † enable knapper og felt korrekt */
        RUN RensFrame. /* Tar bort gammel d.... */
        RUN VisArtBas.
        IF wAktivFlip = 4 THEN
          DO:
            APPLY "entry":U TO BROWSE BROWSE-Bestilling.
            RUN OrdreToolbar.
          END.
        IF wAktivFlip = 5 THEN
        DO:
            IF valid-handle(wHistorikk) THEN
              RUN SetEntry IN wHistorikk.
        END.
      END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BUTTON-Slett C-ArtKort 
PROCEDURE BUTTON-Slett :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR wOk AS LOG FORMAT "Ja/Nei" NO-UNDO.
  DEF VAR wNesteRecid AS RECID NO-UNDO.
  
  FIND bArtBas NO-LOCK WHERE
    recid(bArtBas) = recid(ArtBas) NO-ERROR.
  FIND NEXT bArtBas NO-LOCK NO-ERROR.
  IF NOT AVAILABLE bArtBas THEN
    FIND FIRST bArtBas.
  IF AVAILABLE bArtBas THEN
    ASSIGN
      wNesteRecid = recid(bArtBas).

  /*---
  find first StLinje no-lock where
    StLinje.StType = "ARTIKKEL" and
    StLinje.DataObjekt = string(ArtBAs.ArtikkelNr,"9999999999999") no-error.
  if available StLinje then
    do:
      message "Det finnes statistikkpå artikkeln. Den kan ikke slettes!"
              view-as alert-box message title "Melding".
      return no-apply.
    end.
  ---*/

  ASSIGN wOk = FALSE.
  MESSAGE Tx("Skal artikkelen slettes?",104) VIEW-AS ALERT-BOX 
    QUESTION BUTTONS YES-NO
    TITLE Tx("Bekreftelse",105)
    UPDATE wOk.
  IF wOk = FALSE THEN
    RETURN NO-APPLY "AVBRYT".  
  ELSE DO:
    RUN SlettArtBas.
    IF RETURN-VALUE = "AVBRYT" THEN
      RETURN NO-APPLY.
    ASSIGN 
      wArtBasRecid   = wNesteRecid
      wAktivFlip     = 1 
      wModus         = "ENDRE"
      TOGGLE-Annonse = FALSE.
  
    RUN ByttFrame.
    RUN RensFrame.
    RUN BlankLager.
    RUN VisArtBas.
    APPLY "CHOOSE":U TO ArtBas.VgKat IN FRAME DEFAULT-FRAME.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BytLevBestHode C-ArtKort 
PROCEDURE BytLevBestHode :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER wLevNr AS INT NO-UNDO.
  
  DEF VAR wStatus AS CHAR INITIAL "AVBRYT".
  
  DEF BUFFER bufBestHode FOR BestHode.
  
  DO FOR bufBestHode TRANSACTION:
    FOR EACH bufBestHode EXCLUSIVE-LOCK WHERE
      bufBestHode.LevNr      = ArtBas.LevNr AND
      bufBestHode.ArtikkelNr = ArtBas.ArtikkelNr AND
      bufBestHode.BestStat  >= 1 AND
      bufBestHode.BestStat  <= 2:
      
      ASSIGN
        wStatus           = "OK"
        bufBestHode.LevNr = wLevNr.
  
    END.
    IF AVAILABLE bufBestHode THEN RELEASE bufBestHode.
  END. /* TRANSACTION */
  
  RETURN wStatus.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByttArtikkel C-ArtKort 
PROCEDURE ByttArtikkel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER dArtikkelNr LIKE ArtBas.ArtikkelNr NO-UNDO.

  FIND ArtBas NO-LOCK WHERE
    ArtBas.ArtikkelNr = dArtikkelNr NO-ERROR.
  IF NOT AVAILABLE ArtBas THEN DO:
      FIND ArtBas WHERE RECID(ArtBas) = wArtBasRecid NO-LOCK NO-ERROR.
      RETURN "AVBRYT".
  END.  
  
  ASSIGN wArtBasRecid = RECID(ArtBas).
  
/*   DEF INPUT PARAMETER piArtBasRecid AS RECID NO-UNDO. */
/*                                                       */
/*   ASSIGN                                              */
/*       wArtBasRecid = piArtBasRecid                    */
/*       .                                               */
/*   FIND ArtBas NO-LOCK WHERE                */
/*     recid(ArtBas) = wArtBasRecid NO-ERROR. */
/*   IF NOT AVAILABLE ArtBas THEN             */
/*       RETURN "AVBRYT".                     */

  /* Lagrer eventuelle endringer f›r s›k startes. */
/*   RUN LagreArtBas (1).            */
/*   IF RETURN-VALUE = "AVBRYT" THEN */
/*     RETURN NO-APPLY.              */
  
  RUN RensFrame.
  RUN VisArtBas.
  RUN VisLager(TRUE).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByttFrame C-ArtKort 
PROCEDURE ByttFrame :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR wFrameHAndle AS HANDLE NO-UNDO.
   DEF VAR wFirst-Child AS HANDLE NO-UNDO.


   IF wModus = "NY" AND wAktivFlip <> 1 THEN
     DO:
       wAktivFlip = 1.
       MESSAGE Tx("Artikkelinformasjonen må lagres først!",106)
         VIEW-AS ALERT-BOX MESSAGE TITLE Tx("Melding",107).
     END.
   
   /* Info frame som alltid ligger oppe. */
   IF NOT can-do("2,5",string(wAktivFlip)) THEN
     IF FRAME DEFAULT-FRAME:MOVE-TO-TOP() THEN.

   /* Bytter FLik */
   chTab1Side = chTabs:Item (wAktivFlip BY-VARIANT-POINTER).
   chTabStrip:SelectedItem = chTab1Side.

   /* Bytter frame.                                                  */
   /* NB: Dette gjøres også på OCX'en når man klikker på aktiv flik. */
   /*     Dvs det må vedlikeholdes kode også der.                    */
   CASE wAktivFlip:
      WHEN 1 THEN 
        DO WITH FRAME DEFAULT-FRAME:
          IF FRAME FRAME-ArtInfo:MOVE-TO-TOP()  THEN.
          IF ArtBas.Vg:Sensitive = TRUE 
            THEN APPLY "entry" TO ArtBas.Vg IN FRAME DEFAULT-FRAME.
          ELSE APPLY "entry" TO ArtBas.Beskr IN FRAME DEFAULT-FRAME.
        END.        
      WHEN 2 THEN 
        DO:
          FRAME FRAME-Dummy:MOVE-TO-TOP().
/*           IF VALID-HANDLE(wKalkyle) THEN                                                                                                  */
/*               RUN MoveToTopp IN wKalkyle.                                                                                                 */
/*                                                                                                                                           */
/*           if available ArtBas then                                                                                                        */
/*             do:                                                                                                                           */
/*               if valid-handle(wKalkyle) then                                                                                              */
/*                 run ByttObjekt in wKalkyle (ArtBas.ArtikkelNr).                                                                           */
/*               else                                                                                                                        */
/*                 run w-kalkyle.w persistent set wKalkyle (ArtBas.ArtikkelNr,THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE,h_PrisKo,wModus). */
/*             end.                                                                                                                          */
        END.
      WHEN 3 THEN 
        HISTORIKK:
        DO:
          IF FRAME FRAME-Lager:MOVE-TO-TOP() THEN.
          RUN VisLager (NO).
          APPLY "entry":U TO BROWSE-Lager.
        END. /* HISTORIKK */
      WHEN 4 THEN 
        DO:
          IF FRAME FRAME-Ordre:MOVE-TO-TOP() THEN.
          APPLY "entry":u TO BROWSE BROWSE-Bestilling.
          RUN OrdreToolbar.
        END.
      WHEN 5 THEN 
        DO:
          IF VALID-HANDLE(wHistorikk) THEN
              RUN MoveToTopp IN wHistorikk.
          
          IF AVAILABLE ArtBas THEN
            DO:
              IF valid-handle(wHistorikk) THEN
                RUN ByttObjekt IN wHistorikk (string(ArtBas.ArtikkelNr,"9999999999999")).  
              ELSE         
                RUN w-stlinjeJFSpec.w PERSISTENT SET wHistorikk (wArtBasRecid,string(ArtBas.ArtikkelNr,"9999999999999"),"ARTIKKEL","MANED",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE).  
            END.
        END.
   END.   
      
   /* Aktiverer og disabler felter */
   ASSIGN 
     wAktivFrame = wAktivFlip
     /* Felt som kun er †pne p† f›rste side. */
     ArtBas.Vg:sensitive         = (wAktivFlip = 99) AND wModus =  "Ny"
     ArtBas.LopNr:sensitive      = (wAktivFlip = 99) AND wModus =  "NY" AND wLapTop = FALSE
     BUTTON-SokVg:sensitive      = (wAktivFlip = 99) AND wModus =  "NY" 
     BUTTON-SokArt:sensitive     = /*(wAktivFlip = 99) and*/ /* wModus <> "NY" */ FALSE
     /*
     BUTTON-SokStrType:sensitive = (wAktivFlip = 99) /* and wModus =  "NY" */
     ArtBas.StrTypeId:sensitive  = (wAktivFlip = 99) /* and wModus =  "NY" */
     */
     ArtBas.Beskr:sensitive      = (wAktivFlip = 99) 
     ArtBas.LevKod:sensitive     = (wAktivFlip = 99)
     ArtBas.LevFargKod:sensitive = (wAktivFlip = 99)     
     ArtBas.VgKat:sensitive      = (wAktivFlip = 99) 
     ArtBas.Utgatt:sensitive     = (wAktivFlip = 99)
     ArtBas.lager:sensitive      = (wAktivFlip = 99)
     ArtBas.Storrelser:sensitive = (wAktivFlip = 99)         
     ArtBas.BildNr:sensitive     = (wAktivFlip = 99)         
     BUTTON-SokBilde:sensitive   = (wAktivFlip = 99).      

    /* Sjekker om Leverandør kan endres på artikkelen. */
    IF CAN-FIND(FIRST BestHode WHERE 
          Besthode.LevNr      = ArtBas.LevNr AND
          BestHode.ArtikkelNr = ArtBas.ArtikkelNr AND
          BestHode.BestStat > 2 AND
          BestHode.BestStat < 6) THEN
      DO:
        ASSIGN
          ArtBas.LevNr:sensitive  = FALSE
          BUTTON-SokLev:sensitive = FALSE.
      END.
    ELSE DO:
       ASSIGN
         ArtBas.LevNr:sensitive  = (wAktivFlip = 99)
         BUTTON-SokLev:sensitive = (wAktivFlip = 99).
    END.
         
   /*--- Er nå alltid tillatt
   /* Sjekker om størrelsestype kan endres på artikkelen */
   if (can-find(first ArtLag where ArtLag.Vg = ArtBas.Vg and
                                   ArtLag.LopNr = ArtBas.LopNr)) and
       wModus <> "NY" then
       assign
         ArtBas.StrTypeId:sensitive  = false
         BUTTON-SokStrType:sensitive = false.
   else
       assign
         ArtBas.StrTypeId:sensitive  = (wAktivFlip = 1)
         BUTTON-SokStrType:sensitive = (wAktivFlip = 1).
   ---*/
   
   /* Sjekker om størrelsesflagg kan endres på artikkelen. */
   /* Overstyrer det som har skjedd over.                  */
   /* På Flik 1 er flaggene i utgangspunktet akriverte.    */
   IF wAktivFlip = 1 THEN
   DO:
     IF wModus <> "NY" THEN
       DO:
         IF CAN-FIND(FIRST ArtLag WHERE
                           ArtLag.artikkelnr = ArtBas.artikkelnr) THEN
           ASSIGN
             ArtBas.Storrelser:sensitive = FALSE.
                     
         /* Sjekker om Lagerflagg kan endres på artikkelen. */
         IF CAN-FIND(FIRST Lager WHERE
                           Lager.ArtikkelNr = ArtBas.ArtikkelNr) THEN
           ASSIGN
             ArtBas.Lager:sensitive = FALSE.
       END.
   END.  
   ASSIGN
     ArtBas.LevNr:sensitive  = FALSE
     BUTTON-SokLev:sensitive = FALSE.
   ASSIGN
     ArtBas.Storrelser:sensitive = FALSE.
   ASSIGN
     ArtBas.Lager:sensitive = FALSE.
     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByttVg C-ArtKort 
PROCEDURE ByttVg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR wVgKat AS INT NO-UNDO.

  IF NOT AVAILABLE ArtBas THEN
      RETURN NO-APPLY.

  RUN d-byttvg (ArtBas.ArtikkelNr).
  IF RETURN-VALUE = "AVBRYT" THEN
      RETURN NO-APPLY.

  FIND ArtBas NO-LOCK WHERE
      recid(ArtBas) = wArtBasRecid NO-ERROR.
  FIND VarGr NO-LOCK WHERE
    VarGr.Vg = ArtBas.Vg NO-ERROR.
  IF AVAILABLE VarGr THEN
    FIND FIRST VgKat NO-LOCK WHERE
      VgKat.Vg    = ArtBas.Vg    AND
      VgKat.VgKat = ArtBas.VgKat NO-ERROR.
  
  DISPLAY
    ArtBas.Vg        WHEN AVAILABLE ArtBas
    VarGr.VgBeskr @  FILL-IN-VgBeskr
  WITH FRAME Default-Frame.
  DISPLAY
      ArtBas.BongTekst WHEN AVAILABLE ArtBas
  WITH FRAME FRAME-ArtInfo.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load C-ArtKort  _CONTROL-LOAD
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

OCXFile = SEARCH( "w-vartkorJFSpec.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chCtrlFrame = CtrlFrame:COM-HANDLE
    UIB_S = chCtrlFrame:LoadControls( OCXFile, "CtrlFrame":U)
    CtrlFrame:NAME = "CtrlFrame":U
    chIMAGE-Sko = IMAGE-Sko:COM-HANDLE
    UIB_S = chIMAGE-Sko:LoadControls( OCXFile, "IMAGE-Sko":U)
    IMAGE-Sko:NAME = "IMAGE-Sko":U
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "w-vartkorJFSpec.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-ArtKort  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-ArtKort)
  THEN DELETE WIDGET C-ArtKort.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EnableLevInfo C-ArtKort 
PROCEDURE EnableLevInfo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER iMode AS INT NO-UNDO.

  DO WITH FRAME FRAME-ArtInfo:
    IF iMode = 1 THEN
      ASSIGN
        FI-1:SENSITIVE = TRUE
        FI-2:SENSITIVE = TRUE
        FI-3:SENSITIVE = TRUE
        FI-4:SENSITIVE = TRUE
        FI-5:SENSITIVE = TRUE
        FI-6:SENSITIVE = TRUE
        FI-7:SENSITIVE = TRUE
        FI-8:SENSITIVE = TRUE
        T-1:SENSITIVE  = TRUE
        T-2:SENSITIVE  = TRUE
        T-3:SENSITIVE  = TRUE
        T-4:SENSITIVE  = TRUE
        T-5:SENSITIVE  = TRUE
        T-6:SENSITIVE  = TRUE
        T-7:SENSITIVE  = TRUE
        T-8:SENSITIVE  = TRUE
        T-9:SENSITIVE  = TRUE
        T-10:SENSITIVE IN FRAME default-frame = TRUE
        .
    ELSE ASSIGN
        FI-1:SENSITIVE = FALSE
        FI-2:SENSITIVE = FALSE
        FI-3:SENSITIVE = FALSE
        FI-4:SENSITIVE = FALSE
        FI-5:SENSITIVE = FALSE
        FI-6:SENSITIVE = FALSE
        FI-7:SENSITIVE = FALSE
        FI-8:SENSITIVE = FALSE
        T-1:SENSITIVE  = FALSE
        T-2:SENSITIVE  = FALSE
        T-3:SENSITIVE  = FALSE
        T-4:SENSITIVE  = FALSE
        T-5:SENSITIVE  = FALSE
        T-6:SENSITIVE  = FALSE
        T-7:SENSITIVE  = FALSE
        T-8:SENSITIVE  = FALSE
        T-9:SENSITIVE  = FALSE
        T-10:SENSITIVE IN FRAME default-frame = FALSE
        .
    APPLY "entry" TO FI-1.
  END. /* Frame Scoope */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-ArtKort  _DEFAULT-ENABLE
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
  DISPLAY FI-PaTilbud CB-Sort TOGGLE-Annonse FILL-IN-VgBeskr FILL-IN-LevNamn 
          T-LapTop T-10 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-ArtKort.
  IF AVAILABLE ArtBas THEN 
    DISPLAY ArtBas.ArtikkelNr ArtBas.Vg ArtBas.VgKat ArtBas.LopNr ArtBas.Beskr 
          ArtBas.lager ArtBas.Storrelser ArtBas.Utgatt ArtBas.BildNr 
          ArtBas.LevNr ArtBas.LevKod ArtBas.LevFargKod ArtBas.inn_dato 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-ArtKort.
  IF AVAILABLE Bilderegister THEN 
    DISPLAY Bilderegister.Tekst 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-ArtKort.
  IF AVAILABLE HuvGr THEN 
    DISPLAY HuvGr.Hg HuvGr.HgBeskr 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-ArtKort.
  ENABLE BUTTON-Next BUTTON-Prev CB-Sort BUTTON-Trans Btn_Help BUTTON-Ok 
         RECT-27 RECT-28 RECT-34 RECT-35 RECT-5 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-ArtKort.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  DISPLAY FI-Tekst1 T-EnableLevInfo FILL-IN-19 T-9 FI-1 FI-2 FI-3 FI-4 FI-5 FI-6 
          FI-7 FI-8 FILL-IN-SalgsPris FILL-IN-InnPris FILL-IN-TilbPris 
          FILL-IN-UtsFra FILL-IN-UtsTil FILL-IN-EndretInfo T-8 T-7 T-6 T-5 T-4 
          T-3 T-2 T-1 FI-Tekst-2 FI-Tekst-3 FI-Tekst-4 FI-Tekst-5 
      WITH FRAME FRAME-ArtInfo IN WINDOW C-ArtKort.
  IF AVAILABLE Anv-Kod THEN 
    DISPLAY Anv-Kod.AnvBeskr 
      WITH FRAME FRAME-ArtInfo IN WINDOW C-ArtKort.
  IF AVAILABLE ArtBas THEN 
    DISPLAY ArtBas.StrTypeID ArtBas.SaSong ArtBas.Farg ArtBas.MatKod ArtBas.Klack 
          ArtBas.inner-id ArtBas.ov-id ArtBas.slit-id ArtBas.last-id 
          ArtBas.anv-id ArtBas.BongTekst ArtBas.ProvKod ArtBas.RabKod 
          ArtBas.valkod ArtBas.VMId ArtBas.ProdNr ArtBas.LevDato1 
          ArtBas.LevDato2 ArtBas.Notat 
      WITH FRAME FRAME-ArtInfo IN WINDOW C-ArtKort.
  IF AVAILABLE Farg THEN 
    DISPLAY Farg.FarBeskr 
      WITH FRAME FRAME-ArtInfo IN WINDOW C-ArtKort.
  IF AVAILABLE InnerSula THEN 
    DISPLAY InnerSula.InnerBeskr 
      WITH FRAME FRAME-ArtInfo IN WINDOW C-ArtKort.
  IF AVAILABLE Last-Sko THEN 
    DISPLAY Last-Sko.LastBeskr 
      WITH FRAME FRAME-ArtInfo IN WINDOW C-ArtKort.
  IF AVAILABLE Material THEN 
    DISPLAY Material.MatBeskr 
      WITH FRAME FRAME-ArtInfo IN WINDOW C-ArtKort.
  IF AVAILABLE Ovandel THEN 
    DISPLAY Ovandel.OvBeskr 
      WITH FRAME FRAME-ArtInfo IN WINDOW C-ArtKort.
  IF AVAILABLE Produsent THEN 
    DISPLAY Produsent.Beskrivelse 
      WITH FRAME FRAME-ArtInfo IN WINDOW C-ArtKort.
  IF AVAILABLE Prov THEN 
    DISPLAY Prov.ProvProc 
      WITH FRAME FRAME-ArtInfo IN WINDOW C-ArtKort.
  IF AVAILABLE Rabatt THEN 
    DISPLAY Rabatt.RabProc 
      WITH FRAME FRAME-ArtInfo IN WINDOW C-ArtKort.
  IF AVAILABLE SaSong THEN 
    DISPLAY SaSong.SasBeskr 
      WITH FRAME FRAME-ArtInfo IN WINDOW C-ArtKort.
  IF AVAILABLE SlitSula THEN 
    DISPLAY SlitSula.SlitBeskr 
      WITH FRAME FRAME-ArtInfo IN WINDOW C-ArtKort.
  IF AVAILABLE StrType THEN 
    DISPLAY StrType.Beskrivelse 
      WITH FRAME FRAME-ArtInfo IN WINDOW C-ArtKort.
  IF AVAILABLE Valuta THEN 
    DISPLAY Valuta.ValKurs 
      WITH FRAME FRAME-ArtInfo IN WINDOW C-ArtKort.
  IF AVAILABLE Varemerke THEN 
    DISPLAY Varemerke.Beskrivelse 
      WITH FRAME FRAME-ArtInfo IN WINDOW C-ArtKort.
  ENABLE BUTTON-SokDato-2 T-EnableLevInfo BUTTON-SokProv BUTTON-SokRabatt 
         ArtBas.StrTypeID ArtBas.SaSong ArtBas.Farg ArtBas.MatKod ArtBas.Klack 
         ArtBas.inner-id ArtBas.ov-id ArtBas.slit-id ArtBas.last-id 
         ArtBas.anv-id ArtBas.BongTekst ArtBas.ProvKod ArtBas.RabKod 
         ArtBas.valkod ArtBas.VMId ArtBas.ProdNr ArtBas.LevDato1 
         ArtBas.LevDato2 ArtBas.Notat BUTTON-SokSesong BUTTON-SokFarge 
         BUTTON-SokMaterial BUTTON-SokInner BUTTON-SokOver BUTTON-SokSlit 
         BUTTON-SokLalst BUTTON-SokBruk BUTTON-SokStrType BUTTON-SokVaremerke 
         BUTTON-SokProdusent BUTTON-SokValuta FILL-IN-EndretInfo BUTTON-SokDato 
         RECT-29 RECT-31 RECT-32 RECT-54 
      WITH FRAME FRAME-ArtInfo IN WINDOW C-ArtKort.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-ArtInfo}
  FRAME FRAME-ArtInfo:SENSITIVE = NO.
  VIEW FRAME FRAME-dummy IN WINDOW C-ArtKort.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-dummy}
  FRAME FRAME-dummy:SENSITIVE = NO.
  DISPLAY RS-Vis 
      WITH FRAME FRAME-Lager IN WINDOW C-ArtKort.
  ENABLE B-Chart BROWSE-Lager RS-Vis 
      WITH FRAME FRAME-Lager IN WINDOW C-ArtKort.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-Lager}
  ENABLE BROWSE-Bestilling 
      WITH FRAME FRAME-Ordre IN WINDOW C-ArtKort.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-Ordre}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ForslagLopNr C-ArtKort 
PROCEDURE ForslagLopNr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF wForslagLopNr = "N" THEN
      RUN NesteLopNr.
  ELSE IF wForslagLopNr = "F" THEN
      RUN ForsteLopNr.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ForsteLopNr C-ArtKort 
PROCEDURE ForsteLopNr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR wLoop AS INT NO-UNDO.
      
  DEF BUFFER bufArtBas FOR ArtBas.
  DO WITH FRAME Default-Frame:
 
  FINN-NESTE:  
  REPEAT wLoop = INPUT ArtBas.LopNr TO 10000:
  
    IF wLoop = 0 THEN
      NEXT FINN-NESTE.
      
    IF CAN-FIND(bufArtBas NO-LOCK WHERE
      bufArtBas.Vg    = INPUT ArtBas.Vg AND
      bufArtBas.LopNr = wLoop) THEN
      DO:
        DISPLAY wLoop @ ArtBas.LopNr WITH FRAME Default-Frame.
        NEXT FINN-NESTE.
      END.
    ELSE
      LEAVE FINN-NESTE.          
  END. /* FINN-NESTE */
  
  IF wLoop > 9999 THEN
    DISPLAY " " @ ArtBas.LopNr WITH FRAME Default-Frame.
  ELSE
    DISPLAY wLoop @ ArtBas.LopNr WITH FRAME Default-Frame.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitDivInfo C-ArtKort 
PROCEDURE InitDivInfo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT AVAILABLE LevBas THEN
      RETURN.

  ASSIGN
    T-1  = LevBas.VisDivInfo[ 1]
    T-2  = LevBas.VisDivInfo[ 2]
    T-3  = LevBas.VisDivInfo[ 3]
    T-4  = LevBas.VisDivInfo[ 4]
    T-5  = LevBas.VisDivInfo[ 5]
    T-6  = LevBas.VisDivInfo[ 6]
    T-7  = LevBas.VisDivInfo[ 7]
    T-8  = LevBas.VisDivInfo[ 8]
    T-9  = LevBas.VisDivInfo[ 9]
    T-10 = LevBas.VisDivInfo[10]
    .
  DISPLAY
    T-10
  WITH FRAME Default-frame.
  DISPLAY
    T-1  
    T-2  
    T-3  
    T-4  
    T-5  
    T-6  
    T-7  
    T-8  
    T-9  
  WITH FRAME FRAME-ArtInfo.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initialize-Controls C-ArtKort 
PROCEDURE initialize-Controls :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR wCount AS INTE NO-UNDO.

  ASSIGN chTabStrip = chCtrlFrame:TabStrip
         chTabs     = chTabStrip:Tabs
         chTabs:Item(1):Caption = ENTRY(1,wTabTekst).
  DO wCount = 2 TO NUM-ENTRIES(wTabTekst):
      chTabs:Add(wCount,,ENTRY(wCount,wTabTekst)).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitKalkyle C-ArtKort 
PROCEDURE InitKalkyle PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /* Vises ut fra første prisprofil */
  FIND FIRST PrisProfil NO-LOCK NO-ERROR.

  /* Henter prisen. Hvis ny, slippes current record. */
  IF wModus <> "NY" THEN
    FIND ArtPris NO-LOCK WHERE
      ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
      ArtPris.ProfilNr   = PrisProfil.ProfilNr NO-ERROR. 
  ELSE 
    RELEASE ArtPris.
  
  /* Oppdatering av Side 1 m.m */
  IF AVAILABLE ArtPris THEN
    DO:
      DISPLAY
        /* ArtPris.InnkjopsPris[1] */ ""     @ FILL-IN-InnPris
        ArtPris.Pris[1]               @ FILL-IN-SalgsPris
       WITH FRAME FRAME-ArtInfo.

       IF ArtPris.Tilbud THEN
         DISPLAY
           ArtPris.TilbudFraDato @ FILL-IN-UtsFra
           ArtPris.TilbudTilDato @ FILL-IN-UtsTil
           ArtPris.Pris[2]       @ FILL-IN-TilbPris
         WITH FRAME FRAME-ArtInfo.         
       ELSE DO:
         DISPLAY
           "" @ FILL-IN-UtsFra
           "" @ FILL-IN-UtsTil
           "" @ FILL-IN-TilbPris
         WITH FRAME FRAME-ArtInfo.         
       END.
      /* Setter farge på tilværelsen. */  
      ASSIGN 
        FILL-IN-TilbPris:BGCOLOR IN FRAME FRAME-ArtInfo = 
                                   IF ArtPris.Tilbud THEN 13 ELSE ?
        FILL-IN-SalgsPris:BGCOLOR IN FRAME FRAME-ArtInfo = 
                                 IF ArtPris.Tilbud THEN ? ELSE 10.
    END.
  ELSE DO:
    DISPLAY
      "" @ FILL-IN-InnPris
      "" @ FILL-IN-SalgsPris
      "" @ FILL-IN-UtsFra
      "" @ FILL-IN-UtsTil
      "" @ FILL-IN-TilbPris
    WITH FRAME FRAME-ArtInfo.
    ASSIGN 
      FILL-IN-TilbPris:BGCOLOR IN FRAME FRAME-ArtInfo  = ? 
      FILL-IN-SalgsPris:BGCOLOR IN FRAME FRAME-ArtInfo = 10.
  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitTx C-ArtKort 
PROCEDURE InitTx :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  /* Artikkelinformasjon,Kalkyle,Lager,Bestilling,Historikk */
  ASSIGN         
    wTabTekst = Tx("Artikelinformation,Kalkyl,Lager,Beställning,Historik",100)
/*  wTabTekst = Tx("Artikkelinformasjon,Kalkyle,Lager,Bestilling,Historikk",100) */
    .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LagreArtBas C-ArtKort 
PROCEDURE LagreArtBas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER wBekreft AS INT NO-UNDO.
  
  DEF VAR ipStatus AS CHAR NO-UNDO.
  DEF VAR wOldUtgatt LIKE ArtBas.Utgatt NO-UNDO.
  RETURN "OK".
  ASSIGN ipStatus = "AVBRYT".

  /* Sjekker input */
  FIND VarGr NO-LOCK WHERE
    VarGr.Vg = int(ArtBas.Vg:screen-value IN FRAME DEFAULT-FRAME) NO-ERROR.
  IF NOT AVAILABLE VarGr OR
     int(ArtBas.Vg:screen-value IN FRAME DEFAULT-FRAME) = 0 THEN
    DO:
      MESSAGE Tx("Ukjent varegruppe!",108) VIEW-AS ALERT-BOX WARNING TITLE Tx("Lagringsfeil",109).
      APPLY "ENTRY":U TO ArtBas.Vg IN FRAME DEFAULT-FRAME.
      RETURN ipStatus.
    END.
  FIND FIRST VgKat NO-LOCK WHERE
    VgKat.Vg = int(ArtBas.Vg:screen-value IN FRAME DEFAULT-FRAME) AND
    VgKat.VgKat = INPUT ArtBas.VgKat NO-ERROR.
  IF NOT AVAILABLE VgKat THEN
    DO:
      MESSAGE Tx("Ukjent innkjøpskategori!",110)
        VIEW-AS ALERT-BOX TITLE Tx("Melding",107).
      APPLY "ENTRY":U TO ArtBas.VgKat IN FRAME DEFAULT-FRAME.
      RETURN ipStatus.
    END.
  IF int(ArtBas.LevNr:screen-value IN FRAME default-frame) = 0 THEN
    DO:
      MESSAGE Tx("Ugyldig leverandørnummer!",111)
        VIEW-AS ALERT-BOX TITLE Tx("Melding",107).
      RETURN ipStatus.
    END.
  FIND Rabatt NO-LOCK WHERE
    Rabatt.RabKod = int(ArtBas.RabKod:screen-value IN FRAME FRAME-ArtInfo) NO-ERROR.
  IF NOT AVAILABLE Rabatt THEN
    DO:
      MESSAGE Tx("Ukjent rabatt!",112) VIEW-AS ALERT-BOX WARNING TITLE Tx("Medling",107).
      APPLY "ENTRY":U TO ArtBas.RabKod IN FRAME FRAME-ArtInfo.
      RETURN ipStatus.
    END.
  FIND Prov NO-LOCK WHERE
    Prov.ProvKod = int(ArtBas.ProvKod:screen-value IN FRAME FRAME-ArtInfo) NO-ERROR.
  IF NOT AVAILABLE Prov THEN
    DO:
      MESSAGE Tx("Ukjent provisjon!",113) VIEW-AS ALERT-BOX WARNING TITLE Tx("Melding",107).
      APPLY "ENTRY":U TO ArtBas.ProvKod IN FRAME FRAME-ArtInfo.
      RETURN ipStatus.
    END.
  FIND LevBas NO-LOCK WHERE
    LevBas.LevNr = int(ArtBas.LevNr:screen-value IN FRAME DEFAULT-FRAME) NO-ERROR.
  IF NOT AVAILABLE LevBas THEN
    DO:
      MESSAGE Tx("Ukjent leverandør!",114) VIEW-AS ALERT-BOX WARNING TITLE Tx("Melding",107).
      APPLY "ENTRY":U TO ArtBas.LevNr IN FRAME DEFAULT-FRAME.
      RETURN ipStatus.
    END.
  FIND Farg NO-LOCK WHERE
    Farg.Farg = int(ArtBas.Farg:screen-value IN FRAME FRAME-ArtInfo) NO-ERROR.
  IF NOT AVAILABLE Farg THEN
    DO:
      MESSAGE Tx("Ukjent farge!",115) VIEW-AS ALERT-BOX WARNING TITLE Tx("Melding",107).
      APPLY "ENTRY":U TO ArtBas.Farg IN FRAME FRAME-ArtInfo.
      RETURN ipStatus.
    END.
  FIND Sasong NO-LOCK WHERE
    Sasong.Sasong = int(ArtBas.Sasong:screen-value IN FRAME FRAME-ArtInfo) NO-ERROR.
  IF NOT AVAILABLE Sasong THEN
    DO:
      MESSAGE Tx("Ukjent sesong!",116) VIEW-AS ALERT-BOX WARNING TITLE Tx("Melding",107).
      APPLY "ENTRY":U TO ArtBas.Sasong IN FRAME FRAME-ArtInfo.
      RETURN ipStatus.
    END.
  FIND Valuta NO-LOCK WHERE
    Valuta.ValKod = INPUT ArtBas.ValKod NO-ERROR.
  IF NOT AVAILABLE Valuta THEN
    DO:
      MESSAGE Tx("Ukjent valuta!",117) VIEW-AS ALERT-BOX WARNING TITLE Tx("Melding",107).
      APPLY "ENTRY":U TO ArtBas.ValKod IN FRAME FRAME-ArtInfo.
      RETURN ipStatus.
    END.
  FIND Material NO-LOCK WHERE
    Material.MatKod = int(ArtBas.MatKod:screen-value IN FRAME FRAME-ArtInfo) NO-ERROR.
  IF NOT AVAILABLE Material THEN
    DO:
      MESSAGE Tx("Ukjent Material!",118) VIEW-AS ALERT-BOX WARNING TITLE Tx("Melding",107).
      APPLY "ENTRY":U TO ArtBas.MatKod IN FRAME FRAME-ArtInfo.
      RETURN ipStatus.
    END.
  FIND StrType NO-LOCK WHERE
    StrType.StrTypeID = INPUT ArtBas.StrTypeId NO-ERROR.
  IF NOT AVAILABLE StrType THEN
    DO:
      MESSAGE Tx("Ukjent størrelsestype!",119)
      VIEW-AS ALERT-BOX TITLE "Melding".
      RETURN ipStatus.
    END.
 
  IF AVAILABLE ArtBas THEN 
    wOldUtgatt = ArtBas.Utgatt.
  ELSE
    wOldUtgatt = ?.
    
  LAGRE_ARTBAS:
  DO TRANSACTION:
    IF wModus = "NY" THEN
      DO:
        RELEASE ArtBas. /* Slipper gammel post. */
        
        IF int(ArtBas.LopNr:screen-value IN FRAME DEFAULT-FRAME) <> ? AND
           int(ArtBas.LopNr:screen-value IN FRAME DEFAULT-FRAME) <> 0  THEN
          FIND ArtBas NO-LOCK WHERE
            ArtBas.Vg    = int(ArtBas.Vg:screen-value IN FRAME DEFAULT-FRAME) AND
            ArtBas.LopNr = int(ArtBas.LopNr:screen-value IN FRAME DEFAULT-FRAME) NO-ERROR.
        IF AVAILABLE ArtBas THEN
          DO:
            MESSAGE Tx("Artikkel med dette Varegruppe/løpenummer finnes fra før!",120) 
                    VIEW-AS ALERT-BOX WARNING TITLE Tx("Melding",107).
            APPLY "ENTRY":U TO ArtBas.LopNr IN FRAME DEFAULT-FRAME.
            RETURN ipStatus.
          END.
            
        /* Artikkelinformasjon */
        CREATE ArtBas.
        ASSIGN
          wModus       = "ENDRE"
          /*--- ArtBas.StrTypeId:sensitive = false ---*/
          wArtBasRecid = recid(ArtBas)
          ArtBas.Vg    = int(ArtBas.Vg:screen-value IN FRAME DEFAULT-FRAME)
          ArtBas.LopNr = (if (int(ArtBas.LopNr:screen-value in frame DEFAULT-FRAME) = 0 or
                              int(ArtBas.LopNr:screen-value in frame DEFAULT-FRAME) = ?)
                         then ?
                         else int(ArtBas.LopNr:screen-value in frame DEFAULT-FRAME))
          ArtBas.LapTop = if wLapTop then true else false.
        DISPLAY ArtBas.ArtikkelNr WITH FRAME DEFAULT-FRAME.
        
        /* Pris og lagerinformasjon */
        BUTIKKLOOP:
        FOR EACH Butiker NO-LOCK 
          BREAK BY Butiker.ProfilNr:
          /* En prisrecord pr. profil. */
          /*
          if first-of(Butiker.ProfilNr) then
            do:              
              create ArtPris.
              assign
                ArtPris.ArtikkelNr = ArtBas.ArtikkelNr
                ArtPris.ProfilNr   = Butiker.ProfilNr.
              assign
                ArtPris.Tilbud = false.
            end.
          */
          /* En lagerpost pr.. butikk, hvis det er lagerstyring */
          IF INPUT ArtBas.Lager THEN
            DO:
              CREATE Lager.
              ASSIGN
                Lager.ArtikkelNr = ArtBas.ArtikkelNr
                Lager.Butik      = Butiker.Butik.                    
            END.
                
          /* Artlag poster opprettes der hvor behovet oppstår. */             
        END. /* BUTIKKLOOP */
      END.
    ELSE DO:
      FIND ArtBas EXCLUSIVE-LOCK WHERE
        recid(ArtBas) = wArtBasRecid NO-ERROR NO-WAIT.

    END.
      
    /* Sjekker om den er under oppdatering. */  
    IF LOCKED ArtBas THEN
      DO:
        MESSAGE Tx("Artikkelen oppdateres fra en annen terminal" + chr(13) +
                "Forsöke å lagre en gang til",121) VIEW-AS ALERT-BOX 
          WARNING TITLE Tx("Melding",107).
        RETURN NO-APPLY ipStatus.
      END.

    /* Sjekker om den er slettet. */  
    ELSE IF NOT AVAILABLE ArtBas THEN
      DO:
        MESSAGE Tx("Artikkelen er slettet!",122) SKIP
                VIEW-AS ALERT-BOX 
          WARNING TITLE Tx("Melding",107).
       RETURN NO-APPLY ipStatus.
      END.

    /* Lagrer nökkelfelt */
    IF wModus = "NY" THEN
      ASSIGN
        ArtBas.Ny_Dato = TODAY
        FRAME DEFAULT-FRAME ArtBas.Vg 
        FRAME DEFAULT-FRAME ArtBas.LopNr.

    /* Lagrer ArtBas. */
    ASSIGN
      /* ArtBas.Korr_Dato = today */
      FRAME DEFAULT-FRAME ArtBas.VgKat
      FRAME DEFAULT-FRAME ArtBas.Beskr 
      FRAME DEFAULT-FRAME ArtBas.LevNr 
      FRAME DEFAULT-FRAME ArtBas.LevKod
      FRAME DEFAULT-FRAME ArtBas.LevFargKod
      FRAME DEFAULT-FRAME ArtBas.Utgatt
      FRAME DEFAULT-FRAME ArtBas.Lager
      FRAME DEFAULT-FRAME ArtBas.Storrelser
      FRAME DEFAULT-FRAME ArtBas.BildNr
      FRAME DEFAULT-FRAME TOGGLE-Annonse
      ArtBas.AnonseArtikkel = TOGGLE-Annonse
      ArtBas.Storrelser = IF INPUT ArtBas.Lager 
                            THEN ArtBas.Storrelser
                            ELSE FALSE
      .
      
    IF wOldUtgatt <> ArtBas.Utgatt THEN
      DO:  
        IF ArtBas.Utgatt THEN
          ASSIGN
            ArtBas.UtgattDato = TODAY.
        ELSE
          ASSIGN
            ArtBas.UtgattDato = ?.
        wOldUtgatt = ArtBas.Utgatt.
     END.
    ASSIGN
      ArtBas.Hg = VarGr.Hg
      FRAME FRAME-ArtInfo ArtBas.ValKod
      FRAME FRAME-ArtInfo ArtBas.VMId
      FRAME FRAME-ArtInfo ArtBas.ProdNr
      FRAME FRAME-ArtInfo ArtBas.StrTypeId
      FRAME FRAME-ArtInfo ArtBas.Notat
      FRAME FRAME-ArtInfo ArtBas.Sasong
      FRAME FRAME-ArtInfo ArtBas.Farg
      FRAME FRAME-ArtInfo ArtBas.MatKod
      FRAME FRAME-ArtInfo ArtBas.Klack
      FRAME FRAME-ArtInfo ArtBas.ProvKod
      FRAME FRAME-ArtInfo ArtBas.RabKod
      FRAME FRAME-ArtInfo ArtBas.BongTekst
      FRAME FRAME-ArtInfo ArtBas.Anv-Id
      FRAME FRAME-ArtInfo ArtBas.Last-Id
      FRAME FRAME-ArtInfo ArtBas.Slit-Id
      FRAME FRAME-ArtInfo ArtBas.Ov-Id
      FRAME FRAME-ArtInfo ArtBas.Inner-Id
      ArtBas.DivInfo[ 1] = INPUT FI-1
      ArtBas.DivInfo[ 2] = INPUT FI-2
      ArtBas.DivInfo[ 3] = INPUT FI-3
      ArtBas.DivInfo[ 4] = INPUT FI-4
      ArtBas.DivInfo[ 5] = INPUT FI-5
      ArtBas.DivInfo[ 6] = INPUT FI-6
      ArtBas.DivInfo[ 7] = INPUT FI-7
      ArtBas.DivInfo[ 8] = INPUT FI-8
      ArtBas.VisDivInfo[ 1] = INPUT T-1
      ArtBas.VisDivInfo[ 2] = INPUT T-2
      ArtBas.VisDivInfo[ 3] = INPUT T-3
      ArtBas.VisDivInfo[ 4] = INPUT T-4
      ArtBas.VisDivInfo[ 5] = INPUT T-5
      ArtBas.VisDivInfo[ 6] = INPUT T-6
      ArtBas.VisDivInfo[ 7] = INPUT T-7
      ArtBas.VisDivInfo[ 8] = INPUT T-8
      ArtBas.VisDivInfo[ 9] = INPUT T-9
      ArtBas.VisDivInfo[10] = INPUT T-10
      .

    /* Hvis det ikke finnes Bilderegisterpost, opprettes denne */
    IF NOT CAN-FIND(BildeRegister OF ArtBas) THEN
      DO:
        CREATE BildeRegister.
        ASSIGN
          BildeRegister.BildNr = ArtBas.BildNr.
      END.

    /* Oppdaterer artikkelens bestillinger. */
    IF CAN-FIND(FIRST BestHode WHERE BestHode.ArtikkelNr = ArtBAs.ArtikkelNr AND
                                     BestHode.LEvKod <>    ArtBas.LevKod) THEN
    DO:
    FIND FIRST BestHode EXCLUSIVE-LOCK WHERE
        BestHode.ArtikkelNr =  ArtBas.ArtikkelNr AND
        BestHode.LevKod     <> ArtBas.LevKod NO-ERROR NO-WAIT.
    IF LOCKED BestHode THEN
    DO:
        MESSAGE Tx("Bestilling oppdateres fra en annen terminal." + CHR(13) +
                "Lagring avbrytes.",123)
            VIEW-AS ALERT-BOX MESSAGE TITLE Tx("Melding",107).
        UNDO LAGRE_ARTBAS.
        RETURN ipStatus.
    END.
    ELSE
        ASSIGN
            BestHOde.LEvKod = ArtBas.LevKod.
    DO WHILE AVAILABLE BestHode:
        FIND NEXT BestHode EXCLUSIVE-LOCK WHERE
            BestHode.ArtikkelNr =  ArtBas.ArtikkelNr AND
            BestHode.LevKod     <> ArtBas.LevKod NO-ERROR NO-WAIT.
        IF LOCKED BestHode THEN
        DO:
            MESSAGE Tx("Bestilling oppdateres fra en annen terminal." + chr(13) +
                    "Lagring avbrytes.",123)
                VIEW-AS ALERT-BOX MESSAGE TITLE Tx("Melding",107).
            UNDO LAGRE_ARTBAS.
            RETURN ipStatus.
        END.
        IF AVAILABLE BestHode THEN
            ASSIGN
              BestHode.LevKod = ArtBas.LevKod.
    END.
    END.

    ASSIGN
        wKopi = FALSE
        .
  END. /* TRANSACTION LAGRE_ARTBAS */

  /* run VisArtBas. */
  RUN VisEndretinfo.
  RUN Toolbar.

  /* Frisker opp Lagerfliken */ 
  RUN VisLager (TRUE).  

  RETURN "OK".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NesteLopNr C-ArtKort 
PROCEDURE NesteLopNr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bufArtBas FOR ArtBas.
  
  DO WITH FRAME Default-Frame:

  FIND LAST bufArtBas NO-LOCK WHERE
    bufArtBas.Vg = INPUT ArtBas.Vg AND
    bufArtBas.LopNr <> ? NO-ERROR.
  IF NOT AVAILABLE bufArtBas THEN
    DISPLAY 1 @ ArtBas.LopNr WITH FRAME Default-Frame.
  ELSE DO:
    IF bufArtBas.Lopnr + 1 > 9999 THEN
      DO:
        DISPLAY " " @ ArtBas.LopNr WITH FRAME Default-Frame.
        RETURN NO-APPLY.
      END.
    ELSE
      DISPLAY bufArtBas.LopNr + 1 @ Artbas.LopNr WITH FRAME Default-Frame.
  END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NyBest C-ArtKort 
PROCEDURE NyBest :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT AVAILABLE ArtBas THEN
    RETURN NO-APPLY.
  IF ArtBas.Utgatt = FALSE THEN   
    DO:
      MESSAGE Tx("Artikkelen er utgatt. Bestilling kan ikke registreres på utgåtte artikler.",124)
        VIEW-AS ALERT-BOX MESSAGE TITLE Tx("Melding",107).
      RETURN NO-APPLY.
    END.
  IF ArtBas.Lager = FALSE THEN   
    DO:
      MESSAGE Tx("Artikkelen har ikke lagerstyring. Bestilling kan ikke registreres.",125)
        VIEW-AS ALERT-BOX MESSAGE TITLE Tx("Melding",107).
      RETURN NO-APPLY.
    END.

  CREATE tmpChild.
  ASSIGN
    wBestHodeRecid = ?.
  RUN w-gridord.w PERSISTENT SET tmpChild.wChild (INPUT wArtBasRecid, INPUT-OUTPUT wBestHodeRecid, "NY").  
  FIND BestHode NO-LOCK WHERE
      recid(BestHode) = wBestHodeRecid NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OrdreToolbar C-ArtKort 
PROCEDURE OrdreToolbar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME DEFAULT-FRAME:
  
  APPLY "entry":U TO BROWSE BROWSE-Bestilling.
    DO WITH FRAME FRAME-Ordre:
      ASSIGN
        B-OPpdBest:SENSITIVE          = FALSE
        BUTTON-NyBest:sensitive       = FALSE
        BUTTON-EndreBest:sensitive    = FALSE
        BUTTON-Kalkyle:sensitive      = FALSE
        BUTTON-SlettBest:sensitive    = FALSE
        BUTTON-Innleveranse:sensitive = FALSE.
    END.

/*   if input ArtBas.Aktivert = false or                */
/*      input ArtBas.Lager    = false then              */
/*     do with frame FRAME-Ordre:                       */
/*       assign                                         */
/*         BUTTON-NyBest:sensitive       = false        */
/*         BUTTON-EndreBest:sensitive    = false        */
/*         BUTTON-Kalkyle:sensitive      = false        */
/*         BUTTON-SlettBest:sensitive    = false        */
/*         BUTTON-Innleveranse:sensitive = false.       */
/*     end.                                             */
/*   else do with frame FRAME-Ordre:                    */
/*     if browse BROWSE-Bestilling:focused-row = ? then */
/*       assign                                         */
/*         BUTTON-NyBest:sensitive       = true         */
/*         BUTTON-EndreBest:sensitive    = false        */
/*         BUTTON-Kalkyle:sensitive      = false        */
/*         BUTTON-SlettBest:sensitive    = false        */
/*         BUTTON-Innleveranse:sensitive = false.       */
/*      else                                            */
/*       assign                                         */
/*         BUTTON-NyBest:sensitive       = true         */
/*         BUTTON-EndreBest:sensitive    = true         */
/*         BUTTON-Kalkyle:sensitive      = true         */
/*         BUTTON-SlettBest:sensitive    = true         */
/*         BUTTON-Innleveranse:sensitive = true.        */
/*   end.                                               */
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Overfor C-ArtKort 
PROCEDURE Overfor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME FRAME-Lager:
  IF NOT AVAILABLE ArtBas THEN RETURN NO-APPLY.   
  CREATE tmpChild.
  RUN w-gridlager.w PERSISTENT SET tmpChild.wChild (INPUT wArtBasRecid, "ENDRE").
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RensFrame C-ArtKort 
PROCEDURE RensFrame :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN
    T-LapTop = FALSE
    FI-1 = ""   
    FI-2 = ""   
    FI-3 = ""   
    FI-4 = ""   
    FI-5 = ""   
    FI-6 = ""   
    FI-7 = ""   
    FI-8 = ""   
    T-1  = FALSE
    T-2  = FALSE
    T-3  = FALSE
    T-4  = FALSE
    T-5  = FALSE
    T-6  = FALSE
    T-7  = FALSE
    T-8  = FALSE
    T-9  = FALSE
    T-10 = FALSE
    FI-PaTilbud = ""
    .
  DISPLAY 
    "" @ ArtBas.ArtikkelNr
    "" @ ArtBas.VgKat
    "" @ ArtBas.Vg 
    "" @ ArtBas.LopNr 
    "" @ ArtBas.Beskr 
    "" @ ArtBas.LevNr 
    "" @ ArtBas.LevKod
    "" @ ArtBas.Inn_Dato
    "" @ FILL-IN-VgBeskr
    "" @ FILL-IN-LevNamn
    "" @ huvgr.hg
    "" @ HuvGr.HgBeskr
    "" @ ArtBas.LevFargKod
    "" @ ArtBas.BildNr
    "" @ BildeRegister.Tekst
    "" @ FI-PaTilbud
    TOGGLE-Annonse
    T-LapTop
    T-10 
  WITH FRAME DEFAULT-FRAME.

  /*
  if wAktivFlip = 1 then
    output to value("nul").
  */
  DISPLAY
    "" @ Varemerke.Beskrivelse
    "" @ Produsent.Beskrivelse
    "" @ StrType.Beskrivelse
    "" @ ArtBas.VMId
    "" @ ArtBas.ProdNr
    "" @ ArtBas.StrTypeId
    "" @ ArtBas.Sasong
    "" @ Sasong.SasBeskr 
    "" @ ArtBas.Farg
    "" @ Farg.FarBeskr
    "" @ ArtBas.MatKod
    "" @ Material.MatBeskr
    "" @ ArtBas.Klack
    "" @ ArtBas.ProvKod
    "" @ Prov.ProvProc 
    "" @ ArtBas.RabKod
    "" @ Rabatt.RabProc
    "" @ FILL-IN-SalgsPris
    "" @ FILL-IN-InnPris
    "" @ FILL-IN-TilbPris  
    "" @ FILL-IN-UtsFra
    "" @ FILL-IN-UtsTil
    "" @ ArtBas.BongTekst
    "" @ ArtBas.Anv-Id   
    "" @ Anv-Kod.AnvBeskr        
    "" @ ArtBas.Last-Id  
    "" @ Last-Sko.LastBeskr
    "" @ ArtBas.Slit-Id  
    "" @ SlitSula.SlitBeskr    
    "" @ ArtBas.Ov-Id    
    "" @ Ovandel.OvBeskr      
    "" @ ArtBas.Inner-Id 
    "" @ InnerSula.InnerBeskr
    "" @ ArtBas.ValKod
    "" @ Valuta.ValKurs
    "" @ ArtBas.LevDato1
    "" @ ArtBas.LevDato2
    FI-1    
    FI-2    
    FI-3    
    FI-4    
    FI-5    
    FI-6    
    FI-7    
    FI-8    
    T-1  
    T-2  
    T-3  
    T-4  
    T-5  
    T-6  
    T-7  
    T-8  
    T-9  
  WITH FRAME FRAME-ArtInfo.
  /*
  if wAktivFlip = 1 then
    output close.
  */
  
  IF AVAILABLE ArtBas THEN
    ArtBas.Notat:screen-value IN FRAME FRAME-ArtInfo = "".
  
  /* Viser blankt bilde */
  RUN VisBilde (1). 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetAktivFlik C-ArtKort 
PROCEDURE SetAktivFlik :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piAktivFlip AS INT NO-UNDO.

  ASSIGN 
    wAktivFlip = piAktivFlip
    .
  RUN ByttFrame. /* Bytter tab */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetBrowseRadFont C-ArtKort 
PROCEDURE SetBrowseRadFont :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  ASSIGN
    tmpLager.Butik:bgcolor      IN BROWSE BROWSE-Lager = 8
    tmpLager.SumAntall:bgcolor  IN BROWSE BROWSE-Lager = 8
    tmpLager.SumVerdi:bgcolor   IN BROWSE BROWSE-Lager = 8
    tmpLager.DivAntall:bgcolor   IN BROWSE BROWSE-Lager = 8
    tmpLager.Antall[ 1]:bgcolor IN BROWSE BROWSE-Lager = 8
    tmpLager.Antall[ 2]:bgcolor IN BROWSE BROWSE-Lager = 8
    tmpLager.Antall[ 3]:bgcolor IN BROWSE BROWSE-Lager = 8
    tmpLager.Antall[ 4]:bgcolor IN BROWSE BROWSE-Lager = 8
    tmpLager.Antall[ 5]:bgcolor IN BROWSE BROWSE-Lager = 8
    tmpLager.Antall[ 6]:bgcolor IN BROWSE BROWSE-Lager = 8
    tmpLager.Antall[ 7]:bgcolor IN BROWSE BROWSE-Lager = 8
    tmpLager.Antall[ 8]:bgcolor IN BROWSE BROWSE-Lager = 8
    tmpLager.Antall[ 9]:bgcolor IN BROWSE BROWSE-Lager = 8
    tmpLager.Antall[10]:bgcolor IN BROWSE BROWSE-Lager = 8
    tmpLager.Antall[11]:bgcolor IN BROWSE BROWSE-Lager = 8
    tmpLager.Antall[12]:bgcolor IN BROWSE BROWSE-Lager = 8
    tmpLager.Antall[13]:bgcolor IN BROWSE BROWSE-Lager = 8
    tmpLager.Antall[14]:bgcolor IN BROWSE BROWSE-Lager = 8
    tmpLager.Antall[15]:bgcolor IN BROWSE BROWSE-Lager = 8
    tmpLager.Antall[16]:bgcolor IN BROWSE BROWSE-Lager = 8
    tmpLager.Antall[17]:bgcolor IN BROWSE BROWSE-Lager = 8
    tmpLager.Antall[18]:bgcolor IN BROWSE BROWSE-Lager = 8
    tmpLager.Antall[19]:bgcolor IN BROWSE BROWSE-Lager = 8
    tmpLager.Antall[20]:bgcolor IN BROWSE BROWSE-Lager = 8
    tmpLager.Antall[21]:bgcolor IN BROWSE BROWSE-Lager = 8
    tmpLager.Antall[22]:bgcolor IN BROWSE BROWSE-Lager = 8
    tmpLager.Antall[23]:bgcolor IN BROWSE BROWSE-Lager = 8
    tmpLager.Antall[24]:bgcolor IN BROWSE BROWSE-Lager = 8
    tmpLager.Antall[25]:bgcolor IN BROWSE BROWSE-Lager = 8
    tmpLager.Antall[26]:bgcolor IN BROWSE BROWSE-Lager = 8
    tmpLager.Antall[27]:bgcolor IN BROWSE BROWSE-Lager = 8
    tmpLager.Antall[28]:bgcolor IN BROWSE BROWSE-Lager = 8
    tmpLager.Antall[29]:bgcolor IN BROWSE BROWSE-Lager = 8
    tmpLager.Antall[30]:bgcolor IN BROWSE BROWSE-Lager = 8
    tmpLager.Antall[31]:bgcolor IN BROWSE BROWSE-Lager = 8
    tmpLager.Antall[32]:bgcolor IN BROWSE BROWSE-Lager = 8
    tmpLager.Antall[33]:bgcolor IN BROWSE BROWSE-Lager = 8
    tmpLager.Antall[34]:bgcolor IN BROWSE BROWSE-Lager = 8
    tmpLager.Antall[35]:bgcolor IN BROWSE BROWSE-Lager = 8
    tmpLager.Antall[36]:bgcolor IN BROWSE BROWSE-Lager = 8
    tmpLager.Antall[37]:bgcolor IN BROWSE BROWSE-Lager = 8
    tmpLager.Antall[38]:bgcolor IN BROWSE BROWSE-Lager = 8
    tmpLager.Antall[39]:bgcolor IN BROWSE BROWSE-Lager = 8
    tmpLager.Antall[40]:bgcolor IN BROWSE BROWSE-Lager = 8
    tmpLager.Antall[41]:bgcolor IN BROWSE BROWSE-Lager = 8
    tmpLager.Antall[42]:bgcolor IN BROWSE BROWSE-Lager = 8
    tmpLager.Antall[43]:bgcolor IN BROWSE BROWSE-Lager = 8
    tmpLager.Antall[44]:bgcolor IN BROWSE BROWSE-Lager = 8
    tmpLager.Antall[45]:bgcolor IN BROWSE BROWSE-Lager = 8
    tmpLager.Antall[46]:bgcolor IN BROWSE BROWSE-Lager = 8
    tmpLager.Antall[47]:bgcolor IN BROWSE BROWSE-Lager = 8
    tmpLager.Antall[48]:bgcolor IN BROWSE BROWSE-Lager = 8.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettLopenummer C-ArtKort 
PROCEDURE SettLopenummer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME DEFAULT-FRAME:
  RUN d-vtildelopnr.w (INPUT wArtBasRecid).  
  IF RETURN-VALUE = "AVBRYT" THEN
    RETURN NO-APPLY.
  ELSE
    DO:
      FIND ArtBas NO-LOCK WHERE
        recid(ArtBas) = wArtBasRecid NO-ERROR.
      IF AVAILABLE ArtBas THEN
        DO:
          DISPLAY ArtBas.LopNr WITH FRAME DEFAULT-FRAME.        
          ASSIGN
            BUTTON-SettLopNr:hidden IN FRAME DEFAULT-FRAME    = TRUE
            BUTTON-SettLopNr:sensitive IN FRAME DEFAULT-FRAME = FALSE
            B-ByttVareGr:hidden IN FRAME DEFAULT-FRAME    = TRUE
            B-ByttVareGr:sensitive IN FRAME DEFAULT-FRAME = FALSE.
        END.
    END.

END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettArtbas C-ArtKort 
PROCEDURE SlettArtbas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR i      AS INT NO-UNDO.
  DEF VAR wTekst AS CHAR NO-UNDO.
  DEF VAR wBekreft AS LOG NO-UNDO.

  /* Kontroll mot nytt bestillingsregister. */
  IF CAN-FIND(FIRST BestHode WHERE
    BestHode.ArtikkelNr = ArtBas.ArtikkelNr) THEN
  IF AVAILABLE BestHode THEN
    DO:
      wBekreft = FALSE.
      MESSAGE Tx("Det er registrert bestilling(er) på artikkelen." + chr(13) +
              "Skal den alikevel tas bort?",126) SKIP VIEW-AS ALERT-BOX 
              QUESTION BUTTONS YES-NO TITLE Tx("Bekreftelse",105) UPDATE wBekreft.
      IF wBekreft <> TRUE THEN         
        RETURN NO-APPLY "AVBRYT".
    END.

  /* Kontroll mot kundetransaksjoner. */
  IF CAN-FIND(FIRST KundeTrans WHERE
    KundeTrans.ArtikkelNr = ArtBas.ArtikkelNr) THEN
  IF AVAILABLE KundeTrans THEN
    DO:
      wBekreft = FALSE.
      MESSAGE Tx("Det er kundetransaksjoner registrert på artikkelen." + chr(13) +
              "Skal den alikevel tas bort?",138) SKIP VIEW-AS ALERT-BOX 
              QUESTION BUTTONS YES-NO TITLE Tx("Bekreftelse",105) UPDATE wBekreft.
      IF wBekreft <> TRUE THEN         
        RETURN NO-APPLY "AVBRYT".
    END.
  
  /* Kontroll mot medlemstransaksjoner. */
  IF CAN-FIND(FIRST MedTrans WHERE
    MedTrans.ArtikkelNr = ArtBas.ArtikkelNr) THEN
  IF AVAILABLE MedTrans THEN
    DO:
      wBekreft = FALSE.
      MESSAGE Tx("Det er medlemstransaksjoner registrert på artikkelen." + chr(13) +
              "Skal den alikevel tas bort?",139) SKIP VIEW-AS ALERT-BOX 
              QUESTION BUTTONS YES-NO TITLE Tx("Bekreftelse",105) UPDATE wBekreft.
      IF wBekreft <> TRUE THEN         
        RETURN NO-APPLY "AVBRYT".
    END.
  
/*   /* Kontroll Tellelinje. */                                                     */
/*   if can-find(first Tellelinje where                                             */
/*     Tellelinje.ArtikkelNr = ArtBas.ArtikkelNr) then                              */
/*   if available TelleLinje then                                                   */
/*     do:                                                                          */
/*       wBekreft = false.                                                          */
/*       message Tx("Det er registrert varetelling(er) på artikkelen. " + chr(13) + */
/*               "Disse må slettes først.",137) +                                   */
/*               " (" + string(TelleLinje.TelleNr) + ")" skip view-as alert-box     */
/*               QUESTION buttons OK title Tx("Melding",107).                       */
/*         return no-apply "AVBRYT".                                                */
/*     end.                                                                         */

  /* Kontroll mot kampanjeregister */
  IF CAN-FIND(FIRST KampanjeLinje NO-LOCK WHERE
              KampanjeLinje.ArtikkelNr = ArtBas.ArtikkelNr) THEN
  DO:
      MESSAGE "Det er registrert kampanjelinjer på artikkelen." SKIP
              "Kampanjer som artikkelen er med på må slettes før artikkelen kan slettes."
          VIEW-AS ALERT-BOX INFO BUTTONS OK TITLE "Melding".
      RETURN NO-APPLY "AVBRYT".
  END.

  /* Kontroll mot overføringsbunt. */
  IF CAN-FIND(FIRST OvBuffer WHERE
              OvBuffer.ArtikkelNr = ArtBas.ArtikkelNr) THEN
  DO:
      MESSAGE "Det er registrert overføringstransaksjoner på artikkelen." SKIP
              "Overføringsbunter som artikkelen er med på må slettes før artikkelen kan slettes."
          VIEW-AS ALERT-BOX INFO BUTTONS OK TITLE "Melding".
      RETURN NO-APPLY "AVBRYT".
  END.

  /* Fikser bestillingene */
  FOR EACH BestHode OF ArtBas NO-LOCK:
    RUN SlettBestilling (2).
    IF RETURN-VALUE = "AVBRYT" THEN 
      RETURN NO-APPLY "AVBRYT".
  END.
        
  SLETTING:
  DO:
      RUN slettartbasbatch.w (INPUT ArtBas.ArtikkelNr).
      IF RETURN-VALUE <> "" THEN
      DO:
         MESSAGE "Sletting av artikkel avbrutt. Årsak: " RETURN-VALUE "." SKIP
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
         RETURN "AVBRYT".
      END.
  END. /* SLETTING */

  RETURN NO-APPLY "SLETTET".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettBestilling C-ArtKort 
PROCEDURE SlettBestilling PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER wModus AS INT NO-UNDO. /* 1-Bekreft, 2-Uten bekreftelse" */

DO WITH FRAME FRAME-Ordre:
  IF NOT AVAILABLE BestHode THEN
    RETURN NO-APPLY.
  
  ASSIGN
    wBestHodeRecid = recid(BestHode).

  IF wModus <> 2 THEN 
    DO:
      IF can-do(",4,5",string(BestHode.BestStat)) THEN
        DO:
          MESSAGE Tx("Bestillingen er sendt til leverandør eller delhvis innlevert." + CHR(13) +
              "Skal den alikevel slettes?",127)
              VIEW-AS ALERT-BOX BUTTONS YES-NO-CANCEL TITLE Tx("Bekreftelse",105)
              UPDATE wOk.
        END.
      ELSE DO:  
        MESSAGE Tx("Skal bestilling slettes?",128)
          VIEW-AS ALERT-BOX BUTTONS YES-NO-CANCEL TITLE Tx("Bekreftelse",105) 
        UPDATE wOk.
      END.
    END.
  ELSE 
    wOk = TRUE.

  IF wOk = TRUE THEN
    DO TRANSACTION:
      {sww.i}
      /* KanSlettes*/      
      RUN w-gridord.w (INPUT wArtBasRecid, INPUT-OUTPUT wBestHodeRecid, "SLETT").
      IF wBestHodeRecid = ? THEN      
          wOk = BROWSE-Bestilling:DELETE-CURRENT-ROW( ).
      {swn.i}
      APPLY "entry":U TO BROWSE BROWSE-Bestilling.
      RUN OrdreToolbar.
      RETURN "OK".
  END.
  ELSE RETURN NO-APPLY "AVBRYT".
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettProg C-ArtKort 
PROCEDURE SlettProg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR wFrameHAndle AS HANDLE.
  /*
  if valid-handle(wHistorikk) then
    delete procedure wHistorikk.
  if valid-handle(wKalkyle) then
  DO:
      /*RUN SlettFrame IN wKalkyle (OUTPUT wFrameHandle).*/
      delete procedure wKalkyle.
      /*
      MESSAGE "Gurre" wFrameHandle VALID-HANDLE(wFrameHandle) VIEW-AS ALERT-BOX.
      DELETE WIDGET wFrameHAndle.
      */
  END.
  */
  FOR EACH tmpChild:
    IF valid-handle(tmpChild.wChild) THEN
      DELETE PROCEDURE tmpChild.wChild.
  END.
  APPLY "CLOSE":U TO THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Sok2LevBas C-ArtKort 
PROCEDURE Sok2LevBas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME DEFAULT-FRAME:
  IF int(ArtBas.LevNr:screen-value IN FRAME default-frame) = 0 THEN
    DO:
      MESSAGE Tx("Ugyldig leverandørnummer!",111)
        VIEW-AS ALERT-BOX TITLE Tx("Melding",107).
      RETURN NO-APPLY.
    END.
  FIND LevBas NO-LOCK WHERE
    LevBas.LevNr = int(ArtBas.LevNr:screen-value IN FRAME default-frame) NO-ERROR.
  IF AVAILABLE LevBas THEN 
    DO:
      FIND Valuta NO-LOCK WHERE 
        Valuta.ValKod = LevBas.Valkod NO-ERROR.
      DISPLAY 
        LevBas.LevNamn @ FILL-IN-LevNamn 
      WITH FRAME default-frame.
      
      DISPLAY 
        Valuta.ValKod @ ArtBas.ValKod
        Valuta.ValKurs WHEN AVAILABLE Valuta
      WITH FRAME FRAME-ArtInfo.         
    END.      
  ELSE DO:
      DISPLAY
          "" @ ArtBas.LevNr
          "" @ FILL-IN-LevNamn 
      WITH FRAME Default-Frame.
      RETURN "AVBRYT".
  END.

  IF AVAILABLE LevBas AND AVAILABLE ArtBas THEN
    LEVFIX:
    DO:
      IF wModus = "NY" THEN
        RUN InitDivInfo.

      /* Leverandørnummer er ikke endret. */
      IF LevBas.LevNr = ArtBas.LevNr THEN
        LEAVE LEVFIX.
      ELSE
        RUN InitDivInfo.

      /* Endring av leverandørnummer på artikkelens bestillinger. */
      IF wModus <> "NY" THEN
        DO:  
          IF CAN-FIND(FIRST BestHode WHERE
                        BestHode.LevNr      = ArtBas.LevNr AND
                        BestHode.ArtikkelNr = ArtBas.ArtikkelNr AND
                        BestHode.BestStat >= 1 AND
                        BestHode.BestStat <= 2) THEN
            DO:
              MESSAGE Tx("Det finnes bestillingsforslag og godkjente bestillinger på denne artikkel." + CHR(13) +
                      "Ved bytte av leverandør, må det også byttes leverandør på disse." + chr(13) +
                      "Skal bytte av leverandør utføres?",129) 
                      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
                      TITLE Tx("Bekreftelse",105)
                      UPDATE wSvar AS LOG.
              IF wSvar <> TRUE THEN
                DO:
                  FIND LevBas OF ArtBas NO-LOCK NO-ERROR.
                  DISPLAY 
                    ArtBas.LevNr 
                    LevBas.LevNamn WHEN AVAILABLE LevBas @ FILL-IN-LevNamn
                   WITH FRAME Default-Frame.
                  RETURN NO-APPLY.
                END.
              RUN BytLevBestHode (LevBas.LevNr).
              IF RETURN-VALUE = "AVBRYT" THEN
                RETURN NO-APPLY.
              RUN LagreArtBas (0).
              IF RETURN-VALUE = "AVBRYT" THEN
                RETURN NO-APPLY.
              RUN InitDivInfo.
            END.
        END.
    END. /* LEVFIX */
  ELSE IF AVAILABLE LevBas THEN
      RUN InitDivInfo.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SokAnvId C-ArtKort 
PROCEDURE SokAnvId :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME FRAME-ArtInfo:
  /* Start søkeprogram */
  {soek.i
    &Felt        = ArtBas.Anv-Id
    &Program     = d-banvkod.w
    &Frame       = FRAME-ArtInfo
    &PostRun     = "find Anv-Kod no-lock where
                    recid(Anv-Kod) = int(return-value) no-error."
    &OptDisp     = "anv-Kod.Anv-Id when available Anv-Kod @ ArtBas.Anv-Id 
                    Anv-Kod.AnvBeskr when available Anv-Kod"
  }   
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SokBilde C-ArtKort 
PROCEDURE SokBilde :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME DEFAULT-FRAME:
  /* Start søkeprogram */
  {soek.i
    &Felt        = artbas.bildnr
    &Program     = w-bbilder.w
    &Frame       = DEFAULT-FRAME
    &PostRun     = "find Bilderegister no-lock where
                    recid(Bilderegister) = int(return-value) no-error."
    &OptDisp     = "Bilderegister.BildNr when available Bilderegister @ ArtBas.BildNr 
                    BildeRegister.Tekst when available BildeRegister.
                    if available BildeRegister then run VisBilde (1)"
  }   
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SokFarg C-ArtKort 
PROCEDURE SokFarg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME FRAME-ArtInfo:

  /* Start søkeprogram */
  {soek.i
    &Felt        = ArtBas.Farg
    &Program     = d-bfarg.w
    &Frame       = FRAME-ArtInfo
    &PostRun     = "find Farg no-lock where
                    recid(Farg) = int(return-value) no-error."
    &OptDisp     = "Farg.Farg when available Farg @ ArtBas.Farg 
                    Farg.FarBeskr when available Farg"
  }   
  
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SokInnerId C-ArtKort 
PROCEDURE SokInnerId :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME FRAME-ArtInfo:
  /* Start søkeprogram */
  {soek.i
    &Felt        = ArtBas.Inner-Id
    &Program     = d-binnersula.w
    &Frame       = FRAME-ArtInfo
    &PostRun     = "find InnerSula no-lock where
                    recid(InnerSula) = int(return-value) no-error."
    &OptDisp     = "InnerSula.Inner-Id when available InnerSula @ ArtBas.Inner-Id 
                    InnerSula.InnerBeskr when available InnerSula"
  }   
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SokLastId C-ArtKort 
PROCEDURE SokLastId :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME FRAME-ArtInfo:
  /* Start søkeprogram */
  {soek.i
    &Felt        = ArtBas.Last-Id
    &Program     = d-blast.w
    &Frame       = FRAME-ArtInfo
    &PostRun     = "find Last-Sko no-lock where
                    recid(Last-Sko) = int(return-value) no-error."
    &OptDisp     = "Last-Sko.Last-Id   when available Last-Sko @ ArtBas.Last-Id 
                    Last-Sko.LastBeskr when available Last-Sko"
  }   
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SokLevNr C-ArtKort 
PROCEDURE SokLevNr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME DEFAULT-FRAME:
  {soek.i
    &Felt       = ArtBas.LevNr
    &Frame-Name = DEFAULT-FRAME
    &Program    = d-blevbas.w 
    &OptDisp    = "LevBas.LevNr    when available LevBas @ ArtBas.LevNr 
                   LevBas.LevNamn  when available LevBas @ FILL-IN-LevNamn"
    &PostRun    = "find LevBas no-lock where
                     recid(LevBas) = int(return-value) no-error.
                   if available LevBas then
                      find Valuta no-lock where 
                         Valuta.ValKod = LevBas.Valkod no-error."
  }

  IF AVAILABLE LevBas THEN
     RUN InitDivInfo.

  /* Leverandørnummer er ikke endret. */
  IF AVAILABLE ArtBas THEN
  DO: /* Utføres kun ved endring på eksisterende artikkel */ 
    
    IF LevBas.LevNr <> ArtBas.LevNr AND
     wModus <> "NY" THEN
    LEVFIX:
    DO:
      IF CAN-FIND(FIRST BestHode WHERE
                    BestHode.LevNr      = ArtBas.LevNr AND
                    BestHode.ArtikkelNr = ArtBas.ArtikkelNr AND
                    BestHode.BestStat >= 1 AND
                    BestHode.BestStat <= 2) THEN
        DO:
          MESSAGE Tx("Det finnes bestillingsforslag og godkjente bestillinger på denne artikkel." + CHR(13) + 
                  "Ved bytte av leverandør, må det også byttes leverandør på disse." + CHR(13) +
                  "Skal bytte av leverandør utføres?",129) 
                  VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
                  TITLE Tx("Bekreftelse",105)
                  UPDATE wSvar AS LOG.
          IF wSvar <> TRUE THEN
            DO:
              FIND LevBas OF ArtBas NO-LOCK NO-ERROR.
              DISPLAY 
                ArtBas.LevNr 
                LevBas.LevNamn WHEN AVAILABLE LevBas @ FILL-IN-LevNamn
              WITH FRAME Default-Frame.
              RETURN NO-APPLY.
            END.
          RUN BytLevBestHode (LevBas.LevNr).
          IF RETURN-VALUE = "AVBRYT" THEN
            RETURN NO-APPLY.
          RUN LagreArtBas (0).
          IF RETURN-VALUE = "AVBRYT" THEN
            RETURN NO-APPLY.
          RUN InitDivInfo.
        END.
    END. /* LEVFIX */
  END.
  ELSE IF AVAILABLE LevBas THEN
      RUN InitDivInfo.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SokLopNr C-ArtKort 
PROCEDURE SokLopNr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME DEFAULT-FRAME:
  IF wModus = "NY" THEN
    RETURN NO-APPLY.
    
  /* Lagrer eventuelle endringer f›r s›k startes. */
  RUN LagreArtBas (1).
  IF RETURN-VALUE = "AVBRYT" THEN
    RETURN NO-APPLY.

  /* Start søkeprogram */
  {soek.i
    &Felt        = ArtBas.LopNr
    &NoParam     = "No"
    &Program     = w-bartsok.w
    &Frame       = DEFAULT-FRAME
    &PostRun     = "assign
                      wArtBasRecid = int(return-value).
                    find ArtBas no-lock where
                      recid(ArtBas) = wArtBasRecid no-error.
                    assign w-SokFelt = if available ArtBas then ArtBas.LopNr else 0.
                    run RensFrame.
                    run VisArtBas.
                    run VisLager(true)."
  }   
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SokMatKod C-ArtKort 
PROCEDURE SokMatKod :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME FRAME-ArtInfo:

  /* Start søkeprogram */
  {soek.i
    &Felt        = ArtBas.MatKod
    &Program     = d-bmater.w
    &Frame       = FRAME-ArtInfo
    &PostRun     = "find Material no-lock where
                    recid(Material) = int(return-value) no-error."
    &OptDisp     = "Material.MatKod when available Material @ ArtBas.MatKod 
                    Material.MatBeskr when available Material"
  }   
  
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SokOverdel C-ArtKort 
PROCEDURE SokOverdel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME FRAME-ArtInfo:
  /* Start søkeprogram */
  {soek.i
    &Felt        = ArtBas.Ov-Id
    &Program     = d-bovandel.w
    &Frame       = FRAME-ArtInfo
    &PostRun     = "find Ovandel no-lock where
                    recid(Ovandel) = int(return-value) no-error."
    &OptDisp     = "Ovandel.Ov-Id when available Ovandel @ ArtBas.Ov-Id 
                    Ovandel.OvBeskr when available Ovandel"
  }   
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SokProvKod C-ArtKort 
PROCEDURE SokProvKod :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME FRAME-ArtInfo:
  /* Start søkeprogram */
  {soek.i
    &Felt        = ArtBas.Anv-Id
    &Program     = d-bprov.w
    &Frame       = FRAME-ArtInfo
    &PostRun     = "find Prov no-lock where
                    recid(Prov) = int(return-value) no-error."
    &OptDisp     = "Prov.ProvKod when available Prov @ ArtBas.ProvKod 
                    Prov.ProvProc when available Prov"
  }   

END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SokRabKod C-ArtKort 
PROCEDURE SokRabKod :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME FRAME-ArtInfo:
  /* Start søkeprogram */
  {soek.i
    &Felt        = ArtBas.RabKod
    &Program     = d-brabatt.w
    &Frame       = FRAME-ArtInfo
    &PostRun     = "find Rabatt no-lock where
                    recid(Rabatt) = int(return-value) no-error."
    &OptDisp     = "Rabatt.RabKod when available Rabatt @ ArtBas.RabKod 
                    Rabatt.RabProc when available Rabatt"
  }   

END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SokSaSong C-ArtKort 
PROCEDURE SokSaSong :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /* Start søkeprogram */
  {soek.i
    &Felt        = ArtBas.SaSong
    &Program     = d-bsasong.w
    &Frame       = FRAME-ArtInfo
    &PostRun     = "find Sasong no-lock where
                    recid(Sasong) = int(return-value) no-error."
    &OptDisp     = "SaSong.Sasong   when available SaSong @ ArtBas.SaSong 
                    SaSong.SasBeskr when available SaSong
                    with frame FRAME-ArtInfo"
  }   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SokSlitId C-ArtKort 
PROCEDURE SokSlitId :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME FRAME-ArtInfo:
  /* Start søkeprogram */
  {soek.i
    &Felt        = ArtBas.Slit-Id
    &Program     = d-bslitsula.w
    &Frame       = FRAME-ArtInfo
    &PostRun     = "find SlitSula no-lock where
                    recid(SlitSula) = int(return-value) no-error."
    &OptDisp     = "SlitSula.Slit-Id   when available SlitSula @ ArtBas.Slit-Id 
                    SlitSula.SlitBeskr when available SlitSula"
  }   
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SokStrType C-ArtKort 
PROCEDURE SokStrType :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME FRAME-ArtInfo:
  /* Start søkeprogram */
  {soek.i
    &Felt        = ArtBas.StrTypeId
    &Program     = d-bstrtype.w
    &Frame       = FRAME-ArtInfo
    &PostRun     = "find StrType no-lock where
                    recid(StrType) = int(return-value) no-error."
    &OptDisp     = "StrType.StrTypeId when available StrType @ ArtBas.StrType 
                    StrType.Beskrivelse when available StrType"
  }   

END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SokValKod C-ArtKort 
PROCEDURE SokValKod :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME FRAME-ArtInfo:

  /* Start søkeprogram */
  {soek.i
    &Felt        = ArtBas.ValKod
    &Program     = d-bvaluta.w
    &Frame       = FRAME-ArtInfo
    &PostRun     = "find Valuta no-lock where
                    recid(Valuta) = int(return-value) no-error."
    &OptDisp     = "caps(Valuta.ValKod) when available Valuta @ ArtBas.ValKod 
                    Valuta.ValKurs when available Valuta"
  }   

END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SokVg C-ArtKort 
PROCEDURE SokVg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /* Start søkeprogram */
DO WITH FRAME DEFAULT-FRAME:
  DEF VAR cTekst AS CHAR NO-UNDO.
  cTekst = "Vg".
  RUN JBoxDLookup.w ("VarGr;vg;vgBeskr,HuvGr;Hg;HgBeskr,Moms;MomsProc@3,Avdeling;AvdelingNr;AvdelingNavn",
                     "WHERE true,FIRST HuvGr OF VarGr NO-LOCK, FIRST Moms of VarGr, FIRST Avdeling OF HuvGr NO-LOCK",
                     INPUT-OUTPUT cTekst).


  IF RETURN-VALUE = "AVBRYT" THEN
      RETURN NO-APPLY.
  FIND VarGr NO-LOCK WHERE
    VarGr.Vg = INT(cTekst) NO-ERROR.
  IF AVAILABLE VarGr THEN
  DO:
      if available VarGr then
        find HuvGr of VarGr no-lock no-error.
      if available VarGr then
        find Moms of VarGr no-lock no-error.
      /* Legger opp verdier I de aktuelle feltene */
      ASSIGN
        ArtBas.Vg:SCREEN-VALUE       = cTekst
        FILL-IN-VgBeskr:SCREEN-VALUE = VarGr.VgBeskr
        .
      IF AVAILABLE HuvGr THEN
          ASSIGN
            HuvGr.Hg:SCREEN-VALUE      = STRING(HuvGr.Hg).
            HuvGr.HgBeskr:SCREEN-VALUE = HuvGr.HgBeskr
            .
  END.

  IF wModus = "Ny"  AND 
    AVAILABLE VarGr AND
    ArtBas.BongTekst:screen-value IN FRAME FRAME-ArtInfo = "" THEN
      DISPLAY VarGr.VgBeskr @ ArtBas.BongTekst WITH FRAME FRAME-ArtInfo.       
  IF wKopi  AND 
    AVAILABLE VarGr  THEN
      DISPLAY VarGr.VgBeskr @ ArtBas.BongTekst WITH FRAME FRAME-ArtInfo.       
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SokVgKat C-ArtKort 
PROCEDURE SokVgKat :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME DEFAULT-FRAME:
  /* Start søkeprogram */
  {soek.i
    &Felt        = ArtBas.VgKat
    &Program     = d-bvgkat.w
    &Frame       = DEFAULT-FRAME
    &StartVerdi  = "(if wModus = 'Ny' 
                       then input ArtBas.Vg
                       else ArtBas.Vg)"
  }   
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE testclose C-ArtKort 
PROCEDURE testclose :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/ 
    ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
           THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.
    APPLY "WINDOW-CLOSE":U TO THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Toolbar C-ArtKort 
PROCEDURE Toolbar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DO WITH FRAME DEFAULT-FRAME:  
  ASSIGN
    BUTTON-Ny:sensitive     = IF wModus = "Ny" THEN FALSE ELSE TRUE
 /* BUTTON-Lagre:sensitive  = if wModus = "Ny" then false else true */
    BUTTON-Kopier:sensitive = IF wModus = "Ny" THEN FALSE ELSE TRUE
    BUTTON-Slett:sensitive  = IF wModus = "Ny" THEN FALSE ELSE TRUE
 /* BUTTON-Angre:sensitive  = if wModus = "Ny" then false else true */
    BUTTON-Prev:sensitive   = IF wModus = "Ny" THEN FALSE ELSE TRUE
    BUTTON-Next:sensitive   = IF wModus = "Ny" THEN FALSE ELSE TRUE
    BUTTON-Ok:sensitive     = IF wModus = "Ny" THEN FALSE ELSE TRUE.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisArtBas C-ArtKort 
PROCEDURE VisArtBas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR wTilbud AS LOG NO-UNDO.

DO WITH FRAME FRAME-ArtInfo:
  FIND ArtBas NO-LOCK WHERE
    recid(ArtBas) = wArtBasRecid NO-ERROR.
  IF NOT AVAILABLE ArtBas THEN
    RETURN NO-APPLY "AVBRYT".

  /* Det åpnes for tildeling av løpenummer */
/*   if ArtBas.LopNr = 0 or ArtBas.LopNr = ? then                    */
/*      assign                                                       */
/*        BUTTON-SettLopNr:hidden in frame DEFAULT-FRAME    = false  */
/*        BUTTON-SettLopNr:sensitive in frame DEFAULT-FRAME = true   */
/*        B-ByttVareGr:hidden in frame DEFAULT-FRAME    = false      */
/*        B-ByttVareGr:sensitive in frame DEFAULT-FRAME = true.      */
/*   else                                                            */
/*      assign                                                       */
/*        BUTTON-SettLopNr:hidden in frame DEFAULT-FRAME    = true   */
/*        BUTTON-SettLopNr:sensitive in frame DEFAULT-FRAME = false  */
/*        B-ByttVareGr:hidden in frame DEFAULT-FRAME    = true       */
/*        B-ByttVareGr:sensitive in frame DEFAULT-FRAME = false.     */

  FIND FIRST PrisProfil NO-LOCK NO-ERROR.
  IF AVAILABLE PrisProfil THEN
    FIND ArtPRis NO-LOCK WHERE
      ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
      ArtPris.ProfilNr   = PrisProfil.ProfilNr NO-ERROR.
      
  FIND VarGr     OF ArtBAs NO-LOCK NO-ERROR.
  FIND HuvGr     OF ArtBas NO-LOCK NO-ERROR.
  FIND Valuta    OF ArtBas NO-LOCK NO-ERROR.
  FIND Rabatt    OF ArtBas NO-LOCK NO-ERROR.
  FIND Prov      OF ArtBas NO-LOCK NO-ERROR.
  FIND LevBas    OF ArtBas NO-LOCK NO-ERROR.
  FIND Farg      OF ArtBas NO-LOCK NO-ERROR.
  FIND Sasong    OF ArtBas NO-LOCK NO-ERROR.
  FIND Material  OF ArtBas NO-LOCK NO-ERROR.
  FIND Bilderegister OF ArtBas NO-LOCK NO-ERROR.
  FIND Varemerke OF ArtBas NO-LOCK NO-ERROR.
  FIND Produsent OF ArtBas NO-LOCK NO-ERROR.
  FIND StrType   OF ArtBas NO-LOCK NO-ERROR.
  FIND Innersula OF ArtBas NO-LOCK NO-ERROR.
  FIND Anv-Kod   OF ArtBas NO-LOCK NO-ERROR.
  FIND Last-Sko  OF ArtBas NO-LOCK NO-ERROR.
  FIND Slitsula  OF ArtBas NO-LOCK NO-ERROR.
  FIND Ovandel   OF ArtBas NO-LOCK NO-ERROR.
  IF AVAILABLE VarGr THEN
    FIND Moms OF VarGr NO-LOCK NO-ERROR. 

  ASSIGN
    wDataObjekt    = IF AVAILABLE ArtBas 
                       THEN string(ArtBas.ArtikkelNr,"9999999999999")
                       ELSE ""
    TOGGLE-Annonse = IF AVAILABLE ArtBas
                       THEN ArtBas.AnonseArtikkel
                       ELSE FALSE
    T-LapTop       = ArtBas.LapTop.

  
  IF AVAILABLE ArtBas THEN
  DO:
    ASSIGN
      HuvGr.HgBeskr:TOOLTIP IN FRAME DEFAULT-FRAME  = HuvGr.HgBeskr
      ArtBas.Beskr:TOOLTIP    = ArtBas.Beskr
      FILL-IN-VgBeskr:TOOLTIP = (IF AVAILABLE VarGr 
                                  THEN VarGr.VgBeskr 
                                  ELSE "")
      FI-1 = ArtBas.DivInfo[ 1]   
      FI-2 = ArtBas.DivInfo[ 2]   
      FI-3 = ArtBas.DivInfo[ 3]   
      FI-4 = ArtBas.DivInfo[ 4]   
      FI-5 = ArtBas.DivInfo[ 5]   
      FI-6 = ArtBas.DivInfo[ 6]   
      FI-7 = ArtBas.DivInfo[ 7]   
      FI-8 = ArtBas.DivInfo[ 8]   
      T-1  = ArtBas.VisDivInfo[ 1]
      T-2  = ArtBas.VisDivInfo[ 2]
      T-3  = ArtBas.VisDivInfo[ 3]
      T-4  = ArtBas.VisDivInfo[ 4]
      T-5  = ArtBas.VisDivInfo[ 5]
      T-6  = ArtBas.VisDivInfo[ 6]
      T-7  = ArtBas.VisDivInfo[ 7]
      T-8  = ArtBas.VisDivInfo[ 8]
      T-9  = ArtBas.VisDivInfo[ 9]
      T-10 = ArtBas.VisDivInfo[10]
      FI-PaTilbud = (IF ArtBas.SattPaKampanje <> ?
                       THEN Tx("Kampanje:",130) + " " + STRING(ArtBas.SattPaKampanje)
                       ELSE "")
      .
    DISPLAY 
      ArtBas.ArtikkelNr
      ArtBas.Vg 
      ArtBas.LopNr WHEN ArtBas.LopNr <> ?
      "" WHEN ArtBas.LopNr = ? @ ArtBas.LopNr 
      ArtBas.VgKat
      ArtBas.Beskr 
      ArtBas.LevNr 
      ArtBas.LevKod
      ArtBas.Inn_Dato
      ArtBas.LevFargKod
      ArtBas.Utgatt 
      ArtBas.lager 
      ArtBas.Storrelser
      VarGr.VgBeskr WHEN AVAILABLE VarGr @ FILL-IN-VgBeskr
      LevBas.LevNamn WHEN AVAILABLE LevBas @ FILL-IN-LevNamn
      huvgr.hg WHEN AVAILABLE HuvGr
      HuvGr.HgBeskr WHEN AVAILABLE HuvGr
      ArtBas.BildNr
      T-LapTop
      TOGGLE-Annonse
      T-10
      FI-PaTilbud
    WITH FRAME DEFAULT-FRAME.
  END.

  RUN VisEndretinfo.

  IF wAktivFlip = 1 THEN
    OUTPUT to value("nul").
  IF AVAILABLE ArtBas THEN
    DISPLAY
      ArtBas.VMId
      VareMerke.Beskrivelse WHEN AVAILABLE VareMerke
      ArtBas.ProdNr
      Produsent.Beskrivelse WHEN AVAILABLE Produsent
      ArtBas.StrTypeId
      StrType.Beskrivelse WHEN AVAILABLE StrType
      ArtBas.Sasong
      Sasong.SasBeskr WHEN AVAILABLE Sasong
      ArtBas.Farg
      Farg.FarBeskr WHEN AVAILABLE Farg
      ArtBas.MatKod
      Material.MatBeskr WHEN AVAILABLE Material
      ArtBas.Klack
      ArtBas.ProvKod
      Prov.ProvProc WHEN AVAILABLE Prov
      Valuta.ValKurs WHEN AVAILABLE Valuta
      ArtBas.RabKod
      ArtBas.BongTekst
      ArtBas.ValKod
      ArtBas.Anv-Id   Anv-Kod.AnvBeskr      WHEN AVAILABLE Anv-Kod
      ArtBas.Last-Id  Last-Sko.LastBeskr   WHEN AVAILABLE Last-Sko
      ArtBas.Slit-Id  SlitSula.SlitBeskr    WHEN AVAILABLE SlitSula
      ArtBas.Ov-Id    Ovandel.OvBeskr       WHEN AVAILABLE Ovandel
      ArtBas.Inner-Id InnerSula.InnerBeskr WHEN AVAILABLE InnerSula
      ArtBas.Notat
      "" @ FILL-IN-TilbPris
      FI-1
      FI-2
      FI-3
      FI-4
      FI-5
      FI-6
      FI-7
      FI-8
      T-1
      T-2
      T-3
      T-4
      T-5
      T-6
      T-7
      T-8
      T-9
      ArtBas.LevDato1
      ArtBas.LevDato2
    WITH FRAME FRAME-ArtInfo.
  IF wAktivFlip = 1 THEN
    OUTPUT close.
    
    IF AVAILABLE Bilderegister THEN
      DO:
        DISPLAY
          Bilderegister.BildNr @ ArtBas.BildNr 
          BildeRegister.Tekst
        WITH FRAME DEFAULT-FRAME.

        RUN VisBilde (1).
      END.
    ELSE DO:
      DISPLAY 
        "" @ BildeRegister.Tekst
      WITH FRAME DEFAULT-FRAME.

      RUN VisBilde (2).
    END.  
           
  FIND FIRST ArtLag NO-LOCK WHERE
    ArtLAg.artikkelnr = ArtBas.artikkelnr NO-ERROR.
  /* ArtLAg.ArtikkelNr = ArtBas.ArtikkelNr no-error. */
  IF AVAILABLE VarGr THEN
    FIND Moms OF VarGr NO-LOCK NO-ERROR. 

  RUN InitKalkyle.
END. /* FRAME-ArtInfo */    

  {&OPEN-BROWSERS-IN-QUERY-FRAME-Ordre}
  {&OPEN-BROWSERS-IN-QUERY-FRAME-PrisInfo}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisBilde C-ArtKort 
PROCEDURE VisBilde PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT AVAILABLE ArtBas THEN
    RETURN.
DO WITH FRAME DEFAULT-FRAME:  
  {visbilde.i
    &BldOcx = "chIMAGE-Sko"
    &BildNr = "input ArtBas.BildNr"
  }
END.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisEndretInfo C-ArtKort 
PROCEDURE VisEndretInfo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  IF AVAILABLE ArtBAs THEN
    DO:
      ASSIGN
        Fill-In-EndretInfo = Tx("Opprettet",101) + " " + 
                             (IF ArtBas.RegistrertDato <> ? 
                               THEN string(ArtBas.RegistrertDato)
                               ELSE "        ") + " " +
                             (IF ArtBas.RegistrertTid <> 0
                               THEN string(ArtBas.RegistrertTid,"HH:MM:SS")
                               ELSE "        ") + " " + Tx("av",103) + " " + 
                             ArtBas.RegistrertAv + "    " + Tx("Endret",102) + " " +
                             (IF ArtBas.EDato <> ?
                               THEN string(ArtBas.EDato)
                               ELSE "        ") + " " +
                             (IF ArtBas.ETid <> 0
                               THEN string(ArtBas.ETid,"HH:MM:SS")
                               ELSE "        ") + " " + Tx("av",103) + " " +
                             ArtBas.BrukerId.
    END.
  ELSE DO:
    ASSIGN
      FILL-IN-EndretInfo = "".
  END.
  
  DISPLAY 
    FILL-IN-EndretInfo
  WITH FRAME FRAME-ArtInfo.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisLager C-ArtKort 
PROCEDURE VisLager :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER wOverstyr AS LOG  NO-UNDO.
  

  DEF VAR wStrListe AS CHAR NO-UNDO.
  DEF VAR wLoop     AS INT  NO-UNDO.
  DEFINE VARIABLE cBrukteStr AS CHARACTER NO-UNDO.
  
  IF wVisLager <> wArtBasRecid OR
     wOverStyr THEN
    BYGG-LAGER:
    DO:
      
      RUN bygglager.w (INPUT wArtBasRecid, wRS-Vis, OUTPUT wStrListe, OUTPUT cBrukteStr).
      IF RETURN-VALUE = "AVBRYT" THEN
        LEAVE BYGG-LAGER.
      ASSIGN wVisLAger = wArtBasRecid.

      RUN BrowseLabel (wStrListe).
      
    END. /* BYGG-LAGER */
    
  IF wRS-Vis = 1 THEN
    tmpLager.DivAntall:label IN BROWSE BROWSE-Lager = Tx("AntSolgt",131).
  ELSE
    tmpLager.DivAntall:label IN BROWSE BROWSE-Lager = Tx("AntLager",132).  

  {&OPEN-BROWSERS-IN-QUERY-FRAME-Lager}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisTranser C-ArtKort 
PROCEDURE VisTranser :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT AVAILABLE ArtBas THEN
    RETURN NO-APPLY.
  CREATE tmpChild.
  RUN w-barttransloggJFSpec.w PERSISTENT SET tmpChild.wChild (ArtBas.ArtikkelNr).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WinHlp C-ArtKort 
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

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION OrdreNummer C-ArtKort 
FUNCTION OrdreNummer RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR wOrdreNr AS CHAR NO-UNDO.

  IF AVAILABLE BestHode THEN
    DO:
      IF BestHode.OrdreNr = ? THEN
        wOrdreNr = "".
      ELSE IF BestHode.OrdreNr  = 0 THEN
        wOrdreNr = "".
      ELSE
        wOrdreNr = string(BestHode.OrdreNr,"zzzzzz9").
    END.
  ELSE
    wOrdreNr = "".

  RETURN wOrdreNr.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

