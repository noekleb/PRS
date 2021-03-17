&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description:        Container for a JukeBox window program

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author:            brynjar@chemistry.no

  Created:           18.oct.2006

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

DEF VAR bOk         AS LOG    NO-UNDO.
DEF VAR iX          AS INT    NO-UNDO.
DEF VAR iY          AS INT    NO-UNDO.
DEF VAR hBrowse     AS HANDLE NO-UNDO.
DEF VAR hToolbar    AS HANDLE NO-UNDO.

DEF VAR hUpdate     AS HANDLE NO-UNDO.
DEF VAR hAdHoc      AS HANDLE NO-UNDO.
DEF VAR tth         AS HANDLE NO-UNDO.

DEF VAR gsysHId     AS INT INIT 225 NO-UNDO.
DEF VAR gSysGr      AS INT NO-UNDO.

DEF VAR mpXML       AS MEMPTR NO-UNDO.
DEF VAR hTeamCombo  AS HANDLE NO-UNDO.
DEF VAR hSourceBrw  AS HANDLE NO-UNDO.

DEF VAR bButikk     AS LOG NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rectToolbar RECT-1 RECT-2 RECT-3 RECT-4 ~
RECT-5 hjelpetekst btnbutnr btnAvdelingNr btnHg btnVg btnSaSong ~
advRapportNavn btnMatKod advMail btnVMId advSplittRapport advSendMail ~
btnFarg btnLevNr lblAll btnProdNr colLevKod colLevNamn btnRAvdNr ~
colLevFargkod colBeskrivelse colAvdelingnavn colVMid opris colHgBeskr ~
colVMbeskrivelse kjedevare colJamforenhet colVgBeskr gjennomfaktureres ~
colMengde colRAvdNr colStrekkode grunnsortiment sortPhrase1 ~
useBreakBySummary sortPhrase2 useSortDesc sortPhrase3 useDelsummering BtnOK ~
BtnNew 
&Scoped-Define DISPLAYED-OBJECTS sysGr Beskrivelse hjelpetekst butik ~
avdelingnr advFilePath hg advFileExtent Vg stTypeId SaSong advRapportNavn ~
MatKod advMail VMId advSplittRapport advSendMail Farg LevNr lblAll ProdNr ~
colLevKod colLevNamn RAvdNr colLevFargkod colBeskrivelse colAvdelingnavn ~
colVMid opris colHgBeskr colVMbeskrivelse kjedevare colJamforenhet ~
colVgBeskr gjennomfaktureres colMengde colRAvdNr colStrekkode ~
grunnsortiment sortPhrase1 useBreakBySummary sortPhrase2 useSortDesc ~
sortPhrase3 useDelsummering lblButikk lblAdvanced lblAvdeling lblHg lblVg ~
lblSasong lblMatKod lblvmid lblFarg lblLevnr lblKolonnevisning lblProdNr ~
lblRAvdNr lblDelsummering lblButtons 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 advFilePath advFileExtent stTypeId advRapportNavn ~
advMail 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD chkIkkeSendMail C-Win 
FUNCTION chkIkkeSendMail RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD convLogical C-Win 
FUNCTION convLogical RETURNS CHARACTER
  (INPUT ipValue AS CHARACTER,
   INPUT ipLanguage AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD doGetFrameField C-Win 
FUNCTION doGetFrameField RETURNS HANDLE
  ( INPUT ihFrame      AS HANDLE,
    INPUT icFrameField AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD doSendFilePath C-Win 
FUNCTION doSendFilePath RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD initValues C-Win 
FUNCTION initValues RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD receiveParameterTable C-Win 
FUNCTION receiveParameterTable RETURNS HANDLE
  (INPUT iptth AS HANDLE)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD sendParameterTable C-Win 
FUNCTION sendParameterTable RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setSysGr C-Win 
FUNCTION setSysGr RETURNS LOGICAL
  ( INPUT iiSysgr AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD validateFields C-Win 
FUNCTION validateFields RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE MENU POPUP-MENU-advSendMail 
       MENU-ITEM m_Test_mail_ikke_send LABEL "Test mail (ikke send)"
              TOGGLE-BOX.


/* Definitions of the field level widgets                               */
DEFINE BUTTON btnAvdelingNr 
     LABEL "..." 
     SIZE 4 BY 1.

DEFINE BUTTON btnbutnr 
     LABEL "..." 
     SIZE 4 BY 1.

DEFINE BUTTON BtnDelete DEFAULT 
     LABEL "Slett" 
     SIZE 18 BY 1.67
     BGCOLOR 8 .

DEFINE BUTTON btnFarg 
     LABEL "..." 
     SIZE 4 BY 1.

DEFINE BUTTON btnHg 
     LABEL "..." 
     SIZE 4 BY 1.

DEFINE BUTTON btnLevNr 
     LABEL "..." 
     SIZE 4 BY 1.

DEFINE BUTTON btnMatKod 
     LABEL "..." 
     SIZE 4 BY 1.

DEFINE BUTTON BtnNew DEFAULT 
     LABEL "Ny" 
     SIZE 18 BY 1.67
     BGCOLOR 8 .

DEFINE BUTTON BtnOK AUTO-GO DEFAULT 
     LABEL "Ad-Hoc rapport" 
     SIZE 18 BY 1.67
     BGCOLOR 8 .

DEFINE BUTTON btnProdNr 
     LABEL "..." 
     SIZE 4 BY 1.

DEFINE BUTTON btnRAvdNr 
     LABEL "..." 
     SIZE 4 BY 1.

DEFINE BUTTON btnSaSong 
     LABEL "..." 
     SIZE 4 BY 1.

DEFINE BUTTON BtnUpdate DEFAULT 
     LABEL "Oppdater" 
     SIZE 18 BY 1.67
     BGCOLOR 8 .

DEFINE BUTTON btnVg 
     LABEL "..." 
     SIZE 4 BY 1.

DEFINE BUTTON btnVMId 
     LABEL "..." 
     SIZE 4 BY 1.

DEFINE VARIABLE gjennomfaktureres AS CHARACTER FORMAT "X(256)":U INITIAL "Alle" 
     LABEL "Gj.faktureres" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Alle","Ja","Nei" 
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE grunnsortiment AS CHARACTER FORMAT "X(256)":U INITIAL "Alle" 
     LABEL "Grunnsortiment" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Alle","Ja","Nei" 
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE kjedevare AS CHARACTER FORMAT "X(256)":U INITIAL "Alle" 
     LABEL "Kjedevare" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Alle","Ja","Nei" 
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE opris AS CHARACTER FORMAT "X(256)":U INITIAL "Alle" 
     LABEL "Åpen pris" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Alle","Ja","Nei" 
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE sortPhrase1 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Sortering 1" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE sortPhrase2 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Sortering 2" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE sortPhrase3 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Sortering 1" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE avdelingnr AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP LARGE
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE butik AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP LARGE
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE Farg AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP LARGE
     SIZE 22 BY 1 TOOLTIP "Fargekode".

DEFINE VARIABLE hg AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP LARGE
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE hjelpetekst AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 70 BY 3.33 NO-UNDO.

DEFINE VARIABLE LevNr AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP LARGE
     SIZE 22 BY 1 TOOLTIP "Leverandørnummer".

DEFINE VARIABLE MatKod AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP LARGE
     SIZE 22 BY 1 TOOLTIP "Materialkode".

DEFINE VARIABLE ProdNr AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP LARGE
     SIZE 22 BY 1 TOOLTIP "Leverandørnummer".

DEFINE VARIABLE RAvdNr AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP LARGE
     SIZE 22 BY 1 TOOLTIP "Leverandørnummer".

DEFINE VARIABLE SaSong AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP LARGE
     SIZE 22 BY 1 TOOLTIP "Sesong".

DEFINE VARIABLE Vg AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP LARGE
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE VMId AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP LARGE
     SIZE 22 BY 1 TOOLTIP "Varemerke (~"Brand~").".

DEFINE VARIABLE advFileExtent AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fil type" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE advFilePath AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fil sti" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE advMail AS CHARACTER FORMAT "X(256)":U 
     LABEL "ePost" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 TOOLTIP "Send hovedrapport til angitte adresse(r)" NO-UNDO.

DEFINE VARIABLE advRapportNavn AS CHARACTER FORMAT "X(256)":U 
     LABEL "Rapport navn" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE Beskrivelse AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 62.6 BY 1 NO-UNDO.

DEFINE VARIABLE lblAdvanced AS CHARACTER FORMAT "X(256)":U INITIAL "Avanserte parametre" 
      VIEW-AS TEXT 
     SIZE 21 BY .62 NO-UNDO.

DEFINE VARIABLE lblAvdeling AS CHARACTER FORMAT "X(256)":U INITIAL "Avdeling:" 
      VIEW-AS TEXT 
     SIZE 9 BY .62 NO-UNDO.

DEFINE VARIABLE lblButikk AS CHARACTER FORMAT "X(256)":U INITIAL "Butikk:" 
      VIEW-AS TEXT 
     SIZE 7 BY .62 NO-UNDO.

DEFINE VARIABLE lblButtons AS CHARACTER FORMAT "X(256)":U INITIAL "Faste rapporter" 
      VIEW-AS TEXT 
     SIZE 15 BY .62 NO-UNDO.

DEFINE VARIABLE lblDelsummering AS CHARACTER FORMAT "X(256)":U INITIAL "Delsummering" 
      VIEW-AS TEXT 
     SIZE 15 BY .62 NO-UNDO.

DEFINE VARIABLE lblFarg AS CHARACTER FORMAT "X(256)":U INITIAL "Farge:" 
      VIEW-AS TEXT 
     SIZE 6 BY .62 NO-UNDO.

DEFINE VARIABLE lblHg AS CHARACTER FORMAT "X(256)":U INITIAL "Hovedgruppe:" 
      VIEW-AS TEXT 
     SIZE 14 BY .62 NO-UNDO.

DEFINE VARIABLE lblKolonnevisning AS CHARACTER FORMAT "X(256)":U INITIAL "Kolonnevisning" 
      VIEW-AS TEXT 
     SIZE 16 BY .62 NO-UNDO.

DEFINE VARIABLE lblLevnr AS CHARACTER FORMAT "X(256)":U INITIAL "Leverandør:" 
      VIEW-AS TEXT 
     SIZE 12 BY .62 NO-UNDO.

DEFINE VARIABLE lblMatKod AS CHARACTER FORMAT "X(256)":U INITIAL "Matrial kode:" 
      VIEW-AS TEXT 
     SIZE 12 BY .62 NO-UNDO.

DEFINE VARIABLE lblProdNr AS CHARACTER FORMAT "X(256)":U INITIAL "Produsent:" 
      VIEW-AS TEXT 
     SIZE 11 BY .62 NO-UNDO.

DEFINE VARIABLE lblRAvdNr AS CHARACTER FORMAT "X(256)":U INITIAL "Vareområde:" 
      VIEW-AS TEXT 
     SIZE 13 BY .62 NO-UNDO.

DEFINE VARIABLE lblSasong AS CHARACTER FORMAT "X(256)":U INITIAL "Sesong:" 
      VIEW-AS TEXT 
     SIZE 8 BY .62 NO-UNDO.

DEFINE VARIABLE lblVg AS CHARACTER FORMAT "X(256)":U INITIAL "Varegruppe:" 
      VIEW-AS TEXT 
     SIZE 12 BY .62 NO-UNDO.

DEFINE VARIABLE lblvmid AS CHARACTER FORMAT "X(256)":U INITIAL "Varemerke:" 
      VIEW-AS TEXT 
     SIZE 11 BY .62 NO-UNDO.

DEFINE VARIABLE stTypeId AS CHARACTER FORMAT "X(256)":U 
     LABEL "Rapport type" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE sysGr AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "RapportId" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL   
     SIZE 56.4 BY 2.62.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 44 BY 7.38.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 44 BY 6.86.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 76 BY 2.62.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 76 BY 2.14.

DEFINE RECTANGLE rectToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 18 BY 1.19.

DEFINE VARIABLE advSendMail AS LOGICAL INITIAL no 
     LABEL "Send ePost" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 TOOLTIP "Send splittrapport som epost til butikk ansvarlig" NO-UNDO.

DEFINE VARIABLE advSplittRapport AS LOGICAL INITIAL no 
     LABEL "Splitt rapport" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 TOOLTIP "Generer pr. butikk og send mail" NO-UNDO.

DEFINE VARIABLE colAvdelingnavn AS LOGICAL INITIAL no 
     LABEL "Avd.tekst" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE colBeskrivelse AS LOGICAL INITIAL no 
     LABEL "Produsent" 
     VIEW-AS TOGGLE-BOX
     SIZE 14.6 BY .81 NO-UNDO.

DEFINE VARIABLE colHgBeskr AS LOGICAL INITIAL no 
     LABEL "Hg.tekst" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE colJamforenhet AS LOGICAL INITIAL no 
     LABEL "Jamførenhet" 
     VIEW-AS TOGGLE-BOX
     SIZE 14.6 BY .81 NO-UNDO.

DEFINE VARIABLE colLevFargkod AS LOGICAL INITIAL no 
     LABEL "Lev.fargekode" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY .81 NO-UNDO.

DEFINE VARIABLE colLevKod AS LOGICAL INITIAL no 
     LABEL "Lev.art.nr" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE colLevNamn AS LOGICAL INITIAL no 
     LABEL "Lev.navn" 
     VIEW-AS TOGGLE-BOX
     SIZE 14.6 BY .81 NO-UNDO.

DEFINE VARIABLE colMengde AS LOGICAL INITIAL no 
     LABEL "Mengde" 
     VIEW-AS TOGGLE-BOX
     SIZE 14.6 BY .81 NO-UNDO.

DEFINE VARIABLE colRAvdNr AS LOGICAL INITIAL no 
     LABEL "Vareområde" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .81 NO-UNDO.

DEFINE VARIABLE colStrekkode AS LOGICAL INITIAL no 
     LABEL "EAN koder" 
     VIEW-AS TOGGLE-BOX
     SIZE 14.6 BY .81 NO-UNDO.

DEFINE VARIABLE colVgBeskr AS LOGICAL INITIAL no 
     LABEL "Vg.tekst" 
     VIEW-AS TOGGLE-BOX
     SIZE 14.6 BY .81 NO-UNDO.

DEFINE VARIABLE colVMbeskrivelse AS LOGICAL INITIAL no 
     LABEL "Varemerke" 
     VIEW-AS TOGGLE-BOX
     SIZE 14.6 BY .81 NO-UNDO.

DEFINE VARIABLE colVMid AS LOGICAL INITIAL no 
     LABEL "VMid" 
     VIEW-AS TOGGLE-BOX
     SIZE 14.6 BY .81 NO-UNDO.

DEFINE VARIABLE lblAll AS LOGICAL INITIAL no 
     LABEL "Merk alle" 
     VIEW-AS TOGGLE-BOX
     SIZE 14 BY .81 NO-UNDO.

DEFINE VARIABLE useBreakBySummary AS LOGICAL INITIAL no 
     LABEL "Vis delsum" 
     VIEW-AS TOGGLE-BOX
     SIZE 14.6 BY .81 NO-UNDO.

DEFINE VARIABLE useDelsummering AS LOGICAL INITIAL no 
     LABEL "Vis delsummerings flik" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY .81 NO-UNDO.

DEFINE VARIABLE useSortDesc AS LOGICAL INITIAL no 
     LABEL "Sorter synkende" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     sysGr AT ROW 2.67 COL 16 COLON-ALIGNED
     Beskrivelse AT ROW 2.67 COL 23.4 COLON-ALIGNED NO-LABEL
     hjelpetekst AT ROW 3.86 COL 18 NO-LABEL
     butik AT ROW 7.43 COL 18 NO-LABEL
     btnbutnr AT ROW 7.43 COL 39.8 NO-TAB-STOP 
     avdelingnr AT ROW 8.43 COL 18 NO-LABEL
     btnAvdelingNr AT ROW 8.43 COL 39.8 NO-TAB-STOP 
     advFilePath AT ROW 9.1 COL 64 COLON-ALIGNED
     hg AT ROW 9.43 COL 18 NO-LABEL
     btnHg AT ROW 9.43 COL 39.8 NO-TAB-STOP 
     advFileExtent AT ROW 10.05 COL 64 COLON-ALIGNED
     Vg AT ROW 10.43 COL 18 NO-LABEL
     btnVg AT ROW 10.43 COL 39.8 NO-TAB-STOP 
     stTypeId AT ROW 11 COL 64 COLON-ALIGNED
     SaSong AT ROW 11.43 COL 18 NO-LABEL
     btnSaSong AT ROW 11.43 COL 39.8 NO-TAB-STOP 
     advRapportNavn AT ROW 11.95 COL 64 COLON-ALIGNED
     MatKod AT ROW 12.43 COL 18 NO-LABEL
     btnMatKod AT ROW 12.43 COL 39.8 NO-TAB-STOP 
     advMail AT ROW 12.91 COL 64 COLON-ALIGNED
     VMId AT ROW 13.43 COL 18 NO-LABEL
     btnVMId AT ROW 13.43 COL 39.8 NO-TAB-STOP 
     advSplittRapport AT ROW 14.29 COL 53
     advSendMail AT ROW 14.29 COL 73
     Farg AT ROW 14.43 COL 18 NO-LABEL
     btnFarg AT ROW 14.43 COL 39.8 NO-TAB-STOP 
     LevNr AT ROW 15.43 COL 18 NO-LABEL
     btnLevNr AT ROW 15.48 COL 39.8 NO-TAB-STOP 
     lblAll AT ROW 16.05 COL 79
     ProdNr AT ROW 16.43 COL 18 NO-LABEL
     btnProdNr AT ROW 16.48 COL 39.8 NO-TAB-STOP 
     colLevKod AT ROW 17 COL 53
     colLevNamn AT ROW 17 COL 73
     RAvdNr AT ROW 17.43 COL 18 NO-LABEL
     btnRAvdNr AT ROW 17.48 COL 39.8 NO-TAB-STOP 
     colLevFargkod AT ROW 17.71 COL 53
     colBeskrivelse AT ROW 17.71 COL 73
     colAvdelingnavn AT ROW 18.43 COL 53
     colVMid AT ROW 18.43 COL 73
     opris AT ROW 18.62 COL 16 COLON-ALIGNED
     colHgBeskr AT ROW 19.14 COL 53
     colVMbeskrivelse AT ROW 19.14 COL 73
     kjedevare AT ROW 19.57 COL 16 COLON-ALIGNED
     colJamforenhet AT ROW 19.81 COL 73 WIDGET-ID 4
     colVgBeskr AT ROW 19.86 COL 53
     gjennomfaktureres AT ROW 20.52 COL 16 COLON-ALIGNED
     colMengde AT ROW 20.52 COL 73 WIDGET-ID 6
     colRAvdNr AT ROW 20.62 COL 53
     colStrekkode AT ROW 21.24 COL 73
     grunnsortiment AT ROW 21.52 COL 16 COLON-ALIGNED
     sortPhrase1 AT ROW 23.14 COL 33 COLON-ALIGNED
     useBreakBySummary AT ROW 23.14 COL 59
     sortPhrase2 AT ROW 24.1 COL 33 COLON-ALIGNED
     useSortDesc AT ROW 24.1 COL 59
     sortPhrase3 AT ROW 26.48 COL 33 COLON-ALIGNED WIDGET-ID 2
     useDelsummering AT ROW 26.48 COL 59 WIDGET-ID 8
     BtnOK AT ROW 30.19 COL 20
     BtnNew AT ROW 30.19 COL 40
     BtnUpdate AT ROW 30.19 COL 58
     BtnDelete AT ROW 30.19 COL 76
     lblButikk AT ROW 7.67 COL 8 COLON-ALIGNED NO-LABEL
     lblAdvanced AT ROW 7.91 COL 50 COLON-ALIGNED NO-LABEL
     lblAvdeling AT ROW 8.62 COL 6 COLON-ALIGNED NO-LABEL
     lblHg AT ROW 9.57 COL 1 COLON-ALIGNED NO-LABEL
     lblVg AT ROW 10.67 COL 3 COLON-ALIGNED NO-LABEL
     lblSasong AT ROW 11.71 COL 7 COLON-ALIGNED NO-LABEL
     lblMatKod AT ROW 12.67 COL 3 COLON-ALIGNED NO-LABEL
     lblvmid AT ROW 13.62 COL 4 COLON-ALIGNED NO-LABEL
     lblFarg AT ROW 14.57 COL 9 COLON-ALIGNED NO-LABEL
     lblLevnr AT ROW 15.52 COL 4 COLON-ALIGNED NO-LABEL
     lblKolonnevisning AT ROW 15.81 COL 50 COLON-ALIGNED NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 108.6 BY 33.29.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME DEFAULT-FRAME
     lblProdNr AT ROW 16.48 COL 5 COLON-ALIGNED NO-LABEL
     lblRAvdNr AT ROW 17.43 COL 3 COLON-ALIGNED NO-LABEL
     lblDelsummering AT ROW 25.76 COL 17 COLON-ALIGNED NO-LABEL
     lblButtons AT ROW 29.33 COL 37.6 COLON-ALIGNED NO-LABEL
     rectToolbar AT ROW 1.24 COL 2
     RECT-1 AT ROW 29.71 COL 38.6
     RECT-2 AT ROW 8.14 COL 50
     RECT-3 AT ROW 15.81 COL 50
     RECT-4 AT ROW 22.91 COL 18
     RECT-5 AT ROW 26 COL 17 WIDGET-ID 10
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 108.6 BY 33.29.


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
         TITLE              = "ABC rapporter"
         HEIGHT             = 33.29
         WIDTH              = 108.6
         MAX-HEIGHT         = 46.1
         MAX-WIDTH          = 336
         VIRTUAL-HEIGHT     = 46.1
         VIRTUAL-WIDTH      = 336
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

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT C-Win:LOAD-ICON("ico/app16.ico":U) THEN
    MESSAGE "Unable to load icon: ico/app16.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
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
/* SETTINGS FOR FILL-IN advFileExtent IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN advFilePath IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN advMail IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR FILL-IN advRapportNavn IN FRAME DEFAULT-FRAME
   1                                                                    */
ASSIGN 
       advRapportNavn:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       advSendMail:POPUP-MENU IN FRAME DEFAULT-FRAME       = MENU POPUP-MENU-advSendMail:HANDLE.

/* SETTINGS FOR EDITOR avdelingnr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Beskrivelse IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       Beskrivelse:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR BUTTON BtnDelete IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BtnUpdate IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR EDITOR butik IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR EDITOR Farg IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR EDITOR hg IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       hjelpetekst:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN lblAdvanced IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       lblAdvanced:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN lblAvdeling IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       lblAvdeling:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN lblButikk IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       lblButikk:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN lblButtons IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       lblButtons:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN lblDelsummering IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       lblDelsummering:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN lblFarg IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       lblFarg:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN lblHg IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       lblHg:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN lblKolonnevisning IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       lblKolonnevisning:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN lblLevnr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       lblLevnr:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN lblMatKod IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       lblMatKod:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN lblProdNr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       lblProdNr:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN lblRAvdNr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       lblRAvdNr:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN lblSasong IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       lblSasong:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN lblVg IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       lblVg:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN lblvmid IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       lblvmid:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR EDITOR LevNr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR EDITOR MatKod IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR EDITOR ProdNr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR EDITOR RAvdNr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR EDITOR SaSong IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN stTypeId IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN sysGr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR EDITOR Vg IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR EDITOR VMId IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* ABC rapporter */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* ABC rapporter */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME advSplittRapport
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL advSplittRapport C-Win
ON VALUE-CHANGED OF advSplittRapport IN FRAME DEFAULT-FRAME /* Splitt rapport */
DO:
  ASSIGN 
    advSendMail:SENSITIVE = SELF:CHECKED
  .
  IF NOT SELF:CHECKED THEN advSendMail:CHECKED = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAvdelingNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAvdelingNr C-Win
ON CHOOSE OF btnAvdelingNr IN FRAME DEFAULT-FRAME /* ... */
DO:
  DEF VAR cRowIdList  AS CHAR NO-UNDO.
  DEF VAR cIdList     AS CHAR NO-UNDO.  
  DEF VAR bOk         AS LOG  NO-UNDO.
  DEF VAR cTmp        AS CHAR NO-UNDO.

  cTmp = REPLACE(avdelingnr:SCREEN-VALUE,'|',',').
  IF avdelingnr:SCREEN-VALUE NE '' THEN
    cRowIdList = DYNAMIC-FUNCTION("getRowIdList",
                                  "Avdeling",       /* Buffer(list) for query */
                                  "",              /* Name of buffer to fetch rowid from. Blank: Last buffer in bufferlist) */
                                  "WHERE CAN-DO(" + QUOTER(cTmp) + ",STRING(Avdelingnr))").
  
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "Avdeling"      
                      + ";avdelingnr"  
                      + ";AvdelingNavn"
                      ,"where true",
                      INPUT-OUTPUT cRowIdList,
                      "AvdelingNr", /* Primary key */
                      INPUT-OUTPUT cIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk THEN
  DO:
    ASSIGN 
      Avdelingnr:SCREEN-VALUE = cIdList
      Hg:SCREEN-VALUE         = ''
      Vg:SCREEN-VALUE         = ''
    .
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnbutnr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnbutnr C-Win
ON CHOOSE OF btnbutnr IN FRAME DEFAULT-FRAME /* ... */
DO:
  DEF VAR cRowIdList  AS CHAR NO-UNDO.
  DEF VAR cIdList     AS CHAR NO-UNDO.  
  DEF VAR bOk         AS LOG  NO-UNDO.

  DEF VAR cTmp        AS CHAR NO-UNDO.

  ASSIGN 
    bButikk = TRUE
    cTmp = REPLACE(butik:SCREEN-VALUE,'|',',').
  IF butik:SCREEN-VALUE NE '' THEN
    cRowIdList = DYNAMIC-FUNCTION("getRowIdList",
                                  "Butiker",       /* Buffer(list) for query */
                                  "",              /* Name of buffer to fetch rowid from. Blank: Last buffer in bufferlist) */
                                  "WHERE CAN-DO(" + QUOTER(cTmp) + ",STRING(butik))").

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "Butiker;Butik;ButNamn;+!TeamList|character|x(255)|butikkteam_all.p(rowid)"
                      ,"where true",
                      INPUT-OUTPUT cRowIdList,
                      "butik", /* Primary key */
                      INPUT-OUTPUT cIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk THEN
    butik:SCREEN-VALUE = cIdList.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnDelete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDelete C-Win
ON CHOOSE OF BtnDelete IN FRAME DEFAULT-FRAME /* Slett */
DO:
  RUN updateSysGruppe.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnFarg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFarg C-Win
ON CHOOSE OF btnFarg IN FRAME DEFAULT-FRAME /* ... */
DO:
  DEF VAR cRowIdList  AS CHAR NO-UNDO.
  DEF VAR cIdList     AS CHAR NO-UNDO.  
  DEF VAR bOk         AS LOG  NO-UNDO.

  DEF VAR cTmp        AS CHAR NO-UNDO.

  cTmp = REPLACE(Farg:SCREEN-VALUE,'|',',').
  IF Farg:SCREEN-VALUE NE '' THEN
    cRowIdList = DYNAMIC-FUNCTION("getRowIdList",
                                  "Farg",       /* Buffer(list) for query */
                                  "",              /* Name of buffer to fetch rowid from. Blank: Last buffer in bufferlist) */
                                  "WHERE CAN-DO(" + QUOTER(cTmp) + ",STRING(Farg))").

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "Farg"      
                      + ";Farg"  
                      + ";FarBeskr"
                      ,"where true",
                      INPUT-OUTPUT cRowIdList,
                      "Farg", /* Primary key */
                      INPUT-OUTPUT cIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk THEN
  DO:
    ASSIGN 
      Farg:SCREEN-VALUE = cIdList
    .
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnHg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnHg C-Win
ON CHOOSE OF btnHg IN FRAME DEFAULT-FRAME /* ... */
DO:
  DEF VAR cRowIdList  AS CHAR NO-UNDO.
  DEF VAR cIdList     AS CHAR NO-UNDO.  
  DEF VAR bOk         AS LOG  NO-UNDO.
  DEF VAR cTmp        AS CHAR NO-UNDO.

  cTmp = REPLACE(Hg:SCREEN-VALUE,'|',',').
  IF Hg:SCREEN-VALUE NE '' THEN
    cRowIdList = DYNAMIC-FUNCTION("getRowIdList",
                                  "VarGr",       /* Buffer(list) for query */
                                  "",              /* Name of buffer to fetch rowid from. Blank: Last buffer in bufferlist) */
                                  "WHERE CAN-DO(" + QUOTER(cTmp) + ",STRING(Hg))").
  
  cTmp = REPLACE(avdelingnr:SCREEN-VALUE,'|',',').
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "HuvGr"      
                      + ";Hg"  
                      + ";AvdelingNr"
                      + ";HgBeskr"
                      ,IF Avdelingnr:SCREEN-VALUE NE '' THEN "where can-do(" + QUOTER(cTmp) + ',string(Avdelingnr))' ELSE "where true",
                      INPUT-OUTPUT cRowIdList,
                      "Hg", /* Primary key */
                      INPUT-OUTPUT cIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk THEN
  DO:
    ASSIGN 
      Hg:SCREEN-VALUE = cIdList
      Vg:SCREEN-VALUE = ''
    .
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLevNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLevNr C-Win
ON CHOOSE OF btnLevNr IN FRAME DEFAULT-FRAME /* ... */
DO:
  DEF VAR cRowIdList  AS CHAR NO-UNDO.
  DEF VAR cIdList     AS CHAR NO-UNDO.  
  DEF VAR bOk         AS LOG  NO-UNDO.

  DEF VAR cTmp        AS CHAR NO-UNDO.

  cTmp = REPLACE(LevNr:SCREEN-VALUE,'|',',').
  IF LevNr:SCREEN-VALUE NE '' THEN
    cRowIdList = DYNAMIC-FUNCTION("getRowIdList",
                                  "LevBas",       /* Buffer(list) for query */
                                  "",              /* Name of buffer to fetch rowid from. Blank: Last buffer in bufferlist) */
                                  "WHERE CAN-DO(" + QUOTER(cTmp) + ",STRING(LevNr))").

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "LevBas"      
                      + ";LevNr"  
                      + ";LevNamn"
                      ,"where true",
                      INPUT-OUTPUT cRowIdList,
                      "LevNr", /* Primary key */
                      INPUT-OUTPUT cIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk THEN
  DO:
    ASSIGN 
      LevNr:SCREEN-VALUE = cIdList
    .
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnMatKod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnMatKod C-Win
ON CHOOSE OF btnMatKod IN FRAME DEFAULT-FRAME /* ... */
DO:
  DEF VAR cRowIdList  AS CHAR NO-UNDO.
  DEF VAR cIdList     AS CHAR NO-UNDO.  
  DEF VAR bOk         AS LOG  NO-UNDO.
  DEF VAR cTmp        AS CHAR NO-UNDO.

  cTmp = REPLACE(MatKod:SCREEN-VALUE,'|',',').
  IF MatKod:SCREEN-VALUE NE '' THEN
    cRowIdList = DYNAMIC-FUNCTION("getRowIdList",
                                  "Material",       /* Buffer(list) for query */
                                  "",              /* Name of buffer to fetch rowid from. Blank: Last buffer in bufferlist) */
                                  "WHERE CAN-DO(" + QUOTER(cTmp) + ",STRING(MatKod))").

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "Material"      
                      + ";MatKod"  
                      + ";MatBeskr"
                      ,"where true",
                      INPUT-OUTPUT cRowIdList,
                      "MatKod", /* Primary key */
                      INPUT-OUTPUT cIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk THEN
  DO:
    ASSIGN 
      MatKod:SCREEN-VALUE = cIdList
    .
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnNew
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnNew C-Win
ON CHOOSE OF BtnNew IN FRAME DEFAULT-FRAME /* Ny */
DO:
  RUN updateSysGruppe.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnOK C-Win
ON CHOOSE OF BtnOK IN FRAME DEFAULT-FRAME /* Ad-Hoc rapport */
DO:
  validateFields().
  RUN writeAdHoc.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnProdNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnProdNr C-Win
ON CHOOSE OF btnProdNr IN FRAME DEFAULT-FRAME /* ... */
DO:
  DEF VAR cRowIdList  AS CHAR NO-UNDO.
  DEF VAR cIdList     AS CHAR NO-UNDO.  
  DEF VAR bOk         AS LOG  NO-UNDO.

  DEF VAR cTmp        AS CHAR NO-UNDO.

  cTmp = REPLACE(ProdNr:SCREEN-VALUE,'|',',').
  IF ProdNr:SCREEN-VALUE NE '' THEN
    cRowIdList = DYNAMIC-FUNCTION("getRowIdList",
                                  "Produsent",       /* Buffer(list) for query */
                                  "",              /* Name of buffer to fetch rowid from. Blank: Last buffer in bufferlist) */
                                  "WHERE CAN-DO(" + QUOTER(cTmp) + ",STRING(ProdNr))").

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "Produsent"      
                      + ";ProdNr"  
                      + ";Beskrivelse"
                      ,"where true",
                      INPUT-OUTPUT cRowIdList,
                      "ProdNr", /* Primary key */
                      INPUT-OUTPUT cIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk THEN
  DO:
    ASSIGN 
      ProdNr:SCREEN-VALUE = cIdList
    .
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRAvdNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRAvdNr C-Win
ON CHOOSE OF btnRAvdNr IN FRAME DEFAULT-FRAME /* ... */
DO:
  DEF VAR cRowIdList  AS CHAR NO-UNDO.
  DEF VAR cIdList     AS CHAR NO-UNDO.  
  DEF VAR bOk         AS LOG  NO-UNDO.

  DEF VAR cTmp        AS CHAR NO-UNDO.

  cTmp = REPLACE(RAvdNr:SCREEN-VALUE,'|',',').
  IF RAvdNr:SCREEN-VALUE NE '' THEN
    cRowIdList = DYNAMIC-FUNCTION("getRowIdList",
                                  "Regnskapsavdeling",       /* Buffer(list) for query */
                                  "",              /* Name of buffer to fetch rowid from. Blank: Last buffer in bufferlist) */
                                  "WHERE CAN-DO(" + QUOTER(cTmp) + ",STRING(RAvdNr))").

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "Regnskapsavdeling"      
                      + ";RAvdNr"  
                      + ";RAvdBeskrivelse"
                      ,"where true",
                      INPUT-OUTPUT cRowIdList,
                      "RAvdNr", /* Primary key */
                      INPUT-OUTPUT cIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk THEN
  DO:
    ASSIGN 
      RAvdNr:SCREEN-VALUE = cIdList
    .
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSaSong
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSaSong C-Win
ON CHOOSE OF btnSaSong IN FRAME DEFAULT-FRAME /* ... */
DO:
  DEF VAR cRowIdList  AS CHAR NO-UNDO.
  DEF VAR cIdList     AS CHAR NO-UNDO.  
  DEF VAR bOk         AS LOG  NO-UNDO.
  DEF VAR cTmp        AS CHAR NO-UNDO.

  cTmp = REPLACE(SaSong:SCREEN-VALUE,'|',',').
  IF SaSong:SCREEN-VALUE NE '' THEN
    cRowIdList = DYNAMIC-FUNCTION("getRowIdList",
                                  "SaSong",       /* Buffer(list) for query */
                                  "",              /* Name of buffer to fetch rowid from. Blank: Last buffer in bufferlist) */
                                  "WHERE CAN-DO(" + QUOTER(cTmp) + ",STRING(SaSong))").

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "SaSong"      
                      + ";SaSong"  
                      + ";SasBeskr"
                      ,"where true",
                      INPUT-OUTPUT cRowIdList,
                      "SaSong", /* Primary key */
                      INPUT-OUTPUT cIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk THEN
  DO:
    ASSIGN 
      sasong:SCREEN-VALUE = cIdList
    .
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnUpdate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnUpdate C-Win
ON CHOOSE OF BtnUpdate IN FRAME DEFAULT-FRAME /* Oppdater */
DO:
  RUN updateSysGruppe.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnVg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnVg C-Win
ON CHOOSE OF btnVg IN FRAME DEFAULT-FRAME /* ... */
DO:
  DEF VAR cRowIdList  AS CHAR NO-UNDO.
  DEF VAR cIdList     AS CHAR NO-UNDO.  
  DEF VAR bOk         AS LOG  NO-UNDO.
  DEF VAR cTmp        AS CHAR NO-UNDO.
  DEF VAR cTmp2       AS CHAR NO-UNDO.
  DEF VAR cWhere      AS CHAR NO-UNDO.


  cTmp = REPLACE(Vg:SCREEN-VALUE,'|',',').
  IF Vg:SCREEN-VALUE NE '' THEN
    cRowIdList = DYNAMIC-FUNCTION("getRowIdList",
                                  "VarGr",       /* Buffer(list) for query */
                                  "",              /* Name of buffer to fetch rowid from. Blank: Last buffer in bufferlist) */
                                  "WHERE CAN-DO(" + QUOTER(cTmp) + ",STRING(Vg))").
  cTmp  = REPLACE(Hg:SCREEN-VALUE,'|',',').
  cTmp2 = REPLACE(AvdelingNr:SCREEN-VALUE,'|',',').
  IF cTmp = '' AND cTmp2 NE '' THEN 
    cTmp2 = REPLACE(DYNAMIC-FUNCTION("getFieldList","HuvGr;Hg","where can-do(" + QUOTER(cTmp2) + ",string(AvdelingNr))"),'|',',').
  
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "VarGr"      
                      + ";Vg"  
                      + ";VgBeskr"
                      , IF Hg:SCREEN-VALUE NE '' THEN "where can-do(" + QUOTER(cTmp) + ",string(Hg))"
                        ELSE IF AvdelingNr:SCREEN-VALUE NE '' THEN "where can-do(" + QUOTER(cTmp2) + ',string(Hg))'
                        ELSE "where true"
                      ,INPUT-OUTPUT cRowIdList,
                      "Vg", /* Primary key */
                      INPUT-OUTPUT cIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk THEN
  DO:
    ASSIGN 
      Vg:SCREEN-VALUE = cIdList
    .
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnVMId
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnVMId C-Win
ON CHOOSE OF btnVMId IN FRAME DEFAULT-FRAME /* ... */
DO:
  DEF VAR cRowIdList  AS CHAR NO-UNDO.
  DEF VAR cIdList     AS CHAR NO-UNDO.  
  DEF VAR bOk         AS LOG  NO-UNDO.

  DEF VAR cTmp        AS CHAR NO-UNDO.

  cTmp = REPLACE(VMId:SCREEN-VALUE,'|',',').
  IF VMId:SCREEN-VALUE NE '' THEN
    cRowIdList = DYNAMIC-FUNCTION("getRowIdList",
                                  "Varemerke",       /* Buffer(list) for query */
                                  "",              /* Name of buffer to fetch rowid from. Blank: Last buffer in bufferlist) */
                                  "WHERE CAN-DO(" + QUOTER(cTmp) + ",STRING(VMId))").

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "Varemerke"      
                      + ";VMId"  
                      + ";Kortnavn"
                      + ";Beskrivelse"
                      ,"where true",
                      INPUT-OUTPUT cRowIdList,
                      "VMId", /* Primary key */
                      INPUT-OUTPUT cIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk THEN
  DO:
    ASSIGN 
      VMid:SCREEN-VALUE = cIdList
    .
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lblAll
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lblAll C-Win
ON VALUE-CHANGED OF lblAll IN FRAME DEFAULT-FRAME /* Merk alle */
DO:
  DEF VAR hObj AS HANDLE NO-UNDO.
  
  ASSIGN 
    hObj          = FRAME {&FRAME-NAME}:HANDLE:FIRST-CHILD
    hObj          = hObj:FIRST-CHILD
  .
  DO WHILE VALID-HANDLE(hObj):
    IF hObj:TYPE = 'TOGGLE-BOX' AND hObj:NAME BEGINS 'col' THEN
    DO:
      hObj:CHECKED = SELF:CHECKED.
    END.
    hObj =  hObj:NEXT-SIBLING.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sortPhrase1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sortPhrase1 C-Win
ON VALUE-CHANGED OF sortPhrase1 IN FRAME DEFAULT-FRAME /* Sortering 1 */
DO:
  DEF VAR hField AS HANDLE NO-UNDO.
  
  hField = DYNAMIC-FUNCTION('doGetFrameField',FRAME {&FRAME-NAME}:HANDLE,'col' + SELF:SCREEN-VALUE).
  IF VALID-HANDLE(hField) AND hField:TYPE = 'TOGGLE-BOX' THEN 
    hField:CHECKED = TRUE.
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
  IF VALID-HANDLE(hUpdate) THEN APPLY 'close' TO hUpdate.
  mpXML = ?.
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
  RUN enable_UI.

  &IF DEFINED(UIB_is_Running) NE 0 &THEN
    RUN InitializeObject.
  &ENDIF

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BlankRecord C-Win 
PROCEDURE BlankRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR qh         AS HANDLE NO-UNDO.
  DEF VAR hObj       AS HANDLE NO-UNDO.
  DEF VAR cDroppList AS CHAR   NO-UNDO.
  ASSIGN 
    hObj = FRAME {&FRAME-NAME}:HANDLE:FIRST-CHILD
    hObj = hObj:FIRST-CHILD
    cDroppList = REPLACE("{&List-2}",' ',',') 
 .
  DO WHILE VALID-HANDLE(hObj):
    IF NOT CAN-DO(cDroppList,hObj:NAME) THEN
      CASE hObj:TYPE:
        WHEN 'FILL-IN' OR WHEN 'EDITOR'  THEN hObj:SCREEN-VALUE = ''.
        WHEN 'COMBO-BOX' THEN 
        DO:
          /*IF hObj:NAME = 'opris' THEN hObj:SCREEN-VALUE = 'Alle'.*/
          hObj:SCREEN-VALUE = hObj:ENTRY(1).
          
        END.
        WHEN 'TOGGLE-BOX' THEN
          hObj:CHECKED = FALSE.
      END CASE.
    hObj = hObj:NEXT-SIBLING.
  END.

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN 
      btnUpdate:SENSITIVE = FALSE
      btnDelete:SENSITIVE = FALSE
    .
  END.
  DYNAMIC-FUNCTION('initValues':U).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BuildParameterTable C-Win 
PROCEDURE BuildParameterTable :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR hObj          AS HANDLE    NO-UNDO.
  DEF VAR bh            AS HANDLE    NO-UNDO.
  DEF VAR cFieldName    AS CHAR      NO-UNDO.
  DEF VAR icnt          AS INT       NO-UNDO.
  DEF VAR artbasFields  AS CHARACTER NO-UNDO.
  DEF VAR StLinjeFields AS CHARACTER NO-UNDO.    
  
  DO WITH FRAME {&FRAME-NAME}:
    tth:DEFAULT-BUFFER-HANDLE:EMPTY-TEMP-TABLE().

    /*Dette bør byttes ut med oppslag mot _file slik at en kan legge inn nye felter uten å måtte vedlikeholde
     disse listene*/
    ASSIGN 
      artbasFields  = 'vg,sasong,matkod,vmid,farg,levnr,opris,kjedevare,gjennomfaktureres,grunnsortiment,Jamforenhet,mengde'
      StLinjeFields = 'perid,butik,stTypeId'
      hObj          = FRAME {&FRAME-NAME}:HANDLE:FIRST-CHILD
      hObj          = hObj:FIRST-CHILD
      bh            = tth:DEFAULT-BUFFER-HANDLE
      iCnt          = 0
    .
    /*Løp igjennom widgettreet og lag parametre av alle feltene (fill-in og combo), ved NEW vil sysGr være
    0 så den må fikses i oppdateringsprogrammet*/
    DO WHILE VALID-HANDLE(hObj):
      IF CAN-DO('FILL-IN,EDITOR,COMBO-BOX,TOGGLE-BOX',hObj:TYPE) THEN
      myBlock: 
      DO ON ERROR UNDO, LEAVE:
        IF hObj:NAME BEGINS 'lbl' THEN UNDO myBlock, LEAVE myBlock.
        IF hObj:NAME = 'Beskrivelse' THEN UNDO myBlock, LEAVE myBlock. /*Kan ikke lagre beskrivelse i sysPara, den vedlikeholdes i sysGruppe*/
        cFieldName = IF CAN-DO(artbasfields,hObj:NAME) THEN 'artbas.' + hObj:NAME 
                     ELSE 
                       IF CAN-DO(StLinjeFields,hObj:NAME) THEN 'stlinje.' + hObj:NAME 
                         ELSE hObj:NAME.
        bh:FIND-FIRST('WHERE parameter1 = ' + QUOTER(cFieldName)) NO-ERROR.
        IF bh:AVAILABLE THEN
        DO:
          /*Delete parameter if screen-value has no value or combo-box has value of "ALLE" */
          IF hObj:SCREEN-VALUE = '' OR hObj:SCREEN-VALUE = ? OR (hObj:TYPE = "COMBO-BOX" AND hObj:SCREEN-VALUE = "Alle") THEN
          DO:
            bh:BUFFER-DELETE().
            UNDO myBlock, LEAVE myBlock.
          END.
          bh:BUFFER-FIELD('parameter2'):BUFFER-VALUE = hObj:SCREEN-VALUE.
        END.
        ELSE
        DO:
          /*Do not add if screen-value has no value*/
          IF hObj:SCREEN-VALUE = '' OR hObj:SCREEN-VALUE = ? 
            OR (hObj:NAME = "opris" AND hObj:SCREEN-VALUE = "Alle") THEN 
            UNDO myBlock, LEAVE myBlock.
          bh:BUFFER-CREATE().                                     
          ASSIGN 
            iCnt = iCnt + 1
            bh:BUFFER-FIELD('sysHid'):BUFFER-VALUE     = STRING(gsysHid)
            bh:BUFFER-FIELD('sysGr'):BUFFER-VALUE      = (IF sysGr:SCREEN-VALUE = "0" THEN "999" ELSE sysGr:SCREEN-VALUE)
            bh:BUFFER-FIELD('paranr'):BUFFER-VALUE     = STRING(iCnt)
            bh:BUFFER-FIELD('parameter1'):BUFFER-VALUE = cFieldName
            bh:BUFFER-FIELD('parameter2'):BUFFER-VALUE = hObj:SCREEN-VALUE
          .
          IF cFieldName = 'artbas.opris' THEN /*Må bytte ja til YES og nei til NO*/
          DO:
            bh:BUFFER-FIELD('parameter2'):BUFFER-VALUE = IF hObj:SCREEN-VALUE = 'Ja' THEN 'Yes' ELSE 'No'.
          END.
        END.
      END.
      hObj = hObj:NEXT-SIBLING.
    END.
/*       tth:WRITE-XML('file','.\slettme.xml'). */
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ChangeCompany C-Win 
PROCEDURE ChangeCompany :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*   DYNAMIC-FUNCTION("setAttribute",hBrowse,"basequery","WHERE iJBoxCompanyId = " + STRING(DYNAMIC-FUNCTION("getCompanyId")). */
/*   DYNAMIC-FUNCTION("setCurrentObject",hBrowse).                                                                             */
/*   RUN OpenQuery.                                                                                                            */

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
  DISPLAY sysGr Beskrivelse hjelpetekst butik avdelingnr advFilePath hg 
          advFileExtent Vg stTypeId SaSong advRapportNavn MatKod advMail VMId 
          advSplittRapport advSendMail Farg LevNr lblAll ProdNr colLevKod 
          colLevNamn RAvdNr colLevFargkod colBeskrivelse colAvdelingnavn colVMid 
          opris colHgBeskr colVMbeskrivelse kjedevare colJamforenhet colVgBeskr 
          gjennomfaktureres colMengde colRAvdNr colStrekkode grunnsortiment 
          sortPhrase1 useBreakBySummary sortPhrase2 useSortDesc sortPhrase3 
          useDelsummering lblButikk lblAdvanced lblAvdeling lblHg lblVg 
          lblSasong lblMatKod lblvmid lblFarg lblLevnr lblKolonnevisning 
          lblProdNr lblRAvdNr lblDelsummering lblButtons 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE rectToolbar RECT-1 RECT-2 RECT-3 RECT-4 RECT-5 hjelpetekst btnbutnr 
         btnAvdelingNr btnHg btnVg btnSaSong advRapportNavn btnMatKod advMail 
         btnVMId advSplittRapport advSendMail btnFarg btnLevNr lblAll btnProdNr 
         colLevKod colLevNamn btnRAvdNr colLevFargkod colBeskrivelse 
         colAvdelingnavn colVMid opris colHgBeskr colVMbeskrivelse kjedevare 
         colJamforenhet colVgBeskr gjennomfaktureres colMengde colRAvdNr 
         colStrekkode grunnsortiment sortPhrase1 useBreakBySummary sortPhrase2 
         useSortDesc sortPhrase3 useDelsummering BtnOK BtnNew 
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
  DO WITH FRAME {&FRAME-NAME}:
    hToolbar = DYNAMIC-FUNCTION("NewToolbar",
                                  rectToolbar:HANDLE,
                                  "File",
                                  "Blank¤enabled,rule,Report;Hent ABC rapport¤enabled"
                                 ,"maxborder").
    tth = DYNAMIC-FUNCTION("getTempTable","","sysPara|where sysHid = " + STRING(gSysHid) + " AND SysGr=" + STRING(gsysGr),?).
    ASSIGN 
      sortPhrase1:DELIMITER = "|"
      sortPhrase1:LIST-ITEM-PAIRS = "|0|" + "Avdeling|Avdelingnr|Hovedgruppe|Hg|Varegruppe|Vg|Område|RAvdNr|Leverandør|LevNamn|Produsent|ProdNr|Varemerke|VMid"
    . 
    ASSIGN 
      sortPhrase2:DELIMITER = "|"
      sortPhrase2:LIST-ITEM-PAIRS = "|0|" + "Ant.Solgt|AntSolgt|Innkjøpsverdi|KjopVerdi|Verdi solgt|VerdiSolgt|DB kr.|DBkroner|DB %|DBpros"
    . 
    ASSIGN 
      sortPhrase3:DELIMITER = "|"
      sortPhrase3:LIST-ITEM-PAIRS = "|0|" + "Avdeling|Avdelingnr|Hovedgruppe|Hg|Varegruppe|Vg|Område|RAvdNr|Leverandør|LevNamn|Produsent|ProdNr|Varemerke|VMid"
    . 

    initValues().
    APPLY 'entry' TO butik.
  END.
  DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,500,500,200,200).
  DYNAMIC-FUNCTION("setNoResizex", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,"rect-1,rect-2").
  DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,"hjelpetekst,rect-1,rect-2").
  THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = FALSE.
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
THIS-PROCEDURE:CURRENT-WINDOW:WINDOW-STATE = 3.
THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
APPLY 'entry' TO butik IN FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE myDisplayRecord C-Win 
PROCEDURE myDisplayRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR qh         AS HANDLE NO-UNDO.
  DEF VAR hObj       AS HANDLE NO-UNDO.
  DEF VAR bh         AS HANDLE NO-UNDO.
  DEF VAR cReturn    AS CHAR   NO-UNDO.

  DEF VAR cFieldName AS CHAR NO-UNDO.
  
DO WITH FRAME {&FRAME-NAME}:
  IF VALID-HANDLE(tth) THEN DELETE OBJECT tth NO-ERROR.

  ASSIGN 
    cReturn                  = DYNAMIC-FUNCTION("getFieldValues","sysGruppe","WHERE sysHid = " + STRING(gSysHid) + ' AND sysGr =' + STRING(gSysGr),"sysGr,Beskrivelse,HjelpeTekst")
    sysGr:SCREEN-VALUE = ENTRY(1,cReturn,'|')           /*blir overskrevet om det finnes i parametertabell*/
    beskrivelse:SCREEN-VALUE = ENTRY(2,cReturn,'|')
    hjelpetekst:SCREEN-VALUE = ENTRY(3,cReturn,'|')
    advRapportNavn:SCREEN-VALUE = beskrivelse:SCREEN-VALUE /*blir overskrevet om det finnes i parametertabell*/
    hObj                     = FRAME {&FRAME-NAME}:HANDLE:FIRST-CHILD
    hObj                     = hObj:FIRST-CHILD
    gSysGr                   = INT(sysGr:SCREEN-VALUE)
    tth                      = DYNAMIC-FUNCTION("getTempTable","","sysPara|where sysHid = " + STRING(gSysHid) + " AND SysGr =" + STRING(gSysGr),?)
    bh                       = tth:DEFAULT-BUFFER-HANDLE
  .
  
  DO WHILE VALID-HANDLE(hObj):
    IF (hObj:TYPE = "FILL-IN" AND NOT hObj:NAME BEGINS 'lbl') OR CAN-DO('COMBO-BOX,EDITOR,TOGGLE-BOX',hObj:TYPE) THEN
    DO:
      cFieldName = 'artbas.' + hObj:NAME.
      bh:FIND-FIRST('where sysHid = ' + STRING(gsysHId) 
                                                + ' AND sysGr      = ' + STRING(sysgr:SCREEN-VALUE) 
                                                + ' AND parameter1 = ' + QUOTER(cFieldName),NO-LOCK) NO-ERROR.
      IF NOT bh:AVAILABLE THEN
      DO:
        cFieldName = 'stlinje.' + hObj:NAME.
        bh:FIND-FIRST('where sysHid = ' + STRING(gsysHId) 
                                                  + ' AND sysGr      = ' + STRING(sysgr:SCREEN-VALUE) 
                                                  + ' AND parameter1 = ' + QUOTER(cFieldName),NO-LOCK) NO-ERROR.
      END.
      IF NOT bh:AVAILABLE THEN
      DO:
        cFieldName = hObj:NAME.
        bh:FIND-FIRST('where sysHid = ' + STRING(gsysHId) 
                                                  + ' AND sysGr      = ' + STRING(sysgr:SCREEN-VALUE) 
                                                  + ' AND parameter1 = ' + QUOTER(cFieldName),NO-LOCK) NO-ERROR.
      END.
      IF bh:AVAILABLE THEN
      DO:
        IF hObj:TYPE = "Combo-box" THEN
          hObj:SCREEN-VALUE = convLogical(bh:BUFFER-FIELD('parameter2'):BUFFER-VALUE,'NO'). /*NO = Norwegian*/
        ELSE
          hObj:SCREEN-VALUE = bh:BUFFER-FIELD('parameter2'):BUFFER-VALUE.
      END.
    END.
    hObj = hObj:NEXT-SIBLING.
  END.
  initValues().  
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE myTeam C-Win 
PROCEDURE myTeam :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihSourceBrw AS HANDLE NO-UNDO.
DEF INPUT PARAM ihDropDown  AS HANDLE NO-UNDO.

IF ihDropDown:SCREEN-VALUE = "" THEN
  DYNAMIC-FUNCTION("setAttribute",ihSourceBrw,"QueryFilter","").
ELSE
  DYNAMIC-FUNCTION("setAttribute",ihSourceBrw,"QueryFilter"," AND CAN-DO(teamlist," + QUOTER(ihDropDown:SCREEN-VALUE) + ')').
DYNAMIC-FUNCTION("setCurrentObject",ihSourceBrw).
RUN openQuery.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReportRecord C-Win 
PROCEDURE ReportRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR cReturnValues  AS CHAR NO-UNDO.
  DEF VAR bOk            AS LOG  NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:
    THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
    RUN JBoxLookup.w (THIS-PROCEDURE,50,
                      "sysGruppe"
                      + ";SysGr|Rapp.nr"
                      + ";!SysHid"
                      + ";Beskrivelse"
                      + ";Hjelpetekst"
                     ,"WHERE sysHid = " + STRING(gSysHid)
                      ,""                                                  
                      ,"sysgr,hjelpetekst,beskrivelse",   /* <- return values for these fields */
                        OUTPUT cReturnValues,
                        OUTPUT bOK).
    THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.
  
    IF bOk THEN
    DO:
      RUN BlankRecord.
      ASSIGN 
        sysGr:SCREEN-VALUE        = ENTRY(1,cReturnValues,'|')
        hjelpetekst:SCREEN-VALUE  = ENTRY(2,cReturnValues,'|')
        Beskrivelse:SCREEN-VALUE  = ENTRY(3,cReturnValues,'|')
        btnUpdate:SENSITIVE       = TRUE
        btnDelete:SENSITIVE       = TRUE
        gSysGr                    = INT(sysGr:SCREEN-VALUE)
      .
      RUN myDisplayRecord.
      APPLY 'entry' TO butik.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setSelectorAttributes C-Win 
PROCEDURE setSelectorAttributes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihSourceBrw   AS HANDLE NO-UNDO.
DEF INPUT PARAM ihTargetBrw   AS HANDLE NO-UNDO.

DEF VAR   cList AS CHAR NO-UNDO.
DEF VAR   hFilterToolbar AS HANDLE NO-UNDO.
DEF VAR   hFilterButton  AS HANDLE NO-UNDO.

DEF VAR hSourceBuffer   AS HANDLE NO-UNDO.
DEF VAR hTargetBuffer   AS HANDLE NO-UNDO.
DEF VAR hSelectorWin    AS HANDLE NO-UNDO.
DEF VAR hSelectorSplitB AS HANDLE NO-UNDO.

DEF VAR ix             AS INT    NO-UNDO.
DEF VAR cSelectedRows  AS CHAR   NO-UNDO.

IF NOT bButikk THEN RETURN.

IF TRIM(ENTRY(1,PROGRAM-NAME(3)," ")) = "resendRecord" THEN .
ELSE 
DO:
  cList = " | |" + DYNAMIC-FUNCTION("getFieldList",'ButikkTeam;TeamNr|Beskrivelse;+rButikkTeam|butikkteam_rowid.p',
                           'WHERE TeamTypeId = "2"' /*Hardkodet tallet 2*/
                           ).
  CREATE COMBO-BOX hTeamCombo
    ASSIGN DELIMITER        = "|"
           DATA-TYPE        = "CHARACTER"
           FORMAT           = "x(256)"
           NAME             = "cbTeam"
           SUBTYPE          = "DROP-DOWN-LIST"
           LIST-ITEM-PAIRS  = cList
           INNER-LINES      = 50
           FRAME            = ihSourceBrw:FRAME
           X                = 90
           Y                = 5
           WIDTH-PIXELS     = 250
           VISIBLE          = TRUE
           SENSITIVE        = TRUE
           TRIGGERS:
             ON VALUE-CHANGED PERSISTENT RUN myTeam IN THIS-PROCEDURE (INPUT ihSourceBrw, INPUT hTeamCombo).
  /*            ON TAB           PERSISTENT RUN DoProcessEvent (SOURCE-PROCEDURE,"leave"). */
  /*            ON BACK-TAB      PERSISTENT RUN DoProcessEvent (SOURCE-PROCEDURE,"leave"). */
  /*            ON RETURN        PERSISTENT RUN DoProcessEvent (SOURCE-PROCEDURE,"leave"). */
  /*            ON ENTRY         PERSISTENT RUN DoProcessEvent (SOURCE-PROCEDURE,"entry"). */
             ON END-ERROR     PERSISTENT RUN DoProcessEvent (SOURCE-PROCEDURE,"end-error").
           END TRIGGERS.
   ASSIGN 
     bButikk    = FALSE
     hSourceBrw = ihSourceBrw
   .
END.
  /*Fiks størrelse*/ 
  ASSIGN hSelectorWin               = ihSourceBrw:WINDOW
         hSelectorWin:WIDTH-PIXELS  = hSelectorWin:WIDTH-PIXELS + 400
         hSelectorWin:HEIGHT-PIXELS = hSelectorWin:HEIGHT-PIXELS + 200
         hSelectorSplitB            = DYNAMIC-FUNCTION("getSplitBarHandle",hSelectorWin,"x")
         .
  APPLY "window-resized" TO hSelectorWin.
  hSelectorSplitB:X = hSelectorSplitB:FRAME:WIDTH-PIXELS / 2 - hSelectorSplitB:WIDTH-PIXELS / 2.

  APPLY "end-move" TO hSelectorSplitB.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE updateSysGruppe C-Win 
PROCEDURE updateSysGruppe :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
    Ved ny, kjøres først en generering av parametre (buildParameterTable),
    for så å kjøre selve oppdateringsprogrammet.

------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  DEF VAR cAction AS CHAR NO-UNDO.
  cAction = SUBSTRING(SELF:NAME,4).
  
  IF Butik:SCREEN-VALUE = '' AND cAction NE 'Delete' THEN
  DO:
    MESSAGE 'Feltet butikk MÅ ha verdi, vennligst prøv igjen'
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
    APPLY 'entry' TO btnbutnr.
    RETURN NO-APPLY.
  END.
  
  RUN BuildParameterTable.  
  ASSIGN 
    iX = INT((CURRENT-WINDOW:WIDTH-PIXEL / 2 + CURRENT-WINDOW:X))
    iY = INT((CURRENT-WINDOW:HEIGHT-PIXEL / 2 + CURRENT-WINDOW:Y))  
  .
  RUN excelrapportUpdate.w PERSISTENT SET hUpdate.
  IF VALID-HANDLE(hUpdate) THEN 
  DO:
    ASSIGN 
      hUpdate:CURRENT-WINDOW:X =  iX
      hUpdate:CURRENT-WINDOW:Y =  iY
      . 
    RUN initValues IN hUpdate (INPUT TABLE-HANDLE tth,INPUT cAction,INPUT STRING(gSysHid),INPUT sysGr:SCREEN-VALUE).
    RUN initializeObject IN hUpdate.
    RUN MoveToTop IN hUpdate.
  END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE writeAdHoc C-Win 
PROCEDURE writeAdHoc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*   advfilePath:SCREEN-VALUE IN FRAME {&FRAME-NAME} = SESSION:TEMP-DIRECTORY. */
  RUN BuildParameterTable.  
  ASSIGN 
    iX = INT((CURRENT-WINDOW:WIDTH-PIXEL / 2 + CURRENT-WINDOW:X))
    iY = INT((CURRENT-WINDOW:HEIGHT-PIXEL / 2 + CURRENT-WINDOW:Y))  
  .
  RUN excelrapportadhoc.w PERSISTENT SET hAdHoc.
  
/*   MESSAGE 'Write-xml'   'c:\temp\slettme.xml'  */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK.         */
/*   tth:WRITE-XML('file','c:\temp\slettme.xml'). */
  
  IF VALID-HANDLE(hAdHoc) THEN 
  DO:
    ASSIGN 
      hAdHoc:CURRENT-WINDOW:X =  iX
      hAdHoc:CURRENT-WINDOW:Y =  iY
      . 
    RUN initializeObject IN hAdHoc.
    DYNAMIC-FUNCTION('setReportFile' IN hAdHoc,'excelrapport.p').
    RUN MoveToTop IN hAdHoc.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION chkIkkeSendMail C-Win 
FUNCTION chkIkkeSendMail RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
   RETURN MENU-ITEM m_Test_mail_ikke_send:CHECKED IN MENU POPUP-MENU-advSendMail.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION convLogical C-Win 
FUNCTION convLogical RETURNS CHARACTER
  (INPUT ipValue AS CHARACTER,
   INPUT ipLanguage AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cTrueList  AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cFalseList AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE opValue    AS CHARACTER   NO-UNDO.
  ASSIGN 
    cTrueList  = "1,y,yes,true,j,ja"
    cFalseList = "0,n,no,false,nei"
  .
  CASE ipLanguage:
    WHEN 'NO' OR WHEN 'DK' OR WHEN 'SE' THEN
    DO:
      IF CAN-DO(cTrueList,ipValue) THEN opValue = 'Ja'.
      ELSE IF CAN-DO(cFalseList,ipValue) THEN opValue = 'Nei'.
      ELSE opValue = ipValue.       
    END.
    OTHERWISE 
    DO:
      IF CAN-DO(cTrueList,ipValue) THEN opValue = 'Yes'.
      ELSE IF CAN-DO(cFalseList,ipValue) THEN opValue =  'No'.
      ELSE opValue = ipValue.               
    END.
  END CASE.
  RETURN opValue.
    
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION doGetFrameField C-Win 
FUNCTION doGetFrameField RETURNS HANDLE
  ( INPUT ihFrame      AS HANDLE,
    INPUT icFrameField AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR hObj AS HANDLE NO-UNDO.

  hObj = ihFrame:FIRST-CHILD:FIRST-CHILD.
  DO WHILE VALID-HANDLE(hObj):
    IF hObj:NAME = icFrameField THEN
      RETURN hObj.
    hObj = hObj:NEXT-SIBLING.
  END.
  RETURN ?.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION doSendFilePath C-Win 
FUNCTION doSendFilePath RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN advFilePath:SCREEN-VALUE IN FRAME {&FRAME-NAME}.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION initValues C-Win 
FUNCTION initValues RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE pcFilePath      AS CHARACTER NO-UNDO.
  DEFINE VARIABLE pcFileExtent    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE pbShowFields    AS LOG       NO-UNDO.
  DEFINE VARIABLE pcReportName    AS CHAR      NO-UNDO.

  pcFilePath = DYNAMIC-FUNCTION("getFieldValues","SysPara","WHERE SysHId = 224 and SysGr = 1 and ParaNr = 1","Parameter2").
  IF pcFilePath = ? OR pcFilePath = '' THEN pcFilePath = SESSION:TEMP-DIRECTORY.
  
  pcFileExtent = DYNAMIC-FUNCTION("getFieldValues","SysPara","WHERE SysHId = 224 and SysGr = 1 and ParaNr = 2","Parameter2").
  IF pcFileExtent = ? OR pcFileExtent = '' THEN pcFileExtent = '.xls'.  
  
  pbShowFields    = LOGICAL(DYNAMIC-FUNCTION("getFieldValues","SysPara","WHERE SysHId = 224 and SysGr = 1 and ParaNr = 4","Parameter2")).
  IF pbShowFields = ? THEN pbShowFields = TRUE.
  
  pcReportName = DYNAMIC-FUNCTION("getFieldValues","SysPara","WHERE SysHId = 224 and SysGr = 1 and ParaNr = 6","Parameter2").
  IF pcReportName = ? OR pcReportName = '' THEN pcReportName = 'ABC Artikkelrapport'.

  OS-CREATE-DIR VALUE(RIGHT-TRIM(pcFilePath,'\')). /* Sikrer at filkatalogen finnes. */

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN 
      advFilePath:SCREEN-VALUE    = IF advFilePath:SCREEN-VALUE    = '' THEN pcFilePath ELSE advFilePath:SCREEN-VALUE
      advFileExtent:SCREEN-VALUE  = IF advFileExtent:SCREEN-VALUE  = '' THEN pcFileExtent ELSE advFileExtent:SCREEN-VALUE
      stTypeId:SCREEN-VALUE       = IF stTypeId:SCREEN-VALUE    = '' THEN 'Artikkel' ELSE stTypeId:SCREEN-VALUE
      advRapportNavn:SCREEN-VALUE = pcReportName
      advRapportNavn:SCREEN-VALUE = REPLACE(advRapportNavn:SCREEN-VALUE,':','')
      kjedevare:VISIBLE           = pbShowFields
      gjennomfaktureres:VISIBLE   = pbShowFields
      advSendMail:SENSITIVE       = advSplittRapport:CHECKED
    . 
    IF NOT advSplittRapport:CHECKED THEN advSendMail:CHECKED = FALSE.
  END.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION receiveParameterTable C-Win 
FUNCTION receiveParameterTable RETURNS HANDLE
  (INPUT iptth AS HANDLE) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  tth = iptth.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION sendParameterTable C-Win 
FUNCTION sendParameterTable RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN tth.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setSysGr C-Win 
FUNCTION setSysGr RETURNS LOGICAL
  ( INPUT iiSysgr AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  gSysGr = iiSysGr.

  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION validateFields C-Win 
FUNCTION validateFields RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  DEF VAR hField AS HANDLE NO-UNDO.
  
  hField = DYNAMIC-FUNCTION('doGetFrameField',FRAME {&FRAME-NAME}:HANDLE,'col' + sortPhrase1:SCREEN-VALUE).
  IF VALID-HANDLE(hField) AND hField:TYPE = 'TOGGLE-BOX' AND NOT hFIeld:CHECKED THEN 
  DO:
    MESSAGE 'Kolonnen ' hField:LABEL ' må være valgt for å kunne kjøre med denne sortering, og blir automatisk tatt med.' SKIP
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
    hField:CHECKED = TRUE.
  END.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

