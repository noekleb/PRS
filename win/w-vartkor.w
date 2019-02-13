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
    ASSIGN wArtBasRecid = RECID(ArtBas).
&ELSE
  DEF INPUT PARAMETER wArtBasRecid AS RECID NO-UNDO.
  DEF INPUT PARAMETER wModus       AS CHAR  NO-UNDO. /* NY, ENDRE, SLETT */
&ENDIF

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE iMellankategori AS INTEGER     NO-UNDO.
DEFINE VARIABLE iStrTypeID AS INTEGER    NO-UNDO.
DEFINE VARIABLE iVg        AS INTEGER    NO-UNDO.
DEF VAR bFraKopier AS LOG NO-UNDO.
DEF VAR dSkoModus AS LOG NO-UNDO.
DEF VAR wForslagLopNr      AS CHAR   NO-UNDO.
DEF VAR wNyVg              AS INT    NO-UNDO.
DEF VAR wOk                AS LOG    NO-UNDO.
DEF VAR wOldRecid          AS RECID  NO-UNDO.
DEF VAR hParentHandle      AS HANDLE NO-UNDO.
DEF VAR hLabel             AS HANDLE NO-UNDO.
DEF VAR wBildNr            AS INT    NO-UNDO.
DEF VAR wFilNavn           AS CHAR   NO-UNDO.
DEF VAR wFilExt            AS CHAR   NO-UNDO.
DEF VAR wButikk            AS CHAR FORMAT "x(6)" NO-UNDO.
DEF VAR wVisLager          AS RECID  NO-UNDO.
DEF VAR wBestHodeRecid     AS RECID  NO-UNDO.
DEF VAR wRS-Vis            AS RECID  NO-UNDO.
DEF VAR wPerId             AS CHAR   NO-UNDO.
DEF VAR wStTypeId          AS CHAR   NO-UNDO.
DEF VAR wCl                AS INT    NO-UNDO.
DEF VAR wPerLinTxt         AS CHAR   NO-UNDO.
DEF VAR wHistorikk         AS HANDLE NO-UNDO.
DEF VAR wCurrent-Window    AS HANDLE NO-UNDO.
DEF VAR iEkstVPILevNr      AS INT    NO-UNDO.
DEF VAR wArtikkelNr        AS DEC    NO-UNDO.
DEF VAR lArtikkelNr        AS DEC    NO-UNDO.
DEF VAR cServicehandel     AS CHAR   NO-UNDO.
DEF VAR cENDRET AS CHAR NO-UNDO.
DEF VAR cORG    AS CHAR NO-UNDO.
DEF VAR bGjFakt AS LOG NO-UNDO.
DEF VAR bKjede  AS LOG NO-UNDO.
DEF VAR cFor    AS CHAR NO-UNDO.
DEF VAR cEtter  AS CHAR NO-UNDO.
DEF VAR bKopierVaretekst AS LOG NO-UNDO.
DEFINE VARIABLE iTabOrdning AS INTEGER NO-UNDO.
DEF VAR iAntArtPris  AS INT NO-UNDO.
DEF VAR iAntPrisKo   AS INT NO-UNDO.
DEF VAR iAntBestHode AS INT NO-UNDO.
DEF VAR iAntVpiPris  AS INT NO-UNDO.
DEF VAR iLopNr       AS INT NO-UNDO.
DEFINE VARIABLE cParaAktivFlip AS CHARACTER  NO-UNDO.
DEFINE VARIABLE hPakkeLinje  AS HANDLE     NO-UNDO.
DEFINE VARIABLE hStrekkode   AS HANDLE     NO-UNDO.
DEFINE VARIABLE hTranslogg   AS HANDLE     NO-UNDO.
DEFINE VARIABLE hErstattning AS HANDLE     NO-UNDO.
DEFINE VARIABLE hAktivHandle AS HANDLE     NO-UNDO.
DEFINE VARIABLE hBestilling  AS HANDLE     NO-UNDO.
DEFINE VARIABLE hIndivid     AS HANDLE     NO-UNDO.
DEFINE VARIABLE hArtBestPkt  AS HANDLE     NO-UNDO.
DEFINE VARIABLE hEtikettVindu AS HANDLE     NO-UNDO.
DEFINE VARIABLE iBrukerButikk AS INTEGER    NO-UNDO.
DEFINE VARIABLE cEDBSystem    AS CHARACTER NO-UNDO. 
DEFINE VARIABLE iAntSlett     AS INTEGER   NO-UNDO.
DEFINE VARIABLE iAntNyEndre   AS INTEGER   NO-UNDO.
DEFINE VARIABLE lOverstyrFrame1VgArt AS LOGICAL     NO-UNDO.
DEF VAR bEtiTvang AS LOG NO-UNDO.

DEFINE VARIABLE cLagerAnt       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStrListe       AS CHARACTER NO-UNDO.
DEF VAR cStorrelser AS CHAR NO-UNDO.
DEF VAR cAntall     AS CHAR NO-UNDO.
DEF VAR iCount      AS INT  NO-UNDO.
DEF VAR cTmpStr     AS CHAR NO-UNDO.

DEF VAR wKalkyle           AS HANDLE NO-UNDO.
/* DEF VAR wLapTop            AS LOG    NO-UNDO. */
DEF VAR wBekreft           AS LOG    NO-UNDO.
DEF VAR wKopi              AS LOG    NO-UNDO.
DEF VAR wTabTekst          AS CHAR   NO-UNDO.
DEF VAR h_PrisKo           AS HANDLE NO-UNDO.
DEF VAR cTekst             AS CHAR   NO-UNDO.
DEF VAR iVgTxtBeskr        AS INT    NO-UNDO.
DEF VAR hFocus             AS HANDLE NO-UNDO.
DEF VAR bOk AS LOG NO-UNDO.
DEF VAR cLevBasRowIdList AS CHAR NO-UNDO.
DEF VAR cLevBasIdList    AS CHAR NO-UNDO.
DEF VAR oiLevNr            AS INT NO-UNDO. /* Evt. initiell lev */

DEF VAR cRowIdList         AS CHAR NO-UNDO.
DEF VAR cIdList            AS CHAR NO-UNDO.
DEF VAR iArtSlag           AS INT NO-UNDO.

/* Variabler for håndtering av Tab-Stip. */
DEFINE VAR chTabStrip        AS COM-HANDLE NO-UNDO.
DEFINE VAR chTabs            AS COM-HANDLE NO-UNDO.
DEFINE VAR chTab             AS COM-HANDLE NO-UNDO.
DEFINE VAR chTab1Side        AS COM-HANDLE NO-UNDO.
DEFINE VAR wAktivFlip        AS INT        INIT 1 NO-UNDO. /* !! parameter !! */
DEFINE VARIABLE iAntFlip     AS INTEGER    NO-UNDO. /* antal flips */
/* parameterverdier */
DEFINE VARIABLE hJmfRutine   AS HANDLE     NO-UNDO.
/* DEFINE VARIABLE cOpris       AS CHARACTER  NO-UNDO. */
DEFINE VARIABLE cOLLager     AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cBildeIKasse AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cVPItilHK    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cNyreg       AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cGenEan      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cGenEan000   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cKode        AS CHAR       NO-UNDO.
DEFINE VARIABLE cSisteBildedir  AS CHARACTER INIT ".\" NO-UNDO.
DEFINE VARIABLE cBildeKatalog   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cBekreftNyttBilde AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cVisKlack     AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cVisInnersula AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cVisInnerfor  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cVisSlit      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cVisLast      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cVisBruks     AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cVisBehKod    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cVisMaterial  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cLagerStyr    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cLagerStrListe AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cTillgButikker AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iSaveBuntNr    AS INTEGER    NO-UNDO.
DEFINE VARIABLE cLevStorrelser AS CHARACTER  NO-UNDO.
DEFINE VARIABLE dLevArtikkelnr AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dSletteDato    AS DATE       NO-UNDO.
DEFINE VARIABLE iAntDarSlett   AS INTEGER    NO-UNDO.
DEFINE VARIABLE cFlikEnaDis    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cFlikDefault   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cAktivFlipTag  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cBrukteStr     AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cMottakstr AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cSport1Default AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cBrukPWLagertrans AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPwd AS CHARACTER NO-UNDO.
DEFINE VARIABLE dVisatLagerArtbas AS DECIMAL     NO-UNDO.

DEF VAR hWindow AS HANDLE NO-UNDO.
DEF VAR lBrukHgiFilter AS LOG NO-UNDO.

DEF VAR hModell AS HANDLE NO-UNDO.

DEF VAR iKopierBildeNr AS INTE NO-UNDO.
DEFINE VARIABLE bHk            AS LOG       NO-UNDO.

{windows.i}

{syspara.i 1 1 18 cTekst}
IF CAN-DO("ja,1,true,yes",cTekst) THEN
    bHK = TRUE.
ELSE
    bHK = FALSE.

{syspara.i 2 4 30 cTekst}
IF CAN-DO("ja,1,true,yes",cTekst) THEN
    bKjede = TRUE.
ELSE
    bKjede = FALSE.
{syspara.i 2 4 31 cTekst}
IF CAN-DO("ja,1,true,yes",cTekst) THEN
    bGjFakt = TRUE.
ELSE
    bGjFakt = FALSE.

/* Buffere */
DEF BUFFER bArtBas   FOR ArtBas.
DEF BUFFER etiArtBas FOR ArtBas.
DEF BUFFER clButiker FOR Butiker.
DEF BUFFER bBruker   FOR Bruker.

DEF TEMP-TABLE tmpChild 
  FIELD wChild AS HANDLE.

/* Här styr vi Fliptexterna */

ASSIGN wTabTekst = "Artikkelinformasjon,Tillegg vare,Kalkyle,Strekkode,Pakke,Erstatningsvare,Individ,Bestilling,Lager,Bestillingspunkt,Transaksjoner,Historikk"
       cFlikEnaDis = "E,E,E,E,E,E,E,E,E,E,E,E"
       iAntFlip  = NUM-ENTRIES(wTabTekst).

{runlib.i}

IF NOT VALID-HANDLE(h_PrisKo) THEN
    RUN prisko.p PERSISTENT SET h_PrisKo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BROWSE-Lager

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tmpLager

/* Definitions for BROWSE BROWSE-Lager                                  */
&Scoped-define FIELDS-IN-QUERY-BROWSE-Lager tmpLager.Butik ~
tmpLager.VVareKost tmpLager.SumAntall tmpLager.SumVerdi tmpLager.DivAntall ~
tmpLager.Antall[1] tmpLager.Antall[2] tmpLager.Antall[3] tmpLager.Antall[4] ~
tmpLager.Antall[5] tmpLager.Antall[6] tmpLager.Antall[7] tmpLager.Antall[8] ~
tmpLager.Antall[9] tmpLager.Antall[10] tmpLager.Antall[11] ~
tmpLager.Antall[12] tmpLager.Antall[13] tmpLager.Antall[14] ~
tmpLager.Antall[15] tmpLager.Antall[16] tmpLager.Antall[17] ~
tmpLager.Antall[18] tmpLager.Antall[19] tmpLager.Antall[20] ~
tmpLager.Antall[21] tmpLager.Antall[22] tmpLager.Antall[23] ~
tmpLager.Antall[24] tmpLager.Antall[25] tmpLager.Antall[26] ~
tmpLager.Antall[27] tmpLager.Antall[28] tmpLager.Antall[29] ~
tmpLager.Antall[30] tmpLager.Antall[31] tmpLager.Antall[32] ~
tmpLager.Antall[33] tmpLager.Antall[34] tmpLager.Antall[35] ~
tmpLager.Antall[36] tmpLager.Antall[37] tmpLager.Antall[38] ~
tmpLager.Antall[39] tmpLager.Antall[40] tmpLager.Antall[41] ~
tmpLager.Antall[42] tmpLager.Antall[43] tmpLager.Antall[44] ~
tmpLager.Antall[45] tmpLager.Antall[46] tmpLager.Antall[47] ~
tmpLager.Antall[48] tmpLager.Antall[49] tmpLager.Antall[50] ~
tmpLager.Antall[51] tmpLager.Antall[52] tmpLager.Antall[53] ~
tmpLager.Antall[54] tmpLager.Antall[55] tmpLager.Antall[56] ~
tmpLager.Antall[57] tmpLager.Antall[58] tmpLager.Antall[59] ~
tmpLager.Antall[60] tmpLager.Antall[61] tmpLager.Antall[62] ~
tmpLager.Antall[63] tmpLager.Antall[64] tmpLager.Antall[65] ~
tmpLager.Antall[66] tmpLager.Antall[67] tmpLager.Antall[68] ~
tmpLager.Antall[69] tmpLager.Antall[70] tmpLager.Antall[71] ~
tmpLager.Antall[72] tmpLager.Antall[73] tmpLager.Antall[74] ~
tmpLager.Antall[75] tmpLager.Antall[76] tmpLager.Antall[77] ~
tmpLager.Antall[78] tmpLager.Antall[79] tmpLager.Antall[80] ~
tmpLager.Antall[81] tmpLager.Antall[82] tmpLager.Antall[83] ~
tmpLager.Antall[84] tmpLager.Antall[85] tmpLager.Antall[86] ~
tmpLager.Antall[87] tmpLager.Antall[88] tmpLager.Antall[89] ~
tmpLager.Antall[90] tmpLager.Antall[91] tmpLager.Antall[92] ~
tmpLager.Antall[93] tmpLager.Antall[94] tmpLager.Antall[95] ~
tmpLager.Antall[96] tmpLager.Antall[97] tmpLager.Antall[98] ~
tmpLager.Antall[99] 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-Lager 
&Scoped-define QUERY-STRING-BROWSE-Lager FOR EACH tmpLager NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-Lager OPEN QUERY BROWSE-Lager FOR EACH tmpLager NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-Lager tmpLager
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-Lager tmpLager


/* Definitions for FRAME FRAME-Lager                                    */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-Lager ~
    ~{&OPEN-QUERY-BROWSE-Lager}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS ArtBas.HoyLavMva ArtBas.Telefonkort ~
ArtBas.Grunnsortiment ArtBas.NegVare ArtBas.Gjennomfaktureres ~
ArtBas.KjedeVare ArtBas.BestForslag ArtBas.Vg ArtBas.VgKat ArtBas.LopNr ~
ArtBas.Beskr ArtBas.LevNr ArtBas.LevKod ArtBas.Utgatt ArtBas.IKasse ~
ArtBas.BildeIKasse ArtBas.SanertDato ArtBas.ArtSlag 
&Scoped-define ENABLED-TABLES ArtBas
&Scoped-define FIRST-ENABLED-TABLE ArtBas
&Scoped-Define ENABLED-OBJECTS B-Kobble B-VPI BUTTON-Ok BUTTON-SokLev ~
BUTTON-TilKasse B-Folder B-Sanera BUTTON-Kopierbilde BUTTON-Slettbilde ~
B-Sok Btn_Help BUTTON-Angre BUTTON-Kopier BUTTON-Lagre BUTTON-ModellFarg ~
BUTTON-Next BUTTON-Ny BUTTON-Paste BUTTON-Prev BUTTON-Slett BUTTON-SokFil ~
CB-ModellFarge CB-EkstVPILev TOGGLE-Annonse BUTTON-SokVgKat FI-LevFargKod ~
TB-WebButikkArtikkel RECT-27 RECT-28 RECT-34 RECT-35 RECT-5 RECT-55 RECT-56 
&Scoped-Define DISPLAYED-FIELDS ArtBas.HoyLavMva ArtBas.Telefonkort ~
ArtBas.Grunnsortiment ArtBas.NegVare ArtBas.NON_Sale ArtBas.UtgattDato ~
ArtBas.ManueltOpprettet ArtBas.Gjennomfaktureres ArtBas.KjedeVare ~
ArtBas.BestForslag ArtBas.IndividType ArtBas.ArtikkelNr HuvGr.Hg ~
HuvGr.HgBeskr ArtBas.Vg ArtBas.VgKat ArtBas.LopNr ArtBas.Pant ArtBas.Beskr ~
ArtBas.LevNr ArtBas.LevKod ArtBas.lager ArtBas.Pakke ArtBas.Utgatt ~
ArtBas.IKasse ArtBas.BildeIKasse ArtBas.OPris ArtBas.HkStyrt ~
ArtBas.KjentPaHK ArtBas.LokPris ArtBas.SanertDato ArtBas.ArtSlag 
&Scoped-define DISPLAYED-TABLES ArtBas HuvGr
&Scoped-define FIRST-DISPLAYED-TABLE ArtBas
&Scoped-define SECOND-DISPLAYED-TABLE HuvGr
&Scoped-Define DISPLAYED-OBJECTS FI-ArtBut CB-ModellFarge CB-EkstVPILev ~
FILL-IN-VgBeskr FILL-IN-LevNamn FI-PaTilbud TOGGLE-Annonse FI-LevFargKod ~
TB-WebButikkArtikkel 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AlleEANok C-ArtKort 
FUNCTION AlleEANok RETURNS CHARACTER
  ( INPUT cStorrelser AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getActivArtNr C-ArtKort 
FUNCTION getActivArtNr RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getArtBut C-ArtKort 
FUNCTION getArtBut RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getArtNr C-ArtKort 
FUNCTION getArtNr RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getPakkeNr C-ArtKort 
FUNCTION getPakkeNr RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getStorl C-ArtKort 
FUNCTION getStorl RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD initAlternativLev C-ArtKort 
FUNCTION initAlternativLev RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD initKarakteristikk C-ArtKort 
FUNCTION initKarakteristikk RETURNS LOGICAL
        (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD initMellankategori C-ArtKort 
FUNCTION initMellankategori RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD initMellanUkat C-ArtKort 
FUNCTION initMellanUkat RETURNS LOGICAL
  ( INPUT iMkatid AS INTE, INPUT iUnderkat AS INTE)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD initUnderkategori C-ArtKort 
FUNCTION initUnderkategori RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD lagerButikk C-ArtKort 
FUNCTION lagerButikk RETURNS LOGICAL
  ( OUTPUT cStrListe AS CHARACTER, OUTPUT cLagerAnt AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ModellBytte C-ArtKort 
FUNCTION ModellBytte RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD NyOpris C-ArtKort 
FUNCTION NyOpris RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD PostFelt C-ArtKort 
FUNCTION PostFelt RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setAlternativLev C-ArtKort 
FUNCTION setAlternativLev RETURNS LOGICAL
  ( INPUT ihFillIn    AS HANDLE,
    INPUT icRowidList AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setKarakteristikk C-ArtKort 
FUNCTION setKarakteristikk RETURNS LOGICAL
  ( INPUT ihFillIn    AS HANDLE,
    INPUT icRowidList AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD settArtSlag C-ArtKort 
FUNCTION settArtSlag RETURNS INTEGER
  ( INPUT iArtSlag AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setUnderkategori C-ArtKort 
FUNCTION setUnderkategori RETURNS LOGICAL
  ( INPUT ihFillIn    AS HANDLE,
    INPUT icRowidList AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD StorlToEAN C-ArtKort 
FUNCTION StorlToEAN RETURNS LOGICAL
  ( INPUT-OUTPUT cEtiketterStorl AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-ArtKort AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE MENU POPUP-MENU-BUTTON-Kopierbilde 
       MENU-ITEM m_Kopier       LABEL "Kopier"        
              DISABLED
       MENU-ITEM m_Lim_inn      LABEL "Lim inn"       
              DISABLED
       MENU-ITEM m_Angre_kopier LABEL "Angre kopier"  
              DISABLED
       MENU-ITEM m_Kopier_fra_artikkel LABEL "Kopier fra artikkel".


/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE IMAGE-Sko AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chIMAGE-Sko AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Folder 
     LABEL "Folder..." 
     SIZE 12 BY 1.14 TOOLTIP "Åpner/lukker foldere i artikkelkortet. Lagring av valgt oppsett.".

DEFINE BUTTON B-Jamfor  NO-FOCUS
     LABEL "Sa&mmenlign..." 
     SIZE 16 BY .81 TOOLTIP "Åpner visning av vindu for sammenligning av artiklene i modellen".

DEFINE BUTTON B-Kobble 
     LABEL "Koble web" 
     SIZE 15 BY 1.05.

DEFINE BUTTON B-Sanera 
     LABEL "Saner..." 
     SIZE 15 BY 1.14 TOOLTIP "Overfører all artikkelinformasjon og statistikk til en annen artikkel".

DEFINE BUTTON B-Sok 
     IMAGE-UP FILE "icon/select.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Søk" 
     SIZE 4.6 BY 1.05 TOOLTIP "Starter Alt-S søkefunksjonen".

DEFINE BUTTON B-VPI  NO-FOCUS
     LABEL "Oppslag i &VPI register..." 
     SIZE 26 BY 1.05 TOOLTIP "Åpner søk i VPI register (På angitt VPI leverandør)".

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

DEFINE BUTTON BUTTON-Kopierbilde 
     IMAGE-UP FILE "icon/e-copy.bmp":U NO-FOCUS
     LABEL "" 
     SIZE 4.6 BY 1.14 TOOLTIP "Kopier bilde" DROP-TARGET.

DEFINE BUTTON BUTTON-Lagre 
     IMAGE-UP FILE "icon/saverec.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "&Lagra" 
     SIZE 4.6 BY 1.05 TOOLTIP "Lagre post (Alt-L)".

DEFINE BUTTON BUTTON-ModellFarg 
     IMAGE-UP FILE "icon/color.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "ModellFarg" 
     SIZE 4.6 BY 1.05 TOOLTIP "Modell/Farge".

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
     SIZE 4.6 BY 1.14 TOOLTIP "Henter bilde fra ClipBoard" DROP-TARGET.

DEFINE BUTTON BUTTON-Prev 
     IMAGE-UP FILE "icon/prev.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Forrige" 
     SIZE 4.6 BY 1.05 TOOLTIP "Forrige post (Alt-PilOpp)".

DEFINE BUTTON BUTTON-SettLopNr  NO-FOCUS
     LABEL "Sett løpenr i varegruppen..." 
     SIZE 27 BY 1.05 TOOLTIP "Tildeler artikkel et løpenummer i vareguppen".

DEFINE BUTTON BUTTON-Slett 
     IMAGE-UP FILE "icon/deleterec.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Radera" 
     SIZE 4.6 BY 1.05 TOOLTIP "Slett post (Alt-D)".

DEFINE BUTTON BUTTON-Slettbilde 
     IMAGE-UP FILE "icon/e-del.bmp":U NO-FOCUS
     LABEL "" 
     SIZE 4.6 BY 1.14 TOOLTIP "Slett bilde" DROP-TARGET.

DEFINE BUTTON BUTTON-SokFil 
     IMAGE-UP FILE "icon/e-open.bmp":U NO-FOCUS
     LABEL "" 
     SIZE 4.6 BY 1.14 TOOLTIP "Henter bilde fra fil" DROP-TARGET.

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

DEFINE BUTTON BUTTON-TilKasse  NO-FOCUS
     LABEL "Send til kasse..." 
     SIZE 18.2 BY 1.05 TOOLTIP "Send artikkel till kassene".

DEFINE VARIABLE CB-EkstVPILev AS INTEGER FORMAT ">>>>>>9":U INITIAL 0 
     LABEL "VPI lev" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1",0
     DROP-DOWN-LIST
     SIZE 28.6 BY 1 TOOLTIP "Viser hvilken VPI leverandør søk i VPI register gjøres mot." NO-UNDO.

DEFINE VARIABLE CB-ModellFarge AS CHARACTER FORMAT "X(256)":U 
     LABEL "Modell" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 54 BY 1 TOOLTIP "Valg av artikkel i modell som skal vises i artikkelkortet" NO-UNDO.

DEFINE VARIABLE FI-ArtBut AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY .71 NO-UNDO.

DEFINE VARIABLE FI-LevFargKod AS CHARACTER FORMAT "X(200)" 
     LABEL "Lev.fargekode" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 44.4 BY 1 TOOLTIP "Leverandørens fargekode og betegnelse på farge" NO-UNDO.

DEFINE VARIABLE FI-PaTilbud AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 27 BY .62
     FGCOLOR 12 FONT 6 NO-UNDO.

DEFINE VARIABLE FILL-IN-LevNamn AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN NATIVE 
     SIZE 29.4 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-VgBeskr AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN NATIVE 
     SIZE 41.2 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-27
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 208.4 BY .14.

DEFINE RECTANGLE RECT-28
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 208.4 BY .14.

DEFINE RECTANGLE RECT-34
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 36 BY 5.95.

DEFINE RECTANGLE RECT-35
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 47 BY 2.62.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 172 BY 3.33.

DEFINE RECTANGLE RECT-55
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 24 BY 2.62.

DEFINE RECTANGLE RECT-56
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 101 BY 2.62.

DEFINE VARIABLE TB-WebButikkArtikkel AS LOGICAL INITIAL no 
     LABEL "Aktiv i nettbutikk" 
     VIEW-AS TOGGLE-BOX
     SIZE 19.8 BY .81.

DEFINE VARIABLE TOGGLE-Annonse AS LOGICAL INITIAL no 
     LABEL "Annonse" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.6 BY .81 TOOLTIP "Brukes til å merke opp artikler som det er leverandørkampanje på" NO-UNDO.

DEFINE BUTTON B-alternativeLev 
     IMAGE-UP FILE "icon/e-sokpr.ico":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-Refresh 
     IMAGE-UP FILE "icon/oppdater.bmp":U NO-FOCUS
     LABEL "..." 
     SIZE 4.8 BY 1.14.

DEFINE BUTTON B-SokLinkvare 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-SokLinkvare-2 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1 TOOLTIP "Åpne infoside for artikkel".

DEFINE BUTTON BUTTON-NyLevsort 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1 TOOLTIP "Ny levsort".

DEFINE BUTTON BUTTON-NyStrType  NO-FOCUS
     LABEL "Ny" 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokBehKode 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokBruk 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokFarge 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokHovedKategori 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokInner 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokKlak 
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

DEFINE BUTTON BUTTON-SokRegnAvd 
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

DEFINE BUTTON BUTTON-Underkategori 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1 TOOLTIP "Ny levsort".

DEFINE VARIABLE CB-Levsort AS CHARACTER FORMAT "X(256)":U 
     LABEL "Leverandørsinndelinger" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 43 BY 1 TOOLTIP "Liste over tilgjengelige leverandørsinndelinger"
     FONT 0 NO-UNDO.

DEFINE VARIABLE CB-Mellankategori AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Mellankat" 
     VIEW-AS COMBO-BOX SORT INNER-LINES 10
     LIST-ITEM-PAIRS "Item 1",0
     DROP-DOWN-LIST
     SIZE 43 BY 1 TOOLTIP "Mellankategori" NO-UNDO.

DEFINE VARIABLE CB-MUkat AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Underkat" 
     VIEW-AS COMBO-BOX SORT INNER-LINES 10
     LIST-ITEM-PAIRS "Item 1",0
     DROP-DOWN-LIST
     SIZE 43 BY 1 TOOLTIP "Underkategori" NO-UNDO.

DEFINE VARIABLE FI-1 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 22.2 BY 1 NO-UNDO.

DEFINE VARIABLE FI-2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 22.2 BY 1 NO-UNDO.

DEFINE VARIABLE FI-3 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 22.2 BY 1 NO-UNDO.

DEFINE VARIABLE FI-4 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 22.2 BY 1 NO-UNDO.

DEFINE VARIABLE FI-5 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 22.2 BY 1 NO-UNDO.

DEFINE VARIABLE FI-6 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 22.2 BY 1 NO-UNDO.

DEFINE VARIABLE FI-7 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 22.2 BY 1 NO-UNDO.

DEFINE VARIABLE FI-8 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 22.2 BY 1 NO-UNDO.

DEFINE VARIABLE FI-JamforPris AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Jamførpris" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE FI-LinkVareTekst AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 17.8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Tekst-10 AS CHARACTER FORMAT "X(256)":U INITIAL "Jamførpris på etikett og hylleforkant" 
      VIEW-AS TEXT 
     SIZE 44 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-Tekst-11 AS CHARACTER FORMAT "X(256)":U INITIAL "Rabatthåndtering i kasse" 
      VIEW-AS TEXT 
     SIZE 44 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-Tekst-12 AS CHARACTER FORMAT "X(256)":U INITIAL "Krav til depositum på artikkel" 
      VIEW-AS TEXT 
     SIZE 41.6 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-Tekst-13 AS CHARACTER FORMAT "X(256)":U INITIAL "Kjøkkenskriver" 
      VIEW-AS TEXT 
     SIZE 21 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-Tekst-14 AS CHARACTER FORMAT "X(256)":U INITIAL "Salgsstopp" 
      VIEW-AS TEXT 
     SIZE 21 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-Tekst-2 AS CHARACTER FORMAT "X(256)":U INITIAL "Leverandørinformasjon" 
      VIEW-AS TEXT 
     SIZE 26.6 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-Tekst-3 AS CHARACTER FORMAT "X(256)":U INITIAL "Notat" 
      VIEW-AS TEXT 
     SIZE 20 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-Tekst-4 AS CHARACTER FORMAT "X(256)":U INITIAL "Grunninformasjon" 
      VIEW-AS TEXT 
     SIZE 49.6 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-Tekst-5 AS CHARACTER FORMAT "X(256)":U INITIAL "Prisinformasjon fra aktiv kalkyle" 
      VIEW-AS TEXT 
     SIZE 62 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-Tekst-6 AS CHARACTER FORMAT "X(256)":U INITIAL "Nettbutikk" 
      VIEW-AS TEXT 
     SIZE 21.6 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-Tekst-7 AS CHARACTER FORMAT "X(256)":U INITIAL "Leverandørforpakkning" 
      VIEW-AS TEXT 
     SIZE 40.4 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-Tekst-9 AS CHARACTER FORMAT "X(256)":U INITIAL "Leveringsmåte til kunde fra butikk" 
      VIEW-AS TEXT 
     SIZE 43 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-Tekst1 AS CHARACTER FORMAT "X(256)":U INITIAL "Koblede registre" 
      VIEW-AS TEXT 
     SIZE 49.6 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FILL-IN-19 AS CHARACTER FORMAT "X(256)":U INITIAL "Vis på ordre" 
      VIEW-AS TEXT 
     SIZE 12 BY .62 NO-UNDO.

DEFINE VARIABLE FILL-IN-EndretInfo AS CHARACTER FORMAT "X(256)":U INITIAL "Endret informasjon" 
      VIEW-AS TEXT 
     SIZE 155.4 BY .62 NO-UNDO.

DEFINE VARIABLE FILL-IN-InnPris AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Innkjøpspris" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-SalgsPris AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Pris" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-TilbPris AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Kampanjepris og periode" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-UtsFra AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-UtsTil AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE Txt-AlternativLev AS CHARACTER FORMAT "X(256)":U INITIAL "Alternative leverandører:" 
      VIEW-AS TEXT 
     SIZE 23 BY .62 NO-UNDO.

DEFINE VARIABLE Txt-Underkat AS CHARACTER FORMAT "X(256)":U INITIAL "Underkat:" 
      VIEW-AS TEXT 
     SIZE 12 BY .62 NO-UNDO.

DEFINE RECTANGLE RECT-29
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 74 BY 3.81.

DEFINE RECTANGLE RECT-31
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 81.6 BY 17.86.

DEFINE RECTANGLE RECT-54
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 81.6 BY 3.81.

DEFINE RECTANGLE RECT-61
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 203.2 BY 1.14.

DEFINE RECTANGLE RECT-62
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 47.6 BY 3.19.

DEFINE RECTANGLE RECT-63
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 47.6 BY 4.05.

DEFINE RECTANGLE RECT-64
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 47.6 BY 3.14.

DEFINE RECTANGLE RECT-65
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 23 BY 2.19.

DEFINE RECTANGLE RECT-66
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 74 BY 17.86.

DEFINE RECTANGLE RECT-67
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 47.6 BY 4.19.

DEFINE RECTANGLE RECT-68
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 47.6 BY 2.14.

DEFINE RECTANGLE RECT-70
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 47.6 BY 2.62.

DEFINE RECTANGLE RECT-71
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 24.8 BY 2.19.

DEFINE VARIABLE S-AlternativLev AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 43 BY 1.38 TOOLTIP "Andre leverandører som artikkelen kan kjøpes inn fra" NO-UNDO.

DEFINE VARIABLE S-Underkategori AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 61.8 BY 2.38 TOOLTIP "Andre leverandører som artikkelen kan kjøpes inn fra" NO-UNDO.

DEFINE VARIABLE T-1 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 TOOLTIP "Vise leverandørens informasjon på ordreutksrift" NO-UNDO.

DEFINE VARIABLE T-10 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 TOOLTIP "Vise leverandørens informasjon på ordreutksrift" NO-UNDO.

DEFINE VARIABLE T-2 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 TOOLTIP "Vise leverandørens informasjon på ordreutksrift" NO-UNDO.

DEFINE VARIABLE T-3 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 TOOLTIP "Vise leverandørens informasjon på ordreutksrift" NO-UNDO.

DEFINE VARIABLE T-4 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 TOOLTIP "Vise leverandørens informasjon på ordreutksrift" NO-UNDO.

DEFINE VARIABLE T-5 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 TOOLTIP "Vise leverandørens informasjon på ordreutksrift" NO-UNDO.

DEFINE VARIABLE T-6 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 TOOLTIP "Vise leverandørens informasjon på ordreutksrift" NO-UNDO.

DEFINE VARIABLE T-7 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 TOOLTIP "Vise leverandørens informasjon på ordreutksrift" NO-UNDO.

DEFINE VARIABLE T-8 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 TOOLTIP "Vise leverandørens informasjon på ordreutksrift" NO-UNDO.

DEFINE VARIABLE T-9 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 TOOLTIP "Vise kommentarfelt på ordreutskrift" NO-UNDO.

DEFINE VARIABLE T-EnableLevInfo AS LOGICAL INITIAL no 
     LABEL "Endre" 
     VIEW-AS TOGGLE-BOX
     SIZE 14 BY .81 TOOLTIP "Åpner felt som styrer utlegg av informasjon på ordreutskrift til lev." NO-UNDO.

DEFINE BUTTON BUTTON-Karakteristikk 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1 TOOLTIP "Ny levsort".

DEFINE BUTTON BUTTON-SokLandKode 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokLevDat-2 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokLevDat-3 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokLevDat-4 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokLevDat-5 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokLevDat-6 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokLevDat-7 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokLevDat-8 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokLevDato-3 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokLevDato-4 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokLevDato-5 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE FI-Land AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN NATIVE 
     SIZE 64.2 BY 1 NO-UNDO.

DEFINE VARIABLE FI-VpiInfoTxt AS CHARACTER FORMAT "X(256)":U INITIAL "VPIinformasjon (Varebok) Normalt oppdatert via pricat melding" 
      VIEW-AS TEXT 
     SIZE 89 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-VpiInfoTxt-2 AS CHARACTER FORMAT "X(256)":U INITIAL "Postforsendelse" 
      VIEW-AS TEXT 
     SIZE 43.8 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-VpiInfoTxt-3 AS CHARACTER FORMAT "X(256)":U INITIAL "Karakteristikk" 
      VIEW-AS TEXT 
     SIZE 43.8 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE Txt-Linjemerknad AS CHARACTER FORMAT "X(256)":U INITIAL "Melding fra leverandør" 
      VIEW-AS TEXT 
     SIZE 85 BY .62 NO-UNDO.

DEFINE VARIABLE Txt-OpprLand AS CHARACTER FORMAT "X(256)":U INITIAL "Opprinnelsesland" 
      VIEW-AS TEXT 
     SIZE 53.4 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE Txt-Varefakta AS CHARACTER FORMAT "X(256)":U INITIAL "Varefakta" 
      VIEW-AS TEXT 
     SIZE 53.4 BY .62
     FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-33
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 91 BY 22.62.

DEFINE RECTANGLE RECT-36
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 115 BY 14.76.

DEFINE RECTANGLE RECT-69
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46 BY 5.48.

DEFINE RECTANGLE RECT-72
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 115 BY 2.33.

DEFINE RECTANGLE RECT-73
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 68.8 BY 5.48.

DEFINE VARIABLE S-Karakteristikk AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 62.2 BY 4.05 TOOLTIP "Andre leverandører som artikkelen kan kjøpes inn fra" NO-UNDO.

DEFINE BUTTON B-ByggomLager 
     LABEL "&Bygg om lager" 
     SIZE 25 BY 1.14.

DEFINE BUTTON B-Chart 
     IMAGE-UP FILE "icon/e-chart.bmp":U
     LABEL "Button 1" 
     SIZE 4.8 BY 1.14.

DEFINE BUTTON B-ForenklVaremot 
     LABEL "Forenklet varemottak..." 
     SIZE 24.6 BY 1.14 TOOLTIP "Endring av varekost lager".

DEFINE BUTTON B-Lagerjustering  NO-FOCUS
     LABEL "Lagerjustering..." 
     SIZE 16.2 BY 1.14 TOOLTIP "Endring av antall i lager".

DEFINE BUTTON B-Nedskrivning 
     LABEL "Endre varekost..." 
     SIZE 19.4 BY 1.14 TOOLTIP "Endring av varekost lager".

DEFINE BUTTON B-OppdLAger 
     LABEL "&Oppdater lagerbilde" 
     SIZE 25 BY 1.14.

DEFINE BUTTON B-Reklamer 
     LABEL "Reklamer restlager..." 
     SIZE 22 BY 1.14.

DEFINE BUTTON B-Svinn 
     LABEL "Svinn..." 
     SIZE 12.6 BY 1.14 TOOLTIP "Varetelling".

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
"L.Reklam", 8,
"Overført", 9,
"Justert", 10,
"Svinn", 11,
"Nedskrevet", 12,
"Rabatt", 13
     SIZE 170 BY 1.19 NO-UNDO.

DEFINE VARIABLE TG-VisSkjul AS LOGICAL INITIAL no 
     LABEL "Vis alle str" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-Lager FOR 
      tmpLager SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-Lager
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-Lager C-ArtKort _STRUCTURED
  QUERY BROWSE-Lager NO-LOCK DISPLAY
      tmpLager.Butik FORMAT "X(10)":U
      tmpLager.VVareKost FORMAT "->>>>>>9":U
      tmpLager.SumAntall FORMAT "X(14)":U
      tmpLager.SumVerdi FORMAT "X(14)":U
      tmpLager.DivAntall FORMAT "X(14)":U
      tmpLager.Antall[1] FORMAT "X(10)":U
      tmpLager.Antall[2] FORMAT "X(10)":U
      tmpLager.Antall[3] FORMAT "X(10)":U
      tmpLager.Antall[4] FORMAT "X(10)":U
      tmpLager.Antall[5] FORMAT "X(10)":U
      tmpLager.Antall[6] FORMAT "X(10)":U
      tmpLager.Antall[7] FORMAT "X(10)":U
      tmpLager.Antall[8] FORMAT "X(10)":U
      tmpLager.Antall[9] FORMAT "X(10)":U
      tmpLager.Antall[10] FORMAT "X(10)":U
      tmpLager.Antall[11] FORMAT "X(10)":U
      tmpLager.Antall[12] FORMAT "X(10)":U
      tmpLager.Antall[13] FORMAT "X(10)":U
      tmpLager.Antall[14] FORMAT "X(10)":U
      tmpLager.Antall[15] FORMAT "X(10)":U
      tmpLager.Antall[16] FORMAT "X(10)":U
      tmpLager.Antall[17] FORMAT "X(10)":U
      tmpLager.Antall[18] FORMAT "X(10)":U
      tmpLager.Antall[19] FORMAT "X(10)":U
      tmpLager.Antall[20] FORMAT "X(10)":U
      tmpLager.Antall[21] FORMAT "X(10)":U
      tmpLager.Antall[22] FORMAT "X(10)":U
      tmpLager.Antall[23] FORMAT "X(10)":U
      tmpLager.Antall[24] FORMAT "X(10)":U
      tmpLager.Antall[25] FORMAT "X(10)":U
      tmpLager.Antall[26] FORMAT "X(10)":U
      tmpLager.Antall[27] FORMAT "X(10)":U
      tmpLager.Antall[28] FORMAT "X(10)":U
      tmpLager.Antall[29] FORMAT "X(10)":U
      tmpLager.Antall[30] FORMAT "X(10)":U
      tmpLager.Antall[31] FORMAT "X(10)":U
      tmpLager.Antall[32] FORMAT "X(10)":U
      tmpLager.Antall[33] FORMAT "X(10)":U
      tmpLager.Antall[34] FORMAT "X(10)":U
      tmpLager.Antall[35] FORMAT "X(10)":U
      tmpLager.Antall[36] FORMAT "X(10)":U
      tmpLager.Antall[37] FORMAT "X(10)":U
      tmpLager.Antall[38] FORMAT "X(10)":U
      tmpLager.Antall[39] FORMAT "X(10)":U
      tmpLager.Antall[40] FORMAT "X(10)":U
      tmpLager.Antall[41] FORMAT "X(10)":U
      tmpLager.Antall[42] FORMAT "X(10)":U
      tmpLager.Antall[43] FORMAT "X(10)":U
      tmpLager.Antall[44] FORMAT "X(10)":U
      tmpLager.Antall[45] FORMAT "X(10)":U
      tmpLager.Antall[46] FORMAT "X(10)":U
      tmpLager.Antall[47] FORMAT "X(10)":U
      tmpLager.Antall[48] FORMAT "X(10)":U
      tmpLager.Antall[49] FORMAT "X(10)":U
      tmpLager.Antall[50] FORMAT "X(10)":U
      tmpLager.Antall[51] FORMAT "X(10)":U
      tmpLager.Antall[52] FORMAT "X(10)":U
      tmpLager.Antall[53] FORMAT "X(10)":U
      tmpLager.Antall[54] FORMAT "X(10)":U
      tmpLager.Antall[55] FORMAT "X(10)":U
      tmpLager.Antall[56] FORMAT "X(10)":U
      tmpLager.Antall[57] FORMAT "X(10)":U
      tmpLager.Antall[58] FORMAT "X(10)":U
      tmpLager.Antall[59] FORMAT "X(10)":U
      tmpLager.Antall[60] FORMAT "X(10)":U
      tmpLager.Antall[61] FORMAT "X(10)":U
      tmpLager.Antall[62] FORMAT "X(10)":U
      tmpLager.Antall[63] FORMAT "X(10)":U
      tmpLager.Antall[64] FORMAT "X(10)":U
      tmpLager.Antall[65] FORMAT "X(10)":U
      tmpLager.Antall[66] FORMAT "X(10)":U
      tmpLager.Antall[67] FORMAT "X(10)":U
      tmpLager.Antall[68] FORMAT "X(10)":U
      tmpLager.Antall[69] FORMAT "X(10)":U
      tmpLager.Antall[70] FORMAT "X(10)":U
      tmpLager.Antall[71] FORMAT "X(10)":U
      tmpLager.Antall[72] FORMAT "X(10)":U
      tmpLager.Antall[73] FORMAT "X(10)":U
      tmpLager.Antall[74] FORMAT "X(10)":U
      tmpLager.Antall[75] FORMAT "X(10)":U
      tmpLager.Antall[76] FORMAT "X(10)":U
      tmpLager.Antall[77] FORMAT "X(10)":U
      tmpLager.Antall[78] FORMAT "X(10)":U
      tmpLager.Antall[79] FORMAT "X(10)":U
      tmpLager.Antall[80] FORMAT "X(10)":U
      tmpLager.Antall[81] FORMAT "X(10)":U
      tmpLager.Antall[82] FORMAT "X(10)":U
      tmpLager.Antall[83] FORMAT "X(10)":U
      tmpLager.Antall[84] FORMAT "X(10)":U
      tmpLager.Antall[85] FORMAT "X(10)":U
      tmpLager.Antall[86] FORMAT "X(10)":U
      tmpLager.Antall[87] FORMAT "X(10)":U
      tmpLager.Antall[88] FORMAT "X(10)":U
      tmpLager.Antall[89] FORMAT "X(10)":U
      tmpLager.Antall[90] FORMAT "X(10)":U
      tmpLager.Antall[91] FORMAT "X(10)":U
      tmpLager.Antall[92] FORMAT "X(10)":U
      tmpLager.Antall[93] FORMAT "X(10)":U
      tmpLager.Antall[94] FORMAT "X(10)":U
      tmpLager.Antall[95] FORMAT "X(10)":U
      tmpLager.Antall[96] FORMAT "X(10)":U
      tmpLager.Antall[97] FORMAT "X(10)":U
      tmpLager.Antall[98] FORMAT "X(10)":U
      tmpLager.Antall[99] FORMAT "X(10)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 206.2 BY 19.57.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     B-Kobble AT ROW 1.29 COL 155
     FI-ArtBut AT ROW 6.91 COL 126.2 COLON-ALIGNED NO-LABEL
     ArtBas.HoyLavMva AT ROW 5.95 COL 66.2 HELP
          "Overstyring av mva i kassen. Høy eller lav mva."
          LABEL "Høy/lav mva"
          VIEW-AS TOGGLE-BOX
          SIZE 17.8 BY .81 TOOLTIP "Overstyring av mva i kassen. Høy eller lav mva."
     ArtBas.Telefonkort AT ROW 7.52 COL 32
          LABEL "Telefonkort"
          VIEW-AS TOGGLE-BOX
          SIZE 15.6 BY .81 TOOLTIP "Artikkelen skal behandles i kasse som telefonkort"
     ArtBas.Grunnsortiment AT ROW 6.67 COL 66.2
          LABEL "Grunnsort."
          VIEW-AS TOGGLE-BOX
          SIZE 17.8 BY .81 TOOLTIP "Artikkelen inngår i grunnsortimentet"
     B-Jamfor AT ROW 7.57 COL 111.4 HELP
          "Åpner visning av vindu for sammenligning av artiklene i modell"
     ArtBas.NegVare AT ROW 6.71 COL 32
          LABEL "Neg.vare"
          VIEW-AS TOGGLE-BOX
          SIZE 15.6 BY .81
     ArtBas.NON_Sale AT ROW 6.67 COL 17.6
          VIEW-AS TOGGLE-BOX
          SIZE 13.4 BY .81 TOOLTIP "Artikkel er en non-sale artikkel. Gevinst o.l." NO-TAB-STOP 
     ArtBas.UtgattDato AT ROW 3.91 COL 146 COLON-ALIGNED
          LABEL "Utgår"
          VIEW-AS FILL-IN 
          SIZE 15.8 BY 1 TOOLTIP "Dato da artikkelen utgår."
     B-VPI AT ROW 1.33 COL 99 HELP
          "Åpner søk i VPI register (På angitt VPI leverandør)" NO-TAB-STOP 
     BUTTON-Ok AT ROW 1.33 COL 204 HELP
          "Lagre og avslutt" NO-TAB-STOP 
     BUTTON-SettLopNr AT ROW 1.33 COL 125.4 HELP
          "Tildeler artikkel et løpenummer i vareguppen" NO-TAB-STOP 
     BUTTON-SokLev AT ROW 2.86 COL 106.2 HELP
          "Søk i leverandørregister"
     BUTTON-TilKasse AT ROW 1.33 COL 42.8 HELP
          "Send artikkel till kassene"
     ArtBas.ManueltOpprettet AT ROW 7.62 COL 128.2 HELP
          "Artikkel er manuelt opprettet (ikke importert via VPI mottaket)"
          LABEL "Man. opprettet"
          VIEW-AS TOGGLE-BOX
          SIZE 18.8 BY .81 TOOLTIP "Artikkel er manuelt opprettet (ikke importert via VPI mottaket)" NO-TAB-STOP 
     B-Folder AT ROW 1.24 COL 186.8 HELP
          "Åpner/lukker foldere i artikkelkortet. Lagring av valgt oppsett"
     B-Sanera AT ROW 1.24 COL 171.6 HELP
          "Overfører all artikkelinformasjon og statistikk til annen artik"
     BUTTON-Kopierbilde AT ROW 5.14 COL 174.6 NO-TAB-STOP 
     BUTTON-Slettbilde AT ROW 6.24 COL 174.6 NO-TAB-STOP 
     ArtBas.Gjennomfaktureres AT ROW 6.67 COL 104 HELP
          "Artikkelen gjennomfaktureres via kjedekontoret"
          LABEL "Gjennomfaktureres"
          VIEW-AS TOGGLE-BOX
          SIZE 22.4 BY .81 TOOLTIP "Artikkelen gjennomfaktureres via kjedekontoret" NO-TAB-STOP 
     ArtBas.KjedeVare AT ROW 5.95 COL 104 HELP
          "Kjedelevert. Varen kan bestilles fra kjedens sentrallager."
          LABEL "Kjedelevert"
          VIEW-AS TOGGLE-BOX
          SIZE 22 BY .81 TOOLTIP "Kjedelevert. Varen kan bestilles fra kjedens sentrallager." NO-TAB-STOP 
     B-Sok AT ROW 1.33 COL 38.2 NO-TAB-STOP 
     Btn_Help AT ROW 1.33 COL 199.4 NO-TAB-STOP 
     BUTTON-Angre AT ROW 1.33 COL 20 NO-TAB-STOP 
     BUTTON-Kopier AT ROW 1.33 COL 6.2 NO-TAB-STOP 
     BUTTON-Lagre AT ROW 1.33 COL 15.4 NO-TAB-STOP 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 209.2 BY 32.57.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME DEFAULT-FRAME
     BUTTON-ModellFarg AT ROW 1.33 COL 33.8 NO-TAB-STOP 
     BUTTON-Next AT ROW 1.33 COL 29.2 NO-TAB-STOP 
     BUTTON-Ny AT ROW 1.33 COL 1.4 NO-TAB-STOP 
     BUTTON-Paste AT ROW 2.86 COL 174.6 NO-TAB-STOP 
     BUTTON-Prev AT ROW 1.33 COL 24.6 NO-TAB-STOP 
     BUTTON-Slett AT ROW 1.33 COL 10.8 NO-TAB-STOP 
     BUTTON-SokFil AT ROW 4 COL 174.6 NO-TAB-STOP 
     ArtBas.BestForslag AT ROW 6.76 COL 49.2 HELP
          "Artikkelen skal være med i bestillingsforslag"
          LABEL "Best.forslag"
          VIEW-AS TOGGLE-BOX
          SIZE 16.8 BY .81 TOOLTIP "Artikkelen skal være med i bestillingsforslag" NO-TAB-STOP 
     ArtBas.IndividType AT ROW 4.91 COL 140.8
          LABEL "Individ"
          VIEW-AS COMBO-BOX INNER-LINES 25
          LIST-ITEM-PAIRS "Ordinær",0,
                     "Serienr",1,
                     "Serienr tvang",2
          DROP-DOWN-LIST
          SIZE 15.8 BY 1
          BGCOLOR 17 FONT 6
     CB-ModellFarge AT ROW 7.48 COL 55 COLON-ALIGNED HELP
          "Valg av artikkel i modell som skal vises i artikkelkortet"
     ArtBas.ArtikkelNr AT ROW 2.91 COL 12.2 COLON-ALIGNED HELP
          "Systemets interne artikkelnummer. Tildeles automatisk."
          LABEL "ArtikkelNr"
          VIEW-AS FILL-IN NATIVE 
          SIZE 23.4 BY 1 TOOLTIP "Systemets interne artikkelnummer. Tildeles automatisk."
     HuvGr.Hg AT ROW 2.91 COL 36 COLON-ALIGNED HELP
          "Artikkelens hovedgruppetilhørighet." NO-LABEL
          VIEW-AS FILL-IN NATIVE 
          SIZE 7 BY 1 TOOLTIP "Artikkelens hovedgruppetilhørighet."
     HuvGr.HgBeskr AT ROW 2.91 COL 43 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN NATIVE 
          SIZE 34.2 BY 1 TOOLTIP "Hovedgruppebeskrivelse"
     ArtBas.Vg AT ROW 3.91 COL 12.2 COLON-ALIGNED HELP
          "Artikkelens varegruppetilhøringhet (F10 Oppslag)"
          LABEL "Varegr" FORMAT "zzzzz9"
          VIEW-AS FILL-IN 
          SIZE 9.6 BY 1 TOOLTIP "Artikkelens varegruppetilhøringhet"
          BGCOLOR 18 FONT 6
     CB-EkstVPILev AT ROW 1.38 COL 68 COLON-ALIGNED HELP
          "Viser hvilken VPI leverandør søk i VPI register gjøres mot." NO-TAB-STOP 
     ArtBas.VgKat AT ROW 3.91 COL 26.2 COLON-ALIGNED HELP
          "Artikkelens kategoritilhøringhet innenfor varegruppen (F10)" NO-LABEL FORMAT "z9"
          VIEW-AS FILL-IN 
          SIZE 5.4 BY 1 TOOLTIP "Artikkelens kategoritilhøringhet innenfor varegruppen"
     ArtBas.LopNr AT ROW 4.91 COL 12.2 COLON-ALIGNED HELP
          "Artikkelens løpenummer innenfor varegruppen"
          LABEL "Løpenr" FORMAT "zzzzz9"
          VIEW-AS FILL-IN 
          SIZE 19.4 BY 1 TOOLTIP "Artikkelens løpenummer innenfor varegruppen"
          BGCOLOR 18 FONT 6
     ArtBas.Pant AT ROW 6 COL 32
          LABEL "Pantvare"
          VIEW-AS TOGGLE-BOX
          SIZE 15.6 BY .81 NO-TAB-STOP 
     ArtBas.Beskr AT ROW 4.91 COL 36 COLON-ALIGNED HELP
          "Varens varetekst. Benyttes i søkelister" NO-LABEL FORMAT "x(100)"
          VIEW-AS FILL-IN NATIVE 
          SIZE 41.2 BY 1 TOOLTIP "Varens varetekst. Benyttes i søkelister"
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 209.2 BY 32.57.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME DEFAULT-FRAME
     FILL-IN-VgBeskr AT ROW 3.91 COL 36 COLON-ALIGNED NO-LABEL
     ArtBas.LevNr AT ROW 2.91 COL 93.6 COLON-ALIGNED HELP
          "Leverandørnummer på artikkelens leverandør (F10 Oppslag)"
          LABEL "Leverandør" FORMAT "zzzzz9"
          VIEW-AS FILL-IN NATIVE 
          SIZE 10.6 BY 1 TOOLTIP "Leverandørnummer på artikkelens leverandør"
     FILL-IN-LevNamn AT ROW 2.91 COL 108.6 COLON-ALIGNED NO-LABEL
     ArtBas.LevKod AT ROW 3.91 COL 93.6 COLON-ALIGNED HELP
          "Leverandørs bestillingsnummer og/eller modellnummer"
          LABEL "Lev.art.nr" FORMAT "x(200)"
          VIEW-AS FILL-IN NATIVE 
          SIZE 44.4 BY 1 TOOLTIP "Leverandørs bestillingsnummer og/eller modellnummer"
     ArtBas.lager AT ROW 6.67 COL 2 HELP
          "Endre mellom lagerstyrt og ikke lagerstyrt"
          LABEL "Lagerstyrt"
          VIEW-AS TOGGLE-BOX
          SIZE 15 BY .81 TOOLTIP "Endre mellom lagerstyrt og ikke lagerstyrt" NO-TAB-STOP 
     FI-PaTilbud AT ROW 7.81 COL 178 COLON-ALIGNED NO-LABEL
     ArtBas.Pakke AT ROW 6 COL 17.6
          LABEL "Pakke"
          VIEW-AS TOGGLE-BOX
          SIZE 13 BY .81 NO-TAB-STOP 
     ArtBas.Utgatt AT ROW 3.86 COL 164.2
          LABEL ""
          VIEW-AS TOGGLE-BOX
          SIZE 2.8 BY .67 TOOLTIP "Artikkel som er eller skal utgå fra kjedens sortiment." NO-TAB-STOP 
     TOGGLE-Annonse AT ROW 5.95 COL 49.2 HELP
          "Brukes til å merke opp artikler er på leverandørkampanje" NO-TAB-STOP 
     ArtBas.IKasse AT ROW 6.67 COL 85.6 HELP
          "Åpner for at varen sendes til kassene"
          VIEW-AS TOGGLE-BOX
          SIZE 16.8 BY .81 TOOLTIP "Åpner for at varen sendes til kassene" NO-TAB-STOP 
     ArtBas.BildeIKasse AT ROW 6 COL 85.6 HELP
          "Bilde på artikkel overføres til kasse"
          VIEW-AS TOGGLE-BOX
          SIZE 16.4 BY .81 TOOLTIP "Bilde på artikkel overføres til kasse" NO-TAB-STOP 
     ArtBas.OPris AT ROW 5.95 COL 2
          VIEW-AS TOGGLE-BOX
          SIZE 13.6 BY .81 NO-TAB-STOP 
     BUTTON-SokVg AT ROW 3.91 COL 23.8
     BUTTON-SokVgKat AT ROW 3.91 COL 33.6
     ArtBas.HkStyrt AT ROW 6.05 COL 150.8 HELP
          "Varen er opprettet på kjedesentral"
          VIEW-AS TOGGLE-BOX
          SIZE 15.2 BY .81 TOOLTIP "Varen er opprettet på kjedesentral" NO-TAB-STOP 
     ArtBas.KjentPaHK AT ROW 7.52 COL 150.8
          VIEW-AS TOGGLE-BOX
          SIZE 14.6 BY .81 NO-TAB-STOP 
     ArtBas.LokPris AT ROW 6.81 COL 150.8
          LABEL "Lokal pris"
          VIEW-AS TOGGLE-BOX
          SIZE 15.2 BY .81 NO-TAB-STOP 
     FI-LevFargKod AT ROW 4.91 COL 93.6 COLON-ALIGNED HELP
          "Leverandørens fargekode og betegnelse på farge"
     ArtBas.SanertDato AT ROW 2.91 COL 146 COLON-ALIGNED HELP
          "Artikkel er sanert og all informasjon er overført til en annen"
          VIEW-AS FILL-IN NATIVE 
          SIZE 15.8 BY 1 TOOLTIP "Artikkel er sanert og all informasjon er overført til en annen artikkel" NO-TAB-STOP 
     TB-WebButikkArtikkel AT ROW 6.1 COL 128.2 HELP
          "Aktiv i nettbutikk" NO-TAB-STOP 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 209.2 BY 32.57.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME DEFAULT-FRAME
     ArtBas.ArtSlag AT ROW 7.48 COL 10.6 COLON-ALIGNED HELP
          "Artikkelens varetype. Stk, vekt, meter, meter2 eller volum."
          LABEL "Varetype"
          VIEW-AS COMBO-BOX INNER-LINES 25
          LIST-ITEM-PAIRS "Item 1",0
          DROP-DOWN-LIST
          SIZE 18.4 BY 1 TOOLTIP "Artikkelens varetype. Stk, vekt, meter, meter2 eller volum."
          BGCOLOR 17  NO-TAB-STOP 
     RECT-27 AT ROW 1.14 COL 1
     RECT-28 AT ROW 2.43 COL 1
     RECT-34 AT ROW 2.62 COL 173
     RECT-35 AT ROW 5.95 COL 1
     RECT-5 AT ROW 2.62 COL 1.2
     RECT-55 AT ROW 5.95 COL 149
     RECT-56 AT ROW 5.95 COL 48
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 209.2 BY 32.57.

DEFINE FRAME FRAME-Lager
     B-Svinn AT ROW 1.33 COL 19
     B-Nedskrivning AT ROW 1.33 COL 32.6
     B-ForenklVaremot AT ROW 1.33 COL 53
     B-Chart AT ROW 1.33 COL 80.6
     B-OppdLAger AT ROW 1.33 COL 88.4
     BUTTON-Overfor AT ROW 1.33 COL 114.4
     B-Lagerjustering AT ROW 1.33 COL 1.8
     B-Reklamer AT ROW 1.33 COL 136.4
     B-ByggomLager AT ROW 1.33 COL 159.4
     TG-VisSkjul AT ROW 1.48 COL 189.2
     BROWSE-Lager AT ROW 2.67 COL 1.8
     RS-Vis AT ROW 22.62 COL 3 NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 9.86
         SIZE 207.8 BY 23.1.

DEFINE FRAME FRAME-Info2
     BUTTON-SokLandKode AT ROW 8.14 COL 138.2 WIDGET-ID 8
     ArtBas.LinjeMerknad AT ROW 22.81 COL 2 COLON-ALIGNED HELP
          "Melding fra leverandør" NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 86.4 BY 1 TOOLTIP "Melding fra leverandør"
     ArtBas.VPIDato AT ROW 2.67 COL 28 COLON-ALIGNED HELP
          "Dato da det sist ble lest inn VPI fra pricat melding"
          LABEL "Dato siste VPI innlesning"
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1 TOOLTIP "Dato da det sist ble lest inn VPI fra pricat melding"
     ArtBas.TilgjengeligFraLev AT ROW 4.33 COL 28 COLON-ALIGNED HELP
          "Første dato da artikkelen kan bestilles fra leverandør"
          LABEL "Tilgjengelig fra leverandør"
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1 TOOLTIP "Første dato da artikkelen kan bestilles fra leverandør"
     ArtBas.LevDato1 AT ROW 5.33 COL 28 COLON-ALIGNED
          LABEL "1. Leveringsdato"
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     ArtBas.LevDato2 AT ROW 6.33 COL 28 COLON-ALIGNED
          LABEL "2. Leveringsdato"
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     ArtBas.LevDato3 AT ROW 7.33 COL 28 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     ArtBas.LevDato4 AT ROW 8.33 COL 28 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     ArtBas.KatalogPris AT ROW 10 COL 28 COLON-ALIGNED HELP
          "Pris som er oppgitt i leverandørens varekatalog"
          LABEL "Katalogpris(Engros)" FORMAT "->>>>9.99"
          VIEW-AS FILL-IN 
          SIZE 17.4 BY 1 TOOLTIP "Pris som er oppgitt i leverandørens varekatalog"
     ArtBas.AnbefaltPris AT ROW 11 COL 28 COLON-ALIGNED HELP
          "Veiledende pris fra kjede eller leverandør."
          LABEL "Veiledende pris (Prispunkt)"
          VIEW-AS FILL-IN 
          SIZE 17.4 BY 1 TOOLTIP "Veiledende pris fra kjede eller leverandør."
     ArtBas.KjedeInnkPris AT ROW 12 COL 28 COLON-ALIGNED HELP
          "Kjedens innkjøpspris fra leverandør"
          LABEL "Kjedens innkj.pris" FORMAT "->>>>9.99"
          VIEW-AS FILL-IN 
          SIZE 17.4 BY 1 TOOLTIP "Kjedens innkjøpspris fra leverandør"
     ArtBas.KjedeValutaPris AT ROW 13 COL 28 COLON-ALIGNED HELP
          "Kjedens innkjøpspris fra leverandør i leverandørs valuta"
          LABEL "Kjede val.pris"
          VIEW-AS FILL-IN 
          SIZE 17.4 BY 1 TOOLTIP "Kjedens innkjøpspris fra leverandør i leverandørs valuta"
     ArtBas.Sortimentkoder AT ROW 15.05 COL 28 COLON-ALIGNED
          LABEL "Sortimentskoder"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     ArtBas.Lagerkoder AT ROW 16.05 COL 28 COLON-ALIGNED
          LABEL "Lagerkoder"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     ArtBas.Kampanjeuker AT ROW 17.05 COL 28 COLON-ALIGNED
          LABEL "Kampanjeuker"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     ArtBas.Kampanjestotte AT ROW 18.05 COL 28 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     ArtBas.EkstStrTypeNavn AT ROW 19.14 COL 28 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     ArtBas.VPIBildeKode AT ROW 20.76 COL 27.8 COLON-ALIGNED HELP
          "Navn på bildefil. Leses inn med VPI fra pricat." FORMAT "X(200)"
          VIEW-AS FILL-IN 
          SIZE 60.2 BY 1 TOOLTIP "Navn på bildefil. Leses inn med VPI fra pricat."
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 9.86
         SIZE 207.8 BY 23.1.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME FRAME-Info2
     ArtBas.LevDatoStopp1 AT ROW 5.33 COL 70.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     ArtBas.LevDatoStopp2 AT ROW 6.33 COL 70.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     ArtBas.LevDatoStopp3 AT ROW 7.33 COL 70.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     ArtBas.LevDatoStopp4 AT ROW 8.33 COL 70.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     ArtBas.forhRab% AT ROW 10 COL 70.2 COLON-ALIGNED HELP
          "Forhåndsrabatt fra kjede eller leverandør"
          LABEL "Forhåndsrabatt"
          VIEW-AS FILL-IN 
          SIZE 17.4 BY 1 TOOLTIP "Forhåndsrabatt fra kjede eller leverandør"
     ArtBas.supRab% AT ROW 11 COL 70.2 COLON-ALIGNED HELP
          "Suppleringsrabatt fra kjede eller leverandør"
          LABEL "Suppleringsrabatt"
          VIEW-AS FILL-IN 
          SIZE 17.4 BY 1 TOOLTIP "Suppleringsrabatt fra kjede eller leverandør"
     ArtBas.KjedeRab% AT ROW 12 COL 70.2 COLON-ALIGNED HELP
          "Kjedens rabatt fra leverandør"
          LABEL "Kjedens rabatt%"
          VIEW-AS FILL-IN 
          SIZE 17.4 BY 1 TOOLTIP "Kjedens rabatt fra leverandør"
     ArtBas.KjedeProdusent AT ROW 13 COL 70.2 COLON-ALIGNED HELP
          "Produsent som kjeden har benyttet på varen"
          VIEW-AS FILL-IN 
          SIZE 17.4 BY 1 TOOLTIP "Produsent som kjeden har benyttet på varen"
     ArtBas.VareFakta AT ROW 10.33 COL 94.4 HELP
          "Artikkelens varefakta" NO-LABEL
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          SIZE 112.6 BY 13.57 TOOLTIP "Artikkelens varefakta"
     ArtBas.PostBredde AT ROW 2.62 COL 119 COLON-ALIGNED FORMAT ">,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 17.4 BY 1
     ArtBas.PostHoyde AT ROW 3.62 COL 119 COLON-ALIGNED FORMAT ">,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 17.4 BY 1
     ArtBas.PostLengde AT ROW 4.62 COL 119 COLON-ALIGNED FORMAT ">,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 17.4 BY 1
     ArtBas.PostVekt AT ROW 5.62 COL 119 COLON-ALIGNED FORMAT ">>,>>9.999"
          VIEW-AS FILL-IN 
          SIZE 17.4 BY 1
     ArtBas.AlfaKode2 AT ROW 8.14 COL 119 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17.4 BY 1
     S-Karakteristikk AT ROW 2.67 COL 140 HELP
          "Andre leverandører som artikkelen kan kjøpes inn fra (F10)" NO-LABEL WIDGET-ID 16 NO-TAB-STOP 
     FI-Land AT ROW 8.14 COL 140.6 COLON-ALIGNED NO-LABEL
     BUTTON-Karakteristikk AT ROW 2.67 COL 202.4 WIDGET-ID 14
     BUTTON-SokLevDat-3 AT ROW 4.33 COL 43
     BUTTON-SokLevDat-4 AT ROW 2.67 COL 43
     BUTTON-SokLevDat-5 AT ROW 5.33 COL 85.2
     BUTTON-SokLevDat-6 AT ROW 6.33 COL 85.2
     BUTTON-SokLevDat-7 AT ROW 7.33 COL 85.2
     BUTTON-SokLevDat-8 AT ROW 8.33 COL 85.2
     BUTTON-SokLevDat-2 AT ROW 5.33 COL 43
     BUTTON-SokLevDato-3 AT ROW 6.33 COL 43
     BUTTON-SokLevDato-4 AT ROW 7.33 COL 43
     BUTTON-SokLevDato-5 AT ROW 8.33 COL 43
     FI-VpiInfoTxt AT ROW 1.67 COL 3.2 NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 9.86
         SIZE 207.8 BY 23.1.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME FRAME-Info2
     Txt-Linjemerknad AT ROW 22.14 COL 3 COLON-ALIGNED NO-LABEL
     FI-VpiInfoTxt-2 AT ROW 1.62 COL 94.2 NO-LABEL
     Txt-OpprLand AT ROW 7.33 COL 94 NO-LABEL
     Txt-Varefakta AT ROW 9.48 COL 94 NO-LABEL
     FI-VpiInfoTxt-3 AT ROW 1.62 COL 140.4 NO-LABEL
     RECT-33 AT ROW 1.48 COL 2
     RECT-36 AT ROW 9.33 COL 93
     RECT-69 AT ROW 1.48 COL 93
     RECT-72 AT ROW 7 COL 93 WIDGET-ID 4
     RECT-73 AT ROW 1.48 COL 139.2 WIDGET-ID 18
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 9.86
         SIZE 207.8 BY 23.1.

DEFINE FRAME FRAME-ArtInfo
     ArtBas.OnLineLevNr AT ROW 9.86 COL 106 COLON-ALIGNED HELP
          "OnLine leverandørnummer" WIDGET-ID 18
          LABEL "OnLine levnr"
          VIEW-AS COMBO-BOX INNER-LINES 25
          LIST-ITEM-PAIRS "0",1
          DROP-DOWN-LIST
          SIZE 43 BY 1 TOOLTIP "OnLine leverandør"
     ArtBas.LinkVareAnt AT ROW 7.43 COL 126.2 COLON-ALIGNED NO-LABEL FORMAT "z9"
          VIEW-AS FILL-IN 
          SIZE 4.6 BY 1
     ArtBas.SalgsStopp AT ROW 12.19 COL 179 COLON-ALIGNED NO-LABEL WIDGET-ID 14
          VIEW-AS COMBO-BOX INNER-LINES 25
          LIST-ITEM-PAIRS "Åpen",0,
                     "Mykt stop",1,
                     "Hård stop",2
          DROP-DOWN-LIST
          SIZE 20 BY 1
     ArtBas.KundeRabatt AT ROW 10.19 COL 158
          LABEL "Kunderabatt"
          VIEW-AS TOGGLE-BOX
          SIZE 20 BY .81 TOOLTIP "Åpner for automatisk kunderabatt i kasse"
     ArtBas.ManRabIKas AT ROW 9.38 COL 158
          LABEL "Manuell rabatt"
          VIEW-AS TOGGLE-BOX
          SIZE 20 BY .81 TOOLTIP "Åpner for manuell rabatt i kasse"
     B-SokLinkvare-2 AT ROW 17.91 COL 151 HELP
          "Åpne infoside for artikkel" NO-TAB-STOP 
     ArtBas.MengdeRabatt AT ROW 9.38 COL 179.4
          LABEL "Mengderabatt"
          VIEW-AS TOGGLE-BOX
          SIZE 20 BY .81 TOOLTIP "Åpner for mengderabatt i kasse"
     ArtBas.Bonus_Givende AT ROW 10.19 COL 179.4
          LABEL "Rabatt grunnlag"
          VIEW-AS TOGGLE-BOX
          SIZE 20 BY .81 TOOLTIP "Artikkel inngår i rabattgrunnlag for medlemmer"
     ArtBas.PubliserINettbutikk AT ROW 17.43 COL 178.8
          LABEL "Publiser"
          VIEW-AS TOGGLE-BOX
          SIZE 13.2 BY .81
     FI-LinkVareTekst AT ROW 7.43 COL 131 COLON-ALIGNED NO-LABEL
     ArtBas.StrTypeID AT ROW 2.52 COL 14.2 COLON-ALIGNED HELP
          "Artikkelens tilhøringet til størrelsestypeinndeling (F10-Oppsl)"
          LABEL "StrType" FORMAT "zzzzz9"
          VIEW-AS FILL-IN 
          SIZE 9.8 BY 1 TOOLTIP "Styrer artikkelens tilhøringet til størrelsestypeinndeling"
     ArtBas.SaSong AT ROW 3.43 COL 14.2 COLON-ALIGNED HELP
          "Sesong som artikkelen hørere til (F10-Oppslag)" FORMAT ">>>>>9"
          VIEW-AS FILL-IN 
          SIZE 9.8 BY 1 TOOLTIP "Sesong som artikkelen hørere til"
     ArtBas.Farg AT ROW 4.43 COL 14.2 COLON-ALIGNED HELP
          "Grunnfarge som artikkelen har (F10-Oppslag)"
          LABEL "Farge" FORMAT "zzzz9"
          VIEW-AS FILL-IN 
          SIZE 9.8 BY 1 TOOLTIP "Grunnfarge som artikkelen har"
     ArtBas.MatKod AT ROW 5.43 COL 14.2 COLON-ALIGNED HELP
          "Materialkode som beskriver artikkelens materiale (F10-Oppslag)"
          LABEL "Material"
          VIEW-AS FILL-IN 
          SIZE 9.8 BY 1 TOOLTIP "Materialkode som beskriver artikkelens materiale"
     ArtBas.Klack AT ROW 6.43 COL 14.2 COLON-ALIGNED HELP
          "Hælkode som beskriver hælens høyde (F10-Oppslag)"
          VIEW-AS FILL-IN 
          SIZE 9.8 BY 1 TOOLTIP "Hælkode som beskriver hælens høyde"
     ArtBas.inner-id AT ROW 7.43 COL 14.2 COLON-ALIGNED HELP
          "Kode som beskriver artikkelens innersåle (F10-Oppslag)"
          LABEL "Innersåle"
          VIEW-AS FILL-IN 
          SIZE 9.8 BY 1 TOOLTIP "Kode som beskriver artikkelens innersåle"
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 9.86
         SIZE 208.8 BY 23.1.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME FRAME-ArtInfo
     ArtBas.ov-id AT ROW 8.43 COL 14.2 COLON-ALIGNED HELP
          "Kode som beskriver innerfor"
          LABEL "Innerfor"
          VIEW-AS FILL-IN 
          SIZE 9.8 BY 1 TOOLTIP "Kode som beskriver innerfor"
     ArtBas.slit-id AT ROW 9.43 COL 14.2 COLON-ALIGNED HELP
          "Kode som beksriver slitesåle (F10-Oppslag)"
          LABEL "Slitesåle"
          VIEW-AS FILL-IN 
          SIZE 9.8 BY 1 TOOLTIP "Kode som beksriver slitesåle"
     ArtBas.last-id AT ROW 10.43 COL 14.2 COLON-ALIGNED HELP
          "Kode som beskriver læst"
          LABEL "Læst"
          VIEW-AS FILL-IN 
          SIZE 9.8 BY 1 TOOLTIP "Kode som beskriver læst"
     BUTTON-NyStrType AT ROW 2.38 COL 30
     ArtBas.anv-id AT ROW 11.43 COL 14.2 COLON-ALIGNED HELP
          "Kode som beskriver artikkelens bruksområde (F10-Oppslag)"
          LABEL "Brukskode"
          VIEW-AS FILL-IN 
          SIZE 9.8 BY 1 TOOLTIP "Kode som beskriver artikkelens bruksområde"
     ArtBas.VMId AT ROW 12.43 COL 14.2 COLON-ALIGNED HELP
          "Kode som beskriver artikkelens varemerke (F10-Oppslag)"
          LABEL "Varemerke" FORMAT "zzzzz9"
          VIEW-AS FILL-IN 
          SIZE 9.8 BY 1 TOOLTIP "Kode som beskriver artikkelens varemerke"
     ArtBas.ProdNr AT ROW 13.43 COL 14.2 COLON-ALIGNED HELP
          "Kode som beskriver artikkelens produsent (F10 Oppslag)"
          VIEW-AS FILL-IN 
          SIZE 9.8 BY 1 TOOLTIP "Kode som beskriver artikkelens produsent"
     ArtBas.HovedKatNr AT ROW 14.48 COL 14.2 COLON-ALIGNED HELP
          "Kobling av artikkel til hovedkategori"
          LABEL "Hov.kategori"
          VIEW-AS FILL-IN 
          SIZE 9.8 BY 1 TOOLTIP "Hovedkategori som artikkelen tilhører"
     CB-Mellankategori AT ROW 15.67 COL 14.2 COLON-ALIGNED
     B-alternativeLev AT ROW 8.48 COL 151 NO-TAB-STOP 
     CB-MUkat AT ROW 16.86 COL 14 COLON-ALIGNED
     ArtBas.valkod AT ROW 17.95 COL 14.2 COLON-ALIGNED HELP
          "Valuta som artikkelen kjøpes i (F10 Oppslag)"
          LABEL "Valuta"
          VIEW-AS FILL-IN 
          SIZE 9.8 BY 1 TOOLTIP "Valuta som artikkelen kjøpes i"
     ArtBas.Notat AT ROW 19.81 COL 2 HELP
          "Notat om leveringsbetingelser og lignende" NO-LABEL
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          SIZE 76 BY 2.81 TOOLTIP "Notatfelt for angivelse av spesielle leveringsbetingelser og lignende"
     StrType.Beskrivelse AT ROW 2.43 COL 32.6 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 21 BY 1
     SaSong.SasBeskr AT ROW 3.43 COL 28 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 25.6 BY 1
     Farg.FarBeskr AT ROW 4.43 COL 28 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 25.6 BY 1
     Material.MatBeskr AT ROW 5.43 COL 28 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 25.6 BY 1
     B-Refresh AT ROW 2.48 COL 151 NO-TAB-STOP 
     klack.beskrivning AT ROW 6.43 COL 28.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 25.6 BY 1
     InnerSula.InnerBeskr AT ROW 7.43 COL 28.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 25.6 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 9.86
         SIZE 208.8 BY 23.1.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME FRAME-ArtInfo
     Ovandel.OvBeskr AT ROW 8.43 COL 28.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 25.6 BY 1
     B-SokLinkvare AT ROW 7.43 COL 151 NO-TAB-STOP 
     SlitSula.SlitBeskr AT ROW 9.43 COL 28.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 25.6 BY 1
     Last-Sko.LastBeskr AT ROW 10.43 COL 28.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 25.6 BY 1
     Anv-Kod.AnvBeskr AT ROW 11.43 COL 28.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 25.6 BY 1
     Varemerke.Beskrivelse AT ROW 12.43 COL 28.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 48.2 BY 1
     BUTTON-NyLevsort AT ROW 10.95 COL 151.2
     Produsent.Beskrivelse AT ROW 13.43 COL 28.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 48.2 BY 1
     Handtering.Beskrivelse AT ROW 14.95 COL 116 COLON-ALIGNED NO-LABEL DISABLE-AUTO-ZAP 
          VIEW-AS FILL-IN 
          SIZE 33 BY 1
     Regnskapsavdeling.RAvdBeskrivelse AT ROW 15.95 COL 116 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 33 BY 1
     Valuta.ValKurs AT ROW 17.95 COL 28.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 16.2 BY 1
     BUTTON-SokBruk AT ROW 11.48 COL 25.8
     FI-1 AT ROW 3.43 COL 54 COLON-ALIGNED HELP
          "Leverandørens sesongbetegnelse" NO-LABEL
     ArtBas.LevFargKod AT ROW 4.43 COL 54 COLON-ALIGNED HELP
          "Leverandørens fargebetegnelse" NO-LABEL FORMAT "X(200)"
          VIEW-AS FILL-IN 
          SIZE 22.2 BY 1
     FI-2 AT ROW 5.43 COL 54 COLON-ALIGNED HELP
          "Leverandørens materialbetegnelse" NO-LABEL
     FI-3 AT ROW 6.43 COL 54 COLON-ALIGNED HELP
          "Leverandørens betegnelse på hæl" NO-LABEL
     FI-4 AT ROW 7.43 COL 54 COLON-ALIGNED HELP
          "Leverandørens betegnelse på innersåle" NO-LABEL
     FI-5 AT ROW 8.43 COL 54 COLON-ALIGNED HELP
          "Leverandørens betegnelse på innerfor" NO-LABEL
     BUTTON-SokInner AT ROW 7.48 COL 25.8
     FI-6 AT ROW 9.43 COL 54 COLON-ALIGNED HELP
          "Leverandørens betegnelse på slitesåle" NO-LABEL
     FI-7 AT ROW 10.43 COL 54 COLON-ALIGNED HELP
          "Leverandørens betegnelse på læst" NO-LABEL
     FI-8 AT ROW 11.43 COL 54 COLON-ALIGNED HELP
          "Leverandørens betegnelse på brukskode" NO-LABEL
     ArtBas.BongTekst AT ROW 2.29 COL 106 COLON-ALIGNED HELP
          "Tekst som vises på varelinje på kassens kvitteringsutskrift" FORMAT "X(20)"
          VIEW-AS FILL-IN 
          SIZE 43 BY 1 TOOLTIP "Tekst som vises på varelinje på kassens kvitteringsutskrift"
     ArtBas.Etikettekst1 AT ROW 3.43 COL 106 COLON-ALIGNED HELP
          "Varenavn som brukes som etikettekst"
          LABEL "Etikettekst 1"
          VIEW-AS FILL-IN 
          SIZE 43 BY 1 TOOLTIP "Varenavn som brukes som etikettekst"
     ArtBas.Etikettekst2 AT ROW 4.43 COL 106 COLON-ALIGNED HELP
          "Tekstlinje 2 på etikett. Vareinformasjon. Liten skrift."
          LABEL "Etikettekst 2"
          VIEW-AS FILL-IN 
          SIZE 43 BY 1 TOOLTIP "Tekstlinje 2 på etikett. Vareinformasjon. Kommer med liten skrift."
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 9.86
         SIZE 208.8 BY 23.1.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME FRAME-ArtInfo
     ArtBas.Etikett AT ROW 5.43 COL 106 COLON-ALIGNED HELP
          "Antall etiketter som skal skrives ut ved varemottak"
          LABEL "Antall etiketter"
          VIEW-AS COMBO-BOX INNER-LINES 25
          LIST-ITEM-PAIRS "Item 1",1
          DROP-DOWN-LIST
          SIZE 43 BY 1 TOOLTIP "Antall etiketter som skal skrives ut ved varemottak"
     BUTTON-SokKlak AT ROW 6.43 COL 25.8
     ArtBas.Lokasjon AT ROW 6.43 COL 106 COLON-ALIGNED HELP
          "Kode som identifiserer plassering i butikk og/eller på lager"
          VIEW-AS FILL-IN 
          SIZE 43 BY 1 TOOLTIP "Kode som identifiserer plassering i butikk og/eller på lager"
     ArtBas.LinkVareNr AT ROW 7.43 COL 106 COLON-ALIGNED HELP
          "Artikkelnr. på artikkel som representerer pant på varen (F10)"
          LABEL "Link til pantevare"
          VIEW-AS FILL-IN NATIVE 
          SIZE 20.2 BY 1 TOOLTIP "Artikkelnr. på artikkel som representerer pant på varen"
     CB-Levsort AT ROW 10.86 COL 106 COLON-ALIGNED HELP
          "Liste over tilgjengelige leverandørsinndelinger (F10 Oppslag)"
     ArtBas.GarantiKl AT ROW 11.91 COL 106 COLON-ALIGNED HELP
          "Garantibetingelser på artikkelen"
          LABEL "Garanti"
          VIEW-AS COMBO-BOX INNER-LINES 25
          LIST-ITEM-PAIRS "Item 1",0
          DROP-DOWN-LIST
          SIZE 43 BY 1 TOOLTIP "Garantibetingelser på artikkelen"
     BUTTON-SokLalst AT ROW 10.48 COL 25.8
     ArtBas.Alder AT ROW 12.91 COL 106 COLON-ALIGNED HELP
          "Krav til alder ved legimitasjonsplikt"
          LABEL "Legitimasjonsplikt"
          VIEW-AS COMBO-BOX INNER-LINES 25
          LIST-ITEM-PAIRS "Ingen leg.plikt",0,
                     "Leg.plikt",1
          DROP-DOWN-LIST
          SIZE 43 BY 1 TOOLTIP "Krav til alder ved legimitasjonsplikt"
     ArtBas.SalgsEnhet AT ROW 13.91 COL 106 COLON-ALIGNED HELP
          "Artikkelens salgsenhetstekst"
          LABEL "Salgsenhetstekst" FORMAT "X(30)"
          VIEW-AS COMBO-BOX INNER-LINES 25
          LIST-ITEMS "Item 1" 
          DROP-DOWN-LIST
          SIZE 43 BY 1 TOOLTIP "Artikkelens salgsenhetstekst"
     ArtBas.BehKode AT ROW 14.95 COL 106 COLON-ALIGNED HELP
          "Behandling ved reklamasjon (F10 Oppslag)"
          LABEL "Behanl.kode"
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1 TOOLTIP "Kode som beskriver hvordan varen skal håndteres ved reklamasjon"
     ArtBas.RAvdNr AT ROW 15.95 COL 106 COLON-ALIGNED HELP
          "Kode som beskriver artikkelens vareområde (F10 Oppslag)"
          LABEL "Vareområde"
          VIEW-AS FILL-IN 
          SIZE 5.8 BY 1 TOOLTIP "Kode som beskriver artikkelens vareområde og hvem som er produktansvarlig"
     ArtBas.Link_til_Nettside AT ROW 17.95 COL 106 COLON-ALIGNED HELP
          "Link til nettside med artikkelinformasjon"
          LABEL "Link til infoside"
          VIEW-AS FILL-IN 
          SIZE 43 BY 1 TOOLTIP "Link til leverandørs infoside for artikkelen."
     BUTTON-SokMaterial AT ROW 5.43 COL 25.8
     FILL-IN-InnPris AT ROW 20.62 COL 106 COLON-ALIGNED
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 9.86
         SIZE 208.8 BY 23.1.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME FRAME-ArtInfo
     FILL-IN-SalgsPris AT ROW 20.62 COL 131.4 COLON-ALIGNED
     FILL-IN-TilbPris AT ROW 21.62 COL 106 COLON-ALIGNED
     FILL-IN-UtsFra AT ROW 21.62 COL 123.2 COLON-ALIGNED NO-LABEL
     FILL-IN-UtsTil AT ROW 21.62 COL 136 COLON-ALIGNED NO-LABEL
     BUTTON-SokOver AT ROW 8.48 COL 25.8
     ArtBas.VareType AT ROW 2.24 COL 176.8 COLON-ALIGNED HELP
          "Leveringsmåte. 1-Butikkvare,2-Hentevare,3-Skaffevare"
          LABEL "Leveringsmåte"
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEM-PAIRS "0",1
          DROP-DOWN-LIST
          SIZE 23 BY 1 TOOLTIP "Leveringsmåte. 1-Butikkvare,2-Hentevare,3-Skaffevare"
     ArtBas.Leveringstid AT ROW 3.24 COL 176.8 COLON-ALIGNED HELP
          "Antall dager det tar for å få leveert vare til butikk fra lev."
          LABEL "Lev.tid til butikk"
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1 TOOLTIP "Antall dager det tar for å få levert vare til butikk fra leverandør"
     ArtBas.JamforEnhet AT ROW 5.33 COL 177 COLON-ALIGNED HELP
          "Enhet som jamfør pris angis i (stk, kg, hg, m, m2, l)."
          LABEL "Jamførenhet"
          VIEW-AS COMBO-BOX INNER-LINES 25
          LIST-ITEM-PAIRS "<Ikke angitt>","",
                     "Stykk (Stk)","stk",
                     "Kilo (Kg)","kg",
                     "Hekto (Hg)","hg",
                     "Meter (m)","m",
                     "Kvadratmeter (m2)","m2",
                     "Liter (l)","l"
          DROP-DOWN-LIST
          SIZE 23 BY 1 TOOLTIP "Enhet som jamfør pris angis i (stk, kg, hg, m, m2, l)"
     ArtBas.Mengde AT ROW 6.33 COL 177 COLON-ALIGNED
          LABEL "Mengde i salgsenhet"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     BUTTON-SokSesong AT ROW 3.43 COL 25.8
     FI-JamforPris AT ROW 7.33 COL 177 COLON-ALIGNED
     ArtBas.Kjokkenskriver AT ROW 12.19 COL 171.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
     ArtBas.AntIPakn AT ROW 14.43 COL 177 COLON-ALIGNED HELP
          "Antall salgsenheter i forpakning fra leverandør"
          LABEL "Antall i pakning"
          VIEW-AS FILL-IN NATIVE 
          SIZE 10.4 BY 1 TOOLTIP "Antall salgsenheter i forpakning fra leverandør"
     ArtBas.Anbrekk AT ROW 14.43 COL 189.8 HELP
          "Åpner for at det kan bestilles også løse enheter"
          VIEW-AS TOGGLE-BOX
          SIZE 11.8 BY .81 TOOLTIP "Åpner for at det kan bestilles også løse enheter"
     ArtBas.InkrAnbrekk AT ROW 15.43 COL 177 COLON-ALIGNED HELP
          "Minste antall som kan vestilles"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1 TOOLTIP "Minste antall som kan vestilles"
     BUTTON-SokSlit AT ROW 9.48 COL 25.8
     ArtBas.WebButikkArtikkel AT ROW 17.43 COL 158 HELP
          "Aktiverer artikkelen i nettbutikk"
          LABEL "Aktiv i nettbutikk"
          VIEW-AS TOGGLE-BOX
          SIZE 20 BY .81 TOOLTIP "Aktiverer artikkelen i nettbutikk"
     ArtBas.WebLeveringstid AT ROW 18.33 COL 169 COLON-ALIGNED HELP
          "Antall dagers leveringstid til kunde i nettbutikk"
          LABEL "Leveringstid"
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1 TOOLTIP "Antall dagers leveringstid til kunde i nettbutikk"
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 9.86
         SIZE 208.8 BY 23.1.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME FRAME-ArtInfo
     ArtBas.WebMinLager AT ROW 18.38 COL 187.4 COLON-ALIGNED HELP
          "Minimum antall på lager pr. størrelse. i nettbutikk."
          LABEL "Min.lager"
          VIEW-AS FILL-IN 
          SIZE 13.8 BY 1 TOOLTIP "Minimum antall på lager pr. størrelse. i nettbutikk."
     ArtBas.KampanjeKode AT ROW 19.48 COL 182.2 COLON-ALIGNED HELP
          "Kampanjekode i nettbutikk"
          LABEL "Kampanjekode"
          VIEW-AS FILL-IN 
          SIZE 19 BY 1 TOOLTIP "Kampanjekode i nettbutikk"
     ArtBas.Depositum AT ROW 21.67 COL 176.8 COLON-ALIGNED HELP
          "Depositum som må betales ved salg av vare på kredit"
          LABEL "Depositum"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1 TOOLTIP "Depositum som må betales ved salg av vare på kredit"
     HovedKategori.HovedKatTekst AT ROW 14.48 COL 28 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 48.2 BY 1
     BUTTON-SokFarge AT ROW 4.43 COL 25.8
     BUTTON-SokBehKode AT ROW 14.95 COL 113.6
     BUTTON-SokRegnAvd AT ROW 15.95 COL 113.6
     T-EnableLevInfo AT ROW 2.57 COL 56 NO-TAB-STOP 
     T-1 AT ROW 3.52 COL 79.2 NO-TAB-STOP 
     T-10 AT ROW 4.52 COL 79.2 NO-TAB-STOP 
     T-2 AT ROW 5.52 COL 79.2 NO-TAB-STOP 
     T-3 AT ROW 6.52 COL 79.2 NO-TAB-STOP 
     T-4 AT ROW 7.52 COL 79.2 NO-TAB-STOP 
     T-5 AT ROW 8.52 COL 79.2 NO-TAB-STOP 
     T-6 AT ROW 9.52 COL 79.2 NO-TAB-STOP 
     T-7 AT ROW 10.52 COL 79.2 NO-TAB-STOP 
     T-8 AT ROW 11.52 COL 79.2 NO-TAB-STOP 
     T-9 AT ROW 19.67 COL 79.4 NO-TAB-STOP 
     S-AlternativLev AT ROW 8.43 COL 108 HELP
          "Andre leverandører som artikkelen kan kjøpes inn fra (F10)" NO-LABEL NO-TAB-STOP 
     BUTTON-SokStrType AT ROW 2.38 COL 25.8
     BUTTON-SokVaremerke AT ROW 12.48 COL 25.8
     BUTTON-SokProdusent AT ROW 13.48 COL 25.8
     BUTTON-SokValuta AT ROW 17.95 COL 25.8
     FI-Tekst1 AT ROW 1.52 COL 2.4 NO-LABEL
     FI-Tekst-3 AT ROW 19.19 COL 2.8 NO-LABEL
     FILL-IN-EndretInfo AT ROW 23.14 COL 4.6 NO-LABEL
     FI-Tekst-2 AT ROW 1.52 COL 53 COLON-ALIGNED NO-LABEL
     FILL-IN-19 AT ROW 2.67 COL 68.2 COLON-ALIGNED NO-LABEL
     FI-Tekst-4 AT ROW 1.52 COL 84 NO-LABEL
     Txt-AlternativLev AT ROW 8.38 COL 82.6 COLON-ALIGNED NO-LABEL
     FI-Tekst-5 AT ROW 19.52 COL 84 NO-LABEL
     FI-Tekst-9 AT ROW 1.48 COL 158 NO-LABEL
     FI-Tekst-7 AT ROW 13.67 COL 158 NO-LABEL
     FI-Tekst-6 AT ROW 16.86 COL 158.4 NO-LABEL
     ArtBas.inn_dato AT ROW 23.14 COL 178.6 COLON-ALIGNED
          LABEL "Første innleveranse"
           VIEW-AS TEXT 
          SIZE 20.8 BY .62 NO-TAB-STOP 
     FI-Tekst-10 AT ROW 4.62 COL 158 NO-LABEL
     FI-Tekst-12 AT ROW 20.95 COL 158.4 NO-LABEL
     FI-Tekst-11 AT ROW 8.67 COL 158 NO-LABEL
     S-Underkategori AT ROW 15.52 COL 16.2 HELP
          "Andre leverandører som artikkelen kan kjøpes inn fra (F10)" NO-LABEL NO-TAB-STOP 
     BUTTON-Underkategori AT ROW 15.57 COL 78.2
     Txt-Underkat AT ROW 15.57 COL 3.4 NO-LABEL
     BUTTON-SokHovedKategori AT ROW 14.48 COL 25.6
     FI-Tekst-13 AT ROW 11.43 COL 158 NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 9.86
         SIZE 208.8 BY 23.1.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME FRAME-ArtInfo
     FI-Tekst-14 AT ROW 11.43 COL 180.8 NO-LABEL
     RECT-31 AT ROW 1.24 COL 1.4
     RECT-54 AT ROW 19.14 COL 1.4
     RECT-61 AT ROW 22.91 COL 1.4
     RECT-29 AT ROW 19.14 COL 83
     RECT-62 AT ROW 1.24 COL 157
     RECT-63 AT ROW 16.62 COL 157
     RECT-64 AT ROW 13.57 COL 157
     RECT-66 AT ROW 1.24 COL 83
     RECT-67 AT ROW 4.43 COL 157
     RECT-68 AT ROW 20.76 COL 157
     RECT-70 AT ROW 8.62 COL 157
     RECT-65 AT ROW 11.33 COL 157 WIDGET-ID 4
     RECT-71 AT ROW 11.33 COL 179.8 WIDGET-ID 10
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 9.86
         SIZE 208.8 BY 23.1.


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
         TITLE              = "Artikkelkort"
         HEIGHT             = 32.91
         WIDTH              = 209.2
         MAX-HEIGHT         = 32.91
         MAX-WIDTH          = 209.2
         VIRTUAL-HEIGHT     = 32.91
         VIRTUAL-WIDTH      = 209.2
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
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-ArtKort
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME FRAME-ArtInfo:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME FRAME-Info2:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME FRAME-Lager:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
   NOT-VISIBLE FRAME-NAME Custom                                        */
/* SETTINGS FOR FILL-IN ArtBas.ArtikkelNr IN FRAME DEFAULT-FRAME
   NO-ENABLE EXP-LABEL EXP-HELP                                         */
/* SETTINGS FOR COMBO-BOX ArtBas.ArtSlag IN FRAME DEFAULT-FRAME
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR BUTTON B-Jamfor IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ArtBas.Beskr IN FRAME DEFAULT-FRAME
   EXP-FORMAT EXP-HELP                                                  */
/* SETTINGS FOR TOGGLE-BOX ArtBas.BestForslag IN FRAME DEFAULT-FRAME
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR TOGGLE-BOX ArtBas.BildeIKasse IN FRAME DEFAULT-FRAME
   EXP-HELP                                                             */
ASSIGN 
       BUTTON-Kopierbilde:POPUP-MENU IN FRAME DEFAULT-FRAME       = MENU POPUP-MENU-BUTTON-Kopierbilde:HANDLE.

/* SETTINGS FOR BUTTON BUTTON-SettLopNr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BUTTON-SettLopNr:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON BUTTON-SokVg IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BUTTON-TilKasse:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN FI-ArtBut IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       FI-LevFargKod:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN FI-PaTilbud IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-LevNamn IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-VgBeskr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX ArtBas.Gjennomfaktureres IN FRAME DEFAULT-FRAME
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR TOGGLE-BOX ArtBas.Grunnsortiment IN FRAME DEFAULT-FRAME
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN HuvGr.Hg IN FRAME DEFAULT-FRAME
   NO-ENABLE EXP-HELP                                                   */
/* SETTINGS FOR FILL-IN HuvGr.HgBeskr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX ArtBas.HkStyrt IN FRAME DEFAULT-FRAME
   NO-ENABLE EXP-HELP                                                   */
/* SETTINGS FOR TOGGLE-BOX ArtBas.HoyLavMva IN FRAME DEFAULT-FRAME
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR TOGGLE-BOX ArtBas.IKasse IN FRAME DEFAULT-FRAME
   EXP-HELP                                                             */
/* SETTINGS FOR COMBO-BOX ArtBas.IndividType IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L EXP-LABEL                                          */
/* SETTINGS FOR TOGGLE-BOX ArtBas.KjedeVare IN FRAME DEFAULT-FRAME
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR TOGGLE-BOX ArtBas.KjentPaHK IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       ArtBas.KjentPaHK:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR TOGGLE-BOX ArtBas.lager IN FRAME DEFAULT-FRAME
   NO-ENABLE EXP-LABEL EXP-HELP                                         */
/* SETTINGS FOR FILL-IN ArtBas.LevKod IN FRAME DEFAULT-FRAME
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN ArtBas.LevNr IN FRAME DEFAULT-FRAME
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR TOGGLE-BOX ArtBas.LokPris IN FRAME DEFAULT-FRAME
   NO-ENABLE EXP-LABEL                                                  */
ASSIGN 
       ArtBas.LokPris:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN ArtBas.LopNr IN FRAME DEFAULT-FRAME
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR TOGGLE-BOX ArtBas.ManueltOpprettet IN FRAME DEFAULT-FRAME
   NO-ENABLE EXP-LABEL EXP-HELP                                         */
/* SETTINGS FOR TOGGLE-BOX ArtBas.NegVare IN FRAME DEFAULT-FRAME
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX ArtBas.NON_Sale IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX ArtBas.OPris IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX ArtBas.Pakke IN FRAME DEFAULT-FRAME
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR TOGGLE-BOX ArtBas.Pant IN FRAME DEFAULT-FRAME
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN ArtBas.SanertDato IN FRAME DEFAULT-FRAME
   EXP-HELP                                                             */
ASSIGN 
       ArtBas.SanertDato:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR TOGGLE-BOX ArtBas.Telefonkort IN FRAME DEFAULT-FRAME
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX ArtBas.Utgatt IN FRAME DEFAULT-FRAME
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN ArtBas.UtgattDato IN FRAME DEFAULT-FRAME
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN ArtBas.Vg IN FRAME DEFAULT-FRAME
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN ArtBas.VgKat IN FRAME DEFAULT-FRAME
   EXP-FORMAT EXP-HELP                                                  */
/* SETTINGS FOR FRAME FRAME-ArtInfo
   NOT-VISIBLE Custom                                                   */
/* SETTINGS FOR COMBO-BOX ArtBas.Alder IN FRAME FRAME-ArtInfo
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR TOGGLE-BOX ArtBas.Anbrekk IN FRAME FRAME-ArtInfo
   EXP-HELP                                                             */
/* SETTINGS FOR FILL-IN ArtBas.AntIPakn IN FRAME FRAME-ArtInfo
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR FILL-IN ArtBas.anv-id IN FRAME FRAME-ArtInfo
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR FILL-IN Anv-Kod.AnvBeskr IN FRAME FRAME-ArtInfo
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       Anv-Kod.AnvBeskr:HIDDEN IN FRAME FRAME-ArtInfo           = TRUE.

/* SETTINGS FOR FILL-IN ArtBas.BehKode IN FRAME FRAME-ArtInfo
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR FILL-IN Handtering.Beskrivelse IN FRAME FRAME-ArtInfo
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       Handtering.Beskrivelse:HIDDEN IN FRAME FRAME-ArtInfo           = TRUE.

/* SETTINGS FOR FILL-IN Produsent.Beskrivelse IN FRAME FRAME-ArtInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Varemerke.Beskrivelse IN FRAME FRAME-ArtInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN StrType.Beskrivelse IN FRAME FRAME-ArtInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN klack.beskrivning IN FRAME FRAME-ArtInfo
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       klack.beskrivning:HIDDEN IN FRAME FRAME-ArtInfo           = TRUE.

/* SETTINGS FOR FILL-IN ArtBas.BongTekst IN FRAME FRAME-ArtInfo
   EXP-FORMAT EXP-HELP                                                  */
/* SETTINGS FOR TOGGLE-BOX ArtBas.Bonus_Givende IN FRAME FRAME-ArtInfo
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN ArtBas.Depositum IN FRAME FRAME-ArtInfo
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR COMBO-BOX ArtBas.Etikett IN FRAME FRAME-ArtInfo
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR FILL-IN ArtBas.Etikettekst1 IN FRAME FRAME-ArtInfo
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR FILL-IN ArtBas.Etikettekst2 IN FRAME FRAME-ArtInfo
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR FILL-IN Farg.FarBeskr IN FRAME FRAME-ArtInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ArtBas.Farg IN FRAME FRAME-ArtInfo
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN FI-1 IN FRAME FRAME-ArtInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-2 IN FRAME FRAME-ArtInfo
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FI-2:HIDDEN IN FRAME FRAME-ArtInfo           = TRUE.

/* SETTINGS FOR FILL-IN FI-3 IN FRAME FRAME-ArtInfo
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FI-3:HIDDEN IN FRAME FRAME-ArtInfo           = TRUE.

/* SETTINGS FOR FILL-IN FI-4 IN FRAME FRAME-ArtInfo
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FI-4:HIDDEN IN FRAME FRAME-ArtInfo           = TRUE.

/* SETTINGS FOR FILL-IN FI-5 IN FRAME FRAME-ArtInfo
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FI-5:HIDDEN IN FRAME FRAME-ArtInfo           = TRUE.

/* SETTINGS FOR FILL-IN FI-6 IN FRAME FRAME-ArtInfo
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FI-6:HIDDEN IN FRAME FRAME-ArtInfo           = TRUE.

/* SETTINGS FOR FILL-IN FI-7 IN FRAME FRAME-ArtInfo
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FI-7:HIDDEN IN FRAME FRAME-ArtInfo           = TRUE.

/* SETTINGS FOR FILL-IN FI-8 IN FRAME FRAME-ArtInfo
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FI-8:HIDDEN IN FRAME FRAME-ArtInfo           = TRUE.

/* SETTINGS FOR FILL-IN FI-JamforPris IN FRAME FRAME-ArtInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-LinkVareTekst IN FRAME FRAME-ArtInfo
   NO-ENABLE                                                            */
ASSIGN 
       FI-LinkVareTekst:READ-ONLY IN FRAME FRAME-ArtInfo        = TRUE.

/* SETTINGS FOR FILL-IN FI-Tekst-10 IN FRAME FRAME-ArtInfo
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FI-Tekst-11 IN FRAME FRAME-ArtInfo
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FI-Tekst-12 IN FRAME FRAME-ArtInfo
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FI-Tekst-13 IN FRAME FRAME-ArtInfo
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FI-Tekst-14 IN FRAME FRAME-ArtInfo
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FI-Tekst-2 IN FRAME FRAME-ArtInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Tekst-3 IN FRAME FRAME-ArtInfo
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FI-Tekst-4 IN FRAME FRAME-ArtInfo
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FI-Tekst-5 IN FRAME FRAME-ArtInfo
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FI-Tekst-6 IN FRAME FRAME-ArtInfo
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FI-Tekst-7 IN FRAME FRAME-ArtInfo
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FI-Tekst-9 IN FRAME FRAME-ArtInfo
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FI-Tekst1 IN FRAME FRAME-ArtInfo
   NO-ENABLE ALIGN-L                                                    */
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
/* SETTINGS FOR COMBO-BOX ArtBas.GarantiKl IN FRAME FRAME-ArtInfo
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR FILL-IN ArtBas.HovedKatNr IN FRAME FRAME-ArtInfo
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR FILL-IN HovedKategori.HovedKatTekst IN FRAME FRAME-ArtInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ArtBas.InkrAnbrekk IN FRAME FRAME-ArtInfo
   EXP-HELP                                                             */
/* SETTINGS FOR FILL-IN ArtBas.inner-id IN FRAME FRAME-ArtInfo
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR FILL-IN InnerSula.InnerBeskr IN FRAME FRAME-ArtInfo
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       InnerSula.InnerBeskr:HIDDEN IN FRAME FRAME-ArtInfo           = TRUE.

/* SETTINGS FOR FILL-IN ArtBas.inn_dato IN FRAME FRAME-ArtInfo
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR COMBO-BOX ArtBas.JamforEnhet IN FRAME FRAME-ArtInfo
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR FILL-IN ArtBas.KampanjeKode IN FRAME FRAME-ArtInfo
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR FILL-IN ArtBas.Klack IN FRAME FRAME-ArtInfo
   EXP-HELP                                                             */
/* SETTINGS FOR TOGGLE-BOX ArtBas.KundeRabatt IN FRAME FRAME-ArtInfo
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN ArtBas.last-id IN FRAME FRAME-ArtInfo
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR FILL-IN Last-Sko.LastBeskr IN FRAME FRAME-ArtInfo
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       Last-Sko.LastBeskr:HIDDEN IN FRAME FRAME-ArtInfo           = TRUE.

/* SETTINGS FOR FILL-IN ArtBas.Leveringstid IN FRAME FRAME-ArtInfo
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR FILL-IN ArtBas.LevFargKod IN FRAME FRAME-ArtInfo
   EXP-FORMAT EXP-HELP                                                  */
/* SETTINGS FOR FILL-IN ArtBas.LinkVareAnt IN FRAME FRAME-ArtInfo
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN ArtBas.LinkVareNr IN FRAME FRAME-ArtInfo
   NO-ENABLE EXP-LABEL EXP-HELP                                         */
/* SETTINGS FOR FILL-IN ArtBas.Link_til_Nettside IN FRAME FRAME-ArtInfo
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR FILL-IN ArtBas.Lokasjon IN FRAME FRAME-ArtInfo
   EXP-HELP                                                             */
/* SETTINGS FOR TOGGLE-BOX ArtBas.ManRabIKas IN FRAME FRAME-ArtInfo
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Material.MatBeskr IN FRAME FRAME-ArtInfo
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       Material.MatBeskr:HIDDEN IN FRAME FRAME-ArtInfo           = TRUE.

/* SETTINGS FOR FILL-IN ArtBas.MatKod IN FRAME FRAME-ArtInfo
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR FILL-IN ArtBas.Mengde IN FRAME FRAME-ArtInfo
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX ArtBas.MengdeRabatt IN FRAME FRAME-ArtInfo
   EXP-LABEL                                                            */
/* SETTINGS FOR EDITOR ArtBas.Notat IN FRAME FRAME-ArtInfo
   EXP-HELP                                                             */
ASSIGN 
       ArtBas.Notat:RETURN-INSERTED IN FRAME FRAME-ArtInfo  = TRUE.

/* SETTINGS FOR COMBO-BOX ArtBas.OnLineLevNr IN FRAME FRAME-ArtInfo
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR FILL-IN ArtBas.ov-id IN FRAME FRAME-ArtInfo
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR FILL-IN Ovandel.OvBeskr IN FRAME FRAME-ArtInfo
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       Ovandel.OvBeskr:HIDDEN IN FRAME FRAME-ArtInfo           = TRUE.

/* SETTINGS FOR FILL-IN ArtBas.ProdNr IN FRAME FRAME-ArtInfo
   EXP-HELP                                                             */
/* SETTINGS FOR TOGGLE-BOX ArtBas.PubliserINettbutikk IN FRAME FRAME-ArtInfo
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Regnskapsavdeling.RAvdBeskrivelse IN FRAME FRAME-ArtInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ArtBas.RAvdNr IN FRAME FRAME-ArtInfo
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR COMBO-BOX ArtBas.SalgsEnhet IN FRAME FRAME-ArtInfo
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR COMBO-BOX ArtBas.SalgsStopp IN FRAME FRAME-ArtInfo
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN SaSong.SasBeskr IN FRAME FRAME-ArtInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ArtBas.SaSong IN FRAME FRAME-ArtInfo
   EXP-FORMAT EXP-HELP                                                  */
/* SETTINGS FOR FILL-IN ArtBas.slit-id IN FRAME FRAME-ArtInfo
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR FILL-IN SlitSula.SlitBeskr IN FRAME FRAME-ArtInfo
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       SlitSula.SlitBeskr:HIDDEN IN FRAME FRAME-ArtInfo           = TRUE.

/* SETTINGS FOR FILL-IN ArtBas.StrTypeID IN FRAME FRAME-ArtInfo
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR TOGGLE-BOX T-1 IN FRAME FRAME-ArtInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX T-10 IN FRAME FRAME-ArtInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX T-2 IN FRAME FRAME-ArtInfo
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       T-2:HIDDEN IN FRAME FRAME-ArtInfo           = TRUE.

/* SETTINGS FOR TOGGLE-BOX T-3 IN FRAME FRAME-ArtInfo
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       T-3:HIDDEN IN FRAME FRAME-ArtInfo           = TRUE.

/* SETTINGS FOR TOGGLE-BOX T-4 IN FRAME FRAME-ArtInfo
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       T-4:HIDDEN IN FRAME FRAME-ArtInfo           = TRUE.

/* SETTINGS FOR TOGGLE-BOX T-5 IN FRAME FRAME-ArtInfo
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       T-5:HIDDEN IN FRAME FRAME-ArtInfo           = TRUE.

/* SETTINGS FOR TOGGLE-BOX T-6 IN FRAME FRAME-ArtInfo
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       T-6:HIDDEN IN FRAME FRAME-ArtInfo           = TRUE.

/* SETTINGS FOR TOGGLE-BOX T-7 IN FRAME FRAME-ArtInfo
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       T-7:HIDDEN IN FRAME FRAME-ArtInfo           = TRUE.

/* SETTINGS FOR TOGGLE-BOX T-8 IN FRAME FRAME-ArtInfo
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       T-8:HIDDEN IN FRAME FRAME-ArtInfo           = TRUE.

/* SETTINGS FOR TOGGLE-BOX T-9 IN FRAME FRAME-ArtInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Txt-AlternativLev IN FRAME FRAME-ArtInfo
   NO-ENABLE                                                            */
ASSIGN 
       Txt-AlternativLev:READ-ONLY IN FRAME FRAME-ArtInfo        = TRUE.

/* SETTINGS FOR FILL-IN Txt-Underkat IN FRAME FRAME-ArtInfo
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       Txt-Underkat:READ-ONLY IN FRAME FRAME-ArtInfo        = TRUE.

/* SETTINGS FOR FILL-IN ArtBas.valkod IN FRAME FRAME-ArtInfo
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR FILL-IN Valuta.ValKurs IN FRAME FRAME-ArtInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX ArtBas.VareType IN FRAME FRAME-ArtInfo
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR FILL-IN ArtBas.VMId IN FRAME FRAME-ArtInfo
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR TOGGLE-BOX ArtBas.WebButikkArtikkel IN FRAME FRAME-ArtInfo
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR FILL-IN ArtBas.WebLeveringstid IN FRAME FRAME-ArtInfo
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR FILL-IN ArtBas.WebMinLager IN FRAME FRAME-ArtInfo
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR FRAME FRAME-Info2
   NOT-VISIBLE L-To-R,COLUMNS                                           */
/* SETTINGS FOR FILL-IN ArtBas.AnbefaltPris IN FRAME FRAME-Info2
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR FILL-IN FI-Land IN FRAME FRAME-Info2
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-VpiInfoTxt IN FRAME FRAME-Info2
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FI-VpiInfoTxt-2 IN FRAME FRAME-Info2
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FI-VpiInfoTxt-3 IN FRAME FRAME-Info2
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN ArtBas.forhRab% IN FRAME FRAME-Info2
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR FILL-IN ArtBas.Kampanjeuker IN FRAME FRAME-Info2
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN ArtBas.KatalogPris IN FRAME FRAME-Info2
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN ArtBas.KjedeInnkPris IN FRAME FRAME-Info2
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN ArtBas.KjedeProdusent IN FRAME FRAME-Info2
   EXP-HELP                                                             */
/* SETTINGS FOR FILL-IN ArtBas.KjedeRab% IN FRAME FRAME-Info2
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR FILL-IN ArtBas.KjedeValutaPris IN FRAME FRAME-Info2
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR FILL-IN ArtBas.Lagerkoder IN FRAME FRAME-Info2
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN ArtBas.LevDato1 IN FRAME FRAME-Info2
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN ArtBas.LevDato2 IN FRAME FRAME-Info2
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN ArtBas.LinjeMerknad IN FRAME FRAME-Info2
   EXP-HELP                                                             */
/* SETTINGS FOR FILL-IN ArtBas.PostBredde IN FRAME FRAME-Info2
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN ArtBas.PostHoyde IN FRAME FRAME-Info2
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN ArtBas.PostLengde IN FRAME FRAME-Info2
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN ArtBas.PostVekt IN FRAME FRAME-Info2
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN ArtBas.Sortimentkoder IN FRAME FRAME-Info2
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN ArtBas.supRab% IN FRAME FRAME-Info2
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR FILL-IN ArtBas.TilgjengeligFraLev IN FRAME FRAME-Info2
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR FILL-IN Txt-Linjemerknad IN FRAME FRAME-Info2
   NO-ENABLE                                                            */
ASSIGN 
       Txt-Linjemerknad:READ-ONLY IN FRAME FRAME-Info2        = TRUE.

/* SETTINGS FOR FILL-IN Txt-OpprLand IN FRAME FRAME-Info2
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN Txt-Varefakta IN FRAME FRAME-Info2
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR EDITOR ArtBas.VareFakta IN FRAME FRAME-Info2
   EXP-HELP                                                             */
ASSIGN 
       ArtBas.VareFakta:RETURN-INSERTED IN FRAME FRAME-Info2  = TRUE.

/* SETTINGS FOR FILL-IN ArtBas.VPIBildeKode IN FRAME FRAME-Info2
   EXP-FORMAT EXP-HELP                                                  */
/* SETTINGS FOR FILL-IN ArtBas.VPIDato IN FRAME FRAME-Info2
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR FRAME FRAME-Lager
                                                                        */
/* BROWSE-TAB BROWSE-Lager TG-VisSkjul FRAME-Lager */
ASSIGN 
       BROWSE-Lager:NUM-LOCKED-COLUMNS IN FRAME FRAME-Lager     = 5.

ASSIGN 
       tmpLager.Antall[1]:AUTO-RESIZE IN BROWSE BROWSE-Lager = TRUE
       tmpLager.Antall[2]:AUTO-RESIZE IN BROWSE BROWSE-Lager = TRUE
       tmpLager.Antall[3]:AUTO-RESIZE IN BROWSE BROWSE-Lager = TRUE
       tmpLager.Antall[4]:AUTO-RESIZE IN BROWSE BROWSE-Lager = TRUE
       tmpLager.Antall[5]:AUTO-RESIZE IN BROWSE BROWSE-Lager = TRUE
       tmpLager.Antall[6]:AUTO-RESIZE IN BROWSE BROWSE-Lager = TRUE
       tmpLager.Antall[7]:AUTO-RESIZE IN BROWSE BROWSE-Lager = TRUE
       tmpLager.Antall[8]:AUTO-RESIZE IN BROWSE BROWSE-Lager = TRUE
       tmpLager.Antall[9]:AUTO-RESIZE IN BROWSE BROWSE-Lager = TRUE
       tmpLager.Antall[10]:AUTO-RESIZE IN BROWSE BROWSE-Lager = TRUE
       tmpLager.Antall[11]:AUTO-RESIZE IN BROWSE BROWSE-Lager = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-ArtKort)
THEN C-ArtKort:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-Lager
/* Query rebuild information for BROWSE BROWSE-Lager
     _TblList          = "Temp-Tables.tmpLager"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.tmpLager.Butik
"tmpLager.Butik" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.tmpLager.VVareKost
"tmpLager.VVareKost" ? "->>>>>>9" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.tmpLager.SumAntall
"tmpLager.SumAntall" ? "X(14)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.tmpLager.SumVerdi
"tmpLager.SumVerdi" ? "X(14)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.tmpLager.DivAntall
"tmpLager.DivAntall" ? "X(14)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.tmpLager.Antall[1]
"tmpLager.Antall[1]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes yes no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Temp-Tables.tmpLager.Antall[2]
"tmpLager.Antall[2]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes yes no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > Temp-Tables.tmpLager.Antall[3]
"tmpLager.Antall[3]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes yes no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > Temp-Tables.tmpLager.Antall[4]
"tmpLager.Antall[4]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes yes no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > Temp-Tables.tmpLager.Antall[5]
"tmpLager.Antall[5]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes yes no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > Temp-Tables.tmpLager.Antall[6]
"tmpLager.Antall[6]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes yes no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > Temp-Tables.tmpLager.Antall[7]
"tmpLager.Antall[7]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes yes no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > Temp-Tables.tmpLager.Antall[8]
"tmpLager.Antall[8]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes yes no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > Temp-Tables.tmpLager.Antall[9]
"tmpLager.Antall[9]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes yes no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > Temp-Tables.tmpLager.Antall[10]
"tmpLager.Antall[10]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes yes no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > Temp-Tables.tmpLager.Antall[11]
"tmpLager.Antall[11]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes yes no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > Temp-Tables.tmpLager.Antall[12]
"tmpLager.Antall[12]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > Temp-Tables.tmpLager.Antall[13]
"tmpLager.Antall[13]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   > Temp-Tables.tmpLager.Antall[14]
"tmpLager.Antall[14]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[20]   > Temp-Tables.tmpLager.Antall[15]
"tmpLager.Antall[15]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[21]   > Temp-Tables.tmpLager.Antall[16]
"tmpLager.Antall[16]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[22]   > Temp-Tables.tmpLager.Antall[17]
"tmpLager.Antall[17]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[23]   > Temp-Tables.tmpLager.Antall[18]
"tmpLager.Antall[18]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[24]   > Temp-Tables.tmpLager.Antall[19]
"tmpLager.Antall[19]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[25]   > Temp-Tables.tmpLager.Antall[20]
"tmpLager.Antall[20]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[26]   > Temp-Tables.tmpLager.Antall[21]
"tmpLager.Antall[21]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[27]   > Temp-Tables.tmpLager.Antall[22]
"tmpLager.Antall[22]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[28]   > Temp-Tables.tmpLager.Antall[23]
"tmpLager.Antall[23]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[29]   > Temp-Tables.tmpLager.Antall[24]
"tmpLager.Antall[24]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[30]   > Temp-Tables.tmpLager.Antall[25]
"tmpLager.Antall[25]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[31]   > Temp-Tables.tmpLager.Antall[26]
"tmpLager.Antall[26]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[32]   > Temp-Tables.tmpLager.Antall[27]
"tmpLager.Antall[27]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[33]   > Temp-Tables.tmpLager.Antall[28]
"tmpLager.Antall[28]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[34]   > Temp-Tables.tmpLager.Antall[29]
"tmpLager.Antall[29]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[35]   > Temp-Tables.tmpLager.Antall[30]
"tmpLager.Antall[30]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[36]   > Temp-Tables.tmpLager.Antall[31]
"tmpLager.Antall[31]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[37]   > Temp-Tables.tmpLager.Antall[32]
"tmpLager.Antall[32]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[38]   > Temp-Tables.tmpLager.Antall[33]
"tmpLager.Antall[33]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[39]   > Temp-Tables.tmpLager.Antall[34]
"tmpLager.Antall[34]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[40]   > Temp-Tables.tmpLager.Antall[35]
"tmpLager.Antall[35]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[41]   > Temp-Tables.tmpLager.Antall[36]
"tmpLager.Antall[36]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[42]   > Temp-Tables.tmpLager.Antall[37]
"tmpLager.Antall[37]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[43]   > Temp-Tables.tmpLager.Antall[38]
"tmpLager.Antall[38]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[44]   > Temp-Tables.tmpLager.Antall[39]
"tmpLager.Antall[39]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[45]   > Temp-Tables.tmpLager.Antall[40]
"tmpLager.Antall[40]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[46]   > Temp-Tables.tmpLager.Antall[41]
"tmpLager.Antall[41]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[47]   > Temp-Tables.tmpLager.Antall[42]
"tmpLager.Antall[42]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[48]   > Temp-Tables.tmpLager.Antall[43]
"tmpLager.Antall[43]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[49]   > Temp-Tables.tmpLager.Antall[44]
"tmpLager.Antall[44]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[50]   > Temp-Tables.tmpLager.Antall[45]
"tmpLager.Antall[45]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[51]   > Temp-Tables.tmpLager.Antall[46]
"tmpLager.Antall[46]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[52]   > Temp-Tables.tmpLager.Antall[47]
"tmpLager.Antall[47]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[53]   > Temp-Tables.tmpLager.Antall[48]
"tmpLager.Antall[48]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[54]   > Temp-Tables.tmpLager.Antall[49]
"tmpLager.Antall[49]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[55]   > Temp-Tables.tmpLager.Antall[50]
"tmpLager.Antall[50]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[56]   > Temp-Tables.tmpLager.Antall[51]
"tmpLager.Antall[51]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[57]   > Temp-Tables.tmpLager.Antall[52]
"tmpLager.Antall[52]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[58]   > Temp-Tables.tmpLager.Antall[53]
"tmpLager.Antall[53]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[59]   > Temp-Tables.tmpLager.Antall[54]
"tmpLager.Antall[54]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[60]   > Temp-Tables.tmpLager.Antall[55]
"tmpLager.Antall[55]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[61]   > Temp-Tables.tmpLager.Antall[56]
"tmpLager.Antall[56]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[62]   > Temp-Tables.tmpLager.Antall[57]
"tmpLager.Antall[57]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[63]   > Temp-Tables.tmpLager.Antall[58]
"tmpLager.Antall[58]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[64]   > Temp-Tables.tmpLager.Antall[59]
"tmpLager.Antall[59]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[65]   > Temp-Tables.tmpLager.Antall[60]
"tmpLager.Antall[60]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[66]   > Temp-Tables.tmpLager.Antall[61]
"tmpLager.Antall[61]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[67]   > Temp-Tables.tmpLager.Antall[62]
"tmpLager.Antall[62]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[68]   > Temp-Tables.tmpLager.Antall[63]
"tmpLager.Antall[63]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[69]   > Temp-Tables.tmpLager.Antall[64]
"tmpLager.Antall[64]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[70]   > Temp-Tables.tmpLager.Antall[65]
"tmpLager.Antall[65]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[71]   > Temp-Tables.tmpLager.Antall[66]
"tmpLager.Antall[66]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[72]   > Temp-Tables.tmpLager.Antall[67]
"tmpLager.Antall[67]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[73]   > Temp-Tables.tmpLager.Antall[68]
"tmpLager.Antall[68]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[74]   > Temp-Tables.tmpLager.Antall[69]
"tmpLager.Antall[69]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[75]   > Temp-Tables.tmpLager.Antall[70]
"tmpLager.Antall[70]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[76]   > Temp-Tables.tmpLager.Antall[71]
"tmpLager.Antall[71]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[77]   > Temp-Tables.tmpLager.Antall[72]
"tmpLager.Antall[72]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[78]   > Temp-Tables.tmpLager.Antall[73]
"tmpLager.Antall[73]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[79]   > Temp-Tables.tmpLager.Antall[74]
"tmpLager.Antall[74]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[80]   > Temp-Tables.tmpLager.Antall[75]
"tmpLager.Antall[75]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[81]   > Temp-Tables.tmpLager.Antall[76]
"tmpLager.Antall[76]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[82]   > Temp-Tables.tmpLager.Antall[77]
"tmpLager.Antall[77]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[83]   > Temp-Tables.tmpLager.Antall[78]
"tmpLager.Antall[78]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[84]   > Temp-Tables.tmpLager.Antall[79]
"tmpLager.Antall[79]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[85]   > Temp-Tables.tmpLager.Antall[80]
"tmpLager.Antall[80]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[86]   > Temp-Tables.tmpLager.Antall[81]
"tmpLager.Antall[81]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[87]   > Temp-Tables.tmpLager.Antall[82]
"tmpLager.Antall[82]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[88]   > Temp-Tables.tmpLager.Antall[83]
"tmpLager.Antall[83]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[89]   > Temp-Tables.tmpLager.Antall[84]
"tmpLager.Antall[84]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[90]   > Temp-Tables.tmpLager.Antall[85]
"tmpLager.Antall[85]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[91]   > Temp-Tables.tmpLager.Antall[86]
"tmpLager.Antall[86]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[92]   > Temp-Tables.tmpLager.Antall[87]
"tmpLager.Antall[87]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[93]   > Temp-Tables.tmpLager.Antall[88]
"tmpLager.Antall[88]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[94]   > Temp-Tables.tmpLager.Antall[89]
"tmpLager.Antall[89]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[95]   > Temp-Tables.tmpLager.Antall[90]
"tmpLager.Antall[90]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[96]   > Temp-Tables.tmpLager.Antall[91]
"tmpLager.Antall[91]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[97]   > Temp-Tables.tmpLager.Antall[92]
"tmpLager.Antall[92]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[98]   > Temp-Tables.tmpLager.Antall[93]
"tmpLager.Antall[93]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[99]   > Temp-Tables.tmpLager.Antall[94]
"tmpLager.Antall[94]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[100]   > Temp-Tables.tmpLager.Antall[95]
"tmpLager.Antall[95]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[101]   > Temp-Tables.tmpLager.Antall[96]
"tmpLager.Antall[96]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[102]   > Temp-Tables.tmpLager.Antall[97]
"tmpLager.Antall[97]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[103]   > Temp-Tables.tmpLager.Antall[98]
"tmpLager.Antall[98]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[104]   > Temp-Tables.tmpLager.Antall[99]
"tmpLager.Antall[99]" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-Info2
/* Query rebuild information for FRAME FRAME-Info2
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME FRAME-Info2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-Lager
/* Query rebuild information for FRAME FRAME-Lager
     _Query            is NOT OPENED
*/  /* FRAME FRAME-Lager */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME IMAGE-Sko ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 2.91
       COLUMN          = 180
       HEIGHT          = 4.81
       WIDTH           = 27
       TAB-STOP        = no
       HIDDEN          = no
       SENSITIVE       = yes.

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 8.62
       COLUMN          = 1
       HEIGHT          = 24.52
       WIDTH           = 209
       HIDDEN          = no
       SENSITIVE       = yes.
/* IMAGE-Sko OCXINFO:CREATE-CONTROL from: {9A93B740-C96B-11D0-8883-444553540000} type: Picbuf */
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {1EFB6596-857C-11D1-B16A-00C0F0283628} type: TabStrip */
      CtrlFrame:MOVE-AFTER(ArtBas.LevKod:HANDLE IN FRAME DEFAULT-FRAME).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-ArtKort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-ArtKort C-ArtKort
ON END-ERROR OF C-ArtKort /* Artikkelkort */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-ArtKort C-ArtKort
ON WINDOW-CLOSE OF C-ArtKort /* Artikkelkort */
DO:
    IF VALID-HANDLE(hModell) THEN DO:
        RUN EndreKlar IN hModell.
        RETURN NO-APPLY.
    END.
  IF CAN-FIND(FIRST tmpChild WHERE
               VALID-HANDLE(tmpChild.wChild)) THEN
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
    
  /* Lagre oppsett av VPI leverandør */
  RUN SaveToLokalIni ("EKSTVPILEV", CB-EkstVPILev:SCREEN-VALUE IN FRAME DEFAULT-FRAME).

  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-Info2
&Scoped-define SELF-NAME ArtBas.AlfaKode2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ArtBas.AlfaKode2 C-ArtKort
ON TAB OF ArtBas.AlfaKode2 IN FRAME FRAME-Info2 /* Landkode */
OR RETURN OF ArtBas.AlfaKode2
DO:
    IF NOT CAN-FIND(FIRST AlfaLandKode WHERE
                    AlfaLandKode.AlfaKode2 = ArtBas.AlfaKode2:SCREEN-VALUE IN FRAME FRAME-Info2) THEN
    DO:
        MESSAGE 'Ugyldig landkode'
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.
    ELSE DO:
        IF AVAILABLE NumLandKode THEN RELEASE NumLandKode.
        FIND FIRST AlfaLandKode NO-LOCK WHERE 
            AlfaLandKode.AlfaKode2 = ArtBas.AlfaKode2:SCREEN-VALUE IN FRAME FRAME-Info2 NO-ERROR.
        IF AVAILABLE AlfaLandKode THEN
            FIND NumLandKode OF AlfaLandKode NO-LOCK NO-ERROR.
        IF AVAILABLE NumLandKode THEN
            ASSIGN
            FI-Land:SCREEN-VALUE IN FRAME FRAME-Info2 = NumLandKode.Land.
        ELSE
            ASSIGN
            FI-Land:SCREEN-VALUE IN FRAME FRAME-Info2 = ''.
    END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-ArtInfo
&Scoped-define SELF-NAME ArtBas.Anbrekk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ArtBas.Anbrekk C-ArtKort
ON VALUE-CHANGED OF ArtBas.Anbrekk IN FRAME FRAME-ArtInfo /* Anbrekk */
DO:
  IF NOT SELF:CHECKED THEN
      ASSIGN
      ArtBas.InkrAnbrekk:SCREEN-VALUE = ArtBas.AntIPakn:SCREEN-VALUE.
  ASSIGN
    ArtBas.InkrAnbrekk:SENSITIVE = ArtBas.Anbrekk:CHECKED.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ArtBas.anv-id
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ArtBas.anv-id C-ArtKort
ON LEAVE OF ArtBas.anv-id IN FRAME FRAME-ArtInfo /* Brukskode */
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
&Scoped-define SELF-NAME ArtBas.ArtSlag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ArtBas.ArtSlag C-ArtKort
ON VALUE-CHANGED OF ArtBas.ArtSlag IN FRAME DEFAULT-FRAME /* Varetype */
DO:
  bOk = FALSE.
  MESSAGE 'Du endrer nå en grunnleggende egenskap ved artikkelen.' SKIP
          'Er du sikker på at du vil gjøre dette?' SKIP(1)
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE bOk.
  IF bOk = FALSE THEN
  DO:
      ArtBas.ArtSlag:SCREEN-VALUE = STRING(ArtBas.ArtSlag).
      RETURN NO-APPLY.
  END.
  IF AVAILABLE ArtBas THEN
  DO:
      IF CAN-DO('7',STRING(INPUT ArtBas.Artslag)) THEN
          ArtBas.Pakke:CHECKED = TRUE.
      ELSE 
          ArtBas.Pakke:CHECKED = FALSE.

      IF CAN-DO('8',STRING(INPUT ArtBas.Artslag)) THEN
          ArtBas.Pant:CHECKED = TRUE.
      ELSE
          ArtBas.Pant:CHECKED = FALSE.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-ArtInfo
&Scoped-define SELF-NAME B-alternativeLev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-alternativeLev C-ArtKort
ON CHOOSE OF B-alternativeLev IN FRAME FRAME-ArtInfo /* ... */
OR F10 OF S-AlternativLev
DO:
  IF NOT AVAILABLE ArtBas THEN
      RETURN.

  IF wModus <> "NY" AND AVAIL ArtBas AND ArtBas.ArtikkelNr = ArtBas.Vg THEN DO:
      MESSAGE "PLU-artikkel, endring avbrutt."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
  END.


  cLevBasRowIdList = "".
  FOR EACH AltLevBas NO-LOCK WHERE
      AltLevBas.ArtikkelNr = ArtBas.ArtikkelNr:
      FIND LevBas OF AltLevBas NO-ERROR.
      IF AVAILABLE LevBas THEN
      ASSIGN
          cLevBasRowIdList = cLevBasRowIdList + 
                             (IF cLevBasRowIdList <> ""
                                THEN ","
                                ELSE "") + 
                              STRING(ROWID(LevBas))
          .

  END.
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "LevBas;levnr;levnamn;KjedeAvtale|Kjedeavtale|J/N",
                      "where true",
                      INPUT-OUTPUT cLevBasRowIdList,
                      "Levnr",
                      INPUT-OUTPUT cLevBasIdList, 
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk THEN 
  DO:
      setAlternativLev(S-AlternativLev:HANDLE,cLevBasRowIdList).
      RUN LagreArtBas (0).
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-Lager
&Scoped-define SELF-NAME B-ByggomLager
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-ByggomLager C-ArtKort
ON CHOOSE OF B-ByggomLager IN FRAME FRAME-Lager /* Bygg om lager */
DO:
  DEFINE VARIABLE cButik AS CHARACTER  NO-UNDO.

  bOk = FALSE.
  MESSAGE 'Lagerteller på artikklen blir bygget opp på nyt ut fra transaksjonene som ligger på artikkelen.' SKIP
          'Deretter gjøres en refresh av skjermbilde slik at de oppdaterte lagertellerne vises.' SKIP(1)
          '(Lager og Artlag postene slettes og opprettes på nytt ut fra transaksjonene).'
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE bOk.
  IF bOk = FALSE THEN
      RETURN NO-APPLY.

  RUN korrigerArtlag_fra_translogg.p (ArtBas.ArtikkelNr).
  ASSIGN cButik = tmplager.butik.
  FIND Lager WHERE Lager.ArtikkelNr = ArtBas.ArtikkelNr AND
                   Lager.Butik      = INT(tmplager.butik) NO-LOCK NO-ERROR.
  wRS-Vis = INPUT RS-Vis.
  RUN VisLager (TRUE).  
  FIND tmplager WHERE tmplager.butik = cButik NO-ERROR.
  IF AVAIL tmplager THEN
      REPOSITION BROWSE-Lager TO ROWID(ROWID(tmplager)) NO-ERROR.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
    
    IF AVAIL ArtBas AND (ArtBas.OPris = TRUE OR ArtBas.NON_Sale = TRUE) THEN
        RETURN NO-APPLY.

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
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME B-Folder
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Folder C-ArtKort
ON CHOOSE OF B-Folder IN FRAME DEFAULT-FRAME /* Folder... */
DO:
    DEFINE VARIABLE cOrgEnaDis AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE ii AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iTag AS INTEGER    NO-UNDO.
    ASSIGN chTabs     = chTabStrip:Tabs.

/*            chTabs:Item(4):Caption = "Pakke" */
/*            chTabs:ITEM(4):KEY = "A4".       */
/*     chTabs:Add(4,"A" + STRING(4),"Pakke"). */
/*   chTabs:Item (2 BY-VARIANT-POINTER). */
/*   chTab1Side = chTabs:Item ("A2" BY-VARIANT-POINTER). */
/*   chTabStrip:SelectedItem = chTab1Side.               */
       ASSIGN hAktivHandle = ?.
    cOrgEnaDis = cFlikEnaDis.
    RUN d-faneaktivering.w (INPUT wTabTekst,INPUT cFlikDefault, cSport1Default ,INPUT-OUTPUT cFlikEnaDis).

    IF cOrgEnaDis <> cFlikEnaDis THEN DO:
        /* !! ny 5/5 tst ta bort alla */
        DO ii = 4 TO NUM-ENTRIES(cOrgEnaDis):
            IF ENTRY(ii,cOrgEnaDis) = "E" THEN DO:
                chTabs:remove("A" + STRING(ii) BY-VARIANT-POINTER).
            END.
        END.
        /* Lägg in alla E */
        ii = 3.
        DO iTag = 4 TO NUM-ENTRIES(wTabTekst):
            IF ENTRY(iTag,cFlikEnaDis) = "E" THEN DO:
                ii = ii + 1.
                chTabs:Add(ii,"A" + STRING(iTag),ENTRY(iTag,wTabTekst)).
            END.
        END.

                        /*         /* Lägg in alla  */                                          */
                        /*         DO ii = 1 TO NUM-ENTRIES(wTabTekst):                         */
                        /*             IF ENTRY(ii,cOrgEnaDis) = "D" THEN DO:                   */
                        /*                 chTabs:Add(ii,"A" + STRING(ii),ENTRY(ii,wTabTekst)). */
                        /*             END.                                                     */
                        /*         END.                                                         */
                        /*         /* Ta bort disablade */                                      */
                        /*         DO ii = 1 TO NUM-ENTRIES(cFlikEnadis):                       */
                        /*             IF ENTRY(ii,cFlikEnaDis) = "D" THEN DO:                  */
                        /*                 chTabs:remove("A" + STRING(ii) BY-VARIANT-POINTER).  */
                        /*             END.                                                     */
                        /*         END.                                                         */
        /* Uppdatera cAktivFlipTag */
        cAktivFlipTag = "".
        DO ii = 1 TO NUM-ENTRIES(cFlikEnadis):
            IF ENTRY(ii,cFlikEnaDis) = "D" THEN
                NEXT.
            cAktivFlipTag = cAktivFlipTag + (IF cAktivFlipTag <> "" THEN "," ELSE "") + "A" + STRING(ii).
        END.
/*   chTab1Side = chTabs:Item ("A2" BY-VARIANT-POINTER). */
/*   chTabStrip:SelectedItem = chTab1Side.               */
    
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-Lager
&Scoped-define SELF-NAME B-ForenklVaremot
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-ForenklVaremot C-ArtKort
ON CHOOSE OF B-ForenklVaremot IN FRAME FRAME-Lager /* Forenklet varemottak... */
DO:
    RUN Lagerjustering (5).
    RUN ByttArtikkel IN THIS-PROCEDURE (Artbas.artikkelnr).
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME B-Jamfor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Jamfor C-ArtKort
ON CHOOSE OF B-Jamfor IN FRAME DEFAULT-FRAME /* Sammenlign... */
DO:
    IF wModus = "NY" OR NOT AVAIL ArtBas OR VALID-HANDLE(hJmfRutine) THEN
        RETURN.
    RUN w-bildejmf.w PERSISTENT SET hJmfRutine.
    CREATE tmpChild.
    ASSIGN tmpChild.wChild = hJmfRutine.
    RUN VisJmf.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Kobble
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Kobble C-ArtKort
ON CHOOSE OF B-Kobble IN FRAME DEFAULT-FRAME /* Koble web */
DO:
    /* vi skall nog inte göra detta på grund av transaktionsproblematik */
DEF VAR cTekst                      AS CHAR NO-UNDO.
DEFINE VARIABLE cRowIdList AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cIdList    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iReturn AS INTEGER     NO-UNDO.
DEFINE VARIABLE bOK AS LOG   NO-UNDO.
    FOR EACH artbut WHERE artbut.artikkelnr = artbas.artikkelnr AND artbut.deleted = FALSE NO-LOCK:
        FIND butiker WHERE butiker.butik = artbut.butik NO-LOCK NO-ERROR.
        IF AVAIL butiker THEN
    ASSIGN cRowIdList = cRowIdList + (IF cRowIdList <> "" THEN "," ELSE "") + STRING(ROWID(butiker))
           cIdList    = cIdList    + (IF cIdList <> "" THEN "|" ELSE "") + STRING(butiker.butik).
    END.
    RUN JBoxSelector.w (THIS-PROCEDURE,0,
                        "Butiker;Butik;ButNamn;!webbutik",
                        "where Butiker.webbutik",
                        INPUT-OUTPUT cRowIdList,
                        "Butik",
                        INPUT-OUTPUT cIdList,
                        "","",
                        OUTPUT bOK).
IF bOK = TRUE THEN DO WITH FRAME FRAME-ArtInfo:
    IF cIdList <> "" THEN DO:
        IF ArtBas.PubliserINettbutikk = FALSE OR ArtBas.WebButikkArtikkel = FALSE THEN DO:
            ASSIGN ArtBas.PubliserINettbutikk:CHECKED = TRUE
                   ArtBas.WebButikkArtikkel:CHECKED = TRUE.
            RUN LagreArtBas (99).
        END.
        cTekst = REPLACE(cIdList,"|",";").
        IF NOT DYNAMIC-FUNCTION("RunProc","art_sett_WebArtikkelBut.p",
                                cTekst + ",ARTNUM," + STRING(artbas.artikkelnr),?) THEN .
    END.
    ELSE IF cIdList = "" THEN DO:
        ASSIGN ArtBas.PubliserINettbutikk:CHECKED = FALSE
               ArtBas.WebButikkArtikkel:CHECKED = FALSE.
        RUN LagreArtBas (99).
    END.
/*         DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Advarsel",""). */
  RUN VisArtBas.
END.
ELSE
    RETURN.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-Lager
&Scoped-define SELF-NAME B-Lagerjustering
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Lagerjustering C-ArtKort
ON CHOOSE OF B-Lagerjustering IN FRAME FRAME-Lager /* Lagerjustering... */
DO:
    RUN Lagerjustering (7).
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Nedskrivning
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Nedskrivning C-ArtKort
ON CHOOSE OF B-Nedskrivning IN FRAME FRAME-Lager /* Endre varekost... */
DO:
    RUN Lagerjustering (8).
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-OppdLAger
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-OppdLAger C-ArtKort
ON CHOOSE OF B-OppdLAger IN FRAME FRAME-Lager /* Oppdater lagerbilde */
DO:
  DEFINE VARIABLE cButik AS CHARACTER  NO-UNDO.

/*   bOk = FALSE.                                                                                                   */
/*   MESSAGE 'Lagerteller på artikklen blir bygget opp på nyt ut fra transaksjonene som ligger på artikkelen.' SKIP */
/*           'Deretter gjøres en refresh av skjermbilde slik at de oppdaterte lagertellerne vises.' SKIP(1)         */
/*           '(Lager og Artlag postene slettes og opprettes på nytt ut fra transaksjonene).'                        */
/*       VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE bOk.                                                      */
/*   IF bOk = FALSE THEN                                                                                            */
/*       RETURN NO-APPLY.                                                                                           */
/*                                                                                                                  */
/*   RUN korrigerArtlag_fra_translogg.p (ArtBas.ArtikkelNr).                                                        */
  ASSIGN cButik = tmplager.butik.
  FIND Lager WHERE Lager.ArtikkelNr = ArtBas.ArtikkelNr AND
                   Lager.Butik      = INT(tmplager.butik) NO-LOCK NO-ERROR.
  wRS-Vis = INPUT RS-Vis.
  wVisLager = ?.
  RUN VisLager (TRUE).  
  FIND tmplager WHERE tmplager.butik = cButik NO-ERROR.
  IF AVAIL tmplager THEN
      REPOSITION BROWSE-Lager TO ROWID(ROWID(tmplager)) NO-ERROR.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-ArtInfo
&Scoped-define SELF-NAME B-Refresh
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Refresh C-ArtKort
ON CHOOSE OF B-Refresh IN FRAME FRAME-ArtInfo /* ... */
DO:
    IF iVgTxtBeskr = 1 OR iVgTxtBeskr = 3 THEN 
        ArtBas.BongTekst:SCREEN-VALUE IN FRAME FRAME-ArtInfo = SUBSTR(FILL-IN-VgBeskr:SCREEN-VALUE IN FRAME DEFAULT-FRAME,1,20).
    ELSE IF iVgTxtBeskr = 2 THEN 
        ArtBas.BongTekst:SCREEN-VALUE IN FRAME FRAME-ArtInfo = SUBSTR(ArtBas.Beskr:SCREEN-VALUE IN FRAME DEFAULT-FRAME,1,20).
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-Lager
&Scoped-define SELF-NAME B-Reklamer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Reklamer C-ArtKort
ON CHOOSE OF B-Reklamer IN FRAME FRAME-Lager /* Reklamer restlager... */
DO:
  IF artbas.sanertdato <> ? THEN
      RETURN NO-APPLY.
  IF cBrukPWLagertrans = '1' THEN DO:
    RUN d-adgangskontroll.w(INPUT 'Adgangskontroll',INPUT cPwd).
    IF RETURN-VALUE <> 'OK' THEN
      RETURN NO-APPLY.
  END.    
  IF AVAIL ArtBas AND (ArtBas.OPris = TRUE OR ArtBas.NON_Sale = TRUE) THEN DO:
      MESSAGE "Artikkel med åpen pris og NON-Sale artikler, kan ikke velges."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
  END.
  RUN ReklamerRestlager.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME B-Sanera
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Sanera C-ArtKort
ON CHOOSE OF B-Sanera IN FRAME DEFAULT-FRAME /* Saner... */
DO:
  /*
  IF Artbas.Opris = TRUE THEN DO:
      MESSAGE "Vare med åpen pris"
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      RETURN NO-APPLY.
  END.
  */
   IF artbas.sanertdato <> ? THEN DO:
      MESSAGE "Artikkelen er sanert"
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      RETURN NO-APPLY.
   END.
  RUN SanerArtbas IN THIS-PROCEDURE.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Sok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Sok C-ArtKort
ON CHOOSE OF B-Sok IN FRAME DEFAULT-FRAME /* Søk */
DO:
  APPLY "ALT-S" TO C-ArtKort.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-ArtInfo
&Scoped-define SELF-NAME B-SokLinkvare
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SokLinkvare C-ArtKort
ON CHOOSE OF B-SokLinkvare IN FRAME FRAME-ArtInfo /* ... */
OR F10 OF ArtBas.LinkVareNr
DO:
  
  DO WITH FRAME FRAME-ArtInfo:

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.

  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "ArtBas;ArtikkelNr;Beskr",
                      "where ArtBas.Pant = true",
                      INPUT-OUTPUT cTekst,
                      "ArtikkelNr",
                      INPUT-OUTPUT cTekst,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

    IF RETURN-VALUE = "AVBRYT" THEN
        RETURN NO-APPLY.
    FIND bArtBas NO-LOCK WHERE
      bArtBas.ArtikkelNr = dec(cTekst) NO-ERROR.
    IF AVAILABLE bArtBas THEN
    DO:
        /* Legger opp verdier I de aktuelle feltene */
        ASSIGN
          ArtBas.LinkVareNr:SCREEN-VALUE = cTekst
          FI-LinkVareTekst:SCREEN-VALUE  = bArtBas.Beskr
          .
    END.
    ELSE DO:
        /* Legger opp verdier I de aktuelle feltene */
        ASSIGN
          ArtBas.LinkVareNr:SCREEN-VALUE = ''
          FI-LinkVareTekst:SCREEN-VALUE  = ''
          .
    END.
    END.
    APPLY "ENTRY" TO ArtBas.ProdNr.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SokLinkvare-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SokLinkvare-2 C-ArtKort
ON CHOOSE OF B-SokLinkvare-2 IN FRAME FRAME-ArtInfo /* ... */
OR F10 OF ArtBas.LinkVareNr
DO:
  DO WITH FRAME FRAME-ArtInfo:
    IF TRIM(ArtBas.Link_Til_Nettside:SCREEN-VALUE) <> '' THEN
      RUN OpenWeb(ArtBas.Link_Til_Nettside:SCREEN-VALUE).
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-Lager
&Scoped-define SELF-NAME B-Svinn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Svinn C-ArtKort
ON CHOOSE OF B-Svinn IN FRAME FRAME-Lager /* Svinn... */
DO:
    RUN Lagerjustering (9).
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME B-VPI
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-VPI C-ArtKort
ON CHOOSE OF B-VPI IN FRAME DEFAULT-FRAME /* Oppslag i VPI register... */
DO:
  ASSIGN
      hFocus = FOCUS
      .
  IF VALID-HANDLE(hModell) THEN DO:
      RUN EndreKlar IN hModell.
      RETURN.
  END.

  APPLY "ALT-V":U TO C-ArtKort.
  IF VALID-HANDLE(hFocus) THEN
  DO:
      APPLY "ENTRY" TO hFocus.
      ASSIGN
          hFocus:CURSOR-OFFSET = 1 NO-ERROR.
  END.
  ELSE DO:
      APPLY "ENTRY" TO ArtBas.Beskr IN FRAME DEFAULT-FRAME.
      ASSIGN
          ArtBas.Beskr:CURSOR-OFFSET = 1 NO-ERROR.
  END.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-ArtInfo
&Scoped-define SELF-NAME ArtBas.BehKode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ArtBas.BehKode C-ArtKort
ON LEAVE OF ArtBas.BehKode IN FRAME FRAME-ArtInfo /* Behanl.kode */
DO:
  FIND Handtering NO-LOCK WHERE
    Handtering.HandKode = int(ArtBas.BehKode:screen-value IN FRAME FRAME-ArtInfo) NO-ERROR.
  IF AVAILABLE Handtering THEN
    DISPLAY Handtering.BEskrivelse WITH FRAME FRAME-ArtInfo.
  ELSE DO:
      DISPLAY
        "" @ ArtBas.BehKode
        "" @ Handtering.Beskrivelse
      WITH FRAME FRAME-ArtInfo.  
      RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME ArtBas.Beskr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ArtBas.Beskr C-ArtKort
ON LEAVE OF ArtBas.Beskr IN FRAME DEFAULT-FRAME /* Beskrivelse */
DO:
  IF (SELF:SCREEN-VALUE <> "" AND bKopierVaretekst = TRUE) THEN
  DO:
      ASSIGN
          ArtBas.BongTekst:SCREEN-VALUE IN FRAME FRAME-ArtInfo    = SUBSTR(ArtBas.Beskr:SCREEN-VALUE IN FRAME Default-Frame,1,20)
          ArtBas.Etikettekst1:SCREEN-VALUE IN FRAME FRAME-ArtInfo = ArtBas.Beskr:SCREEN-VALUE IN FRAME Default-Frame 
          .
  END.
  ELSE IF (SELF:SCREEN-VALUE <> "" AND iVgTxtBeskr = 2) THEN 
  DO:
    IF ArtBas.BongTekst:SCREEN-VALUE IN FRAME FRAME-ArtInfo = "" THEN
    DO:
      DISPLAY
        SUBSTR(ArtBas.Beskr:SCREEN-VALUE IN FRAME Default-Frame,1,20) @ ArtBas.BongTekst 
      WITH FRAME FRAME-ArtInfo.
    END.
    IF ArtBas.Etikettekst1:SCREEN-VALUE IN FRAME FRAME-ArtInfo = "" THEN
    DO:
      DISPLAY
        SUBSTR(ArtBas.Beskr:SCREEN-VALUE IN FRAME Default-Frame,1,30) @ ArtBas.Etikettekst1 
      WITH FRAME FRAME-ArtInfo.
    END.  
  END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ArtBas.Beskr C-ArtKort
ON RETURN OF ArtBas.Beskr IN FRAME DEFAULT-FRAME /* Beskrivelse */
DO:
    APPLY "ENTRY" TO ArtBas.LevNr.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ArtBas.Beskr C-ArtKort
ON TAB OF ArtBas.Beskr IN FRAME DEFAULT-FRAME /* Beskrivelse */
DO:
    APPLY "ENTRY" TO ArtBas.LevNr.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ArtBas.BestForslag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ArtBas.BestForslag C-ArtKort
ON VALUE-CHANGED OF ArtBas.BestForslag IN FRAME DEFAULT-FRAME /* Best.forslag */
DO:
    IF VALID-HANDLE(hArtBestPkt) THEN
    RUN SetEntry IN hArtBestPkt.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-Lager
&Scoped-define FRAME-NAME FRAME-Lager
&Scoped-define SELF-NAME BROWSE-Lager
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-Lager C-ArtKort
ON ROW-DISPLAY OF BROWSE-Lager IN FRAME FRAME-Lager
DO:
/*   MESSAGE PROGRAM-NAME(0) SKIP           */
/*           PROGRAM-NAME(1) SKIP           */
/*           PROGRAM-NAME(2) SKIP           */
/*           PROGRAM-NAME(3) SKIP           */
/*       VIEW-AS ALERT-BOX INFO BUTTONS OK. */
  IF tmpLager.Butik = "Total" THEN
  DO:
      RUN SetBrowseRadFont.
  END.
  
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
  /*ASSIGN skotex.ArtBas.Pakke:SENSITIVE IN FRAME DEFAULT-FRAME = FALSE.*/

  wKopi = FALSE.
  iKopierBildeNr = 0.
  IF chTabStrip:ENABLED = FALSE THEN
      ASSIGN chTabStrip:ENABLED = TRUE.
  RUN RensFrame.
  ASSIGN wModus = "ENDRE".
  RUN VisArtBas.
  RUN ByttFrame.
  RUN VisLager (TRUE).
  RUN Toolbar.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-Info2
&Scoped-define SELF-NAME BUTTON-Karakteristikk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Karakteristikk C-ArtKort
ON CHOOSE OF BUTTON-Karakteristikk IN FRAME FRAME-Info2 /* ... */
OR F10 OF S-Karakteristikk
DO:
  DEF VAR cKarakteristikkRowIdLst AS CHAR NO-UNDO.
  DEF VAR cKarakteristikkIdLst AS CHAR NO-UNDO.
  IF NOT AVAILABLE ArtBas THEN
      RETURN.

  IF wModus <> "NY" AND AVAIL ArtBas AND ArtBas.ArtikkelNr = ArtBas.Vg THEN DO:
      MESSAGE "PLU-artikkel, endring avbrutt."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
  END.


  cKarakteristikkRowIdLst = "".
  FOR EACH ArtBasKarakteristikk NO-LOCK WHERE
      ArtBasKarakteristikk.ArtikkelNr = ArtBas.ArtikkelNr:
      FIND Karakteristikk OF ArtBasKarakteristikk NO-ERROR.
      IF AVAILABLE Karakteristikk THEN
      ASSIGN
          cKarakteristikkRowIdLst = cKarakteristikkRowIdLst + 
                             (IF cKarakteristikkRowIdLst <> ""
                                THEN ","
                                ELSE "") + 
                              STRING(ROWID(Karakteristikk))
          .

  END.
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "Karakteristikk;KarakteristikkId;KBeskrivelse",
                      "where true",
                      INPUT-OUTPUT cKarakteristikkRowIdLst,
                      "KarakteristikkId",
                      INPUT-OUTPUT cKarakteristikkIdLst, 
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk THEN 
  DO:
      setKarakteristikk (S-Karakteristikk:HANDLE,cKarakteristikkRowIdLst).
      RUN LagreArtBas (0).
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME BUTTON-Kopier
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Kopier C-ArtKort
ON CHOOSE OF BUTTON-Kopier IN FRAME DEFAULT-FRAME /* Kopiera */
DO:
  /* Opprettelse med forenklet artikkelkort. For servicehandelen. */
  IF CAN-DO('1,J,Ja,Yes,true',cServicehandel) OR CAN-DO('2',cServicehandel) THEN 
  OPPRETTELSE_I_DIALOG:
  DO:
      ASSIGN bFraKopier = TRUE
             lArtikkelNr = DEC(ArtBAs.ArtikkelNr:SCREEN-VALUE IN FRAME DEFAULT-FRAME).
      APPLY 'choose' TO BUTTON-Ny.
      bFraKopier = FALSE.
      RETURN NO-APPLY.
  END.
  
  IF VALID-HANDLE(hModell) THEN DO:
      RUN EndreKlar IN hModell.
      RETURN.
  END.
  IF artbas.sanertdat <> ? THEN DO:
      MESSAGE "Artikkelen er sanert"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
  END.
  IF NOT SELF:SENSITIVE THEN
      RETURN NO-APPLY.
  IF ArtBas.ArtikkelNr = ArtBas.Vg THEN DO:
      MESSAGE "PLUartikkel, kopiering ikke tillatt."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
  END.
  /* Lagrer det som er endret. */
  RUN LagreArtBas(1).
  
  /* Bytter til første flik */
  ASSIGN 
    wAktivFlip = 1
    wModus   = "NY"
    wKopi    = TRUE
    iKopierBildeNr = ArtBas.BildNr.
  RUN ByttFrame.  
  RUN BlankLager.
  RUN Toolbar.
  DISPLAY 
    "" @ ArtBas.ArtikkelNr 
    "" @ ArtBas.LopNr
/*     "" @ ArtBas.LevKod */
  WITH FRAME DEFAULT-FRAME.
  DISPLAY 
      "" @ ArtBas.Bongtekst
      WITH FRAME FRAME-ArtInfo.
  IF iMellankategori = 0 THEN
      initUnderkategori().
  ELSE IF iMellankategori = 1 THEN
      initMellankategori().
  ASSIGN
      ArtBas.WebButikkArtikkel:SCREEN-VALUE IN FRAME FRAME-ArtInfo = 'NO'
      ArtBas.PubliserINettbutikk:SCREEN-VALUE IN FRAME FRAME-ArtInfo  = 'NO'
      ArtBas.Notat:SCREEN-VALUE IN FRAME FRAME-ArtInfo  = ''
      Artbas.Utgatt:SCREEN-VALUE = "no"
      Artbas.Utgattdato:SCREEN-VALUE = ?.
/*   IF ArtBas.Etikettekst1:SCREEN-VALUE IN FRAME FRAME-ArtInfo  = '' THEN                                            */
/*       ArtBas.Etikettekst1:SCREEN-VALUE IN FRAME FRAME-ArtInfo  = ArtBas.Beskr:SCREEN-VALUE IN FRAME DEFAULT-FRAME. */
  DISPLAY 
      "0" @ ArtBas.AnbefaltPris
      WITH FRAME FRAME-Info2.
  ArtBas.Varefakta:SCREEN-VALUE IN FRAME FRAME-Info2 = "".


  ASSIGN 
/*       ArtBas.BongTekst:SCREEN-VALUE IN FRAME Frame-ArtInfo = "" */
         chTabStrip:ENABLED = FALSE.
  APPLY "TAB":U TO ArtBas.Vg IN FRAME DEFAULT-FRAME.
  APPLY "ENTRY":U TO ArtBas.Vg IN FRAME DEFAULT-FRAME.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Kopierbilde
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Kopierbilde C-ArtKort
ON CHOOSE OF BUTTON-Kopierbilde IN FRAME DEFAULT-FRAME
DO:
    IF AVAIL ArtBas AND ArtBas.BildNr > 0 THEN DO:
        BUTTON-Kopierbilde:PRIVATE-DATA IN FRAME DEFAULT-FRAME = STRING(ArtBas.BildNr).
        MENU-ITEM m_Angre_kopier:SENSITIVE IN MENU POPUP-MENU-BUTTON-Kopierbilde = 
                                 MENU-ITEM m_Kopier:SENSITIVE  IN MENU POPUP-MENU-BUTTON-Kopierbilde.
    END.
/*     MESSAGE "Bruk høyre musetast"          */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK. */
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Lagre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Lagre C-ArtKort
ON CHOOSE OF BUTTON-Lagre IN FRAME DEFAULT-FRAME /* Lagra */
DO:
  DEF VAR bTmpKopi AS LOG NO-UNDO.
  
  IF ArtBas.StrTypeID:SENSITIVE IN FRAME FRAME-ArtInfo THEN 
      APPLY "TAB" TO ArtBas.StrTypeID IN FRAME FRAME-ArtInfo.
  IF ERROR-STATUS:ERROR THEN DO:
      ASSIGN ERROR-STATUS:ERROR = FALSE.
  END.
  ASSIGN
      bTmpKopi = wKopi.
  IF AVAILABLE ArtBas THEN
      /* Logger for sjekk av endring som krever etikettutskrift. */
      cFor = STRING(ArtBas.vg) + '|' + 
             ArtBas.Beskr + '|' + 
             ArtBas.LevKod + '|' + 
             ArtBas.LevFargKod + '|' + 
             STRING(ArtBas.VmId) + '|' + 
             STRING(ArtBas.ProdNr) + '|' + 
             ArtBas.BongTekst + '|' + 
             ArtBas.Etikettekst1 + '|' + 
             ArtBas.Etikettekst2 + '|' + 
             STRING(ArtBas.LinkVareNr) + '|' + 
             STRING(ArtBas.JamforEnhet) + '|' + 
             STRING(ArtBas.Mengde). 
      .
  RUN LagreArtBas (0).
  IF RETURN-VALUE <> "AVBRYT" THEN DO:
      ASSIGN
          /* Logger for sjekk av endring som krever etikettutskrift. */
          cEtter = ArtBas.vg:SCREEN-VALUE + '|' + 
                   ArtBas.Beskr:SCREEN-VALUE + '|' + 
                   ArtBas.LevKod:SCREEN-VALUE + '|' + 
                   ArtBas.LevFargKod:SCREEN-VALUE + '|' + 
                   ArtBas.VmId:SCREEN-VALUE + '|' + 
                   ArtBas.ProdNr:SCREEN-VALUE + '|' + 
                   ArtBas.BongTekst:SCREEN-VALUE + '|' + 
                   ArtBas.Etikettekst1:SCREEN-VALUE + '|' + 
                   ArtBas.Etikettekst2:SCREEN-VALUE + '|' + 
                   ArtBas.LinkVareNr:SCREEN-VALUE + '|' + 
                   ArtBas.JamforEnhet:SCREEN-VALUE + '|' + 
                   STRING(DEC(ArtBas.Mengde:SCREEN-VALUE)). 
            .

      /* logger endring i etikett hvis det er etikettvang. */
      IF wModus = 'ENDRE' AND (cFor <> cEtter) AND bEtiTvang AND AVAILABLE Bruker THEN 
        DO:
          FIND Bruker NO-LOCK WHERE 
              Bruker.BrukerId = USERID('SkoTex') NO-ERROR.

          cTekst = ''.
          FOR EACH Strekkode OF ArtBas NO-LOCK WHERE
              LENGTH(Strekkode.Kode) > 5:
              ASSIGN
                  cTekst = cTekst + 
                           (IF cTekst <> '' THEN ';' ELSE '') + 
                           STRING(Strekkode.ArtikkelNr) + '|' + Strekkode.Kode.
          END.
          IF (Bruker.BrukerType = 2 AND Bruker.Butik <> 0) THEN
            DO:
              FIND Butiker NO-LOCK WHERE
                  Butiker.Butik = Bruker.Butik NO-ERROR.
              IF AVAILABLE Butiker THEN
                  DYNAMIC-FUNCTION("runProc","etikettartpris_send_til.p",
                               STRING(Butiker.ProfilNr) + ";" + STRING(Butiker.Butik) + ";" + "1;" + "No" + "¤" + cTekst,?).
            END.
          ELSE DO:
              ALLE_PRISPROFIL:
              FOR EACH PrisProfil NO-LOCK WHERE
                  CAN-FIND(FIRST Butiker WHERE 
                           Butiker.ProfilNr = PrisProfil.ProfilNr AND
                           Butiker.ApningsDato <= TODAY AND
                           Butiker.harButikksystem = TRUE AND
                           Butiker.NedlagtDato = ?): 

                  FIND FIRST Butiker NO-LOCK WHERE
                      Butiker.ProfilNr = PrisProfil.ProfilNr AND
                      Butiker.ApningsDato <= TODAY AND
                      Butiker.harButikksystem = TRUE AND
                      Butiker.NedlagtDato = ? NO-ERROR.
                  /* Etikettrutinen legger opp også for eventuelle andre butikker på profilen. */
                  IF AVAILABLE Butiker THEN 
                  DO:
                      /* Er det salg på artikkelen for butikken skal den stoppe i priskø. */
                      IF CAN-FIND (FIRST TransLogg WHERE
                                       TransLogg.ArtikkelNr  = ArtBas.ArtikkelNr AND
                                       TransLogg.Dato        > TODAY - (24 * 31) AND 
                                       TransLogg.Tid        >= 0 AND
                                       TransLogg.Butik       = Butiker.Butik AND
                                       TransLogg.TTId        = 1) 
                         OR 
                         CAN-FIND(FIRST ArtPris WHERE
                                  ArtPris.ArtikkelNr = lArtikkelNr AND
                                  ArtPris.ProfilNr   = PrisProfil.ProfilNr)
                      THEN DYNAMIC-FUNCTION("runProc","etikettartpris_send_til.p",
                                   STRING(Butiker.ProfilNr) + ";" + STRING(Butiker.Butik) + ";" + "1;" + "No" + "¤" + cTekst,
                                   ?).
                  END.
              END. /* ALLE_PRISPROFIL */
          END.
        END.

      ASSIGN wModus = "ENDRE".
      IF chTabStrip:ENABLED = FALSE THEN DO:
          ASSIGN wAktivFlip = 3.
/*           ASSIGN wAktivFlip = 2.  !!! 01062009  !!! */
          RUN ByttFrame. /* For † enable knapper og felt korrekt */
          IF NOT bTmpKopi THEN
            FRAME DEFAULT-FRAME:SENSITIVE = FALSE.
/*           ASSIGN chTabStrip:ENABLED = TRUE. */
      END.
      IF VALID-HANDLE(hModell) THEN DO:
          RUN EndreKlar IN hModell.
          RETURN NO-APPLY.
      END.
      IF NOT ArtBas.MengdeRabatt:CHECKED IN FRAME Frame-ArtInfo THEN
          FOR EACH ArtPris OF ArtBas TRANSACTION:
            ASSIGN
                ArtPris.MengdeRabantall = 0
                ArtPris.MengdeRabPris   = 0.
          END.
      IF VALID-HANDLE(wKalkyle) THEN RUN ByttObjekt IN wKalkyle (ArtBas.ArtikkelNr).
  END.
  ELSE DO:
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-ModellFarg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-ModellFarg C-ArtKort
ON CHOOSE OF BUTTON-ModellFarg IN FRAME DEFAULT-FRAME /* ModellFarg */
DO:
    DEFINE BUFFER bModell FOR artbas.
  IF VALID-HANDLE(hModell) THEN DO:
      RUN EndreKlar IN hModell.
      RETURN NO-APPLY.
  END.
  IF artbas.sanertdato <> ? THEN DO:
      MESSAGE "Artikkelen er sanert"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
  END.
  IF ArtBas.OPris OR ArtBas.Pakke OR ArtBas.NON_Sale THEN DO:
      MESSAGE "PLU/Pakkevarer/NON-Sale ikke tillatt!"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
  END.
  ELSE IF ArtBas.LopNr = ? OR ArtBas.LopNr = 0 THEN DO:
      MESSAGE "Løpenummer ikke tildelt!"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
  END.
  RUN LagreArtBas (0).
  IF RETURN-VALUE = "AVBRYT" THEN
    RETURN NO-APPLY.
  IF ArtBas.ModellFarge > 0 AND ArtBas.ModellFarge <> Artbas.artikkelnr AND NOT CAN-FIND(bModell WHERE bModell.artikkelnr = ArtBas.ModellFarge) THEN DO:
      FIND CURRENT ArtBas EXCLUSIVE.
      ASSIGN ArtBas.ModellFarge = 0
             ArtBas.Hovedmodell = FALSE.
      RUN LagreArtBas (0).
  END.
  ELSE IF ArtBas.ModellFarge > 0 AND ArtBas.ModellFarge = Artbas.artikkelnr AND 
          NOT CAN-FIND(FIRST bModell WHERE bModell.modellfarge = ArtBas.ModellFarge AND bModell.artikkelnr <> bModell.ModellFarge) THEN DO:
      FIND CURRENT ArtBas EXCLUSIVE.
      ASSIGN ArtBas.ModellFarge = 0
             ArtBas.Hovedmodell = FALSE.
      RUN LagreArtBas (0).
  END.

  IF ArtBas.ModellFarge = 0 THEN DO:
      MESSAGE "Har du kontrollert: (kopieres til modell/farge)" SKIP
          "  - Kalkyle" SKIP
          "  - Sesong" SKIP
          "  - Farge" SKIP
          "  - øvrig vital info?"
          VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lKontroll AS LOGICAL.
      IF NOT lKontroll THEN
          RETURN.
  END.
  RUN ModellFarge.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Next
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Next C-ArtKort
ON CHOOSE OF BUTTON-Next IN FRAME DEFAULT-FRAME /* Neste */
DO:
  /* Det er ikke lov å bytte artikkel hvis artikkelen ikke har en ArtPrisPost. */
  IF VALID-HANDLE(hModell) THEN DO:
      RUN EndreKlar IN hModell.
      RETURN.
  END.
  DO:
      FIND ArtBas NO-LOCK WHERE
          RECID(ArtBas) = wArtBasRecid NO-ERROR.
      IF AVAILABLE ArtBas THEN
      DO:
          IF NOT CAN-FIND(FIRST ArtPris OF ArtBas) THEN
          DO:
              MESSAGE "Det må legges opp en kalkyle på denne artikkelen ("
                       ArtBAs.Vg ArtBAs.LopNr ")."
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
              RUN TvangKalkyle.
              RETURN "AVBRYT".
          END.
      END.
  END.
  IF VALID-HANDLE(hParentHandle) THEN DO:
      RUN PrevNext IN hParentHandle ("Next").
  END.
  ELSE
      RUN BUTTON-Next.
  {swn.i}    
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
  DEF VAR iOldArtSlag AS INT NO-UNDO.
  DEF VAR bDispStrType AS LOG INITIAL TRUE NO-UNDO.

  IF VALID-HANDLE(hModell) THEN DO:
      RUN EndreKlar IN hModell.
      RETURN NO-APPLY.
  END.

  IF NOT SELF:SENSITIVE THEN
      RETURN NO-APPLY.
  
  /* Opprettelse med forenklet artikkelkort. For servicehandelen. */
  IF CAN-DO('1,J,Ja,Yes,true',cServicehandel) OR CAN-DO('2',cServicehandel) THEN 
  OPPRETTELSE_I_DIALOG:
  DO:
    IF bFraKopier THEN 
    DO:
        ASSIGN
          iArtSlag = INT(ArtBas.ArtSlag:SCREEN-VALUE)
          NO-ERROR. 
    END.
    ELSE DO:
        RUN gartslag.w.
        ASSIGN
          iArtSlag = INT(RETURN-VALUE)
          NO-ERROR. 
    END.

    IF ERROR-STATUS:ERROR OR RETURN-VALUE = "AVBRYT" THEN
        RETURN "AVBRYT".
    IF NOT bFraKopier THEN lArtikkelNr = 0.
    IF CAN-DO('1,J,Ja,Yes,true',cServicehandel) THEN 
        DO:
            RUN dartbasny.w (iArtSlag, INPUT-OUTPUT lArtikkelNr).
            IF RETURN-VALUE <> '' THEN
                RETURN RETURN-VALUE.
        END.
    ELSE IF CAN-DO('2',cServicehandel) /* FAghandel */ THEN
        DO:
            bDispStrType = FALSE.
            IF bFraKopier THEN
                RUN ArtBasVedlikehold.w (THIS-PROCEDURE,"COPY",OUTPUT bOK).
            ELSE 
                RUN ArtBasVedlikehold.w (THIS-PROCEDURE,"NEW",OUTPUT bOK).
            IF bOk = FALSE THEN
                RETURN 'AVBRYT'.
            /*lArtikkelNr blir satt i NyArtBas fra ArtBasVedlikehold.w*/
        END.
    IF lArtikkelNr <> 0 THEN
      DO:
        ASSIGN wModus = "ENDRE".

        FIND ArtBas NO-LOCK WHERE 
            ArtBas.ArtikkelNr = lArtikkelNr NO-ERROR.    
        IF NOT AVAILABLE ArtBas THEN
           FIND LAST ArtBas NO-LOCK NO-ERROR.

        IF AVAILABLE ArtBas THEN
        DO:
          wAktivFlip = 1. 
          chTab1Side = chTabs:Item (wAktivFlip BY-VARIANT-POINTER).
          chTabStrip:SelectedItem = chTab1Side.
          ASSIGN
            wArtBasRecid = RECID(ArtBas).
          PUBLISH "ByttObjekt" (ArtBas.ArtikkelNr).
          RUN ByttFrame. /* For † enable knapper og felt korrekt */ 
          RUN RensFrame. /* Tar bort gammel d.... */
          RUN VisArtBas.
          IF VALID-HANDLE(wKalkyle) THEN RUN settDagensDato IN wKalkyle NO-ERROR.
          APPLY 'TAB' TO ArtBas.Vg.
        END.
      END.
  END. /* OPPRETTELSE_I_DIALOG */
  ELSE 
  OPPRETTELSE_I_ARTIKKELKORT:
  DO:
      RUN BUTTON-Ny.
      IF RETURN-VALUE = "AVBRYT" THEN
      DO:
          APPLY "CHOOSE" TO BUTTON-Angre.
          RETURN NO-APPLY.
      END.
      RUN RensFrame.
  
      ASSIGN
          ArtBas.Lager:CHECKED       = (IF CAN-DO("0,1,2,3,4,5,6",STRING(iArtSlag)) 
                                THEN TRUE  
                                ELSE FALSE)
          ArtBas.KjedeVare:CHECKED   = bHK
          ArtBas.Pant:CHECKED        = (IF CAN-DO("8",STRING(iArtSlag)) 
                                          THEN TRUE  
                                          ELSE FALSE)
          ArtBas.Pakke:CHECKED       = (IF CAN-DO("7",STRING(iArtSlag)) 
                                          THEN TRUE  
                                          ELSE FALSE)
          ArtBas.IKasse:CHECKED      = (IF CAN-DO("0,1,2,3,4,5,6,8",STRING(iArtSlag)) 
                                            THEN TRUE 
                                            ELSE FALSE)
          .

  END. /* OPPRETTELSE_I_ARTIKKELKORT */
  
  ASSIGN
    ArtBas.Utgatt:CHECKED            = FALSE
    ArtBas.ManueltOpprettet:CHECKED  = TRUE
    TB-WebButikkArtikkel:CHECKED = FALSE
    ArtBas.Gjennomfaktureres:CHECKED = FALSE
    ArtBas.Opris:CHECKED       = FALSE
    ArtBas.NON_Sale:CHECKED    = FALSE
    ArtBas.NegVare:CHECKED     = FALSE
    ArtBas.BildeIKasse:CHECKED = cBildeIKasse = "yes" /*  PARAMETERSTYRT */
    ArtBas.LevNr:SENSITIVE     = TRUE
    BUTTON-SokLev:SENSITIVE    = TRUE
    IndividType:SENSITIVE      = (IF CAN-DO("0",STRING(iArtSlag)) 
                                    THEN TRUE
                                    ELSE FALSE)
    ArtBas.HoyLavMva:CHECKED IN FRAME DEFAULT-FRAME  = FALSE
    ArtBas.Lager:SENSITIVE    = ArtBas.Pakke:CHECKED    = FALSE AND
                                ArtBas.Non_Sale:CHECKED = FALSE AND
                                ArtBas.OPris:CHECKED    = FALSE
    .
  DO WITH FRAME DEFAULT-FRAME:
      ASSIGN 
        ArtBas.HkStyrt:CHECKED     = FALSE    
        ArtBas.LokPris:CHECKED     = TRUE
        ArtBas.KjentPaHK:SENSITIVE = FALSE
        .
  END.

/*     T-LapTop     = IF wLapTop THEN TRUE ELSE FALSE. */
/*   DISPLAY                                           */
/*     T-LapTop                                        */
/*   WITH FRAME DEFAULT-FRAME.                         */

  DO WITH FRAME FRAME-ArtInfo:
    ASSIGN 
        ArtBas.Grunnsortiment:CHECKED = FALSE 
        ArtBas.Telefonkort:CHECKED    = FALSE 
        ArtBas.Alder:SCREEN-VALUE     = ""
        ArtBas.SalgsEnhet:screen-value = 'Stk'
        BUTTON-SokStrType:SENSITIVE   = TRUE
        ArtBas.STrTypeId:SENSITIVE    = (IF CAN-DO("0",STRING(iArtSlag)) 
                                        THEN TRUE
                                        ELSE FALSE)
        ArtBas.STrTypeId:screen-value = IF bDispStrType = TRUE THEN "2" ELSE STRING(ArtBas.StrTypeID)
        BUTTON-SokStrType:SENSITIVE   = ArtBas.STrTypeId:SENSITIVE
        ArtBas.LinkVareNr:SENSITIVE   = NOT ArtBas.Pant:CHECKED
        ArtBas.LinkVareAnt:SENSITIVE  = NOT ArtBas.Pant:CHECKED
        B-SokLinkVare:SENSITIVE       = NOT ArtBas.Pant:CHECKED
        ArtBas.Bonus_Givende:CHECKED = TRUE
        ArtBas.PubliserINettbutikk:CHECKED = FALSE  
        .
  END.
  DO WITH FRAME FRAME-Info2:
    ASSIGN
      ArtBas.WebButikkArtikkel:checked = FALSE
      .
  END.

  FIND ArtBas NO-LOCK WHERE
    RECID(ArtBas) = wArtBasRecid NO-ERROR.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-ArtInfo
&Scoped-define SELF-NAME BUTTON-NyLevsort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-NyLevsort C-ArtKort
ON CHOOSE OF BUTTON-NyLevsort IN FRAME FRAME-ArtInfo /* ... */
OR F10 OF CB-LevSort
DO:
    DEF VAR wLevSort AS RECID NO-UNDO.
    IF wModus = "NY" OR NOT AVAIL ArtBas OR ArtBas.ArtikkelNr = ArtBas.Vg OR ArtBas.StrTypeID < 2 THEN
        RETURN.
    RUN d-vLevSort.w (INPUT-OUTPUT wLevSort,"NY",RECID(LevBas),ArtBas.StrTypeID).
    IF RETURN-VALUE <> "Avbryt" AND wLevSort <> ? THEN
        RUN FillLevSortCB.
    APPLY "ENTRY" TO CB-LevSort.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-NyStrType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-NyStrType C-ArtKort
ON CHOOSE OF BUTTON-NyStrType IN FRAME FRAME-ArtInfo /* Ny */
DO:
  DO WITH FRAME FRAME-ArtInfo:
    wOk = FALSE.
    MESSAGE 'Skal artikkel/modell tildeles ny størrelsestype?' SKIP
            'Inngår artikkelen i en modell, vil størrelsestypen bli byttet også på de andre artikkelen i modellen.'
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE 'Bytte av størrelsestype' UPDATE wOk.
    IF wOk = FALSE THEN
        RETURN NO-APPLY.

    iStrTypeID = 0.
    /*RUN bibl_opprettStrtypeForModell.p (DEC(ArtBas.ArtikkelNr:SCREEN-VALUE IN FRAME Default-Frame), OUTPUT iStrTypeID).*/
    RUN setStorrelsestype.p (ArtBas.ArtikkelNr, 0, TRUE, OUTPUT iStrTypeId).

    IF iStrTypeID > 0 THEN 
    DO:
      ArtBas.StrTypeId:SCREEN-VALUE = STRING(iStrTypeID).
      APPLY "TAB" TO ArtBas.StrTypeId.
      RUN LagreArtBas (0).
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME BUTTON-Ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Ok C-ArtKort
ON CHOOSE OF BUTTON-Ok IN FRAME DEFAULT-FRAME /* Ok */
DO:
  IF CAN-FIND(FIRST tmpChild WHERE
               VALID-HANDLE(tmpChild.wChild)) THEN
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
  
  /* Lagre oppsett av VPI leverandør */
  RUN SaveToLokalIni ("EKSTVPILEV", CB-EkstVPILev:SCREEN-VALUE IN FRAME DEFAULT-FRAME).

  APPLY "CLOSE":U TO THIS-PROCEDURE.  
  RETURN wModus + "," + string(wArtBasRecid).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-Lager
&Scoped-define SELF-NAME BUTTON-Overfor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Overfor C-ArtKort
ON CHOOSE OF BUTTON-Overfor IN FRAME FRAME-Lager /* Overføring... */
DO:

    IF /*CAN-DO('7,8,9',STRING(iTransType)) AND*/ cBrukPwLagerTrans <> '0' THEN 
    DO:
      RUN d-adgangskontroll.w(INPUT 'Adgangskontroll',INPUT cPwd).
      IF RETURN-VALUE <> 'OK' THEN RETURN NO-APPLY.
    END.

    IF artbas.sanertdato <> ? THEN
        RETURN NO-APPLY.
    IF NOT CAN-FIND(FIRST StrTStr WHERE StrTStr.StrTypeID = artbas.StrtypeId) THEN DO:
        MESSAGE "Størrelsestypen mangler størrelser." SKIP
                "Kontakt systemadministratør."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN.
    END.
  IF AVAIL ArtBas AND (ArtBas.OPris = TRUE OR ArtBas.NON_Sale = TRUE) THEN DO:
      MESSAGE "Artikkel med åpen pris og NON-Sale artikler, kan ikke velges."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
  END.
  ELSE IF AVAIL ArtBas AND ArtBas.sanertdato <> ? THEN DO:
      MESSAGE "Artikkelen er sanert"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
  END.
  RUN Overfor.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME BUTTON-Paste
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Paste C-ArtKort
ON CHOOSE OF BUTTON-Paste IN FRAME DEFAULT-FRAME
DO:
    DEFINE VARIABLE OKpressed AS LOGICAL    NO-UNDO.
    DEFINE VARIABLE cFileName AS CHARACTER  NO-UNDO.
    
    RUN LagreArtBas(0).

    IF RETURN-VALUE <> "AVBRYT" THEN 
    NYBILDE: 
    DO:
        IF ArtBas.BildNr > 0 AND 
           CAN-FIND(BildeRegister WHERE BildeRegister.BildNr = INTEGER(ArtBas.BildNr)) AND 
            cBekreftNyttBilde <> "1" THEN 
        DO:
            MESSAGE "Artikkelen er allerede tildelt et bilde." SKIP
                    "Skal eksisterende bilde byttes ut."
                    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Bekreft"
                    UPDATE lBekreftNyttNr AS LOGICAL.
            IF lBekreftNyttNr = FALSE THEN
                LEAVE NYBILDE.
        END.
        
        /* Bruker har brukt kopier knappen og kopiert et bilde. */
        IF BUTTON-Kopierbilde:PRIVATE-DATA IN FRAME DEFAULT-FRAME <> ? THEN 
        DO:
            APPLY "CHOOSE" TO MENU-ITEM m_Lim_inn IN MENU POPUP-MENU-BUTTON-Kopierbilde.
            LEAVE NYBILDE.
        END.

        /* Bruker har trykket knappen for å åpne filbehandler og har valgt en fil. */
        ELSE IF SELF:PRIVATE-DATA = "SEARCH" THEN DO:
            ASSIGN SELF:PRIVATE-DATA = "".
            SYSTEM-DIALOG GET-FILE cFileName
                TITLE      "Velg bildefil ..."
                FILTERS    "Type (*.bmp,*.jpg)"   "*.bmp,*.jpg"
                INITIAL-DIR cSisteBildedir
                MUST-EXIST
                USE-FILENAME
                RETURN-TO-START-DIR
                UPDATE OKpressed.
            IF OKpressed = TRUE THEN DO:
                ASSIGN FILE-INFO:FILENAME = cFileName
                       cFileName = FILE-INFO:FULL-PATHNAME.
                IF ENTRY(NUM-ENTRIES(cFileName,"\"),cFileName,"\") BEGINS "mini" THEN DO:
                    MESSAGE "Fil med navn 'mini...' kann ikke leses inn."
                        VIEW-AS ALERT-BOX INFO BUTTONS OK.
                    LEAVE NYBILDE.
                END.
                ASSIGN cSisteBildeDir = REPLACE(cFileName,ENTRY(NUM-ENTRIES(cFileName,"\"),cFileName,"\"),"")
                       SELF:PRIVATE-DATA = cFilename.
            END.
            ELSE
                ASSIGN SELF:PRIVATE-DATA = "".
        END.

        /*IF SELF:PRIVATE-DATA <> "" THEN*/
        RUN NyttBilde (SELF:PRIVATE-DATA).

        RUN VisBilde(1).
    END. /* NYBILDE*/
    SELF:PRIVATE-DATA = "".
    RETURN NO-APPLY.
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
  IF VALID-HANDLE(hModell) THEN DO:
      RUN EndreKlar IN hModell.
      RETURN.
  END.
  DO:
      FIND ArtBas NO-LOCK WHERE
          RECID(ArtBas) = wArtBasRecid NO-ERROR.
      IF AVAILABLE ArtBas THEN
      DO:
          IF NOT CAN-FIND(FIRST ArtPris OF ArtBas) THEN
          DO:
              MESSAGE "Det må legges opp en kalkyle på denne artikkelen ("
                       ArtBAs.Vg ArtBAs.LopNr ")."
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
              RUN TvangKalkyle.
              RETURN "AVBRYT".
          END.
      END.
  END.
  IF VALID-HANDLE(hParentHandle) THEN DO:
      RUN PrevNext IN hParentHandle ("Prev").
  END.
  ELSE
      RUN BUTTON-Prev.
  {swn.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SettLopNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SettLopNr C-ArtKort
ON CHOOSE OF BUTTON-SettLopNr IN FRAME DEFAULT-FRAME /* Sett løpenr i varegruppen... */
DO:
  RUN SettLopenummer.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Slett
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Slett C-ArtKort
ON CHOOSE OF BUTTON-Slett IN FRAME DEFAULT-FRAME /* Radera */
DO:
  DEF VAR lOk AS LOG INITIAL FALSE NO-UNDO.
  IF VALID-HANDLE(hModell) THEN DO:
      RUN EndreKlar IN hModell.
      RETURN NO-APPLY.
  END.

  IF NOT SELF:SENSITIVE THEN
      RETURN NO-APPLY.
  IF NOT AVAILABLE ArtBAs THEN
      RETURN NO-APPLY.


  dSletteDato = TODAY - iAntDarSlett.
  bOk = FALSE.

  IF ArtBas.ArtikkelNr = ArtBas.Vg THEN 
  DO:
    MESSAGE "Åpen pris artikkel, skal normalt ikke slettes." SKIP
            "Bekreft at den virkelig skal slettes."
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE 'Bekreft sletting av artikkel'
        UPDATE bOk.
    IF bOk = FALSE THEN
        RETURN NO-APPLY.
  END.

  bOk = FALSE.
  IF CAN-FIND(FIRST VareBokLinje WHERE
              VareBokLinje.ArtikkelNr = ArtBas.ArtikkelNr) THEN 
  DO:
    MESSAGE "Artikkelen ligger i en eller flere varebøker." SKIP
            "Bekreft at den virkelig skal slettes."
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE 'Bekreft sletting av artikkel'
        UPDATE bOk.
    IF bOk = FALSE THEN
        RETURN NO-APPLY.
  END.

  bOk = FALSE.
  IF CAN-FIND(VareBehLinje WHERE
      VareBehLinje.ArtikkelNr = ArtBas.ArtikkelNr) THEN 
  DO:
    MESSAGE "Artikkelen ligger i en eller flere varehåndteringsbøker." SKIP
            "Bekreft at den virkelig skal slettes."
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE 'Bekreft sletting av artikkel'
        UPDATE bOk.
    IF bOk = FALSE THEN
        RETURN NO-APPLY.
  END.


  RUN BUTTON-Slett.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Slettbilde
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Slettbilde C-ArtKort
ON CHOOSE OF BUTTON-Slettbilde IN FRAME DEFAULT-FRAME
DO:
    DEFINE VARIABLE iSlettBildnr AS INTEGER    NO-UNDO.
    IF artbas.bildnr <> 0 THEN DO:
        MESSAGE "Skal bilde slettes"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lSlett AS LOG.
        IF lSlett = TRUE THEN DO:
            RUN LagreArtBas(0).
            IF RETURN-VALUE <> "AVBRYT" THEN SLETTBILDE: DO:
                IF artbas.bildnr = artbas.artikkelnr THEN
                    iSlettBildnr = artbas.bildnr.
                FIND CURRENT Artbas EXCLUSIVE NO-ERROR.
                IF AVAIL ArtBas THEN DO:
                    artbas.bildnr = 0.
                    FIND CURRENT Artbas NO-LOCK.
                    IF iSlettBildnr <> 0 THEN
                        RUN SlettBilde (iSlettBildnr).
                END.
                IF BUTTON-Kopierbilde:PRIVATE-DATA = STRING(iSlettBildnr) THEN DO:
                    BUTTON-Kopierbilde:PRIVATE-DATA = ?.
                    MENU-ITEM m_Kopier:SENSITIVE IN MENU POPUP-MENU-BUTTON-Kopierbilde       = FALSE.
                    MENU-ITEM m_Lim_inn:SENSITIVE IN MENU POPUP-MENU-BUTTON-Kopierbilde      = FALSE.
                    MENU-ITEM m_Angre_kopier:SENSITIVE IN MENU POPUP-MENU-BUTTON-Kopierbilde = FALSE.
                END.
                RUN VisArtBas. 
                RUN VisEndretinfo.
                RUN Toolbar.
            END.
        END.
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-ArtInfo
&Scoped-define SELF-NAME BUTTON-SokBehKode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokBehKode C-ArtKort
ON CHOOSE OF BUTTON-SokBehKode IN FRAME FRAME-ArtInfo /* ... */
OR F10 OF ArtBas.BehKode
DO:
  RUN SokBehKode.
  APPLY "ENTRY" TO ArtBas.BehKode.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokBruk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokBruk C-ArtKort
ON CHOOSE OF BUTTON-SokBruk IN FRAME FRAME-ArtInfo /* ... */
OR F10 OF ArtBas.Anv-Id
DO:
  RUN SokAnvId.
  APPLY "ENTRY" TO ArtBas.Anv-Id.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokFarge
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokFarge C-ArtKort
ON CHOOSE OF BUTTON-SokFarge IN FRAME FRAME-ArtInfo /* ... */
OR F10 OF ArtBas.Farg
DO:
  RUN SokFarg.
  APPLY "ENTRY" TO ArtBas.Farg.
  RETURN NO-APPLY.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME BUTTON-SokFil
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokFil C-ArtKort
ON CHOOSE OF BUTTON-SokFil IN FRAME DEFAULT-FRAME
DO:
    ASSIGN BUTTON-Kopierbilde:PRIVATE-DATA  = ?
           BUTTON-Paste:PRIVATE-DATA = "SEARCH".
    APPLY "CHOOSE" TO BUTTON-Paste.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-ArtInfo
&Scoped-define SELF-NAME BUTTON-SokHovedKategori
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokHovedKategori C-ArtKort
ON CHOOSE OF BUTTON-SokHovedKategori IN FRAME FRAME-ArtInfo /* ... */
OR F10 OF ArtBas.HovedKatNr 
DO:
  DO WITH FRAME FRAME-ArtInfo:
      cTekst = "HovedKatNr".
    /*  RUN JBoxDLookup.w ("HovedKategori;HovedKatTekst;HovedKatNr|H.Kategori|>9", "where true", INPUT-OUTPUT cTekst). */
    THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
    RUN JBoxLookup.w (THIS-PROCEDURE,50,
                      "HovedKategori;HovedKatTekst;HovedKatNr|H.Kategori|>9"
                     ,"WHERE TRUE"
                      ,""                                                  
                      ,"HovedKatNr",   /* <- return values for these fields */
                        OUTPUT cTekst,
                        OUTPUT bOK).
    THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.
      IF NOT bOK THEN
          RETURN NO-APPLY.
      FIND Hovedkategori NO-LOCK WHERE
        HovedKategori.HovedKatNr = INT(cTekst) NO-ERROR.
      IF AVAILABLE Hovedkategori THEN
      DO:
          /* Legger opp verdier I de aktuelle feltene */
          ASSIGN
            ArtBas.HovedKatNr:SCREEN-VALUE   = cTekst
            HovedKategori.HovedKatTekst:SCREEN-VALUE = HovedKategori.HovedKatTekst
            .
      END.
      ELSE DO:
          /* Legger opp verdier I de aktuelle feltene */
          ASSIGN
            ArtBas.HovedKatNr:SCREEN-VALUE    = ''
            HovedKategori.HovedKatTekst:SCREEN-VALUE  = ''
            .
      END.
      APPLY "ENTRY" TO ArtBas.HovedKatNr.
      RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokInner
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokInner C-ArtKort
ON CHOOSE OF BUTTON-SokInner IN FRAME FRAME-ArtInfo /* ... */
OR F10 OF ArtBas.Inner-Id
DO:
  RUN SokInnerId.
  APPLY "ENTRY" TO ArtBas.Inner-Id.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokKlak
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokKlak C-ArtKort
ON CHOOSE OF BUTTON-SokKlak IN FRAME FRAME-ArtInfo /* ... */
OR F10 OF ArtBas.Klack
DO:
  RUN SokKlack.
  APPLY "ENTRY" TO ArtBas.Klack.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokLalst
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokLalst C-ArtKort
ON CHOOSE OF BUTTON-SokLalst IN FRAME FRAME-ArtInfo /* ... */
OR F10 OF ArtBas.Last-Id
DO:
  RUN SokLastId.
  APPLY "ENTRY" TO ArtBas.Last-Id.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-Info2
&Scoped-define SELF-NAME BUTTON-SokLandKode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokLandKode C-ArtKort
ON CHOOSE OF BUTTON-SokLandKode IN FRAME FRAME-Info2 /* ... */
OR F10 OF ArtBas.AlfaKode2
DO:
  DO WITH FRAME FRAME-Info2:
      cTekst = "AlfaKode2".
    THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
    RUN JBoxLookup.w (THIS-PROCEDURE,50,
                      "AlfaLandKode;AlfaKode2;AlfaKode3,NumLandKode;Land"
                     ,"WHERE TRUE,FIRST NumLandKode OF AlfaLandKode NO-LOCK"
                      ,""                                                  
                      ,"AlfaKode2",   /* <- return values for these fields */
                        OUTPUT cTekst,
                        OUTPUT bOK).
    THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

      IF RETURN-VALUE = "AVBRYT" THEN
          RETURN NO-APPLY.
      FIND FIRST AlfaLandKode NO-LOCK WHERE
        AlfaLandKode.AlfaKode2 = cTekst NO-ERROR.
      IF AVAILABLE AlfaLandKode THEN
      DO:
          FIND NumLandKode OF AlfaLandKode NO-LOCK NO-ERROR.
          /* Legger opp verdier I de aktuelle feltene */
          ASSIGN
            ArtBas.AlfaKode2:SCREEN-VALUE   = cTekst
            FI-Land:SCREEN-VALUE            = IF AVAILABLE NumLandKode 
                                                THEN NumLandKode.Land 
                                                ELSE ''
            .
      END.
      ELSE DO:
          /* Legger opp verdier I de aktuelle feltene */
          ASSIGN
            ArtBas.AlfaKode2:SCREEN-VALUE    = ''
            FI-Land:SCREEN-VALUE = ""
            .
      END.
      APPLY "ENTRY" TO ArtBas.AlfaKode2.
      RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME BUTTON-SokLev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokLev C-ArtKort
ON CHOOSE OF BUTTON-SokLev IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF ArtBas.LevNr
DO:
    IF wModus <> "NY" AND AVAIL ArtBas AND ArtBas.ArtikkelNr = ArtBas.Vg THEN DO:
        MESSAGE "PLU-artikkel, endring avbrutt."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.
    RUN SokLevNr.
    APPLY "ENTRY" TO ArtBas.LevNr.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-Info2
&Scoped-define SELF-NAME BUTTON-SokLevDat-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokLevDat-2 C-ArtKort
ON CHOOSE OF BUTTON-SokLevDat-2 IN FRAME FRAME-Info2 /* ... */
OR F10 OF ArtBas.LevDato1
DO:
  /* Start søkeprogram */
  {soek.i
    &Felt        = ArtBas.LevDato1
    &Program     = kalender.w
    &Frame       = FRAME-Info2
    &ExtraParam  = "Tx('Datosøk',136)"
  }   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokLevDat-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokLevDat-3 C-ArtKort
ON CHOOSE OF BUTTON-SokLevDat-3 IN FRAME FRAME-Info2 /* ... */
OR F10 OF ArtBas.TilgjengeligFraLev
DO:
  /* Start søkeprogram */
  {soek.i
    &Felt        = ArtBas.TilgjengeligFraLev
    &Program     = kalender.w
    &Frame       = FRAME-Info2
    &ExtraParam  = "Tx('Datosøk',136)"
  }   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokLevDat-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokLevDat-4 C-ArtKort
ON CHOOSE OF BUTTON-SokLevDat-4 IN FRAME FRAME-Info2 /* ... */
OR F10 OF ArtBas.VPIDato
DO:
  /* Start søkeprogram */
  {soek.i
    &Felt        = ArtBas.VPIDato
    &Program     = kalender.w
    &Frame       = FRAME-Info2
    &ExtraParam  = "Tx('Datosøk',136)"
  }   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokLevDat-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokLevDat-5 C-ArtKort
ON CHOOSE OF BUTTON-SokLevDat-5 IN FRAME FRAME-Info2 /* ... */
OR F10 OF ArtBas.LevDatoStopp1
DO:
  /* Start søkeprogram */
  {soek.i
    &Felt        = ArtBas.LevDatoStopp1
    &Program     = kalender.w
    &Frame       = FRAME-Info2
    &ExtraParam  = "Tx('Datosøk',136)"
  }   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokLevDat-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokLevDat-6 C-ArtKort
ON CHOOSE OF BUTTON-SokLevDat-6 IN FRAME FRAME-Info2 /* ... */
OR F10 OF ArtBas.LevDatoStopp2
DO:
  /* Start søkeprogram */
  {soek.i
    &Felt        = ArtBas.LevDatoStopp2
    &Program     = kalender.w
    &Frame       = FRAME-Info2
    &ExtraParam  = "Tx('Datosøk',136)"
  }   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokLevDat-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokLevDat-7 C-ArtKort
ON CHOOSE OF BUTTON-SokLevDat-7 IN FRAME FRAME-Info2 /* ... */
OR F10 OF ArtBas.LevDatoStopp3
DO:
  /* Start søkeprogram */
  {soek.i
    &Felt        = ArtBas.LevDatoStopp3
    &Program     = kalender.w
    &Frame       = FRAME-Info2
    &ExtraParam  = "Tx('Datosøk',136)"
  }   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokLevDat-8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokLevDat-8 C-ArtKort
ON CHOOSE OF BUTTON-SokLevDat-8 IN FRAME FRAME-Info2 /* ... */
OR F10 OF ArtBas.LevDatoStopp4
DO:
  /* Start søkeprogram */
  {soek.i
    &Felt        = ArtBas.LevDatoStopp4
    &Program     = kalender.w
    &Frame       = FRAME-Info2
    &ExtraParam  = "Tx('Datosøk',136)"
  }   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokLevDato-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokLevDato-3 C-ArtKort
ON CHOOSE OF BUTTON-SokLevDato-3 IN FRAME FRAME-Info2 /* ... */
OR F10 OF ArtBas.LevDato2
DO:
  /* Start søkeprogram */
  {soek.i
    &Felt        = ArtBas.LevDato2
    &Program     = kalender.w
    &Frame       = "FRAME-Info2"
    &ExtraParam  = "Tx('Datosøk',136)"
  }   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokLevDato-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokLevDato-4 C-ArtKort
ON CHOOSE OF BUTTON-SokLevDato-4 IN FRAME FRAME-Info2 /* ... */
OR F10 OF ArtBas.LevDato3
DO:
  /* Start søkeprogram */
  {soek.i
    &Felt        = ArtBas.LevDato3
    &Program     = kalender.w
    &Frame       = "FRAME-Info2"
    &ExtraParam  = "Tx('Datosøk',136)"
  }   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokLevDato-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokLevDato-5 C-ArtKort
ON CHOOSE OF BUTTON-SokLevDato-5 IN FRAME FRAME-Info2 /* ... */
OR F10 OF ArtBas.LevDato4
DO:
  /* Start søkeprogram */
  {soek.i
    &Felt        = ArtBas.LevDato4
    &Program     = kalender.w
    &Frame       = "FRAME-Info2"
    &ExtraParam  = "Tx('Datosøk',136)"
  }   
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
  APPLY "ENTRY" TO ArtBas.MatKod.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokOver
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokOver C-ArtKort
ON CHOOSE OF BUTTON-SokOver IN FRAME FRAME-ArtInfo /* ... */
OR F10 OF ArtBas.Ov-Id
DO:
  RUN SokOverdel.
  APPLY "ENTRY" TO ArtBas.Ov-Id.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokProdusent
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokProdusent C-ArtKort
ON CHOOSE OF BUTTON-SokProdusent IN FRAME FRAME-ArtInfo /* ... */
OR F10 OF ArtBas.ProdNr
DO:
  DO WITH FRAME FRAME-ArtInfo:
      cTekst = "ProdNr".
    /*  RUN JBoxDLookup.w ("Produsent;Beskrivelse;ProdNr|Prodnr|>>>>>9", "where true", INPUT-OUTPUT cTekst).*/
    THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
    RUN JBoxLookup.w (THIS-PROCEDURE,50,
                      "Produsent;Beskrivelse;ProdNr|Prodnr|>>>>>9"
                     ,"WHERE TRUE"
                      ,""                                                  
                      ,"ProdNr",   /* <- return values for these fields */
                        OUTPUT cTekst,
                        OUTPUT bOK).
    THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

      IF RETURN-VALUE = "AVBRYT" THEN
          RETURN NO-APPLY.
      FIND Produsent NO-LOCK WHERE
        Produsent.ProdNr = INT(cTekst) NO-ERROR.
      IF AVAILABLE Produsent THEN
      DO:
          /* Legger opp verdier I de aktuelle feltene */
          ASSIGN
            ArtBas.ProdNr:SCREEN-VALUE   = cTekst
            Produsent.Beskrivelse:SCREEN-VALUE = Produsent.Beskrivelse
            .
      END.
      ELSE DO:
          /* Legger opp verdier I de aktuelle feltene */
          ASSIGN
            ArtBas.ProdNr:SCREEN-VALUE    = ''
            Produsent.Beskrivelse:SCREEN-VALUE  = ''
            .
      END.
      APPLY "ENTRY" TO ArtBas.ProdNr.
      RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokRegnAvd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokRegnAvd C-ArtKort
ON CHOOSE OF BUTTON-SokRegnAvd IN FRAME FRAME-ArtInfo /* ... */
OR F10 OF ArtBas.RAvdNr
DO:
  DO WITH FRAME Art-Info:
      DEF VAR cLookupValue AS CHAR NO-UNDO.

      cLookupValue = "RAvdNr".

      /*RUN JBoxDLookup.w ("Regnskapsavdeling;RAvdBeskrivelse;RAvdNr", "where true", INPUT-OUTPUT cLookupValue).*/
      THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
      RUN JBoxLookup.w (THIS-PROCEDURE,50,
                        "Regnskapsavdeling;RAvdBeskrivelse;RAvdNr"
                       ,"WHERE TRUE"
                        ,""                                                  
                        ,"RAvdNr",   /* <- return values for these fields */
                          OUTPUT cTekst,
                          OUTPUT bOK).
      THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

      IF cTekst NE "" THEN DO:
        ArtBas.RAvdNr:SCREEN-VALUE = cTekst.
        FIND Regnskapsavdeling NO-LOCK WHERE
            Regnskapsavdeling.RAvdNr = INPUT ArtBas.RAvdNr NO-ERROR.
        IF AVAILABLE Regnskapsavdeling THEN
            Regnskapsavdeling.RAvdBeskrivelse:screen-value = RegnskapsAvdeling.RAvdBeskrivelse.
        ELSE
            Regnskapsavdeling.RAvdBeskrivelse:SCREEN-VALUE = "".

      END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokSesong
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokSesong C-ArtKort
ON CHOOSE OF BUTTON-SokSesong IN FRAME FRAME-ArtInfo /* ... */
OR F10 OF ArtBas.SaSong
DO:
  RUN SokSaSong.
  APPLY "ENTRY" TO ArtBas.SaSong.
  RETURN NO-APPLY.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokSlit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokSlit C-ArtKort
ON CHOOSE OF BUTTON-SokSlit IN FRAME FRAME-ArtInfo /* ... */
OR F10 OF ArtBas.Slit-Id
DO:
  RUN SokSlitId.
  APPLY "ENTRY" TO ArtBas.Slit-Id.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokStrType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokStrType C-ArtKort
ON CHOOSE OF BUTTON-SokStrType IN FRAME FRAME-ArtInfo /* ... */
OR F10 OF ArtBas.StrTypeId DO:
DO WITH FRAME FRAME-ArtInfo:
    IF wModus <> "NY" AND ArtBas.ArtikkelNr = ArtBas.Vg THEN DO:
        MESSAGE "PLUartikkel, endring ikke tillatt."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.
    DO WITH FRAME DEFAULT-FRAME:
        ASSIGN iVg = INPUT ArtBas.vg.
        IF iVg = 0 THEN DO:
            MESSAGE "Registrer varegruppe"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RETURN NO-APPLY.
        END.
        FIND VarGr WHERE VarGr.Vg = iVg NO-LOCK NO-ERROR.
        IF NOT AVAIL VarGr THEN DO:
            MESSAGE "Feil varegruppe!"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RETURN NO-APPLY.
        END.
    END.
     FIND HuvGr OF VarGr NO-LOCK NO-ERROR.
     ASSIGN iStrTypeID = INPUT ArtBas.StrTypeID.
     /* Kaller søkerutine */
     RUN gstrtype.w (
       INPUT-OUTPUT cTekst, /* Returstreng - chr(1) separert */
       "", /* Feltliste avgrensningsfelt (kommaseparert) */
       "", /* Feltverdier (chr(1) sep) */ 
       ArtBas.StrTypeId:SCREEN-VALUE, /* Post markøren skal stå på */
       (IF AVAILABLE HuvGr
          THEN HuvGr.AvdelingNr
          ELSE 0),
       (IF AVAILABLE HuvGr AND lBrukHgiFilter
           THEN HuvGr.Hg
           ELSE 0),
       "" /* where sats ex. strtypeid > 2 */
       ).
     IF NUM-ENTRIES(RETURN-VALUE) >= 2 THEN
         ASSIGN
         iStrTypeId = INT(ENTRY(2,RETURN-VALUE))
         NO-ERROR.
     IF ERROR-STATUS:ERROR THEN
         ASSIGN iStrTypeID = INPUT ArtBas.StrTypeID.
     IF CAN-DO("ENDRE-STRTYPE,NY-STRTYPE",ENTRY(1,RETURN-VALUE)) THEN
     DO:
         iStrTypeId = (IF ENTRY(1,RETURN-VALUE) = "NY-STRTYPE"
                         THEN ?
                         ELSE iStrTypeId).
         RUN d-strtype.w (INPUT-OUTPUT iStrTypeId,
                          (IF AVAILABLE HuvGr
                           THEN HuvGr.AvdelingNr
                           ELSE 0),
                         (IF AVAILABLE HuvGr AND lBrukHgiFilter
                           THEN ArtBas.Hg
                           ELSE 0)).
         IF ENTRY(1,RETURN-VALUE) = "AVBRYT" THEN 
         DO:
             iStrTypeId = ArtBas.StrTypeId.
             IF wModus = "NY" THEN
                 APPLY "ENTRY" TO ArtBas.StrTypeId.
             ELSE 
                 ASSIGN ArtBas.StrTypeId:SCREEN-VALUE = STRING(ArtBas.StrTypeId).
             RETURN NO-APPLY.
         END.
         ArtBas.StrTypeId:SCREEN-VALUE = STRING(iStrTypeID).
         FIND StrType WHERE StrType.StrTypeId = iStrTypeID NO-LOCK NO-ERROR.
         APPLY "TAB" TO ArtBas.StrTypeId.
     END.
     ELSE DO:
         IF ENTRY(1,RETURN-VALUE) = "AVBRYT" THEN 
         DO:
             IF wModus = "NY" THEN
                 APPLY "ENTRY" TO ArtBas.StrTypeId.
             ELSE 
                 ASSIGN ArtBas.StrTypeId:SCREEN-VALUE = STRING(ArtBas.StrTypeId).
             RETURN NO-APPLY.
         END.
         ELSE IF INT(ENTRY(2,cTekst,CHR(1))) = 1 THEN DO:
             MESSAGE "Feil størrelsestype!." SKIP
                     "Velg en størrelsestype som har størrelser (Ikke størrelsestype = 1)."
                 VIEW-AS ALERT-BOX INFO BUTTONS OK.
             APPLY "ENTRY" TO ArtBas.StrTypeID.
             RETURN NO-APPLY.
         END.
         ELSE DO:
             IF NUM-ENTRIES(cTekst,CHR(1)) >= 3 THEN
             DO:
                 /* Legger opp verdier I de aktuelle feltene */
                 ASSIGN
                   iStrTypeId = INT(ENTRY(2,cTekst,CHR(1)))
                   ArtBas.StrTypeId:SCREEN-VALUE  = ENTRY(2,cTekst,CHR(1))
    /*   SKER I TAB             StrType.Beskrivelse:SCREEN-VALUE = ENTRY(3,cTekst,CHR(1)) */
                   .
             END.

             FIND StrType WHERE StrType.StrTypeId = iStrTypeID NO-LOCK NO-ERROR.
             APPLY "TAB" TO ArtBas.StrTypeId.
         END.
     END.
     RETURN NO-APPLY.
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokValuta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokValuta C-ArtKort
ON CHOOSE OF BUTTON-SokValuta IN FRAME FRAME-ArtInfo /* ... */
OR F10 OF ArtBas.ValKod
DO:
  RUN SokValkod.
  APPLY "ENTRY" TO ArtBas.ValKod.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokVaremerke
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokVaremerke C-ArtKort
ON CHOOSE OF BUTTON-SokVaremerke IN FRAME FRAME-ArtInfo /* ... */
OR F10 OF ArtBas.VMId
DO:
  DO WITH FRAME FRAME-ArtInfo:
      cTekst = "VmId".
      /*RUN JBoxDLookup.w ("Varemerke;Beskrivelse;KortNavn;VmId|Varemerke|>>>>>9", "where true", INPUT-OUTPUT cTekst).*/
      THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
      RUN JBoxLookup.w (THIS-PROCEDURE,50,
                        "Varemerke;Beskrivelse;KortNavn;VmId|Varemerke|>>>>>9"
                       ,"WHERE TRUE"
                        ,""                                                  
                        ,"VmId",   /* <- return values for these fields */
                          OUTPUT cTekst,
                          OUTPUT bOK).
      THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

      IF RETURN-VALUE = "AVBRYT" THEN
          RETURN NO-APPLY.
      FIND Varemerke NO-LOCK WHERE
        Varemerke.VmId = INT(cTekst) NO-ERROR.
      IF AVAILABLE Varemerke THEN
      DO:
          /* Legger opp verdier I de aktuelle feltene */
          ASSIGN
            ArtBas.VmId:SCREEN-VALUE   = cTekst
            Varemerke.Beskrivelse:SCREEN-VALUE = Varemerke.Beskrivelse
            .
      END.
      ELSE DO:
          /* Legger opp verdier I de aktuelle feltene */
          ASSIGN
            ArtBas.VmId:SCREEN-VALUE    = ''
            Varemerke.Beskrivelse:SCREEN-VALUE  = ''
            .
      END.
      APPLY "ENTRY" TO ArtBas.VmId.
      RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME BUTTON-SokVg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokVg C-ArtKort
ON CHOOSE OF BUTTON-SokVg IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF ArtBas.Vg
DO:
  IF NOT wModus = "NY" AND ArtBas.ArtikkelNr = ArtBas.Vg THEN DO:
      MESSAGE "Varen er varegruppevare, bytte ikke tillatt."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
  END.
  /*
  ELSE IF NOT wModus = "NY" AND ArtBas.ModellFarge <> 0 THEN DO:
      MESSAGE "Artikkel ingår i modell/farge, bytte ikke tillatt."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
  END.
  */
  ELSE IF NOT wModus =  "NY" AND (CAN-FIND(FIRST StLinje WHERE StLinje.DataObjekt = STRING(ArtBas.Artikkelnr,"9999999999999") AND
            StLinje.StTypeId = "ARTIKKEL") OR 
            CAN-FIND(FIRST TRANSLOGG WHERE TransLogg.ArtikkelNr = ArtBas.ArtikkelNr) OR 
            CAN-FIND(FIRST ArtLag WHERE ArtLag.ArtikkelNr = ArtBas.ArtikkelNr)) THEN DO:
        
        MESSAGE "Statistikk/traksaksjoner/lager finnes." SKIP
                "1. Bytte av varegruppe på en artikkel, medfører at den også tildeles nytt løpenummer." SKIP
                "2. Bytte av varegruppe på en artikkel som det er oppdatert statistikk på, vil kunne medføre " SKIP
                "avvik i akkumulerte statistikker (Avd, hg og vg) når disse sammenlignes." SKIP
                "Ønsker du alikevel å bytte varegruppe på artikkelen?" 
            VIEW-AS ALERT-BOX INFO BUTTONS YES-NO UPDATE bOk.
        IF NOT bOk THEN
                RETURN NO-APPLY.
        RUN ByttVg.
    END.
  ELSE IF NOT wModus =  "NY" AND ArtBas.LopNr <> ? THEN 
  DO:
        
        MESSAGE "Artikkel er tildelt løpenummer." SKIP
                "1. Bytte av varegruppe på en artikkel, medfører at den også tildeles nytt løpenummer." SKIP
                "Ønsker du alikevel å bytte varegruppe på artikkelen?" 
            VIEW-AS ALERT-BOX INFO BUTTONS YES-NO UPDATE bOk.
        IF NOT bOk THEN
                RETURN NO-APPLY.
        RUN ByttVg.
    END.
  ELSE DO: 
      RUN SokVg.
      IF int(ArtBas.Vg:screen-value) > 0 THEN 
          APPLY 'TAB' TO ArtBas.Vg.
      APPLY 'ENTRY' TO ArtBas.Vg.
      RETURN NO-APPLY.
  END.
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


&Scoped-define SELF-NAME BUTTON-TilKasse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-TilKasse C-ArtKort
ON CHOOSE OF BUTTON-TilKasse IN FRAME DEFAULT-FRAME /* Send til kasse... */
DO:
    IF Artbas.sanertdato <> ? THEN DO:
        MESSAGE "Artikkelen er sanert"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

    END.
    ELSE DO:
        RUN skapaELogg IN THIS-PROCEDURE.
        
        MESSAGE 'Artikkelen er markert som endret, og vil bli lagt ut til kassene.' SKIP
                'Både artikkelinformasjon og gjeldende prisinformasjon vil bli lagt ut'
                VIEW-AS ALERT-BOX INFO.
    END.    
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-ArtInfo
&Scoped-define SELF-NAME BUTTON-Underkategori
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Underkategori C-ArtKort
ON CHOOSE OF BUTTON-Underkategori IN FRAME FRAME-ArtInfo /* ... */
OR F10 OF S-Underkategori
DO:
  DEF VAR cUnderkategoriRowIdLst AS CHAR NO-UNDO.
  DEF VAR cUnderKategoriIdLst AS CHAR NO-UNDO.
  IF NOT AVAILABLE ArtBas THEN
      RETURN.

  IF wModus <> "NY" AND AVAIL ArtBas AND ArtBas.ArtikkelNr = ArtBas.Vg THEN DO:
      MESSAGE "PLU-artikkel, endring avbrutt."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
  END.


  cUnderkategoriRowIdLst = "".
  FOR EACH ArtBasUnderkategori NO-LOCK WHERE
      ArtBasUnderkategori.ArtikkelNr = ArtBas.ArtikkelNr:
      FIND Underkategori OF ArtBasUnderkategori NO-ERROR.
      IF AVAILABLE Underkategori THEN
      ASSIGN
          cUnderkategoriRowIdLst = cUnderkategoriRowIdLst + 
                             (IF cUnderkategoriRowIdLst <> ""
                                THEN ","
                                ELSE "") + 
                              STRING(ROWID(Underkategori))
          .

  END.
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "Underkategori;UnderKatNr;UnderKatTekst",
                      "where true",
                      INPUT-OUTPUT cUnderkategoriRowIdLst,
                      "UnderKatNr",
                      INPUT-OUTPUT cUnderKategoriIdLst, 
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk THEN 
  DO:
      setUnderkategori (S-Underkategori:HANDLE,cUnderkategoriRowIdLst).
      RUN LagreArtBas (0).
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-Mellankategori
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Mellankategori C-ArtKort
ON VALUE-CHANGED OF CB-Mellankategori IN FRAME FRAME-ArtInfo /* Mellankat */
DO:
    initMellanUkat(INT(CB-Mellankategori:SCREEN-VALUE IN FRAME FRAME-ArtInfo),0).
/*     RETURN NO-APPLY. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME CB-ModellFarge
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-ModellFarge C-ArtKort
ON VALUE-CHANGED OF CB-ModellFarge IN FRAME DEFAULT-FRAME /* Modell */
DO:
  RUN HentVis(?,DECIMAL(CB-ModellFarge:SCREEN-VALUE)).
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
/*   MESSAGE chTabStrip:SelectedItem:KEY    */
/*       VIEW-AS ALERT-BOX INFO BUTTONS OK. */
  DEFINE VARIABLE cKey AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iIndex AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iTag AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cTag AS CHARACTER  NO-UNDO.

  cTag = ENTRY(wAktivFlip,cAktivFlipTag).
  iTag = INT(SUBSTRING(cTag,2)).
  IF wAktivFlip <> INT(chTabStrip:SelectedItem:Index) THEN
  DO:
      IF CAN-DO("4,5,10,11",STRING(iTag)) THEN DO:
/*           MESSAGE "click" SKIP iTag SKIP chTabStrip:SelectedItem:Index */
/*               VIEW-AS ALERT-BOX INFO BUTTONS OK.                       */
          CASE iTag:
              WHEN 4 THEN IF VALID-HANDLE(hStrekkode) THEN
                  RUN SlettTmpChild IN hStrekkode NO-ERROR.
              WHEN 5 THEN IF VALID-HANDLE(hPakkelinje) THEN
                  RUN SlettTmpChild IN hPakkelinje NO-ERROR.
              WHEN 10 THEN IF VALID-HANDLE(hTranslogg) THEN
                  RUN SlettTmpChild IN hTranslogg NO-ERROR.
              WHEN 11 THEN IF VALID-HANDLE(wHistorikk) THEN
                  RUN SlettTmpChild IN wHistorikk NO-ERROR.
          END CASE.
      END.
     ASSIGN wAktivFlip = INT(chTabStrip:SelectedItem:Index).
     RUN ByttFrame. /* Bytter tab */
  END.    
  ELSE DO:
/*       Artikkelinformasjon,Info2,Kalkyle,Strekkode,Pakke,Erstatningsvare,Bestilling,Lager,Transaksjoner,Historikk */
      IF VALID-HANDLE(hAktivHandle) THEN
          RUN SetEntry IN hAktivHandle NO-ERROR.
      ELSE CASE wAktivFlip:
          WHEN 1 THEN DO WITH FRAME DEFAULT-FRAME:
              IF ArtBas.Vg:SENSITIVE = TRUE 
                THEN APPLY "ENTRY" TO ArtBas.Vg.
              ELSE APPLY "ENTRY" TO ArtBas.Beskr.
          END.
          WHEN 9 THEN DO:
              APPLY "ENTRY":U TO BROWSE-Lager IN FRAME FRAME-Lager.
          END.
/*           WHEN 7 THEN DO:                                           */
/*               APPLY "ENTRY":U TO BROWSE-Lager IN FRAME FRAME-Lager. */
/*           END.                                                      */
      END.
      RETURN NO-APPLY.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-ArtInfo
&Scoped-define SELF-NAME ArtBas.Depositum
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ArtBas.Depositum C-ArtKort
ON TAB OF ArtBas.Depositum IN FRAME FRAME-ArtInfo /* Depositum */
OR RETURN OF ArtBas.Depositum
DO:
    IF wModus = "NY" THEN
    APPLY "ENTRY":U TO ArtBas.Vg    IN FRAME DEFAULT-FRAME.
  ELSE
    APPLY "ENTRY":U TO ArtBas.VgKat IN FRAME DEFAULT-FRAME.
  RETURN NO-APPLY.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ArtBas.Etikettekst1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ArtBas.Etikettekst1 C-ArtKort
ON RETURN OF ArtBas.Etikettekst1 IN FRAME FRAME-ArtInfo /* Etikettekst 1 */
OR TAB OF ArtBas.Etikettekst1 
DO:

    IF iTabOrdning = 1 THEN DO:
        APPLY "ENTRY" TO ArtBas.LinkVareNr IN FRAME FRAME-ArtInfo.
        RETURN NO-APPLY.
    END.
    ELSE APPLY LASTKEY.

  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ArtBas.Farg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ArtBas.Farg C-ArtKort
ON LEAVE OF ArtBas.Farg IN FRAME FRAME-ArtInfo /* Farge */
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
&Scoped-define SELF-NAME FI-LevFargKod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-LevFargKod C-ArtKort
ON TAB OF FI-LevFargKod IN FRAME DEFAULT-FRAME /* Lev.fargekode */
OR "RETURN" OF ArtBas.LevFargKod
DO:
    APPLY "ENTRY" TO ArtBas.MatKod IN FRAME FRAME-ArtInfo.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-ArtInfo
&Scoped-define SELF-NAME ArtBas.HovedKatNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ArtBas.HovedKatNr C-ArtKort
ON TAB OF ArtBas.HovedKatNr IN FRAME FRAME-ArtInfo /* Hov.kategori */
OR "RETURN":U OF ArtBas.HovedKatNr
DO:
  FIND Hovedkategori NO-LOCK WHERE
    HovedKategori.HovedKatNr = INPUT ArtBas.HovedKatNr NO-ERROR.
  IF NOT AVAILABLE Hovedkategori THEN
    DO:
      DISPLAY
        "" @ ArtBas.HovedKatNr
        "" @ HovedKategori.HovedKatTekst
      WITH FRAME FRAME-ArtInfo.  
      RETURN NO-APPLY.
    END.
  DISPLAY
      HovedKategori.HovedKatTekst
      WITH FRAME FRAME-ArtInfo.      
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
        RUN d-visbil.w (INPUT RECID(BildeRegister)).
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


&Scoped-define FRAME-NAME FRAME-Info2
&Scoped-define SELF-NAME ArtBas.KatalogPris
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ArtBas.KatalogPris C-ArtKort
ON VALUE-CHANGED OF ArtBas.KatalogPris IN FRAME FRAME-Info2 /* Katalogpris(Engros) */
DO:
  IF dec(ArtBas.KjedeRab%:screen-value) <> 0 THEN
    DO:
      ASSIGN 
        ArtBas.KjedeInnkPris:screen-value = STRING(
            dec(ArtBas.KatalogPris:screen-value) - (dec(ArtBas.KatalogPris:screen-value) * dec(ArtBas.KjedeRab%) / 100)
                                                  ).
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ArtBas.KjedeInnkPris
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ArtBas.KjedeInnkPris C-ArtKort
ON VALUE-CHANGED OF ArtBas.KjedeInnkPris IN FRAME FRAME-Info2 /* Kjedens innkj.pris */
DO:
  IF dec(ArtBas.KatalogPris:screen-value) <> 0 THEN
    DO:
      ASSIGN 
        ArtBas.KjedeRab%:screen-value = STRING(
            (dec(ArtBas.KatalogPris:screen-value) - dec(ArtBas.KjedeInnkPris:screen-value))  / dec(ArtBas.KatalogPris:screen-value) * 100
                                                  ).
    END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ArtBas.KjedeRab%
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ArtBas.KjedeRab% C-ArtKort
ON VALUE-CHANGED OF ArtBas.KjedeRab% IN FRAME FRAME-Info2 /* Kjedens rabatt% */
DO:
  IF dec(ArtBas.KatalogPris:screen-value) <> 0 THEN
    DO:
      ASSIGN 
        ArtBas.KjedeInnkPris:screen-value = STRING(
            dec(ArtBas.KatalogPris:screen-value) - (dec(ArtBas.KatalogPris:screen-value) * dec(ArtBas.KjedeRab%) / 100)
                                                  ).
    END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-ArtInfo
&Scoped-define SELF-NAME ArtBas.Klack
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ArtBas.Klack C-ArtKort
ON LEAVE OF ArtBas.Klack IN FRAME FRAME-ArtInfo /* Hæl */
DO:
  FIND Klack NO-LOCK WHERE
    Klack.Klack-Id = int(ArtBas.Klack:screen-value IN FRAME FRAME-ArtInfo) NO-ERROR.
  IF AVAILABLE Klack THEN
    DISPLAY Klack.Beskrivning WITH FRAME FRAME-ArtInfo.
  ELSE DO:
      DISPLAY
        "" @ ArtBas.Klack
        "" @ Klack.Beskrivning
      WITH FRAME FRAME-ArtInfo.  
      RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME ArtBas.lager
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ArtBas.lager C-ArtKort
ON RETURN OF ArtBas.lager IN FRAME DEFAULT-FRAME /* Lagerstyrt */
DO:
  APPLY "tab":U TO {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ArtBas.lager C-ArtKort
ON VALUE-CHANGED OF ArtBas.lager IN FRAME DEFAULT-FRAME /* Lagerstyrt */
DO:
    IF SELF:CHECKED THEN
        ASSIGN 
          ArtBas.Opris:CHECKED    = FALSE
          ArtBas.NON_Sale:CHECKED = FALSE
          ArtBas.NegVare:CHECKED  = FALSE
        .
        
    IF AVAILABLE ArtBas THEN
    DO:    
      IF CAN-FIND(FIRST TransLogg WHERE
                    Translogg.ArtikkelNr = ArtBas.ArtikkelNr AND
                    Translogg.Postert = FALSE) THEN
      DO:
        MESSAGE 'Det ligger ikke oppdaterte transaksjoner på artikkelen.' SKIP
                'Disse må oppdateres for artikkelens lagerstatus kan endres.' SKIP(1)
                'Sjekk at batch server går. Uten at denne går, oppdateres ikke artiklenes transaksjoner.'
                VIEW-AS ALERT-BOX WARNING.
        RETURN NO-APPLY.
      END.    

      IF CAN-FIND(FIRST TransLogg WHERE
                    Translogg.ArtikkelNr = ArtBas.ArtikkelNr) THEN
      DO:
        RUN d-EndreArtLagerstatAdvarsel.w (1,OUTPUT wOk).
        IF wOk = FALSE THEN 
          DO:
            ArtBas.Lager:CHECKED = ArtBas.Lager.
            RETURN NO-APPLY.
          END.
      END.

      /* Endres fra lagerstyrt til ikke lagerstyrt */
      IF SELF:CHECKED = FALSE THEN
        LAGER_TIL_IKKE_LAGER:
        DO:
          /* Ligger det noen transaksjoner, skal bruker bekrefte endring. */
          IF CAN-FIND(FIRST Translogg WHERE
                      Translogg.ArtikkelNr = ArtBas.ArtikkelN) THEN 
          DO:
            wOk = FALSE.
            MESSAGE 'Bekreft at artikkelen skal endres fra LAGERSTYRT til IKKE LAGERSTYRT.' SKIP(1)
              'Artikkelinformasjonen vil bli lagret og lagerteller vil bli satt til 0 for alle butikker.' SKIP
              'Nullstilling av lagerteller gjøres automatisk ved at det blir generert lagerjusteringstransaksjoner.' SKIP
              'Når disse oppdateres av batch server, nullstilles lagerteller.' SKIP(1)
              (IF ArtBas.ModellFarge <> 0 THEN 
                'Artikkelen inngår i en modell. Lagerstatus på de andre artikkelene i modellen vil ikke bli endret.'
               ELSE '')
              UPDATE wOk
              VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO TITLE 'ENDRING AV ARTIKKELS LAGERSTATUS'.
          END.
          ELSE wOk = TRUE.
           
          IF wOk = FALSE THEN 
            DO:
              SELF:CHECKED = TRUE.
              RETURN NO-APPLY.
            END.
          ELSE DO:
            RUN lagreArtBas (0).
            ASSIGN
              cStorrelser = ''
              cAntall     = ''.
            FOR EACH tmpLager:
              IF CAN-FIND(FIRST ArtLag WHERE
                            ArtLag.ArtikkelNr = ArtBas.ArtikkelNr AND
                            ArtLag.Butik      = int(tmpLager.Butik)) THEN
              DO:
                cAntall = ''.
                /* Korrigerer størrelser med paranteser i listen. Prantesene tas bort. */
                DO iCount = 1 TO NUM-ENTRIES(cLagerStrListe,";"):
                    /* Korrigerer størrelse */
                    IF ENTRY(iCount,cLagerStrListe,";") BEGINS "(" THEN DO:
                        ASSIGN cTmpStr = ENTRY(iCount,cLagerStrListe,";")
                               cTmpStr = SUBSTR(cTmpStr,2)
                               cTmpStr = SUBSTR(cTmpStr,1,LENGTH(cTmpStr) - 1)
                               ENTRY(iCount,cLagerStrListe,";") = cTmpStr.
                    END.
                    FIND StrKonv NO-LOCK WHERE
                      StrKonv.Storl = ENTRY(iCount,cLagerStrListe,";") NO-ERROR.
                    IF AVAILABLE StrKonv THEN
                      FIND ArtLag NO-LOCK WHERE
                        ArtLag.ArtikkelNr = ArtBas.ArtikkelNr AND
                        ArtLag.Butik      = int(tmpLager.Butik) AND
                        ArtLag.StrKode    = StrKonv.StrKode NO-ERROR. 
                    
                    /* Bygger antallstreng på nytt lagerantall */
                    cAntall = cAntall + 
                              (IF AVAILABLE ArtLag AND ArtLag.Lagant <> 0 
                                 THEN STRING(ArtLag.LagAnt)
                                 ELSE '') + ','.                              
                END.
                FIND Lager NO-LOCK WHERE
                  Lager.ArtikkelNr = ArtBas.ArtikkelNr AND
                  Lager.Butik      = int(tmpLager.Butik) NO-ERROR.
                IF AVAILABLE clButiker THEN
                  FIND ArtPris NO-LOCK WHERE
                       ArtPris.ArtikkelNr = ArtBAs.ArtikkelNr AND
                       ArtPris.ProfilNr = clButiker.ProfilNr NO-ERROR.
                ELSE FIND FIRST ArtPris OF ArtBas NO-ERROR.                
                IF AVAILABLE Lager THEN DO:
                  RUN oppdatlagerjust.p (tmpLager.Butik,
                                         ArtBas.ArtikkelNr,
                                         7,
                                         cLagerStrListe,
                                         cAntall,
                                         (IF AVAILABLE ArtPris THEN 
                                           ArtPris.VareKost[1]
                                          ELSE Lager.VVarekost)).
                END.
              END.
            END.
          END.
        END. /* LAGER_TIL_IKKE_LAGER */
        
      /* Endres fra ikke lagerstyrt til lagerstyrt */
      ELSE IF SELF:CHECKED THEN
        IKKE_LAGER_TIL_LAGER:
        DO:
          /* Ligger det noen transaksjoner, skal bruker bekrefte endring. */
          IF CAN-FIND(FIRST Translogg WHERE
                      Translogg.ArtikkelNr = ArtBas.ArtikkelN) THEN 
          DO:
            wOk = FALSE.
            MESSAGE 'Bekreft at artikkelen skal endres fra IKKE LAGERSTYRT til LAGERSTYRT.' SKIP(1)
              'Artikkelinformasjonen vil bli lagret. Lagerteller vil bli intiert basert på registrerte transaksjoner.' SKIP
              'Varetelling må gjøres på artikkelen for å sikre at den får riktig lager. (Gjøres med Svinn funksjonen på lager flik i artikkelkort).' SKIP(1)
              (IF ArtBas.ModellFarge <> 0 THEN 
                'Artikkelen inngår i en modell. Lagerstatus på de andre artikkelene i modellen vil ikke bli endret.'
               ELSE '')
              UPDATE wOk
              VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO TITLE 'ENDRING AV ARTIKKELS LAGERSTATUS'.
          END.
          ELSE wOk = TRUE.
          
          IF wOk = FALSE THEN 
            DO:
              SELF:CHECKED = FALSE.
              RETURN NO-APPLY.
            END.
          ELSE DO:
            RUN lagreArtBas (0).
            RUN kall_korrigerArtlag_fra_translogg.p (INPUT ArtBas.ArtikkelNr).
            RUN VisLager (NO).
          END.
        END. /* IKKE_LAGER_TIL_LAGER */
        
    END.    

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


&Scoped-define SELF-NAME ArtBas.LevFargKod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ArtBas.LevFargKod C-ArtKort
ON RETURN OF ArtBas.LevFargKod IN FRAME FRAME-ArtInfo /* LevFargKod */
DO:
  APPLY "TAB" TO SELF.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME ArtBas.LevKod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ArtBas.LevKod C-ArtKort
ON TAB OF ArtBas.LevKod IN FRAME DEFAULT-FRAME /* Lev.art.nr */
OR "RETURN" OF ArtBas.LevKod
DO:
    IF wModus <> "ENDRE" AND 
        INPUT ArtBas.LevNr > 0 AND
        INPUT ArtBas.LevKod <> '' AND
        CAN-FIND(FIRST bArtBas WHERE
                 bArtBas.LevNr = INPUT ArtBas.LevNr AND
                 bArtBas.LevKod = INPUT ArtBas.LevKod) THEN
    DO:
        bOk = FALSE.
        MESSAGE 'Det finnes allerede en artikkel på denne leverandør med det angitte lev.art.nr.' SKIP
            'Skal artikkelen hentes opp i artikkelkortet?'
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE bOk.
        IF bOk THEN
        DO:
           wModus = 'ENDRE'.
           FIND FIRST bArtBas WHERE
                bArtBas.LevNr = INPUT ArtBas.LevNr AND
                bArtBas.LevKod = INPUT ArtBas.LevKod NO-ERROR.
           wArtBasRecid = RECID(bArtBas).
           RUN HentVis(wArtBasRecid,?).
           RUN SettEnableDisable.
           RUN Toolbar.
           RETURN NO-APPLY.
        END.
    END.
    ELSE IF INPUT ArtBas.LevNr > 0 AND
        INPUT ArtBas.LevKod <> '' AND
        CAN-FIND(FIRST bArtBas WHERE
                 bArtBas.LevNr = INPUT ArtBas.LevNr AND
                 bArtBas.LevKod = INPUT ArtBas.LevKod AND
                 RECID(bArtBas) <> RECID(ArtBas)) THEN
    DO:
        bOk = FALSE.
        MESSAGE 'Det finnes allerede en artikkel på denne leverandør med det angitte lev.art.nr.' SKIP
            'Vil du alikevel ha dette artikkelnr på denne artikkelen?'
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE bOk.
        IF NOT bOk THEN
        DO:
           RETURN NO-APPLY.
        END.
    END.

    IF iTabOrdning = 1 THEN 
        APPLY "ENTRY" TO ArtBas.VmId IN FRAME FRAME-ArtInfo.
    ELSE IF ArtBas.StrTypeId:SENSITIVE IN FRAME FRAME-ArtInfo THEN
        APPLY "ENTRY" TO ArtBas.StrTypeId IN FRAME FRAME-ArtInfo.
    ELSE
        APPLY "ENTRY" TO ArtBas.Sasong IN FRAME FRAME-ArtInfo.

    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ArtBas.LevNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ArtBas.LevNr C-ArtKort
ON TAB OF ArtBas.LevNr IN FRAME DEFAULT-FRAME /* Leverandør */
OR "RETURN":U OF ArtBas.LevNr
DO:
  IF wModus <> "NY" AND AVAIL ArtBas AND ArtBas.ArtikkelNr = ArtBas.Vg THEN DO:
      MESSAGE "PLU-artikkel, endring avbrutt."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      IF AVAILABLE ArtBas THEN
          ASSIGN ArtBas.LevNr:SCREEN-VALUE = STRING(ArtBas.LevNr).
      RETURN NO-APPLY.
  END.

  /* Validerer levenr. */
  RUN Sok2LevBas.  
  IF RETURN-VALUE = "AVBRYT" THEN
      RETURN NO-APPLY.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-ArtInfo
&Scoped-define SELF-NAME ArtBas.LinkVareNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ArtBas.LinkVareNr C-ArtKort
ON LEAVE OF ArtBas.LinkVareNr IN FRAME FRAME-ArtInfo /* Link til pantevare */
DO:
  IF INPUT ArtBas.LinkVareNr > 0 AND
    NOT CAN-FIND(ArtBas WHERE
                 ArtBas.ArtikkelNr = INPUT ArtBas.LinkVareNr AND
                 ArtBas.Pant = TRUE) THEN
    DO:
      MESSAGE 'Link kan bare legges inn på varer som er satt opp som pantvare.'
        VIEW-AS ALERT-BOX WARNING.
      FI-Linkvaretekst:SCREEN-VALUE = ''.
      RETURN NO-APPLY.
    END.

    FI-Linkvaretekst:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","ArtBas;Beskr","WHERE ArtikkelNr = '" + ArtBas.LinkVareNr:SCREEN-VALUE + "'").
    
    IF iTabOrdning = 1 THEN DO:
        APPLY "ENTRY" TO ArtBas.Jamforenhet IN FRAME FRAME-ArtInfo.
        RETURN NO-APPLY.
    END.
    ELSE APPLY LASTKEY.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
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


&Scoped-define SELF-NAME ArtBas.ManueltOpprettet
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ArtBas.ManueltOpprettet C-ArtKort
ON RETURN OF ArtBas.ManueltOpprettet IN FRAME DEFAULT-FRAME /* Man. opprettet */
DO:
    APPLY "tab":U TO {&SELF-NAME}.

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


&Scoped-define SELF-NAME ArtBas.Mengde
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ArtBas.Mengde C-ArtKort
ON VALUE-CHANGED OF ArtBas.Mengde IN FRAME FRAME-ArtInfo /* Mengde i salgsenhet */
DO:
  IF NOT AVAILABLE ArtPris THEN
    FIND FIRST ArtPris OF ArtBas NO-LOCK NO-ERROR.
  IF AVAILABLE ArtPris THEN 
    FI-JamforPris:screen-value = STRING(ROUND(ArtPris.Pris[1] / INPUT ArtBas.Mengde,2)).
  IF INPUT FI-JamforPris:screen-value = ? then FI-JamforPris:screen-value = ''.

    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ArtBas.MengdeRabatt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ArtBas.MengdeRabatt C-ArtKort
ON VALUE-CHANGED OF ArtBas.MengdeRabatt IN FRAME FRAME-ArtInfo /* Mengderabatt */
DO:
  IF SELF:CHECKED = FALSE THEN
  DO:
    wOk = FALSE.
    MESSAGE 'Eventuelle mengderabatter som er angitt på kalkyle nullstilles.' + CHR(10) + 
            '(Mengderabatt antall og pris vedlikeholdes på kalkyle pr. profil).'
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE wOk.
    IF NOT wOk THEN
    DO:
        SELF:CHECKED = TRUE.
        RETURN NO-APPLY.
    END.
  END.
  ELSE DO:
    MESSAGE 
        'Menderabatt er nå aktivert på artikkelen.' + CHR(10) + 
        'Antall og pris for mengderabatten vedlikeholdes pr. prisprofil på kalkylefliken.'
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Angre_kopier
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Angre_kopier C-ArtKort
ON CHOOSE OF MENU-ITEM m_Angre_kopier /* Angre kopier */
DO:
  BUTTON-Kopierbilde:PRIVATE-DATA IN FRAME DEFAULT-FRAME = ?.
  MENU-ITEM m_Angre_kopier:SENSITIVE IN MENU POPUP-MENU-BUTTON-Kopierbilde = FALSE.
  MENU-ITEM m_Lim_inn:SENSITIVE IN MENU POPUP-MENU-BUTTON-Kopierbilde = FALSE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Kopier
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Kopier C-ArtKort
ON CHOOSE OF MENU-ITEM m_Kopier /* Kopier */
DO:
    BUTTON-Kopierbilde:PRIVATE-DATA IN FRAME DEFAULT-FRAME = STRING(ArtBas.BildNr).
    MENU-ITEM m_Angre_kopier:SENSITIVE IN MENU POPUP-MENU-BUTTON-Kopierbilde = 
                             MENU-ITEM m_Kopier:SENSITIVE  IN MENU POPUP-MENU-BUTTON-Kopierbilde.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Kopier_fra_artikkel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Kopier_fra_artikkel C-ArtKort
ON CHOOSE OF MENU-ITEM m_Kopier_fra_artikkel /* Kopier fra artikkel */
DO:
    DEFINE VARIABLE iBildNr AS INTEGER  NO-UNDO.
    RUN d-SokBildeArtikkel.w (OUTPUT iBildNr).
    IF iBildNr = 0 THEN
        RETURN NO-APPLY.
    ELSE DO:
        BUTTON-Kopierbilde:PRIVATE-DATA IN FRAME DEFAULT-FRAME = STRING(iBildNr).
        MENU-ITEM m_Angre_kopier:SENSITIVE IN MENU POPUP-MENU-BUTTON-Kopierbilde = 
                                 MENU-ITEM m_Kopier:SENSITIVE  IN MENU POPUP-MENU-BUTTON-Kopierbilde.
    END.
    APPLY "CHOOSE" TO MENU-ITEM m_Lim_inn IN MENU POPUP-MENU-BUTTON-Kopierbilde.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Lim_inn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Lim_inn C-ArtKort
ON CHOOSE OF MENU-ITEM m_Lim_inn /* Lim inn */
DO:
    FIND BildeRegister WHERE Bilderegister.Bildnr = INT(BUTTON-Kopierbilde:PRIVATE-DATA IN FRAME DEFAULT-FRAME) NO-ERROR.
    IF AVAILABLE BildeRegister THEN 
    DO:
        hWindow:PRIVATE-DATA = "VISKNAPP".
        RUN d-visbil.w (INPUT RECID(BildeRegister)).
        IF RETURN-VALUE = "PASTE" THEN DO:

            RUN Kopierbilde (Bilderegister.bildnr,Artbas.artikkelnr,Artbas.bildnr).
            RUN VisBilde (11).
            RUN VisArtBas. 
            RUN VisEndretinfo.
            RUN Toolbar.
            MENU-ITEM m_Lim_inn:SENSITIVE IN MENU POPUP-MENU-BUTTON-Kopierbilde = FALSE.
        END.
    END.
    hWindow:PRIVATE-DATA = ?.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME ArtBas.NON_Sale
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ArtBas.NON_Sale C-ArtKort
ON VALUE-CHANGED OF ArtBas.NON_Sale IN FRAME DEFAULT-FRAME /* Non-Sale */
DO:
    APPLY 'choose' TO BUTTON-Lagre IN FRAME Default-Frame.
    APPLY 'entry' TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ArtBas.OPris
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ArtBas.OPris C-ArtKort
ON VALUE-CHANGED OF ArtBas.OPris IN FRAME DEFAULT-FRAME /* Åpen pris */
DO:
  IF SELF:CHECKED THEN DO:
    MESSAGE "Du endrer nå artikkelen til 'Åpen pris' artikkel." SKIP
            "Dette innebærer at artikkelen blir IKKE lagerstyrt og at varekosten på solgte varer blir beregnet på bakgrunn av db% som er lagt inn i kalkylen." SKIP(1)
            "Bekreft at du vil gjøre dette."
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Endre til ÅPEN PRIS ARTIKKEL"
          UPDATE bOk.
    IF bOk = FALSE THEN DO: 
        Artbas.OPris:CHECKED = FALSE.
        RETURN NO-APPLY.
    END.
    FIND StrType WHERE StrType.StrTypeID = 2 NO-LOCK NO-ERROR.
    ASSIGN /*ArtBas.StrTypeID:SCREEN-VALUE IN FRAME FRAME-ArtInfo = "2"*/
           ArtBas.Lager:CHECKED IN FRAME DEFAULT-FRAME = FALSE
           /*
           ArtBas.StrTypeID:SENSITIVE = FALSE
           ArtBas.OLLager:CHECKED   = FALSE
           BUTTON-SokStrType:SENSITIVE IN FRAME FRAME-ArtInfo = FALSE
           StrType.Beskrivelse:SCREEN-VALUE = IF AVAIL StrType THEN
           StrType.Beskrivelse ELSE ""
           */
           .
  END.
  ELSE DO:
      MESSAGE "Du endrer nå artikkelen fra 'Åpen pris' artikkel til vanlig lagerstyrt artikkel." SKIP
              "Dette innebærer at artikkelen blir lagerstyrt og at varekosten på solgte varer blir beregnet på bakgrunn av vektet varekostberegning." SKIP(1)
              "Bekreft at du vil gjøre dette."
          VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Endre ÅPEN PRIS ARTIKKEL til vanlig artikkel"
            UPDATE bOk.
      IF bOk = FALSE THEN DO:
          ArtBas.OPris:CHECKED = TRUE.
          RETURN NO-APPLY.
      END.
      ASSIGN 
          /*ArtBas.StrTypeID:SCREEN-VALUE IN FRAME FRAME-ArtInfo    = ""*/
          /*StrType.Beskrivelse:SCREEN-VALUE IN FRAME FRAME-ArtInfo = ""*/
          ArtBas.Lager:CHECKED     = cLagerStyr = "1"
          /*
          BUTTON-SokStrType:SENSITIVE IN FRAME FRAME-ArtInfo      = TRUE
          ArtBas.StrTypeID:SENSITIVE IN FRAME FRAME-ArtInfo       = TRUE
          ArtBas.OLLager:CHECKED   = cOLLager = "yes" AND cLagerStyr = "1"
          */
          .
  END.   
  ASSIGN /*
         ArtBas.OLLager:SENSITIVE = NOT SELF:CHECKED
         ArtBas.Lager:SENSITIVE   = NOT SELF:CHECKED.
         */
      .
  APPLY 'choose' TO BUTTON-Lagre IN FRAME Default-Frame.
  APPLY 'entry' TO SELF.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-ArtInfo
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


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME ArtBas.Pakke
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ArtBas.Pakke C-ArtKort
ON VALUE-CHANGED OF ArtBas.Pakke IN FRAME DEFAULT-FRAME /* Pakke */
DO:
    IF SELF:CHECKED THEN 
    DO:
        IF ArtBas.Lager:checked THEN 
        DO:
          MESSAGE 'Artikkelen er lagerstyrt. Artikkelen må endres til ikke lagerstyrt før pakkeflagget kan endres.'
          VIEW-AS ALERT-BOX.
          ArtBas.Pakke:checked = FALSE.  
          RETURN NO-APPLY.
        END.
    END.
    
    IF wModus = "NY" THEN DO:
            FIND StrType WHERE StrType.StrTypeID = 2 NO-LOCK NO-ERROR.
            ASSIGN 
            ArtBas.StrTypeID:SCREEN-VALUE IN FRAME FRAME-ArtInfo = "2"
            ArtBas.StrTypeID:SENSITIVE = FALSE
            StrType.Beskrivelse:SCREEN-VALUE = IF AVAIL StrType THEN
                                                 StrType.Beskrivelse ELSE ""
            BUTTON-SokStrType:SENSITIVE IN FRAME FRAME-ArtInfo = FALSE.
    END.
    ELSE DO:
            ASSIGN ArtBas.StrTypeID:SENSITIVE IN FRAME FRAME-ArtInfo       = TRUE
               BUTTON-SokStrType:SENSITIVE IN FRAME FRAME-ArtInfo      = TRUE
               ArtBas.IKasse:CHECKED            = TRUE
               ArtBas.IKasse:SENSITIVE          = TRUE
            .
    END.
    APPLY 'choose' TO BUTTON-Lagre IN FRAME Default-Frame.
    APPLY 'entry' TO SELF.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ArtBas.Pant
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ArtBas.Pant C-ArtKort
ON VALUE-CHANGED OF ArtBas.Pant IN FRAME DEFAULT-FRAME /* Pantvare */
DO:
  APPLY 'choose' TO BUTTON-Lagre IN FRAME Default-Frame.
  APPLY 'entry' TO SELF.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-ArtInfo
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
      
    IF iTabOrdning = 1 THEN DO:
        APPLY "ENTRY" TO ArtBas.BongTekst IN FRAME FRAME-ArtInfo.
        RETURN NO-APPLY.
    END.
      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ArtBas.PubliserINettbutikk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ArtBas.PubliserINettbutikk C-ArtKort
ON VALUE-CHANGED OF ArtBas.PubliserINettbutikk IN FRAME FRAME-ArtInfo /* Publiser */
DO:
/*   IF ArtBas.WebButikkArtikkel:CHECKED THEN            */
/*   DO:                                                 */
/*       bOk = FALSE.                                    */
/*       IF ArtBas.PubliserINettbutikk:CHECKED THEN      */
/*       DO:                                             */
/*           ArtBas.PubliserINettbutikk:CHECKED = FALSE. */
/*           RETURN NO-APPLY.                            */
/*       END.                                            */
/*       ELSE DO:                                        */
/*           ArtBas.PubliserINettbutikk:CHECKED = TRUE.  */
/*           ArtBas.WebButikkArtikkel:CHECKED   = TRUE.  */
/*           RETURN NO-APPLY.                            */
/*       END.                                            */
/*   END.                                                */


    IF ArtBas.PubliserINettbutikk:CHECKED THEN
        ArtBas.WebButikkArtikkel:CHECKED = TRUE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ArtBas.RAvdNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ArtBas.RAvdNr C-ArtKort
ON LEAVE OF ArtBas.RAvdNr IN FRAME FRAME-ArtInfo /* Vareområde */
DO:
    FIND Regnskapsavdeling NO-LOCK WHERE
      Regnskapsavdeling.RAvdNr = int(ArtBas.RAvdNr:screen-value IN FRAME FRAME-ArtInfo) NO-ERROR.
    IF AVAILABLE Regnskapsavdeling THEN
      DISPLAY Regnskapsavdeling.RAvdBeskrivelse WITH FRAME FRAME-ArtInfo.
    ELSE IF INPUT ArtBas.RAvdNr > 0 THEN 
    DO:
        DISPLAY
          "" @ ArtBas.RAvdNr
          "" @ Regnskapsavdeling.RAvdBeskrivelse
        WITH FRAME FRAME-ArtInfo.  
        MESSAGE "Ugyldig regsnkapskode."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.
    ELSE DO:
        DISPLAY
          "" @ ArtBas.RAvdNr
          "" @ Regnskapsavdeling.RAvdBeskrivelse
        WITH FRAME FRAME-ArtInfo.  
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
  wVisLager = ?.
  RUN VisLager (TRUE).
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-ArtInfo
&Scoped-define SELF-NAME ArtBas.SaSong
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ArtBas.SaSong C-ArtKort
ON BACK-TAB OF ArtBas.SaSong IN FRAME FRAME-ArtInfo /* Sesong */
DO:
  APPLY "ENTRY":U TO SkoTex.ArtBas.LevFargKod IN FRAME FRAME-ArtInfo.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ArtBas.SaSong C-ArtKort
ON LEAVE OF ArtBas.SaSong IN FRAME FRAME-ArtInfo /* Sesong */
DO:
    DO WITH FRAME FRAME-ArtInfo:
        FIND Sasong NO-LOCK WHERE
          Sasong.Sasong = int(ArtBas.Sasong:screen-value IN FRAME FRAME-ArtInfo) NO-ERROR.
        IF AVAILABLE Sasong THEN DISPLAY Sasong.SasBeskr WITH FRAME FRAME-ArtInfo.
        ELSE IF AVAIL ArtBas THEN DO:
            DISPLAY
              ArtBas.Sasong @ ArtBas.Sasong
            /*         "" @ Sasong.SasBeskr */.  
          RETURN NO-APPLY.
        END.
        ELSE
            ASSIGN ArtBas.Sasong:SCREEN-VALUE = ""
                   Sasong.SasBeskr:SCREEN-VALUE = "".
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
  END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ArtBas.StrTypeID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ArtBas.StrTypeID C-ArtKort
ON LEAVE OF ArtBas.StrTypeID IN FRAME FRAME-ArtInfo /* StrType */
OR "TAB":U OF ArtBas.StrTypeID OR "RETURN":U OF ArtBas.StrTypeID DO:
  DEF VAR cTxt AS CHAR NO-UNDO.

  IF SELF:SENSITIVE THEN DO:
      IF SELF:SCREEN-VALUE = "1" THEN DO:
          RUN GetTx (141,OUTPUT cTxt).
          MESSAGE cTxt VIEW-AS ALERT-BOX WARNING. 
          APPLY "ENTRY" TO ArtBas.StrTypeId.
          ERROR-STATUS:ERROR = TRUE.
           RETURN NO-APPLY.
      END. 
      ELSE IF INPUT ArtBas.StrTypeId = 0 OR INPUT ArtBas.StrTypeId = ? THEN DO:
          RETURN.
      END.
  END.
  IF wModus <> "NY" THEN DO:
/* TN 23/9-07 Dette er en ikke relevant test. Den skaper bare problemenr.     */
/*            På messe og messeforberedelser må størrelsestype kunne byttes.  */
/*       IF INPUT ArtBas.StrTypeID <> ArtBas.StrTypeID AND                    */
/*          CAN-FIND(FIRST BestHode OF ArtBas WHERE                           */
/*                BestHode.BestStat < 6) THEN DO:                             */
/* /*           BestHode.BestStat < 6 AND BestHode.TotAntPar > 0) THEN DO: */ */
/*           MESSAGE "Det finnes ikke fulleverte bestillinger på varen." SKIP */
/*                   "Endring av størrelsestype er ikke tillat"               */
/*               VIEW-AS ALERT-BOX INFO BUTTONS OK.                           */
/*           ASSIGN SELF:SCREEN-VALUE  = STRING(ArtBas.StrTypeID)             */
/*                  ERROR-STATUS:ERROR = TRUE.                                */
/*           APPLY "ENTRY" TO ArtBas.StrTypeId.                               */
/*           RETURN NO-APPLY.                                                 */
/*       END.                                                                 */
  END.
  FIND StrType NO-LOCK WHERE
       StrType.StrTypeID = INPUT ArtBas.StrTypeId NO-ERROR.
  IF NOT AVAILABLE StrType THEN
      DO:
        IF wModus = "NY" THEN
            DISPLAY
              "" @ ArtBas.StrType
              "" @ StrType.Beskrivelse
            WITH FRAME FRAME-ArtInfo.  
        ELSE DO:
            MESSAGE "Ukjent størrelsestype"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RUN VisStrlType.
            ASSIGN ERROR-STATUS:ERROR = TRUE.
            RETURN NO-APPLY.
        END.
        RETURN NO-APPLY.
      END.
    ELSE IF AVAIL StrType AND StrType.Intervall = "" THEN DO:
        MESSAGE "Størrelsestypen feilaktig, mangler definerte størrelser."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        IF NOT wModus = "NY" AND AVAIL ArtBas THEN
            ASSIGN ArtBas.StrTypeId:SCREEN-VALUE IN FRAME Frame-ArtInfo = STRING(ArtBas.StrTypeId).
        ELSE
            DISPLAY "" @ ArtBas.StrTypeID
                    "" @ StrType.Beskrivelse.
        RETURN.
    END.
    ELSE 
    DISPLAY
      StrType.Beskrivelse
    WITH FRAME FRAME-ArtInfo.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ArtBas.StrTypeID C-ArtKort
ON VALUE-CHANGED OF ArtBas.StrTypeID IN FRAME FRAME-ArtInfo /* StrType */
DO:

  /* Kontroll av PLU artikkler. */
  IF wModus <> "NY" AND AVAILABLE ArtBas THEN
  DO:
      IF ArtBas.ArtikkelNr = ArtBas.Vg THEN 
      DO:
          MESSAGE "Varegruppartikkel, endring ikke tillatt."
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          ASSIGN ArtBas.StrTypeID:SCREEN-VALUE = STRING(ArtBas.StrTypeID).
          RETURN NO-APPLY.
      END.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-1 C-ArtKort
ON VALUE-CHANGED OF T-1 IN FRAME FRAME-ArtInfo
DO:
  IF SELF:CHECKED THEN FI-1:SENSITIVE = TRUE.
  ELSE FI-1:SENSITIVE = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-10
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-10 C-ArtKort
ON VALUE-CHANGED OF T-10 IN FRAME FRAME-ArtInfo
DO:
    /*
    IF SELF:CHECKED THEN ArtBas.LevFargKod:SENSITIVE = TRUE.
    ELSE ArtBas.LevFargKod:SENSITIVE = FALSE.  
    */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-2 C-ArtKort
ON VALUE-CHANGED OF T-2 IN FRAME FRAME-ArtInfo
DO:
    IF SELF:CHECKED THEN FI-2:SENSITIVE = TRUE.
    ELSE FI-2:SENSITIVE = FALSE.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-3 C-ArtKort
ON VALUE-CHANGED OF T-3 IN FRAME FRAME-ArtInfo
DO:
    IF SELF:CHECKED THEN FI-3:SENSITIVE = TRUE.
  ELSE FI-3:SENSITIVE = FALSE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-4 C-ArtKort
ON VALUE-CHANGED OF T-4 IN FRAME FRAME-ArtInfo
DO:
    IF SELF:CHECKED THEN FI-4:SENSITIVE = TRUE.
  ELSE FI-4:SENSITIVE = FALSE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-5 C-ArtKort
ON VALUE-CHANGED OF T-5 IN FRAME FRAME-ArtInfo
DO:
    IF SELF:CHECKED THEN FI-5:SENSITIVE = TRUE.
  ELSE FI-5:SENSITIVE = FALSE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-6 C-ArtKort
ON VALUE-CHANGED OF T-6 IN FRAME FRAME-ArtInfo
DO:
    IF SELF:CHECKED THEN FI-6:SENSITIVE = TRUE.
  ELSE FI-6:SENSITIVE = FALSE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-7 C-ArtKort
ON VALUE-CHANGED OF T-7 IN FRAME FRAME-ArtInfo
DO:
    IF SELF:CHECKED THEN FI-7:SENSITIVE = TRUE.
    ELSE FI-7:SENSITIVE = FALSE.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-8 C-ArtKort
ON VALUE-CHANGED OF T-8 IN FRAME FRAME-ArtInfo
DO:
    IF SELF:CHECKED THEN FI-8:SENSITIVE = TRUE.
    ELSE FI-8:SENSITIVE = FALSE.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-EnableLevInfo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-EnableLevInfo C-ArtKort
ON VALUE-CHANGED OF T-EnableLevInfo IN FRAME FRAME-ArtInfo /* Endre */
DO:
   RUN EnableLevInfo(SELF:CHECKED).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-Lager
&Scoped-define SELF-NAME TG-VisSkjul
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-VisSkjul C-ArtKort
ON VALUE-CHANGED OF TG-VisSkjul IN FRAME FRAME-Lager /* Vis alle str */
DO:
    RUN HideShowLagerCols (STRING(SELF:CHECKED,"Vis/Skjul")).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME ArtBas.Utgatt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ArtBas.Utgatt C-ArtKort
ON RETURN OF ArtBas.Utgatt IN FRAME DEFAULT-FRAME
DO:
    APPLY "tab":U TO {&SELF-NAME}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ArtBas.Utgatt C-ArtKort
ON VALUE-CHANGED OF ArtBas.Utgatt IN FRAME DEFAULT-FRAME
DO:
  DEFINE VARIABLE dDato AS DATE NO-UNDO.
  bOk = FALSE.
  
  IF SELF:CHECKED THEN
  DO: 
    RUN dSetUtgarDato.w (OUTPUT dDato, OUTPUT bOk).
    IF bOk = FALSE THEN 
    DO:
      SELF:CHECKED = FALSE.
      RETURN NO-APPLY.
    END.
      ArtBas.UtgattDato:SCREEN-VALUE = STRING(dDato).
  END.
  ELSE ArtBas.UtgattDato:screen-value = ''.
  
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
      CAPS(INPUT ArtBas.ValKod) @ ArtBas.ValKod
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


&Scoped-define SELF-NAME ArtBas.VareType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ArtBas.VareType C-ArtKort
ON VALUE-CHANGED OF ArtBas.VareType IN FRAME FRAME-ArtInfo /* Leveringsmåte */
DO:
    IF INTEGER(ArtBas.VareType:SCREEN-VALUE) < 3 THEN 
      ASSIGN
         ArtBas.Leveringstid:SCREEN-VALUE = '0'
         ArtBas.Leveringstid:SENSITIVE = FALSE.
    ELSE
      ASSIGN
         ArtBas.Leveringstid:SENSITIVE = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME ArtBas.Vg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ArtBas.Vg C-ArtKort
ON RETURN OF ArtBas.Vg IN FRAME DEFAULT-FRAME /* Varegr */
OR "TAB" OF ArtBas.Vg
DO:
  DEFINE VARIABLE iLopNr AS INTEGER    NO-UNDO.
  
  FIND VarGr NO-LOCK WHERE
    VarGr.Vg = int(ArtBas.Vg:screen-value IN FRAME DEFAULT-FRAME) NO-ERROR.
  IF AVAILABLE VarGr THEN
    DO:
      DISPLAY 
          VarGr.VgBeskr @ FILL-IN-VgBeskr 
      WITH FRAME default-frame.
      IF wKopi AND (iVgTxtBeskr = 1 OR iVgTxtBeskr = 3) THEN DO:
          DISPLAY VarGr.VgBeskr @ ArtBas.BongTekst
              WITH FRAME FRAME-ArtInfo.
          IF iVgTxtBeskr = 3 THEN
              ArtBas.beskr:SCREEN-VALUE = "".
              ArtBas.Etikettekst1:SCREEN-VALUE = "".
/*               DISPLAY "" @ ArtBas.beskr    */
/*                   "" @ ArtBas.Etikettekst1 */
/*               WITH FRAME FRAME-ArtInfo.    */
      END.
      IF wModus = "Ny" AND 
         ArtBas.BongTekst:screen-value IN FRAME FRAME-ArtInfo = "" AND (iVgTxtBeskr = 1 OR iVgTxtBeskr = 3) THEN
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

  IF wModus = "Ny" OR (wForslagLopNr <> "" AND (INPUT ArtBas.LopNr = ? OR
                              INPUT ArtBas.LopNr = 0)) THEN
  DO:
      RUN SettLopNr.p (INPUT INPUT ArtBas.Vg,wForslagLopNr,OUTPUT iLopNr).
      ASSIGN
          ArtBas.LopNr:SCREEN-VALUE = STRING(iLopnr)
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
ON TAB OF ArtBas.VMId IN FRAME FRAME-ArtInfo /* Varemerke */
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


&Scoped-define SELF-NAME ArtBas.WebButikkArtikkel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ArtBas.WebButikkArtikkel C-ArtKort
ON VALUE-CHANGED OF ArtBas.WebButikkArtikkel IN FRAME FRAME-ArtInfo /* Aktiv i nettbutikk */
DO:
  DO WITH FRAME DEFAULT-FRAME:
      bOk = FALSE.
      IF ArtBas.WebButikkArtikkel:CHECKED THEN
      DO:
/*               RETURN NO-APPLY. */
          /* Setter løpenr hvis det ikke er satt. */
          IF ArtBas.LopNr = ? OR ArtBas.LopNr:SCREEN-VALUE = '?' OR TRIM(ArtBas.LopNr:SCREEN-VALUE) = '' THEN
          DO:
              iLopNr = 0.
              RUN SettLopNr.p (ArtBas.Vg,'F',OUTPUT iLopNr).
              IF iLopNr <> 0 THEN DO:
                  RUN SettLopNrIArtOgLager.p (ArtBas.ArtikkelNr, iLopNr).
                  ArtBas.LopNr:SCREEN-VALUE = STRING(iLopNr).
              END.
          END.
      END.
      ELSE DO:
          ArtBas.PubliserINettbutikk:CHECKED = FALSE.
/*               RETURN NO-APPLY. */
      END.
      ASSIGN 
/*           TB-WebButikkArtikkel:checked IN FRAME DEFAULT-FRAME = ArtBas.WebButikkArtikkel */
/*           ArtBas.PubliserINettbutikk:SENSITIVE IN FRAME FRAME-ArtInfo = ArtBas.WebButikkArtikkel:CHECKED */
          .
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-ArtKort 


/* ***************************  Main Block  *************************** */
/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}
       hWindow                       = {&WINDOW-NAME}.

/* Dette vinduet */
ASSIGN
    wCurrent-Window = THIS-PROCEDURE:CURRENT-WINDOW
    .
DYNAMIC-FUNCTION("EmbedMe" IN SOURCE-PROCEDURE,THIS-PROCEDURE) NO-ERROR.

FIND FIRST bBruker NO-LOCK WHERE
    bBruker.Brukerid = USERID('SkoTex') NO-ERROR.
IF NOT AVAILABLE bBruker THEN
DO:
    MESSAGE 'Ukjent bruker'
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
END.

RUN InitTx.

/* Endring av OPris fra strekkoderegistrering. */
SUBSCRIBE TO 'VisArtBas' ANYWHERE.

/* koppling till anropande program */
IF ENTRY(1,wModus) = "ENDRE" AND NUM-ENTRIES(wModus) > 1 THEN DO:
    ASSIGN hParentHandle = WIDGET-HANDLE(ENTRY(2,wModus))
           cParaAktivFlip = IF NUM-ENTRIES(wModus) = 3 THEN ENTRY(3,wModus) ELSE "".
           wModus = "ENDRE".
    SUBSCRIBE TO "ByttArtikkel" IN hParentHandle.
END.
/* Default VPI leverandør */
{syspara.i 1 12 1 iEkstVPILevNr INT}

{syspara.i 1 1 54 cTekst}
IF CAN-DO('1,J,Ja,True,Y,YES',STRING(cTekst)) THEN
    dSkoModus = TRUE.

/* Servicehandel */
{syspara.i 2 5 103 cServicehandel}

/* Bildekatalog */ 
{syspara.i 10 1 2 cBildeKatalog}
ASSIGN cBildeKatalog = TRIM(cBildeKatalog,"\") + "\".
/* Bekreft bytte av bilde */
{syspara.i 10 1 10 cBekreftNyttBilde}
/* Tab ordning */
{syspara.i 2 5 102 iTabOrdning INT}

{syspara.i 2 4 14 cTekst}
IF CAN-DO("1,yes,trye,ja",cTekst) THEN
    lBrukHgiFilter = TRUE.
ELSE
    lBrukHgiFilter = FALSE.

/* Sjekker om det er tvang på etikett. */
{syspara.i 2 4 41 cTekst}
IF CAN-DO('1,J,Ja,Y,YES,True',cTekst) THEN 
  bEtiTvang = TRUE.
ELSE
  bEtiTvang = FALSE.
{syspara.i 2 4 48 cTekst}
  IF cTekst = "1" THEN 
    lOverstyrFrame1VgArt = TRUE.

  
/* Kopiering av varetekst til bongtekst og etikettekst1. */
{syspara.i 2 4 41 cTekst}
IF CAN-DO('1,J,Ja,Y,YES,True',cTekst) THEN 
  bKopierVaretekst = TRUE.
ELSE
  bKopierVaretekst = FALSE.

/* Kopiere vgtekst eller beskr til bongtekst. */
{syspara.i 2 4 11 iVgTxtBeskr INT}
IF iVgTxtBeskr = 0 THEN
    iVgTxtBeskr = 1.
                                  
/* Antall dar sletting artikkel. */
{syspara.i 2 3 2 iAntDarSlett INT}
IF iAntDarSlett = 0 OR iAntDarSlett =  ? THEN
    iAntDarSlett = 730.

/* Skal passord brukes ved lagertrans */
{syspara.i 2 4 25 cBrukPwLagertrans}
{syspara.i 2 3 1 cPwd}
IF cPwd = '' THEN cPwd = 'MASTER'.

{syspara.i 2 4 50 iMellankategori INT}

/* Biblotek, inn/utmelding og ON CLOSE OF THIS... */
{genlib.i 
  &NoLibCall      = "Nei"
  &WindowName     = "Artikkelkort"
  &PreIClose      = " 
    FIND ArtBas NO-LOCK WHERE
        recid(ArtBas) = wArtBasRecid NO-ERROR.
    IF AVAILABLE ArtBas THEN
    DO:
        IF NOT CAN-FIND(FIRST ArtPris OF ArtBas) THEN
        DO:
            MESSAGE 'Det må legges opp en kalkyle på denne artikkelen ('
                     ArtBAs.Vg ArtBAs.LopNr ').'
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RUN TvangKalkyle.
            RETURN NO-APPLY 'AVBRYT'.
        END.
    END.
  "
/*   &PreIClose      = "run VisArtBas." */
  &PostIClose    = "
                     if valid-handle(wHistorikk) then
                       delete procedure wHistorikk.
                     if valid-handle(wKalkyle) then
                     DO:
                       RUN SlettPrisKo IN wKalkyle.
                       delete procedure wKalkyle.
                     END.
                     IF VALID-HANDLE(hPakkeLinje) THEN
                       DELETE PROCEDURE hPakkeLinje.
                     IF VALID-HANDLE(hStrekkode) THEN
                       APPLY 'CLOSE' TO hStrekkode.
                     IF VALID-HANDLE(hErstattning) THEN
                       DELETE PROCEDURE hErstattning.
                     IF VALID-HANDLE(hTranslogg) THEN DO:
                         RUN SlettTmpChild IN hTranslogg.
                         DELETE PROCEDURE hTranslogg.
                     END.
                     IF VALID-HANDLE(hBestilling) THEN
                       DELETE PROCEDURE hBestilling.
                     IF VALID-HANDLE(hIndivid) THEN
                       APPLY 'CLOSE' TO hIndivid.
                     IF VALID-HANDLE(hArtBestPkt) THEN
                       APPLY 'CLOSE' TO hArtBestPkt.
                     IF VALID-HANDLE(h_PrisKo) THEN
                       DELETE PROCEDURE h_PrisKo.
                     for each tmpChild:
                       if valid-handle(tmpChild.wChild) then
                         delete procedure tmpChild.wChild.
                     end.
                     IF VALID-HANDLE(chCtrlFrame) THEN
                        RELEASE OBJECT chCtrlFrame NO-ERROR.
                     IF VALID-HANDLE(CtrlFrame) THEN
                        DELETE OBJECT CtrlFrame NO-ERROR.
                     IF VALID-HANDLE(chIMAGE-Sko) THEN
                         RELEASE OBJECT chIMAGE-Sko NO-ERROR.
                     IF VALID-HANDLE(IMAGE-Sko) THEN
                         DELETE OBJECT IMAGE-Sko NO-ERROR.
                     IF VALID-HANDLE(chTabStrip) THEN
                         RELEASE OBJECT chTabStrip NO-ERROR.
                     IF VALID-HANDLE(chTabs) THEN
                         RELEASE OBJECT chTabs NO-ERROR.
                     IF VALID-HANDLE(chTab) THEN
                         RELEASE OBJECT chTab NO-ERROR.
                     IF VALID-HANDLE(chTab1Side) THEN
                         RELEASE OBJECT chTab1Side NO-ERROR.
                     ASSIGN CtrlFrame   = ?
                            chTabStrip  = ?
                            chTabs      = ?
                            chTab       = ?
                            chTab1Side  = ?
                            chCtrlFrame = ?
                            chIMAGE-Sko = ?.
                     IF AVAILABLE ArtBas THEN RELEASE ArtBas.
                     IF AVAILABLE Strekkode THEN RELEASE Strekkode.
                     IF AVAILABLE ArtPris THEN RELEASE ArtPris.
                     "                     
  &PostDisable_ui = "wModus = 'AVBRYT'." */
}

/* Forslag på løpenummer ver ny og kopiere artikkel */
{syspara.i 2 4 1 wForslagLopNr}
IF NOT CAN-DO("F,N,",wForslagLopNr) THEN
  wForslagLopNr = "".

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.
/* 
 ALT-B, ALT-P reserverat i wKalyle
 */

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

ON ALT-V OF C-ArtKort ANYWHERE 
  DO:
    IF wModus   = "NY" THEN
      RETURN NO-APPLY.

    /* Det er ikke lov å bytte artikkel hvis artikkelen ikke har en ArtPrisPost. */
    FIND ArtBas NO-LOCK WHERE
        RECID(ArtBas) = wArtBasRecid NO-ERROR.
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

    RUN LagreArtBas (0).
    ASSIGN 
        wOldRecid = wArtBasRecid
        cKode     = ""
        .

    /* Kaller søkerutine */
    RUN gvpiartbas.w (
      INPUT-OUTPUT cTekst, /* Returstreng - chr(1) separert */
      "EkstVPILevNr", /* Feltliste avgrensningsfelt (kommaseparert) */
      STRING(CB-EkstVPILev:SCREEN-VALUE IN FRAME default-frame), /* Feltverdier (chr(1) sep) */ 
      cKode, /* Post markøren skal stå på */
      wOldRecid
      ).
    IF RETURN-VALUE = "AVBRYT" THEN
    DO:
        wArtBasRecid = wOldRecid.
        RETURN NO-APPLY.
    END.
    IF NUM-ENTRIES(cTekst,CHR(1)) >= 3 THEN
    DO:
        FIND ArtBas NO-LOCK WHERE
            ArtBas.ArtikkelNr = dec(ENTRY(3,cTekst,CHR(1))) NO-ERROR.
        IF NOT AVAILABLE ArtBas THEN
        DO:
            wArtBasRecid = wOldRecid.
            RETURN NO-APPLY.
        END.
        ELSE
            wArtBasRecid = RECID(ArtBas).
    END.
    RUN HentVis(wArtBasRecid,?).
  END.
ON F2,ALT-S OF hWindow ANYWHERE 
  DO:
    ASSIGN
        hFocus = FOCUS
        .
    IF wModus   = "NY" OR VALID-HANDLE(hParentHandle) THEN DO:
        IF wModus   = "NY" THEN
            MESSAGE "Den nya posten måste lagras först"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.
    IF VALID-HANDLE(hModell) THEN DO:
        RUN EndreKlar IN hModell.
        RETURN.
    END.
    
    /* Det er ikke lov å bytte artikkel hvis artikkelen ikke har en ArtPrisPost. */
    FIND ArtBas NO-LOCK WHERE
        RECID(ArtBas) = wArtBasRecid NO-ERROR.
    IF AVAILABLE ArtBas THEN
    DO:
        IF NOT CAN-FIND(FIRST ArtPris OF ArtBas) THEN
        DO:
            MESSAGE "Det må legges opp en kalkyle på denne artikkelen ("
                     ArtBAs.Vg ArtBAs.LopNr ")."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RUN TvangKalkyle.
            RETURN "AVBRYT".
        END.
    END.

/*     run LagreArtBas (0). */

    ASSIGN 
        wOldRecid = wArtBasRecid
        cKode     = ""
        .
    RUN d-hsok.w (OUTPUT wArtikkelNr,"JA" + CHR(1) + CB-EkstVPILev:SCREEN-VALUE IN FRAME default-frame). /* JA = sök mot vpistrekkode */
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
    ELSE IF wArtikkelNr < 0 AND wArtikkelNr <> ? THEN
    DO:
        ASSIGN
            cKode = RETURN-VALUE
            wOk   = TRUE 
            .
        FIND EkstVPILev NO-LOCK WHERE
            EkstVPILev.EkstVPILevNr = ABS(INT(wArtikkelNr)) NO-ERROR.
        MESSAGE "Artikkel er ikke registrert i lokalt artikkelregister." SKIP(1)
                "Den finnes i VPI register til " + 
                (IF AVAILABLE EkstVPILev
                   THEN EkstVPILev.KortNavn
                   ELSE "* Ukjent VPI leverandør - " + CB-EkstVPILev:SCREEN-VALUE IN FRAME DEFAULT-FRAME + " *") + 
                "." SKIP
                "Skal oppslag gjøres mot VPI registeret?"                
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
            UPDATE wOk.
        IF wOk <> TRUE THEN
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
        /* Bytter søkebegrep hvis det er en strekkode. */
        IF AVAILABLE EkstVPILev THEN
        DO:
            IF NOT CAN-FIND(FIRST VPIArtBas WHERE
                            VPIArtBas.EkstVPILevNr = EkstVPILev.EkstVPILevNr AND
                            VPIArtBas.VareNr = cKode) THEN
            DO:
                FIND VPIStrekkode NO-LOCK WHERE
                    VPIStrekkode.EkstVPILevNr = EkstVPILev.EkstVPILevNr AND
                    VPISTrekkode.Kode = cKode NO-ERROR.
                IF AVAILABLE VPIStrekkode THEN
                    cKode = VPIStrekkode.VareNr.
            END.
        END.

        /* Kaller søkerutine */
        RUN gvpiartbas.w (
          INPUT-OUTPUT cTekst, /* Returstreng - chr(1) separert */
          "EkstVPILevNr", /* Feltliste avgrensningsfelt (kommaseparert) */
          STRING(ABS(INT(wArtikkelNr))), /* Feltverdier (chr(1) sep) */ 
          cKode, /* Post markøren skal stå på */
          wOldRecid
          ).
        IF RETURN-VALUE = "AVBRYT" THEN
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
        IF NUM-ENTRIES(cTekst,CHR(1)) >= 3 THEN
        DO:
            FIND ArtBas NO-LOCK WHERE
                ArtBas.ArtikkelNr = dec(ENTRY(3,cTekst,CHR(1))) NO-ERROR.
            IF NOT AVAILABLE ArtBas THEN
            DO:
                wArtBasRecid = wOldRecid.
                RETURN NO-APPLY.
            END.
            ELSE
                wArtBasRecid = RECID(ArtBas).
        END.
    END.
    ELSE DO:
        FIND ArtBas NO-LOCK WHERE
            ArtBas.ArtikkelNr = wArtikkelNr NO-ERROR.
        IF AVAILABLE ArtBas THEN
            wArtBasRecid = RECID(ArtBas).
        ELSE DO:
            wArtBasRecid = wOldRecid.
            RETURN NO-APPLY.
        END.
    END.
    RUN HentVis(wArtBasRecid,?).
    {swn.i}
  END.

/* Behandler modus */
IF NUM-ENTRIES(wModus,";") > 1 THEN
  ASSIGN
    wBildNr = int(ENTRY(2,wModus,";"))
    wModus  = ENTRY(1,wModus,";").
ELSE
  ASSIGN
    wBildNr = ?.

ON "CTRL-TAB":U ANYWHERE
  DO:
    IF wAktivFlip = 1 THEN DO:
        RUN LagreArtBas (0).
        IF RETURN-VALUE = "AVBRYT" THEN
            RETURN.
    END.
    ASSIGN
      wAktivFlip = wAktivFLip + 1.
    IF wAktivFlip > iAntFlip THEN
      wAktivFlip = 1.
    RUN ByttFrame.
  END.

ON "SHIFT-CTRL-TAB":U ANYWHERE
  DO:
    IF wAktivFlip = 1 THEN DO:
        RUN LagreArtBas (0).
        IF RETURN-VALUE = "AVBRYT" THEN
            RETURN.
    END.
    ASSIGN
      wAktivFlip = wAktivFLip - 1.
    IF wAktivFlip < 1 THEN
      wAktivFlip = 1.
    RUN ByttFrame.
  END.

ASSIGN
  wNyVg                  = ?
  wRS-Vis                = 1. /* Vis Lager */

/* Flagger at systemet kjøres på en LapTop (Innkjøpssystem) */
/* IF VALID-HANDLE(wLibHandle) THEN                  */
/*   RUN SjekkLapTop IN wLibHandle (OUTPUT wLapTop). */

/* Setter sentrallager butikk */
{syspara.i 5 1 1 wCl INT}
FIND clButiker NO-LOCK WHERE
  clButiker.Butik = wCL NO-ERROR.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  hParentHandle = SOURCE-PROCEDURE.

  IF CAN-DO(hParentHandle:INTERNAL-ENTRIES,"getArtKortParam") AND wArtBasRecid = ? THEN 
    RUN getArtKortParam IN hParentHandle (wModus, 
                                          OUTPUT oiLevNr, 
                                          OUTPUT wArtBasRecid
                                          ).

  RUN HentParametre.
  RUN InitTillgButikker.
  RUN enable_UI.
  IF iMellankategori = 1 THEN DO:
      BUTTON-Underkategori:HIDDEN = TRUE.
      CB-Mellankategori:X = S-Underkategori:X.
      CB-MUkat:X          = S-Underkategori:X.
      S-Underkategori:HIDDEN = TRUE.
      Txt-Underkat:HIDDEN = TRUE.
  END.
  ELSE DO:
      CB-Mellankategori:HIDDEN = TRUE.
      CB-MUkat:HIDDEN = TRUE.
  END.
  FIND butiker WHERE Butiker.webbutik NO-LOCK NO-ERROR.
  IF NOT AMBIG butiker THEN
      B-Kobble:HIDDEN = TRUE.
  BROWSE-Lager:SET-REPOSITIONED-ROW(BROWSE-Lager:DOWN,"ALWAYS").
  ASSIGN ArtBas.MatKod:HIDDEN IN FRAME FRAME-ArtInfo   = cVisMaterial = "0"
         BUTTON-SokMaterial:HIDDEN IN FRAME FRAME-ArtInfo  = cVisMaterial = "0"
         ArtBas.Klack:HIDDEN IN FRAME FRAME-ArtInfo    = cVisKlack = "0"
         BUTTON-SokKlak:HIDDEN IN FRAME FRAME-ArtInfo  = cVisKlack = "0"
         ArtBas.inner-id:HIDDEN IN FRAME FRAME-ArtInfo = cVisInnersula = "0"
         BUTTON-SokInner:HIDDEN IN FRAME FRAME-ArtInfo = cVisInnersula = "0"
         ArtBas.ov-id:HIDDEN IN FRAME FRAME-ArtInfo = cVisInnerfor   = "0"
         BUTTON-SokOver:HIDDEN IN FRAME FRAME-ArtInfo = cVisInnerfor = "0"
         ArtBas.slit-id:HIDDEN IN FRAME FRAME-ArtInfo = cVisSlit = "0"
         BUTTON-SokSlit:HIDDEN IN FRAME FRAME-ArtInfo = cVisSlit = "0"
         ArtBas.last-id:HIDDEN IN FRAME FRAME-ArtInfo = cVisLast = "0"
         BUTTON-SokLalst:HIDDEN IN FRAME FRAME-ArtInfo = cVisLast = "0"
         ArtBas.anv-id:HIDDEN IN FRAME FRAME-ArtInfo = cVisBruks = "0"
         BUTTON-SokBruk:HIDDEN IN FRAME FRAME-ArtInfo = cVisBruks = "0"
         ArtBas.BehKode:HIDDEN IN FRAME FRAME-ArtInfo = cVisBehKod = "0"
         BUTTON-SokBehKode:HIDDEN IN FRAME FRAME-ArtInfo = cVisBehKod = "0"
      .
  {lng.i} /* Oversettelse */

  RUN InitCbEkstVPILev.
  RUN InitCB.
  RUN InitVareslag.
  RUN Toolbar.
  {syspara.i 2 4 9  ArtBas.Etikett:LIST-ITEM-PAIRS}
  
  IF CAN-FIND(ArtBas WHERE RECID(ArtBas) = wArtBasRecid) THEN
    DO:
      RUN VisArtBas.
      {&OPEN-QUERY-BROWSE-Statistikk}
    END.
  RUN ByttFrame. /* Legger opp f›rste fane. */
  ASSIGN chTab1Side = chCtrlFrame:TabStrip. /* COM-Handl 1. side */
  
  IF wModus = "NY" THEN 
    DO:
      /*-- assign ArtBas.StrTypeId:sensitive = true. --*/
      button-Ny:SENSITIVE = TRUE.
      APPLY "CHOOSE":U TO BUTTON-Ny.
      button-Ny:SENSITIVE = FALSE.
      IF oiLevNr NE 0 THEN DO:
        FIND LevBas WHERE LevBas.levnr = oiLevNr NO-LOCK NO-ERROR.
        IF AVAIL LevBas THEN 
          ASSIGN ArtBas.LevNr:SCREEN-VALUE = STRING(oiLevNr)
                 FILL-IN-LevNamn:SCREEN-VALUE = LevBas.levnamn
                 .
      END.
    END.
  /*---  
  else
    assign ArtBas.StrTypeId:sensitive = false.
  ---*/
  IF cParaAktivFlip <> "" THEN
      RUN SetAktivFlik (INT(cParaAktivFlip)).
  ASSIGN
    C-ArtKort:HIDDEN = FALSE.
  
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.

  /* Sikrer at det ikke er transaksjon aktiv her */
  IF AVAILABLE ArtBas THEN
    FIND CURRENT artbas NO-LOCK NO-ERROR.
    
  IF wModus <> "NY" THEN
    FIND ArtBas NO-LOCK WHERE 
      RECID(ArtBas) = wArtBasRecid NO-ERROR.
  /* Retur verdi */  
  IF AVAILABLE ArtBAs AND wModus <> "NY" THEN
    RETURN STRING(RECID(ArtBas)).
  ELSE 
    RETURN "AVBRYT".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AdjustLagerCol C-ArtKort 
PROCEDURE AdjustLagerCol :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER iBredast AS INTEGER     NO-UNDO.
  DEFINE VARIABLE wh AS WIDGET      NO-UNDO.

  DO WITH FRAME FRAME-Lager:
      wh = BROWSE-Lager:FIRST-COLUMN.
         DO WHILE VALID-HANDLE(wh):
             IF TRIM(wh:NAME) BEGINS "Antal" THEN wh:WIDTH-PIXELS = iBredast.
/*              IF TRIM(wh:NAME) BEGINS "Antal" THEN wh:WIDTH = 5. */
          ASSIGN wh = wh:NEXT-COLUMN.
       END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ApplyButtonKopier C-ArtKort 
PROCEDURE ApplyButtonKopier :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
APPLY "choose" TO BUTTON-Kopier IN FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ApplyButtonNy C-ArtKort 
PROCEDURE ApplyButtonNy :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
APPLY "choose" TO BUTTON-Ny IN FRAME {&FRAME-NAME}.
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
  DEFINE OUTPUT PARAMETER iBredast AS INTEGER     NO-UNDO.
  DEF VAR   wLoop               AS INT  NO-UNDO.
  DEF VAR   wStorl              AS CHAR NO-UNDO.
  DEF VAR   iColColor           AS INTE EXTENT 99 INIT ? NO-UNDO.
  IF dSkomodus THEN
      iBredast = MAX(iBredast,FONT-TABLE:GET-TEXT-WIDTH-PIXELS("9999",BROWSE BROWSE-Lager:FIRST-COLUMN:FONT)).
  DO wLoop = 1 TO 98:
    IF NUM-ENTRIES(wStrListe,";") >= wLoop 
      THEN wStorl = ENTRY(wLoop,wStrListe,";") + "  ".
    ELSE wStorl = "".
    IF wStorl BEGINS "(" THEN DO:
        iColColor[wLoop] = 12.
/*         wStorl = TRIM(wStorl).     */
/*         wStorl = TRIM(wStorl,"("). */
/*         wStorl = TRIM(wStorl,")"). */
    END.
    IF NOT dSkoModus THEN 
        ASSIGN
        wStorl = FILL(" ",10 - length(TRIM(wStorl))) + wStorl.
    ELSE DO:
        iBredast = MAX(iBredast,FONT-TABLE:GET-TEXT-WIDTH-PIXELS(TRIM(wStorl),BROWSE BROWSE-Lager:FIRST-COLUMN:FONT)).
    END.
    CASE wLoop:
      WHEN  1 THEN ASSIGN tmpLager.Antall[ 1]:label IN BROWSE BROWSE-Lager = wStorl 
                          tmpLager.Antall[ 1]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[ 1].
      WHEN  2 THEN ASSIGN tmpLager.Antall[ 2]:label IN BROWSE BROWSE-Lager = wStorl 
                          tmpLager.Antall[ 2]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[ 2].
      WHEN  3 THEN ASSIGN tmpLager.Antall[ 3]:label IN BROWSE BROWSE-Lager = wStorl 
                          tmpLager.Antall[ 3]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[ 3].
      WHEN  4 THEN ASSIGN tmpLager.Antall[ 4]:label IN BROWSE BROWSE-Lager = wStorl 
                          tmpLager.Antall[ 4]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[ 4].
      WHEN  5 THEN ASSIGN tmpLager.Antall[ 5]:label IN BROWSE BROWSE-Lager = wStorl 
                          tmpLager.Antall[ 5]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[ 5].
      WHEN  6 THEN ASSIGN tmpLager.Antall[ 6]:label IN BROWSE BROWSE-Lager = wStorl 
                          tmpLager.Antall[ 6]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[ 6].
      WHEN  7 THEN ASSIGN tmpLager.Antall[ 7]:label IN BROWSE BROWSE-Lager = wStorl 
                          tmpLager.Antall[ 7]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[ 7].
      WHEN  8 THEN ASSIGN tmpLager.Antall[ 8]:label IN BROWSE BROWSE-Lager = wStorl 
                          tmpLager.Antall[ 8]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[ 8].
      WHEN  9 THEN ASSIGN tmpLager.Antall[ 9]:label IN BROWSE BROWSE-Lager = wStorl 
                          tmpLager.Antall[ 9]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[ 9].
      WHEN 10 THEN ASSIGN tmpLager.Antall[10]:label IN BROWSE BROWSE-Lager = wStorl 
                          tmpLager.Antall[10]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[10].
      WHEN 11 THEN ASSIGN tmpLager.Antall[11]:label IN BROWSE BROWSE-Lager = wStorl 
                          tmpLager.Antall[11]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[11].
      WHEN 12 THEN ASSIGN tmpLager.Antall[12]:label IN BROWSE BROWSE-Lager = wStorl 
                          tmpLager.Antall[12]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[12].
      WHEN 13 THEN ASSIGN tmpLager.Antall[13]:label IN BROWSE BROWSE-Lager = wStorl 
                          tmpLager.Antall[13]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[13].
      WHEN 14 THEN ASSIGN tmpLager.Antall[14]:label IN BROWSE BROWSE-Lager = wStorl 
                          tmpLager.Antall[14]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[14].
      WHEN 15 THEN ASSIGN tmpLager.Antall[15]:label IN BROWSE BROWSE-Lager = wStorl 
                          tmpLager.Antall[15]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[15].
      WHEN 16 THEN ASSIGN tmpLager.Antall[16]:label IN BROWSE BROWSE-Lager = wStorl 
                          tmpLager.Antall[16]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[16].
      WHEN 17 THEN ASSIGN tmpLager.Antall[17]:label IN BROWSE BROWSE-Lager = wStorl 
                          tmpLager.Antall[17]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[17].
      WHEN 18 THEN ASSIGN tmpLager.Antall[18]:label IN BROWSE BROWSE-Lager = wStorl 
                          tmpLager.Antall[18]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[18].
      WHEN 19 THEN ASSIGN tmpLager.Antall[19]:label IN BROWSE BROWSE-Lager = wStorl 
                          tmpLager.Antall[19]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[19].
      WHEN 20 THEN ASSIGN tmpLager.Antall[20]:label IN BROWSE BROWSE-Lager = wStorl 
                          tmpLager.Antall[20]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[20].
      WHEN 21 THEN ASSIGN tmpLager.Antall[21]:label IN BROWSE BROWSE-Lager = wStorl 
                          tmpLager.Antall[21]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[21].
      WHEN 22 THEN ASSIGN tmpLager.Antall[22]:label IN BROWSE BROWSE-Lager = wStorl 
                          tmpLager.Antall[22]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[22].
      WHEN 23 THEN ASSIGN tmpLager.Antall[23]:label IN BROWSE BROWSE-Lager = wStorl 
                          tmpLager.Antall[23]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[23].
      WHEN 24 THEN ASSIGN tmpLager.Antall[24]:label IN BROWSE BROWSE-Lager = wStorl 
                          tmpLager.Antall[24]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[24].
      WHEN 25 THEN ASSIGN tmpLager.Antall[25]:label IN BROWSE BROWSE-Lager = wStorl 
                          tmpLager.Antall[25]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[25].
      WHEN 26 THEN ASSIGN tmpLager.Antall[26]:label IN BROWSE BROWSE-Lager = wStorl 
                          tmpLager.Antall[26]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[26].
      WHEN 27 THEN ASSIGN tmpLager.Antall[27]:label IN BROWSE BROWSE-Lager = wStorl 
                          tmpLager.Antall[27]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[27].
      WHEN 28 THEN ASSIGN tmpLager.Antall[28]:label IN BROWSE BROWSE-Lager = wStorl 
                          tmpLager.Antall[28]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[28].
      WHEN 29 THEN ASSIGN tmpLager.Antall[29]:label IN BROWSE BROWSE-Lager = wStorl 
                          tmpLager.Antall[29]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[29].
      WHEN 30 THEN ASSIGN tmpLager.Antall[30]:label IN BROWSE BROWSE-Lager = wStorl
                          tmpLager.Antall[30]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[30].
      WHEN 31 THEN ASSIGN tmpLager.Antall[31]:label IN BROWSE BROWSE-Lager = wStorl
                          tmpLager.Antall[31]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[31].
      WHEN 32 THEN ASSIGN tmpLager.Antall[32]:label IN BROWSE BROWSE-Lager = wStorl
                          tmpLager.Antall[32]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[32].
      WHEN 33 THEN ASSIGN tmpLager.Antall[33]:label IN BROWSE BROWSE-Lager = wStorl
                          tmpLager.Antall[33]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[33].
      WHEN 34 THEN ASSIGN tmpLager.Antall[34]:label IN BROWSE BROWSE-Lager = wStorl
                          tmpLager.Antall[34]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[34].
      WHEN 35 THEN ASSIGN tmpLager.Antall[35]:label IN BROWSE BROWSE-Lager = wStorl
                          tmpLager.Antall[35]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[35].
      WHEN 36 THEN ASSIGN tmpLager.Antall[36]:label IN BROWSE BROWSE-Lager = wStorl
                          tmpLager.Antall[36]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[36].
      WHEN 37 THEN ASSIGN tmpLager.Antall[37]:label IN BROWSE BROWSE-Lager = wStorl
                          tmpLager.Antall[37]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[37].
      WHEN 38 THEN ASSIGN tmpLager.Antall[38]:label IN BROWSE BROWSE-Lager = wStorl
                          tmpLager.Antall[38]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[38].
      WHEN 39 THEN ASSIGN tmpLager.Antall[39]:label IN BROWSE BROWSE-Lager = wStorl
                          tmpLager.Antall[39]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[39].
      WHEN 40 THEN ASSIGN tmpLager.Antall[40]:label IN BROWSE BROWSE-Lager = wStorl
                          tmpLager.Antall[40]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[40].
      WHEN 41 THEN ASSIGN tmpLager.Antall[41]:label IN BROWSE BROWSE-Lager = wStorl
                          tmpLager.Antall[41]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[41].
      WHEN 42 THEN ASSIGN tmpLager.Antall[42]:label IN BROWSE BROWSE-Lager = wStorl
                          tmpLager.Antall[42]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[42].
      WHEN 43 THEN ASSIGN tmpLager.Antall[43]:label IN BROWSE BROWSE-Lager = wStorl
                          tmpLager.Antall[43]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[43].
      WHEN 44 THEN ASSIGN tmpLager.Antall[44]:label IN BROWSE BROWSE-Lager = wStorl
                          tmpLager.Antall[44]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[44].
      WHEN 45 THEN ASSIGN tmpLager.Antall[45]:label IN BROWSE BROWSE-Lager = wStorl
                          tmpLager.Antall[45]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[45].
      WHEN 46 THEN ASSIGN tmpLager.Antall[46]:label IN BROWSE BROWSE-Lager = wStorl
                          tmpLager.Antall[46]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[46].
      WHEN 47 THEN ASSIGN tmpLager.Antall[47]:label IN BROWSE BROWSE-Lager = wStorl
                          tmpLager.Antall[47]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[47].
      WHEN 48 THEN ASSIGN tmpLager.Antall[48]:label IN BROWSE BROWSE-Lager = wStorl
                          tmpLager.Antall[48]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[48].
      WHEN 49 THEN ASSIGN tmpLager.Antall[49]:label IN BROWSE BROWSE-Lager = wStorl
                          tmpLager.Antall[49]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[49].
      
        WHEN 50 THEN ASSIGN tmpLager.Antall[50]:label IN BROWSE BROWSE-Lager = wStorl
                            tmpLager.Antall[50]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[50].
        WHEN 51 THEN ASSIGN tmpLager.Antall[51]:label IN BROWSE BROWSE-Lager = wStorl
                            tmpLager.Antall[51]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[51].
        WHEN 52 THEN ASSIGN tmpLager.Antall[52]:label IN BROWSE BROWSE-Lager = wStorl
                            tmpLager.Antall[52]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[52].
        WHEN 53 THEN ASSIGN tmpLager.Antall[53]:label IN BROWSE BROWSE-Lager = wStorl
                            tmpLager.Antall[53]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[53].
        WHEN 54 THEN ASSIGN tmpLager.Antall[54]:label IN BROWSE BROWSE-Lager = wStorl
                            tmpLager.Antall[54]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[54].
        WHEN 55 THEN ASSIGN tmpLager.Antall[55]:label IN BROWSE BROWSE-Lager = wStorl
                            tmpLager.Antall[55]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[55].
        WHEN 56 THEN ASSIGN tmpLager.Antall[56]:label IN BROWSE BROWSE-Lager = wStorl
                            tmpLager.Antall[56]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[56].
        WHEN 57 THEN ASSIGN tmpLager.Antall[57]:label IN BROWSE BROWSE-Lager = wStorl
                            tmpLager.Antall[57]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[57].
        WHEN 58 THEN ASSIGN tmpLager.Antall[58]:label IN BROWSE BROWSE-Lager = wStorl
                            tmpLager.Antall[58]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[58].
        WHEN 59 THEN ASSIGN tmpLager.Antall[59]:label IN BROWSE BROWSE-Lager = wStorl
                            tmpLager.Antall[59]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[59].
          
    
        WHEN 60 THEN ASSIGN tmpLager.Antall[60]:label IN BROWSE BROWSE-Lager = wStorl
                            tmpLager.Antall[60]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[60].
        WHEN 61 THEN ASSIGN tmpLager.Antall[61]:label IN BROWSE BROWSE-Lager = wStorl
                            tmpLager.Antall[61]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[61].
        WHEN 62 THEN ASSIGN tmpLager.Antall[62]:label IN BROWSE BROWSE-Lager = wStorl
                            tmpLager.Antall[62]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[62].
        WHEN 63 THEN ASSIGN tmpLager.Antall[63]:label IN BROWSE BROWSE-Lager = wStorl
                            tmpLager.Antall[63]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[63].
        WHEN 64 THEN ASSIGN tmpLager.Antall[64]:label IN BROWSE BROWSE-Lager = wStorl
                            tmpLager.Antall[64]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[64].
        WHEN 66 THEN ASSIGN tmpLager.Antall[66]:label IN BROWSE BROWSE-Lager = wStorl
                            tmpLager.Antall[66]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[66].
        WHEN 66 THEN ASSIGN tmpLager.Antall[66]:label IN BROWSE BROWSE-Lager = wStorl
                            tmpLager.Antall[66]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[66].
        WHEN 67 THEN ASSIGN tmpLager.Antall[67]:label IN BROWSE BROWSE-Lager = wStorl
                            tmpLager.Antall[67]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[67].
        WHEN 68 THEN ASSIGN tmpLager.Antall[68]:label IN BROWSE BROWSE-Lager = wStorl
                            tmpLager.Antall[68]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[68].
        WHEN 69 THEN ASSIGN tmpLager.Antall[69]:label IN BROWSE BROWSE-Lager = wStorl
                            tmpLager.Antall[69]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[69].

    WHEN 70 THEN ASSIGN tmpLager.Antall[70]:label IN BROWSE BROWSE-Lager = wStorl
                        tmpLager.Antall[70]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[70].
    WHEN 71 THEN ASSIGN tmpLager.Antall[71]:label IN BROWSE BROWSE-Lager = wStorl
                        tmpLager.Antall[71]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[71].
    WHEN 72 THEN ASSIGN tmpLager.Antall[72]:label IN BROWSE BROWSE-Lager = wStorl
                        tmpLager.Antall[72]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[72].
    WHEN 73 THEN ASSIGN tmpLager.Antall[73]:label IN BROWSE BROWSE-Lager = wStorl
                        tmpLager.Antall[73]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[73].
    WHEN 74 THEN ASSIGN tmpLager.Antall[74]:label IN BROWSE BROWSE-Lager = wStorl
                        tmpLager.Antall[74]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[74].
    WHEN 77 THEN ASSIGN tmpLager.Antall[77]:label IN BROWSE BROWSE-Lager = wStorl
                        tmpLager.Antall[77]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[77].
    WHEN 76 THEN ASSIGN tmpLager.Antall[76]:label IN BROWSE BROWSE-Lager = wStorl
                        tmpLager.Antall[76]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[76].
    WHEN 77 THEN ASSIGN tmpLager.Antall[77]:label IN BROWSE BROWSE-Lager = wStorl
                        tmpLager.Antall[77]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[77].
    WHEN 78 THEN ASSIGN tmpLager.Antall[78]:label IN BROWSE BROWSE-Lager = wStorl
                        tmpLager.Antall[78]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[78].
    WHEN 79 THEN ASSIGN tmpLager.Antall[79]:label IN BROWSE BROWSE-Lager = wStorl
                        tmpLager.Antall[79]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[79].

        WHEN 80 THEN ASSIGN tmpLager.Antall[80]:label IN BROWSE BROWSE-Lager = wStorl
                            tmpLager.Antall[80]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[80].
        WHEN 81 THEN ASSIGN tmpLager.Antall[81]:label IN BROWSE BROWSE-Lager = wStorl
                            tmpLager.Antall[81]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[81].
        WHEN 82 THEN ASSIGN tmpLager.Antall[82]:label IN BROWSE BROWSE-Lager = wStorl
                            tmpLager.Antall[82]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[82].
        WHEN 83 THEN ASSIGN tmpLager.Antall[83]:label IN BROWSE BROWSE-Lager = wStorl
                            tmpLager.Antall[83]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[83].
        WHEN 84 THEN ASSIGN tmpLager.Antall[84]:label IN BROWSE BROWSE-Lager = wStorl
                            tmpLager.Antall[84]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[84].
        WHEN 88 THEN ASSIGN tmpLager.Antall[88]:label IN BROWSE BROWSE-Lager = wStorl
                            tmpLager.Antall[88]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[88].
        WHEN 86 THEN ASSIGN tmpLager.Antall[86]:label IN BROWSE BROWSE-Lager = wStorl
                            tmpLager.Antall[86]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[86].
        WHEN 88 THEN ASSIGN tmpLager.Antall[88]:label IN BROWSE BROWSE-Lager = wStorl
                            tmpLager.Antall[88]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[88].
        WHEN 88 THEN ASSIGN tmpLager.Antall[88]:label IN BROWSE BROWSE-Lager = wStorl
                            tmpLager.Antall[88]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[88].
        WHEN 89 THEN ASSIGN tmpLager.Antall[89]:label IN BROWSE BROWSE-Lager = wStorl
                            tmpLager.Antall[89]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[89].

        WHEN 90 THEN ASSIGN tmpLager.Antall[90]:label IN BROWSE BROWSE-Lager = wStorl
                            tmpLager.Antall[90]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[90].
        WHEN 91 THEN ASSIGN tmpLager.Antall[91]:label IN BROWSE BROWSE-Lager = wStorl
                            tmpLager.Antall[91]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[91].
        WHEN 92 THEN ASSIGN tmpLager.Antall[92]:label IN BROWSE BROWSE-Lager = wStorl
                            tmpLager.Antall[92]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[92].
        WHEN 93 THEN ASSIGN tmpLager.Antall[93]:label IN BROWSE BROWSE-Lager = wStorl
                            tmpLager.Antall[93]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[93].
        WHEN 94 THEN ASSIGN tmpLager.Antall[94]:label IN BROWSE BROWSE-Lager = wStorl
                            tmpLager.Antall[94]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[94].
        WHEN 99 THEN ASSIGN tmpLager.Antall[99]:label IN BROWSE BROWSE-Lager = wStorl
                            tmpLager.Antall[99]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[99].
        WHEN 96 THEN ASSIGN tmpLager.Antall[96]:label IN BROWSE BROWSE-Lager = wStorl
                            tmpLager.Antall[96]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[96].
        WHEN 99 THEN ASSIGN tmpLager.Antall[99]:label IN BROWSE BROWSE-Lager = wStorl
                            tmpLager.Antall[99]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[99].
        WHEN 99 THEN ASSIGN tmpLager.Antall[99]:label IN BROWSE BROWSE-Lager = wStorl
                            tmpLager.Antall[99]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[99].
        WHEN 99 THEN ASSIGN tmpLager.Antall[99]:label IN BROWSE BROWSE-Lager = wStorl
                            tmpLager.Antall[99]:LABEL-FGCOLOR IN BROWSE BROWSE-Lager = iColColor[99].

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
  
  DO:
    ASSIGN wModus = "ENDRE".
    IF AVAIL ArtBas AND ArtBas.Lopnr = ? THEN
        FIND NEXT ArtBas USE-INDEX Artikkelnr NO-LOCK NO-ERROR.
    ELSE 
        FIND NEXT Artbas NO-LOCK NO-ERROR.
    IF NOT AVAILABLE ArtBas THEN
       FIND LAST ArtBas NO-LOCK NO-ERROR.

    IF AVAILABLE ArtBas THEN
      DO:
        ASSIGN
          wArtBasRecid = RECID(ArtBas).
        PUBLISH "ByttObjekt" (ArtBas.ArtikkelNr).
        RUN ByttFrame. /* For † enable knapper og felt korrekt */ 
        RUN RensFrame. /* Tar bort gammel d.... */
        RUN VisArtBas.
        IF wAktivFlip = 3 THEN
        DO:
            IF VALID-HANDLE(hStrekkode) THEN
              RUN SetEntry IN hStrekkode.
        END.
        ELSE IF wAktivFlip = 4 THEN
        DO:
            IF VALID-HANDLE(hPakkeLinje) THEN
              RUN SetEntry IN hPakkeLinje.
        END.
        ELSE IF wAktivFlip = 5 THEN
        DO:
            IF VALID-HANDLE(hErstattning) THEN
              RUN SetEntry IN hErstattning.
        END.
        ELSE IF wAktivFlip = 6 THEN DO:
            IF VALID-HANDLE(hIndivid) THEN
              RUN SetEntry IN hIndivid.
        END.
        ELSE IF wAktivFlip = 7 THEN DO:
            IF VALID-HANDLE(hBestilling) THEN
              RUN SetEntry IN hBestilling.
        END.
        ELSE IF wAktivFlip = 8 THEN
            DO:
              APPLY "entry":U TO BROWSE BROWSE-Lager.
            END.

        ELSE IF wAktivFlip = 9 THEN DO:
            IF VALID-HANDLE(hArtBestPkt) THEN
              RUN SetEntry IN hArtBestPkt.
        END.
        ELSE IF wAktivFlip = 10 THEN
        DO:
            IF VALID-HANDLE(hTranslogg) THEN
              RUN SetEntry IN hTranslogg.
        END.
        ELSE IF wAktivFlip = 11 THEN
        DO:
            IF VALID-HANDLE(wHistorikk) THEN
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
/*   if wModus = "ENDRE" then */
  IF wModus = "ENDRE" AND AVAIL ArtBas THEN DO:
      RUN LagreArtBas (1).
      IF RETURN-VALUE = "AVBRYT" THEN
          RETURN NO-APPLY.
  END.
  BUTTON-Ny:PRIVATE-DATA IN FRAME DEFAULT-FRAME = "".
  /* Bytter til f›rste fane. */
  DO WITH FRAME DEFAULT-FRAME:
    RUN gartslag.w.        
    iArtSlag = int(RETURN-VALUE) NO-ERROR.       
    IF ERROR-STATUS:ERROR OR RETURN-VALUE = "AVBRYT" THEN
        RETURN "AVBRYT".

    DO:
      RUN SettEnableDisable.
      RUN SettDefaultVerdier (INPUT INT(RETURN-VALUE)).
      ASSIGN 
          CB-ModellFarge:LIST-ITEM-PAIRS = ","
          CB-ModellFarge:SENSITIVE = FALSE
          ArtBas.LevNr:SENSITIVE IN FRAME default-frame = TRUE
          BUTTON-SokLev:SENSITIVE IN FRAME default-frame = TRUE
          wAktivFlip = 1 
          TOGGLE-Annonse = FALSE
          /* ArtBas.Pakke:SENSITIVE IN FRAME default-frame = TRUE */
          wModus   = "NY".
          chTab1Side = chTabs:Item (wAktivFlip BY-VARIANT-POINTER).
          chTabStrip:SelectedItem = chTab1Side
          .
      END.
      RUN ByttFrame.
      RUN RensFrame.
      RUN BlankLager.
      RUN Toolbar.
      ASSIGN chTabStrip:ENABLED = FALSE.
      APPLY "ENTRY":U TO ArtBas.Vg IN FRAME DEFAULT-FRAME.
    END.

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
  
  DO:
    ASSIGN wModus = "ENDRE".
    IF Artbas.Lopnr = ? THEN
        FIND PREV ArtBas USE-INDEX Artikkelnr.
    ELSE
        FIND PREV ArtBas NO-LOCK NO-ERROR.    
    IF NOT AVAILABLE ArtBas THEN
       FIND FIRST ArtBas NO-LOCK NO-ERROR.

    IF AVAILABLE ArtBas THEN DO:
        ASSIGN wArtBasRecid = RECID(ArtBas).
        PUBLISH "ByttObjekt" (ArtBas.ArtikkelNr).
        RUN ByttFrame. /* For † enable knapper og felt korrekt */
        RUN RensFrame. /* Tar bort gammel d.... */
        RUN VisArtBas.
        IF wAktivFlip = 3 THEN DO:
            IF VALID-HANDLE(hStrekkode) THEN
              RUN SetEntry IN hStrekkode.
        END.
        ELSE IF wAktivFlip = 4 THEN DO:
            IF VALID-HANDLE(hPakkeLinje) THEN
              RUN SetEntry IN hPakkeLinje.
        END.
        ELSE IF wAktivFlip = 5 THEN DO:
            IF VALID-HANDLE(hErstattning) THEN
              RUN SetEntry IN hErstattning.
        END.
        ELSE IF wAktivFlip = 6 THEN DO:                
              IF VALID-HANDLE(hIndivid) THEN
                RUN SetEntry IN hIndivid.
        END.
        ELSE IF wAktivFlip = 7 THEN DO:                
              IF VALID-HANDLE(hBestilling) THEN
                RUN SetEntry IN hBestilling.
        END.
        ELSE IF wAktivFlip = 8 THEN DO:
              APPLY "entry":U TO BROWSE BROWSE-Lager.
        END.
        ELSE IF wAktivFlip = 9 THEN DO:                
              IF VALID-HANDLE(hArtBestPkt) THEN
                RUN SetEntry IN hArtBestPkt.
        END.
        ELSE IF wAktivFlip = 10 THEN DO:
            IF VALID-HANDLE(hTranslogg) THEN
              RUN SetEntry IN hTranslogg.
        END.
        ELSE IF wAktivFlip = 11 THEN DO:
            IF VALID-HANDLE(wHistorikk) THEN
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
    RECID(bArtBas) = recid(ArtBas) NO-ERROR.
  FIND NEXT bArtBas NO-LOCK NO-ERROR.
  IF NOT AVAILABLE bArtBas THEN
    FIND FIRST bArtBas.
  IF AVAILABLE bArtBas THEN
    ASSIGN
      wNesteRecid = RECID(bArtBas).

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
    FIND ArtBas NO-LOCK WHERE
        RECID(ArtBas) = wArtBasRecid NO-ERROR.
    IF AVAIL ArtBas THEN
        PUBLISH "ByttObjekt" (ArtBas.ArtikkelNr).
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
  RUN ByttFrame. /* For † enable knapper og felt korrekt */ 
  RUN RensFrame.
  RUN VisArtBas.
  RUN VisLager(TRUE).
  PUBLISH "ByttObjekt" (ArtBas.ArtikkelNr).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Byttframe C-ArtKort 
PROCEDURE Byttframe :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR wFrameHandle AS HANDLE NO-UNDO.
   DEF VAR wFirst-Child AS HANDLE NO-UNDO.
   DEFINE VARIABLE iTag AS INTEGER    NO-UNDO.
   DEFINE VARIABLE cTag AS CHARACTER  NO-UNDO.

   ASSIGN CURRENT-WINDOW = THIS-PROCEDURE:CURRENT-WINDOW NO-ERROR.

   /* cAktivFlipTag innehåller en lista över Tags  */
   IF wModus = "NY" AND wAktivFlip <> 1 THEN
     DO:
       wAktivFlip = 1.
       MESSAGE Tx("Artikkelinformasjonen må lagres først!",106)
         VIEW-AS ALERT-BOX MESSAGE TITLE Tx("Melding",107).
     END.
   
   ELSE IF NOT AVAILABLE ArtBas THEN 
     RETURN.
     
   ASSIGN hAktivHandle = ?.
   /* iTag är ursprungliga foldertabben */
   cTag = ENTRY(wAktivFlip,cAktivFlipTag).
   iTag = INT(SUBSTRING(cTag,2)).
   /* Info frame som alltid ligger oppe. */  /* !!! 01062009 !!! */
   IF NOT CAN-DO("3,4,5,6,7,9,10,11",STRING(iTag)) THEN
     IF FRAME DEFAULT-FRAME:MOVE-TO-TOP() THEN.
/*    IF NOT can-do("2,3,4,5,6,7,9,10",string(iTag)) THEN */
/*      IF FRAME DEFAULT-FRAME:MOVE-TO-TOP() THEN.        */

   /* Er individvinduet oppe og det byttes flik, skal dette bort. */
/*  !! old ver april 2008    IF VALID-HANDLE(hIndivid) AND not can-do("6",string(wAktivFlip)) THEN */
    IF VALID-HANDLE(hIndivid) AND NOT CAN-DO("6",STRING(iTag)) THEN
         RUN LukkIndDet IN hIndivid.
      
   /* Bytter FLik */
/*    IF chTabs:ITEM <> wAktivFlip THEN DO: */
    chTab1Side = chTabs:Item (wAktivFlip BY-VARIANT-POINTER).
    chTabStrip:SelectedItem = chTab1Side.
/*    END. */
   /* Bytter frame.                                                  */
   /* NB: Dette gjøres også på OCX'en når man klikker på aktiv flik. */
   /*     Dvs det må vedlikeholdes kode også der.                    */
/*  !! old ver april 2008  CASE wAktivFlip: */
       IF wAktivflip = 1 THEN
           FRAME FRAME-ArtInfo:MOVE-TO-TOP().
   CASE iTag:
      WHEN 1 THEN 
        DO WITH FRAME DEFAULT-FRAME:
          FRAME FRAME-ArtInfo:MOVE-TO-TOP().
          IF ArtBas.Vg:Sensitive = TRUE 
            THEN APPLY "entry" TO ArtBas.Vg IN FRAME DEFAULT-FRAME.
          ELSE APPLY "entry" TO ArtBas.Beskr IN FRAME DEFAULT-FRAME.
        END.        
       WHEN 2 THEN DO:
           FRAME FRAME-Info2:MOVE-TO-TOP().
/*            RUN VisLager (NO).               */
/*            APPLY "ENTRY":U TO BROWSE-Lager. */
        END.
      WHEN 3 THEN DO:
           IF AVAILABLE ArtBas THEN DO:
              IF VALID-HANDLE(wKalkyle) THEN
                  RUN MoveToTopp IN wKalkyle.
              ELSE         
                RUN w-kalkyle.w PERSISTENT SET wKalkyle (ArtBas.ArtikkelNr,THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE,h_PrisKo,wModus).  
              ASSIGN hAktivHandle = wKalkyle.
           END.
       END.
       WHEN 4 THEN STREKKODE: DO:
           IF AVAILABLE ArtBas THEN DO:
              IF VALID-HANDLE(hStrekkode) THEN
                  RUN MoveToTopp IN hStrekkode.
              ELSE         
                  RUN w-strekkode.w PERSISTENT SET hStrekkode (ArtBas.ArtikkelNr,THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE).  
              ASSIGN hAktivHandle = hStrekkode.
           END.
       END. /* STREKKODE */
       WHEN 5 THEN PAKKE: DO:
           IF AVAILABLE ArtBas THEN DO:
              IF VALID-HANDLE(hPakkelinje) THEN
                  RUN MoveToTopp IN hPakkelinje.
              ELSE         
                  RUN w-Pakkelinje.w PERSISTENT SET hPakkelinje (ArtBas.ArtikkelNr,THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE).
              ASSIGN hAktivHandle = hPakkelinje.
           END.
       END. /* PAKKE */
       WHEN 6 THEN ERSTATT: DO:
           IF AVAILABLE ArtBas THEN DO:
              IF VALID-HANDLE(hErstattning) THEN DO:
                  RUN MoveToTopp IN hErstattning.
              END.
              ELSE         
                  RUN w-Erstattningsvare.w PERSISTENT SET hErstattning (ArtBas.ArtikkelNr,THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE).
              ASSIGN hAktivHandle = hErstattning. 
          END.
      END. /* ERSTATT */
       WHEN 7 THEN INDIVID: DO:
           IF AVAILABLE ArtBas THEN DO:
              IF VALID-HANDLE(hIndivid) THEN DO:
                  RUN MoveToTopp IN hIndivid.
              END.
              ELSE         
                  RUN w-Individvare.w PERSISTENT SET hIndivid (ArtBas.ArtikkelNr,THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE).
              ASSIGN hAktivHandle = hIndivid.
          END.
      END. /* INDIVID */
      WHEN 8 THEN DO:
          IF AVAILABLE ArtBas THEN DO:
             IF VALID-HANDLE(hBestilling) THEN DO:
                 RUN MoveToTopp IN hBestilling.
             END.
             ELSE         
                 RUN w-Bestilling.w PERSISTENT SET hBestilling (ArtBas.ArtikkelNr,THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE).

             ASSIGN hAktivHandle = hBestilling.
         END.
      END.
      WHEN 9 THEN DO:

          IF FRAME DEFAULT-FRAME:MOVE-TO-TOP() THEN.
          FRAME FRAME-Lager:MOVE-TO-TOP().
          RUN VisLager (?).
          APPLY "ENTRY":U TO BROWSE-Lager.
      END.
      WHEN 10 THEN BESTPKT: DO:
          IF AVAILABLE ArtBas THEN DO:
             IF VALID-HANDLE(hArtBestPkt) THEN DO:
                 RUN MoveToTopp IN hArtBestPkt.
             END.
             ELSE DO:         
                 RUN w-artbestpkt.w PERSISTENT SET hArtBestPkt (ArtBas.ArtikkelNr,THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE).
                 RUN OpenQueryArtBestPkt IN hArtBestPkt.
             END.

             ASSIGN hAktivHandle = hArtBestPkt.
         END.
      END.
      WHEN 11 THEN TRANSLOGG: DO:
          IF AVAILABLE ArtBas THEN DO:
             IF VALID-HANDLE(hTranslogg) THEN DO:
                      RUN MoveToTopp IN hTranslogg.
                      RUN ByttObjekt IN hTranslogg (ArtBas.ArtikkelNr).  
                      RUN SetEntry IN hTranslogg.
             END.
             ELSE         
                 RUN w-barttranslogg.w PERSISTENT SET hTranslogg (ArtBas.ArtikkelNr,THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE).
             ASSIGN hAktivHandle = hTranslogg.
          END.
      END. /* TRANSLOGG */
      WHEN 12 THEN HISTORIKK: DO:
          IF VALID-HANDLE(wHistorikk) THEN
              RUN MoveToTopp IN wHistorikk.
          IF AVAILABLE ArtBas THEN DO:
              IF VALID-HANDLE(wHistorikk) THEN
                RUN ByttObjekt IN wHistorikk (STRING(ArtBas.ArtikkelNr,"9999999999999")).  
              ELSE         
                RUN w-stlinje.w PERSISTENT SET wHistorikk (wArtBasRecid,STRING(ArtBas.ArtikkelNr,"9999999999999"),"ARTIKKEL","MANED",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE).  
              ASSIGN hAktivHandle = wHistorikk.
          END.
       END. /* HISTORIKK */
   END.   

   RUN EnaDis ("ByttFrame").
   IF NOT BUTTON-SettLopNr:HIDDEN IN FRAME DEFAULT-FRAME THEN
       ASSIGN BUTTON-SettLopNr:sensitive IN FRAME DEFAULT-FRAME = wAktivFlip = 1
              .
   ASSIGN
     ArtBas.LevNr:SENSITIVE  = ArtBas.OPris = FALSE AND (wAktivFlip = 1)
     BUTTON-SokLev:SENSITIVE = ArtBas.OPris = FALSE AND (wAktivFlip = 1).
     
   /* Sjekker om størrelsesflagg kan endres på artikkelen. */
   /* Overstyrer det som har skjedd over.                  */
   /* På Flik 1 er flaggene i utgangspunktet akriverte.    */
   IF wAktivFlip = 1 THEN
   DO:
     IF wModus <> "NY" THEN
       DO:
         IF CAN-FIND(FIRST ArtLag WHERE
                           ArtLag.Artikkelnr = Artbas.artikkelnr) THEN
       END.
   END.  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByttFrameOrg C-ArtKort 
PROCEDURE ByttFrameOrg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR wFrameHandle AS HANDLE NO-UNDO.
   DEF VAR wFirst-Child AS HANDLE NO-UNDO.
   ASSIGN CURRENT-WINDOW = {&WINDOW-NAME}.

   IF wModus = "NY" AND wAktivFlip <> 1 THEN
     DO:
       wAktivFlip = 1.
       MESSAGE Tx("Artikkelinformasjonen må lagres først!",106)
         VIEW-AS ALERT-BOX MESSAGE TITLE Tx("Melding",107).
     END.
   ASSIGN hAktivHandle = ?.
   /* Info frame som alltid ligger oppe. */
   IF NOT CAN-DO("2,3,4,5,6,7,9,10",STRING(wAktivFlip)) THEN
     IF FRAME DEFAULT-FRAME:MOVE-TO-TOP() THEN.
   /* Er individvinduet oppe og det byttes flik, skal dette bort. */
     IF VALID-HANDLE(hIndivid) AND NOT CAN-DO("6",STRING(wAktivFlip)) THEN
         RUN LukkIndDet IN hIndivid.
   /* Bytter FLik */
/*    IF chTabs:ITEM <> wAktivFlip THEN DO: */
 MESSAGE wAktivflip
     VIEW-AS ALERT-BOX INFO BUTTONS OK.
       chTab1Side = chTabs:Item (wAktivFlip BY-VARIANT-POINTER).
       chTabStrip:SelectedItem = chTab1Side.
       MESSAGE "E"
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
/*    END. */
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
      WHEN 2 THEN DO:
           IF AVAILABLE ArtBas THEN DO:
              IF VALID-HANDLE(wKalkyle) THEN
                  RUN MoveToTopp IN wKalkyle.
              ELSE         
                RUN w-kalkyle.w PERSISTENT SET wKalkyle (ArtBas.ArtikkelNr,THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE,h_PrisKo,wModus).  
              ASSIGN hAktivHandle = wKalkyle.
           END.
       END.
       WHEN 3 THEN STREKKODE: DO:
           IF AVAILABLE ArtBas THEN DO:
              IF VALID-HANDLE(hStrekkode) THEN
                  RUN MoveToTopp IN hStrekkode.
              ELSE         
                  RUN w-strekkode.w PERSISTENT SET hStrekkode (ArtBas.ArtikkelNr,THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE).  
              ASSIGN hAktivHandle = hStrekkode.
           END.
       END. /* STREKKODE */
       WHEN 4 THEN PAKKE: DO:
           IF AVAILABLE ArtBas THEN DO:
              IF VALID-HANDLE(hPakkelinje) THEN
                  RUN MoveToTopp IN hPakkelinje.
              ELSE         
                  RUN w-Pakkelinje.w PERSISTENT SET hPakkelinje (ArtBas.ArtikkelNr,THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE).
              ASSIGN hAktivHandle = hPakkelinje.
           END.
       END. /* PAKKE */
       WHEN 5 THEN ERSTATT: DO:
           IF AVAILABLE ArtBas THEN DO:
              IF VALID-HANDLE(hErstattning) THEN DO:
                  RUN MoveToTopp IN hErstattning.
              END.
              ELSE         
                  RUN w-Erstattningsvare.w PERSISTENT SET hErstattning (ArtBas.ArtikkelNr,THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE).
              ASSIGN hAktivHandle = hErstattning.
          END.
      END. /* ERSTATT */
       WHEN 6 THEN INDIVID: DO:
           IF AVAILABLE ArtBas THEN DO:
              IF VALID-HANDLE(hIndivid) THEN DO:
                  RUN MoveToTopp IN hIndivid.
              END.
              ELSE         
                  RUN w-Individvare.w PERSISTENT SET hIndivid (ArtBas.ArtikkelNr,THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE).
              ASSIGN hAktivHandle = hIndivid.
          END.
      END. /* INDIVID */
      WHEN 7 THEN DO:
          IF AVAILABLE ArtBas THEN DO:
             IF VALID-HANDLE(hBestilling) THEN DO:
                 RUN MoveToTopp IN hBestilling.
             END.
             ELSE         
                 RUN w-Bestilling.w PERSISTENT SET hBestilling (ArtBas.ArtikkelNr,THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE).
             ASSIGN hAktivHandle = hBestilling.
         END.
      END.
      WHEN 8 THEN DO:
          FRAME FRAME-Lager:MOVE-TO-TOP().
          RUN VisLager (NO).
          APPLY "ENTRY":U TO BROWSE-Lager.
      END.
      WHEN 9 THEN BESTPKT: DO:
          IF AVAILABLE ArtBas THEN DO:
             IF VALID-HANDLE(hArtBestPkt) THEN DO:
                 RUN MoveToTopp IN hArtBestPkt.
             END.
             ELSE DO:         
                 RUN w-artbestpkt.w PERSISTENT SET hArtBestPkt (ArtBas.ArtikkelNr,THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE).
                 RUN OpenQueryArtBestPkt IN hArtBestPkt.
             END.

             ASSIGN hAktivHandle = hArtBestPkt.
         END.
      END.
      WHEN 10 THEN TRANSLOGG: DO:
          IF AVAILABLE ArtBas THEN DO:
             IF VALID-HANDLE(hTranslogg) THEN DO:
                      RUN MoveToTopp IN hTranslogg.
                      RUN ByttObjekt IN hTranslogg (ArtBas.ArtikkelNr).  
                      RUN SetEntry IN hTranslogg.
             END.
             ELSE         
                 RUN w-barttranslogg.w PERSISTENT SET hTranslogg (ArtBas.ArtikkelNr,THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE).
             ASSIGN hAktivHandle = hTranslogg.
          END.
      END. /* TRANSLOGG */
      WHEN 11 THEN HISTORIKK: DO:
          IF VALID-HANDLE(wHistorikk) THEN
              RUN MoveToTopp IN wHistorikk.
          IF AVAILABLE ArtBas THEN DO:
              IF VALID-HANDLE(wHistorikk) THEN
                RUN ByttObjekt IN wHistorikk (STRING(ArtBas.ArtikkelNr,"9999999999999")).  
              ELSE         
                RUN w-stlinje.w PERSISTENT SET wHistorikk (wArtBasRecid,STRING(ArtBas.ArtikkelNr,"9999999999999"),"ARTIKKEL","MANED",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE).  
              ASSIGN hAktivHandle = wHistorikk.
          END.
       END. /* HISTORIKK */
   END.   

   RUN EnaDis ("ByttFrame").
   IF NOT BUTTON-SettLopNr:HIDDEN IN FRAME DEFAULT-FRAME THEN
       ASSIGN BUTTON-SettLopNr:sensitive IN FRAME DEFAULT-FRAME = wAktivFlip = 1
              .

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
         ArtBas.LevNr:sensitive  = ArtBas.OPris = FALSE AND (wAktivFlip = 1)
         BUTTON-SokLev:sensitive = ArtBas.OPris = FALSE AND (wAktivFlip = 1).
    END.
         
   /* Sjekker om størrelsesflagg kan endres på artikkelen. */
   /* Overstyrer det som har skjedd over.                  */
   /* På Flik 1 er flaggene i utgangspunktet akriverte.    */
   IF wAktivFlip = 1 THEN
   DO:
     IF wModus <> "NY" THEN
       DO:
         IF CAN-FIND(FIRST ArtLag WHERE
                           ArtLag.Artikkelnr = Artbas.artikkelnr) THEN
       END.
   END.  

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
      RECID(ArtBas) = wArtBasRecid NO-ERROR.
  FIND VarGr NO-LOCK WHERE
    VarGr.Vg = ArtBas.Vg NO-ERROR.
  FIND HuvGr OF VarGr NO-LOCK NO-ERROR.
  IF AVAILABLE VarGr THEN
    FIND FIRST VgKat NO-LOCK WHERE
      VgKat.Vg    = ArtBas.Vg    AND
      VgKat.VgKat = ArtBas.VgKat NO-ERROR.
  DISPLAY ArtBas.Vg        WHEN AVAILABLE ArtBas
          ArtBas.LopNr     WHEN AVAILABLE ArtBas
          ArtBas.Hg        WHEN AVAILABLE ArtBas @ Huvgr.Hg
          HuvGr.HgBeskr    WHEN AVAILABLE HuvGr
          VarGr.VgBeskr @  FILL-IN-VgBeskr WITH FRAME Default-Frame.

  DISPLAY ArtBas.BongTekst WHEN AVAILABLE ArtBas
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

OCXFile = SEARCH( "w-vartkor.wrx":U ).
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
ELSE MESSAGE "w-vartkor.wrx":U SKIP(1)
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
  DEFINE INPUT  PARAMETER lEnaDis AS LOGICAL    NO-UNDO.

  DO WITH FRAME FRAME-ArtInfo:
      ASSIGN
        FI-1:SENSITIVE = T-1:CHECKED
        /*ArtBas.LevFargKod:SENSITIVE = lEnaDis */ /* Skal alltid være enablet. */
        FI-2:SENSITIVE = T-2:CHECKED /*lEnaDis*/ AND cVisMaterial = "1"
        FI-3:SENSITIVE = T-3:CHECKED AND cVisKlack = "1"
        FI-4:SENSITIVE = T-4:CHECKED
        FI-5:SENSITIVE = T-5:CHECKED
        FI-6:SENSITIVE = T-6:CHECKED
        FI-7:SENSITIVE = T-7:CHECKED
        FI-8:SENSITIVE = T-8:CHECKED
        T-1:SENSITIVE  = lEnaDis
        T-2:SENSITIVE  = lEnaDis AND cVisMaterial = "1"
        T-3:SENSITIVE  = lEnaDis AND cVisKlack = "1"
        T-4:SENSITIVE  = lEnaDis
        T-5:SENSITIVE  = lEnaDis
        T-6:SENSITIVE  = lEnaDis
        T-7:SENSITIVE  = lEnaDis
        T-8:SENSITIVE  = lEnaDis
        T-9:SENSITIVE  = lEnaDis
        T-10:SENSITIVE = lEnaDis.

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
  DISPLAY FI-ArtBut CB-ModellFarge CB-EkstVPILev FILL-IN-VgBeskr FILL-IN-LevNamn 
          FI-PaTilbud TOGGLE-Annonse FI-LevFargKod TB-WebButikkArtikkel 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-ArtKort.
  IF AVAILABLE ArtBas THEN 
    DISPLAY ArtBas.HoyLavMva ArtBas.Telefonkort ArtBas.Grunnsortiment 
          ArtBas.NegVare ArtBas.NON_Sale ArtBas.UtgattDato 
          ArtBas.ManueltOpprettet ArtBas.Gjennomfaktureres ArtBas.KjedeVare 
          ArtBas.BestForslag ArtBas.IndividType ArtBas.ArtikkelNr ArtBas.Vg 
          ArtBas.VgKat ArtBas.LopNr ArtBas.Pant ArtBas.Beskr ArtBas.LevNr 
          ArtBas.LevKod ArtBas.lager ArtBas.Pakke ArtBas.Utgatt ArtBas.IKasse 
          ArtBas.BildeIKasse ArtBas.OPris ArtBas.HkStyrt ArtBas.KjentPaHK 
          ArtBas.LokPris ArtBas.SanertDato ArtBas.ArtSlag 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-ArtKort.
  IF AVAILABLE HuvGr THEN 
    DISPLAY HuvGr.Hg HuvGr.HgBeskr 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-ArtKort.
  ENABLE B-Kobble ArtBas.HoyLavMva ArtBas.Telefonkort ArtBas.Grunnsortiment 
         ArtBas.NegVare B-VPI BUTTON-Ok BUTTON-SokLev BUTTON-TilKasse B-Folder 
         B-Sanera BUTTON-Kopierbilde BUTTON-Slettbilde ArtBas.Gjennomfaktureres 
         ArtBas.KjedeVare B-Sok Btn_Help BUTTON-Angre BUTTON-Kopier 
         BUTTON-Lagre BUTTON-ModellFarg BUTTON-Next BUTTON-Ny BUTTON-Paste 
         BUTTON-Prev BUTTON-Slett BUTTON-SokFil ArtBas.BestForslag 
         CB-ModellFarge ArtBas.Vg CB-EkstVPILev ArtBas.VgKat ArtBas.LopNr 
         ArtBas.Beskr ArtBas.LevNr ArtBas.LevKod ArtBas.Utgatt TOGGLE-Annonse 
         ArtBas.IKasse ArtBas.BildeIKasse BUTTON-SokVgKat FI-LevFargKod 
         ArtBas.SanertDato TB-WebButikkArtikkel ArtBas.ArtSlag RECT-27 RECT-28 
         RECT-34 RECT-35 RECT-5 RECT-55 RECT-56 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-ArtKort.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  DISPLAY FI-LinkVareTekst CB-Mellankategori CB-MUkat FI-1 CB-Levsort 
          FILL-IN-InnPris FILL-IN-SalgsPris FILL-IN-TilbPris FILL-IN-UtsFra 
          FILL-IN-UtsTil FI-JamforPris T-EnableLevInfo T-1 T-10 T-9 
          S-AlternativLev FI-Tekst1 FI-Tekst-3 FILL-IN-EndretInfo FI-Tekst-2 
          FILL-IN-19 FI-Tekst-4 Txt-AlternativLev FI-Tekst-5 FI-Tekst-9 
          FI-Tekst-7 FI-Tekst-6 FI-Tekst-10 FI-Tekst-12 FI-Tekst-11 
          S-Underkategori Txt-Underkat FI-Tekst-13 FI-Tekst-14 
      WITH FRAME FRAME-ArtInfo IN WINDOW C-ArtKort.
  IF AVAILABLE ArtBas THEN 
    DISPLAY ArtBas.OnLineLevNr ArtBas.LinkVareAnt ArtBas.SalgsStopp 
          ArtBas.KundeRabatt ArtBas.ManRabIKas ArtBas.MengdeRabatt 
          ArtBas.Bonus_Givende ArtBas.PubliserINettbutikk ArtBas.StrTypeID 
          ArtBas.SaSong ArtBas.Farg ArtBas.MatKod ArtBas.Klack ArtBas.inner-id 
          ArtBas.ov-id ArtBas.slit-id ArtBas.last-id ArtBas.anv-id ArtBas.VMId 
          ArtBas.ProdNr ArtBas.HovedKatNr ArtBas.valkod ArtBas.Notat 
          ArtBas.LevFargKod ArtBas.BongTekst ArtBas.Etikettekst1 
          ArtBas.Etikettekst2 ArtBas.Etikett ArtBas.Lokasjon ArtBas.LinkVareNr 
          ArtBas.GarantiKl ArtBas.Alder ArtBas.SalgsEnhet ArtBas.BehKode 
          ArtBas.RAvdNr ArtBas.Link_til_Nettside ArtBas.VareType 
          ArtBas.Leveringstid ArtBas.JamforEnhet ArtBas.Mengde 
          ArtBas.Kjokkenskriver ArtBas.AntIPakn ArtBas.Anbrekk 
          ArtBas.InkrAnbrekk ArtBas.WebButikkArtikkel ArtBas.WebLeveringstid 
          ArtBas.WebMinLager ArtBas.KampanjeKode ArtBas.Depositum 
          ArtBas.inn_dato 
      WITH FRAME FRAME-ArtInfo IN WINDOW C-ArtKort.
  IF AVAILABLE Farg THEN 
    DISPLAY Farg.FarBeskr 
      WITH FRAME FRAME-ArtInfo IN WINDOW C-ArtKort.
  IF AVAILABLE HovedKategori THEN 
    DISPLAY HovedKategori.HovedKatTekst 
      WITH FRAME FRAME-ArtInfo IN WINDOW C-ArtKort.
  IF AVAILABLE Produsent THEN 
    DISPLAY Produsent.Beskrivelse 
      WITH FRAME FRAME-ArtInfo IN WINDOW C-ArtKort.
  IF AVAILABLE Regnskapsavdeling THEN 
    DISPLAY Regnskapsavdeling.RAvdBeskrivelse 
      WITH FRAME FRAME-ArtInfo IN WINDOW C-ArtKort.
  IF AVAILABLE SaSong THEN 
    DISPLAY SaSong.SasBeskr 
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
  ENABLE ArtBas.OnLineLevNr ArtBas.LinkVareAnt ArtBas.SalgsStopp 
         ArtBas.KundeRabatt ArtBas.ManRabIKas B-SokLinkvare-2 
         ArtBas.MengdeRabatt ArtBas.Bonus_Givende ArtBas.PubliserINettbutikk 
         ArtBas.StrTypeID ArtBas.SaSong ArtBas.Farg ArtBas.MatKod ArtBas.Klack 
         ArtBas.inner-id ArtBas.ov-id ArtBas.slit-id ArtBas.last-id 
         BUTTON-NyStrType ArtBas.anv-id ArtBas.VMId ArtBas.ProdNr 
         ArtBas.HovedKatNr CB-Mellankategori B-alternativeLev CB-MUkat 
         ArtBas.valkod ArtBas.Notat B-Refresh B-SokLinkvare BUTTON-NyLevsort 
         BUTTON-SokBruk ArtBas.LevFargKod BUTTON-SokInner ArtBas.BongTekst 
         ArtBas.Etikettekst1 ArtBas.Etikettekst2 ArtBas.Etikett BUTTON-SokKlak 
         ArtBas.Lokasjon CB-Levsort ArtBas.GarantiKl BUTTON-SokLalst 
         ArtBas.Alder ArtBas.SalgsEnhet ArtBas.BehKode ArtBas.RAvdNr 
         ArtBas.Link_til_Nettside BUTTON-SokMaterial BUTTON-SokOver 
         ArtBas.VareType ArtBas.Leveringstid ArtBas.JamforEnhet ArtBas.Mengde 
         BUTTON-SokSesong ArtBas.Kjokkenskriver ArtBas.AntIPakn ArtBas.Anbrekk 
         ArtBas.InkrAnbrekk BUTTON-SokSlit ArtBas.WebButikkArtikkel 
         ArtBas.WebLeveringstid ArtBas.WebMinLager ArtBas.KampanjeKode 
         ArtBas.Depositum BUTTON-SokFarge BUTTON-SokBehKode BUTTON-SokRegnAvd 
         T-EnableLevInfo S-AlternativLev BUTTON-SokStrType BUTTON-SokVaremerke 
         BUTTON-SokProdusent BUTTON-SokValuta FILL-IN-EndretInfo 
         S-Underkategori BUTTON-Underkategori BUTTON-SokHovedKategori RECT-31 
         RECT-54 RECT-61 RECT-29 RECT-62 RECT-63 RECT-64 RECT-66 RECT-67 
         RECT-68 RECT-70 RECT-65 RECT-71 
      WITH FRAME FRAME-ArtInfo IN WINDOW C-ArtKort.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-ArtInfo}
  DISPLAY S-Karakteristikk FI-Land FI-VpiInfoTxt Txt-Linjemerknad 
          FI-VpiInfoTxt-2 Txt-OpprLand Txt-Varefakta FI-VpiInfoTxt-3 
      WITH FRAME FRAME-Info2 IN WINDOW C-ArtKort.
  IF AVAILABLE ArtBas THEN 
    DISPLAY ArtBas.LinjeMerknad ArtBas.VPIDato ArtBas.TilgjengeligFraLev 
          ArtBas.LevDato1 ArtBas.LevDato2 ArtBas.LevDato3 ArtBas.LevDato4 
          ArtBas.KatalogPris ArtBas.AnbefaltPris ArtBas.KjedeInnkPris 
          ArtBas.KjedeValutaPris ArtBas.Sortimentkoder ArtBas.Lagerkoder 
          ArtBas.Kampanjeuker ArtBas.Kampanjestotte ArtBas.EkstStrTypeNavn 
          ArtBas.VPIBildeKode ArtBas.LevDatoStopp1 ArtBas.LevDatoStopp2 
          ArtBas.LevDatoStopp3 ArtBas.LevDatoStopp4 ArtBas.forhRab% 
          ArtBas.supRab% ArtBas.KjedeRab% ArtBas.KjedeProdusent ArtBas.VareFakta 
          ArtBas.PostBredde ArtBas.PostHoyde ArtBas.PostLengde ArtBas.PostVekt 
          ArtBas.AlfaKode2 
      WITH FRAME FRAME-Info2 IN WINDOW C-ArtKort.
  ENABLE BUTTON-SokLandKode RECT-33 RECT-36 RECT-69 RECT-72 RECT-73 
         ArtBas.LinjeMerknad ArtBas.VPIDato ArtBas.TilgjengeligFraLev 
         ArtBas.LevDato1 ArtBas.LevDato2 ArtBas.LevDato3 ArtBas.LevDato4 
         ArtBas.KatalogPris ArtBas.AnbefaltPris ArtBas.KjedeInnkPris 
         ArtBas.KjedeValutaPris ArtBas.Sortimentkoder ArtBas.Lagerkoder 
         ArtBas.Kampanjeuker ArtBas.Kampanjestotte ArtBas.EkstStrTypeNavn 
         ArtBas.VPIBildeKode ArtBas.LevDatoStopp1 ArtBas.LevDatoStopp2 
         ArtBas.LevDatoStopp3 ArtBas.LevDatoStopp4 ArtBas.forhRab% 
         ArtBas.supRab% ArtBas.KjedeRab% ArtBas.KjedeProdusent ArtBas.VareFakta 
         ArtBas.PostBredde ArtBas.PostHoyde ArtBas.PostLengde ArtBas.PostVekt 
         ArtBas.AlfaKode2 S-Karakteristikk BUTTON-Karakteristikk 
         BUTTON-SokLevDat-3 BUTTON-SokLevDat-4 BUTTON-SokLevDat-5 
         BUTTON-SokLevDat-6 BUTTON-SokLevDat-7 BUTTON-SokLevDat-8 
         BUTTON-SokLevDat-2 BUTTON-SokLevDato-3 BUTTON-SokLevDato-4 
         BUTTON-SokLevDato-5 
      WITH FRAME FRAME-Info2 IN WINDOW C-ArtKort.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-Info2}
  DISPLAY TG-VisSkjul RS-Vis 
      WITH FRAME FRAME-Lager IN WINDOW C-ArtKort.
  ENABLE B-Svinn B-Nedskrivning B-ForenklVaremot B-Chart B-OppdLAger 
         BUTTON-Overfor B-Lagerjustering B-Reklamer B-ByggomLager TG-VisSkjul 
         BROWSE-Lager RS-Vis 
      WITH FRAME FRAME-Lager IN WINDOW C-ArtKort.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-Lager}
  VIEW C-ArtKort.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EnaDis C-ArtKort 
PROCEDURE EnaDis :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER cFrom AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE lVgArtbas AS LOGICAL    NO-UNDO.
  
  IF NOT AVAILABLE ArtBas THEN RETURN.
  
  IF wModus <> "Ny" AND lOverstyrFrame1VgArt = FALSE THEN DO:
      ASSIGN lVgArtBas = ArtBas.ArtikkelNr = ArtBas.Vg.
  END.

  ASSIGN FRAME FRAME-ArtInfo:SENSITIVE = NOT lVgArtBas.

  ASSIGN
      BUTTON-SokVg:sensitive IN FRAME DEFAULT-FRAME = (wAktivFlip = 1)
      B-Sok:sensitive = wModus <> "Ny"
      B-Folder:SENSITIVE = wAktivFlip = 1
      .
  IF cFrom = "ByttFrame" OR lVgArtBas THEN DO WITH FRAME DEFAULT-FRAME:
      ASSIGN 
        /* Felt som kun er †pne p† f›rste side. */
        ArtBas.Vg:sensitive         = NOT lVgArtBas AND (wAktivFlip = 1) AND (wModus =  "Ny" OR ArtBas.LopNr = ?)
        ArtBas.LopNr:sensitive      = NOT lVgArtBas AND (wAktivFlip = 1) AND wModus =  "NY" /* AND wLapTop = FALSE */
/*         BUTTON-SokVg:sensitive      = (wAktivFlip = 1) and wModus =  "NY" */
        BUTTON-SokVgKat:sensitive   = NOT lVgArtBas AND (wAktivFlip = 1) 
        ArtBas.Beskr:sensitive       = NOT lVgArtBas AND (wAktivFlip = 1) 
        ArtBas.LevKod:sensitive      = NOT lVgArtBas AND (wAktivFlip = 1)
        ArtBas.LevFargKod:sensitive IN FRAME FRAME-ArtInfo = NOT lVgArtBas AND (wAktivFlip = 1)     
        ArtBas.VgKat:sensitive       = NOT lVgArtBas AND (wAktivFlip = 1) 
        ArtBas.Utgatt:sensitive      = NOT lVgArtBas AND (wAktivFlip = 1) AND ArtBas.Slasket = FALSE
        ArtBas.KjedeVare:sensitive   = NOT lVgArtBas AND (wAktivFlip = 1) AND bKjede
        ArtBas.Gjennomfaktureres:sensitive   = NOT lVgArtBas AND (wAktivFlip = 1) AND bGjFakt
        ArtBas.BildeIKasse:sensitive = NOT lVgArtBas AND (wAktivFlip = 1)
        ArtBas.IKasse:SENSITIVE      = (wAktivFlip = 1)
/*         ArtBas.Medlemsutbytte:SENSITIVE = (wAktivFlip = 1) */
   /*   ArtBas.OPris:SENSITIVE       = FALSE */
        TOGGLE-Annonse:SENSITIVE     = NOT lVgArtBas AND (wAktivFlip = 1)
        TB-WebButikkArtikkel:sensitive = FALSE
        ArtBas.WebButikkArtikkel:SENSITIVE = NOT lVgArtBas AND (wAktivFlip = 1)
        /*ArtBas.ManueltOpprettet:SENSITIVE = NOT lVgArtBas AND (wAktivFlip = 1)*/
        ArtBas.BestForsl:SENSITIVE   = NOT lVgArtBas AND (wAktivFlip = 1)
/*         ArtBas.KjentPaHK:SENSITIVE   = (wAktivFlip = 1) AND NOT ArtBas.HkStyrt */
        BUTTON-Paste:SENSITIVE       = NOT lVgArtBas AND /* wModus <> "NY" AND */ ArtBas.HkStyrt = FALSE AND (wAktivFlip = 1)
        BUTTON-SokFil:SENSITIVE      = BUTTON-Paste:SENSITIVE
        BUTTON-NyLevsort:SENSITIVE   = (wAktivFlip = 1)
        ArtBas.LinkVareNr:SENSITIVE  = NOT ArtBas.Pant:CHECKED          
        ArtBas.LinkVareAnt:SENSITIVE  = NOT ArtBas.Pant:CHECKED          
        B-SokLinkVare:SENSITIVE      = ArtBas.LinkVareNr:SENSITIVE 
        .
  END.
  ELSE IF cFrom = "VisArtbas" AND NOT lVgArtBas THEN DO:
      DO WITH FRAME DEFAULT-FRAME:

          IF ArtBas.LopNr = 0 OR ArtBas.LopNr = ? THEN
             ASSIGN BUTTON-SettLopNr:HIDDEN    = FALSE
                    BUTTON-SettLopNr:SENSITIVE = TRUE.
          ELSE
             ASSIGN BUTTON-SettLopNr:HIDDEN    = TRUE
                    BUTTON-SettLopNr:SENSITIVE = FALSE.

           IF NOT BUTTON-SettLopNr:HIDDEN THEN
               ASSIGN BUTTON-SettLopNr:SENSITIVE = wAktivFlip = 1                      
          /* hantering av enabling/disabling nya fält InfoPos-integration */
          BUTTON-SokVgKat:SENSITIVE   = (wAktivFlip = 1) AND NOT ArtBas.StrTypeID = 1
/*           ArtBas.KjentPaHK:SENSITIVE  = (wAktivFlip = 1) AND NOT ArtBas.HkStyrt /* ev para */ */
          ArtBas.Utgatt:SENSITIVE   = (wAktivFlip = 1) AND ArtBas.Slasket = FALSE
          ArtBas.KjedeVare:SENSITIVE   = (wAktivFlip = 1) AND bKjede
          ArtBas.Gjennomfaktureres:SENSITIVE   = (wAktivFlip = 1) AND bGjFakt.
          
      END.
  END.

  ASSIGN
        ArtBas.LokPris:SENSITIVE  = bBruker.BrukerType = 1
        ArtBas.Lager:SENSITIVE    = ((wAktivFlip = 1) AND
                                     ArtBas.Pakke:checked = FALSE AND
                                     ArtBas.OPris:checked = FALSE AND
                                     ArtBas.NON_Sale:checked = FALSE )
        Button-TilKasse:SENSITIVE = ArtBas.IKasse
        IndividType:SENSITIVE     = FALSE 
        ArtBas.Pakke:SENSITIVE = FALSE
        /*
        ArtBas.Pakke:SENSITIVE = ((wAktivFlip = 1) AND
                                ArtBas.Pant:checked = FALSE AND
                                ArtBas.OPris:checked = FALSE AND
                                ArtBas.NON_Sale:checked = FALSE AND
                                ArtBas.Lager:checked = FALSE AND
                                NOT CAN-FIND(FIRST Pakkelinje OF ArtBas) AND
                                NOT CAN-FIND(FIRST TransLogg WHERE
                                                Translogg.ArtikkelNr = ArtBas.ArtikkelNr) AND
                                can-do("0",STRING(iArtSlag)) AND 
                                ArtBas.Vg <> ArtBas.ArtikkelNr)
        */                                                                           
        ArtBas.OPris:SENSITIVE  = ((wAktivFlip = 1) AND
                                ArtBas.Lager:checked = FALSE AND
                                ArtBas.Pant:checked = FALSE AND
                                ArtBas.Pakke:checked = FALSE AND
                                ArtBas.Non_Sale:checked = FALSE AND
                                NOT CAN-FIND(FIRST TransLogg WHERE
                                             TransLogg.ArtikkelNr = ArtBas.ArtikkelNr) AND
                                NOT CAN-FIND(FIRST BestHode WHERE
                                              BestHode.ArtikkelNr = ArtBas.ArtikkelNr) AND
                                ArtBas.Vg <> ArtBas.ArtikkelNr)
                                
       ArtBas.Pant:SENSITIVE  = FALSE
       /*
       ArtBas.Pant:SENSITIVE  = ((wAktivFlip = 1) AND
                              ArtBas.OPris:checked = FALSE AND
                              ArtBas.Pakke:checked = FALSE AND
                              ArtBas.Non_Sale:checked = FALSE AND
                              NOT CAN-FIND(FIRST TransLogg WHERE
                                           TransLogg.ArtikkelNr = ArtBas.ArtikkelNr) AND
                              NOT CAN-FIND(FIRST BestHode WHERE
                                            BestHode.ArtikkelNr = ArtBas.ArtikkelNr) AND
                              ArtBas.Vg <> ArtBas.ArtikkelNr)
        */
        ArtBas.Non_Sale:SENSITIVE = ((wAktivFlip = 1) AND
                                ArtBas.Pakke:checked = FALSE AND
                                ArtBas.Pant:checked = FALSE AND
                                ArtBas.Lager:checked = FALSE AND
                                NOT CAN-FIND(FIRST TransLogg WHERE
                                             TransLogg.ArtikkelNr = ArtBas.ArtikkelNr) AND
                                NOT CAN-FIND(FIRST BestHode WHERE
                                             BestHode.ArtikkelNr = ArtBas.ArtikkelNr)) 

      ArtBas.NegVare:SENSITIVE = ((wAktivFlip = 1) AND 
                                  ArtBas.Non_Sale:CHECKED AND 
                                  NOT CAN-FIND(FIRST TransLogg WHERE
                                               TransLogg.ArtikkelNr = ArtBas.ArtikkelNr) AND
                                  NOT CAN-FIND(FIRST BestHode WHERE
                                               BestHode.ArtikkelNr = ArtBas.ArtikkelNr) AND
                                  ArtBas.Vg <> ArtBas.ArtikkelNr) 
      ArtBas.PubliserINettbutikk:SENSITIVE = ((wAktivFlip = 1) AND 
/* 151006 ken1 */      ArtBas.WebButikkArtikkel:SENSITIVE) 
/*       ArtBas.WebButikkArtikkel:CHECKED) */
      .
  DO WITH FRAME FRAME-ArtInfo:
      ASSIGN 
           ArtBas.STrTypeId:SENSITIVE   = ((wAktivFlip = 1) AND
                                          ArtBas.Pant:CHECKED = FALSE AND
                                          ArtBas.Pakke:CHECKED = FALSE AND
                                          ArtBas.ArtSlag < 1 AND  
                                          ArtBas.Opris:CHECKED = FALSE AND
                                          ArtBas.Non_Sale:CHECKED = FALSE )
           BUTTON-SokStrType:SENSITIVE  = ArtBas.STrTypeId:SENSITIVE
           ArtBas.ManRabIKas:SENSITIVE  = NOT lVgArtBas AND (wAktivFlip = 1)
           ArtBas.KundeRabatt:SENSITIVE = NOT lVgArtBas AND (wAktivFlip = 1)
           ArtBas.MengdeRabatt:SENSITIVE = NOT lVgArtBas AND (wAktivFlip = 1)
           .
  END.
      
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FillCBModellFarge C-ArtKort 
PROCEDURE FillCBModellFarge :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cListItemPairs AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cArtRef AS CHARACTER   NO-UNDO.

  DO WITH FRAME DEFAULT-FRAME:
    IF ArtBas.ModellFarge = 0 THEN
        ASSIGN CB-ModellFarge:LIST-ITEM-PAIRS = ","
               CB-ModellFarge:SENSITIVE       = FALSE.
    ELSE DO:
        FOR EACH bArtBas NO-LOCK WHERE bArtBas.ModellFarge = ArtBas.ModellFarge:
            FIND Farg OF bArtBas NO-LOCK NO-ERROR.
            FIND Material OF bArtBas NO-LOCK NO-ERROR.
            cArtRef = IF bArtBas.Levkod <> "" THEN "L:" + TRIM(bArtBas.Levkod) ELSE "A:" + STRING(bArtbas.Artikkelnr).
            ASSIGN cListItemPairs = cListItemPairs + 
                                    (IF cListItemPairs = "" THEN "" ELSE ",") +
                                    cArtRef + "-" +
/*                                     STRING(bArtBas.ArtikkelNr) + "-" + */
                                    (IF bArtBas.LevFargKod <> "" THEN REPLACE(bArtBas.LevFargKod,","," ") ELSE IF AVAIL Farg THEN REPLACE(Farg.FarBeskr,","," ") ELSE "")
                                   + '/' + (IF AVAILABLE Material THEN REPLACE(Material.MatBeskr,","," ") ELSE '' )
                                   + (IF bArtBas.ArtikkelNr = bArtBas.ModellFarge THEN "-Hovedvare" ELSE "") + "," + STRING(bArtBas.ArtikkelNr)
                                   .
        END.
        IF cListItemPairs = "" THEN
            ASSIGN cListItemPairs                 = ","
                   CB-ModellFarge:LIST-ITEM-PAIRS = cListItemPairs
                   CB-ModellFarge:SENSITIVE       = FALSE.
        ELSE
            ASSIGN CB-ModellFarge:LIST-ITEM-PAIRS = cListItemPairs
                   CB-ModellFarge:SENSITIVE       = TRUE.
        ASSIGN CB-ModellFarge:INNER-LINES         = NUM-ENTRIES(cListItemPairs) / 2.
    END.
    ASSIGN B-Jamfor:SENSITIVE = CB-ModellFarge:SENSITIVE.
/*     IF CB-ModellFarge:LIST-ITEM-PAIRS <> "," THEN                      */
/*       ASSIGN CB-ModellFarge:SCREEN-VALUE = STRING(ArtBas.ModellFarge). */
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FillLevSortCB C-ArtKort 
PROCEDURE FillLevSortCB :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DO WITH FRAME FRAME-ArtInfo:
       DEFINE VARIABLE cList-Items AS CHARACTER  NO-UNDO.
       DEFINE VARIABLE cSortAlle   AS CHARACTER  NO-UNDO.
       DEFINE VARIABLE cLevSant    AS CHARACTER  NO-UNDO.
       DEFINE VARIABLE cSoAnt      AS CHARACTER  NO-UNDO.
       DEFINE VARIABLE iMax        AS INTEGER    NO-UNDO.
       IF wModus = "NY" THEN
           ASSIGN cList-Items = "".
       ELSE IF AVAIL ArtBas THEN DO:
           FOR EACH LevSort WHERE LevSort.LevNr = ArtBas.LevNr AND LevSort.StrTypeID = ArtBas.StrTypeID NO-LOCK.
               ASSIGN cLevSant = ""
                      cSoAnt   = "".
               FOR EACH LevSAnt OF LevSort NO-LOCK BY Seqnr:
                   ASSIGN iMax = MAX(LENGTH(TRIM(LevSAnt.SoStorl)),LENGTH(STRING(LevSAnt.SoAnt))).
                   ASSIGN cLevSant = cLevSant + (IF cLevSant <> "" THEN " " ELSE "") + FILL(" ",iMax - LENGTH(TRIM(LevSAnt.SoStorl))) + TRIM(LevSAnt.SoStorl)
                          cSoAnt   = cSoAnt   + (IF cSoAnt <> "" THEN " " ELSE "") + FILL(" ",iMax - LENGTH(STRING(LevSAnt.SoAnt))) + STRING(LevSAnt.SoAnt).
               END.
               ASSIGN cSortAlle = cSortAlle + (IF cSortAlle <> "" THEN "-" ELSE "") + LevSort.SortID
                      cList-Items = cList-Items + (IF cList-Items <> "" THEN "," ELSE "") + LevSort.SortID + " " + cLevSant + "," + FILL(" ",LENGTH(LevSort.SortID)) + " " + cSoAnt.

           END.
       END.
       IF cList-Items <> "" AND NUM-ENTRIES(cList-Items) > 1 THEN
           ASSIGN cList-Items = cSortAlle + "," + cList-Items.

       CB-LevSort:LIST-ITEMS = cList-Items NO-ERROR.
       CB-LevSort:SCREEN-VALUE = ENTRY(1,CB-LevSort:LIST-ITEMS).
       CB-LevSort:INNER-LINES = MAX(1,NUM-ENTRIES(cList-Items)).
   END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getBildeFilNavn C-ArtKort 
PROCEDURE getBildeFilNavn :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE OUTPUT PARAMETER cBildefilNavn AS CHARACTER NO-UNDO.
   ASSIGN cBildefilNavn = chImage-Sko:Picbuf:FILENAME.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetTx C-ArtKort 
PROCEDURE GetTx :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT  PARAMETER iNr     AS INTEGER    NO-UNDO.
   DEFINE OUTPUT PARAMETER TxTekst AS CHARACTER  NO-UNDO.
   CASE iNr:
       WHEN 141 THEN
           ASSIGN TxTekst = Tx("Størrelser er ikrysset, registrert størrelsestype mangler størrelser!",141).
   END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Hentparametre C-ArtKort 
PROCEDURE Hentparametre :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
{syspara.i 2 4 3 cOLLager}       
{syspara.i 2 4 4 cBildeIKasse}       
{syspara.i 2 4 6 cVPItilHK}       
{syspara.i 2 4 7 cNyreg}       
{syspara.i 2 4 8 cGenEan} 
{syspara.i 2 4 12 cLagerStyr}
IF cLagerStyr <> "0" AND cLagerStyr <> "1" THEN
    ASSIGN cLagerStyr = "1".
{syspara.i 2 5 1 cVisKlack}       
{syspara.i 2 5 2 cVisInnersula}       
{syspara.i 2 5 3 cVisInnerfor}       
{syspara.i 2 5 4 cVisSlit}       
{syspara.i 2 5 5 cVisLast}       
{syspara.i 2 5 6 cVisBruks}       
{syspara.i 2 5 9 cVisBehKod}
{syspara.i 2 5 10 cVisMaterial}
{syspar2.i 2 4 8 cGenEan000}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HentVis C-ArtKort 
PROCEDURE HentVis :
/*------------------------------------------------------------------------------
  Purpose:    Hämtar ny post
              anropas från ALT-S och diverse 
  Parameters: 
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER rArtBasRecid AS RECID               NO-UNDO.
  DEFINE INPUT  PARAMETER dArtikkelNr  LIKE ArtBas.ArtikkelNr NO-UNDO.
  IF rArtBasRecid <> ? THEN
      FIND ArtBas NO-LOCK WHERE RECID(ArtBas) = wArtBasRecid NO-ERROR.
  ELSE
      FIND ArtBas NO-LOCK WHERE ArtBas.ArtikkelNr = dArtikkelNr NO-ERROR.
  ASSIGN wArtBasRecid = RECID(ArtBas).
  PUBLISH "ByttObjekt" (ArtBas.ArtikkelNr).
  RUN ByttFrame. /* For † enable knapper og felt korrekt */
  RUN RensFrame.
  RUN VisArtBas.
/*   run VisLager(true). */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HideShowLagerCols C-ArtKort 
PROCEDURE HideShowLagerCols :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*                                                   */
  DEFINE INPUT  PARAMETER cSkjul AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE hBrCol AS HANDLE     NO-UNDO.
  DEFINE VARIABLE ii AS INTEGER    NO-UNDO.

  DO WITH FRAME FRAME-Lager:
      DO ii = 1 TO BROWSE-Lager:NUM-COLUMNS:
          hBrCol = BROWSE-Lager:GET-BROWSE-COLUMN(ii).
          hBrCol:VISIBLE = TRUE.
      END.

      IF cSkjul = "Skjul" THEN
      DO ii = NUM-ENTRIES(cBrukteStr) TO 1 BY -1:
           IF BROWSE-Lager:NUM-COLUMNS >= (ii + 5) THEN
           DO:
               hBrCol = BROWSE-Lager:GET-BROWSE-COLUMN(ii + 5).
               hBrCol:VISIBLE = ENTRY(ii,cBrukteStr) = "J".
           END.
      END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitCB C-ArtKort 
PROCEDURE InitCB :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR pcTekst        AS CHAR NO-UNDO.

  ASSIGN
      pcTekst = ""
      .
  DO WITH FRAME FRAME-ArtInfo:

      pcTekst = ",0".
      FOR EACH OnLineLeverandor NO-LOCK:
          pcTekst = pcTekst + 
                    (IF pcTekst <> "" 
                       THEN ","
                       ELSE "") + 
                    OnLineLeverandor.OnLineLevBeskr + "," + 
                    STRING(OnLineLeverandor.OnLineLevNr).
      END.
      IF pcTekst <> ",0" THEN 
          ASSIGN 
            ArtBAs.OnLineLevNr:LIST-ITEM-PAIRS = pcTekst
            ArtBAs.OnLineLevNr:SCREEN-VALUE = ENTRY(2,pcTekst).
            .
      pcTekst = "".
      FOR EACH Garanti NO-LOCK:

          ASSIGN
              pcTekst = pcTekst + 
                        (IF pcTekst <> ""
                           THEN ","
                           ELSE "") + 
                        Garanti.GarantiTekst + "," + 
                        string(Garanti.GarantiKl)
              .
      END.
      IF pcTekst <> "" THEN
          ASSIGN
          ArtBAs.GarantiKl:LIST-ITEM-PAIRS = pcTekst
          ArtBAs.GarantiKl:SCREEN-VALUE = ENTRY(2,pcTekst).
          
      pcTekst = ''.    
      FOR EACH Syspara NO-LOCK WHERE
        SysPara.SysHId = 2 AND
        SysPara.SysGr  = 11:

          ASSIGN
              pcTekst = pcTekst + 
                        (IF pcTekst <> ""
                           THEN ","
                           ELSE "") + 
                        SysPara.Beskrivelse + "," + 
                        string(SysPara.ParaNr)
              .
      END.
      IF pcTekst <> "" THEN
          ASSIGN
          ArtBAs.Varetype:LIST-ITEM-PAIRS = pcTekst
          ArtBAs.VareType:SCREEN-VALUE = ENTRY(2,pcTekst).

      pcTekst = ''.    
      FOR EACH Salgsenhet NO-LOCK
        BY SalgsEnhet.SalgsEnhTekst:

          ASSIGN
              pcTekst = pcTekst + 
                        (IF pcTekst <> ""
                           THEN ","
                           ELSE "") + 
                        Salgsenhet.SalgsEnhTekst
              .
      END.
      IF pcTekst <> "" THEN
          ASSIGN
          ArtBas.SalgsEnhet:LIST-ITEMS = pcTekst
          ArtBas.SalgsEnhet:SCREEN-VALUE    = ENTRY(2,pcTekst).
  
      ASSIGN
        pcTekst = '<Ikke angitt>,BLK'.
      FOR EACH JamforEnhet NO-LOCK:
          ASSIGN
              pcTekst = pcTekst + 
                   (IF pcTekst <> ""
                     THEN ","
                     ELSE "") + 
                   JamforEnhet.JamforEnhTekst + "," + 
                   JamforEnhet.JamforEnhet.
      END.
      IF pcTekst <> "" THEN
          ASSIGN
          ArtBas.JamforEnhet:LIST-ITEM-PAIRS = pcTekst
          ArtBas.JamforEnhet:SCREEN-VALUE = ENTRY(2,pcTekst).
  END.

  DO WITH FRAME Default-Frame:
      ASSIGN
          ArtBas.ArtSlag:LIST-ITEM-PAIRS = "Stykkvare(Stk),0" +
                                          ",Vektvare(Kg),1" +
                                          ",Vektvare(Hg),2" +
                                          ",Metervare(m),3" + 
                                          ",Kvadratmetervare(m2),4" + 
                                          ",Volumvare(l),5" +
                                          ",Pakkevare(Stk),7" + 
                                          ",Pant(Stk),8"
          ArtBas.ArtSlag:SCREEN-VALUE = '0'
          .
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitCbEkstVPILev C-ArtKort 
PROCEDURE InitCbEkstVPILev :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR piEkstVPILevNr AS INT  NO-UNDO.
  DEF VAR pcTekst        AS CHAR NO-UNDO.
  DEF VAR piLoop         AS INT  NO-UNDO.

  RUN ReadFromLokalIni ("EKSTVPILEV", OUTPUT pcTekst).
  IF pcTekst <> "" THEN
  DO:
      ASSIGN
          piEkstVPILevNr = INT(pcTekst)
          .
      FIND FIRST EkstVPILev NO-LOCK WHERE 
        EkstVPILev.EkstVPILevNr = piEkstVPILevNr AND
        EkstVPILev.AktivLev     = TRUE NO-ERROR.
      IF AVAILABLE EkstVPILev THEN
          piEkstVPILevNr = EkstVPILev.EkstVPILevNr.
      ELSE
          piEkstVPILevNr = 0.
  END.
  IF piEkstVPILevNr = 0 THEN
  DO:
      {syspara.i 1 12 1 piEkstVPILevNr INT}
  END.
  IF NOT CAN-FIND(EkstVPILev WHERE 
      EkstVPILev.EkstVPILevNr = piEkstVPILevNr AND
      EkstVPILev.AktivLev     = TRUE) THEN
  DO:
      FIND FIRST EkstVPILev NO-LOCK WHERE 
      EkstVPILev.EkstVPILevNr = piEkstVPILevNr AND
      EkstVPILev.AktivLev     = TRUE NO-ERROR.
      IF AVAILABLE EkstVPILev THEN
          piEkstVPILevNr = EkstVPILev.EkstVPILEvNr.
  END.
  ASSIGN
      pcTekst = ""
      .

  DO WITH FRAME default-frame:
      LOOPEN:
      FOR EACH EkstVPILev NO-LOCK WHERE
          EkstVPILevNr      < 1000000 AND
          EkstVPILEv.PrioNr >= 0 AND
          EkstVPILev.AktivLev = TRUE
          BY EkstVPILEv.EkstVPILevNr:
          IF NOT CAN-FIND(FIRST VpiArtbas WHERE VpiArtbas.EkstVPILevNr = EkstVPILev.EkstVPILevNr) THEN
              NEXT.
          ASSIGN
              piLoop  = piLoop + 1
              pcTekst = pcTekst + 
                        (IF pcTekst <> ""
                           THEN ","
                           ELSE "") + 
                        EkstVPILEv.KortNavn + "," + 
                        string(EkstVPILev.EkstVPILevNr)
              .
/*           IF piLoop > 5 THEN */
/*               LEAVE LOOPEN.  */
      END. /*LOOPEN */

      IF NOT CAN-DO(pcTekst,STRING(piEkstVPILevNr)) AND NUM-ENTRIES(pcTekst) > 1 THEN
          piEkstVpiLevNr = INT(ENTRY(2,pcTekst)).
      IF pcTekst <> "" THEN DO:
          ASSIGN
          CB-EkstVPILev:LIST-ITEM-PAIRS = pcTekst
          CB-EkstVPILev = piEkstVPILevNr
          CB-EkstVPILev:SCREEN-VALUE = STRING(piEkstVPILevNr).
      END.
      ELSE
          ASSIGN
              CB-EkstVPILEv:HIDDEN = TRUE
              B-VPI:HIDDEN         = TRUE
              .
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
    T-1  
    T-2 WHEN cVisMaterial = "1" 
    T-3 WHEN cVisKlack     = "1"  
    T-4 WHEN cVisInnersula = "1"  
    T-5 WHEN cVisInnerfor  = "1"  
    T-6 WHEN cVisSlit      = "1"  
    T-7 WHEN cVisLast      = "1"  
    T-8 WHEN cVisBruks     = "1"  
    T-9  
    T-10
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
  DEFINE VARIABLE iFlikNr AS INTEGER    NO-UNDO.
  
  ASSIGN chTabStrip = chCtrlFrame:TabStrip
         chTabs     = chTabStrip:Tabs
         chTabs:Item(1):Caption = ENTRY(1,wTabTekst)
         chTabs:ITEM(1):KEY = "A1"
         iFlikNr    = 1.
         cAktivFlipTag = "A1".
  DO wCount = 2 TO NUM-ENTRIES(wTabTekst):
      IF ENTRY(wCount,cFlikEnaDis) = "D" THEN
          NEXT.
      iFlikNr = iFlikNr + 1.
      chTabs:Add(iFlikNr,"A" + STRING(wCount),ENTRY(wCount,wTabTekst)).
      cAktivFlipTag = cAktivFlipTag + "," + "A" + STRING(wCount).
  END.
 iAntFlip = iFlikNr.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitKalkyle C-ArtKort 
PROCEDURE InitKalkyle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF wModus = "NY" THEN
      RELEASE ArtPris.
  ELSE DO:
      FIND Butiker WHERE Butiker.Butik = wCl NO-LOCK NO-ERROR.
      IF AVAILABLE Butiker THEN
          FIND PrisProfil OF Butiker NO-LOCK NO-ERROR.
      IF AVAIL PrisProfil THEN
          FIND ArtPris NO-LOCK WHERE
               ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
               ArtPris.ProfilNr   = PrisProfil.ProfilNr NO-ERROR. 
      IF NOT AVAIL ArtPris THEN
          FIND FIRST ArtPris OF ArtBas NO-LOCK NO-ERROR.
  END.

  /* Oppdatering av Side 1 m.m */
  IF AVAILABLE ArtPris THEN
    DO:
      DISPLAY
        ArtPris.InnkjopsPris[1]       @ FILL-IN-InnPris
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
                                   IF ArtPris.Tilbud THEN 17 ELSE ?
        FILL-IN-SalgsPris:BGCOLOR IN FRAME FRAME-ArtInfo = 
                                 IF ArtPris.Tilbud THEN ? ELSE 18.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitTillgButikker C-ArtKort 
PROCEDURE InitTillgButikker :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FIND bruker WHERE bruker.brukerid = USERID("skotex") NO-LOCK.
    
    ASSIGN iBrukerButikk = IF Bruker.ButikkNr > 0 THEN Bruker.ButikkNr ELSE 0.
    
    FOR EACH ButikkTeam NO-LOCK WHERE 
        ButikkTeam.BrGrpNr = Bruker.BrGrpNr AND
        ButikkTeam.TeamTypeId = 1.
        
        FOR EACH ButikkKobling OF ButikkTeam.
            IF NOT CAN-DO(cTillgButikker,STRING(ButikkKobling.butik)) THEN
            ASSIGN cTillgButikker = cTillgButikker + (IF cTillgButikker <> "" THEN "," ELSE "") 
                              + STRING(ButikkKobling.butik).
        END.
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
  DEFINE VARIABLE cGetTabTekst   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cGetFlikEnaDis AS CHARACTER  NO-UNDO.
  /* OLD Artikkelinformasjon,Kalkyle,Lager,Bestilling,Historikk */
  /* NEW Feb 2002 "Artikkelinformasjon,Kalkyle,Strekkode,Pakke,Erstattningsvare,Bestilling,Lager,Transaksjoner,Historikk" */
  /* eftersom vi ändrat antal flips kan det vara fel i språk och vi behåller assignen från definition block */
  {Syspara.i 2 5 100 cGetTabTekst}
  {Syspar2.i 2 5 101 cGetFlikEnaDis}
  {Syspara.i 2 5 101 cSport1Default}
      
  ASSIGN         
    wTabTekst   = IF NUM-ENTRIES(cGetTabTekst)   = NUM-ENTRIES(wTabTekst)   THEN cGetTabTekst ELSE wTabTekst.
  IF NUM-ENTRIES(cGetFlikEnaDis) <> NUM-ENTRIES(cFlikEnaDis) THEN
    cGetFlikEnaDis = cSport1Default.
  ASSIGN
    cFlikEnaDis = IF NUM-ENTRIES(cGetFlikEnaDis) = NUM-ENTRIES(cFlikEnaDis) THEN cGetFlikEnaDis ELSE cFlikEnaDis
    ENTRY(1,cFlikEnaDis) = "E"
    ENTRY(2,cFlikEnaDis) = "E"
    ENTRY(3,cFlikEnaDis) = "E".
    cFlikDefault = cFlikEnaDis.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitVareslag C-ArtKort 
PROCEDURE InitVareslag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF VAR pcTekst AS CHAR NO-UNDO.

  ASSIGN
      pcTekst = ""
      .
  DO WITH FRAME {&FRAME-NAME}:
      FOR EACH SysPara NO-LOCK WHERE
          SysPara.SysHId = 2 AND
          SysPara.SysGr  = 7:

          ASSIGN
              pcTekst = pcTekst + 
                        (IF pcTekst <> ""
                           THEN ","
                           ELSE "") + 
                        SysPara.Beskrivelse + "," + 
                        (SysPara.Parameter1)
              .
      END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KopierBilde C-ArtKort 
PROCEDURE KopierBilde :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iBildeNr    AS INTEGER    NO-UNDO.
    DEFINE INPUT  PARAMETER dArtikkelNr AS DECIMAL    NO-UNDO.
    DEFINE INPUT  PARAMETER iOldBildNr  AS INTEGER    NO-UNDO.

    DEFINE VARIABLE iTst AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cFilNavn AS CHARACTER  NO-UNDO.
    DEFINE BUFFER NyttBilderegister FOR Bilderegister.
    DEFINE BUFFER OldBilderegister FOR Bilderegister.
    DEFINE BUFFER NyttBildedata FOR Bildedata.


    ASSIGN iTst = dArtikkelNr NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        RETURN.

    FIND Bilderegister WHERE Bilderegister.bildnr = iBildeNr NO-LOCK NO-ERROR.
    IF NOT AVAIL Bilderegister THEN
        RETURN.
    DO TRANSACTION:
        IF dArtikkelnr = iOldBildNr THEN DO: /* den gamla bilden skall tas bort */
            FIND oldBilderegister WHERE oldBilderegister.bildnr = iOldBildNr NO-ERROR.
            IF AVAIL oldBilderegister THEN DO:
                FOR EACH Bildedata OF oldBilderegister:
                    DELETE bildedata.
                END.
                DELETE oldBilderegister.
            END.
        END.
        NYTTBILDE: 
        DO:
            FIND Bilderegister WHERE Bilderegister.bildnr = iBildeNr NO-LOCK NO-ERROR.
            ASSIGN cFilnavn = STRING(dArtikkelNr) + "." + ENTRY(2,Bilderegister.Filnavn,".").
            CREATE NyttBilderegister.
            BUFFER-COPY Bilderegister EXCEPT Bilderegister.Bildnr Bilderegister.Filnavn TO NyttBilderegister
                ASSIGN NyttBilderegister.bildnr = dArtikkelnr
                       NyttBilderegister.Filnavn = cFilnavn NO-ERROR.
            IF ERROR-STATUS:ERROR THEN DO:
                DELETE NyttBildeRegister.
                LEAVE NYTTBILDE.
            END.
            FOR EACH Bildedata WHERE Bildedata.BildNr = iBildenr NO-LOCK.
                CREATE NyttBildeData.
                BUFFER-COPY Bildedata EXCEPT Bildedata.bildnr TO NyttBildedata
                    ASSIGN Nyttbildedata.Bildnr = dArtikkelnr NO-ERROR.
                IF ERROR-STATUS:ERROR THEN DO:
                    DELETE NyttBildedata.
                    LEAVE NYTTBILDE.
                END.
                RELEASE NyttBildedata.
            END.
            RELEASE NyttBilderegister.
            FIND CURRENT ArtBas EXCLUSIVE.
            ASSIGN ArtBas.Bildnr = dArtikkelnr.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Lagerjustering C-ArtKort 
PROCEDURE Lagerjustering :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iTransType AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cLagerAnt       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cStrListe       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cIOLagerAnt     AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE dVVarekost      AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE dKalkVarekost   AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE dEndringVarekost AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE cArtikkelEti    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cEtiketter      AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cAntallEti      AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cEtiketterStorl AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cIndividNr      AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cIndivid        AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cAntall         AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE iCount          AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iCount2         AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iIndividBatchNr AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cTmpStr         AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE iTst            AS INTEGER    NO-UNDO.
    APPLY "CHOOSE" TO B-OppdLAger IN FRAME FRAME-Lager.
    PROCESS EVENTS.

    DO WITH FRAME Default-Frame:
        IF /*CAN-DO('7,8,9',STRING(iTransType)) AND*/ cBrukPwLagerTrans <> '0' THEN 
        DO:
          RUN d-adgangskontroll.w(INPUT 'Adgangskontroll',INPUT cPwd).
          IF RETURN-VALUE <> 'OK' THEN RETURN 'AVBRYT'.
        END.
        IF INPUT ArtBas.Lager = FALSE THEN
        DO:
            MESSAGE "Artikkelen er ikke lagerstyrt."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RETURN.
        END.
        IF Artbas.sanertdato <> ? THEN DO:
            MESSAGE "Artikkelen er sanert"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RETURN.
        END.
    END.
    IF NOT CAN-FIND(FIRST StrTStr WHERE StrTStr.StrTypeID = artbas.StrtypeId) THEN DO:
        MESSAGE "Størrelsestypen mangler størrelser." SKIP
                "Kontakt systemadministratør."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN.
    END.
    ASSIGN iTst = INT(tmpLager.Butik) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN /* Vi står på Totallinjen */
        RETURN NO-APPLY.
    IF NOT lagerButikk(OUTPUT cStrListe,OUTPUT cLagerAnt) THEN DO:
        MESSAGE "Manglende behørighet."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.
    IF CAN-FIND(FIRST Translogg WHERE Translogg.ArtikkelNr = ArtBas.ArtikkelNr AND
                                      Translogg.Butik = INT(tmplager.butik) AND
                TransLogg.Postert = FALSE) THEN DO:
        MESSAGE "Det finnes ikke oppdaterte transaksjoner for denne artikkelen" SKIP
                "Kontroller at batchserveren kjører."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN.
    END.

    ELSE IF iTransType = 5 THEN DO:
        /* Vi sänder in 1 ArtBas men behandlar eventuellt hel modell, därför en artikkellista tillbaka */
        IF artbas.modellfarge <> 0 THEN
        MESSAGE "Varen ingår i modell." SKIP
            "Skall flere varer i modellen inleveres?"
            VIEW-AS ALERT-BOX BUTTONS YES-NO UPDATE lHeleModell AS LOGICAL.
        RUN w-Varemottak.w (tmplager.butik,ROWID(ArtBas),cMottakstr,cBrukteStr,lHelemodell,OUTPUT cArtikkelEti,OUTPUT cEtiketter,OUTPUT cAntall,OUTPUT cIndividNr,OUTPUT iIndividBatchNr).
/*         RUN d-Varemottak.w (tmplager.butik,ROWID(ArtBas),OUTPUT cArtikkelEti,OUTPUT cEtiketter,OUTPUT cAntall,OUTPUT cIndividNr,OUTPUT iIndividBatchNr). */
        IF cAntall <> "" AND NOT Artbas.IKasse:CHECKED IN FRAME DEFAULT-FRAME THEN DO:
            ArtBas.IKasse:CHECKED = TRUE.
            RUN LagreArtBas (99).
        END.
        IF ArtBas.etikett > 0 AND cArtikkelEti <> "" THEN DO iCount2 = 1 TO NUM-ENTRIES(cArtikkelEti,CHR(1)):
            FIND etiArtBas WHERE etiArtBas.ArtikkelNr = DECI(ENTRY(iCount2,cArtikkelEti,CHR(1))) NO-LOCK NO-ERROR.
            IF NOT AVAIL etiArtBas THEN
                NEXT.
            ASSIGN cEtiketterStorl = ENTRY(iCount2,cEtiketter,CHR(1))
                   cAntallEti      = ENTRY(iCount2,cAntall,CHR(1))
                   cIndivid        = ENTRY(iCount2,cIndividNr,CHR(1)).
            /* Hær skall vi kunna komplettera med officiella strekkoder */
            IF cEtiketterStorl <> "" THEN DO:
                cTmpStr = AlleEANok(cEtiketterStorl).
                IF dSkomodus = FALSE AND cTmpStr <> "" THEN DO:
                    ASSIGN dLevArtikkelnr = etiArtBas.ArtikkelNr
                           cLevStorrelser = cTmpStr.
                    RUN StrekKode.w (THIS-PROCEDURE).
                END.
                IF NOT StorlToEAN(INPUT-OUTPUT cEtiketterStorl) THEN
                    LEAVE.
                IF NOT VALID-HANDLE(hEtikettVindu) THEN
                    RUN w-TmpEtikett.w PERSISTENT SET hEtikettVindu (C-ArtKort).
                CREATE tmpChild.
                ASSIGN tmpChild.wChild = hEtikettVindu.
                IF VALID-HANDLE(hEtikettVindu) THEN DO iCount = 1 TO NUM-ENTRIES(cEtiketterStorl):
                    IF ENTRY(iCount,cEtiketterStorl) <> "" THEN
                        RUN NyEtikettInlev IN hEtikettVindu (ENTRY(iCount,cEtiketterStorl),INT(ENTRY(iCount,cAntallEti)),INT(ENTRY(iCount,cIndivid))).
                END.
                /*   RUN ButtonEnaDis. */
            END.
        END.
        IF iIndividBatchNr > 0 THEN
            RUN gIndividSerie.w (INPUT iIndividBatchNr).
        IF VALID-HANDLE(hIndivid) THEN
            RUN OpenQueryIndivid IN hIndivid NO-ERROR.
        ASSIGN CURRENT-WINDOW                = C-ArtKort
               THIS-PROCEDURE:CURRENT-WINDOW = C-ArtKort.

        RETURN.
    END.
    ELSE IF iTransType = 8 AND INT(tmplager.SumAntall) <= 0 THEN DO:
        MESSAGE "Nedskrivning ikke tillatt. Sumlager <= 0"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN.
    END.
    FIND Lager WHERE Lager.ArtikkelNr = ArtBas.ArtikkelNr AND
                     Lager.Butik      = INT(tmplager.butik) NO-LOCK NO-ERROR.
    ASSIGN cIOLagerAnt = cLagerAnt
           dVVarekost  = IF AVAIL Lager 
                           THEN Lager.VVarekost 
                           ELSE DECI(tmplager.VVarekost)
           dKalkVarekost = dVVarekost.
    RUN d-JustGrid.w (iTransType,(IF iTransType = 8 THEN "Endre varekost"
                                  ELSE IF iTransType = 7 THEN "Lagerjustering"
                                  ELSE "Svinn"),REPLACE(cStrListe,";"," "),cLagerAnt,cBrukteStr,INPUT-OUTPUT cIOLagerAnt,INPUT-OUTPUT dVVarekost).
/*     IF iTransType = 7 OR iTransType = 9 THEN DO: */
    IF (iTransType = 8 AND dVVarekost <> ?) OR cLagerAnt <> cIOLagerAnt THEN
    DO:
        /* Om antal störrelser = 1 och vi har haft neg lager och detta ändras till '0' är det lagt på ett ',' */
        IF NUM-ENTRIES(TRIM(cLagerAnt)," ") = 1 THEN
            cIOLagerAnt = TRIM(cIOLagerAnt,",").
        IF iTransType = 8 THEN /* d-JustGrid gör inget med cIOLagerAnt och måste manipuleras */
            ASSIGN cIOLagerAnt = REPLACE(TRIM(cIOLagerAnt)," ",",").
        IF dVVarekost <> ? THEN
            ASSIGN dEndringVarekost = dKalkVarekost - dVVarekost.
        ELSE
            ASSIGN dEndringVarekost = dVVarekost.
        /* ta bort en parentes */
        DO iCount = 1 TO NUM-ENTRIES(cLagerStrListe,";"):
            IF ENTRY(iCount,cLagerStrListe,";") BEGINS "(" THEN DO:
                ASSIGN cTmpStr = ENTRY(iCount,cLagerStrListe,";")
/*   !!                     cTmpStr = SUBSTR(cTmpStr,2)                     */
/*   !!                     cTmpStr = SUBSTR(cTmpStr,1,LENGTH(cTmpStr) - 1) */
/* !! */            cTmpStr = TRIM(cTmpStr)
/* !! */            cTmpStr = REPLACE(cTmpStr,"(","")
/* !! */            cTmpStr = REPLACE(cTmpStr,")","")
/* /* !! */            cTmpStr = SUBSTR(cTmpStr,1,LENGTH(cTmpStr) - 1) */

                       ENTRY(iCount,cLagerStrListe,";") = cTmpStr.
            END.
        END.
/*         ASSIGN cLagerStrListe = REPLACE(REPLACE(cLagerStrListe,"(",""),")",""). */
        ASSIGN cIOLagerAnt = REPLACE(cIOLagerAnt,"+","").
        RUN oppdatlagerjust.p (tmplager.Butik,ArtBas.ArtikkelNr,iTransType,cLagerStrListe,cIOLagerAnt,dEndringVarekost).
/*         RUN oppdatlagerjust.p (tmplager.Butik,ArtBas.ArtikkelNr,iTransType,cLagerStrListe,cIOLagerAnt,dVVarekost * -1). */
        IF NOT Artbas.IKasse:CHECKED IN FRAME DEFAULT-FRAME THEN DO:
            ArtBas.IKasse:CHECKED = TRUE.
            RUN LagreArtBas (99).
        END.
    END.
    ELSE
        MESSAGE "Ingen justering foretatt"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lagreAltLevBas C-ArtKort 
PROCEDURE lagreAltLevBas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DEF VAR pcTekst AS CHAR NO-UNDO.
  DEF VAR piLoop  AS INT  NO-UNDO.

  IF NOT AVAILABLE ArtBas THEN
      RETURN.

  DEF BUFFER bLevBas FOR LevBas.
  DO WITH FRAME FRAME-ArtInfo:
      pcTekst = "".
      FOR EACH AltLevBAs NO-LOCK WHERE 
          AltLevBas.ArtikkelNr = ArtBas.ArtikkelNr:

          FIND bLevBas OF AltLevBas NO-LOCK WHERE
              bLevBas.LevNr = AltLevBas.LevNr NO-ERROR.
          IF AVAILABLE bLevBas THEN
              pcTekst = pcTekst + 
                        (IF pcTekst = ""
                           THEN ""
                           ELSE ",") + 
                        STRING(bLevBas.LevNr) + " / " + bLevBas.LevNamn + "," + STRING(bLevBas.LevNr).
      END.

      IF pcTekst <> S-AlternativLev:LIST-ITEM-PAIRS THEN
      DO TRANSACTION:
          /* Renser bort gammal dr... */
          FOR EACH AltLevBAs WHERE 
              AltLevBas.ArtikkelNr = ArtBas.ArtikkelNr:
              DELETE AltLevBas.
          END.
          /* Legger opp nytt. */
          DO piLoop = 1 TO (NUM-ENTRIES(S-AlternativLev:LIST-ITEM-PAIRS) / 2):
              CREATE AltLevBas.
              ASSIGN
                  AltLevBas.ArtikkelNr = ArtBas.ArtikkelNr
                  AltLevBas.LevNr      = int(ENTRY(piLoop * 2,S-AlternativLev:LIST-ITEM-PAIRS))
                  .
          END.
      END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LagreArtBas C-ArtKort 
PROCEDURE LagreArtBas :
/*--------------------------------------------------------*/
DEF INPUT PARAMETER wBekreft AS INT NO-UNDO.
DEF VAR ipStatus AS CHAR INIT "AVBRYT" NO-UNDO.
DEF VAR cTekst02Koder    AS CHAR  NO-UNDO.
DEF VAR cTekstAndreKoder AS CHAR  NO-UNDO.
DEF VAR endretStrtype AS LOG    NO-UNDO.
DEF VAR lBekreftStEndring AS LOG    NO-UNDO.
DEF VAR lSlettStrekMstrl   AS LOG    NO-UNDO.
DEF VAR lSlettInterneStrek AS LOG    NO-UNDO.
DEF VAR cTxt AS CHAR NO-UNDO.
DEF VAR iPakkenr AS INT    NO-UNDO.
DEF VAR cTmpModus AS CHAR NO-UNDO.
DEF VAR iArtRecid AS RECID NO-UNDO.
DEF VAR iLopNr AS INT    NO-UNDO.
DEF VAR bKjedevare AS LOG NO-UNDO.
DEF BUFFER bArtBas  FOR ArtBas.
DEF BUFFER bArtPris FOR ArtPris.  
  IF wModus <> "NY" AND Artbas.sanertdato <> ? THEN DO:
    IF wbekreft = 0 THEN
    MESSAGE "Artikkelen er sanert" VIEW-AS ALERT-BOX INFO BUTTONS OK.
    APPLY "CHOOSE" TO BUTTON-Angre IN FRAME DEFAULT-FRAME.
    RETURN.
  END.
  /*IF PostFelt() = FALSE THEN RETURN.*/
  IF wAktivFlip > 3 AND wBekreft <> 99 THEN RETURN "OK".
  IF wBekreft = 99 THEN wBekreft = 0.
  IF wModus <> "NY" THEN ModellBytte(). 
    ELSE ASSIGN cORG = '' cENDRET = ''.
  /* Sjekker input */
  DO:
    IF Artbas.StrTypeId:SCREEN-VALUE IN FRAME FRAME-ArtInfo = "1" THEN DO:
      RUN GetTx (141,OUTPUT cTxt).
      MESSAGE cTxt VIEW-AS ALERT-BOX WARNING. 
      APPLY "ENTRY" TO ArtBas.StrTypeId.
      RETURN ipStatus.
    END.
    ELSE IF wModus <> "NY" AND INPUT Artbas.StrTypeId <> ArtBas.StrTypeID THEN DO:
      IF CAN-FIND(FIRST StrekKode OF ArtBas WHERE StrekKode.Kode BEGINS "02" AND LENGTH(StrekKode.Kode) = 13) THEN
        ASSIGN cTekst02Koder = "Du har byttet størrelsestype og det finnes bedriftsinterne strekkoder (02....)" + CHR(10) +
               "registrert for annen størrelsestype." + CHR(10) 
            /* + ("Disse tas bort" + IF cGenEan = "yes" THEN " og nye genereres." ELSE ".") + CHR(10) */.
      IF CAN-FIND(FIRST StrekKode OF ArtBas WHERE NOT StrekKode.Kode BEGINS "02" AND LENGTH(StrekKode.Kode) = 13) THEN
          ASSIGN cTekstAndreKoder = "Øvrige strekkoder bør også kontrolleres." + CHR(10).
      IF cTekst02Koder <> "" OR cTekstAndreKoder <> "" THEN DO:
        MESSAGE cTekst02Koder cTekstAndreKoder "Ønsker du å fortsette?" VIEW-AS ALERT-BOX INFO BUTTONS YES-NO TITLE 'Bekreft endring størrelsestype'
        UPDATE lBekreftStEndring.
        IF NOT lBekreftStEndring THEN DO:
          RUN VisStrlType.
          RETURN ipStatus.
        END.
      END.
      ASSIGN lSlettInterneStrek = TRUE.
    END.
  END.
  FIND VarGr NO-LOCK WHERE VarGr.Vg = int(ArtBas.Vg:screen-value IN FRAME DEFAULT-FRAME) NO-ERROR.
  IF NOT AVAILABLE VarGr OR int(ArtBas.Vg:screen-value IN FRAME DEFAULT-FRAME) = 0 THEN DO:
      MESSAGE Tx("Ukjent varegruppe!",108) VIEW-AS ALERT-BOX WARNING TITLE Tx("Lagringsfeil",109).
      APPLY "ENTRY":U TO ArtBas.Vg IN FRAME DEFAULT-FRAME.
      RETURN ipStatus.
    END.
  FIND FIRST VgKat NO-LOCK WHERE VgKat.Vg = int(ArtBas.Vg:screen-value IN FRAME DEFAULT-FRAME) AND
    VgKat.VgKat = INPUT ArtBas.VgKat NO-ERROR.
  IF NOT AVAILABLE VgKat THEN DO:
    MESSAGE Tx("Ukjent innkjøpskategori!",110) VIEW-AS ALERT-BOX TITLE Tx("Melding",107).
    APPLY "ENTRY":U TO ArtBas.VgKat IN FRAME DEFAULT-FRAME.
    RETURN ipStatus.
  END.
  IF int(ArtBas.LevNr:screen-value IN FRAME default-frame) = 0 THEN DO:
    MESSAGE Tx("Ugyldig leverandørnummer!",111) VIEW-AS ALERT-BOX TITLE Tx("Melding",107).
    RETURN ipStatus.
  END.
  FIND LevBas NO-LOCK WHERE LevBas.LevNr = int(ArtBas.LevNr:screen-value IN FRAME DEFAULT-FRAME) NO-ERROR.
  IF NOT AVAILABLE LevBas THEN DO:
    MESSAGE Tx("Ukjent leverandør!",114) VIEW-AS ALERT-BOX WARNING TITLE Tx("Melding",107).
    APPLY "ENTRY":U TO ArtBas.LevNr IN FRAME DEFAULT-FRAME.
    RETURN ipStatus.
  END.
  FIND Farg NO-LOCK WHERE Farg.Farg = int(ArtBas.Farg:screen-value IN FRAME FRAME-ArtInfo) NO-ERROR.
  IF NOT AVAILABLE Farg THEN DO:
    MESSAGE Tx("Ukjent farge!",115) VIEW-AS ALERT-BOX WARNING TITLE Tx("Melding",107).
    APPLY "ENTRY":U TO ArtBas.Farg IN FRAME FRAME-ArtInfo.
    RETURN ipStatus.
  END.
  FIND Sasong NO-LOCK WHERE Sasong.Sasong = int(ArtBas.Sasong:screen-value IN FRAME FRAME-ArtInfo) NO-ERROR.
  IF NOT AVAILABLE Sasong THEN DO:
    MESSAGE Tx("Ukjent sesong!",116) VIEW-AS ALERT-BOX WARNING TITLE Tx("Melding",107).
    APPLY "ENTRY":U TO ArtBas.Sasong IN FRAME FRAME-ArtInfo.
    RETURN ipStatus.
  END.
  FIND Valuta NO-LOCK WHERE Valuta.ValKod = INPUT ArtBas.ValKod NO-ERROR.
  IF NOT AVAILABLE Valuta THEN DO:
    MESSAGE Tx("Ukjent valuta!",117) VIEW-AS ALERT-BOX WARNING TITLE Tx("Melding",107).
    APPLY "ENTRY":U TO ArtBas.ValKod IN FRAME FRAME-ArtInfo.
    RETURN ipStatus.
  END.
  IF int(ArtBas.RAvdNr:screen-value IN FRAME FRAME-ArtInfo) > 0 THEN DO:
    FIND Regnskapsavdeling NO-LOCK WHERE
      Regnskapsavdeling.RAvdNr = int(ArtBas.RAvdNr:screen-value IN FRAME FRAME-ArtInfo) NO-ERROR.
    IF NOT AVAILABLE Regnskapsavdeling THEN DO:
      MESSAGE Tx("Ukjent regnskapsavdeling!",118) VIEW-AS ALERT-BOX WARNING TITLE Tx("Melding",107).
      APPLY "ENTRY":U TO ArtBas.RAvdNr IN FRAME FRAME-ArtInfo.
      RETURN ipStatus.
    END.
  END.
  FIND StrType NO-LOCK WHERE StrType.StrTypeID = INPUT ArtBas.StrTypeId NO-ERROR.
  IF NOT AVAILABLE StrType THEN DO:
    MESSAGE Tx("Ukjent størrelsestype!",119) VIEW-AS ALERT-BOX TITLE "Melding".
    RETURN ipStatus.
  END.
  ELSE IF AVAIL StrType AND StrType.Intervall = "" AND INPUT ArtBas.Pakke <> TRUE THEN DO:
    MESSAGE "Størrelsestypen feilaktig, mangler definerte størrelser." VIEW-AS ALERT-BOX INFO BUTTONS OK.
    IF NOT wModus = "NY" AND AVAIL ArtBas THEN
      ASSIGN ArtBas.StrTypeId:SCREEN-VALUE IN FRAME Frame-ArtInfo = STRING(ArtBas.StrTypeId).
    ELSE
      DISPLAY "" @ ArtBas.StrTypeID
              "" @ StrType.Beskrivelse WITH FRAME Frame-ArtInfo.
    RETURN ipStatus.
  END.
  IF cVisKlack = "1" THEN DO:
    FIND Klack NO-LOCK WHERE Klack.Klack-Id = int(ArtBas.Klack:screen-value IN FRAME FRAME-ArtInfo) NO-ERROR.
    IF NOT AVAILABLE Klack THEN DO:
      MESSAGE Tx("Ukjent hælkode!",140) VIEW-AS ALERT-BOX WARNING TITLE Tx("Melding",107).
      APPLY "ENTRY":U TO ArtBas.Klack IN FRAME FRAME-ArtInfo.
      RETURN ipStatus.
    END.
  END.
  IF cVisBehKod = "1" THEN DO:
    FIND Handtering NO-LOCK WHERE Handtering.HandKode = int(ArtBas.BehKode:screen-value IN FRAME FRAME-ArtInfo) NO-ERROR.
    IF NOT AVAILABLE Handtering THEN DO:
      MESSAGE Tx("Ukjent håndteringskode!",141) VIEW-AS ALERT-BOX WARNING TITLE Tx("Melding",107).
      APPLY "ENTRY":U TO ArtBas.BehKode IN FRAME FRAME-ArtInfo.
      RETURN ipStatus.
    END.
  END.
  IF (INPUT ArtBas.LinkVareNr > 0 AND NOT CAN-FIND(ArtBas WHERE ArtBas.ArtikkelNr = INPUT ArtBas.LinkVareNr AND ArtBas.Pant = TRUE)) THEN 
  DO:
    MESSAGE 'Link kan bare legges inn på varer som er satt opp som pantvare.' SKIP 'Lagring avbrutt.' VIEW-AS ALERT-BOX ERROR.
    APPLY 'entry' TO ArtBas.LinkVareNr.
    RETURN ipStatus.
  END.
  IF ArtBas.AlfaKode2:SCREEN-VALUE IN FRAME FRAME-Info2 <> '' THEN DO:
      IF NOT CAN-FIND(FIRST AlfaLandKode WHERE AlfaLandKode.AlfaKode2 = ArtBas.AlfaKode2:SCREEN-VALUE IN FRAME FRAME-Info2) THEN DO:
          MESSAGE 'Ugyldig landkode' VIEW-AS ALERT-BOX.
          APPLY 'ENTRY' TO ArtBas.AlfaKode2 IN FRAME Frame-Info2.
      END.
  END.
  LAGRE_ARTBAS:
DO TRANSACTION:
    IF AVAILABLE ArtBas THEN iArtRecid = RECID(ArtBas). ELSE iArtRecid = 0.
    IF wModus = "NY" THEN DO:
      ASSIGN cTmpModus = wModus.
      IF wKopi THEN
          FIND bArtBas WHERE RECID(bArtBas) = RECID(ArtBas) NO-LOCK NO-ERROR.
      RELEASE ArtBas. /* Slipper gammel post. */
      IF wForslagLopNr <> "" AND (INT(ArtBas.LopNr:SCREEN-VALUE) = ? OR
                                 INT(ArtBas.LopNr:SCREEN-VALUE) = 0) THEN DO:
          RUN SettLopNr.p (INT(ArtBas.Vg:SCREEN-VALUE),wForslagLopNr,OUTPUT iLopNr).
          ASSIGN ArtBas.Lopnr:SCREEN-VALUE = STRING(iLopNr).
      END.
      IF int(ArtBas.LopNr:screen-value IN FRAME DEFAULT-FRAME) <> ? AND
         int(ArtBas.LopNr:screen-value IN FRAME DEFAULT-FRAME) <> 0  THEN
        FIND ArtBas NO-LOCK WHERE
          ArtBas.Vg    = int(ArtBas.Vg:screen-value IN FRAME DEFAULT-FRAME) AND
          ArtBas.LopNr = int(ArtBas.LopNr:screen-value IN FRAME DEFAULT-FRAME) NO-ERROR.
      IF AVAILABLE ArtBas THEN
        DO:
          MESSAGE Tx("Artikkel med dette Varegruppe/løpenummer finnes fra før!",120) VIEW-AS ALERT-BOX WARNING TITLE Tx("Melding",107).
          APPLY "ENTRY":U TO ArtBas.LopNr IN FRAME DEFAULT-FRAME.
          RETURN ipStatus.
        END.
      IF INPUT ArtBas.Pakke = TRUE THEN DO:
          ASSIGN iPakkenr = getPakkeNr().
          IF iPakkeNr = ? THEN DO:
              MESSAGE "Det finnes inget ledigt pakkenr." VIEW-AS ALERT-BOX INFO BUTTONS OK.
              RETURN ipStatus.
          END.
      END.
      /* Artikkelnummer tildeles i databasetrigger. */
      CREATE ArtBas.
      IF ArtBas.ArtikkelNr = 0 THEN DO:
          IF AVAILABLE ArtBas THEN DELETE ArtBas.
          IF iArtRecid > 0 THEN FIND ArtBas WHERE RECID(ArtBas) = iArtRecid NO-LOCK NO-ERROR.
          RETURN ipStatus.
      END.
      IF wKopi = FALSE THEN RUN NyttBilde (BUTTON-Paste:PRIVATE-DATA).
      IF wKopi THEN ASSIGN ArtBas.BildNr = iKopierBildeNr iKopierBildeNr = 0 ArtBas.WebButikkArtikkel = FALSE ArtBas.PubliserINettbutikk = FALSE.
      ASSIGN
       wModus       = "ENDRE"
       wArtBasRecid = RECID(ArtBas)
       ArtBas.Vg    = int(ArtBas.Vg:screen-value IN FRAME DEFAULT-FRAME)
       ArtBas.LopNr = IF int(ArtBas.LopNr:screen-value IN FRAME DEFAULT-FRAME) > 0
                        THEN int(ArtBas.LopNr:screen-value IN FRAME DEFAULT-FRAME)
                      ELSE ?
       ArtBas.Pakke  = INPUT ArtBas.Pakke
       ArtBas.iKasse:SENSITIVE = TRUE
       ArtBas.Pakkenr = iPakkenr
       ArtBas.ManueltOpprettet = INPUT ArtBas.ManueltOpprettet
       ArtBas.Grunnsortiment  = INPUT  ArtBas.Grunnsortiment
       ArtBas.Telefonkort = INPUT  ArtBas.Telefonkort
       ArtBas.StrTypeId = INPUT ArtBas.StrTypeId.
      IF ArtBas.Lopnr = 0 THEN ASSIGN ArtBas.LopNr = ?.
      FIND CURRENT ArtBas.
      DISPLAY ArtBas.ArtikkelNr WITH FRAME DEFAULT-FRAME.
      DISPLAY ArtBas.WebButikkArtikkel ArtBas.PubliserINettbutikk WITH FRAME FRAME-ArtInfo.
      /* Kopierer kalkyle. */
      IF wKopi AND AVAILABLE bArtBas THEN
      DO:
        FOR EACH ArtPris WHERE ArtPris.ArtikkelNr = ArtBas.ArtikkelNr:
          DELETE ArtPris.
        END.
        FIND FIRST bArtPris OF bArtBas NO-ERROR.
        IF AVAILABLE bArtPris THEN DO:
          CREATE ArtPris.
          ASSIGN
           ArtPris.ArtikkelNr = ArtBas.ArtikkelNr
           ArtPris.AktivFraDato = TODAY
           ArtPris.AktivFraTid = TIME
           ArtPris.DB%[1] = bArtPris.DB%[1]
           ArtPris.DBKr[1] = bArtPris.DBKr[1]
           ArtPris.DivKost%[1] = bArtPris.DivKost%[1]
           ArtPris.DivKostKr[1] = bArtPris.DivKostKr[1]
           ArtPris.EuroManuel = bArtPris.EuroManuel
           ArtPris.EuroPris[1] = bArtPris.EuroPris[1]
           ArtPris.Frakt%[1] = bArtPris.Frakt%[1]
           ArtPris.Frakt[1] = bArtPris.Frakt[1]
           ArtPris.InnkjopsPris[1] = bArtPris.InnkjopsPris[1]
           ArtPris.LevNr = bArtPris.LevNr
           ArtPris.MomsKod[1] = bArtPris.MomsKod[1]
           ArtPris.Mva%[1] = bArtPris.Mva%[1]
           ArtPris.MvaKr[1] = bArtPris.MvaKr[1]
           ArtPris.Pris[1] = bArtPris.Pris[1]
           ArtPris.ProfilNr = bArtPris.ProfilNr
           ArtPris.Rab1%[1] = bArtPris.Rab1%[1]
           ArtPris.Rab1Kr[1] = bArtPris.Rab1Kr[1]
           ArtPris.Rab2%[1] = bArtPris.Rab2%[1]
           ArtPris.Rab2Kr[1] = bArtPris.Rab2Kr[1]
           ArtPris.Rab3%[1] = bArtPris.Rab3%[1]
           ArtPris.Rab3Kr[1] = bArtPris.Rab3Kr[1]
           ArtPris.ValPris[1] = bArtPris.ValPris[1]
           ArtPris.VareKost[1] = bArtPris.VareKost[1].
        END.
      END.
      IF ArtBas.Lager:SCREEN-VALUE = "J" THEN DO:
        BUTIKKLOOP:
        FOR EACH Butiker NO-LOCK BREAK BY Butiker.ProfilNr:
          CREATE Lager.
          ASSIGN Lager.ArtikkelNr = ArtBas.ArtikkelNr
                 Lager.Butik      = Butiker.Butik.                    
          RELEASE Lager.
        END. /* BUTIKKLOOP */
      END.              
    END.
    ELSE DO:
      FIND ArtBas EXCLUSIVE-LOCK WHERE RECID(ArtBas) = wArtBasRecid NO-ERROR NO-WAIT.
    END.
    IF LOCKED ArtBas THEN DO:
      MESSAGE Tx("Artikkelen oppdateres fra en annen terminal" + chr(13) + "Forsöke å lagre en gang til",121) VIEW-AS ALERT-BOX 
        WARNING TITLE Tx("Melding",107).
      RETURN NO-APPLY ipStatus.
    END.
    ELSE IF NOT AVAILABLE ArtBas THEN
      DO:
        MESSAGE Tx("Artikkelen er slettet!",122) SKIP VIEW-AS ALERT-BOX WARNING TITLE Tx("Melding",107).
       RETURN NO-APPLY ipStatus.
      END.
    IF INPUT ArtBas.Vg <> ArtBas.Vg AND ArtBas.Lopnr > 0 THEN DO:
      RUN SettLopNr.p (INPUT INPUT ArtBas.Vg,"N",OUTPUT iLopNr).
      ASSIGN ArtBas.Lopnr:SCREEN-VALUE = STRING(iLopNr)
           FRAME DEFAULT-FRAME ArtBas.Vg 
           FRAME DEFAULT-FRAME ArtBas.LopNr.
    END.
    DO WITH FRAME DEFAULT-FRAME:
      IF (ArtBas.LokPris:CHECKED = FALSE AND ArtBas.LokPris = TRUE) THEN RUN artbas_bekreft_slett_artpris.p (ArtBas.ArtikkelNr). 
      bKjedevare = FALSE.
      IF STRING(ArtBas.KjedeVare) <> ArtBas.KjedeVare:SCREEN-VALUE THEN bKjedevare = TRUE. 
      ASSIGN ArtBas.Vg ArtBas.VgKat ArtBas.Beskr  ArtBas.LevNr ArtBas.LevKod ArtBas.Utgatt TB-WebButikkArtikkel ArtBas.LokPris ArtBas.Grunnsortiment ArtBas.TelefonKort ArtBas.HoyLavMva 
       ArtBas.KjedeVare ArtBas.Gjennomfaktureres ArtBas.Lager ArtBas.iKasse ArtBas.BildeIKasse ArtBas.Opris ArtBas.NON_Sale ArtBas.NegVare TOGGLE-Annonse ArtBas.ManueltOpprettet
       ArtBas.Pakke ArtBas.BestForsl ArtBas.Pant ArtBas.KjentPaHK ArtBas.IndividType ArtBas.AnonseArtikkel = TOGGLE-Annonse Artbas.sanertdato ArtBas.UtgattDato.
    END.
    IF ArtBas.Pakke = TRUE AND ArtBas.PakkeNr = 0 THEN DO:
      ASSIGN iPakkenr = getPakkeNr().
      IF iPakkeNr <> ? THEN ArtBas.PakkeNr = iPakkeNr.          
    END.
    ELSE IF ArtBas.Pakke = FALSE AND ArtBas.PakkeNr > 0 THEN ArtBas.PakkeNr = 0.    
    ArtBas.Hg = VarGr.Hg.
    DO WITH FRAME FRAME-ArtInfo:
    IF ArtBas.BongTekst:SCREEN-VALUE = "" THEN
      ASSIGN ArtBas.BongTekst:SCREEN-VALUE = SUBSTR(ArtBas.Beskr,1,20).
    ASSIGN
     ArtBas.GarantiKl ArtBas.OnLineLevNr ArtBas.LevFargKod ArtBas.ValKod ArtBas.VMId ArtBas.LeveringsTid ArtBas.VareType ArtBas.WebLeveringstid ArtBas.WebMinLager 
     ArtBas.ProdNr ArtBas.StrTypeId = INPUT ArtBas.StrTypeId ArtBas.Notat ArtBas.SaSong ArtBas.Kampanjekode ArtBas.KundeRabatt ArtBas.MengdeRabatt
     ArtBas.Farg ArtBas.MatKod ArtBas.Klack ArtBas.BongTekst  ArtBas.Bonus_Givende ArtBas.PubliserINettbutikk ArtBas.ManRabIKas
     ArtBas.Anv-Id  ArtBas.Last-Id ArtBas.Slit-Id ArtBas.Ov-Id ArtBas.Mengde ArtBas.JamforEnhet ArtBas.Kjokkenskriver ArtBas.SalgsStopp
     ArtBas.Inner-Id ArtBas.BehKode ArtBas.Etikett ArtBas.SalgsEnhet ArtBas.Alder ArtBas.LinkVareNr ArtBas.LinkVareAnt
     ArtBas.AntIPakn ArtBas.Lokasjon ArtBas.Etikettekst1 ArtBas.Etikettekst2 ArtBas.RAvdNr ArtBas.HovedKatNr ArtBas.Link_Til_Nettside
     ArtBas.DivInfo[1] = INPUT FI-1
     ArtBas.DivInfo[2] = INPUT FI-2 WHEN cVisMaterial = "1"
     ArtBas.DivInfo[3] = INPUT FI-3 WHEN cVisKlack = "1"
     ArtBas.DivInfo[4] = INPUT FI-4 WHEN cVisInnersula = "1"
     ArtBas.DivInfo[5] = INPUT FI-5 WHEN cVisInnerfor  = "1"
     ArtBas.DivInfo[6] = INPUT FI-6 WHEN cVisSlit      = "1"
     ArtBas.DivInfo[7] = INPUT FI-7 WHEN cVisLast      = "1"
     ArtBas.DivInfo[8] = INPUT FI-8 WHEN cVisBruks     = "1"
     ArtBas.VisDivInfo[1] = INPUT T-1
     ArtBas.VisDivInfo[2] = INPUT T-2 WHEN cVisMaterial = "1"
     ArtBas.VisDivInfo[3] = INPUT T-3 WHEN cVisKlack = "1"
     ArtBas.VisDivInfo[4] = INPUT T-4 WHEN cVisInnersula = "1"
     ArtBas.VisDivInfo[5] = INPUT T-5 WHEN cVisInnerfor  = "1"
     ArtBas.VisDivInfo[6] = INPUT T-6 WHEN cVisSlit      = "1"
     ArtBas.VisDivInfo[7] = INPUT T-7 WHEN cVisLast      = "1"
     ArtBas.VisDivInfo[8] = INPUT T-8 WHEN cVisBruks     = "1"
     ArtBas.VisDivInfo[9] = INPUT T-9
     ArtBas.VisDivInfo[10] = INPUT T-10
     ArtBas.InkrAnbrekk ArtBas.Anbrekk ArtBas.Depositum ArtBas.ArtSlag.
    END.
    ASSIGN ArtBas.ArtSlag =  settArtSlag(ArtBas.ArtSlag). /*settArtSlag(iArtSlag).*/
    DO WITH FRAME FRAME-Info2:
     ASSIGN
     ArtBas.WebButikkArtikkel ArtBas.LinjeMerknad ArtBas.LevDato1 ArtBas.LevDato2 ArtBas.LevDato3 ArtBas.LevDato4 ArtBas.AnbefaltPris ArtBas.KjedeInnkPris ArtBas.VareFakta ArtBas.forhRab%
     ArtBas.supRab% ArtBas.KjedeRab% ArtBas.KjedeInnkPris ArtBas.KatalogPris ArtBas.VPIBildeKode ArtBas.VPIDato ArtBas.KjedeRab% ArtBas.KjedeValutaPris ArtBas.KjedeProdusent
     ArtBas.LevDatoStopp1 ArtBas.LevDatoStopp2 ArtBas.LevDatoStopp3 ArtBas.LevDatoStopp4 ArtBas.TilgjengeligFraLev ArtBas.PostBredde ArtBas.PostLengde ArtBas.PostHoyde ArtBas.PostVekt ArtBas.AlfaKode2
     ArtBas.Sortimentkoder ArtBas.Lagerkoder ArtBas.Kampanjeuker ArtBas.Kampanjestotte ArtBas.EkstStrTypeNavn.
    END.
    IF ArtBas.Opris = TRUE AND NOT CAN-FIND(FIRST ArtPris OF ArtBas) THEN
      RUN vgartopris.p (ArtBas.ArtikkelNr,?).
    IF S-AlternativLev:LIST-ITEM-PAIRS <> "" THEN
      RUN lagreAltLevBas.
    IF S-Underkategori:LIST-ITEM-PAIRS <> "," OR CAN-FIND(FIRST artbasunderkategori OF artbas) THEN
      RUN lagreUnderkategori.
    IF iMellankategori = 1 THEN
        RUN lagreMellankategori.
    IF S-Karakteristikk:LIST-ITEM-PAIRS <> "" THEN
      RUN lagreKarakteristikk.
    IF lSlettInterneStrek = TRUE OR lSlettStrekMstrl = TRUE THEN DO:
      IF (cGenEan = "yes" OR cGenEan000 = "yes") THEN
        RUN genStrekKode.p (ArtBas.ArtikkelNr,IF (cGenEan = "yes") THEN 1 ELSE 0,"").
    END.
    RUN kalkyle_mva_korr.p (ArtBas.ArtikkelNr, INPUT-OUTPUT iAntArtPris, INPUT-OUTPUT iAntPrisKo, INPUT-OUTPUT iAntBestHode, INPUT-OUTPUT iAntVpiPris).               
    IF CAN-FIND(FIRST BestHode WHERE BestHode.ArtikkelNr = ArtBAs.ArtikkelNr AND
        BestHode.LEvKod <>    ArtBas.LevKod) THEN
    DO:
    FIND FIRST BestHode EXCLUSIVE-LOCK WHERE
      BestHode.ArtikkelNr =  ArtBas.ArtikkelNr AND
      BestHode.LevKod     <> ArtBas.LevKod NO-ERROR NO-WAIT.
    IF LOCKED BestHode THEN
    DO:
      MESSAGE Tx("Bestilling oppdateres fra en annen terminal." + CHR(13) + "Lagring avbrytes.",123) VIEW-AS ALERT-BOX MESSAGE TITLE Tx("Melding",107).
      UNDO LAGRE_ARTBAS.
    END.
    ELSE ASSIGN BestHOde.LEvKod = ArtBas.LevKod.
    DO WHILE AVAILABLE BestHode:
      FIND NEXT BestHode EXCLUSIVE-LOCK WHERE BestHode.ArtikkelNr =  ArtBas.ArtikkelNr AND
         BestHode.LevKod     <> ArtBas.LevKod NO-ERROR NO-WAIT.
        IF LOCKED BestHode THEN
        DO:
          MESSAGE Tx("Bestilling oppdateres fra en annen terminal." + chr(13) + "Lagring avbrytes.",123)
           VIEW-AS ALERT-BOX MESSAGE TITLE Tx("Melding",107).
          UNDO LAGRE_ARTBAS.
        END.
        IF AVAILABLE BestHode THEN ASSIGN BestHode.LevKod = ArtBas.LevKod.
      END.
    END.
    PUBLISH "ByttObjekt" (ArtBas.ArtikkelNr).
    IF cTmpModus NE "NY" AND VALID-HANDLE(hParentHandle) AND CAN-DO(hParentHandle:INTERNAL-ENTRIES,"ArtBasEndret") AND AVAIL ArtBas THEN 
      RUN "ArtBasEndret" IN hParentHandle (ArtBas.ArtikkelNr).
    ELSE IF cTmpModus = "NY" AND VALID-HANDLE(hParentHandle) AND CAN-DO(hParentHandle:INTERNAL-ENTRIES,"FreezeWin") AND NOT wKopi THEN 
      DYNAMIC-FUNCTION("FreezeWin" IN hParentHandle,YES).
    ELSE IF cTmpModus = "NY" AND VALID-HANDLE(hParentHandle) AND CAN-DO(hParentHandle:INTERNAL-ENTRIES,"NyArtBas") THEN
      RUN "NyArtBas" IN hParentHandle (ArtBas.ArtikkelNr).
    ASSIGN wKopi = FALSE.
    IF AVAILABLE ArtBas THEN
      FIND CURRENT ArtBas NO-LOCK NO-ERROR.
    IF cTmpModus = "NY" AND (cGenEan = "yes" OR cGenEan000 = "yes") THEN
      RUN genStrekKode.p (ArtBas.ArtikkelNr,IF (cGenEan = "yes") THEN 1 ELSE 0,"").
    IF bHK AND bKjedevare THEN DO:
      cEDBSystem = 'POS' + STRING(TIME). 
      RUN art_to_vpi.p ('ARTNR|' + cEDBSystem + '|' + STRING(ArtBas.ArtikkelNr) + ',',?,'',OUTPUT cTekst,OUTPUT bOk).
      RUN vpieksport.w (cEDBSystem, '',1,OUTPUT iAntSlett,OUTPUT iAntNyEndre).     
    END.
END.
RUN VisArtBas. 
RUN VisEndretinfo.
RUN Toolbar.
IF cORG <> cENDRET THEN RUN oppdModell.
IF AVAILABLE ArtBas THEN PUBLISH "ArtBasEndret" (STRING(ROWID(ArtBas))).
RETURN "OK".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lagreKarakteristikk C-ArtKort 
PROCEDURE lagreKarakteristikk :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
  
  DEF VAR pcTekst AS CHAR NO-UNDO.
  DEF VAR piLoop  AS INT  NO-UNDO.

  IF NOT AVAILABLE ArtBas THEN
      RETURN.

  DEF BUFFER bKarakteristikk FOR Karakteristikk.
  DO WITH FRAME FRAME-Info2:
      pcTekst = "".
      FOR EACH ArtBasKarakteristikk NO-LOCK WHERE 
          ArtBasKarakteristikk.ArtikkelNr = ArtBas.ArtikkelNr:

          FIND bKarakteristikk OF ArtBasKarakteristikk NO-LOCK WHERE
              bKarakteristikk.KarakteristikkId = ArtBasKarakteristikk.KarakteristikkId NO-ERROR.
          IF AVAILABLE bKarakteristikk THEN
              pcTekst = pcTekst + 
                        (IF pcTekst = ""
                           THEN ""
                           ELSE ",") + 
                        STRING(bKarakteristikk.KarakteristikkId) + " / " + bKarakteristikk.KBeskrivelse + "," + STRING(bKarakteristikk.KarakteristikkId).
      END.

      IF pcTekst <> S-Karakteristikk:LIST-ITEM-PAIRS THEN
      DO TRANSACTION:
          /* Renser bort gammal dr... */
          FOR EACH ArtBasKarakteristikk WHERE 
              ArtBasKarakteristikk.ArtikkelNr = ArtBas.ArtikkelNr:
              DELETE ArtBasKarakteristikk.
          END.
          /* Legger opp nytt. */
          DO piLoop = 1 TO (NUM-ENTRIES(S-Karakteristikk:LIST-ITEM-PAIRS) / 2):
              CREATE ArtBasKarakteristikk.
              ASSIGN
                  ArtBasKarakteristikk.ArtikkelNr = ArtBas.ArtikkelNr
                  ArtBasKarakteristikk.KarakteristikkId = (ENTRY(piLoop * 2,S-Karakteristikk:LIST-ITEM-PAIRS))
                  .
          END.
      END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lagreMellankategori C-ArtKort 
PROCEDURE lagreMellankategori :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iMellFinns AS INTEGER     NO-UNDO.
    DEFINE VARIABLE lSkapaElogg AS LOGICAL     NO-UNDO.
    IF NOT AVAIL artbas THEN
        RETURN.
    DO WITH FRAME FRAME-ArtInfo:

        IF INPUT CB-Mellankategori > 0 AND INPUT CB-MUkat > 0 THEN DO:
            ASSIGN INPUT CB-Mellankategori
                   INPUT CB-MUkat.
            FIND MellanUkat WHERE MellanUkat.mkatid = CB-Mellankategori AND MellanUkat.UnderKatNr = CB-MUkat NO-LOCK.
            FIND FIRST ArtBasMUkategori OF artbas NO-LOCK NO-ERROR.
            IF AVAIL ArtBasMUkategori AND ArtBasMUkategori.mukatnr = MellanUkat.mukatnr THEN DO:
                .
            END.
            ELSE IF AVAIL ArtBasMUkategori THEN DO:
                FIND CURRENT ArtBasMUkategori.
                ArtBasMUkategori.mukatnr = MellanUkat.mukatnr.
                FIND CURRENT ArtBasMUkategori NO-LOCK.
                lSkapaElogg = TRUE.
            END.
            ELSE DO:
                CREATE ArtBasMUkategori.
                ASSIGN ArtBasMUkategori.ArtikkelNr = artbas.artikkelnr
                       ArtBasMUkategori.mukatnr    = MellanUkat.mukatnr.
                FIND CURRENT ArtBasMUkategori NO-LOCK.
                lSkapaElogg = TRUE.
            END.
        END.
        ELSE DO:
            FIND FIRST ArtBasMUkategori OF artbas NO-ERROR.
            IF AVAIL ArtBasMUkategori THEN DO:
                DELETE ArtBasMUkategori.
                lSkapaElogg = TRUE.
            END.
        END.
        IF lSkapaElogg THEN
            RUN skapaELogg IN THIS-PROCEDURE.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lagreUnderkategori C-ArtKort 
PROCEDURE lagreUnderkategori :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DEF VAR pcTekst AS CHAR NO-UNDO.
  DEF VAR piLoop  AS INT  NO-UNDO.
  DEFINE VARIABLE iUnderKatNr AS INTEGER     NO-UNDO.
  IF NOT AVAILABLE ArtBas THEN
      RETURN.
  IF iMellankategori = 1 THEN
      RETURN.
  DEF BUFFER bUnderkategori FOR Underkategori.
  DO WITH FRAME FRAME-ArtInfo:
      pcTekst = "".
      FOR EACH ArtBasUnderkategori NO-LOCK WHERE 
          ArtBasUnderKategori.ArtikkelNr = ArtBas.ArtikkelNr:

          FIND bUnderkategori OF ArtBasUnderkategori NO-LOCK WHERE
              bUnderkategori.UnderKatNr = ArtBasUnderkategori.UnderKatNr NO-ERROR.
          IF AVAILABLE bUnderkategori THEN
              pcTekst = pcTekst + 
                        (IF pcTekst = ""
                           THEN ""
                           ELSE ",") + 
                        STRING(bUnderkategori.UnderKatNr) + " / " + bUnderkategori.UnderKatTekst + "," + STRING(bUnderkategori.UnderKatNr).
      END.

      IF pcTekst <> S-Underkategori:LIST-ITEM-PAIRS THEN
      DO TRANSACTION:
          /* Renser bort gammal dr... */
          FOR EACH ArtBasUnderkategori WHERE 
              ArtBasUnderkategori.ArtikkelNr = ArtBas.ArtikkelNr:
              DELETE ArtBasUnderkategori.
          END.
          /* Legger opp nytt. */
          DO piLoop = 1 TO (NUM-ENTRIES(S-Underkategori:LIST-ITEM-PAIRS) / 2):
              iUnderKatNr = int(ENTRY(piLoop * 2,S-Underkategori:LIST-ITEM-PAIRS)).
              IF iUnderKatNr > 0 THEN DO:
                  CREATE ArtBasUnderkategori.
                  ASSIGN
                      ArtBasUnderkategori.ArtikkelNr = ArtBas.ArtikkelNr
                      ArtBasUnderkategori.UnderKatNr = iUnderKatNr
                      .
              END.
          END.
      END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ModellFarge C-ArtKort 
PROCEDURE ModellFarge :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lOppdater AS LOGICAL NO-UNDO.
      THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
      RUN w-ModellFarge.w (IF ArtBas.ModellFarge = 0 THEN ArtBas.ArtikkelNr ELSE ArtBas.ModellFarge,
                   cGenEan,wForslagLopNr,OUTPUT lOppdater).
/*       RUN wSynkModell.w PERSISTENT SET hModell (IF ArtBas.ModellFarge = 0 THEN ArtBas.ArtikkelNr ELSE ArtBas.ModellFarge, */
/*                    cGenEan,wForslagLopNr,OUTPUT lOppdater,INPUT THIS-PROCEDURE, {&WINDOW-NAME}).                          */
      THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.
      IF lOppdater THEN DO:
          FIND CURRENT ArtBas NO-LOCK.
          RUN FillCBModellFarge.
          DO WITH FRAME DEFAULT-FRAME:
               ASSIGN CB-ModellFarge:SCREEN-VALUE = STRING(ArtBas.ArtikkelNr) NO-ERROR.
          END.
      END.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NyArtBas C-ArtKort 
PROCEDURE NyArtBas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER ipArtikkelNr AS DEC  NO-UNDO.
  lArtikkelNr = ipArtikkelNr.
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
  IF ArtBas.Utgatt = TRUE THEN   
    DO:
      MESSAGE Tx("Artikkelen er utgått. Den må settes som ikke utgått før bestilling kan registreres.",124)
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
      RECID(BestHode) = wBestHodeRecid NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NyKalkyleLagret C-ArtKort 
PROCEDURE NyKalkyleLagret :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FRAME DEFAULT-FRAME:SENSITIVE = TRUE.
ASSIGN chTabStrip:ENABLED = TRUE.
PROCESS EVENTS.
IF VALID-HANDLE(hParentHandle) AND CAN-DO(hParentHandle:INTERNAL-ENTRIES,"NyArtBas") AND AVAIL ArtBas THEN 
 RUN "NyArtBas" IN hParentHandle (ArtBas.ArtikkelNr) NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NyttBilde C-ArtKort 
PROCEDURE NyttBilde :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER cFilNavn AS CHARACTER  NO-UNDO.
  DEF VAR cClipTekst     AS CHAR NO-UNDO.
  DEF VAR wReturnValue AS CHAR NO-UNDO.
  DEF BUFFER bufArtBas FOR ArtBas.
  DEFINE VARIABLE iHeight AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iWidth AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iXRes AS INTEGER     NO-UNDO.
  DEFINE VARIABLE decKoeff AS DECIMAL     NO-UNDO.
  IF NOT VALID-HANDLE(wLibHandle) THEN DO:
      MESSAGE "Prosedyrebiblioteket er ikke startet!" VIEW-AS ALERT-BOX TITLE "Melding".
      RETURN NO-APPLY.
  END.
  /* Nullstiller error flagg for ocx. */
  chIMAGE-Sko:Picbuf:errornumber = 0.

  IF cFilNavn = "" OR cFilNavn = ? THEN DO:
      IF CLIPBOARD:NUM-FORMATS > 0 THEN DO:
          IF wModus <> "NY" THEN
              MESSAGE SKIP(1)
                      "       ClipBoard er tomt!" SKIP
                      "              eller" SKIP
                      "det inneholder ugyldige data" SKIP(1)
                      VIEW-AS ALERT-BOX TITLE "Melding".
          RETURN NO-APPLY.
      END.
      ASSIGN cClipTekst                = CLIPBOARD:VALUE NO-ERROR.
      IF cClipTekst <> ? THEN DO:
          IF wModus <> "NY" THEN
               MESSAGE "Clipboard inneholder ugyldige data (1)."
                 VIEW-AS ALERT-BOX INFO BUTTONS OK.
           RETURN "AVBRYT".
      END.
      /* Legger bilde inn i buffer fra ClipBoard. */
      chIMAGE-Sko:Picbuf:PasteClipboard NO-ERROR.

      IF chIMAGE-Sko:Picbuf:errornumber <> 0 THEN DO:
          IF wModus <> "NY" THEN
              MESSAGE "Cliboard inneholder ugyldige data (2)."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN "AVBRYT".
      END.
  END.
  ELSE IF cFilNavn <> "" THEN DO:
  /* Legger bilde inn i buffer fra ClipBoard. */
      chIMAGE-Sko:PicBuf:CLEAR(2).
      ASSIGN chIMAGE-Sko:PicBuf:FILENAME = cFilNavn.
      chIMAGE-Sko:PicBuf:LOAD() NO-ERROR.
      IF chIMAGE-Sko:Picbuf:errornumber <> 0 THEN DO:
          IF wModus <> "NY" THEN
              MESSAGE "Feil på bildefil (2)."
                  SKIP chIMAGE-Sko:Picbuf:errornumber
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN "AVBRYT".
      END.
  END.

  FIND Bilderegister WHERE Bilderegister.BildNr = INTEGER(ArtBas.Artikkelnr) NO-ERROR.
  IF AVAIL Bilderegister THEN DO:
      /* Slett Bilderegister + filer på disk */
      IF SEARCH(cBildeKatalog + Bilderegister.FilNavn) <> ? THEN
          OS-DELETE VALUE(cBildeKatalog + Bilderegister.FilNavn).
      IF SEARCH(cBildeKatalog + "mini" + Bilderegister.FilNavn) <> ? THEN
          OS-DELETE VALUE(cBildeKatalog + "mini" + Bilderegister.FilNavn).
      FOR EACH BildeData OF BildeRegister:
          DELETE BildeData.
      END.
      DELETE BildeRegister.
  END.
  BILDENUMMER:
  DO:
      ASSIGN wBildnr = INTEGER(ArtBas.ArtikkelNr).
      CREATE BildeRegister.
      ASSIGN BildeRegister.BildNr = wBildNr
             BildeRegister.FilNavn = STRING(ArtBas.ArtikkelNr) + ".jpg".
      FIND CURRENT BildeRegister NO-LOCK NO-ERROR.
  
  END. /* BILDENUMMER */
 
  /* Tildeler filnavn */
  chIMAGE-Sko:Picbuf:FileName = cBildeKatalog + BildeRegister.FilNavn.
  
  /* Lagrer bilde på hd. ------------------------------------------------ */

  IF chIMAGE-Sko:Picbuf:WriteCompression = 0 THEN  /* Filen skal komprimeres.    */
     chIMAGE-Sko:Picbuf:WriteCompression = 65.
  chIMAGE-Sko:Picbuf:Store.                 /* Lagre filen til HD.        */
  IF chIMAGE-Sko:Picbuf:WriteCompression <> 0 THEN /* Filen skal komprimeres.    */
     chIMAGE-Sko:Picbuf:WriteCompression = 0.
/*   RUN w-forminskStor.p (Bilderegister.Bildnr).                                */
/*   chIMAGE-Sko:PicBuf:CLEAR(2).                                                */
/*   ASSIGN chIMAGE-Sko:PicBuf:FILENAME = cBildeKatalog + BildeRegister.FilNavn. */
/*   chIMAGE-Sko:PicBuf:LOAD() NO-ERROR.                                         */
/*   /* nytt */                                                                                    */
/*   /* nytt slut */                                                                               */
  /* Leser inn filen. */
  ASSIGN wReturnValue = "AVBRYT".
  IF SEARCH(chIMAGE-Sko:Picbuf:FileName) <> ? THEN DO:
      IF VALID-HANDLE(wLibHandle) THEN
        RUN LesInnBilde IN wLibHandle (BildeRegister.BildNr, chIMAGE-Sko:Picbuf:FileName, OUTPUT wReturnValue).
  END.
  IF wReturnValue = "AVBRYT" THEN DO:
      IF wModus <> "NY" THEN
          MESSAGE "Feil ved lasting av bilde " BildeRegister.BildNr
            VIEW-AS ALERT-BOX ERROR TITLE "Feil".
  END.
  ELSE DO:
      FIND bufArtBas EXCLUSIVE-LOCK WHERE
        RECID(bufArtBAs) = recid(ArtBas) NO-ERROR.
      IF AVAILABLE bufArtBas THEN DO:
          ASSIGN bufArtBas.BildNr = wBildNr.
          RELEASE bufArtBas.
      END.
      RUN w-Forminsk.w (Bilderegister.Bildnr).
  END.

  PUBLISH "NyttBildeLastet" (artbas.artikkelnr).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NyttBildeORG C-ArtKort 
PROCEDURE NyttBildeORG :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER cFilNavn AS CHARACTER  NO-UNDO.
  DEF VAR cClipTekst     AS CHAR NO-UNDO.
  DEF VAR wReturnValue AS CHAR NO-UNDO.
  DEF BUFFER bufArtBas FOR ArtBas.
  DEFINE VARIABLE iHeight AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iWidth AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iXRes AS INTEGER     NO-UNDO.
  DEFINE VARIABLE decKoeff AS DECIMAL     NO-UNDO.
  IF NOT VALID-HANDLE(wLibHandle) THEN DO:
      MESSAGE "Prosedyrebiblioteket er ikke startet!" VIEW-AS ALERT-BOX TITLE "Melding".
      RETURN NO-APPLY.
  END.
  /* Nullstiller error flagg for ocx. */
  chIMAGE-Sko:Picbuf:errornumber = 0.

  IF cFilNavn = "" OR cFilNavn = ? THEN DO:
      IF CLIPBOARD:NUM-FORMATS > 0 THEN DO:
          IF wModus <> "NY" THEN
              MESSAGE SKIP(1)
                      "       ClipBoard er tomt!" SKIP
                      "              eller" SKIP
                      "det inneholder ugyldige data" SKIP(1)
                      VIEW-AS ALERT-BOX TITLE "Melding".
          RETURN NO-APPLY.
      END.
      ASSIGN cClipTekst                = CLIPBOARD:VALUE NO-ERROR.
      IF cClipTekst <> ? THEN DO:
          IF wModus <> "NY" THEN
               MESSAGE "Clipboard inneholder ugyldige data (1)."
                 VIEW-AS ALERT-BOX INFO BUTTONS OK.
           RETURN "AVBRYT".
      END.
      /* Legger bilde inn i buffer fra ClipBoard. */
      chIMAGE-Sko:Picbuf:PasteClipboard NO-ERROR.

      IF chIMAGE-Sko:Picbuf:errornumber <> 0 THEN DO:
          IF wModus <> "NY" THEN
              MESSAGE "Cliboard inneholder ugyldige data (2)."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN "AVBRYT".
      END.
  END.
  ELSE IF cFilNavn <> "" THEN DO:
  /* Legger bilde inn i buffer fra ClipBoard. */
      chIMAGE-Sko:PicBuf:CLEAR(2).
      ASSIGN chIMAGE-Sko:PicBuf:FILENAME = cFilNavn.
      chIMAGE-Sko:PicBuf:LOAD() NO-ERROR.
      IF chIMAGE-Sko:Picbuf:errornumber <> 0 THEN DO:
          IF wModus <> "NY" THEN
              MESSAGE "Feil på bildefil (2)."
                  SKIP chIMAGE-Sko:Picbuf:errornumber
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN "AVBRYT".
      END.
  END.

  FIND Bilderegister WHERE Bilderegister.BildNr = INTEGER(ArtBas.Artikkelnr) NO-ERROR.
  IF AVAIL Bilderegister THEN DO:
      /* Slett Bilderegister + filer på disk */
      IF SEARCH(cBildeKatalog + Bilderegister.FilNavn) <> ? THEN
          OS-DELETE VALUE(cBildeKatalog + Bilderegister.FilNavn).
      IF SEARCH(cBildeKatalog + "mini" + Bilderegister.FilNavn) <> ? THEN
          OS-DELETE VALUE(cBildeKatalog + "mini" + Bilderegister.FilNavn).
      FOR EACH BildeData OF BildeRegister:
          DELETE BildeData.
      END.
      DELETE BildeRegister.
  END.
  BILDENUMMER:
  DO:
      ASSIGN wBildnr = INTEGER(ArtBas.ArtikkelNr).
      CREATE BildeRegister.
      ASSIGN BildeRegister.BildNr = wBildNr
             BildeRegister.FilNavn = STRING(ArtBas.ArtikkelNr) + ".jpg".
      FIND CURRENT BildeRegister NO-LOCK NO-ERROR.
  
  END. /* BILDENUMMER */
 
  /* Tildeler filnavn */
  chIMAGE-Sko:Picbuf:FileName = cBildeKatalog + BildeRegister.FilNavn.
  
  /* Lagrer bilde på hd. ------------------------------------------------ */

  IF chIMAGE-Sko:Picbuf:WriteCompression = 0 THEN  /* Filen skal komprimeres.    */
     chIMAGE-Sko:Picbuf:WriteCompression = 65.
  chIMAGE-Sko:Picbuf:Store.                 /* Lagre filen til HD.        */
  IF chIMAGE-Sko:Picbuf:WriteCompression <> 0 THEN /* Filen skal komprimeres.    */
     chIMAGE-Sko:Picbuf:WriteCompression = 0.
    
/*   /* nytt */                                                                                    */
/*   /* nytt slut */                                                                               */
  /* Leser inn filen. */
  ASSIGN wReturnValue = "AVBRYT".
  IF SEARCH(chIMAGE-Sko:Picbuf:FileName) <> ? THEN DO:
      IF VALID-HANDLE(wLibHandle) THEN
        RUN LesInnBilde IN wLibHandle (BildeRegister.BildNr, chIMAGE-Sko:Picbuf:FileName, OUTPUT wReturnValue).
  END.
  IF wReturnValue = "AVBRYT" THEN DO:
      IF wModus <> "NY" THEN
          MESSAGE "Feil ved lasting av bilde " BildeRegister.BildNr
            VIEW-AS ALERT-BOX ERROR TITLE "Feil".
  END.
  ELSE DO:
      FIND bufArtBas EXCLUSIVE-LOCK WHERE
        RECID(bufArtBAs) = recid(ArtBas) NO-ERROR.
      IF AVAILABLE bufArtBas THEN DO:
          ASSIGN bufArtBas.BildNr = wBildNr.
          RELEASE bufArtBas.
      END.
      RUN w-Forminsk.w (Bilderegister.Bildnr).
  END.

  PUBLISH "NyttBildeLastet" (artbas.artikkelnr).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenWeb C-ArtKort 
PROCEDURE OpenWeb :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER document AS CHAR.

  DEF VAR executable AS CHAR.
  DEF VAR hInstance AS INTEGER.
  
  /* find the associated executable in registry */  
  Executable = FILL("x", 255). /* =allocate memory */
  RUN FindExecutable{&A} IN hpApi (document,
                                   "",
                                   INPUT-OUTPUT Executable,
                                   OUTPUT hInstance).

  /* if not found, show the OpenAs dialog from the Explorer */
  IF hInstance>=0 AND hInstance<=32 AND hInstance <> 2 THEN 
     RUN ShellExecute{&A} IN hpApi(0,
                                  "open",
                                  "rundll32.exe",
                                  "shell32.dll,OpenAs_RunDLL " + document,
                                  "",
                                  1,
                                  OUTPUT hInstance).

  /* now open the document. If the user canceled the OpenAs dialog,
     this ShellExecute call will silently fail */
  RUN ShellExecute{&A} IN hpApi  (0,
                                  "open",
                                  document,
                                  "",
                                  "",
                                  1,
                                  OUTPUT hInstance).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdModell C-ArtKort 
PROCEDURE oppdModell :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  IF NOT AVAILABLE ArtBas THEN RETURN.
  IF ArtBas.ModellFarge = 0 THEN RETURN.
  IF cOrg = '' OR cENDRET = '' THEN RETURN.
  
  RUN d-KoperModell.w (cOrg,cEndret).

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReadFromLokalIni C-ArtKort 
PROCEDURE ReadFromLokalIni :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT  PARAMETER wKey     AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER wValue   AS CHAR NO-UNDO.

DEF VAR wSection AS CHAR NO-UNDO.
DEF VAR wMappe   AS CHAR NO-UNDO.
DEF VAR wIniFil  AS CHAR NO-UNDO.

ASSIGN
    wSection = "FILTERPARAMETRE".

/* Henter mappe og filnavn på den lokale inifilen */
IF VALID-HANDLE(wLibHandle) THEN
DO:
  RUN Mappe  IN wLibHandle (OUTPUT wMappe).
  RUN IniFil IN wLibHandle (OUTPUT wIniFil).
END.
ELSE 
    RETURN "AVBRYT".

/* Lagrer parametre */
IF wMappe <> ? THEN 
  DO:
    /* Laster den lokale ini filen. */
    LOAD wIniFil DIR wMappe BASE-KEY "INI" NO-ERROR.
    /* Finnes den ikke, opprette den. */
    IF ERROR-STATUS:ERROR THEN DO:
       LOAD wIniFil DIR wMappe NEW BASE-KEY "INI" NO-ERROR.
       IF ERROR-STATUS:ERROR THEN RETURN "AVBRYT".
    END.           
    /* Stiller om til lokal ini-fil. */
    USE wIniFil NO-ERROR.
    /* Lagrer parameterverdien */
    IF NOT ERROR-STATUS:ERROR THEN
       GET-KEY-VALUE SECTION wSection KEY wKey VALUE wValue.
    /* Stiller tilbake til oppstarts ini fil. */
    UNLOAD wIniFil NO-ERROR.
  END.

/* Stripper ukjent verdi. */
ASSIGN
    wValue = IF wValue = ? THEN "" ELSE wValue.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Regnskapsavdeling C-ArtKort 
PROCEDURE Regnskapsavdeling :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER iRAvdNr AS INT NO-UNDO.

  IF NOT CAN-FIND(Regnskapsavdeling WHERE 
                  Regnskapsavdeling.RAvdNr = iRAvdNr) THEN
  DO TRANSACTION:
      CREATE Regnskapsavdeling.
      ASSIGN
          Regnskapsavdeling.RAvdNr = iRAvdNr
          Regnskapsavdeling.RAvdBeskrivelse = "Automatisk opprettet"
          .
      RELEASE Regnskapsavdeling.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReklamerRestlager C-ArtKort 
PROCEDURE ReklamerRestlager :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR pbSvar AS LOG NO-UNDO.

  ASSIGN
      pbSvar = FALSE
      .
  MESSAGE "Skal artikkelens restlager reklameres?"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
      UPDATE pbSvar.

  IF pbSvar <> TRUE THEN
      RETURN.

  RUN makulerrestlager.p (wArtBasRecid).
  DO WITH FRAME FRAME-Lager:
      APPLY "CHOOSE" TO B-OppdLager.
  END.
  MESSAGE "Hvis lagertransaksjonene ikke er ferdibehandlet, må lagerbilde " + 
          "oppdateres manuelt for å vise korrekt lagerstatus." 
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
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
  DEF VAR cpTekst AS CHAR NO-UNDO.

  ASSIGN
/*     T-LapTop = FALSE */
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
    S-AlternativLev:LIST-ITEM-PAIRS IN FRAME FRAME-ArtInfo = ","
    S-AlternativLev = ""
    S-Underkategori:LIST-ITEM-PAIRS IN FRAME FRAME-ArtInfo = ","
    S-Underkategori = ""
    .
  DISPLAY 
    "" @ ArtBas.ArtikkelNr
    "" @ ArtBas.VgKat
    "" @ ArtBas.Vg 
    "" @ ArtBas.LopNr 
    "" @ ArtBas.Beskr 
    "" @ ArtBas.LevNr 
    "" @ ArtBas.LevKod
    "" @ FILL-IN-VgBeskr
    "" @ FILL-IN-LevNamn
    "" @ huvgr.hg
    "" @ HuvGr.HgBeskr
    "" @ FI-PaTilbud
    "" @ FI-LevFargKod
    "" @ ArtBas.Sanertdato     "" @ ArtBas.Utgattdato
    TOGGLE-Annonse
/*     T-LapTop */
  WITH FRAME DEFAULT-FRAME.
  ASSIGN
      ArtBas.Kunderabatt:SCREEN-VALUE IN FRAME FRAME-ArtInfo = "yes"
      ArtBas.ManRabIKas:SCREEN-VALUE IN FRAME FRAME-ArtInfo  = "yes"
      ArtBas.Mengderabatt:SCREEN-VALUE IN FRAME FRAME-ArtInfo  = "yes"
      ArtBas.BestForsl:SCREEN-VALUE IN FRAME Default-frame = "no"
      ArtBas.Pant:SCREEN-VALUE  IN FRAME Default-frame = "no"
      ArtBas.ManueltOpprettet:SCREEN-VALUE  IN FRAME Default-frame  = "no"
      ArtBas.GarantiKl:SCREEN-VALUE IN FRAME FRAME-ArtInfo = "0" 
      ArtBas.OnLineLevNr:SCREEN-VALUE IN FRAME FRAME-ArtInfo = "0" 
      ArtBas.VareType:SCREEN-VALUE IN FRAME FRAME-ArtInfo  = '1' 
      FI-JamforPris:screen-value IN FRAME Frame-ArtInfo  = ''
      ArtBas.JamforEnhet:screen-value IN FRAME Frame-ArtInfo = ''
      ArtBas.LinkVareNr:SCREEN-VALUE IN FRAME Frame-ArtInfo = '0'
      ArtBas.LinkVareAnt:SCREEN-VALUE IN FRAME Frame-ArtInfo = '1'
      FI-LinkVareTekst:SCREEN-VALUE IN FRAME Frame-ArtInfo  = ''
      ArtBas.ArtSlag:SCREEN-VALUE IN FRAME Default-frame = "0" 
      ArtBas.Link_til_Nettside:SCREEN-VALUE IN FRAME Frame-ArtInfo = ''
      .

  /*
  if wAktivFlip = 1 then
    output to value("nul").
  */
  DISPLAY
    
    "" @ ArtBas.Inn_Dato
    "" @ ArtBas.LevFargKod
    "" @ Varemerke.Beskrivelse
    "" @ Produsent.Beskrivelse
    "" @ StrType.Beskrivelse
    "" @ ArtBas.VMId
    "" @ ArtBas.ProdNr "" ArtBas.HovedKatNr
    "" @ ArtBas.StrTypeId
    "" @ ArtBas.Sasong
    "" @ Sasong.SasBeskr 
    "" @ ArtBas.Farg
    "" @ Farg.FarBeskr
    "" WHEN cVisMaterial  = "1" @ ArtBas.MatKod
    "" WHEN cVisMaterial  = "1" @ Material.MatBeskr
    "" @ ArtBas.AntIPakn
    "" @ ArtBas.Lokasjon
    "" @ ArtBas.Etikettekst1
    "" @ ArtBas.Etikettekst2
    "" WHEN cVisKlack = "1" @ ArtBas.Klack
    "" @ FILL-IN-SalgsPris
    "" @ FILL-IN-InnPris
    "" @ FILL-IN-TilbPris  
    "" @ FILL-IN-UtsFra
    "" @ FILL-IN-UtsTil
    "" @ ArtBas.BongTekst
    "" WHEN cVisBruks     = "1" @ ArtBas.Anv-Id   
    "" WHEN cVisBruks     = "1" @ Anv-Kod.AnvBeskr        
    "" WHEN cVisLast      = "1" @ ArtBas.Last-Id  
    "" WHEN cVisLast      = "1" @ Last-Sko.LastBeskr
    "" WHEN cVisSlit      = "1" @ ArtBas.Slit-Id  
    "" WHEN cVisSlit      = "1" @ SlitSula.SlitBeskr    
    "" WHEN cVisInnerfor  = "1" @ ArtBas.Ov-Id    
    "" WHEN cVisInnerfor  = "1" @ Ovandel.OvBeskr      
    "" WHEN cVisInnersula = "1" @ ArtBas.Inner-Id 
    "" WHEN cVisInnersula = "1" @ InnerSula.InnerBeskr
    "" @ ArtBas.ValKod
    "" @ Valuta.ValKurs
    "" @ Regnskapsavdeling.RAvdBeskrivelse
    "" @ ArtBas.RAvdNr
    "" @ ArtBas.LeveringsTid 
    "" @ ArtBas.WebLeveringstid 
    "" @ ArtBas.WebMinLager
    "" @ ArtBas.Kampanjekode
    "" @ ArtBas.Depositum    
    "" @ ArtBas.Mengde 
    "" WHEN cVisKlack = "1" @ Klack.Beskrivning 
    "" WHEN cVisBehKod = "1" @ ArtBas.BehKode
    "" WHEN cVisBehKod = "1" @ Handtering.Beskrivelse
    FI-1    
    FI-2 WHEN cVisMaterial  = "1"    
    FI-3 WHEN cVisKlack     = "1"   
    FI-4 WHEN cVisInnersula = "1"   
    FI-5 WHEN cVisInnerfor  = "1"   
    FI-6 WHEN cVisSlit      = "1"       
    FI-7 WHEN cVisLast      = "1"       
    FI-8 WHEN cVisBruks     = "1"      
    T-1  
    T-2 WHEN cVisMaterial  = "1"      
    T-3 WHEN cVisKlack     = "1"
    T-4 WHEN cVisInnersula = "1" 
    T-5 WHEN cVisInnerfor  = "1"
    T-6 WHEN cVisSlit      = "1"
    T-7 WHEN cVisLast      = "1"
    T-8 WHEN cVisBruks     = "1"
    T-9  
    T-10 
    S-AlternativLev S-Underkategori WHEN S-Underkategori:HIDDEN = FALSE
    WITH FRAME FRAME-ArtInfo.
  DISPLAY 
    "" @ ArtBas.AnbefaltPris "" @ ArtBas.LinjeMerknad
    "" @ ArtBas.LevDato1 "" @ ArtBas.LevDato2
    "" @ ArtBas.LevDato3 "" @ ArtBas.LevDato4
      "" @ ArtBas.forhRab% "" @ ArtBas.supRab%
      "" @ ArtBas.KatalogPris "" @ ArtBas.VPIBildeKode
      "" @ ArtBas.VPIDato "" @ ArtBas.KjedeRab%    
      "" @ ArtBas.KjedeInnkPris "" @ ArtBas.KjedeValutaPris 
      "" @ ArtBas.KjedeProdusent "" @ ArtBas.LevDatoStopp1 
     "" @ ArtBas.LevDatoStopp2 "" @ ArtBas.LevDatoStopp3 
     "" @ ArtBas.LevDatoStopp4 "" @ ArtBas.TilgjengeligFraLev
     "" @ ArtBas.PostBredde "" @ ArtBas.PostLengde 
     "" @ ArtBas.PostHoyde "" @ ArtBas.PostVekt "" @ ArtBas.AlfaKode2 "" @ FI-Land
     "" @ ArtBas.Sortimentkoder "" @ ArtBas.Lagerkoder "" @ ArtBas.Kampanjeuker "" @ ArtBas.Kampanjestotte  "" @ ArtBas.EkstStrTypeNavn
  WITH FRAME FRAME-Info2.

  {syspar2.i 2 4 9 cpTekst}
  IF cpTekst = '' THEN cpTekst = '1'.
  ASSIGN
      ArtBas.VareFakta:SCREEN-VALUE IN FRAME FRAME-Info2  = ""
      ArtBas.Etikett:SCREEN-VALUE  IN FRAME FRAME-ArtInfo   = cpTekst
      ArtBas.SalgsEnhet:SCREEN-VALUE IN FRAME FRAME-ArtInfo = ENTRY(1,ArtBas.SalgsEnhet:LIST-ITEMS)
      HuvGr.HgBeskr:TOOLTIP  IN FRAME DEFAULT-FRAME        = ""
      ArtBas.Beskr:TOOLTIP   IN FRAME DEFAULT-FRAME          = ""
      FILL-IN-VgBeskr:TOOLTIP  IN FRAME DEFAULT-FRAME        = ""
      .
  
  ASSIGN
    ArtBas.VareType:SCREEN-VALUE IN FRAME FRAME-ArtInfo = '1'
    ArtBas.Leveringstid:SCREEN-VALUE IN FRAME FRAME-ArtInfo = '0'
    ArtBas.Leveringstid:SENSITIVE = FALSE.
  
  RUN FillLevSortCB.  
  IF AVAILABLE ArtBas THEN
    ArtBas.Notat:screen-value IN FRAME FRAME-ArtInfo = "".
  
  /* Viser blankt bilde */
  chIMAGE-Sko:Picbuf:clear(2).
/*   run VisBilde (1). */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SanerArtbas C-ArtKort 
PROCEDURE SanerArtbas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dSanera2Artikkelnr AS DECIMAL NO-UNDO.
  DEFINE VARIABLE cc AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE lOk AS LOGICAL    NO-UNDO.

  RUN d-bekreftSanera.w (Artbas.artikkelnr,
                         Artbas.Beskr,
                         Artbas.VG,
                         Artbas.LevNr,
                         FILL-In-LevNamn:SCREEN-VALUE IN FRAME DEFAULT-FRAME,
                         Artbas.Lager,
                         ArtBas.StrTypeID,
                         StrType.Beskrivelse:SCREEN-VALUE IN FRAME FRAME-ArtInfo,
                         FILL-IN-SalgsPris:SCREEN-VALUE,
                         FILL-IN-TilbPris:SCREEN-VALUE,
                         FILL-IN-UtsFra:SCREEN-VALUE,
                         FILL-IN-UtsTil:SCREEN-VALUE,
                         wCl,
                         INPUT-OUTPUT dSanera2Artikkelnr,OUTPUT lOk).
    IF dSanera2Artikkelnr > 0 THEN DO WITH FRAME DEFAULT-FRAME:
        MESSAGE "Skal aktuell artikkeldata flyttes til hentet artikkel ?" UPDATE
            lBekreft AS LOGICAL
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO.
        IF lBekreft THEN DO:
            DO TRANSACTION:
                CREATE Elogg.
                ASSIGN ELogg.EksterntSystem  = "KORRHK"
                       ELogg.TabellNavn      = "VPIArtBas"
                       ELogg.Verdier         = "0|" + STRING(dSanera2Artikkelnr) + "|"
                                                    + STRING(Artbas.artikkelnr)
                       ELogg.EndringsType    = 1 
                       ELogg.Behandlet       = FALSE.
                IF AVAILABLE Elogg THEN RELEASE Elogg.
            END. /* TRANSACTION */
            ASSIGN Artbas.IKasse:CHECKED   = FALSE
                   ArtBas.Sanertdato:SCREEN-VALUE = STRING(TODAY)
                   ArtBas.Beskr:screen-value = 'KORR: ' + ArtBas.Beskr:screen-value.
            RUN lagreArtbas(0).
            RUN vpikorreksjon.w.
            IF CAN-FIND(ArtBas WHERE ArtBas.ArtikkelNr = dSanera2Artikkelnr) THEN 
              DO:
                FIND ArtBas WHERE ArtBas.ArtikkelNr = dSanera2Artikkelnr NO-LOCK.
                ASSIGN wArtBasRecid = RECID(ArtBas).
              END.
            RUN VisArtBas.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SaveToLokalIni C-ArtKort 
PROCEDURE SaveToLokalIni :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER wKey     AS CHAR NO-UNDO.
DEF INPUT PARAMETER wValue   AS CHAR NO-UNDO.

DEF VAR wSection AS CHAR NO-UNDO.
DEF VAR wMappe   AS CHAR NO-UNDO.
DEF VAR wIniFil  AS CHAR NO-UNDO.

ASSIGN
    wSection = "FILTERPARAMETRE".

/* Henter mappe og filnavn på den lokale inifilen */
IF VALID-HANDLE(wLibHandle) THEN
DO:
  RUN Mappe  IN wLibHandle (OUTPUT wMappe).
  RUN IniFil IN wLibHandle (OUTPUT wIniFil).
END.
ELSE 
    RETURN "AVBRYT".

/* Lagrer parametre */
IF wMappe <> ? THEN 
  DO:
    /* Laster den lokale ini filen. */
    LOAD wIniFil DIR wMappe BASE-KEY "INI" NO-ERROR.
    /* Finnes den ikke, opprette den. */
    IF ERROR-STATUS:ERROR THEN DO:
       LOAD wIniFil DIR wMappe NEW BASE-KEY "INI" NO-ERROR.
       IF ERROR-STATUS:ERROR THEN RETURN "AVBRYT".
    END.           
    /* Stiller om til lokal ini-fil. */
    USE wIniFil NO-ERROR.
    /* Lagrer parameterverdien */
    IF NOT ERROR-STATUS:ERROR THEN
       PUT-KEY-VALUE SECTION wSection KEY wKey VALUE wValue NO-ERROR.
    /* Stiller tilbake til oppstarts ini fil. */
    UNLOAD wIniFil NO-ERROR.
  END.
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
    tmpLager.VVareKost:BGCOLOR  IN BROWSE BROWSE-LAger = 8
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
    tmpLager.Antall[48]:bgcolor IN BROWSE BROWSE-Lager = 8
    tmpLager.Antall[49]:bgcolor IN BROWSE BROWSE-Lager = 8

  tmpLager.Antall[50]:bgcolor IN BROWSE BROWSE-Lager = 8.
  tmpLager.Antall[51]:bgcolor IN BROWSE BROWSE-Lager = 8.
  tmpLager.Antall[52]:bgcolor IN BROWSE BROWSE-Lager = 8.
  tmpLager.Antall[53]:bgcolor IN BROWSE BROWSE-Lager = 8.
  tmpLager.Antall[54]:bgcolor IN BROWSE BROWSE-Lager = 8.
  tmpLager.Antall[55]:bgcolor IN BROWSE BROWSE-Lager = 8.
  tmpLager.Antall[56]:bgcolor IN BROWSE BROWSE-Lager = 8.
  tmpLager.Antall[57]:bgcolor IN BROWSE BROWSE-Lager = 8.
  tmpLager.Antall[58]:bgcolor IN BROWSE BROWSE-Lager = 8.
  tmpLager.Antall[59]:bgcolor IN BROWSE BROWSE-Lager = 8.

  tmpLager.Antall[60]:bgcolor IN BROWSE BROWSE-Lager = 8.
  tmpLager.Antall[61]:bgcolor IN BROWSE BROWSE-Lager = 8.
  tmpLager.Antall[62]:bgcolor IN BROWSE BROWSE-Lager = 8.
  tmpLager.Antall[63]:bgcolor IN BROWSE BROWSE-Lager = 8.
  tmpLager.Antall[64]:bgcolor IN BROWSE BROWSE-Lager = 8.
  tmpLager.Antall[65]:bgcolor IN BROWSE BROWSE-Lager = 8.
  tmpLager.Antall[66]:bgcolor IN BROWSE BROWSE-Lager = 8.
  tmpLager.Antall[67]:bgcolor IN BROWSE BROWSE-Lager = 8.
  tmpLager.Antall[68]:bgcolor IN BROWSE BROWSE-Lager = 8.
  tmpLager.Antall[69]:bgcolor IN BROWSE BROWSE-Lager = 8.

  tmpLager.Antall[70]:bgcolor IN BROWSE BROWSE-Lager = 8.
  tmpLager.Antall[71]:bgcolor IN BROWSE BROWSE-Lager = 8.
  tmpLager.Antall[72]:bgcolor IN BROWSE BROWSE-Lager = 8.
  tmpLager.Antall[73]:bgcolor IN BROWSE BROWSE-Lager = 8.
  tmpLager.Antall[74]:bgcolor IN BROWSE BROWSE-Lager = 8.
  tmpLager.Antall[75]:bgcolor IN BROWSE BROWSE-Lager = 8.
  tmpLager.Antall[76]:bgcolor IN BROWSE BROWSE-Lager = 8.
  tmpLager.Antall[77]:bgcolor IN BROWSE BROWSE-Lager = 8.
  tmpLager.Antall[78]:bgcolor IN BROWSE BROWSE-Lager = 8.
  tmpLager.Antall[79]:bgcolor IN BROWSE BROWSE-Lager = 8.

  tmpLager.Antall[80]:bgcolor IN BROWSE BROWSE-Lager = 8.
  tmpLager.Antall[81]:bgcolor IN BROWSE BROWSE-Lager = 8.
  tmpLager.Antall[82]:bgcolor IN BROWSE BROWSE-Lager = 8.
  tmpLager.Antall[83]:bgcolor IN BROWSE BROWSE-Lager = 8.
  tmpLager.Antall[84]:bgcolor IN BROWSE BROWSE-Lager = 8.
  tmpLager.Antall[85]:bgcolor IN BROWSE BROWSE-Lager = 8.
  tmpLager.Antall[86]:bgcolor IN BROWSE BROWSE-Lager = 8.
  tmpLager.Antall[87]:bgcolor IN BROWSE BROWSE-Lager = 8.
  tmpLager.Antall[88]:bgcolor IN BROWSE BROWSE-Lager = 8.
  tmpLager.Antall[89]:bgcolor IN BROWSE BROWSE-Lager = 8.

  tmpLager.Antall[90]:bgcolor IN BROWSE BROWSE-Lager = 8.
  tmpLager.Antall[91]:bgcolor IN BROWSE BROWSE-Lager = 8.
  tmpLager.Antall[92]:bgcolor IN BROWSE BROWSE-Lager = 8.
  tmpLager.Antall[93]:bgcolor IN BROWSE BROWSE-Lager = 8.
  tmpLager.Antall[94]:bgcolor IN BROWSE BROWSE-Lager = 8.
  tmpLager.Antall[95]:bgcolor IN BROWSE BROWSE-Lager = 8.
  tmpLager.Antall[96]:bgcolor IN BROWSE BROWSE-Lager = 8.
  tmpLager.Antall[97]:bgcolor IN BROWSE BROWSE-Lager = 8.
  tmpLager.Antall[98]:bgcolor IN BROWSE BROWSE-Lager = 8.
  tmpLager.Antall[99]:bgcolor IN BROWSE BROWSE-Lager = 8.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetGetBuntNr C-ArtKort 
PROCEDURE SetGetBuntNr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT-OUTPUT PARAMETER iBuntnr AS INTEGER    NO-UNDO.
   DEFINE INPUT        PARAMETER cType AS CHARACTER  NO-UNDO.
   IF cType = "GET" THEN
       ASSIGN iBuntnr = iSaveBuntNr.
   ELSE IF cType = "SET" THEN
       ASSIGN iSaveBuntNr = iBuntnr.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setLookupAttributes C-ArtKort 
PROCEDURE setLookupAttributes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihBrowse AS HANDLE NO-UNDO.
DEF INPUT PARAM ihDummy1 AS HANDLE NO-UNDO.
DEF INPUT PARAM ihDummy2 AS HANDLE NO-UNDO.

DEF VAR hCombo AS HANDLE NO-UNDO.

IF ihBrowse:QUERY:GET-BUFFER-HANDLE(1):NAME = "VarGr" THEN DO:


    CREATE COMBO-BOX hCombo 
      ASSIGN DELIMITER        = "|"
             DATA-TYPE        = "CHARACTER"
             FORMAT           = "x(256)"
             NAME             = "cmbHgr"
             SUBTYPE          = "DROP-DOWN-LIST"
             LIST-ITEM-PAIRS  = "<Velg hovedgruppe>|0|" + DYNAMIC-FUNCTION("getFieldList","HuvGr;HgBeskr;Hg","WHERE TRUE BY HgBeskr")
             INNER-LINES      = 50
             FRAME            = ihBrowse:FRAME
             X                = ihBrowse:X + 150
             Y                = ihBrowse:Y - 26
             WIDTH-PIXELS     = 300
             VISIBLE          = YES
             SENSITIVE        = TRUE
             HELP             = "Velg hovedgruppe"
             TOOLTIP          = "Velg hovedgruppe"
             TRIGGERS:
               ON VALUE-CHANGED PERSISTENT RUN setVgLookupFilter IN THIS-PROCEDURE (ihBrowse,hCombo).
/*                ON TAB           PERSISTENT RUN DoProcessEvent (SOURCE-PROCEDURE,"leave").     */
/*                ON BACK-TAB      PERSISTENT RUN DoProcessEvent (SOURCE-PROCEDURE,"leave").     */
/*                ON RETURN        PERSISTENT RUN DoProcessEvent (SOURCE-PROCEDURE,"leave").     */
/*                ON ENTRY         PERSISTENT RUN DoProcessEvent (SOURCE-PROCEDURE,"entry").     */
/*                ON END-ERROR     PERSISTENT RUN DoProcessEvent (SOURCE-PROCEDURE,"end-error"). */
             END TRIGGERS.
      ASSIGN hCombo:SCREEN-VALUE = '0'.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettDefaultVerdier C-ArtKort 
PROCEDURE SettDefaultVerdier :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER pArtSlag AS INT NO-UNDO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettEnableDisable C-ArtKort 
PROCEDURE SettEnableDisable :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

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
        RECID(ArtBas) = wArtBasRecid NO-ERROR.
      IF AVAILABLE ArtBas THEN
        DO:
          DISPLAY ArtBas.LopNr WITH FRAME DEFAULT-FRAME.        
          ASSIGN
            BUTTON-SettLopNr:hidden IN FRAME DEFAULT-FRAME    = TRUE
            BUTTON-SettLopNr:sensitive IN FRAME DEFAULT-FRAME = FALSE.
          RUN LagreArtBas (0).
          RUN genInterleaf.p (ArtBas.Artikkelnr).
        END.
    END.

END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setVgLookupFilter C-ArtKort 
PROCEDURE setVgLookupFilter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihBrowse AS HANDLE NO-UNDO.
DEF INPUT PARAM ihCombo  AS HANDLE NO-UNDO.

DYNAMIC-FUNCTION("setAttribute",ihBrowse,"queryFilter",
                 IF ihCombo:SCREEN-VALUE NE ? AND ihCombo:SCREEN-VALUE NE "" THEN 
                   " AND Hg = " + ihCombo:SCREEN-VALUE
                 ELSE "").

RUN InvokeMethod(ihBrowse,"OpenQuery").


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetWindowSensitive C-ArtKort 
PROCEDURE SetWindowSensitive :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF VALID-HANDLE(wCurrent-Window) THEN
ASSIGN
    wCurrent-Window:SENSITIVE = TRUE
    .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE skapaELogg C-ArtKort 
PROCEDURE skapaELogg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iCount      AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cArtikkelNr AS CHARACTER  NO-UNDO.
    DEFINE BUFFER bArtBas FOR ArtBas.
    IF ArtBas.ModellFarge = 0 THEN
        ASSIGN cArtikkelNr = STRING(ArtBas.ArtikkelNr).
    ELSE DO:
        FOR EACH bArtBas WHERE bArtBas.ModellFarge = ArtBas.ModellFarge AND 
                               bArtBas.Utgatt      = FALSE AND 
                               bArtBas.IKasse      = TRUE NO-LOCK:
            ASSIGN cArtikkelNr = cArtikkelNr + (IF cArtikkelNr <> "" THEN "," ELSE "") + STRING(bArtBas.ArtikkelNr).
        END.
    END.
    LOOPEN:
    DO iCount = 1 TO NUM-ENTRIES(cArtikkelNr):
       /* Logger utlegg for de profiler det gjelder. */
       FIND ELogg WHERE 
            ELogg.TabellNavn     = "ArtBas" AND
            ELogg.EksterntSystem = "POS"    AND
            ELogg.Verdier        = ENTRY(iCount,cArtikkelNr) NO-ERROR NO-WAIT.
       IF LOCKED ELogg THEN NEXT LOOPEN.
       IF NOT AVAIL Elogg THEN DO:
       CREATE Elogg.
       ASSIGN ELogg.TabellNavn     = "ArtBas"
              ELogg.EksterntSystem = "POS"   
              ELogg.Verdier        = ENTRY(iCount,cArtikkelNr).
       END.
       ASSIGN ELogg.EndringsType = 1
              ELogg.Behandlet    = FALSE.
       IF AVAILABLE ELogg THEN RELEASE ELogg.
       FIND bArtBas WHERE bArtBas.artikkelnr = DECI(ENTRY(iCount,cArtikkelNr)) NO-LOCK NO-ERROR.
       IF NOT AVAIL bArtBas OR bArtBas.WebButikkArtikkel = FALSE THEN
           NEXT LOOPEN.
       FIND ELogg WHERE
            ELogg.TabellNavn     = "ArtBas" AND
            ELogg.EksterntSystem = "WEBBUT"    AND
            ELogg.Verdier        = ENTRY(iCount,cArtikkelNr) NO-ERROR NO-WAIT.
       IF LOCKED ELogg THEN NEXT LOOPEN.
       IF NOT AVAIL Elogg THEN DO:
       CREATE Elogg.
       ASSIGN ELogg.TabellNavn     = "ArtBas"
              ELogg.EksterntSystem = "WEBBUT"   
              ELogg.Verdier        = ENTRY(iCount,cArtikkelNr).
       END.
       ASSIGN ELogg.EndringsType = 1
              ELogg.Behandlet    = FALSE.
       IF AVAILABLE ELogg THEN RELEASE ELogg.
       FOR EACH Lager NO-LOCK WHERE
           Lager.ArtikkelNr = DECI(ENTRY(iCount,cArtikkelNr)):
           FIND ELogg WHERE 
                ELogg.TabellNavn     = "Lager" AND
                ELogg.EksterntSystem = "WEBBUT"    AND
                ELogg.Verdier        = STRING(Lager.ArtikkelNr)
                                       + chr(1) + string(Lager.butik) NO-ERROR NO-WAIT.
           IF LOCKED ELogg THEN NEXT.
           ELSE DO:
             IF NOT AVAIL ELogg THEN DO:
                 CREATE ELogg.
                 ASSIGN ELogg.TabellNavn     = "Lager"
                        ELogg.EksterntSystem = "WEBBUT"   
                        ELogg.Verdier        = STRING(Lager.ArtikkelNr)
                                         + chr(1) + string(Lager.butik).
             END.
             ASSIGN ELogg.EndringsType = 1 
                    ELogg.Behandlet    = FALSE.
             RELEASE ELogg.
           END.
       END.
    END. /* LOOPEN */

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
  DEFINE VARIABLE lFinns AS LOGICAL     NO-UNDO.

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
        RETURN "AVBRYT".
    END.
  /* Kontroll mot individ. */
  IF CAN-FIND(FIRST Individ OF ArtBas) THEN
  DO:
    wBekreft = FALSE.
    MESSAGE Tx("Det er individer registrert på artikkelen." + chr(13) +
            "Skal den alikevel tas bort?" + CHR(13) + 
             "Solgte individer blir liggende etter at artikkelen er slettet, mens usolgte individer slettes",138) SKIP VIEW-AS ALERT-BOX 
            QUESTION BUTTONS YES-NO TITLE Tx("Bekreftelse",105) UPDATE wBekreft.
    IF wBekreft <> TRUE THEN         
      RETURN "AVBRYT".
  END.

  /* Kontroll mot kundetransaksjoner. */
  IF CAN-FIND(FIRST KundeTrans WHERE
    KundeTrans.ArtikkelNr = ArtBas.ArtikkelNr) THEN
  DO:
    wBekreft = FALSE.
    MESSAGE Tx("Det er kundetransaksjoner registrert på artikkelen." + chr(13) +
            "Skal den alikevel tas bort?",138) SKIP VIEW-AS ALERT-BOX 
            QUESTION BUTTONS YES-NO TITLE Tx("Bekreftelse",105) UPDATE wBekreft.
    IF wBekreft <> TRUE THEN         
      RETURN "AVBRYT".
  END.
  
  /* Kontroll mot medlemstransaksjoner. */
  FOR EACH Medtrans WHERE MedTrans.ArtikkelNr = ArtBas.ArtikkelNr NO-LOCK.
      IF YEAR(Medtrans.dato) = YEAR(TODAY) THEN DO:
          lFinns = TRUE.
          LEAVE.
      END.
  END.
  IF lFinns THEN DO:
    wBekreft = FALSE.
    MESSAGE Tx("Det er medlemstransaksjoner registrert på artikkelen dette år." + chr(13) +
            "Skal den alikevel tas bort?",139) SKIP VIEW-AS ALERT-BOX 
            QUESTION BUTTONS YES-NO TITLE Tx("Bekreftelse",105) UPDATE wBekreft.
    IF wBekreft <> TRUE THEN         
      RETURN "AVBRYT".
  END.
/*   IF CAN-FIND(FIRST MedTrans WHERE                                                 */
/*     MedTrans.ArtikkelNr = ArtBas.ArtikkelNr) THEN                                  */
/*   DO:                                                                              */
/*     wBekreft = FALSE.                                                              */
/*     MESSAGE Tx("Det er medlemstransaksjoner registrert på artikkelen." + chr(13) + */
/*             "Skal den alikevel tas bort?",139) SKIP VIEW-AS ALERT-BOX              */
/*             QUESTION BUTTONS YES-NO TITLE Tx("Bekreftelse",105) UPDATE wBekreft.   */
/*     IF wBekreft <> TRUE THEN                                                       */
/*       RETURN "AVBRYT".                                                             */
/*   END.                                                                             */

  /* Om pakke, kontrollera om det finns pakkelinjer och få en bekräftelse */
  IF ArtBas.Pakke AND CAN-FIND(FIRST PakkeLinje OF ArtBas) THEN DO:
      ASSIGN wBekreft = FALSE.
      MESSAGE "Det er registrert pakkelinje(er) på artikkelen." SKIP
              "Skal den alikevel tas bort?" SKIP VIEW-AS ALERT-BOX 
              QUESTION BUTTONS YES-NO TITLE Tx("Bekreftelse",105) UPDATE wBekreft.
      IF wBekreft <> TRUE THEN         
        RETURN "AVBRYT".
  END.
  ELSE IF NOT ArtBas.Pakke AND CAN-FIND(FIRST PakkeLinje 
                      WHERE PakkeLinje.PKartikkelNr = ArtBas.Artikkelnr) THEN DO:
      MESSAGE "Artikkelen ingår i pakke og kann ikke slettes!"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN "AVBRYT".
  END.
  
  STATUS DEFAULT "Sletter artikkelen...".
  RUN slettartbasbatch.w (INPUT ArtBas.ArtikkelNr).
  IF RETURN-VALUE <> "" THEN
  DO:
      STATUS DEFAULT "".
      MESSAGE "Sletting av artikkel avbrutt. Årsak: " SKIP
          RETURN-VALUE "." SKIP
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN "AVBRYT".
  END.
  STATUS DEFAULT "".

  RETURN NO-APPLY "SLETTET".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettBestilling C-ArtKort 
PROCEDURE SlettBestilling :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER rBestHodeRecid AS RECID      NO-UNDO.

  DO TRANSACTION:
      {sww.i}
      /* KanSlettes*/      
      RUN w-gridord.w (INPUT wArtBasRecid, INPUT-OUTPUT rBestHodeRecid, "SLETT").
      {swn.i}
      RETURN "OK".
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettBilde C-ArtKort 
PROCEDURE SlettBilde :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipBildNr AS INTEGER    NO-UNDO.
    FIND Bilderegister WHERE Bilderegister.Bildnr = ipBildnr NO-ERROR.
    IF AVAIL Bilderegister THEN DO:
        FOR EACH bildedata OF bilderegister:
            DELETE bildedata.
        END.
        DELETE Bilderegister.
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
    IF VALID-HANDLE(tmpChild.wChild) THEN
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

  IF wModus <> "NY" AND AVAIL ArtBas AND ArtBas.ArtikkelNr = ArtBas.Vg THEN DO:
      MESSAGE "PLU-artikkel, endring avbrutt."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
  END.

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
      cTekst = "Anv-Id".
      /*RUN JBoxDLookup.w ("Anv-Kod;AnvBeskr;Anv-Id", "where true", INPUT-OUTPUT cTekst).*/
      THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
      RUN JBoxLookup.w (THIS-PROCEDURE,50,
                        "Anv-Kod;AnvBeskr;Anv-Id"
                       ,"WHERE TRUE"
                        ,""                                                  
                        ,"Anv-Id",   /* <- return values for these fields */
                          OUTPUT cTekst,
                          OUTPUT bOK).
      THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

      IF NOT bOK THEN
          RETURN NO-APPLY.
      FIND Anv-Kod NO-LOCK WHERE
        Anv-Kod.Anv-Id = INT(cTekst) NO-ERROR.
      IF AVAILABLE Anv-Kod THEN
      DO:
          /* Legger opp verdier I de aktuelle feltene */
          ASSIGN
            ArtBas.Anv-Id:SCREEN-VALUE   = cTekst
            Anv-Kod.AnvBeskr:SCREEN-VALUE = Anv-Kod.AnvBeskr
            .
      END.
      ELSE DO:
          /* Legger opp verdier I de aktuelle feltene */
          ASSIGN
            ArtBas.Anv-Id:SCREEN-VALUE    = ''
            Anv-Kod.AnvBeskr:SCREEN-VALUE  = ''
            .
      END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SokBehKode C-ArtKort 
PROCEDURE SokBehKode :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME FRAME-ArtInfo:
      cTekst = "HandKode".
      /*RUN JBoxDLookup.w ("Handtering;Beskrivelse;HandKode", "where true", INPUT-OUTPUT cTekst).*/
      THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
      RUN JBoxLookup.w (THIS-PROCEDURE,50,
                        "Handtering;Beskrivelse;HandKode"
                       ,"WHERE TRUE"
                        ,""                                                  
                        ,"HandKode",   /* <- return values for these fields */
                          OUTPUT cTekst,
                          OUTPUT bOK).
      THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

      IF NOT bOK THEN
          RETURN NO-APPLY.
      FIND Handtering NO-LOCK WHERE
        Handtering.HandKode = INT(cTekst) NO-ERROR.
      IF AVAILABLE Handtering THEN
      DO:
          /* Legger opp verdier I de aktuelle feltene */
          ASSIGN
            ArtBas.BehKode:SCREEN-VALUE   = cTekst
            Handtering.Beskrivelse:SCREEN-VALUE = Handtering.Beskrivelse
            .
      END.
      ELSE DO:
          /* Legger opp verdier I de aktuelle feltene */
          ASSIGN
            ArtBas.BehKode:SCREEN-VALUE    = ''
            Handtering.Beskrivelse:SCREEN-VALUE  = ''
            .
      END.
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
      cTekst = "Farg".
      /*RUN JBoxDLookup.w ("Farg;FarBeskr;Farg|Farge|>>>>9", "where true", INPUT-OUTPUT cTekst). */
      THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
      RUN JBoxLookup.w (THIS-PROCEDURE,50,
                        "Farg;FarBeskr;Farg|Farge|>>>>9"
                       ,"WHERE TRUE"
                        ,""                                                  
                        ,"Farg",   /* <- return values for these fields */
                          OUTPUT cTekst,
                          OUTPUT bOK).
      THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.


      IF NOT bOK THEN
          RETURN NO-APPLY.
      FIND Farg NO-LOCK WHERE
        Farg.Farg = INT(cTekst) NO-ERROR.
      IF AVAILABLE Farg THEN
      DO:
          /* Legger opp verdier I de aktuelle feltene */
          ASSIGN
            ArtBas.Farg:SCREEN-VALUE   = cTekst
            Farg.FarBeskr:SCREEN-VALUE = Farg.FarBeskr
            .
      END.
      ELSE DO:
          /* Legger opp verdier I de aktuelle feltene */
          ASSIGN
            ArtBas.Farg:SCREEN-VALUE    = ''
            Farg.FarBeskr:SCREEN-VALUE  = ''
            .
      END.
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
      cTekst = "Inner-Id".
      /*RUN JBoxDLookup.w ("Innersula;InnerBeskr;Inner-Id", "where true", INPUT-OUTPUT cTekst).*/
      THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
      RUN JBoxLookup.w (THIS-PROCEDURE,50,
                        "Innersula;InnerBeskr;Inner-Id"
                       ,"WHERE TRUE"
                        ,""                                                  
                        ,"Inner-Id",   /* <- return values for these fields */
                          OUTPUT cTekst,
                          OUTPUT bOK).
      THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

      IF NOT bOK THEN
          RETURN NO-APPLY.
      FIND Innersula NO-LOCK WHERE
        Innersula.Inner-Id = INT(cTekst) NO-ERROR.
      IF AVAILABLE Innersula THEN
      DO:
          /* Legger opp verdier I de aktuelle feltene */
          ASSIGN
            ArtBas.Inner-Id:SCREEN-VALUE   = cTekst
            Innersula.InnerBeskr:SCREEN-VALUE = Innersula.InnerBeskr
            .
      END.
      ELSE DO:
          /* Legger opp verdier I de aktuelle feltene */
          ASSIGN
            ArtBas.Inner-Id:SCREEN-VALUE    = ''
            Innersula.InnerBeskr:SCREEN-VALUE  = ''
            .
      END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SokKlack C-ArtKort 
PROCEDURE SokKlack :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cTekst AS CHAR NO-UNDO.

  DO WITH FRAME FRAME-ArtInfo:
      cTekst = "Klack".
      /*RUN JBoxDLookup.w ("Klack;Beskrivning;Klack-id|Hæl|>>9", "where true", INPUT-OUTPUT cTekst).*/
      THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
      RUN JBoxLookup.w (THIS-PROCEDURE,50,
                        "Klack;Beskrivning;Klack-id|Hæl|>>9"
                       ,"WHERE TRUE"
                        ,""                                                  
                        ,"Klack-id",   /* <- return values for these fields */
                          OUTPUT cTekst,
                          OUTPUT bOK).
      THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

      IF NOT bOK THEN
          RETURN NO-APPLY.
      FIND Klack NO-LOCK WHERE
        Klack.Klack = INT(cTekst) NO-ERROR.
      IF AVAILABLE Klack THEN
      DO:
          /* Legger opp verdier I de aktuelle feltene */
          ASSIGN
            ArtBas.Klack:SCREEN-VALUE   = cTekst
            Klack.Beskrivning:SCREEN-VALUE = Klack.Beskrivning
            .
      END.
      ELSE DO:
          /* Legger opp verdier I de aktuelle feltene */
          ASSIGN
            ArtBas.Klack:SCREEN-VALUE    = ''
            Klack.Beskrivning:SCREEN-VALUE  = ''
            .
      END.
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
      cTekst = "Last-Id".
      /*RUN JBoxDLookup.w ("Last-Sko;LastBeskr;Last-Id", "where true", INPUT-OUTPUT cTekst).*/
      THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
      RUN JBoxLookup.w (THIS-PROCEDURE,50,
                        "Last-Sko;LastBeskr;Last-Id"
                       ,"WHERE TRUE"
                        ,""                                                  
                        ,"Last-Id",   /* <- return values for these fields */
                          OUTPUT cTekst,
                          OUTPUT bOK).
      THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

      IF NOT bOK THEN
          RETURN NO-APPLY.
      FIND Last-Sko NO-LOCK WHERE
        Last-Sko.Last-Id = INT(cTekst) NO-ERROR.
      IF AVAILABLE Last-Sko THEN
      DO:
          /* Legger opp verdier I de aktuelle feltene */
          ASSIGN
            ArtBas.Last-Id:SCREEN-VALUE   = cTekst
            Last-Sko.LastBeskr:SCREEN-VALUE = Last-Sko.LastBeskr
            .
      END.
      ELSE DO:
          /* Legger opp verdier I de aktuelle feltene */
          ASSIGN
            ArtBas.Last-Id:SCREEN-VALUE    = ''
            Last-Sko.LastBeskr:SCREEN-VALUE  = ''
            .
      END.
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
  cTekst = "LevNr".
  RUN JBoxDLookup.w ("LevBas;LevNamn;LevNr;Kjedeavtale", "where true", INPUT-OUTPUT cTekst).
  IF cTekst = '' THEN RETURN.
  FIND LevBas NO-LOCK WHERE
    LevBas.LevNr = INT(cTekst) NO-ERROR.
  IF AVAILABLE LevBas THEN
  DO:
      /* Legger opp verdier I de aktuelle feltene */
      ASSIGN
        ArtBas.LevNr:SCREEN-VALUE   = cTekst
        FILL-IN-LevNamn:SCREEN-VALUE = LevBas.LevNamn
        .
  END.
  ELSE DO:
      /* Legger opp verdier I de aktuelle feltene */
      ASSIGN
        ArtBas.LevNr:SCREEN-VALUE    = ''
        FILL-IN-LevNamn:SCREEN-VALUE  = ''
        .
  END.

  IF AVAILABLE LevBas THEN
     FIND Valuta NO-LOCK WHERE 
        Valuta.ValKod = LevBas.Valkod NO-ERROR.

  IF AVAILABLE LevBas THEN
     RUN InitDivInfo.

  /* Leverandørnummer er ikke endret. */
  IF AVAILABLE ArtBas THEN
  DO: /* Utføres kun ved endring på eksisterende artikkel */ 
    
    IF LevBas.LevNr <> ArtBas.LevNr AND
     wModus <> "NY" THEN
    LEVFIX:
    DO TRANSACTION:
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
          IF RETURN-VALUE = "AVBRYT" THEN DO:
              MESSAGE "Lagring avbrutt"
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
              UNDO LEVFIX, LEAVE LEVFIX.
          END.
          RUN LagreArtBas (0).
          IF RETURN-VALUE = "AVBRYT" THEN DO:
              MESSAGE "Lagring avbrutt"
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
              UNDO LEVFIX, LEAVE LEVFIX.
          END.
/*           IF ArtBas.ModellFarge <> 0 THEN                                         */
/*               FOR EACH bArtBas WHERE bArtBas.ModellFarge = ArtBas.ModellFarge AND */
/*                                      bArtBas.ArtikkelNr <> ArtBas.ArtikkelNr:     */
/*               ASSIGN bArtBas.LevNr = LevBas.LevNr.                                */
/*           END.                                                                    */
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SokMatKod C-ArtKort 
PROCEDURE SokMatKod :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME FRAME-ArtInfo:
      cTekst = "MatKod".
      /*RUN JBoxDLookup.w ("Material;MatBeskr;MatKod", "where true", INPUT-OUTPUT cTekst).*/
      THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
      RUN JBoxLookup.w (THIS-PROCEDURE,50,
                        "Material;MatBeskr;MatKod"
                       ,"WHERE TRUE"
                        ,""                                                  
                        ,"Matkod",   /* <- return values for these fields */
                          OUTPUT cTekst,
                          OUTPUT bOK).
      THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

      IF NOT bOK THEN
          RETURN NO-APPLY.
      FIND Material NO-LOCK WHERE
        Material.MatKod = INT(cTekst) NO-ERROR.
      IF AVAILABLE Material THEN
      DO:
          /* Legger opp verdier I de aktuelle feltene */
          ASSIGN
            ArtBas.MatKod:SCREEN-VALUE   = cTekst
            Material.MatBeskr:SCREEN-VALUE = Material.MatBeskr
            .
      END.
      ELSE DO:
          /* Legger opp verdier I de aktuelle feltene */
          ASSIGN
            ArtBas.MatKod:SCREEN-VALUE    = ''
            Material.MatBeskr:SCREEN-VALUE  = ''
            .
      END.
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
      cTekst = "Ov-Id".
      /*RUN JBoxDLookup.w ("Ovandel;OvBeskr;Ov-Id", "where true", INPUT-OUTPUT cTekst).*/
      THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
      RUN JBoxLookup.w (THIS-PROCEDURE,50,
                        "Ovandel;OvBeskr;Ov-Id"
                       ,"WHERE TRUE"
                        ,""                                                  
                        ,"Ov-Id",   /* <- return values for these fields */
                          OUTPUT cTekst,
                          OUTPUT bOK).
      THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

      IF NOT bOK THEN
          RETURN NO-APPLY.
      FIND Ovandel NO-LOCK WHERE
        Ovandel.Ov-Id = INT(cTekst) NO-ERROR.
      IF AVAILABLE Ovandel THEN
      DO:
          /* Legger opp verdier I de aktuelle feltene */
          ASSIGN
            ArtBas.Ov-Id:SCREEN-VALUE   = cTekst
            Ovandel.OvBeskr:SCREEN-VALUE = Ovandel.OvBeskr
            .
      END.
      ELSE DO:
          /* Legger opp verdier I de aktuelle feltene */
          ASSIGN
            ArtBas.Ov-Id:SCREEN-VALUE    = ''
            Ovandel.OvBeskr:SCREEN-VALUE  = ''
            .
      END.
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
  DO WITH FRAME FRAME-ArtInfo:
      cTekst = "Sasong".
      /*RUN JBoxDLookup.w ("Sasong;SasBeskr;Sasong", "where true", INPUT-OUTPUT cTekst).*/
      THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
      RUN JBoxLookup.w (THIS-PROCEDURE,50,
                        "Sasong;SasBeskr;Sasong;StartDato"
                       ,"WHERE TRUE"
                        ,""                                                  
                        ,"Sasong",   /* <- return values for these fields */
                          OUTPUT cTekst,
                          OUTPUT bOK).
      THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

      IF NOT bOK THEN
          RETURN NO-APPLY.
      FIND Sasong NO-LOCK WHERE
        Sasong.Sasong = INT(cTekst) NO-ERROR.
      IF AVAILABLE Sasong THEN
      DO:
          /* Legger opp verdier I de aktuelle feltene */
          ASSIGN
            ArtBas.Sasong:SCREEN-VALUE   = cTekst
            Sasong.SasBeskr:SCREEN-VALUE = Sasong.SasBeskr
            .
      END.
      ELSE DO:
          /* Legger opp verdier I de aktuelle feltene */
          ASSIGN
            ArtBas.Sasong:SCREEN-VALUE    = ''
            Sasong.SasBeskr:SCREEN-VALUE  = ''
            .
      END.
  END.

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
      cTekst = "Slit-Id".
      /*RUN JBoxDLookup.w ("SlitSula;SlitBeskr;Slit-Id", "where true", INPUT-OUTPUT cTekst).*/
      THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
      RUN JBoxLookup.w (THIS-PROCEDURE,50,
                        "SlitSula;SlitBeskr;Slit-Id"
                       ,"WHERE TRUE"
                        ,""                                                  
                        ,"Slit-Id",   /* <- return values for these fields */
                          OUTPUT cTekst,
                          OUTPUT bOK).
      THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

      IF NOT bOK THEN
          RETURN NO-APPLY.
      FIND Slitsula NO-LOCK WHERE
        SlitSula.Slit-Id = INT(cTekst) NO-ERROR.
      IF AVAILABLE Slitsula THEN
      DO:
          /* Legger opp verdier I de aktuelle feltene */
          ASSIGN
            ArtBas.Slit-Id:SCREEN-VALUE   = cTekst
            SlitSula.SlitBeskr:SCREEN-VALUE = SlitSula.SlitBeskr
            .
      END.
      ELSE DO:
          /* Legger opp verdier I de aktuelle feltene */
          ASSIGN
            ArtBas.Slit-Id:SCREEN-VALUE    = ''
            SlitSula.SlitBeskr:SCREEN-VALUE  = ''
            .
      END.
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
      cTekst = "ValKod".
      /*RUN JBoxDLookup.w ("Valuta;ValKod;ValNavn;ValKurs;ValDatum;ValLand", "where true", INPUT-OUTPUT cTekst).*/
      THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
      RUN JBoxLookup.w (THIS-PROCEDURE,50,
                        "Valuta;ValKod;ValNavn;ValKurs;ValDatum;ValLand"
                       ,"WHERE TRUE"
                        ,""                                                  
                        ,"ValKod",   /* <- return values for these fields */
                          OUTPUT cTekst,
                          OUTPUT bOK).
      THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

      IF NOT bOK THEN
          RETURN NO-APPLY.

      FIND Valuta NO-LOCK WHERE
        Valuta.ValKod = (cTekst) NO-ERROR.
      IF AVAILABLE Valuta THEN
      DO:
          /* Legger opp verdier I de aktuelle feltene */
          ASSIGN
            ArtBas.ValKod:SCREEN-VALUE   = cTekst
            Valuta.ValKurs:SCREEN-VALUE = STRING(Valuta.ValKurs)
            .
      END.
      ELSE DO:
          /* Legger opp verdier I de aktuelle feltene */
          ASSIGN
            ArtBas.ValKod:SCREEN-VALUE    = ''
            Valuta.ValKurs:SCREEN-VALUE  = ''
            .
      END.
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
      cTekst = "Vg".
      /*
      RUN JBoxDLookup.w ("VarGr;VgBeskr;Vg;Hg", "where true", INPUT-OUTPUT cTekst).
      */
      /*
      RUN JBoxDLookup.w ("VarGr;vg;vgBeskr,HuvGr;Hg;HgBeskr,Moms;MomsProc@3,Avdeling;AvdelingNr;AvdelingNavn",
                         "WHERE true,FIRST HuvGr OF VarGr NO-LOCK, FIRST Moms of VarGr, FIRST Avdeling OF HuvGr NO-LOCK",
                         INPUT-OUTPUT cTekst).
      */
      THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
      RUN JBoxLookup.w (THIS-PROCEDURE,50,
                        "VarGr;vg;vgBeskr,HuvGr;Hg;HgBeskr,Moms;MomsProc@3,Avdeling;AvdelingNr;AvdelingNavn"
                       ,"WHERE true,FIRST HuvGr OF VarGr NO-LOCK, FIRST Moms of VarGr, FIRST Avdeling OF HuvGr NO-LOCK"
                        ,""                                                  
                        ,"Vg",   /* <- return values for these fields */
                          OUTPUT cTekst,
                          OUTPUT bOK).
      THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

      IF NOT bOK THEN
          RETURN NO-APPLY.
      FIND VarGr NO-LOCK WHERE
        VarGr.Vg = INT(cTekst) NO-ERROR.
      IF AVAILABLE VarGr THEN
      DO:
          /* Legger opp verdier I de aktuelle feltene */
          ASSIGN
            ArtBas.Vg:SCREEN-VALUE   = cTekst
            FILL-IN-VgBeskr:SCREEN-VALUE = VarGr.VgBeskr
            .
      END.
      ELSE DO:
          /* Legger opp verdier I de aktuelle feltene */
          ASSIGN
            ArtBas.Vg:SCREEN-VALUE    = ''
            FILL-IN-VgBeskr:SCREEN-VALUE  = ''
            .
      END.
      IF wModus = "Ny" AND (iVgTxtBeskr = 1 OR iVgTxtBeskr = 3) AND 
        AVAILABLE VarGr AND
        ArtBas.BongTekst:screen-value IN FRAME FRAME-ArtInfo = "" THEN
          DISPLAY VarGr.VgBeskr @ ArtBas.BongTekst WITH FRAME FRAME-ArtInfo.       
      IF wKopi  AND (iVgTxtBeskr = 1 OR iVgTxtBeskr = 3) AND 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartHjelp C-ArtKort 
PROCEDURE StartHjelp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

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
    ASSIGN BUTTON-Ny:SENSITIVE     = cNyreg = "yes"
           BUTTON-Kopier:SENSITIVE = BUTTON-Ny:SENSITIVE
           BUTTON-Slett:SENSITIVE  = BUTTON-Ny:SENSITIVE
           BUTTON-ModellFarg:SENSITIVE = BUTTON-Ny:SENSITIVE.

    ASSIGN
    BUTTON-Ny:sensitive     = IF wModus = "Ny" THEN FALSE ELSE cNyreg = "yes"
 /* BUTTON-Lagre:sensitive  = if wModus = "Ny" then false else true */
    BUTTON-Kopier:sensitive = BUTTON-Ny:SENSITIVE
    BUTTON-Slett:sensitive  = BUTTON-Ny:SENSITIVE
 /* BUTTON-Angre:sensitive  = if wModus = "Ny" then false else true */
    BUTTON-Prev:sensitive   = IF wModus = "Ny" THEN FALSE ELSE TRUE
    BUTTON-Next:sensitive   = IF wModus = "Ny" THEN FALSE ELSE TRUE
    BUTTON-Ok:sensitive     = IF wModus = "Ny" THEN FALSE ELSE TRUE
    BUTTON-ModellFarg:SENSITIVE = IF wModus = "Ny" THEN FALSE ELSE TRUE.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TvangKalkyle C-ArtKort 
PROCEDURE TvangKalkyle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    chTabStrip:ENABLED = FALSE.
    ASSIGN wAktivFlip = 3.
    RUN ByttFrame. /* For † enable knapper og felt korrekt */
    FRAME DEFAULT-FRAME:SENSITIVE = FALSE.

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

  IF VALID-HANDLE(hJmfRutine) THEN
      DELETE PROCEDURE hJmfRutine.

  FIND ArtBas NO-LOCK WHERE
    RECID(ArtBas) = wArtBasRecid NO-ERROR.
  IF NOT AVAILABLE ArtBas THEN
    RETURN NO-APPLY "AVBRYT".

DO WITH FRAME FRAME-ArtInfo:
                            
  /* Det åpnes for tildeling av løpenummer */
      
  FIND VarGr     OF ArtBAs NO-LOCK NO-ERROR.
  FIND HuvGr     OF ArtBas NO-LOCK NO-ERROR.
  FIND Valuta    OF ArtBas NO-LOCK NO-ERROR.
  FIND LevBas    OF ArtBas NO-LOCK NO-ERROR.
  FIND Farg      OF ArtBas NO-LOCK NO-ERROR.
  FIND Sasong    OF ArtBas NO-LOCK NO-ERROR.
  IF cVisMaterial = "1" THEN FIND Material  OF ArtBas NO-LOCK NO-ERROR.
  FIND Bilderegister OF ArtBas NO-LOCK NO-ERROR.
  FIND Varemerke OF ArtBas NO-LOCK NO-ERROR.
  FIND Produsent OF ArtBas NO-LOCK NO-ERROR.
  FIND StrType   OF ArtBas NO-LOCK NO-ERROR.
  FIND Regnskapsavdeling OF ArtBas NO-LOCK NO-ERROR.
  FIND Hovedkategori OF ArtBas NO-LOCK NO-ERROR.
  IF AVAILABLE NumLandKode THEN RELEASE NumLandKode.
  FIND FIRST AlfaLandKode NO-LOCK WHERE AlfaLandKode.AlfaKode2 = ArtBas.AlfaKode2 NO-ERROR.
  IF AVAILABLE AlfaLandKode THEN FIND NumLandKode OF AlfaLandKode NO-LOCK NO-ERROR.
  IF NOT AVAILABLE Regnskapsavdeling THEN RUN Regnskapsavdeling (INPUT ArtBas.RAvdNr).
  IF cVisInnersula = "1" THEN FIND Innersula OF ArtBas NO-LOCK NO-ERROR.
  IF cVisBruks = "1" THEN FIND Anv-Kod   OF ArtBas NO-LOCK NO-ERROR.
  IF cVisLast = "1" THEN FIND Last-Sko  OF ArtBas NO-LOCK NO-ERROR.
  IF cVisSlit = "1" THEN FIND Slitsula  OF ArtBas NO-LOCK NO-ERROR.
  IF cVisInnerfor = "1" THEN FIND Ovandel   OF ArtBas NO-LOCK NO-ERROR.
  IF cVisKlack = "1" THEN FIND Klack NO-LOCK WHERE Klack.Klack-Id = ArtBas.Klack NO-ERROR.
  IF cVisBehKod = "1" THEN FIND Handtering NO-LOCK WHERE Handtering.HandKode = ArtBas.BehKode NO-ERROR.
  
  ASSIGN
      cLevBasRowIdList = ""
      cLevBasIdList    = ""
      .
  ASSIGN TOGGLE-Annonse = IF AVAILABLE ArtBas THEN 
                             ArtBas.AnonseArtikkel ELSE FALSE.
/*     T-LapTop       = ArtBas.LapTop. */
  
  IF AVAILABLE ArtBas THEN
  DO:
    ASSIGN
      iArtSlag = ArtBas.ArtSlag  
      HuvGr.HgBeskr:TOOLTIP  IN FRAME DEFAULT-FRAME = IF AVAILABLE HuvGr
                                                        THEN HuvGr.HgBeskr
                                                        ELSE ""
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
    CB-EkstVPILev:SCREEN-VALUE IN FRAME DEFAULT-FRAME = ENTRY(2,CB-EkstVPILev:LIST-ITEM-PAIRS).
    TB-WebButikkArtikkel:checked = ArtBas.WebButikkArtikkel.
    FI-ArtBut = getArtBut().
    DISPLAY 
      ArtBas.ArtikkelNr
      ArtBas.Vg 
      ArtBas.LopNr WHEN ArtBas.LopNr <> ?
      "" WHEN ArtBas.LopNr = ? @ ArtBas.LopNr 
      ArtBas.VgKat 
      ArtBas.Beskr  
      ArtBas.LevNr 
      ArtBas.LevKod
      ArtBas.Utgatt 
      ArtBas.KjedeVare
      ArtBas.Gjennomfaktureres
      ArtBas.IndividType
      ArtBas.lager 
      ArtBas.BildeIKasse
      ArtBas.IKasse     
      ArtBas.OPris      
      ArtBas.Pakke
      ArtBas.NON_Sale
      ArtBas.NegVare
      ArtBas.BestForsl
      ArtBas.Pant
      ArtBas.ArtSlag
      ArtBas.HoyLavMva
      VarGr.VgBeskr WHEN AVAILABLE VarGr @ FILL-IN-VgBeskr
      LevBas.LevNamn WHEN AVAILABLE LevBas @ FILL-IN-LevNamn
      huvgr.hg WHEN AVAILABLE HuvGr
      HuvGr.HgBeskr WHEN AVAILABLE HuvGr
      ArtBas.ManueltOpprettet ArtBas.Grunnsortiment ArtBas.Telefonkort
/*       ArtBas.BildNr */
/*       T-LapTop */
      TOGGLE-Annonse
      FI-PaTilbud
      ArtBas.LevFargKod @ FI-LevFargKod
        /**/  ArtBas.HkStyrt
        /**/  ArtBas.LokPris
        /**/  ArtBas.KjentPaHK 
      ArtBas.Sanertdato ArtBas.UtgattDato
      FI-ArtBut
    WITH FRAME DEFAULT-FRAME.
/*     B-Kobble:SENSITIVE = TB-WebButikkArtikkel:checked. */
    IF INTEGER(ArtBas.VareType) < 2 THEN 
      ASSIGN
         ArtBas.Leveringstid:SCREEN-VALUE IN FRAME FRAME-ArtInfo = '0'
         ArtBas.Leveringstid:SENSITIVE = FALSE.
    ELSE
      ASSIGN
         ArtBas.Leveringstid:SENSITIVE = TRUE.
  END.
  RUN VisEndretinfo.
  DISPLAY
      ArtBas.KundeRabatt ArtBas.ManRabIKas ArtBas.MengdeRabatt
      ArtBas.Inn_Dato
      ArtBas.LevFargKod
      /**/  ArtBas.Alder   
      ArtBas.VMId ArtBas.HovedKatNr HovedKategori.HovedKatTekst WHEN AVAILABLE HovedKategori
      VareMerke.Beskrivelse WHEN AVAILABLE VareMerke
      ArtBas.ProdNr
      Produsent.Beskrivelse WHEN AVAILABLE Produsent
      ArtBas.StrTypeId
      StrType.Beskrivelse WHEN AVAILABLE StrType
      ArtBas.Sasong
      Sasong.SasBeskr WHEN AVAILABLE Sasong
      ArtBas.Farg
      Farg.FarBeskr WHEN AVAILABLE Farg
      ArtBas.MatKod WHEN cVisMaterial = "1"
      Material.MatBeskr WHEN AVAILABLE Material
      ArtBas.Klack WHEN cVisKlack = "1"
      Valuta.ValKurs WHEN AVAILABLE Valuta
      ArtBas.BongTekst
      ArtBas.valkod ArtBas.Leveringstid ArtBas.WebLeveringstid ArtBas.WebMinLager ArtBas.Kampanjekode
      ArtBas.GarantiKl ArtBas.OnLineLevNr 
      ArtBas.Lokasjon
      ArtBas.Etikettekst1
      ArtBas.Etikettekst2
      ArtBas.Anv-Id WHEN cVisBruks = "1"  
      Anv-Kod.AnvBeskr WHEN AVAILABLE Anv-Kod
      ArtBas.Last-Id WHEN cVisLast = "1"
      Last-Sko.LastBeskr   WHEN AVAILABLE Last-Sko
      ArtBas.Slit-Id WHEN cVisSlit = "1"
      SlitSula.SlitBeskr    WHEN AVAILABLE SlitSula
      ArtBas.Ov-Id WHEN cVisInnerfor = "1"
      Ovandel.OvBeskr       WHEN AVAILABLE Ovandel
      ArtBas.Inner-Id WHEN cVisInnersula = "1"
      InnerSula.InnerBeskr WHEN AVAILABLE InnerSula
      Klack.Beskrivning WHEN AVAILABLE Klack
      ArtBas.Notat ArtBas.Kjokkenskriver ArtBas.SalgsStopp
      "" @ FILL-IN-TilbPris
      ArtBas.BehKode WHEN cVisBehKod = "1"
      Handtering.Beskrivelse WHEN AVAILABLE Handtering
      ArtBas.LinkVareNr ArtBas.LinkVareAnt
      ArtBas.RAvdNr      
      Regnskapsavdeling.RAvdBeskrivelse WHEN AVAILABLE Regnskapsavdeling
      FI-1
      FI-2 WHEN cVisMaterial = "1"
      FI-3 WHEN cVisKlack = "1"
      FI-4 WHEN cVisInnersula = "1"
      FI-5 WHEN cVisInnerfor  = "1"
      FI-6 WHEN cVisSlit      = "1"
      FI-7 WHEN cVisLast      = "1"
      FI-8 WHEN cVisBruks     = "1"
      T-1
      T-2 WHEN cVisMaterial = "1"
      T-3 WHEN cVisKlack = "1"
      T-4 WHEN cVisInnersula = "1"
      T-5 WHEN cVisInnerfor  = "1"
      T-6 WHEN cVisSlit      = "1"
      T-7 WHEN cVisLast      = "1"
      T-8 WHEN cVisBruks     = "1"
      T-9
      T-10
      ArtBas.Etikett ArtBas.Bonus_Givende
      ArtBas.SalgsEnhet
      ArtBas.AntIPakn ArtBas.PubliserINettbutikk 
      ArtBas.InkrAnbrekk 
      ArtBas.Anbrekk ArtBas.Depositum
      ArtBas.WebButikkArtikkel ArtBas.Link_Til_Nettside
      ArtBas.Mengde 
    WITH FRAME FRAME-ArtInfo.
    ASSIGN
        ArtBas.VareType:SCREEN-VALUE IN FRAME FRAME-ArtInfo = STRING(ArtBas.VareType) 
        ArtBas.JamforEnhet:SCREEN-VALUE IN FRAME FRAME-ArtInfo = IF TRIM(ArtBas.JamforEnhet) = '' 
                                            THEN 'BLK' /* For å tvinge Combo til å virke. Virer ikke med Blank */ 
                                            ELSE TRIM(ArtBas.JamforEnhet)  
        ArtBas.InkrAnbrekk:SENSITIVE    = ArtBas.Anbrekk:CHECKED.

    initAlternativLev().
    IF iMellankategori = 0 THEN
        initUnderkategori().
    ELSE IF iMellankategori = 1 THEN
        initMellankategori().
    initKarakteristikk().

    DISPLAY
      ArtBas.AnbefaltPris ArtBas.KjedeRab% ArtBas.KjedeInnkPris ArtBas.forhRab% ArtBas.supRab% ArtBas.KatalogPris
      ArtBas.VPIBildeKode ArtBas.VPIDato ArtBas.LinjeMerknad ArtBas.VareFakta ArtBas.LevDato1
      ArtBas.LevDato2 ArtBas.LevDato3 ArtBas.LevDato4 ArtBas.KjedeValutaPris ArtBas.KjedeProdusent
      ArtBas.LevDatoStopp1 ArtBas.LevDatoStopp2 ArtBas.LevDatoStopp3 ArtBas.LevDatoStopp4
      ArtBas.Sortimentkoder ArtBas.Lagerkoder ArtBas.Kampanjeuker ArtBas.Kampanjestotte ArtBas.EkstStrTypeNavn   
      ArtBas.TilgjengeligFraLev ArtBas.PostBredde ArtBas.PostLengde ArtBas.PostHoyde ArtBas.PostVekt ArtBas.AlfaKode2
    WITH FRAME FRAME-Info2.
    ASSIGN FI-Land:SCREEN-VALUE IN FRAM FRAME-Info2 = IF AVAILABLE NumLandKode THEN NumLandKode.Land ELSE ''.

    FIND FIRST ArtPris OF ArtBas NO-LOCK NO-ERROR.
    IF AVAILABLE ArtPris THEN
      FI-JamforPris:screen-value IN FRAME FRAME-ArtInfo = STRING(ROUND(ArtPris.Pris[1] / INPUT ArtBas.Mengde,2)).
    IF INPUT FI-JamforPris:screen-value IN FRAME FRAME-ArtInfo = ? then FI-JamforPris:SCREEN-VALUE IN FRAME FRAME-ArtInfo = ''.

    FIND bArtBas NO-LOCK WHERE
      bArtBas.ArtikkelNr = ArtBas.LinkVareNr NO-ERROR.
    IF AVAILABLE bArtBas THEN
    DO:
        /* Legger opp verdier I de aktuelle feltene */
        ASSIGN
          FI-LinkVareTekst:SCREEN-VALUE IN FRAME FRAME-ArtInfo = bArtBas.Beskr
          .
    END.
    ELSE DO:
        /* Legger opp verdier I de aktuelle feltene */
        ASSIGN
          FI-LinkVareTekst:SCREEN-VALUE IN FRAME FRAME-ArtInfo = ''
          .
    END.
    IF AVAILABLE Bilderegister THEN
        RUN VisBilde (1).
    ELSE
        RUN VisBilde (2).
  RUN EnaDis ("VisArtBas").
  RUN EnableLevInfo (T-EnableLevInfo:CHECKED).
           
  IF ArtBas.OPris:checked THEN ArtBas.Lager:Sensitive = FALSE.         
  IF ArtBas.NON_Sale:checked THEN ArtBas.Lager:Sensitive = FALSE.         
  
  FIND FIRST ArtLag NO-LOCK WHERE
    ArtLag.Artikkelnr = ArtBas.artikkelnr NO-ERROR.
  /* ArtLAg.ArtikkelNr = ArtBas.ArtikkelNr no-error. */
  RUN FillLevSortCB.
  RUN FillCBModellFarge.
  IF CB-ModellFarge:LIST-ITEM-PAIRS <> "," THEN
    ASSIGN CB-ModellFarge:SCREEN-VALUE IN FRAME DEFAULT-FRAME = STRING(ArtBas.ArtikkelNr).
  ELSE "".
  RUN InitKalkyle.
  RUN VisLager (NO).
  ASSIGN
      ArtBas.ValKod:SENSITIVE = FALSE
      BUTTON-SokValuta:HIDDEN = TRUE
      .
END. /* FRAME-ArtInfo */    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisBestilling C-ArtKort 
PROCEDURE VisBestilling :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF VALID-HANDLE(hBestilling) AND AVAIL artbas THEN
        RUN ByttObjekt IN hBestilling (artbas.artikkelnr) NO-ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisBilde C-ArtKort 
PROCEDURE VisBilde :
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
    &BildNr = "ArtBas.BildNr"
/*     &BildNr = "input ArtBas.BildNr" */
  }
END.
  MENU-ITEM m_Kopier:SENSITIVE IN MENU POPUP-MENU-BUTTON-Kopierbilde = ArtBas.Bildnr > 0.
  MENU-ITEM m_Lim_inn:SENSITIVE IN MENU POPUP-MENU-BUTTON-Kopierbilde = BUTTON-Kopierbilde:PRIVATE-DATA <> ? AND
                                          STRING(ArtBas.Bildnr) <> BUTTON-Kopierbilde:PRIVATE-DATA.
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
                               THEN STRING(ArtBas.RegistrertDato)
                               ELSE "        ") + " " +
                             (IF ArtBas.RegistrertTid <> 0
                               THEN STRING(ArtBas.RegistrertTid,"HH:MM:SS")
                               ELSE "        ") + " " + Tx("av",103) + " " + 
                             ArtBas.RegistrertAv + "    " + Tx("Endret",102) + " " +
                             (IF ArtBas.EDato <> ?
                               THEN STRING(ArtBas.EDato)
                               ELSE "        ") + " " +
                             (IF ArtBas.ETid <> 0
                               THEN STRING(ArtBas.ETid,"HH:MM:SS")
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisJmf C-ArtKort 
PROCEDURE VisJmf :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.
    DEFINE BUFFER modellArtBas FOR ArtBas.

    /*RUN NyArtBas IN hJmfRutine (ArtBas.ModellFarge).*/

    FOR EACH modellArtBas WHERE modellArtBas.ModellFarge = ArtBas.ModellFarge /*AND
                                modellArtBas.ArtikkelNr <> ArtBas.ModellFarge*/ NO-LOCK.
        RUN NyArtBas IN hJmfRutine (modellArtBas.ArtikkelNr).
        ASSIGN iCount = iCount + 1.
        IF iCount = 5 THEN  /* totalt 6 st */
            LEAVE.
    END.

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
  DEFINE VARIABLE rRowId AS ROWID      NO-UNDO.
  DEFINE VARIABLE iBredast AS INTEGER     NO-UNDO.
  /*
  if wVisLager <> wArtBasRecid or
     wOverStyr then
  */

  IF wOverstyr <> ? AND wVisLager = wArtBasRecid THEN RETURN.

    BYGG-LAGER:
    DO:
      RUN bygglager.w (INPUT wArtBasRecid, wRS-Vis, OUTPUT wStrListe, OUTPUT cBrukteStr).

      IF RETURN-VALUE = "AVBRYT" THEN
        LEAVE BYGG-LAGER.
      ASSIGN wVisLAger = wArtBasRecid.

      RUN BrowseLabel (wStrListe, OUTPUT iBredast). /* iBredast = bredaste kolumn width-pixel */
      IF dSkoModus THEN RUN AdjustLagerCol (INPUT iBredast).

      ASSIGN cLagerStrListe = wStrListe
             cMottakstr     = wStrListe.
    END. /* BYGG-LAGER */
  /* NYTT */
  TG-VisSkjul:CHECKED IN FRAME FRAME-Lager = FALSE.
  RUN HideShowLagerCols ("Skjul").
  
  DO WITH FRAME FRAME-Lager:
      ASSIGN B-Lagerjustering:SENSITIVE = wRS-Vis = 1 AND ArtBas.Lager = TRUE
             B-Nedskrivning:SENSITIVE   = B-Lagerjustering:SENSITIVE
             B-Svinn:SENSITIVE          = B-Lagerjustering:SENSITIVE
             B-ForenklVaremot:SENSITIVE = B-Lagerjustering:SENSITIVE.
  END.
  /* /NYTT */
  IF wRS-Vis = 1 THEN
    tmpLager.DivAntall:label IN BROWSE BROWSE-Lager = Tx("AntSolgt",131).
  ELSE
    tmpLager.DivAntall:label IN BROWSE BROWSE-Lager = Tx("AntLager",132).  
  IF NOT PROGRAM-NAME(2) BEGINS "USER-INTERFACE" AND iBrukerButikk > 0 THEN DO: /* Vid knapp Oppdater lagerbilde skall vi inte repos i browser */
      FIND tmpLager WHERE TRIM(tmpLager.Butik) = STRING(iBrukerButikk) NO-ERROR.
      IF AVAIL tmpLager THEN
          ASSIGN rRowId = ROWID(tmpLager).
  END.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-Lager}
/*   {&OPEN-QUERY-FRAME-Lager} */
  IF rRowId <> ? THEN
      REPOSITION BROWSE-Lager TO ROWID rRowId NO-ERROR.
/*   DEFINE VARIABLE hCol AS HANDLE      NO-UNDO. */
/*   hCol = BROWSE-Lager:FIRST-COLUMN.                                               */
/*                                                                                   */
/*     DO WHILE VALID-HANDLE(hCol):                                                  */
/* /*       hCol:AUTO-RESIZE = FALSE. */                                             */
/*       hCol:WIDTH-PIXELS = FONT-TABLE:GET-TEXT-WIDTH-PIXELS(hCol:LABEL,hCol:FONT). */
/* /*       hCol:AUTO-RESIZE = TRUE. */                                              */
/*       hCol = hCol:NEXT-COLUMN.                                                    */
/*    END.                                                                           */

  PROCESS EVENTS.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisStrlType C-ArtKort 
PROCEDURE VisStrlType :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   FIND StrType NO-LOCK WHERE
        StrType.StrTypeID = ArtBas.StrTypeId NO-ERROR.
   IF AVAIL StrType THEN
       DISPLAY ArtBas.StrTypeID
               StrType.Beskrivelse
             WITH FRAME FRAME-ArtInfo.  
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
  RUN w-barttranslogg.w PERSISTENT SET tmpChild.wChild (ArtBas.ArtikkelNr).

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AlleEANok C-ArtKort 
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
      FIND StrKonv WHERE StrKonv.Storl = ENTRY(iCount,cStorrelser) NO-LOCK NO-ERROR.
      IF AVAIL StrKonv THEN DO:
          FIND FIRST StrekKode OF etiArtBas WHERE StrekKode.KodeType = 1 AND 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getActivArtNr C-ArtKort 
FUNCTION getActivArtNr RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  RETURN lArtikkelNr.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getArtBut C-ArtKort 
FUNCTION getArtBut RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cBut AS CHARACTER   NO-UNDO.
  IF ArtBas.WebButikkArtikkel THEN DO:
      FOR EACH ArtBut WHERE ArtBut.artikkelnr = artbas.artikkelnr AND artbut.deleted = FALSE NO-LOCK:
          cBut = cBut + (IF cBut <> "" THEN "," ELSE "") + STRING(ArtBut.butik).
      END.
      RETURN cBut.
  END.
  ELSE
      RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getArtNr C-ArtKort 
FUNCTION getArtNr RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN dLevArtikkelnr.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getPakkeNr C-ArtKort 
FUNCTION getPakkeNr RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iPakkenr     AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iHentetValue AS INTEGER    NO-UNDO.
  ASSIGN iPakkeNr     = NEXT-VALUE(Pakkenr)
         iHentetValue = iPakkeNr.
  IF CAN-FIND(ArtBas WHERE ArtBas.PakkeNr = iPakkeNr) THEN FINNLEDIGT: DO:
      DO WHILE iPakkeNr < 9999:
          ASSIGN iPakkeNr = iPakkeNr + 1.
          IF NOT CAN-FIND(ArtBas WHERE ArtBas.PakkeNr = iPakkeNr) THEN DO:
              CURRENT-VALUE(Pakkenr) = iPakkeNr.
              LEAVE FINNLEDIGT.
          END.
      END.
      ASSIGN iPakkeNr = 0.
      DO WHILE iPakkeNr < iHentetValue:
          ASSIGN iPakkeNr = iPakkeNr + 1.
          IF NOT CAN-FIND(ArtBas WHERE ArtBas.PakkeNr = iPakkeNr) THEN
              LEAVE FINNLEDIGT.
      END.
      ASSIGN iPakkeNr = ?.
  END.

  RETURN iPakkenr.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getStorl C-ArtKort 
FUNCTION getStorl RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN cLevStorrelser.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION initAlternativLev C-ArtKort 
FUNCTION initAlternativLev RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEF VAR piLoop  AS INT  NO-UNDO.
    DEF VAR pcTekst AS CHAR NO-UNDO.

    DEF BUFFER bLevBas FOR LevBas.

    IF NOT AVAILABLE ArtBas THEN
        RETURN FALSE.

    DO WITH FRAME FRAME-ArtInfo:
        pcTekst = "".
        FOR EACH AltLevBas NO-LOCK WHERE
            AltLevBas.ArtikkelNr = ArtBas.ArtikkelNr:

            FIND bLevBas NO-LOCK WHERE
                bLevBas.LevNr = AltLevBas.LevNr NO-ERROR.
            IF AVAILABLE bLevBas THEN
                pcTekst = pcTekst + 
                          (IF pcTekst = ""
                             THEN ""
                             ELSE ",") + 
                          STRING(bLevBas.LevNr) + " / " + bLevBas.LevNamn + "," + STRING(bLevBas.LevNr).
        END.
        IF pcTekst <> "" THEN
            ASSIGN
            S-AlternativLev:LIST-ITEM-PAIRS = pcTekst
            S-AlternativLev:SCREEN-VALUE    = ENTRY(2,pcTekst)
            .
        ELSE 
            ASSIGN
            S-AlternativLev:LIST-ITEM-PAIRS = ","
            S-AlternativLev:SCREEN-VALUE    = ""
            .
    END.

    RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION initKarakteristikk C-ArtKort 
FUNCTION initKarakteristikk RETURNS LOGICAL
        (  ):
        /*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
    DEF VAR piLoop  AS INT  NO-UNDO.
    DEF VAR pcTekst AS CHAR NO-UNDO.

    DEF BUFFER bKarakteristikk FOR Karakteristikk.

    IF NOT AVAILABLE ArtBas THEN
        RETURN FALSE.

    DO WITH FRAME FRAME-Info2:
        pcTekst = "".
        FOR EACH ArtBasKarakteristikk NO-LOCK WHERE
            ArtBasKarakteristikk.ArtikkelNr = ArtBas.ArtikkelNr:

            FIND bKarakteristikk NO-LOCK WHERE
                bKarakteristikk.KarakteristikkId = ArtBasKarakteristikk.KarakteristikkId NO-ERROR.
            IF AVAILABLE bKarakteristikk THEN
                pcTekst = pcTekst + 
                          (IF pcTekst = ""
                             THEN ""
                             ELSE ",") + 
                          STRING(bKarakteristikk.KarakteristikkId) + " / " + bKarakteristikk.KBeskrivelse + "," + STRING(bKarakteristikk.KarakteristikkId).
        END.
        
        IF pcTekst <> "" THEN
            ASSIGN
            S-Karakteristikk:LIST-ITEM-PAIRS = pcTekst
            S-Karakteristikk:SCREEN-VALUE    = ENTRY(2,pcTekst)
            .
        ELSE 
            ASSIGN
            S-Karakteristikk:LIST-ITEM-PAIRS = ","
            S-Karakteristikk:SCREEN-VALUE    = ""
            .
        
    END.

    RETURN TRUE.   /* Function return value. */ 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION initMellankategori C-ArtKort 
FUNCTION initMellankategori RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cLIPairM AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE iInitMellan AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iUnderkat AS INTEGER     NO-UNDO.
  cLIPairM = ",0".
  FOR EACH Mellankategori NO-LOCK WHERE Mellankategori.mkatid > 0 AND CAN-FIND(FIRST MellanUkat WHERE MellanUkat.mkatid = Mellankategori.mkatid):
      cLIPairM = cLIPairM + "," + Mellankategori.mkatbeskr + "," + STRING(Mellankategori.mkatid).
  END.
  IF AVAIL artbas THEN DO:
      IF CAN-FIND(FIRST ArtBasMUkategori OF artbas) THEN DO:
          FIND FIRST ArtBasMUkategori OF artbas NO-LOCK.
          FIND MellanUkat WHERE MellanUkat.mukatnr = ArtBasMUkategori.mukatnr NO-LOCK NO-ERROR.
          IF AVAIL MellanUkat THEN DO:
              iInitMellan = MellanUkat.mkatid.
              iUnderkat   = MellanUkat.UnderKatNr.
/*                   MellanUkat.mukatnr    */
/*                                         */
/*                   MellanUkat.UnderKatNr */
          END.
      END.
  END.
  DO WITH FRAME FRAME-ArtInfo:
      CB-Mellankategori:LIST-ITEM-PAIRS = cLIPairM.
      CB-Mellankategori:SCREEN-VALUE    = STRING(iInitMellan).
/*       APPLY "VALUE-CHANGED" TO CB-Mellankategori. */
      initMellanUkat(iInitMellan,iUnderkat).
  END.
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION initMellanUkat C-ArtKort 
FUNCTION initMellanUkat RETURNS LOGICAL
  ( INPUT iMkatid AS INTE, INPUT iUnderkat AS INTE) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cLIPairU AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE iInitUnder  AS INTEGER     NO-UNDO.
  DEFINE VARIABLE lEntry AS LOGICAL     NO-UNDO.
  IF iMkatid = 0 THEN
      cLIPairU = ",0".
  ELSE FOR EACH MellanUkat WHERE MellanUkat.mkatid = iMkatid NO-LOCK:
      FIND Underkategori WHERE underkategori.UnderKatNr = MellanUkat.UnderKatNr NO-LOCK NO-ERROR.
      IF AVAIL MellanUkat THEN DO:
          /* Enklare om vi använder text från Underkategori och mukatnr för det är det vi kopplar till artbas */
          cLIPairU = cLIPairU + (IF cLIPairU = "" THEN "" ELSE ",") + Underkategori.UnderKatTekst + "," + STRING(Underkategori.UnderKatNr).
/* /*           cLIPairU = cLIPairU + "," + Underkategori.UnderKatTekst + "," + STRING(Underkategori.UnderKatNr). */ */
      END.
  END.
  DO WITH FRAME FRAME-ArtInfo:

     IF iMkatid > 0 AND iUnderkat = 0 THEN
         ASSIGN iUnderkat = INT(ENTRY(2,cLIPairU))
                lEntry = TRUE. /* Här har vi bytt mellankategori och skall hoppa till under */

      CB-MUkat:LIST-ITEM-PAIRS = cLIPairU.
      CB-MUkat:SCREEN-VALUE = STRING(iUnderkat).
/*       IF lEntry THEN                 */
/*           APPLY "ENTRY" TO CB-MUkat. */
  END.

  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION initUnderkategori C-ArtKort 
FUNCTION initUnderkategori RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEF VAR piLoop  AS INT  NO-UNDO.
    DEF VAR pcTekst AS CHAR NO-UNDO.

    DEF BUFFER bUnderkategori FOR Underkategori.
    IF AVAILABLE ArtBas AND wKopi = FALSE THEN
    DO WITH FRAME FRAME-ArtInfo:
        pcTekst = "".
        FOR EACH ArtBasUnderKategori NO-LOCK WHERE
            ArtBasUnderKategori.ArtikkelNr = ArtBas.ArtikkelNr:

            FIND bUnderkategori NO-LOCK WHERE
                bUnderkategori.UnderKatNr = ArtBasUnderKategori.UnderKatNr NO-ERROR.
            IF AVAILABLE bUnderkategori THEN
                pcTekst = pcTekst + 
                          (IF pcTekst = ""
                             THEN ""
                             ELSE ",") + 
                          STRING(bUnderkategori.UnderKatNr) + " / " + bUnderkategori.UnderkatTekst + "," + STRING(bUnderkategori.UnderKatNr).
        END.
        
        IF pcTekst <> "" THEN
            ASSIGN
            S-Underkategori:LIST-ITEM-PAIRS = pcTekst
            S-Underkategori:SCREEN-VALUE    = ENTRY(2,pcTekst)
            .
        ELSE 
            ASSIGN
            S-Underkategori:LIST-ITEM-PAIRS = ","
            S-Underkategori:SCREEN-VALUE    = ""
            .
        
            RETURN TRUE.   /* Function return value. */
    END.
    ELSE DO WITH FRAME FRAME-ArtInfo:
            ASSIGN
            S-Underkategori:LIST-ITEM-PAIRS = ","
            S-Underkategori:SCREEN-VALUE    = ""
            .
        RETURN FALSE.
    END.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION lagerButikk C-ArtKort 
FUNCTION lagerButikk RETURNS LOGICAL
  ( OUTPUT cStrListe AS CHARACTER, OUTPUT cLagerAnt AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iButTst AS INTEGER   NO-UNDO.
  DEFINE VARIABLE iCount  AS INTEGER   NO-UNDO.
  
  ASSIGN iButTst = INT(tmplager.Butik) NO-ERROR.
  
  IF NOT ERROR-STATUS:ERROR AND
      CAN-DO(cTillgButikker,STRING(iButTst)) AND CAN-FIND(Butiker WHERE Butiker.Butik = iButTst) THEN DO:
      ASSIGN cStrListe = ""
             cLagerAnt = "".
      DO iCount = 1 TO NUM-ENTRIES(cLagerStrListe,";") - 1:
          ASSIGN cStrListe = cStrListe + (IF cStrListe <> "" THEN "," ELSE "") + TRIM(ENTRY(iCount,cLagerStrListe,";"))
                 cLagerAnt = cLagerAnt + (IF cLagerAnt <> "" THEN " " ELSE "") + STRING(INT((tmpLager.Ant[iCount]))).
      END.
/*       ASSIGN cLagerAnt = REPLACE(cLagerAnt,","," "). */
      RETURN TRUE.
  END.
  ELSE
       RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ModellBytte C-ArtKort 
FUNCTION ModellBytte RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DO WITH FRAME Frame-ArtInfo:
    ASSIGN
      cORG    = STRING(ArtBas.ArtikkelNr) + '|' +
                string(ArtBas.LevNr) + '|' + 
                string(ArtBas.Sasong) + '|' + 
                string(ArtBas.Vg) + '|' + 
                string(ArtBas.VmId) + '|' + 
                string(ArtBas.ProdNr) + '|' + 
                string(ArtBas.RAvdNr) + '|' + 
                string(ArtBas.WebButikkArtikkel) + '|' + 
                string(ArtBas.VPIBildeKode)
      cEndret = ArtBas.ArtikkelNr:Screen-value IN FRAME Default-Frame + '|' + 
                ArtBas.LevNr:screen-value IN FRAME Default-Frame + '|' + 
                ArtBas.Sasong:Screen-value + '|' + 
                ArtBas.Vg:screen-value IN FRAME Default-Frame + '|' + 
                ArtBas.VmId:screen-value + '|' + 
                ArtBas.ProdNr:screen-value + '|' + 
                ArtBas.RAvdNr:screen-value + '|' +
                ArtBas.WebButikkArtikkel:screen-value + '|' + 
                ArtBas.VPIBildeKode:screen-value IN FRAME Frame-Info2.
  END.

  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION NyOpris C-ArtKort 
FUNCTION NyOpris RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN chTabStrip:ENABLED = FALSE AND ArtBas.Opris = TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION PostFelt C-ArtKort 
FUNCTION PostFelt RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  DO WITH FRAME FRAME-Info2:
    IF (dec(ArtBas.PostBredde:screen-value) + dec(ArtBas.PostHoyde:screen-value) + dec(ArtBas.PostLengde:screen-value) + dec(ArtBas.PostVekt:screen-value) > 0) THEN
    DO:
      IF dec(ArtBas.PostBredde:screen-value) = 0 OR dec(ArtBas.PostHoyde:screen-value) = 0 OR dec(ArtBas.PostLengde:screen-value) = 0 OR dec(ArtBas.PostVekt:screen-value) = 0 THEN
      DO:
        MESSAGE "Hvis noen av postforsendelsesfeltene er utfyllt, må alle fylles ut." VIEW-AS ALERT-BOX.
        RETURN FALSE.
      END.
      ELSE RETURN TRUE.
    END.
    ELSE RETURN TRUE.
  END. 
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setAlternativLev C-ArtKort 
FUNCTION setAlternativLev RETURNS LOGICAL
  ( INPUT ihFillIn    AS HANDLE,
    INPUT icRowidList AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR piLoop  AS INT  NO-UNDO.
DEF VAR pcTekst AS CHAR NO-UNDO.

DEF BUFFER bLevBas FOR LevBas.

pcTekst = "".
IF icRowidList NE "" THEN 
DO piLoop = 1 TO NUM-ENTRIES(icRowidList):
    FIND bLevBas NO-LOCK WHERE
        ROWID(bLevBas) = TO-ROWID(ENTRY(piLoop,icRowidList)) NO-ERROR.
    IF AVAILABLE bLevBas THEN
        pcTekst = pcTekst + 
                  (IF pcTekst = ""
                     THEN ""
                     ELSE ",") + 
                  STRING(bLevBas.LevNr) + " / " + bLevBas.LevNamn + "," + STRING(bLevBas.LevNr).
END.
IF pcTekst <> "" THEN
    ASSIGN
    ihFillIn:LIST-ITEM-PAIRS = pcTekst
    ihFillIn:SCREEN-VALUE    = (ENTRY(2,ihFillIn:LIST-ITEM-PAIRS))
    .
ELSE 
    ASSIGN
    ihFillIn:LIST-ITEM-PAIRS = ","
    ihFillIn:SCREEN-VALUE    = ""
    .

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setKarakteristikk C-ArtKort 
FUNCTION setKarakteristikk RETURNS LOGICAL
  ( INPUT ihFillIn    AS HANDLE,
    INPUT icRowidList AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR piLoop  AS INT  NO-UNDO.
DEF VAR pcTekst AS CHAR NO-UNDO.

DEF BUFFER bKarakteristikk FOR Karakteristikk.

pcTekst = "".
IF icRowidList NE "" THEN 
DO piLoop = 1 TO NUM-ENTRIES(icRowidList):
    FIND bKarakteristikk NO-LOCK WHERE
        ROWID(bKarakteristikk) = TO-ROWID(ENTRY(piLoop,icRowidList)) NO-ERROR.
    IF AVAILABLE bKarakteristikk THEN
        pcTekst = pcTekst + 
                  (IF pcTekst = ""
                     THEN ""
                     ELSE ",") + 
                  STRING(bKarakteristikk.KarakteristikkId) + " / " + bKarakteristikk.KBeskrivelse + "," + STRING(bKarakteristikk.KarakteristikkId).
END.

IF pcTekst <> "" THEN
    ASSIGN
    ihFillIn:LIST-ITEM-PAIRS = pcTekst
    ihFillIn:SCREEN-VALUE    = (ENTRY(2,ihFillIn:LIST-ITEM-PAIRS))
    .
ELSE 
    ASSIGN
    ihFillIn:LIST-ITEM-PAIRS = ","
    ihFillIn:SCREEN-VALUE    = ""
    .

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION settArtSlag C-ArtKort 
FUNCTION settArtSlag RETURNS INTEGER
  ( INPUT iArtSlag AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  CASE iArtSLag:
      /* Stykkvare (Stk)       */ WHEN  0 THEN iArtSLag = 0.
      /* Vektvare (Kg)         */ WHEN  1 THEN iArtSLag = 1.
      /* Vektvare (Hg)         */ WHEN  2 THEN iArtSLag = 2.
      /* Metervare (m)         */ WHEN  3 THEN iArtSLag = 3.
      /* Kvadratmetervare (m2) */ WHEN  4 THEN iArtSLag = 4.
      /* Volumvare (l)         */ WHEN  5 THEN iArtSLag = 5.
      /* Pakkevare (Stk)       */ WHEN  7 THEN iArtSLag = 0.
      /* Pant (Stk)            */ WHEN  8 THEN iArtSLag = 0.
      OTHERWISE iArtSlag = 0.  /* Stk er default. */
  END CASE.


  RETURN iArtSlag.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setUnderkategori C-ArtKort 
FUNCTION setUnderkategori RETURNS LOGICAL
  ( INPUT ihFillIn    AS HANDLE,
    INPUT icRowidList AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR piLoop  AS INT  NO-UNDO.
DEF VAR pcTekst AS CHAR NO-UNDO.

DEF BUFFER bUnderkategori FOR Underkategori.

pcTekst = "".
IF icRowidList NE "" THEN 
DO piLoop = 1 TO NUM-ENTRIES(icRowidList):
    FIND bUnderkategori NO-LOCK WHERE
        ROWID(bUnderkategori) = TO-ROWID(ENTRY(piLoop,icRowidList)) NO-ERROR.
    IF AVAILABLE bUnderkategori THEN
        pcTekst = pcTekst + 
                  (IF pcTekst = ""
                     THEN ""
                     ELSE ",") + 
                  STRING(bUnderkategori.UnderKatNr) + " / " + bUnderkategori.UnderkatTekst + "," + STRING(bUnderkategori.UnderKatNr).
END.

IF pcTekst <> "" THEN
    ASSIGN
    ihFillIn:LIST-ITEM-PAIRS = pcTekst
    ihFillIn:SCREEN-VALUE    = (ENTRY(2,ihFillIn:LIST-ITEM-PAIRS))
    .
ELSE 
    ASSIGN
    ihFillIn:LIST-ITEM-PAIRS = ","
    ihFillIn:SCREEN-VALUE    = ""
    .

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION StorlToEAN C-ArtKort 
FUNCTION StorlToEAN RETURNS LOGICAL
  ( INPUT-OUTPUT cEtiketterStorl AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.
  DEFINE VARIABLE lFunnet AS LOGICAL    NO-UNDO.
  DO iCount = 1 TO NUM-ENTRIES(cEtiketterStorl):
      FIND StrKonv WHERE StrKonv.Storl = ENTRY(iCount,cEtiketterStorl) NO-LOCK NO-ERROR.
      IF AVAIL StrKonv THEN DO:
          FIND FIRST StrekKode OF etiArtBas WHERE StrekKode.KodeType = 1 AND 
                                               StrekKode.StrKode = StrKonv.StrKode AND
                                               NOT StrekKode.Kode BEGINS "02" NO-LOCK NO-ERROR.
          IF NOT AVAIL StrekKode THEN
              FIND FIRST StrekKode OF etiArtBas WHERE StrekKode.KodeType = 1 AND 
                                                   StrekKode.StrKode = StrKonv.StrKode NO-LOCK NO-ERROR.
          ASSIGN ENTRY(iCount,cEtiketterStorl) = IF AVAIL StrekKode THEN StrekKode.Kode ELSE "".
          IF AVAIL StrekKode THEN
              ASSIGN lFunnet = TRUE.
      END.
  END.
  RETURN lFunnet.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

