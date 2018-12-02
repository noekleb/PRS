&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME C-KundKort


/* Temp-Table and Buffer definitions                                    */
DEFINE NEW SHARED TEMP-TABLE tStLinje NO-UNDO LIKE StLinje.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-KundKort 
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
  def var wInputRecid as recid no-undo.
  def var wModus       as char  no-undo. /* NY, ENDRE, SLETT */
  assign 
    wModus = "ENDRE". /* Default */
 find first Medlem no-lock no-error.
  if available Medlem then
    assign wInputRecid = recid(Medlem).
&ELSE
  def input parameter wInputRecid as recid no-undo.
  def input parameter wModus       as char  no-undo. /* NY, ENDRE, SLETT */
&ENDIF

/* Oppslag mot SPAR */
DEFINE VARIABLE cConnectString AS CHARACTER   NO-UNDO.
DEFINE VARIABLE hServer        AS HANDLE NO-UNDO. 
DEFINE VARIABLE lConnected     AS LOGICAL NO-UNDO. 
DEF VAR cMedlemsNr   AS CHAR FORMAT "x(20)" NO-UNDO.
DEF VAR cMedlemskort AS CHAR FORMAT "x(20)" NO-UNDO.
DEF VAR cSkrivRader  AS CHAR FORMAT "x(20)" NO-UNDO.
DEF VAR cMedlemsNamn AS CHAR FORMAT "x(40)" NO-UNDO.
DEF VAR cStatus      AS CHAR FORMAT "x(20)" NO-UNDO.
DEF VAR lOK          AS LOG                 NO-UNDO.
DEF VAR cMelding     AS CHAR FORMAT "x(40)" NO-UNDO.
DEF VAR lMedlemsNr AS DEC NO-UNDO.

/* Local Variable Definitions ---                                       */
def var wOk                as log    no-undo.
DEF VAR wSvar              AS LOG FORMAT "Ja/Nei" NO-UNDO.
def var wOldRecid          as recid  no-undo.
def var hHandle            as handle no-undo.
def var hLabel             as handle no-undo.
def var wSubWin1           as handle no-undo.
def var wSubWin2           as handle no-undo.
def var wSubWin3           as handle no-undo.
def var wLapTop            as log    no-undo.
def var wBekreft           as log    no-undo.
def var wFeil              as log    no-undo.
DEF VAR wKortNummer        AS CHAR   NO-UNDO.
DEF VAR wGyldighet         AS INT    NO-UNDO.
DEF VAR wLedige            AS INT    NO-UNDO.
DEF VAR wDbId              AS char   NO-UNDO.
DEF VAR ipMedlemsNr        AS DEC    NO-UNDO.
DEF VAR cTekst             AS CHAR NO-UNDO.
DEF VAR cValgListe         AS CHAR   NO-UNDO.
DEF VAR wCl                AS INT    NO-UNDO.
DEF VAR cManuellt          AS CHAR NO-UNDO.  /* manuell inmatning av medlemskortnr om = "1" */
DEF VAR woldKundeNr        AS DEC    NO-UNDO.
def var hParentHandle      as handle no-undo.
DEFINE VARIABLE cMobilFormat AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iMobilTkn    AS INTEGER     NO-UNDO.
/* Variabler for håndtering av Tab-Stip. */
DEFINE VAR chTabs            AS COM-HANDLE NO-UNDO.
DEFINE VAR chTab             AS COM-HANDLE NO-UNDO.
define var chTab1Side        as com-handle no-undo.
DEFINE VAR wAktivFlip        AS INT        INIT 1 NO-UNDO. /* !! parameter !! */
DEFINE VAR wAktivFrame       AS INTE INIT  1.
DEFINE VAR wTabTekst         AS CHAR 
    INIT "Vedlikehold,Transaksjoner,Egne + koblede medlemmer" NO-UNDO.
DEFINE VAR wTabHnd           AS HANDLE EXTENT 3 NO-UNDO.
DEFINE VAR wBrowseHandle     AS HANDLE NO-UNDO.
DEF VAR cPersonNr AS CHAR NO-UNDO.

/* SPAR oppslag */
DEFINE VARIABLE Persondetaljer_Fornamn AS CHAR NO-UNDO. 
DEFINE VARIABLE Persondetaljer_Efternamn AS CHAR NO-UNDO. 
DEFINE VARIABLE Persondetaljer_Fodelsetid AS CHAR NO-UNDO. 
DEFINE VARIABLE Persondetaljer_Kon AS CHAR NO-UNDO. 
DEFINE VARIABLE Folkbokforingsadress_Utdelningsadress2 AS CHAR NO-UNDO. 
DEFINE VARIABLE Folkbokforingsadress_PostNr AS CHAR NO-UNDO. 
DEFINE VARIABLE Folkbokforingsadress_Postort AS CHAR NO-UNDO. 
DEFINE VARIABLE Folkbokforingsadress_FolkbokfordLanKod AS CHAR NO-UNDO. 
DEFINE VARIABLE Folkbokforingsadress_FolkbokfordKommunKod AS CHAR NO-UNDO. 
DEFINE VARIABLE Folkbokforingsadress_FolkbokfordForsamlingKod AS CHAR NO-UNDO. 
DEFINE VARIABLE cErrorMessage AS CHAR NO-UNDO. 
DEFINE VARIABLE lPersonFunnet AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lSuccess AS LOGICAL NO-UNDO. 

/* Buffere */
def buffer bMedlem for Medlem.
DEF BUFFER clButiker FOR Butiker.
DEF BUFFER ledMedlem FOR Medlem.
def temp-table tmpChild 
  field wChild as handle.
{runlib.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-27 RECT-28 B-GDPR B-Konv CB-Sort ~
B-Overfor B-OppdatSaldo B-AltS BUTTON-Angre BUTTON-Lagre BUTTON-Next ~
BUTTON-Ny BUTTON-Prev BUTTON-Slett Btn_Help BUTTON-Ok 
&Scoped-Define DISPLAYED-OBJECTS CB-Sort FILL-IN-MedlemsNr FILL-IN-Navn ~
FI-TotKjop FI-Kunde 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Modifierad C-KundKort 
FUNCTION Modifierad RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-KundKort AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE SUB-MENU m_Fil 
       MENU-ITEM m_Avslutt      LABEL "&Avslutt"       ACCELERATOR "ALT-F4".

DEFINE MENU MENU-BAR-C-ArtKort MENUBAR
       SUB-MENU  m_Fil          LABEL "&Fil"          .


/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE TabStrip AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chTabStrip AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-AltS 
     IMAGE-UP FILE "icon/select.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Søk" 
     SIZE 4.6 BY 1.1 TOOLTIP "Starter Alt-S søkefunksjonen".

DEFINE BUTTON B-GDPR 
     LABEL "GDPR" 
     SIZE 26.4 BY 1.

DEFINE BUTTON B-Konv 
     LABEL "Konverter medlemsnr" 
     SIZE 33 BY 1.

DEFINE BUTTON B-OppdatSaldo 
     LABEL "Oppdater saldo" 
     SIZE 20 BY 1.

DEFINE BUTTON B-Overfor 
     LABEL "Overfør til medlem..." 
     SIZE 27 BY 1.

DEFINE BUTTON Btn_Help DEFAULT 
     IMAGE-UP FILE "icon\e-help":U NO-FOCUS FLAT-BUTTON
     LABEL "&Help" 
     SIZE 4.6 BY 1.05
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-Angre 
     IMAGE-UP FILE "icon\e-undo":U NO-FOCUS FLAT-BUTTON
     LABEL "&Ångra" 
     SIZE 4.6 BY 1.05 TOOLTIP "Angre (Alt-A)".

DEFINE BUTTON BUTTON-Kopier 
     IMAGE-UP FILE "icon\e-copy":U NO-FOCUS FLAT-BUTTON
     LABEL "&Kopiera" 
     SIZE 4.6 BY 1.05 TOOLTIP "Kopier post  (Alt-K)".

DEFINE BUTTON BUTTON-Lagre 
     IMAGE-UP FILE "icon\e-save":U NO-FOCUS FLAT-BUTTON
     LABEL "&Lagra" 
     SIZE 4.6 BY 1.05 TOOLTIP "Lagre post (Alt-L)".

DEFINE BUTTON BUTTON-Next 
     IMAGE-UP FILE "icon\e-pilned":U NO-FOCUS FLAT-BUTTON
     LABEL "Neste" 
     SIZE 4.6 BY 1.05 TOOLTIP "Neste post (Alt-PilNed)".

DEFINE BUTTON BUTTON-Ny 
     IMAGE-UP FILE "icon\e-ny":U NO-FOCUS FLAT-BUTTON
     LABEL "&Ny" 
     SIZE 4.6 BY 1.05 TOOLTIP "Ny post (Alt-N)".

DEFINE BUTTON BUTTON-Ok 
     IMAGE-UP FILE "icon\e-exit":U NO-FOCUS FLAT-BUTTON
     LABEL "&Ok" 
     SIZE 4.6 BY 1.05 TOOLTIP "Lagre og avslutt".

DEFINE BUTTON BUTTON-Prev 
     IMAGE-UP FILE "icon\e-pilopp":U NO-FOCUS FLAT-BUTTON
     LABEL "Forrige" 
     SIZE 4.6 BY 1.05 TOOLTIP "Forrige post (Alt-PilOpp)".

DEFINE BUTTON BUTTON-Slett 
     IMAGE-UP FILE "icon\e-del":U NO-FOCUS FLAT-BUTTON
     LABEL "Radera" 
     SIZE 4.6 BY 1.05 TOOLTIP "Slett post (Alt-D)".

DEFINE BUTTON BUTTON-SokMedlem2 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1 TOOLTIP "Søk i leverandørregister".

DEFINE VARIABLE CB-Sort AS CHARACTER FORMAT "X(256)":U INITIAL " 1: Vg/LpNr" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEMS " 1: Nummer"," 2: Etternavn"," 3: Fornavn" 
     DROP-DOWN-LIST
     SIZE 35 BY 1 TOOLTIP "Velg sorteringsordning som skal gjelde for blaing med pil Opp/Ned." NO-UNDO.

DEFINE VARIABLE FI-Kunde AS CHARACTER FORMAT "X(256)":U 
     LABEL "Kundekobling" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1 NO-UNDO.

DEFINE VARIABLE FI-TotKjop AS DECIMAL FORMAT "->>,>>>,>>9.99":U INITIAL 0 
     LABEL "Totalt kjøp" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-MedlemsNr AS DECIMAL FORMAT "->>>>>>>>>>>>9" INITIAL 0 
     LABEL "Medlem" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE FILL-IN-Navn AS CHARACTER FORMAT "x(200)" 
     VIEW-AS FILL-IN 
     SIZE 58 BY 1.

DEFINE RECTANGLE RECT-27
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 206.2 BY .1.

DEFINE RECTANGLE RECT-28
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 206.4 BY .1.

DEFINE BUTTON B-Del  NO-FOCUS
     LABEL "Del" 
     SIZE 7 BY 1 TOOLTIP "Slett kundekobling".

DEFINE BUTTON B-SokButikk 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-SokHovedMedlem 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1 TOOLTIP "Søk i leverandørregister".

DEFINE BUTTON B-SokMedlemsGruppe 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-SokMedlemsType 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-SokPost 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-SokRegion 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-Spar 
     LABEL "Oppslag mot SPAR" 
     SIZE 24 BY 1.

DEFINE BUTTON BUTTON-SokeForsteKjop 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokeKort 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokeKunde 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokMedlem 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE FI-ForsteKjop AS DATE FORMAT "99/99/99":U 
     LABEL "Første gang kjøpt" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Hovedmedlem AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE FI-KundeTekst AS CHARACTER FORMAT "X(256)":U INITIAL "Medlemsinformasjon" 
      VIEW-AS TEXT 
     SIZE 38 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-MedlemsKort AS CHARACTER FORMAT "X(256)":U 
     LABEL "Medlemskort" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Poststed1 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE FI-SisteKjop AS DATE FORMAT "99/99/99":U 
     LABEL "Siste gang kjøpt" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-10 AS CHARACTER FORMAT "X(256)":U INITIAL "Kundekobling" 
      VIEW-AS TEXT 
     SIZE 34 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FILL-IN-11 AS CHARACTER FORMAT "X(256)":U INITIAL "Notat:" 
      VIEW-AS TEXT 
     SIZE 25 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FILL-IN-2 AS CHARACTER FORMAT "X(256)":U INITIAL "Kortinformasjon" 
      VIEW-AS TEXT 
     SIZE 25 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FILL-IN-3 AS CHARACTER FORMAT "X(256)":U INITIAL "Øvrig informasjon" 
      VIEW-AS TEXT 
     SIZE 25 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FILL-IN-EndretInfo AS CHARACTER FORMAT "X(256)":U INITIAL "Endret informasjon" 
      VIEW-AS TEXT 
     SIZE 77.8 BY .62 NO-UNDO.

DEFINE RECTANGLE RECT-46
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 202.8 BY 21.67.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BUTTON-Kopier AT ROW 1.33 COL 11.4 NO-TAB-STOP 
     B-GDPR AT ROW 1.29 COL 82.2
     B-Konv AT ROW 1.29 COL 113 WIDGET-ID 2
     CB-Sort AT ROW 1.33 COL 38.8 COLON-ALIGNED NO-LABEL
     B-Overfor AT ROW 1.33 COL 146.6
     B-OppdatSaldo AT ROW 1.33 COL 173.8
     FILL-IN-MedlemsNr AT ROW 3 COL 19.8 COLON-ALIGNED HELP
          "Kundenummer"
     FILL-IN-Navn AT ROW 3 COL 44.6 COLON-ALIGNED HELP
          "Kundens navn" NO-LABEL
     FI-TotKjop AT ROW 3 COL 116 COLON-ALIGNED
     FI-Kunde AT ROW 4 COL 116 COLON-ALIGNED
     B-AltS AT ROW 1.33 COL 25.6 NO-TAB-STOP 
     BUTTON-SokMedlem2 AT ROW 3 COL 42
     BUTTON-Angre AT ROW 1.33 COL 20.8 NO-TAB-STOP 
     BUTTON-Lagre AT ROW 1.33 COL 6.8 NO-TAB-STOP 
     BUTTON-Next AT ROW 1.33 COL 35.2 NO-TAB-STOP 
     BUTTON-Ny AT ROW 1.33 COL 2 NO-TAB-STOP 
     BUTTON-Prev AT ROW 1.33 COL 30.6 NO-TAB-STOP 
     BUTTON-Slett AT ROW 1.33 COL 16 NO-TAB-STOP 
     Btn_Help AT ROW 1.33 COL 197.2 NO-TAB-STOP 
     BUTTON-Ok AT ROW 1.33 COL 202.2 NO-TAB-STOP 
     RECT-27 AT ROW 1.1 COL 1.4
     RECT-28 AT ROW 2.43 COL 1.2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 206.6 BY 30.76.

DEFINE FRAME FRAME-2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 9.81
         SIZE 202 BY 21.43.

DEFINE FRAME FRAME-3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 202 BY 21.43.

DEFINE FRAME FRAME-1
     B-Del AT ROW 16.24 COL 146.6
     B-SokHovedMedlem AT ROW 6.33 COL 117.4
     B-Spar AT ROW 7.33 COL 42 WIDGET-ID 16
     FILL-IN-11 AT ROW 4.57 COL 154 COLON-ALIGNED NO-LABEL
     Medlem.Bonus_Forsendelse AT ROW 3.24 COL 122 COLON-ALIGNED
          LABEL "Rab.sjekk forsendelse"
          VIEW-AS COMBO-BOX INNER-LINES 25
          LIST-ITEM-PAIRS "Post",1,
                     "Post+eMail",2,
                     "eMail",3,
                     "SMS",4
          DROP-DOWN-LIST
          SIZE 30 BY 1 TOOLTIP "Hvordan rabattsjekker skal sendes til medlemmet"
     Medlem.Bonus_varsel AT ROW 4.29 COL 122 COLON-ALIGNED
          LABEL "Rab.sjekk varsling"
          VIEW-AS COMBO-BOX INNER-LINES 25
          LIST-ITEM-PAIRS "Ingen",1,
                     "eMail",2,
                     "SMS",3
          DROP-DOWN-LIST
          SIZE 30 BY 1 TOOLTIP "Hvordan varsle medlemmet om opptjent bonsu"
     Medlem.Aktiv AT ROW 2.43 COL 71
          VIEW-AS TOGGLE-BOX
          SIZE 19 BY .81
     Medlem.MottaeMailUtsendelser AT ROW 3.38 COL 71 WIDGET-ID 4
          VIEW-AS TOGGLE-BOX
          SIZE 29 BY .81 TOOLTIP "Medlemmet vil ta i mot eMail utsendelser"
     FI-Hovedmedlem AT ROW 6.33 COL 120 COLON-ALIGNED NO-LABEL
     Medlem.MedlemsNr AT ROW 2.43 COL 22 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20.2 BY 1
     Medlem.HovedMedlemFlagg AT ROW 2.43 COL 51
          VIEW-AS TOGGLE-BOX
          SIZE 19 BY .81
     Medlem.ForNavn AT ROW 4.33 COL 22 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 42 BY 1
     Medlem.EtterNavn AT ROW 5.33 COL 22 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 42 BY 1
     Medlem.FodselsDato AT ROW 6.29 COL 22 COLON-ALIGNED FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     Medlem.FodtAr AT ROW 6.29 COL 56 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7.6 BY 1
     FI-MedlemsKort AT ROW 14.19 COL 95 COLON-ALIGNED
     FI-ForsteKjop AT ROW 12.19 COL 95 COLON-ALIGNED
     FI-SisteKjop AT ROW 13.19 COL 95 COLON-ALIGNED
     Medlem.PersonNr AT ROW 7.33 COL 22 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     Medlem.Adresse1 AT ROW 8.33 COL 22 COLON-ALIGNED
          LABEL "Adresse1"
          VIEW-AS FILL-IN 
          SIZE 42 BY 1
     Medlem.Adresse2 AT ROW 9.33 COL 22 COLON-ALIGNED
          LABEL "Adresse2"
          VIEW-AS FILL-IN 
          SIZE 42 BY 1
     Medlem.PostNr AT ROW 10.33 COL 22 COLON-ALIGNED
          LABEL "PostNr/BydelsNr"
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     Medlem.BydelsNr AT ROW 10.33 COL 42 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     FI-Poststed1 AT ROW 10.33 COL 52 COLON-ALIGNED NO-LABEL
     Medlem.Land AT ROW 11.33 COL 22 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     Medlem.ePostAdresse AT ROW 12.33 COL 22 COLON-ALIGNED FORMAT "X(60)"
          VIEW-AS FILL-IN 
          SIZE 54 BY 1
     Medlem.Telefon AT ROW 13.33 COL 22 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     Medlem.MobilTlf AT ROW 14.33 COL 22 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     Medlem.Telefaks AT ROW 15.33 COL 22 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.8 ROW 9.38
         SIZE 204.2 BY 22.1.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME FRAME-1
     Medlem.AktivertFraWeb AT ROW 13.33 COL 58 COLON-ALIGNED
          LABEL "Aktivert (Web)"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     Medlem.WebBrukerId AT ROW 14.33 COL 58 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     Medlem.WebPassord AT ROW 15.33 COL 58 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     Medlem.MedlemInfo AT ROW 16.33 COL 22 COLON-ALIGNED HELP
          "Medlemsinformasjon."
          LABEL "Medlemsinformasjon"
          VIEW-AS FILL-IN 
          SIZE 54 BY 1
     B-SokMedlemsGruppe AT ROW 7.33 COL 104.8
     Medlem.Bonus_Berettiget AT ROW 2.33 COL 97
          LABEL "Opptjene rabatt"
          VIEW-AS TOGGLE-BOX
          SIZE 34 BY .81
     Medlem.Kjonn AT ROW 4.33 COL 95 COLON-ALIGNED
          LABEL "Kjønn (Mann/Kvinne)" FORMAT "M/K"
          VIEW-AS FILL-IN 
          SIZE 4 BY 1
     Medlem.ButikkNr AT ROW 5.33 COL 95 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     Medlem.HovedMedlemsNr AT ROW 6.33 COL 95 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20.2 BY 1
     Medlem.MedGruppe AT ROW 7.33 COL 95 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7.6 BY 1
     Medlem.MedType AT ROW 8.33 COL 95 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7.6 BY 1
     B-SokMedlemsType AT ROW 8.33 COL 104.8
     Medlem.RegKode AT ROW 9.33 COL 95 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     Medlem.Opphort AT ROW 10.33 COL 95 COLON-ALIGNED
          LABEL "Opphører"
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     Butiker.ButNamn AT ROW 5.33 COL 109.4 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 42.6 BY 1
     MedlemsGruppe.Beskrivelse AT ROW 7.33 COL 107 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 45 BY 1
     MedlemsType.Beskrivelse AT ROW 8.33 COL 107 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 45 BY 1
     Region.Navn AT ROW 9.33 COL 114.6 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 37.4 BY 1
     Medlem.KundeNr AT ROW 16.19 COL 95 COLON-ALIGNED
          VIEW-AS FILL-IN NATIVE 
          SIZE 20.2 BY 1
     Kunde.Navn AT ROW 16.24 COL 119.4 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN NATIVE 
          SIZE 25 BY 1
     Medlem.Kilde AT ROW 11.14 COL 129.6 COLON-ALIGNED
          VIEW-AS FILL-IN NATIVE 
          SIZE 22 BY 1
     Medlem.TilgKilde AT ROW 12.19 COL 129.8 COLON-ALIGNED
          VIEW-AS FILL-IN NATIVE 
          SIZE 22 BY 1
     B-SokRegion AT ROW 9.33 COL 112.4
     Medlem.EksterntMedlemsNr AT ROW 13.19 COL 129.8 COLON-ALIGNED
          VIEW-AS FILL-IN NATIVE 
          SIZE 22 BY 1
     Medlem.Rabatt AT ROW 14.19 COL 135.8 COLON-ALIGNED
          VIEW-AS FILL-IN NATIVE 
          SIZE 16 BY 1
     Medlem.MKlubbId AT ROW 3.24 COL 172 COLON-ALIGNED WIDGET-ID 6
          VIEW-AS COMBO-BOX INNER-LINES 25
          LIST-ITEM-PAIRS "0",1
          DROP-DOWN-LIST
          SIZE 30 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.8 ROW 9.38
         SIZE 204.2 BY 22.1.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME FRAME-1
     Medlem.MedlemNotat AT ROW 5.33 COL 155.6 NO-LABEL WIDGET-ID 10
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          SIZE 48 BY 11.86
     B-SokPost AT ROW 10.33 COL 39.6
     BUTTON-SokeForsteKjop AT ROW 12.19 COL 111
     FILL-IN-EndretInfo AT ROW 22 COL 3.2 NO-LABEL
     FI-KundeTekst AT ROW 3.62 COL 22 COLON-ALIGNED NO-LABEL
     FILL-IN-2 AT ROW 11.48 COL 95 COLON-ALIGNED NO-LABEL
     FILL-IN-3 AT ROW 1.71 COL 94 COLON-ALIGNED NO-LABEL
     BUTTON-SokeKort AT ROW 14.19 COL 121
     B-SokButikk AT ROW 5.33 COL 107.2
     FILL-IN-10 AT ROW 15.52 COL 95 COLON-ALIGNED NO-LABEL
     BUTTON-SokeKunde AT ROW 16.24 COL 117.2
     BUTTON-SokMedlem AT ROW 2.43 COL 44 WIDGET-ID 14
     RECT-46 AT ROW 1.24 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.8 ROW 9.38
         SIZE 204.2 BY 22.1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: tStLinje T "NEW SHARED" NO-UNDO skotex StLinje
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-KundKort ASSIGN
         HIDDEN             = YES
         TITLE              = "Medlemsregister"
         HEIGHT             = 30.76
         WIDTH              = 206.6
         MAX-HEIGHT         = 49.33
         MAX-WIDTH          = 384
         VIRTUAL-HEIGHT     = 49.33
         VIRTUAL-WIDTH      = 384
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
/* SETTINGS FOR WINDOW C-KundKort
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* REPARENT FRAME */
ASSIGN FRAME FRAME-1:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME FRAME-2:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME FRAME-3:FRAME = FRAME FRAME-2:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
ASSIGN 
       FRAME DEFAULT-FRAME:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON BUTTON-Kopier IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-SokMedlem2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Kunde IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-TotKjop IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-MedlemsNr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-Navn IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME FRAME-1
   Custom                                                               */
/* SETTINGS FOR FILL-IN Medlem.Adresse1 IN FRAME FRAME-1
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Medlem.Adresse2 IN FRAME FRAME-1
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Medlem.AktivertFraWeb IN FRAME FRAME-1
   EXP-LABEL                                                            */
/* SETTINGS FOR BUTTON B-SokHovedMedlem IN FRAME FRAME-1
   NO-ENABLE                                                            */
ASSIGN 
       B-SokHovedMedlem:HIDDEN IN FRAME FRAME-1           = TRUE.

/* SETTINGS FOR FILL-IN MedlemsType.Beskrivelse IN FRAME FRAME-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN MedlemsGruppe.Beskrivelse IN FRAME FRAME-1
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX Medlem.Bonus_Berettiget IN FRAME FRAME-1
   EXP-LABEL                                                            */
/* SETTINGS FOR COMBO-BOX Medlem.Bonus_Forsendelse IN FRAME FRAME-1
   EXP-LABEL                                                            */
/* SETTINGS FOR COMBO-BOX Medlem.Bonus_varsel IN FRAME FRAME-1
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Butiker.ButNamn IN FRAME FRAME-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Medlem.BydelsNr IN FRAME FRAME-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Medlem.ePostAdresse IN FRAME FRAME-1
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN FI-ForsteKjop IN FRAME FRAME-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Hovedmedlem IN FRAME FRAME-1
   NO-ENABLE                                                            */
ASSIGN 
       FI-Hovedmedlem:HIDDEN IN FRAME FRAME-1           = TRUE.

/* SETTINGS FOR FILL-IN FI-KundeTekst IN FRAME FRAME-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-MedlemsKort IN FRAME FRAME-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Poststed1 IN FRAME FRAME-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-SisteKjop IN FRAME FRAME-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-EndretInfo IN FRAME FRAME-1
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN Medlem.FodselsDato IN FRAME FRAME-1
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN Medlem.FodtAr IN FRAME FRAME-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Medlem.HovedMedlemsNr IN FRAME FRAME-1
   NO-ENABLE                                                            */
ASSIGN 
       Medlem.HovedMedlemsNr:HIDDEN IN FRAME FRAME-1           = TRUE.

/* SETTINGS FOR FILL-IN Medlem.Kjonn IN FRAME FRAME-1
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Medlem.KundeNr IN FRAME FRAME-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Medlem.MedlemInfo IN FRAME FRAME-1
   EXP-LABEL EXP-HELP                                                   */
ASSIGN 
       Medlem.MedlemNotat:RETURN-INSERTED IN FRAME FRAME-1  = TRUE.

/* SETTINGS FOR FILL-IN Medlem.MedlemsNr IN FRAME FRAME-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Region.Navn IN FRAME FRAME-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Kunde.Navn IN FRAME FRAME-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Medlem.Opphort IN FRAME FRAME-1
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Medlem.PostNr IN FRAME FRAME-1
   EXP-LABEL                                                            */
/* SETTINGS FOR FRAME FRAME-2
                                                                        */
/* SETTINGS FOR FRAME FRAME-3
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-KundKort)
THEN C-KundKort:HIDDEN = yes.

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
     _Query            is NOT OPENED
*/  /* FRAME FRAME-2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-3
/* Query rebuild information for FRAME FRAME-3
     _Query            is NOT OPENED
*/  /* FRAME FRAME-3 */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME TabStrip ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 8.14
       COLUMN          = 1
       HEIGHT          = 23.57
       WIDTH           = 206
       HIDDEN          = no
       SENSITIVE       = yes.
/* TabStrip OCXINFO:CREATE-CONTROL from: {1EFB6596-857C-11D1-B16A-00C0F0283628} type: TabStrip */
      TabStrip:MOVE-AFTER(FI-Kunde:HANDLE IN FRAME DEFAULT-FRAME).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-KundKort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-KundKort C-KundKort
ON END-ERROR OF C-KundKort /* Medlemsregister */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-KundKort C-KundKort
ON WINDOW-CLOSE OF C-KundKort /* Medlemsregister */
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


&Scoped-define SELF-NAME B-AltS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-AltS C-KundKort
ON CHOOSE OF B-AltS IN FRAME DEFAULT-FRAME /* Søk */
DO:
  APPLY "ALT-S" to {&WINDOW-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-1
&Scoped-define SELF-NAME B-Del
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Del C-KundKort
ON CHOOSE OF B-Del IN FRAME FRAME-1 /* Del */
DO:
  DO WITH FRAME FRAME-1:
  ASSIGN
      Medlem.KundeNr:SCREEN-VALUE = ""
      Kunde.Navn:SCREEN-VALUE     = ""
      .
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME B-GDPR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-GDPR C-KundKort
ON CHOOSE OF B-GDPR IN FRAME DEFAULT-FRAME /* GDPR */
DO:
    DEFINE VARIABLE cResultat AS CHARACTER   NO-UNDO.
  IF NOT AVAILABLE Medlem THEN
      RETURN NO-APPLY.
  IF AVAIL medlem THEN DO:
      lMedlemsNr = INPUT FILL-IN-MedlemsNr.
      RUN w-GDPR.w (INPUT lMedlemsNr, "MEDLEM",OUTPUT cResultat).
      APPLY "CLOSE" TO THIS-PROCEDURE.
      RETURN NO-APPLY.
  END.

  FIND Medlem WHERE Medlem.MedlemsNr = lMedlemsNr NO-LOCK NO-ERROR.
  IF AVAILABLE Medlem THEN
    DO:
      ASSIGN 
        wInputRecid = RECID(Medlem)
        wModus      = 'ENDRE'.
      RUN VisPost.
    END.
  RETURN NO-APPLY.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Konv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Konv C-KundKort
ON CHOOSE OF B-Konv IN FRAME DEFAULT-FRAME /* Konverter medlemsnr */
DO:
  IF NOT AVAILABLE Medlem THEN
      RETURN NO-APPLY.

  IF NOT CAN-DO('10,11',STRING(LENGTH(Medlem.Personnr))) THEN
  DO:
      MESSAGE 'Medlemmet må ha et 10 eller 11 sifret personnr for at medlemsnummeret skal kunne konverteres.'
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
  END.

  IF Medlem.MedlemsNr = DEC(Medlem.PersonNr) THEN
      RETURN NO-APPLY.

  lOk = FALSE.
  MESSAGE 'Skal medlemsnr. konverteres til personnr.?'
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lOk.
  IF lOk <> TRUE THEN
      RETURN NO-APPLY.

  lMedlemsNr = Medlem.MedlemsNr.
  RUN konverter_medlemsnr_personnr.p (INPUT-OUTPUT lMedlemsNr).
  FIND Medlem WHERE Medlem.MedlemsNr = lMedlemsNr NO-LOCK NO-ERROR.
  IF AVAILABLE Medlem THEN
    DO:
      ASSIGN 
        wInputRecid = RECID(Medlem)
        wModus      = 'ENDRE'.
      RUN VisPost.
    END.
  RETURN NO-APPLY.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-OppdatSaldo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-OppdatSaldo C-KundKort
ON CHOOSE OF B-OppdatSaldo IN FRAME DEFAULT-FRAME /* Oppdater saldo */
DO:
  IF AVAILABLE Medlem THEN
    RUN beregnmedlemsaldo.p(Medlem.MedlemsNr, 0).
  RUN VisMedlemSaldo.
  RUN VisSaldo.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Overfor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Overfor C-KundKort
ON CHOOSE OF B-Overfor IN FRAME DEFAULT-FRAME /* Overfør til medlem... */
DO:
  DEF VAR w2MedlemsNr LIKE Medlem.MedlemsNr NO-UNDO.
  DEF VAR piValg      AS INT NO-UNDO.

  IF NOT AVAILABLE Medlem THEN
      RETURN NO-APPLY.
  IF Medlem.KundeNr <> 0 THEN
  DO:
      MESSAGE "Du kan ikke overføre transaksjoner fra et medlem som er koblet til en kunde."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
  END.

  IF wModus = "ENDRE" AND Modifierad() THEN DO:
     MESSAGE "Vil du lagre posten?" VIEW-AS ALERT-BOX  QUESTION BUTTONS YES-NO
     TITLE "" UPDATE wChoice AS LOGICAL.
     IF wChoice = TRUE THEN
        RUN LagrePost(0).
        IF RETURN-VALUE = "AVBRYT" THEN DO:
            RETURN NO-APPLY.
     END.
     RUN VisPost.
   END.
   ASSIGN w2MedlemsNr = IF wModus = "ENDRE" THEN Medlem.MedlemsNr ELSE ?.
   RUN d-bmedlem.w (INPUT w2MedlemsNr, "").
   IF RETURN-VALUE <> "AVBRYT" THEN DO:
     FIND bMedlem WHERE recid(bMedlem) = int(return-value) NO-LOCK.
     IF AVAILABLE bMedlem THEN 
     DO:
         IF Medlem.MedlemsNr = bMedlem.MedlemsNr THEN
         DO:
             MESSAGE "Kan ikke flytte til seg selv."
                 VIEW-AS ALERT-BOX INFO BUTTONS OK.
             RETURN NO-APPLY.
         END.
         RUN gmedlemflytt.w (OUTPUT piValg).
         IF RETURN-VALUE = "AVBRYT" THEN
             RETURN NO-APPLY.

         /* Flytter Kort og transaksjoner. */
         IF piValg = 1 THEN
         DO:
             /* Henter liste med valgte transaksjoner */
             IF VALID-HANDLE(wSubWin2) THEN
               run HentValgListe in wSubWin2 (OUTPUT cValgListe).  
             ELSE DO:
                 run w-bmedtrans.w persistent set wSubWin2 (recid(Medlem),THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE).  
                 run HentValgListe in wSubWin2 (OUTPUT cValgListe).  
             END.
             RUN FlyttTrans&Kort (bMedlem.MedlemsNr, cValgListe, piValg).
             RUN beregnmedlemsaldo.p(Medlem.MedlemsNr, 0). /* Gammelt medlemsnummer */
             RUN beregnmedlemsaldo.p(bMedlem.MedlemsNr, 0). /* Nytt medlemsnummer */
             RUN oppdatmedlemstat.p (Medlem.MedlemsNr).  /* Gammelt */
             RUN oppdatmedlemstat.p (bMedlem.MedlemsNr). /* Nytt    */
             RUN VisPost.
             RUN ByttFrame.
             RUN VisMedlemSaldo.
             RUN VisSaldo.
         END.
         /* Flytter Kort. */
         ELSE IF piValg = 2 THEN
         DO:
             /* Henter liste med valgte transaksjoner */
             RUN FlyttTrans&Kort (bMedlem.MedlemsNr, cValgListe, piValg).
             RUN VisPost.
             RUN ByttFrame.
         END.
         ELSE IF piValg = 3 THEN
         DO:
             /* Henter liste med valgte transaksjoner */
             IF VALID-HANDLE(wSubWin2) THEN
               run HentValgListe in wSubWin2 (OUTPUT cValgListe).  
             ELSE DO:
                 run w-bmedtrans.w persistent set wSubWin2 (recid(Medlem),THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE).  
                 run HentValgListe in wSubWin2 (OUTPUT cValgListe).  
             END.
             RUN FlyttTrans&Kort (bMedlem.MedlemsNr, cValgListe, piValg).
             RUN beregnmedlemsaldo.p(Medlem.MedlemsNr, 0). /* Nytt medlemsnummer */
             RUN beregnmedlemsaldo.p(bMedlem.MedlemsNr, 0). /* Nytt medlemsnummer */
             RUN oppdatmedlemstat.p (Medlem.MedlemsNr).  /* Gammelt */
             RUN oppdatmedlemstat.p (bMedlem.MedlemsNr). /* Nytt    */
             RUN VisPost.
             RUN ByttFrame.
             RUN VisMedlemSaldo.
             RUN VisSaldo.
         END.
     END.
   END.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-1
&Scoped-define SELF-NAME B-SokButikk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SokButikk C-KundKort
ON CHOOSE OF B-SokButikk IN FRAME FRAME-1 /* ... */
or "F10" of Medlem.ButikkNr
DO:
  DO WITH FRAME FRAME-1:

    cTekst = "Butiker".
    THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
    RUN JBoxLookup.w (THIS-PROCEDURE,50,
                      "Butiker;Butik;ButNamn"
                     ,"WHERE TRUE"
                     ,""                                                  
                     ,"Butik",   /* <- return values for these fields */
                       OUTPUT cTekst,
                       OUTPUT wOK).
    THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

    IF RETURN-VALUE = "AVBRYT" THEN
        RETURN NO-APPLY.
    Medlem.ButikkNr:SCREEN-VALUE = cTekst.
    FIND Butiker NO-LOCK WHERE
      Butiker.Butik = INT(cTekst) NO-ERROR.
    IF AVAILABLE Butiker THEN
        Butiker.ButNamn:SCREEN-VALUE = Butiker.ButNamn.
    APPLY "ENTRY" TO Medlem.ButikkNr.
    RETURN NO-APPLY.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SokHovedMedlem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SokHovedMedlem C-KundKort
ON CHOOSE OF B-SokHovedMedlem IN FRAME FRAME-1 /* ... */
or "F10" of Medlem.HovedmedlemsNr
DO:
  {soek.i
    &Felt        = Medlem.HovedmedlemsNr
    &Program     = d-bhovedmedlem.w
    &ExtraParam  = "' '"
    &Frame       = FRAME-1
    &PostRun     = "find bMedlem no-lock where
                    recid(bMedlem) = int(return-value) no-error."
    &OptDisp     = " "
  } 
  FIND bMedlem NO-LOCK WHERE
      bMedlem.MedlemsNr = INPUT Medlem.HovedMedlemsNr NO-ERROR.
  IF AVAILABLE bMedlem THEN
      DISPLAY 
        bMedlem.Fornavn + " "  + bMedlem.EtterNavn @ FI-Hovedmedlem
      WITH FRAME Frame-1.
  ELSE 
      DISPLAY
        "" @ FI-HovedMedlem
      WITH FRAME Frame-1.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SokMedlemsGruppe
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SokMedlemsGruppe C-KundKort
ON CHOOSE OF B-SokMedlemsGruppe IN FRAME FRAME-1 /* ... */
or "F10" of Medlem.MedGruppe
DO:
  DO WITH FRAME FRAME-1:

    cTekst = "MedlemsGruppe".
    THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
    RUN JBoxLookup.w (THIS-PROCEDURE,50,
                      "MedlemsGruppe;MedGruppe;Beskrivelse"
                     ,"WHERE TRUE"
                     ,""                                                  
                     ,"MedGruppe",   /* <- return values for these fields */
                       OUTPUT cTekst,
                       OUTPUT wOK).
    THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

    IF RETURN-VALUE = "AVBRYT" THEN
        RETURN NO-APPLY.
    Medlem.MedGruppe:SCREEN-VALUE = cTekst.
    FIND MedlemsGruppe NO-LOCK WHERE
      MedlemsGruppe.MedGruppe = INT(cTekst) NO-ERROR.
    IF AVAILABLE MedlemsGruppe THEN
        MedlemsGruppe.Beskrivelse:SCREEN-VALUE = MedlemsGruppe.Beskrivelse.
    APPLY "ENTRY" TO Medlem.MedGruppe.
    RETURN NO-APPLY.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SokMedlemsType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SokMedlemsType C-KundKort
ON CHOOSE OF B-SokMedlemsType IN FRAME FRAME-1 /* ... */
or "F10" of Medlem.MedType
DO:
  DO WITH FRAME FRAME-1:

    cTekst = "MedlemsType".
    THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
    RUN JBoxLookup.w (THIS-PROCEDURE,50,
                      "MedlemsType;MedType;Beskrivelse"
                     ,"WHERE TRUE"
                     ,""                                                  
                     ,"MedType",   /* <- return values for these fields */
                       OUTPUT cTekst,
                       OUTPUT wOK).
    THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

    IF RETURN-VALUE = "AVBRYT" THEN
        RETURN NO-APPLY.
    Medlem.MedType:SCREEN-VALUE = cTekst.
    FIND MedlemsType NO-LOCK WHERE
      MedlemsType.MedType = INT(cTekst) NO-ERROR.
    IF AVAILABLE MedlemsType THEN
        MedlemsType.Beskrivelse:SCREEN-VALUE = MedlemsType.Beskrivelse.
    APPLY "ENTRY" TO Medlem.MedType.
    RETURN NO-APPLY.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SokPost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SokPost C-KundKort
ON CHOOSE OF B-SokPost IN FRAME FRAME-1 /* ... */
or "F10" of Medlem.PostNr
DO:

  DO WITH FRAME FRAME-1:

    cTekst = "Post".
    THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
    RUN JBoxLookup.w (THIS-PROCEDURE,50,
                      "Post;PostNr;Beskrivelse"
                     ,"WHERE TRUE"
                     ,""                                                  
                     ,"PostNr",   /* <- return values for these fields */
                       OUTPUT cTekst,
                       OUTPUT wOK).
    THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

    IF RETURN-VALUE = "AVBRYT" THEN
        RETURN NO-APPLY.
    FIND Post NO-LOCK WHERE
      Post.PostNr = (cTekst) NO-ERROR.
    IF AVAILABLE Post THEN
    DO:
        ASSIGN
            Medlem.PostNr:SCREEN-VALUE = cTekst
            FI-Poststed1:SCREEN-VALUE = Post.Beskrivelse
            .
    END.
    APPLY "ENTRY" TO Medlem.PostNr.
    RETURN NO-APPLY.
  END.


  /* Kaller søkerutine */
  RUN gpost.w (
    INPUT-OUTPUT cTekst, /* Returstreng - chr(1) separert */
    "", /* Feltliste avgrensningsfelt (kommaseparert) */
    "", /* Feltverdier (chr(1) sep) */ 
    Medlem.PostNr:SCREEN-VALUE /* Post markøren skal stå på */
    ).
  IF RETURN-VALUE = "AVBRYT" THEN
      RETURN NO-APPLY.
  IF NUM-ENTRIES(cTekst,CHR(1)) >= 3 THEN
  DO:
      /* Legger opp verdier I de aktuelle feltene */
      /* KundeNr,Navn,Adresse1,PostNr,Telefon,MobilTlf,KontE-Post */
      ASSIGN
        Medlem.PostNr:SCREEN-VALUE = ENTRY(2,cTekst,CHR(1))
        Medlem.BydelsNr:SCREEN-VALUE = SUBSTRING(TRIM(ENTRY(2,cTekst,CHR(1))),1,3)
        FI-PostSted1:SCREEN-VALUE = ENTRY(3,cTekst,CHR(1))
        .
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SokRegion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SokRegion C-KundKort
ON CHOOSE OF B-SokRegion IN FRAME FRAME-1 /* ... */
or "F10" of Medlem.RegKode
DO:
  DO WITH FRAME FRAME-1:

    cTekst = "Region".
    THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
    RUN JBoxLookup.w (THIS-PROCEDURE,50,
                      "Region;RegKode;Navn"
                     ,"WHERE TRUE"
                     ,""                                                  
                     ,"RegKode",   /* <- return values for these fields */
                       OUTPUT cTekst,
                       OUTPUT wOK).
    THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

    IF RETURN-VALUE = "AVBRYT" THEN
        RETURN NO-APPLY.
    Medlem.RegKode:SCREEN-VALUE = cTekst.
    FIND Region NO-LOCK WHERE
      Region.RegKode = cTekst NO-ERROR.
    IF AVAILABLE Region THEN
        Region.Navn:SCREEN-VALUE = Region.Navn.
    APPLY "ENTRY" TO Medlem.RegKode.
    RETURN NO-APPLY.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Spar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Spar C-KundKort
ON CHOOSE OF B-Spar IN FRAME FRAME-1 /* Oppslag mot SPAR */
DO:
    DEFINE VARIABLE cPrefix AS CHARACTER   NO-UNDO.
  DO WITH FRAME FRAME-1:
      IF LENGTH(Medlem.PersonNr:SCREEN-VALUE) <> 10 THEN
      DO:
          MESSAGE 'Det må være 10 siffer i personnr mot SPAR'
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN NO-APPLY.
      END.
      cPrefix = IF YEAR(TODAY) - INT('19' + SUBSTRING(Medlem.PersonNr:SCREEN-VALUE,1,2)) > 100
                     THEN "20"
                     ELSE "19".


      ASSIGN
          cErrorMessage = ''
          cPersonNr    = cPrefix + Medlem.PersonNr:SCREEN-VALUE
          lPersonFunnet = FALSE
          lSuccess      = FALSE 
          .
      {sww.i}
      RUN SPAR_Personsokninfraga.p (cPersonNr, /* personnr */
        OUTPUT Persondetaljer_Fornamn,
        OUTPUT Persondetaljer_Efternamn,
        OUTPUT Persondetaljer_Fodelsetid,
        OUTPUT Persondetaljer_Kon,
        OUTPUT Folkbokforingsadress_Utdelningsadress2,
        OUTPUT Folkbokforingsadress_PostNr,
        OUTPUT Folkbokforingsadress_Postort,
        OUTPUT Folkbokforingsadress_FolkbokfordLanKod,
        OUTPUT Folkbokforingsadress_FolkbokfordKommunKod,
        OUTPUT Folkbokforingsadress_FolkbokfordForsamlingKod,
        OUTPUT cErrorMessage,
        OUTPUT lPersonFunnet,
        OUTPUT lSuccess).
      {swn.i}
      IF lSuccess AND lPersonFunnet THEN
      DO:
          ASSIGN
              Medlem.ForNavn:SCREEN-VALUE   = Persondetaljer_Fornamn
              Medlem.Etternavn:SCREEN-VALUE = Persondetaljer_Efternamn
              Medlem.PostNr:SCREEN-VALUE    = Folkbokforingsadress_PostNr
              Medlem.Adresse1:SCREEN-VALUE  = Folkbokforingsadress_Utdelningsadress2
              .
      END.
  END.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help C-KundKort
ON CHOOSE OF Btn_Help IN FRAME DEFAULT-FRAME /* Help */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  run WinHlp.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-1
&Scoped-define SELF-NAME Medlem.ButikkNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Medlem.ButikkNr C-KundKort
ON TAB OF Medlem.ButikkNr IN FRAME FRAME-1 /* Butikk */
OR "RETURN" OF Medlem.ButikkNr
DO:
    FIND Butiker NO-LOCK WHERE
        Butiker.Butik = INPUT Medlem.butikkNr NO-ERROR.
    IF AVAILABLE Butiker THEN
        ASSIGN
          Medlem.ButikkNr:SCREEN-VALUE = STRING(Butiker.Butik)
          Butiker.ButNamn:SCREEN-VALUE = Butiker.ButNamn
        .
    ELSE 
    ASSIGN
      Medlem.ButikkNr:SCREEN-VALUE  = ""
      Butiker.ButNamn:SCREEN-VALUE = ""
    .
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME BUTTON-Angre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Angre C-KundKort
ON CHOOSE OF BUTTON-Angre IN FRAME DEFAULT-FRAME /* Ångra */
DO:
  IF wModus = "NY" THEN
     ASSIGN Medlem.MedlemsNr:SENSITIVE IN FRAME FRAME-1 = NO.
            wModus = "ENDRE".
  RUN VisPost.
  RUN BUTTONEnaDis.
  return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Kopier
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Kopier C-KundKort
ON CHOOSE OF BUTTON-Kopier IN FRAME DEFAULT-FRAME /* Kopiera */
DO:
    MESSAGE "Ikke tillatt. Kontakt Polygon."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN NO-APPLY.
  IF NOT AVAILABLE Medlem THEN
      RETURN NO-APPLY.

  RUN GetMedlemsNr IN wLibHandle (OUTPUT ipMedlemsNr).

  DO WITH FRAME FRAME-1:
    assign 
      wModus   = "NY"
      Medlem.MedlemsNr:SENSITIVE = FALSE
      Medlem.MedlemsNr:SCREEN-VALUE = string(ipMedlemsNr)
      Medlem.KundeNr:SCREEN-VALUE = ""
      FI-MedlemsKort:SCREEN-VALUE = ""
      AktivertFraWeb:SCREEN-VALUE = ""
      WebBrukerid:SCREEN-VALUE    = ""
      WebPassord:SCREEN-VALUE     = ""
      .
    RUN BUTTONEnaDis.
/*     RUN d-kortnummer.w.                            */
/*     IF RETURN-VALUE = "AVBRYT" THEN                */
/*     DO:                                            */
/*         APPLY "CHOOSE" TO BUTTON-Angre.            */
/*         RETURN NO-APPLY.                           */
/*     END.                                           */
/*     ELSE ASSIGN                                    */
/*         wKortNummer = RETURN-VALUE                 */
/*         FI-Medlemskort:SCREEN-VALUE = wKortNummer. */

    apply "ENTRY":U to Medlem.ForNavn.
  END.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Lagre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Lagre C-KundKort
ON CHOOSE OF BUTTON-Lagre IN FRAME DEFAULT-FRAME /* Lagra */
DO:
  run LagrePost (0).
  if return-value = "AVBRYT" then
    return no-apply.
  RUN VisEndretInfo.
  RUN BUTTONEnaDis.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Next
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Next C-KundKort
ON CHOOSE OF BUTTON-Next IN FRAME DEFAULT-FRAME /* Neste */
DO:
    IF VALID-HANDLE(hParentHandle) THEN DO:
        RUN PrevNext IN hParentHandle ("Next").
    END.
    ELSE
        run Bytpost("Next").
    RETURN NO-APPLY.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Ny
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Ny C-KundKort
ON CHOOSE OF BUTTON-Ny IN FRAME DEFAULT-FRAME /* Ny */
DO:
    ASSIGN wKortNummer = "".
    IF cManuellt = "1" THEN 
    DO:
        RUN d-kortnummer.w.
        IF RETURN-VALUE = "AVBRYT" THEN
        DO:
            IF NOT AVAILABLE Medlem THEN
                APPLY "CLOSE" TO THIS-PROCEDURE.
            ELSE
              RETURN NO-APPLY.
        END.
        ELSE ASSIGN
            wKortNummer = RETURN-VALUE.
        run BUTTON-Ny.
        RETURN NO-APPLY.
    END.
    ELSE IF cManuellt = "2" THEN
    DO:
       
        /* Oppslag mot SPAR gir automatisk opprettelse av nytt medlem. */
        /* RETURN-VALUE inneholder medlemsnr.                          */
        RUN wSPAR_PersonOppslag.w (OUTPUT cMedlemsNr).
        IF cMedlemsNr = "AVBRYT" OR 
            cMedlemsNr = '' THEN
        DO:
            IF NOT AVAILABLE Medlem THEN
                APPLY "CLOSE" TO THIS-PROCEDURE.
            ELSE
              RETURN NO-APPLY.
        END.
        ELSE DO:
            ASSIGN wKortNummer = cMedlemsNr.
            FIND Medlem WHERE Medlem.MedlemsNr = DEC(wKortNummer) NO-LOCK NO-ERROR.
            IF AVAILABLE Medlem THEN
            DO:
                ASSIGN 
                    wInputRecid = RECID(Medlem)
                    wModus      = 'ENDRE'.
                RUN VisPost.
            END.
            RETURN NO-APPLY.
        END.
    END.
    ELSE DO:
        run BUTTON-Ny.
        RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Ok C-KundKort
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
  IF Modifierad() THEN DO: 
    run LagrePost (0).
    if return-value <> "OK" then
     do:
       readkey pause 0.
       return no-apply.
    end.
  END.
  APPLY "CLOSE":U TO THIS-PROCEDURE.  
  return wModus + "," + string(wInputRecid).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Prev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Prev C-KundKort
ON CHOOSE OF BUTTON-Prev IN FRAME DEFAULT-FRAME /* Forrige */
DO:
    IF VALID-HANDLE(hParentHandle) THEN DO:
        RUN PrevNext IN hParentHandle ("Prev").
    END.
    ELSE
        run Bytpost("Prev").
    RETURN NO-APPLY.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Slett
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Slett C-KundKort
ON CHOOSE OF BUTTON-Slett IN FRAME DEFAULT-FRAME /* Radera */
DO:
  IF NOT AVAILABLE Medlem THEN
      RETURN NO-APPLY.
  run BUTTON-Slett.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-1
&Scoped-define SELF-NAME BUTTON-SokeForsteKjop
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokeForsteKjop C-KundKort
ON CHOOSE OF BUTTON-SokeForsteKjop IN FRAME FRAME-1 /* ... */
DO:
  IF AVAILABLE Medlem THEN
    RUN d-bmedlemsaldo.w (0,Medlem.MedlemsNr,"V").
  RUN VisMedlemSaldo.
  RUN VisSaldo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokeKort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokeKort C-KundKort
ON CHOOSE OF BUTTON-SokeKort IN FRAME FRAME-1 /* ... */
DO:
  IF AVAILABLE Medlem THEN
        RUN d-bmedlemskort.w (0,Medlem.MedlemsNr,"V").
  RUN VisMedlemsKort.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokeKunde
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokeKunde C-KundKort
ON CHOOSE OF BUTTON-SokeKunde IN FRAME FRAME-1 /* ... */
or F10 of Medlem.MedlemsNr
DO:
  DO WITH FRAME FRAME-1:
      ASSIGN
          wOldKundeNr = INPUT Medlem.KundeNr
          .
      cTekst = "Kunde".
      THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
      RUN JBoxLookup.w (THIS-PROCEDURE,50,
                        "Kunde;Navn;KundeNr"
                       ,"WHERE TRUE"
                       ,""                                                  
                       ,"KundeNr",   /* <- return values for these fields */
                         OUTPUT cTekst,
                         OUTPUT wOK).
      THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

      IF RETURN-VALUE = "AVBRYT" THEN
          RETURN NO-APPLY.


      FIND Kunde WHERE Kunde.KundeNr = DEC(cTekst) NO-LOCK.
      IF AVAILABLE Kunde THEN
          DISPLAY
            Kunde.KundeNr @ Medlem.KundeNr
            Kunde.Navn
          WITH FRAME FRAME-1.
      ELSE 
          DISPLAY
            "" @ Medlem.KundeNr
            "" @ Kunde.Navn
          WITH FRAME FRAME-1.

      IF (INPUT Medlem.KundeNr <> wOldKundeNr) AND wOldKundeNr > 0 THEN
      DO:
          MESSAGE "Skal medlemmets kundetilhøringhet byttes fra " + STRING(wOldKundeNr) +
                  " til " + STRING(INPUT Medlem.KundeNr) + "?" SKIP(1)
                  "Følgende endringer vil da bli gjennomført:" SKIP
                  "* Medlemmet kobles til ny kunde" SKIP
                  "* Kundekort som er koblet 'en til en' mot medlemskort blir flyttet til ny kunde"
                  "* Innlest medlemssalg blir liggende på medlemmet" SKIP
                  "* Medlemmets salg som er innlest på gammelt kundenr blir liggende urørt"
              VIEW-AS ALERT-BOX BUTTONS YES-NO UPDATE wSvar.
          IF wSvar = TRUE THEN DO:
              RUN flytt_medlem_til_ny_kunde.p (INPUT INPUT Medlem.MedlemsNr,
                                               INPUT INPUT Medlem.KundeNr,
                                               INPUT wOldKundeNr,
                                               OUTPUT wSvar).
              IF wSvar = FALSE THEN
                  MESSAGE "Flytting av medlem til nytt kundenummer misslykkes."
                      VIEW-AS ALERT-BOX INFO BUTTONS OK.
          END.

      END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokMedlem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokMedlem C-KundKort
ON CHOOSE OF BUTTON-SokMedlem IN FRAME FRAME-1 /* ... */
OR F10 OF Medlem.MedlemsNr 
DO:
  DO WITH FRAME FRAME-1:

    IF AVAILABLE Medlem THEN RUN LagrePost(0).

    cTekst = "Medlem".
    THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
    RUN JBoxLookup.w (THIS-PROCEDURE,50,
                      "Medlem;Fornavn;Etternavn;MedlemsNr"
                     ,"WHERE TRUE"
                     ,""                                                  
                     ,"MedlemsNr",   /* <- return values for these fields */
                       OUTPUT cTekst,
                       OUTPUT wOK).
    THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

    IF RETURN-VALUE = "AVBRYT" THEN
        RETURN NO-APPLY.
    FIND Medlem NO-LOCK WHERE
      Medlem.MedlemsNr = DEC(cTekst) NO-ERROR.
    IF AVAILABLE Medlem THEN
        RUN VisPost.
    APPLY "ENTRY" TO Medlem.Fornavn.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME BUTTON-SokMedlem2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokMedlem2 C-KundKort
ON CHOOSE OF BUTTON-SokMedlem2 IN FRAME DEFAULT-FRAME /* ... */
DO:
    APPLY "CHOOSE" TO BUTTON-SokMedlem IN FRAME FRAME-1.
    IF RETURN-VALUE <> "AVBRYT" AND AVAILABLE Medlem THEN 
    DO:
        RUN LagrePost (0).
        RUN ByttFrame.
    END.
    /*    
        AND wAktivFlip = 2 THEN DO:
        run ByttObjekt in wSubWin1 (string(Medlem.MedlemsNr,"9999999999999")).  
    END.
    */
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-1
&Scoped-define SELF-NAME Medlem.FodselsDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Medlem.FodselsDato C-KundKort
ON DELETE-CHARACTER OF Medlem.FodselsDato IN FRAME FRAME-1 /* Fødselsdato */
DO:
  ASSIGN
      SELF:SCREEN-VALUE = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Medlem.FodselsDato C-KundKort
ON TAB OF Medlem.FodselsDato IN FRAME FRAME-1 /* Fødselsdato */
OR "RETURN" OF Medlem.fodselsDato
DO:
  IF INPUT Medlem.FodselsDato <> ? THEN
  DO:
    ASSIGN
        Medlem.FodtAr:SCREEN-VALUE = STRING(year(INPUT Medlem.FodselsDato))
        Medlem.FodtAr:SENSITIVE = false
        .
  END.
  ELSE DO:
      ASSIGN
          Medlem.FodtAr:SCREEN-VALUE = ""
          Medlem.FodtAr:SENSITIVE = TRUE
          .
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Medlem.HovedMedlemFlagg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Medlem.HovedMedlemFlagg C-KundKort
ON RETURN OF Medlem.HovedMedlemFlagg IN FRAME FRAME-1 /* Hovedmedlem */
DO:
  APPLY "TAB" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Medlem.HovedMedlemFlagg C-KundKort
ON VALUE-CHANGED OF Medlem.HovedMedlemFlagg IN FRAME FRAME-1 /* Hovedmedlem */
DO:
  IF INPUT Medlem.HovedmedlemFlagg = TRUE THEN
      ASSIGN
        Medlem.HovedMedlemsNr:SCREEN-VALUE = ""
        FI-Hovedmedlem:SCREEN-VALUE        = ""
      .
  RUN SetSokHovedMedlem.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Medlem.HovedMedlemsNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Medlem.HovedMedlemsNr C-KundKort
ON TAB OF Medlem.HovedMedlemsNr IN FRAME FRAME-1 /* Hovedmedlem */
OR "RETURN" OF Medlem.MedlemsNr
DO:
  FIND bMedlem NO-LOCK WHERE
      bMedlem.MedlemsNr = INPUT Medlem.HovedMedlemsNr NO-ERROR.
  IF AVAILABLE bMedlem THEN
      DISPLAY 
        bMedlem.Fornavn + " "  + bMedlem.EtterNavn @ FI-Hovedmedlem
      WITH FRAME Frame-1.
  ELSE 
      DISPLAY
        "" @ FI-HovedMedlem
      WITH FRAME Frame-1.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Medlem.MedGruppe
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Medlem.MedGruppe C-KundKort
ON TAB OF Medlem.MedGruppe IN FRAME FRAME-1 /* Medlemsgruppe */
OR "RETURN" OF Medlem.MedGruppe
DO:
    FIND MedlemsGruppe NO-LOCK WHERE
        MedlemsGruppe.MedGruppe = INPUT Medlem.MedGruppe NO-ERROR.
    IF AVAILABLE MedlemsGruppe THEN
        ASSIGN
          Medlem.MedGruppe:SCREEN-VALUE = STRING(MedlemsGruppe.MedGruppe)
          MedlemsGruppe.Beskrivelse:SCREEN-VALUE = MedlemsGruppe.Beskrivelse
        .
    ELSE 
    ASSIGN
      Medlem.MedGruppe:SCREEN-VALUE  = ""
      MedlemsGruppe.Beskrivelse:SCREEN-VALUE = ""
    .
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Medlem.MedlemNotat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Medlem.MedlemNotat C-KundKort
ON TAB OF Medlem.MedlemNotat IN FRAME FRAME-1 /* Notat */
DO:
   APPLY "Entry" TO Medlem.Fornavn.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Medlem.MedType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Medlem.MedType C-KundKort
ON TAB OF Medlem.MedType IN FRAME FRAME-1 /* Medlemstype */
OR "RETURN" OF Medlem.MedType
DO:
    FIND MedlemsType NO-LOCK WHERE
        MedlemsType.MedTYPE = INPUT Medlem.MedType NO-ERROR.
    IF AVAILABLE MedlemsType THEN
        ASSIGN
          Medlem.MedTYPE:SCREEN-VALUE = STRING(MedlemsType.MedType)
          MedlemsType.Beskrivelse:SCREEN-VALUE = MedlemsType.Beskrivelse
        .
    ELSE 
    ASSIGN
      Medlem.MedType:SCREEN-VALUE  = ""
      MedlemsType.Beskrivelse:SCREEN-VALUE = ""
    .
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Medlem.Opphort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Medlem.Opphort C-KundKort
ON DELETE-CHARACTER OF Medlem.Opphort IN FRAME FRAME-1 /* Opphører */
DO:
  ASSIGN
      self:screen-value = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Medlem.PostNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Medlem.PostNr C-KundKort
ON TAB OF Medlem.PostNr IN FRAME FRAME-1 /* PostNr/BydelsNr */
OR "RETURN" OF Medlem.Post
DO:
  DO WITH FRAME FRAME-1:
    FIND Post NO-LOCK WHERE
        Post.PostNr = INPUT Medlem.PostNr NO-ERROR.
    IF NOT AVAILABLE Post THEN
        ASSIGN
          FI-PostSted1:SCREEN-VALUE   = ""
          Medlem.BydelsNr:SCREEN-VALUE = "".
    ELSE
        ASSIGN
          FI-PostSted1:SCREEN-VALUE   = Post.Beskrivelse
          Medlem.BydelsNr:SCREEN-VALUE = substring(INPUT Medlem.PostNr,1,3).
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME TabStrip
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TabStrip C-KundKort OCX.Click
PROCEDURE TabStrip.TabStrip.Click .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/

IF wAktivFlip <> INT(chTabStrip:SelectedItem:Index) THEN DO:
  IF wModus = "NY" THEN DO:
      MESSAGE "Posten må lagres først." VIEW-AS ALERT-BOX INFORMATION TITLE "Feil".
              ASSIGN wFeil = TRUE
                     chTab1Side = chTabs:Item (wAktivFlip BY-VARIANT-POINTER)
                     chTabStrip:SelectedItem = chTab1Side.
  END.
  ELSE IF wAktivFlip = 1 AND Modifierad() THEN DO:
/*       MESSAGE "Vill du lagre posten?" VIEW-AS ALERT-BOX  QUESTION BUTTONS YES-NO */
/*       TITLE "" UPDATE wChoice AS LOGICAL.                                        */
      DEF VAR wChoice AS LOG INIT TRUE NO-UNDO.
      IF wChoice = TRUE THEN
          RUN LagrePost(0).
          IF RETURN-VALUE = "AVBRYT" THEN DO:
              ASSIGN wFeil = TRUE
                     chTab1Side = chTabs:Item (wAktivFlip BY-VARIANT-POINTER)
                     chTabStrip:SelectedItem = chTab1Side.
              RETURN NO-APPLY.
      END.
      ELSE DO:
          IF wModus = "NY" THEN
              ASSIGN wModus = "ENDRE".
      END.
      RUN VisPost.
  END.
  IF NOT wFeil THEN DO:
    ASSIGN wAktivFlip = INT(chTabStrip:SelectedItem:Index).
    RUN ByttFrame. /* Bytter tab */
    RUN ButtonEnaDis.
  END.
  IF wAktivFlip = 1 AND NOT wFeil THEN
      APPLY "ENTRY" TO Medlem.FORNavn IN FRAME FRAME-1.
  ELSE IF wAktivFlip <> 1 THEN
      APPLY "ENTRY" TO CB-Sort IN FRAME DEFAULT-FRAME.
 END.
 ELSE DO:
    IF NOT wFeil THEN DO:
     IF wAktivFlip = 1 THEN
            APPLY "ENTRY" TO Medlem.ForNavn IN FRAME FRAME-1.
     ELSE
       APPLY "ENTRY" TO CB-Sort IN FRAME DEFAULT-FRAME.
    END.
    ELSE
        wFeil = FALSE.
 END.
  RETURN NO-APPLY.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-KundKort 


/* ***************************  Main Block  *************************** */
/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* Biblotek, inn/utmelding og ON CLOSE OF THIS... */
{genlib.i 
  &NoLibCall      = "Nei"
  &WindowName     = "Medlemskort"
  &PostIClose    = "if valid-handle(wSubWin1) then
                         delete procedure wSubWin1 no-error.
                    if valid-handle(wSubWin2) then
                         delete procedure wSubWin2 no-error.
                    if valid-handle(wSubWin3) then
                         delete procedure wSubWin3 no-error.
                     RUN DelTmpChild.
                     IF VALID-HANDLE(chTabStrip) THEN
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
  &PostDisable_ui = "wModus = 'AVBRYT'." 
}
{syspara.i 14 1 3 wGyldighet INT}
{syspara.i 1 1 17 wDbId}          /* DatabaseID */
{syspara.i 14 1 4 wLedige INT}    /* Føste eller neste ledige medlemsnummer. */
{syspara.i 14 1 5 cManuellt}      /* Manuell reg av medlemskort */
{syspara.i 5 1 1 wCl INT}
{syspara.i 14 1 26 iMobilTkn INT}
IF iMobilTkn > 0 THEN
    cMobilFormat = "X(" + STRING(iMobilTkn) + ")".
FIND clButiker WHERE
  clButiker.Butik = wCl NO-ERROR.

IF ENTRY(1,wModus) = "ENDRE" AND NUM-ENTRIES(wModus) > 1 THEN DO:
    ASSIGN hParentHandle = WIDGET-HANDLE(ENTRY(2,wModus))
           wModus = "ENDRE".
    SUBSCRIBE TO "ByttMedlem" IN hParentHandle.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* HotKeySøk - DYYYYRT */
on ALT-N of {&WINDOW-NAME} anywhere 
  do:
    apply "CHOOSE":U to BUTTON-Ny in frame DEFAULT-FRAME.
  end.
on ALT-L of {&WINDOW-NAME} anywhere 
  do:
    apply "CHOOSE":U to BUTTON-Lagre in frame DEFAULT-FRAME.
  end.
on ALT-K of {&WINDOW-NAME} anywhere 
  do:
    apply "CHOOSE":U to BUTTON-Kopier in frame DEFAULT-FRAME.
  end.
on ALT-D of {&WINDOW-NAME} anywhere 
  do:
    apply "CHOOSE":U to BUTTON-Slett in frame DEFAULT-FRAME.
  end.
on ALT-A of {&WINDOW-NAME} anywhere 
  do:
    apply "CHOOSE":U to BUTTON-angre in frame DEFAULT-FRAME.
  end.
on ALT-CURSOR-UP of {&WINDOW-NAME} anywhere 
  do:
    apply "CHOOSE":U to BUTTON-Prev in frame DEFAULT-FRAME.
  end.
on ALT-CURSOR-DOWN of {&WINDOW-NAME} anywhere 
  do:
    apply "CHOOSE":U to BUTTON-Next in frame DEFAULT-FRAME.
  end.
on ALT-S of {&WINDOW-NAME} anywhere 
  do: /* SOKTRIGGER */
    RUN d-medlemssok.w.
    IF RETURN-VALUE = "AVBRYT" THEN 
        RETURN NO-APPLY.
    ELSE DO:
      FIND Medlem NO-LOCK WHERE
        Medlem.MedlemsNr = DEC(RETURN-VALUE) NO-ERROR.
      if available Medlem then
      do:
        assign
          wInputRecid = recid(Medlem).
        run VisPost.
        RUN ByttFrame.
        /*
        if wAktivFlip = 2 then
          DO:
            run ByttObjekt in wSubWin1 (string(Medlem.MedlemsNr,"9999999999999")).  
          end.
        if wAktivFlip = 3 then
          do:
            IF VALID-HANDLE(wSubWin1) THEN
              DELETE PROCEDURE wSubWin1.
            run w-bmedtrans.w persistent set wSubWin1 (wInputRecid,THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE).  
          end.
        */
      end.
    END.
  end. /* SOKTRIGGER */

on "CTRL-TAB":U anywhere
  do:
    chTab1Side = chTabs:Item ((IF wAktivFlip = NUM-ENTRIES(wTabTekst)
                      THEN 1 ELSE wAktivFlip + 1) BY-VARIANT-POINTER).
    chTabStrip:SelectedItem = chTab1Side.
    RETURN NO-APPLY.
  end.

/* Flagger at systemet kjøres på en LapTop (Innkjøpssystem) */
if valid-handle(wLibHandle) then
  run SjekkLapTop in wLibHandle (output wLapTop).

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  IF wInputRecid = ? AND wModus = "ENDRE" THEN DO:
      MESSAGE "Recid = ? och wModus = 'ENDRE' --> FEL !!" VIEW-AS ALERT-BOX ERROR.
      RETURN "AVBRYT".
  END.
  RUN enable_UI.

  B-GDPR:SENSITIVE = SEARCH("w-GDPR.w") <> ?.

  IF cMobilFormat <> "" THEN
      Medlem.MobilTlf:FORMAT = cMobilFormat.
  {lng.i} /* Oversettelse */
  assign
    cTekst = Tx(" Medlemsregister",1)
    .
  C-KundKort:TITLE = cTekst.

  
  RUN InitCB.
  ASSIGN {&WINDOW-NAME}:HIDDEN = NO
         CB-Sort:SCREEN-VALUE = ENTRY(1,CB-Sort:LIST-ITEMS)
         wTabHnd[1] = FRAME FRAME-1:HANDLE
         wTabHnd[2] = FRAME FRAME-2:HANDLE
         wTabHnd[3] = FRAME FRAME-3:HANDLE
         .

  run ByttFrame. /* Legger opp f›rste fane. */

/*   assign chTab1Side = chCtrlFrame:TabStrip. /* COM-Handl 1. side */ */
  assign chTab1Side = chTabStrip. /* COM-Handl 1. side */
  

  if wModus <> "NY" then DO:
      find Medlem where recid(Medlem) = wInputRecid no-lock no-error.
      run VisPost.
  END.
  run ByttFrame. /* Legger opp f›rste fane. */ 

/*   assign chTab1Side = chCtrlFrame:TabStrip. /* COM-Handl 1. side */ */
  assign chTab1Side = chTabStrip. /* COM-Handl 1. side */
  
  if wModus = "NY" then 
    do:
      apply "CHOOSE":U to BUTTON-Ny.
    end.
  else if wModus = "SLETT" THEN DO:
     APPLY "CHOOSE" TO BUTTON-Slett.
     RETURN RETURN-VALUE.
  END.
  
  RUN SetSokHovedMedlem.

  /*  */

  APPLY "entry" TO Medlem.ForNavn IN FRAME FRAME-1.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
    
  if wModus <> "NY" then
    find Medlem no-lock where 
      recid(Medlem) = wInputRecid no-error.
      
  /* Retur verdi */  
  if available Medlem and wModus <> "NY" then
    return string(recid(Medlem)).
  else 
    return "AVBRYT".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BUTTON-Ny C-KundKort 
PROCEDURE BUTTON-Ny :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  /*RUN GetMedlemsNr IN wLibHandle (OUTPUT ipMedlemsNr).*/
  
  DEF VAR wSort AS INT NO-UNDO.

  DO WITH FRAME FRAME-1:
     IF Modifierad() THEN DO:
         RUN LagrePost(0).
         IF RETURN-VALUE = "AVBRYT" THEN
             RETURN NO-APPLY.
     END.
     FIND FIRST MedlemsGruppe NO-LOCK NO-ERROR.
     FIND FIRST MedlemsType   NO-LOCK NO-ERROR.

     CLEAR FRAME FRAME-1.
     ASSIGN wModus = "NY"
            FILL-IN-MedlemsNr:SCREEN-VALUE IN FRAME DEFAULT-FRAME = "" 
            FILL-IN-Navn:SCREEN-VALUE IN FRAME DEFAULT-FRAME = ""
            Medlem.MedlemsNr:SENSITIVE = FALSE
            /*Medlem.MedlemsNr:SCREEN-VALUE = string(ipMedlemsNr)*/
            Medlem.MedType:SCREEN-VALUE = "0"
            Medlem.MedGruppe:SCREEN-VALUE = "0"
            Medlem.RegKode:SCREEN-VALUE = " "
            Medlem.HovedMedlemFlagg:SCREEN-VALUE = "yes"
            Medlem.Aktiv:SCREEN-VALUE = "yes"
            Medlem.MottaeMailUtsendelser:SCREEN-VALUE = "yes"
            Medlem.Bonus_Berettiget:SCREEN-VALUE = 'YES'
            Medlem.Bonus_Varsel:SCREEN-VALUE = '1'
            Medlem.Bonus_Forsendelse:SCREEN-VALUE = '1'
            FI-MedlemsKort:SCREEN-VALUE = wKortNummer
            Medlem.Kjonn:SCREEN-VALUE = "M"
            Medlem.Medtype:SCREEN-VALUE = IF AVAILABLE MedlemsType 
                                            THEN string(MedlemsType.Medtype)
                                            ELSE '0'
            Medlem.MedGruppe:SCREEN-VALUE = IF AVAILABLE MedlemsGruppe
                                             THEN string(MedlemsGruppe.MedGruppe)
                                             ELSE '0'
            Medlem.butikkNr:SCREEN-VALUE  = IF AVAILABLE clButiker
                                                 THEN string(clButiker.Butik)
                                                 ELSE '0'
            Medlem.Kilde:SCREEN-VALUE = ""
            Medlem.TilgKilde:SCREEN-VALUE = ""
            Medlem.EksterntMedlemsNr:SCREEN-VALUE = ""
            Medlem.Rabatt:SCREEN-VALUE = "0"
            .

    RUN BUTTONEnaDis.
    APPLY "ENTRY" TO Medlem.ForNavn.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BUTTON-Slett C-KundKort 
PROCEDURE BUTTON-Slett :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var wOk as log format "Ja/Nei" no-undo.
  def var wNesteRecid as recid no-undo.
  
  IF NOT AVAILABLE Medlem THEN
      RETURN "AVBRYT".

  IF CAN-FIND(FIRST bMedlem WHERE
              bMedlem.HovedMedlemsNr = Medlem.MedlemsNr) THEN
  DO:
      MESSAGE "Dette medlemmet er et hovedmedlem og har andre medlemmer " skip
              "koblet til seg. Disse må kobles fra før dette medlemmet kan slettes." SKIP
          VIEW-AS ALERT-BOX INFO BUTTONS OK TITLE "Validering av sletting".
      RETURN NO-APPLY "AVBRYT".
  END.

  find bMedlem no-lock where
    recid(bMedlem) = recid(Medlem) no-error.
  find next bMedlem no-lock no-error.
  if not available bMedlem then
    find first bMedlem.
  if available bMedlem then
    assign
      wNesteRecid = recid(bMedlem).

  assign wOk = false.
  message "Skal Medlemmet slettes?" view-as alert-box 
    QUESTION BUTTONS YES-NO
    title "Bekreftelse"
    update wOk.
  if wOk = false then
    return no-apply "AVBRYT".  
  else do:
    /* Tar bort statistikk */
    FOR EACH stLinje EXCLUSIVE where
             StLinje.StType = "MEDLEM" and
             StLinje.DataObjekt = string(Medlem.MedlemsNr,"9999999999999"):
       DELETE stLinje.
    END.
    /* Medlemskort */
    FOR EACH MedlemsKort OF Medlem EXCLUSIVE-LOCK:
        DELETE MedlemsKort.
    END.
    /* Medlemssaldo */
    FOR EACH MedlemSaldo EXCLUSIVE-LOCK WHERE
        MedlemSaldo.MedlemsNr = Medlem.MedlemsNr:
        DELETE MedlemSaldo.
    END.
    /* Medlemstransaksjoner */
    FOR EACH MedTrans OF Medlem EXCLUSIVE-LOCK:
        DELETE MedTrans.
    END.

    FIND CURRENT Medlem EXCLUSIVE.
    DELETE Medlem.
    IF wModus = "SLETT" THEN
        RETURN "OK".
    assign 
          wInputRecid   = wNesteRecid
          wModus        = "ENDRE".
    FIND Medlem NO-LOCK WHERE RECID(Medlem) = wInputRecid.
    RUN VisPost.
    RUN BUTTONEnaDis.
  end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ButtonEnaDis C-KundKort 
PROCEDURE ButtonEnaDis :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
do with frame DEFAULT-FRAME:  
  assign
    B-AltS:sensitive  = NOT wModus = "Ny" 
    BUTTON-SokeKort:sensitive IN FRAME FRAME-1  = NOT wModus = "Ny" AND wAktivFlip = 1
    BUTTON-SokeKunde:sensitive IN FRAME FRAME-1  = NOT wModus = "Ny" AND wAktivFlip = 1
    BUTTON-Ny:sensitive        = NOT wModus = "Ny" AND wAktivFlip = 1
/* Vi skall inte har kopierknapp - ger konstigt medlemsnummer */
/*     BUTTON-Kopier:sensitive    = NOT wModus = "Ny" AND wAktivFlip = 1 */
    BUTTON-Slett:sensitive     = NOT wModus = "Ny"  AND wAktivFlip = 1
    BUTTON-Prev:sensitive      = NOT wModus = "Ny"
    BUTTON-Next:sensitive      = NOT wModus = "Ny"
    BUTTON-Ok:sensitive        = NOT wModus = "Ny"
    BUTTON-Lagre:sensitive     = wAktivFlip = 1
    BUTTON-Angre:sensitive     = wAktivFlip = 1 AND wInputRecid <> ?
    BUTTON-SokMedlem2:sensitive = wAktivFlip <> 1.
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Bytpost C-KundKort 
PROCEDURE Bytpost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER wRettning AS CHAR NO-UNDO.
  def var wSort as int no-undo.
  assign
    wSort = int(entry(1,CB-Sort:screen-value in frame DEFAULT-FRAME,":")).
  IF Modifierad() THEN
    run LagrePost (0).

  if return-value = "AVBRYT" then
    return no-apply.
  else do:
    assign wModus = "ENDRE".
    
    case wSort:
      when 1 then
        do:
          IF wRettning = "Next" THEN DO:
              find Next Medlem no-lock use-index Medlem no-error.    
              if not available Medlem then
                 find last Medlem no-lock use-index Medlem  no-error.
          END.
          ELSE DO:
              find Prev Medlem no-lock use-index Medlem no-error.    
              if not available Medlem then
                 find First Medlem no-lock use-index Medlem  no-error.
          END.
        end.
      when 2 then
        do:
          IF wRettning = "Next" THEN DO:
              find Next Medlem no-lock use-index ForNavn no-error.    
              if not available Medlem then
                 find last Medlem no-lock use-index ForNavn  no-error.
          END.
          ELSE DO:
              find Prev Medlem no-lock use-index ForNavn no-error.    
              if not available Medlem then
                 find First Medlem no-lock use-index ForNavn  no-error.
          END.
        end.
        when 3 then
          do:
            IF wRettning = "Next" THEN DO:
                find Next Medlem no-lock use-index EtterNavn no-error.    
                if not available Medlem then
                   find last Medlem no-lock use-index EtterNavn  no-error.
            END.
            ELSE DO:
                find Prev Medlem no-lock use-index EtterNavn no-error.    
                if not available Medlem then
                   find First Medlem no-lock use-index EtterNavn  no-error.
            END.
          end.
    end case.  
    if available Medlem then
      do:
        assign
          wInputRecid = recid(Medlem).
        run VisPost.
        RUN ByttFrame.
      end.
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByttFrame C-KundKort 
PROCEDURE ByttFrame :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    IF wAktivFlip <> INT(chTabStrip:SelectedItem:Index) OR 
       INT(chTabStrip:SelectedItem:Index) = 1 THEN
    DO:
      FRAME DEFAULT-FRAME:MOVE-TO-TOP().
      wTabHnd[wAktivFlip]:MOVE-TO-TOP().
    END.
    IF wAktivFlip = 2 THEN 
    DO:
        IF VALID-HANDLE(wSubWin2) THEN
        DO:
          run ByttObjekt in wSubWin2 (recid(Medlem)).  
          RUN MoveToTopp IN wSubWin2.
        END.
        ELSE
          run w-bmedtrans.w persistent set wSubWin2 (recid(Medlem),THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE).  
    END.
    IF wAktivFlip = 3 THEN 
    DO:
        IF VALID-HANDLE(wSubWin3) THEN
        DO:
          run ByttObjekt in wSubWin3 (recid(Medlem)).  
          RUN MoveToTopp IN wSubWin3.
        END.
        ELSE
          run w-bmedtransalle.w persistent set wSubWin3 (recid(Medlem),THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE).  
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByttMedlem C-KundKort 
PROCEDURE ByttMedlem :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER dMedlemsNr LIKE Medlem.MedlemsNr NO-UNDO.

  FIND Medlem NO-LOCK WHERE
      Medlem.MedlemsNr = dMedlemsNr NO-ERROR.
  IF NOT AVAILABLE Medlem THEN DO:
      FIND Medlem WHERE RECID(Medlem) = wInputRecid NO-LOCK NO-ERROR.
      RETURN "AVBRYT".
  END.
  ASSIGN wInputRecid = RECID(ArtBas).
  run ByttFrame. /* For † enable knapper og felt korrekt */ 
  run VisPost.
  PUBLISH "ByttObjekt" (Medlem.MedlemsNr).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load C-KundKort  _CONTROL-LOAD
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

OCXFile = SEARCH( "w-vmedlem.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chTabStrip = TabStrip:COM-HANDLE
    UIB_S = chTabStrip:LoadControls( OCXFile, "TabStrip":U)
    TabStrip:NAME = "TabStrip":U
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "w-vmedlem.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DelTmpChild C-KundKort 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-KundKort  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-KundKort)
  THEN DELETE WIDGET C-KundKort.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-KundKort  _DEFAULT-ENABLE
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
  DISPLAY CB-Sort FILL-IN-MedlemsNr FILL-IN-Navn FI-TotKjop FI-Kunde 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-KundKort.
  ENABLE RECT-27 RECT-28 B-GDPR B-Konv CB-Sort B-Overfor B-OppdatSaldo B-AltS 
         BUTTON-Angre BUTTON-Lagre BUTTON-Next BUTTON-Ny BUTTON-Prev 
         BUTTON-Slett Btn_Help BUTTON-Ok 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-KundKort.
  VIEW FRAME DEFAULT-FRAME IN WINDOW C-KundKort.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW FRAME FRAME-3 IN WINDOW C-KundKort.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-3}
  DISPLAY FILL-IN-11 FI-Hovedmedlem FI-MedlemsKort FI-ForsteKjop FI-SisteKjop 
          FI-Poststed1 FILL-IN-EndretInfo FI-KundeTekst FILL-IN-2 FILL-IN-3 
          FILL-IN-10 
      WITH FRAME FRAME-1 IN WINDOW C-KundKort.
  IF AVAILABLE Butiker THEN 
    DISPLAY Butiker.ButNamn 
      WITH FRAME FRAME-1 IN WINDOW C-KundKort.
  IF AVAILABLE Kunde THEN 
    DISPLAY Kunde.Navn 
      WITH FRAME FRAME-1 IN WINDOW C-KundKort.
  IF AVAILABLE Medlem THEN 
    DISPLAY Medlem.Bonus_Forsendelse Medlem.Bonus_varsel Medlem.Aktiv 
          Medlem.MottaeMailUtsendelser Medlem.MedlemsNr Medlem.HovedMedlemFlagg 
          Medlem.ForNavn Medlem.EtterNavn Medlem.FodselsDato Medlem.FodtAr 
          Medlem.PersonNr Medlem.Adresse1 Medlem.Adresse2 Medlem.PostNr 
          Medlem.BydelsNr Medlem.Land Medlem.ePostAdresse Medlem.Telefon 
          Medlem.MobilTlf Medlem.Telefaks Medlem.AktivertFraWeb 
          Medlem.WebBrukerId Medlem.WebPassord Medlem.MedlemInfo 
          Medlem.Bonus_Berettiget Medlem.Kjonn Medlem.ButikkNr 
          Medlem.HovedMedlemsNr Medlem.MedGruppe Medlem.MedType Medlem.RegKode 
          Medlem.Opphort Medlem.KundeNr Medlem.Kilde Medlem.TilgKilde 
          Medlem.EksterntMedlemsNr Medlem.Rabatt Medlem.MKlubbId 
          Medlem.MedlemNotat 
      WITH FRAME FRAME-1 IN WINDOW C-KundKort.
  IF AVAILABLE MedlemsGruppe THEN 
    DISPLAY MedlemsGruppe.Beskrivelse 
      WITH FRAME FRAME-1 IN WINDOW C-KundKort.
  IF AVAILABLE MedlemsType THEN 
    DISPLAY MedlemsType.Beskrivelse 
      WITH FRAME FRAME-1 IN WINDOW C-KundKort.
  IF AVAILABLE Region THEN 
    DISPLAY Region.Navn 
      WITH FRAME FRAME-1 IN WINDOW C-KundKort.
  ENABLE B-Del B-Spar FILL-IN-11 Medlem.Bonus_Forsendelse Medlem.Bonus_varsel 
         Medlem.Aktiv Medlem.MottaeMailUtsendelser Medlem.HovedMedlemFlagg 
         Medlem.ForNavn Medlem.EtterNavn Medlem.FodselsDato Medlem.PersonNr 
         Medlem.Adresse1 Medlem.Adresse2 Medlem.PostNr Medlem.Land 
         Medlem.ePostAdresse Medlem.Telefon Medlem.MobilTlf Medlem.Telefaks 
         Medlem.AktivertFraWeb Medlem.WebBrukerId Medlem.WebPassord 
         Medlem.MedlemInfo B-SokMedlemsGruppe Medlem.Bonus_Berettiget 
         Medlem.Kjonn Medlem.ButikkNr Medlem.MedGruppe Medlem.MedType 
         B-SokMedlemsType Medlem.RegKode Medlem.Opphort Medlem.Kilde 
         Medlem.TilgKilde B-SokRegion Medlem.EksterntMedlemsNr Medlem.Rabatt 
         Medlem.MKlubbId Medlem.MedlemNotat B-SokPost BUTTON-SokeForsteKjop 
         FILL-IN-EndretInfo FILL-IN-2 FILL-IN-3 BUTTON-SokeKort B-SokButikk 
         FILL-IN-10 BUTTON-SokeKunde BUTTON-SokMedlem RECT-46 
      WITH FRAME FRAME-1 IN WINDOW C-KundKort.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-1}
  VIEW FRAME FRAME-2 IN WINDOW C-KundKort.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-2}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FlyttTrans&Kort C-KundKort 
PROCEDURE FlyttTrans&Kort :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER plMedlemsNr LIKE MEdlem.MedlemsNr NO-UNDO.
  DEF INPUT PARAMETER pcValgListe AS CHAR               NO-UNDO.
  DEF INPUT PARAMETER piValg      AS INT                NO-UNDO.

  DEF VAR piTransNr   AS INT NO-UNDO.
  DEF VAR bMotPostert AS LOG NO-UNDO.

  DEF BUFFER bmedTrans FOR MedTrans.
  DEF BUFFER bKundeBetTrans FOR KundeBetTrans.

  IF NOT AVAILABLE Medlem THEN
      RETURN.
  FIND bMedlem NO-LOCK WHERE
      bMedlem.MedlemsNr = plMedlemsNr NO-ERROR.

  IF NOT AVAILABLE bMedlem THEN
      RETURN.

  /* Flytter kort */
  IF CAN-DO("1,2",STRING(piValg)) THEN
  FOR EACH MedlemsKort EXCLUSIVE-LOCK WHERE
      MedlemsKort.MedlemsNr = Medlem.MedlemsNr:
      ASSIGN
          MedlemsKort.MedlemsNr = bMedlem.MedlemsNr
          .
  END.

  /* Flytter transaksjoner */
  IF CAN-DO("1,3",STRING(piValg)) THEN
  FLYTT-BLOKK:
  DO:
      /* Finner siste TransNr. */
      FIND LAST KundeBetTrans NO-LOCK WHERE
        KundeBetTrans.KundeNr = MedTrans.BongId AND
        KundeBetTrans.Butik   = MedTrans.Butik
        USE-INDEX KundeBetTrans NO-ERROR.
      IF AVAILABLE KundeBetTrans THEN
        piTransNr = KundeBetTrans.TransNr.
      ELSE
        piTransNr = 0.
      RELEASE KundeBetTrans.

      /* Flytter medlemstransaksjonene */
      FLYTT-TRANS:
      FOR EACH MedTrans EXCLUSIVE-LOCK WHERE
          MedTrans.MedlemsNr = Medlem.MedlemsNr AND
          (IF pcValgListe <> ""
             THEN CAN-DO(pcValgListe,string(recid(MedTrans)))
             ELSE TRUE)
          BREAK
          BY MedTrans.MedlemsNr
          BY MedTrans.Butik 
          BY MedTrans.KassaNr 
          BY MedTrans.Dato 
          BY MedTrans.BongId:

          /* Mottagende medlem er koblet til kunde og transaksjonene skal også 
             posteres inn på kunden. */
          IF bMedlem.KundeNr <> 0 THEN
          KUNDE:
          DO:
              FIND Kunde NO-LOCK WHERE
                  Kunde.KundeNr = bMedlem.KundeNr NO-ERROR.
              IF NOT AVAILABLE Kunde THEN
                  LEAVE KUNDE.

              /* Salgstransaksjonene */
              DO ON ERROR UNDO, LEAVE:
                  IF AVAILABLE KundeTrans THEN
                      RELEASE KundeTrans.
                  CREATE KundeTrans.
                  BUFFER-COPY MedTrans TO KundeTrans
                      ASSIGN
                      KundeTrans.KundeNr = Kunde.KundeNr
                      NO-ERROR
                      .
                  IF ERROR-STATUS:ERROR THEN
                  DO:
                      IF AVAILABLE KundeTrans THEN
                          DELETE KundeTrans.
                  END.

              END.

              IF LAST-OF(MedTrans.BongId) THEN
              ERRORBLOKK:
              DO ON ERROR UNDO, LEAVE:
                  FOR EACH BongHode NO-LOCK WHERE
                      BongHode.ButikkNr = MedTrans.Butik AND
                      BongHode.GruppeNr = 1 AND
                      BongHode.KasseNr  = MedTrans.KassaNr AND
                      BongHode.Dato     = MedTrans.Dato AND
                      BongHode.BongNr   = MedTrans.BongId:

                      ASSIGN
                          bMotPostert = TRUE 
                          .
                      /* Sjekker om det er kreditsalg  eller rekvisisjon på bongen. */
                      TRANSRAD-1:
                      FOR EACH BongLinje NO-LOCK WHERE
                          BongLinje.B_Id = BongHode.B_Id:
                          IF CAN-DO("55,65",STRING(BongLinje.TTId,"99")) THEN
                              ASSIGN
                                bMotPostert = FALSE 
                                .
                      END. /* TRANSRAD-1 */

                      /* Legger inn betalingstransaksjonene */
                      FOR EACH BongLinje NO-LOCK WHERE
                          BongLinje.B_Id = BongHode.B_Id AND
                          BongLinje.TTId >= 50 AND
                          BongLinje.Makulert = FALSE:

                          ASSIGN
                              piTransNr = piTransNr + 1
                              .

                          /* Transaksjonen kan ha blitt oppdatert tidligere. */
                          IF CAN-FIND(FIRST bKundeBetTrans NO-LOCK where
                                      bKundeBetTrans.KundeNr     = Kunde.KundeNr      AND
                                      bKundeBetTrans.Butik       = BongLinje.ButikkNr AND
                                      bKundeBetTrans.KassaNr     = BongLinje.KasseNr  AND
                                      bKundeBetTrans.BongId      = BongLinje.BongNr   AND
                                      bKundeBetTrans.BongLinjeNr = BongLinje.LinjeNr  AND
                                      bKundeBetTrans.Dato        = BongLinje.TransDato) THEN
                          DO:
                              NEXT.
                          END.

                          /* Transaksjonen kan ha blitt oppdatert tidligere. */
                          FIND  bKundeBetTrans NO-LOCK where
                                bKundeBetTrans.KundeNr   = Kunde.KundeNr AND
                                bKundeBetTrans.Butik     = BongLinje.ButikkNr    AND
                                bKundeBetTrans.TransNr   = piTransNr  AND
                                bKundeBetTrans.SeqNr     = BongLinje.SeqNr NO-ERROR.
                          DO WHILE AVAILABLE bKundeBetTrans:
                              ASSIGN
                                  piTransNr = piTransNr + 1
                                  .
                              /* Transaksjonen kan ha blitt oppdatert tidligere. */
                              FIND  bKundeBetTrans NO-LOCK where
                                    bKundeBetTrans.KundeNr   = Kunde.KundeNr AND
                                    bKundeBetTrans.Butik     = BongLinje.ButikkNr    AND
                                    bKundeBetTrans.TransNr   = piTransNr  AND
                                    bKundeBetTrans.SeqNr     = BongLinje.SeqNr NO-ERROR.
                          END.

                          IF NOT AVAILABLE bKundeBetTrans THEN
                          POSTERING-TRANS:
                          DO:
                            CREATE bKundeBetTrans.
                            ASSIGN
                                bKundeBetTrans.KundeNr      = Kunde.KundeNr
                                bKundeBetTrans.Butik        = BongLinje.ButikkNr
                                bKundeBetTrans.TransNr      = piTransNr
                                bKundeBetTrans.SeqNr        = BongLinje.SeqNr
                                NO-ERROR
                                .
                            ASSIGN
                              bKundeBetTrans.KassaNr        = BongLinje.KasseNr
                              bKundeBetTrans.Dato           = BongLinje.TransDato
                              bKundeBetTrans.Tid            = BongLinje.TransTid
                              bKundeBetTrans.BongId         = BongLinje.BongNr
                              bKundeBetTrans.BongLinjeNr    = BongLinje.LinjeNr

                              bKundeBetTrans.betButik       = BongLinje.ButikkNr 
                              bKundeBetTrans.betKassaNr     = BongLinje.KasseNr
                              bKundeBetTrans.betBongId      = BongLinje.BongNr

                              bKundeBetTrans.BatchNr        = MedTrans.BatchNr
                              bKundeBetTrans.TTId           = BongLinje.TTId
                              bKundeBetTrans.TBId           = BongLinje.TBId
                              bKundeBetTrans.Belop          = BongLinje.LinjeSum
                              bKundeBetTrans.OrgBelop       = BongLinje.LinjeSum
                              bKundeBetTrans.SelgerNr       = BongHode.SelgerNr
                              bKundeBetTrans.MedlemsNr      = BongHode.MedlemsNr
                              bKundeBetTrans.KortNr         = IF BongHode.KortType = 3 /* Medlemskort */
                                                           THEN BongHode.MedlemsKort
                                                         ELSE IF BongHode.KortType = 2 
                                                           THEN BongHode.KundeKort
                                                         ELSE ""
                              bKundeBetTrans.ForsNr         = BongHode.KassererNr
                              bKundeBetTrans.MotPostert     = bMotPostert
                              NO-ERROR
                              .
                          END. /* POSTERING-TRANS */
                      END.
                  END.
              END. /* ERRORBLOKK */
          END. /* KUNDE */

          ASSIGN
            MedTrans.MedlemsNr = bMedlem.MedlemsNr
            NO-ERROR.

      END. /* FLYTT-TRANS */
  END. /* FLYTT-BLOKK */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitCB C-KundKort 
PROCEDURE InitCB :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR pcTekst AS CHAR NO-UNDO.
    
  assign
      pcTekst = Tx(" 1: Nummer, 2: Etternavn, 3: Fornavn",1)
      pcTekst = (IF NUM-ENTRIES(pcTekst) <> 3
                   THEN " 1: Nummer, 2: Etternavn, 3: Fornavn"
                   ELSE pcTekst)
      .

  DO WITH FRAME Default-Frame:
      ASSIGN
          CB-Sort:LIST-ITEMS = pcTekst
          CB-Sort:SCREEN-VALUE = ENTRY(1,pcTekst)
          .
  END.

  ASSIGN
      pcTekst = ' ,0'.
  FOR EACH MedlemsKlubb NO-LOCK:
      ASSIGN
          pcTekst = pcTekst + 
                    (IF pcTekst = '' THEN '' ELSE ',') + 
                    MedlemsKlubb.MKlubbBeskrivelse + ',' + 
                    STRING(MedlemsKlubb.MKlubbId)
                    .
  END.

  DO WITH FRAME FRAME-1:
      ASSIGN
          Medlem.MKlubbId:LIST-ITEM-PAIRS IN FRAME FRAME-1 = pcTekst
          Medlem.MKlubbId:SCREEN-VALUE IN FRAME FRAME-1 = ENTRY(2,pcTekst)
          .
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initialize-Controls C-KundKort 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LagrePost C-KundKort 
PROCEDURE LagrePost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def input parameter wBekreft as int no-undo.
  def var ipStatus as char INIT "AVBRYT" no-undo.
  DEFINE VARIABLE cTmpModus AS CHARACTER  NO-UNDO.

  DO WITH FRAME FRAME-1:
    IF INPUT Medlem.HovedMedlemsNr <> 0 AND INPUT Medlem.HovedMedlemFlagg THEN
    DO:
      IF INPUT Medlem.MedlemsNr = INPUT Medlem.HovedmedlemsNr THEN
      DO:
        MESSAGE "Du kan ikke koble et medlem til seg selv." SKIP
                "Ugyldig hovedmedlemsnummer. " SKIP
            VIEW-AS ALERT-BOX ERROR BUTTONS OK TITLE "Lagringsfeil".
        RETURN ipStatus.
      END.
    END.

    IF wModus = "NY" THEN DO:
        ASSIGN cTmpModus = wModus.
      /*
      /* Sjekker input */
      if input Medlem.MedlemsNr = 0 then
        do:
          message "Medlemsnummer må være større enn 0"
          view-as alert-box title "Lagringsfeil".
          apply "ENTRY":U to Medlem.MedlemsNr.
          return ipStatus.
        end.
      ELSE if can-find(MEdlem where
        MEdlem.MEdlemsNr = input Medlem.MedlemsNr) then
        do:
          message "Det finnes allerede et medlem med dette medlemsnummer!"
            view-as alert-box message title "Melding".
          apply "ENTRY":U TO MEdlem.MedlemsNr.
          return ipStatus.
        end.
       */
    END.
    /*
    if not can-find(MedlemsType where Medlem.MedType = input Medlem.MedType) then
    do:
      message "Ukjent medlemstype!"
        view-as alert-box message title "Melding".
          apply "ENTRY":U to Medlem.MEdtype.
          return ipStatus.
    end.
    */
    if not can-find(MedlemsGruppe where MEdlemsGruppe.MedGruppe = input Medlem.MedGruppe) then
    do:
      message "Ukjent medlemsgruppegruppe!"
        view-as alert-box message title "Melding".
          apply "ENTRY":U to Medlem.MedGruppe.
          return ipStatus.
    end.
    if not can-find(Post where Post.PostNr = input Medlem.PostNr) then
    do:
      message "Ukjent postnummer på medlemmet!"
        view-as alert-box message title "Melding".
          apply "ENTRY":U to Medlem.PostNr.
          return ipStatus.
    end.  
    
    if not can-find(Butiker where Butiker.Butik = input Medlem.ButikkNr) then
      do:
        message "Ukjent butikknummer på medlemmet!"
          view-as alert-box message title "Melding".
            apply "ENTRY":U to Medlem.ButikkNr.
            return ipStatus.
      end.  
    ELSE
      FIND Butiker NO-LOCK where
           Butiker.butik = INPUT Medlem.ButikkNr NO-ERROR.
    LAGRE_MEDLEM:
      do TRANSACTION:
        if wModus = "NY" then
          do:
            RUN opprettmedlem.p (?, INPUT wKortNummer, INPUT INPUT Medlem.ButikkNr, OUTPUT ipMedlemsNr).
            FIND Medlem EXCLUSIVE-LOCK WHERE
                Medlem.MedlemsNr = ipMedlemsNr NO-ERROR.
            IF NOT AVAILABLE Medlem THEN
                RETURN ipStatus.
            ELSE 
                wInputRecid = RECID(Medlem).

          end.
        else do:
          FIND CURRENT Medlem EXCLUSIVE-LOCK no-error no-wait.
          if locked Medlem then
            do:
                message "Medlemmet oppdateres fra en annen terminal" skip
                  "Forsök å lagre en gang til" view-as alert-box 
               WARNING title "Lagringsfeil".
           return no-apply ipStatus.
          end.
        end.
        assign
            wModus = "ENDRE"
            Medlem.MedlemsNr:SENSITIVE = FALSE
            Medlem.MedlemsNr 
            Medlem.ForNavn 
            Medlem.MedType  
            Medlem.MedGruppe
            Medlem.Adresse1 
            Medlem.Adresse2 
            Medlem.PostNr   
            Medlem.Telefon  
            Medlem.Telefaks 
            Medlem.MobilTlf 
            Medlem.Land     
            Medlem.Opphort  
            Medlem.ButikkNr 
            Medlem.BydelsNr 
            Medlem.ePostAdresse
            Medlem.EtterNavn
            Medlem.Aktiv
            Medlem.MottaeMailUtsendelser
            Medlem.Bonus_Berettiget
            Medlem.Bonus_Varsel
            Medlem.Bonus_Forsendelse
            Medlem.HovedMedlemFlagg 
            Medlem.HovedMedlemsNr   
            Medlem.FodselsDato      
            Medlem.FodtAr           
            Medlem.Kjonn
            Medlem.RegKode
            Medlem.KundeNr
            Medlem.AktivertFraWeb
            Medlem.WebBrukerid
            Medlem.WebPassord
            Medlem.Kilde
            Medlem.TilgKilde
            Medlem.EksterntMedlemsNr
            Medlem.Rabatt
            Medlem.MedlemInfo
            Medlem.MKlubbId
            Medlem.Personnr
            Medlem.MedlemNotat
            .
         IF INPUT Medlem.FodselsDato <> ? THEN
             ASSIGN
               Medlem.FodtAr = YEAR(INPUT Medlem.FodselsDato).
         IF cTmpModus = "NY" THEN
         DO:
             FIND FIRST MedlemsKort WHERE
                 MedlemsKort.MedlemsNr = Medlem.MedlemsNr NO-ERROR.
             IF AVAILABLE MedlemsKort THEN
             KORT:
             DO:
                 ASSIGN
                     MedlemsKort.Innehaver = Medlem.ForNavn:SCREEN-VALUE + " " + 
                                             Medlem.EtterNavn:screen-value
                     MedlemsKort.AktivertDato = TODAY
                     .
             END. /* KORT */
         END.
         FIND CURRENT Medlem NO-LOCK.
         RUN VisPost.
         RUN BUTTONEnaDis.
      end.
  END.
  return "OK".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetSokHovedMedlem C-KundKort 
PROCEDURE SetSokHovedMedlem :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME FRAME-1:
  IF INPUT HovedMedlemFlagg = true THEN
    ASSIGN
      HovedmedlemsNr:SENSITIVE = FALSE
      B-SokHovedMedlem:SENSITIVE = FALSE
      .
  ELSE 
    ASSIGN
      HovedmedlemsNr:SENSITIVE = TRUE
      B-SokHovedMedlem:SENSITIVE = TRUE
      .

END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettModified C-KundKort 
PROCEDURE SettModified :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER wModified AS LOGI NO-UNDO.
  DO WITH FRAME FRAME-1:
    ASSIGN 
        Medlem.MedlemsNr:MODIFIED = wModified 
        Medlem.ForNavn:MODIFIED = wModified 
        Medlem.MedType:MODIFIED = wModified  
        Medlem.MedGruppe:MODIFIED = wModified
        Medlem.Adresse1:MODIFIED = wModified 
        Medlem.Adresse2:MODIFIED = wModified 
        Medlem.PostNr:MODIFIED = wModified   
        Medlem.Telefon:MODIFIED = wModified  
        Medlem.Telefaks:MODIFIED = wModified 
        Medlem.MobilTlf:MODIFIED = wModified 
        Medlem.Land:MODIFIED = wModified     
        Medlem.Opphort:MODIFIED = wModified  
        Medlem.ButikkNr:MODIFIED = wModified 
        Medlem.BydelsNr:MODIFIED = wModified 
        Medlem.ePostAdresse:MODIFIED = wModified
        Medlem.EtterNavn:MODIFIED = wModified
        Medlem.Aktiv:MODIFIED = wModified 
        Medlem.MottaeMailUtsendelser:MODIFIED = wModified
        Medlem.Bonus_Berettiget:MODIFIED = wModified
        Medlem.Bonus_Varsel:MODIFIED = wModified
        Medlem.Bonus_Forsendelse:MODIFIED = wModified
        Medlem.HovedMedlemFlagg:MODIFIED = wModified 
        Medlem.HovedMedlemsNr:MODIFIED = wModified   
        Medlem.FodselsDato:MODIFIED = wModified      
        Medlem.FodtAr:MODIFIED = wModified           
        Medlem.Kjonn:MODIFIED = wModified   
        Medlem.RegKode:MODIFIED = wModified 
        Medlem.Kunde:MODIFIED = wModified 
        Medlem.Kilde:MODIFIED = wModified
        Medlem.TilgKilde:MODIFIED = wModified
        Medlem.EksterntMedlemsNr:MODIFIED = wModified
        Medlem.Rabatt:MODIFIED = wModified
      .
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapaTmpChild C-KundKort 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettProg C-KundKort 
PROCEDURE SlettProg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  apply "CLOSE":U to this-procedure. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisEndretInfo C-KundKort 
PROCEDURE VisEndretInfo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  if available Medlem then
    do:
      assign
        Fill-In-EndretInfo = "Opprettet " + 
                             (if Medlem.RegistrertDato <> ? 
                               then string(Medlem.RegistrertDato)
                               else "        ") + " " +
                             (if Medlem.RegistrertTid <> 0
                               then string(Medlem.RegistrertTid,"HH:MM:SS")
                               else "        ") + " av " + 
                             Medlem.RegistrertAv + "    Endret " +
                             (if Medlem.EDato <> ?
                               then string(Medlem.EDato)
                               else "        ") + " " +
                             (if Medlem.ETid <> 0
                               then string(Medlem.ETid,"HH:MM:SS")
                               else "        ") + " av " +
                             Medlem.BrukerId.
    end.
  else do:
    assign
      FILL-IN-EndretInfo = "".
  end.
  display 
    FILL-IN-EndretInfo
  with frame FRAME-1.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisMedlemSaldo C-KundKort 
PROCEDURE VisMedlemSaldo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME Frame-1:
  
  IF NOT AVAILABLE Medlem THEN
      ASSIGN
        FI-ForsteKjop:SCREEN-VALUE = ""
        FI-SisteKjop:SCREEN-VALUE = ""
        .
  FIND first MedlemSaldo NO-LOCK WHERE
      MedlemSaldo.MedlemsNr = Medlem.MedlemsNr USE-INDEX Forste NO-ERROR.
  IF AVAILABLE MedlemSaldo THEN
      ASSIGN
        FI-ForsteKjop:SCREEN-VALUE = string(MedlemSaldo.ForsteDato).
        .
  FIND LAST MedlemSaldo NO-LOCK WHERE
      MedlemSaldo.MedlemsNr = Medlem.MedlemsNr USE-INDEX Siste NO-ERROR.
  IF AVAILABLE MedlemSaldo THEN
      ASSIGN
        FI-SisteKjop:SCREEN-VALUE = string(MedlemSaldo.DatoSiste).
        .
END. /* FrameScoop */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisMedlemsKort C-KundKort 
PROCEDURE VisMedlemsKort :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME FRAME-1:
  IF NOT AVAILABLE Medlem THEN
      RETURN.
  FIND FIRST MedlemsKort OF Medlem NO-LOCK NO-ERROR.
  IF NOT AVAILABLE MedlemsKort THEN
      ASSIGN
        FI-MedlemsKort:SCREEN-VALUE = "".
  ELSE
      ASSIGN
        FI-MedlemsKort:SCREEN-VALUE = MedlemsKort.KortNr.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisPost C-KundKort 
PROCEDURE VisPost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME DEFAULT-FRAME:
    ASSIGN FILL-IN-MedlemsNr:SCREEN-VALUE = STRING(Medlem.MedlemsNr)
           FILL-IN-Navn:SCREEN-VALUE = Medlem.Fornavn + " " + Medlem.EtterNavn.
  END.

  find Post where Post.PostNr = Medlem.PostNr no-lock no-error.
  ASSIGN FI-PostSted1 = IF AVAIL Post THEN Post.Beskrivelse ELSE "".
  FIND MedlemsType OF Medlem NO-LOCK NO-ERROR.
  FIND MedlemsGruppe OF Medlem NO-LOCK NO-ERROR.
  FIND Region OF Medlem NO-LOCK NO-ERROR.
  FIND Butiker NO-LOCK WHERE
      Butiker.Butik = Medlem.ButikkNr NO-ERROR.
  FIND Kunde NO-LOCK WHERE
      Kunde.KundeNr = MEdlem.KundeNr NO-ERROR.
  FIND bMedlem NO-LOCK WHERE
      bMedlem.MedlemsNr = Medlem.HovedMedlemsNr NO-ERROR.
  IF AVAILABLE Kunde THEN
      DISPLAY
        STRING(Kunde.KundeNr) + "  " + Kunde.Navn @ FI-Kunde
      WITH FRAME Default-frame.
  ELSE
      DISPLAY
        "" @ FI-Kunde
      WITH FRAME Default-frame.

  DO WITH FRAME FRAME-1:
      DISPLAY 
          FI-SisteKjop
          FI-ForsteKjop
          (IF AVAIL MedlemsType THEN MedlemsType.Beskrivelse ELSE "") @ MedlemsType.Beskrivelse 
          (IF AVAIL MedlemsGruppe THEN MedlemsGruppe.Beskrivelse ELSE "") @ MedlemsGruppe.Beskrivelse 
          Butiker.ButNamn WHEN AVAILABLE Butiker 
          Medlem.MedlemsNr 
          Medlem.ForNavn 
          Medlem.MedType  
          Medlem.MedGruppe
          Medlem.Adresse1 
          Medlem.Adresse2 
          Medlem.PostNr   
          Medlem.Telefon  
          Medlem.Telefaks 
          Medlem.MobilTlf 
          Medlem.Land     
          Medlem.Opphort  
          Medlem.ButikkNr 
          Medlem.BydelsNr 
          Medlem.ePostAdresse
          Medlem.EtterNavn
          Medlem.Aktiv
          Medlem.MottaeMailUtsendelser
          Medlem.Bonus_Berettiget
          Medlem.Bonus_Varsel
          Medlem.Bonus_Forsendelse
          Medlem.HovedMedlemFlagg 
          Medlem.HovedMedlemsNr   
          Medlem.FodselsDato      
          Medlem.FodtAr           
          Medlem.Kjonn   
          Medlem.RegKode 
          Medlem.KundeNr
          Medlem.AktivertFraWeb
          Medlem.WebBrukerId
          Medlem.WebPassord
          Medlem.Kilde
          Medlem.TilgKilde
          Medlem.EksterntMedlemsNr
          Medlem.Rabatt
          Medlem.MKlubbId
          Medlem.Personnr
          Medlem.MedlemNotat
          "" WHEN Medlem.KundeNr = 0 @ Medlem.KundeNr
          "" @ Kunde.Navn
          Kunde.Navn WHEN AVAILABLE Kunde
          Medlem.MedlemInfo
          .
          IF AVAILABLE bMedlem THEN
              DISPLAY 
                bMedlem.Fornavn + " "  + bMedlem.EtterNavn @ FI-Hovedmedlem
              .
          ELSE 
              DISPLAY
                "" @ FI-HovedMedlem
              .
      FIND Post NO-LOCK WHERE
          Post.PostNr = Medlem.PostNr NO-ERROR.
      IF NOT AVAILABLE Post THEN
          ASSIGN
            FI-PostSted1:SCREEN-VALUE   = ""
            Medlem.BydelsNr:SCREEN-VALUE = "".
      ELSE
          ASSIGN
            FI-PostSted1:SCREEN-VALUE   = Post.Beskrivelse
            Medlem.BydelsNr:SCREEN-VALUE = substring(Medlem.PostNr,1,3).

      RUN SetSokHovedMedlem.
  END.
  RUN VisEndretInfo.
  RUN SettModified(FALSE).
  RUN VisMedlemSaldo.
  RUN VisSaldo.
  RUN VisMedlemsKort.
  IF Medlem.FodselsDato <> ? THEN
      ASSIGN
        Medlem.FodtAr:SENSITIVE = FALSE.
  ELSE Medlem.FodtAr:SENSITIVE = TRUE.
  IF wAktivFlip = 1 THEN
      APPLY "ENTRY" TO Medlem.FORNavn IN FRAME FRAME-1.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisSaldo C-KundKort 
PROCEDURE VisSaldo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR wTotKjop AS DEC FORMAT "->>,>>>,>>>,>>9.99" NO-UNDO.

    
DO WITH FRAME Default-Frame:
    IF NOT AVAILABLE Medlem THEN
        ASSIGN
          FI-TotKjop:SCREEN-VALUE = ""
        .
    ASSIGN
        wTotKjop = 0.
    FOR EACH MedlemSaldo NO-LOCK WHERE
        MedlemSaldo.MedlemsNr = Medlem.MedlemsNr:
        ASSIGN
            wTotKjop = wTotKjop + MedlemSaldo.TotaltKjop.
    END.
    ASSIGN
      FI-TotKjop:SCREEN-VALUE = string(wTotKjop)
      .

END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WinHlp C-KundKort 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Modifierad C-KundKort 
FUNCTION Modifierad RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DO WITH FRAME FRAME-1:
    RETURN IF
        Medlem.MedlemsNr:MODIFIED = TRUE OR 
        Medlem.ForNavn:MODIFIED = TRUE OR 
        Medlem.MedType:MODIFIED = TRUE OR  
        Medlem.MedGruppe:MODIFIED = TRUE OR
        Medlem.Adresse1:MODIFIED = TRUE OR 
        Medlem.Adresse2:MODIFIED = TRUE OR 
        Medlem.PostNr:MODIFIED = TRUE OR   
        Medlem.Telefon:MODIFIED = TRUE OR  
        Medlem.Telefaks:MODIFIED = TRUE OR 
        Medlem.MobilTlf:MODIFIED = TRUE OR 
        Medlem.Land:MODIFIED = TRUE OR     
        Medlem.Opphort:MODIFIED = TRUE OR  
        Medlem.ButikkNr:MODIFIED = TRUE OR 
        Medlem.BydelsNr:MODIFIED = TRUE OR 
        Medlem.ePostAdresse:MODIFIED = TRUE OR
        Medlem.EtterNavn:MODIFIED = TRUE OR
        Medlem.Aktiv:MODIFIED = TRUE OR 
        Medlem.Bonus_Berettiget:MODIFIED = TRUE OR 
        Medlem.Bonus_Varsel:MODIFIED = TRUE OR
        Medlem.Bonus_Forsendelse:MODIFIED = TRUE OR
        Medlem.HovedMedlemFlagg:MODIFIED = TRUE OR 
        Medlem.HovedMedlemsNr:MODIFIED = TRUE OR   
        Medlem.FodselsDato:MODIFIED = TRUE OR      
        Medlem.FodtAr:MODIFIED = TRUE OR           
        Medlem.Kjonn:MODIFIED = TRUE OR   
        Medlem.Kilde:MODIFIED = TRUE OR
        Medlem.TilgKilde:MODIFIED = TRUE OR
        Medlem.EksterntMedlemsNr:MODIFIED = TRUE OR
        Medlem.Rabatt:MODIFIED = TRUE OR
        Medlem.RegKode:MODIFIED = TRUE OR 
        Medlem.Personnr:MODIFIED = TRUE OR
        Medlem.MedlemNotat:MODIFIED = TRUE OR
        Medlem.MedlemInfo:MODIFIED = TRUE
        THEN TRUE ELSE FALSE 
        .
  END.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

