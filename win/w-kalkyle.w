&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME C-Kalkyle


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE TT_Prisprofil NO-UNDO LIKE Prisprofil
       FIELD Inpris AS DECI FORMAT ">>,>>9.99"
       FIELD Pris as DECI FORMAT ">>,>>9.99"
       FIELD KalkFinns AS LOGI
       FIELD Tilbud as LOGI FORMAT "J/N"
       .



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Kalkyle 
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
DEF INPUT PARAMETER wArtikkelNr   LIKE ArtBas.ArtikkelNr NO-UNDO.
DEF INPUT PARAMETER wCurrent-Window    AS HANDLE NO-UNDO.
DEF INPUT PARAMETER wParentHandle      AS HANDLE NO-UNDO.
DEF INPUT PARAMETER wh_PrisKo          AS HANDLE NO-UNDO.
DEF INPUT PARAMETER wModus             AS CHAR   NO-UNDO.

/* Local Variable Definitions ---                                       */
DEF VAR wDirektePrisOppdat AS LOG    NO-UNDO.
DEF VAR wSjekkStreng       AS CHAR   NO-UNDO.
DEF VAR wArtBasRecid       AS RECID  NO-UNDO.
DEF VAR wCl                AS INT    NO-UNDO.
DEF VAR w2Type             AS CHAR   NO-UNDO.
DEF VAR wType              AS CHAR   NO-UNDO.
DEF VAR wETid              AS CHAR   FORMAT "x(10)".
DEF VAR w2Etid             AS CHAR   FORMAT "x(10)".
DEF VAR wAktTid            AS CHAR   FORMAT "x(10)".
DEF VAR wTilTid            AS CHAR   FORMAT "x(10)".
DEF VAR wOldSkjerm         AS CHAR   NO-UNDO.
DEF VAR wFrameHandle       AS HANDLE NO-UNDO.
DEF VAR wMaksDB%           AS DEC    NO-UNDO.
DEF VAR wOk                AS LOG    NO-UNDO.
DEF VAR iOldProfilNr       AS INT    NO-UNDO.
DEF VAR cTekst             AS CHAR   NO-UNDO.
/* DEF VAR iTmpProfilNr       AS INT    NO-UNDO. */

DEF VAR wStorlekar         AS CHAR   NO-UNDO.
DEF VAR wBrGrpNr           LIKE Bruker.BrGrpNr NO-UNDO.
DEF VAR wLapTop            AS LOG    NO-UNDO.
DEF VAR wOpprettBestilling AS INT    NO-UNDO.
DEF VAR h_PrisKo           AS HANDLE NO-UNDO.
DEF VAR lOK                AS LOG    NO-UNDO.
DEF VAR lMaxPris           AS DEC    NO-UNDO.
DEFINE VARIABLE iFrameYpos AS INTEGER    NO-UNDO.
DEFINE VARIABLE lNybestilling AS LOGICAL    NO-UNDO. /* flyttat opprettbestilling till ny proc */
DEFINE VARIABLE hSourceP AS HANDLE     NO-UNDO.
DEF BUFFER clButiker FOR Butiker.
DEF BUFFER bufArtBas FOR ArtBas.
DEFINE BUFFER bufPrisprofil FOR Prisprofil.
DEFINE BUFFER bufArtPris FOR ArtPris.
DEFINE VARIABLE cNyaProfiler AS CHAR.
DEFINE VARIABLE dTilBudFra AS DATETIME    NO-UNDO.
DEFINE VARIABLE dTilBudTil AS DATETIME    NO-UNDO.
DEFINE VARIABLE dPgmStart  AS DATE        NO-UNDO.
{runlib.i}

CREATE WIDGET-POOL.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-PrisInfo
&Scoped-define BROWSE-NAME BROWSE-HPrisKo

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES HPrisKo TT_Prisprofil PrisKo ArtPris

/* Definitions for BROWSE BROWSE-HPrisKo                                */
&Scoped-define FIELDS-IN-QUERY-BROWSE-HPrisKo wType HPrisKo.AktiveresDato ~
wAktTid HPrisKo.Pris HPrisKo.VareKost HPrisKo.GyldigTilDato wTilTid ~
HPrisKo.BrukerID HPrisKo.EDato wETid 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-HPrisKo 
&Scoped-define QUERY-STRING-BROWSE-HPrisKo FOR EACH HPrisKo ~
      WHERE HPrisKo.ArtikkelNr = ArtBas.ArtikkelNr and ~
HPrisKo.ProfilNr   = FI-ProfilNr NO-LOCK
&Scoped-define OPEN-QUERY-BROWSE-HPrisKo OPEN QUERY BROWSE-HPrisKo FOR EACH HPrisKo ~
      WHERE HPrisKo.ArtikkelNr = ArtBas.ArtikkelNr and ~
HPrisKo.ProfilNr   = FI-ProfilNr NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BROWSE-HPrisKo HPrisKo
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-HPrisKo HPrisKo


/* Definitions for BROWSE BROWSE-KalkProfil                             */
&Scoped-define FIELDS-IN-QUERY-BROWSE-KalkProfil TT_Prisprofil.ProfilNr ~
TT_Prisprofil.Tilbud TT_Prisprofil.Inpris TT_Prisprofil.Pris 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-KalkProfil 
&Scoped-define QUERY-STRING-BROWSE-KalkProfil FOR EACH TT_Prisprofil ~
      WHERE TT_Prisprofil.Kalkfinns = TRUE NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-KalkProfil OPEN QUERY BROWSE-KalkProfil FOR EACH TT_Prisprofil ~
      WHERE TT_Prisprofil.Kalkfinns = TRUE NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-KalkProfil TT_Prisprofil
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-KalkProfil TT_Prisprofil


/* Definitions for BROWSE BROWSE-PrisKo                                 */
&Scoped-define FIELDS-IN-QUERY-BROWSE-PrisKo PrisKo.ProfilNr w2Type ~
PrisKo.EtikettStatus PrisKo.KlargjorStatus PrisKo.AktiveresDato wAktTid ~
PrisKo.Pris PrisKo.VareKost PrisKo.GyldigTilDato wTilTid PrisKo.BrukerID ~
PrisKo.EDato w2ETid 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-PrisKo 
&Scoped-define QUERY-STRING-BROWSE-PrisKo FOR EACH PrisKo ~
      WHERE PrisKo.ArtikkelNr = ArtBas.ArtikkelNr and ~
PrisKo.ProfilNr   = FI-ProfilNr NO-LOCK
&Scoped-define OPEN-QUERY-BROWSE-PrisKo OPEN QUERY BROWSE-PrisKo FOR EACH PrisKo ~
      WHERE PrisKo.ArtikkelNr = ArtBas.ArtikkelNr and ~
PrisKo.ProfilNr   = FI-ProfilNr NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BROWSE-PrisKo PrisKo
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-PrisKo PrisKo


/* Definitions for BROWSE BROWSE-PrisKoAlle                             */
&Scoped-define FIELDS-IN-QUERY-BROWSE-PrisKoAlle PrisKo.ProfilNr w2Type ~
PrisKo.EtikettStatus PrisKo.KlargjorStatus PrisKo.AktiveresDato wAktTid ~
PrisKo.Pris PrisKo.VareKost PrisKo.GyldigTilDato wTilTid PrisKo.BrukerID ~
PrisKo.EDato w2ETid 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-PrisKoAlle 
&Scoped-define QUERY-STRING-BROWSE-PrisKoAlle FOR EACH PrisKo ~
      WHERE PrisKo.ArtikkelNr = ArtBas.ArtikkelNr NO-LOCK ~
    BY PrisKo.ArtikkelNr ~
       BY PrisKo.ProfilNr ~
        BY PrisKo.AktiveresDato ~
         BY PrisKo.AktiveresTid
&Scoped-define OPEN-QUERY-BROWSE-PrisKoAlle OPEN QUERY BROWSE-PrisKoAlle FOR EACH PrisKo ~
      WHERE PrisKo.ArtikkelNr = ArtBas.ArtikkelNr NO-LOCK ~
    BY PrisKo.ArtikkelNr ~
       BY PrisKo.ProfilNr ~
        BY PrisKo.AktiveresDato ~
         BY PrisKo.AktiveresTid.
&Scoped-define TABLES-IN-QUERY-BROWSE-PrisKoAlle PrisKo
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-PrisKoAlle PrisKo


/* Definitions for BROWSE BROWSE-Prisprofil                             */
&Scoped-define FIELDS-IN-QUERY-BROWSE-Prisprofil ArtPris.ProfilNr ~
ArtPris.AktivFraDato wAkttid ArtPris.Pris[1] ArtPris.VareKost[1] ~
ArtPris.DB%[1] ArtPris.EDato wETid ArtPris.BrukerID ArtPris.ArtikkelNr 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-Prisprofil 
&Scoped-define QUERY-STRING-BROWSE-Prisprofil FOR EACH ArtPris ~
      WHERE ArtPris.ArtikkelNr = ArtBas.ArtikkelNr NO-LOCK
&Scoped-define OPEN-QUERY-BROWSE-Prisprofil OPEN QUERY BROWSE-Prisprofil FOR EACH ArtPris ~
      WHERE ArtPris.ArtikkelNr = ArtBas.ArtikkelNr NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BROWSE-Prisprofil ArtPris
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-Prisprofil ArtPris


/* Definitions for FRAME FRAME-PrisInfo                                 */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-PrisInfo ~
    ~{&OPEN-QUERY-BROWSE-HPrisKo}~
    ~{&OPEN-QUERY-BROWSE-KalkProfil}~
    ~{&OPEN-QUERY-BROWSE-PrisKo}~
    ~{&OPEN-QUERY-BROWSE-PrisKoAlle}~
    ~{&OPEN-QUERY-BROWSE-Prisprofil}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS ArtPris.MengdeRabAntall ArtPris.MengdeRabPris 
&Scoped-define ENABLED-TABLES ArtPris
&Scoped-define FIRST-ENABLED-TABLE ArtPris
&Scoped-Define ENABLED-OBJECTS BROWSE-KalkProfil B-SokValuta B-Tilbakestill ~
B-SupRab FI-Kroner-1 FI-Prosent-1 FI-Valuta-1 RS-Liste B-EndreDato ~
B-SlettPrisKo FI-ProfilNr FI-ValPris FI-Rab1 FI-Rab1% FI-Rab2 FI-Rab2% ~
FI-Frakt FI-Frakt% FI-DivKost FI-DivKost% FI-Rab3 FI-Rab3% FI-DB FI-DB% ~
FI-Pris RS-VisKalkyle CB-Tilbud FI-NAktFra FI-NTid BROWSE-PrisKo FI-AktFra ~
FI-Tid1 FI-AktTil FI-Tid2 B-Godkjenn B-Aktiver BROWSE-HPrisKo T-Manuel ~
FI-ToppTekst FI-ToppTekst-2 FI-ToppTekst-3 FI-ToppTekst-4 FI-Kroner-2 ~
FI-Prosent-2 btnNAktFra btnAktFra btnAktTil B-GodkjennKopier B-ForhRab ~
B-Katalogpris B-VeilPris FI-ToppTekst-5 B-NyKalkyle FI-ToppTekst-6 ~
FI-ToppTekst-7 B-EtiSimuler FI-MengdeRabTekst RECT-36 RECT-37 RECT-38 ~
RECT-39 RECT-40 RECT-41 RECT-42 RECT-43 RECT-44 RECT-56 RECT-62 ~
BROWSE-Prisprofil RECT-72 BROWSE-PrisKoAlle 
&Scoped-Define DISPLAYED-FIELDS ArtPris.MengdeRabAntall ~
ArtPris.MengdeRabPris 
&Scoped-define DISPLAYED-TABLES ArtPris
&Scoped-define FIRST-DISPLAYED-TABLE ArtPris
&Scoped-Define DISPLAYED-OBJECTS FI-ValKod FI-Kroner-1 FI-Prosent-1 ~
FI-Valuta-1 RS-Liste FI-ProfilNr FI-Beskrivelse FI-AktFra-2 FI-AktTil-2 ~
FI-ValPris FI-InnPris FI-InnPris-2 FI-Rab1 FI-Rab1% FI-Rab1-2 FI-Rab1%-2 ~
FI-Rab2 FI-Rab2% FI-Rab2-2 FI-Rab2%-2 FI-Frakt FI-Frakt% FI-Frakt-2 ~
FI-Frakt%-2 FI-DivKost FI-DivKost% FI-DivKost-2 FI-DivKost%-2 FI-Rab3 ~
FI-Rab3% FI-Rab3-2 FI-Rab3%-2 FI-VareKost FI-VareKost-2 FI-DB FI-DB% ~
FI-DB-2 FI-DB%-2 FI-MVA FI-Mva% FI-MVA-2 FI-Mva%-2 FI-Pris RS-VisKalkyle ~
FI-Pris-2 FI-EuPris CB-Tilbud FI-NAktFra FI-NTid FI-AktFra FI-Tid1 ~
FI-AktTil FI-Tid2 FI-EuPris-2 T-Manuel FI-VisPrisStatus FI-ToppTekst ~
FI-ToppTekst-2 FI-ToppTekst-3 FI-Txt2 FI-Txt1 FI-ToppTekst-4 FI-Kroner-2 ~
FI-Prosent-2 FI-KatalogPris FI-AnbefaltPris FI-forhRab% FI-supRab% ~
FI-VPIDato FI-ToppTekst-5 FI-ToppTekst-6 FI-ToppTekst-7 FI-MengdeRabTekst 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD feilIkalkyle C-Kalkyle 
FUNCTION feilIkalkyle RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD KalkStreng C-Kalkyle 
FUNCTION KalkStreng RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Kalkyle AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE MENU POPUP-MENU-BROWSE-ArtPris 
       MENU-ITEM m_Ta_bort_prisendring LABEL "Ta bort prisendring".

DEFINE MENU POPUP-MENU-BROWSE-ArtPris-2 
       MENU-ITEM m_Ta_bort_prisendring-2 LABEL "Ta bort prisendring".


/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Aktiver 
     LABEL "Aktiver.." 
     SIZE 13 BY 1.14.

DEFINE BUTTON B-EndreDato 
     LABEL "Endre dato og pris..." 
     SIZE 20 BY 1.14.

DEFINE BUTTON B-EtiSimuler 
     LABEL "Etikett simulering" 
     SIZE 18.2 BY 1.14.

DEFINE BUTTON B-ForhRab 
     LABEL "Velg" 
     SIZE 6 BY 1.14.

DEFINE BUTTON B-Godkjenn 
     LABEL "G&odkjenn pris" 
     SIZE 18.6 BY 1.14.

DEFINE BUTTON B-GodkjennKopier 
     LABEL "Godkjenn/&Kopier" 
     SIZE 18.6 BY 1.14.

DEFINE BUTTON B-Katalogpris 
     LABEL "Velg" 
     SIZE 6 BY 1.14.

DEFINE BUTTON B-NyKalkyle 
     LABEL "Ny kalkyle/prisprofil" 
     SIZE 22 BY 1.

DEFINE BUTTON B-SlettArtPris  NO-FOCUS
     LABEL "Slett" 
     SIZE 13.6 BY 1.14.

DEFINE BUTTON B-SlettPrisKo 
     LABEL "Slett..." 
     SIZE 12 BY 1.14.

DEFINE BUTTON B-SokValuta 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-SupRab 
     LABEL "Velg" 
     SIZE 6 BY 1.14.

DEFINE BUTTON B-Tilbakestill  NO-FOCUS
     LABEL "&Tilbakestill" 
     SIZE 18.6 BY 1.14.

DEFINE BUTTON B-VeilPris 
     LABEL "Velg" 
     SIZE 6 BY 1.14.

DEFINE BUTTON btnAktFra 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnAktTil 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnNAktFra 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE CB-Tilbud AS INTEGER FORMAT "9":U INITIAL 1 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "Normal",1,
                     "Tilbud",2,
                     "Endre tilbud",3,
                     "Lev.kampanje",5
     DROP-DOWN-LIST
     SIZE 19 BY 1
     FONT 4 NO-UNDO.

DEFINE VARIABLE FI-AktFra AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 12.4 BY 1 NO-UNDO.

DEFINE VARIABLE FI-AktFra-2 AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE FI-AktTil AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 12.4 BY 1 NO-UNDO.

DEFINE VARIABLE FI-AktTil-2 AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE FI-AnbefaltPris LIKE ArtBas.AnbefaltPris
     LABEL "Veil.pris" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Beskrivelse AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE FI-DB AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "D&B (+)" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-DB% AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-DB%-2 AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-DB-2 AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "DB (+)" 
     VIEW-AS FILL-IN 
     SIZE 15.2 BY 1 NO-UNDO.

DEFINE VARIABLE FI-DivKost AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Div. kost (+)" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-DivKost% AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-DivKost%-2 AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-DivKost-2 AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Div. kost (+)" 
     VIEW-AS FILL-IN 
     SIZE 15.2 BY 1 NO-UNDO.

DEFINE VARIABLE FI-EuPris AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Pris (Euro)" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-EuPris-2 AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Pris (Euro)" 
     VIEW-AS FILL-IN 
     SIZE 15.2 BY 1 NO-UNDO.

DEFINE VARIABLE FI-forhRab% LIKE ArtBas.forhRab%
     LABEL "Forhåndsrab%" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Frakt AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Frakt (+)" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Frakt% AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Frakt%-2 AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Frakt-2 AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Frakt (+)" 
     VIEW-AS FILL-IN 
     SIZE 15.2 BY 1 NO-UNDO.

DEFINE VARIABLE FI-InnPris AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Inkjøpspris" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-InnPris-2 AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Inkjøpspris" 
     VIEW-AS FILL-IN 
     SIZE 15.2 BY 1 NO-UNDO.

DEFINE VARIABLE FI-KatalogPris LIKE ArtBas.KatalogPris
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Kroner-1 AS CHARACTER FORMAT "X(256)":U INITIAL "Kroner" 
      VIEW-AS TEXT 
     SIZE 15 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-Kroner-2 AS CHARACTER FORMAT "X(256)":U INITIAL "Kroner" 
      VIEW-AS TEXT 
     SIZE 14.6 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-MengdeRabTekst AS CHARACTER FORMAT "X(256)":U INITIAL " Mengderabatt" 
      VIEW-AS TEXT 
     SIZE 52 BY .62
     BGCOLOR 9 FGCOLOR 15 FONT 6 NO-UNDO.

DEFINE VARIABLE FI-MVA AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Mva (+)" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Mva% AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Mva%-2 AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-MVA-2 AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Mva (+)" 
     VIEW-AS FILL-IN 
     SIZE 15.2 BY 1 NO-UNDO.

DEFINE VARIABLE FI-NAktFra AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 12.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-NTid AS DECIMAL FORMAT "99.99":U INITIAL 0 
     LABEL "Kl" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Pris AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "&Pris" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Pris-2 AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Pris" 
     VIEW-AS FILL-IN 
     SIZE 15.2 BY 1 NO-UNDO.

DEFINE VARIABLE FI-ProfilNr AS INTEGER FORMAT ">>>>>>9":U INITIAL 0 
     LABEL "Prisprofil" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Prosent-1 AS CHARACTER FORMAT "X(256)":U INITIAL "Prosent" 
      VIEW-AS TEXT 
     SIZE 14 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-Prosent-2 AS CHARACTER FORMAT "X(256)":U INITIAL "Prosent" 
      VIEW-AS TEXT 
     SIZE 13.8 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-Rab1 AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Rabatt 1 (-)" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Rab1% AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Rab1%-2 AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Rab1-2 AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Rabatt 1 (-)" 
     VIEW-AS FILL-IN 
     SIZE 15.2 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Rab2 AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Rabatt 2 (-)" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Rab2% AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Rab2%-2 AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Rab2-2 AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Rabatt 2 (-)" 
     VIEW-AS FILL-IN 
     SIZE 15.2 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Rab3 AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Rabatt 3 (-)" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Rab3% AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Rab3%-2 AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Rab3-2 AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Rabatt 3 (-)" 
     VIEW-AS FILL-IN 
     SIZE 15.2 BY 1 NO-UNDO.

DEFINE VARIABLE FI-supRab% LIKE ArtBas.supRab%
     LABEL "Suppl.rab%" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Tid1 AS DECIMAL FORMAT "99.99":U INITIAL 23.59 
     LABEL "Kl" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Tid2 AS DECIMAL FORMAT "99.99":U INITIAL 0 
     LABEL "Kl" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE FI-ToppTekst AS CHARACTER FORMAT "X(256)":U INITIAL "  Ordinær kalkyle" 
      VIEW-AS TEXT 
     SIZE 64 BY .62
     BGCOLOR 9 FGCOLOR 15 FONT 6 NO-UNDO.

DEFINE VARIABLE FI-ToppTekst-2 AS CHARACTER FORMAT "X(256)":U INITIAL "  Siste ordinær kalkyle" 
      VIEW-AS TEXT 
     SIZE 53 BY .62
     BGCOLOR 9 FGCOLOR 15 FONT 6 NO-UNDO.

DEFINE VARIABLE FI-ToppTekst-3 AS CHARACTER FORMAT "X(256)":U INITIAL "  Priskø for valgt prisprofil" 
      VIEW-AS TEXT 
     SIZE 84 BY .62
     BGCOLOR 9 FGCOLOR 15 FONT 6 NO-UNDO.

DEFINE VARIABLE FI-ToppTekst-4 AS CHARACTER FORMAT "X(256)":U INITIAL " Prishistorikk" 
      VIEW-AS TEXT 
     SIZE 84 BY .62
     BGCOLOR 9 FGCOLOR 15 FONT 6 NO-UNDO.

DEFINE VARIABLE FI-ToppTekst-5 AS CHARACTER FORMAT "X(256)":U INITIAL "  VPI informasjon" 
      VIEW-AS TEXT 
     SIZE 64 BY .62
     BGCOLOR 9 FGCOLOR 15 FONT 6 NO-UNDO.

DEFINE VARIABLE FI-ToppTekst-6 AS CHARACTER FORMAT "X(256)":U INITIAL " Prisprofil" 
      VIEW-AS TEXT 
     SIZE 84 BY .62
     BGCOLOR 9 FGCOLOR 15 FONT 6 NO-UNDO.

DEFINE VARIABLE FI-ToppTekst-7 AS CHARACTER FORMAT "X(256)":U INITIAL " Prisprofil (Alle profiler)" 
      VIEW-AS TEXT 
     SIZE 84 BY .62
     BGCOLOR 9 FGCOLOR 15 FONT 6 NO-UNDO.

DEFINE VARIABLE FI-Txt1 AS CHARACTER FORMAT "X(256)":U INITIAL "Tilbud" 
      VIEW-AS TEXT 
     SIZE 11.8 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-Txt2 AS CHARACTER FORMAT "X(256)":U INITIAL "Normalpris" 
      VIEW-AS TEXT 
     SIZE 13.8 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-ValKod AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN NATIVE 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE FI-ValPris AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Valutapris" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Valuta-1 AS CHARACTER FORMAT "X(256)":U INITIAL "Valuta" 
      VIEW-AS TEXT 
     SIZE 14 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-VareKost AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Varekost" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-VareKost-2 AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Varekost" 
     VIEW-AS FILL-IN 
     SIZE 15.2 BY 1 NO-UNDO.

DEFINE VARIABLE FI-VisPrisStatus AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 128.8 BY .76 NO-UNDO.

DEFINE VARIABLE FI-VPIDato LIKE ArtBas.VPIDato
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE RS-Liste AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Priskø":U, 1,
"Prishistorikk":U, 2,
"Prisprofiler":U, 3,
"Priskø (Alle)":U, 4
     SIZE 71.8 BY .95 NO-UNDO.

DEFINE VARIABLE RS-VisKalkyle AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Ordinær", 1,
"Tilbud", 2
     SIZE 12 BY 1.91 NO-UNDO.

DEFINE RECTANGLE RECT-36
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 44.6 BY .1.

DEFINE RECTANGLE RECT-37
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 44.6 BY .1.

DEFINE RECTANGLE RECT-38
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 44.6 BY .1.

DEFINE RECTANGLE RECT-39
     EDGE-PIXELS 3 GRAPHIC-EDGE  NO-FILL   
     SIZE 65 BY 14.43.

DEFINE RECTANGLE RECT-40
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 51.2 BY .1.

DEFINE RECTANGLE RECT-41
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 51.2 BY .1.

DEFINE RECTANGLE RECT-42
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 51.2 BY .1.

DEFINE RECTANGLE RECT-43
     EDGE-PIXELS 3 GRAPHIC-EDGE  NO-FILL   
     SIZE 54 BY 16.62.

DEFINE RECTANGLE RECT-44
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 84 BY 19.

DEFINE RECTANGLE RECT-56
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE .2 BY 14.43.

DEFINE RECTANGLE RECT-62
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 65 BY 4.05.

DEFINE RECTANGLE RECT-72
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 54 BY 2.43.

DEFINE VARIABLE T-Manuel AS LOGICAL INITIAL no 
     LABEL "Manuel" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-HPrisKo FOR 
      HPrisKo SCROLLING.

DEFINE QUERY BROWSE-KalkProfil FOR 
      TT_Prisprofil SCROLLING.

DEFINE QUERY BROWSE-PrisKo FOR 
      PrisKo SCROLLING.

DEFINE QUERY BROWSE-PrisKoAlle FOR 
      PrisKo SCROLLING.

DEFINE QUERY BROWSE-Prisprofil FOR 
      ArtPris SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-HPrisKo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-HPrisKo C-Kalkyle _STRUCTURED
  QUERY BROWSE-HPrisKo NO-LOCK DISPLAY
      wType COLUMN-LABEL "Type" FORMAT "xxxx":U WIDTH 5.2
      HPrisKo.AktiveresDato COLUMN-LABEL "Aktivert" FORMAT "99/99/9999":U
      wAktTid COLUMN-LABEL "Kl" WIDTH 10
      HPrisKo.Pris FORMAT "->>,>>9.99":U WIDTH 11
      HPrisKo.VareKost FORMAT "->,>>>,>>9.99":U
      HPrisKo.GyldigTilDato FORMAT "99/99/9999":U
      wTilTid
      HPrisKo.BrukerID FORMAT "X(18)":U WIDTH 15
      HPrisKo.EDato FORMAT "99/99/99":U
      wETid COLUMN-LABEL "Klokken" WIDTH 10.4
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS NO-TAB-STOP SIZE 79.4 BY 17.62 FIT-LAST-COLUMN.

DEFINE BROWSE BROWSE-KalkProfil
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-KalkProfil C-Kalkyle _STRUCTURED
  QUERY BROWSE-KalkProfil NO-LOCK DISPLAY
      TT_Prisprofil.ProfilNr FORMAT ">>>>>>9":U
      TT_Prisprofil.Tilbud COLUMN-LABEL "På tilbud" FORMAT "J/N":U
      TT_Prisprofil.Inpris COLUMN-LABEL "Innpris" FORMAT ">>,>>9.99":U
      TT_Prisprofil.Pris COLUMN-LABEL "Utpris" FORMAT ">>,>>9.99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 45.6 BY 2.62 ROW-HEIGHT-CHARS .63 FIT-LAST-COLUMN.

DEFINE BROWSE BROWSE-PrisKo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-PrisKo C-Kalkyle _STRUCTURED
  QUERY BROWSE-PrisKo NO-LOCK DISPLAY
      PrisKo.ProfilNr FORMAT ">>>>>>9":U
      w2Type COLUMN-LABEL "Type" FORMAT "xxxx":U
      PrisKo.EtikettStatus COLUMN-LABEL "Eti" FORMAT "9":U WIDTH 4
      PrisKo.KlargjorStatus COLUMN-LABEL "Beh" FORMAT "9":U WIDTH 4
      PrisKo.AktiveresDato FORMAT "99/99/99":U
      wAktTid COLUMN-LABEL "Kl" WIDTH 10
      PrisKo.Pris FORMAT "->>,>>9.99":U WIDTH 13.8
      PrisKo.VareKost FORMAT "->,>>>,>>9.99":U
      PrisKo.GyldigTilDato FORMAT "99/99/99":U
      wTilTid COLUMN-LABEL "Kl" WIDTH 10
      PrisKo.BrukerID FORMAT "X(18)":U
      PrisKo.EDato FORMAT "99/99/9999":U
      w2ETid COLUMN-LABEL "Kl" FORMAT "x(10)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS NO-TAB-STOP SIZE 79.4 BY 16.19 ROW-HEIGHT-CHARS .63 FIT-LAST-COLUMN.

DEFINE BROWSE BROWSE-PrisKoAlle
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-PrisKoAlle C-Kalkyle _STRUCTURED
  QUERY BROWSE-PrisKoAlle NO-LOCK DISPLAY
      PrisKo.ProfilNr FORMAT ">>>>>>9":U
      w2Type COLUMN-LABEL "Type" FORMAT "xxxx":U
      PrisKo.EtikettStatus COLUMN-LABEL "Eti" FORMAT "9":U WIDTH 4
      PrisKo.KlargjorStatus COLUMN-LABEL "Beh" FORMAT "9":U WIDTH 4
      PrisKo.AktiveresDato FORMAT "99/99/99":U
      wAktTid COLUMN-LABEL "Kl" WIDTH 10
      PrisKo.Pris FORMAT "->>,>>9.99":U WIDTH 13.8
      PrisKo.VareKost FORMAT "->,>>>,>>9.99":U
      PrisKo.GyldigTilDato FORMAT "99/99/99":U
      wTilTid COLUMN-LABEL "Kl" WIDTH 10
      PrisKo.BrukerID FORMAT "X(18)":U
      PrisKo.EDato FORMAT "99/99/9999":U
      w2ETid COLUMN-LABEL "Kl" FORMAT "x(10)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS NO-TAB-STOP SIZE 79.4 BY 17.62 ROW-HEIGHT-CHARS .63 FIT-LAST-COLUMN.

DEFINE BROWSE BROWSE-Prisprofil
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-Prisprofil C-Kalkyle _STRUCTURED
  QUERY BROWSE-Prisprofil NO-LOCK DISPLAY
      ArtPris.ProfilNr FORMAT ">>>>>>9":U
      ArtPris.AktivFraDato FORMAT "99/99/9999":U
      wAkttid COLUMN-LABEL "Kl"
      ArtPris.Pris[1] FORMAT "->,>>>,>>9.99":U
      ArtPris.VareKost[1] FORMAT "->>,>>9.99":U
      ArtPris.DB%[1] FORMAT "->>,>>9.99":U
      ArtPris.EDato FORMAT "99/99/9999":U
      wETid COLUMN-LABEL "Kl"
      ArtPris.BrukerID FORMAT "X(10)":U
      ArtPris.ArtikkelNr FORMAT "zzzzzzzzzzzz9":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS NO-TAB-STOP SIZE 79.4 BY 17.62 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-PrisInfo
     BROWSE-KalkProfil AT ROW 1.14 COL 3.4
     B-SlettArtPris AT ROW 19.67 COL 100.2 NO-TAB-STOP 
     B-SokValuta AT ROW 5.91 COL 42.8
     FI-ValKod AT ROW 5.91 COL 31 COLON-ALIGNED NO-LABEL
     B-Tilbakestill AT ROW 14.38 COL 48.6 NO-TAB-STOP 
     B-SupRab AT ROW 22.29 COL 61.2
     FI-Kroner-1 AT ROW 5.19 COL 15.4 COLON-ALIGNED NO-LABEL
     FI-Prosent-1 AT ROW 7.48 COL 31.4 COLON-ALIGNED NO-LABEL
     FI-Valuta-1 AT ROW 5.19 COL 31.4 COLON-ALIGNED NO-LABEL
     RS-Liste AT ROW 4.86 COL 127 NO-LABEL
     B-EndreDato AT ROW 22.24 COL 138.6
     B-SlettPrisKo AT ROW 22.24 COL 126.6
     FI-ProfilNr AT ROW 5.14 COL 77 COLON-ALIGNED
     FI-Beskrivelse AT ROW 5.14 COL 90 COLON-ALIGNED NO-LABEL
     FI-AktFra-2 AT ROW 7.1 COL 82.6 COLON-ALIGNED NO-LABEL
     FI-AktTil-2 AT ROW 7.1 COL 97.8 COLON-ALIGNED NO-LABEL
     FI-ValPris AT ROW 5.91 COL 15 COLON-ALIGNED
     FI-InnPris AT ROW 7.14 COL 15 COLON-ALIGNED
     FI-InnPris-2 AT ROW 9.1 COL 82.6 COLON-ALIGNED
     FI-Rab1 AT ROW 8.19 COL 15 COLON-ALIGNED
     FI-Rab1% AT ROW 8.19 COL 31 COLON-ALIGNED NO-LABEL
     FI-Rab1-2 AT ROW 10.14 COL 82.6 COLON-ALIGNED
     FI-Rab1%-2 AT ROW 10.14 COL 97.8 COLON-ALIGNED NO-LABEL
     FI-Rab2 AT ROW 9.24 COL 15 COLON-ALIGNED
     FI-Rab2% AT ROW 9.24 COL 31 COLON-ALIGNED NO-LABEL
     FI-Rab2-2 AT ROW 11.19 COL 82.6 COLON-ALIGNED
     FI-Rab2%-2 AT ROW 11.19 COL 97.8 COLON-ALIGNED NO-LABEL
     FI-Frakt AT ROW 10.24 COL 15 COLON-ALIGNED
     FI-Frakt% AT ROW 10.24 COL 31 COLON-ALIGNED NO-LABEL
     FI-Frakt-2 AT ROW 12.19 COL 82.6 COLON-ALIGNED
     FI-Frakt%-2 AT ROW 12.19 COL 97.8 COLON-ALIGNED NO-LABEL
     FI-DivKost AT ROW 11.19 COL 15 COLON-ALIGNED
     FI-DivKost% AT ROW 11.19 COL 31 COLON-ALIGNED NO-LABEL
     FI-DivKost-2 AT ROW 13.14 COL 82.6 COLON-ALIGNED
     FI-DivKost%-2 AT ROW 13.14 COL 97.8 COLON-ALIGNED NO-LABEL
     FI-Rab3 AT ROW 12.14 COL 15 COLON-ALIGNED
     FI-Rab3% AT ROW 12.14 COL 31 COLON-ALIGNED NO-LABEL
     FI-Rab3-2 AT ROW 14.1 COL 82.6 COLON-ALIGNED
     FI-Rab3%-2 AT ROW 14.1 COL 97.8 COLON-ALIGNED NO-LABEL
     FI-VareKost AT ROW 13.57 COL 15 COLON-ALIGNED
     FI-VareKost-2 AT ROW 15.52 COL 82.6 COLON-ALIGNED
     FI-DB AT ROW 14.57 COL 15 COLON-ALIGNED
     FI-DB% AT ROW 14.57 COL 31 COLON-ALIGNED NO-LABEL
     FI-DB-2 AT ROW 16.52 COL 82.6 COLON-ALIGNED
     FI-DB%-2 AT ROW 16.52 COL 97.8 COLON-ALIGNED NO-LABEL
     FI-MVA AT ROW 15.57 COL 15 COLON-ALIGNED
     FI-Mva% AT ROW 15.57 COL 31 COLON-ALIGNED NO-LABEL
     FI-MVA-2 AT ROW 17.52 COL 82.6 COLON-ALIGNED
     FI-Mva%-2 AT ROW 17.52 COL 97.8 COLON-ALIGNED NO-LABEL
     FI-Pris AT ROW 16.81 COL 15 COLON-ALIGNED
     RS-VisKalkyle AT ROW 6.86 COL 71.4 NO-LABEL NO-TAB-STOP 
     FI-Pris-2 AT ROW 18.76 COL 82.6 COLON-ALIGNED
     FI-EuPris AT ROW 17.81 COL 15 COLON-ALIGNED
     CB-Tilbud AT ROW 4.91 COL 48 HELP
          "Valg av prisendringstype." NO-LABEL
     FI-NAktFra AT ROW 7.14 COL 46.2 COLON-ALIGNED NO-LABEL
     FI-NTid AT ROW 8.24 COL 49.8 COLON-ALIGNED
     BROWSE-PrisKo AT ROW 5.81 COL 126.6
     FI-AktFra AT ROW 10.1 COL 46.4 COLON-ALIGNED NO-LABEL
     FI-Tid1 AT ROW 11.19 COL 49.8 COLON-ALIGNED
     FI-AktTil AT ROW 12.24 COL 46.4 COLON-ALIGNED NO-LABEL
     FI-Tid2 AT ROW 13.29 COL 49.8 COLON-ALIGNED
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 9.8
         SIZE 207.8 BY 23.1.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME FRAME-PrisInfo
     B-Godkjenn AT ROW 17.76 COL 48.4
     FI-EuPris-2 AT ROW 19.76 COL 82.6 COLON-ALIGNED
     B-Aktiver AT ROW 22.24 COL 158.6
     BROWSE-HPrisKo AT ROW 5.81 COL 126.6
     T-Manuel AT ROW 17.81 COL 33 NO-TAB-STOP 
     FI-VisPrisStatus AT ROW 2.76 COL 70.2 COLON-ALIGNED NO-LABEL
     FI-ToppTekst AT ROW 3.95 COL 3 NO-LABEL
     FI-ToppTekst-2 AT ROW 3.91 COL 69 NO-LABEL
     FI-ToppTekst-3 AT ROW 3.91 COL 123 NO-LABEL
     FI-Txt2 AT ROW 6.33 COL 46.6 COLON-ALIGNED NO-LABEL
     FI-Txt1 AT ROW 9.43 COL 47 COLON-ALIGNED NO-LABEL
     FI-ToppTekst-4 AT ROW 3.91 COL 123 NO-LABEL
     FI-Kroner-2 AT ROW 8.24 COL 82.6 COLON-ALIGNED NO-LABEL
     FI-Prosent-2 AT ROW 8.24 COL 98.4 COLON-ALIGNED NO-LABEL
     btnNAktFra AT ROW 7.24 COL 60.8 NO-TAB-STOP 
     btnAktFra AT ROW 10.24 COL 61 NO-TAB-STOP 
     btnAktTil AT ROW 12.29 COL 61 NO-TAB-STOP 
     B-GodkjennKopier AT ROW 16.57 COL 48.4
     FI-KatalogPris AT ROW 21.14 COL 13.6 COLON-ALIGNED HELP
          "" FORMAT "->>>>9.99"
     FI-AnbefaltPris AT ROW 22.24 COL 13.6 COLON-ALIGNED HELP
          ""
          LABEL "Veil.pris"
     FI-forhRab% AT ROW 21.14 COL 49.8 COLON-ALIGNED HELP
          ""
          LABEL "Forhåndsrab%"
     FI-supRab% AT ROW 22.24 COL 49.8 COLON-ALIGNED HELP
          ""
          LABEL "Suppl.rab%"
     FI-VPIDato AT ROW 20.05 COL 13.6 COLON-ALIGNED HELP
          ""
     B-ForhRab AT ROW 21.14 COL 61.2
     B-Katalogpris AT ROW 21.14 COL 30.6
     B-VeilPris AT ROW 22.29 COL 30.6
     FI-ToppTekst-5 AT ROW 19.29 COL 3 NO-LABEL
     B-NyKalkyle AT ROW 2.76 COL 49.8
     FI-ToppTekst-6 AT ROW 3.86 COL 123 NO-LABEL
     FI-ToppTekst-7 AT ROW 3.86 COL 123 NO-LABEL
     B-EtiSimuler AT ROW 22.24 COL 171.8
     FI-MengdeRabTekst AT ROW 21.43 COL 70 NO-LABEL
     ArtPris.MengdeRabAntall AT ROW 22.24 COL 82.6 COLON-ALIGNED
          LABEL "Antall"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     ArtPris.MengdeRabPris AT ROW 22.24 COL 99.2 COLON-ALIGNED
          LABEL "Pris"
          VIEW-AS FILL-IN 
          SIZE 20.2 BY 1 TOOLTIP "Pris som gjelder når mengderabatt slår til"
     BROWSE-Prisprofil AT ROW 5.81 COL 126.6
     BROWSE-PrisKoAlle AT ROW 5.91 COL 127
     RECT-36 AT ROW 7 COL 3
     RECT-37 AT ROW 13.29 COL 3
     RECT-38 AT ROW 16.62 COL 3
     RECT-39 AT ROW 4.67 COL 3
     RECT-40 AT ROW 8.95 COL 69.4
     RECT-41 AT ROW 15.24 COL 69.4
     RECT-42 AT ROW 18.57 COL 69.4
     RECT-43 AT ROW 4.67 COL 69
     RECT-44 AT ROW 4.67 COL 123
     RECT-56 AT ROW 4.62 COL 47.8
     RECT-62 AT ROW 19.62 COL 3
     RECT-72 AT ROW 21.29 COL 69
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 9.8
         SIZE 207.8 BY 23.1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: TT_Prisprofil T "?" NO-UNDO SkoTex Prisprofil
      ADDITIONAL-FIELDS:
          FIELD Inpris AS DECI FORMAT ">>,>>9.99"
          FIELD Pris as DECI FORMAT ">>,>>9.99"
          FIELD KalkFinns AS LOGI
          FIELD Tilbud as LOGI FORMAT "J/N"
          
      END-FIELDS.
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* SUPPRESS Window definition (used by the UIB) 
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Kalkyle ASSIGN
         HIDDEN             = YES
         TITLE              = "Artikkelkalkyle"
         HEIGHT             = 31.91
         WIDTH              = 208
         MAX-HEIGHT         = 31.91
         MAX-WIDTH          = 276
         VIRTUAL-HEIGHT     = 31.91
         VIRTUAL-WIDTH      = 276
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
                                                                        */
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME
ASSIGN C-Kalkyle = CURRENT-WINDOW.




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Kalkyle
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME FRAME-PrisInfo
   NOT-VISIBLE FRAME-NAME Custom                                        */
/* BROWSE-TAB BROWSE-KalkProfil 1 FRAME-PrisInfo */
/* BROWSE-TAB BROWSE-PrisKo FI-NTid FRAME-PrisInfo */
/* BROWSE-TAB BROWSE-HPrisKo B-Aktiver FRAME-PrisInfo */
/* BROWSE-TAB BROWSE-Prisprofil RECT-62 FRAME-PrisInfo */
/* BROWSE-TAB BROWSE-PrisKoAlle RECT-72 FRAME-PrisInfo */
ASSIGN 
       FRAME FRAME-PrisInfo:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON B-SlettArtPris IN FRAME FRAME-PrisInfo
   NO-ENABLE                                                            */
ASSIGN 
       BROWSE-HPrisKo:HIDDEN  IN FRAME FRAME-PrisInfo                = TRUE
       BROWSE-HPrisKo:NUM-LOCKED-COLUMNS IN FRAME FRAME-PrisInfo     = 1.

ASSIGN 
       BROWSE-PrisKo:POPUP-MENU IN FRAME FRAME-PrisInfo             = MENU POPUP-MENU-BROWSE-ArtPris:HANDLE
       BROWSE-PrisKo:NUM-LOCKED-COLUMNS IN FRAME FRAME-PrisInfo     = 1.

ASSIGN 
       BROWSE-PrisKoAlle:POPUP-MENU IN FRAME FRAME-PrisInfo             = MENU POPUP-MENU-BROWSE-ArtPris-2:HANDLE
       BROWSE-PrisKoAlle:NUM-LOCKED-COLUMNS IN FRAME FRAME-PrisInfo     = 1.

ASSIGN 
       BROWSE-Prisprofil:HIDDEN  IN FRAME FRAME-PrisInfo                = TRUE
       BROWSE-Prisprofil:NUM-LOCKED-COLUMNS IN FRAME FRAME-PrisInfo     = 1.

/* SETTINGS FOR COMBO-BOX CB-Tilbud IN FRAME FRAME-PrisInfo
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FI-AktFra-2 IN FRAME FRAME-PrisInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-AktTil-2 IN FRAME FRAME-PrisInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-AnbefaltPris IN FRAME FRAME-PrisInfo
   NO-ENABLE LIKE = skotex.ArtBas.AnbefaltPris EXP-LABEL EXP-SIZE       */
/* SETTINGS FOR FILL-IN FI-Beskrivelse IN FRAME FRAME-PrisInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-DB%-2 IN FRAME FRAME-PrisInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-DB-2 IN FRAME FRAME-PrisInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-DivKost%-2 IN FRAME FRAME-PrisInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-DivKost-2 IN FRAME FRAME-PrisInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-EuPris IN FRAME FRAME-PrisInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-EuPris-2 IN FRAME FRAME-PrisInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-forhRab% IN FRAME FRAME-PrisInfo
   NO-ENABLE LIKE = skotex.ArtBas.forhRab% EXP-LABEL EXP-SIZE           */
/* SETTINGS FOR FILL-IN FI-Frakt%-2 IN FRAME FRAME-PrisInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Frakt-2 IN FRAME FRAME-PrisInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-InnPris IN FRAME FRAME-PrisInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-InnPris-2 IN FRAME FRAME-PrisInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-KatalogPris IN FRAME FRAME-PrisInfo
   NO-ENABLE LIKE = skotex.ArtBas.KatalogPris EXP-FORMAT EXP-SIZE       */
/* SETTINGS FOR FILL-IN FI-MengdeRabTekst IN FRAME FRAME-PrisInfo
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FI-MVA IN FRAME FRAME-PrisInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Mva% IN FRAME FRAME-PrisInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Mva%-2 IN FRAME FRAME-PrisInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-MVA-2 IN FRAME FRAME-PrisInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Pris-2 IN FRAME FRAME-PrisInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Rab1%-2 IN FRAME FRAME-PrisInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Rab1-2 IN FRAME FRAME-PrisInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Rab2%-2 IN FRAME FRAME-PrisInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Rab2-2 IN FRAME FRAME-PrisInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Rab3%-2 IN FRAME FRAME-PrisInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Rab3-2 IN FRAME FRAME-PrisInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-supRab% IN FRAME FRAME-PrisInfo
   NO-ENABLE LIKE = skotex.ArtBas.supRab% EXP-LABEL EXP-SIZE            */
/* SETTINGS FOR FILL-IN FI-ToppTekst IN FRAME FRAME-PrisInfo
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FI-ToppTekst-2 IN FRAME FRAME-PrisInfo
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FI-ToppTekst-3 IN FRAME FRAME-PrisInfo
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FI-ToppTekst-4 IN FRAME FRAME-PrisInfo
   ALIGN-L                                                              */
ASSIGN 
       FI-ToppTekst-4:HIDDEN IN FRAME FRAME-PrisInfo           = TRUE.

/* SETTINGS FOR FILL-IN FI-ToppTekst-5 IN FRAME FRAME-PrisInfo
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FI-ToppTekst-6 IN FRAME FRAME-PrisInfo
   ALIGN-L                                                              */
ASSIGN 
       FI-ToppTekst-6:HIDDEN IN FRAME FRAME-PrisInfo           = TRUE.

/* SETTINGS FOR FILL-IN FI-ToppTekst-7 IN FRAME FRAME-PrisInfo
   ALIGN-L                                                              */
ASSIGN 
       FI-ToppTekst-7:HIDDEN IN FRAME FRAME-PrisInfo           = TRUE.

/* SETTINGS FOR FILL-IN FI-Txt1 IN FRAME FRAME-PrisInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Txt2 IN FRAME FRAME-PrisInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-ValKod IN FRAME FRAME-PrisInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-VareKost IN FRAME FRAME-PrisInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-VareKost-2 IN FRAME FRAME-PrisInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-VisPrisStatus IN FRAME FRAME-PrisInfo
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-VPIDato IN FRAME FRAME-PrisInfo
   NO-ENABLE LIKE = skotex.ArtBas.VPIDato EXP-SIZE                      */
/* SETTINGS FOR FILL-IN ArtPris.MengdeRabAntall IN FRAME FRAME-PrisInfo
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN ArtPris.MengdeRabPris IN FRAME FRAME-PrisInfo
   EXP-LABEL                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-HPrisKo
/* Query rebuild information for BROWSE BROWSE-HPrisKo
     _TblList          = "SkoTex.HPrisKo"
     _Options          = "NO-LOCK"
     _Where[1]         = "HPrisKo.ArtikkelNr = ArtBas.ArtikkelNr and
HPrisKo.ProfilNr   = FI-ProfilNr"
     _FldNameList[1]   > "_<CALC>"
"wType" "Type" "xxxx" ? ? ? ? ? ? ? no ? no no "5.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > SkoTex.HPrisKo.AktiveresDato
"HPrisKo.AktiveresDato" "Aktivert" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > "_<CALC>"
"wAktTid" "Kl" ? ? ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > SkoTex.HPrisKo.Pris
"HPrisKo.Pris" ? ? "decimal" ? ? ? ? ? ? no ? no no "11" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   = SkoTex.HPrisKo.VareKost
     _FldNameList[6]   = SkoTex.HPrisKo.GyldigTilDato
     _FldNameList[7]   > "_<CALC>"
"wTilTid" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > SkoTex.HPrisKo.BrukerID
"HPrisKo.BrukerID" ? "X(18)" "character" ? ? ? ? ? ? no ? no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > SkoTex.HPrisKo.EDato
"HPrisKo.EDato" ? "99/99/99" "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > "_<CALC>"
"wETid" "Klokken" ? ? ? ? ? ? ? ? no ? no no "10.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BROWSE-HPrisKo */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-KalkProfil
/* Query rebuild information for BROWSE BROWSE-KalkProfil
     _TblList          = "Temp-Tables.TT_Prisprofil"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "TT_Prisprofil.Kalkfinns = TRUE"
     _FldNameList[1]   = Temp-Tables.TT_Prisprofil.ProfilNr
     _FldNameList[2]   > "_<CALC>"
"TT_Prisprofil.Tilbud" "På tilbud" "J/N" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > "_<CALC>"
"TT_Prisprofil.Inpris" "Innpris" ">>,>>9.99" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > "_<CALC>"
"TT_Prisprofil.Pris" "Utpris" ">>,>>9.99" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BROWSE-KalkProfil */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-PrisKo
/* Query rebuild information for BROWSE BROWSE-PrisKo
     _TblList          = "SkoTex.PrisKo"
     _Options          = "NO-LOCK"
     _Where[1]         = "PrisKo.ArtikkelNr = ArtBas.ArtikkelNr and
PrisKo.ProfilNr   = FI-ProfilNr"
     _FldNameList[1]   = SkoTex.PrisKo.ProfilNr
     _FldNameList[2]   > "_<CALC>"
"w2Type" "Type" "xxxx" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > SkoTex.PrisKo.EtikettStatus
"PrisKo.EtikettStatus" "Eti" ? "integer" ? ? ? ? ? ? no ? no no "4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > SkoTex.PrisKo.KlargjorStatus
"PrisKo.KlargjorStatus" "Beh" ? "integer" ? ? ? ? ? ? no ? no no "4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > SkoTex.PrisKo.AktiveresDato
"PrisKo.AktiveresDato" ? "99/99/99" "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > "_<CALC>"
"wAktTid" "Kl" ? ? ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > SkoTex.PrisKo.Pris
"PrisKo.Pris" ? ? "decimal" ? ? ? ? ? ? no ? no no "13.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   = SkoTex.PrisKo.VareKost
     _FldNameList[9]   > SkoTex.PrisKo.GyldigTilDato
"PrisKo.GyldigTilDato" ? "99/99/99" "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > "_<CALC>"
"wTilTid" "Kl" ? ? ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > SkoTex.PrisKo.BrukerID
"PrisKo.BrukerID" ? "X(18)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   = SkoTex.PrisKo.EDato
     _FldNameList[13]   > "_<CALC>"
"w2ETid" "Kl" "x(10)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BROWSE-PrisKo */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-PrisKoAlle
/* Query rebuild information for BROWSE BROWSE-PrisKoAlle
     _TblList          = "SkoTex.PrisKo"
     _Options          = "NO-LOCK"
     _OrdList          = "SkoTex.PrisKo.ArtikkelNr|yes,SkoTex.PrisKo.ProfilNr|yes,SkoTex.PrisKo.AktiveresDato|yes,SkoTex.PrisKo.AktiveresTid|yes"
     _Where[1]         = "PrisKo.ArtikkelNr = ArtBas.ArtikkelNr"
     _FldNameList[1]   = SkoTex.PrisKo.ProfilNr
     _FldNameList[2]   > "_<CALC>"
"w2Type" "Type" "xxxx" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > SkoTex.PrisKo.EtikettStatus
"PrisKo.EtikettStatus" "Eti" ? "integer" ? ? ? ? ? ? no ? no no "4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > SkoTex.PrisKo.KlargjorStatus
"PrisKo.KlargjorStatus" "Beh" ? "integer" ? ? ? ? ? ? no ? no no "4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > SkoTex.PrisKo.AktiveresDato
"PrisKo.AktiveresDato" ? "99/99/99" "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > "_<CALC>"
"wAktTid" "Kl" ? ? ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > SkoTex.PrisKo.Pris
"PrisKo.Pris" ? ? "decimal" ? ? ? ? ? ? no ? no no "13.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   = SkoTex.PrisKo.VareKost
     _FldNameList[9]   > SkoTex.PrisKo.GyldigTilDato
"PrisKo.GyldigTilDato" ? "99/99/99" "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > "_<CALC>"
"wTilTid" "Kl" ? ? ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > SkoTex.PrisKo.BrukerID
"PrisKo.BrukerID" ? "X(18)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   = SkoTex.PrisKo.EDato
     _FldNameList[13]   > "_<CALC>"
"w2ETid" "Kl" "x(10)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BROWSE-PrisKoAlle */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-Prisprofil
/* Query rebuild information for BROWSE BROWSE-Prisprofil
     _TblList          = "SkoTex.ArtPris"
     _Options          = "NO-LOCK"
     _Where[1]         = "SkoTex.ArtPris.ArtikkelNr = ArtBas.ArtikkelNr"
     _FldNameList[1]   = SkoTex.ArtPris.ProfilNr
     _FldNameList[2]   = SkoTex.ArtPris.AktivFraDato
     _FldNameList[3]   > "_<CALC>"
"wAkttid" "Kl" ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   = SkoTex.ArtPris.Pris[1]
     _FldNameList[5]   = SkoTex.ArtPris.VareKost[1]
     _FldNameList[6]   = SkoTex.ArtPris.DB%[1]
     _FldNameList[7]   = SkoTex.ArtPris.EDato
     _FldNameList[8]   > "_<CALC>"
"wETid" "Kl" ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   = SkoTex.ArtPris.BrukerID
     _FldNameList[10]   = SkoTex.ArtPris.ArtikkelNr
     _Query            is OPENED
*/  /* BROWSE BROWSE-Prisprofil */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Kalkyle
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Kalkyle C-Kalkyle
ON END-ERROR OF C-Kalkyle /* Artikkelkalkyle */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Kalkyle C-Kalkyle
ON WINDOW-CLOSE OF C-Kalkyle /* Artikkelkalkyle */
DO:
    IF NOT AVAIL ArtPris THEN DO:
        MESSAGE "Det må legges opp en kalkyle på denne artikkelen."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.

  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Aktiver
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Aktiver C-Kalkyle
ON CHOOSE OF B-Aktiver IN FRAME FRAME-PrisInfo /* Aktiver.. */
DO:
    IF AVAILABLE PrisKo THEN
    DO:
        IF VALID-HANDLE(h_PrisKo) THEN
        DO:
            IF AVAILABLE ArtPris THEN
                FIND CURRENT ArtPris NO-LOCK NO-ERROR.
            /* Er posten tatt hånd om av priskø server, fikser vi bare opp */
            /* skjermen og returnerer.                                     */
            FIND CURRENT PrisKo NO-LOCK NO-ERROR.
            IF NOT AVAILABLE PrisKo THEN
            DO:
                {&OPEN-QUERY-BROWSE-HPrisKo}
                {&OPEN-QUERY-BROWSE-PrisKo}
/*                 {&OPEN-BROWSERS-IN-QUERY-FRAME-PrisInfo} */
                RUN InitKalkyle (INPUT FI-ProfilNr,1).
                RUN VisPrisStatus.
                RUN VisTilbudsFlagg(3).
                RETURN NO-APPLY.
            END.

            FIND bufArtBas NO-LOCK WHERE
              bufArtBas.ArtikkelNr = PrisKo.ArtikkelNr NO-ERROR.
            IF NOT AVAILABLE bufArtBas THEN
                RETURN NO-APPLY.

            /* Klargjør priskø for artikkelen. */
            RUN KlargjorPrisKoEn IN h_PrisKo (ROWID(ArtBas)).
            
            /* Hvis ArtPris for profilen har blitt slettet vedklargjøring, må tmp tabellen oppdateres. */
            RUN UpdatePPArtpris.

            /* Leser den oppdaterte versjonen av ArtPris inn i bufferet. */
            FIND ArtPris OF ArtBas NO-LOCK WHERE
                ArtPris.ProfilNr = INT(INPUT FI-ProfilNr) NO-ERROR.
            IF NOT AVAILABLE ArtPris THEN
                FIND ArtPris OF ArtBas NO-LOCK WHERE 
                  ArtPris.ProfilNr = clButiker.ProfilNr NO-ERROR.
            IF AVAILABLE ArtPris THEN
                FIND TT_PrisProfil WHERE
                  TT_PrisProfil.ProfilNr = ArtPris.ProfilNr NO-ERROR.
            
            RUN InitKalkyle (FALSE,1).
            RUN VisPrisStatus.
            RUN VisTilbudsFlagg(3).

            RUN VisArtBas IN wParentHandle NO-ERROR.
            {&OPEN-QUERY-BROWSE-HPrisKo}
            {&OPEN-QUERY-BROWSE-PrisKo}
            {&OPEN-QUERY-BROWSE-PrisKoAlle}
/*                 {&OPEN-BROWSERS-IN-QUERY-FRAME-PrisInfo} */
            APPLY "ENTRY" TO FI-ValPris IN FRAME FRAME-PrisInfo.
        END.
        ELSE
            RETURN NO-APPLY.
    END.
    ELSE
        RETURN NO-APPLY.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-EndreDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-EndreDato C-Kalkyle
ON CHOOSE OF B-EndreDato IN FRAME FRAME-PrisInfo /* Endre dato og pris... */
DO:
    IF AVAILABLE PrisKo THEN
    DO:
        IF VALID-HANDLE(h_PrisKo) THEN
        DO:
            IF AVAILABLE ArtPris THEN
                FIND CURRENT ArtPris NO-LOCK NO-ERROR.
            FIND CURRENT PrisKo NO-LOCK NO-ERROR.
            IF NOT AVAILABLE PrisKo THEN
            DO:
                MESSAGE "Priskøposten er klargjort/behandlet av en annen bruker."
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
                RUN InitKalkyle (INPUT FI-ProfilNr,1).
                RUN VisPrisStatus.
                RUN VisTilbudsFlagg(3).
                {&OPEN-QUERY-BROWSE-HPrisKo}
                {&OPEN-QUERY-BROWSE-PrisKo}
                {&OPEN-QUERY-BROWSE-PrisKoAlle}
/*                 {&OPEN-BROWSERS-IN-QUERY-FRAME-PrisInfo} */
                RETURN NO-APPLY.
            END.
            RUN EndreDato IN h_PrisKo (ROWID(PrisKo)).
            IF RETURN-VALUE = "AVBRYT" THEN
                RETURN NO-APPLY.
            FIND CURRENT PrisKo NO-LOCK.
            BROWSE BROWSE-PrisKo:REFRESH().
            {dispprisko.i &Tabell="PrisKo"}
        END.
        ELSE
            RETURN NO-APPLY.
    END.
    ELSE
        RETURN NO-APPLY.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-EtiSimuler
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-EtiSimuler C-Kalkyle
ON CHOOSE OF B-EtiSimuler IN FRAME FRAME-PrisInfo /* Etikett simulering */
DO:
    IF AVAILABLE PrisKo THEN
    DO:
        IF VALID-HANDLE(h_PrisKo) THEN
        DO:
            IF AVAILABLE ArtPris THEN
                FIND CURRENT ArtPris NO-LOCK NO-ERROR.
            /* Er posten tatt hånd om av priskø server, fikser vi bare opp */
            /* skjermen og returnerer.                                     */
            FIND CURRENT PrisKo NO-LOCK NO-ERROR.
            IF NOT AVAILABLE PrisKo THEN
            DO:
                {&OPEN-QUERY-BROWSE-HPrisKo}
                {&OPEN-QUERY-BROWSE-PrisKo}
                {&OPEN-QUERY-BROWSE-PrisKoAlle}
/*                 {&OPEN-BROWSERS-IN-QUERY-FRAME-PrisInfo} */
                RUN InitKalkyle (INPUT FI-ProfilNr,1).
                RUN VisPrisStatus.
                RUN VisTilbudsFlagg(3).
                RETURN NO-APPLY.
            END.

            IF PrisKo.EtikettStatus > 0 THEN
            DO:
                MESSAGE "Etiett er allerede skrevet ut på denne priskøposten for profil: " INPUT FI-ProfilNr Prisko.ProfilNr SKIP
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
                RETURN NO-APPLY.
            END.

            FIND bufArtBas NO-LOCK WHERE
              bufArtBas.ArtikkelNr = PrisKo.ArtikkelNr NO-ERROR.
            IF NOT AVAILABLE bufArtBas THEN
                RETURN NO-APPLY.

            /* Setter etikettflagget */
            DO TRANSACTION:
                FIND CURRENT PrisKo EXCLUSIVE-LOCK.
                ASSIGN
                    PrisKo.EtikettStatus = 1
                    .
            END.
            FIND CURRENT PrisKo NO-LOCK.
            BROWSE BROWSE-PrisKo:REFRESH().

            /* Leser den oppdaterte versjonen av ArtPris inn i bufferet. */
            IF AVAILABLE ArtPris THEN
                FIND CURRENT ArtPris NO-LOCK NO-ERROR.
        END.
        ELSE
            RETURN NO-APPLY.
    END.
    ELSE
        RETURN NO-APPLY.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-ForhRab
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-ForhRab C-Kalkyle
ON CHOOSE OF B-ForhRab IN FRAME FRAME-PrisInfo /* Velg */
DO:
    ASSIGN
        FI-Rab1%:SCREEN-VALUE = FI-ForhRab%:SCREEN-VALUE
        .
     APPLY "ENTRY" TO FI-Rab1%.
     RUN Kalkulasjon (1).
     RETURN NO-APPLY.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Godkjenn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Godkjenn C-Kalkyle
ON CHOOSE OF B-Godkjenn IN FRAME FRAME-PrisInfo /* Godkjenn pris */
DO:
    DEFINE VARIABLE lNyKalkyle AS LOGICAL    NO-UNDO.
    ASSIGN
        iOldProfilNr = INPUT FI-ProfilNr.
    APPLY "TAB" TO FOCUS.
    IF INPUT FI-ValPris = 0 THEN DO:
        wOk = FALSE.
        MESSAGE "Valutapris/innkjøpspris skal normalt være > 0." SKIP
                "Bekreft at kalkylen med 0 i valutapris/innkjøpspris skal sendes til kassen."
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE 'Bekreft kalkyle' 
            UPDATE wOk.
        IF wOk = FALSE THEN
            RETURN NO-APPLY.
    END.
    IF ArtBas.Opris = TRUE THEN DO:
        RUN KontrollerOpris.
        IF RETURN-VALUE = "AVBRYT" THEN DO:
            APPLY "CHOOSE" TO B-Tilbakestill.
            RETURN NO-APPLY.
        END.
    END.
  ASSIGN
    CB-Tilbud
    /*T-Tilbud*/
      lNyKalkyle = NOT AVAIL ArtPris
    .
    
  /* Sjekker om dagens dato er angitt */
  IF (CB-Tilbud = 2 OR CB-Tilbud = 5) THEN
  /*IF T-Tilbud THEN*/
  DO:
      ASSIGN
          FI-AktFra
          FI-Tid1
          FI-AktTil
          FI-Tid2.
  END.
  ELSE DO:
      ASSIGN
          FI-NAktFra
          FI-NTid
          .
  END.
  RUN LagreKalkyle.    /* Oppdaterer ArtPris og PrisKo.         */

  IF RETURN-VALUE BEGINS "AVBRYT" THEN
  DO:
    ASSIGN lNybestilling = FALSE.
    RETURN NO-APPLY.      
  END.
  
  /* Nullstiller tilbud */
  IF CB-Tilbud <> 1 THEN
  /*IF T-Tilbud THEN*/
  DO:
      /*
      assign
          T-Tilbud = FALSE.
      DISPLAY 
          T-Tilbud
      WITH FRAME FRAME-PrisInfo.
      */    
      ASSIGN
          CB-Tilbud = 1.
      DISPLAY 
          CB-Tilbud
      WITH FRAME FRAME-PrisInfo.
  END.

  /* Dette må hentes her, eller blir pris for profil 1 alltid vist. */
  /* Hentes for at pris for andre profiler enn profil 1 skal bli stående i skjermen. */
  FIND Prisprofil NO-LOCK WHERE
      PrisProfil.ProfilNr = INPUT FI-ProfilNr NO-ERROR.
  FIND ArtPris NO-LOCK WHERE
      ArtPris.ArtikkelNr = wArtikkelNr AND
      ArtPris.ProfilNr = INPUT FI-ProfilNr NO-ERROR.
  /* Oppdaterer visningsramme */
  RUN InitKalkyle (FALSE,1).
  RUN VisPrisStatus.
  RUN VisTilbudsFlagg(3).
  RUN VisArtBas IN wParentHandle NO-ERROR.
  {&OPEN-QUERY-BROWSE-HPrisKo}
  {&OPEN-QUERY-BROWSE-PrisKo}
  {&OPEN-QUERY-BROWSE-PrisKoAlle}
/*                 {&OPEN-BROWSERS-IN-QUERY-FRAME-PrisInfo} */
  APPLY "choose" TO B-Aktiver IN FRAME FRAME-PrisInfo.
  APPLY "ENTRY" TO FI-ValPris IN FRAME FRAME-PrisInfo.

/*   IF lNyKalkyle THEN */
      RUN NyKalkyleLagret IN wParentHandle.

  IF lNybestilling = TRUE THEN DO:
      RUN NyBestilling.
      RUN VisArtBas IN wParentHandle NO-ERROR.
  END.
  IF VALID-HANDLE(hSourceP) AND CAN-DO(hSourceP:INTERNAL-ENTRIES,"setGridEntry") THEN DO:
/*       APPLY "LEAVE" TO FI-ProfilNr. */
      APPLY "LEAVE" TO FRAME FRAME-PrisInfo.
      PROCESS EVENTS.
      RUN setGridEntry IN hSourceP NO-ERROR.
  END.
  ASSIGN
      FI-ProfilNr:SCREEN-VALUE = STRING(iOldProfilNr).
/*   APPLY "TAB" TO FI-ProfilNr.                                  */
/*   MESSAGE   FI-ProfilNr:SCREEN-VALUE SKIP STRING(iOldProfilNr) */
/*                                                                */
/*       VIEW-AS ALERT-BOX INFO BUTTONS OK.                       */
  RUN BytVis.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Godkjenn C-Kalkyle
ON TAB OF B-Godkjenn IN FRAME FRAME-PrisInfo /* Godkjenn pris */
DO:
  APPLY "ENTRY":U TO FI-ValPris IN FRAME FRAME-PrisInfo.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-GodkjennKopier
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-GodkjennKopier C-Kalkyle
ON CHOOSE OF B-GodkjennKopier IN FRAME FRAME-PrisInfo /* Godkjenn/Kopier */
DO:
  APPLY "CHOOSE" TO B-Godkjenn.
  IF clButiker.Profilnr <> INPUT FI-Profilnr THEN DO:
  END.
  RUN dsynkmodellpris.w (artbas.artikkelnr,INPUT INPUT FI-Profilnr).
  IF VALID-HANDLE(hSourceP) AND CAN-DO(hSourceP:INTERNAL-ENTRIES,"setGridEntry") THEN DO:
/*       APPLY "LEAVE" TO FI-ProfilNr. */
      APPLY "LEAVE" TO FRAME FRAME-PrisInfo.
      PROCESS EVENTS.
      RUN setGridEntry IN hSourceP NO-ERROR.
  END.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-GodkjennKopier C-Kalkyle
ON TAB OF B-GodkjennKopier IN FRAME FRAME-PrisInfo /* Godkjenn/Kopier */
DO:
  APPLY "ENTRY":U TO FI-ValPris IN FRAME FRAME-PrisInfo.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Katalogpris
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Katalogpris C-Kalkyle
ON CHOOSE OF B-Katalogpris IN FRAME FRAME-PrisInfo /* Velg */
DO:
  ASSIGN
      FI-ValPris:SCREEN-VALUE = FI-KatalogPris:SCREEN-VALUE
      .
   APPLY "ENTRY" TO FI-ValPris.
   RUN Kalkulasjon (1).
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-NyKalkyle
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-NyKalkyle C-Kalkyle
ON CHOOSE OF B-NyKalkyle IN FRAME FRAME-PrisInfo /* Ny kalkyle/prisprofil */
DO:
  DEF VAR cLookupValue AS CHAR NO-UNDO.
  
  IF NOT CAN-FIND(FIRST artpris WHERE artpris.artikkelnr = wArtikkelnr) THEN DO:
      APPLY "ENTRY" TO FI-ValPris.
      RETURN NO-APPLY.
  END.
  IF AVAIL artbas AND artbas.opris = TRUE THEN DO:
      MESSAGE "Åpen pris -> ingen ny kalkyle kan registreres"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
  END.
  IF CB-Tilbud:SCREEN-VALUE <> "1" THEN DO:
      MESSAGE "Endre til normalkalkyle"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
  END.
  cLookupValue = "Profilnr".
  RUN JBoxDLookup.w ("Prisprofil;Profilnr|Profilnr|>>>>>>9;Beskrivelse", 
                     "WHERE CAN-DO('" + cNyaProfiler + "',STRING(Prisprofil.profilnr))",
                     INPUT-OUTPUT cLookupValue).

  IF cLookupValue = "" THEN
      RETURN NO-APPLY.
  FI-ProfilNr:SCREEN-VALUE = ENTRY(1,cLookupValue,"|").
  ASSIGN
    FI-ProfilNr.
  FIND PrisProfil NO-LOCK WHERE
    PrisProfil.ProfilNr = INPUT FI-ProfilNr NO-ERROR.
  IF NOT AVAILABLE PrisProfil THEN
    DO:
      MESSAGE "Ugyldig prisprofil!"
              VIEW-AS ALERT-BOX TITLE "Melding".
      RETURN NO-APPLY.
    END.

  ELSE DO:
    DISPLAY
      PrisProfil.ProfilNr    @ FI-PRofilNr
      Prisprofil.Beskrivelse @ FI-Beskrivelse
    WITH FRAME FRAME-PrisInfo.
  END.

  IF CAN-FIND(ArtPris WHERE
              ArtPris.artikkelNr = wArtikkelNr AND
              ArtPris.ProfilNr   = INPUT FI-ProfilNr) THEN
  DO:
      MESSAGE "Det er allerede lagt opp en kalkyle på denne profilen"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
  END.
  DO:
      MESSAGE "Det er ikke registrert kalkyle på angitt profil." SKIP(1)
          "Skal det opprettes en ny kalkyle på den angitte profilen?"
          VIEW-AS ALERT-BOX INFO BUTTONS YES-NO UPDATE wOk.
      IF wOk THEN DO:
          RUN NyArtPrisProfilnr (wArtikkelNr, INPUT Prisprofil.profilnr).
          IF RETURN-VALUE <> "OK" THEN
              RETURN NO-APPLY.
      END.
      ELSE RETURN NO-APPLY.
  END.
  RUN BytVis.
  APPLY "ENTRY" TO FI-ValPris.
  RETURN NO-APPLY.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SlettArtPris
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SlettArtPris C-Kalkyle
ON CHOOSE OF B-SlettArtPris IN FRAME FRAME-PrisInfo /* Slett */
DO:
  DO WITH FRAME FRAME-PrisInfo:
      IF INPUT FI-ProfilNr = clButiker.ProfilNr THEN
      DO:
          MESSAGE "Pris på sentrallagerets prisprofil kan ikke slettes."
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN NO-APPLY.
      END.
      ELSE DO:
          RUN SlettArtPris.
      END.
  END.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SlettPrisKo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SlettPrisKo C-Kalkyle
ON CHOOSE OF B-SlettPrisKo IN FRAME FRAME-PrisInfo /* Slett... */
DO:
  IF AVAILABLE PrisKo THEN
  DO:
      FIND CURRENT PrisKo NO-LOCK NO-ERROR.
      IF NOT AVAILABLE PrisKo THEN
      DO:
          IF AVAILABLE ArtPris THEN
            FIND CURRENT ArtPris NO-LOCK NO-ERROR.
          MESSAGE "Priskøposten er klargjort/behandlet av en annen bruker."
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          {&OPEN-QUERY-BROWSE-HPrisKo}
          {&OPEN-QUERY-BROWSE-PrisKo}
          {&OPEN-QUERY-BROWSE-PrisKoAlle}
/*                 {&OPEN-BROWSERS-IN-QUERY-FRAME-PrisInfo} */
          RUN InitKalkyle (INPUT FI-ProfilNr,1).
          RUN VisPrisStatus.
          RUN VisTilbudsFlagg(3).
          RETURN NO-APPLY.
      END.
      ASSIGN
          lOk = FALSE
          .
      MESSAGE "Skal prisposten slettes?"
          VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Bekreft"
          UPDATE lOk.
      IF lOK <> TRUE THEN
          RETURN NO-APPLY.

      IF VALID-HANDLE(h_PrisKo) THEN
      SLETTBLOKK:
      DO:
          /* Av tilbudspost skal avslutte tilbudet istedenfor slettes. */
          IF (Prisko.Tilbud = TRUE AND PrisKo.TYPE = 3) THEN
          TRANSBLOKK:
          DO:
              FIND CURRENT Prisko EXCLUSIVE-LOCK.
              ASSIGN
                  Prisko.AktiveresDato = TODAY
                  Prisko.AktiveresTid  = TIME - 10
                  .
              FIND CURRENT Prisko NO-LOCK.
              APPLY "choose" TO B-Aktiver.
              RETURN NO-APPLY.
          END. /* TRANSBLOKK */
          ELSE DO:
              RUN SlettPrisKo IN h_PrisKo (ROWID(PrisKo)).
              RUN InitKalkyle (INPUT FI-ProfilNr,1).
              RUN VisPrisStatus.
              RUN VisTilbudsFlagg(3).

              RUN VisArtBas IN wParentHandle NO-ERROR.
          END.

          {&OPEN-QUERY-BROWSE-HPrisKo}
          {&OPEN-QUERY-BROWSE-PrisKo}
          {&OPEN-QUERY-BROWSE-PrisKoAlle}
/*                 {&OPEN-BROWSERS-IN-QUERY-FRAME-PrisInfo} */
          APPLY "ENTRY" TO FI-ValPris IN FRAME FRAME-PrisInfo.
      END. /* SLETTBLOKK */ 
      ELSE
          RETURN NO-APPLY.
  END.
  ELSE
      RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SokValuta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SokValuta C-Kalkyle
ON CHOOSE OF B-SokValuta IN FRAME FRAME-PrisInfo /* ... */
OR F10 OF FI-ValKod
DO:
    cTekst = "ValKod".
    RUN JBoxDLookup.w ("Valuta;ValKod;ValNavn;ValKurs;ValDatum;ValLand", "where true", INPUT-OUTPUT cTekst).

    IF RETURN-VALUE = "AVBRYT" THEN
        RETURN NO-APPLY.

    FIND Valuta NO-LOCK WHERE
      Valuta.ValKod = (cTekst) NO-ERROR.
    IF AVAILABLE Valuta THEN
    DO:
        /* Legger opp verdier I de aktuelle feltene */
        ASSIGN
          FI-ValKod:SCREEN-VALUE   = cTekst.
    END.
    ELSE DO:
        /* Legger opp verdier I de aktuelle feltene */
        ASSIGN
          FI-ValKod:SCREEN-VALUE    = ''.
    END.
    APPLY "LEAVE" TO FI-ValPris.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SupRab
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SupRab C-Kalkyle
ON CHOOSE OF B-SupRab IN FRAME FRAME-PrisInfo /* Velg */
DO:
    ASSIGN
        FI-Rab1%:SCREEN-VALUE = FI-SupRab%:SCREEN-VALUE
        .
     APPLY "ENTRY" TO FI-Rab1%.
     RUN Kalkulasjon (1).
     RETURN NO-APPLY.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Tilbakestill
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Tilbakestill C-Kalkyle
ON CHOOSE OF B-Tilbakestill IN FRAME FRAME-PrisInfo /* Tilbakestill */
DO:
    IF AVAIL Artpris AND feilIkalkyle() THEN DO:
        RUN TaBortFeil.
        RETURN NO-APPLY.
    END.
    ELSE
        RUN InitKalkyle(FALSE,1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-VeilPris
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-VeilPris C-Kalkyle
ON CHOOSE OF B-VeilPris IN FRAME FRAME-PrisInfo /* Velg */
DO:
    ASSIGN
        FI-Pris:SCREEN-VALUE = FI-AnbefaltPris:SCREEN-VALUE
        .
     APPLY "ENTRY" TO FI-Pris.
     RUN Kalkulasjon (1).
     RETURN NO-APPLY.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-HPrisKo
&Scoped-define SELF-NAME BROWSE-HPrisKo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-HPrisKo C-Kalkyle
ON MOUSE-SELECT-CLICK OF BROWSE-HPrisKo IN FRAME FRAME-PrisInfo
OR "RETURN":U OF BROWSE-HPrisKo
DO:
  {dispprisko.i &Tabell="HPrisKo"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-HPrisKo C-Kalkyle
ON ROW-DISPLAY OF BROWSE-HPrisKo IN FRAME FRAME-PrisInfo
DO:
  ASSIGN
    wAktTid = STRING(HPrisKo.AktiveresTid,"HH:MM:SS")
    wAktTid = IF wAktTid = "00:00:00"
                THEN ""
                ELSE wAktTid
    wTilTid = STRING(HPrisKo.GyldigTilTid,"HH:MM:SS")
    wTilTid = IF wTilTid = "00:00:00"
                  THEN ""
                  ELSE wTilTid
    wETid = STRING(HPrisKo.ETid,"HH:MM:SS")
    wType = IF HPrisKo.Type = 1 
              THEN "NOR"
            ELSE IF HPRisKo.TYPE = 2 
              THEN "PÅ"
            ELSE IF HPRisKo.TYPE = 4 
              THEN "ETIL"
            ELSE IF HPRisKo.TYPE = 5 
              THEN "LPÅ"
            ELSE IF HPRisKo.TYPE = 6 
              THEN "LAV"
            ELSE "AV"
    wType:bgcolor IN BROWSE BROWSE-HPrisKo = IF HPrisKo.Type = 1
                                               THEN ?
                                             ELSE IF HPrisKo.Type = 2
                                               THEN 12
                                             ELSE IF HPrisKo.TYPE = 4
                                               THEN 13
                                             ELSE IF HPrisKo.TYPE = 5
                                               THEN 4
                                             ELSE IF HPrisKo.TYPE = 6
                                               THEN 4
                                             ELSE 12.
              
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-KalkProfil
&Scoped-define SELF-NAME BROWSE-KalkProfil
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-KalkProfil C-Kalkyle
ON DEFAULT-ACTION OF BROWSE-KalkProfil IN FRAME FRAME-PrisInfo
DO:
/*     IF TT_Prisprofil.Profilnr <> INPUT FI-Profilnr THEN DO:        */
/*         FI-Profilnr:SCREEN-VALUE = STRING(TT_Prisprofil.Profilnr). */
/*         ASSIGN FI-Profilnr.                                        */
/*         RUN BytVis.                                                */
/*     END.                                                           */
/*     APPLY "ENTRY" TO FI-ValPris.                                   */
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-KalkProfil C-Kalkyle
ON VALUE-CHANGED OF BROWSE-KalkProfil IN FRAME FRAME-PrisInfo
DO:
    IF NOT AVAIL TT_Prisprofil OR TT_Prisprofil.profilnr = INPUT FI-Profilnr THEN
        RETURN NO-APPLY.
    IF TT_Prisprofil.Profilnr <> INPUT FI-Profilnr THEN DO:
        FI-Profilnr:SCREEN-VALUE = STRING(TT_Prisprofil.Profilnr).
        ASSIGN FI-Profilnr.
        RUN BytVis.
    END.
    APPLY "ENTRY" TO FI-ValPris.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-PrisKo
&Scoped-define SELF-NAME BROWSE-PrisKo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-PrisKo C-Kalkyle
ON MOUSE-MENU-DOWN OF BROWSE-PrisKo IN FRAME FRAME-PrisInfo
DO:
  DEFINE VARIABLE hMenu AS HANDLE     NO-UNDO.
 ASSIGN hMenu = BROWSE BROWSE-PrisKo:POPUP-MENU
        hMenu:SENSITIVE = BROWSE BROWSE-PrisKo:FOCUSED-ROW <> ?.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-PrisKo C-Kalkyle
ON MOUSE-SELECT-CLICK OF BROWSE-PrisKo IN FRAME FRAME-PrisInfo
OR "RETURN":U OF BROWSE-PrisKo
DO:
  {dispprisko.i &Tabell="PrisKo"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-PrisKo C-Kalkyle
ON ROW-DISPLAY OF BROWSE-PrisKo IN FRAME FRAME-PrisInfo
DO:
  ASSIGN
    w2ETid  = STRING(PrisKo.ETid,"HH:MM:SS")
    wAktTid = STRING(PrisKo.AktiveresTid,"HH:MM:SS")
    wAktTid = IF wAktTid = "00:00:00"
                THEN ""
                ELSE wAktTid
    wTilTid = STRING(PrisKo.GyldigTilTid,"HH:MM:SS")
    wTilTid = IF wTilTid = "00:00:00"
                THEN ""
                ELSE wTilTid
    w2Type  = IF PrisKo.Type = 1 
                THEN "NOR"
              ELSE IF PrisKo.Type = 2 
                THEN "PÅ"
              ELSE IF PrisKo.Type = 4 
                THEN "ETIL"
              ELSE IF PRisKo.TYPE = 5 
                THEN "LPÅ"
              ELSE IF PRisKo.TYPE = 6 
                THEN "LAV"
              ELSE "AV"    
    w2Type:bgcolor IN BROWSE BROWSE-PrisKo = IF PrisKo.Type = 1
                                               THEN ?
                                             ELSE IF PrisKo.Type = 2
                                               THEN 12
                                             ELSE IF PrisKo.Type = 4
                                               THEN 13
                                             ELSE IF PrisKo.TYPE = 5
                                               THEN 4
                                             ELSE IF PrisKo.TYPE = 6
                                               THEN 4
                                             ELSE 12.
  IF AVAILABLE HPrisKo THEN
    DO:
      ASSIGN
      FI-ValPris:screen-value  = STRING(HPrisKo.ValPris)
      FI-InnPris:screen-value  = STRING(HPrisKo.InnkjopsPris)
      FI-Rab1:screen-value     = STRING(HPrisKo.Rab1Kr)
      FI-Rab1%:screen-value    = STRING(HPrisKo.Rab1%)
      FI-Rab2:screen-value     = STRING(HPrisKo.Rab2Kr)
      FI-Rab2%:screen-value    = STRING(HPrisKo.Rab2%)
      FI-Frakt:screen-value    = STRING(HPrisKo.Frakt)
      FI-Frakt%:screen-value   = STRING(HPrisKo.Frakt%)
      FI-DivKost:screen-value  = STRING(HPrisKo.DivKostKr)
      FI-DivKost%:screen-value = STRING(HPrisKo.DivKost%)
      FI-Rab3:screen-value     = STRING(HPrisKo.Rab3Kr)
      FI-Rab3%:screen-value    = STRING(HPrisKo.Rab3%)
      FI-VareKost:screen-value = STRING(HPrisKo.VareKost)
      FI-Mva:screen-value      = STRING(HPrisKo.MvaKr)
      FI-Mva%:screen-value     = STRING(HPrisKo.Mva%)
      FI-DB:screen-value       = STRING(HPrisKo.DBKr)
      FI-DB%:screen-value      = STRING(HPrisKo.DB%)
      FI-Pris:screen-value     = STRING(HPrisKo.Pris)
      FI-EUPris:screen-value   = STRING(HPrisKo.EuroPris).
      IF HPrisKo.TYPE <> 2 THEN
      /*if HPrisKo.Tilbud = false then*/
        ASSIGN                             
          FI-NAktFra:screen-value = STRING(HPrisKo.AktiveresDato)
          FI-NTid:SCREEN-VALUE    = SUBSTRING(STRING(HPrisKo.AktiveresTid,"HH:MM"),1,2) + "," + 
                                    SUBstring(STRING(HPrisKo.AktiveresTid,"HH:MM"),4,2)
          FI-Txt1:HIDDEN   = TRUE
          FI-AktFra:HIDDEN = TRUE 
          FI-Tid1:HIDDEN   = TRUE 
          FI-AktTil:HIDDEN = TRUE 
          FI-Tid2:HIDDEN   = TRUE 
          btnAktFra:HIDDEN = TRUE
          btnAktTil:HIDDEN = TRUE
          .
      ELSE
        ASSIGN
          FI-Txt2:HIDDEN    = TRUE
          FI-NAktFra:HIDDEN = TRUE 
          FI-NTid:HIDDEN    = TRUE 
          btnNAktFra:HIDDEN = TRUE
          FI-AktFra:screen-value  = STRING(HPrisKo.AktiveresDato)      
          FI-Tid1:SCREEN-VALUE    = SUBSTRING(STRING(HPrisKo.AktiveresTid,"HH:MM"),1,2) + "," + 
                                    SUBstring(STRING(HPrisKo.AktiveresTid,"HH:MM"),4,2)
          FI-AktTil:screen-value  = STRING(HPrisKo.GyldigTilDato)
          FI-Tid2:SCREEN-VALUE    = SUBSTRING(STRING(HPrisKo.GyldigTilTid,"HH:MM"),1,2) + "," + 
                                    SUBstring(STRING(HPrisKo.GyldigTilTid,"HH:MM"),4,2)
          .
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-PrisKoAlle
&Scoped-define SELF-NAME BROWSE-PrisKoAlle
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-PrisKoAlle C-Kalkyle
ON MOUSE-MENU-DOWN OF BROWSE-PrisKoAlle IN FRAME FRAME-PrisInfo
DO:
  DEFINE VARIABLE hMenu AS HANDLE     NO-UNDO.
 ASSIGN hMenu = BROWSE BROWSE-PrisKo:POPUP-MENU
        hMenu:SENSITIVE = BROWSE BROWSE-PrisKo:FOCUSED-ROW <> ?.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-PrisKoAlle C-Kalkyle
ON MOUSE-SELECT-CLICK OF BROWSE-PrisKoAlle IN FRAME FRAME-PrisInfo
OR "RETURN":U OF BROWSE-PrisKo
DO:
  {dispprisko.i &Tabell="PrisKo"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-PrisKoAlle C-Kalkyle
ON ROW-DISPLAY OF BROWSE-PrisKoAlle IN FRAME FRAME-PrisInfo
DO:
  ASSIGN
    w2ETid  = STRING(PrisKo.ETid,"HH:MM:SS")
    wAktTid = STRING(PrisKo.AktiveresTid,"HH:MM:SS")
    wAktTid = IF wAktTid = "00:00:00"
                THEN ""
                ELSE wAktTid
    wTilTid = STRING(PrisKo.GyldigTilTid,"HH:MM:SS")
    wTilTid = IF wTilTid = "00:00:00"
                THEN ""
                ELSE wTilTid
    w2Type  = IF PrisKo.Type = 1 
                THEN "NOR"
              ELSE IF PrisKo.Type = 2 
                THEN "PÅ"
              ELSE IF PrisKo.Type = 4 
                THEN "ETIL"
              ELSE IF PRisKo.TYPE = 5 
                THEN "LPÅ"
              ELSE IF PRisKo.TYPE = 6 
                THEN "LAV"
              ELSE "AV"    
    w2Type:bgcolor IN BROWSE BROWSE-PrisKoAlle = IF PrisKo.Type = 1
                                               THEN ?
                                             ELSE IF PrisKo.Type = 2
                                               THEN 12
                                             ELSE IF PrisKo.Type = 4
                                               THEN 13
                                             ELSE IF PrisKo.TYPE = 5
                                               THEN 4
                                             ELSE IF PrisKo.TYPE = 6
                                               THEN 4
                                             ELSE 12.
  IF AVAILABLE HPrisKo THEN
    DO:
      ASSIGN
      FI-ValPris:screen-value  = STRING(HPrisKo.ValPris)
      FI-InnPris:screen-value  = STRING(HPrisKo.InnkjopsPris)
      FI-Rab1:screen-value     = STRING(HPrisKo.Rab1Kr)
      FI-Rab1%:screen-value    = STRING(HPrisKo.Rab1%)
      FI-Rab2:screen-value     = STRING(HPrisKo.Rab2Kr)
      FI-Rab2%:screen-value    = STRING(HPrisKo.Rab2%)
      FI-Frakt:screen-value    = STRING(HPrisKo.Frakt)
      FI-Frakt%:screen-value   = STRING(HPrisKo.Frakt%)
      FI-DivKost:screen-value  = STRING(HPrisKo.DivKostKr)
      FI-DivKost%:screen-value = STRING(HPrisKo.DivKost%)
      FI-Rab3:screen-value     = STRING(HPrisKo.Rab3Kr)
      FI-Rab3%:screen-value    = STRING(HPrisKo.Rab3%)
      FI-VareKost:screen-value = STRING(HPrisKo.VareKost)
      FI-Mva:screen-value      = STRING(HPrisKo.MvaKr)
      FI-Mva%:screen-value     = STRING(HPrisKo.Mva%)
      FI-DB:screen-value       = STRING(HPrisKo.DBKr)
      FI-DB%:screen-value      = STRING(HPrisKo.DB%)
      FI-Pris:screen-value     = STRING(HPrisKo.Pris)
      FI-EUPris:screen-value   = STRING(HPrisKo.EuroPris).
      IF HPrisKo.TYPE <> 2 THEN
      /*if HPrisKo.Tilbud = false then*/
        ASSIGN                             
          FI-NAktFra:screen-value = STRING(HPrisKo.AktiveresDato)
          FI-NTid:SCREEN-VALUE    = SUBSTRING(STRING(HPrisKo.AktiveresTid,"HH:MM"),1,2) + "," + 
                                    SUBstring(STRING(HPrisKo.AktiveresTid,"HH:MM"),4,2)
          FI-Txt1:HIDDEN   = TRUE
          FI-AktFra:HIDDEN = TRUE 
          FI-Tid1:HIDDEN   = TRUE 
          FI-AktTil:HIDDEN = TRUE 
          FI-Tid2:HIDDEN   = TRUE 
          btnAktFra:HIDDEN = TRUE
          btnAktTil:HIDDEN = TRUE
          .
      ELSE
        ASSIGN
          FI-Txt2:HIDDEN    = TRUE
          FI-NAktFra:HIDDEN = TRUE 
          FI-NTid:HIDDEN    = TRUE 
          btnNAktFra:HIDDEN = TRUE
          FI-AktFra:screen-value  = STRING(HPrisKo.AktiveresDato)      
          FI-Tid1:SCREEN-VALUE    = SUBSTRING(STRING(HPrisKo.AktiveresTid,"HH:MM"),1,2) + "," + 
                                    SUBstring(STRING(HPrisKo.AktiveresTid,"HH:MM"),4,2)
          FI-AktTil:screen-value  = STRING(HPrisKo.GyldigTilDato)
          FI-Tid2:SCREEN-VALUE    = SUBSTRING(STRING(HPrisKo.GyldigTilTid,"HH:MM"),1,2) + "," + 
                                    SUBstring(STRING(HPrisKo.GyldigTilTid,"HH:MM"),4,2)
          .
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-Prisprofil
&Scoped-define SELF-NAME BROWSE-Prisprofil
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-Prisprofil C-Kalkyle
ON MOUSE-SELECT-CLICK OF BROWSE-Prisprofil IN FRAME FRAME-PrisInfo
OR "RETURN":U OF BROWSE-Prisprofil
DO:
  {dispartpris.i &Tabell="ArtPris" &Felt="ArtikkelNr"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-Prisprofil C-Kalkyle
ON ROW-DISPLAY OF BROWSE-Prisprofil IN FRAME FRAME-PrisInfo
DO:
  ASSIGN
    wAktTid = STRING(ArtPris.AktivFraTid,"HH:MM:SS")
    wAktTid = IF wAktTid = "00:00:00"
                THEN ""
                ELSE wAktTid
    wETid = STRING(ArtPris.ETid,"HH:MM:SS")
    .
              
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAktFra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAktFra C-Kalkyle
ON CHOOSE OF btnAktFra IN FRAME FRAME-PrisInfo /* ... */
DO:
  RUN Cal.w (FI-AktFra:HANDLE).
  APPLY "ENTRY" TO SELF.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAktTil
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAktTil C-Kalkyle
ON CHOOSE OF btnAktTil IN FRAME FRAME-PrisInfo /* ... */
DO:
  RUN Cal.w (FI-AktTil:HANDLE).
  APPLY "ENTRY" TO SELF.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnNAktFra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnNAktFra C-Kalkyle
ON CHOOSE OF btnNAktFra IN FRAME FRAME-PrisInfo /* ... */
DO:
  RUN Cal.w (FI-NAktFra:HANDLE).
  APPLY "ENTRY" TO SELF.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-Tilbud
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Tilbud C-Kalkyle
ON VALUE-CHANGED OF CB-Tilbud IN FRAME FRAME-PrisInfo
DO:
  DEFINE VARIABLE iTidFra AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iTidTil AS INTEGER     NO-UNDO.
  IF AVAILABLE ArtBas THEN 
  DO: 
    IF ArtBas.OPris THEN
    DO:
        IF int(SELF:SCREEN-VALUE) > 1 THEN
        DO:
            SELF:SCREEN-VALUE = "1".
            MESSAGE "Tilbud kan ikke registreres på artikkler med åpen pris (PLU)."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RETURN NO-APPLY.
        END.
    END.
  END.

  ASSIGN FRAME FRAME-PrisInfo
    CB-Tilbud
    /*T-Tilbud*/
    .

  RUN VisTilbudsFlagg (1).
  IF (CB-Tilbud = 2 OR CB-Tilbud = 5) THEN
  /*if T-Tilbud then*/
    DO:
      IF NOT dPgmStart = TODAY THEN DO:
          dPgmStart  = TODAY.
          dTilBudFra = DATETIME(dPgmStart + 1).
          dTilBudTil = DATETIME(dPgmStart + 3) - 1000. 
      END.
      ELSE IF dTilBudFra = ? THEN
          ASSIGN dTilBudFra = DATETIME(dPgmStart + 1)
                 dTilBudTil = DATETIME(dPgmStart + 3) - 1000. 

      ASSIGN
/*         FI-AktFra:SCREEN-VALUE  = STRING(TODAY + 1)                                       */
/*         FI-Tid1:SCREEN-VALUE    = (SUBSTRING(STRING(0,"HH:MM"),1,2) + "," +               */
/*                                    SUBstring(STRING(0,"HH:MM"),4,2))                      */
/*         FI-AktTil:SCREEN-VALUE  = STRING(TODAY + 2)                                       */
/*         FI-Tid2:SCREEN-VALUE    = '23,59' /*(SUBSTRING(STRING(86000,"HH:MM"),1,2) + "," + */
/*                                    SUBstring(STRING(86000,"HH:MM"),4,2))*/                */
          iTidFra = MTIME(dTilBudFra) / 1000
          iTidTil = MTIME(dTilBudTil) / 1000

          FI-AktFra:SCREEN-VALUE  = STRING(DATE(dTilBudFra))
          FI-Tid1:SCREEN-VALUE    = (SUBSTRING(STRING(iTidFra,"HH:MM"),1,2) + "," + 
                                     SUBstring(STRING(iTidFra,"HH:MM"),4,2))
          FI-AktTil:SCREEN-VALUE  = STRING(dTilBudTil)
          FI-Tid2:SCREEN-VALUE    = (SUBSTRING(STRING(iTidTil,"HH:MM"),1,2) + "," + 
                                     SUBstring(STRING(iTidTil,"HH:MM"),4,2))
        FI-NAktFra = ?
        FI-NTid    = 0
        FI-Txt2:HIDDEN    = TRUE
        FI-NAktFra:HIDDEN = TRUE 
        FI-NTid:HIDDEN    = TRUE 
        btnNAktFra:HIDDEN = TRUE
        .
    END.
  ELSE DO:
      ASSIGN
        FI-AktFra  = ?
        FI-Tid1    = 0
        FI-AktTil  = ?
        FI-Tid2    = 0
        FI-NAktFra:SCREEN-VALUE = STRING(TODAY)
        FI-NTid:SCREEN-VALUE    = (SUBSTRING(STRING(TIME - 600,"HH:MM"),1,2) + "," + 
                                   SUBstring(STRING(TIME - 600,"HH:MM"),4,2))
        FI-Txt1:HIDDEN   = TRUE
        FI-AktFra:HIDDEN = TRUE 
        FI-Tid1:HIDDEN   = TRUE 
        FI-AktTil:HIDDEN = TRUE 
        FI-Tid2:HIDDEN   = TRUE 
        btnAktFra:HIDDEN = TRUE
        btnAktTil:HIDDEN = TRUE
        .
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-AktFra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-AktFra C-Kalkyle
ON LEAVE OF FI-AktFra IN FRAME FRAME-PrisInfo
DO:
  ASSIGN
    FI-AktFra.
  
  /* Er det et tilbud og det kun ligger en av tilbudspost i køen, skal aktiveringsdato */
  /* kopieres til fra dato feltet.                                                     */
  IF INPUT CB-Tilbud = 2 OR CB-Tilbud = 5 THEN
  /*if INPUT T-Tilbud = true then*/
  FIND FIRST PrisKo NO-LOCK WHERE
    PrisKo.ArtikkelNr    = ArtBas.ArtikkelNr AND
    PrisKo.ProfilNr      = PrisProfil.ProfilNr AND
    PrisKo.Tilbud        = (IF INPUT CB-Tilbud = 2
                              THEN TRUE
                              ELSE FALSE) AND
    PrisKo.Type          > 1 AND
    PrisKo.TYPE          < 4 NO-ERROR.
  IF AVAILABLE PrisKo THEN
    DO:
     IF PrisKo.TYPE = 3 OR PrisKo.TYPE = 5 THEN
       ASSIGN
         FI-AktTil = FI-AktFra
         FI-AktTil:SCREEN-VALUE = STRING(FI-AktFra).
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-AktFra-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-AktFra-2 C-Kalkyle
ON TAB OF FI-AktFra-2 IN FRAME FRAME-PrisInfo
OR "RETURN":U OF FI-AktFra
DO:
  ASSIGN
    /*T-Tilbud*/
    CB-Tilbud
    .
  IF (CB-Tilbud <> 2 AND CB-Tilbud <> 5) THEN
  /*if T-Tilbud = false then*/
    DO:
      APPLY "entry":U TO B-Godkjenn IN FRAME FRAME-PrisInfo.
      RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-AktTil-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-AktTil-2 C-Kalkyle
ON TAB OF FI-AktTil-2 IN FRAME FRAME-PrisInfo
OR "return":U OF FI-AktTil
DO:
  ASSIGN
    /*T-Tilbud*/
    CB-Tilbud
    .
  IF (CB-Tilbud <> 2 AND CB-Tilbud <> 5) THEN
  /*if T-Tilbud = false then*/
    DO:
      APPLY "entry":U TO B-Godkjenn IN FRAME FRAME-PrisInfo.
      RETURN NO-APPLY.
    END.

  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-DB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-DB C-Kalkyle
ON LEAVE OF FI-DB IN FRAME FRAME-PrisInfo /* DB (+) */
DO:
  RUN Kalkulasjon (1).
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-DB%
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-DB% C-Kalkyle
ON LEAVE OF FI-DB% IN FRAME FRAME-PrisInfo
DO:
  IF INPUT FI-DB% > 99.99 THEN
    DO:
      MESSAGE "Du kan ikke ha mer enn 99.99 i DB%!" 
              VIEW-AS ALERT-BOX TITLE "Kalkulasjonsfeil".
      RETURN NO-APPLY.
    END.
    RUN Kalkulasjon (1).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-DivKost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-DivKost C-Kalkyle
ON LEAVE OF FI-DivKost IN FRAME FRAME-PrisInfo /* Div. kost (+) */
DO:
  RUN Kalkulasjon (1).
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-DivKost%
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-DivKost% C-Kalkyle
ON LEAVE OF FI-DivKost% IN FRAME FRAME-PrisInfo
DO:
  RUN Kalkulasjon (1).
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Frakt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Frakt C-Kalkyle
ON LEAVE OF FI-Frakt IN FRAME FRAME-PrisInfo /* Frakt (+) */
DO:
  RUN Kalkulasjon (1).
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Frakt%
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Frakt% C-Kalkyle
ON LEAVE OF FI-Frakt% IN FRAME FRAME-PrisInfo
DO:
  RUN Kalkulasjon (1).
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-NAktFra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-NAktFra C-Kalkyle
ON LEAVE OF FI-NAktFra IN FRAME FRAME-PrisInfo
DO:
  ASSIGN
    FI-NAktFra.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-NTid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-NTid C-Kalkyle
ON TAB OF FI-NTid IN FRAME FRAME-PrisInfo /* Kl */
OR "RETURN":U OF FI-NTid
DO:
  ASSIGN
    /*T-Tilbud*/
    CB-Tilbud
    .
  IF (CB-Tilbud <> 2 AND CB-Tilbud <> 5) THEN
  /*if T-Tilbud = false then*/
    DO:
      APPLY "entry":U TO B-Godkjenn IN FRAME FRAME-PrisInfo.
      RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Pris
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Pris C-Kalkyle
ON LEAVE OF FI-Pris IN FRAME FRAME-PrisInfo /* Pris */
DO:
  RUN Kalkulasjon (1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-ProfilNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-ProfilNr C-Kalkyle
ON TAB OF FI-ProfilNr IN FRAME FRAME-PrisInfo /* Prisprofil */
OR "RETURN":U OF FI-ProfilNr 
DO:
  ASSIGN
    FI-ProfilNr.
  FIND PrisProfil NO-LOCK WHERE
    PrisProfil.ProfilNr = INPUT FI-ProfilNr NO-ERROR.
  IF NOT AVAILABLE PrisProfil THEN
    DO:
      MESSAGE "Ugyldig prosprofil!"
              VIEW-AS ALERT-BOX TITLE "Melding".
      RETURN NO-APPLY.    
    END.
  IF INPUT FI-ProfilNr > 1 AND NOT CAN-FIND(ArtPris NO-LOCK WHERE
      ArtPris.ArtikkelNr = wArtikkelNr AND
      ArtPris.ProfilNr   = INPUT FI-ProfilNr) THEN
  DO:
      MESSAGE "Det er ikke lagt opp pris på den agitte prisprofil." SKIP
              "Bruk NyPris knappen for å legge opp en pris. " SKIP(1)
              "Angi ønsket profil som det skal legges opp ny pris på, og trykk deretter på knappen NyPris"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      FIND PrisProfil NO-LOCK WHERE
          PrisProfil.ProfilNr = clButiker.ProfilNr NO-ERROR.
      DISPLAY 
        PrisProfil.ProfilNr    @ FI-PRofilNr
        Prisprofil.Beskrivelse @ FI-Beskrivelse
      WITH FRAME FRAME-PrisInfo.
      RETURN NO-APPLY.
  END.
  ELSE DO:
    DISPLAY 
      PrisProfil.ProfilNr    @ FI-PRofilNr
      Prisprofil.Beskrivelse @ FI-Beskrivelse
    WITH FRAME FRAME-PrisInfo.

    FIND ArtPris NO-LOCK WHERE
      ArtPris.ArtikkelNr = wArtikkelNr AND
      ArtPris.ProfilNr   = INPUT FI-ProfilNr NO-ERROR.

    IF NOT AVAILABLE ArtPris THEN
    DO:
        FIND ArtPris NO-LOCK WHERE
          ArtPris.ArtikkelNr = wArtikkelNr AND
          ArtPris.ProfilNr   = clButiker.ProfilNr NO-ERROR.
        FIND PrisProfil NO-LOCK WHERE
          PrisProfil.ProfilNr = clButiker.ProfilNr NO-ERROR.
        DISPLAY 
          PrisProfil.ProfilNr    @ FI-PRofilNr
          Prisprofil.Beskrivelse @ FI-Beskrivelse
        WITH FRAME FRAME-PrisInfo.
    END.

    RUN InitKalkyle (FALSE,1).
    /*
    run VisPrisStatus.
    */
    {&OPEN-QUERY-BROWSE-HPrisKo}
    {&OPEN-QUERY-BROWSE-PrisKo}
    {&OPEN-QUERY-BROWSE-PrisKoAlle}
/*                 {&OPEN-BROWSERS-IN-QUERY-FRAME-PrisInfo} */
    RUN VisTilbudsFlagg (1).
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Rab1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Rab1 C-Kalkyle
ON LEAVE OF FI-Rab1 IN FRAME FRAME-PrisInfo /* Rabatt 1 (-) */
DO:
  RUN Kalkulasjon (1).
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Rab1%
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Rab1% C-Kalkyle
ON LEAVE OF FI-Rab1% IN FRAME FRAME-PrisInfo
DO:
  RUN Kalkulasjon (1).
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Rab2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Rab2 C-Kalkyle
ON LEAVE OF FI-Rab2 IN FRAME FRAME-PrisInfo /* Rabatt 2 (-) */
DO:
  RUN Kalkulasjon (1).
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Rab2%
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Rab2% C-Kalkyle
ON LEAVE OF FI-Rab2% IN FRAME FRAME-PrisInfo
DO:
  RUN Kalkulasjon (1).
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Rab3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Rab3 C-Kalkyle
ON LEAVE OF FI-Rab3 IN FRAME FRAME-PrisInfo /* Rabatt 3 (-) */
DO:
  RUN Kalkulasjon (1).
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Rab3%
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Rab3% C-Kalkyle
ON LEAVE OF FI-Rab3% IN FRAME FRAME-PrisInfo
DO:
  RUN Kalkulasjon (1).
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Tid2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Tid2 C-Kalkyle
ON TAB OF FI-Tid2 IN FRAME FRAME-PrisInfo /* Kl */
OR "return":U OF FI-Tid2
DO:
  ASSIGN
    /*T-Tilbud*/
    CB-Tilbud
    .
  IF (CB-Tilbud <> 2 AND CB-Tilbud <> 5) THEN
  /*if T-Tilbud = false then*/
    DO:
      APPLY "entry":U TO B-Godkjenn IN FRAME FRAME-PrisInfo.
      RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-ValPris
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-ValPris C-Kalkyle
ON LEAVE OF FI-ValPris IN FRAME FRAME-PrisInfo /* Valutapris */
DO:
  RUN Kalkulasjon (1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RS-Liste
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RS-Liste C-Kalkyle
ON VALUE-CHANGED OF RS-Liste IN FRAME FRAME-PrisInfo
DO:
  IF INPUT RS-Liste = 1 THEN
  DO:
      ASSIGN          
          BROWSE-Prisprofil:HIDDEN = TRUE
          BROWSE-PrisKo:HIDDEN    = FALSE
          BROWSE-PrisKoAlle:HIDDEN = TRUE
          BROWSE-HPrisKo:HIDDEN   = TRUE
          FI-Topptekst-7:HIDDEN    = TRUE  
          FI-Topptekst-6:HIDDEN    = TRUE            
          FI-Topptekst-3:HIDDEN   = FALSE  
          FI-Topptekst-4:HIDDEN   = TRUE
          B-SlettPrisKo:SENSITIVE = TRUE 
          B-EndreDato:SENSITIVE   = TRUE 
          B-Aktiver:SENSITIVE     = TRUE 
          B-EtiSimuler:SENSITIVE  = TRUE 
          .
      APPLY "ENTRY":U TO BROWSE-PrisKo.
  END.
  ELSE IF INPUT RS-Liste = 2 THEN
  DO:
      ASSIGN
          BROWSE-Prisprofil:HIDDEN = TRUE
          BROWSE-PrisKo:HIDDEN     = TRUE
          BROWSE-PrisKoAlle:HIDDEN = TRUE
          BROWSE-HPrisKo:HIDDEN    = FALSE 
          FI-Topptekst-7:HIDDEN    = TRUE  
          FI-Topptekst-6:HIDDEN    = TRUE  
          FI-Topptekst-3:HIDDEN    = TRUE 
          FI-Topptekst-4:HIDDEN    = FALSE
          B-SlettPrisKo:SENSITIVE = FALSE
          B-EndreDato:SENSITIVE   = FALSE
          B-Aktiver:SENSITIVE     = FALSE
          B-EtiSimuler:SENSITIVE  = FALSE 
          .
      APPLY "ENTRY":U TO BROWSE-HPrisKo.
  END.
  ELSE IF INPUT RS-Liste = 3 THEN
  DO:
      ASSIGN
          BROWSE-Prisprofil:HIDDEN = FALSE
          BROWSE-PrisKo:HIDDEN     = TRUE
          BROWSE-PrisKoAlle:HIDDEN = TRUE
          BROWSE-HPrisKo:HIDDEN    = TRUE 
          FI-Topptekst-7:HIDDEN    = TRUE  
          FI-Topptekst-6:HIDDEN    = FALSE  
          FI-Topptekst-3:HIDDEN    = TRUE 
          FI-Topptekst-4:HIDDEN    = TRUE
          B-SlettPrisKo:SENSITIVE  = FALSE
          B-EndreDato:SENSITIVE    = FALSE
          B-Aktiver:SENSITIVE      = FALSE
          B-EtiSimuler:SENSITIVE  = FALSE 
          .
      APPLY "ENTRY":U TO BROWSE-Prisprofil.
  END.
  ELSE 
  DO:
      ASSIGN
          BROWSE-Prisprofil:HIDDEN = TRUE
          BROWSE-PrisKo:HIDDEN     = TRUE
          BROWSE-PrisKoAlle:HIDDEN = FALSE
          BROWSE-HPrisKo:HIDDEN    = TRUE 
          FI-Topptekst-7:HIDDEN    = FALSE  
          FI-Topptekst-6:HIDDEN    = TRUE  
          FI-Topptekst-3:HIDDEN    = TRUE 
          FI-Topptekst-4:HIDDEN    = TRUE
          B-SlettPrisKo:SENSITIVE  = FALSE
          B-EndreDato:SENSITIVE    = FALSE
          B-Aktiver:SENSITIVE      = FALSE
          B-EtiSimuler:SENSITIVE  = FALSE 
          .
      APPLY "ENTRY":U TO BROWSE-Prisprofil.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RS-VisKalkyle
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RS-VisKalkyle C-Kalkyle
ON VALUE-CHANGED OF RS-VisKalkyle IN FRAME FRAME-PrisInfo
DO:
  RUN VisInfoHeading.
  RUN InitKalkyle (FALSE,INPUT INPUT RS-VisKalkyle + 1).
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-Manuel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-Manuel C-Kalkyle
ON RETURN OF T-Manuel IN FRAME FRAME-PrisInfo /* Manuel */
OR "tab":U OF T-Manuel
DO:
  IF INPUT T-Manuel = FALSE THEN
    /*apply "Entry":U to T-Tilbud in frame FRAME-PrisInfo.*/
    APPLY "Entry":U TO CB-Tilbud IN FRAME FRAME-PrisInfo.
  ELSE IF INPUT T-Manuel = TRUE AND FI-EuPris:sensitive IN FRAME FRAME-PRisInfo = TRUE THEN
    /*apply "Entry":U to T-Tilbud in frame FRAME-PrisInfo.*/
    APPLY "Entry":U TO CB-Tilbud IN FRAME FRAME-PrisInfo.
  ELSE
      APPLY "Entry":U TO FI-EuPris IN FRAME FRAME-PrisInfo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-Manuel C-Kalkyle
ON VALUE-CHANGED OF T-Manuel IN FRAME FRAME-PrisInfo /* Manuel */
DO:
  IF INPUT T-Manuel = FALSE THEN
    ASSIGN
      FI-EuPris:sensitive = FALSE.
  ELSE
    ASSIGN
      FI-EuPris:sensitive = TRUE.        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-HPrisKo
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Kalkyle 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = wCurrent-Window 
       THIS-PROCEDURE:CURRENT-WINDOW = wCurrent-Window.      

/* för att kunna hantera Tilbud bättre */
dPgmStart = DATE(TODAY).


/* Maksimal tillatt pris til kunde. */
{syspara.i 2 1 11 lMaxPris DEC}
IF lMaxPris = 0 THEN
    lMaxPris = 99999.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
DO:
  IF VALID-HANDLE(wParentHandle) THEN
    RUN SlettProg IN wParentHandle.
  
  RUN disable_UI.
END.
/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Setter handle til priskøfunksjoner. */
ASSIGN
    h_PrisKo = wh_PrisKo
    .

FIND Bruker WHERE Bruker.BrukerId = USERID("skotex") NO-LOCK.
IF AVAIL Bruker THEN DO:
    FIND BrukerGrp WHERE BrukerGrp.BrGrpNr = Bruker.BrGrpNr NO-LOCK NO-ERROR.
    IF NOT AVAIL BrukerGrp THEN DO:
        MESSAGE "Ugyldig brukergruppe." VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY "AVBRYT".
    END.
    ASSIGN wBrGrpNr = BrukerGrp.BrGrpNr.
END.

/* Flagger at systemet kjøres på en LapTop (Innkjøpssystem) */
IF VALID-HANDLE(wLibHandle) THEN
  RUN SjekkLapTop IN wLibHandle (OUTPUT wLapTop).

/* Automatisk opprette bestilling ved opprettelse av ny artikkel */
{syspara.i 5 4 100 wOpprettBestilling INT}

/* Henter sentrallager */
{syspara.i 5 1 1 wCl int}
FIND clButiker NO-LOCK WHERE
  clButiker.Butik = wCl NO-ERROR.
FI-Profilnr = clButiker.Profilnr.
/* Maks tillatt DB% */
{syspara.i 2 1 5 wMaksDB% DEC}
IF wMaksDB% <= 0 THEN
    wMaksDB% = 80.

/* Direkte oppdatering av priser fra kalkylen */
/*
if valid-handle(wLibHandle) then
  run DirektePrisOppdat in wLibHandle (output wDirektePrisOppdat).
*/  
ASSIGN
  wDirektePrisOppdat = FALSE.
  
/* Henter artikkelen */
FIND ArtBas NO-LOCK WHERE
  ArtBas.ArtikkelNr = wArtikkelNr NO-ERROR.
IF AVAILABLE ArtBas THEN
  ASSIGN
    wArtBasRecid = RECID(ArtBas).
ELSE DO:
  ASSIGN
    wArtBasRecid = ?.        
  RUN RensKalkyle.
  RETURN "AVBRYT".
END.
/* Henter relaterte poster til artikkelen */
FIND VarGr OF ArtBas NO-LOCK NO-ERROR.
FIND Moms OF VarGr NO-LOCK NO-ERROR.
FIND Valuta OF ArtBas NO-LOCK NO-ERROR.
FIND LevBas OF ArtBas NO-LOCK NO-ERROR.


/* Triggere */
ON ALT-T OF FRAME FRAME-PrisInfo ANYWHERE 
  DO:
    /*
    assign
      T-Tilbud = if T-Tilbud then false else true.
    display T-Tilbud with frame FRAME-PrisInfo.
    apply "VALUE-CHANGED" to T-Tilbud in frame FRAME-PrisInfo.
    */
    ASSIGN
      CB-Tilbud = IF (CB-Tilbud = 2 OR CB-Tilbud = 5) THEN 1 ELSE 2.
    DISPLAY CB-Tilbud WITH FRAME FRAME-PrisInfo.
    APPLY "VALUE-CHANGED" TO CB-Tilbud IN FRAME FRAME-PrisInfo.
  END.
ON ALT-O OF FRAME FRAME-PrisInfo ANYWHERE 
  DO:
    APPLY "CHOOSE" TO B-Godkjenn.
  END.
ON ALT-T OF FRAME FRAME-PrisInfo ANYWHERE 
  DO:
    APPLY "CHOOSE" TO B-Tilbakestill.
  END.
ON ALT-K OF FRAME FRAME-PrisInfo ANYWHERE 
  DO:
    APPLY "CHOOSE" TO B-GodkjennKopier.
  END.

  

ASSIGN
  /*T-Tilbud               = false*/
  CB-Tilbud              = 1
  FI-ToppTekst           = "  Ordinær kalkyle"
  FI-Topptekst:BGCOLOR   = 9
  RS-VisKalkyle          = 1
  FI-ToppTekst-2         = "  Siste ordinær kalkyle"
  FI-Topptekst-2:BGCOLOR = 9
  .  
  RUN SkapaTT_PP.
/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

/*   &SCOP PROCEDURE-TYPE FRAME */
  &SCOP PROCEDURE-TYPE DIALOG /* NU FUNGERAR ÖVERSÄTTNING */
/*   {lng.i} */
  RUN UpdatePPArtpris.
  RUN enable_UI.
  {lng.i}
  ASSIGN 
      FI-ProfilNr:SENSITIVE             = FALSE
      ArtPris.MengdeRabAntall:SENSITIVE = ArtBas.MengdeRabatt
      ArtPris.MengdeRabPris:SENSITIVE   = ArtBas.MengdeRabatt
      .

  IF CAN-DO(SOURCE-PROCEDURE:INTERNAL-ENTRIES,"sendYpos") THEN DO:
      hSourceP = SOURCE-PROCEDURE.
      RUN sendYpos IN hSourceP (OUTPUT iFrameYpos) NO-ERROR.
      IF iFrameYpos > 0 THEN
          FRAME FRAME-PrisInfo:Y = iFrameYpos.
  END.

  B-GodkjennKopier:HIDDEN = Artbas.ModellFarge = 0.
  ASSIGN RS-Liste:SCREEN-VALUE IN FRAME FRAME-PrisInfo= '1'.
  APPLY "VALUE-CHANGED" TO RS-Liste IN FRAME FRAME-PrisInfo.

  /* Starter bibliotek for håndtering av priskooppdateringer. */
  IF NOT VALID-HANDLE(h_PrisKo) THEN
      RUN prisko.p PERSISTENT SET h_PrisKo.
  
  /* Tar vare på framens handle for å slette den ved avsluttning. */
  ASSIGN
      wFrameHandle = FRAME FRAME-PrisInfo:handle.

  RUN InitKalkyle (clButiker.ProfilNr,1).
  RUN VisPrisStatus.
  
/*   RS-Liste:RADIO-BUTTONS = "Priskø,1,Prishistorikk,2,Prisprofiler,3,Priskø (Alle),4". */

  {&OPEN-QUERY-BROWSE-HPrisKo}
  {&OPEN-QUERY-BROWSE-PrisKo}
  {&OPEN-QUERY-BROWSE-PrisKoAlle}
/*                 {&OPEN-BROWSERS-IN-QUERY-FRAME-PrisInfo} */
  ASSIGN
      C-Kalkyle:HIDDEN            = FALSE
      FRAME FRAME-PrisInfo:HIDDEN = FALSE
      CB-Tilbud:SCREEN-VALUE      = RS-VisKalkyle:SCREEN-VALUE
      .
  APPLY "VALUE-CHANGED" TO CB-Tilbud IN FRAME FRAME-PrisInfo.
  IF iFrameYpos = 0 THEN
/*       APPLY "ENTRY":U to FI-ProfilNr. */
      APPLY "ENTRY":U TO FI-ValPris.
  ELSE
      APPLY "LEAVE" TO FRAME FRAME-PrisInfo.
  SUBSCRIBE TO "ByttObjekt" IN wParenthandle.
  IF AVAIL artbas THEN
      FRAME FRAME-PrisInfo:SENSITIVE = artbas.sanertdato = ?.
  APPLY "VALUE-CHANGED" TO BROWSE Browse-KalkProfil.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByttObjekt C-Kalkyle 
PROCEDURE ByttObjekt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER ipArtikkelNr LIKE ArtBas.ArtikkelNr NO-UNDO.
  /* Henter artikkelen */
  IF ipArtikkelnr <> wArtikkelnr THEN DO:
      CLOSE QUERY BROWSE-KalkProfil.
  END.
  FIND ArtBas NO-LOCK WHERE
    ArtBas.ArtikkelNr = ipArtikkelNr NO-ERROR.
  IF AVAILABLE ArtBas THEN
    ASSIGN
      wArtikkelNr  = ipArtikkelNr
      wArtBasRecid = RECID(ArtBas).
  ELSE 
    RETURN "AVBRYT".
  /* Henter relaterte poster til artikkelen */
  FIND VarGr OF ArtBas NO-LOCK NO-ERROR.
  FIND Moms OF VarGr NO-LOCK NO-ERROR.
  FIND Valuta OF ArtBas NO-LOCK NO-ERROR.

  /*ASSIGN RS-Liste:SCREEN-VALUE IN FRAME FRAME-PrisInfo = "1".*/

  APPLY "VALUE-CHANGED":U TO RS-Liste IN FRAME FRAME-PrisInfo.
/*   IF TT_Prisprofil.profilnr > 0 THEN DO: */
/*   IF TT_Prisprofil.profilnr <> clButiker.profilnr THEN DO: */
/*       RUN InitKalkyle (TRUE,1).                            */
/*   END.                                                     */
/*   ELSE                                                     */
/*       RUN InitKalkyle (FALSE,1).                           */
  RUN InitKalkyle (TRUE,1).

  RUN VisPrisStatus.

  {&OPEN-QUERY-BROWSE-HPrisKo}
  {&OPEN-QUERY-BROWSE-PrisKo}
  {&OPEN-QUERY-BROWSE-PrisKoAlle}
  {&OPEN-QUERY-BROWSE-Prisprofil}
/*                 {&OPEN-BROWSERS-IN-QUERY-FRAME-PrisInfo} */
  RUN VisTilbudsFlagg(3).
  IF FRAME FRAME-PrisInfo:SENSITIVE THEN
      APPLY "ENTRY":U TO FI-ValPris IN FRAME FRAME-PrisInfo.
  FRAME FRAME-PrisInfo:SENSITIVE = artbas.sanertdato = ?.
  /* VPI info */
  IF AVAILABLE ArtBas THEN
  DO WITH FRAME FRAME-PrisInfo:
      ASSIGN
      FI-VPIDato:SCREEN-VALUE = STRING(ArtBas.VPIDato)
      FI-KatalogPris:SCREEN-VALUE = STRING(ArtBas.KatalogPris)
      FI-AnbefaltPris:SCREEN-VALUE = STRING(ArtBas.AnbefaltPris)
      FI-forhRab%:SCREEN-VALUE = STRING(ArtBas.ForhRab%)
      FI-supRab%:SCREEN-VALUE = STRING(ArtBas.SupRab%)
      ArtPris.MengdeRabAntall:SENSITIVE = ArtBas.MengdeRabatt
      ArtPris.MengdeRabPris:SENSITIVE   = ArtBas.MengdeRabatt
      B-SokValuta:SENSITIVE = ArtBas.OPris = FALSE.
      IF ArtBas.ValKod = '' THEN
          FI-ValKod:SCREEN-VALUE = ''.
      ELSE 
          FI-ValKod:SCREEN-VALUE = ArtBas.ValKod.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BytVis C-Kalkyle 
PROCEDURE BytVis :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME FRAME-PrisInfo:
/*       iTmpProfilNr = TT_Prisprofil.Profilnr. */

      RUN Byttobjekt (wArtikkelNr).

      FIND TT_Prisprofil WHERE TT_Prisprofil.profilnr = INPUT FI-Profilnr NO-ERROR.
      IF AVAIL TT_Prisprofil THEN DO:
          REPOSITION BROWSE-Kalkprofil TO ROWID ROWID(TT_Prisprofil).
          BROWSE BROWSE-Kalkprofil:SELECT-FOCUSED-ROW().
      END.

  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Kalkyle  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME FRAME-PrisInfo.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Kalkyle  _DEFAULT-ENABLE
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
  DISPLAY FI-ValKod FI-Kroner-1 FI-Prosent-1 FI-Valuta-1 RS-Liste FI-ProfilNr 
          FI-Beskrivelse FI-AktFra-2 FI-AktTil-2 FI-ValPris FI-InnPris 
          FI-InnPris-2 FI-Rab1 FI-Rab1% FI-Rab1-2 FI-Rab1%-2 FI-Rab2 FI-Rab2% 
          FI-Rab2-2 FI-Rab2%-2 FI-Frakt FI-Frakt% FI-Frakt-2 FI-Frakt%-2 
          FI-DivKost FI-DivKost% FI-DivKost-2 FI-DivKost%-2 FI-Rab3 FI-Rab3% 
          FI-Rab3-2 FI-Rab3%-2 FI-VareKost FI-VareKost-2 FI-DB FI-DB% FI-DB-2 
          FI-DB%-2 FI-MVA FI-Mva% FI-MVA-2 FI-Mva%-2 FI-Pris RS-VisKalkyle 
          FI-Pris-2 FI-EuPris CB-Tilbud FI-NAktFra FI-NTid FI-AktFra FI-Tid1 
          FI-AktTil FI-Tid2 FI-EuPris-2 T-Manuel FI-VisPrisStatus FI-ToppTekst 
          FI-ToppTekst-2 FI-ToppTekst-3 FI-Txt2 FI-Txt1 FI-ToppTekst-4 
          FI-Kroner-2 FI-Prosent-2 FI-KatalogPris FI-AnbefaltPris FI-forhRab% 
          FI-supRab% FI-VPIDato FI-ToppTekst-5 FI-ToppTekst-6 FI-ToppTekst-7 
          FI-MengdeRabTekst 
      WITH FRAME FRAME-PrisInfo.
  IF AVAILABLE ArtPris THEN 
    DISPLAY ArtPris.MengdeRabAntall ArtPris.MengdeRabPris 
      WITH FRAME FRAME-PrisInfo.
  ENABLE BROWSE-KalkProfil B-SokValuta B-Tilbakestill B-SupRab FI-Kroner-1 
         FI-Prosent-1 FI-Valuta-1 RS-Liste B-EndreDato B-SlettPrisKo 
         FI-ProfilNr FI-ValPris FI-Rab1 FI-Rab1% FI-Rab2 FI-Rab2% FI-Frakt 
         FI-Frakt% FI-DivKost FI-DivKost% FI-Rab3 FI-Rab3% FI-DB FI-DB% FI-Pris 
         RS-VisKalkyle CB-Tilbud FI-NAktFra FI-NTid BROWSE-PrisKo FI-AktFra 
         FI-Tid1 FI-AktTil FI-Tid2 B-Godkjenn B-Aktiver BROWSE-HPrisKo T-Manuel 
         FI-ToppTekst FI-ToppTekst-2 FI-ToppTekst-3 FI-ToppTekst-4 FI-Kroner-2 
         FI-Prosent-2 btnNAktFra btnAktFra btnAktTil B-GodkjennKopier B-ForhRab 
         B-Katalogpris B-VeilPris FI-ToppTekst-5 B-NyKalkyle FI-ToppTekst-6 
         FI-ToppTekst-7 B-EtiSimuler FI-MengdeRabTekst ArtPris.MengdeRabAntall 
         ArtPris.MengdeRabPris RECT-36 RECT-37 RECT-38 RECT-39 RECT-40 RECT-41 
         RECT-42 RECT-43 RECT-44 RECT-56 RECT-62 BROWSE-Prisprofil RECT-72 
         BROWSE-PrisKoAlle 
      WITH FRAME FRAME-PrisInfo.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-PrisInfo}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitKalkyle C-Kalkyle 
PROCEDURE InitKalkyle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER wHentProfil AS LOG NO-UNDO.
  DEF INPUT PARAMETER wSetNr AS INT NO-UNDO. /* 1-Kalk. og Visningsramme */
                                             /* 2-Visningsramme ordinær  */
                                             /* 3-Visningsramme tilbud.  */
  DEF VAR wFeltNr    AS INT  NO-UNDO.
  DEF VAR wSkjerm    AS CHAR NO-UNDO.
  
  B-GodkjennKopier:HIDDEN IN FRAME {&FRAME-NAME} = Artbas.ModellFarge = 0.
  FIND PrisProfil WHERE 
      Prisprofil.profilnr = TT_Prisprofil.profilnr NO-LOCK NO-ERROR.
  IF NOT AVAILABLE PrisProfil THEN
  DO:
      FIND FIRST TT_Prisprofil WHERE
          TT_PrisProfil.ProfilNr = clButiker.profilnr NO-ERROR.
      FIND PrisProfil WHERE Prisprofil.profilnr = TT_Prisprofil.profilnr NO-LOCK NO-ERROR.
  END.
  
  IF AVAILABLE Valuta THEN RELEASE Valuta.
  FIND Valuta OF ArtBas NO-LOCK NO-ERROR. 
  
  IF NOT AVAILABLE PrisProfil THEN
    DO:
      MESSAGE "Det er ikke definert noen prisprofil!"
              VIEW-AS ALERT-BOX TITLE "Kalkulasjonsfeil".
      RETURN NO-APPLY.
    END.
  ELSE
    ASSIGN
      FI-ProfilNr    = PrisProfil.ProfilNr
      FI-Beskrivelse = PrisProfil.Beskrivelse.
  
  ASSIGN
    FI-ValKod = IF AVAILABLE Valuta 
                  THEN Valuta.ValKod
                  ELSE "".                                        
      
  /* Legger prisprofil opp i skjermen. */
  DISPLAY 
    FI-ProfilNr
    FI-Beskrivelse
    FI-ValKod 
    ArtPris.MengdeRabAnt WHEN AVAILABLE ArtPris
    ArtPris.MengdeRabPris WHEN AVAILABLE ArtPris
  WITH FRAME FRAME-PrisInfo.    
  
  /* Starter alltid fra valutapris. */
  ASSIGN
    wFeltNr = 1.
  /* Henter prisen. Hvis ny, slippes current record. */
  IF wModus <> "NY" THEN
    DO:
      FIND ArtPris NO-LOCK WHERE
        ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
        ArtPris.ProfilNr   = PrisProfil.ProfilNr NO-ERROR. 
      IF wHentProfil THEN
      DO:
        IF NOT AVAILABLE ArtPris THEN DO:
            FIND FIRST ArtPris OF ArtBas NO-LOCK NO-ERROR.
            IF AVAILABLE ArtPris THEN
                FIND PrisProfil OF ArtPris NO-LOCK NO-ERROR.
        END.
        ASSIGN
            FI-ProfilNr   = IF AVAILABLE ArtPris
                              THEN ArtPris.ProfilNr
                              ELSE FI-ProfilNr
            FI-Beskrivelse = IF AVAILABLE PrisProfil
                              THEN PrisProfil.Beskrivelse
                              ELSE FI-Beskrivelse
            .
        /* Legger prisprofil opp i skjermen. */
        DISPLAY 
          FI-ProfilNr
          FI-Beskrivelse
        WITH FRAME FRAME-PrisInfo.    
      END.
    END.
  ELSE DO:
    IF AVAILABLE ArtPris THEN
      RELEASE ArtPris.
  END.

  IF NOT AVAILABLE ArtPris THEN 
    RENSFRAME:
    DO: 
      /* Renser feltene i kalkulatoren */
      ASSIGN
        FI-ValPris:screen-value  = ""
        FI-InnPris:screen-value  = ""
        FI-Rab1:screen-value     = "" 
        FI-Rab1%:screen-value    = "" 
        FI-Rab2:screen-value     = "" 
        FI-Rab2%:screen-value    = "" 
        FI-Frakt:screen-value    = "" 
        FI-Frakt%:screen-value   = "" 
        FI-DivKost:screen-value  = "" 
        FI-DivKost%:screen-value = "" 
        FI-Rab3:screen-value     = "" 
        FI-Rab3%:screen-value    = "" 
        FI-VareKost:screen-value = "" 
        FI-Mva:screen-value      = "" 
        FI-Mva%:screen-value     = "" 
        FI-DB:screen-value       = "" 
        FI-DB%:screen-value      = "" 
        FI-Pris:screen-value     = "" 
        FI-EUPris:screen-value   = "" 
        T-Manuel                 = FALSE
        .
        IF (CB-Tilbud <> 2 AND CB-Tilbud <> 5) THEN
        /*if T-Tilbud = false then*/
          ASSIGN                             
            FI-NAktFra:screen-value  = ""
            FI-NTid:SCREEN-VALUE     = ""
            FI-AktFra:screen-value   = ""
            FI-Tid1:SCREEN-VALUE     = ""
            FI-AktTil:screen-value   = ""
            FI-Tid2:SCREEN-VALUE     = ""
            .
        ELSE
          ASSIGN          
            FI-AktFra:screen-value   = ""      
            FI-Tid2:SCREEN-VALUE     = ""
            FI-AktTil:screen-value   = ""
            FI-Tid2:SCREEN-VALUE     = ""
            FI-NAktFra:screen-value  = ""
            FI-NTid:SCREEN-VALUE     = ""
            .
    END. /* RENSFRAME */
    
  /* Henter kalkylen.                        */
  /* Artpris opprettes hvis den ikke finnes. */  
  IF wSetNr = 1 THEN
  FRAME-SCOOPE:
  DO WITH FRAME FRAME-PrisInfo:
   IF AVAILABLE ArtPris THEN
     ASSIGN
       RS-VisKalkyle = IF ArtPris.Tilbud THEN 2 ELSE 1.
   ELSE
     ASSIGN
       RS-VisKalkyle = 1.          

   DISPLAY RS-VisKalkyle WITH FRAME FRAME-PrisInfo.
   
   RUN VisInfoHeading.

   /* Henter kalkyle. */
   /* NB: Kalkulasjonen skjer i prosedyrebilboteket. */

   IF VALID-HANDLE(h_PrisKo) THEN
     RUN InitKalkyle IN h_PrisKo
          (INPUT wArtBasRecid, 
           INPUT PrisProfil.ProfilNr,
           INPUT-OUTPUT wSkjerm,
           INPUT (IF AVAILABLE Moms THEN Moms.MomsProc ELSE 0),
           INPUT Valuta.ValKurs, 
           INPUT wFeltNr,
           INPUT (IF AVAILABLE ArtPris
                    THEN ArtPris.Tilbud
                    ELSE FALSE)).
    ELSE 
      MESSAGE "Prosedyrebiblotek er ikke startet!" 
              VIEW-AS ALERT-BOX TITLE "Kalkulasjonsfeil".
    /* Legger nye verdier opp på skjermen igjen. */
    ASSIGN
      FI-ValPris:screen-value  = ENTRY(1,wSkjerm,";")
      FI-InnPris:screen-value  = ENTRY(2,wSkjerm,";")
      FI-Rab1:screen-value     = ENTRY(3,wSkjerm,";")
      FI-Rab1%:screen-value    = ENTRY(4,wSkjerm,";")
      FI-Rab2:screen-value     = ENTRY(5,wSkjerm,";")
      FI-Rab2%:screen-value    = ENTRY(6,wSkjerm,";")
      FI-Frakt:screen-value    = ENTRY(7,wSkjerm,";")
      FI-Frakt%:screen-value   = ENTRY(8,wSkjerm,";")
      FI-DivKost:screen-value  = ENTRY(9,wSkjerm,";")
      FI-DivKost%:screen-value = ENTRY(10,wSkjerm,";")
      FI-Rab3:screen-value     = ENTRY(11,wSkjerm,";")
      FI-Rab3%:screen-value    = ENTRY(12,wSkjerm,";")
      FI-VareKost:screen-value = ENTRY(13,wSkjerm,";")
      FI-Mva:screen-value      = ENTRY(14,wSkjerm,";")
      FI-Mva%:screen-value     = ENTRY(15,wSkjerm,";")
      FI-DB:screen-value       = ENTRY(16,wSkjerm,";")
      FI-DB%:screen-value      = ENTRY(17,wSkjerm,";")
      FI-Pris:screen-value     = ENTRY(18,wSkjerm,";")
      FI-EUPris:screen-value   = ENTRY(19,wSkjerm,";")
      T-Manuel                 = (IF ENTRY(20,wSkjerm,";") = "true"
                                   THEN TRUE
                                   ELSE FALSE).
      IF (CB-Tilbud <> 2 AND CB-Tilbud <> 5) THEN
      /*if T-Tilbud = false then*/
        ASSIGN                             
          FI-NAktFra:screen-value  = STRING(TODAY)
          FI-NTid:SCREEN-VALUE     = SUBSTRING(STRING(TIME,"HH:MM"),1,2) + "," + 
                                     SUBstring(STRING(TIME,"HH:MM"),4,2)
          FI-AktFra:screen-value   = ""
          FI-Tid1:SCREEN-VALUE     = ""
          FI-AktTil:screen-value   = ""
          FI-Tid2:SCREEN-VALUE     = ""
          .
      ELSE
        ASSIGN          
          FI-AktFra:screen-value  = STRING(TODAY)      
          FI-Tid2:SCREEN-VALUE     = SUBSTRING(STRING(TIME,"HH:MM"),1,2) + "," + 
                                     SUBstring(STRING(TIME,"HH:MM"),4,2)
          FI-AktTil:screen-value  = STRING(TODAY + 1)
          FI-Tid2:SCREEN-VALUE     = "23,59"
          FI-NAktFra:screen-value = ""
          FI-NTid:SCREEN-VALUE    = ""
          .
    /* Tar vare på skjermstreng for å kunne detektere endringer */
    ASSIGN
        wOldSkjerm = wSkjerm.
    DISPLAY 
      T-Manuel 
    WITH FRAME FRAME-PrisInfo.
  END. /* FRAME-SCOOPE */
  
  /* Er artpris opprettet i procedurebibloteket, henter vi den her. */
  IF NOT AVAILABLE ArtPris THEN
    FIND ArtPris NO-LOCK WHERE
      ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
      ArtPris.ProfilNr   = PrisProfil.ProfilNr NO-ERROR.

  /* Oppdaterer visningskalkyle framen. */
  IF AVAILABLE ArtPris AND
    valid-handle(h_PrisKo) THEN
    RUN InitKalkyle IN h_PrisKo
         (INPUT wArtBasRecid, 
          INPUT PrisProfil.ProfilNr,
          INPUT-OUTPUT wSkjerm,
          INPUT Moms.MomsProc,
          INPUT Valuta.ValKurs, 
          INPUT wFeltNr,
          INPUT (IF wSetNr = 1
                   THEN ArtPris.Tilbud
                 ELSE IF wSetNr = 2
                   THEN FALSE
                 ELSE TRUE)).
  ELSE 
    ASSIGN
      wSkjerm = "0;0;0;0;0;0;0;0;0;0;0;0;0;0;" + 
                (IF AVAILABLE Moms 
                  THEN STRING(Moms.MomsProc)
                  ELSE "0") + 
                ";0;0;0;0".      

  /* Legger nye verdier opp på skjermen igjen. */
  ASSIGN
    FI-InnPris-2:screen-value  = ENTRY(2,wSkjerm,";")
    FI-Rab1-2:screen-value     = ENTRY(3,wSkjerm,";")
    FI-Rab1%-2:screen-value    = ENTRY(4,wSkjerm,";")
    FI-Rab2-2:screen-value     = ENTRY(5,wSkjerm,";")
    FI-Rab2%-2:screen-value    = ENTRY(6,wSkjerm,";")
    FI-Frakt-2:screen-value    = ENTRY(7,wSkjerm,";")
    FI-Frakt%-2:screen-value   = ENTRY(8,wSkjerm,";")
    FI-DivKost-2:screen-value  = ENTRY(9,wSkjerm,";")
    FI-DivKost%-2:screen-value = ENTRY(10,wSkjerm,";")
    FI-Rab3-2:screen-value     = ENTRY(11,wSkjerm,";")
    FI-Rab3%-2:screen-value    = ENTRY(12,wSkjerm,";")
    FI-VareKost-2:screen-value = ENTRY(13,wSkjerm,";")
    FI-Mva-2:screen-value      = ENTRY(14,wSkjerm,";")
    FI-Mva%-2:screen-value     = ENTRY(15,wSkjerm,";")
    FI-DB-2:screen-value       = ENTRY(16,wSkjerm,";")
    FI-DB%-2:screen-value      = ENTRY(17,wSkjerm,";")
    FI-Pris-2:screen-value     = ENTRY(18,wSkjerm,";")
    FI-EUPris-2:screen-value   = ENTRY(19,wSkjerm,";")
    ArtPris.MengdeRabAnt:SCREEN-VALUE  = STRING(ArtPris.MengdeRabAnt)
    ArtPris.MengdeRabPris:SCREEN-VALUE = STRING(ArtPris.MengdeRabPris).
  
  /* Oppdatering av Side 1 m.m */
  IF AVAILABLE ArtPris THEN
    DO:
       IF INPUT RS-VisKalkyle = 2 THEN
         DISPLAY
           ArtPris.TilbudFraDato @ FI-AktFra-2
           ArtPris.TilbudTilDato @ FI-AktTil-2
         WITH FRAME FRAME-PrisInfo.         
       ELSE
         DISPLAY
           ArtPris.AktivFraDato  @ FI-AktFra-2
           ""                    @ FI-AktTil-2
         WITH FRAME FRAME-PrisInfo.         
    END.
  ELSE DO:
    DISPLAY
      "" @ FI-AktFra-2
      "" @ FI-AktTil-2
    WITH FRAME FRAME-PrisInfo.         
  END.
  ASSIGN FRAME FRAME-PrisInfo:SENSITIVE = ArtBas.Artikkelnr <> ArtBas.Vg
         CB-Tilbud:SENSITIVE   = ArtBas.Opris = FALSE
/*          FI-ProfilNr:SENSITIVE = ArtBas.Opris = FALSE */
         FI-NAktFra:SENSITIVE  = ArtBas.Opris = FALSE
         FI-NTid:SENSITIVE     = ArtBas.Opris = FALSE.

  /* VPI info */
  IF AVAILABLE ArtBas THEN
  DO WITH FRAME FRAME-PrisInfo:
      ASSIGN
      FI-VPIDato:SCREEN-VALUE = STRING(ArtBas.VPIDato)
      FI-KatalogPris:SCREEN-VALUE = STRING(ArtBas.KatalogPris)
      FI-AnbefaltPris:SCREEN-VALUE = STRING(ArtBas.AnbefaltPris)
      FI-forhRab%:SCREEN-VALUE = STRING(ArtBas.ForhRab%)
      FI-supRab%:SCREEN-VALUE = STRING(ArtBas.SupRab%)
      .
  END.

  RUN UpdatePPArtpris.
  {&OPEN-QUERY-BROWSE-KalkProfil}
  FIND TT_PrisProfil WHERE TT_PrisProfil.profilnr = Prisprofil.profilnr NO-ERROR.
  IF AVAIL TT_Prisprofil THEN DO:
      REPOSITION BROWSE-KalkProfil TO ROWID ROWID(TT_PrisProfil).
      BROWSE BROWSE-KalkProfil:SELECT-FOCUSED-ROW() NO-ERROR.
      APPLY "ENTRY" TO FI-Valpris.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Kalkulasjon C-Kalkyle 
PROCEDURE Kalkulasjon :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER wSetNr AS INT NO-UNDO. /* 1 - Kalkulasjonsramme          */
                                             /* 2 - Visningsramme (Høyre side) */

  DEF VAR wFeltListe AS CHAR NO-UNDO.
  DEF VAR wFraFelt   AS CHAR NO-UNDO.
  DEF VAR wFeltNr    AS INT  NO-UNDO.
  DEF VAR wSkjerm    AS CHAR NO-UNDO.
  

  FRAME-SCOOPE:
  DO WITH FRAME FRAME-PrisInfo:
    IF FI-ValKod:SCREEN-VALUE = ? THEN
        FIND Valuta NO-LOCK WHERE
             Valuta.ValKod = '' NO-ERROR.
    ELSE
        FIND Valuta NO-LOCK WHERE
             Valuta.ValKod = FI-ValKod:SCREEN-VALUE IN FRAME FRAME-PrisInfo NO-ERROR.

    ASSIGN
      wFraFelt   = frame-field
      wFeltListe = "ValPris,InnPris,Rab1,Rab1%,Rab2,Rab2%,Frakt,Frakt%," + 
                   "DivKost,DivKost%,Rab3,Rab3%,VareKost,DB,DB%," +
                   "FI-Mva,FI-Mva%,Pris,EU-Pris".

    /* Finner i hvilket felt markøren sto når prosedyren ble kalt. */
    CASE wSetNr:
      WHEN 1 THEN ASSIGN 
                     wFraFelt = SUBSTRING(wFraFelt,4).
      WHEN 2 THEN ASSIGN 
                     wFraFelt = SUBSTRING(wFraFelt,4)
                     wFraFelt = SUBSTRING(wFraFelt,LENGTH(wFraFelt) - 2).
    END CASE. 
    ASSIGN
      wFeltNr = LOOKUP(wFraFelt,wFeltListe).

    /* Ukjent felt. */  
    IF wFeltNr = 0 THEN
      DO:
        /*message "Ukjent felt!" view-as alert-box title "Kalkylefeil".*/
        RETURN NO-APPLY.  
      END.

    /* Pakker ned verdiene som ligger i skjermen. */
    ASSIGN
      wSkjerm = KalkStreng().

    /* Det skal ikke skje noe ved tabbing mellom feltene                 */
    /* Gambler her på at det ikke kommer to artikkler med samme kalkyle. */
    /* Hvis så om atte, så gjør det ikke noe.                            */
    /*
    IF (wSjekkStreng <> wSkjerm) THEN
      DO:
        ASSIGN wSjekkStreng = wSkjerm.
      END.
    ELSE 
      RETURN NO-APPLY.
   */
   ASSIGN wSjekkStreng = wSkjerm.

   /* Starter omkalkulering.                         */
   /* NB: Kalkulasjonen skjer i prosedyrebilboteket. */
   IF VALID-HANDLE(h_PrisKo) THEN
     RUN Omregning IN h_PrisKo
          (INPUT wArtBasRecid, 
           INPUT PrisProfil.ProfilNr,
           INPUT-OUTPUT wSkjerm,
           INPUT Moms.MomsProc,
           INPUT Valuta.ValKurs, 
           INPUT wFeltNr,
           IF (INPUT CB-Tilbud = 2 OR INPUT CB-Tilbud = 5)
             THEN TRUE
             ELSE FALSE).
    ELSE 
      MESSAGE "Prosedyrebiblotek er ikke startet!" VIEW-AS ALERT-BOX.
                  
    /* Legger nye verier opp på skjermen igjen. */
    ASSIGN
      FI-ValPris:screen-value  = ENTRY(1,wSkjerm,";")
      FI-InnPris:screen-value  = ENTRY(2,wSkjerm,";")
      FI-Rab1:screen-value     = ENTRY(3,wSkjerm,";")
      FI-Rab1%:screen-value    = ENTRY(4,wSkjerm,";")
      FI-Rab2:screen-value     = ENTRY(5,wSkjerm,";")
      FI-Rab2%:screen-value    = ENTRY(6,wSkjerm,";")
      FI-Frakt:screen-value    = ENTRY(7,wSkjerm,";")
      FI-Frakt%:screen-value   = ENTRY(8,wSkjerm,";")
      FI-DivKost:screen-value  = ENTRY(9,wSkjerm,";")
      FI-DivKost%:screen-value = ENTRY(10,wSkjerm,";")
      FI-Rab3:screen-value     = ENTRY(11,wSkjerm,";")
      FI-Rab3%:screen-value    = ENTRY(12,wSkjerm,";")
      FI-VareKost:screen-value = ENTRY(13,wSkjerm,";")
      FI-Mva:screen-value      = ENTRY(14,wSkjerm,";")
      FI-Mva%:screen-value     = ENTRY(15,wSkjerm,";")
      FI-DB:screen-value       = ENTRY(16,wSkjerm,";")
      FI-DB%:screen-value      = ENTRY(17,wSkjerm,";")
      FI-Pris:screen-value     = ENTRY(18,wSkjerm,";")
      FI-EUPris:screen-value   = ENTRY(19,wSkjerm,";").
  END. /* FRAME-SCOOPE */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KontrollerOpris C-Kalkyle 
PROCEDURE KontrollerOpris :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE dVarekost AS DECIMAL    NO-UNDO.
    DO WITH FRAME FRAME-PrisInfo:

        IF INPUT FI-Valpris > 100 OR INPUT FI-Valpris <> INPUT FI-Varekost OR 
                 INPUT FI-Pris <> 100 + INPUT FI-MVA THEN DO:
            MESSAGE "Feil i PLU-kalkyle. " 
                 "Registrer Varekost '1-100'" UPDATE dVarekost.
                IF dVarekost = 0 OR dVarekost > 100 THEN
            RETURN "AVBRYT".
            ASSIGN FI-Pris:SCREEN-VALUE = STRING(100 + INPUT FI-MVA%)
                   FI-ValPris:SCREEN-VALUE = STRING(dVarekost)
                   FI-DB:SCREEN-VALUE = "0"
                   FI-DB%:SCREEN-VALUE = "0"
                   FI-Rab1:SCREEN-VALUE = "0"
                   FI-Rab1%:SCREEN-VALUE = "0"
                   FI-Rab2:SCREEN-VALUE = "0"
                   FI-Rab2%:SCREEN-VALUE = "0"
                   FI-Frakt:SCREEN-VALUE = "0"
                   FI-Frakt%:SCREEN-VALUE = "0"
                   FI-DivKost:SCREEN-VALUE = "0"
                   FI-DivKost%:SCREEN-VALUE = "0"
                   FI-Rab3:SCREEN-VALUE = "0"
                   FI-Rab3%:SCREEN-VALUE = "0".
                   
            APPLY "TAB" TO FI-ValPris.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LagreKalkyle C-Kalkyle 
PROCEDURE LagreKalkyle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR wSkjerm    AS CHAR NO-UNDO.
  DEF VAR wStatus    AS CHAR INITIAL "AVBRYT".
  DEF VAR wSvar      AS LOG FORMAT "Ja/Nei" INITIAL FALSE.
  
  DEF VAR pdDato     AS DATE  NO-UNDO.
  DEF VAR piTid      AS INT   NO-UNDO.
  DEF VAR pdDato2    AS DATE  NO-UNDO.
  DEF VAR piTid2     AS INT   NO-UNDO.
  DEF VAR piStatus   AS INT   NO-UNDO.
  DEF VAR pcError    AS CHAR  NO-UNDO.
  DEF VAR plSvar     AS LOG   NO-UNDO.
  DEF VAR plDirekte  AS LOG   NO-UNDO.
  DEF VAR rowPrisKo  AS RECID NO-UNDO.

  DEFINE BUFFER bArtPris FOR ArtPris.
  DEFINE BUFFER bArtBas FOR ArtBas.
/*   DEF VAR wBestHodeRecid AS RECID NO-UNDO. */

  FRAME-SCOOPE:
  DO WITH FRAME FRAME-PrisInfo:
  
  ASSIGN FRAME FRAME-PrisInfo
      /*T-Tilbud*/
      CB-Tilbud
      FI-AktFra
      FI-Tid1
      FI-AktTil
      FI-Tid2
      FI-NAktFra
      FI-NTid
      .
  IF (CB-Tilbud = 2 OR CB-Tilbud = 5) THEN DO:
  /*IF T-Tilbud THEN*/
      ASSIGN
        pdDato  = FI-AktFra
        piTid   = 60 * 60 * int(SUBSTRING(STRING(FI-Tid1,"99.99"),1,2)) +
                   60 * int(SUBSTRING(STRING(FI-Tid1,"99.99"),4,2))
        pdDato2 = FI-AktTil
        piTid2  = 60 * 60 * int(SUBSTRING(STRING(FI-Tid2,"99.99"),1,2)) +
                   60 * int(SUBSTRING(STRING(FI-Tid2,"99.99"),4,2))
        .
      dTilBudFra = DATETIME(pdDato,piTid * 1000).
      dTilBudTil = DATETIME(pdDato2,piTid2 * 1000).
  END.
  ELSE
      ASSIGN
          pdDato = FI-NAktFra
          piTid  = 60 * 60 * int(SUBSTRING(STRING(FI-NTid,"99.99"),1,2)) +
                   60 * int(SUBSTRING(STRING(FI-NTid,"99.99"),4,2))
          pdDato2 = ?
          piTid2  = 0
      .

  /* Er DB% i kalkyle null eller negativ, skal det bekreftes. */
  IF INPUT FI-DB% <= 0 OR INPUT FI-DB <= 0 THEN
  DO:
      ASSIGN
          wSvar = FALSE.
      MESSAGE "DB eller DB% er mindre eller lik 0." SKIP(1)
               "Skal kalkylen lagres?"
          VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Bekreft lagring" UPDATE wSvar.
      IF wSvar = FALSE THEN RETURN NO-APPLY wStatus.
  END.

  /* Overskrides MAKSDB%. */
  IF INPUT FI-DB% >= wMaksDB% THEN
  DO:
      ASSIGN
          wSvar = FALSE.
      MESSAGE "DB% er større eller lik maksimal tillatt DB%." SKIP(1)
               "Skal kalkylen lagres?"
          VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Bekreft lagring" UPDATE wSvar.
      IF wSvar = FALSE THEN RETURN NO-APPLY wStatus.
  END. 

  /* Overskrides MAKSPRIS */
  IF INPUT FI-Pris >= lMaxPris THEN
  DO:
      MESSAGE "Angitt pris (" + FI-Pris:SCREEN-VALUE + ")" + " er større eller lik maksimal tillatt pris (" + STRING(lMaxPris) + ")." SKIP(1)
              VIEW-AS ALERT-BOX INFO TITLE "Priskontroll".
      RETURN NO-APPLY wStatus.
  END. 

  /* Lagrer valuta */
  DO TRANSACTION:
    FIND bArtBas EXCLUSIVE-LOCK WHERE RECID(bArtBas) = RECID(ArtBas) NO-ERROR.
    IF AVAILABLE bArtBas THEN
    DO:
        ASSIGN 
            bArtBas.Valkod = IF FI-ValKod:SCREEN-VALUE = ? THEN '' ELSE FI-ValKod:SCREEN-VALUE
            FI-ValKod      = IF FI-ValKod:SCREEN-VALUE = ? THEN '' ELSE FI-ValKod:SCREEN-VALUE
            .
        RELEASE bArtBas.
        FIND Valuta NO-LOCK WHERE
            Valuta.ValKod = FI-ValKod NO-ERROR.
    END.
  END.

  /* Pakker ned verdiene som ligger i skjermen. */
  ASSIGN
    wSkjerm = KalkStreng().

  /* Sjekker om det er gjort endringer */
  IF wOldSkjerm = wSkjerm THEN
    DO:
      MESSAGE "Kalkylen er ikke endret. Ingen oppdatering utføres." SKIP
              VIEW-AS ALERT-BOX TITLE "Kalkulasjon".
      RETURN NO-APPLY wStatus.
   END.
  
  /* Sjekker om ny post kan opprettes */
  RUN SjekkNyPrisKo IN h_PrisKo (ArtBas.ArtikkelNr,
                                 INPUT FI-ProfilNr,
                                 pdDato,
                                 piTid,
                                 pdDato2,
                                 piTid2,
                                 ?,
                                 (IF (CB-Tilbud = 2 OR CB-Tilbud = 5)
                                    THEN TRUE 
                                    ELSE FALSE),
                                 (IF CB-Tilbud = 2
                                   THEN 2
                                  ELSE IF CB-Tilbud = 4
                                   THEN 4
                                  ELSE IF CB-Tilbud = 5
                                   THEN 6
                                  ELSE 1),
                                 OUTPUT pcError).
  /* Feil som krever avbrudd for tilbud og normalpris. */
  IF CAN-DO("1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19," + 
            "50,51,52,53,54,55,56,57",ENTRY(1,pcError,CHR(1))) THEN
  DO:
    MESSAGE ENTRY(2,pcError,CHR(1))
        VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    RETURN NO-APPLY wStatus.
  END.

  /* Håndtering av feilmeldinger på normalpris. */
  IF (CB-Tilbud <> 2 AND CB-Tilbud <> 5) THEN
  /*IF T-Tilbud = FALSE THEN*/
  DO:
      /* Feil som krever bekreftelse */
      IF CAN-DO("20",ENTRY(1,pcError,CHR(1))) THEN
      DO:
          plSvar = FALSE.
          MESSAGE ENTRY(2,pcError,CHR(1))
              VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Bekreft"
              UPDATE plSvar.
          IF plSvar = FALSE THEN
              RETURN NO-APPLY wStatus.
      END.
  END.
  /* Håndtering av feilmeldinger på tilbud. */
  ELSE IF (CB-Tilbud = 2 OR CB-Tilbud <> 5) THEN
  /*ELSE IF T-Tilbud = TRUE THEN*/
  DO:
      /* Feil som krever bekreftelse */
      IF CAN-DO("50",ENTRY(1,pcError,CHR(1))) THEN
      DO:
          plSvar = FALSE.
          MESSAGE ENTRY(2,pcError,CHR(1))
              VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Bekreft"
              UPDATE plSvar.
          IF plSvar = FALSE THEN
              RETURN NO-APPLY wStatus.
      END.
  END.

  /* Lagrer mengderabatt hvis artpris finnes. */
  IF AVAILABLE ArtBas THEN
  DO FOR bArtPris TRANSACTION:
      FIND bArtPris EXCLUSIVE-LOCK WHERE 
          bArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
          bArtPris.ProfilNr = INT(FI-ProfilNr:SCREEN-VALUE) NO-ERROR.
      IF AVAILABLE bArtPris THEN
      DO:
          ASSIGN
              bArtPris.MengdeRabAnt  = INT(ArtPris.MengdeRabAnt:SCREEN-VALUE)
              bArtPris.MengdeRabPris = DEC(ArtPris.MengdeRabPris:SCREEN-VALUE)
              .
          RELEASE bArtPris.
      END.
  END. /* TRANSACTION */

  /* Lagrer prisendring i priskø. */
  RUN NyPrisKo IN h_PrisKo 
      (
       INPUT wArtBasRecid, 
       INPUT INPUT FI-ProfilNr,
       INPUT-OUTPUT wSkjerm,
       (IF (INPUT CB-Tilbud = 2 OR INPUT CB-Tilbud = 5) 
          THEN TRUE
          ELSE FALSE),
       (IF INPUT CB-Tilbud = 1
         THEN 1
        ELSE IF INPUT CB-Tilbud = 2 
         THEN 2 
        ELSE IF INPUT CB-Tilbud = 5 
         THEN 5 
        ELSE 4)
      ).

  /* Sjekker om det finnes noen artpris post på artikkelen. Gjør det ikke
     det, skal prispost opprettes direkte og initieres med aktuell kalkyle.
     Dette gjelder bare normalprisposter.
  */
  
  IF CB-Tilbud < 2 THEN
  /*IF T-Tilbud = FALSE THEN*/
  DIREKTE:
  DO:
    IF NOT CAN-FIND(FIRST ArtPris WHERE
                    ArtPris.ArtikkelNr = wArtikkelNr AND
                    ArtPris.ProfilNr   = INPUT FI-ProfilNr) THEN
    DO:
      rowPrisKO = ?.
      RUN GetLastPrisKo IN h_Prisko (OUTPUT rowPrisKo).

      FIND PrisKo NO-LOCK WHERE
          RECID(PrisKo) = rowPrisKo NO-ERROR.

      IF NOT AVAILABLE PrisKo THEN
          LEAVE DIREKTE.

      RUN LagreArtPris IN h_Prisko
          (INPUT RECID(ArtBas),
           INPUT INPUT FI-ProfilNr,
           INPUT-OUTPUT wSkjerm,
           INPUT FALSE,  /* wTilbud = false - Dvs ordinær kalkyle.          */
           INPUT TRUE,   /* Direkte oppdatering av prisene som er kalkulert */
           INPUT PrisKo.TYPE,
           INPUT ROWID(PrisKo)).
    END.
  END. /* DIREKTE */
  
  /* Oppdaterer anbefalt pris */
  IF CB-Tilbud = 1 AND ArtBas.AnbefaltPris = 0 THEN
  DO TRANSACTION:
    FIND bArtBas EXCLUSIVE-LOCK WHERE RECID(bArtBas) = RECID(ArtBas) NO-ERROR.
    IF AVAILABLE bArtBas THEN
    DO:
        ASSIGN 
            bArtBas.AnbefaltPris = INPUT FI-Pris
            bArtBas.Valkod       = IF FI-ValKod:SCREEN-VALUE = ? THEN '' ELSE FI-ValKod:SCREEN-VALUE
            .
        RELEASE bArtBas.
    END.
  END.

  END. /* FRAME-SCOOPE */
  
  IF NOT CAN-FIND(FIRST BestHode OF ArtBas) AND
     wOpprettBestilling = 1 AND
     ArtBas.Lager = TRUE THEN
  OPPRETTBESTILLING:
  DO:
      /* Bygger opp liste med tilgjengelige størrelser. */
      ASSIGN
          wStorlekar = "".
      FOR EACH StrTStr NO-LOCK WHERE
          StrTStr.STrTypeId = ArtBas.StrTypeId
          BREAK BY StrTStr.SeqNr:
        ASSIGN
          wStorlekar = wStorlekar + 
                       (IF wStorlekar = ""
                          THEN ""
                          ELSE " ") + 
                       trim(StrTStr.SoStorl).
      END.
      /* Opretter tom bestilling. */
      ASSIGN lNybestilling = TRUE.
/*       RUN opprettbestilling.p    */
/*           (RECID(ArtBas),        */
/*            ?,                    */
/*            ?,                    */
/*            wLapTop,              */
/*            wCL,                  */
/*            wStorlekar,           */
/*            wBrGrpNr,             */
/*            "",                   */
/*            OUTPUT wBestHodeRecid */
/*           ).                     */

  END. /* OPPRETTBESTILLING */

  ASSIGN 
    wStatus    = "OK"
    wOldSkjerm = wSkjerm.
  
  RETURN wStatus.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MoveToTopp C-Kalkyle 
PROCEDURE MoveToTopp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   IF FRAME FRAME-PrisInfo:MOVE-TO-TOP() THEN.
   APPLY "ENTRY":U TO FI-ValPris.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NyArtPrisProfilnr C-Kalkyle 
PROCEDURE NyArtPrisProfilnr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER pfArtikkelNr AS DEC NO-UNDO.
  DEF INPUT PARAMETER piProfilNr   AS INT NO-UNDO.
  DEF BUFFER profilCLbutiker FOR butiker.
  DEF BUFFER profilButiker     FOR butiker.
  RELEASE bufArtPris.
  FIND FIRST profilButiker WHERE profilButiker.profilnr = piProfilNr NO-LOCK NO-ERROR.
  IF AVAIL profilButiker THEN DO:
      FIND profilCLbutiker WHERE profilCLbutiker.butik = profilButiker.clButikkNr NO-LOCK NO-ERROR.
      IF AVAIL profilCLbutiker THEN
          FIND bufArtPris WHERE bufArtpris.Artikkelnr = pfArtikkelnr AND 
                                bufArtpris.profilnr = profilCLbutiker.profilnr NO-LOCK NO-ERROR.
  END.
  IF NOT AVAIL bufArtpris THEN 
      FIND bufArtPris WHERE bufArtpris.Artikkelnr = pfArtikkelnr AND 
                            bufArtpris.profilnr   = clButiker.profilnr NO-LOCK NO-ERROR.
  IF NOT AVAIL bufArtpris THEN 
      FIND FIRST bufArtpris WHERE bufArtpris.Artikkelnr = pfArtikkelnr NO-LOCK NO-ERROR.
  IF AVAIL bufArtpris THEN DO:
      CREATE artpris.
      BUFFER-COPY bufArtpris EXCEPT profilnr TO Artpris
          ASSIGN Artpris.profilnr = piProfilnr NO-ERROR.
      IF ERROR-STATUS:ERROR THEN
          DELETE Artpris.
      ELSE DO:
/*           CB-Tilbud:SCREEN-VALUE IN FRAME FRAME-PrisInfo = "1". */
          ASSIGN ArtPris.AktivFraDato    = TODAY
                 ArtPris.AktivFraTid     = TIME
                 ArtPris.DB%[2]          = 0
                 ArtPris.DBKr[2]         = 0
                 ArtPris.DivKost%[2]     = 0
                 ArtPris.DivKostKr[2]    = 0
                 ArtPris.EuroPris[2]     = 0
                 ArtPris.Frakt%[2]       = 0
                 ArtPris.Frakt[2]        = 0
                 ArtPris.InnkjopsPris[2] = 0
                 ArtPris.MomsKod[2]      = 0
                 ArtPris.Mva%[2]         = 0
                 ArtPris.MvaKr[2]        = 0
                 ArtPris.Pris[2]         = 0
                 ArtPris.Rab1%[2]        = 0
                 ArtPris.Rab1Kr[2]       = 0
                 ArtPris.Rab2%[2]        = 0
                 ArtPris.Rab2Kr[2]       = 0
                 ArtPris.Rab3%[2]        = 0
                 ArtPris.Rab3Kr[2]       = 0
                 ArtPris.TilBud          = FALSE
                 ArtPris.TilbudFraDato = ?
                 ArtPris.TilbudFraTid  = 0
                 ArtPris.TilbudTilDato = ?
                 ArtPris.TilbudTilTid  = 0
                 ArtPris.TilbudTimeStyrt = FALSE
                 ArtPris.ValPris[2] = 0
                 ArtPris.VareKost[2] = 0.
          RUN UpdatePPArtpris.
          FIND TT_Prisprofil WHERE TT_prisprofil.profilnr = artpris.profilnr.
          REPOSITION BROWSE-KalkProfil TO ROWID ROWID(tt_prisprofil).
          RETURN "OK".
      END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NyBestilling C-Kalkyle 
PROCEDURE NyBestilling :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR wBestHodeRecid AS RECID NO-UNDO.
    ASSIGN lNybestilling = FALSE.

      RUN opprettbestilling.p 
          (RECID(ArtBas),
           ?,
           ?,
           wLapTop,
           wCL,
           wStorlekar,
           wBrGrpNr,
           "",
           OUTPUT wBestHodeRecid
          ).
      RUN VisBestilling IN wParentHandle NO-ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpprettArtPris C-Kalkyle 
PROCEDURE OpprettArtPris :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF INPUT PARAMETER pfArtikkelNr AS DEC NO-UNDO.
  DEF INPUT PARAMETER piProfilNr   AS INT NO-UNDO.

  DO TRANSACTION:
      CREATE ArtPris.
      ASSIGN
          ArtPris.ArtikkelNr = pfArtikkelNr
          ArtPris.profilNr   = piProfilNr
          .
      FIND CURRENT ArtPris NO-LOCK.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RensKalkyle C-Kalkyle 
PROCEDURE RensKalkyle :
/*------------------------------------------------------------------------------
  Purpose:     Renser kalkylerammen ved NY artikkel.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /* Skal initieres ut fra første prisprofil. */
  FIND FIRST PrisProfil NO-LOCK NO-ERROR.
  IF NOT AVAILABLE PrisProfil THEN
    DO:
      MESSAGE "Det er ikke definert noen prisprofil!"
              VIEW-AS ALERT-BOX TITLE "Kalkulasjonsfeil".
      RETURN NO-APPLY.
    END.
  ELSE
    ASSIGN
      FI-ProfilNr    = PrisProfil.ProfilNr
      FI-Beskrivelse = PrisProfil.Beskrivelse.
      
  /* Legger prisprofil opp i skjermen. */
  DISPLAY 
    FI-ProfilNr
    FI-Beskrivelse
  WITH FRAME FRAME-PrisInfo.    
  
  /* Blanker kalkylen.                        */
  FRAME-SCOOPE:
  DO WITH FRAME FRAME-PrisInfo:
    ASSIGN
      RS-VisKalkyle = 1.
    DISPLAY RS-VisKalkyle WITH FRAME FRAME-PrisInfo.
    RUN VisInfoHeading.

    /* Legger nye verdier opp på skjermen igjen. */
    ASSIGN
      FI-ValPris:screen-value  = ""
      FI-InnPris:screen-value  = ""
      FI-Rab1:screen-value     = ""
      FI-Rab1%:screen-value    = ""
      FI-Rab2:screen-value     = ""
      FI-Rab2%:screen-value    = ""
      FI-Frakt:screen-value    = ""
      FI-Frakt%:screen-value   = ""
      FI-DivKost:screen-value  = ""
      FI-DivKost%:screen-value = ""
      FI-Rab3:screen-value     = ""
      FI-Rab3%:screen-value    = ""
      FI-VareKost:screen-value = ""
      FI-Mva:screen-value      = ""
      FI-Mva%:screen-value     = ""
      FI-DB:screen-value       = ""
      FI-DB%:screen-value      = ""
      FI-Pris:screen-value     = ""
      FI-EUPris:screen-value   = ""
      T-Manuel                 = FALSE
      FI-AktFra:screen-value   = STRING(TODAY,"99/99/9999")
      FI-AktTil:screen-value   = "".
          
    DISPLAY 
      T-Manuel 
    WITH FRAME FRAME-PrisInfo.

    /* Legger nye verdier opp på skjermen igjen. */
    ASSIGN
      FI-InnPris-2:screen-value  = ""
      FI-Rab1-2:screen-value     = ""
      FI-Rab1%-2:screen-value    = ""
      FI-Rab2-2:screen-value     = ""
      FI-Rab2%-2:screen-value    = ""
      FI-Frakt-2:screen-value    = ""
      FI-Frakt%-2:screen-value   = ""
      FI-DivKost-2:screen-value  = ""
      FI-DivKost%-2:screen-value = ""
      FI-Rab3-2:screen-value     = ""
      FI-Rab3%-2:screen-value    = ""
      FI-VareKost-2:screen-value = ""
      FI-Mva-2:screen-value      = ""
      FI-Mva%-2:screen-value     = ""
      FI-DB-2:screen-value       = ""
      FI-DB%-2:screen-value      = ""
      FI-Pris-2:screen-value     = ""
      FI-EUPris-2:screen-value   = "".
  END. /* FRAME-SCOOPE */ 
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetEntry C-Kalkyle 
PROCEDURE SetEntry :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  APPLY "ENTRY":U TO FI-ValPris IN FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE settDagensDato C-Kalkyle 
PROCEDURE settDagensDato :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DO WITH FRAME FRAME-PrisInfo:
    ASSIGN
        FI-NAktFra:SCREEN-VALUE = STRING(TODAY)
        FI-NTid:SCREEN-VALUE    = SUBSTRING(STRING(TIME,"HH:MM"),1,2) + "," + 
                                  SUBSTRING(STRING(TIME,"HH:MM"),4,2)
        .

END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapaTT_PP C-Kalkyle 
PROCEDURE SkapaTT_PP :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH bufPrisprofil NO-LOCK:
        IF NOT CAN-FIND(TT_Prisprofil WHERE 
                        TT_Prisprofil.Profilnr = bufPrisprofil.profilnr) THEN 
        DO:
            CREATE TT_Prisprofil.
            ASSIGN TT_Prisprofil.profilnr = bufPrisprofil.profilnr.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettArtPris C-Kalkyle 
PROCEDURE SlettArtPris :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR pbBekreft    AS LOG  NO-UNDO.
  DEF VAR pcOldModus   AS CHAR NO-UNDO.

  DO WITH FRAME FRAME-PrisInfo:

    IF INPUT FI-ProfilNr = clButiker.ProfilNr THEN
        RETURN.
    DO TRANSACTION:
      FIND ArtPris NO-LOCK WHERE
          ArtPris.ArtikkelNr = wArtikkelNr AND
          ArtPris.ProfilNr   = INPUT FI-ProfilNr NO-ERROR.
      IF AVAILABLE ArtPris THEN
      DO:
          ASSIGN
              pbBekreft  = FALSE
              .
          MESSAGE "Skal prisposten for profil " + string(INPUT FI-ProfilNr) + " tas bort?"
              VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Bekreft"
              UPDATE pbBekreft.
          IF pbBekreft THEN
          DO:
              FIND CURRENT ArtPris EXCLUSIVE-LOCK.
              DELETE ArtPris.
              FOR EACH prisko WHERE prisko.artikkelnr = wArtikkelnr AND 
                                    prisko.profilnr   = INPUT FI-Profilnr:
                  DELETE prisko.
              END.
/*               ASSIGN                */
/*                 pcOldModus = wModus */
/*                 wModus     = "NY"   */
/*                 .                   */
              FI-Profilnr:SCREEN-VALUE = STRING(clButiker.ProfilNr).
              RUN ByttObjekt (wArtikkelNr).
              ASSIGN
                wModus = pcOldModus
                .
              RETURN NO-APPLY.
          END.
      END.
    END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettFrame C-Kalkyle 
PROCEDURE SlettFrame :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAMETER wHandle AS HANDLE.

  IF VALID-HANDLE(wFrameHandle) THEN
    DO:
        wHandle = wFrameHandle.
    END.
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettPrisKo C-Kalkyle 
PROCEDURE SlettPrisKo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SokProfil C-Kalkyle 
PROCEDURE SokProfil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME FRAME-PrisInfo:
  DEF VAR cLookupValue AS CHAR NO-UNDO.

  cLookupValue = "ProfilNr;KortNavn;Beskrivelse".
  RUN JBoxDLookup.w ("Prisprofil;ProfilNr;KortNavn;Beskrivelse","where true",INPUT-OUTPUT cLookupValue).

  IF cLookupValue NE "" THEN 
    FIND Prisprofil NO-LOCK WHERE
      Prisprofil.ProfilNr = int(ENTRY(1,cLookupValue,"|")) NO-ERROR.
  
  IF AVAILABLE PrisProfil THEN
    DO:
      DISPLAY 
        PrisProfil.ProfilNr    @ FI-PRofilNr
        Prisprofil.Beskrivelse @ FI-Beskrivelse
      WITH FRAME FRAME-PrisInfo.

      FIND ArtPris NO-LOCK WHERE
        ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
        ArtPris.ProfilNr   = PrisProfil.ProfilNr NO-ERROR.
      RUN VisPrisStatus.
      RUN VisTilbudsFlagg (2).
      RUN InitKalkyle (FALSE,1).
    END.
  
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TaBortFeil C-Kalkyle 
PROCEDURE TaBortFeil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ON WRITE OF ArtPris OVERRIDE DO: END.
    FIND CURRENT ArtPris.
    ASSIGN ArtPris.DB%[1]          = IF ArtPris.DB%[1]          = ? THEN 0 ELSE ArtPris.DB%[1]         
           ArtPris.DBKr[1]         = IF ArtPris.DBKr[1]         = ? THEN 0 ELSE ArtPris.DBKr[1]        
           ArtPris.DivKost%[1]     = IF ArtPris.DivKost%[1]     = ? THEN 0 ELSE ArtPris.DivKost%[1]    
           ArtPris.DivKostKr[1]    = IF ArtPris.DivKostKr[1]    = ? THEN 0 ELSE ArtPris.DivKostKr[1]   
           ArtPris.EuroPris[1]     = IF ArtPris.EuroPris[1]     = ? THEN 0 ELSE ArtPris.EuroPris[1]    
           ArtPris.Frakt%[1]       = IF ArtPris.Frakt%[1]       = ? THEN 0 ELSE ArtPris.Frakt%[1]      
           ArtPris.Frakt[1]        = IF ArtPris.Frakt[1]        = ? THEN 0 ELSE ArtPris.Frakt[1]       
           ArtPris.InnkjopsPris[1] = IF ArtPris.InnkjopsPris[1] = ? THEN 0 ELSE ArtPris.InnkjopsPris[1]
           ArtPris.MomsKod[1]      = IF ArtPris.MomsKod[1]      = ? THEN 0 ELSE ArtPris.MomsKod[1]     
           ArtPris.Mva%[1]         = IF ArtPris.Mva%[1]         = ? THEN Moms.MomsProc ELSE ArtPris.Mva%[1]        
           ArtPris.MvaKr[1]        = IF ArtPris.MvaKr[1]        = ? THEN 0 ELSE ArtPris.MvaKr[1]       
           ArtPris.Pris[1]         = IF ArtPris.Pris[1]         = ? THEN 0 ELSE ArtPris.Pris[1]        
           ArtPris.Rab1%[1]        = IF ArtPris.Rab1%[1]        = ? THEN 0 ELSE ArtPris.Rab1%[1]       
           ArtPris.Rab1Kr[1]       = IF ArtPris.Rab1Kr[1]       = ? THEN 0 ELSE ArtPris.Rab1Kr[1]      
           ArtPris.Rab2%[1]        = IF ArtPris.Rab2%[1]        = ? THEN 0 ELSE ArtPris.Rab2%[1]       
           ArtPris.Rab2Kr[1]       = IF ArtPris.Rab2Kr[1]       = ? THEN 0 ELSE ArtPris.Rab2Kr[1]      
           ArtPris.Rab3%[1]        = IF ArtPris.Rab3%[1]        = ? THEN 0 ELSE ArtPris.Rab3%[1]       
           ArtPris.Rab3Kr[1]       = IF ArtPris.Rab3Kr[1]       = ? THEN 0 ELSE ArtPris.Rab3Kr[1]      
           ArtPris.ValPris[1]      = IF ArtPris.ValPris[1]      = ? THEN 0 ELSE ArtPris.ValPris[1]     
           ArtPris.VareKost[1]     = IF ArtPris.VareKost[1]     = ? THEN 0 ELSE ArtPris.VareKost[1]. 
    RUN ByttObjekt(ArtPris.Artikkelnr).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UpdatePPArtpris C-Kalkyle 
PROCEDURE UpdatePPArtpris :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR cKomisjonsLSt AS CHAR NO-UNDO.
    DEF VAR lKom% AS DEC NO-UNDO.

    DEF BUFFER lokArtBas FOR ArtBas.

    ASSIGN
        cKomisjonsLSt = '100'
        lKom%         = 45
        .
    
    cNyaProfiler = "".
    FOR EACH TT_Prisprofil:
        TT_Prisprofil.Kalkfinns = FALSE.
    END.

    FOR EACH bufArtPris WHERE 
        bufArtPris.artikkelnr = wArtikkelnr NO-LOCK:
        
        FIND TT_Prisprofil WHERE 
            TT_Prisprofil.profilnr = bufArtPris.profilnr NO-ERROR.
        IF NOT AVAIL TT_Prisprofil THEN DO:
          CREATE TT_PrisProfil.
          ASSIGN TT_PrisProfil.ProfilNr = bufArtPris.ProfilNr.
        END.
        ASSIGN TT_Prisprofil.Inpris = bufArtPris.InnkjopsPris[1]
               TT_Prisprofil.Pris   = bufArtPris.pris[1]
               TT_Prisprofil.tilbud = bufArtPris.tilbud
               TT_Prisprofil.KalkFinns = TRUE.
        IF CAN-DO(cKomisjonsLSt,STRING(TT_PrisProfil.ProfilNr)) THEN
        DO:
            FIND lokArtBas NO-LOCK WHERE 
                lokArtBas.ArtikkelNr = bufArtPris.ProfilNr NO-ERROR.
            TT_Prisprofil.Inpris = 0.
            IF AVAILABLE lokArtBas THEN
                TT_Prisprofil.Inpris = lokArtBas.KjedeInnkPris.
            IF TT_Prisprofil.Inpris = 0 THEN
                TT_Prisprofil.Inpris = ROUND((bufArtPris.InnkjopsPris[1] * 100) / 100,0).
        END.
    END.
    
    IF NOT CAN-FIND(FIRST artpris WHERE 
                    artpris.artikkelnr = wArtikkelnr) THEN 
      DO:
        FIND TT_Prisprofil WHERE tt_Prisprofil.profilnr = clButiker.profilnr.
            TT_Prisprofil.Kalkfinns = TRUE.
      END.
    
    FOR EACH TT_Prisprofil WHERE 
        TT_Prisprofil.KalkFinns = FALSE:
        cNyaProfiler = cNyaProfiler + 
                       (IF cNyaProfiler <> "" THEN "," ELSE "") + 
                       STRING(TT_Prisprofil.Profilnr).
    END.
    B-NyKalkyle:SENSITIVE IN FRAME FRAME-PrisInfo
             = CAN-FIND(FIRST TT_Prisprofil WHERE TT_Prisprofil.KalkFinns = FALSE).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisInfoHeading C-Kalkyle 
PROCEDURE VisInfoHeading :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME FRAME-PrisInfo:
  IF INPUT RS-VisKalkyle = 1 THEN
    ASSIGN
      FI-ToppTekst-2 = STRING(wCurrlng = "SE","  Sista ordinarie kalkyl/  Siste ordinær kalkyle")
      FI-Topptekst-2:BGCOLOR = 9.
  ELSE
    ASSIGN
      FI-ToppTekst-2 = STRING(wCurrlng = "SE","  Sista kampanjkalkyl/  Siste tilbuds kalkyle")
      FI-Topptekst-2:BGCOLOR = 12.  
  DISPLAY FI-ToppTekst-2.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisPrisStatus C-Kalkyle 
PROCEDURE VisPrisStatus :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME FRAME-PrisInfo:
  IF AVAILABLE ArtPris THEN
    DO:
      IF ArtPris.Tilbud = FALSE THEN
        ASSIGN
        FI-VisPrisStatus         = STRING(wCurrlng = "SE","  Ordinarie pris aktiverat /  Ordinær pris aktivert ") +
                                     (IF AVAILABLE ArtPris 
                                          THEN STRING(ArtPris.AktivFraDato)
                                          ELSE "") + " " +
                                     (IF AVAILABLE ArtPris 
                                          THEN STRING(ArtPris.AktivFraTid,"HH:MM")
                                          ELSE "")  
        FI-VisPrisStatus:BGColor = ?.
      ELSE
        ASSIGN
        FI-VisPrisStatus         = STRING(wCurrlng = "SE","  Aktiv på kampanj /  Aktiv på tilbud ") +
                                   (IF AVAILABLE ArtPris 
                                      THEN STRING(ArtPris.TilbudFraDato)
                                      ELSE "") + " " +
                                   (IF AVAILABLE ArtPris 
                                      THEN STRING(ArtPris.TilbudFraTid,"HH:MM")
                                      ELSE "") + 
                                   " - " +  
                                   (IF AVAILABLE ArtPris 
                                      THEN STRING(ArtPris.TilbudTilDato)
                                      ELSE "") + " " + 
                                   (IF AVAILABLE ArtPris 
                                      THEN STRING(ArtPris.TilbudTilTid,"HH:MM")
                                      ELSE "") 
        FI-VisPrisStatus:BGColor = 12.    
        
        
    END. 
  ELSE
    ASSIGN
      FI-VisPrisStatus         = ""
      FI-VisPrisStatus:BGColor = ?.
  
  DISPLAY
    FI-VisPrisStatus
  WITH FRAME FRAME-PrisInfo.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisTilbudsFlagg C-Kalkyle 
PROCEDURE VisTilbudsFlagg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER wInputFlagg AS INT NO-UNDO.

/* Skjermverdi skal benyttes. */
IF wInputFlagg = 1 THEN. /* Gjør ingenting. */

/* Oridnær kalkyle skal vises. */
ELSE IF wInputFlagg = 2 THEN
  ASSIGN
    /*T-Tilbud = false*/
    CB-Tilbud = 1
    .

/* Henter verdi på T-Tilbud fra ArtPris for første prisprofil. */
ELSE IF wInputFLagg = 3 THEN
  DO:
    IF AVAILABLE ArtBas THEN
      DO:
        FIND PrisProfil WHERE Prisprofil.profilnr = tt_prisprofil.profilnr NO-LOCK NO-ERROR.
        IF AVAILABLE PrisProfil THEN
          FIND ArtPris NO-LOCK WHERE
               ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND artpris.profilnr = Prisprofil.profilnr NO-ERROR.
        IF AVAILABLE ArtPris THEN
          CB-Tilbud = (IF ArtPris.Tilbud
                         THEN 2
                         ELSE 1).
        ELSE
          CB-Tilbud = 1.
      END.
    ELSE
      /*T-Tilbud = false.*/
      CB-Tilbud = 1.
  END.

DO WITH FRAME FRAME-PrisInfo:
  IF (CB-Tilbud <> 2 AND CB-Tilbud <> 5) THEN
  /*if T-Tilbud = false then*/
  DO:
    ASSIGN
      FI-ToppTekst           = STRING(wCurrlng = "SE","  Ordinarie kalkyl/  Ordinær kalkyle")
      FI-Topptekst:BGCOLOR   = 9

      FI-Txt2:HIDDEN         = FALSE
      FI-NAktFra:HIDDEN      = FALSE 
      FI-NTid:HIDDEN         = FALSE
      btnNAktFra:HIDDEN      = FALSE

      FI-Txt1:HIDDEN         = TRUE 
      FI-AktFra:HIDDEN       = TRUE
      FI-Tid1:HIDDEN         = TRUE
      FI-AktTil:HIDDEN       = TRUE
      FI-Tid2:HIDDEN         = TRUE
      btnAktFra:HIDDEN       = TRUE
      btnAktTil:HIDDEN       = TRUE
      FI-AktFra              = ?
      FI-AktTil              = ?
      FI-Tid1                = 0.0
      FI-Tid2                = 0.0
      .
    /* Blankt datofelt skal initieres. */
    IF FI-NAktFra = ? THEN
        ASSIGN
        FI-NAktFra             = TODAY
        FI-NTid                = dec(INT(ENTRY(1,STRING(TIME,"HH:MM"),":") + entry(2,STRING(TIME,"HH:MM"),":")) / 100)
        .
    DISPLAY 
        FI-Txt2
        FI-NAktFra 
        FI-NTid
    WITH FRAME FRAME-PrisInfo.
  END.
      
  ELSE
  DO:
    ASSIGN
        FI-ToppTekst           = STRING(wCurrlng = "SE","  Kampanjkalkyl/  Tilbudskalkyle")
        FI-Topptekst:BGCOLOR   = 12

        FI-Txt1:HIDDEN         = FALSE
        FI-AktFra:HIDDEN       = FALSE
        FI-Tid1:HIDDEN         = FALSE
        FI-AktTil:HIDDEN       = FALSE
        FI-Tid2:HIDDEN         = FALSE
        btnAktFra:HIDDEN       = FALSE
        btnAktTil:HIDDEN       = FALSE

        FI-NAktFra:HIDDEN      = TRUE  
        FI-NTid:HIDDEN         = TRUE
        FI-Txt2:HIDDEN         = TRUE 
        btnNAktFra:HIDDEN      = TRUE

        FI-NAktFra             = ?
        FI-NTid                = 0.0
        .
    DISPLAY 
        FI-Txt1 
        FI-AktFra
        FI-Tid1
        FI-AktTil
        FI-Tid2
    WITH FRAME FRAME-PrisInfo.
  END.
  DISPLAY 
    FI-ToppTekst 
    /*T-Tilbud*/
    CB-Tilbud
  WITH FRAME FRAME-PrisInfo.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisTilbudsFlaggOld C-Kalkyle 
PROCEDURE VisTilbudsFlaggOld :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER wInputFlagg AS INT NO-UNDO.

/* Skjermverdi skal benyttes. */
IF wInputFlagg = 1 THEN. /* Gjør ingenting. */

/* Oridnær kalkyle skal vises. */
ELSE IF wInputFlagg = 2 THEN
  ASSIGN
    /*T-Tilbud = false*/
    CB-Tilbud = 1
    .

/* Henter verdi på T-Tilbud fra ArtPris for første prisprofil. */
ELSE IF wInputFLagg = 3 THEN
  DO:
    IF AVAILABLE ArtBas THEN
      DO:
        FIND FIRST PrisProfil NO-LOCK NO-ERROR.
        IF AVAILABLE PrisProfil THEN
          FIND FIRST ArtPris NO-LOCK WHERE
               ArtPris.ArtikkelNr = ArtBas.ArtikkelNr NO-ERROR.
        /*
        if available ArtPris then
          T-Tilbud = ArtPris.Tilbud.
        else
          T-Tilbud = false.
        */
        IF AVAILABLE ArtPris THEN
          CB-Tilbud = (IF ArtPris.Tilbud
                         THEN 2
                         ELSE 1).
        ELSE
          CB-Tilbud = 1.
      END.
    ELSE
      /*T-Tilbud = false.*/
      CB-Tilbud = 1.
  END.

DO WITH FRAME FRAME-PrisInfo:
  IF (CB-Tilbud <> 2 AND CB-Tilbud <> 5) THEN
  /*if T-Tilbud = false then*/
  DO:
    ASSIGN
      FI-ToppTekst           = "  Ordinær kalkyle"
      FI-Topptekst:BGCOLOR   = 9

      FI-Txt2:HIDDEN         = FALSE
      FI-NAktFra:HIDDEN      = FALSE 
      FI-NTid:HIDDEN         = FALSE
      btnNAktFra:HIDDEN      = FALSE

      FI-Txt1:HIDDEN         = TRUE 
      FI-AktFra:HIDDEN       = TRUE
      FI-Tid1:HIDDEN         = TRUE
      FI-AktTil:HIDDEN       = TRUE
      FI-Tid2:HIDDEN         = TRUE
      btnAktFra:HIDDEN       = TRUE
      btnAktTil:HIDDEN       = TRUE
      FI-AktFra              = ?
      FI-AktTil              = ?
      FI-Tid1                = 0.0
      FI-Tid2                = 0.0
      .
    /* Blankt datofelt skal initieres. */
    IF FI-NAktFra = ? THEN
        ASSIGN
        FI-NAktFra             = TODAY
        FI-NTid                = dec(INT(ENTRY(1,STRING(TIME,"HH:MM"),":") + entry(2,STRING(TIME,"HH:MM"),":")) / 100)
        .
    DISPLAY 
        FI-Txt2
        FI-NAktFra 
        FI-NTid
    WITH FRAME FRAME-PrisInfo.
  END.
      
  ELSE
  DO:
    ASSIGN
        FI-ToppTekst           = "  Tilbudskalkyle"
        FI-Topptekst:BGCOLOR   = 12

        FI-Txt1:HIDDEN         = FALSE
        FI-AktFra:HIDDEN       = FALSE
        FI-Tid1:HIDDEN         = FALSE
        FI-AktTil:HIDDEN       = FALSE
        FI-Tid2:HIDDEN         = FALSE
        btnAktFra:HIDDEN       = FALSE
        btnAktTil:HIDDEN       = FALSE

        FI-NAktFra:HIDDEN      = TRUE  
        FI-NTid:HIDDEN         = TRUE
        FI-Txt2:HIDDEN         = TRUE 
        btnNAktFra:HIDDEN      = TRUE

        FI-NAktFra             = ?
        FI-NTid                = 0.0
        .
    DISPLAY 
        FI-Txt1 
        FI-AktFra
        FI-Tid1
        FI-AktTil
        FI-Tid2
    WITH FRAME FRAME-PrisInfo.
  END.
  DISPLAY 
    FI-ToppTekst 
    /*T-Tilbud*/
    CB-Tilbud
  WITH FRAME FRAME-PrisInfo.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION feilIkalkyle C-Kalkyle 
FUNCTION feilIkalkyle RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose: Om det smugit sig in fel i kalkylen, händer ibland från VPI 
    Notes: returnerar TRUE om det är fel. 
------------------------------------------------------------------------------*/
  IF SkoTex.ArtPris.DB%[1]          = ? OR
     SkoTex.ArtPris.DBKr[1]         = ? OR
     SkoTex.ArtPris.DivKost%[1]     = ? OR
     SkoTex.ArtPris.DivKostKr[1]    = ? OR
     SkoTex.ArtPris.EuroPris[1]     = ? OR
     SkoTex.ArtPris.Frakt%[1]       = ? OR
     SkoTex.ArtPris.Frakt[1]        = ? OR
     SkoTex.ArtPris.InnkjopsPris[1] = ? OR
     SkoTex.ArtPris.MomsKod[1]      = ? OR
     SkoTex.ArtPris.Mva%[1]         = ? OR
     SkoTex.ArtPris.MvaKr[1]        = ? OR
     SkoTex.ArtPris.Pris[1]         = ? OR
     SkoTex.ArtPris.Rab1%[1]        = ? OR
     SkoTex.ArtPris.Rab1Kr[1]       = ? OR
     SkoTex.ArtPris.Rab2%[1]        = ? OR
     SkoTex.ArtPris.Rab2Kr[1]       = ? OR
     SkoTex.ArtPris.Rab3%[1]        = ? OR
     SkoTex.ArtPris.Rab3Kr[1]       = ? OR
     SkoTex.ArtPris.ValPris[1]      = ? OR
     SkoTex.ArtPris.VareKost[1]     = ? THEN
      RETURN TRUE. 
  ELSE
      RETURN FALSE.   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION KalkStreng C-Kalkyle 
FUNCTION KalkStreng RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR wTekst   AS CHAR NO-UNDO.
  DEF VAR wAktDato AS DATE NO-UNDO.
  
  DO WITH FRAME FRAME-PrisInfo:
  ASSIGN
    /*T-Tilbud*/
    CB-Tilbud
    /*wAktDato =  if T-Tilbud then input FI-AktFra else input FI-NAktFra*/
    wAktDato =  IF (CB-Tilbud = 2 OR CB-Tilbud = 5) THEN INPUT FI-AktFra ELSE INPUT FI-NAktFra
    wTekst   =  STRING(INPUT FI-ValPris) + ";" +
                string(INPUT FI-InnPris) + ";" +
                string(INPUT FI-Rab1) + ";" +
                string(INPUT FI-Rab1%) + ";" +
                string(INPUT FI-Rab2) + ";" +
                string(INPUT FI-Rab2%) + ";" +
                string(INPUT FI-Frakt) + ";" +
                string(INPUT FI-Frakt%) + ";" +
                string(INPUT FI-DivKost) + ";" +
                string(INPUT FI-DivKost%) + ";" +
                string(INPUT FI-Rab3) + ";" +
                string(INPUT FI-Rab3%) + ";" +
                string(INPUT FI-VareKost) + ";" +
                string(INPUT FI-Mva) + ";" +
                string(INPUT FI-Mva%) + ";" +
                string(INPUT FI-DB) + ";" +
                string(INPUT FI-DB%) + ";" +
                string(INPUT FI-Pris) + ";" +
                string(INPUT FI-EUPris) + ";" +
                (IF INPUT T-Manuel = TRUE
                   THEN "yes"
                   ELSE "no") + ";".
  /* Normal aktiveringsdag/tid */                 
  IF (CB-Tilbud <> 2 AND CB-Tilbud <> 5) THEN
  /*if T-Tilbud = false then*/
    wTekst = wTekst +              
             (IF wAktDato <> ?
                THEN STRING(wAktDato)
                ELSE "") + ";" +
              string(
                    60 * 60 * int(SUBSTRING(STRING(INPUT FI-NTid,"99.99"),1,2)) +
                    60 * int(SUBSTRING(STRING(INPUT FI-NTid,"99.99"),4,2))
                    )
              + ";".
  ELSE 
    wTekst = wTekst + ";0;".
  /* Tilbudaktiveringsdag / tid. */  
  IF (Cb-Tilbud <> 2 AND CB-Tilbud <> 5) THEN
  /*if T-Tilbud = false then*/
    wTekst = wTekst + 
             ";0;;0;no".
  ELSE
    wTekst = wTekst + 
             (IF wAktDato <> ?
                THEN STRING(wAktDato)
                ELSE "") + ";" +
              string(
                    60 * 60 * int(SUBSTRING(STRING(INPUT FI-Tid1,"99.99"),1,2)) +
                    60 * int(SUBSTRING(STRING(INPUT FI-Tid1,"99.99"),4,2))
                    ) + ";" +
             (IF INPUT FI-AktTil <> ? 
               THEN STRING(INPUT FI-AktTil)
               ELSE "") + ";" +
              string(
                    60 * 60 * int(SUBSTRING(STRING(INPUT FI-Tid2,"99.99"),1,2)) +
                    60 * int(SUBSTRING(STRING(INPUT FI-Tid2,"99.99"),4,2))
                    ) + ";" +              
              "no".
  END.
  
  RETURN wTekst.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

