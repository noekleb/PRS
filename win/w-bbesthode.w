&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 
  STEP 1: Välj tabell i browsern
  STEP 2: Definiera de fælt du ønskar i din browser
          Kryssa av enabled på de fælt du ønskar sortering
  STEP 3: Gør de andringar som behøvs i alla scope.
          Sorttype skall ha lika många entries som antal enablade fält.
              - Tillåtna värden = "" (blank -> BY = default)
              - 

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
&scope br-tabell   BestHode
&scope Sorttype    BY DESCENDING,,,,USE-INDEX,,
&scope BrowseIdx   BestNr,BestillingsDato,OrdreNr,LevKod,LevDato,ArtikkelNr,EkstId
&scope Sokvillkor  >=,>=,>=,>=,>=,>=,>=
&scope InitIdx     BestNr
&scope ip-felt     BestNr
/* Om du önskar input parameter. Ger en startup-record */ 
/*
&scope ip-variabel w{&ip-felt}
*/

&scope assign-retur-verdi ASSIGN retur-verdi = if available {&br-tabell} ~
                                                then STRING({&br-tabell}.BestNr) ~
                                                else "".

&scope BrowseQ     FOR EACH {&br-tabell} NO-LOCK XSORTTYPE XSORT INDEXED-REPOSITION
&scope BrowseSQ    FOR EACH b{&br-tabell} NO-LOCK WHERE b{&br-tabell}.XFIELD XSOKV XFILL ~
                           USE-INDEX XIDX MAX-ROWS 1

/* Parameter Definisjoner ---                                           */
&IF LENGTH("{&ip-variabel}") > 0 &THEN

  &scope return-ip   ASSIGN {&ip-variabel} = {&br-tabell}.{&ip-felt}
  &scope init-phrase FIND b{&br-tabell} WHERE b{&br-tabell}.{&ip-felt} = ~
                        {&ip-variabel} USE-INDEX {&InitIdx} NO-LOCK NO-ERROR.

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    DEFINE VAR {&ip-variabel} LIKE {&br-tabell}.{&ip-felt} INIT 6 NO-UNDO.
  &ELSE
    DEFINE INPUT-OUTPUT PARAMETER {&ip-variabel} LIKE {&br-tabell}.{&ip-felt} NO-UNDO.
  &ENDIF

&ENDIF

/* Preprossessor direktiver ---                                         */

/* Buffer og Temp-Table Definisjoner ---                                */
DEFINE BUFFER b{&br-tabell} FOR {&br-tabell}.
DEFINE QUERY wSQ FOR b{&br-tabell} SCROLLING.
define temp-table tmpChild
  field wChild as handle.

/* Lokale variabler ---                                                 */

DEFINE VAR retur-verdi       AS CHAR INIT "AVBRYT" NO-UNDO.

DEFINE VAR wAktivCol         AS INT INIT 1  NO-UNDO.
DEFINE VAR wOrgBgCol         AS INT         NO-UNDO.
DEFINE VAR wSortBgCol        AS INT INIT 15 NO-UNDO.
DEFINE VAR wSearchCols       AS CHAR       NO-UNDO.
DEFINE VAR wSearchColsH      AS WIDGET EXTENT 10 NO-UNDO.
DEFINE VAR wQ                AS WIDGET      NO-UNDO.
DEFINE VAR wSortCol          AS WIDGET      NO-UNDO.
DEFINE VAR wAntSortCol       AS INTE        NO-UNDO.
DEFINE VAR wAktivFillIn      AS WIDGET      NO-UNDO.
DEFINE VAR wSorttype         AS CHAR   INIT "{&Sorttype}"   NO-UNDO.
DEFINE VAR wSokvillkor       AS CHAR   INIT "{&Sokvillkor}" NO-UNDO.
DEFINE VAR wBrowseIdx        AS CHAR   INIT "{&BrowseIdx}"  NO-UNDO.
define var wOk               as log         no-undo.
define var wAlle             as char        no-undo.
define var wAktivQString     as char        no-undo.
define var wBekreft          as log         no-undo.
DEFINE VAR wBlank            AS LOG         NO-UNDO.
define var wRetStatus        as log         no-undo.
define var wLevKod           like BestHode.LevKod no-undo.
DEFINE VAR wVgLopNr          AS CHAR        NO-UNDO.
DEFINE VAR wLevNavn          AS CHAR FORMAT "x(30)" NO-UNDO.
DEFINE VAR cHKinst           AS CHARACTER  NO-UNDO.
DEFINE VAR wArtBeskr         AS CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE VAR rArtikkelNr       AS DEC         NO-UNDO.
DEFINE VARIABLE cInnlevSamlet AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cBekreftet AS CHARACTER   NO-UNDO.
{runlib.i}

DEF BUFFER clButiker FOR Butiker.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BROWSE-BestHode

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES BestHode ArtBas

/* Definitions for BROWSE BROWSE-BestHode                               */
&Scoped-define FIELDS-IN-QUERY-BROWSE-BestHode BestHode.OrdreNr ~
BestHode.BestNr BestHode.BestStat getBekreftet() @ cBekreftet wVgLopNr ~
BestHode.LevKod wArtBeskr BestHode.LevFargKod BestHode.BestillingsDato ~
BestHode.LevDato wLevNavn BestHode.TotAntPar BestHode.TotInnLev ~
INT(BestHode.TotAntPar - ABS(BestHode.TotInnLev) - BestHode.TotMakulert + BestHode.TotOverLev) ~
BestHode.Beskrivelse BestHode.TotMakulert BestHode.TotOverLev ~
BestHode.TotInnkjVerdi BestHode.TotDbKr BestHode.TotSalgsVerdi ~
BestHode.ArtikkelNr BestHode.AnonseArtikkel BestHode.DirekteLev ~
BestHode.LapTop BestHode.CL BestHode.TeamNr BestHode.BestType ~
BestHode.StrTypeID BestHode.Merknad BestHode.EkstId BestHode.EDato ~
BestHode.BrukerID BestHode.RegistrertDato BestHode.RegistrertAv ~
BestHode.fraERP BestHode.VareBehNr 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-BestHode BestHode.OrdreNr ~
BestHode.BestNr BestHode.LevKod BestHode.BestillingsDato BestHode.LevDato ~
BestHode.ArtikkelNr BestHode.EkstId 
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-BestHode BestHode
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-BestHode BestHode
&Scoped-define QUERY-STRING-BROWSE-BestHode FOR EACH BestHode NO-LOCK, ~
      EACH ArtBas OF BestHode NO-LOCK ~
    BY BestHode.BestNr DESCENDING
&Scoped-define OPEN-QUERY-BROWSE-BestHode OPEN QUERY BROWSE-BestHode FOR EACH BestHode NO-LOCK, ~
      EACH ArtBas OF BestHode NO-LOCK ~
    BY BestHode.BestNr DESCENDING.
&Scoped-define TABLES-IN-QUERY-BROWSE-BestHode BestHode ArtBas
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-BestHode BestHode
&Scoped-define SECOND-TABLE-IN-QUERY-BROWSE-BestHode ArtBas


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FI-VareBehNr BUTTON-Sokart FI-Beskr ~
FI-LevFargKod FI-EkstId BUTTON-SokLev BUTTON-SokLev-2 CB-BestStat FI-LevNr ~
FI-LevKod FI-Sentrallager B-Print FI-OrdreNr B-Print-2 B-PrintInnlev ~
Btn_Help FI-Bestillingsnr1 FI-Bestillingsnr2 Btn_OK BUTTON-ArtKort ~
FI-LevPeriode1 BUTTON-EndreBest FI-LevPeriode2 BUTTON-Innleveranse ~
FI-BestDato1 BUTTON-Kalkyle FI-BestDato2 BUTTON-NyBest CB-BestType ~
BUTTON-SettLopNr T-DirLev BUTTON-SlettBest T-Annonsevarer T-LapTop ~
FILL-IN-SOK-DATE FILL-IN-SOK-CHAR FILL-IN-SOK-INTE FILL-IN-SOK-DECI ~
BROWSE-BestHode BUTTON-Sok B-Blank B-Blank2 B-Oppdater B-Blank-3 B-Blank-4 ~
B-Blank-5 B-BestDato B-BestNr btnSokKunde B-Blank-6 B-Blank-7 ~
BUTTON-Avskriv RECT-49 RECT-50 RECT-51 RECT-52 
&Scoped-Define DISPLAYED-OBJECTS FI-VareBehNr FI-Beskr FI-LevFargKod ~
FI-EkstId FI-clNavn CB-BestStat FI-LevNr FI-LevKod FI-Sentrallager ~
FI-OrdreNr FI-Bestillingsnr1 FI-Bestillingsnr2 FI-LevPeriode1 ~
FI-LevPeriode2 FI-BestDato1 FI-BestDato2 CB-BestType T-DirLev ~
T-Annonsevarer T-LapTop FILL-IN-SOK-DATE FILL-IN-SOK-CHAR FILL-IN-SOK-INTE ~
FILL-IN-SOK-DECI FI-LevNamn 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fLockvindu C-Win 
FUNCTION fLockvindu RETURNS CHARACTER
  ( INPUT lLock AS LOGICAL )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getBekreftet C-Win 
FUNCTION getBekreftet RETURNS CHARACTER
  (  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Prep-Private-Data C-Win 
FUNCTION Prep-Private-Data RETURNS CHARACTER
  ( INPUT wQueryCol AS WIDGET,INPUT wQueryCol# AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE Image-Sko AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chImage-Sko AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-BestDato 
     LABEL "Blank" 
     SIZE 7 BY 1.

DEFINE BUTTON B-BestNr 
     LABEL "Blank" 
     SIZE 7 BY 1.

DEFINE BUTTON B-Blank 
     LABEL "Blank" 
     SIZE 7 BY 1.

DEFINE BUTTON B-Blank-3 
     LABEL "Blank" 
     SIZE 7 BY 1.

DEFINE BUTTON B-Blank-4 
     LABEL "Blank" 
     SIZE 7 BY 1.

DEFINE BUTTON B-Blank-5 
     LABEL "Blank" 
     SIZE 7 BY 1.

DEFINE BUTTON B-Blank-6 
     LABEL "Blank" 
     SIZE 7 BY 1.

DEFINE BUTTON B-Blank-7 
     LABEL "Blank" 
     SIZE 7 BY 1.

DEFINE BUTTON B-Blank2 
     LABEL "Blank" 
     SIZE 7 BY 1.

DEFINE BUTTON B-ByttNr  NO-FOCUS
     LABEL "Bytt nr..." 
     SIZE 15 BY 1.1.

DEFINE BUTTON B-Oppdater 
     LABEL "&Oppdater browser" 
     SIZE 26.4 BY 1.14.

DEFINE BUTTON B-Print 
     IMAGE-UP FILE "icon/e-print":U NO-FOCUS
     LABEL "Button 1" 
     SIZE 4.6 BY 1.1 TOOLTIP "Utskrift av bestillingskort m/kalkyle.".

DEFINE BUTTON B-Print-2 
     IMAGE-UP FILE "icon/e-print":U NO-FOCUS
     LABEL "B print 2" 
     SIZE 4.6 BY 1.1 TOOLTIP "Utskrift av bestillingskort u/kalkyle.".

DEFINE BUTTON B-PrintInnlev 
     IMAGE-UP FILE "icon/e-print":U NO-FOCUS
     LABEL "B print 2" 
     SIZE 4.6 BY 1.1 TOOLTIP "Utskrift av innleveransrapport".

DEFINE BUTTON btnSokKunde  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON Btn_Help DEFAULT 
     IMAGE-UP FILE "icon/e-help":U NO-FOCUS
     LABEL "&Hjelp" 
     SIZE 4.6 BY 1.1 TOOLTIP "Hjelp"
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     IMAGE-UP FILE "icon/e-exit":U NO-FOCUS
     LABEL "OK" 
     SIZE 4.6 BY 1.1 TOOLTIP "Avslutt"
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-ArtKort  NO-FOCUS
     LABEL "Arti&kkelkort..." 
     SIZE 15.2 BY 1.1 TOOLTIP "Start artikkelkortet".

DEFINE BUTTON BUTTON-Avskriv  NO-FOCUS
     LABEL "Avskriv rest..." 
     SIZE 22 BY 1.14.

DEFINE BUTTON BUTTON-EndreBest 
     IMAGE-UP FILE "icon/e-detail":U NO-FOCUS
     LABEL "&Endre.." 
     SIZE 4.6 BY 1.1 TOOLTIP "Endre bestilling".

DEFINE BUTTON BUTTON-Innleveranse  NO-FOCUS
     LABEL "&Innleveranse..." 
     SIZE 17 BY 1.1 TOOLTIP "Innleveranse av bestilling".

DEFINE BUTTON BUTTON-Kalkyle  NO-FOCUS
     LABEL "&Kalkyle..." 
     SIZE 15 BY 1.1 TOOLTIP "Vise bestillingens kalkyle".

DEFINE BUTTON BUTTON-NyBest 
     IMAGE-UP FILE "icon/e-ny":U NO-FOCUS
     LABEL "&Ny.." 
     SIZE 4.6 BY 1.1 TOOLTIP "Opprette ny bestilling".

DEFINE BUTTON BUTTON-SettLopNr  NO-FOCUS
     LABEL "Sett løpenummer..." 
     SIZE 20.2 BY 1.1 TOOLTIP "Tildel artikklenen på bestillingen et løpenummer".

DEFINE BUTTON BUTTON-SlettBest 
     IMAGE-UP FILE "icon/e-del":U NO-FOCUS
     LABEL "&Slette" 
     SIZE 4.6 BY 1.1 TOOLTIP "Ta bort bestilling".

DEFINE BUTTON BUTTON-Sok 
     LABEL "Søk" 
     SIZE 10.2 BY 1 TOOLTIP "Søk i AKTIV kollonne".

DEFINE BUTTON BUTTON-Sokart 
     IMAGE-UP FILE "icon/e-search.bmp":U NO-FOCUS
     LABEL "&Søk" 
     SIZE 4.6 BY 1.1 TOOLTIP "Ta bort bestilling".

DEFINE BUTTON BUTTON-SokLev 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1 TOOLTIP "Søk i leverandørregister".

DEFINE BUTTON BUTTON-SokLev-2 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1 TOOLTIP "Søk i leverandørregister".

DEFINE VARIABLE CB-BestStat AS CHARACTER FORMAT "X(256)":U 
     LABEL "Bestillingsstatus" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEMS "[Alle]" 
     DROP-DOWN-LIST
     SIZE 49.2 BY 1 NO-UNDO.

DEFINE VARIABLE CB-BestType AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Bestillingstype" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "[Alle]",0,
                     "Grunnbestilling",1,
                     "Tilleggsbestilling",2
     DROP-DOWN-LIST
     SIZE 36.4 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Beskr AS CHARACTER FORMAT "X(256)":U 
     LABEL "Varetekst" 
     VIEW-AS FILL-IN 
     SIZE 42.2 BY 1 NO-UNDO.

DEFINE VARIABLE FI-BestDato1 AS DATE FORMAT "99/99/99":U 
     LABEL "Bestillingsdato" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-BestDato2 AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN NATIVE 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Bestillingsnr1 AS DECIMAL FORMAT ">>>>>>>9":U INITIAL 0 
     LABEL "Bestillingsnr" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Bestillingsnr2 AS DECIMAL FORMAT ">>>>>>>9":U INITIAL 0 
     VIEW-AS FILL-IN NATIVE 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-clNavn AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE FI-EkstId AS CHARACTER FORMAT "X(15)":U 
     LABEL "Ekstern ref." 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE FI-LevFargKod AS CHARACTER FORMAT "X(256)":U 
     LABEL "Lev.fargekode" 
     VIEW-AS FILL-IN 
     SIZE 42.2 BY 1 NO-UNDO.

DEFINE VARIABLE FI-LevKod AS CHARACTER FORMAT "X(256)":U 
     LABEL "Lev.art.nr" 
     VIEW-AS FILL-IN 
     SIZE 42.2 BY 1 NO-UNDO.

DEFINE VARIABLE FI-LevNamn AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE FI-LevNr AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Leverandør" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-LevPeriode1 AS DATE FORMAT "99/99/99":U 
     LABEL "Lev.dato" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-LevPeriode2 AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-OrdreNr AS INTEGER FORMAT ">>>>>>>9":U INITIAL 0 
     LABEL "Ordrenummer" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Sentrallager AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     LABEL "Sentrallager" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-VareBehNr AS DECIMAL FORMAT ">>>>>>>>>>>>9" INITIAL 0 
     LABEL "Vareh.bok" 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-SOK-CHAR AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-SOK-DATE AS DATE FORMAT "99-99-99":U 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-SOK-DECI AS DECIMAL FORMAT ">>>>>>>>>>>>>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-SOK-INTE AS INTEGER FORMAT ">>>>>>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-49
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 26.4 BY 4.76.

DEFINE RECTANGLE RECT-50
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 157 BY .1.

DEFINE RECTANGLE RECT-51
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 202 BY .19.

DEFINE RECTANGLE RECT-52
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 202 BY .19.

DEFINE VARIABLE T-Annonsevarer AS LOGICAL INITIAL no 
     LABEL "Annonsevarer" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY .81 NO-UNDO.

DEFINE VARIABLE T-DirLev AS LOGICAL INITIAL no 
     LABEL "Direkteleverte" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 NO-UNDO.

DEFINE VARIABLE T-LapTop AS LOGICAL INITIAL no 
     LABEL "LapTop" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY .81 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-BestHode FOR 
      BestHode, 
      ArtBas SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-BestHode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-BestHode C-Win _STRUCTURED
  QUERY BROWSE-BestHode NO-LOCK DISPLAY
      BestHode.OrdreNr FORMAT ">>>>>>>9":U
      BestHode.BestNr FORMAT ">>>>>>>9":U
      BestHode.BestStat COLUMN-LABEL "S" FORMAT ">9":U
      getBekreftet() @ cBekreftet COLUMN-LABEL "B" FORMAT "x(2)":U
      wVgLopNr COLUMN-LABEL "Vg/LøpeNr" WIDTH 10
      BestHode.LevKod FORMAT "x(20)":U
      wArtBeskr COLUMN-LABEL "Artikkelbeskrivelse" WIDTH 28
      BestHode.LevFargKod FORMAT "X(15)":U
      BestHode.BestillingsDato FORMAT "99/99/9999":U
      BestHode.LevDato COLUMN-LABEL "LevDato" FORMAT "99/99/99":U
            WIDTH 13.8
      wLevNavn COLUMN-LABEL "Leverandør" WIDTH 20.8
      BestHode.TotAntPar COLUMN-LABEL "Ant.Par" FORMAT "->>,>>9":U
            WIDTH 12
      BestHode.TotInnLev FORMAT "->>,>>9":U
      INT(BestHode.TotAntPar - ABS(BestHode.TotInnLev) - BestHode.TotMakulert + BestHode.TotOverLev) COLUMN-LABEL "Rest"
      BestHode.Beskrivelse FORMAT "x(25)":U
      BestHode.TotMakulert FORMAT "->>,>>9":U
      BestHode.TotOverLev FORMAT "->>,>>9":U
      BestHode.TotInnkjVerdi COLUMN-LABEL "Verdi" FORMAT "->,>>>,>>9.99":U
            WIDTH 14
      BestHode.TotDbKr COLUMN-LABEL "DbKr" FORMAT "->,>>>,>>9.99":U
      BestHode.TotSalgsVerdi COLUMN-LABEL "SalgsVerdi" FORMAT "->,>>>,>>9.99":U
      BestHode.ArtikkelNr COLUMN-LABEL "Artikkelnr" FORMAT "zzzzzzzzzzzz9":U
            WIDTH 15.2
      BestHode.AnonseArtikkel FORMAT "J/N":U
      BestHode.DirekteLev FORMAT "yes/no":U
      BestHode.LapTop COLUMN-LABEL "LT" FORMAT "*/":U
      BestHode.CL FORMAT ">>>>>9":U
      BestHode.TeamNr FORMAT "zzz9":U
      BestHode.BestType COLUMN-LABEL "BT" FORMAT "9":U
      BestHode.StrTypeID COLUMN-LABEL "StT" FORMAT ">>>>>9":U
      BestHode.Merknad FORMAT "x(30)":U
      BestHode.EkstId FORMAT "X(25)":U
      BestHode.EDato FORMAT "99/99/9999":U
      BestHode.BrukerID FORMAT "X(10)":U
      BestHode.RegistrertDato COLUMN-LABEL "Opprettet" FORMAT "99/99/9999":U
      BestHode.RegistrertAv COLUMN-LABEL "Oppr.Av" FORMAT "X(10)":U
      BestHode.fraERP FORMAT "yes/no":U
      BestHode.VareBehNr FORMAT ">>>>>>>>>>>>9":U
  ENABLE
      BestHode.OrdreNr
      BestHode.BestNr
      BestHode.LevKod
      BestHode.BestillingsDato
      BestHode.LevDato
      BestHode.ArtikkelNr
      BestHode.EkstId
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN NO-ROW-MARKERS SEPARATORS SIZE 200 BY 23.33 ROW-HEIGHT-CHARS .63.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     FI-VareBehNr AT ROW 7.67 COL 149 COLON-ALIGNED HELP
          "Varebhåndteringsnummer"
     BUTTON-Sokart AT ROW 1.48 COL 47.8
     FI-Beskr AT ROW 6.19 COL 16.8 COLON-ALIGNED
     FI-LevFargKod AT ROW 7.19 COL 16.8 COLON-ALIGNED
     FI-EkstId AT ROW 3.14 COL 149 COLON-ALIGNED
     BUTTON-SokLev AT ROW 4.14 COL 32.8
     BUTTON-SokLev-2 AT ROW 6.19 COL 100.2
     FI-clNavn AT ROW 6.19 COL 102.4 COLON-ALIGNED NO-LABEL
     CB-BestStat AT ROW 3.14 COL 16.6 COLON-ALIGNED
     FI-LevNr AT ROW 4.14 COL 16.8 COLON-ALIGNED
     FI-LevKod AT ROW 5.14 COL 16.8 COLON-ALIGNED
     B-ByttNr AT ROW 1.48 COL 105.2
     FI-Sentrallager AT ROW 6.19 COL 84.2 COLON-ALIGNED
     B-Print AT ROW 1.48 COL 57.8
     FI-OrdreNr AT ROW 7.19 COL 84.2 COLON-ALIGNED
     B-Print-2 AT ROW 1.48 COL 62.8
     B-PrintInnlev AT ROW 1.48 COL 67.8
     Btn_Help AT ROW 1.43 COL 194
     FI-Bestillingsnr1 AT ROW 8.19 COL 84.2 COLON-ALIGNED
     FI-Bestillingsnr2 AT ROW 8.19 COL 102.4 COLON-ALIGNED NO-LABEL
     Btn_OK AT ROW 1.43 COL 198
     BUTTON-ArtKort AT ROW 1.48 COL 120.6
     FI-LevPeriode1 AT ROW 3.14 COL 84.2 COLON-ALIGNED
     BUTTON-EndreBest AT ROW 1.48 COL 38
     FI-LevPeriode2 AT ROW 3.14 COL 99.2 COLON-ALIGNED NO-LABEL
     BUTTON-Innleveranse AT ROW 1.48 COL 87.8
     FI-BestDato1 AT ROW 4.14 COL 84.2 COLON-ALIGNED
     BUTTON-Kalkyle AT ROW 1.48 COL 72.8
     FI-BestDato2 AT ROW 4.14 COL 99.2 COLON-ALIGNED NO-LABEL
     BUTTON-NyBest AT ROW 1.48 COL 33
     CB-BestType AT ROW 5.14 COL 84.2 COLON-ALIGNED
     BUTTON-SettLopNr AT ROW 1.48 COL 135.8
     T-DirLev AT ROW 4.33 COL 151
     BUTTON-SlettBest AT ROW 1.48 COL 43
     T-Annonsevarer AT ROW 5.29 COL 151
     T-LapTop AT ROW 6.24 COL 151
     FILL-IN-SOK-DATE AT ROW 1.48 COL 2 NO-LABEL
     FILL-IN-SOK-CHAR AT ROW 1.48 COL 2 NO-LABEL
     FILL-IN-SOK-INTE AT ROW 1.48 COL 2 NO-LABEL
     FILL-IN-SOK-DECI AT ROW 1.48 COL 2 NO-LABEL
     BROWSE-BestHode AT ROW 9.33 COL 2
     BUTTON-Sok AT ROW 1.52 COL 21.4
     FI-LevNamn AT ROW 4.14 COL 35 COLON-ALIGNED NO-LABEL
     B-Blank AT ROW 3.14 COL 115.2
     B-Blank2 AT ROW 5.14 COL 61
     B-Oppdater AT ROW 8.05 COL 176
     B-Blank-3 AT ROW 6.19 COL 128.4
     B-Blank-4 AT ROW 4.14 COL 61
     B-Blank-5 AT ROW 7.19 COL 128.4
     B-BestDato AT ROW 4.14 COL 115.2
     B-BestNr AT ROW 8.19 COL 128.4
     btnSokKunde AT ROW 7.24 COL 100.2 NO-TAB-STOP 
     B-Blank-6 AT ROW 6.19 COL 61
     B-Blank-7 AT ROW 7.19 COL 61
     BUTTON-Avskriv AT ROW 1.48 COL 156.2 WIDGET-ID 2
     RECT-49 AT ROW 2.91 COL 176
     RECT-50 AT ROW 1 COL 1
     RECT-51 AT ROW 1.14 COL 1
     RECT-52 AT ROW 2.57 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 202.8 BY 31.81.


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
         TITLE              = "Søkeliste bestillinger"
         HEIGHT             = 31.81
         WIDTH              = 202.8
         MAX-HEIGHT         = 39.19
         MAX-WIDTH          = 230.4
         VIRTUAL-HEIGHT     = 39.19
         VIRTUAL-WIDTH      = 230.4
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win 
/* ************************* Included-Libraries *********************** */

{incl/DevMode.i}
{incl/CustDevMode.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   NOT-VISIBLE FRAME-NAME Custom                                        */
/* BROWSE-TAB BROWSE-BestHode FILL-IN-SOK-DECI DEFAULT-FRAME */
/* SETTINGS FOR BUTTON B-ByttNr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BROWSE-BestHode:NUM-LOCKED-COLUMNS IN FRAME DEFAULT-FRAME     = 7
       BROWSE-BestHode:MAX-DATA-GUESS IN FRAME DEFAULT-FRAME         = 481
       BROWSE-BestHode:COLUMN-RESIZABLE IN FRAME DEFAULT-FRAME       = TRUE.

/* SETTINGS FOR FILL-IN FI-clNavn IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-LevNamn IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-SOK-CHAR IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-SOK-DATE IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-SOK-DECI IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-SOK-INTE IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-BestHode
/* Query rebuild information for BROWSE BROWSE-BestHode
     _TblList          = "SkoTex.BestHode,SkoTex.ArtBas OF SkoTex.BestHode"
     _Options          = "NO-LOCK"
     _OrdList          = "SkoTex.BestHode.BestNr|no"
     _FldNameList[1]   > SkoTex.BestHode.OrdreNr
"BestHode.OrdreNr" ? ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > SkoTex.BestHode.BestNr
"BestHode.BestNr" ? ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > SkoTex.BestHode.BestStat
"BestHode.BestStat" "S" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > "_<CALC>"
"getBekreftet() @ cBekreftet" "B" "x(2)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > "_<CALC>"
"wVgLopNr" "Vg/LøpeNr" ? ? ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > SkoTex.BestHode.LevKod
"BestHode.LevKod" ? "x(20)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > "_<CALC>"
"wArtBeskr" "Artikkelbeskrivelse" ? ? ? ? ? ? ? ? no ? no no "28" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   = SkoTex.BestHode.LevFargKod
     _FldNameList[9]   > SkoTex.BestHode.BestillingsDato
"BestHode.BestillingsDato" ? ? "date" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > SkoTex.BestHode.LevDato
"BestHode.LevDato" "LevDato" "99/99/99" "date" ? ? ? ? ? ? yes ? no no "13.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > "_<CALC>"
"wLevNavn" "Leverandør" ? ? ? ? ? ? ? ? no ? no no "20.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > SkoTex.BestHode.TotAntPar
"BestHode.TotAntPar" "Ant.Par" ? "decimal" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   = SkoTex.BestHode.TotInnLev
     _FldNameList[14]   > "_<CALC>"
"INT(BestHode.TotAntPar - ABS(BestHode.TotInnLev) - BestHode.TotMakulert + BestHode.TotOverLev)" "Rest" ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > SkoTex.BestHode.Beskrivelse
"BestHode.Beskrivelse" ? "x(25)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   = SkoTex.BestHode.TotMakulert
     _FldNameList[17]   = SkoTex.BestHode.TotOverLev
     _FldNameList[18]   > SkoTex.BestHode.TotInnkjVerdi
"BestHode.TotInnkjVerdi" "Verdi" ? "decimal" ? ? ? ? ? ? no ? no no "14" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   > SkoTex.BestHode.TotDbKr
"BestHode.TotDbKr" "DbKr" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[20]   > SkoTex.BestHode.TotSalgsVerdi
"BestHode.TotSalgsVerdi" "SalgsVerdi" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[21]   > SkoTex.BestHode.ArtikkelNr
"BestHode.ArtikkelNr" "Artikkelnr" ? "decimal" ? ? ? ? ? ? yes ? no no "15.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[22]   = SkoTex.BestHode.AnonseArtikkel
     _FldNameList[23]   = SkoTex.BestHode.DirekteLev
     _FldNameList[24]   > SkoTex.BestHode.LapTop
"BestHode.LapTop" "LT" "*~~/" "logical" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[25]   = SkoTex.BestHode.CL
     _FldNameList[26]   = SkoTex.BestHode.TeamNr
     _FldNameList[27]   > SkoTex.BestHode.BestType
"BestHode.BestType" "BT" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[28]   > SkoTex.BestHode.StrTypeID
"BestHode.StrTypeID" "StT" ">>>>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[29]   > SkoTex.BestHode.Merknad
"BestHode.Merknad" ? "x(30)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[30]   > SkoTex.BestHode.EkstId
"BestHode.EkstId" ? "X(25)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[31]   = SkoTex.BestHode.EDato
     _FldNameList[32]   = SkoTex.BestHode.BrukerID
     _FldNameList[33]   > SkoTex.BestHode.RegistrertDato
"BestHode.RegistrertDato" "Opprettet" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[34]   > SkoTex.BestHode.RegistrertAv
"BestHode.RegistrertAv" "Oppr.Av" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[35]   = SkoTex.BestHode.fraERP
     _FldNameList[36]   = SkoTex.BestHode.VareBehNr
     _Query            is NOT OPENED
*/  /* BROWSE BROWSE-BestHode */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _Query            is NOT OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME Image-Sko ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 3.19
       COLUMN          = 177.6
       HEIGHT          = 4.19
       WIDTH           = 23.4
       HIDDEN          = no
       SENSITIVE       = yes.
/* Image-Sko OCXINFO:CREATE-CONTROL from: {9A93B740-C96B-11D0-8883-444553540000} type: Picbuf */
      Image-Sko:MOVE-AFTER(BROWSE-BestHode:HANDLE IN FRAME DEFAULT-FRAME).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Søkeliste bestillinger */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Søkeliste bestillinger */
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
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-BestDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-BestDato C-Win
ON CHOOSE OF B-BestDato IN FRAME DEFAULT-FRAME /* Blank */
DO:
  assign
    FI-BestDato1 = ?
    FI-BestDato2 = ?
    .
  display 
      FI-BestDato1 
      FI-BestDato2
      with frame DEFAULT-FRAME.

  RUN SD-CURSOR (" ").
  RETURN NO-APPLY.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-BestNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-BestNr C-Win
ON CHOOSE OF B-BestNr IN FRAME DEFAULT-FRAME /* Blank */
DO:
  assign
    FI-BestillingsNr1:SCREEN-VALUE = ""
    FI-BestillingsNr2:SCREEN-VALUE = ""
    .

  RUN SD-CURSOR (" ").
  RETURN NO-APPLY.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Blank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Blank C-Win
ON CHOOSE OF B-Blank IN FRAME DEFAULT-FRAME /* Blank */
DO:
  assign
    FI-LevPeriode1 = ?
    FI-LevPeriode2 = ?
    .
  display 
      FI-LevPeriode1 
      FI-LevPeriode2
      with frame DEFAULT-FRAME.

  RUN SD-CURSOR (" ").
  RETURN NO-APPLY.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Blank-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Blank-3 C-Win
ON CHOOSE OF B-Blank-3 IN FRAME DEFAULT-FRAME /* Blank */
DO:
  assign
    FI-Sentrallager:SCREEN-VALUE = ""
    FI-clNavn:SCREEN-VALUE       = ""
    .

  RUN SD-CURSOR (" ").
  RETURN NO-APPLY.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Blank-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Blank-4 C-Win
ON CHOOSE OF B-Blank-4 IN FRAME DEFAULT-FRAME /* Blank */
DO:
  assign
    FI-LevNr:SCREEN-VALUE = ""
    FI-LevNamn:SCREEN-VALUE = "".

  RUN SD-CURSOR (" ").
  RETURN NO-APPLY.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Blank-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Blank-5 C-Win
ON CHOOSE OF B-Blank-5 IN FRAME DEFAULT-FRAME /* Blank */
DO:
  assign
    FI-OrdreNr:SCREEN-VALUE = ""
    .

  RUN SD-CURSOR (" ").
  RETURN NO-APPLY.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Blank-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Blank-6 C-Win
ON CHOOSE OF B-Blank-6 IN FRAME DEFAULT-FRAME /* Blank */
DO:
  assign
    FI-Beskr = "".
  display FI-Beskr with frame DEFAULT-FRAME.

  RUN SD-CURSOR (" ").
  RETURN NO-APPLY.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Blank-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Blank-7 C-Win
ON CHOOSE OF B-Blank-7 IN FRAME DEFAULT-FRAME /* Blank */
DO:
  assign
    FI-LevFargKod = "".
  display FI-LevFargKod with frame DEFAULT-FRAME.

  RUN SD-CURSOR (" ").
  RETURN NO-APPLY.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Blank2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Blank2 C-Win
ON CHOOSE OF B-Blank2 IN FRAME DEFAULT-FRAME /* Blank */
DO:
  assign
    FI-LevKod = "".
  display FI-LevKod with frame DEFAULT-FRAME.

  RUN SD-CURSOR (" ").
  RETURN NO-APPLY.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-ByttNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-ByttNr C-Win
ON CHOOSE OF B-ByttNr IN FRAME DEFAULT-FRAME /* Bytt nr... */
DO:
  IF NOT AVAILABLE BestHOde THEN
      RETURN NO-APPLY.
  RUN endrebestnr.w (BestHode.BestNr).
  IF BROWSE-BestHode:REFRESH() THEN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Oppdater
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Oppdater C-Win
ON CHOOSE OF B-Oppdater IN FRAME DEFAULT-FRAME /* Oppdater browser */
DO:
  RUN SD-CURSOR (" ").
  /* open-query.... tar inte hänsyn till filter */
/*   {&OPEN-QUERY-{&BROWSE-NAME}} */
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


&Scoped-define SELF-NAME B-Print-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Print-2 C-Win
ON CHOOSE OF B-Print-2 IN FRAME DEFAULT-FRAME /* B print 2 */
DO:
  if not available BestHode then
    return.
  IF cInnlevSamlet = "1" THEN
      run bestillingskort.p (recid(BestHode),108,4,no).
  ELSE
      run bestillingskortX.p (recid(BestHode),108,4,no).
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
  message "Skal det skrives ut vedlegg pr. butikk?"
    view-as alert-box question buttons yes-no-cancel title "Innleveranserapport"
    update wBekreft.
  if wBekreft = ? then
    return no-apply.
  else DO:
      IF cInnlevSamlet = "1" THEN
          run bestillingskort.p (recid(BestHode),109,4,wBekreft).
      ELSE
          run bestillingskortX.p (recid(BestHode),109,4,wBekreft).
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-BestHode
&Scoped-define SELF-NAME BROWSE-BestHode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-BestHode C-Win
ON ANY-PRINTABLE OF BROWSE-BestHode IN FRAME DEFAULT-FRAME
DO:
  RUN SD-ANY-PRINTABLE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-BestHode C-Win
ON CURSOR-LEFT OF BROWSE-BestHode IN FRAME DEFAULT-FRAME
DO:
  IF wAntSortCol < 2 THEN
    RETURN NO-APPLY.
  RUN SD-CURSOR ("LEFT").
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-BestHode C-Win
ON CURSOR-RIGHT OF BROWSE-BestHode IN FRAME DEFAULT-FRAME
DO:
  IF wAntSortCol < 2 THEN
    RETURN NO-APPLY.
  RUN SD-CURSOR ("RIGHT").
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-BestHode C-Win
ON DEFAULT-ACTION OF BROWSE-BestHode IN FRAME DEFAULT-FRAME
DO:
  APPLY "CHOOSE" TO BUTTON-EndreBest.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-BestHode C-Win
ON END-MOVE OF BROWSE-BestHode IN FRAME DEFAULT-FRAME
DO:
  message "gurre var her " view-as alert-box.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-BestHode C-Win
ON HOME OF BROWSE-BestHode IN FRAME DEFAULT-FRAME
DO:
  APPLY "ENTRY" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-BestHode C-Win
ON ROW-DISPLAY OF BROWSE-BestHode IN FRAME DEFAULT-FRAME
DO:
  IF AVAILABLE BestHode THEN
  DO:
      FIND ArtBas NO-LOCK WHERE
          ArtBAs.ArtikkelNr = BestHode.ArtikkelNr NO-ERROR.
      IF AVAILABLE ArtBas THEN
          ASSIGN
            wVgLopNr = STRING(ArtBas.Vg) + "/" + 
                       (IF (ArtBas.LopNr <> ? AND ArtBas.LopNr <> 0)
                          THEN STRING(ArtBAs.LopNr)
                          ELSE "?")
            wArtBeskr = ArtBas.Beskr
            .
      ELSE
          ASSIGN wVgLopNr  = ""
                 wArtBeskr = "".
      FIND LevBas NO-LOCK WHERE
          LevBas.LevNr = BestHode.LevNr NO-ERROR.
      IF AVAILABLE LevBas THEN
          wLevNavn = LevBas.LevNamn.
      ELSE
          wLevNavn = "".
  END.
  ELSE
      wVgLopNr = ""
      .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-BestHode C-Win
ON START-SEARCH OF BROWSE-BestHode IN FRAME DEFAULT-FRAME
DO:
  RUN lockwindowupdate(FRAME {&FRAME-NAME}:HWND).
  DEF VAR wSearchCol  AS WIDGET NO-UNDO.
  DEF VAR wQString    AS CHAR   NO-UNDO.
  DEF VAR wSortColIdx AS INTE   NO-UNDO.
  ASSIGN wSearchCol = SELF:CURRENT-COLUMN.
  IF wSortCol <> SELF:CURRENT-COLUMN AND
                   LOOKUP(wSearchCol:NAME,wSearchCols) > 0 THEN DO:
      ASSIGN wSortCol = SELF:CURRENT-COLUMN.
      RUN SortNyCol.
  END.
  ELSE IF LOOKUP(wSearchCol:NAME,wSearchCols) > 0 AND
          LOOKUP("USE-INDEX",wQ:PREPARE-STRING," ") = 0 THEN DO:
      ASSIGN wQString = wQ:PREPARE-STRING
             wSortColIdx = LOOKUP("{&br-tabell}." + wSearchCol:NAME,wQString," ")
             wQString = IF ENTRY(wSortColIdx + 1,wQString," ") = "DESCENDING" THEN
                 REPLACE(wQString,"{&br-tabell}." + wSearchCol:NAME + " DESCENDING","{&br-tabell}." + wSearchCol:NAME)
                        ELSE
                 REPLACE(wQString,"{&br-tabell}." + wSearchCol:NAME, 
                           "{&br-tabell}." + wSearchCol:NAME + " DESCENDING")
             wSearchCol:PRIVATE-DATA = wQString.
      wQ:QUERY-PREPARE(wQString).
      FIND b{&br-tabell} WHERE ROWID(b{&br-tabell}) = ROWID({&br-tabell}) NO-LOCK.

      wQ:QUERY-OPEN().
      RUN SD-Reposition.
  END.
  APPLY "LEAVE" TO SELF. /* annars fungerar inte "ENTRY" ?? */
  APPLY "ENTRY" TO SELF.
  APPLY "END-SEARCH" TO SELF.
  RUN lockwindowupdate(0).
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-BestHode C-Win
ON VALUE-CHANGED OF BROWSE-BestHode IN FRAME DEFAULT-FRAME
DO:
  if available BestHode then
    find ArtBas of BestHode no-lock no-error.
  if available ArtBas then
    do:
      run VisBilde (1).  
      {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}  
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSokKunde
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSokKunde C-Win
ON CHOOSE OF btnSokKunde IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF FI-OrdreNr
DO:
  DEF VAR cLookupValue AS CHAR NO-UNDO.

  /* Syntaks: Param1: <Tabell>;<Felt>;<Felt>...,<Tabell>;<Felt>;<Felt>...              */
  /*          Param2: <Where sats> m/Join                                              */
  /*          Param3: <Returfelt1>[;<Returfelt2>;......],<Filterfelt1>[;<Filterfelt2>] (Settes i cLookupValue) */
  /* Kalkulerte felt kan også benyttes, label, format o.l..       */
  cLookupValue = "OrdreNr".
  RUN JBoxDLookup.w ("Ordre;OrdreNr;Merknad;SendtDato;OrdreStatus","where true",INPUT-OUTPUT cLookupValue).

  IF cLookupValue NE "" THEN 
  DO:
    FI-OrdreNr:SCREEN-VALUE = cLookupValue.
    APPLY "TAB" TO FI-OrdreNr.
    RETURN NO-APPLY.
  END.
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


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK C-Win
ON CHOOSE OF Btn_OK IN FRAME DEFAULT-FRAME /* OK */
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
                     
  {&return-ip}
  .
  &IF DEFINED(assign-retur-verdi) &THEN
      {&assign-retur-verdi}
      .
  &ELSE
      ASSIGN retur-verdi = "OK".
  &ENDIF
  apply "close":U to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-ArtKort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-ArtKort C-Win
ON CHOOSE OF BUTTON-ArtKort IN FRAME DEFAULT-FRAME /* Artikkelkort... */
DO:
    RUN Artikkelkort.
    RETURN NO-APPLY.
/*   if not available BestHode then                                                   */
/*     return no-apply.                                                               */
/*   find ArtBas of BestHode no-lock no-error.                                        */
/*                                                                                    */
/*   create tmpChild.                                                                 */
/*   if available ArtBas then                                                         */
/*     run w-vartkor  persistent set tmpChild.wChild  (input recid(ArtBas), "ENDRE"). */
/*   else                                                                             */
/*     run w-vartkor  persistent set tmpChild.wChild  (input ?, "ENDRE").             */
/*   if not valid-handle(tmpChild.wChild) then delete tmpChild.                       */
/*                                                                                    */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Avskriv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Avskriv C-Win
ON CHOOSE OF BUTTON-Avskriv IN FRAME DEFAULT-FRAME /* Avskriv rest... */
DO:
  DEF VAR bOk AS LOG NO-UNDO.

  IF NOT AVAILABLE BestHode THEN 
      RETURN.
  IF BestHode.BestStat < 3 OR 
     BestHode.BestStat > 5  THEN 
  DO:
      MESSAGE "Bestilling med denne status kan ikke avskrives!"
        VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
  END.  
  MESSAGE "Skal bestillingen avskrives?"
    VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE bOk.
  IF NOT bOk THEN
      RETURN NO-APPLY.

  RUN besthode_avskriv_rest.p (BestHode.BestNr).
  fLockvindu(FALSE).
  FIND CURRENT BestHode NO-LOCK.
  BROWSE {&BROWSE-NAME}:REFRESH().

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-EndreBest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-EndreBest C-Win
ON CHOOSE OF BUTTON-EndreBest IN FRAME DEFAULT-FRAME /* Endre.. */
DO:
  def var wBestHodeRecid as recid no-undo.

  if not available BestHode then 
    return no-apply.
  find ArtBas of BestHode no-lock no-error.
  
  if not available ArtBas then
    return no-apply.
  
  if not available BestHode then
    return no-apply.
  create tmpChild.
  assign
    wBestHodeRecid = recid(BestHode).
  run w-gridord.w  persistent set tmpChild.wChild (input recid(ArtBas), input-output wBestHodeRecid, "ENDRE").
  if not valid-handle(tmpChild.wChild) then delete tmpChild.
  /*RUN SD-CURSOR (" ").*/
  RETURN NO-APPLY.  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Innleveranse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Innleveranse C-Win
ON CHOOSE OF BUTTON-Innleveranse IN FRAME DEFAULT-FRAME /* Innleveranse... */
DO:
    RUN Innleveranse.
  RETURN NO-APPLY.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Kalkyle
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Kalkyle C-Win
ON CHOOSE OF BUTTON-Kalkyle IN FRAME DEFAULT-FRAME /* Kalkyle... */
DO:
  def var wBestHodeRecid as recid no-undo.

  if not available BestHode then 
    return no-apply.
  find ArtBas of BestHode no-lock no-error.
  
  if not available ArtBas then
    return no-apply.
  if not available BestHode then
    return no-apply.
    
  assign
    wBestHodeRecid = recid(BestHode).
  run d-vbestkalkyle.w (input recid(ArtBas), input wBestHodeRecid).
  APPLY "ENTRY" TO BROWSE {&BROWSE-NAME}.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-NyBest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-NyBest C-Win
ON CHOOSE OF BUTTON-NyBest IN FRAME DEFAULT-FRAME /* Ny.. */
DO:
  def var wBestHodeRecid as recid no-undo.
  
  if not available BestHode then 
    return no-apply.
  find ArtBas of BestHode no-lock no-error.
  
  if not available ArtBas then
    return no-apply.
  if ArtBas.Utgatt = true then   
    do:
      message "Artikkelen er utgått. Bestilling kan ikke registreres på utgåtte artikler."
        view-as alert-box message title "Melding".
      return no-apply.
    end.
  if ArtBas.Lager = false then   
    do:
      message "Artikkelen har ikke lagerstyring. Bestilling kan ikke registreres."
        view-as alert-box message title "Melding".
      return no-apply.
    end.

  create tmpChild.
  assign
    wBestHodeRecid = ?.
  run w-gridord.w persistent set tmpChild.wChild (input recid(ArtBas), input-output wBestHodeRecid, "NY").  
  if not valid-handle(tmpChild.wChild) then delete tmpChild.
  if wBestHodeRecid <> ? then
    do:
      /*RUN SD-CURSOR (" ").*/
      RETURN NO-APPLY.  
    end.
  return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SettLopNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SettLopNr C-Win
ON CHOOSE OF BUTTON-SettLopNr IN FRAME DEFAULT-FRAME /* Sett løpenummer... */
DO:

  if not available BestHode then 
    return no-apply.
  find ArtBas of BestHode no-lock no-error.

  RUN SetLopenummer.
  FIND CURRENT ArtBas NO-LOCK NO-ERROR.
  BROWSE {&BROWSE-NAME}:REFRESH().
/*   RUN SD-CURSOR (" "). */
  RETURN NO-APPLY.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SlettBest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SlettBest C-Win
ON CHOOSE OF BUTTON-SlettBest IN FRAME DEFAULT-FRAME /* Slette */
DO:

  run SlettBestilling.
  return no-apply.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Sok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Sok C-Win
ON CHOOSE OF BUTTON-Sok IN FRAME DEFAULT-FRAME /* Søk */
DO:
   /* DEFINE VAR wBlank AS LOG  NO-UNDO.*/
   DEFINE VAR wSQStr AS CHAR NO-UNDO. 
   DEFINE VAR wChar AS  CHAR NO-UNDO.
   DEFINE VAR wXSokv AS CHAR NO-UNDO.
   
   wBlank = false.
   
   IF wAktivFillIn:SCREEN-VALUE = "" THEN
       wBlank = TRUE.
   ELSE DO:
       ASSIGN wChar  = IF wAktivFillIn:DATA-TYPE BEGINS "CHAR" THEN '"' ELSE ''
              wSQStr = REPLACE("{&BrowseSQ}","XFIELD",ENTRY(wAktivCol,wSearchCols))
              wSQStr = REPLACE(wSQStr,"XSOKV",ENTRY(wAktivCol,wSokvillkor))
              wSQStr = IF ENTRY(wAktivCol,wBrowseIdx) <> "" THEN
                         REPLACE(wSQStr,"XIDX",ENTRY(wAktivCol,wBrowseIdx)) 
                       ELSE 
                           REPLACE(wSQStr,"USE-INDEX XIDX ","") 
              wSQStr = REPLACE(wSQStr,"XFILL",wChar + wAktivFillIn:SCREEN-VALUE + wChar).
      
       QUERY wSQ:QUERY-PREPARE(wSQStr).
       QUERY wSQ:QUERY-OPEN().
       GET FIRST wSQ.
       IF NOT AVAIL b{&br-tabell} THEN DO:
           APPLY "ENTRY" TO wAktivFillIn.
           RETURN NO-APPLY.         
       END.
   END.

   IF wBlank THEN
       APPLY "ENTRY" TO BROWSE {&BROWSE-NAME}.
   ELSE do:
     wBlank = ?.
     RUN SD-Reposition.
     wBlank = false.
   end.
   if available BestHode then
    find ArtBas of BestHode no-lock no-error.
   if available ArtBas THEN
     run VisBilde (1).  
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Sokart
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Sokart C-Win
ON CHOOSE OF BUTTON-Sokart IN FRAME DEFAULT-FRAME /* Søk */
DO:

  run d-hsok.w (output rArtikkelNr,"NEI" + CHR(1) + "0"). /* JA = sök mot vpistrekkode */
  if rArtikkelNr = ? then
      RETURN NO-APPLY.

  ELSE DO:
     /* FY FY */
     FIND ArtBas NO-LOCK WHERE
         ArtBas.ArtikkelNr = rArtikkelNr NO-ERROR.

     IF AVAILABLE ArtBas THEN
     DO:
         ASSIGN
             FI-LevNr:SCREEN-VALUE = string(ArtBas.LevNr)
             FI-LevKod:SCREEN-VALUE = ArtBas.LevKod
             .
         APPLY "TAB" TO FI-LevNr.

     END.
  END.
  return no-apply.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokLev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokLev C-Win
ON CHOOSE OF BUTTON-SokLev IN FRAME DEFAULT-FRAME /* ... */
DO:
do WITH FRAME DEFAULT-FRAME:
  {soek.i
    &Felt        = FI-LevNr
    &Program     = d-blevbas.w
    &Frame       = DEFAULT-FRAME
    &PostRun     = "find LevBas no-lock where
                      RECID(LevBas) = INT(RETURN-VALUE) NO-ERROR."
    &OptDisp     = "'' when not available LevBas @ FI-LevNr
                    LevBas.LevNamn when available LevBas @ FI-LevNamn"
  }   
  RUN SD-CURSOR (" ").

  RETURN NO-APPLY.  
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokLev-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokLev-2 C-Win
ON CHOOSE OF BUTTON-SokLev-2 IN FRAME DEFAULT-FRAME /* ... */
DO:
do WITH FRAME DEFAULT-FRAME:
  {soek.i
    &Felt        = FI-Sentrallager
    &Program     = d-bcl.w
    &Frame       = DEFAULT-FRAME
    &PostRun     = "find clButiker no-lock where
                      RECID(clButiker) = INT(RETURN-VALUE) NO-ERROR."
    &OptDisp     = "'' when not available clButiker @ FI-Sentrallager
                    clButiker.ButNamn when available clButiker @ FI-clNavn"
  }   
  RUN SD-CURSOR (" ").

  RETURN NO-APPLY.  
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-BestStat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-BestStat C-Win
ON VALUE-CHANGED OF CB-BestStat IN FRAME DEFAULT-FRAME /* Bestillingsstatus */
DO:
  RUN SD-CURSOR (" ").
  RETURN NO-APPLY.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-BestType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-BestType C-Win
ON VALUE-CHANGED OF CB-BestType IN FRAME DEFAULT-FRAME /* Bestillingstype */
DO:
  ASSIGN
      CB-BestType
      .
  RUN SD-CURSOR (" ").
  RETURN NO-APPLY.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Beskr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Beskr C-Win
ON TAB OF FI-Beskr IN FRAME DEFAULT-FRAME /* Varetekst */
or "RETURN" of FI-Beskr 
DO:
  RUN SD-CURSOR (" ").
  RETURN NO-APPLY.  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-BestDato2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-BestDato2 C-Win
ON TAB OF FI-BestDato2 IN FRAME DEFAULT-FRAME
OR "RETURN" OF FI-LevPeriode2
DO:
  ASSIGN
      FI-BestDato1
      FI-BestDato2.
  IF FI-BestDato2 < FI-BestDAto1 THEN
  DO:
      MESSAGE "Til dato er mindre enn fra dato!"
          VIEW-AS ALERT-BOX ERROR TITLE "Feil".
      RETURN NO-APPLY.
  END.
  RUN SD-CURSOR (" ").
  RETURN NO-APPLY.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Bestillingsnr2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Bestillingsnr2 C-Win
ON TAB OF FI-Bestillingsnr2 IN FRAME DEFAULT-FRAME
OR "RETURN" OF FI-BestillingsNr2
DO:
  ASSIGN
      FI-BestillingsNr2
      .
  RUN SD-CURSOR (" ").
  RETURN NO-APPLY.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-EkstId
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-EkstId C-Win
ON TAB OF FI-EkstId IN FRAME DEFAULT-FRAME /* Ekstern ref. */
or "RETURN" of FI-EkstId
DO:
  RUN SD-CURSOR (" ").
  RETURN NO-APPLY.  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-LevFargKod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-LevFargKod C-Win
ON TAB OF FI-LevFargKod IN FRAME DEFAULT-FRAME /* Lev.fargekode */
or "RETURN" of FI-LevFargKod
DO:
  RUN SD-CURSOR (" ").
  RETURN NO-APPLY.  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-LevKod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-LevKod C-Win
ON TAB OF FI-LevKod IN FRAME DEFAULT-FRAME /* Lev.art.nr */
or "RETURN" of FI-LevKod
DO:
  RUN SD-CURSOR (" ").
  RETURN NO-APPLY.  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-LevNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-LevNr C-Win
ON TAB OF FI-LevNr IN FRAME DEFAULT-FRAME /* Leverandør */
OR "RETURN" OF FI-LevNr
DO:
    IF INPUT FI-LevNr = 0 THEN
    DO:
        ASSIGN
            FI-LevNamn = wAlle.
        DISPLAY 
            "" @ FI-LevNr
            FI-LevNamn WITH FRAME default-frame.
    END.
    ELSE DO:
        FIND LEvBAs NO-LOCK WHERE
            LevBAs.LevNr = INPUT FI-LevNr NO-ERROR.
        IF AVAILABLE LEvBAs THEN
        DO:
            ASSIGN
                FI-LevNR   = LevBas.LEvNr
                FI-LevNamn = LevBAs.LEvNamn.
            DISPLAY 
                FI-LevNr
                FI-LevNamn
            WITH FRAME default-frame.
        END.
        ELSE 
          DISPLAY
              ""    @ FI-LEvNr
              wAlle @ FI-LevNamn
          WITH FRAME Default-frame.
    END.
 
    RUN SD-CURSOR (" ").
    RETURN NO-APPLY.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-LevPeriode2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-LevPeriode2 C-Win
ON TAB OF FI-LevPeriode2 IN FRAME DEFAULT-FRAME
OR "RETURN" OF FI-LevPeriode2
DO:
  ASSIGN
      FI-LevPeriode1
      FI-LevPeriode2.
  IF FI-LevPeriode2 < FI-LevPeriode1 THEN
  DO:
      MESSAGE "Til dato er mindre enn fra dato!"
          VIEW-AS ALERT-BOX ERROR TITLE "Feil".
      RETURN NO-APPLY.
  END.
  RUN SD-CURSOR (" ").
  RETURN NO-APPLY.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-OrdreNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-OrdreNr C-Win
ON LEAVE OF FI-OrdreNr IN FRAME DEFAULT-FRAME /* Ordrenummer */
OR "RETURN" OF FI-OrdreNr
DO:
  RUN SD-CURSOR (" ").
  RETURN NO-APPLY.  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Sentrallager
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Sentrallager C-Win
ON TAB OF FI-Sentrallager IN FRAME DEFAULT-FRAME /* Sentrallager */
OR "RETURN" OF FI-Sentrallager
DO:
  FIND clButiker NO-LOCK WHERE
    clButiker.butik = INPUT FI-Sentrallager NO-ERROR.
  IF AVAILABLE clbutiker THEN
  DO:
    ASSIGN
      FI-clNavn:SCREEN-VALUE = clButiker.ButNamn
      .
  END.
  ELSE DO:
    IF INPUT FI-Sentrallager > 0 THEN
    DO:
      MESSAGE "Ukjent sentrallager."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
    END.
  END.
  RUN SD-CURSOR (" ").
  RETURN NO-APPLY.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-VareBehNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-VareBehNr C-Win
ON TAB OF FI-VareBehNr IN FRAME DEFAULT-FRAME /* Vareh.bok */
or "RETURN" of FI-VareBehNr
DO:
  RUN SD-CURSOR (" ").
  RETURN NO-APPLY.  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-SOK-CHAR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-SOK-CHAR C-Win
ON RETURN OF FILL-IN-SOK-CHAR IN FRAME DEFAULT-FRAME
DO:
  APPLY "CHOOSE" TO BUTTON-Sok.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-SOK-DATE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-SOK-DATE C-Win
ON LEAVE OF FILL-IN-SOK-DATE IN FRAME DEFAULT-FRAME
DO:
    DEFINE VAR wDate AS DATE.
    ASSIGN wDate = DATE(FILL-IN-SOK-DATE:SCREEN-VALUE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        MESSAGE "Feilaktigt tastet dato." VIEW-AS ALERT-BOX ERROR TITLE "Feil dato".
        APPLY "ENTRY" TO FILL-IN-SOK-DATE.
        RETURN NO-APPLY.
    END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-SOK-DATE C-Win
ON RETURN OF FILL-IN-SOK-DATE IN FRAME DEFAULT-FRAME
DO:
    DEFINE VAR wDate AS DATE.
    wDate = DATE(FILL-IN-SOK-DATE:SCREEN-VALUE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        MESSAGE "Feilaktigt tastet dato." VIEW-AS ALERT-BOX ERROR TITLE "Feil dato".
        APPLY "ENTRY" TO FILL-IN-SOK-DATE.
        RETURN NO-APPLY.
    END.
  APPLY "CHOOSE" TO BUTTON-Sok.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-SOK-DECI
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-SOK-DECI C-Win
ON RETURN OF FILL-IN-SOK-DECI IN FRAME DEFAULT-FRAME
DO:
  APPLY "CHOOSE" TO BUTTON-Sok.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-SOK-INTE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-SOK-INTE C-Win
ON RETURN OF FILL-IN-SOK-INTE IN FRAME DEFAULT-FRAME
DO:
  APPLY "CHOOSE" TO BUTTON-Sok.
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


&Scoped-define SELF-NAME T-Annonsevarer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-Annonsevarer C-Win
ON VALUE-CHANGED OF T-Annonsevarer IN FRAME DEFAULT-FRAME /* Annonsevarer */
DO:
  RUN SD-CURSOR (" ").
  RETURN NO-APPLY.  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-DirLev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-DirLev C-Win
ON VALUE-CHANGED OF T-DirLev IN FRAME DEFAULT-FRAME /* Direkteleverte */
DO:
  RUN SD-CURSOR (" ").
  RETURN NO-APPLY.  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-LapTop
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-LapTop C-Win
ON VALUE-CHANGED OF T-LapTop IN FRAME DEFAULT-FRAME /* LapTop */
DO:
    RUN SD-CURSOR (" ").
    RETURN NO-APPLY.  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

{inutmld.i &Modus = "Opprett"} /* Melder fra at programmet har startet. */

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
{genlib.i
  &NoLibCall      = "Nei"
  &WindowName     = "Søkeliste Bestilling"
  &PostIClose     = " "
  &PostDisable_ui = "for each tmpChild:
                       if valid-handle(tmpChild.wChild) then
                         delete procedure tmpChild.wChild.
                     end.
                     IF VALID-HANDLE(chImage-Sko) THEN
                         RELEASE OBJECT chImage-Sko NO-ERROR.
                     IF VALID-HANDLE(Image-Sko) THEN
                         DELETE OBJECT Image-Sko NO-ERROR.
                     ASSIGN chImage-Sko = ?.
                     &IF DEFINED(UIB_IS_RUNNING) EQ 0 &THEN
                       return retur-verdi.
                     &else
                       message {&ip-variabel} retur-verdi view-as alert-box.
                     &endif

                       "
}
/* Dette skal inn i include igjen når problemet er fikset.  
  &PreIClose      = "RUN SaveBrowseSettings."
*/

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

{syspara.i 1 100 1 wAlle}
{syspara.i 1 1 18 cHKinst}
{syspara.i 5 4 17 cInnlevSamlet}


on F2,ALT-S of FRAME {&FRAME-NAME} anywhere 
do: 
    APPLY "choose" TO BUTTON-Sokart.
end.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

    RUN InitVars. /* inkl open-query */
    SUBSCRIBE TO "RefreshBest" ANYWHERE.
    IF RETURN-VALUE = "FEIL" THEN
       RETURN.
/*    IF "Komnavn" <> wSortCol:NAME THEN
        RUN QueryCustomSettings ("Komnavn"). */
    RUN SD-QUERY-OPEN.

    IF RETURN-VALUE = "FEIL" THEN
        LEAVE MAIN-BLOCK.

    RUN enable_UI.
    ASSIGN BUTTON-Innleveranse:SENSITIVE = cHKinst = "no".
    {lng.i} 

    /* Dette påvirker sortering m.m. og gir feil. Må fikses før det kan benyttes...
    {browsesettings.i {&BROWSE-NAME}}
    */
    IF BROWSE {&BROWSE-NAME}:CURRENT-COLUMN <> wSortCol THEN DO:
        wSortCol = BROWSE {&BROWSE-NAME}:CURRENT-COLUMN.
        RUN QueryCustomSettings (wSortCol:NAME).
    END.

    RUN InitBestStatus.
    /*run InitLev (no).   */
    
    assign
      FI-LEvNr          = 0
      FI-LevNamn        = wAlle.
    display 
        FI-LevNr
        FI-LevNamn
    with frame DEFAULT-FRAME.
    
    RUN LabelColor.
    RUN Move-Fill-To-Top. 
    &IF DEFINED(init-phrase) &THEN
    {&init-phrase}
    &ENDIF 
    IF AVAILABLE b{&br-tabell} THEN DO:
        RUN SD-Reposition.
    END. 
    ELSE IF AVAILABLE {&br-tabell} THEN
      do:
        find ArtBas of BestHode no-lock no-error.
        run VisBilde (1).
        REPOSITION {&BROWSE-NAME} TO ROW 1.
      end.
      
    APPLY "ENTRY" TO BROWSE {&BROWSE-NAME}.

  assign
    C-Win:hidden = false.
    
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

PROCEDURE LockWindowUpdate EXTERNAL "user32.dll" :
  DEFINE INPUT  PARAMETER hWndLock AS LONG.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Artikkelkort C-Win 
PROCEDURE Artikkelkort :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF AVAIL BestHode THEN
      FIND ArtBas WHERE ArtBas.ArtikkelNr = BestHode.ArtikkelNr NO-LOCK NO-ERROR.
  IF AVAIL ArtBas THEN DO:
      fLockvindu(TRUE).
      run w-vartkor (input recid(ArtBas), "ENDRE," + STRING(THIS-PROCEDURE)).
      fLockvindu(FALSE).
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

OCXFile = SEARCH( "w-bbesthode.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chImage-Sko = Image-Sko:COM-HANDLE
    UIB_S = chImage-Sko:LoadControls( OCXFile, "Image-Sko":U)
    Image-Sko:NAME = "Image-Sko":U
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "w-bbesthode.wrx":U SKIP(1)
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
  DISPLAY FI-VareBehNr FI-Beskr FI-LevFargKod FI-EkstId FI-clNavn CB-BestStat 
          FI-LevNr FI-LevKod FI-Sentrallager FI-OrdreNr FI-Bestillingsnr1 
          FI-Bestillingsnr2 FI-LevPeriode1 FI-LevPeriode2 FI-BestDato1 
          FI-BestDato2 CB-BestType T-DirLev T-Annonsevarer T-LapTop 
          FILL-IN-SOK-DATE FILL-IN-SOK-CHAR FILL-IN-SOK-INTE FILL-IN-SOK-DECI 
          FI-LevNamn 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE FI-VareBehNr BUTTON-Sokart FI-Beskr FI-LevFargKod FI-EkstId 
         BUTTON-SokLev BUTTON-SokLev-2 CB-BestStat FI-LevNr FI-LevKod 
         FI-Sentrallager B-Print FI-OrdreNr B-Print-2 B-PrintInnlev Btn_Help 
         FI-Bestillingsnr1 FI-Bestillingsnr2 Btn_OK BUTTON-ArtKort 
         FI-LevPeriode1 BUTTON-EndreBest FI-LevPeriode2 BUTTON-Innleveranse 
         FI-BestDato1 BUTTON-Kalkyle FI-BestDato2 BUTTON-NyBest CB-BestType 
         BUTTON-SettLopNr T-DirLev BUTTON-SlettBest T-Annonsevarer T-LapTop 
         FILL-IN-SOK-DATE FILL-IN-SOK-CHAR FILL-IN-SOK-INTE FILL-IN-SOK-DECI 
         BROWSE-BestHode BUTTON-Sok B-Blank B-Blank2 B-Oppdater B-Blank-3 
         B-Blank-4 B-Blank-5 B-BestDato B-BestNr btnSokKunde B-Blank-6 
         B-Blank-7 BUTTON-Avskriv RECT-49 RECT-50 RECT-51 RECT-52 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitBestStatus C-Win 
PROCEDURE InitBestStatus :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
--------------------------------------------------------------------------------*/
  def var wBestStatus as char no-undo.
  
  wBestStatus = wAlle.

  SYSPARA:
  for each SysPAra no-lock where
    SysPara.SysHId = 5 and
    SysPara.SysGr  = 2 and
    SysPara.ParaNr < 99:
    
    assign
      wBestStatus = wBestStatus + 
                   (if wBestStatus = "" 
                      then ""
                      else ",") +
                   string(SysPara.ParaNr,"z9") + ": " + SysPara.Parameter1.    
  end. /* SYSPARA */

  assign
    CB-BestStat = entry(1,wBestStatus)
    CB-BestStat:List-Items in frame DEFAULT-FRAME = wBestStatus.
  
  display CB-BestStat with frame {&FRAME-NAME}.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitVars C-Win 
PROCEDURE InitVars :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR wW     AS WIDGET NO-UNDO.
    DEF VAR wCount AS INTE   NO-UNDO.

    ASSIGN wAntSortCol = NUM-ENTRIES("{&ENABLED-FIELDS-IN-QUERY-{&BROWSE-NAME}}"," ")
           wAktivCol   = IF wAktivCol > wAntSortCol THEN 1 ELSE wAktivCol.
  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN
    DEF VAR wIdx  AS INTE NO-UNDO.
    DEF VAR wNumE AS INTE NO-UNDO.
    IF wAntSortCol < 1 THEN DO:
        MESSAGE "Du må 'enabla' minst ett felt" view-as alert-box.
        RETURN "FEIL".
    END.
    IF NUM-ENTRIES(wSorttype) <> wAntSortCol THEN DO:
        MESSAGE "&scope Sorttype skall ha " + STRING(wAntSortCol) +
                " entries i definitionsblocket," skip
                " kommaseparert med valgfritt BY, BY DESCENDING eller USE-INDEX." 
                VIEW-AS ALERT-BOX ERROR.
        RETURN "FEIL".
    END.
    IF NUM-ENTRIES(wSokvillkor) <> wAntSortCol THEN DO:
        MESSAGE "&scope Sokvillkor skall ha " + STRING(wAntSortCol) +
                " entries i definitionsblocket," skip
                " kommaseparert med valgfritt <=,>= eller =." SKIP
                "Aktuellt värde: " + wSokvillkor
                 VIEW-AS ALERT-BOX ERROR.
        RETURN "FEIL".
    END.
    DO wIdx = 1 TO wAntSortCol:
        CASE ENTRY(wIdx,wSorttype):
            WHEN "" OR WHEN "BY" OR WHEN "BY DESCENDING" OR WHEN "USE-INDEX" THEN.
            OTHERWISE DO:
                          MESSAGE "Tillåtna entries i 'scope Sort' = ''(tomt),BY,BY DESCENDING OCH USE-INDEX"
                                 VIEW-AS ALERT-BOX ERROR.
                          RETURN "FEIL".
                      END.
        END CASE.
    END.
    ASSIGN wNumE = NUM-ENTRIES(wBrowseIdx).
    IF NOT CAN-DO(wBrowseIdx,"USE-INDEX") AND NUM-ENTRIES(wBrowseIdx) <> wAntSortCol THEN DO:
        MESSAGE "Antall entries i 'scope BrowseIdx' <>" wAntSortCol VIEW-AS ALERT-BOX ERROR.
        RETURN "FEIL".
    END.
    DO wIdx = 1 TO wAntSortCol:
        IF ENTRY(wIdx,wSorttype) = "USE-INDEX" AND
                (wIdx > wNumE OR ENTRY(wIdx,"{&BrowseIdx}") = "") THEN DO:
            MESSAGE "Entry " wIdx " av 'scope Sorttype' = USE-INDEX och" SKIP
                    "entry " wIdx " saknas i 'scope BrowseIdx'" VIEW-AS ALERT-BOX ERROR.
            RETURN "FEIL".
        END.
    END.
  &ENDIF
    DO wCount = 1 TO wAntSortCol:
        ASSIGN wSearchCols = IF wSearchCols = "" THEN 
               ENTRY(
                     NUM-ENTRIES(ENTRY(wCount,"{&ENABLED-FIELDS-IN-QUERY-{&BROWSE-NAME}}"," "),"."),
                     ENTRY(wCount,"{&ENABLED-FIELDS-IN-QUERY-{&BROWSE-NAME}}"," "),"."
                    )
               ELSE
                 wSearchCols + "," + 
                   ENTRY(NUM-ENTRIES(ENTRY(wCount,"{&ENABLED-FIELDS-IN-QUERY-{&BROWSE-NAME}}"," "),"."),ENTRY(wCount,"{&ENABLED-FIELDS-IN-QUERY-{&BROWSE-NAME}}"," "),".").
    END.
    ASSIGN wW        = BROWSE {&BROWSE-NAME}:FIRST-COLUMN
           wOrgBgCol = wW:Label-bgcolor
           wCount = 1.
    REPEAT WHILE VALID-HANDLE(wW):
        IF LOOKUP(wW:NAME,wSearchCols) > 0 THEN
          do:
            ASSIGN wW:PRIVATE-DATA      = PREP-PRIVATE-DATA(wW:HANDLE,wCount)
                   wSearchColsH[wCount] = wW:HANDLE
                   wW:LABEL = wW:LABEL + STRING(LOOKUP("*",wW:LABEL," ") = 0," */")
                   wW:LABEL = wW:LABEL + IF ENTRY(wCount,wSortType) = "USE-INDEX" THEN
                                "" ELSE "*"
                   wCount               = wCount + 1
                   wW:READ-ONLY         = YES.
          end.
        ASSIGN wW = wW:NEXT-COLUMN.
    END.
    ASSIGN BROWSE {&BROWSE-NAME}:CURRENT-COLUMN = wSearchColsH[wAktivCol]
           wSortCol                             = wSearchColsH[wAktivCol]
           wQ                                   = BROWSE {&BROWSE-NAME}:QUERY.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Innleveranse C-Win 
PROCEDURE Innleveranse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var wBestHodeRecid as recid no-undo.

  if not available BestHode then 
    return no-apply.
  find ArtBas of BestHode no-lock no-error.
  
  if not available ArtBAs then
    return no-apply.
    
  if ArtBas.LopNr = 0 or ArtBas.LopNr = ? then
    do:
      message "Artikkelen må tildeles løpenummer før innleveranse kan gjøres!"
        view-as alert-box MESSAGE title "Melding".
      return no-apply.
    end.
    
  if not available BestHode then
    return no-apply.

  if BestHode.BestStat < 2  then
    do:
      message "Bestilling med denne status kan ikke innleveres!"
        view-as alert-box title "Melding".
      return no-apply.
    end.  
  assign
    wBestHodeRecid = recid(BestHode).
  fLockvindu(TRUE).
  run w-gridinnlev.w  (input recid(ArtBas), input-output wBestHodeRecid, "INLEV").
  /*RUN SD-CURSOR (" ").*/
  fLockvindu(FALSE).
  FIND CURRENT besthode NO-LOCK NO-ERROR.
  BROWSE {&BROWSE-NAME}:REFRESH().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LabelColor C-Win 
PROCEDURE LabelColor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR wColorCol AS WIDGET NO-UNDO.
  ASSIGN wColorCol = BROWSE {&BROWSE-NAME}:FIRST-COLUMN.
  REPEAT WHILE VALID-HANDLE(wColorCol):
      IF LOOKUP("{&br-tabell}." + wColorCol:NAME,"{&ENABLED-FIELDS-IN-QUERY-{&BROWSE-NAME}}"," ") > 0 THEN
          ASSIGN wColorCol:LABEL-BGCOLOR = 
            IF wColorCol:NAME = wSortCol:NAME THEN wSortBgCol ELSE wOrgBgCol.
      ASSIGN wColorCol = wColorCol:NEXT-COLUMN.
  END.
   RETURN NO-APPLY.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LimFraClipBoard C-Win 
PROCEDURE LimFraClipBoard :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var wLoop        as int  no-undo.
  def var wNyFil       as char no-undo.
  def var wKatalog     as char no-undo.
  def var wReturnValue as char no-undo.
  def var wFilNAvn     as char no-undo.
  def var wBildNr      like BildeRegister.BildNr no-undo.
  
  def buffer bufArtBas for ArtBas.

do with frame DEFAULT-FRAME:

  if not available BestHode then
    return.
    
  if available BestHode then
    find ArtBas of BestHode no-lock no-error.
  if not available ArtBas then
    return.

  /* Sjekker om det ligger noe i ClipBoard. */
  if ClipBoard:num-formats > 0 then
    do:
      message skip(1)
              "       ClipBoard er tomt!" skip
              "              eller" skip
              "det inneholder ugyldige data" skip(1)
              view-as alert-box title "Melding".
      return no-apply.
    end.

  /* Legger bilde inn i buffer fra ClipBoard. */
  chIMAGE-Sko:Picbuf:PasteClipboard.

  /* Finner første ledige bildenummer */
  if valid-handle(wLibHandle) then
    do:
      run BildeNummer in wLibHandle (input "B", output wBildNr, output wFilNavn, output wKatalog).
  
      if return-value = "AVBRYT" then
        do:
          message "Klarte ikke å skape et filnavn!" view-as alert-box 
            title "Melding".
          return no-apply.
        end.
      find BildeRegister no-lock where
         BildeRegister.BildNr = wBildNr no-error.
    end.
  else do:
    message "Prosedyrebiblioteket er ikke startet!" view-as alert-box Title "Melding".
    return no-apply.
  end.
  
  /* Setter bildenummer i ArtBas */
  do for bufArtBas TRANSACTION:
    find bufArtBas exclusive-lock where
      recid(bufArtBAs) = recid(ArtBas) no-error.
    if available bufArtBas then
      do:
        assign
          bufArtBas.BildNr = wBildNr.
        release bufArtBas.
      end.
  end.

  /* Tildeler filnavn */
  chIMAGE-Sko:Picbuf:FileName = wKatalog + "\" + wFilNavn.
  
  /* Lagrer bilde på hd. ------------------------------------------------ */
  If chIMAGE-Sko:Picbuf:WriteCompression = 0 Then  /* Filen skal komprimeres.    */
    chIMAGE-Sko:Picbuf:WriteCompression = 65.
  chIMAGE-Sko:Picbuf:Store.                 /* Lagre filen til HD.        */
  If chIMAGE-Sko:Picbuf:WriteCompression <> 0 Then /* Filen skal komprimeres.    */
    chIMAGE-Sko:Picbuf:WriteCompression = 0.
    
  /* Leser inn filen. */
  assign wReturnValue = "AVBRYT".
  if search(wKatalog + "\" + wFilNavn) <> ? then
    do:
      if valid-handle(wLibHandle) then
        run LesInnBilde in wLibHandle (BildeRegister.BildNr, wKatalog + "\" + wFilNavn, output wReturnValue).
    end.
  if wReturnValue = "AVBRYT" then
    do:
      message "Feil ved lasting av bilde " BildeRegister.BildNr
        view-as alert-box error title "Feil".
    end.
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Move-Fill-To-Top C-Win 
PROCEDURE Move-Fill-To-Top :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        CASE SUBSTR(wSortCol:DATA-TYPE,1,4).
             WHEN "INTE" THEN
                 ASSIGN wAktivFillIn = FILL-IN-SOK-INTE:HANDLE.
             WHEN "CHAR" THEN
                 ASSIGN wAktivFillIn = FILL-IN-SOK-CHAR:HANDLE.
             WHEN "DATE" THEN
                 ASSIGN wAktivFillIn = FILL-IN-SOK-DATE:HANDLE.
             WHEN "DECI" THEN
                 ASSIGN wAktivFillIn = FILL-IN-SOK-DECI:HANDLE.
        END CASE.
    END.
    wAktivFillIn:MOVE-TO-TOP().
    ASSIGN wAktivFillIn:FORMAT                               = 
           IF wSortCol:DATA-TYPE = "INTEGER" THEN FILL(">",LENGTH(wSortCol:FORMAT)) ELSE wSortCol:FORMAT
           FILL-IN-SOK-DATE:SENSITIVE IN FRAME {&FRAME-NAME} = SUBSTR(wSortCol:DATA-TYPE,1,4) = "DATE"
           FILL-IN-SOK-INTE:SENSITIVE IN FRAME {&FRAME-NAME} = SUBSTR(wSortCol:DATA-TYPE,1,4) = "INTE"
           FILL-IN-SOK-CHAR:SENSITIVE IN FRAME {&FRAME-NAME} = SUBSTR(wSortCol:DATA-TYPE,1,4) = "CHAR"
           FILL-IN-SOK-DECI:SENSITIVE IN FRAME {&FRAME-NAME} = SUBSTR(wSortCol:DATA-TYPE,1,4) = "DECI".
    RETURN NO-APPLY.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrevNext C-Win 
PROCEDURE PrevNext :
/*------------------------------------------------------------------------------
  Purpose:     bläddring från artikelkort
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER cRettning AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE pcState AS CHARACTER  NO-UNDO.
    IF CAN-DO("Prev,Next",cRettning) THEN DO:
        CASE cRettning:
            WHEN "Prev" THEN DO:
                IF NOT BROWSE BROWSE-BestHode:SELECT-PREV-ROW() THEN
                    RETURN.
            END.
            WHEN "Next" THEN DO:
                IF NOT BROWSE BROWSE-BestHode:SELECT-NEXT-ROW() THEN
                    RETURN.
            END.
        END CASE.
        PUBLISH "ByttArtikkel" (BestHode.ArtikkelNr).
        APPLY "VALUE-CHANGED" TO BROWSE {&BROWSE-NAME}.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE QueryCustomSettings C-Win 
PROCEDURE QueryCustomSettings :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER wInitSortColNavn AS CHAR NO-UNDO. 
    DEF VAR wW            AS WIDGET        NO-UNDO.
    DEF VAR wCount        AS INTE   INIT 1 NO-UNDO.
    DEF VAR wLookUp       AS INTE          NO-UNDO.
    DEF VAR wNySearchCols AS CHAR          NO-UNDO.
    DEF VAR wNySokvillkor AS CHAR          NO-UNDO.
    DEF VAR wNyBrowseIdx  AS CHAR          NO-UNDO.
    ASSIGN wW = BROWSE {&BROWSE-NAME}:FIRST-COLUMN
           wInitSortColNavn = IF wInitSortColNavn = "" THEN
                                  ENTRY(wAktivCol,wSearchCols)
                              ELSE wInitSortColNavn.
    
    REPEAT WHILE VALID-HANDLE(wW):
      ASSIGN wLookUp = LOOKUP(wW:NAME,wSearchCols).
      IF wLookUp /* LOOKUP(wW:NAME,wSearchCols) */ > 0 THEN
            ASSIGN wNySearchCols = IF wNySearchCols = "" THEN wW:NAME ELSE
                                      wNySearchCols + "," + wW:NAME
                   wNySokvillkor = IF wNySokvillkor = "" THEN ENTRY(wLookup,wSokvillkor) ELSE
                                      wNySokvillkor + "," + ENTRY(wLookup,wSokvillkor)
                   wNyBrowseIdx = IF wNyBrowseIdx = "" THEN ENTRY(wLookup,wBrowseIdx) ELSE
                                      wNyBrowseIdx + "," + ENTRY(wLookup,wBrowseIdx)
                   wSearchColsH[wCount] = wW:HANDLE
                   wSortCol             = IF wW:NAME = wInitSortColNavn THEN wW:HANDLE ELSE wSortCol
                   wAktivCol            = IF wW:NAME = wInitSortColNavn THEN wCount ELSE wAktivCol
                   wCount               = wCount + 1.
        ASSIGN wW = wW:NEXT-COLUMN.
    END. 
    ASSIGN wSearchCols = wNySearchCols
           wSokvillkor = wNySokvillkor
           wBrowseIdx     = wNyBrowseIdx.
/*
    RUN SortNyCol. 
    */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RefreshBest C-Win 
PROCEDURE RefreshBest :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER cMode AS CHARACTER  NO-UNDO.
    IF cMode = "SLETT" THEN /* tas om hand i slettbestilling */
        RETURN.
    IF cMode = "NY" THEN DO:
        RUN SD-Cursor (" ").
    END.
    ELSE
        BROWSE {&BROWSE-NAME}:REFRESH() NO-ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SD-Any-Printable C-Win 
PROCEDURE SD-Any-Printable :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
    CASE SUBSTR(wSortCol:DATA-TYPE,1,4):
        WHEN "INTE" OR WHEN "DATE" THEN DO:
            IF KEY-FUNCTION(LASTKEY) < "0" OR KEY-FUNCTION(LASTKEY) > "9" THEN
                RETURN NO-APPLY.
        END.
    END CASE.
    APPLY "ENTRY" TO wAktivFillIn.
    APPLY LASTKEY.
    IF wSortCol:DATA-TYPE = "INTEGER" THEN
        ASSIGN wAktivFillIn:CURSOR-OFFSET = 2.
    /*
    CASE SUBSTR(wSortCol:DATA-TYPE,1,4):
        WHEN "INTE" THEN DO:
            APPLY "ENTRY" TO FILL-IN-SOK-INTE.
            APPLY LASTKEY. 
            FILL-IN-SOK-INTE:CURSOR-OFFSET = 2.
        END.
        WHEN "CHAR" THEN DO:
            APPLY "ENTRY" TO FILL-IN-SOK-CHAR.
            APPLY LASTKEY.
        END.
        WHEN "DATE" THEN DO:
            APPLY "ENTRY" TO FILL-IN-SOK-DATE.
            APPLY LASTKEY.
        END.
    END CASE.
    */
  END.
    RETURN NO-APPLY.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SD-Cursor C-Win 
PROCEDURE SD-Cursor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER wLeft-Right AS CHAR NO-UNDO.
    def var wW as widget no-undo.
  
    run lockwindowupdate(frame {&FRAME-NAME}:hwnd).

    CASE wLeft-Right:
        WHEN "LEFT" THEN
            ASSIGN wAktivCol = IF wAktivCol = 1 THEN wAntSortCol ELSE wAktivCol - 1.
        WHEN "RIGHT" THEN
            ASSIGN wAktivCol = IF wAktivCol = wAntSortCol THEN 1 ELSE wAktivCol + 1.
    END CASE.
    ASSIGN wSortCol                             = wSearchColsH[wAktivCol]
           BROWSE {&BROWSE-NAME}:CURRENT-COLUMN = wSortCol.
    RUN SortNyCol.
    
    APPLY "ENTRY" TO BROWSE {&BROWSE-NAME}.
    APPLY "VALUE-CHANGED" TO BROWSE {&BROWSE-NAME}.
    run lockwindowupdate(0).
    
    RETURN NO-APPLY.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SD-Query-Open C-Win 
PROCEDURE SD-Query-Open :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var wWhere  as char no-undo.
  DEF VAR w2Where AS CHAR NO-UNDO.
  
  run SettWhereSats (output wWhere).
   
  assign
    wAktivQString = wSortCol:PRIVATE-DATA.
  IF wWhere <> "" THEN
    wAktivQString = REPLACE(wAktivQString, "NO-LOCK", wWhere).


  FRAME-SCOOP:
  do with frame DEFAULT-FRAME:
  
    /* Bygger where sats */
    assign
      w2Where = IF (INPUT FI-Beskr <> "" OR INPUT FI-LevFargKod <> "")  
                  THEN "where "
                  ELSE ""
      w2Where = w2Where + 
                (if input FI-Beskr <> ""
                 then (if substring(input FI-Beskr,1,1) = "*"
                         then "ArtBas.Beskr matches '"
                         else "ArtBas.Beskr begins '") + 
                       input FI-Beskr + 
                      (if input FI-Beskr BEGINS "*" then "*" else "") +
                       "'"
                 else "")
      w2Where = w2Where + 
                (if input FI-LevFargKod <> ""
                 then (if w2Where = "where " then "" else "and ") + 
                      (if substring(input FI-LevFargKod,1,1) = "*"
                         then "ArtBas.LevFargKod matches '"
                         else "ArtBas.LevFargKod begins '") + 
                       input FI-LevFargKod + 
                       (if input FI-LevFargKod BEGINS "*" then "*" else "") +
                       "'" 
               else "")
      .
    ASSIGN
      wAktivQString = REPLACE(wAktivQString,"BY"," ,each ArtBas of BestHode " + w2Where +  " no-lock BY").

     {sww.i}
     /* wQ:QUERY-PREPARE(wSortCol:PRIVATE-DATA). */
     wQ:QUERY-PREPARE(wAktivQString).
     wQ:QUERY-OPEN().
     {swn.i}
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SD-Reposition C-Win 
PROCEDURE SD-Reposition :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        if wBlank = ? then
          REPOSITION {&BROWSE-NAME} TO ROWID rowid(b{&br-tabell}) NO-ERROR.
        ASSIGN wAktivFillIn:SCREEN-VALUE = "".
        APPLY "ENTRY" TO BROWSE {&BROWSE-NAME}.
    END.
    RETURN NO-APPLY.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetLopenummer C-Win 
PROCEDURE SetLopenummer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
do with frame DEFAULT-FRAME:
  if not available ArtBas then
    return no-apply.
    
  run d-vtildelopnr.w (input recid(ArtBas)).  
  if return-value = "AVBRYT" then
    return no-apply.
  ELSE
    RUN genInterleaf.p (ArtBas.Artikkelnr).

end.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettWhereSats C-Win 
PROCEDURE SettWhereSats :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def output parameter wWhere as char no-undo.

  if frame DEFAULT-FRAME:visible = false then
    return.

  FRAME-SCOOP:
  do with frame DEFAULT-FRAME:
  
  /* Bygger where sats */
  assign
    wWhere = (if input CB-BestStat <> wAlle
               then (if wWhere = "" then "" else "and ") + "BestHode.BestStat = " + entry(1,input CB-BestStat,":")
               else "") 
    wWhere = wWhere + " " +
             (if input FI-LevNr <> 0
               then (if wWhere = "" then "" else "and ") + "BestHode.LevNr = " + string(INPUT FI-LevNr)
               else "")
    wWhere = wWhere + " " +
             (if input FI-Sentrallager <> 0
               then (if wWhere = "" then "" else "and ") + "BestHode.CL = " + string(INPUT FI-Sentrallager)
               else "")
    wWhere = wWhere + " " +
             (if input FI-VareBehNr <> 0
               then (if wWhere = "" then "" else "and ") + "BestHode.VareBehNr = " + string(INPUT FI-VareBehNr)
               else "")
    wWhere = wWhere + " " +
             (if input T-Annonsevarer
               then (if wWhere = "" then "" else "and ") + "BestHode.Anonseartikkel = true"
               else "")
    wWhere = wWhere + " " +
             (if input T-LapTop
               then (if wWhere = "" then "" else "and ") + "BestHode.LapTop = true"
               else "")
    wWhere = wWhere + " " +
             (if input T-DirLev
               then (if wWhere = "" then "" else "and ") + "BestHode.DirekteLev = true"
               else "")
    wWhere = wWhere + " " +
             (if input FI-LevKod <> ""
               then (if wWhere = "" then "" else "and ") + 
                     (if substring(input FI-LevKod,1,1) = "*"
                       then "BestHode.LevKod matches '"
                       else "BestHode.LevKod begins '") + 
                     input FI-LevKod + 
                     (if substring(input FI-LevKod,1,1) = "*"
                       then "*"
                       else "") + "'"
               else "")
    wWhere = wWhere + " " +
             (if input FI-LevPeriode1 <> ? 
               then (if wWhere = "" then "" else "and ") + 
                    " BestHode.LevDato >= " + STRING(INPUT FI-LevPeriode1)  
               else "")
    wWhere = wWhere + " " +
             (if INPUT FI-LevPeriode2 <> ?
               then (if wWhere = "" then "" else "and ") + 
                    " BestHode.LevDato <= " + STRING(INPUT FI-LevPeriode2) 
               else "")
    wWhere = wWhere + " " +
             (if input FI-BestDato1 <> ? 
               then (if wWhere = "" then "" else "and ") + 
                    " BestHode.BestillingsDato >= " + STRING(INPUT FI-BestDato1)                    
               else "")
    wWhere = wWhere + " " +
             (if INPUT FI-BestDato2 <> ?
               then (if wWhere = "" then "" else "and ") + 
                    " BestHode.BestillingsDato <= " + STRING(INPUT FI-BestDato2)                    
               else "")
    wWhere = wWhere + " " +
             (if input FI-OrdreNr <> 0
               then (if wWhere = "" then "" else "and ") + "BestHode.OrdreNr = " + string(INPUT FI-OrdreNr)
               else "")
    wWhere = wWhere + " " +
             (if input FI-BestillingsNr1 <> 0
               then (if wWhere = "" then "" else "and ") + "BestHode.BestNr >= " + string(INPUT FI-BestillingsNr1)
               else "")
    wWhere = wWhere + " " +
             (if input FI-BestillingsNr2 <> 0
               then (if wWhere = "" then "" else "and ") + "BestHode.BestNr <= " + string(INPUT FI-BestillingsNr2)
               else "")
    wWhere = wWhere + " " +
             (if input CB-BestType <> 0
               then (if wWhere = "" then "" else "and ") + "BestHode.BestType = " + string(INPUT CB-BestType)
               else "")
      wWhere = wWhere + " " +
               (if input FI-EkstId <> ""
                 then (if wWhere = "" then "" else "and ") + "BestHode.EkstId = '" + TRIM(INPUT FI-EkstId) + "'"
                 else "")
    wWhere = if wWhere <> "" then "NO-LOCK where " + wWhere else "NO-LOCK".
    
  end. /* FRAME SCOOP */       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettBestilling C-Win 
PROCEDURE SlettBestilling :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
do with frame DEFAULT-FRAME:
  def var wBestHodeRecid as recid no-undo.
  
  if not available BestHode then
    return no-apply.
  /*
  if not available ArtBas then
    return no-apply.
  */
  assign
    wBestHodeRecid = recid(BestHode).

  if can-do(",4",string(BestHode.BestStat)) then
    do:
      message "Bestillingen er sendt til leverandør!" skip
              "Skal den alikevel slettes?"
              view-as alert-box buttons yes-no-cancel title "Bekreftelse"
              update wOk.
    end.
  ELSE if can-do(",3",string(BestHode.BestStat)) then
    do:
      message "Bestillingen er koblet til ordre!" skip
              "Skal den alikevel slettes?"
              view-as alert-box buttons yes-no-cancel title "Bekreftelse"
              update wOk.
    end.
  else do:  
    message "Skal bestilling slettes?"
      view-as alert-box buttons yes-no-cancel title "Bekreftelse" 
    update wOk.
  end.

  if wOk = true then
    do transaction:
      {sww.i}
      /* KanSlettes*/      
      run w-gridord.w (input (IF AVAILABLE ArtBas
                                THEN recid(ArtBas)
                                ELSE ?), input-output wBestHodeRecid, "SLETT").
      {swn.i}
      RUN SD-CURSOR (" ").
      RETURN NO-APPLY.  
  end.
  else return no-apply.
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SortNyCol C-Win 
PROCEDURE SortNyCol :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN wAktivCol  =  LOOKUP(wSortCol:NAME,wSearchCols)
           BROWSE {&BROWSE-NAME}:CURRENT-COLUMN = wSortCol.
    FIND b{&br-tabell} WHERE ROWID(b{&br-tabell}) = ROWID({&br-tabell}) NO-LOCK no-error.

    RUN SD-QUERY-OPEN.
    RUN Move-Fill-To-Top.
    RUN LabelColor.
    RUN SD-Reposition.

    RETURN NO-APPLY.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getBekreftet C-Win 
FUNCTION getBekreftet RETURNS CHARACTER
  (  ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR cTekst AS CHAR NO-UNDO.
      cTekst = STRING(Besthode.bekreftetdato <> ?," J/").

  RETURN cTekst.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Prep-Private-Data C-Win 
FUNCTION Prep-Private-Data RETURNS CHARACTER
  ( INPUT wQueryCol AS WIDGET,INPUT wQueryCol# AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
   DEFINE VAR wQStr      AS CHAR NO-UNDO.
   DEFINE VAR wXSorttype AS CHAR NO-UNDO. 
   DEFINE VAR wXSort     AS CHAR NO-UNDO. 
   ASSIGN wXSorttype = IF ENTRY(wQueryCol#,wSorttype) = "" THEN "BY" ELSE
                          ENTRY(wQueryCol#,wSorttype)
          wXSort     = IF wXSorttype = "BY" THEN
                           "{&br-tabell}." + wQueryCol:Name
                       ELSE IF wXSorttype = "BY DESCENDING" THEN
                           "{&br-tabell}." + wQueryCol:Name + " DESCENDING" 
                       ELSE
                           ENTRY(wQueryCol#,"{&BrowseIdx}")
          wXSorttype = IF wXSorttype = "BY DESCENDING" THEN "BY" ELSE
                              wXSorttype
          wQStr = REPLACE("{&BrowseQ}","XSORTTYPE",wXSorttype)
          wQStr = REPLACE(wQStr,"XSORT",wXSort).
  RETURN wQStr.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

