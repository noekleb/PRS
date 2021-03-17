&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win


/* Temp-Table and Buffer definitions                                    */
DEFINE NEW SHARED TEMP-TABLE TT_OvBuffer NO-UNDO LIKE OvBuffer.



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
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
  DEF VAR wTelleHodeRecid AS RECID NO-UNDO.
&ELSE
  DEF INPUT PARAMETER wTelleHodeRecid AS RECID NO-UNDO.
&ENDIF

DEF VAR wVgLopNr    LIKE TelleLinje.VgLopNr NO-UNDO.
DEF VAR wButik AS   INT           NO-UNDO.

DEF VAR cLokasjonRowIdList AS CHAR   NO-UNDO.
DEF VAR cLokasjonIdList    AS CHAR   NO-UNDO.
DEF VAR bOk                AS LOG    NO-UNDO.
DEF VAR hSetGetNr          AS HANDLE NO-UNDO.

&scope br-tabell   Tellelinje
/* &scope Sorttype    BY,BY,BY,BY,BY,BY,BY,BY,BY,BY,BY,BY */
&scope Sorttype    USE-INDEX,BY,BY,BY,BY,BY,BY,BY,BY,BY,BY,BY,BY
/* &scope Sorttype    BY,BY,BY,BY,BY,BY,BY,BY,BY,BY,BY,BY */
&scope BrowseIdx   VgLopNr,,Beskr,,,LevKod,,,,,,,
/* &scope BrowseIdx   VgLopNr,Beskr,Storl,Kode,LevKod,AntallPAr,AntallTalt,Nedskrevet,VVarekost,AntallDiff,VerdiDiff,Merknad */
&scope Sokvillkor  >=,>=,>=,>=,>=,>=,>=,>=,>=,>=,>=,>=,>=
&scope InitIdx     SortVgLopNrStorl  
&scope ip-felt     
/* Om du önskar input parameter. Ger en startup-record */ 
/*
&scope ip-variabel w{&ip-felt}
*/

&scope assign-retur-verdi ASSIGN retur-verdi = if available {&br-tabell} ~
                                                then STRING({&br-tabell}.LevNr) ~
                                                else "".
&scope BrowseInitQ FOR EACH {&br-tabell} NO-LOCK WHERE {&br-tabell}.TelleNr = XTELLENR USE-INDEX VgLopNr
/* &scope BrowseInitQ FOR EACH {&br-tabell} NO-LOCK WHERE {&br-tabell}.TelleNr = XTELLENR BY {&br-tabell}.VgLopNr */
/* &scope BrowseInitQ FOR EACH {&br-tabell} NO-LOCK WHERE {&br-tabell}.TelleNr = XTELLENR AND {&br-tabell}.Butik = XBUTIKK BY VgLopNr */
&scope BrowseQ     FOR EACH {&br-tabell} NO-LOCK XSORTTYPE XSORT INDEXED-REPOSITION
&scope BrowseSQ    FOR EACH b{&br-tabell} NO-LOCK WHERE b{&br-tabell}.TelleNr = XTELLENR AND b{&br-tabell}.Butik = XBUTIKK AND b{&br-tabell}.XFIELD XSOKV XFILL USE-INDEX XIDX MAX-ROWS 1

/* /* Parameter Definisjoner ---                                           */             */
/* &IF LENGTH("{&ip-variabel}") > 0 &THEN                                                 */
/*                                                                                        */
/*   &scope return-ip   ASSIGN {&ip-variabel} = {&br-tabell}.{&ip-felt}                   */
/*   &scope init-phrase FIND b{&br-tabell} WHERE b{&br-tabell}.{&ip-felt} = ~             */
/*                         {&ip-variabel} USE-INDEX {&InitIdx} NO-LOCK NO-ERROR.          */
/*                                                                                        */
/*   &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN                                               */
/*     DEFINE VAR {&ip-variabel} LIKE {&br-tabell}.{&ip-felt} INIT 6 NO-UNDO.             */
/*   &ELSE                                                                                */
/*     DEFINE INPUT-OUTPUT PARAMETER {&ip-variabel} LIKE {&br-tabell}.{&ip-felt} NO-UNDO. */
/*   &ENDIF                                                                               */
/*                                                                                        */
/* &ENDIF                                                                                 */

/* Preprossessor direktiver ---                                         */

/* Buffer og Temp-Table Definisjoner ---                                */
DEFINE BUFFER b{&br-tabell} FOR {&br-tabell}.
DEFINE QUERY wSQ FOR b{&br-tabell} SCROLLING.
DEFINE TEMP-TABLE tmpChild
  FIELD wChild AS HANDLE.

/* Lokale variabler ---                                                 */

DEFINE VAR retur-verdi       AS CHAR INIT "AVBRYT" NO-UNDO.

DEFINE VAR wAktivCol         AS INT INIT 1  NO-UNDO.
DEFINE VAR wOrgBgCol         AS INT         NO-UNDO.
DEFINE VAR wSortBgCol        AS INT INIT 15 NO-UNDO.
DEFINE VAR wSearchCols       AS CHAR       NO-UNDO.
DEFINE VAR wSearchColsH      AS WIDGET EXTENT 15 NO-UNDO.
DEFINE VAR wQ                AS WIDGET      NO-UNDO.
DEFINE VAR wSortCol          AS WIDGET      NO-UNDO.
DEFINE VAR wAntSortCol       AS INTE        NO-UNDO.
DEFINE VAR wAktivFillIn      AS WIDGET      NO-UNDO.
DEFINE VAR wSorttype         AS CHAR   INIT "{&Sorttype}"   NO-UNDO.
DEFINE VAR wSokvillkor       AS CHAR   INIT "{&Sokvillkor}" NO-UNDO.
DEFINE VAR wBrowseIdx        AS CHAR   INIT "{&BrowseIdx}"  NO-UNDO.
DEFINE VAR wOk               AS LOG         NO-UNDO.
DEFINE VAR wAlle             AS CHAR        NO-UNDO.
DEFINE VAR wAktivQString     AS CHAR        NO-UNDO.
DEFINE VAR wBekreft          AS LOG         NO-UNDO.
DEFINE VAR wBlank            AS LOG         NO-UNDO.
DEFINE VAR wRetStatus        AS LOG         NO-UNDO.
DEFINE VAR wPostadresse      AS CHAR LABEL "Postadresse" NO-UNDO.
DEFINE VAR wExcEkstent       AS CHAR        NO-UNDO.
DEF VAR cFileName        AS CHAR  NO-UNDO. 
DEF VAR cExcEkstent      AS CHAR  NO-UNDO.
DEF VAR cKunde           AS CHAR  NO-UNDO.
DEF VAR cSkoTex          AS CHAR  NO-UNDO.
DEFINE VARIABLE cSkomodus AS CHARACTER  NO-UNDO.
/*----*/
DEF VAR wRecid        AS RECID                 NO-UNDO.
DEF VAR wNedskriv     AS LOG  INITIAL FALSE    NO-UNDO.
DEF VAR wTekst        AS CHAR                  NO-UNDO.
DEF VAR wStdTxt       AS CHAR                  NO-UNDO.
DEF VAR iCl           AS INT                   NO-UNDO.
DEF VAR wEDB-System   AS CHAR                  NO-UNDO.
DEF VAR wTabell       AS CHAR                  NO-UNDO.  
DEF VAR wParentHandle AS HANDLE                NO-UNDO.
DEF VAR wLinjeHandle  AS HANDLE                NO-UNDO.
DEFINE VARIABLE iCLProfilnr     AS INTEGER    NO-UNDO.
DEFINE VARIABLE iWidthPix  AS INTEGER    NO-UNDO.
DEFINE VARIABLE iHeightPix AS INTEGER    NO-UNDO.

DEFINE VAR wAktivDataType    AS CHAR INIT "{&init-datatype}" NO-UNDO.
DEFINE VAR wLoop             AS INT         NO-UNDO.
DEFINE VAR wDiffColor        AS INT         NO-UNDO.
DEFINE VAR wDiffColor2       AS INT         NO-UNDO.
DEFINE VAR wDiffColor3       AS INT         NO-UNDO.
DEF    VAR wSvar             AS LOG         NO-UNDO.

DEF VAR cVareTekst           AS CHAR        NO-UNDO.
DEF VAR lPris                AS DEC FORMAT "->>>,>>9.99" NO-UNDO.
DEF VAR lTilbPris            AS DEC FORMAT "->>>,>>9.99" NO-UNDO.

DEFINE VARIABLE cFilterStr AS CHARACTER  NO-UNDO.

DEF BUFFER tmpTellelinje FOR Tellelinje.
DEF BUFFER clButiker     FOR Butiker.
DEFINE VARIABLE hEtikettVindu AS HANDLE     NO-UNDO.
DEFINE TEMP-TABLE TT_Suppler NO-UNDO
    FIELD Artikkelnr LIKE ArtBas.Artikkelnr
    FIELD Vg         LIKE ArtBas.Vg.

{runlib.i}
{windows.i}

/* {vargrliste.i &New="New"} */
DEF STREAM Eksport.
DEF STREAM sExportFile.
{methodexcel.i}
{incl/DevMode.i}
{incl/CustDevMode.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BROWSE-TelleLinje
&Scoped-define QUERY-NAME QUERY-Alle

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES TelleLinje bTellelinje tmpTellelinje

/* Definitions for BROWSE BROWSE-TelleLinje                             */
&Scoped-define FIELDS-IN-QUERY-BROWSE-TelleLinje TelleLinje.VgLopNr ~
TelleLinje.ArtikkelNr TelleLinje.Beskr TelleLinje.Storl TelleLinje.Kode ~
TelleLinje.LevKod TelleLinje.LevFargKod TelleLinje.AntallPar ~
TelleLinje.AntallTalt TelleLinje.Nedskrevet TelleLinje.VVareKost ~
TelleLinje.AntallDiff TelleLinje.VerdiDiff lPris TelleLinje.RabKr ~
TelleLinje.Merknad lTilbPris TelleLinje.Oppdatert TelleLinje.OpprVerdi ~
TelleLinje.OpptVerdi TelleLinje.EDato TelleLinje.BrukerID TelleLinje.Farg ~
TelleLinje.LevNr TelleLinje.MatKod TelleLinje.Sasong TelleLinje.Vg ~
TelleLinje.SeqNr 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-TelleLinje TelleLinje.VgLopNr ~
TelleLinje.ArtikkelNr TelleLinje.Beskr TelleLinje.Storl TelleLinje.Kode ~
TelleLinje.LevKod TelleLinje.AntallPar TelleLinje.AntallTalt ~
TelleLinje.Nedskrevet TelleLinje.VVareKost TelleLinje.AntallDiff ~
TelleLinje.VerdiDiff TelleLinje.Merknad 
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-TelleLinje TelleLinje
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-TelleLinje TelleLinje
&Scoped-define QUERY-STRING-BROWSE-TelleLinje FOR EACH TelleLinje ~
      WHERE TelleLinje.TelleNr = TelleHode.TelleNr and ~
TelleLinje.Butik   = wButik NO-LOCK ~
    BY TelleLinje.TelleNr DESCENDING INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-TelleLinje OPEN QUERY BROWSE-TelleLinje FOR EACH TelleLinje ~
      WHERE TelleLinje.TelleNr = TelleHode.TelleNr and ~
TelleLinje.Butik   = wButik NO-LOCK ~
    BY TelleLinje.TelleNr DESCENDING INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-TelleLinje TelleLinje
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-TelleLinje TelleLinje


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BROWSE-TelleLinje}

/* Definitions for QUERY QUERY-Alle                                     */
&Scoped-define SELF-NAME QUERY-Alle
&Scoped-define QUERY-STRING-QUERY-Alle FOR EACH bTellelinje NO-LOCK WHERE     bTellelinje.TelleNr = TelleHode.TelleNr     BY VgLopNr
&Scoped-define OPEN-QUERY-QUERY-Alle OPEN QUERY {&SELF-NAME} FOR EACH bTellelinje NO-LOCK WHERE     bTellelinje.TelleNr = TelleHode.TelleNr     BY VgLopNr.
&Scoped-define TABLES-IN-QUERY-QUERY-Alle bTellelinje
&Scoped-define FIRST-TABLE-IN-QUERY-QUERY-Alle bTellelinje


/* Definitions for QUERY QUERY-tmpx                                     */
&Scoped-define SELF-NAME QUERY-tmpx
&Scoped-define QUERY-STRING-QUERY-tmpx FOR EACH tmpTellelinje NO-LOCK where     tmpTellelinje.TelleNr = TelleHode.TelleNr     BY tmpTelleLinje.VgLopNr
&Scoped-define OPEN-QUERY-QUERY-tmpx OPEN QUERY {&SELF-NAME} FOR EACH tmpTellelinje NO-LOCK where     tmpTellelinje.TelleNr = TelleHode.TelleNr     BY tmpTelleLinje.VgLopNr.
&Scoped-define TABLES-IN-QUERY-QUERY-tmpx tmpTellelinje
&Scoped-define FIRST-TABLE-IN-QUERY-QUERY-tmpx tmpTellelinje


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-TelleLinje B-Filter B-Etiketter ~
B-Artikkelkort B-Excel B-SkrivUt BUTTON-Ny BUTTON-Slett B-Suppler B-HTFil-2 ~
Btn_Help-2 Btn_OK T-Avangsert FILL-IN-SOK-DECI FILL-IN-SOK-DATE ~
FILL-IN-SOK-INTE FILL-IN-SOK-CHAR BUTTON-Sok BUTTON-3 BUTTON-1 B-HentLister ~
RECT-1 RECT-2 RECT-3 RECT-63 
&Scoped-Define DISPLAYED-OBJECTS CB-Butikk T-Avangsert FILL-IN-SOK-DECI ~
FILL-IN-SOK-DATE FILL-IN-SOK-INTE FILL-IN-SOK-CHAR 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FiksStorl C-Win 
FUNCTION FiksStorl RETURNS CHARACTER
  ( INPUT wStorl AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fLockVindu C-Win 
FUNCTION fLockVindu RETURNS CHARACTER
  ( INPUT lLock AS LOGICAL )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-qry C-Win 
FUNCTION get-qry RETURNS CHARACTER
  ( INPUT cType AS CHARACTER )  FORWARD.

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

/* Menu Definitions                                                     */
DEFINE MENU POPUP-MENU-B-Filter 
       MENU-ITEM m_Nullstill    LABEL "Nullstill filter".


/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE ProgressBar AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chProgressBar AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-1 
     LABEL "&Antall talt = 0" 
     SIZE 23 BY 1 TOOLTIP "Setter antall talt til 0 for alle merkede poster.".

DEFINE BUTTON B-2 
     LABEL "A&ntall talt = AntallPar" 
     SIZE 23 BY 1 TOOLTIP "Setter antall talt lik antall par på alle merkede poster.".

DEFINE BUTTON B-3 
     LABEL "&Ta bort linje(er)" 
     SIZE 23 BY 1 TOOLTIP "Tar bort alle merkede poster.".

DEFINE BUTTON B-Artikkelkort 
     IMAGE-UP FILE "icon/e-detail.bmp":U NO-FOCUS
     LABEL "Artikkelkort..." 
     SIZE 4.6 BY 1.1 TOOLTIP "Artikkelkort".

DEFINE BUTTON B-bygg 
     LABEL "&Bygg liste..." 
     SIZE 14 BY 1.1 TOOLTIP "Starter program for generering av tellelinjer.".

DEFINE BUTTON B-Etiketter 
     IMAGE-UP FILE "icon/ean13.jpg":U NO-FOCUS
     LABEL "Etiketter..." 
     SIZE 4.6 BY 1.1 TOOLTIP "Etiketter".

DEFINE BUTTON B-Excel 
     IMAGE-UP FILE "icon/excel":U NO-FOCUS
     LABEL "Excel..." 
     SIZE 4.6 BY 1.1 TOOLTIP "Eksporter alle eller merkede tellelinjer til Excel. Alt-X.".

DEFINE BUTTON B-Fil 
     LABEL "Les inn &fil..." 
     SIZE 14 BY 1.1 TOOLTIP "Starter program for import av telling fra håndterminal.".

DEFINE BUTTON B-Filter 
     IMAGE-UP FILE "jukebox\img\gif\filteru.gif":U NO-FOCUS
     LABEL "Artikkelkort..." 
     SIZE 4.6 BY 1.1 TOOLTIP "Filter".

DEFINE BUTTON B-HentLister 
     LABEL "Hent lokasjonsliste..." 
     SIZE 28 BY 1.

DEFINE BUTTON B-HTFil 
     LABEL "&HT-eksport..." 
     SIZE 14 BY 1.1 TOOLTIP "Telleliste til håndterminal".

DEFINE BUTTON B-HTFil-2 
     LABEL "&HT-setup..." 
     SIZE 14 BY 1.1 TOOLTIP "Telleliste til håndterminal".

DEFINE BUTTON B-Negative 
     LABEL "Ta bort negative linjer" 
     SIZE 23 BY 1.

DEFINE BUTTON B-Oppdat 
     LABEL "&Oppdater..." 
     SIZE 14 BY 1.1 TOOLTIP "Skaper lagertransaksjoner fra tellelisten og merker den som oppdatert.".

DEFINE BUTTON B-Positive 
     LABEL "Ta bort positive linjer" 
     SIZE 23 BY 1.

DEFINE BUTTON B-Pris 
     LABEL "Endre pris" 
     SIZE 23 BY 1.

DEFINE BUTTON B-Rabatt 
     LABEL "Angi rabatt" 
     SIZE 23 BY 1.

DEFINE BUTTON B-SkrivUt 
     IMAGE-UP FILE "icon/e-print":U NO-FOCUS
     LABEL "Button 32" 
     SIZE 4.6 BY 1.1 TOOLTIP "Skriv ut alle eller merkede tellelinjer. Alt-P.".

DEFINE BUTTON B-Suppler  NO-FOCUS
     LABEL "Hent ikke talte str." 
     SIZE 22 BY 1.1.

DEFINE BUTTON B-Tomme 
     LABEL "Ta bort tomme linjer" 
     SIZE 23 BY 1.

DEFINE BUTTON Btn_Help-2 DEFAULT 
     IMAGE-UP FILE "icon/e-help":U
     LABEL "&Help" 
     SIZE 4.4 BY 1.1 TOOLTIP "Hjelp"
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     IMAGE-UP FILE "icon/e-exit":U
     LABEL "OK" 
     SIZE 4.4 BY 1.1 TOOLTIP "Avslutt"
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-1 
     LABEL "Stre&kkode registrering" 
     SIZE 28 BY 1.

DEFINE BUTTON BUTTON-3 
     LABEL "&Registrer antall" 
     SIZE 28 BY 1.

DEFINE BUTTON BUTTON-Ny 
     IMAGE-UP FILE "icon/e-ny":U NO-FOCUS
     LABEL "&Ny..." 
     SIZE 4.4 BY 1.1 TOOLTIP "Legge til nye tellelinjer. Alt-N".

DEFINE BUTTON BUTTON-Slett 
     IMAGE-UP FILE "icon/e-del":U NO-FOCUS
     LABEL "&Slett" 
     SIZE 4.4 BY 1.1 TOOLTIP "Ta bort merket tellelinje. Alt-D.".

DEFINE BUTTON BUTTON-Sok 
     LABEL "Søk" 
     SIZE 10.2 BY 1 TOOLTIP "Søk i AKTIV kollonne".

DEFINE VARIABLE CB-Butikk AS CHARACTER FORMAT "X(256)":U 
     LABEL "Butikk" 
     VIEW-AS COMBO-BOX INNER-LINES 20
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-SOK-CHAR AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-SOK-DATE AS DATE FORMAT "99-99-99":U 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-SOK-DECI AS DECIMAL FORMAT ">>>>>>>>>>>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-SOK-INTE AS INTEGER FORMAT ">>>>>>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 205 BY .1.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 205 BY .1.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 143 BY 3.57.

DEFINE RECTANGLE RECT-63
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 31 BY 3.57.

DEFINE VARIABLE T-Avangsert AS LOGICAL INITIAL no 
     LABEL "Avansert" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .81 NO-UNDO.

DEFINE BUTTON B-FilterOK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14.

DEFINE BUTTON B-LevNrBlank  NO-FOCUS
     LABEL "Blank" 
     SIZE 7 BY 1.

DEFINE BUTTON B-SokLevNr  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-VaretekstBlank  NO-FOCUS
     LABEL "Blank" 
     SIZE 7 BY 1.

DEFINE VARIABLE FI-Levnavn AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 31.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-LevNr AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     LABEL "Leverandør" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Varetekst AS CHARACTER FORMAT "X(256)":U 
     LABEL "Varetekst" 
     VIEW-AS FILL-IN 
     SIZE 50.2 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-TelleLinje FOR 
      TelleLinje SCROLLING.

DEFINE QUERY QUERY-Alle FOR 
      bTellelinje SCROLLING.

DEFINE QUERY QUERY-tmpx FOR 
      tmpTellelinje SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-TelleLinje
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-TelleLinje C-Win _STRUCTURED
  QUERY BROWSE-TelleLinje NO-LOCK DISPLAY
      TelleLinje.VgLopNr COLUMN-LABEL "Vg/LpNr" FORMAT "X(16)":U
            WIDTH 14.2
      TelleLinje.ArtikkelNr FORMAT "zzzzzzzzzzzz9":U
      TelleLinje.Beskr COLUMN-LABEL "Varetekst" FORMAT "x(20)":U
      TelleLinje.Storl FORMAT "x(10)":U
      TelleLinje.Kode FORMAT "X(20)":U
      TelleLinje.LevKod FORMAT "x(20)":U
      TelleLinje.LevFargKod COLUMN-LABEL "Farve" FORMAT "X(15)":U
      TelleLinje.AntallPar COLUMN-LABEL "Lager" FORMAT "-z,zzz,zz9":U
            WIDTH 7
      TelleLinje.AntallTalt COLUMN-LABEL "Talt" FORMAT "-z,zzz,zz9":U
            WIDTH 9
      TelleLinje.Nedskrevet COLUMN-LABEL "Nedskr." FORMAT "-z,zzz,zz9.99":U
            WIDTH 11.2
      TelleLinje.VVareKost FORMAT "->>,>>9.99":U
      TelleLinje.AntallDiff FORMAT "-z,zzz,zz9":U WIDTH 9
      TelleLinje.VerdiDiff FORMAT "-zz,zzz,zz9.99":U WIDTH 12.8
      lPris COLUMN-LABEL "Pris"
      TelleLinje.RabKr FORMAT "->,>>>,>>9.99":U WIDTH 11
      TelleLinje.Merknad FORMAT "X(30)":U WIDTH 37.2
      lTilbPris COLUMN-LABEL "Tilb.pris"
      TelleLinje.Oppdatert COLUMN-LABEL "Oppd" FORMAT "Ja/Nei":U
      TelleLinje.OpprVerdi FORMAT "-z,zzz,zz9.99":U
      TelleLinje.OpptVerdi FORMAT "->>,>>>,>>9.99":U
      TelleLinje.EDato FORMAT "99/99/9999":U
      TelleLinje.BrukerID FORMAT "X(10)":U
      TelleLinje.Farg FORMAT "zzzz9":U WIDTH 7
      TelleLinje.LevNr FORMAT "zzzzz9":U
      TelleLinje.MatKod FORMAT "z9":U
      TelleLinje.Sasong FORMAT "zz9":U
      TelleLinje.Vg FORMAT "zzzzz9":U
      TelleLinje.SeqNr FORMAT "->>>>9":U
  ENABLE
      TelleLinje.VgLopNr
      TelleLinje.ArtikkelNr
      TelleLinje.Beskr
      TelleLinje.Storl
      TelleLinje.Kode
      TelleLinje.LevKod
      TelleLinje.AntallPar
      TelleLinje.AntallTalt
      TelleLinje.Nedskrevet
      TelleLinje.VVareKost
      TelleLinje.AntallDiff
      TelleLinje.VerdiDiff
      TelleLinje.Merknad
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS MULTIPLE SIZE 205 BY 21.19 ROW-HEIGHT-CHARS .63.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BROWSE-TelleLinje AT ROW 6.48 COL 1
     B-Filter AT ROW 1.43 COL 29.4
     B-Etiketter AT ROW 1.43 COL 23.8
     B-Artikkelkort AT ROW 1.43 COL 19.2
     B-Excel AT ROW 1.43 COL 14.8
     B-SkrivUt AT ROW 1.43 COL 10.4
     BUTTON-Ny AT ROW 1.43 COL 2
     BUTTON-Slett AT ROW 1.43 COL 6.2
     B-Suppler AT ROW 4.95 COL 2.4
     CB-Butikk AT ROW 1.48 COL 39.8 COLON-ALIGNED
     B-bygg AT ROW 1.43 COL 78.8
     B-Fil AT ROW 1.43 COL 93
     B-Oppdat AT ROW 1.43 COL 107.2
     B-HTFil AT ROW 1.43 COL 121.4
     B-HTFil-2 AT ROW 1.43 COL 135.6
     Btn_Help-2 AT ROW 1.43 COL 196.2
     Btn_OK AT ROW 1.43 COL 201
     T-Avangsert AT ROW 2.95 COL 66
     FILL-IN-SOK-DECI AT ROW 3.14 COL 2 NO-LABEL
     FILL-IN-SOK-DATE AT ROW 3.14 COL 2 NO-LABEL
     FILL-IN-SOK-INTE AT ROW 3.14 COL 2 NO-LABEL
     FILL-IN-SOK-CHAR AT ROW 3.14 COL 2 NO-LABEL
     BUTTON-Sok AT ROW 3.19 COL 21.4
     B-Tomme AT ROW 4.1 COL 111.4
     B-Positive AT ROW 4.1 COL 134.4
     B-Negative AT ROW 5.05 COL 134.4
     BUTTON-3 AT ROW 5.05 COL 33.6
     B-Rabatt AT ROW 4.1 COL 65.4 NO-TAB-STOP 
     BUTTON-1 AT ROW 4.1 COL 33.6
     B-1 AT ROW 4.1 COL 88.4 NO-TAB-STOP 
     B-Pris AT ROW 5.05 COL 65.4 NO-TAB-STOP 
     B-2 AT ROW 5.05 COL 88.4 NO-TAB-STOP 
     B-3 AT ROW 5.05 COL 111.4 NO-TAB-STOP 
     B-HentLister AT ROW 3.1 COL 33.6
     RECT-1 AT ROW 1.1 COL 1
     RECT-2 AT ROW 2.67 COL 1
     RECT-3 AT ROW 2.81 COL 63
     RECT-63 AT ROW 2.76 COL 32
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 205.8 BY 26.86.

DEFINE FRAME F-Filter
     B-LevNrBlank AT ROW 3.86 COL 72.4 NO-TAB-STOP 
     FI-Varetekst AT ROW 2.67 COL 17.8 COLON-ALIGNED
     FI-LevNr AT ROW 3.86 COL 17.8 COLON-ALIGNED
     FI-Levnavn AT ROW 3.86 COL 37.4 COLON-ALIGNED NO-LABEL
     B-FilterOK AT ROW 6.24 COL 65
     B-VaretekstBlank AT ROW 2.71 COL 72 NO-TAB-STOP 
     B-SokLevNr AT ROW 3.91 COL 34.4 NO-TAB-STOP 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 33 ROW 10.29
         SIZE 83 BY 8.33
         TITLE "Filter".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: TT_OvBuffer T "NEW SHARED" NO-UNDO skotex OvBuffer
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Tellelinjer"
         HEIGHT             = 26.86
         WIDTH              = 205.8
         MAX-HEIGHT         = 39.19
         MAX-WIDTH          = 273.2
         VIRTUAL-HEIGHT     = 39.19
         VIRTUAL-WIDTH      = 273.2
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
/* REPARENT FRAME */
ASSIGN FRAME F-Filter:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME Custom                                                    */
/* BROWSE-TAB BROWSE-TelleLinje 1 DEFAULT-FRAME */
ASSIGN 
       FRAME DEFAULT-FRAME:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON B-1 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON B-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON B-3 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON B-bygg IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       B-bygg:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON B-Fil IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       B-Filter:POPUP-MENU IN FRAME DEFAULT-FRAME       = MENU POPUP-MENU-B-Filter:HANDLE.

/* SETTINGS FOR BUTTON B-HTFil IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       B-HTFil:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON B-Negative IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON B-Oppdat IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON B-Positive IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON B-Pris IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       B-Pris:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON B-Rabatt IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       B-Rabatt:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON B-Tomme IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BROWSE-TelleLinje:NUM-LOCKED-COLUMNS IN FRAME DEFAULT-FRAME     = 4.

/* SETTINGS FOR COMBO-BOX CB-Butikk IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-SOK-CHAR IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-SOK-DATE IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-SOK-DECI IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-SOK-INTE IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR FRAME F-Filter
                                                                        */
ASSIGN 
       FRAME F-Filter:SENSITIVE        = FALSE.

/* SETTINGS FOR FILL-IN FI-Levnavn IN FRAME F-Filter
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-TelleLinje
/* Query rebuild information for BROWSE BROWSE-TelleLinje
     _TblList          = "skotex.TelleLinje"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "skotex.TelleLinje.TelleNr|no"
     _Where[1]         = "TelleLinje.TelleNr = TelleHode.TelleNr and
TelleLinje.Butik   = wButik"
     _FldNameList[1]   > skotex.TelleLinje.VgLopNr
"TelleLinje.VgLopNr" "Vg/LpNr" "X(16)" "character" ? ? ? ? ? ? yes ? no no "14.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > skotex.TelleLinje.ArtikkelNr
"TelleLinje.ArtikkelNr" ? ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > skotex.TelleLinje.Beskr
"TelleLinje.Beskr" "Varetekst" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > skotex.TelleLinje.Storl
"TelleLinje.Storl" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > skotex.TelleLinje.Kode
"TelleLinje.Kode" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > skotex.TelleLinje.LevKod
"TelleLinje.LevKod" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > skotex.TelleLinje.LevFargKod
"TelleLinje.LevFargKod" "Farve" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > skotex.TelleLinje.AntallPar
"TelleLinje.AntallPar" "Lager" ? "decimal" ? ? ? ? ? ? yes ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > skotex.TelleLinje.AntallTalt
"TelleLinje.AntallTalt" "Talt" ? "decimal" ? ? ? ? ? ? yes ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > skotex.TelleLinje.Nedskrevet
"TelleLinje.Nedskrevet" "Nedskr." ? "decimal" ? ? ? ? ? ? yes ? no no "11.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > skotex.TelleLinje.VVareKost
"TelleLinje.VVareKost" ? ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > skotex.TelleLinje.AntallDiff
"TelleLinje.AntallDiff" ? ? "decimal" ? ? ? ? ? ? yes ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > skotex.TelleLinje.VerdiDiff
"TelleLinje.VerdiDiff" ? ? "decimal" ? ? ? ? ? ? yes ? no no "12.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > "_<CALC>"
"lPris" "Pris" ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > skotex.TelleLinje.RabKr
"TelleLinje.RabKr" ? ? "decimal" ? ? ? ? ? ? no ? no no "11" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > skotex.TelleLinje.Merknad
"TelleLinje.Merknad" ? ? "character" ? ? ? ? ? ? yes ? no no "37.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > "_<CALC>"
"lTilbPris" "Tilb.pris" ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > skotex.TelleLinje.Oppdatert
"TelleLinje.Oppdatert" "Oppd" "Ja/Nei" "logical" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   = skotex.TelleLinje.OpprVerdi
     _FldNameList[20]   = skotex.TelleLinje.OpptVerdi
     _FldNameList[21]   = skotex.TelleLinje.EDato
     _FldNameList[22]   = skotex.TelleLinje.BrukerID
     _FldNameList[23]   > skotex.TelleLinje.Farg
"TelleLinje.Farg" ? "zzzz9" "integer" ? ? ? ? ? ? no ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[24]   = skotex.TelleLinje.LevNr
     _FldNameList[25]   = skotex.TelleLinje.MatKod
     _FldNameList[26]   = skotex.TelleLinje.Sasong
     _FldNameList[27]   = skotex.TelleLinje.Vg
     _FldNameList[28]   = skotex.TelleLinje.SeqNr
     _Query            is OPENED
*/  /* BROWSE BROWSE-TelleLinje */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK QUERY QUERY-Alle
/* Query rebuild information for QUERY QUERY-Alle
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH bTellelinje NO-LOCK WHERE
    bTellelinje.TelleNr = TelleHode.TelleNr
    BY VgLopNr.
     _END_FREEFORM
     _Design-Parent    is FRAME DEFAULT-FRAME @ ( 4.33 , 25 )
*/  /* QUERY QUERY-Alle */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK QUERY QUERY-tmpx
/* Query rebuild information for QUERY QUERY-tmpx
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tmpTellelinje NO-LOCK where
    tmpTellelinje.TelleNr = TelleHode.TelleNr
    BY tmpTelleLinje.VgLopNr.
     _END_FREEFORM
     _Design-Parent    is FRAME DEFAULT-FRAME @ ( 5.52 , 25 )
*/  /* QUERY QUERY-tmpx */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME ProgressBar ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 2.91
       COLUMN          = 89
       HEIGHT          = .95
       WIDTH           = 116
       HIDDEN          = no
       SENSITIVE       = yes.
/* ProgressBar OCXINFO:CREATE-CONTROL from: {35053A22-8589-11D1-B16A-00C0F0283628} type: ProgressBar */
      ProgressBar:MOVE-AFTER(B-bygg:HANDLE IN FRAME DEFAULT-FRAME).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Tellelinjer */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  /*IF THIS-PROCEDURE:PERSISTENT THEN*/ RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Tellelinjer */
DO:
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
                       
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* Tellelinjer */
DO:
    IF {&WINDOW-NAME}:WIDTH-PIXELS < iWidthPix THEN
            {&WINDOW-NAME}:WIDTH-PIXELS = iWidthPix.
    DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"Resize","").  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-1 C-Win
ON CHOOSE OF B-1 IN FRAME DEFAULT-FRAME /* Antall talt = 0 */
DO:
  DEF VAR wSvar AS LOG NO-UNDO.
  DEF VAR wLoop AS INT NO-UNDO.
  
  wSvar = FALSE.
  /* Bruker har valgt en eller flere linjer. */
  IF BROWSE-TelleLinje:NUM-SELECTED-ROWS > 0 THEN
  DO:  
    MESSAGE "Skal antall OPPTALT nullstilles for de valgte radene?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Bekreft nullstilling"
            UPDATE wSvar.
    IF wSvar = FALSE THEN 
      RETURN NO-APPLY.  

    {sww.i}
    DO wLoop = 1 TO BROWSE-TelleLinje:NUM-SELECTED-ROWS TRANSACTION:
      wOk = BROWSE-TelleLinje:FETCH-SELECTED-ROW(wLoop).
      FIND CURRENT TelleLinje EXCLUSIVE-LOCK.
      
      /* Setter antall på lager inn i antall talt. */
      ASSIGN
        TelleLinje.AntallTalt = 0
        TelleLinje.AntallDiff = TelleLinje.AntallPar - TelleLinje.AntallTalt 
        TelleLinje.OpptVerdi  = TelleLinje.AntallTalt * TelleLinje.VVareKost
        TelleLinje.VerdiDiff  = TelleLinje.AntallDiff * TelleLinje.VVareKost.
            
      FIND CURRENT TelleLinje NO-LOCK.
    END. /* TRANSACTION */
    wOk = BROWSE-TelleLinje:REFRESH( ).
    {swn.i}
  END. 
  
  /* Ingen linjer er valgt. Alle linjer skal behandles. Bruker må bekrefte */
  ELSE DO:
    MESSAGE "Skal antall OPPTALT nullstilles for ALLE radene i denne butikkens liste?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Bekreft nullstilling"
            UPDATE wSvar.
    IF wSvar = FALSE THEN 
      RETURN NO-APPLY.  
    {sww.i}
    DO:
      FOR EACH TelleLinje EXCLUSIVE-LOCK WHERE
        TelleLinje.TelleNr = TelleHode.TelleNr AND
        TelleLinje.Butik   = wButik:
        /* Setter antall på lager inn i antall talt. */
        ASSIGN
          TelleLinje.AntallTalt = 0
          TelleLinje.AntallDiff = TelleLinje.AntallPar - TelleLinje.AntallTalt
          TelleLinje.OpptVerdi  = TelleLinje.AntallTalt * TelleLinje.VVareKost
          TelleLinje.VerdiDiff  = TelleLinje.AntallDiff * TelleLinje.VVareKost.        
      END.  
      wOk = BROWSE-TelleLinje:REFRESH( ).
    END.
    {swn.i}
  END. 
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-2 C-Win
ON CHOOSE OF B-2 IN FRAME DEFAULT-FRAME /* Antall talt = AntallPar */
DO:
  DEF VAR wSvar AS LOG NO-UNDO.
  DEF VAR wLoop AS INT NO-UNDO.
  
  wSvar = FALSE.
  
  /* Tar de valgte radene */
  IF BROWSE-TelleLinje:NUM-SELECTED-ROWS > 0 THEN
  DO:  
    MESSAGE "Skal antall OPPTALT settes lik antall PÅ LAGER for de valgte radene?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Bekreft"
            UPDATE wSvar.
    IF wSvar = FALSE THEN 
      RETURN NO-APPLY.

    {sww.i}
    DO wLoop = 1 TO BROWSE-TelleLinje:NUM-SELECTED-ROWS TRANSACTION:
      wOk = BROWSE-TelleLinje:FETCH-SELECTED-ROW(wLoop).
      FIND CURRENT TelleLinje EXCLUSIVE-LOCK.
      
      /* Setter antall på lager inn i antall talt. */
      ASSIGN
        TelleLinje.AntallTalt = TelleLinje.AntallPar
        TelleLinje.AntallDiff = TelleLinje.AntallPar - TelleLinje.AntallTalt
        TelleLinje.OpptVerdi  = TelleLinje.AntallTalt * TelleLinje.VVareKost
        TelleLinje.VerdiDiff  = TelleLinje.AntallDiff * TelleLinje.VVareKost.
            
      FIND CURRENT TelleLinje NO-LOCK.
    END.
    wOk = BROWSE-TelleLinje:REFRESH( ).
    {swn.i}
  END.
  
  /* tar alle radene. Bruker må bekrefte */
  ELSE DO:
    MESSAGE "Skal antall OPPTALT settes lik antall PÅ LAGER for ALLE radene i butikkens liste?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Bekreft"
            UPDATE wSvar.
    IF wSvar = FALSE THEN 
      RETURN NO-APPLY.

    {sww.i}
    DO:
      FOR EACH TelleLinje EXCLUSIVE-LOCK WHERE
        TelleLinje.TelleNr = TelleHode.TelleNr AND
        TelleLinje.Butik   = wButik:
        /* Setter antall på lager inn i antall talt. */
        ASSIGN
          TelleLinje.AntallTalt = TelleLinje.AntallPar
          TelleLinje.AntallDiff = TelleLinje.AntallPar - TelleLinje.AntallTalt
          TelleLinje.OpptVerdi  = TelleLinje.AntallTalt * TelleLinje.VVareKost
          TelleLinje.VerdiDiff  = TelleLinje.AntallDiff * TelleLinje.VVareKost.
      END.  
      wOk = BROWSE-TelleLinje:REFRESH( ).
    END. 
    {swn.i}
  END.
          
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-3 C-Win
ON CHOOSE OF B-3 IN FRAME DEFAULT-FRAME /* Ta bort linje(er) */
DO:
  DEF VAR wSvar AS LOG NO-UNDO.
  DEF VAR wLoop AS INT NO-UNDO.
  
  IF BROWSE-TelleLinje:NUM-SELECTED-ROWS > 0 THEN
    DO:
      wSvar = FALSE.
      MESSAGE "Skal de merkede radene tas bort?"
              VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Bekreft sletting"
              UPDATE wSvar.
      IF wSvar = FALSE THEN 
        RETURN NO-APPLY.
  
      {sww.i}
      SLETTE_LOOP:
      DO wLoop = BROWSE-TelleLinje:NUM-SELECTED-ROWS TO 1 BY -1 TRANSACTION:
        wOk = BROWSE-TelleLinje:FETCH-SELECTED-ROW(wLoop).

        /* Sender posten til den siste hvile. */
        IF AVAILABLE TelleLinje THEN
          RUN SlettLinje (INPUT 0). 

      END. /* SLETTE_LOOP */  
      {swn.i}
  
      wOk = BROWSE-TelleLinje:DELETE-SELECTED-ROWS( ).
    END.
  /* Tar bort alle rader i listen. */
  ELSE DO: 
    MESSAGE "Skal ALLE radene i butikkens liste tas bort?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Bekreft sletting"
            UPDATE wSvar.
    IF wSvar = FALSE THEN 
      RETURN NO-APPLY.
    
    {sww.i}
    DO:
      FOR EACH TelleLinje EXCLUSIVE-LOCK WHERE
        TelleLinje.TelleNr = TelleHode.TelleNr AND
        TelleLinje.Butik   = wButik:

        /* Sender posten til den siste hvile. */
        RUN SlettLinje (INPUT 0). 
      END.  
      wOk = BROWSE-TelleLinje:REFRESH( ).
    END.  
    {swn.i}
  END.
    
  RUN SettButtons.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Artikkelkort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Artikkelkort C-Win
ON CHOOSE OF B-Artikkelkort IN FRAME DEFAULT-FRAME /* Artikkelkort... */
DO:
  RUN Artikkelkort.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-bygg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-bygg C-Win
ON CHOOSE OF B-bygg IN FRAME DEFAULT-FRAME /* Bygg liste... */
DO:
  IF NOT AVAILABLE TelleHode THEN
    RETURN NO-APPLY.
  RUN d-byggtelleliste.w (INPUT RECID(TelleHode)).
  {sww.i}
  RUN OppdatTelleHode (INPUT ?).
  {&OPEN-QUERY-{&BROWSE-NAME}}
  {swn.i}
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Etiketter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Etiketter C-Win
ON CHOOSE OF B-Etiketter IN FRAME DEFAULT-FRAME /* Etiketter... */
DO:
     DEFINE VARIABLE iLoop  AS INTEGER    NO-UNDO.
    DEFINE VARIABLE lOk    AS LOGICAL    NO-UNDO.
    DEFINE VARIABLE iRetur AS INTEGER    NO-UNDO.
    IF NOT CAN-FIND(FIRST TelleLinje OF TelleHode) THEN
        RETURN NO-APPLY.
/*     IF BROWSE-TelleLinje:NUM-SELECTED-ROWS > 0 THEN DO: */
/*         MESSAGE "Etiketter for alle eller"              */
/*             VIEW-AS ALERT-BOX INFO BUTTONS OK.          */
/*     END.                                                */
    RUN JBoxBrowseSelectMsg.w ("Utskriftsalternativ",BROWSE-TelleLinje:NUM-SELECTED-ROWS,?,OUTPUT iRetur).
    IF iRetur = 0 THEN
        RETURN NO-APPLY.
    IF iRetur = 2 THEN DO:
        DO iLoop = 1 TO BROWSE-TelleLinje:NUM-SELECTED-ROWS:
            BROWSE-TelleLinje:FETCH-SELECTED-ROW(iLoop).
            IF TelleLinje.antallTalt > 0 THEN DO:
                lOK = TRUE.
                LEAVE.
            END.
        END.
        IF NOT lOK THEN DO:
            MESSAGE "Finnes ingen valgt post med antall talt > 0"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RETURN NO-APPLY.
        END.
    END.
    ELSE DO:
        IF NOT CAN-FIND(FIRST TelleLinje OF TelleHode WHERE TelleLinje.antallTalt > 0) THEN DO:
            MESSAGE "Finnes ingen post med antall talt > 0"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RETURN NO-APPLY.
        END.
    END.
    RUN Etiketter(iRetur).
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Excel C-Win
ON CHOOSE OF B-Excel IN FRAME DEFAULT-FRAME /* Excel... */
DO:
  RUN ExportToExcel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Fil
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Fil C-Win
ON CHOOSE OF B-Fil IN FRAME DEFAULT-FRAME /* Les inn fil... */
DO:
  DEF VAR wAntTyper AS INT NO-UNDO.
  DEF VAR wTypeId   AS INT NO-UNDO.

  IF NOT AVAILABLE TelleHode THEN
    RETURN.   
    
  IF wNedskriv THEN
    DO:
      MESSAGE "Import fra håndterminal kan ikke gjøres mot tellinger av type NEDSKRIVING."
              VIEW-AS ALERT-BOX MESSAGE TITLE "Melding".
      RETURN NO-APPLY.
    END.
  
  IF TelleHode.ButikkListe = "" THEN
    DO:
      MESSAGE "Det er ikke satt opp butikker på tellelisten!"
              VIEW-AS ALERT-BOX MESSAGE TITLE "Melding".
      RETURN NO-APPLY.
    END.
  
  FIND FIRST HT-Type NO-LOCK NO-ERROR.
  IF NOT AVAILABLE HT-Type THEN
    DO:
      MESSAGE "Det er ikke satt opp noen håndterminalsdefinisjoner!"
              VIEW-AS ALERT-BOX ERROR TITLE "Feil".
      RETURN NO-APPLY.
    END.
  ELSE
    wTypeId = HT-Type.TypeId.
    
  fLockvindu(TRUE).
  RUN w-bht-filhode.w (INPUT TelleHode.TelleNr, INPUT wTypeId, 0).
  fLockvindu(FALSE).

  /*
  if return-value = "AVBRYT" then
    return no-apply.
  */
  {sww.i}
  RUN OppdatTelleHode (INPUT ?).
  {&OPEN-QUERY-{&BROWSE-NAME}}
  {swn.i}
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Filter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Filter C-Win
ON CHOOSE OF B-Filter IN FRAME DEFAULT-FRAME /* Artikkelkort... */
DO:
/*   FRAME DEFAULT-FRAME:SENSITIVE = FALSE. */
  FRAME F-Filter:SENSITIVE = TRUE.
  FRAME F-Filter:MOVE-TO-TOP().
  APPLY "ENTRY" TO FI-Varetekst IN FRAME F-Filter.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Filter
&Scoped-define SELF-NAME B-FilterOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-FilterOK C-Win
ON CHOOSE OF B-FilterOK IN FRAME F-Filter /* OK */
DO:
    DEFINE VARIABLE cGif AS CHARACTER  NO-UNDO.
    FRAME DEFAULT-FRAME:SENSITIVE = TRUE.
    FRAME F-Filter:MOVE-TO-BOTTOM().
    FRAME F-Filter:SENSITIVE = FALSE.
    cGif = B-Filter:IMAGE-UP.
    IF INPUT FI-Varetekst <> "" OR INPUT FI-LevNr <> 0 THEN
        cGif = REPLACE(cGif,"filteru","filterc").
    ELSE
        cGif = REPLACE(cGif,"filterc","filteru").
    B-Filter:LOAD-IMAGE-UP(cGif).
    APPLY "ENTRY" TO FRAME DEFAULT-FRAME.
    RUN SD-Query-Open.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME B-HentLister
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-HentLister C-Win
ON CHOOSE OF B-HentLister IN FRAME DEFAULT-FRAME /* Hent lokasjonsliste... */
DO:
    wOk = FALSE.
    THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
    RUN JBoxSelector.w (THIS-PROCEDURE,0,
                        "Tellehode;TelleNr;Beskrivelse;TTId;Oppdatert;ButikkListe",
                        "where TelleHode.TTId = 9 and TelleHode.Oppdatert = ? and TelleHode.TelleNr <> " + string(TelleHode.TelleNr) +
                          " and TelleHode.ButikkListe = '" + trim(ENTRY(1,CB-Butikk:screen-value,":")) + "'",
                        INPUT-OUTPUT cLokasjonRowIdList,
                        "TelleNr",
                        INPUT-OUTPUT cLokasjonIdList,
                        "","",
                        OUTPUT bOK).
    THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

    IF cLokasjonIdList <> "" THEN
    LOKASJON:
    DO:
        MESSAGE 
        "Skal tellelistene " + cLokasjonIdList + " oppdateres mot åpen telleliste?"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
            UPDATE wOk.
        IF wOk <> TRUE THEN
            LEAVE LOKASJON.
        fLockvindu(TRUE).
        RUN oppdatlokasjon.p (INPUT TelleHode.TelleNr,
                              INPUT INT(TRIM(ENTRY(1,CB-Butikk:screen-value,":"))),
                              INPUT cLokasjonIdList).
        fLockvindu(FALSE).

        {sww.i}
        RUN OppdatTelleHode (INPUT ?).
        {&OPEN-QUERY-{&BROWSE-NAME}}
        {swn.i}
    END. /* LOKASJON */

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-HTFil
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-HTFil C-Win
ON CHOOSE OF B-HTFil IN FRAME DEFAULT-FRAME /* HT-eksport... */
DO:  
  DEFINE VARIABLE iTypeId AS INTEGER    NO-UNDO.

  MESSAGE "Husk at tellelisten bare inneholder tellelinjer på de størrelser hvor lagerteller <> 0." SKIP
          "Den listen med varianter som nå sendes til håndterminalen, er derfor muliigens ikke komplett." SKIP(1)
          "For å bygge en komplett artikkelfil til håndterminalen, skal dette gjøres via artikkelutvalget." SKIP(1)
          "Ønsker du alikevel å legge ut en artikkelfil basert på registrerte tellelinjer?"
      VIEW-AS ALERT-BOX WARNING BUTTONS YES-NO UPDATE bOk.
  IF bOk <> TRUE THEN
      RETURN.

  IF 1 = 1 OR NOT AVAILABLE TelleHode THEN /* listan är aldrig komplett */
      RETURN NO-APPLY.
  RUN d-velgHT.w (OUTPUT iTypeId).
  IF RETURN-VALUE = "AVBRYT" THEN
      RETURN NO-APPLY.
  {sww.i}
  RUN htvarefilFraTelling.p (TelleHode.TelleNr,iTypeId).
  {swn.i}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-HTFil-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-HTFil-2 C-Win
ON CHOOSE OF B-HTFil-2 IN FRAME DEFAULT-FRAME /* HT-setup... */
DO:
  DEFINE VARIABLE wTypeId LIKE HT-Type.TypeId NO-UNDO.
  
  RUN d-bht-type.w (INPUT-OUTPUT wTypeId).
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Filter
&Scoped-define SELF-NAME B-LevNrBlank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-LevNrBlank C-Win
ON CHOOSE OF B-LevNrBlank IN FRAME F-Filter /* Blank */
DO:
  ASSIGN FI-LevNr:SCREEN-VALUE = ""
         FI-LevNavn:SCREEN-VALUE = "".
  APPLY "TAB" TO FI-LevNr.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME B-Negative
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Negative C-Win
ON CHOOSE OF B-Negative IN FRAME DEFAULT-FRAME /* Ta bort negative linjer */
DO:
  DEF VAR wSvar AS LOG NO-UNDO.
  DEF VAR wLoop AS INT NO-UNDO.
  
  IF BROWSE-TelleLinje:NUM-SELECTED-ROWS > 0 THEN
    DO:
      wSvar = FALSE.
      MESSAGE "Skal de merkede radene som er negative tas bort?"
              VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Bekreft sletting"
              UPDATE wSvar.
      IF wSvar = FALSE THEN 
        RETURN NO-APPLY.
  
      {sww.i}
      SLETTE_LOOP:
      DO wLoop = BROWSE-TelleLinje:NUM-SELECTED-ROWS TO 1 BY -1 TRANSACTION:
        wOk = BROWSE-TelleLinje:FETCH-SELECTED-ROW(wLoop).

        /* Sender posten til den siste hvile. */
        IF AVAILABLE TelleLinje THEN
          DO:
            IF TelleLinje.AntallDiff < 0 THEN
              RUN SlettLinje (INPUT 0). 
          END.

      END. /* SLETTE_LOOP */  
      {swn.i}
  
      /*wOk = BROWSE-TelleLinje:DELETE-SELECTED-ROWS( ).*/
      wOk = BROWSE-TelleLinje:REFRESH( ).
    END.
  /* Tar bort alle rader i listen. */
  ELSE DO: 
    MESSAGE "Skal ALLE de negative radene i butikkens liste tas bort?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Bekreft sletting"
            UPDATE wSvar.
    IF wSvar = FALSE THEN 
      RETURN NO-APPLY.
    
    {sww.i}
    DO:
      FOR EACH TelleLinje EXCLUSIVE-LOCK WHERE
        TelleLinje.TelleNr = TelleHode.TelleNr AND
        TelleLinje.Butik   = wButik:

        /* Sender posten til den siste hvile. */
        IF AVAILABLE TelleLinje THEN
          DO:
            IF TelleLinje.AntallDiff < 0 THEN
              RUN SlettLinje (INPUT 0). 
          END.
      END.  
      wOk = BROWSE-TelleLinje:REFRESH( ).
    END.   
    {swn.i}
  END.
    
  RUN SettButtons.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Oppdat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Oppdat C-Win
ON CHOOSE OF B-Oppdat IN FRAME DEFAULT-FRAME /* Oppdater... */
DO:
  IF NOT AVAILABLE TelleHode THEN
      RETURN NO-APPLY.

  wOk = FALSE.
  MESSAGE "Skal oppdatering av lager og statistikker startes?" SKIP
          "Denne rutinen starter oppdaterer lager og statistikker."
          VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Bekreft oppdatering"
          UPDATE wOk.
  IF wOk THEN
    DO:

      IF TelleHode.AntLinjer >= 5 THEN
      DO:
          ASSIGN
              chProgressBar:Min    = 1
              chProgressBar:Max    = TelleHode.AntLinjer
              chProgressBar:Value  = 1
              .
      END.
      IF TelleHode.TTId = 6 THEN /* Overføringer */
          RUN oppdatOverforing.
      ELSE IF TelleHode.TTID = 1 THEN  /* Varesalg */
          RUN poster_bonger_fra_telling.p (INPUT TelleHode.TelleNr).
      ELSE
          RUN oppdaterTelling.p (TelleHode.TelleNr, THIS-PROCEDURE).

      IF VALID-HANDLE(chProgressBar) THEN
          RELEASE OBJECT chProgressBar NO-ERROR.
      IF VALID-HANDLE(ProgressBar) THEN
          DELETE OBJECT ProgressBar NO-ERROR.
      ASSIGN ProgressBar   = ?
             chProgressBar = ?.

      B-Oppdat:SENSITIVE = FALSE.
      {&OPEN-QUERY-{&BROWSE-NAME}}

      APPLY "choose" TO Btn_Ok.

    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Positive
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Positive C-Win
ON CHOOSE OF B-Positive IN FRAME DEFAULT-FRAME /* Ta bort positive linjer */
DO:
  DEF VAR wSvar AS LOG NO-UNDO.
  DEF VAR wLoop AS INT NO-UNDO.
  
  IF BROWSE-TelleLinje:NUM-SELECTED-ROWS > 0 THEN
    DO:
      wSvar = FALSE.
      MESSAGE "Skal de merkede radene som er positive tas bort?"
              VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Bekreft sletting"
              UPDATE wSvar.
      IF wSvar = FALSE THEN 
        RETURN NO-APPLY.
  
      {sww.i}
      SLETTE_LOOP:
      DO wLoop = BROWSE-TelleLinje:NUM-SELECTED-ROWS TO 1 BY -1 TRANSACTION:
        wOk = BROWSE-TelleLinje:FETCH-SELECTED-ROW(wLoop).

        /* Sender posten til den siste hvile. */
        IF AVAILABLE TelleLinje THEN
          DO:
            IF TelleLinje.AntallDiff > 0 THEN
              RUN SlettLinje (INPUT 0). 
          END.

      END. /* SLETTE_LOOP */  
      {swn.i}
  
      /*wOk = BROWSE-TelleLinje:DELETE-SELECTED-ROWS( ).*/
      wOk = BROWSE-TelleLinje:REFRESH( ).
    END.
  /* Tar bort alle rader i listen. */
  ELSE DO: 
    MESSAGE "Skal ALLE de positive radene i butikkens liste tas bort?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Bekreft sletting"
            UPDATE wSvar.
    IF wSvar = FALSE THEN 
      RETURN NO-APPLY.
    
    {sww.i}
    DO:
      FOR EACH TelleLinje EXCLUSIVE-LOCK WHERE
        TelleLinje.TelleNr = TelleHode.TelleNr AND
        TelleLinje.Butik   = wButik:

        /* Sender posten til den siste hvile. */
        IF AVAILABLE TelleLinje THEN
          DO:
            IF TelleLinje.AntallDiff > 0 THEN
              RUN SlettLinje (INPUT 0). 
          END.
      END.  
      wOk = BROWSE-TelleLinje:REFRESH( ).
    END.  
    {swn.i}
  END.
    
  RUN SettButtons.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Pris
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Pris C-Win
ON CHOOSE OF B-Pris IN FRAME DEFAULT-FRAME /* Endre pris */
DO:
  DEF VAR wLoop   AS INT NO-UNDO.
  DEF VAR wPris   AS DEC NO-UNDO.
  DEF VAR wPrisI% AS LOG INITIAL FALSE NO-UNDO.
  DEF VAR wPris%  AS DEC NO-UNDO.
  
  /* Bruker har valgt en eller flere linjer. */
  IF BROWSE-TelleLinje:NUM-SELECTED-ROWS > 0 THEN
  PRIS1:
  DO  TRANSACTION ON ENDKEY UNDO,LEAVE:  
    RUN d-settpris.w (INPUT-OUTPUT wPris, INPUT-OUTPUT wPrisI%, "Pris settes på alle valgte linjer").

    IF RETURN-VALUE = "AVBRYT" THEN
      LEAVE PRIS1.
    IF wPrisI% THEN
      wPris% = wPris.

    DO wLoop = 1 TO BROWSE-TelleLinje:NUM-SELECTED-ROWS:
      wOk = BROWSE-TelleLinje:FETCH-SELECTED-ROW(wLoop).
      FIND CURRENT TelleLinje EXCLUSIVE-LOCK.
 
      /* Er det prosent, skal prisen regnes ut. */
      IF wPrisI% THEN
        wPris = (TelleLinje.VVareKost * wPris%) / 100.
      
      /* Setter antall på lager inn i antall talt. */
      ASSIGN
        TelleLinje.VVareKost = wPris
        TelleLinje.VerdiDiff = TelleLinje.AntallDiff * TelleLinje.VVareKost
        TelleLinje.OpptVerdi = TelleLinje.AntallTalt * TelleLinje.VVareKost
        .
            
      FIND CURRENT TelleLinje NO-LOCK.
    END.
    wOk = BROWSE-TelleLinje:REFRESH( ).
  END. /* PRIS1 TRANSACTION */
  
  /* Ingen linjer er valgt. Alle linjer skal behandles. Bruker må bekrefte */
  ELSE 
  PRIS2:
  DO ON ENDKEY UNDO,LEAVE:  
    RUN d-settpris.w (INPUT-OUTPUT wPris, INPUT-OUTPUT wPrisI%, "Pris settes på alle linjer i listen").
    IF RETURN-VALUE = "AVBRYT" THEN
      LEAVE PRIS2.
    IF wPrisI% THEN
      wPris% = wPris.
    
    {sww.i}
    DO TRANSACTION:
      FOR EACH TelleLinje EXCLUSIVE-LOCK WHERE
        TelleLinje.TelleNr = TelleHode.TelleNr AND
        TelleLinje.Butik   = wButik:
        
        /* Er det prosent, skal rabatten regnes ut. */
        IF wPrisI% THEN
          wPris = (TelleLinje.VVareKost * wPris%) / 100.
      
        /* Setter antall på lager inn i antall talt. */
        ASSIGN
          TelleLinje.VVareKost = wPris
          TelleLinje.VerdiDiff = TelleLinje.AntallDiff * TelleLinje.VVareKost
          TelleLinje.OpptVerdi = TelleLinje.AntallTalt * TelleLinje.VVareKost
          .
      END.  
      wOk = BROWSE-TelleLinje:REFRESH( ).
    END. /* TRANSACTION */
    {swn.i}
  END. /* PRIS2 */

  HIDE FRAME FRAME-Rabatt NO-PAUSE.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Rabatt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Rabatt C-Win
ON CHOOSE OF B-Rabatt IN FRAME DEFAULT-FRAME /* Angi rabatt */
DO:
  DEF VAR wLoop  AS INT NO-UNDO.
  DEF VAR wRabKr AS DEC NO-UNDO.
  DEF VAR wRabI% AS LOG INITIAL TRUE NO-UNDO.
  DEF VAR wRab%  AS DEC NO-UNDO.
  
  /* Bruker har valgt en eller flere linjer. */
  IF BROWSE-TelleLinje:NUM-SELECTED-ROWS > 0 THEN
  RAB1:
  DO ON ENDKEY UNDO,LEAVE:  
    RUN d-settrabatt.w (INPUT-OUTPUT wRabKr, INPUT-OUTPUT wRabI%, "Rabatt settes på alle valgte linjer").
    IF RETURN-VALUE = "AVBRYT" THEN
      LEAVE RAB1.
    IF wRabI% THEN
      wRab% = wRabKr.

    /*{sww.i}*/
    DO wLoop = 1 TO BROWSE-TelleLinje:NUM-SELECTED-ROWS TRANSACTION:
      wOk = BROWSE-TelleLinje:FETCH-SELECTED-ROW(wLoop).
      FIND CURRENT TelleLinje EXCLUSIVE-LOCK.
 
      /* Er rabatt angitt i prosent, skal rabatten regnes ut. */
      IF wRabI% THEN
        wRabKr = (TelleLinje.VVareKost * wRab%) / 100.
      
      /* Setter rabatten inn på linjen. */
      ASSIGN
        TelleLinje.RabKr     = wRabKr
        TelleLinje.VerdiDiff = TelleLinje.AntallDiff * (TelleLinje.VVareKost - TelleLinje.RabKr)
        TelleLinje.OpptVerdi = TelleLinje.AntallTalt * (TelleLinje.VVareKost - TelleLinje.RabKr)
        .

      FIND CURRENT TelleLinje NO-LOCK.
    END. /* TRANSACTION */
    wOk = BROWSE-TelleLinje:REFRESH( ).
    /*{swn.i} */
  END. /* RAB1 TRANSACTION */
  
  /* Ingen linjer er valgt. Alle linjer skal behandles. Bruker må bekrefte */
  ELSE 
  RAB2:
  DO ON ENDKEY UNDO,LEAVE:  
    RUN d-settrabatt.w (INPUT-OUTPUT wRabKr, INPUT-OUTPUT wRabI%, "Rabatt settes på alle linjer i listen").
    IF RETURN-VALUE = "AVBRYT" THEN
      LEAVE RAB2.
    IF wRabI% THEN
      wRab% = wRabKr.
    
    /*{sww.i}*/
    DO TRANSACTION:
      FOR EACH TelleLinje EXCLUSIVE-LOCK WHERE
        TelleLinje.TelleNr = TelleHode.TelleNr AND
        TelleLinje.Butik   = wButik:
        
        /* Er det prosent, skal rabatten regnes ut. */
        IF wRabI% THEN
          wRabKr = (TelleLinje.VVareKost * wRab%) / 100.
      
        /* Setter antall på lager inn i antall talt. */
        ASSIGN
          TelleLinje.RabKr     = wRabKr
          TelleLinje.VerdiDiff = TelleLinje.AntallDiff * (TelleLinje.VVareKost - TelleLinje.RabKr)
          TelleLinje.OpptVerdi = TelleLinje.AntallTalt * (TelleLinje.VVareKost - TelleLinje.RabKr)
          .
      END.  
      wOk = BROWSE-TelleLinje:REFRESH( ).
    END. /* TRANSACTION */
    /* {swn.i} */
  END. /* RAB2 */

  HIDE FRAME FRAME-Rabatt NO-PAUSE.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SkrivUt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SkrivUt C-Win
ON CHOOSE OF B-SkrivUt IN FRAME DEFAULT-FRAME /* Button 32 */
DO:
  IF NOT AVAILABLE {&br-tabell} THEN
    RETURN NO-APPLY.
  RUN printTellelisteX.p (ROWID(TelleHode),wQ:PREPARE-STRING).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Filter
&Scoped-define SELF-NAME B-SokLevNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SokLevNr C-Win
ON CHOOSE OF B-SokLevNr IN FRAME F-Filter /* ... */
OR F10 OF FI-LevNr
DO:
  DEFINE VARIABLE cTekst AS CHARACTER  NO-UNDO.
  ASSIGN
      cTekst = "".

  /* Kaller søkerutine */
  RUN gLevbas.w (
    INPUT-OUTPUT cTekst, /* Returstreng - chr(1) separert */
    "", /* Feltliste avgrensningsfelt (kommaseparert) */
    "", /* Feltverdier (chr(1) sep) */ 
    FI-LevNr:SCREEN-VALUE /* Post markøren skal stå på */
    ).
  IF RETURN-VALUE = "AVBRYT" THEN
      RETURN NO-APPLY.
  IF NUM-ENTRIES(cTekst,CHR(1)) = 3 THEN
  DO:
      /* Legger opp verdier I de aktuelle feltene */
      ASSIGN
        FI-LevNr:SCREEN-VALUE = ENTRY(2,cTekst,CHR(1))
        FI-LevNavn:SCREEN-VALUE = ENTRY(3,cTekst,CHR(1))
        .
        /* Flagger at det er gjort endringer på recorden og trigger toolbar. */
        APPLY "TAB":U TO FI-LevNr.
        RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME B-Suppler
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Suppler C-Win
ON CHOOSE OF B-Suppler IN FRAME DEFAULT-FRAME /* Hent ikke talte str. */
DO:
/*     MESSAGE "Ved nyregistrering, ønsker du att alle størrelser automatiskt skapes?" */
/*       VIEW-AS ALERT-BOX INFO BUTTONS YES-NO UPDATE lAlle AS LOGICAL.                */
    MESSAGE 
        "Henter alle størrelser for alle artikkler i listen." SKIP
        "Fortsette ?"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lFortsette AS LOGICAL.
    IF lFortsette THEN
        RUN Suppler.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Tomme
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Tomme C-Win
ON CHOOSE OF B-Tomme IN FRAME DEFAULT-FRAME /* Ta bort tomme linjer */
DO:
  DEF VAR wSvar AS LOG NO-UNDO.
  DEF VAR wLoop AS INT NO-UNDO.
  
  IF BROWSE-TelleLinje:NUM-SELECTED-ROWS > 0 THEN
    DO:
      wSvar = FALSE.
      MESSAGE "Skal de merkede radene som er tomme tas bort? (Lager=0,Talt=0)"
              VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Bekreft sletting"
              UPDATE wSvar.
      IF wSvar = FALSE THEN 
        RETURN NO-APPLY.
  
      {sww.i}
      SLETTE_LOOP:
      DO wLoop = BROWSE-TelleLinje:NUM-SELECTED-ROWS TO 1 BY -1 TRANSACTION:
        wOk = BROWSE-TelleLinje:FETCH-SELECTED-ROW(wLoop).

        /* Sender posten til den siste hvile. */
        IF AVAILABLE TelleLinje THEN
          DO:
            IF TelleLinje.AntallTalt = 0 AND TelleLinje.AntallPar = 0 THEN 
              RUN SlettLinje (INPUT 0). 
          END.

      END. /* SLETTE_LOOP */  
      {swn.i}
  
      /*wOk = BROWSE-TelleLinje:DELETE-SELECTED-ROWS( ).*/
      wOk = BROWSE-TelleLinje:REFRESH( ).
    END.
  /* Tar bort alle rader i listen. */
  ELSE DO: 
    MESSAGE "Skal ALLE de tomme radene i butikkens liste tas bort? (Lager=0,Talt=0)"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Bekreft sletting"
            UPDATE wSvar.
    IF wSvar = FALSE THEN 
      RETURN NO-APPLY.

    {sww.i}
    DO:
      FOR EACH TelleLinje EXCLUSIVE-LOCK WHERE
        TelleLinje.TelleNr = TelleHode.TelleNr AND
        TelleLinje.Butik   = wButik:

        /* Sender posten til den siste hvile. */
        IF AVAILABLE TelleLinje THEN
          DO:
            IF TelleLinje.AntallTalt = 0 AND TelleLinje.AntallPar = 0 THEN 
              RUN SlettLinje (INPUT 0). 
          END.
      END.  
      wOk = BROWSE-TelleLinje:REFRESH( ).
    END.   
    {swn.i}
  END.
    
  RUN SettButtons.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Filter
&Scoped-define SELF-NAME B-VaretekstBlank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-VaretekstBlank C-Win
ON CHOOSE OF B-VaretekstBlank IN FRAME F-Filter /* Blank */
DO:
  ASSIGN FI-Varetekst:SCREEN-VALUE = "".
  APPLY "TAB" TO FI-Varetekst.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-TelleLinje
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME BROWSE-TelleLinje
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-TelleLinje C-Win
ON ANY-PRINTABLE OF BROWSE-TelleLinje IN FRAME DEFAULT-FRAME
DO:
    IF LASTKEY <> 32 THEN
      DO:
        RUN SD-ANY-PRINTABLE.
        RETURN NO-APPLY.
      END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-TelleLinje C-Win
ON CURSOR-LEFT OF BROWSE-TelleLinje IN FRAME DEFAULT-FRAME
DO:
    IF wAntSortCol < 2 THEN
      RETURN NO-APPLY.
    RUN SD-CURSOR ("LEFT").
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-TelleLinje C-Win
ON CURSOR-RIGHT OF BROWSE-TelleLinje IN FRAME DEFAULT-FRAME
DO:
    IF wAntSortCol < 2 THEN
      RETURN NO-APPLY.
    RUN SD-CURSOR ("RIGHT").
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-TelleLinje C-Win
ON MOUSE-SELECT-DOWN OF BROWSE-TelleLinje IN FRAME DEFAULT-FRAME
DO:
  ASSIGN
    B-1:sensitive     = FALSE 
    B-2:sensitive     = FALSE 
    B-3:sensitive     = FALSE
    B-Tomme:sensitive = FALSE
    B-Negative:sensitive = FALSE
    B-Positive:sensitive = FALSE.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-TelleLinje C-Win
ON MOUSE-SELECT-UP OF BROWSE-TelleLinje IN FRAME DEFAULT-FRAME
DO:
  RUN SettButtons.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-TelleLinje C-Win
ON ROW-DISPLAY OF BROWSE-TelleLinje IN FRAME DEFAULT-FRAME
DO:
  IF AVAILABLE TelleLinje THEN
    DO:
      FIND ArtBas NO-LOCK WHERE
        ArtBas.ArtikkelNr = TelleLinje.ArtikkelNr NO-ERROR.
      IF AVAILABLE ArtBas THEN
        DO:
          IF ArtBas.Utgatt = TRUE THEN
            ASSIGN        
              TelleLinje.VgLopNr:bgcolor IN BROWSE BROWSE-TelleLinje = wDiffColor3.        
          ASSIGN
              cVareTekst = ArtBas.Beskr
              .
          FIND ArtPris OF ArtBas NO-LOCK WHERE
              ArtPris.ProfilNr = iCLProfilNr NO-ERROR.
          IF AVAILABLE ArtPris THEN
          DO:
              ASSIGN        
              lPris:SCREEN-VALUE IN BROWSE BROWSE-TelleLinje = STRING(ArtPris.Pris[1])
              lTilbPris:SCREEN-VALUE IN BROWSE BROWSE-TelleLinje = STRING(ArtPris.Pris[2])
              .
              IF ArtPris.Tilbud THEN
              ASSIGN        
                lTilbPris:BGCOLOR IN BROWSE BROWSE-TelleLinje = 17.        
          END.
          ELSE ASSIGN        
          lPris:SCREEN-VALUE IN BROWSE BROWSE-TelleLinje = ''
          lTilbPris:SCREEN-VALUE IN BROWSE BROWSE-TelleLinje = ''
          .
        END.
      IF wNedskriv = FALSE THEN
        DO:
          IF (TelleLinje.AntallPar - TelleLinje.AntallTalt) > 0 THEN
            ASSIGN        
              TelleLinje.AntallDiff:bgcolor IN BROWSE BROWSE-TelleLinje = wDiffColor.        
          ELSE IF (TelleLinje.AntallPar - TelleLinje.AntallTalt) < 0 THEN
            ASSIGN        
              TelleLinje.AntallDiff:bgcolor IN BROWSE BROWSE-TelleLinje = wDiffColor2.        
          ELSE
            ASSIGN        
              TelleLinje.Nedskrevet:bgcolor IN BROWSE BROWSE-TelleLinje = ?.
        END.
      ELSE DO:
          IF TelleLinje.Nedskrevet <> TelleLinje.VVareKost THEN
            DO:
              ASSIGN        
                TelleLinje.Nedskrevet:bgcolor IN BROWSE BROWSE-TelleLinje = wDiffColor.
            END.
          ELSE
            ASSIGN        
              TelleLinje.Nedskrevet:bgcolor IN BROWSE BROWSE-TelleLinje = ?.
      END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-TelleLinje C-Win
ON ROW-LEAVE OF BROWSE-TelleLinje IN FRAME DEFAULT-FRAME
DO:
  IF AVAILABLE TelleLinje THEN 
    DO:
      IF wNedskriv = FALSE THEN
      IKKE-NEDSKRIVNING:
      DO:
        /* Setter inn utregnet differanse. */
        IF  TelleLinje.AntallTalt <> int(TelleLinje.AntallTalt:screen-value IN BROWSE BROWSE-TelleLinje) THEN
          DO TRANSACTION:
            FIND CURRENT TelleLinje EXCLUSIVE-LOCK.
      
            ASSIGN
              TelleLinje.AntallDiff:screen-value = 
                   STRING(
                          int(TelleLinje.AntallPar) - 
                          int(TelleLinje.AntallTalt:screen-value IN BROWSE BROWSE-TelleLinje)
                         )
              TelleLinje.AntallDiff = int(TelleLinje.AntallDiff:screen-value)
              TelleLinje.OpptVerdi:screen-value = 
                   STRING(
                          int(TelleLinje.AntallTalt:screen-value IN BROWSE BROWSE-TelleLinje) *
                          (TelleLinje.VVareKost - (IF TelleHode.TTId = 1 THEN TelleLinje.RabKr ELSE 0))
                         )
              TelleLinje.OpptVerdi = int(TelleLinje.OpptVerdi:screen-value)
              TelleLinje.VerdiDiff:screen-value = 
                   STRING(
                          (
                           int(TelleLinje.AntallPar) - 
                           int(TelleLinje.AntallTalt:screen-value IN BROWSE BROWSE-TelleLinje)
                           ) * (TelleLinje.VVareKost - (IF TelleHode.TTId = 1 THEN TelleLinje.RabKr ELSE 0))
                         )
              TelleLinje.VerdiDiff = int(TelleLinje.VerdiDiff:screen-value).
            /* Setter på korrekt farge på differansen. */
            IF int(TelleLinje.AntallDiff:screen-value IN BROWSE BROWSE-TelleLinje) = 0 THEN
              TelleLinje.AntallDiff:bgcolor IN BROWSE BROWSE-TelleLinje = ?.
            ELSE IF int(TelleLinje.AntallDiff:screen-value IN BROWSE BROWSE-TelleLinje) > 0 THEN
              TelleLinje.AntallDiff:bgcolor IN BROWSE BROWSE-TelleLinje = wDiffColor.
            ELSE
              TelleLinje.AntallDiff:bgcolor IN BROWSE BROWSE-TelleLinje = wDiffColor2.

            FIND CURRENT TelleLinje NO-LOCK.
          END.
      END. /* IKKE-NEDSKRIVNING */
      ELSE
      NEDSKRIVNING:
      DO:
        /* Setter inn utregnet differanse. */
        IF  TelleLinje.Nedskrevet <> int(TelleLinje.Nedskrevet:screen-value IN BROWSE BROWSE-TelleLinje) THEN
          DO TRANSACTION:
            FIND CURRENT TelleLinje EXCLUSIVE-LOCK.
      
            ASSIGN
              TelleLinje.VerdiDiff:screen-value = 
                   STRING(
                          (TelleLinje.AntallPar * TelleLinje.VVareKost) -
                          (TelleLinje.AntallPar * dec(TelleLinje.Nedskrevet:screen-value IN BROWSE BROWSE-TelleLinje))
                         )
              TelleLinje.VerdiDiff = dec(TelleLinje.VerdiDiff:screen-value).
              
            /* Setter på korrekt farge på differansen. */
            IF dec(TelleLinje.Nedskrevet:screen-value IN BROWSE BROWSE-TelleLinje) = TelleLinje.VVareKost THEN
              TelleLinje.Nedskrevet:bgcolor IN BROWSE BROWSE-TelleLinje = ?.
            ELSE
              TelleLinje.Nedskrevet:bgcolor IN BROWSE BROWSE-TelleLinje = wDiffColor.

            FIND CURRENT TelleLinje NO-LOCK.
            
            /* Oppdaterer på alle størrelser for butikken. */
            FOR EACH bTelleLinje EXCLUSIVE-LOCK WHERE
              bTelleLinje.TelleNr    = TelleLinje.TelleNr AND
              bTelleLinje.ArtikkelNr = TelleLinje.ArtikkelNr AND
              (IF TRIM(ENTRY(2,CB-Butikk:screen-value,":")) = wStdTxt 
                THEN bTelleLinje.Butik > 0
                ELSE bTelleLinje.Butik = TelleLinje.Butik):
              ASSIGN
                bTelleLinje.Nedskrevet = dec(TelleLinje.Nedskrevet:screen-value IN BROWSE BROWSE-TelleLinje)
                bTelleLinje.VerdiDiff  = (bTelleLinje.AntallPar * TelleLinje.VVareKost) -
                                         (bTelleLinje.AntallPar * bTelleLinje.Nedskrevet).
            END.
            wOk = BROWSE-TelleLinje:refresh().
          END.
        ELSE DO:
          TelleLinje.VerdiDiff:bgcolor IN BROWSE BROWSE-TelleLinje = ?.
        END.
      END. /* NEDSKRIVNING */
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-TelleLinje C-Win
ON START-SEARCH OF BROWSE-TelleLinje IN FRAME DEFAULT-FRAME
DO:
  IF BROWSE {&BROWSE-NAME}:FOCUSED-ROW = ? THEN DO:
      APPLY "END-SEARCH" TO SELF.
      RETURN NO-APPLY.
  END.
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
/* MESSAGE wSearchCol:PRIVATE-DATA        */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK. */
/*       ASSIGN wQString = wQ:PREPARE-STRING                                                                              */
/*              wSortColIdx = LOOKUP("{&br-tabell}." + wSearchCol:NAME,wQString," ")                                      */
/*              wQString = IF ENTRY(wSortColIdx + 1,wQString," ") = "DESCENDING" THEN                                     */
/*                  REPLACE(wQString,"{&br-tabell}." + wSearchCol:NAME + " DESCENDING","{&br-tabell}." + wSearchCol:NAME) */
/*                         ELSE                                                                                           */
/*                  REPLACE(wQString,"{&br-tabell}." + wSearchCol:NAME,                                                   */
/*                            "{&br-tabell}." + wSearchCol:NAME + " DESCENDING")                                          */
/*              wSearchCol:PRIVATE-DATA = wQString                                                                        */
/*              cQstring2 = wQString.                                                                                     */
         wQString = get-qry("").
      wQ:QUERY-PREPARE(wQString).
      FIND b{&br-tabell} WHERE ROWID(b{&br-tabell}) = ROWID({&br-tabell}) NO-LOCK.
      wQ:QUERY-OPEN().
      RUN SD-Reposition.
  END.
  APPLY "LEAVE" TO SELF. /* annars fungerar inte "ENTRY" ?? */
  APPLY "ENTRY" TO SELF.
  APPLY "END-SEARCH" TO SELF.
  SELF:SELECT-FOCUSED-ROW().
  RUN lockwindowupdate(0).
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help-2 C-Win
ON CHOOSE OF Btn_Help-2 IN FRAME DEFAULT-FRAME /* Help */
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
  {&return-ip}
  
  &IF DEFINED(assign-retur-verdi) &THEN
      {&assign-retur-verdi}
  &ELSE
      retur-verdi = "OK".
  &ENDIF
  
  APPLY "close":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 C-Win
ON CHOOSE OF BUTTON-1 IN FRAME DEFAULT-FRAME /* Strekkode registrering */
DO:
  APPLY 'CHOOSE' TO BUTTON-Ny.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-3 C-Win
ON CHOOSE OF BUTTON-3 IN FRAME DEFAULT-FRAME /* Registrer antall */
DO:
  APPLY "ENTRY" TO BROWSE-TelleLinje.
  APPLY "ENTRY" TO Tellelinje.AntallTalt IN BROWSE BROWSE-TelleLinje.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Ny
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Ny C-Win
ON CHOOSE OF BUTTON-Ny IN FRAME DEFAULT-FRAME /* Ny... */
DO:
  IF NOT AVAILABLE TelleHode THEN
    RETURN.
  /*
  if available TelleLinje then
    wRecid = recid(TelleLinje).
  else
  */
  wRecid = ?.
  RUN w-v{&br-tabell}.w PERSISTENT SET wLinjeHandle (INPUT-OUTPUT wRecid,"Endre", wButik, ROWID(TelleHode), THIS-PROCEDURE:HANDLE, TRIM(ENTRY(2,CB-Butikk:screen-value,":")) = wStdTxt ).
  RETURN NO-APPLY.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Slett
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Slett C-Win
ON CHOOSE OF BUTTON-Slett IN FRAME DEFAULT-FRAME /* Slett */
DO:
  DEF VAR wSvar AS LOG NO-UNDO.

  IF NOT AVAILABLE TelleLinje THEN
    RETURN NO-APPLY.
    
  IF BROWSE-TelleLinje:NUM-SELECTED-ROWS = 0 THEN
    DO:
      MESSAGE "Det er ingen merket rad å ta bort."
              VIEW-AS ALERT-BOX MESSAGE TITLE "Melding".
      RETURN NO-APPLY.
    END.   
  
  ASSIGN wSvar = FALSE.

  MESSAGE "Skal telleposten slettes?"
  VIEW-AS ALERT-BOX BUTTONS YES-NO TITLE "Bekreftelse" 
  UPDATE wSvar.

  RUN SlettLinje (INPUT 1).            
  RUN SettButtons.
  APPLY "ENTRY" TO BROWSE {&BROWSE-NAME}.
  RETURN NO-APPLY.
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
   DEFINE VARIABLE iTst AS INTEGER    NO-UNDO.
   DEFINE VARIABLE cSearchValue AS CHARACTER  NO-UNDO.
   wBlank = FALSE. 

   IF wAktivFillIn:SCREEN-VALUE = "" THEN
       wBlank = TRUE.
   ELSE DOBLOCK: DO:
       IF wSortCol:NAME = "VgLopNr" THEN DO:
           ASSIGN cSearchValue = TRIM(wAktivFillIn:SCREEN-VALUE).
           IF NUM-ENTRIES(cSearchValue,"/") > 2 THEN DO:
               ASSIGN wAktivFillIn:SCREEN-VALUE = ""
                      wBlank = TRUE.
               LEAVE DOBLOCK.
           END.
           ELSE IF NUM-ENTRIES(cSearchValue,"/") = 1 OR (NUM-ENTRIES(cSearchValue,"/") = 2 AND ENTRY(2,cSearchValue,"/") = "") THEN DO:
               ASSIGN cSearchValue = ENTRY(1,cSearchValue,"/")
                      iTst = INT(cSearchValue) NO-ERROR.
               IF ERROR-STATUS:ERROR THEN DO:
                   ASSIGN wAktivFillIn:SCREEN-VALUE = ""
                          wBlank = TRUE.
                   LEAVE DOBLOCK.
               END.
               wSQStr = "FOR EACH bTellelinje NO-LOCK WHERE bTellelinje.TelleNr = " + STRING(TelleHode.TelleNr) +
                   " AND bTelleLinje.Vg >= " + cSearchValue + " USE-INDEX VglopNr INDEXED-REPOSITION".
           END.
           ELSE DO:
               wSQStr = "FOR EACH bTellelinje NO-LOCK WHERE bTellelinje.TelleNr = " + STRING(TelleHode.TelleNr) +
                   " AND bTelleLinje.Butik = " + STRING(Butiker.Butik) + " AND bTelleLinje.VgLopNr >= '" + cSearchValue +
                    "' USE-INDEX SortVglopNrStorl INDEXED-REPOSITION".
           END.
       END.
       ELSE IF wSortCol:NAME = "ArtikkelNr" THEN DO:
           ASSIGN cSearchValue = TRIM(wAktivFillIn:SCREEN-VALUE).
           wSQStr = "FOR EACH bTellelinje NO-LOCK WHERE bTellelinje.TelleNr = " + STRING(TelleHode.TelleNr) +
               " AND bTelleLinje.Artikkelnr = " + cSearchValue + " USE-INDEX SeqNr INDEXED-REPOSITION".
       END.
       ELSE DO:
           ASSIGN cSearchValue = IF wSortCol:NAME = "Storl" THEN FiksStorl(TRIM(wAktivFillIn:SCREEN-VALUE)) ELSE wAktivFillIn:SCREEN-VALUE
                  wChar  = IF wAktivFillIn:DATA-TYPE BEGINS "CHAR" OR wAktivFillIn:DATA-TYPE BEGINS "DECI" THEN '"' ELSE ''
                  wSQStr = BROWSE {&BROWSE-NAME}:QUERY:PREPARE-STRING
               wSQStr = REPLACE(wSQStr,"BY", "AND TelleLinje." + wSortCol:NAME + " >= " + wChar + cSearchValue + wChar + " BY")
                  wSQStr = REPLACE(wSQStr,"TelleLinje","bTelleLinje").
       END.
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
   ELSE DO:
     wBlank = ?.
     RUN SD-Reposition.
     wBlank = FALSE.
   END.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-Butikk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Butikk C-Win
ON VALUE-CHANGED OF CB-Butikk IN FRAME DEFAULT-FRAME /* Butikk */
DO:
  ASSIGN
    wButik = int(ENTRY(1,CB-Butikk:screen-value,":")).
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  APPLY "entry":U TO BROWSE-TelleLinje.
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


&Scoped-define SELF-NAME m_Nullstill
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Nullstill C-Win
ON CHOOSE OF MENU-ITEM m_Nullstill /* Nullstill filter */
DO:
    DO WITH FRAME F-Filter:
        APPLY "CHOOSE" TO B-VaretekstBlank.
        APPLY "CHOOSE" TO B-LevNrBlank.
        APPLY "CHOOSE" TO B-FilterOK.
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME QUERY-Alle
&Scoped-define SELF-NAME QUERY-tmpx
&Scoped-define SELF-NAME T-Avangsert
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-Avangsert C-Win
ON VALUE-CHANGED OF T-Avangsert IN FRAME DEFAULT-FRAME /* Avansert */
DO:
  IF SELF:SCREEN-VALUE = "no" THEN
  DO:
      ASSIGN
          B-1:HIDDEN        = TRUE
          B-2:HIDDEN        = TRUE
          B-Tomme:HIDDEN    = TRUE
          B-3:HIDDEN        = TRUE
          B-Positive:HIDDEN = TRUE
          B-Negative:HIDDEN = TRUE
          .
  END.
  ELSE DO:
      ASSIGN
          B-1:HIDDEN        = FALSE
          B-2:HIDDEN        = FALSE
          B-Tomme:HIDDEN    = FALSE
          B-3:HIDDEN        = FALSE
          B-Positive:HIDDEN = FALSE
          B-Negative:HIDDEN = FALSE
          .
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
ASSIGN iWidthPix  = {&WINDOW-NAME}:WIDTH-PIXELS
       iHeightPix = {&WINDOW-NAME}:HEIGHT-PIXELS.

{inutmld.i &Modus = "Opprett"} /* Melder fra at programmet har startet. */

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
    {genlib.i
      &NoLibCall      = "Nei"
      &WindowName     = "TelleLinje"
      &PostDisable_ui = "if valid-Handle(wLinjeHandle) then
                           delete procedure wLinjeHandle."
    }

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

{syspara.i 1 100 1 wAlle}
{syspara.i 1 4 1 wExcEkstent}
wExcEkstent = IF wExcEkstent = "" THEN "sdv" ELSE wExcEkstent.   

ASSIGN
  wParentHandle = ?.

FIND TelleHode NO-LOCK WHERE
  RECID(TelleHode) = wTelleHodeRecid NO-ERROR.
IF NOT AVAILABLE TelleHode THEN
  DO:
    MESSAGE "Ukjent tellehode!" VIEW-AS ALERT-BOX ERROR TITLE "Feil".
    RETURN "AVBRYT".
  END.

{syspara.i 1 4 1 wExcEkstent}
wExcEkstent = IF wExcEkstent = "" THEN "sdv" ELSE wExcEkstent.   
{syspara.i 1 100 1 wStdTxt}
{syspara.i 5   1 1 iCl INT}
{syspara.i 1 2 4 wEDB-System}

ASSIGN wTabell = "ArtBas".

/* HotKey */
ON ALT-N OF C-Win ANYWHERE 
  DO:
    APPLY "CHOOSE":U TO BUTTON-Ny IN FRAME DEFAULT-FRAME.
  END.
ON ALT-D OF C-Win ANYWHERE 
  DO:
    APPLY "CHOOSE":U TO BUTTON-Slett IN FRAME DEFAULT-FRAME.
  END.
ON ALT-P OF C-Win ANYWHERE 
  DO:
    APPLY "CHOOSE":U TO B-SkrivUt IN FRAME DEFAULT-FRAME.
  END.
ON ALT-X OF C-Win ANYWHERE 
  DO:
    APPLY "CHOOSE":U TO B-Excel IN FRAME DEFAULT-FRAME.
  END.

/* Nedskrivning */
{syspara.i 4 1 2 wTekst}
IF STRING(TelleHode.TTId) = wTekst THEN
  wNedskriv = TRUE.
ELSE 
  wNedskriv = FALSE.  

/* Oppsett av farger. */
ASSIGN
  wDiffColor  = 12
  wDiffColor2 = 10
  wDiffColor3 = 14.

{syspara.i 1 4   1 cExcEkstent}
cExcEkstent = IF cExcEkstent = "" THEN "sdv" ELSE cExcEkstent.   
{syspara.i 1 1 54 cSkomodus}
{syspara.i 1 1 100 cKunde}
{syspara.i 1 1 101 cSkoTex}

FIND clButiker NO-LOCK WHERE
    clButiker.Butik = iCl NO-ERROR.
IF NOT AVAILABLE clButiker THEN
DO:
    MESSAGE 
    "Ukjent sentrallager butikk (" + STRING(icl) + ")."
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
END.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    RUN SetDivResize.
    RUN InitVars. /* inkl open-query */
    IF RETURN-VALUE = "FEIL" THEN
       RETURN.
/*     IF "VgLopNr" <> wSortCol:NAME THEN       */
/*         RUN QueryCustomSettings ("VgLopNr"). */
    RUN SD-QUERY-OPEN.

    IF RETURN-VALUE = "FEIL" THEN
        LEAVE MAIN-BLOCK.

    FIND Butiker WHERE Butiker.Butik = iCL NO-LOCK NO-ERROR.
    ASSIGN iCLProfilNr = Butiker.ProfilNr.
    
    RUN PrBut.

    RUN enable_UI.
    FRAME F-Filter:MOVE-TO-BOTTOM().
    ASSIGN {&WINDOW-NAME}:X = SOURCE-PROCEDURE:CURRENT-WINDOW:X + 20
           {&WINDOW-NAME}:Y = SOURCE-PROCEDURE:CURRENT-WINDOW:Y + 30.
    BROWSE {&BROWSE-NAME}:QUERY:QUERY-PREPARE(REPLACE(REPLACE("{&BrowseInitQ}","XTELLENR","'" + trim(STRING(TelleHode.TelleNr)) + "'"),"XBUTIKK",ENTRY(1,CB-Butikk:screen-value,":")) + " INDEXED-REPOSITION").
    BROWSE {&BROWSE-NAME}:FIRST-COLUMN:PRIVATE-DATA = BROWSE {&BROWSE-NAME}:QUERY:PREPARE-STRING.
    ASSIGN wSortCol = BROWSE {&BROWSE-NAME}:FIRST-COLUMN.
    BROWSE {&BROWSE-NAME}:QUERY:QUERY-OPEN().
/*     RUN StartSearch(BROWSE {&BROWSE-NAME}:FIRST-COLUMN). */
    RUN FixLabelWidth.
    {lng.i} 

    {browsesettings.i {&BROWSE-NAME}}
    IF BROWSE {&BROWSE-NAME}:CURRENT-COLUMN <> wSortCol THEN DO:
        wSortCol = BROWSE {&BROWSE-NAME}:CURRENT-COLUMN.
        RUN QueryCustomSettings (wSortCol:NAME).
    END.
    
    RUN LabelColor.
    RUN Move-Fill-To-Top. 
    &IF DEFINED(init-phrase) &THEN
    {&init-phrase}
    &ENDIF 
    IF AVAILABLE b{&br-tabell} THEN DO:
        RUN SD-Reposition.
    END. 
    ELSE IF AVAILABLE {&br-tabell} THEN
      DO:
        REPOSITION {&BROWSE-NAME} TO ROW 1.
      END.

      IF TelleHode.TTId <> 9 THEN
          B-HentLister:HIDDEN = TRUE.
      ASSIGN B-Suppler:HIDDEN   = NOT (TelleHode.TTId = 9 AND TelleHode.Oppdatert = ?)
             B-Etiketter:HIDDEN = (TelleHode.TTId <> 5 AND TelleHode.TTId <> 6).
      IF TelleHode.Oppdatert = ? THEN
        DO:
          ASSIGN
            B-1:sensitive          = TRUE 
            B-2:sensitive          = TRUE 
            B-3:sensitive          = TRUE
            B-Tomme:sensitive      = TRUE
            B-Negative:sensitive   = TRUE
            B-Positive:sensitive   = TRUE
            BUTTON-Ny:sensitive    = TRUE 
            BUTTON-Slett:sensitive = TRUE      
            B-Oppdat:SENSITIVE     = TRUE
            B-Bygg:SENSITIVE       = TRUE
            B-Fil:SENSITIVE        = TRUE
            TelleLinje.Merknad:READ-ONLY IN BROWSE BROWSE-TelleLinje = FALSE.

          IF wNedskriv THEN
            ASSIGN
              TelleLinje.AntallTalt:READ-ONLY IN BROWSE BROWSE-TelleLinje    = TRUE
              TelleLinje.Nedskrevet:READ-ONLY IN BROWSE BROWSE-TelleLinje    = FALSE.
              /*
              TelleLinje.VerdiDiff:column-label in browse BROWSE-TelleLinje  = "NyVerdi".
              */
          ELSE 
            ASSIGN
              TelleLinje.AntallTalt:READ-ONLY IN BROWSE BROWSE-TelleLinje = FALSE
              TelleLinje.Nedskrevet:READ-ONLY IN BROWSE BROWSE-TelleLinje  = TRUE.
          IF TelleHode.TTId = 1 /* VAreSalg */ THEN
            ASSIGN
              B-Pris:hidden      = FALSE
              B-Pris:sensitive   = TRUE
              B-Rabatt:hidden    = FALSE
              B-Rabatt:sensitive = TRUE.
        END.
      ELSE DO:
        ASSIGN
          T-Avangsert:SENSITIVE  = FALSE
          B-1:sensitive          = FALSE 
          B-2:sensitive          = FALSE 
          B-3:sensitive          = FALSE
          B-3:sensitive          = FALSE
          BUTTON-Ny:sensitive    = FALSE 
          BUTTON-Slett:sensitive = FALSE      
          TelleLinje.AntallTalt:READ-ONLY IN BROWSE BROWSE-TelleLinje = TRUE
          TelleLinje.Nedskrevet:READ-ONLY IN BROWSE BROWSE-TelleLinje = TRUE.  
          TelleLinje.Merknad:READ-ONLY IN BROWSE BROWSE-TelleLinje    = TRUE.
      END.      

/*       RUN PrBut. */

/*       display                   */
/*         CB-Butikk               */
/*       with frame DEFAULT-FRAME. */
      ASSIGN
          B-1:HIDDEN        = TRUE
          B-2:HIDDEN        = TRUE
          B-Tomme:HIDDEN    = TRUE
          B-3:HIDDEN        = TRUE
          B-Positive:HIDDEN = TRUE
          B-Negative:HIDDEN = TRUE
          .
    ASSIGN {&WINDOW-NAME}:HIDDEN = FALSE.
    IF BROWSE {&BROWSE-NAME}:FOCUSED-ROW <> ? THEN
        BROWSE {&BROWSE-NAME}:SELECT-FOCUSED-ROW().
/*     APPLY "LEAVE" TO BROWSE {&BROWSE-NAME}. */
/*     APPLY "ENTRY" TO BROWSE {&BROWSE-NAME}. */
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

IF AVAIL Tellehode AND tellehode.oppdatert = ? THEN 
    RUN OppdatTelleHode (INPUT ?).

&IF DEFINED(UIB_IS_RUNNING) EQ 0 &THEN
 RETURN retur-verdi.
&else
 MESSAGE retur-verdi VIEW-AS ALERT-BOX.
&endif

PROCEDURE LockWindowUpdate EXTERNAL "user32.dll" :
  DEFINE INPUT  PARAMETER hWndLock AS LONG.
END PROCEDURE.

PROCEDURE ShellExecute{&A} EXTERNAL "shell32" :
   DEFINE INPUT PARAMETER hwnd AS long.
   DEFINE INPUT PARAMETER lpOperation AS CHAR.
   DEFINE INPUT PARAMETER lpFile AS CHAR.
   DEFINE INPUT PARAMETER lpParameters AS CHAR.
   DEFINE INPUT PARAMETER lpDirectory AS CHAR.
   DEFINE INPUT PARAMETER nShowCmd AS long.
   DEFINE RETURN PARAMETER hInstance AS long.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AlleBut C-Win 
PROCEDURE AlleBut :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME DEFAULT-FRAME:
wTekst = IF (wNedskriv AND NUM-ENTRIES(TelleHode.ButikkListe) > 1)
           then string(iCl,"zzzzz9") + ": " + wStdTxt
           else "".
ASSIGN
  CB-Butikk:List-Items = wTekst
  CB-Butikk            = wTekst
  wButik               = INT(ENTRY(1,CB-Butikk,":")).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Artikkelkort C-Win 
PROCEDURE Artikkelkort :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF Tellelinje.Artikkelnr = 0 OR NOT CAN-FIND(ArtBas WHERE ArtBas.ArtikkelNr
                                = Tellelinje.Artikkelnr) THEN
    RETURN.
  FIND ArtBas WHERE ArtBas.ArtikkelNr = Tellelinje.Artikkelnr NO-LOCK.
  fLockvindu(TRUE).
  RUN w-vartkor (INPUT RECID(ArtBas), "ENDRE," + STRING(THIS-PROCEDURE)).
  fLockvindu(FALSE).

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

OCXFile = SEARCH( "w-btellelinje.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chProgressBar = ProgressBar:COM-HANDLE
    UIB_S = chProgressBar:LoadControls( OCXFile, "ProgressBar":U)
    ProgressBar:NAME = "ProgressBar":U
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "w-btellelinje.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DelTmpChild C-Win 
PROCEDURE DelTmpChild :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH tmpChild:
        IF VALID-HANDLE(tmpChild.wChild) THEN DO:
            RUN DelTmpChild IN tmpChild.wChild NO-ERROR.
            DELETE PROCEDURE tmpChild.wChild.
        END.
    END.

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
  DISPLAY CB-Butikk T-Avangsert FILL-IN-SOK-DECI FILL-IN-SOK-DATE 
          FILL-IN-SOK-INTE FILL-IN-SOK-CHAR 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE BROWSE-TelleLinje B-Filter B-Etiketter B-Artikkelkort B-Excel 
         B-SkrivUt BUTTON-Ny BUTTON-Slett B-Suppler B-HTFil-2 Btn_Help-2 Btn_OK 
         T-Avangsert FILL-IN-SOK-DECI FILL-IN-SOK-DATE FILL-IN-SOK-INTE 
         FILL-IN-SOK-CHAR BUTTON-Sok BUTTON-3 BUTTON-1 B-HentLister RECT-1 
         RECT-2 RECT-3 RECT-63 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  VIEW FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  DISPLAY FI-Varetekst FI-LevNr FI-Levnavn 
      WITH FRAME F-Filter IN WINDOW C-Win.
  ENABLE B-LevNrBlank FI-Varetekst FI-LevNr B-FilterOK B-VaretekstBlank 
         B-SokLevNr 
      WITH FRAME F-Filter IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Filter}
  FRAME F-Filter:SENSITIVE = NO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Etiketter C-Win 
PROCEDURE Etiketter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER iType AS INTEGER    NO-UNDO.
  DEFINE        VARIABLE  iLoop AS INTEGER    NO-UNDO.
  IF NOT VALID-HANDLE(hEtikettVindu) THEN
      RUN w-TmpEtikett.w PERSISTENT SET hEtikettVindu (C-Win).
  IF iType = 1 THEN DO:
      FOR EACH TelleLinje OF TelleHode WHERE TelleLinje.AntallTalt > 0 NO-LOCK:
          FIND StrKonv WHERE StrKonv.Storl = TelleLinje.Storl NO-LOCK NO-ERROR.
          IF AVAIL StrKonv THEN DO:
            FIND FIRST StrekKode WHERE StrekKode.ArtikkelNr = TelleLinje.Artikkelnr AND
                                       StrekKode.KodeType = 1 AND 
                                       StrekKode.StrKode = StrKonv.StrKode AND
                                       NOT StrekKode.Kode BEGINS "02" NO-LOCK NO-ERROR.
            IF NOT AVAIL StrekKode THEN
                FIND FIRST StrekKode WHERE StrekKode.ArtikkelNr = TelleLinje.Artikkelnr AND
                                           StrekKode.KodeType = 1 AND 
                                           StrekKode.StrKode = StrKonv.StrKode NO-LOCK NO-ERROR.
            IF AVAIL StrekKode AND VALID-HANDLE(hEtikettVindu) THEN
                RUN NyEtikettTelling IN hEtikettVindu (TelleLinje.TelleNr,StrekKode.Kode,TelleLinje.antallTalt,0).
          END.
      END.
  END.
  ELSE DO WITH FRAME {&FRAME-NAME}:
        DO iLoop = 1 TO BROWSE-TelleLinje:NUM-SELECTED-ROWS:
            BROWSE-TelleLinje:FETCH-SELECTED-ROW(iLoop).
            IF TelleLinje.antallTalt > 0 THEN DO:
                FIND StrKonv WHERE StrKonv.Storl = TelleLinje.Storl NO-LOCK NO-ERROR.
                IF AVAIL StrKonv THEN DO:
                  FIND FIRST StrekKode WHERE StrekKode.ArtikkelNr = TelleLinje.Artikkelnr AND
                                             StrekKode.KodeType = 1 AND 
                                             StrekKode.StrKode = StrKonv.StrKode AND
                                             NOT StrekKode.Kode BEGINS "02" NO-LOCK NO-ERROR.
                  IF NOT AVAIL StrekKode THEN
                      FIND FIRST StrekKode WHERE StrekKode.ArtikkelNr = TelleLinje.Artikkelnr AND
                                                 StrekKode.KodeType = 1 AND 
                                                 StrekKode.StrKode = StrKonv.StrKode NO-LOCK NO-ERROR.
                  IF AVAIL StrekKode AND VALID-HANDLE(hEtikettVindu) THEN
                      RUN NyEtikettTelling IN hEtikettVindu (TelleLinje.TelleNr,StrekKode.Kode,TelleLinje.antallTalt,0).
                END.
            END.
        END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExportToExcel C-Win 
PROCEDURE ExportToExcel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT AVAILABLE TelleHode THEN
    RETURN.
    
  DEFINE VARIABLE iTelleNr AS INTEGER NO-UNDO.
  
  ASSIGN 
    iTelleNr = TelleHode.TelleNr.
  RUN Varetelling_Til_Excel.p (iTelleNr).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FixLabelWidth C-Win 
PROCEDURE FixLabelWidth :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE wW AS HANDLE     NO-UNDO.
    ASSIGN wW        = BROWSE {&BROWSE-NAME}:FIRST-COLUMN.
    REPEAT WHILE VALID-HANDLE(wW):
        ASSIGN wW:AUTO-RESIZE = TRUE
              wW:LABEL        = TRIM(wW:LABEL)
               wW             = wW:NEXT-COLUMN.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HTuppslag C-Win 
PROCEDURE HTuppslag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER lUppslag AS LOGICAL NO-UNDO.
  lUppslag = TRUE.
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
       ASSIGN chProgressBar = chProgressBar:ProgressBar.
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
        MESSAGE "Du må 'enabla' minst ett felt" VIEW-AS ALERT-BOX.
        RETURN "FEIL".
    END.
    IF NUM-ENTRIES(wSorttype) <> wAntSortCol THEN DO:
        MESSAGE "&scope Sorttype skall ha " + STRING(wAntSortCol) +
                " entries i definitionsblocket," SKIP
                " kommaseparert med valgfritt BY, BY DESCENDING eller USE-INDEX." 
                VIEW-AS ALERT-BOX ERROR.
        RETURN "FEIL".
    END.
    IF NUM-ENTRIES(wSokvillkor) <> wAntSortCol THEN DO:
        MESSAGE "&scope Sokvillkor skall ha " + STRING(wAntSortCol) +
                " entries i definitionsblocket," SKIP
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
               ENTRY(NUM-ENTRIES(ENTRY(wCount,"{&ENABLED-FIELDS-IN-QUERY-{&BROWSE-NAME}}"," "),"."),ENTRY(wCount,"{&ENABLED-FIELDS-IN-QUERY-{&BROWSE-NAME}}"," "),".")
                                                ELSE
               wSearchCols + "," + 
                   ENTRY(NUM-ENTRIES(ENTRY(wCount,"{&ENABLED-FIELDS-IN-QUERY-{&BROWSE-NAME}}"," "),"."),ENTRY(wCount,"{&ENABLED-FIELDS-IN-QUERY-{&BROWSE-NAME}}"," "),".").
    END.
    ASSIGN wW        = BROWSE {&BROWSE-NAME}:FIRST-COLUMN
           wOrgBgCol = wW:Label-bgcolor
           wCount = 1.
    REPEAT WHILE VALID-HANDLE(wW):
        IF LOOKUP(wW:NAME,wSearchCols) > 0 THEN
/*             ASSIGN wW:PRIVATE-DATA      = PREP-PRIVATE-DATA(wW:HANDLE,wCount) */
            ASSIGN wW:PRIVATE-DATA      = PREP-PRIVATE-DATA(wW:HANDLE,wCount)
                   wSearchColsH[wCount] = wW:HANDLE
                   wW:LABEL = wW:LABEL + STRING(LOOKUP("*",wW:LABEL," ") = 0," */")
                   wW:LABEL = wW:LABEL + IF ENTRY(wCount,wSortType) = "USE-INDEX" THEN
                                "" ELSE "*"
                   wW:LABEL = " " + wW:LABEL
                   wCount               = wCount + 1
                   wW:READ-ONLY         = YES.
        ASSIGN wW = wW:NEXT-COLUMN.
    END.
    ASSIGN BROWSE {&BROWSE-NAME}:CURRENT-COLUMN = wSearchColsH[wAktivCol]
           wSortCol                             = wSearchColsH[wAktivCol]
           wQ                                   = BROWSE {&BROWSE-NAME}:QUERY.
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
           IF wSortCol:DATA-TYPE = "INTEGER" THEN FILL(">",LENGTH(wSortCol:FORMAT)) ELSE IF wSortCol:DATA-TYPE = "DECIMAL"
                  THEN wAktivFillIn:FORMAT ELSE wSortCol:FORMAT
           FILL-IN-SOK-DATE:SENSITIVE IN FRAME {&FRAME-NAME} = SUBSTR(wSortCol:DATA-TYPE,1,4) = "DATE"
           FILL-IN-SOK-INTE:SENSITIVE IN FRAME {&FRAME-NAME} = SUBSTR(wSortCol:DATA-TYPE,1,4) = "INTE"
           FILL-IN-SOK-CHAR:SENSITIVE IN FRAME {&FRAME-NAME} = SUBSTR(wSortCol:DATA-TYPE,1,4) = "CHAR"
           FILL-IN-SOK-DECI:SENSITIVE IN FRAME {&FRAME-NAME} = SUBSTR(wSortCol:DATA-TYPE,1,4) = "DECI".
               
    RETURN NO-APPLY.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NyLinje C-Win 
PROCEDURE NyLinje :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER wRecid AS ROWID NO-UNDO.
  
  {&OPEN-QUERY-{&BROWSE-NAME}}
  FIND {&br-tabell} NO-LOCK WHERE
    ROWID({&br-tabell}) = wRecid NO-ERROR.

  IF AVAILABLE TelleLinje THEN
    DO WITH FRAME DEFAULT-FRAME:
      REPOSITION BROWSE-TelleLinje TO ROWID ROWID({&br-tabell}) NO-ERROR.
      APPLY "ENTRY" TO BROWSE BROWSE-TelleLinje.
    END.
  
  APPLY "VALUE-CHANGED":U TO BROWSE BROWSE-TelleLinje.
  
  RETURN "Ok".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdatOverforing C-Win 
PROCEDURE oppdatOverforing :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iBuntNr AS INTEGER INIT ? NO-UNDO.
  DEFINE VARIABLE cBuntTxt AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iOpphav AS INT NO-UNDO.

  iOpphav = 9. /* Varetelling */
  
  IF iBuntNr = 0 THEN
      ASSIGN iBuntNr = ?.
      
  ASSIGN cBuntTxt = "Overf. fra varetelling" + STRING(TODAY) + STRING(TIME,"HH:MM").
/*   Här skapas automatiskt en ny overföring. Vi skall inte köra d-velgov */
/*   RUN d-velgovbunt.w (INPUT-OUTPUT iBuntNr,INPUT-OUTPUT cBuntTxt).     */
/*   IF RETURN-VALUE <> "AVBRYT" THEN                                     */
  DO:
  /*{sww.i}*/
      IF cBuntTxt = "" THEN
          ASSIGN cBuntTxt = "Overf. fra varetelling " + STRING(TODAY) + STRING(TIME,"HH:MM").
/*   run LagreOverforinger. */

      RUN SkapaTT_OvBuffer.
      IF CAN-FIND(FIRST TT_OvBuffer) THEN DO:
          RUN LagraOvBuffer.p (INPUT-OUTPUT iBuntNr,
                               ArtBas.ArtikkelNr,
                               cBuntTxt,
                               wEDB-System,
                               wTabell,
                               iOpphav).
          EMPTY TEMP-TABLE TT_OvBuffer NO-ERROR.
      END.
  /*{swn.i}*/ 
  END.    
/*   ELSE DO:                                   */
/*       MESSAGE "Lagring avbrutt"              */
/*           VIEW-AS ALERT-BOX INFO BUTTONS OK. */
/*       RETURN NO-APPLY.                       */
/*   END.                                       */

  IF iBuntNr <> ? THEN
  DO:
      MESSAGE 
          "Telleliste vellyket overført til overføringsordre med ordrenr: " + string(iBuntNr) + "." SKIP
          "For å oppdatere lager og statistikker må overføringsordren også oppdateres." SKIP
          "Dette gjøres i overføringsmodulen under overføringsordre."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.

  END.
  ELSE DO:
      MESSAGE "Opprettelse av overføringsordre feilet. Kontakt systemansvarlig."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppdatTellehode C-Win 
PROCEDURE OppdatTellehode :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER wToday AS DATE.
   
  DEF VAR piAntallPar  AS DEC  NO-UNDO.
  DEF VAR piAntallTalt AS DEC  NO-UNDO.
  DEF VAR piOpptVerdi  AS DEC  NO-UNDO.
  DEF VAR piVerdiDiff  AS DEC  NO-UNDO.
  DEF VAR piAntallDiff AS DEC  NO-UNDO.
  DEF VAR piOpprVerdi  AS DEC  NO-UNDO.
  DEF VAR piAntLinjer  AS DEC  NO-UNDO.
  DEF VAR plOppdatert  AS DATE NO-UNDO.

  DEF BUFFER bTelleHode FOR TelleHode.

  {sww.i}
  DO WITH FRAME DEFAULT-FRAME:
    FIND bTelleHode NO-LOCK WHERE
         ROWID(bTelleHode) = rowid(TelleHode).
    ASSIGN
      plOppdatert  = bTelleHode.Oppdatert
      plOppdatert  = IF wToday <> ? 
                       THEN wToday
                       ELSE plOppdatert
      piAntallPar  = 0
      piAntallTalt = 0
      piOpptVerdi  = 0
      piVerdiDiff  = 0
      piAntallDiff = 0
      piOpprVerdi  = 0
      piAntLinjer  = 0.

    FOR EACH TelleLinje OF bTelleHode NO-LOCK:
      ASSIGN
        piAntallPar  = piAntallPar  + TelleLinje.AntallPar
        piAntallTalt = piAntallTalt + TelleLinje.AntallTalt
        piOpprVerdi  = piOpprVerdi  + TelleLinje.OpprVerdi      
        piOpptVerdi  = piOpptVerdi  + TelleLinje.OpptVerdi
        piAntLinjer  = piAntLinjer  + 1.      

      IF wNedSkriv THEN
        DO:
          ASSIGN
            piVerdiDiff  = piOpprVerdi  - piOpptVerdi
            piAntallDiff = piAntallPar  - piAntallTalt.      
        END.
      ELSE DO:
        ASSIGN
          piVerdiDiff  = piOpprVerdi  - piOpptVerdi
          piAntallDiff = piAntallPar  - piAntallTalt.      
      END.

    END.
    DO TRANSACTION:
        FIND bTelleHode EXCLUSIVE-LOCK WHERE
          ROWID(bTelleHode) = rowid(TelleHode).
        ASSIGN
            bTelleHode.Oppdatert  = wToday
            bTelleHode.AntallPar  = piAntallPar  
            bTelleHode.AntallTalt = piAntallTalt 
            bTelleHode.OpptVerdi  = piOpptVerdi  
            bTelleHode.VerdiDiff  = piVerdiDiff  
            bTelleHode.AntallDiff = piAntallDiff 
            bTelleHode.OpprVerdi  = piOpprVerdi  
            bTelleHode.AntLinjer  = piAntLinjer.
            .
        RELEASE bTelleHode.
    END.
  END.
  {swn.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrBut C-Win 
PROCEDURE PrBut :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME DEFAULT-FRAME:
ASSIGN wTekst = "".
DO wLoop = 1 TO NUM-ENTRIES(TelleHode.ButikkListe):
  FIND Butiker NO-LOCK WHERE
    Butiker.Butik = int(ENTRY(wLoop,TelleHode.ButikkListe)) NO-ERROR.
  IF AVAILABLE Butiker THEN 
    DO:
      ASSIGN
        wTekst = wTekst + 
                 (IF wTekst = "" 
                    THEN ""
                    ELSE ",") + 
                 string(Butiker.Butik,"zzzzz9") + ": " +
                 Butiker.ButNamn.
    END.
END.
ASSIGN
  CB-Butikk:List-Items = wTekst
  CB-Butikk            = ENTRY(1,wTekst)
  wButik               = INT(ENTRY(1,CB-Butikk,":")).
END.  
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
    IF CAN-DO("Prev,Next",cRettning) THEN DO WITH FRAME {&FRAME-NAME}:
        CASE cRettning:
            WHEN "Prev" THEN
                BROWSE {&BROWSE-NAME}:SELECT-PREV-ROW( ).
            WHEN "Next" THEN
                BROWSE {&BROWSE-NAME}:SELECT-NEXT-ROW( ).
        END CASE.
        IF Tellelinje.Artikkelnr = 0 OR NOT CAN-FIND(ArtBas WHERE ArtBas.ArtikkelNr
                                      = Tellelinje.Artikkelnr) THEN
          RETURN.
        FIND ArtBas WHERE ArtBas.ArtikkelNr = Tellelinje.Artikkelnr NO-LOCK.

        PUBLISH "ByttArtikkel" (ArtBas.ArtikkelNr).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RefreshBrowser C-Win 
PROCEDURE RefreshBrowser :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER wRowId AS ROWID NO-UNDO.
  
  FIND TelleLinje NO-LOCK WHERE
    ROWID(TelleLinje) = wRowId NO-ERROR.
  IF AVAILABLE TelleLinje THEN
    DO WITH FRAME DEFAULT-FRAME:
      {&OPEN-QUERY-{&BROWSE-NAME}}
      FIND TelleLinje NO-LOCK WHERE
        ROWID(TelleLinje) = wRowId NO-ERROR.

      REPOSITION BROWSE-TelleLinje TO ROWID ROWID({&br-tabell}) NO-ERROR.
      APPLY "ENTRY" TO BROWSE BROWSE-TelleLinje.
      RETURN "Ok".
    END.
  ELSE RETURN "AVBRYT".
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
    IF wSortCol:DATA-TYPE = "INTEGER" OR wSortCol:DATA-TYPE = "DECIMAL" THEN
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
    DEF VAR wW AS widget NO-UNDO.
  
    RUN lockwindowupdate(FRAME {&FRAME-NAME}:hwnd).

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
    
    RUN lockwindowupdate(0).
    
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
  DEF VAR wWhere AS CHAR NO-UNDO.
  DEFINE VARIABLE cQString2 AS CHARACTER  NO-UNDO.
  RUN SettWhereSats (OUTPUT wWhere).
   
/*   assign                                    */
/*       wAktivQString = wSortCol:PRIVATE-DATA */
/*       .                                     */
/*   IF NOT wAktivQString MATCHES "*" + wWhere + "*" THEN         */
/*     wAktivQString = REPLACE(wAktivQString, "NO-LOCK", wWhere). */
  wAktivQString = get-qry("NY").
  cQString2 = wAktivQString.
   /* wQ:QUERY-PREPARE(wSortCol:PRIVATE-DATA). */
/*   IF cFilterStr <> "" THEN DO:                                                   */
/*       IF cQstring2 MATCHES "* USE-INDEX *" THEN                                  */
/*           cQString2 = REPLACE(cQString2,"USE-INDEX", cFilterStr + " USE-INDEX"). */
/*       ELSE IF cQstring2 MATCHES "* BY *" THEN                                    */
/*           cQString2 = REPLACE(cQString2,"BY", cFilterStr + " BY").               */
/*   END.                                                                           */
   wQ:QUERY-PREPARE(cQString2).
   wQ:QUERY-OPEN().
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
        IF wBlank = ? THEN
          REPOSITION {&BROWSE-NAME} TO ROWID ROWID(b{&br-tabell}) NO-ERROR.
        ASSIGN wAktivFillIn:SCREEN-VALUE = "".
        APPLY "ENTRY" TO BROWSE {&BROWSE-NAME}.
    END.
    RETURN NO-APPLY.
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
                                "ProgressBar,RECT-3").
DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,
                                FRAME DEFAULT-FRAME:HANDLE,
                                "ProgressBar,RECT-1,RECT-2,RECT-3").
DYNAMIC-FUNCTION("setAddMoveX",THIS-PROCEDURE:CURRENT-WINDOW,
                                FRAME DEFAULT-FRAME:HANDLE,
"B-1,B-2,B-3,B-Negative,B-Positive,B-Pris,B-Rabatt,B-Tomme,B-bygg,B-Fil,B-HTFil,B-HTFil-2,B-Oppdat,Btn_Help-2,Btn_OK,T-Avangsert,ProgressBar,RECT-3").


/* DYNAMIC-FUNCTION("SetResizeADM2panel",TRUE). */
DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,iWidthPix,iHeightPix,0,300).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettButtons C-Win 
PROCEDURE SettButtons :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /* Knappene er utilgjengelige hvis tellingen er oppdtert. */
  IF TelleHode.Oppdatert = ? THEN
    DO WITH FRAME DEFAULT-FRAME:
      IF wNedSkriv THEN
        DO:
        IF BROWSE-TelleLinje:NUM-SELECTED-ROWS >= 0 THEN
            ASSIGN
              B-1:sensitive     = FALSE 
              B-2:sensitive     = FALSE 
              B-3:sensitive     = TRUE
              B-Tomme:sensitive = TRUE
              B-Negative:sensitive = TRUE
              B-Positive:sensitive = TRUE.
          ELSE
            ASSIGN
              B-1:sensitive     = FALSE 
              B-2:sensitive     = FALSE 
              B-3:sensitive     = FALSE
              B-Tomme:sensitive = FALSE
              B-Negative:sensitive = FALSE
              B-Positive:sensitive = FALSE.
        END.
      ELSE DO:
        IF BROWSE-TelleLinje:NUM-SELECTED-ROWS >= 0 THEN
          ASSIGN
            B-1:sensitive     = TRUE 
            B-2:sensitive     = TRUE 
            B-3:sensitive     = TRUE
            B-Tomme:sensitive = TRUE
            B-Negative:sensitive = TRUE
            B-Positive:sensitive = TRUE.
        ELSE
          ASSIGN
            B-1:sensitive     = FALSE 
            B-2:sensitive     = FALSE 
            B-3:sensitive     = FALSE
            B-Tomme:sensitive = FALSE
            B-Negative:sensitive = FALSE
            B-Positive:sensitive = FALSE.
      END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettParentHandle C-Win 
PROCEDURE SettParentHandle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF INPUT PARAMETER ipParentHandle AS HANDLE NO-UNDO.
  
  ASSIGN
    wParentHandle = ipParentHandle.

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
  DEF OUTPUT PARAMETER wWhere AS CHAR NO-UNDO.
  DEFINE VARIABLE cTxt AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTxtOperator AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iNr  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cTmp AS CHARACTER  NO-UNDO.
  DO WITH FRAME F-Filter:
      ASSIGN cTxt = FI-Varetekst:SCREEN-VALUE
             iNr  = INPUT FI-LevNr.
  END.
  IF cTxt <> "" OR iNr > 0 THEN DO:
      IF TRIM(cTxt) <> "" THEN DO:
          IF NUM-ENTRIES(cTxt,"*") > 1 THEN
              ASSIGN cTxt = "*" + TRIM(cTxt,"*") + "*"
                     cTxtOperator = "MATCHES ".
          ELSE
              cTxtOperator = "BEGINS ".
          ASSIGN cTxt = "'" + cTxt + "'"
                 cTmp = "Tellelinje.beskr " + cTxtOperator + cTxt.
      END.
      IF iNr > 0 THEN DO:
          IF cTmp = "" THEN
              ASSIGN cTmp = "Tellelinje.Levnr = " + STRING(iNr).
          ELSE
              ASSIGN cTmp = cTmp + " and Tellelinje.Levnr = " + STRING(iNr).
      END.
  END.
  ASSIGN
      wWhere = (IF wWhere = "" THEN "" ELSE "and ") + 
               "no-lock where Tellelinje.TelleNr = '" + string(TelleHode.TelleNr) + "'"
      .
  ASSIGN cFilterStr = IF cTmp <> "" THEN " and " + cTmp + " " ELSE "".
/*
  if frame DEFAULT-FRAME:visible = false then
    return.

  FRAME-SCOOP:
  do with frame DEFAULT-FRAME:
  
  /* Bygger where sats */
  assign
/*
    wWhere = (if input CB-Type <> wAlle
               then (if wWhere = "" then "" else "and ") + "Kund.TypeId = " + entry(1,input CB-Type,":")
               else "") 

    wWhere = wWhere + " " +
             (if input CB-Gruppe <> wAlle
               then (if wWhere = "" then "" else "and ") + "VarGr.Hg = " + entry(1,input CB-Gruppe,":")
               else "")
    wWhere = wWhere + " " +
             (if input FI-Navn <> "*"
               then (if wWhere = "" then "" else "and ") + 
                     (if substring(input FI-Navn,1,1) = "*"
                       then "VarGHr.VgBeskr matches '"
                       else "VarGHr.VgBeskr begins '") + 
                     input FI-Navn + 
                     (if substring(input FI-Navn,1,1) = "*"
                       then "*"
                       else "") + "'"
               else "")
*/
    wWhere = if wWhere <> "" then "NO-LOCK where " + wWhere else "".
    
  end. /* FRAME SCOOP */       
*/
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
    FOR EACH TelleLinje OF TelleHode EXCLUSIVE-LOCK WHERE
      TelleLinje.Oppdatert = FALSE
      BREAK
      BY TelleLinje.TelleNr
      BY TelleLinje.Vg
      BY TelleLinje.LopNr
      BY TelleLinje.Butik 
      BY TelleLinje.Storl TRANSACTION:

      IF FIRST-OF(TelleLinje.Butik) THEN
        FIND Butiker NO-LOCK WHERE
          Butiker.Butik = TelleLinje.Butik NO-ERROR.

      ASSIGN
        TelleLinje.Oppdatert = TRUE.      

        FIND Lager NO-LOCK WHERE
             Lager.ArtikkelNr = TelleLinje.ArtikkelNr AND
             Lager.Butik      = TelleLinje.Butik NO-ERROR.
        CREATE TT_OvBuffer.
        ASSIGN TT_OvBuffer.BuntNr      = 999 /* dummy, kan vara vad som helst */
               TT_OvBuffer.LinjeNr     = iLinjeNr
               TT_OvBuffer.ButikkNrFra = TelleLinje.Butik
               TT_OvBuffer.ButikkNrTil = TelleHode.Tilbutikk        
               TT_OvBuffer.ArtikkelNr  = TelleLinje.ArtikkelNr
               TT_OvBuffer.Vg          = TelleLinje.vg   
               TT_OvBuffer.LopNr       = TelleLinje.LopNr
               TT_OvBuffer.Antall      = TelleLinje.AntallTalt
               TT_OvBuffer.Merknad     = "Varetelling"
               TT_OvBuffer.Storl       = REPLACE(REPLACE(TelleLinje.Storl,"(",""),")","")
               TT_OvBuffer.TilStorl    = TelleLinje.Storl
               TT_OvBuffer.Varekost    = TelleLinje.VVAreKost
               iLinjeNr                = iLinjeNr + 1.
    END.

    RUN OppdatTelleHode (INPUT TODAY).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettLinje C-Win 
PROCEDURE SlettLinje :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER wSlettIBrowse AS INT NO-UNDO.
  
  DEF VAR wArtikkelNr  LIKE ArtBas.ArtikkelNr NO-UNDO.
  DEF VAR wButik       AS INT  NO-UNDO. 

  DEF BUFFER bTelleLinje FOR TelleLinje.
  DEF BUFFER bKonvReg    FOR KonvReg.

DO FOR bTelleLinje, bKonvReg TRANSACTION
  WITH FRAME DEFAULT-FRAME: 

  /* Leser av verdiene for aktiv tellelinje */
  ASSIGN
    wArtikkelNr = TelleLinje.ArtikkelNr
    wButik      = TelleLinje.Butik.

  /* Tar bort tellelinjen */
  FIND bTelleLinje EXCLUSIVE-LOCK WHERE
    RECID(bTelleLinje) = recid(TelleLinje) NO-ERROR.
  DELETE bTelleLinje.
  
  IF wSlettIBrowse = 1 THEN
    wOk = BROWSE-TelleLinje:DELETE-CURRENT-ROW( ).
    
  /* Sjekker om låsen skal slettes for andre typer av tellinger.      */
  /* Finnes det ikke flere tellelinjer igjen på artikkelen og butikk, */
  /* skal tellelåsen slettes.                                         */
  IF wNedskriv = FALSE THEN
    DO:
      IF NOT CAN-FIND(FIRST bTelleLinje WHERE
        bTelleLinje.TelleNr    = TelleHode.TelleNr AND
        bTelleLinje.ArtikkelNr = wArtikkelNr AND
        bTelleLinje.Butik      = wButik) THEN
        DO:
          FIND FIRST bKonvReg EXCLUSIVE-LOCK WHERE
            bKonvReg.EDB-System = wEDB-System AND
            bKonvReg.Tabell     = wTabell     AND
            bKonvReg.EkstId     = STRING(wArtikkelNr) + "," + 
                                 string(wButik) AND
            bKonvReg.InterntId  = STRING(wArtikkelNr) + "," + 
                                 string(wButik) NO-ERROR.
          IF AVAILABLE bKonvReg THEN
            DELETE bKonvReg.
        END.     
    END.      
  /* Sletter låsen for nedskrivning. */
  /* Her finnes kun en linje pr. artikkel og butikk. */
  ELSE DO:
      FIND FIRST bKonvReg EXCLUSIVE-LOCK WHERE
        bKonvReg.EDB-System = wEDB-System AND
        bKonvReg.Tabell     = wTabell     AND
        bKonvReg.EkstId     = STRING(wArtikkelNr) + "," + 
                             string(wButik) AND
        bKonvReg.InterntId  = STRING(wArtikkelNr) + "," + 
                             string(wButik) NO-ERROR.
      IF AVAILABLE bKonvReg THEN
        DELETE bKonvReg.
  END.
  
END. /* TRANSACTION */
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
    FIND b{&br-tabell} WHERE ROWID(b{&br-tabell}) = ROWID({&br-tabell}) NO-LOCK NO-ERROR.
    wSortCol:PRIVATE-DATA = "DESCENDING".
    RUN SD-QUERY-OPEN.
    RUN Move-Fill-To-Top.
    RUN LabelColor.
    RUN SD-Reposition.

    RETURN NO-APPLY.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Suppler C-Win 
PROCEDURE Suppler :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE BUFFER bTellelinje FOR TelleLinje.
    DEFINE VARIABLE hh AS HANDLE     NO-UNDO.
    DEFINE VARIABLE bOK AS LOGICAL    NO-UNDO.
    {sww.i}
    EMPTY TEMP-TABLE TT_Suppler.
    FOR EACH TelleLinje OF TelleHode NO-LOCK BREAK BY TelleLinje.Artikkelnr.
        IF FIRST-OF(TelleLinje.Artikkelnr) THEN DO:
            CREATE TT_Suppler.
            ASSIGN TT_Suppler.Artikkelnr = Tellelinje.artikkelnr
                   TT_Suppler.Vg         = Tellelinje.Vg.
            RELEASE TT_Suppler.
        END.
    END.
    hh = BUFFER TT_Suppler:HANDLE:TABLE-HANDLE.
    bOK = DYNAMIC-FUNCTION("RunProc","art_to_telling.p",STRING(TelleHode.TelleNr),hh).
    {swn.i}
    {&OPEN-QUERY-{&BROWSE-NAME}}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Teller C-Win 
PROCEDURE Teller :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piAntLinjer AS INT NO-UNDO.

  IF piAntLinjer > chProgressBar:MAX THEN
      piAntLinjer = chProgressBar:MAX.
  IF piAntLinjer < 1 THEN
      piAntLinjer = 1.

  IF piAntLinjer >= 5 THEN
  DO:
      ASSIGN
          chProgressBar:Value = piAntLinjer
          .
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FiksStorl C-Win 
FUNCTION FiksStorl RETURNS CHARACTER
  ( INPUT wStorl AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  Formaterer størrelsen korrekt etter SkoTex standard.
    Notes:  
------------------------------------------------------------------------------*/

 ASSIGN
    wStorl = TRIM(wStorl)
    wStorl = CAPS(wStorl)
    wStorl = IF (LENGTH(wStorl) = 1 OR 
                 LENGTH(wStorl) = 3
                 ) 
                then " " + wStorl
                else wStorl.          

  /* Bytter ut eventuelle comma med punkt. */
  IF INDEX(wStorl,",") <> 0 THEN
    OVERLAY(wStorl, INDEX(wStorl,","), 1, "CHARACTER") = ".".

  RETURN wStorl.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fLockVindu C-Win 
FUNCTION fLockVindu RETURNS CHARACTER
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-qry C-Win 
FUNCTION get-qry RETURNS CHARACTER
  ( INPUT cType AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE wWhere AS CHAR NO-UNDO.
  DEFINE VARIABLE cTxt AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTxtOperator AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iNr  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cTmp AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cQstr AS CHARACTER  NO-UNDO.
  IF wSortCol:NAME <> "VgLopNr" THEN DO:
      IF cType = "NY" THEN
          wSortCol:PRIVATE-DATA = "".
      ELSE DO:
          wSortCol:PRIVATE-DATA = STRING(wSortCol:PRIVATE-DATA = "","DESCENDING/").
      END.
  END.
  DO WITH FRAME F-Filter:
      ASSIGN cTxt = FI-Varetekst:SCREEN-VALUE
             iNr  = INPUT FI-LevNr.
  END.
  IF cTxt <> "" OR iNr > 0 THEN DO:
      IF TRIM(cTxt) <> "" THEN DO:
          IF NUM-ENTRIES(cTxt,"*") > 1 THEN
              ASSIGN cTxt = "*" + TRIM(cTxt,"*") + "*"
                     cTxtOperator = "MATCHES ".
          ELSE
              cTxtOperator = "BEGINS ".
          ASSIGN cTxt = "'" + cTxt + "'"
                 cTmp = "Tellelinje.beskr " + cTxtOperator + cTxt.
      END.
      IF iNr > 0 THEN DO:
          IF cTmp = "" THEN
              ASSIGN cTmp = "Tellelinje.Levnr = " + STRING(iNr).
          ELSE
              ASSIGN cTmp = cTmp + " and Tellelinje.Levnr = " + STRING(iNr).
      END.
      cTmp = " and " + cTmp.
  END.
  
  cQstr = "FOR EACH Tellelinje NO-LOCK WHERE Tellelinje.tellenr = " + STRING(TelleHode.tellenr) + cTmp.
          
  IF wSortCol:NAME = "VgLopNr" THEN DO:
      cQstr = cQstr + " USE-INDEX VgLopNr INDEXED-REPOSITION".
  END.
  ELSE DO:
      cQstr = cQstr + " BY Tellelinje." + wSortCol:NAME + " " + TRIM(wSortCol:PRIVATE-DATA)  + " INDEXED-REPOSITION".
  END.
  RETURN cQstr.   /* Function return value. */

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

