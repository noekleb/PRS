&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
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

DEF VAR bOk               AS LOG  NO-UNDO.
DEF VAR ix                AS INT  NO-UNDO.
DEF VAR iReturn           AS INT  NO-UNDO.
                          
DEF VAR hFieldMap         AS HANDLE NO-UNDO.
DEF VAR hFieldMap-2       AS HANDLE NO-UNDO.
DEF VAR hBrowse           AS HANDLE NO-UNDO.
DEF VAR hBrowse-2         AS HANDLE NO-UNDO.
DEF VAR hQuery            AS HANDLE NO-UNDO.
DEF VAR hQuery-2          AS HANDLE NO-UNDO.
DEF VAR hBrwPrisko        AS HANDLE NO-UNDO.

DEF VAR hToolBar          AS HANDLE NO-UNDO.
DEF VAR hToolBar-2        AS HANDLE NO-UNDO.
DEF VAR hWinToolBar       AS HANDLE NO-UNDO.
DEF VAR hParent           AS HANDLE NO-UNDO.

DEF VAR iArtikkelNr       AS DEC NO-UNDO.
DEF VAR iArtikkelNrOld    AS DEC NO-UNDO.

DEF VAR hbcChkStrekNy     AS HANDLE NO-UNDO.
DEF VAR hbfChkStrekNy     AS HANDLE NO-UNDO.
DEF VAR hbcChkStrekFeil   AS HANDLE NO-UNDO.
DEF VAR hbfChkStrekFeil   AS HANDLE NO-UNDO.
DEF VAR cTekst            AS CHAR NO-UNDO. 

DEF VAR hArtBasSok        AS HANDLE NO-UNDO.

DEF VAR plMvaKr           AS DEC NO-UNDO.
DEF VAR plDbKr            AS DEC NO-UNDO.
DEF VAR plVarekost        AS DEC NO-UNDO.
DEF VAR iBrukerType      AS INT    NO-UNDO.
DEF VAR cBrukerId        AS CHAR   NO-UNDO.
DEF VAR iArtPrisProfilNr  AS INT    NO-UNDO.


DEF TEMP-TABLE ttWidget NO-UNDO
  FIELD cField AS CHAR
  FIELD hField AS HANDLE
  FIELD cType  AS CHAR
  FIELD bVPI   AS LOG
  INDEX cField IS PRIMARY cField
  INDEX bVPI   bVPI
  .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Grunnsortiment btnCalLevDato1 Jamforenhet ~
LokPris-2 LokPris VPIBildeKode-2 VPIBildeKode Katalogpris#1 ForhRab%#1 ~
suppRab%#1 btnProdusent-2 AnbefaltPris ProfilNr btnProdusent-3 ProfilNr-2 ~
btnProdusent btnProdusent-5 Grunnsortiment-2 btnProdusent-4 btnCalLevDato2 ~
btnCalLevDato3 btnCalLevDato4 B-SokLinkvare VgBeskr-2 VmBeskrivelse-2 ~
VmBeskrivelse btnStrType rectToolbar rectWinToolbar rectBrowse RECT-1 ~
RECT-2 RECT-3 RECT-4 rectToolbar-2 RECT-5 RECT-6 brwPrisko 
&Scoped-Define DISPLAYED-OBJECTS Grunnsortiment ArtikkelNr-2 ArtikkelNr ~
Beskr-2 Beskr Bongtekst Bongtekst-2 LevKod-2 LevKod Etikett Etikett-2 ~
LevFargKod-2 LevFargKod Etikettekst1 Etikettekst1-2 Vg-2 Vg LinkVareNr ~
LinkVareNr-2 LevNr-2 LevNr StrTypeId ProdNr-2 VmId-2 ProdNr VmId LevDato1-2 ~
Jamforenhet-2 LevDato1 Jamforenhet LevDato2-2 Mengde-2 LevDato2 Mengde ~
LevDato3-2 Lager-2 LevDato3 Lager LevDato4-2 LokPris-2 LevDato4 LokPris ~
VPIBildeKode-2 VPIDato-2 VPIDato VPIBildeKode SaSong-2 SaSong Katalogpris#1 ~
ForhRab%#1 Varekost suppRab%#1 suppVareKost AnbefaltPris Innkjopspris#1 ~
ProfilNr Katalogpris-2 Innkjopspris#1-2 ProfilNr-2 ForhRab%-2 Rab1%#1 ~
Rab1%#1-2 Varekost-2 Varekost#1 Varekost#1-2 suppRab%-2 DB%#1 DB%#1-2 ~
suppVareKost-2 Mva%#1 Mva%#1-2 AnbefaltPris-2 Pris#1 Pris#1-2 ~
Grunnsortiment-2 StrTypeId-2 txtVPIartbas txtArtikkel VgBeskr-2 VgBeskr ~
LevNamn-2 LevNamn Beskrivelse-2 VmBeskrivelse-2 VmBeskrivelse Beskrivelse ~
Tilbud Sasbeskr-2 Sasbeskr lblTekst1 lblTekst-2 FI-JamforPris1 ~
FI-JamforPris2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AddStr C-Win 
FUNCTION AddStr RETURNS LOGICAL
  ( INPUT ifArtikkelNr AS DEC,
    INPUT icStorl      AS CHAR,
    INPUT ifPlukkAnt   AS DEC,
    input icAction     AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AdjustBrowseColumns C-Win 
FUNCTION AdjustBrowseColumns RETURNS LOGICAL
  ( INPUT ihBrowse AS HANDLE,
    INPUT icBrowseName AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD buildWidgetTable C-Win 
FUNCTION buildWidgetTable RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setColour C-Win 
FUNCTION setColour RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-SokLinkvare 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnBeskr 
     LABEL "..." 
     SIZE 4 BY 1 TOOLTIP "Søk artikkelregister for treff".

DEFINE BUTTON btnCalLevDato1 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 3.8 BY 1.

DEFINE BUTTON btnCalLevDato2 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 3.8 BY 1.

DEFINE BUTTON btnCalLevDato3 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 3.8 BY 1.

DEFINE BUTTON btnCalLevDato4 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 3.8 BY 1.

DEFINE BUTTON btnLevFargKod 
     LABEL "..." 
     SIZE 4 BY 1 TOOLTIP "Søk artikkelregister for treff".

DEFINE BUTTON btnLevKod 
     LABEL "..." 
     SIZE 4 BY 1 TOOLTIP "Søk artikkelregister for treff".

DEFINE BUTTON btnProdusent 
     LABEL "..." 
     SIZE 4 BY 1.

DEFINE BUTTON btnProdusent-2 
     LABEL "..." 
     SIZE 4 BY 1.

DEFINE BUTTON btnProdusent-3 
     LABEL "..." 
     SIZE 4 BY 1.

DEFINE BUTTON btnProdusent-4 
     LABEL "..." 
     SIZE 4 BY 1.

DEFINE BUTTON btnProdusent-5 
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnStrType 
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE Etikett AS INTEGER FORMAT "9":U INITIAL 0 
     LABEL "Etikett" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "item1",1
     DROP-DOWN-LIST
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE Etikett-2 AS INTEGER FORMAT "9":U INITIAL 0 
     LABEL "Etikett" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "item1",1
     DROP-DOWN-LIST
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE Jamforenhet AS CHARACTER FORMAT "X(256)":U 
     LABEL "Jamførenhet" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "<Ikke angitt>"," ",
                     "Stykk (Stk)","stk",
                     "Kilo (Kg)","kg",
                     "Hekto (Hg)","hg",
                     "Meter (m)","m",
                     "Kvadratmeter (m2)","m2",
                     "Liter (l)","l"
     DROP-DOWN-LIST
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE Jamforenhet-2 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Jamførenhet" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "<Ikke angitt>"," ",
                     "Stykk (Stk)","stk",
                     "Kilo (Kg)","kg",
                     "Hekto (Hg)","hg",
                     "Meter (m)","m",
                     "Kvadratmeter (m2)","m2",
                     "Liter (l)","l"
     DROP-DOWN-LIST
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE AnbefaltPris AS DECIMAL FORMAT "->>>,>>9.99" INITIAL 0 
     LABEL "Veil. pris" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1.

DEFINE VARIABLE AnbefaltPris-2 AS DECIMAL FORMAT "->>>,>>9.99" INITIAL 0 
     LABEL "Veil. pris" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1.

DEFINE VARIABLE ArtikkelNr AS DECIMAL FORMAT "zzzzzzzzzzzz9" INITIAL 0 
     LABEL "Artikkelnummer" 
     VIEW-AS FILL-IN 
     SIZE 20.2 BY 1.

DEFINE VARIABLE ArtikkelNr-2 AS DECIMAL FORMAT "zzzzzzzzzzzz9" INITIAL 0 
     LABEL "Artikkelnummer" 
     VIEW-AS FILL-IN 
     SIZE 20.2 BY 1.

DEFINE VARIABLE Beskr AS CHARACTER FORMAT "x(256)" 
     LABEL "Beskrivelse" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1.

DEFINE VARIABLE Beskr-2 AS CHARACTER FORMAT "x(256)" 
     LABEL "Beskrivelse" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1.

DEFINE VARIABLE Beskrivelse AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE Beskrivelse-2 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE Bongtekst AS CHARACTER FORMAT "X(256)":U 
     LABEL "Bongtekst" 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE Bongtekst-2 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Bongtekst" 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE DB%#1 AS DECIMAL FORMAT "->>>,>>9.99" INITIAL 0 
     LABEL "Dekn.Bid%" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1.

DEFINE VARIABLE DB%#1-2 AS DECIMAL FORMAT "->>>,>>9.99" INITIAL 0 
     LABEL "Dekn.Bid%" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1.

DEFINE VARIABLE Etikettekst1 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Etikettekst1" 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE Etikettekst1-2 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Etikettekst1" 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE FI-JamforPris1 AS DECIMAL FORMAT "->>>,>>9.99" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1.

DEFINE VARIABLE FI-JamforPris2 AS DECIMAL FORMAT "->>>,>>9.99" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1.

DEFINE VARIABLE ForhRab%#1 AS DECIMAL FORMAT "->>>,>>9.99" INITIAL 0 
     LABEL "Forh.rabatt%" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1.

DEFINE VARIABLE ForhRab%-2 AS DECIMAL FORMAT "->>>,>>9.99" INITIAL 0 
     LABEL "Forh.rabatt%" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1.

DEFINE VARIABLE Innkjopspris#1 AS DECIMAL FORMAT "->>>,>>9.99" INITIAL 0 
     LABEL "Innkjøpspris" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1.

DEFINE VARIABLE Innkjopspris#1-2 AS DECIMAL FORMAT "->>>,>>9.99" INITIAL 0 
     LABEL "Innkjøpspris" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1.

DEFINE VARIABLE Katalogpris#1 AS DECIMAL FORMAT "->>>,>>9.99" INITIAL 0 
     LABEL "Engros" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1.

DEFINE VARIABLE Katalogpris-2 AS DECIMAL FORMAT "->>>,>>9.99" INITIAL 0 
     LABEL "Engros" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1.

DEFINE VARIABLE lblTekst-2 AS CHARACTER FORMAT "X(256)":U INITIAL "Normal kalkyle" 
      VIEW-AS TEXT 
     SIZE 14 BY .62 NO-UNDO.

DEFINE VARIABLE lblTekst1 AS CHARACTER FORMAT "X(256)":U INITIAL "Kalkyle" 
      VIEW-AS TEXT 
     SIZE 8 BY .62 NO-UNDO.

DEFINE VARIABLE LevDato1 AS DATE FORMAT "99/99/99" 
     LABEL "1. Leveringsdato" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE LevDato1-2 AS DATE FORMAT "99/99/99" 
     LABEL "1. Leveringsdato" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE LevDato2 AS DATE FORMAT "99/99/99" 
     LABEL "2. leveringsdato" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE LevDato2-2 AS DATE FORMAT "99/99/99" 
     LABEL "2. leveringsdato" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE LevDato3 AS DATE FORMAT "99/99/99" 
     LABEL "3. leveringsdato" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE LevDato3-2 AS DATE FORMAT "99/99/99" 
     LABEL "3. leveringsdato" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE LevDato4 AS DATE FORMAT "99/99/99" 
     LABEL "4. leveringsdato" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE LevDato4-2 AS DATE FORMAT "99/99/99" 
     LABEL "4. leveringsdato" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE LevFargKod AS CHARACTER FORMAT "X(256)":U 
     LABEL "Farge kode" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE LevFargKod-2 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Farge kode" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE LevKod AS CHARACTER FORMAT "x(30)" 
     LABEL "LevArtNr" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1.

DEFINE VARIABLE LevKod-2 AS CHARACTER FORMAT "x(30)" 
     LABEL "LevArtNr" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1.

DEFINE VARIABLE LevNamn AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE LevNamn-2 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 22.8 BY 1 NO-UNDO.

DEFINE VARIABLE LevNr AS INTEGER FORMAT "zzzzz9" INITIAL 0 
     LABEL "LevNr" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE LevNr-2 AS INTEGER FORMAT "zzzzz9" INITIAL 0 
     LABEL "LevNr" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE LinkVareNr AS DECIMAL FORMAT ">>>>>>>>>>>>9":U INITIAL 0 
     LABEL "Link til pant" 
     VIEW-AS FILL-IN 
     SIZE 20.2 BY 1 NO-UNDO.

DEFINE VARIABLE LinkVareNr-2 AS DECIMAL FORMAT ">>>>>>>>>>>>9":U INITIAL 0 
     LABEL "Link til pant" 
     VIEW-AS FILL-IN 
     SIZE 20.2 BY 1 NO-UNDO.

DEFINE VARIABLE Mengde AS DECIMAL FORMAT "->>9.999":U INITIAL 0 
     LABEL "Mengde i s.enhet/jamforpris" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE Mengde-2 AS DECIMAL FORMAT "->>9.999":U INITIAL 0 
     LABEL "Mengde i s.enhet/jamførpris" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE Mva%#1 AS DECIMAL FORMAT "->>>,>>9.99" INITIAL 0 
     LABEL "Mva%" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1.

DEFINE VARIABLE Mva%#1-2 AS DECIMAL FORMAT "->>>,>>9.99" INITIAL 0 
     LABEL "Mva%" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1.

DEFINE VARIABLE Pris#1 AS DECIMAL FORMAT "->>>,>>9.99" INITIAL 0 
     LABEL "Pris" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1.

DEFINE VARIABLE Pris#1-2 AS DECIMAL FORMAT "->>>,>>9.99" INITIAL 0 
     LABEL "Pris" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1.

DEFINE VARIABLE ProdNr AS INTEGER FORMAT "zzzzz9" INITIAL 0 
     LABEL "Produsent" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE ProdNr-2 AS INTEGER FORMAT "zzzzz9" INITIAL 0 
     LABEL "Produsent" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE ProfilNr AS INTEGER FORMAT "->>>>>>9":U INITIAL 0 
     LABEL "Profil" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE ProfilNr-2 AS INTEGER FORMAT "->>>>>>9":U INITIAL 0 
     LABEL "Profil" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE Rab1%#1 AS DECIMAL FORMAT "->>>,>>9.99" INITIAL 0 
     LABEL "Rabatt%1" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1.

DEFINE VARIABLE Rab1%#1-2 AS DECIMAL FORMAT "->>>,>>9.99" INITIAL 0 
     LABEL "Rabatt%1" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1.

DEFINE VARIABLE Sasbeskr AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE Sasbeskr-2 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 16.6 BY 1 NO-UNDO.

DEFINE VARIABLE SaSong AS INTEGER FORMAT "zz9" INITIAL 0 
     LABEL "Sesong" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE SaSong-2 AS INTEGER FORMAT "zz9" INITIAL 0 
     LABEL "Sesong" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE StrTypeId AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     LABEL "Str.type" 
     VIEW-AS FILL-IN 
     SIZE 20.2 BY 1 NO-UNDO.

DEFINE VARIABLE StrTypeId-2 AS INTEGER FORMAT ">>>>>>9":U INITIAL 0 
     LABEL "Str.type" 
     VIEW-AS FILL-IN 
     SIZE 20.2 BY 1 NO-UNDO.

DEFINE VARIABLE suppRab%#1 AS DECIMAL FORMAT "->>>,>>9.99" INITIAL 0 
     LABEL "Sup.Rabatt%" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1.

DEFINE VARIABLE suppRab%-2 AS DECIMAL FORMAT "->>>,>>9.99" INITIAL 0 
     LABEL "Sup.Rabatt%" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1.

DEFINE VARIABLE suppVareKost AS DECIMAL FORMAT "->>>,>>9.99" INITIAL 0 
     LABEL "Nto. Innpr. supp" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1.

DEFINE VARIABLE suppVareKost-2 AS DECIMAL FORMAT "->>>,>>9.99" INITIAL 0 
     LABEL "Nto. innpr. supp" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1.

DEFINE VARIABLE Tilbud AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 26 BY 1.38
     FGCOLOR 12 FONT 8 NO-UNDO.

DEFINE VARIABLE txtArtikkel AS CHARACTER FORMAT "X(256)":U INITIAL "Artikkel" 
      VIEW-AS TEXT 
     SIZE 9 BY .62 NO-UNDO.

DEFINE VARIABLE txtVPIartbas AS CHARACTER FORMAT "X(256)":U INITIAL "VPI Artbas" 
      VIEW-AS TEXT 
     SIZE 11 BY .62 NO-UNDO.

DEFINE VARIABLE Varekost AS DECIMAL FORMAT "->>>,>>9.99" INITIAL 0 
     LABEL "Nto Innpr. forh" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1.

DEFINE VARIABLE Varekost#1 AS DECIMAL FORMAT "->>>,>>9.99" INITIAL 0 
     LABEL "Varekost" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1.

DEFINE VARIABLE Varekost#1-2 AS DECIMAL FORMAT "->>>,>>9.99" INITIAL 0 
     LABEL "Varekost" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1.

DEFINE VARIABLE Varekost-2 AS DECIMAL FORMAT "->>>,>>9.99" INITIAL 0 
     LABEL "Nto. Innpr. forh" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1.

DEFINE VARIABLE Vg AS INTEGER FORMAT "zzzzz9" INITIAL 0 
     LABEL "VgNr" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE Vg-2 AS INTEGER FORMAT "zzzzz9" INITIAL 0 
     LABEL "VgNr" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE VgBeskr AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE VgBeskr-2 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 16.6 BY 1 NO-UNDO.

DEFINE VARIABLE VmBeskrivelse AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE VmBeskrivelse-2 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE VmId AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     LABEL "Varemerke" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE VmId-2 AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     LABEL "Varemerke" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE VPIBildeKode AS CHARACTER FORMAT "X(256)":U 
     LABEL "Bildereferanse" 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE VPIBildeKode-2 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Bildereferanse" 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE VPIDato AS DATE FORMAT "99/99/99" 
     LABEL "VpiDato" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE VPIDato-2 AS DATE FORMAT "99/99/99" 
     LABEL "VpiDato" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE RECTANGLE brwPrisko
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 84.4 BY 6.91.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 106 BY 23.1.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 99 BY 23.1.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 68.4 BY 7.38.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 58.8 BY 7.38.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 36.6 BY 7.38.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 38 BY 7.38.

DEFINE RECTANGLE rectBrowse
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 120.6 BY 6.91.

DEFINE RECTANGLE rectToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 13.6 BY .95.

DEFINE RECTANGLE rectToolbar-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 13.6 BY .95.

DEFINE RECTANGLE rectWinToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 8 BY .95.

DEFINE VARIABLE Grunnsortiment AS LOGICAL INITIAL no 
     LABEL "Grunnsortiment" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY .81 NO-UNDO.

DEFINE VARIABLE Grunnsortiment-2 AS LOGICAL INITIAL no 
     LABEL "Grunnsortiment" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY .81 NO-UNDO.

DEFINE VARIABLE Lager AS LOGICAL INITIAL no 
     LABEL "Lagerstyrt" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY .81 NO-UNDO.

DEFINE VARIABLE Lager-2 AS LOGICAL INITIAL no 
     LABEL "Lagerstyrt" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE LokPris AS LOGICAL INITIAL no 
     LABEL "Lokal pris" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY .81 NO-UNDO.

DEFINE VARIABLE LokPris-2 AS LOGICAL INITIAL no 
     LABEL "Lokal pris" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     Grunnsortiment AT ROW 16 COL 71
     ArtikkelNr-2 AT ROW 3.62 COL 125 COLON-ALIGNED
     ArtikkelNr AT ROW 3.86 COL 17 COLON-ALIGNED
     Beskr-2 AT ROW 4.81 COL 125 COLON-ALIGNED HELP
          "Kort beskrivelse av artikkelen"
     Beskr AT ROW 4.95 COL 17 COLON-ALIGNED HELP
          "Kort beskrivelse av artikkelen"
     Bongtekst AT ROW 4.95 COL 69 COLON-ALIGNED
     Bongtekst-2 AT ROW 4.95 COL 171 COLON-ALIGNED
     LevKod-2 AT ROW 5.86 COL 125 COLON-ALIGNED HELP
          "Leverandørens artikkelnummer"
     LevKod AT ROW 6 COL 17 COLON-ALIGNED HELP
          "Leverandørens artikkelnummer"
     Etikett AT ROW 6 COL 69 COLON-ALIGNED
     btnCalLevDato1 AT ROW 11.81 COL 32 NO-TAB-STOP 
     Etikett-2 AT ROW 6 COL 171 COLON-ALIGNED
     LevFargKod-2 AT ROW 6.91 COL 125 COLON-ALIGNED
     LevFargKod AT ROW 7.05 COL 17 COLON-ALIGNED
     Etikettekst1 AT ROW 7.05 COL 69 COLON-ALIGNED
     Etikettekst1-2 AT ROW 7.05 COL 171 COLON-ALIGNED
     Vg-2 AT ROW 8 COL 125 COLON-ALIGNED HELP
          "'varegruppenummer"
     Vg AT ROW 8.14 COL 17 COLON-ALIGNED HELP
          "'varegruppenummer"
     LinkVareNr AT ROW 8.14 COL 69 COLON-ALIGNED
     LinkVareNr-2 AT ROW 8.14 COL 171 COLON-ALIGNED
     LevNr-2 AT ROW 9.05 COL 125 COLON-ALIGNED HELP
          "Leverandørnummer"
     LevNr AT ROW 9.19 COL 17 COLON-ALIGNED HELP
          "Leverandørnummer"
     StrTypeId AT ROW 9.19 COL 69 COLON-ALIGNED
     ProdNr-2 AT ROW 10.57 COL 125 COLON-ALIGNED HELP
          "Produsent"
     VmId-2 AT ROW 10.57 COL 171 COLON-ALIGNED
     ProdNr AT ROW 10.71 COL 17 COLON-ALIGNED HELP
          "Produsent"
     VmId AT ROW 10.71 COL 69 COLON-ALIGNED
     LevDato1-2 AT ROW 11.62 COL 125 COLON-ALIGNED HELP
          "Første dato da varene forventes levert butikk/lager."
     Jamforenhet-2 AT ROW 11.62 COL 171 COLON-ALIGNED
     LevDato1 AT ROW 11.76 COL 17 COLON-ALIGNED HELP
          "Første dato da varene forventes levert butikk/lager."
     Jamforenhet AT ROW 11.76 COL 69 COLON-ALIGNED
     LevDato2-2 AT ROW 12.67 COL 125 COLON-ALIGNED HELP
          "Forventet dato for andre leveranse."
     btnBeskr AT ROW 4.95 COL 49 NO-TAB-STOP 
     Mengde-2 AT ROW 12.67 COL 171 COLON-ALIGNED
     LevDato2 AT ROW 12.81 COL 17 COLON-ALIGNED HELP
          "Forventet dato for andre leveranse."
     Mengde AT ROW 12.81 COL 69 COLON-ALIGNED
     LevDato3-2 AT ROW 13.76 COL 125 COLON-ALIGNED HELP
          "Forventet dato for tredje leveranse."
     Lager-2 AT ROW 14.81 COL 173
     LevDato3 AT ROW 13.91 COL 17 COLON-ALIGNED HELP
          "Forventet dato for tredje leveranse."
     Lager AT ROW 14.81 COL 71
     btnLevKod AT ROW 6 COL 49 NO-TAB-STOP 
     LevDato4-2 AT ROW 14.81 COL 125 COLON-ALIGNED HELP
          "Forventet dato for fjerde leveranse."
     LokPris-2 AT ROW 14.81 COL 189.8
     LevDato4 AT ROW 14.95 COL 17 COLON-ALIGNED HELP
          "Forventet dato for fjerde leveranse."
     LokPris AT ROW 14.81 COL 89.4
     VPIBildeKode-2 AT ROW 16.91 COL 171 COLON-ALIGNED
     btnLevFargKod AT ROW 7 COL 49 NO-TAB-STOP 
     VPIDato-2 AT ROW 15.86 COL 125 COLON-ALIGNED HELP
          "Dato da vpi ble importert"
     VPIDato AT ROW 16 COL 17 COLON-ALIGNED HELP
          "Dato da vpi ble importert"
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1.05
         SIZE 206.6 BY 33.24.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME DEFAULT-FRAME
     VPIBildeKode AT ROW 17 COL 69 COLON-ALIGNED
     SaSong-2 AT ROW 16.91 COL 125 COLON-ALIGNED HELP
          "Sesong"
     SaSong AT ROW 17 COL 17 COLON-ALIGNED HELP
          "Sesong"
     Katalogpris#1 AT ROW 18.86 COL 17 COLON-ALIGNED HELP
          "Anbefalt pris"
     ForhRab%#1 AT ROW 19.86 COL 17 COLON-ALIGNED HELP
          "Anbefalt pris"
     Varekost AT ROW 20.86 COL 17 COLON-ALIGNED HELP
          "Anbefalt pris"
     suppRab%#1 AT ROW 21.86 COL 17 COLON-ALIGNED HELP
          "Anbefalt pris"
     suppVareKost AT ROW 22.86 COL 17 COLON-ALIGNED HELP
          "Anbefalt pris"
     btnProdusent-2 AT ROW 8.14 COL 32 NO-TAB-STOP 
     AnbefaltPris AT ROW 23.86 COL 17 COLON-ALIGNED HELP
          "Anbefalt pris"
     Innkjopspris#1 AT ROW 18.86 COL 52 COLON-ALIGNED HELP
          "Anbefalt pris"
     ProfilNr AT ROW 18.86 COL 82 COLON-ALIGNED
     btnProdusent-3 AT ROW 9.14 COL 32 NO-TAB-STOP 
     Katalogpris-2 AT ROW 18.86 COL 125 COLON-ALIGNED HELP
          "Anbefalt pris"
     Innkjopspris#1-2 AT ROW 18.86 COL 160 COLON-ALIGNED HELP
          "Anbefalt pris"
     ProfilNr-2 AT ROW 18.86 COL 185.2 COLON-ALIGNED
     ForhRab%-2 AT ROW 19.81 COL 125 COLON-ALIGNED HELP
          "Anbefalt pris"
     Rab1%#1 AT ROW 19.86 COL 52 COLON-ALIGNED HELP
          "Anbefalt pris"
     Rab1%#1-2 AT ROW 19.86 COL 160 COLON-ALIGNED HELP
          "Anbefalt pris"
     Varekost-2 AT ROW 20.81 COL 125 COLON-ALIGNED HELP
          "Anbefalt pris"
     Varekost#1 AT ROW 20.86 COL 52 COLON-ALIGNED HELP
          "Anbefalt pris"
     Varekost#1-2 AT ROW 20.86 COL 160 COLON-ALIGNED HELP
          "Anbefalt pris"
     suppRab%-2 AT ROW 21.81 COL 125 COLON-ALIGNED HELP
          "Anbefalt pris"
     btnProdusent AT ROW 10.76 COL 32 NO-TAB-STOP 
     DB%#1 AT ROW 21.86 COL 52 COLON-ALIGNED HELP
          "Anbefalt pris"
     btnProdusent-5 AT ROW 10.76 COL 83 NO-TAB-STOP 
     DB%#1-2 AT ROW 21.86 COL 160 COLON-ALIGNED HELP
          "Anbefalt pris"
     suppVareKost-2 AT ROW 22.81 COL 125 COLON-ALIGNED HELP
          "Anbefalt pris"
     Mva%#1 AT ROW 22.86 COL 52 COLON-ALIGNED HELP
          "Anbefalt pris"
     Mva%#1-2 AT ROW 22.86 COL 160 COLON-ALIGNED HELP
          "Anbefalt pris"
     AnbefaltPris-2 AT ROW 23.81 COL 125 COLON-ALIGNED HELP
          "Anbefalt pris"
     Pris#1 AT ROW 23.86 COL 52 COLON-ALIGNED HELP
          "Anbefalt pris"
     Pris#1-2 AT ROW 23.86 COL 160 COLON-ALIGNED HELP
          "Anbefalt pris"
     Grunnsortiment-2 AT ROW 15.86 COL 173
     StrTypeId-2 AT ROW 9.19 COL 171 COLON-ALIGNED
     btnProdusent-4 AT ROW 16.95 COL 32 NO-TAB-STOP 
     btnCalLevDato2 AT ROW 12.91 COL 32 NO-TAB-STOP 
     btnCalLevDato3 AT ROW 13.95 COL 32 NO-TAB-STOP 
     btnCalLevDato4 AT ROW 15.05 COL 32 NO-TAB-STOP 
     B-SokLinkvare AT ROW 8.14 COL 91 NO-TAB-STOP 
     txtVPIartbas AT ROW 2.52 COL 2 COLON-ALIGNED NO-LABEL
     txtArtikkel AT ROW 2.67 COL 111 COLON-ALIGNED NO-LABEL
     VgBeskr-2 AT ROW 7.95 COL 138.4 COLON-ALIGNED NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1.05
         SIZE 206.6 BY 33.24.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME DEFAULT-FRAME
     VgBeskr AT ROW 8.14 COL 35 COLON-ALIGNED NO-LABEL
     LevNamn-2 AT ROW 9.05 COL 138.2 COLON-ALIGNED NO-LABEL
     LevNamn AT ROW 9.24 COL 34.4 COLON-ALIGNED NO-LABEL
     Beskrivelse-2 AT ROW 10.52 COL 138 COLON-ALIGNED NO-LABEL
     VmBeskrivelse-2 AT ROW 10.57 COL 183 COLON-ALIGNED NO-LABEL
     VmBeskrivelse AT ROW 10.71 COL 86 COLON-ALIGNED NO-LABEL
     Beskrivelse AT ROW 10.76 COL 34.4 COLON-ALIGNED NO-LABEL
     Tilbud AT ROW 14.1 COL 140 COLON-ALIGNED NO-LABEL
     Sasbeskr-2 AT ROW 16.95 COL 138.4 COLON-ALIGNED NO-LABEL
     Sasbeskr AT ROW 17.05 COL 34.4 COLON-ALIGNED NO-LABEL
     lblTekst1 AT ROW 18.14 COL 42 COLON-ALIGNED NO-LABEL
     lblTekst-2 AT ROW 18.14 COL 150 COLON-ALIGNED NO-LABEL
     btnStrType AT ROW 9.19 COL 91 NO-TAB-STOP 
     FI-JamforPris1 AT ROW 12.81 COL 80.4 COLON-ALIGNED HELP
          "Anbefalt pris" NO-LABEL WIDGET-ID 2
     FI-JamforPris2 AT ROW 12.67 COL 186 COLON-ALIGNED HELP
          "Anbefalt pris" NO-LABEL WIDGET-ID 4
     rectToolbar AT ROW 1.33 COL 2.4
     rectWinToolbar AT ROW 1.29 COL 198.4
     rectBrowse AT ROW 27.29 COL 1.6
     RECT-1 AT ROW 2.91 COL 1.4
     RECT-2 AT ROW 2.91 COL 108
     RECT-3 AT ROW 18.38 COL 38.6
     RECT-4 AT ROW 18.38 COL 147.2
     rectToolbar-2 AT ROW 26.14 COL 1.6
     RECT-5 AT ROW 18.38 COL 2
     RECT-6 AT ROW 18.38 COL 109.2
     brwPrisko AT ROW 27.29 COL 122.6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1.05
         SIZE 206.6 BY 33.24.


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
         TITLE              = "Avviks visning"
         HEIGHT             = 33.29
         WIDTH              = 206.8
         MAX-HEIGHT         = 57.14
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 57.14
         VIRTUAL-WIDTH      = 320
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
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

{incl/devmode.i}
{incl/custdevmode.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME Custom                                                    */
ASSIGN 
       AnbefaltPris:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN AnbefaltPris-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       AnbefaltPris-2:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN ArtikkelNr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       ArtikkelNr:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN ArtikkelNr-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       ArtikkelNr-2:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN Beskr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       Beskr:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN Beskr-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       Beskr-2:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN Beskrivelse IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Beskrivelse-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Bongtekst IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Bongtekst-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnBeskr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnLevFargKod IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnLevKod IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN DB%#1 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       DB%#1:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN DB%#1-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       DB%#1-2:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR COMBO-BOX Etikett IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX Etikett-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Etikettekst1 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Etikettekst1-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-JamforPris1 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       FI-JamforPris1:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN FI-JamforPris2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       FI-JamforPris2:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       ForhRab%#1:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN ForhRab%-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       ForhRab%-2:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN Innkjopspris#1 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       Innkjopspris#1:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN Innkjopspris#1-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       Innkjopspris#1-2:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR COMBO-BOX Jamforenhet-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       Katalogpris#1:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN Katalogpris-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       Katalogpris-2:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR TOGGLE-BOX Lager IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX Lager-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lblTekst-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       lblTekst-2:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN lblTekst1 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       lblTekst1:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN LevDato1 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN LevDato1-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN LevDato2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN LevDato2-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN LevDato3 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN LevDato3-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN LevDato4 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN LevDato4-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN LevFargKod IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN LevFargKod-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN LevKod IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       LevKod:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN LevKod-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       LevKod-2:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN LevNamn IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN LevNamn-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN LevNr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       LevNr:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN LevNr-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       LevNr-2:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN LinkVareNr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN LinkVareNr-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Mengde IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Mengde-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Mva%#1 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       Mva%#1:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN Mva%#1-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       Mva%#1-2:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN Pris#1 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       Pris#1:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN Pris#1-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       Pris#1-2:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN ProdNr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       ProdNr:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN ProdNr-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       ProdNr-2:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       ProfilNr:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       ProfilNr-2:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN Rab1%#1 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       Rab1%#1:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN Rab1%#1-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       Rab1%#1-2:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN Sasbeskr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Sasbeskr-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN SaSong IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       SaSong:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN SaSong-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       SaSong-2:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN StrTypeId IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN StrTypeId-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       suppRab%#1:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN suppRab%-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       suppRab%-2:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN suppVareKost IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       suppVareKost:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN suppVareKost-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       suppVareKost-2:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN Tilbud IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN txtArtikkel IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       txtArtikkel:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN txtVPIartbas IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       txtVPIartbas:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN Varekost IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       Varekost:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN Varekost#1 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       Varekost#1:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN Varekost#1-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       Varekost#1-2:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN Varekost-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       Varekost-2:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN Vg IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       Vg:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN Vg-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       Vg-2:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN VgBeskr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN VmId IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN VmId-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN VPIDato IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN VPIDato-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Avviks visning */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Avviks visning */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SokLinkvare
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SokLinkvare C-Win
ON CHOOSE OF B-SokLinkvare IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF LinkVareNr
DO:
  
  DO WITH FRAME Default-Frame:

    THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
    RUN JBoxLookup.w (THIS-PROCEDURE,1000,
                      "ArtBas"
                       + ";ArtikkelNr"
                       + ";Beskr"
                       ,
                     "WHERE ArtBas.Pant = TRUE"
                      ,""
                      ,"ArtikkelNr",
                      OUTPUT cTekst,
                      OUTPUT bOK).
    THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.
    
    IF RETURN-VALUE = "AVBRYT" THEN DO:
      /* Legger opp verdier I de aktuelle feltene */
      ASSIGN
        LinkVareNr:SCREEN-VALUE = ''.
    END.
    ELSE DO:
      /* Legger opp verdier I de aktuelle feltene */
      ASSIGN
        LinkVareNr:SCREEN-VALUE = cTekst.
/*       APPLY "ENTRY" TO VmId. */
    END.
  END.
  APPLY "ENTRY" TO LinkVareNr.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnBeskr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnBeskr C-Win
ON CHOOSE OF btnBeskr IN FRAME DEFAULT-FRAME /* ... */
DO:
  IF Beskr:SCREEN-VALUE = "" THEN RETURN NO-APPLY.

  IF VALID-HANDLE(hArtBasSok) THEN 
    APPLY "close" TO hArtBasSok.

  RUN ArtBasSok.w PERSIST SET hArtBasSok.
  IF VALID-HANDLE(hArtBasSok) THEN
  DO:
    SUBSCRIBE TO "ArtBasSokArtikkelNr" IN hArtBasSok.
    DYNAMIC-FUNCTION("setHideStr" IN hArtBasSok).
    RUN InitializeObject IN hArtBasSok.
    DYNAMIC-FUNCTION('setCloseOnSelect' IN hArtBasSok,YES).
    DYNAMIC-FUNCTION("setBeskr" IN hArtBasSok,Beskr:SCREEN-VALUE,YES).
    RUN MoveToTop IN hArtBasSok.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalLevDato1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalLevDato1 C-Win
ON CHOOSE OF btnCalLevDato1 IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF LevDato1
DO:
  RUN Cal.w (LevDato1:HANDLE).
  APPLY "ENTRY" TO LevDato1.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalLevDato2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalLevDato2 C-Win
ON CHOOSE OF btnCalLevDato2 IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF LevDato2
DO:
  RUN Cal.w (LevDato2:HANDLE).
  APPLY "ENTRY" TO LevDato2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalLevDato3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalLevDato3 C-Win
ON CHOOSE OF btnCalLevDato3 IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF LevDato3
DO:
  RUN Cal.w (LevDato3:HANDLE).
  APPLY "ENTRY" TO LevDato3.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalLevDato4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalLevDato4 C-Win
ON CHOOSE OF btnCalLevDato4 IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF LevDato4
DO:
  RUN Cal.w (LevDato4:HANDLE).
  APPLY "ENTRY" TO LevDato4.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLevFargKod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLevFargKod C-Win
ON CHOOSE OF btnLevFargKod IN FRAME DEFAULT-FRAME /* ... */
DO:
  IF LevFargKod:SCREEN-VALUE = "" THEN RETURN NO-APPLY.

  IF NOT VALID-HANDLE(hArtBasSok) THEN DO:
    DYNAMIC-FUNCTION("DoMessage",0,0,"Du må starte søket enten med beskrivelse eller lev.art.nr først","","").
    RETURN NO-APPLY.
  END.

  DYNAMIC-FUNCTION("setLevFargKod" IN hArtBasSok,LevFargKod:SCREEN-VALUE,YES).
  RUN MoveToTop IN hArtBasSok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLevKod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLevKod C-Win
ON CHOOSE OF btnLevKod IN FRAME DEFAULT-FRAME /* ... */
DO:
  IF LevKod:SCREEN-VALUE = "" THEN RETURN NO-APPLY.

  IF VALID-HANDLE(hArtBasSok) THEN 
    APPLY "close" TO hArtBasSok.

  RUN ArtBasSok.w PERSIST SET hArtBasSok.
  IF VALID-HANDLE(hArtBasSok) THEN
  DO:
    SUBSCRIBE TO "ArtBasSokArtikkelNr" IN hArtBasSok.
    DYNAMIC-FUNCTION("setHideStr" IN hArtBasSok).
    RUN InitializeObject IN hArtBasSok.
    DYNAMIC-FUNCTION('setCloseOnSelect' IN hArtBasSok,YES).
    DYNAMIC-FUNCTION("setLevKodQuery" IN hArtBasSok,LevKod:SCREEN-VALUE).
    RUN MoveToTop IN hArtBasSok.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnProdusent
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnProdusent C-Win
ON CHOOSE OF btnProdusent IN FRAME DEFAULT-FRAME /* ... */
OR "F10" OF ProdNr DO:
  DEF VAR cTekst AS CHAR NO-UNDO.

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxLookup.w (THIS-PROCEDURE,1000,
                    "Produsent"
                     + ";ProdNr"
                     + ";Beskrivelse"
                     ,
                   "WHERE true"
                    ,""
                    ,"ProdNr,Beskrivelse",
                    OUTPUT cTekst,
                    OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk AND cTekst NE "" THEN DO:
    ASSIGN 
       ProdNr:SCREEN-VALUE      = ENTRY(1,cTekst,"|")
       Beskrivelse:SCREEN-VALUE = ENTRY(2,cTekst,"|")
       .
    APPLY "any-printable" TO ProdNr.
  END.
  APPLY "ENTRY" TO ProdNr.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnProdusent-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnProdusent-2 C-Win
ON CHOOSE OF btnProdusent-2 IN FRAME DEFAULT-FRAME /* ... */
OR "F10" OF Vg DO:
  DEF VAR cTekst AS CHAR NO-UNDO.

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxLookup.w (THIS-PROCEDURE,1000,
                    "VarGr"
                     + ";Vg"
                     + ";VgBeskr"
                     + ";MomsKod"
                     ,
                   "WHERE true"
                    ,""
                    ,"Vg,VgBeskr,MomsKod",
                    OUTPUT cTekst,
                    OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk AND cTekst NE "" THEN DO:
    ASSIGN 
       Vg:SCREEN-VALUE      = ENTRY(1,cTekst,"|")
       VgBeskr:SCREEN-VALUE = ENTRY(2,cTekst,"|")
       .

    Mva%#1:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldValues","Moms",
                                                   "WHERE MomsKod = " + ENTRY(3,cTekst,"|"),"MomsProc").

    APPLY "value-changed" TO Innkjopspris#1.
    APPLY "any-printable" TO Vg.
  END.
  APPLY "ENTRY" TO Vg.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnProdusent-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnProdusent-3 C-Win
ON CHOOSE OF btnProdusent-3 IN FRAME DEFAULT-FRAME /* ... */
OR "F10" OF LevNr DO:
  DEF VAR cTekst AS CHAR NO-UNDO.

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxLookup.w (THIS-PROCEDURE,1000,
                    "LevBas"
                     + ";LevNr"
                     + ";LevNamn"
                     ,
                   "WHERE true"
                    ,""
                    ,"LevNr,LevNamn",
                    OUTPUT cTekst,
                    OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk AND cTekst NE "" THEN DO:
    ASSIGN 
       LevNr:SCREEN-VALUE   = ENTRY(1,cTekst,"|")
       LevNamn:SCREEN-VALUE = ENTRY(2,cTekst,"|")
       .
    APPLY "any-printable" TO LevNr.
  END.
  APPLY "ENTRY" TO LevNr.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnProdusent-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnProdusent-4 C-Win
ON CHOOSE OF btnProdusent-4 IN FRAME DEFAULT-FRAME /* ... */
OR "F10" OF Sasong DO:
  DEF VAR cTekst AS CHAR NO-UNDO.

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxLookup.w (THIS-PROCEDURE,1000,
                    "Sasong"
                     + ";Sasong"
                     + ";SasBeskr"
                     ,
                   "WHERE true"
                    ,""
                    ,"Sasong,SasBeskr",
                    OUTPUT cTekst,
                    OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk AND cTekst NE "" THEN DO:
    ASSIGN 
       Sasong:SCREEN-VALUE      = ENTRY(1,cTekst,"|")
       SasBeskr:SCREEN-VALUE = ENTRY(2,cTekst,"|")
       .
    APPLY "any-printable" TO Sasong.
  END.
  APPLY "ENTRY" TO Sasong.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnProdusent-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnProdusent-5 C-Win
ON CHOOSE OF btnProdusent-5 IN FRAME DEFAULT-FRAME /* ... */
OR "F10" OF VmId DO:
  DEF VAR cTekst AS CHAR NO-UNDO.

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxLookup.w (THIS-PROCEDURE,1000,
                    "Varemerke"
                     + ";VmId"
                     + ";Beskrivelse"
                     ,
                   "WHERE true"
                    ,""
                    ,"VmId,Beskrivelse",
                    OUTPUT cTekst,
                    OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk AND cTekst NE "" THEN DO:
    ASSIGN 
       VmId:SCREEN-VALUE          = ENTRY(1,cTekst,"|")
       VmBeskrivelse:SCREEN-VALUE = ENTRY(2,cTekst,"|")
       .
    APPLY "any-printable" TO VmId.
  END.
  APPLY "ENTRY" TO VmId.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnStrType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnStrType C-Win
ON CHOOSE OF btnStrType IN FRAME DEFAULT-FRAME /* ... */
OR "F10" OF StrTypeId DO:
  DEF VAR cTekst AS CHAR NO-UNDO.

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxLookup.w (THIS-PROCEDURE,1000,
                    "StrType"
                     + ";StrTypeId"
                     + ";Beskrivelse"
                     + ";Fordeling"
                     ,
                   "WHERE true"
                    ,""
                    ,"StrTypeId,Beskrivelse",
                    OUTPUT cTekst,
                    OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk AND cTekst NE "" THEN DO:
    ASSIGN 
       StrTypeId:SCREEN-VALUE       = ENTRY(1,cTekst,"|")
       .
    APPLY "any-printable" TO StrTypeId.
  END.
  APPLY "ENTRY" TO StrTypeId.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-JamforPris1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-JamforPris1 C-Win
ON VALUE-CHANGED OF FI-JamforPris1 IN FRAME DEFAULT-FRAME
DO:
    IF DEC(Katalogpris#1:SCREEN-VALUE) <> 0 THEN 
    DO:
        ASSIGN
            VareKost:SCREEN-VALUE = STRING(DEC(Katalogpris#1:SCREEN-VALUE) - (DEC(Katalogpris#1:SCREEN-VALUE) * DEC(ForhRab%#1:SCREEN-VALUE) / 100))
            suppVareKost:SCREEN-VALUE = STRING(DEC(Katalogpris#1:SCREEN-VALUE) - (DEC(Katalogpris#1:SCREEN-VALUE) * DEC(suppRab%#1:SCREEN-VALUE) / 100))
            .
    END.
    ELSE 
        ASSIGN
            VareKost:SCREEN-VALUE     = ''
            suppVareKost:SCREEN-VALUE = ''
            .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-JamforPris2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-JamforPris2 C-Win
ON VALUE-CHANGED OF FI-JamforPris2 IN FRAME DEFAULT-FRAME
DO:
    IF DEC(Katalogpris#1:SCREEN-VALUE) <> 0 THEN 
    DO:
        ASSIGN
            VareKost:SCREEN-VALUE = STRING(DEC(Katalogpris#1:SCREEN-VALUE) - (DEC(Katalogpris#1:SCREEN-VALUE) * DEC(ForhRab%#1:SCREEN-VALUE) / 100))
            suppVareKost:SCREEN-VALUE = STRING(DEC(Katalogpris#1:SCREEN-VALUE) - (DEC(Katalogpris#1:SCREEN-VALUE) * DEC(suppRab%#1:SCREEN-VALUE) / 100))
            .
    END.
    ELSE 
        ASSIGN
            VareKost:SCREEN-VALUE     = ''
            suppVareKost:SCREEN-VALUE = ''
            .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ForhRab%#1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ForhRab%#1 C-Win
ON VALUE-CHANGED OF ForhRab%#1 IN FRAME DEFAULT-FRAME /* Forh.rabatt% */
DO:
    IF DEC(Katalogpris#1:SCREEN-VALUE) <> 0 THEN 
    DO:
        ASSIGN
            VareKost:SCREEN-VALUE = STRING(DEC(Katalogpris#1:SCREEN-VALUE) - (DEC(Katalogpris#1:SCREEN-VALUE) * DEC(ForhRab%#1:SCREEN-VALUE) / 100))
            .
    END.
    ELSE
        ASSIGN
            VareKost:SCREEN-VALUE = ''.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Innkjopspris#1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Innkjopspris#1 C-Win
ON VALUE-CHANGED OF Innkjopspris#1 IN FRAME DEFAULT-FRAME /* Innkjøpspris */
DO:
    IF DEC(Innkjopspris#1:SCREEN-VALUE) <> 0 AND 
        DEC(Pris#1:SCREEN-VALUE) <> 0 THEN
    DO:
        ASSIGN
            plVareKost = DEC(Innkjopspris#1:SCREEN-VALUE) - (DEC(Innkjopspris#1:SCREEN-VALUE) * DEC(Rab1%#1:SCREEN-VALUE) / 100)
            Varekost#1:SCREEN-VALUE = STRING(plVareKost)
            
            plMvaKr = ROUND(DEC(Pris#1:SCREEN-VALUE) - (DEC(Pris#1:SCREEN-VALUE) / (1 + (DEC(Mva%#1:SCREEN-VALUE) / 100))),2)
            plDbKr  = ROUND(DEC(Pris#1:SCREEN-VALUE) - plMvaKr - DEC(Varekost#1:SCREEN-VALUE),2)
            DB%#1  = ROUND((plDbKr * 100) / (DEC(Varekost#1:SCREEN-VALUE) + plDbKr),2)
            DB%#1  = IF DB%#1 = ? THEN 0 ELSE DB%#1
            DB%#1:SCREEN-VALUE = STRING(DB%#1)
            .
    END.
    ELSE
      DB%#1:SCREEN-VALUE = ''.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Katalogpris#1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Katalogpris#1 C-Win
ON VALUE-CHANGED OF Katalogpris#1 IN FRAME DEFAULT-FRAME /* Engros */
DO:
    IF DEC(Katalogpris#1:SCREEN-VALUE) <> 0 THEN 
    DO:
        ASSIGN
            VareKost:SCREEN-VALUE = STRING(DEC(Katalogpris#1:SCREEN-VALUE) - (DEC(Katalogpris#1:SCREEN-VALUE) * DEC(ForhRab%#1:SCREEN-VALUE) / 100))
            suppVareKost:SCREEN-VALUE = STRING(DEC(Katalogpris#1:SCREEN-VALUE) - (DEC(Katalogpris#1:SCREEN-VALUE) * DEC(suppRab%#1:SCREEN-VALUE) / 100))
            .
    END.
    ELSE 
        ASSIGN
            VareKost:SCREEN-VALUE     = ''
            suppVareKost:SCREEN-VALUE = ''
            .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Mengde
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Mengde C-Win
ON VALUE-CHANGED OF Mengde IN FRAME DEFAULT-FRAME /* Mengde i s.enhet/jamforpris */
DO:
  FI-JamforPris1:screen-value = STRING(ROUND(INPUT Pris#1 / INPUT Mengde,2)).
  IF FI-JamforPris1:screen-value = ? then FI-JamforPris1:screen-value = ''.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Pris#1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Pris#1 C-Win
ON TAB OF Pris#1 IN FRAME DEFAULT-FRAME /* Pris */
OR RETURN OF Pris#1 DO:
    APPLY "ENTRY" TO Beskr.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Pris#1 C-Win
ON VALUE-CHANGED OF Pris#1 IN FRAME DEFAULT-FRAME /* Pris */
DO:
    IF DEC(Innkjopspris#1:SCREEN-VALUE) <> 0 AND 
        DEC(Pris#1:SCREEN-VALUE) <> 0 THEN
    DO:
        ASSIGN
            plMvaKr = ROUND(DEC(Pris#1:SCREEN-VALUE) - (DEC(Pris#1:SCREEN-VALUE) / (1 + (DEC(Mva%#1:SCREEN-VALUE) / 100))),2)
            plDbKr  = ROUND(DEC(Pris#1:SCREEN-VALUE) - plMvaKr - DEC(Varekost#1:SCREEN-VALUE),2)
            DB%#1  = ROUND((plDbKr * 100) / (DEC(Varekost#1:SCREEN-VALUE) + plDbKr),2)
            DB%#1  = IF DB%#1 = ? THEN 0 ELSE DB%#1
            DB%#1:SCREEN-VALUE = STRING(DB%#1)
            .
    END.

    ELSE
      DB%#1:SCREEN-VALUE = ''.

    FI-JamforPris1:screen-value = STRING(ROUND(INPUT Pris#1 / INPUT Mengde,2)).
    IF FI-JamforPris1:screen-value = ? then FI-JamforPris1:screen-value = ''.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Rab1%#1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Rab1%#1 C-Win
ON VALUE-CHANGED OF Rab1%#1 IN FRAME DEFAULT-FRAME /* Rabatt%1 */
DO:
    IF DEC(Innkjopspris#1:SCREEN-VALUE) <> 0 THEN
    DO:
        ASSIGN
            plVareKost = ROUND(DEC(Innkjopspris#1:SCREEN-VALUE) - (DEC(Innkjopspris#1:SCREEN-VALUE) * DEC(Rab1%#1:SCREEN-VALUE) / 100),2)
            Varekost#1:SCREEN-VALUE = STRING(plVareKost)
            
            plMvaKr = ROUND(DEC(Pris#1:SCREEN-VALUE) - (DEC(Pris#1:SCREEN-VALUE) / (1 + (DEC(Mva%#1:SCREEN-VALUE) / 100))),2)
            plDbKr  = ROUND(DEC(Pris#1:SCREEN-VALUE) - plMvaKr - DEC(Varekost#1:SCREEN-VALUE),2)
            DB%#1  = ROUND((plDbKr * 100) / (DEC(Varekost#1:SCREEN-VALUE) + plDbKr),2)
            DB%#1  = IF DB%#1 = ? THEN 0 ELSE DB%#1
            DB%#1:SCREEN-VALUE = STRING(DB%#1)
            .
    END.
    ELSE
      DB%#1:SCREEN-VALUE = ''.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME suppRab%#1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL suppRab%#1 C-Win
ON VALUE-CHANGED OF suppRab%#1 IN FRAME DEFAULT-FRAME /* Sup.Rabatt% */
DO:
    IF DEC(Katalogpris#1:SCREEN-VALUE) <> 0 THEN 
    DO:
        ASSIGN
            suppVareKost:SCREEN-VALUE = STRING(DEC(Katalogpris#1:SCREEN-VALUE) - (DEC(Katalogpris#1:SCREEN-VALUE) * DEC(suppRab%#1:SCREEN-VALUE) / 100))
            .
    END.
    ELSE
        ASSIGN
            suppVareKost:SCREEN-VALUE = ''.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME suppVareKost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL suppVareKost C-Win
ON VALUE-CHANGED OF suppVareKost IN FRAME DEFAULT-FRAME /* Nto. Innpr. supp */
DO:
    IF DEC(Katalogpris#1:SCREEN-VALUE) <> 0 AND DEC(suppVarekost:SCREEN-VALUE) <> 0 THEN 
    DO:
        ASSIGN
            suppRab%#1:SCREEN-VALUE = STRING((DEC(Katalogpris#1:SCREEN-VALUE) - DEC(suppVarekost:SCREEN-VALUE)) * 100 / DEC(Katalogpris#1:SCREEN-VALUE)).
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Varekost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Varekost C-Win
ON VALUE-CHANGED OF Varekost IN FRAME DEFAULT-FRAME /* Nto Innpr. forh */
DO:
    IF DEC(Katalogpris#1:SCREEN-VALUE) <> 0 AND DEC(Varekost:SCREEN-VALUE) <> 0 THEN 
    DO:
        ASSIGN
            ForhRab%#1:SCREEN-VALUE = STRING(
                                             (DEC(Katalogpris#1:SCREEN-VALUE) - DEC(Varekost:SCREEN-VALUE)) * 100 
                                              / 
                                             DEC(Katalogpris#1:SCREEN-VALUE)).
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Varekost#1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Varekost#1 C-Win
ON VALUE-CHANGED OF Varekost#1 IN FRAME DEFAULT-FRAME /* Varekost */
DO:
    IF DEC(Innkjopspris#1:SCREEN-VALUE) <> 0 AND DEC(Varekost#1:SCREEN-VALUE) <> 0 THEN 
    DO:
        ASSIGN
            Rab1%#1:SCREEN-VALUE = STRING(
                                             (DEC(Innkjopspris#1:SCREEN-VALUE) - DEC(Varekost#1:SCREEN-VALUE)) * 100 
                                              / 
                                             DEC(Innkjopspris#1:SCREEN-VALUE)).
        APPLY 'value-changed' TO Innkjopspris#1.
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

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
  IF NOT DYNAMIC-FUNCTION("DoCleanUpObjects",THIS-PROCEDURE:CURRENT-WINDOW) THEN RETURN NO-APPLY.
  PUBLISH "InvalidateHandle".
  DYNAMIC-FUNCTION("SetCleanUpResize",THIS-PROCEDURE:CURRENT-WINDOW).
  IF VALID-HANDLE(hArtBasSok) THEN APPLY "close" TO hArtBasSok.
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

  hParent = SOURCE-PROCEDURE.

  RUN enable_UI.

  RUN InitializeObject.
  THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = FALSE.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ArtBasSokArtikkelNr C-Win 
PROCEDURE ArtBasSokArtikkelNr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM iiArtNr AS INT NO-UNDO.

DYNAMIC-FUNCTION("setAttribute",hQuery-2,"baseQuery","WHERE ArtikkelNr = " + STRING(iiArtNr)).
RUN InvokeMethod(hQuery-2,"OpenQuery").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ArtikkelKortRecord C-Win 
PROCEDURE ArtikkelKortRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN ArtikkelKortRecord IN hParent NO-ERROR.
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
/* DYNAMIC-FUNCTION("setAttribute",hBrowse,"basequery","WHERE iJBoxCompanyId = " + STRING(DYNAMIC-FUNCTION("getCompanyId"))). */
/* DYNAMIC-FUNCTION("setCurrentObject",hBrowse).                                                                              */
/* RUN OpenQuery.                                                                                                             */
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplayRecord C-Win 
PROCEDURE DisplayRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR cQueryJoin   AS CHAR NO-UNDO.

  IF DYNAMIC-FUNCTION('getCurrentObject') = hQuery  THEN
  DO:
      
    /*
    iArtPrisProfilnr = DYNAMIC-FUNCTION("getFieldValues",'FIRST VPIArtPris','WHERE EkstVPILevNr = ' + string(hFieldMap:buffer-field('EkstVPILEvNr'):buffer-value) + ' and artikkelnr = ' + string(hFieldMap:buffer-field('Artikkelnr'):buffer-value) 
                     + ' AND profilnr = ' + STRING(hFieldMap:BUFFER-FIELD('Profilnr'):BUFFER-VALUE),'ProfilNr').
    */
    iArtPrisProfilnr = DYNAMIC-FUNCTION("getFieldValues",'ArtPris','WHERE artikkelnr = ' + string(hFieldMap:buffer-field('Artikkelnr'):buffer-value) 
                       + ' AND profilnr = ' + STRING(hFieldMap:BUFFER-FIELD('Profilnr'):BUFFER-VALUE),'ProfilNr').
    IF iArtPrisProfilnr = ? THEN
        iArtPrisProfilnr = DYNAMIC-FUNCTION("getFieldValues",'FIRST ArtPris','WHERE artikkelnr = ' + string(hFieldMap:buffer-field('Artikkelnr'):buffer-value),'ProfilNr').

    ASSIGN 
      cQueryJoin = ",FIRST VarGr OUTER-JOIN OF ArtBas NO-LOCK"
                + ", FIRST LevBas OUTER-JOIN OF ArtBas NO-LOCK"
                + ", FIRST Produsent OUTER-JOIN OF ArtBas NO-LOCK"
                + ", FIRST SaSong OUTER-JOIN OF ArtBas NO-LOCK"
                + ", FIRST Varemerke OUTER-JOIN OF ArtBas NO-LOCK"
    .
    cQueryJoin = cQueryJoin + (IF iArtPrisProfilNr NE ? THEN ", FIRST ArtPris OUTER-JOIN NO-LOCK WHERE ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND ArtPris.ProfilNr = " + STRING(iArtPrisProfilNr)
                                                        ELSE ", FIRST ArtPris OUTER-JOIN NO-LOCK WHERE ArtPris.ArtikkelNr = ArtBas.ArtikkelNr").
    DYNAMIC-FUNCTION('setAttribute',hQuery-2,'QueryJoin',cQueryJoin).
  END.
  RUN super.
  IF hFieldMap-2:AVAIL THEN 
      Tilbud:SCREEN-VALUE IN FRAME {&FRAME-NAME} = IF hFieldMap-2:BUFFER-FIELD('Tilbud'):BUFFER-VALUE 
                                                     THEN 'PÅ TILBUD!' 
                                                     ELSE ''.


  ASSIGN
      btnBeskr:HIDDEN           = TRUE
      btnLevKod:HIDDEN          = TRUE
      btnLevFargKod:HIDDEN      = TRUE
      /*
      Innkjopspris#1:SENSITIVE  = FALSE
      Rab1%#1:SENSITIVE         = FALSE
      Pris#1:SENSITIVE          = FALSE
      Varekost#1:SENSITIVE      = FALSE
      */
      Profilnr:SENSITIVE        = FALSE
      DB%#1:SENSITIVE           = FALSE
      Mva%#1:SENSITIVE          = FALSE
      /*
      KatalogPris#1:SENSITIVE   = FALSE
      ForhRab%#1:SENSITIVE      = FALSE
      Varekost:SENSITIVE        = FALSE
      suppRab%#1:SENSITIVE      = FALSE
      suppVareKost:SENSITIVE    = FALSE
      AnbefaltPris:SENSITIVE    = FALSE
      */
      .

  IF TRIM(LevDato1:SCREEN-VALUE) = '/  /' THEN ASSIGN 
                                                 LevDato1:SCREEN-VALUE = ''
                                                 LevDato1:MODIFIED = FALSE.
  IF TRIM(LevDato2:SCREEN-VALUE) = '/  /' THEN ASSIGN 
                                                 LevDato2:SCREEN-VALUE = ''
                                                 LevDato2:MODIFIED = FALSE.
  IF TRIM(LevDato3:SCREEN-VALUE) = '/  /' THEN 
                                               ASSIGN 
                                                 LevDato3:SCREEN-VALUE = ''
                                                 LevDato3:MODIFIED = FALSE.
  IF TRIM(LevDato4:SCREEN-VALUE) = '/  /' THEN ASSIGN 
                                                 LevDato4:SCREEN-VALUE = ''
                                                 LevDato4:MODIFIED = FALSE.
  IF TRIM(VPIDato:SCREEN-VALUE)  = '/  /' THEN ASSIGN 
                                                 VPIDato:SCREEN-VALUE = ''
                                                 VPIDato:MODIFIED = FALSE.

  VmBeskrivelse:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldValues","Varemerke","WHERE VmId = " + VmId:SCREEN-VALUE,"Beskrivelse").
  VmBeskrivelse-2:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldValues","Varemerke","WHERE VmId = " + VmId-2:SCREEN-VALUE,"Beskrivelse").

  FI-JamforPris1:screen-value = STRING(ROUND(INPUT Pris#1 / INPUT Mengde,2)).
  IF FI-JamforPris1:screen-value = ? then FI-JamforPris1:screen-value = ''.
  FI-JamforPris2:screen-value = STRING(ROUND(INPUT Pris#1-2 / INPUT Mengde-2,2)).
  IF FI-JamforPris2:screen-value = ? then FI-JamforPris2:screen-value = ''.

  RUN MoveToTop.
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
  DISPLAY Grunnsortiment ArtikkelNr-2 ArtikkelNr Beskr-2 Beskr Bongtekst 
          Bongtekst-2 LevKod-2 LevKod Etikett Etikett-2 LevFargKod-2 LevFargKod 
          Etikettekst1 Etikettekst1-2 Vg-2 Vg LinkVareNr LinkVareNr-2 LevNr-2 
          LevNr StrTypeId ProdNr-2 VmId-2 ProdNr VmId LevDato1-2 Jamforenhet-2 
          LevDato1 Jamforenhet LevDato2-2 Mengde-2 LevDato2 Mengde LevDato3-2 
          Lager-2 LevDato3 Lager LevDato4-2 LokPris-2 LevDato4 LokPris 
          VPIBildeKode-2 VPIDato-2 VPIDato VPIBildeKode SaSong-2 SaSong 
          Katalogpris#1 ForhRab%#1 Varekost suppRab%#1 suppVareKost AnbefaltPris 
          Innkjopspris#1 ProfilNr Katalogpris-2 Innkjopspris#1-2 ProfilNr-2 
          ForhRab%-2 Rab1%#1 Rab1%#1-2 Varekost-2 Varekost#1 Varekost#1-2 
          suppRab%-2 DB%#1 DB%#1-2 suppVareKost-2 Mva%#1 Mva%#1-2 AnbefaltPris-2 
          Pris#1 Pris#1-2 Grunnsortiment-2 StrTypeId-2 txtVPIartbas txtArtikkel 
          VgBeskr-2 VgBeskr LevNamn-2 LevNamn Beskrivelse-2 VmBeskrivelse-2 
          VmBeskrivelse Beskrivelse Tilbud Sasbeskr-2 Sasbeskr lblTekst1 
          lblTekst-2 FI-JamforPris1 FI-JamforPris2 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE Grunnsortiment btnCalLevDato1 Jamforenhet LokPris-2 LokPris 
         VPIBildeKode-2 VPIBildeKode Katalogpris#1 ForhRab%#1 suppRab%#1 
         btnProdusent-2 AnbefaltPris ProfilNr btnProdusent-3 ProfilNr-2 
         btnProdusent btnProdusent-5 Grunnsortiment-2 btnProdusent-4 
         btnCalLevDato2 btnCalLevDato3 btnCalLevDato4 B-SokLinkvare VgBeskr-2 
         VmBeskrivelse-2 VmBeskrivelse btnStrType rectToolbar rectWinToolbar 
         rectBrowse RECT-1 RECT-2 RECT-3 RECT-4 rectToolbar-2 RECT-5 RECT-6 
         brwPrisko 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FirstRecord C-Win 
PROCEDURE FirstRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN SUPER.
  DYNAMIC-FUNCTION('setColour').
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeObject C-Win 
PROCEDURE InitializeObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  buildWidgetTable().
  hBrowse = DYNAMIC-FUNCTION("getParentBrowse" IN hParent).    

  cBrukerId   = DYNAMIC-FUNCTION("getASuserId").
  iBrukerType = DYNAMIC-FUNCTION("getFieldValues","Bruker","WHERE BrukerId = '" + cBrukerId + "'","ButikkNr").

  Etikett:LIST-ITEM-PAIRS = DYNAMIC-FUNCTION("getFieldValues","SysPara",
                                            "WHERE SysHId = 2 and SysGr = 4 and ParaNr = 9","Parameter1").
  Etikett-2:LIST-ITEM-PAIRS = DYNAMIC-FUNCTION("getFieldValues","SysPara",
                                            "WHERE SysHId = 2 and SysGr = 4 and ParaNr = 9","Parameter1").
  ASSIGN 
    Jamforenhet:DELIMITER = "|"
    Jamforenhet:LIST-ITEM-PAIRS = DYNAMIC-FUNCTION("getFieldList","JamforEnhet;JamforEnhTekst;JamforEnhet",
                                                   "WHERE TRUE BY JamforEnhet")
    Jamforenhet-2:DELIMITER = "|"
    Jamforenhet-2:LIST-ITEM-PAIRS = DYNAMIC-FUNCTION("getFieldList","JamforEnhet;JamforEnhTekst;JamforEnhet",
                                                     "WHERE TRUE BY JamforEnhet")
    . 
  hQuery = DYNAMIC-FUNCTION("NewQuery",1,""
                             ,"VPIArtBas;!VPIDato;VPIBildeKode;!VisDivInfo;!VgKat;!Vg;!VareNr;!ValKod;!Tilv-Land;!suppRab%;!StrKode2;!StrKode1;!Storrelser;!Slit-Id;!SentralBestilling;!SattPaKampanje;!SaSong;!SalgsEnhet;!RegistrertTid;!RegistrertDato;!RegistrertAv;!RabKod;!ProvKod;!ProdNr;!PrisGrpNr;!Pant;!Pakkenr;!Pakke;!Ov-Id;!OPris;!Oppdatert;!OLLager;!Notat;!ModellFarge;!MatKod;!ManRabIKas;!LopNr;!LokArtikkelNr;!Linjemerknad;!LevVareTekst;!LevNr;!LevKod;!LevFargKod;!LevDato4;!LevDato3;!LevDato2;!LevDato1;!Last-Id;!KundeRabatt;!Kommentar;!Klack;!KjentPaHK;!KjedeVare;!KatalogPris;!Inner-Id;!IndividType;!IKasse;!HovedModellFarge;!HKVareId;!HkStyrt;!Hg;!HandKode;!Gjennomfaktureres;!GarantiKl;!forhRab%;!Foder-Id;!Farg;!ETid;!EkstVPILevNr;!EDato;!DivInfo;!BrukerID;!BildNr;!BildeIKasse;!BestForslag;!Beskr;!BehKode;!ArtSlag;!ArtikkelNr;!Anv-Id;!AntIPkn;!AnonseArtikkel;!AnbefaltPris;!Alder"
                             + ";+KatalogPris#1|decimal|>>>><>>9.99|KatalogPris#1|Katalogpris"
                             + ";+forhRab%#1|decimal|>>>><>>9.99|forhRab%#1|Forh.rab"
                             + ";+suppRab%#1|decimal|>>>><>>9.99|suppRab%#1|Sup.Rabatt"
                             + ";+VareKost|decimal|>>>><>>9.99|VareKost|Varekost"
                             + ";+suppVareKost|decimal|>>>><>>9.99|suppVareKost|Sup.Varekost"
                             + ";Lager|Lagerstyrt"
                             + ";LokPris|Lokal pris"
                             + ";Grunnsortiment"
                             + ";Bongtekst|Bongtekst"
                             + ";Etikett"
                             + ";Etikettekst1"
                             + ";LinkVareNr|Link til pantvare"
                             + ";StrTypeId|Størrelsestype"
                             + ";VmId|Varemerke"
                             + ";Jamforenhet|Jamførenhet"
                             + ";Mengde|Mengde i s.enhet|->>9.999"
                             + ",VarGr;Vg;VgBeskr"
                             + ",LevBas;LevNr;LevNamn"
                             + ",Produsent;ProdNr;Beskrivelse"
                             + ",SaSong;SaSong;SasBeskr"
                             + ",Varemerke"
                              + ";Beskrivelse"
                             + ",VPIArtPris;!VareNr;!VareKost;!ValPris;!TilbudTimeStyrt;!TilbudTilTid;!TilbudTilDato;!TilbudFraTid;!TilbudFraDato;!Tilbud;!RegistrertTid;!RegistrertDato;!RegistrertAv;!Rab3Kr;!Rab3%;!Rab2Kr;!Rab2%;!Rab1Kr;!Rab1%;ProfilNr;!Pris;!MvaKr;!Mva%;!LevNr;!InnkjopsPris;!Frakt%;!Frakt;!EuroPris;!EuroManuel;!ETid;!EkstVPILevNr;!EDato;!DivKostKr;!DivKost%;!DBKr;!DB%;!BrukerID;!ArtikkelNr;!AktivFraTid;!AktivFraDato"
                              + ";+innkjopspris#1|decimal|>>>><>>9.99|innkjopspris#1|Innkjøpspris"
                              + ";+Rab1%#1|decimal|>>>><>>9.99|Rab1%#1|Rabatt1%"
                              + ";+Varekost#1|decimal|>>>><>>9.99|Varekost#1|Varekost"
                              + ";+DB%#1|decimal|>>>><>>9.99|DB%#1|DB%"
                              + ";+Mva%#1|decimal|>>>><>>9.99|Mva%#1|MVA"
                              + ";+Pris#1|decimal|>>>><>>9.99|Pris#1|Pris"
                             ,"WHERE FALSE "
                             + ", FIRST VarGr OUTER-JOIN OF VPIArtBas NO-LOCK"
                             + ", FIRST LevBas OUTER-JOIN OF VPIArtBas NO-LOCK"
                             + ", FIRST Produsent OUTER-JOIN OF VPIArtBas NO-LOCK"
                             + ", FIRST SaSong OUTER-JOIN OF VPIArtBas NO-LOCK"
                             + ", FIRST Varemerke OUTER-JOIN OF VPIArtBas NO-LOCK"
                             + ", FIRST VPIArtPris OUTER-JOIN WHERE VPIArtPris.EkstVPILevNr = VPIArtBas.EkstVPILevNr AND VPIArtPris.VareNr = VPIArtBas.VareNr NO-LOCK"
                             ,"").
  
  DYNAMIC-FUNCTION("CreateOneToOneLink",hQuery,hBrowse,"EkstVPILevNr,Varenr").
  DYNAMIC-FUNCTION('setAttribute',hQuery,'calcfieldproc','vpiartbas_artikkel_detailcalc.p').

  /* A FieldMap describes the relationship between the retrieved record (in browse or query) and the screen input and disp. fields */
  hFieldMap = DYNAMIC-FUNCTION("NewFieldMap",
                             hQuery,                    
                             FRAME {&FRAME-NAME}:HANDLE,
                                  /* Updateable buffer fields and their correspondign input fields (blank if the same) */
                             "VPIBildeKode,Beskr,LevKod,LevFargKod,Vg,VgBeskr,LevNr,LevNamn,ProdNr,Beskrivelse,LevDato1,LevDato2,LevDato3" 
                               + ",Lager"
                               + ",LokPris"
                               + ",Grunnsortiment"
                               + ",Bongtekst"
                               + ",Etikett"
                               + ",Etikettekst1"
                               + ",LinkVareNr"
                               + ",StrTypeId"
                               + ",VmId"
                               + ",Jamforenhet"
                               + ",Mengde"
                               + ",ProfilNr"
                               + ",LevDato4,VPIDato,SaSong,SasBeskr,KatalogPris#1,forhRab%#1,suppRab%#1,VareKost,suppVareKost,Innkjopspris#1,Rab1%#1,Varekost#1,DB%#1,Mva%#1,Pris#1,AnbefaltPris",
                             "",
                               "ArtikkelNr",
                               "", 
                             ""). /* If any other fields are used for input these are listed here */

  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"customUpdateValProc","=vpiartbas_artikkeldetalj_updval.p").

  /*
,,InnkjopsPris,VareKost,ForhRab%,SupVarekost,SupRab%,Pris,AnbefaltPris,KjedeRab%,KjedeInnkPris  
  */
/*   SUBSCRIBE TO 'ReopenChild' IN hParent. /*For å kjøre hquery, siden jeg havnet i en evig loop ved openquery*/ */
    
  hQuery-2 = DYNAMIC-FUNCTION("NewQuery",1,""
                              ,"ArtBas;!VPIDato;VPIBildeKode;!VisDivInfo;!VgKat;!Vg;!ValKod;!Tilv-Land;!supRab%;!StrKode2;!StrKode1;!Storrelser;!Slit-Id;!SentralBestilling;!SattPaKampanje;!SaSong;!SalgsEnhet;!RegistrertTid;!RegistrertDato;!RegistrertAv;!RabKod;!ProvKod;!ProdNr;!PrisGrpNr;!Pant;!Pakkenr;!Pakke;!Ov-Id;!OPris;!OLLager;!Notat;!ModellFarge;!MatKod;!ManRabIKas;!LopNr;!Linjemerknad;!LevVareTekst;!LevNr;!LevKod;!LevFargKod;!LevDato4;!LevDato3;!LevDato2;!LevDato1;!Last-Id;!KundeRabatt;!Kommentar;!Klack;!KjentPaHK;!KjedeVare;!KatalogPris;!Inner-Id;!IndividType;!IKasse;!HovedModellFarge;!HKVareId;!HkStyrt;!Hg;!BehKode;!Gjennomfaktureres;!GarantiKl;!forhRab%;!Foder-Id;!Farg;!ETid;!EDato;!DivInfo;!BrukerID;!BildNr;!BildeIKasse;!BestForslag;!Beskr;!BehKode;!ArtSlag;!ArtikkelNr;!Anv-Id;!AntIPakn;!AnonseArtikkel;!AnbefaltPris;!Alder"
                              + ";Katalogpris;forhrab%;supRab%"
                              + ";+VareKost-2|decimal|>>>><>>9.99|VareKost-2|Varekost"
                              + ";+suppVareKost-2|decimal|>>>><>>9.99|suppVareKost-2|Sup.Varekost"
                              + ";Lager|Lagerstyrt"
                              + ";LokPris|Lokal pris"
                              + ";Grunnsortiment|Grunnsortiment"
                              + ";Bongtekst|Bongtekst"
                              + ";Etikett"
                              + ";Etikettekst1"
                              + ";LinkVareNr|Link til pantvare"
                              + ";StrTypeId|Størrelsestype"
                              + ";VmId|Varemerke"
                              + ";Jamforenhet|Jamførenhet"
                              + ";Mengde|Mengde i s.enhet|->>9.999"
                              + ",VarGr;Vg;VgBeskr"
                              + ",LevBas;LevNr;LevNamn"
                              + ",Produsent;ProdNr;Beskrivelse"
                              + ",SaSong;SaSong;SasBeskr"
                              + ",Varemerke"
                               + ";Beskrivelse"
                              + ",ArtPris;!VareKost;!ValPris;!TilbudTimeStyrt;!TilbudTilTid;!TilbudTilDato;!TilbudFraTid;!TilbudFraDato;!Tilbud;!RegistrertTid;!RegistrertDato;!RegistrertAv;!Rab3Kr;!Rab3%;!Rab2Kr;!Rab2%;!Rab1Kr;!Rab1%;ProfilNr;!Pris;!MvaKr;!Mva%;!MomsKod;!LevNr;!InnkjopsPris;!Frakt%;!Frakt;!EuroPris;!EuroManuel;!ETid;!EDato;!DivKostKr;!DivKost%;!DBKr;!DB%;!BrukerID;!ArtikkelNr;!AktivFraTid;!AktivFraDato"
                              + ";+innkjopspris#1-2|decimal|>>>><>>9.99|innkjopspris#1-2|Innkjøpspris"
                              + ";+Rab1%#1-2|decimal|>>>><>>9.99|Rab1%#1-2|Rabatt1%"
                              + ";+Varekost#1-2|decimal|>>>><>>9.99|Varekost#1-2|Varekost"
                              + ";+DB%#1-2|decimal|>>>><>>9.99|DB%#1-2|DB%"
                              + ";+Mva%#1-2|decimal|>>>><>>9.99|Mva%#1-2|MVA"
                              + ";+Pris#1-2|decimal|>>>><>>9.99|Pris#1-2|Pris"
                             ,"WHERE FALSE"
                              + ", FIRST VarGr OUTER-JOIN OF ArtBas NO-LOCK"
                              + ", FIRST LevBas OUTER-JOIN OF ArtBas NO-LOCK"
                              + ", FIRST Produsent OUTER-JOIN OF ArtBas NO-LOCK"
                              + ", FIRST SaSong OUTER-JOIN OF ArtBas NO-LOCK"
                              + ", FIRST Varemerke OUTER-JOIN OF ArtBas NO-LOCK"
                              + IF iArtPrisProfilNr GT 0 THEN (", FIRST ArtPris OUTER-JOIN WHERE ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND ArtPris.ProfilNr = ArtPris.ProfilNr NO-LOCK")
                                                         ELSE (", FIRST ArtPris OUTER-JOIN WHERE ArtPris.ArtikkelNr = ArtBas.ArtikkelNr NO-LOCK")
                             ,"").
  DYNAMIC-FUNCTION('setAttribute',hQuery-2,'calcfieldproc','vpiartbas_artikkel_detailcalc.p').
  
  hFieldMap-2 = DYNAMIC-FUNCTION("NewFieldMap",
                              hQuery-2,                    
                              FRAME {&FRAME-NAME}:HANDLE,
                              "",
                              "",
                              "ArtikkelNr,VPIBildeKode,Beskr,LevKod,LevFargKod,Vg,VgBeskr,LevNr,LevNamn,ProdNr,Beskrivelse,LevDato1,LevDato2,LevDato3"
                                 + ",Lager"
                                 + ",LokPris"
                                 + ",Grunnsortiment"
                                 + ",Bongtekst"
                                 + ",Etikett"
                                 + ",Etikettekst1"
                                 + ",LinkVareNr"
                                 + ",StrTypeId"
                                 + ",VmId"
                                 + ",Jamforenhet"
                                 + ",Mengde"
                                 + ",ProfilNr"
                              + ",LevDato4,VPIDato,SaSong,SasBeskr,Katalogpris,forhRab%,supRab%,VareKost-2,suppVareKost-2,Innkjopspris#1-2,Rab1%#1-2,Varekost#1-2,DB%#1-2,Mva%#1-2,Pris#1-2,AnbefaltPris",
                               "ArtikkelNr-2,VPIBildeKode-2,Beskr-2,LevKod-2,LevFargKod-2,Vg-2,VgBeskr-2,LevNr-2,LevNamn-2,ProdNr-2,Beskrivelse-2,LevDato1-2,LevDato2-2,LevDato3-2"
                                 + ",Lager-2"
                                 + ",LokPris-2"
                                 + ",Grunnsortiment-2"
                                 + ",Bongtekst-2"
                                 + ",Etikett-2"
                                 + ",Etikettekst1-2"
                                 + ",LinkVareNr-2"
                                 + ",StrTypeId-2"
                                 + ",VmId-2"
                                 + ",Jamforenhet-2"
                                 + ",Mengde-2"
                                 + ",ProfilNr-2"
                               + ",LevDato4-2,VPIDato-2,SaSong-2,SasBeskr-2,Katalogpris-2,forhRab%-2,suppRab%-2,VareKost-2,suppVareKost-2,Innkjopspris#1-2,Rab1%#1-2,Varekost#1-2,DB%#1-2,Mva%#1-2,Pris#1-2,AnbefaltPris-2",
                              ""). /* If any other fields are used for input these are listed here */

  DYNAMIC-FUNCTION("CreateParentLink",hQuery-2,hQuery,"parentlink").
  DYNAMIC-FUNCTION("CreateObjectLink",hFieldMap-2,hQuery-2).

/*   DYNAMIC-FUNCTION("createParentLink",hQuery,hBrowse,"Varenr"). */
  
  /* Create the browse: */

  hBrowse-2 = DYNAMIC-FUNCTION("NewBrowse",
                             rectBrowse:HANDLE,
                             100,
                             "",
                             "VPIStrekKode"
                             + ";StrKode;Storl|Str|;Kode"
                              + ";+chkStrekNy|logical|*/|chkStrekNy(ROWID)|Ny"
                              + ";+chkStrekFeil|logical|*/|chkStrekFeil(ROWID)|Feil" 
                              + ";Bestillingsnummer|Nytt best.nr"
                              + ";+gmlBestillingsnummer|char|x(15)|gmlBestNr(ROWID)|Gml. best.nr" 
                              + ";ERPNr;!EkstVPILevNr;!VareNr;EDato"
                             ,"WHERE FALSE "
                             ,"SORT|Storl").
  
  DYNAMIC-FUNCTION("createParentLink",hBrowse-2,hBrowse,"EkstVPILevNr,Varenr").
  DYNAMIC-FUNCTION('setAttribute',hBrowse-2,'calcfieldproc','vpiartbas_varebok_detailcalc.p').


  hBrwPrisko = DYNAMIC-FUNCTION("NewBrowse"
          ,brwPrisko:HANDLE
          ,10000
          ,""
          ,"PrisKo"
           + ";!ArtikkelNr|Art.nr"
/*            + ";+EndrType|CHARACTER|x(4)|endrType|Type" */
           + ";Profilnr|Profil"
           + ";Pris|Priskø-pris|>><>>><>>9.99"
           + ";DB%"
           + ";Varekost|Varekost"
           + ";AktiveresDato"
/*            + ";+AktivTid|CHAR|x(5)|jb_hhmm(AktiveresTid)|Akt.tid"                   */
/*            + ";+etikettStat|CHARACTER|x(15)|etikettStatus|Etikettstatus"            */
/*            + ";+klarStat|CHARACTER|x(10)|klargjorStatus|Behandlingsstatus"          */
/*            + ";+LokalPris|DECIMAL|->>><>>9.99|lokal_pris(ROWID)|Lokal pris"         */
/*            + ";+LokalVarekost|DECIMAL|->>><>>9.99|lokal_varekost(ROWID)|Lokal pris" */
/*            + ";+LokalDB%|DECIMAL|->>9.99|lokal_db%(ROWID)|Lokal pris"               */
           + ";Opphav"
           + ";!Aktivert"
           + ";!BrukerID"
           + ";!RegistrertDato"
           + ";!AktiveresTid"
           + ";!Tilbud"
           + ";EndringsType"
           + ";!Type"
          ,"WHERE false"
          ,"").

/*   DYNAMIC-FUNCTION("setAttribute",hBrwPrisko,"calcFieldProc","prisko_brwcalc.p").  */
  DYNAMIC-FUNCTION("createParentLink",hBrwPrisko,hBrowse,"ArtikkelNr").

  IF iBrukerType <= 1 THEN
      hToolbar = DYNAMIC-FUNCTION("NewToolbar",
                             rectToolbar:HANDLE,
                             "Fil",
                             "undo;Angre,save;Lagre"
                             + ",rule,first|Naviger;Første,prev|Naviger;Forrige,next|Naviger;Neste,last|Naviger;Siste"
                             + ",rule"                                      
                             + (IF CAN-DO(hParent:INTERNAL-ENTRIES,"sendArtikkelRegisterRecord")
                                THEN ",SendArtikkelRegister;Send til artikkel&register"
                                ELSE "")
                             + ",ArtikkelKort;Artikkel kort",
                             "maxborder").
  ELSE 
      hToolbar = DYNAMIC-FUNCTION("NewToolbar",
                             rectToolbar:HANDLE,
                             "Fil",
                             "undo;Angre,save;Lagre"
                             + ",rule,first|Naviger;Første,prev|Naviger;Forrige,next|Naviger;Neste,last|Naviger;Siste"
                             + ",rule",                                      
                             "maxborder").

  hToolbar-2 = DYNAMIC-FUNCTION("NewToolbar",
                             rectToolbar-2:HANDLE,
                             "Fil",
                             "Delete;Slett strekkode",
                             "maxborder").

  hWinToolbar = DYNAMIC-FUNCTION("NewToolbar",
                             rectWinToolbar:HANDLE,
                             "Fil",
                             "close;E&xit",
                             "right|enable").

  /* Disable the browse toolbar */
  DYNAMIC-FUNCTION("setToolbar",DYNAMIC-FUNCTION("getLinkedObject",hBrowse,"toolbar","from"),"disable").

  DYNAMIC-FUNCTION("CreateObjectLink",hQuery,hFieldMap).
  DYNAMIC-FUNCTION("CreateObjectLink",hFieldMap,hToolbar).
  DYNAMIC-FUNCTION("CreateObjectLink",hQuery,hToolbar).
  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse-2,hToolbar-2).
/*   DYNAMIC-FUNCTION("ReplaceObjectLink",hQuery,hToolbar). /* the detail win toolbar should now be linked to the browse */ */

  DYNAMIC-FUNCTION("setOrgWinSize", THIS-PROCEDURE:CURRENT-WINDOW,300,150,0,0).
          
  DYNAMIC-FUNCTION("InitTranslation",THIS-PROCEDURE:CURRENT-WINDOW).

END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LastRecord C-Win 
PROCEDURE LastRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN SUPER.
  DYNAMIC-FUNCTION('setColour').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LeaveOfFieldHandle C-Win 
PROCEDURE LeaveOfFieldHandle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihField AS HANDLE NO-UNDO.

IF NOT ihField:MODIFIED THEN RETURN.
ELSE DO WITH FRAME {&FRAME-NAME}:
  CASE ihField:NAME:
    WHEN "Beskr" THEN
      ASSIGN Bongtekst:SCREEN-VALUE = Beskr:SCREEN-VALUE 
             Etikettekst1:SCREEN-VALUE = Beskr:SCREEN-VALUE 
             .
      WHEN "Vg" THEN
        DO:
          VgBeskr:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldValues","VarGr","WHERE Vg = " + Vg:SCREEN-VALUE,"VgBeskr").
          Mva%#1:SCREEN-VALUE  = DYNAMIC-FUNCTION("getFieldValues","Moms",
                                                         "WHERE MomsKod = " + 
                                                         DYNAMIC-FUNCTION("getFieldValues","VarGr","WHERE Vg = " + Vg:SCREEN-VALUE,"MomsKod")
                                                         ,"MomsProc").
          APPLY 'value-changed' TO Innkjopspris#1.
        END.
      WHEN "LevNr" THEN LevNamn:SCREEN-VALUE      = DYNAMIC-FUNCTION("getFieldValues","LevBas","WHERE LevNr = " + LevNr:SCREEN-VALUE,"LevNamn").
      WHEN "ProdNr" THEN Beskrivelse:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldValues","Produsent","WHERE ProdNr = " + ProdNr:SCREEN-VALUE,"Beskrivelse").
      WHEN "Sasong" THEN SasBeskr:SCREEN-VALUE    = DYNAMIC-FUNCTION("getFieldValues","Sasong","WHERE Sasong = " + Sasong:SCREEN-VALUE,"SasBeskr").
      WHEN "VmId" THEN VmBeskrivelse:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldValues","Varemerke","WHERE VmId = " + VmId:SCREEN-VALUE,"Beskrivelse").
  END CASE.
END.

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
APPLY 'entry' TO Beskr IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NextRecord C-Win 
PROCEDURE NextRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN SUPER.
  DYNAMIC-FUNCTION('setColour').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenQuery C-Win 
PROCEDURE OpenQuery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR hKorrArtNr AS HANDLE NO-UNDO.

hKorrArtNr = hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("KorrArtikkelNr") NO-ERROR.
IF VALID-HANDLE(hKorrArtNr) AND
   hKorrArtNr:BUFFER-VALUE > 0 AND hKorrArtNr:BUFFER-VALUE NE ? AND
   DYNAMIC-FUNCTION("getFieldValues","ArtBas","WHERE ArtikkelNr = " + STRING(hKorrArtNr:BUFFER-VALUE),"ArtikkelNr") NE ? THEN
  iArtikkelNr = hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('KorrArtikkelNr'):BUFFER-VALUE.
ELSE
  iArtikkelNr = hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('ArtikkelNr'):BUFFER-VALUE.

 DYNAMIC-FUNCTION("setAttribute",hQuery-2,"parentlink"," WHERE ArtikkelNr = " + STRING(iArtikkelNr)).

 RUN SUPER.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrevRecord C-Win 
PROCEDURE PrevRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN SUPER.
  DYNAMIC-FUNCTION('setColour').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SaveRecord C-Win 
PROCEDURE SaveRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF FOCUS:TYPE = "FILL-IN" THEN
  RUN LeaveOfFieldHandle(FOCUS).
RUN SUPER.
setColour().
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sendArtikkelRegisterRecord C-Win 
PROCEDURE sendArtikkelRegisterRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN sendArtikkelRegisterRecord IN hParent NO-ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendVarebokRecord C-Win 
PROCEDURE SendVarebokRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF VALID-HANDLE(hParent) THEN RUN SendArtikkelRegisterRecord IN hParent.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettSTrekkodeRecord C-Win 
PROCEDURE SlettSTrekkodeRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AddStr C-Win 
FUNCTION AddStr RETURNS LOGICAL
  ( INPUT ifArtikkelNr AS DEC,
    INPUT icStorl      AS CHAR,
    INPUT ifPlukkAnt   AS DEC,
    input icAction     AS CHAR) :

RETURN DYNAMIC-FUNCTION("AddStr" IN hParent,ifArtikkelNr,icStorl,ifPlukkAnt,icAction).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AdjustBrowseColumns C-Win 
FUNCTION AdjustBrowseColumns RETURNS LOGICAL
  ( INPUT ihBrowse AS HANDLE,
    INPUT icBrowseName AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF icBrowseName = "rectBrowse" THEN
  ASSIGN
    hbcChkStrekNy    = ihBrowse:GET-BROWSE-COLUMN(4)
    hbfChkStrekNy    = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('ChkStrekNy')
    hbcChkStrekFeil  = ihBrowse:GET-BROWSE-COLUMN(5)
    hbfChkStrekFeil  = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('ChkStrekFeil')
    hbcChkStrekNy:COLUMN-FONT   = 8
    hbcChkStrekFeil:COLUMN-FONT = 8
  .
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION buildWidgetTable C-Win 
FUNCTION buildWidgetTable RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hObject AS HANDLE NO-UNDO.
ASSIGN 
  hObject = FRAME {&FRAME-NAME}:HANDLE:FIRST-CHILD
  hObject = hObject:FIRST-CHILD
.

DO WHILE VALID-HANDLE(hObject):
  IF can-do('FILL-IN,COMBO-BOX',hObject:TYPE) THEN
  DO:
    CREATE ttWidget.
    ASSIGN 
      ttWidget.cField = hObject:NAME
      ttWidget.hField = hObject:HANDLE
      ttWidget.bVPI   = INDEX(hObject:NAME,'-2') LE 0 
      ttWidget.cType  = hObject:DATA-TYPE
    .
  END.
  hObject = hObject:NEXT-SIBLING.
END.
 /*
 FOR EACH ttWidget:                                                 
   MESSAGE ttWidget.cField SKIP hField SKIP bVPI VIEW-AS ALERT-BOX. 
 END.
 */                                                              
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setColour C-Win 
FUNCTION setColour RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF BUFFER bttWidget FOR ttWidget.

  FOR EACH ttWidget WHERE ttWidget.bVPI NO-LOCK:
    FIND FIRST bttWidget WHERE bttWidget.cField = ttWidget.cField + '-2' NO-LOCK NO-ERROR.
    IF AVAIL bttWidget THEN
    DO:
      IF ttWidget.hField:SCREEN-VALUE = bttWidget.hField:SCREEN-VALUE THEN
      DO:
        ttWidget.hField:BGCOLOR = ?.
        NEXT.  /*Like verdier dropper vi å gjøre noe med*/
      END.
      CASE ttWidget.hField:DATA-TYPE:
        WHEN 'INTEGER' OR WHEN 'DECIMAL' THEN
        DO:
          IF INDEX(ttWidget.hField:NAME,'%') GT 0 THEN
          DO: /*Snu uttrykk, da lav verdi er dårlig*/
            ttWidget.hField:BGCOLOR =  IF DEC(ttWidget.hField:SCREEN-VALUE) < DEC(bttWidget.hField:SCREEN-VALUE) THEN
                                         12
                                       ELSE
                                         10.
          END.
          ELSE
          DO: /*Høyere verdi på VPI er gunstig*/
            ttWidget.hField:BGCOLOR =  IF DEC(ttWidget.hField:SCREEN-VALUE) > DEC(bttWidget.hField:SCREEN-VALUE) THEN
                                         12
                                       ELSE
                                         10.
          END.
        END.
        OTHERWISE
        DO:
          ttWidget.hField:BGCOLOR =  14.
        END.
      END CASE.
    END.
  END.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

