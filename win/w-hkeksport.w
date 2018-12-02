&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
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

DEFINE VARIABLE cSendesDir AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cSentrallagerliste AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cHKinst            AS CHARACTER  NO-UNDO.
DEFINE TEMP-TABLE TT_Elogg             NO-UNDO LIKE Elogg.
DEFINE TEMP-TABLE TT_Anv-Kod          LIKE Anv-Kod.
DEFINE TEMP-TABLE TT_Avdeling         LIKE Avdeling.
/* DEFINE TEMP-TABLE TT_Behandlingskode  LIKE Behandlingskode. */
DEFINE TEMP-TABLE TT_Farg             LIKE Farg.
DEFINE TEMP-TABLE TT_Feilkode         LIKE Feilkode.
DEFINE TEMP-TABLE TT_Foder            LIKE Foder.
DEFINE TEMP-TABLE TT_Handtering       LIKE Handtering.
DEFINE TEMP-TABLE TT_HuvGr            LIKE HuvGr.
DEFINE TEMP-TABLE TT_InnerSula        LIKE InnerSula.
DEFINE TEMP-TABLE TT_KasValuta        LIKE KasValuta.
DEFINE TEMP-TABLE TT_Kategori         LIKE Kategori.
DEFINE TEMP-TABLE TT_Kjede            LIKE Kjede.
DEFINE TEMP-TABLE TT_KjedeDistrikt    LIKE KjedeDistrikt.
DEFINE TEMP-TABLE TT_KjedeRegion      LIKE KjedeRegion.
DEFINE TEMP-TABLE TT_KjedensButikker  LIKE KjedensButikker.
DEFINE TEMP-TABLE TT_Klack            LIKE Klack.
DEFINE TEMP-TABLE TT_Kravkode         LIKE Kravkode.
DEFINE TEMP-TABLE TT_Last-Sko         LIKE Last-Sko.
DEFINE TEMP-TABLE TT_LevBas           LIKE LevBas.
DEFINE TEMP-TABLE TT_DefaultLevDato   LIKE DefaultLevDato.
DEFINE TEMP-TABLE TT_LevSort          LIKE LevSort.
DEFINE TEMP-TABLE TT_LevSant          LIKE LevSant.
DEFINE TEMP-TABLE TT_Material         LIKE Material.
DEFINE TEMP-TABLE TT_Moms             LIKE Moms.
DEFINE TEMP-TABLE TT_Ovandel          LIKE Ovandel.
DEFINE TEMP-TABLE TT_Post             LIKE Post.
DEFINE TEMP-TABLE TT_Prisgruppe       LIKE Prisgruppe.
DEFINE TEMP-TABLE TT_Prisprofil       LIKE Prisprofil.
DEFINE TEMP-TABLE TT_Produsent        LIKE Produsent.
DEFINE TEMP-TABLE TT_Prov             LIKE Prov.
DEFINE TEMP-TABLE TT_Rabatt           LIKE Rabatt.
DEFINE TEMP-TABLE TT_SaSong           LIKE SaSong.
DEFINE TEMP-TABLE TT_Slitsula         LIKE Slitsula.
DEFINE TEMP-TABLE TT_StrKonv          LIKE StrKonv.
DEFINE TEMP-TABLE TT_StrTStr          LIKE StrTStr.
DEFINE TEMP-TABLE TT_StrType          LIKE StrType.
DEFINE TEMP-TABLE TT_Valuta           LIKE Valuta.
DEFINE TEMP-TABLE TT_Varemerke        LIKE Varemerke.
DEFINE TEMP-TABLE TT_VarGr            LIKE VarGr.
DEFINE TEMP-TABLE TT_VgKat            LIKE VgKat.

DEF VAR iLevNrMax AS INT NO-UNDO.
DEF VAR iVPISendt AS INT NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 Btn_Help RECT-27 RECT-28 RECT-55 ~
RECT-56 RECT-57 RECT-58 RECT-59 RECT-60 RECT-61 TG-Velg BUTTON-Ok ~
TG-AlleAvdeling TG-AlleHuvGr TG-AlleKategori TG-AlleVarGr TG-AlleVgKat ~
TG-AlleSasong TG-AlleFeilkode TG-AlleHandtering TG-AlleKravKode ~
TG-AllePrisgruppe TG-AllePrisprofil TG-AlleProv TG-AlleRabatt TG-AlleMoms ~
TG-AllePost FI-Butikker FI-Valgte B-Kontroller B-StartEksport ~
TG-AlleStrKonv TG-AlleStrType TG-AlleFarg TG-AlleMaterial TG-AlleKlack ~
TG-AlleInnersula TG-AlleOvandel TG-AlleFoder TG-AlleSlitsula ~
TG-AlleLast-Sko TG-AlleAnv-Kod TG-AlleLevBas TG-AlleLevSort ~
TG-AlleProdusent TG-AlleVaremerke TG-AlleDefaultLevDato TG-AlleOnOff ~
B-Sendfiler B-VPIEksp TG-AlleKjede TG-AlleKjedeRegion TG-AlleKjedeDistrikt ~
TG-AlleKjedensButikker TG-AlleValuta TG-AlleKasValuta TG-AlleVPI FILL-IN-2 
&Scoped-Define DISPLAYED-OBJECTS TG-Avdeling TG-HuvGr TG-Kategori TG-VarGr ~
TG-VgKat TG-SaSong TG-Feilkode TG-Handtering TG-Kravkode TG-Prisgruppe ~
TG-Prisprofil TG-Prov TG-Rabatt TG-Moms TG-Post FI-Eksportdir FI-Filnavn ~
TG-Velg TG-SendAvdeling TG-SendHuvGr TG-SendKategori TG-SendVarGr ~
TG-SendVgKat TG-SendSaSong TG-SendFeilkode TG-SendHandtering ~
TG-SendKravkode TG-SendPrisgruppe TG-SendPrisprofil TG-SendProv ~
TG-SendRabatt TG-SendMoms TG-SendPost TG-AlleAvdeling TG-AlleHuvGr ~
TG-AlleKategori TG-AlleVarGr TG-AlleVgKat TG-AlleSasong TG-AlleFeilkode ~
TG-AlleHandtering TG-AlleKravKode TG-AllePrisgruppe TG-AllePrisprofil ~
TG-AlleProv TG-AlleRabatt TG-AlleMoms TG-AllePost TG-StrKonv TG-StrType ~
TG-Farg TG-Material TG-Klack TG-InnerSula TG-Ovandel TG-Foder TG-Slitsula ~
TG-Last-Sko TG-Anv-Kod TG-LevBas TG-LevSort TG-Produsent TG-Varemerke ~
TG-DefaultLevDato FI-Butikker FI-Valgte TG-SendStrKonv TG-SendStrType ~
TG-SendFarg TG-SendMaterial TG-SendKlack TG-SendInnersula TG-SendOvandel ~
TG-SendFoder TG-SendSlitSula TG-SendLast-Sko TG-SendAnv-Kod TG-SendLevBas ~
TG-SendLevSort TG-SendProdusent TG-SendVaremerke TG-SendDefaultLevDato ~
TG-AlleStrKonv TG-AlleStrType TG-AlleFarg TG-AlleMaterial TG-AlleKlack ~
TG-AlleInnersula TG-AlleOvandel TG-AlleFoder TG-AlleSlitsula ~
TG-AlleLast-Sko TG-AlleAnv-Kod TG-AlleLevBas TG-AlleLevSort ~
TG-AlleProdusent TG-AlleVaremerke TG-AlleDefaultLevDato TG-AlleOnOff ~
TG-Kjede TG-KjedeRegion TG-KjedeDistrikt TG-KjedensButikker TG-Valuta ~
TG-KasValuta TG-VPI TG-SendKjede TG-SendKjedeRegion TG-SendKjedeDistrikt ~
TG-SendKjedensButikker TG-SendValuta TG-SendKasValuta TG-SendVPI ~
TG-AlleKjede TG-AlleKjedeRegion TG-AlleKjedeDistrikt TG-AlleKjedensButikker ~
TG-AlleValuta TG-AlleKasValuta TG-AlleVPI FILL-IN-2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CanFindTTElogg C-Win 
FUNCTION CanFindTTElogg RETURNS LOGICAL
  ( INPUT cTabell AS CHARACTER,INPUT cVerdi AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FindFirst C-Win 
FUNCTION FindFirst RETURNS LOGICAL
  ( INPUT cTabellNavn AS CHARACTER, INPUT lAlle AS LOGICAL )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getAntLevSAnt C-Win 
FUNCTION getAntLevSAnt RETURNS INTEGER
  ( INPUT iLevNr AS INTEGER, INPUT cSortID AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getAntStrTStr C-Win 
FUNCTION getAntStrTStr RETURNS INTEGER
  ( INPUT iStrTypeID AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SendChecked C-Win 
FUNCTION SendChecked RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Kontroller 
     LABEL "Kontroller" 
     SIZE 15 BY 1.14.

DEFINE BUTTON B-Sendfiler 
     LABEL "Filer til butik" 
     SIZE 18 BY 1.14.

DEFINE BUTTON B-StartEksport 
     LABEL "Start eksport" 
     SIZE 15 BY 1.14.

DEFINE BUTTON B-VPIEksp 
     LABEL "VPI utvalg..." 
     SIZE 18 BY 1.14.

DEFINE BUTTON Btn_Help DEFAULT 
     IMAGE-UP FILE "icon\e-help":U NO-FOCUS FLAT-BUTTON
     LABEL "&Help" 
     SIZE 4.6 BY 1.05
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-Butikker 
     LABEL "Butikker" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-Ok 
     IMAGE-UP FILE "icon\e-exit":U NO-FOCUS FLAT-BUTTON
     LABEL "&Ok" 
     SIZE 4.6 BY 1.05 TOOLTIP "Lagre og avslutt".

DEFINE VARIABLE FI-Butikker AS CHARACTER FORMAT "X(1024)":U 
     LABEL "Butikker" 
     VIEW-AS FILL-IN 
     SIZE 25.8 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-Eksportdir AS CHARACTER FORMAT "X(256)":U 
     LABEL "Eksportdirectory" 
     VIEW-AS FILL-IN 
     SIZE 65.8 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-Filnavn AS CHARACTER FORMAT "X(256)":U 
     LABEL "Eksportert fil" 
     VIEW-AS FILL-IN 
     SIZE 25.8 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-Valgte AS CHARACTER FORMAT "X(256)":U 
     LABEL "Senest valgte" 
     VIEW-AS FILL-IN 
     SIZE 25.8 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FILL-IN-2 AS CHARACTER FORMAT "X(256)":U INITIAL " Endrede tabeller" 
      VIEW-AS TEXT 
     SIZE 18 BY .62
     BGCOLOR 10  NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 157 BY 19.33.

DEFINE RECTANGLE RECT-27
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 159 BY .1.

DEFINE RECTANGLE RECT-28
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 159 BY .1.

DEFINE RECTANGLE RECT-55
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE .2 BY 19.33.

DEFINE RECTANGLE RECT-56
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 53 BY .1.

DEFINE RECTANGLE RECT-57
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 105 BY .1.

DEFINE RECTANGLE RECT-58
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE .2 BY 19.33.

DEFINE RECTANGLE RECT-59
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 52 BY .1.

DEFINE RECTANGLE RECT-60
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 157 BY 4.29.

DEFINE RECTANGLE RECT-61
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 52 BY .1.

DEFINE VARIABLE TG-AlleAnv-Kod AS LOGICAL INITIAL no 
     LABEL "Alle" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .81 NO-UNDO.

DEFINE VARIABLE TG-AlleAvdeling AS LOGICAL INITIAL no 
     LABEL "Alle" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .81 NO-UNDO.

DEFINE VARIABLE TG-AlleDefaultLevDato AS LOGICAL INITIAL no 
     LABEL "Alle" 
     VIEW-AS TOGGLE-BOX
     SIZE 10 BY .81 NO-UNDO.

DEFINE VARIABLE TG-AlleFarg AS LOGICAL INITIAL no 
     LABEL "Alle" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .81 NO-UNDO.

DEFINE VARIABLE TG-AlleFeilkode AS LOGICAL INITIAL no 
     LABEL "Alle" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .81 NO-UNDO.

DEFINE VARIABLE TG-AlleFoder AS LOGICAL INITIAL no 
     LABEL "Alle" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .81 NO-UNDO.

DEFINE VARIABLE TG-AlleHandtering AS LOGICAL INITIAL no 
     LABEL "Alle" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .81 NO-UNDO.

DEFINE VARIABLE TG-AlleHuvGr AS LOGICAL INITIAL no 
     LABEL "Alle" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .81 NO-UNDO.

DEFINE VARIABLE TG-AlleInnersula AS LOGICAL INITIAL no 
     LABEL "Alle" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .81 NO-UNDO.

DEFINE VARIABLE TG-AlleKasValuta AS LOGICAL INITIAL no 
     LABEL "Alle" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .81 NO-UNDO.

DEFINE VARIABLE TG-AlleKategori AS LOGICAL INITIAL no 
     LABEL "Alle" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .81 NO-UNDO.

DEFINE VARIABLE TG-AlleKjede AS LOGICAL INITIAL no 
     LABEL "Alle" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .81 NO-UNDO.

DEFINE VARIABLE TG-AlleKjedeDistrikt AS LOGICAL INITIAL no 
     LABEL "Alle" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .81 NO-UNDO.

DEFINE VARIABLE TG-AlleKjedensButikker AS LOGICAL INITIAL no 
     LABEL "Alle" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .81 NO-UNDO.

DEFINE VARIABLE TG-AlleKjedeRegion AS LOGICAL INITIAL no 
     LABEL "Alle" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .81 NO-UNDO.

DEFINE VARIABLE TG-AlleKlack AS LOGICAL INITIAL no 
     LABEL "Alle" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .81 NO-UNDO.

DEFINE VARIABLE TG-AlleKravKode AS LOGICAL INITIAL no 
     LABEL "Alle" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .81 NO-UNDO.

DEFINE VARIABLE TG-AlleLast-Sko AS LOGICAL INITIAL no 
     LABEL "Alle" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .81 NO-UNDO.

DEFINE VARIABLE TG-AlleLevBas AS LOGICAL INITIAL no 
     LABEL "Alle" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .81 NO-UNDO.

DEFINE VARIABLE TG-AlleLevSort AS LOGICAL INITIAL no 
     LABEL "Alle" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .81 NO-UNDO.

DEFINE VARIABLE TG-AlleMaterial AS LOGICAL INITIAL no 
     LABEL "Alle" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .81 NO-UNDO.

DEFINE VARIABLE TG-AlleMoms AS LOGICAL INITIAL no 
     LABEL "Alle" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .81 NO-UNDO.

DEFINE VARIABLE TG-AlleOnOff AS LOGICAL INITIAL no 
     LABEL "Send - marker alle På/Av" 
     VIEW-AS TOGGLE-BOX
     SIZE 30.2 BY .81 NO-UNDO.

DEFINE VARIABLE TG-AlleOvandel AS LOGICAL INITIAL no 
     LABEL "Alle" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .81 NO-UNDO.

DEFINE VARIABLE TG-AllePost AS LOGICAL INITIAL no 
     LABEL "Alle" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .81 NO-UNDO.

DEFINE VARIABLE TG-AllePrisgruppe AS LOGICAL INITIAL no 
     LABEL "Alle" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .81 NO-UNDO.

DEFINE VARIABLE TG-AllePrisprofil AS LOGICAL INITIAL no 
     LABEL "Alle" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .81 NO-UNDO.

DEFINE VARIABLE TG-AlleProdusent AS LOGICAL INITIAL no 
     LABEL "Alle" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .81 NO-UNDO.

DEFINE VARIABLE TG-AlleProv AS LOGICAL INITIAL no 
     LABEL "Alle" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .81 NO-UNDO.

DEFINE VARIABLE TG-AlleRabatt AS LOGICAL INITIAL no 
     LABEL "Alle" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .81 NO-UNDO.

DEFINE VARIABLE TG-AlleSasong AS LOGICAL INITIAL no 
     LABEL "Alle" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .81 NO-UNDO.

DEFINE VARIABLE TG-AlleSlitsula AS LOGICAL INITIAL no 
     LABEL "Alle" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .81 NO-UNDO.

DEFINE VARIABLE TG-AlleStrKonv AS LOGICAL INITIAL no 
     LABEL "Alle" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .81 NO-UNDO.

DEFINE VARIABLE TG-AlleStrType AS LOGICAL INITIAL no 
     LABEL "Alle" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .81 NO-UNDO.

DEFINE VARIABLE TG-AlleValuta AS LOGICAL INITIAL no 
     LABEL "Alle" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .81 NO-UNDO.

DEFINE VARIABLE TG-AlleVaremerke AS LOGICAL INITIAL no 
     LABEL "Alle" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .81 NO-UNDO.

DEFINE VARIABLE TG-AlleVarGr AS LOGICAL INITIAL no 
     LABEL "Alle" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .81 NO-UNDO.

DEFINE VARIABLE TG-AlleVgKat AS LOGICAL INITIAL no 
     LABEL "Alle" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .81 NO-UNDO.

DEFINE VARIABLE TG-AlleVPI AS LOGICAL INITIAL no 
     LABEL "Alle" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .81 NO-UNDO.

DEFINE VARIABLE TG-Anv-Kod AS LOGICAL INITIAL no 
     LABEL "Anv-Kod" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE TG-Avdeling AS LOGICAL INITIAL no 
     LABEL "Avdeling" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE TG-DefaultLevDato AS LOGICAL INITIAL no 
     LABEL "Ukedag fri levering" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE TG-Farg AS LOGICAL INITIAL no 
     LABEL "Farger" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE TG-Feilkode AS LOGICAL INITIAL no 
     LABEL "Feilkode" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE TG-Foder AS LOGICAL INITIAL no 
     LABEL "Foder" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE TG-Handtering AS LOGICAL INITIAL no 
     LABEL "Håndteringskoder" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE TG-HuvGr AS LOGICAL INITIAL no 
     LABEL "Hovedgrupper" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE TG-InnerSula AS LOGICAL INITIAL no 
     LABEL "Innersåle" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE TG-KasValuta AS LOGICAL INITIAL no 
     LABEL "Kassevaluta" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE TG-Kategori AS LOGICAL INITIAL no 
     LABEL "Kategori" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE TG-Kjede AS LOGICAL INITIAL no 
     LABEL "Kjede" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE TG-KjedeDistrikt AS LOGICAL INITIAL no 
     LABEL "Kjededistrikt" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE TG-KjedensButikker AS LOGICAL INITIAL no 
     LABEL "Kjedebutikker" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE TG-KjedeRegion AS LOGICAL INITIAL no 
     LABEL "Kjederegion" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE TG-Klack AS LOGICAL INITIAL no 
     LABEL "Klack" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE TG-Kravkode AS LOGICAL INITIAL no 
     LABEL "Behandlingskoder" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE TG-Last-Sko AS LOGICAL INITIAL no 
     LABEL "Læst" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE TG-LevBas AS LOGICAL INITIAL no 
     LABEL "Leverandører" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE TG-LevSort AS LOGICAL INITIAL no 
     LABEL "Leverandørsortiment" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE TG-Material AS LOGICAL INITIAL no 
     LABEL "Material" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE TG-Moms AS LOGICAL INITIAL no 
     LABEL "Moms" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE TG-Ovandel AS LOGICAL INITIAL no 
     LABEL "Ovandel" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE TG-Post AS LOGICAL INITIAL no 
     LABEL "Post ?Byte av pnr ?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE TG-Prisgruppe AS LOGICAL INITIAL no 
     LABEL "Prisgruppe" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE TG-Prisprofil AS LOGICAL INITIAL no 
     LABEL "Prisprofil" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE TG-Produsent AS LOGICAL INITIAL no 
     LABEL "Produsent" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE TG-Prov AS LOGICAL INITIAL no 
     LABEL "Provisjon" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE TG-Rabatt AS LOGICAL INITIAL no 
     LABEL "Rabatt" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE TG-SaSong AS LOGICAL INITIAL no 
     LABEL "Sesongskoder" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE TG-SendAnv-Kod AS LOGICAL INITIAL no 
     LABEL "Send" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY .81 NO-UNDO.

DEFINE VARIABLE TG-SendAvdeling AS LOGICAL INITIAL no 
     LABEL "Send" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY .81 NO-UNDO.

DEFINE VARIABLE TG-SendDefaultLevDato AS LOGICAL INITIAL no 
     LABEL "Send" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE TG-SendFarg AS LOGICAL INITIAL no 
     LABEL "Send" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY .81 NO-UNDO.

DEFINE VARIABLE TG-SendFeilkode AS LOGICAL INITIAL no 
     LABEL "Send" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY .81 NO-UNDO.

DEFINE VARIABLE TG-SendFoder AS LOGICAL INITIAL no 
     LABEL "Send" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY .81 NO-UNDO.

DEFINE VARIABLE TG-SendHandtering AS LOGICAL INITIAL no 
     LABEL "Send" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY .81 NO-UNDO.

DEFINE VARIABLE TG-SendHuvGr AS LOGICAL INITIAL no 
     LABEL "Send" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY .81 NO-UNDO.

DEFINE VARIABLE TG-SendInnersula AS LOGICAL INITIAL no 
     LABEL "Send" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY .81 NO-UNDO.

DEFINE VARIABLE TG-SendKasValuta AS LOGICAL INITIAL no 
     LABEL "Send" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY .81 NO-UNDO.

DEFINE VARIABLE TG-SendKategori AS LOGICAL INITIAL no 
     LABEL "Send" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY .81 NO-UNDO.

DEFINE VARIABLE TG-SendKjede AS LOGICAL INITIAL no 
     LABEL "Send" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY .81 NO-UNDO.

DEFINE VARIABLE TG-SendKjedeDistrikt AS LOGICAL INITIAL no 
     LABEL "Send" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY .81 NO-UNDO.

DEFINE VARIABLE TG-SendKjedensButikker AS LOGICAL INITIAL no 
     LABEL "Send" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY .81 NO-UNDO.

DEFINE VARIABLE TG-SendKjedeRegion AS LOGICAL INITIAL no 
     LABEL "Send" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY .81 NO-UNDO.

DEFINE VARIABLE TG-SendKlack AS LOGICAL INITIAL no 
     LABEL "Send" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY .81 NO-UNDO.

DEFINE VARIABLE TG-SendKravkode AS LOGICAL INITIAL no 
     LABEL "Send" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY .81 NO-UNDO.

DEFINE VARIABLE TG-SendLast-Sko AS LOGICAL INITIAL no 
     LABEL "Send" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY .81 NO-UNDO.

DEFINE VARIABLE TG-SendLevBas AS LOGICAL INITIAL no 
     LABEL "Send" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY .81 NO-UNDO.

DEFINE VARIABLE TG-SendLevSort AS LOGICAL INITIAL no 
     LABEL "Send" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY .81 NO-UNDO.

DEFINE VARIABLE TG-SendMaterial AS LOGICAL INITIAL no 
     LABEL "Send" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY .81 NO-UNDO.

DEFINE VARIABLE TG-SendMoms AS LOGICAL INITIAL no 
     LABEL "Send" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY .81 NO-UNDO.

DEFINE VARIABLE TG-SendOvandel AS LOGICAL INITIAL no 
     LABEL "Send" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY .81 NO-UNDO.

DEFINE VARIABLE TG-SendPost AS LOGICAL INITIAL no 
     LABEL "Send" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY .81 NO-UNDO.

DEFINE VARIABLE TG-SendPrisgruppe AS LOGICAL INITIAL no 
     LABEL "Send" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY .81 NO-UNDO.

DEFINE VARIABLE TG-SendPrisprofil AS LOGICAL INITIAL no 
     LABEL "Send" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY .81 NO-UNDO.

DEFINE VARIABLE TG-SendProdusent AS LOGICAL INITIAL no 
     LABEL "Send" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY .81 NO-UNDO.

DEFINE VARIABLE TG-SendProv AS LOGICAL INITIAL no 
     LABEL "Send" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY .81 NO-UNDO.

DEFINE VARIABLE TG-SendRabatt AS LOGICAL INITIAL no 
     LABEL "Send" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY .81 NO-UNDO.

DEFINE VARIABLE TG-SendSaSong AS LOGICAL INITIAL no 
     LABEL "Send" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY .81 NO-UNDO.

DEFINE VARIABLE TG-SendSlitSula AS LOGICAL INITIAL no 
     LABEL "Send" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY .81 NO-UNDO.

DEFINE VARIABLE TG-SendStrKonv AS LOGICAL INITIAL no 
     LABEL "Send" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY .81 NO-UNDO.

DEFINE VARIABLE TG-SendStrType AS LOGICAL INITIAL no 
     LABEL "Send" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY .81 NO-UNDO.

DEFINE VARIABLE TG-SendValuta AS LOGICAL INITIAL no 
     LABEL "Send" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY .81 NO-UNDO.

DEFINE VARIABLE TG-SendVaremerke AS LOGICAL INITIAL no 
     LABEL "Send" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY .81 NO-UNDO.

DEFINE VARIABLE TG-SendVarGr AS LOGICAL INITIAL no 
     LABEL "Send" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY .81 NO-UNDO.

DEFINE VARIABLE TG-SendVgKat AS LOGICAL INITIAL no 
     LABEL "Send" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY .81 NO-UNDO.

DEFINE VARIABLE TG-SendVPI AS LOGICAL INITIAL no 
     LABEL "Send" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .81 NO-UNDO.

DEFINE VARIABLE TG-Slitsula AS LOGICAL INITIAL no 
     LABEL "Slitsåle" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE TG-StrKonv AS LOGICAL INITIAL no 
     LABEL "Størrelseskonv" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE TG-StrType AS LOGICAL INITIAL no 
     LABEL "Størrelsestype" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE TG-Valuta AS LOGICAL INITIAL no 
     LABEL "Kalkylevaluta" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE TG-Varemerke AS LOGICAL INITIAL no 
     LABEL "Varemerke" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE TG-VarGr AS LOGICAL INITIAL no 
     LABEL "Varegruppe" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE TG-Velg AS LOGICAL INITIAL no 
     LABEL "Velg butikker" 
     VIEW-AS TOGGLE-BOX
     SIZE 24.6 BY .81 NO-UNDO.

DEFINE VARIABLE TG-VgKat AS LOGICAL INITIAL no 
     LABEL "Varegruppe kategori" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE TG-VPI AS LOGICAL INITIAL no 
     LABEL "VPI" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     Btn_Help AT ROW 1.33 COL 150.6 NO-TAB-STOP 
     BUTTON-Butikker AT ROW 1.24 COL 3.4
     TG-Avdeling AT ROW 8.48 COL 6.4
     TG-HuvGr AT ROW 9.48 COL 6.4
     TG-Kategori AT ROW 10.48 COL 6.4
     TG-VarGr AT ROW 11.48 COL 6.4
     TG-VgKat AT ROW 12.48 COL 6.4
     TG-SaSong AT ROW 13.48 COL 6.4
     TG-Feilkode AT ROW 15.48 COL 6.4
     TG-Handtering AT ROW 16.48 COL 6.4
     TG-Kravkode AT ROW 17.48 COL 6.4
     TG-Prisgruppe AT ROW 20.48 COL 6.4
     TG-Prisprofil AT ROW 21.48 COL 6.4
     TG-Prov AT ROW 22.48 COL 6.4
     TG-Rabatt AT ROW 23.48 COL 6.4
     TG-Moms AT ROW 24.48 COL 6.4
     TG-Post AT ROW 25.48 COL 6.4
     FI-Eksportdir AT ROW 3.62 COL 23 COLON-ALIGNED
     FI-Filnavn AT ROW 4.71 COL 23 COLON-ALIGNED
     TG-Velg AT ROW 6 COL 25.4
     TG-SendAvdeling AT ROW 8.48 COL 33.4
     TG-SendHuvGr AT ROW 9.48 COL 33.4
     TG-SendKategori AT ROW 10.48 COL 33.4
     TG-SendVarGr AT ROW 11.48 COL 33.4
     TG-SendVgKat AT ROW 12.48 COL 33.4
     TG-SendSaSong AT ROW 13.48 COL 33.4
     TG-SendFeilkode AT ROW 15.48 COL 33.4
     TG-SendHandtering AT ROW 16.48 COL 33.4
     TG-SendKravkode AT ROW 17.48 COL 33.4
     TG-SendPrisgruppe AT ROW 20.48 COL 33.4
     TG-SendPrisprofil AT ROW 21.48 COL 33.4
     TG-SendProv AT ROW 22.48 COL 33.4
     TG-SendRabatt AT ROW 23.48 COL 33.4
     TG-SendMoms AT ROW 24.48 COL 33.4
     TG-SendPost AT ROW 25.48 COL 33.4
     BUTTON-Ok AT ROW 1.33 COL 155.2 NO-TAB-STOP 
     TG-AlleAvdeling AT ROW 8.48 COL 46.4
     TG-AlleHuvGr AT ROW 9.48 COL 46.4
     TG-AlleKategori AT ROW 10.48 COL 46.4
     TG-AlleVarGr AT ROW 11.48 COL 46.4
     TG-AlleVgKat AT ROW 12.48 COL 46.4
     TG-AlleSasong AT ROW 13.48 COL 46.4
     TG-AlleFeilkode AT ROW 15.48 COL 46.4
     TG-AlleHandtering AT ROW 16.48 COL 46.4
     TG-AlleKravKode AT ROW 17.48 COL 46.4
     TG-AllePrisgruppe AT ROW 20.48 COL 46.4
     TG-AllePrisprofil AT ROW 21.48 COL 46.4
     TG-AlleProv AT ROW 22.48 COL 46.4
     TG-AlleRabatt AT ROW 23.48 COL 46.4
     TG-AlleMoms AT ROW 24.48 COL 46.4
     TG-AllePost AT ROW 25.48 COL 46.4
     TG-StrKonv AT ROW 8.48 COL 59
     TG-StrType AT ROW 9.48 COL 59
     TG-Farg AT ROW 10.48 COL 59
     TG-Material AT ROW 11.48 COL 59
     TG-Klack AT ROW 12.48 COL 59
     TG-InnerSula AT ROW 13.48 COL 59
     TG-Ovandel AT ROW 14.48 COL 59
     TG-Foder AT ROW 15.48 COL 59
     TG-Slitsula AT ROW 16.48 COL 59
     TG-Last-Sko AT ROW 17.48 COL 59
     TG-Anv-Kod AT ROW 18.48 COL 59
     TG-LevBas AT ROW 20.48 COL 59
     TG-LevSort AT ROW 21.48 COL 59
     TG-Produsent AT ROW 22.48 COL 59
     TG-Varemerke AT ROW 23.48 COL 59
     TG-DefaultLevDato AT ROW 24.38 COL 59
     FI-Butikker AT ROW 4.71 COL 63 COLON-ALIGNED
     FI-Valgte AT ROW 5.86 COL 63 COLON-ALIGNED
     TG-SendStrKonv AT ROW 8.48 COL 84
     TG-SendStrType AT ROW 9.48 COL 84
     TG-SendFarg AT ROW 10.48 COL 84
     TG-SendMaterial AT ROW 11.48 COL 84
     TG-SendKlack AT ROW 12.48 COL 84
     TG-SendInnersula AT ROW 13.48 COL 84
     TG-SendOvandel AT ROW 14.48 COL 84
     TG-SendFoder AT ROW 15.48 COL 84
     TG-SendSlitSula AT ROW 16.48 COL 84
     TG-SendLast-Sko AT ROW 17.48 COL 84
     TG-SendAnv-Kod AT ROW 18.48 COL 84
     TG-SendLevBas AT ROW 20.48 COL 84
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME DEFAULT-FRAME
     TG-SendLevSort AT ROW 21.48 COL 84
     TG-SendProdusent AT ROW 22.48 COL 84
     TG-SendVaremerke AT ROW 23.48 COL 84
     TG-SendDefaultLevDato AT ROW 24.38 COL 84
     B-Kontroller AT ROW 3.52 COL 93.8
     B-StartEksport AT ROW 4.76 COL 93.8
     TG-AlleStrKonv AT ROW 8.48 COL 97
     TG-AlleStrType AT ROW 9.48 COL 97
     TG-AlleFarg AT ROW 10.48 COL 97
     TG-AlleMaterial AT ROW 11.48 COL 97
     TG-AlleKlack AT ROW 12.48 COL 97
     TG-AlleInnersula AT ROW 13.48 COL 97
     TG-AlleOvandel AT ROW 14.48 COL 97
     TG-AlleFoder AT ROW 15.48 COL 97
     TG-AlleSlitsula AT ROW 16.48 COL 97
     TG-AlleLast-Sko AT ROW 17.48 COL 97
     TG-AlleAnv-Kod AT ROW 18.48 COL 97
     TG-AlleLevBas AT ROW 20.48 COL 97
     TG-AlleLevSort AT ROW 21.48 COL 97
     TG-AlleProdusent AT ROW 22.48 COL 97
     TG-AlleVaremerke AT ROW 23.48 COL 97
     TG-AlleDefaultLevDato AT ROW 24.33 COL 97
     TG-AlleOnOff AT ROW 3.62 COL 110
     TG-Kjede AT ROW 8.48 COL 111.2
     TG-KjedeRegion AT ROW 9.48 COL 111.2
     TG-KjedeDistrikt AT ROW 10.48 COL 111.2
     TG-KjedensButikker AT ROW 11.48 COL 111.2
     TG-Valuta AT ROW 13.48 COL 111.2
     TG-KasValuta AT ROW 14.48 COL 111.2
     TG-VPI AT ROW 16.48 COL 111.2
     B-Sendfiler AT ROW 3.52 COL 139
     B-VPIEksp AT ROW 4.81 COL 139
     TG-SendKjede AT ROW 8.48 COL 136.2
     TG-SendKjedeRegion AT ROW 9.48 COL 136.2
     TG-SendKjedeDistrikt AT ROW 10.48 COL 136.2
     TG-SendKjedensButikker AT ROW 11.48 COL 136.2
     TG-SendValuta AT ROW 13.48 COL 136.2
     TG-SendKasValuta AT ROW 14.48 COL 136.2
     TG-SendVPI AT ROW 16.48 COL 136.2
     TG-AlleKjede AT ROW 8.48 COL 149.2
     TG-AlleKjedeRegion AT ROW 9.48 COL 149.2
     TG-AlleKjedeDistrikt AT ROW 10.48 COL 149.2
     TG-AlleKjedensButikker AT ROW 11.48 COL 149.2
     TG-AlleValuta AT ROW 13.48 COL 149.2
     TG-AlleKasValuta AT ROW 14.48 COL 149.2
     TG-AlleVPI AT ROW 16.48 COL 149.2
     FILL-IN-2 AT ROW 7.43 COL 44 COLON-ALIGNED NO-LABEL
     RECT-1 AT ROW 7.67 COL 3
     RECT-27 AT ROW 1.1 COL 1
     RECT-28 AT ROW 2.43 COL 1
     RECT-55 AT ROW 7.67 COL 55.6
     RECT-56 AT ROW 14.86 COL 3
     RECT-57 AT ROW 19.81 COL 3
     RECT-58 AT ROW 7.67 COL 107.8
     RECT-59 AT ROW 12.86 COL 107.8
     RECT-60 AT ROW 2.81 COL 3
     RECT-61 AT ROW 15.67 COL 107.8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


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
         TITLE              = "HK fildistribusjon"
         HEIGHT             = 26.52
         WIDTH              = 159.2
         MAX-HEIGHT         = 26.62
         MAX-WIDTH          = 160
         VIRTUAL-HEIGHT     = 26.62
         VIRTUAL-WIDTH      = 160
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
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME Size-to-Fit L-To-R,COLUMNS                                */
ASSIGN 
       FRAME DEFAULT-FRAME:SCROLLABLE       = FALSE.

/* SETTINGS FOR BUTTON BUTTON-Butikker IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BUTTON-Butikker:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       FI-Butikker:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN FI-Eksportdir IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Filnavn IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       FI-Valgte:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR TOGGLE-BOX TG-Anv-Kod IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-Avdeling IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-DefaultLevDato IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-Farg IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-Feilkode IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-Foder IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-Handtering IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-HuvGr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-InnerSula IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-KasValuta IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-Kategori IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-Kjede IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-KjedeDistrikt IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-KjedensButikker IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-KjedeRegion IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-Klack IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-Kravkode IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-Last-Sko IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-LevBas IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-LevSort IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-Material IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-Moms IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-Ovandel IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-Post IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-Prisgruppe IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-Prisprofil IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-Produsent IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-Prov IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-Rabatt IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-SaSong IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-SendAnv-Kod IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-SendAvdeling IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-SendDefaultLevDato IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-SendFarg IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-SendFeilkode IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-SendFoder IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-SendHandtering IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-SendHuvGr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-SendInnersula IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-SendKasValuta IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-SendKategori IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-SendKjede IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-SendKjedeDistrikt IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-SendKjedensButikker IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-SendKjedeRegion IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-SendKlack IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-SendKravkode IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-SendLast-Sko IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-SendLevBas IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-SendLevSort IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-SendMaterial IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-SendMoms IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-SendOvandel IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-SendPost IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-SendPrisgruppe IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-SendPrisprofil IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-SendProdusent IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-SendProv IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-SendRabatt IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-SendSaSong IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-SendSlitSula IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-SendStrKonv IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-SendStrType IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-SendValuta IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-SendVaremerke IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-SendVarGr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-SendVgKat IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-SendVPI IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-Slitsula IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-StrKonv IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-StrType IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-Valuta IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-Varemerke IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-VarGr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-VgKat IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-VPI IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _Query            is NOT OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* HK fildistribusjon */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* HK fildistribusjon */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Kontroller
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Kontroller C-Win
ON CHOOSE OF B-Kontroller IN FRAME DEFAULT-FRAME /* Kontroller */
DO:
  RUN Kontroller.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Sendfiler
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Sendfiler C-Win
ON CHOOSE OF B-Sendfiler IN FRAME DEFAULT-FRAME /* Filer til butik */
DO:
  RUN d-VisSendHKfiler.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-StartEksport
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-StartEksport C-Win
ON CHOOSE OF B-StartEksport IN FRAME DEFAULT-FRAME /* Start eksport */
DO:
    DEFINE VARIABLE cValgteListe AS CHARACTER  NO-UNDO.
    IF FI-Butikker = "" THEN DO:
        MESSAGE "Det finnes ingen butikker for eksport."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.
    {sww.i}
    
    IF SendChecked() THEN DO:
        ASSIGN FI-Valgte:SCREEN-VALUE = "".
        IF TG-Velg:CHECKED THEN DO:
            RUN d-TagButHKEksport.w (INPUT FI-Butikker, OUTPUT cValgteListe).
            IF RETURN-VALUE = "AVBRYT" THEN DO:
                MESSAGE "Avbrutt"
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
                RETURN NO-APPLY.
            END.
            ELSE IF cValgteListe = FI-Butikker THEN DO:
                MESSAGE "Valgte butikker = alle butikker"
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
                RETURN NO-APPLY.
            END.
        END.
        IF cValgteListe <> "" THEN
            ASSIGN FI-Valgte:SCREEN-VALUE = cValgteListe.
        RUN StartEksport.
    END.
    RUN SlettBehandlet.
    RUN Kontroller.
    ASSIGN TG-Velg:CHECKED = FALSE.
    {swn.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-VPIEksp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-VPIEksp C-Win
ON CHOOSE OF B-VPIEksp IN FRAME DEFAULT-FRAME /* VPI utvalg... */
DO:
  RUN dvpiutvalg.w.
  RUN Kontroller.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help C-Win
ON CHOOSE OF Btn_Help IN FRAME DEFAULT-FRAME /* Help */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
/*    {winhlp.i} */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Butikker
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Butikker C-Win
ON CHOOSE OF BUTTON-Butikker IN FRAME DEFAULT-FRAME /* Butikker */
DO:
    DEFINE VARIABLE recBrukerGrp AS RECID NO-UNDO.
    FIND FIRST BrukerGrp WHERE BrukerGrp.BrGrpNr = 99 NO-LOCK NO-ERROR.
    IF AVAIL BrukerGrp THEN DO:
        ASSIGN recBrukerGrp = RECID(BrukerGrp).
        RUN d-vbrukergrp(INPUT-OUTPUT recBrukerGrp, "HKEKSPORT").
        IF RETURN-VALUE <> "AVBRYT" THEN
            RUN InitCLliste.
    END.
    ELSE DO:
        MESSAGE "Ingen data finnes for å velge butikker for eksport." SKIP
            "Data eksporteres til alle sentrallagerbutikker."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Ok C-Win
ON CHOOSE OF BUTTON-Ok IN FRAME DEFAULT-FRAME /* Ok */
DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-AlleAnv-Kod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-AlleAnv-Kod C-Win
ON VALUE-CHANGED OF TG-AlleAnv-Kod IN FRAME DEFAULT-FRAME /* Alle */
DO:
  RUN NySlettAlle("Anv-Kod",STRING(SELF:CHECKED,"NY/SLETT")).
  RUN TG_Kontroll("Anv-Kod",TG-Anv-Kod:HANDLE,TG-SendAnv-Kod:HANDLE,?).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-AlleAvdeling
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-AlleAvdeling C-Win
ON VALUE-CHANGED OF TG-AlleAvdeling IN FRAME DEFAULT-FRAME /* Alle */
DO:
  RUN NySlettAlle("Avdeling",STRING(SELF:CHECKED,"NY/SLETT")).
  RUN TG_Kontroll("Avdeling",TG-Avdeling:HANDLE,TG-SendAvdeling:HANDLE,?).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-AlleDefaultLevDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-AlleDefaultLevDato C-Win
ON VALUE-CHANGED OF TG-AlleDefaultLevDato IN FRAME DEFAULT-FRAME /* Alle */
DO:
    RUN NySlettAlle("DefaultLevDato",STRING(SELF:CHECKED,"NY/SLETT")).
    RUN TG_Kontroll("DefaultLevDato",TG-DefaultLevDato:HANDLE,TG-SendDefaultLevDato:HANDLE,?).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-AlleFarg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-AlleFarg C-Win
ON VALUE-CHANGED OF TG-AlleFarg IN FRAME DEFAULT-FRAME /* Alle */
DO:
  RUN NySlettAlle("Farg",STRING(SELF:CHECKED,"NY/SLETT")).
  RUN TG_Kontroll("Farg",TG-Farg:HANDLE,TG-SendFarg:HANDLE,?).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-AlleFeilkode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-AlleFeilkode C-Win
ON VALUE-CHANGED OF TG-AlleFeilkode IN FRAME DEFAULT-FRAME /* Alle */
DO:
  RUN NySlettAlle("Feilkode",STRING(SELF:CHECKED,"NY/SLETT")).
  RUN TG_Kontroll("Feilkode",TG-Feilkode:HANDLE,TG-SendFeilkode:HANDLE,?).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-AlleFoder
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-AlleFoder C-Win
ON VALUE-CHANGED OF TG-AlleFoder IN FRAME DEFAULT-FRAME /* Alle */
DO:
  RUN NySlettAlle("Foder",STRING(SELF:CHECKED,"NY/SLETT")).
  RUN TG_Kontroll("Foder",TG-Foder:HANDLE,TG-SendFoder:HANDLE,?).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-AlleHandtering
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-AlleHandtering C-Win
ON VALUE-CHANGED OF TG-AlleHandtering IN FRAME DEFAULT-FRAME /* Alle */
DO:
  RUN NySlettAlle("Handtering",STRING(SELF:CHECKED,"NY/SLETT")).
  RUN TG_Kontroll("Handtering",TG-Handtering:HANDLE,TG-SendHandtering:HANDLE,?).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-AlleHuvGr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-AlleHuvGr C-Win
ON VALUE-CHANGED OF TG-AlleHuvGr IN FRAME DEFAULT-FRAME /* Alle */
DO:
  RUN NySlettAlle("HuvGr",STRING(SELF:CHECKED,"NY/SLETT")).
  RUN TG_Kontroll("HuvGr",TG-HuvGr:HANDLE,TG-SendHuvGr:HANDLE,?).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-AlleInnersula
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-AlleInnersula C-Win
ON VALUE-CHANGED OF TG-AlleInnersula IN FRAME DEFAULT-FRAME /* Alle */
DO:
  RUN NySlettAlle("Innersula",STRING(SELF:CHECKED,"NY/SLETT")).
  RUN TG_Kontroll("Innersula",TG-Innersula:HANDLE,TG-SendInnersula:HANDLE,?).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-AlleKasValuta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-AlleKasValuta C-Win
ON VALUE-CHANGED OF TG-AlleKasValuta IN FRAME DEFAULT-FRAME /* Alle */
DO:
  RUN NySlettAlle("KasValuta",STRING(SELF:CHECKED,"NY/SLETT")).
  RUN TG_Kontroll("KasValuta",TG-KasValuta:HANDLE,TG-SendKasValuta:HANDLE,?).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-AlleKategori
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-AlleKategori C-Win
ON VALUE-CHANGED OF TG-AlleKategori IN FRAME DEFAULT-FRAME /* Alle */
DO:
  RUN NySlettAlle("Kategori",STRING(SELF:CHECKED,"NY/SLETT")).
  RUN TG_Kontroll("Kategori",TG-Kategori:HANDLE,TG-SendKategori:HANDLE,?).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-AlleKjede
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-AlleKjede C-Win
ON VALUE-CHANGED OF TG-AlleKjede IN FRAME DEFAULT-FRAME /* Alle */
DO:
  RUN NySlettAlle("Kjede",STRING(SELF:CHECKED,"NY/SLETT")).
  RUN TG_Kontroll("Kjede",TG-Kjede:HANDLE,TG-SendKjede:HANDLE,?).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-AlleKjedeDistrikt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-AlleKjedeDistrikt C-Win
ON VALUE-CHANGED OF TG-AlleKjedeDistrikt IN FRAME DEFAULT-FRAME /* Alle */
DO:
  RUN NySlettAlle("KjedeDistrikt",STRING(SELF:CHECKED,"NY/SLETT")).
  RUN TG_Kontroll("KjedeDistrikt",TG-KjedeDistrikt:HANDLE,TG-SendKjedeDistrikt:HANDLE,?).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-AlleKjedensButikker
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-AlleKjedensButikker C-Win
ON VALUE-CHANGED OF TG-AlleKjedensButikker IN FRAME DEFAULT-FRAME /* Alle */
DO:
  RUN NySlettAlle("KjedensButikker",STRING(SELF:CHECKED,"NY/SLETT")).
  RUN TG_Kontroll("KjedensButikker",TG-KjedensButikker:HANDLE,TG-SendKjedensButikker:HANDLE,?).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-AlleKjedeRegion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-AlleKjedeRegion C-Win
ON VALUE-CHANGED OF TG-AlleKjedeRegion IN FRAME DEFAULT-FRAME /* Alle */
DO:
  RUN NySlettAlle("KjedeRegion",STRING(SELF:CHECKED,"NY/SLETT")).
  RUN TG_Kontroll("KjedeRegion",TG-KjedeRegion:HANDLE,TG-SendKjedeRegion:HANDLE,?).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-AlleKlack
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-AlleKlack C-Win
ON VALUE-CHANGED OF TG-AlleKlack IN FRAME DEFAULT-FRAME /* Alle */
DO:
  RUN NySlettAlle("Klack",STRING(SELF:CHECKED,"NY/SLETT")).
  RUN TG_Kontroll("Klack",TG-Klack:HANDLE,TG-SendKlack:HANDLE,?).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-AlleKravKode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-AlleKravKode C-Win
ON VALUE-CHANGED OF TG-AlleKravKode IN FRAME DEFAULT-FRAME /* Alle */
DO:
  RUN NySlettAlle("Kravkode",STRING(SELF:CHECKED,"NY/SLETT")).
  RUN TG_Kontroll("Kravkode",TG-Kravkode:HANDLE,TG-SendKravkode:HANDLE,?).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-AlleLast-Sko
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-AlleLast-Sko C-Win
ON VALUE-CHANGED OF TG-AlleLast-Sko IN FRAME DEFAULT-FRAME /* Alle */
DO:
  RUN NySlettAlle("Last-Sko",STRING(SELF:CHECKED,"NY/SLETT")).
  RUN TG_Kontroll("Last-Sko",TG-Last-Sko:HANDLE,TG-SendLast-Sko:HANDLE,?).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-AlleLevBas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-AlleLevBas C-Win
ON VALUE-CHANGED OF TG-AlleLevBas IN FRAME DEFAULT-FRAME /* Alle */
DO:
  RUN NySlettAlle("LevBas",STRING(SELF:CHECKED,"NY/SLETT")).
  RUN TG_Kontroll("LevBas",TG-LevBas:HANDLE,TG-SendLevBas:HANDLE,?).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-AlleLevSort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-AlleLevSort C-Win
ON VALUE-CHANGED OF TG-AlleLevSort IN FRAME DEFAULT-FRAME /* Alle */
DO:
  RUN NySlettAlle("LevSort",STRING(SELF:CHECKED,"NY/SLETT")).
  RUN TG_Kontroll("LevSort",TG-LevSort:HANDLE,TG-SendLevSort:HANDLE,?).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-AlleMaterial
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-AlleMaterial C-Win
ON VALUE-CHANGED OF TG-AlleMaterial IN FRAME DEFAULT-FRAME /* Alle */
DO:
  RUN NySlettAlle("Material",STRING(SELF:CHECKED,"NY/SLETT")).
  RUN TG_Kontroll("Material",TG-Material:HANDLE,TG-SendMaterial:HANDLE,?).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-AlleMoms
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-AlleMoms C-Win
ON VALUE-CHANGED OF TG-AlleMoms IN FRAME DEFAULT-FRAME /* Alle */
DO:
  RUN NySlettAlle("Moms",STRING(SELF:CHECKED,"NY/SLETT")).
  RUN TG_Kontroll("Moms",TG-Moms:HANDLE,TG-SendMoms:HANDLE,?).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-AlleOnOff
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-AlleOnOff C-Win
ON VALUE-CHANGED OF TG-AlleOnOff IN FRAME DEFAULT-FRAME /* Send - marker alle På/Av */
DO:
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
      TG-SendAnv-Kod:CHECKED         = TG-SendAnv-Kod:SENSITIVE AND SELF:CHECKED
      TG-SendAvdeling:CHECKED        = TG-SendAvdeling:SENSITIVE AND SELF:CHECKED
/*       TG-SendBehandlingskode:CHECKED = TG-SendBehandlingskode:SENSITIVE AND SELF:CHECKED */
      TG-SendFarg:CHECKED            = TG-SendFarg:SENSITIVE AND SELF:CHECKED
      TG-SendFeilkode:CHECKED        = TG-SendFeilkode:SENSITIVE AND SELF:CHECKED
      TG-SendFoder:CHECKED           = TG-SendFoder:SENSITIVE AND SELF:CHECKED
      TG-SendHandtering:CHECKED      = TG-SendHandtering:SENSITIVE AND SELF:CHECKED
      TG-SendHuvGr:CHECKED           = TG-SendHuvGr:SENSITIVE AND SELF:CHECKED
      TG-SendInnersula:CHECKED       = TG-SendInnersula:SENSITIVE AND SELF:CHECKED
      TG-SendKategori:CHECKED        = TG-SendKategori:SENSITIVE AND SELF:CHECKED
      TG-SendKlack:CHECKED           = TG-SendKlack:SENSITIVE AND SELF:CHECKED
      TG-SendKravkode:CHECKED        = TG-SendKravkode:SENSITIVE AND SELF:CHECKED
      TG-SendLast-Sko:CHECKED        = TG-SendLast-Sko:SENSITIVE AND SELF:CHECKED
      TG-SendLevBas:CHECKED          = TG-SendLevBas:SENSITIVE AND SELF:CHECKED
      TG-SendDefaultLevDato:CHECKED  = TG-SendDefaultLevDato:SENSITIVE AND SELF:CHECKED
      TG-SendLevSort:CHECKED         = TG-SendLevSort:SENSITIVE AND SELF:CHECKED
      TG-SendMaterial:CHECKED        = TG-SendMaterial:SENSITIVE AND SELF:CHECKED
      TG-SendMoms:CHECKED            = TG-SendMoms:SENSITIVE AND SELF:CHECKED
      TG-SendOvandel:CHECKED         = TG-SendOvandel:SENSITIVE AND SELF:CHECKED
      TG-SendPost:CHECKED            = TG-SendPost:SENSITIVE AND SELF:CHECKED
      TG-SendPrisgruppe:CHECKED      = TG-SendPrisgruppe:SENSITIVE AND SELF:CHECKED
      TG-SendPrisprofil:CHECKED      = TG-SendPrisprofil:SENSITIVE AND SELF:CHECKED
      TG-SendProdusent:CHECKED       = TG-SendProdusent:SENSITIVE AND SELF:CHECKED
      TG-SendProv:CHECKED            = TG-SendProv:SENSITIVE AND SELF:CHECKED
      TG-SendRabatt:CHECKED          = TG-SendRabatt:SENSITIVE AND SELF:CHECKED
      TG-SendSaSong:CHECKED          = TG-SendSaSong:SENSITIVE AND SELF:CHECKED
      TG-SendSlitSula:CHECKED        = TG-SendSlitSula:SENSITIVE AND SELF:CHECKED
      TG-SendStrKonv:CHECKED         = TG-SendStrKonv:SENSITIVE AND SELF:CHECKED
      TG-SendStrType:CHECKED         = TG-SendStrType:SENSITIVE AND SELF:CHECKED
      TG-SendVaremerke:CHECKED       = TG-SendVaremerke:SENSITIVE AND SELF:CHECKED
      TG-SendVarGr:CHECKED           = TG-SendVarGr:SENSITIVE AND SELF:CHECKED
      TG-SendVgKat:CHECKED           = TG-SendVgKat:SENSITIVE AND SELF:CHECKED.
      TG-SendVPI:CHECKED             = TG-SendVPI:SENSITIVE AND SELF:CHECKED.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-AlleOvandel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-AlleOvandel C-Win
ON VALUE-CHANGED OF TG-AlleOvandel IN FRAME DEFAULT-FRAME /* Alle */
DO:
  RUN NySlettAlle("Ovandel",STRING(SELF:CHECKED,"NY/SLETT")).
  RUN TG_Kontroll("Ovandel",TG-Ovandel:HANDLE,TG-SendOvandel:HANDLE,?).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-AllePost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-AllePost C-Win
ON VALUE-CHANGED OF TG-AllePost IN FRAME DEFAULT-FRAME /* Alle */
DO:
  RUN NySlettAlle("Post",STRING(SELF:CHECKED,"NY/SLETT")).
  RUN TG_Kontroll("Post",TG-Post:HANDLE,TG-SendPost:HANDLE,?).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-AllePrisgruppe
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-AllePrisgruppe C-Win
ON VALUE-CHANGED OF TG-AllePrisgruppe IN FRAME DEFAULT-FRAME /* Alle */
DO:
  RUN NySlettAlle("Prisgruppe",STRING(SELF:CHECKED,"NY/SLETT")).
  RUN TG_Kontroll("Prisgruppe",TG-Prisgruppe:HANDLE,TG-SendPrisgruppe:HANDLE,?).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-AllePrisprofil
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-AllePrisprofil C-Win
ON VALUE-CHANGED OF TG-AllePrisprofil IN FRAME DEFAULT-FRAME /* Alle */
DO:
  RUN NySlettAlle("Prisprofil",STRING(SELF:CHECKED,"NY/SLETT")).
  RUN TG_Kontroll("Prisprofil",TG-Prisprofil:HANDLE,TG-SendPrisprofil:HANDLE,?).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-AlleProdusent
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-AlleProdusent C-Win
ON VALUE-CHANGED OF TG-AlleProdusent IN FRAME DEFAULT-FRAME /* Alle */
DO:
  RUN NySlettAlle("Produsent",STRING(SELF:CHECKED,"NY/SLETT")).
  RUN TG_Kontroll("Produsent",TG-Produsent:HANDLE,TG-SendProdusent:HANDLE,?).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-AlleProv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-AlleProv C-Win
ON VALUE-CHANGED OF TG-AlleProv IN FRAME DEFAULT-FRAME /* Alle */
DO:
  RUN NySlettAlle("Prov",STRING(SELF:CHECKED,"NY/SLETT")).
  RUN TG_Kontroll("Prov",TG-Prov:HANDLE,TG-SendProv:HANDLE,?).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-AlleRabatt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-AlleRabatt C-Win
ON VALUE-CHANGED OF TG-AlleRabatt IN FRAME DEFAULT-FRAME /* Alle */
DO:
  RUN NySlettAlle("Rabatt",STRING(SELF:CHECKED,"NY/SLETT")).
  RUN TG_Kontroll("Rabatt",TG-Rabatt:HANDLE,TG-SendRabatt:HANDLE,?).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-AlleSasong
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-AlleSasong C-Win
ON VALUE-CHANGED OF TG-AlleSasong IN FRAME DEFAULT-FRAME /* Alle */
DO:
  RUN NySlettAlle("Sasong",STRING(SELF:CHECKED,"NY/SLETT")).
  RUN TG_Kontroll("Sasong",TG-Sasong:HANDLE,TG-SendSasong:HANDLE,?).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-AlleSlitsula
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-AlleSlitsula C-Win
ON VALUE-CHANGED OF TG-AlleSlitsula IN FRAME DEFAULT-FRAME /* Alle */
DO:
  RUN NySlettAlle("Slitsula",STRING(SELF:CHECKED,"NY/SLETT")).
  RUN TG_Kontroll("Slitsula",TG-Slitsula:HANDLE,TG-SendSlitsula:HANDLE,?).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-AlleStrKonv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-AlleStrKonv C-Win
ON VALUE-CHANGED OF TG-AlleStrKonv IN FRAME DEFAULT-FRAME /* Alle */
DO:
  RUN NySlettAlle("StrKonv",STRING(SELF:CHECKED,"NY/SLETT")).
  RUN TG_Kontroll("StrKonv",TG-StrKonv:HANDLE,TG-SendStrKonv:HANDLE,?).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-AlleStrType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-AlleStrType C-Win
ON VALUE-CHANGED OF TG-AlleStrType IN FRAME DEFAULT-FRAME /* Alle */
DO:
  RUN NySlettAlle("StrType",STRING(SELF:CHECKED,"NY/SLETT")).
  RUN TG_Kontroll("StrType",TG-StrType:HANDLE,TG-SendStrType:HANDLE,?).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-AlleValuta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-AlleValuta C-Win
ON VALUE-CHANGED OF TG-AlleValuta IN FRAME DEFAULT-FRAME /* Alle */
DO:
  RUN NySlettAlle("Valuta",STRING(SELF:CHECKED,"NY/SLETT")).
  RUN TG_Kontroll("Valuta",TG-Valuta:HANDLE,TG-SendValuta:HANDLE,?).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-AlleVaremerke
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-AlleVaremerke C-Win
ON VALUE-CHANGED OF TG-AlleVaremerke IN FRAME DEFAULT-FRAME /* Alle */
DO:
  RUN NySlettAlle("Varemerke",STRING(SELF:CHECKED,"NY/SLETT")).
  RUN TG_Kontroll("Varemerke",TG-Varemerke:HANDLE,TG-SendVaremerke:HANDLE,?).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-AlleVarGr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-AlleVarGr C-Win
ON VALUE-CHANGED OF TG-AlleVarGr IN FRAME DEFAULT-FRAME /* Alle */
DO:
  RUN NySlettAlle("VarGr",STRING(SELF:CHECKED,"NY/SLETT")).
  RUN TG_Kontroll("VarGr",TG-VarGr:HANDLE,TG-SendVarGr:HANDLE,?).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-AlleVgKat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-AlleVgKat C-Win
ON VALUE-CHANGED OF TG-AlleVgKat IN FRAME DEFAULT-FRAME /* Alle */
DO:
  RUN NySlettAlle("VgKat",STRING(SELF:CHECKED,"NY/SLETT")).
  RUN TG_Kontroll("VgKat",TG-VgKat:HANDLE,TG-SendVgKat:HANDLE,?).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-AlleVPI
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-AlleVPI C-Win
ON VALUE-CHANGED OF TG-AlleVPI IN FRAME DEFAULT-FRAME /* Alle */
DO:
  RUN NySlettAlle("VPIArtBas",STRING(SELF:CHECKED,"NY/SLETT")).
  RUN TG_Kontroll("VPIArtbas",TG-VPI:HANDLE,TG-SendVPI:HANDLE,?).
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-SendAnv-Kod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-SendAnv-Kod C-Win
ON VALUE-CHANGED OF TG-SendAnv-Kod IN FRAME DEFAULT-FRAME /* Send */
DO:
    IF NOT SELF:CHECKED THEN 
      ASSIGN TG-AlleOnOff:CHECKED = FALSE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-SendAvdeling
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-SendAvdeling C-Win
ON VALUE-CHANGED OF TG-SendAvdeling IN FRAME DEFAULT-FRAME /* Send */
DO:
    IF NOT SELF:CHECKED THEN 
      ASSIGN TG-AlleOnOff:CHECKED = FALSE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-SendFarg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-SendFarg C-Win
ON VALUE-CHANGED OF TG-SendFarg IN FRAME DEFAULT-FRAME /* Send */
DO:
    IF NOT SELF:CHECKED THEN 
      ASSIGN TG-AlleOnOff:CHECKED = FALSE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-SendFeilkode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-SendFeilkode C-Win
ON VALUE-CHANGED OF TG-SendFeilkode IN FRAME DEFAULT-FRAME /* Send */
DO:
    IF NOT SELF:CHECKED THEN 
      ASSIGN TG-AlleOnOff:CHECKED = FALSE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-SendFoder
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-SendFoder C-Win
ON VALUE-CHANGED OF TG-SendFoder IN FRAME DEFAULT-FRAME /* Send */
DO:
    IF NOT SELF:CHECKED THEN 
      ASSIGN TG-AlleOnOff:CHECKED = FALSE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-SendHandtering
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-SendHandtering C-Win
ON VALUE-CHANGED OF TG-SendHandtering IN FRAME DEFAULT-FRAME /* Send */
DO:
    IF NOT SELF:CHECKED THEN 
      ASSIGN TG-AlleOnOff:CHECKED = FALSE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-SendHuvGr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-SendHuvGr C-Win
ON VALUE-CHANGED OF TG-SendHuvGr IN FRAME DEFAULT-FRAME /* Send */
DO:
    IF NOT SELF:CHECKED THEN 
      ASSIGN TG-AlleOnOff:CHECKED = FALSE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-SendInnersula
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-SendInnersula C-Win
ON VALUE-CHANGED OF TG-SendInnersula IN FRAME DEFAULT-FRAME /* Send */
DO:
    IF NOT SELF:CHECKED THEN 
      ASSIGN TG-AlleOnOff:CHECKED = FALSE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-SendKasValuta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-SendKasValuta C-Win
ON VALUE-CHANGED OF TG-SendKasValuta IN FRAME DEFAULT-FRAME /* Send */
DO:
    IF NOT SELF:CHECKED THEN
      ASSIGN TG-AlleOnOff:CHECKED = FALSE.
    IF TG-SendKasValuta:SENSITIVE AND SELF:CHECKED THEN DO:
        ASSIGN TG-SendKasValuta:CHECKED = TRUE.
        APPLY "VALUE-CHANGED" TO TG-SendKasValuta.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-SendKategori
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-SendKategori C-Win
ON VALUE-CHANGED OF TG-SendKategori IN FRAME DEFAULT-FRAME /* Send */
DO:
    IF NOT SELF:CHECKED THEN 
      ASSIGN TG-AlleOnOff:CHECKED = FALSE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-SendKjede
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-SendKjede C-Win
ON VALUE-CHANGED OF TG-SendKjede IN FRAME DEFAULT-FRAME /* Send */
DO:
    IF NOT SELF:CHECKED THEN
      ASSIGN TG-AlleOnOff:CHECKED = FALSE.
    IF NOT SELF:CHECKED THEN DO:
        ASSIGN TG-SendKjedeRegion:CHECKED = FALSE.
        APPLY "VALUE-CHANGED" TO TG-SendKjedeRegion.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-SendKjedeDistrikt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-SendKjedeDistrikt C-Win
ON VALUE-CHANGED OF TG-SendKjedeDistrikt IN FRAME DEFAULT-FRAME /* Send */
DO:
    IF NOT SELF:CHECKED THEN
      ASSIGN TG-AlleOnOff:CHECKED = FALSE.
    IF TG-SendKjedeRegion:SENSITIVE AND SELF:CHECKED THEN DO:
        IF SELF:CHECKED THEN
            ASSIGN TG-SendKjedeRegion:CHECKED = TRUE.
        APPLY "VALUE-CHANGED" TO TG-SendKjedeRegion.
    END.
    ELSE IF NOT SELF:CHECKED THEN
        ASSIGN TG-SendKjedensButikker:CHECKED = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-SendKjedensButikker
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-SendKjedensButikker C-Win
ON VALUE-CHANGED OF TG-SendKjedensButikker IN FRAME DEFAULT-FRAME /* Send */
DO:
    IF NOT SELF:CHECKED THEN
      ASSIGN TG-AlleOnOff:CHECKED = FALSE.
    IF TG-SendKjedeDistrikt:SENSITIVE AND SELF:CHECKED THEN DO:
        ASSIGN TG-SendKjedeDistrikt:CHECKED = TRUE.
        APPLY "VALUE-CHANGED" TO TG-SendKjedeDistrikt.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-SendKjedeRegion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-SendKjedeRegion C-Win
ON VALUE-CHANGED OF TG-SendKjedeRegion IN FRAME DEFAULT-FRAME /* Send */
DO:
    IF NOT SELF:CHECKED THEN
      ASSIGN TG-AlleOnOff:CHECKED = FALSE.
    IF TG-SendKjede:SENSITIVE AND SELF:CHECKED THEN DO:
        ASSIGN TG-SendKjede:CHECKED = TRUE.
        APPLY "VALUE-CHANGED" TO TG-SendKjede.
    END.
    ELSE IF NOT SELF:CHECKED THEN DO:
        ASSIGN TG-SendKjedeDistrikt:CHECKED = FALSE.
        APPLY "VALUE-CHANGED" TO TG-SendKjedeDistrikt.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-SendKlack
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-SendKlack C-Win
ON VALUE-CHANGED OF TG-SendKlack IN FRAME DEFAULT-FRAME /* Send */
DO:
    IF NOT SELF:CHECKED THEN 
      ASSIGN TG-AlleOnOff:CHECKED = FALSE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-SendKravkode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-SendKravkode C-Win
ON VALUE-CHANGED OF TG-SendKravkode IN FRAME DEFAULT-FRAME /* Send */
DO:
    IF NOT SELF:CHECKED THEN 
      ASSIGN TG-AlleOnOff:CHECKED = FALSE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-SendLast-Sko
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-SendLast-Sko C-Win
ON VALUE-CHANGED OF TG-SendLast-Sko IN FRAME DEFAULT-FRAME /* Send */
DO:
    IF NOT SELF:CHECKED THEN 
      ASSIGN TG-AlleOnOff:CHECKED = FALSE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-SendLevBas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-SendLevBas C-Win
ON VALUE-CHANGED OF TG-SendLevBas IN FRAME DEFAULT-FRAME /* Send */
DO:
    IF NOT SELF:CHECKED THEN 
      ASSIGN TG-AlleOnOff:CHECKED = FALSE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-SendLevSort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-SendLevSort C-Win
ON VALUE-CHANGED OF TG-SendLevSort IN FRAME DEFAULT-FRAME /* Send */
DO:
    IF NOT SELF:CHECKED THEN 
      ASSIGN TG-AlleOnOff:CHECKED = FALSE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-SendMaterial
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-SendMaterial C-Win
ON VALUE-CHANGED OF TG-SendMaterial IN FRAME DEFAULT-FRAME /* Send */
DO:
    IF NOT SELF:CHECKED THEN 
      ASSIGN TG-AlleOnOff:CHECKED = FALSE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-SendMoms
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-SendMoms C-Win
ON VALUE-CHANGED OF TG-SendMoms IN FRAME DEFAULT-FRAME /* Send */
DO:
    IF NOT SELF:CHECKED THEN 
      ASSIGN TG-AlleOnOff:CHECKED = FALSE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-SendOvandel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-SendOvandel C-Win
ON VALUE-CHANGED OF TG-SendOvandel IN FRAME DEFAULT-FRAME /* Send */
DO:
    IF NOT SELF:CHECKED THEN 
      ASSIGN TG-AlleOnOff:CHECKED = FALSE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-SendPost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-SendPost C-Win
ON VALUE-CHANGED OF TG-SendPost IN FRAME DEFAULT-FRAME /* Send */
DO:
    IF NOT SELF:CHECKED THEN 
      ASSIGN TG-AlleOnOff:CHECKED = FALSE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-SendPrisgruppe
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-SendPrisgruppe C-Win
ON VALUE-CHANGED OF TG-SendPrisgruppe IN FRAME DEFAULT-FRAME /* Send */
DO:
    IF NOT SELF:CHECKED THEN 
      ASSIGN TG-AlleOnOff:CHECKED = FALSE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-SendPrisprofil
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-SendPrisprofil C-Win
ON VALUE-CHANGED OF TG-SendPrisprofil IN FRAME DEFAULT-FRAME /* Send */
DO:
    IF NOT SELF:CHECKED THEN 
      ASSIGN TG-AlleOnOff:CHECKED = FALSE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-SendProdusent
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-SendProdusent C-Win
ON VALUE-CHANGED OF TG-SendProdusent IN FRAME DEFAULT-FRAME /* Send */
DO:
    IF NOT SELF:CHECKED THEN 
      ASSIGN TG-AlleOnOff:CHECKED = FALSE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-SendProv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-SendProv C-Win
ON VALUE-CHANGED OF TG-SendProv IN FRAME DEFAULT-FRAME /* Send */
DO:
    IF NOT SELF:CHECKED THEN 
      ASSIGN TG-AlleOnOff:CHECKED = FALSE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-SendRabatt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-SendRabatt C-Win
ON VALUE-CHANGED OF TG-SendRabatt IN FRAME DEFAULT-FRAME /* Send */
DO:
    IF NOT SELF:CHECKED THEN 
      ASSIGN TG-AlleOnOff:CHECKED = FALSE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-SendSaSong
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-SendSaSong C-Win
ON VALUE-CHANGED OF TG-SendSaSong IN FRAME DEFAULT-FRAME /* Send */
DO:
    IF NOT SELF:CHECKED THEN 
      ASSIGN TG-AlleOnOff:CHECKED = FALSE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-SendSlitSula
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-SendSlitSula C-Win
ON VALUE-CHANGED OF TG-SendSlitSula IN FRAME DEFAULT-FRAME /* Send */
DO:
    IF NOT SELF:CHECKED THEN 
      ASSIGN TG-AlleOnOff:CHECKED = FALSE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-SendStrKonv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-SendStrKonv C-Win
ON VALUE-CHANGED OF TG-SendStrKonv IN FRAME DEFAULT-FRAME /* Send */
DO:
    IF NOT SELF:CHECKED THEN 
      ASSIGN TG-AlleOnOff:CHECKED = FALSE.
  IF TG-SendStrType:SENSITIVE THEN
      ASSIGN TG-SendStrType:CHECKED = SELF:CHECKED.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-SendStrType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-SendStrType C-Win
ON VALUE-CHANGED OF TG-SendStrType IN FRAME DEFAULT-FRAME /* Send */
DO:
    IF NOT SELF:CHECKED THEN 
      ASSIGN TG-AlleOnOff:CHECKED = FALSE.
  IF TG-SendStrKonv:SENSITIVE THEN
      ASSIGN TG-SendStrKonv:CHECKED = SELF:CHECKED.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-SendValuta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-SendValuta C-Win
ON VALUE-CHANGED OF TG-SendValuta IN FRAME DEFAULT-FRAME /* Send */
DO:
    IF NOT SELF:CHECKED THEN
      ASSIGN TG-AlleOnOff:CHECKED = FALSE.
    IF TG-SendValuta:SENSITIVE AND SELF:CHECKED THEN DO:
        ASSIGN TG-SendValuta:CHECKED = TRUE.
        APPLY "VALUE-CHANGED" TO TG-SendValuta.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-SendVaremerke
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-SendVaremerke C-Win
ON VALUE-CHANGED OF TG-SendVaremerke IN FRAME DEFAULT-FRAME /* Send */
DO:
    IF NOT SELF:CHECKED THEN 
      ASSIGN TG-AlleOnOff:CHECKED = FALSE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-SendVarGr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-SendVarGr C-Win
ON VALUE-CHANGED OF TG-SendVarGr IN FRAME DEFAULT-FRAME /* Send */
DO:
  IF NOT SELF:CHECKED THEN 
      ASSIGN TG-AlleOnOff:CHECKED = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-SendVgKat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-SendVgKat C-Win
ON VALUE-CHANGED OF TG-SendVgKat IN FRAME DEFAULT-FRAME /* Send */
DO:
    IF NOT SELF:CHECKED THEN 
      ASSIGN TG-AlleOnOff:CHECKED = FALSE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-SendVPI
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-SendVPI C-Win
ON VALUE-CHANGED OF TG-SendVPI IN FRAME DEFAULT-FRAME /* Send */
DO:
    IF NOT SELF:CHECKED THEN
      ASSIGN TG-AlleOnOff:CHECKED = FALSE.
    IF NOT SELF:CHECKED THEN DO:
        ASSIGN TG-SendVPI:CHECKED = FALSE.
        APPLY "VALUE-CHANGED" TO TG-SendVPI.
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
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Setter høyeste levnr som skal eksporteres. */
{syspara.i 16 2 2 iLevNrMax INT}
IF iLevNrMax = 0 THEN
    iLevNrMax = 999.

/* Eksportkatalog */
{syspara.i 1 1 51 cSendesDir}
IF cSendesDir = "" THEN "C:\home\lindbak\sendes".

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    RUN InitCLliste.
  ASSIGN FI-Eksportdir = cSendesDir.

  RUN enable_UI.
  {syspara.i 1 1 18 cHKinst}
  /*
  IF NOT CAN-DO("yes,TRU,1",cHKinst) THEN DO:
      MESSAGE "Ikke HK-installasjon"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY "CLOSE" TO THIS-PROCEDURE.
  END.
  */
  {lng.i} /* Oversettelse */
  APPLY "CHOOSE" TO B-Kontroller.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.
{swn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  DISPLAY TG-Avdeling TG-HuvGr TG-Kategori TG-VarGr TG-VgKat TG-SaSong 
          TG-Feilkode TG-Handtering TG-Kravkode TG-Prisgruppe TG-Prisprofil 
          TG-Prov TG-Rabatt TG-Moms TG-Post FI-Eksportdir FI-Filnavn TG-Velg 
          TG-SendAvdeling TG-SendHuvGr TG-SendKategori TG-SendVarGr TG-SendVgKat 
          TG-SendSaSong TG-SendFeilkode TG-SendHandtering TG-SendKravkode 
          TG-SendPrisgruppe TG-SendPrisprofil TG-SendProv TG-SendRabatt 
          TG-SendMoms TG-SendPost TG-AlleAvdeling TG-AlleHuvGr TG-AlleKategori 
          TG-AlleVarGr TG-AlleVgKat TG-AlleSasong TG-AlleFeilkode 
          TG-AlleHandtering TG-AlleKravKode TG-AllePrisgruppe TG-AllePrisprofil 
          TG-AlleProv TG-AlleRabatt TG-AlleMoms TG-AllePost TG-StrKonv 
          TG-StrType TG-Farg TG-Material TG-Klack TG-InnerSula TG-Ovandel 
          TG-Foder TG-Slitsula TG-Last-Sko TG-Anv-Kod TG-LevBas TG-LevSort 
          TG-Produsent TG-Varemerke TG-DefaultLevDato FI-Butikker FI-Valgte 
          TG-SendStrKonv TG-SendStrType TG-SendFarg TG-SendMaterial TG-SendKlack 
          TG-SendInnersula TG-SendOvandel TG-SendFoder TG-SendSlitSula 
          TG-SendLast-Sko TG-SendAnv-Kod TG-SendLevBas TG-SendLevSort 
          TG-SendProdusent TG-SendVaremerke TG-SendDefaultLevDato TG-AlleStrKonv 
          TG-AlleStrType TG-AlleFarg TG-AlleMaterial TG-AlleKlack 
          TG-AlleInnersula TG-AlleOvandel TG-AlleFoder TG-AlleSlitsula 
          TG-AlleLast-Sko TG-AlleAnv-Kod TG-AlleLevBas TG-AlleLevSort 
          TG-AlleProdusent TG-AlleVaremerke TG-AlleDefaultLevDato TG-AlleOnOff 
          TG-Kjede TG-KjedeRegion TG-KjedeDistrikt TG-KjedensButikker TG-Valuta 
          TG-KasValuta TG-VPI TG-SendKjede TG-SendKjedeRegion 
          TG-SendKjedeDistrikt TG-SendKjedensButikker TG-SendValuta 
          TG-SendKasValuta TG-SendVPI TG-AlleKjede TG-AlleKjedeRegion 
          TG-AlleKjedeDistrikt TG-AlleKjedensButikker TG-AlleValuta 
          TG-AlleKasValuta TG-AlleVPI FILL-IN-2 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-1 Btn_Help RECT-27 RECT-28 RECT-55 RECT-56 RECT-57 RECT-58 
         RECT-59 RECT-60 RECT-61 TG-Velg BUTTON-Ok TG-AlleAvdeling TG-AlleHuvGr 
         TG-AlleKategori TG-AlleVarGr TG-AlleVgKat TG-AlleSasong 
         TG-AlleFeilkode TG-AlleHandtering TG-AlleKravKode TG-AllePrisgruppe 
         TG-AllePrisprofil TG-AlleProv TG-AlleRabatt TG-AlleMoms TG-AllePost 
         FI-Butikker FI-Valgte B-Kontroller B-StartEksport TG-AlleStrKonv 
         TG-AlleStrType TG-AlleFarg TG-AlleMaterial TG-AlleKlack 
         TG-AlleInnersula TG-AlleOvandel TG-AlleFoder TG-AlleSlitsula 
         TG-AlleLast-Sko TG-AlleAnv-Kod TG-AlleLevBas TG-AlleLevSort 
         TG-AlleProdusent TG-AlleVaremerke TG-AlleDefaultLevDato TG-AlleOnOff 
         B-Sendfiler B-VPIEksp TG-AlleKjede TG-AlleKjedeRegion 
         TG-AlleKjedeDistrikt TG-AlleKjedensButikker TG-AlleValuta 
         TG-AlleKasValuta TG-AlleVPI FILL-IN-2 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getAntLevKontakt C-Win 
PROCEDURE getAntLevKontakt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT  PARAMETER piLevNr  AS INT NO-UNDO.
  DEF OUTPUT PARAMETER piantall AS INT NO-UNDO.

  piantall = 0.
  FOR EACH LevKontakt NO-LOCK WHERE
      LevKontakt.LevNr = piLevNr:
      piantall = piAntall + 1.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitCLliste C-Win 
PROCEDURE InitCLliste :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cIkkeCL               AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cIkkeButikk           AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTmpSentrallagerliste AS CHARACTER  NO-UNDO.
  FOR EACH Butiker WHERE Butiker.Sentrallager = TRUE AND Butiker.ApningsDato <> ? AND
          (Butiker.NedlagtDato = ? OR Butiker.NedlagtDato >= TODAY) NO-LOCK.
      ASSIGN cTmpSentrallagerliste = cTmpSentrallagerliste +
             (IF cTmpSentrallagerliste = "" THEN "" ELSE ",") + STRING(Butiker.Butik).
  END.
  ASSIGN cSentrallagerliste = cTmpSentrallagerliste
         FI-Butikker = cSentrallagerliste
         FI-Butikker:SCREEN-VALUE IN FRAME {&FRAME-NAME} = cSentrallagerliste.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitCLlisteOLD C-Win 
PROCEDURE InitCLlisteOLD :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cIkkeCL               AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cIkkeButikk           AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTmpSentrallagerliste AS CHARACTER  NO-UNDO.
    FIND BrukerGrp WHERE BrukerGrp.BrGrpNr = 99 NO-LOCK NO-ERROR.
    IF AVAIL BrukerGrp THEN DO:
        FOR EACH ButikkTilgang OF BrukerGrp NO-LOCK:
            FIND Butiker WHERE Butiker.Butik = ButikkTilgang.Butik NO-LOCK NO-ERROR.
            IF NOT AVAIL Butiker THEN
                ASSIGN cIkkeButikk = cIkkeButikk + (IF cIkkeButikk = "" THEN "" ELSE ",")
                                             + STRING(ButikkTilgang.Butik).
            ELSE DO:
/*                 IF Butiker.Sentrallager = FALSE THEN                               */
/*                     ASSIGN cIkkeCL = cIkkeCL +                                     */
/*                        (IF cIkkeCL = "" THEN "" ELSE ",") + STRING(Butiker.Butik). */
/*                 ELSE                                                               */
                    ASSIGN cTmpSentrallagerliste = cTmpSentrallagerliste +
                           (IF cTmpSentrallagerliste = "" THEN "" ELSE ",") + STRING(Butiker.Butik).
            END.
        END.
        IF cIkkeButikk <> "" OR cIkkeCL <> "" OR cTmpSentrallagerliste = "" THEN DO:
/*             MESSAGE  cIkkeButikk SKIP              */
/*                 cIkkeCL SKIP                       */
/*                 cTmpSentrallagerliste                 */
/*                 VIEW-AS ALERT-BOX INFO BUTTONS OK. */
            MESSAGE "Feil i registrerte butikker for overføring av data til butikker." +
                 (IF cIkkeButikk <> "" THEN 
                     CHR(10) + "Butikk(er) " + cIkkeButikk + " finnes ikke." ELSE "") +
                (IF cIkkeCL <> "" THEN 
                    CHR(10) + "Butikk(er) " + cIkkeCL + " ikke sentrallager." ELSE "") +
                (IF cTmpSentrallagerliste = "" THEN 
                    CHR(10) + "Ingen sentrallagerbutikk registrert for overføring." ELSE "") 
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RETURN NO-APPLY.
        END.
    END.
    ELSE DO:
        FOR EACH Butiker WHERE Butiker.Sentrallager = TRUE.
            ASSIGN cTmpSentrallagerliste = cTmpSentrallagerliste +
                   (IF cTmpSentrallagerliste = "" THEN "" ELSE ",") + STRING(Butiker.Butik).
        END.
    END.
    ASSIGN cSentrallagerliste = cTmpSentrallagerliste
           FI-Butikker = cSentrallagerliste
           FI-Butikker:SCREEN-VALUE IN FRAME {&FRAME-NAME} = cSentrallagerliste.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Kontroller C-Win 
PROCEDURE Kontroller :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
      
      RUN TG_Kontroll("Anv-Kod",TG-Anv-Kod:HANDLE,TG-SendAnv-Kod:HANDLE,TG-AlleAnv-Kod:HANDLE).
      RUN TG_Kontroll("Avdeling",TG-Avdeling:HANDLE,TG-SendAvdeling:HANDLE,TG-AlleAvdeling:HANDLE).
/*       RUN TG_Kontroll("Behandlingskode",TG-Behandlingskode:HANDLE,TG-SendBehandlingskode:HANDLE,TG-AlleBehandlingskode:HANDLE). */
      RUN TG_Kontroll("Farg",TG-Farg:HANDLE,TG-SendFarg:HANDLE,TG-AlleFarg:HANDLE).
      RUN TG_Kontroll("FeilKode",TG-FeilKode:HANDLE,TG-SendFeilKode:HANDLE,TG-AlleFeilKode:HANDLE).
      RUN TG_Kontroll("Foder",TG-Foder:HANDLE,TG-SendFoder:HANDLE,TG-AlleFoder:HANDLE).
      RUN TG_Kontroll("Handtering",TG-Handtering:HANDLE,TG-SendHandtering:HANDLE,TG-AlleHandtering:HANDLE).
      RUN TG_Kontroll("HuvGr",TG-HuvGr:HANDLE,TG-SendHuvGr:HANDLE,TG-AlleHuvGr:HANDLE).
      RUN TG_Kontroll("Innersula",TG-Innersula:HANDLE,TG-SendInnersula:HANDLE,TG-AlleInnersula:HANDLE).
      RUN TG_Kontroll("KasValuta",TG-KasValuta:HANDLE,TG-SendKasValuta:HANDLE,TG-AlleKasValuta:HANDLE).
      RUN TG_Kontroll("Kategori",TG-Kategori:HANDLE,TG-SendKategori:HANDLE,TG-AlleKategori:HANDLE).
      RUN TG_Kontroll("Klack",TG-Klack:HANDLE,TG-SendKlack:HANDLE,TG-AlleKlack:HANDLE).
      RUN TG_Kontroll("Kjede",TG-Kjede:HANDLE,TG-SendKjede:HANDLE,TG-AlleKjede:HANDLE).
      RUN TG_Kontroll("KjedeDistrikt",TG-KjedeDistrikt:HANDLE,TG-SendKjedeDistrikt:HANDLE,TG-AlleKjedeDistrikt:HANDLE).
      RUN TG_Kontroll("KjedeRegion",TG-KjedeRegion:HANDLE,TG-SendKjedeRegion:HANDLE,TG-AlleKjedeRegion:HANDLE).
      RUN TG_Kontroll("KjedensButikker",TG-KjedensButikker:HANDLE,TG-SendKjedensButikker:HANDLE,TG-AlleKjedensButikker:HANDLE).
      RUN TG_Kontroll("Kravkode",TG-Kravkode:HANDLE,TG-SendKravkode:HANDLE,TG-AlleKravkode:HANDLE).
      RUN TG_Kontroll("Last-Sko",TG-Last-Sko:HANDLE,TG-SendLast-Sko:HANDLE,TG-AlleLast-Sko:HANDLE).
      RUN TG_Kontroll("LevBas",TG-LevBas:HANDLE,TG-SendLevBas:HANDLE,TG-AlleLevBas:HANDLE).
      RUN TG_Kontroll("DefaultLevDato",TG-DefaultLevDato:HANDLE,TG-SendDefaultLevDato:HANDLE,TG-AlleDefaultLevDato:HANDLE).
      RUN TG_Kontroll("LevSort",TG-LevSort:HANDLE,TG-SendLevSort:HANDLE,TG-AlleLevSort:HANDLE).
      RUN TG_Kontroll("Material",TG-Material:HANDLE,TG-SendMaterial:HANDLE,TG-AlleMaterial:HANDLE).
      RUN TG_Kontroll("Moms",TG-Moms:HANDLE,TG-SendMoms:HANDLE,TG-AlleMoms:HANDLE).
      RUN TG_Kontroll("Ovandel",TG-Ovandel:HANDLE,TG-SendOvandel:HANDLE,TG-AlleOvandel:HANDLE).
      RUN TG_Kontroll("Post",TG-Post:HANDLE,TG-SendPost:HANDLE,TG-AllePost:HANDLE).
      RUN TG_Kontroll("Prisgruppe",TG-Prisgruppe:HANDLE,TG-SendPrisgruppe:HANDLE,TG-AllePrisgruppe:HANDLE).
      RUN TG_Kontroll("Prisprofil",TG-Prisprofil:HANDLE,TG-SendPrisprofil:HANDLE,TG-AllePrisprofil:HANDLE).
      RUN TG_Kontroll("Produsent",TG-Produsent:HANDLE,TG-SendProdusent:HANDLE,TG-AlleProdusent:HANDLE).
      RUN TG_Kontroll("Prov",TG-Prov:HANDLE,TG-SendProv:HANDLE,TG-AlleProv:HANDLE).
      RUN TG_Kontroll("Rabatt",TG-Rabatt:HANDLE,TG-SendRabatt:HANDLE,TG-AlleRabatt:HANDLE).
      RUN TG_Kontroll("Sasong",TG-Sasong:HANDLE,TG-SendSasong:HANDLE,TG-AlleSasong:HANDLE).
      RUN TG_Kontroll("Slitsula",TG-Slitsula:HANDLE,TG-SendSlitsula:HANDLE,TG-AlleSlitsula:HANDLE).
      RUN TG_Kontroll("StrKonv",TG-StrKonv:HANDLE,TG-SendStrKonv:HANDLE,TG-AlleStrKonv:HANDLE).
      RUN TG_Kontroll("StrType",TG-StrType:HANDLE,TG-SendStrType:HANDLE,TG-AlleStrType:HANDLE).
      RUN TG_Kontroll("Valuta",TG-Valuta:HANDLE,TG-SendValuta:HANDLE,TG-AlleValuta:HANDLE).
      RUN TG_Kontroll("Varemerke",TG-Varemerke:HANDLE,TG-SendVaremerke:HANDLE,TG-AlleVaremerke:HANDLE).
      RUN TG_Kontroll("VarGr",TG-VarGr:HANDLE,TG-SendVarGr:HANDLE,TG-AlleVarGr:HANDLE).
      RUN TG_Kontroll("VgKat",TG-VgKat:HANDLE,TG-SendVgKat:HANDLE,TG-AlleVgKat:HANDLE).
      RUN TG_Kontroll("VPIArtBas",TG-VPI:HANDLE,TG-SendVPI:HANDLE,TG-AlleVPI:HANDLE).
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KontrollerORG C-Win 
PROCEDURE KontrollerORG PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*   DO WITH FRAME {&FRAME-NAME}:                                                                 */
/*       ASSIGN TG-Anv-Kod:CHECKED       = FindFirst("Anv-Kod")                                   */
/*              TG-SendAnv-Kod:SENSITIVE = TG-Anv-Kod:CHECKED                                     */
/*              TG-SendAnv-Kod:CHECKED   = IF TG-SendAnv-Kod:SENSITIVE THEN                       */
/*                             TG-AlleOnOff:CHECKED ELSE FALSE                                    */
/*                                                                                                */
/*              TG-Avdeling:CHECKED       = FindFirst("Avdeling")                                 */
/*              TG-SendAvdeling:SENSITIVE = TG-Avdeling:CHECKED                                   */
/*              TG-SendAvdeling:CHECKED   = IF TG-SendAvdeling:SENSITIVE THEN                     */
/*                             TG-AlleOnOff:CHECKED ELSE FALSE                                    */
/*                                                                                                */
/* /*              TG-Behandlingskode:CHECKED       = FindFirst("Behandlingskode")             */ */
/* /*              TG-SendBehandlingskode:SENSITIVE = TG-Behandlingskode:CHECKED               */ */
/* /*              TG-SendBehandlingskode:CHECKED   = IF TG-SendBehandlingskode:SENSITIVE THEN */ */
/* /*                             TG-AlleOnOff:CHECKED ELSE FALSE                              */ */
/*                                                                                                */
/*              TG-Farg:CHECKED       = FindFirst("Farg")                                         */
/*              TG-SendFarg:SENSITIVE = TG-Farg:CHECKED                                           */
/*              TG-SendFarg:CHECKED   = IF TG-SendFarg:SENSITIVE THEN                             */
/*                             TG-AlleOnOff:CHECKED ELSE FALSE                                    */
/*                                                                                                */
/*              TG-Feilkode:CHECKED       = FindFirst("FeilKode")                                 */
/*              TG-SendFeilkode:SENSITIVE = TG-Feilkode:CHECKED                                   */
/*              TG-SendFeilkode:CHECKED   = IF TG-SendFeilkode:SENSITIVE THEN                     */
/*                             TG-AlleOnOff:CHECKED ELSE FALSE                                    */
/*                                                                                                */
/*              TG-Foder:CHECKED       = FindFirst("Foder")                                       */
/*              TG-SendFoder:SENSITIVE = TG-Foder:CHECKED                                         */
/*              TG-SendFoder:CHECKED   = IF TG-SendFoder:SENSITIVE THEN                           */
/*                             TG-AlleOnOff:CHECKED ELSE FALSE                                    */
/*                                                                                                */
/*              TG-Handtering:CHECKED       = FindFirst("Handtering")                             */
/*              TG-SendHandtering:SENSITIVE = TG-Handtering:CHECKED                               */
/*              TG-SendHandtering:CHECKED   = IF TG-SendHandtering:SENSITIVE THEN                 */
/*                             TG-AlleOnOff:CHECKED ELSE FALSE                                    */
/*                                                                                                */
/*              TG-HuvGr:CHECKED       = FindFirst("HuvGr")                                       */
/*              TG-SendHuvGr:SENSITIVE = TG-HuvGr:CHECKED                                         */
/*              TG-SendHuvGr:CHECKED   = IF TG-SendHuvGr:SENSITIVE THEN                           */
/*                             TG-AlleOnOff:CHECKED ELSE FALSE                                    */
/*                                                                                                */
/*              TG-Innersula:CHECKED       = FindFirst("Innersula")                               */
/*              TG-SendInnersula:SENSITIVE = TG-Innersula:CHECKED                                 */
/*              TG-SendInnersula:CHECKED   = IF TG-SendInnersula:SENSITIVE THEN                   */
/*                             TG-AlleOnOff:CHECKED ELSE FALSE                                    */
/*                                                                                                */
/*              TG-Kategori:CHECKED       = FindFirst("Kategori")                                 */
/*              TG-SendKategori:SENSITIVE = TG-Kategori:CHECKED                                   */
/*              TG-SendKategori:CHECKED   = IF TG-SendKategori:SENSITIVE THEN                     */
/*                             TG-AlleOnOff:CHECKED ELSE FALSE                                    */
/*                                                                                                */
/*              TG-Klack:CHECKED       = FindFirst("Klack")                                       */
/*              TG-SendKlack:SENSITIVE = TG-Klack:CHECKED                                         */
/*              TG-SendKlack:CHECKED   = IF TG-SendKlack:SENSITIVE THEN                           */
/*                             TG-AlleOnOff:CHECKED ELSE FALSE                                    */
/*                                                                                                */
/*              TG-Kravkode:CHECKED       = FindFirst("Kravkode")                                 */
/*              TG-SendKravkode:SENSITIVE = TG-Kravkode:CHECKED                                   */
/*              TG-SendKravkode:CHECKED   = IF TG-SendKravkode:SENSITIVE THEN                     */
/*                             TG-AlleOnOff:CHECKED ELSE FALSE                                    */
/*                                                                                                */
/*              TG-Last-Sko:CHECKED       = FindFirst("Last-Sko")                                 */
/*              TG-SendLast-Sko:SENSITIVE = TG-Last-Sko:CHECKED                                   */
/*              TG-SendLast-Sko:CHECKED   = IF TG-SendLast-Sko:SENSITIVE THEN                     */
/*                             TG-AlleOnOff:CHECKED ELSE FALSE                                    */
/*                                                                                                */
/*              TG-LevBas:CHECKED       = FindFirst("LevBas")                                     */
/*              TG-SendLevBas:SENSITIVE = TG-LevBas:CHECKED                                       */
/*              TG-SendLevBas:CHECKED   = IF TG-SendLevBas:SENSITIVE THEN                         */
/*                             TG-AlleOnOff:CHECKED ELSE FALSE                                    */
/*                                                                                                */
/*              TG-LevSort:CHECKED       = FindFirst("LevSort")                                   */
/*              TG-SendLevSort:SENSITIVE = TG-LevSort:CHECKED                                     */
/*              TG-SendLevSort:CHECKED   = IF TG-SendLevSort:SENSITIVE THEN                       */
/*                             TG-AlleOnOff:CHECKED ELSE FALSE                                    */
/*                                                                                                */
/*              TG-Material:CHECKED       = FindFirst("Material")                                 */
/*              TG-SendMaterial:SENSITIVE = TG-Material:CHECKED                                   */
/*              TG-SendMaterial:CHECKED   = IF TG-SendMaterial:SENSITIVE THEN                     */
/*                             TG-AlleOnOff:CHECKED ELSE FALSE                                    */
/*                                                                                                */
/*              TG-Moms:CHECKED       = FindFirst("Moms")                                         */
/*              TG-SendMoms:SENSITIVE = TG-Moms:CHECKED                                           */
/*              TG-SendMoms:CHECKED   = IF TG-SendMoms:SENSITIVE THEN                             */
/*                             TG-AlleOnOff:CHECKED ELSE FALSE                                    */
/*                                                                                                */
/*              TG-Ovandel:CHECKED       = FindFirst("Ovandel")                                   */
/*              TG-SendOvandel:SENSITIVE = TG-Ovandel:CHECKED                                     */
/*              TG-SendOvandel:CHECKED   = IF TG-SendOvandel:SENSITIVE THEN                       */
/*                             TG-AlleOnOff:CHECKED ELSE FALSE                                    */
/*                                                                                                */
/*              TG-Post:CHECKED       = FindFirst("Post")                                         */
/*              TG-SendPost:SENSITIVE = TG-Post:CHECKED                                           */
/*              TG-SendPost:CHECKED   = IF TG-SendPost:SENSITIVE THEN                             */
/*                             TG-AlleOnOff:CHECKED ELSE FALSE                                    */
/*                                                                                                */
/*              TG-Prisgruppe:CHECKED       = FindFirst("Prisgruppe")                             */
/*              TG-SendPrisgruppe:SENSITIVE = TG-Prisgruppe:CHECKED                               */
/*              TG-SendPrisgruppe:CHECKED   = IF TG-SendPrisgruppe:SENSITIVE THEN                 */
/*                             TG-AlleOnOff:CHECKED ELSE FALSE                                    */
/*                                                                                                */
/*              TG-Prisprofil:CHECKED       = FindFirst("Prisprofil")                             */
/*              TG-SendPrisprofil:SENSITIVE = TG-Prisprofil:CHECKED                               */
/*              TG-SendPrisprofil:CHECKED   = IF TG-SendPrisprofil:SENSITIVE THEN                 */
/*                             TG-AlleOnOff:CHECKED ELSE FALSE                                    */
/*                                                                                                */
/*              TG-Produsent:CHECKED       = FindFirst("Produsent")                               */
/*              TG-SendProdusent:SENSITIVE = TG-Produsent:CHECKED                                 */
/*              TG-SendProdusent:CHECKED   = IF TG-SendProdusent:SENSITIVE THEN                   */
/*                             TG-AlleOnOff:CHECKED ELSE FALSE                                    */
/*                                                                                                */
/*              TG-Prov:CHECKED       = FindFirst("Prov")                                         */
/*              TG-SendProv:SENSITIVE = TG-Prov:CHECKED                                           */
/*              TG-SendProv:CHECKED   = IF TG-SendProv:SENSITIVE THEN                             */
/*                             TG-AlleOnOff:CHECKED ELSE FALSE                                    */
/*                                                                                                */
/*              TG-Rabatt:CHECKED       = FindFirst("Rabatt")                                     */
/*              TG-SendRabatt:SENSITIVE = TG-Rabatt:CHECKED                                       */
/*              TG-SendRabatt:CHECKED   = IF TG-SendRabatt:SENSITIVE THEN                         */
/*                             TG-AlleOnOff:CHECKED ELSE FALSE                                    */
/*                                                                                                */
/*              TG-Sasong:CHECKED       = FindFirst("Sasong")                                     */
/*              TG-SendSasong:SENSITIVE = TG-Sasong:CHECKED                                       */
/*              TG-SendSasong:CHECKED   = IF TG-SendSasong:SENSITIVE THEN                         */
/*                             TG-AlleOnOff:CHECKED ELSE FALSE                                    */
/*                                                                                                */
/*              TG-Slitsula:CHECKED       = FindFirst("Slitsula")                                 */
/*              TG-SendSlitsula:SENSITIVE = TG-Slitsula:CHECKED                                   */
/*              TG-SendSlitsula:CHECKED   = IF TG-SendSlitsula:SENSITIVE THEN                     */
/*                             TG-AlleOnOff:CHECKED ELSE FALSE                                    */
/*                                                                                                */
/* /*              TG-Valuta:CHECKED       = FindFirst("Valuta")             */                   */
/* /*              TG-SendValuta:SENSITIVE = TG-Valuta:CHECKED               */                   */
/* /*              TG-SendValuta:CHECKED   = IF TG-SendValuta:SENSITIVE THEN */                   */
/* /*                             TG-AlleOnOff:CHECKED ELSE FALSE            */                   */
/*                                                                                                */
/*              TG-StrKonv:CHECKED       = FindFirst("StrKonv")                                   */
/*              TG-SendStrKonv:SENSITIVE = TG-StrKonv:CHECKED                                     */
/*              TG-SendStrKonv:CHECKED   = IF TG-SendStrKonv:SENSITIVE THEN                       */
/*                             TG-AlleOnOff:CHECKED ELSE FALSE                                    */
/*                                                                                                */
/*              TG-StrType:CHECKED       = FindFirst("StrType")                                   */
/*              TG-SendStrType:SENSITIVE = TG-StrType:CHECKED                                     */
/*              TG-SendStrType:CHECKED   = IF TG-SendStrType:SENSITIVE THEN                       */
/*                             TG-AlleOnOff:CHECKED ELSE FALSE                                    */
/*                                                                                                */
/*              TG-Varemerke:CHECKED       = FindFirst("Varemerke")                               */
/*              TG-SendVaremerke:SENSITIVE = TG-Varemerke:CHECKED                                 */
/*              TG-SendVaremerke:CHECKED   = IF TG-SendVaremerke:SENSITIVE THEN                   */
/*                             TG-AlleOnOff:CHECKED ELSE FALSE                                    */
/*                                                                                                */
/*              TG-VarGr:CHECKED       = FindFirst("VarGr")                                       */
/*              TG-SendVarGr:SENSITIVE = TG-VarGr:CHECKED                                         */
/*              TG-SendVarGr:CHECKED   = IF TG-SendVarGr:SENSITIVE THEN                           */
/*                             TG-AlleOnOff:CHECKED ELSE FALSE                                    */
/*                                                                                                */
/*              TG-VgKat:CHECKED       = FindFirst("VgKat")                                       */
/*              TG-SendVgKat:SENSITIVE = TG-VgKat:CHECKED                                         */
/*              TG-SendVgKat:CHECKED   = IF TG-SendVgKat:SENSITIVE THEN                           */
/*                             TG-AlleOnOff:CHECKED ELSE FALSE.                                   */
/*   END.                                                                                         */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KopierElogg C-Win 
PROCEDURE KopierElogg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER cTabellNavn AS CHARACTER  NO-UNDO.
  DEFINE OUTPUT PARAMETER iAntSlett   AS INTEGER    NO-UNDO.
  DEFINE OUTPUT PARAMETER iAntNyEndre AS INTEGER    NO-UNDO.
  
  FOR EACH ELogg WHERE ELogg.TabellNavn = cTabellNavn AND ELogg.EksterntSystem = "POS":
      CREATE TT_Elogg.
      BUFFER-COPY Elogg TO TT_Elogg.
      RELEASE TT_Elogg.
      ASSIGN Elogg.Behandlet = TRUE
             iAntSlett   = iAntSlett   + IF Elogg.EndringsType = 3 THEN 1 ELSE 0
             iAntNyEndre = iAntNyEndre + IF Elogg.EndringsType = 1 AND ELogg.Verdier <> "ALLE" THEN 1 ELSE 0.
  END.
  FIND FIRST TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND 
                              TT_ELogg.EksterntSystem = "POS" AND
                              TT_ELogg.Verdier = "ALLE" NO-ERROR.
  IF AVAIL TT_ELogg THEN DO:
      DELETE TT_ELogg.
      RUN SkapaTTELoggAlle(INPUT-OUTPUT iAntNyEndre,cTabellNavn).
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KopierEloggLevSort C-Win 
PROCEDURE KopierEloggLevSort :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER cTabellNavn AS CHARACTER  NO-UNDO.
  DEFINE OUTPUT PARAMETER iAntSlett   AS INTEGER    NO-UNDO.
  DEFINE OUTPUT PARAMETER iAntNyEndre AS INTEGER    NO-UNDO.
  FOR EACH ELogg WHERE ELogg.TabellNavn = cTabellNavn AND ELogg.EksterntSystem = "POS".
      IF Elogg.EndringsType = 3 THEN DO:
          IF NUM-ENTRIES(ELogg.Verdier) = 2 THEN DO:
              IF NOT CAN-FIND(FIRST LevSant WHERE LevSant.LevNr  = INT(ENTRY(1,ELogg.Verdier)) AND
                                                  LevSant.SortID = ENTRY(2,ELogg.Verdier)) THEN DO:
                  IF NOT CAN-FIND(FIRST TT_Elogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND 
                                            TT_ELogg.EksterntSystem        = "POS"       AND
                                            TT_ELogg.Verdier               = ELogg.Verdier) THEN DO:
                      CREATE TT_ELogg.
                      BUFFER-COPY ELogg TO TT_Elogg.
                      RELEASE TT_Elogg.
                      ASSIGN iAntSlett   = iAntSlett + 1.
                  END.
              END.
              ELSE DO:
                  CREATE TT_ELogg.
                  BUFFER-COPY ELogg EXCEPT EndringsType TO TT_Elogg.
                  ASSIGN TT_Elogg.EndringsType = 1.
                  RELEASE TT_Elogg.
                  ASSIGN iAntNyEndre = iAntNyEndre + 1.
              END.
          END.
      END.
      ELSE DO:
          IF NOT CAN-FIND(FIRST TT_Elogg WHERE TT_Elogg.Verdier = ELogg.Verdier) THEN DO:
              CREATE TT_ELogg.
              BUFFER-COPY ELogg TO TT_Elogg.
              RELEASE TT_Elogg.
              ASSIGN iAntNyEndre = iAntNyEndre + IF ELogg.Verdier <> "ALLE" THEN 1 ELSE 0.
          END.
      END.
      ASSIGN Elogg.Behandlet = TRUE.
  END.
  FIND FIRST TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND 
                              TT_ELogg.EksterntSystem = "POS" AND
                              TT_ELogg.Verdier = "ALLE" NO-ERROR.
  IF AVAIL TT_ELogg THEN DO:
      DELETE TT_ELogg.
      RUN SkapaTTELoggAlle(INPUT-OUTPUT iAntNyEndre,cTabellNavn).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KopierEloggStrType C-Win 
PROCEDURE KopierEloggStrType :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER cTabellNavn AS CHARACTER  NO-UNDO.
  DEFINE OUTPUT PARAMETER iAntSlett   AS INTEGER    NO-UNDO.
  DEFINE OUTPUT PARAMETER iAntNyEndre AS INTEGER    NO-UNDO.
  
  FOR EACH ELogg WHERE ELogg.TabellNavn = cTabellNavn AND ELogg.EksterntSystem = "POS".
      IF Elogg.EndringsType = 3 THEN DO:
          IF NUM-ENTRIES(ELogg.Verdier) = 2 THEN DO:
              IF NOT CAN-FIND(FIRST StrType WHERE StrType.StrTypeID = INT(ENTRY(1,ELogg.Verdier))) THEN DO:
                  IF NOT CAN-FIND(FIRST TT_Elogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND 
                                            TT_ELogg.EksterntSystem = "POS" AND
                                            TT_ELogg.Verdier = ENTRY(1,ELogg.Verdier)) THEN DO:
                      CREATE TT_ELogg.
                      BUFFER-COPY ELogg EXCEPT Verdier TO TT_Elogg.
                      ASSIGN TT_Elogg.Verdier = ENTRY(1,ELogg.Verdier).
                      RELEASE TT_Elogg.
                      ASSIGN iAntSlett   = iAntSlett + 1.
                  END.
              END.
              ELSE DO:
                  IF NOT CAN-FIND(FIRST TT_Elogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND 
                                            TT_ELogg.EksterntSystem = "POS" AND
                                            TT_ELogg.Verdier = ENTRY(1,ELogg.Verdier)) THEN DO:
                      CREATE TT_ELogg.
                      BUFFER-COPY ELogg EXCEPT Verdier EndringsType TO TT_Elogg.
                      ASSIGN TT_Elogg.Verdier = ENTRY(1,ELogg.Verdier)
                             TT_Elogg.EndringsType = 1.
                      RELEASE TT_Elogg.
                      ASSIGN iAntNyEndre = iAntNyEndre + 1.
                  END.
              END.
          END.
      END.
      ELSE DO:
          IF NOT CAN-FIND(FIRST TT_Elogg WHERE TT_Elogg.Verdier = ENTRY(1,ELogg.Verdier)) THEN DO:
              CREATE TT_ELogg.
              BUFFER-COPY ELogg EXCEPT Verdier TO TT_Elogg.
              ASSIGN TT_Elogg.Verdier = ENTRY(1,ELogg.Verdier).
              RELEASE TT_Elogg.
              ASSIGN iAntNyEndre = iAntNyEndre + IF ELogg.Verdier <> "ALLE" THEN 1 ELSE 0.
          END.
      END.
      ASSIGN Elogg.Behandlet = TRUE.
  END.
  FIND FIRST TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND 
                              TT_ELogg.EksterntSystem = "POS" AND
                              TT_ELogg.Verdier = "ALLE" NO-ERROR.
  IF AVAIL TT_ELogg THEN DO:
      DELETE TT_ELogg.
      RUN SkapaTTELoggAlle(INPUT-OUTPUT iAntNyEndre,cTabellNavn).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NySlettAlle C-Win 
PROCEDURE NySlettAlle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER cTabell AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER cAction AS CHARACTER  NO-UNDO.
    IF cAction = "NY" AND NOT CAN-FIND(FIRST ELogg WHERE 
                                       ELogg.TabellNavn     = cTabell AND
                                       ELogg.EksterntSystem = "POS"    AND
                                       ELogg.Verdier        = "ALLE") THEN DO:
        CREATE Elogg.
        ASSIGN ELogg.TabellNavn     = cTabell
               ELogg.EksterntSystem = "POS"   
               ELogg.Verdier        = "ALLE"
               ELogg.EndringsType   = 1
               ELogg.Behandlet      = FALSE.
    END.
    ELSE IF cAction = "SLETT" THEN DO:
        FIND FIRST ELogg WHERE ELogg.TabellNavn     = cTabell AND
                               ELogg.EksterntSystem = "POS"    AND
                               ELogg.Verdier        = "ALLE" NO-ERROR.
        IF AVAIL ELogg THEN
            DELETE ELogg.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NyTTElogg C-Win 
PROCEDURE NyTTElogg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER cTabell AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER cVerdi  AS CHARACTER  NO-UNDO.
    CREATE TT_Elogg.
    ASSIGN TT_ELogg.TabellNavn     = cTabell
           TT_ELogg.EksterntSystem = "POS"   
           TT_ELogg.Verdier        = cVerdi
           TT_ELogg.EndringsType   = 1
           TT_ELogg.Behandlet      = FALSE.
    RELEASE TT_ELogg.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendAnv-Kod C-Win 
PROCEDURE SendAnv-Kod :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    &SCOPED-DEFINE Tabell  Anv-Kod
    &SCOPED-DEFINE KeyFelt Anv-Id
    DEFINE VARIABLE iAntSlett   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iAntNyEndre AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cTabellNavn AS CHARACTER  INIT "{&Tabell}" NO-UNDO.
    EMPTY TEMP-TABLE TT_{&Tabell}.
    RUN KopierElogg (cTabellNavn,OUTPUT iAntSlett,OUTPUT iAntNyEndre).
    IF iAntSlett > 0 THEN DO:
        EXPORT "H" cTabellNavn 3 "1.0" iAntSlett.
        CREATE TT_{&Tabell}.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "POS"
                            AND TT_Elogg.EndringsType = 3:
            ASSIGN TT_{&Tabell}.{&KeyFelt} = INT(TT_Elogg.Verdier).
            EXPORT TT_{&Tabell}.Anv-Id        
                                 TT_{&Tabell}.AnvBeskr      
                                 TT_{&Tabell}.EDato         
                                 TT_{&Tabell}.ETid          
                                 TT_{&Tabell}.BrukerID      
                                 TT_{&Tabell}.RegistrertDato
                                 TT_{&Tabell}.RegistrertTid
                                 TT_{&Tabell}.RegistrertAv.
        END.
    END.
    IF iAntNyEndre > 0 THEN DO:
        EXPORT "H" cTabellNavn 1 "1.0" iAntNyEndre.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "POS"
                            AND TT_Elogg.EndringsType = 1:
            FIND {&Tabell} WHERE {&Tabell}.{&KeyFelt} = INT(TT_Elogg.Verdier) NO-LOCK NO-ERROR.
            IF AVAIL {&Tabell} THEN
                EXPORT {&Tabell}.Anv-Id        
                       {&Tabell}.AnvBeskr      
                       {&Tabell}.EDato         
                       {&Tabell}.ETid          
                       {&Tabell}.BrukerID      
                       {&Tabell}.RegistrertDato
                       {&Tabell}.RegistrertTid 
                       {&Tabell}.RegistrertAv. 
        END.
    END.
    &UNDEFINE Tabell
    &UNDEFINE KeyFelt
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendAvdeling C-Win 
PROCEDURE SendAvdeling :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    &SCOPED-DEFINE Tabell  Avdeling
    &SCOPED-DEFINE KeyFelt AvdelingNr
    DEFINE VARIABLE iAntSlett   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iAntNyEndre AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cTabellNavn AS CHARACTER  INIT "{&Tabell}" NO-UNDO.
    EMPTY TEMP-TABLE TT_{&Tabell}.
    RUN KopierElogg (cTabellNavn,OUTPUT iAntSlett,OUTPUT iAntNyEndre).
    IF iAntSlett > 0 THEN DO:
        EXPORT "H" cTabellNavn 3 "1.0" iAntSlett.
        CREATE TT_{&Tabell}.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "POS"
                            AND TT_Elogg.EndringsType = 3:
            ASSIGN TT_{&Tabell}.{&KeyFelt} = INT(TT_Elogg.Verdier).
            EXPORT TT_{&Tabell}.AvdelingNr
                                 TT_{&Tabell}.AvdelingNavn
                                 TT_{&Tabell}.EDato
                                 TT_{&Tabell}.ETid
                                 TT_{&Tabell}.BrukerID
                                 TT_{&Tabell}.RegistrertDato
                                 TT_{&Tabell}.RegistrertTid
                                 TT_{&Tabell}.RegistrertAv.
        END.
    END.
    IF iAntNyEndre > 0 THEN DO:
        EXPORT "H" cTabellNavn 1 "1.0" iAntNyEndre.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "POS"
                            AND TT_Elogg.EndringsType = 1:
            FIND {&Tabell} WHERE {&Tabell}.{&KeyFelt} = INT(TT_Elogg.Verdier) NO-LOCK NO-ERROR.
            IF AVAIL {&Tabell} THEN
                EXPORT {&Tabell}.AvdelingNr
                                     {&Tabell}.AvdelingNavn
                                     {&Tabell}.EDato
                                     {&Tabell}.ETid
                                     {&Tabell}.BrukerID
                                     {&Tabell}.RegistrertDato
                                     {&Tabell}.RegistrertTid
                                     {&Tabell}.RegistrertAv.
        END.
    END.
    &UNDEFINE Tabell
    &UNDEFINE KeyFelt
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendBehandlingskode C-Win 
PROCEDURE SendBehandlingskode :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*     &SCOPED-DEFINE Tabell  Behandlingskode                                                            */
/*     &SCOPED-DEFINE KeyFelt BehKode                                                                    */
/*     DEFINE VARIABLE iAntSlett   AS INTEGER    NO-UNDO.                                                */
/*     DEFINE VARIABLE iAntNyEndre AS INTEGER    NO-UNDO.                                                */
/*     DEFINE VARIABLE cTabellNavn AS CHARACTER  INIT "{&Tabell}" NO-UNDO.                               */
/*     EMPTY TEMP-TABLE TT_{&Tabell}.                                                                    */
/*     RUN KopierElogg (cTabellNavn,OUTPUT iAntSlett,OUTPUT iAntNyEndre).                                */
/*     IF iAntSlett > 0 THEN DO:                                                                         */
/*         EXPORT "H" cTabellNavn 3 "1.0" iAntSlett.                                                     */
/*         CREATE TT_{&Tabell}.                                                                          */
/*         FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "POS" */
/*                             AND TT_Elogg.EndringsType = 3:                                            */
/*             ASSIGN TT_{&Tabell}.{&KeyFelt} = INT(TT_Elogg.Verdier).                                   */
/*             EXPORT TT_{&Tabell}.BehKode                                                               */
/*                                  TT_{&Tabell}.EDato                                                   */
/*                                  TT_{&Tabell}.ETid                                                    */
/*                                  TT_{&Tabell}.BrukerID                                                */
/*                                  TT_{&Tabell}.RegistrertDato                                          */
/*                                  TT_{&Tabell}.RegistrertTid                                           */
/*                                  TT_{&Tabell}.RegistrertAv                                            */
/*                                  TT_{&Tabell}.Beskrivelse.                                            */
/*         END.                                                                                          */
/*     END.                                                                                              */
/*     IF iAntNyEndre > 0 THEN DO:                                                                       */
/*         EXPORT "H" cTabellNavn 1 "1.0" iAntNyEndre.                                                   */
/*         FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "POS" */
/*                             AND TT_Elogg.EndringsType = 1:                                            */
/*             FIND {&Tabell} WHERE {&Tabell}.{&KeyFelt} = INT(TT_Elogg.Verdier) NO-LOCK NO-ERROR.       */
/*             IF AVAIL {&Tabell} THEN                                                                   */
/*                 EXPORT {&Tabell}.BehKode                                                              */
/*                                      {&Tabell}.EDato                                                  */
/*                                      {&Tabell}.ETid                                                   */
/*                                      {&Tabell}.BrukerID                                               */
/*                                      {&Tabell}.RegistrertDato                                         */
/*                                      {&Tabell}.RegistrertTid                                          */
/*                                      {&Tabell}.RegistrertAv                                           */
/*                                      {&Tabell}.Beskrivelse.                                           */
/*         END.                                                                                          */
/*     END.                                                                                              */
/*     &UNDEFINE Tabell                                                                                  */
/*     &UNDEFINE KeyFelt                                                                                 */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendDefaultLevDato C-Win 
PROCEDURE SendDefaultLevDato :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    &SCOPED-DEFINE Tabell  DefaultLevDato
    &SCOPED-DEFINE KeyFelt ButikkNr
    DEFINE VARIABLE iAntSlett   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iAntNyEndre AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cTabellNavn AS CHARACTER  INIT "{&Tabell}" NO-UNDO.

    EMPTY TEMP-TABLE TT_{&Tabell}.

    RUN KopierElogg (cTabellNavn,OUTPUT iAntSlett,OUTPUT iAntNyEndre).

    ASSIGN
        iAntSlett   = 0
        iAntNyEndre = 0
        .
    FOR EACH TT_ELogg WHERE 
        TT_ELogg.TabellNavn     = cTabellNavn AND 
        TT_ELogg.EksterntSystem = "POS":

        IF TT_Elogg.EndringsType = 3 THEN
            iAntSlett = iAntSlett + 1.
        IF TT_Elogg.EndringsType = 1 THEN
            iAntNyEndre = iAntNyEndre + 1.
    END.


    IF iAntSlett > 0 THEN 
    DO:
        EXPORT "H" cTabellNavn 3 "1.0" iAntSlett.
        CREATE TT_{&Tabell}.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "POS"
                            AND TT_Elogg.EndringsType = 3:
            ASSIGN TT_{&Tabell}.{&KeyFelt} = INT(ENTRY(1,TT_Elogg.Verdier))
                   TT_{&Tabell}.LevNr = INT(ENTRY(2,TT_Elogg.Verdier))
                   .
            EXPORT TT_{&Tabell}.{&KeyFelt}
                   TT_{&Tabell}.levnr         
                   TT_{&Tabell}.UkeDag       
                   TT_{&Tabell}.EDato      
                   TT_{&Tabell}.ETid       
                   TT_{&Tabell}.BrukerId       
                   TT_{&Tabell}.RegistrertDato    
                   TT_{&Tabell}.RegistrertTid
                   TT_{&Tabell}.RegistrertAv
                   TT_{&Tabell}.Tid
                   TT_{&Tabell}.HasterTid      
                   .
        END.
    END.
    IF iAntNyEndre > 0 THEN DO:
        EXPORT "H" cTabellNavn 1 "1.0" iAntNyEndre.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "POS"
                            AND TT_Elogg.EndringsType = 1:
            FIND {&Tabell} WHERE 
                {&Tabell}.{&KeyFelt} = INT(ENTRY(1,TT_Elogg.Verdier)) AND
                {&Tabell}.LevNr = INT(ENTRY(2,TT_Elogg.Verdier))
                NO-LOCK NO-ERROR.
            IF AVAIL {&Tabell} THEN DO:
                EXPORT {&Tabell}.ButikkNr         
                       {&Tabell}.LevNr       
                       {&Tabell}.UkeDag       
                       {&Tabell}.EDato      
                       {&Tabell}.ETid       
                       {&Tabell}.BrukerId       
                       {&Tabell}.RegistrertDato    
                       {&Tabell}.RegistrertTid
                       {&Tabell}.RegistrertAv
                       {&Tabell}.Tid
                       {&Tabell}.HasterTid      
                       .
            END.
        END.
    END.
    EMPTY TEMP-TABLE TT_{&Tabell}.
    &UNDEFINE Tabell
    &UNDEFINE KeyFelt
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendFarg C-Win 
PROCEDURE SendFarg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    &SCOPED-DEFINE Tabell  Farg
    &SCOPED-DEFINE KeyFelt Farg
    DEFINE VARIABLE iAntSlett   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iAntNyEndre AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cTabellNavn AS CHARACTER  INIT "{&Tabell}" NO-UNDO.

    EMPTY TEMP-TABLE TT_{&Tabell}.

    RUN KopierElogg (cTabellNavn,OUTPUT iAntSlett,OUTPUT iAntNyEndre).

    IF iAntSlett > 0 THEN DO:
        EXPORT "H" cTabellNavn 3 "1.0" iAntSlett.
        CREATE TT_{&Tabell}.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "POS"
                            AND TT_Elogg.EndringsType = 3:
            ASSIGN TT_{&Tabell}.{&KeyFelt} = INT(TT_Elogg.Verdier).
            EXPORT TT_{&Tabell}.Farg          
                                 TT_{&Tabell}.FarBeskr      
                                 TT_{&Tabell}.KFarge        
                                 TT_{&Tabell}.EDato         
                                 TT_{&Tabell}.ETid          
                                 TT_{&Tabell}.BrukerID      
                                 TT_{&Tabell}.RegistrertDato
                                 TT_{&Tabell}.RegistrertTid 
                                 TT_{&Tabell}.RegistrertAv.
        END.
    END.
    IF iAntNyEndre > 0 THEN DO:
        EXPORT "H" cTabellNavn 1 "1.0" iAntNyEndre.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "POS"
                            AND TT_Elogg.EndringsType = 1:
            FIND {&Tabell} WHERE {&Tabell}.{&KeyFelt} = INT(TT_Elogg.Verdier) NO-LOCK NO-ERROR.
            IF AVAIL {&Tabell} THEN
                EXPORT {&Tabell}.Farg          
                       {&Tabell}.FarBeskr      
                       {&Tabell}.KFarge        
                       {&Tabell}.EDato         
                       {&Tabell}.ETid          
                       {&Tabell}.BrukerID      
                       {&Tabell}.RegistrertDato
                       {&Tabell}.RegistrertTid 
                       {&Tabell}.RegistrertAv.
        END.
    END.
    &UNDEFINE Tabell
    &UNDEFINE KeyFelt
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendFeilkode C-Win 
PROCEDURE SendFeilkode :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    &SCOPED-DEFINE Tabell  Feilkode
    &SCOPED-DEFINE KeyFelt FeilKode
    DEFINE VARIABLE iAntSlett   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iAntNyEndre AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cTabellNavn AS CHARACTER  INIT "{&Tabell}" NO-UNDO.
    EMPTY TEMP-TABLE TT_{&Tabell}.
    RUN KopierElogg (cTabellNavn,OUTPUT iAntSlett,OUTPUT iAntNyEndre).
    IF iAntSlett > 0 THEN DO:
        EXPORT "H" cTabellNavn 3 "1.0" iAntSlett.
        CREATE TT_{&Tabell}.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "POS"
                            AND TT_Elogg.EndringsType = 3:
            ASSIGN TT_{&Tabell}.{&KeyFelt} = INT(TT_Elogg.Verdier).
            EXPORT TT_{&Tabell}.FeilKode      
                                 TT_{&Tabell}.Beskrivelse   
                                 TT_{&Tabell}.Notat         
                                 TT_{&Tabell}.Belastes      
                                 TT_{&Tabell}.EDato         
                                 TT_{&Tabell}.ETid          
                                 TT_{&Tabell}.BrukerID      
                                 TT_{&Tabell}.RegistrertDato
                                 TT_{&Tabell}.RegistrertTid 
                                 TT_{&Tabell}.RegistrertAv.
        END.
    END.
    IF iAntNyEndre > 0 THEN DO:
        EXPORT "H" cTabellNavn 1 "1.0" iAntNyEndre.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "POS"
                            AND TT_Elogg.EndringsType = 1:
            FIND {&Tabell} WHERE {&Tabell}.{&KeyFelt} = INT(TT_Elogg.Verdier) NO-LOCK NO-ERROR.
            IF AVAIL {&Tabell} THEN
                EXPORT {&Tabell}.FeilKode      
                                     {&Tabell}.Beskrivelse   
                                     {&Tabell}.Notat         
                                     {&Tabell}.Belastes      
                                     {&Tabell}.EDato         
                                     {&Tabell}.ETid          
                                     {&Tabell}.BrukerID      
                                     {&Tabell}.RegistrertDato
                                     {&Tabell}.RegistrertTid 
                                     {&Tabell}.RegistrertAv.
        END.
    END.
    &UNDEFINE Tabell
    &UNDEFINE KeyFelt
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendFoder C-Win 
PROCEDURE SendFoder :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    &SCOPED-DEFINE Tabell  Foder
    &SCOPED-DEFINE KeyFelt foder-id
    DEFINE VARIABLE iAntSlett   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iAntNyEndre AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cTabellNavn AS CHARACTER  INIT "{&Tabell}" NO-UNDO.
    EMPTY TEMP-TABLE TT_{&Tabell}.
    RUN KopierElogg (cTabellNavn,OUTPUT iAntSlett,OUTPUT iAntNyEndre).
    IF iAntSlett > 0 THEN DO:
        EXPORT "H" cTabellNavn 3 "1.0" iAntSlett.
        CREATE TT_{&Tabell}.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "POS"
                            AND TT_Elogg.EndringsType = 3:
            ASSIGN TT_{&Tabell}.{&KeyFelt} = INT(TT_Elogg.Verdier).
            EXPORT TT_{&Tabell}.foder-id      
                                 TT_{&Tabell}.beskrivning   
                                 TT_{&Tabell}.EDato         
                                 TT_{&Tabell}.ETid          
                                 TT_{&Tabell}.BrukerID      
                                 TT_{&Tabell}.RegistrertDato
                                 TT_{&Tabell}.RegistrertTid 
                                 TT_{&Tabell}.RegistrertAv.
        END.
    END.
    IF iAntNyEndre > 0 THEN DO:
        EXPORT "H" cTabellNavn 1 "1.0" iAntNyEndre.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "POS"
                            AND TT_Elogg.EndringsType = 1:
            FIND {&Tabell} WHERE {&Tabell}.{&KeyFelt} = INT(TT_Elogg.Verdier) NO-LOCK NO-ERROR.
            IF AVAIL {&Tabell} THEN
                EXPORT {&Tabell}.foder-id      
                                     {&Tabell}.beskrivning   
                                     {&Tabell}.EDato         
                                     {&Tabell}.ETid          
                                     {&Tabell}.BrukerID      
                                     {&Tabell}.RegistrertDato
                                     {&Tabell}.RegistrertTid 
                                     {&Tabell}.RegistrertAv.
        END.
    END.
    &UNDEFINE Tabell
    &UNDEFINE KeyFelt
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendHandtering C-Win 
PROCEDURE SendHandtering :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    &SCOPED-DEFINE Tabell  Handtering
    &SCOPED-DEFINE KeyFelt HandKode
    DEFINE VARIABLE iAntSlett   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iAntNyEndre AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cTabellNavn AS CHARACTER  INIT "{&Tabell}" NO-UNDO.
    EMPTY TEMP-TABLE TT_{&Tabell}.
    RUN KopierElogg (cTabellNavn,OUTPUT iAntSlett,OUTPUT iAntNyEndre).
    IF iAntSlett > 0 THEN DO:
        EXPORT "H" cTabellNavn 3 "1.0" iAntSlett.
        CREATE TT_{&Tabell}.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "POS"
                            AND TT_Elogg.EndringsType = 3:
            ASSIGN TT_{&Tabell}.{&KeyFelt} = INT(TT_Elogg.Verdier).
            EXPORT TT_{&Tabell}.HandKode      
                                 TT_{&Tabell}.Beskrivelse   
                                 TT_{&Tabell}.Notat         
                                 TT_{&Tabell}.EDato         
                                 TT_{&Tabell}.ETid          
                                 TT_{&Tabell}.BrukerID      
                                 TT_{&Tabell}.RegistrertDato
                                 TT_{&Tabell}.RegistrertTid 
                                 TT_{&Tabell}.RegistrertAv.
        END.
    END.
    IF iAntNyEndre > 0 THEN DO:
        EXPORT "H" cTabellNavn 1 "1.0" iAntNyEndre.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "POS"
                            AND TT_Elogg.EndringsType = 1:
            FIND {&Tabell} WHERE {&Tabell}.{&KeyFelt} = INT(TT_Elogg.Verdier) NO-LOCK NO-ERROR.
            IF AVAIL {&Tabell} THEN
                EXPORT {&Tabell}.HandKode      
                                     {&Tabell}.Beskrivelse   
                                     {&Tabell}.Notat         
                                     {&Tabell}.EDato         
                                     {&Tabell}.ETid          
                                     {&Tabell}.BrukerID      
                                     {&Tabell}.RegistrertDato
                                     {&Tabell}.RegistrertTid 
                                     {&Tabell}.RegistrertAv.
        END.
    END.
    &UNDEFINE Tabell
    &UNDEFINE KeyFelt
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendHuvGr C-Win 
PROCEDURE SendHuvGr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    &SCOPED-DEFINE Tabell  HuvGr
    &SCOPED-DEFINE KeyFelt Hg
    DEFINE VARIABLE iAntSlett   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iAntNyEndre AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cTabellNavn AS CHARACTER  INIT "{&Tabell}" NO-UNDO.
    EMPTY TEMP-TABLE TT_{&Tabell}.
    RUN KopierElogg (cTabellNavn,OUTPUT iAntSlett,OUTPUT iAntNyEndre).
    IF iAntSlett > 0 THEN DO:
        EXPORT "H" cTabellNavn 3 "1.0" iAntSlett.
        CREATE TT_{&Tabell}.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "POS"
                            AND TT_Elogg.EndringsType = 3:
            ASSIGN TT_{&Tabell}.{&KeyFelt} = INT(TT_Elogg.Verdier).
            EXPORT TT_{&Tabell}.Hg            
                                 TT_{&Tabell}.HgBeskr       
                                 TT_{&Tabell}.AvdelingNr    
                                 TT_{&Tabell}.EDato         
                                 TT_{&Tabell}.ETid          
                                 TT_{&Tabell}.BrukerID      
                                 TT_{&Tabell}.RegistrertDato
                                 TT_{&Tabell}.RegistrertTid 
                                 TT_{&Tabell}.RegistrertAv.
        END.
    END.
    IF iAntNyEndre > 0 THEN DO:
        EXPORT "H" cTabellNavn 1 "1.0" iAntNyEndre.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "POS"
                            AND TT_Elogg.EndringsType = 1:
            FIND {&Tabell} WHERE {&Tabell}.{&KeyFelt} = INT(TT_Elogg.Verdier) NO-LOCK NO-ERROR.
            IF AVAIL {&Tabell} THEN
                EXPORT {&Tabell}.Hg            
                                     {&Tabell}.HgBeskr       
                                     {&Tabell}.AvdelingNr    
                                     {&Tabell}.EDato         
                                     {&Tabell}.ETid          
                                     {&Tabell}.BrukerID      
                                     {&Tabell}.RegistrertDato
                                     {&Tabell}.RegistrertTid 
                                     {&Tabell}.RegistrertAv.
        END.
    END.
    &UNDEFINE Tabell
    &UNDEFINE KeyFelt
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendInnersula C-Win 
PROCEDURE SendInnersula :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    &SCOPED-DEFINE Tabell  Innersula
    &SCOPED-DEFINE KeyFelt Inner-Id
    DEFINE VARIABLE iAntSlett   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iAntNyEndre AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cTabellNavn AS CHARACTER  INIT "{&Tabell}" NO-UNDO.
    EMPTY TEMP-TABLE TT_{&Tabell}.
    RUN KopierElogg (cTabellNavn,OUTPUT iAntSlett,OUTPUT iAntNyEndre).
    IF iAntSlett > 0 THEN DO:
        EXPORT "H" cTabellNavn 3 "1.0" iAntSlett.
        CREATE TT_{&Tabell}.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "POS"
                            AND TT_Elogg.EndringsType = 3:
            ASSIGN TT_{&Tabell}.{&KeyFelt} = INT(TT_Elogg.Verdier).
            EXPORT TT_{&Tabell}.Inner-Id      
                                 TT_{&Tabell}.InnerBeskr    
                                 TT_{&Tabell}.EDato         
                                 TT_{&Tabell}.ETid          
                                 TT_{&Tabell}.BrukerID      
                                 TT_{&Tabell}.RegistrertDato
                                 TT_{&Tabell}.RegistrertTid 
                                 TT_{&Tabell}.RegistrertAv.  
        END.
    END.
    IF iAntNyEndre > 0 THEN DO:
        EXPORT "H" cTabellNavn 1 "1.0" iAntNyEndre.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "POS"
                            AND TT_Elogg.EndringsType = 1:
            FIND {&Tabell} WHERE {&Tabell}.{&KeyFelt} = INT(TT_Elogg.Verdier) NO-LOCK NO-ERROR.
            IF AVAIL {&Tabell} THEN
                EXPORT {&Tabell}.Inner-Id      
                                     {&Tabell}.InnerBeskr    
                                     {&Tabell}.EDato         
                                     {&Tabell}.ETid          
                                     {&Tabell}.BrukerID      
                                     {&Tabell}.RegistrertDato
                                     {&Tabell}.RegistrertTid 
                                     {&Tabell}.RegistrertAv.
        END.
    END.
    &UNDEFINE Tabell
    &UNDEFINE KeyFelt
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendKasValuta C-Win 
PROCEDURE SendKasValuta :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    &SCOPED-DEFINE Tabell  KasValuta
    &SCOPED-DEFINE KeyFelt ValKod
    DEFINE VARIABLE iAntSlett   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iAntNyEndre AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cTabellNavn AS CHARACTER  INIT "{&Tabell}" NO-UNDO.
    EMPTY TEMP-TABLE TT_{&Tabell}.

    ON WRITE OF KasValuta OVERRIDE DO: END.
    RUN KopierElogg (cTabellNavn,OUTPUT iAntSlett,OUTPUT iAntNyEndre).
/*     Vi hanterar bara ändring av "ALLE" */
/*     IF iAntSlett > 0 THEN DO:                                                                         */
/*         EXPORT "H" cTabellNavn 3 "1.0" iAntSlett.                                                     */
/*         CREATE TT_{&Tabell}.                                                                          */
/*         FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "POS" */
/*                             AND TT_Elogg.EndringsType = 3:                                            */
/*             ASSIGN TT_{&Tabell}.{&KeyFelt} = TT_Elogg.Verdier.                                        */
/*             EXPORT TT_{&Tabell}.KatNr                                                                 */
/*                    TT_{&Tabell}.Beskrivelse                                                           */
/*                    TT_{&Tabell}.Merknad                                                               */
/*                    TT_{&Tabell}.EDato                                                                 */
/*                    TT_{&Tabell}.ETid                                                                  */
/*                    TT_{&Tabell}.BrukerID                                                              */
/*                    TT_{&Tabell}.RegistrertDato                                                        */
/*                    TT_{&Tabell}.RegistrertTid                                                         */
/*                    TT_{&Tabell}.RegistrertAv.                                                         */
/*         END.                                                                                          */
/*     END.                                                                                              */
    IF iAntNyEndre > 0 THEN 
    EKSPORT:
    DO:
        /* Teller opp riktig antall poster. */
        iAntNyEndre = 0.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "POS"
                            AND TT_Elogg.EndringsType = 1:
            FIND {&Tabell} WHERE {&Tabell}.{&KeyFelt} = TT_Elogg.Verdier NO-ERROR.
            IF NOT AVAIL {&Tabell} THEN
                NEXT.
            ELSE iAntNyEndre = iAntNyEndre + 1.
        END.
        IF iAntNyEndre = 0 THEN
            LEAVE EKSPORT.

        EXPORT "H" cTabellNavn 1 "1.0" iAntNyEndre.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "POS"
                            AND TT_Elogg.EndringsType = 1:
            FIND {&Tabell} WHERE {&Tabell}.{&KeyFelt} = TT_Elogg.Verdier NO-ERROR.
            IF NOT AVAIL {&Tabell} THEN
                NEXT.
            ASSIGN {&Tabell}.ValDatum = TODAY.
            IF AVAIL {&Tabell} THEN
                EXPORT {&Tabell}.ValKod        
                       {&Tabell}.ValKurs       
                       {&Tabell}.ValLand       
                       {&Tabell}.ValDatum      
                       {&Tabell}.EDato         
                       {&Tabell}.ETid          
                       {&Tabell}.BrukerID      
                       {&Tabell}.RegistrertDato
                       {&Tabell}.RegistrertTid 
                       {&Tabell}.RegistrertAv  
                       {&Tabell}.ValNr         
                       {&Tabell}.ValNavn       
                       {&Tabell}.indeks        
                       {&Tabell}.retur         
                       {&Tabell}.KasseValKurs  
                       {&Tabell}.ValAktiv.
                       {&Tabell}.EgenValuta.
        END.
    END. /* EKSPORT */
    &UNDEFINE Tabell
    &UNDEFINE KeyFelt
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendKategori C-Win 
PROCEDURE SendKategori :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    &SCOPED-DEFINE Tabell  Kategori
    &SCOPED-DEFINE KeyFelt KatNr
    DEFINE VARIABLE iAntSlett   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iAntNyEndre AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cTabellNavn AS CHARACTER  INIT "{&Tabell}" NO-UNDO.
    EMPTY TEMP-TABLE TT_{&Tabell}.
    RUN KopierElogg (cTabellNavn,OUTPUT iAntSlett,OUTPUT iAntNyEndre).
    IF iAntSlett > 0 THEN DO:
        EXPORT "H" cTabellNavn 3 "1.0" iAntSlett.
        CREATE TT_{&Tabell}.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "POS"
                            AND TT_Elogg.EndringsType = 3:
            ASSIGN TT_{&Tabell}.{&KeyFelt} = INT(TT_Elogg.Verdier).
            EXPORT TT_{&Tabell}.KatNr         
                                 TT_{&Tabell}.Beskrivelse   
                                 TT_{&Tabell}.Merknad       
                                 TT_{&Tabell}.EDato         
                                 TT_{&Tabell}.ETid          
                                 TT_{&Tabell}.BrukerID      
                                 TT_{&Tabell}.RegistrertDato
                                 TT_{&Tabell}.RegistrertTid 
                                 TT_{&Tabell}.RegistrertAv.
        END.
    END.
    IF iAntNyEndre > 0 THEN DO:
        EXPORT "H" cTabellNavn 1 "1.0" iAntNyEndre.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "POS"
                            AND TT_Elogg.EndringsType = 1:
            FIND {&Tabell} WHERE {&Tabell}.{&KeyFelt} = INT(TT_Elogg.Verdier) NO-LOCK NO-ERROR.
            IF AVAIL {&Tabell} THEN
                EXPORT {&Tabell}.KatNr         
                                     {&Tabell}.Beskrivelse   
                                     {&Tabell}.Merknad       
                                     {&Tabell}.EDato         
                                     {&Tabell}.ETid          
                                     {&Tabell}.BrukerID      
                                     {&Tabell}.RegistrertDato
                                     {&Tabell}.RegistrertTid 
                                     {&Tabell}.RegistrertAv.
        END.
    END.
    &UNDEFINE Tabell
    &UNDEFINE KeyFelt
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendKjede C-Win 
PROCEDURE SendKjede :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    &SCOPED-DEFINE Tabell  Kjede
    &SCOPED-DEFINE KeyFelt KjedeNr
    DEFINE VARIABLE iAntSlett   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iAntNyEndre AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cTabellNavn AS CHARACTER  INIT "{&Tabell}" NO-UNDO.
    EMPTY TEMP-TABLE TT_{&Tabell}.
    RUN KopierElogg (cTabellNavn,OUTPUT iAntSlett,OUTPUT iAntNyEndre).
    IF iAntSlett > 0 THEN DO:
        EXPORT "H" cTabellNavn 3 "1.0" iAntSlett.
        CREATE TT_{&Tabell}.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "POS"
                            AND TT_Elogg.EndringsType = 3:
            ASSIGN TT_{&Tabell}.{&KeyFelt} = INT(TT_Elogg.Verdier).
            EXPORT TT_{&Tabell}.KjedeNr       
                   TT_{&Tabell}.KjedeNavn     
                   TT_{&Tabell}.KjedeKNavn    
                   TT_{&Tabell}.EDato         
                   TT_{&Tabell}.ETid          
                   TT_{&Tabell}.BrukerID      
                   TT_{&Tabell}.RegistrertDato
                   TT_{&Tabell}.RegistrertTid 
                   TT_{&Tabell}.RegistrertAv.
        END.
    END.
    IF iAntNyEndre > 0 THEN DO:
        EXPORT "H" cTabellNavn 1 "1.0" iAntNyEndre.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "POS"
                            AND TT_Elogg.EndringsType = 1:
            FIND {&Tabell} WHERE {&Tabell}.{&KeyFelt} = INT(TT_Elogg.Verdier) NO-LOCK NO-ERROR.
            IF AVAIL {&Tabell} THEN
                EXPORT {&Tabell}.KjedeNr       
                       {&Tabell}.KjedeNavn     
                       {&Tabell}.KjedeKNavn    
                       {&Tabell}.EDato         
                       {&Tabell}.ETid          
                       {&Tabell}.BrukerID      
                       {&Tabell}.RegistrertDato
                       {&Tabell}.RegistrertTid 
                       {&Tabell}.RegistrertAv.
        END.
    END.
    &UNDEFINE Tabell
    &UNDEFINE KeyFelt
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendKjedeDistrikt C-Win 
PROCEDURE SendKjedeDistrikt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    &SCOPED-DEFINE Tabell  KjedeDistrikt
    &SCOPED-DEFINE KeyFelt KjedeNr
    &SCOPED-DEFINE KeyFelt2 RegionNr
    &SCOPED-DEFINE KeyFelt3 DistriktNr
    DEFINE VARIABLE iAntSlett   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iAntNyEndre AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cTabellNavn AS CHARACTER  INIT "{&Tabell}" NO-UNDO.
    EMPTY TEMP-TABLE TT_{&Tabell}.
    RUN KopierElogg (cTabellNavn,OUTPUT iAntSlett,OUTPUT iAntNyEndre).
    IF iAntSlett > 0 THEN DO:
        EXPORT "H" cTabellNavn 3 "1.0" iAntSlett.
        CREATE TT_{&Tabell}.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "POS"
                            AND TT_Elogg.EndringsType = 3:
            ASSIGN TT_{&Tabell}.{&KeyFelt}  = INT(ENTRY(1,TT_Elogg.Verdier))
                   TT_{&Tabell}.{&KeyFelt2} = INT(ENTRY(2,TT_Elogg.Verdier))
                   TT_{&Tabell}.{&KeyFelt3} = INT(ENTRY(3,TT_Elogg.Verdier)).
            EXPORT TT_{&Tabell}.KjedeNr       
                   TT_{&Tabell}.EDato         
                   TT_{&Tabell}.ETid          
                   TT_{&Tabell}.BrukerID      
                   TT_{&Tabell}.RegistrertDato
                   TT_{&Tabell}.RegistrertTid 
                   TT_{&Tabell}.RegistrertAv  
                   TT_{&Tabell}.RegionNr      
                   TT_{&Tabell}.DistriktNavn  
                   TT_{&Tabell}.DistriktKNavn 
                   TT_{&Tabell}.Kontaktperson 
                   TT_{&Tabell}.Telefon       
                   TT_{&Tabell}.Mombil        
                   TT_{&Tabell}.EMail         
                   TT_{&Tabell}.DistriktNr.
        END.
    END.
    IF iAntNyEndre > 0 THEN DO:
        EXPORT "H" cTabellNavn 1 "1.0" iAntNyEndre.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "POS"
                            AND TT_Elogg.EndringsType = 1:
            FIND {&Tabell} WHERE {&Tabell}.{&KeyFelt}  = INT(ENTRY(1,TT_Elogg.Verdier)) AND 
                                 {&Tabell}.{&KeyFelt2} = INT(ENTRY(2,TT_Elogg.Verdier)) AND
                                 {&Tabell}.{&KeyFelt3} = INT(ENTRY(3,TT_Elogg.Verdier))
                NO-LOCK NO-ERROR.
            IF AVAIL {&Tabell} THEN
                EXPORT {&Tabell}.KjedeNr       
                       {&Tabell}.EDato         
                       {&Tabell}.ETid          
                       {&Tabell}.BrukerID      
                       {&Tabell}.RegistrertDato
                       {&Tabell}.RegistrertTid 
                       {&Tabell}.RegistrertAv  
                       {&Tabell}.RegionNr      
                       {&Tabell}.DistriktNavn  
                       {&Tabell}.DistriktKNavn 
                       {&Tabell}.Kontaktperson 
                       {&Tabell}.Telefon       
                       {&Tabell}.Mombil        
                       {&Tabell}.EMail         
                       {&Tabell}.DistriktNr.
        END.
    END.
    &UNDEFINE Tabell
    &UNDEFINE KeyFelt
    &UNDEFINE KeyFelt2
    &UNDEFINE KeyFelt3
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendKjedensButikker C-Win 
PROCEDURE SendKjedensButikker :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    &SCOPED-DEFINE Tabell  KjedensButikker
    &SCOPED-DEFINE KeyFelt KjedeNr
    &SCOPED-DEFINE KeyFelt2 RegionNr
    &SCOPED-DEFINE KeyFelt3 DistriktNr
    &SCOPED-DEFINE KeyFelt4 ButikkNr
    DEFINE VARIABLE iAntSlett   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iAntNyEndre AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cTabellNavn AS CHARACTER  INIT "{&Tabell}" NO-UNDO.
    EMPTY TEMP-TABLE TT_{&Tabell}.
    RUN KopierElogg (cTabellNavn,OUTPUT iAntSlett,OUTPUT iAntNyEndre).
    IF iAntSlett > 0 THEN DO:
        EXPORT "H" cTabellNavn 3 "1.0" iAntSlett.
        CREATE TT_{&Tabell}.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "POS"
                            AND TT_Elogg.EndringsType = 3:
            ASSIGN TT_{&Tabell}.{&KeyFelt}  = INT(ENTRY(1,TT_Elogg.Verdier))
                   TT_{&Tabell}.{&KeyFelt2} = INT(ENTRY(2,TT_Elogg.Verdier))
                   TT_{&Tabell}.{&KeyFelt3} = INT(ENTRY(3,TT_Elogg.Verdier))
                   TT_{&Tabell}.{&KeyFelt4} = INT(ENTRY(4,TT_Elogg.Verdier)).
            EXPORT TT_{&Tabell}.ButikkNr      
                   TT_{&Tabell}.ButikkNavn    
                   TT_{&Tabell}.Kontaktperson 
                   TT_{&Tabell}.E-Mail        
                   TT_{&Tabell}.Telefon       
                   TT_{&Tabell}.Mobil         
                   TT_{&Tabell}.Telefaks      
                   TT_{&Tabell}.PostNr        
                   TT_{&Tabell}.Firmanavn     
                   TT_{&Tabell}.DagligLeder   
                   TT_{&Tabell}.Adresse1      
                   TT_{&Tabell}.Adresse2      
                   TT_{&Tabell}.Medlemsstatus 
                   TT_{&Tabell}.KjedeNr       
                   TT_{&Tabell}.RegionNr      
                   TT_{&Tabell}.DistriktNr    
                   TT_{&Tabell}.EDato         
                   TT_{&Tabell}.ETid          
                   TT_{&Tabell}.BrukerID      
                   TT_{&Tabell}.RegistrertDato
                   TT_{&Tabell}.RegistrertTid 
                   TT_{&Tabell}.RegistrertAv.
        END.
    END.
    IF iAntNyEndre > 0 THEN DO:
        EXPORT "H" cTabellNavn 1 "1.0" iAntNyEndre.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "POS"
                            AND TT_Elogg.EndringsType = 1:
            FIND {&Tabell} WHERE {&Tabell}.{&KeyFelt}  = INT(ENTRY(1,TT_Elogg.Verdier)) AND 
                                 {&Tabell}.{&KeyFelt2} = INT(ENTRY(2,TT_Elogg.Verdier)) AND
                                 {&Tabell}.{&KeyFelt3} = INT(ENTRY(3,TT_Elogg.Verdier)) AND
                                 {&Tabell}.{&KeyFelt4} = INT(ENTRY(4,TT_Elogg.Verdier)) NO-LOCK NO-ERROR.
            IF AVAIL {&Tabell} THEN
                EXPORT {&Tabell}.ButikkNr      
                       {&Tabell}.ButikkNavn    
                       {&Tabell}.Kontaktperson 
                       {&Tabell}.E-Mail        
                       {&Tabell}.Telefon       
                       {&Tabell}.Mobil         
                       {&Tabell}.Telefaks      
                       {&Tabell}.PostNr        
                       {&Tabell}.Firmanavn     
                       {&Tabell}.DagligLeder   
                       {&Tabell}.Adresse1      
                       {&Tabell}.Adresse2      
                       {&Tabell}.Medlemsstatus 
                       {&Tabell}.KjedeNr       
                       {&Tabell}.RegionNr      
                       {&Tabell}.DistriktNr    
                       {&Tabell}.EDato         
                       {&Tabell}.ETid          
                       {&Tabell}.BrukerID      
                       {&Tabell}.RegistrertDato
                       {&Tabell}.RegistrertTid 
                       {&Tabell}.RegistrertAv.
        END.
    END.
    &UNDEFINE Tabell
    &UNDEFINE KeyFelt
    &UNDEFINE KeyFelt2
    &UNDEFINE KeyFelt3
    &UNDEFINE KeyFelt4
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendKjedeRegion C-Win 
PROCEDURE SendKjedeRegion :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    &SCOPED-DEFINE Tabell  KjedeRegion
    &SCOPED-DEFINE KeyFelt KjedeNr
    &SCOPED-DEFINE KeyFelt2 RegionNr
    DEFINE VARIABLE iAntSlett   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iAntNyEndre AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cTabellNavn AS CHARACTER  INIT "{&Tabell}" NO-UNDO.
    EMPTY TEMP-TABLE TT_{&Tabell}.
    RUN KopierElogg (cTabellNavn,OUTPUT iAntSlett,OUTPUT iAntNyEndre).
    IF iAntSlett > 0 THEN DO:
        EXPORT "H" cTabellNavn 3 "1.0" iAntSlett.
        CREATE TT_{&Tabell}.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "POS"
                            AND TT_Elogg.EndringsType = 3:
            ASSIGN TT_{&Tabell}.{&KeyFelt}  = INT(ENTRY(1,TT_Elogg.Verdier))
                   TT_{&Tabell}.{&KeyFelt2} = INT(ENTRY(2,TT_Elogg.Verdier)).
            EXPORT TT_{&Tabell}.KjedeNr       
                   TT_{&Tabell}.EDato         
                   TT_{&Tabell}.ETid          
                   TT_{&Tabell}.BrukerID      
                   TT_{&Tabell}.RegistrertDato
                   TT_{&Tabell}.RegistrertTid 
                   TT_{&Tabell}.RegistrertAv  
                   TT_{&Tabell}.RegionNr      
                   TT_{&Tabell}.RegionNavn    
                   TT_{&Tabell}.RegionKNavn   
                   TT_{&Tabell}.Kontaktperson 
                   TT_{&Tabell}.Telefon       
                   TT_{&Tabell}.Mombil        
                   TT_{&Tabell}.EMail.
        END.
    END.
    IF iAntNyEndre > 0 THEN DO:
        EXPORT "H" cTabellNavn 1 "1.0" iAntNyEndre.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "POS"
                            AND TT_Elogg.EndringsType = 1:
            FIND {&Tabell} WHERE {&Tabell}.{&KeyFelt}  = INT(ENTRY(1,TT_Elogg.Verdier)) AND 
                                 {&Tabell}.{&KeyFelt2} = INT(ENTRY(2,TT_Elogg.Verdier))
                NO-LOCK NO-ERROR.
            IF AVAIL {&Tabell} THEN
                EXPORT {&Tabell}.KjedeNr       
                       {&Tabell}.EDato         
                       {&Tabell}.ETid          
                       {&Tabell}.BrukerID      
                       {&Tabell}.RegistrertDato
                       {&Tabell}.RegistrertTid 
                       {&Tabell}.RegistrertAv  
                       {&Tabell}.RegionNr      
                       {&Tabell}.RegionNavn    
                       {&Tabell}.RegionKNavn   
                       {&Tabell}.Kontaktperson 
                       {&Tabell}.Telefon       
                       {&Tabell}.Mombil        
                       {&Tabell}.EMail.
        END.
    END.
    &UNDEFINE Tabell
    &UNDEFINE KeyFelt
    &UNDEFINE KeyFelt2
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendKlack C-Win 
PROCEDURE SendKlack :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    &SCOPED-DEFINE Tabell  Klack
    &SCOPED-DEFINE KeyFelt klack-id
    DEFINE VARIABLE iAntSlett   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iAntNyEndre AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cTabellNavn AS CHARACTER  INIT "{&Tabell}" NO-UNDO.
    EMPTY TEMP-TABLE TT_{&Tabell}.
    RUN KopierElogg (cTabellNavn,OUTPUT iAntSlett,OUTPUT iAntNyEndre).
    IF iAntSlett > 0 THEN DO:
        EXPORT "H" cTabellNavn 3 "1.0" iAntSlett.
        CREATE TT_{&Tabell}.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "POS"
                            AND TT_Elogg.EndringsType = 3:
            ASSIGN TT_{&Tabell}.{&KeyFelt} = INT(TT_Elogg.Verdier).
            EXPORT TT_{&Tabell}.beskrivning   
                                 TT_{&Tabell}.klack-id      
                                 TT_{&Tabell}.EDato         
                                 TT_{&Tabell}.ETid          
                                 TT_{&Tabell}.BrukerID      
                                 TT_{&Tabell}.RegistrertDato
                                 TT_{&Tabell}.RegistrertTid 
                                 TT_{&Tabell}.RegistrertAv.

        END.
    END.
    IF iAntNyEndre > 0 THEN DO:
        EXPORT "H" cTabellNavn 1 "1.0" iAntNyEndre.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "POS"
                            AND TT_Elogg.EndringsType = 1:
            FIND {&Tabell} WHERE {&Tabell}.{&KeyFelt} = INT(TT_Elogg.Verdier) NO-LOCK NO-ERROR.
            IF AVAIL {&Tabell} THEN
                EXPORT {&Tabell}.beskrivning   
                                     {&Tabell}.klack-id      
                                     {&Tabell}.EDato         
                                     {&Tabell}.ETid          
                                     {&Tabell}.BrukerID      
                                     {&Tabell}.RegistrertDato
                                     {&Tabell}.RegistrertTid 
                                     {&Tabell}.RegistrertAv.
        END.
    END.
    &UNDEFINE Tabell
    &UNDEFINE KeyFelt
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendKravkode C-Win 
PROCEDURE SendKravkode :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    &SCOPED-DEFINE Tabell  Kravkode
    &SCOPED-DEFINE KeyFelt KravKode
    DEFINE VARIABLE iAntSlett   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iAntNyEndre AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cTabellNavn AS CHARACTER  INIT "{&Tabell}" NO-UNDO.
    EMPTY TEMP-TABLE TT_{&Tabell}.
    RUN KopierElogg (cTabellNavn,OUTPUT iAntSlett,OUTPUT iAntNyEndre).
    IF iAntSlett > 0 THEN DO:
        EXPORT "H" cTabellNavn 3 "1.0" iAntSlett.
        CREATE TT_{&Tabell}.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "POS"
                            AND TT_Elogg.EndringsType = 3:
            ASSIGN TT_{&Tabell}.{&KeyFelt} = INT(TT_Elogg.Verdier).
            EXPORT TT_{&Tabell}.KravKode      
                   TT_{&Tabell}.Beskrivelse   
                   TT_{&Tabell}.Notat         
                   TT_{&Tabell}.EDato         
                   TT_{&Tabell}.ETid          
                   TT_{&Tabell}.BrukerID      
                   TT_{&Tabell}.RegistrertDato
                   TT_{&Tabell}.RegistrertTid 
                   TT_{&Tabell}.RegistrertAv.
        END.
    END.
    IF iAntNyEndre > 0 THEN DO:
        EXPORT "H" cTabellNavn 1 "1.0" iAntNyEndre.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "POS"
                            AND TT_Elogg.EndringsType = 1:
            FIND {&Tabell} WHERE {&Tabell}.{&KeyFelt} = INT(TT_Elogg.Verdier) NO-LOCK NO-ERROR.
            IF AVAIL {&Tabell} THEN
                EXPORT {&Tabell}.KravKode      
                       {&Tabell}.Beskrivelse   
                       {&Tabell}.Notat         
                       {&Tabell}.EDato         
                       {&Tabell}.ETid          
                       {&Tabell}.BrukerID      
                       {&Tabell}.RegistrertDato
                       {&Tabell}.RegistrertTid 
                       {&Tabell}.RegistrertAv.
        END.
    END.
    &UNDEFINE Tabell
    &UNDEFINE KeyFelt
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendLast-Sko C-Win 
PROCEDURE SendLast-Sko :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    &SCOPED-DEFINE Tabell  Last-Sko
    &SCOPED-DEFINE KeyFelt Last-Id
    DEFINE VARIABLE iAntSlett   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iAntNyEndre AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cTabellNavn AS CHARACTER  INIT "{&Tabell}" NO-UNDO.
    EMPTY TEMP-TABLE TT_{&Tabell}.
    RUN KopierElogg (cTabellNavn,OUTPUT iAntSlett,OUTPUT iAntNyEndre).
    IF iAntSlett > 0 THEN DO:
        EXPORT "H" cTabellNavn 3 "1.0" iAntSlett.
        CREATE TT_{&Tabell}.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "POS"
                            AND TT_Elogg.EndringsType = 3:
            ASSIGN TT_{&Tabell}.{&KeyFelt} = INT(TT_Elogg.Verdier).
            EXPORT TT_{&Tabell}.Last-Id       
                                 TT_{&Tabell}.LastBeskr     
                                 TT_{&Tabell}.EDato         
                                 TT_{&Tabell}.ETid          
                                 TT_{&Tabell}.BrukerID      
                                 TT_{&Tabell}.RegistrertDato
                                 TT_{&Tabell}.RegistrertTid 
                                 TT_{&Tabell}.RegistrertAv.
        END.
    END.
    IF iAntNyEndre > 0 THEN DO:
        EXPORT "H" cTabellNavn 1 "1.0" iAntNyEndre.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "POS"
                            AND TT_Elogg.EndringsType = 1:
            FIND {&Tabell} WHERE {&Tabell}.{&KeyFelt} = INT(TT_Elogg.Verdier) NO-LOCK NO-ERROR.
            IF AVAIL {&Tabell} THEN
                EXPORT {&Tabell}.Last-Id       
                                     {&Tabell}.LastBeskr     
                                     {&Tabell}.EDato         
                                     {&Tabell}.ETid          
                                     {&Tabell}.BrukerID      
                                     {&Tabell}.RegistrertDato
                                     {&Tabell}.RegistrertTid 
                                     {&Tabell}.RegistrertAv.
        END.
    END.
    &UNDEFINE Tabell
    &UNDEFINE KeyFelt
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendLevBas C-Win 
PROCEDURE SendLevBas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    &SCOPED-DEFINE Tabell  LevBas
    &SCOPED-DEFINE KeyFelt levnr
    DEFINE VARIABLE iAntSlett   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iAntNyEndre AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cTabellNavn AS CHARACTER  INIT "{&Tabell}" NO-UNDO.

    DEF BUFFER bLevBas FOR LevBas.

    EMPTY TEMP-TABLE TT_{&Tabell}.

    RUN KopierElogg (cTabellNavn,OUTPUT iAntSlett,OUTPUT iAntNyEndre).

    /* Renser bort E-Logg poster som ikke skal være der.   */
    /* Lokalt opprettede leverandører skal ikke sendes ut. */
    FOR EACH TT_ELogg WHERE 
        TT_ELogg.TabellNavn     = cTabellNavn AND 
        TT_ELogg.EksterntSystem = "POS".
        
        FIND bLevBas NO-LOCK WHERE
            bLevBas.LevNr = INT(TT_Elogg.Verdier) NO-ERROR.
        IF NOT AVAILABLE bLevBas THEN 
            DELETE TT_ELogg.
        ELSE IF AVAILABLE TT_ELogg AND TT_Elogg.EndringsType = 1 THEN DO:
            FIND LevBas WHERE LevBas.LevNr = INT(TT_Elogg.Verdier) NO-LOCK NO-ERROR.
            IF NOT AVAIL LevBas OR LevBas.levnamn BEGINS "Lokal" THEN
                DELETE TT_ELogg.
        END.
    END.
    ASSIGN
        iAntSlett   = 0
        iAntNyEndre = 0
        .
    FOR EACH TT_ELogg WHERE 
        TT_ELogg.TabellNavn     = cTabellNavn AND 
        TT_ELogg.EksterntSystem = "POS":

        IF TT_Elogg.EndringsType = 3 THEN
            iAntSlett = iAntSlett + 1.
        IF TT_Elogg.EndringsType = 1 THEN
            iAntNyEndre = iAntNyEndre + 1.
    END.

    IF iAntSlett > 0 THEN 
    DO:
        EXPORT "H" cTabellNavn 3 "1.0" iAntSlett.
        CREATE TT_{&Tabell}.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "POS"
                            AND TT_Elogg.EndringsType = 3:
            ASSIGN TT_{&Tabell}.{&KeyFelt} = INT(TT_Elogg.Verdier).
            EXPORT TT_{&Tabell}.levnr         
                   TT_{&Tabell}.levnamn       
                   TT_{&Tabell}.levadr        
                   TT_{&Tabell}.levponr       
                   TT_{&Tabell}.levpadr       
                   TT_{&Tabell}.levland       
                   TT_{&Tabell}.levtel        
                   TT_{&Tabell}.levkon        
                   TT_{&Tabell}.levsal        
                   TT_{&Tabell}.telefax       
                   TT_{&Tabell}.telex         
                   TT_{&Tabell}.kommentar[1]  
                   TT_{&Tabell}.kommentar[2]  
                   TT_{&Tabell}.kommentar[3]  
                   TT_{&Tabell}.kommentar[4]  
                   TT_{&Tabell}.valkod        
                   TT_{&Tabell}.koadr         
                   TT_{&Tabell}.koponr        
                   TT_{&Tabell}.kopadr        
                   TT_{&Tabell}.koland        
                   TT_{&Tabell}.kotel         
                   TT_{&Tabell}.kotelefax     
                   TT_{&Tabell}.kotelex       
                   TT_{&Tabell}.betant        
                   TT_{&Tabell}.EDato         
                   TT_{&Tabell}.ETid          
                   TT_{&Tabell}.BrukerID      
                   TT_{&Tabell}.BildNr        
                   TT_{&Tabell}.RegistrertDato
                   TT_{&Tabell}.RegistrertTid 
                   TT_{&Tabell}.RegistrertAv  
                   TT_{&Tabell}.Notat
                   TT_{&Tabell}.VisDivInfo[ 1]
                   TT_{&Tabell}.VisDivInfo[ 2]
                   TT_{&Tabell}.VisDivInfo[ 3]
                   TT_{&Tabell}.VisDivInfo[ 4]
                   TT_{&Tabell}.VisDivInfo[ 5]
                   TT_{&Tabell}.VisDivInfo[ 6]
                   TT_{&Tabell}.VisDivInfo[ 7]
                   TT_{&Tabell}.VisDivInfo[ 8]
                   TT_{&Tabell}.VisDivInfo[ 9]
                   TT_{&Tabell}.VisDivInfo[10]
                   TT_{&Tabell}.VisDivInfo[11]
                   TT_{&Tabell}.VisDivInfo[12]
                   TT_{&Tabell}.VisDivInfo[13]
                   TT_{&Tabell}.VisDivInfo[14]
                   TT_{&Tabell}.VisDivInfo[15]
                   TT_{&Tabell}.VisDivInfo[16]
                   TT_{&Tabell}.VisDivInfo[17]
                   TT_{&Tabell}.VisDivInfo[18]
                   TT_{&Tabell}.VisDivInfo[19]
                   TT_{&Tabell}.VisDivInfo[20]
                   TT_{&Tabell}.Lng           
                   TT_{&Tabell}.E_MailKontakt 
                   TT_{&Tabell}.E_MailLev
                   TT_{&Tabell}.Kjedeavtale
                   TT_{&Tabell}.ReklAdresse1      
                   TT_{&Tabell}.ReklAdresse2      
                   TT_{&Tabell}.ReklPostNr        
                   TT_{&Tabell}.ReklPostBoks      
                   TT_{&Tabell}.Rab1%             
                   TT_{&Tabell}.Rab2%             
                   TT_{&Tabell}.Frakt%            
                   TT_{&Tabell}.DivKost%          
                   TT_{&Tabell}.Rab3%             
                   TT_{&Tabell}.EgetKundeNrHosLev 
                   TT_{&Tabell}.supRab1%          
                   TT_{&Tabell}.supRab2%          
                   TT_{&Tabell}.supDivKost%       
                   TT_{&Tabell}.supRab3%          
                   .
        END.
    END.
    IF iAntNyEndre > 0 THEN DO:
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "POS"
                            AND TT_Elogg.EndringsType = 1:
            FIND {&Tabell} WHERE {&Tabell}.{&KeyFelt} = INT(TT_Elogg.Verdier) NO-LOCK NO-ERROR.
            IF AVAIL {&Tabell} THEN DO:
                EXPORT "H" cTabellNavn 1 "1.0" 1 /*iAntNyEndre*/.
                EXPORT {&Tabell}.levnr         
                       {&Tabell}.levnamn       
                       {&Tabell}.levadr        
                       {&Tabell}.levponr       
                       {&Tabell}.levpadr       
                       {&Tabell}.levland       
                       {&Tabell}.levtel        
                       {&Tabell}.levkon        
                       {&Tabell}.levsal        
                       {&Tabell}.telefax       
                       {&Tabell}.telex         
                       {&Tabell}.kommentar[1]  
                       {&Tabell}.kommentar[2]  
                       {&Tabell}.kommentar[3]  
                       {&Tabell}.kommentar[4]  
                       {&Tabell}.valkod        
                       {&Tabell}.koadr         
                       {&Tabell}.koponr        
                       {&Tabell}.kopadr        
                       {&Tabell}.koland        
                       {&Tabell}.kotel         
                       {&Tabell}.kotelefax     
                       {&Tabell}.kotelex       
                       {&Tabell}.betant        
                       {&Tabell}.EDato         
                       {&Tabell}.ETid          
                       {&Tabell}.BrukerID      
                       {&Tabell}.BildNr        
                       {&Tabell}.RegistrertDato
                       {&Tabell}.RegistrertTid 
                       {&Tabell}.RegistrertAv  
/*                        {&Tabell}.Notat */
                       REPLACE(REPLACE({&Tabell}.Notat,CHR(10)," "),CHR(13)," ")
                       {&Tabell}.VisDivInfo[ 1]
                       {&Tabell}.VisDivInfo[ 2]
                       {&Tabell}.VisDivInfo[ 3]
                       {&Tabell}.VisDivInfo[ 4]
                       {&Tabell}.VisDivInfo[ 5]
                       {&Tabell}.VisDivInfo[ 6]
                       {&Tabell}.VisDivInfo[ 7]
                       {&Tabell}.VisDivInfo[ 8]
                       {&Tabell}.VisDivInfo[ 9]
                       {&Tabell}.VisDivInfo[10]
                       {&Tabell}.VisDivInfo[11]
                       {&Tabell}.VisDivInfo[12]
                       {&Tabell}.VisDivInfo[13]
                       {&Tabell}.VisDivInfo[14]
                       {&Tabell}.VisDivInfo[15]
                       {&Tabell}.VisDivInfo[16]
                       {&Tabell}.VisDivInfo[17]
                       {&Tabell}.VisDivInfo[18]
                       {&Tabell}.VisDivInfo[19]
                       {&Tabell}.VisDivInfo[20]
                       {&Tabell}.Lng           
                       {&Tabell}.E_MailKontakt 
                       {&Tabell}.E_MailLev
                       {&Tabell}.Kjedeavtale
                       {&Tabell}.ReklAdresse1      
                       {&Tabell}.ReklAdresse2      
                       {&Tabell}.ReklPostNr        
                       {&Tabell}.ReklPostBoks      
                       {&Tabell}.Rab1%             
                       {&Tabell}.Rab2%             
                       {&Tabell}.Frakt%            
                       {&Tabell}.DivKost%          
                       {&Tabell}.Rab3%             
                       {&Tabell}.EgetKundeNrHosLev 
                       {&Tabell}.supRab1%          
                       {&Tabell}.supRab2%          
                       {&Tabell}.supDivKost%       
                       {&Tabell}.supRab3%          
                       .
                RUN getAntLevKontakt ({&Tabell}.levnr, OUTPUT iAntNyEndre).
                IF iAntNyEndre > 0 THEN DO:
                    EXPORT "H" "LevKontakt" 1 "1.0" iAntNyEndre.
                    FOR EACH LevKontakt NO-LOCK WHERE
                        LevKontakt.LevNr = {&Tabell}.levnr:

                      EXPORT 
                        Levkontakt.LevNr       
                        Levkontakt.Navn           
                        Levkontakt.Stilling       
                        Levkontakt.Telefon        
                        Levkontakt.Telefaks       
                        Levkontakt.Mobiltelefon   
                        Levkontakt.Merknad        
                        Levkontakt.EDato          
                        Levkontakt.ETid           
                        Levkontakt.BrukerID       
                        Levkontakt.RegistrertDato 
                        Levkontakt.RegistrertTid  
                        Levkontakt.RegistrertAv   
                        Levkontakt.KontNr         
                        Levkontakt.E_MailKontakt  
                        .
                    END.
                END.
            END.
        END.
    END.
    EMPTY TEMP-TABLE TT_{&Tabell}.
    &UNDEFINE Tabell
    &UNDEFINE KeyFelt
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendLevsort C-Win 
PROCEDURE SendLevsort :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    &SCOPED-DEFINE Tabell_1  LevSort
    &SCOPED-DEFINE Tabell_2  LevSant
    &SCOPED-DEFINE KeyFelt_1 LevNr
    &SCOPED-DEFINE KeyFelt_2 SortID
    DEFINE VARIABLE iAntSlett   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iAntNyEndre AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cTabellNavn_1 AS CHARACTER  INIT "{&Tabell_1}" NO-UNDO.
    DEFINE VARIABLE cTabellNavn_2 AS CHARACTER  INIT "{&Tabell_2}" NO-UNDO.
    EMPTY TEMP-TABLE TT_{&Tabell_1}.
    EMPTY TEMP-TABLE TT_{&Tabell_2}.

    RUN KopierEloggLevSort (cTabellNavn_1,OUTPUT iAntSlett,OUTPUT iAntNyEndre).
    
    /* Renser bort E-Logg poster som ikke skal være der. */
    FOR EACH TT_ELogg WHERE 
        TT_ELogg.TabellNavn           = cTabellNavn_1 AND 
        TT_ELogg.EksterntSystem       = "POS":
        IF INT(ENTRY(1,TT_Elogg.Verdier)) > iLevNrMax  THEN
            DELETE TT_ELogg.
        ELSE IF TT_Elogg.EndringsType = 1 THEN DO:
            FIND LevBas WHERE LevBas.LevNr = INT(ENTRY(1,TT_Elogg.Verdier)) NO-LOCK NO-ERROR.
            IF NOT AVAIL LevBas OR LevBas.LevNamn BEGINS "Lokal lev" THEN
                DELETE TT_Elogg.
        END.
    END.
    ASSIGN
        iAntSlett   = 0
        iAntNyEndre = 0
        .
    FOR EACH TT_ELogg WHERE 
        TT_ELogg.TabellNavn     = cTabellNavn_1 AND 
        TT_ELogg.EksterntSystem = "POS":

        IF TT_Elogg.EndringsType = 3 THEN
            iAntSlett = iAntSlett + 1.
        IF TT_Elogg.EndringsType = 1 THEN
            iAntNyEndre = iAntNyEndre + 1.
    END.

    IF iAntSlett > 0 THEN DO:
        EXPORT "H" cTabellNavn_1 3 "1.0" iAntSlett.
        CREATE TT_{&Tabell_1}.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn_1 AND 
                                TT_ELogg.EksterntSystem = "POS"
                            AND TT_Elogg.EndringsType = 3:
            ASSIGN TT_{&Tabell_1}.{&KeyFelt_1} = INT(ENTRY(1,TT_Elogg.Verdier))
                   TT_{&Tabell_1}.{&KeyFelt_2} = ENTRY(2,TT_Elogg.Verdier).
            EXPORT TT_{&Tabell_1}.LevNr      
                   TT_{&Tabell_1}.StrTypeID  
                   TT_{&Tabell_1}.SortID     
                   TT_{&Tabell_1}.Beskrivelse
                   TT_{&Tabell_1}.Merknad    
                   TT_{&Tabell_1}.Fri.
        END.
    END.
    IF iAntNyEndre > 0 THEN DO:
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn_1 AND TT_ELogg.EksterntSystem = "POS"
                            AND TT_Elogg.EndringsType = 1:
            EXPORT "H" cTabellNavn_1 1 "1.0" 1.
            FIND {&Tabell_1} WHERE {&Tabell_1}.{&KeyFelt_1} = INT(ENTRY(1,TT_Elogg.Verdier)) AND
                                   {&Tabell_1}.{&KeyFelt_2} = ENTRY(2,TT_Elogg.Verdier) NO-LOCK NO-ERROR.
            IF AVAIL {&Tabell_1} THEN DO:
                EXPORT {&Tabell_1}.LevNr      
                       {&Tabell_1}.StrTypeID  
                       {&Tabell_1}.SortID     
                       {&Tabell_1}.Beskrivelse
                       {&Tabell_1}.Merknad    
                       {&Tabell_1}.Fri.
                EXPORT "H" cTabellNavn_2 1 "1.0" getAntLevSAnt({&Tabell_1}.{&KeyFelt_1},{&Tabell_1}.{&KeyFelt_2}).
                FOR EACH {&Tabell_2} OF {&Tabell_1}:
                    EXPORT {&Tabell_2}.SoStorl
                           {&Tabell_2}.SoAnt  
                           {&Tabell_2}.SeqNr  
                           {&Tabell_2}.LevNr  
                           {&Tabell_2}.SortID. 
                END.
            END.
        END.
    END.
    &UNDEFINE Tabell_1
    &UNDEFINE Tabell_2
    &UNDEFINE KeyFelt_1
    &UNDEFINE KeyFelt_2
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendMaterial C-Win 
PROCEDURE SendMaterial :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    &SCOPED-DEFINE Tabell  Material
    &SCOPED-DEFINE KeyFelt MatKod
    DEFINE VARIABLE iAntSlett   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iAntNyEndre AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cTabellNavn AS CHARACTER  INIT "{&Tabell}" NO-UNDO.
    EMPTY TEMP-TABLE TT_{&Tabell}.
    RUN KopierElogg (cTabellNavn,OUTPUT iAntSlett,OUTPUT iAntNyEndre).
    IF iAntSlett > 0 THEN DO:
        EXPORT "H" cTabellNavn 3 "1.0" iAntSlett.
        CREATE TT_{&Tabell}.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "POS"
                            AND TT_Elogg.EndringsType = 3:
            ASSIGN TT_{&Tabell}.{&KeyFelt} = INT(TT_Elogg.Verdier).
            EXPORT TT_{&Tabell}.MatKod        
                                 TT_{&Tabell}.MatBeskr      
                                 TT_{&Tabell}.EDato         
                                 TT_{&Tabell}.ETid          
                                 TT_{&Tabell}.BrukerID      
                                 TT_{&Tabell}.RegistrertDato
                                 TT_{&Tabell}.RegistrertTid 
                                 TT_{&Tabell}.RegistrertAv.
        END.
    END.
    IF iAntNyEndre > 0 THEN DO:
        EXPORT "H" cTabellNavn 1 "1.0" iAntNyEndre.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "POS"
                            AND TT_Elogg.EndringsType = 1:
            FIND {&Tabell} WHERE {&Tabell}.{&KeyFelt} = INT(TT_Elogg.Verdier) NO-LOCK NO-ERROR.
            IF AVAIL {&Tabell} THEN
                EXPORT {&Tabell}.MatKod        
                                     {&Tabell}.MatBeskr      
                                     {&Tabell}.EDato         
                                     {&Tabell}.ETid          
                                     {&Tabell}.BrukerID      
                                     {&Tabell}.RegistrertDato
                                     {&Tabell}.RegistrertTid 
                                     {&Tabell}.RegistrertAv.
        END.
    END.
    &UNDEFINE Tabell
    &UNDEFINE KeyFelt
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendMoms C-Win 
PROCEDURE SendMoms :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    &SCOPED-DEFINE Tabell  Moms
    &SCOPED-DEFINE KeyFelt MomsKod
    DEFINE VARIABLE iAntSlett   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iAntNyEndre AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cTabellNavn AS CHARACTER  INIT "{&Tabell}" NO-UNDO.
    EMPTY TEMP-TABLE TT_{&Tabell}.
    RUN KopierElogg (cTabellNavn,OUTPUT iAntSlett,OUTPUT iAntNyEndre).
    IF iAntSlett > 0 THEN DO:
        EXPORT "H" cTabellNavn 3 "1.0" iAntSlett.
        CREATE TT_{&Tabell}.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "POS"
                            AND TT_Elogg.EndringsType = 3:
            ASSIGN TT_{&Tabell}.{&KeyFelt} = INT(TT_Elogg.Verdier).
            EXPORT TT_{&Tabell}.MomsKod       
                                 TT_{&Tabell}.MomsProc      
                                 TT_{&Tabell}.Beskrivelse   
                                 TT_{&Tabell}.EDato         
                                 TT_{&Tabell}.ETid          
                                 TT_{&Tabell}.BrukerID      
                                 TT_{&Tabell}.RegistrertDato
                                 TT_{&Tabell}.RegistrertTid 
                                 TT_{&Tabell}.RegistrertAv.
        END.
    END.
    IF iAntNyEndre > 0 THEN DO:
        EXPORT "H" cTabellNavn 1 "1.0" iAntNyEndre.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "POS"
                            AND TT_Elogg.EndringsType = 1:
            FIND {&Tabell} WHERE {&Tabell}.{&KeyFelt} = INT(TT_Elogg.Verdier) NO-LOCK NO-ERROR.
            IF AVAIL {&Tabell} THEN
                EXPORT {&Tabell}.MomsKod       
                                     {&Tabell}.MomsProc      
                                     {&Tabell}.Beskrivelse   
                                     {&Tabell}.EDato         
                                     {&Tabell}.ETid          
                                     {&Tabell}.BrukerID      
                                     {&Tabell}.RegistrertDato
                                     {&Tabell}.RegistrertTid 
                                     {&Tabell}.RegistrertAv.
        END.
    END.
    &UNDEFINE Tabell
    &UNDEFINE KeyFelt
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendOvandel C-Win 
PROCEDURE SendOvandel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    &SCOPED-DEFINE Tabell  Ovandel
    &SCOPED-DEFINE KeyFelt Ov-Id
    DEFINE VARIABLE iAntSlett   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iAntNyEndre AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cTabellNavn AS CHARACTER  INIT "{&Tabell}" NO-UNDO.
    EMPTY TEMP-TABLE TT_{&Tabell}.
    RUN KopierElogg (cTabellNavn,OUTPUT iAntSlett,OUTPUT iAntNyEndre).
    IF iAntSlett > 0 THEN DO:
        EXPORT "H" cTabellNavn 3 "1.0" iAntSlett.
        CREATE TT_{&Tabell}.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "POS"
                            AND TT_Elogg.EndringsType = 3:
            ASSIGN TT_{&Tabell}.{&KeyFelt} = INT(TT_Elogg.Verdier).
            EXPORT TT_{&Tabell}.Ov-Id         
                                 TT_{&Tabell}.OvBeskr       
                                 TT_{&Tabell}.EDato         
                                 TT_{&Tabell}.ETid          
                                 TT_{&Tabell}.BrukerID      
                                 TT_{&Tabell}.RegistrertDato
                                 TT_{&Tabell}.RegistrertTid 
                                 TT_{&Tabell}.RegistrertAv.
        END.
    END.
    IF iAntNyEndre > 0 THEN DO:
        EXPORT "H" cTabellNavn 1 "1.0" iAntNyEndre.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "POS"
                            AND TT_Elogg.EndringsType = 1:
            FIND {&Tabell} WHERE {&Tabell}.{&KeyFelt} = INT(TT_Elogg.Verdier) NO-LOCK NO-ERROR.
            IF AVAIL {&Tabell} THEN
                EXPORT {&Tabell}.Ov-Id         
                                     {&Tabell}.OvBeskr       
                                     {&Tabell}.EDato         
                                     {&Tabell}.ETid          
                                     {&Tabell}.BrukerID      
                                     {&Tabell}.RegistrertDato
                                     {&Tabell}.RegistrertTid 
                                     {&Tabell}.RegistrertAv.
        END.
    END.
    &UNDEFINE Tabell
    &UNDEFINE KeyFelt
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendPost C-Win 
PROCEDURE SendPost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    &SCOPED-DEFINE Tabell  Post
    &SCOPED-DEFINE KeyFelt PostNr
    DEFINE VARIABLE iAntSlett   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iAntNyEndre AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cTabellNavn AS CHARACTER  INIT "{&Tabell}" NO-UNDO.
    EMPTY TEMP-TABLE TT_{&Tabell}.
    RUN KopierElogg (cTabellNavn,OUTPUT iAntSlett,OUTPUT iAntNyEndre).
    IF iAntSlett > 0 THEN DO:
        EXPORT "H" cTabellNavn 3 "1.0" iAntSlett.
        CREATE TT_{&Tabell}.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "POS"
                            AND TT_Elogg.EndringsType = 3:
            ASSIGN TT_{&Tabell}.{&KeyFelt} = TT_Elogg.Verdier.
            EXPORT TT_{&Tabell}.PostNr        
                                 TT_{&Tabell}.KommNr        
                                 TT_{&Tabell}.Beskrivelse   
                                 TT_{&Tabell}.Merknad       
                                 TT_{&Tabell}.FylkesNr      
                                 TT_{&Tabell}.EDato         
                                 TT_{&Tabell}.ETid          
                                 TT_{&Tabell}.BrukerID      
                                 TT_{&Tabell}.RegistrertDato
                                 TT_{&Tabell}.RegistrertTid 
                                 TT_{&Tabell}.RegistrertAv.
        END.
    END.
    IF iAntNyEndre > 0 THEN DO:
        EXPORT "H" cTabellNavn 1 "1.0" iAntNyEndre.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "POS"
                            AND TT_Elogg.EndringsType = 1:
            FIND {&Tabell} WHERE {&Tabell}.{&KeyFelt} = TT_Elogg.Verdier NO-LOCK NO-ERROR.
            IF AVAIL {&Tabell} THEN
                EXPORT {&Tabell}.PostNr        
                                     {&Tabell}.KommNr        
                                     {&Tabell}.Beskrivelse   
                                     {&Tabell}.Merknad       
                                     {&Tabell}.FylkesNr      
                                     {&Tabell}.EDato         
                                     {&Tabell}.ETid          
                                     {&Tabell}.BrukerID      
                                     {&Tabell}.RegistrertDato
                                     {&Tabell}.RegistrertTid 
                                     {&Tabell}.RegistrertAv.
        END.
    END.
    &UNDEFINE Tabell
    &UNDEFINE KeyFelt
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendPrisgruppe C-Win 
PROCEDURE SendPrisgruppe :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    &SCOPED-DEFINE Tabell  Prisgruppe
    &SCOPED-DEFINE KeyFelt PrisGrpNr
    DEFINE VARIABLE iAntSlett   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iAntNyEndre AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cTabellNavn AS CHARACTER  INIT "{&Tabell}" NO-UNDO.
    EMPTY TEMP-TABLE TT_{&Tabell}.
    RUN KopierElogg (cTabellNavn,OUTPUT iAntSlett,OUTPUT iAntNyEndre).
    IF iAntSlett > 0 THEN DO:
        EXPORT "H" cTabellNavn 3 "1.0" iAntSlett.
        CREATE TT_{&Tabell}.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "POS"
                            AND TT_Elogg.EndringsType = 3:
            ASSIGN TT_{&Tabell}.{&KeyFelt} = INT(TT_Elogg.Verdier).
            EXPORT TT_{&Tabell}.PrisGrpNr     
                                 TT_{&Tabell}.Beskrivelse   
                                 TT_{&Tabell}.FraPris       
                                 TT_{&Tabell}.TilPris       
                                 TT_{&Tabell}.EDato         
                                 TT_{&Tabell}.ETid          
                                 TT_{&Tabell}.BrukerID      
                                 TT_{&Tabell}.RegistrertDato
                                 TT_{&Tabell}.RegistrertTid 
                                 TT_{&Tabell}.RegistrertAv.
        END.
    END.
    IF iAntNyEndre > 0 THEN DO:
        EXPORT "H" cTabellNavn 1 "1.0" iAntNyEndre.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "POS"
                            AND TT_Elogg.EndringsType = 1:
            FIND {&Tabell} WHERE {&Tabell}.{&KeyFelt} = INT(TT_Elogg.Verdier) NO-LOCK NO-ERROR.
            IF AVAIL {&Tabell} THEN
                EXPORT {&Tabell}.PrisGrpNr     
                                     {&Tabell}.Beskrivelse   
                                     {&Tabell}.FraPris       
                                     {&Tabell}.TilPris       
                                     {&Tabell}.EDato         
                                     {&Tabell}.ETid          
                                     {&Tabell}.BrukerID      
                                     {&Tabell}.RegistrertDato
                                     {&Tabell}.RegistrertTid 
                                     {&Tabell}.RegistrertAv.
        END.
    END.
    &UNDEFINE Tabell
    &UNDEFINE KeyFelt
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendPrisprofil C-Win 
PROCEDURE SendPrisprofil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    &SCOPED-DEFINE Tabell  Prisprofil
    &SCOPED-DEFINE KeyFelt ProfilNr
    DEFINE VARIABLE iAntSlett   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iAntNyEndre AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cTabellNavn AS CHARACTER  INIT "{&Tabell}" NO-UNDO.
    EMPTY TEMP-TABLE TT_{&Tabell}.
    RUN KopierElogg (cTabellNavn,OUTPUT iAntSlett,OUTPUT iAntNyEndre).
    IF iAntSlett > 0 THEN DO:
        EXPORT "H" cTabellNavn 3 "1.0" iAntSlett.
        CREATE TT_{&Tabell}.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "POS"
                            AND TT_Elogg.EndringsType = 3:
            ASSIGN TT_{&Tabell}.{&KeyFelt} = INT(TT_Elogg.Verdier).
            EXPORT TT_{&Tabell}.EDato         
                                 TT_{&Tabell}.ETid          
                                 TT_{&Tabell}.BrukerID      
                                 TT_{&Tabell}.RegistrertDato
                                 TT_{&Tabell}.RegistrertTid 
                                 TT_{&Tabell}.RegistrertAv  
                                 TT_{&Tabell}.ProfilNr      
                                 TT_{&Tabell}.KortNavn      
                                 TT_{&Tabell}.Beskrivelse   
                                 TT_{&Tabell}.Merknad       
                                 TT_{&Tabell}.Notat.
        END.
    END.
    IF iAntNyEndre > 0 THEN DO:
        EXPORT "H" cTabellNavn 1 "1.0" iAntNyEndre.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "POS"
                            AND TT_Elogg.EndringsType = 1:
            FIND {&Tabell} WHERE {&Tabell}.{&KeyFelt} = INT(TT_Elogg.Verdier) NO-LOCK NO-ERROR.
            IF AVAIL {&Tabell} THEN
                EXPORT {&Tabell}.EDato         
                                     {&Tabell}.ETid          
                                     {&Tabell}.BrukerID      
                                     {&Tabell}.RegistrertDato
                                     {&Tabell}.RegistrertTid 
                                     {&Tabell}.RegistrertAv  
                                     {&Tabell}.ProfilNr      
                                     {&Tabell}.KortNavn      
                                     {&Tabell}.Beskrivelse   
                                     {&Tabell}.Merknad       
                                     {&Tabell}.Notat.
        END.
    END.
    &UNDEFINE Tabell
    &UNDEFINE KeyFelt
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendProdusent C-Win 
PROCEDURE SendProdusent :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    &SCOPED-DEFINE Tabell  Produsent
    &SCOPED-DEFINE KeyFelt ProdNr
    DEFINE VARIABLE iAntSlett   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iAntNyEndre AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cTabellNavn AS CHARACTER  INIT "{&Tabell}" NO-UNDO.
    EMPTY TEMP-TABLE TT_{&Tabell}.
    RUN KopierElogg (cTabellNavn,OUTPUT iAntSlett,OUTPUT iAntNyEndre).
    IF iAntSlett > 0 THEN DO:
        EXPORT "H" cTabellNavn 3 "1.0" iAntSlett.
        CREATE TT_{&Tabell}.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "POS"
                            AND TT_Elogg.EndringsType = 3:
            ASSIGN TT_{&Tabell}.{&KeyFelt} = INT(TT_Elogg.Verdier).
            EXPORT TT_{&Tabell}.RegistrertDato
                                 TT_{&Tabell}.RegistrertTid 
                                 TT_{&Tabell}.EDato         
                                 TT_{&Tabell}.ETid          
                                 TT_{&Tabell}.BrukerID      
                                 TT_{&Tabell}.RegistrertAv  
                                 TT_{&Tabell}.Merknad       
                                 TT_{&Tabell}.Beskrivelse   
                                 TT_{&Tabell}.Notat         
                                 TT_{&Tabell}.Adresse1      
                                 TT_{&Tabell}.Adresse2      
                                 TT_{&Tabell}.PostNr        
                                 TT_{&Tabell}.PostBoks      
                                 TT_{&Tabell}.Telefon       
                                 TT_{&Tabell}.Kontakt       
                                 TT_{&Tabell}.Land          
                                 TT_{&Tabell}.ProdNr.
        END.
    END.
    IF iAntNyEndre > 0 THEN DO:
        EXPORT "H" cTabellNavn 1 "1.0" iAntNyEndre.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "POS"
                            AND TT_Elogg.EndringsType = 1:
            FIND {&Tabell} WHERE {&Tabell}.{&KeyFelt} = INT(TT_Elogg.Verdier) NO-LOCK NO-ERROR.
            IF AVAIL {&Tabell} THEN
                EXPORT {&Tabell}.RegistrertDato
                                     {&Tabell}.RegistrertTid 
                                     {&Tabell}.EDato         
                                     {&Tabell}.ETid          
                                     {&Tabell}.BrukerID      
                                     {&Tabell}.RegistrertAv  
                                     {&Tabell}.Merknad       
                                     {&Tabell}.Beskrivelse   
                                     {&Tabell}.Notat         
                                     {&Tabell}.Adresse1      
                                     {&Tabell}.Adresse2      
                                     {&Tabell}.PostNr        
                                     {&Tabell}.PostBoks      
                                     {&Tabell}.Telefon       
                                     {&Tabell}.Kontakt       
                                     {&Tabell}.Land          
                                     {&Tabell}.ProdNr.
        END.
    END.
    &UNDEFINE Tabell
    &UNDEFINE KeyFelt
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendProv C-Win 
PROCEDURE SendProv :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    &SCOPED-DEFINE Tabell  Prov
    &SCOPED-DEFINE KeyFelt ProvKod
    DEFINE VARIABLE iAntSlett   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iAntNyEndre AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cTabellNavn AS CHARACTER  INIT "{&Tabell}" NO-UNDO.
    EMPTY TEMP-TABLE TT_{&Tabell}.
    RUN KopierElogg (cTabellNavn,OUTPUT iAntSlett,OUTPUT iAntNyEndre).
    IF iAntSlett > 0 THEN DO:
        EXPORT "H" cTabellNavn 3 "1.0" iAntSlett.
        CREATE TT_{&Tabell}.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "POS"
                            AND TT_Elogg.EndringsType = 3:
            ASSIGN TT_{&Tabell}.{&KeyFelt} = INT(TT_Elogg.Verdier).
            EXPORT TT_{&Tabell}.ProvKod       
                                 TT_{&Tabell}.ProvProc      
                                 TT_{&Tabell}.ProvBeskr     
                                 TT_{&Tabell}.EDato         
                                 TT_{&Tabell}.ETid          
                                 TT_{&Tabell}.BrukerID      
                                 TT_{&Tabell}.RegistrertDato
                                 TT_{&Tabell}.RegistrertTid 
                                 TT_{&Tabell}.RegistrertAv.
        END.
    END.
    IF iAntNyEndre > 0 THEN DO:
        EXPORT "H" cTabellNavn 1 "1.0" iAntNyEndre.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "POS"
                            AND TT_Elogg.EndringsType = 1:
            FIND {&Tabell} WHERE {&Tabell}.{&KeyFelt} = INT(TT_Elogg.Verdier) NO-LOCK NO-ERROR.
            IF AVAIL {&Tabell} THEN
                EXPORT {&Tabell}.ProvKod       
                                     {&Tabell}.ProvProc      
                                     {&Tabell}.ProvBeskr     
                                     {&Tabell}.EDato         
                                     {&Tabell}.ETid          
                                     {&Tabell}.BrukerID      
                                     {&Tabell}.RegistrertDato
                                     {&Tabell}.RegistrertTid 
                                     {&Tabell}.RegistrertAv.
        END.
    END.
    &UNDEFINE Tabell
    &UNDEFINE KeyFelt
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendRabatt C-Win 
PROCEDURE SendRabatt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    &SCOPED-DEFINE Tabell  Rabatt
    &SCOPED-DEFINE KeyFelt RabKod
    DEFINE VARIABLE iAntSlett   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iAntNyEndre AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cTabellNavn AS CHARACTER  INIT "{&Tabell}" NO-UNDO.
    EMPTY TEMP-TABLE TT_{&Tabell}.
    RUN KopierElogg (cTabellNavn,OUTPUT iAntSlett,OUTPUT iAntNyEndre).
    IF iAntSlett > 0 THEN DO:
        EXPORT "H" cTabellNavn 3 "1.0" iAntSlett.
        CREATE TT_{&Tabell}.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "POS"
                            AND TT_Elogg.EndringsType = 3:
            ASSIGN TT_{&Tabell}.{&KeyFelt} = INT(TT_Elogg.Verdier).
            EXPORT TT_{&Tabell}.RabKod        
                                 TT_{&Tabell}.RabProc       
                                 TT_{&Tabell}.RabBeskr      
                                 TT_{&Tabell}.EDato         
                                 TT_{&Tabell}.ETid          
                                 TT_{&Tabell}.BrukerID      
                                 TT_{&Tabell}.RegistrertDato
                                 TT_{&Tabell}.RegistrertTid 
                                 TT_{&Tabell}.RegistrertAv.
        END.
    END.
    IF iAntNyEndre > 0 THEN DO:
        EXPORT "H" cTabellNavn 1 "1.0" iAntNyEndre.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "POS"
                            AND TT_Elogg.EndringsType = 1:
            FIND {&Tabell} WHERE {&Tabell}.{&KeyFelt} = INT(TT_Elogg.Verdier) NO-LOCK NO-ERROR.
            IF AVAIL {&Tabell} THEN
                EXPORT {&Tabell}.RabKod        
                                     {&Tabell}.RabProc       
                                     {&Tabell}.RabBeskr      
                                     {&Tabell}.EDato         
                                     {&Tabell}.ETid          
                                     {&Tabell}.BrukerID      
                                     {&Tabell}.RegistrertDato
                                     {&Tabell}.RegistrertTid 
                                     {&Tabell}.RegistrertAv.
        END.
    END.
    &UNDEFINE Tabell
    &UNDEFINE KeyFelt
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendSaSong C-Win 
PROCEDURE SendSaSong :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    &SCOPED-DEFINE Tabell  SaSong
    &SCOPED-DEFINE KeyFelt Sasong
    DEFINE VARIABLE iAntSlett   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iAntNyEndre AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cTabellNavn AS CHARACTER  INIT "{&Tabell}" NO-UNDO.
    EMPTY TEMP-TABLE TT_{&Tabell}.
    RUN KopierElogg (cTabellNavn,OUTPUT iAntSlett,OUTPUT iAntNyEndre).
    IF iAntSlett > 0 THEN DO:
        EXPORT "H" cTabellNavn 3 "1.0" iAntSlett.
        CREATE TT_{&Tabell}.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "POS"
                            AND TT_Elogg.EndringsType = 3:
            ASSIGN TT_{&Tabell}.{&KeyFelt} = INT(TT_Elogg.Verdier).
            EXPORT TT_{&Tabell}.Sasong        
                                 TT_{&Tabell}.SasBeskr      
                                 TT_{&Tabell}.EDato         
                                 TT_{&Tabell}.ETid          
                                 TT_{&Tabell}.BrukerID      
                                 TT_{&Tabell}.RegistrertDato
                                 TT_{&Tabell}.RegistrertTid 
                                 TT_{&Tabell}.RegistrertAv.
        END.
    END.
    IF iAntNyEndre > 0 THEN DO:
        EXPORT "H" cTabellNavn 1 "1.0" iAntNyEndre.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "POS"
                            AND TT_Elogg.EndringsType = 1:
            FIND {&Tabell} WHERE {&Tabell}.{&KeyFelt} = INT(TT_Elogg.Verdier) NO-LOCK NO-ERROR.
            IF AVAIL {&Tabell} THEN
                EXPORT {&Tabell}.Sasong        
                                     {&Tabell}.SasBeskr      
                                     {&Tabell}.EDato         
                                     {&Tabell}.ETid          
                                     {&Tabell}.BrukerID      
                                     {&Tabell}.RegistrertDato
                                     {&Tabell}.RegistrertTid 
                                     {&Tabell}.RegistrertAv.
        END.
    END.
    &UNDEFINE Tabell
    &UNDEFINE KeyFelt
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendSlitSula C-Win 
PROCEDURE SendSlitSula :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    &SCOPED-DEFINE Tabell  SlitSula
    &SCOPED-DEFINE KeyFelt Slit-Id
    DEFINE VARIABLE iAntSlett   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iAntNyEndre AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cTabellNavn AS CHARACTER  INIT "{&Tabell}" NO-UNDO.
    EMPTY TEMP-TABLE TT_{&Tabell}.
    RUN KopierElogg (cTabellNavn,OUTPUT iAntSlett,OUTPUT iAntNyEndre).
    IF iAntSlett > 0 THEN DO:
        EXPORT "H" cTabellNavn 3 "1.0" iAntSlett.
        CREATE TT_{&Tabell}.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "POS"
                            AND TT_Elogg.EndringsType = 3:
            ASSIGN TT_{&Tabell}.{&KeyFelt} = INT(TT_Elogg.Verdier).
            EXPORT TT_{&Tabell}.Slit-Id       
                                 TT_{&Tabell}.SlitBeskr     
                                 TT_{&Tabell}.EDato         
                                 TT_{&Tabell}.ETid          
                                 TT_{&Tabell}.BrukerID      
                                 TT_{&Tabell}.RegistrertDato
                                 TT_{&Tabell}.RegistrertTid 
                                 TT_{&Tabell}.RegistrertAv.
        END.
    END.
    IF iAntNyEndre > 0 THEN DO:
        EXPORT "H" cTabellNavn 1 "1.0" iAntNyEndre.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "POS"
                            AND TT_Elogg.EndringsType = 1:
            FIND {&Tabell} WHERE {&Tabell}.{&KeyFelt} = INT(TT_Elogg.Verdier) NO-LOCK NO-ERROR.
            IF AVAIL {&Tabell} THEN
                EXPORT {&Tabell}.Slit-Id       
                                     {&Tabell}.SlitBeskr     
                                     {&Tabell}.EDato         
                                     {&Tabell}.ETid          
                                     {&Tabell}.BrukerID      
                                     {&Tabell}.RegistrertDato
                                     {&Tabell}.RegistrertTid 
                                     {&Tabell}.RegistrertAv.
        END.
    END.
    &UNDEFINE Tabell
    &UNDEFINE KeyFelt
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendStrKonv C-Win 
PROCEDURE SendStrKonv :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    &SCOPED-DEFINE Tabell  StrKonv
    &SCOPED-DEFINE KeyFelt StrKode
    DEFINE VARIABLE iAntSlett   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iAntNyEndre AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cTabellNavn AS CHARACTER  INIT "{&Tabell}" NO-UNDO.
    EMPTY TEMP-TABLE TT_{&Tabell}.
    
    RUN KopierElogg (cTabellNavn,OUTPUT iAntSlett,OUTPUT iAntNyEndre).
    IF iAntSlett > 0 THEN DO:
        EXPORT "H" cTabellNavn 3 "1.0" iAntSlett.
        CREATE TT_{&Tabell}.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "POS"
                            AND TT_Elogg.EndringsType = 3:
            ASSIGN TT_{&Tabell}.{&KeyFelt} = INT(TT_Elogg.Verdier).
            EXPORT TT_{&Tabell}.StrKode       
                                 TT_{&Tabell}.Storl         
                                 TT_{&Tabell}.Merknad       
                                 TT_{&Tabell}.EDato         
                                 TT_{&Tabell}.ETid          
                                 TT_{&Tabell}.BrukerID      
                                 TT_{&Tabell}.RegistrertDato
                                 TT_{&Tabell}.RegistrertTid 
                                 TT_{&Tabell}.RegistrertAv
                                 TT_{&Tabell}.SeqNr.
        END.
    END.
    
    IF iAntNyEndre > 0 THEN DO:
        EXPORT "H" cTabellNavn 1 "1.0" iAntNyEndre.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "POS"
                            AND TT_Elogg.EndringsType = 1:
            FIND {&Tabell} WHERE {&Tabell}.{&KeyFelt} = INT(TT_Elogg.Verdier) NO-LOCK NO-ERROR.
            IF AVAIL {&Tabell} THEN
                EXPORT {&Tabell}.StrKode       
                                     {&Tabell}.Storl         
                                     {&Tabell}.Merknad       
                                     {&Tabell}.EDato         
                                     {&Tabell}.ETid          
                                     {&Tabell}.BrukerID      
                                     {&Tabell}.RegistrertDato
                                     {&Tabell}.RegistrertTid 
                                     {&Tabell}.RegistrertAv
                                     {&Tabell}.SeqNr.
        END.
    END.
    &UNDEFINE Tabell
    &UNDEFINE KeyFelt
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendStrType C-Win 
PROCEDURE SendStrType :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    &SCOPED-DEFINE Tabell_1  StrType
    &SCOPED-DEFINE Tabell_2  StrTStr
    &SCOPED-DEFINE KeyFelt_1 StrTypeID
    &SCOPED-DEFINE KeyFelt_2 SoStorl
    DEFINE VARIABLE iAntSlett   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iAntNyEndre AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cTabellNavn_1 AS CHARACTER  INIT "{&Tabell_1}" NO-UNDO.
    DEFINE VARIABLE cTabellNavn_2 AS CHARACTER  INIT "{&Tabell_2}" NO-UNDO.

    DEF VAR iAntall AS INT NO-UNDO.
    DEF VAR iLoop   AS INT NO-UNDO.

    EMPTY TEMP-TABLE TT_{&Tabell_1}.
    EMPTY TEMP-TABLE TT_{&Tabell_2}.

    RUN KopierEloggStrType (cTabellNavn_1,OUTPUT iAntSlett,OUTPUT iAntNyEndre).

    IF iAntSlett > 0 THEN DO:
        EXPORT "H" cTabellNavn_1 3 "1.0" iAntSlett.
        CREATE TT_{&Tabell_1}.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn_1 AND TT_ELogg.EksterntSystem = "POS"
                            AND TT_Elogg.EndringsType = 3:
            ASSIGN TT_{&Tabell_1}.{&KeyFelt_1} = INT(TT_Elogg.Verdier).
            EXPORT TT_{&Tabell_1}.StrTypeID     
                   TT_{&Tabell_1}.Beskrivelse   
                   TT_{&Tabell_1}.Intervall     
                   TT_{&Tabell_1}.Fordeling     
                   TT_{&Tabell_1}.KortNavn      
                   TT_{&Tabell_1}.EDato         
                   TT_{&Tabell_1}.ETid          
                   TT_{&Tabell_1}.BrukerID      
                   TT_{&Tabell_1}.RegistrertDato
                   TT_{&Tabell_1}.RegistrertTid 
                   TT_{&Tabell_1}.RegistrertAv  
                   TT_{&Tabell_1}.AlfaFordeling.
        END.
    END.

    IF iAntNyEndre > 0 THEN DO:
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn_1 AND TT_ELogg.EksterntSystem = "POS"
                            AND TT_Elogg.EndringsType = 1:
            EXPORT "H" cTabellNavn_1 1 "1.0" 1.
            FIND {&Tabell_1} WHERE {&Tabell_1}.{&KeyFelt_1} = INT(TT_Elogg.Verdier) NO-LOCK NO-ERROR.
            IF AVAIL {&Tabell_1} THEN DO:
                EXPORT {&Tabell_1}.StrTypeID     
                       {&Tabell_1}.Beskrivelse   
                       {&Tabell_1}.Intervall     
                       {&Tabell_1}.Fordeling     
                       {&Tabell_1}.KortNavn      
                       {&Tabell_1}.EDato         
                       {&Tabell_1}.ETid          
                       {&Tabell_1}.BrukerID      
                       {&Tabell_1}.RegistrertDato
                       {&Tabell_1}.RegistrertTid 
                       {&Tabell_1}.RegistrertAv  
                       {&Tabell_1}.AlfaFordeling.
                ASSIGN
                    iAntall = INT(getAntStrTStr({&Tabell_1}.{&KeyFelt_1}))
                    iLoop   = 0.
                IF iAntall > 48 THEN iAntall = 48.
                EXPORT "H" cTabellNavn_2 1 "1.0" STRING(iAntall).
                LOOPEN:
                FOR EACH {&Tabell_2} OF {&Tabell_1} USE-INDEX StrTStr:
                    iLoop = iLoop + 1.
                    EXPORT {&Tabell_2}.StrTypeID     
                           {&Tabell_2}.SoStorl       
                           {&Tabell_2}.SeqNr         
                           {&Tabell_2}.EDato         
                           {&Tabell_2}.ETid          
                           {&Tabell_2}.BrukerID      
                           {&Tabell_2}.RegistrertDato
                           {&Tabell_2}.RegistrertTid 
                           {&Tabell_2}.RegistrertAv.  
                    IF iLoop >= 48 THEN LEAVE LOOPEN.
                END. /* LOOPEN */
            END.
        END.
    END.
    &UNDEFINE Tabell_1
    &UNDEFINE Tabell_2
    &UNDEFINE KeyFelt_1
    &UNDEFINE KeyFelt_2
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendValuta C-Win 
PROCEDURE SendValuta :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    &SCOPED-DEFINE Tabell  Valuta
    &SCOPED-DEFINE KeyFelt ValKod
    DEFINE VARIABLE iAntSlett   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iAntNyEndre AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cTabellNavn AS CHARACTER  INIT "{&Tabell}" NO-UNDO.
    EMPTY TEMP-TABLE TT_{&Tabell}.

    ON WRITE OF KasValuta OVERRIDE DO: END.
    RUN KopierElogg (cTabellNavn,OUTPUT iAntSlett,OUTPUT iAntNyEndre).
/*     Vi hanterar bara ändring av "ALLE" */
/*     IF iAntSlett > 0 THEN DO:                                                                         */
/*         EXPORT "H" cTabellNavn 3 "1.0" iAntSlett.                                                     */
/*         CREATE TT_{&Tabell}.                                                                          */
/*         FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "POS" */
/*                             AND TT_Elogg.EndringsType = 3:                                            */
/*             ASSIGN TT_{&Tabell}.{&KeyFelt} = TT_Elogg.Verdier.                                        */
/*             EXPORT TT_{&Tabell}.KatNr                                                                 */
/*                    TT_{&Tabell}.Beskrivelse                                                           */
/*                    TT_{&Tabell}.Merknad                                                               */
/*                    TT_{&Tabell}.EDato                                                                 */
/*                    TT_{&Tabell}.ETid                                                                  */
/*                    TT_{&Tabell}.BrukerID                                                              */
/*                    TT_{&Tabell}.RegistrertDato                                                        */
/*                    TT_{&Tabell}.RegistrertTid                                                         */
/*                    TT_{&Tabell}.RegistrertAv.                                                         */
/*         END.                                                                                          */
/*     END.                                                                                              */
    IF iAntNyEndre > 0 THEN DO:
        EXPORT "H" cTabellNavn 1 "1.0" iAntNyEndre.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "POS"
                            AND TT_Elogg.EndringsType = 1:
            FIND {&Tabell} WHERE {&Tabell}.{&KeyFelt} = TT_Elogg.Verdier NO-ERROR.
            IF NOT AVAIL {&Tabell} THEN
                NEXT.
            IF AVAIL {&Tabell} THEN
                EXPORT {&Tabell}.ValKod        
                       {&Tabell}.ValKurs       
                       {&Tabell}.ValLand       
                       {&Tabell}.ValDatum      
                       {&Tabell}.EDato         
                       {&Tabell}.ETid          
                       {&Tabell}.BrukerID      
                       {&Tabell}.RegistrertDato
                       {&Tabell}.RegistrertTid 
                       {&Tabell}.RegistrertAv  
                       {&Tabell}.ValNr         
                       {&Tabell}.ValNavn       
                       {&Tabell}.indeks        
                       {&Tabell}.retur         
                       {&Tabell}.KasseValKurs.
        END.
    END.
    &UNDEFINE Tabell
    &UNDEFINE KeyFelt
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendVaremerke C-Win 
PROCEDURE SendVaremerke :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    &SCOPED-DEFINE Tabell  Varemerke
    &SCOPED-DEFINE KeyFelt VMId
    DEFINE VARIABLE iAntSlett   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iAntNyEndre AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cTabellNavn AS CHARACTER  INIT "{&Tabell}" NO-UNDO.
    EMPTY TEMP-TABLE TT_{&Tabell}.
    RUN KopierElogg (cTabellNavn,OUTPUT iAntSlett,OUTPUT iAntNyEndre).
    IF iAntSlett > 0 THEN DO:
        EXPORT "H" cTabellNavn 3 "1.0" iAntSlett.
        CREATE TT_{&Tabell}.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "POS"
                            AND TT_Elogg.EndringsType = 3:
            ASSIGN TT_{&Tabell}.{&KeyFelt} = INT(TT_Elogg.Verdier).
            EXPORT TT_{&Tabell}.VMId          
                   TT_{&Tabell}.Beskrivelse   
                   TT_{&Tabell}.Merknad       
                   TT_{&Tabell}.RegistrertDato
                   TT_{&Tabell}.RegistrertTid 
                   TT_{&Tabell}.EDato         
                   TT_{&Tabell}.ETid          
                   TT_{&Tabell}.BrukerID      
                   TT_{&Tabell}.RegistrertAv  
                   TT_{&Tabell}.KortNavn.
        END.
    END.
    IF iAntNyEndre > 0 THEN DO:
        EXPORT "H" cTabellNavn 1 "1.0" iAntNyEndre.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "POS"
                            AND TT_Elogg.EndringsType = 1:
            FIND {&Tabell} WHERE {&Tabell}.{&KeyFelt} = INT(TT_Elogg.Verdier) NO-LOCK NO-ERROR.
            IF AVAIL {&Tabell} THEN
                EXPORT {&Tabell}.VMId          
                       {&Tabell}.Beskrivelse   
                       {&Tabell}.Merknad       
                       {&Tabell}.RegistrertDato
                       {&Tabell}.RegistrertTid 
                       {&Tabell}.EDato         
                       {&Tabell}.ETid          
                       {&Tabell}.BrukerID      
                       {&Tabell}.RegistrertAv  
                       {&Tabell}.KortNavn.
        END.
    END.
    &UNDEFINE Tabell
    &UNDEFINE KeyFelt
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendVarGr C-Win 
PROCEDURE SendVarGr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    &SCOPED-DEFINE Tabell  VarGr
    &SCOPED-DEFINE KeyFelt Vg
    DEFINE VARIABLE iAntSlett   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iAntNyEndre AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cTabellNavn AS CHARACTER  INIT "{&Tabell}" NO-UNDO.
    EMPTY TEMP-TABLE TT_{&Tabell}.
    RUN KopierElogg (cTabellNavn,OUTPUT iAntSlett,OUTPUT iAntNyEndre).
    IF iAntSlett > 0 THEN DO:
        EXPORT "H" cTabellNavn 3 "1.0" iAntSlett.
        CREATE TT_{&Tabell}.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "POS"
                            AND TT_Elogg.EndringsType = 3:
            ASSIGN TT_{&Tabell}.{&KeyFelt} = INT(TT_Elogg.Verdier).
            EXPORT TT_{&Tabell}.Vg            
                                 TT_{&Tabell}.VgBeskr       
                                 TT_{&Tabell}.StoArt        
                                 TT_{&Tabell}.MomsKod       
                                 TT_{&Tabell}.Hg            
                                 TT_{&Tabell}.Kost_Proc     
                                 TT_{&Tabell}.Kolonne       
                                 TT_{&Tabell}.EDato         
                                 TT_{&Tabell}.ETid          
                                 TT_{&Tabell}.BrukerID      
                                 TT_{&Tabell}.RegistrertDato
                                 TT_{&Tabell}.RegistrertTid 
                                 TT_{&Tabell}.RegistrertAv.
        END.
    END.
    IF iAntNyEndre > 0 THEN DO:
        EXPORT "H" cTabellNavn 1 "1.0" iAntNyEndre.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "POS"
                            AND TT_Elogg.EndringsType = 1:
            FIND {&Tabell} WHERE {&Tabell}.{&KeyFelt} = INT(TT_Elogg.Verdier) NO-LOCK NO-ERROR.
            IF AVAIL {&Tabell} THEN
                EXPORT {&Tabell}.Vg            
                                     {&Tabell}.VgBeskr       
                                     {&Tabell}.StoArt        
                                     {&Tabell}.MomsKod       
                                     {&Tabell}.Hg            
                                     {&Tabell}.Kost_Proc     
                                     {&Tabell}.Kolonne       
                                     {&Tabell}.EDato         
                                     {&Tabell}.ETid          
                                     {&Tabell}.BrukerID      
                                     {&Tabell}.RegistrertDato
                                     {&Tabell}.RegistrertTid 
                                     {&Tabell}.RegistrertAv. 
        END.
    END.
    &UNDEFINE Tabell
    &UNDEFINE KeyFelt
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendVgKat C-Win 
PROCEDURE SendVgKat :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    &SCOPED-DEFINE Tabell  VgKat
    &SCOPED-DEFINE KeyFelt Vg
    &SCOPED-DEFINE KeyFelt2 VgKat
    &SCOPED-DEFINE KeyFelt3 KatNr
    DEFINE VARIABLE iAntSlett   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iAntNyEndre AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cTabellNavn AS CHARACTER  INIT "{&Tabell}" NO-UNDO.
    EMPTY TEMP-TABLE TT_{&Tabell}.
    RUN KopierElogg (cTabellNavn,OUTPUT iAntSlett,OUTPUT iAntNyEndre).
    IF iAntSlett > 0 THEN DO:
        EXPORT "H" cTabellNavn 3 "1.0" iAntSlett.
        CREATE TT_{&Tabell}.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "POS"
                            AND TT_Elogg.EndringsType = 3:
            ASSIGN TT_{&Tabell}.{&KeyFelt}  = INT(ENTRY(1,TT_Elogg.Verdier,CHR(1)))
                   TT_{&Tabell}.{&KeyFelt2} = INT(ENTRY(2,TT_Elogg.Verdier,CHR(1)))
                   TT_{&Tabell}.{&KeyFelt3} = INT(ENTRY(3,TT_Elogg.Verdier,CHR(1))).
            EXPORT TT_{&Tabell}.Vg            
                                 TT_{&Tabell}.VgKat         
                                 TT_{&Tabell}.KatNr         
                                 TT_{&Tabell}.EDato         
                                 TT_{&Tabell}.ETid          
                                 TT_{&Tabell}.BrukerID      
                                 TT_{&Tabell}.RegistrertDato
                                 TT_{&Tabell}.RegistrertTid 
                                 TT_{&Tabell}.RegistrertAv.
        END.
    END.
    IF iAntNyEndre > 0 THEN DO:
        EXPORT "H" cTabellNavn 1 "1.0" iAntNyEndre.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "POS"
                            AND TT_Elogg.EndringsType = 1:
            FIND {&Tabell} WHERE {&Tabell}.{&KeyFelt}  = INT(ENTRY(1,TT_Elogg.Verdier,CHR(1))) AND 
                                 {&Tabell}.{&KeyFelt2} = INT(ENTRY(2,TT_Elogg.Verdier,CHR(1))) AND
                                 {&Tabell}.{&KeyFelt3} = INT(ENTRY(3,TT_Elogg.Verdier,CHR(1)))
                NO-LOCK NO-ERROR.
            IF AVAIL {&Tabell} THEN
                EXPORT {&Tabell}.Vg            
                                     {&Tabell}.VgKat         
                                     {&Tabell}.KatNr         
                                     {&Tabell}.EDato         
                                     {&Tabell}.ETid          
                                     {&Tabell}.BrukerID      
                                     {&Tabell}.RegistrertDato
                                     {&Tabell}.RegistrertTid 
                                     {&Tabell}.RegistrertAv.
        END.
    END.
    &UNDEFINE Tabell
    &UNDEFINE KeyFelt
    &UNDEFINE KeyFelt2
    &UNDEFINE KeyFelt3
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendVPIArtBas C-Win 
PROCEDURE SendVPIArtBas :
/*------------------------------------------------------------------------------
  Purpose:     VPIArtBas.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iAntSlett   AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iAntNyEndre AS INTEGER    NO-UNDO.
  
  DEF VAR cButListe AS CHAR NO-UNDO.

  
  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN
          iVPISendt = 0
          cButListe = IF FI-Valgte:SCREEN-VALUE <> "" 
                        THEN FI-Valgte:SCREEN-VALUE
                       ELSE cSentrallagerliste.
                        
      RUN vpieksport.w ("POS",
                        cButListe,
                        0, /* Bildedata skal ikke sendes med */
                        OUTPUT iAntSlett,
                        OUTPUT iAntNyEndre).
      ASSIGN iVPISendt = iAntNyEndre + iAntSlett.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapaTTELoggAlle C-Win 
PROCEDURE SkapaTTELoggAlle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  
  Notes:      iAntNyEndre har ett värde med sig 
------------------------------------------------------------------------------*/
  DEFINE INPUT-OUTPUT PARAMETER iAntNyEndre AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER cTabell AS CHARACTER  NO-UNDO.
  CASE cTabell:
      WHEN "Anv-Kod" THEN DO:
          FOR EACH Anv-Kod NO-LOCK:
              IF NOT CanFindTTElogg(cTabell,STRING(Anv-Kod.Anv-Id)) THEN DO:
                  RUN NyTTElogg(cTabell,STRING(Anv-Kod.Anv-Id)).
                  ASSIGN iAntNyEndre = iAntNyEndre + 1.
              END.
          END.
      END.
      WHEN "Avdeling" THEN DO:
          FOR EACH Avdeling NO-LOCK:
              IF NOT CanFindTTElogg(cTabell,STRING(Avdeling.AvdelingNr)) THEN DO:
                  RUN NyTTElogg(cTabell,STRING(Avdeling.AvdelingNr)).
                  ASSIGN iAntNyEndre = iAntNyEndre + 1.
              END.
          END.
      END.
      WHEN "Farg" THEN DO:
          FOR EACH Farg NO-LOCK:
              IF NOT CanFindTTElogg(cTabell,STRING(Farg.Farg)) THEN DO:
                  RUN NyTTElogg(cTabell,STRING(Farg.Farg)).
                  ASSIGN iAntNyEndre = iAntNyEndre + 1.
              END.
          END.
      END.
      WHEN "Feilkode" THEN DO:
          FOR EACH Feilkode NO-LOCK:
              IF NOT CanFindTTElogg(cTabell,STRING(Feilkode.FeilKode)) THEN DO:
                  RUN NyTTElogg(cTabell,STRING(Feilkode.Feilkode)).
                  ASSIGN iAntNyEndre = iAntNyEndre + 1.
              END.
          END.
      END.
      WHEN "Foder" THEN DO:
          FOR EACH Foder NO-LOCK:
              IF NOT CanFindTTElogg(cTabell,STRING(Foder.foder-id)) THEN DO:
                  RUN NyTTElogg(cTabell,STRING(Foder.foder-id)).
                  ASSIGN iAntNyEndre = iAntNyEndre + 1.
              END.
          END.
      END.
      WHEN "Handtering" THEN DO:
          FOR EACH Handtering NO-LOCK:
              IF NOT CanFindTTElogg(cTabell,STRING(Handtering.HandKode)) THEN DO:
                  RUN NyTTElogg(cTabell,STRING(Handtering.HandKode)).
                  ASSIGN iAntNyEndre = iAntNyEndre + 1.
              END.
          END.
      END.
      WHEN "HuvGr" THEN DO:
          FOR EACH HuvGr NO-LOCK:
              IF NOT CanFindTTElogg(cTabell,STRING(HuvGr.Hg)) THEN DO:
                  RUN NyTTElogg(cTabell,STRING(HuvGr.Hg)).
                  ASSIGN iAntNyEndre = iAntNyEndre + 1.
              END.
          END.
      END.
      WHEN "Innersula" THEN DO:
          FOR EACH Innersula NO-LOCK:
              IF NOT CanFindTTElogg(cTabell,STRING(Innersula.Inner-Id)) THEN DO:
                  RUN NyTTElogg(cTabell,STRING(Innersula.Inner-Id)).
                  ASSIGN iAntNyEndre = iAntNyEndre + 1.
              END.
          END.
      END.
      WHEN "KasValuta" THEN DO:
          FOR EACH KasValuta NO-LOCK:
              IF NOT CanFindTTElogg(cTabell,STRING(KasValuta.ValKod)) THEN DO:
                  RUN NyTTElogg(cTabell,STRING(KasValuta.ValKod)).
                  ASSIGN iAntNyEndre = iAntNyEndre + 1.
              END.
          END.
      END.
      WHEN "Kategori" THEN DO:
          FOR EACH Kategori NO-LOCK:
              IF NOT CanFindTTElogg(cTabell,STRING(Kategori.KatNr)) THEN DO:
                  RUN NyTTElogg(cTabell,STRING(Kategori.KatNr)).
                  ASSIGN iAntNyEndre = iAntNyEndre + 1.
              END.
          END.
      END.
      WHEN "Kjede" THEN DO:
          FOR EACH Kjede NO-LOCK:
              IF NOT CanFindTTElogg(cTabell,STRING(Kjede.KjedeNr)) THEN DO:
                  RUN NyTTElogg(cTabell,STRING(Kjede.KjedeNr)).
                  ASSIGN iAntNyEndre = iAntNyEndre + 1.
              END.
          END.
      END.
      WHEN "KjedeRegion" THEN DO:
          FOR EACH KjedeRegion NO-LOCK:
              IF NOT CanFindTTElogg(cTabell,STRING(KjedeRegion.KjedeNr) + "," + STRING(KjedeRegion.RegionNr)) THEN DO:
                  RUN NyTTElogg(cTabell,STRING(KjedeRegion.KjedeNr) + "," + STRING(KjedeRegion.RegionNr)).
                  ASSIGN iAntNyEndre = iAntNyEndre + 1.
              END.
          END.
      END.
      WHEN "KjedeDistrikt" THEN DO:
          FOR EACH KjedeDistrikt NO-LOCK:
              IF NOT CanFindTTElogg(cTabell,STRING(KjedeDistrikt.KjedeNr) + "," + STRING(KjedeDistrikt.RegionNr) + "," + STRING(KjedeDistrikt.DistriktNr)) THEN DO:
                  RUN NyTTElogg(cTabell,STRING(KjedeDistrikt.KjedeNr) + "," + STRING(KjedeDistrikt.RegionNr) + "," + STRING(KjedeDistrikt.DistriktNr)).
                  ASSIGN iAntNyEndre = iAntNyEndre + 1.
              END.
          END.
      END.
      WHEN "KjedensButikker" THEN DO:
          FOR EACH KjedensButikker NO-LOCK:
              IF NOT CanFindTTElogg(cTabell,STRING(KjedensButikker.KjedeNr) + "," + STRING(KjedensButikker.RegionNr) + "," + STRING(KjedensButikker.DistriktNr) + "," + STRING(KjedensButikker.ButikkNr)) THEN DO:
                  RUN NyTTElogg(cTabell,STRING(KjedensButikker.KjedeNr) + "," + STRING(KjedensButikker.RegionNr) + "," + STRING(KjedensButikker.DistriktNr) + "," + STRING(KjedensButikker.ButikkNr)).
                  ASSIGN iAntNyEndre = iAntNyEndre + 1.
              END.
          END.
      END.
      WHEN "Klack" THEN DO:
          FOR EACH Klack NO-LOCK:
              IF NOT CanFindTTElogg(cTabell,STRING(Klack.klack-id)) THEN DO:
                  RUN NyTTElogg(cTabell,STRING(Klack.klack-id)).
                  ASSIGN iAntNyEndre = iAntNyEndre + 1.
              END.
          END.
      END.
      WHEN "Kravkode" THEN DO:
          FOR EACH Kravkode NO-LOCK:
              IF NOT CanFindTTElogg(cTabell,STRING(Kravkode.KravKode)) THEN DO:
                  RUN NyTTElogg(cTabell,STRING(Kravkode.KravKode)).
                  ASSIGN iAntNyEndre = iAntNyEndre + 1.
              END.
          END.
      END.
      WHEN "Last-Sko" THEN DO:
          FOR EACH Last-Sko NO-LOCK:
              IF NOT CanFindTTElogg(cTabell,STRING(Last-Sko.Last-Id)) THEN DO:
                  RUN NyTTElogg(cTabell,STRING(Last-Sko.Last-Id)).
                  ASSIGN iAntNyEndre = iAntNyEndre + 1.
              END.
          END.
      END.
      WHEN "LevBas" THEN DO:
          FOR EACH LevBas NO-LOCK:
              IF NOT CanFindTTElogg(cTabell,STRING(LevBas.LevNr)) THEN DO:
                  RUN NyTTElogg(cTabell,STRING(LevBas.LevNr)).
                  ASSIGN iAntNyEndre = iAntNyEndre + 1.
              END.
          END.
      END.
      WHEN "DefaultLevDato" THEN DO:
          FOR EACH DefaultLevDato NO-LOCK:
              IF NOT CanFindTTElogg(cTabell,STRING(DefaultLevDato.ButikkNr) + ',' + STRING(DefaultLevDato.LevNr)) THEN DO:
                  RUN NyTTElogg(cTabell,STRING(DefaultLevDato.ButikkNr) + ',' + STRING(DefaultLevDato.LevNr)).
                  ASSIGN iAntNyEndre = iAntNyEndre + 1.
              END.
          END.
      END.
      WHEN "LevSort" THEN DO:
          FOR EACH LevSort NO-LOCK:
              IF NOT CanFindTTElogg(cTabell,STRING(LevSort.LevNr) + "," + LevSort.SortID) THEN DO:
                  RUN NyTTElogg(cTabell,STRING(LevSort.LevNr) + "," + LevSort.SortID).
                  ASSIGN iAntNyEndre = iAntNyEndre + 1.
              END.
          END.
      END.
      WHEN "Material" THEN DO:
          FOR EACH Material NO-LOCK:
              IF NOT CanFindTTElogg(cTabell,STRING(Material.MatKod)) THEN DO:
                  RUN NyTTElogg(cTabell,STRING(Material.MatKod)).
                  ASSIGN iAntNyEndre = iAntNyEndre + 1.
              END.
          END.
      END.
      WHEN "Moms" THEN DO:
          FOR EACH Moms NO-LOCK:
              IF NOT CanFindTTElogg(cTabell,STRING(Moms.MomsKod)) THEN DO:
                  RUN NyTTElogg(cTabell,STRING(Moms.MomsKod)).
                  ASSIGN iAntNyEndre = iAntNyEndre + 1.
              END.
          END.
      END.
      WHEN "Ovandel" THEN DO:
          FOR EACH Ovandel NO-LOCK:
              IF NOT CanFindTTElogg(cTabell,STRING(Ovandel.Ov-Id)) THEN DO:
                  RUN NyTTElogg(cTabell,STRING(Ovandel.Ov-Id)).
                  ASSIGN iAntNyEndre = iAntNyEndre + 1.
              END.
          END.
      END.
      WHEN "Post" THEN DO:
          FOR EACH Post NO-LOCK:
              IF NOT CanFindTTElogg(cTabell,STRING(Post.PostNr)) THEN DO:
                  RUN NyTTElogg(cTabell,STRING(Post.PostNr)).
                  ASSIGN iAntNyEndre = iAntNyEndre + 1.
              END.
          END.
      END.
      WHEN "Prisgruppe" THEN DO:
          FOR EACH Prisgruppe NO-LOCK:
              IF NOT CanFindTTElogg(cTabell,STRING(Prisgruppe.PrisGrpNr)) THEN DO:
                  RUN NyTTElogg(cTabell,STRING(Prisgruppe.PrisGrpNr)).
                  ASSIGN iAntNyEndre = iAntNyEndre + 1.
              END.
          END.
      END.
      WHEN "Prisprofil" THEN DO:
          FOR EACH Prisprofil NO-LOCK:
              IF NOT CanFindTTElogg(cTabell,STRING(Prisprofil.ProfilNr)) THEN DO:
                  RUN NyTTElogg(cTabell,STRING(Prisprofil.ProfilNr)).
                  ASSIGN iAntNyEndre = iAntNyEndre + 1.
              END.
          END.
      END.
      WHEN "Produsent" THEN DO:
          FOR EACH Produsent NO-LOCK:
              IF NOT CanFindTTElogg(cTabell,STRING(Produsent.ProdNr)) THEN DO:
                  RUN NyTTElogg(cTabell,STRING(Produsent.ProdNr)).
                  ASSIGN iAntNyEndre = iAntNyEndre + 1.
              END.
          END.
      END.
      WHEN "Prov" THEN DO:
          FOR EACH Prov NO-LOCK:
              IF NOT CanFindTTElogg(cTabell,STRING(Prov.ProvKod)) THEN DO:
                  RUN NyTTElogg(cTabell,STRING(Prov.ProvKod)).
                  ASSIGN iAntNyEndre = iAntNyEndre + 1.
              END.
          END.
      END.
      WHEN "Rabatt" THEN DO:
          FOR EACH Rabatt NO-LOCK:
              IF NOT CanFindTTElogg(cTabell,STRING(Rabatt.RabKod)) THEN DO:
                  RUN NyTTElogg(cTabell,STRING(Rabatt.RabKod)).
                  ASSIGN iAntNyEndre = iAntNyEndre + 1.
              END.
          END.
      END.
      WHEN "Sasong" THEN DO:
          FOR EACH Sasong NO-LOCK:
              IF NOT CanFindTTElogg(cTabell,STRING(Sasong.Sasong)) THEN DO:
                  RUN NyTTElogg(cTabell,STRING(Sasong.Sasong)).
                  ASSIGN iAntNyEndre = iAntNyEndre + 1.
              END.
          END.
      END.
      WHEN "Slitsula" THEN DO:
          FOR EACH Slitsula NO-LOCK:
              IF NOT CanFindTTElogg(cTabell,STRING(Slitsula.Slit-Id)) THEN DO:
                  RUN NyTTElogg(cTabell,STRING(Slitsula.Slit-Id)).
                  ASSIGN iAntNyEndre = iAntNyEndre + 1.
              END.
          END.
      END.
      WHEN "StrKonv" THEN DO:
          FOR EACH StrKonv NO-LOCK:
              IF NOT CanFindTTElogg(cTabell,STRING(StrKonv.StrKode)) THEN DO:
                  RUN NyTTElogg(cTabell,STRING(StrKonv.StrKode)).
                  ASSIGN iAntNyEndre = iAntNyEndre + 1.
              END.
          END.
      END.
      WHEN "StrType" THEN DO:
          FOR EACH StrType NO-LOCK WHERE StrType.StrTypeID < 1000:
              IF NOT CanFindTTElogg(cTabell,STRING(StrType.StrTypeID)) THEN DO:
                  RUN NyTTElogg(cTabell,STRING(StrType.StrTypeID)).
                  ASSIGN iAntNyEndre = iAntNyEndre + 1.
              END.
          END.
      END.
      WHEN "Valuta" THEN DO:
          FOR EACH Valuta NO-LOCK:
              IF NOT CanFindTTElogg(cTabell,STRING(Valuta.ValKod)) THEN DO:
                  RUN NyTTElogg(cTabell,STRING(Valuta.ValKod)).
                  ASSIGN iAntNyEndre = iAntNyEndre + 1.
              END.
          END.
      END.
      WHEN "Varemerke" THEN DO:
          FOR EACH Varemerke NO-LOCK:
              IF NOT CanFindTTElogg(cTabell,STRING(Varemerke.VMId)) THEN DO:
                  RUN NyTTElogg(cTabell,STRING(Varemerke.VMId)).
                  ASSIGN iAntNyEndre = iAntNyEndre + 1.
              END.
          END.
      END.
      WHEN "VarGr" THEN DO:
          FOR EACH VarGr NO-LOCK:
              IF NOT CanFindTTElogg(cTabell,STRING(VarGr.Vg)) THEN DO:
                  RUN NyTTElogg(cTabell,STRING(VarGr.Vg)).
                  ASSIGN iAntNyEndre = iAntNyEndre + 1.
              END.
          END.
      END.
      WHEN "VgKat" THEN DO:
          FOR EACH VgKat NO-LOCK:
              IF NOT CanFindTTElogg(cTabell,STRING(VgKat.Vg) + CHR(1) + STRING(VgKat.VgKat) + CHR(1) + STRING(VgKat.KatNr)) THEN DO:
                  RUN NyTTElogg(cTabell,STRING(VgKat.Vg) + CHR(1) + STRING(VgKat.VgKat) + CHR(1) + STRING(VgKat.KatNr)).
                  ASSIGN iAntNyEndre = iAntNyEndre + 1.
              END.
          END.
      END.
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettBehandlet C-Win 
PROCEDURE SlettBehandlet :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH Elogg WHERE Elogg.Behandlet = TRUE:
        DELETE Elogg.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartEksport C-Win 
PROCEDURE StartEksport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iCount             AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cFilnavn           AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE c2Filnavn          AS CHAR       NO-UNDO.
  DEFINE VARIABLE cNumericFormat     AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cDateFormat        AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cButListe          AS CHARACTER  NO-UNDO.

  ASSIGN cNumericFormat         = SESSION:NUMERIC-FORMAT
         cDateFormat            = SESSION:DATE-FORMAT
         SESSION:NUMERIC-FORMAT = "EUROPEAN"
         SESSION:DATE-FORMAT    = "dmy".

  DO WITH FRAME {&FRAME-NAME}:
    IF cSentrallagerliste = "" THEN DO:
        MESSAGE "Ingen butikk registrert som sentrallager."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.
    ASSIGN FI-Filnavn:SCREEN-VALUE = "HK" + STRING(YEAR(TODAY)) + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") + "_" + REPLACE(STRING(TIME,"HH:MM:SS"),":","")
           cFilnavn = FI-Eksportdir + "\" + FI-Filnavn:SCREEN-VALUE.
    EMPTY TEMP-TABLE TT_Elogg.
    OUTPUT TO VALUE(cFilnavn).
    IF TG-SendAnv-Kod:CHECKED THEN DO:
        RUN SendAnv-Kod.
    END.
    IF TG-SendAvdeling:CHECKED THEN DO:
        RUN SendAvdeling.
    END.
/*     IF TG-SendBehandlingskode:CHECKED THEN DO: */
/*         RUN SendBehandlingskode.               */
/*     END.                                       */
    IF TG-SendFarg:CHECKED THEN DO:
        RUN SendFarg.
    END.
    IF TG-SendFeilkode:CHECKED THEN DO:
        RUN SendFeilkode.
    END.
    IF TG-SendFoder:CHECKED THEN DO:
        RUN SendFoder.
    END.
    IF TG-SendHandtering:CHECKED THEN DO:
        RUN SendHandtering.
    END.
    IF TG-SendHuvGr:CHECKED THEN DO:
        RUN SendHuvGr.
    END.
    IF TG-SendInnersula:CHECKED THEN DO:
        RUN SendInnersula.
    END.
    IF TG-SendKasValuta:CHECKED THEN DO:
        RUN SendKasValuta.
    END.
    IF TG-SendKategori:CHECKED THEN DO:
        RUN SendKategori.
    END.
    IF TG-SendKjede:CHECKED THEN DO:
        RUN SendKjede.
    END.
    IF TG-SendKjedeRegion:CHECKED THEN DO:
        RUN SendKjedeRegion.
    END.
    IF TG-SendKjedeDistrikt:CHECKED THEN DO:
        RUN SendKjedeDistrikt.
    END.
    IF TG-SendKjedensButikker:CHECKED THEN DO:
        RUN SendKjedensButikker.
    END.
    IF TG-SendKlack:CHECKED THEN DO:
        RUN SendKlack.
    END.
    IF TG-SendKravkode:CHECKED THEN DO:
        RUN SendKravkode.
    END.
    IF TG-SendLast-Sko:CHECKED THEN DO:
        RUN SendLast-Sko.
    END.
    IF TG-SendLevBas:CHECKED THEN DO:
        RUN SendLevBas.
    END.
    IF TG-SendDefaultLevDato:CHECKED THEN DO:
        RUN SendDefaultLevDato.
    END.
    IF TG-SendLevSort:CHECKED THEN DO:
        RUN SendLevSort.
    END.
    IF TG-SendMaterial:CHECKED THEN DO:
        RUN SendMaterial.
    END.
    IF TG-SendMoms:CHECKED THEN DO:
        RUN SendMoms.
    END.
    IF TG-SendOvandel:CHECKED THEN DO:
        RUN SendOvandel.
    END.
    IF TG-SendPost:CHECKED THEN DO:
        RUN SendPost.
    END.
    IF TG-SendPrisgruppe:CHECKED THEN DO:
        RUN SendPrisgruppe.
    END.
    IF TG-SendPrisprofil:CHECKED THEN DO:
        RUN SendPrisprofil.
    END.
    IF TG-SendProdusent:CHECKED THEN DO:
        RUN SendProdusent.
    END.
    IF TG-SendProv:CHECKED THEN DO:
        RUN SendProv.
    END.
    IF TG-SendRabatt:CHECKED THEN DO:
        RUN SendRabatt.
    END.
    IF TG-SendSaSong:CHECKED THEN DO:
        RUN SendSaSong.
    END.
    IF TG-SendSlitSula:CHECKED THEN DO:
        RUN SendSlitSula.
    END.
    IF TG-SendStrKonv:CHECKED THEN DO:
        RUN SendStrKonv.
    END.
    IF TG-SendStrType:CHECKED THEN DO:
        RUN SendStrType.
    END.
    IF TG-SendValuta:CHECKED THEN DO:
        RUN SendValuta.
    END.
    IF TG-SendVaremerke:CHECKED THEN DO:
        RUN SendVaremerke.
    END.
    IF TG-SendVarGr:CHECKED THEN DO:
        RUN SendVarGr.
    END.
    IF TG-SendVgKat:CHECKED THEN DO:
        RUN SendVgKat.
    END.
    IF TG-SendVPI:CHECKED THEN DO:
        RUN SendVPIArtBas.
    END.
    OUTPUT CLOSE.
  END.
  ASSIGN SESSION:NUMERIC-FORMAT = cNumericFormat
         SESSION:DATE-FORMAT    = cDateFormat.
  FILE-INFO:FILE-NAME = cFilnavn.
  IF NOT FILE-INFO:FILE-SIZE > 0 THEN DO:
      IF iVPISendt = 0 THEN
      MESSAGE "Ingen data for eksport"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
  END.
  ELSE DO: 
      ASSIGN 
          c2Filnavn = REPLACE(cFilnavn,"HK","HKVPI")
          cButListe = IF FI-Valgte:SCREEN-VALUE <> "" THEN FI-Valgte:SCREEN-VALUE ELSE
                           cSentrallagerliste.
      DO iCount = 1 TO NUM-ENTRIES(cButListe):
          OS-COPY VALUE(cFilnavn) VALUE(c2Filnavn + "." + ENTRY(iCount,cButListe)).
      END.
  END.
  OS-DELETE VALUE(cFilnavn).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TG_Kontroll C-Win 
PROCEDURE TG_Kontroll :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER cTabell       AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER hTabellToggle AS HANDLE     NO-UNDO.
  DEFINE INPUT  PARAMETER hSendToggle   AS HANDLE     NO-UNDO.
  DEFINE INPUT  PARAMETER hAlleToggle   AS HANDLE     NO-UNDO.
      ASSIGN hTabellToggle:CHECKED       = FindFirst(cTabell,FALSE)
             hSendToggle:SENSITIVE = hTabellToggle:CHECKED
             hSendToggle:CHECKED   = IF hSendToggle:SENSITIVE THEN 
                            TG-AlleOnOff:CHECKED IN FRAME {&FRAME-NAME} ELSE FALSE.
      IF VALID-HANDLE(hAlleToggle) THEN
             ASSIGN hAlleToggle:CHECKED   = hTabellToggle:CHECKED AND FindFirst(cTabell,TRUE).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CanFindTTElogg C-Win 
FUNCTION CanFindTTElogg RETURNS LOGICAL
  ( INPUT cTabell AS CHARACTER,INPUT cVerdi AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN CAN-FIND(FIRST TT_ELogg WHERE TT_ELogg.TabellNavn = cTabell AND
                                    TT_ELogg.EksterntSystem = "POS" AND
                                    TT_ELogg.Verdier = cVerdi).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FindFirst C-Win 
FUNCTION FindFirst RETURNS LOGICAL
  ( INPUT cTabellNavn AS CHARACTER, INPUT lAlle AS LOGICAL ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
   RETURN IF lAlle = TRUE THEN CAN-FIND(FIRST ELogg WHERE ELogg.TabellNavn = cTabellNavn AND
                                    ELogg.EksterntSystem = "POS" AND
                                    ELogg.Verdier = "ALLE") ELSE
             CAN-FIND(FIRST ELogg WHERE ELogg.TabellNavn = cTabellNavn AND
                                    ELogg.EksterntSystem = "POS").
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getAntLevSAnt C-Win 
FUNCTION getAntLevSAnt RETURNS INTEGER
  ( INPUT iLevNr AS INTEGER, INPUT cSortID AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iAnt AS INTEGER    NO-UNDO.
  FOR EACH LevSAnt WHERE LevSAnt.LevNr = iLevNr AND LevSAnt.SortID = cSortID NO-LOCK.
      ASSIGN iAnt = iAnt + 1.
  END.
  RETURN iAnt.   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getAntStrTStr C-Win 
FUNCTION getAntStrTStr RETURNS INTEGER
  ( INPUT iStrTypeID AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iAnt AS INTEGER    NO-UNDO.
  FOR EACH StrTStr WHERE StrTStr.StrTypeID = iStrTypeID NO-LOCK.
      ASSIGN iAnt = iAnt + 1.
  END.
  RETURN iAnt.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SendChecked C-Win 
FUNCTION SendChecked RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose: 
    Notes: 
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
    IF TG-SendAvdeling:CHECKED THEN
          RETURN TRUE. 
    ELSE IF TG-SendAnv-Kod:CHECKED THEN
        RETURN TRUE. 
/*     ELSE IF TG-SendBehandlingskode:CHECKED THEN */
/*         RETURN TRUE.                            */
    ELSE IF TG-SendFarg:CHECKED THEN
        RETURN TRUE.
    ELSE IF TG-SendFeilkode:CHECKED THEN
        RETURN TRUE. 
    ELSE IF TG-SendFoder:CHECKED THEN
        RETURN TRUE. 
    ELSE IF TG-SendHandtering:CHECKED THEN
        RETURN TRUE. 
    ELSE IF TG-SendHuvGr:CHECKED THEN
        RETURN TRUE. 
    ELSE IF TG-SendInnersula:CHECKED THEN
        RETURN TRUE. 
    ELSE IF TG-SendKasValuta:CHECKED THEN
        RETURN TRUE. 
    ELSE IF TG-SendKategori:CHECKED THEN
        RETURN TRUE. 
    ELSE IF TG-SendKjede:CHECKED THEN
        RETURN TRUE. 
    ELSE IF TG-SendKjedeDistrikt:CHECKED THEN
        RETURN TRUE. 
    ELSE IF TG-SendKjedensButikker:CHECKED THEN
        RETURN TRUE. 
    ELSE IF TG-SendKjedeRegion:CHECKED THEN
        RETURN TRUE. 
    ELSE IF TG-SendKlack:CHECKED THEN
        RETURN TRUE. 
    ELSE IF TG-SendKravkode:CHECKED THEN
        RETURN TRUE. 
    ELSE IF TG-SendLast-Sko:CHECKED THEN
        RETURN TRUE. 
    ELSE IF TG-SendLevBas:CHECKED THEN
        RETURN TRUE. 
    ELSE IF TG-SendDefaultLevDato:CHECKED THEN
        RETURN TRUE. 
    ELSE IF TG-SendLevSort:CHECKED THEN
        RETURN TRUE. 
    ELSE IF TG-SendMaterial:CHECKED THEN
        RETURN TRUE. 
    ELSE IF TG-SendMoms:CHECKED THEN
        RETURN TRUE. 
    ELSE IF TG-SendOvandel:CHECKED THEN
        RETURN TRUE. 
    ELSE IF TG-SendPost:CHECKED THEN
        RETURN TRUE. 
    ELSE IF TG-SendPrisgruppe:CHECKED THEN
        RETURN TRUE. 
    ELSE IF TG-SendPrisprofil:CHECKED THEN
        RETURN TRUE. 
    ELSE IF TG-SendProdusent:CHECKED THEN
        RETURN TRUE. 
    ELSE IF TG-SendProv:CHECKED THEN
        RETURN TRUE. 
    ELSE IF TG-SendRabatt:CHECKED THEN
        RETURN TRUE. 
    ELSE IF TG-SendSaSong:CHECKED THEN
        RETURN TRUE. 
    ELSE IF TG-SendSlitSula:CHECKED THEN
        RETURN TRUE. 
    ELSE IF TG-SendStrKonv:CHECKED THEN
        RETURN TRUE. 
    ELSE IF TG-SendStrType:CHECKED THEN
        RETURN TRUE. 
    ELSE IF TG-SendValuta:CHECKED THEN
        RETURN TRUE. 
    ELSE IF TG-SendVaremerke:CHECKED THEN
        RETURN TRUE. 
    ELSE IF TG-SendVarGr:CHECKED THEN
        RETURN TRUE.
    ELSE IF TG-SendVgKat:CHECKED THEN
        RETURN TRUE.
    ELSE IF TG-SendVPI:CHECKED THEN
        RETURN TRUE.
    ELSE
        RETURN FALSE.
  END.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

