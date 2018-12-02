&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
/* Procedure Description
"Basic Window Template

Use this template to create a new window. Alter this default template or create new ones to accomodate your needs for different default sizes and/or attributes."
*/
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*********************************************************************
* Copyright (C) 2001 by Progress Software Corporation ("PSC"),       *
* 14 Oak Park, Bedford, MA 01730, and other contributors as listed   *
* below.  All Rights Reserved.                                       *
*                                                                    *
* The Initial Developer of the Original Code is PSC.  The Original   *
* Code is Progress IDE code released to open source December 1, 2000.*
*                                                                    *
* The contents of this file are subject to the Possenet Public       *
* License Version 1.0 (the "License"); you may not use this file     *
* except in compliance with the License.  A copy of the License is   *
* available as of the date of this notice at                         *
* http://www.possenet.org/license.html                               *
*                                                                    *
* Software distributed under the License is distributed on an "AS IS"*
* basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. You*
* should refer to the License for the specific language governing    *
* rights and limitations under the License.                          *
*                                                                    *
* Contributors:                                                      *
*                                                                    *
*********************************************************************/
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
{incl/Excel_1.3.i}

DEF VAR bOK                    AS LOG    NO-UNDO.
DEF VAR ix                     AS INT    NO-UNDO.
DEF VAR iy                     AS INT    NO-UNDO.
DEF VAR iReturn                AS INT    NO-UNDO.
DEF VAR bHKinst                AS LOG    NO-UNDO.
DEF VAR bCloseQuery            AS LOG    NO-UNDO.
DEF VAR hPrintBtn              AS HANDLE NO-UNDO.
DEF VAR iColor                 AS INT    NO-UNDO.
DEF VAR iColNum                AS INT    NO-UNDO.
                              
DEF VAR hToolbar               AS HANDLE NO-UNDO.
DEF VAR hUpdToolbar            AS HANDLE NO-UNDO.
DEF VAR hBrowseListe           AS HANDLE NO-UNDO.
DEF VAR hBrowseLinje           AS HANDLE NO-UNDO.
DEF VAR hBrowseSort            AS HANDLE NO-UNDO.
DEF VAR hFieldMap              AS HANDLE NO-UNDO.
DEF VAR hFieldMapLinje         AS HANDLE NO-UNDO.
DEF VAR hSearchListe           AS HANDLE NO-UNDO.
DEF VAR hSearchLinje           AS HANDLE NO-UNDO.
DEF VAR hWindow                AS HANDLE NO-UNDO.
DEF VAR hArtBilde              AS HANDLE NO-UNDO.
DEF VAR hArtBildeFrame         AS HANDLE NO-UNDO.

DEF VAR hBrwOBeskr             AS HANDLE NO-UNDO.
DEF VAR hBrwOInnkjopsPris      AS HANDLE NO-UNDO.
DEF VAR hBrwOLevFargKod        AS HANDLE NO-UNDO.
DEF VAR hBrwOAnbefaltPris      AS HANDLE NO-UNDO.
                               
DEF VAR hBrwOSekv              AS HANDLE NO-UNDO.
DEF VAR hBrwOLMerk             AS HANDLE NO-UNDO.
DEF VAR hBrwOVarekost          AS HANDLE NO-UNDO.
DEF VAR hBrwOSupVarekost       AS HANDLE NO-UNDO.
DEF VAR hBrwOforhRab%          AS HANDLE NO-UNDO.
DEF VAR hBrwOsupRab%           AS HANDLE NO-UNDO.
DEF VAR hBrwOPris              AS HANDLE NO-UNDO.
DEF VAR hBrwOKampanjePris      AS HANDLE NO-UNDO.
DEF VAR hBrwOAntall            AS HANDLE NO-UNDO.
DEF VAR hBrwOSupAntall         AS HANDLE NO-UNDO.
DEF VAR hBrwOKjedeRab%         AS HANDLE NO-UNDO.
DEF VAR hBrwOKjedeInnkPris     AS HANDLE NO-UNDO.
DEF VAR hBrwOKjedeSupRab%      AS HANDLE NO-UNDO.
DEF VAR hBrwOKjedeSupInnkPris  AS HANDLE NO-UNDO.
DEF VAR hBrwOLevUke1           AS HANDLE NO-UNDO.
DEF VAR hBrwOLevUke2           AS HANDLE NO-UNDO.
DEF VAR hBrwOLevUke3           AS HANDLE NO-UNDO.
DEF VAR hBrwOLevUke4           AS HANDLE NO-UNDO.
DEF VAR hBrwOFrittTillegg      AS HANDLE NO-UNDO.
DEF VAR hBrwOMinAntKjop        AS HANDLE NO-UNDO.
DEF VAR hBrwOKjedeVare         AS HANDLE NO-UNDO.
DEF VAR hBrwOGjFakt            AS HANDLE NO-UNDO.
DEF VAR hColumn                AS HANDLE NO-UNDO.
DEF VAR hNavVarebokToolBar     AS HANDLE NO-UNDO.
DEF VAR hBrwOSortiment         AS HANDLE NO-UNDO.
DEF VAR hBrwOKampanjeUke       AS HANDLE NO-UNDO.
DEF VAR hBrwOKampanjeStotte    AS HANDLE NO-UNDO.
DEF VAR hBrwOLagerKode         AS HANDLE NO-UNDO.
DEF VAR hBrwOSaSong            AS HANDLE NO-UNDO.

DEF VAR cCurrSelectBuffer      AS CHAR   NO-UNDO.
DEF VAR iSelectorSourcCount    AS INT    NO-UNDO.
DEF VAR cHuvGrAvdelingList     AS CHAR   NO-UNDO.
DEF VAR cVarGrHuvGrList        AS CHAR   NO-UNDO.
DEF VAR cLevBasRowIdList       AS CHAR   NO-UNDO.
DEF VAR cLevBasIdList          AS CHAR   NO-UNDO.
DEF VAR cAvdelingRowIdList     AS CHAR   NO-UNDO.
DEF VAR cAvdelingIdList        AS CHAR   NO-UNDO.
DEF VAR cHuvGrRowIdList        AS CHAR   NO-UNDO.
DEF VAR cHuvGrIdList           AS CHAR   NO-UNDO.
DEF VAR cVarGrRowIdList        AS CHAR   NO-UNDO.
DEF VAR cVarGrIdList           AS CHAR   NO-UNDO.

DEF VAR cAdgang                AS CHAR   NO-UNDO.
DEF VAR hTLdet                 AS HANDLE NO-UNDO.
DEF VAR hVisBilde              AS HANDLE NO-UNDO.
DEF VAR hArtikkelkort          AS HANDLE NO-UNDO.
DEF VAR hBufArtStr             AS HANDLE NO-UNDO.
                              
DEF VAR iTab                   AS INT    NO-UNDO.
                              
DEF VAR cState                 AS CHAR   NO-UNDO.
DEF VAR cVareBokRowIdList      AS CHAR   NO-UNDO.
DEF VAR cVareBokIdList         AS CHAR   NO-UNDO.
                              
DEF VAR hUtvalg                AS HANDLE NO-UNDO.
DEF VAR bUtvalgIsMaster        AS LOG    NO-UNDO.
DEF VAR cBtnUtvalgHandles      AS CHAR   NO-UNDO.
DEF VAR cBtnHentUtvalgHandles  AS CHAR   NO-UNDO.
DEF VAR fArtikkelNr            AS DEC    NO-UNDO.
DEF VAR rRowIdLinje            AS ROWID  NO-UNDO.
DEF VAR cVarebokLinjeJoin      AS CHAR   NO-UNDO.
DEF VAR cAktivitetJoin         AS CHAR   NO-UNDO.
DEF VAR hRowsToBatchMenu       AS HANDLE NO-UNDO.

DEF VAR cStatLabelHandles      AS CHAR   NO-UNDO.
DEF VAR cStatLabelNames        AS CHAR   NO-UNDO.
DEF VAR cStatFieldHandles      AS CHAR   NO-UNDO.
DEF VAR cStatFieldNames        AS CHAR   NO-UNDO.
DEF VAR cStatFieldPrefixList   AS CHAR   NO-UNDO.
DEF VAR cPrefixedStatFields    AS CHAR   NO-UNDO.

DEF VAR cOppmerking            AS CHAR   NO-UNDO.
DEF VAR cPrintMark             AS CHAR   NO-UNDO.
DEF VAR hBrwColArtBeskr        AS HANDLE NO-UNDO. 
DEF VAR hFieldRGBcolor         AS HANDLE NO-UNDO.
DEF VAR hBrwColKjedeVare       AS HANDLE NO-UNDO.
DEF VAR hBrwColGjFakt          AS HANDLE NO-UNDO.

DEF VAR hMenuItemNewSort       AS HANDLE NO-UNDO.
DEF VAR hMenuItemSelectSort    AS HANDLE NO-UNDO.
DEF VAR hSortPanel             AS HANDLE NO-UNDO.
DEF VAR cSortDescr             AS CHAR   NO-UNDO.
DEF VAR hSpesialSortMnu        AS HANDLE NO-UNDO.
DEF VAR cSpesialSortFields     AS CHAR   NO-UNDO
    INIT "Sekv,VPIbildeKode,LevNamn,Beskr,LevKod,LevFargKod,InnkjopsPris,VareKost,forhRab%,forhKalkyle,supVareKost,supRab%,supKalkyle,Pris,AnbefaltPris,KampanjePris,LinjeMerknad,Db%,DBKr,supDb%,supDBKr,Vg,VgBeskr,Hg,HgBeskr,AvdelingNr,AvdelingNavn,LevNr,ProdNr,ProdusentBeskrivelse,KjedeVare,Gjennomfaktureres,SalgsEnhet,FrittTillegg,AntIPakn,Beskrivelse,SasBeskr".

DEF VAR iFontWingdings         AS INT    NO-UNDO.

DEFINE STREAM Stream1.

DEF TEMP-TABLE ttBildeData
    FIELD BildNr    AS INT
    FIELD Teller    AS INT
    FIELD RawData   AS RAW
    FIELD RowIdent  AS CHAR
    .
DEF VAR hBufBildeData AS HANDLE.
hBufBildeData = BUFFER ttBildeData:HANDLE.

DEF TEMP-TABLE ttVarebokLinje
    FIELD VarebokNr    AS DEC
    FIELD ArtikkelNr   AS DEC.
DEF VAR httVarebokLinje AS HANDLE NO-UNDO.
httVarebokLinje = BUFFER ttVarebokLinje:HANDLE:TABLE-HANDLE.

DEF TEMP-TABLE ttEANrows
    FIELD iRow AS INT.

DEF TEMP-TABLE ttWeekRows
    FIELD iRow AS INT.

DEF TEMP-TABLE ttRapport NO-UNDO
    FIELD cRpLinje AS CHAR.
DEF VAR httRapport AS HANDLE NO-UNDO.
httRapport = BUFFER ttRapport:HANDLE:TABLE-HANDLE.

DEF VAR hOppdaterViaExcel AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rectToolBar rectUpdToolbar rectVarebok ~
VareBokNr VareBokBeskrivelse BeskrivelseVarebokType MesseNr ~
MesseBeskrivelse KortNavn 
&Scoped-Define DISPLAYED-OBJECTS VareBokNr VareBokBeskrivelse VareBokType ~
BeskrivelseVarebokType MesseNr MesseBeskrivelse ProfilNr KortNavn 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AdjustBrowseColumns C-Win 
FUNCTION AdjustBrowseColumns RETURNS LOGICAL
  ( INPUT ihBrowse     AS HANDLE,
    INPUT icBrowseName AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ArtPrintToExcel C-Win 
FUNCTION ArtPrintToExcel RETURNS LOGICAL
  ( INPUT icFileAndCount AS CHAR,
    INPUT ibTestPrint    AS LOG)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD EAN13BC C-Win 
FUNCTION EAN13BC RETURNS CHARACTER
  ( INPUT icStrekKode AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD EanPrintToExcel C-Win 
FUNCTION EanPrintToExcel RETURNS LOGICAL
  ( INPUT icFileAndCount AS CHAR,
    INPUT ibTestPrint    AS LOG)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD EndreMangeLinjer C-Win 
FUNCTION EndreMangeLinjer RETURNS LOGICAL
  ( INPUT icField AS CHAR,
    INPUT icValue AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ExcelAuto C-Win 
FUNCTION ExcelAuto RETURNS LOGICAL
  ( INPUT ihBuffer AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getReportFile C-Win 
FUNCTION getReportFile RETURNS CHARACTER
  ( INPUT icClientServerFile AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getVareBokNr C-Win 
FUNCTION getVareBokNr RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getWeekNum C-Win 
FUNCTION getWeekNum RETURNS CHARACTER
  ( INPUT dSomeDate AS DATE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD InitOverlays C-Win 
FUNCTION InitOverlays RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD InitUtvalgToVarebok C-Win 
FUNCTION InitUtvalgToVarebok RETURNS LOGICAL
  ( INPUT ihUtvalg         AS HANDLE,
    INPUT ibUtvalgIsMaster AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LevPrintToExcel C-Win 
FUNCTION LevPrintToExcel RETURNS LOGICAL
  ( INPUT icFileAndCount AS CHAR,
    INPUT ibTestPrint    AS LOG)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LinkOverlays C-Win 
FUNCTION LinkOverlays RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LoadRowsToBatchSettings C-Win 
FUNCTION LoadRowsToBatchSettings RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SaveSortString C-Win 
FUNCTION SaveSortString RETURNS LOGICAL
  ( INPUT icName        AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setFieldColor C-Win 
FUNCTION setFieldColor RETURNS INTEGER
  ( INPUT iRGBcolor AS INT)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setSpesialSortMenu C-Win 
FUNCTION setSpesialSortMenu RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SetTabStrip C-Win 
FUNCTION SetTabStrip RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SettHentUtvalgSensitive C-Win 
FUNCTION SettHentUtvalgSensitive RETURNS LOGICAL
  ( INPUT ibSensitive AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SettUtvalgSensitive C-Win 
FUNCTION SettUtvalgSensitive RETURNS LOGICAL
  ( INPUT ibSensitive AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD StartSpesialSort C-Win 
FUNCTION StartSpesialSort RETURNS LOGICAL
  ( INPUT icSortDescr AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD StrToExcel C-Win 
FUNCTION StrToExcel RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD TabStripChanged C-Win 
FUNCTION TabStripChanged RETURNS LOGICAL
  ( INPUT iiTab AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD UtvalgIsMaster C-Win 
FUNCTION UtvalgIsMaster RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ViewStat C-Win 
FUNCTION ViewStat RETURNS LOGICAL
  ( INPUT ibView AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE MENU POPUP-MENU-btnHuvGr 
       MENU-ITEM m_Sk_p_alle_hovedgrupper LABEL "Vis alle hovedgrupper".

DEFINE MENU POPUP-MENU-btnLev 
       MENU-ITEM m_Vis_alle_leverandr LABEL "Vis alle leverandører".


/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE TabStrip AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chTabStrip AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE BeskrivelseVarebokType AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 21.6 BY 1.

DEFINE VARIABLE KortNavn AS CHARACTER FORMAT "X(15)" 
     VIEW-AS FILL-IN 
     SIZE 16.2 BY 1.

DEFINE VARIABLE MesseBeskrivelse AS CHARACTER FORMAT "X(40)" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE MesseNr AS DECIMAL FORMAT ">>>>>>>9" INITIAL 0 
     LABEL "Messe" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE ProfilNr AS INTEGER FORMAT ">>>>>>9" INITIAL 0 
     LABEL "Prisprofil" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1.

DEFINE VARIABLE VareBokBeskrivelse AS CHARACTER FORMAT "X(40)" 
     VIEW-AS FILL-IN 
     SIZE 22.8 BY 1.

DEFINE VARIABLE VareBokNr AS DECIMAL FORMAT ">>>>>>>>>>>>9" INITIAL 0 
     LABEL "VareBok" 
     VIEW-AS FILL-IN 
     SIZE 16.2 BY 1.

DEFINE VARIABLE VareBokType AS INTEGER FORMAT ">>9" INITIAL 0 
     LABEL "Type" 
     VIEW-AS FILL-IN 
     SIZE 4.2 BY 1.

DEFINE RECTANGLE rectNavVarebok
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 31.8 BY 1.19.

DEFINE RECTANGLE rectToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 9.2 BY 1.19.

DEFINE RECTANGLE rectUpdToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 16.6 BY 1.19.

DEFINE RECTANGLE rectVarebok
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 190 BY 1.52.

DEFINE BUTTON btnSokMesseNr  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnSokProfilNr  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnSokVarebokType  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE VareBokNotat AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 60 BY 10.24 NO-UNDO.

DEFINE VARIABLE OppdatAv AS CHARACTER FORMAT "X(15)" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE OppdatDato AS DATE FORMAT "99/99/99" 
     LABEL "Dato/BrukerId" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 65 BY 1.43.

DEFINE VARIABLE Oppdatert AS LOGICAL INITIAL no 
     LABEL "Oppdatert" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .81 NO-UNDO.

DEFINE BUTTON btnBlankFilter 
     LABEL "Blank filter" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnSokFiltMesseNr 
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnSokFiltProfilNr 
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE fi-cVareBokBeskrivelse AS CHARACTER FORMAT "X(40)" 
     LABEL "Beskrivelse" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1.

DEFINE VARIABLE fi-fMesseNr AS DECIMAL FORMAT ">>>>>>>9" INITIAL 0 
     LABEL "MesseNr" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE fi-fProfilNr AS INTEGER FORMAT ">>>>>>9" INITIAL 0 
     LABEL "Prisprofil" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE BUTTON btnAktivitet 
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnAvdeling 
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnBlankLinjeFilter 
     LABEL "Blank filter" 
     SIZE 12.6 BY 1.

DEFINE BUTTON btnHuvGr 
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnLev 
     LABEL "..." 
     SIZE 4.4 BY 1 TOOLTIP "Høyreklikk for å velge blant alle leverandører".

DEFINE BUTTON btnMerknad 
     LABEL "..." 
     SIZE 4.4 BY 1 TOOLTIP "Høyreklikk for å velge blant alle leverandører".

DEFINE BUTTON btnSplitBarY 
     IMAGE-UP FILE "bmp/tabup.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Button 1" 
     SIZE 188 BY .43.

DEFINE BUTTON btnStartSearch 
     LABEL "Start søk" 
     SIZE 12.6 BY 1.

DEFINE BUTTON btnStartStat 
     LABEL "&Hent statistikk" 
     SIZE 16.4 BY 1.14.

DEFINE BUTTON btnVarGr 
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE AlfaFordeling AS CHARACTER FORMAT "X(256)":U 
     LABEL "Tilgj.str" 
     VIEW-AS FILL-IN 
     SIZE 30.4 BY .76 TOOLTIP "Størrelser med strekkode" NO-UNDO.

DEFINE VARIABLE ArtikkelNr AS DECIMAL FORMAT "zzzzzzzzzzzz9" INITIAL 0 
     LABEL "SEnr" 
     VIEW-AS FILL-IN 
     SIZE 20.2 BY 1.

DEFINE VARIABLE AvdelingNavn AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1.

DEFINE VARIABLE AvdelingNr AS INTEGER FORMAT ">>>9" INITIAL 0 
     LABEL "Avdeling" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1.

DEFINE VARIABLE avg_DB% AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1.

DEFINE VARIABLE avg_DBKr AS DECIMAL FORMAT "->,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15.8 BY 1 NO-UNDO.

DEFINE VARIABLE avg_forhKalkyle AS DECIMAL FORMAT "-9.9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 7.6 BY 1.

DEFINE VARIABLE avg_forhRab% AS DECIMAL FORMAT "->>9.99" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1.

DEFINE VARIABLE avg_Pris AS DECIMAL FORMAT "->>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE avg_supDB% AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1.

DEFINE VARIABLE avg_supDBKr AS DECIMAL FORMAT "->,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15.8 BY 1 NO-UNDO.

DEFINE VARIABLE avg_supKalkyle AS DECIMAL FORMAT "-9.9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 7.6 BY 1.

DEFINE VARIABLE avg_supRab% AS DECIMAL FORMAT "->>9.99" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1.

DEFINE VARIABLE avg_supVareKost AS DECIMAL FORMAT "->,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1.

DEFINE VARIABLE avg_VareKost AS DECIMAL FORMAT "->,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1.

DEFINE VARIABLE Beskr AS CHARACTER FORMAT "x(30)" 
     LABEL "Art.navn" 
     VIEW-AS FILL-IN 
     SIZE 35.6 BY 1.

DEFINE VARIABLE BrukerID AS CHARACTER FORMAT "X(10)" 
     VIEW-AS FILL-IN 
     SIZE 6.8 BY 1.

DEFINE VARIABLE DB% AS DECIMAL FORMAT "->9.99" INITIAL 0 
     LABEL "DB% forh" 
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1.

DEFINE VARIABLE DBKr AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     LABEL "DB forh" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE EDato AS DATE FORMAT "99/99/99" 
     LABEL "Endr" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE fi-cAnnenVarebok AS CHARACTER FORMAT "X(256)":U 
     LABEL "Andre VB" 
     VIEW-AS FILL-IN 
     SIZE 44 BY 1 NO-UNDO.

DEFINE VARIABLE Hg AS INTEGER FORMAT ">>>9" INITIAL 0 
     LABEL "Hovedgr" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1.

DEFINE VARIABLE HgBeskr AS CHARACTER FORMAT "x(20)" 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1.

DEFINE VARIABLE lab_avg AS CHARACTER FORMAT "X(256)":U INITIAL "Gj.Snitt:" 
      VIEW-AS TEXT 
     SIZE 9.4 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE lab_DB% AS CHARACTER FORMAT "X(256)":U INITIAL "DB%Forh:" 
      VIEW-AS TEXT 
     SIZE 11 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE lab_DB%sup AS CHARACTER FORMAT "X(256)":U INITIAL "DB%Sup:" 
      VIEW-AS TEXT 
     SIZE 11 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE lab_DBkr AS CHARACTER FORMAT "X(256)":U INITIAL "DBkr.Forh:" 
      VIEW-AS TEXT 
     SIZE 13.8 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE lab_DBkrSup AS CHARACTER FORMAT "X(256)":U INITIAL "DBkr.Sup:" 
      VIEW-AS TEXT 
     SIZE 12.4 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE lab_kalkf AS CHARACTER FORMAT "X(256)":U INITIAL "Klk.f:" 
      VIEW-AS TEXT 
     SIZE 6.8 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE lab_kalks AS CHARACTER FORMAT "X(256)":U INITIAL "Klk.s:" 
      VIEW-AS TEXT 
     SIZE 6.8 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE lab_ntoForh AS CHARACTER FORMAT "X(256)":U INITIAL "Nto.forh:" 
      VIEW-AS TEXT 
     SIZE 11 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE lab_ntoSup AS CHARACTER FORMAT "X(256)":U INITIAL "Nto.sup:" 
      VIEW-AS TEXT 
     SIZE 11 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE lab_Pris AS CHARACTER FORMAT "X(256)":U INITIAL "Markedspr(f+s):" 
      VIEW-AS TEXT 
     SIZE 17.4 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE lab_rabf AS CHARACTER FORMAT "X(256)":U INITIAL "Rab.forh:" 
      VIEW-AS TEXT 
     SIZE 11 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE lab_sup AS CHARACTER FORMAT "X(256)":U INITIAL "Rab.sup:" 
      VIEW-AS TEXT 
     SIZE 11 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE lab_tot AS CHARACTER FORMAT "X(256)":U INITIAL "Tot:" 
      VIEW-AS TEXT 
     SIZE 5.6 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE LevFargKod AS CHARACTER FORMAT "X(15)" 
     LABEL "Lev.farge" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1.

DEFINE VARIABLE LevKod AS CHARACTER FORMAT "x(20)" 
     LABEL "Lev.art.nr" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1.

DEFINE VARIABLE levnamn AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1.

DEFINE VARIABLE levnr AS INTEGER FORMAT "zzzzz9" INITIAL 0 
     LABEL "Leverandør" 
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1.

DEFINE VARIABLE LinjeMerknad AS CHARACTER FORMAT "X(256)":U 
     LABEL "Merknad" 
     VIEW-AS FILL-IN 
     SIZE 44 BY 1 NO-UNDO.

DEFINE VARIABLE ProdNr AS INTEGER FORMAT "zzzzz9" INITIAL 0 
     LABEL "Produsent" 
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1.

DEFINE VARIABLE ProdusentBeskrivelse AS CHARACTER FORMAT "x(50)" 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1.

DEFINE VARIABLE sokAktBeskrivelse AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE sokAktNr AS INTEGER FORMAT "zzzzz9" INITIAL 0 
     LABEL "Aktivitet" 
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1.

DEFINE VARIABLE sokArtikkelNr AS DECIMAL FORMAT "zzzzzzzzzzz9" INITIAL 0 
     LABEL "SE art.nr" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1.

DEFINE VARIABLE sokAvdelingNavn AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE sokAvdelingNr AS INTEGER FORMAT "zzzzz9" INITIAL 0 
     LABEL "Avdeling" 
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1.

DEFINE VARIABLE sokBeskr AS CHARACTER FORMAT "X(256)":U 
     LABEL "Utv.søk" 
     VIEW-AS FILL-IN 
     SIZE 33.6 BY 1
     BGCOLOR 16  NO-UNDO.

DEFINE VARIABLE sokFraEdato AS DATE FORMAT "99/99/99":U 
     LABEL "Endret dato" 
     VIEW-AS FILL-IN 
     SIZE 14.6 BY 1 NO-UNDO.

DEFINE VARIABLE sokFraVPIdato AS DATE FORMAT "99/99/99":U 
     LABEL "VPI dato" 
     VIEW-AS FILL-IN 
     SIZE 14.6 BY 1 NO-UNDO.

DEFINE VARIABLE sokHg AS INTEGER FORMAT "zzzzz9" INITIAL 0 
     LABEL "Hovedgruppe" 
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1.

DEFINE VARIABLE sokHgBeskr AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE sokLevFargKod AS CHARACTER FORMAT "X(15)" 
     LABEL "LevFargeK" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1.

DEFINE VARIABLE sokLevKod AS CHARACTER FORMAT "x(30)" 
     LABEL "LevArtNr" 
     VIEW-AS FILL-IN 
     SIZE 33.6 BY 1.

DEFINE VARIABLE sokLevNamn AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 22.8 BY 1 NO-UNDO.

DEFINE VARIABLE sokLevNr AS INTEGER FORMAT "zzzzz9" INITIAL 0 
     LABEL "Leverandør" 
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1.

DEFINE VARIABLE sokLinjeMerknad AS CHARACTER FORMAT "X(600)":U 
     LABEL "Merknad" 
     VIEW-AS FILL-IN 
     SIZE 33.4 BY 1 NO-UNDO.

DEFINE VARIABLE sokTilEdato AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 14.6 BY 1 NO-UNDO.

DEFINE VARIABLE sokTilVPIdato AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 14.6 BY 1 NO-UNDO.

DEFINE VARIABLE sokVg AS INTEGER FORMAT "zzzzz9" INITIAL 0 
     LABEL "Varegruppe" 
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1.

DEFINE VARIABLE sokVgBeskr AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE supDB% AS DECIMAL FORMAT "->9.99" INITIAL 0 
     LABEL "DB% sup" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1.

DEFINE VARIABLE supDBKr AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     LABEL "DB sup" 
     VIEW-AS FILL-IN 
     SIZE 15.2 BY 1.

DEFINE VARIABLE tot_Antall AS INTEGER FORMAT "->>>>>9":U INITIAL 0 
     LABEL "Ant.f" 
     VIEW-AS FILL-IN 
     SIZE 9.6 BY 1 NO-UNDO.

DEFINE VARIABLE tot_DBKr AS DECIMAL FORMAT "->,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15.8 BY 1 NO-UNDO.

DEFINE VARIABLE tot_Pris AS DECIMAL FORMAT "->>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE tot_supAntall AS INTEGER FORMAT "->>>>>9":U INITIAL 0 
     LABEL "Ant.s" 
     VIEW-AS FILL-IN 
     SIZE 9.6 BY 1 NO-UNDO.

DEFINE VARIABLE tot_supDBKr AS DECIMAL FORMAT "->,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15.8 BY 1 NO-UNDO.

DEFINE VARIABLE tot_supVareKost AS DECIMAL FORMAT "->,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1.

DEFINE VARIABLE tot_VareKost AS DECIMAL FORMAT "->,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1.

DEFINE VARIABLE Vg AS INTEGER FORMAT "zzzzz9" INITIAL 0 
     LABEL "Varegr" 
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1.

DEFINE VARIABLE VgBeskr AS CHARACTER FORMAT "x(20)" 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1.

DEFINE VARIABLE rsButModus AS LOGICAL INITIAL yes 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Butikk", yes,
"Kjedemodus", no
     SIZE 26 BY .81 NO-UNDO.

DEFINE RECTANGLE ArtikkelBilde
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 27 BY 5.33.

DEFINE RECTANGLE rectBrowseLinje
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 188 BY 10.71.

DEFINE RECTANGLE RectBrowseSearchLinje
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 22 BY 1.19.

DEFINE RECTANGLE rectBrowseSort
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 37.8 BY 3.48.

DEFINE RECTANGLE rectStatFields
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 187 BY 2.62.

DEFINE VARIABLE tbForventet AS LOGICAL INITIAL no 
     LABEL "Forv.ant" 
     VIEW-AS TOGGLE-BOX
     SIZE 12.8 BY .81 NO-UNDO.

DEFINE VARIABLE tbIkkeMerknad AS LOGICAL INITIAL no 
     LABEL "Artikler uten merknad" 
     VIEW-AS TOGGLE-BOX
     SIZE 23.8 BY .81 TOOLTIP "Vis kun artikler med valgt inndeling" NO-UNDO.

DEFINE VARIABLE tbSearchOnValueChange AS LOGICAL INITIAL yes 
     LABEL "Start søk ved endring av filterkriterier" 
     VIEW-AS TOGGLE-BOX
     SIZE 41 BY .81 NO-UNDO.

DEFINE VARIABLE tbUpdateStat AS LOGICAL INITIAL no 
     LABEL "Vis statistikk" 
     VIEW-AS TOGGLE-BOX
     SIZE 16.8 BY .67 NO-UNDO.

DEFINE VARIABLE tbValgtInndeling AS LOGICAL INITIAL no 
     LABEL "Artikler med inndeling" 
     VIEW-AS TOGGLE-BOX
     SIZE 23.8 BY .81 TOOLTIP "Vis kun artikler med valgt inndeling" NO-UNDO.

DEFINE RECTANGLE rectBrowseListe
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 188 BY 18.57.

DEFINE RECTANGLE RectBrowseSearchListe
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 22 BY 1.19.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     VareBokNr AT ROW 2.81 COL 9.8 COLON-ALIGNED HELP
          "Vareboknummer"
     VareBokBeskrivelse AT ROW 2.81 COL 26.2 COLON-ALIGNED HELP
          "Kort beskrivelse av vareboken" NO-LABEL
     VareBokType AT ROW 2.81 COL 55.8 COLON-ALIGNED HELP
          "Vareboktype"
     BeskrivelseVarebokType AT ROW 2.81 COL 60 COLON-ALIGNED HELP
          "Kort beskrivelse av vareboktypen" NO-LABEL
     MesseNr AT ROW 2.81 COL 90.8 COLON-ALIGNED HELP
          "Messenummer"
     MesseBeskrivelse AT ROW 2.81 COL 104.2 COLON-ALIGNED HELP
          "Navn eller kort beskrivelse av messen." NO-LABEL
     ProfilNr AT ROW 2.81 COL 134 COLON-ALIGNED HELP
          "Prisprofil"
     KortNavn AT ROW 2.81 COL 144.6 COLON-ALIGNED HELP
          "Kort betegnelse på prisprofilen" NO-LABEL
     rectToolBar AT ROW 1.14 COL 182
     rectUpdToolbar AT ROW 1.14 COL 1.4
     rectVarebok AT ROW 2.57 COL 1.4
     rectNavVarebok AT ROW 1.14 COL 150
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 191.2 BY 29.76.

DEFINE FRAME frmListe
     rectBrowseListe AT ROW 2.67 COL 2
     RectBrowseSearchListe AT ROW 1.33 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 10
         SIZE 189 BY 20.38.

DEFINE FRAME frmLinje
     sokAvdelingNr AT ROW 1.05 COL 13.8 COLON-ALIGNED HELP
          "Varegruppe"
     sokAvdelingNavn AT ROW 1.05 COL 24.6 COLON-ALIGNED NO-LABEL
     sokHg AT ROW 2.1 COL 13.8 COLON-ALIGNED HELP
          "Varegruppe"
     sokHgBeskr AT ROW 2.14 COL 24.6 COLON-ALIGNED NO-LABEL
     sokVg AT ROW 3.19 COL 13.8 COLON-ALIGNED HELP
          "Varegruppe"
     sokVgBeskr AT ROW 3.19 COL 24.6 COLON-ALIGNED NO-LABEL
     sokAktNr AT ROW 4.29 COL 13.8 COLON-ALIGNED HELP
          "Varegruppe"
     sokAktBeskrivelse AT ROW 4.29 COL 24.6 COLON-ALIGNED NO-LABEL
     btnSplitBarY AT ROW 17.71 COL 1
     sokLevNr AT ROW 1.05 COL 67.4 COLON-ALIGNED HELP
          "Varegruppe"
     btnAvdeling AT ROW 1.05 COL 52.2 NO-TAB-STOP 
     sokLevNamn AT ROW 1.05 COL 78.2 COLON-ALIGNED NO-LABEL
     sokBeskr AT ROW 2.14 COL 67.4 COLON-ALIGNED
     btnMerknad AT ROW 3.19 COL 103.2
     btnLev AT ROW 1.05 COL 103.4 NO-TAB-STOP 
     sokLevKod AT ROW 4.33 COL 67.4 COLON-ALIGNED HELP
          "Leverandørens artikkelnummer - bestillingsnummer"
     sokFraVPIdato AT ROW 1.05 COL 115.8 COLON-ALIGNED
     sokTilVPIdato AT ROW 1.05 COL 130.8 COLON-ALIGNED NO-LABEL
     sokFraEdato AT ROW 2.19 COL 115.8 COLON-ALIGNED
     btnHuvGr AT ROW 2.14 COL 52.2 NO-TAB-STOP 
     sokTilEdato AT ROW 2.19 COL 131 COLON-ALIGNED NO-LABEL
     sokArtikkelNr AT ROW 3.24 COL 115.8 COLON-ALIGNED
     sokLevFargKod AT ROW 4.33 COL 115.8 COLON-ALIGNED HELP
          "Leverandørens fargekode"
     tbIkkeMerknad AT ROW 3.38 COL 137
     tbValgtInndeling AT ROW 4.48 COL 137
     btnBlankLinjeFilter AT ROW 1.1 COL 148
     sokLinjeMerknad AT ROW 3.24 COL 67.4 COLON-ALIGNED
     btnVarGr AT ROW 3.19 COL 52.2 NO-TAB-STOP 
     btnStartSearch AT ROW 5.76 COL 148
     tbSearchOnValueChange AT ROW 5.86 COL 117.4
     rsButModus AT ROW 5.95 COL 69 NO-LABEL
     AvdelingNr AT ROW 18.33 COL 9 COLON-ALIGNED HELP
          "Avdeling"
     AvdelingNavn AT ROW 18.33 COL 19.8 COLON-ALIGNED HELP
          "Avdelingsnavn" NO-LABEL
     levnr AT ROW 18.33 COL 57.6 COLON-ALIGNED
     levnamn AT ROW 18.33 COL 68.2 COLON-ALIGNED NO-LABEL
     btnAktivitet AT ROW 4.29 COL 52.2 NO-TAB-STOP 
     LevKod AT ROW 18.33 COL 104 COLON-ALIGNED HELP
          "Leverandørens artikkelnummer - bestillingsnummer"
     ArtikkelNr AT ROW 18.33 COL 127.8 COLON-ALIGNED
     Hg AT ROW 19.38 COL 9 COLON-ALIGNED HELP
          "Hovedgruppe som varegruppen er koblet til"
     HgBeskr AT ROW 19.38 COL 19.8 COLON-ALIGNED HELP
          "Kort beskrivelse av hovedgruppen" NO-LABEL
     ProdNr AT ROW 19.38 COL 57.6 COLON-ALIGNED HELP
          "Produsent"
     ProdusentBeskrivelse AT ROW 19.38 COL 68.2 COLON-ALIGNED HELP
          "Kort beskrivelse av produsent" NO-LABEL
     LevFargKod AT ROW 19.38 COL 104 COLON-ALIGNED HELP
          "Leverandørens fargekode"
     EDato AT ROW 19.38 COL 127.8 COLON-ALIGNED HELP
          "Endret dato"
     BrukerID AT ROW 19.38 COL 141 COLON-ALIGNED HELP
          "Bruker som registrerte/endret posten" NO-LABEL
     Vg AT ROW 20.43 COL 9 COLON-ALIGNED HELP
          "Varegruppe"
     VgBeskr AT ROW 20.43 COL 19.8 COLON-ALIGNED HELP
          "Kort beskrivelse av varegruppen" NO-LABEL
     fi-cAnnenVarebok AT ROW 20.43 COL 104 COLON-ALIGNED
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 5.52
         SCROLLABLE SIZE 188 BY 24.76.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME frmLinje
     Beskr AT ROW 20.48 COL 57.6 COLON-ALIGNED HELP
          "Varetekst - kort beskrivelse av artikkelen"
     LinjeMerknad AT ROW 21.48 COL 104 COLON-ALIGNED
     DB% AT ROW 21.52 COL 9 COLON-ALIGNED HELP
          "Dekningsbidrag oppgitt i %"
     DBKr AT ROW 21.52 COL 28.8 COLON-ALIGNED HELP
          "Dekningsbidrag"
     supDB% AT ROW 21.52 COL 57.6 COLON-ALIGNED HELP
          "Suppleringsdekningsbidrag oppgitt i %"
     supDBKr AT ROW 21.52 COL 78 COLON-ALIGNED HELP
          "Suppleringsdekningsbidrag"
     AlfaFordeling AT ROW 21.76 COL 156 COLON-ALIGNED
     tbUpdateStat AT ROW 22.86 COL 3.2
     tot_Antall AT ROW 23.43 COL 176 COLON-ALIGNED
     avg_VareKost AT ROW 23.48 COL 25.6 COLON-ALIGNED HELP
          "Varekost" NO-LABEL
     avg_forhRab% AT ROW 23.48 COL 41.2 COLON-ALIGNED HELP
          "Rabatt på forhåndskjøp" NO-LABEL
     avg_forhKalkyle AT ROW 23.48 COL 52.8 COLON-ALIGNED HELP
          "Forhåndskalkyle" NO-LABEL
     avg_DBKr AT ROW 23.48 COL 60.8 COLON-ALIGNED HELP
          "Dekningsbidrag" NO-LABEL
     avg_DB% AT ROW 23.48 COL 77.6 COLON-ALIGNED HELP
          "Dekningsbidrag oppgitt i %" NO-LABEL
     avg_supVareKost AT ROW 23.48 COL 89.8 COLON-ALIGNED HELP
          "Varekost" NO-LABEL
     avg_supRab% AT ROW 23.48 COL 105.6 COLON-ALIGNED HELP
          "Rabatt på forhåndskjøp" NO-LABEL
     avg_supKalkyle AT ROW 23.48 COL 116.8 COLON-ALIGNED HELP
          "Forhåndskalkyle" NO-LABEL
     avg_supDBKr AT ROW 23.48 COL 125.2 COLON-ALIGNED HELP
          "Dekningsbidrag" NO-LABEL
     avg_supDB% AT ROW 23.48 COL 141.8 COLON-ALIGNED HELP
          "Dekningsbidrag oppgitt i %" NO-LABEL
     avg_Pris AT ROW 23.48 COL 153.6 COLON-ALIGNED HELP
          "Pris (Til kunde)" NO-LABEL
     tbForventet AT ROW 23.57 COL 4
     btnStartStat AT ROW 24.43 COL 3.6
     tot_supAntall AT ROW 24.52 COL 176 COLON-ALIGNED
     tot_VareKost AT ROW 24.57 COL 25.6 COLON-ALIGNED HELP
          "Varekost" NO-LABEL
     tot_DBKr AT ROW 24.57 COL 60.8 COLON-ALIGNED HELP
          "Dekningsbidrag" NO-LABEL
     tot_supVareKost AT ROW 24.57 COL 89.8 COLON-ALIGNED HELP
          "Varekost" NO-LABEL
     tot_supDBKr AT ROW 24.57 COL 125.2 COLON-ALIGNED HELP
          "Dekningsbidrag" NO-LABEL
     tot_Pris AT ROW 24.57 COL 153.6 COLON-ALIGNED HELP
          "Pris (Til kunde)" NO-LABEL
     lab_ntoForh AT ROW 22.86 COL 26 COLON-ALIGNED NO-LABEL
     lab_rabf AT ROW 22.86 COL 41.2 COLON-ALIGNED NO-LABEL
     lab_kalkf AT ROW 22.86 COL 53.6 COLON-ALIGNED NO-LABEL
     lab_DBkr AT ROW 22.86 COL 61.8 COLON-ALIGNED NO-LABEL
     lab_DB% AT ROW 22.86 COL 77.2 COLON-ALIGNED NO-LABEL
     lab_ntoSup AT ROW 22.86 COL 90.2 COLON-ALIGNED NO-LABEL
     lab_sup AT ROW 22.86 COL 105.6 COLON-ALIGNED NO-LABEL
     lab_kalks AT ROW 22.86 COL 117.6 COLON-ALIGNED NO-LABEL
     lab_DBkrSup AT ROW 22.86 COL 126.2 COLON-ALIGNED NO-LABEL
     lab_DB%sup AT ROW 22.86 COL 141.4 COLON-ALIGNED NO-LABEL
     lab_Pris AT ROW 22.86 COL 153.6 COLON-ALIGNED NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 5.52
         SCROLLABLE SIZE 188 BY 24.76.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME frmLinje
     lab_avg AT ROW 23.67 COL 15.6 COLON-ALIGNED NO-LABEL
     lab_tot AT ROW 24.67 COL 19.6 COLON-ALIGNED NO-LABEL
     RectBrowseSearchLinje AT ROW 5.62 COL 2
     rectBrowseLinje AT ROW 6.95 COL 1
     rectStatFields AT ROW 23.14 COL 2
     rectBrowseSort AT ROW 18.24 COL 151
     ArtikkelBilde AT ROW 1.14 COL 161.6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 5.52
         SCROLLABLE SIZE 188 BY 24.76.

DEFINE FRAME frmFilter
     fi-fMesseNr AT ROW 1.24 COL 27.2 COLON-ALIGNED HELP
          "Messenummer"
     btnSokFiltMesseNr AT ROW 1.24 COL 42.4 NO-TAB-STOP 
     btnSokFiltProfilNr AT ROW 2.19 COL 42.4 NO-TAB-STOP 
     fi-fProfilNr AT ROW 2.24 COL 27.2 COLON-ALIGNED HELP
          "Prisprofil"
     btnBlankFilter AT ROW 3.14 COL 142
     fi-cVareBokBeskrivelse AT ROW 3.29 COL 27.2 COLON-ALIGNED HELP
          "Kort beskrivelse av vareboken"
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 5.52
         SIZE 188 BY 4.76.

DEFINE FRAME frmDetalj
     btnSokMesseNr AT ROW 5.43 COL 34.2 NO-TAB-STOP 
     VareBokNr AT ROW 1.24 COL 19 COLON-ALIGNED HELP
          "Vareboknummer"
          LABEL "VareBokNr" FORMAT ">>>>>>>>>>>>9"
          VIEW-AS FILL-IN 
          SIZE 20.2 BY 1
     OppdatDato AT ROW 1.48 COL 122.6 COLON-ALIGNED
     OppdatAv AT ROW 1.48 COL 136.6 COLON-ALIGNED NO-LABEL
     Oppdatert AT ROW 1.62 COL 93.4 HELP
          "Varebok er oppdatert"
     VareBokType AT ROW 2.29 COL 19 COLON-ALIGNED HELP
          "Vareboktype"
          LABEL "Vareboktype" FORMAT ">>9"
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     BeskrivelseVarebokType AT ROW 2.29 COL 29.8 COLON-ALIGNED HELP
          "Kort beskrivelse av vareboktypen" NO-LABEL FORMAT "X(30)"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     ProfilNr AT ROW 3.33 COL 19 COLON-ALIGNED HELP
          "Prisprofil"
          LABEL "Prisprofil" FORMAT ">>>>>>9"
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
     KortNavn AT ROW 3.33 COL 37 COLON-ALIGNED HELP
          "Kort betegnelse på prisprofilen" NO-LABEL FORMAT "X(15)"
          VIEW-AS FILL-IN 
          SIZE 25 BY 1
     VareBokBeskrivelse AT ROW 4.38 COL 19 COLON-ALIGNED HELP
          "Kort beskrivelse av vareboken"
          LABEL "Beskrivelse" FORMAT "X(60)"
          VIEW-AS FILL-IN 
          SIZE 60 BY 1
     MesseNr AT ROW 5.43 COL 19 COLON-ALIGNED HELP
          "Messenummer"
          LABEL "MesseNr" FORMAT ">>>>>>>9"
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     MesseBeskrivelse AT ROW 5.43 COL 36.6 COLON-ALIGNED HELP
          "Navn eller kort beskrivelse av messen." NO-LABEL FORMAT "X(40)"
          VIEW-AS FILL-IN 
          SIZE 42.4 BY 1
     VareBokNotat AT ROW 6.52 COL 21 NO-LABEL
     btnSokVarebokType AT ROW 2.29 COL 27.2 NO-TAB-STOP 
     btnSokProfilNr AT ROW 3.33 COL 34.2 NO-TAB-STOP 
     "(Notatfeltet blir satt inn som undertekst i EAN rapport - CTRL-Enter: Linjeskift i tekst)" VIEW-AS TEXT
          SIZE 80 BY .62 AT ROW 17.19 COL 22
     RECT-1 AT ROW 1.24 COL 92
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 5.52
         SIZE 188 BY 24.76.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window Template
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Varebokregister"
         HEIGHT             = 29.52
         WIDTH              = 191.6
         MAX-HEIGHT         = 50
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 50
         VIRTUAL-WIDTH      = 320
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

{incl/devmode.i}
{incl/CustDevMode.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* REPARENT FRAME */
ASSIGN FRAME frmDetalj:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME frmFilter:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME frmLinje:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME frmListe:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
ASSIGN 
       BeskrivelseVarebokType:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       KortNavn:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       MesseBeskrivelse:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       MesseNr:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN ProfilNr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE rectNavVarebok IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       rectNavVarebok:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       VareBokBeskrivelse:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       VareBokNr:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN VareBokType IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME frmDetalj
                                                                        */
/* SETTINGS FOR FILL-IN KortNavn IN FRAME frmDetalj
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN MesseBeskrivelse IN FRAME frmDetalj
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN OppdatAv IN FRAME frmDetalj
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN OppdatDato IN FRAME frmDetalj
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ProfilNr IN FRAME frmDetalj
   NO-ENABLE                                                            */
ASSIGN 
       VareBokNotat:RETURN-INSERTED IN FRAME frmDetalj  = TRUE.

/* SETTINGS FOR FILL-IN VareBokNr IN FRAME frmDetalj
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME frmFilter
                                                                        */
/* SETTINGS FOR FRAME frmLinje
   Custom                                                               */
ASSIGN 
       FRAME frmLinje:HEIGHT           = 24.76
       FRAME frmLinje:WIDTH            = 188.

ASSIGN 
       AvdelingNavn:READ-ONLY IN FRAME frmLinje        = TRUE.

ASSIGN 
       avg_DB%:READ-ONLY IN FRAME frmLinje        = TRUE.

ASSIGN 
       avg_DBKr:READ-ONLY IN FRAME frmLinje        = TRUE.

ASSIGN 
       avg_forhKalkyle:READ-ONLY IN FRAME frmLinje        = TRUE.

ASSIGN 
       avg_forhRab%:READ-ONLY IN FRAME frmLinje        = TRUE.

ASSIGN 
       avg_Pris:READ-ONLY IN FRAME frmLinje        = TRUE.

ASSIGN 
       avg_supDB%:READ-ONLY IN FRAME frmLinje        = TRUE.

ASSIGN 
       avg_supDBKr:READ-ONLY IN FRAME frmLinje        = TRUE.

ASSIGN 
       avg_supKalkyle:READ-ONLY IN FRAME frmLinje        = TRUE.

ASSIGN 
       avg_supRab%:READ-ONLY IN FRAME frmLinje        = TRUE.

ASSIGN 
       avg_supVareKost:READ-ONLY IN FRAME frmLinje        = TRUE.

ASSIGN 
       avg_VareKost:READ-ONLY IN FRAME frmLinje        = TRUE.

ASSIGN 
       btnHuvGr:POPUP-MENU IN FRAME frmLinje       = MENU POPUP-MENU-btnHuvGr:HANDLE.

ASSIGN 
       btnLev:POPUP-MENU IN FRAME frmLinje       = MENU POPUP-MENU-btnLev:HANDLE.

ASSIGN 
       btnSplitBarY:MOVABLE IN FRAME frmLinje          = TRUE.

/* SETTINGS FOR BUTTON btnStartSearch IN FRAME frmLinje
   NO-ENABLE                                                            */
ASSIGN 
       btnStartSearch:HIDDEN IN FRAME frmLinje           = TRUE.

ASSIGN 
       fi-cAnnenVarebok:READ-ONLY IN FRAME frmLinje        = TRUE.

ASSIGN 
       HgBeskr:READ-ONLY IN FRAME frmLinje        = TRUE.

ASSIGN 
       sokLinjeMerknad:READ-ONLY IN FRAME frmLinje        = TRUE.

/* SETTINGS FOR TOGGLE-BOX tbSearchOnValueChange IN FRAME frmLinje
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       tbSearchOnValueChange:HIDDEN IN FRAME frmLinje           = TRUE.

ASSIGN 
       tot_Antall:READ-ONLY IN FRAME frmLinje        = TRUE.

ASSIGN 
       tot_DBKr:READ-ONLY IN FRAME frmLinje        = TRUE.

ASSIGN 
       tot_Pris:READ-ONLY IN FRAME frmLinje        = TRUE.

ASSIGN 
       tot_supAntall:READ-ONLY IN FRAME frmLinje        = TRUE.

ASSIGN 
       tot_supDBKr:READ-ONLY IN FRAME frmLinje        = TRUE.

ASSIGN 
       tot_supVareKost:READ-ONLY IN FRAME frmLinje        = TRUE.

ASSIGN 
       tot_VareKost:READ-ONLY IN FRAME frmLinje        = TRUE.

ASSIGN 
       VgBeskr:READ-ONLY IN FRAME frmLinje        = TRUE.

/* SETTINGS FOR FRAME frmListe
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME frmDetalj
/* Query rebuild information for FRAME frmDetalj
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME frmDetalj */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME frmLinje
/* Query rebuild information for FRAME frmLinje
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME frmLinje */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME TabStrip ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 4.33
       COLUMN          = 2
       HEIGHT          = 26.19
       WIDTH           = 189.6
       HIDDEN          = no
       SENSITIVE       = yes.
/* TabStrip OCXINFO:CREATE-CONTROL from: {1EFB6596-857C-11D1-B16A-00C0F0283628} type: TabStrip */
      TabStrip:MOVE-AFTER(KortNavn:HANDLE IN FRAME DEFAULT-FRAME).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Varebokregister */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Varebokregister */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frmLinje
&Scoped-define SELF-NAME btnAktivitet
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAktivitet C-Win
ON CHOOSE OF btnAktivitet IN FRAME frmLinje /* ... */
DO:
  DEF VAR cLookupValue AS CHAR NO-UNDO.

  cLookupValue = "AktNr;Beskrivelse".

  IF sokVg:SCREEN-VALUE NE "0" THEN 
    RUN JBoxDLookup.w ("VgAkt;Vg,Aktivitet;AktNr;Beskrivelse", 
                       "WHERE Vg = " + sokVg:SCREEN-VALUE + ",first Aktivitet OF VgAkt",
                       INPUT-OUTPUT cLookupValue).
  ELSE
    RUN JBoxDLookup.w ("Aktivitet;AktNr;Beskrivelse", 
                       "where true", 
                       INPUT-OUTPUT cLookupValue).

  IF cLookupValue NE "" THEN DO:      
    ASSIGN sokAktNr:SCREEN-VALUE          = ENTRY(1,cLookupValue,"|")
           sokAktBeskrivelse:SCREEN-VALUE = ENTRY(2,cLookupValue,"|")
           .
    IF tbSearchOnValueChange THEN RUN OpenQuery.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAvdeling
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAvdeling C-Win
ON CHOOSE OF btnAvdeling IN FRAME frmLinje /* ... */
DO:
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "Avdeling;AvdelingNr;AvdelingNavn",
                      "where true",
                      INPUT-OUTPUT cAvdelingRowIdList,
                      "AvdelingNr",
                      INPUT-OUTPUT cAvdelingIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk THEN DO:
    IF NUM-ENTRIES(cAvdelingRowidList) > 1 THEN 
      ASSIGN sokAvdelingNr:SCREEN-VALUE   = "0"
             sokAvdelingNavn:SCREEN-VALUE = STRING(NUM-ENTRIES(cAvdelingRowidList)) + " av " +
                                       STRING(iSelectorSourcCount)
                                       .
    ELSE
      ASSIGN sokAvdelingNr:SCREEN-VALUE   = cAvdelingIdList
             sokAvdelingNavn:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","Avdeling;AvdelingNavn","WHERE AvdelingNr = " + sokAvdelingNr:SCREEN-VALUE).
    IF tbSearchOnValueChange THEN RUN OpenQuery.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frmFilter
&Scoped-define SELF-NAME btnBlankFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnBlankFilter C-Win
ON CHOOSE OF btnBlankFilter IN FRAME frmFilter /* Blank filter */
DO:
  ASSIGN
      fi-fProfilNr:SCREEN-VALUE = ""
      fi-fMesseNr:SCREEN-VALUE = ""
      fi-cVareBokBeskrivelse:SCREEN-VALUE = ""
      .
   RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frmLinje
&Scoped-define SELF-NAME btnBlankLinjeFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnBlankLinjeFilter C-Win
ON CHOOSE OF btnBlankLinjeFilter IN FRAME frmLinje /* Blank filter */
DO:
  ASSIGN sokAvdelingNavn:SCREEN-VALUE   = ""
         sokAvdelingNr:SCREEN-VALUE     = "0" 
         sokHg:SCREEN-VALUE             = "0" 
         sokHgBeskr:SCREEN-VALUE        = "" 
         sokVg:SCREEN-VALUE             = "0" 
         sokVgBeskr:SCREEN-VALUE        = ""
         sokLevNr:SCREEN-VALUE          = "0"
         sokLevNamn:SCREEN-VALUE        = ""
         sokBeskr:SCREEN-VALUE          = ""
         sokAktNr:SCREEN-VALUE          = "0"
         sokAktBeskrivelse:SCREEN-VALUE = ""
         sokFraEdato:SCREEN-VALUE       = ?
         sokTilEdato:SCREEN-VALUE       = ?
         sokFraVPIdato:SCREEN-VALUE     = ?
         sokTilVPIdato:SCREEN-VALUE     = ?
         cLevBasRowIdList               = ""
         cLevBasIdList                  = ""
         cAvdelingRowIdList             = ""
         cAvdelingIdList                = ""
         sokArtikkelNr:SCREEN-VALUE     = STRING(0)
         sokLevFargKod:SCREEN-VALUE     = ""
         sokLevKod:SCREEN-VALUE         = ""
         bOK                            = FALSE
         sokLinjeMerknad:SCREEN-VALUE   = " "
         .
/*    MESSAGE "Vil du starte spørring uten filterkriterier?"                        */
/*            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE bOk.                 */
/*    IF NOT bOK THEN DO:                                                           */
/*      bCloseQuery = TRUE.                                                         */
/*      DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"queryfilter","where false").  */
/*    END.                                                                          */
   RUN OpenQuery.
   bCloseQuery = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnHuvGr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnHuvGr C-Win
ON CHOOSE OF btnHuvGr IN FRAME frmLinje /* ... */
DO:

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "HuvGr;Hg;HgBeskr,Avdeling;AvdelingNr;AvdelingNavn,VarebokLinje;",
                      "where true, first Avdeling OF HuvGr NO-LOCK " + 
                         (IF cAvdelingRowIdList NE "" THEN
                            "WHERE CAN-DO('" + cAvdelingRowIdList + "',STRING(ROWID(Avdeling)))"
                          ELSE "")
                    + ",FIRST VarebokLinje OF HuvGr NO-LOCK WHERE VarebokNr = " + STRING(hFieldMap:BUFFER-FIELD("VarebokNr"):BUFFER-VALUE)
                      ,
                      INPUT-OUTPUT cHuvGrRowIdList,
                      "Hg",
                      INPUT-OUTPUT cHuvGrIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.


  IF bOk THEN DO:
    IF NUM-ENTRIES(cHuvGrRowidList) > 1 THEN 
      ASSIGN sokHg:SCREEN-VALUE   = "0"
             sokHgBeskr:SCREEN-VALUE = STRING(NUM-ENTRIES(cHuvGrRowidList)) + " av " +
                                       STRING(iSelectorSourcCount)
                                       .
    ELSE
      ASSIGN sokHg:SCREEN-VALUE   = cHuvGrIdList
             sokHgBeskr:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","HuvGr;HgBeskr","WHERE Hg = " + sokHg:SCREEN-VALUE).
    IF tbSearchOnValueChange THEN RUN OpenQuery.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLev C-Win
ON CHOOSE OF btnLev IN FRAME frmLinje /* ... */
DO:
  IF NOT hFieldMap:AVAIL THEN RETURN NO-APPLY.

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "LevBas;levnamn;levnr;KjedeAvtale|Kjedeavtale|J/N"
                       + ",VarebokLinje;"
                      ,"WHERE TRUE"
                       + ",FIRST VarebokLinje OF LevBas NO-LOCK WHERE VarebokNr = " + STRING(hFieldMap:BUFFER-FIELD("VarebokNr"):BUFFER-VALUE) 
                      ,
                      INPUT-OUTPUT cLevBasRowIdList,
                      "Levnr",
                      INPUT-OUTPUT cLevBasIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk THEN DO WITH FRAME frmLinje:
    IF NUM-ENTRIES(cLevBasRowidList) > 1 THEN 
      ASSIGN sokLevNr:SCREEN-VALUE   = "0"
             sokLevNamn:SCREEN-VALUE = STRING(NUM-ENTRIES(cLevBasRowidList)) + " av " +
                                       STRING(iSelectorSourcCount)
                                       .
    ELSE
      ASSIGN sokLevNr:SCREEN-VALUE   = cLevBasIdList
             sokLevNamn:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","LevBas;LevNamn","WHERE LevNr = " + sokLevNr:SCREEN-VALUE).
    IF tbSearchOnValueChange THEN RUN OpenQuery.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnMerknad
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnMerknad C-Win
ON CHOOSE OF btnMerknad IN FRAME frmLinje /* ... */
DO:
  DEF VAR cRowIdList  AS CHAR NO-UNDO.
  DEF VAR cIdList     AS CHAR NO-UNDO.
  DEF VAR iy          AS INT  NO-UNDO.

  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "temp-table"                               /* Dummy buffer */
                      + ";!dummy|CHARACTER"                      /* <- must invoke a dummy non-visual field to avoid initial sort since calculated fields normally arn't sortable */
                      + ";Description|CHARACTER|x(30)||Merknadselement"
                      ,"where false",
                      INPUT-OUTPUT cRowIdList,
                      "Description",
                      INPUT-OUTPUT cIdList,
                      "","",
                      OUTPUT bOK).

  IF bOk THEN DO:
    sokLinjeMerknad:SCREEN-VALUE = REPLACE(cIdList,"|",",").
    RUN InvokeMethod(hBrowseLinje,"OpenQuery").
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frmFilter
&Scoped-define SELF-NAME btnSokFiltMesseNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSokFiltMesseNr C-Win
ON CHOOSE OF btnSokFiltMesseNr IN FRAME frmFilter /* ... */
OR F10 OF MesseNr
DO:
  DEF VAR cLookupValue AS CHAR NO-UNDO.

  /* Syntaks: Param1: <Tabell>;<Felt>;<Felt>...,<Tabell>;<Felt>;<Felt>...              */
  /*          Param2: <Where sats> m/Join                                              */
  /*          Param3: <Returfelt1>[;<Returfelt2>;......],<Filterfelt1>[;<Filterfelt2>] (Settes i cLookupValue) */
  /* Kalkulerte felt kan også benyttes, label, format o.l..       */
  cLookupValue = "MesseNr".
  RUN JBoxDLookup.w ("Messe;MesseNr;MesseBeskrivelse","where true",INPUT-OUTPUT cLookupValue).

  IF cLookupValue NE "" THEN 
  DO:
    fi-fMesseNr:SCREEN-VALUE = cLookupValue.
    RUN OpenQuery.
  END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSokFiltProfilNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSokFiltProfilNr C-Win
ON CHOOSE OF btnSokFiltProfilNr IN FRAME frmFilter /* ... */
OR F10 OF ProfilNr
DO:
  DEF VAR cLookupValue AS CHAR NO-UNDO.

  /* Syntaks: Param1: <Tabell>;<Felt>;<Felt>...,<Tabell>;<Felt>;<Felt>...              */
  /*          Param2: <Where sats> m/Join                                              */
  /*          Param3: <Returfelt1>[;<Returfelt2>;......],<Filterfelt1>[;<Filterfelt2>] (Settes i cLookupValue) */
  /* Kalkulerte felt kan også benyttes, label, format o.l..       */
  cLookupValue = "ProfilNr".
  RUN JBoxDLookup.w ("PrisProfil;ProfilNr|Profilnr|>>>>>>9;KortNavn","where true",INPUT-OUTPUT cLookupValue).

  IF cLookupValue NE "" THEN 
  DO:
    fi-fProfilNr:SCREEN-VALUE = cLookupValue.
    RUN OpenQuery.
  END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frmDetalj
&Scoped-define SELF-NAME btnSokMesseNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSokMesseNr C-Win
ON CHOOSE OF btnSokMesseNr IN FRAME frmDetalj /* ... */
OR F10 OF MesseNr
DO:
  DEF VAR cLookupValue AS CHAR NO-UNDO.

  /* Syntaks: Param1: <Tabell>;<Felt>;<Felt>...,<Tabell>;<Felt>;<Felt>...              */
  /*          Param2: <Where sats> m/Join                                              */
  /*          Param3: <Returfelt1>[;<Returfelt2>;......],<Filterfelt1>[;<Filterfelt2>] (Settes i cLookupValue) */
  /* Kalkulerte felt kan også benyttes, label, format o.l..       */
  cLookupValue = "MesseNr".
  RUN JBoxDLookup.w ("Messe;MesseNr;MesseBeskrivelse","where true",INPUT-OUTPUT cLookupValue).

  IF cLookupValue NE "" THEN 
  DO:
    MesseNr:SCREEN-VALUE = cLookupValue.
    APPLY "ANY-PRINTABLE":U TO MesseNr.
    APPLY "TAB":U TO MesseNr.
  END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSokProfilNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSokProfilNr C-Win
ON CHOOSE OF btnSokProfilNr IN FRAME frmDetalj /* ... */
OR F10 OF ProfilNr
DO:
  DEF VAR cLookupValue AS CHAR NO-UNDO.

  /* Syntaks: Param1: <Tabell>;<Felt>;<Felt>...,<Tabell>;<Felt>;<Felt>...              */
  /*          Param2: <Where sats> m/Join                                              */
  /*          Param3: <Returfelt1>[;<Returfelt2>;......],<Filterfelt1>[;<Filterfelt2>] (Settes i cLookupValue) */
  /* Kalkulerte felt kan også benyttes, label, format o.l..       */
  cLookupValue = "ProfilNr".
  RUN JBoxDLookup.w ("PrisProfil;ProfilNr|Profilnr|>>>>>>9;KortNavn","where true",INPUT-OUTPUT cLookupValue).

  IF cLookupValue NE "" THEN 
  DO:
    ProfilNr:SCREEN-VALUE = cLookupValue.
    APPLY "ANY-PRINTABLE":U TO ProfilNr.
    APPLY "TAB":U TO ProfilNr.
  END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSokVarebokType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSokVarebokType C-Win
ON CHOOSE OF btnSokVarebokType IN FRAME frmDetalj /* ... */
OR F10 OF VarebokType 
DO:
  DEF VAR cLookupValue AS CHAR NO-UNDO.

  /* Syntaks: Param1: <Tabell>;<Felt>;<Felt>...,<Tabell>;<Felt>;<Felt>...              */
  /*          Param2: <Where sats> m/Join                                              */
  /*          Param3: <Returfelt1>[;<Returfelt2>;......],<Filterfelt1>[;<Filterfelt2>] (Settes i cLookupValue) */
  /* Kalkulerte felt kan også benyttes, label, format o.l..       */
  cLookupValue = "VarebokType".
  RUN JBoxDLookup.w ("VarebokType;VarebokType;BeskrivelseVarebokType","where true",INPUT-OUTPUT cLookupValue).

  IF cLookupValue NE "" THEN 
  DO:
    VarebokType:SCREEN-VALUE = cLookupValue.
    APPLY "ANY-PRINTABLE":U TO VarebokType.
    APPLY "TAB":U TO VarebokType.
  END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frmLinje
&Scoped-define SELF-NAME btnSplitBarY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSplitBarY C-Win
ON END-MOVE OF btnSplitBarY IN FRAME frmLinje /* Button 1 */
DO:
  DYNAMIC-FUNCTION("setSplitBarY" , THIS-PROCEDURE:CURRENT-WINDOW, btnSplitBarY:HANDLE IN FRAME frmLinje,NO).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnStartSearch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnStartSearch C-Win
ON CHOOSE OF btnStartSearch IN FRAME frmLinje /* Start søk */
DO:
  DYNAMIC-FUNCTION("setTmpObject",?).
  DYNAMIC-FUNCTION("setCurrentObject",hBrowseLinje).
  RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnStartStat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnStartStat C-Win
ON CHOOSE OF btnStartStat IN FRAME frmLinje /* Hent statistikk */
DO:
  
  IF NOT hFieldMap:AVAIL THEN
    hBrowseListe:SELECT-ROW(hBrowseListe:FOCUSED-ROW) NO-ERROR.

  IF NOT hFieldMap:AVAIL THEN RETURN.

  IF tbForventet:CHECKED THEN DO:
    IF DYNAMIC-FUNCTION("runProc","varebok_querystat.p",
                         DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"BaseQuery") +
                         DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"QueryFilter") + "|" +
                         cStatFieldNames + "|" +
                         cPrefixedStatFields
                        ,?) THEN DO:
      DYNAMIC-FUNCTION("setQueryStatValues",DYNAMIC-FUNCTION("getTransactionMessage")).
      ViewStat(TRUE).
      APPLY "entry" TO hBrowseLinje.
    END.
    ELSE DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
  END.

  ELSE DO:
    DYNAMIC-FUNCTION("setCurrentObject",hBrowseLinje).
    RUN OpenQuery.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnVarGr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnVarGr C-Win
ON CHOOSE OF btnVarGr IN FRAME frmLinje /* ... */
DO:
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "VarGr;Vg;VgBeskr,HuvGr;Hg;HgBeskr",
                      "where true, first HuvGr OF VarGr NO-LOCK " + 
                         (IF cHuvGrRowIdList NE "" THEN
                            "WHERE CAN-DO('" + cHuvGrRowIdList + "',STRING(ROWID(HuvGr)))"
                          ELSE ""),
                      INPUT-OUTPUT cVarGrRowIdList,
                      "Vg",
                      INPUT-OUTPUT cVarGrIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.


  IF bOk THEN DO:
    IF NUM-ENTRIES(cVarGrRowidList) > 1 THEN 
      ASSIGN sokVG:SCREEN-VALUE      = "0"
             sokVGBeskr:SCREEN-VALUE = STRING(NUM-ENTRIES(cVarGrRowidList)) + " av " +
                                       STRING(iSelectorSourcCount)
                                       .
    ELSE
      ASSIGN sokVG:SCREEN-VALUE   = cVarGrIdList
             sokVGBeskr:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","VarGr;VgBeskr","WHERE Vg = " + sokVG:SCREEN-VALUE).
    IF tbSearchOnValueChange THEN RUN OpenQuery.
  END.

/*   DEF VAR cLookupValue AS CHAR NO-UNDO.                               */
/*                                                                       */
/*   cLookupValue = "Vg;VgBeskr".                                        */
/*                                                                       */
/*   RUN JBoxDLookup.w ("VarGr;Vg;VgBeskr",                              */
/*                      (IF sokHg:SCREEN-VALUE NE "0" THEN               */
/*                         "WHERE Hg = " + sokHg:SCREEN-VALUE            */
/*                       ELSE "where true"),                             */
/*                      INPUT-OUTPUT cLookupValue).                      */
/*                                                                       */
/*   IF cLookupValue NE "" THEN DO:                                      */
/*     ASSIGN sokVg:SCREEN-VALUE             = ENTRY(1,cLookupValue,"|") */
/*            sokVgBeskr:SCREEN-VALUE        = ENTRY(2,cLookupValue,"|") */
/*            sokAktNr:SCREEN-VALUE          = STRING(0)                 */
/*            sokAktBeskrivelse:SCREEN-VALUE = ""                        */
/*            .                                                          */
/*     RUN OpenQuery.                                                    */
/*   END.                                                                */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frmFilter
&Scoped-define SELF-NAME fi-cVareBokBeskrivelse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cVareBokBeskrivelse C-Win
ON RETURN OF fi-cVareBokBeskrivelse IN FRAME frmFilter /* Beskrivelse */
OR "TAB":U OF fi-cVareBokBeskrivelse
DO:
  RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-fMesseNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-fMesseNr C-Win
ON RETURN OF fi-fMesseNr IN FRAME frmFilter /* MesseNr */
OR "TAB":U OF fi-fMesseNr
DO:
  IF fi-fMesseNr:MODIFIED THEN RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-fProfilNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-fProfilNr C-Win
ON RETURN OF fi-fProfilNr IN FRAME frmFilter /* Prisprofil */
OR "TAB":U OF fi-fProfilNr
DO:
  IF fi-fProfilNr:MODIFIED THEN RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME MesseNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL MesseNr C-Win
ON RETURN OF MesseNr IN FRAME DEFAULT-FRAME /* Messe */
OR "TAB" OF MesseNr
DO:
  IF SELF:MODIFIED THEN
  DO:
      MesseBeskrivelse:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldValues","Messe",
                                              "WHERE MesseNr = '" + MesseNr:SCREEN-VALUE + "'","MesseBeskrivelse").  
      APPLY "ANY-PRINTABLE":U TO MesseNr.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frmDetalj
&Scoped-define SELF-NAME MesseNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL MesseNr C-Win
ON RETURN OF MesseNr IN FRAME frmDetalj /* MesseNr */
OR "TAB" OF MesseNr
DO:
  IF SELF:MODIFIED THEN
  DO:
      MesseBeskrivelse:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldValues","Messe",
                                              "WHERE MesseNr = '" + MesseNr:SCREEN-VALUE + "'","MesseBeskrivelse").  
      APPLY "ANY-PRINTABLE":U TO MesseNr.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Sk_p_alle_hovedgrupper
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Sk_p_alle_hovedgrupper C-Win
ON CHOOSE OF MENU-ITEM m_Sk_p_alle_hovedgrupper /* Vis alle hovedgrupper */
DO:
  DO WITH FRAME frmLinje:
    THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
    RUN JBoxSelector.w (THIS-PROCEDURE,0,
                        "HuvGr;Hg;HgBeskr,Avdeling;AvdelingNr;AvdelingNavn",
                        "where true, first Avdeling OF HuvGr NO-LOCK " + 
                           (IF cAvdelingRowIdList NE "" THEN
                              "WHERE CAN-DO('" + cAvdelingRowIdList + "',STRING(ROWID(Avdeling)))"
                            ELSE ""),
                        INPUT-OUTPUT cHuvGrRowIdList,
                        "Hg",
                        INPUT-OUTPUT cHuvGrIdList,
                        "","",
                        OUTPUT bOK).
    THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.


    IF bOk THEN DO:
      IF NUM-ENTRIES(cHuvGrRowidList) > 1 THEN 
        ASSIGN sokHg:SCREEN-VALUE   = "0"
               sokHgBeskr:SCREEN-VALUE = STRING(NUM-ENTRIES(cHuvGrRowidList)) + " av " +
                                         STRING(iSelectorSourcCount)
                                         .
      ELSE
        ASSIGN sokHg:SCREEN-VALUE   = cHuvGrIdList
               sokHgBeskr:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","HuvGr;HgBeskr","WHERE Hg = " + sokHg:SCREEN-VALUE).
      IF tbSearchOnValueChange THEN RUN OpenQuery.
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Vis_alle_leverandr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Vis_alle_leverandr C-Win
ON CHOOSE OF MENU-ITEM m_Vis_alle_leverandr /* Vis alle leverandører */
DO:
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "LevBas;levnr;levnamn;KjedeAvtale|Kjedeavtale|J/N"
/*                       + ";+LevBet1|CHARACTER|x(40)|levbas_kommentar1.p" */
/*                       + ";+LevBet2|CHARACTER|x(40)|levbas_kommentar2.p" */
/*                       + ";+LevBet3|CHARACTER|x(40)|levbas_kommentar3.p" */
/*                       + ";+LevBet4|CHARACTER|x(40)|levbas_kommentar4.p" */
                      ,
                      "where true",
                      INPUT-OUTPUT cLevBasRowIdList,
                      "Levnr",
                      INPUT-OUTPUT cLevBasIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk THEN DO WITH FRAME frmLinje:
    IF NUM-ENTRIES(cLevBasRowidList) > 1 THEN 
      ASSIGN sokLevNr:SCREEN-VALUE   = "0"
             sokLevNamn:SCREEN-VALUE = STRING(NUM-ENTRIES(cLevBasRowidList)) + " av " +
                                       STRING(iSelectorSourcCount)
                                       .
    ELSE
      ASSIGN sokLevNr:SCREEN-VALUE   = cLevBasIdList
             sokLevNamn:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","LevBas;LevNamn","WHERE LevNr = " + sokLevNr:SCREEN-VALUE).
    IF tbSearchOnValueChange THEN RUN OpenQuery.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Oppdatert
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Oppdatert C-Win
ON VALUE-CHANGED OF Oppdatert IN FRAME frmDetalj /* Oppdatert */
DO:
  IF INPUT Oppdatert = TRUE THEN
      ASSIGN
      OppdatDato:SCREEN-VALUE = STRING(TODAY)
      OppdatAv:SCREEN-VALUE   = USERID("SkoTex")
      .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ProfilNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ProfilNr C-Win
ON RETURN OF ProfilNr IN FRAME frmDetalj /* Prisprofil */
OR "TAB" OF ProfilNr
DO:
  IF SELF:MODIFIED THEN
  DO:
      KortNavn:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldValues","PrisProfil",
                                              "WHERE ProfilNr = '" + ProfilNr:SCREEN-VALUE + "'","KortNavn").  
      APPLY "ANY-PRINTABLE":U TO ProfilNr.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME ProfilNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ProfilNr C-Win
ON RETURN OF ProfilNr IN FRAME DEFAULT-FRAME /* Prisprofil */
OR "TAB" OF ProfilNr
DO:
  IF SELF:MODIFIED THEN
  DO:
      KortNavn:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldValues","PrisProfil",
                                              "WHERE ProfilNr = '" + ProfilNr:SCREEN-VALUE + "'","KortNavn").  
      APPLY "ANY-PRINTABLE":U TO ProfilNr.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frmLinje
&Scoped-define SELF-NAME rsButModus
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rsButModus C-Win
ON VALUE-CHANGED OF rsButModus IN FRAME frmLinje
DO:
  ASSIGN rsButModus.
  LinkOverlays().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokAktBeskrivelse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokAktBeskrivelse C-Win
ON RETURN OF sokAktBeskrivelse IN FRAME frmLinje
OR TAB OF sokAktBeskrivelse DO:
  IF sokAktBeskrivelse:MODIFIED AND tbSearchOnValueChange THEN RUN OpenQuery.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokAktNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokAktNr C-Win
ON F3 OF sokAktNr IN FRAME frmLinje /* Aktivitet */
OR F10 OF sokAktNr DO:
  APPLY "choose" TO btnAktivitet.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokAktNr C-Win
ON RETURN OF sokAktNr IN FRAME frmLinje /* Aktivitet */
OR TAB OF sokAktNr DO:
  IF sokAktNr:MODIFIED THEN DO: 
    sokAktBeskrivelse:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","Aktivitet;Beskrivelse","WHERE AktNr = " + sokAktNr:SCREEN-VALUE).
    IF sokAktBeskrivelse:SCREEN-VALUE NE "" AND tbSearchOnValueChange THEN DO:
      RUN OpenQuery.
      RETURN NO-APPLY.
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokArtikkelNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokArtikkelNr C-Win
ON RETURN OF sokArtikkelNr IN FRAME frmLinje /* SE art.nr */
OR TAB OF sokArtikkelnr DO:
  IF sokArtikkelnr:MODIFIED THEN DO:
    RUN OpenQuery.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokAvdelingNavn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokAvdelingNavn C-Win
ON RETURN OF sokAvdelingNavn IN FRAME frmLinje
OR TAB OF sokAvdelingNavn DO:
  IF sokAvdelingNavn:MODIFIED THEN DO: 
    ASSIGN cAvdelingRowIdList = ""
           cAvdelingIdList    = ""
           .    
    IF tbSearchOnValueChange THEN RUN OpenQuery.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokAvdelingNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokAvdelingNr C-Win
ON F3 OF sokAvdelingNr IN FRAME frmLinje /* Avdeling */
OR F10 OF sokAvdelingNr DO:
  APPLY "choose" TO btnAvdeling.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokAvdelingNr C-Win
ON RETURN OF sokAvdelingNr IN FRAME frmLinje /* Avdeling */
OR TAB OF sokAvdelingNr DO:
  IF sokAvdelingNr:MODIFIED THEN DO:
    sokAvdelingNavn:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","Avdeling;AvdelingNavn","WHERE AvdelingNr = " + sokAvdelingNr:SCREEN-VALUE).
    IF tbSearchOnValueChange THEN RUN OpenQuery.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokBeskr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokBeskr C-Win
ON RETURN OF sokBeskr IN FRAME frmLinje /* Utv.søk */
OR TAB OF sokBeskr DO:
  IF sokBeskr:MODIFIED AND tbSearchOnValueChange THEN RUN OpenQuery.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokFraEdato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokFraEdato C-Win
ON RETURN OF sokFraEdato IN FRAME frmLinje /* Endret dato */
OR TAB OF sokFraEdato DO:
  IF SELF:MODIFIED AND tbSearchOnValueChange THEN RUN OpenQuery.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokFraVPIdato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokFraVPIdato C-Win
ON RETURN OF sokFraVPIdato IN FRAME frmLinje /* VPI dato */
OR TAB OF sokFraVPIdato DO:
  IF SELF:MODIFIED AND tbSearchOnValueChange THEN RUN OpenQuery.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokHg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokHg C-Win
ON F3 OF sokHg IN FRAME frmLinje /* Hovedgruppe */
OR F10 OF sokHg DO:
  APPLY "choose" TO btnHuvGr.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokHg C-Win
ON RETURN OF sokHg IN FRAME frmLinje /* Hovedgruppe */
OR TAB OF sokHg DO:
  IF sokHg:MODIFIED THEN DO: 
    sokHgBeskr:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","HuvGr;HgBeskr","WHERE Hg = " + sokHg:SCREEN-VALUE).
    IF tbSearchOnValueChange THEN RUN OpenQuery.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokHgBeskr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokHgBeskr C-Win
ON RETURN OF sokHgBeskr IN FRAME frmLinje
OR TAB OF sokHgBeskr DO:
  IF sokHgBeskr:MODIFIED THEN DO: 
    ASSIGN cHuvGrRowIdList = ""
           cHuvGrIdList    = ""
           .
    IF tbSearchOnValueChange THEN RUN OpenQuery.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokLevFargKod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokLevFargKod C-Win
ON RETURN OF sokLevFargKod IN FRAME frmLinje /* LevFargeK */
OR TAB OF sokLevFargKod DO:
  IF sokLevFargKod:MODIFIED THEN DO:
    RUN OpenQuery.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokLevKod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokLevKod C-Win
ON RETURN OF sokLevKod IN FRAME frmLinje /* LevArtNr */
OR TAB OF sokLevKod DO:
  IF sokLevKod:MODIFIED THEN DO:
    RUN OpenQuery.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokLevNamn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokLevNamn C-Win
ON RETURN OF sokLevNamn IN FRAME frmLinje
OR TAB OF sokLevNamn DO:
  IF sokLevNamn:MODIFIED THEN DO:
    ASSIGN cLevBasRowIdList = ""
           cLevBasIdList    = ""
           .
    IF tbSearchOnValueChange THEN RUN OpenQuery.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokLevNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokLevNr C-Win
ON F3 OF sokLevNr IN FRAME frmLinje /* Leverandør */
OR F10 OF sokLevNr DO:
  APPLY "choose" TO btnLev.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokLevNr C-Win
ON RETURN OF sokLevNr IN FRAME frmLinje /* Leverandør */
OR TAB OF sokLevNr DO:
  IF sokLevNr:MODIFIED THEN DO:
    sokLevNamn:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","LevBas;LevNamn","WHERE LevNr = " + sokLevNr:SCREEN-VALUE).
    IF tbSearchOnValueChange THEN RUN OpenQuery.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokTilEdato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokTilEdato C-Win
ON RETURN OF sokTilEdato IN FRAME frmLinje
OR TAB OF sokTilEdato DO:
  IF SELF:MODIFIED AND tbSearchOnValueChange THEN RUN OpenQuery.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokTilVPIdato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokTilVPIdato C-Win
ON RETURN OF sokTilVPIdato IN FRAME frmLinje
OR TAB OF sokTilVPIdato DO:
  IF SELF:MODIFIED AND tbSearchOnValueChange THEN RUN OpenQuery.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokVg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokVg C-Win
ON F3 OF sokVg IN FRAME frmLinje /* Varegruppe */
OR F10 OF sokVg DO:
  APPLY "choose" TO btnVarGr.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokVg C-Win
ON RETURN OF sokVg IN FRAME frmLinje /* Varegruppe */
OR TAB OF sokVg DO:
  IF sokVg:MODIFIED THEN DO: 
    sokVgBeskr:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","VarGr;VgBeskr","WHERE Vg = " + sokVg:SCREEN-VALUE).
    IF tbSearchOnValueChange THEN RUN OpenQuery.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokVgBeskr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokVgBeskr C-Win
ON RETURN OF sokVgBeskr IN FRAME frmLinje
OR TAB OF sokVgBeskr DO:
  IF sokVgBeskr:MODIFIED THEN DO:
    ASSIGN cVarGrRowIdList = ""
           cVarGrIdList    = ""
           .
    IF tbSearchOnValueChange THEN RUN OpenQuery.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME TabStrip
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TabStrip C-Win OCX.MouseUp
PROCEDURE TabStrip.TabStrip.MouseUp .
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

  ASSIGN
      cState = DYNAMIC-FUNCTION('getToolbarState',hUpdToolBar)
      .
  IF CAN-DO('new,modified',cstate) THEN
  DO:
      MESSAGE "Du må lagre eller avbryte før tab kan byttes."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      ASSIGN
          iTab = 2 + 10
          .
      TabStripChanged(iTab).
      RETURN NO-APPLY.
  END.

  TabStripChanged(INT(COM-SELF:SelectedItem:Index)).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frmLinje
&Scoped-define SELF-NAME tbIkkeMerknad
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbIkkeMerknad C-Win
ON VALUE-CHANGED OF tbIkkeMerknad IN FRAME frmLinje /* Artikler uten merknad */
DO:
  RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbSearchOnValueChange
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbSearchOnValueChange C-Win
ON VALUE-CHANGED OF tbSearchOnValueChange IN FRAME frmLinje /* Start søk ved endring av filterkriterier */
DO:
  ASSIGN tbSearchOnValueChange.
  IF tbSearchOnValueChange THEN
    btnStartSearch:HIDDEN = TRUE.
  ELSE 
    btnStartSearch:HIDDEN = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbUpdateStat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbUpdateStat C-Win
ON VALUE-CHANGED OF tbUpdateStat IN FRAME frmLinje /* Vis statistikk */
DO:
  DEF VAR hWidget AS HANDLE NO-UNDO.
  DEF VAR iOrgWidthPixels AS INT NO-UNDO.
  DEF VAR iOrgHeightPixels AS INT NO-UNDO.

  ASSIGN tbUpdateStat.

  ASSIGN tbForventet:HIDDEN IN FRAME frmLinje = IF tbUpdateStat THEN FALSE ELSE TRUE
         btnStartStat:HIDDEN = IF tbUpdateStat THEN FALSE ELSE TRUE
         .

  DO ix = 1 TO NUM-ENTRIES(cStatFieldHandles):
    hWidget = WIDGET-HANDLE(ENTRY(ix,cStatFieldHandles)).
    ASSIGN hWidget:HIDDEN = IF tbUpdateStat THEN FALSE ELSE TRUE.
  END.
  DO ix = 1 TO NUM-ENTRIES(cStatLabelHandles):
    hWidget = WIDGET-HANDLE(ENTRY(ix,cStatLabelHandles)).
    ASSIGN hWidget:HIDDEN = IF tbUpdateStat THEN FALSE ELSE TRUE.
  END.

  ASSIGN FRAME frmLinje:WIDTH-PIXELS  = FRAME frmLinje:WIDTH-PIXELS
         FRAME frmLinje:HEIGHT-PIXELS = FRAME frmLinje:HEIGHT-PIXELS
         .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbValgtInndeling
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbValgtInndeling C-Win
ON VALUE-CHANGED OF tbValgtInndeling IN FRAME frmLinje /* Artikler med inndeling */
DO:
  RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frmDetalj
&Scoped-define SELF-NAME VareBokType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL VareBokType C-Win
ON RETURN OF VareBokType IN FRAME frmDetalj /* Vareboktype */
OR "TAB" OF VarebokType
DO:
  IF SELF:MODIFIED THEN
  DO:
      BeskrivelseVarebokType:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldValues","VarebokType",
                                              "WHERE VarebokType = '" + VarebokType:SCREEN-VALUE + "'","BeskrivelseVarebokType").  
      APPLY "ANY-PRINTABLE":U TO VarebokType.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}
       hWindow                       = {&WINDOW-NAME}.

DYNAMIC-FUNCTION("EmbedMe" IN SOURCE-PROCEDURE,THIS-PROCEDURE) NO-ERROR.
/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
  IF NOT DYNAMIC-FUNCTION("DoCleanUpObjects",THIS-PROCEDURE:CURRENT-WINDOW) THEN
      RETURN NO-APPLY "cancel".
  DYNAMIC-FUNCTION("setCleanUpResize",THIS-PROCEDURE:CURRENT-WINDOW).
  IF VALID-HANDLE(hVisBilde) THEN APPLY "close" TO hVisBilde.
  IF VALID-HANDLE(hArtikkelkort) THEN APPLY "close" TO hArtikkelkort.
  IF VALID-HANDLE(hOppdaterViaExcel) THEN APPLY "close" TO hOppdaterViaExcel.

  IF bUtvalgIsMaster AND VALID-HANDLE(hUtvalg) THEN 
    RUN InvalidateHandle IN hUtvalg (THIS-PROCEDURE).
  ELSE IF NOT bUtvalgIsMaster AND VALID-HANDLE(hUtvalg) THEN
    APPLY "close" TO hUtvalg.

  IF VALID-HANDLE(hArtBilde) THEN APPLY "close" TO hArtBilde.

  PUBLISH "InvalidateHandle" (THIS-PROCEDURE).
  RUN disable_UI.
  RETURN.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

ON F2,ALT-S OF hWindow ANYWHERE DO:
  DEF VAR iTmpTab     AS INT NO-UNDO.
  
  IF iTab = 3 THEN DO:
    RUN d-hsok.w (OUTPUT fArtikkelNr,""). 
    IF fArtikkelNr = ? OR fArtikkelNr = 0 THEN
        RETURN NO-APPLY.
    RUN newVareBokLinje.
  END.
  ELSE IF iTab < 3 THEN DO:
    RUN d-hsok.w (OUTPUT fArtikkelNr,"JA"). 
    IF fArtikkelNr = ? OR fArtikkelNr = 0 THEN 
      RETURN NO-APPLY.

    DYNAMIC-FUNCTION("setAttribute",hBrowseListe,"prescanqueryfilter",
                       "VarebokLinje WHERE ArtikkelNr = " + STRING(fArtikkelNr) + ",FIRST VarebokHode OF VarebokLinje NO-LOCK").
    DYNAMIC-FUNCTION("setCurrentObject",hBrowseListe).
    RUN OpenQuery.
    DYNAMIC-FUNCTION("setAttribute",hBrowseListe,"prescanqueryfilter","").
    IF hBrowseListe:QUERY:NUM-RESULTS > 0 THEN DO:
      sokArtikkelNr:SCREEN-VALUE IN FRAME frmLinje = STRING(fArtikkelNr).
      ASSIGN iTmpTab = iTab
             iTab    = 3.
      APPLY "choose" TO btnStartSearch IN FRAME frmLinje.
      iTab = iTmpTab.
    END.
  END.
  ELSE
    RETURN NO-APPLY.
END.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  {lng.i}
  RUN enable_UI.
  &IF DEFINED(UIB_is_Running) NE 0 &THEN
    RUN InitializeObject.
    RUN MoveToTop.
  &ENDIF

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

{incl/wintrigg.i}

ON 'window-resized':U OF {&WINDOW-NAME} DO:
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"Resize","").
  IF iTab = 3 AND VALID-HANDLE(hBrowseLinje) THEN
    APPLY "value-changed" TO hBrowseLinje.
  ELSE IF iTab = 1 THEN DO:
    FRAME frmListe:MOVE-TO-TOP().
    FRAME frmFilter:MOVE-TO-TOP().
  END.
  ELSE IF iTab = 2 THEN 
    FRAME frmDetalj:MOVE-TO-TOP().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AktiverKalkyle C-Win 
PROCEDURE AktiverKalkyle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF NOT hFieldMap:AVAIL THEN RETURN.

IF NOT DYNAMIC-FUNCTION("RunProc","vareboklinje_aktiver_kalyle.p",
                         STRING(hFieldMap:BUFFER-FIELD("VarebokNr"):BUFFER-VALUE),
                         ?) THEN
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil i aktivering av kalkyle",""). 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AktiverKalkyleValgte C-Win 
PROCEDURE AktiverKalkyleValgte :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cArtNrList AS CHAR NO-UNDO.

IF NOT hFieldMap:AVAIL THEN RETURN.

DO ix = 1 TO hBrowseLinje:NUM-SELECTED-ROWS:
  IF hBrowseLinje:FETCH-SELECTED-ROW(ix) THEN 
    cArtNrList = cArtNrList + STRING(hFieldMapLinje:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) + ",". 
END.
IF NOT DYNAMIC-FUNCTION("RunProc","vareboklinje_aktiver_kalyle.p",
                         STRING(hFieldMap:BUFFER-FIELD("VarebokNr"):BUFFER-VALUE) + ";" + " AND CAN-DO('" + TRIM(cArtNrList,",") + "',STRING(VarebokLinje.ArtikkelNr))",
                         ?) THEN
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil i aktivering av kalkyle",""). 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AktiverVPIAlle C-Win 
PROCEDURE AktiverVPIAlle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iSend    AS INT  NO-UNDO.
DEF VAR cButList AS CHAR NO-UNDO.
DEF VAR cDummy   AS CHAR NO-UNDO.
    
IF NOT hFieldMap:AVAIL THEN RETURN.

/* Spørre bruker om det VPI'en også skal sendes til butikkene. */
iSend = DYNAMIC-FUNCTION("DoMessage",0,3,"Skal varene som legges i VPI register også sendes til butikkene?" /* + CHR(10) + */
/*                                     "Sending vil skje til alle butikker." */
                                   ,"","").

IF iSend = 2 THEN RETURN.
IF iSend = 6 THEN DO:
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "Butiker;Butik;ButNamn",
                      "where Butiker.Sentrallager = true and Butiker.ApningsDato <> ? and Butiker.NedlagtDato = ?",
                      INPUT-OUTPUT cDummy,  /* Ingen valgt på forhånd */
                      "Butik",
                      INPUT-OUTPUT cButList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.
  IF cButList = "" AND DYNAMIC-FUNCTION("DoMessage",0,1,"Ingen butikker er valgt - send til alle?","","") NE 1
     THEN RETURN.
  iSend = 1. /* Settes for å slippe å endre server-rutine. Endret over for å få ja/nei/avbr spm */
END.

/* Denne skal bruker kunne initiere via taggliste mot butikkregister. */
/* Blank liste = alle butikker.                                       */
/* cButList = "1". */

IF NOT DYNAMIC-FUNCTION("RunProc","vareboklinje_aktiver_vpi.p",
                         STRING(hFieldMap:BUFFER-FIELD("VarebokNr"):BUFFER-VALUE) + ";;" +
                         STRING(iSend) + ";" + cButList,
                         ?) THEN
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil i aktivering av kalkyle",""). 
ELSE MESSAGE DYNAMIC-FUNCTION("getTransactionMessage")
      VIEW-AS ALERT-BOX INFO BUTTONS OK.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AktiverVPIValgte C-Win 
PROCEDURE AktiverVPIValgte :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cArtNrList AS CHAR NO-UNDO.
DEF VAR iSend    AS INT  NO-UNDO.
DEF VAR cButList AS CHAR NO-UNDO.
DEF VAR cDummy   AS CHAR NO-UNDO.

IF NOT hFieldMap:AVAIL THEN RETURN.

iSend = DYNAMIC-FUNCTION("DoMessage",0,3,"Skal varene som legges i VPI register også sendes til butikkene?" /* + CHR(10) + */
/*                                     "Sending vil skje til alle butikker." */
                                   ,"","").

IF iSend = 2 THEN RETURN.
ELSE IF iSend = 6 THEN DO:
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "Butiker;Butik;ButNamn",
                      "WHERE true",
                      INPUT-OUTPUT cDummy,  /* Ingen valgt på forhånd */
                      "Butik",
                      INPUT-OUTPUT cButList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.
  IF cButList = "" AND DYNAMIC-FUNCTION("DoMessage",0,1,"Ingen butikker er valgt - send til alle?","","") NE 1
     THEN RETURN.
  iSend = 1. /* Settes for å slippe å endre server-rutine. Endret over for å få ja/nei/avbr spm */
END.

DO ix = 1 TO hBrowseLinje:NUM-SELECTED-ROWS:
  IF hBrowseLinje:FETCH-SELECTED-ROW(ix) THEN 
    cArtNrList = cArtNrList + STRING(hFieldMapLinje:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) + ",". 
END.                              
IF NOT DYNAMIC-FUNCTION("RunProc","vareboklinje_aktiver_vpi.p",
                         STRING(hFieldMap:BUFFER-FIELD("VarebokNr"):BUFFER-VALUE) + ";" + " AND CAN-DO('" + TRIM(cArtNrList,",") + "',STRING(VarebokLinje.ArtikkelNr))" + 
                         ";" + STRING(iSend) + ";" + cButList,
                         ?) THEN
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil i aktivering av kalkyle",""). 
ELSE MESSAGE DYNAMIC-FUNCTION("getTransactionMessage")
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AnbefaltPrisRecord C-Win 
PROCEDURE AnbefaltPrisRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR ocValue     AS CHAR NO-UNDO.
DEF VAR iConfirm    AS INT NO-UNDO.

iReturn = 0.
RUN JBoxBrowseMsgUpdSelVal.w ("Oppdater veiledende pris",
                              hBrowseLinje:NUM-SELECTED-ROWS,
                              DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"Totalcount"),
                              "DECIMAL|>>>,>>9.99|Veil.pris",
                              OUTPUT ocValue, 
                              OUTPUT iReturn).
IF DEC(ocValue) = 0 AND iReturn NE 0 THEN 
  iConfirm = DYNAMIC-FUNCTION("DoMessage",0,1,"Bekreft at veiledende pris skal settes til 0","Bekreft","").

IF iReturn NE 0 AND iConfirm NE 2 THEN
  EndreMangeLinjer("AnbefaltPris",ocValue).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AntallRecord C-Win 
PROCEDURE AntallRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR ocValue     AS CHAR NO-UNDO.
DEF VAR iConfirm    AS INT NO-UNDO.

iReturn = 0.
RUN JBoxBrowseMsgUpdSelVal.w ("Oppdater antall forhåndskjøp",
                              hBrowseLinje:NUM-SELECTED-ROWS,
                              DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"Totalcount"),
                              "DECIMAL|>>>,>>9.99|Antall forh.kjøp",
                              OUTPUT ocValue, 
                              OUTPUT iReturn).
IF DEC(ocValue) = 0 AND iReturn NE 0 THEN 
  iConfirm = DYNAMIC-FUNCTION("DoMessage",0,1,"Bekreft at antall forhåndskjøp skal settes til 0","Bekreft","").

IF iReturn NE 0 AND iConfirm NE 2 THEN
  EndreMangeLinjer("Antall",ocValue).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ArtBasEndret C-Win 
PROCEDURE ArtBasEndret :
/*------------------------------------------------------------------------------
  Purpose:    Fange endring av artikkelegenskaper 
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ifArtikkelnr AS DEC NO-UNDO.

/* DYNAMIC-FUNCTION("runproc","update_varebok_from_artbas.p",                            */
/*                            STRING(ifArtikkelnr) + "," +                               */
/*                            STRING(hFieldMap:BUFFER-FIELD("VarebokNr"):BUFFER-VALUE),  */
/*                            ?).                                                        */

/* DYNAMIC-FUNCTION("refreshRowids",hBrowseLinje,hFieldMapLinje:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE).  */
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
IF NOT VALID-HANDLE(hArtikkelkort) THEN
  RUN w-vartkor.w  PERSIST SET hArtikkelkort (DYNAMIC-FUNCTION("getRecId","ArtBas",hFieldMapLinje:BUFFER-FIELD("RowIdent2"):BUFFER-VALUE), "ENDRE," + STRING(THIS-PROCEDURE)).
ELSE
  RUN ByttArtikkel IN hArtikkelkort (hFieldMapLinje:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ArtPrintRecord C-Win 
PROCEDURE ArtPrintRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF NOT hFieldMap:AVAIL THEN RETURN.

IF DYNAMIC-FUNCTION("runProc","varebok_artprint.p",
                    DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"BaseQuery") +
                    DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"QueryFilter") +
                    IF DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"QuerySort") NE "" THEN 
                      ";" + DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"QuerySort") + " " + DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"QueryDesc")
                    ELSE ";"
                    ,httRapport) THEN
  ArtPrintToExcel(DYNAMIC-FUNCTION("getTransactionMessage"),FALSE).
ELSE DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BildeGrid C-Win 
PROCEDURE BildeGrid :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cArtNrList    AS CHAR NO-UNDO.

iReturn = DYNAMIC-FUNCTION("DoMessage",0,1,"Bekreft overføring av &1 artikler til bildegrid","",
                 IF hBrowseLinje:NUM-SELECTED-ROWS > 0 THEN
                   STRING(hBrowseLinje:NUM-SELECTED-ROWS)
                 ELSE
                   DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"TotalCount")
                 ).
IF iReturn = 2 THEN RETURN.

IF hBrowseLinje:NUM-SELECTED-ROWS > 0 THEN DO:
  DO ix = 1 TO hBrowseLinje:NUM-SELECTED-ROWS:
    IF hBrowseLinje:FETCH-SELECTED-ROW(ix) THEN
      cArtNrList = cArtNrList + STRING(hFieldMapLinje:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) + ",".
  END.
  RUN DynBildeGrid.p (hFieldMapLinje:TABLE-HANDLE,"FOR EACH vareboklinje WHERE CAN-DO('" + TRIM(cArtNrList,",") + "',STRING(ArtikkelNr))","Artikkelnr").
END.
ELSE RUN DynBildeGrid.p (hFieldMapLinje:TABLE-HANDLE,"FOR EACH vareboklinje","Artikkelnr").

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

OCXFile = SEARCH( "varebokhode.wrx":U ).
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
ELSE MESSAGE "varebokhode.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DefaultActionBrowse C-Win 
PROCEDURE DefaultActionBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowseSort THEN 
  RUN EditSortRecord.
ELSE IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowseLinje THEN DO:
  IF hFieldMapLinje:AVAIL THEN
    RUN Artikkelkort.
  ELSE APPLY "choose" TO btnStartSearch IN FRAME frmLinje.
END.
ELSE IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowseListe THEN
  TabStripChanged(13).
ELSE RUN SUPER.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deleteRecord C-Win 
PROCEDURE deleteRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR plOk AS LOG INITIAL FALSE NO-UNDO.
DEF VAR ocValue     AS CHAR NO-UNDO.

IF CAN-DO("1,2",STRING(iTab)) THEN
DO:
    /* Det kommer et spørsmål før dette fra MyDeleteMessage. */
    MESSAGE 
        "Vil du slette vareboken?" SKIP
        "Hele vareboken med ALLE varelinjene vil bli slettet."
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO 
        UPDATE plOk.
    IF plOk <> TRUE THEN
        RETURN NO-APPLY.
   
    RUN SUPER.
END.
ELSE IF iTab = 3 THEN DO:
  iReturn = 0.
  RUN JBoxBrowseMsgUpdateVal.w ("Slette artikler fra varebok og tilh. messebok (hvis ikke reg. best)",
                                hBrowseLinje:NUM-SELECTED-ROWS,
                                IF INT(DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"Totalcount")) LT INT(DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"rowstobatch")) THEN
                                  INT(DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"Totalcount"))
                                ELSE 99999,
                                "",
                                OUTPUT ocValue, 
                                OUTPUT iReturn).
  IF iReturn = 0 THEN RETURN.

  IF DYNAMIC-FUNCTION("getFieldValues","VarebehHode","WHERE kilde = " + STRING(hFieldMap:BUFFER-FIELD("VarebokNr"):BUFFER-VALUE),"VarebehNr") NE ? THEN DO:
    RUN dVarebokSlettAdvarsel.w (OUTPUT bOk).
    IF NOT bOk THEN RETURN.
  END.

  IF iReturn = 1 THEN DO:
    IF NOT DYNAMIC-FUNCTION("processQuery",hBrowseLinje,"vareboklinje_slett.p","") THEN DO:
      ocValue = DYNAMIC-FUNCTION("getTransactionMessage"). 
      DYNAMIC-FUNCTION("DoMessage",0,0,ocValue,"","").
    END.
    APPLY "VALUE-CHANGED":U TO hBrowseListe.
  END.
  ELSE IF iReturn = 2 THEN DO:
    IF NOT DYNAMIC-FUNCTION("processSelectedRows",hBrowseLinje,"vareboklinje_slett.p","") THEN DO:
      ocValue = DYNAMIC-FUNCTION("getTransactionMessage"). 
      DYNAMIC-FUNCTION("DoMessage",0,0,ocValue,"","").
    END.
    APPLY "VALUE-CHANGED":U TO hBrowseListe.
  END.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DeleteSortRecord C-Win 
PROCEDURE DeleteSortRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF DYNAMIC-FUNCTION("DoMessage",0,4,"Slett inndeling " + hBrowseSort:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("SortId"):BUFFER-VALUE + CHR(10) +
                                    "(alle koblinger fra artikkel til inndeling blir også slettet)","","") = 6 THEN DO:
  IF DYNAMIC-FUNCTION("runproc","levsort_delete.p",STRING(hBrowseSort:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("LevNr"):BUFFER-VALUE) + "|"
                                                 + hBrowseSort:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("SortId"):BUFFER-VALUE,?) THEN DO:
    DYNAMIC-FUNCTION("RefreshRowids",hBrowseLinje,hFieldMapLinje:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE).
    APPLY "value-changed" TO hBrowseLinje.
  END.
  ELSE DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DeselectRecord C-Win 
PROCEDURE DeselectRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF iTab = 3 THEN
  hBrowseLinje:DESELECT-ROWS() NO-ERROR.
ELSE 
  hBrowseListe:DESELECT-ROWS() NO-ERROR.
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
DEF VAR cMerknadList        AS CHAR NO-UNDO.
DEF VAR cSortimentList      AS CHAR NO-UNDO.
DEF VAR cKampanjeukeList    AS CHAR NO-UNDO.
DEF VAR cKampanjestotteList AS CHAR NO-UNDO.
DEF VAR cLagerkodeList      AS CHAR NO-UNDO.

DYNAMIC-FUNCTION("DoLockWindow",THIS-PROCEDURE:CURRENT-WINDOW).
DYNAMIC-FUNCTION("setAttribute",SESSION,"userkeepswindowlocked","yes").

DYNAMIC-FUNCTION('setAttribute',hUpdToolBar,'DisabledEvents','OppdaterViaExcel').

IF DYNAMIC-FUNCTION("getCurrentObject") NE hBrowseListe AND
   DYNAMIC-FUNCTION("getCurrentObject") NE hBrowseLinje AND
   iTab < 3 THEN
 
DYNAMIC-FUNCTION("setCurrentObject",hBrowseLinje).

IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowseListe AND hFieldMap:AVAIL THEN DO WITH FRAME frmLinje:
  DO ix = 1 TO NUM-ENTRIES(hFieldMap:BUFFER-FIELD("Oppmerking"):BUFFER-VALUE,"¤"):
    cMerknadList = cMerknadList + 
                   ENTRY(ix,hFieldMap:BUFFER-FIELD("Oppmerking"):BUFFER-VALUE,"¤") + "|" +
                   ENTRY(ix,hFieldMap:BUFFER-FIELD("Oppmerking"):BUFFER-VALUE,"¤") + "|".
  END.
  cOppmerking = TRIM(cMerknadList,"|").
/*   hBrwOLMerk:LIST-ITEM-PAIRS = IF cMerknadList NE "" THEN "||" + TRIM(cMerknadList,"|") ELSE "|". */

  DO ix = 1 TO NUM-ENTRIES(hFieldMap:BUFFER-FIELD("Sortimentkoder"):BUFFER-VALUE,"¤"):
    cSortimentList = cSortimentList + 
                   ENTRY(ix,hFieldMap:BUFFER-FIELD("Sortimentkoder"):BUFFER-VALUE,"¤") + "|" +
                   ENTRY(ix,hFieldMap:BUFFER-FIELD("Sortimentkoder"):BUFFER-VALUE,"¤") + "|".
  END.
  hBrwOSortiment:LIST-ITEM-PAIRS = IF cSortimentList NE "" THEN "||" + TRIM(cSortimentList,"|") ELSE "|".

  DO ix = 1 TO NUM-ENTRIES(hFieldMap:BUFFER-FIELD("Kampanjeuker"):BUFFER-VALUE,"¤"):
    cKampanjeukeList = cKampanjeukeList + 
                   ENTRY(ix,hFieldMap:BUFFER-FIELD("Kampanjeuker"):BUFFER-VALUE,"¤") + "|" +
                   ENTRY(ix,hFieldMap:BUFFER-FIELD("Kampanjeuker"):BUFFER-VALUE,"¤") + "|".
  END.
  hBrwOKampanjeuke:LIST-ITEM-PAIRS = IF cKampanjeukeList NE "" THEN "||" + TRIM(cKampanjeukeList,"|") ELSE "|".

  DO ix = 1 TO NUM-ENTRIES(hFieldMap:BUFFER-FIELD("Kampanjestotte"):BUFFER-VALUE,"¤"):
    cKampanjestotteList = cKampanjestotteList + 
                   ENTRY(ix,hFieldMap:BUFFER-FIELD("Kampanjestotte"):BUFFER-VALUE,"¤") + "|" +
                   ENTRY(ix,hFieldMap:BUFFER-FIELD("Kampanjestotte"):BUFFER-VALUE,"¤") + "|".
  END.
  hBrwOKampanjestotte:LIST-ITEM-PAIRS = IF cKampanjestotteList NE "" THEN "||" + TRIM(cKampanjestotteList,"|") ELSE "|".

  DO ix = 1 TO NUM-ENTRIES(hFieldMap:BUFFER-FIELD("Lagerkoder"):BUFFER-VALUE,"¤"):
    cLagerkodeList = cLagerkodeList + 
                   ENTRY(ix,hFieldMap:BUFFER-FIELD("Lagerkoder"):BUFFER-VALUE,"¤") + "|" +
                   ENTRY(ix,hFieldMap:BUFFER-FIELD("Lagerkoder"):BUFFER-VALUE,"¤") + "|".
  END.
  hBrwOLagerkode:LIST-ITEM-PAIRS = IF cLagerkodeList NE "" THEN "||" + TRIM(cLagerkodeList,"|") ELSE "|".
END.
ELSE IF hFieldMapLinje:AVAIL THEN DO:
  IF hBrwOLMerk:LOOKUP(hFieldMapLinje:BUFFER-FIELD("LinjeMerknad"):BUFFER-VALUE) = 0 THEN
     hBrwOLMerk:LIST-ITEM-PAIRS = hBrwOLMerk:LIST-ITEM-PAIRS + "|" + hFieldMapLinje:BUFFER-FIELD("LinjeMerknad"):BUFFER-VALUE + "|" + hFieldMapLinje:BUFFER-FIELD("LinjeMerknad"):BUFFER-VALUE.
END.

RUN SUPER.

IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowseListe THEN DO WITH FRAME {&FRAME-NAME}:
  IF hFieldMap:AVAIL THEN DO:
    ASSIGN KortNavn:SCREEN-VALUE           = hFieldMap:BUFFER-FIELD("KortNavn"):BUFFER-VALUE
           MesseBeskrivelse:SCREEN-VALUE   = hFieldMap:BUFFER-FIELD("MesseBeskrivelse"):BUFFER-VALUE
           MesseNr:SCREEN-VALUE            = hFieldMap:BUFFER-FIELD("MesseNr"):STRING-VALUE
           ProfilNr:SCREEN-VALUE           = hFieldMap:BUFFER-FIELD("ProfilNr"):STRING-VALUE
           VareBokBeskrivelse:SCREEN-VALUE = hFieldMap:BUFFER-FIELD("VareBokBeskrivelse"):BUFFER-VALUE
           VareBokNr:SCREEN-VALUE          = hFieldMap:BUFFER-FIELD("VareBokNr"):STRING-VALUE
           BeskrivelseVareBokType:SCREEN-VALUE = hFieldMap:BUFFER-FIELD("BeskrivelseVareBokType"):BUFFER-VALUE
           VareBokType:SCREEN-VALUE        = hFieldMap:BUFFER-FIELD("VareBokType"):STRING-VALUE
           btnSokProfilNr:SENSITIVE IN FRAME frmDetalj = TRUE
           btnSokMesseNr:SENSITIVE IN FRAME frmDetalj = TRUE
           .
  END.
  ELSE 
    ASSIGN KortNavn:SCREEN-VALUE           = ""
           MesseBeskrivelse:SCREEN-VALUE   = ""
           MesseNr:SCREEN-VALUE            = ""
           ProfilNr:SCREEN-VALUE           = ""
           VareBokBeskrivelse:SCREEN-VALUE = ""
           VareBokNr:SCREEN-VALUE          = ""
           BeskrivelseVareBokType:SCREEN-VALUE = ""
           VareBoktype:SCREEN-VALUE        = ""
           btnSokProfilNr:SENSITIVE IN FRAME frmDetalj = FALSE
           btnSokMesseNr:SENSITIVE IN FRAME frmDetalj = FALSE
           .
END.

IF iTab = 3 THEN DO WITH FRAME frmLinje:
  DYNAMIC-FUNCTION('setAttribute',hUpdToolBar,'DisabledEvents','').
  RUN VisMiniBilde IN hArtBilde 
      (IF hFieldMapLinje:AVAIL THEN hFieldMapLinje:BUFFER-FIELD("BildNr"):BUFFER-VALUE 
       ELSE 0).
  IF VALID-HANDLE(hArtikkelkort) THEN
    RUN ByttArtikkel IN hArtikkelkort (IF hFieldMapLinje:AVAIL THEN hFieldMapLinje:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE
                                       ELSE 0).

  IF hFieldMapLinje:AVAIL THEN DO:
    hMenuItemNewSort:SENSITIVE = TRUE.
    fi-cAnnenVarebok:SCREEN-VALUE = hFieldMapLinje:BUFFER-FIELD("AndreVareboker"):BUFFER-VALUE.   

    IF DYNAMIC-FUNCTION("runproc","varebok_tilgjsort.p",
                        STRING(hFieldMapLinje:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) + "," +
                        STRING(hFieldMapLinje:BUFFER-FIELD("LevNr"):BUFFER-VALUE)
                        ,?) THEN
/*     IF hFieldMapLinje:BUFFER-FIELD("TilgjSort"):BUFFER-VALUE = "*" THEN */
      ASSIGN hMenuItemSelectSort:SENSITIVE = TRUE
             hMenuItemNewSort:SENSITIVE    = FALSE.
    ELSE
      ASSIGN hMenuItemSelectSort:SENSITIVE = FALSE
             hMenuItemNewSort:SENSITIVE    = TRUE.

    IF DYNAMIC-FUNCTION("runproc","artbas_gyldig_str.p",STRING(hFieldMapLinje:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE),?) THEN
      AlfaFordeling:SCREEN-VALUE = DYNAMIC-FUNCTION("getTransactionMessage").
    ELSE
      AlfaFordeling:SCREEN-VALUE = hFieldMapLinje:BUFFER-FIELD("AlfaFordeling"):BUFFER-VALUE.
  END.
END.
SettUtvalgSensitive(IF bUtvalgIsMaster THEN FALSE ELSE TRUE).
SettHentUtvalgSensitive(IF VALID-HANDLE(hUtvalg) THEN TRUE ELSE FALSE).

DYNAMIC-FUNCTION("setAttribute",SESSION,"userkeepswindowlocked","").
DYNAMIC-FUNCTION("DoLockWindow",?).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EanPrintRecord C-Win 
PROCEDURE EanPrintRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cRowIdList  AS CHAR NO-UNDO.
DEF VAR cIdList     AS CHAR NO-UNDO.
DEF VAR iy          AS INT  NO-UNDO.

IF NOT hFieldMap:AVAIL THEN RETURN.

RUN JBoxSelector.w (THIS-PROCEDURE,0,
                    "_field"                               /* Dummy buffer */
                    + ";!_field-name"                      /* <- must invoke a dummy non-visual field to avoid initial sort since calculated fields normally arn't sortable */
                    + ";+Description|CHARACTER|x(30)||Velg merknad for skravering"
                    ,"where false",
                    INPUT-OUTPUT cRowIdList,
                    "Description",
                    INPUT-OUTPUT cIdList,
                    "","",
                    OUTPUT bOK).

IF bOk THEN 
  cPrintMark = REPLACE(cIdList,"|",",").
ELSE RETURN.

IF DYNAMIC-FUNCTION("runProc","varebok_eanprint.p",
                     DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"BaseQuery") +
                     DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"QueryFilter") +
                     IF DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"QuerySort") NE "" THEN 
                       ";" + DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"QuerySort") + " " + DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"QueryDesc")
                     ELSE ""
                    ,httRapport) THEN
  EanPrintToExcel(DYNAMIC-FUNCTION("getTransactionMessage"),FALSE).
ELSE DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EANRapportValgteRecord C-Win 
PROCEDURE EANRapportValgteRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cRowIdList AS CHAR NO-UNDO.

DO ix = 1 TO hBrowseLinje:NUM-SELECTED-ROWS:
  IF hBrowseLinje:FETCH-SELECTED-ROW(ix) THEN
    cRowIdList = cRowIdList + hFieldMapLinje:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE + ",".
END.
cRowIdList = TRIM(cRowIdList,",").
IF cRowIdList NE "" THEN DO:
  IF DYNAMIC-FUNCTION("runProc","varebok_eanprint.p",
                       DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"BaseQuery") +
                       DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"QueryFilter") +
                       (IF DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"QuerySort") NE "" THEN 
                         ";" + DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"QuerySort") + " " + DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"QueryDesc")
                        ELSE ";") + ";" + cRowIdList
                      ,httRapport) THEN
    EanPrintToExcel(DYNAMIC-FUNCTION("getTransactionMessage"),FALSE).
  ELSE DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EanTestPrintRecord C-Win 
PROCEDURE EanTestPrintRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF NOT hFieldMap:AVAIL THEN RETURN.
IF DYNAMIC-FUNCTION("runProc","varebok_eanprint.p",
                     DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"BaseQuery") +
                     DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"QueryFilter") + 
                     IF DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"QuerySort") NE "" THEN 
                       ";" + DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"QuerySort") + " " + DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"QueryDesc")
                     ELSE ""
                    ,httRapport) THEN
  EanPrintToExcel(DYNAMIC-FUNCTION("getTransactionMessage"),TRUE).
ELSE DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE editRecord C-Win 
PROCEDURE editRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  CASE iTab:
      WHEN 1 THEN APPLY 'default-action':U TO hBrowseListe.
      WHEN 3 THEN DO:          
        IF hFieldMapLinje:AVAIL THEN
          RUN Artikkelkort.
      END.
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EditSortRecord C-Win 
PROCEDURE EditSortRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VAR rLevBas    AS RECID NO-UNDO.
DEFINE VAR wLevSort   AS RECID NO-UNDO.

rLevBas = DYNAMIC-FUNCTION("getRecId","levbas","WHERE LevNr = " + STRING(hBrowseSort:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("LevNr"):BUFFER-VALUE)).
IF rLevBas = ? THEN DO:
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
  RETURN.
END.

wLevSort = DYNAMIC-FUNCTION("getRecId","LevSort",hBrowseSort:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("RowIdent2"):BUFFER-VALUE).
IF wLevSort = ? THEN DO:
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
  RETURN.
END.


/* Parameter 4 används vid registrering från w-gridord,
   för att ange StrType */
RUN d-vlevsort.w (INPUT-OUTPUT wLevSort,"ENDRE",rLevBas,hFieldMapLinje:BUFFER-FIELD("StrTypeId"):BUFFER-VALUE).
IF wLevSort <> ? THEN 
  APPLY "value-changed" TO hBrowseLinje.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EksportPricat C-Win 
PROCEDURE EksportPricat :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cFileName          AS CHAR       NO-UNDO.
DEF VAR chExcelApplication AS COM-HANDLE NO-UNDO.
DEF VAR cKatalog           AS CHAR       NO-UNDO.

{syspara.i 1 1 51 cKatalog}
cKatalog = RIGHT-TRIM(cKatalog,'\').

IF NOT hFieldMap:AVAIL THEN RETURN.
ELSE cFileName = 'VB' + STRING(hFieldMap:BUFFER-FIELD("VarebokNr"):BUFFER-VALUE) + ".csv".

/*SYSTEM-DIALOG GET-DIR cFileName FILTERS ".csv" "*.csv" USE-FILENAME UPDATE bOK. */

SYSTEM-DIALOG GET-DIR cKatalog INITIAL-DIR cKatalog RETURN-TO-START-DIR. 
ASSIGN
    cKatalog = RIGHT-TRIM(cKatalog,'\')
    bOk      = TRUE.

IF bOK THEN DO:
  cFileName = cKatalog + '\' + RIGHT-TRIM(cFileName,".csv") + ".csv".

  IF NOT DYNAMIC-FUNCTION("RunProc","varebok_eksportertilpricat.p",    
                           cFileName + "¤" + STRING(hFieldMap:BUFFER-FIELD("VarebokNr"):BUFFER-VALUE)
                          /*
                           DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"buffersandfields") + "¤" +
                           DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"allviewfields") + "¤" +
                           DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"BaseQuery") +
/*                            (IF DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"QueryFilter") NE ""                          */
/*                               AND DYNAMIC-FUNCTION("DoMessage",0,1,"Benytt gjeldende filter ved eksport?","","") = 1 THEN */
                             " " + DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"QueryFilter") +
/*                             ELSE "") + */
                           DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"QueryJoin") + " BY " +
                           DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"QuerySort") + " " + DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"QueryDesc")
                           */
                           ,?) THEN
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil ved eksport av varebok",""). 
  ELSE DO:
    MESSAGE 'VPI lagt ut med en fil pr. leverandør. ' + CHR(10) + 
            'Filnavn ' + RIGHT-TRIM(cFileName,".csv") + '_<LevNr>.csv' + '.'
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    /*
    chExcelApplication = DYNAMIC-FUNCTION("getExcelHandle").
    IF NOT VALID-HANDLE(chExcelApplication) THEN DO:
      IF DYNAMIC-FUNCTION("setWebDoc","Open",cFileName) NE "" THEN
        MESSAGE "Could not open file: " cFileName VIEW-AS ALERT-BOX ERROR.
    END.

    chExcelApplication:Workbooks:OpenText(cFileName,2,,,,,TRUE).

    chExcelApplication:VISIBLE = TRUE.
    */
  END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EksportRecord C-Win 
PROCEDURE EksportRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cFileName          AS CHAR       NO-UNDO.
DEF VAR chExcelApplication AS COM-HANDLE NO-UNDO.

IF NOT hFieldMap:AVAIL THEN RETURN.
ELSE cFileName = STRING(hFieldMap:BUFFER-FIELD("VarebokNr"):BUFFER-VALUE) + ".csv".

SYSTEM-DIALOG GET-FILE cFileName FILTERS ".csv" "*.csv" USE-FILENAME UPDATE bOK.

IF bOK THEN DO:
  cFileName = RIGHT-TRIM(cFileName,".csv") + ".csv".

  IF NOT DYNAMIC-FUNCTION("RunProc","varebok_eksportertilfil.p",    
                           cFileName + "¤" + STRING(hFieldMap:BUFFER-FIELD("VarebokNr"):BUFFER-VALUE)
/*                            DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"buffersandfields") + "¤" +                                                    */
/*                            DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"allviewfields") + "¤" +                                                       */
/*                            DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"BaseQuery") +                                                                 */
/* /*                            (IF DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"QueryFilter") NE ""                          */                        */
/* /*                               AND DYNAMIC-FUNCTION("DoMessage",0,1,"Benytt gjeldende filter ved eksport?","","") = 1 THEN */                        */
/*                              " " + DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"QueryFilter") +                                                       */
/* /*                             ELSE "") + */                                                                                                           */
/*                            DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"QueryJoin") + " BY " +                                                        */
/*                            DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"QuerySort") + " " + DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"QueryDesc") */
                           ,?) THEN
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil ved eksport av varebok",""). 
  ELSE DO:
    chExcelApplication = DYNAMIC-FUNCTION("getExcelHandle").
    IF NOT VALID-HANDLE(chExcelApplication) THEN DO:
      IF DYNAMIC-FUNCTION("setWebDoc","Open",cFileName) NE "" THEN
        MESSAGE "Could not open file: " cFileName VIEW-AS ALERT-BOX ERROR.
    END.

    chExcelApplication:Workbooks:OpenText(cFileName,2,,,,,TRUE).

    chExcelApplication:VISIBLE = TRUE.
  END.
END.
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
  DISPLAY VareBokNr VareBokBeskrivelse VareBokType BeskrivelseVarebokType 
          MesseNr MesseBeskrivelse ProfilNr KortNavn 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE rectToolBar rectUpdToolbar rectVarebok VareBokNr VareBokBeskrivelse 
         BeskrivelseVarebokType MesseNr MesseBeskrivelse KortNavn 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  DISPLAY VareBokNr OppdatDato OppdatAv Oppdatert VareBokType 
          BeskrivelseVarebokType ProfilNr KortNavn VareBokBeskrivelse MesseNr 
          MesseBeskrivelse VareBokNotat 
      WITH FRAME frmDetalj IN WINDOW C-Win.
  ENABLE btnSokMesseNr RECT-1 Oppdatert VareBokType BeskrivelseVarebokType 
         VareBokBeskrivelse MesseNr VareBokNotat btnSokVarebokType 
         btnSokProfilNr 
      WITH FRAME frmDetalj IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-frmDetalj}
  DISPLAY fi-fMesseNr fi-fProfilNr fi-cVareBokBeskrivelse 
      WITH FRAME frmFilter IN WINDOW C-Win.
  ENABLE fi-fMesseNr btnSokFiltMesseNr btnSokFiltProfilNr fi-fProfilNr 
         btnBlankFilter fi-cVareBokBeskrivelse 
      WITH FRAME frmFilter IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-frmFilter}
  DISPLAY sokAvdelingNr sokAvdelingNavn sokHg sokHgBeskr sokVg sokVgBeskr 
          sokAktNr sokAktBeskrivelse sokLevNr sokLevNamn sokBeskr sokLevKod 
          sokFraVPIdato sokTilVPIdato sokFraEdato sokTilEdato sokArtikkelNr 
          sokLevFargKod tbIkkeMerknad tbValgtInndeling sokLinjeMerknad 
          rsButModus AvdelingNr AvdelingNavn levnr levnamn LevKod ArtikkelNr Hg 
          HgBeskr ProdNr ProdusentBeskrivelse LevFargKod EDato BrukerID Vg 
          VgBeskr fi-cAnnenVarebok Beskr LinjeMerknad DB% DBKr supDB% supDBKr 
          AlfaFordeling tbUpdateStat tot_Antall avg_VareKost avg_forhRab% 
          avg_forhKalkyle avg_DBKr avg_DB% avg_supVareKost avg_supRab% 
          avg_supKalkyle avg_supDBKr avg_supDB% avg_Pris tbForventet 
          tot_supAntall tot_VareKost tot_DBKr tot_supVareKost tot_supDBKr 
          tot_Pris lab_ntoForh lab_rabf lab_kalkf lab_DBkr lab_DB% lab_ntoSup 
          lab_sup lab_kalks lab_DBkrSup lab_DB%sup lab_Pris lab_avg lab_tot 
      WITH FRAME frmLinje IN WINDOW C-Win.
  ENABLE sokAvdelingNr sokAvdelingNavn sokHg sokHgBeskr sokVg sokVgBeskr 
         sokAktNr sokAktBeskrivelse btnSplitBarY sokLevNr btnAvdeling 
         sokLevNamn sokBeskr btnMerknad btnLev sokLevKod sokFraVPIdato 
         sokTilVPIdato sokFraEdato btnHuvGr sokTilEdato sokArtikkelNr 
         sokLevFargKod tbIkkeMerknad tbValgtInndeling btnBlankLinjeFilter 
         sokLinjeMerknad btnVarGr rsButModus AvdelingNr AvdelingNavn levnr 
         levnamn btnAktivitet LevKod ArtikkelNr Hg HgBeskr ProdNr 
         ProdusentBeskrivelse LevFargKod EDato BrukerID Vg VgBeskr 
         fi-cAnnenVarebok Beskr LinjeMerknad DB% DBKr supDB% supDBKr 
         AlfaFordeling tbUpdateStat tot_Antall avg_VareKost avg_forhRab% 
         avg_forhKalkyle avg_DBKr avg_DB% avg_supVareKost avg_supRab% 
         avg_supKalkyle avg_supDBKr avg_supDB% avg_Pris tbForventet 
         btnStartStat tot_supAntall tot_VareKost tot_DBKr tot_supVareKost 
         tot_supDBKr tot_Pris lab_ntoForh lab_rabf lab_kalkf lab_DBkr lab_DB% 
         lab_ntoSup lab_sup lab_kalks lab_DBkrSup lab_DB%sup lab_Pris lab_avg 
         lab_tot RectBrowseSearchLinje rectBrowseLinje rectStatFields 
         rectBrowseSort ArtikkelBilde 
      WITH FRAME frmLinje IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-frmLinje}
  ENABLE rectBrowseListe RectBrowseSearchListe 
      WITH FRAME frmListe IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-frmListe}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExcelSheetParams C-Win 
PROCEDURE ExcelSheetParams :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihOutputObject     AS HANDLE NO-UNDO.
DEF INPUT PARAM chExcelApplication AS COM-HANDLE NO-UNDO.
DEF INPUT PARAM chWorkbook         AS COM-HANDLE NO-UNDO.
DEF INPUT PARAM chWorksheet        AS COM-HANDLE NO-UNDO.
DEF INPUT PARAM iCount             AS INTEGER NO-UNDO.

chWorkSheet:PageSetup:LeftHeader = "Varebok inkl størrelsestyper".
chWorkSheet:Range("F:F"):NumberFormat = "###########0".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FlatViewRecord C-Win 
PROCEDURE FlatViewRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR hFlatView  AS HANDLE NO-UNDO.
DEF VAR hFlatBrw   AS HANDLE NO-UNDO.
DEF VAR cTmpFilter AS CHAR   NO-UNDO.

DYNAMIC-FUNCTION("DeleteObjectLink",hBrowseLinje,hBrowseSort).
IF DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"queryfilter") MATCHES "*< 0" THEN DO:
  cTmpFilter = DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"queryfilter").
  DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"queryfilter","").
END.

RUN SUPER.

DYNAMIC-FUNCTION("CreateParentLink",hBrowseSort,hBrowseLinje,"ArtikkelNr").
IF cTmpFilter NE "" THEN
  DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"queryfilter",cTmpFilter).


hFlatView = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hUpdToolBar,"flatviewhandle")) NO-ERROR.
IF NOT VALID-HANDLE(hFlatView) THEN RETURN.

/* hFlatBrw  = DYNAMIC-FUNCTION("getBrowseHandle" IN hFlatView).                  */
/*                                                                                */
/* DYNAMIC-FUNCTION("setAttribute",hFlatBrw,"SkipUniqueRows","1,2,3,4,5,6,7,8").  */

{incl/dynfilterlookups_art.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE forhRab%Record C-Win 
PROCEDURE forhRab%Record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR ocValue     AS CHAR NO-UNDO.
DEF VAR iConfirm    AS INT NO-UNDO.

iReturn = 0.
RUN JBoxBrowseMsgUpdSelVal.w ("Oppdater rabatt forhåndskjøp",
                              hBrowseLinje:NUM-SELECTED-ROWS,
                              DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"Totalcount"),
                              "DECIMAL|>>,>>9.99|Rabatt forh.kjøp",
                              OUTPUT ocValue, 
                              OUTPUT iReturn).
IF DEC(ocValue) = 0 AND iReturn NE 0 THEN 
  iConfirm = DYNAMIC-FUNCTION("DoMessage",0,1,"Bekreft at rabatt forhåndskjøp skal settes til 0","Bekreft","").

IF iReturn NE 0 AND iConfirm NE 2 THEN
  EndreMangeLinjer("forhRab%",ocValue).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getArtSokFargeKoding C-Win 
PROCEDURE getArtSokFargeKoding :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF OUTPUT PARAM ocFarge AS CHAR NO-UNDO INIT "varebok".
DEF OUTPUT PARAM ocKrit  AS CHAR NO-UNDO.

IF iTab = 3 AND hFieldMap:AVAIL THEN
  ocKrit = STRING(hFieldMap:BUFFER-FIELD("VarebokNr"):BUFFER-VALUE).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getSelectorAttributes C-Win 
PROCEDURE getSelectorAttributes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM  ihSelectorSource AS HANDLE NO-UNDO.
DEF INPUT PARAM  ihSelectorTarget AS HANDLE NO-UNDO.
DEF INPUT PARAM  icDeSelRowidList AS CHAR NO-UNDO.
DEF OUTPUT PARAM oiReturn         AS INT NO-UNDO.

DEF VAR cTmpHuvGrList    AS CHAR NO-UNDO.
DEF VAR cTmpVarGrList    AS CHAR NO-UNDO.

ASSIGN cCurrSelectBuffer   = ihSelectorSource:QUERY:GET-BUFFER-HANDLE(1):NAME
       iSelectorSourcCount = INT(DYNAMIC-FUNCTION("getAttribute",ihSelectorSource,"totalcount"))
       cSortDescr          = "".

/* Håndtering av avhengighet Avdeling/HuvGr: */
IF cCurrSelectBuffer = "HuvGr" THEN DO WITH FRAME frmLinje:
  cHuvGrAvdelingList = "".
  ihSelectorTarget:QUERY:GET-FIRST().
  REPEAT WHILE NOT ihSelectorTarget:QUERY:QUERY-OFF-END:
    cHuvGrAvdelingList = cHuvGrAvdelingList + STRING(ihSelectorTarget:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("AvdelingNr"):BUFFER-VALUE) + ",".
    ihSelectorTarget:QUERY:GET-NEXT().
  END.  
  cHuvGrAvdelingList = TRIM(cHuvGrAvdelingList,",").

  IF cVarGrHuvGrList NE "" THEN DO:
    DO ix = 1 TO NUM-ENTRIES(cVarGrRowIdList):
      bOK = ihSelectorTarget:QUERY:GET-BUFFER-HANDLE(1):FIND-FIRST("WHERE Hg = " + ENTRY(ix,cVarGrHuvGrList)) NO-ERROR.
      IF bOk THEN
        cTmpVarGrList = cTmpVarGrList + ENTRY(ix,cVarGrRowIdList) + ",".
    END.
    IF NUM-ENTRIES(TRIM(cTmpVarGrList,",")) NE NUM-ENTRIES(cVarGrRowIdList) THEN DO:
      ASSIGN cVarGrRowIdList         = ""
             cVarGrIdList            = ""
             sokVg:SCREEN-VALUE      = "0"
             sokVgBeskr:SCREEN-VALUE = ""
             .
    END.
  END.
END.
ELSE IF cCurrSelectBuffer = "Avdeling" AND cHuvGrAvdelingList NE "" THEN DO:
  DO ix = 1 TO NUM-ENTRIES(cHuvGrRowIdList):
    bOK = ihSelectorTarget:QUERY:GET-BUFFER-HANDLE(1):FIND-FIRST("WHERE AvdelingNr = " + ENTRY(ix,cHuvGrAvdelingList)) NO-ERROR.
    IF bOk THEN
      cTmpHuvGrList = cTmpHuvGrList + ENTRY(ix,cHuvGrRowIdList) + ",".
  END.
  IF NUM-ENTRIES(TRIM(cTmpHuvGrList,",")) NE NUM-ENTRIES(cHuvGrRowIdList) THEN DO:
    ASSIGN cHuvGrRowIdList         = ""
           cHuvGrIdList            = ""
           cVarGrRowIdList         = ""
           cVarGrIdList            = ""
           sokHg:SCREEN-VALUE      = "0"
           sokHgBeskr:SCREEN-VALUE = ""
           sokVg:SCREEN-VALUE      = "0"
           sokVgBeskr:SCREEN-VALUE = ""
           .
  END.
END.

/* Håndtering av avhengighet HuvGr/VarGr: */
ELSE IF cCurrSelectBuffer = "VarGr" THEN DO:
  cVarGrHuvGrList = "".
  ihSelectorTarget:QUERY:GET-FIRST().
  REPEAT WHILE NOT ihSelectorTarget:QUERY:QUERY-OFF-END:
    cVarGrHuvGrList = cVarGrHuvGrList + STRING(ihSelectorTarget:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("Hg"):BUFFER-VALUE) + ",".
    ihSelectorTarget:QUERY:GET-NEXT().
  END.  
  cVarGrHuvGrList = TRIM(cVarGrHuvGrList,",").
END.

ELSE IF VALID-HANDLE(hSortPanel) THEN 
  cSortDescr = DYNAMIC-FUNCTION("getTekst" IN hSortPanel).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GjFaktRecord C-Win 
PROCEDURE GjFaktRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR ocValue     AS CHAR NO-UNDO.
DEF VAR iConfirm    AS INT NO-UNDO.

iReturn = 0.
RUN JBoxBrowseMsgUpdSelVal.w ("Oppdater flagg for gjennomfakturering",
                              hBrowseLinje:NUM-SELECTED-ROWS,
                              DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"Totalcount"),
                              "LOGICAL|Ja/Nei|Nei",
                              OUTPUT ocValue, 
                              OUTPUT iReturn).

IF iReturn NE 0 AND iConfirm NE 2 THEN
  EndreMangeLinjer("Gjennomfaktureres",ocValue).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Help C-Win 
PROCEDURE Help :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cFileName AS CHAR NO-UNDO.

cFileName = ".\hlp\" + SUBSTR(THIS-PROCEDURE:FILE-NAME,1,INDEX(THIS-PROCEDURE:FILE-NAME,".")) + "htm".

IF SEARCH(cFileName) NE ? THEN DO:
  FILE-INFO:FILE-NAME = cFileName.
  DYNAMIC-FUNCTION("setWebDoc","open",FILE-INFO:FULL-PATHNAME).
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HentUtvalgRecord C-Win 
PROCEDURE HentUtvalgRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF hFieldMap:AVAIL AND hFieldMap:BUFFER-FIELD("oppdatert"):BUFFER-VALUE = NO AND VALID-HANDLE(hUtvalg) THEN DO:
  RUN OverfUtvalgTilVarebok IN hUtvalg (hFieldMap:BUFFER-FIELD("VarebokNr"):BUFFER-VALUE, OUTPUT bOK).
  IF bOK THEN DO: 
    IF iTab < 3 THEN
      TabStripChanged(13).
    ELSE APPLY "value-changed" TO hBrowseListe.
  END.
END.
ELSE IF NOT VALID-HANDLE(hUtvalg) THEN
  DYNAMIC-FUNCTION("DoMessage",0,0,"Utvalg ikke tilgjengelig","","").
ELSE IF hFieldMap:AVAIL THEN 
  DYNAMIC-FUNCTION("DoMessage",0,0,"Varebok &1 er oppdatert og kan ikke tilføres flere artikler","",STRING(hFieldMap:BUFFER-FIELD("VarebokNr"):BUFFER-VALUE)).
ELSE 
  DYNAMIC-FUNCTION("DoMessage",0,0,"Ingen varebok valgt","","").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeObject C-Win 
PROCEDURE InitializeObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>  
  Notes:       
              ;FeilVare|Reklam  
------------------------------------------------------------------------------*/
DEF VAR cHKinst     AS CHAR NO-UNDO.
DEF VAR hBildeFrame AS HANDLE NO-UNDO.

iFontWingdings = DYNAMIC-FUNCTION("setAppFont","Wingdings, size=11 Script=symbol","").

SetTabStrip().

ASSIGN cAdgang        = DYNAMIC-FUNCTION("getFieldValues","SysPara",
                            "WHERE SysHId = 16 and SysGr = 39 and ParaNr = 2","Parameter1")
       cHKinst        = DYNAMIC-FUNCTION("getFieldValues","SysPara",
                            "WHERE SysHId = 1 and SysGr = 1 and ParaNr = 18","Parameter1")
       bHKinst        = IF CAN-DO("1,yes,true",cHKinst) THEN TRUE ELSE FALSE
       .  
/*        bHKinst = TRUE.  */

DO WITH FRAME frmListe:
  hBrowseListe = DYNAMIC-FUNCTION("NewBrowse",                 /* Create a browse object */
                    rectBrowseListe:HANDLE IN FRAME frmListe,  /* Rectangle to define coordinates for browse */
                    50,                                        /* Rows to batch */
                    "NUM-LOCKED-COLUMNS|1",           /* Browse properties, ie MULTIPLE,NUM-LOCKED-COLUMNS, etc */
                    "VareBokHode"
                      + ";VareBokNr"
                      + ";VareBokBeskrivelse"
                      + ";MesseNr"
                      + ";!VarebokType;!ProfilNr;!Oppdatert;!OppdatDato;!EDato;!BrukerId;!VareBokNotat;!OppdatAv"
/*                       + ";!+Oppmerking|CHARACTER|x(40)|varebokhode_oppmerking.p" */
                    + ",Messe"
                      + ";MesseBeskrivelse|Messenavn"
                      + ";MesseFraDato|Messe fra"
                      + ";MesseTilDato|Messe til"
                      + ";FraDato|Ordre fra"
                      + ";TilDato|Ordre til"
                      + ";!Fargekoder;!Sortimentkoder;!Kampanjeuker;!Kampanjestotte;!Lagerkoder;!Oppmerking"
                    + ",PrisProfil"
                      + ";KortNavn|Profil@3"
                    + ",VarebokType"
                      + ";BeskrivelseVarebokType|Type@4",
                    "WHERE true, FIRST Messe NO-LOCK where Messe.MesseNr = VareBokHode.MesseNr OUTER-JOIN " 
                      + ",FIRST PrisProfil NO-LOCK where PrisProfil.ProfilNr = VareBokHode.ProfilNr OUTER-JOIN"
                      + ",FIRST VarebokType NO-LOCK OF VareBokHode OUTER-JOIN",
                    "sort|VareBokNr DESC,getrecordcount").
  hBrowseListe:NAME = "Varebøker". /* This name is neccessary because the browser is due to special treatment during resize */
  DYNAMIC-FUNCTION("setAttribute",hBrowseListe,"querywhere","").
  DYNAMIC-FUNCTION("setAttribute",hBrowseListe,"nocolumnsearch","MesseBeskrivelse,Kortnavn,BeskrivelseVarebokType").
  
/*   hBrowseListe:MOVE-COLUMN(10,3). */
/*   hBrowseListe:MOVE-COLUMN(10,3). */
  hSearchListe = DYNAMIC-FUNCTION("NewBrowseSearchField",RectBrowseSearchListe:HANDLE,hBrowseListe,1).
END.

DO WITH FRAME frmDetalj:
 
  hFieldMap = DYNAMIC-FUNCTION("NewFieldMap",      /* A fieldmap object holds extra info for display and input fields (fill-ins)
                                                       and their corresponding buffer columns return handle equals the buffer handle */
                    hBrowseListe:QUERY,
                    FRAME frmDetalj:HANDLE,         /* Frame for the input/display fields (might not be the same frame as the browse) */
                                                    /* Nb: Felt som brukes her må være hentet i Browser først.                        */
                    "VareBokBeskrivelse,MesseNr,VareBokNotat,ProfilNr,Oppdatert,OppdatDato,OppdatAv,VarebokType"
                    ,"",       
                    "VareBokNr,MesseBeskrivelse,KortNavn,BeskrivelseVarebokType", /* Additional buffer and displ.fields - not updateable*/
                    "",  
                    "").     /* Input fields other than in buffer */

    DYNAMIC-FUNCTION("setAttribute",hFieldMap,"customUpdateValProc","ignore"). 
    DYNAMIC-FUNCTION("setAttribute",hFieldMap,"customDeleteValProc","=delval_varebokhode.p"). 
END.

RUN InitLinje.

DO WITH FRAME {&FRAME-NAME}:
  IF CAN-DO("1",cAdgang) THEN
      hUpdToolBar = DYNAMIC-FUNCTION("NewToolBar",
                    rectUpdToolBar:HANDLE,         
                    "Fil",                         
                    "Undo;Angre,save;Lagre,flatview,excel;Eksporter til E&xcel,print|"
                     + ",rule,first|Naviger;Første,prev|Naviger;Forrige,next|Naviger;Neste,last|Naviger;Siste&,|Innstillinger,rule"
                     + ",BrowseConfig|;Kolonneoppsett"
                     + ",StartUtvalg;Utvalg"
                     + ",HentUtvalg;Hent artikler fra utvalg"
                     + ",OppdaterViaExcel;Oppdater i Excel"
                    ,
                    "maxborder").   
  ELSE
      hUpdToolBar = DYNAMIC-FUNCTION("NewToolBar",
                    rectUpdToolBar:HANDLE,     
                    "Fil",                     
                    "New;Ny,Undo;Angre,delete;Slett&,save;Lagre,flatview,excel;Eksporter til E&xcel,print|"
                     + ",rule,first|Naviger;Første,prev|Naviger;Forrige,next|Naviger;Neste,last|Naviger;Siste&,|Innstillinger,rule"
                     + ",BrowseConfig|;Kolonneoppsett"
                     + ",StartUtvalg;Utvalg"
                     + ",HentUtvalg;Hent artikler fra utvalg"
                     + ",OppdaterViaExcel;Oppdater i Excel"
                    ,
                    "maxborder").      

  hPrintBtn = DYNAMIC-FUNCTION("getEventWidget",hUpdToolBar,"print","").
  hPrintBtn:TOOLTIP  = "Høyreklikk for å velge rapportalternativer".
  hPrintBtn:MENU-KEY = "left-mouse-down".

  cBtnUtvalgHandles     = DYNAMIC-FUNCTION("getToolbarHandles",hUpdToolbar,"*StartUtvalg*").
  cBtnHentUtvalgHandles = DYNAMIC-FUNCTION("getToolbarHandles",hUpdToolbar,"*HentUtvalg*").
    
  DYNAMIC-FUNCTION("NewMenuBand",
                    hPrintBtn,             /* parent widget */
                    "levPrint;Leverandørsortert liste"
                  + ",EANPrint;Varer med størrelser"
                  + ",ArtPrint;Artikkelrapport"
                  + ",LevTestPrint;Testutskrift Leverandørsortert liste (ikke formattert)"
                  + ",EanTestPrint;Testutskrift Varer med størrelser (ikke formattert)"
                  + ",LevStrKodeSjekkPrint;Kontrollrapport: Artikler med strekkoder som mangler i str.type"
                    ,
                    "").

  DYNAMIC-FUNCTION("NewMenuBand",
                    WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hUpdToolbar,"placeholder1")),  /* parent widget */
                    "RowsToBatch;Antall rader i resultatsett¤enable"
                   ,"").

  hRowsToBatchMenu = DYNAMIC-FUNCTION("getEventWidget",hUpdToolbar,"RowsToBatch","menu-item").

  hToolBar = DYNAMIC-FUNCTION("NewToolBar",
                    rectToolBar:HANDLE,             
                    "Fil",                          
                    "Help|Hjelp,Close;Avslutt",
                    "").   
  
  
  /* Link objects: */

  DYNAMIC-FUNCTION("createObjectLink",hBrowseListe,hUpdToolbar).
  DYNAMIC-FUNCTION("createObjectLink",hFieldMap,hUpdToolbar).
  DYNAMIC-FUNCTION("createObjectLink",hBrowseListe,hFieldMap).
  DYNAMIC-FUNCTION("createObjectLink",hBrowseLinje,hFieldMapLinje).
  DYNAMIC-FUNCTION("createParentLink",hBrowseLinje,hBrowseListe,'VareBokNr').
  DYNAMIC-FUNCTION("createObjectLink",hBrowseListe,hSearchListe).
  IF bHKinst THEN DO WITH FRAME frmLinje:
    rsButModus:SCREEN-VALUE = STRING(NO).
    APPLY "value-changed" TO rsButModus.
  END.
  ELSE LinkOverlays(). 
       
  iColor = INT(DYNAMIC-FUNCTION("getUserSetting",THIS-PROCEDURE:FILE-NAME,DYNAMIC-FUNCTION("getObjectName",hBrowseLinje),"","overlaybgcolor")).
  IF iColor NE 0 THEN 
    DYNAMIC-FUNCTION("setBrwOverlayBGcolNum",hBrowseLinje,setFieldColor(iColor)).

  DYNAMIC-FUNCTION("setAddMoveX",  THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "rectNavVarebok").
  DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "rectUpdToolBar,rectVarebok").
  DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "rectNavVarebok").

  DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME frmFilter:HANDLE, "frmFilter").

  DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME frmListe:HANDLE, "RectBrowseSearchListe").

  DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME frmDetalj:HANDLE, "rect-1").
  DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME frmDetalj:HANDLE, "rect-1").

  DYNAMIC-FUNCTION("setNoMoveX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME frmLinje:HANDLE, "btnStartSearch,btnBlankLinjeFilter,ArtikkelNr,brwSort,rectBrowseSort,AlfaFordeling," 
                                                                                      + cStatLabelNames + "," + cStatFieldNames + "," + cPrefixedStatFields).
  DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME frmLinje:HANDLE, "RectBrowseSearchLinje,IMAGE-Sko").
  DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME frmLinje:HANDLE, "IMAGE-Sko,rectStatFields,brwLinje,rectBrowseLinje").
  DYNAMIC-FUNCTION("setAddResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME frmLinje:HANDLE, "AlfaFordeling").

/*   hBildeFrame = DYNAMIC-FUNCTION("getFrameHandle" IN hArtBilde).                                     */
/*   DYNAMIC-FUNCTION("setNoMoveX", THIS-PROCEDURE:CURRENT-WINDOW, hBildeFrame, "frmImage,IMAGE-sko").  */

  DYNAMIC-FUNCTION("setSplitBarY" , THIS-PROCEDURE:CURRENT-WINDOW, btnSplitBarY:HANDLE IN FRAME frmLinje,NO).
  DYNAMIC-FUNCTION("setFollowSplitBarY",THIS-PROCEDURE:CURRENT-WINDOW, btnSplitBarY:HANDLE IN FRAME frmLinje,
                    STRING(rectBrowseLinje:HANDLE)
                  + "," + STRING(hBrowseLinje)
                  + "," + STRING(rectBrowseSort:HANDLE)
                  + "," + STRING(hBrowseSort:HANDLE)).
  DYNAMIC-FUNCTION("setSplitBarYLimits",btnSplitBarY:HANDLE IN FRAME frmLinje,200,170).

  DYNAMIC-FUNCTION("setOrgWinSize", THIS-PROCEDURE:CURRENT-WINDOW,730,350,0,0).

  LoadRowsToBatchSettings().

  DYNAMIC-FUNCTION("initTranslation",THIS-PROCEDURE:CURRENT-WINDOW).

  DYNAMIC-FUNCTION("SetToolbar",hToolBar,"enable").

  APPLY "value-changed" TO tbSearchOnValueChange.
END.

SUBSCRIBE TO "ArtBasEndret"         ANYWHERE.
SUBSCRIBE TO "ExcelSheetParams"     ANYWHERE.
SUBSCRIBE TO "getArtSokFargeKoding" ANYWHERE.
SUBSCRIBE TO "NyttBildeLastet"      ANYWHERE.

TabStripChanged(11).

IF NOT hFieldMap:AVAIL AND hBrowseListe:QUERY:IS-OPEN THEN
  hBrowseListe:QUERY:GET-FIRST().

IF hBrowseListe:NUM-ITERATIONS > 0 THEN
    APPLY "value-changed" TO hBrowseListe.

APPLY "ENTRY" TO hBrowseListe.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitLinje C-Win 
PROCEDURE InitLinje :
DEF VAR hField         AS HANDLE NO-UNDO.

DO WITH FRAME frmLinje:
  RUN VisMiniBilde.w PERSIST SET hArtBilde.
  RUN InitializeObject IN hArtBilde (ArtikkelBilde:HANDLE).
  hArtBildeFrame = DYNAMIC-FUNCTION("getFrameHandle" IN hArtBilde).

  ASSIGN cVarebokLinjeJoin = ",FIRST ArtBas OF VareBokLinje NO-LOCK"
                              + ",FIRST Varemerke OF ArtBas NO-LOCK OUTER-JOIN"
                              + ",FIRST SaSong OF VareBokLinje NO-LOCK"
         cAktivitetJoin    =  ",FIRST VgAkt WHERE VgAkt.Vg = VarebokLinje.Vg OUTER-JOIN"
                              + ",FIRST Aktivitet OF VgAkt OUTER-JOIN"
         rsButModus:HIDDEN = IF bHKinst THEN FALSE ELSE TRUE
                             .
  hBrowseLinje = DYNAMIC-FUNCTION("NewBrowse",         
                    rectBrowseLinje:HANDLE IN FRAME frmLinje,  
                    50,                          
                    "MULTIPLE", 
                    "VareBokLinje" 
                      + ";Sekv"
                      + ";LevNamn|Lev.navn|x(30)"
                      + ";Beskr|Art.navn|x(30)"
                      + ";LevKod|Lev.art.nr|x(20)"
                      + ";LevFargKod|Lev.farge|x(20)"
                      + ";InnkjopsPris|Engros"
                      + ";VareKost|Nto.innpr forh"
                      + ";forhRab%|Rab.forh"  /* TN */
                      + ";forhKalkyle|Klk.f|->>>9.99"
                      + ";supVareKost|Nto.innpr sup"
                      + ";supRab%|Rab.sup"    /* TN */
                      + ";supKalkyle|Klk.s|->>>9.99"
                      + ";Pris|Markedspris"
                      + ";AnbefaltPris|Veil.pris"
                      + ";KampanjePris|Kamp.pris"
                      + ";+LevUke1|INTEGER|>>>>>9|vareboklinje_levuke1"
                      + ";+LevUke2|INTEGER|>>>>>9|vareboklinje_levuke2"
                      + ";+LevUke3|INTEGER|>>>>>9|vareboklinje_levuke3"
                      + ";+LevUke4|INTEGER|>>>>>9|vareboklinje_levuke4"
                      + ";Sortimentkoder|Sort"
                      + ";Kampanjeuker|Kamp.uke"
                      + ";Kampanjestotte|Støtte"
                      + ";Lagerkoder|Lager"
                      + ";LinjeMerknad|Merknad|x(256)"
/*                       + ";+TilgjSort|CHARACTER|x|varebok_tilgjsort|Tilgj.Innd"  */
                      + ";+ValgtSort|CHARACTER|x|varebok_valgtsort|Valgt.Innd"
                      +  /* (IF bHKinst THEN  */
                           ";KjedeRab%|Kj.rab%"
                           + ";KjedeInnkPris|Kj.ink.pr"
                           + ";KjedeSupRab%|Kj.sup.rab%"
                           + ";KjedeSupInnkPris|Kj.sup.ink.pr"
                           + ";Antall|Anf.forh|>>>>9"
                           + ";supAntall|Ant.sup|>>>>9" 
/*                          ELSE "") */
                      + ";Db%|Db%forh"
                      + ";DBKr|DBkrForh"
                      + ";supDb%|Db%sup"
                      + ";supDBKr|DBkrSup"
                      + ";Mva%"
/*                       + ";LevKod|Lev.art.nr|x(20)"  */
                      + ";ArtikkelNr|Art.nr"
                      + ";ModellFarge|Modell|>>>>>>9"
                      + ";Vg|VgNr"
                      + ";VgBeskr|VgBeskr|x(20)"
                      + ";Hg|HgNr"
                      + ";HgBeskr|HgBeskr|x(20)"
                      + ";AvdelingNr|AvdNr"
                      + ";AvdelingNavn|AvdBeskr|x(25)"
                      + ";LevNr"
                      + ";ProdNr"
                      + ";ProdusentBeskrivelse|Prod.Navn|x(20)"
                      + ";EDato"
/*                       + ";+cEndretTid|CHARACTER|x(5)|jbserv_int_to_hhmm_time.p(etid)|Tid" */
                      + ";+cEndretTid|CHARACTER|x(5)|vareboklinje_endrettid(etid)|Tid"
                      + ";!ETid"
                      + ";BrukerID"
                      + ";VPIdato"
                      + ";+AndreVareboker|CHARACTER|x(20)|artikkel_varebok_messe|AndreVareB"
                      + ";!+RGBfarge|INTEGER|>>>>>>>>9|vareboklinje_rgb"
                      + ";!VarebokNr"
                      + ";KjedeVare|Kj.vare@23"
                      + ";Gjennomfaktureres|Gj.fakt@24"
                      + ";!Sasong"
                      + ";!Utvidetsok"
                    + ",ArtBas" 
                      + ";SalgsEnhet"
                      + ";FrittTillegg|Fritt tillegg@25"
                      + ";+VareFaktaInd|CHARACTER|x|artbas_varefaktaind|VF@26"
                      + ";AntIPakn"
                      + ";VPIbildeKode"
                      + ";!RAvdNr|Vareomr"
                      + ";!LevDato1;!LevDato2;!LevDato3;!LevDato4;!StrTypeId;!VareFakta;!BildNr;!Valkod;!VPIBildeKode"
                    + ",Varemerke;Beskrivelse|Varemerke"
                    + ",Sasong;SasBeskr|Sesong@22"
                    + ",VgAkt;AktNr"
                    + ",Aktivitet;Beskrivelse|Akt.beskr"
                    + ",StrType;!AlfaFordeling"
                    + ",ArtSort;!SortId"
                    + ",Regnskapsavdeling;RAvdBeskrivelse|Vareomr@27"
                    ,"WHERE VarebokNr < 0 AND ArtikkelNr < 0"
                      + cVarebokLinjeJoin
                      + cAktivitetJoin
                      + ",FIRST StrType OF ArtBas NO-LOCK"
                      + ",FIRST ArtSort OF ArtBas NO-LOCK OUTER-JOIN"
                      + ",FIRST Regnskapsavdeling OF ArtBas NO-LOCK OUTER-JOIN"
                    ,"calcfieldproc|varebok_browsekalk.p").
  hBrowseLinje:NAME = "brwLinje". 


/*   ihBrowse:MOVE-COLUMN(48,22).  /* Kjedevare */         */
/*   ihBrowse:MOVE-COLUMN(49,23).  /* Gjennomfaktureres */ */
/*   ihBrowse:MOVE-COLUMN(51,24).  /* Fritt tillegg */     */
/*   ihBrowse:MOVE-COLUMN(52,24).  /* VareFaktaInd */      */
/*   ihBrowse:MOVE-COLUMN(55,22).  /* SasBeskr */          */
/*                                                         */
/*   hBrwColKjedeVare = ihBrowse:GET-BROWSE-COLUMN(23).    */
/*   hBrwColGjFakt    = ihBrowse:GET-BROWSE-COLUMN(24).    */

/*   DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"windowsbrowse","yes"). */
  DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"querywhere","").  
  DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"select1strow","yes").  
  DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"mandatoryfields","levnamn,beskr,levkod").  
  DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"configeditfields","yes").
  DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"sortmap","cEndretTid;ETid,LevUke1;LevDato1,LevUke2;LevDato2,LevUke3;LevDato3,LevUke4;LevDato4,ValgtSort;SortId,VareFaktaInd;VareFakta").  
  DYNAMIC-FUNCTION("setNoColumnSort",hBrowseLinje,"AndreVareboker,ValgtSort").
/*   DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"enableondblclick","yes").  */

  hSearchLinje = DYNAMIC-FUNCTION("NewBrowseSearchField",RectBrowseSearchLinje:HANDLE,hBrowseLinje,2).
  DYNAMIC-FUNCTION("createObjectLink",hBrowseLinje,hSearchLinje).
  hSearchLinje:HIDDEN = YES.
  
  InitOverlays().

  DYNAMIC-FUNCTION("NewMenuBand",
                    hBrowseListe,  
                    "Deselect;Fjern markering av rad",
                    "").   

  DYNAMIC-FUNCTION("NewMenuBand",
                    hBrowseLinje,  
                    "Deselect;Fjern markering av rad"
                    + ",MultiSortBrowse;Sorter på flere kolonner" 
                    + ",|Spesialsortering"
                    + ",InitSekv;Initier sekv.nr (krever filter på HG - initierer alle art. i HG)"
                    + ",VareFakta;Rediger varefakta for artikkel"
                    + ",BildeGrid;Vis bildegrid;BildeGrid" 
                    + ",|-" 
                    + ",RefreshArt;Frisk opp artikkelinformasjon alle artikler"
                    + ",RefreshArtValgte;Frisker opp artikkelinformasjon valgte;RefreshArtRecordValgte"
/*                     + ",DelUtv;Fjern markerte artikler fra varebok;SlettFraUtvalg"  */
                    + ",|-" 
                    + ",|Justér"
                    + ",|-" 
                    + ",OverforVB;Kopier markerte artikler til annen varebok;OverforTilAnnenVB"
                    + ",|-" 
                    + ",KopierVBL;Kopier artikler til varehåndteringsbok;KopierTilVarebehandling" 
                    + ",KopierValgte;Kopier markerte artikler til varehåndteringsbok;KopierValgteTilVarebeh" 
                    + ",|-" 
                    + ",KopierPrisValgte;Oppdater kalkyle for markerte artikler i tilhørende varehåndteringsbok;KopierPrisForValgteTilVareh" 
                    + ",|-" 
                    + ",AktiverKalkyle;Aktiver kalkyle i artikkelregister for alle artikler;AktiverKalkyle" 
                    + ",AktiverValgte;Aktiver kalkyle i artikkelregister for valgte artikler;AktiverKalkyleValgte" 
                    + ",|-" 
                    + ",AktiverVPIAlle;Overfør alle artikler til VPI register/send til butikk;AktiverVPIAlle" 
                    + ",AktiverVPIValgte;Overfør valgte artikler til VPI register/send til butikk;AktiverVPIValgte" 
                    + ",|-" 
/*                     + ",OverforVPIERPAlle;Overfør alle artikler til ERP;OverforVPIERPAlle" */
/*                     + ",OverforVPIERPValgte;Overfør valgte artikler til ERP;OverforVPIERPValgte" */
                    + ",OverforVPI;Overfør artikler til ERP;OverforVPI"
                    + ",|-" 
                    + ",EANRapportValgte;Skriv ut varebok med EAN kode for valgte artikler" 
                    + ",|-" 
                    + ",Eksport;Eksporter varebok med størrelser til fil" 
                    + ",EksportPricat;Eksporter varebok med størrelser til pricat;EksportPricat" 
                    ,
                    "").     


  hSpesialSortMnu = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"placeholder1")).
  setSpesialSortMenu().

  DYNAMIC-FUNCTION("NewMenuBand",
                    WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"placeholder2")), 
                    "Innkjopspris;Engrospris"
                    + ",Varekost;Netto innkjøpspris forhåndskjøp"
                    + ",supVarekost;Netto innkjøpspris suppleringskjøp"
                    + ",forhRab%;Rabatt forhåndskjøp"
                    + ",supRab%;Rabatt suppleringsskjøp"
                    + (IF bHKinst THEN ",KjedeRab%;Kjederabatt" ELSE "")
                    + (IF bHKinst THEN ",KjedeInnkPris;Kjedens innkjøpspris" ELSE "")
                    + (IF bHKinst THEN ",KjedeSupRab%;Kjede sup.rab" ELSE "")
                    + (IF bHKinst THEN ",KjedeSupInnkPris;Kjede sup.innk.pris" ELSE "")
                    + ",Pris;Markedspris"
                    + ",AnbefaltPris;Veiledende pris"
                    + ",KampanjePris;KampanjePris"
                    + ",|-" 
                    + (IF bHKinst THEN ",KjedeVare;Flagg for Kjedevare" ELSE "")
                    + (IF bHKinst THEN ",GjFakt;Flagg for Gjennomfakturering" ELSE "")
                    + ",|-" 
                    + (IF bHKinst THEN ",Antall;Antall forhåndskjøp" ELSE "")
                    + (IF bHKinst THEN ",supAntall;Antall suppleringskjøp" ELSE "")
                    + ",Sortimentkoder;Sortimentkode for markerte linjer"
                    + ",Kampanjeuker;Kampanjeuker for markerte linjer"
                    + ",Kampanjestotte;Kampanjestøtte for markerte linjer"
                    + ",Lagerkoder;Lagerkoder for markerte linjer"
                    + ",LinjeMerknad;Merknad for markerte linjer"
                    + ",LevUke1;Leveringsuke 1;levukeRecord"
                    + ",LevUke2;Leveringsuke 2;levukeRecord"
                    + ",LevUke3;Leveringsuke 3;levukeRecord"
                    + ",LevUke4;Leveringsuke 4;levukeRecord"
                    + ",Sesong;Sesong"
                    + ",Varegruppe;Varegruppe"
                    + ",Vareomr;Vareområde"
                    ,
                    "").     

  hFieldMapLinje = DYNAMIC-FUNCTION("NewFieldMap", 
                    hBrowseLinje:QUERY,
                    FRAME frmLinje:HANDLE,     
                    "",
                    "",
                    "LevNr,LevNamn,LevKod,LevFargKod,ProdNr,ProdusentBeskrivelse,Vg,VgBeskr,Hg,HgBeskr,AvdelingNr," + 
                    "AvdelingNavn,EDato,BrukerID,LinjeMerknad,DB%,supDB%,DBkr,supDBkr,Beskr,ArtikkelNr",/*AlfaFordeling", */
                    "",  
                    "").     
  hFieldRGBcolor = hFieldMapLinje:BUFFER-FIELD("RGBfarge"). 

  DYNAMIC-FUNCTION("setAttribute",hFieldMapLinje,"customUpdateValProc","=update_vareboklinje.p").
  DYNAMIC-FUNCTION("setAttribute",hFieldMapLinje,"checkmodified","never").  

  cStatFieldHandles = DYNAMIC-FUNCTION("getWidgetHandles",FRAME frmLinje:HANDLE,0,
                                       rectStatFields:X,
                                       rectStatFields:X + rectStatFields:WIDTH-PIXELS,
                                       rectStatFields:Y,
                                       rectStatFields:Y + rectStatFields:HEIGHT-PIXELS,
                                       "fill-in").
  cStatLabelHandles = DYNAMIC-FUNCTION("getWidgetHandles",FRAME frmLinje:HANDLE,0,
                                       rectStatFields:X + 10,
                                       rectStatFields:X + rectStatFields:WIDTH-PIXELS,
                                       rectStatFields:Y - 15,
                                       rectStatFields:Y + rectStatFields:HEIGHT-PIXELS,
                                       "text").

  DO ix = 1 TO NUM-ENTRIES(cStatFieldHandles):
    hField = WIDGET-HANDLE(ENTRY(ix,cStatFieldHandles)).
    IF INDEX(hField:NAME,"_") > 0 THEN
      ASSIGN cStatFieldNames      = cStatFieldNames + SUBSTR(hField:NAME,INDEX(hField:NAME,"_") + 1) + ","
             cStatFieldPrefixList = cStatFieldPrefixList + SUBSTR(hField:NAME,1,INDEX(hField:NAME,"_") - 1)
                                    .
    ELSE
      ASSIGN cStatFieldNames      = cStatFieldNames + hField:NAME + ","
             cStatFieldPrefixList = cStatFieldPrefixList + ","
             .
    cPrefixedStatFields = cPrefixedStatFields + hField:NAME + ",".
  END.
  DO ix = 1 TO NUM-ENTRIES(cStatLabelHandles):
    hField = WIDGET-HANDLE(ENTRY(ix,cStatLabelHandles)).
    cStatLabelNames = cStatLabelNames + hField:NAME + ",".
  END.


  hBrowseSort = DYNAMIC-FUNCTION("NewBrowse",         
                    rectBrowseSort:HANDLE IN FRAME frmLinje,  
                    50,                          
                    "",
                    "ArtSort" 
                    + ";SortID|Innd"
                    + ";+StrInterv|CHARACTER|x(40)|artsort_intervall.p|Intervall"
                    + ";+StrFord|CHARACTER|x(40)|artsort_fordeling.p|Fordeling"
                    + ";+SumAntStr|INTEGER|>>9|artsort_sumant.p|Tot"
                  + ",LevSort;!LevNr"
                   ,"WHERE false"
                  + ",FIRST LevSort OF ArtSort NO-LOCK"
                   ,"sort|SortId").
  hBrowseSort:NAME = "brwSort".
  hBrowseSort:GET-BROWSE-COLUMN(1):WIDTH-PIXELS = 25.
  hBrowseSort:GET-BROWSE-COLUMN(2):WIDTH-PIXELS = 40.
  hBrowseSort:GET-BROWSE-COLUMN(3):WIDTH-PIXELS = 55.
  hBrowseSort:GET-BROWSE-COLUMN(4):WIDTH-PIXELS = 30.

  DYNAMIC-FUNCTION("NewMenuBand",
                    hBrowseSort,
                    "MultiSortBrowse;Sorter på flere kolonner"
                    + ",SelectSort;Velg leverandørinndeling (sortiment)"
                    + ",NewSort;Opprett leverandørinndeling (sortiment)"
                    + ",EditSort;Endre leverandørinndeling"
                    + ",DeleteSort;Slett leverandørinndeling"
                    ,"").   

  hMenuItemSelectSort = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hBrowseSort,"menu-itemSelectSort")).
  hMenuItemNewSort = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hBrowseSort,"menu-itemNewSort")).

  DYNAMIC-FUNCTION("CreateParentLink",hBrowseSort,hBrowseLinje,"ArtikkelNr").

  APPLY "value-changed" TO tbUpdateStat.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitSekvRecord C-Win 
PROCEDURE InitSekvRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF hFieldMap:AVAIL THEN DO WITH FRAME frmLinje:
  IF INT(sokHg:SCREEN-VALUE) LE 0 OR sokHg:SCREEN-VALUE = ? THEN DO:
    DYNAMIC-FUNCTION("DoMessage",0,0,"Initiering av sekvensnummer krever at filter er satt for èn hovedgruppe","","").
    RETURN.
  END.
  ELSE IF DYNAMIC-FUNCTION("DoMessage",0,1,"Start initiering av sekv.nr for hovedgruppe " + sokHG:SCREEN-VALUE,"","") = 1 THEN DO:
    IF DYNAMIC-FUNCTION("runproc","vareboklinje_init_sekv.p",
                         STRING(hFieldMap:BUFFER-FIELD("VarebokNr"):BUFFER-VALUE) + "|"
                       + sokHG:SCREEN-VALUE + "|" 
                       + DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"querysort") + " "
                       + DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"querydesc"),
                         ?) THEN DO:
      DYNAMIC-FUNCTION("setCurrentObject",hBrowseLinje).
      RUN OpenQuery.
    END.
    ELSE DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
  END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InnkjopsprisRecord C-Win 
PROCEDURE InnkjopsprisRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR ocValue     AS CHAR NO-UNDO.
DEF VAR iConfirm    AS INT NO-UNDO.

iReturn = 0.
RUN JBoxBrowseMsgUpdSelVal.w ("Oppdater butikkenes innkjøpspris",
                              hBrowseLinje:NUM-SELECTED-ROWS,
                              DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"Totalcount"),
                              "DECIMAL|>>>,>>9.99|Innkj.pris",
                              OUTPUT ocValue, 
                              OUTPUT iReturn).
IF DEC(ocValue) = 0 AND iReturn NE 0 THEN 
  iConfirm = DYNAMIC-FUNCTION("DoMessage",0,1,"Bekreft at innkjøpspris skal settes til 0","Bekreft","").

IF iReturn NE 0 AND iConfirm NE 2 THEN
  EndreMangeLinjer("InnkjopsPris",ocValue).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KampanjePrisRecord C-Win 
PROCEDURE KampanjePrisRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR ocValue     AS CHAR NO-UNDO.
DEF VAR iConfirm    AS INT NO-UNDO.

iReturn = 0.
RUN JBoxBrowseMsgUpdSelVal.w ("Oppdater kampanjepris",
                              hBrowseLinje:NUM-SELECTED-ROWS,
                              DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"Totalcount"),
                              "DECIMAL|>>>,>>9.99|Kampanjepris",
                              OUTPUT ocValue, 
                              OUTPUT iReturn).
IF DEC(ocValue) = 0 AND iReturn NE 0 THEN 
  iConfirm = DYNAMIC-FUNCTION("DoMessage",0,1,"Bekreft at kampanjepris skal settes til 0","Bekreft","").

IF iReturn NE 0 AND iConfirm NE 2 THEN
  EndreMangeLinjer("KampanjePris",ocValue).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KampanjestotteRecord C-Win 
PROCEDURE KampanjestotteRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR ocValue      AS CHAR NO-UNDO.
DEF VAR iConfirm     AS INT NO-UNDO.

iReturn = 2.

RUN JBoxDSimpleSelectList.w (hBrwOKampanjestotte:LIST-ITEM-PAIRS,?,OUTPUT ocValue).

IF iReturn NE 0 AND iConfirm NE 2 AND ocValue NE ? THEN
  EndreMangeLinjer("Kampanjestotte",ocValue).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KampanjeukerRecord C-Win 
PROCEDURE KampanjeukerRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR ocValue      AS CHAR NO-UNDO.
DEF VAR iConfirm     AS INT NO-UNDO.

iReturn = 2.

RUN JBoxDSimpleSelectList.w (hBrwOKampanjeuke:LIST-ITEM-PAIRS,?,OUTPUT ocValue).

IF iReturn NE 0 AND iConfirm NE 2 AND ocValue NE ? THEN
  EndreMangeLinjer("Kampanjeuker",ocValue).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KjedeInnkPrisRecord C-Win 
PROCEDURE KjedeInnkPrisRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR ocValue     AS CHAR NO-UNDO.
DEF VAR iConfirm    AS INT NO-UNDO.

iReturn = 0.
RUN JBoxBrowseMsgUpdSelVal.w ("Oppdater kjedens innkjøpspris",
                              hBrowseLinje:NUM-SELECTED-ROWS,
                              DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"Totalcount"),
                              "DECIMAL|>>>,>>9.99|Kjedens innk.pris",
                              OUTPUT ocValue, 
                              OUTPUT iReturn).
IF DEC(ocValue) = 0 AND iReturn NE 0 THEN 
  iConfirm = DYNAMIC-FUNCTION("DoMessage",0,1,"Bekreft at kjeden innkjøpspris skal settes til 0","Bekreft","").

IF iReturn NE 0 AND iConfirm NE 2 THEN
  EndreMangeLinjer("KjedeInnkPris",ocValue).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KjedeRab%Record C-Win 
PROCEDURE KjedeRab%Record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR ocValue     AS CHAR NO-UNDO.
DEF VAR iConfirm    AS INT NO-UNDO.

iReturn = 0.
RUN JBoxBrowseMsgUpdSelVal.w ("Oppdater kjederabatt (bonsus%)",
                              hBrowseLinje:NUM-SELECTED-ROWS,
                              DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"Totalcount"),
                              "DECIMAL|>>,>>9.99|Kjederabatt",
                              OUTPUT ocValue, 
                              OUTPUT iReturn).
IF DEC(ocValue) = 0 AND iReturn NE 0 THEN 
  iConfirm = DYNAMIC-FUNCTION("DoMessage",0,1,"Bekreft at kjederabatt (bonus%) skal settes til 0","Bekreft","").

IF iReturn NE 0 AND iConfirm NE 2 THEN
  EndreMangeLinjer("KjedeRab%",ocValue).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KjedeSupInnkPrisRecord C-Win 
PROCEDURE KjedeSupInnkPrisRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR ocValue     AS CHAR NO-UNDO.
DEF VAR iConfirm    AS INT NO-UNDO.

iReturn = 0.
RUN JBoxBrowseMsgUpdSelVal.w ("Oppdater kjedens sup.innkjøpspris",
                              hBrowseLinje:NUM-SELECTED-ROWS,
                              DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"Totalcount"),
                              "DECIMAL|>>>,>>9.99|Kjedens sup.innk.pris",
                              OUTPUT ocValue, 
                              OUTPUT iReturn).
IF DEC(ocValue) = 0 AND iReturn NE 0 THEN 
  iConfirm = DYNAMIC-FUNCTION("DoMessage",0,1,"Bekreft at kjeden sup.innkjøpspris skal settes til 0","Bekreft","").

IF iReturn NE 0 AND iConfirm NE 2 THEN
  EndreMangeLinjer("KjedeSupInnkPris",ocValue).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KjedeSupRab%Record C-Win 
PROCEDURE KjedeSupRab%Record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR ocValue     AS CHAR NO-UNDO.
DEF VAR iConfirm    AS INT NO-UNDO.

iReturn = 0.
RUN JBoxBrowseMsgUpdSelVal.w ("Oppdater kjede sup.rabatt",
                              hBrowseLinje:NUM-SELECTED-ROWS,
                              DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"Totalcount"),
                              "DECIMAL|>>,>>9.99|Kjede sup.rabatt",
                              OUTPUT ocValue, 
                              OUTPUT iReturn).
IF DEC(ocValue) = 0 AND iReturn NE 0 THEN 
  iConfirm = DYNAMIC-FUNCTION("DoMessage",0,1,"Bekreft at kjede sup.rabatt skal settes til 0","Bekreft","").

IF iReturn NE 0 AND iConfirm NE 2 THEN
  EndreMangeLinjer("KjedeSupRab%",ocValue).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KjedeVareRecord C-Win 
PROCEDURE KjedeVareRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR ocValue     AS CHAR NO-UNDO.
DEF VAR iConfirm    AS INT NO-UNDO.

iReturn = 0.
RUN JBoxBrowseMsgUpdSelVal.w ("Oppdater flagg for kjedevare",
                              hBrowseLinje:NUM-SELECTED-ROWS,
                              DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"Totalcount"),
                              "LOGICAL|Ja/Nei|Nei",
                              OUTPUT ocValue, 
                              OUTPUT iReturn).

IF iReturn NE 0 AND iConfirm NE 2 THEN
  EndreMangeLinjer("KjedeVare",ocValue).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KopierPrisForValgteTilVareh C-Win 
PROCEDURE KopierPrisForValgteTilVareh :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cArtNrList AS CHAR NO-UNDO.
DEF VAR cVarebehNr AS CHAR NO-UNDO.

IF NOT hFieldMap:AVAIL THEN RETURN.

cVarebehNr = DYNAMIC-FUNCTION("getFieldValues","VarebehHode",
                                     "WHERE Kilde = " + hFieldMap:BUFFER-FIELD("VarebokNr"):BUFFER-VALUE,
                                     "VarebehNr").
IF cVarebehNr NE ? THEN DO:
  IF DYNAMIC-FUNCTION("DoMessage",0,1,"Bekreft kopiering av priser til varehåndteringsbok nr " + cVarebehNr + CHR(10) +
                                      "for valgte artiker" + CHR(10) +
                                      "Alle andre varehåndteringsbøker for denne messen der artiklene inngår vil også bli oppdatert"
                                     ,"","") NE 1 THEN
    RETURN.
END.
ELSE DO:
  DYNAMIC-FUNCTION("DoMessage",0,0,"Vareboken er ikke overført til varehåndtering","","").
  RETURN.
END.

DO ix = 1 TO hBrowseLinje:NUM-SELECTED-ROWS:
  IF hBrowseLinje:FETCH-SELECTED-ROW(ix) THEN 
    cArtNrList = cArtNrList + STRING(hFieldMapLinje:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) + ",". 
END.
IF NOT DYNAMIC-FUNCTION("RunProc","vareboklinje_kopier_pris_tilvarebeh.p",
                         STRING(hFieldMap:BUFFER-FIELD("VarebokNr"):BUFFER-VALUE) + ";" 
                      + " AND CAN-DO('" + TRIM(cArtNrList,",") + "',STRING(VarebokLinje.ArtikkelNr))" + ";"
                      + cVarebehNr,?) THEN
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil i kopiering av pris for artikler",""). 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KopierTilVarebehandling C-Win 
PROCEDURE KopierTilVarebehandling :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cLookupValue AS CHAR NO-UNDO.
DEF VAR cVarebehNr AS CHAR NO-UNDO.

IF NOT hFieldMap:AVAIL THEN RETURN.

cVarebehNr = DYNAMIC-FUNCTION("getFieldValues","VarebehHode",
                                     "WHERE Kilde = " + hFieldMap:BUFFER-FIELD("VarebokNr"):BUFFER-VALUE,
                                     "VarebehNr").
IF cVarebehNr NE ? THEN DO:
  IF DYNAMIC-FUNCTION("DoMessage",0,1,"Vareboken er allerede kopiert til varehåndtering." + CHR(10) +
                                      "Nye varer i vareboken bli lagt til sist i varehåndgeringbok " + cVarebehNr
                                     ,"","") NE 1 THEN
    RETURN.
END.
ELSE IF DYNAMIC-FUNCTION("DoMessage",0,1,
                          "Varelinjer i varebehandlingsboken vil få et sekvensnummer som tilsvarer" + CHR(10) 
                        + "gjeldende sortering i vareboken. Har du satt sorteringen riktig?" + CHR(10) + CHR(10)
                        + "(Alle varer blir kopiert uavhengig av gjeldende filterkriterium)"
                          ,"","") NE 1 THEN RETURN.

IF NOT DYNAMIC-FUNCTION("RunProc","vareboklinje_kopiertilvarebeh.p",
                         STRING(hFieldMap:BUFFER-FIELD("VarebokNr"):BUFFER-VALUE) + ";;" +
                         DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"QuerySort") + " " + DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"QueryDesc"),
                        ?) THEN
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil i kopiering av varer",""). 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KopierValgteTilVarebeh C-Win 
PROCEDURE KopierValgteTilVarebeh :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cArtNrList AS CHAR NO-UNDO.
DEF VAR cVarebehNr AS CHAR NO-UNDO.

IF NOT hFieldMap:AVAIL THEN RETURN.

cVarebehNr = DYNAMIC-FUNCTION("getFieldValues","VarebehHode",
                                     "WHERE Kilde = " + hFieldMap:BUFFER-FIELD("VarebokNr"):BUFFER-VALUE,
                                     "VarebehNr").
IF cVarebehNr NE ? THEN DO:
  IF DYNAMIC-FUNCTION("DoMessage",0,1,"Vareboken er allerede kopiert til varehåndtering." + CHR(10) +
                                      "Nye varer i vareboken bli lagt til sist i varehåndgeringbok nr " + cVarebehNr
                                     ,"","") NE 1 THEN
    RETURN.
END.
ELSE 
  IF DYNAMIC-FUNCTION("DoMessage",0,1,"Det vil nå bli opprettet en varehåndteringsbok med bare de markerte artiklene" + CHR(10) +
                                      "Varelinjer i varehåndteringsboken vil få et sekvensnummer som tilsvarer"  + CHR(10) +
                                      "gjeldende sortering i vareboken.","","") NE 1 THEN 
    RETURN.


DO ix = 1 TO hBrowseLinje:NUM-SELECTED-ROWS:
  IF hBrowseLinje:FETCH-SELECTED-ROW(ix) THEN 
    cArtNrList = cArtNrList + STRING(hFieldMapLinje:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) + ",". 
END.
IF NOT DYNAMIC-FUNCTION("RunProc","vareboklinje_kopiertilvarebeh.p",
                         STRING(hFieldMap:BUFFER-FIELD("VarebokNr"):BUFFER-VALUE) + ";" + " AND CAN-DO('" + TRIM(cArtNrList,",") + "',STRING(VarebokLinje.ArtikkelNr))" + ";" +
                         DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"QuerySort") + " " + DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"QueryDesc"),
                         ?) THEN
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil i kopiering av varer",""). 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LagerkoderRecord C-Win 
PROCEDURE LagerkoderRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR ocValue      AS CHAR NO-UNDO.
DEF VAR iConfirm     AS INT NO-UNDO.

iReturn = 2.

RUN JBoxDSimpleSelectList.w (hBrwOLagerkode:LIST-ITEM-PAIRS,?,OUTPUT ocValue).

IF iReturn NE 0 AND iConfirm NE 2 AND ocValue NE ? THEN
  EndreMangeLinjer("Lagerkoder",ocValue).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LeaveBrowseFillIn C-Win 
PROCEDURE LeaveBrowseFillIn :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DYNAMIC-FUNCTION("setAttribute",DYNAMIC-FUNCTION("getCurrentObject"),"last-event",LAST-EVENT:LABEL).
RUN SUPER.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LevPrintRecord C-Win 
PROCEDURE LevPrintRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF NOT hFieldMap:AVAIL THEN RETURN.

IF DYNAMIC-FUNCTION("runProc","varebok_levprint.p",
                    DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"BaseQuery") +
                    DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"QueryFilter") +
                    IF DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"QuerySort") NE "" THEN 
                      ";" + DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"QuerySort") + " " + DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"QueryDesc")
                    ELSE ";"
                    ,httRapport) THEN
  LevPrintToExcel(DYNAMIC-FUNCTION("getTransactionMessage"),FALSE).
ELSE DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LevStrKodeSjekkPrintRecord C-Win 
PROCEDURE LevStrKodeSjekkPrintRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF NOT hFieldMap:AVAIL THEN RETURN.

IF DYNAMIC-FUNCTION("runProc","varebok_levprint.p",
                    DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"BaseQuery") +
                    DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"QueryFilter") +
                    (IF DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"QuerySort") NE "" THEN 
                      ";" + DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"QuerySort") + DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"QueryDesc")
                     ELSE ";") + ";yes"
                    ,httRapport) THEN
  LevPrintToExcel(DYNAMIC-FUNCTION("getTransactionMessage"),TRUE).
ELSE DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LevTestPrintRecord C-Win 
PROCEDURE LevTestPrintRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF NOT hFieldMap:AVAIL THEN RETURN.

IF DYNAMIC-FUNCTION("runProc","varebok_levprint.p",
                    DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"BaseQuery") +
                    DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"QueryFilter") +
                    IF DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"QuerySort") NE "" THEN 
                      ";" + DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"QuerySort") + DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"QueryDesc")
                    ELSE ";"
                    ,httRapport) THEN
  LevPrintToExcel(DYNAMIC-FUNCTION("getTransactionMessage"),TRUE).
ELSE DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LevUkeRecord C-Win 
PROCEDURE LevUkeRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR ocValue     AS CHAR NO-UNDO.
DEF VAR ix          AS INT    NO-UNDO.
DEF VAR cYY         AS CHAR   NO-UNDO.
DEF VAR cWW         AS CHAR   NO-UNDO.
DEF VAR oiWeek      AS INT    NO-UNDO.
DEF VAR dFirst      AS DATE   NO-UNDO.
DEF VAR hMnuItem    AS HANDLE NO-UNDO.
DEF VAR bOk         AS LOG    NO-UNDO.

hMnuItem = DYNAMIC-FUNCTION("getCurrentWidget").

iReturn = 2.
RUN JBoxBrowseMsgUpdSelVal.w ("Oppdater " + hMnuItem:LABEL,
                              hBrowseLinje:NUM-SELECTED-ROWS,
                              DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"Totalcount"),
                              "INTEGER|999999",
                              OUTPUT ocValue, 
                              OUTPUT iReturn).

IF iReturn = 0 THEN RETURN.

IF ocValue = '000000' THEN
  DO:
    bOk = FALSE.
    MESSAGE 'Det er ikke angitt noen leveeringsuke. Skal leveringsuke settes til 0?'
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE bOk.
    IF bOk = FALSE THEN RETURN.
    ELSE EndreMangeLinjer("LevDato" + SUBSTRING(hMnuItem:NAME,LENGTH(hMnuItem:NAME)),'?').
  END.
ELSE 
DO: 
    ASSIGN cYY = SUBSTR(ocValue,1,4)
           cWW = SUBSTR(ocValue,5).
    IF INT(cYY) < YEAR(TODAY) OR INT(cYY) > YEAR(TODAY) + 10 OR cWW < "01" OR cWW > "53" THEN DO:
      DYNAMIC-FUNCTION("DoMessage",0,0,"Ugyldig angivelse av uke","","").
      RETURN.
    END.
    DO ix = 1 TO 5:      
      RUN weeknum.p (DATE(1,ix,INT(cYY)), OUTPUT oiWeek).
      IF SUBSTR(STRING(oiWeek),5) = "01" THEN LEAVE.
    END.
    dFirst = DATE(1,ix,INT(cYY)) + INT(cWW) * 7 - 7 NO-ERROR.
    IF NOT ERROR-STATUS:ERROR THEN 
      EndreMangeLinjer("LevDato" + SUBSTRING(hMnuItem:NAME,LENGTH(hMnuItem:NAME)),STRING(dFirst)).
    ELSE DO:
      DYNAMIC-FUNCTION("DoMessage",0,0,"Ugyldig verdi for leveringsuke","","").
      RETURN.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LinjeMerknadRecord C-Win 
PROCEDURE LinjeMerknadRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR ocValue      AS CHAR NO-UNDO.
DEF VAR iConfirm     AS INT NO-UNDO.
DEF VAR cList        AS CHAR NO-UNDO.
DEF VAR cMerknadList AS CHAR NO-UNDO.

iReturn = 2.

cList = DYNAMIC-FUNCTION("getFieldValues","Messe","WHERE MesseNr = " + STRING(hFieldMap:BUFFER-FIELD("MesseNr"):BUFFER-VALUE),"Oppmerking").  
IF cList NE ? THEN DO:      
  DO ix = 1 TO NUM-ENTRIES(cList,"¤"):
    cMerknadList = cMerknadList + 
                   ENTRY(ix,cList,"¤") + "|" +
                   ENTRY(ix,cList,"¤") + "|".
  END.
  cMerknadList = TRIM(cMerknadList,"|").
END.

RUN JBoxDSimpleSelectList.w (cMerknadList,?,OUTPUT ocValue).
/* RUN JBoxBrowseMsgUpdSelVal.w ("Oppdater merknad",                                           */
/*                               hBrowseLinje:NUM-SELECTED-ROWS,                               */
/*                               DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"Totalcount"),   */
/*                               "CHARACTER|x(256)|Merknad",                                   */
/*                               OUTPUT ocValue,                                               */
/*                               OUTPUT iReturn).                                              */
/* IF ocValue = "" AND iReturn NE 0 THEN                                                       */
/*   iConfirm = DYNAMIC-FUNCTION("DoMessage",0,1,"Bekreft merknad skal fjernes","Bekreft",""). */

IF iReturn NE 0 AND iConfirm NE 2 AND ocValue NE ? THEN
  EndreMangeLinjer("LinjeMerknad",ocValue).

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
THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = FALSE.

IF {&WINDOW-NAME}:WINDOW-STATE NE 1 THEN
  {&WINDOW-NAME}:WINDOW-STATE = 3.

{&WINDOW-NAME}:MOVE-TO-TOP().
/* APPLY "entry" TO hBrowse.  */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MultiSortBrowseRecord C-Win 
PROCEDURE MultiSortBrowseRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN SUPER.
IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowseLinje THEN
  StartSpesialSort("fjern_markering_av_valgt_spesialsortering").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MyDeleteMessage C-Win 
PROCEDURE MyDeleteMessage :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAMETER plOk AS LOG NO-UNDO.

  plOk = FALSE.
  IF CAN-DO("1,2",STRING(itab)) THEN
  DO:
      MESSAGE "Er du helt sikker på at du vil slette hele vareboken med alle varelinjene?"
          VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
          UPDATE plOk.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NewRecord C-Win 
PROCEDURE NewRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  
IF iTab = 1 THEN 
  TabStripChanged(12).

IF CAN-DO("1,2",STRING(iTab)) THEN
DO:
  DYNAMIC-FUNCTION("setCurrentObject",hUpdToolbar).
  RUN SUPER.

  DO WITH FRAME frmDetalj:
    ASSIGN
        VareBokNr:SENSITIVE = FALSE
        ProfilNr:SENSITIVE = TRUE 
        btnSokProfilNr:SENSITIVE = TRUE
        btnSokMesseNr:SENSITIVE = TRUE
        .
    APPLY "ENTRY" TO VarebokType.
    RETURN NO-APPLY.
  END.
END.
ELSE IF CAN-DO("3",STRING(iTab)) THEN
DO:
    APPLY "ALT-S" TO CURRENT-WINDOW.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NewSortRecord C-Win 
PROCEDURE NewSortRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VAR rLevBas    AS RECID NO-UNDO.
DEFINE VAR wLevSort   AS RECID NO-UNDO.
DEF VAR cRowIdList    AS CHAR  NO-UNDO.

rLevBas = DYNAMIC-FUNCTION("getRecId","levbas","WHERE LevNr = " + STRING(hFieldMapLinje:BUFFER-FIELD("LevNr"):BUFFER-VALUE)).

IF rLevBas = ? THEN DO:
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
  RETURN.
END.

/* IF hFieldMapLinje:BUFFER-FIELD("StrTypeId"):BUFFER-VALUE = 2 THEN DO:                                          */
/*   DYNAMIC-FUNCTION("DoMessage",0,0,"Artikkel har en størrelstype som kan ikke benyttes for inndeling","","").  */
/*   RETURN.                                                                                                      */
/* END.                                                                                                           */

/* Parameter 4 används vid registrering från w-gridord,
   för att ange StrType */
RUN d-vlevsort.w (INPUT-OUTPUT wLevSort,"NY",rLevBas,hFieldMapLinje:BUFFER-FIELD("StrTypeId"):BUFFER-VALUE).
IF wLevSort <> ? THEN DO:
  cRowIdList = DYNAMIC-FUNCTION("getRowIdList","LevSort","","WHERE RECID(levsort) = " + STRING(wLevSort)).
  IF DYNAMIC-FUNCTION("runproc","artsort_vedlikehold.p",STRING(hFieldMapLinje:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) + "|N|" + cRowIdList,?) THEN DO:
    DYNAMIC-FUNCTION("RefreshRowids",hBrowseLinje,hFieldMapLinje:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE).
    APPLY "value-changed" TO hBrowseLinje.
  END.
  ELSE DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE newVareBokLinje C-Win 
PROCEDURE newVareBokLinje :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR phBuffer AS HANDLE NO-UNDO.

    ASSIGN
        rRowIdLinje = ?
        .

    /* Via AppServer */
    IF DYNAMIC-FUNCTION("getAppserviceHandle") <> ? THEN
    DO:
        RUN newvareboklinje.p ON DYNAMIC-FUNCTION("getAppserviceHandle") 
            (fArtikkelNr,dec(VareBokNr:SCREEN-VALUE IN FRAME frmDetalj),OUTPUT rRowIdLinje) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
        DO:
            IF DYNAMIC-FUNCTION("reConnectServer") THEN
                RUN newVareBokLinje.p ON DYNAMIC-FUNCTION("getAppserviceHandle") 
                    (fArtikkelNr,dec(VareBokNr:SCREEN-VALUE IN FRAME frmDetalj),OUTPUT rRowIdLinje) NO-ERROR.
        END.       
    END.
    /* Direkte */
    ELSE
        RUN newvareboklinje.p (fArtikkelNr,dec(VareBokNr:SCREEN-VALUE IN FRAME frmDetalj),OUTPUT rRowIdLinje) NO-ERROR.
    IF rRowIdLinje <> ? THEN
    DO:
        APPLY "VALUE-CHANGED":U TO hBrowseListe.

        bOk = phBuffer:FIND-FIRST('where rowident1 = "' + STRING(rRowIdLinje) + '"') NO-ERROR.
        IF bOK THEN 
          hBrowseLinje:QUERY:REPOSITION-TO-ROWID(phBuffer:ROWID).

        APPLY "ENTRY":U TO hBrowseLinje.
        RETURN NO-APPLY.
    END.
    ELSE DO:
        MESSAGE RETURN-VALUE
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
        APPLY "ENTRY":U TO hBrowseLinje.
        RETURN NO-APPLY.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NyttBildeLastet C-Win 
PROCEDURE NyttBildeLastet :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ifArtikkelNr AS DEC NO-UNDO.

IF iTab = 3 AND hFieldMapLinje:AVAIL AND hFieldMapLinje:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE = ifArtikkelNr THEN DO:
  DYNAMIC-FUNCTION("RefreshRowids",hBrowseLinje,hFieldMapLinje:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE).  
  APPLY "value-changed" TO hBrowseLinje.
END.

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
DEF VAR cFilter AS CHAR NO-UNDO.

IF iTab = 1 THEN DO WITH FRAME frmFilter:
  ASSIGN fi-fMesseNr:MODIFIED = FALSE 
         fi-fProfilNr:MODIFIED = FALSE
         fi-cVareBokBeskrivelse:MODIFIED = FALSE
         .
  DYNAMIC-FUNCTION("SetCurrentObject",hBrowseListe).
  DYNAMIC-FUNCTION("SetAttribute",hBrowseListe,"queryfilter",
                   (IF fi-fProfilNr:SCREEN-VALUE NE "0" THEN
                     " where ProfilNr = " + fi-fProfilNr:SCREEN-VALUE
                    ELSE "where true") +
                   (IF fi-fMesseNr:SCREEN-VALUE NE "0" THEN
                     " AND MesseNr = " + fi-fMesseNr:SCREEN-VALUE
                    ELSE "") +
                   (IF fi-cVareBokBeskrivelse:SCREEN-VALUE NE "" THEN
                      IF fi-cVareBokBeskrivelse:SCREEN-VALUE BEGINS "*" THEN
                       " AND VareBokBeskrivelse MATCHES '" + fi-cVareBokBeskrivelse:SCREEN-VALUE + "*'"
                      ELSE
                       " AND VareBokBeskrivelse BEGINS '" + fi-cVareBokBeskrivelse:SCREEN-VALUE + "'"
                    ELSE "")
                   ).
  DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"queryfilter"," and ArtikkelNr < 0").
END.
ELSE IF iTab = 3 THEN DO WITH FRAME frmLinje:
  ASSIGN sokFraEdato sokFraVPIdato sokTilEdato sokTilVPIdato tbUpdateStat sokArtikkelnr
         sokAvdelingNavn:MODIFIED = FALSE
         sokAvdelingNr:MODIFIED   = FALSE
         sokHg:MODIFIED           = FALSE
         sokHgBeskr:MODIFIED      = FALSE
         sokVg:MODIFIED           = FALSE
         sokVgBeskr:MODIFIED      = FALSE
         sokAktNr:MODIFIED        = FALSE
         sokAktBeskrivelse:MODIFIED = FALSE
         sokBeskr:MODIFIED        = FALSE
         sokLinjeMerknad:MODIFIED = FALSE
         sokFraEdato:MODIFIED     = FALSE
         sokFraVPIdato:MODIFIED   = FALSE
         sokTilEdato:MODIFIED     = FALSE
         sokTilVPIdato:MODIFIED   = FALSE
         .

  DYNAMIC-FUNCTION("SetCurrentObject",hBrowseLinje).

  IF NOT bCloseQuery THEN DO:
    DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"queryfilter","").

    cFilter =      (IF sokAvdelingNr:SCREEN-VALUE NE "0" THEN
                     " AND AvdelingNr = " + sokAvdelingNr:SCREEN-VALUE
                    ELSE IF sokAvdelingNavn:SCREEN-VALUE NE "" AND NUM-ENTRIES(cAvdelingRowIdList) < 2 THEN
                      " AND " +
                      (IF INDEX(sokAvdelingNavn:SCREEN-VALUE,"*") > 0 THEN 
                        "AvdelingNavn MATCHES '" + sokAvdelingNavn:SCREEN-VALUE + "*'"
                       ELSE
                        "AvdelingNavn BEGINS '" + sokAvdelingNavn:SCREEN-VALUE + "'")
                    ELSE IF sokAvdelingNavn:SCREEN-VALUE NE "" AND NUM-ENTRIES(cAvdelingRowIdList) > 1 THEN
                      " AND CAN-DO('" + REPLACE(cAvdelingIdList,"|",",") + "',STRING(AvdelingNr))"
                    ELSE "")
                 + (IF sokHg:SCREEN-VALUE NE "0" THEN
                     " AND Hg = " + sokHg:SCREEN-VALUE
                    ELSE IF sokHgBeskr:SCREEN-VALUE NE "" AND NUM-ENTRIES(cHuvGrRowIdList) < 2 THEN
                     " AND " +
                     (IF INDEX(sokHgBeskr:SCREEN-VALUE,"*") > 0 THEN
                       "HgBeskr MATCHES '" + sokHgBeskr:SCREEN-VALUE + "*'"
                      ELSE
                       "HgBeskr BEGINS '" + sokHgBeskr:SCREEN-VALUE + "'")
                    ELSE IF sokHgBeskr:SCREEN-VALUE NE "" AND NUM-ENTRIES(cHuvGrRowIdList) > 1 THEN
                      " AND CAN-DO('" + REPLACE(cHuvGrIdList,"|",",") + "',STRING(Hg))"
                    ELSE "") 
                 + (IF sokVg:SCREEN-VALUE NE "0" THEN
                     " AND Vg = " + sokVg:SCREEN-VALUE
                    ELSE IF sokVgBeskr:SCREEN-VALUE NE "" AND NUM-ENTRIES(cVarGrRowIdList) < 2 THEN
                      " AND " +
                     (IF INDEX(sokVgBeskr:SCREEN-VALUE,"*") > 0 THEN
                       "VgBeskr MATCHES '" + sokVgBeskr:SCREEN-VALUE + "*'"
                      ELSE
                       "VgBeskr BEGINS '" + sokVgBeskr:SCREEN-VALUE + "'")
                    ELSE IF sokVgBeskr:SCREEN-VALUE NE "" AND NUM-ENTRIES(cVarGrRowIdList) > 1 THEN
                      " AND CAN-DO('" + REPLACE(cVarGrIdList,"|",",") + "',STRING(Vg))"
                    ELSE "") 
                 + (IF sokLevNr:SCREEN-VALUE NE "0" THEN
                     " AND LevNr = " + sokLevNr:SCREEN-VALUE
                    ELSE IF sokLevNamn:SCREEN-VALUE NE "" AND NUM-ENTRIES(cLevBasRowIdList) < 2 THEN
                      " AND " +
                     (IF INDEX(sokLevNamn:SCREEN-VALUE,"*") > 0 THEN
                       "LevNamn MATCHES '" + sokLevNamn:SCREEN-VALUE + "*'"
                      ELSE
                       "LevNamn BEGINS '" + sokLevNamn:SCREEN-VALUE + "'")
                    ELSE IF sokLevNamn:SCREEN-VALUE NE "" AND NUM-ENTRIES(cLevBasRowIdList) > 1 THEN
                      " AND CAN-DO('" + REPLACE(cLevBasIdList,"|",",") + "',STRING(LevNr))"
                    ELSE "")
                 + (IF sokBeskr:SCREEN-VALUE NE "" THEN
                     " AND Utvidetsok CONTAINS '" + sokBeskr:SCREEN-VALUE + "'"
                    ELSE "") 
                 + (IF sokLevKod:SCREEN-VALUE NE "" THEN
                     " AND LevKod " + 
                     (IF INDEX(sokLevKod,"*") > 0 THEN 
                       "MATCHES '" + sokLevKod:SCREEN-VALUE + "*'"
                      ELSE 
                       "BEGINS '" + sokLevKod:SCREEN-VALUE + "'")
                    ELSE "") 
                 + (IF sokLevFargKod:SCREEN-VALUE NE "" THEN
                     " AND LevFargKod " + 
                     (IF INDEX(sokLevFargKod,"*") > 0 THEN 
                       "MATCHES '" + sokLevFargKod:SCREEN-VALUE + "*'"
                      ELSE 
                       "BEGINS '" + sokLevFargKod:SCREEN-VALUE + "'")
                    ELSE "") 
                 + (IF sokArtikkelnr NE 0 THEN
                     " AND ArtikkelNr = DEC('" + sokArtikkelNr:SCREEN-VALUE + "')"
                    ELSE "") 
                 
/*                  + (IF sokLinjeMerknad:SCREEN-VALUE NE "" AND sokLinjeMerknad:SCREEN-VALUE NE ? THEN   */
/*                    " AND CAN-DO(LinjeMerknad,'" + sokLinjeMerknad:SCREEN-VALUE + "')"                  */
/* /*                      " AND LOOKUP(LinjeMerknad,'" + sokLinjeMerknad:SCREEN-VALUE + "','|') > 0" */  */
/*                     ELSE "")                                                                           */

                 + (IF sokFraEdato NE ? THEN
                     " AND Edato GE DATE('" + sokFraEdato:SCREEN-VALUE + "')"
                    ELSE "")
                 + (IF sokTilEdato NE ? THEN
                     " AND Edato LE DATE('" + sokTilEdato:SCREEN-VALUE + "')"
                    ELSE "")
                 + (IF sokFraVPIdato NE ? THEN
                     " AND VPIdato GE DATE('" + sokFraVPIdato:SCREEN-VALUE + "')"
                    ELSE "")
                 + (IF sokTilVPIdato NE ? THEN
                     " AND VPIdato LE DATE('" + sokTilVPIdato:SCREEN-VALUE + "')"
                    ELSE "")
                 + (IF tbIkkeMerknad:CHECKED THEN
                     " AND LinjeMerknad = ''"
                    ELSE "")
                    .
    DO ix = 1 TO NUM-ENTRIES(sokLinjeMerknad:SCREEN-VALUE):
      cFilter = cFilter + " AND CAN-DO(LinjeMerknad,'" + ENTRY(ix,sokLinjeMerknad:SCREEN-VALUE) + "')".
    END.

    IF cFilter = "" THEN cFilter = cFilter + " USE-INDEX VarebokArtNrLevNamn ".
/*     ELSE IF cFilter MATCHES ("*levnr*") THEN cFilter = cFilter + " USE-INDEX LevNr ". */
    cFilter = " AND ArtikkelNr > 0 " + cFilter.              
    DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"QueryFilter",cFilter).

    IF sokAktNr:SCREEN-VALUE NE "0" THEN
      DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"queryjoin",cVareboklinjeJoin +
                                      ",first VgAkt WHERE VgAkt.Vg = VarebokLinje.Vg AND AktNr = " + sokAktNr:SCREEN-VALUE 
                                    + ",first Aktivitet OF VgAkt"
                                    + ",FIRST StrType OF ArtBas NO-LOCK"
                                    + ",FIRST ArtSort OF ArtBas NO-LOCK OUTER-JOIN"
                                    + ",FIRST Regnskapsavdeling OF ArtBas NO-LOCK OUTER-JOIN"
                       ).
    ELSE IF sokAktBeskrivelse:SCREEN-VALUE NE "" THEN
      DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"queryjoin",cVareboklinjeJoin +
                                      ",EACH VgAkt WHERE VgAkt.Vg = VarebokLinje.Vg" 
                                    + ",first Aktivitet OF VgAkt WHERE Beskrivelse " 
                                    + (IF INDEX(sokAktBeskrivelse:SCREEN-VALUE,"*") > 0 THEN 
                                        "MATCHES '" + sokAktBeskrivelse:SCREEN-VALUE + "*'"
                                       ELSE
                                        "BEGINS '" + sokAktBeskrivelse:SCREEN-VALUE + "'")
                                    + ",FIRST StrType OF ArtBas NO-LOCK"
                                    + ",FIRST ArtSort OF ArtBas NO-LOCK OUTER-JOIN"
                                    + ",FIRST Regnskapsavdeling OF ArtBas NO-LOCK OUTER-JOIN"
                       ).
    ELSE
      DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"queryjoin",cVareboklinjeJoin + cAktivitetJoin 
                                  + ",FIRST StrType OF ArtBas NO-LOCK"
                                  + ",FIRST ArtSort OF ArtBas NO-LOCK"
                                  + (IF tbValgtInndeling:CHECKED THEN "" ELSE " OUTER-JOIN")
                                  + ",FIRST Regnskapsavdeling OF ArtBas NO-LOCK OUTER-JOIN"
                       ).

    IF tbUpdateStat AND NOT tbForventet:CHECKED THEN
      DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"querystatfields",cStatFieldNames).
    ELSE DO:
      DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"querystatfields","").
      ViewStat(FALSE).
    END.
  END.
END.

RUN SUPER.

RUN SettSensitive.

IF iTab = 1 THEN DO:
  IF NOT hFieldMap:AVAIL AND hBrowseListe:QUERY:IS-OPEN THEN
    hBrowseListe:QUERY:GET-FIRST().
  APPLY "entry" TO hBrowseListe.
END.
ELSE IF iTab = 3 THEN DO:
  IF tbUpdateStat THEN ViewStat(TRUE).
  APPLY "entry" TO hBrowseLinje.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppdaterViaExcelRecord C-Win 
PROCEDURE OppdaterViaExcelRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iReturn AS INT NO-UNDO.
  IF NOT VALID-HANDLE(hOppdaterViaExcel) THEN
  DO:
    RUN vareboklinje_oppdaterviaexcel.w PERSISTENT SET hOppdaterViaExcel.
    RUN setParameter IN hOppdaterViaExcel (INPUT hFieldMap, INPUT hBrowseLinje,INPUT hFieldMapLinje).
    RUN initializeObject IN hOppdaterViaExcel.
  END.
  ELSE
  DO:
    RUN MoveToTop IN hOppdaterViaExcel.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OverforTilAnnenVB C-Win 
PROCEDURE OverforTilAnnenVB :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cRowIdList  AS CHAR NO-UNDO.

IF NOT hFieldMap:AVAIL THEN RETURN.

THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
RUN JBoxSelector.w (THIS-PROCEDURE,0,
                    "VareBokHode;VareBokNr;VareBokBeskrivelse;MesseNr,Messe;MesseBeskrivelse",
                    "WHERE VarebokNr NE " + STRING(hFieldMap:BUFFER-FIELD("VarebokNr"):BUFFER-VALUE) + ",FIRST Messe OF vareBokHode",
                    INPUT-OUTPUT cVareBokRowIdList,
                    "VareBokNr",
                    INPUT-OUTPUT cVareBokIdList,
                    "","",
                    OUTPUT bOK).
THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

IF bOK THEN DO:
  bOK = DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowseLinje,"vareboklinje_kopiertilannenvb.p",
                              cVareBokIdList).
  /*
  DO ix = 1 TO hBrowseLinje:NUM-SELECTED-ROWS:
    IF hBrowseLinje:FETCH-SELECTED-ROW(ix) THEN 
      cRowIdList = cRowIdList + hFieldMapLinje:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE + "|". 
  END.
  bOK = DYNAMIC-FUNCTION("RunProc","vareboklinje_kopiertilannenvb.p",
                              cVareBokIdList
                            + ",ROWID," + TRIM(cRowIdList,"|"),
                              ?).
  */                              
END.
ELSE RETURN.

IF NOT bOK THEN
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil i kopiering av varer",""). 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OverforVPI C-Win 
PROCEDURE OverforVPI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR ocValue AS CHAR NO-UNDO.
iReturn = 0.
RUN JBoxBrowseMsgUpdateVal.w ("Overføre artikler til ERP",
                              hBrowseLinje:NUM-SELECTED-ROWS,
                              IF INT(DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"Totalcount")) LT INT(DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"rowstobatch")) THEN
                                INT(DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"Totalcount"))
                              ELSE 99999,
                              "",
                              OUTPUT ocValue, 
                              OUTPUT iReturn).
IF iReturn = 0 THEN RETURN.

IF iReturn = 1 THEN DO:
  IF NOT DYNAMIC-FUNCTION("processQuery",hBrowseLinje,"vareboklinje_overfor_til_erp.p",
                         STRING(hFieldMap:BUFFER-FIELD("VarebokNr"):BUFFER-VALUE)) THEN 
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil i overføring til ERP","").
  ELSE
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
END.
ELSE IF iReturn = 2 THEN DO:
  DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"RefreshProcessedRows","no").
  IF NOT DYNAMIC-FUNCTION("processSelectedRows",hBrowseLinje,"vareboklinje_overfor_til_erp.p",
                         STRING(hFieldMap:BUFFER-FIELD("VarebokNr"):BUFFER-VALUE)) THEN 
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil i overføring til ERP","").
  ELSE
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OverforVPIERPAlle C-Win 
PROCEDURE OverforVPIERPAlle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF NOT hFieldMap:AVAIL THEN RETURN.

IF DYNAMIC-FUNCTION("DoMessage",0,1,"Bekreft overføring av vare og prisinformasjon for ALLE varelinjene til ERP - Varebok nr " + STRING(hFieldMap:BUFFER-FIELD("VarebokNr"):BUFFER-VALUE) + CHR(10)
                                   ,"","") NE 1 THEN
  RETURN.

IF NOT DYNAMIC-FUNCTION("RunProc","vareboklinje_overfor_til_erp.p",
                         STRING(hFieldMap:BUFFER-FIELD("VarebokNr"):BUFFER-VALUE),
                         ?) THEN
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil i overføring av VPI til ERP",""). 
ELSE MESSAGE DYNAMIC-FUNCTION("getTransactionMessage")
      VIEW-AS ALERT-BOX INFO BUTTONS OK.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OverforVPIERPValgte C-Win 
PROCEDURE OverforVPIERPValgte :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cArtNrList AS CHAR NO-UNDO.

IF NOT hFieldMap:AVAIL THEN RETURN.

IF DYNAMIC-FUNCTION("DoMessage",0,1,"Bekreft overføring av vare og prisinformasjon til ERP for valgte artikler - Varebok nr " + STRING(hFieldMap:BUFFER-FIELD("VarebokNr"):BUFFER-VALUE) + CHR(10)
                                   ,"","") NE 1 THEN
  RETURN.


DO ix = 1 TO hBrowseLinje:NUM-SELECTED-ROWS:
  IF hBrowseLinje:FETCH-SELECTED-ROW(ix) THEN 
    cArtNrList = cArtNrList + STRING(hFieldMapLinje:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) + ",". 
END.                              
IF NOT DYNAMIC-FUNCTION("RunProc","vareboklinje_overfor_til_erp.p",
                         STRING(hFieldMap:BUFFER-FIELD("VarebokNr"):BUFFER-VALUE) + ";" + " AND CAN-DO('" + TRIM(cArtNrList,",") + "',STRING(VarebokLinje.ArtikkelNr))",
                         ?) THEN
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil i overføring til ERP",""). 
ELSE MESSAGE DYNAMIC-FUNCTION("getTransactionMessage")
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrevNext C-Win 
PROCEDURE PrevNext :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER cRettning AS CHARACTER  NO-UNDO.

IF CAN-DO("Prev,Next",cRettning) THEN DO:
  hBrowseLinje:SELECT-FOCUSED-ROW().
  CASE cRettning:
      WHEN "Prev" THEN
          hBrowseLinje:SELECT-PREV-ROW().
      WHEN "Next" THEN
        hBrowseLinje:SELECT-NEXT-ROW().
  END CASE.
  APPLY "value-changed" TO hBrowseLinje.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrintRecord C-Win 
PROCEDURE PrintRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DYNAMIC-FUNCTION("DoMessage",0,0,"Velg hvilken rapport du vil kjøre ved å klikke med høyre musetast på print-knappen","","").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrisRecord C-Win 
PROCEDURE PrisRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR ocValue     AS CHAR NO-UNDO.
DEF VAR iConfirm    AS INT NO-UNDO.

iReturn = 0.
RUN JBoxBrowseMsgUpdSelVal.w ("Oppdater markedspris",
                              hBrowseLinje:NUM-SELECTED-ROWS,
                              DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"Totalcount"),
                              "DECIMAL|>>>,>>9.99|Markedspris",
                              OUTPUT ocValue, 
                              OUTPUT iReturn).
IF DEC(ocValue) = 0 AND iReturn NE 0 THEN 
  iConfirm = DYNAMIC-FUNCTION("DoMessage",0,1,"Bekreft at markedspris skal settes til 0","Bekreft","").

IF iReturn NE 0 AND iConfirm NE 2 THEN
  EndreMangeLinjer("Pris",ocValue).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RefreshArtRecord C-Win 
PROCEDURE RefreshArtRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cLabelList AS CHAR NO-UNDO.
DEF VAR cFieldList AS CHAR NO-UNDO.

IF NOT hFieldMap:AVAIL THEN RETURN.

RUN VarebokLinjeDVelgFelter.w (OUTPUT cFieldList,OUTPUT cLabelList,OUTPUT iReturn).

IF iReturn NE 1 THEN RETURN.

iReturn = DYNAMIC-FUNCTION("DoMessage",0,1,"Bekreft oppfrisking av artikkelinformasjon for alle artikler i vareboken" + CHR(10) +
                                           "(også andre varebøker for messen der en artikkel i denne vareboken inngår)" + CHR(10) +
                                           "Felter som oppdateres: " + CHR(10) + CHR(10) + cLabelList
                                           ,"Bekreft","").

IF iReturn = 1 THEN DO:
  bOK = DYNAMIC-FUNCTION("RunProc","vareboklinje_refresh_all.p",STRING(hFieldMap:BUFFER-FIELD("VarebokNr"):BUFFER-VALUE) + ";;" + cFieldList
                         ,?).
  IF NOT bOK THEN
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil i oppfrisking av artikkelinformasjon",""). 
  ELSE RUN OpenQuery.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RefreshArtRecordValgte C-Win 
PROCEDURE RefreshArtRecordValgte :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cArtNrList AS CHAR NO-UNDO.
DEF VAR cLabelList AS CHAR NO-UNDO.
DEF VAR cFieldList AS CHAR NO-UNDO.

IF NOT hFieldMap:AVAIL THEN RETURN.

RUN VarebokLinjeDVelgFelter.w (OUTPUT cFieldList,OUTPUT cLabelList,OUTPUT iReturn).

IF iReturn NE 1 THEN RETURN.

iReturn = DYNAMIC-FUNCTION("DoMessage",0,1,"Bekreft oppfrisking av artikkelinformasjon for valgte artikler i vareboken" + CHR(10) +
                                           "(også andre varebøker for messen der en artikkel i denne vareboken inngår)" + CHR(10) +
                                           "Felter som oppdateres: " + CHR(10) + CHR(10) + cLabelList
                                           ,"Bekreft","").
IF iReturn = 1 THEN 
LOOPEN:
DO:
  /* Bygger liste med artikler som skal ha frisket opp artikkelinformasjonen. */
  DO ix = 1 TO hBrowseLinje:NUM-SELECTED-ROWS:
    IF hBrowseLinje:FETCH-SELECTED-ROW(ix) THEN 
      cArtNrList = cArtNrList + STRING(hFieldMapLinje:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) + ",". 
  END.
  IF cArtNrList = "" THEN
  DO:
      DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Ingen artikler valgt. Funksjon avbrutt",""). 
      LEAVE LOOPEN.
  END.

  bOk = DYNAMIC-FUNCTION("RunProc","vareboklinje_refresh_all.p",
                           STRING(hFieldMap:BUFFER-FIELD("VarebokNr"):BUFFER-VALUE) + ";" + TRIM(cArtNrList,",") + ";" + cFieldList,
                           ?).
  IF NOT bOk THEN
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil i oppfriskning av artikkelinformasjon",""). 
  ELSE
      RUN OpenQuery.
END. /* LOOPEN */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE resetLinjeBrowse C-Win 
PROCEDURE resetLinjeBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    APPLY "VALUE-CHANGED":U TO hBrowseListe.
    APPLY "ENTRY":U TO hBrowseLinje.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RowDisplayBrowse C-Win 
PROCEDURE RowDisplayBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowseLinje AND hFieldRGBcolor:BUFFER-VALUE > 0 THEN
  hBrwColArtBeskr:BGCOLOR = setFieldColor(hFieldRGBcolor:BUFFER-VALUE).
/*
IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowseLinje THEN 
  ASSIGN hBrwColKjedeVare:FONT   = iFontWingdings
         hBrwColKjedeVare:FORMAT = CHR(254) + "/"  + CHR(168)
         hBrwColGjFakt:FONT      = iFontWingdings
         hBrwColGjFakt:FORMAT    = CHR(254) + "/"  + CHR(168).
*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RowsToBatchRecord C-Win 
PROCEDURE RowsToBatchRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cRowsToBatch AS CHAR NO-UNDO.

RUN JBoxDSelectRowsToBatch.w (DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"rowsToBatch"), OUTPUT cRowsToBatch).

IF cRowsToBatch NE "" THEN DO:
  DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"rowsToBatch",cRowsToBatch).
  DYNAMIC-FUNCTION("setCustomWinSettings",THIS-PROCEDURE:CURRENT-WINDOW,"<StartRowsToBatch>|brwLinje|" + cRowsToBatch + "|<EndRowsToBatch>").
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saveRecord C-Win 
PROCEDURE saveRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cState     AS CHAR                      NO-UNDO.

cState = DYNAMIC-FUNCTION('getToolbarState',hUpdToolBar).

IF iTab = 2 THEN DO WITH FRAME frmDetalj:

  IF int(ProfilNr:screen-value) = 0 THEN
  DO:
      MESSAGE "Prisprofil ikke angitt."
          VIEW-AS ALERT-BOX WARNING BUTTONS OK.
      APPLY "ENTRY" TO ProfilNr.
      RETURN NO-APPLY.
  END.
  IF DYNAMIC-FUNCTION("getFieldValues","PrisProfil","WHERE ProfilNr = int(" + ProfilNr:SCREEN-VALUE + ")","KortNavn") = ? THEN
  DO:
      MESSAGE "Ukjent prisprofil " + ProfilNr:SCREEN-VALUE + "."
          VIEW-AS ALERT-BOX WARNING BUTTONS OK  TITLE "Lagringskontroll".
      APPLY "ENTRY" TO ProfilNr.
      RETURN NO-APPLY.
  END.
  IF (VareBokBeskrivelse:screen-value) = "" THEN
  DO:
      MESSAGE "Varebok mangler beskrivelse."
          VIEW-AS ALERT-BOX WARNING BUTTONS OK.
      APPLY "ENTRY" TO VareBokBeskrivelse.
      RETURN NO-APPLY.
  END.
        
  ASSIGN
      VareBokNr:SENSITIVE      = FALSE
      ProfilNr:SENSITIVE       = FALSE
      btnSokProfilNr:SENSITIVE = FALSE
      .

END.

RUN SUPER.

IF iTab = 2 THEN DO:
  IF  cState = "new" THEN 
    RUN DisplayRecord.
/*   cOppmerking = "".                                                                                                                                                  */
/*   IF hFieldMap:BUFFER-FIELD("MesseNr"):BUFFER-VALUE NE 0 THEN                                                                                                        */
/*     settOppmerking(DYNAMIC-FUNCTION("getFieldValues","Messe","WHERE MesseNr = DEC('" + STRING(hFieldMap:BUFFER-FIELD("MesseNr"):BUFFER-VALUE) + "')","Oppmerking")). */
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SelectSortRecord C-Win 
PROCEDURE SelectSortRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cRowIdList AS CHAR NO-UNDO.
DEF VAR cIdList    AS CHAR NO-UNDO.

IF hBrowseSort:QUERY:IS-OPEN THEN DO:
  hBrowseSort:QUERY:GET-FIRST().
  REPEAT WHILE NOT hBrowseSort:QUERY:QUERY-OFF-END:
    cRowIdList = cRowIdList + hBrowseSort:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("RowIdent2"):BUFFER-VALUE + ",".
    hBrowseSort:QUERY:GET-NEXT().
  END.
  cRowIdList = TRIM(cRowIdList,",").
END.

THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
RUN JBoxSelector.w (THIS-PROCEDURE,0,
                    "LevSort"
                    + ";SortId"
                    + ";+StrInterv|CHARACTER|x(40)|levsort_intervall.p|Intervall"
                    + ";+StrFord|CHARACTER|x(40)|levsort_fordeling.p|Fordeling"
                    + ";+SumAntStr|INTEGER|>>9|levsort_sumant.p|Tot"
                    + ";!LevNr;!StrTypeId"
                    ,"where LevNr GE 0 AND StrTypeId = " + STRING(hFieldMapLinje:BUFFER-FIELD("StrTypeId"):BUFFER-VALUE),
                    INPUT-OUTPUT cRowIdList,
                    "SortId",
                    INPUT-OUTPUT cIdList,
                    "","",
                    OUTPUT bOK).
THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

IF bOk THEN DO:
  IF DYNAMIC-FUNCTION("runproc","artsort_vedlikehold.p",STRING(hFieldMapLinje:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) + "|Y|" + cRowIdList,?) THEN DO:
    DYNAMIC-FUNCTION("RefreshRowids",hBrowseLinje,hFieldMapLinje:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE).
    APPLY "value-changed" TO hBrowseLinje.
  END.
  ELSE DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
END.

hMenuItemNewSort:SENSITIVE = TRUE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SesongRecord C-Win 
PROCEDURE SesongRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR ocValue     AS CHAR NO-UNDO.
DEF VAR iConfirm    AS INT NO-UNDO.

RUN JBoxDSimpleSelectList.w (DYNAMIC-FUNCTION("getFieldList","SaSong;SasBeskr;SaSong","WHERE true"),?,OUTPUT ocValue).

IF ocValue NE ? THEN DO:
  iReturn = 2.
  EndreMangeLinjer("SaSong",ocValue).
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
DEF INPUT PARAM ihBrwSource AS HANDLE NO-UNDO.
DEF INPUT PARAM ihBrwTarget AS HANDLE NO-UNDO.

DEF VAR hSourceBuffer  AS HANDLE NO-UNDO.
DEF VAR hTargetBuffer  AS HANDLE NO-UNDO.
DEF VAR iy             AS INT    NO-UNDO.
DEF VAR cSelectedRows  AS CHAR   NO-UNDO.
DEF VAR hHandle        AS HANDLE NO-UNDO.
DEF VAR hPanelFrame    AS HANDLE NO-UNDO.
DEF VAR hSelector      AS HANDLE NO-UNDO.
DEF VAR cMerknadsKoder AS CHAR   NO-UNDO.

DEF VAR cList          AS CHAR NO-UNDO.
DEF VAR cMerknadList   AS CHAR NO-UNDO.

IF (CAN-DO("_file,_field",ihBrwSource:QUERY:GET-BUFFER-HANDLE(1):NAME) OR 
    ihBrwSource:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD(1):NAME = "dummy")
    AND hFieldMap:AVAIL THEN DO:
  cList = DYNAMIC-FUNCTION("getFieldValues","Messe","WHERE MesseNr = " + STRING(hFieldMap:BUFFER-FIELD("MesseNr"):BUFFER-VALUE),"Oppmerking").  
  IF cList NE ? THEN DO:      
    DO ix = 1 TO NUM-ENTRIES(cList,"¤"):
      cMerknadList = cMerknadList + 
                     ENTRY(ix,cList,"¤") + "|" +
                     ENTRY(ix,cList,"¤") + "|".
      DO iy = 1 TO NUM-ENTRIES(ENTRY(ix,cList,"¤")):
        IF NOT CAN-DO(cMerknadsKoder,ENTRY(iy,ENTRY(ix,cList,"¤"))) THEN
          cMerknadsKoder = cMerknadsKoder + (IF cMerknadsKoder NE "" THEN "," ELSE "") + ENTRY(iy,ENTRY(ix,cList,"¤")).
      END.
    END.
    cOppmerking = TRIM(cMerknadList,"|").
  END.
END.

ASSIGN hSourceBuffer = ihBrwSource:QUERY:GET-BUFFER-HANDLE(1)
       hTargetBuffer = ihBrwTarget:QUERY:GET-BUFFER-HANDLE(1).

IF ihBrwSource:QUERY:GET-BUFFER-HANDLE(1):NAME = "LevSort" THEN 
  ASSIGN ihBrwSource:GET-BROWSE-COLUMN(1):WIDTH-PIXELS = 40
         ihBrwSource:GET-BROWSE-COLUMN(2):WIDTH-PIXELS = 100
         ihBrwSource:GET-BROWSE-COLUMN(3):WIDTH-PIXELS = 100
         ihBrwTarget:GET-BROWSE-COLUMN(1):WIDTH-PIXELS = 40
         ihBrwTarget:GET-BROWSE-COLUMN(2):WIDTH-PIXELS = 100
         ihBrwTarget:GET-BROWSE-COLUMN(3):WIDTH-PIXELS = 100
         .
ELSE IF ihBrwSource:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD(1):NAME = "dummy" THEN DO:

  DYNAMIC-FUNCTION("setAttribute",ihBrwSource,"basequery","where true").
  DYNAMIC-FUNCTION("setAttribute",ihBrwSource,"querysort","Description").
  DYNAMIC-FUNCTION("setAttribute",ihBrwSource,"uselocaldata","yes").
  DYNAMIC-FUNCTION("setAttribute",ihBrwTarget,"uselocaldata","yes").


  /* Fill temp-table: */
  DO ix = 1 TO NUM-ENTRIES(cMerknadsKoder):
    hSourceBuffer:BUFFER-CREATE().
    ASSIGN hSourceBuffer:BUFFER-FIELD("Description"):BUFFER-VALUE = ENTRY(ix,cMerknadsKoder)
           hSourceBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE = "rowid" + STRING(ix)
           .
  END.

/*   DO ix = 1 TO NUM-ENTRIES(cOppmerking,"|") BY 2:                                                        */
/*     hSourceBuffer:BUFFER-CREATE().                                                                       */
/*     ASSIGN hSourceBuffer:BUFFER-FIELD("Description"):BUFFER-VALUE = ENTRY(ix,cOppmerking,"|")            */
/*            hSourceBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE = "rowid" + STRING(ix)                   */
/*            .                                                                                             */
/*                                                                                                          */
/*     /* If any records should be pre-selected: */                                                         */
/*     IF LOOKUP(ENTRY(ix,cOppmerking,"|"),sokLinjeMerknad:SCREEN-VALUE IN FRAME frmLinje,"|") > 0 THEN DO: */
/*       hTargetBuffer:BUFFER-CREATE().                                                                     */
/*       ASSIGN hTargetBuffer:BUFFER-FIELD("Description"):BUFFER-VALUE = ENTRY(ix,cOppmerking,"|")          */
/*              hTargetBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE = "rowid" + STRING(ix)                 */
/*              cSelectedRows = cSelectedRows + "rowid" + STRING(ix) + ","                                  */
/*              .                                                                                           */
/*     END.                                                                                                 */
/*   END.                                                                                                   */
/*                                                                                                          */
/*   DYNAMIC-FUNCTION("setSelectedRowids" IN SOURCE-PROCEDURE,cSelectedRows).                               */

  DYNAMIC-FUNCTION("setCurrentObject",ihBrwSource).
  RUN OpenQuerySource IN SOURCE-PROCEDURE.

  DYNAMIC-FUNCTION("setCurrentObject",ihBrwTarget).
  RUN OpenQueryTarget IN SOURCE-PROCEDURE.
END.

ELSE IF ihBrwSource:QUERY:GET-BUFFER-HANDLE(1):NAME = "_field" THEN DO:

  DYNAMIC-FUNCTION("setAttribute",ihBrwSource,"basequery","where true").
  DYNAMIC-FUNCTION("setAttribute",ihBrwSource,"querysort","Description").
  DYNAMIC-FUNCTION("setAttribute",ihBrwSource,"uselocaldata","yes").
  DYNAMIC-FUNCTION("setAttribute",ihBrwTarget,"uselocaldata","yes").

  cOppmerking = TRIM(hBrwOLMerk:LIST-ITEM-PAIRS,"|").

  /* Fill temp-table: */
  DO ix = 1 TO NUM-ENTRIES(cOppmerking,"|") BY 2:
    DO iy = 1 TO NUM-ENTRIES(ENTRY(ix,cOppmerking,"|")):
      IF NOT CAN-DO(cList,ENTRY(iy,ENTRY(ix,cOppmerking,"|"))) THEN DO:
        hSourceBuffer:BUFFER-CREATE().
        ASSIGN hSourceBuffer:BUFFER-FIELD("Description"):BUFFER-VALUE = ENTRY(iy,ENTRY(ix,cOppmerking,"|"))
               hSourceBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE = "rowid" + STRING(ix)
               cList = cList + ENTRY(iy,ENTRY(ix,cOppmerking,"|")) + ","
               .
      END.
    END.
  END.

  DYNAMIC-FUNCTION("setSelectedRowids" IN SOURCE-PROCEDURE,cSelectedRows).

  DYNAMIC-FUNCTION("setCurrentObject",ihBrwSource).
  RUN OpenQuerySource IN SOURCE-PROCEDURE.

  DYNAMIC-FUNCTION("setCurrentObject",ihBrwTarget).
  RUN OpenQueryTarget IN SOURCE-PROCEDURE.
END.

ELSE IF ihBrwSource:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD(1):NAME = "FieldName" THEN DO:

  hSelector = SOURCE-PROCEDURE.
  RUN LoadPanel IN SOURCE-PROCEDURE ("VarebokSortPanel.w",OUTPUT hSortPanel).

  hHandle = DYNAMIC-FUNCTION("getBrowseColumn",ihBrwSource,"SortAsc").
  IF VALID-HANDLE(hHandle) THEN hHandle:VISIBLE = NO.

  hHandle = DYNAMIC-FUNCTION("getBrowseColumn",ihBrwTarget,"FieldPosition").
  IF VALID-HANDLE(hHandle) THEN hHandle:VISIBLE = NO.

  hHandle = DYNAMIC-FUNCTION("getBrowseColumn",ihBrwTarget,"FieldLabel").
  IF VALID-HANDLE(hHandle) THEN hHandle:WIDTH-PIXELS = hHandle:WIDTH-PIXELS - 50.

  cList = DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"allcalcfields").
  DO ix = 1 TO hBrowseLinje:NUM-COLUMNS:
    IF hBrowseLinje:GET-BROWSE-COLUMN(ix):VISIBLE AND NOT CAN-DO(cList,hBrowseLinje:GET-BROWSE-COLUMN(ix):NAME)
       AND CAN-DO(cSpesialSortFields,hBrowseLinje:GET-BROWSE-COLUMN(ix):NAME) THEN DO:
      hSourceBuffer:BUFFER-CREATE().
      ASSIGN hSourceBuffer:BUFFER-FIELD("FieldPosition"):BUFFER-VALUE = ix
             hSourceBuffer:BUFFER-FIELD("FieldLabel"):BUFFER-VALUE = hBrowseLinje:GET-BROWSE-COLUMN(ix):LABEL
             hSourceBuffer:BUFFER-FIELD("FieldName"):BUFFER-VALUE = hBrowseLinje:GET-BROWSE-COLUMN(ix):NAME
             hSourceBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE = "rowid" + STRING(ix)
             hSourceBuffer:BUFFER-FIELD("SortAsc"):BUFFER-VALUE = YES
             .
    END.
  END.

  hHandle = ihBrwSource:WINDOW.
  hHandle:HEIGHT-PIXELS = hHandle:HEIGHT-PIXELS + 200.
  APPLY "window-resized" TO hHandle.

  DYNAMIC-FUNCTION("adjustBrowseHeight" IN hSelector,-50).
  hPanelFrame = DYNAMIC-FUNCTION("getFrameHandle" IN hSortPanel).
  hPanelFrame:Y = ihBrwSource:Y + ihBrwSource:HEIGHT-PIXELS + 5. 
  RUN InitializeObject IN hSortPanel (ihBrwTarget,THIS-PROCEDURE).

  DYNAMIC-FUNCTION("setCurrentObject",ihBrwSource).
  RUN OpenQuerySource IN hSelector.

  hHandle = DYNAMIC-FUNCTION("NewBrowseToggle",ihBrwTarget,"SortAsc","SortAsc","").
  DYNAMIC-FUNCTION("CreateOverlayLink",ihBrwTarget,hHandle,"SortAsc").

END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettSensitive C-Win 
PROCEDURE SettSensitive :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME frmLinje:
/*     IF NOT BtnHentFraUtvalg:HIDDEN THEN                           */
/*       IF dec(VareBokNr:SCREEN-VALUE IN FRAME frmDetalj) <> 0 THEN */
/*         BtnHentFraUtvalg:SENSITIVE = FALSE.                       */
/*       ELSE BtnHentFraUtvalg:SENSITIVE = TRUE.                     */

    IF Oppdatert:SCREEN-VALUE IN FRAME frmDetalj = "yes" THEN DO:
      SettUtvalgSensitive(FALSE).
      IF VALID-HANDLE(hUtvalg) THEN 
        DYNAMIC-FUNCTION("setEnableBtnSendUtvalg" IN hUtvalg,FALSE).
    END.
    ELSE DO:
      SettUtvalgSensitive(TRUE).
      IF VALID-HANDLE(hUtvalg) THEN 
        DYNAMIC-FUNCTION("setEnableBtnSendUtvalg" IN hUtvalg,TRUE).
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettFraUtvalg C-Win 
PROCEDURE SlettFraUtvalg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO ix = 1 TO hBrowseLinje:NUM-SELECTED-ROWS:
  IF hBrowseLinje:FETCH-SELECTED-ROW(ix) THEN 
      DYNAMIC-FUNCTION('doDelete','VareBokLinje','ignore','',hBrowseLinje:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('RowIdent1'):BUFFER-VALUE,FALSE).
END.
DYNAMIC-FUNCTION('doCommit',FALSE).

APPLY "VALUE-CHANGED":U TO hBrowseListe.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettSpesialSortRecord C-Win 
PROCEDURE SlettSpesialSortRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR ocValue AS CHAR NO-UNDO.

RUN JBoxDSimpleSelectList.w (DYNAMIC-FUNCTION("getFieldList","JBoxGenCode;cDescription;iJboxGenCodeId","WHERE cCodeType = 'VarebokSort' BY cDescription"),
                             ?,OUTPUT ocValue).
IF ocValue NE ? THEN DO:
  IF DYNAMIC-FUNCTION("DoDelete","JBoxGenCode","ignore",
                      "iJboxGenCodeId",ocValue,YES) THEN
    setSpesialSortMenu().
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SortimentkoderRecord C-Win 
PROCEDURE SortimentkoderRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR ocValue      AS CHAR NO-UNDO.
DEF VAR iConfirm     AS INT NO-UNDO.

iReturn = 2.

RUN JBoxDSimpleSelectList.w (hBrwOSortiment:LIST-ITEM-PAIRS,?,OUTPUT ocValue).

IF iReturn NE 0 AND iConfirm NE 2 AND ocValue NE ? THEN
  EndreMangeLinjer("Sortimentkoder",ocValue).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SortSpesialRecord C-Win 
PROCEDURE SortSpesialRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cRowIdList  AS CHAR NO-UNDO.
DEF VAR cIdList     AS CHAR NO-UNDO.
DEF VAR bOk         AS LOG  NO-UNDO.
DEF VAR cQuerySort  AS CHAR NO-UNDO.

THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = NO.

RUN JBoxSelector.w (THIS-PROCEDURE,0,
/* RUN JBoxDSelector.w (THIS-PROCEDURE,0,  */
                    "temp-table" 
                    + ";!FieldName|CHARACTER|x(30)"
                    + ";FieldPosition|INTEGER|>9||Posisjon"
                    + ";FieldLabel|CHARACTER|x(40)||Feltnavn"
                    + ";SortAsc|LOGICAL|J/N||Stigende"
                    ,"where false",
                    INPUT-OUTPUT cRowIdList,
                    "FieldName,SortAsc",
                    INPUT-OUTPUT cIdList,
                    "","",
                    OUTPUT bOK).

THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = YES.

IF bOk THEN DO:    

  DO ix = 1 TO NUM-ENTRIES(cIdList,"|") - 1 BY 2:
    cQuerySort = cQuerySort 
               + (IF cQuerySort NE "" THEN "BY " ELSE "")
               + ENTRY(ix,cIdList,"|") + (IF ENTRY(ix + 1,cIdList,"|") = "no" THEN " DESC " ELSE " ").
  END.

  IF cSortDescr NE "" THEN DO:
    DYNAMIC-FUNCTION("runProc","varebok_lagre_sortering.p",cQuerySort + "|" + cSortDescr,?).
    setSpesialSortMenu().
    startSpesialSort(cSortDescr).
  END.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartSearch C-Win 
PROCEDURE StartSearch :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN SUPER.
IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowseLinje THEN
  StartSpesialSort("fjern_markering_av_valgt_spesialsortering").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartUtvalgRecord C-Win 
PROCEDURE StartUtvalgRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
SESSION:SET-WAIT-STATE("general").
IF NOT VALID-HANDLE(hUtvalg) THEN DO:
  RUN wtmpartbas.w PERSIST SET hUtvalg.
  DYNAMIC-FUNCTION("setTellingIsMaster" IN hUtvalg,TRUE).
  RUN InitializeObject IN hUtvalg.
/*     InitUtvalgToTelling(hUtvalg,NO). /* NO: Utvalg er IKKE master */  */
END.
DYNAMIC-FUNCTION("InitFromVareBok" IN hUtvalg, THIS-PROCEDURE).

SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE supAntallRecord C-Win 
PROCEDURE supAntallRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR ocValue     AS CHAR NO-UNDO.
DEF VAR iConfirm    AS INT NO-UNDO.

iReturn = 0.
RUN JBoxBrowseMsgUpdSelVal.w ("Oppdater antall suppleringskjøp",
                              hBrowseLinje:NUM-SELECTED-ROWS,
                              DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"Totalcount"),
                              "DECIMAL|>>>,>>9.99|Antall supp.kjøp",
                              OUTPUT ocValue, 
                              OUTPUT iReturn).
IF DEC(ocValue) = 0 AND iReturn NE 0 THEN 
  iConfirm = DYNAMIC-FUNCTION("DoMessage",0,1,"Bekreft at antall suppleringskjøp skal settes til 0","Bekreft","").

IF iReturn NE 0 AND iConfirm NE 2 THEN
  EndreMangeLinjer("supAntall",ocValue).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE supRab%Record C-Win 
PROCEDURE supRab%Record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR ocValue     AS CHAR NO-UNDO.
DEF VAR iConfirm    AS INT NO-UNDO.

iReturn = 0.
RUN JBoxBrowseMsgUpdSelVal.w ("Oppdater rabatt suppleringsskjøp",
                              hBrowseLinje:NUM-SELECTED-ROWS,
                              DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"Totalcount"),
                              "DECIMAL|->>,>>9.99|Rabatt supp.kjøp",
                              OUTPUT ocValue, 
                              OUTPUT iReturn).
IF DEC(ocValue) = 0 AND iReturn NE 0 THEN 
  iConfirm = DYNAMIC-FUNCTION("DoMessage",0,1,"Bekreft at rabatt suppleringskjøp skal settes til 0","Bekreft","").

IF iReturn NE 0 AND iConfirm NE 2 THEN
  EndreMangeLinjer("supRab%",ocValue).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE supVarekostRecord C-Win 
PROCEDURE supVarekostRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR ocValue     AS CHAR NO-UNDO.
DEF VAR iConfirm    AS INT NO-UNDO.

iReturn = 0.
RUN JBoxBrowseMsgUpdSelVal.w ("Oppdater netto innpris suppleringskjøp",
                              hBrowseLinje:NUM-SELECTED-ROWS,
                              DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"Totalcount"),
                              "DECIMAL|>>>,>>9.99|Netto innpris supp",
                              OUTPUT ocValue, 
                              OUTPUT iReturn).
IF DEC(ocValue) = 0 AND iReturn NE 0 THEN 
  iConfirm = DYNAMIC-FUNCTION("DoMessage",0,1,"Bekreft at netto innpris suppleringskjøp skal settes til 0","Bekreft","").

IF iReturn NE 0 AND iConfirm NE 2 THEN
  EndreMangeLinjer("supVarekost",ocValue).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE undoRecord C-Win 
PROCEDURE undoRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  RUN SUPER.

  DO WITH FRAME frmDetalj:
      ASSIGN          
          VareBokNr:SENSITIVE      = FALSE
          ProfilNr:SENSITIVE       = FALSE
          btnSokProfilNr:SENSITIVE = FALSE
          .
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VareFaktaRecord C-Win 
PROCEDURE VareFaktaRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cVareFakta AS CHAR NO-UNDO.
RUN dArtBasVareFakta.w (hFieldMapLinje:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE,
                        hFieldMapLinje:BUFFER-FIELD("VareFakta"):BUFFER-VALUE,
                        OUTPUT cVareFakta).
IF cVareFakta NE "" THEN DO:
  IF DYNAMIC-FUNCTION("runproc","artbas_endre_varefakta.p",STRING(hFieldMapLinje:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) + "|" + cVareFakta,?) THEN
    DYNAMIC-FUNCTION("RefreshRowids",hBrowseLinje,hFieldMapLinje:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE).
  ELSE 
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VaregruppeRecord C-Win 
PROCEDURE VaregruppeRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR ocValue     AS CHAR NO-UNDO.
DEF VAR iConfirm    AS INT NO-UNDO.

ocValue = "Vg".

RUN JBoxDLookup.w ("VarGr;Vg;VgBeskr","WHERE true",INPUT-OUTPUT ocValue).

IF ocValue NE ? AND ocValue NE "" THEN DO:
  iReturn = 2.
  EndreMangeLinjer("Vg",ocValue).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VarekostRecord C-Win 
PROCEDURE VarekostRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR ocValue     AS CHAR NO-UNDO.
DEF VAR iConfirm    AS INT NO-UNDO.

iReturn = 0.
RUN JBoxBrowseMsgUpdSelVal.w ("Oppdater netto innpris forhåndskjøp",
                              hBrowseLinje:NUM-SELECTED-ROWS,
                              DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"Totalcount"),
                              "DECIMAL|>>>,>>9.99|Netto innpris forh",
                              OUTPUT ocValue, 
                              OUTPUT iReturn).
IF DEC(ocValue) = 0 AND iReturn NE 0 THEN 
  iConfirm = DYNAMIC-FUNCTION("DoMessage",0,1,"Bekreft at netto innpris forhåndskjøp skal settes til 0","Bekreft","").

IF iReturn NE 0 AND iConfirm NE 2 THEN
  EndreMangeLinjer("Varekost",ocValue).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VareomrRecord C-Win 
PROCEDURE VareomrRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR ocValue     AS CHAR NO-UNDO.
DEF VAR iConfirm    AS INT NO-UNDO.

ocValue = "RAvdNr".

RUN JBoxDLookup.w ("Regnskapsavdeling;RAvdNr;RAvdBeskrivelse;OmradeAnsvarlig","WHERE true",INPUT-OUTPUT ocValue).

IF ocValue NE ? AND ocValue NE "" THEN DO:
  iReturn = 2.
  EndreMangeLinjer("RAvdNr",ocValue).
/*   IF DYNAMIC-FUNCTION("processSelectedRows",hBrowseLinje,"art_sett_ravdnr_vareb.p",ocValue) THEN */
/*     RUN InvokeMethod(hBrowseLinje,"OpenQuery").                                                  */
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VelgSpesialSort C-Win 
PROCEDURE VelgSpesialSort :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR hMnuItem AS HANDLE NO-UNDO.

hMnuItem = DYNAMIC-FUNCTION("getCurrentWidget").
startSpesialSort(hMnuItem:LABEL).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AdjustBrowseColumns C-Win 
FUNCTION AdjustBrowseColumns RETURNS LOGICAL
  ( INPUT ihBrowse     AS HANDLE,
    INPUT icBrowseName AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose: Hook fra NewBrowse slik at kolonner kan flyttes regelbasert.
           Gjør også justeringer av kolonnebredder her slik at disse 
           blir tatt vare på.
    Notes:  
------------------------------------------------------------------------------*/
IF icBrowseName = "rectBrowseLinje" THEN DO:  
/*   IF bHKinst THEN DO: */
/*     ihBrowse:MOVE-COLUMN(48,22).  /* Kjedevare */          */
/*     ihBrowse:MOVE-COLUMN(49,23).  /* Gjennomfaktureres */  */
/*     ihBrowse:MOVE-COLUMN(51,24).  /* Fritt tillegg */      */
/*     ihBrowse:MOVE-COLUMN(52,24).  /* VareFaktaInd */       */
/*     ihBrowse:MOVE-COLUMN(55,22).  /* SasBeskr */           */
/*   END.                                                    */
/*   ELSE DO:                                                */
/*     ihBrowse:MOVE-COLUMN(44,22).  /* Kjedevare */         */
/*     ihBrowse:MOVE-COLUMN(45,23).  /* Gjennomfaktureres */ */
/*     ihBrowse:MOVE-COLUMN(47,23).  /* Fritt tillegg */     */
/*     ihBrowse:MOVE-COLUMN(48,23).  /* VareFaktaInd */      */
/*   END.                                                    */

  hBrwColKjedeVare = ihBrowse:GET-BROWSE-COLUMN(23).
  hBrwColGjFakt    = ihBrowse:GET-BROWSE-COLUMN(24).

  ASSIGN ihBrowse:GET-BROWSE-COLUMN(2):WIDTH-PIXELS  = 70
         ihBrowse:GET-BROWSE-COLUMN(3):WIDTH-PIXELS  = 110
         ihBrowse:GET-BROWSE-COLUMN(4):WIDTH-PIXELS  = 70
         ihBrowse:GET-BROWSE-COLUMN(5):WIDTH-PIXELS  = 70
         ihBrowse:GET-BROWSE-COLUMN(6):WIDTH-PIXELS  = 80
         ihBrowse:GET-BROWSE-COLUMN(23):WIDTH-PIXELS = 40
         ihBrowse:GET-BROWSE-COLUMN(24):WIDTH-PIXELS = 40

/*          ihBrowse:GET-BROWSE-COLUMN(IF FALSE /* bHKinst */ THEN 22 ELSE 20):WIDTH-PIXELS = 110 */
         .

  DO ix = 1 TO 15: 
    ihBrowse:GET-BROWSE-COLUMN(ix):WIDTH-PIXELS = ihBrowse:GET-BROWSE-COLUMN(ix):WIDTH-PIXELS - 15.
    IF ihBrowse:GET-BROWSE-COLUMN(ix):NAME = "Beskr" THEN
      hBrwColArtBeskr = ihBrowse:GET-BROWSE-COLUMN(ix).
  END.

  ihBrowse:GET-BROWSE-COLUMN(17):WIDTH-PIXELS  = 50.
  ihBrowse:GET-BROWSE-COLUMN(18):WIDTH-PIXELS  = 50.
  ihBrowse:GET-BROWSE-COLUMN(19):WIDTH-PIXELS  = 50.
  ihBrowse:GET-BROWSE-COLUMN(20):WIDTH-PIXELS  = 50.
  ihBrowse:GET-BROWSE-COLUMN(21):WIDTH-PIXELS  = 100.

END.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ArtPrintToExcel C-Win 
FUNCTION ArtPrintToExcel RETURNS LOGICAL
  ( INPUT icFileAndCount AS CHAR,
    INPUT ibTestPrint    AS LOG) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR chExcelApplication      AS COM-HANDLE NO-UNDO.
DEF VAR chWorkbook              AS COM-HANDLE NO-UNDO.
DEF VAR chWorksheet             AS COM-HANDLE NO-UNDO.
DEF VAR chInterior              AS COM-HANDLE NO-UNDO.
DEF VAR chBorder                AS COM-HANDLE NO-UNDO.
DEF VAR chBefore                AS COM-HANDLE NO-UNDO.
DEF VAR iCount                  AS INTEGER NO-UNDO.
DEF VAR iNumCols                AS INT NO-UNDO INIT 14.
DEF VAR cRange                  AS CHAR NO-UNDO.
DEF VAR cFileName               AS CHAR NO-UNDO.
DEF VAR iCntStr                 AS INT NO-UNDO.
DEF VAR cCurrLevNavn            AS CHAR NO-UNDO.              
DEF VAR cPrevLevNavn            AS CHAR NO-UNDO.              
DEF VAR iShade                  AS INT NO-UNDO.

ASSIGN cFileName = getReportFile(ENTRY(1,icFileAndCount,"|"))
       iCount    = INT(ENTRY(2,icFileAndCount,"|"))
       .

chExcelApplication = DYNAMIC-FUNCTION("getExcelHandle").
IF NOT VALID-HANDLE(chExcelApplication) THEN DO:
  IF DYNAMIC-FUNCTION("setWebDoc","Open",cFileName) NE "" THEN
    MESSAGE "Could not open file: " cFileName VIEW-AS ALERT-BOX ERROR.
  RETURN TRUE.
END.
  
chExcelApplication:Workbooks:OpenText(cFileName,2,,,,,TRUE).

chExcelApplication:ScreenUpdating = FALSE.

cRange = "A2:Q" + STRING(iCount + 2). 

ASSIGN chWorkbook                           = chExcelApplication:WorkBooks:ITEM(1)
       chWorkSheet                          = chExcelApplication:Sheets:ITEM(1)
       chWorkSheet:NAME                     = ENTRY(1,hFieldMap:BUFFER-FIELD("VareBokBeskrivelse"):BUFFER-VALUE," ")

       chWorkSheet:Range(cRange):FONT:NAME  = "Arial"
       chWorkSheet:Range(cRange):FONT:SIZE  = 12

       chWorkSheet:Rows(1):FONT:Bold        = TRUE
       chWorkSheet:Rows(2):FONT:Bold        = TRUE
       chWorkSheet:Rows(2):FONT:NAME        = "Arial Narrow"
       chWorkSheet:Rows(2):HorizontalAlignment = 3
       chWorkSheet:PageSetup:CenterHeader   = "Varebok " + STRING(hFieldMap:BUFFER-FIELD("VareBokNr"):BUFFER-VALUE) + " - artikkelutskrift"
       chWorkSheet:PageSetup:LeftFooter     = DYNAMIC-FUNCTION("getAppTitle")
       chWorkSheet:PageSetup:CenterFooter   = STRING(TODAY,"99/99/9999")  + " " + STRING(TIME,"HH:MM") /*"&D &T"*/
       chWorkSheet:PageSetup:RightFooter    = "Page &P of &N"
       chWorkSheet:PageSetup:Orientation    = 2
       chWorkSheet:PageSetup:FitToPagesWide = 1
       chWorkSheet:PageSetup:TopMargin      = 50
       chWorkSheet:PageSetup:LeftMargin     = 25
       chWorkSheet:PageSetup:RightMargin    = 25
       chWorkSheet:PageSetup:Zoom           = 65
       chWorkSheet:PageSetup:PrintTitleRows = "$1:$2"
       .

chWorkSheet:Range("A1:Q2"):SELECT().
chInterior = chExcelApplication:SELECTION:Interior.
chInterior:ColorIndex = 37.

chWorkSheet:Range("A2:F2"):HorizontalAlignment = {&xlLeft}.
chWorkSheet:Range("P2:Q2"):HorizontalAlignment = {&xlLeft}.
chWorkSheet:Range("A1"):FONT:SIZE = 18.

chWorkSheet:Range("B:B"):NumberFormat = "0".
chWorkSheet:Range("E:E"):NumberFormat = "@".

chBorder = chWorkSheet:Range("a1:Q" + STRING(iCount + 2)):Borders({&xlEdgeTop}).
ASSIGN chBorder:LineStyle = {&xlContinuous}
       chBorder:Weight    = {&xlThin}.
chBorder = chWorkSheet:Range("a1:Q" + STRING(iCount + 2)):Borders({&xlEdgeBottom}).
ASSIGN chBorder:LineStyle = {&xlContinuous}
       chBorder:Weight    = {&xlThin}.
chBorder = chWorkSheet:Range("a1:Q" + STRING(iCount + 2)):Borders({&xlEdgeRight}).
ASSIGN chBorder:LineStyle = {&xlContinuous}
       chBorder:Weight    = {&xlThin}.
chBorder = chWorkSheet:Range("a1:Q" + STRING(iCount + 2)):Borders({&xlEdgeLeft}).
ASSIGN chBorder:LineStyle = {&xlContinuous}
       chBorder:Weight    = {&xlThin}.

chWorkSheet:Range("Q:Q"):columnwidth = 15.
chWorkSheet:Range("Q1:Q" + STRING(iCount + 2)):WrapText = TRUE.

IF NOT ibTestPrint THEN DO ix = 3 TO iCount + 2:

  chBorder = chWorkSheet:Range("A" + STRING(ix - 1) + ":Q" + STRING(ix - 1)):Borders({&xlEdgeBottom}).
  ASSIGN chBorder:LineStyle = {&xlContinuous}
         chBorder:Weight    = {&xlThin}.

  chBorder = chWorkSheet:Range("F" + STRING(ix) + ":F" + STRING(ix)):Borders({&xlEdgeRight}).
  ASSIGN chBorder:LineStyle = {&xlContinuous}
         chBorder:Weight    = {&xlThin}.
  chBorder = chWorkSheet:Range("G" + STRING(ix) + ":G" + STRING(ix)):Borders({&xlEdgeRight}).
  ASSIGN chBorder:LineStyle = {&xlContinuous}
         chBorder:Weight    = {&xlThin}.
  chBorder = chWorkSheet:Range("H" + STRING(ix) + ":H" + STRING(ix)):Borders({&xlEdgeRight}).
  ASSIGN chBorder:LineStyle = {&xlContinuous}
         chBorder:Weight    = {&xlThin}.
  chBorder = chWorkSheet:Range("I" + STRING(ix) + ":I" + STRING(ix)):Borders({&xlEdgeRight}).
  ASSIGN chBorder:LineStyle = {&xlContinuous}
         chBorder:Weight    = {&xlThin}.
  chBorder = chWorkSheet:Range("J" + STRING(ix) + ":J" + STRING(ix)):Borders({&xlEdgeRight}).
  ASSIGN chBorder:LineStyle = {&xlContinuous}
         chBorder:Weight    = {&xlThin}.
  chBorder = chWorkSheet:Range("K" + STRING(ix) + ":K" + STRING(ix)):Borders({&xlEdgeRight}).
  ASSIGN chBorder:LineStyle = {&xlContinuous}
         chBorder:Weight    = {&xlThin}.
  chBorder = chWorkSheet:Range("L" + STRING(ix) + ":L" + STRING(ix)):Borders({&xlEdgeRight}).
  ASSIGN chBorder:LineStyle = {&xlContinuous}
         chBorder:Weight    = {&xlThin}.
  chBorder = chWorkSheet:Range("M" + STRING(ix) + ":M" + STRING(ix)):Borders({&xlEdgeRight}).
  ASSIGN chBorder:LineStyle = {&xlContinuous}
         chBorder:Weight    = {&xlThin}.
  chBorder = chWorkSheet:Range("N" + STRING(ix) + ":N" + STRING(ix)):Borders({&xlEdgeRight}).
  ASSIGN chBorder:LineStyle = {&xlContinuous}
         chBorder:Weight    = {&xlThin}.
  chBorder = chWorkSheet:Range("O" + STRING(ix) + ":O" + STRING(ix)):Borders({&xlEdgeRight}).
  ASSIGN chBorder:LineStyle = {&xlContinuous}
         chBorder:Weight    = {&xlThin}.
  chBorder = chWorkSheet:Range("P" + STRING(ix) + ":P" + STRING(ix)):Borders({&xlEdgeRight}).
  ASSIGN chBorder:LineStyle = {&xlContinuous}
         chBorder:Weight    = {&xlThin}.

  ASSIGN chWorkSheet:Range("H" + STRING(ix) + ":O" + STRING(ix)):NumberFormat = "0,00"
         chWorkSheet:Range("I" + STRING(ix)):FONT:Bold = TRUE
         chWorkSheet:Range("L" + STRING(ix)):FONT:Bold = TRUE
         chWorkSheet:Range("N" + STRING(ix)):FONT:Bold = TRUE
         .

END.

chWorkSheet:Columns("A:O"):AutoFit().
/* chWorkSheet:Range("A:A"):columnwidth = chWorkSheet:Range("A:A"):columnwidth * 2.5. */

chWorkSheet:Range("A2"):Select().
chExcelApplication:ActiveWindow:FreezePanes = TRUE.

chExcelApplication:ScreenUpdating = YES.

chExcelApplication:VISIBLE = TRUE.

/* release com-handles */
RELEASE OBJECT chBorder NO-ERROR.
RELEASE OBJECT chBefore NO-ERROR.
RELEASE OBJECT chInterior NO-ERROR.
RELEASE OBJECT chWorksheet NO-ERROR.
RELEASE OBJECT chWorkbook NO-ERROR.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION EAN13BC C-Win 
FUNCTION EAN13BC RETURNS CHARACTER
  ( INPUT icStrekKode AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEFINE VARIABLE cBarCode AS CHARACTER EXTENT 14 NO-UNDO.
DEFINE VARIABLE cBCString AS CHARACTER NO-UNDO.
DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.

IF LENGTH(icStrekKode) NE 13 THEN
  RETURN "".

ASSIGN cBarCode[1] = CHR(ASC(SUBSTR(icStrekKode,1,1)) - 15) 
       cBarCode[2] = CHR(ASC(SUBSTR(icStrekKode,2,1)) + 48)
       cBarCode[8] = CHR(124).

ASSIGN cBarcode[3] = SUBSTR(icStrekKode,3,1)
       cBarcode[4] = SUBSTR(icStrekKode,4,1)
       cBarcode[5] = SUBSTR(icStrekKode,5,1)
       cBarcode[6] = SUBSTR(icStrekKode,6,1)
       cBarcode[7] = SUBSTR(icStrekKode,7,1).

CASE SUBSTR(icStrekKode,1,1):
    WHEN "1" THEN
        ASSIGN cBarcode[4] = CHR(ASC(cBarcode[4]) + 16)
               cBarcode[6] = CHR(ASC(cBarcode[6]) + 16)
               cBarcode[7] = CHR(ASC(cBarcode[7]) + 16).
    WHEN "2" THEN
        ASSIGN cBarcode[4] = CHR(ASC(cBarcode[4]) + 16)
               cBarcode[5] = CHR(ASC(cBarcode[5]) + 16)
               cBarcode[7] = CHR(ASC(cBarcode[7]) + 16).
    WHEN "3" THEN
        ASSIGN cBarcode[4] = CHR(ASC(cBarcode[4]) + 16)
               cBarcode[5] = CHR(ASC(cBarcode[5]) + 16)
               cBarcode[6] = CHR(ASC(cBarcode[6]) + 16).
    WHEN "4" THEN
        ASSIGN cBarcode[3] = CHR(ASC(cBarcode[3]) + 16)
               cBarcode[6] = CHR(ASC(cBarcode[6]) + 16)
               cBarcode[7] = CHR(ASC(cBarcode[7]) + 16).
    WHEN "5" THEN
        ASSIGN cBarcode[3] = CHR(ASC(cBarcode[3]) + 16)
               cBarcode[4] = CHR(ASC(cBarcode[4]) + 16)
               cBarcode[7] = CHR(ASC(cBarcode[7]) + 16).
    WHEN "6" THEN
        ASSIGN cBarcode[3] = CHR(ASC(cBarcode[3]) + 16)
               cBarcode[4] = CHR(ASC(cBarcode[4]) + 16)
               cBarcode[5] = CHR(ASC(cBarcode[5]) + 16).
    WHEN "7" THEN
        ASSIGN cBarcode[3] = CHR(ASC(cBarcode[3]) + 16)
               cBarcode[5] = CHR(ASC(cBarcode[5]) + 16)
               cBarcode[7] = CHR(ASC(cBarcode[7]) + 16).
    WHEN "8" THEN
        ASSIGN cBarcode[3] = CHR(ASC(cBarcode[3]) + 16)
               cBarcode[5] = CHR(ASC(cBarcode[5]) + 16)
               cBarcode[6] = CHR(ASC(cBarcode[6]) + 16).
    WHEN "9" THEN
        ASSIGN cBarcode[3] = CHR(ASC(cBarcode[3]) + 16)
               cBarcode[4] = CHR(ASC(cBarcode[4]) + 16)
               cBarcode[6] = CHR(ASC(cBarcode[6]) + 16).
END CASE.
ASSIGN cBarcode[9] = CHR(ASC(SUBSTR(icStrekKode,8,1)) + 32) 
       cBarcode[10] = CHR(ASC(SUBSTR(icStrekKode,9,1)) + 32) 
       cBarcode[11] = CHR(ASC(SUBSTR(icStrekKode,10,1)) + 32) 
       cBarcode[12] = CHR(ASC(SUBSTR(icStrekKode,11,1)) + 32) 
       cBarcode[13] = CHR(ASC(SUBSTR(icStrekKode,12,1)) + 32).
       

cBarcode[14] = CHR(ASC(SUBSTR(icStrekKode,13,1)) + 64).
DO iCount = 1 TO 14:
    cBCString = cBCString + cBarCode[iCount] .
END.

IF cBCstring = ? THEN cBCstring = "".

RETURN cBCstring. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION EanPrintToExcel C-Win 
FUNCTION EanPrintToExcel RETURNS LOGICAL
  ( INPUT icFileAndCount AS CHAR,
    INPUT ibTestPrint    AS LOG) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR chExcelApplication      AS COM-HANDLE NO-UNDO.
DEF VAR chWorkbook              AS COM-HANDLE NO-UNDO.
DEF VAR chWorksheet             AS COM-HANDLE NO-UNDO.
DEF VAR chInterior              AS COM-HANDLE NO-UNDO.
DEF VAR chBorder                AS COM-HANDLE NO-UNDO.
DEF VAR chBefore                AS COM-HANDLE NO-UNDO.
DEF VAR iCount                  AS INTEGER NO-UNDO.
DEF VAR iNumCols                AS INT NO-UNDO INIT 14.
DEF VAR cRange                  AS CHAR NO-UNDO.
DEF VAR cFileName               AS CHAR NO-UNDO.
DEF VAR iCntStr                 AS INT NO-UNDO.
DEF VAR cCurrLevNavn            AS CHAR NO-UNDO.              
DEF VAR cPrevLevNavn            AS CHAR NO-UNDO.   
DEF VAR cVaregr                 AS CHAR NO-UNDO.
DEF VAR cInndLabel              AS CHAR NO-UNDO.
DEF VAR cMerknad                AS CHAR NO-UNDO.
DEF VAR bPageBreak              AS LOG  NO-UNDO.
DEF VAR bMark                   AS LOG  NO-UNDO.

ASSIGN cFileName = getReportFile(ENTRY(1,icFileAndCount,"|")) 
       iCount    = INT(ENTRY(2,icFileAndCount,"|"))
       .

IF DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"QuerySort") BEGINS "levnamn" THEN bPageBreak = TRUE.

EMPTY TEMP-TABLE ttEANrows.
EMPTY TEMP-TABLE ttWeekRows.

chExcelApplication = DYNAMIC-FUNCTION("getExcelHandle").
IF NOT VALID-HANDLE(chExcelApplication) THEN DO:
  IF DYNAMIC-FUNCTION("setWebDoc","Open",cFileName) NE "" THEN
    MESSAGE "Could not open file: " cFileName VIEW-AS ALERT-BOX ERROR.
  RETURN TRUE.
END.
  
chExcelApplication:ScreenUpdating = FALSE.

chExcelApplication:Workbooks:OpenText(cFileName,2,,,,,TRUE).

cRange = "A2:N" + STRING(iCount + 3). 

ASSIGN chWorkbook                           = chExcelApplication:WorkBooks:ITEM(1)
       chWorkSheet                          = chExcelApplication:Sheets:ITEM(1)
       chWorkSheet:NAME                     = ENTRY(1,hFieldMap:BUFFER-FIELD("VareBokBeskrivelse"):BUFFER-VALUE," ")

       chWorkSheet:Range(cRange):FONT:NAME  = "Arial"
       chWorkSheet:Range(cRange):FONT:SIZE  = 12

       chWorkSheet:Rows(1):FONT:Bold        = TRUE
       chWorkSheet:Rows(2):FONT:Bold        = TRUE
       chWorkSheet:Rows(3):FONT:Bold        = TRUE
       chWorkSheet:Rows(2):FONT:NAME        = "Arial Narrow"
       chWorkSheet:Rows(3):FONT:NAME        = "Arial Narrow"
       chWorkSheet:Rows(2):HorizontalAlignment = 3
       chWorkSheet:Rows(3):HorizontalAlignment = 3

       chWorkSheet:PageSetup:LeftHeader     = "Butikk nr: ..............   Firmanavn: ............................................"
       chWorkSheet:PageSetup:CenterHeader   = "Varebok" + STRING(hFieldMap:BUFFER-FIELD("VareBokNr"):BUFFER-VALUE) + " - artikler med størrelser"
       chWorkSheet:PageSetup:RightHeader    = STRING(TODAY,"99/99/9999")  + " " + STRING(TIME,"HH:MM") /*"&D &T"*/
       chWorkSheet:PageSetup:LeftFooter     = DYNAMIC-FUNCTION("getAppTitle")
       chWorkSheet:PageSetup:CenterFooter   = hFieldMap:BUFFER-FIELD("VareBokNotat"):BUFFER-VALUE
       chWorkSheet:PageSetup:RightFooter    = "Page &P of &N"

       chWorkSheet:PageSetup:ORIENTATION    = 2
       chWorkSheet:PageSetup:FitToPagesWide = 1
       chWorkSheet:PageSetup:TopMargin      = 50
       chWorkSheet:PageSetup:LeftMargin     = 25
       chWorkSheet:PageSetup:RightMargin    = 25
       chWorkSheet:PageSetup:Zoom           = 60
       chWorkSheet:PageSetup:PrintTitleRows = "$1:$3"
       .

chWorkSheet:Range("A1:M3"):SELECT().
chInterior = chExcelApplication:SELECTION:Interior.
chInterior:ColorIndex = 37.

chWorkSheet:Range("E1:E1"):FONT:SIZE = 18.

chWorkSheet:Range("K:K"):NumberFormat = "@".

chWorkSheet:COLUMNS("I:I"):HorizontalAlignment = {&xlCenter}.

chWorkSheet:Range("B4:B" + STRING(iCount + 3)):FONT:Bold = TRUE.


chBorder = chWorkSheet:Range("a1:M" + STRING(iCount + 3)):Borders({&xlEdgeTop}).
ASSIGN chBorder:LineStyle = {&xlContinuous}
       chBorder:Weight    = {&xlThin}.
chBorder = chWorkSheet:Range("a1:M" + STRING(iCount + 3)):Borders({&xlEdgeBottom}).
ASSIGN chBorder:LineStyle = {&xlContinuous}
       chBorder:Weight    = {&xlThin}.
chBorder = chWorkSheet:Range("a1:M" + STRING(iCount + 3)):Borders({&xlEdgeRight}).
ASSIGN chBorder:LineStyle = {&xlContinuous}
       chBorder:Weight    = {&xlThin}.
chBorder = chWorkSheet:Range("a1:M" + STRING(iCount + 3)):Borders({&xlEdgeLeft}).
ASSIGN chBorder:LineStyle = {&xlContinuous}
       chBorder:Weight    = {&xlThin}.

chBorder = chWorkSheet:Range("J2:J" + STRING(iCount + 3)):Borders({&xlEdgeLeft}).
ASSIGN chBorder:LineStyle = {&xlContinuous}
       chBorder:Weight    = {&xlThin}.
chBorder = chWorkSheet:Range("K2:K" + STRING(iCount + 3)):Borders({&xlEdgeLeft}).
ASSIGN chBorder:LineStyle = {&xlContinuous}
       chBorder:Weight    = {&xlThin}.
chBorder = chWorkSheet:Range("L2:L" + STRING(iCount + 3)):Borders({&xlEdgeLeft}).
ASSIGN chBorder:LineStyle = {&xlContinuous}
       chBorder:Weight    = {&xlThin}.
chBorder = chWorkSheet:Range("M2:M" + STRING(iCount + 3)):Borders({&xlEdgeLeft}).
ASSIGN chBorder:LineStyle = {&xlContinuous}
       chBorder:Weight    = {&xlThin}.

IF NOT ibTestPrint THEN DO ix = 4 TO iCount + 3:
  ASSIGN cCurrLevNavn = chWorkSheet:Range("A" + STRING(ix)):VALUE
         cVaregr      = chWorkSheet:Range("D" + STRING(ix)):VALUE
         cInndLabel   = chWorkSheet:Range("E" + STRING(ix)):VALUE
         cMerknad     = cInndLabel
         .

  IF cCurrLevNavn BEGINS "<lev>" THEN DO:  /* Første linje, artikkel */

    IF cPrevLevNavn NE "" AND cPrevLevNavn NE cCurrLevNavn THEN DO:
      chBefore = chWorkSheet:Range(cRange).
      chWorkSheet:HPageBreaks:Add(chBefore).
    END.
    cPrevLevNavn = cCurrLevNavn.

    chWorkSheet:Range("A" + STRING(ix)):VALUE = SUBSTR(cCurrLevNavn,6).

    chBorder = chWorkSheet:Range("a" + STRING(ix - 1) + ":M" + STRING(ix - 1)):Borders({&xlEdgeBottom}).
    ASSIGN chBorder:LineStyle = {&xlContinuous}
           chBorder:Weight    = {&xlThin}.

    CREATE ttEANrows.
    ttEANrows.iRow = ix.

    chWorkSheet:Range("J" + STRING(ix) + ":J" + STRING(ix)):SELECT().
    chInterior = chExcelApplication:SELECTION:Interior.
    chInterior:ColorIndex = 15. 

    chBorder = chWorkSheet:Range("J" + STRING(ix) + ":M" + STRING(ix)):Borders({&xlEdgeBottom}).
    ASSIGN chBorder:LineStyle = {&xlContinuous}
           chBorder:Weight    = {&xlThin}.

    ASSIGN chWorkSheet:Range("F" + STRING(ix) +  ":F" + STRING(ix)):NumberFormat = "0,00"
           chWorkSheet:Range("F" + STRING(ix) +  ":F" + STRING(ix)):FONT:NAME = "Arial"
           chWorkSheet:Range("F" + STRING(ix) +  ":F" + STRING(ix)):FONT:SIZE = 12
           chWorkSheet:Range("F" + STRING(ix) +  ":F" + STRING(ix)):FONT:Bold = TRUE
           .
  END.

  ELSE IF cInndLabel = "Inndeling" THEN DO:    /* Inndeling label */
    chWorkSheet:Range("E" + STRING(ix) + ":I" + STRING(ix)):SELECT().
    chInterior = chExcelApplication:SELECTION:Interior.
    chInterior:ColorIndex = 37.
    chWorkSheet:Range("E" + STRING(ix) + ":I" + STRING(ix)):FONT:Bold = TRUE.
    chWorkSheet:Range("E" + STRING(ix) + ":I" + STRING(ix)):FONT:NAME = "Arial Narrow".

    chWorkSheet:Range("J" + STRING(ix - 1) + ":M" + STRING(ix)):SELECT().
    chInterior = chExcelApplication:SELECTION:Interior.
    chInterior:ColorIndex = 15. 

    chBorder = chWorkSheet:Range("J" + STRING(ix) + ":M" + STRING(ix)):Borders({&xlEdgeBottom}).
    ASSIGN chBorder:LineStyle = {&xlContinuous}
           chBorder:Weight    = {&xlThin}.

    FIND FIRST ttEANrows WHERE ttEANrows.iRow = ix - 1 NO-ERROR.
    IF AVAIL ttEANrows THEN DELETE ttEANrows.
  END.

  ELSE IF cVaregr = ? AND cInndLabel = ? THEN DO: /* Størrelser: */
    iCntStr = iCntStr + 1.
    CREATE ttEANrows.
    ttEANrows.iRow = ix.

    chBorder = chWorkSheet:Range("J" + STRING(ix) + ":M" + STRING(ix)):Borders({&xlEdgeBottom}).
    ASSIGN chBorder:LineStyle = {&xlContinuous}
           chBorder:Weight    = {&xlThin}.
  END.

  ELSE IF cVaregr = ? AND cInndLabel NE ? THEN DO: /* Inndelinger: */
    CREATE ttEANrows.
    ttEANrows.iRow = ix.

    chBorder = chWorkSheet:Range("J" + STRING(ix) + ":M" + STRING(ix)):Borders({&xlEdgeBottom}).
    ASSIGN chBorder:LineStyle = {&xlContinuous}
           chBorder:Weight    = {&xlThin}.
  END.

  ELSE DO: /* Linje 2, artikkel */
    CREATE ttEANrows.
    ttEANrows.iRow = ix.
    chBorder = chWorkSheet:Range("J" + STRING(ix) + ":M" + STRING(ix)):Borders({&xlEdgeBottom}).
    ASSIGN chBorder:LineStyle = {&xlContinuous}
           chBorder:Weight    = {&xlThin}.

    ASSIGN chWorkSheet:Range("D" + STRING(ix) +  ":D" + STRING(ix)):NumberFormat = "0,00"
           chWorkSheet:Range("D" + STRING(ix) +  ":D" + STRING(ix)):FONT:NAME = "Arial"
           chWorkSheet:Range("D" + STRING(ix) +  ":D" + STRING(ix)):FONT:SIZE = 12
           chWorkSheet:Range("D" + STRING(ix) +  ":D" + STRING(ix)):FONT:Bold = TRUE

           chWorkSheet:Range("F" + STRING(ix) +  ":H" + STRING(ix)):NumberFormat = "0,00"
           chWorkSheet:Range("F" + STRING(ix) +  ":H" + STRING(ix)):FONT:NAME = "Arial"
           chWorkSheet:Range("F" + STRING(ix) +  ":H" + STRING(ix)):FONT:SIZE = 12
           chWorkSheet:Range("F" + STRING(ix) +  ":H" + STRING(ix)):FONT:Bold = TRUE

           bMark = FALSE
           .
    DO iy = 1 TO NUM-ENTRIES(cPrintMark):
      IF cMerknad MATCHES "*" + ENTRY(iy,cPrintMark) + "*" THEN bMark = TRUE.
    END.
    IF bMark THEN DO:
      chWorkSheet:Range("E" + STRING(ix) + ":E" + STRING(ix)):SELECT().
      chInterior = chExcelApplication:SELECTION:Interior.
      chInterior:ColorIndex = 15. 
    END.
  END.

/*
  ELSE DO: /* Artikkelinfo og inndelinger: */
    ASSIGN cRange       = "A" + STRING(ix)
    IF cCurrLevNavn BEGINS "<lev>" THEN DO:
      chWorkSheet:Range("A" + STRING(ix)):VALUE = SUBSTR(cCurrLevNavn,6).
      CREATE ttWeekRows.
      ttWeekRows.iRow = ix.
    END.

    IF (FALSE /* iCntStr > 14 */ OR (cCurrLevNavn NE cPrevLevNavn AND cPrevLevNavn NE "" AND cCurrLevNavn BEGINS "<lev>")) AND cCurrLevNavn NE ? THEN DO:
    END.
    ELSE IF cCurrLevNavn = ? THEN DO:
      chBorder = chWorkSheet:Range("e" + STRING(ix - 1) + ":M" + STRING(ix - 1)):Borders({&xlEdgeBottom}).
      ASSIGN chBorder:LineStyle = {&xlContinuous}
             chBorder:Weight    = {&xlThin}.
    END.

    chWorkSheet:Range("D" + STRING(ix) +  ":D" + STRING(ix)):FONT:NAME = "Arial".
    chWorkSheet:Range("D" + STRING(ix) +  ":D" + STRING(ix)):FONT:SIZE = 12.
    chWorkSheet:Rows(STRING(ix) + ":" + STRING(ix)):RowHeight = 15.75.

/*     chWorkSheet:Range("B" + STRING(ix) + ":B" + STRING(ix)):FONT:Bold = FALSE. */

    iCntStr = 0.

    IF cCurrLevNavn NE ? OR cInndLabel = "Inndeling" THEN DO:
      chBorder = chWorkSheet:Range("a" + STRING(ix) + ":M" + STRING(ix)):Borders({&xlEdgeTop}).
      ASSIGN chBorder:LineStyle = {&xlContinuous}
             chBorder:Weight    = {&xlThin}.
   
      chWorkSheet:Range("K" + STRING(ix) + ":M" + STRING(ix)):SELECT().
      chInterior = chExcelApplication:SELECTION:Interior.
      chInterior:ColorIndex = 15. 
    
      chWorkSheet:Range(cRange):FONT:Bold = TRUE.
      IF cCurrLevNavn BEGINS "<lev>" THEN cPrevLevNavn = cCurrLevNavn.

    END.
    /* Hvis blank i kolonne B så er dette en inndeling og vi gir litt mer plass til å skrive: */
    ELSE IF cInndLine = ? THEN DO:
      CREATE ttEANrows.
      ttEANrows.iRow = ix.
/*       chWorkSheet:Rows(STRING(ix) + ":" + STRING(ix)):RowHeight = 40.           */
/*       chWorkSheet:Range("E" + STRING(ix) + ":E" + STRING(ix)):FONT:Bold = TRUE. */
    END.

    IF cVareFakta BEGINS "<VF>" THEN DO:
      ASSIGN chWorkSheet:Range("D" + STRING(ix)):VALUE = SUBSTR(cVareFakta,5)
             chWorkSheet:Range("D" + STRING(ix) +  ":D" + STRING(ix)):FONT:SIZE = 8
             chWorkSheet:Range("D" + STRING(ix) +  ":D" + STRING(ix)):WrapText = TRUE
             chWorkSheet:Rows(ix):VerticalAlignment = {&xlTop}
             .
    END.
  END.
  */
END.

chWorkSheet:Columns("A:M"):AutoFit().
/* chWorkSheet:Range("B:B"):columnwidth = 22. */
/* chWorkSheet:Columns("C:M"):AutoFit().      */
/* chWorkSheet:Range("D:D"):columnwidth = 20.                                         */
/* chWorkSheet:Range("E:E"):columnwidth = chWorkSheet:Range("E:E"):columnwidth * 1.3. */
chWorkSheet:Rows("1:" + STRING(ix)):AutoFit().

FOR EACH ttEANrows:
  chWorkSheet:Rows(STRING(ttEANrows.iRow) + ":" + STRING(ttEANrows.iRow)):RowHeight = 40.
END.
/* FOR EACH ttWeekRows: */
/*   chWorkSheet:Rows(STRING(ttWeekRows.iRow) + ":" + STRING(ttWeekRows.iRow)):RowHeight = 25. */
/* END.                                                                                        */

chWorkSheet:Range("B2"):Select().
chExcelApplication:ActiveWindow:FreezePanes = TRUE.

chExcelApplication:ScreenUpdating = TRUE.

chExcelApplication:VISIBLE = TRUE.

/* release com-handles */
RELEASE OBJECT chBorder NO-ERROR.
RELEASE OBJECT chBefore NO-ERROR.
RELEASE OBJECT chInterior NO-ERROR.
RELEASE OBJECT chWorksheet NO-ERROR.
RELEASE OBJECT chWorkbook NO-ERROR.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION EndreMangeLinjer C-Win 
FUNCTION EndreMangeLinjer RETURNS LOGICAL
  ( INPUT icField AS CHAR,
    INPUT icValue AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cRowIdList  AS CHAR NO-UNDO EXTENT 100.
DEF VAR iy          AS INT  NO-UNDO INIT 1.

IF iReturn = 0 THEN RETURN FALSE.

IF iReturn = 1 THEN 
  bOk = DYNAMIC-FUNCTION("ProcessQuery",hBrowseLinje,"vareboklinje_justermange.p",icField + "|" + icValue).    
ELSE DO:
  DO ix = 1 TO hBrowseLinje:NUM-SELECTED-ROWS:
    IF ix MOD 50 = 0 THEN iy = iy + 1.
    IF hBrowseLinje:FETCH-SELECTED-ROW(ix) THEN 
      cRowIdList[iy] = cRowIdList[iy] + hFieldMapLinje:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE + "|". 
  END.    
  DO ix = 1 TO iy:
    cRowIdList[ix] = TRIM(cRowIdList[ix],"|").
  END.
  bOk = DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowseLinje,"vareboklinje_justermange.p",icField + "|" + icValue).
END.

IF NOT bOK THEN
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil i justering av " + icField,""). 
ELSE IF cRowIdList[1] NE "" THEN DO:
  DO ix = 1 TO iy:
    DYNAMIC-FUNCTION("refreshRowids",hBrowseLinje,REPLACE(cRowIdList[ix],"|",",")).
  END.
  APPLY "value-changed" TO hBrowseLinje.
END.
ELSE RUN OpenQuery.

RETURN bOk. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ExcelAuto C-Win 
FUNCTION ExcelAuto RETURNS LOGICAL
  ( INPUT ihBuffer AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hQuery AS HANDLE NO-UNDO.
DEF VAR chExcelApplication      AS COM-HANDLE NO-UNDO.
DEF VAR chWorkbook              AS COM-HANDLE NO-UNDO.
DEF VAR chWorksheet             AS COM-HANDLE NO-UNDO.
DEF VAR chInterior              AS COM-HANDLE NO-UNDO.
/* DEF VAR chWorksheetRange        AS COM-HANDLE NO-UNDO. */
DEF VAR iCount                  AS INTEGER NO-UNDO.
DEF VAR iNumCols                AS INT NO-UNDO INIT 14.
DEF VAR cRange                  AS CHAR NO-UNDO.

DEF VAR cCurrDateFormat AS CHAR NO-UNDO.
DEF VAR cCurrNumFormat  AS CHAR NO-UNDO.
DEF VAR cFileName       AS CHAR NO-UNDO.
DEF VAR cOutput         AS CHAR NO-UNDO.

ASSIGN cCurrDateFormat = SESSION:DATE-FORMAT
       cCurrNumFormat  = SESSION:NUMERIC-FORMAT
       cFileName       = SESSION:TEMP-DIR + STRING(YEAR(TODAY)) + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY)) + STRING(TIME) + ".txt"
       .

OUTPUT TO VALUE(cFileName).

PUT UNFORMATTED "Leverandørnavn"  + "~t" 
                "Artikkelnr"      + "~t"     
                "Varemerke"       + "~t"
/*                 ""                + "~t" */
                "Varegr"          + "~t"         
                "Varegruppetekst" + "~t"
                "Merknad"         + "~t"        
/*                 ""                + "~t" */
                "Enh"             + "~t"            
                "Ant.enh"         + "~t"        
                "Fargetekst"      + "~t"     
                "Str"             + "~t"            
                "1.lev.uke"       + "~t"      
                "2.lev.uke"       + "~t"      
                "3.lev.uke"       + "~t"      
                "4.lev.uke"       + "~t"      
                SKIP.
PUT UNFORMATTED "Lev.art.nr"      + "~t"          
                "Artikkelnavn"    + "~t"   
/*                 ""                + "~t" */
                "EAN kode"        + "~t"       
                "Bestilles fra"   + "~t"  
                "Lev.pris engros" + "~t"
                "Nettopris forh"  + "~t" 
                "BF forhånd"      + "~t"     
                "Netto suppl"     + "~t"    
                "BF suppl"        + "~t"       
                "Uts.pris"        + "~t"       
                "Ant"             + "~t"            
                "Ant"             + "~t"            
                "Ant"             + "~t"            
                "Ant"             + "~t"            
                SKIP.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME).
hQuery:QUERY-OPEN().

hQuery:GET-FIRST().

REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  iCount = iCount + 2.
  DO ix = 1 TO 2:           
    IF ix = 1 THEN 
      PUT UNFORMATTED
          (IF ihBuffer:BUFFER-FIELD("ProdusentBeskrivelse"):BUFFER-VALUE NE ? AND ihBuffer:BUFFER-FIELD("ProdusentBeskrivelse"):BUFFER-VALUE NE "" THEN
              ihBuffer:BUFFER-FIELD("ProdusentBeskrivelse"):BUFFER-VALUE 
           ELSE IF ihBuffer:BUFFER-FIELD("levnamn"):BUFFER-VALUE NE ? THEN 
             ihBuffer:BUFFER-FIELD("levnamn"):BUFFER-VALUE 
           ELSE "") + "~t" +  
          ihBuffer:BUFFER-FIELD("Artikkelnr"):BUFFER-VALUE + "~t" +
          (IF ihBuffer:BUFFER-FIELD("Beskrivelse"):BUFFER-VALUE NE ? THEN ihBuffer:BUFFER-FIELD("Beskrivelse"):BUFFER-VALUE ELSE "") + "~t" +
/*           "" + "~t" + */
          ihBuffer:BUFFER-FIELD("Vg"):BUFFER-VALUE + "~t" +
          ihBuffer:BUFFER-FIELD("VgBeskr"):BUFFER-VALUE + "~t" +
          (IF ihBuffer:BUFFER-FIELD("LinjeMerknad"):BUFFER-VALUE NE ? THEN REPLACE(REPLACE(REPLACE(ihBuffer:BUFFER-FIELD("LinjeMerknad"):BUFFER-VALUE,CHR(10)," "),CHR(9)," "),CHR(13)," ") ELSE "") + "~t" +
/*           "" + "~t" + */
          (IF ihBuffer:BUFFER-FIELD("SalgsEnhet"):BUFFER-VALUE NE ? THEN ihBuffer:BUFFER-FIELD("SalgsEnhet"):BUFFER-VALUE ELSE "") + "~t" +
          "" + "~t" + /* Antall for enhet (i pakke - finnes ikke i db */
          (IF ihBuffer:BUFFER-FIELD("LevFargKod"):BUFFER-VALUE NE ? THEN ihBuffer:BUFFER-FIELD("LevFargKod"):BUFFER-VALUE ELSE "") + "~t" +
          (IF ihBuffer:BUFFER-FIELD("sostorl"):BUFFER-VALUE NE ? THEN ihBuffer:BUFFER-FIELD("sostorl"):BUFFER-VALUE ELSE "") + "~t" +
          (IF ihBuffer:BUFFER-FIELD("LevUke1"):BUFFER-VALUE NE ? THEN ihBuffer:BUFFER-FIELD("LevUke1"):BUFFER-VALUE ELSE "") + "~t" +
          getWeekNum(ihBuffer:BUFFER-FIELD("LevDato2"):BUFFER-VALUE) + "~t" +
          getWeekNum(ihBuffer:BUFFER-FIELD("LevDato3"):BUFFER-VALUE) + "~t" +
          getWeekNum(ihBuffer:BUFFER-FIELD("LevDato4"):BUFFER-VALUE)
          SKIP.
    ELSE 
      PUT UNFORMATTED
          (IF ihBuffer:BUFFER-FIELD("LevKod"):BUFFER-VALUE NE ? THEN ihBuffer:BUFFER-FIELD("LevKod"):BUFFER-VALUE ELSE "") + "~t" +
          (IF ihBuffer:BUFFER-FIELD("Beskr"):BUFFER-VALUE NE ? THEN ihBuffer:BUFFER-FIELD("Beskr"):BUFFER-VALUE ELSE "") + "~t" +
/*           "" + "~t" + */
          EAN13BC(ihBuffer:BUFFER-FIELD("kode"):BUFFER-VALUE) + "~t" +
          (IF ihBuffer:BUFFER-FIELD("levnamn"):BUFFER-VALUE NE ? THEN ihBuffer:BUFFER-FIELD("levnamn"):BUFFER-VALUE ELSE "") + "~t" +
          (IF ihBuffer:BUFFER-FIELD("InnkjopsPris"):BUFFER-VALUE NE ? THEN STRING(ihBuffer:BUFFER-FIELD("InnkjopsPris"):BUFFER-VALUE) ELSE "") + "~t" +
          (IF ihBuffer:BUFFER-FIELD("Varekost"):BUFFER-VALUE NE ? THEN STRING(ihBuffer:BUFFER-FIELD("Varekost"):BUFFER-VALUE) ELSE "") + "~t" +
          (IF ihBuffer:BUFFER-FIELD("DB%"):BUFFER-VALUE NE ? THEN STRING(ihBuffer:BUFFER-FIELD("DB%"):BUFFER-VALUE) ELSE "") + "~t" +
          (IF ihBuffer:BUFFER-FIELD("supVarekost"):BUFFER-VALUE NE ? THEN STRING(ihBuffer:BUFFER-FIELD("supVarekost"):BUFFER-VALUE) ELSE "") + "~t" +
          (IF ihBuffer:BUFFER-FIELD("supDB%"):BUFFER-VALUE NE ? THEN STRING(ihBuffer:BUFFER-FIELD("supDB%"):BUFFER-VALUE) ELSE "") + "~t" +
          (IF ihBuffer:BUFFER-FIELD("Pris"):BUFFER-VALUE NE ? THEN STRING(ihBuffer:BUFFER-FIELD("Pris"):BUFFER-VALUE) ELSE "")
          SKIP.
  END.
  hQuery:GET-NEXT().
END.
iCount = iCount + 2.

OUTPUT CLOSE.

ASSIGN SESSION:DATE-FORMAT    = cCurrDateFormat
       SESSION:NUMERIC-FORMAT = cCurrNumFormat.

CREATE "Excel.Application" chExcelApplication NO-ERROR.  
IF NOT VALID-HANDLE(chExcelApplication) THEN DO:
  IF DYNAMIC-FUNCTION("setWebDoc","Open",cFileName) NE "" THEN
    MESSAGE "Could not open file: " cFileName VIEW-AS ALERT-BOX ERROR.
  RETURN TRUE.
END.
chExcelApplication:VISIBLE = FALSE.
  
chExcelApplication:Workbooks:OpenText(cFileName,2,,,,,TRUE).

ASSIGN chWorkbook                           = chExcelApplication:WorkBooks:ITEM(1)
       chWorkSheet                          = chExcelApplication:Sheets:ITEM(1)
       chWorkSheet:NAME                     = hFieldMap:BUFFER-FIELD("VareBokBeskrivelse"):BUFFER-VALUE
       chWorkSheet:Rows(1):FONT:Bold        = TRUE
       chWorkSheet:Rows(2):FONT:Bold        = TRUE
       chWorkSheet:PageSetup:LeftFooter     = DYNAMIC-FUNCTION("getAppTitle")
       chWorkSheet:PageSetup:RightFooter    = "Page &P of &N"
       chWorkSheet:PageSetup:CenterFooter   = STRING(TODAY,"99/99/9999")  + " " + STRING(TIME,"HH:MM") /*"&D &T"*/
       chWorkSheet:PageSetup:RightFooter    = "Page &P of &N"
       chWorkSheet:PageSetup:Orientation    = 2
       chWorkSheet:PageSetup:FitToPagesWide = 1
       chWorkSheet:PageSetup:LeftHeader     = hFieldMap:BUFFER-FIELD("VareBokBeskrivelse"):BUFFER-VALUE
       .

chWorkSheet:Columns("A:Z"):AutoFit().

cRange = "A3:Z" + STRING(iCount). 
chWorkSheet:Range(cRange):FONT:NAME = "Arial".
chWorkSheet:Range(cRange):FONT:SIZE = 13.

DO ix = 4 TO iCount BY 2:
  ASSIGN cRange = "C" + STRING(ix)
         chWorkSheet:Range(cRange):FONT:NAME = "EAN-13B Half Height"
         chWorkSheet:Range(cRange):FONT:SIZE = 42
         .

  chWorkSheet:rows(ix - 1):SELECT().
  chInterior = chExcelApplication:SELECTION:Interior.
  chInterior:ColorIndex = 20. /* 15. */
/*   chInterior:Pattern = -4124.           */
/*   chInterior:PatternColorIndex = -4105. */
END.
chWorkSheet:Range("B2"):Select().
chExcelApplication:ActiveWindow:FreezePanes = TRUE.

/* cRange = "L2:O" + STRING(ix).                 */
/* chWorkSheet:Range(cRange):Select().           */
/* chExcelApplication:Selection:Style = "Comma". */

chExcelApplication:VISIBLE = TRUE.

/* release com-handles */
RELEASE OBJECT chInterior NO-ERROR.
RELEASE OBJECT chWorksheet NO-ERROR.
RELEASE OBJECT chWorkbook NO-ERROR.
RELEASE OBJECT chExcelApplication NO-ERROR.      

RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getReportFile C-Win 
FUNCTION getReportFile RETURNS CHARACTER
  ( INPUT icClientServerFile AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cFileName    AS CHAR   NO-UNDO.
DEF VAR httRepTable  AS HANDLE NO-UNDO.
DEF VAR httRepBuffer AS HANDLE NO-UNDO. 
DEF VAR hQuery       AS HANDLE NO-UNDO.

RETURN icClientServerFile. /* <- Fjern denne ved overgang til Appserver */

ASSIGN httRepTable  = DYNAMIC-FUNCTION("getRunProcReturnTable",?)
       httRepBuffer = httRepTable:DEFAULT-BUFFER-HANDLE
       cFileName = SESSION:TEMP-DIR + STRING(hFieldMap:BUFFER-FIELD("VarebokNr"):BUFFER-VALUE) + "_" + DYNAMIC-FUNCTION("getASuserId") + "_" + STRING(YEAR(TODAY)) + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY)) + STRING(TIME) + ".txt".  

OUTPUT TO VALUE(cFileName).

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(httRepBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + httRepBuffer:NAME).
hQuery:QUERY-OPEN().
hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  PUT UNFORMATTED httRepBuffer:BUFFER-FIELD("cRpLinje"):BUFFER-VALUE.
  hQuery:GET-NEXT().
END.

OUTPUT CLOSE.

DELETE OBJECT hQuery.
DELETE OBJECT httRepTable.

RETURN cFileName.   

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getVareBokNr C-Win 
FUNCTION getVareBokNr RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DO WITH FRAME frmDetalj:
      RETURN dec(VareBokNr:SCREEN-VALUE).   /* Function return value. */
  END.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getWeekNum C-Win 
FUNCTION getWeekNum RETURNS CHARACTER
  ( INPUT dSomeDate AS DATE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEFINE VAR yyyyww    AS INT NO-UNDO.   /* Output week, eg 9042     */

DEFINE VARIABLE yr   AS INT NO-UNDO.  /* Year of dSomeDate, eg 1990      */
DEFINE VARIABLE d1   AS INT NO-UNDO.  /* Weekday of 1/1 current year, eg 2  */
                              /* (01/01/90 is a Monday)      */
DEFINE VARIABLE dat1 AS DATE NO-UNDO. /* Starting date of week 1     */
DEFINE VARIABLE wn   AS INT NO-UNDO.  /* Week number , eg 45         */

ASSIGN
  yr   = YEAR(dSomeDate)
  d1   = WEEKDAY(DATE( 1 , 1 , yr))
  dat1 = (IF d1 LE 5 THEN DATE(1,  3, yr) - d1 ELSE
                          DATE(1, 10, yr) - d1 )
  wn   = TRUNCATE((dSomeDate - dat1 + 7) / 7 , 0)
  yyyyww = yr * 100 + wn.

IF wn < 1 THEN       /* Week 52 or 53 previous year ? */
ASSIGN
  yr     = yr - 1
  d1     = WEEKDAY(DATE( 1 , 1 , yr))
  dat1   = (IF d1 LE 5 THEN DATE(1,  3, yr) - d1 ELSE
                            DATE(1, 10, yr) - d1 )
  wn     = TRUNCATE((dSomeDate - dat1 + 7) / 7 , 0)
  yyyyww = yr * 100 + wn.

ELSE IF wn > 52 THEN  /* Week 53 this year or week 1 next year ? */
ASSIGN
  yr     = yr + 1
  d1     = WEEKDAY(DATE( 1 , 1 , yr))
  yyyyww = IF d1 EQ 6 OR d1 EQ 7 OR d1 EQ 1
              THEN (yr - 1) * 100 + 53 ELSE yr * 100 + 1.

IF yyyyww > 0 THEN
  RETURN STRING(yyyyww).
ELSE RETURN "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION InitOverlays C-Win 
FUNCTION InitOverlays RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
hBrwOLMerk = DYNAMIC-FUNCTION("NewBrowseDropDown",
                  hBrowseLinje,
                  "LinjeMerknad",
                  "LinjeMerknad",
                  "","","du|mmy").
ASSIGN hBrwOLMerk:NAME   = "fiLinjeMerknad"
       hBrwOLMerk:HELP   = "Merknad"
       hBrwOLMerk:FORMAT = "x(256)".
DYNAMIC-FUNCTION("setAttribute",hBrwOLMerk,"refreshrow","yes").

hBrwOSortiment = DYNAMIC-FUNCTION("NewBrowseDropDown",
                  hBrowseLinje,                  
                  "Sortimentkoder",                
                  "Sortimentkoder",                
                  "","","du|mmy").                           
ASSIGN hBrwOSortiment:NAME   = "fiSortimentkoder"
       hBrwOSortiment:HELP   = "Sortiment"
       hBrwOSortiment:FORMAT = "x(256)".
DYNAMIC-FUNCTION("setAttribute",hBrwOSortiment,"refreshrow","yes").

hBrwOKampanjeUke = DYNAMIC-FUNCTION("NewBrowseDropDown",
                  hBrowseLinje,                  
                  "Kampanjeuker",                
                  "Kampanjeuker",                
                  "","","du|mmy").                           
ASSIGN hBrwOKampanjeUke:NAME   = "fiKampanjeuker"
       hBrwOKampanjeUke:HELP   = "Kampanjeuker"
       hBrwOKampanjeUke:FORMAT = "x(256)".
DYNAMIC-FUNCTION("setAttribute",hBrwOKampanjeUke,"refreshrow","yes").

hBrwOKampanjeStotte = DYNAMIC-FUNCTION("NewBrowseDropDown",
                  hBrowseLinje,                  
                  "Kampanjestotte",                
                  "Kampanjestotte",                
                  "","","du|mmy").                           
ASSIGN hBrwOKampanjeStotte:NAME   = "fiKampanjestotte"
       hBrwOKampanjeStotte:HELP   = "Kampanjestøtte"
       hBrwOKampanjeStotte:FORMAT = "x(256)".
DYNAMIC-FUNCTION("setAttribute",hBrwOKampanjeStotte,"refreshrow","yes").

hBrwOLagerKode = DYNAMIC-FUNCTION("NewBrowseDropDown",
                  hBrowseLinje,                  
                  "LagerKoder",                
                  "LagerKoder",                
                  "","","du|mmy").                           
ASSIGN hBrwOLagerKode:NAME   = "fiLagerKode"
       hBrwOLagerKode:HELP   = "Lagerkode"
       hBrwOLagerKode:FORMAT = "x(256)".
DYNAMIC-FUNCTION("setAttribute",hBrwOLagerKode,"refreshrow","yes").

hBrwOSaSong = DYNAMIC-FUNCTION("NewBrowseDropDown",
                  hBrowseLinje,                  
                  "SasBeskr",                
                  "Sasong",                
                  "SaSong;SasBeskr;SaSong",
                  "WHERE true",
                  "").                           
ASSIGN hBrwOLagerKode:NAME   = "fiSesong"
       hBrwOLagerKode:HELP   = "Sesong"
       hBrwOLagerKode:FORMAT = "x(256)".
DYNAMIC-FUNCTION("setAttribute",hBrwOSaSong,"refreshrow","yes").

hBrwOSekv = DYNAMIC-FUNCTION("NewBrowseFillIn",
                  hBrowseLinje,          
                  "Sekv",     
                  "Sekv",     
                  "","","","").                
ASSIGN hBrwOSekv:NAME = "fiSekv"
       hBrwOSekv:HELP = "Sekvensnummer innenfor hovedgruppe".

DYNAMIC-FUNCTION("setAttribute",hBrwOVarekost,"refreshrow","yes").
/* IF bHKinst THEN DO:  */
  hBrwOLevFargKod = DYNAMIC-FUNCTION("NewBrowseFillIn",
                    hBrowseLinje,          
                    "LevFargKod",     
                    "LevFargKod",     
                    "","","","").                
  ASSIGN hBrwOLevFargKod:NAME = "fiVarekost"
         hBrwOLevFargKod:HELP = "Leverandørens fargekode".

  hBrwOBeskr = DYNAMIC-FUNCTION("NewBrowseFillIn",
                    hBrowseLinje,          
                    "Beskr",     
                    "Beskr",     
                    "","","","").                
  ASSIGN hBrwOBeskr:NAME = "fiVarekost"
         hBrwOBeskr:HELP = "Innkjøpspris".

  hBrwOInnkjopsPris = DYNAMIC-FUNCTION("NewBrowseFillIn",
                    hBrowseLinje,          
                    "InnkjopsPris",     
                    "InnkjopsPris",     
                    "","","","").                
  ASSIGN hBrwOInnkjopsPris:NAME = "fiInnkjopsPris"
         hBrwOInnkjopsPris:HELP = "Engros (veil.innkjøpspris)".
  DYNAMIC-FUNCTION("setAttribute",hBrwOInnkjopsPris,"refreshrow","yes").

  hBrwOPris = DYNAMIC-FUNCTION("NewBrowseFillIn",
                    hBrowseLinje,          
                    "Pris",     
                    "Pris",     
                    "","","","").                
  ASSIGN hBrwOPris:NAME = "fiPris"
         hBrwOPris:HELP = "Markedspris".
  DYNAMIC-FUNCTION("setAttribute",hBrwOPris,"refreshrow","yes").

  hBrwOAnbefaltPris = DYNAMIC-FUNCTION("NewBrowseFillIn",
                    hBrowseLinje,          
                    "AnbefaltPris",     
                    "AnbefaltPris",     
                    "","","","").                
  ASSIGN hBrwOAnbefaltPris:NAME = "fiAnbefaltPris"
         hBrwOAnbefaltPris:HELP = "Veil.pris".
/* END. */

hBrwOVarekost = DYNAMIC-FUNCTION("NewBrowseFillIn",
                  hBrowseLinje,          
                  "Varekost",     
                  "Varekost",     
                  "","","","").                
ASSIGN hBrwOVarekost:NAME = "fiVarekost"
       hBrwOVarekost:HELP = "Innkjøpspris".
DYNAMIC-FUNCTION("setAttribute",hBrwOVarekost,"refreshrow","yes").

hBrwOsupVarekost = DYNAMIC-FUNCTION("NewBrowseFillIn",
                  hBrowseLinje,          
                  "supVarekost",     
                  "supVarekost",     
                  "","","","").                
ASSIGN hBrwOsupVarekost:NAME = "fisupVarekost"
       hBrwOsupVarekost:HELP = "Innkjøpspris".
DYNAMIC-FUNCTION("setAttribute",hBrwOsupVarekost,"refreshrow","yes").

hBrwOforhRab% = DYNAMIC-FUNCTION("NewBrowseFillIn",
                  hBrowseLinje,
                  "forhRab%",
                  "forhRab%",
                  "","","","").
ASSIGN hBrwOforhRab%:NAME = "fiforhRab%"
       hBrwOforhRab%:HELP = "Rabatt, forhåndskjøp".
DYNAMIC-FUNCTION("setAttribute",hBrwOforhRab%,"refreshrow","yes").

hBrwOsupRab% = DYNAMIC-FUNCTION("NewBrowseFillIn",
                  hBrowseLinje,
                  "supRab%",
                  "supRab%",
                  "","","","").
ASSIGN hBrwOsupRab%:NAME = "fisupRab%"
       hBrwOsupRab%:HELP = "Rabatt, supleringskjøp".
DYNAMIC-FUNCTION("setAttribute",hBrwOsupRab%,"refreshrow","yes").

hBrwOPris = DYNAMIC-FUNCTION("NewBrowseFillIn",
                  hBrowseLinje,
                  "Pris",
                  "Pris",
                  "","","","").
ASSIGN hBrwOPris:NAME = "fiPris"
       hBrwOPris:HELP = "Utsalgspris".
DYNAMIC-FUNCTION("setAttribute",hBrwOPris,"refreshrow","yes").

hBrwOKampanjePris = DYNAMIC-FUNCTION("NewBrowseFillIn",
                  hBrowseLinje,
                  "KampanjePris",
                  "KampanjePris",
                  "","","","").
ASSIGN hBrwOKampanjePris:NAME = "fiKampanjePris"
       hBrwOKampanjePris:HELP = "Kampanjepris".
DYNAMIC-FUNCTION("setAttribute",hBrwOKampanjePris,"refreshrow","yes").

/* IF bHKinst THEN DO: */
  hBrwOKjedeRab% = DYNAMIC-FUNCTION("NewBrowseFillIn",
                    hBrowseLinje,       
                    "KjedeRab%",  
                    "KjedeRab%",  
                    "","","",""). 
  ASSIGN hBrwOKjedeRab%:NAME = "fiKjedeRab%"
         hBrwOKjedeRab%:HELP = "Kjederabatt".
  DYNAMIC-FUNCTION("setAttribute",hBrwOKjedeRab%,"refreshrow","yes").

  hBrwOKjedeSupRab% = DYNAMIC-FUNCTION("NewBrowseFillIn",
                    hBrowseLinje,       
                    "KjedeSupRab%",  
                    "KjedeSupRab%",  
                    "","","",""). 
  ASSIGN hBrwOKjedeSupRab%:NAME = "fiKjedeSupRab%"
         hBrwOKjedeSupRab%:HELP = "KjedeSuprabatt".
  DYNAMIC-FUNCTION("setAttribute",hBrwOKjedeSupRab%,"refreshrow","yes").

  hBrwOKjedeInnkPris = DYNAMIC-FUNCTION("NewBrowseFillIn",
                    hBrowseLinje,       
                    "KjedeInnkPris",  
                    "KjedeInnkPris",  
                    "","","",""). 
  ASSIGN hBrwOKjedeInnkPris:NAME = "fiKjedeInnkPris"
         hBrwOKjedeInnkPris:HELP = "Kjedens innkjøpspris".
  DYNAMIC-FUNCTION("setAttribute",hBrwOKjedeInnkPris,"refreshrow","yes").

  hBrwOKjedeSupInnkPris = DYNAMIC-FUNCTION("NewBrowseFillIn",
                    hBrowseLinje,       
                    "KjedeSupInnkPris",  
                    "KjedeSupInnkPris",  
                    "","","",""). 
  ASSIGN hBrwOKjedeSupInnkPris:NAME = "fiKjedeSupInnkPris"
         hBrwOKjedeSupInnkPris:HELP = "Kjedens sup.innkjøpspris".
  DYNAMIC-FUNCTION("setAttribute",hBrwOKjedeSupInnkPris,"refreshrow","yes").

  hBrwOAntall = DYNAMIC-FUNCTION("NewBrowseFillIn",
                    hBrowseLinje,      
                    "Antall",  
                    "Antall",  
                    "","","","").
  ASSIGN hBrwOAntall:NAME = "Antall"
         hBrwOAntall:HELP = "Antall forhåndskjøp".
  DYNAMIC-FUNCTION("setAttribute",hBrwOAntall,"refreshrow","yes").

  hBrwOSupAntall = DYNAMIC-FUNCTION("NewBrowseFillIn",
                    hBrowseLinje,      
                    "supAntall",  
                    "supAntall",
                    "","","","").
  ASSIGN hBrwOSupAntall:NAME = "supAntall"
         hBrwOSupAntall:HELP = "Antall suppleringskjøp".
  DYNAMIC-FUNCTION("setAttribute",hBrwOSupAntall,"refreshrow","yes").
/*
  hBrwOKjedeVare = DYNAMIC-FUNCTION("NewBrowseToggle",
                    hBrowseLinje,      
                    "KjedeVare",  
                    "KjedeVare",
                    "").
  ASSIGN hBrwOKjedeVare:NAME = "KjedeVare"
         hBrwOKjedeVare:HELP = "Kjedevare".
*/
  hBrwOKjedeVare = DYNAMIC-FUNCTION("NewBrowseFillIn",
                    hBrowseLinje,
                    "KjedeVare",
                    "KjedeVare",
                    "","","","").
  ASSIGN hBrwOKjedeVare:NAME   = "KjedeVare"
         hBrwOKjedeVare:HELP   = "KjedeVare".

/*
  hBrwOGjFakt = DYNAMIC-FUNCTION("NewBrowseToggle",
                    hBrowseLinje,      
                    "Gjennomfaktureres",  
                    "Gjennomfaktureres",
                    "").
  ASSIGN hBrwOGjFakt:NAME = "Gjennomfaktureres"
         hBrwOGjFakt:HELP = "Gjennomfaktureres".
*/
  hBrwOGjFakt = DYNAMIC-FUNCTION("NewBrowseFillIn",
                    hBrowseLinje,
                    "Gjennomfaktureres",
                    "Gjennomfaktureres",
                    "","","","").
  ASSIGN hBrwOGjFakt:NAME   = "Gjennomfaktureres"
         hBrwOGjFakt:HELP   = "Gjennomfaktureres".

/* END. */

hBrwOLevUke1 = DYNAMIC-FUNCTION("NewBrowseFillIn",
                  hBrowseLinje,                  
                  "LevUke1",                
                  "LevUke1",                
                  "","","","").                           
ASSIGN hBrwOLevUke1:NAME   = "fiLevUke1"
       hBrwOLevUke1:HELP   = "Leveringsuke 1".
DYNAMIC-FUNCTION("setAttribute",hBrwOLevUke1,"refreshrow","yes").

hBrwOLevUke2 = DYNAMIC-FUNCTION("NewBrowseFillIn",
                  hBrowseLinje,                  
                  "LevUke2",                
                  "LevUke2",                
                  "","","","").                           
ASSIGN hBrwOLevUke2:NAME   = "fiLevUke2"
       hBrwOLevUke2:HELP   = "Leveringsuke 2".
DYNAMIC-FUNCTION("setAttribute",hBrwOLevUke2,"refreshrow","yes").

hBrwOLevUke3 = DYNAMIC-FUNCTION("NewBrowseFillIn",
                  hBrowseLinje,                  
                  "LevUke3",                
                  "LevUke3",                
                  "","","","").                           
ASSIGN hBrwOLevUke3:NAME   = "fiLevUke3"
       hBrwOLevUke3:HELP   = "Leveringsuke 3".
DYNAMIC-FUNCTION("setAttribute",hBrwOLevUke3,"refreshrow","yes").

hBrwOLevUke4 = DYNAMIC-FUNCTION("NewBrowseFillIn",
                  hBrowseLinje,                  
                  "LevUke4",                
                  "LevUke4",                
                  "","","","").                           
ASSIGN hBrwOLevUke4:NAME   = "fiLevUke4"
       hBrwOLevUke4:HELP   = "Leveringsuke 4".
DYNAMIC-FUNCTION("setAttribute",hBrwOLevUke4,"refreshrow","yes").


hBrwOFrittTillegg = DYNAMIC-FUNCTION("NewBrowseFillIn",
                  hBrowseLinje,
                  "FrittTillegg",
                  "FrittTillegg",
                  "","","","").
ASSIGN hBrwOFrittTillegg:NAME   = "fiFrittTillegg"
       hBrwOFrittTillegg:HELP   = "Kan det kjøpes fritt i tillegg til fast inndeling (sortiment)".



RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION InitUtvalgToVarebok C-Win 
FUNCTION InitUtvalgToVarebok RETURNS LOGICAL
  ( INPUT ihUtvalg         AS HANDLE,
    INPUT ibUtvalgIsMaster AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose: Enable / disable buttons according to who is master
    Notes:  
------------------------------------------------------------------------------*/
DO WITH FRAME frmLinje:
  ASSIGN hUtvalg                    = ihUtvalg
         bUtvalgIsMaster            = ibUtvalgIsMaster
         .
  SettUtvalgSensitive(IF bUtvalgIsMaster THEN FALSE ELSE TRUE).
  SettHentUtvalgSensitive(IF VALID-HANDLE(hUtvalg) THEN TRUE ELSE FALSE).
END.
IF NOT ibUtvalgIsMaster THEN
  APPLY "value-changed" TO hBrowseListe.
  
RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LevPrintToExcel C-Win 
FUNCTION LevPrintToExcel RETURNS LOGICAL
  ( INPUT icFileAndCount AS CHAR,
    INPUT ibTestPrint    AS LOG) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR chExcelApplication      AS COM-HANDLE NO-UNDO.
DEF VAR chWorkbook              AS COM-HANDLE NO-UNDO.
DEF VAR chWorksheet             AS COM-HANDLE NO-UNDO.
DEF VAR chInterior              AS COM-HANDLE NO-UNDO.
DEF VAR chBorder                AS COM-HANDLE NO-UNDO.
DEF VAR chBefore                AS COM-HANDLE NO-UNDO.
DEF VAR iCount                  AS INTEGER NO-UNDO.
DEF VAR iNumCols                AS INT NO-UNDO INIT 14.
DEF VAR cRange                  AS CHAR NO-UNDO.
DEF VAR cFileName               AS CHAR NO-UNDO.
DEF VAR iCntStr                 AS INT NO-UNDO.
DEF VAR cCurrLevNavn            AS CHAR NO-UNDO.              
DEF VAR cPrevLevNavn            AS CHAR NO-UNDO.              
DEF VAR iShade                  AS INT NO-UNDO.
DEF VAR bFirstLevLine           AS LOG NO-UNDO.

ASSIGN cFileName = getReportFile(ENTRY(1,icFileAndCount,"|")) 
       iCount    = INT(ENTRY(2,icFileAndCount,"|"))
       .

chExcelApplication = DYNAMIC-FUNCTION("getExcelHandle").
IF NOT VALID-HANDLE(chExcelApplication) THEN DO:
  IF DYNAMIC-FUNCTION("setWebDoc","Open",cFileName) NE "" THEN
    MESSAGE "Could not open file: " cFileName VIEW-AS ALERT-BOX ERROR.
  RETURN TRUE.
END.
  
chExcelApplication:Workbooks:OpenText(cFileName,2,,,,,TRUE).

cRange = "A2:P" + STRING(iCount + 2). 

ASSIGN chWorkbook                           = chExcelApplication:WorkBooks:ITEM(1)
       chWorkSheet                          = chExcelApplication:Sheets:ITEM(1)
       chWorkSheet:NAME                     = ENTRY(1,hFieldMap:BUFFER-FIELD("VareBokBeskrivelse"):BUFFER-VALUE," ")

       chWorkSheet:Range(cRange):FONT:NAME  = "Arial"
       chWorkSheet:Range(cRange):FONT:SIZE  = 12

       chWorkSheet:Rows(1):FONT:Bold        = TRUE
       chWorkSheet:Rows(2):FONT:Bold        = TRUE
       chWorkSheet:Rows(2):FONT:NAME        = "Arial Narrow"
       chWorkSheet:Rows(2):HorizontalAlignment = 3
       chWorkSheet:PageSetup:CenterHeader   = "Varebok " + STRING(hFieldMap:BUFFER-FIELD("VareBokNr"):BUFFER-VALUE) + " - artikler pr leverandør"
       chWorkSheet:PageSetup:LeftFooter     = DYNAMIC-FUNCTION("getAppTitle")
       chWorkSheet:PageSetup:CenterFooter   = STRING(TODAY,"99/99/9999")  + " " + STRING(TIME,"HH:MM") /*"&D &T"*/
       chWorkSheet:PageSetup:RightFooter    = "Page &P of &N"
       chWorkSheet:PageSetup:Orientation    = 2
       chWorkSheet:PageSetup:FitToPagesWide = 1
       chWorkSheet:PageSetup:TopMargin      = 50
       chWorkSheet:PageSetup:LeftMargin     = 25
       chWorkSheet:PageSetup:RightMargin    = 25
       chWorkSheet:PageSetup:Zoom           = 65
       chWorkSheet:PageSetup:PrintTitleRows = "$1:$2"
       .

chWorkSheet:Range("A1:P2"):SELECT().
chInterior = chExcelApplication:SELECTION:Interior.
chInterior:ColorIndex = 37.

chWorkSheet:Range("A2:F2"):HorizontalAlignment = {&xlLeft}.
chWorkSheet:Range("P2:P2"):HorizontalAlignment = {&xlLeft}.
chWorkSheet:Range("A1"):FONT:SIZE = 18.

chWorkSheet:Range("B:B"):NumberFormat = "0".
chWorkSheet:Range("E:E"):NumberFormat = "@".

chBorder = chWorkSheet:Range("a1:p" + STRING(iCount + 2)):Borders({&xlEdgeTop}).
ASSIGN chBorder:LineStyle = {&xlContinuous}
       chBorder:Weight    = {&xlThin}.
chBorder = chWorkSheet:Range("a1:p" + STRING(iCount + 2)):Borders({&xlEdgeBottom}).
ASSIGN chBorder:LineStyle = {&xlContinuous}
       chBorder:Weight    = {&xlThin}.
chBorder = chWorkSheet:Range("a1:p" + STRING(iCount + 2)):Borders({&xlEdgeRight}).
ASSIGN chBorder:LineStyle = {&xlContinuous}
       chBorder:Weight    = {&xlThin}.
chBorder = chWorkSheet:Range("a1:p" + STRING(iCount + 2)):Borders({&xlEdgeLeft}).
ASSIGN chBorder:LineStyle = {&xlContinuous}
       chBorder:Weight    = {&xlThin}.

chWorkSheet:Range("p:p"):columnwidth = 15.
chWorkSheet:Range("p1:p" + STRING(iCount + 2)):WrapText = TRUE.

IF NOT ibTestPrint THEN DO ix = 3 TO iCount + 2:
  IF chWorkSheet:Range("F" + STRING(ix)):VALUE = ? THEN DO:
    cRange = "A" + STRING(ix).
    chWorkSheet:Range(cRange):FONT:Bold = TRUE.
    iShade = 1.
    IF bFirstLevLine THEN DO:
      chBefore = chWorkSheet:Range(cRange).
      chWorkSheet:HPageBreaks:Add(chBefore).

      chBorder = chWorkSheet:Range("A" + STRING(ix - 1) + ":P" + STRING(ix - 1)):Borders({&xlEdgeBottom}).
      ASSIGN chBorder:LineStyle = {&xlContinuous}
             chBorder:Weight    = {&xlThin}.

      chBorder = chWorkSheet:Range("A" + STRING(ix) + ":P" + STRING(ix)):Borders({&xlEdgeTop}).
      ASSIGN chBorder:LineStyle = {&xlContinuous}
             chBorder:Weight    = {&xlThin}
             bFirstLevLine      = FALSE.
    END.
  END.
  ELSE DO:
/*     iShade = iShade + 1.  */
/*     IF iShade MOD 2 = 0 THEN DO: */
/*       chWorkSheet:Range("A" + STRING(ix) + ":Q" + STRING(ix)):SELECT(). */
/*       chInterior = chExcelApplication:SELECTION:Interior.               */
/*       chInterior:ColorIndex = 15.                                       */
/*     END. */

    chBorder = chWorkSheet:Range("A" + STRING(ix - 1) + ":p" + STRING(ix - 1)):Borders({&xlEdgeBottom}).
    ASSIGN chBorder:LineStyle = {&xlContinuous}
           chBorder:Weight    = {&xlThin}.

    chBorder = chWorkSheet:Range("F" + STRING(ix) + ":F" + STRING(ix)):Borders({&xlEdgeRight}).
    ASSIGN chBorder:LineStyle = {&xlContinuous}
           chBorder:Weight    = {&xlThin}.
    chBorder = chWorkSheet:Range("G" + STRING(ix) + ":G" + STRING(ix)):Borders({&xlEdgeRight}).
    ASSIGN chBorder:LineStyle = {&xlContinuous}
           chBorder:Weight    = {&xlThin}.
    chBorder = chWorkSheet:Range("H" + STRING(ix) + ":H" + STRING(ix)):Borders({&xlEdgeRight}).
    ASSIGN chBorder:LineStyle = {&xlContinuous}
           chBorder:Weight    = {&xlThin}.
    chBorder = chWorkSheet:Range("I" + STRING(ix) + ":I" + STRING(ix)):Borders({&xlEdgeRight}).
    ASSIGN chBorder:LineStyle = {&xlContinuous}
           chBorder:Weight    = {&xlThin}.
    chBorder = chWorkSheet:Range("J" + STRING(ix) + ":J" + STRING(ix)):Borders({&xlEdgeRight}).
    ASSIGN chBorder:LineStyle = {&xlContinuous}
           chBorder:Weight    = {&xlThin}.
    chBorder = chWorkSheet:Range("K" + STRING(ix) + ":K" + STRING(ix)):Borders({&xlEdgeRight}).
    ASSIGN chBorder:LineStyle = {&xlContinuous}
           chBorder:Weight    = {&xlThin}.
    chBorder = chWorkSheet:Range("L" + STRING(ix) + ":L" + STRING(ix)):Borders({&xlEdgeRight}).
    ASSIGN chBorder:LineStyle = {&xlContinuous}
           chBorder:Weight    = {&xlThin}.
    chBorder = chWorkSheet:Range("M" + STRING(ix) + ":M" + STRING(ix)):Borders({&xlEdgeRight}).
    ASSIGN chBorder:LineStyle = {&xlContinuous}
           chBorder:Weight    = {&xlThin}.
    chBorder = chWorkSheet:Range("N" + STRING(ix) + ":N" + STRING(ix)):Borders({&xlEdgeRight}).
    ASSIGN chBorder:LineStyle = {&xlContinuous}
           chBorder:Weight    = {&xlThin}.
    chBorder = chWorkSheet:Range("O" + STRING(ix) + ":O" + STRING(ix)):Borders({&xlEdgeRight}).
    ASSIGN chBorder:LineStyle = {&xlContinuous}
           chBorder:Weight    = {&xlThin}.

    ASSIGN chWorkSheet:Range("G" + STRING(ix) + ":N" + STRING(ix)):NumberFormat = "0,00"
           chWorkSheet:Range("H" + STRING(ix)):FONT:Bold = TRUE
           chWorkSheet:Range("K" + STRING(ix)):FONT:Bold = TRUE
           chWorkSheet:Range("M" + STRING(ix)):FONT:Bold = TRUE
           bFirstLevLine = TRUE
           .

/*     ASSIGN chWorkSheet:Range("F" + STRING(ix) + ":N" + STRING(ix)):NumberFormat = "0,00" */
/*            chWorkSheet:Range("G" + STRING(ix)):FONT:Bold = TRUE                          */
/*            chWorkSheet:Range("J" + STRING(ix)):FONT:Bold = TRUE                          */
/*            chWorkSheet:Range("M" + STRING(ix)):FONT:Bold = TRUE                          */
  END.
END.

chWorkSheet:Columns("B:O"):AutoFit().
chWorkSheet:Range("A:A"):columnwidth = chWorkSheet:Range("A:A"):columnwidth * 2.5.

chWorkSheet:Range("A2"):Select().
chExcelApplication:ActiveWindow:FreezePanes = TRUE.

chExcelApplication:VISIBLE = TRUE.

/* release com-handles */
RELEASE OBJECT chBorder NO-ERROR.
RELEASE OBJECT chBefore NO-ERROR.
RELEASE OBJECT chInterior NO-ERROR.
RELEASE OBJECT chWorksheet NO-ERROR.
RELEASE OBJECT chWorkbook NO-ERROR.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LinkOverlays C-Win 
FUNCTION LinkOverlays RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DYNAMIC-FUNCTION("DeleteObjectLink",hBrowseLinje,hBrwOInnkjopsPris). 
DYNAMIC-FUNCTION("DeleteObjectLink",hBrowseLinje,hBrwOforhRab%).
DYNAMIC-FUNCTION("DeleteObjectLink",hBrowseLinje,hBrwOsupRab%).
DYNAMIC-FUNCTION("DeleteObjectLink",hBrowseLinje,hBrwOKjedeRab%).
DYNAMIC-FUNCTION("DeleteObjectLink",hBrowseLinje,hBrwOKjedeInnkPris).
DYNAMIC-FUNCTION("DeleteObjectLink",hBrowseLinje,hBrwOKjedeSupRab%).
DYNAMIC-FUNCTION("DeleteObjectLink",hBrowseLinje,hBrwOKjedeSupInnkPris).
DYNAMIC-FUNCTION("DeleteObjectLink",hBrowseLinje,hBrwOPris).
DYNAMIC-FUNCTION("DeleteObjectLink",hBrowseLinje,hBrwOKampanjePris).
DYNAMIC-FUNCTION("DeleteObjectLink",hBrowseLinje,hBrwOAntall).
DYNAMIC-FUNCTION("DeleteObjectLink",hBrowseLinje,hBrwOSupAntall).
DYNAMIC-FUNCTION("DeleteObjectLink",hBrowseLinje,hBrwOLMerk).
DYNAMIC-FUNCTION("DeleteObjectLink",hBrowseLinje,hBrwOLevFargKod).
DYNAMIC-FUNCTION("DeleteObjectLink",hBrowseLinje,hBrwOPris).
DYNAMIC-FUNCTION("DeleteObjectLink",hBrowseLinje,hBrwOVarekost).
DYNAMIC-FUNCTION("DeleteObjectLink",hBrowseLinje,hBrwOSupVarekost).
DYNAMIC-FUNCTION("DeleteObjectLink",hBrowseLinje,hBrwOAnbefaltPris).
DYNAMIC-FUNCTION("DeleteObjectLink",hBrowseLinje,hBrwOBeskr).
DYNAMIC-FUNCTION("DeleteObjectLink",hBrowseLinje,hBrwOLevUke1).
DYNAMIC-FUNCTION("DeleteObjectLink",hBrowseLinje,hBrwOLevUke2).
DYNAMIC-FUNCTION("DeleteObjectLink",hBrowseLinje,hBrwOLevUke3).
DYNAMIC-FUNCTION("DeleteObjectLink",hBrowseLinje,hBrwOLevUke4).
DYNAMIC-FUNCTION("DeleteObjectLink",hBrowseLinje,hBrwOFrittTillegg).
DYNAMIC-FUNCTION("DeleteObjectLink",hBrowseLinje,hBrwOSekv).
DYNAMIC-FUNCTION("DeleteObjectLink",hBrowseLinje,hBrwOKjedeVare).
DYNAMIC-FUNCTION("DeleteObjectLink",hBrowseLinje,hBrwOGjFakt).
DYNAMIC-FUNCTION("DeleteObjectLink",hBrowseLinje,hBrwOSortiment).
DYNAMIC-FUNCTION("DeleteObjectLink",hBrowseLinje,hBrwOKampanjeUke).
DYNAMIC-FUNCTION("DeleteObjectLink",hBrowseLinje,hBrwOKampanjeStotte).
DYNAMIC-FUNCTION("DeleteObjectLink",hBrowseLinje,hBrwOLagerKode).
DYNAMIC-FUNCTION("DeleteObjectLink",hBrowseLinje,hBrwOSaSong).

ASSIGN hBrwOVarekost:HIDDEN        = TRUE
       hBrwOsupVarekost:HIDDEN     = TRUE
       hBrwOPris:HIDDEN            = TRUE       
       hBrwOKampanjePris:HIDDEN    = TRUE       
/*        hBrwOLMerk:HIDDEN    = TRUE  */
       .

/* IF bHKinst THEN  */
  ASSIGN 
         hBrwOKjedeRab%:HIDDEN     = TRUE  
         hBrwOKjedeInnkPris:HIDDEN = TRUE
         hBrwOKjedeSupRab%:HIDDEN     = TRUE  
         hBrwOKjedeSupInnkPris:HIDDEN = TRUE
         hBrwOAntall:HIDDEN        = TRUE     
         hBrwOSupAntall:HIDDEN     = TRUE  
         .

DYNAMIC-FUNCTION("CreateOverlayLink",hBrowseLinje,hBrwOSortiment,"Sortimentkoder").
DYNAMIC-FUNCTION("CreateOverlayLink",hBrowseLinje,hBrwOKampanjeUke,"Kampanjeuker").
DYNAMIC-FUNCTION("CreateOverlayLink",hBrowseLinje,hBrwOKampanjeStotte,"KampanjeStotte").
DYNAMIC-FUNCTION("CreateOverlayLink",hBrowseLinje,hBrwOLagerkode,"Lagerkoder").
DYNAMIC-FUNCTION("CreateOverlayLink",hBrowseLinje,hBrwOSaSong,"SasBeskr").

IF rsButModus THEN DO:
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowseLinje,hBrwOVarekost,"Varekost").
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowseLinje,hBrwOForhRab%,"ForhRab%").
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowseLinje,hBrwOSupRab%,"SupRab%").
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowseLinje,hBrwOSupVarekost,"supVarekost").
/*   DYNAMIC-FUNCTION("CreateOverlayLink",hBrowseLinje,hBrwOKampanjePris,"KampanjePris"). */
/*   DYNAMIC-FUNCTION("CreateOverlayLink",hBrowseLinje,hBrwOLMerk,"LinjeMerknad"). */
END.
ELSE DO:
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowseLinje,hBrwOForhRab%,"ForhRab%").
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowseLinje,hBrwOSupRab%,"SupRab%").
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowseLinje,hBrwOVarekost,"Varekost").
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowseLinje,hBrwOSupVarekost,"supVarekost").
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowseLinje,hBrwOPris,"Pris").
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowseLinje,hBrwOKampanjePris,"KampanjePris").
/*   DYNAMIC-FUNCTION("CreateOverlayLink",hBrowseLinje,hBrwOLMerk,"LinjeMerknad").  */
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowseLinje,hBrwOLevUke1,"LevUke1"). 
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowseLinje,hBrwOLevUke2,"LevUke2"). 
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowseLinje,hBrwOLevUke3,"LevUke3"). 
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowseLinje,hBrwOLevUke4,"LevUke4"). 
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowseLinje,hBrwOSekv,"Sekv").
/*   IF bHKinst THEN DO:  */
    DYNAMIC-FUNCTION("CreateOverlayLink",hBrowseLinje,hBrwOBeskr,"Beskr").
    DYNAMIC-FUNCTION("CreateOverlayLink",hBrowseLinje,hBrwOLevFargKod,"LevFargKod").
    DYNAMIC-FUNCTION("CreateOverlayLink",hBrowseLinje,hBrwOInnkjopsPris,"InnkjopsPris").
    DYNAMIC-FUNCTION("CreateOverlayLink",hBrowseLinje,hBrwOAnbefaltPris,"AnbefaltPris").
    DYNAMIC-FUNCTION("CreateOverlayLink",hBrowseLinje,hBrwOKjedeRab%,"KjedeRab%").
    DYNAMIC-FUNCTION("CreateOverlayLink",hBrowseLinje,hBrwOKjedeInnkPris,"KjedeInnkPris").
    DYNAMIC-FUNCTION("CreateOverlayLink",hBrowseLinje,hBrwOKjedeSupRab%,"KjedeSupRab%").
    DYNAMIC-FUNCTION("CreateOverlayLink",hBrowseLinje,hBrwOKjedeSupInnkPris,"KjedeSupInnkPris").
    DYNAMIC-FUNCTION("CreateOverlayLink",hBrowseLinje,hBrwOAntall,"Antall").
    DYNAMIC-FUNCTION("CreateOverlayLink",hBrowseLinje,hBrwOSupAntall,"supAntall").
    DYNAMIC-FUNCTION("CreateOverlayLink",hBrowseLinje,hBrwOFrittTillegg,"FrittTillegg").
    DYNAMIC-FUNCTION("CreateOverlayLink",hBrowseLinje,hBrwOKjedeVare,"KjedeVare").
    DYNAMIC-FUNCTION("CreateOverlayLink",hBrowseLinje,hBrwOGjFakt,"Gjennomfaktureres").
/*   END. */
END.

APPLY "value-changed" TO hBrowseLinje.

RETURN TRUE.   

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LoadRowsToBatchSettings C-Win 
FUNCTION LoadRowsToBatchSettings RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR iy           AS INT NO-UNDO.
DEF VAR cWinSettings AS CHAR NO-UNDO.

cWinSettings = DYNAMIC-FUNCTION("getCustomWinSettings",THIS-PROCEDURE:CURRENT-WINDOW).

DO ix = 1 TO NUM-ENTRIES(cWinSettings,"|"):
  IF ENTRY(ix,cWinSettings,"|") = "<StartRowsToBatch>" THEN
    DO iy = ix + 1 TO NUM-ENTRIES(cWinSettings,"|") BY 2:
      IF ENTRY(iy,cWinSettings,"|") = "<EndRowsToBatch>" THEN LEAVE.
      ELSE DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"rowsToBatch",ENTRY(iy + 1,cWinSettings,"|")).
    END.
END.
                       
RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SaveSortString C-Win 
FUNCTION SaveSortString RETURNS LOGICAL
  ( INPUT icName        AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cNameList  AS CHAR   NO-UNDO.
DEF VAR cValueList AS CHAR   NO-UNDO.
DEF VAR cAttrList  AS CHAR   NO-UNDO.

/*

IF icName NE "" THEN DO:
  DYNAMIC-FUNCTION("setUserSetting",THIS-PROCEDURE:FILE-NAME,
                   "userquery",
                   DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"usersettingcontext"),
                   "",
                   icName).
  SetMyQueriesMenu().
  
  DYNAMIC-FUNCTION("setUserSetting",THIS-PROCEDURE:FILE-NAME,
                   "",
                   DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"usersettingcontext"),
                   cCurrQuerySettings,
                   "delete_setting").

  IF icName = "delete_setting" THEN DO:
    ClearAttributes().
    DYNAMIC-FUNCTION("setToolbarToggles",hToolbar,NO,"").
    RETURN YES.
  END.
END.

DO ix = 1 TO NUM-ENTRIES(cQuerySettingList):
  cAttrList  = DYNAMIC-FUNCTION("getAttributeList",hBrowseLinje
                                ,ENTRY(ix,cQuerySettingList)
                                ,""
                                ,YES).
/*                                 ,IF ENTRY(ix,cQuerySettingList) = "FieldGroup_" THEN "0" ELSE "" */
/*                                 ,NO).                                                            */
  IF ENTRY(1,cAttrList,"|") NE "" THEN
    ASSIGN cNameList  = cNameList  + ENTRY(1,cAttrList,"|") + ","
           cValueList = cValueList + ENTRY(2,cAttrList,"|") + CHR(1).
END.
ASSIGN cValueList = SUBSTR(cValueList,1,LENGTH(cValueList) - 1)
       cNameList  = SUBSTR(cNameList,1,LENGTH(cNameList) - 1).

DYNAMIC-FUNCTION("setUserSetting",THIS-PROCEDURE:FILE-NAME,
                 "",
                 DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"usersettingcontext"),
                 cNameList,
                 cValueList).
*/                 
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setFieldColor C-Win 
FUNCTION setFieldColor RETURNS INTEGER
  ( INPUT iRGBcolor AS INT) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR iy AS INT NO-UNDO.

IF iRGBcolor = 0 THEN RETURN 15.

IF iRGBcolor NE 0 THEN DO iy = 0 TO COLOR-TABLE:NUM-ENTRIES:
  IF COLOR-TABLE:GET-RGB-VALUE(iy) = iRGBcolor THEN RETURN iy.
END.

ASSIGN iy = COLOR-TABLE:NUM-ENTRIES
       COLOR-TABLE:NUM-ENTRIES = iy + 1.

IF iy = 256 THEN
  MESSAGE PROGRAM-NAME(1) SKIP
          256 SKIP
          VIEW-AS ALERT-BOX.

COLOR-TABLE:SET-DYNAMIC(iy, YES).
COLOR-TABLE:SET-RGB-VALUE(iy,iRGBcolor).

RETURN iy.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setSpesialSortMenu C-Win 
FUNCTION setSpesialSortMenu RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hMnuItem        AS HANDLE NO-UNDO.
DEF VAR hNextMnuItem    AS HANDLE NO-UNDO.
DEF VAR cMenuList       AS CHAR   NO-UNDO.
DEF VAR cMenuLabels     AS CHAR   NO-UNDO.

IF NOT VALID-HANDLE(hSpesialSortMnu) THEN RETURN NO.  

IF VALID-HANDLE(hSpesialSortMnu:FIRST-CHILD) THEN DO:
  ASSIGN hMnuItem     = hSpesialSortMnu:FIRST-CHILD
         hNextMnuItem = hMnuItem.

  REPEAT WHILE VALID-HANDLE(hNextMnuItem):
    hNextMnuItem = hMnuItem:NEXT-SIBLING.
    DELETE OBJECT hMnuItem NO-ERROR.
    IF VALID-HANDLE(hNextMnuItem) THEN
      hMnuItem = hNextMnuItem.
  END.
END.  

cMenuLabels = DYNAMIC-FUNCTION("getFieldList","JBoxGenCode;cDescription;cMisc1","WHERE cCodeType = 'VarebokSort' BY cDescription").

IF cMenuLabels NE "" THEN DO:    
  DO ix = 1 TO NUM-ENTRIES(cMenuLabels,"|") - 1 BY 2:
    cMenuList = cMenuList + "VelgSpesialSort;" + ENTRY(ix,cMenuLabels,"|") 
              + (IF ENTRY(ix + 1,cMenuLabels,"|") NE "" AND LOGICAL(ENTRY(ix + 1,cMenuLabels,"|")) THEN
                   " (standard Messereg.)"
                 ELSE "")
              + ";VelgSpesialSort;toggle,".
  END.
  cMenuList = cMenuList + "rule,".
END.

DYNAMIC-FUNCTION("NewMenuBand",
                  hSpesialSortMnu, 
                  cMenuList + "SortSpesial;Ny.."
                 + (IF cMenuLabels NE "" THEN ",SlettSpesialSort;Slett.." ELSE "")
                 ,"").
  
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SetTabStrip C-Win 
FUNCTION SetTabStrip RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR iy            AS INT NO-UNDO.
DEF VAR cTabStripList AS CHAR NO-UNDO.

/* cTabStripList = DYNAMIC-FUNCTION("getFieldList","JBoxGenCode;cCodeValue;cMisc1;cMisc2",               */
/*                                   "WHERE cCodeType = 'JBoxQueryTabs'" +                               */
/*                                   "  AND cLanguage = '" + DYNAMIC-FUNCTION("getLanguageCode") + "'" + */
/*                                   " BY cMisc1") NO-ERROR.                                             */

cTabStripList = "Liste|Detalj|Linje".

IF cTabStripList NE "" THEN DO:
  DO ix = 9 TO 1 BY -1:
    chTabStrip:TabStrip:Tabs:Remove(ix) NO-ERROR.
  END.
  DO ix = 1 TO NUM-ENTRIES(cTabStripList,"|"):
    iy = iy + 1.
    chTabStrip:TabStrip:Tabs:ADD(iy).
    chTabStrip:TabStrip:Tabs:Item(iy):Caption = ENTRY(ix,cTabStripList,"|").
  END.
END.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SettHentUtvalgSensitive C-Win 
FUNCTION SettHentUtvalgSensitive RETURNS LOGICAL
  ( INPUT ibSensitive AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hWidget  AS HANDLE NO-UNDO.
DO ix = 1 TO NUM-ENTRIES(cBtnHentUtvalgHandles):
  hWidget = WIDGET-HANDLE(ENTRY(ix,cBtnHentUtvalgHandles)).
  IF hWidget:TYPE = "button" THEN DO:
    hWidget:HIDDEN = NOT ibSensitive.
    IF NOT hWidget:HIDDEN THEN
      hWidget:SENSITIVE = TRUE.
  END.
  ELSE
    hWidget:SENSITIVE = ibSensitive.
END.
RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SettUtvalgSensitive C-Win 
FUNCTION SettUtvalgSensitive RETURNS LOGICAL
  ( INPUT ibSensitive AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hWidget  AS HANDLE NO-UNDO.
DO ix = 1 TO NUM-ENTRIES(cBtnUtvalgHandles):
  hWidget = WIDGET-HANDLE(ENTRY(ix,cBtnUtvalgHandles)).
  hWidget:SENSITIVE = ibSensitive.
END.
RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION StartSpesialSort C-Win 
FUNCTION StartSpesialSort RETURNS LOGICAL
  ( INPUT icSortDescr AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cSortString AS CHAR   NO-UNDO.
DEF VAR hMnuItem    AS HANDLE NO-UNDO.
DEF VAR c4First     AS CHAR   NO-UNDO.
DEF VAR iy          AS INT    NO-UNDO.


hMnuItem = hSpesialSortMnu:FIRST-CHILD.
REPEAT WHILE VALID-HANDLE(hMnuItem):
  hMnuItem:CHECKED = icSortDescr = hMnuItem:LABEL NO-ERROR.
  hMnuItem = hMnuItem:NEXT-SIBLING.
END.

IF INDEX(icSortDescr," (standard Messereg.)") > 0 THEN
  icSortDescr = SUBSTR(icSortDescr,1,INDEX(icSortDescr," (standard Messereg.)") - 1).

cSortString = DYNAMIC-FUNCTION("getFieldValues","JBoxGenCode",
                               "WHERE JBoxGenCode.cCodeType = 'VarebokSort' AND JBoxGenCode.cDescription = '" + icSortDescr + "'",
                               "cCodeValue").

IF cSortString NE ? THEN DO:
  DYNAMIC-FUNCTION("setSortLabel",hBrowseLinje,"",NO).

  DO ix = 1 TO NUM-ENTRIES(cSortString," "):
    IF ENTRY(ix,cSortString," ") NE "desc" AND ENTRY(ix,cSortString," ") NE "by" THEN
      ASSIGN iy = iy + 1 
             c4First = c4First + (IF c4First NE "" THEN "," ELSE "") + ENTRY(ix,cSortString," ").
    ELSE IF ENTRY(ix,cSortString," ") = "desc" THEN
      c4First = c4First + ";DESC".
    IF iy = 4 THEN LEAVE.
  END.

  DYNAMIC-FUNCTION("setSortString",hBrowseLinje,c4First).
  DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"querysort",cSortString).
  DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"localsort",cSortString).
  DYNAMIC-FUNCTION("setCurrentObject",hBrowseLinje).
  RUN OpenQuery.
END.
ELSE RETURN NO.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION StrToExcel C-Win 
FUNCTION StrToExcel RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR httArtStr      AS HANDLE NO-UNDO.
DEF VAR hStrQuery      AS HANDLE NO-UNDO.

DEF VAR iy        AS INT NO-UNDO.
DEF VAR cStrList  AS CHAR NO-UNDO.

SESSION:SET-WAIT-STATE("general").

CREATE TEMP-TABLE httArtStr.
DO ix = 1 TO hBrowseLinje:NUM-COLUMNS:
  httArtStr:ADD-LIKE-FIELD(hBrowseLinje:GET-BROWSE-COLUMN(ix):NAME,hBrowseLinje:GET-BROWSE-COLUMN(ix):BUFFER-FIELD).
END.
httArtStr:ADD-NEW-FIELD("sostorl","CHARACTER",0,"x(5)","","Str").
httArtStr:ADD-NEW-FIELD("kode","CHARACTER",0,"x(20)","","EANkode").
httArtStr:ADD-NEW-FIELD("LevDato2","DATE",0,"99/99/99","","").
httArtStr:ADD-NEW-FIELD("LevDato3","DATE",0,"99/99/99","","").
httArtStr:ADD-NEW-FIELD("LevDato4","DATE",0,"99/99/99","","").

httArtStr:TEMP-TABLE-PREPARE("ttVarebokLinje").
hBufArtStr = httArtStr:DEFAULT-BUFFER-HANDLE.

IF hBrowseLinje:NUM-SELECTED-ROWS > 1 THEN
  DO ix = 1 TO hBrowseLinje:NUM-SELECTED-ROWS:
    IF hBrowseLinje:FETCH-SELECTED-ROW(ix) THEN DO:
      cStrList = DYNAMIC-FUNCTION("getFieldList","strtstr;sostorl,strkonv;!strkode,strekkode;kode",
                                  "WHERE StrTypeId = " +
                                    DYNAMIC-FUNCTION("getFieldValues","ArtBas",
                                                     "WHERE ArtikkelNr = "
                                                          + STRING(hFieldMapLinje:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE),
                                                     "StrTypeId"
                                                     )
                                   + ", FIRST strkonv WHERE storl = strtstr.sostorl NO-LOCK"
                                   + ", FIRST strekkode NO-LOCK WHERE Strekkode.ArtikkelNr = " + STRING(hFieldMapLinje:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE)
                                     + " AND strekkode.kode > ''"
                                     + " AND strekkode.strkode = strkonv.strkode"
                                  ).

      DO iy = 1 TO MAX(1,NUM-ENTRIES(cStrList,"|") - 1) BY 2:
        hBufArtStr:BUFFER-CREATE().
        hBufArtStr:BUFFER-COPY(hFieldMapLinje).
        hBufArtStr:BUFFER-FIELD("sostorl"):BUFFER-VALUE = (IF cStrList NE "" THEN "`" + TRIM(ENTRY(iy,cStrList,"|")) ELSE "").
        hBufArtStr:BUFFER-FIELD("kode"):BUFFER-VALUE = (IF cStrList NE "" THEN TRIM(ENTRY(iy + 1,cStrList,"|")) ELSE "").
      END.
    END.
  END.
ELSE DO:
  hBrowseLinje:QUERY:GET-FIRST().
  REPEAT WHILE NOT hBrowseLinje:QUERY:QUERY-OFF-END:
/*     cStrList = "".                                                                                               */
/*     FOR EACH strtstr  NO-LOCK                                                                                    */
/*         WHERE strtstr.strtypeid = INT(hFieldMapLinje:BUFFER-FIELD("StrTypeId"):BUFFER-VALUE),                    */
/*         FIRST strkonv WHERE strkonv.storl = strtstr.sostorl NO-LOCK,                                             */
/*         FIRST strekkode WHERE strekkode.artikkelnr = DEC(hFieldMapLinje:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) */
/*                           AND strekkode.strkode = strkonv.strkode:                                               */
/*                                                                                                                  */
/*       cStrList = cStrList + STRING(sostorl) + "|" + strekkode.kode + "|".                                        */
/*     END.                                                                                                         */
/*     cStrList = TRIM(cStrList,"|").                                                                               */

    cStrList = DYNAMIC-FUNCTION("getFieldList","strtstr;sostorl,strkonv;!strkode,strekkode;kode",
                                "WHERE StrTypeId = " +
                                  DYNAMIC-FUNCTION("getFieldValues","ArtBas",
                                                   "WHERE ArtikkelNr = "
                                                        + STRING(hFieldMapLinje:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE),
                                                   "StrTypeId"
                                                   )
                                 + ", FIRST strkonv WHERE storl = strtstr.sostorl NO-LOCK"
                                 + ", FIRST strekkode NO-LOCK WHERE Strekkode.ArtikkelNr = " + STRING(hFieldMapLinje:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE)
                                   + " AND strekkode.kode > ''"
                                   + " AND strekkode.strkode = strkonv.strkode"
                                ).

    DO iy = 1 TO MAX(1,NUM-ENTRIES(cStrList,"|") - 1) BY 2:
      hBufArtStr:BUFFER-CREATE().
      hBufArtStr:BUFFER-COPY(hFieldMapLinje).
      hBufArtStr:BUFFER-FIELD("sostorl"):BUFFER-VALUE = (IF cStrList NE "" THEN "`" + TRIM(ENTRY(iy,cStrList,"|")) ELSE "").
      hBufArtStr:BUFFER-FIELD("kode"):BUFFER-VALUE = (IF cStrList NE "" THEN TRIM(ENTRY(iy + 1,cStrList,"|")) ELSE "").
    END.
    hBrowseLinje:QUERY:GET-NEXT().
  END.
END.


MESSAGE PROGRAM-NAME(1) SKIP
        ETIME / 1000
        VIEW-AS ALERT-BOX.
ETIME(TRUE).

ExcelAuto(hBufArtStr).
SESSION:SET-WAIT-STATE("").

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION TabStripChanged C-Win 
FUNCTION TabStripChanged RETURNS LOGICAL
  ( INPUT iiTab AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cFollowXbar   AS CHAR NO-UNDO.
ASSIGN
    cState = DYNAMIC-FUNCTION('getToolbarState',hUpdToolBar)
    .

DO WITH FRAME {&FRAME-NAME}:

  IF iiTab > 10 THEN DO:
    iiTab = iiTab - 10.
    chTabStrip:TabStrip:Tabs:ITEM(iiTab):SELECTED = TRUE.
  END.

  iTab = iiTab.

  hArtBildeFrame:MOVE-TO-BOTTOM().

  IF iTab < 3 THEN DO:
    DYNAMIC-FUNCTION("ReplaceObjectLink",hUpdToolbar,hFieldMap).
    DYNAMIC-FUNCTION("ReplaceObjectLink",hUpdToolbar,hBrowseListe).
    IF VALID-HANDLE(hNavVarebokToolBar) THEN
      DYNAMIC-FUNCTION("DeleteObject",hNavVarebokToolBar).

    hRowsToBatchMenu:SENSITIVE = FALSE.
  END.

  IF iiTab = 1 THEN
  DO:
      FRAME frmListe:MOVE-TO-TOP().
      FRAME frmFilter:MOVE-TO-TOP().
      APPLY "entry" TO hBrowseListe.
  END.
  ELSE IF iiTab = 2 THEN DO:
    FRAME frmDetalj:MOVE-TO-TOP().
    IF CAN-DO('new',cstate) THEN
        ASSIGN
            VareBokNr:SENSITIVE      = FALSE
            ProfilNr:SENSITIVE       = TRUE 
            btnSokProfilNr:SENSITIVE = TRUE
            .
    ELSE
        ASSIGN
            VareBokNr:SENSITIVE      = FALSE
            ProfilNr:SENSITIVE       = FALSE  
            btnSokProfilNr:SENSITIVE = FALSE
            .
  END.
  IF iiTab = 3 THEN DO:
    hArtBildeFrame:MOVE-TO-TOP().
    FRAME frmLinje:MOVE-TO-TOP().
    hColumn = hBrowseLinje:GET-BROWSE-COLUMN(1).
    APPLY "END-RESIZE" TO hColumn.

    DYNAMIC-FUNCTION("replaceObjectLink",hBrowseLinje,hUpdToolbar).
    DYNAMIC-FUNCTION("replaceObjectLink",hFieldMapLinje,hUpdToolbar).

    IF NOT hFieldMap:AVAIL THEN 
      hBrowseListe:SELECT-ROW(hBrowseListe:FOCUSED-ROW) NO-ERROR.

    IF hFieldMap:AVAIL THEN DO:
      hNavVarebokToolBar = DYNAMIC-FUNCTION("NewToolBar",
                        rectNavVarebok:HANDLE,            /* Rectangle to define coordinates for toolbar */
                        "Naviger",                          /* Corresponding menu label - no menu if blank */
                        "Last;Siste varebok&,Next;Neste varebok,Prev;Forrige varebok,First;Første varebok",
                                                        /* Buttons / Menu items: action;label;tooltip;Method;image,action;label.. 
                                                           Any number of properties accepted (one ok - if predef. action) */
                        "right").                       /* Misc - for something I might need in next version.. */

      DYNAMIC-FUNCTION("CreateObjectLink",hNavVarebokToolBar,hBrowseListe).
      DYNAMIC-FUNCTION("CreateObjectLink",hNavVarebokToolBar,hFieldMap).
      DYNAMIC-FUNCTION("setToolbar",hNavVarebokToolBar,"enable").
      DYNAMIC-FUNCTION("setAddMoveX",THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, 
                                    "rectNavVarebok," +
                                    DYNAMIC-FUNCTION("getToolbarNames",hNavVarebokToolBar,"")    
                                    ).
    END.

    ASSIGN hBrwOVarekost:HIDDEN        = TRUE
           hBrwOsupVarekost:HIDDEN     = TRUE
           hBrwOPris:HIDDEN            = TRUE       
           hBrwOKampanjePris:HIDDEN    = TRUE       
           hBrwOLMerk:HIDDEN    = TRUE
           .

/*     IF bHKinst THEN  */
      ASSIGN 
             hBrwOKjedeRab%:HIDDEN     = TRUE  
             hBrwOKjedeInnkPris:HIDDEN = TRUE
             hBrwOKjedeSupRab%:HIDDEN     = TRUE  
             hBrwOKjedeSupInnkPris:HIDDEN = TRUE
             hBrwOAntall:HIDDEN        = TRUE     
             hBrwOSupAntall:HIDDEN     = TRUE  
             .

    IF VALID-HANDLE(hRowsToBatchMenu) THEN
      hRowsToBatchMenu:SENSITIVE = TRUE.

  END.

  IF iiTab = 3 THEN DO:      
    DYNAMIC-FUNCTION("setCurrentObject",hBrowseLinje).
    RUN OpenQuery.
  END.
  ELSE 
    DYNAMIC-FUNCTION("setCurrentObject",hBrowseListe).
  RUN DisplayRecord.

  RETURN TRUE.
END.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION UtvalgIsMaster C-Win 
FUNCTION UtvalgIsMaster RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN bUtvalgIsMaster.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ViewStat C-Win 
FUNCTION ViewStat RETURNS LOGICAL
  ( INPUT ibView AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cStatFieldValues  AS CHAR NO-UNDO.
DEF VAR hField            AS HANDLE NO-UNDO.
DEF VAR hField_avg        AS HANDLE NO-UNDO.
DEF VAR hField_tot        AS HANDLE NO-UNDO.
DEF VAR iCount            AS INT NO-UNDO.

IF ibView THEN DO WITH FRAME frmLinje:
  cStatFieldValues = DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"querystatfieldvalues").
  
  DO ix = 1 TO NUM-ENTRIES(cStatFieldValues,";"):
    IF ENTRY(1,ENTRY(ix,cStatFieldValues,";"),"|") = "rowcount" THEN
      ASSIGN iCount            = INT(ENTRY(2,ENTRY(ix,cStatFieldValues,";"),"|"))
             hBrowseLinje:HELP = ENTRY(2,ENTRY(ix,cStatFieldValues,";"),"|")
             .
    IF CAN-DO(cPrefixedStatFields,"tot_" + ENTRY(1,ENTRY(ix,cStatFieldValues,";"),"|")) THEN 
      ASSIGN hField_tot = WIDGET-HANDLE(ENTRY(LOOKUP("tot_" + ENTRY(1,ENTRY(ix,cStatFieldValues,";"),"|"),cPrefixedStatFields),cStatFieldHandles))
             hField_tot:SCREEN-VALUE = ENTRY(2,ENTRY(ix,cStatFieldValues,";"),"|")
             .
    IF CAN-DO(cPrefixedStatFields,"avg_" + ENTRY(1,ENTRY(ix,cStatFieldValues,";"),"|")) THEN
      ASSIGN hField_avg = WIDGET-HANDLE(ENTRY(LOOKUP("avg_" + ENTRY(1,ENTRY(ix,cStatFieldValues,";"),"|"),cPrefixedStatFields),cStatFieldHandles))
             hField_avg:SCREEN-VALUE = STRING(DEC(ENTRY(2,ENTRY(ix,cStatFieldValues,";"),"|")) / (IF NOT tbForventet:CHECKED THEN iCount ELSE 1))
             .
    IF NOT CAN-DO(cPrefixedStatFields,"avg_" + ENTRY(1,ENTRY(ix,cStatFieldValues,";"),"|")) AND 
       NOT CAN-DO(cPrefixedStatFields,"tot_" + ENTRY(1,ENTRY(ix,cStatFieldValues,";"),"|")) AND
       CAN-DO(cStatFieldNames,ENTRY(1,ENTRY(ix,cStatFieldValues,";"),"|")) THEN 
      ASSIGN hField = WIDGET-HANDLE(ENTRY(LOOKUP(ENTRY(1,ENTRY(ix,cStatFieldValues,";"),"|"),cStatFieldNames),cStatFieldHandles))
             hField:SCREEN-VALUE = ENTRY(2,ENTRY(ix,cStatFieldValues,";"),"|")
             .
  END.
  IF tbForventet:CHECKED THEN DO ix = 1 TO NUM-ENTRIES(cStatFieldValues,";"):
    IF CAN-DO(cPrefixedStatFields,"avg_" + ENTRY(1,ENTRY(ix,cStatFieldValues,";"),"|")) THEN DO:
      hField_avg = WIDGET-HANDLE(ENTRY(LOOKUP("avg_" + ENTRY(1,ENTRY(ix,cStatFieldValues,";"),"|"),cPrefixedStatFields),cStatFieldHandles)).
      IF CAN-DO("avg_VareKost,avg_DBkr",hField_avg:NAME) THEN
        hField_avg:SCREEN-VALUE = STRING(DEC(ENTRY(2,ENTRY(ix,cStatFieldValues,";"),"|")) / DEC(tot_Antall:SCREEN-VALUE)).
      ELSE IF CAN-DO("avg_supVareKost,avg_supDBkr",hField_avg:NAME) THEN
        hField_avg:SCREEN-VALUE = STRING(DEC(ENTRY(2,ENTRY(ix,cStatFieldValues,";"),"|")) / DEC(tot_supAntall:SCREEN-VALUE)).
      ELSE IF CAN-DO("avg_Pris",hField_avg:NAME) THEN
        hField_avg:SCREEN-VALUE = STRING(DEC(ENTRY(2,ENTRY(ix,cStatFieldValues,";"),"|")) / (DEC(tot_supAntall:SCREEN-VALUE) + DEC(tot_supAntall:SCREEN-VALUE))).
    END.
  END.

END.
ELSE 
  DO ix = 1 TO NUM-ENTRIES(cStatFieldHandles):
    ASSIGN hField = WIDGET-HANDLE(ENTRY(ix,cStatFieldHandles))
           hField:SCREEN-VALUE = "".
  END.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

