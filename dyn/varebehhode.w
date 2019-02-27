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
{incl/ean13bc.i}
{incl/Excel_1.3.i}

DEF VAR bOK                    AS LOG    NO-UNDO.
DEF VAR ix                     AS INT    NO-UNDO.
DEF VAR iReturn                AS INT    NO-UNDO.
DEF VAR bHKinst                AS LOG    NO-UNDO.
DEF VAR bAlleTrans             AS LOG    NO-UNDO.
DEF VAR cLevNrListe            AS CHAR   NO-UNDO.

DEF VAR iStartWeek             AS INT    NO-UNDO.
DEF VAR iEndWeek               AS INT    NO-UNDO.
                              
DEF VAR hToolbar               AS HANDLE NO-UNDO.
DEF VAR hUpdToolbar            AS HANDLE NO-UNDO.
DEF VAR hLinjeTransToolbar     AS HANDLE NO-UNDO.
DEF VAR hRegStatTransToolbar   AS HANDLE NO-UNDO.
DEF VAR hBrowseListe           AS HANDLE NO-UNDO.
DEF VAR hBrowseLinje           AS HANDLE NO-UNDO.
DEF VAR hBrowseTrans           AS HANDLE NO-UNDO.
DEF VAR hSumTransBuffer        AS HANDLE NO-UNDO.
DEF VAR hSumTransQuery         AS HANDLE NO-UNDO.
DEF VAR hBrowseRegStat         AS HANDLE NO-UNDO.
DEF VAR hBrowseRegStatTrans    AS HANDLE NO-UNDO.
DEF VAR hFieldMap              AS HANDLE NO-UNDO.
DEF VAR hFieldMapLinje         AS HANDLE NO-UNDO.
DEF VAR hFieldMapTrans         AS HANDLE NO-UNDO.
DEF VAR hFieldMapRegStat       AS HANDLE NO-UNDO.
DEF VAR hFieldMapRegStatTrans  AS HANDLE NO-UNDO.
DEF VAR hBuffLinje             AS HANDLE NO-UNDO.
DEF VAR hSearchListe           AS HANDLE NO-UNDO.
DEF VAR hSearchLinje           AS HANDLE NO-UNDO.
DEF VAR hWindow                AS HANDLE NO-UNDO.
DEF VAR hBrwTransBestilt1      AS HANDLE NO-UNDO.
DEF VAR hBrwTransBestilt2      AS HANDLE NO-UNDO.
DEF VAR hBrwTransBestilt3      AS HANDLE NO-UNDO.
DEF VAR hBrwTransBestilt4      AS HANDLE NO-UNDO.
DEF VAR hColumn                AS HANDLE NO-UNDO.
DEF VAR hNavVarebehToolBar     AS HANDLE NO-UNDO.
DEF VAR hArtBilde              AS HANDLE NO-UNDO.
DEF VAR hArtBildeFrame         AS HANDLE NO-UNDO.
DEF VAR hRowsToBatchMenu       AS HANDLE NO-UNDO.
DEF VAR hQueryCountMenu        AS HANDLE NO-UNDO.
DEF VAR hTotalMenu             AS HANDLE NO-UNDO.

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
DEF VAR cButikerRowIdList      AS CHAR   NO-UNDO.
DEF VAR cButikerIdList         AS CHAR   NO-UNDO.
DEF VAR cKode                  AS CHAR   NO-UNDO.
DEF VAR cTmpKode               AS CHAR   NO-UNDO.
DEF VAR cTekst                 AS CHAR   NO-UNDO.
DEF VAR cVPILevKortNavnLevNr   AS CHAR   NO-UNDO.
DEF VAR hMenuItemFlatView      AS HANDLE NO-UNDO.
DEF VAR hButtonFlatView        AS HANDLE NO-UNDO.

DEF VAR hCurrLevUke            AS HANDLE NO-UNDO.

DEF VAR cAdgang                AS CHAR   NO-UNDO.
DEF VAR hTLdet                 AS HANDLE NO-UNDO.
DEF VAR hVisBilde              AS HANDLE NO-UNDO.
DEF VAR hArtikkelkort          AS HANDLE NO-UNDO.
DEF VAR hBufArtStr             AS HANDLE NO-UNDO.
DEF VAR bStartSearch           AS LOG    NO-UNDO.
DEF VAR cHelp                  AS CHAR   NO-UNDO.
DEF VAR wArtBasRecid           AS RECID  NO-UNDO.
                              
DEF VAR iTab                   AS INT    NO-UNDO.
                              
DEF VAR cState                 AS CHAR   NO-UNDO.
DEF VAR cVarebehRowIdList      AS CHAR   NO-UNDO.
DEF VAR cVarebehIdList         AS CHAR   NO-UNDO.
                              
DEF VAR fArtikkelNr            AS DEC    NO-UNDO.
DEF VAR rRowIdLinje            AS ROWID  NO-UNDO.
DEF VAR cVarebehLinjeJoin      AS CHAR   NO-UNDO.
DEF VAR cAktivitetJoin         AS CHAR   NO-UNDO.
DEF VAR cVarebehLinjeThodeJoin AS CHAR   NO-UNDO.
DEF VAR cVarebehLinjeTransJoin AS CHAR   NO-UNDO.

DEF VAR cModellFarge           AS CHAR   NO-UNDO.
DEF VAR cModellArtList         AS CHAR   NO-UNDO.
DEF VAR iModIdx                AS INT    NO-UNDO.

DEF VAR cButikkListe           AS CHAR   NO-UNDO INIT "*".
DEF VAR cCurrWeekList          AS CHAR   NO-UNDO.
DEF VAR hSekv                  AS HANDLE NO-UNDO.
DEF VAR hBestilt               AS HANDLE NO-UNDO.
DEF VAR hGodkjent              AS HANDLE NO-UNDO.
DEF VAR hBeskr                 AS HANDLE NO-UNDO.
DEF VAR hFieldRGBcolor         AS HANDLE NO-UNDO.
DEF VAR hBestVerdi             AS HANDLE NO-UNDO.
DEF VAR hLinjeMerknad          AS HANDLE NO-UNDO.
DEF VAR iColNumBeskr           AS INT    NO-UNDO.
DEF VAR iColNumBestilt         AS INT    NO-UNDO.
DEF VAR iColNumBestiltGodk     AS INT    NO-UNDO.
DEF VAR iColNumBestiltForsl    AS INT    NO-UNDO.
DEF VAR iBestVarsel            AS INT  INITIAL 99  NO-UNDO.
DEF VAR bWarning               AS LOG    NO-UNDO.
DEF VAR fCurrArtNr             AS DEC    NO-UNDO.
DEF VAR hBrwLinjeGodkjent      AS HANDLE NO-UNDO.
DEF VAR hBrwLinjeTransStorl    AS HANDLE NO-UNDO.
DEF VAR hBrwLinjeTransButikknr AS HANDLE NO-UNDO.
DEF VAR hBrwLinjeTransArtikkelnr AS HANDLE NO-UNDO.
DEF VAR hTransGodkjent         AS HANDLE NO-UNDO.
DEF VAR hBrwLinjeTransUker     AS HANDLE NO-UNDO EXTENT 8.
DEF VAR hRegLevUke             AS HANDLE NO-UNDO EXTENT 4.
DEF VAR iGrayColor             AS INT    NO-UNDO.
DEF VAR hGodkjentCol           AS HANDLE NO-UNDO.
DEF VAR iFontGodkjent          AS INT    NO-UNDO.
DEF VAR hBrwColKjedeVare       AS HANDLE NO-UNDO.
DEF VAR hBrwColGjFakt          AS HANDLE NO-UNDO.

DEF VAR cPrintMark             AS CHAR   NO-UNDO.
DEF VAR bArtVedl               AS LOG    NO-UNDO.
DEF VAR cInitPrisProfil        AS CHAR   NO-UNDO.
DEF VAR cCL                    AS CHAR   NO-UNDO.
DEF VAR cBtnUtvalgHandles      AS CHAR   NO-UNDO.
DEF VAR cBtnHentUtvalgHandles  AS CHAR   NO-UNDO.
DEF VAR hUtvalg                AS HANDLE NO-UNDO.
DEF VAR bUtvalgIsMaster        AS LOG    NO-UNDO.

DEF VAR hSpesialSortMnu        AS HANDLE NO-UNDO.
DEF VAR hLevBekrUtvalgPanel    AS HANDLE NO-UNDO.
DEF VAR hButBekrUtvalgPanel    AS HANDLE NO-UNDO.
DEF VAR cLevBekrCat            AS CHAR   NO-UNDO.
DEF VAR cLevBekrUtvalg         AS CHAR   NO-UNDO.
DEF VAR cButBekrCat            AS CHAR   NO-UNDO.
DEF VAR bFormatReport          AS LOG    NO-UNDO.

DEFINE STREAM Stream1.

DEF TEMP-TABLE ttBildeData
    FIELD BildNr    AS INT
    FIELD Teller    AS INT
    FIELD RawData   AS RAW
    FIELD RowIdent  AS CHAR
    .
DEF VAR hBufBildeData AS HANDLE.
hBufBildeData = BUFFER ttBildeData:HANDLE.

DEF TEMP-TABLE ttVarebehLinjeTrans
    FIELD VarebehNr    AS DEC
    FIELD ArtikkelNr   AS DEC
    FIELD ButikkNr     AS INT
    FIELD kode         AS CHAR.
DEF VAR httVarebehLinjeTrans AS HANDLE NO-UNDO.
httVarebehLinjeTrans = BUFFER ttVarebehLinjeTrans:HANDLE:TABLE-HANDLE.

DEF TEMP-TABLE ttRapport NO-UNDO
    FIELD cRpLinje AS CHAR.
DEF VAR httRapport AS HANDLE NO-UNDO.
httRapport = BUFFER ttRapport:HANDLE:TABLE-HANDLE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rectToolBar rectUpdToolbar rectVarebeh ~
VarebehNr VarebehBeskrivelse BeskrivelseVarebehType MesseNr ~
MesseBeskrivelse KortNavn 
&Scoped-Define DISPLAYED-OBJECTS VarebehNr VarebehBeskrivelse VarebehType ~
BeskrivelseVarebehType MesseNr MesseBeskrivelse ProfilNr KortNavn 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ApplyFocusToBestilt1 C-Win 
FUNCTION ApplyFocusToBestilt1 RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ClearFilter C-Win 
FUNCTION ClearFilter RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD EndreMangeTranser C-Win 
FUNCTION EndreMangeTranser RETURNS LOGICAL
  ( INPUT icField AS CHAR,
    INPUT icValue AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FinnLevUker C-Win 
FUNCTION FinnLevUker RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getArtNr C-Win 
FUNCTION getArtNr RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFromNotat C-Win 
FUNCTION getFromNotat RETURNS CHARACTER
  ( INPUT icMerke AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getLevNr C-Win 
FUNCTION getLevNr RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getReportFile C-Win 
FUNCTION getReportFile RETURNS CHARACTER
  ( INPUT icClientFileName AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getVarebehNr C-Win 
FUNCTION getVarebehNr RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD InitUtvalgToVarebeh C-Win 
FUNCTION InitUtvalgToVarebeh RETURNS LOGICAL
  ( INPUT ihUtvalg         AS HANDLE,
    INPUT ibUtvalgIsMaster AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LastInnstillinger C-Win 
FUNCTION LastInnstillinger RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LevForecastToExcel C-Win 
FUNCTION LevForecastToExcel RETURNS LOGICAL
  ( INPUT icFileAndCount AS CHAR,
    INPUT icType         AS CHAR,
    INPUT icLevNr        AS CHAR,
    INPUT icTargetFile   AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LevOrdreBekrToExcel C-Win 
FUNCTION LevOrdreBekrToExcel RETURNS LOGICAL
  ( INPUT icFileAndCount AS CHAR,
    INPUT icType         AS CHAR,
    INPUT icLevNr        AS CHAR,
    INPUT icTargetFile   AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LinkOverlays C-Win 
FUNCTION LinkOverlays RETURNS LOGICAL
   (INPUT ibView AS LOG) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD OrdreBekreftToExcel C-Win 
FUNCTION OrdreBekreftToExcel RETURNS LOGICAL
  ( INPUT icFileAndCount AS CHAR,
    INPUT icType         AS CHAR,
    INPUT icButLev       AS CHAR,
    INPUT ibEAN          AS LOG,
    INPUT icSortBy       AS CHAR,
    INPUT icTargetFile   AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setButikkListe C-Win 
FUNCTION setButikkListe RETURNS LOGICAL
  (  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setFieldColor C-Win 
FUNCTION setFieldColor RETURNS INTEGER
  ( INPUT iRGBcolor AS INT)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setLevUke C-Win 
FUNCTION setLevUke RETURNS LOGICAL
  ( INPUT ihLevUke AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setNotat C-Win 
FUNCTION setNotat RETURNS LOGICAL
  ( INPUT icMerke AS CHAR,
    INPUT icVerdi AS CHAR )  FORWARD.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setTema C-Win 
FUNCTION setTema RETURNS LOGICAL
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD startSpesialSort C-Win 
FUNCTION startSpesialSort RETURNS LOGICAL
  ( INPUT icSortDescr AS CHAR,
    INPUT ibOpenQuery AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SumAntOgVerdi C-Win 
FUNCTION SumAntOgVerdi RETURNS LOGICAL
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ViewQueryStat C-Win 
FUNCTION ViewQueryStat RETURNS LOGICAL
  ( INPUT ihBrowse AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE MENU POPUP-MENU-btnLev 
       MENU-ITEM m_Vis_alle_leverandrer LABEL "Vis alle leverandører".


/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE TabStrip AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chTabStrip AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE BeskrivelseVarebehType AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 14.4 BY 1.

DEFINE VARIABLE KortNavn AS CHARACTER FORMAT "X(15)" 
     VIEW-AS FILL-IN 
     SIZE 16.2 BY 1.

DEFINE VARIABLE MesseBeskrivelse AS CHARACTER FORMAT "X(40)" 
     VIEW-AS FILL-IN 
     SIZE 23.6 BY 1.

DEFINE VARIABLE MesseNr AS DECIMAL FORMAT ">>>>>>>9" INITIAL 0 
     LABEL "Messe" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE ProfilNr AS INTEGER FORMAT "zz9" INITIAL 0 
     LABEL "Prisprofil" 
     VIEW-AS FILL-IN 
     SIZE 6.2 BY 1.

DEFINE VARIABLE VarebehBeskrivelse AS CHARACTER FORMAT "X(40)" 
     VIEW-AS FILL-IN 
     SIZE 31 BY 1.

DEFINE VARIABLE VarebehNr AS DECIMAL FORMAT ">>>>>>>>>>>>9" INITIAL 0 
     LABEL "Vareh." 
     VIEW-AS FILL-IN 
     SIZE 16.2 BY 1.

DEFINE VARIABLE VarebehType AS INTEGER FORMAT ">>9" INITIAL 0 
     LABEL "Type" 
     VIEW-AS FILL-IN 
     SIZE 4.2 BY 1.

DEFINE RECTANGLE rectNavVarebeh
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 31.8 BY 1.19.

DEFINE RECTANGLE rectToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 9.2 BY 1.19.

DEFINE RECTANGLE rectUpdToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 16.6 BY 1.19.

DEFINE RECTANGLE rectVarebeh
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 200 BY 1.52.

DEFINE BUTTON btnSokMesseNr  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnSokProfilNr  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE VarebehNotat AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 161 BY 10.24 NO-UNDO.

DEFINE VARIABLE Kilde AS DECIMAL FORMAT ">>>>>>>>>>>>9" INITIAL 0 
     LABEL "Fra varebok" 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1.

DEFINE VARIABLE OppdatAv AS CHARACTER FORMAT "X(15)" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE OppdatDato AS DATE FORMAT "99/99/99" 
     LABEL "Dato/BrukerId" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE VareBokBeskrivelse AS CHARACTER FORMAT "X(40)" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1.

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

DEFINE VARIABLE sokButikk AS CHARACTER FORMAT "X(256)":U 
     LABEL "Butikk" 
     VIEW-AS COMBO-BOX INNER-LINES 40
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 45 BY 1 NO-UNDO.

DEFINE VARIABLE fi-cVarebehBeskrivelse AS CHARACTER FORMAT "X(40)" 
     LABEL "Beskrivelse" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1.

DEFINE VARIABLE fi-fMesseNr AS DECIMAL FORMAT ">>>>>>>9" INITIAL 0 
     LABEL "MesseNr" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE fi-fProfilNr AS INTEGER FORMAT "zz9" INITIAL 0 
     LABEL "Prisprofil" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE rsSokGodkStatus AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Alle", 1,
"Kun godkjente", 2,
"Kun forslag", 3
     SIZE 40 BY .71 NO-UNDO.

DEFINE RECTANGLE RectArtSok
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 54.2 BY 3.29.

DEFINE BUTTON btnAvdeling 
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnBlankLinjeFilter 
     LABEL "Blank filter" 
     SIZE 15 BY 1.1.

DEFINE BUTTON btnButTotal 
     LABEL "Oppdater total for butikk:" 
     SIZE 24.6 BY 1.14 TOOLTIP "Sum antall bestilte varer * netto innkjøpspris (varekost)".

DEFINE BUTTON btnCalFraDato 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 3.8 BY .81.

DEFINE BUTTON btnCalLevUke1 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 3.8 BY .81.

DEFINE BUTTON btnCalLevUke2 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 3.8 BY .81.

DEFINE BUTTON btnCalLevUke3 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 3.8 BY .81.

DEFINE BUTTON btnCalLevUke4 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 3.8 BY .81.

DEFINE BUTTON btnCalTilDato 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 3.8 BY .81.

DEFINE BUTTON btnHuvGr 
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnLev 
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnLev-2 
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnSplitBarY 
     IMAGE-UP FILE "bmp/tabup.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Button 1" 
     SIZE 198 BY .48.

DEFINE BUTTON btnTema 
     LABEL "Rediger tema" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnVarGr 
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnVarGr-2 
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE ButikkNr AS CHARACTER 
     LABEL "Butikk" 
     VIEW-AS COMBO-BOX INNER-LINES 20
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN AUTO-COMPLETION
     SIZE 36 BY 1 TOOLTIP "Trykk ENTER for å skifte butikk" NO-UNDO.

DEFINE VARIABLE cmbTema AS CHARACTER FORMAT "X(256)":U 
     LABEL "Tema" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 41 BY 1 TOOLTIP "Valg av tema forutsetter at en leverandør er valgt" NO-UNDO.

DEFINE VARIABLE LevUke1 AS CHARACTER 
     LABEL "Levuke1" 
     VIEW-AS COMBO-BOX INNER-LINES 35
     LIST-ITEMS "Item 1" 
     DROP-DOWN AUTO-COMPLETION UNIQUE-MATCH
     SIZE 11.2 BY 1 NO-UNDO.

DEFINE VARIABLE LevUke2 AS CHARACTER 
     LABEL "Levuke2" 
     VIEW-AS COMBO-BOX INNER-LINES 35
     LIST-ITEMS "Item 1" 
     DROP-DOWN AUTO-COMPLETION UNIQUE-MATCH
     SIZE 11.2 BY 1 NO-UNDO.

DEFINE VARIABLE LevUke3 AS CHARACTER 
     LABEL "Levuke3" 
     VIEW-AS COMBO-BOX INNER-LINES 35
     LIST-ITEMS "Item 1" 
     DROP-DOWN AUTO-COMPLETION UNIQUE-MATCH
     SIZE 11.2 BY 1 NO-UNDO.

DEFINE VARIABLE LevUke4 AS CHARACTER 
     LABEL "Levuke4" 
     VIEW-AS COMBO-BOX INNER-LINES 35
     LIST-ITEMS "Item 1" 
     DROP-DOWN AUTO-COMPLETION UNIQUE-MATCH
     SIZE 11.2 BY 1 NO-UNDO.

DEFINE VARIABLE stdLevUke1 AS CHARACTER 
     LABEL "Std.uker" 
     VIEW-AS COMBO-BOX INNER-LINES 35
     LIST-ITEMS "Item 1" 
     DROP-DOWN AUTO-COMPLETION UNIQUE-MATCH
     SIZE 10.6 BY 1 NO-UNDO.

DEFINE VARIABLE stdLevUke2 AS CHARACTER 
     VIEW-AS COMBO-BOX INNER-LINES 35
     LIST-ITEMS "Item 1" 
     DROP-DOWN AUTO-COMPLETION UNIQUE-MATCH
     SIZE 10.4 BY 1 NO-UNDO.

DEFINE VARIABLE stdLevUke3 AS CHARACTER 
     VIEW-AS COMBO-BOX INNER-LINES 35
     LIST-ITEMS "Item 1" 
     DROP-DOWN AUTO-COMPLETION UNIQUE-MATCH
     SIZE 10.8 BY 1 NO-UNDO.

DEFINE VARIABLE stdLevUke4 AS CHARACTER 
     VIEW-AS COMBO-BOX INNER-LINES 35
     LIST-ITEMS "Item 1" 
     DROP-DOWN AUTO-COMPLETION UNIQUE-MATCH
     SIZE 10.4 BY 1 NO-UNDO.

DEFINE VARIABLE fiButTotal AS DECIMAL FORMAT "->,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16.4 BY 1 NO-UNDO.

DEFINE VARIABLE fiSumAnt AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Sum ant" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE fiSumVerdi AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Verdi" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiTimeTot AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE Kode AS CHARACTER FORMAT "X(20)" 
     LABEL "Strekkode" 
     VIEW-AS FILL-IN 
     SIZE 20.2 BY 1.

DEFINE VARIABLE KodeX AS CHARACTER FORMAT "X(20)" 
     LABEL "Strekkode(hidden)" 
     VIEW-AS FILL-IN 
     SIZE 23.2 BY 1.

DEFINE VARIABLE sokAktBeskrivelse AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE sokAktNr AS INTEGER FORMAT "zzzzz9" INITIAL 0 
     LABEL "Aktivitet" 
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1.

DEFINE VARIABLE sokArtikkelNr AS DECIMAL FORMAT "zzzzzzzzzzzz9" INITIAL 0 
     LABEL "S&E Art.nr" 
     VIEW-AS FILL-IN 
     SIZE 20.4 BY 1 TOOLTIP "ALT-E".

DEFINE VARIABLE sokAvdelingNavn AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE sokAvdelingNr AS INTEGER FORMAT "zzzzz9" INITIAL 0 
     LABEL "Avdeling" 
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1.

DEFINE VARIABLE sokBeskr AS CHARACTER FORMAT "X(256)":U 
     LABEL "Utv.søk" 
     VIEW-AS FILL-IN 
     SIZE 35 BY 1
     BGCOLOR 16  NO-UNDO.

DEFINE VARIABLE sokBestVerdi AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Verdi >" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE sokFraEdato AS DATE FORMAT "99/99/99":U 
     LABEL "Endret dato" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE sokFraVPIdato AS DATE FORMAT "99/99/99":U 
     LABEL "VPI dato (hidden)" 
     VIEW-AS FILL-IN 
     SIZE 14.6 BY 1 NO-UNDO.

DEFINE VARIABLE sokHg AS INTEGER FORMAT "zzzzz9" INITIAL 0 
     LABEL "Hovedgr" 
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1.

DEFINE VARIABLE sokHgBeskr AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE sokLevKod AS CHARACTER FORMAT "x(30)" 
     LABEL "LevArtNr" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.

DEFINE VARIABLE sokLevNamn AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 24.2 BY 1 NO-UNDO.

DEFINE VARIABLE sokLevNr AS INTEGER FORMAT "zzzzz9" INITIAL 0 
     LABEL "Lev" 
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1.

DEFINE VARIABLE sokLinjeMerknad AS CHARACTER FORMAT "X(256)":U 
     LABEL "Merknad" 
     VIEW-AS FILL-IN 
     SIZE 35 BY 1 NO-UNDO.

DEFINE VARIABLE sokTilEdato AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE sokTilVPIdato AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 14.6 BY 1 NO-UNDO.

DEFINE VARIABLE sokVg AS INTEGER FORMAT "zzzzz9" INITIAL 0 
     LABEL "Varegr" 
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1.

DEFINE VARIABLE sokVgBeskr AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE rsBestilling AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Alle", 0,
"Med bestilling", 1,
"Uten", 2
     SIZE 34 BY .95 NO-UNDO.

DEFINE VARIABLE rsGodkjent AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Alle", 0,
"Godkjente", 1,
"Forslag", 2
     SIZE 34 BY .95 NO-UNDO.

DEFINE RECTANGLE ArtikkelBilde
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 28 BY 5.48.

DEFINE RECTANGLE rectBrowseLinje
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 198 BY 10.95.

DEFINE RECTANGLE RectBrowseSearchLinje
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 20.2 BY 1.19.

DEFINE RECTANGLE rectBrowseTrans
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 197.6 BY 9.52.

DEFINE RECTANGLE rectButTotal
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 147 BY 1.43.

DEFINE RECTANGLE rectTBvarebehlinjetrans
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 6 BY .91.

DEFINE VARIABLE tbGjennomfaktureres AS LOGICAL INITIAL no 
     LABEL "Kun gj.fakt" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.8 BY .81 NO-UNDO.

DEFINE VARIABLE tbKjedeVare AS LOGICAL INITIAL no 
     LABEL "Kun kjedev." 
     VIEW-AS TOGGLE-BOX
     SIZE 14.8 BY .81 NO-UNDO.

DEFINE RECTANGLE rectBrowseListe
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 198 BY 22.86.

DEFINE RECTANGLE RectBrowseSearchListe
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 22 BY 1.19.

DEFINE BUTTON btnAlleBut 
     LABEL "Registreringer til Excel" 
     SIZE 23 BY 1.14 TOOLTIP "Eksporter alle registreringer uavhengig av butikk for vareboken".

DEFINE BUTTON btnAlleButSam 
     LABEL "Sammendrag til Excel" 
     SIZE 23 BY 1.14 TOOLTIP "Eksporter alle registreringer uavhengig av butikk for vareboken".

DEFINE BUTTON btnButnr 
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE fiTotSumOrdre AS DECIMAL FORMAT "->>,>>>,>>9.99":U INITIAL 0 
     LABEL "Total ordresum" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE fiUtskrNr AS CHARACTER FORMAT "X(256)":U 
     LABEL "Søk utskriftsnr for butikken" 
     VIEW-AS FILL-IN 
     SIZE 8.6 BY 1 TOOLTIP "Blank for alle" NO-UNDO.

DEFINE VARIABLE sokButikkNr AS INTEGER FORMAT "zzzzz9" INITIAL 0 
     LABEL "Butikk" 
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1.

DEFINE VARIABLE sokButNamn AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE rsAlleReg AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Valgt butikk", 1,
"Alle butikker", 2
     SIZE 30.6 BY .95 NO-UNDO.

DEFINE VARIABLE rsForslagGodk AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Alle registreringer", 1,
"Kun forslag", 2,
"Kun godkjente", 3
     SIZE 52.4 BY .95
     BGCOLOR 8  NO-UNDO.

DEFINE RECTANGLE rectRegStat
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 196.2 BY 9.71.

DEFINE RECTANGLE rectRegStatTrans
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 196 BY 13.43.

DEFINE RECTANGLE rectRegStatTransToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 9.8 BY 1.19.

DEFINE VARIABLE tbVisLevuker AS LOGICAL INITIAL no 
     LABEL "Vis lev.uker" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     VarebehNr AT ROW 2.81 COL 7.6 COLON-ALIGNED HELP
          "Varebehnummer"
     VarebehBeskrivelse AT ROW 2.81 COL 24 COLON-ALIGNED HELP
          "Kort beskrivelse av Varebehen" NO-LABEL
     VarebehType AT ROW 2.81 COL 61.2 COLON-ALIGNED HELP
          "Varebehtype"
     BeskrivelseVarebehType AT ROW 2.81 COL 65.6 COLON-ALIGNED HELP
          "Kort beskrivelse av Varebehtypen" NO-LABEL
     MesseNr AT ROW 2.81 COL 87.8 COLON-ALIGNED HELP
          "Messenummer"
     MesseBeskrivelse AT ROW 2.81 COL 101.2 COLON-ALIGNED HELP
          "Navn eller kort beskrivelse av messen." NO-LABEL
     ProfilNr AT ROW 2.81 COL 134 COLON-ALIGNED HELP
          "Prisprofil"
     KortNavn AT ROW 2.81 COL 140.4 COLON-ALIGNED HELP
          "Kort betegnelse på prisprofilen" NO-LABEL
     rectToolBar AT ROW 1.14 COL 191
     rectUpdToolbar AT ROW 1.14 COL 1.4
     rectVarebeh AT ROW 2.57 COL 1
     rectNavVarebeh AT ROW 1.14 COL 159
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 200.2 BY 32.91.

DEFINE FRAME frmLinje
     tbKjedeVare AT ROW 2.33 COL 93.4
     sokBestVerdi AT ROW 3.24 COL 153 COLON-ALIGNED
     sokAvdelingNr AT ROW 1.1 COL 9 COLON-ALIGNED HELP
          "Varegruppe"
     sokAvdelingNavn AT ROW 1.1 COL 19.8 COLON-ALIGNED NO-LABEL
     btnSplitBarY AT ROW 17.91 COL 1
     sokHg AT ROW 2.14 COL 9 COLON-ALIGNED HELP
          "Varegruppe"
     sokHgBeskr AT ROW 2.14 COL 19.8 COLON-ALIGNED NO-LABEL
     sokVg AT ROW 3.19 COL 9 COLON-ALIGNED HELP
          "Varegruppe"
     sokVgBeskr AT ROW 3.19 COL 19.8 COLON-ALIGNED NO-LABEL
     sokAktNr AT ROW 4.24 COL 9 COLON-ALIGNED HELP
          "Varegruppe"
     sokAktBeskrivelse AT ROW 4.24 COL 19.8 COLON-ALIGNED NO-LABEL
     btnButTotal AT ROW 5.62 COL 119 NO-TAB-STOP 
     sokLevNr AT ROW 1.1 COL 54.6 COLON-ALIGNED HELP
          "Varegruppe"
     sokLevNamn AT ROW 1.1 COL 65.4 COLON-ALIGNED NO-LABEL
     sokBeskr AT ROW 2.19 COL 54.6 COLON-ALIGNED
     sokLevKod AT ROW 3.24 COL 54.6 COLON-ALIGNED HELP
          "Leverandørens artikkelnummer - bestillingsnummer"
     sokLinjeMerknad AT ROW 4.33 COL 54.6 COLON-ALIGNED
     cmbTema AT ROW 1.1 COL 111 COLON-ALIGNED
     rsBestilling AT ROW 2.29 COL 112.4 NO-LABEL
     rsGodkjent AT ROW 3.29 COL 112.4 NO-LABEL
     sokFraEdato AT ROW 4.33 COL 115 COLON-ALIGNED
     sokTilEdato AT ROW 4.33 COL 133.4 COLON-ALIGNED NO-LABEL
     sokFraVPIdato AT ROW 11.71 COL 164 COLON-ALIGNED
     sokTilVPIdato AT ROW 12.91 COL 164 COLON-ALIGNED NO-LABEL
     fiButTotal AT ROW 5.67 COL 141.8 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     btnTema AT ROW 1.05 COL 154.6
     btnBlankLinjeFilter AT ROW 4.33 COL 154.8
     ButikkNr AT ROW 5.71 COL 28.6 COLON-ALIGNED HELP
          "Trykk ENTER for å skifte butikk"
     stdLevUke1 AT ROW 5.71 COL 103 COLON-ALIGNED
     stdLevUke2 AT ROW 5.71 COL 102.4 COLON-ALIGNED NO-LABEL
     stdLevUke3 AT ROW 5.71 COL 102.2 COLON-ALIGNED NO-LABEL
     stdLevUke4 AT ROW 5.71 COL 106.4 COLON-ALIGNED NO-LABEL
     sokArtikkelNr AT ROW 18.48 COL 9.6 COLON-ALIGNED HELP
          "PRS artikkelnr"
     fiTimeTot AT ROW 5.67 COL 158.4 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     LevUke1 AT ROW 18.48 COL 41.2 COLON-ALIGNED
     LevUke2 AT ROW 18.48 COL 66.8 COLON-ALIGNED
     LevUke3 AT ROW 18.48 COL 92.8 COLON-ALIGNED
     LevUke4 AT ROW 18.48 COL 119 COLON-ALIGNED
     KodeX AT ROW 8.14 COL 162 COLON-ALIGNED HELP
          "Strekkode inklusive sjekksiffer."
     fiSumAnt AT ROW 18.48 COL 159.2 COLON-ALIGNED NO-TAB-STOP 
     btnAvdeling AT ROW 1.1 COL 42 NO-TAB-STOP 
     btnHuvGr AT ROW 2.14 COL 42 NO-TAB-STOP 
     btnVarGr AT ROW 3.19 COL 42 NO-TAB-STOP 
     btnVarGr-2 AT ROW 4.24 COL 42 NO-TAB-STOP 
     btnLev AT ROW 1.1 COL 91.8 NO-TAB-STOP 
     fiSumVerdi AT ROW 18.48 COL 175 COLON-ALIGNED NO-TAB-STOP 
     btnLev-2 AT ROW 4.33 COL 91.8 NO-TAB-STOP 
     btnCalFraDato AT ROW 4.43 COL 130
     btnCalTilDato AT ROW 4.43 COL 148.6
     btnCalLevUke1 AT ROW 18.57 COL 54.6
     btnCalLevUke4 AT ROW 18.57 COL 132.4
     btnCalLevUke3 AT ROW 18.57 COL 106.2
     btnCalLevUke2 AT ROW 18.57 COL 80
     tbGjennomfaktureres AT ROW 3.33 COL 93.4
     Kode AT ROW 5.71 COL 78.4 COLON-ALIGNED HELP
          "Strekkode inklusive sjekksiffer."
     RectBrowseSearchLinje AT ROW 5.62 COL 1.8
    WITH 1 DOWN NO-BOX OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 5.48
         SCROLLABLE SIZE 198 BY 28.14.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME frmLinje
     rectBrowseLinje AT ROW 6.95 COL 1
     rectBrowseTrans AT ROW 19.57 COL 1.4
     rectButTotal AT ROW 5.48 COL 23
     rectTBvarebehlinjetrans AT ROW 18.48 COL 192
     ArtikkelBilde AT ROW 1.14 COL 170.6
    WITH 1 DOWN NO-BOX OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 5.48
         SCROLLABLE SIZE 198 BY 28.14.

DEFINE FRAME frmRegStat
     sokButikkNr AT ROW 1.48 COL 8.8 COLON-ALIGNED HELP
          "Varegruppe"
     sokButNamn AT ROW 1.48 COL 20 COLON-ALIGNED NO-LABEL
     btnButnr AT ROW 1.48 COL 54.8
     btnAlleButSam AT ROW 12.95 COL 152
     btnAlleBut AT ROW 12.95 COL 175
     fiUtskrNr AT ROW 13.05 COL 120 COLON-ALIGNED
     fiTotSumOrdre AT ROW 14.33 COL 174 COLON-ALIGNED
     rsForslagGodk AT ROW 14.43 COL 33.8 NO-LABEL
     rsAlleReg AT ROW 14.48 COL 2.4 NO-LABEL
     tbVisLevuker AT ROW 14.48 COL 141
     rectRegStat AT ROW 2.95 COL 1.8
     rectRegStatTrans AT ROW 15.52 COL 3
     rectRegStatTransToolbar AT ROW 13 COL 2.2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 5.52
         SIZE 198 BY 28.1.

DEFINE FRAME frmFilter
     fi-fMesseNr AT ROW 1.24 COL 27.2 COLON-ALIGNED HELP
          "Messenummer"
     btnSokFiltMesseNr AT ROW 1.24 COL 42.4 NO-TAB-STOP 
     rsSokGodkStatus AT ROW 2.1 COL 81.4 NO-LABEL
     btnSokFiltProfilNr AT ROW 2.19 COL 42.4 NO-TAB-STOP 
     fi-fProfilNr AT ROW 2.24 COL 27.2 COLON-ALIGNED HELP
          "Prisprofil"
     sokButikk AT ROW 3.14 COL 79 COLON-ALIGNED
     fi-cVarebehBeskrivelse AT ROW 3.29 COL 27.2 COLON-ALIGNED HELP
          "Kort beskrivelse av Varebehen"
     btnBlankFilter AT ROW 3.29 COL 183.2
     "Status:" VIEW-AS TEXT
          SIZE 7 BY .62 AT ROW 2.14 COL 73.8
     "Ved søk etter artikkel på tvers av vareh.bøker (alt-s/F2)" VIEW-AS TEXT
          SIZE 53.2 BY .62 AT ROW 1.1 COL 73.8
     RectArtSok AT ROW 1.29 COL 72.8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 5.48
         SIZE 198 BY 3.91.

DEFINE FRAME frmListe
     rectBrowseListe AT ROW 2.43 COL 1
     RectBrowseSearchListe AT ROW 1.1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 9.33
         SCROLLABLE SIZE 198 BY 24.29.

DEFINE FRAME frmDetalj
     btnSokMesseNr AT ROW 5.43 COL 35.6 NO-TAB-STOP 
     VarebehNr AT ROW 1.24 COL 19 COLON-ALIGNED HELP
          "Varebehnummer"
          LABEL "Vareh.Nr" FORMAT ">>>>>>>>>>>>9"
          VIEW-AS FILL-IN 
          SIZE 20.2 BY 1
     OppdatDato AT ROW 1.48 COL 122.6 COLON-ALIGNED
     OppdatAv AT ROW 1.48 COL 136.6 COLON-ALIGNED NO-LABEL
     Oppdatert AT ROW 1.62 COL 93.4 HELP
          "Varebeh er oppdatert"
     VarebehType AT ROW 2.29 COL 19 COLON-ALIGNED HELP
          "Varebehtype"
          LABEL "Vareh.type" FORMAT "zz9"
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     BeskrivelseVarebehType AT ROW 2.29 COL 25.8 COLON-ALIGNED HELP
          "Kort beskrivelse av Varebehtypen" NO-LABEL FORMAT "X(30)"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     ProfilNr AT ROW 3.33 COL 19 COLON-ALIGNED HELP
          "Prisprofil"
          LABEL "Prisprofil" FORMAT "zz9"
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     KortNavn AT ROW 3.33 COL 29.8 COLON-ALIGNED HELP
          "Kort betegnelse på prisprofilen" NO-LABEL FORMAT "X(15)"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     VarebehBeskrivelse AT ROW 4.38 COL 19 COLON-ALIGNED HELP
          "Kort beskrivelse av Varebehen"
          LABEL "Beskrivelse" FORMAT "X(40)"
          VIEW-AS FILL-IN 
          SIZE 65 BY 1
     MesseNr AT ROW 5.43 COL 19 COLON-ALIGNED HELP
          "Messenummer"
          LABEL "MesseNr" FORMAT ">>>>>>>9"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     MesseBeskrivelse AT ROW 5.43 COL 38 COLON-ALIGNED HELP
          "Navn eller kort beskrivelse av messen." NO-LABEL FORMAT "X(40)"
          VIEW-AS FILL-IN 
          SIZE 46 BY 1
     Kilde AT ROW 6.48 COL 19 COLON-ALIGNED HELP
          "F.eks vareboknummer"
     VareBokBeskrivelse AT ROW 6.48 COL 42 COLON-ALIGNED HELP
          "Kort beskrivelse av vareboken" NO-LABEL
     VarebehNotat AT ROW 7.57 COL 21 NO-LABEL
     btnSokProfilNr AT ROW 3.33 COL 27.2 NO-TAB-STOP 
     RECT-1 AT ROW 1.24 COL 92
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 5.57
         SCROLLABLE SIZE 198 BY 28.05.


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
         TITLE              = "Varehåndtering, messe"
         HEIGHT             = 32.81
         WIDTH              = 200.4
         MAX-HEIGHT         = 57.14
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 57.14
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
       FRAME frmListe:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME frmRegStat:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
ASSIGN 
       BeskrivelseVarebehType:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       KortNavn:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       MesseBeskrivelse:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       MesseNr:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN ProfilNr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE rectNavVarebeh IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       rectNavVarebeh:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       VarebehBeskrivelse:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       VarebehNr:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN VarebehType IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME frmDetalj
                                                                        */
ASSIGN 
       FRAME frmDetalj:HEIGHT           = 28.05
       FRAME frmDetalj:WIDTH            = 198.

/* SETTINGS FOR FILL-IN KortNavn IN FRAME frmDetalj
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN MesseBeskrivelse IN FRAME frmDetalj
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN OppdatAv IN FRAME frmDetalj
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN OppdatDato IN FRAME frmDetalj
   NO-ENABLE                                                            */
ASSIGN 
       VarebehNotat:RETURN-INSERTED IN FRAME frmDetalj  = TRUE.

/* SETTINGS FOR FILL-IN VarebehNr IN FRAME frmDetalj
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME frmFilter
                                                                        */
/* SETTINGS FOR FRAME frmLinje
   Custom                                                               */
ASSIGN 
       FRAME frmLinje:HEIGHT           = 28.14
       FRAME frmLinje:WIDTH            = 198.

ASSIGN 
       btnLev:POPUP-MENU IN FRAME frmLinje       = MENU POPUP-MENU-btnLev:HANDLE.

ASSIGN 
       btnSplitBarY:MOVABLE IN FRAME frmLinje          = TRUE.

ASSIGN 
       fiButTotal:READ-ONLY IN FRAME frmLinje        = TRUE.

ASSIGN 
       fiSumAnt:READ-ONLY IN FRAME frmLinje        = TRUE.

ASSIGN 
       fiSumVerdi:READ-ONLY IN FRAME frmLinje        = TRUE.

ASSIGN 
       fiTimeTot:READ-ONLY IN FRAME frmLinje        = TRUE.

/* SETTINGS FOR FILL-IN KodeX IN FRAME frmLinje
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       KodeX:HIDDEN IN FRAME frmLinje           = TRUE.

/* SETTINGS FOR RADIO-SET rsGodkjent IN FRAME frmLinje
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN sokBestVerdi IN FRAME frmLinje
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN sokFraVPIdato IN FRAME frmLinje
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       sokFraVPIdato:HIDDEN IN FRAME frmLinje           = TRUE.

/* SETTINGS FOR FILL-IN sokLinjeMerknad IN FRAME frmLinje
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN sokTilVPIdato IN FRAME frmLinje
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       sokTilVPIdato:HIDDEN IN FRAME frmLinje           = TRUE.

ASSIGN 
       stdLevUke1:HIDDEN IN FRAME frmLinje           = TRUE.

ASSIGN 
       stdLevUke2:HIDDEN IN FRAME frmLinje           = TRUE.

ASSIGN 
       stdLevUke3:HIDDEN IN FRAME frmLinje           = TRUE.

ASSIGN 
       stdLevUke4:HIDDEN IN FRAME frmLinje           = TRUE.

/* SETTINGS FOR FRAME frmListe
                                                                        */
ASSIGN 
       FRAME frmListe:HEIGHT           = 24.29
       FRAME frmListe:WIDTH            = 198.

/* SETTINGS FOR FRAME frmRegStat
                                                                        */
/* SETTINGS FOR FILL-IN fiTotSumOrdre IN FRAME frmRegStat
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       fiTotSumOrdre:HIDDEN IN FRAME frmRegStat           = TRUE
       fiTotSumOrdre:READ-ONLY IN FRAME frmRegStat        = TRUE.

/* SETTINGS FOR FILL-IN fiUtskrNr IN FRAME frmRegStat
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       fiUtskrNr:HIDDEN IN FRAME frmRegStat           = TRUE.

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
       COLUMN          = 1
       HEIGHT          = 29.48
       WIDTH           = 200
       HIDDEN          = no
       SENSITIVE       = yes.
/* TabStrip OCXINFO:CREATE-CONTROL from: {1EFB6596-857C-11D1-B16A-00C0F0283628} type: TabStrip */
      TabStrip:MOVE-AFTER(KortNavn:HANDLE IN FRAME DEFAULT-FRAME).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Varehåndtering, messe */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Varehåndtering, messe */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frmRegStat
&Scoped-define SELF-NAME btnAlleBut
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAlleBut C-Win
ON CHOOSE OF btnAlleBut IN FRAME frmRegStat /* Registreringer til Excel */
DO:
  DEF VAR cBaseQuery   AS CHAR NO-UNDO.
  DEF VAR iRowsToBatch AS INT  NO-UNDO.
  DEF VAR cRowIdList   AS CHAR NO-UNDO.

  IF hBrowseRegStatTrans:NUM-SELECTED-ROWS > 0 THEN DO:
    DO ix = 1 TO hBrowseRegStatTrans:NUM-SELECTED-ROWS:
      IF hBrowseRegStatTrans:FETCH-SELECTED-ROW(ix) THEN
        cRowIdList = cRowIdList + hFieldMapRegStatTrans:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE + "|".
    END.
    IF DYNAMIC-FUNCTION("runProc","varebeh_ordrebekr.p",
                         STRING(hFieldMapRegStat:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE) + ",,,,,,"
                       + TRIM(cRowIdList,"|") + ",dump"
                        ,httRapport) THEN
      OrdreBekreftToExcel(DYNAMIC-FUNCTION("getTransactionMessage"),"","",FALSE,"","").
    ELSE DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
  END.

  ELSE DO:
    MESSAGE "Ja: Eksporter alle registreringer" SKIP
            "Nei: Eksporter bare registreringer som er knyttet til artikler der kjedens innpris er registrert"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL UPDATE bOK.
    IF bOK NE ? THEN DO:
  
      IF DYNAMIC-FUNCTION("runProc","varebeh_ordrebekr.p",
                           STRING(hFieldMapRegStat:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE) + ",,,,,,,dump" 
                         + (IF NOT bOk THEN ",kjedepris" ELSE "")
                          ,httRapport) THEN
        OrdreBekreftToExcel(DYNAMIC-FUNCTION("getTransactionMessage"),"","",FALSE,"","").
      ELSE DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
  
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAlleButSam
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAlleButSam C-Win
ON CHOOSE OF btnAlleButSam IN FRAME frmRegStat /* Sammendrag til Excel */
DO:
  DEF VAR chExcelApplication AS COM-HANDLE NO-UNDO.
  DEF VAR chWorkSheet        AS COM-HANDLE NO-UNDO.
  DEF VAR cFileName AS CHAR NO-UNDO.

  IF DYNAMIC-FUNCTION("runProc","varebeh_sammendrag.p",
                       STRING(hFieldMap:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE) + ";;"
                      ,?) THEN DO:

    cFileName = DYNAMIC-FUNCTION("getTransactionMessage").
    chExcelApplication = DYNAMIC-FUNCTION("getExcelHandle").
    IF NOT VALID-HANDLE(chExcelApplication) THEN DO:
      IF DYNAMIC-FUNCTION("setWebDoc","Open",cFileName) NE "" THEN
        MESSAGE "Could not open file: " cFileName VIEW-AS ALERT-BOX ERROR.
      RETURN.
    END.
    ELSE DO:
      chExcelApplication:Workbooks:OpenText(cFileName,2,,,,,TRUE).
      chWorkSheet = chExcelApplication:Sheets:ITEM(1).
      chWorkSheet:Rows(1):FONT:Bold = TRUE.
      chWorkSheet:Columns("A:A"):FONT:Bold = TRUE.
      chWorkSheet:Range("C:C"):NumberFormat = "# ##0,00".
      chWorkSheet:Range("E:E"):NumberFormat = "# ##0,00".
      chWorkSheet:Columns("A:E"):AutoFit().
      chExcelApplication:VISIBLE = TRUE.
    END.
  END.
  ELSE DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frmLinje
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
    RUN OpenQuery.
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
      fi-cVarebehBeskrivelse:SCREEN-VALUE = ""
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
  ClearFilter().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frmRegStat
&Scoped-define SELF-NAME btnButnr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnButnr C-Win
ON CHOOSE OF btnButnr IN FRAME frmRegStat /* ... */
DO:
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "Butiker;Butik;ButNamn",
                      "WHERE CAN-DO('" + cButikkListe + "',STRING(Butik))",
                      INPUT-OUTPUT cButikerRowIdList,
                      "Butik",
                      INPUT-OUTPUT cButikerIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk THEN DO:
    IF NUM-ENTRIES(cButikerRowidList) > 1 THEN 
      ASSIGN sokButikkNr:SCREEN-VALUE   = "0"
             sokButNamn:SCREEN-VALUE = STRING(NUM-ENTRIES(cButikerRowidList)) + " av " +
                                       STRING(iSelectorSourcCount)
                                       .
    ELSE
      ASSIGN sokButikkNr:SCREEN-VALUE = cButikerIdList
             sokButNamn:SCREEN-VALUE  = DYNAMIC-FUNCTION("getFieldList","Butiker;ButNamn","WHERE Butik = " + sokButikkNr:SCREEN-VALUE).
      
    DYNAMIC-FUNCTION("setCurrentObject",hBrowseRegStat).
    RUN OpenQuery.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frmLinje
&Scoped-define SELF-NAME btnButTotal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnButTotal C-Win
ON CHOOSE OF btnButTotal IN FRAME frmLinje /* Oppdater total for butikk: */
DO:
  IF ButikkNr:SCREEN-VALUE = ? OR ButikkNr:SCREEN-VALUE = "0" THEN
    DYNAMIC-FUNCTION("DoMessage",0,0,"Du må velge en butikk først","","").
  ELSE DO:
    IF DYNAMIC-FUNCTION("runproc","varebeh_kalktot.p",STRING(hFieldMap:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE)
                                              + "|" + ButikkNr:SCREEN-VALUE
                                              + "|" + cLevNrListe
                                              + "|yes"
                                              ,?) THEN
      ASSIGN fiButTotal:SCREEN-VALUE = DYNAMIC-FUNCTION("getTransactionMessage")
             fiTimeTot:SCREEN-VALUE  = STRING(TIME,"HH:MM")
             .
    ELSE DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalFraDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalFraDato C-Win
ON CHOOSE OF btnCalFraDato IN FRAME frmLinje /* ... */
DO:
  RUN Cal.w (sokFraEdato:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalLevUke1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalLevUke1 C-Win
ON CHOOSE OF btnCalLevUke1 IN FRAME frmLinje /* ... */
DO:
  DEF VAR cUke AS CHAR NO-UNDO.
  IF hFieldMap:AVAIL THEN DO:
    RUN UkeVelger.p (DATE(DYNAMIC-FUNCTION("getFieldValues","messe","WHERE MesseNr = " + STRING(hFieldMap:BUFFER-FIELD("MesseNr"):BUFFER-VALUE),"FraDato")),
                     DATE(DYNAMIC-FUNCTION("getFieldValues","messe","WHERE MesseNr = " + STRING(hFieldMap:BUFFER-FIELD("MesseNr"):BUFFER-VALUE),"TilDato")),
                     OUTPUT cUke).
    IF cUke NE "" THEN DO:
      LevUke1:SCREEN-VALUE = cUke.
      APPLY "entry" TO LevUke1.
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalLevUke2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalLevUke2 C-Win
ON CHOOSE OF btnCalLevUke2 IN FRAME frmLinje /* ... */
DO:
  DEF VAR cUke AS CHAR NO-UNDO.
  IF hFieldMap:AVAIL THEN DO:
    RUN UkeVelger.p (DATE(DYNAMIC-FUNCTION("getFieldValues","messe","WHERE MesseNr = " + STRING(hFieldMap:BUFFER-FIELD("MesseNr"):BUFFER-VALUE),"FraDato")),
                     DATE(DYNAMIC-FUNCTION("getFieldValues","messe","WHERE MesseNr = " + STRING(hFieldMap:BUFFER-FIELD("MesseNr"):BUFFER-VALUE),"TilDato")),
                     OUTPUT cUke).
    IF cUke NE "" THEN DO:
      LevUke2:SCREEN-VALUE = cUke.
      APPLY "entry" TO LevUke2.
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalLevUke3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalLevUke3 C-Win
ON CHOOSE OF btnCalLevUke3 IN FRAME frmLinje /* ... */
DO:
  DEF VAR cUke AS CHAR NO-UNDO.
  IF hFieldMap:AVAIL THEN DO:
    RUN UkeVelger.p (DATE(DYNAMIC-FUNCTION("getFieldValues","messe","WHERE MesseNr = " + STRING(hFieldMap:BUFFER-FIELD("MesseNr"):BUFFER-VALUE),"FraDato")),
                     DATE(DYNAMIC-FUNCTION("getFieldValues","messe","WHERE MesseNr = " + STRING(hFieldMap:BUFFER-FIELD("MesseNr"):BUFFER-VALUE),"TilDato")),
                     OUTPUT cUke).
    IF cUke NE "" THEN DO:
      LevUke3:SCREEN-VALUE = cUke.
      APPLY "entry" TO LevUke3.
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalLevUke4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalLevUke4 C-Win
ON CHOOSE OF btnCalLevUke4 IN FRAME frmLinje /* ... */
DO:
  DEF VAR cUke AS CHAR NO-UNDO.
  IF hFieldMap:AVAIL THEN DO:
    RUN UkeVelger.p (DATE(DYNAMIC-FUNCTION("getFieldValues","messe","WHERE MesseNr = " + STRING(hFieldMap:BUFFER-FIELD("MesseNr"):BUFFER-VALUE),"FraDato")),
                     DATE(DYNAMIC-FUNCTION("getFieldValues","messe","WHERE MesseNr = " + STRING(hFieldMap:BUFFER-FIELD("MesseNr"):BUFFER-VALUE),"TilDato")),
                     OUTPUT cUke).
    IF cUke NE "" THEN DO:
      LevUke4:SCREEN-VALUE = cUke.
      APPLY "entry" TO LevUke4.
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalTilDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalTilDato C-Win
ON CHOOSE OF btnCalTilDato IN FRAME frmLinje /* ... */
DO:
  RUN Cal.w (sokTilEdato:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnHuvGr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnHuvGr C-Win
ON CHOOSE OF btnHuvGr IN FRAME frmLinje /* ... */
DO:

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
    RUN OpenQuery.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLev C-Win
ON CHOOSE OF btnLev IN FRAME frmLinje /* ... */
DO:
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "LevBas;levnr;levnamn;KjedeAvtale|Kjedeavtale|J/N"
                    + ",VarebehLinje;",
                      "WHERE CAN-DO('" + cLevNrListe + "',STRING(LevNr))" 
                    + ",FIRST VarebehLinje OF LevBas NO-LOCK WHERE VarebehNr = " + STRING(hFieldMap:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE)
                      ,INPUT-OUTPUT cLevBasRowIdList,
                      "Levnr",
                      INPUT-OUTPUT cLevBasIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk THEN DO:
    IF NUM-ENTRIES(cLevBasRowidList) > 1 THEN 
      ASSIGN sokLevNr:SCREEN-VALUE   = "0"
             sokLevNamn:SCREEN-VALUE = STRING(NUM-ENTRIES(cLevBasRowidList)) + " av " +
                                       STRING(iSelectorSourcCount)
                                       .
    ELSE 
      ASSIGN sokLevNr:SCREEN-VALUE   = cLevBasIdList
             sokLevNamn:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","LevBas;LevNamn","WHERE LevNr = " + sokLevNr:SCREEN-VALUE)
             .

    setTema().

    RUN OpenQuery.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLev-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLev-2 C-Win
ON CHOOSE OF btnLev-2 IN FRAME frmLinje /* ... */
DO:
  DEF VAR cRowIdList  AS CHAR NO-UNDO.
  DEF VAR cIdList     AS CHAR NO-UNDO.
  DEF VAR iy          AS INT  NO-UNDO.

  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "_file"                               /* Dummy buffer */
                      + ";!_file-name"                      /* <- must invoke a dummy non-visual field to avoid initial sort since calculated fields normally arn't sortable */
                      + ";+Description|CHARACTER|x(30)||Kombinasjon"
                      ,"where false",
                      INPUT-OUTPUT cRowIdList,
                      "Description",
                      INPUT-OUTPUT cIdList,
                      "","",
                      OUTPUT bOK).

  IF bOk THEN DO:
    sokLinjeMerknad:SCREEN-VALUE = TRIM(REPLACE(cIdList,"|",",")).
    RUN OpenQuery.
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
  RUN JBoxDLookup.w ("PrisProfil;ProfilNr;KortNavn","where true",INPUT-OUTPUT cLookupValue).

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
  RUN JBoxDLookup.w ("PrisProfil;ProfilNr;KortNavn","where true",INPUT-OUTPUT cLookupValue).

  IF cLookupValue NE "" THEN 
  DO:
    ProfilNr:SCREEN-VALUE = cLookupValue.
    APPLY "ANY-PRINTABLE":U TO ProfilNr.
    APPLY "TAB":U TO ProfilNr.
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


&Scoped-define SELF-NAME btnTema
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnTema C-Win
ON CHOOSE OF btnTema IN FRAME frmLinje /* Rediger tema */
DO:
  DEF VAR cCurrTema AS CHAR NO-UNDO.

  IF NOT hFieldMap:AVAIL THEN RETURN NO-APPLY.

  cCurrTema = cmbTema:SCREEN-VALUE.

  IF CAN-DO("1",cAdgang) THEN DO:
    IF sokLevNr:SCREEN-VALUE IN FRAME frmLinje = "0" THEN DO:
      DYNAMIC-FUNCTION("DoMessage",0,0,"En leverandør må være valgt før tema kan angis","","").
      RETURN NO-APPLY.
    END.
    ELSE IF cButikkListe NE "*" THEN DO:
      DYNAMIC-FUNCTION("DoMessage",0,0,"Du har ikke adgang til å redigere tema for artikler","","").
      RETURN NO-APPLY.
    END.
  END.

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.

  RUN Levtema.w (hFieldMap:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE,
                 IF CAN-DO("1",cAdgang) OR sokLevNr:SCREEN-VALUE NE "0" THEN sokLevNr:SCREEN-VALUE ELSE "0").
  
  setTema().

  IF cCurrTema NE ? THEN
    cmbTema:SCREEN-VALUE = cCurrTema.

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

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
    RUN OpenQuery.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnVarGr-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnVarGr-2 C-Win
ON CHOOSE OF btnVarGr-2 IN FRAME frmLinje /* ... */
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
    RUN OpenQuery.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ButikkNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ButikkNr C-Win
ON RETURN OF ButikkNr IN FRAME frmLinje /* Butikk */
OR TAB OF ButikkNr DO:
  IF ButikkNr:MODIFIED AND ButikkNr:SCREEN-VALUE NE ? AND hFieldMap:AVAIL THEN 
  DO:
    /* ---- */
/*     IF ButikkNr:SCREEN-VALUE NE "0" AND                                                                                      */
/*        DYNAMIC-FUNCTION("getFieldValues","VareBehLinjeTHode",                                                                */
/*                         "WHERE VarebehNr = " + hFieldMap:BUFFER-FIELD("VarebehNr"):STRING-VALUE                              */
/*                        + " AND ButikkNr  = " + ButikkNr:SCREEN-VALUE,                                                        */
/*                         "VarebehNr") = ? THEN                                                                                */
/*     DO:                                                                                                                      */
/*       iReturn = DYNAMIC-FUNCTION("DoMessage",0,1,"Start registrering for butikk &1" + CHR(10) +                              */
/*                                  "(det tar litt tid å generere registreringsunderlag for butikk)","",ButikkNr:SCREEN-VALUE). */
/*       IF iReturn = 1 THEN                                                                                                    */
/*       DO:                                                                                                                    */
/*         IF NOT DYNAMIC-FUNCTION("runproc",                                                                                   */
/*                                 "varebehlinje_gentrans.p",                                                                   */
/*                                 hFieldMap:BUFFER-FIELD("VarebehNr"):STRING-VALUE + "," + ButikkNr:SCREEN-VALUE,?) THEN       */
/*           DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil i generering av størrelser","").  */
/*         ELSE                                                                                                                 */
/*           APPLY "value-changed" TO hBrowseListe.                                                                             */
/*       END.                                                                                                                   */
/*     END.                                                                                                                     */
    /*
    ELSE 
    DO:
    */
    /* ---- */
      RUN OpenQuery.
      APPLY "value-changed" TO hBrowseLinje.
/*     END. */

    IF DYNAMIC-FUNCTION("getFieldValues","VarebehLinjeThode",
                         "WHERE VarebehNr = " + STRING(hFieldMap:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE)
                        + " AND ButikkNr  = " + ButikkNr:SCREEN-VALUE,
                         "Godkjent") = "no" THEN 
    DO:
      LinkOverlays(TRUE).
/*       ApplyFocusToBestilt1().  */
      APPLY "entry" TO hBrowseLinje.
    END.
    ELSE 
      LinkOverlays(FALSE).
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmbTema
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbTema C-Win
ON VALUE-CHANGED OF cmbTema IN FRAME frmLinje /* Tema */
DO:
  IF NUM-ENTRIES(cmbTema:LIST-ITEM-PAIRS,"|") = 2 AND sokLevNr:SCREEN-VALUE = "0" THEN DO:
    DYNAMIC-FUNCTION("DoMessage",0,0,"Velg en leverandør for å få liste over tema","","").
    RETURN NO-APPLY.
  END.
  RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frmFilter
&Scoped-define SELF-NAME fi-cVarebehBeskrivelse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cVarebehBeskrivelse C-Win
ON RETURN OF fi-cVarebehBeskrivelse IN FRAME frmFilter /* Beskrivelse */
OR "TAB":U OF fi-cVarebehBeskrivelse
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
  RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-fProfilNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-fProfilNr C-Win
ON RETURN OF fi-fProfilNr IN FRAME frmFilter /* Prisprofil */
OR "TAB":U OF fi-fProfilNr
DO:
  RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frmRegStat
&Scoped-define SELF-NAME fiUtskrNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiUtskrNr C-Win
ON RETURN OF fiUtskrNr IN FRAME frmRegStat /* Søk utskriftsnr for butikken */
DO:
  DEF VAR cTemp AS CHAR NO-UNDO.
  cTemp = fiUtskrNr:SCREEN-VALUE.
  APPLY "value-changed" TO hBrowseRegStat.
  fiUtskrNr:SCREEN-VALUE = cTemp.
  APPLY "entry" TO hBrowseRegStatTrans.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frmLinje
&Scoped-define SELF-NAME Kode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Kode C-Win
ON RETURN OF Kode IN FRAME frmLinje /* Strekkode */
DO:
  DEF VAR cQueryWhere AS CHAR NO-UNDO INIT " AND (ArtikkelNr = ".
  DEF VAR cArtNrList  AS CHAR NO-UNDO.
  DEF VAR cKode       AS CHAR NO-UNDO.
  DEF VAR iEn         AS INT  NO-UNDO INIT 49.
  DEF VAR iPluss      AS INT  NO-UNDO INIT 43.
  DEF VAR iTabulator  AS INT  NO-UNDO INIT 9.
  DEF VAR cBestNr     AS CHAR NO-UNDO.
  DEF VAR cStorl      AS CHAR NO-UNDO.
  DEF VAR iMaxBestNr  AS INT  NO-UNDO.
  DEF VAR cBaseQuery  AS CHAR NO-UNDO.
  DEF VAR cLevKod     AS CHAR NO-UNDO.
  
  IF NOT hFieldMap:AVAIL THEN 
    hBrowseListe:SELECT-ROW(hBrowseListe:FOCUSED-ROW).

  IF Kode:SCREEN-VALUE NE "" THEN DO:
    DO ix = 1 TO LENGTH(Kode:SCREEN-VALUE):
      IF CAN-DO("0,1,2,3,4,5,6,7,8,9",SUBSTR(Kode:SCREEN-VALUE,ix,1)) THEN
        cKode = cKode + SUBSTR(Kode:SCREEN-VALUE,ix,1).
    END.
    cKode = FILL("0",13 - LENGTH(cKode)) + cKode.
    IF DYNAMIC-FUNCTION("runproc","strekkode_modell_artlist.p","varebehlinje,varebehnr," + STRING(hFieldMap:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE) + "," + cKode,?) THEN
      ASSIGN cArtNrList = ENTRY(1,DYNAMIC-FUNCTION("getTransactionMessage"),";")
             cStorl     = ENTRY(2,DYNAMIC-FUNCTION("getTransactionMessage"),";")
             Kode:SCREEN-VALUE = cKode.
    ELSE DO: 
      DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
      RETURN.
    END.

    cBaseQuery = DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"baseQuery").
    DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"baseQuery","WHERE false").
    ClearFilter().
    DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"baseQuery",cBaseQuery).

    cLevKod = DYNAMIC-FUNCTION("getFieldValues","VarebehLinje","WHERE VarebehNr = " + STRING(hFieldMap:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE) + " AND ArtikkelNr = " + ENTRY(1,cArtNrList),"LevKod").
/*     cLevKod = DYNAMIC-FUNCTION("getFieldValues","ArtBas","WHERE ArtikkelNr = " + ENTRY(1,cArtNrList),"LevKod"). */

    IF cLevKod NE ? THEN
      cQueryWhere = " AND LevKod = '" + cLevKod + "'".
    ELSE
      DO ix = 1 TO NUM-ENTRIES(cArtNrList):
        cQueryWhere = cQueryWhere + ENTRY(ix,cArtNrList) + (IF ix < NUM-ENTRIES(cArtNrList) THEN " OR ArtikkelNr = " ELSE ")").
      END.

    DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"querywhere",cQueryWhere).
  END.
  ELSE DO:
    ClearFilter().
    RETURN NO-APPLY.
  END.
  
  DYNAMIC-FUNCTION("setCurrentObject",hBrowseLinje).
  RUN OpenQuery.

  DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"querywhere","").

  IF NUM-ENTRIES(cArtNrList) > 1 THEN DO:                          
    bOK = hFieldMapLinje:FIND-FIRST("WHERE VarebehNr = " + STRING(hFieldMap:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE)
                  + " AND ArtikkelNr = " + ENTRY(1,cArtNrList)
                  ,NO-LOCK) NO-ERROR.
    IF bOK THEN DO:
      hBrowseLinje:SET-REPOSITIONED-ROW(hBrowseLinje:DOWN,"Conditional").
      hBrowseLinje:QUERY:REPOSITION-TO-ROWID(hFieldMapLinje:ROWID) NO-ERROR.
      hBrowseLinje:GET-REPOSITIONED-ROW().
      IF NOT ERROR-STATUS:ERROR THEN DO:
        hBrowseLinje:DESELECT-ROWS().
        hBrowseLinje:SELECT-FOCUSED-ROW().
        APPLY "value-changed" TO hBrowseLinje.

      END.
      APPLY "entry" TO Kode.
      RETURN NO-APPLY.
    END.
  END.
  APPLY "entry" TO Kode.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME KodeX
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL KodeX C-Win
ON RETURN OF KodeX IN FRAME frmLinje /* Strekkode(hidden) */
DO:
  RUN StartLinjeTransReg.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME LevUke1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL LevUke1 C-Win
ON RETURN OF LevUke1 IN FRAME frmLinje /* Levuke1 */
OR "tab" OF LevUke1 DO:
  DEF VAR bMod AS LOG NO-UNDO.
  IF NOT CAN-DO(SELF:LIST-ITEMS,SELF:SCREEN-VALUE) THEN DO:
    DYNAMIC-FUNCTION("DoMessage",0,0,"Ugyldig verdi","","").
    RETURN NO-APPLY.
  END.
  bMod = SELF:MODIFIED.
/*   setLevUke(SELF). */
  IF bMod AND setLevUke(SELF) THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME LevUke2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL LevUke2 C-Win
ON RETURN OF LevUke2 IN FRAME frmLinje /* Levuke2 */
OR "tab" OF LevUke2 DO:
  DEF VAR bMod AS LOG NO-UNDO.
  IF NOT CAN-DO(SELF:LIST-ITEMS,SELF:SCREEN-VALUE) THEN DO:
    DYNAMIC-FUNCTION("DoMessage",0,0,"Ugyldig verdi","","").
    RETURN NO-APPLY.
  END.
  bMod = SELF:MODIFIED.
/*   setLevUke(SELF). */
  IF bMod AND setLevUke(SELF) THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME LevUke3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL LevUke3 C-Win
ON RETURN OF LevUke3 IN FRAME frmLinje /* Levuke3 */
OR "tab" OF LevUke3 DO:
  DEF VAR bMod AS LOG NO-UNDO.
  IF NOT CAN-DO(SELF:LIST-ITEMS,SELF:SCREEN-VALUE) THEN DO:
    DYNAMIC-FUNCTION("DoMessage",0,0,"Ugyldig verdi","","").
    RETURN NO-APPLY.
  END.
  bMod = SELF:MODIFIED.
/*   setLevUke(SELF). */
  IF bMod AND setLevUke(SELF) THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME LevUke4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL LevUke4 C-Win
ON RETURN OF LevUke4 IN FRAME frmLinje /* Levuke4 */
OR 'tab' OF LevUke4 DO:  
  DEF VAR hWidget AS HANDLE NO-UNDO.

  IF SELF:MODIFIED THEN DO:
    IF NOT CAN-DO(SELF:LIST-ITEMS,SELF:SCREEN-VALUE) THEN DO:
      DYNAMIC-FUNCTION("DoMessage",0,0,"Ugyldig verdi","","").
      RETURN NO-APPLY.
    END.
    setLevUke(SELF).
    RETURN NO-APPLY.
  END.

  IF DYNAMIC-FUNCTION("getAttribute",hBrowseTrans,"tabchainoverlays") = "" THEN RETURN NO-APPLY.

  hWidget = WIDGET-HANDLE(ENTRY(1,DYNAMIC-FUNCTION("getAttribute",hBrowseTrans,"tabchainoverlays"))).
  IF VALID-HANDLE(hWidget) THEN DO:
    hWidget:TAB-STOP = TRUE.
    LevUke4:MOVE-BEFORE(hWidget).
    hBrowseTrans:PARENT:LAST-TAB-ITEM = hBrowseTrans.
    hBrowseTrans:SELECT-ROW(1).
    APPLY "value-changed" TO hBrowseTrans.
    APPLY "entry" TO hWidget.
  END.

  RETURN NO-APPLY.
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


&Scoped-define SELF-NAME m_Vis_alle_leverandrer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Vis_alle_leverandrer C-Win
ON CHOOSE OF MENU-ITEM m_Vis_alle_leverandrer /* Vis alle leverandører */
DO:
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "LevBas;levnr;levnamn;KjedeAvtale|Kjedeavtale|J/N",
                      "where can-do('" + cLevNrListe + "',string(levnr))",
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
    RUN OpenQuery.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frmDetalj
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


&Scoped-define FRAME-NAME frmRegStat
&Scoped-define SELF-NAME rsAlleReg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rsAlleReg C-Win
ON VALUE-CHANGED OF rsAlleReg IN FRAME frmRegStat
DO:
  IF NOT hFieldMap:AVAIL THEN DO:
    rsAlleReg:SCREEN-VALUE = "1".
    rsAlleReg = 1.
    RETURN NO-APPLY.
  END.

  DYNAMIC-FUNCTION("setAttribute",hBrowseRegStatTrans,"uselocaldata","no").

  ASSIGN rsAlleReg.
  IF rsAlleReg = 1 THEN DO:
    DYNAMIC-FUNCTION("createParentLink",hBrowseRegStatTrans,hBrowseRegStat,'VarebehNr,ButikkNr').
    APPLY "value-changed" TO hBrowseRegStat.
  END.
  ELSE DO:
    DYNAMIC-FUNCTION("DeleteObjectLink",hBrowseRegStatTrans,hBrowseRegStat).
    DYNAMIC-FUNCTION("setAttribute",hBrowseRegStatTrans,"basequery",
                     "WHERE VarebehNr = " + STRING(hFieldMap:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE)
                   + " AND ArtikkelNr > 0").
    DYNAMIC-FUNCTION("setCurrentObject",hBrowseRegStatTrans).
    RUN OpenQuery.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frmLinje
&Scoped-define SELF-NAME rsBestilling
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rsBestilling C-Win
ON VALUE-CHANGED OF rsBestilling IN FRAME frmLinje
DO:
  IF rsBestilling:SCREEN-VALUE > "0" AND (ButikkNr:SCREEN-VALUE = "0" OR ButikkNr:SCREEN-VALUE = ?) THEN DO:
    DYNAMIC-FUNCTION("DoMessage",0,0,"Filteret gir ikke resultat siden det ikke er valgt noen butikk","","").
    rsBestilling:SCREEN-VALUE = "0".
    RETURN NO-APPLY.
  END.
  ASSIGN rsBestilling.
  IF rsBestilling = 1 THEN DO:
    ASSIGN sokBestVerdi:SENSITIVE = TRUE
           rsGodkjent:SENSITIVE   = TRUE.
    APPLY "entry" TO sokBestVerdi.
  END.
  ELSE DO:
    ASSIGN sokBestVerdi:SCREEN-VALUE = "0"
           sokBestVerdi:SENSITIVE    = FALSE
           rsGodkjent:SCREEN-VALUE   = "0"
           rsGodkjent:SENSITIVE      = FALSE
           .
    RUN OpenQuery.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frmRegStat
&Scoped-define SELF-NAME rsForslagGodk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rsForslagGodk C-Win
ON VALUE-CHANGED OF rsForslagGodk IN FRAME frmRegStat
DO:
  IF NOT hFieldMap:AVAIL THEN RETURN NO-APPLY.

  ASSIGN rsForslagGodk.
  IF rsForslagGodk = 1 THEN DO:
    DYNAMIC-FUNCTION("setAttribute",hBrowseRegStatTrans,"calcparamGodkjentBest","").
    rsForslagGodk:BGCOLOR = 8.
  END.
  ELSE IF rsForslagGodk = 2 THEN DO:
    DYNAMIC-FUNCTION("setAttribute",hBrowseRegStatTrans,"calcparamGodkjentBest","no").
    rsForslagGodk:BGCOLOR = iColNumBestiltForsl.
  END.
  ELSE DO:
    DYNAMIC-FUNCTION("setAttribute",hBrowseRegStatTrans,"calcparamGodkjentBest","yes").
    rsForslagGodk:BGCOLOR = iColNumBestiltGodk.
  END.
  APPLY "value-changed" TO rsAlleReg.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frmLinje
&Scoped-define SELF-NAME rsGodkjent
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rsGodkjent C-Win
ON VALUE-CHANGED OF rsGodkjent IN FRAME frmLinje
DO:
  ASSIGN rsGodkjent.
  IF rsGodkjent > 0 THEN
    RUN OpenQuery.
  ELSE
    APPLY "entry" TO sokBestVerdi.
/*   IF rsGodkjent = 1 THEN DO:               */
/*     sokBestVerdi:SENSITIVE = TRUE.         */
/*   END.                                     */
/*   ELSE DO:                                 */
/*     ASSIGN sokBestVerdi:SCREEN-VALUE = "0" */
/*            sokBestVerdi:SENSITIVE = FALSE  */
/*            .                               */
/*     RUN OpenQuery.                         */
/*   END.                                     */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokAktBeskrivelse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokAktBeskrivelse C-Win
ON RETURN OF sokAktBeskrivelse IN FRAME frmLinje
OR TAB OF sokAktBeskrivelse DO:
  IF sokAktBeskrivelse:MODIFIED THEN RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokAktNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokAktNr C-Win
ON RETURN OF sokAktNr IN FRAME frmLinje /* Aktivitet */
OR TAB OF sokAktNr DO:
  IF sokAktNr:MODIFIED THEN DO: 
    sokAktNr:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","Aktivitet;Beskrivelse","WHERE AktNr = " + sokAktNr:SCREEN-VALUE).
    RUN OpenQuery.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokArtikkelNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokArtikkelNr C-Win
ON RETURN OF sokArtikkelNr IN FRAME frmLinje /* SE Art.nr */
OR TAB OF sokArtikkelNr DO:
  IF sokArtikkelNr:MODIFIED THEN DO:
    RUN OpenQuery.
    ApplyFocusToBestilt1().
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
    RUN OpenQuery.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokAvdelingNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokAvdelingNr C-Win
ON RETURN OF sokAvdelingNr IN FRAME frmLinje /* Avdeling */
OR TAB OF sokAvdelingNr DO:
  IF sokAvdelingNr:MODIFIED THEN DO:
    sokAvdelingNavn:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","Avdeling;AvdelingNavn","WHERE AvdelingNr = " + sokAvdelingNr:SCREEN-VALUE).
    RUN OpenQuery.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokBeskr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokBeskr C-Win
ON RETURN OF sokBeskr IN FRAME frmLinje /* Utv.søk */
OR TAB OF sokBeskr DO:
  IF sokBeskr:MODIFIED THEN RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokBestVerdi
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokBestVerdi C-Win
ON RETURN OF sokBestVerdi IN FRAME frmLinje /* Verdi > */
DO:
  RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokBestVerdi C-Win
ON TAB OF sokBestVerdi IN FRAME frmLinje /* Verdi > */
DO:
  IF SELF:MODIFIED THEN RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frmRegStat
&Scoped-define SELF-NAME sokButikkNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokButikkNr C-Win
ON RETURN OF sokButikkNr IN FRAME frmRegStat /* Butikk */
OR TAB OF sokButikkNr DO:
  IF sokButikkNr:MODIFIED THEN DO:
    sokButNamn:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","Butiker;Butnamn","WHERE Butik = " + sokButikkNr:SCREEN-VALUE).
    DYNAMIC-FUNCTION("setCurrentObject",hBrowseRegStat).
    RUN OpenQuery.
  END.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokButNamn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokButNamn C-Win
ON RETURN OF sokButNamn IN FRAME frmRegStat
OR TAB OF sokButNamn DO:
  IF sokButNamn:MODIFIED THEN DO: 
    ASSIGN cButikerRowIdList = ""
           cButikerIdList    = ""
           .   
    DYNAMIC-FUNCTION("setCurrentObject",hBrowseRegStat).
    RUN OpenQuery.
  END.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frmLinje
&Scoped-define SELF-NAME sokFraEdato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokFraEdato C-Win
ON RETURN OF sokFraEdato IN FRAME frmLinje /* Endret dato */
OR TAB OF sokFraEdato DO:
  IF SELF:MODIFIED THEN RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokFraVPIdato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokFraVPIdato C-Win
ON RETURN OF sokFraVPIdato IN FRAME frmLinje /* VPI dato (hidden) */
OR TAB OF sokFraVPIdato DO:
  IF SELF:MODIFIED THEN RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokHg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokHg C-Win
ON RETURN OF sokHg IN FRAME frmLinje /* Hovedgr */
OR TAB OF sokHg DO:
  IF sokHg:MODIFIED THEN DO: 
    sokHgBeskr:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","HuvGr;HgBeskr","WHERE Hg = " + sokHg:SCREEN-VALUE).
    RUN OpenQuery.
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
    RUN OpenQuery.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokLevKod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokLevKod C-Win
ON RETURN OF sokLevKod IN FRAME frmLinje /* LevArtNr */
OR TAB OF sokLevKod DO:
  IF sokLevKod:MODIFIED THEN 
    RUN OpenQuery.
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
    RUN OpenQuery.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokLevNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokLevNr C-Win
ON RETURN OF sokLevNr IN FRAME frmLinje /* Lev */
OR TAB OF sokLevNr DO:
  IF sokLevNr:MODIFIED THEN DO:
    sokLevNamn:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","LevBas;LevNamn","WHERE LevNr = " + sokLevNr:SCREEN-VALUE).
    IF sokLevNr:SCREEN-VALUE NE "0" THEN
      setTema().
    RUN OpenQuery.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokLinjeMerknad
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokLinjeMerknad C-Win
ON RETURN OF sokLinjeMerknad IN FRAME frmLinje /* Merknad */
OR TAB OF sokLinjeMerknad DO:
  IF sokLinjeMerknad:MODIFIED THEN RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokTilEdato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokTilEdato C-Win
ON RETURN OF sokTilEdato IN FRAME frmLinje
OR TAB OF sokTilEdato DO:
  IF SELF:MODIFIED THEN RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokTilVPIdato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokTilVPIdato C-Win
ON RETURN OF sokTilVPIdato IN FRAME frmLinje
OR TAB OF sokTilVPIdato DO:
  IF SELF:MODIFIED THEN RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokVg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokVg C-Win
ON RETURN OF sokVg IN FRAME frmLinje /* Varegr */
OR TAB OF sokVg DO:
  IF sokVg:MODIFIED THEN DO: 
    sokVgBeskr:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","VarGr;VgBeskr","WHERE Vg = " + sokVg:SCREEN-VALUE).
    RUN OpenQuery.
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
    RUN OpenQuery.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME stdLevUke1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stdLevUke1 C-Win
ON LEAVE OF stdLevUke1 IN FRAME frmLinje /* Std.uker */
DO:
  IF NOT CAN-DO(SELF:LIST-ITEMS,SELF:SCREEN-VALUE) THEN DO:
    DYNAMIC-FUNCTION("DoMessage",0,0,"Ugyldig verdi","","").
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME stdLevUke2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stdLevUke2 C-Win
ON LEAVE OF stdLevUke2 IN FRAME frmLinje
DO:
  IF NOT CAN-DO(SELF:LIST-ITEMS,SELF:SCREEN-VALUE) THEN DO:
    DYNAMIC-FUNCTION("DoMessage",0,0,"Ugyldig verdi","","").
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME stdLevUke3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stdLevUke3 C-Win
ON LEAVE OF stdLevUke3 IN FRAME frmLinje
DO:
  IF NOT CAN-DO(SELF:LIST-ITEMS,SELF:SCREEN-VALUE) THEN DO:
    DYNAMIC-FUNCTION("DoMessage",0,0,"Ugyldig verdi","","").
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME stdLevUke4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stdLevUke4 C-Win
ON LEAVE OF stdLevUke4 IN FRAME frmLinje
DO:
  IF NOT CAN-DO(SELF:LIST-ITEMS,SELF:SCREEN-VALUE) THEN DO:
    DYNAMIC-FUNCTION("DoMessage",0,0,"Ugyldig verdi","","").
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
&Scoped-define SELF-NAME tbGjennomfaktureres
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbGjennomfaktureres C-Win
ON VALUE-CHANGED OF tbGjennomfaktureres IN FRAME frmLinje /* Kun gj.fakt */
DO:
  RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbKjedeVare
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbKjedeVare C-Win
ON VALUE-CHANGED OF tbKjedeVare IN FRAME frmLinje /* Kun kjedev. */
DO:
  RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frmRegStat
&Scoped-define SELF-NAME tbVisLevuker
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbVisLevuker C-Win
ON VALUE-CHANGED OF tbVisLevuker IN FRAME frmRegStat /* Vis lev.uker */
DO:
  DO ix = 1 TO 8:
    hBrwLinjeTransUker[ix]:VISIBLE = SELF:CHECKED.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frmDetalj
&Scoped-define SELF-NAME VarebehType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL VarebehType C-Win
ON RETURN OF VarebehType IN FRAME frmDetalj /* Vareh.type */
OR "TAB" OF VarebehType
DO:
  IF SELF:MODIFIED THEN
  DO:
      BeskrivelseVarebehType:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldValues","VarebehType",
                                              "WHERE VarebehType = '" + VarebehType:SCREEN-VALUE + "'","BeskrivelseVarebehType").  
      APPLY "ANY-PRINTABLE":U TO VarebehType.
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

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
  IF NOT DYNAMIC-FUNCTION("DoCleanUpObjects",THIS-PROCEDURE:CURRENT-WINDOW) THEN
      RETURN NO-APPLY.
  DYNAMIC-FUNCTION("setCleanUpResize",THIS-PROCEDURE:CURRENT-WINDOW).
  IF VALID-HANDLE(hVisBilde) THEN APPLY "close" TO hVisBilde.
  IF VALID-HANDLE(hArtikkelkort) THEN APPLY "close" TO hArtikkelkort.
  IF VALID-HANDLE(hArtBilde) THEN APPLY "close" TO hArtBilde.

  DELETE OBJECT hSumTransBuffer NO-ERROR.
  DELETE OBJECT hSumTransQuery NO-ERROR.
  PUBLISH "InvalidateHandle" (THIS-PROCEDURE).
  RUN disable_UI.
  &IF DEFINED(UIB_is_Running) NE 0 &THEN
    QUIT.
  &ENDIF
END.

{incl/wintrigg.i}

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

ON 'ctrl-S':U OF hWindow ANYWHERE DO: 
  IF iTab = 3 THEN DO:
    DYNAMIC-FUNCTION("DoLockWindow",THIS-PROCEDURE:CURRENT-WINDOW).
    APPLY "return".
    DYNAMIC-FUNCTION("setCurrentObject",hUpdToolBar).
    RUN NextRecord.
    ApplyFocusToBestilt1().
    DYNAMIC-FUNCTION("DoLockWindow",?).
  END.
  ELSE
    RETURN NO-APPLY.
END.

ON 'ctrl-P':U OF hWindow ANYWHERE DO: 
  IF iTab = 3 THEN DO:
    DYNAMIC-FUNCTION("DoLockWindow",THIS-PROCEDURE:CURRENT-WINDOW).
    APPLY "return".
    DYNAMIC-FUNCTION("setCurrentObject",hUpdToolBar).
    RUN PrevRecord.
    ApplyFocusToBestilt1().
    DYNAMIC-FUNCTION("DoLockWindow",?).
  END.
  ELSE
    RETURN NO-APPLY.
END.

ON F2,ALT-S OF hWindow ANYWHERE 
DO:
  IF iTab = 3 AND hFieldMap:AVAIL AND cButikkListe = "*" AND cLevNrListe = "*" AND bArtVedl THEN DO:
    IF DYNAMIC-FUNCTION("DoMessage",0,1,
                        "NB! Artikkelen du legger til her vil få kalkyleopplysninger fra artikkelkortet" + CHR(10)
                      + "Det vil si at f.eks suppleringspris ikke vil vises på ordrebekreftelser" + CHR(10) + CHR(10)
                      + "Du kan også føre over nye artikler til vareboken, justere kalkyle og deretter komplettere varehåndteringsboken" + CHR(10)
                      + "Vil du fortsette med å hente artikkel direkte fra artikkelregisteret?"
                        ,"","") 
                        NE 1 THEN RETURN NO-APPLY.
    RUN d-hsok.w (OUTPUT fArtikkelNr,"JA"). 
    IF fArtikkelNr = ? OR fArtikkelNr = 0 THEN 
      RETURN NO-APPLY.
    /* --- Henter artikkel fra VPI --- */
    ELSE IF fArtikkelNr < 0 AND fArtikkelNr <> ? THEN
    DO:
      ASSIGN
          cKode = RETURN-VALUE
          bOk   = TRUE 
          .

      cVPILevKortNavnLevNr = DYNAMIC-FUNCTION("getFieldValues","EkstVPILev","WHERE EkstVPILevNr = " + STRING(ABS(INT(fArtikkelNr))),"KortNavn,EkstVPILevNr").
      IF DYNAMIC-FUNCTION("DoMessage",0,1,
                          "Artikkel er ikke registrert i lokalt artikkelregister." + CHR(10) + CHR(10) +
                          (IF cVPILevKortNavnLevNr NE ? THEN "Den finnes i VPI register til " + ENTRY(1,cVPILevKortNavnLevNr,"|")
                           ELSE "* Ukjent VPI leverandør - " + STRING(ABS(INT(fArtikkelNr))) + " *") + CHR(10) +
                          "Skal oppslag gjøres mot VPI registeret?",
                          "","") = 1 THEN DO:

        IF cVPILevKortNavnLevNr NE ? THEN DO:
          /* Bytter søkebegrep hvis det er en strekkode. */
          IF DYNAMIC-FUNCTION("getFieldValues","VPIArtBas","WHERE EkstVPILevNr = " + ENTRY(2,cVPILevKortNavnLevNr,"|") 
                                                          + " AND VareNr = '" + cKode + "'","VareNr") = ? THEN DO:
            cTmpKode = DYNAMIC-FUNCTION("getFieldValues","VPIStrekkode","WHERE EkstVPILevNr = " + ENTRY(2,cVPILevKortNavnLevNr,"|")
                                                                    + " AND Kode = '" + cKode + "'","Varenr").
            IF cTmpKode NE ? THEN
              cKode = cTmpKode.
          END.
        END.
      END.
      ELSE RETURN NO-APPLY.

      /* Kaller søkerutine */
      RUN gvpiartbas.w (
        INPUT-OUTPUT cTekst, /* Returstreng - chr(1) separert */
        "EkstVPILevNr", /* Feltliste avgrensningsfelt (kommaseparert) */
        STRING(ABS(INT(fArtikkelNr))), /* Feltverdier (chr(1) sep) */ 
        cKode, /* Post markøren skal stå på */
        wArtBasRecid
        ).
      IF RETURN-VALUE = "AVBRYT" THEN
          RETURN NO-APPLY.
      IF NUM-ENTRIES(cTekst,CHR(1)) >= 3 THEN
      DO:
        fArtikkelNr = DEC(DYNAMIC-FUNCTION("getFieldValues","ArtBas","WHERE ArtikkelNr = DEC('" + ENTRY(3,cTekst,CHR(1)) + "')","ArtikkelNr")) NO-ERROR.
        IF fArtikkelNr = ? THEN RETURN NO-APPLY.
      END.
    END.
    /* --- VPI henting ferdig      --- */

    /* Hent evt andre artikkler i samme modell: */
    ELSE DO:
      cModellFarge = DYNAMIC-FUNCTION("getFieldValues","ArtBas","WHERE ArtikkelNr = " + STRING(fArtikkelNr),"ModellFarge").
      IF cModellFarge NE "0" AND cModellFarge NE ? THEN DO:
        cModellArtList = DYNAMIC-FUNCTION("getFieldList","ArtBas;ArtikkelNr",
                                          "WHERE ArtBas.ModellFarge = " + cModellFarge).
        IF NUM-ENTRIES(cModellArtList,"|") > 0 THEN DO:
          IF DYNAMIC-FUNCTION("DoMessage",0,4,"Det finnes andre artikler med samme modell. Vil du også hente disse?","","") = 6 THEN DO:
            IF NOT DYNAMIC-FUNCTION("runproc","varebeh_legg_til_art_messe.p",STRING(hFieldMap:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE) + "," +
                                                                             cModellArtList,?) THEN
              DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
            ELSE DO:
              DYNAMIC-FUNCTION("setCurrentObject",hBrowseLinje).
              hBrowseLinje:SET-REPOSITIONED-ROW(hBrowseLinje:FOCUSED-ROW,"conditional").
              RUN OpenQuery.
              bOk = hFieldMapLinje:FIND-FIRST("WHERE ArtikkelNr = " + STRING(fArtikkelNr),NO-LOCK) NO-ERROR.
              IF bOk THEN DO:
                hBrowseLinje:QUERY:REPOSITION-TO-ROWID(hFieldMapLinje:ROWID) NO-ERROR.
                IF NOT ERROR-STATUS:ERROR THEN
                  APPLY "value-changed" TO hBrowseLinje.
              END.
            END.
          END.
          RETURN NO-APPLY.
        END.
      END.
    END.

    DO:  
      IF NOT DYNAMIC-FUNCTION("runproc","varebeh_legg_til_art_messe.p",STRING(hFieldMap:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE) + "," +
                                                                       STRING(fArtikkelNr),?) THEN
        DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
      ELSE DO:
        DYNAMIC-FUNCTION("setCurrentObject",hBrowseLinje).
        hBrowseLinje:SET-REPOSITIONED-ROW(hBrowseLinje:FOCUSED-ROW,"conditional").
        RUN OpenQuery.
        bOk = hFieldMapLinje:FIND-FIRST("WHERE ArtikkelNr = " + STRING(fArtikkelNr),NO-LOCK) NO-ERROR.
        IF bOk THEN DO:
          hBrowseLinje:QUERY:REPOSITION-TO-ROWID(hFieldMapLinje:ROWID) NO-ERROR.
          IF NOT ERROR-STATUS:ERROR THEN
            APPLY "value-changed" TO hBrowseLinje.
        END.
      END.
    END.
  END.
  ELSE IF iTab < 3 THEN DO WITH FRAME frmFilter:
    RUN d-hsok.w (OUTPUT fArtikkelNr,"JA"). 
    IF fArtikkelNr = ? OR fArtikkelNr = 0 THEN 
      RETURN NO-APPLY.

    IF rsSokGodkStatus NE 1 OR sokButikk:SCREEN-VALUE NE "0" THEN
      DYNAMIC-FUNCTION("setAttribute",hBrowseListe,"prescanqueryfilter",
                       "VarebehLinjeTrans WHERE ArtikkelNr = " + STRING(fArtikkelNr) 
                     + " AND VarebehlinjeTrans.VarebehNr  > -999999"
                     + (IF sokButikk:SCREEN-VALUE NE "0" THEN 
                         " AND ButikkNr = " + sokButikk:SCREEN-VALUE
                        ELSE 
                          " AND ButikkNr > -99999")
                     + " AND VarebehlinjeTrans.SeqNr  > -999999"
                     + (IF rsSokGodkStatus:SCREEN-VALUE NE "1" THEN 
                         " AND GodkjentBestilling = " + STRING(rsSokGodkStatus:SCREEN-VALUE = "2")
                        ELSE "")
                     + ",FIRST VarebehLinje OF VarebehLinjeTrans NO-LOCK,FIRST VarebehHode OF VarebehLinje NO-LOCK").
    ELSE
      DYNAMIC-FUNCTION("setAttribute",hBrowseListe,"prescanqueryfilter",
                       "VarebehLinjeTrans WHERE ArtikkelNr = " + STRING(fArtikkelNr) + ",FIRST VarebehHode OF VarebehLinjeTrans NO-LOCK").
    DYNAMIC-FUNCTION("setCurrentObject",hBrowseListe).
    RUN OpenQuery.
    DYNAMIC-FUNCTION("setAttribute",hBrowseListe,"prescanqueryfilter","").
    IF hBrowseListe:QUERY:NUM-RESULTS > 0 THEN DO:
      sokArtikkelNr:SCREEN-VALUE IN FRAME frmLinje = STRING(fArtikkelNr).
      IF sokButikk:SCREEN-VALUE NE "0" THEN
        ButikkNr:SCREEN-VALUE = sokButikk:SCREEN-VALUE.
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

  DYNAMIC-FUNCTION("DoLockWindow",THIS-PROCEDURE:CURRENT-WINDOW).
  DYNAMIC-FUNCTION("setAttribute",SESSION,"userkeepswindowlocked","yes").
  {lng.i}
  RUN enable_UI.
  &IF DEFINED(UIB_is_Running) NE 0 &THEN
    DYNAMIC-FUNCTION("NewObject",SESSION,SESSION,"session").
    DYNAMIC-FUNCTION("setAttribute",SESSION,"ButikkListe","260").
  &ENDIF
  setButikkListe().
  RUN InitWindow.

  DYNAMIC-FUNCTION("setAttribute",SESSION,"userkeepswindowlocked","").
  DYNAMIC-FUNCTION("DoLockWindow",?).

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

ON 'window-resized':U OF {&WINDOW-NAME} DO:
  DEF VAR rHode AS ROWID NO-UNDO.
    
  rHode = hFieldMap:ROWID.
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"Resize","").

  hFieldMap:FIND-BY-ROWID(rHode).

  CASE iTab:
    WHEN 1 THEN DO:
      FRAME frmListe:MOVE-TO-TOP().
      FRAME frmFilter:MOVE-TO-TOP().
    END.
    WHEN 2 THEN DO:
      FRAME frmDetalj:MOVE-TO-TOP().
    END.
    WHEN 3 THEN DO:
      FRAME frmLinje:MOVE-TO-TOP().
      APPLY "value-changed" TO hBrowseTrans.
    END.
    WHEN 4 THEN DO:
      FRAME frmRegStat:MOVE-TO-TOP().
    END.
  END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AddColorRecord C-Win 
PROCEDURE AddColorRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cArtRowIdList AS CHAR NO-UNDO.
DEF VAR cArtIdList    AS CHAR NO-UNDO.
DEF VAR cModellFarge  AS CHAR NO-UNDO.
DEF VAR crRepos       AS CHAR NO-UNDO.

IF hBrowseLinje:NUM-SELECTED-ROWS > 1 THEN DO:
  DYNAMIC-FUNCTION("DoMessage",0,0,"Funksjonen kan bare utføres hvis èn artikkel er valgt","","").
  RETURN.
END.

IF hFieldMapLinje:AVAIL THEN DO:

  ASSIGN cModellFarge = DYNAMIC-FUNCTION("getFieldValues","ArtBas","WHERE ArtikkelNr = " + STRING(hFieldMapLinje:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE),"ModellFarge")
         iReturn = 0
         crRepos = hFieldMapLinje:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE
         .

  IF DEC(cModellFarge) = 0 THEN DO:
    iReturn = DYNAMIC-FUNCTION("DoMessage",0,1,"Artikkel inngår ikke i modell. Vil du søke på leverandørnr og varegruppe for denne artiklen?" + CHR(10) +
                                               "OBS! De artiklene som velges for å legges til vil få samme kalkyle som den artiklen du har markert (PRS artikkelnr " + 
                                               STRING(hFieldMapLinje:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) + ") har i varehåndteringsboken." + CHR(10) +
                                               "Hvis artiklen du velger allerede eksisterer i varehåndteringsboken så blir den ikke skrevet over" + CHR(10) +
                                               "Valgte artikler legges inn i varehåndteringsboken og det generers samtidig registreringsunderlag for de butikker som er påbegynt med registrering"
                               ,"","").
    IF iReturn NE 1 THEN RETURN.
  END.
  ELSE 
    DYNAMIC-FUNCTION("DoMessage",0,0,"OBS! De artiklene som velges for å legges til vil få samme kalkyle som den artiklen du har markert (PRS artikkelnr " + 
                                     STRING(hFieldMapLinje:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) + ") har i varehåndteringsboken." + CHR(10) +
                                     "Hvis artiklen du velger allerede eksisterer i varehåndteringsboken så blir den ikke skrevet over." + CHR(10) +
                                     "Valgte artikler legges inn i varehåndteringsboken og det generers samtidig registreringsunderlag for de butikker som er påbegynt med registrering"
                               ,"","").

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "ArtBas;ArtikkelNr;LevKod;Beskr;LevFargKod;Farg;!ModellFarge;!Levnr;!Vg,Farg;FarBeskr,LevBas;LevNamn,VarGr;VgBeskr",
                      (IF iReturn = 1 THEN
                        "WHERE ArtBas.Levnr = " + DYNAMIC-FUNCTION("getFieldValues","ArtBas","WHERE ArtikkelNr = " + STRING(hFieldMapLinje:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE),"Levnr") 
                      + "  AND ArtBas.Vg    = " + STRING(hFieldMapLinje:BUFFER-FIELD("Vg"):BUFFER-VALUE)
                      + "  AND ArtBas.Lopnr GE 0"
                       ELSE
                        "WHERE ArtBas.Modellfarge = " + cModellFarge)
                      + "  AND ArtBas.ArtikkelNr NE " + STRING(hFieldMapLinje:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE)
                      + ",FIRST Farg OF ArtBas NO-LOCK, FIRST LevBas OF ArtBas NO-LOCK, FIRST VarGr OF ArtBas NO-LOCK",
                      INPUT-OUTPUT cArtRowIdList,
                      "ArtikkelNr",
                      INPUT-OUTPUT cArtIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk AND cArtIdList NE "" THEN DO:
    IF NOT DYNAMIC-FUNCTION("runproc","varebeh_legg_til_farger.p",STRING(hFieldMapLinje:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE) + "," +
                                                                  STRING(hFieldMapLinje:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) + "," + cArtIdList,?) THEN
      DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
    ELSE DO:
      DYNAMIC-FUNCTION("setCurrentObject",hBrowseLinje).
      hBrowseLinje:SET-REPOSITIONED-ROW(hBrowseLinje:FOCUSED-ROW,"conditional").
      RUN OpenQuery.
      bOk = hFieldMapLinje:FIND-FIRST("WHERE RowIdent1 = '" + crRepos + "'",NO-LOCK).
      IF bOk THEN DO:
        hBrowseLinje:QUERY:REPOSITION-TO-ROWID(hFieldMapLinje:ROWID) NO-ERROR.
        IF NOT ERROR-STATUS:ERROR THEN
          APPLY "value-changed" TO hBrowseLinje.
      END.
    END.
  END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AddInndelingRecord C-Win 
PROCEDURE AddInndelingRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cRowIdList AS CHAR NO-UNDO.
DEF VAR cIdList    AS CHAR NO-UNDO.

IF NOT hFieldMap:AVAIL THEN RETURN.

IF hBrowseLinje:NUM-SELECTED-ROWS > 1 THEN DO:
  DYNAMIC-FUNCTION("DoMessage",0,0,"Funksjonen kan bare utføres hvis èn artikkel er valgt","","").
  RETURN.
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
  IF DYNAMIC-FUNCTION("runproc","varebeh_legg_til_inndeling.p",
                      STRING(hFieldMapLinje:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE) + "," +
                      STRING(hFieldMapLinje:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) + "," + cIdList,?) THEN 
    APPLY "value-changed" TO hBrowseLinje.
  ELSE DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AddStrRecord C-Win 
PROCEDURE AddStrRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cKodeRowIdList AS CHAR NO-UNDO.
DEF VAR cKodeIdList    AS CHAR NO-UNDO.
DEF VAR cModellFarge  AS CHAR NO-UNDO.

IF hBrowseLinje:NUM-SELECTED-ROWS > 1 THEN DO:
  DYNAMIC-FUNCTION("DoMessage",0,0,"Funksjonen kan bare utføres hvis èn artikkel er valgt","","").
  RETURN.
END.

IF hFieldMapLinje:AVAIL THEN DO:

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "StrekKode;!Kode;!StrKode;!HovedNr;!ArtikkelNr,StrKonv;distinct Storl",
                      "WHERE Strekkode.ArtikkelNr = " + STRING(hFieldMapLinje:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) 
                      + " AND Strekkode.kode > ''"
                      + ",FIRST StrKonv OF Strekkode NO-LOCK"
                      + ";WHERE Strekkode.ArtikkelNr < 0",
                      INPUT-OUTPUT cKodeRowIdList,
                      "StrKode",
                      INPUT-OUTPUT cKodeIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk AND cKodeIdList NE "" THEN DO:
    IF NOT DYNAMIC-FUNCTION("runproc","varebeh_legg_til_strekkoder.p",STRING(hFieldMapLinje:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE) + "," +
                                                                      STRING(hFieldMapLinje:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) + "," + cKodeIdList,?) THEN
      DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
    ELSE APPLY "value-changed" TO hBrowseLinje.
  END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AddToTemaRecord C-Win 
PROCEDURE AddToTemaRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR ocValue     AS CHAR NO-UNDO.
DEF VAR cRowIdList  AS CHAR NO-UNDO.
DEF VAR cTotTema    AS CHAR NO-UNDO.
DEF VAR cTema       AS CHAR NO-UNDO.
DEF VAR cLevNavn    AS CHAR NO-UNDO.

IF NOT hFieldMap:AVAIL THEN RETURN.

IF CAN-DO("1",cAdgang) THEN 
  cTotTema = SUBSTR(cmbTema:LIST-ITEM-PAIRS IN FRAME frmLinje,4).
ELSE 
  cTotTema = DYNAMIC-FUNCTION("getFieldList","VarebokTemaHode;VbBeskrivelse;VbTemeNr",
                                             "WHERE VarebehNr = " + STRING(hFieldMap:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE)
                              ).

IF cTotTema NE "" THEN DO:
  IF CAN-DO("1",cAdgang) THEN DO ix = 1 TO NUM-ENTRIES(cTotTema,"|") BY 2:
    IF INT(DYNAMIC-FUNCTION("getFieldValues","VarebokTemaHode","WHERE VbTemeNr = " + ENTRY(ix + 1,cTotTema,"|"),"LevNr")) NE 0 THEN
      cTema = cTema + ENTRY(ix,cTotTema,"|") + "|" + ENTRY(ix + 1,cTotTema,"|") + "|".
  END.
  ELSE DO ix = 1 TO NUM-ENTRIES(cTotTema,"|") BY 2:
    cLevNavn = DYNAMIC-FUNCTION("getFieldValues","LevBas",
                                "WHERE LevNr = " + DYNAMIC-FUNCTION("getFieldValues","VarebokTemaHode","WHERE VbTemeNr = " + ENTRY(ix + 1,cTotTema,"|"),"LevNr"),"LevNamn").
    cTema = cTema + ENTRY(ix,cTotTema,"|") + (IF cLevNavn NE ? THEN " / " + cLevNavn ELSE "")
          + "|" + ENTRY(ix + 1,cTotTema,"|") + "|".
  END.
END.
ELSE DO:
  DYNAMIC-FUNCTION("DoMessage",0,0,"Ingen tema er definert for messebok","","").
  RETURN.
END.

cTema = TRIM(cTema,"|").

IF CAN-DO("1",cAdgang) THEN DO:
  IF sokLevNr:SCREEN-VALUE IN FRAME frmLinje = "0" THEN DO:
    DYNAMIC-FUNCTION("DoMessage",0,0,"Du må velge en leverandør først","","").
    RETURN.
  END.
  
  IF cTema = "" THEN DO:
    DYNAMIC-FUNCTION("DoMessage",0,0,"Det er ikke definert noen tema for leverandøren","","").
    RETURN.
  END.
END.

IF hBrowseLinje:NUM-SELECTED-ROWS = 0 THEN DO:
  DYNAMIC-FUNCTION("DoMessage",0,0,"Velg en eller flere artikler som skal legges til et tema","","").
  RETURN.
END.

RUN JBoxDSimpleSelectList.w (cTema,?,OUTPUT ocValue).

IF ocValue NE ? THEN DO:
  DO ix = 1 TO hBrowseLinje:NUM-SELECTED-ROWS:
    IF hBrowseLinje:FETCH-SELECTED-ROW(ix) THEN
      cRowIdList = cRowIdList + hFieldMapLinje:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE + ",".
  END.

  IF NOT DYNAMIC-FUNCTION("runproc","varebehlinje_vedl_tema.p",STRING(hFieldMap:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE) + ";" + 
                                    TRIM(cRowIdList,",") + ";" + ocValue + ";yes",?) THEN 
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AnnulerBestForslRecord C-Win 
PROCEDURE AnnulerBestForslRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cRowIdList AS CHAR NO-UNDO.
   
IF ButikkNr:SCREEN-VALUE IN FRAME frmLinje = "0" OR ButikkNr:SCREEN-VALUE = ? THEN DO:
  DYNAMIC-FUNCTION("DoMessage",0,0,"En butikk må velges før operasjonen kan utføres","","").
  RETURN.
END.

IF hBrowseLinje:NUM-SELECTED-ROWS = 0 THEN DO:
  DYNAMIC-FUNCTION("DoMessage",0,0,"Velg en eller flere artikler der bestillingsforslag skal annuleres","","").
  RETURN.
END.

IF DYNAMIC-FUNCTION("DoMessage",0,1,"Bekreft at bestillingsforslag for " 
                                  + STRING(hBrowseLinje:NUM-SELECTED-ROWS) 
                                  + " artikler skal annuleres (evt. godkjente bestillinger slettes ikke)"                        
                               ,"","") = 1 THEN DO:
  DO ix = 1 TO hBrowseLinje:NUM-SELECTED-ROWS:
    IF hBrowseLinje:FETCH-SELECTED-ROW(ix) THEN
      cRowIdList = cRowIdList + hFieldMapLinje:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE + ",".
  END.
  
  IF DYNAMIC-FUNCTION("runproc","varebehlinje_annuler_forslag.p",TRIM(cRowIdList,",") + ";" + ButikkNr:SCREEN-VALUE + ";yes",?) THEN DO:
    DYNAMIC-FUNCTION("setCurrentObject",hBrowseLinje).
    RUN OpenQuery.
  END.
  ELSE DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
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
IF NOT VALID-HANDLE(hArtikkelkort) THEN
  RUN w-vartkor.w  PERSIST SET hArtikkelkort (DYNAMIC-FUNCTION("getRecId","ArtBas",hFieldMapLinje:BUFFER-FIELD("RowIdent2"):BUFFER-VALUE), "ENDRE," + STRING(THIS-PROCEDURE)).
ELSE
  RUN ByttArtikkel IN hArtikkelkort (hFieldMapLinje:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Bestilt1Record C-Win 
PROCEDURE Bestilt1Record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR ocValue     AS CHAR NO-UNDO.
DEF VAR iConfirm    AS INT NO-UNDO.

IF hBrowseTrans:NUM-SELECTED-ROWS = 0 THEN DO:
  DYNAMIC-FUNCTION("DoMessage",0,0,"Ingen bestillingsrader er valgt","","").
  RETURN.
END.
iReturn = 0.
RUN JBoxBrowseMsgUpdSelVal.w ("Oppdater bestilt antall, leveringsuke 1",
                              hBrowseTrans:NUM-SELECTED-ROWS,
                              DYNAMIC-FUNCTION("getAttribute",hBrowseTrans,"Totalcount"),
                              "DECIMAL|>>>>>9|Antall, levuke 1",
                              OUTPUT ocValue, 
                              OUTPUT iReturn).

IF iReturn NE 0 THEN
  EndreMangeTranser("Bestilt1",ocValue).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Bestilt2Record C-Win 
PROCEDURE Bestilt2Record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR ocValue     AS CHAR NO-UNDO.
DEF VAR iConfirm    AS INT NO-UNDO.

IF hBrowseTrans:NUM-SELECTED-ROWS = 0 THEN DO:
  DYNAMIC-FUNCTION("DoMessage",0,0,"Ingen bestillingsrader er valgt","","").
  RETURN.
END.

iReturn = 0.
RUN JBoxBrowseMsgUpdSelVal.w ("Oppdater bestilt antall, leveringsuke 2",
                              hBrowseTrans:NUM-SELECTED-ROWS,
                              DYNAMIC-FUNCTION("getAttribute",hBrowseTrans,"Totalcount"),
                              "DECIMAL|>>>>>9|Antall, levuke 2",
                              OUTPUT ocValue, 
                              OUTPUT iReturn).

IF iReturn NE 0 THEN
  EndreMangeTranser("Bestilt2",ocValue).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Bestilt3Record C-Win 
PROCEDURE Bestilt3Record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR ocValue     AS CHAR NO-UNDO.
DEF VAR iConfirm    AS INT NO-UNDO.

IF hBrowseTrans:NUM-SELECTED-ROWS = 0 THEN DO:
  DYNAMIC-FUNCTION("DoMessage",0,0,"Ingen bestillingsrader er valgt","","").
  RETURN.
END.

iReturn = 0.
RUN JBoxBrowseMsgUpdSelVal.w ("Oppdater bestilt antall, leveringsuke 3",
                              hBrowseTrans:NUM-SELECTED-ROWS,
                              DYNAMIC-FUNCTION("getAttribute",hBrowseTrans,"Totalcount"),
                              "DECIMAL|>>>>>9|Antall, levuke 3",
                              OUTPUT ocValue, 
                              OUTPUT iReturn).

IF iReturn NE 0 THEN
  EndreMangeTranser("Bestilt3",ocValue).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Bestilt4Record C-Win 
PROCEDURE Bestilt4Record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR ocValue     AS CHAR NO-UNDO.
DEF VAR iConfirm    AS INT NO-UNDO.

IF hBrowseTrans:NUM-SELECTED-ROWS = 0 THEN DO:
  DYNAMIC-FUNCTION("DoMessage",0,0,"Ingen bestillingsrader er valgt","","").
  RETURN.
END.

iReturn = 0.
RUN JBoxBrowseMsgUpdSelVal.w ("Oppdater bestilt antall, leveringsuke 4",
                              hBrowseTrans:NUM-SELECTED-ROWS,
                              DYNAMIC-FUNCTION("getAttribute",hBrowseTrans,"Totalcount"),
                              "DECIMAL|>>>>>9|Antall, levuke 4",
                              OUTPUT ocValue, 
                              OUTPUT iReturn).

IF iReturn NE 0 THEN
  EndreMangeTranser("Bestilt4",ocValue).

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
  RUN DynBildeGrid.p (hFieldMapLinje:TABLE-HANDLE,"FOR EACH Varebehlinje WHERE CAN-DO('" + TRIM(cArtNrList,",") + "',STRING(ArtikkelNr))","Artikkelnr").
END.
ELSE RUN DynBildeGrid.p (hFieldMapLinje:TABLE-HANDLE,"FOR EACH Varebehlinje","Artikkelnr").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BrukerGrTilgangRecord C-Win 
PROCEDURE BrukerGrTilgangRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cRowIdList     AS CHAR NO-UNDO.
DEF VAR cBrukerGrpList AS CHAR NO-UNDO.

cRowIdList = DYNAMIC-FUNCTION("getRowIdList","VareBhBrukerGrp,BrukerGrp","",
                              "WHERE VareBhBrukerGrp.VareBehNr = DECIMAL(" + STRING(hFieldMap:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE) + ")"
                             + ",FIRST BrukerGrp OF VareBhBrukerGrp NO-LOCK"
                              ).

THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
RUN JBoxSelector.w (THIS-PROCEDURE,0,
                    "BrukerGrp;BrGrpNr;Beskrivelse",
                    "where true",
                    INPUT-OUTPUT cRowIdList,
                    "BrGrpNr",
                    INPUT-OUTPUT cBrukerGrpList,
                    "","",
                    OUTPUT bOK).
THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

IF bOk AND NOT DYNAMIC-FUNCTION("runProc","varebeh_sett_brukergrp.p",STRING(hFieldMap:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE) + ";" + cRowidList,?) THEN
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ButOrdrebekrPrLevRecord C-Win 
PROCEDURE ButOrdrebekrPrLevRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR bEAN               AS LOG NO-UNDO.

DEF VAR cButikerRowIdList  AS CHAR NO-UNDO.
DEF VAR cButikerIdList     AS CHAR NO-UNDO.
DEF VAR cSourceFile        AS CHAR NO-UNDO.
DEF VAR cTargetFile        AS CHAR NO-UNDO.
DEF VAR hShell             AS COM-HANDLE NO-UNDO.
DEF VAR chExcelApplication AS COM-HANDLE NO-UNDO.
DEF VAR ix                 AS INT  NO-UNDO.

IF cLevNrListe = "*" AND cButikkListe = "*" THEN DO:
  IF NOT hFieldMap:AVAIL THEN RETURN.
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "Butiker;Butik;Butnamn"
                      + ",VarebehLinjeTrans;",
                        "WHERE true" 
                      + ",FIRST VarebehLinjeTrans NO-LOCK WHERE ButikkNr = Butiker.Butik AND VarebehNr = " 
                         + STRING(hFieldMap:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE)
                         + " AND ArtikkelNr > 0 AND GodkjentBestilling"
                         + " AND (Bestilt1 > 0 OR Bestilt2 > 0 OR Bestilt3 > 0 OR Bestilt4 > 0)"
                      ,INPUT-OUTPUT cButikerRowIdList,
                      "Butik",
                      INPUT-OUTPUT cButikerIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk THEN DO:
    DO ix = 1 TO NUM-ENTRIES(cButikerIdList,"|"):
      IF DYNAMIC-FUNCTION("runProc","varebeh_ordrebekr.p",
                          STRING(hFieldMap:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE) + "," +
                          ENTRY(ix,cButikerIdList,"|") + ",,levnamn,"
                          ,httRapport) THEN DO:
        IF cButBekrCat NE "" THEN DO:
          ASSIGN cSourceFile = getReportFile(DYNAMIC-FUNCTION("getTransactionMessage"))  
                 cTargetFile = RIGHT-TRIM(cButBekrCat,"\") + "\" + STRING(hFieldMap:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE) + "_butbekrPrLev_"                              
                             + ENTRY(ix,cButikerIdList,"|") + "_"
                             + REPLACE(DYNAMIC-FUNCTION("getFieldValues","Butiker","WHERE Butik = " + ENTRY(ix,cButikerIdList,"|"),"ButNamn"),'/','') 
                             + ".xls".

          IF bFormatReport THEN
            OrdreBekreftToExcel(cSourceFile,"but",ENTRY(ix,cButikerIdList,"|"),bEAN,"levnamn",cTargetFile).
          ELSE
            OS-COPY VALUE(ENTRY(1,cSourceFile,"|")) VALUE(cTargetFile).
        END.
        ELSE IF NUM-ENTRIES(cButikerIdList,"|") = 1 THEN
          OrdreBekreftToExcel(DYNAMIC-FUNCTION("getTransactionMessage"),"but",ENTRY(ix,cButikerIdList,"|"),bEAN,"levnamn","").
      END.
    END.
    IF cButBekrCat NE "" THEN DO:
      chExcelApplication = DYNAMIC-FUNCTION("getExcelHandle").
      chExcelApplication:QUIT().
      RELEASE OBJECT chExcelApplication NO-ERROR.
      CREATE "Shell.Application" hShell.
      hShell:Explore(cButBekrCat).      
    END.
  END.
    
END.
ELSE DO:

  IF NOT hFieldMapRegStat:AVAIL THEN RETURN.

  /* bEAN = IF DYNAMIC-FUNCTION("DoMessage",0,3,"Ta med EAN koder på utskriften?","","") = 6 THEN TRUE ELSE FALSE.  */
  
  IF DYNAMIC-FUNCTION("runProc","varebeh_ordrebekr.p",
                       STRING(hFieldMapRegStat:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE) + "," +
                       STRING(hFieldMapRegStat:BUFFER-FIELD("ButikkNr"):BUFFER-VALUE) + ",,levnamn," + (IF bEAN THEN "yes" ELSE "")
                      ,httRapport) THEN
    OrdreBekreftToExcel(DYNAMIC-FUNCTION("getTransactionMessage"),"but",STRING(hFieldMapRegStat:BUFFER-FIELD("ButikkNr"):BUFFER-VALUE),bEAN,"levnamn","").
  ELSE DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ButOrdrebekrRecord C-Win 
PROCEDURE ButOrdrebekrRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR bEAN               AS LOG  NO-UNDO.
DEF VAR cLevNr             AS CHAR NO-UNDO.
DEF VAR cLookupValue       AS CHAR NO-UNDO INIT "LevNr".

DEF VAR cButikerRowIdList  AS CHAR NO-UNDO.
DEF VAR cButikerIdList     AS CHAR NO-UNDO.
DEF VAR cSourceFile        AS CHAR NO-UNDO.
    DEF VAR cTargetFile        AS CHAR       NO-UNDO.
    DEF VAR hShell             AS COM-HANDLE NO-UNDO.
    DEF VAR chExcelApplication AS COM-HANDLE NO-UNDO.
    DEF VAR ix                 AS INT        NO-UNDO.

    IF cLevNrListe = "*" AND cButikkListe = "*" THEN 
    DO:
        IF NOT hFieldMap:AVAIL THEN RETURN.
  
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "Butiker;Butik;Butnamn"
                    + ",VarebehLinjeTrans;",
                      "WHERE true" 
                    + ",FIRST VarebehLinjeTrans NO-LOCK WHERE ButikkNr = Butiker.Butik AND VarebehNr = " 
                       + STRING(hFieldMap:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE)
                       + " AND ArtikkelNr > 0 AND GodkjentBestilling"
                       + " AND (Bestilt1 > 0 OR Bestilt2 > 0 OR Bestilt3 > 0 OR Bestilt4 > 0)"
                      ,INPUT-OUTPUT cButikerRowIdList,
                      "Butik",
                      INPUT-OUTPUT cButikerIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk THEN DO:
    DO ix = 1 TO NUM-ENTRIES(cButikerIdList,"|"):
      IF DYNAMIC-FUNCTION("runProc","varebeh_ordrebekr.p",
                          STRING(hFieldMap:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE) + "," +
                          ENTRY(ix,cButikerIdList,"|") + 
                          (IF NUM-ENTRIES(cButikerIdList,"|") > 1 THEN ",vis_butnr" ELSE ",") + ",,"
                          ,httRapport) THEN DO:
        IF cButBekrCat NE "" THEN DO:
          ASSIGN cSourceFile = getReportFile(DYNAMIC-FUNCTION("getTransactionMessage"))  
                 cTargetFile = RIGHT-TRIM(cButBekrCat,"\") + "\" + STRING(hFieldMap:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE) + "_butbekr_"                              
                             + ENTRY(ix,cButikerIdList,"|") + "_"
                             + REPLACE(DYNAMIC-FUNCTION("getFieldValues","Butiker","WHERE Butik = " + ENTRY(ix,cButikerIdList,"|"),"ButNamn"),'/','') 
                             + ".xls".

          IF bFormatReport THEN
            OrdreBekreftToExcel(cSourceFile,"but",ENTRY(ix,cButikerIdList,"|"),bEAN,"levnamn",cTargetFile).
          ELSE
            OS-COPY VALUE(ENTRY(1,cSourceFile,"|")) VALUE(cTargetFile).
        END.
        ELSE IF NUM-ENTRIES(cButikerIdList,"|") = 1 THEN
          OrdreBekreftToExcel(DYNAMIC-FUNCTION("getTransactionMessage"),"but",ENTRY(ix,cButikerIdList,"|"),bEAN,"","").
      END.
    END.
    IF cButBekrCat NE "" THEN DO:
      chExcelApplication = DYNAMIC-FUNCTION("getExcelHandle").
      chExcelApplication:QUIT().
      RELEASE OBJECT chExcelApplication NO-ERROR.
      CREATE "Shell.Application" hShell.
      hShell:Explore(cButBekrCat).      
    END.
  END.
    
END.
ELSE DO:
  IF NOT hFieldMapRegStat:AVAIL THEN RETURN.

  IF NUM-ENTRIES(cLevNrListe) > 1 THEN DO:
    RUN JBoxDLookup.w ("LevBas;Levnr;levnamn,varebehlinje;", 
                       "where true,FIRST VarebehLinje OF LevBas WHERE VarebehLinje.VarebehNr = " 
                     + STRING(hFieldMap:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE)
                     + " AND CAN-DO('" + cLevNrListe + "',STRING(VarebehLinje.LevNr))", 
                       INPUT-OUTPUT cLookupValue).
    IF cLookupValue NE "" THEN
      cLevNr = cLookupValue.
    ELSE RETURN.
  END.
  ELSE IF cLevNrListe NE "*" THEN cLevNr = cLevNrListe.
  
  
  /* bEAN = IF DYNAMIC-FUNCTION("DoMessage",0,3,"Ta med EAN koder på utskriften?","","") = 6 THEN TRUE ELSE FALSE.  */
  
  IF DYNAMIC-FUNCTION("runProc","varebeh_ordrebekr.p",
                       STRING(hFieldMapRegStat:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE) + "," +
                       STRING(hFieldMapRegStat:BUFFER-FIELD("ButikkNr"):BUFFER-VALUE) + "," + 
                       cLevNr + ",," +
                       (IF bEAN THEN "yes" ELSE "")
                      ,httRapport) THEN
    OrdreBekreftToExcel(DYNAMIC-FUNCTION("getTransactionMessage"),"but",STRING(hFieldMapRegStat:BUFFER-FIELD("ButikkNr"):BUFFER-VALUE),bEAN,"","").
  ELSE DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ButOrdreForslRecord C-Win 
PROCEDURE ButOrdreForslRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR bEAN AS LOG NO-UNDO.
DEF VAR cLevNr AS CHAR NO-UNDO.
DEF VAR cLookupValue AS CHAR NO-UNDO INIT "LevNr".

IF NOT hFieldMapRegStat:AVAIL THEN RETURN.

IF NUM-ENTRIES(cLevNrListe) > 1 OR cLevNrListe = "*" THEN DO:
  RUN JBoxDLookup.w ("LevBas;Levnr;levnamn,varebehlinje;", 
                     "where true,FIRST VarebehLinje OF LevBas WHERE VarebehLinje.VarebehNr = " 
                   + STRING(hFieldMap:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE)
                   + " AND CAN-DO('" + cLevNrListe + "',STRING(VarebehLinje.LevNr))", 
                     INPUT-OUTPUT cLookupValue).
  IF cLookupValue NE "" THEN
    cLevNr = cLookupValue.
  ELSE IF cLevNrListe NE "*" THEN RETURN.
END.
ELSE IF cLevNrListe NE "*" THEN cLevNr = cLevNrListe.


/* bEAN = IF DYNAMIC-FUNCTION("DoMessage",0,3,"Ta med EAN koder på utskriften?","","") = 6 THEN TRUE ELSE FALSE.  */

IF DYNAMIC-FUNCTION("runProc","varebeh_ordrebekr.p",
                     STRING(hFieldMapRegStat:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE) + "," +
                     STRING(hFieldMapRegStat:BUFFER-FIELD("ButikkNr"):BUFFER-VALUE) + "," + 
                     cLevNr + ",,forslag"
                     ,httRapport) THEN
  OrdreBekreftToExcel(DYNAMIC-FUNCTION("getTransactionMessage"),"butforslag",STRING(hFieldMapRegStat:BUFFER-FIELD("ButikkNr"):BUFFER-VALUE),?,"","").
ELSE DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CalenderAction C-Win 
PROCEDURE CalenderAction :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF iTab = 3 THEN APPLY "value-changed" TO hBrowseListe.
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

OCXFile = SEARCH( "varebehhode.wrx":U ).
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
ELSE MESSAGE "varebehhode.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CopyRecord C-Win 
PROCEDURE CopyRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF iTab = 1 THEN DO:
  TabStripChanged(12).
  DYNAMIC-FUNCTION("setCurrentObject",hUpdToolbar).
END.
ELSE IF iTab = 3 AND hFieldMapLinje:AVAIL THEN DO:  
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN ArtBasVedlikehold.w (THIS-PROCEDURE,"copy",OUTPUT bOk).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  RETURN.
END.

IF iTab NE 4 THEN RUN SUPER.

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
DEF VAR hCurrObject AS HANDLE NO-UNDO.

hCurrObject = DYNAMIC-FUNCTION("getCurrentObject").

CASE hCurrObject:
  WHEN hBrowseListe THEN DO:
    TabStripChanged(13).

/*     DO WITH FRAME frmDetalj:                             */
/*       APPLY "Entry" TO VarebehBeskrivelse.            .  */
/*     END.                                                 */
  END.

  WHEN hBrowseLinje THEN DO:
    IF cButikkListe = "*" AND cLevNrListe = "*" THEN
      RUN Artikkelkort.
/*     ELSE DYNAMIC-FUNCTION("DoMessage",0,0,"Du har ikke adgang til å åpne artikkelkort","","").  */
  END.

  WHEN hBrowseRegStatTrans THEN DO:
    TabStripChanged(13).
    ASSIGN Kode:SCREEN-VALUE IN FRAME frmLinje = hFieldMapRegStatTrans:BUFFER-FIELD("Kode"):BUFFER-VALUE
           ButikkNr:SCREEN-VALUE = STRING(hFieldMapRegStatTrans:BUFFER-FIELD("ButikkNr"):BUFFER-VALUE)
           .
    APPLY "choose" TO btnBlankLinjeFilter IN FRAME frmLinje.
    RUN StartLinjeTransReg.
  END.
END CASE.

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
ELSE IF iTab = 4 THEN
  hBrowseRegStatTrans:DESELECT-ROWS() NO-ERROR.
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
DEF VAR cWeekList   AS CHAR NO-UNDO.
DEF VAR iMousePosX  AS INT  NO-UNDO.
DEF VAR iMousePosY  AS INT  NO-UNDO.

IF iTab = 3 AND DYNAMIC-FUNCTION("getCurrentObject") = hBrowseLinje OR bStartSearch THEN DO WITH FRAME frmLinje:
  DYNAMIC-FUNCTION("DoLockWindow",THIS-PROCEDURE:CURRENT-WINDOW).
  DYNAMIC-FUNCTION("setAttribute",SESSION,"userkeepswindowlocked","yes").
  IF ButikkNr:SCREEN-VALUE NE ? AND ButikkNr:SCREEN-VALUE NE "0" AND hFieldMapLinje:AVAIL THEN DO:
    IF NOT DYNAMIC-FUNCTION("runproc","varebehlinje_gentrans.p",
                            hFieldMapLinje:BUFFER-FIELD("VarebehNr"):STRING-VALUE + ","
                            + ButikkNr:SCREEN-VALUE + ","
                            + hFieldMapLinje:BUFFER-FIELD("ArtikkelNr"):STRING-VALUE 
                            ,?) THEN 
      DYNAMIC-FUNCTION("DoMessage",0,1,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
    ELSE
      DYNAMIC-FUNCTION("setAttribute",hBrowseTrans,"parentlink",
                                     "WHERE Artikkelnr = " + STRING(hFieldMapLinje:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE)
                                    + " AND VarebehNr  = " + STRING(hFieldMapLinje:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE)
                                    + " AND ButikkNr   = " + ButikkNr:SCREEN-VALUE).

    IF hBestilt:BUFFER-VALUE AND DYNAMIC-FUNCTION("getLinkedObject",hBrowseLinje,"browseoverlay","from") = ? THEN DO:
      hBrwLinjeGodkjent = DYNAMIC-FUNCTION("NewBrowseToggle",
                        hBrowseLinje,
                        "GodkjentBest",
                        "GodkjentBest",
                        "").
      DYNAMIC-FUNCTION("CreateOverlayLink",hBrowseLinje,hBrwLinjeGodkjent,"GodkjentBest").
      DYNAMIC-FUNCTION("setAttribute",hBrwLinjeGodkjent,"refreshrow","yes").
      DYNAMIC-FUNCTION("setAttribute",hBrwLinjeGodkjent,"customUpdateValProc","=varebehlinje_godkjenn.p").
      DYNAMIC-FUNCTION("setAttribute",hBrwLinjeGodkjent,"BufferExtraFields","UpdButikknr").
    END.
    ELSE IF NOT hBestilt:BUFFER-VALUE AND VALID-HANDLE(hBrwLinjeGodkjent) THEN 
      DYNAMIC-FUNCTION("DeleteObject",hBrwLinjeGodkjent).
  END.
  ELSE DO:    
    DYNAMIC-FUNCTION("setAttribute",hBrowseTrans,"parentlink","WHERE FALSE").
    IF VALID-HANDLE(hBrwLinjeGodkjent) THEN hBrwLinjeGodkjent:HIDDEN = TRUE.
  END.

  DYNAMIC-FUNCTION("setCurrentObject",hBrowseLinje).
END.

IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowseListe AND hFieldMap:AVAIL THEN DO:
  RUN Weeknum.p (DATE(DYNAMIC-FUNCTION("getFieldValues","messe","WHERE MesseNr = " + STRING(hFieldMap:BUFFER-FIELD("MesseNr"):BUFFER-VALUE),"FraDato"))
                ,OUTPUT iStartWeek).
  RUN Weeknum.p (DATE(DYNAMIC-FUNCTION("getFieldValues","messe","WHERE MesseNr = " + STRING(hFieldMap:BUFFER-FIELD("MesseNr"):BUFFER-VALUE),"TilDato"))
                ,OUTPUT iEndWeek).

  ASSIGN iStartWeek = INT(SUBSTR(STRING(iStartWeek),3))
         iEndWeek = INT(SUBSTR(STRING(iEndWeek),3)).

  DO ix = iStartWeek TO iEndWeek:
    IF INT(SUBSTR(STRING(ix,"9999"),3)) > 0 AND INT(SUBSTR(STRING(ix,"9999"),3)) < 53 THEN
      cWeekList = cWeekList + SUBSTR(STRING(ix,"9999"),3) + SUBSTR(STRING(ix,"9999"),1,2) + ",".
  END.
  cWeekList             = TRIM(cWeekList,",").

  IF cWeekList NE cCurrWeekList THEN
    ASSIGN cCurrWeekList         = cWeekList
/*            stdLevUke1:LIST-ITEMS = ",," + cWeekList      */
/*            stdLevUke2:LIST-ITEMS = stdLevUke1:LIST-ITEMS */
/*            stdLevUke3:LIST-ITEMS = stdLevUke1:LIST-ITEMS */
/*            stdLevUke4:LIST-ITEMS = stdLevUke1:LIST-ITEMS */
           LevUke1:LIST-ITEMS    = cWeekList
           LevUke2:LIST-ITEMS    = LevUke1:LIST-ITEMS
           LevUke3:LIST-ITEMS    = LevUke1:LIST-ITEMS
           LevUke4:LIST-ITEMS    = LevUke1:LIST-ITEMS
           .

  ASSIGN 
/*          iColNumBeskr   = setFieldColor(INT(getFromNotat("merknadsfarge")))    */
/*          iColNumBestilt = setFieldColor(INT(getFromNotat("bestillingsfarge"))) */
         iColNumBestiltGodk  = setFieldColor(INT(getFromNotat("bestilling_godk")))
         iColNumBestiltForsl = setFieldColor(INT(getFromNotat("bestilling_forsl")))
/*          cPrintMark          = getFromNotat("markertlinjemerknad") */
         iBestVarsel         = INT(getFromNotat("bestillingsvarsel"))
         iBestVarsel         = IF iBestVarsel = 0 THEN 99 ELSE iBestVarsel.
  setTema().
END.
ELSE IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowseListe THEN
  cmbTema:LIST-ITEM-PAIRS = RIGHT-TRIM("|0").

RUN SUPER.

IF iTab = 3 THEN DO WITH FRAME frmLinje:
  IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowseLinje THEN DO:
    RUN VisMiniBilde IN hArtBilde 
        (IF hFieldMapLinje:AVAIL THEN hFieldMapLinje:BUFFER-FIELD("BildNr"):BUFFER-VALUE 
         ELSE 0).
                                     
    IF VALID-HANDLE(hArtikkelkort) THEN
      RUN ByttArtikkel IN hArtikkelkort (IF hFieldMapLinje:AVAIL THEN hFieldMapLinje:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE
                                         ELSE 0).
    IF bStartSearch THEN DO WITH FRAME frmLinje:
      hSearchLinje = DYNAMIC-FUNCTION("getLinkedObject",hBrowseLinje,"browse-search-field","from").
      hSearchLinje:HELP = cHelp.
      bStartSearch = FALSE.
    END.
    bWarning   = FALSE.
    SumAntOgVerdi().
    FinnLevUker().

    DYNAMIC-FUNCTION("setAttribute",SESSION,"userkeepswindowlocked","").
    DYNAMIC-FUNCTION("DoLockWindow",?).

  END.
  ELSE IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowseTrans THEN DO:
    DYNAMIC-FUNCTION("DoLockWindow",THIS-PROCEDURE:CURRENT-WINDOW).
    SumAntOgVerdi().
    DYNAMIC-FUNCTION("DoLockWindow",?).
  END.
END.

IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowseListe THEN
  DO WITH FRAME {&FRAME-NAME}:  
    IF hFieldMap:AVAIL THEN 
      ASSIGN KortNavn:SCREEN-VALUE           = hFieldMap:BUFFER-FIELD("KortNavn"):BUFFER-VALUE
             MesseBeskrivelse:SCREEN-VALUE   = hFieldMap:BUFFER-FIELD("MesseBeskrivelse"):BUFFER-VALUE
             MesseNr:SCREEN-VALUE            = hFieldMap:BUFFER-FIELD("MesseNr"):STRING-VALUE
             ProfilNr:SCREEN-VALUE           = hFieldMap:BUFFER-FIELD("ProfilNr"):STRING-VALUE
             VarebehBeskrivelse:SCREEN-VALUE = hFieldMap:BUFFER-FIELD("VarebehBeskrivelse"):BUFFER-VALUE
             VarebehNr:SCREEN-VALUE          = hFieldMap:BUFFER-FIELD("VarebehNr"):STRING-VALUE
             BeskrivelseVarebehType:SCREEN-VALUE = hFieldMap:BUFFER-FIELD("BeskrivelseVarebehType"):BUFFER-VALUE
             VarebehType:SCREEN-VALUE        = hFieldMap:BUFFER-FIELD("VarebehType"):STRING-VALUE
             .
    ELSE 
      ASSIGN KortNavn:SCREEN-VALUE           = ""
             MesseBeskrivelse:SCREEN-VALUE   = ""
             MesseNr:SCREEN-VALUE            = ""
             ProfilNr:SCREEN-VALUE           = ""
             VarebehBeskrivelse:SCREEN-VALUE = ""
             VarebehNr:SCREEN-VALUE          = ""
             BeskrivelseVarebehType:SCREEN-VALUE = ""
             Varebehtype:SCREEN-VALUE        = ""
             .
  END.

IF iTab = 4 THEN
  fiUtskrNr:SCREEN-VALUE IN FRAME frmRegStat = "".

IF iTab = 4 OR (hFieldMap:AVAIL AND hFieldMap:BUFFER-FIELD("Oppdatert"):BUFFER-VALUE = TRUE) THEN
  DYNAMIC-FUNCTION("setAttribute",hUpdToolbar,"disabledevents","delete" + IF iTab < 4 THEN ",flatview" ELSE "").
ELSE
  DYNAMIC-FUNCTION("setAttribute",hUpdToolbar,"disabledevents","flatview").

IF hFieldMap:AVAIL THEN
  SettHentUtvalgSensitive(IF VALID-HANDLE(hUtvalg) AND NOT hFieldMap:BUFFER-FIELD("Oppdatert"):BUFFER-VALUE THEN TRUE ELSE FALSE).

IF iTab = 3 THEN DO:
  iMousePosX = DYNAMIC-FUNCTION("getMousePosition",FRAME {&FRAME-NAME}:HANDLE,"x").
  iMousePosY = DYNAMIC-FUNCTION("getMousePosition",FRAME {&FRAME-NAME}:HANDLE,"y").

  IF iMousePosY > hBrowseTrans:Y THEN DO:
    IF VALID-HANDLE(hBrwTransBestilt4) AND hBrwTransBestilt4:SENSITIVE AND iMousePosX > hBrwTransBestilt3:X + hBrwTransBestilt3:WIDTH-PIXELS THEN
      DYNAMIC-FUNCTION("setWidgetEnter",hBrwTransBestilt4).
    ELSE IF VALID-HANDLE(hBrwTransBestilt3) AND hBrwTransBestilt3:SENSITIVE AND iMousePosX > hBrwTransBestilt2:X + hBrwTransBestilt2:WIDTH-PIXELS THEN
      DYNAMIC-FUNCTION("setWidgetEnter",hBrwTransBestilt3).
    ELSE IF VALID-HANDLE(hBrwTransBestilt2) AND hBrwTransBestilt2:SENSITIVE AND iMousePosX > hBrwTransBestilt1:X + hBrwTransBestilt1:WIDTH-PIXELS THEN
      DYNAMIC-FUNCTION("setWidgetEnter",hBrwTransBestilt2).
    ELSE IF VALID-HANDLE(hBrwTransBestilt1) AND hBrwTransBestilt1:SENSITIVE AND iMousePosX > hBrwTransBestilt1:X - hBrowseTrans:GET-BROWSE-COLUMN(3):WIDTH-PIXELS THEN
      DYNAMIC-FUNCTION("setWidgetEnter",hBrwTransBestilt1).
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
  DISPLAY VarebehNr VarebehBeskrivelse VarebehType BeskrivelseVarebehType 
          MesseNr MesseBeskrivelse ProfilNr KortNavn 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE rectToolBar rectUpdToolbar rectVarebeh VarebehNr VarebehBeskrivelse 
         BeskrivelseVarebehType MesseNr MesseBeskrivelse KortNavn 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  DISPLAY fi-fMesseNr rsSokGodkStatus fi-fProfilNr sokButikk 
          fi-cVarebehBeskrivelse 
      WITH FRAME frmFilter IN WINDOW C-Win.
  ENABLE RectArtSok fi-fMesseNr btnSokFiltMesseNr rsSokGodkStatus 
         btnSokFiltProfilNr fi-fProfilNr sokButikk fi-cVarebehBeskrivelse 
         btnBlankFilter 
      WITH FRAME frmFilter IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-frmFilter}
  DISPLAY tbKjedeVare sokBestVerdi sokAvdelingNr sokAvdelingNavn sokHg 
          sokHgBeskr sokVg sokVgBeskr sokAktNr sokAktBeskrivelse sokLevNr 
          sokLevNamn sokBeskr sokLevKod sokLinjeMerknad cmbTema rsBestilling 
          rsGodkjent sokFraEdato sokTilEdato fiButTotal ButikkNr stdLevUke1 
          stdLevUke2 stdLevUke3 stdLevUke4 sokArtikkelNr fiTimeTot LevUke1 
          LevUke2 LevUke3 LevUke4 fiSumAnt fiSumVerdi tbGjennomfaktureres Kode 
      WITH FRAME frmLinje IN WINDOW C-Win.
  ENABLE tbKjedeVare sokAvdelingNr sokAvdelingNavn btnSplitBarY sokHg 
         sokHgBeskr sokVg sokVgBeskr sokAktNr sokAktBeskrivelse btnButTotal 
         sokLevNr sokLevNamn sokBeskr sokLevKod cmbTema rsBestilling 
         sokFraEdato sokTilEdato fiButTotal btnTema btnBlankLinjeFilter 
         ButikkNr stdLevUke1 stdLevUke2 stdLevUke3 stdLevUke4 sokArtikkelNr 
         fiTimeTot LevUke1 LevUke2 LevUke3 LevUke4 fiSumAnt btnAvdeling 
         btnHuvGr btnVarGr btnVarGr-2 btnLev fiSumVerdi btnLev-2 btnCalFraDato 
         btnCalTilDato btnCalLevUke1 btnCalLevUke4 btnCalLevUke3 btnCalLevUke2 
         tbGjennomfaktureres Kode RectBrowseSearchLinje rectBrowseLinje 
         rectBrowseTrans rectButTotal rectTBvarebehlinjetrans ArtikkelBilde 
      WITH FRAME frmLinje IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-frmLinje}
  DISPLAY sokButikkNr sokButNamn rsForslagGodk rsAlleReg tbVisLevuker 
      WITH FRAME frmRegStat IN WINDOW C-Win.
  ENABLE rectRegStat rectRegStatTrans rectRegStatTransToolbar sokButikkNr 
         sokButNamn btnButnr btnAlleButSam btnAlleBut rsForslagGodk rsAlleReg 
         tbVisLevuker 
      WITH FRAME frmRegStat IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-frmRegStat}
  DISPLAY VarebehNr OppdatDato OppdatAv Oppdatert VarebehType 
          BeskrivelseVarebehType ProfilNr KortNavn VarebehBeskrivelse MesseNr 
          MesseBeskrivelse Kilde VareBokBeskrivelse VarebehNotat 
      WITH FRAME frmDetalj IN WINDOW C-Win.
  ENABLE btnSokMesseNr RECT-1 Oppdatert VarebehType BeskrivelseVarebehType 
         ProfilNr VarebehBeskrivelse MesseNr Kilde VareBokBeskrivelse 
         VarebehNotat btnSokProfilNr 
      WITH FRAME frmDetalj IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-frmDetalj}
  ENABLE rectBrowseListe RectBrowseSearchListe 
      WITH FRAME frmListe IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-frmListe}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExcelRecord C-Win 
PROCEDURE ExcelRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cLookupValue       AS CHAR NO-UNDO.
DEF VAR chExcelApplication AS COM-HANDLE NO-UNDO.
DEF VAR chWorkSheet        AS COM-HANDLE NO-UNDO.
DEF VAR cFileName          AS CHAR NO-UNDO.

IF hFieldMap:AVAIL AND DYNAMIC-FUNCTION("getCurrentObject") = hLinjeTransToolbar THEN DO WITH FRAME frmLinje:

  IF cLevNrListe NE "*" AND NUM-ENTRIES(cLevNrListe) > 1 THEN DO:
    cLookupValue = "Levnr".  
    RUN JBoxDLookup.w ("LevBas;Levnr;levnamn,varebehlinje;", 
                       "where true,FIRST VarebehLinje OF LevBas WHERE VarebehLinje.VarebehNr = " 
                     + STRING(hFieldMap:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE)
                     + " AND CAN-DO('" + cLevNrListe + "',STRING(VarebehLinje.LevNr))", 
                       INPUT-OUTPUT cLookupValue).
  
    IF cLookupValue = "" THEN RETURN.     
  END.
  ELSE cLookupValue = cLevNrListe.

  IF DYNAMIC-FUNCTION("runProc","varebeh_sammendrag.p",
                       STRING(hFieldMap:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE) + ";" + ButikkNr:SCREEN-VALUE + ";" + cLookupValue
                      ,?) THEN DO:

    cFileName = DYNAMIC-FUNCTION("getTransactionMessage").
    chExcelApplication = DYNAMIC-FUNCTION("getExcelHandle").
    IF NOT VALID-HANDLE(chExcelApplication) THEN DO:
      IF DYNAMIC-FUNCTION("setWebDoc","Open",cFileName) NE "" THEN
        MESSAGE "Could not open file: " cFileName VIEW-AS ALERT-BOX ERROR.
      RETURN.
    END.
    ELSE DO:
      chExcelApplication:Workbooks:OpenText(cFileName,2,,,,,TRUE).
      chWorkSheet = chExcelApplication:Sheets:ITEM(1).
      chWorkSheet:Rows(1):FONT:Bold = TRUE.
      chWorkSheet:Columns("A:A"):FONT:Bold = TRUE.
      chWorkSheet:Range("C:C"):NumberFormat = "# ##0,00".
      chWorkSheet:Range("E:E"):NumberFormat = "# ##0,00".
      chWorkSheet:Columns("A:E"):AutoFit().
      chExcelApplication:VISIBLE = TRUE.
    END.
  END.
  ELSE DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
END.
ELSE RUN SUPER.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExcelSheetParams C-Win 
PROCEDURE ExcelSheetParams :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       13.01.06: Ikke lenger i bruk
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihOutputObject     AS HANDLE NO-UNDO.
DEF INPUT PARAM chExcelApplication AS COM-HANDLE NO-UNDO.
DEF INPUT PARAM chWorkbook         AS COM-HANDLE NO-UNDO.
DEF INPUT PARAM chWorksheet        AS COM-HANDLE NO-UNDO.
DEF INPUT PARAM iCount             AS INTEGER NO-UNDO.

DEF VAR cRange   AS CHAR NO-UNDO.
DEF VAR cValue   AS CHAR NO-UNDO.
DEF VAR hQuery   AS HANDLE NO-UNDO.
DEF VAR hBuffer  AS HANDLE NO-UNDO.
DEF VAR iUtskrNr AS INT NO-UNDO.
DEF VAR bNyUtskr AS LOG NO-UNDO.

RETURN.

/* TN 12/9-17 Har kommentert ut denne kdoen, da den aldri kan bli kjørt. Gir feil ved kompilering. 
IF ihOutputObject = hBrowseRegStatTrans THEN DO:

  IF bAlleTrans THEN DO:
    bAlleTrans = FALSE.
    RETURN.
  END.

  CREATE QUERY hQuery.
  CREATE BUFFER hBuffer FOR TABLE hFieldMapRegStatTrans.
  hQuery:SET-BUFFERS(hBuffer).
  hQuery:QUERY-PREPARE("FOR EACH " + hBuffer:NAME + " BY UtskrNr DESC").
  hQuery:QUERY-OPEN().
  hQuery:GET-FIRST().
  IF hBuffer:AVAIL THEN
    iUtskrNr = hBuffer:BUFFER-FIELD("UtskrNr"):BUFFER-VALUE + 1.
  DELETE OBJECT hBuffer.
  DELETE OBJECT hQuery.

  chWorkSheet:Range("A1:Z1"):rowheight = chWorkSheet:Range("A1:Z1"):rowheight * 2.
  chWorkSheet:Range("A1:A1"):WrapText = TRUE.
  chWorkSheet:Range("A:A"):columnwidth = chWorkSheet:Range("A:A"):columnwidth * .6.
  chWorkSheet:Range("D1:D1"):WrapText = TRUE.
  chWorkSheet:Range("D:D"):columnwidth = chWorkSheet:Range("A:A"):columnwidth * .6.
  chWorkSheet:Range("G1:G1"):WrapText = TRUE.
  chWorkSheet:Range("G:G"):columnwidth = chWorkSheet:Range("A:A"):columnwidth * .6.
  chWorkSheet:Range("I1:I1"):WrapText = TRUE.
  chWorkSheet:Range("I:I"):columnwidth = chWorkSheet:Range("A:A"):columnwidth * .6.
  chWorkSheet:Range("J1:J1"):WrapText = TRUE.
  chWorkSheet:Range("J:J"):columnwidth = chWorkSheet:Range("A:A"):columnwidth * .6.
  chWorkSheet:Range("K1:K1"):WrapText = TRUE.
  chWorkSheet:Range("K:K"):columnwidth = chWorkSheet:Range("A:A"):columnwidth * .6.

  chWorkSheet:Range("W1:AF" + STRING(iCount)):DELETE().

  DO ix = 2 TO iCount:
    ASSIGN cRange = "L" + STRING(ix) + ":L" + STRING(ix)
           cValue = ENTRY(1,STRING(chWorkSheet:Range(cRange):VALUE))
           cValue = FILL("0",13 - LENGTH(cValue)) + cValue
           .
    bOK = hFieldMapRegStatTrans:FIND-FIRST("WHERE Kode = '" + cValue + "'").
    IF bOK AND hFieldMapRegStatTrans:BUFFER-FIELD("UtskrNr"):BUFFER-VALUE = 0 THEN DO:
      bNyUtskr = TRUE.
      hFieldMapRegStatTrans:BUFFER-FIELD("UtskrNr"):BUFFER-VALUE = iUtskrNr.
      DYNAMIC-FUNCTION("DoUpdate","VarebehLinjeTrans",
                    "ignore",
                    "",
                    hFieldMapRegStatTrans:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE,
                    "UtskrNr",
                    STRING(iUtskrNr),
                    FALSE).

    END.
    chWorkSheet:Range(cRange):VALUE = ean13bc(cValue).
  END.

  IF NOT bNyUtskr THEN iUtskrNr = iUtskrNr - 1.

  cRange = "L2:L" + STRING(iCount).
  ASSIGN chWorkSheet:Range(cRange):FONT:NAME = "EAN-13B Half Height"
         chWorkSheet:Range(cRange):FONT:SIZE = 42
         .
  chWorkSheet:Columns("L"):AutoFit().
  IF NOT hFieldMapRegStat:AVAIL THEN
    hBrowseRegStat:SELECT-ROW(hBrowseRegStat:FOCUSED-ROW).

  chWorkSheet:PageSetup:LeftHeader     = STRING(hFieldMapRegStat:BUFFER-FIELD("ButikkNr"):BUFFER-VALUE) + " " + hFieldMapRegStat:BUFFER-FIELD("ButNamn"):BUFFER-VALUE.
  chWorkSheet:PageSetup:CenterHeader   = "Ordrebekreftelse. Utskrift nr " + STRING(iUtskrNr).
  chWorkSheet:PageSetup:RightHeader    = "Signatur: ______________________________________".
  chWorkSheet:PageSetup:PrintGridLines = TRUE.
  chWorkSheet:PageSetup:Zoom           = 65.
  chWorkSheet:PageSetup:LeftMargin     = 50.
  chWorkSheet:PageSetup:RightMargin    = 40.
  chWorkSheet:PageSetup:TopMargin      = 55.
  chWorkSheet:PageSetup:BottomMargin   = 50.
  chWorkSheet:PageSetup:PrintTitleRows = "$1:$1".
  chWorkSheet:PageSetup:RightFooter    = "Side &P av &N". 
  
  chWorkSheet:Range("T" + STRING(iCount + 2)):FormulaR1C1 = "Sum ordre".
  chWorkSheet:Range("V" + STRING(iCount + 2)):FormulaR1C1 = "=SUM(R[-" + STRING(iCount) + "]C:R[-1]C)".
  chWorkSheet:Range("T" + STRING(iCount + 2) + ":V" + STRING(iCount + 2)):Font:Bold = TRUE.
  chWorkSheet:Range("V:V"):NumberFormat = "# ##0,0".

  DYNAMIC-FUNCTION("setAttribute",ihOutputObject,"excelpreview","yeah").

  bOK = DYNAMIC-FUNCTION("DoCommit",FALSE).
  IF bOk THEN
    hBrowseRegStatTrans:REFRESH().
  ELSE
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil i oppdatering av utskr.nr","").
END.
----------------- */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExtraFlatViewRecord C-Win 
PROCEDURE ExtraFlatViewRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT  PARAM ihFlatView AS HANDLE NO-UNDO.
DEF OUTPUT PARAM obOk       AS LOG NO-UNDO INIT TRUE.

DEF VAR hFlatBrw AS HANDLE NO-UNDO.

hFlatBrw = DYNAMIC-FUNCTION("getBrowseHandle" IN ihFlatView).
DYNAMIC-FUNCTION("setAttribute",hFlatBrw,"getrecordcount","no").
DO WITH FRAME frmRegStat:
  IF rsForslagGodk = 1 THEN 
    DYNAMIC-FUNCTION("setAttribute",hFlatBrw,"calcparamGodkjentBest","").
  ELSE IF rsForslagGodk = 2 THEN 
    DYNAMIC-FUNCTION("setAttribute",hFlatBrw,"calcparamGodkjentBest","no").
  ELSE 
    DYNAMIC-FUNCTION("setAttribute",hFlatBrw,"calcparamGodkjentBest","yes").
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExtraStartSearch C-Win 
PROCEDURE ExtraStartSearch :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT  PARAM ihBrowse  AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSortCol AS CHAR   NO-UNDO.
DEF INPUT  PARAM ibDesc    AS LOG    NO-UNDO.
DEF OUTPUT PARAM obOk      AS LOG    NO-UNDO INIT YES.

IF ihBrowse = hBrowseLinje AND cmbTema:SCREEN-VALUE IN FRAME frmLinje NE "0" AND cmbTema:SCREEN-VALUE NE ? THEN 
  obOk = NO.

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
DEF VAR hFlatView AS HANDLE NO-UNDO.
DEF VAR hFlatBrw  AS HANDLE NO-UNDO.

RUN SUPER.

hFlatView = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hUpdToolBar,"flatviewhandle")) NO-ERROR.

hFlatBrw  = DYNAMIC-FUNCTION("getBrowseHandle" IN hFlatView).
{incl/dynfilterlookups_art.i}

DYNAMIC-FUNCTION("setAttribute",hFlatBrw,"availdistinctcolumns",
                  "VarebehNr,ArtikkelNr,ButikkNr,clButikkNr,levKod,LevNr,LevNamn,Vg,VgBeskr,Hg,HgBeskr,AvdelingsNr,AvdelingsNavn,Ordrenr,OrdreStatus,CL").
DYNAMIC-FUNCTION("setAttribute",hFlatBrw,"availaccumfields",
                  "SumAntall,OrdreTot").

DYNAMIC-FUNCTION("setAttribute",hFlatBrw,"accumdatatypes","DECIMAL,INTEGER").
DYNAMIC-FUNCTION("setAttribute",hFlatBrw,"distinctdatatypes","CHARACTER,DECIMAL,INTEGER,DATE").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ForecastLevRecord C-Win 
PROCEDURE ForecastLevRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cLookupValue       AS CHAR NO-UNDO.
DEF VAR bEAN               AS LOG  NO-UNDO.
DEF VAR cReturn            AS CHAR NO-UNDO.
DEF VAR cLevBasRowIdList   AS CHAR NO-UNDO.
DEF VAR cLevBasIdList      AS CHAR NO-UNDO.
DEF VAR cSourceFile        AS CHAR NO-UNDO.
DEF VAR cTargetFile        AS CHAR NO-UNDO.
DEF VAR hShell             AS COM-HANDLE NO-UNDO.
DEF VAR chExcelApplication AS COM-HANDLE NO-UNDO.
DEF VAR chWorkBook         AS COM-HANDLE NO-UNDO.
DEF VAR chWorkSheet        AS COM-HANDLE NO-UNDO.

THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
RUN JBoxSelector.w (THIS-PROCEDURE,0,
                    "LevBas;levnr;levnamn"
                  + ",VarebehLinje;!VarebehNr",
                    "WHERE true" 
                  + ",FIRST VarebehLinje OF LevBas NO-LOCK WHERE VarebehNr = " + STRING(hFieldMap:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE)
                    ,INPUT-OUTPUT cLevBasRowIdList,
                    "Levnr",
                    INPUT-OUTPUT cLevBasIdList,
                    "","",
                    OUTPUT bOK).
THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

IF bOk THEN DO:
  DO ix = 1 TO NUM-ENTRIES(cLevBasIdList,"|"):
    IF DYNAMIC-FUNCTION("runProc","varebeh_forecastlev.p",
                         STRING(hFieldMap:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE) + "," + ENTRY(ix,cLevBasIdList,"|") + ",godkjent," + cLevBekrUtvalg
                        ,httRapport) THEN DO:
      IF cLevBekrCat NE "" THEN DO:
        ASSIGN cSourceFile = DYNAMIC-FUNCTION("getTransactionMessage")
               cTargetFile = RIGHT-TRIM(cLevBekrCat,"\") + "\" + STRING(hFieldMapRegStat:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE) + "_levforecast_" + ENTRY(ix,cLevBasIdList,"|") + "_"
                           + REPLACE(DYNAMIC-FUNCTION("getFieldValues","LevBas","WHERE LevNr = " + ENTRY(ix,cLevBasIdList,"|"),"LevNamn"),'/','')
                           + ".xls".
        IF cSourceFile NE "" THEN DO:     
          IF bFormatReport THEN
            LevForecastToExcel(cSourceFile,"godkjent",ENTRY(ix,cLevBasIdList,"|"),cTargetFile).
          ELSE
            OS-COPY VALUE(ENTRY(1,cSourceFile,"|")) VALUE(cTargetFile).
        END.
      END.
      ELSE IF cLevBekrCat = "" AND NUM-ENTRIES(cLevBasIdList,"|") = 1 THEN
        LevForecastToExcel(DYNAMIC-FUNCTION("getTransactionMessage"),"godkjent",ENTRY(ix,cLevBasIdList,"|"),"").
    END.
    ELSE IF NUM-ENTRIES(cLevBasIdList,"|") = 1 THEN 
      DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
  END.
  IF cLevBekrCat NE "" THEN DO:
    chExcelApplication = DYNAMIC-FUNCTION("getExcelHandle").
    chExcelApplication:QUIT().
    RELEASE OBJECT chExcelApplication NO-ERROR.
    CREATE "Shell.Application" hShell.
    hShell:Explore(cLevBekrCat).      
    RELEASE OBJECT hShell NO-ERROR.
  END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GenBestCLrecord C-Win 
PROCEDURE GenBestCLrecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cButikerIdList      AS CHAR NO-UNDO.
DEF VAR cButikerRowIdList   AS CHAR NO-UNDO.
DEF VAR iReturn             AS INT  NO-UNDO.
DEF VAR cLevNrListe         AS CHAR NO-UNDO.
DEF VAR cArtikkelNr         AS CHAR NO-UNDO INIT "0".
DEF VAR bRegenIkkeSendt     AS LOG  NO-UNDO.
DEF VAR bSendOrdre          AS LOG  NO-UNDO.

IF NOT hFieldMap:AVAIL THEN RETURN.

THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
RUN JBoxSelector.w (THIS-PROCEDURE,0,
                    "Butiker;Butik;Butnamn;Sentrallager,VarebehLinjeTHode;godkjent"
                   ,"where Sentrallager and NedlagtDato = ?,first VarebehLinjeThode where Varebehnr = " 
                      + STRING(hFieldMap:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE) 
                      + " AND ButikkNr = Butik OUTER-JOIN BY Butik"
                   ,INPUT-OUTPUT cButikerRowIdList,
                    "Butik",
                    INPUT-OUTPUT cButikerIdList,
                    "","",
                    OUTPUT bOK).
THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

IF bOk THEN DO:
  RUN dOrdreGenFilter.w (OUTPUT cLevNrListe,
                         OUTPUT cArtikkelNr,
                         OUTPUT bRegenIkkeSendt,
                         OUTPUT bSendOrdre,
                         OUTPUT bOk).
  IF NOT bOk THEN RETURN.

  IF NOT DYNAMIC-FUNCTION("runproc","genvarebehmesse_best.p",
                          STRING(hFieldMap:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE) + ","
                        + (IF bSendOrdre THEN "send" ELSE "") + ","
                        + DYNAMIC-FUNCTION("getASuserId") + ","
                        + cButikerIdList + ","
                        + cLevNrListe + ","
                        + "0," 
/*                           + cArtikkelNr + "," */
                        + (IF bRegenIkkeSendt THEN "regen" ELSE "")
                         ,?) THEN
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").


/*   iReturn = DYNAMIC-FUNCTION("DoMessage",0,3,"Skal ordre også settes til sendt?","",""). */
/*   IF iReturn = 2 THEN RETURN.                                                            */

/*   IF NOT DYNAMIC-FUNCTION("runproc","genvarebehmesse_best.p",                            */
/*                           STRING(hFieldMap:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE) + "," */
/*                         + (IF iReturn = 6 THEN "send" ELSE "") + ","                     */
/*                         + DYNAMIC-FUNCTION("getASuserId") + ","                          */
/*                         + cButikerIdList,?) THEN                                         */
/*     DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").   */
END.


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
DEF OUTPUT PARAM ocFarge AS CHAR NO-UNDO INIT "varebeh".
DEF OUTPUT PARAM ocKrit  AS CHAR NO-UNDO.

DO WITH FRAME frmFilter:
  IF iTab < 3 THEN DO:
    IF sokButikk:SCREEN-VALUE NE "0" OR rsSokGodkStatus:SCREEN-VALUE > "1" THEN
      ocKrit = "global," + sokButikk:SCREEN-VALUE + "|" + rsSokGodkStatus:SCREEN-VALUE.
  END.
  ELSE IF hFieldMap:AVAIL THEN ocKrit = STRING(hFieldMap:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE).
END.
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

cCurrSelectBuffer = ihSelectorSource:QUERY:GET-BUFFER-HANDLE(1):NAME.
iSelectorSourcCount = INT(DYNAMIC-FUNCTION("getAttribute",ihSelectorSource,"totalcount")).


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

ELSE IF VALID-HANDLE(hLevBekrUtvalgPanel) THEN DO:    
  ASSIGN cLevBekrCat    = DYNAMIC-FUNCTION("getFilOmr" IN hLevBekrUtvalgPanel)
         cLevBekrUtvalg = DYNAMIC-FUNCTION("getUtvalg" IN hLevBekrUtvalgPanel)
         bFormatReport  = LOGICAL(DYNAMIC-FUNCTION("getFormatFiles" IN hLevBekrUtvalgPanel)).

  IF cLevBekrCat NE "" THEN DO:      
    FILE-INFO:FILE-NAME = cLevBekrCat.
    IF FILE-INFO:FILE-TYPE NE "drw" THEN DO:
      DYNAMIC-FUNCTION("DoMessage",0,0,"Angitt filområde eksisterer ikke. Filer blir lagt under " + SESSION:TEMP-DIRECTORY,"","").
      cLevBekrCat = "". 
    END.
  END.
  IF cLevBekrUtvalg = "" OR cLevBekrUtvalg = ? THEN cLevBekrUtvalg = "0".
END.

ELSE IF VALID-HANDLE(hButBekrUtvalgPanel) THEN DO:    
  ASSIGN cButBekrCat = DYNAMIC-FUNCTION("getFilOmr" IN hButBekrUtvalgPanel)
         bFormatReport  = LOGICAL(DYNAMIC-FUNCTION("getFormatFiles" IN hButBekrUtvalgPanel)).

  IF cButBekrCat NE "" THEN DO:      
    FILE-INFO:FILE-NAME = cButBekrCat.
    IF FILE-INFO:FILE-TYPE NE "drw" THEN DO:
      DYNAMIC-FUNCTION("DoMessage",0,0,"Angitt filområde eksisterer ikke. Filer blir lagt under " + SESSION:TEMP-DIRECTORY,"","").
      cButBekrCat = "". 
    END.
  END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GodkjennBestRecord C-Win 
PROCEDURE GodkjennBestRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cRowIdList AS CHAR NO-UNDO.
   
IF ButikkNr:SCREEN-VALUE IN FRAME frmLinje = "0" OR ButikkNr:SCREEN-VALUE = ? THEN DO:
  DYNAMIC-FUNCTION("DoMessage",0,0,"En butikk må velges før bestillinger kan godkjennes","","").
  RETURN.
END.

IF hBrowseLinje:NUM-SELECTED-ROWS = 0 THEN DO:
  DYNAMIC-FUNCTION("DoMessage",0,0,"Velg en eller flere artikler som skal godkjennes","","").
  RETURN.
END.

IF DYNAMIC-FUNCTION("DoMessage",0,1,"Bekreft at bestillinger for " + STRING(hBrowseLinje:NUM-SELECTED-ROWS) + " artikler skal godkjennes","","") = 1 THEN DO:
  DO ix = 1 TO hBrowseLinje:NUM-SELECTED-ROWS:
    IF hBrowseLinje:FETCH-SELECTED-ROW(ix) THEN
      cRowIdList = cRowIdList + hFieldMapLinje:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE + ",".
  END.
  
  IF DYNAMIC-FUNCTION("runproc","varebehlinje_vedl_godkjenning.p",TRIM(cRowIdList,",") + ";" + ButikkNr:SCREEN-VALUE + ";yes",?) THEN DO:
    DYNAMIC-FUNCTION("setCurrentObject",hBrowseLinje).
    RUN OpenQuery.
  END.
  ELSE DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
END.
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
  RUN OverfUtvalgTilVbeh IN hUtvalg (hFieldMap:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE, OUTPUT bOK).
  IF bOK THEN DO: 
    IF iTab < 3 THEN
      TabStripChanged(13).
    ELSE APPLY "value-changed" TO hBrowseListe.
  END.
END.
ELSE IF NOT VALID-HANDLE(hUtvalg) THEN
  DYNAMIC-FUNCTION("DoMessage",0,0,"Utvalg ikke tilgjengelig","","").
ELSE IF hFieldMap:AVAIL THEN 
  DYNAMIC-FUNCTION("DoMessage",0,0,"Varebeh &1 er oppdatert og kan ikke tilføres flere artikler","",STRING(hFieldMap:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE)).
ELSE 
  DYNAMIC-FUNCTION("DoMessage",0,0,"Ingen Varebeh valgt","","").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitLinje C-Win 
PROCEDURE InitLinje :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR hExcelTransBtn  AS HANDLE NO-UNDO.
DEF VAR cDefaultSort    AS CHAR   NO-UNDO.

DO WITH FRAME frmLinje:
  ASSIGN cVarebehLinjeJoin = ",FIRST ArtBas OF VarebehLinje NO-LOCK"
                           + ",FIRST Varemerke OF ArtBas NO-LOCK OUTER-JOIN"
                           + ",FIRST SaSong OF ArtBas NO-LOCK"
         cAktivitetJoin    = ",FIRST VgAkt WHERE VgAkt.Vg = VarebehLinje.Vg OUTER-JOIN"
                           + ",FIRST Aktivitet OF VgAkt OUTER-JOIN"

         cAktivitetJoin    = ""  /* <--Merk */
         ButikkNr:DELIMITER = "|"
         ButikkNr:LIST-ITEM-PAIRS = RIGHT-TRIM("|0|" + DYNAMIC-FUNCTION("GetFieldList","Butiker;Butik|ButNamn;Butik","WHERE CAN-DO('" + cButikkListe + "',STRING(Butik)) AND NedlagtDato = ? BY ButNamn"),"|")

         ButikkNr:SCREEN-VALUE = ButikkNr:ENTRY(1)

         cmbTema:DELIMITER = "|"
         iGrayColor        = setFieldColor(RGB-VALUE(220,220,220))
         iFontGodkjent     = DYNAMIC-FUNCTION("setAppFont","Wingdings, size=11 Script=symbol","skotex")
         stdLevUke1:HIDDEN = YES
         stdLevUke2:HIDDEN = YES
         stdLevUke3:HIDDEN = YES
         stdLevUke4:HIDDEN = YES
         Kode:SIDE-LABEL-HANDLE:FONT = 6
         Kode:SIDE-LABEL-HANDLE:X = Kode:SIDE-LABEL-HANDLE:X - 8
         .

  IF cButikkListe NE "*" THEN btnTema:SENSITIVE = FALSE.

  IF cLevNrListe NE "*" THEN DO:
    IF NUM-ENTRIES(cLevNrListe) > 1 THEN
      cLevBasIdList = cLevNrListe.
    ELSE
      ASSIGN sokLevNr:SCREEN-VALUE = cLevNrListe
             sokLevNamn:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","LevBas;LevNamn","WHERE LevNr = " + sokLevNr:SCREEN-VALUE).
    ASSIGN sokLevNr:SENSITIVE   = FALSE
           sokLevNamn:SENSITIVE = FALSE.
/*            btnLev:SENSITIVE     = FALSE. */
  END.

  RUN VisMiniBilde.w PERSIST SET hArtBilde.
  RUN InitializeObject IN hArtBilde (ArtikkelBilde:HANDLE).
  hArtBildeFrame = DYNAMIC-FUNCTION("getFrameHandle" IN hArtBilde).

/*   DYNAMIC-FUNCTION("setAttribute",rectBrowseLinje:HANDLE,"1stSortColumn","LevNamn").  */
/*   DYNAMIC-FUNCTION("setAttribute",rectBrowseLinje:HANDLE,"2ndSortColumn","Beskr").    */

  hBrowseLinje = DYNAMIC-FUNCTION("NewBrowse",         
                    rectBrowseLinje:HANDLE,  
                    1000,                          
                    "NUM-LOCKED-COLUMNS|5,MULTIPLE",  
                    "VarebehLinje" 
                      + ";Sortering|Sekv|>>>>9 "
                      + ";+GodkjentBest|LOGICAL|*/|varebehlinje_bestforslag(ROWID)|Godkj"
                      + ";LevNamn|Lev.navn|x(30)"
                      + ";ArtikkelNr|SE Art.nr  |zzzzzzzzzz9 "
                      + ";LevKod| Lev.art.nr| x(20)"
                      + ";Beskr|Art.navn|x(30)"
                      + ";LevFargKod|Lev.farge|x(20)"
                      + ";Vg|VgNr"
                      + ";VgBeskr|Vg.beskr"
                      + ";InnkjopsPris|Innpris"
                      + ";Varekost|Nto.Innpris"
                      + ";Pris|Markedspris"    
                      + ";supVareKost|Nto.supl"
                      + ";DBkr"
                      + ";DB%"
                      + ";+fBestVerdi|DECIMAL|>><>>><>>9.99|varebehlinje_best_verdi(ROWID)|Sum innpris"
                      + ";KampanjePris"
                      + ";LinjeMerknad"
                      + ";!+TemaSeq|INTEGER|>>>>9|varebehlinje_tema(ROWID)"
                      + ";!VarebehNr;!LevNr"
                      + ";!+bBestilt|LOGICAL|yes/no|varebehlinje_bestilt(ROWID)"
                      + ";!+bMatchMerknad|LOGICAL|yes/no|varebehlinje_match_merknad(ROWID)"
                      + ";!+RGBfarge|INTEGER|>>>>>>>>9|varebehlinje_rgb"
                      + ";!forhRab%;!forhKalkyle;!SupDb%;!SupDbKr"
                      + ";KjedeVare|Kj.lev|J/N"
                      + ";Gjennomfaktureres|Gj.fakt|J/N"
                      + ";!Utvidetsok"
                                  + ";Hg|HgNr"
                                  + ";HgBeskr|HgBeskr|x(20)"
                                  + ";AvdelingNr|AvdNr"
                                  + ";AvdelingNavn|AvdBeskr|x(25)"
                                  + ";LevNr"
                                  + ";ProdNr"
                                  + ";ProdusentBeskrivelse|Prod.Navn|x(20)"
                    + ",ArtBas;!BildNr" 
                      + ";SalgsEnhet"
                      + ";FrittTillegg|Fritt tillegg"
                      + ";AntIPakn"
                      + ";!StrTypeId"
                      + ";VPIbildeKode"
                    + ",Varemerke;Beskrivelse|Varemerke"
                    + ",Sasong;SasBeskr|Sesong"

/*                       + ";!+KjedeVareFilter|CHARACTER|x|artbas_kjedevare(ROWID)"  */
                    ,"WHERE false"
                      + cVarebehLinjeJoin
                    ,"calcfieldproc|varebeh_browsekalk.p"
                      ).     
  hBrowseLinje:NAME = "Varehand-artikler". /* This name is neccessary because the browser is due to special treatment during resize */
  hBrowseLinje:TOOLTIP = "CTRL-S/P lagrer og skifter til neste/forrige artikkel".
  hBrowseLinje:MOVE-BEFORE(ButikkNr:HANDLE).
  ASSIGN hSekv        = hBrowseLinje:GET-BROWSE-COLUMN(1)
         hGodkjentCol = hBrowseLinje:GET-BROWSE-COLUMN(2)
         hBeskr       = hBrowseLinje:GET-BROWSE-COLUMN(6)
         hBestVerdi   = hBrowseLinje:GET-BROWSE-COLUMN(16)
         .
  DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"windowsbrowse","yes").
  DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"nexttabitem",STRING(sokArtikkelNr:HANDLE)).
  DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"prevtabitem",STRING(ButikkNr:HANDLE)).
  
  IF cLevNrListe NE "*" AND DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"getrecordcount") NE "no" THEN
    DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"getrecordcount","yes").

  DO ix = 2 TO 12:
    hBrowseLinje:GET-BROWSE-COLUMN(ix):WIDTH-PIXELS = hBrowseLinje:GET-BROWSE-COLUMN(ix):WIDTH-PIXELS - 
                (IF hBrowseLinje:GET-BROWSE-COLUMN(ix):WIDTH-PIXELS > 100 THEN 30 ELSE 10).
  END.
  ASSIGN hBrwColKjedeVare = hBrowseLinje:GET-BROWSE-COLUMN(18)
         hBrwColGjFakt    = hBrowseLinje:GET-BROWSE-COLUMN(19).
  hBrowseLinje:MOVE-AFTER-TAB-ITEM(stdLevUke4:HANDLE).

/*   DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"querywhere","").  */

  hSearchLinje = DYNAMIC-FUNCTION("NewBrowseSearchField",RectBrowseSearchLinje:HANDLE,hBrowseLinje,1).

  DYNAMIC-FUNCTION("NewMenuBand",
                    hBrowseLinje,  /* parent widget */
                    "Deselect;Fjern markering av rad"
                    + ",MultiSortBrowse;Sorter på flere kolonner" 
                    + ",|Spesialsortering"
                    + ",KopierReg;Kopier best. til andre butikker (eksisterende best. blir ikke overskrevet)" 
                    + ",GodkjennBest;Godgjenn bestillinger for valgte artikler"
                    + ",OpphevGodkjBest;Opphev godkjenning av bestillinger for valgte artikler"
                    + ",AnnulerBestForsl;Annuler bestillingsforslag for valgte artikler"
                    + ",TotBestilt;Sum registrerte bestillinger for markerte artikler"
                    + ",TotForslag;Sum bestillingsforslag for markerte artikler"
                    + ",SettLevUker;Endre leveringsuker for valgte artikler" 
                    + (IF cButikkListe = "*" THEN
                        ",AddToTema;Legg valgte artikler til tema"
                      + ",RemoveFromTema;Fjern valgte artikler fra tema"
                       ELSE "")
                    + (IF cButikkListe = "*" AND cLevNrListe = "*" THEN 
                        ",AddColor;Legg til farger (artikler) for denne modell" 
                      + ",AddStr;Legg til størrelser for denne artikkel" 
                      + ",AddInndeling;Legg til inndeling for denne artikkel" 
                       ELSE "")
                    ,
                    "").     

  hSpesialSortMnu = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"placeholder1")).
  setSpesialSortMenu().

  cDefaultSort = DYNAMIC-FUNCTION("getFieldValues","JBoxGenCode","WHERE cCodeType = 'VarebokSort' AND cMisc1 = 'yes'","cDescription"). 
  IF cDefaultSort NE ? THEN
    startSpesialSort(cDefaultSort,NO).
  ELSE
    DYNAMIC-FUNCTION("setSortString",hBrowseLinje,"LevNamn,Beskr").

  hFieldMapLinje = DYNAMIC-FUNCTION("NewFieldMap", 
                    hBrowseLinje:QUERY,
                    FRAME frmLinje:HANDLE,     
                    "","",
                    ""
                    , 
                    "",  
                    "").     
  DYNAMIC-FUNCTION("setAttribute",hFieldMapLinje,"customDeleteValProc","=delval_varebehlinje.p").
  ASSIGN hBestilt       = hFieldMapLinje:BUFFER-FIELD("bBestilt")
         hGodkjent      = hFieldMapLinje:BUFFER-FIELD("GodkjentBest")
         hLinjeMerknad  = hFieldMapLinje:BUFFER-FIELD("LinjeMerknad")
         hFieldRGBcolor = hFieldMapLinje:BUFFER-FIELD("RGBfarge")
         .

  hBrowseTrans = DYNAMIC-FUNCTION("NewBrowse",         
                    rectBrowseTrans:HANDLE IN FRAME frmLinje,  
                    10000,                          
                    "MULTIPLE", /* ,NUM-LOCKED-COLUMNS|1 */  
                    "VareBehLinjeTrans" 
                      + ";+cArtNavn|CHARACTER|x(30)|ArtNavn|Art.navn"
                      + ";+VisLevUke1|INTEGER|9999|VisLevUke(LevUke1)|Levuke1"
                      + ";!LevUke1"
                      + ";Bestilt1"
                      + ";+VisLevUke2|INTEGER|9999|VisLevUke(LevUke2)|Levuke2"
                      + ";!LevUke2"
                      + ";Bestilt2"
                      + ";+VisLevUke3|INTEGER|9999|VisLevUke(LevUke3)|Levuke3"
                      + ";!LevUke3"
                      + ";Bestilt3"
                      + ";+VisLevUke4|INTEGER|9999|VisLevUke(LevUke4)|Levuke4"
                      + ";!LevUke4"
                      + ";Bestilt4"
                      + ";+SumAntStr|INTEGER|>><>>9|SumAntStr|Sum ant"
                      + ";+SumVerdiStr|DECIMAL|>>><>>9.99|SumVerdiStr|Sum verdi"
                      + ";ButikkNr"
                      + ";!LevDato1"
                      + ";!LevDato2"
                      + ";!LevDato3"
                      + ";!LevDato4"
                      + ";!VarebehNr"
                      + ";!ArtikkelNr"
                      + ";!SeqNr|Sekv"
                      + ";Kode|Visma art/Innd."
/*                     + ",Strekkode;!StrKode" */

                      + ";+StrInterv|CHARACTER|x(40)|artsort_intervall|Str.interv.@12"
                      + ";+StrFord|CHARACTER|x(40)|artsort_fordeling|Fordeling@13"
                      + ";+SumAntIpk|CHARACTER|x(3)|artsort_sumant|Ant.i pk@14"
                    + ",StrKonv;Storl|Str@2"
/*                     + ",ArtSort;!SortId"                                               */
/*                       + ";+StrInterv|CHARACTER|x(40)|artsort_intervall.p|Str.interv."  */
/*                       + ";+StrFord|CHARACTER|x(40)|artsort_fordeling.p|Fordeling"      */
/*                       + ";+SumAntIpk|CHARACTER|x(3)|artsort_sumant.p|Ant.i pk"         */
                      ,
                    "WHERE false"
/*                       + ",FIRST Strekkode OF VareBehLinjeTrans NO-LOCK OUTER-JOIN" */
/*                       + ",FIRST StrKonv OF Strekkode NO-LOCK OUTER-JOIN" */
                      + ",FIRST StrKonv OF VareBehLinjeTrans NO-LOCK OUTER-JOIN"
/*                       + ",FIRST ArtSort WHERE ArtSort.ArtikkelNr = VarebehLinjeTrans.ArtikkelNr AND ArtSort.SortId = VarebehLinjeTrans.kode NO-LOCK"  */
                      ,
                    "sort|SeqNr").                            /* Misc - for something I might need in next version.. */
  hBrowseTrans:NAME = "brwTrans". /* This name is neccessary because the browser is due to special treatment during resize */

  ASSIGN hRegLevUke[1] = DYNAMIC-FUNCTION("getBrowseColumn",hBrowseTrans,"VisLevUke1") /* hBrowseTrans:GET-BROWSE-COLUMN(2) */
         hRegLevUke[2] = DYNAMIC-FUNCTION("getBrowseColumn",hBrowseTrans,"VisLevUke2") /* hBrowseTrans:GET-BROWSE-COLUMN(4) */
         hRegLevUke[3] = DYNAMIC-FUNCTION("getBrowseColumn",hBrowseTrans,"VisLevUke3") /* hBrowseTrans:GET-BROWSE-COLUMN(6)  */
         hRegLevUke[4] = DYNAMIC-FUNCTION("getBrowseColumn",hBrowseTrans,"VisLevUke4") /* hBrowseTrans:GET-BROWSE-COLUMN(8) */
         .

/*   hBrowseTrans:GET-BROWSE-COLUMN(2):WIDTH-PIXELS = 70. */
  hBrowseTrans:GET-BROWSE-COLUMN(12):WIDTH-PIXELS = 90.
  hBrowseTrans:GET-BROWSE-COLUMN(13):WIDTH-PIXELS = 70.
/*   hBrowseTrans:MOVE-COLUMN(14,2).   */
/*   hBrowseTrans:MOVE-COLUMN(15,12).  */
/*   hBrowseTrans:MOVE-COLUMN(16,13).  */
/*   hBrowseTrans:MOVE-COLUMN(17,14).  */

  DYNAMIC-FUNCTION("setAttribute",hBrowseTrans,"calcfieldproc","varebehlinjetrans_regbrwcalc.p").
  DYNAMIC-FUNCTION("setNoColumnSort",hBrowseTrans,"SumAntStr,SumVerdiStr,cArtNavn,StrInterv,StrFord,SumAntIpk").
  DYNAMIC-FUNCTION("setAttribute",hBrowseTrans,"prevtabitem",STRING(LevUke4:HANDLE)).
  DYNAMIC-FUNCTION("CreateParentLink",hBrowseTrans,hBrowseLinje,"parentlink").

  hBrwTransBestilt1 = DYNAMIC-FUNCTION("NewBrowseFillIn",
                    hBrowseTrans,
                    "Bestilt1",
                    "Bestilt1",
                    "","","",
                    "0,9999").
  ASSIGN hBrwTransBestilt1:NAME = "fiBestilt1"
         hBrwTransBestilt1:HELP = "Antall bestilt, 1. leveringsuke".
  DYNAMIC-FUNCTION("setAttribute",hBrwTransBestilt1,"refreshrow","yes").

  hBrwTransBestilt2 = DYNAMIC-FUNCTION("NewBrowseFillIn",
                    hBrowseTrans,
                    "Bestilt2",
                    "Bestilt2",
                    "","","",
                    "").
  ASSIGN hBrwTransBestilt2:NAME = "fiBestilt2"
         hBrwTransBestilt2:HELP = "Antall bestilt, 2. leveringsuke".
  DYNAMIC-FUNCTION("setAttribute",hBrwTransBestilt2,"refreshrow","yes").

  hBrwTransBestilt3 = DYNAMIC-FUNCTION("NewBrowseFillIn",
                    hBrowseTrans,
                    "Bestilt3",
                    "Bestilt3",
                    "","","",
                    "").
  ASSIGN hBrwTransBestilt3:NAME = "fiBestilt3"
         hBrwTransBestilt3:HELP = "Antall bestilt, 3. leveringsuke".
  DYNAMIC-FUNCTION("setAttribute",hBrwTransBestilt3,"refreshrow","yes").

  hBrwTransBestilt4 = DYNAMIC-FUNCTION("NewBrowseFillIn",
                    hBrowseTrans,
                    "Bestilt4",
                    "Bestilt4",
                    "","","",
                    "").
  ASSIGN hBrwTransBestilt4:NAME = "fiBestilt4"
         hBrwTransBestilt4:HELP = "Antall bestilt, 4. leveringsuke".
  DYNAMIC-FUNCTION("setAttribute",hBrwTransBestilt4,"refreshrow","yes").

  DYNAMIC-FUNCTION("NewMenuBand",
                    hBrowseTrans,  /* parent widget */
                    "Bestilt1;Sett bestilt antall for 1. leveringsuke" 
                    + ",Bestilt2;Sett bestilt antall for 2. leveringsuke" 
                    + ",Bestilt3;Sett bestilt antall for 3. leveringsuke" 
                    + ",Bestilt4;Sett bestilt antall for 4. leveringsuke" 
                    + (IF cLevNrListe = "*" AND cButikkListe = "*" THEN
                        ",SlettStr;Slett markert(e) størrelse(r) fra registreringsunderlag"
                       ELSE "")
                    ,
                    "").     

  hFieldMapTrans = DYNAMIC-FUNCTION("NewFieldMap", 
                    hBrowseTrans:QUERY,
                    FRAME frmLinje:HANDLE,     
                    "","",
                    "","",  
                    "").     
  DYNAMIC-FUNCTION("setAttribute",hFieldMapTrans,"PostUpdateProc","varebehlinjetrans_post_update.p").
/*   DYNAMIC-FUNCTION("setAttribute",hFieldMapTrans,"customUpdateValProc","=varebehlinjetrans_update.p"). */
  /* Bruker et ekstra felt som parameter til serverprosedyren. Verdi settes i LeaveBrowseFillIn: */
  DYNAMIC-FUNCTION("setAttribute",hFieldMapTrans,"bufferextrafields","levnrlist").

  hLinjeTransToolbar = DYNAMIC-FUNCTION("NewToolbar",
                    rectTBvarebehlinjetrans:HANDLE IN FRAME frmLinje,
                    "","Excel;Nøkkeltallsrapport","").
  hExcelTransBtn = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hLinjeTransToolbar,"buttonExcel")).
  hExcelTransBtn:TAB-STOP = FALSE.

  CREATE BUFFER hSumTransBuffer FOR TABLE hFieldMapTrans.
  CREATE QUERY  hSumTransQuery.
  hSumTransQuery:SET-BUFFERS(hSumTransBuffer).
  hSumTransQuery:QUERY-PREPARE("FOR EACH " + hSumTransBuffer:NAME).
                   
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitRegStat C-Win 
PROCEDURE InitRegStat :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME frmRegStat:
  ASSIGN cVarebehLinjeThodeJoin = ",FIRST Butiker WHERE Butik = VarebehLinjeThode.ButikkNr AND NedlagtDato = ? NO-LOCK"
         cVarebehLinjeTransJoin = ",FIRST VarebehLinje OF VarebehLinjeTrans NO-LOCK"
                                + (IF NUM-ENTRIES(cLevNrListe) > 1 THEN
                                     " WHERE CAN-DO('" + cLevNrListe + "',STRING(VarebehLinje.LevNr))"
                                   ELSE IF cLevNrListe NE "*" THEN
                                     " WHERE VarebehLinje.LevNr = " + cLevNrListe
                                   ELSE "")
                                + ",FIRST ArtBas OF VarebehLinje NO-LOCK"
                                + ",FIRST Varemerke OF ArtBas NO-LOCK"
                                  .

  IF cButikkListe NE "*" THEN rsAlleReg:SENSITIVE = FALSE.

  hBrowseRegStat = DYNAMIC-FUNCTION("NewBrowse",         
                    rectRegStat:HANDLE IN FRAME frmRegStat,  
                    10000,                          
                    "", /* MULTIPLE,NUM-LOCKED-COLUMNS|1 */  
                    "VarebehLinjeThode" 
                      + ";ButikkNr"
                      + ";Godkjent|Godkjent|J/N"
                      + ";!VarebehNr"
                      + ";!+BestiltButikk|CHARACTER|x|varebehlinjethode_bestilt.p"
                    + ",Butiker"
                      + ";Butnamn"
                      + ";Sentrallager|Sentr.lager|J/N"
                      + ";clButikkNr|Sentr.lager butikk" 
                      ,
                    "WHERE false"
                      + cVarebehLinjeThodeJoin
                      ,
                    "sort|Butikknr").                            /* Misc - for something I might need in next version.. */
  hBrowseRegStat:NAME = "brwRegStat". /* This name is neccessary because the browser is due to special treatment during resize */

  DYNAMIC-FUNCTION("setAttribute",hBrowseRegStat,"querywhere","").  

  IF cButikkListe = "*" AND cLevNrListe = "*" THEN
    DYNAMIC-FUNCTION("NewMenuBand",
                      hBrowseRegStat,  /* parent widget */
                      "SettGodkjenning;Sett godkjenning" 
                      ,
                      "").     

  hFieldMapRegStat = DYNAMIC-FUNCTION("NewFieldMap", 
                    hBrowseRegStat:QUERY,
                    FRAME frmRegStat:HANDLE,     
                    "","",
                    ""
                    , 
                    "",  
                    "").     

  DYNAMIC-FUNCTION("createObjectLink",hBrowseRegStat,hFieldMapRegStat).

  hBrowseRegStatTrans = DYNAMIC-FUNCTION("NewBrowse",         
                    rectRegStatTrans:HANDLE IN FRAME frmRegStat,  
                    100,                          
                    "MULTIPLE", /* ,NUM-LOCKED-COLUMNS|1 */  
                    "VareBehLinjeTrans" 
                      + ";ArtikkelNr|PRS SE" + CHR(10) + "Art.nr"
                      + ";LevUke1|L.uke1"
                      + ";Bestilt1"
                      + ";LevUke2|L.uke2"
                      + ";Bestilt2"
                      + ";LevUke3|L.uke3"
                      + ";Bestilt3"
                      + ";LevUke4|L.uke4"
                      + ";Bestilt4"
                      + ";ButikkNr"
                      + ";+SumAntall|INTEGER|>>>>9|varebehlinjetrans_totbestilt(ROWID)|Sum antall"
                      + ";+OrdreTot|DECIMAL|>>>>>>>9.99|varebehlinjetrans_ordretot(ROWID)|Sum ordre"
                      + ";EDato"
                      + ";!VarebehNr;!SeqNr;!Kode"
                      + ";!+DistinctCol|DECIMAL|>>>>>>>>9|varebehlinjetrans_distinct(ROWID)"
                      + ";!+GodkjentBest|LOGICAL|*/|varebehlinjetrans_bestforslag(ROWID)|Godkj"
                      + ";!RegistrertBestilling;!GodkjentBestilling"
/*                     + ",Strekkode;!StrKode"  */
                    + ",StrKonv;Storl|Str"
                    + ",VareBehLinje"
                      + ";levKod"
                      + ";Beskr|Artikkelnavn"
                      + ";LevFargKod|Farge" + CHR(10) + "tekst"
                      + ";Levnr"
                      + ";levnamn|Leverandør"
                      + ";VareKost|Netto" + CHR(10) + "forh.pris"
                      + ";DB%"
                      + ";supVareKost|Netto" + CHR(10) + "sup.pris"
                      + ";Pris|Veil." + CHR(10) + "uts.pris"
                      + ";KampanjePris|Kampanje" + CHR(10) + "pris"
                      + "!;Sortering|Art.sekv"
                      + ";Vg"
                      + ";VgBeskr|Vgr.tekst"
                      + ";Hg"
                      + ";HgBeskr|Hgr.tekst"
                      + ";AvdelingNr"
                      + ";AvdelingNavn"
                    + ",ArtBas;!VMid"
                    + ",Varemerke;Beskrivelse|Varemerke"
                    ,"WHERE false"
/*                       + ",FIRST Strekkode OF VareBehLinjeTrans NO-LOCK OUTER-JOIN" */
/*                       + ",FIRST StrKonv OF Strekkode NO-LOCK OUTER-JOIN" */
                      + ",FIRST StrKonv OF VareBehLinjeTrans NO-LOCK OUTER-JOIN"
                      + cVarebehLinjeTransJoin
                    ,"calcfieldproc|varebehlinjetrans_browsekalk.p").
  hBrowseRegStatTrans:NAME = "brwRegStatTrans". 
  DYNAMIC-FUNCTION("setAttribute",hBrowseRegStatTrans,"querysort","Sortering BY SeqNr").
/*   DYNAMIC-FUNCTION("setAttribute",hBrowseRegStatTrans,"getrecordcount","yes").  */
  ASSIGN hBrwLinjeTransArtikkelnr = hBrowseRegStatTrans:GET-BROWSE-COLUMN(1)
         hBrwLinjeTransButikknr   = hBrowseRegStatTrans:GET-BROWSE-COLUMN(10)
         hBrwLinjeTransStorl      = hBrowseRegStatTrans:GET-BROWSE-COLUMN(14).

  DO ix = 2 TO 9:
    hBrwLinjeTransUker[ix - 1] = hBrowseRegStatTrans:GET-BROWSE-COLUMN(ix).
    hBrwLinjeTransUker[ix - 1]:VISIBLE = FALSE.
  END.

  DO ix = 14 TO 17:
    hBrowseRegStatTrans:MOVE-COLUMN(ix,ix - 12).
  END.

  DYNAMIC-FUNCTION("setAttribute",hBrowseRegStatTrans,"availdistinctcolumns",
                    "ArtikkelNr,ButikkNr,levKod,LevNamn,Vg,VgBeskr,Hg,HgBeskr,AvdelingsNr,AvdelingsNavn").
  DYNAMIC-FUNCTION("setAttribute",hBrowseRegStatTrans,"availaccumfields",
                    "SumAntall,OrdreTot").
  DYNAMIC-FUNCTION("setAttribute",hBrowseRegStatTrans,"accumdatatypes","DECIMAL,INTEGER").
  DYNAMIC-FUNCTION("setAttribute",hBrowseRegStatTrans,"distinctdatatypes","CHARACTER,DECIMAL,INTEGER,DATE").

  DYNAMIC-FUNCTION("setAttribute",hBrowseRegStatTrans,"QueryFilter",
                   " AND RegistrertBestilling AND (Bestilt1 > 0 OR Bestilt2 > 0 OR Bestilt3 > 0 OR Bestilt4 > 0)"
                   ).
  DYNAMIC-FUNCTION("setAttribute",hBrowseRegStatTrans,"QueryWhere",
                   " AND RegistrertBestilling AND (Bestilt1 > 0 OR Bestilt2 > 0 OR Bestilt3 > 0 OR Bestilt4 > 0)"
                   ).
  
  DYNAMIC-FUNCTION("setAttribute",hBrowseRegStatTrans,"prescanqueryVaremerke","EACH ArtBas OF Varemerke NO-LOCK,EACH VarebehLinje OF ArtBas NO-LOCK,EACH VarebehLinjeTrans OF VarebehLinje NO-LOCK"). 

  DYNAMIC-FUNCTION("createParentLink",hBrowseRegStatTrans,hBrowseRegStat,'VarebehNr,ButikkNr').

  DYNAMIC-FUNCTION("NewMenuBand",
                    hBrowseRegStatTrans,  /* parent widget */
                    "Deselect;Fjern markering av rad"
                    + ",MultiSortBrowse;Sorter på flere kolonner" 
                    ,
                    "").     

  hFieldMapRegStatTrans = DYNAMIC-FUNCTION("NewFieldMap", 
                    hBrowseRegStatTrans:QUERY,
                    FRAME frmRegStat:HANDLE,     
                    "","",
                    "","",  
                    "").  
  hTransGodkjent = hFieldMapRegStatTrans:BUFFER-FIELD("GodkjentBest").
                    
  IF cButikkListe NE "*" OR cLevNrListe NE "*" THEN
    ASSIGN btnAlleBut:HIDDEN = TRUE
           btnAlleButSam:HIDDEN = TRUE.

  hRegStatTransToolbar = DYNAMIC-FUNCTION("NewToolBar",
                    rectRegStatTransToolbar:HANDLE IN FRAME frmRegStat,
                    "",
                    "ButOrdrebekr;Ordrebekr.butikk"
                  + ",ButOrdreForsl;Ordreforslag butikk"
                  + (IF cLevNrListe = "*" THEN
                      ",ButOrdrebekrPrLev;Ordrebekr.butikk pr lev"
                     ELSE "")
                  + (IF cButikkListe = "*" THEN 
                       ",LevOrdrebekr;Ordrebekr.leverandør"
                     ELSE "")
                  + (IF cButikkListe = "*" AND cLevNrListe = "*" THEN 
                       ",ForecastLev;Forecast lev"
                     + ",GenBestCL;Generer ordre for CL"
                     + ",SjekkBestCL;Avstem ordre"
                     + ",Excel;Overfør til Excel"
/*                      + ",Filter"          */
/*                      + ",Accum;Akkumuler" */
                    ELSE "")
                    ,"maxborder").

  DYNAMIC-FUNCTION("createObjectLink",hFieldMapRegStatTrans,hRegStatTransToolbar).
  DYNAMIC-FUNCTION("createObjectLink",hBrowseRegStatTrans,hRegStatTransToolbar).

END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitWindow C-Win 
PROCEDURE InitWindow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>  
  Notes:      
------------------------------------------------------------------------------*/
DEF VAR cHKinst         AS CHAR NO-UNDO.

SetTabStrip().

ASSIGN cAdgang        = DYNAMIC-FUNCTION("getFieldValues","SysPara",
                            "WHERE SysHId = 16 and SysGr = 39 and ParaNr = 2","Parameter1")
       cHKinst        = DYNAMIC-FUNCTION("getFieldValues","SysPara",
                            "WHERE SysHId = 1 and SysGr = 1 and ParaNr = 18","Parameter1")
       cCL             = DYNAMIC-FUNCTION("getFieldValues","SysPara",
                            "WHERE SysHId = 5 and SysGr = 1 and ParaNr = 1","Parameter1")
       bArtVedl       = DYNAMIC-FUNCTION("getFieldValues","SysPara",
                           "WHERE SysHId = 5 and SysGr = 7 and ParaNr = 101","Parameter1") = "1"
       bHKinst        = IF CAN-DO("1,yes,true",cHKinst) THEN TRUE ELSE FALSE
       cInitPrisProfil = DYNAMIC-FUNCTION("getFieldValues","Butiker",
                            "WHERE Butik = " + cCl,"ProfilNr")
       cLevNrListe    = REPLACE(DYNAMIC-FUNCTION("getFieldList","BrukerLev;levnr","where BrukerId = '" + DYNAMIC-FUNCTION("getASuserId") + "'"),"|",",")
       .  

/* cLevNrListe = "1".  */
/* cAdgang = "1".      */
/* MESSAGE PROGRAM-NAME(1) SKIP  */
/*         cLevNrListe SKIP      */
/*         cButikkListe          */
/*         VIEW-AS ALERT-BOX.    */

IF cLevNrListe = "" OR cLevNrListe = "0" THEN cLevNrListe = "*".

IF cButikkListe NE "*" OR cLevNrListe NE "*" THEN cAdgang = "1".

DO WITH FRAME frmListe:
  hBrowseListe = DYNAMIC-FUNCTION("NewBrowse",
                    rectBrowseListe:HANDLE IN FRAME frmListe,
                    200,
                    "NUM-LOCKED-COLUMNS|1",
                    "VarebehHode"
                      + ";VarebehNr|Vareh.nr"
                      + ";VarebehBeskrivelse"
                      + ";VarebehType|Type"
                      + ";MesseNr;ProfilNr|Profilnr|>>>>>>9;Oppdatert;OppdatDato;EDato;BrukerId;Kilde"
                      + ";!VarebehNotat;!OppdatAv"
                      + ";!+VarebehFilter|CHARACTER|x(5)|varebehhode_filter.p(ROWID" 
                         + REPLACE(cButikkListe,",","&") + "¤" + REPLACE(cLevNrListe,",","&") 
                         + "¤" + DYNAMIC-FUNCTION("getASuserId")
                         + ")"
                      + ",Messe;MesseBeskrivelse;!Oppmerking"
                      + ",PrisProfil;KortNavn|Profil"
                      + ",VarebehType;BeskrivelseVarebehType"
                      + ",VarebokHode;VareBokBeskrivelse"
                      ,
                    "WHERE " + (IF cButikkListe NE "*" THEN "NOT Oppdatert" ELSE "true")  
                      + ",FIRST Messe NO-LOCK where Messe.MesseNr = VarebehHode.MesseNr" 
                      + ",FIRST PrisProfil NO-LOCK where PrisProfil.ProfilNr = VarebehHode.ProfilNr OUTER-JOIN"
                      + ",FIRST VarebehType NO-LOCK OF VarebehHode OUTER-JOIN"
                      + ",FIRST VarebokHode NO-LOCK WHERE VarebokHode.VarebokNr = VarebehHode.Kilde OUTER-JOIN"
                    ,"sort|MesseNr DESC").                          /* Misc - for something I might need in next version.. */
  hBrowseListe:NAME = "Varehåndteringsbøker". /* This name is neccessary because the browser is due to special treatment during resize */
  DYNAMIC-FUNCTION("setAttribute",hBrowseListe,"querywhere","").
  DYNAMIC-FUNCTION("setAttribute",hBrowseListe,"nocolumnsearch","MesseBeskrivelse,Kortnavn,BeskrivelseVarebehType,VareBokBeskrivelse").
  
  hBrowseListe:MOVE-COLUMN(13,4).
  hBrowseListe:MOVE-COLUMN(12,6).
  hBrowseListe:MOVE-COLUMN(13,8).
  hSearchListe = DYNAMIC-FUNCTION("NewBrowseSearchField",RectBrowseSearchListe:HANDLE,hBrowseListe,1).
END.

DO WITH FRAME frmDetalj: 
  hFieldMap = DYNAMIC-FUNCTION("NewFieldMap",
                    hBrowseListe:QUERY,
                    FRAME frmDetalj:HANDLE,
                    IF bArtVedl THEN
                      "VarebehBeskrivelse,MesseNr,VarebehNotat,ProfilNr,VarebehType"
                    ELSE IF cButikkListe = "*" AND cButikkListe = "*" THEN "Oppdatert" 
                    ELSE "",
                    "", 
                    IF bArtVedl THEN
                      "VarebehNr,MesseBeskrivelse,OppdatDato,OppdatAv,BeskrivelseVarebehType,KortNavn"
                    ELSE
                      "VarebehBeskrivelse,MesseNr,VarebehNotat,ProfilNr,OppdatDato,OppdatAv,VarebehType"
                      + ",VarebehNr,MesseBeskrivelse,KortNavn,BeskrivelseVarebehType,Kilde,VareBokBeskrivelse"
                      + (IF cButikkListe NE "*" AND cLevNrListe = "*" THEN ",Oppdatert" ELSE ""), 
                    "",  
                    "").

  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"customUpdateValProc","ignore"). 
  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"customDeleteValProc","=delval_Varebehhode.p"). 
  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"BufferExtraFields","OppdatAv,OppdatDato").
END.

RUN InitLinje.

ASSIGN sokButikk:DELIMITER IN FRAME frmFilter = "|"
       sokButikk:LIST-ITEM-PAIRS = ButikkNr:LIST-ITEM-PAIRS IN FRAME frmLinje
       sokButikk:SCREEN-VALUE = "0".

RUN InitRegStat.

/* Menyer: */

DYNAMIC-FUNCTION("NewMenuBand",
                  hBrowseListe,  /* parent widget */
                  "Deselect;Fjern markering av rad"
                 ,"").   

DO WITH FRAME {&FRAME-NAME}:
  IF CAN-DO("1",cAdgang) THEN DO:      
    hUpdToolBar = DYNAMIC-FUNCTION("NewToolBar",
                  rectUpdToolBar:HANDLE,         
                  "Fil",                         
                  "Undo;Angre,save;Lagre,excel;Eksporter til E&xcel"
                   + ",rule,first|Naviger;Første,prev|Naviger;Forrige,next|Naviger;Neste,last|Naviger;Siste,|Totaler,|Innstillinger",
                  "maxborder").   
    hTotalMenu = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hUpdToolbar,"placeholder1")) NO-ERROR.
    IF VALID-HANDLE(hTotalMenu) THEN
      DYNAMIC-FUNCTION("NewMenuBand",
                       hTotalMenu,
                       "TotBestilt;Sum registrerte bestillinger for gjeldende utvalg av artikler"
                     + ",TotForslag;Sum bestillingsforslag for gjeldende utvalg av artikler"
                       ,"").
  END.
  ELSE DO:
    hUpdToolBar = DYNAMIC-FUNCTION("NewToolBar",
                  rectUpdToolBar:HANDLE,     
                  "Fil",  
                  (IF bArtVedl AND cButikkListe = "*" AND cLevNrListe = "*" THEN "New;Ny,Copy;Kopier," ELSE "") +
                  "Undo;Angre,delete;Slett&,save;Lagre,excel;Eksporter til E&xcel,FlatView"
                   + ",rule,first|Naviger;Første,prev|Naviger;Forrige,next|Naviger;Neste,last|Naviger;Siste&,|Totaler,|Innstillinger,|Oppsett"
                   + (IF bArtVedl AND cButikkListe = "*" AND cLevNrListe = "*" THEN 
                       ",rule,StartUtvalg;Utvalg"
                     + ",HentUtvalg;Hent artikler fra utvalg"
                      ELSE "") 
                  ,"maxborder").      
    ASSIGN hMenuItemFlatView = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hUpdToolbar,"menuitemFlatView"))
           hButtonFlatView   = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hUpdToolbar,"buttonFlatView"))
           cBtnUtvalgHandles     = DYNAMIC-FUNCTION("getToolbarHandles",hUpdToolbar,"*StartUtvalg*")
           cBtnHentUtvalgHandles = DYNAMIC-FUNCTION("getToolbarHandles",hUpdToolbar,"*HentUtvalg*")
           .

    DYNAMIC-FUNCTION("NewMenuBand",
                      WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hUpdToolbar,"placeholder2")),  /* parent widget */
/*                      "SetColorLinjeMerknad;Velg farge for markering basert på merknadskode;SetColorLinjeMerknad" */
/*                    + ",SetColor;Velg merknadskoder for fargemarkering;MarkerLinjeMerknad"                        */
/*                      "SetColorSekv;Velg farge for markering av bestilte artikler;SetColorBestilt"  */
                     "SetColorSekvGodk;Velg farge for markering av godkjente bestillinger;SetColorBestiltGodk"
                   + ",SetColorSekvForsl;Velg farge for markering av bestillingsforslag;SetColorBestiltForsl"
                   + ",BestiltVarsel;Angi best.antall for varsling;SetBestiltVarsel"
                   + ",BrukerGrTilgang;Sett tilgang for brukergr (ingen valgt - ingen restriksjon. Kun but.brukere)" 
                     ,"").

    hTotalMenu = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hUpdToolbar,"placeholder1")) NO-ERROR.
    IF VALID-HANDLE(hTotalMenu) THEN
      DYNAMIC-FUNCTION("NewMenuBand",
                       hTotalMenu,
                       "TotBestilt;Sum registrerte bestillinger for gjeldende utvalg av artikler"
                     + ",TotForslag;Sum bestillingsforslag for gjeldende utvalg av artikler"
                       ,"").

  END.

  DYNAMIC-FUNCTION("NewMenuBand",
                    WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hUpdToolbar,"placeholder2")),  /* parent widget */
                    "RowsToBatch;Antall rader i resultatsett"
                  + ",ViewQueryCount;Vis antall rader i resultatsett;ViewQueryCountRecord;toggle"
                    ,"").

  hRowsToBatchMenu = DYNAMIC-FUNCTION("getEventWidget",hUpdToolbar,"RowsToBatch","menu-item").
  hQueryCountMenu = DYNAMIC-FUNCTION("getEventWidget",hUpdToolbar,"ViewQueryCount","menu-item").

  hToolBar = DYNAMIC-FUNCTION("NewToolBar",
                    rectToolBar:HANDLE,             
                    "Fil",                          
                    "Help|Hjelp,Close;Avslutt",
                    "").   
  
  DYNAMIC-FUNCTION("createObjectLink",hBrowseListe,hUpdToolbar).
  DYNAMIC-FUNCTION("createObjectLink",hFieldMap,hUpdToolbar).
  DYNAMIC-FUNCTION("createObjectLink",hBrowseListe,hFieldMap).
  DYNAMIC-FUNCTION("createObjectLink",hBrowseLinje,hFieldMapLinje).
  DYNAMIC-FUNCTION("createObjectLink",hBrowseTrans,hLinjeTransToolbar).
  DYNAMIC-FUNCTION("createParentLink",hBrowseLinje,hBrowseListe,'VarebehNr').
  DYNAMIC-FUNCTION("createObjectLink",hBrowseListe,hSearchListe).
  DYNAMIC-FUNCTION("createObjectLink",hBrowseLinje,hSearchLinje).

  DYNAMIC-FUNCTION("setAddMoveX",   THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "rectNavVarebeh").
  DYNAMIC-FUNCTION("setNoResizeY",  THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "rectUpdToolBar,rectVarebeh").
  DYNAMIC-FUNCTION("setNoResizeX",  THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "rectNavVarebeh").

  DYNAMIC-FUNCTION("setNoResizeX",  THIS-PROCEDURE:CURRENT-WINDOW, FRAME frmFilter:HANDLE, "RectArtSok").
  DYNAMIC-FUNCTION("setNoResizeY",  THIS-PROCEDURE:CURRENT-WINDOW, FRAME frmFilter:HANDLE, "frmFilter,RectArtSok").

  DYNAMIC-FUNCTION("setNoResizeX",  THIS-PROCEDURE:CURRENT-WINDOW, FRAME frmListe:HANDLE, "RectBrowseSearchListe").

  DYNAMIC-FUNCTION("setNoResizeX",  THIS-PROCEDURE:CURRENT-WINDOW, FRAME frmDetalj:HANDLE, "rect-1").
  DYNAMIC-FUNCTION("setNoResizeY",  THIS-PROCEDURE:CURRENT-WINDOW, FRAME frmDetalj:HANDLE, "rect-1").

  DYNAMIC-FUNCTION("setNoResizeX",  THIS-PROCEDURE:CURRENT-WINDOW, FRAME frmLinje:HANDLE, "RectBrowseSearchLinje,IMAGE-Sko,rectButTotal").
  DYNAMIC-FUNCTION("setNoResizeY",  THIS-PROCEDURE:CURRENT-WINDOW, FRAME frmLinje:HANDLE, "IMAGE-Sko,Varehand-artikler,rectBrowseLinje,rectButTotal").
  DYNAMIC-FUNCTION("setNoMoveX",    THIS-PROCEDURE:CURRENT-WINDOW, FRAME frmLinje:HANDLE, "sokTilEdato,sokTilVPIdato,btnBlankLinjeFilter,fiTimeTot,btnTema,sokBestVerdi,btnCalTilDato").
  DYNAMIC-FUNCTION("setNoMoveY",    THIS-PROCEDURE:CURRENT-WINDOW, FRAME frmLinje:HANDLE, "ButikkNr,brwTrans").
  DYNAMIC-FUNCTION("setAddResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME frmLinje:HANDLE, "fi-cAnnenVarebeh,LinjeMerknad").
  DYNAMIC-FUNCTION("setAddMoveX",   THIS-PROCEDURE:CURRENT-WINDOW, FRAME frmLinje:HANDLE, "IMAGE-Sko").

  DYNAMIC-FUNCTION("setNoResizeY",  THIS-PROCEDURE:CURRENT-WINDOW, FRAME frmRegStat:HANDLE, "rectRegStat,brwRegStat,rectRegStatTransToolbar").

  DYNAMIC-FUNCTION("setSplitBarY" , THIS-PROCEDURE:CURRENT-WINDOW, btnSplitBarY:HANDLE IN FRAME frmLinje,NO).
  DYNAMIC-FUNCTION("setFollowSplitBarY",THIS-PROCEDURE:CURRENT-WINDOW, btnSplitBarY:HANDLE IN FRAME frmLinje,
                    STRING(rectBrowseLinje:HANDLE)
                  + "," + STRING(hBrowseLinje)
                  + "," + STRING(rectBrowseTrans:HANDLE)
                  + "," + STRING(hBrowseTrans:HANDLE)
                  + "," + STRING(sokArtikkelNr:HANDLE)
                  + "," + STRING(LevUke1:HANDLE)
                  + "," + STRING(LevUke2:HANDLE)
                  + "," + STRING(LevUke3:HANDLE)
                  + "," + STRING(LevUke4:HANDLE)
                  + "," + STRING(btnCalLevUke1:HANDLE)
                  + "," + STRING(btnCalLevUke2:HANDLE)
                  + "," + STRING(btnCalLevUke3:HANDLE)
                  + "," + STRING(btnCalLevUke4:HANDLE)
                  + "," + STRING(fiSumAnt:HANDLE)
                  + "," + STRING(fiSumVerdi:HANDLE)
                  + "," + DYNAMIC-FUNCTION("getToolbarHandles",hLinjeTransToolbar,"")
                   ).
  DYNAMIC-FUNCTION("setSplitBarYLimits",btnSplitBarY:HANDLE IN FRAME frmLinje,200,250).

  DYNAMIC-FUNCTION("setOrgWinSize", THIS-PROCEDURE:CURRENT-WINDOW,730,500,0,0).

  DYNAMIC-FUNCTION("initTranslation",THIS-PROCEDURE:CURRENT-WINDOW).

  DYNAMIC-FUNCTION("SetToolbar",hToolBar,"enable").


  LastInnstillinger().

END.

SUBSCRIBE TO "ArtBasEndret"          ANYWHERE.
SUBSCRIBE TO "ExcelSheetParams"      ANYWHERE.
SUBSCRIBE TO "getArtSokFargeKoding"  ANYWHERE.

TabStripChanged(11).

THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = FALSE.

IF NOT hFieldMap:AVAIL AND hBrowseListe:QUERY:IS-OPEN THEN
  hBrowseListe:QUERY:GET-FIRST().

/* IF hBrowseListe:NUM-ITERATIONS > 0 THEN  */
    APPLY "value-changed" TO hBrowseListe.

APPLY "ENTRY" TO hBrowseListe.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KopierRegRecord C-Win 
PROCEDURE KopierRegRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cButikkRowIdList   AS CHAR NO-UNDO.
DEF VAR cButikkIdList      AS CHAR NO-UNDO.
DEF VAR iReturn            AS INT  NO-UNDO.
DEF VAR iNumSelected       AS INT  NO-UNDO.
DEF VAR iNumTotal          AS INT  NO-UNDO.
DEF VAR cLinjeRowIdList    AS CHAR NO-UNDO.
DEF VAR cFileName          AS CHAR NO-UNDO.
DEF VAR chExcelApplication AS COM-HANDLE NO-UNDO.

IF hFieldMap:AVAIL THEN DO WITH FRAME frmLinje:
  IF ButikkNr:SCREEN-VALUE = ? OR ButikkNr:SCREEN-VALUE = "0" THEN
    DYNAMIC-FUNCTION("DoMessage",0,0,"Du må velge en butikk først","","").
  ELSE IF ButikkNr:SCREEN-VALUE = cButikkListe THEN
    DYNAMIC-FUNCTION("DoMessage",0,0,"Du har bare tilgang til å registrere for en butikk","","").
  ELSE DO:

    THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
    RUN JBoxSelector.w (THIS-PROCEDURE,0,
                        "Butiker;Butik;butnamn",
                        "WHERE CAN-DO('" + cButikkListe + "',STRING(butik)) AND butik NE " + ButikkNr:SCREEN-VALUE,
                        INPUT-OUTPUT cButikkRowIdList,
                        "Butik",
                        INPUT-OUTPUT cButikkIdList,
                        "","",
                        OUTPUT bOK).
    THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

    IF bOk THEN DO:
      IF cLevNrListe NE "*" THEN
        iNumTotal = INT(DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"recordcount")).
      ELSE
        iNumTotal = INT(DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"currentcount")).

      IF hBrowseLinje:NUM-SELECTED-ROWS > 0 THEN DO:
        RUN JBoxBrowseSelectMsg.w ("Kopier fra " + ButikkNr:SCREEN-VALUE + " til " + REPLACE(cButikkIdList,"|",",")
/*                                    + (IF DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"lastrowid") = "" THEN " (minst " + STRING(iNumTotal) + ")" ELSE ""), */
                                   ,hBrowseLinje:NUM-SELECTED-ROWS,
                                   iNumTotal,
                                   OUTPUT iReturn).
        IF iReturn = 2 THEN DO:
          DO ix = 1 TO hBrowseLinje:NUM-SELECTED-ROWS:
            IF hBrowseLinje:FETCH-SELECTED-ROW(ix) THEN
              cLinjeRowIdList = cLinjeRowIdList + hFieldMapLinje:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE + ",".
          END.
          IF cLevNrListe NE "*" THEN DO:
            iReturn = DYNAMIC-FUNCTION("DoMessage",0,3,"Skal eventuelt godkjente bestillinger settes som bestillingsforslag?"
                                                        ,"","").
            IF iReturn = 2 THEN RETURN.
          END.
        END.
        ELSE IF iReturn = 0 THEN RETURN.
      END.
      ELSE DO:
        IF cLevNrListe NE "*" THEN DO:
          iReturn = DYNAMIC-FUNCTION("DoMessage",0,3,"Ved kopier av registreringer for " + STRING(iNumTotal) + " artikler " + CHR(10) + "fra " + 
                                                       ButikkNr:SCREEN-VALUE + " til " + REPLACE(cButikkIdList,"|",",") + "," + CHR(10) +
                                                       "skal eventuelt godkjente bestillinger settes som bestillingsforslag?"
                                                      ,"","").
          IF iReturn = 2 THEN RETURN.
        END.
        ELSE IF DYNAMIC-FUNCTION("DoMessage",0,1,"Kopier registreringer for " + STRING(iNumTotal) + " artikler " + CHR(10) + "fra " + 
                                                  ButikkNr:SCREEN-VALUE + " til " + REPLACE(cButikkIdList,"|",","),"","") = 2 THEN RETURN.
      END.
      
      IF NOT DYNAMIC-FUNCTION("runproc","varebeh_kopier_til_butikk.p",
                          ButikkNr:SCREEN-VALUE + "¤" +
                          cButikkIdList + "¤" +
                          STRING(hFieldMap:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE) + "¤" +
                          DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"buffersandfields") + "¤" +
                          DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"basequery") + " " +
                          (IF cLinjeRowIdList NE "" THEN
                            "AND CAN-DO('" + TRIM(cLinjeRowIdList,",") + "',STRING(ROWID(VarebehLinje)))"
                           ELSE DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"queryfilter")) +
                          DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"queryjoin") + "¤" +
                          TRIM(cLinjeRowIdList,",") + "¤" +
                          DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"calcfieldproc") + "¤" + 
                          STRING(iReturn) /* 3 = sett nye bestillinger som registreringsforslag */
                          ,?) THEN
        DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
      ELSE DO:
        cFileName = DYNAMIC-FUNCTION("getTransactionMessage").
        IF cFileName NE "" THEN DO:
          chExcelApplication = DYNAMIC-FUNCTION("getExcelHandle").
          IF NOT VALID-HANDLE(chExcelApplication) THEN DO:
            IF DYNAMIC-FUNCTION("setWebDoc","Open",cFileName) NE "" THEN
              MESSAGE "Could not open file: " cFileName VIEW-AS ALERT-BOX ERROR.
          END.

          chExcelApplication:Workbooks:OpenText(cFileName,2,,,,,TRUE).

          chExcelApplication:VISIBLE = TRUE.
        END.
      END.
    END.
  END.
END.
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
DYNAMIC-FUNCTION("setAttribute",hBrowseTrans,"BufferExtraValues",cLevNrListe).

RUN SUPER.

/* Denne er bare satt (i MySaveBrowseFillIn) hvis ukenr ikke er angitt ved registrering av antall */
IF hCurrLevUke NE ? THEN 
  APPLY "entry" TO hCurrLevUke.

hCurrLevUke = ?.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LevOrdrebekrRecord C-Win 
PROCEDURE LevOrdrebekrRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cLookupValue       AS CHAR NO-UNDO.
DEF VAR bEAN               AS LOG  NO-UNDO.
DEF VAR cReturn            AS CHAR NO-UNDO.
DEF VAR cLevBasRowIdList   AS CHAR NO-UNDO.
DEF VAR cLevBasIdList      AS CHAR NO-UNDO.
DEF VAR cSourceFile        AS CHAR NO-UNDO.
DEF VAR cTargetFile        AS CHAR NO-UNDO.
DEF VAR hShell             AS COM-HANDLE NO-UNDO.
DEF VAR chExcelApplication AS COM-HANDLE NO-UNDO.
DEF VAR ix                 AS INT  NO-UNDO.
DEF VAR bBestReg           AS LOG  NO-UNDO.

THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
RUN JBoxSelector.w (THIS-PROCEDURE,0,
                    "LevBas;levnr;levnamn"
                  + ",VarebehLinje;!VarebehNr",
                    "WHERE true" 
                  + ",FIRST VarebehLinje OF LevBas NO-LOCK WHERE VarebehNr = " + STRING(hFieldMap:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE)
                    ,INPUT-OUTPUT cLevBasRowIdList,
                    "Levnr",
                    INPUT-OUTPUT cLevBasIdList,
                    "","",
                    OUTPUT bOK).
THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

IF bOk THEN DO:
  DO ix = 1 TO NUM-ENTRIES(cLevBasIdList,"|"):
    bBestReg = DYNAMIC-FUNCTION("getFieldList","varebehlinje;levnr,varebehlinjetrans;",
                                "WHERE varebehNr = " + STRING(hFieldMap:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE)
                              + " AND LevNr = " + ENTRY(ix,cLevBasIdList,"|")
                              + ",FIRST VarebehLinjeTrans NO-LOCK OF VarebehLinje"
                              + " WHERE RegistrertBestilling AND GodkjentBestilling") NE "".
    IF bBestReg AND
       DYNAMIC-FUNCTION("runProc","varebeh_ordrebekrlev.p",
                         STRING(hFieldMap:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE) + "," + ENTRY(ix,cLevBasIdList,"|") + ",godkjent," + cLevBekrUtvalg
                        ,httRapport) THEN DO:
      IF cLevBekrCat NE "" THEN DO:
        ASSIGN cSourceFile = DYNAMIC-FUNCTION("getTransactionMessage")
               cTargetFile = RIGHT-TRIM(cLevBekrCat,"\") + "\" + STRING(hFieldMapRegStat:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE) + "_levbekr_" + ENTRY(ix,cLevBasIdList,"|") + "_"
                           + REPLACE(DYNAMIC-FUNCTION("getFieldValues","LevBas","WHERE LevNr = " + ENTRY(ix,cLevBasIdList,"|"),"LevNamn"),'/','') 
                           + ".xls".
        IF cSourceFile NE "" THEN DO:     
          IF bFormatReport THEN
            LevOrdreBekrToExcel(cSourceFile,"godkjent",ENTRY(ix,cLevBasIdList,"|"),cTargetFile).
          ELSE
            OS-COPY VALUE(ENTRY(1,cSourceFile,"|")) VALUE(cTargetFile).
/*           IF NUM-ENTRIES(cLevBasIdList,"|") = 1 THEN DO:                              */
/*             IF DYNAMIC-FUNCTION("setWebDoc","Open",cTargetFile) NE "" THEN            */
/*                 MESSAGE "Could not open file: " cTargetFile VIEW-AS ALERT-BOX ERROR.  */
/*           END.                                                                        */
        END.
      END.
      ELSE IF cLevBekrCat = "" AND NUM-ENTRIES(cLevBasIdList,"|") = 1 THEN
        LevOrdreBekrToExcel(DYNAMIC-FUNCTION("getTransactionMessage"),"godkjent",ENTRY(ix,cLevBasIdList,"|"),"").
    END.
    ELSE IF NUM-ENTRIES(cLevBasIdList,"|") = 1 AND NOT bBestReg THEN DO:
      DYNAMIC-FUNCTION("DoMessage",0,0,"Ingen bestillinger registrert","","").
      RETURN.
    END.
  END.
  IF cLevBekrCat NE "" THEN DO:
    chExcelApplication = DYNAMIC-FUNCTION("getExcelHandle").
    chExcelApplication:QUIT().
    RELEASE OBJECT chExcelApplication NO-ERROR.
    CREATE "Shell.Application" hShell.
    hShell:Explore(cLevBekrCat).      
    RELEASE OBJECT hShell NO-ERROR.
  END.
END.

/* cLookupValue = "Levnr".                                                                                                                                                                  */
/*                                                                                                                                                                                          */
/* RUN JBoxDLookup.w ("LevBas;Levnr;levnamn,varebehlinje;!varebehnr",                                                                                                                       */
/*                    "where true,FIRST VarebehLinje OF LevBas WHERE VarebehLinje.VarebehNr = "                                                                                             */
/*                  + STRING(hFieldMap:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE)                                                                                                              */
/*                  + " AND CAN-DO('" + cLevNrListe + "',STRING(VarebehLinje.LevNr))",                                                                                                      */
/*                    INPUT-OUTPUT cLookupValue).                                                                                                                                           */
/*                                                                                                                                                                                          */
/* IF cLookupValue NE "" THEN DO:                                                                                                                                                           */
/*   IF cLevNrListe = "*" AND cButikkListe = "*" THEN DO:                                                                                                                                   */
/*     RUN JBoxDSimpleSelectList.w ("Alle leveringsmåter|0|Lagerførte varer (kjeldevarer)|1|Gjennomfaktureres|2|Lagerført eller gj.fakt (kontroll mot alle lev.måter)|3",?,OUTPUT cReturn). */
/*     IF cReturn = ? THEN RETURN.                                                                                                                                                          */
/*   END.                                                                                                                                                                                   */
/*   IF DYNAMIC-FUNCTION("runProc","varebeh_ordrebekrlev.p",                                                                                                                                */
/*                        STRING(hFieldMapRegStat:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE) + "," + cLookupValue + ",godkjent," + cReturn                                                     */
/*                       ,httRapport) THEN                                                                                                                                                  */
/*   LevOrdreBekrToExcel(DYNAMIC-FUNCTION("getTransactionMessage"),"godkjent",cLookupValue).                                                                                                */
/*   ELSE DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").                                                                                                */
/* END.                                                                                                                                                                                     */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MarkerLinjeMerknad C-Win 
PROCEDURE MarkerLinjeMerknad :
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

IF bOk THEN DO:
  cPrintMark = REPLACE(cIdList,"|",",").
  setNotat("markertlinjemerknad",REPLACE(cIdList,"|",",")).
END.
ELSE RETURN.

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
  StartSpesialSort("fjern_markering_av_valgt_spesialsortering",NO).

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
  MESSAGE "Er du helt sikker på at du vil slette hele varehåndteringsboken med alle artikler og registreringer?"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
        UPDATE plOk.
ELSE IF iTab = 3 THEN 
  MESSAGE "Er du helt sikker på at du vil slette hele artikkel med tilhørende registreringer?"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
        UPDATE plOk.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MySaveBrowseFillIn C-Win 
PROCEDURE MySaveBrowseFillIn :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM  ihFillIn AS HANDLE NO-UNDO.
DEF INPUT PARAM  ihBuffer AS HANDLE NO-UNDO.
DEF OUTPUT PARAM obOk     AS LOG NO-UNDO.

DEF VAR cFullWeek   AS CHAR NO-UNDO.

IF ihBuffer = hFieldMapTrans THEN DO WITH FRAME frmLinje:
  ASSIGN LevUke1 LevUke2 LevUke3 LevUke4.

  IF STRING(hFieldMapTrans:BUFFER-FIELD("ButikkNr"):BUFFER-VALUE) = ButikkNr:SCREEN-VALUE THEN DO:
    bOK = TRUE.
    CASE ihFillin:NAME:
      WHEN "fiBestilt1" THEN DO:
        IF INT(LevUke1) = 0 AND ihFillIn:SCREEN-VALUE NE "0" THEN bOK = FALSE.
        hCurrLevUke = LevUke1:HANDLE.
      END.
      WHEN "fiBestilt2" THEN DO:
        IF INT(LevUke2) = 0 AND ihFillIn:SCREEN-VALUE NE "0" THEN bOK = FALSE.
        hCurrLevUke = LevUke2:HANDLE.
      END.
      WHEN "fiBestilt3" THEN DO:
        IF INT(LevUke3) = 0 AND ihFillIn:SCREEN-VALUE NE "0" THEN bOK = FALSE.
        hCurrLevUke = LevUke3:HANDLE.
      END.
      WHEN "fiBestilt4" THEN DO:
        IF INT(LevUke4) = 0 AND ihFillIn:SCREEN-VALUE NE "0" THEN bOK = FALSE.
        hCurrLevUke = LevUke4:HANDLE.
      END.
    END CASE.
    IF NOT bOK THEN DO:
      DYNAMIC-FUNCTION("DoMessage",0,0,"Antall kan ikke registreres hvis ikke leveringsuke er utfylt","Feil","").
      ihFillin:SCREEN-VALUE = "0".
      PROCESS EVENTS.
      APPLY "entry" TO hCurrLevUke.
      RETURN.
    END.
    cFullWeek = "20" + SUBSTR(hCurrLevUke:SCREEN-VALUE,3) + SUBSTR(hCurrLevUke:SCREEN-VALUE,1,2).
    IF LENGTH(cFullWeek) NE 6 AND INT(ihFillIn:SCREEN-VALUE) NE 0 THEN DO:
      ihFillIn:SCREEN-VALUE = "0".
      ihFillIn:MODIFIED = NO.
      DYNAMIC-FUNCTION("DoMessage",0,0,"Ugyldig leveringsuke","Feil","").
    END.
    ELSE DO:
      IF INT(ihFillIn:SCREEN-VALUE) = 0 THEN cFullWeek = "0".

      DYNAMIC-FUNCTION("setPostUpdProc",DYNAMIC-FUNCTION("getAttribute",ihBuffer,"postUpdateProc")).
      obOk = DYNAMIC-FUNCTION("DoUpdate",ihBuffer:NAME,
                    DYNAMIC-FUNCTION("getAttribute",ihBuffer,"customUpdateValProc"),
                    "",
                    ihBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE,
                    DYNAMIC-FUNCTION("getAttribute",ihFillIn,"buffercolumn") + "," + hCurrLevUke:NAME + ",levnrlist",
                    ihFillIn:SCREEN-VALUE + "|" + cFullWeek + "|" + cLevNrListe,
                    TRUE).
      ASSIGN hCurrLevUke = ?
             bWarning    = TRUE.
    END.
  END.
  ELSE DO:
    MESSAGE "Uoverenstemmelse i angivelse av butikknr" SKIP 
             "(husk å trykke ENTER ved skifte av butikk)"
              VIEW-AS ALERT-BOX ERROR.
    ihFillin:SCREEN-VALUE = "0".
    APPLY "entry" TO ButikkNr.
  END.

END.
ELSE hCurrLevUke = ?.

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
DEF VAR rNew    AS ROWID NO-UNDO.
DEF VAR cType   AS CHAR NO-UNDO.
  
IF iTab = 1 THEN 
  TabStripChanged(12).

IF CAN-DO("1,2",STRING(iTab)) THEN
DO:
  RUN SUPER.

  DO WITH FRAME frmDetalj:
    ProfilNr:SCREEN-VALUE = cInitPrisProfil.
    cType   = DYNAMIC-FUNCTION("getFieldValues","SysPara",
                         "WHERE SysHId = 5 and SysGr = 6 and ParaNr = 1","Parameter1,Beskrivelse").

    ASSIGN VarebehType:SCREEN-VALUE = ENTRY(1,cType,"|")
           BeskrivelseVarebehType:SCREEN-VALUE = ENTRY(2,cType,"|")
           .
    APPLY "ENTRY" TO ProfilNr.
  END.
END.
ELSE IF iTab = 3 THEN DO WITH FRAME frmLinje:
  IF DYNAMIC-FUNCTION("getCurrentObject") = hUpdToolbar THEN DO:
    THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
    RUN ArtBasVedlikehold.w (THIS-PROCEDURE,"new",OUTPUT bOK).
    THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.
    THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().

  END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NyArtBas C-Win 
PROCEDURE NyArtBas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ifArtikkelNr AS DEC NO-UNDO.

IF NOT DYNAMIC-FUNCTION("runproc","varebeh_legg_til_art_messe.p",STRING(hFieldMap:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE) + "," +
                                                                 STRING(ifArtikkelNr),?) THEN
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
ELSE DO:
  DYNAMIC-FUNCTION("setCurrentObject",hBrowseLinje).
  hBrowseLinje:SET-REPOSITIONED-ROW(hBrowseLinje:FOCUSED-ROW,"conditional").
  RUN OpenQuery.
  bOk = hFieldMapLinje:FIND-FIRST("WHERE ArtikkelNr = " + STRING(ifArtikkelNr),NO-LOCK) NO-ERROR.
  IF bOk THEN DO:
    hBrowseLinje:QUERY:REPOSITION-TO-ROWID(hFieldMapLinje:ROWID) NO-ERROR.
    IF NOT ERROR-STATUS:ERROR THEN
      APPLY "value-changed" TO hBrowseLinje.
  END.
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
DEF VAR iUtskrNr     AS INT  NO-UNDO.
DEF VAR cBufsAndFlds AS CHAR NO-UNDO.
DEF VAR iy           AS INT  NO-UNDO.

IF iTab = 1 THEN DO WITH FRAME frmFilter:
  DYNAMIC-FUNCTION("SetCurrentObject",hBrowseListe).
  DYNAMIC-FUNCTION("SetAttribute",hBrowseListe,"queryfilter",
                   (IF fi-fProfilNr:SCREEN-VALUE NE "0" THEN
                     " where ProfilNr = " + fi-fProfilNr:SCREEN-VALUE
                    ELSE "where true") +
                   (IF fi-fMesseNr:SCREEN-VALUE NE "0" THEN
                     " AND MesseNr = " + fi-fMesseNr:SCREEN-VALUE
                    ELSE "") +
                   (IF fi-cVarebehBeskrivelse:SCREEN-VALUE NE "" THEN
                      IF fi-cVarebehBeskrivelse:SCREEN-VALUE BEGINS "*" THEN
                       " AND VarebehBeskrivelse MATCHES '" + fi-cVarebehBeskrivelse:SCREEN-VALUE + "*'"
                      ELSE
                       " AND VarebehBeskrivelse BEGINS '" + fi-cVarebehBeskrivelse:SCREEN-VALUE + "'"
                    ELSE "") 
                 + (IF cButikkListe NE "*" THEN " AND NOT Oppdatert" ELSE "") 
                   ).
  DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"queryfilter"," and false").
END.

ELSE IF iTab = 3 THEN DO WITH FRAME frmLinje:
  ASSIGN sokFraEdato sokFraVPIdato sokTilEdato sokTilVPIdato rsBestilling sokBestVerdi
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
         sokBestVerdi:MODIFIED    = FALSE
         .
  DYNAMIC-FUNCTION("SetCurrentObject",hBrowseLinje).
  DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"queryfilter","").

  DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"QueryFilter",
                   (IF sokAvdelingNr:SCREEN-VALUE NE "0" THEN
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
                    ELSE IF cLevNrListe NE "*" THEN
                      " AND CAN-DO('" + cLevNrListe + "',STRING(LevNr))"
                    ELSE "")
                 + (IF sokArtikkelNr:SCREEN-VALUE NE "0" THEN
                     " AND ArtikkelNr = " + sokArtikkelNr:SCREEN-VALUE
                    ELSE "")
                    
/*                  + (IF sokBeskr:SCREEN-VALUE NE "" THEN           */
/*                      " AND Beskr " +                              */
/*                      (IF INDEX(sokBeskr,"*") > 0 THEN             */
/*                        "MATCHES '" + sokBeskr:SCREEN-VALUE + "*'" */
/*                       ELSE                                        */
/*                        "BEGINS '" + sokBeskr:SCREEN-VALUE + "'")  */
/*                     ELSE "")                                      */
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
                 + (IF tbKjedeVare:CHECKED THEN
                     " AND Kjedevare"
                    ELSE "")
                 + (IF tbGjennomfaktureres:CHECKED THEN 
                     " AND Gjennomfaktureres"
                    ELSE "")
                    ).

  IF sokAktNr:SCREEN-VALUE NE "0" THEN
    DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"queryjoin",cVarebehlinjeJoin +
                                    ",first VgAkt WHERE VgAkt.Vg = VarebehLinje.Vg AND AktNr = " + sokAktNr:SCREEN-VALUE 
                                  + ",first Aktivitet OF VgAkt").
  ELSE IF sokAktBeskrivelse:SCREEN-VALUE NE "" THEN
    DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"queryjoin",cVarebehlinjeJoin +
                                    ",EACH VgAkt WHERE VgAkt.Vg = VarebehLinje.Vg" 
                                  + ",first Aktivitet OF VgAkt WHERE Beskrivelse " 
                                  + (IF INDEX(sokAktBeskrivelse:SCREEN-VALUE,"*") > 0 THEN 
                                      "MATCHES '" + sokAktBeskrivelse:SCREEN-VALUE + "*'"
                                     ELSE
                                      "BEGINS '" + sokAktBeskrivelse:SCREEN-VALUE + "'")).
  ELSE
    DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"queryjoin",cVarebehlinjeJoin + cAktivitetJoin).

  IF rsBestilling > 0 AND ButikkNr:SCREEN-VALUE NE "0" AND ButikkNr:SCREEN-VALUE NE ? THEN 
    DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"calcparamBBestilt",ButikkNr:SCREEN-VALUE + "¤" + rsBestilling:SCREEN-VALUE).
  ELSE IF ButikkNr:SCREEN-VALUE NE "0" AND ButikkNr:SCREEN-VALUE NE ? THEN
    DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"calcparamBBestilt",ButikkNr:SCREEN-VALUE).
  ELSE
    DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"calcparamBBestilt","").

  IF rsGodkjent > 0 AND ButikkNr:SCREEN-VALUE NE "0" AND ButikkNr:SCREEN-VALUE NE ? THEN 
    DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"calcparamGodkjentBest",ButikkNr:SCREEN-VALUE + "¤" + rsGodkjent:SCREEN-VALUE).
  ELSE IF ButikkNr:SCREEN-VALUE NE "0" AND ButikkNr:SCREEN-VALUE NE ? THEN
    DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"calcparamGodkjentBest",ButikkNr:SCREEN-VALUE).
  ELSE
    DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"calcparamGodkjentBest","").

  IF sokBestVerdi:SENSITIVE AND sokBestVerdi > 0 THEN DO:
    DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"calcparamFBestVerdi",ButikkNr:SCREEN-VALUE + "¤" + sokBestVerdi:SCREEN-VALUE).
    hBestVerdi:VISIBLE = TRUE.
  END.
  ELSE DO:
    DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"calcparamFBestVerdi","").
    hBestVerdi:VISIBLE = FALSE.
  END.

  IF cmbTema:SCREEN-VALUE NE "0" AND cmbTema:SCREEN-VALUE NE ? THEN DO:
    DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"calcparamTemaSeq",cmbTema:SCREEN-VALUE).
    DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"querysort","TemaSeq").
    DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"localsort","TemaSeq").
    IF VALID-HANDLE(hSearchLinje) THEN hSearchLinje:HIDDEN = YES.
  END.
  ELSE DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"calcparamTemaSeq","").

/*   IF tbKjedeVare:CHECKED THEN                                                       */
/*     DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"calcparamKjedeVareFilter","yes"). */
/*   ELSE                                                                              */
/*     DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"calcparamKjedeVareFilter","").    */

  DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"calcparamBMatchMerknad",REPLACE(sokLinjeMerknad:SCREEN-VALUE,",","¤")).
END.

ELSE IF iTab = 4 THEN DO WITH FRAME frmRegStat:
  IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowseRegStat THEN DO:
    ASSIGN sokButikkNr:MODIFIED     = FALSE
           sokAvdelingNavn:MODIFIED = FALSE
           .
      
    DYNAMIC-FUNCTION("setAttribute",hBrowseRegStat,"QueryFilter",
                     (IF sokButikkNr:SCREEN-VALUE NE "0" THEN
                       " AND ButikkNr = " + sokButikkNr:SCREEN-VALUE
                      ELSE IF sokButNamn:SCREEN-VALUE NE "" AND NUM-ENTRIES(cButikerRowIdList) > 1 THEN
                        " AND CAN-DO('" + REPLACE(cButikerIdList,"|",",") + "',STRING(ButikkNr))"
                      ELSE "") 
                   + " AND CAN-DO('" + cButikkListe + "',STRING(ButikkNr))"
                   + DYNAMIC-FUNCTION("getAttribute",hBrowseRegStat,"querywhere")
                      ).
    IF sokButNamn:SCREEN-VALUE NE "" AND NUM-ENTRIES(cButikerRowIdList) < 2 THEN
      DYNAMIC-FUNCTION("setAttribute",hBrowseRegStat,"queryjoin",
                        ",FIRST Butiker NO-LOCK WHERE Butik = VarebehLinjeThode.ButikkNr"
                      + " AND "
                      +  (IF INDEX(sokButNamn:SCREEN-VALUE,"*") > 0 THEN 
                           "ButNamn MATCHES '" + sokButNamn:SCREEN-VALUE + "*'"
                          ELSE
                           "ButNamn BEGINS '" + sokButNamn:SCREEN-VALUE + "'")
                       ).
    ELSE 
      DYNAMIC-FUNCTION("setAttribute",hBrowseRegStat,"queryjoin",cVarebehlinjeThodeJoin).
  END.
END.

RUN SUPER.

IF iTab = 1 THEN DO:
  IF NOT hFieldMap:AVAIL AND hBrowseListe:QUERY:IS-OPEN THEN
    hBrowseListe:QUERY:GET-FIRST().
  hBrowseListe:HELP = DYNAMIC-FUNCTION("getAttribute",hBrowseListe,"TotalCount").
  APPLY "entry" TO hBrowseListe.
END.
ELSE IF iTab = 3 THEN DO:

  IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowseTrans AND hFieldMap:AVAIL THEN DO:
    IF DYNAMIC-FUNCTION("getFieldValues","VarebehLinjeThode",
                         "WHERE VarebehNr = " + STRING(hFieldMap:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE)
                        + " AND ButikkNr  = " + ButikkNr:SCREEN-VALUE,
                         "Godkjent") = "no" 
       AND hFieldMap:BUFFER-FIELD("Oppdatert"):BUFFER-VALUE = FALSE THEN DO:
      LinkOverlays(TRUE).
    END.
    ELSE 
      LinkOverlays(FALSE).
  END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpphevGodkjBestRecord C-Win 
PROCEDURE OpphevGodkjBestRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cRowIdList AS CHAR NO-UNDO.

IF ButikkNr:SCREEN-VALUE IN FRAME frmLinje = "0" OR ButikkNr:SCREEN-VALUE = ? THEN DO:
  DYNAMIC-FUNCTION("DoMessage",0,0,"En butikk må velges før oppheving av godkjenning","","").
  RETURN.
END.

IF hBrowseLinje:NUM-SELECTED-ROWS = 0 THEN DO:
  DYNAMIC-FUNCTION("DoMessage",0,0,"Velg en eller flere artikler som skal få opphevet godkjenning","","").
  RETURN.
END.

IF DYNAMIC-FUNCTION("DoMessage",0,1,"Bekreft at oppheving av godkjenning av bestillinger for " + STRING(hBrowseLinje:NUM-SELECTED-ROWS) + " artikler","","") = 1 THEN DO:
  DO ix = 1 TO hBrowseLinje:NUM-SELECTED-ROWS:
    IF hBrowseLinje:FETCH-SELECTED-ROW(ix) THEN
      cRowIdList = cRowIdList + hFieldMapLinje:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE + ",".
  END.
  
  IF DYNAMIC-FUNCTION("runproc","varebehlinje_vedl_godkjenning.p",TRIM(cRowIdList,",") + ";" + ButikkNr:SCREEN-VALUE + ";no",?) THEN DO:
    DYNAMIC-FUNCTION("setCurrentObject",hBrowseLinje).
    RUN OpenQuery.
  END.
  ELSE DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
END.
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
/* IF NOT hFieldMap:AVAIL THEN DO:                                                                                                                  */
/*   IF DYNAMIC-FUNCTION("runProc","varebeh_sammendrag.p",                                                                                          */
/*                        STRING(hFieldMapRegStat:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE) + "," +                                                   */
/*                        STRING(hFieldMapRegStat:BUFFER-FIELD("ButikkNr"):BUFFER-VALUE) + "," +                                                    */
/*                        cLevNr + ",," +                                                                                                           */
/*                        (IF bEAN THEN "yes" ELSE "")                                                                                              */
/*                       ,?) THEN                                                                                                                   */
/*     OrdreBekreftToExcel(DYNAMIC-FUNCTION("getTransactionMessage"),"but",STRING(hFieldMapRegStat:BUFFER-FIELD("ButikkNr"):BUFFER-VALUE),bEAN,""). */
/*   ELSE DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").                                                        */
/* END.                                                                                                                                             */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RemoveFromTemaRecord C-Win 
PROCEDURE RemoveFromTemaRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cRowIdList AS CHAR NO-UNDO.
DEF VAR cTotTema    AS CHAR NO-UNDO.
DEF VAR cLevTema    AS CHAR NO-UNDO.

IF NOT hFieldMap:AVAIL THEN RETURN.

cTotTema = SUBSTR(cmbTema:LIST-ITEM-PAIRS IN FRAME frmLinje,4).

DO ix = 1 TO NUM-ENTRIES(cTotTema,"|") BY 2:
  IF INT(DYNAMIC-FUNCTION("getFieldValues","VarebokTemaHode","WHERE VbTemeNr = " + ENTRY(ix + 1,cTotTema,"|"),"LevNr")) NE 0 THEN
    cLevTema = cLevTema + ENTRY(ix + 1,cTotTema,"|") + ",".
END.
cLevTema = TRIM(cLevTema,",").

IF cmbTema:SCREEN-VALUE IN FRAME frmLinje = "0" OR cmbTema:SCREEN-VALUE = ? THEN DO:
  DYNAMIC-FUNCTION("DoMessage",0,0,"Tema må velges i filteret før artikler kan fjernes","","").
  RETURN.
END.

IF CAN-DO("1",cAdgang) AND NOT CAN-DO(cLevTema,cmbTema:SCREEN-VALUE) THEN DO:
  DYNAMIC-FUNCTION("DoMessage",0,0,"Du kan ikke fjerne artikler fra dette tema","","").
  RETURN.
END.

IF hBrowseLinje:NUM-SELECTED-ROWS = 0 THEN DO:
  DYNAMIC-FUNCTION("DoMessage",0,0,"Velg en eller flere artikler som skal fjernes fra tema","","").
  RETURN.
END.

IF DYNAMIC-FUNCTION("DoMessage",0,1,"Bekreft at " + STRING(hBrowseLinje:NUM-SELECTED-ROWS) + " skal fjernes fra tema " + DYNAMIC-FUNCTION("getDropDownLabel",cmbTema:HANDLE,"|"),"","") = 1 THEN DO:
  DO ix = 1 TO hBrowseLinje:NUM-SELECTED-ROWS:
    IF hBrowseLinje:FETCH-SELECTED-ROW(ix) THEN
      cRowIdList = cRowIdList + hFieldMapLinje:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE + ",".
  END.
  
  IF DYNAMIC-FUNCTION("runproc","varebehlinje_vedl_tema.p",STRING(hFieldMap:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE) + ";" +
                                                           TRIM(cRowIdList,",") + ";" + cmbTema:SCREEN-VALUE + ";no",?) THEN DO:
    DYNAMIC-FUNCTION("setCurrentObject",hBrowseLinje).
    RUN OpenQuery.
  END.
  ELSE DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
END.

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
DEF VAR iy       AS INT  NO-UNDO.
DEF VAR bMark    AS LOG  NO-UNDO.

IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowseLinje THEN DO: 
  
  IF hBestilt:BUFFER-VALUE THEN DO:

    hGodkjentCol:FONT   = iFontGodkjent NO-ERROR.
    hGodkjentCol:FORMAT = CHR(254) + "/"  + CHR(168) NO-ERROR.

    IF hGodkjent:BUFFER-VALUE THEN
      hSekv:BGCOLOR = iColNumBestiltGodk NO-ERROR.
    ELSE
      hSekv:BGCOLOR = iColNumBestiltForsl NO-ERROR.
  END.
  ELSE DO:
    hGodkjentCol:FONT   = ? NO-ERROR.
    hGodkjentCol:FORMAT = "v/" NO-ERROR.
  END.

  IF hFieldRGBcolor:BUFFER-VALUE > 0 THEN
    hBeskr:BGCOLOR = setFieldColor(hFieldRGBcolor:BUFFER-VALUE) NO-ERROR.
                            
/*   ASSIGN hBrwColKjedeVare:FONT   = iFontGodkjent                */
/*          hBrwColKjedeVare:FORMAT = CHR(254) + "/"  + CHR(168)   */
/*          hBrwColGjFakt:FONT      = iFontGodkjent                */
/*          hBrwColGjFakt:FORMAT    = CHR(254) + "/"  + CHR(168).  */

/*   IF cPrintMark NE "" THEN DO:                                                                  */
/*     DO iy = 1 TO NUM-ENTRIES(cPrintMark):                                                       */
/*       IF hLinjeMerknad:BUFFER-VALUE MATCHES "*" + ENTRY(iy,cPrintMark) + "*" THEN bMark = TRUE. */
/*     END.                                                                                        */
/*     IF bMark THEN hBeskr:BGCOLOR = iColNumBeskr.                                                */
/*   END.                                                                                          */

END.
ELSE IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowseRegStatTrans THEN DO:
  IF hTransGodkjent:BUFFER-VALUE THEN
    hBrwLinjeTransArtikkelnr:BGCOLOR = iColNumBestiltGodk NO-ERROR.
  ELSE
    hBrwLinjeTransArtikkelnr:BGCOLOR = iColNumBestiltForsl NO-ERROR.
END.
ELSE IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowseTrans THEN DO ix = 1 TO 4:
  hRegLevUke[ix]:BGCOLOR = iGrayColor NO-ERROR.
END.
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
IF iTab = 2 THEN DO WITH FRAME frmDetalj:        
  IF Oppdatert:CHECKED THEN
    DYNAMIC-FUNCTION("setAttribute",hFieldMap,"BufferExtraValues",
                     DYNAMIC-FUNCTION("getASuserid") + "|" + STRING(TODAY)).
  ELSE
    DYNAMIC-FUNCTION("setAttribute",hFieldMap,"BufferExtraValues","|").
END.

RUN SUPER.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetBestiltVarsel C-Win 
PROCEDURE SetBestiltVarsel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iAntall  AS INT  NO-UNDO.
DEF VAR iReturn  AS INT  NO-UNDO.
DEF VAR iocValue AS CHAR NO-UNDO.

IF NOT hFieldMap:AVAIL THEN RETURN.

iocValue = getFromNotat("bestillingsvarsel").

IF int(iocValue) = 0 THEN
    iocValue = "99".
iReturn = 0.
RUN JBoxAskForValue.w ("Antall som gir advarsel ved bestilling",
                        "INTEGER|>>>>>9",
                        INPUT-OUTPUT iocValue, 
                        OUTPUT iReturn).

IF iReturn = 2 THEN DO:
  iBestVarsel = INT(iocValue).
  setNotat("bestillingsvarsel",iocValue).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetColorBestilt C-Win 
PROCEDURE SetColorBestilt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iColor AS INT NO-UNDO.

IF NOT hFieldMap:AVAIL THEN RETURN.

iColor = INT(getFromNotat("bestillingsfarge")).

RUN JBoxDSelectColor.w ("Sekv.nr felt for bestillinger",INPUT-OUTPUT iColor,OUTPUT iColNumBestilt,OUTPUT bOK).

IF bOK THEN
  setNotat("bestillingsfarge",STRING(iColor)).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetColorBestiltForsl C-Win 
PROCEDURE SetColorBestiltForsl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iColor AS INT NO-UNDO.

IF NOT hFieldMap:AVAIL THEN RETURN.

iColor = INT(getFromNotat("bestilling_forsl")).

RUN JBoxDSelectColor.w ("Sekv.nr felt for bestillinger",INPUT-OUTPUT iColor,OUTPUT iColNumBestiltForsl,OUTPUT bOK).

IF bOK THEN
  setNotat("bestilling_forsl",STRING(iColor)).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetColorBestiltGodk C-Win 
PROCEDURE SetColorBestiltGodk :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iColor AS INT NO-UNDO.

IF NOT hFieldMap:AVAIL THEN RETURN.

iColor = INT(getFromNotat("bestilling_godk")).

RUN JBoxDSelectColor.w ("Sekv.nr felt for bestillinger",INPUT-OUTPUT iColor,OUTPUT iColNumBestiltGodk,OUTPUT bOK).

IF bOK THEN
  setNotat("bestilling_godk",STRING(iColor)).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetColorLinjeMerknad C-Win 
PROCEDURE SetColorLinjeMerknad :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iColor AS INT NO-UNDO.

IF NOT hFieldMap:AVAIL THEN RETURN.

iColor = INT(getFromNotat("merknadsfarge")).

RUN JBoxDSelectColor.w ("Art.tekst farge for linjemerknad",INPUT-OUTPUT iColor,OUTPUT iColNumBeskr,OUTPUT bOK).

IF bOK THEN
  setNotat("merknadsfarge",STRING(iColor)).



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

DEF VAR hSourceBuffer AS HANDLE NO-UNDO.
DEF VAR hTargetBuffer AS HANDLE NO-UNDO.
DEF VAR cOppmerking   AS CHAR   NO-UNDO.
DEF VAR iy            AS INT    NO-UNDO.
DEF VAR cList         AS CHAR   NO-UNDO.
DEF VAR cSelList      AS CHAR   NO-UNDO.
DEF VAR cSelectedRows AS CHAR   NO-UNDO.
DEF VAR cPreSelected  AS CHAR   NO-UNDO.
DEF VAR hSelector     AS HANDLE NO-UNDO.
DEF VAR hHandle       AS HANDLE NO-UNDO.
DEF VAR hPanelFrame   AS HANDLE NO-UNDO.

IF ihBrwSource:QUERY:GET-BUFFER-HANDLE(1):NAME = "Strekkode" THEN
  ihBrwSource:WINDOW:TITLE = "Velg strekkoder for å legge til [" + STRING(hFieldMapLinje:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) + "]".

ELSE IF ihBrwSource:QUERY:GET-BUFFER-HANDLE(1):NAME = "LevSort" THEN 
  ASSIGN ihBrwSource:GET-BROWSE-COLUMN(1):WIDTH-PIXELS = 40
         ihBrwSource:GET-BROWSE-COLUMN(2):WIDTH-PIXELS = 100
         ihBrwSource:GET-BROWSE-COLUMN(3):WIDTH-PIXELS = 100
         ihBrwTarget:GET-BROWSE-COLUMN(1):WIDTH-PIXELS = 40
         ihBrwTarget:GET-BROWSE-COLUMN(2):WIDTH-PIXELS = 100
         ihBrwTarget:GET-BROWSE-COLUMN(3):WIDTH-PIXELS = 100
         .

ELSE IF ihBrwSource:QUERY:GET-BUFFER-HANDLE(1):NAME = "_file" THEN DO:

  hSourceBuffer = ihBrwSource:QUERY:GET-BUFFER-HANDLE(1).
  hTargetBuffer = ihBrwTarget:QUERY:GET-BUFFER-HANDLE(1).

  DYNAMIC-FUNCTION("setAttribute",ihBrwSource,"basequery","where true").
  DYNAMIC-FUNCTION("setAttribute",ihBrwSource,"querysort","Description").
  DYNAMIC-FUNCTION("setAttribute",ihBrwSource,"uselocaldata","yes").
  DYNAMIC-FUNCTION("setAttribute",ihBrwTarget,"uselocaldata","yes").

  cOppmerking = hFieldMap:BUFFER-FIELD("Oppmerking"):BUFFER-VALUE.

  /* Fill temp-table: */
  DO ix = 1 TO NUM-ENTRIES(cOppmerking,"¤"):
    DO iy = 1 TO NUM-ENTRIES(ENTRY(ix,cOppmerking,"¤")):
      IF NOT CAN-DO(cList,ENTRY(iy,ENTRY(ix,cOppmerking,"¤"))) THEN DO:
        hSourceBuffer:BUFFER-CREATE().
        ASSIGN hSourceBuffer:BUFFER-FIELD("Description"):BUFFER-VALUE = ENTRY(iy,ENTRY(ix,cOppmerking,"¤"))
               hSourceBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE = "rowid" + STRING(ix)
               cList = cList + ENTRY(iy,ENTRY(ix,cOppmerking,"¤")) + ","
               .
      END.

      /* If any records should be pre-selected: */
      IF LOOKUP(ENTRY(iy,ENTRY(ix,cOppmerking,"¤")),sokLinjeMerknad:SCREEN-VALUE IN FRAME frmLinje) > 0 
         AND NOT CAN-DO(cSelList,ENTRY(iy,ENTRY(ix,cOppmerking,"¤"))) THEN DO:
        hTargetBuffer:BUFFER-CREATE().
        ASSIGN hTargetBuffer:BUFFER-FIELD("Description"):BUFFER-VALUE = ENTRY(iy,ENTRY(ix,cOppmerking,"¤"))
               hTargetBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE = "rowid" + STRING(ix)
               cSelectedRows = cSelectedRows + "rowid" + STRING(ix) + ","
               cSelList      = cSelList + ENTRY(iy,ENTRY(ix,cOppmerking,"¤")) + ","
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

ELSE IF ihBrwSource:QUERY:GET-BUFFER-HANDLE(1):NAME = "_field" THEN DO:

  hSourceBuffer = ihBrwSource:QUERY:GET-BUFFER-HANDLE(1).
  hTargetBuffer = ihBrwTarget:QUERY:GET-BUFFER-HANDLE(1).

  DYNAMIC-FUNCTION("setAttribute",ihBrwSource,"basequery","where true").
  DYNAMIC-FUNCTION("setAttribute",ihBrwSource,"querysort","Description").
  DYNAMIC-FUNCTION("setAttribute",ihBrwSource,"uselocaldata","yes").
  DYNAMIC-FUNCTION("setAttribute",ihBrwTarget,"uselocaldata","yes").

  ASSIGN cOppmerking  = hFieldMap:BUFFER-FIELD("Oppmerking"):BUFFER-VALUE
         cPreSelected = getFromNotat("markertlinjemerknad")
         .

  /* Fill temp-table: */
  DO ix = 1 TO NUM-ENTRIES(cOppmerking,"¤"):
    DO iy = 1 TO NUM-ENTRIES(ENTRY(ix,cOppmerking,"¤")):
      IF NOT CAN-DO(cList,ENTRY(iy,ENTRY(ix,cOppmerking,"¤"))) THEN DO:
        hSourceBuffer:BUFFER-CREATE().
        ASSIGN hSourceBuffer:BUFFER-FIELD("Description"):BUFFER-VALUE = ENTRY(iy,ENTRY(ix,cOppmerking,"¤"))
               hSourceBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE = "rowid" + STRING(ix)
               cList = cList + ENTRY(iy,ENTRY(ix,cOppmerking,"¤")) + ","
               .
      END.

      IF LOOKUP(ENTRY(iy,ENTRY(ix,cOppmerking,"¤")),cPreSelected) > 0 
         AND NOT CAN-DO(cSelList,ENTRY(iy,ENTRY(ix,cOppmerking,"¤"))) THEN DO:
        hTargetBuffer:BUFFER-CREATE().
        ASSIGN hTargetBuffer:BUFFER-FIELD("Description"):BUFFER-VALUE = ENTRY(iy,ENTRY(ix,cOppmerking,"¤"))
               hTargetBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE = "rowid" + STRING(ix)
               cSelectedRows = cSelectedRows + "rowid" + STRING(ix) + ","
               cSelList      = cSelList + ENTRY(iy,ENTRY(ix,cOppmerking,"¤")) + ","
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

ELSE IF ihBrwSource:QUERY:GET-BUFFER-HANDLE(1):NAME = "LevBas"
        AND ihBrwSource:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD(6):NAME = "VarebehNr" THEN DO: /* Nr 6 pga 3 ekstra felter */

  hSelector = SOURCE-PROCEDURE.
  RUN LoadPanel IN hSelector ("VarebehLevBekrPanel.w",OUTPUT hLevBekrUtvalgPanel).
  RUN InitializeObject IN hLevBekrUtvalgPanel (ihBrwTarget,THIS-PROCEDURE).

  hHandle = ihBrwSource:WINDOW.
  hHandle:HEIGHT-PIXELS = hHandle:HEIGHT-PIXELS + 150.
  APPLY "window-resized" TO hHandle.

  DYNAMIC-FUNCTION("adjustBrowseHeight" IN hSelector,-150).
  hPanelFrame = DYNAMIC-FUNCTION("getFrameHandle" IN hLevBekrUtvalgPanel).
  hPanelFrame:Y = ihBrwSource:Y + ihBrwSource:HEIGHT-PIXELS + 5.

END.
ELSE IF ihBrwSource:QUERY:GET-BUFFER-HANDLE(1):NAME = "Butiker" THEN DO:

  hSelector = SOURCE-PROCEDURE.
  RUN LoadPanel IN hSelector ("VarebehButBekrPanel.w",OUTPUT hButBekrUtvalgPanel).
  RUN InitializeObject IN hButBekrUtvalgPanel (ihBrwTarget,THIS-PROCEDURE).

  hHandle = ihBrwSource:WINDOW.
  hHandle:HEIGHT-PIXELS = hHandle:HEIGHT-PIXELS + 50.
  APPLY "window-resized" TO hHandle.

  DYNAMIC-FUNCTION("adjustBrowseHeight" IN hSelector,-50).
  hPanelFrame = DYNAMIC-FUNCTION("getFrameHandle" IN hButBekrUtvalgPanel).
  hPanelFrame:Y = ihBrwSource:Y + ihBrwSource:HEIGHT-PIXELS + 5.

END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettGodkjenningRecord C-Win 
PROCEDURE SettGodkjenningRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cButikerIdList      AS CHAR NO-UNDO.
DEF VAR cButikerRowIdList   AS CHAR NO-UNDO.
DEF VAR cOrgButRowIdList    AS CHAR NO-UNDO.
DEF VAR iReturn             AS INT  NO-UNDO.

IF NOT hFieldMap:AVAIL THEN RETURN.

ASSIGN 
  cButikerRowIdList = DYNAMIC-FUNCTION("getRowIdList","VarebehLinjeTHode","","WHERE VarebehNr = " + STRING(hFieldMap:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE) + " AND godkjent")
  cOrgButRowIdlist  = cButikerRowIdList.


THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
RUN JBoxSelector.w (THIS-PROCEDURE,0,
                    "VarebehLinjeTHode;godkjent;!VarebehNr,Butiker;Butik;Butnamn;Sentrallager"
                   ,"WHERE Varebehnr = " 
                      + STRING(hFieldMap:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE) 
                      + ",FIRST Butiker WHERE Butiker.Butik = ButikkNr NO-LOCK BY Butik"
                   ,INPUT-OUTPUT cButikerRowIdList,
                    "Butik",
                    INPUT-OUTPUT cButikerIdList,
                    "","",
                    OUTPUT bOK).
THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

IF bOk THEN DO:
  DO ix = 1 TO NUM-ENTRIES(cButikerRowIdList):
    IF NOT CAN-DO(cOrgButRowIdList,ENTRY(ix,cButikerRowIdList)) THEN
      DYNAMIC-FUNCTION("DoUpdate","VarebehLinjeThode",
                    "ignore",
                    "",
                    ENTRY(ix,cButikerRowIdList),
                    "Godkjent",
                    "YES",
                    NO).
  END.
  DO ix = 1 TO NUM-ENTRIES(cOrgButRowIdList):
    IF NOT CAN-DO(cButikerRowIdList,ENTRY(ix,cOrgButRowIdList)) THEN
      DYNAMIC-FUNCTION("DoUpdate","VarebehLinjeThode",
                    "ignore",
                    "",
                    ENTRY(ix,cOrgButRowIdList),
                    "Godkjent",
                    "NO",
                    NO).
  END.

  DYNAMIC-FUNCTION("DoCommit",NO).

  DYNAMIC-FUNCTION("setCurrentObject",hBrowseRegStat).
  RUN OpenQuery.

END.

/* IF hFieldMapRegStat:AVAIL AND hFieldMapRegStat:BUFFER-FIELD("Godkjent"):BUFFER-VALUE = FALSE THEN DO:                                          */
/*   iReturn = DYNAMIC-FUNCTION("DoMessage",0,1,"Godkjenn registreringer for butikk &1" + CHR(10) +                                               */
/*                                              "Godkjenning innebærer at ingen flere registreringer kan gjøres for vareboken på denne butikken", */
/*                                              "Godkjenn registrering",hFieldMapRegStat:BUFFER-FIELD("ButNamn"):BUFFER-VALUE).                   */
/*   IF iReturn = 1 THEN DO:                                                                                                                      */
/*     hFieldMapRegStat:BUFFER-FIELD("Godkjent"):BUFFER-VALUE = TRUE.                                                                             */
/*     DYNAMIC-FUNCTION("DoUpdate","VarebehLinjeThode",                                                                                           */
/*                   "ignore",                                                                                                                    */
/*                   "",                                                                                                                          */
/*                   hFieldMapRegStat:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE,                                                                     */
/*                   "Godkjent",                                                                                                                  */
/*                   STRING(TRUE),                                                                                                                */
/*                   TRUE).                                                                                                                       */
/*     hBrowseRegStat:REFRESH().                                                                                                                  */
/*   END.                                                                                                                                         */
/* END.                                                                                                                                           */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettLevUkerRecord C-Win 
PROCEDURE SettLevUkerRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR bAllebut    AS LOG  NO-UNDO.
DEF VAR cLevUkeList AS CHAR NO-UNDO.

IF cButikkListe NE "*" AND ButikkNr:SCREEN-VALUE IN FRAME frmLinje = ? THEN DO:
  DYNAMIC-FUNCTION("DoMessage",0,0,"Du må velge butikk for å benytte denne funksjonen","","").
  RETURN.
END.

RUN dSettLevuke.w (hBrowseLinje:NUM-SELECTED-ROWS,
                   LevUke1:HANDLE IN FRAME frmLinje,
                   ButikkNr:SCREEN-VALUE,
                   IF cButikkListe = "*" THEN YES ELSE NO,
                   OUTPUT bOk,
                   OUTPUT bAlleBut,
                   OUTPUT cLevUkeList).

IF bOK THEN DO:
  IF NOT DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowseLinje,"varebehlinje_sett_levuker.p",
                          (IF bAlleBut THEN "" ELSE ButikkNr:SCREEN-VALUE) + ";" + cLevUkeList) THEN
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").

  ELSE APPLY "value-changed" TO hBrowseLinje.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SjekkBestCLRecord C-Win 
PROCEDURE SjekkBestCLRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cFileName           AS CHAR NO-UNDO.
DEF VAR chExcelApplication  AS COM-HANDLE NO-UNDO.
DEF VAR cButikerIdList      AS CHAR NO-UNDO.
DEF VAR cButikerRowIdList   AS CHAR NO-UNDO.

IF NOT hFieldMap:AVAIL THEN RETURN.

THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
RUN JBoxSelector.w (THIS-PROCEDURE,0,
                    "VarebehLinjeTHode;ButikkNr;godkjent,Butiker;Butnamn"
                   ,"where Varebehnr = " + STRING(hFieldMap:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE) 
                     + " and Godkjent"
                     + ",FIRST Butiker NO-LOCK WHERE Butik = ButikkNr"
                   ,INPUT-OUTPUT cButikerRowIdList,
                    "ButikkNr",
                    INPUT-OUTPUT cButikerIdList,
                    "","",
                    OUTPUT bOK).
THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

IF bOk THEN DO:
  IF DYNAMIC-FUNCTION("runProc","varebeh_avstem_ordre.p",
                      STRING(hFieldMap:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE) + "," + cButikerIdList
                      ,?) THEN DO:
    cFileName = DYNAMIC-FUNCTION("getTransactionMessage").
  
    chExcelApplication = DYNAMIC-FUNCTION("getExcelHandle").
    IF NOT VALID-HANDLE(chExcelApplication) THEN DO:
      IF DYNAMIC-FUNCTION("setWebDoc","Open",cFileName) NE "" THEN
        MESSAGE "Could not open file: " cFileName VIEW-AS ALERT-BOX ERROR.
      RETURN.
    END.
  
    chExcelApplication:Workbooks:OpenText(cFileName,2,,,,,TRUE).
    chExcelApplication:VISIBLE = TRUE.
  END.
  ELSE DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettStrRecord C-Win 
PROCEDURE SlettStrRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cRowIdList AS CHAR NO-UNDO.
DO ix = 1 TO hBrowseTrans:NUM-SELECTED-ROWS:
  IF hBrowseTrans:FETCH-SELECTED-ROW(ix) THEN DO:
    IF hFieldMapTrans:BUFFER-FIELD("Bestilt1"):BUFFER-VALUE > 0 OR
       hFieldMapTrans:BUFFER-FIELD("Bestilt2"):BUFFER-VALUE > 0 OR
       hFieldMapTrans:BUFFER-FIELD("Bestilt3"):BUFFER-VALUE > 0 OR
       hFieldMapTrans:BUFFER-FIELD("Bestilt4"):BUFFER-VALUE > 0 
       THEN DO:
      DYNAMIC-FUNCTION("DoMessage",0,0,"Sletting kan ikke gjøres dersom det er gjort registrering på størrelse","","").
      cRowIdList = "".
      LEAVE.
    END.
    ELSE cRowIdList = cRowIdList + hFieldMapTrans:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE + ",".
  END.
END.
cRowIdList = TRIM(cRowIdList,",").
IF cRowIdList NE "" AND 
   DYNAMIC-FUNCTION("DoMessage",0,1,"Slett " + STRING(NUM-ENTRIES(cRowIdList)) + " størrelser fra registreringsunderlaget?","","") = 1 THEN DO:
  DO ix = 1 TO NUM-ENTRIES(cRowIdList):
    DYNAMIC-FUNCTION("DoDelete",hFieldMapTrans:NAME,"",
                     "",
                     ENTRY(ix,cRowIdList),
                     FALSE).
  END.
  DYNAMIC-FUNCTION("DoCommit",YES).
  APPLY "value-changed" TO hBrowseLinje.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartLinjeTransReg C-Win 
PROCEDURE StartLinjeTransReg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR hFirstFillIn AS HANDLE NO-UNDO.
DEF VAR cOrgKode     AS CHAR   NO-UNDO.

DO WITH FRAME frmLinje:
  IF NOT hFieldMap:AVAIL THEN 
    hBrowseListe:SELECT-ROW(hBrowseListe:FOCUSED-ROW).
  ASSIGN cOrgKode = Kode:SCREEN-VALUE
         Kode:SCREEN-VALUE = FILL("0",13 - LENGTH(Kode:SCREEN-VALUE)) + Kode:SCREEN-VALUE.

  bOK = hFieldMapLinje:FIND-UNIQUE("WHERE VarebehNr = " + STRING(hFieldMap:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE) 
                                 + " AND ArtikkelNr = " + DYNAMIC-FUNCTION("getFieldValues","Strekkode","WHERE kode = '" + kode:SCREEN-VALUE + "'","ArtikkelNr")
                                  ,NO-LOCK) NO-ERROR.
  IF NOT bOK AND hFieldMapRegStatTrans:AVAIL THEN 
    bOK = hFieldMapLinje:FIND-UNIQUE("WHERE VarebehNr = " + STRING(hFieldMap:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE) 
                                   + " AND ArtikkelNr = " + STRING(hFieldMapRegStatTrans:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE)
                                    ,NO-LOCK) NO-ERROR.
  IF bOK THEN DO:
    hBrowseLinje:QUERY:REPOSITION-TO-ROWID(hFieldMapLinje:ROWID) NO-ERROR.
    IF NOT ERROR-STATUS:ERROR THEN DO:
      APPLY "value-changed" TO hBrowseLinje.
      bOK = hBrowseTrans:QUERY:GET-BUFFER-HANDLE(1):FIND-UNIQUE("WHERE kode = '" + kode:SCREEN-VALUE + "'",NO-LOCK) NO-ERROR.
      IF NOT bOK THEN
        bOK = hBrowseTrans:QUERY:GET-BUFFER-HANDLE(1):FIND-UNIQUE("WHERE kode = '" + cOrgKode + "'",NO-LOCK) NO-ERROR.
      IF bOk THEN DO:
        hBrowseTrans:SET-REPOSITIONED-ROW(2,"conditional").
        hBrowseTrans:QUERY:REPOSITION-TO-ROWID(hBrowseTrans:QUERY:GET-BUFFER-HANDLE(1):ROWID) NO-ERROR.
        IF NOT ERROR-STATUS:ERROR THEN DO:
          APPLY "value-changed" TO hBrowseTrans.
          hBrowseTrans:SELECT-ROW(hBrowseTrans:FOCUSED-ROW).
          ApplyFocusToBestilt1().
        END.
      END.
    END.
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
IF iTab = 3 AND DYNAMIC-FUNCTION("getCurrentObject") = hBrowseLinje THEN DO:
  ASSIGN cHelp        = hBrowseLinje:CURRENT-COLUMN:LABEL
         bStartSearch = TRUE
         .  
  IF cmbTema:SCREEN-VALUE IN FRAME frmLinje NE ? AND cmbTema:SCREEN-VALUE NE "0" THEN
    RETURN.
END.

RUN SUPER.

IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowseLinje THEN
  StartSpesialSort("fjern_markering_av_valgt_spesialsortering",NO).
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
DYNAMIC-FUNCTION("InitFromVarebeh" IN hUtvalg, THIS-PROCEDURE).

SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TotBestiltRecord C-Win 
PROCEDURE TotBestiltRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN VisTotSum("bestilt").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TotForslagRecord C-Win 
PROCEDURE TotForslagRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN VisTotSum("forslag").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValChngBrowseToggle C-Win 
PROCEDURE ValChngBrowseToggle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DYNAMIC-FUNCTION("setAttribute",hBrwLinjeGodkjent,"BufferExtraValues",Butikknr:SCREEN-VALUE IN FRAME frmLinje).
RUN SUPER.
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
startSpesialSort(hMnuItem:LABEL,YES).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ViewQueryCountRecord C-Win 
PROCEDURE ViewQueryCountRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF hQueryCountMenu:CHECKED THEN 
  DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"getrecordcount","yes").
ELSE
  DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"getrecordcount","").

DYNAMIC-FUNCTION("setCustomWinSettings",THIS-PROCEDURE:CURRENT-WINDOW,"<StartViewQueryCount>|brwLinje|" + STRING(hQueryCountMenu:CHECKED) + "|<EndViewQueryCount>").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisTotSum C-Win 
PROCEDURE VisTotSum :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icSumType AS CHAR NO-UNDO.

DEF VAR cResult       AS CHAR NO-UNDO.
DEF VAR cButikkIdList AS CHAR NO-UNDO.
DEF VAR cRowIdList    AS CHAR NO-UNDO.
DEF VAR bSelectedRows AS LOG  NO-UNDO.

bSelectedRows = DYNAMIC-FUNCTION("getCurrentObject") = hBrowseLinje.

cRowIdList = DYNAMIC-FUNCTION("getRowIdList","butiker,varebehlinjethode","butiker",
                              "WHERE CAN-DO('" + cButikkListe + "',STRING(butik))"
                           + ",FIRST VarebehLinjeThode NO-LOCK WHERE VarebehLinjeThode.ButikkNr = Butiker.Butik AND VarebehNr = " + STRING(hFieldMap:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE)
                              ).
THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
RUN JBoxSelector.w (THIS-PROCEDURE,0,
                    "Butiker;butik;butnamn,VarebehLinjeThode",
                    "WHERE CAN-DO('" + cButikkListe + "',STRING(butik))"
                  + ",FIRST VarebehLinjeThode NO-LOCK WHERE VarebehLinjeThode.ButikkNr = Butiker.Butik AND VarebehNr = " + STRING(hFieldMap:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE)
                    ,INPUT-OUTPUT cRowIdList,
                    "Butik",
                    INPUT-OUTPUT cButikkIdList,
                    "","",
                    OUTPUT bOK).
THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

IF bOk THEN DO:
  IF bSelectedRows THEN
    bOk = DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowseLinje,"varebehmesse_tot.p",
                                              REPLACE(cButikkIdList,"|",",")
                                           + "|" + icSumType).
  ELSE 
    bOk = DYNAMIC-FUNCTION("ProcessQuery",hBrowseLinje,"varebehmesse_tot.p",
                                            REPLACE(cButikkIdList,"|",",")
                                         + "|" + icSumType).
  IF bOk THEN DO:
    cResult = (IF icSumType = "forslag" THEN 
                "Sum bestillingsforslag for messebok "
               ELSE
                "Sum bestillinger for messebok ")
            + "~t~t~t" + STRING(TODAY) + " " + STRING(TIME,"HH:MM") + CHR(10) 
            + STRING(hFieldMap:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE) + ", "
            + hFieldMap:BUFFER-FIELD("VareBehBeskrivelse"):BUFFER-VALUE + CHR(10)
            + CHR(10) + DYNAMIC-FUNCTION("getTransactionMessage").
                
    DYNAMIC-FUNCTION("DoMessage",0,20,cResult,"","").
  
/*     IF DYNAMIC-FUNCTION("DoMessage",0,4,cResult,"Kopier melding til utklippstavle?","") = 6 */
/*       THEN CLIPBOARD:VALUE = cResult.                                                       */
  END.
  ELSE DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ApplyFocusToBestilt1 C-Win 
FUNCTION ApplyFocusToBestilt1 RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
/* APPLY "entry" TO LevUke2 IN FRAME frmLinje.  */
/* RETURN TRUE.  */

DEF VAR hWidget AS HANDLE NO-UNDO.

IF DYNAMIC-FUNCTION("getAttribute",hBrowseTrans,"tabchainoverlays") = "" THEN RETURN FALSE.

hWidget = WIDGET-HANDLE(ENTRY(1,DYNAMIC-FUNCTION("getAttribute",hBrowseTrans,"tabchainoverlays"))).
IF VALID-HANDLE(hWidget) THEN DO:
  hBrowseTrans:SELECT-ROW(hBrowseTrans:FOCUSED-ROW).
  hWidget:TAB-STOP = TRUE.
  hWidget:MOVE-AFTER(hBrowseTrans).
  APPLY "entry" TO hWidget.
END.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ClearFilter C-Win 
FUNCTION ClearFilter RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DO WITH FRAME frmLinje:
  ASSIGN sokAvdelingNavn:SCREEN-VALUE   = ""
         sokAvdelingNr:SCREEN-VALUE     = "0" 
         sokHg:SCREEN-VALUE             = "0" 
         sokHgBeskr:SCREEN-VALUE        = "" 
         sokVg:SCREEN-VALUE             = "0" 
         sokVgBeskr:SCREEN-VALUE        = ""
         sokLevNr:SCREEN-VALUE          = IF cLevNrListe = "*" THEN "0" ELSE sokLevNr:SCREEN-VALUE
         sokLevNamn:SCREEN-VALUE        = IF cLevNrListe = "*" OR NUM-ENTRIES(cLevNrListe) > 1 THEN "" ELSE sokLevNamn:SCREEN-VALUE
         sokBeskr:SCREEN-VALUE          = ""
         sokAktNr:SCREEN-VALUE          = "0"
         sokAktBeskrivelse:SCREEN-VALUE = ""
         sokLinjeMerknad:SCREEN-VALUE   = ""
         sokFraEdato:SCREEN-VALUE       = ?
         sokTilEdato:SCREEN-VALUE       = ?
         sokFraVPIdato:SCREEN-VALUE     = ?
         sokTilVPIdato:SCREEN-VALUE     = ?
         rsBestilling:SCREEN-VALUE      = "0"
         sokBestVerdi:SCREEN-VALUE      = "0"
         sokBestVerdi:SENSITIVE         = FALSE
         sokArtikkelNr:SCREEN-VALUE     = "0"
         cmbTema:SCREEN-VALUE           = "0"
         tbKjedeVare:CHECKED            = NO
         tbGjennomfaktureres:CHECKED    = NO
         .

   APPLY "value-changed" TO rsBestilling.
   IF ButikkNr:SCREEN-VALUE = "0" OR ButikkNr:SCREEN-VALUE NE ? THEN DO:
     DYNAMIC-FUNCTION("setAttribute",hBrowseLinje:HANDLE,"1stSortColumn","LevNamn").
     DYNAMIC-FUNCTION("setAttribute",hBrowseLinje:HANDLE,"2ndSortColumn","Beskr").

     DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"querysort","levnamn by beskr").
     DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"querydesc","").
     DYNAMIC-FUNCTION("setCurrentObject",hBrowseLinje).
     RUN OpenQuery.
   END.
END.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION EndreMangeTranser C-Win 
FUNCTION EndreMangeTranser RETURNS LOGICAL
  ( INPUT icField AS CHAR,
    INPUT icValue AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cRowIdList  AS CHAR NO-UNDO.
DEF VAR cLevUke     AS CHAR NO-UNDO.

IF iReturn = 0 THEN RETURN FALSE.

DO WITH FRAME frmLinje:
  bOK = TRUE.
  DO ix = 1 TO hBrowseTrans:NUM-SELECTED-ROWS:
    IF hBrowseTrans:FETCH-SELECTED-ROW(ix) THEN DO:
      cRowIdList = cRowIdList + hFieldMapTrans:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE + "|".
      CASE icField:
        WHEN "Bestilt1" THEN DO:
          IF hFieldMapTrans:BUFFER-FIELD("Levuke1"):BUFFER-VALUE = 0 AND 
             LevUke1:SCREEN-VALUE = ? THEN
            bOK = FALSE.
          ELSE cLevUke = LevUke1:SCREEN-VALUE.
        END.
        WHEN "Bestilt2" THEN DO:
          IF hFieldMapTrans:BUFFER-FIELD("Levuke2"):BUFFER-VALUE = 0 AND 
             LevUke2:SCREEN-VALUE = ? THEN
            bOK = FALSE.
          ELSE cLevUke = LevUke2:SCREEN-VALUE.
        END.
        WHEN "Bestilt3" THEN DO:
          IF hFieldMapTrans:BUFFER-FIELD("Levuke3"):BUFFER-VALUE = 0 AND 
             LevUke3:SCREEN-VALUE = ? THEN
            bOK = FALSE.
          ELSE cLevUke = LevUke3:SCREEN-VALUE.
        END.
        WHEN "Bestilt4" THEN DO:
          IF hFieldMapTrans:BUFFER-FIELD("Levuke4"):BUFFER-VALUE = 0 AND 
             LevUke4:SCREEN-VALUE = ? THEN
            bOK = FALSE.
          ELSE cLevUke = LevUke4:SCREEN-VALUE.
        END.
      END CASE.
    END.
  END.
  IF NOT bOK THEN DO:
    DYNAMIC-FUNCTION("DoMessage",0,0,"Antall kan ikke settes hvis ikke leveringsuke er registrert","Feil","").
    RETURN FALSE.
  END.
  ELSE IF NOT DYNAMIC-FUNCTION("RunProc","varebehlinjetrans_justermange.p",
                                icField + "|" + icValue + "|" + cLevUke + "|" + cLevNrListe
                              + "|ROWID|" + TRIM(cRowIdList,"|"),
                                ?) THEN
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil i justering av " + icField,""). 
  ELSE DO:
    DYNAMIC-FUNCTION("refreshRowids",hBrowseLinje,hFieldMapLinje:BUFFER-FIELD("rowident1"):BUFFER-VALUE).
    hBrowseLinje:REFRESH().
    APPLY "value-changed" TO hBrowseLinje.

/*     DYNAMIC-FUNCTION("refreshRowids",hBrowseTrans,REPLACE(cRowIdList,"|",",")). */
/*     APPLY "value-changed" TO hBrowseTrans.                                      */
  END.
END.

RETURN TRUE. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FinnLevUker C-Win 
FUNCTION FinnLevUker RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR rTrans AS ROWID NO-UNDO.
DEF VAR bOk    AS LOG   NO-UNDO.

IF hFieldMapTrans:AVAIL THEN DO WITH FRAME frmLinje:
  rTrans = hFieldMapTrans:ROWID.
  bOk = hFieldMapTrans:FIND-FIRST("WHERE LevUke1 NE 0") NO-ERROR.
  IF bOk THEN
    LevUke1:SCREEN-VALUE = SUBSTR(hFieldMapTrans:BUFFER-FIELD("LevUke1"):STRING-VALUE,5) +
                           SUBSTR(hFieldMapTrans:BUFFER-FIELD("LevUke1"):STRING-VALUE,3,2).

  bOk = hFieldMapTrans:FIND-FIRST("WHERE LevUke2 NE 0") NO-ERROR.
  IF bOk THEN
    LevUke2:SCREEN-VALUE = SUBSTR(hFieldMapTrans:BUFFER-FIELD("LevUke2"):STRING-VALUE,5) +
                           SUBSTR(hFieldMapTrans:BUFFER-FIELD("LevUke2"):STRING-VALUE,3,2).

  bOk = hFieldMapTrans:FIND-FIRST("WHERE LevUke3 NE 0") NO-ERROR.
  IF bOk THEN
    LevUke3:SCREEN-VALUE = SUBSTR(hFieldMapTrans:BUFFER-FIELD("LevUke3"):STRING-VALUE,5) +
                           SUBSTR(hFieldMapTrans:BUFFER-FIELD("LevUke3"):STRING-VALUE,3,2).

  bOk = hFieldMapTrans:FIND-FIRST("WHERE LevUke4 NE 0") NO-ERROR.
  IF bOk THEN
    LevUke4:SCREEN-VALUE = SUBSTR(hFieldMapTrans:BUFFER-FIELD("LevUke4"):STRING-VALUE,5) +
                           SUBSTR(hFieldMapTrans:BUFFER-FIELD("LevUke4"):STRING-VALUE,3,2).

  bOk = hFieldMapTrans:FIND-BY-ROWID(rTrans) NO-ERROR.

  ASSIGN LevUke1:MODIFIED = FALSE
         LevUke2:MODIFIED = FALSE                                        
         LevUke3:MODIFIED = FALSE                                        
         LevUke4:MODIFIED = FALSE                                        
         .
END.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getArtNr C-Win 
FUNCTION getArtNr RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF hFieldMapLinje:AVAIL THEN
  RETURN hFieldMapLinje:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE.
ELSE
  RETURN 0.00. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFromNotat C-Win 
FUNCTION getFromNotat RETURNS CHARACTER
  ( INPUT icMerke AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose: Lagre merke og verdi i notatfelt 
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR iStart    AS INT  NO-UNDO.
DEF VAR iEnd      AS INT  NO-UNDO.

IF NOT hFieldMap:AVAIL OR NOT hFieldMap:BUFFER-FIELD("VareBehNotat"):BUFFER-VALUE MATCHES "*" + icMerke + "*" THEN RETURN "".

ASSIGN iStart   = INDEX(hFieldMap:BUFFER-FIELD("VareBehNotat"):BUFFER-VALUE,icMerke + "<") + LENGTH(icMerke) + 1
       iEnd     = INDEX(hFieldMap:BUFFER-FIELD("VareBehNotat"):BUFFER-VALUE,">",iStart)
       .
 
RETURN SUBSTR(hFieldMap:BUFFER-FIELD("VareBehNotat"):BUFFER-VALUE,iStart,iEnd - iStart).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getLevNr C-Win 
FUNCTION getLevNr RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF sokLevNr:SCREEN-VALUE IN FRAME frmLinje NE "0" THEN
  RETURN sokLevNr:SCREEN-VALUE IN FRAME frmLinje. 
ELSE IF hFieldMapLinje:AVAIL THEN
  RETURN STRING(hFieldMapLinje:BUFFER-FIELD("LevNr"):BUFFER-VALUE). 
ELSE RETURN "0".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getReportFile C-Win 
FUNCTION getReportFile RETURNS CHARACTER
  ( INPUT icClientFileName AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cFileName    AS CHAR   NO-UNDO.
DEF VAR httRepTable  AS HANDLE NO-UNDO.
DEF VAR httRepBuffer AS HANDLE NO-UNDO. 
DEF VAR hQuery       AS HANDLE NO-UNDO.

RETURN icClientFileName. /* Kommenter ut denne ved overgang til appserver */

/* -------------
ASSIGN httRepTable  = DYNAMIC-FUNCTION("getRunProcReturnTable",?)
       httRepBuffer = httRepTable:DEFAULT-BUFFER-HANDLE
       cFileName = SESSION:TEMP-DIR + STRING(hFieldMap:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE) + "_" + DYNAMIC-FUNCTION("getASuserId") + "_" + STRING(YEAR(TODAY)) + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY)) + STRING(TIME) + ".txt".  

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
------- */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getVarebehNr C-Win 
FUNCTION getVarebehNr RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DO WITH FRAME frmDetalj:
      RETURN dec(VarebehNr:SCREEN-VALUE).   /* Function return value. */
  END.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION InitUtvalgToVarebeh C-Win 
FUNCTION InitUtvalgToVarebeh RETURNS LOGICAL
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LastInnstillinger C-Win 
FUNCTION LastInnstillinger RETURNS LOGICAL
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
  ELSE IF ENTRY(ix,cWinSettings,"|") = "<StartViewQueryCount>" THEN
    DO iy = ix + 1 TO NUM-ENTRIES(cWinSettings,"|") BY 2:
      IF ENTRY(iy,cWinSettings,"|") = "<EndViewQueryCount>" THEN LEAVE.
      ELSE DO:
        DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"getrecordcount",ENTRY(iy + 1,cWinSettings,"|")).
        hQueryCountMenu:CHECKED = ENTRY(iy + 1,cWinSettings,"|") = "yes" NO-ERROR.
      END.
    END.
END.
                       
RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LevForecastToExcel C-Win 
FUNCTION LevForecastToExcel RETURNS LOGICAL
  ( INPUT icFileAndCount AS CHAR,
    INPUT icType         AS CHAR,
    INPUT icLevNr        AS CHAR,
    INPUT icTargetFile   AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
    
   ( INPUT icFileAndCount AS CHAR,
    INPUT icType         AS CHAR,
    INPUT icLevNr       AS CHAR ) :

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
DEF VAR cValue                  AS CHAR NO-UNDO.
DEF VAR iEndringerFra           AS INT NO-UNDO.
DEF VAR cLevNavn                AS CHAR NO-UNDO.
DEF VAR cVbokMesse              AS CHAR NO-UNDO.

ASSIGN cFileName = getReportFile(ENTRY(1,icFileAndCount,"|"))
       iCount    = INT(ENTRY(2,icFileAndCount,"|"))
       .
cLevNavn   = DYNAMIC-FUNCTION("getFieldList","LevBas;Levnamn","WHERE LevNr = " + icLevNr).
cVbokMesse = "Varebok: " + DYNAMIC-FUNCTION("getFieldList",
                              "VarebokHode;VareBokBeskrivelse",
                              "WHERE VareBokNr = " + STRING(hFieldMap:BUFFER-FIELD("Kilde"):BUFFER-VALUE)).
cVbokMesse = cVbokMesse + ". Messe: " + hFieldMap:BUFFER-FIELD("MesseBeskrivelse"):BUFFER-VALUE.

chExcelApplication = DYNAMIC-FUNCTION("getExcelHandle").
IF NOT VALID-HANDLE(chExcelApplication) THEN DO:
  IF DYNAMIC-FUNCTION("setWebDoc","Open",cFileName) NE "" THEN
    MESSAGE "Could not open file: " cFileName VIEW-AS ALERT-BOX ERROR.
  RETURN TRUE.
END.
  
chExcelApplication:Workbooks:OpenText(cFileName,2,,,,,TRUE).

chExcelApplication:ScreenUpdating = FALSE.

cRange = "A2:D" + STRING(iCount + 3). 

ASSIGN chWorkbook                           = chExcelApplication:WorkBooks:ITEM(1)
       chWorkSheet                          = chExcelApplication:Sheets:ITEM(1)
       chWorkSheet:NAME                     = ENTRY(1,hFieldMap:BUFFER-FIELD("VareBehBeskrivelse"):BUFFER-VALUE," ")

       chWorkSheet:Range(cRange):FONT:NAME  = "Arial Narrow"
       chWorkSheet:Range(cRange):FONT:SIZE  = 13

       chWorkSheet:Rows(1):FONT:Bold        = TRUE
       chWorkSheet:Rows(3):FONT:Bold        = TRUE

       chWorkSheet:PageSetup:LeftHeader     = "Lev: " + icLevNr + ", " + cLevNavn
       chWorkSheet:PageSetup:CenterHeader   = cVbokMesse
       chWorkSheet:PageSetup:RightHeader    = "Forecast"
       chWorkSheet:PageSetup:LeftFooter     = DYNAMIC-FUNCTION("getAppTitle")
       chWorkSheet:PageSetup:CenterFooter   = STRING(TODAY,"99/99/9999")  + " " + STRING(TIME,"HH:MM") /*"&D &T"*/
       chWorkSheet:PageSetup:RightFooter    = "Page &P of &N"

       chWorkSheet:PageSetup:Orientation    = 2
       chWorkSheet:PageSetup:FitToPagesWide = 1
       chWorkSheet:PageSetup:TopMargin      = 50
       chWorkSheet:PageSetup:LeftMargin     = 25
       chWorkSheet:PageSetup:RightMargin    = 25
       chWorkSheet:PageSetup:Zoom           = 70
       chWorkSheet:PageSetup:PrintTitleRows = "$1:$1"
       .

chWorkSheet:Range("A1:D1"):SELECT().
chInterior = chExcelApplication:SELECTION:Interior.
chInterior:ColorIndex = 6.

chWorkSheet:Rows("1"):RowHeight = 38.

chWorkSheet:Range("D4:D" + STRING(iCount + 3)):FONT:Bold = TRUE.

chWorkSheet:Range("A:A"):columnwidth  = chWorkSheet:Range("A:A"):columnwidth * 2. 
chWorkSheet:Range("B:B"):columnwidth  = chWorkSheet:Range("B:B"):columnwidth * 3.
chWorkSheet:Range("A:A"):NumberFormat = "@".
chWorkSheet:Range("D:D"):NumberFormat = "##0".
    
chWorkSheet:Columns("C:D"):AutoFit().

chWorkSheet:Range("B2"):Select().
chExcelApplication:ActiveWindow:FreezePanes = TRUE.

IF icTargetFile NE "" THEN DO:
  IF SEARCH(icTargetFile) NE ? THEN
    icTargetFile = SUBSTR(icTargetFile,1,R-INDEX(icTargetFile,".xls") - 1) + "_" + STRING(TIME) + ".xls".
    
  chWorkBook:SaveAs(icTargetFile, -4143,,,,,,, TRUE).
  
  icTargetFile = REPLACE(icTargetFile,"/","-").

  chWorkbook:Close().
END.
ELSE DO:
  chExcelApplication:ScreenUpdating = YES.
  chExcelApplication:VISIBLE = TRUE.
END.

RELEASE OBJECT chBorder NO-ERROR.
RELEASE OBJECT chBefore NO-ERROR.
RELEASE OBJECT chInterior NO-ERROR.
RELEASE OBJECT chWorksheet NO-ERROR.
RELEASE OBJECT chWorkbook NO-ERROR.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LevOrdreBekrToExcel C-Win 
FUNCTION LevOrdreBekrToExcel RETURNS LOGICAL
  ( INPUT icFileAndCount AS CHAR,
    INPUT icType         AS CHAR,
    INPUT icLevNr        AS CHAR,
    INPUT icTargetFile   AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
    
   ( INPUT icFileAndCount AS CHAR,
    INPUT icType         AS CHAR,
    INPUT icLevNr       AS CHAR ) :

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
DEF VAR cValue                  AS CHAR NO-UNDO.
DEF VAR iEndringerFra           AS INT NO-UNDO.
DEF VAR cLevNavn                AS CHAR NO-UNDO.
DEF VAR cVbokMesse              AS CHAR NO-UNDO.

ASSIGN cFileName = getReportFile(ENTRY(1,icFileAndCount,"|"))
       iCount    = INT(ENTRY(2,icFileAndCount,"|"))
       .
cLevNavn   = DYNAMIC-FUNCTION("getFieldList","LevBas;Levnamn","WHERE LevNr = " + icLevNr).
cVbokMesse = ". Varebok: " + DYNAMIC-FUNCTION("getFieldList",
                              "VarebokHode;VareBokBeskrivelse",
                              "WHERE VareBokNr = " + STRING(hFieldMap:BUFFER-FIELD("Kilde"):BUFFER-VALUE)).
cVbokMesse = cVbokMesse + ". Messe: " + hFieldMap:BUFFER-FIELD("MesseBeskrivelse"):BUFFER-VALUE.

chExcelApplication = DYNAMIC-FUNCTION("getExcelHandle").
IF NOT VALID-HANDLE(chExcelApplication) THEN DO:
  IF DYNAMIC-FUNCTION("setWebDoc","Open",cFileName) NE "" THEN
    MESSAGE "Could not open file: " cFileName VIEW-AS ALERT-BOX ERROR.
  RETURN TRUE.
END.
  
chExcelApplication:Workbooks:OpenText(cFileName,2,,,,,TRUE).

chExcelApplication:ScreenUpdating = FALSE.

cRange = "A2:Y" + STRING(iCount + 600). 

ASSIGN chWorkbook                           = chExcelApplication:WorkBooks:ITEM(1)
       chWorkSheet                          = chExcelApplication:Sheets:ITEM(1)
       chWorkSheet:NAME                     = ENTRY(1,hFieldMap:BUFFER-FIELD("VareBehBeskrivelse"):BUFFER-VALUE," ")

       chWorkSheet:Range(cRange):FONT:NAME  = "Arial Narrow"
       chWorkSheet:Range(cRange):FONT:SIZE  = 13

       chWorkSheet:Rows(1):FONT:Bold        = TRUE
/*        chWorkSheet:Rows(2):FONT:NAME        = "Arial Narrow" */
/*        chWorkSheet:Rows(2):HorizontalAlignment = 3  */
/*        chWorkSheet:Rows(3):HorizontalAlignment = 3  */

       chWorkSheet:PageSetup:LeftHeader     = "Lev: " + icLevNr + ", " + cLevNavn
       chWorkSheet:PageSetup:CenterHeader   = cVbokMesse
       chWorkSheet:PageSetup:RightHeader    = (IF icType = 'godkjent' THEN "Bekreftede ordre " ELSE "Ordreforslag ")
       chWorkSheet:PageSetup:LeftFooter     = DYNAMIC-FUNCTION("getAppTitle")
       chWorkSheet:PageSetup:CenterFooter   = STRING(TODAY,"99/99/9999")  + " " + STRING(TIME,"HH:MM") /*"&D &T"*/
       chWorkSheet:PageSetup:RightFooter    = "Page &P of &N"

       chWorkSheet:PageSetup:Orientation    = 2
       chWorkSheet:PageSetup:FitToPagesWide = 1
       chWorkSheet:PageSetup:TopMargin      = 50
       chWorkSheet:PageSetup:LeftMargin     = 25
       chWorkSheet:PageSetup:RightMargin    = 25
       chWorkSheet:PageSetup:Zoom           = 70
       chWorkSheet:PageSetup:PrintTitleRows = "$1:$1"
       .

chWorkSheet:Range("A1:T1"):SELECT().
chInterior = chExcelApplication:SELECTION:Interior.
chInterior:ColorIndex = 6.

chWorkSheet:Rows("1"):RowHeight = 38.

chWorkSheet:Range("P2:P" + STRING(iCount + 600)):FONT:Bold = TRUE.

chWorkSheet:Range("B1:B1"):WrapText = TRUE.
chWorkSheet:Range("D1:D1"):WrapText = TRUE.
chWorkSheet:Range("E1:E1"):WrapText = TRUE.

chWorkSheet:Range("F:F"):NumberFormat = "@".
/* chWorkSheet:Range("F:F"):NumberFormat = "##0". */
chWorkSheet:Range("G:G"):NumberFormat = "# ##0,00".
chWorkSheet:Range("H:H"):NumberFormat = "##0".
chWorkSheet:Range("I:I"):NumberFormat = "##0".
chWorkSheet:Range("J:J"):NumberFormat = "##0".
chWorkSheet:Range("K:K"):NumberFormat = "##0".
chWorkSheet:Range("L:L"):NumberFormat = "##0".
chWorkSheet:Range("M:M"):NumberFormat = "##0".
chWorkSheet:Range("N:N"):NumberFormat = "##0".
chWorkSheet:Range("O:O"):NumberFormat = "##0".
chWorkSheet:Range("P:P"):NumberFormat = "##0".
chWorkSheet:Range("Q:Q"):NumberFormat = "# ##0,00".
chWorkSheet:Range("T:T"):NumberFormat = "0000000000000".
/* chWorkSheet:Range("T:T"):NumberFormat = "##0". */
    
/*

IF CAN-DO("but,lev",icType) THEN DO:
  DO ix = 2 TO iCount:
    chBorder = chWorkSheet:Range("A" + STRING(ix) + (IF icType = "lev" THEN ":Y" ELSE ":W") + STRING(ix)):Borders({&xlEdgeBottom}).
    ASSIGN chBorder:LineStyle = {&xlContinuous}
           chBorder:Weight    = {&xlThin}.
  
    IF icSortBy = "levnamn" AND 
       chWorkSheet:Range("A" + STRING(ix) + ":A" + STRING(ix)):VALUE = "SUM Levnr" THEN DO:
      chBefore = chWorkSheet:Range("A" + STRING(ix + 1) + ":A" + STRING(ix + 1)).
      chWorkSheet:HPageBreaks:Add(chBefore).
    END.
  END.
  DO ix = iCount + 2 TO iCount + 600:
    ASSIGN cRange = (IF icType = "lev" THEN "I" ELSE "G") + STRING(ix)
           cValue = chWorkSheet:Range(cRange):VALUE
           .
    IF cValue NE ? THEN DO:
      chBorder = chWorkSheet:Range("A" + STRING(ix) + (IF icType = "lev" THEN ":I" ELSE ":G") + STRING(ix)):Borders({&xlEdgeBottom}).
      ASSIGN chBorder:LineStyle = {&xlContinuous}
             chBorder:Weight    = {&xlThin}.
    END.
  
    ASSIGN cRange = "A" + STRING(ix)
           cValue = chWorkSheet:Range(cRange):VALUE.
    IF cValue BEGINS "Endring" THEN DO:
      iEndringerFra = ix.
      LEAVE.
    END.
  END.

  IF iEndringerFra NE 0 THEN
    DO ix = iEndringerFra TO iEndringerFra + 600:
      ASSIGN cRange = "A" + STRING(ix)
             cValue = chWorkSheet:Range(cRange):VALUE.
      IF cValue NE ? THEN DO:
        chBorder = chWorkSheet:Range("A" + STRING(ix) + (IF icType = "lev" THEN ":Y" ELSE ":W") + STRING(ix)):Borders({&xlEdgeBottom}).
        ASSIGN chBorder:LineStyle = {&xlContinuous}
               chBorder:Weight    = {&xlThin}.
      END.
    END.
END.

*/
/* chBefore = chWorkSheet:Range("A" + STRING(iCount + 2)).  */
/* chWorkSheet:HPageBreaks:Add(chBefore).                   */

chWorkSheet:Range("A:A"):columnwidth = chWorkSheet:Range("A:A"):columnwidth * 0.6.
chWorkSheet:Range("B:B"):columnwidth = chWorkSheet:Range("B:B"):columnwidth * 1.8.
chWorkSheet:Columns("C:C"):AutoFit().
chWorkSheet:Columns("E:R"):AutoFit().
chWorkSheet:Range("D:D"):columnwidth = chWorkSheet:Range("D:D"):columnwidth * 1.4.
chWorkSheet:Range("S:S"):columnwidth = chWorkSheet:Range("S:S"):columnwidth * 1.1.

chWorkSheet:Range("B2"):Select().
chExcelApplication:ActiveWindow:FreezePanes = TRUE.

IF icTargetFile NE "" THEN DO:
  IF SEARCH(icTargetFile) NE ? THEN
    icTargetFile = SUBSTR(icTargetFile,1,R-INDEX(icTargetFile,".xls") - 1) + "_" + STRING(TIME) + ".xls".
  icTargetFile = REPLACE(icTargetFile,"/","-").
  chWorkBook:SaveAs(icTargetFile, -4143,,,,,,, TRUE).
  chWorkbook:Close().
END.
ELSE DO:
  chExcelApplication:ScreenUpdating = YES.
  chExcelApplication:VISIBLE = TRUE.
END.

/* release com-handles */

/*
IF CAN-DO("but,lev",icType) THEN DO:
  THIS-PROCEDURE:CURRENT-WINDOW:WINDOW-STATE = 3.
  THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
  IF DYNAMIC-FUNCTION("DoMessage",0,4,"Vil du logge utskriften slik at senere endringer kan vises separat?","","") = 6 THEN 
    DYNAMIC-FUNCTION("DoCreate","PrintLogg",",=create_printloggtype.p"
                     ,"LoggType,NumLoggNokkel,CharLoggNokkel"
                     ,(IF icType = "but" THEN "but_ordrebekr_messe|" ELSE "lev_ordrebekr_messe|") + icLevNr + "|" + STRING(hFieldMap:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE)
                     ,TRUE).
END.
*/

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
   (INPUT ibView AS LOG):
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DYNAMIC-FUNCTION("DeleteObjectLink",hBrowseTrans,hBrwTransBestilt1).
DYNAMIC-FUNCTION("DeleteObjectLink",hBrowseTrans,hBrwTransBestilt2).
DYNAMIC-FUNCTION("DeleteObjectLink",hBrowseTrans,hBrwTransBestilt3).
DYNAMIC-FUNCTION("DeleteObjectLink",hBrowseTrans,hBrwTransBestilt4).

IF NOT ibView THEN
  ASSIGN hBrwTransBestilt1:HIDDEN   = TRUE
         hBrwTransBestilt2:HIDDEN   = TRUE
         hBrwTransBestilt3:HIDDEN   = TRUE
         hBrwTransBestilt4:HIDDEN   = TRUE
         .

IF ibView THEN DO:
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowseTrans,hBrwTransBestilt1,"Bestilt1").
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowseTrans,hBrwTransBestilt2,"Bestilt2").
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowseTrans,hBrwTransBestilt3,"Bestilt3").
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowseTrans,hBrwTransBestilt4,"Bestilt4").
  ASSIGN hBrwTransBestilt1:HIDDEN  = FALSE
         hBrwTransBestilt2:HIDDEN  = FALSE
         hBrwTransBestilt3:HIDDEN  = FALSE
         hBrwTransBestilt4:HIDDEN  = FALSE
         .
  DYNAMIC-FUNCTION("setToolbar",hBrowseTrans,"enable").
END.
ELSE
  DYNAMIC-FUNCTION("setToolbar",hBrowseTrans,"disable").


/* APPLY "value-changed" TO hBrowseTrans.  */

RETURN TRUE.   

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION OrdreBekreftToExcel C-Win 
FUNCTION OrdreBekreftToExcel RETURNS LOGICAL
  ( INPUT icFileAndCount AS CHAR,
    INPUT icType         AS CHAR,
    INPUT icButLev       AS CHAR,
    INPUT ibEAN          AS LOG,
    INPUT icSortBy       AS CHAR,
    INPUT icTargetFile   AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
    
   ( INPUT icFileAndCount AS CHAR,
    INPUT icType         AS CHAR,
    INPUT icButLev       AS CHAR ) :

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
DEF VAR cValue                  AS CHAR NO-UNDO.
DEF VAR iEndringerFra           AS INT NO-UNDO.
DEF VAR cMedNavn                AS CHAR NO-UNDO.
DEF VAR cVbokMesse              AS CHAR NO-UNDO.
DEF VAR bForslag                AS LOG  NO-UNDO.

ASSIGN cFileName = getReportFile(ENTRY(1,icFileAndCount,"|")) 
       iCount    = INT(ENTRY(2,icFileAndCount,"|"))
       bForslag  = icType = "butforslag"
       icType    = (IF bForslag THEN "but" ELSE icType)
       .
IF icButLev NE "" THEN 
  cMedNavn   = DYNAMIC-FUNCTION("getFieldList","Butiker;ButNamn","WHERE Butik = " + icButLev).
cVbokMesse = ". Varebok: " + DYNAMIC-FUNCTION("getFieldList",
                              "VarebokHode;VareBokBeskrivelse",
                              "WHERE VareBokNr = " + STRING(hFieldMap:BUFFER-FIELD("Kilde"):BUFFER-VALUE)).
cVbokMesse = cVbokMesse + ". Messe: " + hFieldMap:BUFFER-FIELD("MesseBeskrivelse"):BUFFER-VALUE.

chExcelApplication = DYNAMIC-FUNCTION("getExcelHandle").
IF NOT VALID-HANDLE(chExcelApplication) THEN DO:
  IF DYNAMIC-FUNCTION("setWebDoc","Open",cFileName) NE "" THEN
    MESSAGE "Could not open file: " cFileName VIEW-AS ALERT-BOX ERROR.
  RETURN TRUE.
END.
  
chExcelApplication:Workbooks:OpenText(cFileName,2,,,,,TRUE).

chExcelApplication:ScreenUpdating = FALSE.

cRange = "A2:Y" + STRING(iCount + 600). 

ASSIGN chWorkbook                           = chExcelApplication:WorkBooks:ITEM(1)
       chWorkSheet                          = chExcelApplication:Sheets:ITEM(1)
       chWorkSheet:NAME                     = ENTRY(1,hFieldMap:BUFFER-FIELD("VareBehBeskrivelse"):BUFFER-VALUE," ")

       chWorkSheet:Range(cRange):FONT:NAME  = "Arial Narrow"
       chWorkSheet:Range(cRange):FONT:SIZE  = 13

       chWorkSheet:Rows(1):FONT:Bold        = TRUE
/*        chWorkSheet:Rows(2):FONT:NAME        = "Arial Narrow" */
/*        chWorkSheet:Rows(2):HorizontalAlignment = 3  */
/*        chWorkSheet:Rows(3):HorizontalAlignment = 3  */

       chWorkSheet:PageSetup:LeftHeader     = IF icType = 'Lev'
                                                THEN "Medlemsnummer og firmanavn: ..............................................................................."
                                                ELSE "Medlemsnummer og firmanavn: " + icButLev + "  " + cMedNavn
       chWorkSheet:PageSetup:CenterHeader   = (IF bForslag THEN "BESTILLINGSFORSLAG" ELSE "Ordrekvittering") + cVbokMesse
       chWorkSheet:PageSetup:RightHeader    = IF NOT bForslag THEN "Signatur" ELSE ""
       chWorkSheet:PageSetup:LeftFooter     = DYNAMIC-FUNCTION("getAppTitle")
       chWorkSheet:PageSetup:CenterFooter   = STRING(TODAY,"99/99/9999")  + " " + STRING(TIME,"HH:MM") /*"&D &T"*/
       chWorkSheet:PageSetup:RightFooter    = "Page &P of &N"

       chWorkSheet:PageSetup:Orientation    = 2
       chWorkSheet:PageSetup:FitToPagesWide = 1
       chWorkSheet:PageSetup:TopMargin      = 50
       chWorkSheet:PageSetup:LeftMargin     = 25
       chWorkSheet:PageSetup:RightMargin    = 25
       chWorkSheet:PageSetup:Zoom           = IF ibEAN OR icType = "lev" THEN 52 ELSE 60
       chWorkSheet:PageSetup:PrintTitleRows = "$1:$1"
       .

chWorkSheet:Range("A1:" + IF icType = "lev" THEN (IF ibEAN THEN "Y1" ELSE "X1") ELSE (IF ibEAN THEN "W1" ELSE "V1")):SELECT().
chInterior = chExcelApplication:SELECTION:Interior.
chInterior:ColorIndex = 6.

chWorkSheet:Rows("1"):RowHeight = 38.

chWorkSheet:Range("A2:A" + STRING(iCount + 600)):FONT:Bold = TRUE.

IF icType = "lev" THEN DO:
  chWorkSheet:Range("I2:I" + STRING(iCount + 600)):FONT:Bold = TRUE.

  chWorkSheet:Range("C1:C1"):WrapText = TRUE.
  chWorkSheet:Range("I1:I1"):WrapText = TRUE.
  chWorkSheet:Range("K1:K1"):WrapText = TRUE.
  chWorkSheet:Range("L1:L1"):WrapText = TRUE.
  chWorkSheet:Range("M1:M1"):WrapText = TRUE.
  chWorkSheet:Range("N1:N1"):WrapText = TRUE.

  chWorkSheet:Range("C:C"):NumberFormat = "@".
  chWorkSheet:Range("D:D"):NumberFormat = "##0".
  chWorkSheet:Range("N:N"):NumberFormat = "##0".
  chWorkSheet:Range("O:O"):NumberFormat = "@".
  chWorkSheet:Range("I:I"):NumberFormat = "# ##0,00".
  chWorkSheet:Range("K:K"):NumberFormat = "# ##0,00".
  chWorkSheet:Range("L:L"):NumberFormat = "# ##0,00".
  chWorkSheet:Range("M:M"):NumberFormat = "# ##0,00".
  IF ibEAN THEN DO:
    chWorkSheet:Range("Y:Y"):NumberFormat = "# ##0,00".
    chWorkSheet:Range("O:O"):NumberFormat = "@".

    chWorkSheet:Range("O2:O" + STRING(iCount + 600)):FONT:Bold = TRUE.
    chWorkSheet:Range("Q2:Q" + STRING(iCount + 600)):FONT:Bold = TRUE.
    chWorkSheet:Range("S2:S" + STRING(iCount + 600)):FONT:Bold = TRUE.
    chWorkSheet:Range("U2:U" + STRING(iCount + 600)):FONT:Bold = TRUE.
    chWorkSheet:Range("W2:W" + STRING(iCount + 600)):FONT:Bold = TRUE.    
    chWorkSheet:Range("X2:X" + STRING(iCount + 600)):FONT:Bold = TRUE.    
    chWorkSheet:Range("Y2:Y" + STRING(iCount + 600)):FONT:Bold = TRUE.    
  END.
  ELSE DO:
    chWorkSheet:Range("X:X"):NumberFormat = "# ##0,00".
    chWorkSheet:Range("N:N"):NumberFormat = "@".

    chWorkSheet:Range("N2:N" + STRING(iCount + 600)):FONT:Bold = TRUE.
    chWorkSheet:Range("P2:P" + STRING(iCount + 600)):FONT:Bold = TRUE.
    chWorkSheet:Range("R2:R" + STRING(iCount + 600)):FONT:Bold = TRUE.
    chWorkSheet:Range("T2:T" + STRING(iCount + 600)):FONT:Bold = TRUE.
    chWorkSheet:Range("V2:V" + STRING(iCount + 600)):FONT:Bold = TRUE.    
    chWorkSheet:Range("W2:W" + STRING(iCount + 600)):FONT:Bold = TRUE.    
    chWorkSheet:Range("X2:X" + STRING(iCount + 600)):FONT:Bold = TRUE.    
  END.

END.
ELSE DO:
  chWorkSheet:Range("G2:G" + STRING(iCount + 600)):FONT:Bold = TRUE.

  chWorkSheet:Range("A1:A1"):WrapText = TRUE.
  chWorkSheet:Range("G1:G1"):WrapText = TRUE.
  chWorkSheet:Range("I1:I1"):WrapText = TRUE.
  chWorkSheet:Range("J1:J1"):WrapText = TRUE.
  chWorkSheet:Range("K1:K1"):WrapText = TRUE.
  chWorkSheet:Range("L1:L1"):WrapText = TRUE.
  chWorkSheet:Range("M1:M1"):WrapText = TRUE.

  chWorkSheet:Range("B:B"):NumberFormat = "##0".
  chWorkSheet:Range("C:C"):NumberFormat = "@".
  chWorkSheet:Range("G:G"):NumberFormat = "# ##0,00".
  chWorkSheet:Range("I:I"):NumberFormat = "# ##0,00".
  chWorkSheet:Range("J:J"):NumberFormat = "# ##0,00".
  chWorkSheet:Range("K:K"):NumberFormat = "# ##0,00".

  IF ibEAN THEN DO:
    chWorkSheet:Range("W:W"):NumberFormat = "# ##0,00".
    chWorkSheet:Range("M:M"):NumberFormat = "@".

    chWorkSheet:Range("M2:M" + STRING(iCount + 600)):FONT:Bold = TRUE.
    chWorkSheet:Range("O2:O" + STRING(iCount + 600)):FONT:Bold = TRUE.
    chWorkSheet:Range("Q2:Q" + STRING(iCount + 600)):FONT:Bold = TRUE.
    chWorkSheet:Range("S2:S" + STRING(iCount + 600)):FONT:Bold = TRUE.
    chWorkSheet:Range("U2:U" + STRING(iCount + 600)):FONT:Bold = TRUE.    
    chWorkSheet:Range("V2:V" + STRING(iCount + 600)):FONT:Bold = TRUE.    
    chWorkSheet:Range("W2:W" + STRING(iCount + 600)):FONT:Bold = TRUE.    
  END.
  ELSE DO:
    chWorkSheet:Range("V:V"):NumberFormat = "# ##0,00".  
    chWorkSheet:Range("L:L"):NumberFormat = "@".

    chWorkSheet:Range("L2:L" + STRING(iCount + 600)):FONT:Bold = TRUE.
    chWorkSheet:Range("N2:N" + STRING(iCount + 600)):FONT:Bold = TRUE.
    chWorkSheet:Range("P2:P" + STRING(iCount + 600)):FONT:Bold = TRUE.
    chWorkSheet:Range("R2:R" + STRING(iCount + 600)):FONT:Bold = TRUE.
    chWorkSheet:Range("T2:T" + STRING(iCount + 600)):FONT:Bold = TRUE.    
    chWorkSheet:Range("U2:U" + STRING(iCount + 600)):FONT:Bold = TRUE.    
    chWorkSheet:Range("V2:V" + STRING(iCount + 600)):FONT:Bold = TRUE.    
  END.
END.

IF CAN-DO("but",icType) AND ibEAN THEN DO:
  chWorkSheet:Range("L2:L" + STRING(iCount)):FONT:NAME = "EAN-13B Half Height".
  chWorkSheet:Range("L2:L" + STRING(iCount)):FONT:SIZE = 42.
  chWorkSheet:Rows("2:" + STRING(iCount)):RowHeight = 40.

  chWorkSheet:Rows(STRING(iCount + 1) + ":" + STRING(iCount + 600)):RowHeight = 20.
  chWorkSheet:Range("L" + STRING(iCount + 1) + ":L" + STRING(iCount + 600)):NumberFormat = "##0".
END.
ELSE DO:
  chWorkSheet:Rows("2:" + STRING(iCount + 600)):RowHeight = 20.
/*   chWorkSheet:Range("V2:V" + STRING(iCount + 600)):FONT:Bold = TRUE.  */
END.

IF CAN-DO("but,lev",icType) THEN DO:
  DO ix = 2 TO iCount:
    chBorder = chWorkSheet:Range("A" + STRING(ix) + (IF icType = "lev" THEN ":Y" ELSE ":V") + STRING(ix)):Borders({&xlEdgeBottom}).
    ASSIGN chBorder:LineStyle = {&xlContinuous}
           chBorder:Weight    = {&xlThin}.
  
    IF icSortBy = "levnamn" AND 
       chWorkSheet:Range("A" + STRING(ix) + ":A" + STRING(ix)):VALUE = "SUM Levnr" THEN DO:
      chBefore = chWorkSheet:Range("A" + STRING(ix + 1) + ":A" + STRING(ix + 1)).
      chWorkSheet:HPageBreaks:Add(chBefore).
    END.
  END.
  DO ix = iCount + 2 TO iCount + 600:
    ASSIGN cRange = (IF icType = "lev" THEN "I" ELSE "G") + STRING(ix)
           cValue = chWorkSheet:Range(cRange):VALUE
           .
    IF cValue NE ? THEN DO:
      chBorder = chWorkSheet:Range("A" + STRING(ix) + (IF icType = "lev" THEN ":I" ELSE ":G") + STRING(ix)):Borders({&xlEdgeBottom}).
      ASSIGN chBorder:LineStyle = {&xlContinuous}
             chBorder:Weight    = {&xlThin}.
    END.
  
    ASSIGN cRange = "A" + STRING(ix)
           cValue = chWorkSheet:Range(cRange):VALUE.
    IF cValue BEGINS "Endring" THEN DO:
      iEndringerFra = ix.
      LEAVE.
    END.
  END.

  IF iEndringerFra NE 0 THEN
    DO ix = iEndringerFra TO iEndringerFra + 600:
      ASSIGN cRange = "A" + STRING(ix)
             cValue = chWorkSheet:Range(cRange):VALUE.
      IF cValue NE ? THEN DO:
        chBorder = chWorkSheet:Range("A" + STRING(ix) + (IF icType = "lev" THEN ":Y" ELSE ":W") + STRING(ix)):Borders({&xlEdgeBottom}).
        ASSIGN chBorder:LineStyle = {&xlContinuous}
               chBorder:Weight    = {&xlThin}.
      END.
    END.
END.


chBefore = chWorkSheet:Range("A" + STRING(iCount + 2)).
chWorkSheet:HPageBreaks:Add(chBefore).

chWorkSheet:Columns("A:B"):AutoFit().
chWorkSheet:Columns("G:W"):AutoFit().
chWorkSheet:Range("F:F"):columnwidth = chWorkSheet:Range("F:F"):columnwidth * 1.3.
chWorkSheet:Range("C:C"):columnwidth = 27.
chWorkSheet:Range("D:D"):columnwidth = 10.
chWorkSheet:Range("E:E"):columnwidth = 18.
chWorkSheet:Range("F:F"):columnwidth = 10.

chWorkSheet:Range("B2"):Select().
chExcelApplication:ActiveWindow:FreezePanes = TRUE.



IF icTargetFile NE "" THEN DO:
  IF SEARCH(icTargetFile) NE ? THEN
    icTargetFile = SUBSTR(icTargetFile,1,R-INDEX(icTargetFile,".xls") - 1) + "_" + STRING(TIME) + ".xls".
  icTargetFile = REPLACE(icTargetFile,"/","-").
  chWorkBook:SaveAs(icTargetFile, -4143,,,,,,, TRUE).
  chWorkbook:Close().
END.
ELSE DO:
  chExcelApplication:ScreenUpdating = YES.
  chExcelApplication:VISIBLE = TRUE.

  IF CAN-DO("but,lev",icType) AND NOT bForslag THEN DO:
    THIS-PROCEDURE:CURRENT-WINDOW:WINDOW-STATE = 3.
    THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
    IF DYNAMIC-FUNCTION("DoMessage",0,4,"Vil du logge utskriften slik at senere endringer kan vises separat?","","") = 6 THEN 
      DYNAMIC-FUNCTION("DoCreate","PrintLogg",",=create_printloggtype.p"
                       ,"LoggType,NumLoggNokkel,CharLoggNokkel"
                       ,(IF icType = "but" THEN "but_ordrebekr_messe|" ELSE "lev_ordrebekr_messe|") + icButLev + "|" + STRING(hFieldMap:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE)
                       ,TRUE).
  END.
END.

RELEASE OBJECT chBorder NO-ERROR.
RELEASE OBJECT chBefore NO-ERROR.
RELEASE OBJECT chInterior NO-ERROR.
RELEASE OBJECT chWorksheet NO-ERROR.
RELEASE OBJECT chWorkbook NO-ERROR.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setButikkListe C-Win 
FUNCTION setButikkListe RETURNS LOGICAL
  (  ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
cButikkListe = DYNAMIC-FUNCTION("getAttribute",SESSION,"ButikkListe").
IF cButikkListe = "" THEN cButikkListe = "*".
/* cButikkListe = "260". */

RETURN TRUE.

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
IF iRGBcolor = 0 THEN RETURN 15.

IF iRGBcolor NE 0 THEN DO ix = 0 TO COLOR-TABLE:NUM-ENTRIES:
  IF COLOR-TABLE:GET-RGB-VALUE(ix) = iRGBcolor THEN RETURN ix.
END.

ASSIGN ix = COLOR-TABLE:NUM-ENTRIES
       COLOR-TABLE:NUM-ENTRIES = ix + 1.

IF ix = 256 THEN
  MESSAGE PROGRAM-NAME(1) SKIP
          256 SKIP
          VIEW-AS ALERT-BOX.

COLOR-TABLE:SET-DYNAMIC(ix, YES).
COLOR-TABLE:SET-RGB-VALUE(ix,iRGBcolor).

RETURN ix.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setLevUke C-Win 
FUNCTION setLevUke RETURNS LOGICAL
  ( INPUT ihLevUke AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cRowIdList AS CHAR   NO-UNDO.
DEF VAR bModify    AS LOG    NO-UNDO.
DEF VAR iFocus     AS INT    NO-UNDO.

IF NOT ihLevUke:MODIFIED OR NOT hFieldMapLinje:AVAIL OR NOT hBrowseTrans:QUERY:IS-OPEN THEN RETURN FALSE.

IF hBrowseTrans:FOCUSED-ROW = 0 OR hBrowseTrans:FOCUSED-ROW = ? THEN hBrowseTrans:FOCUSED-ROW = 1.

iFocus = hBrowseTrans:FOCUSED-ROW.

EMPTY TEMP-TABLE ttVarebehLinjeTrans.

hBrowseTrans:QUERY:GET-FIRST().
REPEAT WHILE NOT hBrowseTrans:QUERY:QUERY-OFF-END:
  IF (ihLevUke:NAME = "LevUke1" AND hFieldMapTrans:BUFFER-FIELD("Bestilt1"):BUFFER-VALUE > 0) OR 
     (ihLevUke:NAME = "LevUke2" AND hFieldMapTrans:BUFFER-FIELD("Bestilt2"):BUFFER-VALUE > 0) OR 
     (ihLevUke:NAME = "LevUke3" AND hFieldMapTrans:BUFFER-FIELD("Bestilt3"):BUFFER-VALUE > 0) OR 
     (ihLevUke:NAME = "LevUke4" AND hFieldMapTrans:BUFFER-FIELD("Bestilt4"):BUFFER-VALUE > 0) THEN DO:
    CREATE ttVarebehLinjeTrans.
    BUFFER ttVarebehLinjeTrans:HANDLE:BUFFER-COPY(hFieldMapTrans).
    bModify = TRUE.
  END.
  hBrowseTrans:QUERY:GET-NEXT().
END.

/* DELETE OBJECT hQ. */

IF NOT bModify THEN DO:
  APPLY "value-changed" TO hBrowseLinje.
    
  hBrowseTrans:DESELECT-ROWS().
  hBrowseTrans:SELECT-ROW(1).
  RETURN FALSE.
END.

IF NOT DYNAMIC-FUNCTION("RunProc","varebehlinjetrans_justermange.p",ihLevUke:NAME + "|" + ihLevUke:SCREEN-VALUE,httVarebehLinjeTrans) THEN
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
ELSE 
  APPLY "value-changed" TO hBrowseLinje.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setNotat C-Win 
FUNCTION setNotat RETURNS LOGICAL
  ( INPUT icMerke AS CHAR,
    INPUT icVerdi AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose: Lagre merke og verdi i notatfelt 
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR iStart    AS INT  NO-UNDO.
DEF VAR iEnd      AS INT  NO-UNDO.
DEF VAR cNewText  AS CHAR NO-UNDO.

ASSIGN iStart   = INDEX(hFieldMap:BUFFER-FIELD("VareBehNotat"):BUFFER-VALUE,icMerke + "<")
       iEnd     = (IF iStart > 0 THEN INDEX(hFieldMap:BUFFER-FIELD("VareBehNotat"):BUFFER-VALUE,">",iStart) ELSE 0)
       cNewText = (IF iStart > 0 THEN TRIM(SUBSTR(hFieldMap:BUFFER-FIELD("VareBehNotat"):BUFFER-VALUE,1,iStart - 1),CHR(10)) + CHR(10) 
                   ELSE TRIM(hFieldMap:BUFFER-FIELD("VareBehNotat"):BUFFER-VALUE,CHR(10)) + CHR(10)) 
                   + icMerke + "<" + icVerdi + ">" 
                + (IF iEnd > 0 THEN SUBSTR(hFieldMap:BUFFER-FIELD("VareBehNotat"):BUFFER-VALUE,iEnd + 1) ELSE "")
       .

/* MESSAGE PROGRAM-NAME(1) SKIP                                                           */
/*         iStart - 1 SKIP                                                                */
/*         SUBSTR(hFieldMap:BUFFER-FIELD("VareBehNotat"):BUFFER-VALUE,1,iStart - 1) SKIP  */
/*         icMerke + "<" + icVerdi + ">" SKIP                                             */
/*         SUBSTR(hFieldMap:BUFFER-FIELD("VareBehNotat"):BUFFER-VALUE,iEnd + 1) SKIP      */
/*         iEnd + 1 SKIP(1)                                                               */
/*         cNewText                                                                       */
/*         VIEW-AS ALERT-BOX.                                                             */
IF DYNAMIC-FUNCTION("DoUpdate","VarebehHode","ignore",
                    "",
                    hFieldMap:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE,
                    "VareBehNotat",
                    cNewText,
                    TRUE) THEN DO:
  DYNAMIC-FUNCTION("RefreshRowids",hBrowseListe,hFieldMap:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE).
  VareBehNotat:SCREEN-VALUE IN FRAME frmDetalj = hFieldMap:BUFFER-FIELD("VareBehNotat"):BUFFER-VALUE.
  IF hBrowseLinje:QUERY:NUM-RESULTS > 0 THEN hBrowseLinje:REFRESH().
  RETURN TRUE.
END.
ELSE RETURN FALSE.                           

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

cMenuLabels = DYNAMIC-FUNCTION("getFieldList","JBoxGenCode;cDescription","WHERE cCodeType = 'VarebokSort' BY cDescription").

IF cMenuLabels NE "" THEN DO:    
  DO ix = 1 TO NUM-ENTRIES(cMenuLabels,"|"):
    cMenuList = cMenuList + "VelgSpesialSort;" + ENTRY(ix,cMenuLabels,"|") + ";VelgSpesialSort;toggle,".
  END.
  cMenuList = TRIM(cMenuList,",").
/*   cMenuList = cMenuList + "rule,". */
END.
ELSE RETURN NO.

DYNAMIC-FUNCTION("NewMenuBand",
                  hSpesialSortMnu, 
                  cMenuList /* + "SortSpesial;Ny.." */
/*                  + (IF cMenuLabels NE "" THEN ",SlettSpesialSort;Slett.." ELSE "") */
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

cTabStripList = "Liste|Detalj|Registrering|Rapporter".

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setTema C-Win 
FUNCTION setTema RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cLevNavn AS CHAR NO-UNDO.
DEF VAR cTema    AS CHAR NO-UNDO.
DEF VAR cTotTema AS CHAR NO-UNDO.

DO WITH FRAME frmLinje:
/*   IF CAN-DO("1",cAdgang) THEN                                                                                                                        */
/*     cTema = DYNAMIC-FUNCTION("getFieldList","VarebokTemaHode;VbBeskrivelse;VbTemeNr",                                                                */
/*                                                                     "WHERE (LevNr = " + sokLevNr:SCREEN-VALUE + " OR LevNr = 0)"                     */
/*                                                                  + " AND VarebehNr = " + STRING(hFieldMap:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE)).  */
/*   ELSE DO:                                                                                                                                           */
    cTotTema = DYNAMIC-FUNCTION("getFieldList","VarebokTemaHode;VbBeskrivelse;VbTemeNr",
                                               "WHERE VarebehNr = " + STRING(hFieldMap:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE)
                                ).

    DO ix = 1 TO NUM-ENTRIES(cTotTema,"|") BY 2:                                                                                                                           
      cLevNavn = DYNAMIC-FUNCTION("getFieldValues","LevBas",
                                  "WHERE LevNr = " + DYNAMIC-FUNCTION("getFieldValues","VarebokTemaHode","WHERE VbTemeNr = " + ENTRY(ix + 1,cTotTema,"|"),"LevNr"),"LevNamn").
      cTema = cTema + ENTRY(ix,cTotTema,"|") + (IF cLevNavn NE ? THEN " / " + cLevNavn ELSE "")
            + "|" + ENTRY(ix + 1,cTotTema,"|") + "|".
    END.
/*     cTema = RIGHT-TRIM("|0|" + DYNAMIC-FUNCTION("getFieldList","VarebokTemaHode;VbBeskrivelse|Levnr;VbTemeNr",                                             */
/*                                                                     "WHERE VarebehNr = " + STRING(hFieldMap:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE)),"|"). */
/*   END.  */

  cmbTema:LIST-ITEM-PAIRS = RIGHT-TRIM("|0|" + cTema,"|").
END.
  
RETURN YES.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION startSpesialSort C-Win 
FUNCTION startSpesialSort RETURNS LOGICAL
  ( INPUT icSortDescr AS CHAR,
    INPUT ibOpenQuery AS LOG ) :
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

cSortString = DYNAMIC-FUNCTION("getFieldValues","JBoxGenCode",
                               "WHERE JBoxGenCode.cCodeType = 'VarebokSort' AND JBoxGenCode.cDescription = '" + icSortDescr + "'",
                               "cCodeValue").
IF cSortString NE ? THEN DO:
  DYNAMIC-FUNCTION("setSortLabel",hBrowseLinje,"",NO).

  cSortString = REPLACE(cSortString,"Sekv","Sortering").

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
  IF ibOpenQuery THEN DO:
    DYNAMIC-FUNCTION("setCurrentObject",hBrowseLinje).
    RUN OpenQuery.
  END.
END.
ELSE RETURN NO.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SumAntOgVerdi C-Win 
FUNCTION SumAntOgVerdi RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

DO WITH FRAME frmLinje:
  ASSIGN fiSumAnt   = 0
         fiSumVerdi = 0
         .
  IF hFieldMapLinje:AVAIL THEN DO:
    DYNAMIC-FUNCTION("refreshRowids",hBrowseLinje,hFieldMapLinje:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE).
  

    hSumTransQuery:QUERY-OPEN().
    hSumTransQuery:GET-FIRST().
    REPEAT WHILE NOT hSumTransQuery:QUERY-OFF-END:
      fiSumAnt = fiSumAnt + hSumTransQuery:GET-BUFFER-HANDLE(1):BUFFER-FIELD("SumAntStr"):BUFFER-VALUE.
      hSumTransQuery:GET-NEXT().
    END.
    fiSumVerdi = fiSumAnt * hFieldMapLinje:BUFFER-FIELD("Varekost"):BUFFER-VALUE.

    IF fiSumAnt > iBestVarsel AND bWarning AND hFieldMapLinje:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE NE fCurrArtNr THEN DO:
      MESSAGE "Du har bestilt mer enn " + STRING(iBestVarsel) + " av denne artikklen"
              VIEW-AS ALERT-BOX INFORMATION.

/*       DYNAMIC-FUNCTION("DoMessage",0,0,"Du har bestilt mer enn " + STRING(iBestVarsel) + " av denne artikklen","Informasjon",""). */
      ASSIGN fCurrArtNr = hFieldMapLinje:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE
             bWarning   = FALSE.
    END.
  END.
  ASSIGN fiSumAnt:SCREEN-VALUE   = STRING(fiSumAnt)
         fiSumVerdi:SCREEN-VALUE = STRING(fiSumVerdi).
/*   DISP fiSumAnt fiSumVerdi WITH FRAME frmLinje. */
END.

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
cState = DYNAMIC-FUNCTION('getToolbarState',hUpdToolBar).

DO WITH FRAME {&FRAME-NAME}:

  IF iiTab > 10 THEN DO:
    iiTab = iiTab - 10.
    chTabStrip:TabStrip:Tabs:ITEM(iiTab):SELECTED = TRUE.
  END.

  iTab = iiTab.

  DYNAMIC-FUNCTION("DeleteObjectLink",hBrowseRegStat,hBrowseListe).

  hArtBildeFrame:MOVE-TO-BOTTOM().

  IF iTab NE 3 THEN
    hTotalMenu:SENSITIVE = NO.


  IF iTab < 3 THEN DO:
    DYNAMIC-FUNCTION("ReplaceObjectLink",hUpdToolbar,hFieldMap).
    DYNAMIC-FUNCTION("ReplaceObjectLink",hUpdToolbar,hBrowseListe).
    IF VALID-HANDLE(hNavVarebehToolBar) THEN DO:
      DYNAMIC-FUNCTION("DeleteObject",hNavVarebehToolBar).
      hNavVarebehToolBar = ?.
    END.

    ASSIGN hRowsToBatchMenu:SENSITIVE = FALSE
           hQueryCountMenu:SENSITIVE  = FALSE NO-ERROR.
  END.

  IF iiTab = 1 THEN
  DO:
      FRAME frmListe:MOVE-TO-TOP().
      FRAME frmFilter:MOVE-TO-TOP().
      APPLY "entry" TO hBrowseListe.
  END.
  ELSE IF iiTab = 2 THEN DO:
    FRAME frmDetalj:MOVE-TO-TOP().
  END.
  ELSE IF iiTab = 3 THEN DO:
    hArtBildeFrame:MOVE-TO-TOP().
    FRAME frmLinje:MOVE-TO-TOP().

    ASSIGN hRowsToBatchMenu:SENSITIVE = TRUE
           hQueryCountMenu:SENSITIVE  = TRUE 
           hTotalMenu:SENSITIVE       = YES NO-ERROR.

    DYNAMIC-FUNCTION("replaceObjectLink",hBrowseLinje,hUpdToolbar).
    DYNAMIC-FUNCTION("replaceObjectLink",hFieldMapLinje,hUpdToolbar).


    hColumn = hBrowseTrans:GET-BROWSE-COLUMN(1).
    APPLY "END-RESIZE" TO hColumn.

    IF NOT hFieldMap:AVAIL THEN 
      hBrowseListe:SELECT-ROW(hBrowseListe:FOCUSED-ROW) NO-ERROR.

    IF TRIM(DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"queryfilter")) = "and false" THEN
      RUN OpenQuery.

    APPLY "value-changed" TO hBrowseLinje.
    APPLY "ENTRY":U TO hBrowseLinje.

  END.
  ELSE IF iiTab = 4 THEN DO:
    FRAME frmRegStat:MOVE-TO-TOP().

    ASSIGN hRowsToBatchMenu:SENSITIVE = FALSE 
           hQueryCountMenu:SENSITIVE  = FALSE NO-ERROR.

    DYNAMIC-FUNCTION("createParentLink",hBrowseRegStat,hBrowseListe,'VarebehNr').
    DYNAMIC-FUNCTION("replaceObjectLink",hBrowseRegStat,hUpdToolbar).
    DYNAMIC-FUNCTION("replaceObjectLink",hFieldMapRegStat,hUpdToolbar).

    IF NOT hFieldMap:AVAIL THEN 
      hBrowseListe:SELECT-ROW(hBrowseListe:FOCUSED-ROW) NO-ERROR.

    APPLY "value-changed" TO hBrowseListe.
    IF ButikkNr:SCREEN-VALUE IN FRAME frmLinje NE ? AND ButikkNr:SCREEN-VALUE NE "0" THEN DO: 
      bOK =  hFieldMapRegStat:FIND-FIRST("WHERE ButikkNr = " + ButikkNr:SCREEN-VALUE) NO-ERROR. 
      IF bOK THEN      
        hBrowseRegStat:QUERY:REPOSITION-TO-ROWID(hFieldMapRegStat:ROWID).
        APPLY "value-changed" TO hBrowseRegStat.
    END.
      .
    APPLY "value-changed":U TO hBrowseRegStat.
    APPLY "entry":U TO hBrowseRegStat.

  END.

  IF iiTab > 2 AND hFieldMap:AVAIL AND NOT VALID-HANDLE(hNavVarebehToolBar) THEN DO:
    hNavVarebehToolBar = DYNAMIC-FUNCTION("NewToolBar",
                      rectNavVarebeh:HANDLE,            /* Rectangle to define coordinates for toolbar */
                      "Naviger",                          /* Corresponding menu label - no menu if blank */
                      "Last;Siste Varebeh&,Next;Neste Varebeh,Prev;Forrige Varebeh,First;Første Varebeh",
                                                      /* Buttons / Menu items: action;label;tooltip;Method;image,action;label.. 
                                                         Any number of properties accepted (one ok - if predef. action) */
                      "right").                       /* Misc - for something I might need in next version.. */

    DYNAMIC-FUNCTION("CreateObjectLink",hNavVarebehToolBar,hBrowseListe).
    DYNAMIC-FUNCTION("CreateObjectLink",hNavVarebehToolBar,hFieldMap).
    DYNAMIC-FUNCTION("setToolbar",hNavVarebehToolBar,"enable").
    DYNAMIC-FUNCTION("setAddMoveX",THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, 
                                  "rectNavVarebeh," +
                                  DYNAMIC-FUNCTION("getToolbarNames",hNavVarebehToolBar,"")    
                                  ).
  END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ViewQueryStat C-Win 
FUNCTION ViewQueryStat RETURNS LOGICAL
  ( INPUT ihBrowse AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF ihBrowse = hBrowseRegStatTrans THEN 
  fiTotSumOrdre:SCREEN-VALUE IN FRAME frmRegStat = DYNAMIC-FUNCTION("getAttribute",ihBrowse,"statvalueOrdreTot").

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

