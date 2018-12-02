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
DEF VAR cCL                    AS CHAR   NO-UNDO.
DEF VAR cButCL                 AS CHAR   NO-UNDO.
DEF VAR bAlleTrans             AS LOG    NO-UNDO.
DEF VAR bStrekkodeReg          AS LOG    NO-UNDO.
DEF VAR iColNumBestilt         AS INT    NO-UNDO.
DEF VAR hBestiltAnt            AS HANDLE NO-UNDO.
DEF VAR hBestiltAntCol         AS HANDLE NO-UNDO.
DEF VAR hIkkeSendtBest         AS HANDLE NO-UNDO.

DEF VAR dSisteLevDato          AS DATE   NO-UNDO.
DEF VAR cButikkListe           AS CHAR   NO-UNDO INIT "174,260".
DEF VAR cTigjButikker          AS CHAR   NO-UNDO.
DEF VAR cStrListe              AS CHAR   NO-UNDO INIT "XS,S,M,L,XL,XXL".
DEF VAR iBestVarebeh           AS INT    NO-UNDO INIT 1. /* Vareh.bok type bestilling */
                              
DEF VAR hToolbar               AS HANDLE NO-UNDO.
DEF VAR hUpdToolbar            AS HANDLE NO-UNDO.
DEF VAR hBestHodeToolbar       AS HANDLE NO-UNDO. /* Bestillingshode: VarebehBestHode */
DEF VAR hOrdreToolbar          AS HANDLE NO-UNDO.
DEF VAR hBestToolbar           AS HANDLE NO-UNDO. /* Bestillingshode: BestHode */
DEF VAR hBrowseListe           AS HANDLE NO-UNDO.
DEF VAR cBrwListFlds           AS CHAR   NO-UNDO.
DEF VAR cBrwListJoin           AS CHAR   NO-UNDO.
DEF VAR hBrowseLinje           AS HANDLE NO-UNDO.
DEF VAR hBrowseOrdre           AS HANDLE NO-UNDO.
DEF VAR hBrowseOrdreBest       AS HANDLE NO-UNDO.
DEF VAR hBrowseBestHode        AS HANDLE NO-UNDO.
DEF VAR hBrowseAndreBest       AS HANDLE NO-UNDO.
DEF VAR hBrowseOrdreBestStr    AS HANDLE NO-UNDO.
DEF VAR hBrowseOrdreBestLev    AS HANDLE NO-UNDO.
DEF VAR hFieldMap              AS HANDLE NO-UNDO.
DEF VAR hFieldMapLinje         AS HANDLE NO-UNDO.
DEF VAR hFieldMapBestHode      AS HANDLE NO-UNDO.
DEF VAR hBuffLinje             AS HANDLE NO-UNDO.
DEF VAR hSearchListe           AS HANDLE NO-UNDO.
DEF VAR hSearchLinje           AS HANDLE NO-UNDO.
DEF VAR hWindow                AS HANDLE NO-UNDO.
DEF VAR hColumn                AS HANDLE NO-UNDO.
DEF VAR hNavVarebehToolBar     AS HANDLE NO-UNDO.
DEF VAR hLagerBestButton       AS HANDLE NO-UNDO.
DEF VAR hEtikettVindu          AS HANDLE NO-UNDO.
DEF VAR hArtBilde              AS HANDLE NO-UNDO.
DEF VAR hArtBildeFrame         AS HANDLE NO-UNDO.
DEF VAR hBestBilde             AS HANDLE NO-UNDO.
DEF VAR hBestBildeFrame        AS HANDLE NO-UNDO.
DEF VAR hLagerStatusFrame      AS HANDLE NO-UNDO.
DEF VAR hLagerStatusOCXframe   AS HANDLE NO-UNDO.

DEF VAR wArtBasRecid           AS RECID  NO-UNDO.

DEF VAR cCurrSelectBuffer      AS CHAR   NO-UNDO.
DEF VAR iSelectorSourcCount    AS INT    NO-UNDO.
DEF VAR cHuvGrAvdelingList     AS CHAR   NO-UNDO.
DEF VAR cVarGrHuvGrList        AS CHAR   NO-UNDO.
DEF VAR cLevBasRowIdList       AS CHAR   NO-UNDO.
DEF VAR cLevBasIdList          AS CHAR   NO-UNDO.
DEF VAR cSaSongRowIdList       AS CHAR   NO-UNDO.
DEF VAR cSaSongIdList          AS CHAR   NO-UNDO.
DEF VAR cAvdelingRowIdList     AS CHAR   NO-UNDO.
DEF VAR cAvdelingIdList        AS CHAR   NO-UNDO.
DEF VAR cHuvGrRowIdList        AS CHAR   NO-UNDO.
DEF VAR cHuvGrIdList           AS CHAR   NO-UNDO.
DEF VAR cVarGrRowIdList        AS CHAR   NO-UNDO.
DEF VAR cVarGrIdList           AS CHAR   NO-UNDO.
DEF VAR cButikerRowIdList      AS CHAR   NO-UNDO.
DEF VAR cButikerIdList         AS CHAR   NO-UNDO.
DEF VAR bNewArt                AS LOG    NO-UNDO.
DEF VAR bAlt-S                 AS LOG    NO-UNDO.
DEF VAR cInitPrisProfil        AS CHAR   NO-UNDO.
DEF VAR cBestTypeDesc          AS CHAR   NO-UNDO.
DEF VAR cInnlevTypeDesc        AS CHAR   NO-UNDO.
DEF VAR cAlle                  AS CHAR   NO-UNDO.
DEF VAR cKode                  AS CHAR   NO-UNDO.
DEF VAR cTmpKode               AS CHAR   NO-UNDO.
DEF VAR cTekst                 AS CHAR   NO-UNDO.
DEF VAR cVPILevKortNavnLevNr   AS CHAR   NO-UNDO.
DEF VAR cDefaultDirLev         AS CHAR   NO-UNDO.

DEF VAR cAdgang                AS CHAR   NO-UNDO.
DEF VAR hTLdet                 AS HANDLE NO-UNDO.
DEF VAR hVisBilde              AS HANDLE NO-UNDO.
DEF VAR hArtikkelkort          AS HANDLE NO-UNDO.
DEF VAR hArtikkelKopi          AS HANDLE NO-UNDO.
DEF VAR hBufArtStr             AS HANDLE NO-UNDO.
                              
DEF VAR iTab                   AS INT    NO-UNDO.
                              
DEF VAR cState                 AS CHAR   NO-UNDO.
DEF VAR cVarebehRowIdList      AS CHAR   NO-UNDO.
DEF VAR cVarebehIdList         AS CHAR   NO-UNDO.
                              
DEF VAR hUtvalg                AS HANDLE NO-UNDO.
DEF VAR bUtvalgIsMaster        AS LOG    NO-UNDO.
DEF VAR cBtnUtvalgHandles      AS CHAR   NO-UNDO.
DEF VAR cBtnHentUtvalgHandles  AS CHAR   NO-UNDO.
DEF VAR fArtikkelNr            AS DEC    NO-UNDO.
DEF VAR rRowIdLinje            AS ROWID  NO-UNDO.
DEF VAR cVarebehLinjeJoin      AS CHAR   NO-UNDO.
DEF VAR cAktivitetJoin         AS CHAR   NO-UNDO.
DEF VAR cBestHodeJoin          AS CHAR   NO-UNDO.
DEF VAR cVarebehLBuffAndFlds   AS CHAR   NO-UNDO.
DEF VAR cVarebehLinjeThodeJoin AS CHAR   NO-UNDO.
DEF VAR cVarebehLinjeTransJoin AS CHAR   NO-UNDO.
DEF VAR hKalkyleButton         AS HANDLE NO-UNDO.
DEF VAR hStrekkodeButton       AS HANDLE NO-UNDO.

DEF VAR hStrekkode             AS HANDLE NO-UNDO.
DEF VAR hLagerstatus           AS HANDLE NO-UNDO.

DEF VAR cStatFieldHandles      AS CHAR   NO-UNDO.
DEF VAR cStatFieldNames        AS CHAR   NO-UNDO.
DEF VAR cStatFieldPrefixList   AS CHAR   NO-UNDO.
DEF VAR cPrefixedStatFields    AS CHAR   NO-UNDO.
DEF VAR cBestViewFields        AS CHAR   NO-UNDO.
DEF VAR cInnlevViewFields      AS CHAR   NO-UNDO.
DEF VAR cModellFarge           AS CHAR   NO-UNDO.
DEF VAR cModellArtList         AS CHAR   NO-UNDO.
DEF VAR iModIdx                AS INT    NO-UNDO.
DEF VAR bUpdateStat            AS LOG    NO-UNDO.
DEF VAR hHasteordreColumn      AS HANDLE NO-UNDO.
DEF VAR hHasteordreField       AS HANDLE NO-UNDO.
DEF VAR hHasteordreOverlay     AS HANDLE NO-UNDO.
DEF VAR hBuffOrdre             AS HANDLE NO-UNDO.
DEF VAR hBrwColKjedeVare       AS HANDLE NO-UNDO.
DEF VAR hBrwColArtBeskr        AS HANDLE NO-UNDO. 
DEF VAR hBrwColGjFakt          AS HANDLE NO-UNDO.
DEF VAR hBrwOLMerk             AS HANDLE NO-UNDO.
DEF VAR iFontWingdings         AS INT    NO-UNDO.
DEF VAR hFieldRGBcolor         AS HANDLE NO-UNDO.
DEF VAR dMinLevDato            AS DATE   NO-UNDO.
DEF VAR iCurrLevNr             AS INT    NO-UNDO.
DEF VAR cAlfaFord              AS CHAR   NO-UNDO.
DEF VAR hPakkliste             AS HANDLE NO-UNDO.
DEF VAR cDefaultViewFields     AS CHAR   NO-UNDO.
DEF VAR cCreateViewFields      AS CHAR   NO-UNDO
  INIT "LevKod,Beskr,LevFargKod,MatKod,MatBeskr,StrTypeId,Beskrivelse,Vg,VgBeskr,InnkjopsPris,ForhRab%,SupRab%,Varekost,AnbefaltPris,Pris,LinjeMerknad,Farg,FarBeskr".
DEF VAR cCreateDisplayFields   AS CHAR   NO-UNDO
  INIT "FarBeskr,Beskrivelse,VgBeskr,MatBeskr".
DEF VAR cArtFields     AS CHAR NO-UNDO INIT "ArtikkelNr,LevKod,Beskr,LevFargKod,Farg,StrTypeId,Vg,LinjeMerknad,ForhRab%,SupRab%,AnbefaltPris".
DEF VAR cArtPrisFields AS CHAR NO-UNDO INIT "InnkjopsPris,Pris,VareKost".

DEF VAR hBrwOLLevKod           AS HANDLE NO-UNDO.
DEF VAR hBrwOLBeskr            AS HANDLE NO-UNDO.
DEF VAR hBrwOLLevFargKod       AS HANDLE NO-UNDO.
DEF VAR hBrwOLFarg             AS HANDLE NO-UNDO.
DEF VAR hBrwOLMatKod           AS HANDLE NO-UNDO.
DEF VAR hBrwOLStrTypeId        AS HANDLE NO-UNDO.
DEF VAR hBrwOLVg               AS HANDLE NO-UNDO.
DEF VAR hBrwOLInnkjopsPris     AS HANDLE NO-UNDO.
DEF VAR hBrwOLForhRab%         AS HANDLE NO-UNDO.
DEF VAR hBrwOLSupRab%          AS HANDLE NO-UNDO.
DEF VAR hBrwOLVarekost         AS HANDLE NO-UNDO.
DEF VAR hBrwOLPris             AS HANDLE NO-UNDO.
DEF VAR hBrwOLAnbefaltPris     AS HANDLE NO-UNDO.
DEF VAR hBrwOLMerkFi           AS HANDLE NO-UNDO.
DEF VAR hArtBasSok             AS HANDLE NO-UNDO.
DEF VAR httArtKalk             AS HANDLE NO-UNDO.
DEF VAR hbArtKalk              AS HANDLE NO-UNDO.
DEF VAR bNyregistrering        AS LOG    NO-UNDO.
DEF VAR cLevBasOrdreIdList     AS CHAR   NO-UNDO.
DEF VAR cLevBasOrdreRowidList  AS CHAR   NO-UNDO.
DEF VAR bSkipInitGrid          AS LOG    NO-UNDO.
DEF VAR bSetBestPanel          AS LOG    NO-UNDO.
DEF VAR hSelectorPanel         AS HANDLE NO-UNDO.
DEF VAR cRappBestStatusList    AS CHAR   NO-UNDO.
DEF VAR bAlreadyInitialized    AS LOG    NO-UNDO.

DEFINE STREAM Stream1.

DEF TEMP-TABLE ttBildeData
    FIELD BildNr    AS INT
    FIELD Teller    AS INT
    FIELD RawData   AS RAW
    FIELD RowIdent  AS CHAR
    .
DEF VAR hBufBildeData AS HANDLE.
hBufBildeData = BUFFER ttBildeData:HANDLE.

DEF TEMP-TABLE ttVarebehLinjeTrans NO-UNDO
    FIELD VarebehNr    AS DEC
    FIELD ArtikkelNr   AS DEC
    FIELD ButikkNr     AS INT
    FIELD kode         AS CHAR.
DEF VAR httVarebehLinjeTrans AS HANDLE NO-UNDO.
httVarebehLinjeTrans = BUFFER ttVarebehLinjeTrans:HANDLE:TABLE-HANDLE.

/* DEF TEMP-TABLE TT_VareBehBestLinje NO-UNDO LIKE VareBehBestLinje RCODE-INFORMATION.  */
{incl/tt_varebehbestlinje.i}
DEF VAR hBufftt_varebehbestlinje AS HANDLE NO-UNDO.
hBufftt_varebehbestlinje = BUFFER tt_VarebehBestLinje:HANDLE.

DEF TEMP-TABLE ttSort
    FIELD iSeq    AS INT
    FIELD cValue  AS CHAR.

DEF TEMP-TABLE ttRapport NO-UNDO
    FIELD cRpLinje AS CHAR.
DEF VAR httRapport AS HANDLE NO-UNDO.
httRapport = BUFFER ttRapport:HANDLE:TABLE-HANDLE.

DEF TEMP-TABLE ttVerdier
    FIELD cNavn   AS CHAR
    FIELD cVerdi  AS CHAR
    .
DEF VAR httVerdier  AS HANDLE NO-UNDO.
httVerdier = BUFFER ttVerdier:TABLE-HANDLE.

DEF TEMP-TABLE ttInit
    FIELD cNavn   AS CHAR
    FIELD cVerdi  AS CHAR
    FIELD hFillIn AS HANDLE.

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
VarebehNr VarebehBeskrivelse Beskrivelse MesseNr MesseBeskrivelse KortNavn ~
fi-ModusTekst 
&Scoped-Define DISPLAYED-OBJECTS VarebehNr VarebehBeskrivelse VarebehType ~
Beskrivelse MesseNr MesseBeskrivelse ProfilNr KortNavn fi-ModusTekst 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 BUTTON-Paste sokFraEdato sokTilEdato sokFraVPIdato ~
sokTilVPIdato sokFraRegDatoBestInnl sokTilRegDatoBestInnl tbRepeat btnLev2 ~
btnAvdeling2 btnHuvGr2 btnVarGr2 rectSettings 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AddStr C-Win 
FUNCTION AddStr RETURNS LOGICAL
  ( INPUT ifArtikkelNr  AS DEC,
    INPUT icStorl       AS CHAR,
    INPUT iiAnt         AS INT,
    INPUT icAddReplace  AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AdjustBrowseColumns C-Win 
FUNCTION AdjustBrowseColumns RETURNS LOGICAL
  ( INPUT ihBrowse     AS HANDLE,
    INPUT icBrowseName AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ClearFilter C-Win 
FUNCTION ClearFilter RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD EndreMangeLinjer C-Win 
FUNCTION EndreMangeLinjer RETURNS LOGICAL
  ( INPUT icField AS CHAR,
    INPUT icValue AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FiksStorl C-Win 
FUNCTION FiksStorl RETURNS CHARACTER
  ( INPUT wStorl AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FreezeWin C-Win 
FUNCTION FreezeWin RETURNS LOGICAL
  ( INPUT ibFreeze AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getArtKalkTT C-Win 
FUNCTION getArtKalkTT RETURNS HANDLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getLevDato C-Win 
FUNCTION getLevDato RETURNS DATE
  ( INPUT iLevNr AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getLevNr C-Win 
FUNCTION getLevNr RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getReportFile C-Win 
FUNCTION getReportFile RETURNS CHARACTER
  ( INPUT icClientRepFile AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getStorl C-Win 
FUNCTION getStorl RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getVarebehNr C-Win 
FUNCTION getVarebehNr RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD HentVerdier C-Win 
FUNCTION HentVerdier RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD InitOverlays C-Win 
FUNCTION InitOverlays RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD InitUtvalgToVarebeh C-Win 
FUNCTION InitUtvalgToVarebeh RETURNS LOGICAL
  ( INPUT ihUtvalg         AS HANDLE,
    INPUT ibUtvalgIsMaster AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD RapportToExcel C-Win 
FUNCTION RapportToExcel RETURNS LOGICAL
  ( INPUT icFileNameCount  AS CHAR,
    INPUT icButList        AS CHAR,
    INPUT icLevList        AS CHAR,
    INPUT icBestStatusList AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Registrerat C-Win 
FUNCTION Registrerat RETURNS LOGICAL
  ( INPUT ipRow AS INTEGER )  FORWARD.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setNotat C-Win 
FUNCTION setNotat RETURNS LOGICAL
  ( INPUT icMerke AS CHAR,
    INPUT icVerdi AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setOverlays C-Win 
FUNCTION setOverlays RETURNS LOGICAL
  ( INPUT icAction AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setOverlayValues C-Win 
FUNCTION setOverlayValues RETURNS LOGICAL
  ( INPUT icArtikkelNr AS CHAR,
    INPUT icLevKod     AS CHAR )  FORWARD.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SettLevDatoEllerDag C-Win 
FUNCTION SettLevDatoEllerDag RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SettUtvalgSensitive C-Win 
FUNCTION SettUtvalgSensitive RETURNS LOGICAL
  ( INPUT ibSensitive AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setView C-Win 
FUNCTION setView RETURNS LOGICAL
  ( INPUT icMode AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SkrivEtikett C-Win 
FUNCTION SkrivEtikett RETURNS LOGICAL
  ( INPUT icListe AS CHAR )  FORWARD.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ViewFrameOkReg C-Win 
FUNCTION ViewFrameOkReg RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ViewOverlays C-Win 
FUNCTION ViewOverlays RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ViewQueryStat C-Win 
FUNCTION ViewQueryStat RETURNS LOGICAL
  ( INPUT ihBrowse AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD VisLagerStatus C-Win 
FUNCTION VisLagerStatus RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD xx_fetchProcessedRows C-Win 
FUNCTION xx_fetchProcessedRows RETURNS LOGICAL
  ( INPUT ihBuffer AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE Grid AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chGrid AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE TabStrip AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chTabStrip AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE Beskrivelse AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 21.6 BY 1.

DEFINE VARIABLE fi-ModusTekst AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 40.2 BY 1.14
     FGCOLOR 9 FONT 12 NO-UNDO.

DEFINE VARIABLE KortNavn AS CHARACTER FORMAT "X(15)" 
     VIEW-AS FILL-IN 
     SIZE 12.6 BY 1.

DEFINE VARIABLE MesseBeskrivelse AS CHARACTER FORMAT "X(40)" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE MesseNr AS DECIMAL FORMAT ">>>>>>>9" INITIAL 0 
     LABEL "Innkj.per" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE ProfilNr AS INTEGER FORMAT ">>>>>>9" INITIAL 0 
     LABEL "Prisprofil" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1.

DEFINE VARIABLE VarebehBeskrivelse AS CHARACTER FORMAT "X(40)" 
     VIEW-AS FILL-IN 
     SIZE 22.8 BY 1.

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

DEFINE BUTTON btnButikker 
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnSokMesseNr  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnSokProfilNr  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnVareBokNr 
     LABEL "..." 
     SIZE 4 BY 1.

DEFINE VARIABLE VarebehNotat AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 60 BY 10.33 NO-UNDO.

DEFINE VARIABLE BeskrivelseVareBehType AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1.

DEFINE VARIABLE ButikkListe AS CHARACTER FORMAT "X(60)" 
     LABEL "Butikkliste" 
     VIEW-AS FILL-IN 
     SIZE 55 BY 1.

DEFINE VARIABLE EkstRef AS CHARACTER FORMAT "X(60)" 
     LABEL "Ekst.referanse" 
     VIEW-AS FILL-IN 
     SIZE 60 BY 1.

DEFINE VARIABLE Kilde AS DECIMAL FORMAT ">>>>>>>>>>>>9" INITIAL 0 
     LABEL "VarebokNr" 
     VIEW-AS FILL-IN 
     SIZE 20.2 BY 1 TOOLTIP "Vareboknummer".

DEFINE VARIABLE OppdatAv AS CHARACTER FORMAT "X(15)" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE OppdatDato AS DATE FORMAT "99/99/99" 
     LABEL "Dato/BrukerId" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE VareBokBeskrivelse AS CHARACTER FORMAT "X(40)" 
     VIEW-AS FILL-IN 
     SIZE 35.6 BY 1 TOOLTIP "Kort beskrivelse av vareboken".

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 70 BY 1.43.

DEFINE VARIABLE Oppdatert AS LOGICAL INITIAL no 
     LABEL "Oppdatert" 
     VIEW-AS TOGGLE-BOX
     SIZE 14.6 BY .81 NO-UNDO.

DEFINE BUTTON btnBlankFilter 
     LABEL "Blank filter" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnLev-2 
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnSokFiltMesseNr 
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnSokFiltProfilNr 
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE cmbVarebehType AS CHARACTER FORMAT "X(256)":U 
     LABEL "Type" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "Alle","Alle",
                     "Bestilling","Bestilling",
                     "Varemottak","Varemottak"
     DROP-DOWN-LIST
     SIZE 28.4 BY 1 NO-UNDO.

DEFINE VARIABLE sokBestStat AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE fi-cEkstRef AS CHARACTER FORMAT "X(20)" 
     LABEL "Ekst.referanse" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 TOOLTIP "Ekstern referanse for suppleringsboken (pakkseddel, ol)".

DEFINE VARIABLE fi-cVarebehBeskrivelse AS CHARACTER FORMAT "X(40)" 
     LABEL "Beskrivelse" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1.

DEFINE VARIABLE fi-fMesseNr AS DECIMAL FORMAT ">>>>>>>9" INITIAL 0 
     LABEL "Innkj.per" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE fi-fProfilNr AS INTEGER FORMAT ">>>>>>9" INITIAL 0 
     LABEL "Prisprofil" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE fi-fVarebehNr AS DECIMAL FORMAT ">>>>>>>9":U INITIAL 0 
     LABEL "Vareh.nr" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1 NO-UNDO.

DEFINE VARIABLE ListeSokLevNamn AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 26.8 BY 1 NO-UNDO.

DEFINE VARIABLE ListeSokLevNr AS INTEGER FORMAT "zzzzz9" INITIAL 0 
     LABEL "Leverandør" 
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1.

DEFINE VARIABLE sokEkstId AS CHARACTER FORMAT "X(256)":U 
     LABEL "Ekstern id" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 TOOLTIP "Ekstern id for enkeltbestilling" NO-UNDO.

DEFINE VARIABLE sokGlobOrdrenr AS INTEGER FORMAT "->>>>>>>>9":U INITIAL 0 
     LABEL "Ordrenr" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE RECTANGLE RectArtSok
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 34 BY 3.29.

DEFINE VARIABLE tbKundeOrdre AS LOGICAL INITIAL no 
     LABEL "Kun suppl.bøker for kundeordre" 
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY .81 NO-UNDO.

DEFINE VARIABLE tbMatchBestStat AS LOGICAL INITIAL no 
     LABEL "Bestillingen skal være i status" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE BUTTON btnAvdeling 
     LABEL "..." 
     SIZE 4.4 BY 1 TOOLTIP "Søk i avdelinger som finnes i vareboken".

DEFINE BUTTON btnAvdeling2 
     LABEL "..." 
     SIZE 4.4 BY 1 TOOLTIP "Søk alle avdelinger".

DEFINE BUTTON btnBlankLinjeFilter 
     LABEL "Blank filter" 
     SIZE 15 BY 1.1.

DEFINE BUTTON btnHuvGr 
     LABEL "..." 
     SIZE 4.4 BY 1 TOOLTIP "Søk i hovedgrupper som fins i vareboken".

DEFINE BUTTON btnHuvGr2 
     LABEL "..." 
     SIZE 4.4 BY 1 TOOLTIP "Søk i alle hovedgrupper".

DEFINE BUTTON btnLev 
     LABEL "..." 
     SIZE 4.4 BY 1 TOOLTIP "Søk leverandører i varebok".

DEFINE BUTTON btnLev2 
     LABEL "..." 
     SIZE 4.4 BY 1 TOOLTIP "Søk hele leverandør-register".

DEFINE BUTTON btnLevDatoDate 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 3.8 BY .81.

DEFINE BUTTON btnLinjemerknad 
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnSaSong 
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnSplitBarY 
     IMAGE-UP FILE "bmp/tabdown.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Button 1" 
     SIZE 197.6 BY .48.

DEFINE BUTTON btnTotal 
     LABEL "Oppdater total:" 
     SIZE 17 BY 1.14.

DEFINE BUTTON btnVarGr 
     LABEL "..." 
     SIZE 4.4 BY 1 TOOLTIP "Søk i varegrupper som fins i vareboken".

DEFINE BUTTON btnVarGr-2 
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnVarGr2 
     LABEL "..." 
     SIZE 4.4 BY 1 TOOLTIP "Søk i alle varegrupper".

DEFINE BUTTON BUTTON-Paste 
     IMAGE-UP FILE "icon\e-paste":U NO-FOCUS
     LABEL "" 
     SIZE 4.6 BY 1.1 TOOLTIP "Henter bilde fra ClipBoard" DROP-TARGET.

DEFINE BUTTON button-slett 
     LABEL "Slett rad" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE sokRegBestStat AS CHARACTER FORMAT "X(256)":U 
     LABEL "Best.stat" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE Kode AS CHARACTER FORMAT "X(20)" 
     LABEL "Strekkode" 
     VIEW-AS FILL-IN 
     SIZE 20.2 BY 1.

DEFINE VARIABLE LevDato AS DATE FORMAT "99/99/99":U 
     LABEL "Sendes" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE sokAktBeskrivelse AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 26.4 BY 1 NO-UNDO.

DEFINE VARIABLE sokAktNr AS INTEGER FORMAT "zzzzz9" INITIAL 0 
     LABEL "Aktivitet" 
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1.

DEFINE VARIABLE sokArtikkelNr AS DECIMAL FORMAT ">>>>>>>>>9":U INITIAL 0 
     LABEL "Artnr/Utv.s" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE sokAvdelingNavn AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 26.4 BY 1 NO-UNDO.

DEFINE VARIABLE sokAvdelingNr AS INTEGER FORMAT "zzzzz9" INITIAL 0 
     LABEL "Avdeling" 
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1.

DEFINE VARIABLE sokBeskr AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 30.2 BY 1
     BGCOLOR 16  NO-UNDO.

DEFINE VARIABLE sokFraEdato AS DATE FORMAT "99/99/99":U 
     LABEL "Endret dato" 
     VIEW-AS FILL-IN 
     SIZE 14.6 BY 1 NO-UNDO.

DEFINE VARIABLE sokFraRegDatoBestInnl AS DATE FORMAT "99/99/99" 
     LABEL "Reg.dato" 
     VIEW-AS FILL-IN 
     SIZE 14.6 BY 1 TOOLTIP "NB! bestemmer beregning av sum pr artikkel".

DEFINE VARIABLE sokFraVPIdato AS DATE FORMAT "99/99/99":U 
     LABEL "VPI dato" 
     VIEW-AS FILL-IN 
     SIZE 14.6 BY 1 NO-UNDO.

DEFINE VARIABLE sokHg AS INTEGER FORMAT "zzzzz9" INITIAL 0 
     LABEL "Hovedgr" 
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1.

DEFINE VARIABLE sokHgBeskr AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 26.4 BY 1 NO-UNDO.

DEFINE VARIABLE sokLevKod AS CHARACTER FORMAT "X(256)":U 
     LABEL "Lev.art.nr" 
     VIEW-AS FILL-IN 
     SIZE 16.4 BY 1 NO-UNDO.

DEFINE VARIABLE sokLevNamn AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 35.6 BY 1 NO-UNDO.

DEFINE VARIABLE sokLevNr AS INTEGER FORMAT "zzzzz9" INITIAL 0 
     LABEL "Lev" 
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1.

DEFINE VARIABLE sokLinjeMerknad AS CHARACTER FORMAT "X(256)":U 
     LABEL "Merknad" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1 NO-UNDO.

DEFINE VARIABLE sokSasBeskr AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 17.6 BY 1 NO-UNDO.

DEFINE VARIABLE sokSaSong AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "Ses" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 TOOLTIP "Sesong" NO-UNDO.

DEFINE VARIABLE sokTilEdato AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 14.6 BY 1 NO-UNDO.

DEFINE VARIABLE sokTilRegDatoBestInnl AS DATE FORMAT "99/99/99" 
     VIEW-AS FILL-IN 
     SIZE 14.6 BY 1.

DEFINE VARIABLE sokTilVPIdato AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 14.6 BY 1 NO-UNDO.

DEFINE VARIABLE sokVg AS INTEGER FORMAT "zzzzz9" INITIAL 0 
     LABEL "Varegr" 
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1.

DEFINE VARIABLE sokVgBeskr AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 26.4 BY 1 NO-UNDO.

DEFINE VARIABLE SumPris AS DECIMAL FORMAT "-z,zzz,zz9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 20.2 BY 1 NO-UNDO.

DEFINE VARIABLE SumVarekost AS DECIMAL FORMAT "-z,zzz,zz9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 20.2 BY 1 NO-UNDO.

DEFINE VARIABLE rsLagerStatusSalg AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Lagerstatus (lager/i best)", 1,
"Salg", 2
     SIZE 38.2 BY .95 NO-UNDO.

DEFINE RECTANGLE ArtikkelBilde
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 27.6 BY 5.48.

DEFINE RECTANGLE rectBestHodeToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 18 BY 1.19.

DEFINE RECTANGLE rectBrowseLinje
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 198 BY 8.81.

DEFINE RECTANGLE RectBrowseSearchLinje
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 20.6 BY 1.19.

DEFINE RECTANGLE rectBrwAndreBest
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 82.2 BY 5.86.

DEFINE RECTANGLE rectBrwBestHode
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 114 BY 5.86.

DEFINE RECTANGLE rectSettings
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 82.8 BY 1.29.

DEFINE VARIABLE DirekteLev AS LOGICAL INITIAL yes 
     LABEL "Dir.lev" 
     VIEW-AS TOGGLE-BOX
     SIZE 10 BY .81 NO-UNDO.

DEFINE VARIABLE tbOpprettBest AS LOGICAL INITIAL yes 
     LABEL "Opprett/øk bestilling ved treff" 
     VIEW-AS TOGGLE-BOX
     SIZE 30.8 BY .81 TOOLTIP "Opprett/øk ikke-sendt bestilling ved treff på strekkode (1 stk - 1 butikk)" NO-UNDO.

DEFINE VARIABLE tbRepeat AS LOGICAL INITIAL no 
     LABEL "Gjenta art.søk(alt-S)" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .81 TOOLTIP "Gjenta søk etter artikkel automatisk når en registrering er ferdig" NO-UNDO.

DEFINE VARIABLE tbUpdateStat AS LOGICAL INITIAL no 
     LABEL "Vis total varekost og utpris" 
     VIEW-AS TOGGLE-BOX
     SIZE 28.4 BY .81 TOOLTIP "Skal sum varekost og pris beregnes etter hver endring/oppslag" NO-UNDO.

DEFINE RECTANGLE rectBrowseListe
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 198 BY 21.91.

DEFINE RECTANGLE RectBrowseSearchListe
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 22 BY 1.19.

DEFINE BUTTON btnAvbryt 
     LABEL "Avbryt" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnOk 
     LABEL "&OK" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnBlankOrdreFilter 
     LABEL "Blank filter" 
     SIZE 14.2 BY 1.14.

DEFINE BUTTON btnLev-3 
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnOrdreArtikkel 
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnOrdreLev 
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE cmbGjennomfaktureres AS CHARACTER FORMAT "X(256)":U 
     LABEL "Gjennomfakturering" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 9.6 BY 1 NO-UNDO.

DEFINE VARIABLE cmbKjedevare AS CHARACTER FORMAT "X(256)":U 
     LABEL "Kjedelevert" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 9.6 BY 1 NO-UNDO.

DEFINE VARIABLE sokBestOrdreStatus AS CHARACTER FORMAT "X(256)":U 
     LABEL "Best.status" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 23.8 BY 1 NO-UNDO.

DEFINE VARIABLE sokOpphav AS CHARACTER FORMAT "X(256)":U 
     LABEL "Opphav" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 11.2 BY 1 NO-UNDO.

DEFINE VARIABLE sokOrdremottaker AS CHARACTER FORMAT "X(256)":U 
     LABEL "Ordremottak" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 22.4 BY 1 NO-UNDO.

DEFINE VARIABLE SokOrdreArtBeskr AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 24.6 BY 1 NO-UNDO.

DEFINE VARIABLE sokOrdreArtikkel AS DECIMAL FORMAT ">>>>>>>>>9":U INITIAL 0 
     LABEL "Art.nr" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE sokOrdreCL AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "CL" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE SokOrdreCLNamn AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 15.4 BY 1 NO-UNDO.

DEFINE VARIABLE sokOrdreLevDato AS DATE FORMAT "99/99/99":U 
     LABEL "Lev.dato" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE sokOrdreLevFargKod AS CHARACTER FORMAT "X(256)":U 
     LABEL "Lev.fargek" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE sokOrdreLevKod AS CHARACTER FORMAT "X(256)":U 
     LABEL "Lev.artnr" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE SokOrdreLevNamn AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 25.6 BY 1 NO-UNDO.

DEFINE VARIABLE SokOrdreLevNr AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     LABEL "Levnr" 
     VIEW-AS FILL-IN 
     SIZE 8.4 BY 1.

DEFINE VARIABLE sokOrdreNr AS INTEGER FORMAT ">>>>>>>>9":U INITIAL 0 
     LABEL "Ordrenr" 
     VIEW-AS FILL-IN 
     SIZE 13.6 BY 1 NO-UNDO.

DEFINE RECTANGLE BestBilde
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 15 BY 2.86.

DEFINE RECTANGLE BestToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 18 BY 1.19.

DEFINE RECTANGLE BrwOrdreBest
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 132 BY 10.48.

DEFINE RECTANGLE BrwOrdreBestLev
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 63 BY 4.43.

DEFINE RECTANGLE BrwOrdreBestStr
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 63 BY 5.95.

DEFINE RECTANGLE OrdreToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 18 BY 1.1.

DEFINE RECTANGLE rectBrowseOrdre
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 196 BY 12.62.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     VarebehNr AT ROW 2.81 COL 9.8 COLON-ALIGNED HELP
          "Varebehnummer"
     VarebehBeskrivelse AT ROW 2.81 COL 26.2 COLON-ALIGNED HELP
          "Kort beskrivelse av Varebehen" NO-LABEL
     VarebehType AT ROW 2.81 COL 55.8 COLON-ALIGNED HELP
          "Varebehtype"
     Beskrivelse AT ROW 2.81 COL 60 COLON-ALIGNED HELP
          "Kort beskrivelse av Varebehtypen" NO-LABEL
     MesseNr AT ROW 2.81 COL 91.4 COLON-ALIGNED HELP
          "Messenummer"
     MesseBeskrivelse AT ROW 2.81 COL 104.8 COLON-ALIGNED HELP
          "Navn eller kort beskrivelse av messen." NO-LABEL
     ProfilNr AT ROW 2.81 COL 134 COLON-ALIGNED HELP
          "Prisprofil"
     KortNavn AT ROW 2.81 COL 144 COLON-ALIGNED HELP
          "Kort betegnelse på prisprofilen" NO-LABEL
     fi-ModusTekst AT ROW 2.71 COL 160 NO-LABEL
     rectToolBar AT ROW 1.19 COL 191
     rectUpdToolbar AT ROW 1.19 COL 1.4
     rectVarebeh AT ROW 2.57 COL 1
     rectNavVarebeh AT ROW 1.19 COL 159
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 200.2 BY 32.91.

DEFINE FRAME frmLinje
     BUTTON-Paste AT ROW 1.14 COL 166.2 NO-TAB-STOP 
     tbOpprettBest AT ROW 5.76 COL 56.2
     sokSaSong AT ROW 2.14 COL 88 COLON-ALIGNED HELP
          "Sesong"
     sokRegBestStat AT ROW 4.29 COL 122.8 COLON-ALIGNED
     btnTotal AT ROW 5.62 COL 112
     sokLevKod AT ROW 2.14 COL 66.4 COLON-ALIGNED
     rsLagerStatusSalg AT ROW 16.86 COL 116.8 NO-LABEL
     SumVarekost AT ROW 5.67 COL 127.2 COLON-ALIGNED NO-LABEL
     DirekteLev AT ROW 16.91 COL 104.8
     LevDato AT ROW 16.76 COL 83.4 COLON-ALIGNED
     tbUpdateStat AT ROW 10.76 COL 120
     sokAvdelingNr AT ROW 1.05 COL 8.8 COLON-ALIGNED HELP
          "Varegruppe"
     sokAvdelingNavn AT ROW 1.05 COL 19.6 COLON-ALIGNED NO-LABEL
     sokHg AT ROW 2.1 COL 8.8 COLON-ALIGNED HELP
          "Varegruppe"
     btnSplitBarY AT ROW 15.81 COL 1.4
     sokHgBeskr AT ROW 2.14 COL 19.6 COLON-ALIGNED NO-LABEL
     sokVg AT ROW 3.19 COL 8.8 COLON-ALIGNED HELP
          "Varegruppe"
     sokVgBeskr AT ROW 3.19 COL 19.6 COLON-ALIGNED NO-LABEL
     sokAktNr AT ROW 4.24 COL 8.8 COLON-ALIGNED HELP
          "Varegruppe"
     sokAktBeskrivelse AT ROW 4.24 COL 19.6 COLON-ALIGNED NO-LABEL
     sokLevNr AT ROW 1.1 COL 66.4 COLON-ALIGNED HELP
          "Leverandør - trykk F3 for å søke blant alle leverandører"
     sokLevNamn AT ROW 1.1 COL 77.4 COLON-ALIGNED NO-LABEL
     sokArtikkelNr AT ROW 3.19 COL 66.4 COLON-ALIGNED
     sokBeskr AT ROW 3.19 COL 82.8 COLON-ALIGNED NO-LABEL
     sokFraEdato AT ROW 1.1 COL 134 COLON-ALIGNED
     sokTilEdato AT ROW 1.1 COL 149 COLON-ALIGNED NO-LABEL
     sokFraVPIdato AT ROW 2.14 COL 134 COLON-ALIGNED
     sokTilVPIdato AT ROW 2.14 COL 149 COLON-ALIGNED NO-LABEL
     sokFraRegDatoBestInnl AT ROW 3.19 COL 134 COLON-ALIGNED HELP
          "Dato da posten ble registrert i registeret"
     sokTilRegDatoBestInnl AT ROW 3.19 COL 149 COLON-ALIGNED HELP
          "Dato da posten ble registrert i registeret" NO-LABEL
     Kode AT ROW 5.67 COL 33.6 COLON-ALIGNED HELP
          "Strekkode inklusive sjekksiffer."
     btnAvdeling AT ROW 1.1 COL 48 NO-TAB-STOP 
     btnHuvGr AT ROW 2.14 COL 48 NO-TAB-STOP 
     btnVarGr AT ROW 3.19 COL 48 NO-TAB-STOP 
     btnBlankLinjeFilter AT ROW 4.24 COL 150.8
     btnVarGr-2 AT ROW 4.19 COL 48 NO-TAB-STOP 
     tbRepeat AT ROW 5.81 COL 88.6
     button-slett AT ROW 22.52 COL 181.4
     btnLev AT ROW 1.1 COL 115 NO-TAB-STOP 
     btnLevDatoDate AT ROW 16.86 COL 100.4
     SumPris AT ROW 5.67 COL 147.4 COLON-ALIGNED NO-LABEL
     sokLinjeMerknad AT ROW 4.24 COL 66.4 COLON-ALIGNED
     btnLinjemerknad AT ROW 4.24 COL 110.8 NO-TAB-STOP 
     sokSasBeskr AT ROW 2.14 COL 95.4 COLON-ALIGNED NO-LABEL
     btnSaSong AT ROW 2.14 COL 115 NO-TAB-STOP 
     btnLev2 AT ROW 1.1 COL 119.2 NO-TAB-STOP 
     btnAvdeling2 AT ROW 1.1 COL 52.2 NO-TAB-STOP 
     btnHuvGr2 AT ROW 2.14 COL 52.2 NO-TAB-STOP 
     btnVarGr2 AT ROW 3.19 COL 52.2 NO-TAB-STOP 
     RectBrowseSearchLinje AT ROW 5.62 COL 1.4
     rectBrowseLinje AT ROW 6.95 COL 1
     rectBrwBestHode AT ROW 18 COL 1
     rectBrwAndreBest AT ROW 18 COL 115.8
     ArtikkelBilde AT ROW 1.14 COL 171
     rectBestHodeToolbar AT ROW 16.67 COL 2
     rectSettings AT ROW 5.57 COL 87.4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 5.48
         SCROLLABLE SIZE 198 BY 28.14.

DEFINE FRAME frmOkReg
     btnOk AT ROW 1 COL 1
     btnAvbryt AT ROW 1 COL 15.8
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 166.8 ROW 8.29
         SIZE 30.2 BY 1.29
         BGCOLOR 18 .

DEFINE FRAME frmListe
     rectBrowseListe AT ROW 2.43 COL 1
     RectBrowseSearchListe AT ROW 1.1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 10.24
         SCROLLABLE SIZE 198 BY 23.38.

DEFINE FRAME frmFilter
     cmbVarebehType AT ROW 1.24 COL 17.6 COLON-ALIGNED
     fi-fVarebehNr AT ROW 2.33 COL 17.6 COLON-ALIGNED
     fi-fMesseNr AT ROW 3.43 COL 17.6 COLON-ALIGNED HELP
          "Messenummer"
     fi-fProfilNr AT ROW 4.52 COL 17.6 COLON-ALIGNED HELP
          "Prisprofil"
     btnSokFiltMesseNr AT ROW 3.43 COL 32.8 NO-TAB-STOP 
     btnSokFiltProfilNr AT ROW 4.52 COL 32.8 NO-TAB-STOP 
     fi-cVarebehBeskrivelse AT ROW 1.24 COL 62 COLON-ALIGNED HELP
          "Kort beskrivelse av Varebehen"
     fi-cEkstRef AT ROW 2.33 COL 62 COLON-ALIGNED HELP
          "Ekstern referanse for suppleringsboken (pakkseddel, ol)"
     ListeSokLevNr AT ROW 3.43 COL 62 COLON-ALIGNED HELP
          "Varegruppe"
     sokGlobOrdrenr AT ROW 4.52 COL 62 COLON-ALIGNED
     btnLev-2 AT ROW 3.43 COL 74.6 NO-TAB-STOP 
     ListeSokLevNamn AT ROW 3.43 COL 77.2 COLON-ALIGNED NO-LABEL
     sokEkstId AT ROW 4.52 COL 89 COLON-ALIGNED HELP
          "Ekstern id for enkeltbestilling"
     tbKundeOrdre AT ROW 4.81 COL 109.8
     tbMatchBestStat AT ROW 2.05 COL 111.6
     sokBestStat AT ROW 3.24 COL 109 COLON-ALIGNED NO-LABEL
     btnBlankFilter AT ROW 4.48 COL 183.4
     "Ved søk etter artikkel (alt-s/F2)" VIEW-AS TEXT
          SIZE 31 BY .62 AT ROW 1.14 COL 111
     RectArtSok AT ROW 1.38 COL 109.8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 5.43
         SCROLLABLE SIZE 198 BY 4.86.

DEFINE FRAME frmDetalj
     btnSokMesseNr AT ROW 7.62 COL 34.4 NO-TAB-STOP 
     VarebehNr AT ROW 1.24 COL 19 COLON-ALIGNED HELP
          "Varebehnummer"
          LABEL "Vareh.Nr" FORMAT ">>>>>>>>>>>>9"
          VIEW-AS FILL-IN 
          SIZE 20.2 BY 1
     OppdatDato AT ROW 1.48 COL 127.6 COLON-ALIGNED
     OppdatAv AT ROW 1.48 COL 141.6 COLON-ALIGNED NO-LABEL
     Oppdatert AT ROW 1.52 COL 93.4 HELP
          "Varebeh er oppdatert"
     VarebehType AT ROW 2.33 COL 19 COLON-ALIGNED HELP
          "Varebehtype"
          LABEL "Vareh.type" FORMAT ">>9"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     BeskrivelseVareBehType AT ROW 2.33 COL 32 COLON-ALIGNED HELP
          "Kort beskrivelse av Varebehtypen" NO-LABEL
     ProfilNr AT ROW 3.38 COL 19 COLON-ALIGNED HELP
          "Prisprofil"
          LABEL "Prisprofil" FORMAT ">>>>>>9"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     KortNavn AT ROW 3.38 COL 36 COLON-ALIGNED HELP
          "Kort betegnelse på prisprofilen" NO-LABEL FORMAT "X(15)"
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     ButikkListe AT ROW 4.43 COL 19 COLON-ALIGNED
     btnButikker AT ROW 4.43 COL 76.4 NO-TAB-STOP 
     VarebehBeskrivelse AT ROW 5.48 COL 19 COLON-ALIGNED HELP
          "Kort beskrivelse av Varebehen"
          LABEL "Beskrivelse" FORMAT "X(40)"
          VIEW-AS FILL-IN 
          SIZE 60 BY 1
     EkstRef AT ROW 6.52 COL 19 COLON-ALIGNED HELP
          "Ekstern referanse"
     MesseNr AT ROW 7.62 COL 19 COLON-ALIGNED HELP
          "Innkjøpsperiode"
          LABEL "Innkj.per" FORMAT ">>>>>>>9"
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     MesseBeskrivelse AT ROW 7.62 COL 37 COLON-ALIGNED HELP
          "Navn eller kort beskrivelse av messen." NO-LABEL FORMAT "X(40)"
          VIEW-AS FILL-IN 
          SIZE 42 BY 1
     Kilde AT ROW 8.71 COL 19 COLON-ALIGNED
     btnVareBokNr AT ROW 8.71 COL 41.2 NO-TAB-STOP 
     VareBokBeskrivelse AT ROW 8.71 COL 43.4 COLON-ALIGNED NO-LABEL
     VarebehNotat AT ROW 9.95 COL 21 NO-LABEL
     btnSokProfilNr AT ROW 3.38 COL 33.2 NO-TAB-STOP 
     RECT-1 AT ROW 1.24 COL 92
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 5.48
         SCROLLABLE SIZE 198 BY 28.14.

DEFINE FRAME frmOrdre
     sokBestOrdreStatus AT ROW 1.05 COL 10.8 COLON-ALIGNED
     sokOrdreNr AT ROW 1.05 COL 43 COLON-ALIGNED
     sokEkstId AT ROW 1.05 COL 64.4 COLON-ALIGNED
          LABEL "Ekst.id" FORMAT "X(256)":U
          VIEW-AS FILL-IN 
          SIZE 15 BY 1 TOOLTIP "Ekstern id for enkeltbestilling"
     sokOrdreLevDato AT ROW 1.05 COL 91 COLON-ALIGNED
     sokOrdreCL AT ROW 1.05 COL 109 COLON-ALIGNED
     btnOrdreLev AT ROW 1.05 COL 117.2 NO-TAB-STOP 
     SokOrdreCLNamn AT ROW 1.05 COL 119.6 COLON-ALIGNED NO-LABEL
     SokOrdreLevNr AT ROW 1.05 COL 141.6 COLON-ALIGNED HELP
          "Varegruppe"
     btnLev-3 AT ROW 1.05 COL 152 NO-TAB-STOP 
     SokOrdreLevNamn AT ROW 1.05 COL 154.4 COLON-ALIGNED NO-LABEL
     btnBlankOrdreFilter AT ROW 2.05 COL 168
     sokOrdreArtikkel AT ROW 2.1 COL 10.8 COLON-ALIGNED
     btnOrdreArtikkel AT ROW 2.1 COL 27 NO-TAB-STOP 
     SokOrdreArtBeskr AT ROW 2.1 COL 29.4 COLON-ALIGNED NO-LABEL
     sokOrdreLevKod AT ROW 2.1 COL 64.4 COLON-ALIGNED
     sokOrdreLevFargKod AT ROW 2.1 COL 91 COLON-ALIGNED
     sokOrdremottaker AT ROW 2.1 COL 119.6 COLON-ALIGNED
     sokOpphav AT ROW 2.1 COL 154.4 COLON-ALIGNED
     cmbKjedevare AT ROW 3.14 COL 44 COLON-ALIGNED
     cmbGjennomfaktureres AT ROW 3.14 COL 74 COLON-ALIGNED
     rectBrowseOrdre AT ROW 4.33 COL 2
     BrwOrdreBest AT ROW 18.38 COL 2
     BrwOrdreBestStr AT ROW 18.38 COL 135
     BrwOrdreBestLev AT ROW 24.33 COL 135
     OrdreToolbar AT ROW 3.14 COL 2.6
     BestToolbar AT ROW 17.1 COL 2
     BestBilde AT ROW 1.14 COL 183
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 5.52
         SIZE 198 BY 28.


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
         TITLE              = "Suppleringsordre"
         HEIGHT             = 32.86
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
       FRAME frmOkReg:FRAME = FRAME frmLinje:HANDLE
       FRAME frmOrdre:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
ASSIGN 
       Beskrivelse:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN fi-ModusTekst IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
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
       FRAME frmDetalj:HEIGHT           = 28.14
       FRAME frmDetalj:WIDTH            = 198.

ASSIGN 
       BeskrivelseVareBehType:READ-ONLY IN FRAME frmDetalj        = TRUE.

ASSIGN 
       KortNavn:READ-ONLY IN FRAME frmDetalj        = TRUE.

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
   L-To-R,COLUMNS                                                       */
ASSIGN 
       FRAME frmFilter:HEIGHT           = 4.86
       FRAME frmFilter:WIDTH            = 198.

/* SETTINGS FOR FILL-IN ListeSokLevNamn IN FRAME frmFilter
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX sokBestStat IN FRAME frmFilter
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME frmLinje
   Custom                                                               */
ASSIGN 
       FRAME frmLinje:HEIGHT           = 28.14
       FRAME frmLinje:WIDTH            = 198.

/* SETTINGS FOR BUTTON btnAvdeling2 IN FRAME frmLinje
   1                                                                    */
/* SETTINGS FOR BUTTON btnHuvGr2 IN FRAME frmLinje
   1                                                                    */
/* SETTINGS FOR BUTTON btnLev2 IN FRAME frmLinje
   1                                                                    */
ASSIGN 
       btnSplitBarY:MOVABLE IN FRAME frmLinje          = TRUE.

/* SETTINGS FOR BUTTON btnVarGr2 IN FRAME frmLinje
   1                                                                    */
/* SETTINGS FOR BUTTON BUTTON-Paste IN FRAME frmLinje
   1                                                                    */
/* SETTINGS FOR BUTTON button-slett IN FRAME frmLinje
   NO-ENABLE                                                            */
ASSIGN 
       button-slett:HIDDEN IN FRAME frmLinje           = TRUE.

/* SETTINGS FOR RECTANGLE rectSettings IN FRAME frmLinje
   1                                                                    */
/* SETTINGS FOR FILL-IN sokFraEdato IN FRAME frmLinje
   1                                                                    */
/* SETTINGS FOR FILL-IN sokFraRegDatoBestInnl IN FRAME frmLinje
   1                                                                    */
/* SETTINGS FOR FILL-IN sokFraVPIdato IN FRAME frmLinje
   1                                                                    */
/* SETTINGS FOR FILL-IN sokLinjeMerknad IN FRAME frmLinje
   NO-ENABLE                                                            */
ASSIGN 
       sokSasBeskr:READ-ONLY IN FRAME frmLinje        = TRUE.

/* SETTINGS FOR FILL-IN sokTilEdato IN FRAME frmLinje
   1                                                                    */
/* SETTINGS FOR FILL-IN sokTilRegDatoBestInnl IN FRAME frmLinje
   1                                                                    */
/* SETTINGS FOR FILL-IN sokTilVPIdato IN FRAME frmLinje
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX tbRepeat IN FRAME frmLinje
   1                                                                    */
/* SETTINGS FOR FRAME frmListe
                                                                        */
ASSIGN 
       FRAME frmListe:HEIGHT           = 23.38
       FRAME frmListe:WIDTH            = 198.

/* SETTINGS FOR FRAME frmOkReg
                                                                        */
ASSIGN 
       FRAME frmOkReg:MOVABLE          = TRUE.

/* SETTINGS FOR FRAME frmOrdre
                                                                        */
/* SETTINGS FOR FILL-IN SokOrdreArtBeskr IN FRAME frmOrdre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN SokOrdreCLNamn IN FRAME frmOrdre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN SokOrdreLevNamn IN FRAME frmOrdre
   NO-ENABLE                                                            */
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

CREATE CONTROL-FRAME Grid ASSIGN
       FRAME           = FRAME frmLinje:HANDLE
       ROW             = 23.95
       COLUMN          = 1.4
       HEIGHT          = 5.05
       WIDTH           = 196.6
       HIDDEN          = no
       SENSITIVE       = yes.
/* TabStrip OCXINFO:CREATE-CONTROL from: {1EFB6596-857C-11D1-B16A-00C0F0283628} type: TabStrip */
/* Grid OCXINFO:CREATE-CONTROL from: {0F026C11-5A66-4c2b-87B5-88DDEBAE72A1} type: VSFlexGrid */
      TabStrip:MOVE-AFTER(KortNavn:HANDLE IN FRAME DEFAULT-FRAME).
      Grid:MOVE-AFTER(button-slett:HANDLE IN FRAME frmLinje).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Suppleringsordre */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Suppleringsordre */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frmOkReg
&Scoped-define SELF-NAME btnAvbryt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAvbryt C-Win
ON CHOOSE OF btnAvbryt IN FRAME frmOkReg /* Avbryt */
DO:
  APPLY "return" TO sokLevNr IN FRAME frmLinje.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frmLinje
&Scoped-define SELF-NAME btnAvdeling
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAvdeling C-Win
ON CHOOSE OF btnAvdeling IN FRAME frmLinje /* ... */
DO:
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxDSelector.w (THIS-PROCEDURE,0,
                      "Avdeling;AvdelingNr;AvdelingNavn,VarebehLinje;!VarebehNr",
                      "where true,first varebehlinje no-lock where varebehlinje.avdelingnr = avdeling.avdelingnr and varebehlinje.varebehnr = " + STRING(hFieldMap:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE),
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
    IF NOT DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"uselocaldata") = "yes" THEN
      RUN StartQuery.
  END.
  THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAvdeling2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAvdeling2 C-Win
ON CHOOSE OF btnAvdeling2 IN FRAME frmLinje /* ... */
DO:
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxDSelector.w (THIS-PROCEDURE,0,
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
    IF NOT DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"uselocaldata") = "yes" THEN
      RUN StartQuery.
  END.
  THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frmFilter
&Scoped-define SELF-NAME btnBlankFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnBlankFilter C-Win
ON CHOOSE OF btnBlankFilter IN FRAME frmFilter /* Blank filter */
DO:
  ASSIGN
      fi-fVarebehNr:SCREEN-VALUE          = ""
      fi-fProfilNr:SCREEN-VALUE           = ""
      fi-fMesseNr:SCREEN-VALUE            = ""
      fi-cVarebehBeskrivelse:SCREEN-VALUE = ""
      ListeSokLevNr:SCREEN-VALUE          = ""
      ListeSokLevNamn:SCREEN-VALUE        = ""
      cLevBasIdList                       = ""
      sokGlobOrdrenr:SCREEN-VALUE         = ""
      tbKundeOrdre:CHECKED                = NO
      .
   IF cmbVarebehType:SENSITIVE THEN cmbVarebehType:SCREEN-VALUE  = "0".

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
  DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"uselocaldata","").
  RUN StartQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frmOrdre
&Scoped-define SELF-NAME btnBlankOrdreFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnBlankOrdreFilter C-Win
ON CHOOSE OF btnBlankOrdreFilter IN FRAME frmOrdre /* Blank filter */
DO:
  ASSIGN
      sokBestOrdreStatus:SCREEN-VALUE     = " "
      sokOrdreNr:SCREEN-VALUE             = "0"
      sokEkstId:SCREEN-VALUE              = ""
      sokOrdreLevDato:SCREEN-VALUE        = STRING(?)
      sokOrdreCL:SCREEN-VALUE             = "0"
      SokOrdreCLNamn:SCREEN-VALUE         = ""
      SokOrdreLevNr:SCREEN-VALUE          = "0"
      SokOrdreLevNamn:SCREEN-VALUE        = ""
      sokOrdreArtikkel:SCREEN-VALUE       = "0"
      SokOrdreArtBeskr:SCREEN-VALUE       = ""
      sokOrdreLevKod:SCREEN-VALUE         = ""
      sokOrdreLevFargKod:SCREEN-VALUE     = ""
      sokOrdremottaker:SCREEN-VALUE       = " "
      sokOpphav:SCREEN-VALUE              = " "
      .
  
   RUN StartQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frmDetalj
&Scoped-define SELF-NAME btnButikker
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnButikker C-Win
ON CHOOSE OF btnButikker IN FRAME frmDetalj /* ... */
DO:
  RUN VelgButikker("edit",OUTPUT bOk).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frmLinje
&Scoped-define SELF-NAME btnHuvGr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnHuvGr C-Win
ON CHOOSE OF btnHuvGr IN FRAME frmLinje /* ... */
DO:

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "HuvGr;Hg;HgBeskr,Avdeling;AvdelingNr;AvdelingNavn,VarebehLinje;!VarebehNr",
                      "where true, first Avdeling OF HuvGr NO-LOCK " + 
                         (IF cAvdelingRowIdList NE "" THEN
                            "WHERE CAN-DO('" + cAvdelingRowIdList + "',STRING(ROWID(Avdeling)))"
                          ELSE "") +
                      ",first varebehlinje no-lock where varebehlinje.hg = huvgr.hg and varebehlinje.varebehnr = " + STRING(hFieldMap:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE),
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
    IF NOT DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"uselocaldata") = "yes" THEN
      RUN StartQuery.
  END.
  THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnHuvGr2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnHuvGr2 C-Win
ON CHOOSE OF btnHuvGr2 IN FRAME frmLinje /* ... */
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
   IF NOT DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"uselocaldata") = "yes" THEN
     RUN StartQuery.
  END.
  THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLev C-Win
ON CHOOSE OF btnLev IN FRAME frmLinje /* ... */
DO:
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "LevBas;levnr;levnamn;KjedeAvtale|Kjedeavtale|J/N,VarebehLinje;!VarebehNr",
                      "where true,first varebehlinje no-lock where varebehlinje.levnr = levbas.levnr and varebehlinje.varebehnr = " + STRING(hFieldMap:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE),
                      INPUT-OUTPUT cLevBasRowIdList,
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
             iCurrLevNr              = INT(sokLevNr:SCREEN-VALUE).
    RUN StartQuery.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frmFilter
&Scoped-define SELF-NAME btnLev-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLev-2 C-Win
ON CHOOSE OF btnLev-2 IN FRAME frmFilter /* ... */
DO:
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "LevBas;levnr;levnamn;KjedeAvtale|Kjedeavtale|J/N,VarebehLinje;!VarebehNr",
                      "where true,first varebehlinje no-lock where varebehlinje.levnr = levbas.levnr",
                      INPUT-OUTPUT cLevBasRowIdList,
                      "Levnr",
                      INPUT-OUTPUT cLevBasIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk THEN DO:
    IF NUM-ENTRIES(cLevBasRowidList) > 1 THEN 
      ASSIGN ListeSokLevNr:SCREEN-VALUE   = "0"
             ListeSokLevNamn:SCREEN-VALUE = STRING(NUM-ENTRIES(cLevBasRowidList)) + " av " +
                                       STRING(iSelectorSourcCount)
                                       .
    ELSE
      ASSIGN ListeSokLevNr:SCREEN-VALUE   = cLevBasIdList
             ListeSokLevNamn:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","LevBas;LevNamn","WHERE LevNr = " + ListeSokLevNr:SCREEN-VALUE).
  
    DYNAMIC-FUNCTION("setCurrentObject",hBrowseListe).
    RUN StartQuery.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frmOrdre
&Scoped-define SELF-NAME btnLev-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLev-3 C-Win
ON CHOOSE OF btnLev-3 IN FRAME frmOrdre /* ... */
DO:
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "LevBas;levnr;levnamn;KjedeAvtale|Kjedeavtale|J/N,VarebehLinje;!VarebehNr",
                      "where true,first varebehlinje no-lock where varebehlinje.levnr = levbas.levnr and varebehlinje.varebehnr = " + STRING(hFieldMap:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE),
                      INPUT-OUTPUT cLevBasOrdreRowIdList,
                      "Levnr",
                      INPUT-OUTPUT cLevBasOrdreIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk THEN DO:
    IF NUM-ENTRIES(cLevBasOrdreRowidList) > 1 THEN 
      ASSIGN sokOrdreLevNr:SCREEN-VALUE   = "0"
             sokOrdreLevNamn:SCREEN-VALUE = STRING(NUM-ENTRIES(cLevBasOrdreRowidList)) + " av " +
                                       STRING(iSelectorSourcCount)
                                       .
    ELSE
      ASSIGN sokOrdreLevNr:SCREEN-VALUE   = cLevBasOrdreIdList
             sokOrdreLevNamn:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","LevBas;LevNamn","WHERE LevNr = " + sokOrdreLevNr:SCREEN-VALUE)
             .
    RUN StartQuery.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frmLinje
&Scoped-define SELF-NAME btnLev2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLev2 C-Win
ON CHOOSE OF btnLev2 IN FRAME frmLinje /* ... */
DO:
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "LevBas;levnr;levnamn",
                      "where true",
                      INPUT-OUTPUT cLevBasRowIdList,
                      "Levnr,LevNamn",
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
      ASSIGN sokLevNr:SCREEN-VALUE   = ENTRY(1,cLevBasIdList,"|")
             sokLevNamn:SCREEN-VALUE = ENTRY(2,cLevBasIdList,"|")
             iCurrLevNr              = INT(sokLevNr:SCREEN-VALUE).
    RUN StartQuery.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLevDatoDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLevDatoDate C-Win
ON CHOOSE OF btnLevDatoDate IN FRAME frmLinje /* ... */
DO:
  RUN Cal.w (LevDato:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLinjemerknad
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLinjemerknad C-Win
ON CHOOSE OF btnLinjemerknad IN FRAME frmLinje /* ... */
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
    RUN StartQuery.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frmOkReg
&Scoped-define SELF-NAME btnOk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOk C-Win
ON BACK-TAB OF btnOk IN FRAME frmOkReg /* OK */
DO:  
  DEF VAR hOverlay AS HANDLE NO-UNDO.
  hOverlay = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"lastenabledoverlay")) NO-ERROR.
  IF VALID-HANDLE(hOverlay) THEN DO:
    APPLY "entry" TO hOverlay.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOk C-Win
ON CHOOSE OF btnOk IN FRAME frmOkReg /* OK */
DO:  
  RUN LagreNyLinje.
  IF RETURN-VALUE = "ERROR" THEN APPLY "back-tab" TO btnOk.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frmOrdre
&Scoped-define SELF-NAME btnOrdreArtikkel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOrdreArtikkel C-Win
ON CHOOSE OF btnOrdreArtikkel IN FRAME frmOrdre /* ... */
DO:
  DEF VAR cReturnValues   AS CHAR NO-UNDO.

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxLookup.w (THIS-PROCEDURE,50,
                    "ArtBas"
                    + ";ArtikkelNr"
                    + ";Beskr"
                    + ";LevKod"
                    + ";LevFargKod"
                    ,"WHERE true"
                    ,""                                                  
                    ,"ArtikkelNr,Beskr",   /* <- return values for these fields */
                      OUTPUT cReturnValues,
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk AND cReturnValues NE "" THEN DO:  
    ASSIGN sokOrdreArtikkel:SCREEN-VALUE = ENTRY(1,cReturnValues,"|")
           SokOrdreArtBeskr:SCREEN-VALUE = ENTRY(2,cReturnValues,"|").
    DYNAMIC-FUNCTION("setCurrentObject",hBrowseOrdre).
    RUN StartQuery.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOrdreLev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOrdreLev C-Win
ON CHOOSE OF btnOrdreLev IN FRAME frmOrdre /* ... */
DO:
  DEF VAR cReturnValues   AS CHAR NO-UNDO.

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxLookup.w (THIS-PROCEDURE,50,
                    "Butiker"
                    + ";Butik"
                    + ";ButNamn"
                  + ",Ordre"
                    + ";!OrdreNr"
                    ,(IF cButCl NE "" THEN "WHERE CAN-DO('" + cButCl + "',STRING(Butiker.Butik))" ELSE "WHERE true")
                       + ",FIRST Ordre WHERE Ordre.CL = Butik AND Ordre.VarebehNr = " + STRING(hFieldMap:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE)
                    ,""                                                  
                    ,"Butik,ButNamn",   /* <- return values for these fields */
                      OUTPUT cReturnValues,
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk AND cReturnValues NE "" THEN DO:  
    ASSIGN sokOrdreCL:SCREEN-VALUE = ENTRY(1,cReturnValues,"|")
           SokOrdreCLNamn:SCREEN-VALUE = ENTRY(2,cReturnValues,"|").
    DYNAMIC-FUNCTION("setCurrentObject",hBrowseOrdre).
    RUN OpenQuery.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frmLinje
&Scoped-define SELF-NAME btnSaSong
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSaSong C-Win
ON CHOOSE OF btnSaSong IN FRAME frmLinje /* ... */
DO:
  DEF VAR cSesongList AS CHAR NO-UNDO INIT "*".

  IF NOT hFieldMap:AVAIL THEN RETURN.

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  IF DYNAMIC-FUNCTION("runproc","varebehlinje_distinct_sesong.p",STRING(hFieldMap:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE),?) THEN
    cSesongList = DYNAMIC-FUNCTION("getTransactionMessage").

  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "SaSong;SasBeskr;Sasong",
                      "where CAN-DO('" + cSesongList + "',STRING(Sasong))",
                      INPUT-OUTPUT cSaSongRowIdList,
                      "SaSong",
                      INPUT-OUTPUT cSaSongIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk THEN DO:
    IF NUM-ENTRIES(cSaSongRowidList) > 1 THEN 
      ASSIGN sokSaSong:SCREEN-VALUE   = "0"
             sokSasBeskr:SCREEN-VALUE = STRING(NUM-ENTRIES(cSaSongRowidList)) + " av " +
                                        STRING(iSelectorSourcCount)
                                       .
    ELSE
      ASSIGN sokSaSong:SCREEN-VALUE   = cSaSongIdList
             sokSasBeskr:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","SaSong;sasBeskr","WHERE Sasong = " + sokSaSong:SCREEN-VALUE).
    RUN StartQuery.
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
  RUN JBoxDLookup.w ("Messe;MesseNr|Innkj.per;MesseBeskrivelse","where true",INPUT-OUTPUT cLookupValue).

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
  RUN JBoxDLookup.w ("Messe;MesseNr|Innkj.per;MesseBeskrivelse;MesseType",
                     "where true",
                     INPUT-OUTPUT cLookupValue).

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


&Scoped-define FRAME-NAME frmLinje
&Scoped-define SELF-NAME btnSplitBarY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSplitBarY C-Win
ON END-MOVE OF btnSplitBarY IN FRAME frmLinje /* Button 1 */
DO:
/*   DYNAMIC-FUNCTION("setNoResizeY",  THIS-PROCEDURE:CURRENT-WINDOW, FRAME frmLinje:HANDLE, "IMAGE-Sko,brwLinje,rectBrowseLinje,rectBestHodeToolbar").  */
  DYNAMIC-FUNCTION("setSplitBarY" ,THIS-PROCEDURE:CURRENT-WINDOW, btnSplitBarY:HANDLE IN FRAME frmLinje,NO).
  IF DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"newrow") = "yes" AND
     DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"uselocaldata") = "yes" THEN 
    ViewFrameOkReg().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnTotal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnTotal C-Win
ON CHOOSE OF btnTotal IN FRAME frmLinje /* Oppdater total: */
DO:  
  DEF VAR hWidget AS HANDLE NO-UNDO.
  
  bUpdateStat = YES.

  DO ix = 1 TO NUM-ENTRIES(cStatFieldHandles):
    hWidget = WIDGET-HANDLE(ENTRY(ix,cStatFieldHandles)).
    ASSIGN hWidget:HIDDEN = NO
           hWidget:SCREEN-VALUE = "".
  END.
  RUN StartQuery.
  bUpdateStat = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frmDetalj
&Scoped-define SELF-NAME btnVareBokNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnVareBokNr C-Win
ON CHOOSE OF btnVareBokNr IN FRAME frmDetalj /* ... */
DO:
  DEF VAR cReturnValues   AS CHAR NO-UNDO.
  DEF VAR bOk             AS LOG  NO-UNDO.

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxLookup.w (THIS-PROCEDURE,200,
                    "VareBokHode"
                    + ";VareBokNr"
                    + ";VareBokBeskrivelse"
                    + ";MesseNr"
                   ,"WHERE "
                  + (IF MesseNr:SCREEN-VALUE NE "0" THEN "MesseNr = " + MesseNr:SCREEN-VALUE
                     ELSE "true")
                    ,""                                                  
                    ,"VareBokNr,VarebokBeskrivelse,Messenr",   /* <- return values for these fields */
                    OUTPUT cReturnValues,
                    OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk AND cReturnValues NE "" THEN DO:
    ASSIGN Kilde:SCREEN-VALUE = ENTRY(1,cReturnValues,"|")
           VareBokBeskrivelse:SCREEN-VALUE = ENTRY(2,cReturnValues,"|")
           Messenr:SCREEN-VALUE = ENTRY(3,cReturnValues,"|")
           MesseBeskrivelse:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldValues","Messe",
                                                            "WHERE Messenr = " + Messenr:SCREEN-VALUE,"MesseBeskrivelse")
           .

    APPLY "any-printable" TO Kilde.
  END.
  THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frmLinje
&Scoped-define SELF-NAME btnVarGr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnVarGr C-Win
ON CHOOSE OF btnVarGr IN FRAME frmLinje /* ... */
DO:
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "VarGr;Vg;VgBeskr,HuvGr;Hg;HgBeskr,VarebehLinje;!VarebehNr",
                      "where true, first HuvGr OF VarGr NO-LOCK " + 
                         (IF cHuvGrRowIdList NE "" THEN
                            "WHERE CAN-DO('" + cHuvGrRowIdList + "',STRING(ROWID(HuvGr)))"
                          ELSE "") + 
                      ",first varebehlinje no-lock where varebehlinje.vg = vargr.vg and varebehlinje.varebehnr = " + STRING(hFieldMap:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE),
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
    IF NOT DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"uselocaldata") = "yes" THEN
      RUN StartQuery.
  END.
  THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
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
    RUN StartQuery.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnVarGr2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnVarGr2 C-Win
ON CHOOSE OF btnVarGr2 IN FRAME frmLinje /* ... */
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
    IF NOT DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"uselocaldata") = "yes" THEN
      RUN StartQuery.
  END.
  THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Paste
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Paste C-Win
ON CHOOSE OF BUTTON-Paste IN FRAME frmLinje
DO:
  RUN BildeImport.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Paste C-Win
ON DROP-FILE-NOTIFY OF BUTTON-Paste IN FRAME frmLinje
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
      MESSAGE "Maks 1 fil tillatt!"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME button-slett
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL button-slett C-Win
ON CHOOSE OF button-slett IN FRAME frmLinje /* Slett rad */
DO:  
  DEF VAR iCount AS INTE NO-UNDO.
  chGrid:TextMatrix(chGrid:ROW,1) = "0 ".
  DO iCount = 2 TO chGrid:Cols - 1:
      chGrid:TextMatrix(chGrid:ROW,iCount) = " ".
  END.
  ASSIGN chGrid:COL = chGrid:FixedCols.
  APPLY "any-printable" TO LevDato IN FRAME frmLinje.
  APPLY "ENTRY" TO Grid.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frmOrdre
&Scoped-define SELF-NAME cmbGjennomfaktureres
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbGjennomfaktureres C-Win
ON VALUE-CHANGED OF cmbGjennomfaktureres IN FRAME frmOrdre /* Gjennomfakturering */
DO:
  RUN StartQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmbKjedevare
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbKjedevare C-Win
ON VALUE-CHANGED OF cmbKjedevare IN FRAME frmOrdre /* Kjedelevert */
DO:
  RUN StartQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frmFilter
&Scoped-define SELF-NAME cmbVarebehType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbVarebehType C-Win
ON VALUE-CHANGED OF cmbVarebehType IN FRAME frmFilter /* Type */
DO:
  RUN StartQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cEkstRef
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cEkstRef C-Win
ON LEAVE OF fi-cEkstRef IN FRAME frmFilter /* Ekst.referanse */
OR "TAB":U OF fi-cEkstRef
DO:
  IF fi-cEkstRef:MODIFIED THEN RUN StartQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cVarebehBeskrivelse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cVarebehBeskrivelse C-Win
ON RETURN OF fi-cVarebehBeskrivelse IN FRAME frmFilter /* Beskrivelse */
OR "TAB":U OF fi-cVarebehBeskrivelse
DO:
  IF fi-cVarebehBeskrivelse:MODIFIED THEN RUN StartQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-fMesseNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-fMesseNr C-Win
ON RETURN OF fi-fMesseNr IN FRAME frmFilter /* Innkj.per */
OR "TAB":U OF fi-fMesseNr
DO:
  IF fi-fMesseNr:MODIFIED THEN RUN StartQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-fProfilNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-fProfilNr C-Win
ON RETURN OF fi-fProfilNr IN FRAME frmFilter /* Prisprofil */
OR "TAB":U OF fi-fProfilNr
DO:
  IF fi-fProfilNr:MODIFIED THEN RUN StartQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-fVarebehNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-fVarebehNr C-Win
ON RETURN OF fi-fVarebehNr IN FRAME frmFilter /* Vareh.nr */
OR TAB OF fi-fVarebehNr DO:
  IF fi-fVarebehNr:MODIFIED THEN RUN StartQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frmLinje
&Scoped-define SELF-NAME Grid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Grid C-Win OCX.KeyPress
PROCEDURE Grid.VSFlexGrid.KeyPress .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  Required for OCX.
    KeyAscii
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT-OUTPUT PARAMETER p-KeyAscii AS INTEGER NO-UNDO.
DEFINE VARIABLE               wCentralValue AS INTE    NO-UNDO.
DEFINE VARIABLE               wCellValue    AS INTE    NO-UNDO.
DEFINE VARIABLE               wDiff         AS INTE    NO-UNDO.
/* ASSIGN FI-Liminn:HIDDEN IN FRAME {&FRAME-NAME} = TRUE. */

IF INT(chGrid:TextMatrix(chGrid:ROW,0)) = 0 THEN RETURN NO-APPLY.

IF hFieldMap:BUFFER-FIELD("VarebehType"):BUFFER-VALUE = iBestVarebeh
   OR hFieldMapBestHode:BUFFER-FIELD("BestNr"):BUFFER-VALUE = 0 THEN
  APPLY "any-printable" TO LevDato IN FRAME frmLinje.

IF p-KeyAscii = 13 THEN DO:

    IF  chGrid:Col = chGrid:Cols - 1 AND 
        chGrid:ROW = chGrid:Rows - 1 THEN DO:
      APPLY "ENTRY" TO hLagerBestButton.
    END.
    ELSE IF chGrid:Col = chGrid:Cols - 1 THEN DO:
        chGrid:SELECT(chGrid:ROW + 1,2).
        wdiff = chGrid:CellLeft.
    END.
    ELSE DO:
      ASSIGN chGrid:Col = chGrid:Col + 1.
      wdiff = chGrid:CellLeft.
    END.
    RETURN NO-APPLY.
END.
IF p-KeyAscii = 13 AND chGrid:Col = chGrid:Cols - 1 THEN DO:
    APPLY "ENTRY" TO hLagerBestButton.
    RETURN NO-APPLY.
END.
IF p-KeyAscii = 8 OR (p-KeyAscii >= 43 AND p-KeyAscii <= 45) OR
                     (p-KeyAscii >= 48 AND p-KeyAscii <= 57)     THEN DO:
    IF p-KeyAscii = 8 OR p-KeyAscii = 44 THEN DO:
        IF INT(chGrid:Text) = 0 THEN
            RETURN NO-APPLY.
        ASSIGN wDiff = INT(chGrid:Text)
               chGrid:Text = 
             SUBSTR(chGrid:Text,1,LENGTH(chGrid:Text) - 2) + " ".
        ASSIGN wDiff = INT(chGrid:Text) - wDiff.
    END.
    ELSE IF p-KeyAscii = 43 THEN DO: /* + */
        IF INT(chGrid:Text) + 1 > 9999 THEN
            RETURN NO-APPLY.
        ASSIGN chGrid:Text = STRING(INT(chGrid:Text) + 1) + " "
               wDiff = 1.
    END.
    ELSE IF p-KeyAscii = 45 THEN DO: /* - */
/*         IF INT(chGrid:Text) = 0 THEN  */
/*             RETURN NO-APPLY.          */
        ASSIGN chGrid:Text =
            IF INT(chGrid:Text) - 1 = 0 THEN
                "" 
            ELSE            
                STRING(INT(chGrid:Text) - 1) + " ".
        ASSIGN wDiff = -1.
    END.
    ELSE IF p-KeyAscii > 48 OR (chGrid:Text > "" AND p-KeyAscii = 48) THEN DO:
        IF INT(chGrid:Text) * 10 + p-KeyAscii - 48 > 9999 THEN
            chGrid:Text.
        ELSE DO:
            ASSIGN wDiff = (INT(chGrid:Text) * 10 + p-KeyAscii - 48) - INT(chGrid:Text)
                   chGrid:Text = STRING(INT(chGrid:Text) * 10 + p-KeyAscii - 48) + " ".
        END.
    END.
/*     RUN ChangeFri IN hWindow (chGrid:Col - chGrid:FixedCols + 1,wDiff). */
    ASSIGN chGrid:TextMatrix(chGrid:ROW,1) = STRING(INT(chGrid:TextMatrix(chGrid:ROW,1)) + wDiff) + " "
           BUTTON-Slett:SENSITIVE IN FRAME {&FRAME-NAME} = INT(chGrid:TextMatrix(chGrid:ROW,1)) > 0.
END.
ELSE
    RETURN NO-APPLY.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Grid C-Win OCX.KeyUp
PROCEDURE Grid.VSFlexGrid.KeyUp .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  Required for OCX.
    KeyCode
    Shift
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT-OUTPUT PARAMETER p-KeyCode AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER p-Shift   AS INTEGER NO-UNDO.
    
IF p-KeyCode = 76 AND p-Shift = 4 THEN
  APPLY "CHOOSE" TO hLagerBestButton.
ELSE IF p-KeyCode = 84 AND p-Shift = 4 THEN
  APPLY "CHOOSE" TO hStrekkodeButton.
ELSE IF p-KeyCode = 89 AND p-Shift = 4 THEN
  APPLY "CHOOSE" TO hKalkyleButton.

RETURN NO-APPLY.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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

  IF NOT hFieldMap:AVAIL THEN 
    hBrowseListe:SELECT-ROW(hBrowseListe:FOCUSED-ROW).

  IF Kode:SCREEN-VALUE NE "" THEN 
  DO:
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

    ClearFilter().

    DO ix = 1 TO NUM-ENTRIES(cArtNrList):
      cQueryWhere = cQueryWhere + ENTRY(ix,cArtNrList) + (IF ix < NUM-ENTRIES(cArtNrList) THEN " OR ArtikkelNr = " ELSE ")").
    END.
    DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"querywhere",cQueryWhere).
  END.
  ELSE ClearFilter().

  DYNAMIC-FUNCTION("setCurrentObject",hBrowseLinje).
  RUN StartQuery.

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

        IF NOT tbOpprettBest:HIDDEN AND tbOpprettBest:CHECKED THEN DO:
          cBestNr = DYNAMIC-FUNCTION("getFieldList","BestHode;BestNr,BestStr;",
                                     "WHERE Artikkelnr = " + STRING(hFieldMapLinje:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) + " AND BestStat = 3"
                                   + ",FIRST BestStr OF BestHode WHERE BestStr.Butik = " + cButikkListe).
          IF cBestNr NE "" THEN DO:
            IF NUM-ENTRIES(cBestNr,"|") > 1 THEN DO:        
              DO ix = 1 TO NUM-ENTRIES(cBestNr,"|"):
                IF INT(ENTRY(ix,cBestNr,"|")) > iMaxBestNr THEN
                  iMaxBestNr = INT(ENTRY(ix,cBestNr,"|")).
              END.
              cBestNr = STRING(iMaxBestNr).
            END.
            bOk = hFieldMapBestHode:FIND-FIRST("WHERE BestNr = " + cBestNr) NO-ERROR.
            IF bOk THEN DO:
               hBrowseBestHode:QUERY:REPOSITION-TO-ROWID(hFieldMapBestHode:ROWID) NO-ERROR.
               IF NOT ERROR-STATUS:ERROR THEN DO:
                 APPLY "value-changed" TO hBrowseBestHode.  
                 chGrid:TextMatrix(1,LOOKUP(cStorl,cAlfaFord) + 1) = STRING(INT(chGrid:TextMatrix(1,LOOKUP(cStorl,cAlfaFord) + 1)) + 1) + " ".
                 DYNAMIC-FUNCTION("setCurrentObject",hBestHodeToolbar).
                 RUN SaveRecord.
                 APPLY "value-changed" TO hBrowseLinje.
                 APPLY "entry" TO Kode.
                 RETURN NO-APPLY.
               END.
            END.
          END.
          ELSE DO:              
            DYNAMIC-FUNCTION("setCurrentObject",hBestHodeToolbar).
            RUN NewRecord.
            IF DATE(LevDato:SCREEN-VALUE) NE ? THEN DO:
              chGrid:TextMatrix(1,LOOKUP(cStorl,cAlfaFord) + 1) = STRING(INT(chGrid:TextMatrix(1,LOOKUP(cStorl,cAlfaFord) + 1)) + 1) + " ".
              DYNAMIC-FUNCTION("setCurrentObject",hBestHodeToolbar).
              RUN SaveRecord.
              APPLY "value-changed" TO hBrowseLinje.
              APPLY "entry" TO Kode.
              RETURN NO-APPLY.
            END.
            ELSE DO: 
              APPLY "entry" TO LevDato.
              RETURN NO-APPLY.
            END.
          END.
        END.
      END.
      RETURN NO-APPLY.
    END.
  END.
  ELSE IF NOT tbOpprettBest:HIDDEN AND tbOpprettBest:CHECKED THEN DO:
    cBestNr = DYNAMIC-FUNCTION("getFieldList","BestHode;BestNr,BestStr;",
                               "WHERE Artikkelnr = " + STRING(hFieldMapLinje:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) + " AND BestStat = 3"
                             + ",FIRST BestStr OF BestHode WHERE BestStr.Butik = " + cButikkListe).
    IF cBestNr NE "" THEN DO:
      IF NUM-ENTRIES(cBestNr,"|") > 1 THEN DO:        
        DO ix = 1 TO NUM-ENTRIES(cBestNr,"|"):
          IF INT(ENTRY(ix,cBestNr,"|")) > iMaxBestNr THEN
            iMaxBestNr = INT(ENTRY(ix,cBestNr,"|")).
        END.
        cBestNr = STRING(iMaxBestNr).
      END.
      bOk = hFieldMapBestHode:FIND-FIRST("WHERE BestNr = " + cBestNr) NO-ERROR.
      IF bOk THEN DO:
         hBrowseBestHode:QUERY:REPOSITION-TO-ROWID(hFieldMapBestHode:ROWID) NO-ERROR.
         IF NOT ERROR-STATUS:ERROR THEN DO:
           APPLY "value-changed" TO hBrowseBestHode.  
           chGrid:TextMatrix(1,LOOKUP(cStorl,cAlfaFord) + 1) = STRING(INT(chGrid:TextMatrix(1,LOOKUP(cStorl,cAlfaFord) + 1)) + 1) + " ".
           DYNAMIC-FUNCTION("setCurrentObject",hBestHodeToolbar).
           RUN SaveRecord.
           APPLY "value-changed" TO hBrowseLinje.
           APPLY "entry" TO Kode.
           RETURN NO-APPLY.
         END.
      END.
    END.
    ELSE DO:        
      DYNAMIC-FUNCTION("setCurrentObject",hBestHodeToolbar).
      RUN NewRecord.
      IF DATE(LevDato:SCREEN-VALUE) NE ? THEN DO:
        chGrid:TextMatrix(1,LOOKUP(cStorl,cAlfaFord) + 1) = STRING(INT(chGrid:TextMatrix(1,LOOKUP(cStorl,cAlfaFord) + 1)) + 1) + " ".
        DYNAMIC-FUNCTION("setCurrentObject",hBestHodeToolbar).
        RUN SaveRecord.
        APPLY "value-changed" TO hBrowseLinje.
        APPLY "entry" TO Kode.
        RETURN NO-APPLY.
      END.
      ELSE DO: 
        APPLY "entry" TO LevDato.
        RETURN NO-APPLY.
      END.
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frmFilter
&Scoped-define SELF-NAME ListeSokLevNamn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ListeSokLevNamn C-Win
ON RETURN OF ListeSokLevNamn IN FRAME frmFilter
OR TAB OF sokLevNamn DO:
  IF sokLevNamn:MODIFIED THEN DO: 
    ASSIGN cLevBasRowIdList = ""
           cLevBasIdList    = ""
           .
    RUN StartQuery.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ListeSokLevNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ListeSokLevNr C-Win
ON RETURN OF ListeSokLevNr IN FRAME frmFilter /* Leverandør */
OR TAB OF ListeSokLevnr DO:
  IF ListeSokLevnr:MODIFIED THEN DO:
    ListeSokLevNamn:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","LevBas;LevNamn","WHERE LevNr = " + ListeSokLevnr:SCREEN-VALUE).
    IF ListeSokLevnr:SCREEN-VALUE NE "0" THEN
      cLevBasIdList = ListeSokLevnr:SCREEN-VALUE.
    RUN StartQuery.
    RETURN NO-APPLY.
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


&Scoped-define FRAME-NAME frmDetalj
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


&Scoped-define FRAME-NAME frmLinje
&Scoped-define SELF-NAME rsLagerStatusSalg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rsLagerStatusSalg C-Win
ON VALUE-CHANGED OF rsLagerStatusSalg IN FRAME frmLinje
DO:
  VisLagerStatus().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokAktBeskrivelse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokAktBeskrivelse C-Win
ON RETURN OF sokAktBeskrivelse IN FRAME frmLinje
OR TAB OF sokAktBeskrivelse DO:
  IF sokAktBeskrivelse:MODIFIED THEN DO:
    RUN OpenQuery.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokAktNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokAktNr C-Win
ON RETURN OF sokAktNr IN FRAME frmLinje /* Aktivitet */
OR TAB OF sokAktNr DO:
  IF sokAktNr:MODIFIED THEN DO: 
    sokAktNr:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","Aktivitet;Beskrivelse","WHERE AktNr = " + sokAktNr:SCREEN-VALUE).
    RUN StartQuery.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokArtikkelNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokArtikkelNr C-Win
ON RETURN OF sokArtikkelNr IN FRAME frmLinje /* Artnr/Utv.s */
OR TAB OF sokArtikkelNr DO:
  IF sokArtikkelNr:MODIFIED THEN DO:
    RUN StartQuery.
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
    RUN StartQuery.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokAvdelingNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokAvdelingNr C-Win
ON F3 OF sokAvdelingNr IN FRAME frmLinje /* Avdeling */
DO:
  APPLY "choose" TO btnAvdeling2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokAvdelingNr C-Win
ON RETURN OF sokAvdelingNr IN FRAME frmLinje /* Avdeling */
OR TAB OF sokAvdelingNr DO:
  IF sokAvdelingNr:MODIFIED  THEN DO:
    sokAvdelingNavn:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","Avdeling;AvdelingNavn","WHERE AvdelingNr = " + sokAvdelingNr:SCREEN-VALUE).
    IF DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"uselocaldata") NE "yes" THEN
      RUN StartQuery.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokBeskr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokBeskr C-Win
ON RETURN OF sokBeskr IN FRAME frmLinje
OR TAB OF sokBeskr DO:
  IF sokBeskr:MODIFIED THEN DO:
    RUN StartQuery.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frmOrdre
&Scoped-define SELF-NAME sokBestOrdreStatus
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokBestOrdreStatus C-Win
ON VALUE-CHANGED OF sokBestOrdreStatus IN FRAME frmOrdre /* Best.status */
DO:
  RUN StartQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frmFilter
&Scoped-define SELF-NAME sokEkstId
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokEkstId C-Win
ON RETURN OF sokEkstId IN FRAME frmFilter /* Ekstern id */
OR "TAB":U OF sokEkstId
DO:
  DEF VAR rRepos    AS ROWID NO-UNDO.
  DEF VAR cOrdreNr  AS CHAR  NO-UNDO.

  IF sokEkstId:MODIFIED THEN DO:

    cOrdreNr = DYNAMIC-FUNCTION("getFieldValues","BestHode","WHERE EkstId = '" + sokEkstId:SCREEN-VALUE + "'","Ordrenr").

    IF cOrdreNr = ? THEN DO:
      DYNAMIC-FUNCTION("DoMessage",0,0,"Finner ingen bestilling med denne eksterne id","","").
      RETURN NO-APPLY.
    END.

    DYNAMIC-FUNCTION("setAttribute",hBrowseListe,"prescanqueryfilter",
                     "BestHode WHERE Ordrenr = " + cOrdrenr
                   + ",EACH VarebehBestHode OF BestHode NO-LOCK,FIRST VarebehHode OF VarebehBestHode NO-LOCK").
    RUN OpenQuery.
    DYNAMIC-FUNCTION("setAttribute",hBrowseListe,"prescanqueryfilter","").
    IF hFieldMap:AVAIL THEN DO:
      TabStripChanged(14).
      bOK = hBrowseOrdre:QUERY:GET-BUFFER-HANDLE(1):FIND-FIRST("WHERE OrdreNr = " + cOrdrenr) NO-ERROR.
      IF bOk THEN DO:
        rRepos = hBrowseOrdre:QUERY:GET-BUFFER-HANDLE(1):ROWID.
        hBrowseOrdre:QUERY:REPOSITION-TO-ROWID(rRepos) NO-ERROR.
        IF NOT ERROR-STATUS:ERROR THEN DO:
          hBrowseOrdre:SELECT-ROW(hBrowseOrdre:FOCUSED-ROW).
          DYNAMIC-FUNCTION("setCurrentObject",hBrowseOrdre).
          RUN DisplayRecord.
        END.
      END.
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frmOrdre
&Scoped-define SELF-NAME sokEkstId
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokEkstId C-Win
ON LEAVE OF sokEkstId IN FRAME frmOrdre /* Ekst.id */
DO:
  IF SELF:MODIFIED THEN RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frmLinje
&Scoped-define SELF-NAME sokFraEdato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokFraEdato C-Win
ON RETURN OF sokFraEdato IN FRAME frmLinje /* Endret dato */
OR TAB OF sokFraEdato DO:
  IF SELF:MODIFIED THEN DO: 
    RUN StartQuery.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokFraRegDatoBestInnl
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokFraRegDatoBestInnl C-Win
ON RETURN OF sokFraRegDatoBestInnl IN FRAME frmLinje /* Reg.dato */
OR TAB OF sokFraRegDatoBestInnl DO:
  IF SELF:MODIFIED THEN DO: 
    RUN StartQuery.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokFraVPIdato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokFraVPIdato C-Win
ON RETURN OF sokFraVPIdato IN FRAME frmLinje /* VPI dato */
OR TAB OF sokFraVPIdato DO:
  IF SELF:MODIFIED THEN DO:
    RUN StartQuery.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frmFilter
&Scoped-define SELF-NAME sokGlobOrdrenr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokGlobOrdrenr C-Win
ON RETURN OF sokGlobOrdrenr IN FRAME frmFilter /* Ordrenr */
OR "TAB":U OF sokGlobOrdrenr
DO:
  DEF VAR rRepos AS ROWID NO-UNDO.
  IF sokGlobOrdrenr:MODIFIED THEN DO:
    DYNAMIC-FUNCTION("setAttribute",hBrowseListe,"prescanqueryfilter",
                     "BestHode WHERE Ordrenr = " + sokGlobOrdrenr:SCREEN-VALUE
                   + ",EACH VarebehBestHode OF BestHode NO-LOCK,FIRST VarebehHode OF VarebehBestHode NO-LOCK").
    RUN StartQuery.
    DYNAMIC-FUNCTION("setAttribute",hBrowseListe,"prescanqueryfilter","").
    IF hFieldMap:AVAIL THEN DO:
      TabStripChanged(14).
      bOK = hBrowseOrdre:QUERY:GET-BUFFER-HANDLE(1):FIND-FIRST("WHERE OrdreNr = " + sokGlobOrdrenr:SCREEN-VALUE) NO-ERROR.
      IF bOk THEN DO:
        rRepos = hBrowseOrdre:QUERY:GET-BUFFER-HANDLE(1):ROWID.
        hBrowseOrdre:QUERY:REPOSITION-TO-ROWID(rRepos) NO-ERROR.
        IF NOT ERROR-STATUS:ERROR THEN DO:
          hBrowseOrdre:SELECT-ROW(hBrowseOrdre:FOCUSED-ROW).
          DYNAMIC-FUNCTION("setCurrentObject",hBrowseOrdre).
          RUN DisplayRecord.
        END.
      END.
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frmLinje
&Scoped-define SELF-NAME sokHg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokHg C-Win
ON F3 OF sokHg IN FRAME frmLinje /* Hovedgr */
DO:
  APPLY "choose" TO btnHuvGr2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokHg C-Win
ON RETURN OF sokHg IN FRAME frmLinje /* Hovedgr */
OR TAB OF sokHg DO:
  IF sokHg:MODIFIED THEN DO: 
    sokHgBeskr:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","HuvGr;HgBeskr","WHERE Hg = " + sokHg:SCREEN-VALUE).
    IF DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"uselocaldata") NE "yes" THEN
      RUN StartQuery.
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
    RUN StartQuery.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokLevKod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokLevKod C-Win
ON RETURN OF sokLevKod IN FRAME frmLinje /* Lev.art.nr */
OR TAB OF sokLevKod DO:
  IF sokLevKod:MODIFIED THEN DO:
    RUN StartQuery.
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
    RUN StartQuery.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokLevNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokLevNr C-Win
ON F3 OF sokLevNr IN FRAME frmLinje /* Lev */
DO:
  APPLY "choose" TO btnLev2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokLevNr C-Win
ON RETURN OF sokLevNr IN FRAME frmLinje /* Lev */
OR TAB OF sokLevNr DO:
  IF sokLevNr:MODIFIED THEN DO:
    sokLevNamn:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","LevBas;LevNamn","WHERE LevNr = " + sokLevNr:SCREEN-VALUE).
    iCurrLevNr = INT(sokLevNr:SCREEN-VALUE).
    IF sokLevNr:SCREEN-VALUE NE "0" THEN
      dSisteLevDato = getLevDato(iCurrLevNr).
    RUN StartQuery.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokLinjeMerknad
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokLinjeMerknad C-Win
ON RETURN OF sokLinjeMerknad IN FRAME frmLinje /* Merknad */
OR TAB OF sokLinjeMerknad DO:
  IF sokLinjeMerknad:MODIFIED THEN RUN StartQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frmOrdre
&Scoped-define SELF-NAME sokOpphav
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokOpphav C-Win
ON VALUE-CHANGED OF sokOpphav IN FRAME frmOrdre /* Opphav */
DO:
  RUN StartQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SokOrdreArtBeskr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SokOrdreArtBeskr C-Win
ON RETURN OF SokOrdreArtBeskr IN FRAME frmOrdre
OR TAB OF sokLevNamn DO:
  IF sokLevNamn:MODIFIED THEN DO: 
    ASSIGN cLevBasRowIdList = ""
           cLevBasIdList    = ""
           .
    RUN StartQuery.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokOrdreArtikkel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokOrdreArtikkel C-Win
ON RETURN OF sokOrdreArtikkel IN FRAME frmOrdre /* Art.nr */
DO:
  IF sokOrdreArtikkel:MODIFIED THEN DO:
    SokOrdreArtBeskr:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","ArtBas;Beskr","WHERE ArtikkelNr = " + sokOrdreArtikkel:SCREEN-VALUE).
    RUN StartQuery.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokOrdreArtikkel C-Win
ON TAB OF sokOrdreArtikkel IN FRAME frmOrdre /* Art.nr */
DO:
  IF sokOrdreArtikkel:MODIFIED THEN 
    SokOrdreArtBeskr:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","ArtBas;Beskr","WHERE ArtikkelNr = " + sokOrdreArtikkel:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokOrdreCL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokOrdreCL C-Win
ON RETURN OF sokOrdreCL IN FRAME frmOrdre /* CL */
DO:
  IF sokOrdreCL:MODIFIED THEN DO:
    SokOrdreCLNamn:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","Butiker;ButNamn","WHERE Butik = " + sokOrdreCL:SCREEN-VALUE).
    RUN StartQuery.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokOrdreCL C-Win
ON TAB OF sokOrdreCL IN FRAME frmOrdre /* CL */
DO:
  IF sokOrdreCL:MODIFIED THEN 
    SokOrdreCLNamn:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","Butiker;ButNamn","WHERE Butik = " + sokOrdreCL:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SokOrdreCLNamn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SokOrdreCLNamn C-Win
ON RETURN OF SokOrdreCLNamn IN FRAME frmOrdre
OR TAB OF sokLevNamn DO:
  IF sokLevNamn:MODIFIED THEN DO: 
    ASSIGN cLevBasRowIdList = ""
           cLevBasIdList    = ""
           .
    RUN StartQuery.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokOrdreLevDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokOrdreLevDato C-Win
ON RETURN OF sokOrdreLevDato IN FRAME frmOrdre /* Lev.dato */
DO:
  IF SELF:MODIFIED THEN RUN StartQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokOrdreLevFargKod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokOrdreLevFargKod C-Win
ON RETURN OF sokOrdreLevFargKod IN FRAME frmOrdre /* Lev.fargek */
DO:
  IF sokOrdreLevFargKod:MODIFIED THEN 
    RUN StartQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokOrdreLevKod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokOrdreLevKod C-Win
ON RETURN OF sokOrdreLevKod IN FRAME frmOrdre /* Lev.artnr */
DO:
  IF sokOrdreLevKod:MODIFIED THEN 
    RUN StartQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SokOrdreLevNamn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SokOrdreLevNamn C-Win
ON RETURN OF SokOrdreLevNamn IN FRAME frmOrdre
OR TAB OF sokLevNamn DO:
  IF sokLevNamn:MODIFIED THEN DO: 
    ASSIGN cLevBasRowIdList = ""
           cLevBasIdList    = ""
           .
    RUN StartQuery.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SokOrdreLevNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SokOrdreLevNr C-Win
ON RETURN OF SokOrdreLevNr IN FRAME frmOrdre /* Levnr */
DO:
  IF sokOrdreLevnr:MODIFIED THEN DO:
    SokOrdreLevNamn:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","LevBas;LevNamn","WHERE LevNr = " + sokOrdreLevnr:SCREEN-VALUE).
    RUN StartQuery.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SokOrdreLevNr C-Win
ON TAB OF SokOrdreLevNr IN FRAME frmOrdre /* Levnr */
DO:
  IF sokOrdreLevnr:MODIFIED THEN 
    SokOrdreLevNamn:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","LevBas;LevNamn","WHERE LevNr = " + sokOrdreLevnr:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokOrdremottaker
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokOrdremottaker C-Win
ON VALUE-CHANGED OF sokOrdremottaker IN FRAME frmOrdre /* Ordremottak */
DO:
  RUN StartQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokOrdreNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokOrdreNr C-Win
ON RETURN OF sokOrdreNr IN FRAME frmOrdre /* Ordrenr */
DO:
  IF SELF:MODIFIED THEN RUN StartQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frmLinje
&Scoped-define SELF-NAME sokRegBestStat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokRegBestStat C-Win
ON VALUE-CHANGED OF sokRegBestStat IN FRAME frmLinje /* Best.stat */
DO:
  RUN StartQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokSasBeskr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokSasBeskr C-Win
ON RETURN OF sokSasBeskr IN FRAME frmLinje
OR TAB OF sokSasBeskr DO:
  IF sokSasBeskr:MODIFIED THEN DO: 
    ASSIGN cSaSongRowIdList = ""
           cSaSongIdList    = ""
           .
    RUN StartQuery.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokSaSong
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokSaSong C-Win
ON RETURN OF sokSaSong IN FRAME frmLinje /* Ses */
OR TAB OF sokSaSong DO:
  IF sokSaSong:MODIFIED THEN DO:
    sokSasBeskr:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","SaSong;SasBeskr","WHERE Sasong = " + sokSaSong:SCREEN-VALUE).
    RUN StartQuery.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokTilEdato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokTilEdato C-Win
ON RETURN OF sokTilEdato IN FRAME frmLinje
OR TAB OF sokTilEdato DO:
  IF SELF:MODIFIED THEN DO:
    RUN StartQuery.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokTilRegDatoBestInnl
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokTilRegDatoBestInnl C-Win
ON RETURN OF sokTilRegDatoBestInnl IN FRAME frmLinje
OR TAB OF sokTilRegDatoBestInnl DO:
  IF SELF:MODIFIED THEN DO:
    RUN StartQuery.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokTilVPIdato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokTilVPIdato C-Win
ON RETURN OF sokTilVPIdato IN FRAME frmLinje
OR TAB OF sokTilVPIdato DO:
  IF SELF:MODIFIED THEN DO:
    RUN StartQuery.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokVg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokVg C-Win
ON F3 OF sokVg IN FRAME frmLinje /* Varegr */
DO:
  APPLY "choose" TO btnVarGr2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokVg C-Win
ON RETURN OF sokVg IN FRAME frmLinje /* Varegr */
OR TAB OF sokVg DO:
  IF sokVg:MODIFIED THEN DO: 
    sokVgBeskr:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","VarGr;VgBeskr","WHERE Vg = " + sokVg:SCREEN-VALUE).
    IF DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"uselocaldata") NE "yes" THEN
      RUN StartQuery.
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
    RUN StartQuery.
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

  IF VALID-HANDLE(hArtikkelkort) THEN APPLY "close" TO hArtikkelkort.

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


&Scoped-define FRAME-NAME frmFilter
&Scoped-define SELF-NAME tbKundeOrdre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbKundeOrdre C-Win
ON VALUE-CHANGED OF tbKundeOrdre IN FRAME frmFilter /* Kun suppl.bøker for kundeordre */
DO:
  RUN StartQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbMatchBestStat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbMatchBestStat C-Win
ON VALUE-CHANGED OF tbMatchBestStat IN FRAME frmFilter /* Bestillingen skal være i status */
DO:
  sokBestStat:SENSITIVE = tbMatchBestStat:CHECKED.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frmLinje
&Scoped-define SELF-NAME tbUpdateStat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbUpdateStat C-Win
ON VALUE-CHANGED OF tbUpdateStat IN FRAME frmLinje /* Vis total varekost og utpris */
DO:
  DEF VAR hWidget AS HANDLE NO-UNDO.
  ASSIGN tbUpdateStat.

  DO ix = 1 TO NUM-ENTRIES(cStatFieldHandles):
    hWidget = WIDGET-HANDLE(ENTRY(ix,cStatFieldHandles)).
    ASSIGN hWidget:HIDDEN = IF tbUpdateStat THEN FALSE ELSE TRUE
           hWidget:SCREEN-VALUE = "".
  END.
  IF tbUpdateStat THEN RUN StartQuery.
  ELSE APPLY "window-resized" TO {&WINDOW-NAME}.
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
      BeskrivelseVareBehType:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldValues","VarebehType",
                                              "WHERE VarebehType = '" + VarebehType:SCREEN-VALUE + "'","BeskrivelseVareBehType").  
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
DYNAMIC-FUNCTION("EmbedMe" IN SOURCE-PROCEDURE,THIS-PROCEDURE) NO-ERROR.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
  IF VALID-HANDLE(hEtikettVindu) THEN DO:
    MESSAGE "Vil du avslutte uten å skrive ut etikettene?"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK-CANCEL UPDATE bOk.
    IF NOT bOK THEN RETURN NO-APPLY "cancel".
    ELSE APPLY "close" TO hEtikettVindu.
  END.
  IF NOT DYNAMIC-FUNCTION("DoCleanUpObjects",THIS-PROCEDURE:CURRENT-WINDOW) THEN
      RETURN NO-APPLY "cancel".
  DYNAMIC-FUNCTION("setCleanUpResize",THIS-PROCEDURE:CURRENT-WINDOW).
  IF VALID-HANDLE(hVisBilde)     THEN APPLY "close" TO hVisBilde.
  IF VALID-HANDLE(hArtikkelkort) THEN APPLY "close" TO hArtikkelkort.
  IF VALID-HANDLE(hStrekkode)    THEN APPLY "close" TO hStrekkode.
  IF VALID-HANDLE(hArtikkelKopi) THEN APPLY "close" TO hArtikkelKopi.
  IF VALID-HANDLE(hArtBilde)     THEN APPLY "close" TO hArtBilde.
  IF VALID-HANDLE(hBestBilde)    THEN APPLY "close" TO hBestBilde.
  IF VALID-HANDLE(hLagerstatus)  THEN APPLY "close" TO hLagerstatus.
  IF VALID-HANDLE(hPakkliste)    THEN APPLY "close" TO hPakkliste.
  IF VALID-HANDLE(hArtBasSok)    THEN APPLY "close" TO hArtBasSok.

  IF VALID-HANDLE(httArtKalk)    THEN DELETE OBJECT httArtKalk NO-ERROR.

  IF bUtvalgIsMaster AND VALID-HANDLE(hUtvalg) THEN 
    RUN InvalidateHandle IN hUtvalg (THIS-PROCEDURE).
  ELSE IF NOT bUtvalgIsMaster AND VALID-HANDLE(hUtvalg) THEN
    APPLY "close" TO hUtvalg.

  PUBLISH "InvalidateHandle" (THIS-PROCEDURE).
  RUN disable_UI.
  RETURN.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

{incl/wintrigg.i}

ON 'ctrl-S':U OF hWindow ANYWHERE DO: 
  IF iTab = 3 THEN DO:
    DYNAMIC-FUNCTION("DoLockWindow",THIS-PROCEDURE:CURRENT-WINDOW).
    APPLY "return".
    DYNAMIC-FUNCTION("setCurrentObject",hUpdToolBar).
    RUN NextRecord.
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
    DYNAMIC-FUNCTION("DoLockWindow",?).
  END.
  ELSE
    RETURN NO-APPLY.
END.

ON 'alt-p':U OF hWindow ANYWHERE DO: 
  IF iTab = 3 AND VALID-HANDLE(hEtikettVindu) THEN 
    RUN SkrivUt IN hEtikettVindu.
  ELSE RETURN NO-APPLY.
END.
ON 'alt-q':U OF hWindow ANYWHERE DO: 
  IF iTab = 3 AND VALID-HANDLE(hEtikettVindu) THEN 
    RUN Avslutt IN hEtikettVindu.
  ELSE RETURN NO-APPLY.
END.


ON 'alt-b' OF hWindow ANYWHERE DO: 
  IF iTab = 3 AND hFieldMap:AVAIL THEN DO:
/*     IF iTab = 3 AND hFieldMap:AVAIL  AND NOT Oppdatert:CHECKED IN FRAME frmDetalj THEN DO: */
/*   IF iTab = 3 AND hFieldMap:AVAIL AND hFieldMap:BUFFER-FIELD("VarebehType"):BUFFER-VALUE = iBestVarebeh THEN DO: */
    DYNAMIC-FUNCTION("setCurrentObject",hBestHodeToolbar).
    RUN NewRecord.
  END.
  ELSE RETURN NO-APPLY.
END.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  {lng.i}
/*   RUN enable_UI.     */
/*                      */
/*   setButikkListe().  */

  &IF DEFINED(UIB_is_Running) NE 0 &THEN
    DYNAMIC-FUNCTION("NewObject",SESSION,SESSION,"session").
/*     DYNAMIC-FUNCTION("setAttribute",SESSION,"ButikkListe","218").  */
    RUN InitializeObject.
    RUN MoveToTop.
  &ENDIF
  
/*   RUN InitWindow. */


  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

ON 'alt-a':U OF FRAME frmLinje ANYWHERE
  RUN Artikkelkort.

ON 'window-resized':U OF {&WINDOW-NAME} DO:
  DEF VAR rRowid     AS ROWID  NO-UNDO.
  DEF VAR hColumn    AS HANDLE NO-UNDO.
  DEF VAR cDummy     AS CHAR   NO-UNDO.
  DEF VAR ixPosOkReg AS INT    NO-UNDO.
  IF hFieldMap:AVAIL THEN rRowId = hFieldMap:ROWID.

  ixPosOkReg = FRAME frmOkReg:X.

  DYNAMIC-FUNCTION("DoLockWindow",THIS-PROCEDURE:CURRENT-WINDOW).
  DYNAMIC-FUNCTION("setAttribute",SESSION,"userkeepswindowlocked","yes").

  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"Resize","").

  IF NOT hFieldMap:AVAIL AND rRowid NE ? THEN
    hFieldMap:FIND-BY-ROWID(rRowid).

  IF iTab NE 3 THEN
    hLagerStatusFrame:MOVE-TO-BOTTOM().

  CASE iTab:
    WHEN 1 THEN DO:
      FRAME frmFilter:MOVE-TO-TOP().
      FRAME frmListe:MOVE-TO-TOP().
    END.
    WHEN 2 THEN
      FRAME frmDetalj:MOVE-TO-TOP().
    WHEN 3 THEN DO:        
      FRAME frmLinje:MOVE-TO-TOP().
      IF DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"newrow") = "yes" AND
         DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"uselocaldata") = "yes" THEN DO:
        ViewFrameOkReg().
/*        ViewOverlays(). */
/*         DYNAMIC-FUNCTION("DispBrwOverlayWidgets",hBrowseLinje).  */
/*         PUBLISH "ResizeBrowseColumns" (THIS-PROCEDURE:CURRENT-WINDOW).  */
/*         cDummy = sokLevNr:SCREEN-VALUE.                 */
/*         DISPLAY cDummy @ sokLevNr WITH FRAME frmLinje.  */


        FRAME frmOkReg:X = ixPosOkReg.
        setOverlays("link").
        PROCESS EVENTS.
        hColumn = DYNAMIC-FUNCTION("getBrowseColumn",hBrowseLinje,"LevKod").
        hColumn:WIDTH-PIXELS = hColumn:WIDTH-PIXELS + 1.
        APPLY "end-resize" TO hColumn.
      END.
    END.
    WHEN 4 THEN 
      FRAME frmOrdre:MOVE-TO-TOP().
  END CASE.

  DYNAMIC-FUNCTION("setAttribute",SESSION,"userkeepswindowlocked","").
  DYNAMIC-FUNCTION("DoLockWindow",?).
END.

ON F2,ALT-S OF hWindow ANYWHERE 
DO:
  /*
  IF iTab = 3 AND NOT Oppdatert:CHECKED IN FRAME frmDetalj AND DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"currviewfields") NE cCreateViewFields THEN DO:
    IF CAN-DO("1",cAdgang) OR (bHKinst AND cButikkListe NE "*") THEN RETURN NO-APPLY.

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
                                          "WHERE ArtBas.ModellFarge = " + cModellFarge
                                        + "  AND ArtBas.ArtikkelNr NE " + STRING(fArtikkelNr)).
        IF NUM-ENTRIES(cModellArtList,"|") > 0 THEN DO:
          IF DYNAMIC-FUNCTION("DoMessage",0,4,"Det finnes andre artikler med samme modell. Vil du også hente disse?","","") = 6 THEN
            DO iModIdx = 1 TO NUM-ENTRIES(cModellArtList,"|"):
              RUN NyArtBas (DEC(ENTRY(iModIdx,cModellArtList,"|"))).
            END.
        END.
      END.
    END.

    DO:
      RUN NyArtBas (fArtikkelNr).
      bAlt-S = TRUE.
    END.
  END.
  ELSE 
  */
  IF iTab = 3 AND NOT Oppdatert:CHECKED IN FRAME frmDetalj THEN DO:
/*     IF DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"currviewfields") = cCreateViewFields THEN DO:  */
      DYNAMIC-FUNCTION("setCurrentObject",hBrwOLLevKod).
      RUN BrowseColumnLookup.
/*     END. */
  END.
  ELSE IF iTab < 3 THEN DO WITH FRAME frmFilter:
    RUN d-hsok.w (OUTPUT fArtikkelNr,"JA"). 
    IF fArtikkelNr = ? OR fArtikkelNr = 0 THEN 
      RETURN NO-APPLY.

    IF tbMatchBestStat:CHECKED AND sokBestStat:SCREEN-VALUE NE ? THEN
      DYNAMIC-FUNCTION("setAttribute",hBrowseListe,"prescanqueryfilter",
                       "BestHode WHERE ArtikkelNr = " + STRING(fArtikkelNr) + " AND BestHode.BestStat = " 
                     + (IF sokBestStat:SCREEN-VALUE NE "44" THEN sokBestStat:SCREEN-VALUE
                        ELSE "4 AND BestHode.BekreftetDato NE ?")  
                     + ",EACH VarebehBestHode OF BestHode NO-LOCK,FIRST VarebehHode OF VarebehBestHode NO-LOCK").
    ELSE IF cmbVarebehType:SCREEN-VALUE = "1" THEN
      DYNAMIC-FUNCTION("setAttribute",hBrowseListe,"prescanqueryfilter",
                       "VarebehBestHode WHERE ArtikkelNr = " + STRING(fArtikkelNr) 
                     + " AND VarebehBestHode.BestNr > 0"
                     + ",FIRST VarebehHode OF VarebehBestHode NO-LOCK").
    ELSE
      DYNAMIC-FUNCTION("setAttribute",hBrowseListe,"prescanqueryfilter",
                       "VarebehLinje WHERE ArtikkelNr = " + STRING(fArtikkelNr) 
                     + ",FIRST VarebehHode OF VarebehLinje NO-LOCK").
    DYNAMIC-FUNCTION("setCurrentObject",hBrowseListe).
    RUN StartQuery.
    DYNAMIC-FUNCTION("setAttribute",hBrowseListe,"prescanqueryfilter","").
    IF hBrowseListe:QUERY:NUM-RESULTS > 0 THEN
      sokArtikkelNr:SCREEN-VALUE IN FRAME frmLinje = STRING(fArtikkelNr).
  END.
  ELSE
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ArtBasEndret C-Win 
PROCEDURE ArtBasEndret :
/*------------------------------------------------------------------------------
  Purpose:    Fange endring av artikkelegenskaper 
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ifArtikkelnr AS DEC NO-UNDO.

IF iTab NE 3 OR NOT hFieldMap:AVAIL OR hFieldMap:BUFFER-FIELD("Kilde"):BUFFER-VALUE NE 0 THEN RETURN. 

DYNAMIC-FUNCTION("runproc","update_varebeh_from_artbas.p",
                           STRING(ifArtikkelnr) + "," + 
                           STRING(hFieldMap:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE),
                           ?).

DYNAMIC-FUNCTION("refreshRowids",hBrowseLinje,hFieldMapLinje:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE).
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
DEFINE VAR cRowIdArtBas AS CHAR NO-UNDO.

IF bHKinst AND cButikkListe NE "*" THEN RETURN.

IF NOT VALID-HANDLE(hArtikkelkort) THEN DO:
  IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowseLinje THEN DO:
    cRowIdArtBas = DYNAMIC-FUNCTION("getRowIdList","ArtBas","","WHERE ArtikkelNr = " + STRING(hFieldMapLinje:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE)).
    RUN w-vartkor.w  PERSIST SET hArtikkelkort (DYNAMIC-FUNCTION("getRecId","ArtBas",cRowIdArtBas), "ENDRE," + STRING(THIS-PROCEDURE)).
  END.
  ELSE IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowseOrdreBest OR DYNAMIC-FUNCTION("getCurrentObject") = hBestToolbar THEN
    RUN w-vartkor.w  PERSIST SET hArtikkelkort (DYNAMIC-FUNCTION("getRecId","ArtBas",
                                                hBrowseOrdreBest:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("RowIdent2"):BUFFER-VALUE), "ENDRE," + STRING(THIS-PROCEDURE)).
END.
ELSE DO:
  IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowseLinje THEN
    RUN ByttArtikkel IN hArtikkelkort (hFieldMapLinje:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE).
  ELSE IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowseOrdreBest OR DYNAMIC-FUNCTION("getCurrentObject") = hBestToolbar THEN
    RUN ByttArtikkel IN hArtikkelkort (hBrowseOrdreBest:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BeforeNavBrowseFillIn C-Win 
PROCEDURE BeforeNavBrowseFillIn :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT  PARAM ihFillIn AS HANDLE NO-UNDO.
DEF INPUT  PARAM ihBuffer AS HANDLE NO-UNDO.
DEF OUTPUT PARAM obOk     AS LOG    NO-UNDO.

DEF VAR cValue         AS CHAR   NO-UNDO.
DEF VAR cLastEvent     AS CHAR   NO-UNDO.

IF DYNAMIC-FUNCTION("getAttribute",ihFillIn,"last-event") NE "" THEN 
  cLastEvent = DYNAMIC-FUNCTION("getAttribute",ihFillIn,"last-event").
ELSE
  cLastEvent = LAST-EVENT:LABEL.

IF ihFillIn = hBrwOLLevKod AND ihFillIn:SCREEN-VALUE NE "" THEN DO:
  IF NOT setOverlayValues("",ihFillIn:SCREEN-VALUE) THEN DO:
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
    DYNAMIC-FUNCTION("setWidgetEnter",ihFillIn).
  END.

  IF cLastEvent = "return" THEN
    DYNAMIC-FUNCTION("setAttribute",ihFillIn,"last-event","tab").
END.
ELSE IF ihFillIn = hBrwOLMatKod THEN DO:    
  cValue = DYNAMIC-FUNCTION("getFieldValues","Material","WHERE MatKod = " + hBrwOLMatKod:SCREEN-VALUE,"MatBeskr").
  IF cValue = ? THEN DO:
    DYNAMIC-FUNCTION("DoMessage",0,0,"Ugyldig materialkode: " + ihFillIn:SCREEN-VALUE,"","").
    DYNAMIC-FUNCTION("setWidgetEnter",ihFillIn).
    RETURN.
  END.
  ELSE hFieldMapLinje:BUFFER-FIELD("MatBeskr"):BUFFER-VALUE = cValue.
END.
ELSE IF ihFillIn = hBrwOLFarg THEN DO:    
  cValue = DYNAMIC-FUNCTION("getFieldValues","Farg","WHERE Farg = " + hBrwOLFarg:SCREEN-VALUE,"FarBeskr").
  IF cValue = ? THEN DO:
    DYNAMIC-FUNCTION("DoMessage",0,0,"Ugyldig fargekode: " + ihFillIn:SCREEN-VALUE,"","").
    DYNAMIC-FUNCTION("setWidgetEnter",ihFillIn).
    RETURN.
  END.
  ELSE hFieldMapLinje:BUFFER-FIELD("FarBeskr"):BUFFER-VALUE = cValue.
END.
ELSE IF ihFillIn = hBrwOLStrTypeId THEN DO:    
  cValue = DYNAMIC-FUNCTION("getFieldValues","StrType","WHERE StrTypeId = " + hBrwOLStrTypeId:SCREEN-VALUE,"Beskrivelse").
  IF cValue = ? THEN DO:
    DYNAMIC-FUNCTION("DoMessage",0,0,"Ugyldig størrelsestype: " + ihFillIn:SCREEN-VALUE,"","").
    DYNAMIC-FUNCTION("setWidgetEnter",ihFillIn).
    RETURN.
  END.
  ELSE hFieldMapLinje:BUFFER-FIELD("Beskrivelse"):BUFFER-VALUE = cValue.
  IF cLastEvent = "return" THEN
    DYNAMIC-FUNCTION("setAttribute",ihFillIn,"last-event","tab").
END.
ELSE IF ihFillIn = hBrwOLVg THEN DO:    
  cValue = DYNAMIC-FUNCTION("getFieldValues","VarGr","WHERE Vg = " + hBrwOLVg:SCREEN-VALUE,"VgBeskr").
  IF cValue = ? THEN DO:
    DYNAMIC-FUNCTION("DoMessage",0,0,"Ugyldig varegruppe: " + ihFillIn:SCREEN-VALUE,"","").
    DYNAMIC-FUNCTION("setWidgetEnter",ihFillIn).
    RETURN.
  END.
  ELSE hFieldMapLinje:BUFFER-FIELD("VgBeskr"):BUFFER-VALUE = cValue.
END.
ELSE IF ihFillIn:DATA-TYPE = "DECIMAL" THEN DO:
  IF DYNAMIC-FUNCTION("runProc","butvarebehlinje_kalkny.p",
                      ihFillIn:NAME + "|" + ihFillIn:SCREEN-VALUE + "|" + cInitPrisProfil + "|" + hFieldMap:BUFFER-FIELD("VarebehNr"):STRING-VALUE,
                      getArtKalkTT()) THEN DO:                          
    DYNAMIC-FUNCTION("getRunProcReturnTable",hbArtKalk).

    hFieldMapLinje:BUFFER-COPY(hbArtKalk).

    DO ix = 1 TO hBrowseLinje:NUM-COLUMNS:
      hBrowseLinje:GET-BROWSE-COLUMN(ix):SCREEN-VALUE = STRING(hFieldMapLinje:BUFFER-FIELD(hBrowseLinje:GET-BROWSE-COLUMN(ix):NAME):BUFFER-VALUE).
    END.

/*     hBrowseLinje:REFRESH(). */
    DYNAMIC-FUNCTION("DispBrwOverlayWidgets",hBrowseLinje).
  END.
END.


hBrowseLinje:REFRESH().

IF cLastEvent = "enter" AND (WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"lastenabledoverlay")) = ihFillIn OR
    ihFillIn = hBrwOLPris OR
    ihFillIn = hBrwOLAnbefaltPris OR
    ihFillIn = hBrwOLMerkFi OR
    ihFillIn = hBrwOLMerk)
   THEN 
  RUN LagreNyLinje.
ELSE IF WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"lastenabledoverlay")) = ihFillIn AND cLastEvent = "tab" THEN
  DYNAMIC-FUNCTION("setWidgetEnd",btnOk:HANDLE IN FRAME frmOkReg).  
ELSE IF cLastEvent BEGINS "cursor" THEN 
  obOK = NO.
ELSE IF cLastEvent MATCHES "*tab" THEN 
  obOk = YES.
ELSE IF cLastEvent = "return" THEN DO:
  obOk = YES.
  DYNAMIC-FUNCTION("setAttribute",ihFillIn,"last-event","tab").
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BekrBestRecord C-Win 
PROCEDURE BekrBestRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cBestNrList AS CHAR NO-UNDO.
DEF VAR cBekrDato   AS CHAR NO-UNDO.
DEF VAR cParam      AS CHAR NO-UNDO INIT "best;".

/* TN 25/6-06 Endret for at server rutine skal få riktig verdi. */
cParam = "*best|".

DO ix = 1 TO hBrowseOrdreBest:NUM-SELECTED-ROWS:
  IF hBrowseOrdreBest:FETCH-SELECTED-ROW(ix) THEN
    cBestNrList = cBestNrList + STRING(hBrowseOrdreBest:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("BestNr"):BUFFER-VALUE) + "|".
END.

IF cBestNrList NE "" THEN DO:

  RUN JBoxBrowseMsgUpdSelVal.w ("Sett valgte bestillinger til bekreftet?",
                                hBrowseOrdreBest:NUM-SELECTED-ROWS,
                                0,
                                "date|99/99/9999|" + STRING(TODAY),
                                OUTPUT cBekrDato,
                                OUTPUT iReturn).

  IF iReturn NE 2 THEN RETURN.

  IF DATE(cBekrDato) = ? THEN DO:
    DYNAMIC-FUNCTION("DoMessage",0,0,"Ugyldig dato for ordrebekreftelse","","").
    RETURN.
  END.

  cParam = cParam + cBekrDato + ";" + DYNAMIC-FUNCTION("getASuserId") + ";" + TRIM(cBestNrList,"|").

  IF NOT DYNAMIC-FUNCTION("runproc","ordre_best_bekreft.p",cParam,?) THEN 
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
  ELSE DO:
    DYNAMIC-FUNCTION("RefreshRowids",hBrowseOrdre,hBrowseOrdre:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("RowIdent1"):BUFFER-VALUE).
    APPLY "value-changed" TO hBrowseOrdre.
  END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BekrOrdreRecord C-Win 
PROCEDURE BekrOrdreRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN OrdreBehandling("bekreft").
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BildeImport C-Win 
PROCEDURE BildeImport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iReturn     AS INT  NO-UNDO.
DEF VAR cArtNrList  AS CHAR NO-UNDO.
DEF VAR cFilKatalog AS CHAR NO-UNDO.


RUN d-BildeImport.w ("Importer bilde på artikkel",
                      hBrowseLinje:NUM-SELECTED-ROWS,
                      DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"recordcount"),
                      OUTPUT iReturn,
                      OUTPUT bOK,
                      OUTPUT cFilKatalog).
MESSAGE PROGRAM-NAME(1) SKIP
        iReturn SKIP
        VIEW-AS ALERT-BOX.
IF iReturn = 0 THEN RETURN.

IF hBrowseLinje:NUM-SELECTED-ROWS > 0 AND iReturn = 2 THEN DO:
/*   DO ix = 1 TO hBrowseLinje:NUM-SELECTED-ROWS:                                                                         */
/*     IF hBrowseLinje:FETCH-SELECTED-ROW(ix) THEN                                                                        */
/*       cArtNrList = cArtNrList +                                                                                        */
/*                         STRING(hBrowseLinje:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) + ",". */
/*   END.                                                                                                                 */
/*   IF NOT DYNAMIC-FUNCTION("RunProc","art_bildeimport.p",                                       */
/*                           STRING(bOk) + "," + cFilKatalog + ",ARTNUM," + TRIM(cArtNrList,","), */
/*                           ?) THEN                                                              */
/*     DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Advarsel",""). */

  IF NOT DYNAMIC-FUNCTION("processSelectedRows",hBrowseLinje,"art_bildeimport.p",
                          STRING(bOk) + "," + cFilKatalog) THEN
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Advarsel","").
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BrowseColumnLookup C-Win 
PROCEDURE BrowseColumnLookup :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF DYNAMIC-FUNCTION("getCurrentObject") = hBrwOLLevKod THEN DO WITH FRAME frmLinje:
  IF VALID-HANDLE(hArtBasSok) THEN APPLY "close" TO hArtBasSok.

  RUN ArtBasSok.w PERSIST SET hArtBasSok.
/*   DYNAMIC-FUNCTION("setButikkNr" IN hArtBasSok,hParentBuffer:BUFFER-FIELD("ButikkNr"):BUFFER-VALUE).   */
/*   DYNAMIC-FUNCTION("setOrdreId"  IN hArtBasSok,hParentBuffer:BUFFER-FIELD("KOrdre_Id"):BUFFER-VALUE).  */
  DYNAMIC-FUNCTION("setCloseOnSelect" IN hArtBasSok,YES).
  DYNAMIC-FUNCTION("setUpdateCurrentRow" IN hArtBasSok,YES).
  DYNAMIC-FUNCTION("setVarebehNr" IN hArtBasSok,hFieldMap:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE).
  RUN InitializeObject IN hArtBasSok.
  IF sokAvdelingNr:SCREEN-VALUE NE "0" THEN
    DYNAMIC-FUNCTION("setAvdNr" IN hArtBasSok,sokAvdelingNr:SCREEN-VALUE,NO).
  IF sokHg:SCREEN-VALUE NE "0" THEN
    DYNAMIC-FUNCTION("setHg" IN hArtBasSok,sokHg:SCREEN-VALUE,NO).
  IF sokVg:SCREEN-VALUE NE "0" THEN
    DYNAMIC-FUNCTION("setVg" IN hArtBasSok,sokVg:SCREEN-VALUE,NO).
  DYNAMIC-FUNCTION("setLevNr" IN hArtBasSok,sokLevNr:SCREEN-VALUE IN FRAME frmLinje,YES).

  RUN MoveToTop IN hArtBasSok.
END.

ELSE IF DYNAMIC-FUNCTION("getCurrentObject") = hBrwOLStrTypeId THEN DO WITH FRAME frmLinje:
    
  DEF VAR iocColValues   AS CHAR  NO-UNDO.
  DEF VAR iAvd           AS INT   NO-UNDO.
  DEF VAR iHg            AS INT   NO-UNDO.
  DEF VAR cWhere         AS CHAR  NO-UNDO INIT "strtypeid > 1".
  DEF VAR iSaveStrTypeid AS INT   NO-UNDO.
  DEF VAR iStrTypeId     AS INT   NO-UNDO.

  RUN gstrtype.w (INPUT-OUTPUT iocColValues,
                  "", /* cFelt */
                  "", /* cVerdier */
                  "", /*cStart */
                  INT(sokAvdelingNr:SCREEN-VALUE),
                  INT(sokHg:SCREEN-VALUE), 
                  cWhere).

  DYNAMIC-FUNCTION("setAttribute",hBrwOLStrTypeId,"last-event","tab").

  IF NUM-ENTRIES(RETURN-VALUE) >= 2 THEN
    ASSIGN
      hBrwOLStrTypeId:SCREEN-VALUE = ENTRY(2,RETURN-VALUE)
      NO-ERROR.
  IF CAN-DO("ENDRE-STRTYPE,NY-STRTYPE",ENTRY(1,RETURN-VALUE)) THEN DO:
    iSaveStrTypeid = INT(hBrwOLStrTypeId:SCREEN-VALUE).
    iStrTypeId     = (IF ENTRY(1,RETURN-VALUE) = "NY-STRTYPE"
                      THEN ?
                      ELSE INT(hBrwOLStrTypeId:SCREEN-VALUE)).
    RUN d-strtype.w (INPUT-OUTPUT iStrTypeId,
                     iAvd,
                     iHg).
    IF ENTRY(1,RETURN-VALUE) = "AVBRYT" THEN 
    DO:
        hBrwOLStrTypeId:SCREEN-VALUE = STRING(isavestrtypeid).
        APPLY "ENTRY" TO hBrwOLStrTypeId.
        APPLY "tab" TO hBrwOLStrTypeId.
    END.
    ELSE
        hBrwOLStrTypeId:SCREEN-VALUE = STRING(iStrTypeId).
  END.
  ELSE DO:
      IF RETURN-VALUE = "AVBRYT" THEN
          .
      ELSE IF NUM-ENTRIES(iocColValues,CHR(1)) >= 3 THEN
      DO:
        ASSIGN hBrwOLStrTypeId:SCREEN-VALUE = ENTRY(2,iocColValues,CHR(1)).
      END.
  END.
  APPLY "tab" TO hBrwOLStrTypeId.
END.

ELSE RUN SUPER.

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

OCXFile = SEARCH( "butvarebehhode.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chGrid = Grid:COM-HANDLE
    UIB_S = chGrid:LoadControls( OCXFile, "Grid":U)
    Grid:NAME = "Grid":U
    chTabStrip = TabStrip:COM-HANDLE
    UIB_S = chTabStrip:LoadControls( OCXFile, "TabStrip":U)
    TabStrip:NAME = "TabStrip":U
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "butvarebehhode.wrx":U SKIP(1)
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
DEF VAR cReturn AS CHAR NO-UNDO.

IF iTab = 1 THEN DO:
  TabStripChanged(12).
  DYNAMIC-FUNCTION("setCurrentObject",hUpdToolbar).
END.

IF iTab = 3 AND hFieldMapLinje:AVAIL THEN DO:  
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN ArtBasVedlikehold.w (THIS-PROCEDURE,"copy",OUTPUT bOk).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.
  IF NOT LevDato:HIDDEN IN FRAME frmLinje THEN
    APPLY "entry" TO LevDato.
  ELSE APPLY "entry" TO Grid.

  RETURN NO-APPLY.
END.
ELSE IF iTab = 4 AND DYNAMIC-FUNCTION("getCurrentObject") = hOrdreToolbar THEN DO:
  cReturn = STRING(TODAY + 7).
  RUN JBoxAskForValue.w ("Kopier ordre " + hBrowseOrdre:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("Ordrenr"):STRING-VALUE
                                         + " (kalkyle hentes fra artikkelreg). Leveringsdato:",
                         "DATE|99/99/9999",
                         INPUT-OUTPUT cReturn,
                         OUTPUT bOK).
  IF DATE(cReturn) < TODAY - 30 OR DATE(cReturn) > TODAY + 365 THEN
    bOk = DYNAMIC-FUNCTION("DoMessage",0,4,"Er du sikker på at leveringsdato er " + cReturn,"","") = 6.
  IF bOk THEN DO:
    IF DYNAMIC-FUNCTION("runproc","ordre_kopi.p",hBrowseOrdre:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("Ordrenr"):STRING-VALUE + ";" + cReturn + ";" + DYNAMIC-FUNCTION("getASuserId"),?) THEN DO:
      DYNAMIC-FUNCTION("setCurrentObject",hBrowseOrdre).
      RUN OpenQuery.
    END.
    ELSE DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
  END.
END.
ELSE RUN SUPER.

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
IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowseListe THEN 
  TabStripChanged(13).
ELSE IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowseLinje OR DYNAMIC-FUNCTION("getCurrentObject") = hBrowseOrdreBest
        AND DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"currviewfields") = cCreateViewFields THEN 
  RUN Artikkelkort.    
/* ELSE IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowseOrdreBest THEN DO:                                                                      */
/*   sokArtikkelNr:SCREEN-VALUE IN FRAME frmLinje = STRING(hBrowseOrdreBest:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE). */
/*   TabStripChanged(13).                                                                                                                        */
/*   DYNAMIC-FUNCTION("setCurrentObject",hBrowseLinje).                                                                                          */
/*   RUN OpenQuery.                                                                                                                              */
/* END.                                                                                                                                          */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DeleteRecord C-Win 
PROCEDURE DeleteRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cRepos       AS CHAR  NO-UNDO.
DEF VAR iFocusRow    AS INT   NO-UNDO.

IF DYNAMIC-FUNCTION("getCurrentObject") = hBestToolbar OR DYNAMIC-FUNCTION("getCurrentObject") = hBrowseOrdreBest THEN DO:
  iFocusRow = hBrowseOrdre:FOCUSED-ROW.
  IF iFocusRow NE 0 THEN 
    cRepos = hBrowseOrdre:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("RowIdent1"):BUFFER-VALUE.


  IF hBrowseOrdreBest:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("BestStat"):BUFFER-VALUE = 4 
     AND hBrowseOrdreBest:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("BekreftetDato"):BUFFER-VALUE = ? 
     THEN DO:
    bOk = NO.
    RUN dBestillingSlettAdvarsel.w (OUTPUT bOk).
    IF bOk THEN
      IF NOT DYNAMIC-FUNCTION("runproc","slett_sendt_bestilling.p",STRING(hBrowseOrdreBest:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("Bestnr"):BUFFER-VALUE),?) THEN
        DYNAMIC-FUNCTIO("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
      ELSE APPLY "value-changed" TO hBrowseOrdre.
    RETURN.
  END.
END.

IF (DYNAMIC-FUNCTION("getCurrentObject") = hOrdreToolbar OR DYNAMIC-FUNCTION("getCurrentObject") = hBrowseOrdre) THEN DO:
  IF hBrowseOrdre:NUM-SELECTED-ROWS NE 1 THEN DO:
    DYNAMIC-FUNCTION("DoMessage",0,0,"Sletting kan bare gjøre hvis én ordre er valgt","","").
    RETURN.
  END.
  ELSE IF hBrowseOrdre:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("OrdreStatus"):BUFFER-VALUE = 2 THEN DO:
    bOk = NO.
    RUN dOrdreSlettAdvarsel.w (OUTPUT bOk).
    IF bOk THEN
      IF NOT DYNAMIC-FUNCTION("runproc","slett_sendt_ordre.p",STRING(hBrowseOrdre:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("Ordrenr"):BUFFER-VALUE),?) THEN
        DYNAMIC-FUNCTIO("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
      ELSE DO:
        DYNAMIC-FUNCTION("setCurrentObject",hBrowseOrdre).
        RUN OpenQuery.
      END.
    RETURN.
  END.
END.

RUN SUPER.

IF iFocusRow NE 0 THEN DO:
  DYNAMIC-FUNCTION("setCurrentObject",hBrowseOrdre).
  RUN OpenQuery.

  bOk = hBrowseOrdre:QUERY:GET-BUFFER-HANDLE(1):FIND-FIRST("WHERE RowIdent1 = '" + cRepos + "'") NO-ERROR.
  IF bOk THEN DO:
    hBrowseOrdre:SET-REPOSITIONED-ROW(iFocusRow,"always").
    hBrowseOrdre:QUERY:REPOSITION-TO-ROWID(hBrowseOrdre:QUERY:GET-BUFFER-HANDLE(1):ROWID) NO-ERROR.
    IF NOT ERROR-STATUS:ERROR THEN DO:
      hBrowseOrdre:SELECT-ROW(iFocusRow).
      DYNAMIC-FUNCTION("setCurrentObject",hBrowseOrdre).
      RUN DisplayRecord.
    END.
  END.
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
ELSE IF iTab = 4 THEN
  hBrowseOrdreBestStr:DESELECT-ROWS() NO-ERROR.
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
DEF VAR cDisabled AS CHAR NO-UNDO.
DEF VAR cMerknadList AS CHAR NO-UNDO.
DEF VAR bOnlineVB    AS LOG  NO-UNDO.


DYNAMIC-FUNCTION("DoLockWindow",THIS-PROCEDURE:CURRENT-WINDOW).
IF VALID-HANDLE(hArtikkelkort) THEN
  DYNAMIC-FUNCTION("DoLockWindow",hArtikkelkort:CURRENT-WINDOW).  
DYNAMIC-FUNCTION("setAttribute",SESSION,"userkeepswindowlocked","yes").

IF VALID-HANDLE(hBrwOLMerk) THEN DO:
  IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowseListe AND hFieldMap:AVAIL THEN DO WITH FRAME frmLinje:
    DO ix = 1 TO NUM-ENTRIES(hFieldMap:BUFFER-FIELD("Oppmerking"):BUFFER-VALUE,"¤"):
      cMerknadList = cMerknadList + 
                     ENTRY(ix,hFieldMap:BUFFER-FIELD("Oppmerking"):BUFFER-VALUE,"¤") + "|" +
                     ENTRY(ix,hFieldMap:BUFFER-FIELD("Oppmerking"):BUFFER-VALUE,"¤") + "|".
    END.
    hBrwOLMerk:LIST-ITEM-PAIRS = IF cMerknadList NE "" THEN "||" + TRIM(cMerknadList,"|") ELSE "|".
  END.
  ELSE IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowseLinje AND hFieldMapLinje:AVAIL THEN DO:
    IF hBrwOLMerk:LOOKUP(hFieldMapLinje:BUFFER-FIELD("LinjeMerknad"):BUFFER-VALUE) = 0 THEN
       hBrwOLMerk:LIST-ITEM-PAIRS = hBrwOLMerk:LIST-ITEM-PAIRS + "|" + hFieldMapLinje:BUFFER-FIELD("LinjeMerknad"):BUFFER-VALUE + "|" + hFieldMapLinje:BUFFER-FIELD("LinjeMerknad"):BUFFER-VALUE.
  END.
END.
  
IF hFieldMap:AVAIL THEN DO:
  IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowseListe THEN DO:      
    bOnlineVB = NOT bHKinst AND hFieldMap:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE GE 90000000.  
    IF bOnlineVB OR hFieldMap:BUFFER-FIELD("VareBehType"):BUFFER-VALUE NE iBestVarebeh THEN
      tbOpprettBest:HIDDEN IN FRAME frmLinje = YES.
  END.      

  IF iTab = 3 THEN DO: 
    IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowseLinje THEN DO:
  
      IF hFieldMap:BUFFER-FIELD("Oppdatert"):BUFFER-VALUE THEN DO:
        FRAME frmOkReg:HIDDEN = YES.          
        DYNAMIC-FUNCTION("setAttribute",hUpdToolbar,"disabledevents","new,copy,delete").
      END. 
      ELSE IF DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"uselocaldata") = "yes" THEN          
        DYNAMIC-FUNCTION("setAttribute",hUpdToolbar,"disabledevents","*").
      ELSE DO:          
        DYNAMIC-FUNCTION("setAttribute",hUpdToolbar,"disabledevents","").
        FRAME frmOkReg:HIDDEN = YES.
      END.

      IF hFieldMapLinje:AVAIL THEN DO:          
        IF hFieldMapLinje:BUFFER-FIELD("LevNr"):BUFFER-VALUE NE iCurrLevNr AND hFieldMapLinje:BUFFER-FIELD("LevNr"):BUFFER-VALUE NE 0 AND DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"uselocaldata") NE "yes" THEN
          dMinLevDato = ?.
  
        IF (dSisteLevDato = ? OR bHKinst OR hFieldMapLinje:BUFFER-FIELD("LevNr"):BUFFER-VALUE NE iCurrLevNr) THEN
          dSisteLevDato = getLevDato(hFieldMapLinje:BUFFER-FIELD("LevNr"):BUFFER-VALUE).
  
        iCurrLevNr = hFieldMapLinje:BUFFER-FIELD("LevNr"):BUFFER-VALUE.

        IF hFieldMapLinje:BUFFER-FIELD("LevKod"):BUFFER-VALUE NE "" OR hFieldMapLinje:BUFFER-FIELD("Beskr"):BUFFER-VALUE NE "" THEN
          setOverlays("").
      END.

    END.
    ELSE IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowseBestHode THEN DO:
      Grid:SENSITIVE = TRUE.

      IF hFieldMap:BUFFER-FIELD("VareBehType"):BUFFER-VALUE = iBestVarebeh AND 
         cButikkListe NE "*" AND bHKinst THEN
        cDisabled = "Strekkode,Kalkyle".

      IF hFieldMap:BUFFER-FIELD("Oppdatert"):BUFFER-VALUE OR bOnlineVB THEN 
        cDisabled = TRIM(cDisabled + ",new,save,delete,kalkyle",",").

      IF hFieldMap:BUFFER-FIELD("VareBehType"):BUFFER-VALUE NE iBestVarebeh 
         OR (hFieldMapBestHode:AVAIL 
             AND hFieldMapBestHode:BUFFER-FIELD("BestNr"):BUFFER-VALUE = 0) 
         OR NOT hFieldMapBestHode:AVAIL THEN DO:
        cDisabled = TRIM(cDisabled + ",VisBest,VisInnlev",",").
        IF hFieldMap:BUFFER-FIELD("VareBehType"):BUFFER-VALUE NE iBestVarebeh 
          AND (hFieldMapBestHode:AVAIL AND hFieldMapBestHode:BUFFER-FIELD("BestNr"):BUFFER-VALUE NE 0)
               OR NOT hFieldMapBestHode:AVAIL THEN DO:
          Grid:SENSITIVE = FALSE.
          cDisabled = cDisabled + ",Delete,Kalkyle".
        END.
      END.
      ELSE IF hFieldMap:BUFFER-FIELD("VareBehType"):BUFFER-VALUE = iBestVarebeh 
         AND hFieldMapBestHode:AVAIL 
         AND hFieldMapBestHode:BUFFER-FIELD("BestStat"):BUFFER-VALUE GE 4 
          THEN 
        cDisabled = TRIM(cDisabled + ",Save,Delete",",").
      
      IF hFieldMap:BUFFER-FIELD("VareBehType"):BUFFER-VALUE NE iBestVarebeh 
         OR (hFieldMapBestHode:AVAIL AND hFieldMapBestHode:BUFFER-FIELD("BestStat"):BUFFER-VALUE < 4) THEN
        cDisabled = TRIM(cDisabled + ",VisInnlev",",").

      DYNAMIC-FUNCTION("setAttribute",hBestHodeToolbar,"disabledevents",cDisabled).
    END.

  END.
  ELSE IF iTab = 4 THEN DO:      
    DYNAMIC-FUNCTION("setAttribute",hUpdToolbar,"disabledevents","new,copy,edit").

    IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowseOrdre AND sokOrdreLevKod:SCREEN-VALUE IN FRAME frmOrdre NE "" THEN
      DYNAMIC-FUNCTION("setSortString",hBrowseOrdreBest,"Levkod").
    ELSE IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowseOrdre AND sokOrdreLevFargKod:SCREEN-VALUE IN FRAME frmOrdre NE "" THEN
      DYNAMIC-FUNCTION("setSortString",hBrowseOrdreBest,"LevFargKod").

    /* 25.5.07: Skal ikke lenger ha overlay.. */
/*     IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowseOrdre AND hBuffOrdre:AVAIL  THEN DO:                                                                 */
/*       IF hBuffOrdre:BUFFER-FIELD("OrdreStatus"):BUFFER-VALUE = 1 AND DYNAMIC-FUNCTION("getLinkedObject",hBrowseOrdre,"browseoverlay","from") = ? THEN DO: */
/*         DYNAMIC-FUNCTION("DeleteObject",hHasteordreOverlay).                                                                                              */
/*         hHasteordreOverlay = DYNAMIC-FUNCTION("NewBrowseToggle",                                                                                          */
/*                                               hBrowseOrdre,                                                                                               */
/*                                               "Hasteordre",                                                                                               */
/*                                               "Hasteordre",                                                                                               */
/*                                               "").                                                                                                        */
/*         DYNAMIC-FUNCTION("CreateOverlayLink",hBrowseOrdre,hHasteOrdreOverlay,"Hasteordre").                                                               */
/*       END.                                                                                                                                                */
/*       ELSE IF hBuffOrdre:BUFFER-FIELD("OrdreStatus"):BUFFER-VALUE NE 1 THEN                                                                               */
/*         DYNAMIC-FUNCTION("DeleteObject",hHasteOrdreOverlay).                                                                                              */
/*     END.                                                                                                                                                  */
  END.
  ELSE IF bUtvalgIsMaster THEN
    DYNAMIC-FUNCTION("setAttribute",hUpdToolbar,"disabledevents","HentUtvalg").
  ELSE IF iTab < 3 AND hFieldMap:BUFFER-FIELD("Oppdatert"):BUFFER-VALUE THEN
    DYNAMIC-FUNCTION("setAttribute",hUpdToolbar,"disabledevents","delete,edit").
  ELSE IF iTab < 3 THEN
    DYNAMIC-FUNCTION("setAttribute",hUpdToolbar,"disabledevents","edit").
  ELSE 
    DYNAMIC-FUNCTION("setAttribute",hUpdToolbar,"disabledevents","").

  iColNumBestilt = setFieldColor(INT(getFromNotat("bestillingsfarge"))).

  IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowseListe AND hFieldMap:AVAIL AND NOT bHKinst AND hFieldMap:BUFFER-FIELD("ButikkListe"):BUFFER-VALUE NE "" THEN 
    cButikkListe = hFieldMap:BUFFER-FIELD("ButikkListe"):BUFFER-VALUE.
END.
ELSE DO:
  IF iTab > 2 THEN 
    DYNAMIC-FUNCTION("setAttribute",hUpdToolbar,"disabledevents","new,StartUtvalg").
  ELSE
    DYNAMIC-FUNCTION("setAttribute",hUpdToolbar,"disabledevents","StartUtvalg").
END.

RUN SUPER.

IF iTab = 3 THEN DO:
  IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowseLinje THEN DO:
    RUN VisMiniBilde IN hArtBilde 
        (IF hFieldMapLinje:AVAIL THEN hFieldMapLinje:BUFFER-FIELD("BildNr"):BUFFER-VALUE 
         ELSE 0).
    
    IF VALID-HANDLE(hArtikkelkort) AND NOT bNewArt THEN 
      RUN ByttArtikkel IN hArtikkelkort (IF hFieldMapLinje:AVAIL THEN hFieldMapLinje:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE
                                         ELSE 0).
    VisLagerStatus().
    ViewOverlays().
  END.
  ELSE IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowseBestHode AND NOT bSkipInitGrid THEN DO:
    IF hFieldMapBestHode:AVAIL THEN DO:
      IF DYNAMIC-FUNCTION("runproc","strtype_sjekk_strekkode.p",STRING(hFieldMapLinje:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE),?) THEN
        cAlfaFord = DYNAMIC-FUNCTION("getTransactionMessage").
      ELSE cAlfaFord = hFieldMapBestHode:BUFFER-FIELD("AlfaFordeling"):BUFFER-VALUE.
      cAlfaFord = REPLACE(cAlfaFord," ","").
      RUN InitGrid(cAlfaFord).
    END.
    ELSE RUN InitGrid ("").
  END.
  bSkipInitGrid = NO.
END.
ELSE IF iTab = 4 THEN DO:
  IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowseOrdreBest THEN DO:
    RUN VisMiniBilde IN hBestBilde 
        (IF hBrowseOrdreBest:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN 
           hBrowseOrdreBest:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("BildNr"):BUFFER-VALUE 
         ELSE 0).

    IF VALID-HANDLE(hArtikkelkort) THEN 
      RUN ByttArtikkel IN hArtikkelkort (IF hBrowseOrdreBest:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN 
                                           hBrowseOrdreBest:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE
                                         ELSE 0).
  END.
  ELSE IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowseOrdre AND hBuffOrdre:AVAIL THEN DO:
    IF VALID-HANDLE(hHasteordreOverlay) THEN DO:
      IF hHasteordreField:BUFFER-VALUE THEN
        hHasteordreOverlay:BGCOLOR = hHasteordreColumn:BGCOLOR.
      ELSE hHasteordreOverlay:BGCOLOR = ?.
    END.
  END.

  IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowseOrdre AND sokOrdreArtikkel:SCREEN-VALUE IN FRAME frmOrdre NE "0" THEN DO:
    bOk = hBrowseOrdreBest:QUERY:GET-BUFFER-HANDLE(1):FIND-FIRST("WHERE Artikkelnr = " +  sokOrdreArtikkel:SCREEN-VALUE) NO-ERROR.
    IF bOk THEN DO:
      hBrowseOrdreBest:SET-REPOSITIONED-ROW(3,"conditional").  
      hBrowseOrdreBest:QUERY:REPOSITION-TO-ROWID(hBrowseOrdreBest:QUERY:GET-BUFFER-HANDLE(1):ROWID) NO-ERROR.
      APPLY "value-changed" TO hBrowseOrdreBest.
    END.
  END.
  ELSE IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowseOrdre AND sokOrdreLevKod:SCREEN-VALUE IN FRAME frmOrdre NE "" THEN DO:
    bOk = hBrowseOrdreBest:QUERY:GET-BUFFER-HANDLE(1):FIND-FIRST("WHERE LevKod BEGINS '" +  sokOrdreLevKod:SCREEN-VALUE + "'") NO-ERROR.
    IF bOk THEN DO:
      hBrowseOrdreBest:SET-REPOSITIONED-ROW(3,"conditional").  
      hBrowseOrdreBest:QUERY:REPOSITION-TO-ROWID(hBrowseOrdreBest:QUERY:GET-BUFFER-HANDLE(1):ROWID) NO-ERROR.
      APPLY "value-changed" TO hBrowseOrdreBest.
    END.
  END.
  ELSE IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowseOrdre AND sokOrdreLevFargKod:SCREEN-VALUE IN FRAME frmOrdre NE "" THEN DO:
    bOk = hBrowseOrdreBest:QUERY:GET-BUFFER-HANDLE(1):FIND-FIRST("WHERE LevFargKod BEGINS '" +  sokOrdreLevFargKod:SCREEN-VALUE + "'") NO-ERROR.
    IF bOk THEN DO:
      hBrowseOrdreBest:SET-REPOSITIONED-ROW(3,"conditional").  
      hBrowseOrdreBest:QUERY:REPOSITION-TO-ROWID(hBrowseOrdreBest:QUERY:GET-BUFFER-HANDLE(1):ROWID) NO-ERROR.
      APPLY "value-changed" TO hBrowseOrdreBest.
    END.
  END.

END.

IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowseListe THEN DO WITH FRAME {&FRAME-NAME}:
  DYNAMIC-FUNCTION("DoLockWindow",THIS-PROCEDURE:CURRENT-WINDOW).
  IF hFieldMap:AVAIL THEN DO:
    ASSIGN KortNavn:SCREEN-VALUE           = hFieldMap:BUFFER-FIELD("KortNavn"):BUFFER-VALUE
           MesseBeskrivelse:SCREEN-VALUE   = hFieldMap:BUFFER-FIELD("MesseBeskrivelse"):BUFFER-VALUE
           MesseNr:SCREEN-VALUE            = hFieldMap:BUFFER-FIELD("MesseNr"):STRING-VALUE
           ProfilNr:SCREEN-VALUE           = hFieldMap:BUFFER-FIELD("ProfilNr"):STRING-VALUE
           VarebehNr:SCREEN-VALUE          = hFieldMap:BUFFER-FIELD("VarebehNr"):STRING-VALUE
           VarebehType:SCREEN-VALUE        = hFieldMap:BUFFER-FIELD("VarebehType"):STRING-VALUE
           VarebehBeskrivelse:SCREEN-VALUE = hFieldMap:BUFFER-FIELD("VarebehBeskrivelse"):STRING-VALUE
           .
    IF VarebehType:SCREEN-VALUE = '1' THEN 
        fi-ModusTekst:SCREEN-VALUE IN FRAME {&FRAME-NAME} 
                       = DYNAMIC-FUNCTION("getFieldList","SysPara;Parameter2",
                                "WHERE SysHId = 5 and SysGr = 24 and ParaNr = 1").
    ELSE IF VarebehType:SCREEN-VALUE = '2' THEN 
        fi-ModusTekst:SCREEN-VALUE IN FRAME {&FRAME-NAME}
                       = DYNAMIC-FUNCTION("getFieldList","SysPara;Parameter1",
                                "WHERE SysHId = 5 and SysGr = 24 and ParaNr = 1").
  END.
  ELSE
    ASSIGN KortNavn:SCREEN-VALUE           = ""
           MesseBeskrivelse:SCREEN-VALUE   = ""
           MesseNr:SCREEN-VALUE            = ""
           ProfilNr:SCREEN-VALUE           = ""
           VarebehNr:SCREEN-VALUE          = ""
           Varebehtype:SCREEN-VALUE        = ""
           VarebehBeskrivelse:SCREEN-VALUE = ""
           fi-ModusTekst:SCREEN-VALUE      = ""
           .
  /*
  IF fi-ModusTekst:SCREEN-VALUE IN FRAME {&FRAME-NAME} NE "" 
      THEN fi-ModusTekst:BGCOLOR = 12.
  ELSE fi-ModusTekst:BGCOLOR = ?.
  */
  iBestVarebeh = INT(VarebehType:SCREEN-VALUE).
  
  DYNAMIC-FUNCTION("DoLockWindow",?).
END.

IF iTab = 3 THEN DO: 
  IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowseBestHode THEN DO:
    IF hFieldMap:AVAIL AND hFieldMap:BUFFER-FIELD("VareBehType"):BUFFER-VALUE = iBestVarebeh 
       AND hFieldMapBestHode:AVAIL 
       AND hFieldMapBestHode:BUFFER-FIELD("BestNr"):BUFFER-VALUE = 0 THEN 
      DirekteLev:CHECKED IN FRAME frmLinje = LOGICAL(cDefaultDirLev).
  
    SettLevDatoEllerDag().
  END.
  IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowseLinje THEN DO:
    DYNAMIC-FUNCTION("setAttribute",SESSION,"userkeepswindowlocked","").
    DYNAMIC-FUNCTION("DoLockWindow",?).
  END.
END.

IF hFieldMap:AVAIL THEN
  SettHentUtvalgSensitive(IF VALID-HANDLE(hUtvalg) AND NOT hFieldMap:BUFFER-FIELD("Oppdatert"):BUFFER-VALUE THEN TRUE ELSE FALSE).

DYNAMIC-FUNCTION("setAttribute",SESSION,"userkeepswindowlocked","").
DYNAMIC-FUNCTION("DoLockWindow",?).

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
IF iTab = 1 THEN APPLY 'default-action':U TO hBrowseListe.
ELSE IF iTab = 3 AND DYNAMIC-FUNCTION("getCurrentObject") = hUpdToolbar THEN DO:
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN ArtBasVedlikehold.w (THIS-PROCEDURE,"edit",OUTPUT bOk).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.
  THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
  APPLY "entry" TO hBrowseLinje.
  
  IF bOK THEN DO:
      IF INT(VarebehType:SCREEN-VALUE IN FRAME DEFAULT-FRAME) = 1 THEN
      DO:
          DYNAMIC-FUNCTION("refreshRowids",hBrowseLinje,hBrowseLinje:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("RowIdent1"):BUFFER-VALUE).
      END.
      ELSE DO:
          IF DYNAMIC-FUNCTION("runproc","update_varebeh_from_artbas.p",
                              STRING(hBrowseLinje:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) + "," + 
                              STRING(hBrowseLinje:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("VarebehNr"):BUFFER-VALUE)
                              ,?) THEN 
            DYNAMIC-FUNCTION("refreshRowids",hBrowseLinje,hBrowseLinje:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("RowIdent1"):BUFFER-VALUE).
          ELSE
            DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("GetTransactionMessage"),"Feil","").
      END.
  END.    
END.
ELSE IF iTab = 4 THEN
  RUN Artikkelkort.

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
  DISPLAY VarebehNr VarebehBeskrivelse VarebehType Beskrivelse MesseNr 
          MesseBeskrivelse ProfilNr KortNavn fi-ModusTekst 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE rectToolBar rectUpdToolbar rectVarebeh VarebehNr VarebehBeskrivelse 
         Beskrivelse MesseNr MesseBeskrivelse KortNavn fi-ModusTekst 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  DISPLAY cmbVarebehType fi-fVarebehNr fi-fMesseNr fi-fProfilNr 
          fi-cVarebehBeskrivelse fi-cEkstRef ListeSokLevNr sokGlobOrdrenr 
          ListeSokLevNamn sokEkstId tbKundeOrdre tbMatchBestStat sokBestStat 
      WITH FRAME frmFilter IN WINDOW C-Win.
  ENABLE RectArtSok cmbVarebehType fi-fVarebehNr fi-fMesseNr fi-fProfilNr 
         btnSokFiltMesseNr btnSokFiltProfilNr fi-cVarebehBeskrivelse 
         fi-cEkstRef ListeSokLevNr sokGlobOrdrenr btnLev-2 sokEkstId 
         tbKundeOrdre tbMatchBestStat btnBlankFilter 
      WITH FRAME frmFilter IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-frmFilter}
  DISPLAY VarebehNr OppdatDato OppdatAv Oppdatert VarebehType 
          BeskrivelseVareBehType ProfilNr KortNavn ButikkListe 
          VarebehBeskrivelse EkstRef MesseNr MesseBeskrivelse Kilde 
          VareBokBeskrivelse VarebehNotat 
      WITH FRAME frmDetalj IN WINDOW C-Win.
  ENABLE btnSokMesseNr RECT-1 Oppdatert VarebehType BeskrivelseVareBehType 
         ProfilNr KortNavn ButikkListe btnButikker VarebehBeskrivelse EkstRef 
         MesseNr Kilde btnVareBokNr VareBokBeskrivelse VarebehNotat 
         btnSokProfilNr 
      WITH FRAME frmDetalj IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-frmDetalj}
  DISPLAY tbOpprettBest sokSaSong sokRegBestStat sokLevKod rsLagerStatusSalg 
          SumVarekost DirekteLev LevDato tbUpdateStat sokAvdelingNr 
          sokAvdelingNavn sokHg sokHgBeskr sokVg sokVgBeskr sokAktNr 
          sokAktBeskrivelse sokLevNr sokLevNamn sokArtikkelNr sokBeskr 
          sokFraEdato sokTilEdato sokFraVPIdato sokTilVPIdato 
          sokFraRegDatoBestInnl sokTilRegDatoBestInnl Kode tbRepeat SumPris 
          sokLinjeMerknad sokSasBeskr 
      WITH FRAME frmLinje IN WINDOW C-Win.
  ENABLE BUTTON-Paste tbOpprettBest sokSaSong sokRegBestStat btnTotal sokLevKod 
         rsLagerStatusSalg SumVarekost DirekteLev LevDato tbUpdateStat 
         sokAvdelingNr sokAvdelingNavn sokHg btnSplitBarY sokHgBeskr sokVg 
         sokVgBeskr sokAktNr sokAktBeskrivelse sokLevNr sokLevNamn 
         sokArtikkelNr sokBeskr sokFraEdato sokTilEdato sokFraVPIdato 
         sokTilVPIdato sokFraRegDatoBestInnl sokTilRegDatoBestInnl Kode 
         btnAvdeling btnHuvGr btnVarGr btnBlankLinjeFilter btnVarGr-2 tbRepeat 
         btnLev btnLevDatoDate SumPris btnLinjemerknad sokSasBeskr btnSaSong 
         btnLev2 btnAvdeling2 btnHuvGr2 btnVarGr2 RectBrowseSearchLinje 
         rectBrowseLinje rectBrwBestHode rectBrwAndreBest ArtikkelBilde 
         rectBestHodeToolbar rectSettings 
      WITH FRAME frmLinje IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-frmLinje}
  DISPLAY sokBestOrdreStatus sokOrdreNr sokEkstId sokOrdreLevDato sokOrdreCL 
          SokOrdreCLNamn SokOrdreLevNr SokOrdreLevNamn sokOrdreArtikkel 
          SokOrdreArtBeskr sokOrdreLevKod sokOrdreLevFargKod sokOrdremottaker 
          sokOpphav cmbKjedevare cmbGjennomfaktureres 
      WITH FRAME frmOrdre IN WINDOW C-Win.
  ENABLE rectBrowseOrdre BrwOrdreBest BrwOrdreBestStr BrwOrdreBestLev 
         OrdreToolbar BestToolbar BestBilde sokBestOrdreStatus sokOrdreNr 
         sokEkstId sokOrdreLevDato sokOrdreCL btnOrdreLev SokOrdreLevNr 
         btnLev-3 btnBlankOrdreFilter sokOrdreArtikkel btnOrdreArtikkel 
         sokOrdreLevKod sokOrdreLevFargKod sokOrdremottaker sokOpphav 
         cmbKjedevare cmbGjennomfaktureres 
      WITH FRAME frmOrdre IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-frmOrdre}
  ENABLE btnOk btnAvbryt 
      WITH FRAME frmOkReg IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-frmOkReg}
  ENABLE rectBrowseListe RectBrowseSearchListe 
      WITH FRAME frmListe IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-frmListe}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EndErrorBrowseOverlay C-Win 
PROCEDURE EndErrorBrowseOverlay :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
APPLY "return" TO sokLevNr IN FRAME frmLinje.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EndreOrdreLevDatoRecord C-Win 
PROCEDURE EndreOrdreLevDatoRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN OrdreBehandling("levdato").
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
DEF VAR cFileName           AS CHAR       NO-UNDO.
DEF VAR chExcelApplication  AS COM-HANDLE NO-UNDO.
DEF VAR chWorkSheet         AS COM-HANDLE NO-UNDO.

IF DYNAMIC-FUNCTION("getCurrentObject") = hOrdreToolbar THEN DO:
  IF hFieldMap:AVAIL THEN DO:
    IF DYNAMIC-FUNCTION("runProc","butvarebeh_sammendrag.p",
                         DYNAMIC-FUNCTION("getAttribute",hBrowseOrdre,"basequery")
                       + DYNAMIC-FUNCTION("getAttribute",hBrowseOrdre,"queryfilter")
                       + DYNAMIC-FUNCTION("getAttribute",hBrowseOrdre,"querywhere") + "¤"
                       + STRING(hFieldMap:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE) + "¤"
                       + cButikkListe + "¤"
                       + STRING(bHKinst AND cButikkListe = "*") + "¤"
                       + (IF sokBestOrdreStatus:SCREEN-VALUE IN FRAME frmOrdre NE ? THEN sokBestOrdreStatus:SCREEN-VALUE ELSE "") + "¤"
                       + (IF cmbKjedevare:SCREEN-VALUE NE ? THEN cmbKjedevare:SCREEN-VALUE ELSE "") + "¤"
                       + (IF cmbGjennomfaktureres:SCREEN-VALUE NE ? THEN cmbGjennomfaktureres:SCREEN-VALUE ELSE "")
                        ,httRapport) THEN DO:
      cFileName = getReportFile(DYNAMIC-FUNCTION("getTransactionMessage")).
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
END.
ELSE RUN SUPER.
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
DEF VAR iReturn  AS INT NO-UNDO.

/* IF iTab < 3 THEN                                                                                                                                              */
/*   iReturn = DYNAMIC-FUNCTION("DoMessage",0,1,"Det kan ta til å hente alle bestillinger eller varemottak for aktuelle vareh.bøker - vil du fortsette?","",""). */
/* ELSE                                                                                                                                                          */
  
iReturn = 1.

IF iReturn NE 1 THEN DO:
  obOK = FALSE.
  APPLY "close" TO ihFlatView.
  RETURN.
END.

hFlatBrw = DYNAMIC-FUNCTION("getBrowseHandle" IN ihFlatView).
DYNAMIC-FUNCTION("setAttribute",hFlatBrw,"getrecordcount","no").

/* DYNAMIC-FUNCTION("setUseLocalData" IN ihFlatView,TRUE).  */
DYNAMIC-FUNCTION("setExcludeFields" IN ihFlatView,
                 (IF iBestVarebeh = 1 THEN cInnlevViewFields + ",+BatchNr" ELSE cInnlevViewFields) + ","
               + "EkstRef,Oppdatert,OppdatDato,EDato,BrukerId,+BestHodeRtid,RegistrertDato,RegistrertTid,"
               + "+BestHodeRtid"
               + ",+SumAntall,+SumVarekost,+SumPris"
                 ).


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

DEF VAR cVHBknytning AS CHAR NO-UNDO.
DEF VAR cOrdKnytning AS CHAR NO-UNDO.

IF DYNAMIC-FUNCTION("getCurrentObject") = hUpdToolbar THEN DO:
  
  IF iTab < 3 THEN DO:
    RUN JBoxDSimpleSelectList.w ("Inkluder artikler (og bestillinger)|Artikkel|Inkluder ordre og bestillinger|Ordre|Kun vareh.bøker|Ingen",
                                 ?,OUTPUT cVHBknytning).
    IF cVHBknytning = "Artikkel" THEN DO:
      DYNAMIC-FUNCTION("createParentLink",hBrowseLinje,hBrowseListe,'VarebehNr').
      DYNAMIC-FUNCTION("DeleteObjectLink",hBrowseLinje,hBrowseAndreBest).
    END.
    ELSE IF cVHBknytning = "Ordre" THEN DO:
      DYNAMIC-FUNCTION("createParentLink",hBrowseOrdre,hBrowseListe,'VarebehNr').
      RUN JBoxDSimpleSelectList.w ("Bestillinger pr størrelse|Bestilling|Varemottak|Varemottak|Kun bestilling|Ingen",
                                   ?,OUTPUT cOrdKnytning).
      IF cOrdKnytning = "Bestilling" THEN 
        DYNAMIC-FUNCTION("DeleteObjectLink",hBrowseOrdreBest,hBrowseOrdreBestLev).
      ELSE IF cOrdKnytning = "Varemottak" THEN 
        DYNAMIC-FUNCTION("DeleteObjectLink",hBrowseOrdreBest,hBrowseOrdreBestStr).
      ELSE IF cOrdKnytning = "Ingen" THEN DO:
        DYNAMIC-FUNCTION("DeleteObjectLink",hBrowseOrdreBest,hBrowseOrdreBestLev).
        DYNAMIC-FUNCTION("DeleteObjectLink",hBrowseOrdreBest,hBrowseOrdreBestStr).
      END.
      ELSE RETURN.
    END.
      
  END.
  ELSE IF iTab = 3 THEN
    DYNAMIC-FUNCTION("DeleteObjectLink",hBrowseLinje,hBrowseAndreBest).

  RUN SUPER.
  IF iTab < 3 AND cVHBknytning NE "Ingen" THEN DO:
    IF cVHBknytning = "Artikkel" THEN DO:
      DYNAMIC-FUNCTION("DeleteObjectLink",hBrowseLinje,hBrowseListe).
      DYNAMIC-FUNCTION("CreateParentLink",hBrowseAndreBest,hBrowseLinje,"ArtikkelNr").
    END.
    ELSE IF cVHBknytning = "Ordre" THEN DO:
      DYNAMIC-FUNCTION("DeleteObjectLink",hBrowseOrdre,hBrowseListe).
      IF cOrdKnytning = "Bestilling" THEN 
        DYNAMIC-FUNCTION("CreateParentLink",hBrowseOrdreBestLev,hBrowseOrdreBest,"BestNr").
      ELSE IF cOrdKnytning = "Varemottak" THEN 
        DYNAMIC-FUNCTION("CreateParentLink",hBrowseOrdreBestStr,hBrowseOrdreBest,"BestNr").
      ELSE DO:
        DYNAMIC-FUNCTION("CreateParentLink",hBrowseOrdreBestLev,hBrowseOrdreBest,"BestNr").
        DYNAMIC-FUNCTION("CreateParentLink",hBrowseOrdreBestStr,hBrowseOrdreBest,"BestNr").
      END.
    END.
  END.      
  ELSE IF iTab = 3 THEN
    DYNAMIC-FUNCTION("CreateParentLink",hBrowseAndreBest,hBrowseLinje,"ArtikkelNr").

  hFlatView = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hUpdToolBar,"flatviewhandle")) NO-ERROR.
  IF NOT VALID-HANDLE(hFlatView) THEN RETURN.
END.

ELSE IF DYNAMIC-FUNCTION("getCurrentObject") = hOrdreToolbar THEN DO:
  RUN JBoxDSimpleSelectList.w ("Bestillinger pr størrelse|Bestilling|Varemottak|Varemottak|Kun bestilling|Ingen",
                               ?,OUTPUT cOrdKnytning).
  IF cOrdKnytning = "Bestilling" THEN 
    DYNAMIC-FUNCTION("DeleteObjectLink",hBrowseOrdreBest,hBrowseOrdreBestLev).
  ELSE IF cOrdKnytning = "Varemottak" THEN 
    DYNAMIC-FUNCTION("DeleteObjectLink",hBrowseOrdreBest,hBrowseOrdreBestStr).
  ELSE IF cOrdKnytning = "Ingen" THEN DO:
    DYNAMIC-FUNCTION("DeleteObjectLink",hBrowseOrdreBest,hBrowseOrdreBestLev).
    DYNAMIC-FUNCTION("DeleteObjectLink",hBrowseOrdreBest,hBrowseOrdreBestStr).
  END.
  ELSE RETURN.

  RUN SUPER.

  IF cOrdKnytning = "Bestilling" THEN 
    DYNAMIC-FUNCTION("CreateParentLink",hBrowseOrdreBestLev,hBrowseOrdreBest,"BestNr").
  ELSE IF cOrdKnytning = "Varemottak" THEN 
    DYNAMIC-FUNCTION("CreateParentLink",hBrowseOrdreBestStr,hBrowseOrdreBest,"BestNr").
  ELSE DO:
    DYNAMIC-FUNCTION("CreateParentLink",hBrowseOrdreBestLev,hBrowseOrdreBest,"BestNr").
    DYNAMIC-FUNCTION("CreateParentLink",hBrowseOrdreBestStr,hBrowseOrdreBest,"BestNr").
  END.

  hFlatView = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hOrdreToolbar,"flatviewhandle")) NO-ERROR.
  IF NOT VALID-HANDLE(hFlatView) THEN RETURN.
END.

hFlatBrw  = DYNAMIC-FUNCTION("getBrowseHandle" IN hFlatView).
{incl/dynfilterlookups_art.i}

DYNAMIC-FUNCTION("setAttribute",hFlatBrw,"availdistinctcolumns",
                  "VarebehNr,ArtikkelNr,ButikkNr,BestNr,Beskr,levKod,LeveringsDato,LevNr,LevNamn,Vg,VgBeskr,Hg,HgBeskr,AvdelingsNr,AvdelingsNavn,Ordrenr,OrdreStatus,CL").
DYNAMIC-FUNCTION("setAttribute",hFlatBrw,"availaccumfields",
                  "SumAntall,SumVarekost,SumPris,SumDB,TotAntPar,TotInnkjVerdi,TotSalgsVerdi,TotInnLev,TotMakulert").

DYNAMIC-FUNCTION("setAttribute",hFlatBrw,"accumdatatypes","DECIMAL,INTEGER").
DYNAMIC-FUNCTION("setAttribute",hFlatBrw,"distinctdatatypes","CHARACTER,DECIMAL,INTEGER,DATE").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE genBestRecord C-Win 
PROCEDURE genBestRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
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

/* IF bOk THEN DO:                                                                                                                  */
/*   IF NUM-ENTRIES(cLevBasRowidList) > 1 THEN                                                                                      */
/*     ASSIGN sokLevNr:SCREEN-VALUE   = "0"                                                                                         */
/*            sokLevNamn:SCREEN-VALUE = STRING(NUM-ENTRIES(cLevBasRowidList)) + " av " +                                            */
/*                                      STRING(iSelectorSourcCount)                                                                 */
/*                                      .                                                                                           */
/*   ELSE                                                                                                                           */
/*     ASSIGN sokLevNr:SCREEN-VALUE   = cLevBasIdList                                                                               */
/*            sokLevNamn:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","LevBas;LevNamn","WHERE LevNr = " + sokLevNr:SCREEN-VALUE). */
/*   RUN OpenQuery.                                                                                                                 */
/* END.                                                                                                                             */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getArtKortParam C-Win 
PROCEDURE getArtKortParam :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT  PARAM icMode   AS CHAR NO-UNDO.
DEF OUTPUT PARAM oiLevnr  AS INT NO-UNDO.
DEF OUTPUT PARAM orRecId  AS RECID NO-UNDO. 

IF icMode = "NY" THEN DO WITH FRAME frmLinje:
  orRecid = DYNAMIC-FUNCTION("getRecId","ArtBas",DYNAMIC-FUNCTION("getFieldValues","ArtBas","WHERE lager","ROWID")).
  oiLevNr = INT(sokLevNr:SCREEN-VALUE).
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
DEF OUTPUT PARAM ocFarge AS CHAR NO-UNDO INIT "butvarebeh".
DEF OUTPUT PARAM ocKrit  AS CHAR NO-UNDO.

IF iTab LE 2 THEN DO WITH FRAME frmFilter:
  ocKrit = cmbVarebehType:SCREEN-VALUE + ",".
  IF tbMatchBestStat:CHECKED THEN
    ocKrit = ocKrit + sokBestStat:SCREEN-VALUE.
END.
ELSE IF hFieldMap:AVAIL THEN 
  ocKrit = STRING(hFieldMap:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE).

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

IF bSetBestPanel THEN DO:
  IF VALID-HANDLE(hSelectorPanel) THEN
    cRappBestStatusList = DYNAMIC-FUNCTION("getBestStatusList" IN hSelectorPanel).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getVareSlagExceptList C-Win 
PROCEDURE getVareSlagExceptList :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF OUTPUT PARAM cExceptList AS CHAR NO-UNDO.

cExceptList = REPLACE(DYNAMIC-FUNCTION("getFieldList","SysPara;Parameter1","WHERE SysHId = 2 and SysGr = 8"),"|",",").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HasteOrdreRecord C-Win 
PROCEDURE HasteOrdreRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cHaster       AS CHAR NO-UNDO.
DEF VAR iReturn       AS INT  NO-UNDO.
DEF VAR cOrdreNrList  AS CHAR NO-UNDO.
DEF VAR cMsg          AS CHAR NO-UNDO.

RUN JBoxBrowseMsgUpdSelVal.w ("Sett haste-ordre markering for valgte ordre",
                              hBrowseOrdre:NUM-SELECTED-ROWS,
                              INT(DYNAMIC-FUNCTION("getAttribute",hBrowseOrdre,"recordcount")),
                              "logical|Ja/Nei|Ja",
                              OUTPUT cHaster,
                              OUTPUT iReturn).

IF iReturn = 0 THEN RETURN.


IF iReturn = 2 THEN DO: /* Behandle valgte */
  DO ix = 1 TO hBrowseOrdre:NUM-SELECTED-ROWS:
    IF hBrowseOrdre:FETCH-SELECTED-ROW(ix) THEN
      cOrdreNrList = cOrdreNrList + STRING(hBrowseOrdre:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("OrdreNr"):BUFFER-VALUE) + "|".
  END.

  bOk = DYNAMIC-FUNCTION("runproc","ordre_sett_til_hasteordre.p",
                         TRIM(cOrdreNrList,"|") 
                         + "," + cHaster 
                         ,?).
  cMsg = DYNAMIC-FUNCTION("getTransactionMessage").
  IF bOk THEN DO:
    DYNAMIC-FUNCTION("setCurrentObject",hBrowseOrdre).
    RUN OpenQuery.
  END.
  IF cMsg NE "" THEN
    DYNAMIC-FUNCTION("DoMessage",0,0,cMsg,"","").
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
DEF VAR cType     AS CHAR NO-UNDO.

cType = cmbVarebehType:SCREEN-VALUE IN FRAME frmFilter.
IF NOT CAN-DO("1,2",cType) THEN cType = "1".

cFileName = ".\hlp\" + SUBSTR(THIS-PROCEDURE:FILE-NAME,1,INDEX(THIS-PROCEDURE:FILE-NAME,".") - 1) + 
            cType + ".htm".

IF SEARCH(cFileName) NE ? THEN DO:
  FILE-INFO:FILE-NAME = cFileName.
  DYNAMIC-FUNCTION("setWebDoc","open",FILE-INFO:FULL-PATHNAME).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HentArtiklerRecord C-Win 
PROCEDURE HentArtiklerRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DO WITH FRAME frmLinje:
  IF VALID-HANDLE(hArtBasSok) THEN APPLY "close" TO hArtBasSok.

  RUN ArtBasSok.w PERSIST SET hArtBasSok.
/*   DYNAMIC-FUNCTION("setButikkNr" IN hArtBasSok,hParentBuffer:BUFFER-FIELD("ButikkNr"):BUFFER-VALUE).   */
/*   DYNAMIC-FUNCTION("setOrdreId"  IN hArtBasSok,hParentBuffer:BUFFER-FIELD("KOrdre_Id"):BUFFER-VALUE).  */
  DYNAMIC-FUNCTION("setCloseOnSelect" IN hArtBasSok,YES).
  DYNAMIC-FUNCTION("setUpdateCurrentRow" IN hArtBasSok,YES).
  DYNAMIC-FUNCTION("setVarebehNr" IN hArtBasSok,hFieldMap:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE).
  RUN InitializeObject IN hArtBasSok.
  IF sokAvdelingNr:SCREEN-VALUE NE "0" THEN
    DYNAMIC-FUNCTION("setAvdNr" IN hArtBasSok,sokAvdelingNr:SCREEN-VALUE,NO).
  IF sokHg:SCREEN-VALUE NE "0" THEN
    DYNAMIC-FUNCTION("setHg" IN hArtBasSok,sokHg:SCREEN-VALUE,NO).
  IF sokVg:SCREEN-VALUE NE "0" THEN
    DYNAMIC-FUNCTION("setVg" IN hArtBasSok,sokVg:SCREEN-VALUE,NO).
  DYNAMIC-FUNCTION("setLevNr" IN hArtBasSok,sokLevNr:SCREEN-VALUE IN FRAME frmLinje,YES).
  RUN MoveToTop IN hArtBasSok.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hentModus C-Win 
PROCEDURE hentModus :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAMETER icModus AS CHAR NO-UNDO.

  DO WITH FRAME DEFAULT-FRAME:
    icModus = VarebehType:SCREEN-VALUE.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hentVarebehNr C-Win 
PROCEDURE hentVarebehNr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAMETER ilVarebehNr AS DEC NO-UNDO.

  DO WITH FRAME frmDetalj:
      ASSIGN ilVarebehNr = DEC(VarebehNr:SCREEN-VALUE).
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitGrid C-Win 
PROCEDURE InitGrid :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icStorrelser AS CHAR NO-UNDO.

DEF VAR cButiker      AS CHAR NO-UNDO.
DEF VAR iAntTmp       AS INT  NO-UNDO.   
DEF VAR iRowCount     AS INT  NO-UNDO.
DEF VAR cButikerInUse AS CHAR NO-UNDO.

ASSIGN chGrid:rows = 1
       chGrid:cols = 2.

IF icStorrelser = "clear" THEN RETURN.

IF hFieldMapBestHode:AVAIL THEN DO:

  EMPTY TEMP-TABLE ttSort.
/*   DO ix = 1 TO NUM-ENTRIES(hFieldMap:BUFFER-FIELD("ButikkListe"):BUFFER-VALUE): */
  DO ix = 1 TO NUM-ENTRIES(cButikkListe):
    CREATE ttSort.
/*     ttSort.iSeq = INT(ENTRY(ix,hFieldMap:BUFFER-FIELD("ButikkListe"):BUFFER-VALUE)). */
    ttSort.iSeq = INT(ENTRY(ix,cButikkListe)).
  END.
  FOR EACH ttSort BY iSeq:
    cButiker = cButiker + STRING(ttSort.iSeq) + ",".
  END.

  ASSIGN cButiker    = TRIM(cButiker,",")
         icStorrelser = REPLACE(icStorrelser," ","")
         chGrid:rows = 1 + NUM-ENTRIES(cButiker)
         chGrid:cols = 2 + NUM-ENTRIES(icStorrelser).

  DO ix = 1 TO NUM-ENTRIES(icStorrelser):
    ASSIGN chGrid:ColWidth(ix + 1) = 510
           chGrid:TextMatrix(0,ix + 1) = ENTRY(ix,icStorrelser) + " ".
  END.

  EMPTY TEMP-TABLE tt_VarebehBestLinje.
  DYNAMIC-FUNCTION("getTempTable","varebehlinje_best_innlev.p",STRING(hFieldMap:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE) + "," +
                                                               STRING(hFieldMapLinje:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) + "," +
                                                               STRING(hFieldMapBestHode:BUFFER-FIELD("HodeLinjeId"):BUFFER-VALUE),
                                                    hBuffTT_VarebehBestLinje).
  
  DO ix = 1 TO NUM-ENTRIES(cButiker):
    IF CAN-FIND(FIRST TT_VareBehBestLinje WHERE TT_VareBehBestLinje.BestiltButikkNr = INT(ENTRY(ix,cButiker))) 
       OR NOT CAN-FIND(FIRST TT_VareBehBestLinje) THEN
      ASSIGN iRowCount               = iRowCount + 1
             chGrid:TextMatrix(iRowCount,0) = ENTRY(ix,cButiker)
             chGrid:TextMatrix(iRowCount,1) = "0 "
             cButikerInUse                  = cButikerInUse + ENTRY(ix,cButiker) + ",".
  END.
  cButikerInUse = TRIM(cButikerInUse,",").

  FOR EACH TT_VareBehBestLinje:
    IF CAN-DO(cButiker,STRING(TT_VareBehBestLinje.BestiltButikkNr)) AND CAN-DO(icStorrelser,TT_VareBehBestLinje.Storl) THEN DO:
      chGrid:TextMatrix(LOOKUP(STRING(TT_VareBehBestLinje.BestiltButikkNr),cButikerInUse),LOOKUP(TT_VareBehBestLinje.Storl,icStorrelser) + 1) = STRING(TT_VareBehBestLinje.Bestilt) + " ".
      iAntTmp = INT(chGrid:TextMatrix(LOOKUP(STRING(TT_VareBehBestLinje.BestiltButikkNr),cButikerInUse),1)).
      iAntTmp = iAntTmp + TT_VareBehBestLinje.Bestilt.
      chGrid:TextMatrix(LOOKUP(STRING(TT_VareBehBestLinje.BestiltButikkNr),cButikerInUse),1) = STRING(iAntTmp) + " ".
    END.
  END.

  IF chGrid:rows > 1 AND chGrid:cols > 2 THEN
    ASSIGN chGrid:ROW = 1
           chGrid:COL = 2
           NO-ERROR.
END.


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
DEFINE VAR iIdx        AS INTE NO-UNDO.
DEFINE VAR iRow        AS INTE NO-UNDO.
DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.
DEFINE VARIABLE iAntTmp AS INTEGER    NO-UNDO.
ASSIGN chGrid = chGrid:vsFlexGrid.

ASSIGN chGrid:CellPictureAlignment = 1
       chGrid:Redraw = FALSE. /* disable repaint while populating */
chGrid:Clear().

ASSIGN chGrid:AllowUserResizing = 0 /* user may resize columns/rows */
       chGrid:Enabled = TRUE  /* make it an updateable Grid */
       chGrid:AllowBigSelection = FALSE 
       chGrid:AllowSelection    = FALSE 
       chGrid:Appearance = 1 
       chGrid:Rows = 2
       chGrid:Cols = 3
       chGrid:FixedRows = 1
       chGrid:FixedCols = 2
       chGrid:HonorProKeys = FALSE
       chGrid:TextStyle = 0
       chGrid:TextStyleFixed = 0
       chGrid:ColWidth(1) = 615.

/*     chGrid:Cell(6,0,0,0,chGrid:Cols - 1) = 16777215. */
ASSIGN chGrid:TextMatrix(0,0) = "Butikk "
       chGrid:TextMatrix(0,1) = "Total ".

ASSIGN iIdx = 0.
/* DO iIdx = 1 TO NUM-ENTRIES(cStorrelser):                                                                                                                                               */
/*     ASSIGN chGrid:ColWidth(iIdx + 1) = 510                                                                                                                                             */
/*            chGrid:TextMatrix(0,iIdx + 1) = ENTRY(iIdx,cStorrelser) + " ".                                                                                                              */
/* END.                                                                                                                                                                                   */
/* DO iCount = 1 TO iAntBut:                                                                                                                                                              */
/*     ASSIGN iRow = iCount                                                                                                                                                               */
/*            chGrid:TextMatrix(iRow,0) = ENTRY(iCount,cButiker)                                                                                                                          */
/*            chGrid:TextMatrix(iRow,1) = "0 ".                                                                                                                                           */
/* END.                                                                                                                                                                                   */
ASSIGN chGrid:Row = 1
       chGrid:Col = 2
       chGrid:Redraw = TRUE. /* disable repaint while populating */
/* FOR EACH TT_VareBehBestLinje: */
/*     IF CAN-DO(cButiker,STRING(TT_VareBehBestLinje.BestiltButikkNr)) AND CAN-DO(cStorrelser,TT_VareBehBestLinje.Storl) THEN DO:                                                         */
/*         chGrid:TextMatrix(LOOKUP(STRING(TT_VareBehBestLinje.BestiltButikkNr),cButiker),LOOKUP(TT_VareBehBestLinje.Storl,cStorrelser) + 1) = STRING(TT_VareBehBestLinje.Bestilt) + " ". */
/*         iAntTmp = INT(chGrid:TextMatrix(LOOKUP(STRING(TT_VareBehBestLinje.BestiltButikkNr),cButiker),1)).                                                                              */
/*         iAntTmp = iAntTmp + TT_VareBehBestLinje.Bestilt.                                                                                                                               */
/*         chGrid:TextMatrix(LOOKUP(STRING(TT_VareBehBestLinje.BestiltButikkNr),cButiker),1) = STRING(iAntTmp) + " ".                                                                     */
/*     END.                                                                                                                                                                               */
/* END.                                                                                                                                                                                   */

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
DEF VAR cHKinst   AS CHAR NO-UNDO.

IF bAlreadyInitialized THEN RETURN.

bAlreadyInitialized = YES.

RUN enable_UI.

setButikkListe().

DYNAMIC-FUNCTION("NewObject",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE,"procedure").

SetTabStrip().

ASSIGN cAdgang         = DYNAMIC-FUNCTION("getFieldValues","SysPara",
                            "WHERE SysHId = 16 and SysGr = 39 and ParaNr = 2","Parameter1")
       cCL             = DYNAMIC-FUNCTION("getFieldValues","SysPara",
                            "WHERE SysHId = 5 and SysGr = 1 and ParaNr = 1","Parameter1")
       cBestTypeDesc   = DYNAMIC-FUNCTION("getFieldValues","SysPara",
                            "WHERE SysHId = 5 and SysGr = 6 and ParaNr = 1","Parameter1,Beskrivelse")
       iBestVarebeh    = INT(ENTRY(1,cBestTypeDesc,"|"))
       cInnlevTypeDesc = DYNAMIC-FUNCTION("getFieldValues","SysPara",
                            "WHERE SysHId = 5 and SysGr = 6 and ParaNr = 2","Parameter1,Beskrivelse")
       cAlle           = DYNAMIC-FUNCTION("getFieldValues","SysPara",
                            "WHERE SysHId = 1 and SysGr = 100 and ParaNr = 1","Parameter1")
       cHKinst         = DYNAMIC-FUNCTION("getFieldValues","SysPara",
                            "WHERE SysHId = 1 and SysGr = 1 and ParaNr = 18","Parameter1")
       bHKinst         = IF CAN-DO("1,yes,true",cHKinst) THEN TRUE ELSE FALSE
       cInitPrisProfil = DYNAMIC-FUNCTION("getFieldValues","Butiker",
                            "WHERE Butik = " + cCl,"ProfilNr")
       cDefaultDirLev  = DYNAMIC-FUNCTION("getFieldValues","SysPara",
                            "WHERE SysHId = 5 and SysGr = 4 and ParaNr = 12","Parameter1")
       bStrekkodeReg   = IF DYNAMIC-FUNCTION("getFieldValues","SysPara",
                            "WHERE SysHId = 5 and SysGr = 4 and ParaNr = 16","Parameter1") = "1" THEN TRUE ELSE FALSE
       iFontWingdings  = DYNAMIC-FUNCTION("setAppFont","Wingdings, size=11 Script=symbol","")
       /*
       fi-ModusTekst:SCREEN-VALUE IN FRAME {&FRAME-NAME} 
                       = DYNAMIC-FUNCTION("getFieldList","SysPara;Parameter1",
                                "WHERE SysHId = 5 and SysGr = 24 and ParaNr = 1")
       */

/*        bHKinst = NO  */
       FRAME frmOkReg:HIDDEN = YES
       .  
IF cDefaultDirLev = "1" THEN cDefaultDirLev = "yes".
ELSE IF cDefaultDirLev = "0" THEN cDefaultDirLev = "no".

IF cAlle = "" THEN
  ASSIGN cAlle = "[Alle]".

IF cButikkListe = "" AND DYNAMIC-FUNCTION("runproc","mine_butikker.p",DYNAMIC-FUNCTION("getASuserId") + ",2",?) THEN
  cButikkListe = DYNAMIC-FUNCTION("getTransactionMessage").

IF cButikkListe = "" THEN cButikkListe = "*".
cTigjButikker = cButikkListe.

/* Finner CL som bruker er koblet til. */
IF cButikkListe NE "*" THEN DO:
  IF DYNAMIC-FUNCTION("runproc","min_butikk_cl.p",cButikkListe,?) THEN 
    cButCL = DYNAMIC-FUNCTION("getTransactionMessage").
  ELSE DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
END.
ELSE 
  cButCl = cCL.

IF cButikkListe NE "*" AND bHKinst THEN 
  ASSIGN sokBestStat:DELIMITER IN FRAME frmFilter = "|"
         sokBestStat:LIST-ITEM-PAIRS = DYNAMIC-FUNCTION("getFieldList","SysPara;Parameter2;Parameter1","WHERE SysHId = 21 AND SysGr = 100 BY Parameter1")
         .
ELSE
  ASSIGN sokBestStat:DELIMITER IN FRAME frmFilter = "|"
         sokBestStat:LIST-ITEM-PAIRS = DYNAMIC-FUNCTION("getFieldList","SysPara;Parameter2;Parameter1","WHERE SysHId = 21 AND SysGr = 101 BY Parameter1")
         .
/*   ASSIGN sokBestStat:DELIMITER IN FRAME frmFilter = "|"                                                                                                     */
/*          sokBestStat:LIST-ITEM-PAIRS = DYNAMIC-FUNCTION("getFieldList","SysPara;ParaNr|Parameter1;ParaNr","WHERE SysHId = 5 AND SysGr = 2 AND ParaNr LE 7") */
/*          .                                                                                                                                                  */

DYNAMIC-FUNCTION("setAttribute",THIS-PROCEDURE,"bHKinst",STRING(bHKinst)).
DYNAMIC-FUNCTION("setAttribute",THIS-PROCEDURE,"cButikkListe",cButikkListe).
DYNAMIC-FUNCTION("setAttribute",THIS-PROCEDURE,"cButCL",cButCL).
DYNAMIC-FUNCTION("setAttribute",THIS-PROCEDURE,"cAdgang",cAdgang).

ASSIGN cmbVarebehType:DELIMITER = "|"
       cmbVarebehType:LIST-ITEM-PAIRS IN FRAME frmFilter = cAlle + "|0|"
     + DYNAMIC-FUNCTION("getFieldList","SysPara;Beskrivelse;ParaNr",
                         "WHERE SysHId = 5 and SysGr = 6")
/*            Suppleringsordre,1,Direkte varemottak,2" */
       cmbVarebehType:SCREEN-VALUE = "0"

       sokRegBestStat:DELIMITER IN FRAME frmLinje = "|"
       sokRegBestStat:LIST-ITEM-PAIRS = "||" + sokBestStat:LIST-ITEM-PAIRS
       sokBestOrdreStatus:DELIMITER IN FRAME frmOrdre = "|"
       sokBestOrdreStatus:LIST-ITEM-PAIRS = sokRegBestStat:LIST-ITEM-PAIRS
       .
IF bHKinst AND cButikkListe NE "*" THEN
  sokBestOrdreStatus:SCREEN-VALUE = sokBestOrdreStatus:ENTRY(2).

DO WITH FRAME frmListe:
  ASSIGN cBrwListFlds = "VarebehHode"
                        + ";VarebehNr|Vareh.nr"
                        + ";VarebehBeskrivelse|Vareh.beskrivelse"
                        + ";VarebehType|Type"
                        + ";MesseNr|Innkj.per"
                        + ";Kilde|Varebok nr"
                        + ";ProfilNr|Profilnr|>>>>>>9"
                        + ";EkstRef"
                        + ";Oppdatert"
                        + ";OppdatDato|Oppd.dato"
                        + ";EDato|Endret"
                        + ";BrukerId|Endret av"
                        + ";!VarebehNotat;!OppdatAv;!ButikkListe"
                        + ";+!LevNrList|CHARACTER|x(10)|butvarebeh_levfilter(ROWID)|LevnrList"              
                        + (IF bHKinst THEN 
                            ";!+VarebehFilter|CHARACTER|x(5)|butvarebehhode_messefilter(ROWID" + REPLACE(cButikkListe,",","&") + ")"
                           ELSE "")
                        + ";!+KundeordreBest|CHARACTER|x(5)|butvarebeh_kundeordrefilter(ROWID)"
                      + ",Messe"
                      + ";MesseBeskrivelse|Innkjøpsperiode@6"
                        + ";!MesseType|Messetype"
                        + ";!Oppmerking"
                      + ",VarebokHode"
                        + ";VarebokBeskrivelse@8"
                      + ",PrisProfil"
                        +  ";KortNavn|Profil@10"
                      + ",VarebehType" 
                        + ";BeskrivelseVareBehType|Varebeh.type@4"
         cBrwListJoin = ",FIRST Messe NO-LOCK where Messe.MesseNr = VarebehHode.MesseNr OUTER-JOIN"
                      + ",FIRST VarebokHode NO-LOCK where VarebokHode.VarebokNr = VarebehHode.Kilde OUTER-JOIN"
                      + ",FIRST PrisProfil NO-LOCK where PrisProfil.ProfilNr = VarebehHode.ProfilNr OUTER-JOIN"
                      + ",FIRST VarebehType NO-LOCK WHERE VarebehType.VarebehType = VarebehHode.VarebehType OUTER-JOIN"
                         .
/*  
                                + ",SysPara" 
                                  + ";Beskrivelse|Varebeh.type@4"
                      + ",FIRST SysPara NO-LOCK WHERE SysHId = 5 and SysGr = 6 and ParaNr = VarebehHode.VarebehType OUTER-JOIN"
*/
  DYNAMIC-FUNCTION("setAttribute",rectBrowseListe:HANDLE,"calcfieldproc","butvarebehhode_brwcalc.p").
  hBrowseListe = DYNAMIC-FUNCTION("NewBrowse",                
                    rectBrowseListe:HANDLE IN FRAME frmListe, 
                    50,                                       
                    "NUM-LOCKED-COLUMNS|1",           
                    cBrwListFlds,
                    "WHERE false" + cBrwListJoin
                    ,"sort|VarebehNr DESC,getrecordcount").
  DYNAMIC-FUNCTION("setAttribute",hBrowseListe,"querywhere","").

  DYNAMIC-FUNCTION("NewMenuBand",
                    hBrowseListe,  /* parent widget */
                    "Deselect;Fjern markering av rad",
                    "").   
  
  hSearchListe = DYNAMIC-FUNCTION("NewBrowseSearchField",RectBrowseSearchListe:HANDLE,hBrowseListe,1).
END.

DO WITH FRAME frmDetalj:
  hFieldMap = DYNAMIC-FUNCTION("NewFieldMap",
                    hBrowseListe:QUERY,
                    FRAME frmDetalj:HANDLE,
                    "ProfilNr,VarebehBeskrivelse,MesseNr,EkstRef,VarebehNotat,Oppdatert,Kilde","",       
                    "VarebehNr,VarebehType,MesseBeskrivelse,KortNavn,ButikkListe,OppdatDato,OppdatAv,VarebokBeskrivelse,BeskrivelseVareBehType","",  
                    "btnButikker,btnSokMesseNr,btnSokProfilNr,btnVareBokNr").

  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"customUpdateValProc","=updval_varebehhode.p"). 
  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"customDeleteValProc","=delval_varebehhode.p"). 
  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"BufferExtraFields","OppdatAv,OppdatDato,VarebehType,ButikkListe"). 

  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"enableOnEdit","no").

END.

RUN InitLinje.
RUN InitOrdre.

DO WITH FRAME {&FRAME-NAME}:
  /*
  IF CAN-DO("1",cAdgang) OR (bHKinst AND cButikkListe NE "*") THEN
      hUpdToolBar = DYNAMIC-FUNCTION("NewToolBar",
                    rectUpdToolBar:HANDLE,         
                    "Fil",                         
/*                     "Undo;Angre,save;Lagre,excel;Eksporter til E&xcel" */
/*                      + ",rule," +                                      */
                    "first|Naviger;Første,prev|Naviger;Forrige,next|Naviger;Neste&,last|Naviger;Siste&"
                  + ",Rapport;Rapporter"
                    ,"maxborder").   
  ELSE */ DO:
      hUpdToolBar = DYNAMIC-FUNCTION("NewToolBar",
                    rectUpdToolBar:HANDLE,     
                    "Fil",                     
                    "New;Ny,Copy;Kopier,Edit;&Endre,Undo;Angre,delete;Slett&,save;Lagre,excel;Eksporter til E&xcel"
                     + ",BrowseConfig;Kolonneoppsett&"
                     + ",rule,first|Naviger;Første,prev|Naviger;Forrige,next|Naviger;Neste&,last|Naviger;Siste&,|Innstillinger"
                     + ",rule,StartUtvalg;Utvalg"
                     + ",HentArtikler;Hent artikkel¤Enable"
                     + ",Rapport;Rapporter"
                     + ",HentUtvalg;Hent artikler fra utvalg"
                   ,"maxborder").      

    DYNAMIC-FUNCTION("NewMenuBand",
                      WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hUpdToolbar,"placeholder1")), 
                     "SetColorBestilt;Velg farge for markering av bestilte artikler;SetColorBestilt"
                     ,"").
  
    cBtnUtvalgHandles     = DYNAMIC-FUNCTION("getToolbarHandles",hUpdToolbar,"*StartUtvalg*").
    cBtnHentUtvalgHandles = DYNAMIC-FUNCTION("getToolbarHandles",hUpdToolbar,"*HentUtvalg*").
  END.

  hToolBar = DYNAMIC-FUNCTION("NewToolBar",
                    rectToolBar:HANDLE,             
                    "Fil",                          
                    "Help|Hjelp,Close;Avslutt",
                    "").   
  
  DYNAMIC-FUNCTION("createObjectLink",hBrowseListe,hUpdToolbar).
  DYNAMIC-FUNCTION("createObjectLink",hFieldMap,hUpdToolbar).
  DYNAMIC-FUNCTION("createObjectLink",hBrowseListe,hFieldMap).
  DYNAMIC-FUNCTION("createObjectLink",hBrowseListe,hSearchListe).

  DYNAMIC-FUNCTION("setAddMoveX",   THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "rectNavVarebeh").
/*   DYNAMIC-FUNCTION("setNoMoveX",   THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, DYNAMIC-FUNCTION("getAttribute",hUpdToolbar,"button-names")).  */
  DYNAMIC-FUNCTION("setNoResizeY",  THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "rectUpdToolBar,rectVarebeh,fi-ModusTekst").
  DYNAMIC-FUNCTION("setNoResizeX",  THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "rectNavVarebeh").
  DYNAMIC-FUNCTION("setAddResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "fi-ModusTekst").

  DYNAMIC-FUNCTION("setNoResizeX",  THIS-PROCEDURE:CURRENT-WINDOW, FRAME frmFilter:HANDLE, "RectArtSok").
  DYNAMIC-FUNCTION("setNoResizeY",  THIS-PROCEDURE:CURRENT-WINDOW, FRAME frmFilter:HANDLE, "frmFilter,RectArtSok").

  DYNAMIC-FUNCTION("setNoResizeX",  THIS-PROCEDURE:CURRENT-WINDOW, FRAME frmListe:HANDLE, "RectBrowseSearchListe").

  DYNAMIC-FUNCTION("setNoResizeX",  THIS-PROCEDURE:CURRENT-WINDOW, FRAME frmDetalj:HANDLE, "rect-1").
  DYNAMIC-FUNCTION("setNoResizeY",  THIS-PROCEDURE:CURRENT-WINDOW, FRAME frmDetalj:HANDLE, "rect-1").

  DYNAMIC-FUNCTION("setNoResizeX",  THIS-PROCEDURE:CURRENT-WINDOW, FRAME frmLinje:HANDLE, "RectBrowseSearchLinje,IMAGE-Sko,brwBestHode,rectBrwBestHode,rectSettings").
  DYNAMIC-FUNCTION("setNoResizeY",  THIS-PROCEDURE:CURRENT-WINDOW, FRAME frmLinje:HANDLE, "IMAGE-Sko,rectBrowseLinje,brwBestHode,rectBrwBestHode,rectBestHodeToolbar,brwAndreBest,rectBrwAndreBest,rectStatFields,rectSettings," + hBrowseLinje:NAME).
  DYNAMIC-FUNCTION("setNoMoveY",    THIS-PROCEDURE:CURRENT-WINDOW, FRAME frmLinje:HANDLE, "ButikkNr,brwTrans").
  DYNAMIC-FUNCTION("setNoMoveX",    THIS-PROCEDURE:CURRENT-WINDOW, FRAME frmLinje:HANDLE, "SumPris,frmOkReg").
  
  DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, hLagerStatusFrame,"").

  DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME frmOkReg:HANDLE,"frmOkReg").
  DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME frmOkReg:HANDLE,"frmOkReg").
  DYNAMIC-FUNCTION("setNoMoveX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME frmOkReg:HANDLE,"frmOkReg").
  DYNAMIC-FUNCTION("setNoMoveX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME frmOkReg:HANDLE,"btnOk,btnAvbryt").
  DYNAMIC-FUNCTION("setNoMoveY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME frmOkReg:HANDLE,"btnOk,btnAvbryt").

  DYNAMIC-FUNCTION("setNoResizeX",  THIS-PROCEDURE:CURRENT-WINDOW, FRAME frmOrdre:HANDLE, "BrwOrdreBestStr,BrwOrdreBestLev," + hBrowseOrdreBestStr:NAME + "," + hBrowseOrdreBestLev:NAME).
  DYNAMIC-FUNCTION("setAddMoveX",   THIS-PROCEDURE:CURRENT-WINDOW, FRAME frmOrdre:HANDLE, "BrwOrdreBestStr,BrwOrdreBestLev," + hBrowseOrdreBestStr:NAME + "," + hBrowseOrdreBestLev:NAME).
  DYNAMIC-FUNCTION("setNoResizeY",  THIS-PROCEDURE:CURRENT-WINDOW, FRAME frmOrdre:HANDLE, "BrwOrdreBest,BrwOrdreBestStr,BrwOrdreBestLev,BestToolbar,SokOrdreArtBeskr," + hBrowseOrdreBest:NAME + "," + hBrowseOrdreBestStr:NAME + "," + hBrowseOrdreBestLev:NAME).
  DYNAMIC-FUNCTION("setNoMoveX",    THIS-PROCEDURE:CURRENT-WINDOW, FRAME frmOrdre:HANDLE, "sokOpphav,btnBlankOrdreFilter," + DYNAMIC-FUNCTION("getToolBarNames",hOrdreToolbar,"")).
/*   DYNAMIC-FUNCTION("setAddResizeX",    THIS-PROCEDURE:CURRENT-WINDOW, FRAME frmOrdre:HANDLE, "SokOrdreArtBeskr").  */
  
  DYNAMIC-FUNCTION("setSplitBarY" ,THIS-PROCEDURE:CURRENT-WINDOW, btnSplitBarY:HANDLE IN FRAME frmLinje,NO).
  DYNAMIC-FUNCTION("setSplitBarYlimits",btnSplitBarY:HANDLE,220,230).
  DYNAMIC-FUNCTION("setFollowSplitBarY",THIS-PROCEDURE:CURRENT-WINDOW, btnSplitBarY:HANDLE,
                    STRING(hBrowseLinje) + "," +
                    STRING(rectBrwBestHode:HANDLE) + "," +
                    STRING(hBrowseBestHode) + "," +
                    STRING(button-slett:HANDLE) + "," +
                    STRING(Grid:HANDLE) + "," + 
                    STRING(rectBrwAndreBest:HANDLE) + "," +
                    STRING(hLagerStatusFrame) + "," +
                    STRING(hLagerStatusOCXframe) + "," +
                    STRING(LevDato:HANDLE IN FRAME frmLinje) + "," +
                    STRING(btnLevDatoDate:HANDLE) + "," +
                    STRING(DirekteLev:HANDLE) + "," +
                    STRING(rsLagerStatusSalg:HANDLE) + "," +
                    STRING(rectBestHodeToolbar:HANDLE) + "," +
                    STRING(rectBrowseLinje:HANDLE) + "," +
                    DYNAMIC-FUNCTION("getToolbarHandles",hBestHodeToolbar,"")
                    ).

  DYNAMIC-FUNCTION("setOrgWinSize", THIS-PROCEDURE:CURRENT-WINDOW,800,420,0,0).

  DYNAMIC-FUNCTION("SetToolbar",hToolBar,"enable").

END.

SUBSCRIBE TO "ArtBasEndret"          ANYWHERE.
SUBSCRIBE TO "NyArtBas"              ANYWHERE.
SUBSCRIBE TO "ExcelSheetParams"      ANYWHERE.
SUBSCRIBE TO "getVareSlagExceptList" ANYWHERE.
SUBSCRIBE TO "getArtSokFargeKoding"  ANYWHERE.
SUBSCRIBE TO "InitStringTranslation" ANYWHERE.
SUBSCRIBE TO "PrevNextBest"          ANYWHERE.
SUBSCRIBE TO "setLookupAttributes"   ANYWHERE.

TabStripChanged(11).

DYNAMIC-FUNCTION("initTranslation",THIS-PROCEDURE:CURRENT-WINDOW).
  
DEF VAR icurrw AS INT NO-UNDO.
DEF VAR icurrh AS INT NO-UNDO.

ASSIGN icurrh = THIS-PROCEDURE:CURRENT-WINDOW:HEIGHT-PIXELS
       icurrw = THIS-PROCEDURE:CURRENT-WINDOW:WIDTH-PIXELS.

/* THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = FALSE.  */

/* THIS-PROCEDURE:CURRENT-WINDOW:WINDOW-STATE = 1.                                                               */
/*                                                                                                               */
/* DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"Resize","").  */

/* ASSIGN {&WINDOW-NAME}:WIDTH-PIXELS = {&WINDOW-NAME}:WIDTH-PIXELS + SESSION:WORK-AREA-WIDTH-PIXELS - icurrw     */
/*        {&WINDOW-NAME}:HEIGHT-PIXELS = {&WINDOW-NAME}:HEIGHT-PIXELS + SESSION:WORK-AREA-HEIGHT-PIXELS - icurrh. */
/*                                                                                                                */
/* APPLY "window-resized" TO {&WINDOW-NAME}.                                                                      */
FRAME frmFilter:MOVE-TO-TOP().
FRAME frmListe:MOVE-TO-TOP().


IF NOT hFieldMap:AVAIL AND hBrowseListe:QUERY:IS-OPEN THEN
  hBrowseListe:QUERY:GET-FIRST().
  
RUN NyVarebeh ('0').

APPLY "ENTRY" TO hBrowseListe.  

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
DEF VAR cValidWeeks AS CHAR NO-UNDO.
DEF VAR hNewButton  AS HANDLE NO-UNDO.
DEF VAR hField      AS HANDLE NO-UNDO.

DO WITH FRAME frmLinje:
  RUN VisMiniBilde.w PERSIST SET hArtBilde.
  RUN InitializeObject IN hArtBilde (ArtikkelBilde:HANDLE).
  hArtBildeFrame = DYNAMIC-FUNCTION("getFrameHandle" IN hArtBilde).

  ASSIGN Kode:SIDE-LABEL-HANDLE:FONT = 6
         Kode:SIDE-LABEL-HANDLE:X = Kode:SIDE-LABEL-HANDLE:X - 8.

  BUTTON-Paste:HIDDEN = YES. /* bilde */

  IF bHKinst AND cButikkListe NE "*" THEN
    HIDE {&List-1}.

  ASSIGN cVarebehLinjeJoin    = ",FIRST ArtBas OF VarebehLinje NO-LOCK"
                                 + ",FIRST Farg OF ArtBas OUTER-JOIN"
                                 + ",FIRST SaSong OF ArtBas OUTER-JOIN"
                                 + ",FIRST StrType OF ArtBas OUTER-JOIN"
                                 + ",FIRST Material of ArtBas OUTER-JOIN"
         cAktivitetJoin       =  ",FIRST VgAkt WHERE VgAkt.Vg = VarebehLinje.Vg OUTER-JOIN"
                                 + ",FIRST Aktivitet OF VgAkt OUTER-JOIN"
         cVarebehLBuffAndFlds = "VarebehLinje" 
                                + ";LevNamn|Lev.navn|x(15)"
                                + ";ArtikkelNr|Art.nr  |>>>>>>>>>9 "
                                + ";ModellFarge|Modell|>>>>>>>>>9"
                                + ";LevKod|Lev.art.nr|x(15)"
                                + ";Beskr|Art.navn|x(30)"
                                + ";LevFargKod|Lev.farge|x(20)"
                                + ";Vg|VgNr|>>>>>9"
                                + ";VgBeskr|Vg.beskr"
                                + ";InnkjopsPris|Innpris"
                                + ";Varekost|Nto.Innpris"
                                + ";Pris|Markedspris"
                                + ";DB%"
                                + ";+SumAntall|DECIMAL|->>>>9|butvarebehlinje_antall(ROWID)|Antall"
                                + ";+SumVarekost|DECIMAL|-><>>><>>9.99|butvarebehlinje_varekost(ROWID)|Sum nto.innpr"
                                + ";+SumPris|DECIMAL|-><>>><>>9.99|butvarebehlinje_pris(ROWID)|Sum mar.pris"
                                + ";+SumDB|DECIMAL|-><>>><>>9.99|butvarebehlinje_sumdb(ROWID)|Sum DB"
                                + ";LinjeMerknad"
                                + ";Hg|HgNr"
                                + ";HgBeskr"
                                + ";AvdelingNr"
                                + ";AvdelingNavn"
                                + ";levnr"
                                + ";ModellFarge"
                                + ";!VarebehNr"
                                + ";!+bBestilt|LOGICAL|yes/no|butvarebehlinje_bestilt(ROWID)"
                                + ";!+bIkkeSendtBest|LOGICAL|yes/no|butvarebehlinje_ikke_sendt_best(ROWID)"
                                + ";!+bMatchMerknad|LOGICAL|yes/no|butvarebehlinje_match_merknad(ROWID)"
                                + ";!+RGBfarge|INTEGER|>>>>>>>>9|butvarebehlinje_rgb"
                                + ";KjedeVare|Kj.lev|J/N"
                                + ";Gjennomfaktureres|Gj.f|J/N"
                                + ";ForhRab%|F.rab"
                                + ";SupRab%|S.rab"
/*                                 + ";+Rab%|DECIMAL|>9.99|butvarebehlinje_rabatt|Rabatt" */
                                + ";Anbefaltpris|Veil.pris"
                                + ";!Utvidetsok"
/*                                 + ";+StrTypeTekst|CHARACTER|x(40)||Str.type tekst" */
                              + ",ArtBas"
                                + ";!BildNr;!SalgsEnhet;!SaSong" 
                                + ";LopNr|Lp.nr"
                                + ";StrTypeId|Str.t|>>>>>9"
                                + ";Farg|Farge|>>>>9"
                                + ";+!SaSongList|CHARACTER|x(10)|butvarebeh_sesongfilter(ROWID)"              
                                + ";MatKod"
                              + ",Farg"
                                + ";FarBeskr|Farge@7"
                              + ",Sasong"
                                + ";SasBeskr|Sesong"
                              + ",StrType;Beskrivelse|Str.type@30"
                              + ",Material;MatBeskr|Material|x(12)@8"
                              + ",VgAkt;!ETid"
                              + ",Aktivitet;!AktNr"
                                .   

  hBrowseLinje = DYNAMIC-FUNCTION("NewBrowse",         
                    rectBrowseLinje:HANDLE IN FRAME frmLinje,  
                    50,                          
                    "multiple",  
                    cVarebehLBuffAndFlds,
                    "WHERE false"
                      + cVarebehLinjeJoin + cAktivitetJoin + cBestHodeJoin,
                    "sort|LevNamn,calcfieldproc|butvarebeh_browsekalk.p").
  DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"windowsbrowse","yes").
  DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"select1strow","yes").
  DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"getrecordcount","yes").  
  DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"querywhere","").  
  DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"setReadOnlyOnReturn","yes").
  cDefaultViewFields = DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"currviewfields").
  hBrowseLinje:ROW-HEIGHT-PIXELS = 16.
  InitOverlays().

  IF NOT bHKinst THEN
    ASSIGN sokAvdelingNr:SENSITIVE   = NO 
           sokAvdelingNavn:SENSITIVE = NO 
           sokHg:SENSITIVE           = NO 
           sokHgBeskr:SENSITIVE      = NO
           sokVg:SENSITIVE           = NO 
           sokVgBeskr:SENSITIVE      = NO.

  DYNAMIC-FUNCTION("NewMenuBand",
                  hBrowseLinje, 
                  "Deselect;Fjern markering av rad"
                  + ",MultiSortBrowse;Sorter på flere kolonner" 
                  + (IF NOT bHKinst OR cButikkListe = "*" THEN 
                      ",|rule" 
                    + ",LinjeMerknad;Sett merknad for markerte linjer"
                    + ",RefreshArt;Frisk opp artikkelinformasjon alle artikler"
                    + ",RefreshArtValgte;Frisker opp artikkelinformasjon valgte;RefreshArtRecordValgte"
                    + ",SkiftStrType;Bytt størrelsestype" 
                    + ",OverforVarebok;Overfør artikler til varebok"
                     ELSE "")
                 ,"").     

  hSearchLinje = DYNAMIC-FUNCTION("NewBrowseSearchField",RectBrowseSearchLinje:HANDLE,hBrowseLinje,1).
  DYNAMIC-FUNCTION("createObjectLink",hBrowseLinje,hSearchLinje).

  hFieldMapLinje = DYNAMIC-FUNCTION("NewFieldMap",
                   hBrowseLinje:QUERY,
                   FRAME frmLinje:HANDLE,
                   "","","","","").
  DYNAMIC-FUNCTION("setAttribute",hFieldMapLinje,"customDeleteValProc","=delval_butvarebehlinje.p"). 
  hFieldRGBcolor = hFieldMapLinje:BUFFER-FIELD("RGBfarge"). 
  
  DYNAMIC-FUNCTION("createObjectLink",hBrowseLinje,hFieldMapLinje).

  ASSIGN hBestiltAnt    = hFieldMapLinje:BUFFER-FIELD("SumAntall")
         hBestiltAntCol = hBrowseLinje:GET-BROWSE-COLUMN(13)
         hIkkeSendtBest = hFieldMapLinje:BUFFER-FIELD("bIkkeSendtBest")
         .

  CREATE TEMP-TABLE httArtKalk.
  httArtKalk:CREATE-LIKE(hFieldMapLinje).
  httArtKalk:TEMP-TABLE-PREPARE("ttVarebehlinje").
  hbArtKalk = httArtKalk:DEFAULT-BUFFER-HANDLE.


  hBrowseBestHode = DYNAMIC-FUNCTION("NewBrowse",
                    rectBrwBestHode:HANDLE,
                    100,
                    "",
                    "VarebehBestHode"
                      + ";+BestHodeBestnr|INTEGER|>>>>>>>>9|varebehbesthode_bestnr.p(ROWID)|Bestnr"
                      + ";OrdreNr"
                      + ";+LevDato|DATE|99/99/99|varebehbesthode_levdato.p|Sendes"
                      + ";+BestInnlevStat|INTEGER|9|varebehbesthode_status.p|BS"
                      + ";CLButikkNr|CL"
                      + ";+Batchnr|INTEGER|>>>>>>>9|varebehbesthode_batchnr.p|Batchnr"
                      + ";AntLevert"
                      + ";VerdiLevert"
                      + ";RegistrertDato|Reg.dato"
                      + ";RegistrertAv|Reg.av"
                      + ";+BestHodeRtid|CHARACTER|x(6)|int_to_hhmm_time.p(RegistrertTid)|Reg.tid"
                      + ";!HodeLinjeId"
                      + ";!BestNr|Bestnr"
                      + ";!AlfaFordeling"
                      + ";!RegistrertTid"
                      + ";!VareBehNr"
                      + ";!ArtikkelNr"
                      + ";!DirekteLev"
                  + ",BestHode"
                      + ";!BestStat"
                      + ";TotAntPar|Antall"
                      + ";TotInnkjVerdi|Sum varekost"
                      + ";TotSalgsVerdi|Sum pris"
                      + ";BestType"
                      + ";SendtAv"
                      + ";SendtDato"
                      ,
                    "WHERE false"
                  + ",FIRST BestHode WHERE BestHode.VarebehNr = VarebehBestHode.VarebehNr AND BestHode.BestNr = VareBehBestHode.BestNr NO-LOCK OUTER-JOIN",
                    "sort|Ordrenr desc").
  hBrowseBestHode:NAME = "brwBestHode".

  ASSIGN cInnlevViewFields = "Batchnr,AntLevert,VerdiLevert,RegistrertDato,BestHodeRtid,RegistrertAv"
         cBestViewFields   = "BestHodeBestnr,OrdreNr,LevDato,BestInnlevStat,CLButikkNr,TotAntPar,TotInnkjVerdi,TotSalgsVerdi,BestType,SendtAv,SendtDato,RegistrertDato,BestHodeRtid,RegistrertAv"
                             .
  
  DO ix = 12 TO hBrowseBestHode:NUM-COLUMNS:
    hBrowseBestHode:MOVE-COLUMN(ix,ix - 6).
  END.

/*   DYNAMIC-FUNCTION("setAttribute",hBrowseBestHode,"NoColumnSort","BestHodeBestnr,BestInnlevStat,Batchnr,Dummy").  */
  DYNAMIC-FUNCTION("setAttribute",hBrowseBestHode,"SortMap","BestHodeRtid;RegistrertTid").
  DYNAMIC-FUNCTION("CreateParentLink",hBrowseBestHode,hBrowseLinje,"VarebehNr,ArtikkelNr").


  hFieldMapBestHode = DYNAMIC-FUNCTION("NewFieldMap",
                   hBrowseBestHode:QUERY,
                   FRAME frmLinje:HANDLE,
                   "LevDato,DirekteLev","",
                   "","",
                   "btnLevDatoDate").
  DYNAMIC-FUNCTION("setAttribute",hFieldMapBestHode,"bufferextrafields","VarebehNr,ArtikkelNr,Alfafordeling,CLButikkNr").
  DYNAMIC-FUNCTION("setAttribute",hFieldMapBestHode,"customCreateProc","create_varebehbesthode.p").
  DYNAMIC-FUNCTION("setAttribute",hFieldMapBestHode,"customDeleteValProc","=delval_varebehbesthode.p"). 
  DYNAMIC-FUNCTION("setAttribute",hFieldMapBestHode,"customUpdateValProc","ignore"). 

  DYNAMIC-FUNCTION("CreateObjectLink",hFieldMapBestHode,hBrowseBestHode).

  hBestHodeToolbar = DYNAMIC-FUNCTION("NewToolbar",
                   rectBestHodeToolbar:HANDLE,
                   "",
                   "new;Ny &bestilling/innlevering (ALT-B)"
                   + ",delete;Slett bestilling (innlev kan ikke slettes her)"
                   + ",undo;Angre"
                   + ",save;Lagre bestilling/innlevering"
                   + ",excel"
                   + (IF bHKinst AND cButikkListe NE "*" THEN "" ELSE ",Strekkode;S&trekkoder")
                   + ",Kalkyle;Kalk&yle"
                   + ",VisBest;Vis bestilling"
/*                    + ",VisInnlev;Innleveranse" */
                   ,"maxborder").
  hLagerBestButton = DYNAMIC-FUNCTION("getEventWidget",hBestHodeToolbar,"save","").
  hNewButton = DYNAMIC-FUNCTION("getEventWidget",hBestHodeToolbar,"new","").
  hNewButton:TOOLTIP = "Ny bestilling/innlevering (ALT-B)".
  hKalkyleButton = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hBestHodeToolbar,"buttonKalkyle")).
  hStrekkodeButton = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hBestHodeToolbar,"buttonStrekkode")).

  DYNAMIC-FUNCTION("CreateObjectLink",hBestHodeToolbar,hBrowseBestHode).
  DYNAMIC-FUNCTION("CreateObjectLink",hBestHodeToolbar,hFieldMapBestHode).

  RUN FlexGrid.w PERSIST SET hLagerstatus.
  RUN InitializeObject IN hLagerstatus (rectBrwAndreBest:HANDLE).
  hLagerStatusFrame = DYNAMIC-FUNCTION("getFrameHandle" IN hLagerStatus).
  hLagerStatusOCXframe = DYNAMIC-FUNCTION("getOCXframe" IN hLagerStatus).

  cStatFieldHandles = STRING(SumVarekost:HANDLE) + "," + STRING(SumPris:HANDLE).

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
  cStatFieldNames = TRIM(cStatFieldNames,",").
  APPLY "value-changed" TO tbUpdateStat.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitOrdre C-Win 
PROCEDURE InitOrdre :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR hSendBtn  AS HANDLE NO-UNDO.

DO WITH FRAME frmOrdre:  
  RUN VisMiniBilde.w PERSIST SET hBestBilde.
  RUN InitializeObject IN hBestBilde (BestBilde:HANDLE).
  hBestBildeFrame = DYNAMIC-FUNCTION("getFrameHandle" IN hBestBilde).

/*   ASSIGN sokOrdreStatus:DELIMITER = "|"                                                 */
/*          sokOrdreStatus:LIST-ITEM-PAIRS = RIGHT-TRIM("|0|" +                            */
/*                  DYNAMIC-FUNCTION("getFieldList","SysPara;Beskrivelse;ParaNr",          */
/*                                                  "WHERE SysHId = 5 and SysGr = 3"),"|") */
/*          sokOrdreStatus:SCREEN-VALUE = "0".                                             */
  ASSIGN cmbKjedevare:LIST-ITEM-PAIRS         = ",,Ja,Yes,Nei,No"
         cmbGjennomfaktureres:LIST-ITEM-PAIRS = ",,Ja,Yes,Nei,No"
         sokOrdremottaker:LIST-ITEM-PAIRS     = ",,Kjedelevert,KJEDE,Gjennomfaktureres,GJENNOM"
         sokOpphav:LIST-ITEM-PAIRS            = ",,ERP,ERP,HK,HK"
         .

  IF bHKinst AND cButikkListe NE "*" THEN 
    ASSIGN sokOrdreCL:SCREEN-VALUE     = cButCL  
           SokOrdreCLNamn:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","Butiker;ButNamn","WHERE Butik = " + sokOrdreCL:SCREEN-VALUE)
           sokOrdreCL:SENSITIVE        = NO.


  hBrowseOrdre = DYNAMIC-FUNCTION("NewBrowse",
                    rectBrowseOrdre:HANDLE,
                    200,
                    "MULTIPLE",
                    "Ordre"
                    + ";OrdreNr"
                    + ";Hasteordre|Haster|Ja/Nei"
                    + ";LeveringsDato"
                    + ";CL"
                    + ";+OrdreTotAnt|INTEGER|>>>>9|ordre_tot_antall|Antall"
                    + ";+OrdreTotPris|INTEGER|->>>>>9.99|ordre_tot_pris|Sum innk.pris"
                    + ";+OrdreTotDBkr|INTEGER|->>>>>9.99|ordre_tot_dbkr|Sum DB"
                    + ";+OrdreTotAntLev|INTEGER|>>>>9|ordre_tot_levert|Ant.lev"
                    + ";+OrdreTotRestAnt|INTEGER|->>>9|ordre_tot_rest|Ant.rest"
                    + ";RegistrertDato"
                    + ";SendtDato"
                    + ";BekreftetDato|Bekr.dato"
                    + ";LevNr|Levnr"
                    + ";OrdreStatus|St"
                    + ";OrdreMottaker"
                    + ";!VarebehNr"
                    + ";!+StatusOgButikkSjekk|CHARACTER|x|ordre_for_butikk(ROWID)"
/*                     + ";!+Kjedevare|CHARACTER|x|kjedevare_filter(ROWID)"                 */
/*                     + ";!+Gjennomfaktureres|CHARACTER|x|gjennomfaktureres_filter(ROWID)" */
/*                     + ";EkstId"  */
                  + ",LevBas"
                    + ";LevNamn|Levnavn@14"
                  + ",buf1_SysPara"
                    + ";Beskrivelse|Status|X(20)@2"
                    ,"WHERE false"
                   + ",FIRST LevBas OF Ordre NO-LOCK"
                   + ",FIRST buf1_SysPara WHERE  buf1_SysPara.SysHId = 5 and buf1_SysPara.SysGr = 3 AND buf1_SysPara.ParaNr = Ordre.OrdreStatus NO-LOCK OUTER-JOIN"
                    ,""
/*                    + ",calcfieldproc|ordre_browsekalk.p" */
                     ).

  DYNAMIC-FUNCTION("setAttribute",hBrowseOrdre,"getrecordcount","yes").  
  DYNAMIC-FUNCTION("setAttribute",hBrowseOrdre,"customDeleteValProc","=ordre_delete_all.p").
  DYNAMIC-FUNCTION("setAttribute",hBrowseOrdre,"copytoolbartobrowse","yes").
  DYNAMIC-FUNCTION("setAttribute",hBrowseOrdre,"windowsbrowse","yes").
  DYNAMIC-FUNCTION("setAttribute",hBrowseOrdre,"calcfieldproc","ordre_browsekalk.p").
  DYNAMIC-FUNCTION("setSortString",hBrowseOrdre,"LeveringsDato;desc").

  ASSIGN hHasteordreColumn  = hBrowseOrdre:GET-BROWSE-COLUMN(2)
         hHasteordreField   = hBrowseOrdre:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("Hasteordre")
         hBuffOrdre         = hBrowseOrdre:QUERY:GET-BUFFER-HANDLE(1)
         .
/*          hHasteordreOverlay = DYNAMIC-FUNCTION("NewBrowseToggle",                      */
/*                                                hBrowseOrdre,                           */
/*                                                "Hasteordre",                           */
/*                                                "Hasteordre",                           */
/*                                                "").                                    */
/*   DYNAMIC-FUNCTION("CreateOverlayLink",hBrowseOrdre,hHasteordreOverlay,"Hasteordre").  */

  hBrowseOrdreBest = DYNAMIC-FUNCTION("NewBrowse",
                    BrwOrdreBest:HANDLE,
                    100,
                    "",
                    "BestHode"
                    + ";BestNr"
                    + ";BestStat|BS"
                    + ";ArtikkelNr|Art.nr"
                    + ";LevKod"
                    + ";LevFargKod"
                    + ";BekreftetDato|Bekr.dato"
                    + ";TotAntPar|Ant"
                    + ";EkstId|Ekstern id"
                    + ";TotInnkjVerdi|Sum varekost"
                    + ";TotSalgsVerdi|Sum pris"
                    + ";TotInnLev"
                    + ";TotMakulert"
                    + ";+BestTotRestAnt|INTEGER|->>>9|best_tot_rest.p|Ant.rest"
                    + ";BestType"
                    + ";SendtAv"
                    + ";SendtDato"
                    + ";+Kjedevare|LOGICAL|J/N|kjedevare|Kj.lev"
                    + ";+Gjennomfaktureres|LOGICAL|J/N|gjennomfaktureres|Gj.fakt"
                    + ";!OrdreNr"
                  + ",ArtBas"
                    + ";Beskr@5"
                    + ";!BildNr"
                  + ",buf2_SysPara"
                    + ";Beskrivelse|Status@3"
                    ,"WHERE false"
                   + ",FIRST ArtBas OF BestHode NO-LOCK"
                   + ",FIRST buf2_SysPara WHERE  buf2_SysPara.SysHId = 5 and buf2_SysPara.SysGr = 2 AND buf2_SysPara.ParaNr = BestHode.BestStat NO-LOCK OUTER-JOIN"
                    ,"sort|BestNr").
  DYNAMIC-FUNCTION("CreateParentLink",hBrowseOrdreBest,hBrowseOrdre,"OrdreNr").
  DYNAMIC-FUNCTION("setAttribute",hBrowseOrdreBest,"calcfieldproc","butvarebeh_besthode_brwcalc.p").
  DYNAMIC-FUNCTION("setAttribute",hBrowseOrdreBest,"copytoolbartobrowse","yes").
  DYNAMIC-FUNCTION("setAttribute",hBrowseOrdreBest,"customDeleteValProc","=delval_besthode_varebeh.p").

  hBrowseOrdreBestStr = DYNAMIC-FUNCTION("NewBrowse",
                    BrwOrdreBestStr:HANDLE,
                    100,
                    "",
                    "BestStr"
                    + ";Butik"
                    + ";Storl"
                    + ";BestStat"
                    + ";Bestilt"
                    + ";!BestNr"
                    + ";!+BestStrUnique|CHARACTER|x(20)|beststr_distinct.p(ROWID)"
                  + ",StrKonv"
                    + ";!StrKode"
                    ,"WHERE false"
                   + ",FIRST StrKonv WHERE TRIM(StrKonv.Storl) = TRIM(Storl)"
                    ,"sort|Butik;BY;StrKode").

  DYNAMIC-FUNCTION("CreateParentLink",hBrowseOrdreBestStr,hBrowseOrdreBest,"BestNr").

  hBrowseOrdreBestLev = DYNAMIC-FUNCTION("NewBrowse",
                    BrwOrdreBestLev:HANDLE,
                    100,
                    "",
                    "BestLevert"
                    + ";Butik|But"
                    + ";Storl|Str"
                    + ";Levert|Lev"
                    + ";Rest"
                    + ";Avskrevet|Avskr"
                    + ";LevertAv|Av"
                    + ";LevertDato"
                    + ";!BestNr"
                  + ",StrKonv"
                    + ";!StrKode"
                    ,"WHERE false"
                  + ",FIRST StrKonv WHERE TRIM(StrKonv.Storl) = TRIM(Storl)"
                   ,"sort|Butik;BY;StrKode").
  DO ix = 1 TO 6:
    hBrowseOrdreBestLev:GET-BROWSE-COLUMN(ix):WIDTH-PIXELS = hBrowseOrdreBestLev:GET-BROWSE-COLUMN(ix):WIDTH-PIXELS - (IF ix = 1 THEN 5 ELSE 9).
  END.

  DYNAMIC-FUNCTION("CreateParentLink",hBrowseOrdreBestLev,hBrowseOrdreBest,"BestNr").


  hOrdreToolbar = DYNAMIC-FUNCTION("NewToolbar",
                   OrdreToolbar:HANDLE,
                   "",
                   "HasteOrdre;Haster;Marker valgte ordre som hasteordre"
                   + ",SendOrdre;Send ordre"
                   + ",EndreOrdreLevDato;Endre lev.dato"
                   + (IF NOT bHKinst OR cButikkListe = "*" THEN
                        ",BekrOrdre;Bekreft ordre"
                      + ",InnlevOrdre;Innlever ordre"
                      + ",InnlevRapport;Innleveranserapport"
                      + ",OverforVaremottak;Overfør til varemottak"
                      + ",MakulerOrdre;Avskriv rest"
                      + ",copy;Kopier ordre"
                      ELSE "")
                   + ",ReSendOrdre;Resend ordre til butikk"
                   + ",delete;Slett ordre&"
                   + ",excel"
                   + ",Print;Ordreutskrift"
                   + ",UkePrint;Sum pr uke;Viser ordresum pr uke;;gif/instordr.gif"
                   + ",BrowseConfig;Kolonneoppsett"
                   ,"").
  DYNAMIC-FUNCTION("CreateObjectLink",hBrowseOrdre,hOrdreToolbar).
  hSendBtn = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hOrdreToolbar,"buttonSendOrdre")).
  DYNAMIC-FUNCTION("NewMenuBand",hSendBtn,
                   "OpphevSendt;Sett status tilbake til NY ordre"
                   ,"").

  hBestToolbar = DYNAMIC-FUNCTION("NewToolbar",
                   BestToolbar:HANDLE,
                   "",
                   (IF NOT bHKinst OR cButikkListe = "*" THEN
                       "BekrBest;Bekreft bestilling"
                     + ",InnlevBest;Innlever bestilling"
                     + ",Kalkyle;Kalk&yle"
                     + ",VisBest;Vis bestilling"
                     + ",VisInnlev;Del-leveranse"
                     + ",MakulerBest;Avskriv rest,"
                     ELSE "")
                  + (IF NOT bHKinst THEN
                      "edit;delete;Slett bestilling&"
                     ELSE "delete;Slett bestilling&")
                  + ",excel"
                  + ",BrowseConfig;Kolonneoppsett"
                  ,"maxborder").
  DYNAMIC-FUNCTION("CreateObjectLink",hBrowseOrdreBest,hBestToolbar).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitStringTranslation C-Win 
PROCEDURE InitStringTranslation :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM        ihWindow    AS HANDLE NO-UNDO.
DEF INPUT-OUTPUT PARAM iocTypeList AS CHAR   NO-UNDO.

DEF VAR cCurrValue AS CHAR NO-UNDO.

IF ihWindow NE THIS-PROCEDURE:CURRENT-WINDOW THEN RETURN.

IF NOT CAN-DO(iocTypeList,"Kjedevare") THEN
  iocTypeList = iocTypeList + ",Kjedevare".
IF NOT CAN-DO(iocTypeList,"Gjennomfaktureres") THEN
  iocTypeList = iocTypeList + ",Gjennomfaktureres".

ASSIGN cCurrValue = cmbGjennomfaktureres:SCREEN-VALUE IN FRAME frmOrdre
       cmbGjennomfaktureres:LIST-ITEM-PAIRS = 
                 DYNAMIC-FUNCTION("getStringTranslation",THIS-PROCEDURE:FILE-NAME,"Gjennomfaktureres",cmbGjennomfaktureres:LIST-ITEM-PAIRS)
       cmbGjennomfaktureres:SCREEN-VALUE = cCurrValue

       cCurrValue = cmbKjedevare:SCREEN-VALUE IN FRAME frmOrdre
       cmbKjedevare:LIST-ITEM-PAIRS = 
                 DYNAMIC-FUNCTION("getStringTranslation",THIS-PROCEDURE:FILE-NAME,"Kjedevare",cmbKjedevare:LIST-ITEM-PAIRS)
       cmbKjedevare:SCREEN-VALUE = cCurrValue
       .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InnlevBestRecord C-Win 
PROCEDURE InnlevBestRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cBestNrList AS CHAR NO-UNDO.
DEF VAR cMsg         AS CHAR NO-UNDO.

DO ix = 1 TO hBrowseOrdreBest:NUM-SELECTED-ROWS:
  IF hBrowseOrdreBest:FETCH-SELECTED-ROW(ix) THEN
    cBestNrList = cBestNrList + STRING(hBrowseOrdreBest:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("BestNr"):BUFFER-VALUE) + "|".
END.
IF cBestNrList NE "" THEN DO:
  IF DYNAMIC-FUNCTION("DoMessage",0,4,"Registrer varemottak for bestilling?","","") = 6 THEN
    IF NOT DYNAMIC-FUNCTION("runproc","ordre_best_full_innlev.p","fullbest;" 
                                                                + DYNAMIC-FUNCTION("getASuserId") + ";"
                                                                + TRIM(cBestNrList,"|"),?) THEN DO:
      cMsg = DYNAMIC-FUNCTION("getTransactionMessage").
      DYNAMIC-FUNCTION("DoMessage",0,
                       IF INDEX(cMsg,CHR(10)) > 0 THEN 20 ELSE 0,
                       cMsg,"","").
    END.
    ELSE DO:
      SkrivEtikett(DYNAMIC-FUNCTION("getTransactionMessage")).

      DYNAMIC-FUNCTION("RefreshRowids",hBrowseOrdre,hBrowseOrdre:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("RowIdent1"):BUFFER-VALUE).
      APPLY "value-changed" TO hBrowseOrdre.
    END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InnlevOrdreRecord C-Win 
PROCEDURE InnlevOrdreRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iOrdreNr     AS INT  NO-UNDO.
DEF VAR cMsg         AS CHAR NO-UNDO.
DEF VAR iReturn      AS INT  NO-UNDO.


IF (DYNAMIC-FUNCTION("getCurrentObject") = hOrdreToolbar OR DYNAMIC-FUNCTION("getCurrentObject") = hBrowseOrdre)
   AND hBrowseOrdre:NUM-SELECTED-ROWS NE 1 THEN DO:
  DYNAMIC-FUNCTION("DoMessage",0,0,"Innlevering bare gjøres hvis én ordre er valgt","","").
  RETURN.
END.

iOrdrenr = hBrowseOrdre:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("Ordrenr"):BUFFER-VALUE.

RUN OrdreBehandling("innlev").
IF RETURN-VALUE = "cancel" THEN RETURN.

RUN skrivMottaksrapport.p (iOrdrenr,"","").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InnlevRapportRecord C-Win 
PROCEDURE InnlevRapportRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF (DYNAMIC-FUNCTION("getCurrentObject") = hOrdreToolbar OR DYNAMIC-FUNCTION("getCurrentObject") = hBrowseOrdre)
   AND hBrowseOrdre:NUM-SELECTED-ROWS NE 1 THEN DO:
  DYNAMIC-FUNCTION("DoMessage",0,0,"Innleveranserapport kan bare kjøres for en ordre","","").
  RETURN.
END.
RUN skrivMottaksrapport.p (hBrowseOrdre:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("Ordrenr"):BUFFER-VALUE,"","").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InvalidateHandle C-Win 
PROCEDURE InvalidateHandle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihInvalid AS HANDLE NO-UNDO.

CASE ihInvalid:
  WHEN hStrekkode THEN hStrekkode = ?.
END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KalkyleRecord C-Win 
PROCEDURE KalkyleRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF hFieldMap:BUFFER-FIELD("VareBehType"):BUFFER-VALUE NE iBestVarebeh THEN DO:
  RUN d-vArtKalkyle.w (DYNAMIC-FUNCTION("getRecId","ArtBas",hFieldMapLinje:BUFFER-FIELD("RowIdent2"):BUFFER-VALUE)).
  RUN ArtBasEndret(hFieldMapLinje:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE).
  APPLY "entry" TO Grid.
END.
ELSE DO:
  IF DYNAMIC-FUNCTION("getCurrentObject") = hBestHodeToolbar THEN
    RUN d-vBestKalkyle.w (DYNAMIC-FUNCTION("getRecId","ArtBas",hFieldMapLinje:BUFFER-FIELD("RowIdent2"):BUFFER-VALUE),
                          DYNAMIC-FUNCTION("getRecId","BestHode",hFieldMapBestHode:BUFFER-FIELD("RowIdent2"):BUFFER-VALUE)
                          ).
  ELSE IF hBrowseOrdreBest:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN
    RUN d-vBestKalkyle.w (DYNAMIC-FUNCTION("getRecId","ArtBas",hBrowseOrdreBest:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("RowIdent2"):BUFFER-VALUE),
                          DYNAMIC-FUNCTION("getRecId","BestHode",hBrowseOrdreBest:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("RowIdent1"):BUFFER-VALUE)
                          ).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LagreBestLinje C-Win 
PROCEDURE LagreBestLinje :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VAR iRow     AS INTE NO-UNDO.
DEFINE VAR iCol     AS INTEGER    NO-UNDO.
DEF VAR iTmpLinjeId AS INT NO-UNDO.

EMPTY TEMP-TABLE TT_VareBehBestLinje.

DO iRow = 1 TO chGrid:Rows - 1:
  IF NOT Registrerat(iRow) THEN
      NEXT.
  DO iCol = 2 TO chGrid:Cols - 1:
    IF INT(chGrid:TextMatrix(iRow,iCol)) = 0 THEN NEXT.

    CREATE TT_VareBehBestLinje.
    ASSIGN TT_VareBehBestLinje.VareBehNr       = hFieldMap:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE
           TT_VareBehBestLinje.CLButikkNr      = hFieldMapBestHode:BUFFER-FIELD("CLButikkNr"):BUFFER-VALUE
           TT_VareBehBestLinje.HodeLinjeId     = hFieldMapBestHode:BUFFER-FIELD("HodeLinjeId"):BUFFER-VALUE
           TT_VareBehBestLinje.BestiltButikkNr = INT(chGrid:TextMatrix(iRow,0))
           TT_VareBehBestLinje.Bestilt         = INT(chGrid:TextMatrix(iRow,iCol))
           TT_VareBehBestLinje.Storl           = TRIM(chGrid:TextMatrix(0,iCol))
           .
  END.
END.

iTmpLinjeId = hFieldMapBestHode:BUFFER-FIELD("HodeLinjeId"):BUFFER-VALUE.
IF NOT DYNAMIC-FUNCTION("runproc","varebehbest_lagrebestlinje.p",
                         STRING(hFieldMap:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE) + "," +
                         STRING(hFieldMapBestHode:BUFFER-FIELD("CLButikkNr"):BUFFER-VALUE) + "," +
                         STRING(hFieldMapBestHode:BUFFER-FIELD("HodeLinjeId"):BUFFER-VALUE) + "," +
                         LevDato:SCREEN-VALUE IN FRAME frmLinje + "," +
                         "best" + "," +
                         STRING(DirekteLev:INPUT-VALUE),
                         hBufftt_varebehbestlinje:TABLE-HANDLE) THEN
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil","").
ELSE DO:
  SkrivEtikett(DYNAMIC-FUNCTION("getTransactionMessage")).

  DYNAMIC-FUNCTION("RefreshRowids",hBrowseLinje,hBrowseLinje:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("RowIdent1"):BUFFER-VALUE).
  APPLY "value-changed" TO hBrowseLinje.
  bOK = hFieldMapBestHode:FIND-FIRST("WHERE HodeLinjeId = " + STRING(iTmpLinjeId)) NO-ERROR.
  IF bOK THEN DO:
    hBrowseBestHode:SET-REPOSITIONED-ROW(hBrowseBestHode:DOWN,"conditional").
    hBrowseBestHode:QUERY:REPOSITION-TO-ROWID(hFieldMapBestHode:ROWID) NO-ERROR.
    IF NOT ERROR-STATUS:ERROR THEN 
      APPLY "value-changed" TO hBrowseBestHode.
  END.
  APPLY "entry" TO hBrowseLinje.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LagreNyLinje C-Win 
PROCEDURE LagreNyLinje :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cArtikkelNr     AS CHAR NO-UNDO INIT ?.
DEF VAR cRowId          AS CHAR NO-UNDO.
DEF VAR bTmpAlt-S       AS LOG  NO-UNDO.
DEF VAR cValue          AS CHAR NO-UNDO.
DEF VAR cModellFarge    AS CHAR NO-UNDO.
DEF VAR bNyModell       AS LOG  NO-UNDO.
DEF VAR cEditArtikkelNr AS CHAR NO-UNDO.
DEF VAR dTmpLevDato     AS DATE NO-UNDO.

IF dMinLevDato NE ? THEN
  dTmpLevDato = dMinLevDato.

IF INDEX(hBrwOLBeskr:SCREEN-VALUE,"|") > 0 THEN DO:
  DYNAMIC-FUNCTION("DoMessage",0,0,"Ugyldig tegn: | i artikkelregister","","").
  RETURN "ERROR".
END.

IF DYNAMIC-FUNCTION("runproc","artbas_static_find.p",
                    "LevKod|" + hBrwOLLevKod:SCREEN-VALUE
            + "¤" + "LevNr|" + sokLevNr:SCREEN-VALUE IN FRAME frmLinje
            + "¤" + "LevFargKod|" + hBrwOLLevFargKod:SCREEN-VALUE
            + "¤" + "Beskr|" + hBrwOLBeskr:SCREEN-VALUE
/*             + "¤" + "StrTypeId|" + hBrwOLStrTypeId:SCREEN-VALUE */
/*             + "¤" + "Vg|" + hBrwOLVg:SCREEN-VALUE               */
                   ,?) THEN
  cArtikkelNr = DYNAMIC-FUNCTION("getTransactionMessage").

/* cArtikkelNr = DYNAMIC-FUNCTION("getFieldValues","ArtBas"                                */
/*                           ,'WHERE LevKod = "' + hBrwOLLevKod:SCREEN-VALUE + '"'         */
/*                           + ' AND LevNr = ' + sokLevNr:SCREEN-VALUE IN FRAME frmLinje   */
/*                           + ' AND LevFargKod = "' + hBrwOLLevFargKod:SCREEN-VALUE + '"' */
/*                           + ' AND Beskr = "' + hBrwOLBeskr:SCREEN-VALUE + '"'           */
/*                           + ' AND StrTypeId = ' + hBrwOLStrTypeId:SCREEN-VALUE          */
/*                           + ' AND Vg = ' + hBrwOLVg:SCREEN-VALUE                        */
/*                           ,'ArtikkelNr').                                               */

IF cArtikkelNr NE ? THEN DO:    

  cValue = DYNAMIC-FUNCTION("getFieldValues","VarebehLinje",
                           "WHERE VarebehNr = " + VarebehNr:SCREEN-VALUE IN FRAME frmDetalj
                         + " AND ArtikkelNr = " + cArtikkelNr
                          ,"Beskr").
  IF cValue NE ? THEN DO:
    DYNAMIC-FUNCTION("DoMessage",0,0,"Artikkel er allerede registrert i varebok","","").
    RETURN "ERROR".
  END.

  IF DYNAMIC-FUNCTION("DoMessage",0,1
                   ,"Artikkel med samme leverandør, lev.art.nr, farge, beskrivelse, varegruppe og størrelsestype eksisterer i artikkelregister" + CHR(10)
                  + "Bekreft at denne skal oppdateres og legges inn i varebok"
                   ,"Artikkel er allerede opprettet","") = 2 THEN DO:
    DYNAMIC-FUNCTION("DispBrwOverlayWidgets",hBrowseLinje).                   
    RETURN "ERROR".
  END.

  cEditArtikkelNr = cArtikkelNr.
END.
ELSE DO:
  cArtikkelNr = DYNAMIC-FUNCTION("getFieldValues","ArtBas"
                            ,"WHERE LevKod = '" + hBrwOLLevKod:SCREEN-VALUE + "'" 
                            + " AND LevNr = " + sokLevNr:SCREEN-VALUE IN FRAME frmLinje
                            ,"ArtikkelNr").

  IF cArtikkelNr NE ? THEN DO:      
    IF DYNAMIC-FUNCTION("DoMessage",0,1
                     ,"Artikkel med samme leverandør og lev.art.nr eksisterer i artikkelregister" + CHR(10)
                    + "Bekreft at ny artikkel skal registreres og kobles til samme modell (hovedartikkel)"
                     ,"Tilsvarende modell er allerede opprettet","") = 2 THEN RETURN "ERROR".

    cModellFarge = DYNAMIC-FUNCTION("getFieldValues","ArtBas","WHERE ArtikkelNr = " + cArtikkelNr,"ModellFarge").
    IF cModellFarge = "0" THEN DO:
      cModellFarge = DYNAMIC-FUNCTION("getFieldValues","ArtBas"
                                      ,"WHERE LevKod = '" + hBrwOLLevKod:SCREEN-VALUE + "'" 
                                      + " AND LevNr = " + sokLevNr:SCREEN-VALUE IN FRAME frmLinje
                                      + " AND ModellFarge > 0"
                                      ,"ModellFarge").
      IF cModellFarge = ? THEN
        ASSIGN cModellFarge = cArtikkelNr
               bNyModell    = YES. 
    END.
  END.
END.

IF DYNAMIC-FUNCTION("runproc","artbas_vedlikehold.p",
                    (IF cEditArtikkelNr = "" THEN "new,0," ELSE "edit," + cEditArtikkelNr + ",")
                  + cInitPrisProfil + "," + VarebehNr:SCREEN-VALUE IN FRAME frmDetalj
                    ,HentVerdier()) THEN DO:

  cArtikkelNr = DYNAMIC-FUNCTION("getTransactionMessage").

  IF DYNAMIC-FUNCTION("runProc","varebehlinje_new.p",cArtikkelNr + "|" + VarebehNr:SCREEN-VALUE IN FRAME frmDetalj
                      ,?) THEN
    cRowId = DYNAMIC-FUNCTION("getTransactionMessage").
  ELSE DO:
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
    RETURN "ERROR".
  END.

  IF bNyModell THEN
    DYNAMIC-FUNCTION("DoUpdate","ArtBas","ignore","ArtikkelNr",cModellFarge,"ModellFarge,HovedModellFarge",cModellFarge + "|yes",TRUE).
  IF cModellFarge NE "" THEN
    DYNAMIC-FUNCTION("DoUpdate","ArtBas","ignore","ArtikkelNr",cArtikkelNr,"ModellFarge,HovedModellFarge",cModellFarge + "|no",TRUE).

  hFieldMapLinje:BUFFER-FIELD("Rowident1"):BUFFER-VALUE = cRowId.
  DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"uselocaldata","").

  IF DYNAMIC-FUNCTION("refreshRowids",hBrowseLinje,cRowId) = 1 THEN DO:

    ASSIGN bNewArt   = TRUE
           bTmpAlt-S = bAlt-S
           bAlt-S    = FALSE
           .

    DYNAMIC-FUNCTION("DoLockWindow",THIS-PROCEDURE:CURRENT-WINDOW).
    DYNAMIC-FUNCTION("setAttribute",SESSION,"userKeepsWindowLocked","YES").

    DYNAMIC-FUNCTION("setCurrentObject",hBrowseLinje).
    RUN DisplayRecord.

    bSkipInitGrid = YES.
    DYNAMIC-FUNCTION("setCurrentObject",hBestHodeToolbar).
    RUN NewRecord.
    RUN SaveRecord.

    DYNAMIC-FUNCTION("setToolbar",hBrowseLinje,"enable").

    ASSIGN bAlt-S  = bTmpAlt-S
           bNewArt = NO.

    IF dTmpLevDato NE ? THEN 
      LevDato:SCREEN-VALUE = STRING(dTmpLevDato).

    THIS-PROCEDURE:CURRENT-WINDOW:WIDTH-PIXELS = THIS-PROCEDURE:CURRENT-WINDOW:WIDTH-PIXELS - 1.
    APPLY "WINDOW-RESIZED" TO {&WINDOW-NAME}.
    THIS-PROCEDURE:CURRENT-WINDOW:WIDTH-PIXELS = THIS-PROCEDURE:CURRENT-WINDOW:WIDTH-PIXELS + 1.
    APPLY "WINDOW-RESIZED" TO {&WINDOW-NAME}.
    
    DYNAMIC-FUNCTION("setAttribute",SESSION,"userKeepsWindowLocked","").
    DYNAMIC-FUNCTION("DoLockWindow",?).

    APPLY "entry" TO LevDato IN FRAME frmLinje.
  END.

END.
ELSE DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LeaveBrowseDropDown C-Win 
PROCEDURE LeaveBrowseDropDown :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF DYNAMIC-FUNCTION("getCurrentObject") = hBrwOLMerk AND CAN-DO("tab,enter",LAST-EVENT:LABEL) THEN DO:
  IF LAST-EVENT:LABEL = "tab" THEN
    DYNAMIC-FUNCTION("setWidgetEnter",btnOk:HANDLE IN FRAME frmOkReg).
  ELSE
    RUN LagreNyLinje.
END.

ELSE RUN SUPER.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LeaveOfField C-Win 
PROCEDURE LeaveOfField :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icFieldName AS CHAR NO-UNDO.

DEF VAR cReturnValue AS CHAR NO-UNDO.

CASE icFieldName:
  WHEN "LevDato" THEN DO WITH FRAME frmLinje:
    IF LevDato:MODIFIED THEN DO:
      IF (DATE(LevDato:SCREEN-VALUE) < TODAY - 30 OR DATE(LevDato:SCREEN-VALUE) > TODAY + 365) THEN DO:
        IF DYNAMIC-FUNCTION("DoMessage",0,4,"Er du sikker på at leveringsdato for bestillingen er " + LevDato:SCREEN-VALUE,"Advarsel","") = 7 THEN 
          RETURN NO-APPLY.
      END.
      dMinLevDato = DATE(LevDato:SCREEN-VALUE). 
    END.
    LevDato:MODIFIED = FALSE.
  END.
  WHEN "MesseNr" THEN DO WITH FRAME frmDetalj:
    IF MesseNr:MODIFIED THEN DO:
      ASSIGN MesseBeskrivelse:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldValues","Messe",
                                                               "WHERE MesseNr = '" + MesseNr:SCREEN-VALUE + "'",
                                                              "MesseBeskrivelse,MesseType")
             Kilde:SCREEN-VALUE = "0"
             VarebokBeskrivelse:SCREEN-VALUE = ""
             .
    END.
  END.
END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LeaveOfFieldHandle C-Win 
PROCEDURE LeaveOfFieldHandle :
/*------------------------------------------------------------------------------
  Purpose:     Retrieve lookup values when a foreign key field is modified
  Parameters:  Handle to foreign key field
  Notes:       The cReturn variable should be replaced with a fill-in
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihField AS HANDLE NO-UNDO.

DEF VAR cReturn AS CHAR NO-UNDO.

IF ihField:MODIFIED THEN DO WITH FRAME {&FRAME-NAME}:
  CASE ihField:NAME:
    WHEN "Kilde" THEN 
      ASSIGN cReturn = DYNAMIC-FUNCTION("getFieldValues","VareBokHode",
                                         "WHERE VareBokNr = '" + ihField:SCREEN-VALUE + "'","VarebokBeskrivelse,MesseNr")
             VareBokBeskrivelse:SCREEN-VALUE IN FRAME frmDetalj = ENTRY(1,cReturn,"|")
             Messenr:SCREEN-VALUE = ENTRY(2,cReturn,"|") 
             MesseBeskrivelse:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldValues","Messe",
                                                              "WHERE Messenr = " + Messenr:SCREEN-VALUE,"MesseBeskrivelse")
             .
  END CASE.
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
DEF VAR ocValue     AS CHAR NO-UNDO.
DEF VAR iConfirm    AS INT NO-UNDO.

iReturn = 2.

RUN JBoxDSimpleSelectList.w (hBrwOLMerk:LIST-ITEM-PAIRS,?,OUTPUT ocValue).

IF iReturn NE 0 AND iConfirm NE 2 AND ocValue NE ? THEN
  EndreMangeLinjer("LinjeMerknad",ocValue).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MakulerBestRecord C-Win 
PROCEDURE MakulerBestRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cBestNrList AS CHAR NO-UNDO.

DO ix = 1 TO hBrowseOrdreBest:NUM-SELECTED-ROWS:
  IF hBrowseOrdreBest:FETCH-SELECTED-ROW(ix) THEN
    cBestNrList = cBestNrList + STRING(hBrowseOrdreBest:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("BestNr"):BUFFER-VALUE) + "|".
END.
IF cBestNrList NE "" THEN DO:
  IF DYNAMIC-FUNCTION("DoMessage",0,4,"Avskriv rest for bestilling?","","") = 6 THEN
    IF NOT DYNAMIC-FUNCTION("runproc","ordre_best_makuler.p","fullbest;" 
                                                                + DYNAMIC-FUNCTION("getASuserId") + ";"
                                                                + TRIM(cBestNrList,"|"),?) THEN DO:
      DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
    END.
    ELSE DO:
      DYNAMIC-FUNCTION("RefreshRowids",hBrowseOrdre,hBrowseOrdre:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("RowIdent1"):BUFFER-VALUE).
      APPLY "value-changed" TO hBrowseOrdre.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MakulerOrdreRecord C-Win 
PROCEDURE MakulerOrdreRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN OrdreBehandling("makuler").

/* DEF VAR cOrdreNrList AS CHAR NO-UNDO.                                                                                          */
/*                                                                                                                                */
/* DO ix = 1 TO hBrowseOrdre:NUM-SELECTED-ROWS:                                                                                   */
/*   IF hBrowseOrdre:FETCH-SELECTED-ROW(ix) THEN                                                                                  */
/*     cOrdreNrList = cOrdreNrList + STRING(hBrowseOrdre:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("OrdreNr"):BUFFER-VALUE) + "|".  */
/* END.                                                                                                                           */
/* IF cOrdreNrList NE "" THEN DO:                                                                                                 */
/*   IF DYNAMIC-FUNCTION("DoMessage",0,4,"Makuler rest for valgt(e) ordre?","","") = 6 THEN                                       */
/*     IF NOT DYNAMIC-FUNCTION("runproc","ordre_best_makuler.p","fullordre;"                                                      */
/*                                                                 + DYNAMIC-FUNCTION("getASuserId") + ";"                        */
/*                                                                 + TRIM(cOrdreNrList,"|"),?) THEN DO:                           */
/*       DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").                                       */
/*     END.                                                                                                                       */
/*     ELSE DO:                                                                                                                   */
/*       DYNAMIC-FUNCTION("setCurrentObject",hBrowseOrdre).                                                                       */
/*       RUN OpenQuery.                                                                                                           */
/*     END.                                                                                                                       */
/* END.                                                                                                                           */
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
{&WINDOW-NAME}:WINDOW-STATE = 3.
{&WINDOW-NAME}:MOVE-TO-TOP().

APPLY "ENTRY" TO hBrowseListe.
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
DEF OUTPUT PARAMETER obOk AS LOG NO-UNDO.

DEF VAR hCurrObject AS HANDLE NO-UNDO.
DEF VAR iReturn     AS INT    NO-UNDO.

hCurrObject = DYNAMIC-FUNCTION("getCurrentObject").

IF hCurrObject = hUpdToolbar OR hCurrObject = hBestHodeToolbar THEN
  hCurrObject = DYNAMIC-FUNCTION("getLinkedObject",hCurrObject,"fieldmap","from").

CASE hCurrObject:
  WHEN hFieldMap THEN
    iReturn = DYNAMIC-FUNCTION("DoMessage",0,4,"Er du helt sikker på at du vil slette hele varehåndteringsboken med alle artikler?","","").
  WHEN hFieldMapLinje THEN
    iReturn = DYNAMIC-FUNCTION("DoMessage",0,4,"Slett artikkel (med tilhørende bestillinger eller varemottak)?","","").
  WHEN hFieldMapBestHode THEN
    iReturn = DYNAMIC-FUNCTION("DoMessage",0,4,"Slett bestilling/varemottak?","","").
  WHEN hBrowseOrdre OR WHEN hOrdreToolbar THEN 
    iReturn = DYNAMIC-FUNCTION("DoMessage",0,4,"Slett ordre (or alle underliggende bestillinger)?","","").
  WHEN hBrowseOrdreBest OR WHEN hBestToolbar THEN
    iReturn = DYNAMIC-FUNCTION("DoMessage",0,4,"Slett bestilling?","","").
END CASE.

obOk = iReturn = 6.

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
DEF VAR rNew          AS ROWID  NO-UNDO.
DEF VAR cType         AS CHAR   NO-UNDO.
DEF VAR cNyArtikkelNr AS CHAR   NO-UNDO.
DEF VAR hColumn       AS HANDLE NO-UNDO.
  
IF iTab = 1 THEN 
  TabStripChanged(12).

IF CAN-DO("1,2",STRING(iTab)) THEN DO WITH FRAME frmDetalj:
  RUN SUPER.

  ProfilNr:SCREEN-VALUE = cInitPrisProfil.

  IF cmbVarebehType:SCREEN-VALUE IN FRAME frmFilter = "0" THEN DO:
    RUN dVarebehType.w (OUTPUT cType, OUTPUT bOK).
    IF NOT bOK THEN DO:
      RUN UndoRecord.
      RETURN.
    END.
    ELSE cmbVarebehType:SCREEN-VALUE = cType.
  END.
  ELSE cType = cmbVarebehType:SCREEN-VALUE.

  RUN VelgButikker ("new", OUTPUT bOK).
  IF NOT bOk THEN DO:
    DYNAMIC-FUNCTION("setCurrentObject",hBrowseListe).
    RUN DisplayRecord.
    RETURN.
  END.
  
  APPLY "ENTRY" TO VarebehType.
  IF cType = ENTRY(1,cBestTypeDesc,"|") THEN
    ASSIGN VarebehType:SCREEN-VALUE = ENTRY(1,cBestTypeDesc,"|")
           BeskrivelseVareBehType:SCREEN-VALUE = ENTRY(2,cBestTypeDesc,"|")
           .
  ELSE
    ASSIGN VarebehType:SCREEN-VALUE = ENTRY(1,cInnlevTypeDesc,"|")
           BeskrivelseVareBehType:SCREEN-VALUE = ENTRY(2,cInnlevTypeDesc,"|")
           .
END.
ELSE IF iTab = 3 THEN DO WITH FRAME frmLinje:
  IF DYNAMIC-FUNCTION("getCurrentObject") = hUpdToolbar THEN DO:
    IF sokLevnr:SCREEN-VALUE NE "0" THEN DO:
      DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"uselocaldata","yes").
      DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"insertbrowserow","yes").
      IF DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"currviewfields") NE cCreateViewFields THEN
        setView("create").
      setOverlays("link").
      IF /* NOT CAN-FIND(FIRST ttInit) AND */ hFieldMapLinje:AVAIL THEN HentVerdier().
      RUN SUPER.
      setOverlayValues("","").
      hBrwOLLevKod:MODIFIED = YES.
      ViewFrameOkReg().
      APPLY "entry" TO hBrwOLLevKod.
    END.
    ELSE DO:
      THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
      RUN ArtBasVedlikehold.w (THIS-PROCEDURE,"new",OUTPUT bOK).
      THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.
      THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
  
      IF bOK THEN DO:
        IF hFieldMap:BUFFER-FIELD("VareBehType"):BUFFER-VALUE = iBestVarebeh THEN DO:
          IF dMinLevDato NE ? THEN DO:
            LevDato:SCREEN-VALUE = STRING(dMinLevDato).
            APPLY "entry" TO Grid.
          END.
          ELSE IF dSisteLevDato NE ? THEN DO:
            LevDato:SCREEN-VALUE = STRING(dSisteLevDato).
            APPLY "entry" TO Grid.
          END.
          ELSE APPLY "entry" TO LevDato.
        END.
        ELSE APPLY "entry" TO Grid.
      END.
    END.
  END.
  ELSE DO: 
    IF NOT (bHKinst AND cButikkListe = "*") THEN DO:
      DYNAMIC-FUNCTION("setAttribute",hBestHodeToolbar,"disabledevents","").
      RUN InitGrid("clear").
      RUN SUPER.
      RUN SaveRecord.
      IF hFieldMap:BUFFER-FIELD("VarebehType"):BUFFER-VALUE NE iBestVarebeh THEN DO:
        Grid:SENSITIVE = TRUE.
        APPLY "entry" TO Grid.
      END. 
      ELSE DO:
        Grid:SENSITIVE = TRUE.
        DirekteLev:CHECKED IN FRAME frmLinje = LOGICAL(cDefaultDirLev).
        IF dMinLevDato NE ? THEN DO:
          LevDato:SCREEN-VALUE = STRING(dMinLevDato).
          APPLY "entry" TO Grid.
        END.
        ELSE IF dSisteLevDato NE ? THEN DO:
          LevDato:SCREEN-VALUE = STRING(dSisteLevDato).
          APPLY "entry" TO Grid.
        END.
        ELSE APPLY "entry" TO LevDato.
      END.
    END.
    ELSE DYNAMIC-FUNCTION("DoMessage",0,0,"Kun butikkbrukere kan opprette suppleringsordre på HK","","").
  END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE newVarebehLinje C-Win 
PROCEDURE newVarebehLinje :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ifArtikkelNr AS HANDLE NO-UNDO.

DEF VAR phBuffer AS HANDLE NO-UNDO.
DEF VAR rRepos   AS ROWID NO-UNDO.
DEF VAR cRowId   AS CHAR  NO-UNDO.


IF DYNAMIC-FUNCTION("runProc","varebehlinje_new.p",STRING(ifArtikkelNr) + "|" + VarebehNr:SCREEN-VALUE IN FRAME frmDetalj
                    ,?) THEN
  cRowId = DYNAMIC-FUNCTION("getTransactionMessage").
ELSE DO:
  DYNAMIC-FUNCTION("DoMessage",0,0,
                   DYNAMIC-FUNCTION("getTransactionMessage"),"","").
  RETURN.
END.

IF cRowId NE "" THEN DO:
  rRowIdLinje = TO-ROWID(cRowId).   /* Se NyArtbas - skal skrives om.. */
  IF hBrowseLinje:FOCUSED-ROW NE ? THEN
    hBrowseLinje:SELECT-ROW(hBrowseLinje:FOCUSED-ROW).
  hFieldMapLinje:BUFFER-CREATE().
  hFieldMapLinje:BUFFER-FIELD("Rowident1"):BUFFER-VALUE = cRowId.
  rRepos = hFieldMapLinje:ROWID.

  hBrowseLinje:QUERY:QUERY-OPEN().
  hBrowseLinje:SET-REPOSITIONED-ROW(hBrowseLinje:FOCUSED-ROW,"conditional").
  hBrowseLinje:QUERY:REPOSITION-TO-ROWID(rRepos).

  IF DYNAMIC-FUNCTION("refreshRowids",hBrowseLinje,cRowId) = 0 THEN
    hBrowseLinje:DELETE-CURRENT-ROW().

  DYNAMIC-FUNCTION("setCurrentObject",hBrowseLinje).
  RUN DisplayRecord.
END.
ELSE DO:
  rRowIdLinje = ?.  
  IF RETURN-VALUE NE "OK" THEN
    MESSAGE RETURN-VALUE
        VIEW-AS ALERT-BOX WARNING BUTTONS OK.
  ELSE
    RUN InvokeMethod(hBrowseLinje,"OpenQuery").
  APPLY "ENTRY":U TO hBrowseLinje.
  RETURN NO-APPLY.
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

DEF VAR bTmpAlt-S AS LOG NO-UNDO.

bOK = hFieldMapLinje:FIND-FIRST("WHERE VarebehNr = " + STRING(hFieldMap:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE) 
                               + " AND ArtikkelNr = " + STRING(ifArtikkelNr)
                                ,NO-LOCK) NO-ERROR.
ASSIGN bNewArt   = TRUE
       bTmpAlt-S = bAlt-S
       bAlt-S    = FALSE
       .

IF bOK THEN DO:
  hBrowseLinje:QUERY:REPOSITION-TO-ROWID(hFieldMapLinje:ROWID) NO-ERROR.
  IF NOT ERROR-STATUS:ERROR THEN 
    APPLY "value-changed" TO hBrowseLinje.
END.
ELSE DO:
  RUN newVarebehLinje (ifArtikkelNr).
  IF rRowIdLinje <> ? THEN DO:
    DYNAMIC-FUNCTION("setCurrentObject",hBestHodeToolbar).
    RUN NewRecord.
    RUN SaveRecord.

    DYNAMIC-FUNCTION("setToolbar",hBrowseLinje,"enable").

    bAlt-S = bTmpAlt-S.
    APPLY "entry" TO LevDato IN FRAME frmLinje.
  END.
END.

bNewArt = FALSE.
FreezeWin (FALSE).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NyVarebeh C-Win 
PROCEDURE NyVarebeh :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icType   AS CHAR NO-UNDO.

DEF VAR cFieldList AS CHAR NO-UNDO.
DEF VAR cValueList AS CHAR NO-UNDO.
DEF VAR cLevNr     AS CHAR NO-UNDO.
DEF VAR rRepos     AS ROWID NO-UNDO.

/*cmbVarebehType:SENSITIVE IN FRAME frmFilter = NO.*/

IF CAN-DO("1,2",icType) THEN DO:
  cmbVarebehType:SCREEN-VALUE IN FRAME frmFilter = icType.

  IF icType = "2" THEN
    ASSIGN THIS-PROCEDURE:CURRENT-WINDOW:TITLE = "Direkte varemottak"
           tbMatchBestStat:SENSITIVE = NO
           btnLinjemerknad:SENSITIVE IN FRAM frmLinje = NO
           sokRegBestStat:SENSITIVE = NO
           sokGlobOrdrenr:SENSITIVE = NO
           tbKundeOrdre:HIDDEN      = YES
           .
  ELSE DO:
    IF cButikkListe NE "*" AND bHKinst THEN
      DYNAMIC-FUNCTION("setAttribute",hOrdreToolbar,"disabledevents","InnlevOrdre,MakulerOrdre,copy").
    IF NOT bHKInst OR (bHKinst AND cButikkListe = "*") THEN 
    DO:
      hBrwOLMerk = DYNAMIC-FUNCTION("NewBrowseDropDown",
                        hBrowseLinje,                  
                        "LinjeMerknad",                
                        "LinjeMerknad",                
                        "","","du|mmy").                           
      ASSIGN hBrwOLMerk:NAME   = "fiLinjeMerknad"
             hBrwOLMerk:HELP   = "Merknad"
             hBrwOLMerk:FORMAT = "x(256)".
      DYNAMIC-FUNCTION("setAttribute",hBrwOLMerk,"refreshrow","yes").
      DYNAMIC-FUNCTION("CreateOverlayLink",hBrowseLinje,hBrwOLMerk,"LinjeMerknad"). 
    END.
  END.

  RUN MoveToTop.

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  
  RUN NyButVarebeh.w (icType,OUTPUT cFieldList, OUTPUT cValueList, OUTPUT cLevNr).
  
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.
  
  IF cFieldList NE "" THEN DO:
    DYNAMIC-FUNCTION("DoLockWindow",THIS-PROCEDURE:CURRENT-WINDOW).
    IF DYNAMIC-FUNCTION("DoCreate","VarebehHode","ignore",
                               cFieldList,
                               cValueList,
                               TRUE) THEN DO:
  
      hBrowseListe:QUERY:GET-BUFFER-HANDLE(1):BUFFER-CREATE.
  
      rRepos = hBrowseListe:QUERY:GET-BUFFER-HANDLE(1):ROWID.
      DYNAMIC-FUNCTION("DoRefetchTrans",hBrowseListe:QUERY:GET-BUFFER-HANDLE(1),"FIRST","").
      DYNAMIC-FUNCTION("refreshRow",hBrowseListe,
                        DYNAMIC-FUNCTION("getAttribute",hBrowseListe,"buffersandfields"),
                        DYNAMIC-FUNCTION("getAttribute",hBrowseListe,"queryjoin")).
  
      hBrowseListe:QUERY:QUERY-OPEN().
      hBrowseListe:SET-REPOSITIONED-ROW(hBrowseListe:DOWN,"conditional").
      hBrowseListe:QUERY:REPOSITION-TO-ROWID(rRepos).
  
      APPLY "value-changed" TO hBrowseListe.
      hBrowseListe:QUERY:REPOSITION-TO-ROWID(rRepos).
  
  
      TabStripChanged(13).
  
      IF cLevNr NE "" THEN DO:
        sokLevNr:SCREEN-VALUE IN FRAME frmLinje = cLevnr.
        APPLY "tab" TO sokLevNr.
      END.
      ELSE RUN OpenQuery.
  
    END.
    ELSE DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
    DYNAMIC-FUNCTION("DoLockWindow",?).
  END.
  ELSE RUN OpenQuery.
END.
ELSE DO:
  cmbVarebehType:SCREEN-VALUE IN FRAME frmFilter = icType.
  /*IF NOT bHKInst OR (bHKinst AND cButikkListe = "*") THEN */
  DO:
    hBrwOLMerk = DYNAMIC-FUNCTION("NewBrowseDropDown",
                      hBrowseLinje,                  
                      "LinjeMerknad",                
                      "LinjeMerknad",                
                      "","","du|mmy").                           
    ASSIGN hBrwOLMerk:NAME   = "fiLinjeMerknad"
           hBrwOLMerk:HELP   = "Merknad"
           hBrwOLMerk:FORMAT = "x(256)".
    DYNAMIC-FUNCTION("setAttribute",hBrwOLMerk,"refreshrow","yes").
    DYNAMIC-FUNCTION("CreateOverlayLink",hBrowseLinje,hBrwOLMerk,"LinjeMerknad"). 
  END.
  RUN OpenQuery.
  RUN MoveToTop.
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
DEF VAR iUtskrNr                  AS INT  NO-UNDO.
DEF VAR cThisAktivitetJoin        AS CHAR NO-UNDO.
DEF VAR cBestHodeFilter           AS CHAR NO-UNDO.
DEF VAR cCalcparamSumAntall       AS CHAR NO-UNDO.
DEF VAR cStatusLabel              AS CHAR NO-UNDO.

IF iTab = 1 THEN DO WITH FRAME frmFilter:
  ASSIGN fi-cEkstRef:MODIFIED = NO 
         fi-cVarebehBeskrivelse:MODIFIED = NO 
         fi-fMesseNr:MODIFIED = NO 
         fi-fProfilNr:MODIFIED = NO 
         fi-fVarebehNr:MODIFIED = NO
         .

  DYNAMIC-FUNCTION("SetCurrentObject",hBrowseListe).
  DYNAMIC-FUNCTION("SetAttribute",hBrowseListe,"queryfilter",
                   (IF fi-fProfilNr:SCREEN-VALUE NE "0" THEN
                     " where ProfilNr = " + fi-fProfilNr:SCREEN-VALUE
                    ELSE "where true") +
                   (IF fi-fVarebehNr:SCREEN-VALUE NE "0" THEN
                     " AND VarebehNr = " + fi-fVarebehNr:SCREEN-VALUE
                    ELSE "") +
                   (IF cmbVarebehType:SCREEN-VALUE NE "0" THEN
                     " AND VarebehType = " + cmbVarebehType:SCREEN-VALUE
                    ELSE "") +
                   (IF fi-fMesseNr:SCREEN-VALUE NE "0" THEN
                     " AND MesseNr = " + fi-fMesseNr:SCREEN-VALUE
                    ELSE "") +
                   (IF fi-cVarebehBeskrivelse:SCREEN-VALUE NE "" THEN
                      IF fi-cVarebehBeskrivelse:SCREEN-VALUE BEGINS "*" THEN
                       " AND VarebehBeskrivelse MATCHES '" + fi-cVarebehBeskrivelse:SCREEN-VALUE + "*'"
                      ELSE
                       " AND VarebehBeskrivelse BEGINS '" + fi-cVarebehBeskrivelse:SCREEN-VALUE + "'"
                    ELSE "") +
                   (IF fi-cEkstRef:SCREEN-VALUE NE "" THEN
                      IF fi-cEkstRef:SCREEN-VALUE BEGINS "*" THEN
                       " AND EkstRef MATCHES '" + fi-cEkstRef:SCREEN-VALUE + "*'"
                      ELSE
                       " AND EkstRef BEGINS '" + fi-cEkstRef:SCREEN-VALUE + "'"
                    ELSE "")
/*                  + (IF cButikkListe NE "*" AND cButikkListe NE "" THEN  */
/*                       " AND Oppdatert"                                  */
/*                     ELSE "")                                            */
                   ).
  DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"queryfilter"," and false").

  IF cLevBasIdList NE "" THEN
    DYNAMIC-FUNCTION("setAttribute",hBrowseListe,"calcparamLevNrList",REPLACE(cLevBasIdList,"|","¤")).
  ELSE
    DYNAMIC-FUNCTION("setAttribute",hBrowseListe,"calcparamLevNrList","").

  IF tbKundeOrdre:CHECKED THEN
    DYNAMIC-FUNCTION("setAttribute",hBrowseListe,"calcparamKundeordreBest","kordre").
  ELSE
    DYNAMIC-FUNCTION("setAttribute",hBrowseListe,"calcparamKundeordreBest","").
END.

ELSE IF iTab = 3 THEN DO WITH FRAME frmLinje:
  ASSIGN sokFraEdato sokFraVPIdato sokTilEdato sokTilVPIdato sokFraRegDatoBestInnl sokTilRegDatoBestInnl sokArtikkelNr sokLevKod
         sokAvdelingNavn:MODIFIED       = FALSE
         sokAvdelingNr:MODIFIED         = FALSE
         sokHg:MODIFIED                 = FALSE
         sokHgBeskr:MODIFIED            = FALSE
         sokVg:MODIFIED                 = FALSE
         sokVgBeskr:MODIFIED            = FALSE
         sokAktNr:MODIFIED              = FALSE
         sokAktBeskrivelse:MODIFIED     = FALSE
         sokBeskr:MODIFIED              = FALSE
         sokArtikkelNr:MODIFIED         = FALSE
         sokFraEdato:MODIFIED           = FALSE
         sokFraVPIdato:MODIFIED         = FALSE
         sokTilEdato:MODIFIED           = FALSE
         sokTilVPIdato:MODIFIED         = FALSE
         sokFraRegDatoBestInnl:MODIFIED = FALSE
         sokTilRegDatoBestInnl:MODIFIED = FALSE
         sokLinjeMerknad:MODIFIED       = FALSE
         sokLevKod:MODIFIED             = NO
         .
  IF DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"uselocaldata") NE "yes" THEN
    setView("").

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
                    ELSE "")
                 + (IF sokBeskr:SCREEN-VALUE NE "" THEN
                     " AND Utvidetsok CONTAINS '" + sokBeskr:SCREEN-VALUE + "'"
                    ELSE "") 
/*                  + (IF sokLinjeMerknad:SCREEN-VALUE NE "" THEN            */
/*                      " AND LinjeMerknad " +                               */
/*                      (IF INDEX(sokLinjeMerknad:SCREEN-VALUE,"*") > 0 THEN */
/*                        "MATCHES '" + sokLinjeMerknad:SCREEN-VALUE + "*'"  */
/*                       ELSE                                                */
/*                        "BEGINS '" + sokLinjeMerknad:SCREEN-VALUE + "'")   */
/*                     ELSE "")                                              */
                 + (IF sokLevKod NE "" THEN
                     " AND LevKod " +
                     (IF INDEX(sokLevKod,"*") > 0 THEN "MATCHES" ELSE "BEGINS") +
                      " '" + sokLevKod:SCREEN-VALUE + "'"
                    ELSE "")
                 + (IF sokArtikkelNr NE 0 THEN
                     " AND ArtikkelNr = " + sokArtikkelNr:SCREEN-VALUE
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
                    ).

  IF sokAktNr:SCREEN-VALUE NE "0" THEN 
    cThisAktivitetJoin = ",first VgAkt WHERE VgAkt.Vg = VarebehLinje.Vg AND AktNr = " + sokAktNr:SCREEN-VALUE 
                       + ",first Aktivitet OF VgAkt".
  ELSE IF sokAktBeskrivelse:SCREEN-VALUE NE "" THEN
    cThisAktivitetJoin = ",EACH VgAkt WHERE VgAkt.Vg = VarebehLinje.Vg" 
                       + ",first Aktivitet OF VgAkt WHERE Beskrivelse " 
                       + (IF INDEX(sokAktBeskrivelse:SCREEN-VALUE,"*") > 0 THEN 
                           "MATCHES '" + sokAktBeskrivelse:SCREEN-VALUE + "*'"
                          ELSE
                           "BEGINS '" + sokAktBeskrivelse:SCREEN-VALUE + "'").
  ELSE
    cThisAktivitetJoin = cAktivitetJoin.

  IF sokFraRegDatoBestInnl NE ? AND sokTilRegDatoBestInnl NE ? THEN DO:
    DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"calcparamSumAntall",sokFraRegDatoBestInnl:SCREEN-VALUE + "¤" + sokTilRegDatoBestInnl:SCREEN-VALUE).
    cBestHodeFilter = " VarebehBestHode.RegistrertDato GE DATE('" + sokFraRegDatoBestInnl:SCREEN-VALUE + "') " +
                        " AND VarebehBestHode.RegistrertDato LE DATE('" + sokTilRegDatoBestInnl:SCREEN-VALUE + "')".
  END.
  ELSE IF sokFraRegDatoBestInnl NE ? THEN DO:
    DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"calcparamSumAntall",sokFraRegDatoBestInnl:SCREEN-VALUE + "¤").
    cBestHodeFilter = " VarebehBestHode.RegistrertDato GE DATE('" + sokFraRegDatoBestInnl:SCREEN-VALUE + "')".
  END.
  ELSE IF sokTilRegDatoBestInnl NE ? THEN DO:
    DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"calcparamSumAntall","¤" + sokFraRegDatoBestInnl:SCREEN-VALUE).
    cBestHodeFilter = " VarebehBestHode.RegistrertDato LE DATE('" + sokTilRegDatoBestInnl:SCREEN-VALUE + "')".
  END.
  ELSE DO:
    DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"calcparamSumAntall","").
    cBestHodeFilter = "".
  END.

  cCalcparamSumAntall = DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"calcparamSumAntall").
  IF cCalcparamSumAntall = "" THEN cCalcparamSumAntall = "¤".

  IF sokRegBestStat:SCREEN-VALUE NE ? AND sokRegBestStat:SCREEN-VALUE NE "" THEN DO:
    DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"calcparamBBestilt",sokRegBestStat:SCREEN-VALUE + "¤" + REPLACE(cButikkListe,",",CHR(1))).
    DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"calcparamSumAntall",
                     cCalcparamSumAntall + "¤" + sokRegBestStat:SCREEN-VALUE + "¤" + REPLACE(cButikkListe,",",CHR(1)) ).
    DYNAMIC-FUNCTION("setAttribute",hBrowseBestHode,"calcparamBestHodeBestnr",sokRegBestStat:SCREEN-VALUE + "¤" + REPLACE(cButikkListe,",",CHR(1))).
  END.
  ELSE DO:
    DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"calcparamBBestilt","").
    DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"calcparamSumAntall",
                     cCalcparamSumAntall + "¤¤" + REPLACE(cButikkListe,",",CHR(1)) ).
    DYNAMIC-FUNCTION("setAttribute",hBrowseBestHode,"calcparamBestHodeBestnr","¤" + REPLACE(cButikkListe,",",CHR(1))).
  END.
  DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"calcparamBIkkeSendtBest",REPLACE(cButikkListe,",",CHR(1))).
  
  cCalcparamSumAntall = DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"calcparamSumAntall").
 
  DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"calcparamSumVarekost",cCalcparamSumAntall).
  DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"calcparamSumPris",cCalcparamSumAntall).
  DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"calcparamSumDB",cCalcparamSumAntall).

  DYNAMIC-FUNCTION("setAttribute",hBrowseBestHode,"queryfilter","").

  IF cBestHodeFilter NE "" AND DYNAMIC-FUNCTION("getCurrentObject") = hBrowseBestHode THEN 
    DYNAMIC-FUNCTION("setAttribute",hBrowseBestHode,"queryfilter"," AND " + cBestHodeFilter).

  DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"queryjoin",cVarebehlinjeJoin + cThisAktivitetJoin).

  IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowseLinje THEN 
    DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"querystatfields",IF bUpdateStat THEN cStatFieldNames ELSE "").

  DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"calcparamBMatchMerknad",REPLACE(sokLinjeMerknad:SCREEN-VALUE,",","¤")).

  IF sokSaSong:SCREEN-VALUE NE "0" THEN
    DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"calcparamSaSongList",sokSaSong:SCREEN-VALUE).
  ELSE IF cSaSongIdList NE "" THEN
    DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"calcparamSaSongList",REPLACE(cSaSongIdList,"|","¤")).
  ELSE
    DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"calcparamSaSongList","").
END.

ELSE IF iTab = 4 THEN DO WITH FRAME frmOrdre:
  ASSIGN sokOrdreCL sokOrdreLevDato sokOrdreLevNr sokOrdreNr sokEkstId sokOrdreLevKod sokOrdreLevFargKod
         sokOrdreLevDato:MODIFIED       = FALSE
         sokOrdreLevNr:MODIFIED         = FALSE
         sokOrdreNr:MODIFIED            = FALSE
         sokBestOrdreStatus:MODIFIED    = FALSE
         sokOrdreCL:MODIFIED            = FALSE
         sokEkstId:MODIFIED             = NO
         sokOrdreLevFargKod:MODIFIED    = NO
         sokOrdreLevKod:MODIFIED        = NO
         .
  DYNAMIC-FUNCTION("SetCurrentObject",hBrowseOrdre).
  DYNAMIC-FUNCTION("setAttribute",hBrowseOrdre,"queryfilter","").

  DYNAMIC-FUNCTION("setAttribute",hBrowseOrdre,"QueryFilter",
                   (IF sokOrdreCL NE 0 THEN
                     " AND CL = " + sokOrdreCL:SCREEN-VALUE
                    ELSE "")
                 + (IF sokOrdreLevDato NE ? THEN
                     " AND LeveringsDato = DATE('" + sokOrdreLevDato:SCREEN-VALUE + "')"
                    ELSE "")
                 + (IF sokOrdreLevNr:SCREEN-VALUE NE "0" THEN
                     " AND LevNr = " + sokOrdreLevNr:SCREEN-VALUE
                    ELSE IF sokOrdreLevNamn:SCREEN-VALUE NE "" AND NUM-ENTRIES(cLevBasOrdreRowIdList) > 1 THEN
                      " AND CAN-DO('" + REPLACE(cLevBasOrdreIdList,"|",",") + "',STRING(LevNr))"
                    ELSE "")
                 + (IF sokOrdreNr NE 0 THEN 
                     " AND OrdreNr = " + sokOrdreNr:SCREEN-VALUE
                    ELSE "")
/*                  + (IF sokEkstId NE "" THEN                             */
/*                      " AND EkstId = '" + sokOrdreNr:SCREEN-VALUE + "'"  */
/*                     ELSE "")                                            */
                 + (IF sokOrdremottaker:SCREEN-VALUE NE "" AND sokOrdremottaker:SCREEN-VALUE NE ? THEN 
                     " AND Ordremottaker = '" + sokOrdremottaker:SCREEN-VALUE + "'"
                    ELSE "")
                 + (IF sokOpphav:SCREEN-VALUE NE "" AND sokOpphav:SCREEN-VALUE NE ? THEN 
                     " AND Opphav = '" + sokOpphav:SCREEN-VALUE + "'"
                    ELSE "")
                 + (IF NUM-ENTRIES(cButikkListe) = 1 AND cButikkListe NE "*" THEN
                     " AND CL = " + cButCl
                    ELSE "")
                   ).

  IF sokBestOrdreStatus:SCREEN-VALUE NE ? AND sokBestOrdreStatus:SCREEN-VALUE NE "" THEN 
    DYNAMIC-FUNCTION("setAttribute",hBrowseOrdre,"calcparamStatusOgButikkSjekk",sokBestOrdreStatus:SCREEN-VALUE + "¤" + REPLACE(cButikkListe,",",CHR(1))).
  ELSE
    DYNAMIC-FUNCTION("setAttribute",hBrowseOrdre,"calcparamStatusOgButikkSjekk","¤" + REPLACE(cButikkListe,",",CHR(1))).

  IF sokOrdreArtikkel:SCREEN-VALUE NE "0" OR sokEkstId NE "" OR sokOrdreLevKod NE "" OR sokOrdreLevFargKod NE "" THEN
    DYNAMIC-FUNCTION("setAttribute",hBrowseOrdre,"prescanqueryfilter",
                     "BestHode WHERE VarebehNr = " + STRING(hFieldMap:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE)
                   + (IF sokOrdreArtikkel:SCREEN-VALUE NE "0" THEN 
                        " AND Artikkelnr = " + sokOrdreArtikkel:SCREEN-VALUE
                      ELSE "")
                   + (IF sokEkstId NE "" THEN 
                       " AND EkstId = '" + sokEkstId + "'"
                      ELSE "")
                   + (IF sokOrdreLevKod NE "" THEN 
                       " AND LevKod BEGINS '" + sokOrdreLevKod + "'"
                      ELSE "")
                   + (IF sokOrdreLevFargKod NE "" THEN 
                       " AND LevFargKod BEGINS '" + sokOrdreLevFargKod + "'"
                      ELSE "")
                   + ",FIRST Ordre OF BestHode NO-LOCK").
  ELSE
    DYNAMIC-FUNCTION("setAttribute",hBrowseOrdre,"prescanqueryfilter","").

/*   DYNAMIC-FUNCTION("setAttribute",hBrowseOrdre,"calcparamKjedevare",IF cmbKjedevare:SCREEN-VALUE NE ? THEN cmbKjedevare:SCREEN-VALUE ELSE "").                          */
/*   DYNAMIC-FUNCTION("setAttribute",hBrowseOrdre,"calcparamGjennomfaktureres",IF cmbGjennomfaktureres:SCREEN-VALUE NE ? THEN cmbGjennomfaktureres:SCREEN-VALUE ELSE "").  */
END.

RUN SUPER.

RUN SettSensitive.

IF iTab = 1 THEN DO:
  IF NOT hFieldMap:AVAIL AND hBrowseListe:QUERY:IS-OPEN THEN
    hBrowseListe:QUERY:GET-FIRST().
  APPLY "entry" TO hBrowseListe.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpphevSendtRecord C-Win 
PROCEDURE OpphevSendtRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cDummy        AS CHAR NO-UNDO.
DEF VAR iReturn       AS INT  NO-UNDO.
DEF VAR cOrdreNrList  AS CHAR NO-UNDO.

RUN JBoxBrowseMsgUpdSelVal.w ("Sett valgte ordre tilbake til status NY",
                              hBrowseOrdre:NUM-SELECTED-ROWS,
                              INT(DYNAMIC-FUNCTION("getAttribute",hBrowseOrdre,"recordcount")),
                              "",
                              OUTPUT cDummy,
                              OUTPUT iReturn).

IF iReturn = 0 THEN RETURN.


IF iReturn = 2 THEN DO: /* Behandle valgte */
  DO ix = 1 TO hBrowseOrdre:NUM-SELECTED-ROWS:
    IF hBrowseOrdre:FETCH-SELECTED-ROW(ix) THEN
      cOrdreNrList = cOrdreNrList + STRING(hBrowseOrdre:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("OrdreNr"):BUFFER-VALUE) + "|".
  END.

  IF DYNAMIC-FUNCTION("runproc","ordre_sett_til_statusny.p",TRIM(cOrdreNrList,"|"),?) THEN DO:
    DYNAMIC-FUNCTION("setCurrentObject",hBrowseOrdre).
    RUN OpenQuery.
  END.
  ELSE DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OrdreBehandling C-Win 
PROCEDURE OrdreBehandling :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT  PARAM icAksjon AS CHAR NO-UNDO.

DEF VAR cOrdreNrList AS CHAR  NO-UNDO.
DEF VAR cMsg         AS CHAR  NO-UNDO.
DEF VAR cMelding     AS CHAR  NO-UNDO.
DEF VAR cServerProg  AS CHAR  NO-UNDO.
DEF VAR iReturn      AS INT   NO-UNDO.
DEF VAR cDato        AS CHAR  NO-UNDO.
DEF VAR cParam       AS CHAR  NO-UNDO.
DEF VAR cRepos       AS CHAR  NO-UNDO.
DEF VAR iFocusRow    AS INT   NO-UNDO.

iFocusRow = hBrowseOrdre:FOCUSED-ROW.
IF iFocusRow NE 0 THEN 
  cRepos = hBrowseOrdre:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("RowIdent1"):BUFFER-VALUE.

CASE icAksjon:
  WHEN "innlev" THEN
    ASSIGN cMelding    = "Registrer varemottak for ordre?"
           cServerProg = "ordre_best_full_innlev.p"
           cParam      = "fullordre;"
           .
  WHEN "makuler" THEN
    ASSIGN cMelding    = "Avskriv rest for ordre?"
           cServerProg = "ordre_best_makuler.p"
           cParam      = "fullordre;"
           .
  WHEN "send" THEN
    ASSIGN cMelding    = "Sett ordre til status sendt?"
           cServerProg = "ordre_sett_status.p"
           cParam      = "sendt;"
           .
  WHEN "bekreft" THEN
    ASSIGN cMelding    = "Sett ordre til bekreftet?"
           cServerProg = "ordre_best_bekreft.p"
           cParam      = "fullordre"
           .
  WHEN "levdato" THEN
    ASSIGN cMelding    = "Endre leveringsdato?"
           cServerProg = "ordre_best_nylevdato.p"
           cParam      = "fullordre"
           .
  WHEN "resend" THEN
    ASSIGN cMelding    = "Resend ordre til butikk"
           cServerProg = "ordre_resend_til_butikk.p"
           cParam      = "resend;"
           .
  OTHERWISE RETURN.
END CASE.

IF icAksjon = "innlev" THEN DO:
  iReturn = DYNAMIC-FUNCTION("DoMessage",0,1,"Bekreft full innlevering av ordre","","").  /* 1 = OK, 2 = Cancel */
  IF iReturn = 2 THEN iReturn = 0.
  ELSE iReturn = 2.
END.
ELSE 
  RUN JBoxBrowseMsgUpdateVal.w (cMelding,
                                hBrowseOrdre:NUM-SELECTED-ROWS * -1,
                                INT(DYNAMIC-FUNCTION("getAttribute",hBrowseOrdre,"recordcount")),
                                IF CAN-DO("bekreft,levdato",icAksjon) THEN "date|99/99/9999|" + STRING(TODAY) ELSE "",
                                OUTPUT cDato,
                                OUTPUT iReturn).

IF iReturn = 0 THEN RETURN "cancel".

IF CAN-DO("bekreft,levdato",icAksjon) THEN DO:
  IF DATE(cDato) = ? OR DATE(cDato) < TODAY THEN DO:
    DYNAMIC-FUNCTION("DoMessage",0,0,"Ugyldig dato","","").
    RETURN.
  END.
  /* TN 25/6-06 Lagt inn ";" som separator istedenfor "|". */
  /* BHa 12/10-06: Lagt tilbake | som separator. Dato er en ekstra parameter kun for bekreftelse / endring av lev.dato */
  cParam = cParam + "|" + cDato + ";".
END.

cParam = cParam + DYNAMIC-FUNCTION("getASuserId").

IF iReturn = 2 THEN DO: /* Behandle valgte */
  DO ix = 1 TO hBrowseOrdre:NUM-SELECTED-ROWS:
    IF hBrowseOrdre:FETCH-SELECTED-ROW(ix) THEN
      cOrdreNrList = cOrdreNrList + STRING(hBrowseOrdre:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("OrdreNr"):BUFFER-VALUE) + "|".
  END.
  IF NOT DYNAMIC-FUNCTION("runproc",cServerProg,cParam + ";"
                                              + TRIM(cOrdreNrList,"|"),?) THEN DO:
    cMsg = DYNAMIC-FUNCTION("getTransactionMessage").
    DYNAMIC-FUNCTION("DoMessage",0,
                     IF INDEX(cMsg,CHR(10)) > 0 THEN 20 ELSE 0,
                     cMsg,"","").
  END.
  ELSE IF icAksjon = "innlev" THEN 
    SkrivEtikett(DYNAMIC-FUNCTION("getTransactionMessage")).
END.
ELSE DO: /* Behandle alle */
  IF NOT DYNAMIC-FUNCTION("ProcessQuery",hBrowseOrdre,"ordre_behandling.p",cServerProg + ";" + cParam) THEN DO:
    cMsg = DYNAMIC-FUNCTION("getTransactionMessage").
    DYNAMIC-FUNCTION("DoMessage",0,
                     IF INDEX(cMsg,CHR(10)) > 0 THEN 20 ELSE 0,
                     cMsg,"","").
    RETURN.
  END.
  ELSE IF icAksjon = "innlev" THEN 
    SkrivEtikett(DYNAMIC-FUNCTION("getTransactionMessage")).
END.

DYNAMIC-FUNCTION("setCurrentObject",hBrowseOrdre).
RUN OpenQuery.

IF iFocusRow NE 0 THEN DO:
  bOk = hBrowseOrdre:QUERY:GET-BUFFER-HANDLE(1):FIND-FIRST("WHERE RowIdent1 = '" + cRepos + "'") NO-ERROR.
  IF bOk THEN DO:
    hBrowseOrdre:SET-REPOSITIONED-ROW(iFocusRow,"always").
    hBrowseOrdre:QUERY:REPOSITION-TO-ROWID(hBrowseOrdre:QUERY:GET-BUFFER-HANDLE(1):ROWID) NO-ERROR.
    IF NOT ERROR-STATUS:ERROR THEN DO:
      DYNAMIC-FUNCTION("setCurrentObject",hBrowseOrdre).
      RUN DisplayRecord.
      hBrowseOrdre:SELECT-ROW(iFocusRow) NO-ERROR.
    END.
  END.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OverforVarebokRecord C-Win 
PROCEDURE OverforVarebokRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iReturn    AS INT NO-UNDO.

RUN JBoxBrowseSelectMsg.w ("Overfør artikler til varebok",
                              hBrowseLinje:NUM-SELECTED-ROWS,
                              DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"Totalcount"),
                              OUTPUT iReturn).

IF iReturn = 1 THEN
  DYNAMIC-FUNCTION("ProcessQuery",hBrowseLinje,"varebehlinje_kopiertilvarebok.p",hFieldMap:BUFFER-FIELD("Kilde"):STRING-VALUE).
ELSE IF iReturn = 2 THEN
  DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowseLinje,"varebehlinje_kopiertilvarebok.p",hFieldMap:BUFFER-FIELD("Kilde"):STRING-VALUE).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OverforVaremottakRecord C-Win 
PROCEDURE OverforVaremottakRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cDummy        AS CHAR NO-UNDO.
DEF VAR iReturn       AS INT  NO-UNDO.
DEF VAR cOrdreNrList  AS CHAR NO-UNDO.
DEF VAR cRowIdList    AS CHAR NO-UNDO.
DEF VAR cPkSdlId      AS CHAR NO-UNDO.
DEF VAR cMsg          AS CHAR NO-UNDO.

RUN JBoxBrowseMsgUpdSelVal.w ("Overfør valgte ordre til varemottaksprogram / pakkliste",
                              hBrowseOrdre:NUM-SELECTED-ROWS,
                              INT(DYNAMIC-FUNCTION("getAttribute",hBrowseOrdre,"recordcount")),
                              "",
                              OUTPUT cDummy,
                              OUTPUT iReturn).

IF iReturn = 0 THEN RETURN.


IF iReturn = 2 THEN DO: /* Behandle valgte */
  DO ix = 1 TO hBrowseOrdre:NUM-SELECTED-ROWS:
    IF hBrowseOrdre:FETCH-SELECTED-ROW(ix) THEN
      ASSIGN cOrdreNrList = cOrdreNrList + STRING(hBrowseOrdre:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("OrdreNr"):BUFFER-VALUE) + "|"
             cRowIdList   = cRowIdList + hBrowseOrdre:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("RowIdent1"):BUFFER-VALUE + ",".
  END.

  IF DYNAMIC-FUNCTION("runproc","ordre_overfor_pakkliste.p",TRIM(cOrdreNrList,"|"),?) THEN DO:
    ASSIGN cPkSdlId = ENTRY(2,DYNAMIC-FUNCTION("getTransactionMessage"),"|")
           cMsg     = ENTRY(1,DYNAMIC-FUNCTION("getTransactionMessage"),"|").
                    
    IF cMsg NE "" THEN
      DYNAMIC-FUNCTION("DoMessage",0,0,cMsg,"","").
    DYNAMIC-FUNCTION("RefreshRowids",hBrowseOrdre,TRIM(cRowIdList,",")).
    IF NOT VALID-HANDLE(hPakkliste) THEN DO:        
      RUN PkSdlHode.w PERSIST SET hPakkliste.
      RUN InitializeObject IN hPakkliste.
    END.

    RUN setQuery IN hPakkliste (cPkSdlId,hBrowseOrdre).
    RUN MoveToTop IN hPakkliste.
  END.
  ELSE DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PakklisteRecord C-Win 
PROCEDURE PakklisteRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

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
DEFINE INPUT PARAMETER icDir AS CHARACTER  NO-UNDO.

IF CAN-DO("Prev,Next",icDir) THEN DO:
  IF iTab = 3 THEN DO:    
    IF icDir = "Prev" THEN
      hBrowseLinje:SELECT-PREV-ROW().
    ELSE
      hBrowseLinje:SELECT-NEXT-ROW().
    APPLY "value-changed" TO hBrowseLinje.
  END.
  ELSE DO:
    IF icDir = "next" THEN DO:
      IF NOT hBrowseOrdreBest:SELECT-NEXT-ROW() THEN DO:
        IF NOT hBrowseOrdre:IS-ROW-SELECTED(hBrowseOrdre:FOCUSED-ROW) THEN
          hBrowseOrdre:SELECT-FOCUSED-ROW().
        IF hBrowseOrdre:SELECT-NEXT-ROW() THEN APPLY "value-changed" TO hBrowseOrdre.
        ELSE RETURN.
      END.
      ELSE APPLY "value-changed" TO hBrowseOrdreBest.
    END.
    ELSE DO:
      IF NOT hBrowseOrdreBest:SELECT-PREV-ROW() THEN DO:
        IF NOT hBrowseOrdre:IS-ROW-SELECTED(hBrowseOrdre:FOCUSED-ROW) THEN
          hBrowseOrdre:SELECT-FOCUSED-ROW().
        IF hBrowseOrdre:SELECT-PREV-ROW() THEN APPLY "value-changed" TO hBrowseOrdre.
        ELSE RETURN.
      END.
      ELSE APPLY "value-changed" TO hBrowseOrdreBest.
    END.
  END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrevNextBest C-Win 
PROCEDURE PrevNextBest :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT  PARAM icDir      AS CHAR  NO-UNDO.
DEF OUTPUT PARAM orBestHode AS RECID NO-UNDO.
DEF OUTPUT PARAM orArtBas   AS RECID NO-UNDO.

IF icDir = "next" THEN DO:
  IF NOT hBrowseOrdreBest:SELECT-NEXT-ROW() THEN DO:
    IF NOT hBrowseOrdre:IS-ROW-SELECTED(hBrowseOrdre:FOCUSED-ROW) THEN
      hBrowseOrdre:SELECT-FOCUSED-ROW().
    IF hBrowseOrdre:SELECT-NEXT-ROW() THEN APPLY "value-changed" TO hBrowseOrdre.
    ELSE RETURN.
  END.
  ELSE APPLY "value-changed" TO hBrowseOrdreBest.
END.
ELSE DO:
  IF NOT hBrowseOrdreBest:SELECT-PREV-ROW() THEN DO:
    IF NOT hBrowseOrdre:IS-ROW-SELECTED(hBrowseOrdre:FOCUSED-ROW) THEN
      hBrowseOrdre:SELECT-FOCUSED-ROW().
    IF hBrowseOrdre:SELECT-PREV-ROW() THEN APPLY "value-changed" TO hBrowseOrdre.
    ELSE RETURN.
  END.
  ELSE APPLY "value-changed" TO hBrowseOrdreBest.
END.

IF hBrowseOrdreBest:QUERY:GET-BUFFER-HANDLE(1):AVAIL  THEN
  ASSIGN orBestHode = DYNAMIC-FUNCTION("getRecId","BestHode",hBrowseOrdreBest:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("RowIdent1"):BUFFER-VALUE)
         orArtBas   = DYNAMIC-FUNCTION("getRecId","ArtBas",hBrowseOrdreBest:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("RowIdent2"):BUFFER-VALUE).
         
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
IF DYNAMIC-FUNCTION("getCurrentObject") = hOrdreToolbar OR DYNAMIC-FUNCTION("getCurrentObject") = hBrowseOrdre THEN
  DO ix = 1 TO hBrowseOrdre:NUM-SELECTED-ROWS:
    IF hBrowseOrdre:FETCH-SELECTED-ROW(ix) THEN
      RUN d-ordreutskriftX.w ("",STRING(hBrowseOrdre:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("OrdreNr"):BUFFER-VALUE)).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RapportButikk C-Win 
PROCEDURE RapportButikk :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cButikerRowIdList  AS CHAR NO-UNDO.
DEF VAR cButikerIdList     AS CHAR NO-UNDO.

IF NOT hFieldMap:AVAIL THEN RETURN.
bSetBestPanel = YES.
THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
RUN JBoxSelector.w (THIS-PROCEDURE,0,
                    "Butiker;Butik;Butnamn"
                  + ",VarebehBestLinje;",
                    "WHERE true" 
                  + ",FIRST VarebehBestLinje NO-LOCK WHERE BestiltButikkNr = Butiker.Butik AND VarebehNr = " 
                     + STRING(hFieldMap:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE)
                    ,INPUT-OUTPUT cButikerRowIdList,
                    "Butik",
                    INPUT-OUTPUT cButikerIdList,
                    "","",
                    OUTPUT bOK).
THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.
bSetBestPanel = NO.

IF bOk THEN DO:
  IF DYNAMIC-FUNCTION("runProc","butvarebeh_ordrebekr.p",
                      STRING(hFieldMap:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE) + "," +
                      cButikerIdList + 
                      ",," + cRappBestStatusList
                      ,httRapport) THEN DO:
    RapportToExcel(DYNAMIC-FUNCTION("getTransactionMessage"),cButikerIdList,"",cRappBestStatusList).
  END.
  ELSE DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RapportLev C-Win 
PROCEDURE RapportLev :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cLevBasRowIdList  AS CHAR NO-UNDO.
DEF VAR cLevBasIdList     AS CHAR NO-UNDO.

IF NOT hFieldMap:AVAIL THEN RETURN.
bSetBestPanel = YES.
THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
RUN JBoxSelector.w (THIS-PROCEDURE,0,
                    "LevBas;LevNr;LevNamn"
                  + ",VarebehLinje;"
                   ,"WHERE true" 
                  + ",FIRST VarebehLinje NO-LOCK WHERE varebehlinje.LevNr = LevBas.LevNr AND varebehlinje.VarebehNr = " 
                     + STRING(hFieldMap:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE)
                    ,INPUT-OUTPUT cLevBasRowIdList,
                    "LevNr",
                    INPUT-OUTPUT cLevBasIdList,
                    "","",
                    OUTPUT bOK).
THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.
bSetBestPanel = NO.

IF bOk THEN DO:
  IF DYNAMIC-FUNCTION("runProc","butvarebeh_ordrebekr.p",
                      STRING(hFieldMap:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE) + ",," +
                      cLevBasIdList + 
                      "," + cRappBestStatusList
                      ,httRapport) THEN DO:
    RapportToExcel(DYNAMIC-FUNCTION("getTransactionMessage"),"",cLevBasIdList,cRappBestStatusList).
  END.
  ELSE DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RapportRecord C-Win 
PROCEDURE RapportRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR ocReturn AS CHAR NO-UNDO.
RUN JBoxDSimpleSelectList.w ("Bestillinger for butikk|but_best|Bestillinger for leverandør|lev_best",?,OUTPUT ocReturn).
CASE ocReturn:
  WHEN "but_best" THEN RUN RapportButikk.
  WHEN "lev_best" THEN RUN RapportLev.
END CASE.
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

/* RUN VarebokLinjeDVelgFelter.w (OUTPUT cFieldList,OUTPUT cLabelList,OUTPUT iReturn).  */
/*                                                                                      */
/* IF iReturn NE 1 THEN RETURN.                                                         */

iReturn = DYNAMIC-FUNCTION("DoMessage",0,1,"Bekreft oppfrisking av artikkelinformasjon for alle artikler"
/*                                         + CHR(10) + "Felter som oppdateres: " + CHR(10) + CHR(10) + cLabelList  */
                                           ,"Bekreft","").

IF iReturn = 1 THEN DO:
  bOK = DYNAMIC-FUNCTION("RunProc","varebehlinje_refresh_all.p",STRING(hFieldMap:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE) + ";;" + cFieldList
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

/* RUN VarebokLinjeDVelgFelter.w (OUTPUT cFieldList,OUTPUT cLabelList,OUTPUT iReturn).  */
/*                                                                                      */
/* IF iReturn NE 1 THEN RETURN.                                                         */

iReturn = DYNAMIC-FUNCTION("DoMessage",0,1,"Bekreft oppfrisking av artikkelinformasjon for valgte artikler"
/*                                       + CHR(10) + "Felter som oppdateres: " + CHR(10) + CHR(10) + cLabelList  */
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

  bOk = DYNAMIC-FUNCTION("RunProc","varebehlinje_refresh_all.p",
                           STRING(hFieldMap:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE) + ";" + TRIM(cArtNrList,",") + ";" + cFieldList,
                           ?).
  IF NOT bOk THEN
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil i oppfriskning av artikkelinformasjon",""). 
  ELSE
      RUN OpenQuery.
END. /* LOOPEN */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReSendOrdreRecord C-Win 
PROCEDURE ReSendOrdreRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

RUN OrdreBehandling("resend").

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReturnOfWidget C-Win 
PROCEDURE ReturnOfWidget :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF FOCUS = LevDato:HANDLE IN FRAME frmLinje THEN DO:
  APPLY "entry" TO Grid.
  RETURN NO-APPLY.
END.
ELSE RUN SUPER.
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
  IF hBestiltAnt:BUFFER-VALUE > 0 AND hIkkeSendtBest:BUFFER-VALUE THEN
    hBestiltAntCol:BGCOLOR = iColNumBestilt.
  IF hFieldRGBcolor:BUFFER-VALUE > 0 THEN
    hBrwColArtBeskr:BGCOLOR = setFieldColor(hFieldRGBcolor:BUFFER-VALUE).
/*   ASSIGN hBrwColKjedeVare:FONT   = iFontWingdings               */
/*          hBrwColKjedeVare:FORMAT = CHR(254) + "/"  + CHR(168)   */
/*          hBrwColGjFakt:FONT      = iFontWingdings               */
/*          hBrwColGjFakt:FORMAT    = CHR(254) + "/"  + CHR(168).  */
END.
ELSE IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowseOrdre THEN DO:
/*   hHasteordreColumn:FONT = iFontWingdings.               */
/*   hHasteordreColumn:FORMAT = CHR(254) + "/"  + CHR(168). */
  IF hHasteordreField:BUFFER-VALUE THEN
    hHasteordreColumn:BGCOLOR = 12.
END.
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
DEF VAR hCurrObject AS HANDLE NO-UNDO.
DEF VAR iAntReg     AS INT    NO-UNDO.

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
  IF (VarebehBeskrivelse:screen-value) = "" THEN
  DO:
      MESSAGE "Varebeh mangler beskrivelse."
          VIEW-AS ALERT-BOX WARNING BUTTONS OK.
      APPLY "ENTRY" TO VarebehBeskrivelse.
      RETURN NO-APPLY.
  END.
        
  IF Oppdatert:CHECKED THEN
    DYNAMIC-FUNCTION("setAttribute",hFieldMap,"BufferExtraValues",
                      DYNAMIC-FUNCTION("getASuserid") + "|" + STRING(TODAY) + "|" + VarebehType:SCREEN-VALUE + "|" + ButikkListe:SCREEN-VALUE). 
  ELSE
    DYNAMIC-FUNCTION("setAttribute",hFieldMap,"BufferExtraValues","||" + VarebehType:SCREEN-VALUE + "|" + ButikkListe:SCREEN-VALUE).

  RUN SUPER.
END.
ELSE IF iTab = 3 AND DYNAMIC-FUNCTION("getCurrentObject") = hBestHodeToolbar THEN DO WITH FRAME frmLinje:
  IF NOT hFieldMapLinje:AVAIL THEN RETURN.

  DYNAMIC-FUNCTION("setAttribute",hFieldMapBestHode,"bufferextravalues",
                   STRING(hFieldMap:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE) + "|" +
                   STRING(hFieldMapLinje:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) + "|" +
                   DYNAMIC-FUNCTION("getFieldValues","StrType","WHERE StrTypeId = " + STRING(hFieldMapLinje:BUFFER-FIELD("StrTypeId"):BUFFER-VALUE),"Alfafordeling") + "|" +
                   cButCL).
  hCurrObject = hBestHodeToolbar.

  DYNAMIC-FUNCTION("setAttribute",hFieldMapBestHode,"checkModified","NO").
  IF DYNAMIC-FUNCTION("getToolbarState",hBestHodeToolbar) NE "new" THEN 
  DO:

    RUN SjekkReg (OUTPUT bOK,OUTPUT iAntReg).
    IF NOT bOk THEN RETURN.

    IF iAntReg > 0 THEN DO:
      IF bStrekkodeReg AND DYNAMIC-FUNCTION("DoMessage",0,4,"Vil du registrere strekkoder?","","") = 6 THEN DO:
        THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
        RUN Strekkode.w (THIS-PROCEDURE).
        THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.
      END.
    END.
    RUN LagreBestLinje.
    dSisteLevDato = DATE(LevDato:SCREEN-VALUE).
  END.
  ELSE RUN SUPER.

END.
ELSE RUN SUPER.

IF iTab = 3 AND hBestHodeToolbar = hCurrObject AND tbRepeat:INPUT-VALUE IN FRAME frmLinje AND bAlt-S THEN
  APPLY "alt-s" TO hWindow.
ELSE bAlt-S = FALSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendOrdreRecord C-Win 
PROCEDURE SendOrdreRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN OrdreBehandling("send").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetAltS-param C-Win 
PROCEDURE SetAltS-param :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihAltS AS HANDLE NO-UNDO.

DYNAMIC-FUNCTION("InitScreen" IN ihAltS,"LevNr," + sokLevNr:SCREEN-VALUE IN FRAME frmLinje + ",LevKod").
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setLookupAttributes C-Win 
PROCEDURE setLookupAttributes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihBrowse AS HANDLE NO-UNDO.
DEF INPUT PARAM ihDummy1 AS HANDLE NO-UNDO.
DEF INPUT PARAM ihDummy2 AS HANDLE NO-UNDO.

IF ihBrowse:QUERY:GET-BUFFER-HANDLE(1):NAME = "VarGr" THEN DO WITH FRAME frmLinje:
  IF sokAvdelingNr:SCREEN-VALUE IN FRAME frmLinje NE "0" THEN 
    DYNAMIC-FUNCTION("setAttribute",ihBrowse,"prescanqueryfilter","HuvGr WHERE HuvGr.AvdelingNr = " + sokAvdelingNr:SCREEN-VALUE + ",EACH VarGr OF HuvGr NO-LOCK").
  ELSE IF cAvdelingIdList NE "" THEN 
    DYNAMIC-FUNCTION("setAttribute",ihBrowse,"prescanqueryfilter","HuvGr WHERE CAN-DO('" + REPLACE(cAvdelingIdList,"|",",") + "',STRING(HuvGr.AvdelingNr)),EACH VarGr OF HuvGr NO-LOCK").
  DYNAMIC-FUNCTION("setAttribute",ihBrowse,"basequery","WHERE "
                 + (IF sokHg:SCREEN-VALUE NE "0" THEN 
                      "Hg = " + sokHg:SCREEN-VALUE
                    ELSE IF cHuvGrIdList NE "" THEN
                      "CAN-DO('" + REPLACE(cHuvGrIdList,"|",",") + "',STRING(VarGr.Hg))"
                    ELSE "true")
                 + " AND "
                 + (IF sokVg:SCREEN-VALUE NE "0" THEN 
                      "Vg = " + sokVg:SCREEN-VALUE
                    ELSE IF cVarGrIdList NE "" THEN
                      "CAN-DO('" + REPLACE(cVarGrIdList,"|",",") + "',STRING(VarGr.Vg))"
                    ELSE "true")
                   ).
  RUN InvokeMethod(ihBrowse,"OpenQuery").
  APPLY "entry" TO ihBrowse.
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

DEF VAR hSourceBuffer AS HANDLE NO-UNDO.
DEF VAR hTargetBuffer AS HANDLE NO-UNDO.
DEF VAR cOppmerking   AS CHAR   NO-UNDO.
DEF VAR iy            AS INT    NO-UNDO.
DEF VAR cList         AS CHAR   NO-UNDO.
DEF VAR cSelList      AS CHAR   NO-UNDO.
DEF VAR cSelectedRows AS CHAR   NO-UNDO.
DEF VAR cPreSelected  AS CHAR   NO-UNDO.

IF ihBrwSource:QUERY:GET-BUFFER-HANDLE(1):NAME = "_file" THEN DO:

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

IF bSetBestPanel THEN DO:
    
  DEF VAR hSelector     AS HANDLE NO-UNDO.
  DEF VAR hSelectorWin  AS HANDLE NO-UNDO.
  DEF VAR hPanelFrame   AS HANDLE NO-UNDO.

  hSelector = SOURCE-PROCEDURE.

  RUN LoadPanel IN hSelector ("BestStatusSelectPanel.w",OUTPUT hSelectorPanel).
  IF VALID-HANDLE(hSelector) THEN DO:


    /* Make room for and position the panel frame: */

    ASSIGN hPanelFrame                = DYNAMIC-FUNCTION("getFrameHandle" IN hSelectorPanel)
           hSelectorWin               = ihBrwSource:WINDOW
           hSelectorWin:HEIGHT-PIXELS = hSelectorWin:HEIGHT-PIXELS + hPanelFrame:HEIGHT-PIXELS
           .
    
    APPLY "window-resized" TO hSelectorWin.
    DYNAMIC-FUNCTION("adjustBrowseHeight" IN hSelector,hPanelFrame:HEIGHT-PIXELS * -1).
    hPanelFrame:Y = ihBrwSource:Y + ihBrwSource:HEIGHT-PIXELS. 
    
    DYNAMIC-FUNCTION("setSelList" IN hSelectorPanel,sokBestOrdreStatus:LIST-ITEM-PAIRS IN FRAME frmOrdre).

  END.
  ELSE DYNAMIC-FUNCTION("DoMessage",0,0,"Selector panel not installed","","").
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setSimpleSelectAttributes C-Win 
PROCEDURE setSimpleSelectAttributes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihSelectList AS HANDLE NO-UNDO.

ihSelectList:FRAME:TITLE = "Velg tilleggsknytning".
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SjekkReg C-Win 
PROCEDURE SjekkReg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF OUTPUT PARAM obReg AS LOG NO-UNDO.
DEF OUTPUT PARAM oiAnt AS INT NO-UNDO.

DEF VAR wRow AS INT NO-UNDO.
DEF VAR wCol AS INT NO-UNDO.
DEF VAR iAnt AS INT NO-UNDO.

DO wRow = chGrid:FixedRows TO chGrid:Rows - 1:
  DO wCol = chGrid:FixedCols TO chGrid:Cols - 1:
    IF INT(chGrid:TextMatrix(wRow,wCol)) NE 0 THEN
      ASSIGN oiAnt = oiAnt + INT(chGrid:TextMatrix(wRow,wCol))
             obReg = TRUE
             .
  END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkiftStrTypeRecord C-Win 
PROCEDURE SkiftStrTypeRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iocColValues AS CHAR NO-UNDO.

RUN gstrtype.w (INPUT-OUTPUT iocColValues,
                "", /* cFelt */
                "", /* cVerdier */
                "", /*cStart */
                0,  /* iAvdelingNr */
                0,   /* iHg */
                "").

IF iocColValues NE "" THEN DO:
  IF NOT DYNAMIC-FUNCTION("DoUpdate","ArtBas","=artbas_sjekk_skift_strtype.p",
                          "ArtikkelNr",
                          STRING(hBrowseLinje:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE),
                          "StrTypeId",
                          ENTRY(2,iocColValues,CHR(1)),
                          TRUE) THEN 
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil","").
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
      DYNAMIC-FUNCTION('doDelete','VarebehLinje','ignore','',hBrowseLinje:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('RowIdent1'):BUFFER-VALUE,FALSE).
END.
DYNAMIC-FUNCTION('doCommit',FALSE).

APPLY "VALUE-CHANGED":U TO hBrowseListe.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartQuery C-Win 
PROCEDURE StartQuery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"newrow") = "yes" AND
   DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"uselocaldata") = "yes" AND
   hFieldMapLinje:AVAIL
   THEN
  hFieldMapLinje:BUFFER-DELETE().

DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"uselocaldata","").
RUN OpenQuery.

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
IF DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"newrow") = "yes" AND
   DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"uselocaldata") = "yes" THEN 
  RETURN.

DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"uselocaldata","").
RUN SUPER.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StrekkodeRecord C-Win 
PROCEDURE StrekkodeRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF NOT VALID-HANDLE(hStrekkode) THEN
  RUN Strekkode.w PERSIST SET hStrekkode (THIS-PROCEDURE).

RUN MoveToTop IN hStrekkode.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UkePrintRecord C-Win 
PROCEDURE UkePrintRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VAR cFileName AS CHAR NO-UNDO.

DYNAMIC-FUNCTION("setAttribute",hBrowseOrdre,"RefreshProcessedRows","no").

IF hBrowseOrdre:NUM-SELECTED-ROWS > 1 THEN
  DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowseOrdre,"butvarebeh_ordresum_pr_uke.p","").
ELSE
  DYNAMIC-FUNCTION("ProcessQuery",hBrowseOrdre,"butvarebeh_ordresum_pr_uke.p","").

DYNAMIC-FUNCTION("setAttribute",hBrowseOrdre,"RefreshProcessedRows","").


cFileName = SESSION:TEMP-DIRECTORY + STRING(YEAR(TODAY)) + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") + STRING(RANDOM(1,10000)) + STRING(TIME) + ".xls".
OUTPUT TO VALUE(cFileName).
PUT UNFORMATTED DYNAMIC-FUNCTION("getTransactionMessage").
OUTPUT CLOSE.

DYNAMIC-FUNCTION("setWebDoc","",cFileName).


/* DYNAMIC-FUNCTION("DoMessage",0,20,DYNAMIC-FUNCTION("getTransactionMessage"),"",""). */


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
IF DYNAMIC-FUNCTION("getCurrentObject") = hBestHodeToolbar THEN 
  RUN DeleteRecord.
ELSE RUN SUPER.

/*   IF iTab < 3 THEN DO WITH FRAME frmDetalj:       */
/*       ASSIGN                                      */
/*           VarebehNr:SENSITIVE      = FALSE        */
/* /*           ProfilNr:SENSITIVE       = FALSE  */ */
/* /*           btnSokProfilNr:SENSITIVE = FALSE  */ */
/*           .                                       */
/*   END.                                            */

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
RUN SUPER.
IF DYNAMIC-FUNCTION("getCurrentObject") = hHasteordreOverlay THEN DO: 
  IF hHasteordreOverlay:CHECKED THEN DO:
    IF DYNAMIC-FUNCTION("runproc","ordre_sett_hasteordre.p",hBuffOrdre:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE,?) THEN DO:
      DYNAMIC-FUNCTION("RefreshRowids",hBrowseOrdre,hBuffOrdre:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE).
      hHasteordreOverlay:BGCOLOR = 12.
    END.
    ELSE DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
  END.
  ELSE 
    hHasteordreOverlay:BGCOLOR = ?.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VelgButikker C-Win 
PROCEDURE VelgButikker :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT  PARAM icMode AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOk   AS LOG NO-UNDO.

DEF VAR cMineButikker AS CHAR NO-UNDO.
DEF VAR cRowIdList    AS CHAR NO-UNDO.
DEF VAR cIdList       AS CHAR NO-UNDO.

DO WITH FRAME frmDetalj:

  IF INT(ProfilNr:SCREEN-VALUE) = 0 THEN DO:
    DYNAMIC-FUNCTION("DoMessage",0,1,"Det er ikke angitt noen prisprofil.","","").
    RETURN.
  END.

  /* Bruker med brukertype 1 - system, får returnert blank liste her.                       */
  /*                       2 - Butikk, får returnert liste i henhold til distribusjonsteam. */
  /*                       3 - Leverandør, får returnert blank.                             */ 
  IF DYNAMIC-FUNCTION("runproc","mine_butikker.p",DYNAMIC-FUNCTION("getASuserId") + ",2",?) THEN
    cMineButikker = DYNAMIC-FUNCTION("getTransactionMessage").
  ELSE cMineButikker = cTigjButikker.

  /* Er det ikke en ny suppleringsbok, og det ligger en butikkliste på suppleringsboken, brukes denne. */
  IF icMode NE "new" 
     AND hFieldMap:AVAIL 
     AND hFieldMap:BUFFER-FIELD("ButikkListe"):BUFFER-VALUE NE "" THEN
    ASSIGN cRowIdList = DYNAMIC-FUNCTION("getRowIdList","Butiker","","WHERE CAN-DO('" + hFieldMap:BUFFER-FIELD("ButikkListe"):BUFFER-VALUE + "',STRING(butik))")
           cIdList    = REPLACE(hFieldMap:BUFFER-FIELD("ButikkListe"):BUFFER-VALUE,",","|").
/*  
MESSAGE 'TEST-TN' SKIP
        'cMineButikker' cMineButikker SKIP
        'cTigjButikker' cTigjButikker SKIP
        'RunProc' DYNAMIC-FUNCTION("runproc","mine_butikker.p",DYNAMIC-FUNCTION("getASuserId") + ",2",?) SKIP
        'cRowIdList' cRowIdList SKIP
        'cIdList' cIdList SKIP
VIEW-AS ALERT-BOX.  
*/  
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  /*
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "Butiker;Butik;Butnamn;ProfilNr|Profilnr|>>>>>>9",
                      "WHERE CAN-DO('" + cMineButikker + "',STRING(Butik))"
                    + "  AND ProfilNr = " + ProfilNr:SCREEN-VALUE
                      ,INPUT-OUTPUT cRowIdList,
                      "Butik",
                      INPUT-OUTPUT cIdList,
                      "","",
                      OUTPUT bOK).
  */
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "Butiker;Butik;Butnamn;ProfilNr|Profilnr|>>>>>>9",
                      "WHERE CAN-DO('" + cMineButikker + "',STRING(Butik))"
                      ,INPUT-OUTPUT cRowIdList,
                      "Butik",
                      INPUT-OUTPUT cIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk THEN DO:
    DO ix = 1 TO NUM-ENTRIES(ButikkListe:SCREEN-VALUE):
      IF NOT CAN-DO(REPLACE(cIdList,"|",","),ENTRY(ix,ButikkListe:SCREEN-VALUE)) THEN 
        bOK = FALSE.
    END.
    IF NOT bOK AND DYNAMIC-FUNCTION("getToolbarState",hUpdToolbar) NE "new" THEN DO:
      IF DYNAMIC-FUNCTION("DoMessage",0,1,"Hvis du fjerner en butikk så slettes alle bestillinger for denne (disse)" + CHR(10) +
                                          "Er du sikker på at du vil gjøre dette?","Advarsel","") = 1 THEN DO:
        IF NOT DYNAMIC-FUNCTION("runproc","varebehbest_sjekk_mot_butikkliste.p",
                                 ButikkListe:SCREEN-VALUE + ";" + cIdList + ";" + STRING(hFieldMap:BUFFER-FIELD("ButikkListe"):BUFFER-VALUE),?) THEN
          DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil","").
      END.
      ELSE RETURN NO-APPLY.
    END.
    ButikkListe:SCREEN-VALUE = REPLACE(cIdList,"|",",").
    APPLY "any-printable" TO VarebehBeskrivelse.
    obOK = TRUE.
  END.
  ELSE obOK = FALSE.   
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisBestRecord C-Win 
PROCEDURE VisBestRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR ioRecId AS RECID NO-UNDO.

IF DYNAMIC-FUNCTION("getCurrentObject") = hBestHodeToolbar THEN
  ioRecId = DYNAMIC-FUNCTION("getRecId","BestHode",hFieldMapBestHode:BUFFER-FIELD("RowIdent2"):BUFFER-VALUE).
ELSE IF hBrowseOrdreBest:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN
  ioRecId = DYNAMIC-FUNCTION("getRecId","BestHode",hBrowseOrdreBest:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("RowIdent1"):BUFFER-VALUE).
ELSE RETURN.

THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.

RUN w-gridord.w ((IF DYNAMIC-FUNCTION("getCurrentObject") = hBestHodeToolbar THEN
                    DYNAMIC-FUNCTION("getRecId","ArtBas",hFieldMapLinje:BUFFER-FIELD("RowIdent2"):BUFFER-VALUE)
                  ELSE
                    DYNAMIC-FUNCTION("getRecId","ArtBas",hBrowseOrdreBest:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("RowIdent2"):BUFFER-VALUE)),
                 INPUT-OUTPUT ioRecId,
                 "").
THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

IF DYNAMIC-FUNCTION("getCurrentObject") = hBestHodeToolbar AND hFieldMapBestHode:AVAIL THEN DO:
  DYNAMIC-FUNCTION("refreshRowids",hBrowseBestHode,hFieldMapBestHode:BUFFER-FIELD("Rowident1"):BUFFER-VALUE).
  APPLY "value-changed" TO hBrowseBestHode.
END.
ELSE DO:
  DYNAMIC-FUNCTION("refreshRowids",hBrowseOrdreBest,hBrowseOrdreBest:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("RowIdent1"):BUFFER-VALUE).
  APPLY "value-changed" TO hBrowseOrdreBest.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisInnlevRecord C-Win 
PROCEDURE VisInnlevRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR ioRecId AS RECID NO-UNDO.

IF DYNAMIC-FUNCTION("getCurrentObject") = hBestHodeToolbar THEN
  ioRecId = DYNAMIC-FUNCTION("getRecId","BestHode",hFieldMapBestHode:BUFFER-FIELD("RowIdent2"):BUFFER-VALUE).
ELSE IF hBrowseOrdreBest:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN
  ioRecId = DYNAMIC-FUNCTION("getRecId","BestHode",hBrowseOrdreBest:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("RowIdent1"):BUFFER-VALUE).
ELSE RETURN.

THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.

RUN w-gridinnlev.w 
                 ((IF DYNAMIC-FUNCTION("getCurrentObject") = hBestHodeToolbar THEN
                    DYNAMIC-FUNCTION("getRecId","ArtBas",hFieldMapLinje:BUFFER-FIELD("RowIdent2"):BUFFER-VALUE)
                  ELSE
                    DYNAMIC-FUNCTION("getRecId","ArtBas",hBrowseOrdreBest:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("RowIdent2"):BUFFER-VALUE)),
                 INPUT-OUTPUT ioRecId,
                 "").

IF DYNAMIC-FUNCTION("getCurrentObject") = hBestHodeToolbar AND hFieldMapBestHode:AVAIL THEN
  DYNAMIC-FUNCTION("refreshRowids",hBrowseBestHode,hFieldMapBestHode:BUFFER-FIELD("Rowident1"):BUFFER-VALUE).
ELSE DO:
  DYNAMIC-FUNCTION("refreshRowids",hBrowseOrdreBest,hBrowseOrdreBest:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("RowIdent1"):BUFFER-VALUE).
  APPLY "value-changed" TO hBrowseOrdreBest.
END.

THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AddStr C-Win 
FUNCTION AddStr RETURNS LOGICAL
  ( INPUT ifArtikkelNr  AS DEC,
    INPUT icStorl       AS CHAR,
    INPUT iiAnt         AS INT,
    INPUT icAddReplace  AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cModellFarge    AS CHAR NO-UNDO.
DEF VAR cModellArtList  AS CHAR NO-UNDO.

IF DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"currviewfields") = cCreateViewFields THEN
  RETURN setOverlayValues(STRING(ifArtikkelNr),"").
ELSE DO:
  cModellFarge = DYNAMIC-FUNCTION("getFieldValues","ArtBas","WHERE ArtikkelNr = " + STRING(ifArtikkelNr),"ModellFarge").
  IF cModellFarge NE "0" AND cModellFarge NE ? THEN DO:
    cModellArtList = DYNAMIC-FUNCTION("getFieldList","ArtBas;ArtikkelNr",
                                      "WHERE ArtBas.ModellFarge = " + cModellFarge
                                    + "  AND ArtBas.ArtikkelNr NE " + STRING(ifArtikkelNr)).
    IF NUM-ENTRIES(cModellArtList,"|") > 0 THEN DO:
      IF DYNAMIC-FUNCTION("DoMessage",0,4,"Det finnes andre artikler med samme modell. Vil du også hente disse?","","") = 6 THEN
        DO iModIdx = 1 TO NUM-ENTRIES(cModellArtList,"|"):
          RUN NyArtBas (DEC(ENTRY(iModIdx,cModellArtList,"|"))).
        END.
    END.
  END.
  RUN NyArtBas (ifArtikkelNr).
  bAlt-S = TRUE.
END.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  hBrwColArtBeskr = ihBrowse:GET-BROWSE-COLUMN(4).
  ihBrowse:GET-BROWSE-COLUMN(1):WIDTH-PIXELS  = 60.
  ihBrowse:GET-BROWSE-COLUMN(2):WIDTH-PIXELS  = 100.
  ihBrowse:GET-BROWSE-COLUMN(3):WIDTH-PIXELS  = 60.
  ihBrowse:GET-BROWSE-COLUMN(4):WIDTH-PIXELS  = 160.
  ihBrowse:GET-BROWSE-COLUMN(5):WIDTH-PIXELS  = 50.
  ihBrowse:GET-BROWSE-COLUMN(6):WIDTH-PIXELS  = 30.
  ihBrowse:GET-BROWSE-COLUMN(7):WIDTH-PIXELS  = 50.
  ihBrowse:GET-BROWSE-COLUMN(8):WIDTH-PIXELS  = 50.

  ihBrowse:GET-BROWSE-COLUMN(9):WIDTH-PIXELS  = 70.
  ihBrowse:GET-BROWSE-COLUMN(10):WIDTH-PIXELS = 70.
  ihBrowse:GET-BROWSE-COLUMN(11):WIDTH-PIXELS = 70.
  ihBrowse:GET-BROWSE-COLUMN(26):WIDTH-PIXELS = 50.
  ihBrowse:GET-BROWSE-COLUMN(27):WIDTH-PIXELS = 50.
  ihBrowse:GET-BROWSE-COLUMN(28):WIDTH-PIXELS = 70.

  ihBrowse:GET-BROWSE-COLUMN(30):WIDTH-PIXELS = 40.
  ihBrowse:GET-BROWSE-COLUMN(31):WIDTH-PIXELS = 40.
  ihBrowse:GET-BROWSE-COLUMN(32):WIDTH-PIXELS = 40.

  ASSIGN hBrwColKjedeVare = ihBrowse:GET-BROWSE-COLUMN(24)
         hBrwColGjFakt    = ihBrowse:GET-BROWSE-COLUMN(25).
END.
/* ELSE IF icBrowseName = "rectBrowseListe" THEN DO:  */
/*   ihBrowse:MOVE-COLUMN(13,4).                      */
/*   ihBrowse:MOVE-COLUMN(12,6).                      */
/*   ihBrowse:MOVE-COLUMN(13,8).                      */
/* END.                                               */
ELSE IF icBrowseName = "BrwOrdreBest" THEN DO:
  ihBrowse:GET-BROWSE-COLUMN(3):WIDTH-PIXELS = 60.
  ihBrowse:GET-BROWSE-COLUMN(4):WIDTH-PIXELS = 60.
  ihBrowse:GET-BROWSE-COLUMN(6):WIDTH-PIXELS = 100.
END.
ELSE IF icBrowseName = "rectBrowseOrdre" THEN DO:
/*   ihBrowse:MOVE-COLUMN(15,16).  */
  ihBrowse:GET-BROWSE-COLUMN(2):WIDTH-PIXELS = 40.
  ihBrowse:GET-BROWSE-COLUMN(17):WIDTH-PIXELS = 80.
END.
  
RETURN YES.

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
         sokLevNr:SCREEN-VALUE          = "0"
         sokLevNamn:SCREEN-VALUE        = ""
         sokBeskr:SCREEN-VALUE          = ""
         sokAktNr:SCREEN-VALUE          = "0"
         sokAktBeskrivelse:SCREEN-VALUE = ""
         sokFraEdato:SCREEN-VALUE       = ?
         sokTilEdato:SCREEN-VALUE       = ?
         sokFraVPIdato:SCREEN-VALUE     = ?
         sokTilVPIdato:SCREEN-VALUE     = ?
         sokFraRegDatoBestInnl:SCREEN-VALUE = ?
         sokTilRegDatoBestInnl:SCREEN-VALUE = ?
         sokLinjeMerknad:SCREEN-VALUE   = ""
         sokArtikkelNr:SCREEN-VALUE     = "0"
         sokLevKod:SCREEN-VALUE         = ""
         sokRegBestStat:SCREEN-VALUE    = " "
         sokSaSong:SCREEN-VALUE         = "0"
         sokSasBeskr:SCREEN-VALUE       = ""
         cSaSongIdList                  = ""
         .

  DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"querywhere","").
END.
  
RETURN YES.

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
DEF VAR cRowIdList  AS CHAR NO-UNDO.

IF iReturn = 0 THEN RETURN FALSE.

IF iReturn = 1 THEN 
  bOk = DYNAMIC-FUNCTION("ProcessQuery",hBrowseLinje,"varebehlinje_justermange.p",icField + "|" + icValue).
ELSE
  bOk = DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowseLinje,"varebehlinje_justermange.p",icField + "|" + icValue).

IF NOT bOK THEN
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil i endring av " + icField,""). 
ELSE IF cRowIdList NE "" THEN DO:
  DYNAMIC-FUNCTION("refreshRowids",hBrowseLinje,REPLACE(cRowIdList,"|",",")).
  APPLY "value-changed" TO hBrowseLinje.
END.
ELSE RUN OpenQuery.

RETURN bOk. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FreezeWin C-Win 
FUNCTION FreezeWin RETURNS LOGICAL
  ( INPUT ibFreeze AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = NOT ibFreeze.
  
RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getArtKalkTT C-Win 
FUNCTION getArtKalkTT RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  For å kunne oppdatere kalkylen på et felt under ny-registrering så sendes raden til en server-rutine
    Notes:  
------------------------------------------------------------------------------*/
hbArtKalk:EMPTY-TEMP-TABLE().

hbArtKalk:BUFFER-CREATE().
hbArtKalk:BUFFER-COPY(hFieldMapLinje).

RETURN httArtKalk.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getLevDato C-Win 
FUNCTION getLevDato RETURNS DATE
  ( INPUT iLevNr AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR dLevDato  AS DATE NO-UNDO.
DEF VAR cDagTid   AS CHAR NO-UNDO.
DEF VAR iNesteDag AS INT  NO-UNDO.
DEF VAR iTid      AS INT  NO-UNDO.
DEF VAR iIdag     AS INT  NO-UNDO.


IF hFieldMapLinje:AVAIL THEN DO:

  IF cButikkListe NE "*" THEN DO:
    cDagTid = DYNAMIC-FUNCTION("getFieldValues","defaultLevDato",
                                     "WHERE ButikkNr = " + cButCl  + " AND LevNr = " + STRING(iLevNr),
                                     "Ukedag,Tid").
    IF cDagTid NE ? THEN DO:
      ASSIGN iNesteDag = INT(ENTRY(1,cDagTid,"|"))
             iTid      = INT(ENTRY(2,cDagTid,"|")).
      IF iTid = 0 THEN iTid = 12 * 3600.

      iIdag = WEEKDAY(TODAY) - 1.

      IF iIDag < iNesteDag THEN
        dLevDato = TODAY + iNesteDag - iIDag.
      IF iIdag = iNesteDag AND TIME < iTid THEN
        dLevDato = TODAY.
      ELSE IF iIdag GE iNesteDag THEN
        dLevDato = TODAY + 7 - iIDag + iNesteDag.
    END.
  END.
END.
IF dLevDato = ? THEN dLevDato = TODAY + 7.

RETURN dLevDato.   /* Function return value. */

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
ELSE 
  RETURN ListeSokLevNr:SCREEN-VALUE IN FRAME frmFilter. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getReportFile C-Win 
FUNCTION getReportFile RETURNS CHARACTER
  ( INPUT icClientRepFile AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cFileName    AS CHAR   NO-UNDO.
DEF VAR httRepTable  AS HANDLE NO-UNDO.
DEF VAR httRepBuffer AS HANDLE NO-UNDO. 
DEF VAR hQuery       AS HANDLE NO-UNDO.

RETURN icClientRepFile. /* <- Kommenter ut ved overgang til appserver */

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

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getStorl C-Win 
FUNCTION getStorl RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR iRow      AS INT  NO-UNDO.
DEF VAR iCol      AS INT  NO-UNDO.
DEF VAR cTmpStorl AS CHAR NO-UNDO.

DO iRow = 1 TO chGrid:Rows - 1:
  IF NOT Registrerat(iRow) THEN
      NEXT.
  DO iCol = 2 TO chGrid:Cols - 1:
    IF INT(chGrid:TextMatrix(iRow,iCol)) = 0 THEN NEXT.

    cTmpStorl = IF NOT CAN-DO(cTmpStorl,TRIM(chGrid:TextMatrix(0,iCol))) THEN
                  cTmpStorl + TRIM(chGrid:TextMatrix(0,iCol)) + ","
                ELSE cTmpStorl.
  END.
END.

IF cTmpStorl = "" AND hBrowseBestHode:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN
  cTmpStorl = REPLACE(REPLACE(hBrowseBestHode:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("AlfaFordeling"):BUFFER-VALUE," ",""),"  ","").
          
RETURN TRIM(cTmpStorl,",").

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION HentVerdier C-Win 
FUNCTION HentVerdier RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cFieldName AS CHAR NO-UNDO.

EMPTY TEMP-TABLE ttVerdier.
EMPTY TEMP-TABLE ttInit.

DO WITH FRAME frmLinje:
  CREATE ttVerdier.
  ASSIGN ttVerdier.cNavn  = "Levnr"
         ttVerdier.cVerdi = sokLevNr:SCREEN-VALUE
         .
  DO ix = 1 TO hFieldMapLinje:NUM-FIELDS:
    cFieldName = hFieldMapLinje:BUFFER-FIELD(ix):NAME.
    CREATE ttInit.
    ASSIGN ttInit.cNavn   = cFieldName 
           ttInit.cVerdi  = STRING(hFieldMapLinje:BUFFER-FIELD(ix):BUFFER-VALUE)
           ttInit.hFillIn = DYNAMIC-FUNCTION("getLinkedObjectByInfo",hBrowseLinje,"browseoverlay",ttInit.cNavn)
           .

    IF CAN-DO(cCreateViewFields,cFieldName) AND NOT CAN-DO(cCreateDisplayFields,cFieldName) 
       AND cFieldName NE "Varekost" THEN DO:
      IF cFieldName = "Rab%" THEN cFieldName = "Rab1%".
      CREATE ttVerdier.
      ASSIGN ttVerdier.cNavn  = cFieldName 
             ttVerdier.cVerdi = STRING(hFieldMapLinje:BUFFER-FIELD(ix):BUFFER-VALUE)
             .
    END.
  END.
END.

RETURN httVerdier.

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
hBrwOLLevKod  = DYNAMIC-FUNCTION("NewBrowseFillIn",hBrowseLinje,"LevKod","LevKod"
               ,"ArtBas;ArtikkelNr;Beskr","where false","artikkelnr"
               ,"").
hBrwOLLevKod:HELP = "Lev.varenr. Rød bakgrunn betyr at artikkel finnes (i art.reg)".

hBrwOLBeskr   = DYNAMIC-FUNCTION("NewBrowseFillIn",hBrowseLinje,"Beskr","Beskr"
               ,"","","","").
hBrwOLBeskr:HELP = "Varebeskrivelse. 20 først posisjoner legges i bong-tekst".

hBrwOLLevFargKod  = DYNAMIC-FUNCTION("NewBrowseFillIn",hBrowseLinje,"LevFargKod","LevFargKod"
               ,"","","","").

hBrwOLFarg =  DYNAMIC-FUNCTION("NewBrowseFillIn",hBrowseLinje,"Farg","Farg"
              ,"Farg;Farg|Farge|>>>>9;FarBeskr","WHERE true","Farg"
              ,"").
hBrwOLMatKod =  DYNAMIC-FUNCTION("NewBrowseFillIn",hBrowseLinje,"MatKod","MatKod"
              ,"Material;MatKod|Mat.|>9;MatBeskr","WHERE true","MatKod"
              ,"").
hBrwOLStrTypeId  = DYNAMIC-FUNCTION("NewBrowseFillIn",hBrowseLinje,"StrTypeId","StrTypeId"
              ,"StrType;StrTypeId;Beskrivelse;Intervall","WHERE true","StrTypeId"
              ,"").
hBrwOLVg  =   DYNAMIC-FUNCTION("NewBrowseFillIn",hBrowseLinje,"Vg","Vg"
              ,"VarGr;Vg;VgBeskr;Hg,HuvGr;HgBeskr","WHERE false,FIRST HuvGr OF VarGr NO-LOCK","Vg"
              ,"").
hBrwOLInnkjopsPris  = DYNAMIC-FUNCTION("NewBrowseFillIn",hBrowseLinje,"InnkjopsPris","InnkjopsPris"
               ,"","","","").
hBrwOLInnkjopsPris:HELP = "Brutto innkjøpspris".

hBrwOLForhRab% = DYNAMIC-FUNCTION("NewBrowseFillIn",hBrowseLinje,"ForhRab%","ForhRab%"
               ,"","","","").
hBrwOLForhRab%:HELP = 'Forh.rabatt. Gjelder hvis knyttet til "Forhåndsordre" messe'.

hBrwOLSupRab%  = DYNAMIC-FUNCTION("NewBrowseFillIn",hBrowseLinje,"SupRab%","SupRab%"
               ,"","","","").
hBrwOLSupRab%:HELP = 'Supplieringsrabatt'.

hBrwOLVarekost = DYNAMIC-FUNCTION("NewBrowseFillIn",hBrowseLinje,"Varekost","Varekost"
               ,"","","","").
hBrwOLVarekost:HELP = "Netto innkjøpspris".

hBrwOLPris  = DYNAMIC-FUNCTION("NewBrowseFillIn",hBrowseLinje,"Pris","Pris"
               ,"","","","").
hBrwOLAnbefaltPris  = DYNAMIC-FUNCTION("NewBrowseFillIn",hBrowseLinje,"AnbefaltPris","AnbefaltPris"
               ,"","","","").

hBrwOLMerkFi  = DYNAMIC-FUNCTION("NewBrowseFillIn",hBrowseLinje,"LinjeMerknad","LinjeMerknad"
               ,"","","","").

RETURN YES.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION RapportToExcel C-Win 
FUNCTION RapportToExcel RETURNS LOGICAL
  ( INPUT icFileNameCount  AS CHAR,
    INPUT icButList        AS CHAR,
    INPUT icLevList        AS CHAR,
    INPUT icBestStatusList AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
    
   ( INPUT icCount AS CHAR,
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
DEF VAR iCntStr                 AS INT  NO-UNDO.
DEF VAR cCurrLevNavn            AS CHAR NO-UNDO.              
DEF VAR cPrevLevNavn            AS CHAR NO-UNDO.   
DEF VAR cValue                  AS CHAR NO-UNDO.
DEF VAR iEndringerFra           AS INT  NO-UNDO.
DEF VAR cButNavn                AS CHAR NO-UNDO.
DEF VAR cLevNavn                AS CHAR NO-UNDO.
DEF VAR cVbokMesse              AS CHAR NO-UNDO.
DEF VAR cStatusTextList         AS CHAR NO-UNDO.
DEF VAR bAlleStatus             AS LOG  NO-UNDO INIT YES.

ASSIGN cFileName = ENTRY(1,icFileNameCount,"|")
       iCount    = INT(ENTRY(2,icFileNameCount,"|"))
       .

IF NUM-ENTRIES(icButList,"|") = 1 AND icButList NE "" THEN 
  cButNavn   = DYNAMIC-FUNCTION("getFieldList","Butiker;ButNamn","WHERE Butik = " + icButList).
IF NUM-ENTRIES(icLevList,"|") = 1 AND icLevList NE "" THEN 
  cButNavn   = DYNAMIC-FUNCTION("getFieldList","LevBas;LevNamn","WHERE Levnr = " + icLevList).

cVbokMesse = ". Varebok: " + DYNAMIC-FUNCTION("getFieldList",
                              "VarebokHode;VareBokBeskrivelse",
                              "WHERE VareBokNr = " + STRING(hFieldMap:BUFFER-FIELD("Kilde"):BUFFER-VALUE)).
cVbokMesse = cVbokMesse 
           + (IF hFieldMap:BUFFER-FIELD("MesseBeskrivelse"):BUFFER-VALUE NE "" THEN ". Messe: " ELSE "")
           + hFieldMap:BUFFER-FIELD("MesseBeskrivelse"):BUFFER-VALUE.

chExcelApplication = DYNAMIC-FUNCTION("getExcelHandle").
IF NOT VALID-HANDLE(chExcelApplication) THEN DO:
  IF DYNAMIC-FUNCTION("setWebDoc","Open",cFileName) NE "" THEN
    MESSAGE "Could not open file: " cFileName VIEW-AS ALERT-BOX ERROR.
  RETURN TRUE.
END.

DO ix = 4 TO NUM-ENTRIES(sokBestOrdreStatus:LIST-ITEM-PAIRS IN FRAME frmOrdre,"|") BY 2:
  IF LOOKUP(ENTRY(ix,sokBestOrdreStatus:LIST-ITEM-PAIRS,"|"),icBestStatusList,"|") > 0 THEN
    cStatusTextList = cStatusTextList + (IF cStatusTextList NE "" THEN "," ELSE "") + ENTRY(ix - 1,sokBestOrdreStatus:LIST-ITEM-PAIRS,"|").
  ELSE bAlleStatus = NO.
END.
IF bAlleStatus THEN cStatusTextList = "*".
  
chExcelApplication:Workbooks:OpenText(cFileName,2,,,,,TRUE).

chExcelApplication:ScreenUpdating = FALSE.

cRange = "A2:Y" + STRING(iCount + 600). 

ASSIGN chWorkbook                           = chExcelApplication:WorkBooks:ITEM(1)
       chWorkSheet                          = chExcelApplication:Sheets:ITEM(1)
       chWorkSheet:NAME                     = ENTRY(1,hFieldMap:BUFFER-FIELD("VareBehBeskrivelse"):BUFFER-VALUE," ")

       chWorkSheet:Range(cRange):FONT:NAME  = "Arial Narrow"
       chWorkSheet:Range(cRange):FONT:SIZE  = 13

       chWorkSheet:Rows(1):FONT:Bold        = TRUE

       chWorkSheet:PageSetup:LeftHeader     = (IF icButList NE "" THEN "Butikk: " + REPLACE(icButList,"|",",") + " " + cButNavn ELSE "")
       chWorkSheet:PageSetup:CenterHeader   = "BESTILLINGER " + (IF cVbokMesse NE "" THEN cVbokMesse ELSE hFieldMap:BUFFER-FIELD("VareBehBeskrivelse"):BUFFER-VALUE)
       chWorkSheet:PageSetup:RightHeader    = (IF icLevList NE "" THEN "Leverandør: " + REPLACE(icLevList,"|",",") + " " + cLevNavn ELSE "")
       chWorkSheet:PageSetup:LeftFooter     = "Bestillingsstatus: " + cStatusTextList
       chWorkSheet:PageSetup:CenterFooter   = STRING(TODAY,"99/99/9999")  + " " + STRING(TIME,"HH:MM") /*"&D &T"*/
       chWorkSheet:PageSetup:RightFooter    = "Page &P of &N"

       chWorkSheet:PageSetup:Orientation    = 1
       chWorkSheet:PageSetup:FitToPagesWide = 1
       chWorkSheet:PageSetup:TopMargin      = 50
       chWorkSheet:PageSetup:LeftMargin     = 10
       chWorkSheet:PageSetup:RightMargin    = 10
       chWorkSheet:PageSetup:Zoom           = 52
       chWorkSheet:PageSetup:PrintTitleRows = "$1:$1"
       .

chWorkSheet:Range("A1:O1"):SELECT().
chInterior = chExcelApplication:SELECTION:Interior.
chInterior:ColorIndex = 6.

chWorkSheet:Rows("1"):RowHeight = 38.

chWorkSheet:Range("A2:A" + STRING(iCount + 600)):FONT:Bold = TRUE.

/* IF icType = "lev" THEN DO: */
chWorkSheet:Range("I2:I" + STRING(iCount + 600)):FONT:Bold = TRUE.

chWorkSheet:Range("C1:C1"):WrapText = TRUE.
chWorkSheet:Range("I1:I1"):WrapText = TRUE.
chWorkSheet:Range("K1:K1"):WrapText = TRUE.

chWorkSheet:Range("C:C"):NumberFormat = "@".
chWorkSheet:Range("D:D"):NumberFormat = "##0".
chWorkSheet:Range("N:N"):NumberFormat = "##0".
chWorkSheet:Range("I:I"):NumberFormat = "# ##0,00".
chWorkSheet:Range("K:K"):NumberFormat = "# ##0,00".
chWorkSheet:Range("L:L"):NumberFormat = "# ##0,00".
chWorkSheet:Range("M:M"):NumberFormat = "##0".
chWorkSheet:Range("N:N"):NumberFormat = "##0".
chWorkSheet:Range("O:O"):NumberFormat = "# ##0,00".

chWorkSheet:Range("N2:N" + STRING(iCount + 600)):FONT:Bold = TRUE.
chWorkSheet:Range("O2:O" + STRING(iCount + 600)):FONT:Bold = TRUE.
  
chWorkSheet:Rows("2:" + STRING(iCount + 600)):RowHeight = 20.

DO ix = 2 TO iCount:
  chBorder = chWorkSheet:Range("A" + STRING(ix) + ":O" + STRING(ix)):Borders({&xlEdgeBottom}).
  ASSIGN chBorder:LineStyle = {&xlContinuous}
         chBorder:Weight    = {&xlThin}.

  IF chWorkSheet:Range("A" + STRING(ix) + ":A" + STRING(ix)):VALUE = "SUM Levnr" THEN DO:
    chBefore = chWorkSheet:Range("A" + STRING(ix + 1) + ":A" + STRING(ix + 1)).
    chWorkSheet:HPageBreaks:Add(chBefore).
  END.
END.
DO ix = iCount + 2 TO iCount + 600:
  ASSIGN cRange = "I" + STRING(ix)
         cValue = chWorkSheet:Range(cRange):VALUE
         .
  IF cValue NE ? THEN DO:
    chBorder = chWorkSheet:Range("A" + STRING(ix) + ":I" + STRING(ix)):Borders({&xlEdgeBottom}).
    ASSIGN chBorder:LineStyle = {&xlContinuous}
           chBorder:Weight    = {&xlThin}.
  END.

  ASSIGN cRange = "A" + STRING(ix)
         cValue = chWorkSheet:Range(cRange):VALUE.
END.

chBefore = chWorkSheet:Range("A" + STRING(iCount + 2)).
chWorkSheet:HPageBreaks:Add(chBefore).

chWorkSheet:Columns("A:O"):AutoFit().
/* chWorkSheet:Range("F:F"):columnwidth = 10.                                         */

chWorkSheet:Range("B2"):Select().
chExcelApplication:ActiveWindow:FreezePanes = TRUE.


chExcelApplication:ScreenUpdating = YES.
chExcelApplication:VISIBLE = TRUE.

RELEASE OBJECT chBorder NO-ERROR.
RELEASE OBJECT chBefore NO-ERROR.
RELEASE OBJECT chInterior NO-ERROR.
RELEASE OBJECT chWorksheet NO-ERROR.
RELEASE OBJECT chWorkbook NO-ERROR.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Registrerat C-Win 
FUNCTION Registrerat RETURNS LOGICAL
  ( INPUT ipRow AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR wRow AS INT NO-UNDO.
DEF VAR wCol AS INT NO-UNDO.
DEF VAR iAnt AS INT NO-UNDO.


IF ipRow = ? THEN DO wRow = chGrid:FixedRows TO chGrid:Rows - 1:
  DO wCol = chGrid:FixedCols TO chGrid:Cols - 1:
    IF INT(chGrid:TextMatrix(wRow,wCol)) NE 0 THEN
       RETURN TRUE.
  END.
END.
ELSE DO wCol = chGrid:FixedCols TO chGrid:Cols - 1:
    IF INT(chGrid:TextMatrix(ipRow,wCol)) > 0 OR INT(chGrid:TextMatrix(ipRow,wCol)) < 0 THEN
       RETURN TRUE.
END.


RETURN FALSE.   /* Function return value. */

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
/* IF cButikkListe = "" THEN cButikkListe = "*".  */
/* cButikkListe = "260".  */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setOverlays C-Win 
FUNCTION setOverlays RETURNS LOGICAL
  ( INPUT icAction AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cMerknadList  AS CHAR NO-UNDO.
DEF VAR hBtnFarg      AS HANDLE NO-UNDO.
DEF VAR hBtnStrTypeId AS HANDLE NO-UNDO.
DEF VAR hBtnVg        AS HANDLE NO-UNDO.
DEF VAR hBtnLevKod    AS HANDLE NO-UNDO.

IF icAction = "link" THEN DO:
  IF hFieldMap:BUFFER-FIELD("Oppmerking"):BUFFER-VALUE = "" THEN DO:
    IF VALID-HANDLE(hBrwOLMerk) THEN DO:        
      DYNAMIC-FUNCTION("DeleteObjectLink",hBrowseLinje,hBrwOLMerk).
      hBrwOLMerk:HIDDEN = YES.
    END.
    DYNAMIC-FUNCTION("CreateOverlayLink",hBrowseLinje,hBrwOLMerkFi,"LinjeMerknad").
  END.
  ELSE DO:      
    DYNAMIC-FUNCTION("DeleteObjectLink",hBrowseLinje,hBrwOLMerkFi).
    IF NOT VALID-HANDLE(hBrwOLMerk) THEN DO:        
      hBrwOLMerk = DYNAMIC-FUNCTION("NewBrowseDropDown",
                        hBrowseLinje,                  
                        "LinjeMerknad",                
                        "LinjeMerknad",                
                        "","","du|mmy").                           
      ASSIGN hBrwOLMerk:NAME   = "fiLinjeMerknad"
             hBrwOLMerk:HELP   = "Merknad"
             hBrwOLMerk:FORMAT = "x(256)".
      DYNAMIC-FUNCTION("setAttribute",hBrwOLMerk,"refreshrow","yes").
      DYNAMIC-FUNCTION("CreateOverlayLink",hBrowseLinje,hBrwOLMerk,"LinjeMerknad"). 
      DO ix = 1 TO NUM-ENTRIES(hFieldMap:BUFFER-FIELD("Oppmerking"):BUFFER-VALUE,"¤"):
        cMerknadList = cMerknadList + 
                       ENTRY(ix,hFieldMap:BUFFER-FIELD("Oppmerking"):BUFFER-VALUE,"¤") + "|" +
                       ENTRY(ix,hFieldMap:BUFFER-FIELD("Oppmerking"):BUFFER-VALUE,"¤") + "|".
      END.
      hBrwOLMerk:LIST-ITEM-PAIRS = IF cMerknadList NE "" THEN "||" + TRIM(cMerknadList,"|") ELSE "|".
    END.
  END.
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowseLinje,hBrwOLLevKod,"LevKod").
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowseLinje,hBrwOLMatKod,"MatKod").
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowseLinje,hBrwOLBeskr,"Beskr").
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowseLinje,hBrwOLLevFargKod,"LevFargKod").
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowseLinje,hBrwOLFarg,"Farg").
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowseLinje,hBrwOLStrTypeId,"StrTypeId").
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowseLinje,hBrwOLVg,"Vg").
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowseLinje,hBrwOLInnkjopsPris,"InnkjopsPris").
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowseLinje,hBrwOLForhRab%,"ForhRab%").
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowseLinje,hBrwOLSupRab%,"SupRab%").
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowseLinje,hBrwOLVarekost,"Varekost").
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowseLinje,hBrwOLPris,"Pris").
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowseLinje,hBrwOLAnbefaltPris,"AnbefaltPris").
  hBrwOLLevKod:BGCOLOR = ?.
END.
ELSE DO:
  DYNAMIC-FUNCTION("DeleteObjectLink",hBrowseLinje,hBrwOLLevKod).
  DYNAMIC-FUNCTION("DeleteObjectLink",hBrowseLinje,hBrwOLMatKod).
  DYNAMIC-FUNCTION("DeleteObjectLink",hBrowseLinje,hBrwOLBeskr).
  DYNAMIC-FUNCTION("DeleteObjectLink",hBrowseLinje,hBrwOLLevFargKod).
  DYNAMIC-FUNCTION("DeleteObjectLink",hBrowseLinje,hBrwOLFarg).
  DYNAMIC-FUNCTION("DeleteObjectLink",hBrowseLinje,hBrwOLStrTypeId).
  DYNAMIC-FUNCTION("DeleteObjectLink",hBrowseLinje,hBrwOLVg).
  DYNAMIC-FUNCTION("DeleteObjectLink",hBrowseLinje,hBrwOLInnkjopsPris).
  DYNAMIC-FUNCTION("DeleteObjectLink",hBrowseLinje,hBrwOLForhRab%).
  DYNAMIC-FUNCTION("DeleteObjectLink",hBrowseLinje,hBrwOLSupRab%).
  DYNAMIC-FUNCTION("DeleteObjectLink",hBrowseLinje,hBrwOLVarekost).
  DYNAMIC-FUNCTION("DeleteObjectLink",hBrowseLinje,hBrwOLPris).
  DYNAMIC-FUNCTION("DeleteObjectLink",hBrowseLinje,hBrwOLAnbefaltPris).
  DYNAMIC-FUNCTION("DeleteObjectLink",hBrowseLinje,hBrwOLMerkFi).

  ASSIGN hBrwOLLevKod:HIDDEN       = YES      
         hBrwOLMatKod:HIDDEN       = YES      
         hBrwOLBeskr:HIDDEN        = YES       
         hBrwOLLevFargKod:HIDDEN   = YES  
         hBrwOLFarg:HIDDEN         = YES        
         hBrwOLStrTypeId:HIDDEN    = YES   
         hBrwOLVg:HIDDEN           = YES          
         hBrwOLInnkjopsPris:HIDDEN = YES
         hBrwOLForhRab%:HIDDEN     = YES        
         hBrwOLSupRab%:HIDDEN      = YES        
         hBrwOLVarekost:HIDDEN     = YES        
         hBrwOLPris:HIDDEN         = YES        
         hBrwOLAnbefaltPris:HIDDEN = YES
         hBrwOLMerkFi:HIDDEN       = YES      
         hBtnLevKod                = DYNAMIC-FUNCTION("getEventWidget",hBrwOLLevKod,"choose","browse-lookup")
         hBtnFarg                  = DYNAMIC-FUNCTION("getEventWidget",hBrwOLFarg,"choose","browse-lookup")
         hBtnStrTypeId             = DYNAMIC-FUNCTION("getEventWidget",hBrwOLStrTypeId,"choose","browse-lookup")
         hBtnVg                    = DYNAMIC-FUNCTION("getEventWidget",hBrwOLVg,"choose","browse-lookup")
         hBtnLevKod:HIDDEN         = YES
         hBtnFarg:HIDDEN           = YES
         hBtnStrTypeId:HIDDEN      = YES
         hBtnVg:HIDDEN             = YES
         NO-ERROR.
  IF hFieldMap:AVAIL AND hFieldMap:BUFFER-FIELD("Oppmerking"):BUFFER-VALUE = "" THEN DO:
    DYNAMIC-FUNCTION("DeleteObjectLink",hBrowseLinje,hBrwOLMerk).
    hBrwOLMerk:HIDDEN = YES NO-ERROR.
  END.
END.
  
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setOverlayValues C-Win 
FUNCTION setOverlayValues RETURNS LOGICAL
  ( INPUT icArtikkelNr AS CHAR,
    INPUT icLevKod     AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hColumn AS HANDLE NO-UNDO.
DEF VAR cValue  AS CHAR   NO-UNDO.

IF hBrowseLinje:NUM-ITERATIONS = 1 AND icArtikkelNr = "" AND icLevKod = "" THEN DO WITH FRAME frmLinje:
  IF INT(sokVg:SCREEN-VALUE) NE 0 AND VALID-HANDLE(hBrwOLVg) THEN DO:
    hBrwOLVg:SCREEN-VALUE = sokVg:SCREEN-VALUE.
    hColumn = hFieldMapLinje:BUFFER-FIELD("Vg") NO-ERROR.
    IF VALID-HANDLE(hColumn) THEN
      hFieldMapLinje:BUFFER-FIELD("Vg"):BUFFER-VALUE = sokVg:SCREEN-VALUE.
    hColumn = DYNAMIC-FUNCTION("getBrowseColumn",hBrowseLinje,"Vg").
    IF VALID-HANDLE(hColumn) THEN
      hColumn:SCREEN-VALUE = sokVg:SCREEN-VALUE.      
  END.

  DYNAMIC-FUNCTION("setAttribute",hBrwOLLevKod,"overlaybgcolnum","?").
/*   hBrwOLLevKod:BGCOLOR = ?.  */
  RETURN YES.
END.
IF icArtikkelNr NE "" OR icLevKod NE "" THEN DO:    
  IF icLevKod NE "" THEN
    cValue = DYNAMIC-FUNCTION("getFieldValues","ArtBas",
                           "WHERE Levnr = " + sokLevNr:SCREEN-VALUE IN FRAME frmLinje
                         + " AND LevKod = '" + icLevKod + "'"
                          ,cArtFields).
  ELSE 
    cValue = DYNAMIC-FUNCTION("getFieldValues","ArtBas",
                           "WHERE ArtikkelNr = " + icArtikkelNr
                          ,cArtFields).

  IF cValue NE ? THEN DO:      
    IF NOT CAN-FIND(FIRST ttInit) THEN HentVerdier().
    DO ix = 1 TO NUM-ENTRIES(cArtFields):
      FIND FIRST ttInit
           WHERE ttInit.cNavn = ENTRY(ix,cArtFields)
           NO-ERROR.
      IF AVAIL ttInit THEN
        ttInit.cVerdi = ENTRY(ix,cValue,"|").
    END.
    IF DYNAMIC-FUNCTION("runProc","artpris_hent_felter.p",ENTRY(1,cValue,"|") + ";" + cInitPrisProfil + "|" + cArtPrisFields,?) THEN DO:
      cValue = DYNAMIC-FUNCTION("getTransactionMessage").
  
      DO ix = 1 TO NUM-ENTRIES(cArtPrisFields):
        FIND FIRST ttInit
             WHERE ttInit.cNavn = IF ENTRY(ix,cArtPrisFields) NE "Rab1%" THEN ENTRY(ix,cArtPrisFields) ELSE "Rab%"
             NO-ERROR.
        IF AVAIL ttInit THEN 
          ttInit.cVerdi = ENTRY(ix,cValue,"|").
      END.
/*       hBrwOLLevKod:BGCOLOR = 17. */
      DYNAMIC-FUNCTION("setAttribute",hBrwOLLevKod,"overlaybgcolnum","17").
    END.
    ELSE RETURN NO.
  END.
  ELSE DO:
/*     hBrwOLLevKod:BGCOLOR = ?. */
    DYNAMIC-FUNCTION("setAttribute",hBrwOLLevKod,"overlaybgcolnum","?").
    RETURN YES.
  END. 
END.

FOR EACH ttInit:
  IF VALID-HANDLE(ttInit.hFillIn) AND ttInit.hFillIn:TYPE = "combo-box" THEN NEXT.

  IF VALID-HANDLE(ttInit.hFillIn) THEN 
    ttInit.hFillIn:SCREEN-VALUE = ttInit.cVerdi.
  hColumn = hFieldMapLinje:BUFFER-FIELD(ttInit.cNavn) NO-ERROR.
  IF VALID-HANDLE(hColumn) THEN
    hFieldMapLinje:BUFFER-FIELD(ttInit.cNavn):BUFFER-VALUE = ttInit.cVerdi.
  hColumn = DYNAMIC-FUNCTION("getBrowseColumn",hBrowseLinje,ttInit.cNavn).
  IF VALID-HANDLE(hColumn) THEN
    hColumn:SCREEN-VALUE = ttInit.cVerdi.
END.
hBrowseLinje:REFRESH() NO-ERROR.

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

/* cTabStripList = "Liste|Detalj|Registrering|Statistikk". */
cTabStripList = "Liste|Detalj|Registrering|Ordre".

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SettLevDatoEllerDag C-Win 
FUNCTION SettLevDatoEllerDag RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DO WITH FRAME frmLinje:
  IF hFieldMap:AVAIL THEN DO:
    IF hFieldMap:BUFFER-FIELD("VareBehType"):BUFFER-VALUE = iBestVarebeh THEN DO:
      ASSIGN LevDato:HIDDEN            = FALSE
             btnLevDatoDate:HIDDEN     = FALSE
             DirekteLev:HIDDEN         = FALSE
             hLagerStatusFrame:HIDDEN  = FALSE
             rectBrwAndreBest:HIDDEN   = FALSE
             .
      DO ix = 1 TO hBrowseBestHode:NUM-COLUMNS:
        hBrowseBestHode:GET-BROWSE-COLUMN(ix):VISIBLE = CAN-DO(cBestViewFields,hBrowseBestHode:GET-BROWSE-COLUMN(ix):NAME).
      END.        
      Grid:MOVE-AFTER(LevDato:HANDLE).
    END.
    ELSE DO:
      ASSIGN LevDato:HIDDEN            = TRUE
             btnLevDatoDate:HIDDEN     = TRUE
             DirekteLev:HIDDEN         = TRUE
             hLagerStatusFrame:HIDDEN  = FALSE
             rectBrwAndreBest:HIDDEN   = TRUE
             .
      DO ix = 1 TO hBrowseBestHode:NUM-COLUMNS:
        hBrowseBestHode:GET-BROWSE-COLUMN(ix):VISIBLE = CAN-DO(cInnlevViewFields,hBrowseBestHode:GET-BROWSE-COLUMN(ix):NAME).
      END.
    END.
  END.
  IF hFieldMapBestHode:AVAIL AND hFieldMapBestHode:BUFFER-FIELD("BestNr"):BUFFER-VALUE NE 0 THEN
    ASSIGN LevDato:SENSITIVE        = FALSE
           btnLevDatoDate:SENSITIVE = FALSE
           DirekteLev:SENSITIVE     = FALSE.

END.

RETURN TRUE.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setView C-Win 
FUNCTION setView RETURNS LOGICAL
  ( INPUT icMode AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cSetFields AS CHAR NO-UNDO.

IF (icMode =  "create" AND DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"currviewfields") = cCreateViewFields) OR
   (icMode NE "create" AND DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"currviewfields") = cDefaultViewFields) THEN
  RETURN NO.

IF icMode = "create" THEN
  cSetFields = cCreateViewFields.
ELSE
  cSetFields = cDefaultViewFields.

DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"currviewfields",cSetFields).  
DYNAMIC-FUNCTION("setBrowseColumns",hBrowseLinje,cSetFields,NO).
  
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SkrivEtikett C-Win 
FUNCTION SkrivEtikett RETURNS LOGICAL
  ( INPUT icListe AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cArtikkelEti    AS CHAR NO-UNDO.
DEF VAR cEtiketter      AS CHAR NO-UNDO.
DEF VAR cAntallEti      AS CHAR NO-UNDO.
DEF VAR cIndividNr      AS CHAR NO-UNDO.
DEF VAR iCount          AS INT  NO-UNDO.
DEF VAR cArtEANlist     AS CHAR NO-UNDO.
DEF VAR cArtANTlist     AS CHAR NO-UNDO.
DEF VAR cArtINDlist     AS CHAR NO-UNDO.
DEF VAR iTotAnt         AS INT  NO-UNDO.

IF NUM-ENTRIES(icListe,"|") < 4 THEN RETURN FALSE.

ASSIGN icListe      = SUBSTR(icListe,8)
       cArtikkelEti = ENTRY(1,icListe,"|")
       cEtiketter   = ENTRY(2,icListe,"|")
       cAntallEti   = ENTRY(3,icListe,"|")
       cIndividNr   = ENTRY(4,icListe,"|")
       .

IF cArtikkelEti <> "" THEN DO:
  DO ix = 1 TO NUM-ENTRIES(cArtikkelEti,CHR(1)):
    ASSIGN cArtEANlist = ENTRY(ix,cEtiketter,CHR(1))
           cArtANTlist = ENTRY(ix,cAntallEti,CHR(1))
           cArtINDlist = ENTRY(ix,cIndividNr,CHR(1))
           .
    IF cArtEANlist <> "" THEN 
      DO iCount = 1 TO NUM-ENTRIES(cArtANTlist):
        IF ENTRY(iCount,cArtEANlist) <> "" AND INT(ENTRY(iCount,cArtANTlist)) > 0 THEN
          iTotAnt = iTotAnt + INT(ENTRY(iCount,cArtANTlist)).
      END.
  END.

  IF iTotAnt LE 0 THEN RETURN FALSE.

  DO ix = 1 TO NUM-ENTRIES(cArtikkelEti,CHR(1)):
    ASSIGN cArtEANlist = ENTRY(ix,cEtiketter,CHR(1))
           cArtANTlist = ENTRY(ix,cAntallEti,CHR(1))
           cArtINDlist = ENTRY(ix,cIndividNr,CHR(1))
           .
    IF cArtEANlist <> "" THEN DO:
      IF NOT VALID-HANDLE(hEtikettVindu) THEN
          RUN w-TmpEtikett.w PERSISTENT SET hEtikettVindu (THIS-PROCEDURE:CURRENT-WINDOW).
      IF VALID-HANDLE(hEtikettVindu) THEN 
        DO iCount = 1 TO NUM-ENTRIES(cArtANTlist):
          IF ENTRY(iCount,cArtEANlist) <> "" AND INT(ENTRY(iCount,cArtANTlist)) > 0 THEN
            RUN NyEtikett IN hEtikettVindu (ENTRY(iCount,cArtEANlist),INT(ENTRY(iCount,cArtANTlist)),INT(ENTRY(iCount,cArtINDlist))).
        END.
    END.
  END.
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

IF DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"uselocaldata") = "yes" THEN DO:
  chTabStrip:TabStrip:Tabs:ITEM(3):SELECTED = TRUE.
  RETURN FALSE.    
END.

IF (bHKinst AND cButikkliste NE "*" AND iiTab = 2) THEN DO:
  TabStripChanged(13).
  RETURN FALSE.
END.

DO WITH FRAME {&FRAME-NAME}:

  IF iiTab > 10 THEN DO:
    iiTab = iiTab - 10.
    chTabStrip:TabStrip:Tabs:ITEM(iiTab):SELECTED = TRUE.
  END.

  iTab = iiTab.

  IF iTab < 3 THEN DO:
    DYNAMIC-FUNCTION("ReplaceObjectLink",hUpdToolbar,hFieldMap).
    DYNAMIC-FUNCTION("ReplaceObjectLink",hUpdToolbar,hBrowseListe).
    DYNAMIC-FUNCTION("DeleteObjectLink",hBrowseListe,hBrowseLinje).
    DYNAMIC-FUNCTION("setCurrentObject",hBrowseListe).
    RUN DisplayRecord.

    IF VALID-HANDLE(hNavVarebehToolBar) THEN DO:
      DYNAMIC-FUNCTION("DeleteObject",hNavVarebehToolBar).
      hNavVarebehToolBar = ?.
    END.
  END.
  DYNAMIC-FUNCTION("DeleteObjectLink",hBrowseListe,hBrowseOrdre).
  
  DYNAMIC-FUNCTION("setAttribute",hUpdToolBar,"disabledevents","").

  hArtBildeFrame:MOVE-TO-BOTTOM().
  hBestBildeFrame:MOVE-TO-BOTTOM().
  hLagerStatusFrame:MOVE-TO-BOTTOM().

  IF NOT hFieldMap:AVAIL THEN 
    hBrowseListe:SELECT-ROW(hBrowseListe:FOCUSED-ROW) NO-ERROR.

  IF iiTab = 1 THEN
  DO:
      FRAME frmListe:MOVE-TO-TOP().
      FRAME frmFilter:MOVE-TO-TOP().
      APPLY "entry" TO hBrowseListe.
  END.
  ELSE IF iiTab = 2 THEN DO:
    FRAME frmDetalj:MOVE-TO-TOP().
    IF CAN-DO('new',cstate) THEN
      ASSIGN ProfilNr:SENSITIVE       = TRUE 
             btnSokProfilNr:SENSITIVE = TRUE
            .
  END.
  ELSE IF iiTab = 3 THEN DO:
    hArtBildeFrame:MOVE-TO-TOP().
    hLagerStatusFrame:MOVE-TO-TOP().
    FRAME frmLinje:MOVE-TO-TOP().

    DYNAMIC-FUNCTION("replaceObjectLink",hBrowseLinje,hUpdToolbar).
    DYNAMIC-FUNCTION("replaceObjectLink",hFieldMapLinje,hUpdToolbar).

    DYNAMIC-FUNCTION("createParentLink",hBrowseLinje,hBrowseListe,'VarebehNr').
    SettLevDatoEllerDag().

    APPLY "value-changed" TO hBrowseListe.

    APPLY "ENTRY":U TO hBrowseLinje.
  END.
  ELSE IF iiTab = 4 THEN DO:
    hBestBildeFrame:MOVE-TO-TOP().
    FRAME frmOrdre:MOVE-TO-TOP().

    DYNAMIC-FUNCTION("DeleteLinksFrom",hUpdToolbar).
    DYNAMIC-FUNCTION("DeleteObjectLink",hBrowseListe,hBrowseLinje).
    DYNAMIC-FUNCTION("setToolbar",hUpdToolbar,"disable").

    DYNAMIC-FUNCTION("createParentLink",hBrowseOrdre,hBrowseListe,'VarebehNr').

    APPLY "value-changed" TO hBrowseListe.

    APPLY "ENTRY":U TO hBrowseOrdre.
  END.

  IF iiTab > 2 AND hFieldMap:AVAIL AND NOT VALID-HANDLE(hNavVarebehToolBar) THEN DO:
    hNavVarebehToolBar = DYNAMIC-FUNCTION("NewToolBar",
                      rectNavVarebeh:HANDLE,            /* Rectangle to define coordinates for toolbar */
                      "Naviger",                          /* Corresponding menu label - no menu if blank */
                      "Last;Siste Vareh.bok&,Next;Neste Vareh.bok&,Prev;Forrige Vareh.bok,First;Første Vareh.bok",
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

    IF hFieldMapLinje:AVAIL AND STRING(hFieldMapLinje:ROWID) = DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"firstrowid") THEN
      DYNAMIC-FUNCTION("setToolbar",hUpdToolbar,"first").
    ELSE IF hFieldMapLinje:AVAIL AND STRING(hFieldMapLinje:ROWID) = DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"lastrowid") THEN
      DYNAMIC-FUNCTION("setToolbar",hUpdToolbar,"last").

    IF hFieldMap:AVAIL AND STRING(hFieldMap:ROWID) = DYNAMIC-FUNCTION("getAttribute",hBrowseListe,"firstrowid") THEN
      DYNAMIC-FUNCTION("setToolbar",hNavVarebehToolBar,"first").
    ELSE IF hFieldMap:AVAIL AND STRING(hFieldMap:ROWID) = DYNAMIC-FUNCTION("getAttribute",hBrowseListe,"lastrowid") THEN
      DYNAMIC-FUNCTION("setToolbar",hNavVarebehToolBar,"last").

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ViewFrameOkReg C-Win 
FUNCTION ViewFrameOkReg RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hLastEnabled AS HANDLE NO-UNDO.

hLastEnabled = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"lastenabledoverlay")).

FRAME frmOkReg:Y = 163.

IF VALID-HANDLE(hLastEnabled) THEN
  FRAME frmOkReg:X = hLastEnabled:X - 151.
ELSE  
  FRAME frmOkReg:X = hBrowseLinje:WIDTH-PIXELS - 151.
       
FRAME frmOkReg:HIDDEN = NO.
FRAME frmOkReg:MOVE-TO-TOP().

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ViewOverlays C-Win 
FUNCTION ViewOverlays RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hBtnFarg      AS HANDLE NO-UNDO.
DEF VAR hBtnStrTypeId AS HANDLE NO-UNDO.
DEF VAR hBtnVg        AS HANDLE NO-UNDO.

IF hFieldMapLinje:AVAIL AND  DYNAMIC-FUNCTION("GetLinkedObject",hBrwOLLevKod,"browse","from") NE ? AND hBrwOLLevKod:HIDDEN THEN 
  ASSIGN hBrwOLLevKod:HIDDEN       = NO      
         hBrwOLBeskr:HIDDEN        = NO       
         hBrwOLLevFargKod:HIDDEN   = NO  
         hBrwOLFarg:HIDDEN         = NO        
         hBrwOLStrTypeId:HIDDEN    = NO   
         hBrwOLVg:HIDDEN           = NO          
         hBrwOLInnkjopsPris:HIDDEN = NO
         hBrwOLForhRab%:HIDDEN     = NO        
         hBrwOLSupRab%:HIDDEN      = NO        
         hBrwOLPris:HIDDEN         = NO        
         hBrwOLAnbefaltPris:HIDDEN = NO
         hBrwOLMerkFi:HIDDEN       = NO      
         hBtnFarg                  = DYNAMIC-FUNCTION("getEventWidget",hBrwOLFarg,"choose","browse-lookup")
         hBtnStrTypeId             = DYNAMIC-FUNCTION("getEventWidget",hBrwOLStrTypeId,"choose","browse-lookup")
         hBtnVg                    = DYNAMIC-FUNCTION("getEventWidget",hBrwOLVg,"choose","browse-lookup")
         hBtnFarg:HIDDEN           = NO
         hBtnStrTypeId:HIDDEN      = NO
         hBtnVg:HIDDEN             = NO
         NO-ERROR.
  
RETURN YES.

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
DEF VAR cStatFieldValues  AS CHAR NO-UNDO.
DEF VAR hField            AS HANDLE NO-UNDO.
DEF VAR iCount            AS INT NO-UNDO.

cStatFieldValues = DYNAMIC-FUNCTION("getAttribute",ihBrowse,"querystatfieldvalues").
  
IF bUpdateStat AND cStatFieldValues NE "" THEN DO:
  DO ix = 1 TO NUM-ENTRIES(cStatFieldValues,";"):
    IF ENTRY(1,ENTRY(ix,cStatFieldValues,";"),"|") = "rowcount" THEN
      ASSIGN iCount            = INT(ENTRY(2,ENTRY(ix,cStatFieldValues,";"),"|"))
             hBrowseLinje:HELP = ENTRY(2,ENTRY(ix,cStatFieldValues,";"),"|").
    ELSE IF CAN-DO(cPrefixedStatFields,"tot_" + ENTRY(1,ENTRY(ix,cStatFieldValues,";"),"|")) THEN 
      ASSIGN hField = WIDGET-HANDLE(ENTRY(LOOKUP(ENTRY(1,ENTRY(ix,cStatFieldValues,";"),"|"),cStatFieldNames),cStatFieldHandles))
             hField:SCREEN-VALUE = ENTRY(2,ENTRY(ix,cStatFieldValues,";"),"|")
             .
    ELSE IF CAN-DO(cPrefixedStatFields,"avg_" + ENTRY(1,ENTRY(ix,cStatFieldValues,";"),"|")) THEN DO:
      ASSIGN hField = WIDGET-HANDLE(ENTRY(LOOKUP(ENTRY(1,ENTRY(ix,cStatFieldValues,";"),"|"),cStatFieldNames),cStatFieldHandles))
             hField:SCREEN-VALUE = STRING(DEC(ENTRY(2,ENTRY(ix,cStatFieldValues,";"),"|")) / iCount)
             .
    END.
    ELSE IF CAN-DO(cStatFieldNames,ENTRY(1,ENTRY(ix,cStatFieldValues,";"),"|")) THEN 
      ASSIGN hField = WIDGET-HANDLE(ENTRY(LOOKUP(ENTRY(1,ENTRY(ix,cStatFieldValues,";"),"|"),cStatFieldNames),cStatFieldHandles))
             hField:SCREEN-VALUE = ENTRY(2,ENTRY(ix,cStatFieldValues,";"),"|")
             .
  END.
END.
ELSE IF NOT bUpdateStat THEN
  DO ix = 1 TO NUM-ENTRIES(cStatFieldHandles):
    ASSIGN hField = WIDGET-HANDLE(ENTRY(ix,cStatFieldHandles))
           hField:SCREEN-VALUE = "".
  END.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION VisLagerStatus C-Win 
FUNCTION VisLagerStatus RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  Sett innhold i grid-objekt for lagerstatus
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cLagerInfo AS CHAR NO-UNDO.
DEF VAR cLabel     AS CHAR NO-UNDO.
DEF VAR cRows      AS CHAR NO-UNDO.

IF NOT hFieldMapLinje:AVAIL OR DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"uselocaldata") = "yes" THEN DO:
  DYNAMIC-FUNCTION("setLabels" IN hLagerStatus,"").
  RETURN FALSE.
END.

IF rsLagerStatusSalg:SCREEN-VALUE IN FRAME frmLinje = "1" THEN DO:
  IF DYNAMIC-FUNCTION("runproc","artikkel_lagerinfo.p",STRING(hFieldMapLinje:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) + ";" + cButikkListe,?) THEN DO:
    cLagerInfo = DYNAMIC-FUNCTION("getTransactionMessage").
    DYNAMIC-FUNCTION("setLabels" IN hLagerStatus,REPLACE(ENTRY(1,cLagerInfo,"¤"),",","|")).
    DO ix = 1 TO NUM-ENTRIES(ENTRY(2,cLagerInfo,"¤"),";"):
      DYNAMIC-FUNCTION("addRow" IN hLagerStatus,ENTRY(ix,ENTRY(2,cLagerInfo,"¤"),";")).
    END.
    IF NUM-ENTRIES(cLagerInfo,"¤") = 3 THEN DO:
      DO ix = 1 TO NUM-ENTRIES(ENTRY(3,cLagerInfo,"¤"),";"):
        DYNAMIC-FUNCTION("addRow" IN hLagerStatus,ENTRY(ix,ENTRY(3,cLagerInfo,"¤"),";")).
      END.
      DYNAMIC-FUNCTION("setLabelRows" IN hLagerStatus,3).
    END.

    DYNAMIC-FUNCTION("setLabelColumns" IN hLagerStatus,2).
  END.
  ELSE DYNAMIC-FUNCTION("setLabels" IN hLagerStatus,"").
END.
ELSE DO:
  IF DYNAMIC-FUNCTION("runproc","artikkel_salg.p",STRING(hFieldMapLinje:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) + ";" + cButikkListe,?) THEN DO:
    cLagerInfo = DYNAMIC-FUNCTION("getTransactionMessage").
    DYNAMIC-FUNCTION("setLabels" IN hLagerStatus,REPLACE(ENTRY(1,cLagerInfo,"¤"),",","|")).
    DO ix = 1 TO NUM-ENTRIES(ENTRY(2,cLagerInfo,"¤"),";"):
      DYNAMIC-FUNCTION("addRow" IN hLagerStatus,ENTRY(ix,ENTRY(2,cLagerInfo,"¤"),";")).
    END.
    IF NUM-ENTRIES(cLagerInfo,"¤") = 3 THEN DO:
      DO ix = 1 TO NUM-ENTRIES(ENTRY(3,cLagerInfo,"¤"),";"):
        DYNAMIC-FUNCTION("addRow" IN hLagerStatus,ENTRY(ix,ENTRY(3,cLagerInfo,"¤"),";")).
      END.
      DYNAMIC-FUNCTION("setLabelRows" IN hLagerStatus,3).
    END.

    DYNAMIC-FUNCTION("setLabelColumns" IN hLagerStatus,1).
  END.
  ELSE DYNAMIC-FUNCTION("setLabels" IN hLagerStatus,"").
END.

DYNAMIC-FUNCTION("adjustColumns" IN hLagerStatus).

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION xx_fetchProcessedRows C-Win 
FUNCTION xx_fetchProcessedRows RETURNS LOGICAL
  ( INPUT ihBuffer AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
MESSAGE PROGRAM-NAME(1) SKIP
        ihBuffer:NAME  SKIP
        VIEW-AS ALERT-BOX.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

