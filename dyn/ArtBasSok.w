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

DEF VAR bOK                    AS LOG    NO-UNDO.
DEF VAR ix                     AS INT    NO-UNDO.

DEF VAR hParent                AS HANDLE NO-UNDO.
DEF VAR fKOrdreId              AS DEC    NO-UNDO.
DEF VAR fVarebehNr             AS DEC    NO-UNDO.

DEF VAR hToolbar               AS HANDLE NO-UNDO.
DEF VAR hBrowse                AS HANDLE NO-UNDO.
DEF VAR hBrwArtikkel           AS HANDLE NO-UNDO.
DEF VAR hBuffer                AS HANDLE NO-UNDO.
DEF VAR hBuffArtBas            AS HANDLE NO-UNDO.
DEF VAR hBuffVPI               AS HANDLE NO-UNDO.
DEF VAR hBrwVPI                AS HANDLE NO-UNDO.
DEF VAR hBrwStr                AS HANDLE NO-UNDO.
DEF VAR hBuffStr               AS HANDLE NO-UNDO.
DEF VAR hPlukkStrOverlay       AS HANDLE NO-UNDO.

DEF VAR cCurrSelectBuffer      AS CHAR   NO-UNDO.
DEF VAR cAvdelingRowIdList     AS CHAR   NO-UNDO.
DEF VAR cAvdelingIdList        AS CHAR   NO-UNDO.
DEF VAR cHuvGrAvdelingList     AS CHAR   NO-UNDO.
DEF VAR cVarGrHuvGrList        AS CHAR   NO-UNDO.
DEF VAR cLevBasRowIdList       AS CHAR   NO-UNDO.
DEF VAR cLevBasIdList          AS CHAR   NO-UNDO.
DEF VAR cHuvGrRowIdList        AS CHAR   NO-UNDO.
DEF VAR cHuvGrIdList           AS CHAR   NO-UNDO.
DEF VAR cVarGrRowIdList        AS CHAR   NO-UNDO.
DEF VAR cVarGrIdList           AS CHAR   NO-UNDO.
DEF VAR iSelectorSourcCount    AS INT    NO-UNDO.
DEF VAR fKundeNr               AS DEC    NO-UNDO.
DEF VAR lRunProc               AS LOG    NO-UNDO.

DEF VAR hVisBilde              AS HANDLE NO-UNDO.
DEF VAR hArtikkelkort          AS HANDLE NO-UNDO.
DEF VAR hArtBilde              AS HANDLE NO-UNDO.
DEF VAR cArtBasJoin            AS CHAR   NO-UNDO.
DEF VAR cPrisJoin              AS CHAR   NO-UNDO.
DEF VAR cArtBasFields          AS CHAR   NO-UNDO.
DEF VAR cButikkListe           AS CHAR   NO-UNDO.
DEF VAR cButikkNr              AS CHAR   NO-UNDO.

DEF VAR hTilbField             AS HANDLE NO-UNDO.
DEF VAR hPrisColumn            AS HANDLE NO-UNDO.

DEF VAR hPaaOrdreField         AS HANDLE NO-UNDO.
DEF VAR hPaaVarebeh            AS HANDLE NO-UNDO.
DEF VAR hArtNrColumn           AS HANDLE NO-UNDO.

DEF VAR bCloseOnSelect         AS LOG    NO-UNDO.
DEF VAR bUpdateCurrentRow      AS LOG    NO-UNDO.
DEF VAR cArtNrStrKode          AS CHAR   NO-UNDO.
DEF VAR bEANsok                AS LOG    NO-UNDO.
DEF VAR bStrAdded              AS LOG    NO-UNDO.

DEF VAR fKampId                AS DEC    NO-UNDO.
DEF VAR iKampTilbId            AS INT    NO-UNDO.
DEF VAR hKampTilbArtId         AS HANDLE NO-UNDO.
DEF VAR hArtNr                 AS HANDLE NO-UNDO.
DEF VAR bHideStr               AS LOG    NO-UNDO.
DEF VAR bVPIsearch             AS LOG    NO-UNDO.
DEF VAR cTekst                 AS CHAR   NO-UNDO.
DEF VAR cEAN                   AS CHAR   NO-UNDO.
DEF VAR hFldArtBasExists       AS HANDLE NO-UNDO.
DEF VAR hBrwColVpiArtNr        AS HANDLE NO-UNDO.
DEF VAR hBtnSokVpi             AS HANDLE NO-UNDO.
DEF VAR fReposArtnr            AS DEC    NO-UNDO.
DEF VAR i1ButNr                AS INT    NO-UNDO.

DEFINE STREAM Stream1.
DEF VAR cBildeKatalog          AS CHAR   NO-UNDO.

DEF TEMP-TABLE ttBildeData
    FIELD BildNr    AS INT
    FIELD Teller    AS INT
    FIELD RawData   AS RAW
    FIELD RowIdent  AS CHAR
    .
DEF VAR hBufBildeData AS HANDLE.
hBufBildeData = BUFFER ttBildeData:HANDLE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BrwArtikkel rectToolBar rectWinToolbar ~
BrwStr ArtikkelBilde BrwVPI sokBeskr sokAvdelingNr btnAvdeling ~
sokArtikkelNr sokStrKod sokHg btnHuvGr sokLevNr sokLevNamn btnLev sokVg ~
btnVarGr sokLevKod sokStr btnStr rsButBeholdning tbLager sokPris fiKjoptMnd ~
tbOPris tbTilbud rsButTidlKjop tbTidlKjop fiKjoptMndLabel 
&Scoped-Define DISPLAYED-OBJECTS sokBeskr sokAvdelingNr sokAvdelingNavn ~
sokArtikkelNr sokStrKod sokHg sokHgBeskr sokLevNr sokLevNamn sokVg ~
sokVgBeskr sokLevKod sokStr rsButBeholdning tbLager sokPris fiKjoptMnd ~
tbOPris tbTilbud rsButTidlKjop tbTidlKjop fiKjoptMndLabel 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AdjustBrowseColumns C-Win 
FUNCTION AdjustBrowseColumns RETURNS LOGICAL
  ( INPUT ihBrowse     AS HANDLE,
    INPUT icBrowseName AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getUpdateCurrentRow C-Win 
FUNCTION getUpdateCurrentRow RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD HideVPIfieldSelect C-Win 
FUNCTION HideVPIfieldSelect RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setAvdNr C-Win 
FUNCTION setAvdNr RETURNS LOGICAL
  ( INPUT icAvdelingNr AS CHAR,
    INPUT ibOpenQuery  AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setBeskr C-Win 
FUNCTION setBeskr RETURNS LOGICAL
  ( INPUT icBeskr      AS CHAR,
    INPUT ibOpenQuery  AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setButikkNr C-Win 
FUNCTION setButikkNr RETURNS LOGICAL
  ( INPUT icButikkNr AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setCloseOnSelect C-Win 
FUNCTION setCloseOnSelect RETURNS LOGICAL
  ( INPUT ibCloseOnSelect AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setHg C-Win 
FUNCTION setHg RETURNS LOGICAL
  ( INPUT icHg     AS CHAR,
    INPUT ibOpenQuery AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setHideStr C-Win 
FUNCTION setHideStr RETURNS LOGICAL
  ( )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setKampanjeInfo C-Win 
FUNCTION setKampanjeInfo RETURNS LOGICAL
  ( INPUT ifKampId AS DEC,
    INPUT iiKampTilbId AS INT)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SetLevKodQuery C-Win 
FUNCTION SetLevKodQuery RETURNS HANDLE
  ( INPUT icLevKod AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setLevnr C-Win 
FUNCTION setLevnr RETURNS LOGICAL
  ( INPUT icLevnr     AS CHAR,
    INPUT ibOpenQuery AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setOrdreId C-Win 
FUNCTION setOrdreId RETURNS LOGICAL
  ( INPUT ifKOrdreId AS DEC )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setUpdateCurrentRow C-Win 
FUNCTION setUpdateCurrentRow RETURNS LOGICAL
  ( INPUT ibUpdateCurrentRow AS LOG)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setVarebehNr C-Win 
FUNCTION setVarebehNr RETURNS LOGICAL
  ( INPUT ifVarebehNr AS DEC )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setVg C-Win 
FUNCTION setVg RETURNS LOGICAL
  ( INPUT icVg     AS CHAR,
    INPUT ibOpenQuery AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnAvdeling 
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnHuvGr 
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnLev 
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnStr 
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnVarGr 
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE ButikkNr AS CHARACTER FORMAT "X(256)" 
     VIEW-AS COMBO-BOX INNER-LINES 20
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 11.2 BY 1 NO-UNDO.

DEFINE VARIABLE fiKjoptMnd AS INTEGER FORMAT ">9":U INITIAL 12 
     LABEL "Siste" 
     VIEW-AS FILL-IN 
     SIZE 4.6 BY 1 TOOLTIP "Angi 0 for å sjekke uavhengig av kjøpstidspunkt" NO-UNDO.

DEFINE VARIABLE fiKjoptMndLabel AS CHARACTER FORMAT "X(256)":U INITIAL "mnd" 
      VIEW-AS TEXT 
     SIZE 4 BY .62 NO-UNDO.

DEFINE VARIABLE sokArtikkelNr AS DECIMAL FORMAT "zzzzzzzzzzzz9" INITIAL 0 
     LABEL "Art.nr" 
     VIEW-AS FILL-IN 
     SIZE 20.4 BY 1 TOOLTIP "ALT-E".

DEFINE VARIABLE sokAvdelingNavn AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE sokAvdelingNr AS INTEGER FORMAT "zzzzz9" INITIAL 0 
     LABEL "Avdeling" 
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1.

DEFINE VARIABLE sokBeskr AS CHARACTER FORMAT "X(256)":U 
     LABEL "Utv.sok" 
     VIEW-AS FILL-IN 
     SIZE 49 BY 1
     BGCOLOR 16  NO-UNDO.

DEFINE VARIABLE sokHg AS INTEGER FORMAT "zzzzz9" INITIAL 0 
     LABEL "Hovedgr" 
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1.

DEFINE VARIABLE sokHgBeskr AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE sokLevKod AS CHARACTER FORMAT "x(30)" 
     LABEL "Lev.Art.nr" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1.

DEFINE VARIABLE sokLevNamn AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1 NO-UNDO.

DEFINE VARIABLE sokLevNr AS INTEGER FORMAT "zzzzz9" INITIAL 0 
     LABEL "Lev" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1.

DEFINE VARIABLE sokPris AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Maks pris" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE sokStr AS CHARACTER FORMAT "X(256)":U 
     LABEL "Størrelser" 
     VIEW-AS FILL-IN 
     SIZE 17.6 BY 1 NO-UNDO.

DEFINE VARIABLE sokStrKod AS CHARACTER FORMAT "X(256)":U 
     LABEL "EAN" 
     VIEW-AS FILL-IN 
     SIZE 22.6 BY 1 NO-UNDO.

DEFINE VARIABLE sokVg AS INTEGER FORMAT "zzzzz9" INITIAL 0 
     LABEL "Varegr" 
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1.

DEFINE VARIABLE sokVgBeskr AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE rsButBeholdning AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Egen butikk", 1,
"Alle butikker", 2
     SIZE 30 BY .95 NO-UNDO.

DEFINE VARIABLE rsButTidlKjop AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Egen butikk", 1,
"Alle butikker", 2
     SIZE 30 BY .95 NO-UNDO.

DEFINE RECTANGLE ArtikkelBilde
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 28 BY 5.19.

DEFINE RECTANGLE BrwArtikkel
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 120 BY 11.91.

DEFINE RECTANGLE BrwStr
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 39 BY 11.91.

DEFINE RECTANGLE BrwVPI
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 120 BY 11.91.

DEFINE RECTANGLE rectToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 28 BY .95.

DEFINE RECTANGLE rectWinToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 9.8 BY .91.

DEFINE VARIABLE tbLager AS LOGICAL INITIAL no 
     LABEL "På lager" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.4 BY .81 NO-UNDO.

DEFINE VARIABLE tbOPris AS LOGICAL INITIAL no 
     LABEL "Åpen pris" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE tbTidlKjop AS LOGICAL INITIAL no 
     LABEL "Tidl.kjøpt" 
     VIEW-AS TOGGLE-BOX
     SIZE 12 BY .81 TOOLTIP "Tidligere kjøpte artikler for kunde" NO-UNDO.

DEFINE VARIABLE tbTilbud AS LOGICAL INITIAL no 
     LABEL "På tilbud" 
     VIEW-AS TOGGLE-BOX
     SIZE 12 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     ButikkNr AT ROW 1.24 COL 126 COLON-ALIGNED HELP
          "Trykk ENTER for å skifte butikk" NO-LABEL
     sokBeskr AT ROW 2.43 COL 9.8 COLON-ALIGNED
     sokAvdelingNr AT ROW 2.43 COL 73 COLON-ALIGNED HELP
          "Varegruppe"
     sokAvdelingNavn AT ROW 2.43 COL 83.8 COLON-ALIGNED NO-LABEL
     btnAvdeling AT ROW 2.43 COL 118.2 NO-TAB-STOP 
     sokArtikkelNr AT ROW 3.48 COL 9.8 COLON-ALIGNED HELP
          "PRS artikkelnr"
     sokStrKod AT ROW 3.48 COL 36.2 COLON-ALIGNED
     sokHg AT ROW 3.48 COL 73 COLON-ALIGNED HELP
          "Varegruppe"
     sokHgBeskr AT ROW 3.48 COL 83.8 COLON-ALIGNED NO-LABEL
     btnHuvGr AT ROW 3.48 COL 118.2 NO-TAB-STOP 
     sokLevNr AT ROW 4.52 COL 9.8 COLON-ALIGNED HELP
          "Varegruppe"
     sokLevNamn AT ROW 4.52 COL 20.8 COLON-ALIGNED NO-LABEL
     btnLev AT ROW 4.52 COL 61 NO-TAB-STOP 
     sokVg AT ROW 4.52 COL 73 COLON-ALIGNED HELP
          "Varegruppe"
     sokVgBeskr AT ROW 4.52 COL 83.8 COLON-ALIGNED NO-LABEL
     btnVarGr AT ROW 4.52 COL 118.2 NO-TAB-STOP 
     sokLevKod AT ROW 5.57 COL 9.8 COLON-ALIGNED HELP
          "Leverandørens artikkelnummer - bestillingsnummer"
     sokStr AT ROW 5.57 COL 41.2 COLON-ALIGNED
     btnStr AT ROW 5.57 COL 61 NO-TAB-STOP 
     rsButBeholdning AT ROW 5.67 COL 88 NO-LABEL
     tbLager AT ROW 5.71 COL 75
     sokPris AT ROW 6.62 COL 41.2 COLON-ALIGNED
     fiKjoptMnd AT ROW 6.67 COL 122.4 COLON-ALIGNED
     tbOPris AT ROW 6.71 COL 12
     tbTilbud AT ROW 6.71 COL 61.2
     rsButTidlKjop AT ROW 6.71 COL 88 NO-LABEL
     tbTidlKjop AT ROW 6.76 COL 75
     fiKjoptMndLabel AT ROW 6.86 COL 127 COLON-ALIGNED NO-LABEL
     BrwArtikkel AT ROW 8.14 COL 2
     rectToolBar AT ROW 1.14 COL 1.8
     rectWinToolbar AT ROW 1.14 COL 151.4
     BrwStr AT ROW 8.14 COL 123
     ArtikkelBilde AT ROW 2.43 COL 133.6
     BrwVPI AT ROW 8.14 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 161.8 BY 19.14.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window Template
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Artikkelsøk"
         HEIGHT             = 19.14
         WIDTH              = 161.8
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
{incl/custdevmode.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* SETTINGS FOR COMBO-BOX ButikkNr IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       ButikkNr:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN sokAvdelingNavn IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN sokHgBeskr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN sokVgBeskr IN FRAME DEFAULT-FRAME
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

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Artikkelsøk */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Artikkelsøk */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* Artikkelsøk */
DO:
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"Resize","").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAvdeling
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAvdeling C-Win
ON CHOOSE OF btnAvdeling IN FRAME DEFAULT-FRAME /* ... */
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

  {&WINDOW-NAME}:MOVE-TO-TOP().

  IF bOk THEN DO:
    IF NUM-ENTRIES(cAvdelingRowidList) > 1 THEN 
      ASSIGN sokAvdelingNr:SCREEN-VALUE   = "0"
             sokAvdelingNavn:SCREEN-VALUE = STRING(NUM-ENTRIES(cAvdelingRowidList)) + " av " +
                                       STRING(iSelectorSourcCount)
                                       .
    ELSE
      ASSIGN sokAvdelingNr:SCREEN-VALUE   = cAvdelingIdList
             sokAvdelingNavn:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","Avdeling;AvdelingNavn","WHERE AvdelingNr = " + sokAvdelingNr:SCREEN-VALUE).
      
    RUN StartQuery.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnHuvGr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnHuvGr C-Win
ON CHOOSE OF btnHuvGr IN FRAME DEFAULT-FRAME /* ... */
DO:

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "HuvGr;Hg;HgBeskr,Avdeling;AvdelingNr;AvdelingNavn",
                      "where true, first Avdeling OF HuvGr NO-LOCK" +
                      (IF cAvdelingRowIdList NE "" THEN
                         " WHERE CAN-DO('" + cAvdelingRowIdList + "',STRING(ROWID(Avdeling)))"
                       ELSE ""),
                      INPUT-OUTPUT cHuvGrRowIdList,
                      "Hg",
                      INPUT-OUTPUT cHuvGrIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  {&WINDOW-NAME}:MOVE-TO-TOP().

  IF bOk THEN DO:
    IF NUM-ENTRIES(cHuvGrRowidList) > 1 THEN 
      ASSIGN sokHg:SCREEN-VALUE   = "0"
             sokHgBeskr:SCREEN-VALUE = STRING(NUM-ENTRIES(cHuvGrRowidList)) + " av " +
                                       STRING(iSelectorSourcCount)
                                       .
    ELSE
      ASSIGN sokHg:SCREEN-VALUE   = cHuvGrIdList
             sokHgBeskr:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","HuvGr;HgBeskr","WHERE Hg = " + sokHg:SCREEN-VALUE).
      
    RUN StartQuery.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLev C-Win
ON CHOOSE OF btnLev IN FRAME DEFAULT-FRAME /* ... */
DO:
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

  {&WINDOW-NAME}:MOVE-TO-TOP().

  IF bOk THEN DO WITH FRAME frmLinje:
    IF NUM-ENTRIES(cLevBasRowidList) > 1 THEN 
      ASSIGN sokLevNr:SCREEN-VALUE   = "0"
             sokLevNamn:SCREEN-VALUE = STRING(NUM-ENTRIES(cLevBasRowidList)) + " av " +
                                       STRING(iSelectorSourcCount)
                                       .
    ELSE
      ASSIGN sokLevNr:SCREEN-VALUE   = cLevBasIdList
             sokLevNamn:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","LevBas;LevNamn","WHERE LevNr = " + sokLevNr:SCREEN-VALUE).
    RUN StartQuery.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnStr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnStr C-Win
ON CHOOSE OF btnStr IN FRAME DEFAULT-FRAME /* ... */
DO:
  DEF VAR cStrIdList    AS CHAR NO-UNDO.
  DEF VAR cStrRowIdList AS CHAR NO-UNDO.

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "StrKonv;Storl",
                      "where true",
                      INPUT-OUTPUT cStrRowIdList,
                      "Storl",
                      INPUT-OUTPUT cStrIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  {&WINDOW-NAME}:MOVE-TO-TOP().

  IF bOk THEN DO:
     sokStr:SCREEN-VALUE   = REPLACE(REPLACE(REPLACE(cStrIdList,"|",",")," ","")," ","").
    RUN StartQuery.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnVarGr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnVarGr C-Win
ON CHOOSE OF btnVarGr IN FRAME DEFAULT-FRAME /* ... */
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

  {&WINDOW-NAME}:MOVE-TO-TOP().

  IF bOk THEN DO:
    IF NUM-ENTRIES(cVarGrRowidList) > 1 THEN 
      ASSIGN sokVG:SCREEN-VALUE      = "0"
             sokVGBeskr:SCREEN-VALUE = STRING(NUM-ENTRIES(cVarGrRowidList)) + " av " +
                                       STRING(iSelectorSourcCount)
                                       .
    ELSE
      ASSIGN sokVG:SCREEN-VALUE   = cVarGrIdList
             sokVGBeskr:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","VarGr;VgBeskr","WHERE Vg = " + sokVG:SCREEN-VALUE).

    RUN StartQuery.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ButikkNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ButikkNr C-Win
ON RETURN OF ButikkNr IN FRAME DEFAULT-FRAME
DO:
  RUN StartQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ButikkNr C-Win
ON VALUE-CHANGED OF ButikkNr IN FRAME DEFAULT-FRAME
DO:
  RUN StartQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiKjoptMnd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiKjoptMnd C-Win
ON RETURN OF fiKjoptMnd IN FRAME DEFAULT-FRAME /* Siste */
DO:  
  RUN StartQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rsButBeholdning
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rsButBeholdning C-Win
ON VALUE-CHANGED OF rsButBeholdning IN FRAME DEFAULT-FRAME
DO:
  ASSIGN rsButBeholdning.
/*   IF rsButBeholdning = 1 THEN                      */
/*     hBrwStr:GET-BROWSE-COLUMN(2):VISIBLE = FALSE.  */
/*   ELSE                                             */
/*     hBrwStr:GET-BROWSE-COLUMN(2):VISIBLE = TRUE.   */
  RUN StartQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rsButTidlKjop
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rsButTidlKjop C-Win
ON VALUE-CHANGED OF rsButTidlKjop IN FRAME DEFAULT-FRAME
DO:
  ASSIGN rsButBeholdning.
  IF rsButBeholdning = 1 THEN 
    hBrwStr:GET-BROWSE-COLUMN(2):VISIBLE = FALSE.
  ELSE
    hBrwStr:GET-BROWSE-COLUMN(2):VISIBLE = TRUE.
  RUN StartQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokArtikkelNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokArtikkelNr C-Win
ON RETURN OF sokArtikkelNr IN FRAME DEFAULT-FRAME /* Art.nr */
DO:
  IF SELF:MODIFIED THEN DO:
    RUN StartQuery.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokAvdelingNavn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokAvdelingNavn C-Win
ON RETURN OF sokAvdelingNavn IN FRAME DEFAULT-FRAME
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
ON F10 OF sokAvdelingNr IN FRAME DEFAULT-FRAME /* Avdeling */
OR "F3" OF sokAvdelingNr DO:
  APPLY "choose" TO btnAvdeling.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokAvdelingNr C-Win
ON RETURN OF sokAvdelingNr IN FRAME DEFAULT-FRAME /* Avdeling */
DO:
  IF sokAvdelingNr:MODIFIED THEN DO:
    sokAvdelingNavn:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","Avdeling;AvdelingNavn","WHERE AvdelingNr = " + sokAvdelingNr:SCREEN-VALUE).
    RUN StartQuery.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokAvdelingNr C-Win
ON TAB OF sokAvdelingNr IN FRAME DEFAULT-FRAME /* Avdeling */
DO:
  IF sokAvdelingNr:MODIFIED THEN 
    sokAvdelingNavn:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","Avdeling;AvdelingNavn","WHERE AvdelingNr = " + sokAvdelingNr:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokBeskr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokBeskr C-Win
ON RETURN OF sokBeskr IN FRAME DEFAULT-FRAME /* Utv.sok */
DO:
  IF SELF:MODIFIED THEN DO:
    RUN StartQuery.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokHg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokHg C-Win
ON F10 OF sokHg IN FRAME DEFAULT-FRAME /* Hovedgr */
OR "F3" OF sokHg DO:
  APPLY "choose" TO btnHuvGr.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokHg C-Win
ON RETURN OF sokHg IN FRAME DEFAULT-FRAME /* Hovedgr */
DO:
  IF sokHg:MODIFIED THEN DO: 
    sokHgBeskr:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","HuvGr;HgBeskr","WHERE Hg = " + sokHg:SCREEN-VALUE).
    RUN StartQuery.
  END.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokHg C-Win
ON TAB OF sokHg IN FRAME DEFAULT-FRAME /* Hovedgr */
DO:
  IF sokHg:MODIFIED THEN
    sokHgBeskr:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","HuvGr;HgBeskr","WHERE Hg = " + sokHg:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokHgBeskr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokHgBeskr C-Win
ON RETURN OF sokHgBeskr IN FRAME DEFAULT-FRAME
DO:
  IF sokHgBeskr:MODIFIED THEN DO:
    ASSIGN cHuvGrRowIdList = ""
           cHuvGrIdList    = ""
           .
    RUN StartQuery.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokLevKod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokLevKod C-Win
ON RETURN OF sokLevKod IN FRAME DEFAULT-FRAME /* Lev.Art.nr */
DO:
  IF SELF:MODIFIED THEN DO:
    RUN StartQuery.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokLevNamn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokLevNamn C-Win
ON RETURN OF sokLevNamn IN FRAME DEFAULT-FRAME
DO:
  IF sokLevNamn:MODIFIED THEN DO: 
    ASSIGN cLevBasRowIdList = ""
           cLevBasIdList    = ""
           .
    RUN StartQuery.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokLevNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokLevNr C-Win
ON F10 OF sokLevNr IN FRAME DEFAULT-FRAME /* Lev */
OR "F3" OF sokLevNr DO:
  APPLY "choose" TO btnLev.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokLevNr C-Win
ON RETURN OF sokLevNr IN FRAME DEFAULT-FRAME /* Lev */
DO:
  IF sokLevNr:MODIFIED THEN DO:
    sokLevNamn:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","LevBas;LevNamn","WHERE LevNr = " + sokLevNr:SCREEN-VALUE).
    RUN StartQuery.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokLevNr C-Win
ON TAB OF sokLevNr IN FRAME DEFAULT-FRAME /* Lev */
DO:  
  IF sokLevNr:MODIFIED THEN 
    sokLevNamn:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","LevBas;LevNamn","WHERE LevNr = " + sokLevNr:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokPris
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokPris C-Win
ON RETURN OF sokPris IN FRAME DEFAULT-FRAME /* Maks pris */
DO:
  IF SELF:MODIFIED THEN DO:
    RUN StartQuery.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokStr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokStr C-Win
ON F10 OF sokStr IN FRAME DEFAULT-FRAME /* Størrelser */
OR "F3" OF sokStr DO:
  APPLY "choose" TO btnStr.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokStr C-Win
ON RETURN OF sokStr IN FRAME DEFAULT-FRAME /* Størrelser */
DO:
  IF SELF:MODIFIED THEN DO:
    RUN StartQuery.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokStrKod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokStrKod C-Win
ON RETURN OF sokStrKod IN FRAME DEFAULT-FRAME /* EAN */
DO:
  IF SELF:MODIFIED AND SELF:SCREEN-VALUE NE "" THEN DO:
    cEAN = sokStrKod:SCREEN-VALUE.
    RUN bibl_chkean.p (INPUT-OUTPUT cEAN).
    sokStrKod:SCREEN-VALUE = cEAN.
    cTekst = DYNAMIC-FUNCTION("getFieldValues","Strekkode", "WHERE Kode = '" + cEAN + "'","ArtikkelNr").
    IF cTekst = '' OR cTekst = ? THEN
    DO:
      MESSAGE 'Ukjent strekkode'
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
    END.

    bEANsok = YES.
    RUN StartQuery.
    bEANsok = NO.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokVg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokVg C-Win
ON F10 OF sokVg IN FRAME DEFAULT-FRAME /* Varegr */
OR "F3" OF sokVg DO:
  APPLY "choose" TO btnVarGr.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokVg C-Win
ON RETURN OF sokVg IN FRAME DEFAULT-FRAME /* Varegr */
DO:
  IF sokVg:MODIFIED THEN DO: 
    sokVgBeskr:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","VarGr;VgBeskr","WHERE Vg = " + sokVg:SCREEN-VALUE).
    RUN StartQuery.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokVg C-Win
ON TAB OF sokVg IN FRAME DEFAULT-FRAME /* Varegr */
DO:
  IF sokVg:MODIFIED THEN
    sokVgBeskr:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","VarGr;VgBeskr","WHERE Vg = " + sokVg:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokVgBeskr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokVgBeskr C-Win
ON RETURN OF sokVgBeskr IN FRAME DEFAULT-FRAME
DO:
  IF sokVgBeskr:MODIFIED THEN DO:
    ASSIGN cVarGrRowIdList = ""
           cVarGrIdList    = ""
           .
    RUN StartQuery.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbLager
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbLager C-Win
ON VALUE-CHANGED OF tbLager IN FRAME DEFAULT-FRAME /* På lager */
DO:
/*   IF cButikkNr = "" THEN DO:                                                                          */
/*     DYNAMIC-FUNCTION("DoMessage",0,0,"Beholdning kan bare sjekkes dersom en butikk er valgt","","").  */
/*     tbLager:CHECKED = FALSE.                                                                          */
/*     RETURN NO-APPLY.                                                                                  */
/*   END.                                                                                                */
  rsButBeholdning:HIDDEN = NOT tbLager:CHECKED.

  APPLY "window-resized" TO {&WINDOW-NAME}.

  IF tbLager:CHECKED OR hBrowse:QUERY:NUM-RESULTS > 0 THEN
    RUN StartQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbOPris
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbOPris C-Win
ON VALUE-CHANGED OF tbOPris IN FRAME DEFAULT-FRAME /* Åpen pris */
DO:
  IF tbOpris:CHECKED THEN
    ASSIGN sokPris:SCREEN-VALUE = "0"
           sokPris:SENSITIVE = FALSE.
  ELSE
    sokPris:SENSITIVE = TRUE.

  IF tbOPris:CHECKED OR hBrowse:QUERY:NUM-RESULTS > 0 THEN
    RUN StartQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbTidlKjop
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbTidlKjop C-Win
ON VALUE-CHANGED OF tbTidlKjop IN FRAME DEFAULT-FRAME /* Tidl.kjøpt */
DO:
/*   IF cButikkNr = "" THEN DO:                                                                          */
/*     DYNAMIC-FUNCTION("DoMessage",0,0,"Beholdning kan bare sjekkes dersom en butikk er valgt","","").  */
/*     tbLager:CHECKED = FALSE.                                                                          */
/*     RETURN NO-APPLY.                                                                                  */
/*   END.                                                                                                */
/*   KundeTrans  */
  ASSIGN rsButTidlKjop:HIDDEN   = NOT tbTidlKjop:CHECKED
         fiKjoptMnd:HIDDEN      = rsButTidlKjop:HIDDEN
         fiKjoptMndLabel:HIDDEN = rsButTidlKjop:HIDDEN
         .

  APPLY "window-resized" TO {&WINDOW-NAME}.

  IF tbTidlKjop:CHECKED OR hBrowse:QUERY:NUM-RESULTS > 0 THEN
    RUN StartQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbTilbud
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbTilbud C-Win
ON VALUE-CHANGED OF tbTilbud IN FRAME DEFAULT-FRAME /* På tilbud */
DO:
  IF tbTilbud:CHECKED OR hBrowse:QUERY:NUM-RESULTS > 0 THEN
    RUN StartQuery.
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
  IF VALID-HANDLE(hVisBilde) THEN APPLY "close" TO hVisBilde.
  IF VALID-HANDLE(hArtikkelkort) THEN APPLY "close" TO hArtikkelkort.
  IF VALID-HANDLE(hArtBilde) THEN APPLY "close" TO hArtBilde.
  DYNAMIC-FUNCTION("setCleanUpResize",THIS-PROCEDURE:CURRENT-WINDOW).
  PUBLISH "InvalidateHandle" (THIS-PROCEDURE).
  RUN disable_UI.
/*   CURRENT-WINDOW = hParent. */
  RUN MoveToTop IN hParent.

END.

{incl/wintrigg.i}

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  RUN enable_UI.
  hParent = SOURCE-PROCEDURE.
  &IF DEFINED(UIB_is_Running) NE 0 &THEN
/*     setHideStr().  */
    RUN InitializeObject.
    setCloseOnSelect(YES).
  &ENDIF

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BackTabFromBrowse C-Win 
PROCEDURE BackTabFromBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowse THEN
  DYNAMIC-FUNCTION("setWidgetEnter",sokBeskr:HANDLE IN FRAME {&FRAME-NAME}).
ELSE
  DYNAMIC-FUNCTION("setWidgetEnter",hBrowse).

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
DEF OUTPUT PARAM obOk     AS LOG    NO-UNDO INIT YES.

DEF VAR hQuery    AS HANDLE NO-UNDO.

bStrAdded = NO.

IF LAST-EVENT:LABEL NE "enter" 
   THEN RETURN.

DYNAMIC-FUNCTION("DoLockWindow",hParent:CURRENT-WINDOW).
DYNAMIC-FUNCTION("setAttribute",SESSION,"userkeepswindowlocked","yes").
DYNAMIC-FUNCTION("setAttribute",ihFillIn,"last-event","enter").

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " where PlukkAntStr > 0").
hQuery:QUERY-OPEN().
hQuery:GET-FIRST().
ix = 0.
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  ix = ix + 1.

  IF ix > 1 AND bCloseOnSelect THEN DO:
    DYNAMIC-FUNCTION("DoMessage",0,0,"I endringsmodus kan bare en størrelse velges","","").
    LEAVE.
  END.
  bStrAdded = YES.

  IF NOT lRunProc THEN
  DO:
      DYNAMIC-FUNCTION("AddStr" IN hParent,
                       ihBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE,
                       ihBuffer:BUFFER-FIELD("Storl"):BUFFER-VALUE,
                       ihBuffer:BUFFER-FIELD("PlukkAntStr"):BUFFER-VALUE,
                       IF bCloseOnSelect THEN "replace" ELSE "add").
  END.
  ELSE DO:  
      RUN AddStrProc IN hParent (ihBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE,
                                 ihBuffer:BUFFER-FIELD("Storl"):BUFFER-VALUE,
                                 ihBuffer:BUFFER-FIELD("PlukkAntStr"):BUFFER-VALUE,
                                 IF bCloseOnSelect THEN "replace" ELSE "add").
  END.
  ASSIGN ihBuffer:BUFFER-FIELD("OrdreAntStr"):BUFFER-VALUE = /*ihBuffer:BUFFER-FIELD("OrdreAntStr"):BUFFER-VALUE
                                                           + */ ihBuffer:BUFFER-FIELD("PlukkAntStr"):BUFFER-VALUE
         ihBuffer:BUFFER-FIELD("PlukkAntStr"):BUFFER-VALUE = 0
         .
      
  DYNAMIC-FUNCTION("RefreshRowids",hBrwStr,ihBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE).

  hQuery:GET-NEXT().
END.

DELETE OBJECT hQuery.
     
DYNAMIC-FUNCTION("RefreshRowids",hBrowse,hBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE).

DYNAMIC-FUNCTION("setAttribute",SESSION,"userkeepswindowlocked","").
DYNAMIC-FUNCTION("DoLockWindow",?).
hParent:CURRENT-WINDOW:SENSITIVE = YES.

IF bCloseOnSelect THEN DO:
  obOk = NO.
  APPLY "close" TO THIS-PROCEDURE.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BlankFilterRecord C-Win 
PROCEDURE BlankFilterRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  ASSIGN sokAvdelingNr:SCREEN-VALUE     = "0"
         sokAvdelingNavn:SCREEN-VALUE   = ""
         sokHg:SCREEN-VALUE             = "0" 
         sokHgBeskr:SCREEN-VALUE        = "" 
         sokVg:SCREEN-VALUE             = "0" 
         sokVgBeskr:SCREEN-VALUE        = ""
         sokLevNr:SCREEN-VALUE          = "0"
         sokLevNamn:SCREEN-VALUE        = ""
         sokBeskr:SCREEN-VALUE          = ""
         sokLevKod:SCREEN-VALUE         = ""
/*          sokAktNr:SCREEN-VALUE          = "0" */
/*          sokAktBeskrivelse:SCREEN-VALUE = ""  */
         sokStrKod:SCREEN-VALUE         = ""
         sokStr:SCREEN-VALUE            = ""
         sokArtikkelNr:SCREEN-VALUE     = "0"
         cHuvGrAvdelingList             = ""
         cVarGrHuvGrList                = ""
         cLevBasRowIdList               = ""
         cLevBasIdList                  = ""
         cAvdelingRowIdList             = ""
         cAvdelingIdList                = ""
         cHuvGrRowIdList                = ""
         cHuvGrIdList                   = ""
         cVarGrRowIdList                = ""
         cVarGrIdList                   = ""
         sokPris:SCREEN-VALUE           = "0"
         tbTilbud:CHECKED               = NO 
         tbLager:CHECKED                = NO 
         tbTidlKjop:CHECKED             = NO
         tbOPris:CHECKED                = NO
         tbTilbud:MODIFIED              = NO 
         tbLager:MODIFIED               = NO 
         tbTidlKjop:MODIFIED            = NO
         tbOPris:MODIFIED               = NO
         .
  APPLY "entry" TO sokBeskr.
END.
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
DEF VAR obOk AS LOG NO-UNDO.
bOK = FALSE.

IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowse THEN DO:
  IF hBrowse = hBrwVpi THEN DO:
    RUN sendVpiTilArtBas (OUTPUT obOk).
    IF NOT obOk THEN RETURN.
    ELSE DO:
      RUN StartQuery.
      RETURN.
    END.
  END.

  IF bUpdateCurrentRow AND VALID-HANDLE(hPaaVarebeh) AND hBuffer:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE = fVarebehNr THEN DO:
    IF DYNAMIC-FUNCTION("DoMessage",0,1,
                        "Artikkel finnes allerede i varebok." + CHR(10) 
                      + "Skal den benyttes som utgangspunkt for registrering av ny vare?",
                        "","") NE 1 THEN RETURN.
  END.

  IF VALID-HANDLE(hBrwStr) AND hBrwStr:QUERY:NUM-RESULTS > 1 AND NOT bCloseOnSelect THEN DO:
    DYNAMIC-FUNCTION("DoMessage",0,0,"Velg en størrelse ved å skrive antall og trykke ENTER","","").
    RETURN.
  END.

  DYNAMIC-FUNCTION("DoLockWindow",THIS-PROCEDURE:CURRENT-WINDOW).
  DYNAMIC-FUNCTION("setAttribute",SESSION,"userKeepsWindowLocked","YES").

  IF VALID-HANDLE(hBrwStr) AND hBrwStr:QUERY:NUM-RESULTS = 1 AND hBuffStr:AVAIL AND NOT bCloseOnSelect THEN DO:
    IF NOT lRunProc THEN 
    DO:
        bOk = DYNAMIC-FUNCTION("AddStr" IN hParent,
                         hBuffStr:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE,
                         hBuffStr:BUFFER-FIELD("Storl"):BUFFER-VALUE,
                         1,"add").
    END.
    ELSE DO:
        RUN AddStrProc IN hParent (hBuffStr:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE,
                                   hBuffStr:BUFFER-FIELD("Storl"):BUFFER-VALUE,
                                   1,"add").
    END.
    APPLY "value-changed" TO hBrowse.
  END.
/*   ELSE IF VALID-HANDLE(hBrwStr) AND NOT bCloseOnSelect THEN DO:                                      */
/*     DYNAMIC-FUNCTION("DoMessage",0,0,"Velg en størrelse ved å skrive antall og trykke ENTER","",""). */
/*     RETURN.                                                                                          */
/*   END.                                                                                               */
  ELSE 
    bOk = DYNAMIC-FUNCTION("AddStr" IN hParent,
                     hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE,
                     "",
                     0,
                     "replace").

  DYNAMIC-FUNCTION("setAttribute",SESSION,"userKeepsWindowLocked","").
  DYNAMIC-FUNCTION("DoLockWindow",?).

  IF bCloseOnSelect THEN 
    APPLY "close" TO THIS-PROCEDURE.
  ELSE DO:
    DYNAMIC-FUNCTION("RefreshRowids",hBrowse,hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("RowIdent1"):BUFFER-VALUE).
    IF bOk THEN APPLY "entry" TO hBrowse.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplayRecord C-Win 
PROCEDURE DisplayRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR hDummy     AS HANDLE NO-UNDO.
DEF VAR iMousePosX AS INT    NO-UNDO.
DEF VAR iMousePosY AS INT    NO-UNDO.

IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowse THEN DO:
  RUN VisMiniBilde IN hArtBilde 
      (IF hBuffer:AVAIL THEN hBuffer:BUFFER-FIELD("BildNr"):BUFFER-VALUE 
       ELSE 0).
    
  IF VALID-HANDLE(hArtikkelkort) THEN
    RUN ByttArtikkel IN hArtikkelkort (IF hBuffer:AVAIL THEN hBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE
                                       ELSE 0).

  PUBLISH "ArtBasSokArtikkelNr" (IF hBuffer:AVAIL THEN hBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE ELSE 0).

  IF VALID-HANDLE(hBuffStr) THEN DO:
    hBuffStr:EMPTY-TEMP-TABLE().
    IF hBuffer:AVAIL THEN DO:
      IF i1ButNr = 0 THEN
          hDummy = DYNAMIC-FUNCTION("getTempTable","artbas_str_beh.p",
                                STRING(hBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) + "," + cButikkNr + "," + rsButBeholdning:SCREEN-VALUE IN FRAME {&FRAME-NAME},
                                hBuffStr).
      ELSE
          hDummy = DYNAMIC-FUNCTION("getTempTable","artbas_str_beh.p",
                                STRING(hBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) + "," + STRING(i1ButNr) + "," + rsButBeholdning:SCREEN-VALUE IN FRAME {&FRAME-NAME},
                                hBuffStr).

      DYNAMIC-FUNCTION("getCurrentAntall" IN hParent,hBuffStr) NO-ERROR.
    END.
  END.
END.
RUN SUPER.

IF DYNAMIC-FUNCTION("getLinkedObject",hBrwStr,"BrowseOverlay","from") NE ? THEN DO:
  iMousePosX = DYNAMIC-FUNCTION("getMousePosition",FRAME {&FRAME-NAME}:HANDLE,"x").
  iMousePosY = DYNAMIC-FUNCTION("getMousePosition",FRAME {&FRAME-NAME}:HANDLE,"y").
  
  IF VALID-HANDLE(hPlukkStrOverlay) AND hPlukkStrOverlay:SENSITIVE AND iMousePosX > hBrwStr:X AND
     iMousePosY > hBrwStr:Y THEN
    DYNAMIC-FUNCTION("setWidgetEnter",hPlukkStrOverlay).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EditRecord C-Win 
PROCEDURE EditRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF NOT VALID-HANDLE(hArtikkelkort) THEN
  RUN w-vartkor.w  PERSIST SET hArtikkelkort (DYNAMIC-FUNCTION("getRecId","ArtBas",hBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE), "ENDRE," + STRING(THIS-PROCEDURE)).
ELSE
  RUN ByttArtikkel IN hArtikkelkort (hBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE).
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
  DISPLAY sokBeskr sokAvdelingNr sokAvdelingNavn sokArtikkelNr sokStrKod sokHg 
          sokHgBeskr sokLevNr sokLevNamn sokVg sokVgBeskr sokLevKod sokStr 
          rsButBeholdning tbLager sokPris fiKjoptMnd tbOPris tbTilbud 
          rsButTidlKjop tbTidlKjop fiKjoptMndLabel 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE BrwArtikkel rectToolBar rectWinToolbar BrwStr ArtikkelBilde BrwVPI 
         sokBeskr sokAvdelingNr btnAvdeling sokArtikkelNr sokStrKod sokHg 
         btnHuvGr sokLevNr sokLevNamn btnLev sokVg btnVarGr sokLevKod sokStr 
         btnStr rsButBeholdning tbLager sokPris fiKjoptMnd tbOPris tbTilbud 
         rsButTidlKjop tbTidlKjop fiKjoptMndLabel 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
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
IF cCurrSelectBuffer = "HuvGr" THEN DO WITH FRAME {&FRAME-NAME}:
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
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeObject C-Win 
PROCEDURE InitializeObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:      InitializeObject is called automatically when using JBoxDynMenu.w 
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  RUN VisMiniBilde.w PERSIST SET hArtBilde.
  RUN InitializeObject IN hArtBilde (ArtikkelBilde:HANDLE).

  ASSIGN cButikkListe   = DYNAMIC-FUNCTION("getAttribute",SESSION,"ButikkListe")
         cBildeKatalog  = DYNAMIC-FUNCTION("getFieldValues","SysPara",
                             "WHERE SysHId = 10 and SysGr = 1 and ParaNr = 2","Parameter1")
         cArtBasJoin    = ",FIRST LevBas OF ArtBas NO-LOCK OUTER-JOIN,FIRST Farg OF ArtBas NO-LOCK OUTER-JOIN"
                        + ",FIRST VarGr OF ArtBas NO-LOCK OUTER-JOIN"
                        + ",FIRST HuvGr OF ArtBas NO-LOCK OUTER-JOIN"
                        + ",FIRST Avdeling OF HuvGr NO-LOCK OUTER-JOIN"
                        + (IF fKOrdreId NE 0 THEN
                            ",FIRST KOrdreLinje WHERE KOrdreLinje.KOrdre_Id = " + STRING(fKOrdreId) + " AND KOrdreLinje.VareNr = STRING(ArtBas.ArtikkelNr) OUTER-JOIN"
                           ELSE "")
                        + (IF fKampId NE 0 THEN
                            ",FIRST KampanjeTilbArtikkel WHERE KampanjeTilbArtikkel.KampId = DEC(" + STRING(fKampId) + ")" 
                            + " AND KampanjeTilbArtikkel.KampTilbId = " + STRING(iKampTilbId)  
                            + " AND KampanjeTilbArtikkel.KampTilbArtId = artbas.artikkelnr OUTER-JOIN"
                           ELSE "")
                        + (IF fVarebehNr NE 0 THEN
                            ",FIRST VarebehLinje WHERE VareBehLinje.VareBehNr = " + STRING(fVarebehNr) + " AND VareBehLinje.ArtikkelNr = ArtBas.ArtikkelNr OUTER-JOIN" 
                           ELSE "")

         cArtBasFields  = "ArtBas"
                          + ";Beskr|Beskrivelse|x(40)"
                          + ";ArtikkelNr|SE Art.nr"
                          + ";LevKod|Lev.Art.nr"
                          + ";LevFargKod|Lev.farge"
                          + ";+InnPris|DECIMAL|>><>>><>>9.99|artpris_innpris_2(ROWID)|Inn.Pris"
                          + ";+Pris|DECIMAL|>><>>><>>9.99|artpris_pris_2(ROWID)|Pris"
                          + ";AnbefaltPris|Anb.pris"
                          + ";+ArtStrBeh|INTEGER|->>>>9|artbas_sjekk_strbeh(ROWID)|Beh"
                          + ";!+TilbPris|LOGICAL|*/|art_paa_tilbud(ROWID)|Tilb"
                          + ";!+TidlKjop|LOGICAL|*/|art_tidl_kjopt(ROWID)|Tidl.kjop"
                          + ";!BildNr"
                          + ";Levnr|Lev.nr"
                          + ";!Utvidetsok"
                        + ",LevBas"
                          + ";LevNamn|"
                        + ",Farg"
                          + ";FarBeskr|Farge@5"
                        + ",VarGr"
                          + ";Vg"
                          + ";VgBeskr"
                        + ",HuvGr"
                          + ";Hg"
                          + ";HgBeskr"
                        + ",Avdeling"
                          + ";AvdelingNr"
                          + ";AvdelingNavn"
                        + (IF fKOrdreId NE 0 THEN 
                            ",KOrdreLinje;!KOrdre_Id"
                           ELSE "")
                        + (IF fKampId NE 0 THEN 
                            ",KampanjeTilbArtikkel;!KampId;!KampTilbId;!KampTilbArtId"
                           ELSE "")                      
                        + (IF fVarebehNr NE 0 THEN 
                            ",VareBehLinje;!VarebehNr"
                           ELSE "")                      

         cButikkNr      = IF cButikkNr = "" THEN DYNAMIC-FUNCTION("getFieldValues","Bruker","WHERE BrukerId = '" + DYNAMIC-FUNCTION("getASuserId") + "'","ButikkNr") ELSE cButikkNr
         rsButBeholdning:HIDDEN = YES
         rsButTidlKjop:HIDDEN   = YES
         fiKjoptMnd:HIDDEN      = YES
         fiKjoptMndLabel:HIDDEN = YES
         .

  IF cButikkListe = "" THEN cButikkListe = "*".
  IF cButikkNr = ? THEN cButikkNr = "".

/*   ASSIGN ButikkNr:DELIMITER = "|"                                                                                                                                                                                           */
/*          ButikkNr:LIST-ITEM-PAIRS = RIGHT-TRIM("Alle butikker|0|Butikkfordeling|-1|" + DYNAMIC-FUNCTION("GetFieldList","Butiker;Butik|ButNamn;Butik","WHERE CAN-DO('" + cButikkListe + "',STRING(Butik)) BY ButNamn"),"|")  */
/*          ButikkNr:SCREEN-VALUE = ButikkNr:ENTRY(1)                                                                                                                                                                          */
/*          ButikkNr:HIDDEN = TRUE                                                                                                                                                                                             */
/*          .                                                                                                                                                                                                                  */

/*   IF bCloseOnSelect THEN BrwArtikkel:WIDTH-PIXELS = BrwArtikkel:WIDTH-PIXELS + 200.  */

  IF bHideStr THEN 
    ASSIGN brwArtikkel:WIDTH-CHARS = 160
           brwStr:HIDDEN = YES.

  hBrowse = DYNAMIC-FUNCTION("NewBrowse",
                    BrwArtikkel:HANDLE,
                    50,
                    "TITLE|Artikkelregister",
                    cArtBasFields,
                    "WHERE false" + cArtBasJoin
                    ,"sort|Beskr").   
  ASSIGN hBuffer      = hBrowse:QUERY:GET-BUFFER-HANDLE(1)
         hBrwArtikkel = hBrowse
         hBuffArtBas  = hBuffer.
  DYNAMIC-FUNCTION("NewMenuBand",hBrowse
          ,"MultiSortBrowse;Sorter på flere kolonner"
          ,"").

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcfieldproc","artbassok_brwcalc.p").
  
  hTilbField     = hBuffer:BUFFER-FIELD("TilbPris").
  hPaaOrdreField = hBuffer:BUFFER-FIELD("KOrdre_Id") NO-ERROR.
  hKampTilbArtId = hBuffer:BUFFER-FIELD("KampTilbArtId") NO-ERROR.
  hPaaVarebeh    = hBuffer:BUFFER-FIELD("VarebehNr") NO-ERROR.
  hArtNr         = hBuffer:BUFFER-FIELD("ArtikkelNr") NO-ERROR.

/*   DYNAMIC-FUNCTION("setNoColumnSort",hBrowse,"Pris,ArtStrBeh").  */
/*   DYNAMIC-FUNCTION("setAttribute",hBrowse,"getrecordcount","yes"). */

  hBrwVpi = DYNAMIC-FUNCTION("NewBrowse",
                    BrwVPI:HANDLE,
                    50,
                    "MULTIPLE,TITLE|VPI register - dobbeltklikk/høyreklikk for å overføre artikkel/artikler til artikkelregister",
                    "VPIArtBas;!EkstVPILevNr;!varenr" 
                    + ";!+ArtbasExists|LOGICAL|J/N|chkArtReg(ROWIDno)"
                    + SUBSTR(cArtBasFields,7),
                    "WHERE false" + REPLACE(cArtBasJoin,"ArtBas","VPIArtBas")
                    ,"sort|Beskr").   
  ASSIGN hBuffVPI         = hBrwVPI:QUERY:GET-BUFFER-HANDLE(1)
         hBrwVPI:HIDDEN   = YES
         hFldArtBasExists = hBuffVPI:BUFFER-FIELD("ArtbasExists")
         hBrwColVpiArtNr  = DYNAMIC-FUNCTION("getBrowseColumn",hBrwVpi,"artikkelnr").
         .

  DYNAMIC-FUNCTION("setAttribute",hBrwVpi,"calcFieldProc","vpiartbas_artbas_brwcalc.p").

  DYNAMIC-FUNCTION("NewMenuBand",hBrwVpi
        ,"MultiSortBrowse;Sorter på flere kolonner"
       + ",OverforArtBas;Overfør til artikkelregister"
        ,"").

  IF NOT bHideStr THEN DO:

    hBrwStr = DYNAMIC-FUNCTION("NewBrowse",
                      BrwStr:HANDLE,
                      50,
                      "",
                      "temp-table"
                      + ";+Storl|CHARACTER|x(5)||Str"
                      + ";+Lagant|DECIMAL|->><>>9||Lager"
                      + ";+PlukkAntStr|DECIMAL|->><>>9|0|Velg ant"
                      + ";+OrdreAntStr|DECIMAL|->><>>9|0|Ordr.ant"
                      + ";!+ButikkNr|INTEGER|>>>9||But"
                      + ";!+ArtikkelNr|DECIMAL|>>>>>>>>>>>>9"
                      + ";!+StrKode|INTEGER|>>9"
                      + ";!+SeqNr|INTEGER|>>9"
                     ,"WHERE false"
                     ,"SeqNr").
  
    ASSIGN hBrwStr:GET-BROWSE-COLUMN(1):WIDTH-PIXELS = 40
           hBrwStr:GET-BROWSE-COLUMN(2):WIDTH-PIXELS = 40
           hBrwStr:GET-BROWSE-COLUMN(3):WIDTH-PIXELS = 40
           .
  
    hBuffStr       = hBrwStr:QUERY:GET-BUFFER-HANDLE(1).
    DYNAMIC-FUNCTION("setAttribute",hBrwStr,"uselocaldata","yes").
    DYNAMIC-FUNCTION("CreateParentLink",hBrwStr,hBrowse,"ArtikkelNr").
  
    hPlukkStrOverlay = DYNAMIC-FUNCTION("NewBrowseFillIn",
                      hBrwStr,
                      "PlukkAntStr",
                      "PlukkAntStr",
                      "","","", 
                      "").
    DYNAMIC-FUNCTION("CreateOverlayLink",hBrwStr,hPlukkStrOverlay,"PlukkAntStr").   
  END.

/*   IF bCloseOnSelect THEN           */
/*     ASSIGN hBrwStr:HIDDEN = YES    */
/*            hBrwStr:TAB-STOP = NO.  */

  hToolbar = DYNAMIC-FUNCTION("NewToolBar",
                    rectToolBar:HANDLE,             
                    "Fil",                         
                    "BrowseConfig,Excel;Eksporter til E&xcel,Edit"
                    + ",BlankFilter;&Blank filter¤enable"
                    + ",SokVPI;Søk VPI register¤enable"
                    ,"maxborder").  
  
  DYNAMIC-FUNCTION("setAttribute",hToolbar,"enabledevents","BlankFilter").

  hBtnSokVpi = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hToolbar,"buttonSokVPI")).
  hBtnSokVpi:WIDTH-PIXELS = hBtnSokVpi:WIDTH-PIXELS + 10.

  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hToolbar).

  DYNAMIC-FUNCTION("NewToolBar",
                    rectWinToolBar:HANDLE, 
                    "Fil",                
                    "close;Avslutt",
                    "right,enable").

  SUBSCRIBE TO "InvalidateHandle" ANYWHERE.
END.

DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, 
                 "IMAGE-Sko,BrwStr" + (IF VALID-HANDLE(hBrwStr) THEN "," + hBrwStr:NAME ELSE "")).
DYNAMIC-FUNCTION("setNoMoveX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "fiKjoptMnd,fiKjoptMndLabel").
DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "rectToolBar,IMAGE-Sko").
DYNAMIC-FUNCTION("setOrgWinSize", THIS-PROCEDURE:CURRENT-WINDOW,600,250,0,250).

DYNAMIC-FUNCTION("initTranslation",THIS-PROCEDURE:CURRENT-WINDOW).

THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = FALSE.

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
DEF INPUT PARAM ihParent AS HANDLE NO-UNDO.

IF NOT VALID-HANDLE(ihParent) OR ihParent:FILE-NAME NE "KundeOrdre.w" THEN LEAVE.

APPLY "close" TO THIS-PROCEDURE.
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
RUN SUPER.
IF sokStrKod:SCREEN-VALUE IN FRAME {&FRAME-NAME} NE "" AND bStrAdded THEN 
  DYNAMIC-FUNCTION("setWidgetEnter",sokStrKod:HANDLE).
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
APPLY "entry" TO sokBeskr IN FRAME {&FRAME-NAME}.
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
DEF VAR hCurrObj AS HANDLE NO-UNDO.

DYNAMIC-FUNCTION("DoLockWindow",THIS-PROCEDURE:CURRENT-WINDOW).
DYNAMIC-FUNCTION("setAttribute",SESSION,"userKeepsWindowLocked","YES").

hCurrObj = DYNAMIC-FUNCTION("getCurrentObject").
IF hCurrObj = hBrwArtikkel OR hCurrObj = hBrwVpi THEN
  RUN OpenQueryArtOrVpi.

RUN SUPER.

DYNAMIC-FUNCTION("setAttribute",SESSION,"userKeepsWindowLocked","").
DYNAMIC-FUNCTION("DoLockWindow",?).

IF hCurrObj = hBrwArtikkel AND (hBrowse:QUERY:NUM-RESULTS = 0 OR hBrowse:QUERY:NUM-RESULTS = ?) THEN DO:
  IF DYNAMIC-FUNCTION("DoMessage",0,4,"Finner ingen treff i artikkelregister" + CHR(10)
                                    + "Søk i VPI register?","","") = 6 THEN DO:
                                        
    bVPIsearch = YES.
    RUN StartQuery.
    bVPIsearch = NO.
  END. 
END.

DYNAMIC-FUNCTION("setNoColumnSort",hBrowse,"").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenQueryArtOrVpi C-Win 
PROCEDURE OpenQueryArtOrVpi :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* DEF INPUT PARAM ihBrowse AS HANDLE NO-UNDO.  */
/*                                              */
/* MESSAGE "ihBrowse:NAME" ihBrowse:NAME SKIP   */
/*         "hBrowse:NAME" hBrowse:NAME          */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK.       */
/*                                              */
/* IF hBrowse NE ihBrowse THEN                  */
/*   ASSIGN hBrowse:HIDDEN = YES                */
/*          hBrowse = ihBrowse                  */
/*          hBrowse:HIDDEN = NO                 */
/*          . */

DO WITH FRAME {&FRAME-NAME}:

  ASSIGN sokPris rsButBeholdning
         sokAvdelingNr:MODIFIED   = FALSE
         sokAvdelingNavn:MODIFIED = FALSE
         sokHg:MODIFIED           = FALSE
         sokHgBeskr:MODIFIED      = FALSE
         sokVg:MODIFIED           = FALSE
         sokVgBeskr:MODIFIED      = FALSE
/*          sokAktNr:MODIFIED        = FALSE   */
/*          sokAktBeskrivelse:MODIFIED = FALSE */
         sokBeskr:MODIFIED        = FALSE
         sokStrKod:MODIFIED       = FALSE
         cArtNrStrKode            = ""
         .
  cEAN = sokStrKod:SCREEN-VALUE.
  IF cEAN NE "" THEN DO:
    RUN bibl_chkean.p (INPUT-OUTPUT cEAN).
    sokStrKod:SCREEN-VALUE = cEAN.

    cArtNrStrKode = DYNAMIC-FUNCTION("getFieldList","Strekkode;ArtikkelNr,StrKonv;Storl",
                                      "WHERE Strekkode.kode = '" + cEAN + "'" +
                                      ",FIRST StrKonv OF Strekkode NO-LOCK").
    IF cArtNrStrKode NE "" THEN DO:
      DYNAMIC-FUNCTION("setAttribute",hBrowse,"basequery","WHERE ArtikkelNr = " + ENTRY(1,cArtNrStrKode,"|")).
      cArtNrStrKode = TRIM(ENTRY(2,cArtNrStrKode,"|")).
    END.
    ELSE RETURN.
  END.
  ELSE DYNAMIC-FUNCTION("setAttribute",hBrowse,"basequery","WHERE true").

  IF sokLevNamn:SCREEN-VALUE NE "" AND cLevBasRowIdList = "" THEN 
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"prescanqueryfilter",
                     "LevBas WHERE Levnamn " +
                     (IF INDEX(sokLevNamn:SCREEN-VALUE,"*") > 0 THEN 
                        "MATCHES '"
                      ELSE "BEGINS '") + sokLevNamn:SCREEN-VALUE
                   + (IF hBrowse = hBrwArtikkel THEN
                        "',EACH ArtBas OF LevBas NO-LOCK"
                      ELSE 
                        "',EACH VPIArtBas OF LevBas NO-LOCK")
                       ).
  ELSE
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"prescanqueryfilter","").

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"queryfilter","").

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"QueryFilter",
                   (IF sokHg:SCREEN-VALUE NE "0" THEN
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
/*                     ELSE IF sokLevNamn:SCREEN-VALUE NE "" AND NUM-ENTRIES(cLevBasRowIdList) < 2 THEN */
/*                       " AND " +                                                                      */
/*                      (IF INDEX(sokLevNamn:SCREEN-VALUE,"*") > 0 THEN                                 */
/*                        "LevNamn MATCHES '" + sokLevNamn:SCREEN-VALUE + "*'"                          */
/*                       ELSE                                                                           */
/*                        "LevNamn BEGINS '" + sokLevNamn:SCREEN-VALUE + "'")                           */
                    ELSE IF sokLevNamn:SCREEN-VALUE NE "" AND NUM-ENTRIES(cLevBasRowIdList) > 1 THEN
                      " AND CAN-DO('" + REPLACE(cLevBasIdList,"|",",") + "',STRING(LevNr))"
                    ELSE "")
                 + (IF sokArtikkelNr:SCREEN-VALUE NE "0" THEN
                     " AND ArtikkelNr = " + sokArtikkelNr:SCREEN-VALUE
                    ELSE "")
                 + (IF tbOPris:CHECKED THEN
                     " AND OPris = yes"
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
                    ).

/*   IF sokPris NE 0 THEN                                                                         */
/*     cCurrPrisJoin = ",FIRST ArtPris OF ArtBas NO-LOCK WHERE Pris LE " + sokPris:SCREEN-VALUE.  */
/*   ELSE cCurrPrisJoin = cPrisJoin.                                                              */


  DYNAMIC-FUNCTION("setAttribute",hBrowse,"getRecordCount",
                   IF DYNAMIC-FUNCTION("getAttribute",hBrowse,"queryfilter") = "" THEN "" ELSE "YES").

  IF DYNAMIC-FUNCTION("getAttribute",hBrowse,"queryfilter") = "" AND 
     sokAvdelingNr:SCREEN-VALUE = "0" AND
     cEAN = "" AND
     (tbTilbud:MODIFIED OR tbLager:MODIFIED OR tbTidlKjop:MODIFIED OR sokStr:MODIFIED)
    THEN DO:
    IF DYNAMIC-FUNCTION("DoMessage",0,1,"Søket kan ta lang tid. Vil fortsette?","","") = 2 THEN DO:
      IF tbTilbud:MODIFIED   THEN 
        ASSIGN tbTilbud:CHECKED = NO 
                 tbTilbud:MODIFIED = NO.
      IF tbLager:MODIFIED    THEN 
        ASSIGN tbLager:CHECKED = NO 
               tbLager:MODIFIED = NO
               rsButBeholdning:HIDDEN = YES.
      IF tbTidlKjop:MODIFIED THEN 
        ASSIGN tbTidlKjop:CHECKED = NO 
               tbTidlKjop:MODIFIED = NO
               rsButTidlKjop:HIDDEN = YES
               fiKjoptMnd:HIDDEN = YES
               fiKjoptMndLabel:HIDDEN = YES.
      IF sokStr:MODIFIED THEN
        sokStr:MODIFIED = NO.
      APPLY "window-resized" TO {&WINDOW-NAME}.
      RETURN.
    END.
  END.

  IF sokAvdelingNr:SCREEN-VALUE NE "0" THEN
    DYNAMIC-FUNCTION("ChangePrimarySearchBuffer",hBrowse,
                     "Avdeling",
                     "WHERE AvdelingNr = " + sokAvdelingNr:SCREEN-VALUE
                   + ",EACH HuvGr OF Avdeling NO-LOCK"
                   + (IF hBrowse = hBrwArtikkel THEN
                       ",EACH ArtBas OF HuvGr NO-LOCK"
                      ELSE
                        ",EACH VPIArtBas OF HuvGr NO-LOCK")
                     ).
   ELSE IF sokAvdelingNavn:SCREEN-VALUE NE "" AND NUM-ENTRIES(cAvdelingRowIdList) > 1 THEN
     DYNAMIC-FUNCTION("ChangePrimarySearchBuffer",hBrowse,
                      "Avdeling",
                      " WHERE CAN-DO('" + REPLACE(cAvdelingIdList,"|",",") + "',STRING(AvdelingNr))"
                    + ",EACH HuvGr OF Avdeling NO-LOCK"
                    + (IF hBrowse = hBrwArtikkel THEN
                        ",EACH ArtBas OF HuvGr NO-LOCK"
                       ELSE
                        ",EACH VPIArtBas OF HuvGr NO-LOCK")
                      ).
   ELSE
     DYNAMIC-FUNCTION("ChangePrimarySearchBuffer",hBrowse,"","").

/*   IF sokAktNr:SCREEN-VALUE NE "0" OR sokAktBeskrivelse:SCREEN-VALUE NE "" THEN                                      */
/*     DYNAMIC-FUNCTION("setAttribute",hBrowse,"buffersandfields",cArtBasFields + ",VgAkt,Aktivitet").            */
/*   ELSE                                                                                                              */
/*     DYNAMIC-FUNCTION("setAttribute",hBrowse,"buffersandfields",cArtBasFields).                                 */
/*                                                                                                                     */
/*   IF sokAktNr:SCREEN-VALUE NE "0" THEN                                                                              */
/*     DYNAMIC-FUNCTION("setAttribute",hBrowse,"queryjoin",cArtBasJoin + cCurrPrisJoin                            */
/*                                   + ",first VgAkt WHERE VgAkt.Vg = ArtBas.Vg AND AktNr = " + sokAktNr:SCREEN-VALUE  */
/*                                   + ",first Aktivitet OF VgAkt").                                                   */
/*   ELSE                                                                                                              */
/*     DYNAMIC-FUNCTION("setAttribute",hBrowse,"queryjoin",cArtBasJoin + cCurrPrisJoin).                          */

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcparamArtStrBeh",
                   REPLACE(SokStr:SCREEN-VALUE,",","&") + "¤" + tbLager:INPUT-VALUE + "¤" + 
                   (IF rsButBeholdning = 1 THEN cButikkNr ELSE "")).

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcparamTilbPris",cButikkNr + "¤" + tbTilbud:INPUT-VALUE).
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcparamTidlKjop",cButikkNr + "¤" + tbTidlKjop:INPUT-VALUE + "¤" + fiKjoptMnd:INPUT-VALUE + "¤" + STRING(fKundeNr)).
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcparamPris",cButikkNr + "¤" + SokPris:SCREEN-VALUE).
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcparamInnPris",cButikkNr).

/*   IF rsBestilling > 0 AND ButikkNr:SCREEN-VALUE NE "0" AND ButikkNr:SCREEN-VALUE NE ? THEN                                        */
/*     DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcparamBBestilt",ButikkNr:SCREEN-VALUE + "¤" + rsBestilling:SCREEN-VALUE).    */
/*   ELSE IF ButikkNr:SCREEN-VALUE NE "0" AND ButikkNr:SCREEN-VALUE NE ? THEN                                                        */
/*     DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcparamBBestilt",ButikkNr:SCREEN-VALUE).                                      */
/*                                                                                                                                   */
/*                                                                                                                                   */
/*   DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcparamBMatchMerknad",REPLACE(sokLinjeMerknad:SCREEN-VALUE,",","¤")).           */

  ASSIGN tbTilbud:MODIFIED   = NO 
         tbLager:MODIFIED    = NO 
         tbTidlKjop:MODIFIED = NO
         sokPris:MODIFIED    = NO
         sokStr:MODIFIED     = NO
         .
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OverforArtBasRecord C-Win 
PROCEDURE OverforArtBasRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR obOk AS LOG NO-UNDO.
RUN sendVpiTilArtBas (OUTPUT obOk).
IF obOk THEN
  RUN StartQuery.

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
  hBrowse:SELECT-FOCUSED-ROW().
  CASE cRettning:
      WHEN "Prev" THEN
          hBrowse:SELECT-PREV-ROW().
      WHEN "Next" THEN
        hBrowse:SELECT-NEXT-ROW().
  END CASE.
  APPLY "value-changed" TO hBrowse.
END.
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
IF DYNAMIC-FUNCTION("getCurrentObject") = hBrwArtikkel THEN DO: 
  IF hTilbField:BUFFER-VALUE THEN
    hPrisColumn:BGCOLOR = 12.
  IF VALID-HANDLE(hPaaOrdreField) AND hPaaOrdreField:BUFFER-VALUE = fKOrdreId THEN
    hArtNrColumn:BGCOLOR = 10.
  IF VALID-HANDLE(hKampTilbArtId) AND hKampTilbArtId:BUFFER-VALUE = hArtNr:BUFFER-VALUE THEN
    hArtNrColumn:BGCOLOR = 10.
  IF VALID-HANDLE(hPaaVarebeh) AND hPaaVarebeh:BUFFER-VALUE = fVarebehNr THEN
    hArtNrColumn:BGCOLOR = 10.
END.
ELSE IF DYNAMIC-FUNCTION("getCurrentObject") = hBrwVpi THEN DO: 
  IF NOT hFldArtBasExists:BUFFER-VALUE THEN
    hBrwColVpiArtNr:BGCOLOR = 17.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sendVpiTilArtBas C-Win 
PROCEDURE sendVpiTilArtBas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF OUTPUT PARAM obOk AS LOG NO-UNDO INIT YES.

/*
DEF VAR opcFieldList AS CHAR NO-UNDO.
DEF VAR opcLabelList AS CHAR NO-UNDO.
DEF VAR opiReturn    AS INT  NO-UNDO.

DEF VAR iReturn    AS INT NO-UNDO.
DEF VAR cVareNr AS CHAR NO-UNDO.

RUN velgFelterArtBas_artbas_artpris.w (OUTPUT opcFieldList, OUTPUT opcLabelList, OUTPUT opiReturn).
IF opiReturn = 0 THEN DO:
  obOk = NO.  
  RETURN.
END.


IF opcFieldList = '' THEN DO:
  MESSAGE 'Ingen felter valgt, avbryter overføring...' 
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
  obOk = NO.
  RETURN.
END.
*/

fReposArtnr = hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE.

obOk = DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowse,"artbas_new_update.p"
                       ,STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("EkstVPILevNr"):BUFFER-VALUE) 
                        + ";" + {tbchooseAll.i}
                       ).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setLager C-Win 
PROCEDURE setLager :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piButNr AS INT NO-UNDO.

  ASSIGN
      i1ButNr = piButNr.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setRunProc C-Win 
PROCEDURE setRunProc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER ilRunProc AS LOG NO-UNDO.

  ASSIGN
      lRunProc = ilRunProc.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE settArtikkelNr C-Win 
PROCEDURE settArtikkelNr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER cTekst AS CHAR NO-UNDO.

  DO WITH FRAME DEFAULT-FRAME:
      ASSIGN
          sokArtikkelNr:SCREEN-VALUE = cTekst.
      RUN MoveToTop.
      APPLY 'RETURN' TO sokArtikkelNr. 
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SokVPIRecord C-Win 
PROCEDURE SokVPIRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF hBtnSokVpi:FONT = 6 THEN
  bVPIsearch = NO.
ELSE
  bVPIsearch = YES.
RUN StartQuery.
bVPIsearch = NO.
APPLY "window-resized" TO {&WINDOW-NAME}.

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
hBrowse:HIDDEN = YES.
         
IF bVPIsearch THEN DO:
  ASSIGN hBrowse    = hBrwVPI
         hBuffer    = hBuffVPI
         hPlukkStrOverlay:HIDDEN = YES
         hBtnSokVpi:FONT = 6
         hBtnSokVpi:LABEL = "Søk Art.register"
         .
  DYNAMIC-FUNCTION("DeleteObjectLink",hBrwStr,hPlukkStrOverlay).  

END.
ELSE DO:
  ASSIGN hBrowse = hBrwArtikkel
         hBuffer = hBuffArtBas
         hBtnSokVpi:LABEL = "Søk VPI register"
         hBtnSokVpi:FONT = ?
         .
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrwStr,hPlukkStrOverlay,"PlukkAntStr").   
END.

hBrowse:HIDDEN = NO.

IF NOT bHideStr THEN
  DYNAMIC-FUNCTION("CreateParentLink",hBrwStr,hBrowse,"ArtikkelNr").

DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
RUN OpenQuery.

IF bEANsok AND hBuffer:AVAIL THEN DO:
  bOk = hBuffStr:FIND-FIRST("WHERE Storl = '" + cArtNrStrKode + "'") NO-ERROR.
  IF bOk THEN DO:
    hBrwStr:QUERY:REPOSITION-TO-ROWID(hBuffStr:ROWID) NO-ERROR.
    IF NOT ERROR-STATUS:ERROR THEN DO:
      APPLY "value-changed" TO hBrwStr.
      hPlukkStrOverlay:SCREEN-VALUE = "1".
      APPLY "entry" TO hPlukkStrOverlay.
    END.
  END.
END.
ELSE IF NOT bVPIsearch AND fReposArtnr NE 0 THEN DO:
  IF hBuffer:FIND-FIRST("WHERE ArtikkelNr = " + STRING(fReposArtnr)) THEN DO:
    hBrowse:QUERY:REPOSITION-TO-ROWID(hBuffer:ROWID) NO-ERROR.
    IF NOT ERROR-STATUS:ERROR THEN
      RUN InvokeMethod(hBrowse,"DisplayRecord").
  END.
END.
ELSE IF hBrowse:QUERY:NUM-RESULTS > 0 THEN
  APPLY "ENTRY" TO hBrowse.

fReposArtnr = 0.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TabFromBrowse C-Win 
PROCEDURE TabFromBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowse THEN
  DYNAMIC-FUNCTION("setWidgetEnter",hBrwStr).
ELSE IF hBuffStr:AVAIL AND hBrwStr:VISIBLE THEN
  DYNAMIC-FUNCTION("setWidgetEnter",hPlukkStrOverlay).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AdjustBrowseColumns C-Win 
FUNCTION AdjustBrowseColumns RETURNS LOGICAL
  ( INPUT ihBrowse     AS HANDLE,
    INPUT icBrowseName AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF icBrowseName BEGINS "BrwArtikkel" THEN
  ASSIGN ihBrowse:GET-BROWSE-COLUMN(2):WIDTH-PIXELS  = 55
         ihBrowse:GET-BROWSE-COLUMN(3):WIDTH-PIXELS  = 55
         ihBrowse:GET-BROWSE-COLUMN(4):WIDTH-PIXELS  = 55
         ihBrowse:GET-BROWSE-COLUMN(5):WIDTH-PIXELS  = 55
         ihBrowse:GET-BROWSE-COLUMN(6):WIDTH-PIXELS  = 55
         ihBrowse:GET-BROWSE-COLUMN(7):WIDTH-PIXELS  = 55
         ihBrowse:GET-BROWSE-COLUMN(8):WIDTH-PIXELS  = 30
         ihBrowse:GET-BROWSE-COLUMN(9):WIDTH-PIXELS  = 30
         ihBrowse:GET-BROWSE-COLUMN(10):WIDTH-PIXELS = 55
         hArtNrColumn = ihBrowse:GET-BROWSE-COLUMN(1)
         hPrisColumn  = ihBrowse:GET-BROWSE-COLUMN(6)
         .

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getUpdateCurrentRow C-Win 
FUNCTION getUpdateCurrentRow RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose: Variabelen benyttes til å bestemme om en artikkel skal legges til eller
           om gjeldende rad (kordrelinje) skal endres
    Notes:  
------------------------------------------------------------------------------*/

RETURN bUpdateCurrentRow.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION HideVPIfieldSelect C-Win 
FUNCTION HideVPIfieldSelect RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setAvdNr C-Win 
FUNCTION setAvdNr RETURNS LOGICAL
  ( INPUT icAvdelingNr AS CHAR,
    INPUT ibOpenQuery  AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  sokAvdelingNr:SCREEN-VALUE = icAvdelingNr.
  IF ibOpenQuery THEN DO:      
    ASSIGN sokAvdelingNr:SENSITIVE   = NO
           sokAvdelingNavn:SENSITIVE = NO
           btnAvdeling:SENSITIVE     = NO.
    APPLY "return" TO sokAvdelingNr.
  END.
  ELSE APPLY "tab" TO sokAvdelingNr.
END.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setBeskr C-Win 
FUNCTION setBeskr RETURNS LOGICAL
  ( INPUT icBeskr      AS CHAR,
    INPUT ibOpenQuery  AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  RUN BlankFilterRecord.
  sokBeskr:SCREEN-VALUE = icBeskr.
  IF ibOpenQuery THEN DO:      
/*     ASSIGN sokBeskr:SENSITIVE   = NO       */
/*            sokAvdelingNavn:SENSITIVE = NO  */
/*            btnAvdeling:SENSITIVE     = NO. */
    APPLY "return" TO sokBeskr.
  END.
  ELSE APPLY "tab" TO sokBeskr.
END.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setButikkNr C-Win 
FUNCTION setButikkNr RETURNS LOGICAL
  ( INPUT icButikkNr AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
cButikkNr = icButikkNr.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setCloseOnSelect C-Win 
FUNCTION setCloseOnSelect RETURNS LOGICAL
  ( INPUT ibCloseOnSelect AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
bCloseOnSelect = ibCloseOnSelect.

IF bCloseOnSelect THEN DO:

END.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setHg C-Win 
FUNCTION setHg RETURNS LOGICAL
  ( INPUT icHg     AS CHAR,
    INPUT ibOpenQuery AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  sokHg:SCREEN-VALUE = icHg.
  IF ibOpenQuery THEN DO:      
    ASSIGN sokHg:SENSITIVE   = NO
           sokHgBeskr:SENSITIVE = NO
           btnHuvGr:SENSITIVE     = NO.
    APPLY "return" TO sokHg.
  END.
  ELSE APPLY "tab" TO sokHg.
END.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setHideStr C-Win 
FUNCTION setHideStr RETURNS LOGICAL
  ( ) :
/*------------------------------------------------------------------------------
  Purpose:  Kalles før initializeObject for å hindre at størrelser vises
    Notes:  
------------------------------------------------------------------------------*/

bHideStr = YES.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setKampanjeInfo C-Win 
FUNCTION setKampanjeInfo RETURNS LOGICAL
  ( INPUT ifKampId AS DEC,
    INPUT iiKampTilbId AS INT) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  ASSIGN 
    fKampId = ifKampId
    iKampTilbId = iiKampTilbId
  .
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SetLevKodQuery C-Win 
FUNCTION SetLevKodQuery RETURNS HANDLE
  ( INPUT icLevKod AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
RUN BlankFilterRecord.
sokLevKod:SCREEN-VALUE IN FRAME {&FRAME-NAME} = icLevKod.
DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
RUN OpenQuery.
IF hBrowse:QUERY:NUM-RESULTS = 0 THEN 
  RETURN sokLevKod:HANDLE.
ELSE
  RETURN hBrowse.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setLevnr C-Win 
FUNCTION setLevnr RETURNS LOGICAL
  ( INPUT icLevnr     AS CHAR,
    INPUT ibOpenQuery AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  sokLevNr:SCREEN-VALUE = icLevnr.
  IF ibOpenQuery THEN DO:      
    ASSIGN sokLevNr:SENSITIVE   = NO
           sokLevNamn:SENSITIVE = NO
           btnLev:SENSITIVE     = NO.
    APPLY "return" TO sokLevNr.
  END.
  ELSE APPLY "tab" TO sokLevNr.
END.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setOrdreId C-Win 
FUNCTION setOrdreId RETURNS LOGICAL
  ( INPUT ifKOrdreId AS DEC ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
fKOrdreId = ifKOrdreId.

fKundeNr = DEC(DYNAMIC-FUNCTION("getFieldValues","KOrdreHode","WHERE KOrdre_id = " + STRING(fKOrdreId),"KundeNr")).

IF cArtBasJoin MATCHES "*KOrdreLinje*" THEN
  cArtBasJoin    = ",FIRST LevBas OF ArtBas NO-LOCK" +
                   ",FIRST KOrdreLinje WHERE KOrdreLinje.KOrdre_Id = " + STRING(fKOrdreId) + " AND KOrdreLinje.VareNr = STRING(ArtBas.ArtikkelNr) OUTER-JOIN".

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setUpdateCurrentRow C-Win 
FUNCTION setUpdateCurrentRow RETURNS LOGICAL
  ( INPUT ibUpdateCurrentRow AS LOG) :
/*------------------------------------------------------------------------------
  Purpose: Settes dersom valg artikkel skal oppdatere en gjeldende rad (ordrelinje) 
    Notes: Verdien hentes i kallende program med getUpdateCurrentRow 
------------------------------------------------------------------------------*/
bUpdateCurrentRow = ibUpdateCurrentRow.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setVarebehNr C-Win 
FUNCTION setVarebehNr RETURNS LOGICAL
  ( INPUT ifVarebehNr AS DEC ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
fVarebehNr = ifVarebehNr.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setVg C-Win 
FUNCTION setVg RETURNS LOGICAL
  ( INPUT icVg     AS CHAR,
    INPUT ibOpenQuery AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  sokVg:SCREEN-VALUE = icVg.
  IF ibOpenQuery THEN DO:      
    ASSIGN sokVg:SENSITIVE   = NO
           sokVgBeskr:SENSITIVE = NO
           btnVarGr:SENSITIVE     = NO.
    APPLY "return" TO sokVg.
  END.
  ELSE APPLY "tab" TO sokVg.
END.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

