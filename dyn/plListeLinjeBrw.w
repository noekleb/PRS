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

DEFINE VARIABLE bOk                AS LOGICAL   NO-UNDO.
DEFINE VARIABLE bVisButikker       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE hbcChkAktiv        AS HANDLE    NO-UNDO.
DEFINE VARIABLE hbcChkAktivLev     AS HANDLE    NO-UNDO.
DEFINE VARIABLE hbfChkAktiv        AS HANDLE    NO-UNDO.
DEFINE VARIABLE hbfChkAktivLev     AS HANDLE    NO-UNDO.
DEFINE VARIABLE hBrowse            AS HANDLE    NO-UNDO.
DEFINE VARIABLE hOverlayAntPl      AS HANDLE    NO-UNDO.
DEFINE VARIABLE hChild             AS HANDLE    NO-UNDO.
DEFINE VARIABLE hParent            AS HANDLE    NO-UNDO.
DEFINE VARIABLE hParentBrowse      AS HANDLE    NO-UNDO.
DEFINE VARIABLE hSearchFieldHode   AS HANDLE    NO-UNDO.
DEFINE VARIABLE hSearchField       AS HANDLE    NO-UNDO.
DEFINE VARIABLE hToolbar           AS HANDLE    NO-UNDO.
DEFINE VARIABLE ix                 AS INTEGER   NO-UNDO.
DEFINE VARIABLE cProfnr            AS CHAR      NO-UNDO.
DEF VAR hInstance                 AS INT    NO-UNDO.   
DEF VAR iCl AS INT NO-UNDO.
DEF VAR hbcBekreftet AS HANDLE NO-UNDO.
DEF VAR hbfBekreftet AS HANDLE NO-UNDO.
DEF VAR hbcplListeStatus AS HANDLE NO-UNDO.
DEF VAR hbfplListeStatus AS HANDLE NO-UNDO.
DEFINE VARIABLE cFilter AS CHARACTER NO-UNDO.

DEF VAR hBestBilde             AS HANDLE NO-UNDO.
DEF VAR hBestBildeFrame        AS HANDLE NO-UNDO.
DEF VAR hArtikkelkort          AS HANDLE NO-UNDO.
DEF VAR hFlatView              AS HANDLE NO-UNDO.
DEF VAR hPanelVert             AS HANDLE NO-UNDO.
DEF VAR hPanelFrame            AS HANDLE NO-UNDO.

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
DEF VAR iSelectorSourceCount   AS INT    NO-UNDO.
DEF VAR cCurrSelectBuffer      AS CHAR   NO-UNDO.
DEF VAR iLogPartner            AS INT    NO-UNDO.
DEF VAR bOverfOrdre            AS LOG    NO-UNDO.
DEF VAR cTekst                 AS CHAR   NO-UNDO.

DEF VAR cSesongRowIdList      AS CHAR   NO-UNDO.
DEF VAR cSesongIdList         AS CHAR   NO-UNDO.

DEF VAR iFontWingdings   AS INT    NO-UNDO.
iFontWingdings = DYNAMIC-FUNCTION("setAppFont","Wingdings, size=11 Script=symbol","") NO-ERROR.

DEF VAR hArtBasSok             AS HANDLE NO-UNDO.
DEF VAR hParentBuffer          AS HANDLE NO-UNDO.
DEF VAR hBuffer                AS HANDLE NO-UNDO.
DEF VAR iHTType                AS INT    NO-UNDO.

DEF BUFFER bArtLag FOR ArtLag.

DEF TEMP-TABLE ttStrekkode NO-UNDO LIKE Strekkode
    FIELD Antall AS DEC
    FIELD OkArt AS LOG
    INDEX Kode Kode.

{windows.i}

DEF STREAM Inn.

/* {buildfunction.i} */
/*                   */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BrwPlListeLinje searchField BestBilde ~
BtnPanel rectToolBar btnAvdeling btnSesong sokAvdelingNr sokAvdelingNavn ~
sokSaSong sokSasBeskr tbFilter sokHg sokHgBeskr btnHuvGr cbLinjer sokVg ~
sokVgBeskr btnVarGr sokLevNr sokLevNamn btnLev sokLevKod SumKostForslag ~
SumKostBekreftet 
&Scoped-Define DISPLAYED-OBJECTS sokAvdelingNr sokAvdelingNavn sokSaSong ~
sokSasBeskr tbFilter sokHg sokHgBeskr cbLinjer sokVg sokVgBeskr sokLevNr ~
sokLevNamn sokLevKod SumKostForslag SumKostBekreftet cInfoTekst 

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
    INPUT icAction     AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AdjustBrowseColumns C-Win 
FUNCTION AdjustBrowseColumns RETURNS LOGICAL
  ( INPUT ihBrowse     AS HANDLE,
    INPUT icBrowseName AS CHAR) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getArtNr C-Win 
FUNCTION getArtNr RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFrameHandle C-Win 
FUNCTION getFrameHandle RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getLocalQueryStat C-Win 
FUNCTION getLocalQueryStat RETURNS CHARACTER
  ( INPUT ihBrowseOrQuery AS HANDLE,
    INPUT icStatFields    AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getParentBrowse C-Win 
FUNCTION getParentBrowse RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getStorl C-Win 
FUNCTION getStorl RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD InitializeResize C-Win 
FUNCTION InitializeResize RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setParent C-Win 
FUNCTION setParent RETURNS LOGICAL
  ( INPUT ihParent AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setQuery C-Win 
FUNCTION setQuery RETURNS LOGICAL
  ( INPUT ihQuery AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ViewQueryStat C-Win 
FUNCTION ViewQueryStat RETURNS LOGICAL
  ( INPUT ihQueryObject AS HANDLE )  FORWARD.

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


/* Definitions of the field level widgets                               */
DEFINE BUTTON btnAvdeling 
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnHuvGr 
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnLev 
     LABEL "..." 
     SIZE 4.4 BY 1 TOOLTIP "Høyreklikk for å velge blant alle leverandører".

DEFINE BUTTON btnSesong 
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnVarGr 
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE cbLinjer AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Vis linjer med bestilt antall" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Alle",1,
                     "> 0",2,
                     "= 0",3
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE cInfoTekst AS CHARACTER FORMAT "X(100)" 
      VIEW-AS TEXT 
     SIZE 89.6 BY .62
     FONT 6.

DEFINE VARIABLE sokAvdelingNavn AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE sokAvdelingNr AS INTEGER FORMAT "zzzzz9" INITIAL 0 
     LABEL "Avdeling" 
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1.

DEFINE VARIABLE sokHg AS INTEGER FORMAT "zzzzz9" INITIAL 0 
     LABEL "Hovedgruppe" 
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1.

DEFINE VARIABLE sokHgBeskr AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE sokLevKod AS CHARACTER FORMAT "x(30)" 
     LABEL "LevArtNr" 
     VIEW-AS FILL-IN 
     SIZE 35.6 BY 1.

DEFINE VARIABLE sokLevNamn AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE sokLevNr AS INTEGER FORMAT "zzzzz9" INITIAL 0 
     LABEL "Leverandør" 
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1.

DEFINE VARIABLE sokSasBeskr AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE sokSaSong AS INTEGER FORMAT "zz9" INITIAL 0 
     LABEL "Sesong" 
     VIEW-AS FILL-IN 
     SIZE 6.2 BY 1.

DEFINE VARIABLE sokVg AS INTEGER FORMAT "zzzzz9" INITIAL 0 
     LABEL "Varegruppe" 
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1.

DEFINE VARIABLE sokVgBeskr AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE SumKostBekreftet AS DECIMAL FORMAT "->>>>,>>9.99":U INITIAL 0 
     LABEL "Estimert totalsum" 
     VIEW-AS FILL-IN 
     SIZE 17.8 BY 1 NO-UNDO.

DEFINE VARIABLE SumKostForslag AS DECIMAL FORMAT "->>>>,>>9.99":U INITIAL 0 
     LABEL "Sum forslag" 
     VIEW-AS FILL-IN 
     SIZE 18.8 BY 1 NO-UNDO.

DEFINE RECTANGLE BestBilde
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 20 BY 4.14.

DEFINE RECTANGLE BrwPlListeLinje
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 188 BY 11.91.

DEFINE RECTANGLE BtnPanel
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 90.6 BY 1.62.

DEFINE RECTANGLE rectToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 28 BY .95.

DEFINE RECTANGLE searchField
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 19 BY .95.

DEFINE VARIABLE tbFilter AS LOGICAL INITIAL NO 
     LABEL "Vis filter" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnAvdeling AT ROW 2.43 COL 73.2 NO-TAB-STOP 
     btnSesong AT ROW 2.43 COL 140.6 NO-TAB-STOP 
     sokAvdelingNr AT ROW 2.48 COL 35.4 COLON-ALIGNED HELP
          "Varegruppe"
     sokAvdelingNavn AT ROW 2.48 COL 46 COLON-ALIGNED NO-LABEL
     sokSaSong AT ROW 2.48 COL 107 COLON-ALIGNED HELP
          "Sesong"
     sokSasBeskr AT ROW 2.48 COL 113.2 COLON-ALIGNED NO-LABEL
     tbFilter AT ROW 2.57 COL 3.2
     sokHg AT ROW 3.43 COL 35.4 COLON-ALIGNED HELP
          "Varegruppe"
     sokHgBeskr AT ROW 3.43 COL 46 COLON-ALIGNED NO-LABEL
     btnHuvGr AT ROW 3.43 COL 73.2 NO-TAB-STOP 
     cbLinjer AT ROW 3.52 COL 107 COLON-ALIGNED
     sokVg AT ROW 4.38 COL 35.4 COLON-ALIGNED HELP
          "Varegruppe"
     sokVgBeskr AT ROW 4.38 COL 46 COLON-ALIGNED NO-LABEL
     btnVarGr AT ROW 4.38 COL 73.2 NO-TAB-STOP 
     sokLevNr AT ROW 5.33 COL 35.4 COLON-ALIGNED HELP
          "Varegruppe"
     sokLevNamn AT ROW 5.33 COL 46 COLON-ALIGNED NO-LABEL
     btnLev AT ROW 5.38 COL 73.2 NO-TAB-STOP 
     sokLevKod AT ROW 6.29 COL 35.4 COLON-ALIGNED HELP
          "Leverandørens artikkelnummer - bestillingsnummer"
     SumKostForslag AT ROW 19.62 COL 129 COLON-ALIGNED
     SumKostBekreftet AT ROW 19.62 COL 170 COLON-ALIGNED
     cInfoTekst AT ROW 6.62 COL 78 COLON-ALIGNED HELP
          "Kort beskrivelse av aktivitet" NO-LABEL
     BrwPlListeLinje AT ROW 7.43 COL 2
     searchField AT ROW 6.38 COL 2.2
     BestBilde AT ROW 3.14 COL 170
     BtnPanel AT ROW 4.62 COL 78.4
     rectToolBar AT ROW 1.24 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 189.6 BY 19.76.


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
/* SUPPRESS Window definition (used by the UIB) 
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Suppleringsforslag"
         HEIGHT             = 19.76
         WIDTH              = 189.8
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
                                                                        */
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME
ASSIGN C-Win = CURRENT-WINDOW.


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
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 19.76
       FRAME DEFAULT-FRAME:WIDTH            = 189.6.

ASSIGN 
       btnHuvGr:POPUP-MENU IN FRAME DEFAULT-FRAME       = MENU POPUP-MENU-btnHuvGr:HANDLE.

ASSIGN 
       btnLev:POPUP-MENU IN FRAME DEFAULT-FRAME       = MENU POPUP-MENU-btnLev:HANDLE.

/* SETTINGS FOR FILL-IN cInfoTekst IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       cInfoTekst:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       SumKostForslag:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

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
ON END-ERROR OF C-Win /* Suppleringsforslag */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Suppleringsforslag */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
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

  IF bOk THEN DO:
    IF NUM-ENTRIES(cAvdelingRowidList) > 1 THEN 
      ASSIGN sokAvdelingNr:SCREEN-VALUE   = "0"
             sokAvdelingNavn:SCREEN-VALUE = STRING(NUM-ENTRIES(cAvdelingRowidList)) + " av " +
                                       STRING(iSelectorSourceCount)
                                       .
    ELSE
      ASSIGN sokAvdelingNr:SCREEN-VALUE   = cAvdelingIdList
             sokAvdelingNavn:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","Avdeling;AvdelingNavn","WHERE AvdelingNr = " + sokAvdelingNr:SCREEN-VALUE).
    RUN InvokeMethod(hBrowse,"OpenQuery").
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
                      "where true, first Avdeling OF HuvGr NO-LOCK " + 
                         (IF cAvdelingRowIdList NE "" THEN
                            "WHERE CAN-DO('" + cAvdelingRowIdList + "',STRING(ROWID(Avdeling)))"
                          ELSE IF sokAvdelingNr:SCREEN-VALUE NE "0" THEN
                            "WHERE AvdelingNr = " + sokAvdelingNr:SCREEN-VALUE
                          ELSE "")
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
                                       STRING(iSelectorSourceCount)
                                       .
    ELSE
      ASSIGN sokHg:SCREEN-VALUE   = cHuvGrIdList
             sokHgBeskr:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","HuvGr;HgBeskr","WHERE Hg = " + sokHg:SCREEN-VALUE).

    RUN InvokeMethod(hBrowse,"OpenQuery").
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
                      "LevBas;levnamn;levnr;KjedeAvtale|Kjedeavtale|J/N"
                      ,"WHERE TRUE"
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
                                       STRING(iSelectorSourceCount)
                                       .
    ELSE
      ASSIGN sokLevNr:SCREEN-VALUE   = cLevBasIdList
             sokLevNamn:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","LevBas;LevNamn","WHERE LevNr = " + sokLevNr:SCREEN-VALUE).

    RUN InvokeMethod(hBrowse,"OpenQuery").
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSesong
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSesong C-Win
ON CHOOSE OF btnSesong IN FRAME DEFAULT-FRAME /* ... */
DO:
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "Sasong;Sasong;SasBeskr",
                      "where true",
                      INPUT-OUTPUT cSesongRowIdList,
                      "Sasong",
                      INPUT-OUTPUT cSesongIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk THEN DO:
    IF NUM-ENTRIES(cSesongRowidList) > 1 THEN 
      ASSIGN sokSasong:SCREEN-VALUE   = "0"
             sokSasBeskr:SCREEN-VALUE = STRING(NUM-ENTRIES(cSesongRowidList)) + " av " +
                                       STRING(iSelectorSourceCount)
                                       .
    ELSE
      ASSIGN sokSasong:SCREEN-VALUE   = cSesongIdList
             sokSasBeskr:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","Sasong;SasBeskr","WHERE Sasong = " + sokSasong:SCREEN-VALUE).
    RUN InvokeMethod(hBrowse,"OpenQuery").
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
                          ELSE IF sokHg:SCREEN-VALUE NE "0" THEN
                            "WHERE Hg = " + sokHg:SCREEN-VALUE
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
                                       STRING(iSelectorSourceCount)
                                       .
    ELSE
      ASSIGN sokVG:SCREEN-VALUE   = cVarGrIdList
             sokVGBeskr:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","VarGr;VgBeskr","WHERE Vg = " + sokVG:SCREEN-VALUE).

    RUN InvokeMethod(hBrowse,"OpenQuery").
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbLinjer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbLinjer C-Win
ON VALUE-CHANGED OF cbLinjer IN FRAME DEFAULT-FRAME /* Vis linjer med bestilt antall */
DO:
  RUN InvokeMethod(hBrowse,"OpenQuery").
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


    IF bOk THEN DO WITH FRAME DEFAULT-FRAME:
      IF NUM-ENTRIES(cHuvGrRowidList) > 1 THEN 
        ASSIGN sokHg:SCREEN-VALUE   = "0"
               sokHgBeskr:SCREEN-VALUE = STRING(NUM-ENTRIES(cHuvGrRowidList)) + " av " +
                                         STRING(iSelectorSourceCount)
                                         .
      ELSE
        ASSIGN sokHg:SCREEN-VALUE   = cHuvGrIdList
               sokHgBeskr:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","HuvGr;HgBeskr","WHERE Hg = " + sokHg:SCREEN-VALUE).
      RUN OpenQuery.
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

  IF bOk THEN DO WITH FRAME DEFAULT-FRAME:
    IF NUM-ENTRIES(cLevBasRowidList) > 1 THEN 
      ASSIGN sokLevNr:SCREEN-VALUE   = "0"
             sokLevNamn:SCREEN-VALUE = STRING(NUM-ENTRIES(cLevBasRowidList)) + " av " +
                                       STRING(iSelectorSourceCount)
                                       .
    ELSE
      ASSIGN sokLevNr:SCREEN-VALUE   = cLevBasIdList
             sokLevNamn:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","LevBas;LevNamn","WHERE LevNr = " + sokLevNr:SCREEN-VALUE).
    RUN OpenQuery.
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
    RUN InvokeMethod(hBrowse,"OpenQuery").
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokAvdelingNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokAvdelingNr C-Win
ON F3 OF sokAvdelingNr IN FRAME DEFAULT-FRAME /* Avdeling */
OR F10 OF sokAvdelingNr DO:
   APPLY "choose" TO btnAvdeling.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokAvdelingNr C-Win
ON RETURN OF sokAvdelingNr IN FRAME DEFAULT-FRAME /* Avdeling */
OR TAB OF sokAvdelingNr DO:
  IF sokAvdelingNr:MODIFIED THEN DO:
    sokAvdelingNavn:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","Avdeling;AvdelingNavn","WHERE AvdelingNr = " + sokAvdelingNr:SCREEN-VALUE).
    RUN InvokeMethod(hBrowse,"OpenQuery").
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokHg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokHg C-Win
ON F3 OF sokHg IN FRAME DEFAULT-FRAME /* Hovedgruppe */
OR F10 OF sokHg DO:
  APPLY "choose" TO btnHuvGr.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokHg C-Win
ON RETURN OF sokHg IN FRAME DEFAULT-FRAME /* Hovedgruppe */
OR TAB OF sokHg DO:
  IF sokHg:MODIFIED THEN DO: 
    sokHgBeskr:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","HuvGr;HgBeskr","WHERE Hg = " + sokHg:SCREEN-VALUE).
    RUN InvokeMethod(hBrowse,"OpenQuery").
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokHgBeskr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokHgBeskr C-Win
ON RETURN OF sokHgBeskr IN FRAME DEFAULT-FRAME
OR TAB OF sokHgBeskr DO:
  IF sokHgBeskr:MODIFIED THEN DO: 
    ASSIGN cHuvGrRowIdList = ""
           cHuvGrIdList    = ""
           .
    RUN InvokeMethod(hBrowse,"OpenQuery").
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokLevKod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokLevKod C-Win
ON RETURN OF sokLevKod IN FRAME DEFAULT-FRAME /* LevArtNr */
OR TAB OF sokLevKod DO:
  IF sokLevKod:MODIFIED THEN DO:
    RUN InvokeMethod(hBrowse,"OpenQuery").
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokLevNamn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokLevNamn C-Win
ON RETURN OF sokLevNamn IN FRAME DEFAULT-FRAME
OR TAB OF sokLevNamn DO:
  IF sokLevNamn:MODIFIED THEN DO:
    ASSIGN cLevBasRowIdList = ""
           cLevBasIdList    = ""
           .
    RUN OpenQuery.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokLevNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokLevNr C-Win
ON F3 OF sokLevNr IN FRAME DEFAULT-FRAME /* Leverandør */
OR F10 OF sokLevNr DO:
  APPLY "choose" TO btnLev.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokLevNr C-Win
ON RETURN OF sokLevNr IN FRAME DEFAULT-FRAME /* Leverandør */
OR TAB OF sokLevNr DO:
  IF sokLevNr:MODIFIED THEN DO:
    sokLevNamn:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","LevBas;LevNamn","WHERE LevNr = " + sokLevNr:SCREEN-VALUE).
    RUN OpenQuery.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokSasBeskr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokSasBeskr C-Win
ON RETURN OF sokSasBeskr IN FRAME DEFAULT-FRAME
OR TAB OF sokSasBeskr DO:
  IF sokSasBeskr:MODIFIED THEN DO: 
    ASSIGN cSesongRowIdList = ""
           cSesongIdList    = ""
           .    
    RUN InvokeMethod(hBrowse,"OpenQuery").
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokSaSong
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokSaSong C-Win
ON RETURN OF sokSaSong IN FRAME DEFAULT-FRAME /* Sesong */
OR TAB OF sokSasong DO:
  IF sokSasong:MODIFIED THEN DO: 
    sokSasBeskr:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","Sasong;SasBeskr","WHERE Sasong = " + sokSasong:SCREEN-VALUE).
    RUN OpenQuery.
    RETURN NO-APPLY.
  END.
END.

&Scoped-define SELF-NAME sokVg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokVg C-Win
ON F3 OF sokVg IN FRAME DEFAULT-FRAME /* Varegruppe */
OR F10 OF sokVg DO:
  APPLY "choose" TO btnVarGr.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokVg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokVg C-Win
ON RETURN OF sokVg IN FRAME DEFAULT-FRAME /* Varegruppe */
OR TAB OF sokVg DO:
  IF sokVg:MODIFIED THEN DO: 
    sokVgBeskr:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","VarGr;VgBeskr","WHERE Vg = " + sokVg:SCREEN-VALUE).
    RUN OpenQuery.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokVgBeskr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokVgBeskr C-Win
ON RETURN OF sokVgBeskr IN FRAME DEFAULT-FRAME
OR TAB OF sokVgBeskr DO:
  IF sokVgBeskr:MODIFIED THEN DO:
    ASSIGN cVarGrRowIdList = ""
           cVarGrIdList    = ""
           .
    RUN OpenQuery.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbFilter C-Win
ON VALUE-CHANGED OF tbFilter IN FRAME DEFAULT-FRAME /* Vis filter */
DO:
  DO WITH FRAME Default-Frame:
    ASSIGN
    sokAvdelingNr:HIDDEN   = NOT SELF:CHECKED 
    sokHg:HIDDEN           = NOT SELF:CHECKED
    sokVg:HIDDEN           = NOT SELF:CHECKED
    sokSasong:HIDDEN       = NOT SELF:CHECKED
    sokLevNr:HIDDEN        = NOT SELF:CHECKED
    sokLevKod:HIDDEN       = NOT SELF:CHECKED
    sokAvdelingNavn:HIDDEN = NOT SELF:CHECKED
    sokHgBeskr:HIDDEN      = NOT SELF:CHECKED
    sokVgBeskr:HIDDEN      = NOT SELF:CHECKED
    sokLevNamn:HIDDEN      = NOT SELF:CHECKED
    sokSasBeskr:HIDDEN     = NOT SELF:CHECKED
    btnAvdeling:HIDDEN     = NOT SELF:CHECKED
    btnHuvGr:HIDDEN        = NOT SELF:CHECKED
    btnVarGr:HIDDEN        = NOT SELF:CHECKED
    btnLev:HIDDEN          = NOT SELF:CHECKED
    btnSesong:HIDDEN       = NOT SELF:CHECKED
    .
  END.

  IF SELF:CHECKED THEN 
    ASSIGN
      .
  ELSE
    ASSIGN
      .
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
  PUBLISH "InvalidateHandle".
  IF VALID-HANDLE(hArtikkelkort) THEN APPLY "close" TO hArtikkelkort.
  IF VALID-HANDLE(hArtBasSok) THEN APPLY "close" TO hArtBasSok.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  RUN enable_UI.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

{incl/supptrigg.i hBrowse}

ON 'alt-a':U OF FRAME {&FRAME-NAME} ANYWHERE
  RUN Artikkelkort.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cancelRecord C-Win 
PROCEDURE cancelRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DefaultActionBrowse C-Win 
PROCEDURE DefaultActionBrowse :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/

  /*               
  IF hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("plListeStatus"):BUFFER-VALUE = 1 THEN 
    RUN SUPER.
  */
  RUN EditRecord.
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
  /* Hide all frames. */
  HIDE FRAME DEFAULT-FRAME.
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
DYNAMIC-FUNCTION("DoLockWindow",THIS-PROCEDURE:CURRENT-WINDOW).
DYNAMIC-FUNCTION("setAttribute",SESSION,"userkeepswindowlocked","yes").
   

DEF VAR hParentBrw      AS HANDLE NO-UNDO.

hParentBrw = DYNAMIC-FUNCTION("GetLinkedObject",hBrowse,"parent","from").

IF hParentBrw:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN 
DO:
  hParentBuffer = hParentBrw:QUERY:GET-BUFFER-HANDLE(1).
  IF hParentBuffer:BUFFER-FIELD("plListeStatus"):BUFFER-VALUE > 20 THEN DO:
    DYNAMIC-FUNCTION("setAttribute",hOverlayAntPl,"visible","no").
    DYNAMIC-FUNCTION("setAttribute",hToolbar,"disabledEvents","newart,scan,delete,getPDAFil,NyArt").
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"disabledEvents","setAntallArt,nullstillArt").
  END.
  ELSE DO:
    DYNAMIC-FUNCTION("setAttribute",hOverlayAntPl,"visible","yes").
    DYNAMIC-FUNCTION("setAttribute",hToolbar,"disabledEvents","").
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"disabledEvents","").
  END.
END.
/* Ingen ordre tilgjengelig. */
ELSE DO:
    DYNAMIC-FUNCTION("setAttribute",hOverlayAntPl,"visible","no").
    DYNAMIC-FUNCTION("setAttribute",hToolbar,"disabledEvents","newart,scan,delete,getPDAFil,NyArt").
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"disabledEvents","setAntallArt,nullstillArt").
END.

/*
IF hBrowse:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN DO:
  IF hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("plListeStatus"):BUFFER-VALUE > 1 THEN DO:
    DYNAMIC-FUNCTION("setAttribute",hOverlayAntPl,"visible","no").
    DYNAMIC-FUNCTION("setAttribute",hToolbar,"disabledEvents","newart,scan,delete,getPDAFil,NyArt").
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"disabledEvents","setAntallArt,nullstillArt").
  END.
  ELSE DO:
    DYNAMIC-FUNCTION("setAttribute",hOverlayAntPl,"visible","yes").
    DYNAMIC-FUNCTION("setAttribute",hToolbar,"disabledEvents","").
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"disabledEvents","").
  END.
END.
*/

RUN SUPER.

IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowse THEN 
  RUN VisMiniBilde IN hBestBilde 
      (IF hBrowse:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN 
         hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("BildNr"):BUFFER-VALUE 
       ELSE 0).

DYNAMIC-FUNCTION("setAttribute",SESSION,"userkeepswindowlocked","").
DYNAMIC-FUNCTION("DoLockWindow",?).

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
  RUN w-vartkor.w  PERSIST SET hArtikkelkort (DYNAMIC-FUNCTION("getRecId","ArtBas",hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("RowIdent2"):BUFFER-VALUE), "ENDRE," + STRING(THIS-PROCEDURE)).
ELSE 
  RUN ByttArtikkel IN hArtikkelkort (hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE).
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
  DISPLAY sokAvdelingNr sokAvdelingNavn sokSaSong sokSasBeskr tbFilter sokHg 
          sokHgBeskr cbLinjer sokVg sokVgBeskr sokLevNr sokLevNamn sokLevKod 
          SumKostForslag SumKostBekreftet cInfoTekst 
      WITH FRAME DEFAULT-FRAME.
  ENABLE BrwPlListeLinje searchField BestBilde BtnPanel rectToolBar btnAvdeling 
         btnSesong sokAvdelingNr sokAvdelingNavn sokSaSong sokSasBeskr tbFilter 
         sokHg sokHgBeskr btnHuvGr cbLinjer sokVg sokVgBeskr btnVarGr sokLevNr 
         sokLevNamn btnLev sokLevKod SumKostForslag SumKostBekreftet 
      WITH FRAME DEFAULT-FRAME.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ErrorLog C-Win 
PROCEDURE ErrorLog :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER cFileName AS CHAR NO-UNDO.

  DEF VAR pcTekst   AS CHARACTER NO-UNDO.
  DEF VAR cErrorFil AS CHAR      NO-UNDO.

  IF NOT CAN-FIND(FIRST ttStrekkode WHERE ttSTrekkode.ArtikkelNr = 0)
    THEN RETURN.
  ELSE 
  UTSKRIFT_AV_ERR_Logg:
  DO:
    pcTekst = "  Ukjente strekkoder i fil " +  cFileName + '.'. 

    ASSIGN
      cErrorFil = OS-GETENV('TMP') + '\' 
                 + "Error_" 
                 + ENTRY(NUM-ENTRIES(cFileName,'\'),cFileName,'\')
                 + ".Txt".

    OUTPUT TO VALUE(cErrorFil).
    
    PUT UNFORMATTED
        "Innlesning " + STRING(TODAY) + "  " + STRING(TIME,"HH:MM:SS") + "." SKIP
        "Feil i fil: " + cFileName SKIP
        "  " pcTekst SKIP(1)
        .
    FOR EACH ttStrekkode WHERE ttSTrekkode.ArtikkelNr = 0:
      PUT UNFORMATTED ttStrekkode.Kode SKIP.
    END.
  
    OUTPUT CLOSE.
    
    IF SEARCH(cErrorFil) <> ? THEN
    DO:
      RUN ShellExecute{&A} IN hpApi(0,
                                  "open",
                                  "notepad.exe",
                                  SEARCH(cErrorFil),
                                  "",
                                  1,                                  OUTPUT hInstance).
    END.    


  END. /* UTSKRIFT_AV_ERR_Logg */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EtikettRecord C-Win 
PROCEDURE EtikettRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iReturn AS INT NO-UNDO.

NY_KODE:    
DO:    
    iReturn = 0.
    RUN JBoxBrowseSelectMsg.w ("Etikettutskrift valgte poster?",
        hBrowse:NUM-SELECTED-ROWS,
        IF INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount")) LT INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"rowstobatch")) THEN
        INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount"))
        ELSE 99999,
        OUTPUT iReturn).  /*1=Alle,2=Valgte*/

    IF iReturn = 0 THEN RETURN NO-APPLY.

    IF iReturn = 1 OR hBrowse:NUM-SELECTED-ROWS = 0 THEN /* Alle poster */
      DO:
        IF DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"pllistelinje_etiketter.p","") THEN
           DYNAMIC-FUNCTION("SkrivEtikett" IN hParent, DYNAMIC-FUNCTION("getTransactionMessage")).
      END.
    ELSE DO: /* Valgte poster */
        IF DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowse,"pllistelinje_etiketter.p","") THEN
            DYNAMIC-FUNCTION("SkrivEtikett" IN hParent, DYNAMIC-FUNCTION("getTransactionMessage")).
    END.
END. /* NY_KODE */
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
DEF INPUT PARAM ihFlatView   AS HANDLE NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO INIT YES.


DEF VAR hFlatBrw    AS HANDLE NO-UNDO.

hFlatBrw  = DYNAMIC-FUNCTION("getBrowseHandle" IN ihFlatView).
DYNAMIC-FUNCTION("setAttribute",hFlatBrw,"availDistinctColumns",
                 "Artikkelnr,Beskr,Vg,VgBeskr,LevNamn,FarBeskr").

DYNAMIC-FUNCTION("setAttribute",hFlatBrw,"availAccumFields",
                 "KostForslag,KostBekreftet").

hFlatView = ihFlatView.


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
RUN SUPER.

RUN InitQuery IN hFlatView (
                    ""   /* Filter fields (comma sep) */
                   ,""   /* Filter operators (comma sep) */
                   ,""   /* Filter values (pipe sep) */
                   ,""   /* Initial sort column(s) (comma sep) */
                   ,"Vg,VgBeskr"   /* Distinct columns (comma sep) */
                   ,"KostForslag,KostBekreftet"   /* Columns to accumulate (comma sep) */
                   ).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getPDAFilRecord C-Win 
PROCEDURE getPDAFilRecord :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
------------------------------------------------------------------------------*/
DEF VAR opcFieldList AS CHAR NO-UNDO.
DEF VAR opcLabelList AS CHAR NO-UNDO.
DEF VAR opiReturn    AS INT  NO-UNDO.
DEF VAR cLookupValue AS CHAR NO-UNDO.

DEF VAR iReturn     AS INT    NO-UNDO.
DEF VAR cVareNr     AS CHAR   NO-UNDO.
DEF VAR cDummy      AS CHAR   NO-UNDO.
DEF VAR cButList    AS CHAR   NO-UNDO.
DEF VAR cFileName   AS CHAR   NO-UNDO.
DEF VAR cKatalog    AS CHAR   NO-UNDO.
DEF VAR cFilter     AS CHAR   NO-UNDO.
DEF VAR cFilNavn AS CHAR   NO-UNDO.
DEF VAR cPrefix     AS CHAR NO-UNDO.
DEF VAR iButikkNr   AS INT    NO-UNDO.
DEF VAR hParentBrw      AS HANDLE NO-UNDO.

hParentBrw = DYNAMIC-FUNCTION("GetLinkedObject",hBrowse,"parent","from").

IF hParentBrw:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN 
DO:
  hParentBuffer = hParentBrw:QUERY:GET-BUFFER-HANDLE(1).
  IF INT(hParentBuffer:BUFFER-FIELD("plListeStatus"):BUFFER-VALUE) > 20 THEN 
  DO:
      MESSAGE "Ordre er sendt. Nye linjer kan ikke registreres."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN.
  END.
END.
ELSE DO:
    MESSAGE "Ordre er ikke tilgjengelig. Nye linjer kan ikke registreres."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
END.

iButikkNr = INT(hParentBuffer:BUFFER-FIELD("FraButikkNr"):BUFFER-VALUE).

DO  WITH FRAME DEFAULT-FRAME:
    IF iButikkNr = ? OR iButikkNr = 0 THEN 
    DO:
      DYNAMIC-FUNCTION("DoMessage",0,0,"Kan ikke importere fil hvis ikke butikk er valgt","","").
      RETURN NO-APPLY.
    END.

    RUN JBoxDLookup.w ("HT-Type;TypeId;Betegnelse", 
                         "WHERE HTAktiv = TRUE",
                         INPUT-OUTPUT cLookupValue).
    IF cLookupValue NE "" THEN 
      ASSIGN 
        iHTType = INT(ENTRY(1,cLookupValue,"|")).
    ELSE DO:
        bOk = FALSE.
        RETURN.
    END.
    
    IF NOT CAN-DO("6,10,12",STRING(iHTType)) THEN
    DO:
        MESSAGE 'Import mot pakkseddel kan bare gjøres for håndterminaltype 6 (Symbol PPT8800) og 12 (CubComm).' SKIP
            'Valgt håndterminaltype er ' iHtType '.'
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        bOk = FALSE.
        RETURN.
    END.

    cFileName = DYNAMIC-FUNCTION("getFieldValues","HT-Type","WHERE TypeId = '" + STRING(iHTType) + "'","FilPrefix").
    cPreFix   = cFileName.
    IF cFileName = '' THEN
    DO:
        MESSAGE 'Filnavn ikke satt opp i håndterminalregisteret.'
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        bOk = FALSE.
        RETURN.
    END.
    cKatalog = DYNAMIC-FUNCTION("getFieldValues","HT-Type","WHERE TypeId = '" + STRING(iHTType) + "'","ImportKatalog").
    IF cKatalog = '' THEN
    DO:
        MESSAGE 'Importkatalog ikke satt opp i håndterminalregisteret.'
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        bOk = FALSE.
        RETURN.
    END.
END. /* DEFAULT-FRAME */

/* Setter opp filmaske m.m. */
ASSIGN
    cKatalog = RIGHT-TRIM(cKatalog,'\')
    cFilter  = cFilNavn + '*.' + STRING(iButikkNr)
    .
/* Sikrer at backup katalog finnes. */
OS-CREATE-DIR value(cKatalog + "~\bku").

/* Hvis det ligger en Inv.Dat, skal denne først konverteres til PPT880 formatet.            */
/* vpiartbas_importer_pricat_fil.p kaller opp xbxmobileinvinnles.p som gjør konverteringen. */
bOk = DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"vpiartbas_importer_pricat_fil.p"
                      ,'892;' + cKatalog + '\inv.dat').

bOk = FALSE.
/* Henter VPI fil fra disk */
SYSTEM-DIALOG GET-FILE cFileName 
              FILTERS "Håndterminalfiler: " + cFilter cFilter 
              INITIAL-DIR cKatalog
              RETURN-TO-START-DIR
              MUST-EXIST
              UPDATE bOk.

IF bOk = FALSE THEN
    RETURN NO-APPLY.
ELSE 
    MESSAGE "Skal import av håndterminalfil starte?" SKIP
            "Fil som skal importeres:" cFileName
        VIEW-AS ALERT-BOX INFO BUTTONS YES-NO UPDATE bOk.
IF bOk = FALSE THEN
    RETURN NO-APPLY.


RUN PDAFilImport (cFileName,iButikkNr,iHTType,OUTPUT bOk).
  

IF NOT bOK THEN
    MESSAGE 'Import av fil feilet. Ingen linjer er innlest.'
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
ELSE DO:           
  /* Flytter filen til backup katalog. */
  IF SEARCH(cFileName) <> ? THEN
      OS-COPY VALUE(cFileName) 
              VALUE(REPLACE(cFileName,cPreFix,'bku\' + cPrefix)).
  IF SEARCH(REPLACE(cFileName,cPreFix,'bku\' + cPrefix)) <> ? THEN
      OS-DELETE VALUE(cFileName).

  RUN InvokeMethod (hBrowse,'OpenQuery'). 
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
iSelectorSourceCount = INT(DYNAMIC-FUNCTION("getAttribute",ihSelectorSource,"totalcount")).

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
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
   
  DYNAMIC-FUNCTION("NewObject",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE,"procedure").

  cInfoTekst = DYNAMIC-FUNCTION("getFieldValues","SysPara","WHERE SysHId = 5 and SysGr = 24 and ParaNr = 2","Parameter1").
  IF (cInfoTekst = '' OR cInfoTekst = ? OR cInfoTekst = '?') THEN cInfoTekst = 'Forslaget viser tilgjengelige suppleringsvarer fra sentrallager'.

  RUN VisMiniBilde.w PERSIST SET hBestBilde.

  RUN InitializeObject IN hBestBilde (BestBilde:HANDLE).
  hBestBildeFrame = DYNAMIC-FUNCTION("getFrameHandle" IN hBestBilde).
  
  cProfnr = DYNAMIC-FUNCTION("getFieldValues","FIRST Butiker",
                             "WHERE Sentrallager","ProfilNr").
  IF cProfnr = ? THEN
    cProfnr = DYNAMIC-FUNCTION("getFieldValues","FIRST Prisprofil",
                               "WHERE true","ProfilNr").

  /* Lev.nr på logistikkpartner eller kjedens sentrallager som suppleringsordre for kjedeleverte varer skal legges på. */
  iLogPartner = INT(DYNAMIC-FUNCTION("getFieldValues","SysPara","WHERE SysHId = 5 and SysGr = 24 and ParaNr = 38","Parameter1")).
  IF iLogPartner = 0 THEN iLogPartner = 38.

  cTekst = DYNAMIC-FUNCTION("getFieldValues","SysPara","WHERE SysHId = 5 and SysGr = 24 and ParaNr = 6","Parameter1").
  IF CAN-DO('1,J,Ja,Y,Yes,True',cTekst) THEN 
    bOverfOrdre = TRUE.
  ELSE
    bOverfOrdre = FALSE.    

  iCl = INT(DYNAMIC-FUNCTION("getFieldValues","SysPara",
                              "WHERE SysHId = 5 and SysGr = 1 and ParaNr = 1","Parameter1")). 
  /*
  hBrwArtikkel:TOOLTIP = "Dobbeltklikk for å åpne artikkelkort".
  */
  hBrowse = DYNAMIC-FUNCTION("NewBrowse"
                          ,BrwPlListeLinje:HANDLE
                          ,10000
                          ,"MULTIPLE"
                          ,"PlListeLinje"
                           + ";ArtikkelNr|Art.nr@1"
                           + ";+AlfaStr|CHARACTER|x(8)|plliste_alfastr|Str@5"
                           + ";Antall|Forslag|>>>9@8"
                           + ";!+KostForslag|DECIMAL|>>>><>>9.99|plliste_forslkost|Kost.forsl"
                           + ";AntallPlukket|Bestillt|>>>9@9"
                           + ";+KostBekreftet|DECIMAL|>>>><>>9.99|plliste_bekrkost|Estimert sum@10"
                           + ";+CLLager|DECIMAL|-><>>><>>9|plliste_cllagant|CL Lag@6"
                           + ";!StrKode"
                           + ";!PlListeId"
                           + ";!+StrSeq|INTEGER|>>9|plliste_strseq"
                           + ";EDato|Endret"
                           + ",ArtBas"
                             + ";!LopNr|Løpenr"
                             + ";LevKod|Lev.art.nr|x(50)@2"
                             + ";Beskr|Varetekst|x(50)@3"
                             + ";!LevNr|Lev.nr"
                             + ";!BildNr"
                             + ";LevFargKod|Lev.fargekode@4"
                           + ",LevBas"
                             + ";LevNamn|Lev.navn|x(50)@11"
                           + ",VarGr"
                             + ";Vg|Vg@12"
                             + ";VgBeskr|Vg.tekst|x(50)@13"
                           + ",HuvGr"
                             + ";Hg|Hg"
                             + ";HgBeskr|Hg.tekst|x(50)"
                           + ",Avdeling"
                             + ";AvdelingNr|AvdNr"
                             + ";AvdelingNavn|Avd.tekst|x(50)"
                           + ",Sasong"
                             + ";Sasong|Sesong"
                             + ";SasBeskr|Ses.tekst|x(50)"
                           + ",plListeHode"
                             + ";plListeStatus|Status"
                             + ";!FraButikkNr|ButikkNr"
                           + ",ArtLag"
                             + ";LagAnt|Lager@7"
                          ,"WHERE false"
                           + ",FIRST ArtBas NO-LOCK WHERE ArtBas.ArtikkelNr = PlListeLinje.ArtikkelNr"
                           + ",FIRST LevBas NO-LOCK OF ArtBas"
                           + ",FIRST VarGr NO-LOCK OF ArtBas"
                           + ",FIRST HuvGr NO-LOCK OF VarGr"
                           + ",FIRST Avdeling NO-LOCK OF HuvGr"
                           + ",FIRST Sasong NO-LOCK OF ArtBas"
                           + ",FIRST plListeHode OF plListeLinje"
                           + ",FIRST ArtLag where ArtLag.ArtikkelNr = plListeLinje.ArtikkelNr and ArtLag.Butik = plListeHode.FraButikkNr and ArtLag.StrKode = plListeLinje.StrKode OUTER-JOIN"
                          ,"").
                          
  hBrowse:TOOLTIP = "Trykk ENTER eller Dobbeltklikk for å endre bestillt antall".
  hBuffer = hBrowse:QUERY:GET-BUFFER-HANDLE(1).

  hSearchField = DYNAMIC-FUNCTION("NewBrowseSearchField",SearchField:HANDLE,hBrowse,5).
  DYNAMIC-FUNCTION("CreateObjectLink",hSearchField,hBrowse).

  DYNAMIC-FUNCTION("setSortString",hBrowse,"Beskr,StrSeq").
  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,THIS-PROCEDURE).

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcFieldProc","plliste_brwcalc.p").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"noColumnSort","AlfaStr,Antall,AntallPlukket").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"queryStatFields","KostForslag,KostBekreftet").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"customUpdateValProc","=pllistelinje_update.p").
  /*DYNAMIC-FUNCTION("setAttribute",hBrowse,"enableOnDblClick","yes").*/

  DYNAMIC-FUNCTION("NewMenuBand",hBrowse
                  ,"MultiSortBrowse;Sorter på flere kolonner"
                  + ",rule"
                  + ",setAntallArt;Sett antall bestilt"
                  + ",rule"
                  + ",NullstillArt;Nullstil antall bestilt"
                  ,"").

  hOverlayAntPl  = DYNAMIC-FUNCTION("NewBrowseFillIn",hBrowse,"AntallPlukket","AntallPlukket"
            ,"","","","").

  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hOverlayAntPl,"AntallPlukket").
  hOverlayAntPl:TOOLTIP = "Bestillt antall".
  DYNAMIC-FUNCTION("setAttribute",hOverlayAntPl,"refreshRow","yes").
  
  hToolbar = DYNAMIC-FUNCTION("NewToolBar",
                    rectToolBar:HANDLE, 
                    "",         
                    "Delete"
                  + ",Edit;Artikkelkort&"
                  + ",excel;Eksporter til E&xcel"
                  + ",rule"
                  + ",NyArt;Ny artikkel;Opprett ny artikkel¤enable"
                  + ",Strekkode"
                  + ",Etikett"
                  + ",NewArt;Hent artikkel;Legg til artikkel¤enable"
                  + ",Scan¤enable"
                  /*+ ",rule"
                  + ",getPDAFil;Hent PDA fil¤enable"*/
                  + ",rule"
                  + ",FlatView;Vis sum pr varegr"
                  + ",BrowseConfig"
                    ,"maxborder").    

  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hToolbar).

  hPanelVert = DYNAMIC-FUNCTION("NewPanel"
            ,BtnPanel:HANDLE
            ,"" /* New sub-menu (blank for none) */ 
            ,"SetStatus;&1-Sett rediger stat.;Sett status rediger;SetStatusRecord;ico\write2.ico" 
            + ",Bekreft;&2-Bekreft og send;Send ordre;SendRecord;ico\important_note.ico"
            + ",Print;&3-Skriv ut;Skrivut2;PrintRecord;ico\print.ico"
           /*+ ",Email;&3-Send bestilling;Send bestilling;SendemailRecord;ico\envelope.ico" */
           /*+ ",Rapport;Vis sum pr varegr;Vis sum pr varegr;FlatViewRecord;ico\MSACCESSforms.ico" */
           /*+ ",NullstillArtRecord;Nullstill linjer;Nullstill alle/valgte linjer;NullstillArtRecord;ico\erase.ico"*/
            ,140,36     
            ,"").

  DYNAMIC-FUNCTION("CreateObjectLink",hPanelVert,hBrowse).

  hPanelFrame = DYNAMIC-FUNCTION("getPanelFrame" IN hPanelVert).
  
  /*
  DYNAMIC-FUNCTION("setResizeXGroup", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,33,
                   "BrwPlListeLinje,"
                 + hBrowse:NAME 
                   ).

  DYNAMIC-FUNCTION("setMoveXGroup", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,67,
                   "BrwPlListeLinje," + hBrowse:NAME).
  */
  DYNAMIC-FUNCTION("setAddMoveX", THIS-PROCEDURE:CURRENT-WINDOW, hBestBildeFrame,hBestBildeFrame:NAME).
  DYNAMIC-FUNCTION("setAddMoveX", THIS-PROCEDURE:CURRENT-WINDOW, hPanelFrame,hPanelFrame:NAME).
  
  ASSIGN
    cbLinjer:SCREEN-VALUE = '1'
    sokAvdelingNr:HIDDEN   = TRUE
    sokHg:HIDDEN           = TRUE
    sokVg:HIDDEN           = TRUE
    sokSasong:HIDDEN       = TRUE
    sokLevNr:HIDDEN        = TRUE
    sokLevKod:HIDDEN       = TRUE
    sokAvdelingNavn:HIDDEN = TRUE
    sokHgBeskr:HIDDEN      = TRUE
    sokVgBeskr:HIDDEN      = TRUE
    sokLevNamn:HIDDEN      = TRUE
    sokSasBeskr:HIDDEN     = TRUE
    btnAvdeling:HIDDEN     = TRUE
    btnHuvGr:HIDDEN        = TRUE
    btnVarGr:HIDDEN        = TRUE
    btnLev:HIDDEN          = TRUE
    btnSesong:HIDDEN       = TRUE
    tbFilter:HIDDEN        = FALSE
    SumKostForslag:HIDDEN  = TRUE 
    cInfoTekst:SCREEN-VALUE = cInfoTekst
    .
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
RUN SUPER.
getLocalQueryStat(hBrowse,"").
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
FRAME {&FRAME-NAME}:MOVE-TO-TOP().
hPanelFrame:MOVE-TO-TOP().
APPLY "entry" TO hBrowse.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NewArtRecord C-Win 
PROCEDURE NewArtRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR hParentBrw      AS HANDLE NO-UNDO.

hParentBrw = DYNAMIC-FUNCTION("GetLinkedObject",hBrowse,"parent","from").

IF hParentBrw:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN 
DO:
  hParentBuffer = hParentBrw:QUERY:GET-BUFFER-HANDLE(1).
  IF INT(hParentBuffer:BUFFER-FIELD("plListeStatus"):BUFFER-VALUE) > 20 THEN 
  DO:
      MESSAGE "Ordre er sendt. Nye linjer kan ikke registreres."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN.
  END.
END.
ELSE DO:
    MESSAGE "Ordre er ikke tilgjengelig. Nye linjer kan ikke registreres."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
END.

IF NOT VALID-HANDLE(hArtBasSok) THEN DO:
  RUN ArtBasSok.w PERSIST SET hArtBasSok.
  RUN InitializeObject IN hArtBasSok.

  /*DYNAMIC-FUNCTION("setLevNr" IN hArtBasSok,hParentBuffer:BUFFER-FIELD("LevNr"):BUFFER-VALUE,NO).*/

/*     DYNAMIC-FUNCTION("setButikkNr" IN hArtBasSok,hParentBuffer:BUFFER-FIELD("ButikkNr"):BUFFER-VALUE).  */
/*     DYNAMIC-FUNCTION("setOrdreId"  IN hArtBasSok,hParentBuffer:BUFFER-FIELD("KOrdre_Id"):BUFFER-VALUE). */
/*     DYNAMIC-FUNCTION("setCloseOnSelect" IN hArtBasSok,YES).                                             */
/*     DYNAMIC-FUNCTION("setUpdateCurrentRow" IN hArtBasSok,YES).                                          */

END.

RUN MoveToTop IN hArtBasSok.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NullstillArtRecord C-Win 
PROCEDURE NullstillArtRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*
IF DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"pllinje_nullstillant.p","") THEN
  RUN InvokeMethod(hBrowse,"OpenQuery").
*/

DEF VAR iReturn     AS INT  NO-UNDO.
DEF VAR ocValue     AS CHAR NO-UNDO.
DEFINE VARIABLE cRowIdList AS CHARACTER NO-UNDO.

RUN JBoxBrowseMsgUpdateVal.w ('Skal antall bestillt på alle/valgte linjer nullstilles?',
                              hBrowse:NUM-SELECTED-ROWS,
                              IF INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount")) LT INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"rowstobatch")) THEN
                                INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount"))
                              ELSE 99999,
                              "",
                              OUTPUT ocValue,
                              OUTPUT iReturn).

IF iReturn = 0 THEN RETURN "cancel".

IF iReturn = 1 THEN
  bOk = DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"pllinje_nullstillant.p",STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("plListeId"):BUFFER-VALUE) + ';' + ocValue).
ELSE IF iReturn = 2 THEN
DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN
      cRowIdList = cRowIdList + hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('RowIdent1'):BUFFER-VALUE + ','.
  END.
  bOk = DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowse,"pllinje_nullstillant.p",STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("plListeId"):BUFFER-VALUE) + ';' + ocValue).
END.
ELSE
  LEAVE.
  
IF NOT bOK THEN
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil i sending av informasjon ",""). 
ELSE DO:
/*
  RUN InvokeMethod (hBrowse,'OpenQuery').
  RUN InvokeMethod (hBrowse,'DisplayRecord').
*/
    IF iReturn = 1 THEN 
        RUN InvokeMethod (hBrowse,'OpenQuery'). 
    ELSE 
        DYNAMIC-FUNCTION("RefreshRowids",hBrowse,TRIM(cRowIdList,",")).
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
DEF INPUT PARAMETER piArtikkelNr AS DEC NO-UNDO.
              
DO  WITH FRAME DEFAULT-FRAME:
    RUN ArtBasSok.w PERSIST SET hArtBasSok.
    RUN InitializeObject IN hArtBasSok.
    RUN settArtikkelNr IN hArtBasSok (STRING(piArtikkelNr)).
    RUN MoveToTop  IN hArtBasSok.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NyArtRecord C-Win 
PROCEDURE NyArtRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR iButikkNr   AS INT    NO-UNDO.
DEF VAR hParentBrw      AS HANDLE NO-UNDO.

hParentBrw = DYNAMIC-FUNCTION("GetLinkedObject",hBrowse,"parent","from").

IF hParentBrw:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN 
DO:
  hParentBuffer = hParentBrw:QUERY:GET-BUFFER-HANDLE(1).
  IF INT(hParentBuffer:BUFFER-FIELD("plListeStatus"):BUFFER-VALUE) > 20 THEN 
  DO:
      MESSAGE "Ordre er sendt. Nye linjer kan ikke registreres."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN.
  END.
END.
ELSE DO:
    MESSAGE "Ordre er ikke tilgjengelig. Nye linjer kan ikke registreres."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
END.

iButikkNr = INT(hParentBuffer:BUFFER-FIELD("FraButikkNr"):BUFFER-VALUE).

DO WITH FRAME DEFAULT-FRAME:
    bOk = FALSE.
    THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
    RUN ArtBasVedlikehold.w (THIS-PROCEDURE,"new",OUTPUT bOK).
    THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.
    IF VALID-HANDLE(hArtBasSok) THEN RUN MoveToTop  IN hArtBasSok.
    /*THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().*/
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenQuery C-Win 
PROCEDURE OpenQuery :
/*------------------------------------------------------------------------------
    Purpose:                                                                                                                                          
    Notes:                                                                                                                                            
------------------------------------------------------------------------------*/
DEF VAR cPrescanAvd     AS CHAR NO-UNDO.
DEF VAR cPrescanHgr     AS CHAR NO-UNDO.
DEF VAR cPrescanVgr     AS CHAR NO-UNDO.
DEF VAR cPrescanLev     AS CHAR NO-UNDO.
DEF VAR cPrescanSasong  AS CHAR NO-UNDO.
DEF VAR cBaseQuery      AS CHAR NO-UNDO.
DEF VAR hParentBrw      AS HANDLE NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  ASSIGN sokAvdelingNavn:MODIFIED = FALSE
         sokAvdelingNr:MODIFIED   = FALSE
         sokHg:MODIFIED           = FALSE
         sokHgBeskr:MODIFIED      = FALSE
         sokVg:MODIFIED           = FALSE
         sokVgBeskr:MODIFIED      = FALSE
         sokLevKod:MODIFIED       = FALSE
         sokSasong:MODIFIED       = FALSE
         sokSasBeskr:MODIFIED     = FALSE
         cbLinjer
         .
  hParentBrw = DYNAMIC-FUNCTION("GetLinkedObject",hBrowse,"parent","from").
  IF VALID-HANDLE(hParentBrw) THEN
    hParentBuffer = hParentBrw:QUERY:GET-BUFFER-HANDLE(1).

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"queryFilter",
                   (IF sokLevKod:SCREEN-VALUE NE "" THEN
                     " AND LevKod BEGINS '" + sokLevKod:SCREEN-VALUE + "'"
                    ELSE "")
                 + (IF cbLinjer = 2 THEN 
                     " AND AntallPlukket > 0"
                    ELSE IF cbLinjer = 3 THEN
                     " AND AntallPlukket = 0"
                    ELSE "")).

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"preScanQuery","").

  ASSIGN cPrescanAvd = IF sokAvdelingNr:SCREEN-VALUE NE "0" THEN
                        "AvdelingNr = " + sokAvdelingNr:SCREEN-VALUE
                       ELSE IF sokAvdelingNavn:SCREEN-VALUE NE "" AND NUM-ENTRIES(cAvdelingRowIdList) < 2 THEN
                         (IF INDEX(sokAvdelingNavn:SCREEN-VALUE,"*") > 0 THEN 
                           "AvdelingNavn MATCHES '" + sokAvdelingNavn:SCREEN-VALUE + "*'"
                          ELSE
                           "AvdelingNavn BEGINS '" + sokAvdelingNavn:SCREEN-VALUE + "'")
                       ELSE IF sokAvdelingNavn:SCREEN-VALUE NE "" AND NUM-ENTRIES(cAvdelingRowIdList) > 1 THEN
                        "CAN-DO('" + REPLACE(cAvdelingIdList,"|",",") + "',STRING(AvdelingNr))"
                       ELSE ""
         cPrescanHgr = IF sokHg:SCREEN-VALUE NE "0" THEN
                        "Hg = " + sokHg:SCREEN-VALUE
                       ELSE IF sokHgBeskr:SCREEN-VALUE NE "" AND NUM-ENTRIES(cHuvGrRowIdList) < 2 THEN
                         (IF INDEX(sokHgBeskr:SCREEN-VALUE,"*") > 0 THEN
                           "HgBeskr MATCHES '" + sokHgBeskr:SCREEN-VALUE + "*'"
                          ELSE
                           "HgBeskr BEGINS '" + sokHgBeskr:SCREEN-VALUE + "'")
                       ELSE IF sokHgBeskr:SCREEN-VALUE NE "" AND NUM-ENTRIES(cHuvGrRowIdList) > 1 THEN
                         "CAN-DO('" + REPLACE(cHuvGrIdList,"|",",") + "',STRING(Hg))"
                       ELSE "" 
         cPrescanVgr = IF sokVg:SCREEN-VALUE NE "0" THEN
                         "Vg = " + sokVg:SCREEN-VALUE
                       ELSE IF sokVgBeskr:SCREEN-VALUE NE "" AND NUM-ENTRIES(cVarGrRowIdList) < 2 THEN
                        (IF INDEX(sokVgBeskr:SCREEN-VALUE,"*") > 0 THEN
                          "VgBeskr MATCHES '" + sokVgBeskr:SCREEN-VALUE + "*'"
                         ELSE
                          "VgBeskr BEGINS '" + sokVgBeskr:SCREEN-VALUE + "'")
                       ELSE IF sokVgBeskr:SCREEN-VALUE NE "" AND NUM-ENTRIES(cVarGrRowIdList) > 1 THEN
                         "CAN-DO('" + REPLACE(cVarGrIdList,"|",",") + "',STRING(Vg))"
                       ELSE ""
         cPrescanLev = IF sokLevNr:SCREEN-VALUE NE "0" THEN
                        "LevNr = " + sokLevNr:SCREEN-VALUE
                       ELSE IF sokLevNamn:SCREEN-VALUE NE "" AND NUM-ENTRIES(cLevBasRowIdList) < 2 THEN
                        (IF INDEX(sokLevNamn:SCREEN-VALUE,"*") > 0 THEN
                          "LevNamn MATCHES '" + sokLevNamn:SCREEN-VALUE + "*'"
                         ELSE
                          "LevNamn BEGINS '" + sokLevNamn:SCREEN-VALUE + "'")
                       ELSE IF sokLevNamn:SCREEN-VALUE NE "" AND NUM-ENTRIES(cLevBasRowIdList) > 1 THEN
                         "CAN-DO('" + REPLACE(cLevBasIdList,"|",",") + "',STRING(LevNr))"
                       ELSE ""
         cPrescanSasong = IF sokSasong:SCREEN-VALUE NE "0" THEN
                        "Sasong = " + sokSasong:SCREEN-VALUE
                       ELSE IF sokSasBeskr:SCREEN-VALUE NE "" AND NUM-ENTRIES(cSesongRowIdList) < 2 THEN
                         (IF INDEX(sokSasBeskr:SCREEN-VALUE,"*") > 0 THEN
                           "SasBeskr MATCHES '" + sokSasBeskr:SCREEN-VALUE + "*'"
                          ELSE
                           "SasBeskr BEGINS '" + sokSasBeskr:SCREEN-VALUE + "'")
                       ELSE IF sokSasBeskr:SCREEN-VALUE NE "" AND NUM-ENTRIES(cSesongRowIdList) > 1 THEN
                         "CAN-DO('" + REPLACE(cSesongIdList,"|",",") + "',STRING(Sasong))"
                       ELSE "" 
                       .

    cBaseQuery = DYNAMIC-FUNCTION("getAttribute",hBrowse,"baseQuery").

    IF cPrescanAvd NE "" THEN
      cPrescanAvd = "Avdeling WHERE " + cPrescanAvd + ",EACH HuvGr NO-LOCK OF Avdeling,EACH VarGr NO-LOCK OF HuvGr,EACH ArtBas NO-LOCK OF VarGr,EACH PlListeLinje NO-LOCK OF ArtBas " + cBaseQuery.
    IF cPrescanHgr NE "" THEN
      cPrescanHgr = "HuvGr WHERE " + cPrescanHgr + ",EACH VarGr NO-LOCK OF HuvGr,EACH ArtBas NO-LOCK OF VarGr,EACH PlListeLinje NO-LOCK OF ArtBas " + cBaseQuery.
    IF cPrescanVgr NE "" THEN
      cPrescanVgr = "VarGr WHERE " + cPrescanVgr + ",EACH ArtBas NO-LOCK OF VarGr,EACH PlListeLinje NO-LOCK OF ArtBas " + cBaseQuery.
    IF cPrescanLev NE "" THEN 
      cPrescanLev = "LevBas WHERE " + cPrescanLev + ",EACH ArtBas NO-LOCK OF LevBas,EACH PlListeLinje NO-LOCK OF ArtBas " + cBaseQuery.
    IF cPrescanSasong NE "" THEN
      cPrescanSasong = "Sasong WHERE " + cPrescanSasong + ",EACH ArtBas NO-LOCK OF Sasong,EACH PlListeLinje NO-LOCK OF ArtBas " + cBaseQuery.

    DYNAMIC-FUNCTION("setAttribute",hBrowse,"preScanQueryFilter",
                     cPrescanAvd + "|" + cPrescanHgr + "|" + cPrescanVgr + "|" + cPrescanLev + "|" + cPrescanSasong).

END.

RUN SUPER.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PDAFilImport C-Win 
PROCEDURE PDAFilImport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT  PARAMETER cFileName AS CHAR NO-UNDO.
DEF INPUT  PARAMETER iButikkNr AS INT  NO-UNDO.
DEF INPUT  PARAMETER iHTType   AS INT  NO-UNDO.  
DEF OUTPUT PARAMETER bOk       AS LOG  NO-UNDO.

DEF VAR cLinje AS CHAR NO-UNDO.
DEF VAR piAnt  AS INT NO-UNDO.

/* Tømmer tabellen */
FOR EACH ttStrekkode:
    DELETE ttSTrekkode.
END.
                         
DO WITH FRAME DEFAULT-FRAME:
    ASSIGN
        bOk       = TRUE.

    INPUT STREAM Inn FROM VALUE(cFileName) NO-ECHO.
    INNLES:
    REPEAT:
        piAnt = piAnt + 1.
        IMPORT STREAM Inn UNFORMATTED cLinje NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
        DO:
            MESSAGE 'Feil i importfil. Innlesning avbrudt.'
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            bOk = FALSE.
            INPUT STREAM Inn CLOSE.
            RETURN.
        END.
        CASE iHTType:
          WHEN 6 OR WHEN 10 THEN /*SymbolPPT8800 og BxMobile */ DO:
              FIND ttSTrekkode NO-LOCK WHERE 
                  ttSTrekkode.Kode = TRIM(ENTRY(2,cLinje,' ')) NO-ERROR.
              IF NOT AVAILABLE ttStrekkode THEN
              DO:
                  CREATE ttStrekkode.
                  ASSIGN
                      ttStrekkode.Kode = TRIM(ENTRY(2,cLinje,' ')).
                  FIND Strekkode NO-LOCK WHERE
                      Strekkode.Kode = ttSTrekkode.Kode NO-ERROR.
                  IF AVAILABLE Strekkode THEN
                      ttSTrekkode.ArtikkelNr = StreKkode.ArtikkelNr.
                  IF NOT AVAILABLE Strekkode THEN
                  DO:
                      FIND FIRST VPIStrekkode NO-LOCK WHERE
                          VPISTrekkode.Kode = ttSTrekkode.Kode NO-ERROR.
                      IF AVAILABLE VPIStrekkode THEN
                          ttSTrekkode.ArtikkelNr = DEC(VPIStrekkode.VareNr).
                  END.
              END.
              IF AVAILABL ttStrekkode THEN
                  ASSIGN
                    ttSTrekkode.Antall = ttSTrekkode.Antall + INT(ENTRY(9,cLinje,' ')) NO-ERROR.
          END. /* SymbolPPT8800 */
          WHEN 12 THEN /*CubComm*/ DO:
            IF TRIM(ENTRY(1,cLinje,';')) = '1' THEN
            DO:
                FIND ttSTrekkode NO-LOCK WHERE 
                    ttSTrekkode.Kode = TRIM(ENTRY(3,cLinje,';')) NO-ERROR.
                IF NOT AVAILABLE ttStrekkode THEN
                DO:
                    CREATE ttStrekkode.
                    ASSIGN
                        ttStrekkode.Kode = TRIM(ENTRY(3,cLinje,';')).
                    FIND Strekkode NO-LOCK WHERE
                        Strekkode.Kode = ttSTrekkode.Kode NO-ERROR.
                    IF AVAILABLE Strekkode THEN
                        ttSTrekkode.ArtikkelNr = StreKkode.ArtikkelNr.
                    IF NOT AVAILABLE Strekkode THEN
                    DO:
                        FIND FIRST VPIStrekkode NO-LOCK WHERE
                            VPISTrekkode.Kode = ttSTrekkode.Kode NO-ERROR.
                        IF AVAILABLE VPIStrekkode THEN
                            ttSTrekkode.ArtikkelNr = DEC(VPIStrekkode.VareNr).
                    END.
                END.
                IF AVAILABL ttStrekkode THEN
                    ASSIGN
                      ttSTrekkode.Antall = ttSTrekkode.Antall + INT(ENTRY(10,cLinje,';')) NO-ERROR.
            END.
            ELSE IF TRIM(ENTRY(1,cLinje,';')) = '99' THEN
            DO:
                IF piAnt <> INT(ENTRY(2,cLinje,';')) THEN
                DO:
                    MESSAGE 'Det er innlest ' + STRING(piAnt) + ' linjer. Men filen skal inneholde ' + ENTRY(2,cLinje,';') + '.' + CHR(10) +
                            'Filens innhold samsvarer ikke med sjekksiffer. Filen kan være ødelagt og bør sjekkes.'
                        VIEW-AS ALERT-BOX INFO BUTTONS OK.
                END.
            END.
          END. /* CubComm */
        END. /* CASE */

        IF AVAILABLE ttSTrekkode AND ttStrekkode.ArtikkelNr > 0 THEN
        DO:
            RUN SetskjermVerdier (ttStrekkode.Kode).
            DELETE ttStrekkode.
        END.
    END. /* INNLES */
    INPUT STREAM Inn CLOSE.

     IF CAN-FIND(FIRST ttStrekkode WHERE 
                ttStrekkode.ArtikkelNr = 0) THEN
        RUN ErrorLog (cFileName).
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
DEFINE INPUT PARAMETER icDir AS CHARACTER  NO-UNDO.

DEF VAR fCurrArtikkelNr AS DEC NO-UNDO.

IF VALID-HANDLE(hArtikkelkort) THEN
DO:
    IF CAN-DO("Prev,Next",icDir) THEN DO:
      IF NOT hBrowse:IS-ROW-SELECTED(hBrowse:FOCUSED-ROW) THEN
        hBrowse:SELECT-ROW(hBrowse:FOCUSED-ROW).
      IF icDir = "Prev" THEN
        hBrowse:SELECT-PREV-ROW().
      ELSE
        hBrowse:SELECT-NEXT-ROW().
      RUN ByttArtikkel IN hArtikkelkort (hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE).
    END.
END.

ELSE DO:
    IF CAN-DO("Prev,Next",icDir) THEN DO:
      IF NOT hBrowse:IS-ROW-SELECTED(hBrowse:FOCUSED-ROW) THEN
        hBrowse:SELECT-ROW(hBrowse:FOCUSED-ROW).

      IF icDir = "Prev" THEN DO:
        fCurrArtikkelNr = hBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE.
        REPEAT WHILE hBuffer:AVAIL:
          bOk = hBrowse:SELECT-PREV-ROW().
          IF NOT bOk OR 
             (hBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE NE fCurrArtikkelNr /*AND
              (IF bPrevNextInvalidStrekkode THEN NOT hBuffer:BUFFER-FIELD("GyldigStrekkode"):BUFFER-VALUE ELSE TRUE)*/
              ) THEN
            LEAVE.
        END.
      END.
      ELSE DO:
        fCurrArtikkelNr = hBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE.
        REPEAT:
          bOk = hBrowse:SELECT-NEXT-ROW().
          IF NOT bOk OR 
            (hBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE NE fCurrArtikkelNr /*AND
             (IF bPrevNextInvalidStrekkode THEN NOT hBuffer:BUFFER-FIELD("GyldigStrekkode"):BUFFER-VALUE ELSE TRUE)*/
             ) THEN
            LEAVE.
        END.
      END.
      APPLY "value-changed" TO hBrowse.
    END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrintRecord C-Win 
PROCEDURE PrintRecord :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cRappfil AS CHARACTER   NO-UNDO.
    

    IF NOT CAN-FIND(FIRST plListeLinje WHERE
                      plListeLinje.PlListeId = DEC(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('plListeId'):BUFFER-VALUE()) AND
                      plListeLinje.AntallPlukket > 0
                    ) THEN  
    DO:
      MESSAGE 'Ordren inneholder ingen linje med bestilt antall > 0.'
      VIEW-AS ALERT-BOX TITLE "Varsel".
      RETURN.
    END.

    RUN skrivPllistelinjePDF.p (hBrowse:QUERY:GET-BUFFER-HANDLE(1),"O",OUTPUT cRappfil).
    IF cRappfil = "" THEN DO:
        MESSAGE "Ingen rapport generert"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.
    ELSE
        RUN browse2pdf\viewxmldialog.w (cRappFil,"ORDREFORSLAG").


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
  DEF VAR iGreen  AS INT INIT 10 NO-UNDO.
  DEF VAR iRed    AS INT INIT 12 NO-UNDO.
  DEF VAR iYellow AS INT INIT 14 NO-UNDO.
  DEF VAR iBlue   AS INT INIT 11 NO-UNDO.

  IF hbfplListeStatus:BUFFER-VALUE > 20 THEN
    hbcBekreftet:BGCOLOR = iBlue.
  ELSE IF hbfplListeStatus:BUFFER-VALUE <= 20 THEN
  DO:
    hbcBekreftet:BGCOLOR =  IF hbfBekreftet:BUFFER-VALUE > 0 THEN iGreen 
                            ELSE iRed.
  END.  
                           
RUN SUPER.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ScanRecord C-Win 
PROCEDURE ScanRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR hParentBrw      AS HANDLE NO-UNDO.

hParentBrw = DYNAMIC-FUNCTION("GetLinkedObject",hBrowse,"parent","from").

IF hParentBrw:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN 
DO:
  hParentBuffer = hParentBrw:QUERY:GET-BUFFER-HANDLE(1).
  IF INT(hParentBuffer:BUFFER-FIELD("plListeStatus"):BUFFER-VALUE) > 20 THEN 
  DO:
      MESSAGE "Ordre er sendt. Nye linjer kan ikke registreres."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN.
  END.
END.
ELSE DO:
    MESSAGE "Ordre er ikke tilgjengelig. Nye linjer kan ikke registreres."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
END.

RUN gscannerinput.w (THIS-PROCEDURE).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendemailRecord C-Win 
PROCEDURE SendemailRecord :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cRappfil AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE iButikkNr AS INTEGER     NO-UNDO.
    
    IF INTEGER(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('PlListeStatus'):BUFFER-VALUE()) <= 1 THEN
    DO:
      MESSAGE 'Ordre kan ikke sendes via eMail før den er godkjent.'
      VIEW-AS ALERT-BOX TITLE "Varsel".
      RETURN.
    END.
    
    STATUS DEFAULT ''.
    STATUS DEFAULT 'Generering av vedlegg pågår...'.
    RUN skrivPllistelinjePDF.p (hBrowse:QUERY:GET-BUFFER-HANDLE(1),"O",OUTPUT cRappfil).
    IF cRappfil = "" THEN DO:
        MESSAGE "Ingen rapport generert. Kunne ikke sende eMail."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.
    ELSE DO:
        iButikkNr = DYNAMIC-FUNCTION("getFieldValues","PlListeHode",
                                   "WHERE PlListeId = " + STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('PlListeId'):BUFFER-VALUE()) ,"FraButikkNr").
        STATUS DEFAULT 'Klargjøring av eMail pågår...'.
        RUN d-sendOrdrePlListe.p (INPUT cRappfil, iButikkNr, STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('PlListeId'):BUFFER-VALUE())).
    END.
    STATUS DEFAULT ''.
        


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendRecord C-Win 
PROCEDURE SendRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iReturn     AS INT  NO-UNDO.
DEF VAR ocValue     AS CHAR NO-UNDO.
DEFINE VARIABLE obOk AS LOG NO-UNDO.
DEFINE VARIABLE cRowIdList AS CHARACTER NO-UNDO.

IF INTEGER(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("plListeStatus"):BUFFER-VALUE) > 20 THEN 
  DO:
    MESSAGE 'Ordren er allerede sendt. Skal den sendes om igjen?' SKIP(1)
            'NB: Skriv inn i teksten at det er omsending, slik at ordrekontoret forstår at det er en omsending og at det ikke er en ny ordre som kommer.'
    VIEW-AS ALERT-BOX BUTTONS YES-NO TITLE 'Sending av ordre' UPDATE obOk.
    IF obOk = FALSE THEN RETURN.
    ELSE bOk = TRUE.
  END.
ELSE DO:  
      MESSAGE 'Skal orden sendes?' SKIP(1)
              'NB: Etter at ordren er sendt kan den ikke lenger redigeres.'
      VIEW-AS ALERT-BOX BUTTONS YES-NO TITLE 'Sending av ordre' UPDATE obOk.
      IF obOk = FALSE THEN RETURN.
      bOk = DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"pllinje_send_ordre.p",STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("plListeId"):BUFFER-VALUE) + ';' + ocValue).
  END.

IF bOK THEN 
DO:
    RUN InvokeMethod (hBrowse,'OpenQuery'). 
    IF iLogPartner = INTEGER(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("LevNr"):BUFFER-VALUE) AND 
                     bOverfOrdre = TRUE THEN. /* Gjør ingenting. */
    ELSE RUN SendemailRecord.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setAntallArtRecord C-Win 
PROCEDURE setAntallArtRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR iReturn     AS INT  NO-UNDO.
DEF VAR ocValue     AS CHAR NO-UNDO.
DEFINE VARIABLE cRowIdList AS CHARACTER NO-UNDO.

RUN JBoxBrowseMsgUpdateVal.w ('Sette bestillt antall?',
                              hBrowse:NUM-SELECTED-ROWS,
                              IF INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount")) LT INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"rowstobatch")) THEN
                                INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount"))
                              ELSE 99999,
                              "DECIMAL|->>,>>>,>>9|",
                              OUTPUT ocValue,
                              OUTPUT iReturn).

IF iReturn = 0 THEN RETURN "cancel".

IF iReturn = 1 THEN
  bOk = DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"pllinje_setant.p",STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("plListeId"):BUFFER-VALUE) + ';' + ocValue).
ELSE IF iReturn = 2 THEN
DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN
      cRowIdList = cRowIdList + hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('RowIdent1'):BUFFER-VALUE + ','.
  END.
  bOk = DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowse,"pllinje_setant.p",STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("plListeId"):BUFFER-VALUE) + ';' + ocValue).
END.
ELSE
  LEAVE.
  
IF NOT bOK THEN
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil i sending av informasjon ",""). 
ELSE DO:
/*
  RUN InvokeMethod (hBrowse,'OpenQuery').
  RUN InvokeMethod (hBrowse,'DisplayRecord').
*/
    IF iReturn = 1 THEN 
        RUN InvokeMethod (hBrowse,'OpenQuery'). 
    ELSE 
        DYNAMIC-FUNCTION("RefreshRowids",hBrowse,TRIM(cRowIdList,",")).
END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetSkjermVerdier C-Win 
PROCEDURE SetSkjermVerdier :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAM icStrekkode AS CHAR NO-UNDO.

DEF VAR cArtStorl   AS CHAR NO-UNDO.
DEF VAR lArtikkelNr AS DEC  NO-UNDO.
DEF VAR iAntall AS INT NO-UNDO.

iAntall = 1.

FIND FIRST ttStrekkode NO-LOCK WHERE 
     ttSTrekkode.Kode = icStrekkode NO-ERROR.
IF AVAILABLE ttStrekkode THEN 
    iAntall = ttStrekkode.Antall.

FIND Strekkode NO-LOCK WHERE
    Strekkode.Kode = icStrekkode NO-ERROR.
IF NOT AVAILABLE Strekkode THEN
    FIND FIRST VPIStrekkode NO-LOCK WHERE 
    VPIStrekkode.Kode = icStrekkode NO-ERROR.
IF AVAILABLE Strekkode THEN
    lArtikkelNr = Strekkode.ArtikkelNr.
ELSE IF AVAILABLE VPIStrekkode THEN
    lArtikkelNr = DEC(VPIStrekkode.VareNr).

IF NOT CAN-FIND(ArtBas WHERE ArtBas.ArtikkelNr = lArtikkelNr) AND 
       CAN-FIND(FIRST VPIArtBas WHERE VPIArtBas.ArtikkelNr = lArtikkelNr) THEN 
       DO:
           FIND FIRST VPIArtBas NO-LOCK WHERE VPIArtBas.ArtikkelNr = lArtikkelNr NO-ERROR.
           cTekst = {tbchooseAll.i}.
           RUN artbas_new.p (STRING(VPIArtBas.EkstVPILevNr) + ';' + cTekst + ';' + STRING(lArtikkelNr), 
                             ?, 
                             '', 
                             OUTPUT cTekst, 
                             OUTPUT bOk).      
       END.


cArtStorl = DYNAMIC-FUNCTION("getFieldList","Strekkode;ArtikkelNr,StrKonv;Storl",
                             "WHERE kode = '" + icStrekkode + "',FIRST StrKonv NO-LOCK OF Strekkode").

IF cArtStorl NE ? AND cArtStorl NE '' THEN
  AddStr(DECIMAL(ENTRY(1,cArtStorl,"|")),
         ENTRY(2,cArtStorl,"|"),
         DEC(iAntall),
         "add").
ELSE
  DYNAMIC-FUNCTION("DoMessage",0,0,"Finner ikke strekkode","","").

/*
cArtStorl = DYNAMIC-FUNCTION("getFieldList","Strekkode;ArtikkelNr,StrKonv;Storl",
                             "WHERE kode = '" + icStrekkode + "',FIRST StrKonv NO-LOCK OF Strekkode").

IF cArtStorl NE ? THEN
  AddStr(DECIMAL(ENTRY(1,cArtStorl,"|")),ENTRY(2,cArtStorl,"|"),1,"add").
ELSE
  DYNAMIC-FUNCTION("DoMessage",0,0,"Finner ikke strekkode","","").
*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetStatusRecord C-Win 
PROCEDURE SetStatusRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iReturn     AS INT  NO-UNDO.
DEF VAR ocValue     AS CHAR NO-UNDO.
DEFINE VARIABLE obOk AS LOG NO-UNDO.
DEFINE VARIABLE cRowIdList AS CHARACTER NO-UNDO.

IF INTEGER(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("plListeStatus"):BUFFER-VALUE) <> 1 THEN 
  DO:
    MESSAGE 'Rediger status kan bare settes på nye ordre.' SKIP(1)
    VIEW-AS ALERT-BOX BUTTONS OK TITLE 'Rediger status'.
    RETURN.
  END.
ELSE DO:  
      MESSAGE 'Skal rediger status settes på ordren?' SKIP(1)
              'Ordre som er tildelt redigerstatus, påvirkes ikke ved generering av nye ordre.'
      VIEW-AS ALERT-BOX BUTTONS YES-NO TITLE 'Rediger status' UPDATE obOk.
      IF obOk = FALSE THEN RETURN.
      bOk = DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"pllinje_rediger_ordre.p",STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("plListeId"):BUFFER-VALUE) + ';' + ocValue).
  END.
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

DYNAMIC-FUNCTION("DoLockWindow",hParent:CURRENT-WINDOW).
RUN Strekkode.w (THIS-PROCEDURE).
DYNAMIC-FUNCTION("DoLockWindow",?).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE updateRecord C-Win 
PROCEDURE updateRecord :
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
    INPUT icAction     AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose: Legg til eller endre artikkel. Kalles fra artbassok.w 
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR bReturnFocus AS LOG   NO-UNDO.
DEF VAR cStrList     AS CHAR  NO-UNDO.
DEF VAR bOkStr       AS LOG   NO-UNDO.
DEF VAR iLinjenr     AS INT   NO-UNDO.

DYNAMIC-FUNCTION("setCurrentObject",hToolbar).


bOk = hBuffer:FIND-FIRST("WHERE ArtikkelNr = " + STRING(ifArtikkelNr)
                         + "  AND AlfaStr  = '" + TRIM(icStorl) + "'") NO-ERROR.
IF bOk THEN DO:
  hBrowse:SET-REPOSITIONED-ROW(hBrowse:DOWN,"conditional").
  hBrowse:QUERY:REPOSITION-TO-ROWID(hBuffer:ROWID).

  /*
  IF ifPlukkAnt NE ? AND ifPlukkAnt > 1 THEN
    hOverlayAntPl:SCREEN-VALUE = STRING(ifPlukkAnt).
  ELSE
    hOverlayAntPl:SCREEN-VALUE = STRING(DEC(hOverlayAntPl:SCREEN-VALUE) + 1).
  */

  hOverlayAntPl:SCREEN-VALUE = STRING(DEC(hBuffer:BUFFER-FIELD("AntallPlukket"):BUFFER-VALUE) + ifPlukkAnt).


  RUN InvokeMethod(hOverlayAntPl,"LeaveBrowseFillIn").

  RETURN YES.
END.
  
IF icAction = "add" THEN DO WITH FRAME {&FRAME-NAME}:

  IF ifPlukkAnt = 0 OR ifPlukkAnt = ? THEN ifPlukkAnt = 1.

/*   IF cmbButikkNr:SCREEN-VALUE = ? OR cmbButikkNr:SCREEN-VALUE = "" THEN DO:                          */
/*     DYNAMIC-FUNCTION("DoMessage",0,0,"Kan ikke legge til artikkel hvis ikke butikk er valgt","",""). */
/*     RETURN NO.                                                                                       */
/*   END.                                                                                               */

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"bufferExtraFields","ArtikkelNr,PlLinjeNr,Storl,Antall").
  iLinjenr = INTEGER(DYNAMIC-FUNCTION("getFieldValues","LAST PlListeLinje","WHERE PlListeId = " + STRING(hParentBuffer:BUFFER-FIELD("PlListeId"):BUFFER-VALUE),"PlLinjeNr")).
  IF iLinjenr = ? THEN iLinjenr = 1.
  ELSE iLinjenr = iLinjenr + 1.

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"bufferExtraValues",
                   STRING(ifArtikkelNr) + "|"
                 + STRING(iLinjenr) + "|"
                 + icStorl + "|"
                 + STRING(ifPlukkAnt)
                   ).
  RUN InvokeMethod(hBrowse,"NewRecord").

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"bufferExtraFields","").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"bufferExtraValues","").
END.
ELSE 
  APPLY "value-changed" TO hBrowse.
    
RETURN bReturnFocus.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AdjustBrowseColumns C-Win 
FUNCTION AdjustBrowseColumns RETURNS LOGICAL
  ( INPUT ihBrowse     AS HANDLE,
    INPUT icBrowseName AS CHAR):
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF icBrowseName = "BrwPlListeLinje" THEN 
   ASSIGN 
    ihBrowse:GET-BROWSE-COLUMN(1):WIDTH-PIXELS = 60 
    ihBrowse:GET-BROWSE-COLUMN(2):WIDTH-PIXELS = 80 
    ihBrowse:GET-BROWSE-COLUMN(3):WIDTH-PIXELS = 160 
    ihBrowse:GET-BROWSE-COLUMN(4):WIDTH-PIXELS = 80 
    ihBrowse:GET-BROWSE-COLUMN(5):WIDTH-PIXELS = 35
    ihBrowse:GET-BROWSE-COLUMN(6):WIDTH-PIXELS = 35 
    ihBrowse:GET-BROWSE-COLUMN(7):WIDTH-PIXELS = 35 
    ihBrowse:GET-BROWSE-COLUMN(8):WIDTH-PIXELS = 35 
    ihBrowse:GET-BROWSE-COLUMN(9):WIDTH-PIXELS = 70 
    ihBrowse:GET-BROWSE-COLUMN(10):WIDTH-PIXELS = 160 
    ihBrowse:GET-BROWSE-COLUMN(11):WIDTH-PIXELS = 45
    ihBrowse:GET-BROWSE-COLUMN(12):WIDTH-PIXELS = 160 
    ihBrowse:GET-BROWSE-COLUMN(13):WIDTH-PIXELS = 40 
    ihBrowse:GET-BROWSE-COLUMN(14):WIDTH-PIXELS = 80 
    ihBrowse:GET-BROWSE-COLUMN(15):WIDTH-PIXELS = 160 
    ihBrowse:GET-BROWSE-COLUMN(16):WIDTH-PIXELS = 40 
    ihBrowse:GET-BROWSE-COLUMN(17):WIDTH-PIXELS = 160
    ihBrowse:GET-BROWSE-COLUMN(18):WIDTH-PIXELS = 40 
    ihBrowse:GET-BROWSE-COLUMN(19):WIDTH-PIXELS = 160

/*     ihBrowse:GET-BROWSE-COLUMN(6):WIDTH-PIXELS = 20          */
/*     ihBrowse:GET-BROWSE-COLUMN(9):WIDTH-PIXELS = 200 /* 8 */ */
/*     hbcChkAktivLev = ihBrowse:GET-BROWSE-COLUMN(5)                                   /* Ny */ */
/*     hbfChkAktivLev = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('ChkAktivLev') /* Ny */ */
/*     hbcChkAktiv    = ihBrowse:GET-BROWSE-COLUMN(6)  /*5*/                                     */
/*     hbfChkAktiv    = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('ChkAktiv')             */
  .

  DO ix = 1 TO ihBrowse:NUM-COLUMNS:
    CASE ihBrowse:GET-BROWSE-COLUMN(ix):NAME:
      WHEN 'AntallPlukket' THEN /*Navn på kolonnen som representerer telletype*/
        ASSIGN
          hbcBekreftet = ihBrowse:GET-BROWSE-COLUMN(ix)
          hbfBekreftet = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('AntallPlukket')
        .
      WHEN 'plListeStatus' THEN /*Navn på kolonnen som representerer telletype*/
        ASSIGN
          hbcplListeStatus = ihBrowse:GET-BROWSE-COLUMN(ix)
          hbfplListeStatus = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('plListeStatus')
        .
    END CASE.
  END.

  
RETURN TRUE.

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

  IF hBuffer:AVAIL THEN
      RETURN DEC(hBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE). /* Dummy ved opprettelse av ny artikkel */
  ELSE 
      RETURN 0.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFrameHandle C-Win 
FUNCTION getFrameHandle RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN FRAME {&FRAME-NAME}:HANDLE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getLocalQueryStat C-Win 
FUNCTION getLocalQueryStat RETURNS CHARACTER
  ( INPUT ihBrowseOrQuery AS HANDLE,
    INPUT icStatFields    AS CHAR ) : 
/*------------------------------------------------------------------------------
  Purpose:  Sum up listed fields in a local query
    Notes:  If no fields are specified go and get them from the querystatfields attribute
            and then store the result back in the querystatfieldvalues
------------------------------------------------------------------------------*/
DEF VAR hBuffer       AS HANDLE NO-UNDO.
DEF VAR hQuery        AS HANDLE NO-UNDO.
DEF VAR hQueryBuffer  AS HANDLE NO-UNDO.
DEF VAR cStatFields   AS CHAR   NO-UNDO.
DEF VAR fStatValues   AS DEC    NO-UNDO EXTENT 100.
DEF VAR cReturnString AS CHAR   NO-UNDO.
DEF VAR iCount        AS INT    NO-UNDO.

IF icStatFields = "" THEN
  cStatFields = DYNAMIC-FUNCTION("getAttribute",ihBrowseOrQuery,"querystatfields").
ELSE cStatFields = icStatFields.

IF ihBrowseOrQuery:TYPE = "browse" THEN
  hBuffer = ihBrowseOrQuery:QUERY:GET-BUFFER-HANDLE(1).
ELSE
  hBuffer = ihBrowseOrQuery:GET-BUFFER-HANDLE(1).

CREATE BUFFER hQueryBuffer FOR TABLE hBuffer.
CREATE QUERY hQuery.
hQuery:SET-BUFFERS(hQueryBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + hQueryBuffer:NAME).
hQuery:QUERY-OPEN().
hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  iCount = iCount + 1.
  DO ix = 1 TO NUM-ENTRIES(cStatFields):
    fStatValues[ix] = fStatValues[ix] + hQueryBuffer:BUFFER-FIELD(ENTRY(ix,cStatFields)):BUFFER-VALUE.
  END.
  hQuery:GET-NEXT().
END.

DELETE OBJECT hQuery.
DELETE OBJECT hQueryBuffer.

DO ix = 1 TO NUM-ENTRIES(cStatFields):
  cReturnString = cReturnString + ENTRY(ix,cStatFields) + "|" + STRING(fStatValues[ix]) + ";".
  DYNAMIC-FUNCTION("setAttribute",ihBrowseOrQuery,"statvalue" + ENTRY(ix,cStatFields),STRING(fStatValues[ix])).
END.

IF icStatFields = "" THEN DO:
  DYNAMIC-FUNCTION("setAttribute",ihBrowseOrQuery,"querystatfieldvalues",TRIM(cReturnString,";")).
  DYNAMIC-FUNCTION("setAttribute",ihBrowseOrQuery,"recordcount",STRING(iCount)).
  ViewQueryStat(ihBrowseOrQuery).
END.
ELSE 
  cReturnString = "rowcount|" + STRING(iCount) + ";" + cReturnString.

RETURN TRIM(cReturnString,";").

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getParentBrowse C-Win 
FUNCTION getParentBrowse RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN hBrowse.

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

RETURN REPLACE(
       REPLACE(
       DYNAMIC-FUNCTION("getFieldList","PlListeLinje;,StrKonv;Storl",
                        "WHERE ArtikkelNr = " + STRING(hBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE)
                      + ",FIRST StrKonv NO-LOCK WHERE StrKonv.StrKode = PlListeLinje.StrKode")
      ,"|",",")
      ," ","").

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION InitializeResize C-Win 
FUNCTION InitializeResize RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,
                 "BestBilde,BtnPanel").
DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,
                 "BestBilde,BtnPanel").

DYNAMIC-FUNCTION("setResizeXGroup", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,33,
                 "BrwPlListeLinje"
                 ).

DYNAMIC-FUNCTION("setMoveXGroup", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,33,
                 "BrwPlListeLinje").

DYNAMIC-FUNCTION("setAddMoveX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,
                 "SumKostForslag,SumKostBekreftet,BestBilde,BtnPanel"). 

DYNAMIC-FUNCTION("setAddMoveY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,
                 "SumKostForslag,SumKostBekreftet"). 

RETURN YES.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setParent C-Win 
FUNCTION setParent RETURNS LOGICAL
  ( INPUT ihParent AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
hParent = ihParent.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setQuery C-Win 
FUNCTION setQuery RETURNS LOGICAL
  ( INPUT ihQuery AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
hParentBrowse = ihQuery.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ViewQueryStat C-Win 
FUNCTION ViewQueryStat RETURNS LOGICAL
  ( INPUT ihQueryObject AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF ihQueryObject = hBrowse THEN DO WITH FRAME {&FRAME-NAME}:
  ASSIGN SumKostForslag:SCREEN-VALUE = 
             DYNAMIC-FUNCTION("getAttribute",
             ihQueryObject,"statValueKostForslag")
         SumKostBekreftet:SCREEN-VALUE = 
             DYNAMIC-FUNCTION("getAttribute",
             ihQueryObject,"statValueKostBekreftet")
         .
END.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

