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
DEFINE VARIABLE bOverstyrKol       AS LOG       NO-UNDO.
DEFINE VARIABLE hBrowse            AS HANDLE    NO-UNDO.
DEFINE VARIABLE hBuffer            AS HANDLE    NO-UNDO.
DEFINE VARIABLE hCurrTabFrame      AS HANDLE    NO-UNDO.
DEFINE VARIABLE hCurrTabProc       AS HANDLE    NO-UNDO.
DEFINE VARIABLE hFieldMap          AS HANDLE    NO-UNDO.
DEFINE VARIABLE hInnPrisAvvikCol   AS HANDLE    NO-UNDO.
DEFINE VARIABLE hLevAnt            AS HANDLE    NO-UNDO.
DEFINE VARIABLE hNyFrakt           AS HANDLE    NO-UNDO.
DEFINE VARIABLE hNyInnkjopsPris    AS HANDLE    NO-UNDO.
DEFINE VARIABLE hNyInnkjopsPrisCol AS HANDLE    NO-UNDO.
DEFINE VARIABLE hNyPris            AS HANDLE    NO-UNDO.
DEFINE VARIABLE hNyRab1%           AS HANDLE    NO-UNDO.
DEFINE VARIABLE hNyRab1%Col        AS HANDLE    NO-UNDO.
DEFINE VARIABLE hParent            AS HANDLE    NO-UNDO.
DEFINE VARIABLE hParentBrowse      AS HANDLE    NO-UNDO.
DEFINE VARIABLE hParentBuffer      AS HANDLE    NO-UNDO.
DEFINE VARIABLE hQuery             AS HANDLE    NO-UNDO.
DEFINE VARIABLE hTabFolder         AS HANDLE    NO-UNDO.
DEFINE VARIABLE hTilbudCol         AS HANDLE    NO-UNDO.
DEFINE VARIABLE hToolbar           AS HANDLE    NO-UNDO.
DEFINE VARIABLE hUtPrisAvvikCol    AS HANDLE    NO-UNDO.
DEFINE VARIABLE hVarekostCol       AS HANDLE    NO-UNDO.
DEFINE VARIABLE hViewer            AS HANDLE    NO-UNDO.
DEFINE VARIABLE iCurrTab           AS INTEGER   NO-UNDO.
DEFINE VARIABLE iFontWingdings     AS INTEGER   NO-UNDO.
DEFINE VARIABLE iReturn            AS INTEGER   NO-UNDO.
DEFINE VARIABLE ix                 AS INTEGER   NO-UNDO.
iFontWingdings = DYNAMIC-FUNCTION("setAppFont","Wingdings, size=11 Script=symbol","").

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BrwPkSdlPris TBPkSdlPris PrisViewer ~
rsKorrigert tbAvvikInnPris tbAvvikPris tbAvvikPrisVk 
&Scoped-Define DISPLAYED-OBJECTS rsKorrigert tbAvvikInnPris tbAvvikPris ~
tbAvvikPrisVk 

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFrameHandle C-Win 
FUNCTION getFrameHandle RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getParentBrowse C-Win 
FUNCTION getParentBrowse RETURNS HANDLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setParentQuery C-Win 
FUNCTION setParentQuery RETURNS LOGICAL
  ( INPUT ihParentQuery AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setQuery C-Win 
FUNCTION setQuery RETURNS LOGICAL
  (INPUT ihQuery AS HANDLE)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE rsKorrigert AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Alle", 1,
"Korrigerte", 2,
"Ikke-korrigerte", 3
     SIZE 41.8 BY .95 NO-UNDO.

DEFINE RECTANGLE BrwPkSdlPris
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 151.4 BY 16.67.

DEFINE RECTANGLE PrisViewer
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 150.4 BY 6.29.

DEFINE RECTANGLE TBPkSdlPris
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 9.8 BY .91.

DEFINE VARIABLE tbAvvikInnPris AS LOGICAL INITIAL no 
     LABEL "Avvik varekost" 
     VIEW-AS TOGGLE-BOX
     SIZE 18.4 BY .81 NO-UNDO.

DEFINE VARIABLE tbAvvikPris AS LOGICAL INITIAL no 
     LABEL "Avvik utpris" 
     VIEW-AS TOGGLE-BOX
     SIZE 16.6 BY .81 NO-UNDO.

DEFINE VARIABLE tbAvvikPrisVk AS LOGICAL INITIAL no 
     LABEL "Avvik varekost eller utpris" 
     VIEW-AS TOGGLE-BOX
     SIZE 30.2 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     rsKorrigert AT ROW 1.33 COL 19.8 NO-LABEL
     tbAvvikInnPris AT ROW 1.38 COL 64.6
     tbAvvikPris AT ROW 1.38 COL 85
     tbAvvikPrisVk AT ROW 1.38 COL 102.8
     BrwPkSdlPris AT ROW 2.67 COL 1.6
     TBPkSdlPris AT ROW 1.24 COL 1.8
     PrisViewer AT ROW 19.57 COL 1.6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 153 BY 24.95.


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
         TITLE              = "<insert window title>"
         HEIGHT             = 24.95
         WIDTH              = 153
         MAX-HEIGHT         = 57.14
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 57.14
         VIRTUAL-WIDTH      = 320
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
       FRAME DEFAULT-FRAME:HEIGHT           = 24.95
       FRAME DEFAULT-FRAME:WIDTH            = 153.

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
ON END-ERROR OF C-Win /* <insert window title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <insert window title> */
DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rsKorrigert
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rsKorrigert C-Win
ON VALUE-CHANGED OF rsKorrigert IN FRAME DEFAULT-FRAME
DO:
  RUN SetQueryFilter(YES).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbAvvikInnPris
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbAvvikInnPris C-Win
ON VALUE-CHANGED OF tbAvvikInnPris IN FRAME DEFAULT-FRAME /* Avvik varekost */
DO:
  RUN SetQueryFilter(YES).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbAvvikPris
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbAvvikPris C-Win
ON VALUE-CHANGED OF tbAvvikPris IN FRAME DEFAULT-FRAME /* Avvik utpris */
DO:
  RUN SetQueryFilter(YES).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbAvvikPrisVk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbAvvikPrisVk C-Win
ON VALUE-CHANGED OF tbAvvikPrisVk IN FRAME DEFAULT-FRAME /* Avvik varekost eller utpris */
DO:
  RUN SetQueryFilter(YES).
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
  RUN disable_UI.
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

ON 'alt-s':U OF FRAME {&FRAME-NAME} ANYWHERE
  PUBLISH "AltSKundeOrdre" (THIS-PROCEDURE:FILE-NAME).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DefaultActionBrowse C-Win 
PROCEDURE DefaultActionBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
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
PUBLISH "CurrPrisFillIn" (?).

IF hBuffer:AVAIL THEN DO:
  IF hBuffer:BUFFER-FIELD("MottaksId"):BUFFER-VALUE = 0 THEN DO:
    IF DYNAMIC-FUNCTION("getLinkedObject",hNyInnkjopsPris,"browseoverlay","from") = ? THEN
      DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hNyInnkjopsPris,"NyInnkjopspris").
    IF DYNAMIC-FUNCTION("getLinkedObject",hNyRab1%,"browseoverlay","from") = ? THEN
      DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hNyRab1%,"NyRab1%").
    IF DYNAMIC-FUNCTION("getLinkedObject",hNyFrakt,"browseoverlay","from") = ? THEN
      DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hNyFrakt,"NyFrakt").
    IF DYNAMIC-FUNCTION("getLinkedObject",hNyPris,"browseoverlay","from") = ? THEN
      DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hNyPris,"NyPris").
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"disabledevents","").
  END.
  ELSE DO:      
    IF DYNAMIC-FUNCTION("getLinkedObject",hNyInnkjopsPris,"browse","from") NE ? THEN DO:
      DYNAMIC-FUNCTION("DeleteObjectLink",hNyInnkjopsPris,hBrowse).
      hNyInnkjopsPris:HIDDEN = YES.
    END.
    IF DYNAMIC-FUNCTION("getLinkedObject",hNyRab1%,"browse","from") NE ? THEN DO:
      DYNAMIC-FUNCTION("DeleteObjectLink",hNyRab1%,hBrowse).
      hNyRab1%:HIDDEN = YES.
    END.
    IF DYNAMIC-FUNCTION("getLinkedObject",hNyFrakt,"browse","from") NE ? THEN DO:
      DYNAMIC-FUNCTION("DeleteObjectLink",hNyFrakt,hBrowse).
      hNyFrakt:HIDDEN = YES.
    END.
    IF DYNAMIC-FUNCTION("getLinkedObject",hNyPris,"browse","from") NE ? THEN DO:
      DYNAMIC-FUNCTION("DeleteObjectLink",hNyPris,hBrowse).
      hNyPris:HIDDEN = YES.
    END.
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"disabledevents","utpris").
  END.
END.

RUN SUPER.
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
RUN ArtikkelKort IN hViewer.
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
  DISPLAY rsKorrigert tbAvvikInnPris tbAvvikPris tbAvvikPrisVk 
      WITH FRAME DEFAULT-FRAME.
  ENABLE BrwPkSdlPris TBPkSdlPris PrisViewer rsKorrigert tbAvvikInnPris 
         tbAvvikPris tbAvvikPrisVk 
      WITH FRAME DEFAULT-FRAME.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EnterBrowseFillIn C-Win 
PROCEDURE EnterBrowseFillIn :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN SUPER.
PUBLISH "CurrPrisFillIn" (DYNAMIC-FUNCTION("getCurrentObject")).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FinnStrekkode C-Win 
PROCEDURE FinnStrekkode :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icStrekkode AS CHAR NO-UNDO.

DEF VAR cArtNr AS CHAR NO-UNDO.

cArtNr = DYNAMIC-FUNCTION("getFieldValues","Strekkode","WHERE Kode = '" + icStrekkode + "'","ArtikkelNr").
IF cArtNr NE "" THEN DO:
  bOK = hBuffer:FIND-FIRST("WHERE ArtikkelNr = " + cArtNr) NO-ERROR.
  IF bOk THEN DO:
    hBrowse:QUERY:REPOSITION-TO-ROWID(hBuffer:ROWID) NO-ERROR.
    IF NOT ERROR-STATUS:ERROR THEN
      APPLY "value-changed" TO hBrowse.
  END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FraktPrisRecord C-Win 
PROCEDURE FraktPrisRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cValue    AS CHAR NO-UNDO.
DEF VAR oiReturn  AS INT  NO-UNDO.

RUN JBoxAskForValue.w ("Tillegg for fraktkostnad - fordeles ut fra sum innkjøpspris på artikler",
                        "DECIMAL|>>,>>9.99",
                        OUTPUT cValue,
                        OUTPUT iReturn).

IF iReturn = 2 THEN
  IF DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowse,"pksdl_linje_fraktfordeling.p",cValue) THEN DO:
    DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
    RUN OpenQuery.
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
DEF VAR cTabList    AS CHAR   NO-UNDO.
DEF VAR hPageObject AS HANDLE NO-UNDO.
DEF VAR iy          AS INT    NO-UNDO.
DEF VAR hTabFrame   AS HANDLE NO-UNDO.
DEF VAR cProfilnr   AS CHAR   NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:

  DYNAMIC-FUNCTION("NewObject",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE,"procedure").

  cProfilnr = DYNAMIC-FUNCTION("getFieldList","SysPara;,Butiker;ProfilNr",
                               "WHERE SysPara.SysHId = 5 AND SysPara.SysGr = 1 AND SysPara.ParaNr = 1"
                             + ",FIRST Butiker WHERE Butiker.Butik = INT(SysPara.Parameter1)"
                               ).

  bOverstyrKol = DYNAMIC-FUNCTION("getOverstyrKol" IN hParent).

  IF bOverstyrKol THEN
    tbAvvikPrisVk:HIDDEN = YES.
  ELSE
    ASSIGN rsKorrigert:HIDDEN = YES
           tbAvvikInnPris:HIDDEN = YES
           tbAvvikPris:HIDDEN = YES.

  hBrowse = DYNAMIC-FUNCTION("NewBrowse",
                           BrwPkSdlPris:HANDLE,      
                           500,                      
                           "multiple,!FIT-LAST-COLUMN",                       
                           "PkSdlPris"             
                           + ";ArtikkelNr|Art.nr"
                           + ";LevKod"
                           + ";Beskr"
                           + ";LevFargKod"

                           + ";NyInnkjopsPris|Innkj.pris"
                           + ";NyRab1%|Rab%"
                           + ";NyVarekost|Varekost"
                           + (IF bOverstyrKol THEN
                               ";NyFrakt|Frakt"
                              ELSE ";!NyFrakt|Frakt")
                           + ";NyPris|Pris"
                           + ";NyDB%|DB%"
                           + ";!PkSdlId"
                           + (IF bOverstyrKol THEN
                               ";InnkjopsPris|OrdreInnkj.pris"
                             + ";Rab1%|Ordrerab%"
                             + ";Frakt|Ordrefrakt"
                             + ";Varekost|Ordre varekost"
                             + ";Pris|Ordrepris"
                             + ";DB%|OrdreDB%"
  
                             + ";OverstyrPris|Korr.kalk|J/N"
                             + ";BrukerId"
                             + ";EDato"
                             + ";+EndrTid|CHARACTER|x(5)|jbhhmm_time(ETid)|Tid"
                             + ";!ETid"
                           + ",ArtPris"
                             + ";+GjInnkjopsPris|DECIMAL|>>><>>9.99|artpris_innkjopspris(ArtikkelNr)|Gj.innkj.pris@17"
                             + ";+GjRab1%|DECIMAL|>>><>>9.99|artpris_rab1%(ArtikkelNr)|Gj.rab%@18"
                             + ";+GjFrakt|DECIMAL|>>><>>9.99|artpris_frakt(ArtikkelNr)|Gj.frakt@19"
                             + ";+GjVarekost|DECIMAL|>>><>>9.99|artpris_varekost(ArtikkelNr)|Gj.varekost@20"
                             + ";+GjPris|DECIMAL|>>><>>9.99|artpris_pris(ArtikkelNr)|Gj.pris@21"
                             + ";+GjDB%|DECIMAL|>>><>>9.99|artpris_db%(ArtikkelNr)|Gj.DB%@22"
                             + ";Tilbud|Tilb@11"
                             ELSE 
                                ";!InnkjopsPris|OrdreInnkj.pris"
                              + ";!Varekost|Ordre varekost"
                            + ",ArtPris"
                              + ";!+GjPris|DECIMAL|>>><>>9.99|artpris_pris(ArtikkelNr)|Gj.pris@21"
                               )
                         + ",buf1_PkSdlPris"             
                           + ";+PksdlAvvikInnPris|LOGICAL|*/|pksdlpris_innprisavvik(ROWID)|Avvik varekost@5"
                           + ";+PksdlAvvikUtPris|LOGICAL|*/|pksdlpris_prisavvik(ROWID)|Avvik utpris@6"
                           + ";!+PksdlAvvikUtPrisVk|LOGICAL|*/|pksdlpris_avvik_pris_vk(ROWID)"
                         + ",PkSdlLinje"
                           + ";!MottaksId"
                          ,"WHERE false"
                         + ",FIRST ArtPris WHERE ArtPris.ArtikkelNr = PkSdlPris.ArtikkelNr AND ProfilNr = " + cProfilnr + " NO-LOCK OUTER-JOIN"
                         + ",FIRST buf1_PkSdlPris WHERE ROWID(buf1_PkSdlPris) = ROWID(PkSdlPris) NO-LOCK"
                         + ",FIRST PkSdlLinje WHERE PkSdlLinje.PkSdlId = PkSdlPris.PkSdlId AND PkSdlLinje.ArtikkelNr = PkSdlPris.ArtikkelNr AND PkSdlLinje.MottaksId > 0 NO-LOCK OUTER-JOIN"
                           ,"").         
  hBuffer = hBrowse:QUERY:GET-BUFFER-HANDLE(1).
  DYNAMIC-FUNCTION("createObjectLink",hBrowse,THIS-PROCEDURE).
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"sortmap","EndrTid;ETid").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcfieldproc","artpris_brwcalc.p").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"postUpdateProc","pksdlpris_post_update.p").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"getrecordcount","yes").
  

  DYNAMIC-FUNCTION("NewMenuBand",hBrowse
                  ,"MultiSortBrowse;Sorter på flere kolonner"
                 + ",UtPris;Sett samme utpris for flere artikler"
                  ,"").

  hNyInnkjopsPris = DYNAMIC-FUNCTION("NewBrowseFillIn",hBrowse,"NyInnkjopsPris","NyInnkjopsPris"
                                     ,"","","","").
  DYNAMIC-FUNCTION("setAttribute",hNyInnkjopspris,"refreshrow","yes").
                                     
  hNyRab1% = DYNAMIC-FUNCTION("NewBrowseFillIn",hBrowse,"NyRab1%","NyRab1%"
                              ,"","","","").
  DYNAMIC-FUNCTION("setAttribute",hNyRab1%,"refreshrow","yes").

/*   hNyRab1% = DYNAMIC-FUNCTION("NewBrowseToggle",hBrowse,"OverstyrPris","OverstyrPris",""). */
/*   DYNAMIC-FUNCTION("setAttribute",hNyRab1%,"refreshrow","yes").                            */
/*   hNyRab1%:TOOLTIP = "Oppdater pris og varekost".                                          */

  hNyFrakt   = DYNAMIC-FUNCTION("NewBrowseFillIn",hBrowse,"NyFrakt","NyFrakt"
                                  ,"","","","").
  DYNAMIC-FUNCTION("setAttribute",hNyFrakt,"refreshrow","yes").

  hNyPris       = DYNAMIC-FUNCTION("NewBrowseFillIn",hBrowse,"NyPris","NyPris"
                                  ,"","","","").
  DYNAMIC-FUNCTION("setAttribute",hNyPris,"refreshrow","yes").

  hToolbar = DYNAMIC-FUNCTION("NewToolbar",
                            TBPkSdlPris:HANDLE,
                            "",
                            "edit;Artikkelko&rt"
                           + ",excel"
                          + (IF DYNAMIC-FUNCTION("getOverstyrKol" IN hParent) THEN ",browseconfig" ELSE "")
/*                           + ",FraktPris;Legg inn fraktkostnad" */
                           ,"maxborder").
  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hToolbar).

  hViewer = DYNAMIC-FUNCTION("NewViewer",PrisViewer:HANDLE,hBrowse,"PkSdlPrisView.w").

  SUBSCRIBE TO "InvalidateHandle" IN hViewer.

  SUBSCRIBE TO "FinnStrekkode" IN hParent.

  IF NOT bOverstyrKol THEN DO:
    tbAvvikPrisVk:CHECKED = YES.
    RUN setQueryFilter (NO).
  END.

  DYNAMIC-FUNCTION("setAddMoveY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,"PrisViewer").
  DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,"PrisViewer").
END.

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
IF SOURCE-PROCEDURE = hViewer THEN
  APPLY "close" TO THIS-PROCEDURE.
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
DEF VAR hWidget AS HANDLE NO-UNDO.
hWidget = hBrowse:GET-BROWSE-COLUMN(1).
APPLY "end-resize" TO hWidget.
FRAME {&FRAME-NAME}:MOVE-TO-TOP().

RUN MoveToTop IN hViewer.

APPLY "entry" TO hBrowse.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nextRow C-Win 
PROCEDURE nextRow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF NOT hBrowse:IS-ROW-SELECTED(hBrowse:FOCUSED-ROW) THEN
  hBrowse:SELECT-ROW(hBrowse:FOCUSED-ROW).
IF hBrowse:SELECT-NEXT-ROW() THEN
  APPLY "value-changed" TO hBrowse.

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
  IF NOT hBrowse:IS-ROW-SELECTED(hBrowse:FOCUSED-ROW) THEN
    hBrowse:SELECT-ROW(hBrowse:FOCUSED-ROW).
  IF icDir = "Prev" THEN
    hBrowse:SELECT-PREV-ROW().
  ELSE
    hBrowse:SELECT-NEXT-ROW().
  APPLY "value-changed" TO hBrowse.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE prevRow C-Win 
PROCEDURE prevRow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF NOT hBrowse:IS-ROW-SELECTED(hBrowse:FOCUSED-ROW) THEN
  hBrowse:SELECT-ROW(hBrowse:FOCUSED-ROW).
IF hBrowse:SELECT-PREV-ROW() THEN
  APPLY "value-changed" TO hBrowse.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrisOppdatRecord C-Win 
PROCEDURE PrisOppdatRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cValue    AS CHAR NO-UNDO.
DEF VAR oiReturn  AS INT  NO-UNDO.

RUN JBoxBrowseMsgUpdSelVal.w ("Sett flagg for prisoppdatering",
                              hBrowse:NUM-SELECTED-ROWS,
                              0,
                              "LOGICAL|J/N|J",
                              OUTPUT cValue,
                              OUTPUT iReturn).

IF iReturn = 2 THEN
  IF DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowse,"pksdl_linje_prisflagg.p",cValue) THEN DO:
    DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
    RUN OpenQuery.
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

/* ASSIGN hTilbudCol:FONT         = iFontWingdings              */
/*        hTilbudCol:FORMAT       = CHR(254) + "/"  + CHR(168)  */
/*        hNyRab1%Col:FONT   = iFontWingdings              */
/*        hNyRab1%Col:FORMAT = CHR(254) + "/"  + CHR(168). */

/* IF hBuffer:BUFFER-FIELD("Tilbud"):BUFFER-VALUE THEN                                                                   */
/*   hTilbudCol:BGCOLOR = 12.                                                                                            */
/* IF hBuffer:BUFFER-FIELD("InnkjopsPris"):BUFFER-VALUE > hBuffer:BUFFER-FIELD("GjInnkjopsPris"):BUFFER-VALUE THEN       */
/*   hNyInnkjopsPrisCol:BGCOLOR = 17.                                                                                      */
/* ELSE IF hBuffer:BUFFER-FIELD("InnkjopsPris"):BUFFER-VALUE < hBuffer:BUFFER-FIELD("GjInnkjopsPris"):BUFFER-VALUE THEN  */
/*   hNyInnkjopsPrisCol:BGCOLOR = 18.                                                                                      */

IF hBuffer:BUFFER-FIELD("NyVarekost"):BUFFER-VALUE > hBuffer:BUFFER-FIELD("Varekost"):BUFFER-VALUE THEN
  hInnPrisAvvikCol:BGCOLOR = 17.
ELSE IF hBuffer:BUFFER-FIELD("NyVarekost"):BUFFER-VALUE < hBuffer:BUFFER-FIELD("Varekost"):BUFFER-VALUE THEN
  hInnPrisAvvikCol:BGCOLOR = 18.
ELSE
  hInnPrisAvvikCol:BGCOLOR = ?.

IF hBuffer:BUFFER-FIELD("NyPris"):BUFFER-VALUE > hBuffer:BUFFER-FIELD("GjPris"):BUFFER-VALUE THEN
  hUtPrisAvvikCol:BGCOLOR = 17.
ELSE IF hBuffer:BUFFER-FIELD("NyPris"):BUFFER-VALUE < hBuffer:BUFFER-FIELD("GjPris"):BUFFER-VALUE THEN
  hUtPrisAvvikCol:BGCOLOR = 18.
ELSE
  hUtPrisAvvikCol:BGCOLOR = ?.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setQueryFilter C-Win 
PROCEDURE setQueryFilter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ibExecute AS LOG NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  ASSIGN rsKorrigert.
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"queryfilter", 
                   (IF rsKorrigert = 2 THEN
                     " AND PkSdlPris.OverstyrPris"
                    ELSE IF rsKorrigert = 3 THEN
                      " AND NOT PkSdlPris.OverstyrPris"
                    ELSE "")
                   ).

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcparamPksdlAvvikUtPris",
                   IF tbAvvikPris:CHECKED THEN "true" ELSE "false").

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcparamPksdlAvvikInnPris",
                   IF tbAvvikInnPris:CHECKED THEN "true" ELSE "false").

 DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcparamPksdlAvvikUtPrisVk",
                  IF tbAvvikPrisVk:CHECKED THEN "true" ELSE "false").

  IF ibExecute THEN DO:
    DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
    RUN OpenQuery.
  END.

END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UtprisRecord C-Win 
PROCEDURE UtprisRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iReturn AS INT  NO-UNDO.
DEF VAR ocValue AS CHAR NO-UNDO.

RUN JBoxBrowseMsgUpdateVal.w ("Sett utpris for artikler",
                              hBrowse:NUM-SELECTED-ROWS,
                              DYNAMIC-FUNCTION("getAttribute",hBrowse,"recordcount"),
                              "DECIMAL|->>>,>>9.99|",
                              OUTPUT ocValue, 
                              OUTPUT iReturn).

IF iReturn = 2 THEN 
  DYNAMIC-FUNCTION("processSelectedRows",hBrowse,"pksdlpris_sett_pris.p",ocValue).
ELSE IF iReturn = 1 THEN DO:
  IF DYNAMIC-FUNCTION("processQuery",hBrowse,"pksdlpris_sett_pris.p",ocValue) THEN 
    RUN InvokeMethod(hBrowse,"OpenQuery").
END.
ELSE RETURN.

RUN InvokeMethod(hBrowse,"DisplayRecord").

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

APPLY "value-changed" TO hBrowse.
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
DEF VAR hColumn AS HANDLE NO-UNDO.
IF bOverstyrKol THEN
  ASSIGN ihBrowse:GET-BROWSE-COLUMN(1):WIDTH-PIXELS = 40
         ihBrowse:GET-BROWSE-COLUMN(2):WIDTH-PIXELS = 30
         ihBrowse:GET-BROWSE-COLUMN(3):WIDTH-PIXELS = 50
         ihBrowse:GET-BROWSE-COLUMN(4):WIDTH-PIXELS = 40
         ihBrowse:GET-BROWSE-COLUMN(5):WIDTH-PIXELS = 60
         ihBrowse:GET-BROWSE-COLUMN(6):WIDTH-PIXELS = 60
         ihBrowse:GET-BROWSE-COLUMN(5):COLUMN-FONT  = 12
         ihBrowse:GET-BROWSE-COLUMN(6):COLUMN-FONT  = 12
         hTilbudCol                                 = ihBrowse:GET-BROWSE-COLUMN(11)
         hNyInnkjopsPrisCol                         = ihBrowse:GET-BROWSE-COLUMN(7)
         .
ELSE
  ASSIGN ihBrowse:GET-BROWSE-COLUMN(1):WIDTH-PIXELS = ihBrowse:GET-BROWSE-COLUMN(1):WIDTH-PIXELS - 20
         ihBrowse:GET-BROWSE-COLUMN(2):WIDTH-PIXELS = ihBrowse:GET-BROWSE-COLUMN(2):WIDTH-PIXELS - 20
         ihBrowse:GET-BROWSE-COLUMN(3):WIDTH-PIXELS = ihBrowse:GET-BROWSE-COLUMN(3):WIDTH-PIXELS - 20
         ihBrowse:GET-BROWSE-COLUMN(4):WIDTH-PIXELS = ihBrowse:GET-BROWSE-COLUMN(4):WIDTH-PIXELS - 20
         ihBrowse:GET-BROWSE-COLUMN(5):WIDTH-PIXELS = ihBrowse:GET-BROWSE-COLUMN(5):WIDTH-PIXELS - 20
         ihBrowse:GET-BROWSE-COLUMN(6):WIDTH-PIXELS = ihBrowse:GET-BROWSE-COLUMN(6):WIDTH-PIXELS - 20
         .
ASSIGN hColumn = DYNAMIC-FUNCTION("getBrowseColumn",ihBrowse,"NyPris") 
       hColumn:WIDTH-PIXELS = 80
       hInnPrisAvvikCol = DYNAMIC-FUNCTION("getBrowseColumn",ihBrowse,"PksdlAvvikInnPris")
       hUtPrisAvvikCol = DYNAMIC-FUNCTION("getBrowseColumn",ihBrowse,"PksdlAvvikUtPris")
       hNyRab1%Col = DYNAMIC-FUNCTION("getBrowseColumn",ihBrowse,"NyRab1%")
       .

RETURN YES.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION InitializeResize C-Win 
FUNCTION InitializeResize RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DYNAMIC-FUNCTION("setAddMoveY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,"PrisViewer").
DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,"PrisViewer").
DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,"PrisViewer").

RETURN YES.

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

  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setParentQuery C-Win 
FUNCTION setParentQuery RETURNS LOGICAL
  ( INPUT ihParentQuery AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
hParentBrowse = ihParentQuery.
hParentBuffer = hParentBrowse:QUERY:GET-BUFFER-HANDLE(1).
  
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setQuery C-Win 
FUNCTION setQuery RETURNS LOGICAL
  (INPUT ihQuery AS HANDLE) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
ASSIGN hParentBrowse = ihQuery
       hParentBuffer = ihQuery:QUERY:GET-BUFFER-HANDLE(1).
RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

