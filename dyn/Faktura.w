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
DEF VAR hParent           AS HANDLE NO-UNDO.

DEF VAR hParentBuffer     AS HANDLE NO-UNDO.
DEF VAR hParentBrowse     AS HANDLE NO-UNDO.
DEF VAR hBrowse           AS HANDLE NO-UNDO.
DEF VAR hFieldMap         AS HANDLE NO-UNDO.
DEF VAR hToolbar          AS HANDLE NO-UNDO.
DEF VAR hTabFolder        AS HANDLE NO-UNDO.

DEF VAR hCurrTabProc      AS HANDLE NO-UNDO.
DEF VAR hCurrTabFrame     AS HANDLE NO-UNDO.
DEF VAR iCurrTab          AS INT NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rectFaktHode rectTBfaktHode rectFolder ~
searchFieldFakt cbBilagstype FakturaNr FaktAdresse1 ForfallsDato ~
FaktAdresse2 FakturertDato VaarRef FaktPostNr FaktPostSted LeveringsDato ~
DeresRef FaktLand Telefon Referanse KID 
&Scoped-Define DISPLAYED-OBJECTS cbBilagstype FakturaNr FaktAdresse1 ~
ForfallsDato FaktAdresse2 FakturertDato VaarRef FaktPostNr FaktPostSted ~
LeveringsDato DeresRef FaktLand Telefon Referanse KID 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setFilter C-Win 
FUNCTION setFilter RETURNS LOGICAL
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
  (INPUT ihQuery AS HANDLE)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD TabChanged C-Win 
FUNCTION TabChanged RETURNS LOGICAL
  ( INPUT iiTab AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE cbBilagstype AS CHARACTER FORMAT "X(256)":U 
     LABEL "Type" 
     VIEW-AS COMBO-BOX INNER-LINES 20
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE DeresRef AS CHARACTER FORMAT "X(30)" 
     LABEL "Deres ref" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.

DEFINE VARIABLE FaktAdresse1 AS CHARACTER FORMAT "X(30)" 
     LABEL "Fakt.adresse" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.

DEFINE VARIABLE FaktAdresse2 AS CHARACTER FORMAT "X(30)" 
     LABEL "Fakt.Adresse" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.

DEFINE VARIABLE FaktLand AS CHARACTER FORMAT "X(30)" 
     LABEL "Land" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.

DEFINE VARIABLE FaktPostNr AS CHARACTER FORMAT "X(15)" 
     LABEL "Postnr" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1.

DEFINE VARIABLE FaktPostSted AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE FakturaNr AS DECIMAL FORMAT ">>>>>>>>>>>>9" INITIAL ? 
     LABEL "Fakturanr" 
     VIEW-AS FILL-IN 
     SIZE 20.2 BY 1.

DEFINE VARIABLE FakturertDato AS DATE FORMAT "99/99/9999":U 
     LABEL "Fakturadato" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE ForfallsDato AS DATE FORMAT "99/99/9999":U 
     LABEL "Forfallsdato" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE KID AS DECIMAL FORMAT ">>>>>>>>>>>>>>>>>>>>>>>>9" INITIAL 0 
     LABEL "KID" 
     VIEW-AS FILL-IN 
     SIZE 37 BY 1 TOOLTIP "KID nr.".

DEFINE VARIABLE LeveringsDato AS DATE FORMAT "99/99/9999":U 
     LABEL "Lev.dato" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE Referanse AS CHARACTER FORMAT "X(30)" 
     LABEL "Referanse" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1 TOOLTIP "Evt. bestillingsnummer".

DEFINE VARIABLE Telefon AS CHARACTER FORMAT "X(15)" 
     LABEL "Telefon" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE VaarRef AS CHARACTER FORMAT "X(30)" 
     LABEL "Vår ref" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.

DEFINE RECTANGLE rectFaktHode
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 141.4 BY 9.29.

DEFINE RECTANGLE rectFolder
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 142 BY 7.62.

DEFINE RECTANGLE rectTBfaktHode
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 9.8 BY .91.

DEFINE RECTANGLE searchFieldFakt
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 19 BY .95.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     cbBilagstype AT ROW 1.29 COL 75 COLON-ALIGNED
     FakturaNr AT ROW 12.1 COL 16 COLON-ALIGNED HELP
          "Fakturanummer"
     FaktAdresse1 AT ROW 12.1 COL 67 COLON-ALIGNED HELP
          "Fakturaadresse"
     ForfallsDato AT ROW 12.1 COL 123 COLON-ALIGNED
     FaktAdresse2 AT ROW 13.1 COL 67 COLON-ALIGNED HELP
          "Fakturaadresse"
     FakturertDato AT ROW 13.14 COL 123 COLON-ALIGNED
     VaarRef AT ROW 13.19 COL 16 COLON-ALIGNED HELP
          "Vår referanse"
     FaktPostNr AT ROW 14.14 COL 67 COLON-ALIGNED HELP
          "Postnr. fakturaadresse."
     FaktPostSted AT ROW 14.14 COL 82.4 COLON-ALIGNED NO-LABEL
     LeveringsDato AT ROW 14.19 COL 123 COLON-ALIGNED
     DeresRef AT ROW 14.24 COL 16 COLON-ALIGNED HELP
          "Deres referanse"
     FaktLand AT ROW 15.24 COL 67 COLON-ALIGNED HELP
          "Land"
     Telefon AT ROW 15.38 COL 16 COLON-ALIGNED HELP
          "Telefon"
     Referanse AT ROW 16.48 COL 16 COLON-ALIGNED HELP
          "Referanse til kundens ordre id."
     KID AT ROW 16.48 COL 67 COLON-ALIGNED HELP
          "KID"
     rectFaktHode AT ROW 2.67 COL 1.6
     rectTBfaktHode AT ROW 1.24 COL 1.8
     rectFolder AT ROW 18.14 COL 2
     searchFieldFakt AT ROW 1.29 COL 18
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 143.6 BY 24.95.


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
         WIDTH              = 143.8
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
       FRAME DEFAULT-FRAME:WIDTH            = 143.6.

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
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbBilagstype
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbBilagstype C-Win
ON VALUE-CHANGED OF cbBilagstype IN FRAME DEFAULT-FRAME /* Type */
DO:
  setFilter().
  DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
  RUN OpenQuery.
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
{incl/supptrigg.i hFieldMap}

ON 'alt-s':U OF FRAME {&FRAME-NAME} ANYWHERE
  PUBLISH "AltSKundeOrdre" (THIS-PROCEDURE:FILE-NAME).

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
  /* Hide all frames. */
  HIDE FRAME DEFAULT-FRAME.
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
  DISPLAY cbBilagstype FakturaNr FaktAdresse1 ForfallsDato FaktAdresse2 
          FakturertDato VaarRef FaktPostNr FaktPostSted LeveringsDato DeresRef 
          FaktLand Telefon Referanse KID 
      WITH FRAME DEFAULT-FRAME.
  ENABLE rectFaktHode rectTBfaktHode rectFolder searchFieldFakt cbBilagstype 
         FakturaNr FaktAdresse1 ForfallsDato FaktAdresse2 FakturertDato VaarRef 
         FaktPostNr FaktPostSted LeveringsDato DeresRef FaktLand Telefon 
         Referanse KID 
      WITH FRAME DEFAULT-FRAME.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
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
DEF VAR cTabList     AS CHAR   NO-UNDO.
DEF VAR hPageObject  AS HANDLE NO-UNDO.
DEF VAR iy           AS INT    NO-UNDO.
DEF VAR hTabFrame    AS HANDLE NO-UNDO.
DEF VAR hPageBrowse  AS HANDLE NO-UNDO.
DEF VAR hSearchField AS HANDLE NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:

  DYNAMIC-FUNCTION("NewObject",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE,"procedure").

  ASSIGN cbBilagsType:DELIMITER = "|"
         cbBilagsType:LIST-ITEM-PAIRS = "||" 
                   + DYNAMIC-FUNCTION("getFieldList","BilagsType;BTTekst|BilagsType;BilagsType",
                                      "where Bruk = 1 OR Bruk = 3")
         cbBilagsType:SCREEN-VALUE = cbBilagsType:ENTRY(1)
         .

  /* Create the browse: */
   hBrowse = DYNAMIC-FUNCTION("NewBrowse",
                           rectFaktHode:HANDLE,        /* Coordinates */
                           100,                      /* Batchsize */
                           "MULTIPLE",                /* Attributes that must be set at creation time for the browse */
                           "FakturaHode"              /* Buffers and fields (and calculated fields) for browse */
                           + ";Faktura_id"
                           + ";ButikkNr|ButNr"
                           + ";BilagsType|BilT"
                           + ";BTTekst"
                           + ";Dato|Salgsdato"
                           + ";Butikksalg|BS|*/"
                           + ";FakturaNr"
                           + ";FakturertDato"
                           + ";ForfallsDato" 
                           + ";Totalt|Sum ink.mva"
                           + ";AvgFriSalg|Avgfri"
                           + ";AvgPlSalg|Avgpl"
                           + ";Nettopris|Netto"
                           + ";AvrundingKr"
                           + ";TotalRabattKr|Rabatt"
                           + ";TotalRabatt%|Rabatt%"
                           + ";MvaKr"
                           + ";KID"
                           + ";VaarRef"
                           + ";DeresRef"
                           + ";Referanse"
                           + ";LeveringsDato"
                           + ";Telefon"
                           + ";FaktAdresse1"
                           + ";FaktAdresse2"
                           + ";FaktLand"
                           + ";FaktPostNr"
                           + ";FaktPoststed"
                           + ";!KundeNr"
                           ,"WHERE false"
                           ,"sort|Faktura_id desc").         
  DYNAMIC-FUNCTION("createObjectLink",hBrowse,THIS-PROCEDURE).
  hBrowse:NAME = "brwFaktHode".
  hBrowse:GET-BROWSE-COLUMN(2):WIDTH-PIXELS = 30.
  hBrowse:GET-BROWSE-COLUMN(3):WIDTH-PIXELS = 80.
  hBrowse:GET-BROWSE-COLUMN(5):WIDTH-PIXELS = 30.

  hSearchField = DYNAMIC-FUNCTION("NewBrowseSearchField",SearchFieldFakt:HANDLE,hBrowse,1).
  DYNAMIC-FUNCTION("CreateObjectLink",hSearchField,hBrowse).

  hFieldMap = DYNAMIC-FUNCTION("NewFieldMap",
                           hBrowse:QUERY,
                           FRAME {&FRAME-NAME}:HANDLE,
                           "","",
                           "Fakturanr,ForfallsDato,FaktAdresse1,FakturertDato,FaktAdresse2,FaktPostNr,LeveringsDato,FaktPostSted,FaktLand,VaarRef,DeresRef,Telefon,Referanse,KID","",
                           "cEventTime,btnCalFakturertDato").
  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"bufferextrafields","GuestObj,EventTime").
  DYNAMIC-FUNCTION("CreateObjectLink",hFieldMap,hBrowse).

  hToolbar = DYNAMIC-FUNCTION("NewToolbar",
                            rectTBfaktHode:HANDLE,
                            "",
/*                             "Produce;Produser faktura;Produser faktura;ProduserFaktura;bmp/act16e.bmp" */
                            "PrintPreview;Forhvisn;Forhåndsvisning;PrintPreviewRecord;gif/afprintpre.gif"
                          + ",Print;Utskrift"
                          + ",Oppfrisk;Oppdater fakturainformasjon,excel",
                            "maxborder").
  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hToolbar).
  DYNAMIC-FUNCTION("CreateObjectLink",hFieldMap,hToolbar).



  hTabFolder = DYNAMIC-FUNCTION("NewTabFolder",rectFolder:HANDLE).
  
  DYNAMIC-FUNCTION("InitPages" IN hTabFolder,"Linjer|FakturaLinje.w",hBrowse).
  
  DYNAMIC-FUNCTION("buildFolder" IN hTabFolder).

  DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,FRAME {&FRAME-NAME}:HANDLE,"rectTBfaktHode,rectFaktHode,brwFaktHode").
  DYNAMIC-FUNCTION("setAnchor",THIS-PROCEDURE:CURRENT-WINDOW,hBrowse,
                    STRING(ForfallsDato:HANDLE) + "," 
                  + STRING(FakturertDato:HANDLE) + ","
                  + STRING(LeveringsDato:HANDLE) 
                   ,"").

END.

DYNAMIC-FUNCTION("TabFolderChanged" IN hTabFolder,1).

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


/* hWidget = DYNAMIC-FUNCTION("getTabFrame" IN hTabFolder).  */
/* hWidget:MOVE-TO-TOP().                                    */
DYNAMIC-FUNCTION("MoveTabToTop" IN hTabFolder,rectFolder:HANDLE IN FRAME {&FRAME-NAME}).

/* RUN MoveToTop IN hCurrTabProc NO-ERROR. */
/* IF ERROR-STATUS:ERROR THEN              */
/*   hCurrTabFrame:MOVE-TO-TOP() NO-ERROR. */

APPLY "entry" TO hBrowse.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppfriskRecord C-Win 
PROCEDURE OppfriskRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cIdList     AS CHAR NO-UNDO.
DEF VAR cCurrRowId1 AS CHAR NO-UNDO.
DEF VAR iReposRow   AS INT  NO-UNDO.

IF hBrowse:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN
  ASSIGN cCurrRowid1 = hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("RowIdent1"):BUFFER-VALUE
         iReposRow   = hBrowse:FOCUSED-ROW.

DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
  IF hBrowse:FETCH-SELECTED-ROW(ix) THEN
    ASSIGN 
      cIdList = cIdList + STRING(hFieldMap:BUFFER-FIELD("Faktura_id"):BUFFER-VALUE) + ",".
END.

IF cIdList NE "" THEN DO:
  cIdList = TRIM(cIdList,",").
  IF NOT DYNAMIC-FUNCTION("runproc","faktura_oppfrisk.p","idlist|" + cIdList,?) THEN 
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
  ELSE DO:
    RUN RefreshCurrentRow IN hParent NO-ERROR.
    /*DYNAMIC-FUNCTION("RefreshRowids",hBrowse,hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("RowIdent1"):BUFFER-VALUE).*/

    IF cCurrRowId1 NE "" THEN DO:
      bOk = hBrowse:QUERY:GET-BUFFER-HANDLE(1):FIND-FIRST("WHERE Faktura_id = '" + entry(1,cIdList) + "'") NO-ERROR.
      IF bOk THEN DO:
        hBrowse:SET-REPOSITIONED-ROW(iReposRow,"conditional").
        hBrowse:QUERY:REPOSITION-TO-ROWID(hBrowse:QUERY:GET-BUFFER-HANDLE(1):ROWID) NO-ERROR.

    /* Jeg ser her at jeg har tak i rikti rad og at iReposRow peker på riktig rad. Men ikke fa......n
    MESSAGE 'bOk' bOk SKIP
      'entry(1,cIdList)' entry(1,cIdList) SKIP
      'cCurrRowid1' cCurrRowid1 SKIP
      'iReposRow' iReposRow
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
    */
      END.
    END.

  END.
END.
ELSE DYNAMIC-FUNCTION("DoMessage",0,0,"Ingen faktura er valgt","","").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrintPreviewRecord C-Win 
PROCEDURE PrintPreviewRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cIdList        AS CHAR NO-UNDO.
DEF VAR cFakturaLayout AS CHAR NO-UNDO.
DEF VAR cButikkNr      AS CHAR NO-UNDO.

DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
  IF hBrowse:FETCH-SELECTED-ROW(ix) THEN 
    cIdList = cIdList + STRING(hFieldMap:BUFFER-FIELD("Faktura_id"):BUFFER-VALUE) + ",".
END.
IF cIdList NE "" THEN DO:
  cIdList = TRIM(cIdList,",").

  cButikkNr      = DYNAMIC-FUNCTION("getFieldValues","FakturaHode","WHERE 
                   Faktura_Id = " + cIdList,"ButikkNr").
  cFakturaLayout = DYNAMIC-FUNCTION("getFieldValues","Butiker","WHERE 
                   Butik = " + cButikkNr,"FakturaLayout").
  IF TRIM(cFakturaLayout) = '' OR cFakturaLayout = ? 
    THEN cFakturaLayout = '1'.

/*   IF DYNAMIC-FUNCTION("runproc","faktura_produksjon.p","idlist|" + cIdList,?) THEN DO:  */
/*     IF NUM-ENTRIES(cIdList) > 1 THEN                                                             */
/*       RUN skrivfaktura.p ("|WHERE CAN-DO('" + cIdList + "',STRING(Faktura_id))",FALSE,"setup").  */
/*     ELSE IF cIdList NE "" THEN                                                                   */
    RUN skrivfaktura.p (cIdList + "|",
                        FALSE,"",
                        1,
                        DYNAMIC-FUNCTION("getFieldValues","Kunde","WHERE 
                                         KundeNr = " + STRING(hFieldMap:BUFFER-FIELD("KundeNr"):BUFFER-VALUE),"ePostAdresse"),
                        INT(cFakturaLayout)). 
    
/*   RUN RefreshCurrentRow IN hParent NO-ERROR.*/
/*   END.                                                                                 */
/*   ELSE                                                                                 */
/*     DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"",""). */
END.
ELSE DYNAMIC-FUNCTION("DoMessage",0,0,"Ingen faktura er valgt","","").

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
DEF VAR cIdList  AS CHAR NO-UNDO.
DEF VAR cPrinter AS CHAR NO-UNDO.
DEF VAR iAntEks  AS INT  NO-UNDO.
DEF VAR iFormat  AS INT  NO-UNDO INIT 1.

DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
  IF hBrowse:FETCH-SELECTED-ROW(ix) THEN 
    cIdList = cIdList + STRING(hFieldMap:BUFFER-FIELD("Faktura_id"):BUFFER-VALUE) + ",".
END.
IF cIdList NE "" THEN DO:
  cIdList = TRIM(cIdList,",").
  IF DYNAMIC-FUNCTION("runproc","faktura_produksjon.p","idlist|" + cIdList,?) THEN DO:
    RUN DSelectPrinter.w (INPUT-OUTPUT cPrinter,INPUT-OUTPUT iAntEks,INPUT-OUTPUT iFormat,"FakturaSkriver,FakturaKopi",OUTPUT bOk).
    IF bOk THEN DO:
/*     IF NUM-ENTRIES(cIdList) > 1 THEN                                                             */
/*       RUN skrivfaktura.p ("|WHERE CAN-DO('" + cIdList + "',STRING(Faktura_id))",FALSE,"setup").  */
/*     ELSE IF cIdList NE "" THEN                                                                   */
        
      RUN skrivfaktura.p (cIdList + "|",TRUE,cPrinter,iAntEks,"",iFormat). 

      RUN RefreshCurrentRow IN hParent NO-ERROR.
      
      DYNAMIC-FUNCTION("RefreshRowids",hBrowse,hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("RowIdent1"):BUFFER-VALUE).
    END.
  END.
  ELSE
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
END.
ELSE DYNAMIC-FUNCTION("DoMessage",0,0,"Ingen faktura er valgt","","").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProduserFaktura C-Win 
PROCEDURE ProduserFaktura :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cIdList AS CHAR NO-UNDO.
DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
  IF hBrowse:SELECT-ROW(ix) THEN
    cIdList = cIdList + STRING(hFieldMap:BUFFER-FIELD("Faktura_id"):BUFFER-VALUE) + ",".
END.
IF cIdList NE "" THEN DO:
  cIdList = TRIM(cIdList,",").
  IF NOT DYNAMIC-FUNCTION("runproc","faktura_produksjon.p","idlist|" + cIdList,?) THEN 
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
  ELSE DO:
    DYNAMIC-FUNCTION("setCurrentObject",hParentBrowse).
    RUN DisplayRecord.
  END.
END.
ELSE DYNAMIC-FUNCTION("DoMessage",0,0,"Ingen faktura er valgt","","").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setSearch C-Win 
PROCEDURE setSearch :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icFaktura_id AS CHAR NO-UNDO.

bOk = hFieldMap:FIND-FIRST("WHERE Faktura_id = " + icFaktura_id) NO-ERROR.
IF NOT bOK THEN DO:
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"queryfilter","").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"querywhere",
                   (IF DYNAMIC-FUNCTION("getAttribute",hBrowse,"baseQuery") NE "" THEN " AND" ELSE "WHERE") 
                 + " Faktura_id = " + icFaktura_id).
  RUN OpenQuery.
END.
ELSE DO:
  hBrowse:QUERY:REPOSITION-TO-ROWID(hFieldMap:ROWID).
  RUN DisplayRecord.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

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
DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,FRAME {&FRAME-NAME}:HANDLE,"rectFaktHode").
DYNAMIC-FUNCTION("setNoResizeX",THIS-PROCEDURE:CURRENT-WINDOW,FRAME {&FRAME-NAME}:HANDLE,"searchFieldFakt").

RETURN TRUE. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setFilter C-Win 
FUNCTION setFilter RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  IF cbBilagstype:SCREEN-VALUE NE "" AND cbBilagstype:SCREEN-VALUE NE ? THEN
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"queryfilter",
                     (IF DYNAMIC-FUNCTION("getAttribute",hBrowse,"basequery") = "" THEN "WHERE " ELSE " AND ") +
                     "BilagsType = " + cbBilagstype:SCREEN-VALUE).
  ELSE 
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"queryfilter","").
END.
 
RETURN TRUE.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION TabChanged C-Win 
FUNCTION TabChanged RETURNS LOGICAL
  ( INPUT iiTab AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF iCurrTab > 0 THEN
  DYNAMIC-FUNCTION("DeleteObjectLink",hBrowse,DYNAMIC-FUNCTION("getPageQuery" IN hTabFolder,iCurrTab)).

DYNAMIC-FUNCTION("setAttribute",hToolbar,"disabledevents","").
DYNAMIC-FUNCTION("CreateParentLink",DYNAMIC-FUNCTION("getPageQuery" IN hTabFolder,iiTab),hBrowse,"Faktura_id").

hCurrTabProc  = DYNAMIC-FUNCTION("getPageHandle" IN hTabFolder,iiTab).
hCurrTabFrame = DYNAMIC-FUNCTION("getPageFrame" IN hTabFolder,iiTab).

RUN MoveToTop.

DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
RUN DisplayRecord.

iCurrTab = iiTab.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

