&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
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
DEFINE VARIABLE iWidthPix  AS INTEGER NO-UNDO.
DEFINE VARIABLE iHeightPix AS INTEGER NO-UNDO.
DEFINE VARIABLE hdlParent  AS HANDLE  NO-UNDO. 
DEFINE VARIABLE hToolbar   AS HANDLE  NO-UNDO.

DEFINE VARIABLE gSysHid AS INT INIT 51 NO-UNDO. 
DEFINE VARIABLE gcVPILeverandorNr AS CHAR NO-UNDO. 

DEFINE VARIABLE fldSysParaNr          AS INT INIT ? NO-UNDO.
DEFINE VARIABLE fldSysParaBeskrivelse AS CHAR NO-UNDO. 
DEFINE VARIABLE fldSysParaHjelpeTekst AS CHAR NO-UNDO.
DEFINE VARIABLE fldSysParaParameter1  AS CHAR NO-UNDO.
DEFINE VARIABLE fldSysParaParameter2  AS CHAR NO-UNDO.

DEFINE VARIABLE cDelimiter AS CHAR INIT "," NO-UNDO. 

cDelimiter = CHR(2). 
hdlParent = SOURCE-PROCEDURE. 


DEFINE TEMP-TABLE TT_Prisfil NO-UNDO
        FIELD Radnr           AS INTE FORMAT ">>>>9"  LABEL "Radnr"
        FIELD lNy             AS LOG  FORMAT "J/"     LABEL "Ny"
        FIELD cTandem         AS CHAR FORMAT "x(15)"  LABEL "Tandem"
        FIELD cStatus         AS CHAR FORMAT "x(4)"  LABEL "Anm."
        FIELD artbas_levnr    AS INTE FORMAT ">>>>>>>>>>>9" LABEL "Levnr"
        FIELD artbas_artnr    AS DECI FORMAT ">>>>>>>>>>>>9" LABEL "Varunr"
        FIELD strekkode_kode  AS CHAR FORMAT "x(15)" LABEL "EAN"
        FIELD pris_bestnr     AS INTE FORMAT ">>>>>>>9"      LABEL "Bestnr"
        FIELD vare_varetekst  AS CHAR FORMAT "x(40)"         LABEL "Varutext"
        FIELD vare_mengde     AS DECI FORMAT ">>,>>9.999"    LABEL "Mängd"
        FIELD vare_enhet      AS INTE FORMAT ">9"            LABEL "Enhet"
        FIELD vare_Cenhet     AS CHAR FORMAT "x(4)" LABEL  "Text" /* = 1 THEN "st" ELSE IF vare.enhet = 2 THEN "kg" ELSE IF vare.enhet = 3 THEN "l" ELSE "m") */
        FIELD vare_hgr        AS INTE FORMAT ">>>9" LABEL "Varugr"
        FIELD pris_engrosn    AS DECI FORMAT ">>,>>9.99"   LABEL "Inpris"
        FIELD rabatt          AS DECI FORMAT "9"           LABEL "Rab"
        FIELD pris_utprisn    AS DECI FORMAT ">>,>>9.99"   LABEL "Utpris"
        FIELD dbpris_utprisn  AS DECI FORMAT ">>,>>9.99" 
        FIELD pris_veilpris   AS DECI FORMAT ">>,>>9.99"   LABEL "Riktpris"
        FIELD vare_mva        AS DECI FORMAT ">9.99"       LABEL "Moms%"
        FIELD vare_mvagr      AS INTE FORMAT "9" LABEL "Momsgr" /* konv vid inläsning */
        FIELD vare_antpkn     AS DECI FORMAT ">,>>9.999" LABEL "Förp"
        FIELD vare_link       AS DECI FORMAT ">>>>>>>>>>>>9" LABEL "Link"
        FIELD Vare_Produsent  AS CHAR FORMAT "x(40)" LABEL "Producent"
        FIELD ProdNr          AS INT  FORMAT ">>>>>>9" LABEL "ProdNr"
        FIELD Vare_Varemerke  AS CHAR FORMAT "x(40)" LABEL "Varumärke"
        FIELD VmId            AS INT  FORMAT ">>>>>9" LABEL "VmId"
        FIELD Butikk_ButNr    AS INT  FORMAT ">>>>>9" LABEL "KundNr"
        FIELD ButNamn         AS CHAR FORMAT "x(30)" LABEL "ButNamn"
        FIELD Butik           AS INT  FORMAT ">>>>>9" LABEL "Butikk"
        FIELD fil_tbproc      AS DECI FORMAT "->>9.99" LABEL "TB%"
        FIELD hgrprof_brutto% AS DECI FORMAT "->>9.99" LABEL "VG TB%"
        FIELD orgdata         AS CHAR 
        FIELD cDummy          AS CHAR FORMAT "x(2)" LABEL ""
        INDEX Radnr IS PRIMARY UNIQUE Radnr
        INDEX Butik Butik vare_varetekst pris_bestnr.

 DEFINE TEMP-TABLE ttImportFields NO-UNDO
         FIELD FieldNumber AS INT FORMAT "99" LABEL "Felt Nr." 
         FIELD FieldLabel AS CHAR FORMAT "X(30)" LABEL "Name"
         FIELD FieldName AS CHAR FORMAT "X(30)" LABEL "Kolonne" 
         FIELD FieldSource AS CHAR FORMAT "x(8)" LABEL "Kilde"
         FIELD RowIdent1 AS CHAR .

/* some button "event" def */ 
DEFINE VARIABLE EVENT AS LOGICAL EXTENT 6. 
DEFINE VARIABLE BUTTON-CANCEL AS INT INIT 1. 
DEFINE VARIABLE BUTTON-BACK   AS INT INIT 2. 
DEFINE VARIABLE BUTTON-NEW    AS INT INIT 3. 
DEFINE VARIABLE BUTTON-SAVE   AS INT INIT 4. 
DEFINE VARIABLE BUTTON-DELETE AS INT INIT 5. 
DEFINE VARIABLE BUTTON-UPDATE AS INT INIT 6.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BROWSE-2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttImportFields

/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 ttImportFields.FieldNumber ttImportFields.FieldLabel ttImportFields.FieldSource   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2   
&Scoped-define SELF-NAME BROWSE-2
&Scoped-define QUERY-STRING-BROWSE-2 FOR EACH ttImportFields
&Scoped-define OPEN-QUERY-BROWSE-2 OPEN QUERY {&SELF-NAME} FOR EACH ttImportFields.
&Scoped-define TABLES-IN-QUERY-BROWSE-2 ttImportFields
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 ttImportFields


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BROWSE-2}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rectToolbar RECT-1 BROWSE-2 BUTTON-3 ~
btnSelectValues BUTTON-2 BUTTON-6 BtnNew BtnUpdate BtnDelete 
&Scoped-Define DISPLAYED-OBJECTS lblButtons 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setSysGr C-Win 
FUNCTION setSysGr RETURNS LOGICAL
  ( INPUT  iSysGr AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnDelete DEFAULT 
     LABEL "Slett" 
     SIZE 24 BY 1.67
     BGCOLOR 8 .

DEFINE BUTTON BtnNew DEFAULT 
     LABEL "Ny" 
     SIZE 24 BY 1.67
     BGCOLOR 8 .

DEFINE BUTTON btnSelectValues 
     LABEL "Rediger import kolonner" 
     SIZE 24 BY 1.43.

DEFINE BUTTON BtnUpdate DEFAULT 
     LABEL "Oppdater" 
     SIZE 24 BY 1.67
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-2 
     LABEL "Ok" 
     SIZE 24 BY 1.43.

DEFINE BUTTON BUTTON-3 
     LABEL "Avbryt" 
     SIZE 24 BY 1.43.

DEFINE BUTTON BUTTON-6 
     LABEL "<< Forrige" 
     SIZE 24 BY 1.43.

DEFINE VARIABLE lblButtons AS CHARACTER FORMAT "X(256)":U INITIAL "Pre-definerte import kolonner" 
      VIEW-AS TEXT 
     SIZE 29 BY .62 NO-UNDO.

DEFINE VARIABLE SysGr AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     LABEL "SysGr" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL   
     SIZE 78 BY 2.62.

DEFINE RECTANGLE rectToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 18 BY 1.19.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-2 FOR 
      ttImportFields SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 C-Win _FREEFORM
  QUERY BROWSE-2 DISPLAY
      ttImportFields.FieldNumber
    ttImportFields.FieldLabel FORMAT "x(20)"
    ttImportFields.FieldSource
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 47 BY 16.19 ROW-HEIGHT-CHARS .89 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     SysGr AT ROW 2.19 COL 68 COLON-ALIGNED
     BROWSE-2 AT ROW 4.48 COL 4
     BUTTON-3 AT ROW 4.57 COL 55
     btnSelectValues AT ROW 6.24 COL 55
     BUTTON-2 AT ROW 7.91 COL 55
     BUTTON-6 AT ROW 9.62 COL 55
     BtnNew AT ROW 21.71 COL 4
     BtnUpdate AT ROW 21.71 COL 29
     BtnDelete AT ROW 21.71 COL 54
     lblButtons AT ROW 21 COL 4 NO-LABEL
     "Import Kolonner" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 3.14 COL 4.4
     rectToolbar AT ROW 1.24 COL 3.4
     RECT-1 AT ROW 21.24 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 84.2 BY 23.67.


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
         TITLE              = "VPI Import kolloner"
         HEIGHT             = 23.1
         WIDTH              = 80.4
         MAX-HEIGHT         = 48.95
         MAX-WIDTH          = 384
         VIRTUAL-HEIGHT     = 48.95
         VIRTUAL-WIDTH      = 384
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
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB BROWSE-2 SysGr DEFAULT-FRAME */
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 23.67
       FRAME DEFAULT-FRAME:WIDTH            = 84.2.

/* SETTINGS FOR FILL-IN lblButtons IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       lblButtons:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN SysGr IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       SysGr:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttImportFields.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-2 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* VPI Import kolloner */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* VPI Import kolloner */
DO:
   EVENT[BUTTON-CANCEL] = TRUE.
   APPLY "CLOSE" TO THIS-PROCEDURE.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* VPI Import kolloner */
DO:
    IF {&WINDOW-NAME}:WIDTH-PIXELS < iWidthPix THEN
          {&WINDOW-NAME}:WIDTH-PIXELS = iWidthPix.
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"Resize","").  

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnDelete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDelete C-Win
ON CHOOSE OF BtnDelete IN FRAME DEFAULT-FRAME /* Slett */
DO:
  RUN updateSysPara.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnNew
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnNew C-Win
ON CHOOSE OF BtnNew IN FRAME DEFAULT-FRAME /* Ny */
DO:
  RUN updateSysPara.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSelectValues
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSelectValues C-Win
ON CHOOSE OF btnSelectValues IN FRAME DEFAULT-FRAME /* Rediger import kolonner */
DO:  
  DEFINE VARIABLE cRowIdList  AS CHAR NO-UNDO.
  DEFINE VARIABLE cIdList     AS CHAR NO-UNDO.
  DEFINE VARIABLE bOk         AS LOG  NO-UNDO.

  RUN JBoxDSelector.w (THIS-PROCEDURE,0,
/*   RUN JBoxDSelector.w (THIS-PROCEDURE,0, */
                      "temp-table"                               
                      + ";!dummy|character|x"           /* <- to avoid initial sort invoke a dummy non-visual field */
                      + ";FieldNumber|INTEGER|>>9||Felt Nummer"
                      + ";FieldLabel|CHARACTER|x(30)|Beskrivelse"
                      ,"where false",
                      INPUT-OUTPUT cRowIdList,
                      "FieldLabel",
                      INPUT-OUTPUT cIdList,
                      "","",
                      OUTPUT bOK).

  IF bOk THEN
  DO:
    FOR EACH ttImportFields WHERE CAN-DO(cRowIdList,ttImportFields.RowIdent1): 
         ttImportFields.FieldSource = "db". 
    END.
    {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  END. 

  /* Workaround ... */ 
  IF {&WINDOW-NAME}:WIDTH-PIXELS < iWidthPix THEN
     {&WINDOW-NAME}:WIDTH-PIXELS = iWidthPix.
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"Resize","").  
  VIEW FRAME DEFAULT-FRAME. 
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnUpdate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnUpdate C-Win
ON CHOOSE OF BtnUpdate IN FRAME DEFAULT-FRAME /* Oppdater */
DO:
  RUN updateSysPara.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 C-Win
ON CHOOSE OF BUTTON-2 IN FRAME DEFAULT-FRAME /* Ok */
DO:
   DEFINE VARIABLE hdlParent2 AS HANDLE NO-UNDO.      

   RUN getParentSourceProcedure IN hdlParent (OUTPUT hdlParent2). 
   
   IF VALID-HANDLE(hdlParent2) AND 
   CAN-DO(hdlParent2:INTERNAL-ENTRIES,'SetColumnSelection') THEN
   DO:
       RUN SetColumnSelection IN hdlParent2 (INPUT TABLE ttImportFields) NO-ERROR.
       EMPTY TEMP-TABLE ttImportFields. 
       APPLY "CLOSE" TO THIS-PROCEDURE. 
   END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-3 C-Win
ON CHOOSE OF BUTTON-3 IN FRAME DEFAULT-FRAME /* Avbryt */
DO:
    EVENT[BUTTON-CANCEL] = TRUE.
    APPLY "CLOSE" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-6 C-Win
ON CHOOSE OF BUTTON-6 IN FRAME DEFAULT-FRAME /* << Forrige */
DO:

   EVENT[BUTTON-BACK] = TRUE.
   APPLY "CLOSE" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-2
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */
/* Old menu code for center on top */ 

ASSIGN                                
    C-Win:X = ACTIVE-WINDOW:X + 100
    C-Win:Y = ACTIVE-WINDOW:Y + 0 NO-ERROR.

/* new menu for center based on desktop */ 
IF ERROR-STATUS:ERROR THEN
   ASSIGN {&WINDOW-NAME}:X = (SESSION:WIDTH-PIXELS  - {&WINDOW-NAME}:WIDTH-PIXELS) / 2
          {&WINDOW-NAME}:Y = (SESSION:HEIGHT-PIXELS - {&WINDOW-NAME}:HEIGHT-PIXELS) / 2. 


/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.
{incl/wintrigg.i}

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN InitializeObject. 
  RUN SetDivResize. 
  RUN enable_UI.
  DYNAMIC-FUNCTION("NewObject",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE,"container").
  DYNAMIC-FUNCTION("setAttribute",THIS-PROCEDURE:CURRENT-WINDOW,"keepInitialSize","yes").
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
     WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

IF EVENT[BUTTON-BACK]        THEN RETURN ERROR "back".
ELSE IF EVENT[BUTTON-CANCEL] THEN RETURN ERROR "cancel".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BlankRecord C-Win 
PROCEDURE BlankRecord :
/*------------------------------------------------------------------------------
  Notes:  Clear ttImportFields to "File"      
------------------------------------------------------------------------------*/

   FOR EACH ttImportFields: 
            ttImportFields.FieldSource  = "File".
   END.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}

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
  DISPLAY lblButtons 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE rectToolbar RECT-1 BROWSE-2 BUTTON-3 btnSelectValues BUTTON-2 BUTTON-6 
         BtnNew BtnUpdate BtnDelete 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getBrowserData C-Win 
PROCEDURE getBrowserData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE OUTPUT PARAMETER opcFieldName AS CHAR NO-UNDO. 
   DEFINE OUTPUT PARAMETER opcFieldValue AS CHAR NO-UNDO. 
   
   DEFINE VARIABLE iCnt AS INT NO-UNDO.
   DEFINE VARIABLE icnt2 AS INT NO-UNDO. 

   FOR EACH ttImportFields: iCnt2 = iCnt2 + 1. END.
   opcFieldName  = FILL(cDelimiter,icnt2).
   opcFieldValue = FILL(cDelimiter,icnt2).
   
   FOR EACH ttImportFields icnt = 1 TO icnt2: 
       ENTRY(icnt,opcFieldName,cDelimiter)  = ttImportFields.FieldName.
       ENTRY(icnt,opcFieldValue,cDelimiter) = ttImportFields.FieldSource.
   END.

  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getFieldList C-Win 
PROCEDURE getFieldList :
/*------------------------------------------------------------------------------
  Purpose:     Ikke i bruk ... 
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opcFieldList AS CHAR NO-UNDO. 
    DEFINE OUTPUT PARAMETER opcFieldList2 AS CHAR NO-UNDO. 
    
    DEFINE VARIABLE cFieldList AS CHAR EXTENT 3 NO-UNDO. 
    DEFINE VARIABLE iCnt AS INT NO-UNDO. 
    DEFINE VARIABLE cFieldName AS CHAR NO-UNDO. 
    DEFINE VARIABLE cFieldLabel AS CHAR NO-UNDO. 
    DEFINE VARIABLE hPrisFil AS HANDLE NO-UNDO. 

    hPrisFil = BUFFER tt_prisfil:HANDLE.
    cFieldList = FILL(",",hPrisFil:NUM-FIELDS).
    
    DO iCnt = 1 TO hPrisFil:NUM-FIELDS:
        cFieldLabel = hPrisFil:BUFFER-FIELD(icnt):LABEL.
        cFieldName  = hPrisFil:BUFFER-FIELD(icnt):NAME.

        ENTRY(icnt,cFieldList[1]) = cFieldName.
        IF cFieldLabel NE "" THEN
          ENTRY(icnt,cFieldList[2]) = cFieldLabel.
        ELSE ENTRY(icnt,cFieldList[2]) = ENTRY(icnt,cFieldList[1]).
    END.       

    opcFieldList2 = cFieldList[1].
    opcFieldList  = cFieldList[2].
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
    
               
    /*--  Start build ttImportFields --*/         
    DEFINE VARIABLE cFieldList AS CHAR NO-UNDO.
    DEFINE VARIABLE cFieldName AS CHAR NO-UNDO. 
    DEFINE VARIABLE iCnt AS INT NO-UNDO.
   
    cFieldList = "Levnr,Varunr,EAN,Bestnr,Beskr,Strl,Enhet,Varugr,Inpris,Rabatt,Utpris,Rekpris,Moms,Förp,Link,Producent,Varumärke,KundNr".
    cFieldName = "Levnr,Varunr,EAN,Bestnr,Beskr,Strl,Enhet,Varugr,Inpris,Rabatt,Utpris,Rekpris,Moms,Förp,Link,Producent,Varumärke,KundNr". 

    DO iCnt = 1 TO NUM-ENTRIES(cFieldList):
        IF ENTRY(icnt,cFieldList) = "" THEN NEXT. 
        CREATE ttImportFields. 
        ASSIGN 
            ttImportFields.FieldLabel   = ENTRY(icnt,cFieldList)
            ttImportFields.FieldName    = ENTRY(icnt,cFieldName)
            ttImportFields.FieldNumber  = icnt 
            ttImportFields.FieldSource  = "File"
            ttImportFields.RowIdent     = "rowid" + STRING(icnt).
    END.
    /*--  End build ttImportFields --*/         


    RUN enable_UI.
    DO WITH FRAME {&FRAME-NAME}:      
         hToolbar = DYNAMIC-FUNCTION("NewToolbar",rectToolbar:HANDLE,
                    "File",
                    "Blank¤enabled,rule,Report;Hent Rapport¤enabled"
                    ,"maxborder").
    END. 
    
    gcVPILeverandorNr = '899'. /* default for dev */ 

    IF VALID-HANDLE(hdlParent) AND 
      CAN-DO(hdlParent:INTERNAL-ENTRIES,'getVPILeverandor') THEN
      RUN getVPILeverandor IN hdlParent (OUTPUT gcVPILeverandorNr) NO-ERROR. 

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
APPLY 'entry' TO browse-2 IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReportRecord C-Win 
PROCEDURE ReportRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DEFINE VARIABLE cReturnValues  AS CHAR NO-UNDO.
  DEFINE VARIABLE bOk            AS LOG  NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
    RUN JBoxLookup.w (THIS-PROCEDURE,50,
                      "sysPara"
                      + ";paranr|Import nr"
                      + ";!SysHid"
                      + ";Beskrivelse"
                      + ";Hjelpetekst1"
                      + ";!Parameter1"
                      + ";!Parameter2"
                      ,"WHERE sysgr = " + gcVPILeverandorNr + " AND sysHid = " + STRING(gSysHid) 
                      ,""                                                  
                      ,"paranr,hjelpetekst1,beskrivelse,parameter1,parameter2",   /* <- return values for these fields */
                       OUTPUT cReturnValues,
                       OUTPUT bOK).
    
    THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

    IF bOk THEN
    DO:
        ASSIGN 
        fldSysParaNr          = INT(ENTRY(1,cReturnValues,'|'))
        fldSysParaBeskrivelse = ENTRY(3,cReturnValues,'|')
        fldSysParaHjelpeTekst = ENTRY(2,cReturnValues,'|') 
        fldSysParaParameter1  = ENTRY(4,cReturnValues,'|')
        fldSysParaParameter2  = ENTRY(5,cReturnValues,'|') NO-ERROR.
        
        RUN updateBrowser(fldSysParaParameter1,fldSysParaParameter2).
    END.
  END.

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
/*
DYNAMIC-FUNCTION("setNoResizeX",THIS-PROCEDURE:CURRENT-WINDOW,
                                FRAME DEFAULT-FRAME:HANDLE,
                                "RECT-71").


DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,
                                FRAME DEFAULT-FRAME:HANDLE,
                                "RECT-71,BROWSE-TT_Error,BROWSE-TT_VareOrgInfo,Rect-1,Rect-2").


DYNAMIC-FUNCTION("setAddMoveX",THIS-PROCEDURE:CURRENT-WINDOW, 
                               FRAME DEFAULT-FRAME:HANDLE,   
                        "FI-Filnamn,BUTTON-3,FI-Rigalnr,BUTTON-4,RECT-71,BROWSE-TT_VareOrgInfo").          



/* DYNAMIC-FUNCTION("setAddMoveX",THIS-PROCEDURE:CURRENT-WINDOW, */
/*                                 FRAME DEFAULT-FRAME:HANDLE,   */
/*                                  "Btn_Ok,Btn_Help").          */
/* DYNAMIC-FUNCTION("SetResizeADM2panel",TRUE). */

*/
    DYNAMIC-FUNCTION("setnoMoveX",THIS-PROCEDURE:CURRENT-WINDOW, 
                                   FRAME DEFAULT-FRAME:HANDLE,   
                            "BROWSE-2,TEXT-1,CB-VPILev,rectToolbar").          
    
    DYNAMIC-FUNCTION("setnoMovey",THIS-PROCEDURE:CURRENT-WINDOW, 
                                   FRAME DEFAULT-FRAME:HANDLE,   
                            "BROWSE-2,text-1,BUTTON-2,button-6,button-3,text-1,btnselectvalues,TEXT-1,CB-VPILev,rectToolbar").          
    
    DYNAMIC-FUNCTION("setAddMoveX",THIS-PROCEDURE:CURRENT-WINDOW, 
                            FRAME DEFAULT-FRAME:HANDLE,   
                            "BUTTON-2,button-6,button-3").          
    DYNAMIC-FUNCTION("setNoResizeX",THIS-PROCEDURE:CURRENT-WINDOW,FRAME {&FRAME-NAME}:HANDLE,"text-1").
    
    DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,FRAME {&FRAME-NAME}:HANDLE,"rectToolbar").

    DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,iWidthPix,iHeightPix,0,0).

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
DEF INPUT PARAM ihSourceBrw   AS HANDLE NO-UNDO.
DEF INPUT PARAM ihTargetBrw   AS HANDLE NO-UNDO.

DEF VAR hSourceBuffer  AS HANDLE NO-UNDO.
DEF VAR hTargetBuffer  AS HANDLE NO-UNDO.
DEF VAR ix             AS INT    NO-UNDO.
DEF VAR cSelectedRows  AS CHAR   NO-UNDO.
DEF VAR cFieldList AS CHAR NO-UNDO.
hSourceBuffer = ihSourceBrw:QUERY:GET-BUFFER-HANDLE(1).
hTargetBuffer = ihTargetBrw:QUERY:GET-BUFFER-HANDLE(1).

DYNAMIC-FUNCTION("setAttribute",ihSourceBrw,"basequery","where true").
DYNAMIC-FUNCTION("setAttribute",ihSourceBrw,"uselocaldata","yes").
DYNAMIC-FUNCTION("setAttribute",ihTargetBrw,"uselocaldata","yes").

FOR EACH ttImportFields WHERE 
         ttImportFields.FieldSource = "File" AND 
         ttImportFields.FieldNumber NE 3 AND 
         ttImportFields.FieldLabel NE "" NO-LOCK :
    hSourceBuffer:BUFFER-CREATE().
    hSourceBuffer:BUFFER-COPY(BUFFER ttImportFields:HANDLE ).
END.

FOR EACH ttImportFields WHERE 
         ttImportFields.FieldSource = "db" AND 
         ttImportFields.FieldLabel NE ""  NO-LOCK:
    hTargetBuffer:BUFFER-CREATE().
    hTargetBuffer:BUFFER-COPY(BUFFER ttImportFields:HANDLE ).
    cSelectedRows = cSelectedRows + ttImportFields.RowIdent1 + ",".
END.


    /*
    /* Fill temp-table: */
    DO ix = 1 TO NUM-ENTRIES(cFieldList):
      hSourceBuffer:BUFFER-CREATE().
      ASSIGN hSourceBuffer:BUFFER-FIELD("FieldNumber"):BUFFER-VALUE = ix
             hSourceBuffer:BUFFER-FIELD("FieldName"):BUFFER-VALUE = ENTRY(ix,cFieldList)
             hSourceBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE = "rowid" + STRING(ix)
             .
    
      /* If any records should be pre-selected: 
      IF ix > 6 THEN DO:
        hTargetBuffer:BUFFER-CREATE().
        ASSIGN hTargetBuffer:BUFFER-FIELD("FeltNummer"):BUFFER-VALUE = ix
               hTargetBuffer:BUFFER-FIELD("Beskrivelse"):BUFFER-VALUE = ENTRY(ix,cFieldList[2])
               hTargetBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE = "rowid" + STRING(ix)
               cSelectedRows = cSelectedRows + "rowid" + STRING(ix) + ","
               .
      END.
      */
    END.
    */



    DYNAMIC-FUNCTION("setSelectedRowids" IN SOURCE-PROCEDURE,cSelectedRows).
    
    /* DYNAMIC-FUNCTION("setAttribute",ihSourceBrw,"filterdropdownvaluelist_Description","A|A|B|B").  */
    
    DYNAMIC-FUNCTION("setCurrentObject",ihSourceBrw).
    RUN OpenQuerySource IN SOURCE-PROCEDURE.
    
    DYNAMIC-FUNCTION("setCurrentObject",ihTargetBrw).
    RUN OpenQueryTarget IN SOURCE-PROCEDURE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UpdateBrowser C-Win 
PROCEDURE UpdateBrowser :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ipcFieldName AS CHAR NO-UNDO. 
   DEFINE INPUT PARAMETER ipcFieldValue AS CHAR NO-UNDO. 
   DEFINE VARIABLE iCnt AS INT NO-UNDO. 

   DO iCnt = 1 TO NUM-ENTRIES(ipcFieldName,cDelimiter):
       FIND FIRST ttImportFields WHERE FieldName = ENTRY(icnt,ipcFieldName,cDelimiter) NO-ERROR. 
       IF AVAIL ttImportFields THEN
          ttImportFields.FieldSource  = ENTRY(icnt,ipcFieldValue,cDelimiter).
   END.

  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UpdateSysPara C-Win 
PROCEDURE UpdateSysPara :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
    
    DEFINE VARIABLE ix AS INTEGER NO-UNDO. 
    DEFINE VARIABLE iy AS INTEGER NO-UNDO. 
    DEFINE VARIABLE hUpdate AS HANDLE NO-UNDO.
    DEFINE VARIABLE cAction AS CHAR NO-UNDO.

    cAction = SUBSTRING(SELF:NAME,4).
  
    ASSIGN 
        iX = INT((CURRENT-WINDOW:WIDTH-PIXEL  / 2 + CURRENT-WINDOW:X))
        iY = INT((CURRENT-WINDOW:HEIGHT-PIXEL / 2 + CURRENT-WINDOW:Y)).

  RUN getBrowserData(OUTPUT fldSysParaParameter1,OUTPUT fldSysParaParameter2).

  RUN wVpiSelectImportColumns02Upd.w PERSISTENT SET hUpdate.
  
  IF VALID-HANDLE(hUpdate) THEN 
  DO:
    ASSIGN 
      hUpdate:CURRENT-WINDOW:X =  iX
      hUpdate:CURRENT-WINDOW:Y =  iY. 

    RUN initValues IN hUpdate ( INPUT cAction,
                                INPUT gSysHid,
                                INPUT gcVPILeverandorNr,
                                INPUT fldSysParaNr,
                                INPUT fldSysParaParameter1,
                                INPUT fldSysParaParameter2).
    
    RUN initializeObject IN hUpdate.
    RUN MoveToTop IN hUpdate.
  END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setSysGr C-Win 
FUNCTION setSysGr RETURNS LOGICAL
  ( INPUT  iSysGr AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  /*MESSAGE "SetSysGr: isysgr=" iSysGr VIEW-AS ALERT-BOX.*/ 

  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

