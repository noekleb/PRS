&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME C-KundKort

/* Temp-Table and Buffer definitions                                    */
DEFINE NEW SHARED TEMP-TABLE tStLinje NO-UNDO LIKE StLinje.


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-KundKort 
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
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
&ELSE
&ENDIF

/* Local Variable Definitions ---                                       */
/* def var wOk                as log    no-undo. */
/* def var wOldRecid          as recid  no-undo. */
/* def var hHandle            as handle no-undo. */
/* def var hLabel             as handle no-undo. */
def var hStlinje           as handle no-undo.
def var hBtotmedtrans      as handle no-undo.
/* def var wLapTop            as log    no-undo. */
/* def var wBekreft           as log    no-undo. */
def var wFeil              as log    no-undo.

/* Variabler for håndtering av Tab-Stip. */
DEFINE VAR chTabs            AS COM-HANDLE NO-UNDO.
DEFINE VAR chTab             AS COM-HANDLE NO-UNDO.
define var chTab1Side        as com-handle no-undo.
DEFINE VAR wAktivFlip        AS INT        INIT 1 NO-UNDO. /* !! parameter !! */
DEFINE VAR wAktivFrame       AS INTE INIT  1.
DEFINE VAR wTabTekst         AS CHAR INIT "Vedlikehold,Transaksjoner" NO-UNDO.
DEFINE VAR wTabHnd           AS HANDLE EXTENT 2 NO-UNDO.

/* Buffere */
def buffer bMedlem for Medlem.
DEF BUFFER ledMedlem FOR Medlem.
/* def temp-table tmpChild   */
/*   field wChild as handle. */
{runlib.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn_Help RECT-27 RECT-28 BUTTON-Ok 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-KundKort AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE SUB-MENU m_Fil 
       MENU-ITEM m_Avslutt      LABEL "&Avslutt"       ACCELERATOR "ALT-F4".

DEFINE MENU MENU-BAR-C-ArtKort MENUBAR
       SUB-MENU  m_Fil          LABEL "&Fil"          .


/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE TabStrip AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chTabStrip AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Help DEFAULT 
     IMAGE-UP FILE "icon\e-help":U NO-FOCUS FLAT-BUTTON
     LABEL "&Help" 
     SIZE 4.6 BY 1.05
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-Ok 
     IMAGE-UP FILE "icon\e-exit":U NO-FOCUS FLAT-BUTTON
     LABEL "&Ok" 
     SIZE 4.6 BY 1.05 TOOLTIP "Lagre og avslutt".

DEFINE RECTANGLE RECT-27
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 159.2 BY .1.

DEFINE RECTANGLE RECT-28
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 159.4 BY .1.

DEFINE BUTTON B-Oppdater 
     LABEL "Oppdater" 
     SIZE 22 BY 1.14.

DEFINE VARIABLE FI-Aktive AS INTEGER FORMAT "->>,>>>,>>9":U INITIAL 0 
     LABEL "Aktive (m/kjøp)" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Aktive% AS DECIMAL FORMAT "->>,>>9.99%":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-AntMedlemmer AS INTEGER FORMAT "->>,>>>,>>9":U INITIAL 0 
     LABEL "Medlemmer" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Hovedmedlem% AS DECIMAL FORMAT "->>,>>9.99%":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Hovedmedlemmer AS INTEGER FORMAT "->>,>>>,>>9":U INITIAL 0 
     LABEL "Hovedmedlemmer" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-KundeTekst AS CHARACTER FORMAT "X(256)":U INITIAL "Status kundeklubb" 
      VIEW-AS TEXT 
     SIZE 29 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-Kvinner AS INTEGER FORMAT "->>,>>>,>>9":U INITIAL 0 
     LABEL "Kvinner" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Kvinner% AS DECIMAL FORMAT "->>,>>9.99%":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Menn AS INTEGER FORMAT "->>,>>>,>>9":U INITIAL 0 
     LABEL "Menn" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Menn% AS DECIMAL FORMAT "->>,>>9.99%":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-46
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 153.8 BY 16.91.

DEFINE RECTANGLE RECT-47
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 53 BY 6.19.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     Btn_Help AT ROW 1.33 COL 150.4 NO-TAB-STOP 
     BUTTON-Ok AT ROW 1.33 COL 155.4 NO-TAB-STOP 
     RECT-27 AT ROW 1.1 COL 1.4
     RECT-28 AT ROW 2.43 COL 1.2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 159.8 BY 26.81.

DEFINE FRAME FRAME-1
     B-Oppdater AT ROW 1.48 COL 132
     FI-Aktive AT ROW 4.81 COL 23 COLON-ALIGNED
     FI-Aktive% AT ROW 4.81 COL 37 COLON-ALIGNED NO-LABEL
     FI-AntMedlemmer AT ROW 2.91 COL 23 COLON-ALIGNED
     FI-Hovedmedlem% AT ROW 3.86 COL 37 COLON-ALIGNED NO-LABEL
     FI-Hovedmedlemmer AT ROW 3.86 COL 23 COLON-ALIGNED
     FI-Kvinner AT ROW 5.76 COL 23 COLON-ALIGNED
     FI-Kvinner% AT ROW 5.76 COL 37 COLON-ALIGNED NO-LABEL
     FI-Menn AT ROW 6.71 COL 23 COLON-ALIGNED
     FI-Menn% AT ROW 6.71 COL 37 COLON-ALIGNED NO-LABEL
     FI-KundeTekst AT ROW 1.76 COL 4 COLON-ALIGNED NO-LABEL
     RECT-46 AT ROW 1.24 COL 2.2
     RECT-47 AT ROW 2.43 COL 5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 9.81
         SIZE 156 BY 17.33.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: tStLinje T "NEW SHARED" NO-UNDO skotex StLinje
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-KundKort ASSIGN
         HIDDEN             = YES
         TITLE              = "Kundeklubb - Status"
         HEIGHT             = 26.81
         WIDTH              = 159.8
         MAX-HEIGHT         = 26.81
         MAX-WIDTH          = 159.8
         VIRTUAL-HEIGHT     = 26.81
         VIRTUAL-WIDTH      = 159.8
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

ASSIGN {&WINDOW-NAME}:MENUBAR    = MENU MENU-BAR-C-ArtKort:HANDLE.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-KundKort
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* REPARENT FRAME */
ASSIGN FRAME FRAME-1:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
ASSIGN 
       FRAME DEFAULT-FRAME:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME FRAME-1
   Custom                                                               */
ASSIGN 
       B-Oppdater:HIDDEN IN FRAME FRAME-1           = TRUE.

/* SETTINGS FOR FILL-IN FI-Aktive IN FRAME FRAME-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Aktive% IN FRAME FRAME-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-AntMedlemmer IN FRAME FRAME-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Hovedmedlem% IN FRAME FRAME-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Hovedmedlemmer IN FRAME FRAME-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-KundeTekst IN FRAME FRAME-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Kvinner IN FRAME FRAME-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Kvinner% IN FRAME FRAME-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Menn IN FRAME FRAME-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Menn% IN FRAME FRAME-1
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-KundKort)
THEN C-KundKort:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-1
/* Query rebuild information for FRAME FRAME-1
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME FRAME-1 */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME TabStrip ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 7.43
       COLUMN          = 2
       HEIGHT          = 20.38
       WIDTH           = 158
       HIDDEN          = no
       SENSITIVE       = yes.
      TabStrip:NAME = "TabStrip":U .
/* TabStrip OCXINFO:CREATE-CONTROL from: {1EFB6596-857C-11D1-B16A-00C0F0283628} type: TabStrip */

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-KundKort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-KundKort C-KundKort
ON END-ERROR OF C-KundKort /* Kundeklubb - Status */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-KundKort C-KundKort
ON WINDOW-CLOSE OF C-KundKort /* Kundeklubb - Status */
DO:
  
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  /* RETURN NO-APPLY. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-1
&Scoped-define SELF-NAME B-Oppdater
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Oppdater C-KundKort
ON CHOOSE OF B-Oppdater IN FRAME FRAME-1 /* Oppdater */
DO:
    /* Hidden pga att den för tillfället inte skall användas */
/*   RUN VisPost. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help C-KundKort
ON CHOOSE OF Btn_Help IN FRAME DEFAULT-FRAME /* Help */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  run WinHlp.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Ok C-KundKort
ON CHOOSE OF BUTTON-Ok IN FRAME DEFAULT-FRAME /* Ok */
DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TabStrip
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TabStrip C-KundKort OCX.Click
PROCEDURE TabStrip.TabStrip.Click .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/

IF wAktivFlip <> INT(chTabStrip:SelectedItem:Index) THEN 
DO:
    ASSIGN wAktivFlip = INT(chTabStrip:SelectedItem:Index).
    RUN ByttFrame. /* Bytter tab */
    RETURN NO-APPLY.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-KundKort 


/* ***************************  Main Block  *************************** */
/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* Biblotek, inn/utmelding og ON CLOSE OF THIS... */
{genlib.i 
  &NoLibCall      = "Nei"
  &WindowName     = "MedlemsKlubbStatus"
  &PostIClose    = "IF VALID-HANDLE(chTabStrip) THEN
                       RELEASE OBJECT chTabStrip NO-ERROR.
                    IF VALID-HANDLE(chTabs) THEN
                        RELEASE OBJECT chTabs NO-ERROR.
                    IF VALID-HANDLE(chTab) THEN
                        RELEASE OBJECT chTab NO-ERROR.
                    IF VALID-HANDLE(chTab1Side) THEN
                        RELEASE OBJECT chTab1Side NO-ERROR.
                    IF VALID-HANDLE(TabStrip) THEN
                        DELETE OBJECT TabStrip NO-ERROR.
                    ASSIGN TabStrip    = ?
                           chTabStrip  = ?
                           chTabs      = ?
                           chTab       = ?
                           chTab1Side  = ?.
                    "

}
/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* HotKeySøk - DYYYYRT */
on "CTRL-TAB":U anywhere
  do:
    chTab1Side = chTabs:Item ((IF wAktivFlip = NUM-ENTRIES(wTabTekst)
                      THEN 1 ELSE wAktivFlip + 1) BY-VARIANT-POINTER).
    chTabStrip:SelectedItem = chTab1Side.
    RETURN NO-APPLY.
  end.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  RUN enable_UI.
  {lng.i} /* Oversettelse */
  
      run ByttFrame. /* Legger opp f›rste fane. */ 
      RUN VisPost.
  ASSIGN {&WINDOW-NAME}:HIDDEN = NO.

/*   assign chTab1Side = chCtrlFrame:TabStrip. /* COM-Handl 1. side */ */
  assign chTab1Side = chTabStrip. /* COM-Handl 1. side */
  
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByttFrame C-KundKort 
PROCEDURE ByttFrame :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF VAR wInputRecid AS RECID NO-UNDO.

  ASSIGN
      wInputRecid = ?.
  IF wAktivFlip = 1 THEN DO:
      FRAME DEFAULT-FRAME:MOVE-TO-TOP().
      FRAME FRAME-1:MOVE-TO-TOP().
  END.
  ELSE IF wAktivFlip = 2 THEN 
    DO:
      IF NOT VALID-HANDLE(hBtotmedtrans) THEN
          run w-btotmedtrans.w persistent set hBtotmedtrans (THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE).
      ELSE
          RUN MoveToTopp IN hBtotmedtrans. /* det görs en set entry i MoveToTopp */
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load C-KundKort  _CONTROL-LOAD
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

OCXFile = SEARCH( "w-kundeklubb.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chTabStrip = TabStrip:COM-HANDLE
    UIB_S = chTabStrip:LoadControls( OCXFile, "TabStrip":U)
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "w-kundeklubb.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DelTmpChild C-KundKort 
PROCEDURE DelTmpChild :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*     for each tmpChild:                                   */
/*         if valid-handle(tmpChild.wChild) then do:        */
/*             RUN DelTmpChild IN tmpChild.wChild NO-ERROR. */
/*             delete procedure tmpChild.wChild.            */
/*         end.                                             */
/*     end.                                                 */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-KundKort  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-KundKort)
  THEN DELETE WIDGET C-KundKort.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-KundKort  _DEFAULT-ENABLE
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
  ENABLE Btn_Help RECT-27 RECT-28 BUTTON-Ok 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-KundKort.
  VIEW FRAME DEFAULT-FRAME IN WINDOW C-KundKort.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  DISPLAY FI-Aktive FI-Aktive% FI-AntMedlemmer FI-Hovedmedlem% FI-Hovedmedlemmer 
          FI-Kvinner FI-Kvinner% FI-Menn FI-Menn% FI-KundeTekst 
      WITH FRAME FRAME-1 IN WINDOW C-KundKort.
  ENABLE B-Oppdater RECT-46 RECT-47 
      WITH FRAME FRAME-1 IN WINDOW C-KundKort.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initialize-Controls C-KundKort 
PROCEDURE initialize-Controls :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR wCount AS INTE NO-UNDO.
  ASSIGN chTabStrip = chTabStrip:TabStrip
         chTabs     = chTabStrip:Tabs
         chTabs:Item(1):Caption = ENTRY(1,wTabTekst).
  DO wCount = 2 TO NUM-ENTRIES(wTabTekst):
      chTabs:Add(wCount,,ENTRY(wCount,wTabTekst)).
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapaTmpChild C-KundKort 
PROCEDURE SkapaTmpChild :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*     DEFINE INPUT PARAMETER ipChild AS HANDLE NO-UNDO. */
/*     IF VALID-HANDLE(ipChild) THEN DO:                 */
/*         CREATE tmpChild.                              */
/*         ASSIGN tmpChild.wChild = ipChild.             */
/*         RELEASE tmpChild.                             */
/*     END.                                              */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettProg C-KundKort 
PROCEDURE SlettProg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF VALID-HANDLE(hStlinje) THEN
      DELETE PROCEDURE hStlinje.
    IF VALID-HANDLE(hBtotmedtrans) THEN
      DELETE PROCEDURE hBtotmedtrans.
  APPLY "CLOSE":U TO THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisPost C-KundKort 
PROCEDURE VisPost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  ASSIGN
      FI-AntMedlemmer   = 0
      FI-Hovedmedlemmer = 0
      FI-Aktive         = 0
      FI-Kvinner        = 0
      FI-Menn           = 0
      .
  FOR EACH Medlem NO-LOCK:
      ASSIGN
          FI-AntMedlemmer   = FI-AntMedlemmer + 1
          FI-Hovedmedlemmer = FI-Hovedmedlemmer + 
                              (IF Medlem.HovedMedlemFlagg THEN 1 ELSE 0)
          FI-Aktive         = FI-Aktive + 
                              (IF CAN-FIND(first MedTrans OF Medlem)
                                  THEN 1
                                  ELSE 0)
          FI-Kvinner        = FI-Kvinner +
                              (IF Medlem.Kjonn = false
                                  THEN 1
                                  ELSE 0)
          FI-Menn           = FI-Menn +
                              (IF Medlem.Kjonn = TRUE
                                  THEN 1
                                  ELSE 0)
          .
  END.
  ASSIGN
      FI-Hovedmedlem% = (FI-Hovedmedlemmer / FI-AntMedlemmer) * 100
      FI-Aktive%      = (FI-Aktive / FI-AntMEdlemmer) * 100
      FI-Kvinner%     = (FI-Kvinner / FI-AntMedlemmer) * 100
      FI-Menn%        = (FI-Menn / FI-AntMEdlemmer) * 100
      .
  DISPLAY
      FI-Hovedmedlem%
      FI-Aktive%     
      FI-Kvinner%    
      FI-Menn%
      FI-AntMedlemmer  
      FI-Hovedmedlemmer
      FI-Aktive        
      FI-Kvinner       
      FI-Menn          
  WITH FRAME FRAME-1.
      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WinHlp C-KundKort 
PROCEDURE WinHlp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {winhlp.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

