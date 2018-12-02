&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: New V9 Version - January 15, 1998
          
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AB.              */
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

DEFINE VARIABLE iWindowWidth  AS INTEGER    NO-UNDO.
DEFINE VARIABLE iWindowHeight AS INTEGER    NO-UNDO.
DEFINE VARIABLE dFolderRow    AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dRowDiff      AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dGridheight   AS DECIMAL    NO-UNDO.

DEFINE VARIABLE cDeleteFolderPage AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cVisFolderPage9 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iNumFolders       AS INTEGER     NO-UNDO.
DEF VAR h_wartbasutvalg     AS HANDLE NO-UNDO.
DEF VAR cCurrWhere          AS CHARACTER NO-UNDO.
DEF VAR hAnropProc          AS HANDLE     NO-UNDO.
DEF VAR cProgram            AS CHAR       NO-UNDO. /* för colonnevalg Excel */
/* DEFINE VARIABLE iWidthPix  AS INTEGER    NO-UNDO. */
/* DEFINE VARIABLE iHeightPix AS INTEGER    NO-UNDO. */
DEFINE VARIABLE cSEfolder AS CHARACTER INIT 
    "Transaktioner|Kvitton|Kalkylkontroll|Beställningar|Säsongsanalys|Mässanalys|Månadsrapport|Överföringar|Rabattanalys" NO-UNDO.

DEFINE NEW SHARED TEMP-TABLE TT_Resultat NO-UNDO
    FIELD LinjeNr AS INTEGER
    FIELD Verdier AS CHARACTER
    INDEX Linjenr IS PRIMARY Linjenr.

{tmp2artbasdef.i &NEW=NEW}

{incl/DevMode.i}
{incl/CustDevMode.i}
PROCEDURE LockWindowUpdate EXTERNAL "user32.dll" :
   DEFINE INPUT  PARAMETER hWndLock AS LONG.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fMain

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS B-Excel LINJE-1 LINJE-2 B-Vis B-Xprint 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fLockVindu wWin 
FUNCTION fLockVindu RETURNS CHARACTER
  ( INPUT lLock AS LOGICAL )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD geth_dstlager wWin 
FUNCTION geth_dstlager RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD geth_dstlinje wWin 
FUNCTION geth_dstlinje RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD geth_frapportgrid wWin 
FUNCTION geth_frapportgrid RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD InitializeResize wWin 
FUNCTION InitializeResize RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE MENU POPUP-MENU-B-Excel 
       MENU-ITEM m_Velg_kolonner LABEL "Velg kolonner" .


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_dstlager AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dstlinje AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dtmpartbas AS HANDLE NO-UNDO.
DEFINE VARIABLE h_fbestillingfilter AS HANDLE NO-UNDO.
DEFINE VARIABLE h_fbongfilter AS HANDLE NO-UNDO.
DEFINE VARIABLE h_fbongrabatter AS HANDLE NO-UNDO.
DEFINE VARIABLE h_fkalkylekontroll AS HANDLE NO-UNDO.
DEFINE VARIABLE h_fmanedsrapport AS HANDLE NO-UNDO.
DEFINE VARIABLE h_fmodellanalyse AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_foverforinger AS HANDLE NO-UNDO.
DEFINE VARIABLE h_frapportgrid AS HANDLE NO-UNDO.
DEFINE VARIABLE h_fst_messe AS HANDLE NO-UNDO.
DEFINE VARIABLE h_ftransloggfilter AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Excel 
     IMAGE-UP FILE "icon/excel.bmp":U NO-FOCUS
     LABEL "Excel..." 
     SIZE 4.6 BY 1.05 TOOLTIP "Eksporter til Excel.".

DEFINE BUTTON B-Vis 
     LABEL "Vis / skjul" 
     SIZE 15 BY 1.05.

DEFINE BUTTON B-Xprint 
     IMAGE-UP FILE "icon/e-print.bmp":U NO-FOCUS
     LABEL "Print" 
     SIZE 4.6 BY 1.05 TOOLTIP "XPrint rapport".

DEFINE RECTANGLE LINJE-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 202 BY .14.

DEFINE RECTANGLE LINJE-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 202 BY .14.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     B-Excel AT ROW 1.19 COL 2.2
     B-Vis AT ROW 1.24 COL 24
     B-Xprint AT ROW 1.19 COL 7.2
     LINJE-1 AT ROW 1.05 COL 1
     LINJE-2 AT ROW 2.33 COL 1
     SPACE(0.00) SKIP(30.67)
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Design Page: 9
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Diverse rapporter"
         HEIGHT             = 32.33
         WIDTH              = 203.2
         MAX-HEIGHT         = 32.38
         MAX-WIDTH          = 203.2
         VIRTUAL-HEIGHT     = 32.38
         VIRTUAL-WIDTH      = 203.2
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wWin 
/* ************************* Included-Libraries *********************** */

{incl/DevMode.i}
{incl/CustDevMode.i}
{src/adm2/containr.i}
{dproclibstart.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME fMain
   FRAME-NAME Size-to-Fit                                               */
ASSIGN 
       FRAME fMain:SCROLLABLE       = FALSE.

ASSIGN 
       B-Excel:POPUP-MENU IN FRAME fMain       = MENU POPUP-MENU-B-Excel:HANDLE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Diverse rapporter */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Diverse rapporter */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  IF VALID-HANDLE(h_wartbasutvalg) THEN
         APPLY "CLOSE" TO h_wartbasutvalg.
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-RESIZED OF wWin /* Diverse rapporter */
DO:
/*     IF {&WINDOW-NAME}:WIDTH-PIXELS < iWidthPix THEN                                                              */
/*             {&WINDOW-NAME}:WIDTH-PIXELS = iWidthPix.                                                             */
/*     DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"Resize",""). */
    ASSIGN wWin:HEIGHT = DYNAMIC-FUNCTION('getMinHeight':U)
           wWin:WIDTH  = MAX(DYNAMIC-FUNCTION('getMinWidth':U),DYNAMIC-FUNCTION('getWidth':U))
           FRAME {&FRAME-NAME}:WIDTH = IF FRAME {&FRAME-NAME}:WIDTH < wWin:WIDTH THEN
               wWin:WIDTH ELSE FRAME {&FRAME-NAME}:WIDTH.
           RUN resizeObject IN h_folder
    ( INPUT DYNAMIC-FUNCTION('getHeight':U IN h_folder) /* DECIMAL */,
      INPUT MAX(DYNAMIC-FUNCTION('getMinWidth':U IN h_folder),wWin:WIDTH - 1) /* DECIMAL */).
           RUN resizeObject IN h_frapportgrid
    ( INPUT DYNAMIC-FUNCTION('getHeight':U IN h_frapportgrid) /* DECIMAL */,
      INPUT MAX(DYNAMIC-FUNCTION('getMinWidth':U IN h_frapportgrid),wWin:WIDTH - 4) /* DECIMAL */).
       ASSIGN FRAME {&FRAME-NAME}:WIDTH  = wWin:WIDTH.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Excel wWin
ON CHOOSE OF B-Excel IN FRAME fMain /* Excel... */
DO:
    IF B-Vis:HIDDEN = TRUE THEN
        RUN VisaIExcel IN h_frapportgrid.
    ELSE
        RUN PreHiddenExcel IN h_frapportgrid.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Vis
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Vis wWin
ON CHOOSE OF B-Vis IN FRAME fMain /* Vis / skjul */
DO:
  RUN VisKun IN h_frapportgrid ("",SELF:PRIVATE-DATA).
  SELF:PRIVATE-DATA = STRING(SELF:PRIVATE-DATA = "VIS","SKJUL/VIS").
/*   SELF:HIDDEN = TRUE. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Xprint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Xprint wWin
ON CHOOSE OF B-Xprint IN FRAME fMain /* Print */
DO:
  DEFINE VARIABLE cFilterVerdier AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cColAlign      AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iPage          AS INTEGER    NO-UNDO.
  DEFINE VARIABLE hAktivHandle   AS HANDLE     NO-UNDO.
  DEFINE VARIABLE cTabLabel      AS CHARACTER  NO-UNDO.
  ASSIGN iPage   = DYNAMIC-FUNCTION('getCurrentPage':U).
  CASE iPage:
      WHEN 1 THEN
          ASSIGN hAktivHandle = h_ftransloggfilter.
      WHEN 2 THEN
          ASSIGN hAktivHandle = h_fbongfilter.
      WHEN 3 THEN
          ASSIGN hAktivHandle = h_fkalkylekontroll.
      WHEN 4 THEN
          ASSIGN hAktivHandle = h_fbestillingfilter.
      WHEN 5 THEN
          ASSIGN hAktivHandle = h_fmodellanalyse.
      WHEN 9 THEN
          ASSIGN hAktivHandle = h_fbongrabatter.
  END CASE.
  RUN SendFilterValues IN hAktivHandle (OUTPUT cFilterVerdier, OUTPUT cColAlign) NO-ERROR.
  ASSIGN cFilterVerdier = REPLACE(cFilterverdier,CHR(10)," ")
         cFilterVerdier = REPLACE(cFilterverdier,"AAR","ÅR")
         cFilterVerdier = REPLACE(cFilterverdier,"MANED","MÅNED").
  ASSIGN cTabLabel      = ENTRY(INT(DYNAMIC-FUNCTION('getCurrentPage':U)),DYNAMIC-FUNCTION('getFolderLabels':U IN h_folder),"|")
         cFilterVerdier = cTabLabel + 
                          (IF cFilterVerdier <> "" THEN CHR(2) ELSE "") + 
                          cFilterVerdier.
  PUBLISH "PrintGrid" ("XPRINT",cFilterVerdier,2,"",cColAlign).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Velg_kolonner
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Velg_kolonner wWin
ON CHOOSE OF MENU-ITEM m_Velg_kolonner /* Velg kolonner */
DO:
    RUN VisaIExcelKolonnevalg IN h_frapportgrid (cProgram,DYNAMIC-FUNCTION("getCurrentPage":U)).
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
ASSIGN cProgram = ENTRY(1,PROGRAM-NAME(1),".").

{src/adm2/windowmn.i}

{lng.i &SDO = "SDO"}

{incl\wintrigg.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects wWin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE currentPage  AS INTEGER NO-UNDO.

  ASSIGN currentPage = getCurrentPage().

  CASE currentPage: 

    WHEN 0 THEN DO:
       RUN constructObject (
             INPUT  'sdo/dtmpartbas.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessnoDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedtmpartbasOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dtmpartbas ).
       RUN repositionObject IN h_dtmpartbas ( 1.00 , 87.00 ) NO-ERROR.
       /* Size in AB:  ( 1.43 , 10.80 ) */

       RUN constructObject (
             INPUT  'sdo/dstlager.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessnoDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedstlagerOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dstlager ).
       RUN repositionObject IN h_dstlager ( 1.00 , 108.00 ) NO-ERROR.
       /* Size in AB:  ( 1.43 , 8.00 ) */

       RUN constructObject (
             INPUT  'sdo/dstlinje.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessnoDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedstlinjeOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dstlinje ).
       RUN repositionObject IN h_dstlinje ( 1.24 , 100.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 5.80 ) */

       RUN constructObject (
             INPUT  'prg/frapportgrid.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'LogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_frapportgrid ).
       RUN repositionObject IN h_frapportgrid ( 2.43 , 1.80 ) NO-ERROR.
       /* Size in AB:  ( 21.14 , 201.20 ) */

       RUN constructObject (
             INPUT  'adm2/folder.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FolderLabels':U + 'Translogg|Bonger|Kalkylekontroll|Bestillinger|Sesonganalyse|Messeanalyse|Månedsrapport|Overføringer|Rabattanalyser' + 'FolderTabWidth0FolderFont-1HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_folder ).
       RUN repositionObject IN h_folder ( 23.38 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_folder ( 9.76 , 202.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN initPages ('4,2,9,3,7,5,8,6,1':U) NO-ERROR.

       /* Links to SmartFrame h_frapportgrid. */
       RUN addLink ( h_fbestillingfilter , 'ClearGrid':U , h_frapportgrid ).
       RUN addLink ( h_fbestillingfilter , 'FeltVerdier':U , h_frapportgrid ).
       RUN addLink ( h_fbestillingfilter , 'LoadGrid':U , h_frapportgrid ).
       RUN addLink ( h_fbestillingfilter , 'Summer':U , h_frapportgrid ).
       RUN addLink ( h_fbestillingfilter , 'VisTxtBox':U , h_frapportgrid ).
       RUN addLink ( h_fbongfilter , 'AlignCol':U , h_frapportgrid ).
       RUN addLink ( h_fbongfilter , 'ClearGrid':U , h_frapportgrid ).
       RUN addLink ( h_fbongfilter , 'FeltVerdier':U , h_frapportgrid ).
       RUN addLink ( h_fbongfilter , 'LoadGrid':U , h_frapportgrid ).
       RUN addLink ( h_fbongfilter , 'VisTxtBox':U , h_frapportgrid ).
       RUN addLink ( h_fbongrabatter , 'AlignCol':U , h_frapportgrid ).
       RUN addLink ( h_fbongrabatter , 'ClearGrid':U , h_frapportgrid ).
       RUN addLink ( h_fbongrabatter , 'FeltVerdier':U , h_frapportgrid ).
       RUN addLink ( h_fbongrabatter , 'LoadGrid':U , h_frapportgrid ).
       RUN addLink ( h_fbongrabatter , 'VisKun':U , h_frapportgrid ).
       RUN addLink ( h_fbongrabatter , 'VisTxtBox':U , h_frapportgrid ).
       RUN addLink ( h_fkalkylekontroll , 'AlignCol':U , h_frapportgrid ).
       RUN addLink ( h_fkalkylekontroll , 'ClearGrid':U , h_frapportgrid ).
       RUN addLink ( h_fkalkylekontroll , 'FeltVerdier':U , h_frapportgrid ).
       RUN addLink ( h_fkalkylekontroll , 'LoadGrid':U , h_frapportgrid ).
       RUN addLink ( h_fkalkylekontroll , 'VisTxtBox':U , h_frapportgrid ).
       RUN addLink ( h_fmanedsrapport , 'AlignCol':U , h_frapportgrid ).
       RUN addLink ( h_fmanedsrapport , 'ClearGrid':U , h_frapportgrid ).
       RUN addLink ( h_fmanedsrapport , 'FeltVerdier':U , h_frapportgrid ).
       RUN addLink ( h_fmanedsrapport , 'LoadGrid':U , h_frapportgrid ).
       RUN addLink ( h_fmanedsrapport , 'Summer':U , h_frapportgrid ).
       RUN addLink ( h_fmanedsrapport , 'VisTxtBox':U , h_frapportgrid ).
       RUN addLink ( h_fmodellanalyse , 'ClearGrid':U , h_frapportgrid ).
       RUN addLink ( h_fmodellanalyse , 'FeltVerdier':U , h_frapportgrid ).
       RUN addLink ( h_fmodellanalyse , 'LoadGrid':U , h_frapportgrid ).
       RUN addLink ( h_fmodellanalyse , 'Summer':U , h_frapportgrid ).
       RUN addLink ( h_fmodellanalyse , 'VisTxtBox':U , h_frapportgrid ).
       RUN addLink ( h_foverforinger , 'AlignCol':U , h_frapportgrid ).
       RUN addLink ( h_foverforinger , 'ClearGrid':U , h_frapportgrid ).
       RUN addLink ( h_foverforinger , 'FeltVerdier':U , h_frapportgrid ).
       RUN addLink ( h_foverforinger , 'LoadGrid':U , h_frapportgrid ).
       RUN addLink ( h_foverforinger , 'Summer':U , h_frapportgrid ).
       RUN addLink ( h_foverforinger , 'VisTxtBox':U , h_frapportgrid ).
       RUN addLink ( h_fst_messe , 'ClearGrid':U , h_frapportgrid ).
       RUN addLink ( h_fst_messe , 'FeltVerdier':U , h_frapportgrid ).
       RUN addLink ( h_fst_messe , 'LoadGrid':U , h_frapportgrid ).
       RUN addLink ( h_fst_messe , 'Summer':U , h_frapportgrid ).
       RUN addLink ( h_fst_messe , 'VisTxtBox':U , h_frapportgrid ).
       RUN addLink ( h_ftransloggfilter , 'AlignCol':U , h_frapportgrid ).
       RUN addLink ( h_ftransloggfilter , 'ClearGrid':U , h_frapportgrid ).
       RUN addLink ( h_ftransloggfilter , 'FeltVerdier':U , h_frapportgrid ).
       RUN addLink ( h_ftransloggfilter , 'LoadGrid':U , h_frapportgrid ).
       RUN addLink ( h_ftransloggfilter , 'Summer':U , h_frapportgrid ).
       RUN addLink ( h_ftransloggfilter , 'VisTxtBox':U , h_frapportgrid ).
       RUN addLink ( THIS-PROCEDURE , 'PrintGrid':U , h_frapportgrid ).
       RUN addLink ( h_frapportgrid , 'GetWindowH':U , THIS-PROCEDURE ).

       /* Links to SmartFolder h_folder. */
       RUN addLink ( h_folder , 'Page':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_frapportgrid ,
             B-Vis:HANDLE IN FRAME fMain , 'AFTER':U ).
       RUN adjustTabOrder ( h_folder ,
             h_frapportgrid , 'AFTER':U ).
    END. /* Page 0 */
    WHEN 1 THEN DO:
       RUN constructObject (
             INPUT  'prg/ftransloggfilter.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'LogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_ftransloggfilter ).
       RUN repositionObject IN h_ftransloggfilter ( 24.81 , 6.00 ) NO-ERROR.
       /* Size in AB:  ( 6.29 , 155.00 ) */

       /* Links to SmartFrame h_ftransloggfilter. */
       RUN addLink ( h_ftransloggfilter , 'GetWindowH':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_ftransloggfilter ,
             h_folder , 'AFTER':U ).
    END. /* Page 1 */
    WHEN 2 THEN DO:
       RUN constructObject (
             INPUT  'prg/fbongfilter.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'LogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_fbongfilter ).
       RUN repositionObject IN h_fbongfilter ( 24.81 , 3.00 ) NO-ERROR.
       /* Size in AB:  ( 7.33 , 170.80 ) */

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_fbongfilter ,
             h_folder , 'AFTER':U ).
    END. /* Page 2 */
    WHEN 3 THEN DO:
       RUN constructObject (
             INPUT  'prg/fkalkylekontroll.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'LogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_fkalkylekontroll ).
       RUN repositionObject IN h_fkalkylekontroll ( 24.81 , 6.00 ) NO-ERROR.
       /* Size in AB:  ( 5.71 , 130.40 ) */

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_fkalkylekontroll ,
             h_folder , 'AFTER':U ).
    END. /* Page 3 */
    WHEN 4 THEN DO:
       RUN constructObject (
             INPUT  'prg/fbestillingfilter.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'LogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_fbestillingfilter ).
       RUN repositionObject IN h_fbestillingfilter ( 24.81 , 6.00 ) NO-ERROR.
       /* Size in AB:  ( 6.48 , 187.20 ) */

       /* Links to SmartFrame h_fbestillingfilter. */
       RUN addLink ( h_fbestillingfilter , 'GetWindowH':U , THIS-PROCEDURE ).

    END. /* Page 4 */
    WHEN 5 THEN DO:
       RUN constructObject (
             INPUT  'prg/fmodellanalyse.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'LogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_fmodellanalyse ).
       RUN repositionObject IN h_fmodellanalyse ( 24.81 , 6.00 ) NO-ERROR.
       /* Size in AB:  ( 7.57 , 187.20 ) */

       /* Links to SmartFrame h_fmodellanalyse. */
       RUN addLink ( h_fmodellanalyse , 'GetWindowH':U , THIS-PROCEDURE ).

    END. /* Page 5 */
    WHEN 6 THEN DO:
       RUN constructObject (
             INPUT  'prg/fst_messe.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'LogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_fst_messe ).
       RUN repositionObject IN h_fst_messe ( 24.81 , 6.00 ) NO-ERROR.
       /* Size in AB:  ( 7.57 , 187.20 ) */

       /* Links to SmartFrame h_fst_messe. */
       RUN addLink ( h_fst_messe , 'GetWindowH':U , THIS-PROCEDURE ).

    END. /* Page 6 */
    WHEN 7 THEN DO:
       RUN constructObject (
             INPUT  'prg/fmanedsrapport.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'LogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_fmanedsrapport ).
       RUN repositionObject IN h_fmanedsrapport ( 24.81 , 6.00 ) NO-ERROR.
       /* Size in AB:  ( 6.29 , 155.00 ) */

       /* Links to SmartFrame h_fmanedsrapport. */
       RUN addLink ( h_fmanedsrapport , 'GetWindowH':U , THIS-PROCEDURE ).

    END. /* Page 7 */
    WHEN 8 THEN DO:
       RUN constructObject (
             INPUT  'prg/foverforinger.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'LogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_foverforinger ).
       RUN repositionObject IN h_foverforinger ( 24.81 , 6.00 ) NO-ERROR.
       /* Size in AB:  ( 6.29 , 155.00 ) */

       /* Links to SmartFrame h_foverforinger. */
       RUN addLink ( h_foverforinger , 'GetWindowH':U , THIS-PROCEDURE ).

    END. /* Page 8 */
    WHEN 9 THEN DO:
       RUN constructObject (
             INPUT  'prg/fbongrabatter.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'LogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_fbongrabatter ).
       RUN repositionObject IN h_fbongrabatter ( 24.81 , 3.00 ) NO-ERROR.
       /* Size in AB:  ( 6.29 , 155.00 ) */

       /* Links to SmartFrame h_fbongrabatter. */
       RUN addLink ( h_fbongrabatter , 'GetWindowH':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_fbongrabatter ,
             h_folder , 'AFTER':U ).
    END. /* Page 9 */

  END CASE.
  /* Select a Startup page. */
  IF currentPage eq 0
  THEN RUN selectPage IN THIS-PROCEDURE ( 1 ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ApplyEntryToBrowse wWin 
PROCEDURE ApplyEntryToBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI wWin  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
  THEN DELETE WIDGET wWin.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI wWin  _DEFAULT-ENABLE
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
  ENABLE B-Excel LINJE-1 LINJE-2 B-Vis B-Xprint 
      WITH FRAME fMain IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  VIEW wWin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EndreSize wWin 
PROCEDURE EndreSize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RETURN.
   ASSIGN  wWin:HEIGHT-PIXELS = iWindowHeight
           wWin:WIDTH-PIXELS  = iWindowWidth
           FRAME {&FRAME-NAME}:WIDTH = wWin:WIDTH
           FRAME {&FRAME-NAME}:HEIGHT = wWin:HEIGHT
           LINJE-1:WIDTH-PIXELS = iWindowWidth - 2
           LINJE-2:WIDTH-PIXELS = iWindowWidth - 2.
           RUN resizeObject IN h_folder
    ( INPUT DYNAMIC-FUNCTION('getHeight':U IN h_folder) /* DECIMAL */,
      INPUT MAX(DYNAMIC-FUNCTION('getMinWidth':U IN h_folder),wWin:WIDTH - 1) /* DECIMAL */).
    ASSIGN dRowDiff = dFolderRow - DYNAMIC-FUNCTION('getRow':U IN h_folder).
    RUN repositionObject IN h_folder
       ( INPUT dFolderRow /* DECIMAL */,
         INPUT DYNAMIC-FUNCTION('getCol':U IN h_folder) /* DECIMAL */).
    RUN repositionObject IN h_ftransloggfilter
       ( INPUT DYNAMIC-FUNCTION('getRow':U IN h_ftransloggfilter) + dRowDiff /* DECIMAL */,
         INPUT DYNAMIC-FUNCTION('getCol':U IN h_ftransloggfilter) /* DECIMAL */).
    RUN repositionObject IN h_fbongfilter
       ( INPUT DYNAMIC-FUNCTION('getRow':U IN h_fbongfilter) + dRowDiff /* DECIMAL */,
         INPUT DYNAMIC-FUNCTION('getCol':U IN h_fbongfilter) /* DECIMAL */).
    RUN repositionObject IN h_fkalkylekontroll
       ( INPUT DYNAMIC-FUNCTION('getRow':U IN h_fkalkylekontroll) + dRowDiff /* DECIMAL */,
         INPUT DYNAMIC-FUNCTION('getCol':U IN h_fkalkylekontroll) /* DECIMAL */).
    RUN repositionObject IN h_fbestillingfilter
       ( INPUT DYNAMIC-FUNCTION('getRow':U IN h_fbestillingfilter) + dRowDiff /* DECIMAL */,
         INPUT DYNAMIC-FUNCTION('getCol':U IN h_fbestillingfilter) /* DECIMAL */).
    RUN repositionObject IN h_fmodellanalyse
       ( INPUT DYNAMIC-FUNCTION('getRow':U IN h_fmodellanalyse) + dRowDiff /* DECIMAL */,
         INPUT DYNAMIC-FUNCTION('getCol':U IN h_fmodellanalyse) /* DECIMAL */).

    RUN EndreSize IN h_frapportgrid (dGridheight /* DECIMAL */, wWin:WIDTH - 3 /* DECIMAL */).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exitObject wWin 
PROCEDURE exitObject :
/*------------------------------------------------------------------------------
  Purpose:  Window-specific override of this procedure which destroys 
            its contents and itself.
    Notes:  
------------------------------------------------------------------------------*/
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetWindowH wWin 
PROCEDURE GetWindowH :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER hWindowHandle AS HANDLE     NO-UNDO.
    ASSIGN hWindowHandle = THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iButikkNr  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE dTransDato AS DATE       NO-UNDO.
  DEFINE VARIABLE iTst       AS INTEGER     NO-UNDO.
  DEFINE VARIABLE ii         AS INTEGER     NO-UNDO.
  /* Code placed here will execute PRIOR to standard behavior. */
/*   IF SESSION:WIDTH-PIXELS = 1440 AND  */
/*      SESSION:HEIGHT-PIXELS = 900 THEN */
/*       ASSIGN iWindowWidth  = 1430     */
/*              iWindowHeight = 808      */
/*              dFolderRow    = 31.4     */
/*              dGridheight   = 28.8.    */
/*   ELSE                                */
/*   IF SESSION:WIDTH-PIXELS >= 1280 THEN      */
/*       ASSIGN iWindowWidth  = 1270           */
/*              iWindowHeight = 934            */
/*              dFolderRow    = 35.8           */
/*              dGridheight   = 33.3.          */
/*   ELSE IF SESSION:WIDTH-PIXELS >= 1024 THEN */
/*             ASSIGN iWindowWidth = 1024      */
/*                    iWindowHeight = 680      */
/*                    dFolderRow    = 24.4     */
/*                    dGridheight   = 22.0.    */
  ASSIGN wWin:X = 1
         wWin:Y = 1.
/*   IF SESSION:WIDTH-PIXELS > 1024 THEN */
/*       RUN EndreSize. */
  IF NUM-ENTRIES(PROGRAM-NAME(2)," ") > 1 AND ENTRY(2,PROGRAM-NAME(2)," ") BEGINS "wtmpartbas" THEN
      DYNAMIC-FUNCTION("DoLockWindow",{&WINDOW-NAME}).
  /* LagerVare pg-name(2) */
/*   ASSIGN iWidthPix  = {&WINDOW-NAME}:WIDTH-PIXELS   */
/*          iHeightPix = {&WINDOW-NAME}:HEIGHT-PIXELS. */
  FIND bruker WHERE bruker.brukerid = USERID("skotex") NO-LOCK NO-ERROR.
/*   OUTPUT TO "CLIPBOARD".                                                  */
/*   PUT UNFORMATTED DYNAMIC-FUNCTION('getFolderLabels':U IN h_folder) SKIP. */
/*   OUTPUT CLOSE.                                                           */
  
  IF AVAIL bruker AND CAN-DO("SE,SVE",Bruker.Lng) THEN DO:
      IF NUM-ENTRIES(DYNAMIC-FUNCTION('getFolderLabels':U IN h_folder),"|") =
         NUM-ENTRIES(cSEfolder,"|") THEN
          DYNAMIC-FUNCTION('setFolderLabels':U IN h_folder,
     INPUT cSEfolder /* CHARACTER */).
  END.
  IF bruker.brukertype > 1 THEN
       {syspara.i 6 11 1 cDeleteFolderPage}
      
  {syspar2.i 6 11 1 cVisFolderPage9}

  RUN SUPER.
  B-Vis:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
  /* Code placed here will execute AFTER standard behavior.    */
/*   RUN SetDivResize. */
  IF NUM-ENTRIES(PROGRAM-NAME(3)," ") = 2 AND
      ENTRY(2,PROGRAM-NAME(3)," ") BEGINS "wbokforingsbilag" THEN DO:
      PUBLISH "GetTransDato" (OUTPUT iButikkNr,OUTPUT dTransDato).
      IF dTransDato <> ? THEN DO:
          RUN selectPage
            ( INPUT 12 /* INTEGER */).
          RUN Aktiver IN h_ftransloggfilter (iButikkNr,dTransDato).
      END.
  END.
  InitializeResize().

  {&WINDOW-NAME}:WIDTH-PIXELS = SESSION:WIDTH-PIXELS - 10.
  {&WINDOW-NAME}:HEIGHT-PIXELS = SESSION:HEIGHT-PIXELS - 110.

  APPLY "WINDOW-RESIZED" TO {&WINDOW-NAME}.
  RUN selectPage (1).
/*   RUN deleteFolderPage IN h_folder (9). */
  IF NOT cVisFolderPage9 = "1" THEN
      RUN deleteFolderPage IN h_folder (9).
  IF cDeleteFolderPage <> "" THEN DO:
      iNumFolders = NUM-ENTRIES(DYNAMIC-FUNCTION('getFolderLabels':U IN h_folder),"|").
      DO ii = 1 TO NUM-ENTRIES(cDeleteFolderPage):
          iTst = INT(ENTRY(ii,cDeleteFolderPage)) NO-ERROR.
          IF ERROR-STATUS:ERROR OR iTst < 3 OR ii > iNumFolders THEN
              NEXT.
          RUN deleteFolderPage IN h_folder (iTst).
      END.
    /*   RUN disableFolderPage IN h_folder (8). */
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Nullstill wWin 
PROCEDURE Nullstill :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN Nullstill IN h_dtmpArtBas.
  DYNAMIC-FUNCTION('openQuery':U IN h_dtmpartbas).

  /* Tømmer tabellen - klar for ny runde. */
  EMPTY TEMP-TABLE tmp2ArtBas.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OldInitialize wWin 
PROCEDURE OldInitialize :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iButikkNr  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE dTransDato AS DATE       NO-UNDO.
  /* Code placed here will execute PRIOR to standard behavior. */
  IF SESSION:WIDTH-PIXELS = 1440 AND
     SESSION:HEIGHT-PIXELS = 900 THEN
      ASSIGN iWindowWidth  = 1430
             iWindowHeight = 808
             dFolderRow    = 31.4
             dGridheight   = 28.8.
  ELSE IF SESSION:WIDTH-PIXELS >= 1280 THEN
      ASSIGN iWindowWidth  = 1270
             iWindowHeight = 934
             dFolderRow    = 37.4
             dGridheight   = 34.7.
  ELSE IF SESSION:WIDTH-PIXELS >= 1024 THEN
            ASSIGN iWindowWidth = 1016
                   iWindowHeight = 680
                   dFolderRow    = 25.3
                   dGridheight   = 22.5.
  ASSIGN wWin:X = 1
         wWin:Y = 1.
  IF iWindowWidth > 800 THEN
      RUN EndreSize.
  IF ENTRY(2,PROGRAM-NAME(2)," ") BEGINS "wtmpartbas" THEN
      DYNAMIC-FUNCTION("DoLockWindow",{&WINDOW-NAME}).
  /* LagerVare pg-name(2) */
  RUN SUPER.
  /* Code placed here will execute AFTER standard behavior.    */
  IF NUM-ENTRIES(PROGRAM-NAME(3)," ") = 2 AND
      ENTRY(2,PROGRAM-NAME(3)," ") BEGINS "wbokforingsbilag" THEN DO:
      PUBLISH "GetTransDato" (OUTPUT iButikkNr,OUTPUT dTransDato).
      IF dTransDato <> ? THEN DO:
          RUN selectPage
            ( INPUT 12 /* INTEGER */).
          RUN Aktiver IN h_ftransloggfilter (iButikkNr,dTransDato).
      END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE selectPage wWin 
PROCEDURE selectPage :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAMETER piPageNum AS INTEGER NO-UNDO.
  /* Code placed here will execute PRIOR to standard behavior. */
  DYNAMIC-FUNCTION("DoLockWindow",THIS-PROCEDURE:CURRENT-WINDOW).
/*   run lockwindowupdate(frame {&FRAME-NAME}:hwnd). */

  IF piPageNum <> DYNAMIC-FUNCTION('getCurrentPage':U) THEN DO:
      RUN SUPER( INPUT piPageNum).
      RUN VisVisAlleKnapp (FALSE).
  END.
  /* Code placed here will execute AFTER standard behavior.    */
/*   run lockwindowupdate(0). */
  DYNAMIC-FUNCTION("DoLockWindow",?).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendRapportGridHandle wWin 
PROCEDURE SendRapportGridHandle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER hRapportGrid AS HANDLE     NO-UNDO.
  ASSIGN hRapportGrid = h_frapportgrid.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetDivResize wWin 
PROCEDURE SetDivResize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* DYNAMIC-FUNCTION("setNoResizeX",THIS-PROCEDURE:CURRENT-WINDOW,            */
/*                                 FRAME DEFAULT-FRAME:HANDLE,               */
/*                                 "Image-Sko,RECT-3,RECT-4").               */
/* DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,            */
/*                                 FRAME DEFAULT-FRAME:HANDLE,               */
/*                                 "Image-Sko,RECT-1,RECT-2,RECT-3,RECT-4"). */
/* DYNAMIC-FUNCTION("setAddMoveX",THIS-PROCEDURE:CURRENT-WINDOW,             */
/*                                 FRAME DEFAULT-FRAME:HANDLE,               */
/*                                  "Btn_Done,Btn_Help,BUTTON-Browse").      */
/* DYNAMIC-FUNCTION("SetResizeADM2panel",TRUE). */
/* DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,iWidthPix,430,0,0). */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartUtvalg wWin 
PROCEDURE StartUtvalg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM lcWhere       AS CHAR NO-UNDO.
DEFINE VARIABLE hQry          AS HANDLE     NO-UNDO.

cCurrWhere = lcWhere. 
/* ViewHideStopButton(TRUE). */
RUN VisTxtBox IN h_frapportgrid
    ( INPUT "Bygger temporær artikkelliste..." /* CHARACTER */).
RUN Utvalg IN h_dtmpArtBas (lcWhere).
DYNAMIC-FUNCTION('openQuery':U IN h_dtmpartbas).
RUN VisTxtBox IN h_frapportgrid
    ( INPUT "" /* CHARACTER */).
RUN StartSokArtDyn IN hAnropProc (DYNAMIC-FUNCTION('getQueryHandle':U IN h_dtmpartbas),FALSE,"HENTINTERNT" + CHR(1) + DYNAMIC-FUNCTION('getStatusString':U IN h_dtmpartbas)). 
/* glUtvalg = TRUE.                                                    */
/* ViewHideStopButton(FALSE).                                          */
/*                                                                     */
/* hBrowse:HELP = DYNAMIC-FUNCTION("getStatusString" IN h_dtmpartbas). */
/* setBrowseReadOnly().                                                */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tmpUtvalg wWin 
PROCEDURE tmpUtvalg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipAnropProc AS HANDLE     NO-UNDO.
  ASSIGN hAnropProc = ipAnropProc.
  RUN Nullstill.
  RUN Utvalg.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Translogg wWin 
PROCEDURE Translogg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       Vi måste ha denna för att fönsterhanteringen skall bli bra.
               alltså anropa en annan proc för att det skall bli 'snyggare'
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER pcColValues AS CHAR NO-UNDO.
  DYNAMIC-FUNCTION("DoLockWindow",{&WINDOW-NAME}).

  RUN TransloggRun (pcColValues).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TransloggRun wWin 
PROCEDURE TransloggRun :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER pcColValues AS CHAR NO-UNDO.
  DEFINE VARIABLE dTransdato AS DATE       NO-UNDO.
  DEFINE VARIABLE iButikkNr  AS INTEGER    NO-UNDO.
  ASSIGN iButikkNr  = INT(ENTRY(2,pcColValues,CHR(1)))
         dTransdato = DATE(ENTRY(3,pcColValues,CHR(1))).
  RUN initializeObject NO-ERROR.
  RUN selectPage ( INPUT 1 /* INTEGER */).
  RUN hideObject IN h_ftransloggfilter.
  RUN EndreSize2 IN h_frapportgrid (DYNAMIC-FUNCTION('getHeight':U IN h_frapportgrid) +
                                   DYNAMIC-FUNCTION('getHeight':U IN h_folder) /* DECIMAL */ - .01, DYNAMIC-FUNCTION('getWidth':U IN h_frapportgrid /* DECIMAL */)).
  DYNAMIC-FUNCTION("DoLockWindow",?).
  RUN Aktiver IN h_ftransloggfilter (iButikkNr,dTransDato).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Utvalg wWin 
PROCEDURE Utvalg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR llOk    AS LOG  NO-UNDO.
  DEF VAR lcWhere AS CHAR NO-UNDO.

  IF NOT VALID-HANDLE(h_wartbasutvalg) THEN DO:
    {sww.i} 
    RUN wartbasutvalg.w PERSIST SET h_wartbasutvalg (THIS-PROCEDURE,h_dtmpartbas).
    RUN initializeObject IN h_wartbasutvalg.
    {swn.i} 
  END.
  ELSE h_wartbasutvalg:CURRENT-WINDOW:HIDDEN = FALSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisTranslogg wWin 
PROCEDURE VisTranslogg :
/*------------------------------------------------------------------------------
  Purpose:    anropas från eksternt program 
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
     DEFINE INPUT  PARAMETER hQry     AS HANDLE     NO-UNDO.
     DEFINE INPUT  PARAMETER cButiker AS CHARACTER  NO-UNDO.
     DEFINE INPUT  PARAMETER cType AS CHARACTER  NO-UNDO.
     DEFINE VARIABLE iFane AS INTEGER    NO-UNDO.
     ASSIGN iFane = 1.
     RUN selectpage(iFane).
     RUN hideObject IN h_ftransloggfilter.
     RUN EndreSize2 IN h_frapportgrid (DYNAMIC-FUNCTION('getHeight':U IN h_frapportgrid) + 
                                      DYNAMIC-FUNCTION('getHeight':U IN h_folder) /* DECIMAL */ - .03, DYNAMIC-FUNCTION('getWidth':U IN h_frapportgrid /* DECIMAL */)).
/*      RUN setAVfilter IN h_fst_artlager. */
/*      wWin:TOP-ONLY = TRUE.                      */
     DYNAMIC-FUNCTION("DoLockWindow",?).
/*      wWin:TOP-ONLY = FALSE. */
     RUN StartSokArtDyn IN h_ftransloggfilter (hQry,FALSE,cButiker).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisVisAlleKnapp wWin 
PROCEDURE VisVisAlleKnapp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER lVis AS LOGICAL    NO-UNDO.
    B-Vis:HIDDEN IN FRAME {&FRAME-NAME} = NOT lVis.
    IF B-Vis:HIDDEN THEN
        B-Vis:PRIVATE-DATA = "".
    ELSE
        B-Vis:PRIVATE-DATA = "VIS".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fLockVindu wWin 
FUNCTION fLockVindu RETURNS CHARACTER
  ( INPUT lLock AS LOGICAL ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hDetteVindu AS HANDLE     NO-UNDO.
  ASSIGN hDetteVindu = wWin /* THIS-PROCEDURE:CURRENT-WINDOW */
         hDetteVindu:SENSITIVE = NOT lLock.
  IF lLock = FALSE THEN
      APPLY "ENTRY" TO wWin.
  RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION geth_dstlager wWin 
FUNCTION geth_dstlager RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN h_dstlager.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION geth_dstlinje wWin 
FUNCTION geth_dstlinje RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN h_dstlinje.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION geth_frapportgrid wWin 
FUNCTION geth_frapportgrid RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN h_frapportgrid.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION InitializeResize wWin 
FUNCTION InitializeResize RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hTabFrame     AS HANDLE NO-UNDO.

hTabFrame = DYNAMIC-FUNCTION("getContainerHandle" IN h_folder).
DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,hTabFrame,hTabFrame:NAME).

hTabFrame = DYNAMIC-FUNCTION("getContainerHandle" IN h_fbestillingfilter).
DYNAMIC-FUNCTION("setNoResizeX",THIS-PROCEDURE:CURRENT-WINDOW,hTabFrame,hTabFrame:NAME).
DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,hTabFrame,hTabFrame:NAME).

hTabFrame = DYNAMIC-FUNCTION("getContainerHandle" IN h_fbongfilter).
DYNAMIC-FUNCTION("setNoResizeX",THIS-PROCEDURE:CURRENT-WINDOW,hTabFrame,hTabFrame:NAME).
DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,hTabFrame,hTabFrame:NAME).

hTabFrame = DYNAMIC-FUNCTION("getContainerHandle" IN h_fkalkylekontroll).
DYNAMIC-FUNCTION("setNoResizeX",THIS-PROCEDURE:CURRENT-WINDOW,hTabFrame,hTabFrame:NAME).
DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,hTabFrame,hTabFrame:NAME).

hTabFrame = DYNAMIC-FUNCTION("getContainerHandle" IN h_ftransloggfilter).
DYNAMIC-FUNCTION("setNoResizeX",THIS-PROCEDURE:CURRENT-WINDOW,hTabFrame,hTabFrame:NAME).
DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,hTabFrame,hTabFrame:NAME).

hTabFrame = DYNAMIC-FUNCTION("getContainerHandle" IN h_fmodellanalyse).
DYNAMIC-FUNCTION("setNoResizeX",THIS-PROCEDURE:CURRENT-WINDOW,hTabFrame,hTabFrame:NAME).
DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,hTabFrame,hTabFrame:NAME).

hTabFrame = DYNAMIC-FUNCTION("getContainerHandle" IN h_fst_messe).
DYNAMIC-FUNCTION("setNoResizeX",THIS-PROCEDURE:CURRENT-WINDOW,hTabFrame,hTabFrame:NAME).
DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,hTabFrame,hTabFrame:NAME).

hTabFrame = DYNAMIC-FUNCTION("getContainerHandle" IN h_fmanedsrapport).
DYNAMIC-FUNCTION("setNoResizeX",THIS-PROCEDURE:CURRENT-WINDOW,hTabFrame,hTabFrame:NAME).
DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,hTabFrame,hTabFrame:NAME).

hTabFrame = DYNAMIC-FUNCTION("getContainerHandle" IN h_foverforinger).
DYNAMIC-FUNCTION("setNoResizeX",THIS-PROCEDURE:CURRENT-WINDOW,hTabFrame,hTabFrame:NAME).
DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,hTabFrame,hTabFrame:NAME).

hTabFrame = DYNAMIC-FUNCTION("getContainerHandle" IN h_fbongrabatter).
DYNAMIC-FUNCTION("setNoResizeX",THIS-PROCEDURE:CURRENT-WINDOW,hTabFrame,hTabFrame:NAME).
DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,hTabFrame,hTabFrame:NAME).

hTabFrame = DYNAMIC-FUNCTION("getTxtFrame" IN h_frapportgrid).
DYNAMIC-FUNCTION("setNoResizeX",THIS-PROCEDURE:CURRENT-WINDOW,hTabFrame,hTabFrame:NAME).
DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,hTabFrame,hTabFrame:NAME).


DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,1000,100,0,0).

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

