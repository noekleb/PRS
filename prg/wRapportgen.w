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
DEFINE VARIABLE cKundNamn     AS CHARACTER   NO-UNDO.

DEF VAR hOversettGrid2SE    AS HANDLE NO-UNDO.
DEF VAR h_wartbasutvalg     AS HANDLE NO-UNDO.
DEF VAR cCurrWhere          AS CHARACTER NO-UNDO.
DEF VAR hAnropProc          AS HANDLE     NO-UNDO.
DEF VAR cProgram            AS CHAR       NO-UNDO. /* för colonnevalg Excel */
/* DEFINE VARIABLE iWidthPix  AS INTEGER    NO-UNDO. */
/* DEFINE VARIABLE iHeightPix AS INTEGER    NO-UNDO. */
DEFINE VARIABLE cDeleteFolderPage AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cSprak AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cSEfolder AS CHARACTER INIT
 "Avdelning|Huvudgrupp|Varugrupp|Leverantör|Kassör|Säljare|Butik|Artikel|Nonsale|Kund|Medlem|Lager (Art)|Lager (Stat)|Jämförelse|Kampanj"     
                 NO-UNDO.
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
&Scoped-Define ENABLED-OBJECTS B-Excel LINJE-1 LINJE-2 B-Vis ~
B-XprintOmsrapp B-Xprint B-Notepad B-Topplista 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD geth_fstperiode wWin 
FUNCTION geth_fstperiode RETURNS HANDLE
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
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_frapportgrid AS HANDLE NO-UNDO.
DEFINE VARIABLE h_fstlinjeartikkelfilter AS HANDLE NO-UNDO.
DEFINE VARIABLE h_fstlinjeavdelingfilter AS HANDLE NO-UNDO.
DEFINE VARIABLE h_fstlinjebutikkfilter AS HANDLE NO-UNDO.
DEFINE VARIABLE h_fstlinjehovedgrfilter AS HANDLE NO-UNDO.
DEFINE VARIABLE h_fstlinjekampanjefilter AS HANDLE NO-UNDO.
DEFINE VARIABLE h_fstlinjekassererfilter AS HANDLE NO-UNDO.
DEFINE VARIABLE h_fstlinjekundefilter AS HANDLE NO-UNDO.
DEFINE VARIABLE h_fstlinjelevfilter AS HANDLE NO-UNDO.
DEFINE VARIABLE h_fstlinjemedlemfilter AS HANDLE NO-UNDO.
DEFINE VARIABLE h_fstlinjenonsalefilter AS HANDLE NO-UNDO.
DEFINE VARIABLE h_fstlinjeselgerfilter AS HANDLE NO-UNDO.
DEFINE VARIABLE h_fstlinjevargrfilter AS HANDLE NO-UNDO.
DEFINE VARIABLE h_fstperiode AS HANDLE NO-UNDO.
DEFINE VARIABLE h_fstsammenlign AS HANDLE NO-UNDO.
DEFINE VARIABLE h_fst_artlager AS HANDLE NO-UNDO.
DEFINE VARIABLE h_fst_stlager AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Excel 
     IMAGE-UP FILE "icon/excel.bmp":U NO-FOCUS
     LABEL "Excel..." 
     SIZE 4.6 BY 1.05 TOOLTIP "Eksporter til Excel.".

DEFINE BUTTON B-Notepad 
     IMAGE-UP FILE "icon/e-notes.bmp":U NO-FOCUS
     LABEL "Apotek" 
     SIZE 4.6 BY 1.05 TOOLTIP "Apoteksfil".

DEFINE BUTTON B-Topplista 
     IMAGE-UP FILE "icon/e-print.bmp":U NO-FOCUS
     LABEL "Topplista" 
     SIZE 4.6 BY 1.05 TOOLTIP "ArtikelTopplista".

DEFINE BUTTON B-Vis 
     LABEL "Vis / skjul" 
     SIZE 15 BY 1.05.

DEFINE BUTTON B-Xprint 
     IMAGE-UP FILE "icon/e-print.bmp":U NO-FOCUS
     LABEL "Print" 
     SIZE 4.6 BY 1.05 TOOLTIP "XPrint rapport".

DEFINE BUTTON B-XprintOmsrapp 
     IMAGE-UP FILE "icon/e-print.bmp":U NO-FOCUS
     LABEL "Print" 
     SIZE 4.6 BY 1.05 TOOLTIP "Omsettningsrapport".

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
     B-XprintOmsrapp AT ROW 1.19 COL 12
     B-Xprint AT ROW 1.19 COL 7.2
     B-Notepad AT ROW 1.19 COL 40
     B-Topplista AT ROW 1.24 COL 17.4
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
   Design Page: 6
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Rapporter"
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
ON END-ERROR OF wWin /* Rapporter */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Rapporter */
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
ON WINDOW-RESIZED OF wWin /* Rapporter */
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


&Scoped-define SELF-NAME B-Notepad
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Notepad wWin
ON CHOOSE OF B-Notepad IN FRAME fMain /* Apotek */
DO:
    RUN VisaNotepad IN h_frapportgrid.
/*    RUN wApoteksfil.w.*/
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Topplista
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Topplista wWin
ON CHOOSE OF B-Topplista IN FRAME fMain /* Topplista */
DO:
    DEFINE VARIABLE cFilterVerdier AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cColAlign      AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE iPage          AS INTEGER    NO-UNDO.
    DEFINE VARIABLE hAktivHandle   AS HANDLE     NO-UNDO.
    DEFINE VARIABLE cTabLabel      AS CHARACTER  NO-UNDO.
    ASSIGN iPage   = DYNAMIC-FUNCTION('getCurrentPage':U).

    IF iPage <> 8 THEN
    DO:
      MESSAGE "Bara för Artikel-fliken!"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
    END.

    CASE iPage:
        WHEN 1 THEN
            ASSIGN hAktivHandle = h_fstlinjeavdelingfilter.
        WHEN 2 THEN
            ASSIGN hAktivHandle = h_fstlinjehovedgrfilter.
        WHEN 3 THEN
            ASSIGN hAktivHandle = h_fstlinjevargrfilter.
        WHEN 4 THEN
            ASSIGN hAktivHandle = h_fstlinjelevfilter.
        WHEN 5 THEN
            ASSIGN hAktivHandle = h_fstlinjekassererfilter.
        WHEN 6 THEN
            ASSIGN hAktivHandle = h_fstlinjeselgerfilter.
        WHEN 7 THEN
            ASSIGN hAktivHandle = h_fstlinjebutikkfilter.
        WHEN 8 THEN
            ASSIGN hAktivHandle = h_fstlinjeartikkelfilter.
        WHEN 9 THEN
            ASSIGN hAktivHandle = h_fstlinjekundefilter.
        WHEN 10 THEN
            ASSIGN hAktivHandle = h_fstlinjemedlemfilter.
        WHEN 11 THEN
            ASSIGN hAktivHandle = h_fst_artlager.
        WHEN 12 THEN
            ASSIGN hAktivHandle = h_fst_stlager.
        WHEN 13 THEN
            ASSIGN hAktivHandle = h_fstsammenlign.
        WHEN 14 THEN
            ASSIGN hAktivHandle = h_fstlinjekampanjefilter.
    END CASE.
    RUN SendFilterValues IN hAktivHandle (OUTPUT cFilterVerdier, OUTPUT cColAlign) NO-ERROR.
    ASSIGN cFilterVerdier = REPLACE(cFilterverdier,CHR(10)," ")
           cFilterVerdier = REPLACE(cFilterverdier,"AAR","ÅR")
           cFilterVerdier = REPLACE(cFilterverdier,"MANED","MÅNAD").
    ASSIGN cTabLabel      = ENTRY(INT(DYNAMIC-FUNCTION('getCurrentPage':U)),DYNAMIC-FUNCTION('getFolderLabels':U IN h_folder),"|")
           cFilterVerdier = cTabLabel + 
                            (IF cFilterVerdier <> "" THEN CHR(2) ELSE "") + 
                            cFilterVerdier.
    PUBLISH "PrintGrid" ("ARTIKELTOPP",cFilterVerdier,2,"",cColAlign).

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
          ASSIGN hAktivHandle = h_fstlinjeavdelingfilter.
      WHEN 2 THEN
          ASSIGN hAktivHandle = h_fstlinjehovedgrfilter.
      WHEN 3 THEN
          ASSIGN hAktivHandle = h_fstlinjevargrfilter.
      WHEN 4 THEN
          ASSIGN hAktivHandle = h_fstlinjelevfilter.
      WHEN 5 THEN
          ASSIGN hAktivHandle = h_fstlinjekassererfilter.
      WHEN 6 THEN
          ASSIGN hAktivHandle = h_fstlinjeselgerfilter.
      WHEN 7 THEN
          ASSIGN hAktivHandle = h_fstlinjebutikkfilter.
      WHEN 8 THEN
          ASSIGN hAktivHandle = h_fstlinjeartikkelfilter.
      WHEN 9 THEN
          ASSIGN hAktivHandle = h_fstlinjekundefilter.
      WHEN 10 THEN
          ASSIGN hAktivHandle = h_fstlinjemedlemfilter.
      WHEN 11 THEN
          ASSIGN hAktivHandle = h_fst_artlager.
      WHEN 12 THEN
          ASSIGN hAktivHandle = h_fst_stlager.
      WHEN 13 THEN
          ASSIGN hAktivHandle = h_fstsammenlign.
      WHEN 14 THEN
          ASSIGN hAktivHandle = h_fstlinjekampanjefilter.
  END CASE.
  RUN SendFilterValues IN hAktivHandle (OUTPUT cFilterVerdier, OUTPUT cColAlign) NO-ERROR.
  ASSIGN cFilterVerdier = REPLACE(cFilterverdier,CHR(10)," ")
         cFilterVerdier = REPLACE(cFilterverdier,"AAR","ÅR")
         cFilterVerdier = REPLACE(cFilterverdier,"MANED","MÅNAD").
  ASSIGN cTabLabel      = ENTRY(INT(DYNAMIC-FUNCTION('getCurrentPage':U)),DYNAMIC-FUNCTION('getFolderLabels':U IN h_folder),"|")
         cFilterVerdier = cTabLabel + 
                          (IF cFilterVerdier <> "" THEN CHR(2) ELSE "") + 
                          cFilterVerdier.
  PUBLISH "PrintGrid" ("XPRINT",cFilterVerdier,2,"",cColAlign).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-XprintOmsrapp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-XprintOmsrapp wWin
ON CHOOSE OF B-XprintOmsrapp IN FRAME fMain /* Print */
DO:
  DEFINE VARIABLE cFilterVerdier AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cField#List    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cFeltListe     AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cFeltListeArt  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cColAlign      AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iPage          AS INTEGER    NO-UNDO.
  DEFINE VARIABLE hAktivHandle   AS HANDLE     NO-UNDO.
  ASSIGN iPage      = DYNAMIC-FUNCTION('getCurrentPage':U)
         cFeltListe = "DataObjekt,Beskrivelse,PerLinTxt,AntSolgt,VerdiSolgt,Solgt%,MvaVerdi,DbKr,Db%"
         cFeltListeArt = "DataObjekt,Beskrivelse,VgLopNr,LevKod,PerLinTxt,AntSolgt,VerdiSolgt,Solgt%,MvaVerdi,DbKr,Db%".
  IF iPage > 10 THEN DO:
      MESSAGE "Rapport finnes ikke for denne typen."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
  END.
  CASE iPage:
      WHEN 1 THEN
          ASSIGN hAktivHandle = h_fstlinjeavdelingfilter.
      WHEN 2 THEN
          ASSIGN hAktivHandle = h_fstlinjehovedgrfilter.
      WHEN 3 THEN
          ASSIGN hAktivHandle = h_fstlinjevargrfilter.
      WHEN 4 THEN
          ASSIGN hAktivHandle = h_fstlinjelevfilter.
      WHEN 5 THEN
          ASSIGN hAktivHandle = h_fstlinjekassererfilter.
      WHEN 6 THEN
          ASSIGN hAktivHandle = h_fstlinjeselgerfilter.
      WHEN 7 THEN
          ASSIGN hAktivHandle = h_fstlinjebutikkfilter.
      WHEN 8 THEN
          ASSIGN hAktivHandle = h_fstlinjeartikkelfilter
                 cFeltListe   = cFeltListeArt.
      WHEN 9 THEN
          ASSIGN hAktivHandle = h_fstlinjekundefilter.
      WHEN 10 THEN
          ASSIGN hAktivHandle = h_fstlinjemedlemfilter.
      WHEN 11 THEN
          ASSIGN hAktivHandle = h_fst_artlager.
  END CASE.
  
  RUN SendFilterValues IN hAktivHandle (OUTPUT cFilterVerdier, OUTPUT cColAlign) NO-ERROR.
  ASSIGN cFilterVerdier = CHR(2) + REPLACE(cFilterverdier,CHR(10)," "). /* första är blank ie rubrik hårkodad */
  RUN SendFeltInfo IN hAktivHandle (INPUT cFeltListe,OUTPUT cField#List,OUTPUT cColAlign) NO-ERROR.
  PUBLISH "PrintGrid" ("XPRINTOMS",cFilterVerdier,1,cField#List,cColAlign).
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
/* cProgram blir parameter till grid vi kolonneval */
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
             INPUT  'FolderLabels':U + 'Avdeling|Hovedgruppe|Varegruppe|Leverandør|Kasserer|Selger|Butikk|Artikkel|Nonsale|Kunde|Medlem|Lager (Art)|Lager (Stat)|Sammenligning|Kampanje' + 'FolderTabWidth0FolderFont-1HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_folder ).
       RUN repositionObject IN h_folder ( 23.38 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_folder ( 9.76 , 202.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'prg/fstperiode.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'LogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_fstperiode ).
       RUN repositionObject IN h_fstperiode ( 24.81 , 6.00 ) NO-ERROR.
       /* Size in AB:  ( 6.14 , 36.40 ) */

       /* Initialize other pages that this page requires. */
       RUN initPages ('8,1,7,2,15,5,10,4,11,9,6,3,14,12,13':U) NO-ERROR.

       /* Links to SmartFrame h_frapportgrid. */
       RUN addLink ( h_fstlinjeartikkelfilter , 'ClearGrid':U , h_frapportgrid ).
       RUN addLink ( h_fstlinjeartikkelfilter , 'FeltVerdier':U , h_frapportgrid ).
       RUN addLink ( h_fstlinjeartikkelfilter , 'LoadGrid':U , h_frapportgrid ).
       RUN addLink ( h_fstlinjeartikkelfilter , 'Summer':U , h_frapportgrid ).
       RUN addLink ( h_fstlinjeartikkelfilter , 'VisKun':U , h_frapportgrid ).
       RUN addLink ( h_fstlinjeartikkelfilter , 'VisTxtBox':U , h_frapportgrid ).
       RUN addLink ( h_fstlinjeartikkelfilter , 'X%Solgt':U , h_frapportgrid ).
       RUN addLink ( h_fstlinjeavdelingfilter , 'ClearGrid':U , h_frapportgrid ).
       RUN addLink ( h_fstlinjeavdelingfilter , 'FeltVerdier':U , h_frapportgrid ).
       RUN addLink ( h_fstlinjeavdelingfilter , 'GridLink':U , h_frapportgrid ).
       RUN addLink ( h_fstlinjeavdelingfilter , 'LoadGrid':U , h_frapportgrid ).
       RUN addLink ( h_fstlinjeavdelingfilter , 'Summer':U , h_frapportgrid ).
       RUN addLink ( h_fstlinjeavdelingfilter , 'VisTxtBox':U , h_frapportgrid ).
       RUN addLink ( h_fstlinjeavdelingfilter , 'X%Solgt':U , h_frapportgrid ).
       RUN addLink ( h_fstlinjebutikkfilter , 'ClearGrid':U , h_frapportgrid ).
       RUN addLink ( h_fstlinjebutikkfilter , 'LoadGrid':U , h_frapportgrid ).
       RUN addLink ( h_fstlinjebutikkfilter , 'Summer':U , h_frapportgrid ).
       RUN addLink ( h_fstlinjebutikkfilter , 'VisKun':U , h_frapportgrid ).
       RUN addLink ( h_fstlinjebutikkfilter , 'VisTxtBox':U , h_frapportgrid ).
       RUN addLink ( h_fstlinjebutikkfilter , 'X%Solgt':U , h_frapportgrid ).
       RUN addLink ( h_fstlinjehovedgrfilter , 'ClearGrid':U , h_frapportgrid ).
       RUN addLink ( h_fstlinjehovedgrfilter , 'FeltVerdier':U , h_frapportgrid ).
       RUN addLink ( h_fstlinjehovedgrfilter , 'LoadGrid':U , h_frapportgrid ).
       RUN addLink ( h_fstlinjehovedgrfilter , 'Summer':U , h_frapportgrid ).
       RUN addLink ( h_fstlinjehovedgrfilter , 'VisTxtBox':U , h_frapportgrid ).
       RUN addLink ( h_fstlinjehovedgrfilter , 'X%Solgt':U , h_frapportgrid ).
       RUN addLink ( h_fstlinjekampanjefilter , 'ClearGrid':U , h_frapportgrid ).
       RUN addLink ( h_fstlinjekampanjefilter , 'LoadGrid':U , h_frapportgrid ).
       RUN addLink ( h_fstlinjekampanjefilter , 'Summer':U , h_frapportgrid ).
       RUN addLink ( h_fstlinjekampanjefilter , 'VisTxtBox':U , h_frapportgrid ).
       RUN addLink ( h_fstlinjekampanjefilter , 'X%Solgt':U , h_frapportgrid ).
       RUN addLink ( h_fstlinjekassererfilter , 'ClearGrid':U , h_frapportgrid ).
       RUN addLink ( h_fstlinjekassererfilter , 'LoadGrid':U , h_frapportgrid ).
       RUN addLink ( h_fstlinjekassererfilter , 'Summer':U , h_frapportgrid ).
       RUN addLink ( h_fstlinjekassererfilter , 'VisKun':U , h_frapportgrid ).
       RUN addLink ( h_fstlinjekassererfilter , 'VisTxtBox':U , h_frapportgrid ).
       RUN addLink ( h_fstlinjekundefilter , 'ClearGrid':U , h_frapportgrid ).
       RUN addLink ( h_fstlinjekundefilter , 'LoadGrid':U , h_frapportgrid ).
       RUN addLink ( h_fstlinjekundefilter , 'Summer':U , h_frapportgrid ).
       RUN addLink ( h_fstlinjekundefilter , 'VisKun':U , h_frapportgrid ).
       RUN addLink ( h_fstlinjekundefilter , 'VisTxtBox':U , h_frapportgrid ).
       RUN addLink ( h_fstlinjekundefilter , 'X%Solgt':U , h_frapportgrid ).
       RUN addLink ( h_fstlinjelevfilter , 'ClearGrid':U , h_frapportgrid ).
       RUN addLink ( h_fstlinjelevfilter , 'LoadGrid':U , h_frapportgrid ).
       RUN addLink ( h_fstlinjelevfilter , 'Summer':U , h_frapportgrid ).
       RUN addLink ( h_fstlinjelevfilter , 'VisKun':U , h_frapportgrid ).
       RUN addLink ( h_fstlinjelevfilter , 'VisTxtBox':U , h_frapportgrid ).
       RUN addLink ( h_fstlinjelevfilter , 'X%Solgt':U , h_frapportgrid ).
       RUN addLink ( h_fstlinjemedlemfilter , 'ClearGrid':U , h_frapportgrid ).
       RUN addLink ( h_fstlinjemedlemfilter , 'FeltVerdier':U , h_frapportgrid ).
       RUN addLink ( h_fstlinjemedlemfilter , 'FillTTFelter':U , h_frapportgrid ).
       RUN addLink ( h_fstlinjemedlemfilter , 'LoadGrid':U , h_frapportgrid ).
       RUN addLink ( h_fstlinjemedlemfilter , 'Summer':U , h_frapportgrid ).
       RUN addLink ( h_fstlinjemedlemfilter , 'VisKun':U , h_frapportgrid ).
       RUN addLink ( h_fstlinjemedlemfilter , 'VisTxtBox':U , h_frapportgrid ).
       RUN addLink ( h_fstlinjemedlemfilter , 'X%Solgt':U , h_frapportgrid ).
       RUN addLink ( h_fstlinjenonsalefilter , 'ClearGrid':U , h_frapportgrid ).
       RUN addLink ( h_fstlinjenonsalefilter , 'FeltVerdier':U , h_frapportgrid ).
       RUN addLink ( h_fstlinjenonsalefilter , 'LoadGrid':U , h_frapportgrid ).
       RUN addLink ( h_fstlinjenonsalefilter , 'Summer':U , h_frapportgrid ).
       RUN addLink ( h_fstlinjenonsalefilter , 'VisKun':U , h_frapportgrid ).
       RUN addLink ( h_fstlinjenonsalefilter , 'VisTxtBox':U , h_frapportgrid ).
       RUN addLink ( h_fstlinjenonsalefilter , 'X%Solgt':U , h_frapportgrid ).
       RUN addLink ( h_fstlinjeselgerfilter , 'ClearGrid':U , h_frapportgrid ).
       RUN addLink ( h_fstlinjeselgerfilter , 'LoadGrid':U , h_frapportgrid ).
       RUN addLink ( h_fstlinjeselgerfilter , 'Summer':U , h_frapportgrid ).
       RUN addLink ( h_fstlinjeselgerfilter , 'VisKun':U , h_frapportgrid ).
       RUN addLink ( h_fstlinjeselgerfilter , 'VisTxtBox':U , h_frapportgrid ).
       RUN addLink ( h_fstlinjeselgerfilter , 'X%Solgt':U , h_frapportgrid ).
       RUN addLink ( h_fstlinjevargrfilter , 'ClearGrid':U , h_frapportgrid ).
       RUN addLink ( h_fstlinjevargrfilter , 'FeltVerdier':U , h_frapportgrid ).
       RUN addLink ( h_fstlinjevargrfilter , 'LoadGrid':U , h_frapportgrid ).
       RUN addLink ( h_fstlinjevargrfilter , 'Summer':U , h_frapportgrid ).
       RUN addLink ( h_fstlinjevargrfilter , 'VisKun':U , h_frapportgrid ).
       RUN addLink ( h_fstlinjevargrfilter , 'VisTxtBox':U , h_frapportgrid ).
       RUN addLink ( h_fstlinjevargrfilter , 'X%Solgt':U , h_frapportgrid ).
       RUN addLink ( h_fstsammenlign , 'ClearGrid':U , h_frapportgrid ).
       RUN addLink ( h_fstsammenlign , 'VisKun':U , h_frapportgrid ).
       RUN addLink ( h_fst_artlager , 'ClearGrid':U , h_frapportgrid ).
       RUN addLink ( h_fst_artlager , 'ColVerdier':U , h_frapportgrid ).
       RUN addLink ( h_fst_artlager , 'FeltVerdier':U , h_frapportgrid ).
       RUN addLink ( h_fst_artlager , 'FillTT':U , h_frapportgrid ).
       RUN addLink ( h_fst_artlager , 'LoadGrid':U , h_frapportgrid ).
       RUN addLink ( h_fst_artlager , 'Summer':U , h_frapportgrid ).
       RUN addLink ( h_fst_artlager , 'VisKun':U , h_frapportgrid ).
       RUN addLink ( h_fst_artlager , 'VisTxtBox':U , h_frapportgrid ).
       RUN addLink ( h_fst_stlager , 'ClearGrid':U , h_frapportgrid ).
       RUN addLink ( h_fst_stlager , 'ColVerdier':U , h_frapportgrid ).
       RUN addLink ( h_fst_stlager , 'FeltVerdier':U , h_frapportgrid ).
       RUN addLink ( h_fst_stlager , 'FillTT':U , h_frapportgrid ).
       RUN addLink ( h_fst_stlager , 'LoadGrid':U , h_frapportgrid ).
       RUN addLink ( h_fst_stlager , 'Summer':U , h_frapportgrid ).
       RUN addLink ( h_fst_stlager , 'VisKun':U , h_frapportgrid ).
       RUN addLink ( h_fst_stlager , 'VisTxtBox':U , h_frapportgrid ).
       RUN addLink ( THIS-PROCEDURE , 'PrintGrid':U , h_frapportgrid ).
       RUN addLink ( h_frapportgrid , 'GetWindowH':U , THIS-PROCEDURE ).

       /* Links to SmartFolder h_folder. */
       RUN addLink ( h_folder , 'Page':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_frapportgrid ,
             B-Vis:HANDLE IN FRAME fMain , 'AFTER':U ).
       RUN adjustTabOrder ( h_folder ,
             h_frapportgrid , 'AFTER':U ).
       RUN adjustTabOrder ( h_fstperiode ,
             h_folder , 'AFTER':U ).
    END. /* Page 0 */
    WHEN 1 THEN DO:
       RUN constructObject (
             INPUT  'prg/fstlinjeavdelingfilter.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'LogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_fstlinjeavdelingfilter ).
       RUN repositionObject IN h_fstlinjeavdelingfilter ( 24.81 , 47.00 ) NO-ERROR.
       /* Size in AB:  ( 5.00 , 65.00 ) */

       /* Links to SmartFrame h_fstlinjeavdelingfilter. */
       RUN addLink ( h_folder , 'Page':U , h_fstlinjeavdelingfilter ).
       RUN addLink ( h_fstlinjeavdelingfilter , 'GetWindowH':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_fstlinjeavdelingfilter ,
             h_fstperiode , 'AFTER':U ).
    END. /* Page 1 */
    WHEN 2 THEN DO:
       RUN constructObject (
             INPUT  'prg/fstlinjehovedgrfilter.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'LogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_fstlinjehovedgrfilter ).
       RUN repositionObject IN h_fstlinjehovedgrfilter ( 24.81 , 47.00 ) NO-ERROR.
       /* Size in AB:  ( 5.00 , 74.20 ) */

       /* Links to SmartFrame h_fstlinjehovedgrfilter. */
       RUN addLink ( h_fstlinjehovedgrfilter , 'GetWindowH':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_fstlinjehovedgrfilter ,
             h_fstperiode , 'AFTER':U ).
    END. /* Page 2 */
    WHEN 3 THEN DO:
       RUN constructObject (
             INPUT  'prg/fstlinjevargrfilter.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'LogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_fstlinjevargrfilter ).
       RUN repositionObject IN h_fstlinjevargrfilter ( 24.81 , 47.00 ) NO-ERROR.
       /* Size in AB:  ( 5.00 , 67.00 ) */

       /* Links to SmartFrame h_fstlinjevargrfilter. */
       RUN addLink ( h_fstlinjevargrfilter , 'GetWindowH':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_fstlinjevargrfilter ,
             h_fstperiode , 'AFTER':U ).
    END. /* Page 3 */
    WHEN 4 THEN DO:
       RUN constructObject (
             INPUT  'prg/fstlinjelevfilter.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'LogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_fstlinjelevfilter ).
       RUN repositionObject IN h_fstlinjelevfilter ( 24.81 , 47.00 ) NO-ERROR.
       /* Size in AB:  ( 5.76 , 106.80 ) */

       /* Links to SmartFrame h_fstlinjelevfilter. */
       RUN addLink ( h_fstlinjelevfilter , 'GetWindowH':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_fstlinjelevfilter ,
             h_fstperiode , 'AFTER':U ).
    END. /* Page 4 */
    WHEN 5 THEN DO:
       RUN constructObject (
             INPUT  'prg/fstlinjekassererfilter.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'LogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_fstlinjekassererfilter ).
       RUN repositionObject IN h_fstlinjekassererfilter ( 24.81 , 47.00 ) NO-ERROR.
       /* Size in AB:  ( 5.71 , 106.60 ) */

       /* Links to SmartFrame h_fstlinjekassererfilter. */
       RUN addLink ( h_fstlinjekassererfilter , 'GetWindowH':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_fstlinjekassererfilter ,
             h_fstperiode , 'AFTER':U ).
    END. /* Page 5 */
    WHEN 6 THEN DO:
       RUN constructObject (
             INPUT  'prg/fstlinjeselgerfilter.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'LogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_fstlinjeselgerfilter ).
       RUN repositionObject IN h_fstlinjeselgerfilter ( 24.81 , 47.00 ) NO-ERROR.
       /* Size in AB:  ( 5.71 , 107.20 ) */

       /* Links to SmartFrame h_fstlinjeselgerfilter. */
       RUN addLink ( h_fstlinjeselgerfilter , 'GetWindowH':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_fstlinjeselgerfilter ,
             h_fstperiode , 'AFTER':U ).
    END. /* Page 6 */
    WHEN 7 THEN DO:
       RUN constructObject (
             INPUT  'prg/fstlinjebutikkfilter.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'LogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_fstlinjebutikkfilter ).
       RUN repositionObject IN h_fstlinjebutikkfilter ( 24.81 , 47.00 ) NO-ERROR.
       /* Size in AB:  ( 5.00 , 65.00 ) */

       /* Links to SmartFrame h_fstlinjebutikkfilter. */
       RUN addLink ( h_fstlinjebutikkfilter , 'GetWindowH':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_fstlinjebutikkfilter ,
             h_fstperiode , 'AFTER':U ).
    END. /* Page 7 */
    WHEN 8 THEN DO:
       RUN constructObject (
             INPUT  'prg/fstlinjeartikkelfilter.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'LogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_fstlinjeartikkelfilter ).
       RUN repositionObject IN h_fstlinjeartikkelfilter ( 24.81 , 47.00 ) NO-ERROR.
       /* Size in AB:  ( 5.67 , 148.80 ) */

       /* Links to SmartFrame h_fstlinjeartikkelfilter. */
       RUN addLink ( h_fstlinjeartikkelfilter , 'GetWindowH':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_fstlinjeartikkelfilter ,
             h_fstperiode , 'AFTER':U ).
    END. /* Page 8 */
    WHEN 9 THEN DO:
       RUN constructObject (
             INPUT  'prg/fstlinjenonsalefilter.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'LogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_fstlinjenonsalefilter ).
       RUN repositionObject IN h_fstlinjenonsalefilter ( 24.81 , 47.00 ) NO-ERROR.
       /* Size in AB:  ( 5.71 , 148.60 ) */

       /* Links to SmartFrame h_fstlinjenonsalefilter. */
       RUN addLink ( h_fstlinjenonsalefilter , 'GetWindowH':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_fstlinjenonsalefilter ,
             h_fstperiode , 'AFTER':U ).
    END. /* Page 9 */
    WHEN 10 THEN DO:
       RUN constructObject (
             INPUT  'prg/fstlinjekundefilter.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'LogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_fstlinjekundefilter ).
       RUN repositionObject IN h_fstlinjekundefilter ( 24.81 , 47.00 ) NO-ERROR.
       /* Size in AB:  ( 5.00 , 79.20 ) */

       /* Links to SmartFrame h_fstlinjekundefilter. */
       RUN addLink ( h_fstlinjekundefilter , 'GetWindowH':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_fstlinjekundefilter ,
             h_fstperiode , 'AFTER':U ).
    END. /* Page 10 */
    WHEN 11 THEN DO:
       RUN constructObject (
             INPUT  'prg/fstlinjemedlemfilter.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'LogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_fstlinjemedlemfilter ).
       RUN repositionObject IN h_fstlinjemedlemfilter ( 24.81 , 47.00 ) NO-ERROR.
       /* Size in AB:  ( 6.67 , 116.40 ) */

       /* Links to SmartFrame h_fstlinjemedlemfilter. */
       RUN addLink ( h_fstlinjemedlemfilter , 'GetWindowH':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_fstlinjemedlemfilter ,
             h_fstperiode , 'AFTER':U ).
    END. /* Page 11 */
    WHEN 12 THEN DO:
       RUN constructObject (
             INPUT  'prg/fst_artlager.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'LogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_fst_artlager ).
       RUN repositionObject IN h_fst_artlager ( 24.81 , 6.00 ) NO-ERROR.
       /* Size in AB:  ( 6.62 , 174.00 ) */

       /* Links to SmartFrame h_fst_artlager. */
       RUN addLink ( h_fst_artlager , 'GetWindowH':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_fst_artlager ,
             h_fstperiode , 'AFTER':U ).
    END. /* Page 12 */
    WHEN 13 THEN DO:
       RUN constructObject (
             INPUT  'prg/fst_stlager.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'LogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_fst_stlager ).
       RUN repositionObject IN h_fst_stlager ( 24.81 , 6.00 ) NO-ERROR.
       /* Size in AB:  ( 6.62 , 174.00 ) */

       /* Links to SmartFrame h_fst_stlager. */
       RUN addLink ( h_fst_stlager , 'GetWindowH':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_fst_stlager ,
             h_fstperiode , 'AFTER':U ).
    END. /* Page 13 */
    WHEN 14 THEN DO:
       RUN constructObject (
             INPUT  'prg/fstsammenlign.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'LogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_fstsammenlign ).
       RUN repositionObject IN h_fstsammenlign ( 24.81 , 6.00 ) NO-ERROR.
       /* Size in AB:  ( 7.24 , 139.80 ) */

       /* Links to SmartFrame h_fstsammenlign. */
       RUN addLink ( h_fstsammenlign , 'GetWindowH':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_fstsammenlign ,
             h_fstperiode , 'AFTER':U ).
    END. /* Page 14 */
    WHEN 15 THEN DO:
       RUN constructObject (
             INPUT  'prg/fstlinjekampanjefilter.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'LogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_fstlinjekampanjefilter ).
       RUN repositionObject IN h_fstlinjekampanjefilter ( 24.81 , 47.00 ) NO-ERROR.
       /* Size in AB:  ( 5.71 , 148.60 ) */

       /* Links to SmartFrame h_fstlinjekampanjefilter. */
       RUN addLink ( h_fstlinjekampanjefilter , 'GetWindowH':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_fstlinjekampanjefilter ,
             h_fstperiode , 'AFTER':U ).
    END. /* Page 15 */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AutoAktiver wWin 
PROCEDURE AutoAktiver :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER cFraStType AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER cVerdi     AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER cRowId     AS CHARACTER  NO-UNDO.
  CASE cFraStType:
      WHEN "AVDELING" THEN DO:
          RUN SelectPage(2).
          RUN AutoAktiver IN h_fstlinjehovedgrfilter (cVerdi,cRowId).
      END.
      WHEN "HOVEDGR" THEN DO:
          RUN SelectPage(3).
          RUN AutoAktiver IN h_fstlinjevargrfilter (cVerdi,cRowId).
      END.
      WHEN "VAREGR" THEN DO:
          RUN SelectPage(8).
          RUN AutoAktiver IN h_fstlinjeartikkelfilter (cVerdi,cRowId).
      END.
  END CASE.

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
  ENABLE B-Excel LINJE-1 LINJE-2 B-Vis B-XprintOmsrapp B-Xprint B-Notepad 
         B-Topplista 
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
    RUN repositionObject IN h_fstlinjeartikkelfilter
       ( INPUT DYNAMIC-FUNCTION('getRow':U IN h_fstlinjeartikkelfilter) + dRowDiff /* DECIMAL */,
         INPUT DYNAMIC-FUNCTION('getCol':U IN h_fstlinjeartikkelfilter) /* DECIMAL */).
    RUN repositionObject IN h_fstlinjenonsalefilter
       ( INPUT DYNAMIC-FUNCTION('getRow':U IN h_fstlinjenonsalefilter) + dRowDiff /* DECIMAL */,
         INPUT DYNAMIC-FUNCTION('getCol':U IN h_fstlinjenonsalefilter) /* DECIMAL */).
    RUN repositionObject IN h_fstlinjeavdelingfilter
       ( INPUT DYNAMIC-FUNCTION('getRow':U IN h_fstlinjeavdelingfilter) + dRowDiff /* DECIMAL */,
         INPUT DYNAMIC-FUNCTION('getCol':U IN h_fstlinjeavdelingfilter) /* DECIMAL */).
    RUN repositionObject IN h_fstlinjebutikkfilter
       ( INPUT DYNAMIC-FUNCTION('getRow':U IN h_fstlinjebutikkfilter) + dRowDiff /* DECIMAL */,
         INPUT DYNAMIC-FUNCTION('getCol':U IN h_fstlinjebutikkfilter) /* DECIMAL */).
    RUN repositionObject IN h_fstlinjehovedgrfilter
       ( INPUT DYNAMIC-FUNCTION('getRow':U IN h_fstlinjehovedgrfilter) + dRowDiff /* DECIMAL */,
         INPUT DYNAMIC-FUNCTION('getCol':U IN h_fstlinjehovedgrfilter) /* DECIMAL */).
    RUN repositionObject IN h_fstlinjekassererfilter
       ( INPUT DYNAMIC-FUNCTION('getRow':U IN h_fstlinjekassererfilter) + dRowDiff /* DECIMAL */,
         INPUT DYNAMIC-FUNCTION('getCol':U IN h_fstlinjehovedgrfilter) /* DECIMAL */).
    RUN repositionObject IN h_fstlinjelevfilter
       ( INPUT DYNAMIC-FUNCTION('getRow':U IN h_fstlinjelevfilter) + dRowDiff /* DECIMAL */,
         INPUT DYNAMIC-FUNCTION('getCol':U IN h_fstlinjelevfilter) /* DECIMAL */).
    RUN repositionObject IN h_fstlinjeselgerfilter
       ( INPUT DYNAMIC-FUNCTION('getRow':U IN h_fstlinjeselgerfilter) + dRowDiff /* DECIMAL */,
         INPUT DYNAMIC-FUNCTION('getCol':U IN h_fstlinjeselgerfilter) /* DECIMAL */).
    RUN repositionObject IN h_fstlinjevargrfilter
       ( INPUT DYNAMIC-FUNCTION('getRow':U IN h_fstlinjevargrfilter) + dRowDiff /* DECIMAL */,
         INPUT DYNAMIC-FUNCTION('getCol':U IN h_fstlinjevargrfilter) /* DECIMAL */).
    RUN repositionObject IN h_fstperiode
       ( INPUT DYNAMIC-FUNCTION('getRow':U IN h_fstperiode) + dRowDiff /* DECIMAL */,
         INPUT DYNAMIC-FUNCTION('getCol':U IN h_fstperiode) /* DECIMAL */).
    RUN repositionObject IN h_fstlinjekundefilter
       ( INPUT DYNAMIC-FUNCTION('getRow':U IN h_fstlinjekundefilter) + dRowDiff /* DECIMAL */,
         INPUT DYNAMIC-FUNCTION('getCol':U IN h_fstlinjekundefilter) /* DECIMAL */).
    RUN repositionObject IN h_fstlinjemedlemfilter
       ( INPUT DYNAMIC-FUNCTION('getRow':U IN h_fstlinjemedlemfilter) + dRowDiff /* DECIMAL */,
         INPUT DYNAMIC-FUNCTION('getCol':U IN h_fstlinjemedlemfilter) /* DECIMAL */).
    RUN repositionObject IN h_fst_artlager
       ( INPUT DYNAMIC-FUNCTION('getRow':U IN h_fst_artlager) + dRowDiff /* DECIMAL */,
         INPUT DYNAMIC-FUNCTION('getCol':U IN h_fst_artlager) /* DECIMAL */).
    RUN repositionObject IN h_fst_stlager
       ( INPUT DYNAMIC-FUNCTION('getRow':U IN h_fst_stlager) + dRowDiff /* DECIMAL */,
         INPUT DYNAMIC-FUNCTION('getCol':U IN h_fst_stlager) /* DECIMAL */).
    RUN repositionObject IN h_fstsammenlign
       ( INPUT DYNAMIC-FUNCTION('getRow':U IN h_fstsammenlign) + dRowDiff /* DECIMAL */,
         INPUT DYNAMIC-FUNCTION('getCol':U IN h_fstsammenlign) /* DECIMAL */).
    RUN repositionObject IN h_fstlinjekampanjefilter
       ( INPUT DYNAMIC-FUNCTION('getRow':U IN h_fstlinjekampanjefilter) + dRowDiff /* DECIMAL */,
         INPUT DYNAMIC-FUNCTION('getCol':U IN h_fstlinjekampanjefilter) /* DECIMAL */).
    
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getBrukerLng wWin 
PROCEDURE getBrukerLng :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER cLng AS CHARACTER   NO-UNDO.
  cLng = IF AVAIL bruker THEN Bruker.Lng ELSE "".

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
  DEFINE VARIABLE iButikkNr   AS INTEGER    NO-UNDO.
  DEFINE VARIABLE dTransDato  AS DATE       NO-UNDO.
  DEFINE VARIABLE iNumFolders AS INTEGER     NO-UNDO.
  DEFINE VARIABLE ii AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iTst AS INTEGER     NO-UNDO.
  /* Code placed here will execute PRIOR to standard behavior. */
/*   IF SESSION:WIDTH-PIXELS = 1440 AND        */
/*      SESSION:HEIGHT-PIXELS = 900 THEN       */
/*       ASSIGN iWindowWidth  = 1430           */
/*              iWindowHeight = 808            */
/*              dFolderRow    = 31.4           */
/*              dGridheight   = 28.8.          */
/*   ELSE IF SESSION:WIDTH-PIXELS >= 1280 THEN */
/*       ASSIGN iWindowWidth  = 1270           */
/*              iWindowHeight = 934            */
/*              dFolderRow    = 37.4           */
/*              dGridheight   = 34.7.          */
/*   ELSE IF SESSION:WIDTH-PIXELS >= 1024 THEN */
/*             ASSIGN iWindowWidth = 1024      */
/*                    iWindowHeight = 680      */
/*                    dFolderRow    = 25.3     */
/*                    dGridheight   = 22.5.    */
/*   ASSIGN wWin:X = 1                         */
/*          wWin:Y = 1.                        */
/* /*   IF SESSION:WIDTH-PIXELS > 1024 THEN */ */
/*       RUN EndreSize.                        */
  IF ENTRY(2,PROGRAM-NAME(2)," ") BEGINS "wtmpartbas" THEN
      DYNAMIC-FUNCTION("DoLockWindow",{&WINDOW-NAME}).
  /* LagerVare pg-name(2) */
/*   ASSIGN iWidthPix  = {&WINDOW-NAME}:WIDTH-PIXELS   */
/*          iHeightPix = {&WINDOW-NAME}:HEIGHT-PIXELS. */
  
  ASSIGN {&WINDOW-NAME}:X = 1
         {&WINDOW-NAME}:Y = 1.
  FIND bruker WHERE bruker.brukerid = USERID("skotex") NO-LOCK NO-ERROR.
  IF AVAIL bruker AND CAN-DO("SE,SVE",Bruker.Lng) THEN DO:
      IF NUM-ENTRIES(DYNAMIC-FUNCTION('getFolderLabels':U IN h_folder),"|") =
         NUM-ENTRIES(cSEfolder,"|") THEN
          DYNAMIC-FUNCTION('setFolderLabels':U IN h_folder,
     INPUT cSEfolder /* CHARACTER */).
     /*
     &IF DEFINED(UIB_IS_RUNNING) = 0 &THEN
         RUN oversettGrid2SE.p PERSISTENT SET hOversettGrid2SE.
         THIS-PROCEDURE:ADD-SUPER-PROCEDURE(hOversettGrid2SE).
     &ENDIF
     */    
         
     RUN oversettGrid2SE.p PERSISTENT SET hOversettGrid2SE.
     THIS-PROCEDURE:ADD-SUPER-PROCEDURE(hOversettGrid2SE).
  END.
  IF bruker.brukertype > 1 THEN
       {syspara.i 6 12 1 cDeleteFolderPage}
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
  RUN SUPER.
  /* Code placed here will execute AFTER standard behavior.    */
/*   RUN SetDivResize. */
  B-Vis:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
/*  {syspara.i 1 1 103 cKundNamn}
  IF cKundNamn = "" THEN
     B-NotePad:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.*/
  InitializeResize().

  {&WINDOW-NAME}:WIDTH-PIXELS = SESSION:WIDTH-PIXELS - 10.
  {&WINDOW-NAME}:HEIGHT-PIXELS = SESSION:HEIGHT-PIXELS - 110.
  APPLY "WINDOW-RESIZED" TO {&WINDOW-NAME}.
  RUN selectPage (1).
  RUN viewPage(1).

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrintKnappHidden wWin 
PROCEDURE PrintKnappHidden :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER lHidden AS LOGICAL    NO-UNDO.
    ASSIGN B-XprintOmsrapp:HIDDEN IN FRAME {&FRAME-NAME} = lHidden.
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
      RUN PrintKnappHidden(piPageNum > 10).
      RUN VisVisAlleKnapp (FALSE).
  END.
  RUN SUPER( INPUT piPageNum).

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Vislager wWin 
PROCEDURE Vislager :
/*------------------------------------------------------------------------------
  Purpose:    anropas från eksternt program 
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
     DEFINE INPUT  PARAMETER hQry     AS HANDLE     NO-UNDO.
     DEFINE INPUT  PARAMETER cButiker AS CHARACTER  NO-UNDO.
     RUN selectpage(11).
     RUN hideObject IN h_fst_artlager.
     RUN hideObject IN h_fstperiode.
/*      RUN EndreSize IN h_frapportgrid (DYNAMIC-FUNCTION('getHeight':U IN h_frapportgrid) +                                                                               */
/*                                       DYNAMIC-FUNCTION('getHeight':U IN h_folder) /* DECIMAL */ - .01, DYNAMIC-FUNCTION('getWidth':U IN h_frapportgrid /* DECIMAL */)). */
     RUN setAVfilter IN h_fst_artlager.
/*      wWin:TOP-ONLY = TRUE.                      */
     DYNAMIC-FUNCTION("DoLockWindow",?).
/*      wWin:TOP-ONLY = FALSE. */
     RUN StartSokArtDyn IN h_fst_artlager (hQry,FALSE,cButiker).
/*      RUN StartSokArtDyn IN h_fstlinjeartikkelfilter (hQry,FALSE,cButiker). */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Vislager_stat wWin 
PROCEDURE Vislager_stat :
/*------------------------------------------------------------------------------
  Purpose:    anropas från eksternt program 
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
     DEFINE INPUT  PARAMETER hQry     AS HANDLE     NO-UNDO.
     DEFINE INPUT  PARAMETER cButiker AS CHARACTER  NO-UNDO.
     DEFINE INPUT  PARAMETER cType AS CHARACTER  NO-UNDO.
     DEFINE VARIABLE iFane AS INTEGER    NO-UNDO.
     ASSIGN iFane = IF cType = "STAT" THEN 8 ELSE IF cType = "LAGER" THEN 11
                    ELSE 12.
     RUN selectpage(iFane).
     RUN hideObject IN h_fstperiode.
     IF cType = "LAGER" THEN DO:
         RUN hideObject IN h_fst_artlager.
         RUN hideObject IN h_fstlinjemedlemfilter.
     END.
     ELSE IF cType = "STAT" THEN
         RUN hideObject IN h_fstlinjeartikkelfilter.
/*      RUN EndreSize IN h_frapportgrid (DYNAMIC-FUNCTION('getHeight':U IN h_frapportgrid) +                                                                               */
/*                                       DYNAMIC-FUNCTION('getHeight':U IN h_folder) /* DECIMAL */ - .01, DYNAMIC-FUNCTION('getWidth':U IN h_frapportgrid /* DECIMAL */)). */
     RUN EndreSize2 IN h_frapportgrid ({&WINDOW-NAME}:HEIGHT - 20, {&WINDOW-NAME}:WIDTH - 20).
     RUN setAVfilter IN h_fst_artlager.
/*      wWin:TOP-ONLY = TRUE.                      */
     DYNAMIC-FUNCTION("DoLockWindow",?).
/*      wWin:TOP-ONLY = FALSE. */
     IF cType = "LAGER" THEN DO:
         RUN StartSokArtDyn IN h_fst_artlager (hQry,FALSE,cButiker).
     END.
     ELSE IF cType = "STAT" THEN
         RUN StartSokArtDyn IN h_fstlinjeartikkelfilter (hQry,FALSE,cButiker).

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION geth_fstperiode wWin 
FUNCTION geth_fstperiode RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

 RETURN h_fstperiode.   /* Function return value. */

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

hTabFrame = DYNAMIC-FUNCTION("getContainerHandle" IN h_fst_artlager).
DYNAMIC-FUNCTION("setNoResizeX",THIS-PROCEDURE:CURRENT-WINDOW,hTabFrame,hTabFrame:NAME).
DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,hTabFrame,hTabFrame:NAME).

hTabFrame = DYNAMIC-FUNCTION("getContainerHandle" IN h_fstperiode).
DYNAMIC-FUNCTION("setNoResizeX",THIS-PROCEDURE:CURRENT-WINDOW,hTabFrame,hTabFrame:NAME).
DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,hTabFrame,hTabFrame:NAME).

hTabFrame = DYNAMIC-FUNCTION("getContainerHandle" IN h_fstlinjeartikkelfilter).
DYNAMIC-FUNCTION("setNoResizeX",THIS-PROCEDURE:CURRENT-WINDOW,hTabFrame,hTabFrame:NAME).
DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,hTabFrame,hTabFrame:NAME).

hTabFrame = DYNAMIC-FUNCTION("getContainerHandle" IN h_fstlinjenonsalefilter).
DYNAMIC-FUNCTION("setNoResizeX",THIS-PROCEDURE:CURRENT-WINDOW,hTabFrame,hTabFrame:NAME).
DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,hTabFrame,hTabFrame:NAME).

hTabFrame = DYNAMIC-FUNCTION("getContainerHandle" IN h_fstlinjeavdelingfilter).
DYNAMIC-FUNCTION("setNoResizeX",THIS-PROCEDURE:CURRENT-WINDOW,hTabFrame,hTabFrame:NAME).
DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,hTabFrame,hTabFrame:NAME).

hTabFrame = DYNAMIC-FUNCTION("getContainerHandle" IN h_fstlinjebutikkfilter).
DYNAMIC-FUNCTION("setNoResizeX",THIS-PROCEDURE:CURRENT-WINDOW,hTabFrame,hTabFrame:NAME).
DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,hTabFrame,hTabFrame:NAME).

hTabFrame = DYNAMIC-FUNCTION("getContainerHandle" IN h_fstlinjekassererfilter).
DYNAMIC-FUNCTION("setNoResizeX",THIS-PROCEDURE:CURRENT-WINDOW,hTabFrame,hTabFrame:NAME).
DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,hTabFrame,hTabFrame:NAME).

hTabFrame = DYNAMIC-FUNCTION("getContainerHandle" IN h_fstlinjehovedgrfilter).
DYNAMIC-FUNCTION("setNoResizeX",THIS-PROCEDURE:CURRENT-WINDOW,hTabFrame,hTabFrame:NAME).
DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,hTabFrame,hTabFrame:NAME).

hTabFrame = DYNAMIC-FUNCTION("getContainerHandle" IN h_fstlinjekundefilter).
DYNAMIC-FUNCTION("setNoResizeX",THIS-PROCEDURE:CURRENT-WINDOW,hTabFrame,hTabFrame:NAME).
DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,hTabFrame,hTabFrame:NAME).

hTabFrame = DYNAMIC-FUNCTION("getContainerHandle" IN h_fstlinjelevfilter).
DYNAMIC-FUNCTION("setNoResizeX",THIS-PROCEDURE:CURRENT-WINDOW,hTabFrame,hTabFrame:NAME).
DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,hTabFrame,hTabFrame:NAME).

hTabFrame = DYNAMIC-FUNCTION("getContainerHandle" IN h_fstlinjemedlemfilter).
DYNAMIC-FUNCTION("setNoResizeX",THIS-PROCEDURE:CURRENT-WINDOW,hTabFrame,hTabFrame:NAME).
DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,hTabFrame,hTabFrame:NAME).

hTabFrame = DYNAMIC-FUNCTION("getContainerHandle" IN h_fstlinjeselgerfilter).
DYNAMIC-FUNCTION("setNoResizeX",THIS-PROCEDURE:CURRENT-WINDOW,hTabFrame,hTabFrame:NAME).
DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,hTabFrame,hTabFrame:NAME).

hTabFrame = DYNAMIC-FUNCTION("getContainerHandle" IN h_fstlinjevargrfilter).
DYNAMIC-FUNCTION("setNoResizeX",THIS-PROCEDURE:CURRENT-WINDOW,hTabFrame,hTabFrame:NAME).
DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,hTabFrame,hTabFrame:NAME).

/* hTabFrame = DYNAMIC-FUNCTION("getContainerHandle" IN h_fstlinjevargrfilter).             */
/* DYNAMIC-FUNCTION("setNoResizeX",THIS-PROCEDURE:CURRENT-WINDOW,hTabFrame,hTabFrame:NAME). */
/* DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,hTabFrame,hTabFrame:NAME). */

hTabFrame = DYNAMIC-FUNCTION("getContainerHandle" IN h_fstperiode).
DYNAMIC-FUNCTION("setNoResizeX",THIS-PROCEDURE:CURRENT-WINDOW,hTabFrame,hTabFrame:NAME).
DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,hTabFrame,hTabFrame:NAME).

hTabFrame = DYNAMIC-FUNCTION("getContainerHandle" IN h_fstsammenlign).
DYNAMIC-FUNCTION("setNoResizeX",THIS-PROCEDURE:CURRENT-WINDOW,hTabFrame,hTabFrame:NAME).
DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,hTabFrame,hTabFrame:NAME).

hTabFrame = DYNAMIC-FUNCTION("getContainerHandle" IN h_fst_artlager).
DYNAMIC-FUNCTION("setNoResizeX",THIS-PROCEDURE:CURRENT-WINDOW,hTabFrame,hTabFrame:NAME).
DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,hTabFrame,hTabFrame:NAME).

hTabFrame = DYNAMIC-FUNCTION("getContainerHandle" IN h_fst_stlager).
DYNAMIC-FUNCTION("setNoResizeX",THIS-PROCEDURE:CURRENT-WINDOW,hTabFrame,hTabFrame:NAME).
DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,hTabFrame,hTabFrame:NAME).

hTabFrame = DYNAMIC-FUNCTION("getContainerHandle" IN h_fstlinjekampanjefilter).
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

