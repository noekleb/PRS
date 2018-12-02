&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
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
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
  DEFINE VARIABLE ipFilename AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cSumCols AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cSumString AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cColAlign AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cRappinfo AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cRowBold AS CHARACTER  NO-UNDO.
&ELSE
  DEFINE INPUT  PARAMETER ipFilename AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER cSumCols   AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER cSumString AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER cColAlign  AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER cRappinfo  AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER cRowBold AS CHARACTER  NO-UNDO.
&ENDIF

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE iWindowWidth  AS INTEGER    NO-UNDO.
DEFINE VARIABLE iWindowHeight AS INTEGER    NO-UNDO.
DEFINE VARIABLE dFolderRow    AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dRowDiff      AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dGridheight   AS DECIMAL    NO-UNDO.
DEFINE NEW SHARED TEMP-TABLE TT_Resultat
    FIELD LinjeNr AS INTEGER
    FIELD Verdier AS CHARACTER
    INDEX Linjenr IS PRIMARY Linjenr.

{incl/DevMode.i}
{incl/CustDevMode.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME fMain

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS B-Excel LINJE-1 LINJE-2 B-Xprint 
&Scoped-Define DISPLAYED-OBJECTS FI-Urval 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD InitializeResize wWin 
FUNCTION InitializeResize RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_frapportgrid AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Excel 
     IMAGE-UP FILE "icon/excel.bmp":U NO-FOCUS
     LABEL "Excel..." 
     SIZE 4.6 BY 1.05 TOOLTIP "Eksporter til Excel.".

DEFINE BUTTON B-Xprint 
     IMAGE-UP FILE "icon/e-print.bmp":U NO-FOCUS
     LABEL "Print" 
     SIZE 4.6 BY 1.05 TOOLTIP "XPrint rapport".

DEFINE VARIABLE FI-Urval AS CHARACTER FORMAT "X(256)":U 
     LABEL "Urvalsinfo" 
     VIEW-AS FILL-IN 
     SIZE 118 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE LINJE-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 159.6 BY .1.

DEFINE RECTANGLE LINJE-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 159.6 BY .1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     B-Excel AT ROW 1.19 COL 2.2
     FI-Urval AT ROW 1.24 COL 23.4 COLON-ALIGNED
     B-Xprint AT ROW 1.19 COL 7.2
     LINJE-1 AT ROW 1.05 COL 1
     LINJE-2 AT ROW 2.33 COL 1
     SPACE(42.60) SKIP(21.38)
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Rapporter"
         HEIGHT             = 23
         WIDTH              = 203
         MAX-HEIGHT         = 39.81
         MAX-WIDTH          = 233
         VIRTUAL-HEIGHT     = 39.81
         VIRTUAL-WIDTH      = 233
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

{src/adm2/containr.i}
{dproclibstart.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME fMain
   Size-to-Fit                                                          */
ASSIGN 
       FRAME fMain:SCROLLABLE       = FALSE.

/* SETTINGS FOR FILL-IN FI-Urval IN FRAME fMain
   NO-ENABLE                                                            */
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
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-RESIZED OF wWin /* Rapporter */
DO:
/*     ASSIGN wWin:HEIGHT = DYNAMIC-FUNCTION('getMinHeight':U)                                         */
/*            wWin:WIDTH  = MAX(DYNAMIC-FUNCTION('getMinWidth':U),DYNAMIC-FUNCTION('getWidth':U))      */
/*            FRAME {&FRAME-NAME}:WIDTH = IF FRAME {&FRAME-NAME}:WIDTH < wWin:WIDTH THEN               */
/*                wWin:WIDTH ELSE FRAME {&FRAME-NAME}:WIDTH.                                           */
/* /*            RUN resizeObject IN h_folder                                                       */ */
/* /*     ( INPUT DYNAMIC-FUNCTION('getHeight':U IN h_folder) /* DECIMAL */,                        */ */
/* /*       INPUT MAX(DYNAMIC-FUNCTION('getMinWidth':U IN h_folder),wWin:WIDTH - 1) /* DECIMAL */). */ */
/*            RUN resizeObject IN h_frapportgrid                                                       */
/*     ( INPUT DYNAMIC-FUNCTION('getHeight':U IN h_frapportgrid) /* DECIMAL */,                        */
/*       INPUT MAX(DYNAMIC-FUNCTION('getMinWidth':U IN h_frapportgrid),wWin:WIDTH - 4) /* DECIMAL */). */
/*        ASSIGN FRAME {&FRAME-NAME}:WIDTH  = wWin:WIDTH.                                              */
    DYNAMIC-FUNCTION("setwidgetresize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"resize","").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Excel wWin
ON CHOOSE OF B-Excel IN FRAME fMain /* Excel... */
DO:
    RUN VisaIExcel IN h_frapportgrid.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Xprint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Xprint wWin
ON CHOOSE OF B-Xprint IN FRAME fMain /* Print */
DO:
  DEFINE VARIABLE cFilterVerdier AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iPage          AS INTEGER    NO-UNDO.
  DEFINE VARIABLE hAktivHandle   AS HANDLE     NO-UNDO.
  DEFINE VARIABLE cTabLabel      AS CHARACTER  NO-UNDO.
/*   ASSIGN iPage   = DYNAMIC-FUNCTION('getCurrentPage':U).                                                                         */
/*   CASE iPage:                                                                                                                    */
/*       WHEN 1 THEN                                                                                                                */
/*           ASSIGN hAktivHandle = h_fstlinjeavdelingfilter.                                                                        */
/*       WHEN 2 THEN                                                                                                                */
/*           ASSIGN hAktivHandle = h_fstlinjehovedgrfilter.                                                                         */
/*       WHEN 3 THEN                                                                                                                */
/*           ASSIGN hAktivHandle = h_fstlinjevargrfilter.                                                                           */
/*       WHEN 4 THEN                                                                                                                */
/*           ASSIGN hAktivHandle = h_fstlinjelevfilter.                                                                             */
/*       WHEN 5 THEN                                                                                                                */
/*           ASSIGN hAktivHandle = h_fstlinjekassererfilter.                                                                        */
/*       WHEN 6 THEN                                                                                                                */
/*           ASSIGN hAktivHandle = h_fstlinjeselgerfilter.                                                                          */
/*       WHEN 7 THEN                                                                                                                */
/*           ASSIGN hAktivHandle = h_fstlinjebutikkfilter.                                                                          */
/*       WHEN 8 THEN                                                                                                                */
/*           ASSIGN hAktivHandle = h_fstlinjeartikkelfilter.                                                                        */
/*       WHEN 9 THEN                                                                                                                */
/*           ASSIGN hAktivHandle = h_fstlinjekundefilter.                                                                           */
/*       WHEN 10 THEN                                                                                                               */
/*           ASSIGN hAktivHandle = h_fstlinjemedlemfilter.                                                                          */
/*       WHEN 11 THEN                                                                                                               */
/*           ASSIGN hAktivHandle = h_fst_artlager.                                                                                  */
/*       WHEN 12 THEN                                                                                                               */
/*           ASSIGN hAktivHandle = h_ftransloggfilter.                                                                              */
/*   END CASE.                                                                                                                      */
/*   RUN SendFilterValues IN hAktivHandle (OUTPUT cFilterVerdier, OUTPUT cColAlign) NO-ERROR.                                       */
/*   ASSIGN cFilterVerdier = REPLACE(cFilterverdier,CHR(10)," ")                                                                    */
/*          cFilterVerdier = REPLACE(cFilterverdier,"AAR","ÅR")                                                                     */
/*          cFilterVerdier = REPLACE(cFilterverdier,"MANED","MÅNED").                                                               */
/*   ASSIGN cTabLabel      = ENTRY(INT(DYNAMIC-FUNCTION('getCurrentPage':U)),DYNAMIC-FUNCTION('getFolderLabels':U IN h_folder),"|") */
/*          cFilterVerdier = cTabLabel +                                                                                            */
/*                           (IF cFilterVerdier <> "" THEN CHR(2) ELSE "") +                                                        */
/*                           cFilterVerdier.                                                                                        */
  PUBLISH "PrintGrid" ("XPRINT",cRappinfo,2,"",cColAlign).
/*   PUBLISH "PrintGrid" ("XPRINT",cFilterVerdier,2,"",cColAlign). */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{lng.i &SDO = "SDO"}
{src/adm2/windowmn.i}

/* {lng.i &SDO = "SDO"} */

/* {incl\wintrigg.i} */

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
             INPUT  'prg/frapportgrid.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'LogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_frapportgrid ).
       RUN repositionObject IN h_frapportgrid ( 2.67 , 2.00 ) NO-ERROR.
       /* Size in AB:  ( 21.14 , 201.20 ) */

       /* Links to SmartFrame h_frapportgrid. */
       RUN addLink ( THIS-PROCEDURE , 'PrintGrid':U , h_frapportgrid ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_frapportgrid ,
             FI-Urval:HANDLE IN FRAME fMain , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.
  /* Select a Startup page. */
  IF currentPage eq 0
  THEN RUN selectPage IN THIS-PROCEDURE ( 1 ).

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
  DISPLAY FI-Urval 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE B-Excel LINJE-1 LINJE-2 B-Xprint 
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
   ASSIGN  wWin:HEIGHT-PIXELS = iWindowHeight
           wWin:WIDTH-PIXELS  = iWindowWidth
           FRAME {&FRAME-NAME}:WIDTH = wWin:WIDTH
           FRAME {&FRAME-NAME}:HEIGHT = wWin:HEIGHT
           LINJE-1:WIDTH-PIXELS = iWindowWidth - 2
           LINJE-2:WIDTH-PIXELS = iWindowWidth - 2.
    
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
  DEFINE VARIABLE dTransDato AS DATE       NO-UNDO.
  /* Code placed here will execute PRIOR to standard behavior. */
/*   IF SESSION:WIDTH-PIXELS >= 1280 THEN      */
/*       ASSIGN iWindowWidth  = 1270           */
/*              iWindowHeight = 934            */
/*              dGridheight   = 43.            */
/*   ELSE IF SESSION:WIDTH-PIXELS >= 1024 THEN */
/*             ASSIGN iWindowWidth = 1016      */
/*                    iWindowHeight = 680      */
/*                    dGridheight   = 30.7.    */
/*   ELSE DO:                                           */
/*       ASSIGN iWindowWidth  = wWin:WIDTH-PIXELS - 10  */
/*              iWindowHeight = wWin:HEIGHT-PIXELS - 90 */
/*              dGridheight   = 24.                     */
/*  END.                                                */
  ASSIGN wWin:X = 1
         wWin:Y = 1.
/*   IF iWindowWidth > 800 THEN                                   */
/*       RUN EndreSize.                                           */
/*   ELSE DO:                                                     */
/*       FRAME {&FRAME-NAME}:HEIGHT = wWin:HEIGHT.                */
/*       RUN EndreSize IN h_frapportgrid (23.9 /* DECIMAL */, 0). */
/*   END.                                                         */
  
  ASSIGN FI-Urval = REPLACE(cRappinfo,CHR(2)," - ").
  RUN SUPER.
  InitializeResize().

  {&WINDOW-NAME}:WIDTH-PIXELS = SESSION:WIDTH-PIXELS - 10.
  {&WINDOW-NAME}:HEIGHT-PIXELS = SESSION:HEIGHT-PIXELS - 110.

  APPLY "WINDOW-RESIZED" TO {&WINDOW-NAME}.
  
  
  RUN LoadGrid IN h_frapportgrid (ipFileName, 3).
  IF cSumCols <> "" THEN
      RUN Summer IN h_frapportgrid (cSumCols,cSumString).
  ELSE IF cRowBold <> "" THEN
      RUN SetRowBold IN h_frapportgrid (INT(ENTRY(1,cRowBold,";")),ENTRY(2,cRowBold,";")).
  /* Code placed here will execute AFTER standard behavior.    */
/*   IF NUM-ENTRIES(PROGRAM-NAME(3)," ") = 2 AND                         */
/*       ENTRY(2,PROGRAM-NAME(3)," ") BEGINS "wbokforingsbilag" THEN DO: */
/*       PUBLISH "GetTransDato" (OUTPUT dTransDato).                     */
/*       IF dTransDato <> ? THEN DO:                                     */
/*           RUN selectPage                                              */
/*             ( INPUT 12 /* INTEGER */).                                */
/*           RUN Aktiver IN h_ftransloggfilter (dTransDato).             */
/*       END.                                                            */
/*   END.                                                                */
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

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION InitializeResize wWin 
FUNCTION InitializeResize RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR hTabFrame     AS HANDLE NO-UNDO.
  hTabFrame = DYNAMIC-FUNCTION("getTxtFrame" IN h_frapportgrid).
  DYNAMIC-FUNCTION("setNoResizeX",THIS-PROCEDURE:CURRENT-WINDOW,hTabFrame,hTabFrame:NAME).
  DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,hTabFrame,hTabFrame:NAME).

  DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,1000,100,0,0).
  RETURN YES.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

