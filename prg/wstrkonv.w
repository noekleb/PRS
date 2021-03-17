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
DEFINE VAR hPrint   AS HANDLE NO-UNDO.
DEFINE VAR hExcel   AS HANDLE NO-UNDO.
DEFINE VAR hHelp    AS HANDLE NO-UNDO.
DEFINE VAR hExit    AS HANDLE NO-UNDO.
DEFINE VAR piX      AS INTEGER    NO-UNDO.
DEFINE VAR cLabels  AS CHARACTER  NO-UNDO.
{runlib.i}

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
&Scoped-Define ENABLED-OBJECTS B-KonvTbl 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_bstrkonv AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dstrkonv AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar AS HANDLE NO-UNDO.
DEFINE VARIABLE h_sortsok AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vstrkonv AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-KonvTbl 
     LABEL "Konverteringstabell..." 
     SIZE 22 BY 1.14.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     B-KonvTbl AT ROW 2.43 COL 82.8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 104 BY 24.95.


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
         TITLE              = "Konverteringstabell størrelser"
         HEIGHT             = 24.95
         WIDTH              = 104
         MAX-HEIGHT         = 24.95
         MAX-WIDTH          = 104
         VIRTUAL-HEIGHT     = 24.95
         VIRTUAL-WIDTH      = 104
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
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
{hjelp.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME fMain
   FRAME-NAME                                                           */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Konverteringstabell størrelser */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Konverteringstabell størrelser */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-KonvTbl
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-KonvTbl wWin
ON CHOOSE OF B-KonvTbl IN FRAME fMain /* Konverteringstabell... */
DO:

  RUN KobleStr.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */

{src/adm2/windowmn.i}

{lng.i &SDO = "SDO"}

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
             INPUT  'sdo/dstrkonv.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessnoDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedstrkonvOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dstrkonv ).
       RUN repositionObject IN h_dstrkonv ( 2.91 , 90.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'prg/vstrkonv.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EnabledObjFldsToDisable(None)ModifyFields(All)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vstrkonv ).
       RUN repositionObject IN h_vstrkonv ( 2.43 , 1.00 ) NO-ERROR.
       /* Size in AB:  ( 4.52 , 76.00 ) */

       RUN constructObject (
             INPUT  'prg/bstrkonv.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bstrkonv ).
       RUN repositionObject IN h_bstrkonv ( 9.33 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_bstrkonv ( 16.43 , 104.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EdgePixels2DeactivateTargetOnHidenoDisabledActionsFlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsTableio,NavigationTableIOTypeSaveSupportedLinksNavigation-source,Tableio-sourceToolbarBandsToolbarAutoSizenoToolbarDrawDirectionhorizontalLogicalObjectNameDisabledActionsHiddenActionsUpdate,ResetHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0RemoveMenuOnHidenoCreateSubMenuOnConflictyesNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar ).
       RUN repositionObject IN h_dyntoolbar ( 1.00 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar ( 1.24 , 104.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'prg/sortsok.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_sortsok ).
       RUN repositionObject IN h_sortsok ( 7.43 , 1.00 ) NO-ERROR.
       /* Size in AB:  ( 1.76 , 27.60 ) */

       /* Links to SmartDataObject h_dstrkonv. */
       RUN addLink ( h_dyntoolbar , 'Navigation':U , h_dstrkonv ).

       /* Links to SmartDataViewer h_vstrkonv. */
       RUN addLink ( h_dstrkonv , 'Data':U , h_vstrkonv ).
       RUN addLink ( h_vstrkonv , 'Update':U , h_dstrkonv ).
       RUN addLink ( h_dyntoolbar , 'TableIO':U , h_vstrkonv ).

       /* Links to SmartDataBrowser h_bstrkonv. */
       RUN addLink ( h_dstrkonv , 'Data':U , h_bstrkonv ).

       /* Links to SmartObject h_sortsok. */
       RUN addLink ( h_bstrkonv , 'Sortera':U , h_sortsok ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_dyntoolbar ,
             B-KonvTbl:HANDLE IN FRAME fMain , 'BEFORE':U ).
       RUN adjustTabOrder ( h_vstrkonv ,
             h_dyntoolbar , 'AFTER':U ).
       RUN adjustTabOrder ( h_sortsok ,
             B-KonvTbl:HANDLE IN FRAME fMain , 'AFTER':U ).
       RUN adjustTabOrder ( h_bstrkonv ,
             h_sortsok , 'AFTER':U ).
    END. /* Page 0 */

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
  ENABLE B-KonvTbl 
      WITH FRAME fMain IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  VIEW wWin.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR iTilgang           AS INT    NO-UNDO.

  {syspara.i 16 29 2 iTilgang INT}

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
/*   RUN skapaButtons. */
  RUN OpprettKnapper.
  IF iTilgang = 1 THEN
    DYNAMIC-FUNCTION('setDisabledActions':U IN h_dyntoolbar,
      INPUT "ADD,COPY,DELETE" /* CHARACTER */).
  ELSE
    DYNAMIC-FUNCTION('setDisabledActions':U IN h_dyntoolbar,
      INPUT "Copy" /* CHARACTER */).
  /* Viewer'n får lablar till rapport */
  ASSIGN cLabels  = {&WINDOW-NAME}:TITLE + "," + DYNAMIC-FUNCTION('getBrowseLabels':U IN h_bstrkonv).
  DYNAMIC-FUNCTION('setRapLabels':U IN h_vstrkonv,INPUT cLabels).
  SUBSCRIBE TO "pgmHelp" IN h_dyntoolbar.

  PUBLISH "Sortera" FROM h_bstrkonv.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KobleStr wWin 
PROCEDURE KobleStr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR pcStrKode AS CHAR NO-UNDO.

  ASSIGN
      pcStrKode = ENTRY(2,DYNAMIC-FUNCTION('colValues':U IN h_dstrkonv,
                          INPUT "StrKode"),CHR(1))
      .

  run d-bimpkonv.w (1012,
                    pcStrKode,
                    "Kobling av størrelser"
                    ).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpprettKnapper wWin 
PROCEDURE OpprettKnapper :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE hFrame  AS HANDLE     NO-UNDO.
   DEFINE VARIABLE hHandle AS HANDLE     NO-UNDO.
   DEFINE VARIABLE hButton AS HANDLE     NO-UNDO.
   DEFINE VARIABLE iPos    AS INTEGER    NO-UNDO.
   DEFINE VARIABLE piX     AS INTEGER    NO-UNDO.
   &SCOP dlmt + CHR(1) +
   
   ASSIGN hFrame  = DYNAMIC-FUNCTION('getContainerHandle':U IN h_dyntoolbar)
          hHandle = hFrame
          hHandle = hHandle:FIRST-CHILD  /* första field-group          */
          hHandle = hHandle:FIRST-CHILD. /* första widget i field-group */
   REPEAT WHILE VALID-HANDLE(hHandle):
       /* hämtar X-pos för sista 'rulen' i toolbaren */
       IF hHandle:TYPE = "RECTANGLE" AND hHandle:X > piX THEN
           ASSIGN iPos = hHandle:X + hHandle:WIDTH-PIXELS
                  piX  = hHandle:X.
       ASSIGN hHandle = hHandle:NEXT-SIBLING.
   END.
   
   /* Printbuttons */
   ASSIGN  piX = piX + 4 /* lägger till 'luft' */
           hButton = DYNAMIC-FUNCTION('createButton':U IN h_dyntoolbar,
              INPUT hFrame,
              INPUT-OUTPUT piX           /* INTEGER   */,
              INPUT "Print:StrKonvXprint"              /* CHARACTER */,
              INPUT "Print"              /* CHARACTER */,
              INPUT "Rapport"            /* CHARACTER */,
              INPUT "icon\e-print.bmp"   /* CHARACTER */,
              INPUT TRUE                 /* LOGICAL   */
              ).
    
   DYNAMIC-FUNCTION('defineAction':U IN h_dyntoolbar,
      INPUT "Print":U /* CHARACTER */,
      INPUT "Name,Caption,Image,Link,Type,OnChoose,Parent" /* CHARACTER */,
      INPUT "Print"             {&dlmt}
            "Print Record"      {&dlmt}
            "":U                {&dlmt}
            "TableIO-Target":U                {&dlmt}
            "RUN":U             {&dlmt}
            "printObject":U           {&dlmt}
            "Options":U).
   
   ASSIGN  piX = piX + 4 /* lägger till 'luft' */
           hButton = DYNAMIC-FUNCTION('createButton':U IN h_dyntoolbar,
              INPUT hFrame,
              INPUT-OUTPUT piX           /* INTEGER   */,
              INPUT "Print:StrKonvExcel"              /* CHARACTER */,
              INPUT "Excel"              /* CHARACTER */,
              INPUT "Excel"            /* CHARACTER */,
              INPUT "icon\excel.bmp"   /* CHARACTER */,
              INPUT TRUE                 /* LOGICAL   */
              ).
    
   DYNAMIC-FUNCTION('defineAction':U IN h_dyntoolbar,
      INPUT "Print":U /* CHARACTER */,
      INPUT "Name,Caption,Image,Link,Type,OnChoose,Parent" /* CHARACTER */,
      INPUT "Excel"             {&dlmt}
            "Print Record"      {&dlmt}
            "":U                {&dlmt}
            "TableIO-Target":U                {&dlmt}
            "RUN":U             {&dlmt}
            "printObject":U           {&dlmt}
            "Options":U).
 
   /* Action EXIT finns i toolbar sedan tidigare */
   ASSIGN piX = hFrame:WIDTH-PIXELS - hButton:WIDTH-PIXELS - 4
          hButton = DYNAMIC-FUNCTION('createButton':U IN h_dyntoolbar,
            INPUT hFrame,
            INPUT-OUTPUT piX        /* INTEGER   */,
            INPUT "exit"            /* CHARACTER */,
            INPUT "exit"            /* CHARACTER */,
            INPUT "exit"            /* CHARACTER */,
            INPUT "icon\e-exit.bmp" /* CHARACTER */,
            INPUT TRUE              /* LOGICAL   */
            ).

   ASSIGN piX = hButton:X - hButton:WIDTH-PIXELS - 1.
          hButton = DYNAMIC-FUNCTION('createButton':U IN h_dyntoolbar,
            INPUT hFrame,
            INPUT-OUTPUT piX        /* INTEGER */,
            INPUT "HELP"            /* CHARACTER */,
            INPUT "HELP"            /* CHARACTER */,
            INPUT "HELP"            /* CHARACTER */,
            INPUT "icon\e-help.bmp" /* CHARACTER */,
            INPUT TRUE              /* LOGICAL */).

   DYNAMIC-FUNCTION('defineAction':U IN h_dyntoolbar,
      INPUT "HELP":U /* CHARACTER */,
      INPUT "Name,Caption,Image,Type,OnChoose,AccessType,Parent" /* CHARACTER */,
      INPUT "Help"      {&dlmt}
            "Help"      {&dlmt}
            "":U        {&dlmt}
            "PUBLISH":U {&dlmt}
            "ApplHjelp":U {&dlmt}
            "":U        {&dlmt}
            "":U).
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pgmHelp wWin 
PROCEDURE pgmHelp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {winhlp.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapaButtons wWin 
PROCEDURE SkapaButtons :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /* Printbutton */
  ASSIGN piX = 260
          hPrint = DYNAMIC-FUNCTION('createButton':U IN h_dyntoolbar,
           INPUT FRAME {&FRAME-NAME}:HANDLE /* HANDLE */,
           INPUT-OUTPUT piX /* INTEGER */,
           INPUT "Print:StrKonvXprint" /* CHARACTER */,
           INPUT "Print" /* CHARACTER */,
           INPUT "Rapport" /* CHARACTER */,
           INPUT "icon\e-print.bmp" /* pcBitmap CHARACTER */,
           INPUT TRUE /* LOGICAL */).
   hPrint:MOVE-TO-TOP().
   DYNAMIC-FUNCTION('defineAction':U IN h_dyntoolbar,
     INPUT "Print":U /* CHARACTER */,
     INPUT "Name,Caption,Image,Link,Type,OnChoose,Parent" /* CHARACTER */,
     INPUT "Print" + CHR(1) +
           "Print Record" + CHR(1) +
           "":U  + CHR(1) +
           "TableIO-Target":U  + CHR(1) +
           "RUN":U  + CHR(1) +
           "printObject":U + CHR(1) +
           "Options":U).
   
   ASSIGN piX = hPrint:X + hPrint:WIDTH-PIXELS + 1
           hExcel = DYNAMIC-FUNCTION('createButton':U IN h_dyntoolbar,
            INPUT FRAME {&FRAME-NAME}:HANDLE /* HANDLE */,
            INPUT-OUTPUT piX /* INTEGER */,
            INPUT "Print:StrKonvExcel" /* CHARACTER */,
            INPUT "Excel" /* CHARACTER */,
            INPUT "Excel" /* CHARACTER */,
            INPUT "icon\excel.bmp" /* pcBitmap CHARACTER */,
            INPUT TRUE /* LOGICAL */).
    hExcel:MOVE-TO-TOP().
    DYNAMIC-FUNCTION('defineAction':U IN h_dyntoolbar,
      INPUT "Excel":U /* CHARACTER */,
      INPUT "Name,Caption,Image,Link,Type,OnChoose,Parent" /* CHARACTER */,
      INPUT "Excel" + CHR(1) +
            "Print Record" + CHR(1) +
            "":U  + CHR(1) +
            "TableIO-Target":U  + CHR(1) +
            "RUN":U  + CHR(1) +
            "printObject":U + CHR(1) +
            "Options":U).

   ASSIGN piX = FRAME {&FRAME-NAME}:WIDTH-PIXELS - hPrint:WIDTH-PIXELS - 3
          hExit = DYNAMIC-FUNCTION('createButton':U IN h_dyntoolbar,
           INPUT FRAME {&FRAME-NAME}:HANDLE /* HANDLE */,
           INPUT-OUTPUT piX /* INTEGER */,
           INPUT "exit" /* CHARACTER */,
           INPUT "exit" /* CHARACTER */,
           INPUT "exit" /* CHARACTER */,
           INPUT "icon\e-exit.bmp" /* pcBitmap CHARACTER */,
           INPUT TRUE /* LOGICAL */).
   hExit:MOVE-TO-TOP().

   DYNAMIC-FUNCTION('defineAction':U IN h_dyntoolbar,
     INPUT "HELP":U /* CHARACTER */,
     INPUT "Name,Caption,Image,Type,OnChoose,AccessType,Parent" /* CHARACTER */,
     INPUT "Help" + CHR(1) +
           "Help" + CHR(1) +
           "":U  + CHR(1) +
           "PUBLISH":U  + CHR(1) +
           "pgmHelp":U  + CHR(1) +
           "":U + CHR(1) +
           "":U).
   
   ASSIGN piX = hExit:X - hExit:WIDTH-PIXELS - 1
          hHelp = DYNAMIC-FUNCTION('createButton':U IN h_dyntoolbar,
           INPUT FRAME {&FRAME-NAME}:HANDLE /* HANDLE */,
           INPUT-OUTPUT piX /* INTEGER */,
           INPUT "HELP" /* CHARACTER */,
           INPUT "HELP" /* CHARACTER */,
           INPUT "HELP" /* CHARACTER */,
           INPUT "icon\e-help.bmp" /* pcBitmap CHARACTER */,
           INPUT TRUE /* LOGICAL */).
   hHelp:MOVE-TO-TOP().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

