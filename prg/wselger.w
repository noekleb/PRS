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

DEFINE VARIABLE hExcelButton AS HANDLE      NO-UNDO.
DEFINE VARIABLE bOk          AS LOGICAL     NO-UNDO.

DEFINE STREAM Eksport.

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

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_bselger2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dselger AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar AS HANDLE NO-UNDO.
DEFINE VARIABLE h_fhistorikk AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_fselger AS HANDLE NO-UNDO.
DEFINE VARIABLE h_sortsok AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vselger AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vselger2-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vselgerinfo AS HANDLE NO-UNDO.

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 203.6 BY 30.24.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Design Page: 1
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Selgerregister"
         HEIGHT             = 30.24
         WIDTH              = 203.6
         MAX-HEIGHT         = 30.24
         MAX-WIDTH          = 203.6
         VIRTUAL-HEIGHT     = 30.24
         VIRTUAL-WIDTH      = 203.6
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
ON CTRL-F1 OF wWin /* Selgerregister */
DO:
    IF valid-handle(h_dproclib) then
        RUN HjelpMap in h_dproclib.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Selgerregister */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Selgerregister */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-RESIZED OF wWin /* Selgerregister */
DO:
    DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"Resize","").  
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
             INPUT  'sdo/dselger.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessnoDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedselgerOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dselger ).
       RUN repositionObject IN h_dselger ( 1.00 , 135.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EdgePixels2DeactivateTargetOnHidenoDisabledActionsFlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsTableio,NavigationTableIOTypeSaveSupportedLinksNavigation-source,Tableio-sourceToolbarBandsToolbarAutoSizenoToolbarDrawDirectionhorizontalLogicalObjectNameDisabledActionsHiddenActionsUpdate,ResetHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0RemoveMenuOnHidenoCreateSubMenuOnConflictyesNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar ).
       RUN repositionObject IN h_dyntoolbar ( 1.00 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar ( 1.24 , 203.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/folder.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FolderLabels':U + 'Selgere|Historikk' + 'FolderTabWidth0FolderFont-1HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_folder ).
       RUN repositionObject IN h_folder ( 2.43 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_folder ( 28.81 , 203.00 ) NO-ERROR.

       /* Links to SmartDataObject h_dselger. */
       RUN addLink ( h_dyntoolbar , 'Navigation':U , h_dselger ).

       /* Links to SmartFolder h_folder. */
       RUN addLink ( h_folder , 'Page':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_folder ,
             h_dyntoolbar , 'AFTER':U ).
    END. /* Page 0 */
    WHEN 1 THEN DO:
       RUN constructObject (
             INPUT  'prg/bselger2.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bselger2 ).
       RUN repositionObject IN h_bselger2 ( 5.52 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_bselger2 ( 10.95 , 201.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'prg/fselger.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'LogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_fselger ).
       RUN repositionObject IN h_fselger ( 16.48 , 131.00 ) NO-ERROR.
       /* Size in AB:  ( 14.48 , 71.00 ) */

       RUN constructObject (
             INPUT  'prg/vselger.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EnabledObjFldsToDisable(None)ModifyFields(All)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vselger ).
       RUN repositionObject IN h_vselger ( 16.71 , 2.00 ) NO-ERROR.
       /* Size in AB:  ( 13.81 , 126.00 ) */

       RUN constructObject (
             INPUT  'prg/vselgerinfo.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EnabledObjFldsToDisable(None)ModifyFields(All)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vselgerinfo ).
       RUN repositionObject IN h_vselgerinfo ( 29.33 , 4.00 ) NO-ERROR.
       /* Size in AB:  ( 0.81 , 90.00 ) */

       RUN constructObject (
             INPUT  'prg/sortsok.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_sortsok ).
       RUN repositionObject IN h_sortsok ( 3.62 , 2.00 ) NO-ERROR.
       /* Size in AB:  ( 1.76 , 27.60 ) */

       /* Links to SmartDataBrowser h_bselger2. */
       RUN addLink ( h_dselger , 'Data':U , h_bselger2 ).

       /* Links to SmartFrame h_fselger. */
       RUN addLink ( h_dselger , 'Data':U , h_fselger ).
       RUN addLink ( h_folder , 'Page':U , h_fselger ).

       /* Links to SmartDataViewer h_vselger. */
       RUN addLink ( h_dselger , 'Data':U , h_vselger ).
       RUN addLink ( h_vselger , 'Update':U , h_dselger ).
       RUN addLink ( h_dyntoolbar , 'Tableio':U , h_vselger ).

       /* Links to SmartDataViewer h_vselgerinfo. */
       RUN addLink ( h_dselger , 'Data':U , h_vselgerinfo ).

       /* Links to SmartObject h_sortsok. */
       RUN addLink ( h_bselger2 , 'Sortera':U , h_sortsok ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_sortsok ,
             h_folder , 'AFTER':U ).
       RUN adjustTabOrder ( h_bselger2 ,
             h_sortsok , 'AFTER':U ).
       RUN adjustTabOrder ( h_fselger ,
             h_bselger2 , 'AFTER':U ).
       RUN adjustTabOrder ( h_vselger ,
             h_fselger , 'AFTER':U ).
       RUN adjustTabOrder ( h_vselgerinfo ,
             h_vselger , 'AFTER':U ).
    END. /* Page 1 */
    WHEN 2 THEN DO:
       RUN constructObject (
             INPUT  'prg/vselger2.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EnabledObjFldsToDisable(None)ModifyFields(All)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vselger2-2 ).
       RUN repositionObject IN h_vselger2-2 ( 3.86 , 2.00 ) NO-ERROR.
       /* Size in AB:  ( 1.91 , 78.00 ) */

       RUN constructObject (
             INPUT  'prg/fhistorikk.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'LogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_fhistorikk ).
       RUN repositionObject IN h_fhistorikk ( 6.24 , 2.00 ) NO-ERROR.
       /* Size in AB:  ( 21.76 , 157.80 ) */

       /* Links to SmartDataViewer h_vselger2-2. */
       RUN addLink ( h_dselger , 'Data':U , h_vselger2-2 ).

       /* Links to SmartFrame h_fhistorikk. */
       RUN addLink ( h_dselger , 'Data':U , h_fhistorikk ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_vselger2-2 ,
             h_folder , 'AFTER':U ).
       RUN adjustTabOrder ( h_fhistorikk ,
             h_vselger2-2 , 'AFTER':U ).
    END. /* Page 2 */

  END CASE.
  /* Select a Startup page. */
  IF currentPage eq 0
  THEN RUN selectPage IN THIS-PROCEDURE ( 1 ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Applhjelp wWin 
PROCEDURE Applhjelp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN Hjelp ("","").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE changePage wWin 
PROCEDURE changePage :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR piCurrPage AS INT NO-UNDO.

  DEF VAR hDataS AS HANDLE NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  assign
    piCurrPage = DYNAMIC-FUNCTION('getCurrentPage':U)
    .

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  IF piCurrPage = 2 THEN
      RUN SetFokus IN h_vselger.
  ELSE IF piCurrPAge = 3 THEN
  DO:
      RUN GetSelgerSDO IN h_fselger (OUTPUT hDataS).
      IF VALID-HANDLE(hDataS) THEN
          DYNAMIC-FUNCTION('openQuery':U IN hDataS).
  END.
  ELSE IF piCurrPAge = 4 THEN
  DO:
      RUN GetSelgerSDO IN h_fhistorikk (OUTPUT hDataS).
      IF VALID-HANDLE(hDataS) THEN
          DYNAMIC-FUNCTION('openQuery':U IN hDataS).
  END.

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
  VIEW FRAME fMain IN WINDOW wWin.
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

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  DYNAMIC-FUNCTION('setDisabledActions':U IN h_dyntoolbar,
     INPUT "Copy" /* CHARACTER */).

  PUBLISH "Sortera" FROM h_bselger2.
  SUBSCRIBE TO "PrintExcel" IN h_dynToolbar.

  IF VALID-HANDLE(h_dyntoolbar) THEN DO:
      RUN OpprettKnapper.
   END.

  SUBSCRIBE TO "ApplHjelp" IN h_dynToolbar.

  IF VALID-HANDLE(h_dproclib) THEN
      RUN GetLng IN h_dproclib (OUTPUT wCurrLng).

  RUN initPages
    ( INPUT "3,4" /* CHARACTER */).

    /* Resize - CHO */ 
    DYNAMIC-FUNCTION("InitTranslation",THIS-PROCEDURE:CURRENT-WINDOW).
    DYNAMIC-FUNCTION("setNoResizeX",THIS-PROCEDURE:CURRENT-WINDOW,DYNAMIC-FUNCTION("getContainerHandle" IN h_sortsok),"f-main,rect-1").
    DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,DYNAMIC-FUNCTION("getContainerHandle" IN h_sortsok),"f-main,rect-1").

    
    DYNAMIC-FUNCTION("setNoResizeX",THIS-PROCEDURE:CURRENT-WINDOW,DYNAMIC-FUNCTION("getContainerHandle" IN h_vselger),"f-Main,rect-59").
    DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,DYNAMIC-FUNCTION("getContainerHandle" IN h_vselger),"f-main,rect-59").
    
    DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,DYNAMIC-FUNCTION("getContainerHandle" IN h_fselger),"fmain").

    /*
    DYNAMIC-FUNCTION("setNoMoveY",THIS-PROCEDURE:CURRENT-WINDOW,DYNAMIC-FUNCTION("getContainerHandle" IN h_fselger),"fmain,rect-59").
    DYNAMIC-FUNCTION("setAddResizeX",THIS-PROCEDURE:CURRENT-WINDOW,DYNAMIC-FUNCTION("getContainerHandle" IN h_fselger),"fmain,rect-59").
    */

    /*
    DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,DYNAMIC-FUNCTION("getContainerHandle" IN h_sortsok),"f-main,rect-1").
    
    DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,DYNAMIC-FUNCTION("getContainerHandle" IN h_vbokforingsbilagi),"f-Main").
    DYNAMIC-FUNCTION("setNoResizeX",THIS-PROCEDURE:CURRENT-WINDOW,DYNAMIC-FUNCTION("getContainerHandle" IN h_vbokforingsbilagi),"f-Main").
    */
    DYNAMIC-FUNCTION("SetResizeADM2panel",TRUE).
    DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,500,290,0,0).


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
   
   /*
   /* Printbutton */
   ASSIGN  piX = piX + 4 /* lägger till 'luft' */
           hButton = DYNAMIC-FUNCTION('createButton':U IN h_dyntoolbar,
              INPUT hFrame,
              INPUT-OUTPUT piX           /* INTEGER   */,
              INPUT "Print"              /* CHARACTER */,
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
            "":U                {&dlmt}
            "PUBLISH":U         {&dlmt}
            "PRINT":U           {&dlmt}
            "Options":U).
   */

   /* Varefilbutton */
   ASSIGN  piX = piX + 4 /* lägger till 'luft' */
       hButton = DYNAMIC-FUNCTION('createButton':U IN h_dyntoolbar,
         INPUT hFrame,
         INPUT-OUTPUT piX        /* INTEGER */,
         INPUT "Excel"            /* CHARACTER */,
         INPUT "Excel"            /* CHARACTER */,
         INPUT "Eksport Excel"            /* CHARACTER */,
         INPUT "icon\excel.bmp" /* CHARACTER */,
         INPUT TRUE              /* LOGICAL */).

   DYNAMIC-FUNCTION('defineAction':U IN h_dyntoolbar,
      INPUT "Excel":U /* CHARACTER */,
      INPUT "Name,Caption,Image,Type,OnChoose,AccessType,Parent" /* CHARACTER */,
      INPUT "Excel"      {&dlmt}
            "Excel"      {&dlmt}
            "":U        {&dlmt}
            "PUBLISH":U {&dlmt}
            "PrintExcel":U   {&dlmt}
            "":U        {&dlmt}
            "":U).
   
   ASSIGN hExcelButton = hButton.

 
   /* Action EXIT finns i toolbar sedan tidigare */
   ASSIGN piX = hFrame:WIDTH-PIXELS - 26 /*hButton:WIDTH-PIXELS - 4*/
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
            "ApplHjelp":U   {&dlmt}
            "":U        {&dlmt}
            "":U).
    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE printExcel wWin 
PROCEDURE printExcel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var ctmpFileName as char no-undo.
  def var cExcEkstent  as char no-undo.
  
  FIND bruker NO-LOCK WHERE bruker.BrukerId = USERID("SkoTex") NO-ERROR.
  IF NOT AVAILABLE Bruker THEN
  DO:
      MESSAGE 'Ukjent bruker. Eksport av selgerregister ikke tillatt.'
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN.
  END.

  bOk = FALSE.
  MESSAGE 'Skal selgerregsiteret eksporteres ti Excel?'
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE bOk.
  IF bOk = FALSE THEN
      RETURN.

  {syspara.i 1 4 1 cExcEkstent}
  cExcEkstent = if cExcEkstent = "" then "sdv" else cExcEkstent.    
  /* Henter temporært filnavn. */
  if valid-handle(h_dproclib) then
    run GetTempFileName in h_dproclib (input "selgerlst", input cExcEkstent, output ctmpFileName).
  
  OUTPUT STREAM Eksport TO VALUE(ctmpFileName).

  /* Overskrift. */
  PUT STREAM Eksport 
     "PRS selgernr" ';'
     "Butikknr" ';'
     "Ansattnr" ';'
     "Fornavn" ';'
     "Etternavn" ';'
     "Jobbtittel" ';'
     "Adresse1" ';'
     "Adresse2" ';'
     "Postnr" ';'
     "Postadresse" ';'
     "NavnIKasse" ';'
     "Telefon" ';'
     "Mobiltelefon" ';'
     "Personnr" ';'
     "Lonnsprofil" ';'
     "Arbeidsprosent" ';'
     "Timelonn" ';'
     "Fastlonn" ';'
     "Ansatt dato" ';'
     "Sluttet dato" ';'
     "Fodt" ';'
     "PRS Brukerid" ';'
     "Endret dato" ';'
     "Endret tid" ';'
     "Endret av" ';'
     "Registrert dato" ';'
     "Registrert tid" ';'
     "Registrert av"
     SKIP.          


  FOR EACH Selger NO-LOCK WHERE 
      (IF Bruker.BrukerType = 2 
         THEN Selger.ButikkNr = Bruker.ButikkNr
         ELSE TRUE):
      PUT STREAM Eksport 
         Selger.SelgerNr ';'
         Selger.ButikkNr ';'
         Selger.AnsattNr ';'
         Selger.ForNavn ';'
         Selger.Navn ';'
         Selger.JobTittel ';'
         Selger.Adresse1 ';'
         Selger.Adresse2 ';'
         Selger.PostNr ';'
         (IF AVAILABLE Post THEN Post.Beskrivelse ELSE '') ';'
         Selger.NavnIKasse ';'
         Selger.Telefon ';'
         Selger.Mobiltelefon ';'
         Selger.PersonNr ';'
         Selger.LonnProfil ';'
         Selger.ArbeidsProsent ';'
         Selger.TimeLonn ';'
         Selger.FastLonn ';'
         Selger.AnsattDato ';'
         Selger.SluttetDato ';'
         Selger.FodtDato ';'
         Selger.BrukeridPRS ';'
         Selger.EDato ';'
         Selger.ETid ';'
         Selger.BrukerID ';'
         Selger.RegistrertDato ';'
         Selger.RegistrertTid ';'
         Selger.RegistrertAv ';'
      SKIP.          
  END.
  
  OUTPUT STREAM Eksport CLOSE.

  if valid-handle(h_dproclib) then
    run OpenExcelDocument in h_dproclib (ctmpFileName, " ").
 
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
  DEF VAR pbDataModified AS LOG NO-UNDO.

  IF valid-handle(h_vselger) THEN
  DO:
      ASSIGN
          pbDataModified = 
              DYNAMIC-FUNCTION('getDataModified':U IN h_vselger)
          .
      IF pbDataModified THEN
      DO:
          MESSAGE "Endrede data må lagres eller forkastes før side kan byttes."
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN.
      END.
  END.


  RUN SUPER( INPUT piPageNum).

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

