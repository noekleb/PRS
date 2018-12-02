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

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE hExcelButton AS HANDLE     NO-UNDO.
DEFINE STREAM Eksport.

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

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_bkjede AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bkjededistrikt AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bkjedensbutikker AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bkjederegion AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dkjede AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dkjededistrikt AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dkjedensbutikker AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dkjederegion AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar-but AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar-dist AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar-region AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vkjede AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vkjededistrikt AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vkjededistriktsmall AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vkjedensbutikker AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vkjederegion AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vkjederegionsmall AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vkjedevis AS HANDLE NO-UNDO.

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     SPACE(160.01) SKIP(26.58)
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Design Page: 4
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Kjede/kjedestruktur"
         HEIGHT             = 26.57
         WIDTH              = 160
         MAX-HEIGHT         = 26.57
         MAX-WIDTH          = 160
         VIRTUAL-HEIGHT     = 26.57
         VIRTUAL-WIDTH      = 160
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

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Kjede/kjedestruktur */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Kjede/kjedestruktur */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  RUN selectPage (DYNAMIC-FUNCTION('getCurrentPage':U)).
  IF RETURN-VALUE = "CANCEL" THEN
      RETURN NO-APPLY.
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
  ON 'ALT-S':U  ANYWHERE
  DO:
      IF DYNAMIC-FUNCTION('getCurrentPage':U) <> 4 THEN
          LEAVE.
      RUN FinnButikk IN THIS-PROCEDURE.
  END.

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
             INPUT  'sdo/dkjede.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsKjede.KjedeNr,KjedeNrRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedkjedeUpdateFromSourcenoToggleDataTargetsyesOpenOnInityesPromptOnDeleteyesPromptColumns(NONE)':U ,
             OUTPUT h_dkjede ).
       RUN repositionObject IN h_dkjede ( 2.19 , 150.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'prg/vkjede.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EnabledObjFldsToDisable(None)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vkjedevis ).
       RUN repositionObject IN h_vkjedevis ( 2.67 , 3.00 ) NO-ERROR.
       /* Size in AB:  ( 4.05 , 48.00 ) */

       RUN constructObject (
             INPUT  'sdo/dkjederegion.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsKjedeRegion.KjedeNr,KjedeNrRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedkjederegionUpdateFromSourcenoToggleDataTargetsyesOpenOnInityesPromptOnDeleteyesPromptColumns(NONE)':U ,
             OUTPUT h_dkjederegion ).
       RUN repositionObject IN h_dkjederegion ( 5.76 , 148.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'prg/vkjederegionsmall.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EnabledObjFldsToDisable(None)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vkjederegionsmall ).
       RUN repositionObject IN h_vkjederegionsmall ( 2.67 , 51.00 ) NO-ERROR.
       /* Size in AB:  ( 4.05 , 48.00 ) */

       RUN constructObject (
             INPUT  'sdo/dkjededistrikt.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsKjedeDistrikt.KjedeNr,KjedeNr,KjedeDistrikt.RegionNr,RegionNrRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedkjededistriktUpdateFromSourcenoToggleDataTargetsyesOpenOnInityesPromptOnDeleteyesPromptColumns(NONE)':U ,
             OUTPUT h_dkjededistrikt ).
       RUN repositionObject IN h_dkjededistrikt ( 4.10 , 150.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'prg/vkjededistriktsmall.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EnabledObjFldsToDisable(None)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vkjededistriktsmall ).
       RUN repositionObject IN h_vkjededistriktsmall ( 2.67 , 99.00 ) NO-ERROR.
       /* Size in AB:  ( 4.05 , 48.00 ) */

       RUN constructObject (
             INPUT  'sdo/dkjedensbutikker.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsKjedensButikker.KjedeNr,KjedeNr,KjedensButikker.RegionNr,RegionNr,KjedensButikker.DistriktNr,DistriktNrRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedkjedensbutikkerUpdateFromSourcenoToggleDataTargetsyesOpenOnInityesPromptOnDeleteyesPromptColumns(NONE)':U ,
             OUTPUT h_dkjedensbutikker ).
       RUN repositionObject IN h_dkjedensbutikker ( 4.81 , 147.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsTableio,NavigationSubModulesTableIOTypeSaveSupportedLinksNavigation-source,Tableio-sourceToolbarBandsToolbarParentMenuToolbarAutoSizenoToolbarDrawDirectionhorizontalToolbarInitialStateLogicalObjectNameAutoResizeDisabledActionsHiddenActionsUpdateHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0EdgePixels2PanelTypeToolbarDeactivateTargetOnHidenoDisabledActionsNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar ).
       RUN repositionObject IN h_dyntoolbar ( 1.00 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar ( 1.24 , 160.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/folder.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FolderLabels':U + 'Kjede|Region|Distrikt|Kjedens butikker' + 'FolderTabWidth0FolderFont-1HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_folder ).
       RUN repositionObject IN h_folder ( 7.19 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_folder ( 20.38 , 159.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN initPages ('2,3,4') NO-ERROR.

       /* Links to SmartDataObject h_dkjede. */
       RUN addLink ( h_dyntoolbar , 'Navigation':U , h_dkjede ).

       /* Links to SmartDataViewer h_vkjedevis. */
       RUN addLink ( h_dkjede , 'Data':U , h_vkjedevis ).

       /* Links to SmartDataObject h_dkjederegion. */
       RUN addLink ( h_dkjede , 'Data':U , h_dkjederegion ).
       RUN addLink ( h_dyntoolbar-region , 'Navigation':U , h_dkjederegion ).

       /* Links to SmartDataViewer h_vkjederegionsmall. */
       RUN addLink ( h_dkjederegion , 'Data':U , h_vkjederegionsmall ).

       /* Links to SmartDataObject h_dkjededistrikt. */
       RUN addLink ( h_dkjederegion , 'Data':U , h_dkjededistrikt ).
       RUN addLink ( h_dyntoolbar-dist , 'Navigation':U , h_dkjededistrikt ).

       /* Links to SmartDataViewer h_vkjededistriktsmall. */
       RUN addLink ( h_dkjededistrikt , 'Data':U , h_vkjededistriktsmall ).

       /* Links to SmartDataObject h_dkjedensbutikker. */
       RUN addLink ( h_dkjededistrikt , 'Data':U , h_dkjedensbutikker ).
       RUN addLink ( h_dyntoolbar-but , 'Navigation':U , h_dkjedensbutikker ).

       /* Links to SmartFolder h_folder. */
       RUN addLink ( h_folder , 'Page':U , THIS-PROCEDURE ).

    END. /* Page 0 */

    WHEN 1 THEN DO:
       RUN constructObject (
             INPUT  'prg/vkjede.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EnabledObjFldsToDisable(None)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vkjede ).
       RUN repositionObject IN h_vkjede ( 8.86 , 4.00 ) NO-ERROR.
       /* Size in AB:  ( 4.05 , 48.00 ) */

       RUN constructObject (
             INPUT  'prg/bkjede.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bkjede ).
       RUN repositionObject IN h_bkjede ( 13.14 , 4.00 ) NO-ERROR.
       RUN resizeObject IN h_bkjede ( 14.05 , 147.00 ) NO-ERROR.

       /* Links to SmartDataViewer h_vkjede. */
       RUN addLink ( h_dkjede , 'Data':U , h_vkjede ).
       RUN addLink ( h_vkjede , 'Update':U , h_dkjede ).
       RUN addLink ( h_dyntoolbar , 'TableIO':U , h_vkjede ).

       /* Links to SmartDataBrowser h_bkjede. */
       RUN addLink ( h_dkjede , 'Data':U , h_bkjede ).

    END. /* Page 1 */

    WHEN 2 THEN DO:
       RUN constructObject (
             INPUT  'prg/vkjederegion.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EnabledObjFldsToDisable(None)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vkjederegion ).
       RUN repositionObject IN h_vkjederegion ( 10.52 , 5.00 ) NO-ERROR.
       /* Size in AB:  ( 7.62 , 52.00 ) */

       RUN constructObject (
             INPUT  'prg/bkjederegion.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bkjederegion ).
       RUN repositionObject IN h_bkjederegion ( 18.38 , 5.00 ) NO-ERROR.
       RUN resizeObject IN h_bkjederegion ( 8.57 , 118.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsTableio,NavigationSubModulesTableIOTypeSaveSupportedLinksNavigation-source,Tableio-sourceToolbarBandsToolbarParentMenuToolbarAutoSizenoToolbarDrawDirectionhorizontalToolbarInitialStateLogicalObjectNameAutoResizeDisabledActionsHiddenActionsUpdateHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0EdgePixels2PanelTypeToolbarDeactivateTargetOnHidenoDisabledActionsNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar-region ).
       RUN repositionObject IN h_dyntoolbar-region ( 8.86 , 3.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar-region ( 1.24 , 156.00 ) NO-ERROR.

       /* Links to SmartDataViewer h_vkjederegion. */
       RUN addLink ( h_dkjederegion , 'Data':U , h_vkjederegion ).
       RUN addLink ( h_vkjederegion , 'Update':U , h_dkjederegion ).
       RUN addLink ( h_dyntoolbar-region , 'TableIo':U , h_vkjederegion ).

       /* Links to SmartDataBrowser h_bkjederegion. */
       RUN addLink ( h_dkjederegion , 'Data':U , h_bkjederegion ).

    END. /* Page 2 */

    WHEN 3 THEN DO:
       RUN constructObject (
             INPUT  'prg/vkjededistrikt.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EnabledObjFldsToDisable(None)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vkjededistrikt ).
       RUN repositionObject IN h_vkjededistrikt ( 10.29 , 5.00 ) NO-ERROR.
       /* Size in AB:  ( 7.62 , 54.00 ) */

       RUN constructObject (
             INPUT  'prg/bkjededistrikt.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bkjededistrikt ).
       RUN repositionObject IN h_bkjededistrikt ( 17.91 , 5.00 ) NO-ERROR.
       RUN resizeObject IN h_bkjededistrikt ( 9.05 , 114.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsTableio,NavigationSubModulesTableIOTypeSaveSupportedLinksNavigation-source,Tableio-sourceToolbarBandsToolbarParentMenuToolbarAutoSizenoToolbarDrawDirectionhorizontalToolbarInitialStateLogicalObjectNameAutoResizeDisabledActionsHiddenActionsUpdateHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0EdgePixels2PanelTypeToolbarDeactivateTargetOnHidenoDisabledActionsNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar-dist ).
       RUN repositionObject IN h_dyntoolbar-dist ( 8.62 , 3.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar-dist ( 1.24 , 132.00 ) NO-ERROR.

       /* Links to SmartDataViewer h_vkjededistrikt. */
       RUN addLink ( h_dkjededistrikt , 'Data':U , h_vkjededistrikt ).
       RUN addLink ( h_vkjededistrikt , 'Update':U , h_dkjededistrikt ).
       RUN addLink ( h_dyntoolbar-dist , 'TableIO':U , h_vkjededistrikt ).

       /* Links to SmartDataBrowser h_bkjededistrikt. */
       RUN addLink ( h_dkjededistrikt , 'Data':U , h_bkjededistrikt ).

    END. /* Page 3 */

    WHEN 4 THEN DO:
       RUN constructObject (
             INPUT  'prg/vkjedensbutikker.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EnabledObjFldsToDisable(None)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vkjedensbutikker ).
       RUN repositionObject IN h_vkjedensbutikker ( 9.81 , 3.00 ) NO-ERROR.
       /* Size in AB:  ( 11.10 , 147.00 ) */

       RUN constructObject (
             INPUT  'prg/bkjedensbutikker.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bkjedensbutikker ).
       RUN repositionObject IN h_bkjedensbutikker ( 21.00 , 3.00 ) NO-ERROR.
       RUN resizeObject IN h_bkjedensbutikker ( 6.19 , 147.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsTableio,NavigationSubModulesTableIOTypeSaveSupportedLinksNavigation-source,Tableio-sourceToolbarBandsToolbarParentMenuToolbarAutoSizenoToolbarDrawDirectionhorizontalToolbarInitialStateLogicalObjectNameAutoResizeDisabledActionsHiddenActionsUpdateHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0EdgePixels2PanelTypeToolbarDeactivateTargetOnHidenoDisabledActionsNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar-but ).
       RUN repositionObject IN h_dyntoolbar-but ( 8.38 , 3.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar-but ( 1.24 , 75.00 ) NO-ERROR.

       /* Links to SmartDataViewer h_vkjedensbutikker. */
       RUN addLink ( h_dkjedensbutikker , 'Data':U , h_vkjedensbutikker ).
       RUN addLink ( h_vkjedensbutikker , 'Update':U , h_dkjedensbutikker ).
       RUN addLink ( h_dyntoolbar-but , 'TableIO':U , h_vkjedensbutikker ).

       /* Links to SmartDataBrowser h_bkjedensbutikker. */
       RUN addLink ( h_dkjedensbutikker , 'Data':U , h_bkjedensbutikker ).

       /* Adjust the tab order of the smart objects. */
    END. /* Page 4 */

  END CASE.
  /* Select a Startup page. */
  IF currentPage eq 0
  THEN RUN selectPage IN THIS-PROCEDURE ( 1 ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ApplHjelp wWin 
PROCEDURE ApplHjelp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN Hjelp ("","").

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FinnButikk wWin 
PROCEDURE FinnButikk :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cKjede    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cRegion   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDistrikt AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cButikk   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSokKjede    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSokRegion   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSokDistrikt AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSokButikk   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cVerdier     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iSokButikk   AS INTEGER   NO-UNDO.
    
    ASSIGN cVerdier  = DYNAMIC-FUNCTION('colValues':U IN h_dkjedensbutikker,
                       INPUT "KjedeNr,RegionNr,DistriktNr,ButikkNr" /* CHARACTER */)
           cKjede    = TRIM(ENTRY(2,cVerdier,CHR(1)))
           cRegion   = TRIM(ENTRY(3,cVerdier,CHR(1)))
           cDistrikt = TRIM(ENTRY(4,cVerdier,CHR(1)))
           cButikk   = TRIM(ENTRY(5,cVerdier,CHR(1))).
    RUN d-sokkjedebutikk.w (OUTPUT iSokButikk).
    IF RETURN-VALUE <> "AVBRYT" THEN DO:
        ASSIGN cVerdier = "".
        RUN finnKjedeButikk IN h_dkjedensbutikker (INPUT iSokButikk, OUTPUT cVerdier).
        IF cVerdier = "" THEN
            MESSAGE "Finner ikke butikken"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
        ELSE DO:
            ASSIGN cSokKjede    = TRIM(ENTRY(1,cVerdier,CHR(1)))
                   cSokRegion   = TRIM(ENTRY(2,cVerdier,CHR(1)))
                   cSokDistrikt = TRIM(ENTRY(3,cVerdier,CHR(1)))
                   cSokButikk   = TRIM(ENTRY(4,cVerdier,CHR(1))).
/*             IF cKjede <> cSokKjede THEN */
                DYNAMIC-FUNCTION('findRowWhere':U IN h_dkjede,
                               INPUT "KjedeNr" /* CHARACTER */,
                               INPUT cSokKjede /* CHARACTER */,
                               INPUT "=" /* CHARACTER */).
/*             IF cRegion <> cSokRegion THEN */
                DYNAMIC-FUNCTION('findRowWhere':U IN h_dkjederegion,
                               INPUT "RegionNr" /* CHARACTER */,
                               INPUT cSokRegion /* CHARACTER */,
                               INPUT "=" /* CHARACTER */).

/*             IF cDistrikt <> cSokDistrikt THEN */
                DYNAMIC-FUNCTION('findRowWhere':U IN h_dkjededistrikt,
                               INPUT "DistriktNr" /* CHARACTER */,
                               INPUT cSokDistrikt /* CHARACTER */,
                               INPUT "=" /* CHARACTER */).
/*             IF cButikk <> cSokButikk THEN */
                DYNAMIC-FUNCTION('findRowWhere':U IN h_dkjedensbutikker,
                               INPUT "ButikkNr" /* CHARACTER */,
                               INPUT cSokButikk /* CHARACTER */,
                               INPUT "=" /* CHARACTER */).
        END.
    END.
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
  DEF VAR iTilgangBut        AS INT    NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  
  RUN SUPER.
  /* it'nte tillgänglig vid start */
/*   IF VALID-HANDLE(h_dyntoolbar-region) THEN                           */
/*       DYNAMIC-FUNCTION('setDisabledActions':U IN h_dyntoolbar-region, */
/*      INPUT "COPY" /* CHARACTER */).                                   */

  /* Code placed here will execute AFTER standard behavior.    */
  IF VALID-HANDLE(h_dyntoolbar) THEN DO:
      RUN OpprettKnapper.
   END.

  SUBSCRIBE TO "PrintExcel" IN h_dynToolbar.
  SUBSCRIBE TO "ApplHjelp" IN h_dynToolbar.
  
  RUN initPages ( INPUT "1,2,3,4").

  {syspara.i 16 7 2 iTilgang INT}
  {syspara.i 16 8 2 iTilgangBut INT}
  IF iTilgang = 1 THEN
  DO:
    DYNAMIC-FUNCTION('setDisabledActions':U IN h_dyntoolbar,
     INPUT "ADD,COPY,DELETE" /* CHARACTER */).
    DYNAMIC-FUNCTION('setDisabledActions':U IN h_dyntoolbar-region,
     INPUT "ADD,COPY,DELETE" /* CHARACTER */).
    DYNAMIC-FUNCTION('setDisabledActions':U IN h_dyntoolbar-dist,
     INPUT "ADD,COPY,DELETE" /* CHARACTER */).
  END.
  ELSE DO:
      DYNAMIC-FUNCTION('setDisabledActions':U IN h_dyntoolbar,
       INPUT "COPY" /* CHARACTER */).
      DYNAMIC-FUNCTION('setDisabledActions':U IN h_dyntoolbar-region,
       INPUT "COPY" /* CHARACTER */).
      DYNAMIC-FUNCTION('setDisabledActions':U IN h_dyntoolbar-dist,
       INPUT "COPY" /* CHARACTER */).
  END.
  IF iTilgangBut = 1 THEN DO:
      DYNAMIC-FUNCTION('setDisabledActions':U IN h_dyntoolbar-but,
      INPUT "ADD,COPY,DELETE" /* CHARACTER */).
  END.
  ELSE
      DYNAMIC-FUNCTION('setDisabledActions':U IN h_dyntoolbar-but,
       INPUT "COPY" /* CHARACTER */).
  RUN HideFrame IN h_vkjederegionsmall (TRUE).
  RUN HideFrame IN h_vkjededistriktsmall (TRUE).
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
   DEFINE VARIABLE cHKInst AS CHARACTER  NO-UNDO.
   {syspara.i 1 1 18 cHKInst}
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

  /* Funksjon for blanking av Annonseflagg */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrintExcel wWin 
PROCEDURE PrintExcel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var ctmpFileName as char no-undo.
  def var cExcEkstent  as char no-undo.
  
  
  DO WITH FRAME {&FRAME-NAME}:
      FIND Kjede WHERE Kjede.KjedeNr = INT(DYNAMIC-FUNCTION('columnValue':U IN h_dkjede,INPUT "KjedeNr")) NO-LOCK NO-ERROR.
      IF NOT AVAIL Kjede THEN
          RETURN.
      {syspara.i 1 4 1 cExcEkstent}
      cExcEkstent = if cExcEkstent = "" then "sdv" else cExcEkstent.    
      /* Henter temporært filnavn. */
      if valid-handle(h_dproclib) then
        run GetTempFileName in h_dproclib (input "kjederapp", input cExcEkstent, output ctmpFileName).
      output stream Eksport to value(ctmpFileName).
      {sww.i}
       EXPORT STREAM Eksport DELIMITER ";"
       "Kjedestruktur" STRING(TODAY,"99/99/99").
       EXPORT STREAM Eksport DELIMITER ";"
                             "Kjede"
                             "KjedeRegion"
                             "KjedeDistrikt"
                             "KjedensButikker"
                             "KjedensButikker"
                             "Beliggenhet"
                             "DriftsForm"
                             "DriftsType".
       FOR EACH KjedeRegion OF Kjede NO-LOCK:
           FOR EACH KjedeDistrikt OF KjedeRegion NO-LOCK:
               FOR EACH KjedensButikker OF KjedeDistrikt NO-LOCK BY KjedensButikker.DriftsFormId BY KjedensButikker.DriftsTypeId:
                   FIND Beliggenhet OF KjedensButikker NO-LOCK NO-ERROR.
                   FIND Driftsform OF KjedensButikker NO-LOCK NO-ERROR.
                   FIND Driftstype OF KjedensButikker NO-LOCK NO-ERROR.
                   EXPORT STREAM Eksport DELIMITER ";"
                       Kjede.KjedeNavn
                       KjedeRegion.RegionNavn
                       KjedeDistrikt.DistriktNavn
                       KjedensButikker.ButikkNr
                       KjedensButikker.ButikkNavn
                       (IF AVAIL Beliggenhet THEN Beliggenhet.BeliggenhetNavn ELSE " ")
                       (IF AVAIL DriftsForm THEN DriftsForm.DriftsFormNavn ELSE " ")
                       (IF AVAIL DriftsType THEN DriftsType.DriftsTypeNavn ELSE " ").
               END.
           END.
       END.    
      EXPORT STREAM Eksport " ".
      output stream Eksport close.
  END.
  {swn.i}
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
  DEFINE VARIABLE        plCancel  AS LOGICAL NO-UNDO.
  DEFINE VARIABLE        iCurrPage AS INTEGER    NO-UNDO.
  /* Code placed here will execute PRIOR to standard behavior. */
  ASSIGN iCurrPage = DYNAMIC-FUNCTION('getCurrentPage':U).
  CASE iCurrPage:
      WHEN 1 THEN DO:
          IF VALID-HANDLE(h_vkjede)  AND
             DYNAMIC-FUNCTION('getDataModified':U IN h_vkjede) THEN
              RUN confirmExit (INPUT-OUTPUT plCancel /* LOGICAL */).
      END.
      WHEN 2 THEN DO:
          IF VALID-HANDLE(h_vkjederegion)  AND
             DYNAMIC-FUNCTION('getDataModified':U IN h_vkjederegion) THEN
              RUN confirmExit (INPUT-OUTPUT plCancel /* LOGICAL */).
      END.
      WHEN 3 THEN DO:
          IF VALID-HANDLE(h_vkjededistrikt)  AND
             DYNAMIC-FUNCTION('getDataModified':U IN h_vkjededistrikt) THEN
              RUN confirmExit (INPUT-OUTPUT plCancel /* LOGICAL */).
      END.
      WHEN 4 THEN DO:
          IF VALID-HANDLE(h_vkjedensbutikker)  AND
             DYNAMIC-FUNCTION('getDataModified':U IN h_vkjedensbutikker) THEN
              RUN confirmExit (INPUT-OUTPUT plCancel /* LOGICAL */).
      END.
  END CASE.
  IF plCancel THEN
      RETURN NO-APPLY "CANCEL".

  RUN SUPER( INPUT piPageNum).

  /* Code placed here will execute AFTER standard behavior.    */
  RUN HideFrame IN h_vkjederegionsmall (piPageNum < 2).
  RUN HideFrame IN h_vkjededistriktsmall (piPageNum < 3).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

