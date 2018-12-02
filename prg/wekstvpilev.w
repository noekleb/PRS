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
DEFINE VARIABLE h_bekstvpifil AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bekstvpilev AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bekstvpitabell AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dekstvpifil AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dekstvpilev AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dekstvpitabell AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar-3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_sortsok AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vekstvpifil AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vekstvpifilinfo AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vekstvpilev AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vekstvpilev2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vekstvpilev3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vekstvpitabell AS HANDLE NO-UNDO.

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 117 BY 19.76.


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
         TITLE              = "Import register"
         HEIGHT             = 19.76
         WIDTH              = 117
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.2
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.2
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
ON END-ERROR OF wWin /* Import register */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Import register */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
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
             INPUT  'sdo/dekstvpilev.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessnoDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedekstvpilevOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dekstvpilev ).
       RUN repositionObject IN h_dekstvpilev ( 2.19 , 98.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'sdo/dekstvpifil.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessnoDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsEkstVPIFil.EkstVPILevNr,EkstVPILevNrObjectNamedekstvpifilOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dekstvpifil ).
       RUN repositionObject IN h_dekstvpifil ( 2.19 , 89.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'sdo/dekstvpitabell.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessnoDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsEkstVPITabell.EkstVPILevNr,EkstVPILevNrObjectNamedekstvpitabellOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dekstvpitabell ).
       RUN repositionObject IN h_dekstvpitabell ( 2.19 , 108.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.00 ) */

       RUN constructObject (
             INPUT  'prg/vekstvpilev2.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EnabledObjFldsToDisable(None)ModifyFields(All)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vekstvpilev2 ).
       RUN repositionObject IN h_vekstvpilev2 ( 2.43 , 1.00 ) NO-ERROR.
       /* Size in AB:  ( 1.43 , 81.00 ) */

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EdgePixels2DeactivateTargetOnHidenoDisabledActionsFlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsTableio,NavigationTableIOTypeSaveSupportedLinksNavigation-source,Tableio-sourceToolbarBandsToolbarAutoSizenoToolbarDrawDirectionhorizontalLogicalObjectNameDisabledActionsHiddenActionsUpdate,ResetHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0RemoveMenuOnHidenoCreateSubMenuOnConflictyesNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar ).
       RUN repositionObject IN h_dyntoolbar ( 1.00 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar ( 1.24 , 117.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/folder.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FolderLabels':U + 'EDB systemer|Detaljer|Importfiler|Tabell' + 'FolderTabWidth0FolderFont-1HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_folder ).
       RUN repositionObject IN h_folder ( 3.86 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_folder ( 16.91 , 117.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN initPages ('3,4':U) NO-ERROR.

       /* Links to SmartDataObject h_dekstvpilev. */
       RUN addLink ( h_dyntoolbar , 'Navigation':U , h_dekstvpilev ).

       /* Links to SmartDataObject h_dekstvpifil. */
       RUN addLink ( h_dekstvpilev , 'Data':U , h_dekstvpifil ).
       RUN addLink ( h_dyntoolbar-2 , 'Navigation':U , h_dekstvpifil ).

       /* Links to SmartDataObject h_dekstvpitabell. */
       RUN addLink ( h_dekstvpilev , 'Data':U , h_dekstvpitabell ).
       RUN addLink ( h_dyntoolbar-3 , 'Navigation':U , h_dekstvpitabell ).

       /* Links to SmartDataViewer h_vekstvpilev2. */
       RUN addLink ( h_dekstvpilev , 'Data':U , h_vekstvpilev2 ).

       /* Links to SmartFolder h_folder. */
       RUN addLink ( h_folder , 'Page':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_vekstvpilev2 ,
             h_dyntoolbar , 'AFTER':U ).
       RUN adjustTabOrder ( h_folder ,
             h_vekstvpilev2 , 'AFTER':U ).
    END. /* Page 0 */
    WHEN 1 THEN DO:
       RUN constructObject (
             INPUT  'prg/bekstvpilev.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bekstvpilev ).
       RUN repositionObject IN h_bekstvpilev ( 7.43 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_bekstvpilev ( 12.86 , 115.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'prg/sortsok.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_sortsok ).
       RUN repositionObject IN h_sortsok ( 5.29 , 2.00 ) NO-ERROR.
       /* Size in AB:  ( 1.76 , 27.60 ) */

       /* Links to SmartDataBrowser h_bekstvpilev. */
       RUN addLink ( h_dekstvpilev , 'Data':U , h_bekstvpilev ).

       /* Links to SmartObject h_sortsok. */
       RUN addLink ( h_bekstvpilev , 'Sortera':U , h_sortsok ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_sortsok ,
             h_folder , 'AFTER':U ).
       RUN adjustTabOrder ( h_bekstvpilev ,
             h_sortsok , 'AFTER':U ).
    END. /* Page 1 */
    WHEN 2 THEN DO:
       RUN constructObject (
             INPUT  'prg/vekstvpilev.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EnabledObjFldsToDisable(None)ModifyFields(All)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vekstvpilev ).
       RUN repositionObject IN h_vekstvpilev ( 5.52 , 2.00 ) NO-ERROR.
       /* Size in AB:  ( 8.57 , 73.00 ) */

       RUN constructObject (
             INPUT  'prg/vekstvpilev3.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EnabledObjFldsToDisable(None)ModifyFields(All)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vekstvpilev3 ).
       RUN repositionObject IN h_vekstvpilev3 ( 19.57 , 2.00 ) NO-ERROR.
       /* Size in AB:  ( 0.71 , 80.20 ) */

       /* Links to SmartDataViewer h_vekstvpilev. */
       RUN addLink ( h_dekstvpilev , 'Data':U , h_vekstvpilev ).
       RUN addLink ( h_vekstvpilev , 'Update':U , h_dekstvpilev ).
       RUN addLink ( h_dyntoolbar , 'TableIO':U , h_vekstvpilev ).

       /* Links to SmartDataViewer h_vekstvpilev3. */
       RUN addLink ( h_dekstvpilev , 'Data':U , h_vekstvpilev3 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_vekstvpilev ,
             h_folder , 'AFTER':U ).
       RUN adjustTabOrder ( h_vekstvpilev3 ,
             h_vekstvpilev , 'AFTER':U ).
    END. /* Page 2 */
    WHEN 3 THEN DO:
       RUN constructObject (
             INPUT  'prg/vekstvpifil.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EnabledObjFldsToDisable(None)ModifyFields(All)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vekstvpifil ).
       RUN repositionObject IN h_vekstvpifil ( 6.71 , 51.00 ) NO-ERROR.
       /* Size in AB:  ( 12.38 , 66.00 ) */

       RUN constructObject (
             INPUT  'prg/bekstvpifil.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bekstvpifil ).
       RUN repositionObject IN h_bekstvpifil ( 6.71 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_bekstvpifil ( 12.38 , 48.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'prg/vekstvpifilinfo.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EnabledObjFldsToDisable(None)ModifyFields(All)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vekstvpifilinfo ).
       RUN repositionObject IN h_vekstvpifilinfo ( 19.81 , 2.00 ) NO-ERROR.
       /* Size in AB:  ( 0.76 , 79.00 ) */

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EdgePixels2DeactivateTargetOnHidenoDisabledActionsFlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsTableio,NavigationTableIOTypeSaveSupportedLinksNavigation-source,Tableio-sourceToolbarBandsToolbarAutoSizenoToolbarDrawDirectionhorizontalLogicalObjectNameDisabledActionsHiddenActionsUpdate,ResetHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0RemoveMenuOnHidenoCreateSubMenuOnConflictyesNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar-2 ).
       RUN repositionObject IN h_dyntoolbar-2 ( 5.29 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar-2 ( 1.24 , 115.00 ) NO-ERROR.

       /* Links to SmartDataViewer h_vekstvpifil. */
       RUN addLink ( h_dekstvpifil , 'Data':U , h_vekstvpifil ).
       RUN addLink ( h_vekstvpifil , 'Update':U , h_dekstvpifil ).
       RUN addLink ( h_dyntoolbar-2 , 'TableIo':U , h_vekstvpifil ).

       /* Links to SmartDataBrowser h_bekstvpifil. */
       RUN addLink ( h_dekstvpifil , 'Data':U , h_bekstvpifil ).

       /* Links to SmartDataViewer h_vekstvpifilinfo. */
       RUN addLink ( h_dekstvpifil , 'Data':U , h_vekstvpifilinfo ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_dyntoolbar-2 ,
             h_folder , 'AFTER':U ).
       RUN adjustTabOrder ( h_bekstvpifil ,
             h_dyntoolbar-2 , 'AFTER':U ).
       RUN adjustTabOrder ( h_vekstvpifil ,
             h_bekstvpifil , 'AFTER':U ).
       RUN adjustTabOrder ( h_vekstvpifilinfo ,
             h_vekstvpifil , 'AFTER':U ).
    END. /* Page 3 */
    WHEN 4 THEN DO:
       RUN constructObject (
             INPUT  'prg/bekstvpitabell.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bekstvpitabell ).
       RUN repositionObject IN h_bekstvpitabell ( 6.71 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_bekstvpitabell ( 13.81 , 66.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'prg/vekstvpitabell.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EnabledObjFldsToDisable(None)ModifyFields(All)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vekstvpitabell ).
       RUN repositionObject IN h_vekstvpitabell ( 6.71 , 69.00 ) NO-ERROR.
       /* Size in AB:  ( 2.86 , 45.00 ) */

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EdgePixels2DeactivateTargetOnHidenoDisabledActionsFlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsTableio,NAVIGATIONTableIOTypeSaveSupportedLinksNavigation-source,Tableio-sourceToolbarBandsToolbarAutoSizenoToolbarDrawDirectionhorizontalLogicalObjectNameDisabledActionsHiddenActionsUpdate,ResetHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0RemoveMenuOnHidenoCreateSubMenuOnConflictyesNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar-3 ).
       RUN repositionObject IN h_dyntoolbar-3 ( 5.29 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar-3 ( 1.24 , 112.00 ) NO-ERROR.

       /* Links to SmartDataBrowser h_bekstvpitabell. */
       RUN addLink ( h_dekstvpitabell , 'Data':U , h_bekstvpitabell ).

       /* Links to SmartDataViewer h_vekstvpitabell. */
       RUN addLink ( h_dekstvpitabell , 'Data':U , h_vekstvpitabell ).
       RUN addLink ( h_vekstvpitabell , 'Update':U , h_dekstvpitabell ).
       RUN addLink ( h_dyntoolbar-3 , 'Tableio':U , h_vekstvpitabell ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_dyntoolbar-3 ,
             h_folder , 'AFTER':U ).
       RUN adjustTabOrder ( h_bekstvpitabell ,
             h_dyntoolbar-3 , 'AFTER':U ).
       RUN adjustTabOrder ( h_vekstvpitabell ,
             h_bekstvpitabell , 'AFTER':U ).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE changePage wWin 
PROCEDURE changePage :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  DEF VAR piCurrPage AS INT NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  assign
    piCurrPage = DYNAMIC-FUNCTION('getCurrentPage':U)
    .

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  IF piCurrPage = 2 THEN
      RUN SetFokus IN h_vekstvpilev.
  ELSE IF piCurrPage = 3 THEN
      RUN SetFokus IN h_vekstvpifil.

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
  IF VALID-HANDLE(h_dyntoolbar) THEN DO:
      RUN OpprettKnapper.
   END.

  SUBSCRIBE TO "ApplHjelp" IN h_dynToolbar.
  PUBLISH "Sortera" FROM h_bekstvpilev.

  IF VALID-HANDLE(h_dproclib) THEN
      RUN GetLng IN h_dproclib (OUTPUT wCurrLng).


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

  IF valid-handle(h_vekstvpilev) THEN
  DO:
      /* Kontrollerer EDB system viewer */
      ASSIGN
          pbDataModified = 
              DYNAMIC-FUNCTION('getDataModified':U IN h_vekstvpilev)
          .
      /* Kontrollerer FilVier. */
      IF pbDataModified = FALSE THEN
          pbDataModified = 
              DYNAMIC-FUNCTION('getDataModified':U IN h_vekstvpifil)
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

