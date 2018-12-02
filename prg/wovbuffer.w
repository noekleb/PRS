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
DEFINE VAR hHelp    AS HANDLE NO-UNDO.
DEFINE VAR hExit    AS HANDLE NO-UNDO.
DEFINE VAR piX      AS INTEGER    NO-UNDO.

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

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_bovbuffer AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bovbunt AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dovbuffer AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dovbunt AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_fvisbilde AS HANDLE NO-UNDO.
DEFINE VARIABLE h_sortsok AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vovbuffer AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vovbunt AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vovbuntendretinfo AS HANDLE NO-UNDO.

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     SPACE(159.01) SKIP(26.44)
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


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
         TITLE              = "Overføringstransaksjoner"
         HEIGHT             = 26.52
         WIDTH              = 159.6
         MAX-HEIGHT         = 30.24
         MAX-WIDTH          = 199.8
         VIRTUAL-HEIGHT     = 30.24
         VIRTUAL-WIDTH      = 199.8
         MAX-BUTTON         = no
         RESIZE             = no
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
{hjelp.i}

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

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Overføringstransaksjoner */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Overføringstransaksjoner */
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

ON "CTRL-TAB":U OF wWin ANYWHERE 
DO:
  RUN ByttSide("+").
END.

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
             INPUT  'sdo/dovbunt.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessnoDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedovbuntOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dovbunt ).
       RUN repositionObject IN h_dovbunt ( 2.43 , 130.00 ) NO-ERROR.
       /* Size in AB:  ( 1.91 , 11.00 ) */

       RUN constructObject (
             INPUT  'sdo/dovbuffer.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessnoDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsOvBuffer.BuntNr,BuntNrObjectNamedovbufferOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dovbuffer ).
       RUN repositionObject IN h_dovbuffer ( 2.43 , 140.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EdgePixels2DeactivateTargetOnHidenoDisabledActionsFlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsTableio,NavigationTableIOTypeSaveSupportedLinksNavigation-source,Tableio-sourceToolbarBandsToolbarAutoSizenoToolbarDrawDirectionhorizontalLogicalObjectNameDisabledActionsHiddenActionsUpdate,ResetHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0RemoveMenuOnHidenoCreateSubMenuOnConflictyesNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar ).
       RUN repositionObject IN h_dyntoolbar ( 1.00 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar ( 1.24 , 159.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/folder.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FolderLabels':U + 'Batch|Transaksjoner' + 'FolderTabWidth0FolderFont-1HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_folder ).
       RUN repositionObject IN h_folder ( 2.67 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_folder ( 24.76 , 159.00 ) NO-ERROR.

       /* Links to SmartDataObject h_dovbunt. */
       RUN addLink ( h_dyntoolbar , 'Navigation':U , h_dovbunt ).

       /* Links to SmartDataObject h_dovbuffer. */
       RUN addLink ( h_dovbunt , 'Data':U , h_dovbuffer ).

       /* Links to SmartFolder h_folder. */
       RUN addLink ( h_folder , 'Page':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_folder ,
             h_dyntoolbar , 'AFTER':U ).
    END. /* Page 0 */
    WHEN 1 THEN DO:
       RUN constructObject (
             INPUT  'prg/vovbunt.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EnabledObjFldsToDisable(None)ModifyFields(All)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vovbunt ).
       RUN repositionObject IN h_vovbunt ( 4.14 , 2.00 ) NO-ERROR.
       /* Size in AB:  ( 4.52 , 85.00 ) */

       RUN constructObject (
             INPUT  'prg/bovbunt.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bovbunt ).
       RUN repositionObject IN h_bovbunt ( 10.52 , 3.00 ) NO-ERROR.
       RUN resizeObject IN h_bovbunt ( 15.48 , 155.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'prg/vovbuntendretinfo.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EnabledObjFldsToDisable(None)ModifyFields(All)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vovbuntendretinfo ).
       RUN repositionObject IN h_vovbuntendretinfo ( 26.24 , 3.00 ) NO-ERROR.
       /* Size in AB:  ( 0.81 , 82.00 ) */

       RUN constructObject (
             INPUT  'prg/sortsok.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_sortsok ).
       RUN repositionObject IN h_sortsok ( 8.62 , 3.00 ) NO-ERROR.
       /* Size in AB:  ( 1.76 , 27.60 ) */

       /* Links to SmartDataViewer h_vovbunt. */
       RUN addLink ( h_dovbunt , 'Data':U , h_vovbunt ).
       RUN addLink ( h_vovbunt , 'Update':U , h_dovbunt ).
       RUN addLink ( h_dyntoolbar , 'Tableio':U , h_vovbunt ).

       /* Links to SmartDataBrowser h_bovbunt. */
       RUN addLink ( h_dovbunt , 'Data':U , h_bovbunt ).

       /* Links to SmartDataViewer h_vovbuntendretinfo. */
       RUN addLink ( h_dovbunt , 'Data':U , h_vovbuntendretinfo ).

       /* Links to SmartObject h_sortsok. */
       RUN addLink ( h_bovbunt , 'Sortera':U , h_sortsok ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_vovbunt ,
             h_folder , 'AFTER':U ).
       RUN adjustTabOrder ( h_sortsok ,
             h_vovbunt , 'AFTER':U ).
       RUN adjustTabOrder ( h_bovbunt ,
             h_sortsok , 'AFTER':U ).
       RUN adjustTabOrder ( h_vovbuntendretinfo ,
             h_bovbunt , 'AFTER':U ).
    END. /* Page 1 */
    WHEN 2 THEN DO:
       RUN constructObject (
             INPUT  'prg/vovbuffer.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EnabledObjFldsToDisable(None)ModifyFields(All)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vovbuffer ).
       RUN repositionObject IN h_vovbuffer ( 5.33 , 1.80 ) NO-ERROR.
       /* Size in AB:  ( 5.71 , 126.00 ) */

       RUN constructObject (
             INPUT  'prg/fvisbilde.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'LogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_fvisbilde ).
       RUN repositionObject IN h_fvisbilde ( 5.91 , 129.60 ) NO-ERROR.
       /* Size in AB:  ( 4.81 , 28.20 ) */

       RUN constructObject (
             INPUT  'prg/bovbuffer.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bovbuffer ).
       RUN repositionObject IN h_bovbuffer ( 11.24 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_bovbuffer ( 16.10 , 157.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EdgePixels2DeactivateTargetOnHidenoDisabledActionsFlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsTableioTableIOTypeSaveSupportedLinksTableio-sourceToolbarBandsToolbarAutoSizenoToolbarDrawDirectionhorizontalLogicalObjectNameDisabledActionsHiddenActionsUpdate,ResetHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0RemoveMenuOnHidenoCreateSubMenuOnConflictyesNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar-2 ).
       RUN repositionObject IN h_dyntoolbar-2 ( 4.14 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar-2 ( 1.24 , 157.00 ) NO-ERROR.

       /* Links to SmartDataViewer h_vovbuffer. */
       RUN addLink ( h_dovbuffer , 'Data':U , h_vovbuffer ).
       RUN addLink ( h_vovbuffer , 'Update':U , h_dovbuffer ).
       RUN addLink ( h_dyntoolbar-2 , 'TableIo':U , h_vovbuffer ).

       /* Links to SmartFrame h_fvisbilde. */
       RUN addLink ( h_dovbuffer , 'Data':U , h_fvisbilde ).
       RUN addLink ( h_folder , 'Page':U , h_fvisbilde ).

       /* Links to SmartDataBrowser h_bovbuffer. */
       RUN addLink ( h_dovbuffer , 'Data':U , h_bovbuffer ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_dyntoolbar-2 ,
             h_folder , 'AFTER':U ).
       RUN adjustTabOrder ( h_vovbuffer ,
             h_dyntoolbar-2 , 'AFTER':U ).
       RUN adjustTabOrder ( h_fvisbilde ,
             h_vovbuffer , 'AFTER':U ).
       RUN adjustTabOrder ( h_bovbuffer ,
             h_fvisbilde , 'AFTER':U ).
    END. /* Page 2 */

  END CASE.
  /* Select a Startup page. */
  IF currentPage eq 0
  THEN RUN selectPage IN THIS-PROCEDURE ( 1 ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BrowseBuntEntry wWin 
PROCEDURE BrowseBuntEntry :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    APPLY "ENTRY" TO h_bovbunt.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByttSide wWin 
PROCEDURE ByttSide :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER pcChar AS char NO-UNDO.

  DEF VAR piCurrPage AS INT NO-UNDO.
  DEF VAR piMaxFlik  AS INT NO-UNDO.

      
  ASSIGN
    piCurrPage = DYNAMIC-FUNCTION('getCurrentPage':U)
    piMaxFlik  = 2
    .
  IF pcChar = "+" THEN
    piCurrPage = piCurrPage + 1.
  ELSE 
    piCurrPage = piCurrPage - 1.
  IF piCurrPage > piMaxFlik THEN
      piCurrPage = 1.
  IF piCurrPage < 1 THEN
      piCurrPage = piMaxFlik.
  RUN selectPage
    ( INPUT piCurrPage /* INTEGER */).

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
  DEF VAR piPage AS INT NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  ASSIGN
      piPage = getCurrentPage()
      .

  RUN SUPER.
  
/* Code placed here will execute AFTER standard behavior.    */
  CASE piPage:
      WHEN 1 THEN RUN SetFokus IN h_vovbunt   NO-ERROR.
      WHEN 2 THEN RUN SetFokus IN h_vovbuffer NO-ERROR.
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
      RUN skapaButtons.
   END.
  
   DYNAMIC-FUNCTION('setDisabledActions':U IN h_dyntoolbar,
     INPUT "Copy" /* CHARACTER */).
  SUBSCRIBE TO "pgmHelp" IN h_dyntoolbar.
  SUBSCRIBE TO "SendWinH" ANYWHERE.
  
  PUBLISH "Sortera" FROM h_bovbunt.
  RUN BrowseBuntEntry.
/*   RUN SetFokus IN h_vovbunt   NO-ERROR. */
  DYNAMIC-FUNCTION("InitTranslation",THIS-PROCEDURE:CURRENT-WINDOW).

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE selectPage wWin 
PROCEDURE selectPage :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER piPageNum   AS INTEGER NO-UNDO.
  DEFINE VARIABLE        plCancel    AS LOGICAL NO-UNDO.
  IF DYNAMIC-FUNCTION('getCurrentPage':U) = 1 AND VALID-HANDLE(h_vovbunt) THEN DO:
      IF DYNAMIC-FUNCTION('getRecordState':U IN h_vovbunt) = "NoRecordAvailable" AND 
               DYNAMIC-FUNCTION('getDataModified':U IN h_vovbunt) = FALSE THEN DO:
         MESSAGE "Ingen post er registrert!"
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
         RETURN.
      END.
      RUN confirmExit IN h_vovbunt
        ( INPUT-OUTPUT plCancel /* LOGICAL */).
      IF NOT plCancel  THEN
          RUN cancelRecord IN h_vovbunt.
      IF DYNAMIC-FUNCTION('getRecordState':U IN h_vovbunt) = "NoRecordAvailable" THEN
          RETURN.
     DYNAMIC-FUNCTION('disableActions':U IN h_dyntoolbar,
       INPUT "Add,Update,Copy,Delete" /* CHARACTER */).

  END.
  ELSE IF DYNAMIC-FUNCTION('getCurrentPage':U) = 2 AND 
                         VALID-HANDLE(h_vovbuffer) THEN DO:
      IF NOT DYNAMIC-FUNCTION('getCurrentRowModified':U IN h_dovbuffer) = ? THEN DO:
          RUN confirmExit IN h_vovbuffer
                (INPUT-OUTPUT plCancel /* LOGICAL */).
          IF NOT plCancel  THEN DO:
              RUN cancelRecord IN h_vovbuffer.
          END.
      END.
  END.
  IF plCancel THEN
      RETURN NO-APPLY.
  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER( INPUT piPageNum).
  
  /* Code placed here will execute AFTER standard behavior.    */
  IF DYNAMIC-FUNCTION('getCurrentPage':U) = 1 THEN DO:
      IF VALID-HANDLE(hPrint) THEN
          ASSIGN hPrint:SENSITIVE = TRUE.
      DYNAMIC-FUNCTION('disableActions':U IN h_dyntoolbar,
        INPUT "Copy" /* CHARACTER */).
  END.
  ELSE IF DYNAMIC-FUNCTION('getCurrentPage':U) = 2 THEN DO: 
      IF VALID-HANDLE(hPrint) THEN
          ASSIGN hPrint:SENSITIVE = FALSE.
      DYNAMIC-FUNCTION('openQuery':U IN h_dovbuffer).
      IF DYNAMIC-FUNCTION('getRecordState':U IN h_vovbunt) = "NoRecordAvailable" THEN DO:
          RUN disableObject IN h_vovbuffer.
          DYNAMIC-FUNCTION('disableActions':U IN h_dyntoolbar-2,
              INPUT "Add" /* CHARACTER */).
      END.
      ELSE IF VALID-HANDLE(h_dovbuffer)THEN DO:
        IF DYNAMIC-FUNCTION('getRecordState':U IN h_vovbuffer) = "NoRecordAvailable" THEN DO:
            RUN disableObject IN h_vovbuffer.
            DYNAMIC-FUNCTION('enableActions':U IN h_dyntoolbar-2,
                INPUT "Add" /* CHARACTER */).
        END.
        ELSE IF DYNAMIC-FUNCTION('columnValue':U IN h_dovbuffer,
                       INPUT "fDatoOppdatert" /* CHARACTER */) <> ? THEN
          DYNAMIC-FUNCTION('disableActions':U IN h_dyntoolbar-2,
              INPUT "Add,Update,Copy,Delete" /* CHARACTER */).
      END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendWinH wWin 
PROCEDURE SendWinH :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER hWin AS HANDLE      NO-UNDO.
     hWin = wWin.
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
  DEFINE VARIABLE hPanelFrame AS HANDLE     NO-UNDO.
  ASSIGN hPanelFrame = DYNAMIC-FUNCTION('getContainerHandle':U IN h_dyntoolbar).
  /* Printbutton */
  ASSIGN piX = 260
          hPrint = DYNAMIC-FUNCTION('createButton':U IN h_dyntoolbar,
          hPanelFrame,
/*            INPUT FRAME {&FRAME-NAME}:HANDLE /* HANDLE */, */
           INPUT-OUTPUT piX /* INTEGER */,
           INPUT "Print:ovBunt" /* CHARACTER */,
           INPUT "Print" /* CHARACTER */,
           INPUT "Rapport" /* CHARACTER */,
           INPUT "icon\e-print.bmp" /* pcBitmap CHARACTER */,
           INPUT TRUE /* LOGICAL */).
/*    hPrint:MOVE-TO-TOP(). */
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

   ASSIGN piX = hPanelFrame:WIDTH-PIXELS - hPrint:WIDTH-PIXELS - 3
          hExit = DYNAMIC-FUNCTION('createButton':U IN h_dyntoolbar,
          hPanelFrame,
/*            INPUT FRAME {&FRAME-NAME}:HANDLE /* HANDLE */, */
           INPUT-OUTPUT piX /* INTEGER */,
           INPUT "exit" /* CHARACTER */,
           INPUT "exit" /* CHARACTER */,
           INPUT "exit" /* CHARACTER */,
           INPUT "icon\e-exit.bmp" /* pcBitmap CHARACTER */,
           INPUT TRUE /* LOGICAL */).
/*    hExit:MOVE-TO-TOP(). */

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
          hPanelFrame,
/*            INPUT FRAME {&FRAME-NAME}:HANDLE /* HANDLE */, */
           INPUT-OUTPUT piX /* INTEGER */,
           INPUT "HELP" /* CHARACTER */,
           INPUT "HELP" /* CHARACTER */,
           INPUT "HELP" /* CHARACTER */,
           INPUT "icon\e-help.bmp" /* pcBitmap CHARACTER */,
           INPUT TRUE /* LOGICAL */).
/*    hHelp:MOVE-TO-TOP(). */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

