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
DEFINE VARIABLE h_bkassereroppgj AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dkassererkontanter AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dkassereroppgj AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar AS HANDLE NO-UNDO.
DEFINE VARIABLE h_fkassereroppgj AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_sortsok AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vkassereroppgj AS HANDLE NO-UNDO.

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 159.6 BY 21.48.


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
         TITLE              = "Kasserer oppgjør"
         HEIGHT             = 21.67
         WIDTH              = 131.2
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 161.2
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 161.2
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
   FRAME-NAME                                                           */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON CTRL-F1 OF wWin /* Kasserer oppgjør */
DO:
      IF valid-handle(h_dproclib) then
        RUN HjelpMap in h_dproclib.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Kasserer oppgjør */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Kasserer oppgjør */
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
             INPUT  'sdo/dkassereroppgj.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessnoDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedkassereroppgjOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dkassereroppgj ).
       RUN repositionObject IN h_dkassereroppgj ( 1.71 , 117.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'sdo/dkassererkontanter.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessnoDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsKassererKontanter.ButikkNr,ButikkNr,KassererKontanter.Dato,Dato,KassererKontanter.KassererNr,KassererNr,KassererKontanter.z_nummer,z_nummerObjectNamedkassererkontanterOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dkassererkontanter ).
       RUN repositionObject IN h_dkassererkontanter ( 1.71 , 97.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EdgePixels2DeactivateTargetOnHidenoDisabledActionsFlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsTableio,NavigationTableIOTypeSaveSupportedLinksNavigation-source,Tableio-sourceToolbarBandsToolbarAutoSizeyesToolbarDrawDirectionHorizontalLogicalObjectNameDisabledActionsHiddenActionsUpdate,Copy,ResetHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0RemoveMenuOnHidenoCreateSubMenuOnConflictyesNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar ).
       RUN repositionObject IN h_dyntoolbar ( 1.00 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar ( 1.57 , 131.20 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/folder.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FolderLabels':U + 'Liste|Oppgjør' + 'FolderTabWidth0FolderFont-1HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_folder ).
       RUN repositionObject IN h_folder ( 2.43 , 1.60 ) NO-ERROR.
       RUN resizeObject IN h_folder ( 20.00 , 130.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN initPages ('1':U) NO-ERROR.

       /* Links to SmartDataObject h_dkassereroppgj. */
       RUN addLink ( h_dyntoolbar , 'Navigation':U , h_dkassereroppgj ).
       RUN addLink ( h_fkassereroppgj , 'SokSdo':U , h_dkassereroppgj ).

       /* Links to SmartDataObject h_dkassererkontanter. */
       RUN addLink ( h_dkassereroppgj , 'Data':U , h_dkassererkontanter ).

       /* Links to SmartFolder h_folder. */
       RUN addLink ( h_folder , 'Page':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_folder ,
             h_dyntoolbar , 'AFTER':U ).
    END. /* Page 0 */
    WHEN 1 THEN DO:
       RUN constructObject (
             INPUT  'prg/bkassereroppgj.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bkassereroppgj ).
       RUN repositionObject IN h_bkassereroppgj ( 8.14 , 3.00 ) NO-ERROR.
       RUN resizeObject IN h_bkassereroppgj ( 13.81 , 126.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'prg/fkassereroppgj.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'LogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_fkassereroppgj ).
       RUN repositionObject IN h_fkassereroppgj ( 3.86 , 31.00 ) NO-ERROR.
       /* Size in AB:  ( 4.14 , 100.20 ) */

       RUN constructObject (
             INPUT  'prg/sortsok.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_sortsok ).
       RUN repositionObject IN h_sortsok ( 6.24 , 3.00 ) NO-ERROR.
       /* Size in AB:  ( 1.76 , 27.60 ) */

       /* Links to SmartDataBrowser h_bkassereroppgj. */
       RUN addLink ( h_dkassereroppgj , 'Data':U , h_bkassereroppgj ).

       /* Links to SmartObject h_sortsok. */
       RUN addLink ( h_bkassereroppgj , 'Sortera':U , h_sortsok ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_fkassereroppgj ,
             h_folder , 'AFTER':U ).
       RUN adjustTabOrder ( h_sortsok ,
             h_fkassereroppgj , 'AFTER':U ).
       RUN adjustTabOrder ( h_bkassereroppgj ,
             h_sortsok , 'AFTER':U ).
    END. /* Page 1 */
    WHEN 2 THEN DO:
       RUN constructObject (
             INPUT  'prg/vkassereroppgj.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EnabledObjFldsToDisable(None)ModifyFields(All)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vkassereroppgj ).
       RUN repositionObject IN h_vkassereroppgj ( 3.86 , 2.00 ) NO-ERROR.
       /* Size in AB:  ( 18.00 , 127.20 ) */

       /* Links to SmartDataViewer h_vkassereroppgj. */
       RUN addLink ( h_dkassereroppgj , 'Data':U , h_vkassereroppgj ).
       RUN addLink ( h_vkassereroppgj , 'Update':U , h_dkassereroppgj ).
       RUN addLink ( h_dyntoolbar , 'TableIo':U , h_vkassereroppgj ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_vkassereroppgj ,
             h_folder , 'AFTER':U ).
    END. /* Page 2 */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ButikkDag wWin 
PROCEDURE ButikkDag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER pcColValues AS CHAR NO-UNDO.

  DEF VAR pcWhere AS CHAR NO-UNDO.

  ASSIGN
      pcWhere = "KassererOppgj.ButikkNr = " + ENTRY(2,pcColValues,CHR(1)) +
                " and KassererOppgj.Dato = '" + ENTRY(3,pcColValues,CHR(1)) + "'"
      .

  DYNAMIC-FUNCTION('setQueryWhere':U IN h_dkassereroppgj,
          INPUT pcWhere).

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
      RUN SetFokus IN h_vkassereroppgj.

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
  IF VALID-HANDLE(h_dproclib) THEN
      RUN GetLng IN h_dproclib (OUTPUT wCurrLng).

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  PUBLISH "Sortera" FROM h_bkassereroppgj.

  IF VALID-HANDLE(h_dyntoolbar) THEN DO:
      RUN OpprettKnapper.
   END.
/*    IF iTilgang = 1 THEN                                       */
/*      DYNAMIC-FUNCTION('setDisabledActions':U IN h_dyntoolbar, */
/*        INPUT "ADD,COPY,DELETE" /* CHARACTER */).              */
   RUN viewObject IN h_bkassereroppgj.
   RUN viewObject IN h_sortsok.
   RUN viewObject IN h_fkassereroppgj.
/*   RUN selectPage (1). */
/*   RUN viewPage(1).    */
  SUBSCRIBE TO "ApplHjelp" IN h_dynToolbar.

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

  IF valid-handle(h_vkassereroppgj) THEN
  DO:
      ASSIGN
          pbDataModified = 
              DYNAMIC-FUNCTION('getDataModified':U IN h_vkassereroppgj)
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

