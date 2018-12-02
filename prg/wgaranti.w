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
DEFINE VARIABLE h_bgaranti AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dgaranti AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vgaranti AS HANDLE NO-UNDO.

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 66.4 BY 19.62.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Design Page: 2
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Registervedlikehold garantiklasser"
         HEIGHT             = 19.62
         WIDTH              = 66.4
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
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Registervedlikehold garantiklasser */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Registervedlikehold garantiklasser */
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
             INPUT  'sdo/dgaranti.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedgarantiUpdateFromSourcenoToggleDataTargetsyesOpenOnInityesPromptOnDeleteyesPromptColumns(NONE)':U ,
             OUTPUT h_dgaranti ).
       RUN repositionObject IN h_dgaranti ( 1.48 , 40.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsTableio,NavigationSubModulesTableIOTypeSaveSupportedLinksNavigation-source,Tableio-sourceToolbarBandsToolbarParentMenuToolbarAutoSizenoToolbarDrawDirectionHorizontalToolbarInitialStateLogicalObjectNameAutoResizeDisabledActionsHiddenActionsUpdateHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0EdgePixels2PanelTypeToolbarDeactivateTargetOnHidenoDisabledActionsNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar ).
       RUN repositionObject IN h_dyntoolbar ( 1.00 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar ( 1.24 , 66.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/folder.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FolderLabels':U + 'Liste|Detaljer' + 'FolderTabWidth0FolderFont-1HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_folder ).
       RUN repositionObject IN h_folder ( 2.43 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_folder ( 18.10 , 66.00 ) NO-ERROR.

       /* Links to SmartDataObject h_dgaranti. */
       RUN addLink ( h_dyntoolbar , 'Navigation':U , h_dgaranti ).

       /* Links to SmartFolder h_folder. */
       RUN addLink ( h_folder , 'Page':U , THIS-PROCEDURE ).

    END. /* Page 0 */

    WHEN 1 THEN DO:
       RUN constructObject (
             INPUT  'prg/bgaranti.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bgaranti ).
       RUN repositionObject IN h_bgaranti ( 3.86 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_bgaranti ( 16.43 , 52.00 ) NO-ERROR.

       /* Links to SmartDataBrowser h_bgaranti. */
       RUN addLink ( h_dgaranti , 'Data':U , h_bgaranti ).

    END. /* Page 1 */

    WHEN 2 THEN DO:
       RUN constructObject (
             INPUT  'prg/vgaranti.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EnabledObjFldsToDisable(None)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vgaranti ).
       RUN repositionObject IN h_vgaranti ( 3.62 , 2.00 ) NO-ERROR.
       /* Size in AB:  ( 15.29 , 48.00 ) */

       /* Links to SmartDataViewer h_vgaranti. */
       RUN addLink ( h_dgaranti , 'Data':U , h_vgaranti ).
       RUN addLink ( h_vgaranti , 'Update':U , h_dgaranti ).
       RUN addLink ( h_dyntoolbar , 'TableIO':U , h_vgaranti ).

       /* Adjust the tab order of the smart objects. */
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

