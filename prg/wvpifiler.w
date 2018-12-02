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
DEF VAR cKataloger    AS CHAR NO-UNDO.
DEF VAR cFilNavnListe AS CHAR NO-UNDO.

DEF BUFFER bufVPIFilHode FOR VPIFilHode.

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
DEFINE VARIABLE h_bvpifilhode AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bvpifillinje AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bvpifillogg AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dvpidatasett AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dvpifilhode AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dvpifillinje AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dvpifillogg AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_fvpifilertoolbar AS HANDLE NO-UNDO.
DEFINE VARIABLE h_fvpifilhode AS HANDLE NO-UNDO.
DEFINE VARIABLE h_fvpifillinje AS HANDLE NO-UNDO.
DEFINE VARIABLE h_fvpifilloggfilter AS HANDLE NO-UNDO.
DEFINE VARIABLE h_sortsok AS HANDLE NO-UNDO.
DEFINE VARIABLE h_sortsok-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_sortsok-3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vvpifilhode AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vvpifillinje AS HANDLE NO-UNDO.

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 161.2 BY 27.19.


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
         TITLE              = "Import og behandling av filer fra eksterne systemer"
         HEIGHT             = 27.19
         WIDTH              = 161.2
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 161.2
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 161.2
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
ON END-ERROR OF wWin /* Import og behandling av filer fra eksterne systemer */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Import og behandling av filer fra eksterne systemer */
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
             INPUT  'sdo/dvpifilhode.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessnoDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedvpifilhodeOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dvpifilhode ).
       RUN repositionObject IN h_dvpifilhode ( 1.71 , 128.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EdgePixels2DeactivateTargetOnHidenoDisabledActionsFlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsNavigationTableIOTypeSaveSupportedLinksNavigation-sourceToolbarBandsToolbarAutoSizenoToolbarDrawDirectionhorizontalLogicalObjectNameDisabledActionsHiddenActionsUpdate,ResetHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0RemoveMenuOnHidenoCreateSubMenuOnConflictyesNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar ).
       RUN repositionObject IN h_dyntoolbar ( 1.00 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar ( 1.24 , 160.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'sdo/dvpifillinje.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessnoDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsVPIFilLinje.FilId,FilIdObjectNamedvpifillinjeOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dvpifillinje ).
       RUN repositionObject IN h_dvpifillinje ( 1.71 , 140.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'sdo/dvpifillogg.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessnoDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsVPIFilLogg.FilId,FilIdObjectNamedvpifilloggOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dvpifillogg ).
       RUN repositionObject IN h_dvpifillogg ( 2.19 , 134.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'prg/vvpifilhode.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EnabledObjFldsToDisable(None)ModifyFields(All)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vvpifilhode ).
       RUN repositionObject IN h_vvpifilhode ( 2.43 , 1.00 ) NO-ERROR.
       /* Size in AB:  ( 1.43 , 160.00 ) */

       RUN constructObject (
             INPUT  'adm2/folder.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FolderLabels':U + 'Filer|Fillinjer|Fillogg' + 'FolderTabWidth0FolderFont-1HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_folder ).
       RUN repositionObject IN h_folder ( 4.10 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_folder ( 24.05 , 161.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN initPages ('1,2,3':U) NO-ERROR.

       /* Links to SmartDataObject h_dvpifilhode. */
       RUN addLink ( h_dyntoolbar , 'Navigation':U , h_dvpifilhode ).
       RUN addLink ( h_fvpifilhode , 'SokSdo':U , h_dvpifilhode ).

       /* Links to toolbar h_dyntoolbar. */
       RUN addLink ( h_dvpifilhode , 'Data':U , h_dyntoolbar ).

       /* Links to SmartDataObject h_dvpifillinje. */
       RUN addLink ( h_dvpifilhode , 'Data':U , h_dvpifillinje ).
       RUN addLink ( h_fvpifillinje , 'SokSdo':U , h_dvpifillinje ).

       /* Links to SmartDataObject h_dvpifillogg. */
       RUN addLink ( h_dvpifilhode , 'Data':U , h_dvpifillogg ).
       RUN addLink ( h_fvpifilloggfilter , 'SokSdo':U , h_dvpifillogg ).

       /* Links to SmartDataViewer h_vvpifilhode. */
       RUN addLink ( h_dvpifilhode , 'Data':U , h_vvpifilhode ).
       RUN addLink ( h_vvpifilhode , 'Update':U , h_dvpifilhode ).

       /* Links to SmartFolder h_folder. */
       RUN addLink ( h_folder , 'Page':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_vvpifilhode ,
             h_dyntoolbar , 'AFTER':U ).
       RUN adjustTabOrder ( h_folder ,
             h_vvpifilhode , 'AFTER':U ).
    END. /* Page 0 */
    WHEN 1 THEN DO:
       RUN constructObject (
             INPUT  'sdo/dvpidatasett.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessnoDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsVPIDatasett.EkstVPILevNr,EkstVPILevNrObjectNamedvpidatasettOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dvpidatasett ).
       RUN repositionObject IN h_dvpidatasett ( 3.38 , 150.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'prg/fvpifilhode.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'LogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_fvpifilhode ).
       RUN repositionObject IN h_fvpifilhode ( 5.52 , 30.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 79.40 ) */

       RUN constructObject (
             INPUT  'prg/bvpifilhode.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bvpifilhode ).
       RUN repositionObject IN h_bvpifilhode ( 7.43 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_bvpifilhode ( 20.48 , 138.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'prg/sortsok.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_sortsok ).
       RUN repositionObject IN h_sortsok ( 5.52 , 2.00 ) NO-ERROR.
       /* Size in AB:  ( 1.76 , 27.60 ) */

       RUN constructObject (
             INPUT  'prg/fvpifilertoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'LogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_fvpifilertoolbar ).
       RUN repositionObject IN h_fvpifilertoolbar ( 7.24 , 141.00 ) NO-ERROR.
       /* Size in AB:  ( 20.76 , 20.00 ) */

       /* Links to SmartDataObject h_dvpidatasett. */
       RUN addLink ( h_dvpifilhode , 'Data':U , h_dvpidatasett ).

       /* Links to SmartFrame h_fvpifilhode. */
       RUN addLink ( h_dvpifilhode , 'Data':U , h_fvpifilhode ).

       /* Links to SmartDataBrowser h_bvpifilhode. */
       RUN addLink ( h_dvpifilhode , 'Data':U , h_bvpifilhode ).

       /* Links to SmartObject h_sortsok. */
       RUN addLink ( h_bvpifilhode , 'Sortera':U , h_sortsok ).

       /* Links to SmartFrame h_fvpifilertoolbar. */
       RUN addLink ( h_folder , 'Page':U , h_fvpifilertoolbar ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_sortsok ,
             h_folder , 'AFTER':U ).
       RUN adjustTabOrder ( h_fvpifilhode ,
             h_sortsok , 'AFTER':U ).
       RUN adjustTabOrder ( h_fvpifilertoolbar ,
             h_fvpifilhode , 'AFTER':U ).
       RUN adjustTabOrder ( h_bvpifilhode ,
             h_fvpifilertoolbar , 'AFTER':U ).
    END. /* Page 1 */
    WHEN 2 THEN DO:
       RUN constructObject (
             INPUT  'prg/bvpifillinje.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bvpifillinje ).
       RUN repositionObject IN h_bvpifillinje ( 7.19 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_bvpifillinje ( 15.48 , 159.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'prg/vvpifillinje.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EnabledObjFldsToDisable(None)ModifyFields(All)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vvpifillinje ).
       RUN repositionObject IN h_vvpifillinje ( 22.91 , 2.00 ) NO-ERROR.
       /* Size in AB:  ( 4.76 , 159.00 ) */

       RUN constructObject (
             INPUT  'prg/sortsok.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_sortsok-3 ).
       RUN repositionObject IN h_sortsok-3 ( 5.29 , 2.00 ) NO-ERROR.
       /* Size in AB:  ( 1.76 , 27.60 ) */

       RUN constructObject (
             INPUT  'prg/fvpifillinje.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'LogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_fvpifillinje ).
       RUN repositionObject IN h_fvpifillinje ( 5.29 , 30.00 ) NO-ERROR.
       /* Size in AB:  ( 1.76 , 79.40 ) */

       /* Links to SmartDataBrowser h_bvpifillinje. */
       RUN addLink ( h_dvpifillinje , 'Data':U , h_bvpifillinje ).

       /* Links to SmartDataViewer h_vvpifillinje. */
       RUN addLink ( h_dvpifillinje , 'Data':U , h_vvpifillinje ).
       RUN addLink ( h_vvpifillinje , 'Update':U , h_dvpifillinje ).

       /* Links to SmartObject h_sortsok-3. */
       RUN addLink ( h_bvpifillinje , 'Sortera':U , h_sortsok-3 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_sortsok-3 ,
             h_folder , 'AFTER':U ).
       RUN adjustTabOrder ( h_fvpifillinje ,
             h_sortsok-3 , 'AFTER':U ).
       RUN adjustTabOrder ( h_bvpifillinje ,
             h_fvpifillinje , 'AFTER':U ).
       RUN adjustTabOrder ( h_vvpifillinje ,
             h_bvpifillinje , 'AFTER':U ).
    END. /* Page 2 */
    WHEN 3 THEN DO:
       RUN constructObject (
             INPUT  'prg/bvpifillogg.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bvpifillogg ).
       RUN repositionObject IN h_bvpifillogg ( 7.43 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_bvpifillogg ( 20.48 , 158.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'prg/sortsok.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_sortsok-2 ).
       RUN repositionObject IN h_sortsok-2 ( 5.52 , 2.00 ) NO-ERROR.
       /* Size in AB:  ( 1.76 , 27.60 ) */

       RUN constructObject (
             INPUT  'prg/fvpifilloggfilter.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'LogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_fvpifilloggfilter ).
       RUN repositionObject IN h_fvpifilloggfilter ( 5.52 , 30.00 ) NO-ERROR.
       /* Size in AB:  ( 1.91 , 81.00 ) */

       /* Links to SmartDataBrowser h_bvpifillogg. */
       RUN addLink ( h_dvpifillogg , 'Data':U , h_bvpifillogg ).

       /* Links to SmartObject h_sortsok-2. */
       RUN addLink ( h_bvpifillogg , 'Sortera':U , h_sortsok-2 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_sortsok-2 ,
             h_folder , 'AFTER':U ).
       RUN adjustTabOrder ( h_fvpifilloggfilter ,
             h_sortsok-2 , 'AFTER':U ).
       RUN adjustTabOrder ( h_bvpifillogg ,
             h_fvpifilloggfilter , 'AFTER':U ).
    END. /* Page 3 */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EksporterFil wWin 
PROCEDURE EksporterFil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DEF VAR pcValgteFiler    AS CHAR NO-UNDO.
  DEF VAR piLoop1          AS INT  NO-UNDO.
  DEF VAR pcTekst          AS CHAR NO-UNDO.
  DEF VAR pbOk             AS LOG  NO-UNDO.
  DEF VAR pbMore           AS LOG  NO-UNDO.
  DEF VAR piAntLinjer      AS INT  NO-UNDO.
  DEF VAR pcKeyValues      AS CHAR NO-UNDO.
  DEF VAR piFokusRad       AS INT  NO-UNDO.
  DEF VAR pbVPIFilStatus   AS INT  NO-UNDO.
  DEF VAR pcReturnValue    AS CHAR NO-UNDO.

  /* Henter radnummer på rad som har fokus. */
  RUN GetFocusedRow IN h_bvpifilhode (OUTPUT piFokusRad).

  /* Henter liste med valgte filer i browser. */
  RUN GetSelectedRows IN h_bvpifilhode (OUTPUT pcValgteFiler).

  /* Alle ikke innleste filer */
  IF pcValgteFiler = "" THEN
  DO:
      MESSAGE "Det er ikke valgt fil for eksport."
          VIEW-AS ALERT-BOX INFO BUTTONS OK TITLE "Info".
      RETURN.
  END.
  IF NUM-ENTRIES(pcValgteFiler,CHR(1)) > 1 THEN
  DO:
      MESSAGE "Det kan ikke velges mer enn en fil for eksport."
          VIEW-AS ALERT-BOX INFO BUTTONS OK TITLE "Info".
      RETURN.
  END.
  /* Filer valgt i browser. */
  DO:
      MESSAGE "Der er valgt " + string(NUM-ENTRIES(pcValgteFiler,CHR(1))) + "." SKIP
              "Skal eksport startes?"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Bekreft"
        UPDATE pbOk.
      IF pbOk = FALSE THEN
          RETURN.
  END.

  {sww.i}

  /* Leser X og X filer av gangen, til det er tomt. */
  /* Sikrer at ikke variabel sprekker.              */
  ASSIGN pbMore = TRUE.
  DO WHILE pbMore = TRUE:

      /* Tar vare på den første posten for å reposisjonere */
      ASSIGN
          pbMore      = FALSE
          pcKeyValues = ENTRY(1,pcValgteFiler,CHR(1))
          .

      /* Starter utpakkingsrutine for de filer som skal pakkes ut */
      IF pcValgteFiler <> "" THEN
      FILBEHANDLING:
      DO piLoop1 = 1 TO NUM-ENTRIES(pcValgteFiler,CHR(1)):
         RUN EksporterFil IN h_dvpifilhode (INPUT ENTRY(piLoop1,pcValgteFiler,CHR(1))).
         ASSIGN
             pcReturnValue = RETURN-VALUE
             .
      END. /* FILBEHANDLING */
      ASSIGN
          pcValgteFiler = ""
          .
  END.
  {swn.i}

  IF pcReturnValue <> "" THEN
      MESSAGE pcReturnValue
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
  ELSE
      MESSAGE "Eksport av fil er ferdig."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetAlleIkkeInnleste wWin 
PROCEDURE GetAlleIkkeInnleste :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:      
 
------------------------------------------------------------------------------*/
  DEF INPUT  PARAMETER piAntVPIFilHode AS INT  NO-UNDO.
  DEF OUTPUT PARAMETER pcListe    AS CHAR NO-UNDO.
  DEF OUTPUT PARAMETER pbMore     AS LOG  NO-UNDO.

  ASSIGN
      pbMore = FALSE
      .

  LOOPEN:
  FOR EACH VPIFilHode NO-LOCK WHERE
      VPIFilHode.VPIFilStatus <= 1:
      ASSIGN
          pcListe = pcListe + 
                    (IF pcListe = ""
                       THEN ""
                       ELSE CHR(1)) + 
                    string(VPIFilHode.FilId).
      IF NUM-ENTRIES(pcListe,CHR(1)) > piAntVPIFilHode THEN
      DO:
          ASSIGN
              pbMore = TRUE
              .
          LEAVE LOOPEN.
      END.
  END. /* LOOPEN */

              
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
  RUN initPages ('2,3') NO-ERROR.
  RUN OpprettKnapper.

  SUBSCRIBE TO "Print"             IN h_dyntoolbar.
  SUBSCRIBE TO "ApplHjelp"         IN h_dynToolbar.
  SUBSCRIBE TO "ScannKataloger"    IN h_fvpifilertoolbar.
  SUBSCRIBE TO "LesInn"            IN h_fvpifilertoolbar.
  SUBSCRIBE TO "PakkUt"            IN h_fvpifilertoolbar.
  SUBSCRIBE TO "SlettFil"          IN h_fvpifilertoolbar.
  SUBSCRIBE TO "SlettInneInnleste" IN h_fvpifilertoolbar.
  SUBSCRIBE TO "EksporterFil"      IN h_fvpifilertoolbar.
  SUBSCRIBE TO "OverforPBR"        IN h_fvpifilertoolbar.

  IF VALID-HANDLE(h_dproclib) THEN
      RUN GetLng IN h_dproclib (OUTPUT wCurrLng).

  PUBLISH "Sortera" FROM h_bvpifilhode.
  PUBLISH "Sortera" FROM h_bvpifillinje.
  PUBLISH "Sortera" FROM h_bvpifillogg.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LesInn wWin 
PROCEDURE LesInn :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DEF VAR pcValgteFiler    AS CHAR NO-UNDO.
  DEF VAR piLoop1          AS INT  NO-UNDO.
  DEF VAR pcTekst          AS CHAR NO-UNDO.
  DEF VAR pbOk             AS LOG  NO-UNDO.
  DEF VAR pbMore           AS LOG  NO-UNDO.
  DEF VAR piAntLinjer      AS INT  NO-UNDO.
  DEF VAR pcKeyValues      AS CHAR NO-UNDO.
  DEF VAR piFokusRad       AS INT  NO-UNDO.

  /* Henter radnummer på rad som har fokus. */
  RUN GetFocusedRow IN h_bvpifilhode (OUTPUT piFokusRad).

  /* Henter liste med valgte filer i browser. */
  RUN GetSelectedRows IN h_bvpifilhode (OUTPUT pcValgteFiler).

  /* Alle ikke innleste filer */
  IF pcValgteFiler = "" THEN
  DO:
      pbOk = FALSE.
      MESSAGE "Skal alle ikke innleste filer leses inn?"
          VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Bekreft"
          UPDATE pbOk.
      IF pbOk = FALSE THEN
          RETURN.
  END.
  /* Filer valgt i browser. */
  ELSE DO:
      MESSAGE "Der er valgt " + string(NUM-ENTRIES(pcValgteFiler,CHR(1))) + "." SKIP
              "Skal innlesning startes?"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Bekreft"
        UPDATE pbOk.
      IF pbOk = FALSE THEN
          RETURN.
  END.

  {sww.i}

  /* Leser X og X filer av gangen, til det er tomt. */
  /* Sikrer at ikke variabel sprekker.              */
  ASSIGN pbMore = TRUE.
  DO WHILE pbMore = TRUE:

      IF pcValgteFiler = "" THEN
          RUN GetAlleIkkeInnleste IN h_dvpifilhode (INPUT 100, OUTPUT pcValgteFiler, OUTPUT pbMore).
      ELSE
          pbMore = FALSE.

      /* Tar vare på den første posten for å reposisjonere */
      ASSIGN
          pcKeyValues = ENTRY(1,pcValgteFiler,CHR(1))
          .

      /* Leser inn de valgte filene */
      IF pcValgteFiler <> "" THEN
      DO piLoop1 = 1 TO NUM-ENTRIES(pcValgteFiler,CHR(1)):
          /* Logger fil som leses inn. */
          FIND bufVPIFilHode NO-LOCK WHERE
              bufVPIFilHode.FilId = DEC(ENTRY(piLoop1,pcValgteFiler,CHR(1))) NO-ERROR.
          IF AVAILABLE bufVPIFilHode THEN
              PUBLISH "SkrivTilDataMottaksLogg" (";Leser inn:;" +   
                                                 bufVPIFilHode.FilNavn + " " + 
                                                 string(bufVPIFilHode.Dato) + " " + 
                                                 bufVPIFilHode.Kl + " " + 
                                                 string(bufVPIFilHode.Storrelse) + " " + 
                                                 bufVPIFilHode.Katalog).
        /* Leser inn fil. */
        RUN LesInnFil IN h_dvpifilhode (INPUT ENTRY(piLoop1,pcValgteFiler,CHR(1)), 
                                   OUTPUT pbOk, 
                                   OUTPUT piAntLinjer).
        /* Viser eventuell feilmelding. */
        IF  RETURN-VALUE <> "OK" THEN 
        DO:
            IF RETURN-VALUE <> "" AND 
            NUM-ENTRIES(pcValgteFiler,CHR(1)) = 1 THEN
            MESSAGE 'Fra LesInnFil i dvpifilhode.p: ' + RETURN-VALUE
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
        END.
      END.
      ASSIGN
          pcValgteFiler = ""
          .
  END.
  {swn.i}

  IF pcKeyValues <> "" THEN
  DO:
      DYNAMIC-FUNCTION('openQuery':U IN h_dvpifilhode).
      DYNAMIC-FUNCTION('findRow':U IN h_dvpifilhode,
         INPUT pcKeyValues /* CHARACTER */).
      RUN dataAvailable IN h_dvpifilhode
          ( INPUT "SAME" /* CHARACTER */).
      RUN SetBrowseFocus IN h_bvpifilhode (STRING(piFokusRad)).
  END.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OverforPBR wWin 
PROCEDURE OverforPBR :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN vpiimportpitogregister.w.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PakkUt wWin 
PROCEDURE PakkUt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DEF VAR pcValgteFiler    AS CHAR NO-UNDO.
  DEF VAR piLoop1          AS INT  NO-UNDO.
  DEF VAR pcTekst          AS CHAR NO-UNDO.
  DEF VAR pbOk             AS LOG  NO-UNDO.
  DEF VAR pbMore           AS LOG  NO-UNDO.
  DEF VAR piAntLinjer      AS INT  NO-UNDO.
  DEF VAR pcKeyValues      AS CHAR NO-UNDO.
  DEF VAR piFokusRad       AS INT  NO-UNDO.
  DEF VAR pbVPIFilStatus   AS INT  NO-UNDO.
  DEF VAR piEkstVPILevNr   AS INT  NO-UNDO.

  /* Henter radnummer på rad som har fokus. */
  RUN GetFocusedRow IN h_bvpifilhode (OUTPUT piFokusRad).

  /* Henter liste med valgte filer i browser. */
  RUN GetSelectedRows IN h_bvpifilhode (OUTPUT pcValgteFiler).

  ASSIGN
      piEkstVPILevNr = INT(ENTRY(2,DYNAMIC-FUNCTION('colValues':U IN h_dvpifilhode,
                           INPUT "EkstVPILevNr"),CHR(1)))
      .

  /* Alle ikke innleste filer */
  IF pcValgteFiler = "" THEN
  DO:
      pbOk = FALSE.
      MESSAGE "Skal alle ikke utpakkede filer pakkes ut?"
          VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Bekreft"
          UPDATE pbOk.
      IF pbOk = FALSE THEN
          RETURN.
  END.
  /* Filer valgt i browser. */
  ELSE DO:
      IF NUM-ENTRIES(pcValgteFiler,CHR(1)) > 1 THEN
      DO:
          MESSAGE "Kun en fil kan pakkes ut av gangen."
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN.
      END.
      RUN GetVPIFilStatus IN h_dvpifilhode (INPUT ENTRY(1,pcValgteFiler,CHR(1)),OUTPUT pbVPIFilStatus).
      IF pbVPIFilStatus = 5 THEN
      DO:
          pbOk = FALSE.
          MESSAGE "Filen er allerede pakket ut." SKIP
                  "Skal utpakking kjøres om igjen?"
              VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Bekreft"
              UPDATE pbOk.
          IF pbOk = FALSE THEN
              RETURN.
      END.
      ELSE DO:
          MESSAGE "Der er valgt " + string(NUM-ENTRIES(pcValgteFiler,CHR(1))) + "." SKIP
                  "Skal utpakking startes?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Bekreft"
            UPDATE pbOk.
          IF pbOk = FALSE THEN
              RETURN.
      END.
  END.

  /*{sww.i}*/
  /* Leser X og X filer av gangen, til det er tomt. */
  /* Sikrer at ikke variabel sprekker.              */
  ASSIGN pbMore = TRUE.
  DO WHILE pbMore = TRUE:

      IF pcValgteFiler = "" THEN
          RUN GetAlleIkkeUtpakkede IN h_dvpifilhode (INPUT 100, OUTPUT pcValgteFiler, OUTPUT pbMore).
      ELSE
          pbMore = FALSE.

      /* Tar vare på den første posten for å reposisjonere */
      ASSIGN
          pcKeyValues = ENTRY(1,pcValgteFiler,CHR(1))
          .

      /* Starter utpakkingsrutine for de filer som skal pakkes ut */
      IF pcValgteFiler <> "" THEN
      FILBEHANDLING:
      DO piLoop1 = 1 TO NUM-ENTRIES(pcValgteFiler,CHR(1)):

          /* Logger fil som leses inn. */
          FIND bufVPIFilHode NO-LOCK WHERE
              bufVPIFilHode.FilId = DEC(ENTRY(piLoop1,pcValgteFiler,CHR(1))) NO-ERROR.
          IF AVAILABLE bufVPIFilHode THEN
              PUBLISH "SkrivTilDataMottaksLogg" (";Pakk ut:;" +   
                                                 bufVPIFilHode.FilNavn + " " + 
                                                 string(bufVPIFilHode.Dato) + " " + 
                                                 bufVPIFilHode.Kl + " " + 
                                                 string(bufVPIFilHode.Storrelse) + " " + 
                                                 bufVPIFilHode.Katalog).

          RUN GetVPIFilStatus IN h_dvpifilhode (INPUT ENTRY(piLoop1,pcValgteFiler,CHR(1)),OUTPUT pbVPIFilStatus).
          IF pbVPIFilStatus >= 3 AND 
             pbVPIFilStatus <= 5 THEN
              RUN PakkUtFil IN h_dvpifilhode (INPUT ENTRY(piLoop1,pcValgteFiler,CHR(1))).
      END. /* FILBEHANDLING */
      ASSIGN
          pcValgteFiler = ""
          .
  END.

  {swn.i}

  MESSAGE "Utpakking av filer ferdig."
      VIEW-AS ALERT-BOX INFO BUTTONS OK.

  IF pcKeyValues <> "" THEN
  DO:
      DYNAMIC-FUNCTION('openQuery':U IN h_dvpifilhode).
      DYNAMIC-FUNCTION('findRow':U IN h_dvpifilhode,
         INPUT pcKeyValues /* CHARACTER */).
      RUN dataAvailable IN h_dvpifilhode
          ( INPUT "SAME" /* CHARACTER */).
      RUN SetBrowseFocus IN h_bvpifilhode (STRING(piFokusRad)).
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RensReturn wWin 
PROCEDURE RensReturn :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 RETURN "".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ScannKataloger wWin 
PROCEDURE ScannKataloger :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {sww.i}

  /* 1. Bygg en liste over alle kataloger som skal kontrolleres. */
  RUN GetKatalogListe IN h_dvpifilhode (OUTPUT cKataloger).

  /* 2. Bygg en liste med alle filnavn + ekstent som skal kontrolleres. */
  RUN GetFilNavnListe IN h_dvpifilhode (OUTPUT cFilNavnListe).
  
  /* 3. Opprett en post i fillisten for alle filer som ikke finnes der. */
  RUN OpprettPoster IN h_dvpifilhode (INPUT cKataloger, INPUT cFilNavnListe). 

 /* 5. OpenQuery for fillisten. */ 
  DYNAMIC-FUNCTION('openQuery':U IN h_dvpifilhode).
  RUN SetBrowseFocus IN h_bvpifilhode (?).
  {swn.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettFil wWin 
PROCEDURE SlettFil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DEF VAR pcValgteFiler    AS CHAR NO-UNDO.
  DEF VAR piLoop1          AS INT  NO-UNDO.
  DEF VAR pcTekst          AS CHAR NO-UNDO.
  DEF VAR pbOk             AS LOG  NO-UNDO.
  DEF VAR pbMore           AS LOG  NO-UNDO.
  DEF VAR piAntLinjer      AS INT  NO-UNDO.
  DEF VAR pcKeyValues      AS CHAR NO-UNDO.
  DEF VAR piFokusRad       AS INT  NO-UNDO.

  /* Henter radnummer på rad som har fokus. */
  RUN GetFocusedRow IN h_bvpifilhode (OUTPUT piFokusRad).

  /* Henter liste med valgte filer i browser. */
  RUN GetSelectedRows IN h_bvpifilhode (OUTPUT pcValgteFiler).

  IF pcValgteFiler = "" THEN
  DO:
      MESSAGE "Det må velges en fil for sletting."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN.
  END.
/*   IF NUM-ENTRIES(pcValgteFiler,CHR(1)) > 1 THEN          */
/*   DO:                                                    */
/*       MESSAGE "Det kan bare velges en fil for sletting." */
/*           VIEW-AS ALERT-BOX INFO BUTTONS OK.             */
/*       RETURN.                                            */
/*   END.                                                   */
  /* Filer valgt i browser. */
  DO:
      MESSAGE "Der er valgt " + string(NUM-ENTRIES(pcValgteFiler,CHR(1))) + "." SKIP
              "Skal sletting startes?"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Bekreft"
        UPDATE pbOk.
      IF pbOk = FALSE THEN
          RETURN.
  END.

  {sww.i}

  /* Leser X og X filer av gangen, til det er tomt. */
  /* Sikrer at ikke variabel sprekker.              */
  ASSIGN pbMore = TRUE.
  DO WHILE pbMore = TRUE:

      /* Tar vare på den første posten for å reposisjonere */
      ASSIGN
          pbMore      = FALSE
          pcKeyValues = ENTRY(1,pcValgteFiler,CHR(1))
          .

      /* Leser inn de valgte filene */
      IF pcValgteFiler <> "" THEN
      DO piLoop1 = 1 TO NUM-ENTRIES(pcValgteFiler,CHR(1)):
        /* Leser inn fil. */
        RUN SlettFil IN h_dvpifilhode (INPUT ENTRY(piLoop1,pcValgteFiler,CHR(1)), 
                                       INPUT YES,  
                                       OUTPUT piAntLinjer).
        /* Viser eventuell feilmelding. */
        IF RETURN-VALUE <> "" THEN
            MESSAGE RETURN-VALUE
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
      END.
      ASSIGN
          pcValgteFiler = ""
          .
  END.
  {swn.i}

  MESSAGE "Sletting av fil ferdig."
      VIEW-AS ALERT-BOX INFO BUTTONS OK.

  IF pcKeyValues <> "" THEN
  DO:
      DYNAMIC-FUNCTION('openQuery':U IN h_dvpifilhode).
      RUN SetBrowseFocus IN h_bvpifilhode (STRING(piFokusRad)).
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettInneInnleste wWin 
PROCEDURE SlettInneInnleste :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR pbOk AS LOG NO-UNDO.

  pbOk = FALSE.
  MESSAGE "Skal alle ikke innleste filer slettes?"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Bekreft"
      UPDATE pbOk.
  IF pbOk = FALSE THEN
      RETURN.

  RUN RensTommePoster IN h_dvpifilhode.
  DYNAMIC-FUNCTION('openQuery':U IN h_dvpifilhode).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

