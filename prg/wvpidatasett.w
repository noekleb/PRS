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
DEF VAR h_vartkor     AS HANDLE NO-UNDO.
DEF VAR hField1       AS HANDLE NO-UNDO.
DEF VAR c02SjekkListe AS CHAR NO-UNDO.
DEF VAR h_PrisKo      AS HANDLE NO-UNDO.

DEF TEMP-TABLE tmpVare NO-UNDO
    FIELD VareNr AS CHAR
    INDEX Vare VareNr.

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


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fLockvindu wWin 
FUNCTION fLockvindu RETURNS CHARACTER
  ( INPUT lLock AS LOGICAL )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_bvpiartbas AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bvpidatasett AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dvpiartbas AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dvpidatasett AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_fvpiartbas AS HANDLE NO-UNDO.
DEFINE VARIABLE h_fvpiartbas2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_fvpiartbastoolbar AS HANDLE NO-UNDO.
DEFINE VARIABLE h_sortsok AS HANDLE NO-UNDO.
DEFINE VARIABLE h_sortsok-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vvpiartbas2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vvpidatasett AS HANDLE NO-UNDO.

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 161.2 BY 26.91.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Design Page: 3
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "VPI register"
         HEIGHT             = 26.91
         WIDTH              = 161.2
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
   Custom                                                               */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* VPI register */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* VPI register */
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

IF VALID-HANDLE(h_PrisKo) THEN
  DELETE PROCEDURE h_PrisKo.

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
             INPUT  'sdo/dvpidatasett.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedvpidatasettUpdateFromSourcenoToggleDataTargetsyesOpenOnInityesPromptOnDeleteyesPromptColumns(NONE)':U ,
             OUTPUT h_dvpidatasett ).
       RUN repositionObject IN h_dvpidatasett ( 1.24 , 150.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'sdo/dvpiartbas.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsVPIArtBas.EkstVPILevNr,EkstVPILevNrRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedvpiartbasUpdateFromSourcenoToggleDataTargetsyesOpenOnInityesPromptOnDeleteyesPromptColumns(NONE)':U ,
             OUTPUT h_dvpiartbas ).
       RUN repositionObject IN h_dvpiartbas ( 1.48 , 139.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsNavigationSubModulesTableIOTypeSaveSupportedLinksNavigation-sourceToolbarBandsToolbarParentMenuToolbarAutoSizenoToolbarDrawDirectionhorizontalToolbarInitialStateLogicalObjectNameAutoResizeDisabledActionsHiddenActionsUpdateHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0EdgePixels2PanelTypeToolbarDeactivateTargetOnHidenoDisabledActionsNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar ).
       RUN repositionObject IN h_dyntoolbar ( 1.00 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar ( 1.24 , 161.20 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/folder.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FolderLabels':U + 'VPI leverandør|Artikkelliste|Artikkelinformasjon' + 'FolderTabWidth0FolderFont-1HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_folder ).
       RUN repositionObject IN h_folder ( 2.43 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_folder ( 25.48 , 161.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN initPages ('3,2') NO-ERROR.

       /* Links to SmartDataObject h_dvpidatasett. */
       RUN addLink ( h_dyntoolbar , 'Navigation':U , h_dvpidatasett ).

       /* Links to SmartDataObject h_dvpiartbas. */
       RUN addLink ( h_dvpidatasett , 'Data':U , h_dvpiartbas ).
       RUN addLink ( h_dyntoolbar-2 , 'Navigation':U , h_dvpiartbas ).
       RUN addLink ( h_fvpiartbas , 'SokSdo':U , h_dvpiartbas ).

       /* Links to SmartFolder h_folder. */
       RUN addLink ( h_folder , 'Page':U , THIS-PROCEDURE ).

    END. /* Page 0 */

    WHEN 1 THEN DO:
       RUN constructObject (
             INPUT  'prg/bvpidatasett.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bvpidatasett ).
       RUN repositionObject IN h_bvpidatasett ( 5.76 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_bvpidatasett ( 21.91 , 122.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'prg/sortsok.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_sortsok ).
       RUN repositionObject IN h_sortsok ( 3.86 , 2.00 ) NO-ERROR.
       /* Size in AB:  ( 1.76 , 27.60 ) */

       /* Links to SmartDataBrowser h_bvpidatasett. */
       RUN addLink ( h_dvpidatasett , 'Data':U , h_bvpidatasett ).
       RUN addLink ( h_bvpidatasett , 'Update':U , h_dvpidatasett ).

       /* Links to SmartObject h_sortsok. */
       RUN addLink ( h_bvpidatasett , 'Sortera':U , h_sortsok ).

    END. /* Page 1 */

    WHEN 2 THEN DO:
       RUN constructObject (
             INPUT  'prg/bvpiartbas.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bvpiartbas ).
       RUN repositionObject IN h_bvpiartbas ( 11.95 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_bvpiartbas ( 15.48 , 138.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'prg/vvpidatasett.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EnabledObjFldsToDisable(None)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vvpidatasett ).
       RUN repositionObject IN h_vvpidatasett ( 3.86 , 2.00 ) NO-ERROR.
       /* Size in AB:  ( 4.05 , 40.00 ) */

       RUN constructObject (
             INPUT  'prg/fvpiartbastoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'LogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_fvpiartbastoolbar ).
       RUN repositionObject IN h_fvpiartbastoolbar ( 11.95 , 141.00 ) NO-ERROR.
       /* Size in AB:  ( 15.91 , 20.00 ) */

       RUN constructObject (
             INPUT  'prg/fvpiartbas.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'LogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_fvpiartbas ).
       RUN repositionObject IN h_fvpiartbas ( 3.86 , 43.00 ) NO-ERROR.
       /* Size in AB:  ( 7.71 , 97.20 ) */

       RUN constructObject (
             INPUT  'prg/sortsok.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_sortsok-2 ).
       RUN repositionObject IN h_sortsok-2 ( 9.57 , 2.00 ) NO-ERROR.
       /* Size in AB:  ( 1.76 , 27.60 ) */

       /* Links to SmartDataBrowser h_bvpiartbas. */
       RUN addLink ( h_dvpiartbas , 'Data':U , h_bvpiartbas ).
       RUN addLink ( h_bvpiartbas , 'Update':U , h_dvpiartbas ).

       /* Links to SmartDataViewer h_vvpidatasett. */
       RUN addLink ( h_dvpidatasett , 'Data':U , h_vvpidatasett ).

       /* Links to SmartFrame h_fvpiartbastoolbar. */
       RUN addLink ( h_folder , 'Page':U , h_fvpiartbastoolbar ).

       /* Links to SmartObject h_sortsok-2. */
       RUN addLink ( h_bvpiartbas , 'Sortera':U , h_sortsok-2 ).

    END. /* Page 2 */

    WHEN 3 THEN DO:
       RUN constructObject (
             INPUT  'prg/vvpiartbas2.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EnabledObjFldsToDisable(None)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vvpiartbas2 ).
       RUN repositionObject IN h_vvpiartbas2 ( 3.62 , 2.00 ) NO-ERROR.
       /* Size in AB:  ( 1.33 , 138.00 ) */

       RUN constructObject (
             INPUT  'prg/fvpiartbas2.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'LogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_fvpiartbas2 ).
       RUN repositionObject IN h_fvpiartbas2 ( 5.05 , 2.00 ) NO-ERROR.
       /* Size in AB:  ( 22.24 , 159.40 ) */

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsNavigationSubModulesTableIOTypeSaveSupportedLinksNavigation-sourceToolbarBandsToolbarParentMenuToolbarAutoSizenoToolbarDrawDirectionhorizontalToolbarInitialStateLogicalObjectNameAutoResizeDisabledActionsHiddenActionsUpdateHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0EdgePixels2PanelTypeToolbarDeactivateTargetOnHidenoDisabledActionsNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar-2 ).
       RUN repositionObject IN h_dyntoolbar-2 ( 3.62 , 140.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar-2 ( 1.24 , 21.20 ) NO-ERROR.

       /* Links to SmartDataViewer h_vvpiartbas2. */
       RUN addLink ( h_dvpiartbas , 'Data':U , h_vvpiartbas2 ).

       /* Links to SmartFrame h_fvpiartbas2. */
       RUN addLink ( h_dvpiartbas , 'Data':U , h_fvpiartbas2 ).

       /* Adjust the tab order of the smart objects. */
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Bygg02SjekkListe wWin 
PROCEDURE Bygg02SjekkListe :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  ASSIGN
      c02SjekkListe = ""
      .
  FOR EACH Butiker NO-LOCK:
       ASSIGN
           c02SjekkListe = c02SjekkListe + 
                           (IF c02SjekkListe = ""
                              THEN ""
                              ELSE ",") +
                           "02" + trim(STRING(Butiker.Butik,">>>999"))
           .
   END.
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

  /* Code placed here will execute PRIOR to standard behavior. */
  DEFINE VARIABLE currentPage  AS INTEGER NO-UNDO.

  ASSIGN 
      currentPage = getCurrentPage().

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
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
  IF VALID-HANDLE(h_PrisKo) THEN
    DELETE PROCEDURE h_PrisKo.

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
  IF NOT VALID-HANDLE(h_PrisKo) THEN
      RUN prisko.p PERSISTENT SET h_PrisKo.

  RUN SjekkHeader IN h_dvpidatasett.

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  DEF VAR pcolValues     AS CHAR NO-UNDO.
  DEF VAR pcTekst        AS CHAR NO-UNDO.
  DEF VAR piEkstVPILevNr AS INT  NO-UNDO.
  DEF VAR bT-Modell     AS LOG  NO-UNDO.

  RUN initPages
    ( INPUT "2" /* CHARACTER */).
  RUN OpprettKnapper.

  SUBSCRIBE TO "Print"             IN h_dyntoolbar.
  SUBSCRIBE TO "ApplHjelp"         IN h_dynToolbar.
  SUBSCRIBE TO "Koble"             IN h_fvpiartbastoolbar.
  SUBSCRIBE TO "Pris"              IN h_fvpiartbastoolbar.
  SUBSCRIBE TO "OpprettUtvalg"     IN h_fvpiartbastoolbar.
  SUBSCRIBE TO "Ny"                IN h_fvpiartbastoolbar.
  SUBSCRIBE TO "Opphev"            IN h_fvpiartbastoolbar.
  SUBSCRIBE TO "Slett"             IN h_fvpiartbastoolbar.
  SUBSCRIBE TO "SettOppdat"        IN h_fvpiartbastoolbar.
  SUBSCRIBE TO "VisArtikkel"       IN h_fvpiartbastoolbar.
  SUBSCRIBE TO "SjekkKobling"      IN h_fvpiartbastoolbar.
  SUBSCRIBE TO "OverforTilHKVPI"   IN h_fvpiartbastoolbar.
  
  PUBLISH "Sortera" FROM h_bvpidatasett.
  PUBLISH "Sortera" FROM h_bvpiartbas.

  SUBSCRIBE PROCEDURE h_fvpiartbastoolbar TO "EnableKobleEan"       IN h_vvpidatasett.
  SUBSCRIBE PROCEDURE h_fvpiartbastoolbar TO "DisableKobleEan"      IN h_vvpidatasett.
  SUBSCRIBE PROCEDURE h_fvpiartbastoolbar TO "EnableOpprettUtvalg"  IN h_vvpidatasett.
  SUBSCRIBE PROCEDURE h_fvpiartbastoolbar TO "DisableOpprettUtvalg" IN h_vvpidatasett.

  ASSIGN
      piEkstVPILevNr = INT(entry(2,DYNAMIC-FUNCTION('colValues':U IN h_dvpidatasett,
                       INPUT "EkstVPILevNr"),CHR(1)))
      .
  /* Leverandører som det ikke er lagt opp parameter for, har slik håndtering. */
  /* Lev. som har "nei, no, false , 0" har ikke.                               */
  {syspara.i 1 13 piEkstVPILevNr pcTekst}
  IF can-do("1,Nei,no,false,N", pcTekst) THEN
      bT-Modell = FALSE.
  ELSE 
      bT-Modell = TRUE.

  IF bT-Modell = TRUE THEN
  DO:
      RUN DisableKobleEan      IN h_fvpiartbastoolbar.
      RUN EnableOpprettUtvalg  IN h_fvpiartbastoolbar.
  END.
  ELSE DO:
      RUN EnableKobleEan       IN h_fvpiartbastoolbar.
      RUN DisableOpprettUtvalg IN h_fvpiartbastoolbar.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Koble wWin 
PROCEDURE Koble :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DEF VAR pcValgteArtikler AS CHAR NO-UNDO.
  DEF VAR piLoop1          AS INT  NO-UNDO.
  DEF VAR pcTekst          AS CHAR NO-UNDO.
  DEF VAR pbOk             AS LOG  NO-UNDO.
  DEF VAR pbMore           AS LOG  NO-UNDO.
  DEF VAR piAntLinjer      AS INT  NO-UNDO.
  DEF VAR pcKeyValues      AS char NO-UNDO.
  DEF VAR piFokusRad       AS INT  NO-UNDO.
  DEF VAR piEkstVpiLevNr   AS INT  NO-UNDO.
  DEF VAR pcArtikkelNr     AS CHAR NO-UNDO.

  ASSIGN
      piEkstVpiLevNr = INT(DYNAMIC-FUNCTION('getForeignValues':U IN h_dvpiartbas))
      pcArtikkelNr   = DYNAMIC-FUNCTION('colValues':U IN h_dvpiartbas,
                                         INPUT "ArtikkelNr")
      .

  /* Henter radnummer på rad som har fokus. */
  RUN GetFocusedRow IN h_bvpiartbas (OUTPUT piFokusRad).

  /* Henter liste med valgte filer i browser. */
  RUN GetSelectedRows IN h_bvpiartbas (OUTPUT pcValgteArtikler).

  /* Ikke lov å velge INGEN */
  IF pcValgteArtikler = "" THEN
  DO:
      MESSAGE "Ingen artikler valgt for oppdatering."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN.
  END.

  /* Artikler valgt i browser. */
  MESSAGE "Der er valgt " + string(num-entries(pcValgteArtikler,CHR(1))) + " VPI poster." SKIP
          "Skal kobling startes?"
    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Bekreft"
    UPDATE pbOk.
  IF pbOk = FALSE THEN
      RETURN.

  /* Tar vare på den første posten for å reposisjonere i browser etter oppdatering. */
  ASSIGN
      pcKeyValues = string(piEkstVPILevNr) + chr(1) + 
                    ENTRY(1,pcValgteArtikler,CHR(1))
      .
  /* Oppdaterer valgte artikler.                                                   */
  /* Oppdateringsrutinen henter informasjonen fra første artikkel i listen. Fra de */
  /* øvrige artiklene hentes kun EAN koden.                                        */
  /* Oppdatering gjøres mot en eksisterende lokal artikkel, eller det opprettes en */
  /* ny lokal artikkel.                                                            */
  RUN OppdaterArtikkel (INPUT  piEkstVPILevNr,
                        INPUT  pcValgteArtikler, 
                        INPUT  2, /* Koble */
                        OUTPUT pbOk).
  /* Viser eventuell feilmelding. */
  IF RETURN-VALUE = "AVBRYT" THEN
      RETURN "".
  ELSE IF RETURN-VALUE <> "" THEN
      MESSAGE RETURN-VALUE
          VIEW-AS ALERT-BOX INFO BUTTONS OK.

  IF pcKeyValues <> "" THEN
  DO:
      DYNAMIC-FUNCTION('openQuery':U IN h_dvpiartbas).
      DYNAMIC-FUNCTION('findRow':U IN h_dvpiartbas,
         INPUT pcKeyValues /* CHARACTER */).
      RUN dataAvailable IN h_dvpiartbas
          ( INPUT "SAME" /* CHARACTER */).
      RUN SetBrowseFocus IN h_bvpiartbas (pcValgteArtikler,
                                          piEkstVpiLevNr).
  END.

  /* Henter opp artikkel for redigering. */
  RUN Vis2Artikkel (INPUT piEkstVpiLevNr, INPUT ENTRY(1,pcValgteArtikler,CHR(1))).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Ny wWin 
PROCEDURE Ny :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DEF VAR pcValgteArtikler AS CHAR NO-UNDO.
  DEF VAR piLoop1          AS INT  NO-UNDO.
  DEF VAR pcTekst          AS CHAR NO-UNDO.
  DEF VAR pbOk             AS LOG  NO-UNDO.
  DEF VAR pbMore           AS LOG  NO-UNDO.
  DEF VAR piAntLinjer      AS INT  NO-UNDO.
  DEF VAR pcKeyValues      AS char NO-UNDO.
  DEF VAR piFokusRad       AS INT  NO-UNDO.
  DEF VAR piEkstVpiLevNr   AS INT  NO-UNDO.
  DEF VAR pcArtikkelNr     AS CHAR NO-UNDO.
  DEF VAR pbT-Modell       AS LOG  NO-UNDO.

  ASSIGN
      piEkstVpiLevNr = INT(DYNAMIC-FUNCTION('getForeignValues':U IN h_dvpiartbas))
      pcArtikkelNr   = DYNAMIC-FUNCTION('colValues':U IN h_dvpiartbas,
                                         INPUT "ArtikkelNr")
      .
  /* Leverandører som det ikke er lagt opp parameter for, har slik håndtering. */
  /* Lev. som har "nei, no, false , 0" har ikke.                               */
  {syspara.i 1 13 piEkstVPILevNr pcTekst}
  IF can-do("1,Nei,no,false,N", pcTekst) THEN
      pbT-Modell = FALSE.
  ELSE 
      pbT-Modell = TRUE.

  /* Henter radnummer på rad som har fokus. */
  RUN GetFocusedRow IN h_bvpiartbas (OUTPUT piFokusRad).

  /* Henter liste med valgte filer i browser. */
  RUN GetSelectedRows IN h_bvpiartbas (OUTPUT pcValgteArtikler).

  /* Ikke lov å velge INGEN */
  IF pcValgteArtikler = "" THEN
  DO:
      MESSAGE "Ingen artikler valgt for oppdatering."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN.
  END.
  /* For VPI lev som leverer komplette artikkler, kan bare en post velges. */
  IF pbT-Modell = TRUE AND num-entries(pcValgteArtikler,CHR(1)) > 1 THEN
  DO:
      MESSAGE "Denne VPI leverandøren leverer komplett artikkelinformasjon." SKIP
              "Det kan derfor bare velges en og en artikkel ad gangen." SKIP
              "Med artikkel kommer alle strekkoder, størrelser m.m."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN.
  END.

  /* Artikler valgt i browser. */
  MESSAGE "Der er valgt " + string(num-entries(pcValgteArtikler,CHR(1))) + " VPI poster." SKIP
          "Skal kobling startes?"
    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Bekreft"
    UPDATE pbOk.
  IF pbOk = FALSE THEN
      RETURN.

  /* Tar vare på den første posten for å reposisjonere i browser etter oppdatering. */
  ASSIGN
      pcKeyValues = string(piEkstVPILevNr) + chr(1) + 
                    ENTRY(1,pcValgteArtikler,CHR(1))
      .
  /* Oppdaterer valgte artikler.                                                   */
  /* Oppdateringsrutinen henter informasjonen fra første artikkel i listen. Fra de */
  /* øvrige artiklene hentes kun EAN koden.                                        */
  /* Oppdatering gjøres mot en eksisterende lokal artikkel, eller det opprettes en */
  /* ny lokal artikkel.                                                            */
  RUN OppdaterArtikkel (INPUT  piEkstVPILevNr,
                        INPUT  pcValgteArtikler, 
                        INPUT  1, /* Ny */
                        OUTPUT pbOk).
  /* Viser eventuell feilmelding. */
  IF RETURN-VALUE = "AVBRYT" THEN
      RETURN "".
  ELSE IF RETURN-VALUE <> "" THEN
      MESSAGE RETURN-VALUE
          VIEW-AS ALERT-BOX INFO BUTTONS OK.

  IF pcKeyValues <> "" THEN
  DO:
      DYNAMIC-FUNCTION('openQuery':U IN h_dvpiartbas).
      DYNAMIC-FUNCTION('findRow':U IN h_dvpiartbas,
         INPUT pcKeyValues /* CHARACTER */).
      RUN dataAvailable IN h_dvpiartbas
          ( INPUT "SAME" /* CHARACTER */).
      RUN SetBrowseFocus IN h_bvpiartbas (pcValgteArtikler,
                                          piEkstVpiLevNr).
  END.

  /* Henter opp artikkel for redigering. */
  RUN Vis2Artikkel (INPUT piEkstVpiLevNr, INPUT entry(1,pcValgteArtikler,CHR(1))).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppdaterArtikkel wWin 
PROCEDURE OppdaterArtikkel PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  /* Oppdaterer valgte artikler.                                                   */
  /* Oppdateringsrutinen henter informasjonen fra første artikkel i listen. Fra de */
  /* øvrige artiklene hentes kun EAN koden.                                        */
  /* Oppdatering gjøres mot en eksisterende lokal artikkel, eller det opprettes en */
  /* ny lokal artikkel.                                                            */
    RUN OppdaterArtikkel (INPUT  piEkstVPILevNr,
                          INPUT  pcValgteArtikler, 
                          OUTPUT pbOk).

------------------------------------------------------------------------------*/
  DEF INPUT  PARAMETER piEkstVpiLevNr AS INT  NO-UNDO.
  DEF INPUT  PARAMETER pcListe        AS CHAR NO-UNDO. /* CHR(1) */
  DEF INPUT  PARAMETER piModus        AS INT  NO-UNDO. /* 1-Ny, 2-Koble */
  DEF OUTPUT PARAMETER pbOk           AS LOG  NO-UNDO.

  DEF VAR pcVareNr     AS CHAR NO-UNDO.
  DEF VAR plArtikkelNr AS DEC  NO-UNDO.
  DEF VAR pcColValues  AS CHAR NO-UNDO.
  DEF VAR piLoop       AS INT  NO-UNDO.
  DEF VAR piLoop2      AS INT  NO-UNDO.
  DEF VAR pbSjekk      AS LOG  NO-UNDO.

  ASSIGN
      pcVareNr = ENTRY(1,pcListe,CHR(1))
      .
  /* For ny og koble, skal ikke VPI posten være koblet. */
  IF CAN-DO("1,2",STRING(piModus)) THEN
  SJEKK-KOBLING:
  DO piLoop = 1 TO NUM-ENTRIES(pcListe,CHR(1)):
    /* Returneres 0 i plArtikkelNr, er artikkelen ikke koblet. */
    RUN GetArtikkelNr IN h_dvpiartbas (piEkstVpiLevNr, ENTRY(piLoop,pcListe,CHR(1)), OUTPUT plArtikkelNr).

    /* Dobbelsjekk på at strekkoden ikke allerede finnes lokalt. */
    /* Registrering av strekkoder i lokalt register, oppdaterer  */
    /* ikke vpi registeret. Derfor denne ekstra sjekken.         */
    FIND Strekkode NO-LOCK WHERE
       Strekkode.Kode = TRIM(ENTRY(piLoop,pcListe,CHR(1))) NO-ERROR.
    IF AVAILABLE Strekkode THEN
    DO:
      ASSIGN
        plArtikkelNr = Strekkode.ArtikkelNr
        .
    END.

/*     /* Artikkelen er allerede koblet */                                      */
/*     IF plArtikkelNr <> 0 and                                                 */
/*         can-find(ArtBas where                                                */
/*                 ArtBas.ArtikkelNr = plArtikkelNr) THEN                       */
/*     DO:                                                                      */
/*         MESSAGE "En eller flere av VPI postene er allerede koblet." SKIP     */
/*                  "Ingen av VPI postene som velges, kan være koblet fra før." */
/*               VIEW-AS ALERT-BOX INFO BUTTONS OK.                             */
/*         RETURN "AVBRYT".                                                     */
/*     END.                                                                     */

    /* Er lengden på 13 siffer og de to første er 02, skal koden forkastes. */
    IF LENGTH(ENTRY(piLoop,pcListe,CHR(1))) = 13 AND
       SUBSTRING(ENTRY(piLoop,pcListe,CHR(1)),1,2) = "02" AND 
       c02SjekkListe = "" THEN
    LENGDE-02:
    DO:
        /* Er det bare valgt en post, kan det tillates, men da kan bare en */
        /* post være valgt.                                                */
        IF NUM-ENTRIES(pcListe,CHR(1)) = 1 AND piModus = 1 THEN
        DO:
          MESSAGE "Varen har en bedriftsintern EAN kode. Den vil få tildelt ny EAN kode i lokalt artikkelregister."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
          ASSIGN
            piModus = 1   /* Slår om til ny vare. */
            .
          LEAVE SJEKK-KOBLING.
        END.

        MESSAGE "En eller flere av VPI postene inneholder bedriftsinterne EAN koder." SKIP
                "Slike koder kan ikke hentes inn fra SportKat." SKIP(1)
                "Varer med slike EAN koder kan hentes inn, men da kan bare en vare velges ad gangen." SKIP
                "Det må velges: Opprett ny Vare."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN "AVBRYT".
    END. /* LENGDE-02 */
    ELSE
    LENGDE-03:
    DO:
        ASSIGN
            pbSjekk = FALSE
            .
        IF c02SjekkListe <> "" THEN
        DO piLoop2 = 1 TO NUM-ENTRIES(c02SjekkListe):
            IF ENTRY(piLoop,pcListe,CHR(1)) BEGINS ENTRY(piLoop2,c02SjekkListe) THEN
                pbSjekk = TRUE.
        END.
        IF pbSjekk THEN
        DO:
            MESSAGE "En eller flere av VPI postene inneholder bedriftsinterne EAN koder." SKIP
                    "Slike koder kan ikke hentes inn." SKIP(1)
                    "Varer med slike EAN koder kan hentes inn fra SportKat, men da kan bare en vare velges ad gangen." SKIP
                    "Det må velges: Opprett ny Vare."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RETURN "AVBRYT".
        END.
    END. /* LENGDE-03 */
  END. /* SJEKK-KOBLING */

  /* Kobling.                                             */
  /* Bruker kan koble eller velge å opprette ny artikkel. */
  IF piModus = 2 THEN
  KOBLE:
  DO: 
      /* Er artikkelen ikke koblet, skal den kobles.                             */
      /* Kobling kan gjøres mot en eksisterende artikkel eller det kan opprettes */
      /* en ny.                                                                  */
      RUN gartbassok.w (INPUT-OUTPUT pcColValues, "", "" , "KOBLE"). 
      IF RETURN-VALUE = "AVBRYT" THEN
          RETURN "AVBRYT".
      /* Ny artikkel skal opprettes */
      ELSE IF RETURN-VALUE = "NY" THEN
      DO:
          RUN OpprettNy    IN h_dvpiartbas (piEkstVpiLevNr, pcListe, OUTPUT plArtikkelNr).
          IF RETURN-VALUE <> "" THEN
          DO:
              MESSAGE RETURN-VALUE
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
              RETURN.
          END.
          RUN OppdaterInfo IN h_dvpiartbas (piEkstVpiLevNr, pcListe, plArtikkelNr).
      END.
      /* Eksisterende artikkel oppdateres med ny informasjon. */
      ELSE DO:
          ASSIGN
              plArtikkelNr = DEC(entry(2,pcColValues,CHR(1)))
              .
          RUN OppdaterInfo IN h_dvpiartbas (piEkstVpiLevNr, pcListe, plArtikkelNr).
      END.
  END. /* KOBLE */
  ELSE IF piModus = 3 THEN
  PRIS:
  DO:
       RUN GetArtikkelNr IN h_dvpiartbas (piEkstVpiLevNr, ENTRY(1,pcListe,CHR(1)), OUTPUT plArtikkelNr).
       RUN OppdaterPris IN h_dvpiartbas (piEkstVpiLevNr, pcListe, plArtikkelNr).
  END. /* PRIS */
  /* Ny artikkel opprettes. */
  ELSE 
  OPPRETTNY:
  DO:
      RUN SettAutoImport IN h_dvpiartbas (INPUT TRUE).
      RUN OpprettNy    IN h_dvpiartbas (piEkstVpiLevNr, pcListe, OUTPUT plArtikkelNr).
      IF RETURN-VALUE <> "" THEN
      DO:
          MESSAGE RETURN-VALUE
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN.
      END.
      RUN OppdaterInfo IN h_dvpiartbas (piEkstVpiLevNr, pcListe, plArtikkelNr).
  END. /* OPPRETTNY */


  RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpphevArtikkel wWin 
PROCEDURE OpphevArtikkel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  /* Oppdaterer valgte artikler.                                                   */
  /* Oppdateringsrutinen henter informasjonen fra første artikkel i listen. Fra de */
  /* øvrige artiklene hentes kun EAN koden.                                        */
  /* Oppdatering gjøres mot en eksisterende lokal artikkel, eller det opprettes en */
  /* ny lokal artikkel.                                                            */
    RUN OppdaterArtikkel (INPUT  piEkstVPILevNr,
                          INPUT  pcValgteArtikler, 
                          OUTPUT pbOk).

------------------------------------------------------------------------------*/
  DEF INPUT  PARAMETER piEkstVpiLevNr AS INT  NO-UNDO.
  DEF INPUT  PARAMETER pcListe        AS CHAR NO-UNDO. /* CHR(1) */
  DEF OUTPUT PARAMETER pbOk           AS LOG  NO-UNDO.

  DEF VAR pcVareNr     AS CHAR NO-UNDO.
  DEF VAR plArtikkelNr AS DEC  NO-UNDO.
  DEF VAR pcColValues  AS CHAR NO-UNDO.

  ASSIGN
      pcVareNr = ENTRY(1,pcListe,CHR(1))
      .

  /* Returneres 0 i plArtikkelNr, er artikkelen ikke koblet. */
  RUN GetArtikkelNr IN h_dvpiartbas (piEkstVpiLevNr, pcVareNr, OUTPUT plArtikkelNr).

  /* Artikkelen er ikke koblet fra før */
  IF plArtikkelNr = 0 THEN
  DO:
      MESSAGE "VPI er ikke koblet."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN.
  END.

  /* Kobling oppheves */
  RUN OpphevKobling IN h_dvpiartbas (piEkstVpiLevNr, pcListe, plArtikkelNr).

  RETURN.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpprettUtvalg wWin 
PROCEDURE OpprettUtvalg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DEF VAR pcValgteArtikler AS CHAR NO-UNDO.
  DEF VAR piLoop1          AS INT  NO-UNDO.
  DEF VAR pcTekst          AS CHAR NO-UNDO.
  DEF VAR pbOk             AS LOG  NO-UNDO.
  DEF VAR pbMore           AS LOG  NO-UNDO.
  DEF VAR piAntLinjer      AS INT  NO-UNDO.
  DEF VAR pcKeyValues      AS char NO-UNDO.
  DEF VAR piFokusRad       AS INT  NO-UNDO.
  DEF VAR piEkstVpiLevNr   AS INT  NO-UNDO.
  DEF VAR pcArtikkelNr     AS CHAR NO-UNDO.
  DEF VAR pbT-Modell       AS LOG  NO-UNDO.
  DEF VAR pcQueryWhere     AS CHAR NO-UNDO.
  DEF VAR plArtikkelNr     AS DEC  NO-UNDO.
  
  DEF VAR hBuffer          AS HANDLE NO-UNDO.
  DEF VAR hQuery           AS HANDLE NO-UNDO.

  DEFINE BUFFER bVPIArtBas FOR VPIArtBas.


  /* Tømmer tmp-file */
  FOR EACH tmpVare:
      DELETE tmpVare.
  END.

  RUN Bygg02SjekkListe.

  ASSIGN
      piAntLinjer    = 0
      piEkstVpiLevNr = INT(DYNAMIC-FUNCTION('getForeignValues':U IN h_dvpiartbas))
      pcArtikkelNr   = DYNAMIC-FUNCTION('colValues':U IN h_dvpiartbas,
                                         INPUT "ArtikkelNr")
      .
  /* Leverandører som det ikke er lagt opp parameter for, har slik håndtering. */
  /* Lev. som har "nei, no, false , 0" har ikke.                               */
  {syspara.i 1 13 piEkstVPILevNr pcTekst}
  IF can-do("1,Nei,no,false,N", pcTekst) THEN
      pbT-Modell = FALSE.
  ELSE 
      pbT-Modell = TRUE.

  /* Henter radnummer på rad som har fokus. */
  RUN GetFocusedRow IN h_bvpiartbas (OUTPUT piFokusRad).

  /* Henter liste med valgte filer i browser. */
  RUN GetSelectedRows IN h_bvpiartbas (OUTPUT pcValgteArtikler).

  /* Bygg temp-table fra utvalg i browser. */
  IF pcValgteArtikler <> "" THEN
  UTVALG-BROWSE:
  DO piLoop1 = 1 TO NUM-ENTRIES(pcValgteArtikler,CHR(1)):
    FIND VPIArtBas NO-LOCK WHERE
        VPIArtBas.EkstVPILevNr = piEkstVPILevNr AND
        VPIArtBAs.VAreNr       = ENTRY(piLoop1,pcValgteArtikler,CHR(1)) NO-ERROR.
    IF NOT AVAILABLE VPIArtBas THEN 
        NEXT UTVALG-BROWSE.
        .
    /* Returneres 0 i plArtikkelNr, er artikkelen ikke koblet. */
    RUN GetArtikkelNr IN h_dvpiartbas (piEkstVpiLevNr, VPIArtBas.VareNr, OUTPUT plArtikkelNr).
    IF AVAILABLE Strekkode THEN
        RELEASE Strekkode.
    FIND Strekkode NO-LOCK WHERE
       Strekkode.Kode = VPIArtBas.VareNr NO-ERROR.
    IF AVAILABLE Strekkode THEN
      ASSIGN
        plArtikkelNr = Strekkode.ArtikkelNr
        .
/*     /* Artikkelen er allerede koblet */                */
/*     IF plArtikkelNr <> 0 AND                           */
/*         can-find(ArtBas where                          */
/*                 ArtBas.ArtikkelNr = plArtikkelNr) THEN */
/*         NEXT UTVALG-BROWSE.                            */

    FIND tmpVare WHERE 
        tmpVare.VareNr = VPIArtBas.VareNr NO-ERROR.
    IF NOT AVAILABLE tmpVare THEN
    DO:
        CREATE tmpVare.
        ASSIGN
            piAntLinjer    = piAntLinjer + 1
            tmpVare.VareNr = VPIArtBas.VareNr
            .
        STATUS DEFAULT "Teller linjer (" + STRING(piAntLinjer) + ").".
    END.
  END. /* UTVALG-BROWSE */

  /* Bygg temp-table med alle valgte artikkler og bruk den videre. */
  /* Henter aktiv query og benytter denne til å bygge varenummerliste */
  ELSE
  BRUK-QUERY:
  DO:
      ASSIGN
          pcQueryWhere = DYNAMIC-FUNCTION('getQueryWhere':U IN h_dvpiartbas).
          .
      CREATE QUERY  hQuery.
      CREATE BUFFER hBuffer FOR TABLE "VPIArtBas".

      hQuery:SET-BUFFERS(hBuffer).
      hQuery:QUERY-PREPARE(pcQueryWhere).
      hQuery:QUERY-OPEN().

      /* Leser alle VPIArtBas i query. */
      LESVARENR:
      REPEAT:
          hQuery:GET-NEXT() NO-ERROR.
          IF NOT hBuffer:AVAILABLE THEN LEAVE.
          ASSIGN 
              hField1 = hBuffer:BUFFER-FIELD("VareNr")
              .
          /* Returneres 0 i plArtikkelNr, er artikkelen ikke koblet. */
          RUN GetArtikkelNr IN h_dvpiartbas (piEkstVpiLevNr, hField1:BUFFER-VALUE(), OUTPUT plArtikkelNr).
          IF AVAILABLE Strekkode THEN
              RELEASE Strekkode.
          FIND Strekkode NO-LOCK WHERE
             Strekkode.Kode = hField1:BUFFER-VALUE() NO-ERROR.
          IF AVAILABLE Strekkode THEN
            ASSIGN
              plArtikkelNr = Strekkode.ArtikkelNr
              .
/*           /* Artikkelen er allerede koblet */                */
/*           IF plArtikkelNr <> 0 AND                           */
/*               can-find(ArtBas where                          */
/*                       ArtBas.ArtikkelNr = plArtikkelNr) THEN */
/*               NEXT LESVARENR.                                */

          FIND tmpVare WHERE 
              tmpVare.VareNr = hField1:BUFFER-VALUE() NO-ERROR.
          IF NOT AVAILABLE tmpVare THEN
          DO:
              CREATE tmpVare.
              ASSIGN
                  piAntLinjer    = piAntLinjer + 1
                  tmpVare.VareNr = hField1:BUFFER-VALUE()
                  .
              IF piAntLinjer MODULO 100 = 0 THEN
              DO:
                  STATUS DEFAULT "Teller linjer (" + STRING(piAntLinjer) + ").".
              END.
          END.
      END. /* LESVARENR */
      hQuery:QUERY-CLOSE().
      ASSIGN
          hQuery = ?
          .
      STATUS DEFAULT "".
  END. /* BRUK-QUERY */
 
  /* Ikke lov å velge INGEN */
  IF piAntLinjer = 0 THEN
  DO:
      MESSAGE "Ingen artikler valgt for oppdatering."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      c02SjekkListe = "".
      RETURN.
  END.

  /* Artikler valgt i browser. */
  MESSAGE "Skal oppdatering av utvalg startes?"
    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Bekreft"
    UPDATE pbOk.
  IF pbOk = FALSE THEN
  DO:
      c02SjekkListe = "".
      RETURN.
  END.

  /* Tar vare på den første posten for å reposisjonere i browser etter oppdatering. */
  FIND FIRST tmpVare.
  ASSIGN
      pcKeyValues = string(piEkstVPILevNr) + chr(1) + 
                    tmpVare.VareNr
      .
  /* Behandler artikklene.                                            */
  /* Kontrollerer fortløpende om neste artikkel allerede er behandlet */
  /* fordi den er med i en modell.                                    */
  VARE:
  FOR EACH tmpVare:

      ASSIGN
          piLoop1          = piLoop1 + 1
          plArtikkelNr     = 0
          pcValgteArtikler = tmpVare.VareNr
      .

      STATUS DEFAULT "Oppretter " + STRING(piLoop1) + " av " + STRING(piAntLinjer) + ".".

      /* Returneres 0 i plArtikkelNr, er artikkelen ikke koblet. */
      RUN GetArtikkelNr IN h_dvpiartbas (piEkstVpiLevNr, tmpVare.VareNr, OUTPUT plArtikkelNr).
      IF AVAILABLE Strekkode THEN
          RELEASE Strekkode.
      FIND Strekkode NO-LOCK WHERE
         Strekkode.Kode = tmpVare.VareNr NO-ERROR.
      IF AVAILABLE Strekkode THEN
        ASSIGN
          plArtikkelNr = Strekkode.ArtikkelNr
          .
/*       /* Artikkelen er allerede koblet */                */
/*       IF plArtikkelNr <> 0 and                           */
/*          can-find(ArtBas where                           */
/*                   ArtBas.ArtikkelNr = plArtikkelNr) THEN */
/*           NEXT VARE.                                     */
      RUN OppdaterArtikkel (INPUT  piEkstVPILevNr,
                            INPUT  pcValgteArtikler, 
                            INPUT  1, /* Ny */
                            OUTPUT pbOk).
      IF RETURN-VALUE = "AVBRYT" THEN
      DO:
          c02SjekkListe = "".
          RETURN "Oppdatering avbrutt. (2)".
      END.
  END. /* VARE */

  STATUS DEFAULT " ".

  /* Oppdaterer prisene */
  RUN Pris.

  /*
  /* Setter markøren på valgt rad. */
  IF pcKeyValues <> "" THEN
  DO:
      DYNAMIC-FUNCTION('openQuery':U IN h_dvpiartbas).
      DYNAMIC-FUNCTION('findRow':U IN h_dvpiartbas,
         INPUT pcKeyValues /* CHARACTER */).
      RUN dataAvailable IN h_dvpiartbas
          ( INPUT "SAME" /* CHARACTER */).
      RUN SetBrowseFocus IN h_bvpiartbas (pcValgteArtikler,
                                          piEkstVpiLevNr).
  END.
  */
  /* Nullstiller 02 sjekken */
  ASSIGN
      c02SjekkListe = ""
      .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OverforTilHKVPI wWin 
PROCEDURE OverforTilHKVPI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DEF VAR pcValgteArtikler AS CHAR NO-UNDO.
  DEF VAR piLoop1          AS INT  NO-UNDO.
  DEF VAR pcTekst          AS CHAR NO-UNDO.
  DEF VAR pbOk             AS LOG  NO-UNDO.
  DEF VAR pbMore           AS LOG  NO-UNDO.
  DEF VAR piAntLinjer      AS INT  NO-UNDO.
  DEF VAR pcKeyValues      AS char NO-UNDO.
  DEF VAR piFokusRad       AS INT  NO-UNDO.
  DEF VAR piEkstVpiLevNr   AS INT  NO-UNDO.
  DEF VAR pcArtikkelNr     AS CHAR NO-UNDO.
  DEF VAR pbT-Modell       AS LOG  NO-UNDO.
  DEF VAR pcQueryWhere     AS CHAR NO-UNDO.
  DEF VAR plArtikkelNr     AS DEC  NO-UNDO.
  
  DEF VAR hBuffer          AS HANDLE NO-UNDO.
  DEF VAR hQuery           AS HANDLE NO-UNDO.

  DEFINE BUFFER bufVPIArtBas FOR VPIArtBas.


  /* Tømmer tmp-file */
  FOR EACH tmpVare:
      DELETE tmpVare.
  END.

  ASSIGN
      piAntLinjer    = 0
      piEkstVpiLevNr = INT(DYNAMIC-FUNCTION('getForeignValues':U IN h_dvpiartbas))
      pcArtikkelNr   = DYNAMIC-FUNCTION('colValues':U IN h_dvpiartbas,
                                         INPUT "ArtikkelNr")
      .
  /* Henter radnummer på rad som har fokus. */
  RUN GetFocusedRow IN h_bvpiartbas (OUTPUT piFokusRad).

  /* Henter liste med valgte filer i browser. */
  RUN GetSelectedRows IN h_bvpiartbas (OUTPUT pcValgteArtikler).

  /* Bygg temp-table fra utvalg i browser. */
  IF pcValgteArtikler <> "" THEN
  UTVALG-BROWSE:
  DO piLoop1 = 1 TO NUM-ENTRIES(pcValgteArtikler,CHR(1)):
    FIND VPIArtBas NO-LOCK WHERE
        VPIArtBas.EkstVPILevNr = piEkstVPILevNr AND
        VPIArtBAs.VAreNr       = ENTRY(piLoop1,pcValgteArtikler,CHR(1)) NO-ERROR.
    IF NOT AVAILABLE VPIArtBas THEN 
        NEXT UTVALG-BROWSE.
        .
    /* Returneres 0 i plArtikkelNr, er artikkelen ikke koblet. */
    RUN GetArtikkelNr IN h_dvpiartbas (piEkstVpiLevNr, VPIArtBas.VareNr, OUTPUT plArtikkelNr).
    IF AVAILABLE Strekkode THEN
        RELEASE Strekkode.
    FIND Strekkode NO-LOCK WHERE
       Strekkode.Kode = VPIArtBas.VareNr NO-ERROR.
    IF AVAILABLE Strekkode THEN
      ASSIGN
        plArtikkelNr = Strekkode.ArtikkelNr
        .
/*     /* Artikkelen er allerede koblet */                */
/*     IF plArtikkelNr <> 0 AND                           */
/*         can-find(ArtBas where                          */
/*                 ArtBas.ArtikkelNr = plArtikkelNr) THEN */
/*         NEXT UTVALG-BROWSE.                            */

    FIND tmpVare WHERE 
        tmpVare.VareNr = VPIArtBas.VareNr NO-ERROR.
    IF NOT AVAILABLE tmpVare THEN
    DO:
        CREATE tmpVare.
        ASSIGN
            piAntLinjer    = piAntLinjer + 1
            tmpVare.VareNr = VPIArtBas.VareNr
            .
        STATUS DEFAULT "Teller linjer (" + STRING(piAntLinjer) + ").".
    END.
  END. /* UTVALG-BROWSE */

  /* Bygg temp-table med alle valgte artikkler og bruk den videre. */
  /* Henter aktiv query og benytter denne til å bygge varenummerliste */
  ELSE
  BRUK-QUERY:
  DO:
      ASSIGN
          pcQueryWhere = DYNAMIC-FUNCTION('getQueryWhere':U IN h_dvpiartbas).
          .
      CREATE QUERY  hQuery.
      CREATE BUFFER hBuffer FOR TABLE "VPIArtBas".

      hQuery:SET-BUFFERS(hBuffer).
      hQuery:QUERY-PREPARE(pcQueryWhere).
      hQuery:QUERY-OPEN().

      /* Leser alle VPIArtBas i query. */
      LESVARENR:
      REPEAT:
          hQuery:GET-NEXT() NO-ERROR.
          IF NOT hBuffer:AVAILABLE THEN LEAVE.
          ASSIGN 
              hField1 = hBuffer:BUFFER-FIELD("VareNr")
              .
          /* Returneres 0 i plArtikkelNr, er artikkelen ikke koblet. */
          RUN GetArtikkelNr IN h_dvpiartbas (piEkstVpiLevNr, hField1:BUFFER-VALUE(), OUTPUT plArtikkelNr).
          IF AVAILABLE Strekkode THEN
              RELEASE Strekkode.
          FIND Strekkode NO-LOCK WHERE
             Strekkode.Kode = hField1:BUFFER-VALUE() NO-ERROR.
          IF AVAILABLE Strekkode THEN
            ASSIGN
              plArtikkelNr = Strekkode.ArtikkelNr
              .
/*           /* Artikkelen er allerede koblet */                */
/*           IF plArtikkelNr <> 0 AND                           */
/*               can-find(ArtBas where                          */
/*                       ArtBas.ArtikkelNr = plArtikkelNr) THEN */
/*               NEXT LESVARENR.                                */

          FIND tmpVare WHERE 
              tmpVare.VareNr = hField1:BUFFER-VALUE() NO-ERROR.
          IF NOT AVAILABLE tmpVare THEN
          DO:
              CREATE tmpVare.
              ASSIGN
                  piAntLinjer    = piAntLinjer + 1
                  tmpVare.VareNr = hField1:BUFFER-VALUE()
                  .
              IF piAntLinjer MODULO 100 = 0 THEN
              DO:
                  STATUS DEFAULT "Teller linjer (" + STRING(piAntLinjer) + ").".
              END.
          END.
      END. /* LESVARENR */
      hQuery:QUERY-CLOSE().
      ASSIGN
          hQuery = ?
          .
      STATUS DEFAULT "".
  END. /* BRUK-QUERY */
 
  /* Ikke lov å velge INGEN */
  IF piAntLinjer = 0 THEN
  DO:
      MESSAGE "Ingen artikler valgt for overføring til VPI."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN.
  END.

  /* Artikler valgt i browser. */
  MESSAGE "Skal overføring til HK's VPI register av utvalg startes?"
    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Bekreft"
    UPDATE pbOk.
  IF pbOk = FALSE THEN
  DO:
      RETURN.
  END.

  /* Tar vare på den første posten for å reposisjonere i browser etter oppdatering. */
  FIND FIRST tmpVare.
  ASSIGN
      pcKeyValues = string(piEkstVPILevNr) + chr(1) + 
                    tmpVare.VareNr
      piLoop1 = 0
      .
  /* Behandler artikklene.                                            */
  /* Kontrollerer fortløpende om neste artikkel allerede er behandlet */
  /* fordi den er med i en modell.                                    */
  VARE:
  FOR EACH tmpVare:

      ASSIGN
          plArtikkelNr     = 0
          pcValgteArtikler = tmpVare.VareNr
      .

      STATUS DEFAULT "Oppretter " + STRING(piLoop1) + " av " + STRING(piAntLinjer) + ".".

      /* Returneres 0 i plArtikkelNr, er artikkelen ikke koblet. */
      RUN GetArtikkelNr IN h_dvpiartbas (piEkstVpiLevNr, tmpVare.VareNr, OUTPUT plArtikkelNr).
      IF AVAILABLE Strekkode THEN
          RELEASE Strekkode.
      FIND Strekkode NO-LOCK WHERE
         Strekkode.Kode = tmpVare.VareNr NO-ERROR.
      IF AVAILABLE Strekkode THEN
        ASSIGN
          plArtikkelNr = Strekkode.ArtikkelNr
          .
      /* Kopierer VPI til HK's VPI register */
      FIND VPIArtBas NO-LOCK WHERE
          VPIArtBAs.EkstVPILevNr = piEkstVpiLevNr AND
          VPIArtBas.VareNr       = tmpVare.VareNr NO-ERROR.
      IF AVAILABLE VPIArtBas THEN
      DO:
          FIND bufVPIArtBas EXCLUSIVE-LOCK WHERE
              bufVPIArtBAs.EkstVPILevNr = 1 /* HK */ AND
              bufVPIArtBas.VareNr       = tmpVare.VareNr NO-ERROR.
          IF NOT AVAILABLE bufVPIArtBas THEN
              CREATE bufVPIArtBas.
          BUFFER-COPY VPIArtBas TO bufVPIArtBas
              ASSIGN
              bufVPIArtBas.EkstVPILevNr = 1.
          ASSIGN
              piLoop1 = piLoop1 + 1.

      END.

  END. /* VARE */

  IF piLoop1 > 0 THEN
      MESSAGE STRING(piLoop1) + " Artikler kopiert til HK's VPI register."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
  ELSE
      MESSAGE "Ingen artikler ble kopiert."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.

  STATUS DEFAULT " ".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrevNext wWin 
PROCEDURE PrevNext :
/*------------------------------------------------------------------------------
  Purpose:     bläddring från artikelkort
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER cRettning AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE pcState AS CHARACTER  NO-UNDO.
    IF CAN-DO("Prev,Next",cRettning) THEN DO:
        CASE cRettning:
            WHEN "Prev" THEN
                RUN fetchPrev IN h_dvpiartbas.
            WHEN "Next" THEN
                RUN fetchNext IN h_dvpiartbas.
        END CASE.
        PUBLISH "ByttArtikkel" (DECI(DYNAMIC-FUNCTION('columnValue':U IN h_dvpiartbas,
              INPUT "Artikkelnr"))).
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Pris wWin 
PROCEDURE Pris :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DEF VAR pcValgteArtikler AS CHAR NO-UNDO.
  DEF VAR piLoop1          AS INT  NO-UNDO.
  DEF VAR pcTekst          AS CHAR NO-UNDO.
  DEF VAR pbOk             AS LOG  NO-UNDO.
  DEF VAR pbMore           AS LOG  NO-UNDO.
  DEF VAR piAntLinjer      AS INT  NO-UNDO.
  DEF VAR pcKeyValues      AS char NO-UNDO.
  DEF VAR piFokusRad       AS INT  NO-UNDO.
  DEF VAR piEkstVpiLevNr   AS INT  NO-UNDO.
  DEF VAR pcArtikkelNr     AS CHAR NO-UNDO.
  DEF VAR plPRis           AS DEC  NO-UNDO.

  DEF VAR pcQueryWhere     AS CHAR NO-UNDO.
  DEF VAR plArtikkelNr     AS DEC  NO-UNDO.

  DEF VAR hBuffer          AS HANDLE NO-UNDO.
  DEF VAR hQuery           AS HANDLE NO-UNDO.

  DEFINE BUFFER bVPIArtBas FOR VPIArtBas.

  /* Tømmer tmp-file */
  FOR EACH tmpVare:
      DELETE tmpVare.
  END.

  ASSIGN
      piEkstVpiLevNr = INT(DYNAMIC-FUNCTION('getForeignValues':U IN h_dvpiartbas))
      pcArtikkelNr   = DYNAMIC-FUNCTION('colValues':U IN h_dvpiartbas,
                                         INPUT "ArtikkelNr")
      .

  /* Henter radnummer på rad som har fokus. */
  RUN GetFocusedRow IN h_bvpiartbas (OUTPUT piFokusRad).

  /* Henter liste med valgte filer i browser. */
  RUN GetSelectedRows IN h_bvpiartbas (OUTPUT pcValgteArtikler).

  /* Bygg temp-table fra utvalg i browser. */
  IF pcValgteArtikler <> "" THEN
  UTVALG-BROWSE:
  DO piLoop1 = 1 TO NUM-ENTRIES(pcValgteArtikler,CHR(1)):
    FIND VPIArtBas NO-LOCK WHERE
        VPIArtBas.EkstVPILevNr = piEkstVPILevNr AND
        VPIArtBAs.VAreNr       = ENTRY(piLoop1,pcValgteArtikler,CHR(1)) NO-ERROR.
    IF NOT AVAILABLE VPIArtBas THEN 
        NEXT UTVALG-BROWSE.
        .
    /* Returneres 0 i plArtikkelNr, er artikkelen ikke koblet. */
    RUN GetArtikkelNr IN h_dvpiartbas (piEkstVpiLevNr, VPIArtBas.VareNr, OUTPUT plArtikkelNr).
    IF AVAILABLE Strekkode THEN
        RELEASE Strekkode.
    FIND Strekkode NO-LOCK WHERE
       Strekkode.Kode = VPIArtBas.VareNr NO-ERROR.
    IF AVAILABLE Strekkode THEN
      ASSIGN
        plArtikkelNr = Strekkode.ArtikkelNr
        .
/*     /* Artikkelen er allerede koblet */                */
/*     IF plArtikkelNr <> 0 AND                           */
/*         can-find(ArtBas where                          */
/*                 ArtBas.ArtikkelNr = plArtikkelNr) THEN */
/*         NEXT UTVALG-BROWSE.                            */

    FIND tmpVare WHERE 
        tmpVare.VareNr = VPIArtBas.VareNr NO-ERROR.
    IF NOT AVAILABLE tmpVare THEN
    DO:
        CREATE tmpVare.
        ASSIGN
            piAntLinjer    = piAntLinjer + 1
            tmpVare.VareNr = VPIArtBas.VareNr
            .
        STATUS DEFAULT "Teller linjer (" + STRING(piAntLinjer) + ").".
    END.
  END. /* UTVALG-BROWSE */

  /* Bygg temp-table med alle valgte artikkler og bruk den videre. */
  /* Henter aktiv query og benytter denne til å bygge varenummerliste */
  ELSE
  BRUK-QUERY:
  DO:
      ASSIGN
          pcQueryWhere = DYNAMIC-FUNCTION('getQueryWhere':U IN h_dvpiartbas).
          .
      CREATE QUERY  hQuery.
      CREATE BUFFER hBuffer FOR TABLE "VPIArtBas".

      hQuery:SET-BUFFERS(hBuffer).
      hQuery:QUERY-PREPARE(pcQueryWhere).
      hQuery:QUERY-OPEN().

      /* Leser alle VPIArtBas i query. */
      LESVARENR:
      REPEAT:
          hQuery:GET-NEXT() NO-ERROR.
          IF NOT hBuffer:AVAILABLE THEN LEAVE.
          ASSIGN 
              hField1 = hBuffer:BUFFER-FIELD("VareNr")
              .
          /* Returneres 0 i plArtikkelNr, er artikkelen ikke koblet. */
          RUN GetArtikkelNr IN h_dvpiartbas (piEkstVpiLevNr, hField1:BUFFER-VALUE(), OUTPUT plArtikkelNr).
          IF AVAILABLE Strekkode THEN
              RELEASE Strekkode.
          FIND Strekkode NO-LOCK WHERE
             Strekkode.Kode = hField1:BUFFER-VALUE() NO-ERROR.
          IF AVAILABLE Strekkode THEN
            ASSIGN
              plArtikkelNr = Strekkode.ArtikkelNr
              .
/*           /* Artikkelen er allerede koblet */                */
/*           IF plArtikkelNr <> 0 AND                           */
/*               can-find(ArtBas where                          */
/*                       ArtBas.ArtikkelNr = plArtikkelNr) THEN */
/*               NEXT LESVARENR.                                */

          FIND tmpVare WHERE 
              tmpVare.VareNr = hField1:BUFFER-VALUE() NO-ERROR.
          IF NOT AVAILABLE tmpVare THEN
          DO:
              CREATE tmpVare.
              ASSIGN
                  piAntLinjer    = piAntLinjer + 1
                  tmpVare.VareNr = hField1:BUFFER-VALUE()
                  .
              IF piAntLinjer MODULO 100 = 0 THEN
              DO:
                  STATUS DEFAULT "Teller linjer (" + STRING(piAntLinjer) + ").".
              END.
          END.
      END. /* LESVARENR */
      hQuery:QUERY-CLOSE().
      ASSIGN
          hQuery = ?
          .
      STATUS DEFAULT "".
  END. /* BRUK-QUERY */

  /* Ikke lov å velge INGEN */
  IF piAntLinjer = 0 THEN
  DO:
      MESSAGE "Ingen artikler valgt for oppdatering."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN.
  END.

  /* Tar vare på den første posten for å reposisjonere i browser etter oppdatering. */
  FIND FIRST tmpVare.
  ASSIGN
      pcKeyValues = string(piEkstVPILevNr) + chr(1) + 
                    tmpVare.VareNr
      .
  /* Behandler artikklene.                                            */
  /* Kontrollerer fortløpende om neste artikkel allerede er behandlet */
  /* fordi den er med i en modell.                                    */
  VARE:
  FOR EACH tmpVare:
  
    ASSIGN
        piLoop1          = piLoop1 + 1
        plArtikkelNr     = 0
        pcValgteArtikler = tmpVare.VareNr
    .
  
    STATUS DEFAULT "Overfører pris " + STRING(piLoop1) + " av " + STRING(piAntLinjer) + ".".
  
    /* Returneres 0 i plArtikkelNr, er artikkelen ikke koblet. */
    RUN SjekkVPIPris IN h_dvpiartbas (piEkstVpiLevNr, ENTRY(1,pcValgteArtikler,CHR(1)), OUTPUT plPris).
    IF plPris = 0 THEN
        NEXT.
  
  
    /* Oppdaterer valgte artikler.                                                   */
    /* Oppdateringsrutinen henter informasjonen fra første artikkel i listen. Fra de */
    /* øvrige artiklene hentes kun EAN koden.                                        */
    /* Oppdatering gjøres mot en eksisterende lokal artikkel, eller det opprettes en */
    /* ny lokal artikkel.                                                            */
    RUN OppdaterArtikkel (INPUT  piEkstVPILevNr,
                          INPUT  pcValgteArtikler, 
                          INPUT  3, /* Pris */
                          OUTPUT pbOk).
    /* Viser eventuell feilmelding. */
    IF RETURN-VALUE = "AVBRYT" THEN
        RETURN "Overføring av pris avbrutt.".

    /* aktiverer priskøposten. */
    FIND ArtBas NO-LOCK WHERE
        ArtBas.ArtikkelNr = DEC(pcValgteArtikler) NO-ERROR.
    IF AVAILABLE ArtBas THEN
        RUN KlargjorPrisKoEn IN h_PrisKo (ROWID(ArtBas)).

  END. /* VARE */

  IF pcKeyValues <> "" THEN
  DO:
      DYNAMIC-FUNCTION('openQuery':U IN h_dvpiartbas).
      DYNAMIC-FUNCTION('findRow':U IN h_dvpiartbas,
         INPUT pcKeyValues /* CHARACTER */).
      RUN dataAvailable IN h_dvpiartbas
          ( INPUT "SAME" /* CHARACTER */).
      RUN SetBrowseFocus IN h_bvpiartbas (pcValgteArtikler,
                                          piEkstVpiLevNr).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettOppdat wWin 
PROCEDURE SettOppdat :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DEF VAR pcValgteArtikler AS CHAR NO-UNDO.
  DEF VAR piLoop1          AS INT  NO-UNDO.
  DEF VAR pcTekst          AS CHAR NO-UNDO.
  DEF VAR pbOk             AS LOG  NO-UNDO.
  DEF VAR pbMore           AS LOG  NO-UNDO.
  DEF VAR piAntLinjer      AS INT  NO-UNDO.
  DEF VAR pcKeyValues      AS char NO-UNDO.
  DEF VAR piFokusRad       AS INT  NO-UNDO.
  DEF VAR piEkstVpiLevNr   AS INT  NO-UNDO.

  ASSIGN
      piEkstVpiLevNr = INT(DYNAMIC-FUNCTION('getForeignValues':U IN h_dvpiartbas))
      .

  /* Henter radnummer på rad som har fokus. */
  RUN GetFocusedRow IN h_bvpiartbas (OUTPUT piFokusRad).

  /* Artikler valgt i browser. */
  MESSAGE "Skal status på datasettes settes til OPPDATERT?" SKIP(1)
          "Dette gjøres normal bare hvis det er nødvendig å lese inn en ny VPI fil" SKIP
          "før datasettet datasettet er ferdigbehandlet."
    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Bekreft"
    UPDATE pbOk.
  IF pbOk = FALSE THEN
      RETURN.

  /* Tar vare på den første posten for å reposisjonere i browser etter oppdatering. */
  ASSIGN
      pcKeyValues = string(piEkstVPILevNr) + chr(1) + ENTRY(1,pcValgteArtikler,CHR(1))
      .
  /* Setter datasettet til statu 5 - Oppdatert. */
  RUN SettOppdatert IN h_dvpidatasett (INPUT  piEkstVPILevNr, INPUT 5).

  /* Viser eventuell feilmelding. */
  IF RETURN-VALUE <> "" THEN
      MESSAGE RETURN-VALUE
          VIEW-AS ALERT-BOX INFO BUTTONS OK.

  IF pcKeyValues <> "" THEN
  DO:
      DYNAMIC-FUNCTION('openQuery':U IN h_dvpidatasett).
      DYNAMIC-FUNCTION('findRow':U IN h_dvpidatasett,
         INPUT pcKeyValues /* CHARACTER */).
      RUN dataAvailable IN h_dvpidatasett
          ( INPUT "SAME" /* CHARACTER */).
      RUN SetBrowseFocus IN h_bvpidatasett (STRING(piEkstVPILevNr)).
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SjekkKobling wWin 
PROCEDURE SjekkKobling :
/*------------------------------------------------------------------------------
  Purpose:     Initierer kontroll av kobling mot lokalt VPI register
               for alle VPI poster på aktuell VPI leverandør.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DEF VAR pcValgteArtikler AS CHAR NO-UNDO.
  DEF VAR piLoop1          AS INT  NO-UNDO.
  DEF VAR pcTekst          AS CHAR NO-UNDO.
  DEF VAR pbOk             AS LOG  NO-UNDO.
  DEF VAR pbMore           AS LOG  NO-UNDO.
  DEF VAR piAntLinjer      AS INT  NO-UNDO.
  DEF VAR pcKeyValues      AS char NO-UNDO.
  DEF VAR piFokusRad       AS INT  NO-UNDO.
  DEF VAR piEkstVpiLevNr   AS INT  NO-UNDO.
  DEF VAR pcArtikkelNr     AS CHAR NO-UNDO.

  ASSIGN
      piEkstVpiLevNr = INT(DYNAMIC-FUNCTION('getForeignValues':U IN h_dvpiartbas))
      pcArtikkelNr   = DYNAMIC-FUNCTION('colValues':U IN h_dvpiartbas,
                                         INPUT "ArtikkelNr")
      .

  /* Henter radnummer på rad som har fokus. */
  RUN GetFocusedRow IN h_bvpiartbas (OUTPUT piFokusRad).

  /* Henter liste med valgte poster i browser. */
  RUN GetSelectedRows IN h_bvpiartbas (OUTPUT pcValgteArtikler).

  /* Ikke lov å velge INGEN */
  IF pcValgteArtikler <> "" THEN
  DO:
      MESSAGE "Skal kontroll av valgte VPI poster startes?" SKIP(1)
              "Eksisterende koblinger tas bort og ny kontroll utført." SKIP
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Bekreft"
        UPDATE pbOk.
      IF pbOk = FALSE THEN
          RETURN.
  END.
  ELSE DO:
      MESSAGE "Skal kontroll av VPI register startes?" SKIP(1)
              "Eksisterende koblinger tas bort og ny kontroll utført." SKIP
              "Kontrollen utføres på hele registeret til VPI leverandøren" SKIP
              "og vil kunne ta noen minutter hvis det er mange poster i registeret."
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Bekreft"
        UPDATE pbOk.
      IF pbOk = FALSE THEN
          RETURN.
  END.

  {sww.i}
  /* Tar vare på den første posten for å reposisjonere i browser etter oppdatering. */
  ASSIGN
      pcKeyValues = string(piEkstVPILevNr) + chr(1) + 
                    ENTRY(1,pcValgteArtikler,CHR(1))
      .
  /* Oppheving av kobling, nullstiller kun artikkelnummeret i VPI posten. Den */
  /* lokale artikkelen blir liggende og må slettes manuelt.                   */
  RUN KontrollerKobling IN h_dvpiartbas (piEkstVpiLevNr, pcValgteArtikler).
  {swn.i}

  /* Viser eventuell feilmelding. */
  IF RETURN-VALUE <> "" THEN
      MESSAGE RETURN-VALUE
          VIEW-AS ALERT-BOX INFO BUTTONS OK.

  IF pcKeyValues <> "" THEN
  DO:
      DYNAMIC-FUNCTION('openQuery':U IN h_dvpiartbas).
      DYNAMIC-FUNCTION('findRow':U IN h_dvpiartbas,
         INPUT pcKeyValues /* CHARACTER */).
      RUN dataAvailable IN h_dvpiartbas
          ( INPUT "SAME" /* CHARACTER */).
      RUN SetBrowseFocus IN h_bvpiartbas (pcValgteArtikler,
                                          piEkstVpiLevNr).
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Slett wWin 
PROCEDURE Slett :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR pbSvar           AS LOG  NO-UNDO.
  DEF VAR piEkstVPILevNr   AS INT  NO-UNDO.
  DEF VAR pcKeyValues      AS char NO-UNDO.
  DEF VAR piFokusRad       AS INT  NO-UNDO.
  DEF VAR pcListe          AS CHAR NO-UNDO.

  ASSIGN
    piEkstVpiLevNr = INT(DYNAMIC-FUNCTION('getForeignValues':U IN h_dvpiartbas))
    pbSvar = FALSE
    .

  /* Henter radnummer på rad som har fokus. */
  RUN GetFocusedRow IN h_bvpiartbas (OUTPUT piFokusRad).

  /* Henter liste med valgte linjer i browser. */
  RUN GetSelectedRows IN h_bvpiartbas (OUTPUT pcListe).

  /* Ikke lov å velge INGEN */
  IF pcListe = "" THEN
  DO:
      MESSAGE "Du har valgt å sletter ALLE vpi poster fra VPIleverandør " + STRING(piEkstVPILevNr) + "." SKIP(1)
              "Er du sikker på at alle VPI postene for denne VPIleverandøren skal slettes?"
          VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
          UPDATE pbSvar.
      IF pbSvar <> TRUE THEN
          RETURN.
  END.
  ELSE DO:
      /* Artikler valgt i browser. */
      MESSAGE "Der er valgt " + string(num-entries(pcListe,CHR(1))) + " VPI poster." SKIP
              "Skal sletting startes?"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Bekreft"
        UPDATE pbSvar.
      IF pbSvar = FALSE THEN
          RETURN.
  END.

  /* Tar vare på den første posten for å reposisjonere i browser etter oppdatering. */
  IF pcListe <> "" THEN
  DO:
      ASSIGN
          pcKeyValues = ENTRY(1,pcListe,CHR(1))
          .
      RUN GetPrevVPIArtBas IN h_dvpiartbas (piEkstVPILevNr, INPUT-OUTPUT pcKeyValues).
      ASSIGN
          pcKeyValues = string(piEkstVPILevNr) + chr(1) + 
                        pcKeyValues
          .
  END.
  ELSE
      pcKeyValues = "".

  RUN SlettVPIRegister IN h_dvpiartbas (INPUT piEkstVPILevNr, INPUT pcListe).
  IF pcKeyValues = "" THEN
      DYNAMIC-FUNCTION('openQuery':U IN h_dvpiartbas).
  ELSE DO:
      IF pcKeyValues <> "" THEN
      DO:
          DYNAMIC-FUNCTION('openQuery':U IN h_dvpiartbas).
          DYNAMIC-FUNCTION('findRow':U IN h_dvpiartbas,
             INPUT pcKeyValues /* CHARACTER */).
          RUN dataAvailable IN h_dvpiartbas
              ( INPUT "SAME" /* CHARACTER */).
          RUN SetBrowseFocus IN h_bvpiartbas (pcListe,
                                              piEkstVpiLevNr).
      END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Vis2Artikkel wWin 
PROCEDURE Vis2Artikkel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  
------------------------------------------------------------------------------*/
  
  DEF INPUT PARAMETER piEkstVpiLevNr   AS INT   NO-UNDO.
  DEF INPUT PARAMETER pcValgteArtikler AS CHAR  NO-UNDO.

  DEF VAR piLoop1          AS INT   NO-UNDO.
  DEF VAR pcTekst          AS CHAR  NO-UNDO.
  DEF VAR pbOk             AS LOG   NO-UNDO.
  DEF VAR pbMore           AS LOG   NO-UNDO.
  DEF VAR piAntLinjer      AS INT   NO-UNDO.
  DEF VAR pcKeyValues      AS char  NO-UNDO.
  DEF VAR piFokusRad       AS INT   NO-UNDO.
  DEF VAR prRecid          AS RECID NO-UNDO.

  /* Ikke lov å velge INGEN */
  IF pcValgteArtikler = "" THEN
  DO:
      MESSAGE "Ingen artikler valgt for visning."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN.
  END.

  /* Kun en kan velges */
  IF num-entries(pcValgteArtikler,CHR(1)) > 1 THEN
  DO:
      MESSAGE "Kun en artikkel kan være valgt for visning av artikkel."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN.
  END.

  /* Henter artikkelnummer */
  RUN GetRecidArtBas IN h_dvpiartbas (INPUT  piEkstVPILevNr,
                                      INPUT  entry(1,pcValgteArtikler,CHR(1)), 
                                      OUTPUT prRecid).
  /* Ugyldig artikkelnummer */
  IF prRecid = ? THEN
  DO:
      MESSAGE "Det finnes ingen lokal artikkel på denne VPI posten." SKIP
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN.
  END.
  /* Oppstart av artikkelkort */
  fLockvindu(TRUE).
  run w-vartkor (prRecid, "ENDRE," + STRING(THIS-PROCEDURE)).
  fLockvindu(FALSE).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisArtikkel wWin 
PROCEDURE VisArtikkel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  
------------------------------------------------------------------------------*/
  
  DEF VAR pcValgteArtikler AS CHAR  NO-UNDO.

  DEF VAR piLoop1          AS INT   NO-UNDO.
  DEF VAR pcTekst          AS CHAR  NO-UNDO.
  DEF VAR pbOk             AS LOG   NO-UNDO.
  DEF VAR pbMore           AS LOG   NO-UNDO.
  DEF VAR piAntLinjer      AS INT   NO-UNDO.
  DEF VAR pcKeyValues      AS char  NO-UNDO.
  DEF VAR piFokusRad       AS INT   NO-UNDO.
  DEF VAR piEkstVpiLevNr   AS INT   NO-UNDO.
  DEF VAR prRecid          AS RECID NO-UNDO.

  ASSIGN
      piEkstVpiLevNr = INT(DYNAMIC-FUNCTION('getForeignValues':U IN h_dvpiartbas))
      .

  /* Henter radnummer på rad som har fokus. */
  RUN GetFocusedRow IN h_bvpiartbas (OUTPUT piFokusRad).

  /* Henter liste med valgte filer i browser. */
  RUN GetSelectedRows IN h_bvpiartbas (OUTPUT pcValgteArtikler).

  /* Viser artikkel */
  RUN Vis2Artikkel (INPUT piEkstVpiLevNr, INPUT pcValgteArtikler).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fLockvindu wWin 
FUNCTION fLockvindu RETURNS CHARACTER
  ( INPUT lLock AS LOGICAL ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hDetteVindu AS HANDLE     NO-UNDO.
  ASSIGN hDetteVindu = THIS-PROCEDURE:CURRENT-WINDOW
         hDetteVindu:SENSITIVE = NOT lLock.

  RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

