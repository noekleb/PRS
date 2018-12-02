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

DEFINE VAR iAntFlip  AS INTE   NO-UNDO.
DEFINE VAR hPrint    AS HANDLE NO-UNDO.
DEFINE VAR hHelp     AS HANDLE NO-UNDO.
DEFINE VAR hExit     AS HANDLE NO-UNDO.
DEFINE VAR hSearch   AS HANDLE NO-UNDO.
DEFINE VAR hVareEksport AS HANDLE NO-UNDO.
DEFINE VAR hArtBas   AS HANDLE NO-UNDO.
DEFINE VAR piX       AS INTEGER    NO-UNDO.
DEFINE VAR piX2      AS INTEGER    NO-UNDO.
DEFINE VAR cKampRowIdList AS CHAR NO-UNDO.
DEFINE VAR cKampIdList    AS CHAR NO-UNDO.

DEF VAR hUtvalg      AS HANDLE NO-UNDO.
DEF VAR hBrowseLinje AS HANDLE NO-UNDO.
DEF VAR bOk          AS LOG NO-UNDO.
DEF VAR ix           AS INT NO-UNDO.
DEF VAR iReturn      AS INT NO-UNDO.
DEF VAR hRowObject   AS HANDLE NO-UNDO.

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


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fLockvindu wWin 
FUNCTION fLockvindu RETURNS CHARACTER
  ( INPUT lLock AS LOGICAL )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getHodeStatus wWin 
FUNCTION getHodeStatus RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getKampanjeHodeViewer wWin 
FUNCTION getKampanjeHodeViewer RETURNS HANDLE
  ( INPUT ihUtvalg AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setUtvalgHandle wWin 
FUNCTION setUtvalgHandle RETURNS LOGICAL
  ( INPUT ihUtvalg AS HANDLE)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_bkampanjehode AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bkampanjelinje AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dartpris AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dkampanjehode AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dkampanjelinje AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_fvisbilde AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vartpris AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vkampanjehode AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vkampanjehodesmall AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vkampanjelinje AS HANDLE NO-UNDO.

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     SPACE(160.01) SKIP(31.92)
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
         TITLE              = "Kampanjer / Normalprisendringer"
         HEIGHT             = 32.43
         WIDTH              = 162
         MAX-HEIGHT         = 57.14
         MAX-WIDTH          = 384
         VIRTUAL-HEIGHT     = 57.14
         VIRTUAL-WIDTH      = 384
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
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
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
ON CTRL-TAB OF wWin /* Kampanjer / Normalprisendringer */
DO:
  MESSAGE "jjj"
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Kampanjer / Normalprisendringer */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Kampanjer / Normalprisendringer */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  RUN exitObject.
/*   APPLY "CLOSE":U TO THIS-PROCEDURE.  */
/*   RETURN NO-APPLY.                    */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-RESIZED OF wWin /* Kampanjer / Normalprisendringer */
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

&IF DEFINED(UIB_is_Running) NE 0 &THEN
  RUN InitializeObject.
&ENDIF

ON 'CTRL-TAB':U OF wWin ANYWHERE DO:
    IF DYNAMIC-FUNCTION('getCurrentPage':U) < iAntFlip THEN
        RUN SelectPage (DYNAMIC-FUNCTION('getCurrentPage':U) + 1).
    ELSE
        RUN SelectPage (1).
END.
ON 'HELP':U OF FRAME {&FRAME-NAME}
DO:
    RUN pgmHelp.
END.
/*     ON 'choose':U OF hPrint                                                  */
/*     DO:                                                                      */
/*         RUN PrintKampanje IN h_dkampanjehode                                 */
/*             ( INPUT INT(DYNAMIC-FUNCTION('columnValue':U IN h_dkampanjehode, */
/*               INPUT "KampanjeId" ))).                                        */
/*     END.                                                                     */

ON "ALT-N":U OF CURRENT-WINDOW ANYWHERE   
DO:
    IF DYNAMIC-FUNCTION('getCurrentPage':U) = 1 THEN
        RUN addRecord IN h_vKampanjehode.
    ELSE
        RUN addRecord IN h_vKampanjeLinje.
END.
ON "ALT-A":U OF CURRENT-WINDOW ANYWHERE   
DO:
    IF DYNAMIC-FUNCTION('getCurrentPage':U) = 1 THEN
        RUN CancelRecord IN h_vKampanjehode.
    ELSE
        RUN CancelRecord IN h_vKampanjeLinje.
END.
ON "ALT-L":U OF CURRENT-WINDOW ANYWHERE   
DO:
    IF DYNAMIC-FUNCTION('getCurrentPage':U) = 1 THEN
        RUN UpdateRecord IN h_vKampanjehode.
    ELSE
        RUN UpdateRecord IN h_vKampanjeLinje.
END.
ON "ALT-K":U OF CURRENT-WINDOW ANYWHERE   
DO:
    IF DYNAMIC-FUNCTION('getCurrentPage':U) = 1 THEN
        RUN CopyRecord IN h_vKampanjehode.
    ELSE
        RUN CopyRecord IN h_vKampanjeLinje.
END.
ON "ALT-D":U OF CURRENT-WINDOW ANYWHERE   
DO:
    IF DYNAMIC-FUNCTION('getCurrentPage':U) = 1 THEN
        RUN DeleteRecord IN h_vKampanjehode.
    ELSE
        RUN DeleteRecord IN h_vKampanjeLinje.
END.

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
             INPUT  'sdo/dkampanjehode.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessnoDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedkampanjehodeOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dkampanjehode ).
       RUN repositionObject IN h_dkampanjehode ( 2.43 , 149.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 9.80 ) */

       RUN constructObject (
             INPUT  'sdo/dkampanjelinje.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessnoDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsKampanjeLinje.KampanjeId,KampanjeIdObjectNamedkampanjelinjeOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dkampanjelinje ).
       RUN repositionObject IN h_dkampanjelinje ( 2.67 , 137.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'sdo/dartpris.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessnoDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsArtPris.ArtikkelNr,ArtikkelNr,ArtPris.ProfilNr,ProfilNrObjectNamedartprisOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dartpris ).
       RUN repositionObject IN h_dartpris ( 3.62 , 150.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'prg/vkampanjehodesmall.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EnabledObjFldsToDisable(None)ModifyFields(All)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vkampanjehodesmall ).
       RUN repositionObject IN h_vkampanjehodesmall ( 2.91 , 2.00 ) NO-ERROR.
       /* Size in AB:  ( 2.00 , 151.20 ) */

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EdgePixels2DeactivateTargetOnHidenoDisabledActionsFlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsTableio,NavigationTableIOTypeSaveSupportedLinksNavigation-source,Tableio-sourceToolbarBandsToolbarAutoSizenoToolbarDrawDirectionhorizontalLogicalObjectNameDisabledActionsHiddenActionsUpdate,ResetHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0RemoveMenuOnHidenoCreateSubMenuOnConflictyesNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar ).
       RUN repositionObject IN h_dyntoolbar ( 1.00 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar ( 1.24 , 159.80 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/folder.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FolderLabels':U + 'Endring|Artikler' + 'FolderTabWidth0FolderFont-1HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_folder ).
       RUN repositionObject IN h_folder ( 5.05 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_folder ( 27.86 , 160.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN initPages ('2':U) NO-ERROR.

       /* Links to SmartDataObject h_dkampanjehode. */
       RUN addLink ( h_dyntoolbar , 'Navigation':U , h_dkampanjehode ).

       /* Links to SmartDataObject h_dkampanjelinje. */
       RUN addLink ( h_dkampanjehode , 'Data':U , h_dkampanjelinje ).
       RUN addLink ( h_dyntoolbar-2 , 'Navigation':U , h_dkampanjelinje ).

       /* Links to SmartDataObject h_dartpris. */
       RUN addLink ( h_dkampanjelinje , 'Data':U , h_dartpris ).

       /* Links to SmartDataViewer h_vkampanjehodesmall. */
       RUN addLink ( h_dkampanjehode , 'Data':U , h_vkampanjehodesmall ).

       /* Links to SmartFolder h_folder. */
       RUN addLink ( h_folder , 'Page':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_vkampanjehodesmall ,
             h_dyntoolbar , 'AFTER':U ).
       RUN adjustTabOrder ( h_folder ,
             h_vkampanjehodesmall , 'AFTER':U ).
    END. /* Page 0 */
    WHEN 1 THEN DO:
       RUN constructObject (
             INPUT  'prg/vkampanjehode.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EnabledObjFldsToDisable(None)ModifyFields(All)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vkampanjehode ).
       RUN repositionObject IN h_vkampanjehode ( 6.71 , 6.00 ) NO-ERROR.
       /* Size in AB:  ( 7.33 , 151.40 ) */

       RUN constructObject (
             INPUT  'prg/bkampanjehode.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bkampanjehode ).
       RUN repositionObject IN h_bkampanjehode ( 14.29 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_bkampanjehode ( 18.38 , 157.00 ) NO-ERROR.

       /* Links to SmartDataViewer h_vkampanjehode. */
       RUN addLink ( h_dkampanjehode , 'Data':U , h_vkampanjehode ).
       RUN addLink ( h_vkampanjehode , 'Update':U , h_dkampanjehode ).
       RUN addLink ( h_dyntoolbar , 'TableIO':U , h_vkampanjehode ).

       /* Links to SmartDataBrowser h_bkampanjehode. */
       RUN addLink ( h_dkampanjehode , 'Data':U , h_bkampanjehode ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_vkampanjehode ,
             h_folder , 'AFTER':U ).
       RUN adjustTabOrder ( h_bkampanjehode ,
             h_vkampanjehode , 'AFTER':U ).
    END. /* Page 1 */
    WHEN 2 THEN DO:
       RUN constructObject (
             INPUT  'prg/vartpris.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EnabledObjFldsToDisable(None)ModifyFields(All)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vartpris ).
       RUN repositionObject IN h_vartpris ( 8.38 , 113.00 ) NO-ERROR.
       /* Size in AB:  ( 22.14 , 47.00 ) */

       RUN constructObject (
             INPUT  'prg/vkampanjelinje.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EnabledObjFldsToDisable(None)ModifyFields(All)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vkampanjelinje ).
       RUN repositionObject IN h_vkampanjelinje ( 8.62 , 2.00 ) NO-ERROR.
       /* Size in AB:  ( 6.48 , 78.00 ) */

       RUN constructObject (
             INPUT  'prg/fvisbilde.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'LogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_fvisbilde ).
       RUN repositionObject IN h_fvisbilde ( 9.57 , 83.00 ) NO-ERROR.
       /* Size in AB:  ( 4.81 , 28.20 ) */

       RUN constructObject (
             INPUT  'prg/bkampanjelinje.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bkampanjelinje ).
       RUN repositionObject IN h_bkampanjelinje ( 15.52 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_bkampanjelinje ( 17.14 , 110.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EdgePixels2DeactivateTargetOnHidenoDisabledActionsFlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsTableioTableIOTypeSaveSupportedLinksTableio-sourceToolbarBandsToolbarAutoSizenoToolbarDrawDirectionHorizontalLogicalObjectNameDisabledActionsHiddenActionsUpdate,ResetHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0RemoveMenuOnHidenoCreateSubMenuOnConflictyesNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar-2 ).
       RUN repositionObject IN h_dyntoolbar-2 ( 6.71 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar-2 ( 1.24 , 157.00 ) NO-ERROR.

       /* Links to SmartDataViewer h_vartpris. */
       RUN addLink ( h_dartpris , 'Data':U , h_vartpris ).

       /* Links to SmartDataViewer h_vkampanjelinje. */
       RUN addLink ( h_dkampanjelinje , 'Data':U , h_vkampanjelinje ).
       RUN addLink ( h_vkampanjelinje , 'Update':U , h_dkampanjelinje ).
       RUN addLink ( h_dyntoolbar-2 , 'TableIo':U , h_vkampanjelinje ).
       RUN addLink ( h_vkampanjehodesmall , 'KampProcent':U , h_vkampanjelinje ).

       /* Links to SmartFrame h_fvisbilde. */
       RUN addLink ( h_dkampanjelinje , 'Data':U , h_fvisbilde ).
       RUN addLink ( h_folder , 'Page':U , h_fvisbilde ).

       /* Links to SmartDataBrowser h_bkampanjelinje. */
       RUN addLink ( h_dkampanjelinje , 'Data':U , h_bkampanjelinje ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_dyntoolbar-2 ,
             h_folder , 'AFTER':U ).
       RUN adjustTabOrder ( h_vartpris ,
             h_dyntoolbar-2 , 'AFTER':U ).
       RUN adjustTabOrder ( h_vkampanjelinje ,
             h_vartpris , 'AFTER':U ).
       RUN adjustTabOrder ( h_fvisbilde ,
             h_vkampanjelinje , 'AFTER':U ).
       RUN adjustTabOrder ( h_bkampanjelinje ,
             h_fvisbilde , 'AFTER':U ).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Artikkelkort wWin 
PROCEDURE Artikkelkort :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cArtikkelNr AS CHARACTER  NO-UNDO.
  ASSIGN cArtikkelNr = DYNAMIC-FUNCTION('columnValue':U IN h_dkampanjelinje,
        INPUT "Artikkelnr" /* CHARACTER */).

/*   DYNAMIC-FUNCTION('closeQuery':U IN h_dkampanjelinje).  */
/*   DYNAMIC-FUNCTION('openQuery':U IN h_dkampanjelinje).   */
/*   DYNAMIC-FUNCTION('findRowWhere':U IN h_dkampanjelinje, */
/*       INPUT "Artikkelnr" /* CHARACTER */,                */
/*       INPUT cArtikkelNr /* CHARACTER */,                 */
/*       INPUT "=" /* CHARACTER */).                        */
  FIND ArtBas WHERE ArtBas.ArtikkelNr = DECI(cArtikkelNr) NO-LOCK NO-ERROR.
  IF NOT AVAIL ArtBas THEN
      RETURN.
  fLockvindu(TRUE).
  run w-vartkor (input recid(ArtBas), "ENDRE," + STRING(THIS-PROCEDURE)).
  fLockvindu(FALSE).
  RUN refreshRow IN h_dkampanjelinje.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ArtPrisOK wWin 
PROCEDURE ArtPrisOK :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE dKampanjePris AS DECIMAL    NO-UNDO.
    ASSIGN dKampanjePris = DECI(DYNAMIC-FUNCTION('columnValue':U IN h_dkampanjelinje,
     INPUT "Pris2" /* CHARACTER */)).
    RUN KampanjVerdier IN h_vartpris
    ( INPUT dKampanjePris /* DECIMAL */).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE artSearch wWin 
PROCEDURE artSearch :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE iKampanjeId AS INTEGER     NO-UNDO.
DEFINE VARIABLE dArtikkelnr AS DECIMAL     NO-UNDO.
    
    RUN d-sokikampanje.w (OUTPUT dArtikkelnr).
    IF dArtikkelnr > 0 THEN DO:
        RUN wSokIkampanje.w (INPUT dArtikkelnr,OUTPUT iKampanjeId).
        IF iKampanjeId > 0 THEN
            DYNAMIC-FUNCTION('findRowWhere':U IN h_dkampanjehode,
                INPUT "KampanjeId" /* CHARACTER */,
                INPUT STRING(iKampanjeId) /* CHARACTER */,
                INPUT "=" /* CHARACTER */).
/*             MESSAGE iKampanjeId                    */
/*                 VIEW-AS ALERT-BOX INFO BUTTONS OK. */
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE avlysKampanje wWin 
PROCEDURE avlysKampanje :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT  PARAMETER cValues AS CHARACTER  NO-UNDO.

   IF NOT DYNAMIC-FUNCTION("RunProc","avbryt_Kampanje.p",
                           cValues,
                           ?) THEN.
     /*DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Advarsel","").*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DeselectRecord wWin 
PROCEDURE DeselectRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
hBrowseLinje:DESELECT-FOCUSED-ROW().
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
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Etiketter wWin 
PROCEDURE Etiketter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cKampanjeId AS CHARACTER  NO-UNDO.
  ASSIGN cKampanjeId = DYNAMIC-FUNCTION('columnValue':U IN h_dkampanjelinje,
        INPUT "KampanjeId" /* CHARACTER */).
  IF cKampanjeId = ? THEN
      RETURN NO-APPLY.
  RUN d-skrivEanEtikett.w ("Kampanje=" + cKampanjeId).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE excelRecord wWin 
PROCEDURE excelRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DYNAMIC-FUNCTION("ToExcelViaFile",DYNAMIC-FUNCTION("getBrowseHandle" IN h_bkampanjelinje),0).
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
IF DYNAMIC-FUNCTION("UtvalgIsMaster" IN h_vkampanjehode) AND VALID-HANDLE(hUtvalg) THEN 
  RUN InvalidateHandle IN hUtvalg (THIS-PROCEDURE).
ELSE IF NOT DYNAMIC-FUNCTION("UtvalgIsMaster" IN h_vkampanjehode) AND VALID-HANDLE(hUtvalg) THEN
  APPLY "close" TO hUtvalg.

DYNAMIC-FUNCTION("setCleanUpResize", THIS-PROCEDURE:CURRENT-WINDOW).

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
  DEF VAR hDynSelect AS HANDLE NO-UNDO. 

  /* Code placed here will execute PRIOR to standard behavior. */
  
  IF VALID-HANDLE(h_folder) THEN
DYNAMIC-FUNCTION('setFolderLabels':U IN h_folder,
     INPUT "Endring|Artikler" /* CHARACTER */).
  DYNAMIC-FUNCTION("setRowsToBatch" IN h_dkampanjelinje,2000).

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
/*    RUN InitierSok IN h_sosok                                                     */
/*     ( INPUT h_bkampanjehode, INPUT "KampanjeId,Beskrivelse,StartDato,Aktivert"). */
/*    RUN initPages                  */
/*     ( INPUT "2" /* CHARACTER */). */
  IF VALID-HANDLE(h_dyntoolbar) THEN DO:
      RUN skapaButtons.
   END.
   SUBSCRIBE TO "ApplHjelp" IN h_dynToolbar.

   IF VALID-HANDLE(h_dproclib) THEN
       RUN GetLng IN h_dproclib (OUTPUT wCurrLng).

   IF VALID-HANDLE(h_folder) THEN
       ASSIGN iAntFlip = NUM-ENTRIES(DYNAMIC-FUNCTION('getFolderLabels':U IN h_folder),"|").
   SUBSCRIBE TO "ArtPrisOK" IN h_dartpris.
   SUBSCRIBE TO "Print" IN h_dyntoolbar.
   SUBSCRIBE TO "pgmHelp" IN h_dyntoolbar.
   SUBSCRIBE TO "artSearch" IN h_dyntoolbar.
   SUBSCRIBE TO "VareEksport" IN h_dyntoolbar.
   SUBSCRIBE TO "Artikkelkort" IN h_dyntoolbar-2.
   SUBSCRIBE TO "Etiketter" IN h_dyntoolbar-2.
   SUBSCRIBE TO "excelRecord" IN h_dyntoolbar-2.
/*    SUBSCRIBE TO "Artikkelkort" IN h_bkampanjelinje.  */
   
   DYNAMIC-FUNCTION('setDisabledActions':U IN h_dyntoolbar,
     INPUT "Copy" /* CHARACTER */).
   
   DYNAMIC-FUNCTION('setDisabledActions':U IN h_dyntoolbar-2,
     INPUT "Copy" /* CHARACTER */).

   IF VALID-HANDLE(h_vkampanjelinje) THEN DO:
       RUN setContainerHandle IN h_vkampanjelinje (THIS-PROCEDURE).
       IF VALID-HANDLE(h_dartpris) THEN
           RUN setArtPrisHandle IN h_dkampanjelinje (h_dartpris).
   END.

   IF VALID-HANDLE(h_dkampanjelinje) THEN
       RUN refreshRow IN h_dkampanjelinje.

  DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, 
                   DYNAMIC-FUNCTION("getContainerHandle" IN h_vkampanjehodesmall),
                   "F-Main").
 
  RUN getDynSelectHdl IN  h_vkampanjehodesmall (OUTPUT hDynSelect) NO-ERROR.

  DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, 
                 DYNAMIC-FUNCTION("getContainerHandle" IN hdynSelect),
                 "frmSelection") NO-ERROR.

  DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, 
                 DYNAMIC-FUNCTION("getContainerHandle" IN hdynSelect),
                 "frmSelection") NO-ERROR.

  DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, 
                   DYNAMIC-FUNCTION("getContainerHandle" IN h_vkampanjehode),
                   "F-Main,Notat").
  DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, 
                   DYNAMIC-FUNCTION("getContainerHandle" IN h_vkampanjehode),
                   "F-Main,Notat").

  DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, 
                   DYNAMIC-FUNCTION("getContainerHandle" IN h_vkampanjehode),
                   "F-Main,ProfilNr,Notat").
  /*    .
  DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, 
                   DYNAMIC-FUNCTION("getFrmSelection" IN h_vkampanjehode),
                   "frmSelection").
  */
  DYNAMIC-FUNCTION("setNoResizeX",THIS-PROCEDURE:CURRENT-WINDOW,
                                  DYNAMIC-FUNCTION("getContainerHandle" IN h_fvisbilde),
                                  "fMain,IMAGE-Sko").
  DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,
                                  DYNAMIC-FUNCTION("getContainerHandle" IN h_fvisbilde),
                                  "fMain,IMAGE-Sko").
  DYNAMIC-FUNCTION("setNoMoveX",THIS-PROCEDURE:CURRENT-WINDOW,
                                  DYNAMIC-FUNCTION("getContainerHandle" IN h_fvisbilde),
                                  "IMAGE-Sko").
  DYNAMIC-FUNCTION("setNoMoveY",THIS-PROCEDURE:CURRENT-WINDOW,
                                  DYNAMIC-FUNCTION("getContainerHandle" IN h_fvisbilde),
                                  "IMAGE-Sko").
  DYNAMIC-FUNCTION("setAddMoveX",THIS-PROCEDURE:CURRENT-WINDOW,
                                  DYNAMIC-FUNCTION("getContainerHandle" IN h_fvisbilde),
                                  "fMain").

  DYNAMIC-FUNCTION("setAddResizeX", THIS-PROCEDURE:CURRENT-WINDOW, 
                   DYNAMIC-FUNCTION("getContainerHandle" IN h_vkampanjelinje),
                   "Beskr").
  DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, 
                   DYNAMIC-FUNCTION("getContainerHandle" IN h_vkampanjelinje),
                   "F-Main").

  DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, 
                   DYNAMIC-FUNCTION("getContainerHandle" IN h_vartpris),
                   "rect-42,rect-43,rect-44,rect-55,f-main").
  DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, 
                   DYNAMIC-FUNCTION("getContainerHandle" IN h_vartpris),
                   "rect-42,rect-43,rect-44,rect-55").
  DYNAMIC-FUNCTION("setAddMoveX", THIS-PROCEDURE:CURRENT-WINDOW,
                   DYNAMIC-FUNCTION("getContainerHandle" IN h_vartpris),
                   "f-main").
  DYNAMIC-FUNCTION("setNoMoveX", THIS-PROCEDURE:CURRENT-WINDOW,
                   DYNAMIC-FUNCTION("getContainerHandle" IN h_vartpris),
                   "FI-profil,profilnr,"  +
                   "f-main,RECT-42,RECT-43,RECT-44,RECT-55," +
                   "FI-Gjeldende,FI-Kampanje,FI-Kronor,FI-Procent,FI-Tilbud,RS-Kampanje," +
                   "AktivFraDato,DB%1,DB%2,DBKr1,DBKr2,DivKost%1,DivKost%2,DivKostKr1,DivKostKr2,EuroPris1," +
                   "EuroPris2,Frakt%1,Frakt%2,Frakt1,Frakt2,InnkjopsPris1,InnkjopsPris2,Mva%1,Mva%2,MvaKr1,MvaKr2," +
                   "Pris1,Pris2,Rab1%1,Rab1%2,Rab1Kr1,Rab1Kr2,Rab2%1,Rab2%2,Rab2Kr1,Rab2Kr2,Rab3%1,Rab3%2,Rab3Kr1,Rab3Kr2," +
                   "Tilbud,TilbudTilDato,VareKost1,VareKost2"
                   ).
  DYNAMIC-FUNCTION("setNoMoveY", THIS-PROCEDURE:CURRENT-WINDOW,
                   DYNAMIC-FUNCTION("getContainerHandle" IN h_vartpris),
                   "FI-profil,profilnr,Rect-55" +
                   "f-main,RECT-42,RECT-43,RECT-44," + 
                   "FI-Gjeldende,FI-Kampanje,FI-Kronor,FI-Procent,FI-Tilbud,RS-Kampanje," +
                   "AktivFraDato,DB%1,DB%2,DBKr1,DBKr2,DivKost%1,DivKost%2,DivKostKr1,DivKostKr2,EuroPris1," +
                   "EuroPris2,Frakt%1,Frakt%2,Frakt1,Frakt2,InnkjopsPris1,InnkjopsPris2,Mva%1,Mva%2,MvaKr1,MvaKr2," +
                   "Pris1,Pris2,Rab1%1,Rab1%2,Rab1Kr1,Rab1Kr2,Rab2%1,Rab2%2,Rab2Kr1,Rab2Kr2,Rab3%1,Rab3%2,Rab3Kr1,Rab3Kr2," +
                   "Tilbud,TilbudTilDato,VareKost1,VareKost2"
                   ).
/*   DYNAMIC-FUNCTION("setAddMoveY", THIS-PROCEDURE:CURRENT-WINDOW,         */
/*                    DYNAMIC-FUNCTION("getContainerHandle" IN h_vartpris), */
/*                    "f-main").                                            */

  DYNAMIC-FUNCTION("SetResizeADM2panel",TRUE).
  DYNAMIC-FUNCTION("setOrgWinSize", THIS-PROCEDURE:CURRENT-WINDOW,650,500,0,0).

  hBrowseLinje = DYNAMIC-FUNCTION("getBrowseHandle" IN h_bkampanjelinje).

  DYNAMIC-FUNCTION("NewMenuBand",
                    hBrowseLinje,  /* parent widget */
                    "Deselect;Fjern markering av rad" +
                    ",SettPris;Endre pris for (markerte) artikler;SettArtPris" +
                    ",SlettArt;Fjern (markerte) artikler;SlettArt",
                   "").

  hRowObject = DYNAMIC-FUNCTION("getRowObject" IN h_dKampanjeHode).
  bOK = hRowObject:FIND-FIRST() NO-ERROR.
  IF NOT bOk THEN
    DYNAMIC-FUNCTION("setButtons" IN h_vKampanjeHode,FALSE).

  /* TN 24/2-09 Programmet går i loop hos JF hvis ikke det dukker opp en dialogboks. */
  IF USERID("skotex") = 'katrin' THEN
      MESSAGE "Kampanjeregistrering klar til bruk for " + USERID("skotex") + "."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
  /*
  RUN SelectPage(1).
  RUN viewPage(1).
  hBrowseLinje:MOVE-TO-TOP().
  */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE onChoose wWin 
PROCEDURE onChoose :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER xxx AS CHARACTER  NO-UNDO.
MESSAGE xxx
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
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
                RUN fetchPrev IN h_dkampanjelinje.
            WHEN "Next" THEN
                RUN fetchNext IN h_dkampanjelinje.
        END CASE.
        PUBLISH "ByttArtikkel" (DECI(DYNAMIC-FUNCTION('columnValue':U IN h_dkampanjelinje,
              INPUT "Artikkelnr"))).
    END.
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
  IF DYNAMIC-FUNCTION('getCurrentPage':U) = 1 AND 
     VALID-HANDLE(h_vkampanjehode) AND
     DYNAMIC-FUNCTION('getDataModified':U IN h_vkampanjehode)  THEN
      RUN confirmExit (INPUT-OUTPUT plCancel /* LOGICAL */).
  ELSE IF DYNAMIC-FUNCTION('getCurrentPage':U) = 2 AND 
       VALID-HANDLE(h_vkampanjelinje) AND
       DYNAMIC-FUNCTION('getDataModified':U IN h_vkampanjelinje)  THEN
      RUN confirmExit (INPUT-OUTPUT plCancel /* LOGICAL */).
  IF plCancel THEN
      RETURN NO-APPLY.
/*   IF DYNAMIC-FUNCTION('getCurrentPage':U) = 1 THEN DO: */
/*       RUN cancelRecord IN h_vkampanjehode.             */
/*   END.                                                 */
  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER( INPUT piPageNum).
  /* Code placed here will execute AFTER standard behavior.    */
  IF DYNAMIC-FUNCTION('getCurrentPage':U) = 1 THEN DO:
      IF VALID-HANDLE(hPrint) THEN
          ASSIGN hPrint:SENSITIVE = TRUE.
      IF VALID-HANDLE(hSearch) THEN
          ASSIGN hSearch:SENSITIVE = TRUE.
      IF VALID-HANDLE(hVareEksport) THEN
          ASSIGN hVareEksport:SENSITIVE = TRUE.
      DYNAMIC-FUNCTION('disableActions':U IN h_dyntoolbar,
          INPUT "Copy" /* CHARACTER */).
      IF DYNAMIC-FUNCTION('getRecordState':U IN h_vkampanjelinje) = "RecordAvailable"THEN
          RUN refreshRow IN h_dkampanjehode.
  END.
  ELSE IF DYNAMIC-FUNCTION('getCurrentPage':U) = 2 THEN DO:
      ASSIGN hPrint:SENSITIVE = FALSE.
      ASSIGN hSearch:SENSITIVE = FALSE.
      ASSIGN hVareEksport:SENSITIVE = FALSE.
      DYNAMIC-FUNCTION('disableActions':U IN h_dyntoolbar,
        INPUT "Add,Update,Copy,Delete" /* CHARACTER */).
      IF DYNAMIC-FUNCTION('getRecordState':U IN h_vkampanjehode) = "NoRecordAvailable" THEN DO:
          RUN disableObject IN h_vkampanjelinje.
          DYNAMIC-FUNCTION('disableActions':U IN h_dyntoolbar-2,
              INPUT "Add" /* CHARACTER */).
      END.
      ELSE
          DYNAMIC-FUNCTION('openQuery':U IN h_dkampanjelinje).
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetBrowseFocus wWin 
PROCEDURE SetBrowseFocus :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN RadFokus IN h_bkampanjelinje.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettArtPris wWin 
PROCEDURE SettArtPris :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR oiAvslagType  AS INT NO-UNDO.
DEF VAR ofEndring     AS DEC NO-UNDO.
DEF VAR cArtNumList   AS CHAR NO-UNDO.

bOK = FALSE.

RUN d-SettArtPrisKampanje.w ((IF hBrowseLinje:NUM-SELECTED-ROWS > 0 THEN 
                                hBrowseLinje:NUM-SELECTED-ROWS
                              ELSE 0),
                              OUTPUT oiAvslagType,
                              OUTPUT ofEndring,
                              OUTPUT bOK).

IF bOK THEN DO:
  IF hBrowseLinje:NUM-SELECTED-ROWS > 0 THEN DO:
    DO ix = 1 TO hBrowseLinje:NUM-SELECTED-ROWS:
      IF hBrowseLinje:FETCH-SELECTED-ROW(ix) THEN
        cArtNumList = cArtNumList + STRING(hBrowseLinje:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) + ",".
    END.
    bOK = DYNAMIC-FUNCTION("RunProc","artpris_for_kampanje.p",
                            STRING(DYNAMIC-FUNCTION("columnValue" IN h_dkampanjehode,"KampanjeId")) +
                            ",ARTNUM," + STRING(oiAvslagType) + "," + STRING(ofEndring) + "," + TRIM(cArtNumList,","),
                            ?).
  END.
  ELSE bOK = DYNAMIC-FUNCTION("RunProc","artpris_for_kampanje.p",
                            STRING(DYNAMIC-FUNCTION("columnValue" IN h_dkampanjehode,"KampanjeId")) +
                            ",ARTNUM," + STRING(oiAvslagType) + "," + STRING(ofEndring),
                            DYNAMIC-FUNCTION("getRowObjectTable" IN h_dkampanjelinje)).
  IF bOK THEN
    DYNAMIC-FUNCTION("openQuery" IN h_dkampanjelinje).
  ELSE 
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
END.
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
          DYNAMIC-FUNCTION('getContainerHandle':U IN h_dyntoolbar),
/*            INPUT FRAME {&FRAME-NAME}:HANDLE /* HANDLE */, */
           INPUT-OUTPUT piX /* INTEGER */,
           INPUT "Print:Kampanjehode" /* CHARACTER */,
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
 /* ------------- */
   DYNAMIC-FUNCTION('defineAction':U IN h_dyntoolbar,
     INPUT "SEARCH":U /* CHARACTER */,
     INPUT "Name,Caption,Image,Type,OnChoose,AccessType,Parent" /* CHARACTER */,
     INPUT "Search" + CHR(1) +
           "Search" + CHR(1) +
           "":U  + CHR(1) +
           "PUBLISH":U  + CHR(1) +
           "artSearch":U  + CHR(1) +
           "":U + CHR(1) +
           "":U).

   ASSIGN piX = hPrint:X + hPrint:WIDTH-PIXELS + 1
          hSearch = DYNAMIC-FUNCTION('createButton':U IN h_dyntoolbar,
          DYNAMIC-FUNCTION('getContainerHandle':U IN h_dyntoolbar),
/*            INPUT FRAME {&FRAME-NAME}:HANDLE /* HANDLE */, */
           INPUT-OUTPUT piX /* INTEGER */,
           INPUT "SEARCH" /* CHARACTER */,
           INPUT "SEARCH" /* CHARACTER */,
           INPUT "Sök" /* CHARACTER */,
           INPUT "icon\select.bmp" /* pcBitmap CHARACTER */,
           INPUT TRUE /* LOGICAL */).
/*    hHelp:MOVE-TO-TOP(). */
 /* -------------- */
  
   ASSIGN piX = FRAME {&FRAME-NAME}:WIDTH-PIXELS - hPrint:WIDTH-PIXELS - 3
          hExit = DYNAMIC-FUNCTION('createButton':U IN h_dyntoolbar,
          DYNAMIC-FUNCTION('getContainerHandle':U IN h_dyntoolbar),
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
          DYNAMIC-FUNCTION('getContainerHandle':U IN h_dyntoolbar),
/*            INPUT FRAME {&FRAME-NAME}:HANDLE /* HANDLE */, */
           INPUT-OUTPUT piX /* INTEGER */,
           INPUT "HELP" /* CHARACTER */,
           INPUT "HELP" /* CHARACTER */,
           INPUT "HELP" /* CHARACTER */,
           INPUT "icon\e-help.bmp" /* pcBitmap CHARACTER */,
           INPUT TRUE /* LOGICAL */).
/*    hHelp:MOVE-TO-TOP(). */
   
   ASSIGN piX2 = 150  /* 260 hvis nav-panel */
           hArtBas = DYNAMIC-FUNCTION('createButton':U IN h_dyntoolbar-2,
            DYNAMIC-FUNCTION('getContainerHandle':U IN h_dyntoolbar-2),
 /*            INPUT FRAME {&FRAME-NAME}:HANDLE /* HANDLE */, */
            INPUT-OUTPUT piX2 /* INTEGER */,
            INPUT "Artikkelkort" /* CHARACTER */,
            INPUT "Artikkelkort" /* CHARACTER */,
            INPUT "Artikkelkort" /* CHARACTER */,
            INPUT "icon\e-detail.bmp" /* pcBitmap CHARACTER */,
            INPUT TRUE /* LOGICAL */).
 /*    hArtBas:MOVE-TO-TOP(). */
    DYNAMIC-FUNCTION('defineAction':U IN h_dyntoolbar-2,
      INPUT "Artikkelkort":U /* CHARACTER */,
      INPUT "Name,Caption,Image,Link,Type,OnChoose,Parent" /* CHARACTER */,
      INPUT "Artikkelkort" + CHR(1) +
            "Vis artikkel" + CHR(1) +
            "":U  + CHR(1) +
            "":U  + CHR(1) +
            "PUBLISH":U  + CHR(1) +
            "Artikkelkort":U + CHR(1) +
            "Options":U).

    ASSIGN piX2 = piX2 + 4.
            hArtBas = DYNAMIC-FUNCTION('createButton':U IN h_dyntoolbar-2,
             DYNAMIC-FUNCTION('getContainerHandle':U IN h_dyntoolbar-2),
  /*            INPUT FRAME {&FRAME-NAME}:HANDLE /* HANDLE */, */
             INPUT-OUTPUT piX2 /* INTEGER */,
             INPUT "Etiketter" /* CHARACTER */,
             INPUT "Etiketter" /* CHARACTER */,
             INPUT "Etiketter" /* CHARACTER */,
             INPUT "" /* pcBitmap CHARACTER */,
             INPUT TRUE /* LOGICAL */).
  /*    hArtBas:MOVE-TO-TOP(). */
     DYNAMIC-FUNCTION('defineAction':U IN h_dyntoolbar-2,
       INPUT "Etiketter":U /* CHARACTER */,
       INPUT "Name,Caption,Image,Link,Type,OnChoose,Parent" /* CHARACTER */,
       INPUT "Etiketter" + CHR(1) +
             "Print etiketter" + CHR(1) +
             "":U  + CHR(1) +
             "":U  + CHR(1) +
             "PUBLISH":U  + CHR(1) +
             "Etiketter":U + CHR(1) +
             "Options":U).
   
     ASSIGN piX2 = piX2 + 4.
             hArtBas = DYNAMIC-FUNCTION('createButton':U IN h_dyntoolbar-2,
              DYNAMIC-FUNCTION('getContainerHandle':U IN h_dyntoolbar-2),
   /*            INPUT FRAME {&FRAME-NAME}:HANDLE /* HANDLE */, */
              INPUT-OUTPUT piX2 /* INTEGER */,
              INPUT "excelRecord" /* CHARACTER */,
              INPUT "excelRecord" /* CHARACTER */,
              INPUT "excelRecord" /* CHARACTER */,
              INPUT "gif/afexcel.gif" /* pcBitmap CHARACTER */,
              INPUT TRUE /* LOGICAL */).
   /*    hArtBas:MOVE-TO-TOP(). */
      DYNAMIC-FUNCTION('defineAction':U IN h_dyntoolbar-2,
        INPUT "excelRecord":U /* CHARACTER */,
        INPUT "Name,Caption,Image,Link,Type,OnChoose,Parent" /* CHARACTER */,
        INPUT "excelRecord" + CHR(1) +
              "Eksporter til Excel" + CHR(1) +
              "":U  + CHR(1) +
              "":U  + CHR(1) +
              "PUBLISH":U  + CHR(1) +
              "excelRecord":U + CHR(1) +
              "Options":U).

    /* ------------- */
       DYNAMIC-FUNCTION('defineAction':U IN h_dyntoolbar,
         INPUT "VareEksport":U /* CHARACTER */,
         INPUT "Name,Caption,Image,Type,OnChoose,AccessType,Parent" /* CHARACTER */,
         INPUT "VareEksport" + CHR(1) +
               "VareEksport" + CHR(1) +
               "":U  + CHR(1) +
               "PUBLISH":U  + CHR(1) +
               "VareEksport":U  + CHR(1) +
               "":U + CHR(1) +
               "":U).
    
       ASSIGN piX = hSearch:X + hPrint:WIDTH-PIXELS + 1
              hVareEksport = DYNAMIC-FUNCTION('createButton':U IN h_dyntoolbar,
              DYNAMIC-FUNCTION('getContainerHandle':U IN h_dyntoolbar),
    /*            INPUT FRAME {&FRAME-NAME}:HANDLE /* HANDLE */, */
               INPUT-OUTPUT piX /* INTEGER */,
               INPUT "VareEksport" /* CHARACTER */,
               INPUT "VareEksport" /* CHARACTER */,
               INPUT "Vare Eksport" /* CHARACTER */,
               INPUT "" /* pcBitmap CHARACTER */,
               INPUT TRUE /* LOGICAL */).
    /*    hHelp:MOVE-TO-TOP(). */
     /* -------------- */
 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettArt wWin 
PROCEDURE SlettArt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cArtNumList   AS CHAR NO-UNDO.

iReturn = DYNAMIC-FUNCTION("DoMessage",0,1,"Bekreft fjerning av &1 artikler","",
                           IF hBrowseLinje:NUM-SELECTED-ROWS > 0 THEN
                             STRING(hBrowseLinje:NUM-SELECTED-ROWS)
                           ELSE "alle").

IF iReturn = 1 THEN DO:
  IF hBrowseLinje:NUM-SELECTED-ROWS > 0 THEN DO:
    DO ix = 1 TO hBrowseLinje:NUM-SELECTED-ROWS:
      IF hBrowseLinje:FETCH-SELECTED-ROW(ix) THEN
        cArtNumList = cArtNumList + STRING(hBrowseLinje:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) + ",".
    END.
    bOK = DYNAMIC-FUNCTION("RunProc","slettart_fra_kampanje.p",
                            STRING(DYNAMIC-FUNCTION("columnValue" IN h_dkampanjehode,"KampanjeId")) +
                            ",ARTNUM," + TRIM(cArtNumList,","),
                            ?).
  END.
  ELSE bOK = DYNAMIC-FUNCTION("RunProc","slettart_fra_kampanje.p",
                            STRING(DYNAMIC-FUNCTION("columnValue" IN h_dkampanjehode,"KampanjeId")),
                            DYNAMIC-FUNCTION("getRowObjectTable" IN h_dkampanjelinje)).
  IF bOK THEN
    DYNAMIC-FUNCTION("openQuery" IN h_dkampanjelinje).
  ELSE 
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VareEksport wWin 
PROCEDURE VareEksport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "KampanjeHode;KampanjeId;Beskrivelse;StartDato;SluttDato;Kamp%;KampanjePris",
                      "where true",
                      INPUT-OUTPUT cKampRowIdList,
                      "KampanjeId",
                      INPUT-OUTPUT cKampIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.
  IF cKampIdList = '' THEN
  DO:
      MESSAGE 'Ingen data å eksportere.'
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN.
  END.
  ELSE DO:
      RUN eksporter_kampanjevarer.p(cKampIdList).
      MESSAGE "Varene i de valgte kampanjer er eksportert til Excel." SKIP
          cKampIdList
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisBildeKalkyle wWin 
PROCEDURE VisBildeKalkyle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT  PARAMETER ipArtikkelnr AS DECIMAL    NO-UNDO.
   DEFINE INPUT  PARAMETER ipBildNr     AS INTEGER    NO-UNDO.
   DEFINE        VARIABLE  ipProfilNr AS INTEGER    NO-UNDO.

   ipProfilNr = INT(ENTRY(2,DYNAMIC-FUNCTION('colValues':U IN h_dkampanjehode,
                         INPUT "profilnr" /* CHARACTER */),CHR(1))).
    RUN FindArtPris IN h_dartpris
    ( INPUT ipArtikkelNr /* DECIMAL */,
      INPUT ipProfilNr /* INTEGER */).
    
    FIND BildeRegister WHERE BildeRegister.BildNr = ipBildNr NO-LOCK NO-ERROR.
    IF AVAIL BildeRegister AND Bilderegister.FilNavn <> "" THEN
        RUN NyttBilde IN h_fvisbilde
            ( INPUT (IF CAN-FIND(FIRST bildedata OF BildeRegister WHERE teller >= 200) THEN "mini" ELSE "") + Bilderegister.FilNavn).


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getHodeStatus wWin 
FUNCTION getHodeStatus RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  RETURN DYNAMIC-FUNCTION('colValues':U IN h_dkampanjehode,
     INPUT "Aktivert,Komplett" /* CHARACTER */).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getKampanjeHodeViewer wWin 
FUNCTION getKampanjeHodeViewer RETURNS HANDLE
  ( INPUT ihUtvalg AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
hUtvalg = ihUtvalg.

RETURN h_vkampanjehode.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setUtvalgHandle wWin 
FUNCTION setUtvalgHandle RETURNS LOGICAL
  ( INPUT ihUtvalg AS HANDLE) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
hUtvalg = ihUtvalg.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

