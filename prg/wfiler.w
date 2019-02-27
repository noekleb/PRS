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

{tmpfiler.i &NEW = "NEW"}

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
DEFINE VARIABLE h_bdatasett AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bfiler AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bfillinjer AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bfillogg AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bkasse2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_ddatasett AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dfiler AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dfillinjer AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dfillogg AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dkasse AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar AS HANDLE NO-UNDO.
DEFINE VARIABLE h_fdatasettoolbar AS HANDLE NO-UNDO.
DEFINE VARIABLE h_ffilertoolbar AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vfiler AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vfiler2 AS HANDLE NO-UNDO.

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 161.6 BY 27.19.


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
         TITLE              = "Datamottak"
         HEIGHT             = 27.19
         WIDTH              = 161.6
         MAX-HEIGHT         = 34.62
         MAX-WIDTH          = 204.8
         VIRTUAL-HEIGHT     = 34.62
         VIRTUAL-WIDTH      = 204.8
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
ON END-ERROR OF wWin /* Datamottak */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Datamottak */
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
             INPUT  'sdo/dfiler.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessnoDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedfilerOpenOnInityesPromptColumnsFilId,Katalog,FilNavn,DatoPromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dfiler ).
       RUN repositionObject IN h_dfiler ( 1.48 , 116.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'sdo/ddatasett.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessnoDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsDatasett.FilId,FilIdObjectNameddatasettOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_ddatasett ).
       RUN repositionObject IN h_ddatasett ( 1.71 , 150.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.20 ) */

       RUN constructObject (
             INPUT  'sdo/dfillinjer.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessnoDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsFilLinjer.FilId,FilId,FilLinjer.DataSettId,DataSettIdObjectNamedfillinjerOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dfillinjer ).
       RUN repositionObject IN h_dfillinjer ( 1.71 , 143.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'sdo/dfillogg.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessnoDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsFilLogg.FilId,FilIdObjectNamedfilloggOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dfillogg ).
       RUN repositionObject IN h_dfillogg ( 1.95 , 133.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'prg/vfiler2.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EnabledObjFldsToDisable(None)ModifyFields(All)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vfiler2 ).
       RUN repositionObject IN h_vfiler2 ( 2.67 , 1.00 ) NO-ERROR.
       /* Size in AB:  ( 1.67 , 161.00 ) */

       RUN constructObject (
             INPUT  'sdo/dkasse.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessnoDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedkasseOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dkasse ).
       RUN repositionObject IN h_dkasse ( 1.71 , 99.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EdgePixels2DeactivateTargetOnHidenoDisabledActionsFlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsNavigationTableIOTypeSaveSupportedLinksNavigation-sourceToolbarBandsToolbarAutoSizenoToolbarDrawDirectionhorizontalLogicalObjectNameDisabledActionsHiddenActionsUpdate,ResetHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0RemoveMenuOnHidenoCreateSubMenuOnConflictyesNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar ).
       RUN repositionObject IN h_dyntoolbar ( 1.00 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar ( 1.24 , 161.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/folder.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FolderLabels':U + 'Filliste|Filinformasjon|Datasett|Transaksjoner|Butikker/Grupper/Kasser' + 'FolderTabWidth0FolderFont-1HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_folder ).
       RUN repositionObject IN h_folder ( 4.57 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_folder ( 23.57 , 161.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN initPages ('2':U) NO-ERROR.

       /* Links to SmartDataObject h_dfiler. */
       RUN addLink ( h_dyntoolbar , 'Navigation':U , h_dfiler ).

       /* Links to SmartDataObject h_ddatasett. */
       RUN addLink ( h_dfiler , 'Data':U , h_ddatasett ).
       RUN addLink ( h_dfiler , 'INFO':U , h_ddatasett ).

       /* Links to SmartDataObject h_dfillinjer. */
       RUN addLink ( h_ddatasett , 'Data':U , h_dfillinjer ).

       /* Links to SmartDataObject h_dfillogg. */
       RUN addLink ( h_dfiler , 'Data':U , h_dfillogg ).

       /* Links to SmartDataViewer h_vfiler2. */
       RUN addLink ( h_dfiler , 'Data':U , h_vfiler2 ).
       RUN addLink ( h_vfiler , 'GroupAssign':U , h_vfiler2 ).

       /* Links to SmartFolder h_folder. */
       RUN addLink ( h_folder , 'Page':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_vfiler2 ,
             h_dyntoolbar , 'AFTER':U ).
       RUN adjustTabOrder ( h_folder ,
             h_vfiler2 , 'AFTER':U ).
    END. /* Page 0 */
    WHEN 1 THEN DO:
       RUN constructObject (
             INPUT  'prg/ffilertoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'LogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_ffilertoolbar ).
       RUN repositionObject IN h_ffilertoolbar ( 5.76 , 142.00 ) NO-ERROR.
       /* Size in AB:  ( 22.10 , 19.20 ) */

       RUN constructObject (
             INPUT  'prg/bfiler.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bfiler ).
       RUN repositionObject IN h_bfiler ( 6.00 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_bfiler ( 21.91 , 139.00 ) NO-ERROR.

       /* Links to SmartFrame h_ffilertoolbar. */
       RUN addLink ( h_dfiler , 'Data':U , h_ffilertoolbar ).
       RUN addLink ( h_folder , 'Page':U , h_ffilertoolbar ).

       /* Links to SmartDataBrowser h_bfiler. */
       RUN addLink ( h_dfiler , 'Data':U , h_bfiler ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_ffilertoolbar ,
             h_folder , 'AFTER':U ).
       RUN adjustTabOrder ( h_bfiler ,
             h_ffilertoolbar , 'AFTER':U ).
    END. /* Page 1 */
    WHEN 2 THEN DO:
       RUN constructObject (
             INPUT  'prg/bfillogg.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bfillogg ).
       RUN repositionObject IN h_bfillogg ( 10.76 , 3.00 ) NO-ERROR.
       RUN resizeObject IN h_bfillogg ( 17.14 , 156.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'prg/vfiler.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EnabledObjFldsToDisable(None)ModifyFields(All)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vfiler ).
       RUN repositionObject IN h_vfiler ( 6.00 , 3.00 ) NO-ERROR.
       /* Size in AB:  ( 4.76 , 159.00 ) */

       /* Links to SmartDataBrowser h_bfillogg. */
       RUN addLink ( h_dfillogg , 'Data':U , h_bfillogg ).

       /* Links to SmartDataViewer h_vfiler. */
       RUN addLink ( h_dfiler , 'Data':U , h_vfiler ).
       RUN addLink ( h_vfiler , 'Update':U , h_dfiler ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_vfiler ,
             h_folder , 'AFTER':U ).
       RUN adjustTabOrder ( h_bfillogg ,
             h_vfiler , 'AFTER':U ).
    END. /* Page 2 */
    WHEN 3 THEN DO:
       RUN constructObject (
             INPUT  'prg/bdatasett.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bdatasett ).
       RUN repositionObject IN h_bdatasett ( 6.00 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_bdatasett ( 21.67 , 142.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'prg/fdatasettoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'LogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_fdatasettoolbar ).
       RUN repositionObject IN h_fdatasettoolbar ( 6.71 , 144.00 ) NO-ERROR.
       /* Size in AB:  ( 21.05 , 16.40 ) */

       /* Links to SmartDataBrowser h_bdatasett. */
       RUN addLink ( h_ddatasett , 'Data':U , h_bdatasett ).
       RUN addLink ( h_bdatasett , 'Update':U , h_ddatasett ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_bdatasett ,
             h_folder , 'AFTER':U ).
       RUN adjustTabOrder ( h_fdatasettoolbar ,
             h_bdatasett , 'AFTER':U ).
    END. /* Page 3 */
    WHEN 4 THEN DO:
       RUN constructObject (
             INPUT  'prg/bfillinjer.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bfillinjer ).
       RUN repositionObject IN h_bfillinjer ( 6.00 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_bfillinjer ( 21.43 , 157.00 ) NO-ERROR.

       /* Links to SmartDataBrowser h_bfillinjer. */
       RUN addLink ( h_dfillinjer , 'Data':U , h_bfillinjer ).
       RUN addLink ( h_bfillinjer , 'Update':U , h_dfillinjer ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_bfillinjer ,
             h_folder , 'AFTER':U ).
    END. /* Page 4 */
    WHEN 5 THEN DO:
       RUN constructObject (
             INPUT  'prg/bkasse2.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bkasse2 ).
       RUN repositionObject IN h_bkasse2 ( 6.00 , 3.00 ) NO-ERROR.
       RUN resizeObject IN h_bkasse2 ( 21.19 , 150.00 ) NO-ERROR.

       /* Links to SmartDataBrowser h_bkasse2. */
       RUN addLink ( h_dkasse , 'Data':U , h_bkasse2 ).
       RUN addLink ( h_bkasse2 , 'Update':U , h_dkasse ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_bkasse2 ,
             h_folder , 'AFTER':U ).
    END. /* Page 5 */

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
  DEF VAR piSideNr AS INT NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  assign
    piSideNr = DYNAMIC-FUNCTION('getCurrentPage':U)
    .
                                                                 
  /* Code placed here will execute AFTER standard behavior.    */
  
  /* Kobler inn/ut linker og abonnerer på hendelser. */
  CASE piSideNr:
      WHEN 1 THEN 
      DO:
        /*
        /* Datasett */
        RUN RemoveLink IN h_Folder (h_ddatasett, "DATA",   h_bdatasett) NO-ERROR.
        RUN RemoveLink IN h_Folder (h_bdatasett, "UPDATE", h_ddatasett) NO-ERROR.
        
        /* Loggposter */
        RUN RemoveLink IN h_Folder (h_dfillogg, "DATA",   h_bfillogg) NO-ERROR.
        RUN RemoveLink IN h_Folder (h_bfillogg, "UPDATE", h_dfillogg) NO-ERROR.

        /* Transaksjoner */
        RUN RemoveLink IN h_Folder (h_dfillinjer, "DATA",   h_bfillinjer) NO-ERROR.
        RUN RemoveLink IN h_Folder (h_bfillinjer, "UPDATE", h_dfillinjer) NO-ERROR.
        */
      END.
      WHEN 2 THEN 
      DO:
          /*
          /* Loggposter */
          RUN AddLink IN h_Folder (h_dfillogg, "DATA",   h_bfillogg) NO-ERROR.
          RUN AddLink IN h_Folder (h_bfillogg, "UPDATE", h_dfillogg) NO-ERROR.
          */
      END.
      WHEN 3 THEN DO:
          SUBSCRIBE "SlettDatasett"          IN h_fdatasettoolbar.
          SUBSCRIBE "OppdaterEttDatasettGUI" IN h_fdatasettoolbar.
          SUBSCRIBE "OverforDatasettGUI"     IN h_fdatasettoolbar.
          SUBSCRIBE "MakulerGUI"             IN h_fdatasettoolbar.
          /*          
          /* Datasett */
          RUN AddLink IN h_Folder (h_ddatasett, "DATA",   h_bdatasett) NO-ERROR.
          RUN AddLink IN h_Folder (h_bdatasett, "UPDATE", h_ddatasett) NO-ERROR.
      
          /* Transaksjoner */
          RUN RemoveLink IN h_Folder (h_dfillinjer, "DATA",   h_bfillinjer) NO-ERROR.
          RUN RemoveLink IN h_Folder (h_bfillinjer, "UPDATE", h_dfillinjer) NO-ERROR.
          */
      END.
      WHEN 4 THEN DO:
          /*
          /* Transaksjoner */
          RUN AddLink IN h_Folder (h_dfillinjer, "DATA",   h_bfillinjer) NO-ERROR.
          RUN AddLink IN h_Folder (h_bfillinjer, "UPDATE", h_dfillinjer) NO-ERROR.
          */
          RUN SjekkFilterSetting IN h_bfillinjer.
      END.
  END CASE.
 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dfilerDataAvailable wWin 
PROCEDURE dfilerDataAvailable :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  IF VALID-HANDLE(h_bdatasett) THEN
  DO:
      RUN DisableAlle IN h_bdatasett.
      DYNAMIC-FUNCTION('openQuery':U IN h_ddatasett).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EksporterFil wWin 
PROCEDURE EksporterFil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR pbOk        AS   LOG         NO-UNDO.
  DEF VAR pcFilNavn   AS   CHAR        NO-UNDO.
  DEF VAR pcColValues AS   CHAR        NO-UNDO.
  DEF VAR plFilId     LIKE Filer.FilId NO-UNDO.
  DEF VAR piAntLinjer AS   INT         NO-UNDO.

  /* 1. Sjekk om det er en filPost tilgjengelig i query. */
  assign
      pbOk = DYNAMIC-FUNCTION('rowAvailable':U IN h_dfiler,
               INPUT "CURRENT" /* CHARACTER */).

  IF pbOk THEN
  DO:
    assign
        pcColValues = DYNAMIC-FUNCTION('colValues':U IN h_dfiler,
                              INPUT "Innlest,FilNavn,FilId" /* CHARACTER */)
        pcFilNavn   = entry(3,pcColValues,CHR(1))
        pbOk        = IF entry(2,pcColValues,CHR(1)) = "yes"
                        THEN TRUE
                        ELSE FALSE
        plFilId    = dec(entry(4,pcColValues,CHR(1)))
        .
    IF pbOk = FALSE THEN
    DO:
        MESSAGE "Filen " pcFilNavn "er ikke innlest. Kan ikke eksporteres."
            VIEW-AS ALERT-BOX INFO BUTTONS OK TITLE "Informasjon".
        RETURN.
    END.
    ELSE DO:
        pbOk = FALSE.
        MESSAGE "Skal filen " + pcFilNavn + " eksporteres?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Bekreft"
            UPDATE pbOk.
        IF pbOk = FALSE THEN
            RETURN.
    END.
    {sww.i}
    RUN EksporterFil IN h_dfiler (INPUT plFilId, OUTPUT pbOk, OUTPUT piAntLinjer).
    {swn.i}

    /* Viser melding fra eksportrutinen. */
    IF RETURN-VALUE <> "" THEN
        MESSAGE RETURN-VALUE
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enableObject wWin 
PROCEDURE enableObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  
  /*IF VALID-HANDLE(h_ffilertoolbar) THEN*/
  SUBSCRIBE "LesInnGUI"              IN h_ffilertoolbar.
  SUBSCRIBE "ScannKataloger"         IN h_ffilertoolbar.
  SUBSCRIBE "SlettFil"               IN h_ffilertoolbar.
  SUBSCRIBE "SlettTommePoster"       IN h_ffilertoolbar.
  SUBSCRIBE "SlettFilUansett"        IN h_ffilertoolbar.
  SUBSCRIBE "EksporterFil"           IN h_ffilertoolbar.
  SUBSCRIBE "Telleverk"              IN h_ddatasett.
  SUBSCRIBE "OppdaterGUI"            IN h_ffilertoolbar. 
  SUBSCRIBE "SlettAlleData"          IN h_ffilertoolbar.
  SUBSCRIBE "OverforFilGUI"          IN h_ffilertoolbar.
  SUBSCRIBE "NotePad"                IN h_ffilertoolbar.
  SUBSCRIBE "SlettDagsRapp"          IN h_ffilertoolbar.
  SUBSCRIBE "OverforPBR"             IN h_ffilertoolbar.
  SUBSCRIBE "dfilerDataAvailable"    IN h_dfiler.

  /* Status meldinger */
  SUBSCRIBE "FilStatusTekst" ANYWHERE.
  
  /* Denne trenger av en eller annen grunn et spark bak. */
  DYNAMIC-FUNCTION('openQuery':U IN h_ddatasett).

  /* Forteller datasett om div handle. */
  RUN SetDFiler          IN h_ddatasett (INPUT h_dfiler).
  RUN SetHandleTelleverk IN h_dfiler    (INPUT THIS-PROCEDURE).
  RUN SetHandleTelleverk IN h_ddatasett (INPUT THIS-PROCEDURE).

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FilStatusTekst wWin 
PROCEDURE FilStatusTekst :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER pcStatusTekst AS CHAR NO-UNDO.

  STATUS DEFAULT pcStatusTekst.

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
  RUN OpprettKnapper.

  SUBSCRIBE TO "Print"     IN h_dyntoolbar.
  SUBSCRIBE TO "ApplHjelp" IN h_dynToolbar.
  IF VALID-HANDLE(h_dproclib) THEN
      RUN GetLng IN h_dproclib (OUTPUT wCurrLng).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LesInnBatch wWin 
PROCEDURE LesInnBatch :
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
  DEF VAR pcKeyValues      AS char NO-UNDO.
  DEF VAR cFilId           AS CHAR NO-UNDO.

  /* Leser X og X filer av gangen, til det er tomt. */
  /* Sikrer at ikke variabel sprekker.              */
  ASSIGN pbMore = TRUE.
  DO WHILE pbMore = TRUE:

      IF pcValgteFiler = "" THEN
          RUN GetAlleIkkeInnleste IN h_dfiler (INPUT 100,INPUT DECI(cFilId), OUTPUT pcValgteFiler, OUTPUT pbMore).
      ELSE
          pbMore = FALSE.

      /* Tar vare på den første posten for å reposisjonere */
      ASSIGN
          pcKeyValues = ENTRY(1,pcValgteFiler,CHR(1))
          .

      /* Leser inn de valgte filene */
      IF pcValgteFiler <> "" THEN
      DO piLoop1 = 1 TO num-entries(pcValgteFiler,chr(1)):
        /* Leser inn fil. */
        cFilId = ENTRY(piLoop1,pcValgteFiler,CHR(1)).
        RUN LesInnFil IN h_dfiler (INPUT cFilId, 
                                   OUTPUT pbOk, 
                                   OUTPUT piAntLinjer).
      END.
      ASSIGN
          pcValgteFiler = ""
          .
  END.
  RUN JournalFilAdmin IN h_dfiler.
  IF pcKeyValues <> "" THEN
  DO:
      DYNAMIC-FUNCTION('openQuery':U IN h_dfiler).
      DYNAMIC-FUNCTION('findRow':U IN h_dfiler,
         INPUT pcKeyValues /* CHARACTER */).
      RUN dataAvailable IN h_dfiler
          ( INPUT "SAME" /* CHARACTER */).
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LesInnGui wWin 
PROCEDURE LesInnGui :
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
  DEF VAR pcKeyValues      AS char NO-UNDO.
  DEF VAR piFokusRad       AS INT  NO-UNDO.
  DEF VAR cFilId           AS CHAR NO-UNDO.

  /* Henter radnummer på rad som har fokus. */
  RUN GetFocusedRow IN h_bfiler (OUTPUT piFokusRad).

  /* Henter liste med valgte filer i browser. */
  RUN GetSelectedRows IN h_bfiler (OUTPUT pcValgteFiler).

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
      MESSAGE "Der er valgt " + string(num-entries(pcValgteFiler,CHR(1))) + "." SKIP
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
          RUN GetAlleIkkeInnleste IN h_dfiler (INPUT 100,INPUT DECI(cFilId), OUTPUT pcValgteFiler, OUTPUT pbMore).
      ELSE
          pbMore = FALSE.

      /* Tar vare på den første posten for å reposisjonere */
      ASSIGN
          pcKeyValues = ENTRY(1,pcValgteFiler,CHR(1))
          .

      /* Leser inn de valgte filene */
      IF pcValgteFiler <> "" THEN
      DO piLoop1 = 1 TO num-entries(pcValgteFiler,chr(1)):
        /* Leser inn fil. */
        cFilId = ENTRY(piLoop1,pcValgteFiler,CHR(1)).
        RUN LesInnFil IN h_dfiler (INPUT cFilId, 
                                   OUTPUT pbOk, 
                                   OUTPUT piAntLinjer).
      END.
      ASSIGN
          pcValgteFiler = ""
          .
  END.
  RUN JournalFilAdmin IN h_dfiler.
  {swn.i}

  MESSAGE "Innlesning av filer ferdig."
      VIEW-AS ALERT-BOX INFO BUTTONS OK.

  IF pcKeyValues <> "" THEN
  DO:
      DYNAMIC-FUNCTION('openQuery':U IN h_dfiler).
      DYNAMIC-FUNCTION('findRow':U IN h_dfiler,
         INPUT pcKeyValues /* CHARACTER */).
      RUN dataAvailable IN h_dfiler
          ( INPUT "SAME" /* CHARACTER */).
      RUN SetBrowseFocus IN h_bfiler (STRING(piFokusRad)).
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MakulerGUI wWin 
PROCEDURE MakulerGUI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR pcValgteDatasett AS CHAR NO-UNDO.
  DEF VAR piLoop1          AS INT  NO-UNDO.
  DEF VAR pcTekst          AS CHAR NO-UNDO.
  DEF VAR pbOk             AS LOG  NO-UNDO.
  DEF VAR pbMore           AS LOG  NO-UNDO.
  DEF VAR piAntLinjer      AS INT  NO-UNDO.
  DEF VAR pcKeyValues      AS char NO-UNDO.
  DEF VAR pbOppdatert      AS LOG  NO-UNDO.
  DEF VAR piFokusRad       AS INT  NO-UNDO.
  DEF VAR pcFilId          AS CHAR NO-UNDO.
  DEF VAR pcColValues      AS CHAR NO-UNDO.

  assign
      pcColValues  = DYNAMIC-FUNCTION('colValues':U IN h_dfiler,
                            INPUT "FilId" /* CHARACTER */)
      pcFilId      = entry(2,pcColValues,CHR(1))
      .

  /* Henter radnummer på rad som har fokus. */
  RUN GetFocusedRow IN h_bdatasett (OUTPUT piFokusRad).

  /* Henter liste med valgte filer i browser. */
  RUN GetSelectedRows IN h_bdatasett (OUTPUT pcValgteDatasett).

  /* Ingen rader er valgt */
  IF pcValgteDataSett = "" THEN
  DO:
      MESSAGE "Det er ikke valgt noen datasett."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN.
  END.

  /* Filer valgt i browser. */
  MESSAGE "Der er valgt " + string(num-entries(pcValgteDatasett,CHR(1))) + " datasett." SKIP
          "Skal makulering startes?"
    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Bekreft"
    UPDATE pbOk.
  IF pbOk = FALSE THEN
      RETURN.

  {sww.i}

  DO piLoop1 = 1 TO num-entries(pcValgteDatasett,chr(1)):
      RUN SetMakulert IN h_ddatasett (INPUT entry(piLoop1,pcValgteDatasett,CHR(1))).
  END. /* FILBEHANDLING */

  {swn.i}

  MESSAGE "Makulering av datasett er ferdig."
      VIEW-AS ALERT-BOX INFO BUTTONS OK.

  RUN StartSok IN h_bdatasett.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NoneVisible wWin 
PROCEDURE NoneVisible :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN wWIn:VISIBLE = FALSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NotePad wWin 
PROCEDURE NotePad :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR plFilId     AS DEC  NO-UNDO.
  DEF VAR pcFilListe  AS CHAR NO-UNDO.
  DEF VAR pcFilNavn   AS CHAR NO-UNDO.
  DEF VAR pbOk        AS LOG  NO-UNDO.
  DEF VAR piLoop1     AS INT  NO-UNDO.
  DEF VAR pcColValues AS CHAR NO-UNDO.
  DEF VAR piFokusRad  AS INT  NO-UNDO.

  /* Henter liste med valgte filer i browser. */
  RUN GetSelectedRows IN h_bfiler (OUTPUT pcFilListe).

  /* Alle ikke innleste filer */
  IF pcFilListe <> "" THEN
  DO:
      /*
      pbOk = FALSE.
      MESSAGE "Skal valgte file(er) åpnes i NotePad?"
          VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Bekreft"
          UPDATE pbOk.
      IF pbOk = FALSE THEN
          RETURN.
      */
  END.
  ELSE DO:
      MESSAGE "Det er ikke valgt noen filer."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN.
  END.
  
  IF NUM-ENTRIES(pcFilListe,CHR(1)) <> 1 THEN
  DO:
      MESSAGE "Det er valgt mer en en fil." SKIP
              "(" pcFilListe ")" SKIP
              "Det er bare den siste valgte filen som vil bli åpnet."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
  END.

  IF pcFilListe <> "" THEN
  OPENFIL:
  DO piLoop1 = 1 TO NUM-ENTRIES(pcFilListe,CHR(1)):
      DYNAMIC-FUNCTION('findRowWhere':U IN h_dfiler,
        INPUT "FilId" /* CHARACTER */,
        INPUT entry(piLoop1,pcFilListe,chr(1)) /* CHARACTER */,
        INPUT "=" /* CHARACTER */).

      assign
          pcColValues = DYNAMIC-FUNCTION('colValues':U IN h_dfiler,
                                INPUT "FilNavn,Katalog" /* CHARACTER */)
          pcFilNavn   = entry(3,pcColValues,CHR(1)) + "\" + 
                        entry(2,pcColValues,CHR(1))
          .
      IF SEARCH(pcFilNavn) <> ? THEN
          RUN StartNotepad IN h_dproclib (pcFilNavn).
      LEAVE OPENFIL.
  END. /* OPENFIL */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppdaterBatch wWin 
PROCEDURE OppdaterBatch :
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
  DEF VAR pcKeyValues      AS char NO-UNDO.
  DEF VAR pbOppdatert      AS LOG  NO-UNDO.
  DEF VAR pbInnlest        AS LOG  NO-UNDO.
  DEF VAR cFilId           AS CHAR NO-UNDO.


  /* Leser X og X filer av gangen, til det er tomt. */
  /* Sikrer at ikke variabel sprekker.              */
  ASSIGN pbMore = TRUE.
  DO WHILE pbMore = TRUE:

      IF pcValgteFiler = "" THEN
          RUN GetAlleIkkeOppdaterte IN h_dfiler (INPUT 100,DECI(cFilId) ,OUTPUT pcValgteFiler, OUTPUT pbMore).
      ELSE
          pbMore = FALSE.

      /* Tar vare på den første posten for å reposisjonere */
      ASSIGN
          pcKeyValues = ENTRY(1,pcValgteFiler,CHR(1))
          .

      /* Leser inn de valgte filene */
      IF pcValgteFiler <> "" THEN
      FILBEHANDLING:
      DO piLoop1 = 1 TO num-entries(pcValgteFiler,chr(1)):
          cFilId = entry(piLoop1,pcValgteFiler,CHR(1)).
          RUN GetOppdatert IN h_dfiler (INPUT cFilId,OUTPUT pbOppdatert).
          RUN GetInnlest   IN h_dfiler (INPUT cFilId,OUTPUT pbInnlest).
          /* True og ? skal ikke behandles */
          IF pbOppdatert = false AND pbInnlest = TRUE THEN
              RUN OppdaterDatasettForFil (INPUT cFilId).
      END. /* FILBEHANDLING */
      ASSIGN
          pcValgteFiler = ""
          .
  END.
  IF pcKeyValues <> "" THEN
  DO:
      DYNAMIC-FUNCTION('openQuery':U IN h_dfiler).
      DYNAMIC-FUNCTION('findRow':U IN h_dfiler,
         INPUT pcKeyValues /* CHARACTER */).
      RUN dataAvailable IN h_dfiler
          ( INPUT "SAME" /* CHARACTER */).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppdaterDatasettForFil wWin 
PROCEDURE OppdaterDatasettForFil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER pcFilId   AS CHAR NO-UNDO.

  DEF VAR pcValgteDataSett AS CHAR NO-UNDO.
  DEF VAR piLoop1          AS INT  NO-UNDO.
  DEF VAR piLoop2          AS INT  NO-UNDO.
  DEF VAR pcTekst          AS CHAR NO-UNDO.
  DEF VAR pbOk             AS LOG  NO-UNDO.
  DEF VAR pbMore           AS LOG  NO-UNDO.

  ASSIGN
      pbMore = TRUE
      .

  PUBLISH "NyFilLogg" (INPUT dec(pcFilId), STRING(TODAY) + 
                       " " + 
                       STRING(TIME,"HH:MM:SS") + " " + userid("skotex") + 
                       " - Starter oppdatering av datasett for fil med filid: " + 
                       pcFilId + "." + 
                       CHR(1) + "0").

  DATASETTLOOP:
  DO WHILE pbMore = TRUE:

      /* Behandler 10 og 10 datasett */
      RUN GetDataSettForFil IN h_dfiler (INPUT pcFilId, 
                                         INPUT 2, 
                                         INPUT 10, 
                                         OUTPUT pcValgteDataSett,
                                         OUTPUT pbMore).

      PUBLISH "NyFilLogg" (INPUT dec(pcFilId), STRING(TODAY) + 
                         " " + 
                         STRING(TIME,"HH:MM:SS") + " " + userid("skotex") + 
                         " - Oppdaterer datasett: " + 
                         pcValgteDataSett + " (" + 
                         (IF pbMore = TRUE
                            THEN "Mer"
                            ELSE "Slutt") + ")." + 
                         CHR(1) + "0").

      PUBLISH "FilStatusTekst" ("Oppdaterer datasett: " + 
                                 pcValgteDataSett + " (" + 
                                 (IF pbMore = TRUE
                                    THEN "Mer"
                                    ELSE "Slutt") + ").").
                       
      DO piLoop1 = 1 TO num-entries(pcValgteDataSett,chr(1)):
          RUN OppdaterDatasett IN h_ddatasett (INPUT entry(piLoop1,pcValgteDatasett,CHR(1)), 
                                               INPUT dec(pcFilId),
                                               OUTPUT pbOk).
      END.

      /* Flagger filen oppdatert når alle datasettene er behandlet. */
      IF pbMore = FALSE THEN
      DO:
          RUN SettFilOppdatert IN h_dfiler (INPUT pcFilId).

      END.

      PUBLISH "NyFilLogg" (INPUT dec(pcFilId), STRING(TODAY) + 
                           " " + 
                           STRING(TIME,"HH:MM:SS") + " " + userid("skotex") + 
                           " - Ferdig med oppdatering av datasett for fil: " + 
                           pcFilId + ")." + 
                           CHR(1) + "8").
  END. /* DATASETTLOOP */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppdaterEttDatasettGUI wWin 
PROCEDURE OppdaterEttDatasettGUI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR pcValgteDatasett AS CHAR NO-UNDO.
  DEF VAR piLoop1          AS INT  NO-UNDO.
  DEF VAR pcTekst          AS CHAR NO-UNDO.
  DEF VAR pbOk             AS LOG  NO-UNDO.
  DEF VAR pbMore           AS LOG  NO-UNDO.
  DEF VAR piAntLinjer      AS INT  NO-UNDO.
  DEF VAR pcKeyValues      AS char NO-UNDO.
  DEF VAR pbOppdatert      AS LOG  NO-UNDO.
  DEF VAR piFokusRad       AS INT  NO-UNDO.
  DEF VAR pcFilId          AS CHAR NO-UNDO.
  DEF VAR pcColValues      AS CHAR NO-UNDO.

  assign
      pcColValues  = DYNAMIC-FUNCTION('colValues':U IN h_dfiler,
                            INPUT "FilId" /* CHARACTER */)
      pcFilId      = entry(2,pcColValues,CHR(1))
      .

  /* Henter radnummer på rad som har fokus. */
  RUN GetFocusedRow IN h_bdatasett (OUTPUT piFokusRad).

  /* Henter liste med valgte filer i browser. */
  RUN GetSelectedRows IN h_bdatasett (OUTPUT pcValgteDatasett).

  /* Alle ikke innleste filer */
  IF pcValgteDatasett = "" THEN
  DO:
      pbOk = FALSE.
      MESSAGE "Skal alle ikke oppdaterte datasett oppdateres?"
          VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Bekreft"
          UPDATE pbOk.
      IF pbOk = FALSE THEN
          RETURN.
  END.
  /* Filer valgt i browser. */
  ELSE DO:
      MESSAGE "Der er valgt " + string(num-entries(pcValgteDatasett,CHR(1))) + " datasett." SKIP
              "Skal oppdatering startes?"
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

      IF pcValgteDatasett = "" THEN
          /* Behandler 10 og 10 datasett */
          RUN GetDataSettForFil IN h_dfiler (INPUT pcFilId, 
                                             INPUT 2, 
                                             INPUT 10, 
                                             OUTPUT pcValgteDataSett,
                                             OUTPUT pbMore).
      ELSE
          pbMore = FALSE.

      /* Tar vare på den siste posten for å reposisjonere */
      ASSIGN
          pcKeyValues = ENTRY(1,pcValgteDatasett,CHR(1))
          .

      /* Leser inn de valgte filene */
      IF pcValgteDatasett <> "" THEN
      FILBEHANDLING:
      DO piLoop1 = 1 TO num-entries(pcValgteDatasett,chr(1)):
          RUN GetOppdatert IN h_ddatasett (INPUT entry(piLoop1,pcValgteDatasett,CHR(1)),OUTPUT pbOppdatert).
          /* True og ? skal ikke behandles */
          IF pbOppdatert = false THEN
              RUN OppdaterDatasett IN h_ddatasett (INPUT entry(piLoop1,pcValgteDatasett,CHR(1)), 
                                                   INPUT dec(pcFilId),
                                                   OUTPUT pbOk).
      END. /* FILBEHANDLING */
      ASSIGN
          pcValgteDatasett = ""
          .
  END.
  {swn.i}

  MESSAGE "Oppdatering av datasett er ferdig."
      VIEW-AS ALERT-BOX INFO BUTTONS OK.

  IF pcKeyValues <> "" THEN
  DO:
      DYNAMIC-FUNCTION('openQuery':U IN h_ddatasett).
      DYNAMIC-FUNCTION('findRow':U IN h_ddatasett,
         INPUT pcKeyValues /* CHARACTER */).
      RUN dataAvailable IN h_ddatasett
          ( INPUT "SAME" /* CHARACTER */).
      RUN SetBrowseFocus IN h_bdatasett (STRING(piFokusRad)).
  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppdaterGUI wWin 
PROCEDURE OppdaterGUI :
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
  DEF VAR pcKeyValues      AS char NO-UNDO.
  DEF VAR pbOppdatert      AS LOG  NO-UNDO.
  DEF VAR piFokusRad       AS INT  NO-UNDO.
  DEF VAR pbInnlest        AS LOG  NO-UNDO.
  DEF VAR cFilId           AS CHAR NO-UNDO.

  /* Henter radnummer på rad som har fokus. */
  RUN GetFocusedRow IN h_bfiler (OUTPUT piFokusRad).

  /* Henter liste med valgte filer i browser. */
  RUN GetSelectedRows IN h_bfiler (OUTPUT pcValgteFiler).

  /* Alle ikke innleste filer */
  IF pcValgteFiler = "" THEN
  DO:
      pbOk = FALSE.
      MESSAGE "Skal alle ikke oppdaterte filer oppdateres?"
          VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Bekreft"
          UPDATE pbOk.
      IF pbOk = FALSE THEN
          RETURN.
  END.
  /* Filer valgt i browser. */
  ELSE DO:
      MESSAGE "Der er valgt " + string(num-entries(pcValgteFiler,CHR(1))) + "." SKIP
              "Skal oppdatering startes?"
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
          RUN GetAlleIkkeOppdaterte IN h_dfiler (INPUT 100,DECI(cFilId) ,OUTPUT pcValgteFiler, OUTPUT pbMore).
      ELSE
          pbMore = FALSE.

      /* Tar vare på den første posten for å reposisjonere */
      ASSIGN
          pcKeyValues = ENTRY(1,pcValgteFiler,CHR(1))
          .

      /* Leser inn de valgte filene */
      IF pcValgteFiler <> "" THEN
      FILBEHANDLING:
      DO piLoop1 = 1 TO num-entries(pcValgteFiler,chr(1)):
          cFilId = entry(piLoop1,pcValgteFiler,CHR(1)).
          RUN GetOppdatert IN h_dfiler (INPUT cFilId,OUTPUT pbOppdatert).
          RUN GetInnlest   IN h_dfiler (INPUT cFilId,OUTPUT pbInnlest).
          /* True og ? skal ikke behandles */
          IF pbOppdatert = false AND pbInnlest = TRUE THEN
              RUN OppdaterDatasettForFil (INPUT cFilId).
      END. /* FILBEHANDLING */
      ASSIGN
          pcValgteFiler = ""
          .
  END.
  {swn.i}

  MESSAGE "Oppdatering av filer ferdig."
      VIEW-AS ALERT-BOX INFO BUTTONS OK.

  IF pcKeyValues <> "" THEN
  DO:
      DYNAMIC-FUNCTION('openQuery':U IN h_dfiler).
      DYNAMIC-FUNCTION('findRow':U IN h_dfiler,
         INPUT pcKeyValues /* CHARACTER */).
      RUN dataAvailable IN h_dfiler
          ( INPUT "SAME" /* CHARACTER */).
      RUN SetBrowseFocus IN h_bfiler (STRING(piFokusRad)).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OverforDatasettForFil wWin 
PROCEDURE OverforDatasettForFil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER pcFilId   AS CHAR NO-UNDO.

  DEF VAR pcValgteDataSett AS CHAR NO-UNDO.
  DEF VAR piLoop1          AS INT  NO-UNDO.
  DEF VAR piLoop2          AS INT  NO-UNDO.
  DEF VAR pcTekst          AS CHAR NO-UNDO.
  DEF VAR pbOk             AS LOG  NO-UNDO.
  DEF VAR pbMore           AS LOG  NO-UNDO.

  ASSIGN
      pbMore = TRUE
      .

  PUBLISH "NyFilLogg" (INPUT dec(pcFilId), STRING(TODAY) + 
                       " " + 
                       STRING(TIME,"HH:MM:SS") + " " + userid("skotex") + 
                       " - Starter overføring av datasett for fil: " + 
                       pcFilId + ")." + 
                       CHR(1) + "1").

  DATASETTLOOP:
  DO WHILE pbMore = TRUE:

      /* Behandler 10 og 10 datasett */
      RUN GetDataSettForFil IN h_dfiler (INPUT pcFilId, 
                                         INPUT 3, 
                                         INPUT 10, 
                                         OUTPUT pcValgteDataSett,
                                         OUTPUT pbMore).
      PUBLISH "NyFilLogg" (INPUT dec(pcFilId), STRING(TODAY) + 
                         " " + 
                         STRING(TIME,"HH:MM:SS") + " " + userid("skotex") + 
                         " - Overfører datasett: " + 
                         pcValgteDataSett + " (" + 
                         (IF pbMore = TRUE
                            THEN "Mer"
                            ELSE "Slutt") + ")." + 
                         CHR(1) + "1").

      PUBLISH "FilStatusTekst" ("Overfører datasett: " + 
                                 pcValgteDataSett + " (" + 
                                 (IF pbMore = TRUE
                                    THEN "Mer"
                                    ELSE "Slutt") + ").").
                       
      DO piLoop1 = 1 TO num-entries(pcValgteDataSett,chr(1)):
          RUN OverforDatasett IN h_ddatasett (INPUT entry(piLoop1,pcValgteDatasett,CHR(1)), 
                                               INPUT dec(pcFilId),
                                               OUTPUT pbOk).
      END.

      /* Flagger filen oppdatert når alle datasettene er behandlet. */
      IF pbMore = FALSE THEN
          RUN SetFilOverfort IN h_dfiler (INPUT pcFilId).

      PUBLISH "NyFilLogg" (INPUT dec(pcFilId), STRING(TODAY) + 
                           " " + 
                           STRING(TIME,"HH:MM:SS") + " " + userid("skotex") + 
                           " - Ferdig med overføring av datasett for fil: " + 
                           pcFilId + ")." + 
                           CHR(1) + "8").
  END. /* DATASETTLOOP */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OverforDatasettGUI wWin 
PROCEDURE OverforDatasettGUI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR pcValgteDatasett AS CHAR NO-UNDO.
  DEF VAR piLoop1          AS INT  NO-UNDO.
  DEF VAR pcTekst          AS CHAR NO-UNDO.
  DEF VAR pbOk             AS LOG  NO-UNDO.
  DEF VAR pbMore           AS LOG  NO-UNDO.
  DEF VAR piAntLinjer      AS INT  NO-UNDO.
  DEF VAR pcKeyValues      AS char NO-UNDO.
  DEF VAR pbOVerfort       AS LOG  NO-UNDO.
  DEF VAR piFokusRad       AS INT  NO-UNDO.
  DEF VAR pcFilId          AS CHAR NO-UNDO.
  DEF VAR pcColValues      AS CHAR NO-UNDO.

  assign
      pcColValues  = DYNAMIC-FUNCTION('colValues':U IN h_dfiler,
                            INPUT "FilId" /* CHARACTER */)
      pcFilId      = entry(2,pcColValues,CHR(1))
      .

  /* Henter radnummer på rad som har fokus. */
  RUN GetFocusedRow IN h_bdatasett (OUTPUT piFokusRad).

  /* Henter liste med valgte filer i browser. */
  RUN GetSelectedRows IN h_bdatasett (OUTPUT pcValgteDatasett).

  /* Alle ikke innleste filer */
  IF pcValgteDatasett = "" THEN
  DO:
      pbOk = FALSE.
      MESSAGE "Skal alle ikke overførte datasett overføres?"
          VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Bekreft"
          UPDATE pbOk.
      IF pbOk = FALSE THEN
          RETURN.
  END.
  /* Filer valgt i browser. */
  ELSE DO:
      MESSAGE "Der er valgt " + string(num-entries(pcValgteDatasett,CHR(1))) + " datasett." SKIP
              "Skal overføring startes?"
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

      IF pcValgteDatasett = "" THEN
          /* Behandler 10 og 10 datasett */
          RUN GetDataSettForFil IN h_dfiler (INPUT pcFilId, 
                                             INPUT 2, 
                                             INPUT 10, 
                                             OUTPUT pcValgteDataSett,
                                             OUTPUT pbMore).
      ELSE
          pbMore = FALSE.

      /* Tar vare på den siste posten for å reposisjonere */
      ASSIGN
          pcKeyValues = ENTRY(1,pcValgteDatasett,CHR(1))
          .

      /* Leser inn de valgte filene */
      IF pcValgteDatasett <> "" THEN
      FILBEHANDLING:
      DO piLoop1 = 1 TO num-entries(pcValgteDatasett,chr(1)):
          RUN GetOverfort IN h_ddatasett (INPUT entry(piLoop1,pcValgteDatasett,CHR(1)),OUTPUT pbOverfort).
          /* True og ? skal ikke behandles */
          IF pbOverfort = false THEN
              RUN OverforDatasett IN h_ddatasett (INPUT entry(piLoop1,pcValgteDatasett,CHR(1)), 
                                                   INPUT dec(pcFilId),
                                                   OUTPUT pbOk).
      END. /* FILBEHANDLING */
      ASSIGN
          pcValgteDatasett = ""
          .
  END.
  {swn.i}

  MESSAGE "Overføring av datasett er ferdig."
      VIEW-AS ALERT-BOX INFO BUTTONS OK.

  IF pcKeyValues <> "" THEN
  DO:
      DYNAMIC-FUNCTION('openQuery':U IN h_ddatasett).
      DYNAMIC-FUNCTION('findRow':U IN h_ddatasett,
         INPUT pcKeyValues /* CHARACTER */).
      RUN dataAvailable IN h_ddatasett
          ( INPUT "SAME" /* CHARACTER */).
      RUN SetBrowseFocus IN h_bdatasett (STRING(piFokusRad)).
  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OverforFilBatch wWin 
PROCEDURE OverforFilBatch :
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
  DEF VAR pcKeyValues      AS char NO-UNDO.
  DEF VAR pbOverfort       AS LOG  NO-UNDO.
  DEF VAR piFokusRad       AS INT  NO-UNDO.


  /* Leser X og X filer av gangen, til det er tomt. */
  /* Sikrer at ikke variabel sprekker.              */
  ASSIGN pbMore = TRUE.
  DO WHILE pbMore = TRUE:

      IF pcValgteFiler = "" THEN
          RUN GetAlleIkkeOverforte IN h_dfiler (INPUT 100, OUTPUT pcValgteFiler, OUTPUT pbMore).
      ELSE
          pbMore = FALSE.

      /* Tar vare på den første posten for å reposisjonere */
      ASSIGN
          pcKeyValues = ENTRY(1,pcValgteFiler,CHR(1))
          .

      /* Leser inn de valgte filene */
      IF pcValgteFiler <> "" THEN
      FILBEHANDLING:
      DO piLoop1 = 1 TO num-entries(pcValgteFiler,chr(1)):
          RUN GetOverfort IN h_dfiler (INPUT entry(piLoop1,pcValgteFiler,CHR(1)),OUTPUT pbOVerfort).
          /* True og ? skal ikke behandles */
          IF pbOverfort = false THEN
              RUN OverforDatasettForFil (INPUT entry(piLoop1,pcValgteFiler,CHR(1))).
      END. /* FILBEHANDLING */
      ASSIGN
          pcValgteFiler = ""
          .
  END.
  IF pcKeyValues <> "" THEN
  DO:
      DYNAMIC-FUNCTION('openQuery':U IN h_dfiler).
      DYNAMIC-FUNCTION('findRow':U IN h_dfiler,
         INPUT pcKeyValues /* CHARACTER */).
      RUN dataAvailable IN h_dfiler
          ( INPUT "SAME" /* CHARACTER */).
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OverforFilGUI wWin 
PROCEDURE OverforFilGUI :
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
  DEF VAR pcKeyValues      AS char NO-UNDO.
  DEF VAR pbOverfort       AS LOG  NO-UNDO.
  DEF VAR piFokusRad       AS INT  NO-UNDO.

  /* Henter radnummer på rad som har fokus. */
  RUN GetFocusedRow IN h_bfiler (OUTPUT piFokusRad).

  /* Henter liste med valgte filer i browser. */
  RUN GetSelectedRows IN h_bfiler (OUTPUT pcValgteFiler).

  /* Alle ikke innleste filer */
  IF pcValgteFiler = "" THEN
  DO:
      pbOk = FALSE.
      MESSAGE "Skal alle ikke oppdaterte filer overføres?"
          VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Bekreft"
          UPDATE pbOk.
      IF pbOk = FALSE THEN
          RETURN.
  END.
  /* Filer valgt i browser. */
  ELSE DO:
      MESSAGE "Der er valgt " + string(num-entries(pcValgteFiler,CHR(1))) + "." SKIP
              "Skal overføring startes?"
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
          RUN GetAlleIkkeOverforte IN h_dfiler (INPUT 100, OUTPUT pcValgteFiler, OUTPUT pbMore).
      ELSE
          pbMore = FALSE.

      /* Tar vare på den første posten for å reposisjonere */
      ASSIGN
          pcKeyValues = ENTRY(1,pcValgteFiler,CHR(1))
          .

      /* Leser inn de valgte filene */
      IF pcValgteFiler <> "" THEN
      FILBEHANDLING:
      DO piLoop1 = 1 TO num-entries(pcValgteFiler,chr(1)):
          RUN GetOverfort IN h_dfiler (INPUT entry(piLoop1,pcValgteFiler,CHR(1)),OUTPUT pbOVerfort).
          /* True og ? skal ikke behandles */
          IF pbOverfort = false THEN
              RUN OverforDatasettForFil (INPUT entry(piLoop1,pcValgteFiler,CHR(1))).
      END. /* FILBEHANDLING */
      ASSIGN
          pcValgteFiler = ""
          .
  END.
  {swn.i}

  MESSAGE "Overføring av filer ferdig."
      VIEW-AS ALERT-BOX INFO BUTTONS OK.

  IF pcKeyValues <> "" THEN
  DO:
      DYNAMIC-FUNCTION('openQuery':U IN h_dfiler).
      DYNAMIC-FUNCTION('findRow':U IN h_dfiler,
         INPUT pcKeyValues /* CHARACTER */).
      RUN dataAvailable IN h_dfiler
          ( INPUT "SAME" /* CHARACTER */).
      RUN SetBrowseFocus IN h_bfiler (STRING(piFokusRad)).
  END.
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
    
    RUN pfxoppdatstat.p.

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
  RUN bibl_logg.p ('DataMottak', 'wfiler.w ScannKataloger: Starter. Starter GetKatalogListe.').
  RUN GetKatalogListe IN h_dfiler (OUTPUT cKataloger).

  /* 2. Bygg en liste med alle filnavn + ekstent som skal kontrolleres. */
  RUN bibl_logg.p ('DataMottak', 'wfiler.w ScannKataloger: Starter. Starter GetFilNavnListe.').
  RUN GetFilNavnListe IN h_dfiler (OUTPUT cFilNavnListe).
  
  /* 3. Rens bort alle poster i fillisten som ikke er innlest. */
  RUN bibl_logg.p ('DataMottak', 'wfiler.w ScannKataloger: Starter. Starter RensTommePoster.').
  RUN RensTommePoster IN h_dfiler (INPUT ""). /* Alle ikke innleste */

  /* 4. Opprett en post i fillisten for alle filer som ikke finnes der. */
  RUN bibl_logg.p ('DataMottak', 'wfiler.w ScannKataloger: Starter. Starter OpprettPoster.').
  RUN OpprettPoster IN h_dfiler (INPUT cKataloger, INPUT cFilNavnListe). 

 /* 5. OpenQuery for fillisten. */ 
  RUN bibl_logg.p ('DataMottak', 'wfiler.w ScannKataloger: Starter. Starter openQuery.').
  DYNAMIC-FUNCTION('openQuery':U IN h_dfiler).
  RUN SetBrowseFocus IN h_bfiler (?).
  {swn.i}

  RUN bibl_logg.p ('DataMottak', 'wfiler.w ScannKataloger: Starter. Ferdig.').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettAlleData wWin 
PROCEDURE SlettAlleData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF VAR pbOk AS LOG NO-UNDO.

  ASSIGN
      pbOk = FALSE
      .
  MESSAGE "Skal ALLE data fra kassene slettes?" 
      VIEW-AS ALERT-BOX QUESTION  BUTTONS YES-NO TITLE "Bekreft"
      UPDATE pbOk.
  IF pbOk <> TRUE THEN
      RETURN.
  ASSIGN
      pbOk = FALSE
      .
  MESSAGE "Er du dønn dønn dønn sikker på at du virkelig vil slette ALLE data fra kassene???????" 
      VIEW-AS ALERT-BOX QUESTION  BUTTONS YES-NO TITLE "Bekreft"
      UPDATE pbOk.
  IF pbOk <> TRUE THEN
      RETURN.


FOR EACH Filer:
    DELETE Filer.
END.
FOR EACH fillinjer:
    DELETE FilLinjer.
END.
FOR EACH DataSett:
    DELETE DataSett.

END.
FOR EACH FilLogg:
    DELETE filLogg.
END.
FOR EACH BongHode:
    FOR EACH BongLinje WHERE
        BongLinje.B_Id = BongHode.B_Id:
        DELETE BongLinje.
    END.
    DELETE BongHode.
END.

RUN StartSok IN h_bfiler.

MESSAGE "Ja det var det!"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettDagsRapp wWin 
PROCEDURE SlettDagsRapp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN slettsalgsdata.w.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettDatasett wWin 
PROCEDURE SlettDatasett :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DEF VAR pcValgteDatasett AS CHAR NO-UNDO.
  DEF VAR piLoop1          AS INT  NO-UNDO.
  DEF VAR pcTekst          AS CHAR NO-UNDO.
  DEF VAR pbOk             AS LOG  NO-UNDO.
  DEF VAR pbMore           AS LOG  NO-UNDO.
  DEF VAR piAntLinjer      AS INT  NO-UNDO.
  DEF VAR pcKeyValues      AS char NO-UNDO.
  DEF VAR pbOppdatert      AS LOG  NO-UNDO.
  DEF VAR piFokusRad       AS INT  NO-UNDO.
  DEF VAR pcFilId          AS CHAR NO-UNDO.
  DEF VAR pcColValues      AS CHAR NO-UNDO.

  assign
      pcColValues  = DYNAMIC-FUNCTION('colValues':U IN h_dfiler,
                            INPUT "FilId" /* CHARACTER */)
      pcFilId      = entry(2,pcColValues,CHR(1))
      .

  /* Henter radnummer på rad som har fokus. */
  RUN GetFocusedRow IN h_bdatasett (OUTPUT piFokusRad).

  /* Henter liste med valgte filer i browser. */
  RUN GetSelectedRows IN h_bdatasett (OUTPUT pcValgteDatasett).

  /* Ingen rader er valgt */
  IF pcValgteDataSett = "" THEN
  DO:
      MESSAGE "Det er ikke valgt noen datasett."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN.
  END.

  /* Filer valgt i browser. */
  MESSAGE "Der er valgt " + string(num-entries(pcValgteDatasett,CHR(1))) + " datasett." SKIP
          "Skal sletting startes?" SKIP(1)
          "Kun datasett med behandlingsstatus MAKULERT vil bli slettes."
    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Bekreft"
    UPDATE pbOk.
  IF pbOk = FALSE THEN
      RETURN.

  {sww.i}

  DO piLoop1 = 1 TO num-entries(pcValgteDatasett,chr(1)):
      RUN SlettDatasett IN h_ddatasett (INPUT entry(piLoop1,pcValgteDatasett,CHR(1)), OUTPUT pbOk).
  END. /* FILBEHANDLING */

  {swn.i}

  MESSAGE "Sletting av datasett er ferdig."
      VIEW-AS ALERT-BOX INFO BUTTONS OK.

  RUN StartSok IN h_bdatasett.
  
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
  DEF VAR pbOk        AS   LOG         NO-UNDO.
  DEF VAR pcFilNavn   AS   CHAR        NO-UNDO.
  DEF VAR pcColValues AS   CHAR        NO-UNDO.
  DEF VAR plFilId     LIKE Filer.FilId NO-UNDO.
  DEF VAR pbSlettet   AS   LOG         NO-UNDO.

  /* 1. Sjekk om det er en filPost tilgjengelig i query. */
  assign
      pbOk = DYNAMIC-FUNCTION('rowAvailable':U IN h_dfiler,
               INPUT "CURRENT" /* CHARACTER */).

  /* 2. Er det en post tilgjengelig, sett innlestflagget i viewer og */
  /* 3. lagre endringer i post.                                      */
  IF pbOk THEN
  DO:
    assign
        pcColValues = DYNAMIC-FUNCTION('colValues':U IN h_dfiler,
                              INPUT "Innlest,FilNavn,FilId,Slettet" /* CHARACTER */)
        pcFilNavn   = entry(3,pcColValues,CHR(1))
        pbOk        = IF entry(2,pcColValues,CHR(1)) = "yes"
                        THEN TRUE
                        ELSE FALSE
        plFilId    = dec(entry(4,pcColValues,CHR(1)))
        pbSlettet  = IF entry(5,pcColValues,CHR(1)) = "yes"
                        THEN TRUE
                        ELSE FALSE
        .
    IF pbSlettet THEN
    DO:
        MESSAGE "Filen " pcFilNavn " er allerede slettet." SKIP
            VIEW-AS ALERT-BOX INFO BUTTONS OK TITLE "Informasjon".
        RETURN.
    END.
    IF pbOk THEN
    DO:
        MESSAGE "Filen " pcFilNavn "er allerede innlest." SKIP
                "Innleste filer kan ikke slettes, men alle transaksjonslinjene" SKIP
                "vil bli slettet."
            VIEW-AS ALERT-BOX INFO BUTTONS OK TITLE "Informasjon".
    END.

    pbOk = FALSE.
    MESSAGE "Skal filen " + pcFilNavn + " slettes?"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Bekreft"
        UPDATE pbOk.
    IF pbOk = FALSE THEN
        RETURN.
    {sww.i}
    RUN SlettFil IN h_dfiler (INPUT plFilId, INPUT FALSE, OUTPUT pbOk).
    {swn.i}
    IF pbOk THEN
    DO: 
        RUN refreshRow IN h_dfiler.
        RUN SetSlettet   IN h_vfiler.
        RUN updateRecord IN h_vfiler.
        DYNAMIC-FUNCTION('openQuery':U IN h_dfillogg).
        DYNAMIC-FUNCTION('openQuery':U IN h_dfillinjer).
        DYNAMIC-FUNCTION('openQuery':U IN h_dfiler).
    END.
    ELSE MESSAGE RETURN-VALUE
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettFilUansett wWin 
PROCEDURE SlettFilUansett :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR pbOk        AS   LOG         NO-UNDO.
  DEF VAR pcFilNavn   AS   CHAR        NO-UNDO.
  DEF VAR pcColValues AS   CHAR        NO-UNDO.
  DEF VAR plFilId     LIKE Filer.FilId NO-UNDO.
  DEF VAR pbSlettet   AS   LOG         NO-UNDO.

  /* 1. Sjekk om det er en filPost tilgjengelig i query. */
  assign
      pbOk = DYNAMIC-FUNCTION('rowAvailable':U IN h_dfiler,
               INPUT "CURRENT" /* CHARACTER */).

  /* 2. Er det en post tilgjengelig, sett innlestflagget i viewer og */
  /* 3. lagre endringer i post.                                      */
  IF pbOk THEN
  DO:
    assign
        pcColValues = DYNAMIC-FUNCTION('colValues':U IN h_dfiler,
                              INPUT "Innlest,FilNavn,FilId,Slettet" /* CHARACTER */)
        pcFilNavn   = entry(3,pcColValues,CHR(1))
        pbOk        = IF entry(2,pcColValues,CHR(1)) = "yes"
                        THEN TRUE
                        ELSE FALSE
        plFilId    = dec(entry(4,pcColValues,CHR(1)))
        pbSlettet  = IF entry(5,pcColValues,CHR(1)) = "yes"
                        THEN TRUE
                        ELSE FALSE
        .
    pbOk = FALSE.
    MESSAGE "Skal filen " + pcFilNavn + "slettes uansett?" SKIP
            "Bonger som tilhører filen blir også slettet."
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Bekreft"
        UPDATE pbOk.
    IF pbOk = FALSE THEN
        RETURN.
    {sww.i}
    RUN SlettFil IN h_dfiler (INPUT plFilId, INPUT TRUE, OUTPUT pbOk).
    {swn.i}
    IF pbOk THEN
    DO:
        DYNAMIC-FUNCTION('openQuery':U IN h_dfiler).
    END.
    ELSE MESSAGE RETURN-VALUE
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettTommePoster wWin 
PROCEDURE SlettTommePoster :
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

  RUN RensTommePoster IN h_dfiler (INPUT "").
  DYNAMIC-FUNCTION('openQuery':U IN h_dfiler).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Telleverk wWin 
PROCEDURE Telleverk :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER pcTekst AS CHAR NO-UNDO.


  STATUS DEFAULT pcTekst.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE viewObject wWin 
PROCEDURE viewObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

