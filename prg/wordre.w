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
DEFINE VAR hAvskriv AS HANDLE NO-UNDO.
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
DEFINE VARIABLE h_bbesthode AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bordre AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dbesthode AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dordre AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_fordre AS HANDLE NO-UNDO.
DEFINE VARIABLE h_fordrebestbuttons AS HANDLE NO-UNDO.
DEFINE VARIABLE h_sortsok AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vordre AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vordresmall AS HANDLE NO-UNDO.

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     SPACE(160.01) SKIP(26.44)
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
         TITLE              = "Ordre"
         HEIGHT             = 26.57
         WIDTH              = 160
         MAX-HEIGHT         = 26.57
         MAX-WIDTH          = 160
         VIRTUAL-HEIGHT     = 26.57
         VIRTUAL-WIDTH      = 160
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
ON END-ERROR OF wWin /* Ordre */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Ordre */
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
{lng.i &SDO="SDO"}

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
             INPUT  'sdo/dordre.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessnoDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedordreOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dordre ).
       RUN repositionObject IN h_dordre ( 3.14 , 138.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'prg/vordresmall.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EnabledObjFldsToDisable(None)ModifyFields(All)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vordresmall ).
       RUN repositionObject IN h_vordresmall ( 2.43 , 2.00 ) NO-ERROR.
       /* Size in AB:  ( 4.05 , 158.00 ) */

       RUN constructObject (
             INPUT  'sdo/dbesthode.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessnoDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsBestHode.OrdreNr,OrdreNrObjectNamedbesthodeOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dbesthode ).
       RUN repositionObject IN h_dbesthode ( 3.14 , 127.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EdgePixels2DeactivateTargetOnHidenoDisabledActionsFlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsTableio,NavigationTableIOTypeSaveSupportedLinksNavigation-source,Tableio-sourceToolbarBandsToolbarAutoSizenoToolbarDrawDirectionhorizontalLogicalObjectNameDisabledActionsHiddenActionsUpdate,ResetHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0RemoveMenuOnHidenoCreateSubMenuOnConflictyesNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar ).
       RUN repositionObject IN h_dyntoolbar ( 1.00 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar ( 1.24 , 160.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/folder.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FolderLabels':U + 'Ordre|Detaljer|Bestillinger' + 'FolderTabWidth0FolderFont-1HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_folder ).
       RUN repositionObject IN h_folder ( 6.48 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_folder ( 20.95 , 159.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN initPages ('1':U) NO-ERROR.

       /* Links to SmartDataObject h_dordre. */
       RUN addLink ( h_dyntoolbar , 'Navigation':U , h_dordre ).
       RUN addLink ( h_fordre , 'SokSdo':U , h_dordre ).

       /* Links to SmartDataViewer h_vordresmall. */
       RUN addLink ( h_dordre , 'Data':U , h_vordresmall ).

       /* Links to SmartDataObject h_dbesthode. */
       RUN addLink ( h_dordre , 'Data':U , h_dbesthode ).

       /* Links to SmartFolder h_folder. */
       RUN addLink ( h_folder , 'Page':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_vordresmall ,
             h_dyntoolbar , 'AFTER':U ).
       RUN adjustTabOrder ( h_folder ,
             h_vordresmall , 'AFTER':U ).
    END. /* Page 0 */
    WHEN 1 THEN DO:
       RUN constructObject (
             INPUT  'prg/bordre.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bordre ).
       RUN repositionObject IN h_bordre ( 13.38 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_bordre ( 13.33 , 157.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'prg/fordre.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'LogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_fordre ).
       RUN repositionObject IN h_fordre ( 7.91 , 30.20 ) NO-ERROR.
       /* Size in AB:  ( 5.48 , 129.20 ) */

       RUN constructObject (
             INPUT  'prg/sortsok.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_sortsok ).
       RUN repositionObject IN h_sortsok ( 11.48 , 2.00 ) NO-ERROR.
       /* Size in AB:  ( 1.76 , 27.60 ) */

       /* Links to SmartDataBrowser h_bordre. */
       RUN addLink ( h_dordre , 'Data':U , h_bordre ).

       /* Links to SmartObject h_sortsok. */
       RUN addLink ( h_bordre , 'Sortera':U , h_sortsok ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_fordre ,
             h_folder , 'AFTER':U ).
       RUN adjustTabOrder ( h_sortsok ,
             h_fordre , 'AFTER':U ).
       RUN adjustTabOrder ( h_bordre ,
             h_sortsok , 'AFTER':U ).
    END. /* Page 1 */
    WHEN 2 THEN DO:
       RUN constructObject (
             INPUT  'prg/vordre.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EnabledObjFldsToDisable(None)ModifyFields(All)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vordre ).
       RUN repositionObject IN h_vordre ( 8.38 , 5.00 ) NO-ERROR.
       /* Size in AB:  ( 17.57 , 150.00 ) */

       /* Links to SmartDataViewer h_vordre. */
       RUN addLink ( h_dordre , 'Data':U , h_vordre ).
       RUN addLink ( h_vordre , 'Update':U , h_dordre ).
       RUN addLink ( h_dyntoolbar , 'TableIO':U , h_vordre ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_vordre ,
             h_folder , 'AFTER':U ).
    END. /* Page 2 */
    WHEN 3 THEN DO:
       RUN constructObject (
             INPUT  'prg/bbesthode.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bbesthode ).
       RUN repositionObject IN h_bbesthode ( 8.38 , 5.00 ) NO-ERROR.
       RUN resizeObject IN h_bbesthode ( 15.71 , 153.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'prg/fordrebestbuttons.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'LogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_fordrebestbuttons ).
       RUN repositionObject IN h_fordrebestbuttons ( 25.05 , 5.00 ) NO-ERROR.
       /* Size in AB:  ( 1.33 , 29.20 ) */

       /* Links to SmartDataBrowser h_bbesthode. */
       RUN addLink ( h_dbesthode , 'Data':U , h_bbesthode ).

       /* Links to SmartFrame h_fordrebestbuttons. */
       RUN addLink ( h_dordre , 'Data':U , h_fordrebestbuttons ).
       RUN addLink ( h_folder , 'Page':U , h_fordrebestbuttons ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_bbesthode ,
             h_folder , 'AFTER':U ).
       RUN adjustTabOrder ( h_fordrebestbuttons ,
             h_bbesthode , 'AFTER':U ).
    END. /* Page 3 */

  END CASE.
  /* Select a Startup page. */
  IF currentPage eq 0
  THEN RUN selectPage IN THIS-PROCEDURE ( 1 ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AvskrivObject wWin 
PROCEDURE AvskrivObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cObject AS CHARACTER INIT "Avskriv" NO-UNDO.
/*   DEFINE INPUT  PARAMETER cObject AS CHARACTER  NO-UNDO. */
  DEFINE VARIABLE cOrdreNr AS CHARACTER  NO-UNDO.
  DEF VAR bOk AS LOG NO-UNDO.

  IF cObject = "Avskriv" THEN DO:
      ASSIGN cOrdreNr = DYNAMIC-FUNCTION('columnValue':U IN h_dordre,
                      INPUT "OrdreNr" /* CHARACTER */).
      
      IF INT(cOrdreNr) > 0 THEN
      DO:
          FIND Ordre NO-LOCK WHERE 
              Ordre.OrdreNr = INT(cOrdreNr) NO-ERROR.
          IF NOT AVAILABLE Ordre THEN
              RETURN.
          IF Ordre.OrdreStatus = 6 THEN
              RETURN.
          MESSAGE 'Slal ordre avskrives?'
              VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
              UPDATE bOk.
          IF bOk THEN
          DO TRANSACTION:
              FOR EACH BestHode NO-LOCK WHERE
                BestHode.OrdreNr = INT(cOrdreNr):
                RUN besthode_avskriv_rest.p (BestHode.BestNr).
              END.
              FIND CURRENT Ordre EXCLUSIVE-LOCK.
              ASSIGN
                  Ordre.OrdreStatus = 6.
              RELEASE Ordre.

              RUN refreshBrowse IN h_bordre.
          END. /* TRANSACTION */
      END.
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
  DEFINE VARIABLE plCancel AS LOGICAL    NO-UNDO.
  IF DYNAMIC-FUNCTION('getCurrentPage':U) = 2 AND VALID-HANDLE(h_vordre) AND
     DYNAMIC-FUNCTION('getDataModified':U IN h_vordre)  THEN
      RUN confirmExit (INPUT-OUTPUT plCancel /* LOGICAL */).
  IF plCancel THEN
      RETURN.
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
  IF VALID-HANDLE(h_dordre) THEN
      DYNAMIC-FUNCTION('setQueryWhere':U IN h_dordre,
     INPUT "FALSE" /* CHARACTER */).
  RUN SUPER.
  IF VALID-HANDLE(h_dordre) THEN
      DYNAMIC-FUNCTION('setQueryWhere':U IN h_dordre,
     INPUT "" /* CHARACTER */).

  /* Code placed here will execute AFTER standard behavior.    */
  IF VALID-HANDLE(h_dyntoolbar) THEN DO:
      RUN skapaButtons.
   END.
   
   SUBSCRIBE TO "ApplHjelp" IN h_dynToolbar.
  DYNAMIC-FUNCTION('setDisabledActions':U IN h_dyntoolbar,
  INPUT "Copy" /* CHARACTER */).
/*   IF VALID-HANDLE(h_dproclib) THEN              */
/*     RUN GetLng IN h_dproclib (OUTPUT wCurrLng). */
  SUBSCRIBE TO "printObject" IN h_dyntoolbar.
  SUBSCRIBE TO "AvskrivObject" IN h_dyntoolbar.
  SUBSCRIBE TO "OpenQueryBest" ANYWHERE.
  RUN Sortera IN h_sortsok NO-ERROR.
/*   PUBLISH "Sortera" FROM h_dordre. */
/*   RUN fetchFirst IN h_dordre. */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenQueryBest wWin 
PROCEDURE OpenQueryBest :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DYNAMIC-FUNCTION('openQuery':U IN h_dbesthode).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE printObject wWin 
PROCEDURE printObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cObject AS CHARACTER INIT "Ordre" NO-UNDO.
/*   DEFINE INPUT  PARAMETER cObject AS CHARACTER  NO-UNDO. */
  DEFINE VARIABLE cOrdreNr AS CHARACTER  NO-UNDO.
  IF cObject = "Ordre" THEN DO:
      ASSIGN cOrdreNr = DYNAMIC-FUNCTION('columnValue':U IN h_dordre,
                      INPUT "OrdreNr" /* CHARACTER */).
      IF INT(cOrdreNr) > 0 THEN
          RUN d-ordreutskriftX.w ("",cOrdreNr).
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
  
  /* Code placed here will execute PRIOR to standard behavior. */
  
  IF DYNAMIC-FUNCTION('getCurrentPage':U) = 2 AND VALID-HANDLE(h_vordre) AND
     DYNAMIC-FUNCTION('getDataModified':U IN h_vordre)  THEN
      RUN confirmExit (INPUT-OUTPUT plCancel /* LOGICAL */).
  IF plCancel THEN
      RETURN NO-APPLY.
  IF piPageNum = 2 THEN
      DYNAMIC-FUNCTION('setDisabledActions':U IN h_dyntoolbar,
        INPUT "Copy" /* CHARACTER */).

  RUN SUPER( INPUT piPageNum).
  
  /* Code placed here will execute AFTER standard behavior.    */
  
  IF DYNAMIC-FUNCTION('getCurrentPage':U) <> 2 THEN DO:
      DYNAMIC-FUNCTION('setDisabledActions':U IN h_dyntoolbar,
          INPUT "Add,Copy,Delete" /* CHARACTER */).
  END.
  ELSE
      RUN dataAvailable IN h_vordre ("SAME").
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
/*            INPUT "Print:Ordre" /* CHARACTER */, */
           INPUT "Print" /* CHARACTER */,
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
           "":U  + CHR(1) +
           "PUBLISH":U  + CHR(1) +
           "printObject":U + CHR(1) +
           "Options":U).


   ASSIGN piX = 285
           hAvskriv = DYNAMIC-FUNCTION('createButton':U IN h_dyntoolbar,
           DYNAMIC-FUNCTION('getContainerHandle':U IN h_dyntoolbar),
 /*            INPUT FRAME {&FRAME-NAME}:HANDLE /* HANDLE */, */
            INPUT-OUTPUT piX /* INTEGER */,
 /*            INPUT "Print:Ordre" /* CHARACTER */, */
            INPUT "Avskriv" /* CHARACTER */,
            INPUT "Avskriv" /* CHARACTER */,
            INPUT "Avskriv ordre" /* CHARACTER */,
            INPUT "icon\delete.bmp" /* pcBitmap CHARACTER */,
            INPUT TRUE /* LOGICAL */).
 /*    hPrint:MOVE-TO-TOP(). */
    DYNAMIC-FUNCTION('defineAction':U IN h_dyntoolbar,
      INPUT "Avskriv":U /* CHARACTER */,
      INPUT "Name,Caption,Image,Link,Type,OnChoose,Parent" /* CHARACTER */,
      INPUT "Avskriv" + CHR(1) +
            "Avskriv Record" + CHR(1) +
            "":U  + CHR(1) +
            "":U  + CHR(1) +
            "PUBLISH":U  + CHR(1) +
            "AvskrivObject":U + CHR(1) +
            "Options":U).

   ASSIGN piX = FRAME {&FRAME-NAME}:WIDTH-PIXELS - hAvskriv:WIDTH-PIXELS - 3
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

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

