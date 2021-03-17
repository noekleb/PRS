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
DEF VAR hBrowse             AS HANDLE NO-UNDO.
DEF VAR ix                  AS INT    NO-UNDO.
DEF VAR bOk                 AS LOG    NO-UNDO.
DEF VAR iReturn             AS INT    NO-UNDO.

DEFINE VARIABLE hRapport        AS HANDLE     NO-UNDO.
DEFINE VARIABLE hwkassereroppgj AS HANDLE     NO-UNDO.
DEFINE VARIABLE hRapportPlus    AS HANDLE     NO-UNDO.

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
DEFINE VARIABLE h_bbokforingsbilag AS HANDLE NO-UNDO.
DEFINE VARIABLE h_beodkasse AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dbokforingsbilag AS HANDLE NO-UNDO.
DEFINE VARIABLE h_deodkasse AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar AS HANDLE NO-UNDO.
DEFINE VARIABLE h_ffilterbokforingsbilag AS HANDLE NO-UNDO.
DEFINE VARIABLE h_sortsok AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vbokforingsbilagi AS HANDLE NO-UNDO.

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 179.2 BY 28.86.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Godkjenning av dagsoppgjør"
         HEIGHT             = 28.86
         WIDTH              = 179.2
         MAX-HEIGHT         = 28.86
         MAX-WIDTH          = 179.2
         VIRTUAL-HEIGHT     = 28.86
         VIRTUAL-WIDTH      = 179.2
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
{incl/devmode.i}
{incl/custdevmode.i}
{hjelp.i}
{incl/wintrigg.i}

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
ON END-ERROR OF wWin /* Godkjenning av dagsoppgjør */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Godkjenning av dagsoppgjør */
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
             INPUT  'sdo/dbokforingsbilag.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch50CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedbokforingsbilagUpdateFromSourcenoToggleDataTargetsyesOpenOnInityesPromptOnDeleteyesPromptColumns(NONE)':U ,
             OUTPUT h_dbokforingsbilag ).
       RUN repositionObject IN h_dbokforingsbilag ( 25.52 , 161.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'prg/vbokforingsbilagi.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EnabledObjFldsToDisable(None)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vbokforingsbilagi ).
       RUN repositionObject IN h_vbokforingsbilagi ( 5.29 , 145.00 ) NO-ERROR.
       /* Size in AB:  ( 14.24 , 35.00 ) */

       RUN constructObject (
             INPUT  'prg/bbokforingsbilag.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bbokforingsbilag ).
       RUN repositionObject IN h_bbokforingsbilag ( 6.00 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_bbokforingsbilag ( 23.57 , 143.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'sdo/deodkasse.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsEODKasse.ButikkNr,ButikkNr,EODKasse.EODDato,OmsetningsDatoRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedeodkasseUpdateFromSourcenoToggleDataTargetsyesOpenOnInityesPromptOnDeleteyesPromptColumns(NONE)':U ,
             OUTPUT h_deodkasse ).
       RUN repositionObject IN h_deodkasse ( 18.62 , 153.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'prg/beodkasse.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_beodkasse ).
       RUN repositionObject IN h_beodkasse ( 19.81 , 145.00 ) NO-ERROR.
       RUN resizeObject IN h_beodkasse ( 9.76 , 35.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsTableio,NavigationSubModulesTableIOTypeSaveSupportedLinksNavigation-source,Tableio-sourceToolbarBandsToolbarParentMenuToolbarAutoSizenoToolbarDrawDirectionHorizontalToolbarInitialStateLogicalObjectNameAutoResizeDisabledActionsAdd,Copy,DeleteHiddenActionsUpdate,ResetHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0EdgePixels2PanelTypeToolbarDeactivateTargetOnHidenoDisabledActionsAdd,Copy,DeleteNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar ).
       RUN repositionObject IN h_dyntoolbar ( 1.00 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar ( 1.24 , 179.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'prg/ffilterbokforingsbilag.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'LogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_ffilterbokforingsbilag ).
       RUN repositionObject IN h_ffilterbokforingsbilag ( 2.43 , 30.00 ) NO-ERROR.
       /* Size in AB:  ( 3.48 , 149.80 ) */

       RUN constructObject (
             INPUT  'prg/sortsok.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_sortsok ).
       RUN repositionObject IN h_sortsok ( 3.86 , 1.00 ) NO-ERROR.
       /* Size in AB:  ( 1.76 , 27.60 ) */

       /* Links to SmartDataObject h_dbokforingsbilag. */
       RUN addLink ( h_dyntoolbar , 'Navigation':U , h_dbokforingsbilag ).
       RUN addLink ( h_ffilterbokforingsbilag , 'SokSdo':U , h_dbokforingsbilag ).

       /* Links to SmartDataViewer h_vbokforingsbilagi. */
       RUN addLink ( h_dbokforingsbilag , 'Data':U , h_vbokforingsbilagi ).
       RUN addLink ( h_vbokforingsbilagi , 'Update':U , h_dbokforingsbilag ).
       RUN addLink ( h_dyntoolbar , 'TableIo':U , h_vbokforingsbilagi ).

       /* Links to SmartDataBrowser h_bbokforingsbilag. */
       RUN addLink ( h_dbokforingsbilag , 'Data':U , h_bbokforingsbilag ).

       /* Links to SmartDataObject h_deodkasse. */
       RUN addLink ( h_dbokforingsbilag , 'Data':U , h_deodkasse ).

       /* Links to SmartDataBrowser h_beodkasse. */
       RUN addLink ( h_deodkasse , 'Data':U , h_beodkasse ).
       RUN addLink ( h_beodkasse , 'Update':U , h_deodkasse ).

       /* Links to SmartObject h_sortsok. */
       RUN addLink ( h_bbokforingsbilag , 'Sortera':U , h_sortsok ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_ffilterbokforingsbilag ,
             h_dyntoolbar , 'AFTER':U ).
       RUN adjustTabOrder ( h_sortsok ,
             h_ffilterbokforingsbilag , 'AFTER':U ).
       RUN adjustTabOrder ( h_vbokforingsbilagi ,
             h_sortsok , 'AFTER':U ).
       RUN adjustTabOrder ( h_bbokforingsbilag ,
             h_vbokforingsbilagi , 'AFTER':U ).
       RUN adjustTabOrder ( h_beodkasse ,
             h_bbokforingsbilag , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Aktivitetsrapport wWin 
PROCEDURE Aktivitetsrapport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN w-aktivitetsrapport.w.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE destroyObject wWin 
PROCEDURE destroyObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  
  IF VALID-HANDLE(hwkassereroppgj) THEN
      DELETE PROCEDURE hwkassereroppgj.
  IF VALID-HANDLE(hRapportPlus) THEN
      DELETE PROCEDURE hRapportPlus.
  IF VALID-HANDLE(hRapport) THEN
      DELETE PROCEDURE hRapport.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EksportBokfBilag wWin 
PROCEDURE EksportBokfBilag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR liLoop          AS INT  NO-UNDO.
  DEF VAR lcColValues     AS CHAR NO-UNDO.
  DEF VAR llArtikkelNr    AS DEC  NO-UNDO.
  DEF VAR lcRecordState   AS CHAR NO-UNDO.
  DEF VAR lcQueryPos      AS CHAR NO-UNDO.


/*   /* Sjekker om det er noen vare i utvalget. */      */
/*   {get QueryPosition lcQueryPos h_bbokforingsbilag}. */
/*   IF lcQueryPos = 'NoRecordAvailable' THEN           */
/*       RETURN.                                        */

  iReturn = 0.
  RUN JBoxBrowseSelectMsg.w ("Bekreft eksport av bokføringsbilag (Kun godkjente bilag eksporteres).",
                              hBrowse:NUM-SELECTED-ROWS,
                              ?, /*STRING(DYNAMIC-FUNCTION("getRecordCount" IN h_bbokforingsbilag)),*/
                              OUTPUT iReturn).
  IF iReturn = 0 THEN RETURN.

  /*
  IF hBrowse:NUM-SELECTED-ROWS > 0 AND iReturn = 2 THEN 
  DO:
    DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
      IF hBrowse:FETCH-SELECTED-ROW(ix) THEN 
        cArtNrList = cArtNrList +
                          STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) + ",". 
    END.
    IF NOT DYNAMIC-FUNCTION("RunProc","art_sett_produsent.p",
                            cValues + ",ARTNUM," + TRIM(cArtNrList,","),
                            ?) THEN 
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Advarsel","").
  END.
  ELSE IF NOT DYNAMIC-FUNCTION("RunProc","art_sett_produsent.p",cValues,DYNAMIC-FUNCTION("getTmpArtBasHandle" IN h_dtmpartbas)) THEN 
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Advarsel","").
  */
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetTransDato wWin 
PROCEDURE GetTransDato :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER iButikkNr  AS INTEGER    NO-UNDO.
  DEFINE OUTPUT PARAMETER dTransDato AS DATE       NO-UNDO.
  DEFINE VARIABLE pcColValues        AS CHARACTER  NO-UNDO.

      pcColValues = DYNAMIC-FUNCTION('colValues':U IN h_dbokforingsbilag,
                    INPUT "ButikkNr,OmsetningsDato").
      ASSIGN iButikkNr  = INT(ENTRY(2,pcColValues,CHR(1)))
             dTransdato = DATE(ENTRY(3,pcColValues,CHR(1))).
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
  PUBLISH "Sortera" FROM h_bbokforingsbilag.
  IF VALID-HANDLE(h_dyntoolbar) THEN DO:
      RUN OpprettKnapper.
   END.

  SUBSCRIBE TO "ApplHjelp"         IN h_dynToolbar.
  SUBSCRIBE TO "GetTransDato"      ANYWHERE.
  SUBSCRIBE TO "Kassereroppgjor"   IN h_dyntoolbar.
  SUBSCRIBE TO "Translogg"         IN h_dyntoolbar.
  SUBSCRIBE TO "PrintBF"           IN h_dyntoolbar. /* Printer direkt */
  SUBSCRIBE TO "PrintBFPV"         IN h_dyntoolbar. /* Preview */
  SUBSCRIBE TO "PrintFR"           IN h_dyntoolbar. /* Printer direkt */
  SUBSCRIBE TO "PrintFRPV"         IN h_dyntoolbar. /* Preview */
  SUBSCRIBE TO "PrintBR"           IN h_dyntoolbar. /* Printer direkt */
  SUBSCRIBE TO "PrintBRPV"         IN h_dyntoolbar. /* Preview */
  SUBSCRIBE TO "Perioderapport"    IN h_dyntoolbar. /* Preview */
  SUBSCRIBE TO "Manedsrapport"     IN h_dyntoolbar. /* Preview */
  SUBSCRIBE TO "Aktivitetsrapport" IN h_dyntoolbar. /* Utvalg */

  hBrowse = DYNAMIC-FUNCTION("getBrowseHandle" IN h_bbokforingsbilag).

  MENY-HOYREKLIKK:
  DO:
    DYNAMIC-FUNCTION("NewMenuBand",
                      hBrowse,  /* parent widget */
                      "EksportBokfBilag;Eksport av bokføringsbilag;EksportBokfBilag"
                      ,
                      "").     
  
  END. /* MENY-HOYREKLIKK */
  
  DYNAMIC-FUNCTION("InitTranslation",THIS-PROCEDURE:CURRENT-WINDOW).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Kassereroppgjor wWin 
PROCEDURE Kassereroppgjor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR pcColValues AS CHAR NO-UNDO.  
                                     
  pcColValues = DYNAMIC-FUNCTION('colValues':U IN h_dbokforingsbilag,
                INPUT "ButikkNr,OmsetningsDato").

  RUN wkassereroppgj.w PERSISTENT SET hwkassereroppgj.
  RUN ButikkDag IN hwkassereroppgj (INPUT pcColValues).
  RUN initializeObject IN hwkassereroppgj.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Manedsrapport wWin 
PROCEDURE Manedsrapport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN manedsrapport.w.
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
   
   ASSIGN piX = piX + 4
       hButton = DYNAMIC-FUNCTION('createButton':U IN h_dyntoolbar,
         INPUT hFrame,
         INPUT-OUTPUT piX                 /* INTEGER */,
         INPUT "Translogg"                  /* CHARACTER */,
         INPUT "Translogg" /* CHARACTER Knapptext*/,
         INPUT "Translogg"  /* CHARACTER */,
         INPUT "" /* CHARACTER */,
/*          INPUT "icon\ht.bmp"  /* CHARACTER */, */
         INPUT TRUE              /* LOGICAL */).

  DYNAMIC-FUNCTION('defineAction':U IN h_dyntoolbar,
     INPUT "Translogg":U /* CHARACTER */,
     INPUT "Name,Caption,Image,Type,OnChoose,AccessType,Parent" /* CHARACTER */,
     INPUT "Translogg"      {&dlmt}
          "Translogg"      {&dlmt}
          "":U        {&dlmt}
          "PUBLISH":U {&dlmt}
          "Translogg":U   {&dlmt}
          "":U        {&dlmt}
          "":U).

/*----------------------*/
  ASSIGN piX = piX + 4
      hButton = DYNAMIC-FUNCTION('createButton':U IN h_dyntoolbar,
        INPUT hFrame,
        INPUT-OUTPUT piX                 /* INTEGER */,
        INPUT "Kassereroppgjor"          /* CHARACTER */,
        INPUT "Kassereroppgjør"          /* CHARACTER Knapptext*/,
        INPUT "Kassereroppgjor"          /* CHARACTER */,
        INPUT ""                         /* CHARACTER */,
/*          INPUT "icon\ht.bmp"  /* CHARACTER */, */
        INPUT TRUE              /* LOGICAL */).

 DYNAMIC-FUNCTION('defineAction':U IN h_dyntoolbar,
    INPUT "Kassereroppgjor":U /* CHARACTER */,
    INPUT "Name,Caption,Image,Type,OnChoose,AccessType,Parent" /* CHARACTER */,
    INPUT "Kassereroppgjor"      {&dlmt}
         "Kassereroppgjor"      {&dlmt}
         "":U        {&dlmt}
         "PUBLISH":U {&dlmt}
         "Kassereroppgjor":U   {&dlmt}
         "":U        {&dlmt}
         "":U).
/*----------------------*/

   /* Printbuttons */
   ASSIGN  piX = piX + 4 /* lägger till 'luft' */
           hButton = DYNAMIC-FUNCTION('createButton':U IN h_dyntoolbar,
              INPUT hFrame,
              INPUT-OUTPUT piX           /* INTEGER   */,
              INPUT "PrintBF"              /* CHARACTER */,
              INPUT "PrintBF"              /* CHARACTER */,
              INPUT "Bokføringsbilag"            /* CHARACTER */,
              INPUT "icon\e-print.bmp"   /* CHARACTER */,
              INPUT TRUE                 /* LOGICAL   */
              ).

   DYNAMIC-FUNCTION('defineAction':U IN h_dyntoolbar,
      INPUT "PrintBF":U /* CHARACTER */,
      INPUT "Name,Caption,Image,Link,Type,OnChoose,Parent" /* CHARACTER */,
      INPUT "PrintBF"             {&dlmt}
            "PrintBF"      {&dlmt}
            "":U                {&dlmt}
            "":U                {&dlmt}
            "PUBLISH":U         {&dlmt}
            "PRINTBF":U           {&dlmt}
            "Options":U).

   ASSIGN  piX = piX + 4 /* lägger till 'luft' */
           hButton = DYNAMIC-FUNCTION('createButton':U IN h_dyntoolbar,
              INPUT hFrame,
              INPUT-OUTPUT piX           /* INTEGER   */,
              INPUT "PrintBFPV"              /* CHARACTER */,
              INPUT "PrintBFPV"              /* CHARACTER */,
              INPUT "Vis bokføringsbilag"            /* CHARACTER */,
              INPUT "icon\e-vis.bmp"   /* CHARACTER */,
              INPUT TRUE                 /* LOGICAL   */
              ).

   DYNAMIC-FUNCTION('defineAction':U IN h_dyntoolbar,
      INPUT "PrintBFPV":U /* CHARACTER */,
      INPUT "Name,Caption,Image,Link,Type,OnChoose,Parent" /* CHARACTER */,
      INPUT "PrintBFPV"             {&dlmt}
            "Print Record"      {&dlmt}
            "":U                {&dlmt}
            "":U                {&dlmt}
            "PUBLISH":U         {&dlmt}
            "PRINTBFPV":U           {&dlmt}
            "Options":U).
   
   ASSIGN  piX = piX + 4 /* lägger till 'luft' */
           hButton = DYNAMIC-FUNCTION('createButton':U IN h_dyntoolbar,
              INPUT hFrame,
              INPUT-OUTPUT piX           /* INTEGER   */,
              INPUT "PrintFR"              /* CHARACTER */,
              INPUT "PrintFR"              /* CHARACTER */,
              INPUT "Finansrapport"            /* CHARACTER */,
              INPUT "icon\e-print.bmp"   /* CHARACTER */,
              INPUT TRUE                 /* LOGICAL   */
              ).

   DYNAMIC-FUNCTION('defineAction':U IN h_dyntoolbar,
      INPUT "PrintFR":U /* CHARACTER */,
      INPUT "Name,Caption,Image,Link,Type,OnChoose,Parent" /* CHARACTER */,
      INPUT "PrintFR"             {&dlmt}
            "PrintFR"      {&dlmt}
            "":U                {&dlmt}
            "":U                {&dlmt}
            "PUBLISH":U         {&dlmt}
            "PRINTFR":U           {&dlmt}
            "Options":U).

   ASSIGN  piX = piX + 4 /* lägger till 'luft' */
           hButton = DYNAMIC-FUNCTION('createButton':U IN h_dyntoolbar,
              INPUT hFrame,
              INPUT-OUTPUT piX           /* INTEGER   */,
              INPUT "PrintFRPV"              /* CHARACTER */,
              INPUT "PrintFRPV"              /* CHARACTER */,
              INPUT "Vis finansrapport"            /* CHARACTER */,
              INPUT "icon\e-vis.bmp"   /* CHARACTER */,
              INPUT TRUE                 /* LOGICAL   */
              ).

   DYNAMIC-FUNCTION('defineAction':U IN h_dyntoolbar,
      INPUT "PrintFRPV":U /* CHARACTER */,
      INPUT "Name,Caption,Image,Link,Type,OnChoose,Parent" /* CHARACTER */,
      INPUT "PrintFRPV"             {&dlmt}
            "Print Record"      {&dlmt}
            "":U                {&dlmt}
            "":U                {&dlmt}
            "PUBLISH":U         {&dlmt}
            "PRINTFRPV":U           {&dlmt}
            "Options":U).
 
/* ---------------- Bongrapport */
   ASSIGN  piX = piX + 4 /* lägger till 'luft' */
           hButton = DYNAMIC-FUNCTION('createButton':U IN h_dyntoolbar,
              INPUT hFrame,
              INPUT-OUTPUT piX           /* INTEGER   */,
              INPUT "PrintBR"              /* CHARACTER */,
              INPUT "PrintBR"              /* CHARACTER */,
              INPUT "Bongrapport"            /* CHARACTER */,
              INPUT "icon\e-print.bmp"   /* CHARACTER */,
              INPUT TRUE                 /* LOGICAL   */
              ).

   DYNAMIC-FUNCTION('defineAction':U IN h_dyntoolbar,
      INPUT "PrintBR":U /* CHARACTER */,
      INPUT "Name,Caption,Image,Link,Type,OnChoose,Parent" /* CHARACTER */,
      INPUT "PrintBR"             {&dlmt}
            "PrintBR"      {&dlmt}
            "":U                {&dlmt}
            "":U                {&dlmt}
            "PUBLISH":U         {&dlmt}
            "PRINTBR":U           {&dlmt}
            "Options":U).

   ASSIGN  piX = piX + 4 /* lägger till 'luft' */
           hButton = DYNAMIC-FUNCTION('createButton':U IN h_dyntoolbar,
              INPUT hFrame,
              INPUT-OUTPUT piX           /* INTEGER   */,
              INPUT "PrintBRPV"              /* CHARACTER */,
              INPUT "PrintBRPV"              /* CHARACTER */,
              INPUT "Vis bongrapport"            /* CHARACTER */,
              INPUT "icon\e-vis.bmp"   /* CHARACTER */,
              INPUT TRUE                 /* LOGICAL   */
              ).

   DYNAMIC-FUNCTION('defineAction':U IN h_dyntoolbar,
      INPUT "PrintBRPV":U /* CHARACTER */,
      INPUT "Name,Caption,Image,Link,Type,OnChoose,Parent" /* CHARACTER */,
      INPUT "PrintBRPV"             {&dlmt}
            "Print Record"      {&dlmt}
            "":U                {&dlmt}
            "":U                {&dlmt}
            "PUBLISH":U         {&dlmt}
            "PRINTBRPV":U           {&dlmt}
            "Options":U).
/* ---------------- Bongrapport slut */

/* ------------ periodrapport  */

   ASSIGN piX = piX + 4
       hButton = DYNAMIC-FUNCTION('createButton':U IN h_dyntoolbar,
         INPUT hFrame,
         INPUT-OUTPUT piX                 /* INTEGER */,
         INPUT "Perioderapport"                  /* CHARACTER */,
         INPUT "Perioderapport" /* CHARACTER Knapptext*/,
         INPUT "Perioderapport"  /* CHARACTER */,
         INPUT "" /* CHARACTER */,
/*          INPUT "icon\ht.bmp"  /* CHARACTER */, */
         INPUT TRUE              /* LOGICAL */).

  DYNAMIC-FUNCTION('defineAction':U IN h_dyntoolbar,
     INPUT "Perioderapport":U /* CHARACTER */,
     INPUT "Name,Caption,Image,Type,OnChoose,AccessType,Parent" /* CHARACTER */,
     INPUT "Perioderapport"      {&dlmt}
          "Perioderapport"      {&dlmt}
          "":U        {&dlmt}
          "PUBLISH":U {&dlmt}
          "Perioderapport":U   {&dlmt}
          "":U        {&dlmt}
          "":U).


/* ------------ Periodrapport slut  */

/* ------------ Månedsrapport  */

   ASSIGN piX = piX + 4
       hButton = DYNAMIC-FUNCTION('createButton':U IN h_dyntoolbar,
         INPUT hFrame,
         INPUT-OUTPUT piX                 /* INTEGER */,
         INPUT "Manedsrapport"                  /* CHARACTER */,
         INPUT "Månedsrapport" /* CHARACTER Knapptext*/,
         INPUT "Manedsrapport"  /* CHARACTER */,
         INPUT "" /* CHARACTER */,
/*          INPUT "icon\ht.bmp"  /* CHARACTER */, */
         INPUT TRUE              /* LOGICAL */).

  DYNAMIC-FUNCTION('defineAction':U IN h_dyntoolbar,
     INPUT "Manedsrapport":U /* CHARACTER */,
     INPUT "Name,Caption,Image,Type,OnChoose,AccessType,Parent" /* CHARACTER */,
     INPUT "Manedsrapport"      {&dlmt}
          "Månedsrapport"      {&dlmt}
          "":U        {&dlmt}
          "PUBLISH":U {&dlmt}
          "Manedsrapport":U   {&dlmt}
          "":U        {&dlmt}
          "":U).


/* ------------ Månedsrapport slut  */

/* ------------ Aktivitetsrapport  w-aktivitetsrapport.w */

   ASSIGN piX = piX + 4
       hButton = DYNAMIC-FUNCTION('createButton':U IN h_dyntoolbar,
         INPUT hFrame,
         INPUT-OUTPUT piX                 /* INTEGER */,
         INPUT "Aktivitetsrapport"                  /* CHARACTER */,
         INPUT "Aktivitetsrapport" /* CHARACTER Knapptext*/,
         INPUT "Aktivitetsrapport"  /* CHARACTER */,
         INPUT "" /* CHARACTER */,
/*          INPUT "icon\ht.bmp"  /* CHARACTER */, */
         INPUT TRUE              /* LOGICAL */).

  DYNAMIC-FUNCTION('defineAction':U IN h_dyntoolbar,
     INPUT "Aktivitetsrapport":U /* CHARACTER */,
     INPUT "Name,Caption,Image,Type,OnChoose,AccessType,Parent" /* CHARACTER */,
     INPUT "Aktivitetsrapport"      {&dlmt}
          "Aktivitetsrapport"      {&dlmt}
          "":U        {&dlmt}
          "PUBLISH":U {&dlmt}
          "Aktivitetsrapport":U   {&dlmt}
          "":U        {&dlmt}
          "":U).


/* ------------ Månedsrapport slut  */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Perioderapport wWin 
PROCEDURE Perioderapport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN w-rkassarapportx.w.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Print wWin 
PROCEDURE Print PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER cType AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iButikkNr LIKE Butiker.Butik    NO-UNDO.
  DEFINE VARIABLE dDato     AS DATE       NO-UNDO.
  DEFINE VARIABLE iRapptype AS INTEGER  INIT ?  NO-UNDO.
  IF CAN-DO("BF,BFPV,FR,FRPV",cType) THEN DO:
      CASE cType:
          WHEN "BF" THEN
              ASSIGN iRappType = 21.
          WHEN "BFPV" THEN
              ASSIGN iRappType = 1.
          WHEN "FR" THEN
              ASSIGN iRappType = 20.
          WHEN "FRPV" THEN
              ASSIGN iRappType = 0.
      END CASE.
    /*   RUN d-VelgRapptypeFiBo.w (OUTPUT iRappType). */
    /*   IF iRapptype = ? THEN                        */
    /*       RETURN NO-APPLY.                         */
      ASSIGN iButikkNr = INT(DYNAMIC-FUNCTION('columnValue':U IN h_dbokforingsbilag,
         INPUT "ButikkNr" /* CHARACTER */))
             dDato = DATE(DYNAMIC-FUNCTION('columnValue':U IN h_dbokforingsbilag,
         INPUT "OmsetningsDato" /* CHARACTER */)).

      RUN w-rkassarapportx.w PERSISTENT SET hRapport.
      RUN AutoInit IN hRapport (iRappType,iButikkNr,dDato).
  END.
  ELSE IF CAN-DO("BR,BRPV",cType) THEN DO:
      ASSIGN iButikkNr = INT(DYNAMIC-FUNCTION('columnValue':U IN h_dbokforingsbilag,
         INPUT "ButikkNr" /* CHARACTER */))
             dDato = DATE(DYNAMIC-FUNCTION('columnValue':U IN h_dbokforingsbilag,
         INPUT "OmsetningsDato" /* CHARACTER */)).
      /* (Butik,dato,batch,direkte) */
      RUN skrivbongrap.p (iButikkNr,dDato,FALSE,cType = "BR").
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrintBF wWin 
PROCEDURE PrintBF :
/*------------------------------------------------------------------------------
  Purpose:    Print bokföringsbilag direkte 
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN Print ("BF").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrintBFPV wWin 
PROCEDURE PrintBFPV :
/*------------------------------------------------------------------------------
  Purpose:     Preview bokföringsbilag
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN Print ("BFPV").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrintBR wWin 
PROCEDURE PrintBR :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN Print ("BR").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrintBRPV wWin 
PROCEDURE PrintBRPV :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN Print ("BRPV").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrintFR wWin 
PROCEDURE PrintFR :
/*------------------------------------------------------------------------------
  Purpose:     Print bokföringsbilag direkte
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN Print ("FR").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrintFRPV wWin 
PROCEDURE PrintFRPV :
/*------------------------------------------------------------------------------
  Purpose:     Finansrapport preview
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN Print ("FRPV").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Translogg wWin 
PROCEDURE Translogg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR pcColValues AS CHAR NO-UNDO.
/*                                                                       */
  pcColValues = DYNAMIC-FUNCTION('colValues':U IN h_dbokforingsbilag,
                INPUT "ButikkNr,OmsetningsDato").
  RUN wRapportPlus.w PERSISTENT SET hRapportPlus.
/*   MESSAGE VALID-HANDLE(hRapportGen)             */
/*       VIEW-AS ALERT-BOX INFO BUTTONS OK.        */
  IF VALID-HANDLE(hRapportPlus) THEN
      RUN Translogg IN hRapportPlus (INPUT pcColValues).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

