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
    DEFINE VARIABLE iButFra     AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iButTil     AS INTEGER    NO-UNDO.
    DEFINE VARIABLE dFraDato    AS DATE       NO-UNDO.
    DEFINE VARIABLE dTilDato    AS DATE       NO-UNDO.
    DEFINE VARIABLE cEAN        AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE lAktuell    AS LOGICAL    NO-UNDO.
    DEFINE VARIABLE iSortering  AS INTEGER    NO-UNDO.

DEFINE TEMP-TABLE TT_Priskontroll LIKE Priskontroll
    FIELD sumDiffPris AS DECIMAL
    FIELD sumDiffVVK  AS DECIMAL
    FIELD sumDiffDB   AS DECIMAL.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getButListe wWin 
FUNCTION getButListe RETURNS CHARACTER
  ( INPUT iFraBut AS INTEGER, INPUT iTilBut AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_bpriskontroll AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dpriskontroll AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar AS HANDLE NO-UNDO.
DEFINE VARIABLE h_fpriskontrollfilter AS HANDLE NO-UNDO.

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     SPACE(156.01) SKIP(25.72)
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1 SCROLLABLE .


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
         TITLE              = "Priskontroll"
         HEIGHT             = 24.86
         WIDTH              = 156.6
         MAX-HEIGHT         = 26.19
         MAX-WIDTH          = 157
         VIRTUAL-HEIGHT     = 26.19
         VIRTUAL-WIDTH      = 157
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME fMain
   Size-to-Fit                                                          */
ASSIGN 
       FRAME fMain:SCROLLABLE       = FALSE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Priskontroll */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Priskontroll */
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
             INPUT  'sdo/dpriskontroll.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedpriskontrollUpdateFromSourcenoToggleDataTargetsyesOpenOnInityesPromptOnDeleteyesPromptColumns(NONE)':U ,
             OUTPUT h_dpriskontroll ).
       RUN repositionObject IN h_dpriskontroll ( 2.43 , 145.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'prg/bpriskontroll.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bpriskontroll ).
       RUN repositionObject IN h_bpriskontroll ( 7.19 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_bpriskontroll ( 19.52 , 154.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FlatButtonsyesMenuyesShowBorderyesToolbaryesActionGroupsNavigationSubModulesTableIOTypeSupportedLinksNavigation-sourceToolbarBandsToolbarParentMenuToolbarAutoSizenoToolbarDrawDirectionHorizontalToolbarInitialStateLogicalObjectNameAutoResizeDisabledActionsHiddenActionsUpdateHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0EdgePixels2PanelTypeToolbarDeactivateTargetOnHidenoDisabledActionsNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar ).
       RUN repositionObject IN h_dyntoolbar ( 1.00 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar ( 1.24 , 156.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'prg/fpriskontrollfilter.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'LogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_fpriskontrollfilter ).
       RUN repositionObject IN h_fpriskontrollfilter ( 2.67 , 2.00 ) NO-ERROR.
       /* Size in AB:  ( 4.14 , 51.00 ) */

       /* Links to SmartDataObject h_dpriskontroll. */
       RUN addLink ( h_dyntoolbar , 'Navigation':U , h_dpriskontroll ).
       RUN addLink ( h_fpriskontrollfilter , 'SokSdo':U , h_dpriskontroll ).

       /* Links to SmartDataBrowser h_bpriskontroll. */
       RUN addLink ( h_dpriskontroll , 'Data':U , h_bpriskontroll ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_fpriskontrollfilter ,
             h_dyntoolbar , 'AFTER':U ).
       RUN adjustTabOrder ( h_bpriskontroll ,
             h_fpriskontrollfilter , 'AFTER':U ).
    END. /* Page 0 */

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
      RUN OpprettKnapper.
   END.
   SUBSCRIBE TO "Printrapp"    IN h_dyntoolbar.

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
   DEFINE VARIABLE hHandle  AS HANDLE     NO-UNDO.
   DEFINE VARIABLE hPrint  AS HANDLE     NO-UNDO.
   DEFINE VARIABLE hExit   AS HANDLE     NO-UNDO.
   DEFINE VARIABLE hHelp   AS HANDLE     NO-UNDO.
   DEFINE VARIABLE piX     AS INTEGER    NO-UNDO.
  /* Printbutton */
   ASSIGN hFrame  = DYNAMIC-FUNCTION('getContainerHandle':U IN h_dyntoolbar)
          hHandle = hFrame
          hHandle = hHandle:FIRST-CHILD  /* första field-group          */
          hHandle = hHandle:FIRST-CHILD. /* första widget i field-group */
   REPEAT WHILE VALID-HANDLE(hHandle):
       /* hämtar X-pos för sista 'rulen' i toolbaren */
       IF hHandle:TYPE = "RECTANGLE" AND hHandle:X > piX THEN
           ASSIGN piX  = hHandle:X.
       ASSIGN hHandle = hHandle:NEXT-SIBLING.
   END.
  ASSIGN piX = piX + 4 
         hPrint = DYNAMIC-FUNCTION('createButton':U IN h_dyntoolbar,
           INPUT FRAME {&FRAME-NAME}:HANDLE /* HANDLE */,
           INPUT-OUTPUT piX /* INTEGER */,
           INPUT "Print" /* CHARACTER */,
           INPUT "Print" /* CHARACTER */,
           INPUT "Rapport" /* CHARACTER */,
           INPUT "icon\e-print.bmp" /* pcBitmap CHARACTER */,
           INPUT TRUE /* LOGICAL */).
   hPrint:MOVE-TO-TOP().
   DYNAMIC-FUNCTION('defineAction':U IN h_dyntoolbar,
     INPUT "Print":U /* CHARACTER */,
     INPUT "Name,Caption,Image,Type,OnChoose,Parent" /* CHARACTER */,
     INPUT "Print" + CHR(1) +
           "Print Record" + CHR(1) +
           "":U  + CHR(1) +
           "PUBLISH":U  + CHR(1) +
           "Printrapp":U + CHR(1) +
           "Options":U).

   ASSIGN piX = FRAME {&FRAME-NAME}:WIDTH-PIXELS - hPrint:WIDTH-PIXELS - 3
          hExit = DYNAMIC-FUNCTION('createButton':U IN h_dyntoolbar,
           INPUT FRAME {&FRAME-NAME}:HANDLE /* HANDLE */,
           INPUT-OUTPUT piX /* INTEGER */,
           INPUT "exit" /* CHARACTER */,
           INPUT "exit" /* CHARACTER */,
           INPUT "exit" /* CHARACTER */,
           INPUT "icon\e-exit.bmp" /* pcBitmap CHARACTER */,
           INPUT TRUE /* LOGICAL */).
   hExit:MOVE-TO-TOP().

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
           INPUT FRAME {&FRAME-NAME}:HANDLE /* HANDLE */,
           INPUT-OUTPUT piX /* INTEGER */,
           INPUT "HELP" /* CHARACTER */,
           INPUT "HELP" /* CHARACTER */,
           INPUT "HELP" /* CHARACTER */,
           INPUT "icon\e-help.bmp" /* pcBitmap CHARACTER */,
           INPUT TRUE /* LOGICAL */).
   hHelp:MOVE-TO-TOP().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Printrapp wWin 
PROCEDURE Printrapp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cButNrListe AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE dArtikkelNr LIKE ArtBas.ArtikkelNr NO-UNDO.
    DEFINE VARIABLE dDatoLoop   AS DATE       NO-UNDO.
    DEFINE VARIABLE iCount      AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iRadnr      AS INTEGER    NO-UNDO.
    DEFINE VARIABLE crad        AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cSumCols    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cSumString  AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cColAlign   AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE dTmpFraDato AS DATE       NO-UNDO.
    DEFINE VARIABLE dTmpTilDato AS DATE       NO-UNDO.
    DEFINE VARIABLE lPerArtikel AS LOGICAL    NO-UNDO.
    DEFINE VARIABLE dsumDiffPris AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE dsumDiffVVK  AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE dsumDiffDB   AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE dtotDiffPris AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE dtotDiffVVK  AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE dtotDiffDB   AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE cRowBold     AS CHARACTER  NO-UNDO.
    EMPTY TEMP-TABLE TT_Priskontroll.
/*     IF lAktuell = TRUE THEN    */
/*         ASSIGN iButFra = 0     */
/*                iButTil = 0     */
/*                dFradato = ?    */
/*                dTilDato = ?    */
/*                cEAN     = "*". */
    RUN d-Priskontrurval (INPUT-OUTPUT lAktuell,INPUT-OUTPUT iButFra,INPUT-OUTPUT iButTil,
           INPUT-OUTPUT dFraDato,INPUT-OUTPUT dTilDato,INPUT-OUTPUT cEAN,INPUT-OUTPUT iSortering).
    ASSIGN dTmpFraDato = dFraDato
           dTmpTilDato = dTilDato.
    IF RETURN-VALUE = "AVBRYT" THEN
        RETURN.
    IF lAktuell = TRUE THEN DO:
        RUN getFileterverdier IN h_fpriskontrollfilter (OUTPUT cButNrListe,OUTPUT dFraDato).
        ASSIGN dTilDato    = dFraDato
               dArtikkelNr = ?.
    END.
    ELSE DO:
        ASSIGN cButNrListe = getButListe(iButFra,iButTil).
        IF cEAN <> "*" THEN DO:
            FIND StrekKode WHERE StrekKode.Kode = cEAN NO-LOCK.
            ASSIGN dArtikkelNr = StrekKode.ArtikkelNr.
        END.
        ELSE
            ASSIGN dArtikkelNr = ?.
    END.
    ASSIGN lPerArtikel = lAktuell = TRUE OR iSortering < 3. 
    DO iCount = 1 TO NUM-ENTRIES(cButNrListe):
        DO dDatoLoop = dFraDato TO dTilDato:
            FOR EACH PrisKontroll NO-LOCK WHERE PrisKontroll.ButikkNr = INT(ENTRY(iCount,cButNrListe)) AND
                            PrisKontroll.Dato = dDatoLoop AND 
                            IF dArtikkelNr <> ? THEN
                            PrisKontroll.ArtikkelNr = dArtikkelNr ELSE TRUE:
                FIND TT_Priskontroll WHERE TT_PrisKontroll.ButikkNr = (IF lAktuell = FALSE AND iSortering = 1 THEN 0 ELSE PrisKontroll.ButikkNr) AND
                     TT_PrisKontroll.Dato = (IF lAktuell = FALSE AND iSortering < 3 THEN TODAY ELSE PrisKontroll.Dato) AND
                     TT_PrisKontroll.ArtikkelNr = (IF lPerArtikel THEN PrisKontroll.ArtikkelNr ELSE 0) NO-ERROR.
                IF NOT AVAIL TT_PrisKontroll THEN DO:
                    CREATE TT_PrisKontroll.
                    ASSIGN TT_priskontroll.ArtikkelNr    = IF lPerArtikel THEN PrisKontroll.ArtikkelNr ELSE 0
                           TT_priskontroll.BongPris      = priskontroll.BongPris     
                           TT_priskontroll.BongTekst     = priskontroll.BongTekst    
                           TT_priskontroll.ButikkNr      = IF lAktuell = FALSE AND iSortering = 1 THEN 0 ELSE PrisKontroll.ButikkNr
                           TT_priskontroll.Dato          = IF lAktuell = FALSE AND iSortering < 3 THEN TODAY ELSE PrisKontroll.Dato.
/*                            TT_priskontroll.KasseVarekost = priskontroll.KasseVarekost */
/*                            TT_priskontroll.SEPris        = priskontroll.SEPris        */
/*                            TT_priskontroll.SEVarekost    = priskontroll.SEVarekost    */
                END.
                ASSIGN TT_priskontroll.Antall        = TT_priskontroll.Antall + priskontroll.Antall
/*                        TT_priskontroll.BongPris      = */
/*                        TT_priskontroll.KasseVarekost = */
/*                        TT_priskontroll.SEPris        = */
/*                        TT_priskontroll.SEVarekost    = */
                       TT_priskontroll.sumDiffPris   = TT_priskontroll.sumDiffPris + (priskontroll.Antall * (priskontroll.SEPris - priskontroll.BongPris))
                       TT_priskontroll.sumDiffVVK    = TT_priskontroll.sumDiffVVK + (priskontroll.Antall * (priskontroll.SEVarekost - priskontroll.KasseVarekost))
                       TT_priskontroll.sumDiffDB     = TT_priskontroll.sumDiffDB +
                       (priskontroll.Antall * (priskontroll.SEPris - priskontroll.SEVarekost - (priskontroll.BongPris - priskontroll.KasseVarekost))).
            END.
        END.
    END.
    IF NOT CAN-FIND(FIRST TT_Priskontroll) THEN DO:
        MESSAGE "Ingen data att rapportera"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN.
    END.
    OUTPUT TO VALUE(SESSION:TEMP-DIRECTORY + "rappriskont.txt").
    IF lAktuell OR iSortering = 1 THEN DO:
        PUT UNFORMATTED "|Butik|Artikkelnr|Beskrivelse|Dato|Antal|Diff pris|Diff varekost|Diff TB".
        FOR EACH TT_Priskontroll:
            ASSIGN cRad = ""
                   iRadNr = iRadNr + 1.
            ASSIGN cRad = STRING(iRadNr) + "|" + (IF lAktuell THEN STRING(TT_Priskontroll.ButikkNr) ELSE " ") +  "|" + STRING(TT_Priskontroll.ArtikkelNr) + "|" +
                          TT_Priskontroll.Bongtekst + "|" + (IF lAktuell THEN STRING(TT_Priskontroll.Dato) ELSE " ") + "|" + STRING(TT_Priskontroll.Antall) + "|" +
                          STRING(TT_Priskontroll.sumDiffPris,"->,>>>,>>9.99") + "|" + STRING(TT_Priskontroll.sumDiffVVK,"->,>>>,>>9.99") + "|" + 
                          STRING(TT_Priskontroll.sumDiffDB,"->,>>>,>>9.99").
            PUT CONTROL CHR(10) cRad.
        END.
        ASSIGN cSumCols =  "6,7,8"
               cSumString = "4,SUM"
               cColAlign  = "1,1,,,1,1,1,1".
    END.
    ELSE IF iSortering = 2 THEN DO:
        PUT UNFORMATTED "|Butik|Artikkelnr|Beskrivelse|Dato|Antal|Diff pris|Diff varekost|Diff TB".
        FOR EACH TT_Priskontroll BY TT_Priskontroll.ArtikkelNr BY TT_Priskontroll.ButikkNr:
            ASSIGN cRad = ""
                   iRadNr = iRadNr + 1.
            ASSIGN cRad = STRING(iRadNr) + "|" + STRING(TT_Priskontroll.ButikkNr) +  "|" + STRING(TT_Priskontroll.ArtikkelNr) + "|" +
                          TT_Priskontroll.Bongtekst + "|" + " " + "|" + STRING(TT_Priskontroll.Antall) + "|" +
                          STRING(TT_Priskontroll.sumDiffPris,"->,>>>,>>9.99") + "|" + STRING(TT_Priskontroll.sumDiffVVK,"->,>>>,>>9.99") + "|" + 
                          STRING(TT_Priskontroll.sumDiffDB,"->,>>>,>>9.99").
            PUT CONTROL CHR(10) cRad.
        END.
        ASSIGN cSumCols =  "6,7,8"
               cSumString = "4,SUM"
               cColAlign  = "1,1,,,1,1,1,1".
    END.
    ELSE IF iSortering = 3 THEN DO:
        PUT UNFORMATTED "|Butik|Artikkelnr|Beskrivelse|Dato|Antal|Diff pris|Diff varekost|Diff TB".
        FOR EACH TT_Priskontroll BREAK BY TT_Priskontroll.Dato BY TT_Priskontroll.ButikkNr:
            ASSIGN cRad = ""
                   iRadNr = iRadNr + 1.
            ASSIGN cRad = STRING(iRadNr) + "|" + STRING(TT_Priskontroll.ButikkNr) +  "|" + " " + "|" +
                          " " + "|" + STRING(TT_Priskontroll.Dato) + "|" + STRING(TT_Priskontroll.Antall) + "|" +
                          STRING(TT_Priskontroll.sumDiffPris,"->,>>>,>>9.99") + "|" + STRING(TT_Priskontroll.sumDiffVVK,"->,>>>,>>9.99") + "|" + 
                          STRING(TT_Priskontroll.sumDiffDB,"->,>>>,>>9.99").
            ASSIGN dsumDiffPris = dsumDiffPris + TT_Priskontroll.sumDiffPris
                   dsumDiffVVK  = dsumDiffVVK  + TT_Priskontroll.sumDiffVVK
                   dsumDiffDB   = dsumDiffDB   + TT_Priskontroll.sumDiffDB.
            PUT CONTROL CHR(10) cRad.
            IF LAST-OF(TT_Priskontroll.Dato) THEN DO:
                ASSIGN iRadNr = iRadNR + 1
                       cRad   = STRING(iRadNr) + "|" + " " +  "|" + " " + "|" +
                          "SUM" + "|" + STRING(TT_Priskontroll.Dato) + "|" + " " + "|" +
                          STRING(dsumDiffPris,"->,>>>,>>9.99") + "|" + STRING(dsumDiffVVK,"->,>>>,>>9.99") + "|" + 
                          STRING(dsumDiffDB,"->,>>>,>>9.99").
                PUT CONTROL CHR(10) cRad.
                ASSIGN dtotDiffPris = dtotDiffPris + dsumDiffPris
                       dtotDiffVVK  = dtotDiffVVK  + dsumDiffVVK
                       dtotDiffDB   = dtotDiffDB   + dsumDiffDB
                       dsumDiffPris = 0
                       dsumDiffVVK  = 0
                       dsumDiffDB   = 0.
            END.
        END.
        ASSIGN iRadNr = iRadNR + 1
               cRad   = STRING(iRadNr) + "|" + " " +  "|" + " " + "|" +
                  "TOTAL" + "|" + " " + "|" + " " + "|" +
                  STRING(dtotDiffPris,"->,>>>,>>9.99") + "|" + STRING(dtotDiffVVK,"->,>>>,>>9.99") + "|" + 
                  STRING(dtotDiffDB,"->,>>>,>>9.99").
        PUT CONTROL CHR(10) cRad.
        ASSIGN /* cSumCols =  "6,7,8"
               cSumString = "4,SUM" */
               cRowBold  = "3;SUM,TOTAL"
               cColAlign  = "1,1,,,1,1,1,1".
    END.
    ELSE IF iSortering = 4 THEN DO:
        PUT UNFORMATTED "|Butik|Artikkelnr|Beskrivelse|Dato|Antal|Diff pris|Diff varekost|Diff TB".
        FOR EACH TT_Priskontroll BREAK BY TT_Priskontroll.ButikkNr BY TT_Priskontroll.Dato:
            ASSIGN cRad = ""
                   iRadNr = iRadNr + 1.
            ASSIGN cRad = STRING(iRadNr) + "|" + STRING(TT_Priskontroll.ButikkNr) +  "|" + " " + "|" +
                          " " + "|" + STRING(TT_Priskontroll.Dato) + "|" + STRING(TT_Priskontroll.Antall) + "|" +
                          STRING(TT_Priskontroll.sumDiffPris,"->,>>>,>>9.99") + "|" + STRING(TT_Priskontroll.sumDiffVVK,"->,>>>,>>9.99") + "|" + 
                          STRING(TT_Priskontroll.sumDiffDB,"->,>>>,>>9.99").
            ASSIGN dsumDiffPris = dsumDiffPris + TT_Priskontroll.sumDiffPris
                   dsumDiffVVK  = dsumDiffVVK  + TT_Priskontroll.sumDiffVVK
                   dsumDiffDB   = dsumDiffDB   + TT_Priskontroll.sumDiffDB.
            PUT CONTROL CHR(10) cRad.
            IF LAST-OF(TT_Priskontroll.ButikkNr) THEN DO:
                ASSIGN iRadNr = iRadNR + 1
                       cRad   = STRING(iRadNr) + "|" + STRING(TT_Priskontroll.ButikkNr) + "|" + " " + "|" +
                          "SUM" + "|" + " " + "|" + " " + "|" +
                          STRING(dsumDiffPris,"->,>>>,>>9.99") + "|" + STRING(dsumDiffVVK,"->,>>>,>>9.99") + "|" + 
                          STRING(dsumDiffDB,"->,>>>,>>9.99").
                PUT CONTROL CHR(10) cRad.
                ASSIGN dtotDiffPris = dtotDiffPris + dsumDiffPris
                       dtotDiffVVK  = dtotDiffVVK  + dsumDiffVVK
                       dtotDiffDB   = dtotDiffDB   + dsumDiffDB
                       dsumDiffPris = 0
                       dsumDiffVVK  = 0
                       dsumDiffDB   = 0.
            END.
        END.
        ASSIGN iRadNr = iRadNR + 1
               cRad   = STRING(iRadNr) + "|" + " " +  "|" + " " + "|" +
                  "TOTAL" + "|" + " " + "|" + " " + "|" +
                  STRING(dtotDiffPris,"->,>>>,>>9.99") + "|" + STRING(dtotDiffVVK,"->,>>>,>>9.99") + "|" + 
                  STRING(dtotDiffDB,"->,>>>,>>9.99").
        PUT CONTROL CHR(10) cRad.
        ASSIGN /* cSumCols =  "6,7,8"
               cSumString = "4,SUM" */
               cRowBold  = "3;SUM,TOTAL"
               cColAlign  = "1,1,,,1,1,1,1".
    END.
    OUTPUT CLOSE.
    RUN wVisRapport.w (SESSION:TEMP-DIRECTORY + "rappriskont.txt",cSumCols,cSumString,cColAlign,
                       "Prisavviksrapport" + CHR(2) + STRING(dFraDato) + "-" +
                       STRING(dTilDato) + "  " + (IF NUM-ENTRIES(cButNrListe) = 1 THEN "Butik: " + cButNrListe
                            ELSE "Butiker: " + ENTRY(1,cButNrListe) + "-" + ENTRY(NUM-ENTRIES(cButNrListe),cButNrListe)),cRowBold).
    ASSIGN dFraDato = dTmpFraDato
           dTilDato = dTmpTilDato.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getButListe wWin 
FUNCTION getButListe RETURNS CHARACTER
  ( INPUT iFraBut AS INTEGER, INPUT iTilBut AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cButListe AS CHARACTER  NO-UNDO.
  FOR EACH Butiker NO-LOCK WHERE Butiker.Butik >= iFraBut AND Butiker.Butik <= iTilBut:
      ASSIGN cButListe = cButListe + (IF cButListe <> "" THEN "," ELSE "") + STRING(Butiker.Butik).
  END.
  RETURN cButListe.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

