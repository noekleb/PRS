&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME gDialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS gDialog 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrdlg.w - ADM2 SmartDialog Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
&IF "{&UIB_is_Running}" = ""  &THEN
  DEF INPUT-OUTPUT PARAMETER cColValues AS CHAR  NO-UNDO.
  DEF INPUT        PARAMETER cFelt      AS CHAR  NO-UNDO.
  DEF INPUT        PARAMETER cVerdier   AS CHAR  NO-UNDO.
  DEF INPUT        PARAMETER cStart     AS CHAR  NO-UNDO.
  DEF INPUT        PARAMETER iRecid     AS RECID NO-UNDO.
&else
  DEF VAR cColValues AS CHAR NO-UNDO.
  DEF VAR cFelt             AS CHAR NO-UNDO.
  DEF VAR cVerdier          AS CHAR NO-UNDO.
  DEF VAR cStart            AS CHAR NO-UNDO.
  DEF VAR iRecid            AS INT  NO-UNDO.

  ASSIGN
      cFelt    = "EkstVPILevNr"
      cVerdier = "100"
      cStart   = "0098095343980"
      iRecid   = ?
      .
&ENDIF  

/* Local Variable Definitions ---                                       */
DEF VAR cReturn-Value AS CHAR INITIAL "AVBRYT" NO-UNDO.
DEF VAR cTekst        AS CHAR NO-UNDO.
DEF VAR iModus        AS INT  INITIAL 0 NO-UNDO. /* 0-Avbryt, 1-Ny, 2-Koble */
DEF VAR lArtikkelNr   AS DEC  NO-UNDO.
DEF VAR bT-Modell     AS LOG  NO-UNDO.
DEF VAR lValgArtikkel AS DEC  NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME gDialog

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BUTTON-2 B-NyArt B-Koble Btn_Cancel 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_bvpiartbas AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dvpiartbas AS HANDLE NO-UNDO.
DEFINE VARIABLE h_fvisvpibilde AS HANDLE NO-UNDO.
DEFINE VARIABLE h_fvpiartbas AS HANDLE NO-UNDO.
DEFINE VARIABLE h_sortsok AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Koble 
     LABEL "Koble til valgt artikkel" 
     SIZE 25 BY 1.14.

DEFINE BUTTON B-NyArt 
     LABEL "Opprett ny artikkel" 
     SIZE 25 BY 1.14.

DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Avbryt" 
     SIZE 24 BY 1.14.

DEFINE BUTTON BUTTON-2 
     LABEL "La stå!" 
     SIZE 15 BY 1.14.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME gDialog
     BUTTON-2 AT ROW 7.67 COL 130
     B-NyArt AT ROW 24.57 COL 1
     B-Koble AT ROW 24.57 COL 26
     Btn_Cancel AT ROW 24.57 COL 121
     SPACE(0.39) SKIP(0.00)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Søkeliste VPI register"
         CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB gDialog 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}
{dproclibstart.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX gDialog
   NOT-VISIBLE FRAME-NAME Custom                                        */
ASSIGN 
       FRAME gDialog:SCROLLABLE       = FALSE
       FRAME gDialog:HIDDEN           = TRUE.

ASSIGN 
       BUTTON-2:HIDDEN IN FRAME gDialog           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX gDialog
/* Query rebuild information for DIALOG-BOX gDialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX gDialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME gDialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gDialog gDialog
ON GO OF FRAME gDialog /* Søkeliste VPI register */
DO:
  ASSIGN
      cReturn-Value = "OK"
      .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gDialog gDialog
ON WINDOW-CLOSE OF FRAME gDialog /* Søkeliste VPI register */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Koble
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Koble gDialog
ON CHOOSE OF B-Koble IN FRAME gDialog /* Koble til valgt artikkel */
DO:
  ASSIGN
      iModus = 2
      .
  RUN Koble.
  IF RETURN-VALUE <> "AVBRYT" THEN
      APPLY "GO" TO FRAME gDialog.
  ELSE 
      RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-NyArt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-NyArt gDialog
ON CHOOSE OF B-NyArt IN FRAME gDialog /* Opprett ny artikkel */
DO:
  ASSIGN
      iModus = 1
      .
  RUN Ny.
  IF RETURN-VALUE <> "AVBRYT" THEN
      APPLY "GO" TO FRAME gDialog.
  ELSE 
      RETURN NO-APPLY.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 gDialog
ON CHOOSE OF BUTTON-2 IN FRAME gDialog /* La stå! */
DO:
  /* Denne koden virker ikke i initialise object, men den virker */
  /* når den kjøres herfra ???                                   */
  DYNAMIC-FUNCTION('findRowWhere':U IN h_dvpiartbas,
     INPUT cFelt    + (IF cFelt = "" THEN "" ELSE ",") + "VareNr",
     INPUT cVerdier + (IF cVerdier = "" THEN "" ELSE CHR(1)) + cStart,
     INPUT "=,=" /* CHARACTER */).
  RUN dataAvailable IN h_dvpiartbas
    ( INPUT "SAME" /* CHARACTER */).


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK gDialog 


/* ***************************  Main Block  *************************** */

{lng.i &SDO = "SDO"}
{src/adm2/dialogmn.i}

RETURN cReturn-Value.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects gDialog  _ADM-CREATE-OBJECTS
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
             INPUT  'sdo/dvpiartbas.wDB-AWARE':U ,
             INPUT  FRAME gDialog:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessnoDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedvpiartbasOpenOnInitnoPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dvpiartbas ).
       RUN repositionObject IN h_dvpiartbas ( 1.00 , 1.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'prg/fvisvpibilde.w':U ,
             INPUT  FRAME gDialog:HANDLE ,
             INPUT  'LogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_fvisvpibilde ).
       RUN repositionObject IN h_fvisvpibilde ( 1.95 , 2.00 ) NO-ERROR.
       /* Size in AB:  ( 4.81 , 28.20 ) */

       RUN constructObject (
             INPUT  'prg/bvpiartbas.w':U ,
             INPUT  FRAME gDialog:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bvpiartbas ).
       RUN repositionObject IN h_bvpiartbas ( 8.86 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_bvpiartbas ( 15.48 , 144.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'prg/fvpiartbas.w':U ,
             INPUT  FRAME gDialog:HANDLE ,
             INPUT  'LogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_fvpiartbas ).
       RUN repositionObject IN h_fvpiartbas ( 1.00 , 48.00 ) NO-ERROR.
       /* Size in AB:  ( 7.71 , 97.20 ) */

       RUN constructObject (
             INPUT  'prg/sortsok.w':U ,
             INPUT  FRAME gDialog:HANDLE ,
             INPUT  'HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_sortsok ).
       RUN repositionObject IN h_sortsok ( 6.95 , 1.00 ) NO-ERROR.
       /* Size in AB:  ( 1.76 , 27.60 ) */

       /* Links to SmartDataObject h_dvpiartbas. */
       RUN addLink ( h_fvpiartbas , 'SokSdo':U , h_dvpiartbas ).

       /* Links to SmartFrame h_fvisvpibilde. */
       RUN addLink ( h_dvpiartbas , 'Data':U , h_fvisvpibilde ).

       /* Links to SmartDataBrowser h_bvpiartbas. */
       RUN addLink ( h_dvpiartbas , 'Data':U , h_bvpiartbas ).
       RUN addLink ( h_bvpiartbas , 'Update':U , h_dvpiartbas ).

       /* Links to SmartObject h_sortsok. */
       RUN addLink ( h_bvpiartbas , 'Sortera':U , h_sortsok ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_bvpiartbas ,
             BUTTON-2:HANDLE , 'AFTER':U ).
       RUN adjustTabOrder ( h_fvisvpibilde ,
             h_bvpiartbas , 'AFTER':U ).
       RUN adjustTabOrder ( h_fvpiartbas ,
             B-Koble:HANDLE , 'AFTER':U ).
       RUN adjustTabOrder ( h_sortsok ,
             h_fvpiartbas , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createObjects gDialog 
PROCEDURE createObjects :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  SUBSCRIBE "MouseDblClick" IN h_bvpiartbas.

  /* Setter opp Where sats i Query.  */
  /* Legges inn der hvor det skal gjøres avgrensning I datasett for søk. */
  IF cFelt <> "" THEN
  DO:
    DYNAMIC-FUNCTION('assignQuerySelection':U IN h_dvpiartbas,
      INPUT cFelt,    /* Comma separerte verdier  */
      INPUT cVerdier, /* CHR(1) separerte verdier */
      INPUT "EQ").
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE destroyObject gDialog 
PROCEDURE destroyObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  IF cReturn-Value <> "AVBRYT" THEN  
  DO:
      /* Oppretter artikkel */
      IF iModus = 1 THEN
      NY-ARTIKKEL:
      DO:
          ASSIGN
              cColValues = DYNAMIC-FUNCTION('colValues':U IN h_dvpiArtBas,
                             INPUT "VareNr,ArtikkelNr" /* CHARACTER */)
              ENTRY(3,cColValues,CHR(1)) = STRING(lValgArtikkel)
              .  
      
      END. /* NY-ARTIKKEL */
      ELSE IF iModus = 2 THEN
      KOBLE-ARTIKKEL:
      DO: 
          ASSIGN
            cColValues = DYNAMIC-FUNCTION('colValues':U IN h_dvpiArtBas,
                           INPUT "VareNr,ArtikkelNr" /* CHARACTER */)
            ENTRY(3,cColValues,CHR(1)) = STRING(lValgArtikkel)
            .  
      END. /* KOBLE-ARTIKKEL */
  END.

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI gDialog  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME gDialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI gDialog  _DEFAULT-ENABLE
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
  ENABLE BUTTON-2 B-NyArt B-Koble Btn_Cancel 
      WITH FRAME gDialog.
  {&OPEN-BROWSERS-IN-QUERY-gDialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject gDialog 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR pcTekst      AS CHAR NO-UNDO.
  DEF VAR pcUtvidetSok AS CHAR NO-UNDO.

  IF cStart BEGINS "Utvidet" THEN
      ASSIGN
        pcUtvidetsok = ENTRY(2,cStart ,CHR(1))
        cColValues   = ""
        .

  /* Code placed here will execute PRIOR to standard behavior. */
  DYNAMIC-FUNCTION('setForeignFields':U IN h_dvpiartbas,
     INPUT cFelt /* CHARACTER */).
  DYNAMIC-FUNCTION('setForeignValues':U IN h_dvpiartbas,
     INPUT cVerdier /* CHARACTER */).

  RUN SwitchLng.
  
  RUN SUPER.
  
  ASSIGN
    BUTTON-2:HIDDEN IN FRAME {&FRAME-NAME} = TRUE .

  /* Snuskm men... */
  FIND ArtBas NO-LOCK WHERE
      RECID(ArtBas) = iRecid NO-ERROR.
  IF AVAILABLE ArtBas THEN
      lArtikkelNr = ArtBas.ArtikkelNr.
  ELSE
      lArtikkelNr = 0.

  /* Signalerer til de andre objektene. */
  RUN dataAvailable IN h_dvpiartbas
    ( INPUT "SAME" /* CHARACTER */).


  /* Huff */
  FIND EkstVPILev NO-LOCK WHERE
      EkstVPILev.EkstVPILevNr = INT(cVerdier) NO-ERROR.
  IF AVAILABLE EkstVPILev THEN
  DO:
      /* Leverandører som det ikke er lagt opp parameter for, har slik håndtering. */
      /* Lev. som har "nei, no, false , 0" har ikke.                               */
      {syspara.i 1 13 EkstVPILev.EkstVPILevNr pcTekst}
      IF can-do("1,Nei,no,false,N", pcTekst) THEN
          bT-Modell = FALSE.
      ELSE 
          bT-Modell = TRUE.

    ASSIGN 
    FRAME {&FRAME-NAME}:TITLE = FRAME {&FRAME-NAME}:TITLE + "  " + 
                               "VPI leverandør " + EkstVPILev.KortNavn
    B-Koble:HIDDEN IN FRAME {&FRAME-NAME} = 
        IF bT-Modell = TRUE OR (AVAIL ArtBas AND ArtBas.ArtikkelNr = ArtBas.Vg)
                                              THEN TRUE
                                              ELSE FALSE
    .
  END.

  IF NUM-ENTRIES(cStart,CHR(1)) >= 2 THEN DO:
     RUN SetFilter     IN h_fvpiartbas (INPUT cStart).
  END.
  IF pcUtvidetsok <> "" THEN
  CASE ENTRY(1,cStart,chr(1)):
      WHEN 'Utvidet'       THEN RUN SetUtvidetsok IN h_fvpiartbas (INPUT pcUtvidetsok).
      WHEN 'UtvidetBeskr'  THEN RUN SetBeskr      IN h_fvpiartbas (INPUT pcUtvidetsok).
      WHEN 'UtvidetLevKod' THEN RUN SetLevKod     IN h_fvpiartbas (INPUT pcUtvidetsok).
  END CASE.

  PUBLISH "Sortera" FROM h_bvpiartbas.
/*   RUN SetRadFokus IN h_bvpiartbas. */
/*   RUN InitierDiv IN h_sortsok.     */

  /* Code placed here will execute AFTER standard behavior.    */
  IF NUM-ENTRIES(cStart,CHR(1)) <= 1 THEN
  DO:
/*       DYNAMIC-FUNCTION('findRowWhere':U IN h_dvpiartbas,                   */
/*          INPUT cFelt    + (IF cFelt = "" THEN "" ELSE ",") + "VareNr",     */
/*          INPUT cVerdier + (IF cVerdier = "" THEN "" ELSE CHR(1)) + cStart, */
/*          INPUT "=,=" /* CHARACTER */).                                     */
/*       RUN dataAvailable IN h_dvpiartbas                                    */
/*         ( INPUT "SAME" /* CHARACTER */).                                   */
      /* Koden virker ikke her, men den virker under knappen. */
      APPLY "CHOOSE" TO button-2 IN FRAME {&FRAME-NAME}.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Koble gDialog 
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
  DEF VAR pcKeyValues      AS CHAR NO-UNDO.
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
  DO:
       iModus = 0.
      RETURN "AVBRYT".
  END.

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

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MouseDblClick gDialog 
PROCEDURE MouseDblClick :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN
      iModus = 1
      .
  APPLY "CHOOSE":U TO B-NyArt IN FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Ny gDialog 
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
  DEF VAR pcKeyValues      AS CHAR NO-UNDO.
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

  /* For VPI lev som leverer komplette artikkler, kan bare en post velges. */
/*   IF piEkstVPILevNr < 100 AND num-entries(pcValgteArtikler,CHR(1)) > 1 THEN */
  IF bT-Modell = TRUE AND num-entries(pcValgteArtikler,CHR(1)) > 1 THEN 
  DO:
      MESSAGE "Denne VPI leverandøren leverer komplett artikkelinformasjon." SKIP
              "Det kan derfor bare velges en og en artikkel ad gangen." SKIP
              "Med artikkel kommer alle strekkoder, størrelser m.m."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN.
  END.

  /* Artikler valgt i browser. */
  ASSIGN pbOk = TRUE.
  MESSAGE "Der er valgt " + string(num-entries(pcValgteArtikler,CHR(1))) + " VPI poster." SKIP
          "Skal kobling startes?"
    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Bekreft"
    UPDATE pbOk.
  IF pbOk <> TRUE THEN
  DO:
       iModus = 0.
       RETURN "AVBRYT".
  END.

  /* Tar vare på den første posten for å reposisjonere i browser etter oppdatering. */
  ASSIGN
      pcKeyValues   = string(piEkstVPILevNr) + chr(1) + 
                      ENTRY(1,pcValgteArtikler,CHR(1))
      lValgArtikkel = DEC(ENTRY(1,pcValgteArtikler,CHR(1))).
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

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppdaterArtikkel gDialog 
PROCEDURE OppdaterArtikkel :
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

  ASSIGN
      pcVareNr = ENTRY(1,pcListe,CHR(1))
      .
  /* For ny og koble, skal ikke VPI posten være koblet. */
  /* Den skal ikke inneholde 02 EAN koder.              */
  IF CAN-DO("1,2",STRING(piModus)) THEN
  SJEKK-KOBLING:
  DO piLoop = 1 TO NUM-ENTRIES(pcListe,CHR(1)):
      /* Returneres 0 i plArtikkelNr, er artikkelen ikke koblet. */
      RUN GetArtikkelNr IN h_dvpiartbas (piEkstVpiLevNr, ENTRY(piLoop,pcListe,CHR(1)), OUTPUT plArtikkelNr).

      /* Dobbelsjekk på at strekkoden ikke allerede finnes lokalt. */
      /* Registrering av strekkoder i lokalt register, oppdaterer  */
      /* ikke vpi registeret. Derfor denne ekstra sjekken.         */
      IF AVAILABLE Strekkode THEN RELEASE Strekkode.
      FIND Strekkode NO-LOCK WHERE
         Strekkode.Kode = TRIM(ENTRY(piLoop,pcListe,CHR(1))) NO-ERROR.
      IF AVAILABLE Strekkode THEN
      DO:
        ASSIGN
          plArtikkelNr = Strekkode.ArtikkelNr
          .
      END.

      /* Artikkelen er allerede koblet */
      IF plArtikkelNr <> 0 AND CAN-FIND(ArtBas WHERE
                                        ArtBas.ArtikkelNr = plArtikkelNr) THEN
      DO:
          MESSAGE "En eller flere av VPI postene er allerede koblet." SKIP
                  "Ingen av VPI postene som velges, kan være koblet fra før." SKIP
                  "Skal eventuelle nye strekkoder hentes ut fra VPI og legges på artikkelen?"
              VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE pbOk.
          IF NOT pbOk THEN
            RETURN "AVBRYT".
      END.

/*       /* Er lengden på 13 siffer og de to første er 02, Bedriftsintern kode. */                                           */
/*       IF LENGTH(ENTRY(piLoop,pcListe,CHR(1))) = 13 AND                                                                    */
/*          SUBSTRING(ENTRY(piLoop,pcListe,CHR(1)),1,2) = "02" THEN                                                          */
/*       DO:                                                                                                                 */
/*           /* Er det bare valgt en post, kan det tillates, men da kan bare en */                                           */
/*           /* post være valgt.                                                */                                           */
/*           IF NUM-ENTRIES(pcListe,CHR(1)) = 1 AND piModus = 1 THEN                                                         */
/*           DO:                                                                                                             */
/*             MESSAGE "Varen har en bedriftsintern EAN kode. Den vil få tildelt ny EAN kode i lokalt artikkelregister."     */
/*               VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                                          */
/*             ASSIGN                                                                                                        */
/*               piModus = 1   /* Slår om til ny vare. */                                                                    */
/*               .                                                                                                           */
/*             LEAVE SJEKK-KOBLING.                                                                                          */
/*           END.                                                                                                            */
/*                                                                                                                           */
/*           MESSAGE "En eller flere av VPI postene inneholder bedriftsinterne EAN koder." SKIP                              */
/*                   "Slike koder kan ikke hentes inn fra SportKat." SKIP(1)                                                 */
/*                   "Varer med slike EAN koder kan hentes inn fra SportKat, men da kan bare en vare velges ad gangen." SKIP */
/*                   "Det må velges: Opprett ny Vare."                                                                       */
/*               VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                                          */
/*           RETURN "AVBRYT".                                                                                                */
/*       END.                                                                                                                */

  END. /* SJEKK-KOBLING */

  /* Kobling.                                             */
  /* KOBLE. */
  IF piModus = 2 THEN
  KOBLE:
  DO: 
    RUN OppdaterInfo IN h_dvpiartbas (piEkstVpiLevNr, pcListe, lArtikkelNr).
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
      IF NOT CAN-FIND(ArtBas WHERE ArtBas.ArtikkelNr = plArtikkelNr) THEN 
      DO:
          RUN OpprettNy    IN h_dvpiartbas (piEkstVpiLevNr, pcListe, OUTPUT plArtikkelNr).
          IF RETURN-VALUE <> "" THEN
          DO:
              MESSAGE RETURN-VALUE
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
              RETURN.
          END.
      END.
      RUN OppdaterInfo IN h_dvpiartbas (piEkstVpiLevNr, pcListe, plArtikkelNr).
      ASSIGN
          lArtikkelNr = plArtikkelNr
          .
  END. /* OPPRETTNY */


  RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

