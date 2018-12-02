&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

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

/* Local Variable Definitions ---                                       */
DEF VAR cExcep AS CHAR NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 cFilKatalog B-StartEksport cBehandler ~
BtnDone 
&Scoped-Define DISPLAYED-OBJECTS cFilKatalog cBehandler 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-StartEksport 
     LABEL "Start eksport" 
     SIZE 42 BY 1.14.

DEFINE BUTTON BtnDone DEFAULT 
     LABEL "&Avslutt" 
     SIZE 19 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE cBehandler AS CHARACTER FORMAT "X(256)":U 
     LABEL "Behandler" 
     VIEW-AS FILL-IN 
     SIZE 58 BY 1 NO-UNDO.

DEFINE VARIABLE cFilKatalog AS CHARACTER FORMAT "X(256)":U 
     LABEL "Eksportkatalog" 
     VIEW-AS FILL-IN 
     SIZE 58 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 78 BY 5.48.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     cFilKatalog AT ROW 2.67 COL 18 COLON-ALIGNED 
     B-StartEksport AT ROW 3.76 COL 20
     cBehandler AT ROW 5.05 COL 18 COLON-ALIGNED
     BtnDone AT ROW 7.43 COL 61
     RECT-1 AT ROW 1.24 COL 2 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 7.71
         DEFAULT-BUTTON BtnDone.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Eksport av database"
         HEIGHT             = 7.71
         WIDTH              = 80
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 80
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Eksport av database */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Eksport av database */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-StartEksport
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-StartEksport C-Win
ON CHOOSE OF B-StartEksport IN FRAME DEFAULT-FRAME /* Start eksport */
DO:
  RUN StartEksport.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnDone
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDone C-Win
ON CHOOSE OF BtnDone IN FRAME DEFAULT-FRAME /* Avslutt */
DO:
  &IF "{&PROCEDURE-TYPE}" EQ "SmartPanel" &THEN
    &IF "{&ADM-VERSION}" EQ "ADM1.1" &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
    &ELSE
      RUN exitObject.
    &ENDIF
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.


ASSIGN
    cExcep = "akt_rapp," +
             "ArtLag,BatchLogg,BestHLev,BestHode,BestKasse,BestLevert," +
             "BestLevIndivid,BestLinje,BestPris,BestSort,BestStr,Bokforingsbilag," + 
             "Dags_Rap,ELogg,Etikett,_EtikettKo,FakturaHode,FakturaLinje,FilArkiv," + 
             "FriButik,Gavekort,HPrisKo,Jobb,KassererBilag,KassererDag,KassererDat,KassererKontanter," + 
             "KassererOppgj,KassererValuta,KasseTrans,Kas_Konter,Kas_Logg,Kas_Rap,Konto,KOrdreHode,KOrdreLinje," + 
             "Kort_Spes,KundeBetTrans,KundeResKobling,KundeReskontr,KundeSaldo,KundeTrans,KupongTransLogg," + 
             "Lager,LevLager,LevPris,ListeLinje,Lister,MedKjop,MedlemBetTrans,MedlemSaldo,MedRabReskontr," + 
             "MedRabSjekk,MedTrans,NON_Sale_Spes,Ordre,OvArt,OvBuffer,OvBunt,OvLinje,OvLink,OvOrdre,PakkeLinje," + 
             "PkSdlHode,PkSdlLinje,PkSdlMottak,PkSdlPris,plListeArtikkel,plListeHode,plListeLinje,plListeModell," + 
             "PrintLogg,Reklamasjonslinje,Reklamasjonslogg,SieEksport,SIETrans,StLager,StLagerHist,StLinje," + 
             "TelleHode,TelleLinje,TGEmp,TGExport,TGSales,TGSales_Ext,TGTimeStamp,Tilgode," + 
             "TransLogg,VareBehBestHode,VareBehBestLinje,VareBehHode,VareBehLinje,VareBehLinjeTHode," + 
             "VareBehLinjeTrans,VareBehPris,VareBokHode,VareBokLinje,VareTrans,VPIMottak".
/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  {syspara.i 1 1 60 cFilKatalog}

  RUN enable_UI.
  
  DO WITH FRAME DEFAULT-FRAME:
      ASSIGN
          cFilKatalog:SCREEN-VALUE = cFilKatalog.
  END.
 
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  DISPLAY cFilKatalog cBehandler 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-1 cFilKatalog B-StartEksport cBehandler BtnDone 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartEksport C-Win 
PROCEDURE StartEksport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR cTabell AS CHAR NO-UNDO.
  DEF VAR cTekst  AS CHAR NO-UNDO.

  DO WITH FRAME DEFAULT-FRAME:
  
      ASSIGN
          cFilKatalog.

      TABELL:
      FOR EACH SkoTex._File NO-LOCK WHERE
          SkoTex._File._Hidden = FALSE:

          cTabell = SkoTex._File._File-Name.
          cTekst = 'Behandler ' + cTabell + '.'.

          IF LOOKUP(cTabell,cExcep) > 0 THEN
              NEXT TABELL.


          RUN visBehandler (cTekst).
          RUN file_dump.p (cTabell,cFilKatalog).
      END. /* TABELL */
      /* Tar med _User tabellen */
      cTabell = '_User'.
      cTekst = 'Behandler ' + cTabell + '.'.
      RUN visBehandler (cTekst).
      RUN file_dump.p (cTabell,cFilKatalog).
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE visBehandler C-Win 
PROCEDURE visBehandler :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER pcTekst AS CHAR NO-UNDO.

DO WITH FRAME DEFAULT-FRAME:
    ASSIGN
        cBehandler:SCREEN-VALUE = pcTekst.
    PAUSE 1 NO-MESSAGE.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

