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
DEF VAR cKataloger    AS CHAR NO-UNDO.
DEF VAR cFilNavnListe AS CHAR NO-UNDO.

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

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS B-Scann B-Allt B-LesInn B-PakkUt 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_dvpifilhode AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Allt 
     LABEL "Allt" 
     SIZE 27 BY 3.57.

DEFINE BUTTON B-LesInn 
     LABEL "Les inn" 
     SIZE 29 BY 1.14.

DEFINE BUTTON B-PakkUt 
     LABEL "Pakk ut" 
     SIZE 29 BY 1.14.

DEFINE BUTTON B-Scann 
     LABEL "Scann kataloger" 
     SIZE 29 BY 1.14.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     B-Scann AT ROW 1.24 COL 2
     B-Allt AT ROW 1.24 COL 31
     B-LesInn AT ROW 2.43 COL 2
     B-PakkUt AT ROW 3.62 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 57.4 BY 3.95.


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
         TITLE              = "Import av VPI og registerdata"
         HEIGHT             = 3.95
         WIDTH              = 57.4
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
ON END-ERROR OF wWin /* Import av VPI og registerdata */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Import av VPI og registerdata */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Allt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Allt wWin
ON CHOOSE OF B-Allt IN FRAME fMain /* Allt */
DO:
  RUN Allt.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-LesInn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-LesInn wWin
ON CHOOSE OF B-LesInn IN FRAME fMain /* Les inn */
DO:
  {sww.i}
  RUN LesInn.
  {swn.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-PakkUt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-PakkUt wWin
ON CHOOSE OF B-PakkUt IN FRAME fMain /* Pakk ut */
DO:
  {sww.i}
  RUN PakkUt.
  {swn.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Scann
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Scann wWin
ON CHOOSE OF B-Scann IN FRAME fMain /* Scann kataloger */
DO:
    {sww.i}
    RUN ScannKataloger.
    {swn.i}
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
             INPUT  'sdo/dvpifilhode.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedvpifilhodeUpdateFromSourcenoToggleDataTargetsyesOpenOnInityesPromptOnDeleteyesPromptColumns(NONE)':U ,
             OUTPUT h_dvpifilhode ).
       RUN repositionObject IN h_dvpifilhode ( 1.71 , 10.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       /* Adjust the tab order of the smart objects. */
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Allt wWin 
PROCEDURE Allt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
{sww.i}
RUN ScannKataloger.
RUN LesInn.
RUN PakkUt.
{swn.i}

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
  ENABLE B-Scann B-Allt B-LesInn B-PakkUt 
      WITH FRAME fMain IN WINDOW wWin.
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
  DEF VAR pcKeyValues      AS char NO-UNDO.
  DEF VAR piFokusRad       AS INT  NO-UNDO.

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
      DO piLoop1 = 1 TO num-entries(pcValgteFiler,chr(1)):
        /* Leser inn fil. */
        RUN LesInnFil IN h_dvpifilhode (INPUT ENTRY(piLoop1,pcValgteFiler,CHR(1)), 
                                   OUTPUT pbOk, 
                                   OUTPUT piAntLinjer).
        /* Viser eventuell feilmelding. */
        /* PUBLISH .... */
      END.
      ASSIGN
          pcValgteFiler = ""
          .
  END.

  IF pcKeyValues <> "" THEN
  DO:
      DYNAMIC-FUNCTION('openQuery':U IN h_dvpifilhode).
      DYNAMIC-FUNCTION('findRow':U IN h_dvpifilhode,
         INPUT pcKeyValues /* CHARACTER */).
      RUN dataAvailable IN h_dvpifilhode
          ( INPUT "SAME" /* CHARACTER */).
  END.
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
  DEF VAR pcKeyValues      AS char NO-UNDO.
  DEF VAR piFokusRad       AS INT  NO-UNDO.
  DEF VAR pbVPIFilStatus   AS INT  NO-UNDO.

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
      DO piLoop1 = 1 TO num-entries(pcValgteFiler,chr(1)):
          RUN GetVPIFilStatus IN h_dvpifilhode (INPUT entry(piLoop1,pcValgteFiler,CHR(1)),OUTPUT pbVPIFilStatus).
          IF pbVPIFilStatus >= 3 AND 
             pbVPIFilStatus <= 5 THEN
              RUN PakkUtFil IN h_dvpifilhode (INPUT entry(piLoop1,pcValgteFiler,CHR(1))).
      END. /* FILBEHANDLING */
      ASSIGN
          pcValgteFiler = ""
          .
  END.

  IF pcKeyValues <> "" THEN
  DO:
      DYNAMIC-FUNCTION('openQuery':U IN h_dvpifilhode).
      DYNAMIC-FUNCTION('findRow':U IN h_dvpifilhode,
         INPUT pcKeyValues /* CHARACTER */).
      RUN dataAvailable IN h_dvpifilhode
          ( INPUT "SAME" /* CHARACTER */).
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ScannKataloger wWin 
PROCEDURE ScannKataloger :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       Scanner katalog og åpner query med ny liste. 
------------------------------------------------------------------------------*/
  /* 1. Bygg en liste over alle kataloger som skal kontrolleres. */
  RUN GetKatalogListe IN h_dvpifilhode (OUTPUT cKataloger).
  
  /* 2. Bygg en liste med alle filnavn + ekstent som skal kontrolleres. */
  RUN GetFilNavnListe IN h_dvpifilhode (OUTPUT cFilNavnListe).
  
  /* 3. Opprett en post i fillisten for alle filer som ikke finnes der. */
  RUN OpprettPoster IN h_dvpifilhode (INPUT cKataloger, INPUT cFilNavnListe). 

 /* 5. OpenQuery for fillisten. */ 
  DYNAMIC-FUNCTION('openQuery':U IN h_dvpifilhode).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

