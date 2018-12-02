&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS fFrameWin 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrfrm.w - ADM2 SmartFrame Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

 
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartFrame
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER FRAME

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME fMain

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS B-Scann B-LesInn B-Oppdater B-Overfor B-PBR ~
B-Slett B-SlettTomme B-SlettUansett B-EksporterFil B-Notepad ~
B-SlettSalgPeriode B-Farlig 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON B-EksporterFil 
     LABEL "Eksporter" 
     SIZE 18 BY 1.14 TOOLTIP "Eksporterer og gjenskaper filen slik som den var ved import. I samme katalog.".

DEFINE BUTTON B-Farlig 
     LABEL "*Slett alle data*" 
     SIZE 18 BY 1.14.

DEFINE BUTTON B-LesInn 
     LABEL "Les inn" 
     SIZE 18 BY 1.14 TOOLTIP "Leser inn filen fra filsystemet og lagrer den i databasen.".

DEFINE BUTTON B-MQ 
     LABEL "Sjekk MQ" 
     SIZE 18 BY 1.14 TOOLTIP "Henter filer fra SonicMQ".

DEFINE BUTTON B-Notepad 
     LABEL "Notepad" 
     SIZE 18 BY 1.14.

DEFINE BUTTON B-Oppdater 
     LABEL "Oppdater" 
     SIZE 18 BY 1.14 TOOLTIP "Oppretter kvitteringer på grunnlag av filen og filens datasett.".

DEFINE BUTTON B-Overfor 
     LABEL "Overfør" 
     SIZE 18 BY 1.14 TOOLTIP "Oppretter kvitteringer på grunnlag av filen og filens datasett.".

DEFINE BUTTON B-PBR 
     LABEL "Overfør PBR" 
     SIZE 18 BY 1.14 TOOLTIP "Oppretter kvitteringer på grunnlag av filen og filens datasett.".

DEFINE BUTTON B-Scann 
     LABEL "Sjekk kataloger" 
     SIZE 18 BY 1.14 TOOLTIP "Sjekker filkatalogene for innkomne filer for nye filer.".

DEFINE BUTTON B-Slett 
     LABEL "Slett fillinjer" 
     SIZE 18 BY 1.14 TOOLTIP "Sletter filen fra databasen. Er filen oppdatert, slettes ikke filhode.".

DEFINE BUTTON B-SlettSalgPeriode 
     LABEL "Slett for periode" 
     SIZE 18 BY 1.14.

DEFINE BUTTON B-SlettTomme 
     LABEL "Slett ikke inleste" 
     SIZE 18 BY 1.14 TOOLTIP "Sletter filen fra databasen. Er filen oppdatert, slettes ikke filhode.".

DEFINE BUTTON B-SlettUansett 
     LABEL "Slett fil og fillinjer" 
     SIZE 18 BY 1.14 TOOLTIP "Sletter filen og alle tilhørende data fra databasen.".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     B-Scann AT ROW 2.19 COL 1
     B-MQ AT ROW 3.38 COL 1
     B-LesInn AT ROW 5.76 COL 1
     B-Oppdater AT ROW 6.95 COL 1
     B-Overfor AT ROW 8.14 COL 1
     B-PBR AT ROW 9.33 COL 1
     B-Slett AT ROW 11.71 COL 1
     B-SlettTomme AT ROW 12.91 COL 1
     B-SlettUansett AT ROW 14.1 COL 1
     B-EksporterFil AT ROW 18.14 COL 1
     B-Notepad AT ROW 19.33 COL 1
     B-SlettSalgPeriode AT ROW 20.52 COL 1
     B-Farlig AT ROW 21.71 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.4 ROW 1
         SIZE 19.2 BY 22.1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartFrame
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW fFrameWin ASSIGN
         HEIGHT             = 22.1
         WIDTH              = 20.8.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB fFrameWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW fFrameWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME fMain
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME fMain:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON B-MQ IN FRAME fMain
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fMain
/* Query rebuild information for FRAME fMain
     _Options          = ""
     _Query            is NOT OPENED
*/  /* FRAME fMain */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME B-EksporterFil
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-EksporterFil fFrameWin
ON CHOOSE OF B-EksporterFil IN FRAME fMain /* Eksporter */
DO:
  PUBLISH "EksporterFil".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Farlig
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Farlig fFrameWin
ON CHOOSE OF B-Farlig IN FRAME fMain /* *Slett alle data* */
DO:
  PUBLISH "SlettAlleData".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-LesInn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-LesInn fFrameWin
ON CHOOSE OF B-LesInn IN FRAME fMain /* Les inn */
DO:

  /*PUBLISH "LesInnFiler".*/
  PUBLISH "LesInnGUI".

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-MQ
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-MQ fFrameWin
ON CHOOSE OF B-MQ IN FRAME fMain /* Sjekk MQ */
DO:
  PUBLISH "SjekkMQ".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Notepad
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Notepad fFrameWin
ON CHOOSE OF B-Notepad IN FRAME fMain /* Notepad */
DO:
   PUBLISH "NotePad".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Oppdater
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Oppdater fFrameWin
ON CHOOSE OF B-Oppdater IN FRAME fMain /* Oppdater */
DO:
  PUBLISH "OppdaterGUI".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Overfor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Overfor fFrameWin
ON CHOOSE OF B-Overfor IN FRAME fMain /* Overfør */
DO:
  PUBLISH "OverforFilGUI".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-PBR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-PBR fFrameWin
ON CHOOSE OF B-PBR IN FRAME fMain /* Overfør PBR */
DO:
  PUBLISH "OverforPBR".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Scann
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Scann fFrameWin
ON CHOOSE OF B-Scann IN FRAME fMain /* Sjekk kataloger */
DO:
  PUBLISH "ScannKataloger".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Slett
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Slett fFrameWin
ON CHOOSE OF B-Slett IN FRAME fMain /* Slett fillinjer */
DO:
  PUBLISH "SlettFil".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SlettSalgPeriode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SlettSalgPeriode fFrameWin
ON CHOOSE OF B-SlettSalgPeriode IN FRAME fMain /* Slett for periode */
DO:
  PUBLISH "SlettDagsRapp".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SlettTomme
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SlettTomme fFrameWin
ON CHOOSE OF B-SlettTomme IN FRAME fMain /* Slett ikke inleste */
DO:
  PUBLISH "SlettTommePoster".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SlettUansett
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SlettUansett fFrameWin
ON CHOOSE OF B-SlettUansett IN FRAME fMain /* Slett fil og fillinjer */
DO:
  PUBLISH "SlettFilUansett".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK fFrameWin 


/* ***************************  Main Block  *************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN
   /* Now enable the interface  if in test mode - otherwise this happens when
      the object is explicitly initialized from its container. */
   RUN initializeObject.
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects fFrameWin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI fFrameWin  _DEFAULT-DISABLE
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
  HIDE FRAME fMain.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI fFrameWin  _DEFAULT-ENABLE
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
  ENABLE B-Scann B-LesInn B-Oppdater B-Overfor B-PBR B-Slett B-SlettTomme 
         B-SlettUansett B-EksporterFil B-Notepad B-SlettSalgPeriode B-Farlig 
      WITH FRAME fMain.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject fFrameWin 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR pcPBR AS CHAR NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  {syspara.i 50 200 1 pcPBR}

  IF NOT CAN-DO("tomn,Ken1",USERID("dictdb")) THEN
  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN
          B-EksporterFil:HIDDEN = TRUE
          B-Notepad:HIDDEN = TRUE
          B-SlettSalgPeriode:HIDDEN = TRUE
          B-Farlig:HIDDEN = TRUE
          .
  END.
  IF NOT CAN-DO("1,Ja,True,Yes",pcPBR) THEN
  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN
          B-PBR:HIDDEN = TRUE
          .
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

