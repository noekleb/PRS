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
&Scoped-Define ENABLED-OBJECTS B-Ny RECT-57 B-OpprettUtvalg B-Koble B-Pris ~
B-SjekkKobling B-Artikkel BUTTON-VPI B-SetOppdat B-Slett 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Artikkel 
     LABEL "Vis artikkel..." 
     SIZE 18.6 BY 1.14 TOOLTIP "Åpner artikkelkortet (Hvis posten er koblet mot lokat register)".

DEFINE BUTTON B-Koble 
     LABEL "Koble EAN..." 
     SIZE 18.6 BY 1.14 TOOLTIP "Kobler posten til lokal artikkel.".

DEFINE BUTTON B-Ny 
     LABEL "Opprett ny..." 
     SIZE 18.6 BY 1.14 TOOLTIP "Oppretter ny artikkel i lokalt artikkelregsiter.".

DEFINE BUTTON B-OpprettUtvalg 
     LABEL "Opprett utvalg..." 
     SIZE 18.6 BY 1.14 TOOLTIP "Overfører varer i utvalg til lokalt artikkelregister.".

DEFINE BUTTON B-Pris 
     LABEL "Oppdater pris..." 
     SIZE 18.6 BY 1.14 TOOLTIP "Overfører prisen til lokal artikkel. Posten må være koblet.".

DEFINE BUTTON B-SetOppdat 
     LABEL "Sett oppdatert" 
     SIZE 18.6 BY 1.14 TOOLTIP "Merker datasettet som oppdatert, slik at nytt datasett kan leses inn.".

DEFINE BUTTON B-SjekkKobling 
     LABEL "Sjekk kobling..." 
     SIZE 18.6 BY 1.14 TOOLTIP "Oppdaterer kobling mot lokalt register.".

DEFINE BUTTON B-Slett 
     LABEL "Slett" 
     SIZE 18.6 BY 1.14 TOOLTIP "Sletter posten.".

DEFINE BUTTON BUTTON-VPI 
     LABEL "Overfør til VPI" 
     SIZE 18.6 BY 1.14.

DEFINE RECTANGLE RECT-57
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 20 BY 15.52.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     B-Ny AT ROW 1.24 COL 2
     B-OpprettUtvalg AT ROW 2.43 COL 2
     B-Koble AT ROW 3.62 COL 2
     B-Pris AT ROW 4.81 COL 2
     B-SjekkKobling AT ROW 7.14 COL 2
     B-Artikkel AT ROW 8.38 COL 2
     BUTTON-VPI AT ROW 9.57 COL 2
     B-SetOppdat AT ROW 12.67 COL 2
     B-Slett AT ROW 15.05 COL 2
     RECT-57 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.4 ROW 1
         SIZE 20 BY 15.91.


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
         HEIGHT             = 15.62
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

&Scoped-define SELF-NAME B-Artikkel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Artikkel fFrameWin
ON CHOOSE OF B-Artikkel IN FRAME fMain /* Vis artikkel... */
DO:

  PUBLISH "VisArtikkel".

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Koble
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Koble fFrameWin
ON CHOOSE OF B-Koble IN FRAME fMain /* Koble EAN... */
DO:
  PUBLISH "Koble".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Ny
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Ny fFrameWin
ON CHOOSE OF B-Ny IN FRAME fMain /* Opprett ny... */
DO:
  PUBLISH "Ny".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-OpprettUtvalg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-OpprettUtvalg fFrameWin
ON CHOOSE OF B-OpprettUtvalg IN FRAME fMain /* Opprett utvalg... */
DO:
  PUBLISH "OpprettUtvalg".
  PUBLISH "Pris".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Pris
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Pris fFrameWin
ON CHOOSE OF B-Pris IN FRAME fMain /* Oppdater pris... */
DO:
  PUBLISH "Pris".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SetOppdat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SetOppdat fFrameWin
ON CHOOSE OF B-SetOppdat IN FRAME fMain /* Sett oppdatert */
DO:

  PUBLISH "SettOppdat".

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SjekkKobling
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SjekkKobling fFrameWin
ON CHOOSE OF B-SjekkKobling IN FRAME fMain /* Sjekk kobling... */
DO:
  PUBLISH "SjekkKobling".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Slett
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Slett fFrameWin
ON CHOOSE OF B-Slett IN FRAME fMain /* Slett */
DO:
  PUBLISH "Slett".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-VPI
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-VPI fFrameWin
ON CHOOSE OF BUTTON-VPI IN FRAME fMain /* Overfør til VPI */
DO:
  PUBLISH "OverforTilHKVPI".
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisableKobleEan fFrameWin 
PROCEDURE DisableKobleEan :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
        B-Koble:SENSITIVE = FALSE
        .
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisableOpprettUtvalg fFrameWin 
PROCEDURE DisableOpprettUtvalg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
        B-OpprettUtvalg:SENSITIVE = FALSE
        .
END.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EnableKobleEan fFrameWin 
PROCEDURE EnableKobleEan :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
        B-Koble:SENSITIVE = TRUE 
        .
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EnableOpprettUtvalg fFrameWin 
PROCEDURE EnableOpprettUtvalg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
        B-OpprettUtvalg:SENSITIVE = TRUE 
        .
END.
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
  ENABLE B-Ny RECT-57 B-OpprettUtvalg B-Koble B-Pris B-SjekkKobling B-Artikkel 
         BUTTON-VPI B-SetOppdat B-Slett 
      WITH FRAME fMain.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeDataObjects fFrameWin 
PROCEDURE initializeDataObjects :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAMETER plDeep AS LOGICAL NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER( INPUT plDeep).

  /* Code placed here will execute AFTER standard behavior.    */
            
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

