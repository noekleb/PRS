&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
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
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEF VAR cTekst AS CHAR NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-70 TG-Innpris TG-Utpris Btn_OK ~
Btn_Avbryt 
&Scoped-Define DISPLAYED-OBJECTS TG-Innpris TG-Utpris FILL-IN-20 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Avbryt AUTO-END-KEY 
     LABEL "Avbryt" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE FILL-IN-20 AS CHARACTER FORMAT "X(256)":U INITIAL "Overstyring av 'Ny pris' kalkyle ved import av pakkseddler" 
      VIEW-AS TEXT 
     SIZE 70 BY .62
     FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-70
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 78 BY 9.76.

DEFINE VARIABLE TG-Innpris AS LOGICAL INITIAL no 
     LABEL "Overstyr innpris med butikkens egen innpris" 
     VIEW-AS TOGGLE-BOX
     SIZE 60 BY .81 NO-UNDO.

DEFINE VARIABLE TG-Utpris AS LOGICAL INITIAL no 
     LABEL "Overstyr utpris med butikkens egen utpris" 
     VIEW-AS TOGGLE-BOX
     SIZE 60 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     TG-Innpris AT ROW 4.57 COL 11
     TG-Utpris AT ROW 5.76 COL 11
     Btn_OK AT ROW 11.24 COL 48
     Btn_Avbryt AT ROW 11.24 COL 63.8
     FILL-IN-20 AT ROW 3.14 COL 4 COLON-ALIGNED NO-LABEL
     RECT-70 AT ROW 1 COL 1
     SPACE(0.00) SKIP(1.80)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Parameteroppsett - overstyring av inn/utpris"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Avbryt.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME                                                           */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-20 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Parameteroppsett - overstyring av inn/utpris */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  RUN enable_UI.
  
  {syspara.i 22 1 1 cTekst}
  IF CAN-DO('1,j,y,true,Ja,Yes',cTekst) THEN
      TG-Utpris:CHECKED = TRUE.
  {syspara.i 22 1 2 cTekst}
  IF CAN-DO('1,j,y,true,Ja,Yes',cTekst) THEN
      TG-Innpris:CHECKED = TRUE.

  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.

DO TRANSACTION:
    FIND SysPara EXCLUSIVE-LOCK where
      SysPara.SysHId = 22 and
      SysPara.SysGr  = 1 and
      SysPara.ParaNr = 1 NO-ERROR.
    if AVAILABLE SysPara then
      ASSIGN SysPara.Parameter1 = (IF TG-Utpris:CHECKED THEN '1' ELSE '0').
    
    FIND SysPara EXCLUSIVE-LOCK where
      SysPara.SysHId = 22 and
      SysPara.SysGr  = 1 and
      SysPara.ParaNr = 2 NO-ERROR.
    if AVAILABLE SysPara then
      ASSIGN SysPara.Parameter1 = (IF TG-Innpris:CHECKED THEN '1' ELSE '0').

    IF AVAILABLE SysPara THEN RELEASE Syspara.
END. /* TRANSACTION */

RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
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
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
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
  DISPLAY TG-Innpris TG-Utpris FILL-IN-20 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-70 TG-Innpris TG-Utpris Btn_OK Btn_Avbryt 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

