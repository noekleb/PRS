&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
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
DEF INPUT  PARAM iiNumSelected AS INT    NO-UNDO.
DEF INPUT  PARAM ihLevUkeList  AS HANDLE NO-UNDO.
DEF INPUT  PARAM icButikkNr    AS CHAR   NO-UNDO.
DEF INPUT  PARAM ibAlleBut     AS LOG    NO-UNDO.
DEF OUTPUT PARAM obOk          AS LOG    NO-UNDO.
DEF OUTPUT PARAM obAlleBut     AS LOG    NO-UNDO.
DEF OUTPUT PARAM ocLevUkeList  AS CHAR   NO-UNDO.

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS LevUke1 LevUke2 LevUke3 LevUke4 rsBut ~
fiAntArt Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS LevUke1 LevUke2 LevUke3 LevUke4 rsBut ~
fiAntArt 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LocalTranslation Dialog-Frame 
FUNCTION LocalTranslation RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "&Avbryt" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE LevUke1 AS CHARACTER 
     LABEL "Levuke1" 
     VIEW-AS COMBO-BOX INNER-LINES 35
     LIST-ITEMS "Item 1" 
     DROP-DOWN AUTO-COMPLETION UNIQUE-MATCH
     SIZE 11.2 BY 1 NO-UNDO.

DEFINE VARIABLE LevUke2 AS CHARACTER 
     LABEL "Levuke2" 
     VIEW-AS COMBO-BOX INNER-LINES 35
     LIST-ITEMS "Item 1" 
     DROP-DOWN AUTO-COMPLETION UNIQUE-MATCH
     SIZE 11.2 BY 1 NO-UNDO.

DEFINE VARIABLE LevUke3 AS CHARACTER 
     LABEL "Levuke3" 
     VIEW-AS COMBO-BOX INNER-LINES 35
     LIST-ITEMS "Item 1" 
     DROP-DOWN AUTO-COMPLETION UNIQUE-MATCH
     SIZE 11.2 BY 1 NO-UNDO.

DEFINE VARIABLE LevUke4 AS CHARACTER 
     LABEL "Levuke4" 
     VIEW-AS COMBO-BOX INNER-LINES 35
     LIST-ITEMS "Item 1" 
     DROP-DOWN AUTO-COMPLETION UNIQUE-MATCH
     SIZE 11.2 BY 1 NO-UNDO.

DEFINE VARIABLE fiAntArt AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Antall valgte artikler" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE rsBut AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Gjeldende butikk", 1,
"Alle butikker", 2
     SIZE 41.8 BY 1.19 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     LevUke1 AT ROW 2.33 COL 11 COLON-ALIGNED
     LevUke2 AT ROW 2.33 COL 36.6 COLON-ALIGNED
     LevUke3 AT ROW 2.33 COL 62.6 COLON-ALIGNED
     LevUke4 AT ROW 2.33 COL 88.8 COLON-ALIGNED
     rsBut AT ROW 4.33 COL 32.2 NO-LABEL
     fiAntArt AT ROW 6.48 COL 30.2 COLON-ALIGNED
     Btn_OK AT ROW 8.38 COL 71
     Btn_Cancel AT ROW 8.38 COL 87
     SPACE(1.39) SKIP(0.52)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Sett nye leveringuker"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   NOT-VISIBLE FRAME-NAME                                               */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

ASSIGN 
       fiAntArt:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Sett nye leveringuker */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
  ASSIGN rsBut
         obAlleBut    = rsBut = 2
         ocLevUkeList = (IF LevUke1:SCREEN-VALUE NE ? THEN LevUke1:SCREEN-VALUE ELSE "") + "|"
                      + (IF LevUke2:SCREEN-VALUE NE ? THEN LevUke2:SCREEN-VALUE ELSE "") + "|"
                      + (IF LevUke3:SCREEN-VALUE NE ? THEN LevUke3:SCREEN-VALUE ELSE "") + "|"
                      + (IF LevUke4:SCREEN-VALUE NE ? THEN LevUke4:SCREEN-VALUE ELSE "") 
         obOk         = YES
         .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

{incl/frametrigg.i}

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.

  RUN InitWindow.
  
  FRAME {&FRAME-NAME}:HIDDEN = FALSE.

  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
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
  DISPLAY LevUke1 LevUke2 LevUke3 LevUke4 rsBut fiAntArt 
      WITH FRAME Dialog-Frame.
  ENABLE LevUke1 LevUke2 LevUke3 LevUke4 rsBut fiAntArt Btn_OK Btn_Cancel 
      WITH FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitWindow Dialog-Frame 
PROCEDURE InitWindow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  ASSIGN fiAntArt:SCREEN-VALUE  = STRING(iiNumSelected)
         LevUke1:LIST-ITEMS     = ihLevUkeList:LIST-ITEMS 
         LevUke2:LIST-ITEMS     = ihLevUkeList:LIST-ITEMS 
         LevUke3:LIST-ITEMS     = ihLevUkeList:LIST-ITEMS 
         LevUke4:LIST-ITEMS     = ihLevUkeList:LIST-ITEMS 
         .
  LocalTranslation().
  DYNAMIC-FUNCTION("initTranslation",FRAME {&FRAME-NAME}:HANDLE).

  IF icButikkNr = ? THEN
    ASSIGN rsBut:SCREEN-VALUE = "2"
           rsBut:SENSITIVE    = NO.
  ELSE DO:      
    rsBut:RADIO-BUTTONS = REPLACE(rsBut:RADIO-BUTTONS,ENTRY(1,rsBut:RADIO-BUTTONS),ENTRY(1,rsBut:RADIO-BUTTONS) + " (" + icButikkNr + ")").
    IF NOT ibAlleBut THEN
      rsBut:SENSITIVE = NO.
  END.

  APPLY "entry" TO LevUke1.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LocalTranslation Dialog-Frame 
FUNCTION LocalTranslation RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose: Set english labels
    Notes:  
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  IF DYNAMIC-FUNCTION("Scandinavian") THEN RETURN FALSE.
  ELSE 
    ASSIGN Btn_Cancel:LABEL          = "Cancel" 
           FRAME {&FRAME-NAME}:TITLE = "Select"
           .
  
  RETURN TRUE.  
END.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

