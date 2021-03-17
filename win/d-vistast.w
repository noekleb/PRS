&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------
  File:              d-keytest.w
  Author:            Sturla Johnsen
  Created:           juni 1998
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
/* Parameters Definitions ---                                           */
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN  
&ELSE
&ENDIF    
/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-2 BUTTON-Dummy 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-Dummy 
     LABEL "Button 1" 
     SIZE .2 BY .05.

DEFINE VARIABLE FILL-IN-Code AS INTEGER FORMAT ">>>>>>9":U INITIAL 0 
     LABEL "Code" 
     VIEW-AS FILL-IN 
     SIZE 33 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FILL-IN-Column AS DECIMAL FORMAT ">>>9,999":U INITIAL 0 
     LABEL "Column" 
     VIEW-AS FILL-IN 
     SIZE 33 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FILL-IN-Event-Type AS CHARACTER FORMAT "X(200)":U 
     LABEL "Event-type" 
     VIEW-AS FILL-IN 
     SIZE 33 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FILL-IN-Function AS CHARACTER FORMAT "X(200)":U 
     LABEL "Function" 
     VIEW-AS FILL-IN 
     SIZE 33 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FILL-IN-KbLabel AS CHARACTER FORMAT "X(256)":U 
     LABEL "Kblabel" 
     VIEW-AS FILL-IN 
     SIZE 34 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FILL-IN-Keycode AS CHARACTER FORMAT "X(256)":U 
     LABEL "Keycode" 
     VIEW-AS FILL-IN 
     SIZE 34 BY .95
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FILL-IN-Keyfunc AS CHARACTER FORMAT "X(256)":U 
     LABEL "Keyfunction" 
     VIEW-AS FILL-IN 
     SIZE 34 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FILL-IN-KeyLabel AS CHARACTER FORMAT "X(256)":U 
     LABEL "Keylabel" 
     VIEW-AS FILL-IN 
     SIZE 34 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FILL-IN-Label AS CHARACTER FORMAT "X(200)":U 
     LABEL "Label" 
     VIEW-AS FILL-IN 
     SIZE 33 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FILL-IN-Lastkey AS CHARACTER FORMAT "X(256)":U 
     LABEL "Lastkey" 
     VIEW-AS FILL-IN 
     SIZE 34 BY .95
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FILL-IN-On-Frame-Border AS LOGICAL FORMAT "JA/NEI":U INITIAL NO 
     LABEL "On-frame-border" 
     VIEW-AS FILL-IN 
     SIZE 33 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FILL-IN-Row AS DECIMAL FORMAT ">>>9,999":U INITIAL 0 
     LABEL "Row" 
     VIEW-AS FILL-IN 
     SIZE 33 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FILL-IN-Type AS CHARACTER FORMAT "X(200)":U 
     LABEL "Type" 
     VIEW-AS FILL-IN 
     SIZE 33 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FILL-IN-X AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     LABEL "X" 
     VIEW-AS FILL-IN 
     SIZE 33 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FILL-IN-Y AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     LABEL "Y" 
     VIEW-AS FILL-IN 
     SIZE 33 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 53 BY 10.48.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 51 BY 5.71.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     FILL-IN-Event-Type AT ROW 1.95 COL 8.6
     FILL-IN-Lastkey AT ROW 2.19 COL 69 COLON-ALIGNED
     FILL-IN-Code AT ROW 2.91 COL 18 COLON-ALIGNED
     FILL-IN-Keycode AT ROW 3.14 COL 69 COLON-ALIGNED
     FILL-IN-Function AT ROW 3.86 COL 10.6
     FILL-IN-Keyfunc AT ROW 4.1 COL 69 COLON-ALIGNED
     FILL-IN-Label AT ROW 4.81 COL 13.6
     FILL-IN-KeyLabel AT ROW 5.05 COL 69 COLON-ALIGNED
     FILL-IN-Type AT ROW 5.76 COL 14
     FILL-IN-KbLabel AT ROW 6 COL 69 COLON-ALIGNED
     FILL-IN-Column AT ROW 6.71 COL 18 COLON-ALIGNED
     FILL-IN-Row AT ROW 7.67 COL 18 COLON-ALIGNED
     FILL-IN-X AT ROW 8.62 COL 18 COLON-ALIGNED
     FILL-IN-Y AT ROW 9.57 COL 18 COLON-ALIGNED
     FILL-IN-On-Frame-Border AT ROW 10.52 COL 3.6
     BUTTON-Dummy AT ROW 10.71 COL 3
     "Last-evet" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 1.24 COL 3
     "Lastkey" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 1.24 COL 57
     RECT-1 AT ROW 1.48 COL 2
     RECT-2 AT ROW 1.48 COL 56
     "Bruk tastaturet eller mus, se resultatet. Trykk x (evt. flere ganger) når du er ferdig." VIEW-AS TEXT
          SIZE 97 BY .62 AT ROW 12.43 COL 7
          FONT 6
     SPACE(3.00) SKIP(0.00)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Viser informasjon om siste tastetrykk".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
                                                                        */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-Code IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
/* SETTINGS FOR FILL-IN FILL-IN-Column IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
/* SETTINGS FOR FILL-IN FILL-IN-Event-Type IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
/* SETTINGS FOR FILL-IN FILL-IN-Function IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
/* SETTINGS FOR FILL-IN FILL-IN-KbLabel IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
/* SETTINGS FOR FILL-IN FILL-IN-Keycode IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
/* SETTINGS FOR FILL-IN FILL-IN-Keyfunc IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
/* SETTINGS FOR FILL-IN FILL-IN-KeyLabel IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
/* SETTINGS FOR FILL-IN FILL-IN-Label IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
/* SETTINGS FOR FILL-IN FILL-IN-Lastkey IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
/* SETTINGS FOR FILL-IN FILL-IN-On-Frame-Border IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
/* SETTINGS FOR FILL-IN FILL-IN-Row IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
/* SETTINGS FOR FILL-IN FILL-IN-Type IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
/* SETTINGS FOR FILL-IN FILL-IN-X IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
/* SETTINGS FOR FILL-IN FILL-IN-Y IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Viser informasjon om siste tastetrykk */
DO:
  APPLY "U1" TO SELF.
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
DO:
   
  {lng.i} RUN enable_UI.

  REPEAT:
     WAIT-FOR ANY-KEY,
              MOUSE-SELECT-DOWN,
              MOUSE-SELECT-UP,
              MOUSE-SELECT-CLICK,
              MOUSE-SELECT-DBLCLICK,
              MOUSE-MENU-DOWN,
              MOUSE-MENU-UP,
              MOUSE-MENU-CLICK,
              MOUSE-MENU-DBLCLICK,
              MOUSE-EXTEND-DOWN,
              MOUSE-EXTEND-UP,
              MOUSE-EXTEND-CLICK,
              MOUSE-EXTEND-DBLCLICK,
              MOUSE-MOVE-DOWN,
              MOUSE-MOVE-UP,
              MOUSE-MOVE-CLICK,
              MOUSE-MOVE-DBLCLICK 
     OF FRAME {&FRAME-NAME}.
     DO WITH FRAME {&FRAME-NAME}:
        ASSIGN FILL-IN-Code            = LAST-EVENT:CODE
               FILL-IN-Column          = LAST-EVENT:COLUMN
               FILL-IN-Event-Type      = LAST-EVENT:EVENT-TYPE
               FILL-IN-Function        = LAST-EVENT:FUNCTION
               FILL-IN-Label           = LAST-EVENT:LABEL
               FILL-IN-On-Frame-Border = LAST-EVENT:ON-FRAME-BORDER
               FILL-IN-Row             = LAST-EVENT:ROW
               FILL-IN-Type            = LAST-EVENT:TYPE
               FILL-IN-X               = LAST-EVENT:X
               FILL-IN-Y               = LAST-EVENT:Y
               FILL-IN-Lastkey         = STRING(LASTKEY)
               FILL-IN-Keycode         = STRING(LASTKEY)
               FILL-IN-Keyfunc         = KEYFUNCT(LASTKEY)
               FILL-IN-KeyLabel        = KEYLABEL(LASTKEY)
               FILL-IN-KbLabel         = KBLABEL(KEYFUNCT(LASTKEY)).
        DISPL  FILL-IN-Code 
               FILL-IN-Column 
               FILL-IN-Event-Type 
               FILL-IN-Function 
               FILL-IN-Label 
               FILL-IN-On-Frame-Border 
               FILL-IN-Row 
               FILL-IN-Type 
               FILL-IN-X 
               FILL-IN-Y
               FILL-IN-Lastkey
               FILL-IN-Keycode
               FILL-IN-Keyfunc
               FILL-IN-KeyLabel
               FILL-IN-KbLabel.
    
      END.
      IF CAN-DO("x,X",KEYFUNCT(LASTKEY)) THEN LEAVE MAIN-BLOCK.
   END.
END.
RUN disable_UI.
RETURN "".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame _DEFAULT-DISABLE
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame _DEFAULT-ENABLE
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
  ENABLE RECT-2 BUTTON-Dummy 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


