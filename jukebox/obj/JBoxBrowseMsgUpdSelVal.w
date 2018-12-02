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
DEF INPUT  PARAM icMessage     AS CHAR NO-UNDO.
DEF INPUT  PARAM iiNumSelected AS INT  NO-UNDO.
DEF INPUT  PARAM iiNumTotal    AS INT  NO-UNDO.
DEF INPUT  PARAM icFieldSpec   AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocValue       AS CHAR NO-UNDO.
DEF OUTPUT PARAM oiReturn      AS INT  NO-UNDO.
/* Local Variable Definitions ---                                       */


DEF VAR hInputField AS HANDLE NO-UNDO.
DEF VAR cLookupDef  AS CHAR   NO-UNDO.
DEF VAR cLookupQry  AS CHAR   NO-UNDO.
DEF VAR cLookupRet  AS CHAR   NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn_OK Btn_Cancel fi-cMessage 
&Scoped-Define DISPLAYED-OBJECTS fi-cNumSelected fi-cMessage 

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
     LABEL "Avbryt" 
     SIZE 15 BY 1.15
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.15
     BGCOLOR 8 .

DEFINE VARIABLE fi-cMessage AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 93 BY .96
     FONT 6 NO-UNDO.

DEFINE VARIABLE fi-cNumSelected AS CHARACTER FORMAT "X(256)":U 
     LABEL "Antall valgt" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     fi-cNumSelected AT ROW 5.04 COL 26 COLON-ALIGNED
     Btn_OK AT ROW 6.15 COL 63.86
     Btn_Cancel AT ROW 6.15 COL 79.43
     fi-cMessage AT ROW 1.65 COL 2 NO-LABEL
     SPACE(1.39) SKIP(4.81)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Behandle poster"
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
   FRAME-NAME                                                           */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN fi-cMessage IN FRAME Dialog-Frame
   ALIGN-L                                                              */
ASSIGN 
       fi-cMessage:RESIZABLE IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN fi-cNumSelected IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       fi-cNumSelected:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

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
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Behandle poster */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
  oiReturn = 2.
  IF VALID-HANDLE(hInputField) THEN
    ocValue  = IF CAN-DO("CHARACTER,LOGICAL",hInputField:DATA-TYPE) THEN hInputField:INPUT-VALUE
               ELSE hInputField:SCREEN-VALUE.
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
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CalendarLookup Dialog-Frame 
PROCEDURE CalendarLookup :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN dCal.w (hInputField).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dataLookup Dialog-Frame 
PROCEDURE dataLookup :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cLookupValue  AS CHAR NO-UNDO.

cLookupValue = cLookupRet.

RUN JBoxDLookup.w (cLookupDef
                ,cLookupQry
                ,INPUT-OUTPUT cLookupValue).

IF cLookupValue NE "" THEN
  hInputField:SCREEN-VALUE = ENTRY(1,cLookupValue,"|").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  DISPLAY fi-cNumSelected fi-cMessage 
      WITH FRAME Dialog-Frame.
  ENABLE Btn_OK Btn_Cancel fi-cMessage 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
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
DEF VAR iBlanks    AS INT    NO-UNDO.
DEF VAR hButton    AS HANDLE NO-UNDO.
DEF VAR ix         AS INT    NO-UNDO.
DEF VAR cListItems AS CHAR   NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  ASSIGN iBlanks                      = IF LENGTH(icMessage) < 90 THEN 90 - LENGTH(icMessage) ELSE 2
         iBlanks                      = iBlanks / 2
         fi-cMessage:SCREEN-VALUE     = FILL(" ",iBlanks) + icMessage 
         fi-cNumSelected:SCREEN-VALUE = STRING(iiNumSelected)
         .

  IF NUM-ENTRIES(icFieldSpec,"|") > 3 AND ENTRY(4,icFieldSpec,"|") = "COMBO-BOX"  THEN DO:
    IF NUM-ENTRIES(icFieldSpec,"|") > 5 THEN 
      DO ix = 6 TO NUM-ENTRIES(icFieldSpec,"|"):
        cListItems = cListItems + (IF cListItems NE "" THEN "|" ELSE "") + ENTRY(ix,icFieldSpec,"|").
      END.
    IF cListItems NE "" THEN
      cListItems = ENTRY(5,icFieldSpec,"|") + "|" + cListItems.
    ELSE
      cListItems = REPLACE(ENTRY(5,icFieldSpec,"|"),",","|").

    CREATE COMBO-BOX hInputField
      ASSIGN 
       FRAME            = FRAME {&FRAME-NAME}:HANDLE
       DATA-TYPE        = ENTRY(1,icFieldSpec,"|")
       FORMAT           = ENTRY(2,icFieldSpec,"|")
       X                = IF ENTRY(1,icFieldSpec,"|") = "CHARACTER" THEN 100 ELSE 170
       Y                = 40
       WIDTH-PIXELS     = IF ENTRY(1,icFieldSpec,"|") = "CHARACTER" THEN 250 ELSE 100
       INNER-LINES      = 25
       VISIBLE          = TRUE
       SENSITIVE        = TRUE
       DELIMITER        = "|"
       LIST-ITEM-PAIRS  = cListItems
       .

    IF ENTRY(3,icFieldSpec,"|") NE "" THEN
      hInputField:SCREEN-VALUE = ENTRY(3,icFieldSpec,"|").
  END.

  ELSE IF NUM-ENTRIES(icFieldSpec,"|") > 1 THEN DO:
    CREATE FILL-IN hInputField
           ASSIGN FRAME            = FRAME {&FRAME-NAME}:HANDLE
                  DATA-TYPE        = ENTRY(1,icFieldSpec,"|")
                  FORMAT           = ENTRY(2,icFieldSpec,"|")
                  X                = 190
                  Y                = 40
                  WIDTH-PIXELS     = IF ENTRY(1,icFieldSpec,"|") = "CHARACTER" THEN 200 ELSE 100
                  HEIGHT-PIXELS    = 21
                  VISIBLE          = TRUE
                  SENSITIVE        = TRUE
                  .

/*     IF NUM-ENTRIES(icFieldSpec,"|") > 2 THEN                         */
/*       hInputField:SCREEN-VALUE = ENTRY(3,icFieldSpec,"|") NO-ERROR.  */

    IF ENTRY(1,icFieldSpec,"|") = "date" AND NUM-ENTRIES(icFieldSpec,"|") < 4 THEN
      CREATE BUTTON hButton
             ASSIGN FRAME         = FRAME {&FRAME-NAME}:HANDLE
                    WIDTH-PIXELS  = 17
                    TAB-STOP      = FALSE
                    NO-FOCUS      = TRUE
                    HEIGHT-PIXELS = hInputField:HEIGHT-PIXELS 
                    X             = hInputField:X + hInputField:WIDTH-PIXELS + 1 
                    Y             = hInputField:Y 
                    LABEL         = "..."
                    SENSITIVE     = TRUE
                    HIDDEN        = FALSE
             TRIGGERS:
               ON CHOOSE PERSISTENT RUN CalendarLookup.
             END TRIGGERS.

    IF NUM-ENTRIES(icFieldSpec,"|") > 2 THEN
      hInputField:SCREEN-VALUE = ENTRY(3,icFieldSpec,"|") NO-ERROR.

    IF NUM-ENTRIES(icFieldSpec,"|") > 3 THEN DO:
      CREATE BUTTON hButton
             ASSIGN FRAME         = FRAME {&FRAME-NAME}:HANDLE
                    WIDTH-PIXELS  = 17
                    TAB-STOP      = FALSE
                    NO-FOCUS      = TRUE
                    HEIGHT-PIXELS = hInputField:HEIGHT-PIXELS 
                    X             = hInputField:X + hInputField:WIDTH-PIXELS + 1 
                    Y             = hInputField:Y 
                    LABEL         = "..."
                    SENSITIVE     = TRUE
                    HIDDEN        = FALSE
             TRIGGERS:
               ON CHOOSE PERSISTENT RUN dataLookup.
             END TRIGGERS.
      ASSIGN cLookupDef = REPLACE(ENTRY(4,icFieldSpec,"|"),"¤","|")  
             cLookupQry = ENTRY(5,icFieldSpec,"|")  
             cLookupRet = ENTRY(6,icFieldSpec,"|")
                          .
    END.
  END.

  LocalTranslation().
  DYNAMIC-FUNCTION("InitTranslation",FRAME {&FRAME-NAME}:HANDLE).
  IF VALID-HANDLE(hInputField) THEN
    APPLY "entry" TO hInputField.
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
    ASSIGN Btn_Cancel:LABEL         = "Cancel" 
           fi-cNumSelected:LABEL    = "Selected rows"
           FRAME Dialog-Frame:TITLE = "Select records"
           .
  
  RETURN TRUE.  
END.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

