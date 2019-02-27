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
DEF INPUT        PARAM icMessage     AS CHAR NO-UNDO.
DEF INPUT        PARAM icFieldSpec   AS CHAR NO-UNDO.
DEF INPUT-OUTPUT PARAM iocValue      AS CHAR NO-UNDO.
DEF OUTPUT       PARAM oiReturn      AS INT  NO-UNDO.
/* Local Variable Definitions ---                                       */

DEF VAR hInputField AS HANDLE NO-UNDO.
DEF VAR hParent     AS HANDLE NO-UNDO.
DEF VAR hParentWin  AS HANDLE NO-UNDO.

DEF TEMP-TABLE ttFlds
    FIELD hInputField AS HANDLE
    FIELD hLabel      AS HANDLE
    FIELD bCal        AS LOG
    .

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
&Scoped-Define DISPLAYED-OBJECTS fi-cMessage 

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
DEFINE BUTTON btnCalMyDate 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 3.8 BY .81.

DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Avbryt" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE fi-cMessage AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 93 BY .95
     FONT 6 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     btnCalMyDate AT ROW 4.62 COL 42.8
     Btn_OK AT ROW 5.1 COL 63.8
     Btn_Cancel AT ROW 5.1 COL 79.4
     fi-cMessage AT ROW 1.67 COL 2 NO-LABEL
     SPACE(1.42) SKIP(3.68)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Angi verdi"
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

/* SETTINGS FOR BUTTON btnCalMyDate IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       btnCalMyDate:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN fi-cMessage IN FRAME Dialog-Frame
   ALIGN-L                                                              */
ASSIGN 
       fi-cMessage:RESIZABLE IN FRAME Dialog-Frame        = TRUE.

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
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Angi verdi */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalMyDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalMyDate Dialog-Frame
ON CHOOSE OF btnCalMyDate IN FRAME Dialog-Frame /* ... */
DO:
  RUN dCal.w (hInputField).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
  DEF VAR ix AS INT NO-UNDO.
  oiReturn = 2.
  iocValue = "".
  FOR EACH ttFlds:
    ix = ix + 1.
    iocValue = iocValue + (IF ix > 1 THEN "|" ELSE "")
             + IF CAN-DO("CHARACTER,LOGICAL",ttFlds.hInputField:DATA-TYPE) THEN ttFlds.hInputField:INPUT-VALUE
               ELSE ttFlds.hInputField:SCREEN-VALUE.
  END.              
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
  hParent = SOURCE-PROCEDURE.
  RUN InitWindow.
  hParentWin = hParent:CURRENT-WINDOW NO-ERROR.
  IF VALID-HANDLE(hParentWin) THEN
    hParentWin:MOVE-TO-TOP().
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ClearDateTime Dialog-Frame
PROCEDURE ClearDateTime:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihInputField AS HANDLE NO-UNDO.

ihInputField:SCREEN-VALUE = ?.


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
  DISPLAY fi-cMessage 
      WITH FRAME Dialog-Frame.
  ENABLE Btn_OK Btn_Cancel fi-cMessage 
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
DEF VAR iBlanks    AS INT    NO-UNDO.
DEF VAR hSideLabel AS HANDLE NO-UNDO.
DEF VAR cType      AS CHAR   NO-UNDO.
DEF VAR cFormat    AS CHAR   NO-UNDO.
DEF VAR ix         AS INT    NO-UNDO.
DEF VAR iy         AS INT    NO-UNDO.
DEF VAR cFieldSpec AS CHAR   NO-UNDO.
DEF VAR cDataTypes AS CHAR   NO-UNDO INIT "CHARACTER,DATE,DATE-TIME,DECIMAL,INTEGER,LOGICAL".
DEF VAR cLabel     AS CHAR   NO-UNDO.
DEF VAR cList      AS CHAR   NO-UNDO.
DEF VAR cBuffersAndFields AS CHAR NO-UNDO.
DEF VAR cQueryCriteria AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  ASSIGN iBlanks                      = IF LENGTH(icMessage) < 90 THEN 90 - LENGTH(icMessage) ELSE 2
         iBlanks                      = iBlanks / 2
         fi-cMessage:SCREEN-VALUE     = FILL(" ",iBlanks) + icMessage 
         .

  DO ix = 1 TO NUM-ENTRIES(icFieldSpec,"¤"):
    cFieldSpec = ENTRY(ix,icFieldSpec,"¤").
    cLabel = "".
    
    IF NUM-ENTRIES(cFieldSpec,"|") > 1 AND NOT CAN-DO(cDataTypes,ENTRY(1,cFieldSpec,"|")) THEN
      ASSIGN cLabel     = ENTRY(1,cFieldSpec,"|")
             cFieldSpec = SUBSTR(cFieldSpec,INDEX(cFieldSpec,"|") + 1).
    
    CREATE ttFlds.
    ASSIGN FRAME {&FRAME-NAME}:HEIGHT-PIXELS = FRAME {&FRAME-NAME}:HEIGHT-PIXELS + (ix - 1) * 23 
           Btn_Cancel:Y  = Btn_Cancel:Y + (ix - 1) * 23
           Btn_OK:Y      = Btn_OK:Y + (ix - 1) * 23
           .
    
    IF NUM-ENTRIES(cFieldSpec,"|") = 1 OR (NUM-ENTRIES(cFieldSpec,"|") > 1 AND ENTRY(2,cFieldSpec,"|") = "")  THEN DO:
      cType = ENTRY(1,cFieldSpec,"|").
      IF cType BEGINS "CHAR" THEN cFormat = "x(8)".
      ELSE IF cType BEGINS "DEC" THEN cFormat = "->>>,>>9.99".
      ELSE IF cType BEGINS "INT" THEN cFormat = "->>>>>>>9".
      ELSE IF cType = "DATE" THEN cFormat = "99/99/9999".
      ELSE IF cType BEGINS "DATE" THEN cFormat = "99/99/9999 HH:MM".
      ELSE IF cType BEGINS "LOG" THEN cFormat = "Yes/No".
    END.  
    ELSE cFormat = ENTRY(2,cFieldSpec,"|").
  
    IF NUM-ENTRIES(cFieldSpec,"|") > 3 THEN DO:
      cList = "".
      IF ENTRY(4,cFieldSpec,"|") = "" AND ENTRY(5,cFieldSpec,"|") = "" THEN DO: 
        cList = "||".
        IF NUM-ENTRIES(ENTRY(6,cFieldSpec,"|"),";") > 1 THEN
          ASSIGN cBuffersAndFields = ENTRY(6,cFieldSpec,"|")
                 cQueryCriteria = ENTRY(7,cFieldSpec,"|").
        ELSE DO:
          DO iy = 6 TO NUM-ENTRIES(cFieldSpec,"|"):
            cList = cList + ENTRY(iy,cFieldSpec,"|") + "|".
          END.  
          cList = RIGHT-TRIM(cList,"|").
        END.            
      END.
      ELSE DO iy = 4 TO NUM-ENTRIES(cFieldSpec,"|"):
        cList = cList + (IF cList NE "" THEN "|" ELSE "") + ENTRY(iy,cFieldSpec,"|").
      END.  
              
      CREATE COMBO-BOX ttFlds.hInputField 
             ASSIGN DELIMITER        = "|"
                    DATA-TYPE        = "CHARACTER"
                    FORMAT           = "x(256)"
                    SUBTYPE          = "DROP-DOWN-LIST"
                    LIST-ITEM-PAIRS  = IF cList NE "||" AND cList NE "" THEN cList ELSE cList + DYNAMIC-FUNCTION("getFieldList",cBuffersAndFields,cQueryCriteria)
                    INNER-LINES      = 50
                    X                = 100
                    Y                = 40 + (ix - 1) * 23
                    WIDTH-PIXELS     = IF ENTRY(1,cFieldSpec,"|") = "CHARACTER" THEN 300 ELSE 100
                    HEIGHT-PIXELS    = 21
                    FRAME            = FRAME {&FRAME-NAME}:HANDLE
                    VISIBLE          = YES
                    SENSITIVE        = TRUE
                    .
    END.  
    ELSE  
      CREATE FILL-IN ttFlds.hInputField
             ASSIGN DATA-TYPE        = ENTRY(1,cFieldSpec,"|")
                    FORMAT           = cFormat
                    X                = 100
                    Y                = 40 + (ix - 1) * 23
                    WIDTH-PIXELS     = IF ENTRY(1,cFieldSpec,"|") = "CHARACTER" THEN 300 ELSE 100
                    HEIGHT-PIXELS    = 21
                    FRAME            = FRAME {&FRAME-NAME}:HANDLE
                    VISIBLE          = YES
                    SENSITIVE        = TRUE
                    .
                    
    IF cLabel NE "" THEN DO:
      CREATE TEXT ttFlds.hLabel
             ASSIGN SCREEN-VALUE     = TRIM(cLabel,":") + ":"
                    X                = 30
                    Y                = 40 + (ix - 1) * 23
                    HEIGHT-PIXELS    = 21
                    FRAME            = FRAME {&FRAME-NAME}:HANDLE
                    VISIBLE          = YES
                    .
      ttFlds.hInputField:SIDE-LABEL-HANDLE = ttFlds.hLabel.       
    END.  
  
    IF iocValue NE "" THEN
      ttFlds.hInputField:SCREEN-VALUE = iocValue.
    IF ENTRY(1,cFieldSpec,"|") = "DATE" THEN DO:
      ASSIGN btnCalMyDate:SENSITIVE = TRUE
             btnCalMyDate:HIDDEN    = FALSE
             btnCalMyDate:X         = ttFlds.hInputField:X + 101
             btnCalMyDate:Y         = ttFlds.hInputField:Y + 2
             ttFlds.bCal            = YES
             .
      ON BACKSPACE OF ttFlds.hInputField PERSISTENT RUN ClearDateTime IN THIS-PROCEDURE (ttFlds.hInputField).
      ON DELETE-CHARACTER OF ttFlds.hInputField PERSISTENT RUN ClearDateTime IN THIS-PROCEDURE (ttFlds.hInputField).
      ON " " OF ttFlds.hInputField PERSISTENT  RUN ClearDateTime IN THIS-PROCEDURE (ttFlds.hInputField).
      ON "t" OF ttFlds.hInputField PERSISTENT  RUN TodaysDate IN THIS-PROCEDURE (ttFlds.hInputField).
      ON "d" OF ttFlds.hInputField PERSISTENT  RUN TodaysDate IN THIS-PROCEDURE (ttFlds.hInputField).
      ON "+" OF ttFlds.hInputField PERSISTENT  RUN TodaysDate IN THIS-PROCEDURE (ttFlds.hInputField).
      ON "-" OF ttFlds.hInputField PERSISTENT  RUN TodaysDate IN THIS-PROCEDURE (ttFlds.hInputField).             
    END.         
  
    IF NUM-ENTRIES(cFieldSpec,"|") > 2 AND ENTRY(3,cFieldSpec,"|") NE "" THEN
      ttFlds.hInputField:SCREEN-VALUE = ENTRY(3,cFieldSpec,"|") NO-ERROR.
      
    IF ix = 1 THEN hInputField = ttFlds.hInputField.  
  END.

  LocalTranslation().
  DYNAMIC-FUNCTION("InitTranslation",FRAME {&FRAME-NAME}:HANDLE).
  APPLY "entry" TO hInputField.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TodaysDate Dialog-Frame
PROCEDURE TodaysDate:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihInputField AS HANDLE NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  IF DATE(ihInputField:SCREEN-VALUE) = ? THEN
    ihInputField:SCREEN-VALUE = STRING(TODAY).
  IF LAST-EVENT:FUNCTION = "+" THEN
    ihInputField:SCREEN-VALUE = STRING(DATE(ihInputField:SCREEN-VALUE) + 1).
  ELSE IF LAST-EVENT:FUNCTION = "-" THEN
    ihInputField:SCREEN-VALUE = STRING(DATE(ihInputField:SCREEN-VALUE) - 1).
  ELSE
    ihInputField:SCREEN-VALUE = STRING(TODAY).
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
           FRAME Dialog-Frame:TITLE = "Set value"
           .
  
  RETURN TRUE.  
END.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

