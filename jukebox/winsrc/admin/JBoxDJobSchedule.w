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
&IF DEFINED(UIB_is_Running) NE 0 &THEN
  DEF VAR icMode             AS CHAR NO-UNDO INIT "new".
  DEF VAR iiJobDefinitionId  AS INT  NO-UNDO INIT 1.
  DEF VAR iiJobScheduleId    AS INT  NO-UNDO.
  DEF VAR iiJobQueueId       AS INT  NO-UNDO INIT 1.
  DEF VAR obOk               AS LOG  NO-UNDO.
&ELSE
  DEF INPUT  PARAM icMode             AS CHAR NO-UNDO.
  DEF INPUT  PARAM iiJobScheduleId    AS INT  NO-UNDO.
  DEF INPUT  PARAM iiJobDefinitionId  AS INT  NO-UNDO.
  DEF INPUT  PARAM iiJobQueueId       AS INT  NO-UNDO.
  DEF OUTPUT PARAM obOk               AS LOG  NO-UNDO.
&ENDIF

/* Local Variable Definitions ---                                       */

DEF VAR hFieldMap   AS HANDLE NO-UNDO.
DEF VAR hQuery      AS HANDLE NO-UNDO.
DEF VAR bScand      AS LOG    NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 iJBoxJobQueueId ~
iJBoxJobDefinitionId iFrequency cFrequencyType cFrequency tbMonday ~
tbTuesday tbWednesday tbThursday tbSaturday tbSunday tbFriday cMonth dStart ~
dEnd cDayList Btn_OK Btn_Cancel fiFrequencyDesc 
&Scoped-Define DISPLAYED-OBJECTS iJBoxJobQueueId iJBoxJobDefinitionId ~
iFrequency cFrequencyType cFrequency tbMonday tbTuesday tbWednesday ~
tbThursday tbSaturday tbSunday tbFriday cMonth dStart dEnd cDayList ~
fiFrequencyDesc 

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setFrequency Dialog-Frame 
FUNCTION setFrequency RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setFrequencyType Dialog-Frame 
FUNCTION setFrequencyType RETURNS LOGICAL
  ( INPUT icValue AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "C&ancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE cMonth AS INTEGER FORMAT "->,>>>,>>9" INITIAL 0 
     LABEL "Måned" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "0",1
     DROP-DOWN-LIST
     SIZE 18 BY 1.

DEFINE VARIABLE iJBoxJobDefinitionId AS CHARACTER FORMAT "X(256)" INITIAL "0" 
     LABEL "Oppgave" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 52 BY 1.

DEFINE VARIABLE iJBoxJobQueueId AS CHARACTER FORMAT "X(256)" INITIAL "0" 
     LABEL "Jobbkø" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 52 BY 1.

DEFINE VARIABLE cDayList AS CHARACTER FORMAT "x(40)" 
     LABEL "Day-list" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1.

DEFINE VARIABLE dEnd AS DATE FORMAT "99/99/9999" 
     LABEL "Slutt" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE dStart AS DATE FORMAT "99/99/9999" 
     LABEL "Start" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE fiFrequencyDesc AS CHARACTER FORMAT "X(256)":U INITIAL "dag" 
      VIEW-AS TEXT 
     SIZE 14 BY .62 NO-UNDO.

DEFINE VARIABLE iFrequency AS INTEGER FORMAT ">9" INITIAL 1 
     VIEW-AS FILL-IN 
     SIZE 6.6 BY 1.

DEFINE VARIABLE cFrequency AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "1", 1
     SIZE 18.2 BY 6.43 NO-UNDO.

DEFINE VARIABLE cFrequencyType AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "1", 1
     SIZE 19 BY 7.19 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 96 BY 8.33.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 96 BY 1.33.

DEFINE VARIABLE tbFriday AS LOGICAL INITIAL no 
     LABEL "Fredag" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE tbMonday AS LOGICAL INITIAL no 
     LABEL "Mandag" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE tbSaturday AS LOGICAL INITIAL no 
     LABEL "Lørdag" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE tbSunday AS LOGICAL INITIAL no 
     LABEL "Søndag" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE tbThursday AS LOGICAL INITIAL no 
     LABEL "Torsdag" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE tbTuesday AS LOGICAL INITIAL no 
     LABEL "Tirsdag" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE tbWednesday AS LOGICAL INITIAL no 
     LABEL "Onsdag" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     iJBoxJobQueueId AT ROW 1.14 COL 17 COLON-ALIGNED
     iJBoxJobDefinitionId AT ROW 2.14 COL 17 COLON-ALIGNED
     iFrequency AT ROW 3.71 COL 44.8 COLON-ALIGNED NO-LABEL
     cFrequencyType AT ROW 3.86 COL 5 NO-LABEL
     cFrequency AT ROW 3.95 COL 26.8 NO-LABEL
     tbMonday AT ROW 5.29 COL 33
     tbTuesday AT ROW 5.29 COL 48.2
     tbWednesday AT ROW 5.29 COL 63.2
     tbThursday AT ROW 5.29 COL 79
     tbSaturday AT ROW 6.48 COL 48.2
     tbSunday AT ROW 6.48 COL 63.4
     tbFriday AT ROW 6.52 COL 33
     cMonth AT ROW 7.19 COL 77 COLON-ALIGNED
     dStart AT ROW 11.95 COL 15 COLON-ALIGNED
     dEnd AT ROW 11.95 COL 41.6 COLON-ALIGNED
     cDayList AT ROW 13.38 COL 13 COLON-ALIGNED
     Btn_OK AT ROW 13.91 COL 66.2
     Btn_Cancel AT ROW 13.91 COL 82.2
     fiFrequencyDesc AT ROW 3.91 COL 52.6 COLON-ALIGNED NO-LABEL
     RECT-1 AT ROW 3.24 COL 2
     RECT-2 AT ROW 11.81 COL 2
     SPACE(0.99) SKIP(2.14)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Title"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Dialog-Frame 
/* ************************* Included-Libraries *********************** */

{incl/DevMode.i}
{incl/CustDevMode.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME                                                           */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Title */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
  RUN InvokeMethod(hFieldMap,"SaveRecord").
  obOk = YES.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplayRecord Dialog-Frame 
PROCEDURE DisplayRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
setFrequencyType(IF hFieldMap:AVAIL THEN hFieldMap:BUFFER-FIELD("cFrequencyType"):BUFFER-VALUE ELSE "").
RUN SUPER.

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
  DISPLAY iJBoxJobQueueId iJBoxJobDefinitionId iFrequency cFrequencyType 
          cFrequency tbMonday tbTuesday tbWednesday tbThursday tbSaturday 
          tbSunday tbFriday cMonth dStart dEnd cDayList fiFrequencyDesc 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-1 RECT-2 iJBoxJobQueueId iJBoxJobDefinitionId iFrequency 
         cFrequencyType cFrequency tbMonday tbTuesday tbWednesday tbThursday 
         tbSaturday tbSunday tbFriday cMonth dStart dEnd cDayList Btn_OK 
         Btn_Cancel fiFrequencyDesc 
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
  ASSIGN iJBoxJobDefinitionId:DELIMITER = "|"
         iJBoxJobDefinitionId:LIST-ITEM-PAIRS = DYNAMIC-FUNCTION("getFieldList",
                                                             "JBoxJobDefinition;cJobDescription;iJBoxJobDefinitionId",
                                                             "WHERE true")
         iJBoxJobQueueId:DELIMITER = "|"
         iJBoxJobQueueId:LIST-ITEM-PAIRS = DYNAMIC-FUNCTION("getFieldList",
                                                             "JBoxJobQueue;cQueueName;iJBoxJobQueueId",
                                                             "WHERE true")
         cMonth:LIST-ITEM-PAIRS = (IF bScand THEN "Januar,1,Februar,2,Mars,3,April,4,Mai,5,Juni,6,Juli,7,August,8,September,9,Oktober,10,November,11,Desember,12"
                                   ELSE "January,1,February,2,March,3,April,4,May,5,June,6,July,7,August,8,September,9,October,10,November,11,December,12")
         bScand = DYNAMIC-FUNCTION("Scandinavian")
         .

  cFrequencyType:DELETE("1").
  cFrequencyType:ADD-LAST(IF bScand THEN "Daglig" ELSE "Daily","d").
  cFrequencyType:ADD-LAST(IF bScand THEN "Ukentlig" ELSE "Weekly","w").
  cFrequencyType:ADD-LAST(IF bScand THEN "Månedlig" ELSE "Monthly","m").
  cFrequencyType:ADD-LAST(IF bScand THEN "Årlig" ELSE "Yearly","y").

  IF DYNAMIC-FUNCTION("getUserLevel") = "super" THEN DO:
    cFrequencyType:ADD-LAST(IF bScand THEN "Pr minutt" ELSE "Per Minute","min").
    cFrequencyType:ADD-LAST(IF bScand THEN "Pr time" ELSE "Per Hour","h").      
  END.

 


  hQuery = DYNAMIC-FUNCTION("NewQuery"
  ,100
  ,""
  ,"JBoxJobSchedule"
  ,"WHERE false"
  ,"").

  hFieldMap = DYNAMIC-FUNCTION("NewFieldMap"
  ,hQuery
  ,FRAME {&FRAME-NAME}:HANDLE
  ,"cFrequencyType,dEnd,dStart,iFrequency,iJBoxJobQueueId",""
  ,"",""
  ,"").

  DYNAMIC-FUNCTION("CreateObjectLink",hQuery,hFieldMap).

  IF iiJobDefinitionId NE 0 THEN DO:
    DYNAMIC-FUNCTION("setAttribute",hFieldMap,"bufferExtraFields","iJBoxJobDefinitionId").
    DYNAMIC-FUNCTION("setAttribute",hFieldMap,"bufferExtraValues",STRING(iiJobDefinitionId)).
    iJBoxJobDefinitionId:SCREEN-VALUE = STRING(iiJobDefinitionId).
  END.
  IF iiJobQueueId NE 0 THEN DO:
    DYNAMIC-FUNCTION("setAttribute",hFieldMap,"bufferExtraFields","iJBoxJobQueueId").
    DYNAMIC-FUNCTION("setAttribute",hFieldMap,"bufferExtraValues",STRING(iiJobQueueId)).
    iJBoxJobQueueId:SCREEN-VALUE = STRING(iiJobQueueId).
  END.

  DYNAMIC-FUNCTION("InitTranslation",FRAME {&FRAME-NAME}:HANDLE).

  IF icMode NE "new" THEN DO:
    DYNAMIC-FUNCTION("setAttribute",hQuery,"baseQuery",
                     "WHERE iJBoxJobScheduleId = " + STRING(iiJobScheduleId)).
    RUN InvokeMethod(hQuery,"OpenQuery").
    FRAME {&FRAME-NAME}:TITLE = IF bScand THEN "Endre regelmessig oppgave" ELSE "Edit scheduled task".
  END.
  ELSE DO:
    DYNAMIC-FUNCTION("setObjectState",hFieldMap,icMode).
    cFrequencyType:SCREEN-VALUE = "d".
    APPLY "value-changed" TO cFrequencyType.
    cFrequency:SCREEN-VALUE     = "d".
    FRAME {&FRAME-NAME}:TITLE = IF bScand THEN "Ny regelmessig oppgave" ELSE "New scheduled task".
  END.

  LocalTranslation().

  APPLY "entry" TO iJBoxJobQueueId.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValueChangedField Dialog-Frame 
PROCEDURE ValueChangedField :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icFieldName AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  CASE icFieldName:
    WHEN "cFrequencyType" THEN setFrequencyType(cFrequencyType:SCREEN-VALUE).
/*     WHEN "cFrequency"     THEN setFrequency(). */
  END CASE.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LocalTranslation Dialog-Frame 
FUNCTION LocalTranslation RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:

END.
  
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setFrequency Dialog-Frame 
FUNCTION setFrequency RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  ASSIGN tbMonday:HIDDEN    = YES
         tbTuesday:HIDDEN   = YES
         tbWednesday:HIDDEN = YES
         tbSaturday:HIDDEN  = YES
         tbSunday:HIDDEN    = YES 
         tbThursday:HIDDEN  = YES 
         tbFriday:HIDDEN    = YES
         .

END.
 
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setFrequencyType Dialog-Frame 
FUNCTION setFrequencyType RETURNS LOGICAL
  ( INPUT icValue AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR ix     AS INT  NO-UNDO.
DEF VAR cValue AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:

  IF cFrequency:NUM-BUTTONS > 0 THEN
    DO ix = NUM-ENTRIES(cFrequency:RADIO-BUTTONS) - 1 TO 1 BY -2:
      cFrequency:DELETE(ENTRY(ix,cFrequency:RADIO-BUTTONS)).
    END.

  ASSIGN tbMonday:HIDDEN    = YES
         tbTuesday:HIDDEN   = YES
         tbWednesday:HIDDEN = YES
         tbSaturday:HIDDEN  = YES
         tbSunday:HIDDEN    = YES 
         tbThursday:HIDDEN  = YES 
         tbFriday:HIDDEN    = YES
         .

  CASE icValue:
    WHEN "d" THEN DO:
      cFrequency:HEIGHT-CHARS = 2.
      cFrequency:ADD-LAST(IF bScand THEN "Hver" ELSE "Every","d").
      cFrequency:ADD-LAST(IF bScand THEN "Hver ukedag" ELSE "Every weekday","w").
    END.
    WHEN "w" THEN DO:
      cFrequency:HEIGHT-CHARS = 1.
      cFrequency:ADD-LAST(IF bScand THEN "Hver" ELSE "Every","w").
      cFrequency:SCREEN-VALUE = "w".
    END.
  END CASE.

END.
  
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

