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
DEF INPUT  PARAM iiEventId       AS INT   NO-UNDO.
DEF INPUT  PARAM icFrameTitle    AS CHAR  NO-UNDO.
DEF INPUT  PARAM icNewStatus     AS CHAR  NO-UNDO.
DEF INPUT  PARAM icDefaultText   AS CHAR  NO-UNDO.
DEF OUTPUT PARAM obOk            AS LOG   NO-UNDO.

/* Local Variable Definitions ---                                       */

DEF VAR hQuery    AS HANDLE NO-UNDO.
DEF VAR hFieldMap AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS cEventText cResponseText dResponseDate ~
fiTime cEventStatus Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS cEventText cResponseText dResponseDate ~
fiTime cEventStatus 

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

DEFINE VARIABLE cEventStatus AS CHARACTER FORMAT "X(256)":U 
     LABEL "Status" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 26.8 BY 1 NO-UNDO.

DEFINE VARIABLE cEventText AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 81 BY 5.19 NO-UNDO.

DEFINE VARIABLE cResponseText AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 81 BY 5.57 NO-UNDO.

DEFINE VARIABLE dResponseDate AS DATE FORMAT "99/99/9999":U 
     LABEL "Dato" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiTime AS CHARACTER FORMAT "xx:xx":U INITIAL "0" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     cEventText AT ROW 1.29 COL 2.6 NO-LABEL
     cResponseText AT ROW 6.71 COL 2.6 NO-LABEL
     dResponseDate AT ROW 12.67 COL 6 COLON-ALIGNED
     fiTime AT ROW 12.67 COL 21.2 COLON-ALIGNED NO-LABEL
     cEventStatus AT ROW 12.67 COL 55.2 COLON-ALIGNED
     Btn_OK AT ROW 13.91 COL 53.2
     Btn_Cancel AT ROW 13.91 COL 69.2
     SPACE(0.79) SKIP(0.23)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Registrer oppfølging"
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
       cResponseText:RETURN-INSERTED IN FRAME Dialog-Frame  = TRUE.

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
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Registrer oppfølging */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"bufferextravalues",
                   DYNAMIC-FUNCTION("getConvToIntTime",fiTime:SCREEN-VALUE)
                 + (IF cEventStatus:HIDDEN THEN "|" + icNewStatus ELSE "")).

  RUN InvokeMethod(hFieldMap,"SaveRecord").

  IF DYNAMIC-FUNCTION("getEventProcReturnValue",hFieldMap,"") NE "" THEN RETURN NO-APPLY.
  
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

/*   IF CAN-DO(SOURCE-PROCEDURE:INTERNAL-ENTRIES,"setSimpleSelectAttributes") THEN  */
/*     RUN setSimpleSelectAttributes IN SOURCE-PROCEDURE(selSelection:HANDLE).      */
  
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
  DISPLAY cEventText cResponseText dResponseDate fiTime cEventStatus 
      WITH FRAME Dialog-Frame.
  ENABLE cEventText cResponseText dResponseDate fiTime cEventStatus Btn_OK 
         Btn_Cancel 
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
DEF VAR cDocFields AS CHAR NO-UNDO.
DO WITH FRAME {&FRAME-NAME}:
  

  ASSIGN dResponseDate:SCREEN-VALUE = STRING(TODAY)
         fiTime:SCREEN-VALUE        = STRING(TIME,"HH:MM")
         .
  IF icNewStatus NE "" THEN
    cEventStatus:HIDDEN = YES.
  ELSE ASSIGN cEventStatus:DELIMITER = "|"
              cEventStatus:LIST-ITEM-PAIRS = DYNAMIC-FUNCTION("getFieldList",
                                                              "JBoxEventStatus;cEventStatus;cEventStatusText",
                                                              "WHERE bUseInFollowUp").

  hQuery = DYNAMIC-FUNCTION("NewQuery"
          ,1
          ,""
          ,"JBoxEventLog"
           + ";cEventText"
           + ";dResponseDate"
           + ";+fiTime|CHARACTER|x(5)|jb_hhmm(iResponseTime)"
           + ";cEventStatus"
           + ";cResponseText"
          ,"WHERE iJBoxEventLogId = " + STRING(iiEventId)
          ,"").

  hFieldMap = DYNAMIC-FUNCTION("NewFieldMap"
          ,hQuery
          ,FRAME {&FRAME-NAME}:HANDLE
          ,"dResponseDate,fiTime,cResponseText" + (IF NOT cEventStatus:HIDDEN THEN ",cEventStatus" ELSE ""),""
          ,"cEventText",""
          ,"").
  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"bufferextrafields","iResponseTime"
                   + (IF cEventStatus:HIDDEN THEN ",cEventStatus" ELSE "")).

  DYNAMIC-FUNCTION("CreateObjectLink",hFieldMap,hQuery).

  RUN InvokeMethod(hQuery,"OpenQuery").

  ASSIGN dResponseDate:SCREEN-VALUE = STRING(TODAY)
         fiTime:SCREEN-VALUE = STRING(TIME,"HH:MM").
  IF cResponseText:SCREEN-VALUE = "" THEN
    cResponseText:SCREEN-VALUE = icDefaultText.

  LocalTranslation().

  IF icFrameTitle NE "" THEN
    FRAME {&FRAME-NAME}:TITLE = icFrameTitle.

  DYNAMIC-FUNCTION("initTranslation",FRAME {&FRAME-NAME}:HANDLE).

  APPLY "any-printable" TO cResponseText.
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
           FRAME {&FRAME-NAME}:TITLE = "Enter event follow-up"
           dResponseDate:LABEL       = "Date"          
           .
  
  RETURN TRUE.  
END.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

