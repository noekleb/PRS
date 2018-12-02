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
  DEF VAR icJBoxBroadCastMessageId AS CHAR NO-UNDO INIT "2".
&ELSE
  DEF INPUT PARAM icJBoxBroadCastMessageId AS CHAR NO-UNDO.
&ENDIF
/* Local Variable Definitions ---                                       */

DEF VAR bOK                     AS LOG   NO-UNDO.
DEF VAR ix                      AS INT   NO-UNDO.
DEF VAR cBroadCastURL           AS CHAR  NO-UNDO.
DEF VAR bBlobDoc                AS LOG   NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS cBroadCastMessage Btn_OK bDoNotShowAgain 
&Scoped-Define DISPLAYED-OBJECTS cBroadCastMessage bDoNotShowAgain 

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
DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_ReadMore AUTO-GO 
     LABEL "Les mer >>" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE cBroadCastMessage AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 81 BY 13.57 NO-UNDO.

DEFINE VARIABLE bDoNotShowAgain AS LOGICAL INITIAL no 
     LABEL "Ikke vis meldingen flere ganger" 
     VIEW-AS TOGGLE-BOX
     SIZE 33.6 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     cBroadCastMessage AT ROW 1.48 COL 2.4 NO-LABEL
     Btn_ReadMore AT ROW 15.29 COL 53
     Btn_OK AT ROW 15.29 COL 68.6
     bDoNotShowAgain AT ROW 15.43 COL 3.4
     SPACE(47.59) SKIP(0.42)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE ""
         DEFAULT-BUTTON Btn_OK.


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
   NOT-VISIBLE FRAME-NAME                                               */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE.

/* SETTINGS FOR BUTTON Btn_ReadMore IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       Btn_ReadMore:HIDDEN IN FRAME Dialog-Frame           = TRUE.

ASSIGN 
       cBroadCastMessage:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

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
ON WINDOW-CLOSE OF FRAME Dialog-Frame
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
  RUN SaveResponse.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_ReadMore
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_ReadMore Dialog-Frame
ON CHOOSE OF Btn_ReadMore IN FRAME Dialog-Frame /* Les mer >> */
DO:
  IF bBlobDoc THEN
    DYNAMIC-FUNCTION("ViewDocs","JBoxBroadCastMessage",icJBoxBroadCastMessageId,TRUE,"").
  ELSE
    DYNAMIC-FUNCTION("setWebDoc","open",cBroadCastURL).
  RUN SaveResponse.
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
  DISPLAY cBroadCastMessage bDoNotShowAgain 
      WITH FRAME Dialog-Frame.
  ENABLE cBroadCastMessage Btn_OK bDoNotShowAgain 
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
DEF VAR cMsgValues AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  ASSIGN FRAME {&FRAME-NAME}:X = SESSION:WIDTH-PIXELS / 2 - FRAME {&FRAME-NAME}:WIDTH-PIXELS  
         FRAME {&FRAME-NAME}:Y = SESSION:HEIGHT-PIXELS / 2 - FRAME {&FRAME-NAME}:HEIGHT-PIXELS 
         .

  cMsgValues = DYNAMIC-FUNCTION("getFieldList",
                               "JBoxBroadCastMessage"
                               + ";cBroadCastMessage"
                               + ";cBroadCastURL"
                             + ",JBoxDocRel;iJBoxDocumentId"
                               ,"WHERE iJBoxBroadCastMessageId = " + icJBoxBroadCastMessageId
                             + ",FIRST JBoxDocRel NO-LOCK WHERE cContext = 'JBoxBroadCastMessage' AND JBoxDocRel.cEntityId = '" + icJBoxBroadCastMessageId + "' OUTER-JOIN").

  DO ix = 1 TO NUM-ENTRIES(cMsgValues,"|"):
    CASE ix:
      WHEN 1 THEN cBroadCastMessage:SCREEN-VALUE = ENTRY(ix,cMsgValues,"|").
      WHEN 2 THEN cBroadCastURL = ENTRY(ix,cMsgValues,"|").
      WHEN 3 THEN bBlobDoc = YES.
    END CASE.
  END.

  IF bBlobDoc OR cBroadCastURL NE "" THEN
    ASSIGN Btn_ReadMore:SENSITIVE = YES
           Btn_ReadMore:HIDDEN    = NO.

  LocalTranslation().
  DYNAMIC-FUNCTION("InitTranslation",FRAME {&FRAME-NAME}:HANDLE).
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SaveResponse Dialog-Frame 
PROCEDURE SaveResponse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  IF bDoNotShowAgain:CHECKED THEN
    DYNAMIC-FUNCTION("DoUpdate","JBoxUserBroadcast","",
                     "cJBoxUserId,iJBoxBroadCastMessageId",
                     DYNAMIC-FUNCTION("getASuserId") + "|" + icJBoxBroadCastMessageId,
                     "bDoNotShowAgain",
                     "TRUE",
                     TRUE).
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
    ASSIGN Btn_ReadMore:LABEL    = "Read more >>" 
           bDoNotShowAgain:LABEL = "Don't show this message again"
           .
  
  RETURN TRUE.  
END.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

