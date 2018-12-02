&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
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
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEF VAR wColorEdit AS INTE NO-UNDO.
DEF temp-table tStatus 
  FIELD wstatus  as inte format ">9-" LABEL "Status"
  FIELD wTekst   as char format "x(30)" label "Beskrivelse"
  FIELD wRGBchar as char format "x(12)" LABEL "RGB(1)"
  FIELD wRGBint  as inte format ">>>>>>>9-" LABEL "RGB(2)". /* . */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BROWSE-1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tStatus

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 wStatus wTekst wRGBchar wRGBint   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1   
&Scoped-define SELF-NAME BROWSE-1
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY {&SELF-NAME} FOR EACH tStatus.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 tStatus
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 tStatus


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-BROWSE-1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-1 BUTTON-Change FILL-IN-1 Btn_OK ~
Btn_Cancel Btn_Help 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-1 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD hex2int Dialog-Frame 
FUNCTION hex2int RETURNS INTEGER
  ( INPUT wRGB AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_Help 
     LABEL "&Help" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-Change 
     LABEL "&Ändra" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE FILL-IN-1 AS CHARACTER FORMAT "X(8)":U INITIAL "     ABC" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 6.67 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      tStatus SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 Dialog-Frame _FREEFORM
  QUERY BROWSE-1 DISPLAY
      wStatus
      wTekst
      wRGBchar
      wRGBint
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 66 BY 8.1 ROW-HEIGHT-CHARS .63.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BROWSE-1 AT ROW 1.48 COL 4
     BUTTON-Change AT ROW 1.48 COL 71
     FILL-IN-1 AT ROW 2.91 COL 69 COLON-ALIGNED NO-LABEL
     Btn_OK AT ROW 9.81 COL 4
     Btn_Cancel AT ROW 9.81 COL 20
     Btn_Help AT ROW 9.81 COL 71
     SPACE(0.99) SKIP(0.18)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Setter farge som viser bestillingstatus i bildegrid"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
                                                                        */
/* BROWSE-TAB BROWSE-1 1 Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

ASSIGN 
       FILL-IN-1:MANUAL-HIGHLIGHT IN FRAME Dialog-Frame = TRUE
       FILL-IN-1:SELECTABLE IN FRAME Dialog-Frame       = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-1
/* Query rebuild information for BROWSE BROWSE-1
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tStatus
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON GO OF FRAME Dialog-Frame /* Setter farge som viser bestillingstatus i bildegrid */
DO:
  RUN LagreTemp.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Setter farge som viser bestillingstatus i bildegrid */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-1
&Scoped-define SELF-NAME BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 Dialog-Frame
ON DEFAULT-ACTION OF BROWSE-1 IN FRAME Dialog-Frame
DO:
  APPLY "CHOOSE" TO BUTTON-Change.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 Dialog-Frame
ON VALUE-CHANGED OF BROWSE-1 IN FRAME Dialog-Frame
DO:
  COLOR-TABLE:SET-RED-VALUE(wColorEdit,INT(ENTRY(1,tStatus.wRGBchar))).
  COLOR-TABLE:SET-GREEN-VALUE(wColorEdit,INT(ENTRY(2,tStatus.wRGBchar))).
  COLOR-TABLE:SET-BLUE-VALUE(wColorEdit,INT(ENTRY(3,tStatus.wRGBchar))).

  FILL-IN-1:BGCOLOR IN FRAME {&FRAME-NAME} = wColorEdit.
  APPLY "ENTRY" TO BROWSE {&BROWSE-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help Dialog-Frame
ON CHOOSE OF Btn_Help IN FRAME Dialog-Frame /* Help */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  MESSAGE "Help for File: {&FILE-NAME}" VIEW-AS ALERT-BOX INFORMATION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Change
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Change Dialog-Frame
ON CHOOSE OF BUTTON-Change IN FRAME Dialog-Frame /* Ändra */
DO:
  DEFINE VAR wRGBchar AS CHAR NO-UNDO.
  SYSTEM-DIALOG COLOR wColorEdit.
  
  ASSIGN wRGBchar = STRING(COLOR-TABLE:GET-RED-VALUE(wColorEdit)) + "," +
                    STRING(COLOR-TABLE:GET-GREEN-VALUE(wColorEdit)) + "," +
                    STRING(COLOR-TABLE:GET-BLUE-VALUE(wColorEdit)).
  IF wRGBchar <> tStatus.wRGBchar THEN DO:
      ASSIGN tStatus.wRGBchar = wRGBchar
             tStatus.wRGBint  = hex2int(wRGBchar).
      BROWSE {&BROWSE-NAME}:REFRESH().
  END.
  APPLY "VALUE-CHANGED" TO BROWSE {&BROWSE-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-1 Dialog-Frame
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-1 IN FRAME Dialog-Frame
DO:
  APPLY "CHOOSE" TO BUTTON-Change.
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
  RUN SkapaTemp.
  {lng.i} RUN enable_UI.
  ASSIGN wColorEdit = COLOR-TABLE:NUM-ENTRIES
         COLOR-TABLE:NUM-ENTRIES = wColorEdit + 1.
  COLOR-TABLE:SET-DYNAMIC(wColorEdit, yes).
  APPLY "VALUE-CHANGED" TO BROWSE {&BROWSE-NAME}.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
  COLOR-TABLE:NUM-ENTRIES = wColorEdit - 1.
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
  DISPLAY FILL-IN-1 
      WITH FRAME Dialog-Frame.
  ENABLE BROWSE-1 BUTTON-Change FILL-IN-1 Btn_OK Btn_Cancel Btn_Help 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LagreTemp Dialog-Frame 
PROCEDURE LagreTemp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR wRGB   AS CHAR NO-UNDO.
DEF VAR wCount AS INTE NO-UNDO.

wCount = 0.
for each SysPara exclusive-lock where 
  SysPara.SysHId = 5 and
  SysPara.SysGr  = 2 and
  SysPara.ParaNr < 10:
  
  find first tStatus where
    tStatus.wStatus = SysPara.ParaNr no-error.
  if available tStatus then
    ASSIGN SysPara.Parameter2 = tStatus.wRGBchar.
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapaTemp Dialog-Frame 
PROCEDURE SkapaTemp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR wRGB AS CHAR NO-UNDO.
DEF VAR wCount AS INTE NO-UNDO.

/* Default verdier. */
ASSIGN 
  wRGB = "255,0,0;0,255,0;0,0,255".

for each SysPara exclusive-lock where 
  SysPara.SysHId = 5 and
  SysPara.SysGr  = 2 and
  SysPAra.ParaNr < 10:
  
  /* Setter en default farge hvis det ike finnes fra før. */
  if num-entries(SysPara.Parameter2) <> 3 then
    SysPara.Parameter2 = entry(1,wRGB,";").

  CREATE tStatus.
  ASSIGN tStatus.wStatus  = SysPara.ParaNr
         tStatus.wTekst   = SysPara.Beskrivelse
         tStatus.wRGBchar = SysPara.Parameter2
         tStatus.wRGBint  = hex2int(wRGBchar).
  RELEASE tStatus. 
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION hex2int Dialog-Frame 
FUNCTION hex2int RETURNS INTEGER
  ( INPUT wRGB AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN INT(ENTRY(3,wRGB)) * 65536 +
         INT(ENTRY(2,wRGB)) * 256   +
         INT(ENTRY(1,wRGB)).         
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

