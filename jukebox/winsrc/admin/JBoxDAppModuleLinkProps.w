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
  DEF VAR icMode         AS CHAR NO-UNDO.
  DEF VAR iiProgramId    AS INT  NO-UNDO.
  DEF VAR iiModuleItemId AS INT  NO-UNDO INIT 1.
  DEF VAR iiModuleId     AS INT  NO-UNDO.
  DEF VAR obOk           AS LOG  NO-UNDO.
&ELSE
  DEF INPUT  PARAM icMode         AS CHAR NO-UNDO.
  DEF INPUT  PARAM iiModuleItemId AS INT  NO-UNDO.
  DEF INPUT  PARAM iiProgramId    AS INT  NO-UNDO.
  DEF INPUT  PARAM iiModuleId     AS INT  NO-UNDO.
  DEF OUTPUT PARAM obOk           AS LOG  NO-UNDO.
&ENDIF

/* Local Variable Definitions ---                                       */

DEF VAR hFieldMap   AS HANDLE NO-UNDO.
DEF VAR hQuery      AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS iJBoxAppProgramId iJBoxAppModuleId ~
iJBoxCompanyId cConfigParam1 cConfigParam2 cConfigParam3 cConfigParam4 ~
cConfigParam5 Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS iJBoxAppProgramId iJBoxAppModuleId ~
iJBoxCompanyId cConfigParam1 cConfigParam2 cConfigParam3 cConfigParam4 ~
cConfigParam5 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
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

DEFINE VARIABLE iJBoxAppModuleId AS CHARACTER FORMAT "X(256)" INITIAL "0" 
     LABEL "Module" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     DROP-DOWN-LIST
     SIZE 45 BY 1.

DEFINE VARIABLE iJBoxAppProgramId AS CHARACTER FORMAT "X(256)" INITIAL "0" 
     LABEL "Program" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 45 BY 1.

DEFINE VARIABLE iJBoxCompanyId AS CHARACTER FORMAT "X(256)" INITIAL "0" 
     LABEL "Company" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 45 BY 1.

DEFINE VARIABLE cConfigParam1 AS CHARACTER FORMAT "x(80)" 
     LABEL "Config param 1" 
     VIEW-AS FILL-IN 
     SIZE 82 BY 1.

DEFINE VARIABLE cConfigParam2 AS CHARACTER FORMAT "x(80)" 
     LABEL "Config param 2" 
     VIEW-AS FILL-IN 
     SIZE 82 BY 1.

DEFINE VARIABLE cConfigParam3 AS CHARACTER FORMAT "x(80)" 
     LABEL "Config param 3" 
     VIEW-AS FILL-IN 
     SIZE 82 BY 1.

DEFINE VARIABLE cConfigParam4 AS CHARACTER FORMAT "x(80)" 
     LABEL "Config param 4" 
     VIEW-AS FILL-IN 
     SIZE 82 BY 1.

DEFINE VARIABLE cConfigParam5 AS CHARACTER FORMAT "x(80)" 
     LABEL "Config Param 5" 
     VIEW-AS FILL-IN 
     SIZE 82 BY 1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     iJBoxAppProgramId AT ROW 1.95 COL 17 COLON-ALIGNED
     iJBoxAppModuleId AT ROW 2.95 COL 17 COLON-ALIGNED
     iJBoxCompanyId AT ROW 3.95 COL 17 COLON-ALIGNED
     cConfigParam1 AT ROW 4.95 COL 17 COLON-ALIGNED
     cConfigParam2 AT ROW 5.95 COL 17 COLON-ALIGNED
     cConfigParam3 AT ROW 6.95 COL 17 COLON-ALIGNED
     cConfigParam4 AT ROW 7.95 COL 17 COLON-ALIGNED
     cConfigParam5 AT ROW 8.95 COL 17 COLON-ALIGNED
     Btn_OK AT ROW 10.19 COL 69.6
     Btn_Cancel AT ROW 10.19 COL 85.6
     SPACE(1.99) SKIP(0.28)
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
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

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
  DISPLAY iJBoxAppProgramId iJBoxAppModuleId iJBoxCompanyId cConfigParam1 
          cConfigParam2 cConfigParam3 cConfigParam4 cConfigParam5 
      WITH FRAME Dialog-Frame.
  ENABLE iJBoxAppProgramId iJBoxAppModuleId iJBoxCompanyId cConfigParam1 
         cConfigParam2 cConfigParam3 cConfigParam4 cConfigParam5 Btn_OK 
         Btn_Cancel 
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
DO WITH FRAME {&FRAME-NAME}:
  ASSIGN iJBoxAppProgramId:DELIMITER = "|"
         iJBoxAppProgramId:LIST-ITEM-PAIRS = DYNAMIC-FUNCTION("getFieldList",
                                                             "JBoxAppProgram;cFileName;iJBoxAppProgramId",
                                                             "WHERE true")
         iJBoxAppModuleId:DELIMITER = "|"
         iJBoxAppModuleId:LIST-ITEM-PAIRS = DYNAMIC-FUNCTION("getFieldList",
                                                             "JBoxAppModule;cModuleName;iJBoxAppModuleId",
                                                             "WHERE true")
         iJBoxCompanyId:DELIMITER = "|"
         iJBoxCompanyId:LIST-ITEM-PAIRS = DYNAMIC-FUNCTION("getFieldList",
                                                             "JBoxCompany;cCompanyName;iJBoxCompanyId",
                                                              "WHERE true")
         .

  hQuery = DYNAMIC-FUNCTION("NewQuery"
      ,100
      ,""
      ,"JBoxAppModuleItem"
      ,"WHERE false"
      ,"").

  hFieldMap = DYNAMIC-FUNCTION("NewFieldMap"
      ,hQuery
      ,FRAME {&FRAME-NAME}:HANDLE
      ,TRIM((IF iiProgramId = 0 THEN "iJBoxAppProgramId"
            ELSE IF iiModuleId = 0 THEN "iJBoxAppModuleId"
            ELSE "")
         +  ",iJBoxCompanyId,cConfigParam1,cConfigParam2,cConfigParam3,cConfigParam4,cConfigParam5"
            ,","),""
      ,(IF iiProgramId NE 0 THEN "iJBoxAppProgramId" 
        ELSE IF iiModuleId NE 0 THEN "iJBoxAppModuleId"
        ELSE ""),""
      ,"").

  DYNAMIC-FUNCTION("CreateObjectLink",hQuery,hFieldMap).

  IF iiProgramId NE 0 THEN DO:
    DYNAMIC-FUNCTION("setAttribute",hFieldMap,"bufferExtraFields","iJBoxAppProgramId").
    DYNAMIC-FUNCTION("setAttribute",hFieldMap,"bufferExtraValues",STRING(iiProgramId)).
    iJBoxAppProgramId:SCREEN-VALUE = STRING(iiProgramId).
  END.
  ELSE IF iiModuleId NE 0 THEN DO:
    DYNAMIC-FUNCTION("setAttribute",hFieldMap,"bufferExtraFields","iJBoxAppModuleId").
    DYNAMIC-FUNCTION("setAttribute",hFieldMap,"bufferExtraValues",STRING(iiModuleId)).
    iJBoxAppModuleId:SCREEN-VALUE = STRING(iiModuleId).
  END.

  DYNAMIC-FUNCTION("InitTranslation",FRAME {&FRAME-NAME}:HANDLE).

  IF icMode NE "new" THEN DO:
    DYNAMIC-FUNCTION("setAttribute",hQuery,"baseQuery",
                     "WHERE iJBoxAppModuleItemId = " + STRING(iiModuleItemId)).
    RUN InvokeMethod(hQuery,"OpenQuery").
  END.
  ELSE DYNAMIC-FUNCTION("setObjectState",hFieldMap,icMode).

  APPLY "entry" TO iJBoxAppModuleId.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

