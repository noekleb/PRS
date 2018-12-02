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
  DEF VAR icStartDir  AS CHAR NO-UNDO.
  DEF VAR iocFileName AS CHAR NO-UNDO.
  DEF VAR icFileType  AS CHAR NO-UNDO.
  DEF VAR iobOpen     AS LOG  NO-UNDO.
  DEF VAR obOk        AS LOG  NO-UNDO.

  icStartDir = OS-GETENV("homedrive") + OS-GETENV("homepath").
  iocFileName = "test.".
  icFileType  = "pdf".
  iobOpen = YES.
&ELSE
  DEF INPUT        PARAM icStartDir  AS CHAR NO-UNDO.
  DEF INPUT-OUTPUT PARAM iocFileName AS CHAR NO-UNDO.
  DEF INPUT        PARAM icFileType  AS CHAR NO-UNDO.
  DEF INPUT-OUTPUT PARAM iobOpen     AS LOG  NO-UNDO.
  DEF OUTPUT       PARAM obOk        AS LOG  NO-UNDO.
&ENDIF

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
&Scoped-Define ENABLED-OBJECTS fiFileName btnFileName tbOpenFile ~
tbOpenSaveDir Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS fiFileName tbOpenFile tbOpenSaveDir 

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
DEFINE BUTTON btnFileName 
     IMAGE-UP FILE "bmp/folderopen.bmp":U
     LABEL "..." 
     SIZE 4 BY 1.

DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "&Avbryt" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE fiFileName AS CHARACTER FORMAT "X(256)":U 
     LABEL "Lagre som" 
     VIEW-AS FILL-IN 
     SIZE 88 BY 1 NO-UNDO.

DEFINE VARIABLE tbOpenFile AS LOGICAL INITIAL no 
     LABEL "Vis fil" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE tbOpenSaveDir AS LOGICAL INITIAL no 
     LABEL "Vis filområde" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     fiFileName AT ROW 1.95 COL 13 COLON-ALIGNED
     btnFileName AT ROW 1.95 COL 103 WIDGET-ID 6 NO-TAB-STOP 
     tbOpenFile AT ROW 3.38 COL 15 WIDGET-ID 4
     tbOpenSaveDir AT ROW 3.38 COL 33 WIDGET-ID 8
     Btn_OK AT ROW 4.57 COL 76
     Btn_Cancel AT ROW 4.57 COL 92
     SPACE(1.39) SKIP(0.28)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Lagre fil"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel WIDGET-ID 100.


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
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Lagre fil */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnFileName
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFileName Dialog-Frame
ON CHOOSE OF btnFileName IN FRAME Dialog-Frame /* ... */
DO:
  DEF VAR cFileName AS CHAR NO-UNDO.
  DEF VAR bOk       AS LOG  NO-UNDO.

  cFileName = ENTRY(NUM-ENTRIES(fiFileName:SCREEN-VALUE,"\"),fiFileName:SCREEN-VALUE,"\").

  IF icStartDir NE "" AND icFileType NE "" THEN
    SYSTEM-DIALOG GET-FILE cFileName
                  ASK-OVERWRITE
                  INITIAL-DIR icStartDir
                  USE-FILENAME
                  FILTERS icFileType "*." + icFileType,
                          (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Alle filer" ELSE "All files") "*.*"
                  UPDATE bOk.
  ELSE IF icStartDir NE "" THEN
    SYSTEM-DIALOG GET-FILE cFileName
                  ASK-OVERWRITE
                  INITIAL-DIR icStartDir
                  USE-FILENAME
                  UPDATE bOk.
  ELSE
    SYSTEM-DIALOG GET-FILE cFileName
                  ASK-OVERWRITE
                  UPDATE bOk.

  IF bOk THEN DO:
    fiFileName:SCREEN-VALUE = cFileName.

    IF icFileType NE "" AND NOT fiFileName:SCREEN-VALUE MATCHES "*." + icFileType THEN
      fiFileName:SCREEN-VALUE = RIGHT-TRIM(fiFileName:SCREEN-VALUE,".") + "." + icFileType.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
  ASSIGN iocFileName = fiFileName:SCREEN-VALUE
         iobOpen     = NOT tbOpenFile:HIDDEN AND tbOpenFile:CHECKED
         obOk       = YES
         .
  IF tbOpenSaveDir:CHECKED THEN DO:
    DEFINE VARIABLE oServer AS COM-HANDLE NO-UNDO.

    CREATE 'Shell.Application' oServer.

    /* Invoke the Windows Explorer on the C:\WINNT folder               */

    NO-RETURN-VALUE oServer:Explore(SUBSTR(iocFileName,1,R-INDEX(iocFileName,"\"))).

    /* Release the object references                                    */

    RELEASE OBJECT oServer.
  END.
END.


/*
  DEF VAR iocFileName  AS CHAR NO-UNDO.
  DEF VAR icStartDir  AS CHAR NO-UNDO.
  DEF VAR iobOpen      AS LOG  NO-UNDO.
  DEF VAR obOk        AS LOG  NO-UNDO.

*/

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
  DISPLAY fiFileName tbOpenFile tbOpenSaveDir 
      WITH FRAME Dialog-Frame.
  ENABLE fiFileName btnFileName tbOpenFile tbOpenSaveDir Btn_OK Btn_Cancel 
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
  IF NUM-ENTRIES(iocFileName,"\") = 1 AND icStartDir = "" THEN
    icStartDir = OS-GETENV("homedrive") + OS-GETENV("homepath"). 

  fiFileName:SCREEN-VALUE = RIGHT-TRIM(icStartDir,"\") + "\" + REPLACE(iocFileName,",","").
  IF icFileType NE "" AND NOT fiFileName:SCREEN-VALUE MATCHES "*." + icFileType THEN
    fiFileName:SCREEN-VALUE = RIGHT-TRIM(fiFileName:SCREEN-VALUE,".") + "." + icFileType.
  IF iobOpen NE ? THEN
    tbOpenFile:CHECKED = iobOpen.
  ELSE 
    tbOpenFile:HIDDEN = YES.

  LocalTranslation().

  DYNAMIC-FUNCTION("InitTranslation",FRAME {&FRAME-NAME}:HANDLE).

  APPLY "entry" TO fiFileName.
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
IF NOT DYNAMIC-FUNCTION("Scandinavian") THEN DO WITH FRAME {&FRAME-NAME}:
  ASSIGN FRAME {&FRAME-NAME}:TITLE = "Save file"
         fiFileName:LABEL = "Save as"
         btn_Cancel:LABEL = "C&ancel"
         tbOpenSaveDir:LABEL = "Open folder"
         .
  IF NOT tbOpenFile:HIDDEN THEN
    tbOpenFile:LABEL = "Open file".
END.

RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

