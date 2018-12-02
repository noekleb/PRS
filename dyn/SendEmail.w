&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description:        Container for a JukeBox window program

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author:            brynjar@chemistry.no

  Created:           18.oct.2006

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
&IF DEFINED(UIB_is_Running) NE 0 &THEN
  DEF VAR icEpostAdr AS CHAR NO-UNDO INIT "9092 3875".
  DEF VAR icSubject AS CHAR NO-UNDO INIT "Emne".
  DEF VAR icMessage AS CHAR NO-UNDO INIT "Ny test fra Skotex".
  DEF VAR obOk      AS LOG  NO-UNDO.
  DEF VAR ocMessage AS CHAR NO-UNDO.
&ELSE
  DEF INPUT  PARAM icEpostAdr AS CHAR NO-UNDO.
  DEF INPUT  PARAM icSubject AS CHAR NO-UNDO.
  DEF INPUT  PARAM icMessage AS CHAR NO-UNDO.
  DEF OUTPUT PARAM obOk      AS LOG  NO-UNDO.
  DEF OUTPUT PARAM ocMessage AS CHAR NO-UNDO.
&ENDIF

/* Local Variable Definitions ---                                       */

DEF VAR bOk         AS LOG    NO-UNDO.
DEF VAR ix          AS INT    NO-UNDO.
DEF VAR hBrowse     AS HANDLE NO-UNDO.
DEF VAR hQuery      AS HANDLE NO-UNDO.
DEF VAR hToolbar    AS HANDLE NO-UNDO.
DEF VAR hFieldMap   AS HANDLE NO-UNDO.

DEF VAR cMailServer      AS CHAR NO-UNDO.
DEF VAR cAuthorize       AS CHAR NO-UNDO.
DEF VAR cAuthType        AS CHAR NO-UNDO.   
DEF VAR cMailUser        AS CHAR NO-UNDO.
DEF VAR cMailPwd         AS CHAR NO-UNDO.
DEF VAR cMailProgram     AS CHAR NO-UNDO.
DEF VAR cSmsDomain       AS CHAR NO-UNDO.
DEF VAR cSMSReplyTo      AS CHAR NO-UNDO.
DEF VAR cSMSProvider     AS CHAR NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fiTil fiKopi fiSvarTil fiEmne edBody ~
tbVisLogg btnContinue btnSend btnCancel 
&Scoped-Define DISPLAYED-OBJECTS fiTil fiKopi fiSvarTil fiEmne edBody ~
tbVisLogg 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCancel 
     LABEL "&Avbryt" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnContinue AUTO-GO 
     LABEL "Fortsett til epost program" 
     SIZE 35.6 BY 1.14.

DEFINE BUTTON btnSend AUTO-GO 
     LABEL "Send" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE edBody AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 64.6 BY 5.95 NO-UNDO.

DEFINE VARIABLE fiEmne AS CHARACTER FORMAT "X(256)":U 
     LABEL "Emne" 
     VIEW-AS FILL-IN 
     SIZE 64.6 BY 1 NO-UNDO.

DEFINE VARIABLE fiKopi AS CHARACTER FORMAT "X(256)":U 
     LABEL "Kopi" 
     VIEW-AS FILL-IN 
     SIZE 64.6 BY 1 NO-UNDO.

DEFINE VARIABLE fiSvarTil AS CHARACTER FORMAT "X(256)":U 
     LABEL "Svar til" 
     VIEW-AS FILL-IN 
     SIZE 64.6 BY 1 NO-UNDO.

DEFINE VARIABLE fiTil AS CHARACTER FORMAT "X(256)":U 
     LABEL "Til" 
     VIEW-AS FILL-IN 
     SIZE 64.6 BY 1 NO-UNDO.

DEFINE VARIABLE tbVisLogg AS LOGICAL INITIAL no 
     LABEL "Vis loggfil" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     fiTil AT ROW 1.48 COL 9.4 COLON-ALIGNED
     fiKopi AT ROW 2.52 COL 9.4 COLON-ALIGNED
     fiSvarTil AT ROW 3.57 COL 9.4 COLON-ALIGNED
     fiEmne AT ROW 5.05 COL 9.4 COLON-ALIGNED
     edBody AT ROW 6.24 COL 11.4 NO-LABEL
     tbVisLogg AT ROW 12.48 COL 47.6
     btnContinue AT ROW 13.38 COL 11
     btnSend AT ROW 13.38 COL 47
     btnCancel AT ROW 13.38 COL 62
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 77.4 BY 13.62
         DEFAULT-BUTTON btnSend CANCEL-BUTTON btnCancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Send epost"
         HEIGHT             = 13.62
         WIDTH              = 77.6
         MAX-HEIGHT         = 23.52
         MAX-WIDTH          = 114.2
         VIRTUAL-HEIGHT     = 23.52
         VIRTUAL-WIDTH      = 114.2
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win 
/* ************************* Included-Libraries *********************** */

{incl/devmode.i}
{incl/custdevmode.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 13.62
       FRAME DEFAULT-FRAME:WIDTH            = 77.4.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Send epost */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Send epost */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancel C-Win
ON CHOOSE OF btnCancel IN FRAME DEFAULT-FRAME /* Avbryt */
DO:
  APPLY "close" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnContinue
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnContinue C-Win
ON CHOOSE OF btnContinue IN FRAME DEFAULT-FRAME /* Fortsett til epost program */
DO:
  DEF VAR cURL AS CHAR NO-UNDO INIT "mailto:".
  DEF VAR cTo  AS CHAR NO-UNDO INIT "&".

  ASSIGN fiTil:SCREEN-VALUE = REPLACE(fiTil:SCREEN-VALUE,";",",")
         fiKopi:SCREEN-VALUE = REPLACE(fiKopi:SCREEN-VALUE,";",",").
  DO ix = 1 TO NUM-ENTRIES(fiTil:SCREEN-VALUE):
    cTo = cTo + (IF cTo NE "" THEN "&" ELSE "") + "To=" + ENTRY(ix,fiTil:SCREEN-VALUE).
  END.
  DO ix = 1 TO NUM-ENTRIES(fiKopi:SCREEN-VALUE):
    cTo = cTo + (IF cTo NE "" THEN "&" ELSE "") + "Cc=" + ENTRY(ix,fiKopi:SCREEN-VALUE).
  END.

  cURL = cURL
       + "?subject=" + REPLACE(fiEmne:SCREEN-VALUE," ","%20")  
       + "&body=" + REPLACE(REPLACE(edBody:SCREEN-VALUE," ","%20"),CHR(10),"%0A")
       + (IF fiSvarTil:SCREEN-VALUE NE ? AND fiSvarTil:SCREEN-VALUE NE "" THEN "&reply-to=" + fiSvarTil:SCREEN-VALUE ELSE "")
       + cTo
       . 
  DYNAMIC-FUNCTION("setWebDoc","",cURL).

  ASSIGN obOk      = YES
         ocMessage = "Epost " + STRING(TODAY) + ", " + STRING(TIME,"HH:MM") + " " + fiTil:SCREEN-VALUE. 
  APPLY "close" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSend
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSend C-Win
ON CHOOSE OF btnSend IN FRAME DEFAULT-FRAME /* Send */
DO:
  RUN SendEmail.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
  IF NOT DYNAMIC-FUNCTION("DoCleanUpObjects",THIS-PROCEDURE:CURRENT-WINDOW) THEN RETURN NO-APPLY.
  DYNAMIC-FUNCTION("SetCleanUpResize",THIS-PROCEDURE:CURRENT-WINDOW).
  PUBLISH "InvalidateHandle" (THIS-PROCEDURE).
  RUN disable_UI.
END.

{incl/wintrigg.i}

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.

  RUN InitWindow.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  DISPLAY fiTil fiKopi fiSvarTil fiEmne edBody tbVisLogg 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE fiTil fiKopi fiSvarTil fiEmne edBody tbVisLogg btnContinue btnSend 
         btnCancel 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitWindow C-Win 
PROCEDURE InitWindow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       Refer to the <jukebox>\winsrc\samples for working examples for Sports2000
------------------------------------------------------------------------------*/
DEF VAR cReturn          AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  cReturn = DYNAMIC-FUNCTION("getFieldList","SysPara;Beskrivelse;Parameter1",
                             "WHERE SysHId = 50 AND SysGr = 50 AND ParaNr < 7 BY ParaNr").
  IF cReturn = "" OR NUM-ENTRIES(cReturn,"|") < 5 THEN DO:
    DYNAMIC-FUNCTION("DoMessage",0,0,"Parameteroppsett mangler for å sende epost","","").
    APPLY "close" TO THIS-PROCEDURE.
    RETURN.
  END.

  DO ix = 1 TO NUM-ENTRIES(cReturn,"|") BY 2:
    CASE ENTRY(ix,cReturn,"|"):
      WHEN "Mailhub"     THEN cMailServer  = ENTRY(ix + 1,cReturn,"|").
      WHEN "DoAuth"      THEN cAuthorize   = ENTRY(ix + 1,cReturn,"|").
      WHEN "AuthType"    THEN cAuthType    = ENTRY(ix + 1,cReturn,"|").
      WHEN "User"        THEN cMailUser    = ENTRY(ix + 1,cReturn,"|").
      WHEN "Password"    THEN cMailPwd     = ENTRY(ix + 1,cReturn,"|").
      WHEN "Mailprogram" THEN cMailProgram = ENTRY(ix + 1,cReturn,"|").
    END CASE.
  END.
         .
  IF cMailProgram = "" THEN 
    cMailProgram = "prssmtpmailv5_7a.p".

  ASSIGN fiEmne:SCREEN-VALUE     = icSubject
         fiTil:SCREEN-VALUE      = icEpostAdr
         fiSvarTil:SCREEN-VALUE  = cMailUser
         edBody:SCREEN-VALUE     = icMessage
         .
  APPLY "entry" TO fiEmne.
END.

DYNAMIC-FUNCTION("InitTranslation",THIS-PROCEDURE:CURRENT-WINDOW).

DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,200,200,0,0).

THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = NO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MoveToTop C-Win 
PROCEDURE MoveToTop :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = NO.
THIS-PROCEDURE:CURRENT-WINDOW:WINDOW-STATE = 3.
THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendEmail C-Win 
PROCEDURE SendEmail :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cReturn   AS CHAR NO-UNDO.

DEF VAR cTo AS CHAR NO-UNDO.
DEF VAR cCc AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:

  IF cMailProgram = "blat" THEN DO:
    ASSIGN fiTil:SCREEN-VALUE = REPLACE(fiTil:SCREEN-VALUE,";",",")
           fiKopi:SCREEN-VALUE = REPLACE(fiKopi:SCREEN-VALUE,";",",").
    DO ix = 1 TO NUM-ENTRIES(fiTil:SCREEN-VALUE):
      cTo = cTo + (IF cTo NE "" THEN "," ELSE "") + "To;" + ENTRY(ix,fiTil:SCREEN-VALUE).
    END.
    DO ix = 1 TO NUM-ENTRIES(fiKopi:SCREEN-VALUE):
      cTo = cTo + (IF cTo NE "" THEN "," ELSE "") + "Cc;" + ENTRY(ix,fiKopi:SCREEN-VALUE).
    END.

    RUN JBoxSendBlatMail.p (cTo,
                            cMailUser,
                            fiSvarTil:SCREEN-VALUE,
                            IF fiEmne NE ? THEN fiEmne:SCREEN-VALUE ELSE "",
                            edBody:SCREEN-VALUE,
                            "",
                            cMailServer,
                            cMailUser,
                            cMailPwd,
                            "",
                            "",
                            "",
                            "",
                            NO,
                            STRING(tbVisLogg:CHECKED), /* Vis logg */
                            ?,
                            OUTPUT bOk,
                            OUTPUT cReturn
                            ).
    IF NOT bOk THEN
      DYNAMIC-FUNCTION("DoMessage",0,0,cReturn,"","").
  END.
  ELSE DO:

    RUN jbserv_call_smtpmail.p (
                 cMailServer
               + "|" + fiTil:SCREEN-VALUE   /* Til */
               + "|" + fiSvarTil:SCREEN-VALUE  /* Fra */
                     + "|||||" /* |CC|BCC|Attach|localfiles| */
                     + (IF fiEmne:SCREEN-VALUE NE ? THEN fiEmne:SCREEN-VALUE ELSE "")
               + "|" + edBody:SCREEN-VALUE
                     + "||||" /* |cont.type|bodytype|importance| */
                     + (IF cAuthorize = "1" THEN "YES" ELSE "NO")
               + "|" + cAuthType
               + "|" + cMailUser
               + "|" + cMailPwd
               + "|" + cMailProgram
                      ,?
                      ,""
                      ,OUTPUT cReturn
                      ,OUTPUT bOk
                      ).

    IF cReturn <> '' THEN
      DYNAMIC-FUNCTION("DoMessage",0,0,cReturn,"","").

    IF tbVisLogg:CHECKED THEN DO:
      IF SEARCH("socketemail.log") NE ? THEN DO:
        FILE-INFO:FILE-NAME = SEARCH("socketemail.log").
        OS-COMMAND NO-WAIT notepad VALUE(FILE-INFO:FULL-PATHNAME).          
      END.
      ELSE DYNAMIC-FUNCTION("DoMessage",0,0,"Finner ikke socketmail.log","","").
    END.
  END.
END.

IF bOk THEN DO:
  ASSIGN obOk      = YES
         ocMessage = "Epost " + STRING(TODAY) + ", " + STRING(TIME,"HH:MM") + " " + fiTil:SCREEN-VALUE + CHR(10)
                   + fiEmne:SCREEN-VALUE + CHR(10)
                   + edBody:SCREEN-VALUE
                   . 
  APPLY "close" TO THIS-PROCEDURE.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

