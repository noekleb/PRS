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

/* Local Variable Definitions ---                                       */

DEF VAR bOk         AS LOG    NO-UNDO.
DEF VAR ix          AS INT    NO-UNDO.
DEF VAR hBrowse     AS HANDLE NO-UNDO.
DEF VAR hToolbar    AS HANDLE NO-UNDO.

def var cPassword   as char   no-undo.
def var iCompanyId  as int    no-undo.
def var cChangeUser as char   no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rectToolbar rectBrowse 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Prototypes ********************** */



&IF DEFINED(EXCLUDE-NotifyByEmail) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD NotifyByEmail Procedure
FUNCTION NotifyByEmail RETURNS LOGICAL 
	( input icSubject as char,
	  input icBody    as char ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF






&IF DEFINED(EXCLUDE-spinPassword) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD spinPassword Procedure
FUNCTION spinPassword RETURNS CHARACTER 
	(  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE RECTANGLE rectBrowse
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 114 BY 21.67.

DEFINE RECTANGLE rectToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 19 BY .95.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     rectToolbar AT ROW 1.24 COL 1.6
     rectBrowse AT ROW 2.43 COL 1.2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 114.2 BY 23.52 WIDGET-ID 100.


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
         TITLE              = "JukeBox template container"
         HEIGHT             = 23.52
         WIDTH              = 114.2
         MAX-HEIGHT         = 23.52
         MAX-WIDTH          = 114.2
         VIRTUAL-HEIGHT     = 23.52
         VIRTUAL-WIDTH      = 114.2
         RESIZE             = yes
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
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* JukeBox template container */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* JukeBox template container */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
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

  &IF DEFINED(UIB_is_Running) NE 0 &THEN
    RUN InitializeObject.
    RUN MoveToTop.
  &ENDIF

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

 
&IF DEFINED(EXCLUDE-ChangeCompany) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ChangeCompany Include
PROCEDURE ChangeCompany:
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"prescanqueryfilter",
                   "JboxCompanyUser WHERE iJBoxCompanyId = " + STRING(DYNAMIC-FUNCTION("getCompanyId")) 
                   + ",FIRST JboxUser OF JboxCompanyUser NO-LOCK").


RUN OpenQuery.
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF



 
&IF DEFINED(EXCLUDE-ChangePwdRecord) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ChangePwdRecord Include
PROCEDURE ChangePwdRecord:
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	def var cMelding as char no-undo.
  
  assign 
    iCompanyId      = hBrowse:query:get-buffer-handle(1):buffer-field('iJboxCompanyId'):buffer-value                                                                        
    cChangeUser     = hBrowse:query:get-buffer-handle(1):buffer-field('cjboxuserid'):buffer-value
  .
  cPassword = spinPassword().
  cMelding = 'Det nye passordet er ' + chr(10) + cPassword + chr(10) + ' vennligst endre ved neste innlogging.'.
	run setNewPassword.
	NotifyByEmail('Nytt passord i DokReg',cMelding).
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF



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
  ENABLE rectToolbar rectBrowse 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeObject C-Win 
PROCEDURE InitializeObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       Refer to the <jukebox>\winsrc\samples for working examples for Sports2000
------------------------------------------------------------------------------*/
  RUN enable_UI.
  
  DYNAMIC-FUNCTION("CreateStatusBar",THIS-PROCEDURE:CURRENT-WINDOW,"",0,YES,?).
  
  DO WITH FRAME {&FRAME-NAME}:
    DYNAMIC-FUNCTION("NewObject",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE,"container").
  
    hBrowse = DYNAMIC-FUNCTION("NewBrowse",          /* Create a browse object */
                      rectBrowse:HANDLE,              /* Rectangle to define coordinates for browse */
                      100,                            /* Rows to batch */
                      "",                             /* Browse properties, ie MULTIPLE,NUM-LOCKED-COLUMNS, etc */
                      "JBoxUser"                   /* Buffers and fields: <buffer1>;<field1>;<field2>..,<buffer2>;.. No fields gives all*/
                      + ";cJBoxUserId"
                      + ";cUserName"
                      + ";dCreated"
                      + ";cCreatedBy"
                      + ";dModified"
                      + ";cModifiedBy"
                      + ",JboxCompanyUser;!iJboxCompanyId"
                      ,"WHERE false "
                      + ", first JboxCompanyUser NO-LOCK OF JboxUser"
                      ,"sort|cJBoxUserId").                            /* Misc - for something I might need in next version.. */
    hBrowse:GET-BROWSE-COLUMN(1):WIDTH-PIXELS = 60.
    
    DYNAMIC-FUNCTION("createObjectLink",hBrowse,THIS-PROCEDURE).
     
    hToolbar = DYNAMIC-FUNCTION("NewToolbar",
                                rectToolbar:HANDLE,
                                "File","ChangePwd;Send nytt passord"
                               ,"maxborder").
  
     DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hToolbar).
     
     SUBSCRIBE TO "ChangeCompany" ANYWHERE.
     RUN InvokeMethod(hBrowse,"OpenQuery").
  END.

  DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,FRAME {&FRAME-NAME}:HANDLE,"rectToolbar").
  DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,200,200,0,0).

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

 
 
&IF DEFINED(EXCLUDE-NewPassword) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setNewPassword Include
PROCEDURE setNewPassword:
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	
  def var cEncoPass as char no-undo.
  def var cSessContext as char no-undo.
  def var ix as int no-undo.
  def var cDbList as char no-undo.
  	
  cEncoPass = ENCODE(cPassword).
  cSessContext = DYNAMIC-FUNCTION("getFieldValues","JBoxLoginSession",
                                  "WHERE cSessionId = " + quoter(DYNAMIC-FUNCTION("getSessionId")),
                                  "cContext").
                                  
  DO ix = 1 TO NUM-ENTRIES(cSessContext):
    IF ENTRY(ix,cSessContext) BEGINS "dblist" THEN
      cDbList = REPLACE(ENTRY(2,ENTRY(ix,cSessContext),";"),"|",",").
  END.

  IF cDbList NE "" THEN 
  DO:
    DO ix = 1 TO NUM-ENTRIES(cDbList):
      DYNAMIC-FUNCTION("DoDelete",ENTRY(ix,cDbList) + "._User","Avail",
            "_UserId",
            cChangeUser,
            FALSE).
      DYNAMIC-FUNCTION("DoCreate",ENTRY(ix,cDbList) + "._User","",
            "_UserId,_password",
            cChangeUser + "|" + cEncoPass,
            FALSE).
    END.
  END.
  ELSE 
  DO:
    DYNAMIC-FUNCTION("DoDelete","_User","Avail",
          "_UserId",
          cChangeUser,
          FALSE).
    DYNAMIC-FUNCTION("DoCreate","_User","",
          "_UserId,_password",
          cChangeUser + "|" + cEncoPass,
          FALSE).
  END.
  IF NOT DYNAMIC-FUNCTION("DoCommit",TRUE) THEN 
  DO:
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
  END.    

  DYNAMIC-FUNCTION("DoUpdate","JBoxUser","",
        "cJBoxUserId",
        cChangeUser,
        "dLastPwdChange",
        "01/01/1900",
        FALSE).          
  IF NOT DYNAMIC-FUNCTION("DoCommit",TRUE) THEN 
  DO:
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
  END.    

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF



&IF DEFINED(EXCLUDE-OpenQuery) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenQuery Include
PROCEDURE OpenQuery:
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"prescanqueryfilter",
                   "JboxCompanyUser WHERE iJBoxCompanyId = " + STRING(DYNAMIC-FUNCTION("getCompanyId")) 
                   + ",FIRST JboxUser OF JboxCompanyUser NO-LOCK").


  
  run super.
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF




/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-NotifyByEmail) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION NotifyByEmail Include
FUNCTION NotifyByEmail RETURNS LOGICAL 
	( input icSubject as char,
	  input icBody as char ):
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/

DEF VAR cMailTo     AS CHAR NO-UNDO.
DEF VAR cMailFrom   AS CHAR NO-UNDO.
DEF VAR cReturn     AS CHAR NO-UNDO.
def var cMailServer as char no-undo.


  assign 
    cMailServer     = DYNAMIC-FUNCTION("getFieldValues","JBoxEmailAccount",
                                      "WHERE cUsage MATCHES '*Out'(codemaster)","cMailServer")
    .
    
  DYNAMIC-FUNCTION("setAttribute",SESSION,"CompanyMailServer",cMailServer).
  IF cMailServer NE ? THEN 
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN cMailTo = DYNAMIC-FUNCTION("getFieldValues","Person",
                               "WHERE iJboxCompanyId = "
                             + string(iCompanyId) 
                             + " AND cInitialer = " + quoter(cChangeUser)
                             ,"cEpost,cInitialer")
           cMailFrom = DYNAMIC-FUNCTION("getFieldValues","JBoxUser",
                               "WHERE cJBoxUserId = '" + DYNAMIC-FUNCTION("getASuserId") + "'",
                               "cEmail")
                               .
    IF cMailTo NE ? AND ENTRY(1,cMailTo,"|") NE "" /*AND ENTRY(2,cMailTo,"|") NE DYNAMIC-FUNCTION("getASuserId")*/ AND cMailFrom NE "" THEN 
    DO:
      cMailTo = ENTRY(1,cMailTo,"|").
      
      RUN JBoxSendBlatMail.p (cMailTo,   
                              cMailFrom,
                              "",
                              icSubject,
                              icBody,
                              "",
                              "",
                              "",
                              "",
                              "", /*   icEventType, */
                              "", /* icEventExtraText, */
                              "", /* icContextRowidList, */
                              "", /* icContextKeyFields, */
                              NO, /* ibAutoSend, */
                              NO, /*  bViewLogFile, */
                              ?,  /* hResBuffer, */
                              OUTPUT bOk,
                              OUTPUT cReturn).
      IF NOT bOK THEN 
      DO:
        DYNAMIC-FUNCTION("DoMessage",0,0,cReturn,"","").
        RETURN NO.
      END.
    END.
  END.
    
  RETURN YES.

END FUNCTION.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF



&IF DEFINED(EXCLUDE-spinPassword) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION spinPassword Include
FUNCTION spinPassword RETURNS CHARACTER 
	(  ):
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	DEFINE VARIABLE result AS CHARACTER NO-UNDO.
	
  result = cChangeUser + '_' + string(random(1000,9999)).    
	RETURN result.
END FUNCTION.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


