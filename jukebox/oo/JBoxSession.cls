 
 /*------------------------------------------------------------------------
    File        : JBoxSession
    Purpose     : Session information
    Syntax      : 
    Description : 
    Author(s)   : Brynjar
    Created     : Fri Feb 27 12:43:22 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

CLASS JBoxSession:
  DEF VAR cLogFile    AS CHAR NO-UNDO.
  DEF VAR hSourceProc AS HANDLE NO-UNDO.
  DEF VAR cLanguages  AS CHAR NO-UNDO.
  DEF VAR hFocusTimer AS HANDLE NO-UNDO.

  DEFINE PUBLIC STATIC PROPERTY Instance AS CLASS JBoxSession NO-UNDO
         GET():
           IF Instance = ? THEN Instance = NEW JBoxSession().
           RETURN Instance.
         END GET.
         PRIVATE SET.

  DEFINE PUBLIC PROPERTY UserId AS CHAR  
         GET(): RETURN DYNAMIC-FUNCTION("getASuserId"). END GET.
         PROTECTED SET.
          
  DEFINE PUBLIC PROPERTY UserName AS CHAR  
         GET(): RETURN DYNAMIC-FUNCTION("getASuserName"). END GET.
         PROTECTED SET. 

  DEFINE PUBLIC PROPERTY SuperUser AS LOG  
         GET(): RETURN DYNAMIC-FUNCTION("getUserLevel") = "super". END GET.
         PROTECTED SET. 

  DEFINE PUBLIC PROPERTY CompanySuperUser AS LOG  
         GET(): RETURN DYNAMIC-FUNCTION("getUserLevel") MATCHES "*super". END GET.
         PROTECTED SET. 

  DEFINE PUBLIC PROPERTY CompanyId AS INT  
         GET(): RETURN DYNAMIC-FUNCTION("getCompanyId"). END GET.
         SET(INPUT iiCompanyId AS INT):
           DYNAMIC-FUNCTION("setCompanyId",iiCompanyId).
         END SET. 

  DEFINE PUBLIC PROPERTY LanguageCode AS CHAR  
         GET(): RETURN DYNAMIC-FUNCTION("getLanguageCode"). END GET.
         SET(icLangCode AS CHAR):           
           DYNAMIC-FUNCTION("setLanguageCode",icLangCode).
           IF NOT CAN-DO(cLanguages,icLangCode) THEN DO:
             cLanguages = cLanguages + (IF cLanguages NE "" THEN "," ELSE "") + icLangCode.
             DYNAMIC-FUNCTION("setLanguages",cLanguages).
           END.  
         END SET.

  DEFINE PUBLIC PROPERTY BaseLanguageCode AS CHAR  
         GET(): RETURN DYNAMIC-FUNCTION("getBaseLanguageCode"). END GET.
         SET(icLangCode AS CHAR):
           DYNAMIC-FUNCTION("setBaseLanguageCode",icLangCode).
           IF NOT CAN-DO(cLanguages,icLangCode) THEN DO:
             cLanguages = cLanguages + (IF cLanguages NE "" THEN "," ELSE "") + icLangCode.
             DYNAMIC-FUNCTION("setLanguages",cLanguages).
           END.  
         END SET.

  DEFINE PUBLIC PROPERTY AppTitle AS CHAR  
         GET(): RETURN DYNAMIC-FUNCTION("getAppTitle"). END GET.
         SET(icAppTitle AS CHAR):
           DYNAMIC-FUNCTION("setAppTitle",icAppTitle).
         END SET.

  DEFINE PUBLIC PROPERTY UseAdvGui AS LOG  
         GET(): RETURN DYNAMIC-FUNCTION("getAttribute",SESSION,"useAdvGui") = "yes". END GET.
         SET(ibUseAdvGui AS LOG):
           DYNAMIC-FUNCTION("setAttribute",SESSION,"useAdvGui",IF ibUseAdvGui THEN "yes" ELSE "").
         END SET.

  DEFINE PUBLIC PROPERTY AppServerHandle AS HANDLE  
         GET(): RETURN DYNAMIC-FUNCTION("getAppserviceHandle"). END GET.
         SET(ihAppServer AS HANDLE):
           DYNAMIC-FUNCTION("setAppserviceHandle",ihAppServer).
           DYNAMIC-FUNCTION("setAppserviceId",ENTRY(3,ihAppServer:CLIENT-CONNECTION-ID,":")).
         END SET.
          
  DEFINE PUBLIC PROPERTY Scandinavian AS LOG  
         GET(): RETURN DYNAMIC-FUNCTION("Scandinavian"). END GET.
         PROTECTED SET. 

  DEFINE PUBLIC PROPERTY myTempDir AS CHAR  
         GET. 
         SET (icTempDir AS CHAR):
              myTempDir = RIGHT-TRIM(icTempDir,"\") + "\". END SET.
                             
  DEFINE PUBLIC PROPERTY SelectListOk AS LOG NO-UNDO
         GET. PROTECTED SET.
         
  DEFINE PUBLIC PROPERTY SelectListValue AS CHAR NO-UNDO
         GET. PROTECTED SET.

  DEFINE PUBLIC PROPERTY PromptValueOk AS LOG NO-UNDO
         GET. PROTECTED SET.
         
  DEFINE PUBLIC PROPERTY PromptValue AS CHAR NO-UNDO
         GET. PROTECTED SET.
         
  DEFINE PUBLIC PROPERTY MainForm as class JBoxMainForm
  get():
    def var oObj as class Progress.Lang.Object no-undo.
    def var oMainForm as class JBoxMainForm no-undo.
    oObj = session:first-object.
    do while valid-object(oObj):
      if type-of(oObj,Progress.Windows.Form) then
        leave.
      oObj = oObj:Next-Sibling.
    end.
    if valid-object(oObj) then
      oMainForm = CAST(oObj,JBoxMainForm).
    return oMainForm.
  end get.
  
  DEFINE PUBLIC PROPERTY WindowX AS INT
  GET():
  return (if valid-object(this-object:MainForm) then this-object:MainForm:Location:X else SESSION:X) + 500. END GET.
  private SET.
  
  DEFINE PUBLIC PROPERTY WindowY AS INT
  GET(): return (if valid-object(this-object:MainForm) then this-object:MainForm:Location:Y else SESSION:Y) + 200. END GET.
  private SET.
  
  CONSTRUCTOR PUBLIC JBoxSession ():
    SESSION:SUPPRESS-WARNINGS = YES.
    DEFAULT-WINDOW:HIDDEN =  YES.
    
    RUN JBoxLoadLib.p ("ResizeLib.p,JBoxASlib.p,JBoxUIlib.p,JBoxFUlib.p" + (IF SEARCH("controls.dll") NE ? THEN ",controls.p" ELSE "")). 
    
    DYNAMIC-FUNCTION("NewObject",SESSION,SESSION,"Session").
    DYNAMIC-FUNCTION("setEnableColor",NO) NO-ERROR.
    DYNAMIC-FUNCTION("setAttribute",SESSION,"btnPanelHeight","small").
    RUN JBoxOOTimer.p PERSIST SET hFocusTimer ("",200).
    RUN SuspendJBoxTimer IN hFocusTimer (YES).
    
  END CONSTRUCTOR.
                             
  METHOD PUBLIC HANDLE StartTabWindow (icProcName     AS CHAR
                                      ,icTabTitle     AS CHAR
                                      ,ibNew          AS LOG
                                      ,ibKeepOpen     AS LOG).
    DEF VAR ohWinHandle    AS HANDLE NO-UNDO.
  
    PUBLISH "StartTabWindow" FROM SOURCE-PROCEDURE(icProcName,icTabTitle,SOURCE-PROCEDURE,ibNew,ibKeepOpen,OUTPUT ohWinHandle).
    RETURN ohWinHandle.
    
  END METHOD.

  METHOD PUBLIC HANDLE StartChildTab (icProcName     AS CHAR
                                     ,icTabTitle     AS CHAR
                                     ,ibNew          AS LOG
                                     ,ibKeepOpen     AS LOG).
    DEF VAR ohWinHandle    AS HANDLE NO-UNDO.
  
    PUBLISH "StartChildTab" FROM SOURCE-PROCEDURE(icProcName,icTabTitle,SOURCE-PROCEDURE,ibNew,ibKeepOpen,"",OUTPUT ohWinHandle).
  
    RETURN ohWinHandle.
    
  END METHOD.

  METHOD PUBLIC HANDLE StartChildTab (icProcName     AS CHAR
                                     ,icTabTitle     AS CHAR
                                     ,ibNew          AS LOG).
    DEF VAR ohWinHandle    AS HANDLE NO-UNDO.
  
    PUBLISH "StartChildTab" FROM SOURCE-PROCEDURE(icProcName,icTabTitle,SOURCE-PROCEDURE,ibNew,NO,"",OUTPUT ohWinHandle).
  
    RETURN ohWinHandle.
    
  END METHOD.

  METHOD PUBLIC HANDLE StartChildTab (icProcName     AS CHAR
                                     ,icTabTitle     AS CHAR).
    DEF VAR ohWinHandle    AS HANDLE NO-UNDO.
  
    PUBLISH "StartChildTab" FROM SOURCE-PROCEDURE(icProcName,icTabTitle,SOURCE-PROCEDURE,NO,NO,"",OUTPUT ohWinHandle).
  
    RETURN ohWinHandle.
    
  END METHOD.

  METHOD PUBLIC HANDLE StartChildTab (icProcName     AS CHAR).
    DEF VAR ohWinHandle    AS HANDLE NO-UNDO.
  
    PUBLISH "StartChildTab" FROM SOURCE-PROCEDURE(icProcName,"",SOURCE-PROCEDURE,NO,NO,"",OUTPUT ohWinHandle).
  
    RETURN ohWinHandle.
    
  END METHOD.

  METHOD PUBLIC VOID setDelayedFocus (ihFocus  AS HANDLE).
    
    IF VALID-HANDLE(hFocusTimer) AND VALID-HANDLE(ihFocus) AND CAN-DO(hFocusTimer:INTERNAL-ENTRIES,"setFocusHandle") THEN
      RUN setFocusHandle IN hFocusTimer (ihFocus).
      
  END METHOD.
  METHOD PUBLIC VOID setDelayedFocus (ihFocus  AS HANDLE
                                     ,ihFocus2 AS HANDLE).
  
    RUN setTwoFocusHandles IN hFocusTimer (ihFocus,ihFocus2).
      
  END METHOD.
  
  METHOD PUBLIC VOID ViewMessage (icMessage     AS CHAR).
    DYNAMIC-FUNCTION("DoMessage",0,0,icMessage,"","").
  END METHOD.
  METHOD PUBLIC VOID ViewMessage (iiMsgNum      AS INT
                                 ,icMessage     AS CHAR).
    DYNAMIC-FUNCTION("DoMessage",iiMsgNum,0,icMessage,"","").
  END METHOD.
  METHOD PUBLIC VOID ViewMessage (iiMsgNum      AS INT
                                 ,icMessage     AS CHAR
                                 ,icTitle       AS CHAR).
    DYNAMIC-FUNCTION("DoMessage",iiMsgNum,0,icMessage,icTitle,"").
  END METHOD.
  
  METHOD PUBLIC LOGICAL ViewQuestionYesNo (icQuestion AS CHAR).
    RETURN DYNAMIC-FUNCTION("DoMessage",0,4,icQuestion,"","") = 6.
  END METHOD.
  METHOD PUBLIC LOGICAL ViewQuestionYesNo (iiMsgNum   AS INT
                                          ,icQuestion AS CHAR).
    RETURN DYNAMIC-FUNCTION("DoMessage",iiMsgNum,4,icQuestion,"","") = 6.
  END METHOD.
  METHOD PUBLIC LOGICAL ViewQuestionYesNo (iiMsgNum   AS INT
                                          ,icQuestion AS CHAR
                                          ,icTitle    AS CHAR).
    RETURN DYNAMIC-FUNCTION("DoMessage",iiMsgNum,4,icQuestion,icTitle,"") = 6.
  END METHOD.

  METHOD PUBLIC INTEGER ViewQuestionYesNoCancel (icQuestion AS CHAR).
    DEF VAR iReturn AS INT NO-UNDO. 
    iReturn = DYNAMIC-FUNCTION("DoMessage",0,3,icQuestion,"","").
    CASE iReturn:
      WHEN 6 THEN RETURN 1.
      WHEN 7 THEN RETURN 2.
      WHEN 2 THEN RETURN 3.
    END CASE.  
  END METHOD.
  METHOD PUBLIC INTEGER ViewQuestionYesNoCancel (iiMsgNum   AS INT
                                                ,icQuestion AS CHAR).
    DEF VAR iReturn AS INT NO-UNDO. 
    iReturn = DYNAMIC-FUNCTION("DoMessage",iiMsgNum,3,icQuestion,"","").
    CASE iReturn:
      WHEN 6 THEN RETURN 1.
      WHEN 7 THEN RETURN 2.
      WHEN 2 THEN RETURN 3.
    END CASE.  
  END METHOD.
  METHOD PUBLIC INTEGER ViewQuestionYesNoCancel (iiMsgNum   AS INT
                                                ,icQuestion AS CHAR
                                                ,icTitle    AS CHAR).
    DEF VAR iReturn AS INT NO-UNDO. 
    iReturn = DYNAMIC-FUNCTION("DoMessage",iiMsgNum,3,icQuestion,icTitle,"").
    CASE iReturn:
      WHEN 6 THEN RETURN 1.
      WHEN 7 THEN RETURN 2.
      WHEN 2 THEN RETURN 3.
    END CASE.  
  END METHOD.

  METHOD PUBLIC LOGICAL ViewQuestionOkCancel (icQuestion AS CHAR).
    RETURN DYNAMIC-FUNCTION("DoMessage",0,1,icQuestion,"","") = 1.
  END METHOD.
  METHOD PUBLIC LOGICAL ViewQuestionOkCancel (iiMsgNum   AS INT
                                          ,icQuestion AS CHAR).
    RETURN DYNAMIC-FUNCTION("DoMessage",iiMsgNum,1,icQuestion,"","") = 1.
  END METHOD.
  METHOD PUBLIC LOGICAL ViewQuestionOkCancel (iiMsgNum   AS INT
                                          ,icQuestion AS CHAR
                                          ,icTitle    AS CHAR).
    RETURN DYNAMIC-FUNCTION("DoMessage",iiMsgNum,2,icQuestion,icTitle,"") = 1.
  END METHOD.
  METHOD PUBLIC CHARACTER getFiles (INPUT icFilterList       AS CHAR
                                   ,INPUT icInitialDirectory AS CHAR
                                   ,INPUT icDialogTitle      AS CHAR):
                        
    RETURN DYNAMIC-FUNCTION("SelectFileNames",icFilterList,icInitialDirectory,icDialogTitle).
  END METHOD.
  
  METHOD PUBLIC CHARACTER getFiles ():
                        
    RETURN DYNAMIC-FUNCTION("SelectFileNames","","","").
  END METHOD.

  METHOD PUBLIC VOID SimpleSelectListPairs (icList      AS CHAR
                                           ,ihPosWidget AS HANDLE
                                           ,icDelimiter AS CHAR):
    DEF VAR cReturn AS CHAR NO-UNDO.
    
    IF icDelimiter NE "" THEN icList = REPLACE(icList,icDelimiter,"|").
        
    RUN JBoxDSimpleSelectList.w (icList,ihPosWidget,OUTPUT cReturn).
    IF cReturn NE ? THEN
      ASSIGN THIS-OBJECT:SelectListOk = YES
             THIS-OBJECT:SelectListValue = cReturn.
    ELSE THIS-OBJECT:SelectListOk = NO.             
  END.
  
  METHOD PUBLIC VOID SimpleSelectListPairs (icList      AS CHAR
                                           ,ihPosWidget AS HANDLE):
    THIS-OBJECT:SimpleSelectListPairs(icList,ihPosWidget,"").  
  END.

  METHOD PUBLIC VOID SimpleSelectListPairs (icList      AS CHAR
                                      ,icDelimiter AS CHAR):
    THIS-OBJECT:SimpleSelectListPairs(icList,?,icDelimiter).  
  END.

  METHOD PUBLIC VOID SimpleSelectListPairs (icList      AS CHAR):
    THIS-OBJECT:SimpleSelectListPairs(icList,?,"").  
  END.
  
  METHOD PUBLIC VOID SimpleSelectList (icList      AS CHAR
                                      ,ihPosWidget AS HANDLE
                                      ,icDelimiter AS CHAR):
    DEF VAR cReturn AS CHAR NO-UNDO.
    DEF VAR ix      AS INT  NO-UNDO.
    DEF VAR cList   AS CHAR NO-UNDO.
    
    IF icDelimiter NE "" THEN icList = REPLACE(icList,icDelimiter,"|").
    
    DO ix = 1 TO NUM-ENTRIES(icList,"|"):
      cList = cList + (IF cList NE "" THEN "|" ELSE "") + ENTRY(ix,icList,"|") + "|" + ENTRY(ix,icList,"|").
    END.
    
    THIS-OBJECT:SimpleSelectListPairs(cList,ihPosWidget,"").  
  END.

  METHOD PUBLIC VOID SimpleSelectList (icList      AS CHAR
                                      ,ihPosWidget AS HANDLE):
    THIS-OBJECT:SimpleSelectList(icList,ihPosWidget,"").  
  END.

  METHOD PUBLIC VOID SimpleSelectList (icList      AS CHAR
                                      ,icDelimiter AS CHAR):
    THIS-OBJECT:SimpleSelectList(icList,?,icDelimiter).  
  END.

  METHOD PUBLIC VOID SimpleSelectList (icList      AS CHAR):
    IF INDEX(icList,"|") = 0 THEN DO:
      IF INDEX(icList,";") > 0 THEN icList = REPLACE(icList,";","|").
      ELSE IF INDEX(icList,",") > 0 THEN icList = REPLACE(icList,",","|").
    END.  
    THIS-OBJECT:SimpleSelectList(icList,?,"").  
  END.
  
  METHOD PUBLIC VOID PromptForValue (icMessage   AS CHAR
                                    ,icDataType  AS CHAR
                                    ,icFormat    AS CHAR
                                    ,icInitValue AS CHAR
                                    ):
    DEF VAR iReturn AS INT NO-UNDO.
    DEF VAR cReturn AS CHAR NO-UNDO.
    
    cReturn = icInitValue.
    IF icDataType = "" THEN 
      ASSIGN icDataType = "CHARACTER"
             icFormat   = "x(256)".
      
                                   
    RUN JBoxAskForValue.w (icMessage,icDataType + (IF icFormat NE "" THEN "|" + icFormat ELSE ""),INPUT-OUTPUT cReturn,OUTPUT iReturn).
    IF iReturn = 2 THEN 
      ASSIGN THIS-OBJECT:PromptValueOk = YES
             THIS-OBJECT:PromptValue = cReturn.
    ELSE 
      THIS-OBJECT:PromptValueOk = NO.           
  END METHOD.  

  METHOD PUBLIC VOID PromptForValue (icMessage   AS CHAR
                                    ,icDataType  AS CHAR
                                    ,icFormat    AS CHAR):
    THIS-OBJECT:PromptForValue(icMessage,icDataType,icFormat,"").                                  
  END METHOD.

  METHOD PUBLIC VOID PromptForValue (icMessage   AS CHAR
                                    ,icDataType  AS CHAR):
    THIS-OBJECT:PromptForValue(icMessage,icDataType,"","").                                  
  END METHOD.

  METHOD PUBLIC VOID PromptForValue (icMessage   AS CHAR):
    THIS-OBJECT:PromptForValue(icMessage,"","","").                                  
  END METHOD.

  METHOD PUBLIC VOID PromptForValue ():
    THIS-OBJECT:PromptForValue("","","","").                                  
  END METHOD.

  METHOD PUBLIC VOID PromptForValues (icMessage   AS CHAR
                                     ,Label_pipe_Type_pipe_Format_pipe_InitVal_ValuePairListOrBuffsAndFlds_pipe_ValueOrQueryDef_shift4_Label  AS CHAR):
    DEF VAR iReturn AS INT NO-UNDO.
    DEF VAR cReturn AS CHAR NO-UNDO.
                                       
    RUN JBoxAskForValue.w (icMessage,Label_pipe_Type_pipe_Format_pipe_InitVal_ValuePairListOrBuffsAndFlds_pipe_ValueOrQueryDef_shift4_Label,INPUT-OUTPUT cReturn,OUTPUT iReturn).
    
    IF iReturn = 2 THEN 
      ASSIGN THIS-OBJECT:PromptValueOk = YES
             THIS-OBJECT:PromptValue = cReturn.
    ELSE 
      THIS-OBJECT:PromptValueOk = NO.           
  END METHOD.
  
  METHOD PUBLIC LOGICAL StartLogMgr (icLogFile AS CHAR):
    
    IF SEARCH("log-manager-report.p") = ? THEN RETURN NO.

    LOG-MANAGER:CLOSE-LOG() NO-ERROR.
    
    IF icLogFile = "source-proc" THEN
      cLogFile = SESSION:TEMP-DIR + hSourceProc:FILE-NAME + ".log".
    ELSE cLogFile = icLogFile.
    LOG-MANAGER:LOGFILE-NAME    = cLogFile. 
    LOG-MANAGER:LOG-ENTRY-TYPES = "4gltrace:4".
    LOG-MANAGER:CLEAR-LOG().
    LOG-MANAGER:LOGGING-LEVEL   = 4.
    
    RETURN YES.
    
  END METHOD.  

  METHOD PUBLIC LOGICAL StartLogMgr ():
    hSourceProc = SOURCE-PROCEDURE.
    RETURN THIS-OBJECT:StartLogMgr("source-proc").
  END.  

  METHOD PUBLIC LOGICAL ViewLogMgrResult ():
    
    IF SEARCH("log-manager-report.p") = ? THEN RETURN NO.

    LOG-MANAGER:CLOSE-LOG().

    RUN log-manager-report.p (cLogFile,cLogFile + ".txt").
    OS-COMMAND NO-WAIT VALUE("notepad " + cLogFile + ".txt").
    
    RETURN YES.
  END.

  METHOD PUBLIC LOGICAL setRowShadeColor (INPUT iRed   AS INT
                                         ,INPUT iGreen AS INT
                                         ,INPUT iBlue  AS INT):
                        
    DYNAMIC-FUNCTION("AddColor","RowShade",iRed,iGreen,iBlue) NO-ERROR. /* 240,240,240 */
    /* Other colors added this way are available as session attributes: "Color_"<your name> */
    DYNAMIC-FUNCTION("setAttribute",SESSION,"shadedRows","yes").
  END METHOD.
                               
  DESTRUCTOR PUBLIC JBoxSession ( ):
    
  END DESTRUCTOR.
END CLASS.
  