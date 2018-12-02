 
 /*------------------------------------------------------------------------
    File        : JBoxLookupMethods
    Purpose     : Methods available from callback procedure setLookupA
    Syntax      : 
    Description : 
    Author(s)   : Brynjar
    Created     : Thy Mar 3 12:43:22 CEST 2016
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

CLASS JBoxLookupMethods:
  DEF VAR cLogFile    AS CHAR NO-UNDO.
  DEF VAR hSourceProc AS HANDLE NO-UNDO.

  DEF TEMP-TABLE ttLookup
      FIELD hParent    AS HANDLE
      FIELD hLookup    AS HANDLE
      FIELD hBrowse    AS HANDLE
      FIELD hSearch    AS HANDLE
      FIELD hToolbar   AS HANDLE
      FIELD oBrowse    AS Progress.Lang.Object
      .
  
  DEFINE PUBLIC STATIC PROPERTY Instance AS CLASS JBoxLookupMethods NO-UNDO
         GET():
           IF Instance = ? THEN Instance = NEW JBoxLookupMethods().
           RETURN Instance.
         END GET.
         PRIVATE SET.


  CONSTRUCTOR PUBLIC JBoxLookupMethods ():
  END CONSTRUCTOR.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

  METHOD PUBLIC VOID setLookup( INPUT ihParent  AS HANDLE
                               ,INPUT ihLookup  AS HANDLE
                               ,INPUT ihBrowse  AS HANDLE
                               ,INPUT ihSearch  AS HANDLE
                               ,INPUT ihToolbar AS HANDLE 
                                 ):
    
    CREATE ttLookup.
    ASSIGN ttLookup.hParent  = ihParent
           ttLookup.hLookup  = ihLookup
           ttLookup.hBrowse  = ihBrowse
           ttLookup.hSearch  = ihSearch
           ttLookup.hToolbar = ihToolbar
           ttLookup.oBrowse  = CAST(NEW JBoxBrowse(ihBrowse),"Progress.Lang.Object")
           .

  END METHOD.

/*
  METHOD PUBLIC CHARACTER FirstDayOfMonth():
    
    RETURN STRING(DYNAMIC-FUNCTION("getFirstDayOfMonth",TODAY)).

  END METHOD.

	METHOD PUBLIC CHARACTER LastDayOfMonth( INPUT iiYear AS INT
	                                       ,INPUT iiMonth AS INT ):
		
		RETURN STRING(DYNAMIC-FUNCTION("LastDayInMonth",iiYear,iiMonth)).

	END METHOD.

  METHOD PUBLIC CHARACTER LastDayOfMonth( ):
    
    RETURN THIS-OBJECT:LastDayOfMonth(YEAR(TODAY),MONTH(TODAY)).

  END METHOD.

  METHOD PUBLIC CHARACTER LastDayOfNextMonth( ):
    DEF VAR iYear  AS INT NO-UNDO.
    DEF VAR iMonth AS INT NO-UNDO.
    
    iYear = YEAR(TODAY) + IF MONTH(TODAY) = 12 THEN 1 ELSE 0.
    iMonth = IF MONTH(TODAY) = 12 THEN 1 ELSE MONTH(TODAY) + 1.
    
    RETURN THIS-OBJECT:LastDayOfMonth(iYear,iMonth).

  END METHOD.
                               
  METHOD PUBLIC CHAR getTempFile (icType AS CHAR).
  
    DEF VAR cFile AS CHAR NO-UNDO.
    
    cFile = DYNAMIC-FUNCTION("getUniqueFileName").
    
    RETURN SUBSTR(cFile,1,R-INDEX(cFile,".")) + icType.
  END METHOD.

  METHOD PUBLIC CHAR getTempFile ().
  
    RETURN DYNAMIC-FUNCTION("getUniqueFileName").
    
  END METHOD.

  METHOD PUBLIC CHAR ViewFile (icFileName AS CHAR).
    RETURN DYNAMIC-FUNCTION("setWebDoc","open",icFileName).
  END METHOD.

  METHOD PUBLIC CHARACTER TextToFile (icText AS CHAR).
    DEF VAR cFile AS CHAR NO-UNDO.
    cFile = THIS-OBJECT:getTempFile("txt").
    OUTPUT TO VALUE(cFile).
    PUT UNFORMATTED icText.
    OUTPUT CLOSE.
    RETURN cFile.
  END METHOD.
  
  METHOD PUBLIC CHARACTER TextToFile (icText AS CHAR,INPUT ibOpen AS LOG).
    RETURN DYNAMIC-FUNCTION("setWebDoc","open",THIS-OBJECT:TextToFile(icText)). 
  END.
  
  METHOD PUBLIC VOID TextToFile (icText AS CHAR,icFileName AS CHAR).
    OUTPUT TO VALUE(icFileName).
    PUT UNFORMATTED icText.
    OUTPUT CLOSE.
  END METHOD.
  
  METHOD PUBLIC CHARACTER TextToFile (icText AS CHAR,icFileName AS CHAR,INPUT ibOpen AS LOG).
    THIS-OBJECT:TextToFile(icText,icFileName).
    RETURN DYNAMIC-FUNCTION("setWebDoc","open",icFileName). 
  END.

  METHOD PUBLIC CHARACTER TextToFile (icText AS LONGCHAR).
    DEF VAR cFile AS CHAR NO-UNDO.
    cFile = THIS-OBJECT:getTempFile("txt").
    COPY-LOB icText TO FILE cFile.
    RETURN cFile.
  END METHOD.

  METHOD PUBLIC VOID TextToFile (icText AS LONGCHAR,icFileName AS CHAR).
    COPY-LOB icText TO FILE icFileName.
  END METHOD.

  METHOD PUBLIC CHARACTER TextToFile (icText AS LONGCHAR,icFileName AS CHAR,INPUT ibOpen AS LOG).
    THIS-OBJECT:TextToFile(icText,icFileName).
    RETURN DYNAMIC-FUNCTION("setWebDoc","open",icFileName). 
  END.

  METHOD PUBLIC LOGICAL ToExcel (ihHandle AS HANDLE).
    IF VALID-HANDLE(ihHandle) THEN DO:
      RUN toExcelViaFile.p (ihHandle,0).
      RETURN YES.
    END.  
    ELSE RETURN NO.
  END.

  METHOD PUBLIC INTEGER spawnProwin (INPUT icProcedure AS CHAR
                                    ,INPUT icParameter AS CHAR):
                        
    DEF VAR cProwc      AS CHAR NO-UNDO.
    DEF VAR cInifile    AS CHAR NO-UNDO.
    DEF VAR cAssemblies AS CHAR NO-UNDO.
    DEF VAR cSmallS     AS CHAR NO-UNDO.
    DEF VAR cPf         AS CHAR NO-UNDO.
    DEF VAR oStartInfo  AS System.Diagnostics.ProcessStartInfo NO-UNDO.
    DEF VAR oProcess    AS System.Diagnostics.Process NO-UNDO.
    
    cProwc = DYNAMIC-FUNCTION("getMyExeFile") NO-ERROR.
    
    IF cProwc = "" OR cProwc = ? THEN DO:
      IF PROGRESS = "FULL" THEN 
        FILE-INFO:FILE-NAME = IF SEARCH("prowin32.exe") NE ? THEN SEARCH("prowin32.exe") ELSE SEARCH("prowin.exe"). 
      ELSE 
        FILE-INFO:FILE-NAME = SEARCH("prowc.exe").        
      cProwc = '"' + FILE-INFO:FULL-PATHNAME + '"'.  
    END.
        
    IF cProwc = ? THEN DO:
      MESSAGE "Couldn't find startup Progress executable"
      VIEW-AS ALERT-BOX.
      RETURN 0.
    END.  
    
    IF SEARCH(icProcedure) = ? THEN icProcedure = SUBSTR(icProcedure,1,LENGTH(icProcedure) - 1) + "r".
    IF SEARCH(icProcedure) = ? THEN DO:
      MESSAGE "Couldn't find startup procedure " icProcedure
      VIEW-AS ALERT-BOX.
      RETURN 0.
    END.  
    
    icProcedure = SEARCH(icProcedure).
    
    cSmallS = DYNAMIC-FUNCTION("getStartupParam","-s").
    cAssemblies = DYNAMIC-FUNCTION("getStartupParam","-assemblies").
    cPf = DYNAMIC-FUNCTION("getStartupParam","-pf").
    cInifile = DYNAMIC-FUNCTION("getMyIniFile").
    IF cInifile NE "" THEN cInifile = "-ininame " + cInifile.     
        
     /* Courtesy Mike Fechner, "extreme desktop integration": */
    oStartInfo = NEW System.Diagnostics.ProcessStartInfo().
    oStartInfo:FileName = cProwc.
    oStartInfo:Arguments = cInifile + ' ' + cPf + ' ' + cSmallS + ' ' + cAssemblies + ' -p ' + icProcedure + ' -nosplash -debugalert -wy -param "' + icParameter + '" -T ' + SESSION:TEMP-DIR.
    oStartInfo:WorkingDirectory = System.Environment:CurrentDirectory.
    /* optionally obtain access to stdout of the process */
    oStartInfo:UseShellExecute = NO.
    oStartInfo:RedirectStandardOutput = YES.
     
    oProcess = System.Diagnostics.Process:Start (oStartInfo).
    
    RETURN oProcess:Id.
  END METHOD.
    
  METHOD PUBLIC LOGICAL ActivePid (INPUT iiPid AS INT):
    DEFINE VARIABLE hProcess AS System.Diagnostics.Process EXTENT.
    DEFINE VARIABLE ix       AS INTEGER                    NO-UNDO.

    hProcess = System.Diagnostics.Process:GetProcesses().
    DO ix = 1 TO EXTENT (hProcess):
      IF hProcess[ix]:Id = iiPid THEN RETURN YES.
    END. 
    RETURN NO.  
    
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
*/                               
  DESTRUCTOR PUBLIC JBoxLookupMethods ( ):
    
  END DESTRUCTOR.
END CLASS.
  
