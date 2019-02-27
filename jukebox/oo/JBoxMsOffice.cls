 
 /*------------------------------------------------------------------------
    File        : JboxMsOffice
    Purpose     : Various functions
    Syntax      : 
    Description : 
    Author(s)   : Brynjar
    Created     : Thy Mar 3 12:43:22 CEST 2016
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

CLASS JBoxMsOffice:

  DEF VAR oStartInfo    AS System.Diagnostics.ProcessStartInfo NO-UNDO.
  DEF VAR oProcess      AS System.Diagnostics.Process NO-UNDO.
  

  CONSTRUCTOR PUBLIC JboxMsOffice ():

  END CONSTRUCTOR.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

  METHOD PUBLIC VOID Word(INPUT icFileName AS CHAR):
    
    DEF VAR cProwc      AS CHAR NO-UNDO.
    DEF VAR cServer     AS CHAR NO-UNDO.
    DEF VAR ix          AS INT  NO-UNDO.
    
    FILE-INFO:FILE-NAME = IF PROGRESS = "FULL" THEN SEARCH("prowin32.exe") ELSE SEARCH("prowc.exe").
    cProwc = '"' + FILE-INFO:FULL-PATHNAME + '"'.
    IF cProwc = ? THEN RETURN.
    
    cServer = IF PROGRESS = "FULL" THEN SEARCH("JBoxComEventServer.w") ELSE SEARCH("JBoxComEventServer.r").
    
    IF cServer = ? THEN DO:
      MESSAGE "Com event server procedure is missing"
      VIEW-AS ALERT-BOX.
      RETURN.
    END.
    
     /* Courtesy Mike Fechner, "extreme desktop integration": */
    oStartInfo = NEW System.Diagnostics.ProcessStartInfo().
    oStartInfo:FileName = cProwc.
    oStartInfo:Arguments = '-p ' + cServer + ' -nosplash -debugalert -wy -param "' + icFileName + '" -T ' + SESSION:TEMP-DIR.
    oStartInfo:WorkingDirectory = System.Environment:CurrentDirectory.
    /* optionally obtain access to stdout of the process */
    oStartInfo:UseShellExecute = NO.
    oStartInfo:RedirectStandardOutput = YES.
     
    oProcess = System.Diagnostics.Process:Start (oStartInfo).
/*    oProcess:EnableRaisingEvents = YES.                         */
/*    oProcess:Exited:Subscribe(THIS-OBJECT:ProcessExitedHandler).*/
    
    REPEAT WHILE ActivePid(oProcess:Id):
      PAUSE 1.
    END.
  END METHOD.

  METHOD PUBLIC LOGICAL ActivePid (INPUT iiPid AS INT):
    DEFINE VARIABLE hProcess AS System.Diagnostics.Process EXTENT.
    DEFINE VARIABLE i        AS INTEGER                    NO-UNDO.
    DEFINE VARIABLE Pid     AS INTEGER.
    DEF VAR bKill AS LOG NO-UNDO.

    hProcess = System.Diagnostics.Process:GetProcesses().
    DO i = 1 TO EXTENT (hProcess):
      IF hProcess[i]:Id = iiPid THEN RETURN YES.
    END. 
    RETURN NO.  
    
  END METHOD.  

  METHOD PROTECTED VOID ProcessExitedHandler( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
    MESSAGE "exit"
    VIEW-AS ALERT-BOX.
       RETURN.
  END METHOD.
    
  DESTRUCTOR PUBLIC JboxMsOffice ( ):
    
  END DESTRUCTOR.
END CLASS.
  
