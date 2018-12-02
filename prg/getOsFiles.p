/* ====================================================================
   file      windows.i
   by        Jurjen Dijkstra, 1997
             mailto:jurjen@global-shared.com
             http://www.global-shared.com
   language  Progress 8.2A
   ==================================================================== */

&IF DEFINED(WINDOWS_I)=0 &THEN
&GLOBAL-DEFINE WINDOWS_I

&IF "{&OPSYS}":U="WIN32":U &THEN
   /* 32-bit definitions, Progress 8.2+ */

   &GLOB A A
   /* data types */
   &Glob HWND long
   &Glob BOOL long
   &Glob HINSTANCE long
   &Glob INT long
   &GLOB INTSIZE 4

   /* libraries */
   &GLOB USER     "user32"
   &GLOB KERNEL   "kernel32"
   &GLOB SHELL    "shell32"
   &GLOB MAPI     "mapi32"
   &GLOB GDI      "gdi32"
   &GLOB MMEDIA   "winmm"
   &GLOB WINSPOOL "winspool.drv"
   &GLOB ADVAPI   "advapi32"
   &GLOB A A

&ELSE
   /* 16-bit definitions, Progress 7 to 8.1 */

   /* data types */
   &Glob HWND short
   &Glob BOOL short
   &Glob HINSTANCE short
   &Glob INT short
   &GLOB INTSIZE 2

   /* libraries */
   &GLOB USER   "user.exe"
   &GLOB KERNEL "kernel.exe"
   &GLOB SHELL  "shell.dll"
   &GLOB MAPI   "mapi.dll"
   &GLOB GDI    "gdi.exe"
   &GLOB A

&ENDIF


/* messages */
&Glob WM_PAINT 15
&Glob WM_HSCROLL 276
&Glob WM_VSCROLL 277
&Glob WM_LBUTTONDOWN 513
&Glob WM_LBUTTONUP 514
&Glob WM_RBUTTONDOWN 516
&Glob WM_RBUTTONUP 517
&GLOB WM_USER 1024

/* mouse buttons */
&Glob MK_LBUTTON 1
&Glob MK_RBUTTON 2

/* scrollbars */
&Glob SB_HORZ 0
&Glob SB_VERT 1
&Glob SB_BOTH 3
&Glob SB_THUMBPOSITION 4

/* editors */
&IF "{&OPSYS}":U="WIN32":U &THEN
   &GLOB EM_SETPASSWORDCHAR 204
&ELSE
    &GLOB EM_SETPASSWORDCHAR {&WM_USER} + 28
&ENDIF

/* some window styles */
&GLOB GWL_STYLE -16
&GLOB WS_MAXIMIZEBOX 65536
&GLOB WS_MINIMIZEBOX 131072
&GLOB WS_THICKFRAME  262144
&GLOB WS_CAPTION 12582912
&GLOB WS_BORDER 8388608

/* some extended window styles */
&GLOB GWL_EXSTYLE -20
&GLOB WS_EX_CONTEXTHELP 1024
&GLOB WS_EX_PALETTEWINDOW 392

/* system commands/menu */
&GLOB SC_SIZE      61440  
&GLOB SC_MINIMIZE  61472
&GLOB SC_MAXIMIZE  61488  
&GLOB MF_BYCOMMAND 0

/* placement order (Z-order) */
&GLOB HWND_TOPMOST -1
&GLOB HWND_NOTOPMOST -2
 
/* window-positioning flags */
&GLOB SWP_NOSIZE 1
&GLOB SWP_NOMOVE 2
&GLOB SWP_NOZORDER 4
&GLOB SWP_NOACTIVATE 16 
&GLOB SWP_FRAMECHANGED 32
&GLOB SWP_SHOWWINDOW 64

/* registry */
&GLOB HKEY_CLASSES_ROOT -2147483648
&GLOB HKEY_CURRENT_USER -2147483647
&GLOB HKEY_LOCAL_MACHINE -2147483646
&GLOB HKEY_USERS -2147483645
&GLOB HKEY_PERFORMANCE_DATA -2147483644
&GLOB HKEY_CURRENT_CONFIG -2147483643
&GLOB HKEY_DYN_DATA -2147483642

&GLOB ERROR_SUCCESS 0
&GLOB ERROR_NO_MORE_ITEMS 259

&GLOB MAX_PATH 260

/* results from WaitForSingleObject */
&GLOB WAIT_ABANDONED 128
&GLOB WAIT_OBJECT_0 0

/* menu manipulation */
&GLOB MF_BYPOSITION 1024
&GLOB MF_REMOVE     256



&ENDIF  /* &IF DEFINED(WINDOWS_I)=0 */



/* =================================================================
   file    : file-api.p
   purpose : some PROCEDUREs for returning file/dir information
   by      : Jurjen Dijkstra, 1997
             email jurjend@pi.net
             from  http://home.pi.net/~jurjend
   ================================================================= */



/* ================================================================= */
/*               STANDARD WINDOWS DEFINITIONS                        */
/* ================================================================= */

/* ================================================================= */
/*                 additions to windows.i                            */
/* ================================================================= */


&IF "{&OPSYS}":U="WIN32":U &THEN
   /* 32-bit definitions, Progress 8.2+ */

   &GLOB  MAX_PATH 260
   &GLOB  FIND_DATA-SIZE 4           /* dwFileAttributes       */~
                       + 8           /* ftCreationTime         */~
                       + 8           /* ftLastAccessTime       */~
                       + 8           /* ftLastWriteTime        */~
                       + 4           /* nFileSizeHigh          */~
                       + 4           /* nFileSizeLow           */~
                       + 4           /* dwReserved0            */~
                       + 4           /* dwReserved1            */~
                       + {&MAX_PATH} /* cFileName[MAX_PATH]    */~
                       + 14          /* cAlternateFileName[14] */


&ELSE
   &MESSAGE "Don't know 16-bit definitions for MAX_PATH and FIND_DATA structure"
&ENDIF

&GLOB INVALID_HANDLE_VALUE -1


/* ================================================================= */
/*                 additions to windows.p                            */
/* ================================================================= */


PROCEDURE FindFirstFile{&A} external {&KERNEL} :
    define input parameter  lpFileName AS CHAR.
    define input parameter  lpFindFileData as memptr.
    define return parameter hSearch as {&INT}.
end PROCEDURE.    

PROCEDURE FindNextFile{&A} external {&KERNEL} :
    define input parameter  hSearch as {&INT}.
    define input parameter  lpFindFileData as memptr.
    define return parameter found as {&BOOL}.
end PROCEDURE.

PROCEDURE FindClose external {&kernel} :
    define input parameter hSearch as {&INT}.
    define return parameter ReturnValue as {&BOOL}.
end PROCEDURE.

PROCEDURE FileTimeToLocalFileTime external {&KERNEL} :
  define input parameter lpFileTime as long.  /* pointer */
  define input parameter lpLocalFileTime as long. /* pointer */
  define return parameter ReturnValue as {&BOOL}.
end PROCEDURE.

PROCEDURE FileTimeToDosDateTime external {&KERNEL} :
  define input parameter  lpFileTime as long. /* pointer */
  define output parameter FatDate as short.   /* word */
  define output parameter FatTime as short.   /* word */
  define return parameter ReturnValue as {&BOOL}.
end PROCEDURE.


/* ================================================================= */
/*                  'high level' PROCEDUREs                          */
/* ================================================================= */

   
/* -----------------------------------------------------------------
   PROCEDURE FileFind
   purpose   want to know information about one particular file?
             1. call FileFind to obtain a lpFindData structure
             2. call one or more of the FileInfo_xxx PROCEDUREs
                to get information from the lpFindData structure
   ----------------------------------------------------------------- */

PROCEDURE FileFind :
   def input  parameter FileName   AS CHAR.
   DEFINE OUTPUT PARAMETER lpFindData as memptr.

   DEFINE VARIABLE hSearch     AS INTEGER no-undo.
   DEFINE VARIABLE ReturnValue AS INTEGER NO-UNDO.

   set-size(lpFindData) = {&FIND_DATA-SIZE}.
   run FindFirstFile{&A} (FileName, lpFindData, output hSearch).
   if hSearch<>{&INVALID_HANDLE_VALUE} then
      run FindClose (hSearch, output ReturnValue).
   else
      set-size(lpFindData)=0.
end PROCEDURE.

/* -----------------------------------------------------------------
   PROCEDURE FileFindLoop
   purpose   want to scan a directory using wildcards?
             call FileFindLoop; for each found file it will run
             value(ipProcessFindData) in hExtProc. That callback-
             PROCEDURE can use the lpFindData structure to decide
             what to do next.
   ----------------------------------------------------------------- */

PROCEDURE FileFindLoop :
   DEFINE INPUT PARAMETER FileMask          AS CHAR.
   DEFINE INPUT PARAMETER ipProcessFindData AS CHAR.
   DEFINE INPUT PARAMETER hExtProc          as handle.
   
   DEFINE VARIABLE lpFindData as memptr no-undo.
   DEFINE VARIABLE hSearch AS INTEGER no-undo.
   DEFINE VARIABLE found AS INTEGER no-undo initial 1.
   DEFINE VARIABLE ReturnValue AS INTEGER no-undo.

   set-size(lpFindData) = {&FIND_DATA-SIZE}.

   run FindFirstFile{&A} (FileMask, lpFindData, output hSearch).
   if hSearch<>{&INVALID_HANDLE_VALUE} then do:
     do while found<>0 :
        run value(ipProcessFindData) in hExtProc (lpFindData).
        run FindNextFile{&A} (hSearch, lpFindData, output found).
     end.
     run FindClose (hSearch, output ReturnValue).
   end.
   
   set-size(lpFindData) = 0.

end PROCEDURE.

   
/* ================================================================= */
/* PROCEDUREs for returning info from the  lpFindData structure:     */
/* ================================================================= */

/* -----------------------------------------------------------------
   PROCEDURE FileInfo_Size
   purpose   returns size in bytes of a file.
   ----------------------------------------------------------------- */
PROCEDURE FileInfo_Size :
   DEFINE INPUT PARAMETER  lpFindData AS memptr.
   DEFINE OUTPUT PARAMETER FileSize AS INTEGER initial -1.

   IF GET-SIZE(lpFindData)={&FIND_DATA-SIZE} THEN
      FileSize = GET-LONG(lpFindData, 33). /* =nFileSizeLow */
end PROCEDURE.


/* -----------------------------------------------------------------
   PROCEDURE FileInfo_LongName
   purpose   returns long filename
   ----------------------------------------------------------------- */
PROCEDURE FileInfo_LongName :
   DEFINE INPUT PARAMETER  lpFindData AS memptr.
   DEFINE OUTPUT PARAMETER FileName   AS CHAR initial "".

   IF GET-SIZE(lpFindData)={&FIND_DATA-SIZE} THEN
      FileName = GET-STRING(lpFindData, 45). /* =cFileName */
END PROCEDURE.

/* -----------------------------------------------------------------
   PROCEDURE FileInfo_ShortName
   purpose   returns short filename. If the Long name is already short,
             ShortName would be empty. But this PROCEDURE returns
             LongName if ShortName is empty.
   ----------------------------------------------------------------- */

PROCEDURE FileInfo_ShortName :
   DEFINE INPUT PARAMETER  lpFindData AS memptr.
   DEFINE OUTPUT PARAMETER FileName   AS CHAR initial "".

   IF GET-SIZE(lpFindData)={&FIND_DATA-SIZE} THEN 
   DO:
      FileName = GET-STRING(lpFindData, 45 + {&MAX_PATH}). /* =cAlternateFileName */
      IF FileName="" THEN
         FileName = GET-STRING(lpFindData, 45). /* =cFileName */
   END.
END PROCEDURE.

/* -----------------------------------------------------------------
   PROCEDURE FileInfo_LastAccess
   purpose   returns date and time when file was last accessed.
             Time can be displayed using string(chTime,"hh:mm").
   ----------------------------------------------------------------- */

PROCEDURE FileInfo_LastAccess :
   DEFINE INPUT PARAMETER  lpFindData as memptr.
   DEFINE OUTPUT PARAMETER chDate AS DATE.
   DEFINE OUTPUT PARAMETER chTime AS INTEGER.

   IF GET-SIZE(lpFindData)={&FIND_DATA-SIZE} THEN
   RUN FileTimeToProgressDateTime(GET-POINTER-VALUE(lpFindData) + 12, 
                                  OUTPUT chDate, 
                                  OUTPUT chTime).

end PROCEDURE.

/* -----------------------------------------------------------------
   PROCEDURE FileInfo_LastWrite
   purpose   returns date and time when file was last changed.
             Time can be displayed using string(chTime,"hh:mm").
   ----------------------------------------------------------------- */

PROCEDURE FileInfo_LastWrite :
   DEFINE INPUT PARAMETER  lpFindData AS memptr.
   DEFINE OUTPUT PARAMETER chDate AS DATE.
   DEFINE OUTPUT PARAMETER chTime AS INTEGER.
    
   IF GET-SIZE(lpFindData)={&FIND_DATA-SIZE} THEN
   RUN FileTimeToProgressDateTime(GET-POINTER-VALUE(lpFindData) + 20, 
                                  OUTPUT chDate, 
                                  OUTPUT chTime).
    
END PROCEDURE.



DEFINE INPUT PARAMETER ipcDirectory AS CHAR NO-UNDO. 
DEFINE INPUT PARAMETER ipcFileMask AS CHAR NO-UNDO. 
DEFINE INPUT PARAMETER ipcCallBackProcName AS CHAR NO-UNDO. 
DEFINE INPUT PARAMETER iphCallBackProc AS HANDLE NO-UNDO. 
 
FILE-INFO:FILE-NAME = ipcDirectory. 

IF FILE-INFO:PATHNAME = ? THEN 
    RETURN ERROR "Can't access directory:" + ipcDirectory.

ELSE IF FILE-INFO:FILE-TYPE BEGINS "D" THEN
   RUN fileFindLoop(ipcDirectory + '\' + ipcFilemask, 
                  'ProcedureCallBack',
                   THIS-PROCEDURE).

ELSE RETURN ERROR "Unknown directory issue".     


PROCEDURE ProcedureCallBack:                            

   DEFINE INPUT PARAMETER mFileName AS memptr NO-UNDO. 
   DEFINE VARIABLE cFilename AS CHAR NO-UNDO. 
   DEFINE VARIABLE iTime AS INT NO-UNDO. 
   DEFINE VARIABLE dDate AS DATE NO-UNDO. 

   RUN FileInfo_LongName(mFileName,OUTPUT cFilename).
   RUN VALUE(ipcCallBackProcName) IN iphCallBackProc 
         (ipcDirectory,cFileName,dDate,iTime).
END. 

