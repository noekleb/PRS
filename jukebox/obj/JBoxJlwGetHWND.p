DEF INPUT  PARAM icPattern  AS CHAR NO-UNDO.
DEF INPUT  PARAM icNewTitle AS CHAR NO-UNDO.
DEF INPUT  PARAM icParam    AS CHAR NO-UNDO.
DEF OUTPUT PARAM oiHWND     AS INT  NO-UNDO. 

DEF VAR oiReturn AS INT NO-UNDO.

IF SEARCH("VolvatUtil.dll") = ? THEN RETURN.

PROCEDURE BuildDesktopWindowList  EXTERNAL "VolvatUtil.dll" PERSISTENT:
    DEFINE RETURN PARAMETER BDWL_iEntries AS LONG.
END PROCEDURE.

PROCEDURE GetEnumInfo EXTERNAL "VolvatUtil.dll":
    DEFINE INPUT PARAMETER GEI_iIndex AS LONG.
    DEFINE RETURN PARAMETER GEI_mptrInfo AS MEMPTR.
END PROCEDURE.

PROCEDURE SetForegroundWindow EXTERNAL "USER32.DLL":
    DEFINE INPUT  PARAMETER intHwnd    AS LONG.
    DEFINE RETURN PARAMETER intResult  AS LONG.
END PROCEDURE.

procedure ShowWindow external "user32":
  def return param dll-return    as long no-undo.
  def input  param hWnd          as long no-undo.
  def input  param nCmdShow      as long no-undo.
end procedure.

procedure SetWindowTextA external "user32":
  def input  param hWnd          as long no-undo.
  def input  param cTitle        as CHAR no-undo.
  def return param dll-return    as long no-undo.
end procedure.

DEFINE VARIABLE i          AS INTEGER NO-UNDO.
DEFINE VARIABLE iItem      AS INTEGER NO-UNDO.
DEFINE VARIABLE mptr       AS MEMPTR  NO-UNDO.  
DEFINE VARIABLE mString    AS MEMPTR  NO-UNDO.  
DEFINE VARIABLE winhandle  AS INTEGER NO-UNDO.

RUN BuildDesktopWindowList(OUTPUT i).

IF i > 0 THEN DO:
  i = i - 1.
  REPEAT iItem = 0 TO i  BY 1:
    RUN GetEnumInfo(iITem,OUTPUT mptr).
    /*First get the window handle from the pointer.*/
    winhandle = GET-LONG(mptr,1).
    /*Now, get the string pointer and set mString to point to it.*/
    SET-POINTER-VALUE(mString) = GET-LONG(mptr,5).
    /*Finally, get the window title.*/
    IF GET-STRING(mString,1) MATCHES icPattern THEN DO:
      oiHWND = winhandle.
      LEAVE.  
    END.
  END.
END.

IF oiHWND = 0 THEN RETURN.

/*====================================================================
  Brynjar,  when retrieving the items from the you must declare two mempointer
  variables.  The first mempointer is passed to GetEnumInfo.  The second
  is used to retrieve the pointer that is stored in the first pointer at 
  position 5.  This pointer contains the window title string.
  Do not initialize any of these pointers.  The DLL handles cleaning up 
  the pointers, so you don't have to worry about setting their size and
  using set-size.
=======================================================================*/

IF icNewTitle NE "" THEN
  RUN SetWindowTextA (oiHWND,icNewTitle,OUTPUT oiReturn).

IF icParam MATCHES "*restore*" THEN DO:
  RUN ShowWindow (OUTPUT oiReturn,oiHWND,9).
  RUN SetForegroundWindow (oiHWND,OUTPUT oiReturn).   
END.
