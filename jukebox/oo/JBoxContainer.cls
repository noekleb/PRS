 
 /*------------------------------------------------------------------------
    File        : JBoxContainer
    Purpose     : Help manage the content, resize settings and such
    Syntax      : 
    Description : 
    Author(s)   : Brynjar
    Created     : Fri Feb 13 12:43:22 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
/*USING oo.* FROM PROPATH.*/

CLASS JBoxContainer:
  DEF TEMP-TABLE ttChildWin 
      FIELD hChildWin AS HANDLE.
 
  DEF PUBLIC PROPERTY CONTAINER-HANDLE AS HANDLE GET. PROTECTED SET.

  DEF PUBLIC PROPERTY CURRENT-WINDOW AS HANDLE GET. PROTECTED SET.

  DEF PUBLIC PROPERTY FRAME-HANDLE AS HANDLE GET. 
      SET(ihFrame AS HANDLE): 
        FRAME-HANDLE = ihFrame. 
      END SET.
      
  DEF PUBLIC PROPERTY BROWSE-HANDLE AS HANDLE GET. SET.    
  DEF PUBLIC PROPERTY QUERY-HANDLE AS HANDLE GET. SET.    
  DEF PUBLIC PROPERTY BUFFER-HANDLE AS HANDLE GET. SET.    
  DEF PUBLIC PROPERTY SplitBarX AS HANDLE GET. PROTECTED SET.    
  DEF PUBLIC PROPERTY SplitBarY AS HANDLE GET. PROTECTED SET.    
      
  DEF PUBLIC PROPERTY statusBar AS JBoxStatusbar GET. PROTECTED SET.

  CONSTRUCTOR PUBLIC JBoxContainer ():
      SUPER ().

    IF PROVERSION < "11" THEN CONTAINER-HANDLE = DYNAMIC-FUNCTION ("getProcedureHandle",PROGRAM-NAME(2)).
    ELSE CONTAINER-HANDLE = SOURCE-PROCEDURE.

    DYNAMIC-FUNCTION("setObjectSourceProc",CONTAINER-HANDLE).    
    DYNAMIC-FUNCTION("NewObject",CONTAINER-HANDLE:CURRENT-WINDOW,CONTAINER-HANDLE,"Container").
    CURRENT-WINDOW = CONTAINER-HANDLE:CURRENT-WINDOW NO-ERROR.
    IF VALID-HANDLE(CURRENT-WINDOW) THEN
      FRAME-HANDLE = CURRENT-WINDOW:FIRST-CHILD.
    PUBLISH "setWindowContainerObject" FROM CONTAINER-HANDLE (CONTAINER-HANDLE,THIS-OBJECT).

  END CONSTRUCTOR.
  
   METHOD PUBLIC LOGICAL bringContainerToFront():
    IF VALID-HANDLE(CONTAINER-HANDLE) THEN DO:
      PUBLISH "ViewTabProcHandle" FROM CONTAINER-HANDLE (CONTAINER-HANDLE).
      THIS-OBJECT:unFreezeContainer().
      RETURN YES.
    END.
    ELSE IF VALID-HANDLE(CURRENT-WINDOW) THEN CURRENT-WINDOW:MOVE-TO-TOP().
    ELSE RETURN NO.  
  END METHOD.  

  METHOD PUBLIC LOGICAL freezeContainer():
    IF VALID-HANDLE(CURRENT-WINDOW) THEN DO:
      DYNAMIC-FUNCTION("DoLockWindow",CURRENT-WINDOW).
      DYNAMIC-FUNCTION("setAttribute",SESSION,"userKeepsWindowLocked","yes").
      RETURN YES.
    END.
    ELSE RETURN NO.  
  END METHOD.  

  METHOD PUBLIC LOGICAL unFreezeContainer():
    IF VALID-HANDLE(CURRENT-WINDOW) THEN DO:
      DYNAMIC-FUNCTION("setAttribute",SESSION,"userKeepsWindowLocked","").
      DYNAMIC-FUNCTION("DoLockWindow",?).
      RETURN YES.
    END.
    ELSE RETURN NO.  
  END METHOD.  
  
  METHOD PUBLIC JBoxStatusbar addStatusBar():
    THIS-OBJECT:statusBar = NEW JBoxStatusbar(CONTAINER-HANDLE).
  END METHOD.

  METHOD PUBLIC JBoxStatusbar addStatusBar(icPixelList AS CHAR):
    THIS-OBJECT:statusBar = NEW JBoxStatusbar(icPixelList,CONTAINER-HANDLE).
  END METHOD.

  METHOD PUBLIC HANDLE StartChildWindow (icProcName     AS CHAR
                                        ,icWinTitle     AS CHAR
                                        ,ibNew          AS LOG).
    DEF VAR ohWinHandle    AS HANDLE NO-UNDO.
  
    PUBLISH "StartChildWindow" FROM CONTAINER-HANDLE(icProcName,icWinTitle,CONTAINER-HANDLE,ibNew,OUTPUT ohWinHandle).
    FIND FIRST ttChildWin WHERE ttChildWin.hChildWin = ohWinHandle
         NO-ERROR.
    IF NOT AVAIL ttChildWin THEN DO:
      CREATE ttChildWin.
      ttChildWin.hChildWin = ohWinHandle.
    END.       
    RETURN ohWinHandle.
    
  END METHOD.

  METHOD PUBLIC HANDLE StartChildTab (icProcName     AS CHAR
                                     ,icTabTitle     AS CHAR
                                     ,ibNew          AS LOG
                                     ,ibKeepOpen     AS LOG).
    DEF VAR ohWinHandle    AS HANDLE NO-UNDO.
  
    PUBLISH "StartChildTab" FROM CONTAINER-HANDLE(icProcName,icTabTitle,CONTAINER-HANDLE,ibNew,ibKeepOpen,"",OUTPUT ohWinHandle).
    RETURN ohWinHandle.
    
  END METHOD.
  
  METHOD PUBLIC HANDLE StartTabWindow (icProcName     AS CHAR
                                      ,icTabTitle     AS CHAR
                                      ,ibNew          AS LOG
                                      ,ibKeepOpen     AS LOG).
    DEF VAR ohWinHandle    AS HANDLE NO-UNDO.
  
    PUBLISH "StartTabWindow" FROM CONTAINER-HANDLE(icProcName,icTabTitle,CONTAINER-HANDLE,ibNew,ibKeepOpen,OUTPUT ohWinHandle).
    RETURN ohWinHandle.
    
  END METHOD.


  METHOD PUBLIC VOID initResize():
    DYNAMIC-FUNCTION("setOrgWinSize",CONTAINER-HANDLE:CURRENT-WINDOW,1600,1200,0,0).
  END METHOD.
                                 
  METHOD PUBLIC VOID initResize(iMinXmove AS INT
                               ,iMinYmove AS INT):
    DYNAMIC-FUNCTION("setOrgWinSize",CONTAINER-HANDLE:CURRENT-WINDOW,iMinXmove,iMinYmove,0,0).
  END METHOD.
                                 
  METHOD PUBLIC VOID initResize(iMinXmove AS INT
                               ,iMinYmove AS INT
                               ,iMinXsize AS INT
                               ,iMinYsize AS INT):
    DYNAMIC-FUNCTION("setOrgWinSize",CONTAINER-HANDLE:CURRENT-WINDOW,iMinXmove,iMinYmove,iMinXsize,iMinYsize).
  END METHOD.    

  METHOD PUBLIC VOID setNoResizeX(INPUT icWidgetNameList AS CHAR):
    PUBLISH "DoResizeSetting" FROM CONTAINER-HANDLE (ENTRY(1,PROGRAM-NAME(1)," "),FRAME-HANDLE,icWidgetNameList).
  END METHOD.
  METHOD PUBLIC VOID setNoResizeY(INPUT icWidgetNameList AS CHAR):
    PUBLISH "DoResizeSetting" FROM CONTAINER-HANDLE (ENTRY(1,PROGRAM-NAME(1)," "),FRAME-HANDLE,icWidgetNameList).
  END METHOD.
  METHOD PUBLIC VOID setAddResizeX(INPUT icWidgetNameList AS CHAR):
    PUBLISH "DoResizeSetting" FROM CONTAINER-HANDLE (ENTRY(1,PROGRAM-NAME(1)," "),FRAME-HANDLE,icWidgetNameList).
  END METHOD.
  METHOD PUBLIC VOID setAddResizeY(INPUT icWidgetNameList AS CHAR):
    PUBLISH "DoResizeSetting" FROM CONTAINER-HANDLE (ENTRY(1,PROGRAM-NAME(1)," "),FRAME-HANDLE,icWidgetNameList).
  END METHOD.

  METHOD PUBLIC VOID setNoResizeX(INPUT ihFrame AS HANDLE
                                 ,INPUT icWidgetNameList AS CHAR):
    PUBLISH "DoResizeSetting" FROM CONTAINER-HANDLE (ENTRY(1,PROGRAM-NAME(1)," "),ihFrame,icWidgetNameList).
  END METHOD.
  METHOD PUBLIC VOID setNoResizeY(INPUT ihFrame AS HANDLE
                                 ,INPUT icWidgetNameList AS CHAR):
    PUBLISH "DoResizeSetting" FROM CONTAINER-HANDLE (ENTRY(1,PROGRAM-NAME(1)," "),ihFrame,icWidgetNameList).
  END METHOD.
  METHOD PUBLIC VOID setAddResizeX(INPUT ihFrame AS HANDLE
                                  ,INPUT icWidgetNameList AS CHAR):
    PUBLISH "DoResizeSetting" FROM CONTAINER-HANDLE (ENTRY(1,PROGRAM-NAME(1)," "),ihFrame,icWidgetNameList).
  END METHOD.
  METHOD PUBLIC VOID setAddResizeY(INPUT ihFrame AS HANDLE
                                  ,INPUT icWidgetNameList AS CHAR):
    PUBLISH "DoResizeSetting" FROM CONTAINER-HANDLE (ENTRY(1,PROGRAM-NAME(1)," "),ihFrame,icWidgetNameList).
  END METHOD.

  METHOD PUBLIC VOID setNoMoveX(INPUT icWidgetNameList AS CHAR):
    PUBLISH "DoResizeSetting" FROM CONTAINER-HANDLE (ENTRY(1,PROGRAM-NAME(1)," "),FRAME-HANDLE,icWidgetNameList).
  END METHOD.
  METHOD PUBLIC VOID setNoMoveY(INPUT icWidgetNameList AS CHAR):
    PUBLISH "DoResizeSetting" FROM CONTAINER-HANDLE (ENTRY(1,PROGRAM-NAME(1)," "),FRAME-HANDLE,icWidgetNameList).
  END METHOD.
  METHOD PUBLIC VOID setAddMoveX(INPUT icWidgetNameList AS CHAR):
    PUBLISH "DoResizeSetting" FROM CONTAINER-HANDLE (ENTRY(1,PROGRAM-NAME(1)," "),FRAME-HANDLE,icWidgetNameList).
  END METHOD.
  METHOD PUBLIC VOID setAddMoveY(INPUT icWidgetNameList AS CHAR):
    PUBLISH "DoResizeSetting" FROM CONTAINER-HANDLE (ENTRY(1,PROGRAM-NAME(1)," "),FRAME-HANDLE,icWidgetNameList).
  END METHOD.
  
  METHOD PUBLIC VOID setNoMoveX(INPUT ihFrame AS HANDLE
                               ,INPUT icWidgetNameList AS CHAR):
    PUBLISH "DoResizeSetting" FROM CONTAINER-HANDLE (ENTRY(1,PROGRAM-NAME(1)," "),ihFrame,icWidgetNameList).
  END METHOD.
  METHOD PUBLIC VOID setNoMoveY(INPUT ihFrame AS HANDLE
                               ,INPUT icWidgetNameList AS CHAR):
    PUBLISH "DoResizeSetting" FROM CONTAINER-HANDLE (ENTRY(1,PROGRAM-NAME(1)," "),ihFrame,icWidgetNameList).
  END METHOD.
  METHOD PUBLIC VOID setAddMoveX(INPUT ihFrame AS HANDLE
                                ,INPUT icWidgetNameList AS CHAR):
    PUBLISH "DoResizeSetting" FROM CONTAINER-HANDLE (ENTRY(1,PROGRAM-NAME(1)," "),ihFrame,icWidgetNameList).
  END METHOD.
  METHOD PUBLIC VOID setAddMoveY(INPUT ihFrame AS HANDLE
                                ,INPUT icWidgetNameList AS CHAR):
    PUBLISH "DoResizeSetting" FROM CONTAINER-HANDLE (ENTRY(1,PROGRAM-NAME(1)," "),ihFrame,icWidgetNameList).
  END METHOD.
  
  METHOD PUBLIC VOID setSplitBarX(INPUT ihSplitBarBtn AS HANDLE):
    SplitBarX = ihSplitBarBtn.
    PUBLISH "DoSplitBar" FROM CONTAINER-HANDLE (ENTRY(1,PROGRAM-NAME(1)," "),ihSplitBarBtn,"",0,0).
  END METHOD.
  METHOD PUBLIC VOID setFollowSplitBarX(INPUT ihSplitBarBtn AS HANDLE
                                       ,INPUT icWidgetList  AS CHAR):
    PUBLISH "DoSplitBar" FROM CONTAINER-HANDLE (ENTRY(1,PROGRAM-NAME(1)," "),ihSplitBarBtn,icWidgetList,0,0).
  END METHOD.
  METHOD PUBLIC VOID setFollowSplitBarX(INPUT icWidgetList  AS CHAR):
    PUBLISH "DoSplitBar" FROM CONTAINER-HANDLE (ENTRY(1,PROGRAM-NAME(1)," "),SplitBarX,icWidgetList,0,0).
  END METHOD.
  METHOD PUBLIC VOID setSplitBarXlimits(INPUT ihSplitBarBtn AS HANDLE
                                       ,INPUT iMinX         AS INT
                                       ,INPUT iMaxX         AS INT):
    PUBLISH "DoSplitBar" FROM CONTAINER-HANDLE (ENTRY(1,PROGRAM-NAME(1)," "),ihSplitBarBtn,"",iMinX,iMaxX).
  END METHOD.
  METHOD PUBLIC VOID setSplitBarXlimits(INPUT iMinX         AS INT
                                       ,INPUT iMaxX         AS INT):
    PUBLISH "DoSplitBar" FROM CONTAINER-HANDLE (ENTRY(1,PROGRAM-NAME(1)," "),SplitBarX,"",iMinX,iMaxX).
  END METHOD.

  METHOD PUBLIC VOID setSplitBarY(INPUT ihSplitBarBtn AS HANDLE):
    SplitBarY = ihSplitBarBtn.
    PUBLISH "DoSplitBar" FROM CONTAINER-HANDLE (ENTRY(1,PROGRAM-NAME(1)," "),ihSplitBarBtn,"",0,0).
  END METHOD.
  METHOD PUBLIC VOID setFollowSplitBarY(INPUT ihSplitBarBtn AS HANDLE
                                       ,INPUT icWidgetList  AS CHAR):
    PUBLISH "DoSplitBar" FROM CONTAINER-HANDLE (ENTRY(1,PROGRAM-NAME(1)," "),ihSplitBarBtn,icWidgetList,0,0).
  END METHOD.
  METHOD PUBLIC VOID setFollowSplitBarY(INPUT icWidgetList  AS CHAR):
    PUBLISH "DoSplitBar" FROM CONTAINER-HANDLE (ENTRY(1,PROGRAM-NAME(1)," "),SplitBarY,icWidgetList,0,0).
  END METHOD.
  METHOD PUBLIC VOID setSplitBarYlimits(INPUT ihSplitBarBtn AS HANDLE
                                       ,INPUT iMinX         AS INT
                                       ,INPUT iMaxX         AS INT):
    PUBLISH "DoSplitBar" FROM CONTAINER-HANDLE (ENTRY(1,PROGRAM-NAME(1)," "),ihSplitBarBtn,"",iMinX,iMaxX).
  END METHOD.
  METHOD PUBLIC VOID setSplitBarYlimits(INPUT iMinX         AS INT
                                       ,INPUT iMaxX         AS INT):
    PUBLISH "DoSplitBar" FROM CONTAINER-HANDLE (ENTRY(1,PROGRAM-NAME(1)," "),SplitBarY,"",iMinX,iMaxX).
  END METHOD.

  METHOD PUBLIC VOID setResizeXgroup(INPUT ifPercent        AS DEC
                                    ,INPUT icWidgetNameList AS CHAR):
    PUBLISH "DoResizeGroup" FROM CONTAINER-HANDLE (ENTRY(1,PROGRAM-NAME(1)," "),FRAME-HANDLE,ifPercent,icWidgetNameList).
  END METHOD.
  METHOD PUBLIC VOID setResizeYgroup(INPUT ifPercent        AS DEC
                                    ,INPUT icWidgetNameList AS CHAR):
    PUBLISH "DoResizeGroup" FROM CONTAINER-HANDLE (ENTRY(1,PROGRAM-NAME(1)," "),FRAME-HANDLE,ifPercent,icWidgetNameList).
  END METHOD.
  METHOD PUBLIC VOID setMoveXgroup(INPUT ifPercent        AS DEC
                                  ,INPUT icWidgetNameList AS CHAR):
    PUBLISH "DoResizeGroup" FROM CONTAINER-HANDLE (ENTRY(1,PROGRAM-NAME(1)," "),FRAME-HANDLE,ifPercent,icWidgetNameList).
  END METHOD.
  METHOD PUBLIC VOID setMoveYgroup(INPUT ifPercent        AS DEC
                                    ,INPUT icWidgetNameList AS CHAR):
    PUBLISH "DoResizeGroup" FROM CONTAINER-HANDLE (ENTRY(1,PROGRAM-NAME(1)," "),FRAME-HANDLE,ifPercent,icWidgetNameList).
  END METHOD.
  
  DESTRUCTOR PUBLIC JBoxContainer ( ):
    FOR EACH ttChildWin:
      IF VALID-HANDLE(ttChildWin.hChildWin) THEN 
        APPLY "close" TO ttChildWin.hChildWin.
    END.  
  END DESTRUCTOR.
END CLASS.
  