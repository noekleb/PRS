 /*------------------------------------------------------------------------
    File        : JBoxSplitbar
    Purpose     : Helper class for splitbar
    Syntax      : 
    Description : 
    Author(s)   : Brynjar
    Created     : Mon Feb 23 12:43:22 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

CLASS JBoxSplitbar:
  DEF VAR hSourceProc   AS HANDLE NO-UNDO.
  DEF VAR cDirection    AS CHAR   NO-UNDO.

  DEF PUBLIC PROPERTY CURRENT-WINDOW AS HANDLE
      GET(): RETURN hSourceProc:CURRENT-WINDOW. 
      END GET. 
      PROTECTED SET.

  DEF PUBLIC PROPERTY SPLITBAR-HANDLE AS HANDLE GET. 
      SET(ihSplitbar AS HANDLE): 
        SPLITBAR-HANDLE = ihSplitbar. 
      END SET.
      
  DEF PUBLIC PROPERTY statusBar AS JBoxStatusbar GET. PROTECTED SET.

  CONSTRUCTOR PUBLIC JBoxSplitbar (INPUT ihSplitBarbutton AS HANDLE):
      SUPER ().

    IF PROVERSION < "11" THEN hSourceProc = DYNAMIC-FUNCTION ("getProcedureHandle",PROGRAM-NAME(2)).
    ELSE hSourceProc = SOURCE-PROCEDURE.

    IF ihSplitBarbutton:HEIGHT-PIXELS > 20 THEN cDirection = "X". ELSE cDirection = "Y".
    IF cDirection = "X" THEN DO:
      DYNAMIC-FUNCTION("setSplitBarX",ihSplitBarButton:WINDOW, ihSplitBarButton:HANDLE,NO).
/*      DYNAMIC-FUNCTION("setAddResizeY", hSourceProc:CURRENT-WINDOW, ihSplitBarButton:FRAME,ihSplitBarbutton:NAME).*/
/*      MESSAGE                                                                                                     */
/*      VIEW-AS ALERT-BOX.                                                                                          */
    END.  
    ELSE 
      DYNAMIC-FUNCTION("setSplitBarY",ihSplitBarButton:WINDOW, ihSplitBarButton:HANDLE,NO).
    
    SPLITBAR-HANDLE = ihSplitbarButton.

  END CONSTRUCTOR.
  

  METHOD PUBLIC VOID setFollowSplitBar(INPUT icWidgetHandleList AS CHAR):
    PUBLISH "DoResizeSetting" FROM hSourceProc (ENTRY(1,PROGRAM-NAME(1)," ") + cDirection,SPLITBAR-HANDLE,icWidgetHandleList).
  END METHOD.

  METHOD PUBLIC VOID setFollowSplitBar(INPUT ihBorderWidget AS HANDLE):
    THIS-OBJECT:setFollowSplitBar(DYNAMIC-FUNCTION("getWidgetsByLasso",ihBorderWidget,0,"frame,browse,control-frame,rectangle,editor")).
  END METHOD.
  
  /*
  METHOD PUBLIC VOID setNoResizeY(INPUT icWidgetNameList AS CHAR):
    PUBLISH "DoResizeSetting" FROM hSourceProc (ENTRY(1,PROGRAM-NAME(1)," "),FRAME-HANDLE,icWidgetNameList).
  END METHOD.
  METHOD PUBLIC VOID setAddResizeX(INPUT icWidgetNameList AS CHAR):
    PUBLISH "DoResizeSetting" FROM hSourceProc (ENTRY(1,PROGRAM-NAME(1)," "),FRAME-HANDLE,icWidgetNameList).
  END METHOD.
  METHOD PUBLIC VOID setAddResizeY(INPUT icWidgetNameList AS CHAR):
    PUBLISH "DoResizeSetting" FROM hSourceProc (ENTRY(1,PROGRAM-NAME(1)," "),FRAME-HANDLE,icWidgetNameList).
  END METHOD.

  METHOD PUBLIC VOID setNoResizeX(INPUT ihFrame AS HANDLE
                                 ,INPUT icWidgetNameList AS CHAR):
    PUBLISH "DoResizeSetting" FROM hSourceProc (ENTRY(1,PROGRAM-NAME(1)," "),ihFrame,icWidgetNameList).
  END METHOD.
  METHOD PUBLIC VOID setNoResizeY(INPUT ihFrame AS HANDLE
                                 ,INPUT icWidgetNameList AS CHAR):
    PUBLISH "DoResizeSetting" FROM hSourceProc (ENTRY(1,PROGRAM-NAME(1)," "),ihFrame,icWidgetNameList).
  END METHOD.
  METHOD PUBLIC VOID setAddResizeX(INPUT ihFrame AS HANDLE
                                  ,INPUT icWidgetNameList AS CHAR):
    PUBLISH "DoResizeSetting" FROM hSourceProc (ENTRY(1,PROGRAM-NAME(1)," "),ihFrame,icWidgetNameList).
  END METHOD.
  METHOD PUBLIC VOID setAddResizeY(INPUT ihFrame AS HANDLE
                                  ,INPUT icWidgetNameList AS CHAR):
    PUBLISH "DoResizeSetting" FROM hSourceProc (ENTRY(1,PROGRAM-NAME(1)," "),ihFrame,icWidgetNameList).
  END METHOD.

  METHOD PUBLIC VOID setNoMoveX(INPUT icWidgetNameList AS CHAR):
    PUBLISH "DoResizeSetting" FROM hSourceProc (ENTRY(1,PROGRAM-NAME(1)," "),FRAME-HANDLE,icWidgetNameList).
  END METHOD.
  METHOD PUBLIC VOID setNoMoveY(INPUT icWidgetNameList AS CHAR):
    PUBLISH "DoResizeSetting" FROM hSourceProc (ENTRY(1,PROGRAM-NAME(1)," "),FRAME-HANDLE,icWidgetNameList).
  END METHOD.
  METHOD PUBLIC VOID setAddMoveX(INPUT icWidgetNameList AS CHAR):
    PUBLISH "DoResizeSetting" FROM hSourceProc (ENTRY(1,PROGRAM-NAME(1)," "),FRAME-HANDLE,icWidgetNameList).
  END METHOD.
  METHOD PUBLIC VOID setAddMoveY(INPUT icWidgetNameList AS CHAR):
    PUBLISH "DoResizeSetting" FROM hSourceProc (ENTRY(1,PROGRAM-NAME(1)," "),FRAME-HANDLE,icWidgetNameList).
  END METHOD.
  
  METHOD PUBLIC VOID setNoMoveX(INPUT ihFrame AS HANDLE
                               ,INPUT icWidgetNameList AS CHAR):
    PUBLISH "DoResizeSetting" FROM hSourceProc (ENTRY(1,PROGRAM-NAME(1)," "),ihFrame,icWidgetNameList).
  END METHOD.
  METHOD PUBLIC VOID setNoMoveY(INPUT ihFrame AS HANDLE
                               ,INPUT icWidgetNameList AS CHAR):
    PUBLISH "DoResizeSetting" FROM hSourceProc (ENTRY(1,PROGRAM-NAME(1)," "),ihFrame,icWidgetNameList).
  END METHOD.
  METHOD PUBLIC VOID setAddMoveX(INPUT ihFrame AS HANDLE
                               ,INPUT icWidgetNameList AS CHAR):
    PUBLISH "DoResizeSetting" FROM hSourceProc (ENTRY(1,PROGRAM-NAME(1)," "),ihFrame,icWidgetNameList).
  END METHOD.
  METHOD PUBLIC VOID setAddMoveY(INPUT ihFrame AS HANDLE
                               ,INPUT icWidgetNameList AS CHAR):
    PUBLISH "DoResizeSetting" FROM hSourceProc (ENTRY(1,PROGRAM-NAME(1)," "),ihFrame,icWidgetNameList).
  END METHOD.
  */  
                             
END CLASS.
  