/* USING System.Windows.Forms.*. */
using Progress.Windows.*.
/* using workspace.Sports2000.*.              */
using Infragistics.Win.UltraWinToolbars.*.
/* using workspace.Sports2000.*.  */

RUN jboxloadlib.p ("ResizeLib.p,JBoxUiLib.p,JBoxAsLib.p,JBoxFuLib.p,controls.p").
DYNAMIC-FUNCTION("AddColor","RowShade",240,240,240).
DYNAMIC-FUNCTION("AddColor","FrameBg",206,224,255).
DYNAMIC-FUNCTION("AddColor","FillInBg",198,202,255).


/* {menuitemvalues.i}  */
def var goJBoxMainForm as JBoxMainForm no-undo.

goJBoxMainForm = new JBoxMainForm(this-procedure).  
goJBoxMainForm:ClientSize = NEW System.Drawing.Size(1024,768).
goJBoxMainForm:setWindowTitle("JukeBox demo app for Sports2000").
goJBoxMainForm:setBgImage("bg-green.bmp").
goJBoxMainForm:setIconImage("RUN.bmp").
goJBoxMainForm:setStatusText("Brynjar").

DEF TEMP-TABLE ttRibbon
    FIELD oRibbonObj   AS CLASS Progress.Lang.Object
    FIELD iNodeIndex   AS INT
    FIELD cObjectType  AS CHAR
    FIELD cStartProg   AS CHAR
    FIELD hProc        AS HANDLE
    FIELD iFormTag     AS INT
    .

/* DEF VAR vRibbonTab   AS CLASS Infragistics.Win.UltraWinToolbars.RibbonTab NO-UNDO.             */
/* DEF VAR vRibbonGroup AS CLASS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.           */
/* DEF VAR vRibbonTool  AS CLASS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.            */
/* DEF VAR vRibbonPopup AS CLASS Infragistics.Win.UltraWinToolbars.PopupMenuTool NO-UNDO.         */
/* DEF VAR vRibbon      AS CLASS Infragistics.Win.UltraWinToolbars.UltraToolbarsManager NO-UNDO.  */
DEF VAR vObject      AS CLASS Progress.Lang.Object NO-UNDO.
DEF VAR cObjectType  AS CHAR  NO-UNDO.
DEF VAR vToolbar     AS CLASS Infragistics.Win.UltraWinToolbars.UltraToolbarsManager.

/* vToolbar = goJBoxMainForm:getToolbarHandle(). */
/* vRibbon:LoadFromXml("c:\temp\testform.xml").  */
CREATE ttRibbon.
ASSIGN ttRibbon.iNodeIndex = 100     
       ttRibbon.oRibbonObj = goJBoxMainForm:getToolbarHandle(OUTPUT ttRibbon.cObjectType)
       vObject = ttRibbon.oRibbonObj
       cObjectType = ttRibbon.cObjectType.

CREATE ttRibbon.
ASSIGN ttRibbon.iNodeIndex = 101
       ttRibbon.oRibbonObj = goJBoxMainForm:AddRibbonButtonTool(vObject,cObjectType,ttRibbon.iNodeIndex,"HovedKnapp 1","ind-s.bmp",YES,OUTPUT ttRibbon.cObjectType)
       ttRibbon.cStartProg = "OrderSchedulePrSalesRep.w"
       .

CREATE ttRibbon.
ASSIGN ttRibbon.iNodeIndex = 102
       ttRibbon.oRibbonObj = goJBoxMainForm:AddRibbonPopupMenuTool(vObject,cObjectType,ttRibbon.iNodeIndex,"HovedPopup 1","redtick.bmp",YES,OUTPUT ttRibbon.cObjectType)
       vObject = ttRibbon.oRibbonObj
       cObjectType = ttRibbon.cObjectType
       .

CREATE ttRibbon.
ASSIGN ttRibbon.iNodeIndex = 103
       ttRibbon.oRibbonObj = goJBoxMainForm:AddRibbonPopupMenuTool(vObject,cObjectType,ttRibbon.iNodeIndex,"UnderPopup 1","",NO,OUTPUT ttRibbon.cObjectType)
       vObject = ttRibbon.oRibbonObj
       cObjectType = ttRibbon.cObjectType
       .

CREATE ttRibbon.
ASSIGN ttRibbon.iNodeIndex = 1     
       ttRibbon.oRibbonObj = goJBoxMainForm:AddRibbonTab(ttRibbon.iNodeIndex,"tab1",OUTPUT ttRibbon.cObjectType)
       vObject = ttRibbon.oRibbonObj
       cObjectType = ttRibbon.cObjectType.

CREATE ttRibbon.
ASSIGN ttRibbon.iNodeIndex = 2     
       ttRibbon.oRibbonObj = goJBoxMainForm:AddRibbonGroup(vObject,cObjectType,ttRibbon.iNodeIndex,"Group 1",OUTPUT ttRibbon.cObjectType)
       vObject = ttRibbon.oRibbonObj
       cObjectType = ttRibbon.cObjectType.

CREATE ttRibbon.
ASSIGN ttRibbon.iNodeIndex = 3
       ttRibbon.oRibbonObj = goJBoxMainForm:AddRibbonButtonTool(vObject,cObjectType,ttRibbon.iNodeIndex,"Knapp 1","ind-s.bmp",YES,OUTPUT ttRibbon.cObjectType)
       ttRibbon.cStartProg = "OrderSchedulePrSalesRep.w"
       .

CREATE ttRibbon.
ASSIGN ttRibbon.iNodeIndex = 4
       ttRibbon.oRibbonObj = goJBoxMainForm:AddRibbonPopupMenuTool(vObject,cObjectType,ttRibbon.iNodeIndex,"Popup 1","redtick.bmp",YES,OUTPUT ttRibbon.cObjectType)
       vObject = ttRibbon.oRibbonObj
       cObjectType = ttRibbon.cObjectType.

CREATE ttRibbon.
ASSIGN ttRibbon.iNodeIndex = 5 
       ttRibbon.oRibbonObj = goJBoxMainForm:AddRibbonButtonTool(vObject,cObjectType,ttRibbon.iNodeIndex,"Knapp 2","tbls.bmp",YES,OUTPUT ttRibbon.cObjectType)
       ttRibbon.cStartProg = "CustOrder.w"
       .

CREATE ttRibbon.
ASSIGN ttRibbon.iNodeIndex = 6
       ttRibbon.oRibbonObj = goJBoxMainForm:AddRibbonButtonTool(vObject,cObjectType,ttRibbon.iNodeIndex,"Knapp 3","",YES,OUTPUT ttRibbon.cObjectType)
       .

CREATE ttRibbon.
ASSIGN ttRibbon.iNodeIndex = 7
       ttRibbon.oRibbonObj = goJBoxMainForm:AddRibbonPopupMenuTool(vObject,cObjectType,ttRibbon.iNodeIndex,"Popup 2","",YES,OUTPUT ttRibbon.cObjectType)
       vObject = ttRibbon.oRibbonObj
       cObjectType = ttRibbon.cObjectType.

CREATE ttRibbon.
ASSIGN ttRibbon.iNodeIndex = 8
       ttRibbon.oRibbonObj = goJBoxMainForm:AddRibbonTab(ttRibbon.iNodeIndex,"Tab 2",OUTPUT ttRibbon.cObjectType)
       vObject = ttRibbon.oRibbonObj.


/* goJBoxMainForm:DisableMenuItem(201).  */

goJBoxMainForm:Show().

/* goJBoxMainForm:SetTabOrientation(705).  */

/* Use the .Net event blocking loop if the CLR is loaded. */
/* Need some dummy window for this */
def var oForm as Progress.Windows.Form.
oForm = new Progress.Windows.Form().

WAIT-FOR System.Windows.Forms.Application:Run().


PROCEDURE EventHandler:
  /*------------------------------------------------------------------------------
      Purpose: Handles all events from the form  																	  
      Notes:  																	  
    ------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER pcEventName AS CHARACTER   NO-UNDO.
  DEFINE INPUT PARAMETER poSender AS System.Object  NO-UNDO.
  DEFINE INPUT PARAMETER poArgs AS System.EventArgs NO-UNDO.
  
  def var cRetVal as character no-undo.

  CASE pcEventName:
      /* pass menu item stuff through */
      WHEN 'ToolClick' THEN DO:
        IF TYPE-OF(poSender, UltraToolbarsManager) THEN 
          RUN StartWindow (cast(poArgs, ToolClickEventArgs):Tool:KEY).
      END.    
      WHEN 'FormClosed' THEN 
          QUIT.
      WHEN 'FormClosing' THEN 
        RUN CloseWindow (INT(STRING(CAST(poSender, Form):Tag))).
  END CASE.

  return cRetVal.
end procedure.

PROCEDURE StartWindow:
  DEF INPUT PARAM icNodeKey      AS CHAR                    NO-UNDO.

  DEFINE VARIABLE hProc         AS HANDLE                  NO-UNDO.
  DEFINE VARIABLE hStatusFrame  AS HANDLE                  NO-UNDO.
  DEFINE VARIABLE hWin          AS HANDLE                  NO-UNDO.
  DEFINE VARIABLE hWinFrame     AS handle                  NO-UNDO.
  DEFINE VARIABLE iDesignHeight AS INTEGER                 NO-UNDO.
  DEFINE VARIABLE iDesignWidth  AS INTEGER                 NO-UNDO.
  DEFINE VARIABLE oChildForm    AS Progress.Windows.Form   NO-UNDO.

  FIND FIRST ttRibbon 
       WHERE ttRibbon.iNodeIndex = INT(icNodeKey)
       NO-ERROR.
  IF NOT AVAIL ttRibbon THEN RETURN.

  RUN VALUE(ttRibbon.cStartProg) PERSISTENT SET ttRibbon.hProc.

  hWin = ttRibbon.hProc:CURRENT-WINDOW.
  
  oChildForm = NEW MDIChildForm(goJBoxMainForm, hWin).
  
  oChildForm:Tag = ttRibbon.iNodeIndex.
/*   oChildForm:Icon = goJBoxMainForm:Icon. */
  oChildForm:Text = hWin:Title .
  
  oChildForm:FormClosing:Subscribe(goJBoxMainForm:OnMdiChildFormClosing).
  oChildForm:Show().
                                                 
  ASSIGN iDesignWidth  = hWin:WIDTH-PIXELS 
         iDesignHeight = hWin:HEIGHT-PIXELS   
         .

  RUN InitializeObject IN ttRibbon.hProc NO-ERROR.

  /* One way to detect that resize setting are in place: */
  IF DYNAMIC-FUNCTION("getWinMinXmove",hWin,"X") > 0 THEN DO:
    hWinFrame = hWin:FIRST-CHILD.
    REPEAT while valid-handle(hWinFrame):
      IF hWinFrame:name = "StatusFrame" THEN
        hStatusFrame = hWinFrame.
      hWinFrame = hWinFrame:NEXT-SIBLING.
    END.

    /* The window is automatically sized to fit the available space so to get a delta for the frames to grow or shrink we need to re-assign design-size to current */
    DYNAMIC-FUNCTION("setCurrWidthHeight",hWin,iDesignWidth - 3,iDesignHeight + (IF VALID-HANDLE(hStatusFrame) THEN hStatusFrame:HEIGHT-PIXELS ELSE 0)).
    
    /* The status-bar frame is created after the window was resized in MDIChildForm and therefore it shouldn't be touched this one time: */
    DYNAMIC-FUNCTION("setOneTimeException",hStatusFrame).

    APPLY "WINDOW-RESIZED" TO hWin.
  END.

  RUN MoveToTop IN ttRibbon.hProc NO-ERROR.

END PROCEDURE.


PROCEDURE CloseWindow:
  DEF INPUT PARAM iiNodeKey AS INT NO-UNDO.

  FIND FIRST ttRibbon 
       WHERE ttRibbon.iNodeIndex = iiNodeKey
       NO-ERROR.
  IF AVAIL ttRibbon AND VALID-HANDLE(ttRibbon.hProc) THEN 
    APPLY "CLOSE" TO ttRibbon.hProc.

END PROCEDURE.

procedure FormFactory :
/*------------------------------------------------------------------------------
  Purpose:     Factory procedure for creating AUI forms. 
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    define input  parameter pcFormName  as character no-undo.
    define output parameter poForm      as Form no-undo.
    
    poForm = dynamic-new(pcFormName) (this-procedure).
    
    return.
end.
