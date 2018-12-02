/* Helper proc that subscribes to procedures so that objects within the viewer may be accessible to the container
   Created: 26.aug.2015 by brynjar@chemistry.no
*****************************************************************************************************/   

DEF INPUT PARAM ioTabs AS JBoxMsTabs NO-UNDO.

SUBSCRIBE TO "ResizeTabWindows" ANYWHERE. /* published from ResizeFormComponents in JBoxWrapWindowInForm */
SUBSCRIBE TO "setChildBrowseObject" ANYWHERE. /* published from browse class */
SUBSCRIBE TO "setChildQueryObject" ANYWHERE. /* published from query class */
SUBSCRIBE TO "setChildToolbarObject" ANYWHERE. /* published from toolbar class */
SUBSCRIBE TO "setPageQueryHandle" ANYWHERE. /* published from objlib.NewBrowse when dynamic browse */

SUBSCRIBE TO "EmbedInChildForm" ANYWHERE.

PROCEDURE ResizeTabWindows:
/*------------------------------------------------------------------------------
 Purpose: receive message from browse in tab-program to be assigned to the tab folder class 
 Notes:
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihPlaceHolder   AS HANDLE NO-UNDO.
DEF INPUT PARAM iiX             AS INT NO-UNDO.
DEF INPUT PARAM iiY             AS INT NO-UNDO.
DEF INPUT PARAM iiDeltaX        AS INT NO-UNDO.
DEF INPUT PARAM iiDeltaY        AS INT NO-UNDO.

IF ioTabs:PLACEHOLDER = ihPlaceHolder THEN 
  ioTabs:resizeTabWindows(iiX,iiY,iiDeltaX,iiDeltaY).


END PROCEDURE.

PROCEDURE setChildBrowseObject:
/*------------------------------------------------------------------------------
 Purpose: receive message from browse in tab-program to be assigned to the tab folder class 
 Notes:
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihTabSourceProc AS HANDLE NO-UNDO.
DEF INPUT PARAM ioBrowseObject  AS JBoxBrowse NO-UNDO.

DEF VAR iTab# AS INT NO-UNDO.

iTab# = ioTabs:getPageTabNum(ihTabSourceProc).

IF iTab# > 0 THEN
  ioTabs:setChildBrowseObjectIfVoid(ioBrowseObject,iTab#).

END PROCEDURE.

	
PROCEDURE setChildQueryObject:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihTabSourceProc AS HANDLE NO-UNDO.
DEF INPUT PARAM ioQueryObject   AS JBoxQuery NO-UNDO.

DEF VAR iTab# AS INT NO-UNDO.

iTab# = ioTabs:getPageTabNum(ihTabSourceProc).

IF iTab# > 0 THEN
  ioTabs:setChildQueryObjectIfVoid(ioQueryObject,iTab#).

END PROCEDURE.

PROCEDURE setChildToolbarObject:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihTabSourceProc AS HANDLE NO-UNDO.
DEF INPUT PARAM ioToolbarObject AS JBoxToolbar NO-UNDO.

DEF VAR iTab# AS INT NO-UNDO.

iTab# = ioTabs:getPageTabNum(ihTabSourceProc).

IF iTab# > 0 THEN
  ioTabs:setChildToolbarObjectIfVoid(ioToolbarObject,iTab#).


END PROCEDURE.

PROCEDURE setPageQueryHandle:
DEF INPUT PARAM ihPageQuery AS HANDLE NO-UNDO.

ioTabs:setPageQueryIfVoid(ihPageQuery).

END PROCEDURE.


PROCEDURE EmbedInChildForm:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEF INPUT  PARAM ihProc     AS HANDLE NO-UNDO.
DEF OUTPUT PARAM obEmbed    AS LOG    NO-UNDO.

IF PROGRAM-NAME(4) BEGINS "AddPage " THEN DO:
  obEmbed = IF ioTabs:FormIsChildForm THEN YES ELSE ?.
  ioTabs:PublishForm = YES.
END.

END PROCEDURE.
