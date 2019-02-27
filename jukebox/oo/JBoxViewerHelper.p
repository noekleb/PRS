/* Helper proc that subscribes to procedures so that objects within the viewer may be accessible to the container
   Created: 26.aug.2015 by brynjar@chemistry.no
*****************************************************************************************************/   

DEF INPUT PARAM ioViewer AS JBoxViewer NO-UNDO.

SUBSCRIBE TO "setChildBrowseObject" ANYWHERE. /* published from browse class */
SUBSCRIBE TO "setChildQueryObject" ANYWHERE. /* published from query class */
SUBSCRIBE TO "setChildToolbarObject" ANYWHERE. /* published from toolbar class */

PROCEDURE setChildBrowseObject:
/*------------------------------------------------------------------------------
 Purpose: receive message from browse in tab-program to be assigned to the tab folder class 
 Notes:
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihViewer        AS HANDLE NO-UNDO.
DEF INPUT PARAM ioBrowseObject  AS JBoxBrowse NO-UNDO.

IF ioViewer:VIEWER-HANDLE = ihViewer THEN
  ioViewer:setChildBrowseObjectIfVoid(ioBrowseObject).

END PROCEDURE.
	
PROCEDURE setChildQueryObject:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihViewer        AS HANDLE NO-UNDO.
DEF INPUT PARAM ioQueryObject   AS JBoxQuery NO-UNDO.

IF ioViewer:VIEWER-HANDLE = ihViewer THEN
  ioViewer:setChildQueryObjectIfVoid(ioQueryObject).


END PROCEDURE.

PROCEDURE setChildToolbarObject:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihViewer        AS HANDLE NO-UNDO.
DEF INPUT PARAM ioToolbarObject AS JBoxToolbar NO-UNDO.

IF ioViewer:VIEWER-HANDLE = ihViewer THEN
  ioViewer:setChildToolbarObjectIfVoid(ioToolbarObject).


END PROCEDURE.
