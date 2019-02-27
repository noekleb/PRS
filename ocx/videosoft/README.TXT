===================================================================================
VSFlexGrid Pro Build  7.0.0.67		
===================================================================================
==================================================
DOCUMENTATION CHANGES (CUMULATIVE)     20 Jul 2000
==================================================

Modify the BeforeEdit and StartEdit events to show the editing event sequence below:
     KeyDown  65         ' user pressed the 'a' key (65 = 'A')
     KeyPress  97        ' *** new
     BeforeEdit 1 1      ' allows you to cancel the editing
     StartEdit 1 1       ' not canceled, edit is about to start
     KeyPressEdit  97    ' 'a' key passed to editor (97 = 'a')
     KeyUpEdit  65       ' user released the 'A' key
     KeyDownEdit  13     ' user pressed Enter
     KeyPressEdit  13    ' *** new
     ValidateEdit        ' allows you to validate the edit
     AfterEdit           ' editing done
     BeforeEdit          ' repainting cell
     KeyUp  13           ' user released Enter key

Add the following to the OLEDragMode property:
      In OleDragAutomatic mode, we allow only copying.
      To allow move/copy, trap OLEStartDrag and set AllowedEffects as in:
      //
	  // Private Sub flxTemp_OLEStartDrag(Data As VSFlex7LCtl.VSDataObject, AllowedEffects As Long)
	  //    AllowedEffects = vbDropEffectMove + vbDropEffectCopy
	  // End Sub
      //

Add the following to the OLEDragOver Event:

The grid will scroll automatically while OLE dropping. The 
scrolling happens when you hold the drop cursor near the edges 
of the grid for a short time. Scrolling is provided 
automatically when OLEDropMode is set to flexOLEDropAutomatic (2),
and not when OLEDropMode is set to flexOLEDropManual (1). You 
can enable scrolling in manual mode (or prevent it in automatic 
mode) by changing the Effect parameter in the OLEDragOver 
event. For example:

    Private Sub VSFlexGrid1_OLEDragOver(Data, Effect, Button, Shift, X, Y, State)

       If g_bEnableOLEDropScrolling Then
          Effect = Effect Or vbDropEffectScroll
       Else
          Effect = Effect And (Not vbDropEffectScroll)
       End If

    End Sub

The following items are not reflected in the printed 
documentation. Please see the Help file for details on these 
items.

There is a new GetNode method that returns a VSFlexNode object. 
The VSFlexNode object corresponds to an outline row and has 
methods for adding, removing, locating, moving, sorting, and 
changing outline nodes.

The Cell property has a new flexcpSort setting that allows you 
to sort a range without changing the selection.

The new BookMark(Row) property returns a row's bookmark when 
the grid is bound to an ADO data source.

Translated combos now use strings instead of numbers for keys.

The ComboData property is now a string (used to be a long).

The Appearance property has a new setting: flex3DLight creates 
a thin 3-D border around the control.

CellBorder now supports double lines: use width values >= 100 
to get double lines.

Node.Move flexNMChildOf, nd makes a node a child of another node

MousePointer has a few new cursor options (OLEMove, OLECopy, 
Hand)

Tri-State check boxes (grayed) are now supported (CellChecked 
has new settings)

The flexcpCustomFormat setting of the Cell property has a new 
capability: set it to a string containing a cell reference 
(e.g. "200|5") to copy the given cell's format (everything: 
font, color, alignment, picture, borders, etc).

New ColIndent property allows you to specify an indentation for 
each column (in twips).

================================================== 
CORRECTED PROBLEMS                     20 Jul 2000 
================================================== 

Years before 1900 formatted as yy didn't display correctly

Could set OutlineCol to bad value sometimes

KeyDownEdit firing on EditCell for combos with custom entries

Crash with MergeCells=flexMergeRestrictAll all rows/columns merged.

FlexOutlineSubtotals assumed there were hidden rows, showed + symbol

Clear data object on OleDragLeave

No dialog when ShowMsgBox set to True in the Error event.

Firing BeforeDataRefresh and AfterDataRefresh when clearing the DataSource

Safer code for binding to > 64k array cells.

Made mouse handling synchronous (receives messages while processing)

Editing w/EditMask, then dbl-clicking a different control kept placeholders

Typing invalid character makes listbox select first item

Partial matches in combos overriding previous contents

Added sanity check on illegal default column on ComboList

Cleaned up combo search behavior.

ShowMsgBox did not work in the Error event

ComboList/ColComboList throw error when assigned invalid lists

Honor Cancel param in BeforeDataRefresh also when bound

Preserve FrozenCols with data refresh

Improved picture rendering/clipping

AutoSize row heights didn't work well with cell pictures

Made AutoSearch work on empty cells.

Improved cell merging with check boxes

Frozen col marker was bad with RightToLeft = True

Keep initial custom strings in drop-down combos

Improved combo search (cycling)

Improved interpreting ColImagelist index (use doubles)

Update selection before changing check box when ADO-bound.

Improved mask truncation when content longer than mask

Prevent resize/freeze when all rows/cols hidden

Reset scroll range after clearing subtotals

Clear method must reset merging state

Checkboxes and node symbols were overlapping

AfterMoveRow event was not fireing when dragging to same position.

Various paint optimizations

Ignore col data type when aligning translated columns

Honor Case/FullMatch in FindRow when looking for Variants

Clear subtotals on DataRefresh

FormatString not honored properly when data bound (blank headers)

When HighLight is set to never, setting cell properties failed to invalidate properly

TopRow/LeftCol not honored with no scrollbars

When set illegal settings to multi-column combo, the result is different.

The CellChanged is fired unnecessarily (when setting empty strings).

IE shuts down when run when LoadArray without dimensioning array.

Vertical scrollbar should not display (with lots of frozen rows)

VB will report error when set Cancel=true in StartPage.

Extra safety in ADO data-binding, check error code in AddItem

Hidden Columns don't spill

Fixed memory leak in grdGetClip

Optimized buffered painting

Drag & drop run incorrectly when message box in OLEDragDrop event.

Highlight matches in AutoSearch with wrapping and center/right align didn't work well

CellBorder overflows to adjacent cells when width > 128 (negative)

The NewCol parameter in AfterSelChange event (should be NewColSel)

Problem rendering BackColor on paper

Cell border doesn't refresh correctly with underlined raised text

RowStatus is not reset to flexRSNew after Clear method

Wordwrap does not work with long string (with no spaces in it).

<Ctrl+C> can not copy in-cell text (in IE).

VB shuts down after set EditMask and ColComboList (Win2000 only).

Wrong cursor position when EditMask starts with literals ("\[99\]").

The print is short of two rows of the control.

Half of combo box button stays in fixed area (when cell is very narrow).

The <PageUp> does not take function in the frozen area.

AfterUserFreeze event should fire only after the user freezes with mouse

Subtotal merging with Frozen columns was broken

Clear item of subtotal method makes wrong node symbol.

Increase fixed row will make text overlap node symbol.

Fire KeyDown with enter when AutoSearching

No header/footer when doc name is null

Added clipping to paper rendering (needed for multi-line strings)

Collapsing a hidden node made it visible

Allow to change fixed columns when bound to custom data source

There will be a gap between the drop-down list and the combobox.

Set breakpoint to Keydown event causes program to freeze.

Focus rectangle does not go with selection in loaded grid.

AutoSearch highlights characters in opposite direction.

Mouse pointer should not display the default OLE drag cursor.

Highlight looks incorrect for the current selection

BackColor and BackColorAlternate need Refresh in 256 resolutions.

Unexpected thorns appear on (thick) gridlines.

The focus rectangle loses two corners

Dblclick on the outline tree plus button starts editing other cell

Scroll behaviour is funny after showing Msgbox in BeforeScroll or AfterScroll events.

When RowHeight less than text height, refresh error

Focus rectangle is incorrect after regaining focus.

Top in CellBorder will affect the right border width.

The Outline Column can be printed outside of the grid

A line displays when setting MergeRow

Need to reset merging when changing col types (to handle boolean cols)

Fixed problem with node object's MoveIn/MoveOut methods

SelChange not always firing with Select x,y method

Allow to set col data type, fixed rows when bound to custom data sources

CellChanged event does not fire when it should fire. (resetcolinfo in DAO)

Control will add one more column after call BuildComboList (fields with tabs)

A line displays when setting MergeRow

RowHeight property can clear data in outline bar. (clear needs to reset total info)

When setting flexExMoveRows, node can not be expanded. (with the mouse)

Smart ComboSearch for listbox gets wrong item.

Cursor disappears when navigating in the cell.

Fixed crash with DAO/Fox driver/255+ columns

Better handling of gridlines when editing

Can not input an underscore when EditMask on.

Wrong cursor location after clearing EditMask.

ShowMsgBox does not work in the Error event. (wrong error code)

Illegally merge special characters.

Can not show sort triangle when EditMask works (no text).

Focus rectangle does not go with selection in loaded grid. (invalid cursor after loading)

The control in the form was not same as control in Preview tab. (apply format after changing font)

Check validation should be run (on OutlineCol).

Smart ComboSearch for listbox gets wrong item. (when AutoSearch fails)

No response when Sorting the nodes.

Mouse moving cause the control to flicker

When setting RightToLeft=True, the combo spin button behavior is strange.

VB shuts down when dragging DBCS. (use alloc for drag-drop instead of W2T, free clip string)

AutoSearch is case-sensitive with DBCS alphabet. (use vsStrCmp instead of lib funs)

When typeing in the drop-down-list cell, VB will be frozen.

No response when Sorting the nodes. (freeze when calling nd.Sort <badSetting>)

OLEDrag & Drop acts funny ... (when cells contain tabs/returns).

Made VSDataObject expose IDataObject interface 

Support right to left in combo boxes

Honor custom row delimiters when saving/loading text

Various DBCS fixes

OLE drag/drop with DBCS 

DBCS combo search is incorrect.

Set font to default charset to work with Japanese fonts

Fixed SimpleFrame to work with FoxPro

Fixed new OleDragDrop to drag raw text instead of formatted

Fixed new OleDragDrop so IEnumFormatEtc returns files too

No automatic checkboxes for subtotal columns

Fixed new OleDragDrop typo (was zapping the original data object)

Back to IEnumFE.h to remove dependency on URLMON.DLL

Fire BeforeMouseDown on editors so user can provide a custom context menu. For example:
   Private Sub fg_BeforeMouseDown(...)
      If Button = vbRightButton And fg.EditWindow <> 0 Then
         PopupMenu mCustomPopup
         Cancel = True
      End If
   End Sub

Cell(flexcpPicture) now accepts a url string

DataObject returns pointer to foreign object COM_INTERFACE_ENTRY_AGGREGATE

Update sort glyphs when code sorting as well as clicking the ExplorerBar
 
Fixed crash with combo lists in Unicode

Getting ComboIndex with no combo should return -1, no error.

Re-bind VSFlexDataSource if GetData raises an error

Avoid loop when calling Select from AfterEdit

Accept OutlineCol = anything when Cols = 0 

Don't apply Format on FontChanged at run time, it's called by OnSize 

ChangeEdit doesn't fire when list selection is changed with the mouse

Can't remove checkboxes from fixed cells in boolean columns

Can't align checkboxes in fixed cells in boolean columns

==================================================
End of entries for build 7.0.0.67      20 Jul 2000
==================================================


===================================================================================
VSFlexGrid Pro Build  7.0.0.41		
===================================================================================

================================================== 
CORRECTED PROBLEMS                     13 Apr 2000 
================================================== 
Now has different ProgIDs for each flavor (DAO, ADO, etc.) of 
the control

ADO grid now works with Apex data control.

Improved keyboard events, we now fire KeyPress after KeyDown in 
all cases (see DOCUMENTATION CHANGES above)

Improved mouse capture so control works OK with SetCapture API

Improved formatting with leading zeros for VB-compatibility

Made SaveGrid/LoadGrid handle CellData (all simple types 
including strings)

Fixed OLE drag drop bug that prevented dropping

Added automatic scrolling while OLE dropping (see DOCUMENTATION 
CHANGES above) 

Fixed bug when deleting records in DAO recordsource

Fixed bug in VSDataObject (reporting wrong number of files)

BuildComboList was ignoring non-string values. Now it accepts 
them.

Improved ADO RowFromCursor to work with large recordsets 
(million rec range)

Fixed problem when ADO source recordset was closed and we were 
bound

Made AddItem/RemoveItem honor ADO batch DataMode/LockType

Added support for multiline scrolltips (include \n or vbCrLf in 
the text)

Added a new control flavor: UNICODE/Light (VSFlex7N.ocx)

Made BuildComboList faster (especially on large recordsets)

Made MouseRow/MouseCol return proper cell within merged ranges

Take ColDataType into account when aligning cells (general 
alignment)

Fixed problem when editing a grid that had no rows/cols using 
the property page

Fixed problem with translated combo lists

Fixed DBCS problem when going into edit mode 

Made spill/merge account for size of pictures in cells

Changed export to text: now saving formatted cell contents 
(good for translated lists)

Cleaned up Cell(flexcpPicture) for consistency with 
CellPicture: returns Nothing if no picture (instead of Emtpy), 
may assign Empty or Nothing to clear

SelChange was firing twice on big selections/Select method

Make right-aligned numbers with parentheses line up.

Handle drop-down clicks when cell is truncated

Writing empty strings to DB failed for non-string fields

No change to Row property after setting Rows property (used to 
avoid fixed area)

Fixed problems with adding/deleting columns when bound to a 
data source

Fixed ChangeEdit problem (sometimes wouldn't fire)

Changed vsAtoF to honor percentage signs (e.g. vsAtoF("20%") = 
0.2)

EditChanged was firing twice when overtyping a selection. 

Fixed problem with autosize and very narrow grids

RemoveNode deleted two nodes when the node had no children

Setting Frozen* then Fixed* screwed up mouse selection 

DBStringToVariant did not take column type into account (was 
converting "1e2" into 100).

Changed automatic drag to perform/allow copy by default. To 
provide copy/move drag drop, you need to trap the OleStartDrag 
event and set the AllowedEffects parameter to vbDropEffectCopy 
Or vbDropEffectMove.

Move sorting glyphs when moving columns

No ctrl-click when AllowSelection is false

Cell merging sometimes failed when data-bound

Fire StartEdit with checkboxes/keyboard

Dropping data from Excel overwrites the last cell on the grid

Combos not painting properly in VB4

Finish pending edits when user clicks the blank area

ShowCell wasn't working without scrollbars

Crash when binding DAO to empty ODBC recordset

Header text not always loaded from recordset

Added code to deal with resize controls (e.g. ElasticLight)

Changing BackColorSel was not refreshing properly in ListBox 
mode

Cell(flexcpSort) was not refreshing the grid after sorting

Freezing with mouse when Frozen/Fixed/Rows/Cols = 0 was bad

OLE drag-drop now allows custom clipboard formats

Fixed DB binding bug when adding columns

TranslateAccelerator now ignores Ctrl+Tab (to work with MDI 
forms)

PrintForm now works

Allow auto wrap of text in edit window

Optimized setting grid dimensions when binding to non-DB sources

Date formatting was off for 12:00:00 AM

Roundoff problem: rounding 10.555 into "10.55" (instead of 
"10.56")

Improved ComboList handling of invalid characters

Prevented DBCS in ClipSeparators

Fixed RightToLeft column freezing with the mouse

Improved SaveGrid/LoadGrid to persist row/col data

Fixed MousePointer problem (99, no icon)

Fire OLESetData

Improved logic when adding/removing rows in free mode

EditSelText was returning one extra character at the end

Added support for mouse wheel

AutoSize row heights was not accounting for outline tree

CellChange event was firing twice for custom data sources, and 
it was also firing when the same value was re-assigned to a cell

AutoSize row heights was not accounting for cell pictures

ColKey was not initialized properly

Added support for pictures in our OLEDragDrop Data object. For 
sample code that demonstrates this, go to the VB samples page 
for VSFlexGrid 7.0 on our website: 
http://vs.videosoft.com/vsflexgrid/support/samples/vb.asp

==================================================
End of entries for build 7.0.0.41      13 Apr 2000
==================================================


===================================================================================
VSFlexGrid Pro Build  7.0.0.12		
===================================================================================
================================================== 
CORRECTED PROBLEMS                     13 Dec 1999 
================================================== 
ColFormat now recognizes leading zeros (e.g. "0##" formats "2" 
as "002")

Sort glyphs now change to reflect setting of the Sort property

We fire KeyPressEdit on <Return> and <Esc>

Reset LeftCol/TopRow when setting FrozenCols/FrozenRows

Finish edits before clicks on checkboxes (better event sequence)

Fixed coordinates in OLEDrag events (use client, twips)

No redraw while loading long text files

Improved EditText to return next setting of CheckBoxes

Improved ComboList/ColComboList to display Chr(11) as pipes

Improved response to system color change when control is a 
child of another control

Forced ScrollTips window to top so it's visible over topmost 
windows in NT

Expand rows before deleting them (good when clearing subtotals)

Refresh when setting ColComboLists (to show translated values)

PrintGrid was not working properly with FrozenCols

Now able to dag/drop columns into the first column of the grid

Added different ProgIDs for each flavor of the control: 
VSFlexGrid.VSFlexGridADO, VSFlexGrid.VSFlexGridDAO, 
SFlexGrid.VSFlexGridL, VSFlexGrid.VSFlexGridU. Previously all 
four controls had the ID VSFlexGrid.VSFlexGrid. This caused a 
problem when using the Add method of the Form.Controls 
collection.

==================================================
End of entries for build 7.0.0.12      13 Dec 1999
==================================================


===================================================================================
Initial build 7.0.0	
===================================================================================
Note: The initial release of VSFlexGrid 7.0 is not binarily 
compatible with some previous Beta versions. The GetOutlineNode 
property was deleted and reincarnated as the GetNodeRow method 
for better consistency with the new GetNode method. The data 
type of the ComboData property was also changed.

We refrain from making such changes to a released product, and 
apologize for any inconvenience caused to our testers by doing 
this during the beta cycle.

==================================================
UPGRADE FROM VERSION 6 TO VERSION 7    19 Nov 1999
==================================================
You can upgrade projects that use VSFlexGrid from version 6.0 
to version 7.0 with the Convert utility which is installed in 
C:\Vs\vsFlex7\Convert.  Simply run the utility and point it to 
your project. The utility will make backup copies of your 
project. This utility is provided in source-code form, and 
distributed "as-is." It will also upgrade projects that use 
MSFlexGrid.

==================================================
CORRECTED PROBLEMS                     19 Nov 1999
==================================================
Not applicable for the initial build.

==================================================
End of entries for the initial release 19 Nov 1999
==================================================

