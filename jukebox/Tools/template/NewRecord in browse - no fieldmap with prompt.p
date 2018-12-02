/* Either simple dynamic prompt (abhack: afv) */
RUN JBoxAskForValue.w ("<message>","<datatype>|<format>|<opt:init.val>",INPUT-OUTPUT <char_var>,OUTPUT iReturn).
IF iReturn NE 2 THEN RETURN.

/* Or select from a list (abhack: sis) */
RUN JBoxDSimpleSelectList.w ("<desc>|<value>|<desc>|<value>..",<opt posWidgetHld>,OUTPUT cReturn).
IF cReturn = ? THEN RETURN.

/* Or your own prompt.. */

/* Then create the record: */
RUN SUPER.

/* And update it with your prompted value(s) before you refresh the row */

/* To invoke a postUpdate procedure for this update:
DYNAMIC-FUNCTION("setPostUpdProc","mypostupdateproc.p").
*/

IF DYNAMIC-FUNCTION("DoUpdate",
	<QueryObject>:BUFFER-HANDLE:NAME,
	"Ignore",/* no (dynamic) validation. Could also be f.ex "=mycustomvalproc.p" */
	"", /* no key field - use the RowIdent1 */
	<QueryObject>:BUFFER-HANDLE::RowIdent1,
	"field1,field2,..",
	<value1>|<value2>|..,  
	YES) /* Commit */ THEN
  DYNAMIC-FUNCTION("RefreshRowids",<QueryObject>:BROWSE-HANDLE,<QueryObject>:BUFFER-HANDLE::RowIdent1).
