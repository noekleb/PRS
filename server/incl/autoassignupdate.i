
/* Assign these columns automatically on update (if they exist in table)
   Place this include after session validation since some values might come from there.. 
   Columns should be comma-separated. Values should be | sep.
   Todays date should be put in as TODAY in the list */
   
   
DEF VAR cUpdateAutoAssign AS CHAR NO-UNDO INIT "cModifiedBy,dModified,EDato". 
DEF VAR cUpdateValues     AS CHAR NO-UNDO.
IF AVAIL JBoxLoginSession THEN
  cUpdateValues = JBoxLoginSession.cJBoxUserId.
ELSE
  cUpdateValues = "Unknown".
cUpdateValues = cUpdateValues + "|TODAY|TODAY".
