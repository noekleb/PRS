/* Assign these columns automatically on update (if they exist in table)
   Place this include after session validation since some values might come from there.. 
   Columns should be comma-separated. Values should be | sep.
   Todays date should be put in as TODAY in the list
   
   01.04.05: Changed to a procedure call to avoid multiple r-code versions of jbserv_servertrans.p 
   */
   
   
DEF VAR cUpdateAutoAssign AS CHAR NO-UNDO INIT "cModifiedBy,dModified". 
DEF VAR cUpdateValues     AS CHAR NO-UNDO.

IF SEARCH("jbserv_getautoassignupdvalues.r") NE ? OR SEARCH("jbserv_getautoassignupdvalues.p") NE ? THEN
  RUN jbserv_getautoassignupdvalues.p (INPUT  icSessionId,
                                       OUTPUT cUpdateAutoAssign, 
                                       OUTPUT cUpdateValues).
ELSE
  cUpdateValues = cUpdateValues + "|TODAY".
