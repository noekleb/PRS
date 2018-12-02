/* Assign these columns automatically on create (if they exist in table)
   Place this include after session validation since some values might come from there.. 
   Columns should be comma-separated. Values should be | sep. 
   Todays date should be put in as TODAY in the list 
   
   01.04.05: Changed to a procedure call to avoid multiple r-code versions of jbserv_servertrans.p 
*/
      
DEF VAR cCreateAutoAssign AS CHAR NO-UNDO INIT "cCreatedBy,dCreated".  
DEF VAR cCreateValues     AS CHAR NO-UNDO.

IF SEARCH("jbserv_getautoassigncrevalues.r") NE ? OR SEARCH("jbserv_getautoassigncrevalues.p") NE ? THEN
  RUN jbserv_getautoassigncrevalues.p (INPUT  icSessionId,
                                       OUTPUT cCreateAutoAssign, 
                                       OUTPUT cCreateValues).
ELSE
  cCreateValues = cCreateValues + "|TODAY".
