
/* Assign these columns automatically on create (if they exist in table)
   Place this include after session validation since some values might come from there.. 
   Columns should be comma-separated. Values should be | sep. 
   Todays date should be put in as TODAY in the list */
   
   
DEF VAR cCreateAutoAssign AS CHAR NO-UNDO INIT "cCreatedBy,dCreated,RegistrertAv". 
DEF VAR cCreateValues     AS CHAR NO-UNDO.
IF AVAIL JBoxLoginSession THEN
  cCreateValues = JBoxLoginSession.cJBoxUserId + "|TODAY|" + JBoxLoginSession.cJBoxUserId.
ELSE
  cCreateValues = USERID(LDBNAME(1)) + "|TODAY|" + USERID(LDBNAME(1)).
