/* To send a constant as extra parameter from the client append the parameter value
   to the constant "ROWID":   
   
   For Browse or Query:
     "+<FieldName>|<datatype>|<format>|<procname.p(ROWID<parameter>)>|<label>"
   For field-list:
     "+<FieldName>|<procname.p(ROWID<parameter>)>"   
   
   For a browse you can also set the parameter dynamically by using an attribute:
     (the field def would then be just
       "+<FieldName>|<datatype>|<format>|<procname.p(ROWID)>|<label>"
      ). The attribute should be set in the OpenQuery procedure (before RUN SUPER):
      DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcparam<FieldName>",<parameter>).
      
  To discard the row (based on the findings here), assign ocReturn = "skiprow"
*/   
DEF INPUT PARAM  irBuffer      AS ROWID NO-UNDO.
DEF INPUT PARAM  icParam       AS CHAR  NO-UNDO.
DEF INPUT PARAM  icSessionId   AS CHAR  NO-UNDO.
DEF OUTPUT PARAM ocReturn      AS CHAR  NO-UNDO.

FIND Customer WHERE ROWID(Customer) = irBuffer NO-LOCK NO-ERROR.
IF AVAIL Customer THEN
  ...
