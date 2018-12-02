/* To invoke the parameter set an attribute for the browse field:

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcparam<fieldname>",<parameter>).
  F.ex like this:
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcparamOrderTotal",fillInSalesRep:SCREEN-VALUE + "¤" + fillInMinValue:SCREEN-VALUE).

  The calculated field must also be defined with the parameter (ROWID), f.ex like this:
  
  ";+OrderValue|DECIMAL|->><>>><>>9.99|order_value.p(ROWID)|Order value"

  The output parameter value can also be used to control the returned result my returning "skiprow" 
  if the calculation doesn't meet f.ex an order value limit
  (Remember to use a delimiter different from ,;| to avoid messing up the query string)
------------------------------------------------------------------------------------------------------*/  
DEF INPUT PARAM  irBuffer    AS ROWID NO-UNDO.
DEF INPUT PARAM  icParam     AS CHAR  NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocValue     AS CHAR NO-UNDO.

