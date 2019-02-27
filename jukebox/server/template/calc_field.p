/* Simple variant of calculated fields with no input param (context) other 
   than the rowid for current buffer.
   To invoke (all v9 datatypes supported):
  + ";+MyCalcField|CHARACTER|x(30)|mycalcfieldproc.p|mylabel"

DEF INPUT PARAM  irBuffer      AS ROWID NO-UNDO.
DEF INPUT PARAM  icSessionId   AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn      AS CHAR NO-UNDO.

FIND Customer WHERE ROWID(Customer) = irBuffer NO-LOCK NO-ERROR.

ocReturn = Customer.Address.
