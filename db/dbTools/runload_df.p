/* loaddf.p */

DEF VAR cDfFile AS CHAR NO-UNDO.

ASSIGN 
     cDfFile = ''
    .

/*run prodict/load_df ( input session:parameter ).*/

run prodict/load_df ( input cDfFile ).

return.
