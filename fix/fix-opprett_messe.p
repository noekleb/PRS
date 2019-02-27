DEF VAR lVareBehNr AS DEC NO-UNDO. 

/*
DEFINE INPUT  PARAMETER iMesseType AS INTEGER  NO-UNDO.
DEFINE INPUT  PARAMETER iOpprett   AS LOG      NO-UNDO.
DEFINE OUTPUT PARAMETER dVareBehNr AS DECIMAL  NO-UNDO.
*/
run bibl_AktivSupplering.p (2, TRUE, output lVareBehNr).

IF lVareBehNr <> 0 THEN   DO:
    FIND VareBehHode NO-LOCK WHERE
        VareBehHode.VareBehNr = lVareBehNr NO-ERROR.

    MESSAGE lVareBehNr
        VareBehHode.VareBehNr 
        VareBehHode.MesseNr
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.
ELSE 
    MESSAGE lVareBehNr
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
