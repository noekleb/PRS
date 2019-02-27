DEF INPUT PARAMETER iCL AS INT NO-UNDO.
DEF OUTPUT PARAMETER dRabSjekkSerieNr AS DEC NO-UNDO.

FIND LAST MedRabSjekk NO-LOCK WHERE
    MedRabSjekk.ButikkNr = iCL USE-INDEX ButRabSjekkSerienr NO-ERROR.
IF AVAILABLE MedRabSjekk THEN
    dRabSjekkSerieNr = MedRabSjekk.RabSjekkSerieNr + 1.
ELSE
    dRabSjekkSerieNr = 1.
