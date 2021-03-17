DEFINE VAR lcShipping  AS LONGCHAR NO-UNDO.
DEFINE VAR obOk     AS LOG      NO-UNDO.
DEFINE VAR ocReturn AS CHAR     NO-UNDO. 

RUN asGetPRSKOrdre.p (OUTPUT lcShipping, OUTPUT obOk, OUTPUT ocReturn).

MESSAGE obOk SKIP
    ocReturn SKIP
    STRING(lcShipping)
    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
