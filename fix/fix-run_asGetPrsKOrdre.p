DEFINE var lcShipping  AS LONGCHAR NO-UNDO.
DEFINE VAR obOk     AS LOG      NO-UNDO.
DEFINE VAR ocReturn AS CHAR     NO-UNDO. 
 
RUN asGetPRSKOrdre.p(OUTPUT lcShipping, OUTPUT obOk, OUTPUT ocReturn).

MESSAGE STRING(lcShipping) SKIP(1)
    obok SKIP
    ocReturn SKIP
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
