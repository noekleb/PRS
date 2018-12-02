DEF VAR obOk AS LOG NO-UNDO.
DEF VAR ocReturn AS CHAR NO-UNDO.

run asSjekkOverforing.p (2, 2, output obOk, output ocReturn).

MESSAGE obOk SKIP
    ocReturn
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
