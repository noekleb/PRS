DEF VAR obOk AS LOG NO-UNDO.
DEF VAR cOvbutLst AS CHAR NO-UNDO.
DEF VAR ocReturn AS CHAR NO-UNDO.

run asGetOvbutLst.p (2, 2, output obOk, OUTPUT cOvbutLst, output ocReturn).

MESSAGE obOk SKIP
    cOvbutLst SKIP
    ocReturn
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
