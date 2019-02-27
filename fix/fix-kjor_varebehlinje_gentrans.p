DEF VAR ocReturn AS CHAR NO-UNDO.
DEF VAR obOk AS LOG NO-UNDO.

RUN varebehlinje_gentrans.p ('1720000003,172,1744673',?,'',OUTPUT ocReturn, OUTPUT obOk).

MESSAGE ocReturn SKIP
    obOk
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
