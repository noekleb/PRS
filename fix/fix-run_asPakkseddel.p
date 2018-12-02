DEF VAR bOk AS LOG NO-UNDO.
DEF VAR cReturn AS CHAR NO-UNDO.
RUN asPakkseddel.p(
    9,
    153597,
    FALSE,
    FALSE,
    OUTPUT bOk,
    OUTPUT cReturn
    ).
MESSAGE bOk cReturn
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
