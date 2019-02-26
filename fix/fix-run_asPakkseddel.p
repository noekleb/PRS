DEF VAR bOk AS LOG NO-UNDO.
DEF VAR cReturn AS CHAR NO-UNDO.
RUN asPakkseddel.p(
    10,
    250219160421,
    NO,
    NO,
    OUTPUT bOk,
    OUTPUT cReturn
    ).
MESSAGE bOk cReturn
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
