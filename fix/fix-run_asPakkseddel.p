DEF VAR bOk AS LOG NO-UNDO.
DEF VAR cReturn AS CHAR NO-UNDO.
DEF VAR cPkSdlNr AS CHAR NO-UNDO.

ASSIGN 
    cPkSdlNr = '200487'
    .

FIND LAST PkSdlHode NO-LOCK WHERE 
    PkSdlHode.PkSdlNr = cPkSdlNr NO-ERROR.

IF NOT AVAILABLE PkSdlhode THEN
DO:
    MESSAGE 'Ukjent pakkseddel'
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
    RETURN.
END.
ELSE 
    FIND FIRST PkSdlLinje OF PkSdlhode NO-ERROR.
IF NOT AVAILABLE PkSdlLinje THEN
DO:
    MESSAGE 'Pakkseddel uten linjer'
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
    RETURN.
END.

RUN asPakkseddel.p(
    PkSdlLinje.butikkNr,
    cPkSdlNr,
    NO,
    NO,
    OUTPUT bOk,
    OUTPUT cReturn
    ).
MESSAGE bOk cReturn
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
