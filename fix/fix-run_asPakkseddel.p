DEF VAR bOk AS LOG NO-UNDO.
DEF VAR cReturn AS CHAR NO-UNDO.
DEF VAR lPkSdlId AS DEC NO-UNDO.
DEF VAR cPkSdlNr AS CHAR NO-UNDO.
DEF VAR ibutNr AS INT NO-UNDO.

ASSIGN 
    lPkSdlId = 100010
    ibutNr   = 10100
    .

FIND PkSdlHode NO-LOCK WHERE 
    PkSdlId = lPkSdlId NO-ERROR.

IF AVAILABLE PkSdlHode THEN 
DO:
    FIND FIRST PkSdlLinje OF PkSdlHode NO-LOCK.
    IF NOT AVAILABLE PkSdlLinje THEN
        LEAVE.
    RUN asPakkseddel.p(
        PkSdlLinje.ButikkNr,
        PkSdlHode.PkSdlNr,
        YES,
        YES,  /* bSkrivEtikett */
        OUTPUT bOk,
        OUTPUT cReturn
        ).

    MESSAGE bOk cReturn
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.
