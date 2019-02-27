DEF VAR lPkSdlId       AS DEC       NO-UNDO.
DEFINE VAR iButNr      AS INTEGER   NO-UNDO.
DEFINE VAR cOKButLst   AS CHARACTER NO-UNDO.
DEF VAR cOKButNavn AS CHAR NO-UNDO.
DEFINE VAR lOK         AS LOGICAL   NO-UNDO.
DEFINE VAR cReturn     AS CHARACTER NO-UNDO.
DEF VAR iOrgOvbut AS INT NO-UNDO.

ASSIGN
    ibutNr   = 4
    lPkSdlId = 100002
    .

RUN  asPksdlSjekkOverforing.p (lPkSdlId, iButNr, OUTPUT iOrgOvbut, OUTPUT cOKButLst, OUTPUT cOKButNavn, OUTPUT lOk, OUTPUT cReturn).

MESSAGE 'Butikkliste:' cOkButLst SKIP(1)
    'Navneliste:' cOkbutNavn SKIP(1)
    'Status:' lOk SKIP(1)
    'Melding:' cReturn 
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
