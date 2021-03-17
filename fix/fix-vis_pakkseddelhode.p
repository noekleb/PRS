CURRENT-WINDOW:WIDTH = 300.

DEF VAR cPkSdlNr AS CHAR NO-UNDO.

ASSIGN
    cPkSdlNr = '16506542'
    .

FOR EACH PkSdlHode NO-LOCK WHERE
    PkSdlHode.PkSdlNr = cPkSdlNr:

    DISPLAY
        PkSdlHode.PkSdlId
        PkSdlHode.PkSdlNr
        PkSdlHode.PkSdlStatus
        PkSdlHode.EDato
        PkSdlHode.SendtDato
        PkSdlHode.RegistrertDato
    WITH WIDTH 300.
END.
