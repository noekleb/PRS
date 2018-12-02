
DEF VAR cFileName AS CHAR NO-UNDO.

DEF STREAM Ut.

ASSIGN
    cFileName = 'c:\tmp\schema_dump.csv'
    .
CURRENT-WINDOW:WIDTH = 450.

OUTPUT STREAM Ut TO VALUE(cFileName).

PUT STREAM Ut UNFORMATTED
   '_File-Name
    _Field-Name
    _DATA-TYPE
    _Label
    _Mandatory
    _Initial  FORMAT "x(15)"
    _Format FORMAT "x(15)"
    _Decimals
    _Extent
    _HELP FORMAT "x(60)"
    _Desc FORMAT "x(60)"
    SKIP.

FOR EACH _File NO-LOCK,
    EACH _Field OF _file NO-LOCK:
    DISPLAY
        _File._File-Name
        _Field._Field-Name
        _Field._DATA-TYPE
        _Field._Label
        _field._Mandatory
        _Field._Initial  FORMAT "x(15)"
        _Field._Format FORMAT "x(15)"
        _Field._Decimals
        _Field._Extent
        _Field._HELP FORMAT "x(60)"
        _Field._Desc FORMAT "x(60)"
    WITH WIDTH 450.
END.

OUTPUT STREAM Ut CLOSE.
