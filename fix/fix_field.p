CURRENT-WINDOW:WIDTH = 200.
FOR EACH _Field WHERE _Field-Name = "EkstVPILevNR":
    FIND _File OF _Field.
    ASSIGN
        _Field._Format = ">>>>>>9"
        .
    DISPLAY
        _Field._field-Name
        _Field._Format
        _File._File-Name
        WITH WIDTH 200.
END.
