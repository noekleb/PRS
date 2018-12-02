FOR EACH StHode NO-LOCK WHERE
    StHode.StTypeId = "AVDELING":

    PAUSE 0.
    DISPLAY 
        StHode
        .
    FOR EACH StLinje OF StHode EXCLUSIVE-LOCK:
        ASSIGN
            StLinje.DataObjekt = string(INT(StLinje.DataObjekt),"9999")
            .
        PAUSE 0.
        DISPLAY
            StLinje.StTypeId
            StLinje.DataObjekt
            .
    END.
END.
FOR EACH StHode NO-LOCK WHERE
    StHode.StTypeId = "HOVEDGR":

    PAUSE 0.
    DISPLAY 
        StHode
        .
    FOR EACH StLinje OF StHode EXCLUSIVE-LOCK:
        ASSIGN
            StLinje.DataObjekt = string(INT(StLinje.DataObjekt),"9999")
            .
        PAUSE 0.
        DISPLAY
            StLinje.StTypeId
            StLinje.DataObjekt
            .
    END.
END.
FOR EACH StHode NO-LOCK WHERE
    StHode.StTypeId = "VAREGR":

    PAUSE 0.
    DISPLAY 
        StHode
        .
    FOR EACH StLinje OF StHode EXCLUSIVE-LOCK:
        ASSIGN
            StLinje.DataObjekt = string(INT(StLinje.DataObjekt),"999999")
            .
        PAUSE 0.
        DISPLAY
            StLinje.StTypeId
            StLinje.DataObjekt
            .
    END.
END.
