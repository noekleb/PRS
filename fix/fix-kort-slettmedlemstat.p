FOR EACH StHode WHERE StTypeId = "MEDLEM":
    DISPLAY StHode.
    FOR EACH StLinje OF StHode:
      DELETE StLinje.
    END.
    DELETE StHode.
END.

FOR EACH StHode WHERE StTypeId = "MEDLEMTOT" :
    DISPLAY StHode.
    FOR EACH StLinje OF StHode:
        DELETE StLinje.
    END.
    DELETE StHode.
END.

FOR EACH StHode WHERE StTypeId = "KUNDSTAT" :
    DISPLAY StHode.
    FOR EACH StLinje OF StHode:
        DELETE StLinje.
    END.
    DELETE StHode.
END.
