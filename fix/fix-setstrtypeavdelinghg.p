FOR EACH StrType:
    IF StrTypeId >= 100 AND
        StrTypeId <= 999 THEN
        ASSIGN
        strType.AvdelingNr = int(SUBstring(STRING(StrTypeId,"999"),1,1))
        strType.Hg         = int(SUBstring(STRING(StrTypeId,"999"),1,2))
        .
    DISPLAY
        StrTypeId
        AvdelingNr
        Hg
        .
END.
