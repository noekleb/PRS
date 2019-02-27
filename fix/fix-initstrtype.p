FOR EACH StrType WHERE StrTypeid >= 100 AND strTypeid <= 999:
    ASSIGN
        strType.AvdelingNr = int(SUBstring(STRING(StrTypeId,"999"),1,1))
        .
    DISPLAY 
        StrTypeId avdelingnr hg
        .
END.
