DEF VAR cTekst AS CHAR NO-UNDO.

PUBLISH 'infoDisp' ("Korrigerer Størrelsestype.").
FOR EACH StrType:
    ASSIGN
        cTekst = STRING(StrTypeId,"999").
    IF StrTypeId >= 100 AND StrTypeId <= 999 THEN
        ASSIGN
        StrType.AvdelingNr = int(SUBSTRING(cTekst,1,1))
        StrType.Hg = int(SUBSTRING(cTekst,1,2))
        .
END.
