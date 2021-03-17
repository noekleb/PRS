TRIGGER PROCEDURE FOR DELETE OF TransType.

FOR EACH TransBeskr OF TransType:
    DELETE TransBeskr.
END.

