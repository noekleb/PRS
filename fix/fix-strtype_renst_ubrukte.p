FOR EACH StrType WHERE StrType.StrTypeId > 9999 AND 
    NOT CAN-FIND(FIRST ArtBAs WHERE ArtBAs.StrTypeId = StrType.StrTypeId):

    FOR EACH STrTstr OF StrType:
        DELETE STrTStr.
    END.
    DELETE StrType.
END.
