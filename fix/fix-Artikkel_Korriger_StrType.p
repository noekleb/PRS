DEF VAR iStrTypeID AS INT NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS CLASS cls.StdFunk.StandardFunksjoner NO-UNDO.
rStandardFunksjoner = NEW cls.StdFunk.StandardFunksjoner( ).

FOR EACH ArtBas WHERE 
    NOT CAN-FIND(StrType OF ArtBas):

    rStandardFunksjoner:setStdStrType(ArtBas.ArtikkelNr).

    IF NOT CAN-FIND(StrType OF ArtBas) THEN
    DO:
        iStrTypeID = 0.
        RUN setStorrelsestype.p (ArtBas.ArtikkelNr, 0, TRUE, OUTPUT iStrTypeId).
    END.
    
    /*
    IF ArtBas.StrTypeId < 930 OR ArtBas.StrTypeId > 999 THEN
    DO:
        display
            ArtBas.ArtikkelNr
            ArtBas.StrTypeId
            .
        UPDATE ArtBas.ArtikkelNr.
    END.
    */
END.
