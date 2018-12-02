TRIGGER PROCEDURE FOR DELETE OF Bilderegister.

    DEF BUFFER trgBildeData FOR BildeData.

    FOR EACH trgBildeData OF BildeRegister:
        DELETE trgBildeData.
    END.


