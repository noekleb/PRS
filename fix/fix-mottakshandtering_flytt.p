FOR EACH PkSdlHode NO-LOCK WHERE
    PkSdlHode.PkSdlStatus = 10 /* NY */ AND
    CAN-FIND(FIRST Pksdllinje OF pksdlhode WHERE 
             pksdllinje.butikknr = 1):

    DISPLAY
        PkSdlHode.PkSdlId.

    FOR EACH pksdllinje OF Pksdlhode:
        butikknr = 11.
    END.
END.
