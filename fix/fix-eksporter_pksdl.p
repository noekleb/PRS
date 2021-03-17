FOR EACH PkSdlHode NO-LOCK WHERE 
    PkSdlHode.EDato >= 01/01/2016:

    OUTPUT TO 'pksdlhode.d' APPEND.
    EXPORT pksdlHode.
    OUTPUT CLOSE.


    OUTPUT TO 'pksdllinje.d' APPEND.
    FOR EACH PkSdlLinje OF pksdlhode:
        EXPORT pksdllinje.
    END.
    OUTPUT CLOSE.

    OUTPUT TO 'pksdlpris.d' APPEND.
    FOR EACH PkSdlPris OF pksdlhode:
        EXPORT pksdlPris.
    END.
    OUTPUT CLOSE.

    OUTPUT TO 'pksdlMot.d' APPEND.
    FOR EACH PkSdlMottak OF pksdlhode:
        EXPORT pksdlMottak.
    END.    
    OUTPUT CLOSE.
END.
