
DEF VAR piLoop AS INT NO-UNDO.

FOR EACH VPIArtBas WHERE
    CAN-FIND(ArtBas WHERE
             ArtBas.ARtikkelNr = VPIArtBAs.ArtikkelNr):
    piLoop = piLoop + 1.

    FOR EACH VPIArtPris OF VPIArtBas EXCLUSIVE-LOCK:
        DELETE VPIArtPRis.
    END.

    FOR EACH VPIStrekkode OF VPIArtBas EXCLUSIVE-LOCK:
        DELETE VPIStrekkode.
    END.

    FOR EACH VPIBildeRegister WHERE 
             VPIBildeRegister.EkstVPILevNr = VPIArtBas.EkstVPILevNr AND
             VPIBildeRegister.VareNr       = VPIArtBas.VareNr AND
             VPIBildeRegister.BildNr       = VPIArtBas.BildNr:
        FOR EACH VPIBildeData OF VPIBildeRegister:
            DELETE VPIBildeData.
        END.
        DELETE VPIBildeRegister.
    END.

    FOR EACH VPIErstattningsvare WHERE
        VPIErstattningsvare.EkstVPILevNr = VPIArtBas.EkstVPILevNr AND
        VPIErstattningsvare.VareNr       = VPIArtBas.VareNr AND
        VPIErstattningsvare.ArtikkelNr   = VPIArtBas.ArtikkelNr:
        DELETE VPIErstattningsvare.
    END.

    FOR EACH VPIPakkeLinje OF VPIArtBas:
        DELETE VPIPakkelinje.
    END.

    DELETE VPIArtBas.
END.
DISPLAY piLoop.


