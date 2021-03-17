CURRENT-WINDOW:WIDTH = 350.
FOR EACH VPIFilHode:
    DISPLAY
        VPIFilHode.EkstVPILevNr FORMAT "->>>>>>9"
        VPIFilHode.FilId
        VPIFilHode.FilNavn FORMAT "x(60)"
        VPIFilHode.VPIFilStatus
    WITH WIDTH 350.
    FOR EACH VPIFillinje OF VPIFilHode:
        DISPLAY
            VPIFilLinje
        WITH WIDTH 350.
    END.
END.
