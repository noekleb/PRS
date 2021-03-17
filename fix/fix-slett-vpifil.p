FOR EACH VPIFilHode:
    FOR EACH VPIFilLogg OF VPIFilHode:
        DELETE VPIFilLogg.
    END.
    FOR EACH VPIFilLinje OF VPIFilHode:
        DELETE VPIFilLinje.
    END.
    DELETE VPIFilHode.

END.
