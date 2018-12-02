FOR EACH EkstVPILev WHERE
      EkstVPILev.EkstVPILevNr > 1 AND 
      EkstVPILev.EkstVPILevNr < 800:
    FOR EACH EkstVPIFil OF EkstVPILev:
        DELETE EkstVPIFil.
    END.
    DELETE EkstVPILEv.
END.
FOR EACH VPIDatasett WHERE
    VPIDatasett.EkstVPILevNr > 1 AND
    VPIDatasett.EkstVPILevNr < 800:
    DELETE VPIDatasett.
END.

FOR EACH vpidatasett WHERE NOT CAN-FIND(EkstVPILEv WHERE
                                        EkstVPILev.EkstVPILevNr = VPIDataSett.EkstVPILEvNr AND
                                        EkstVPILev.AktivLev = TRUE):
    DELETE vpidatasett.
END.

