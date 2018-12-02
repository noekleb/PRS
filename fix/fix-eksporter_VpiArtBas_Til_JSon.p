DEFINE VARIABLE rPricat AS CLASS cls.vpi.Pricat NO-UNDO.

rPricat = NEW cls.vpi.Pricat().


FOR EACH VPIDatasett WHERE 
    EkstVPILEvNr >= 10000 AND 
    EkstVPILEvNr <= 11999:

    rPricat:eksporterVpiLev( EkstVPILEvNr ).
END.




