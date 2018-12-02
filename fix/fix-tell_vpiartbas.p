DEF VAR X AS INT.
DEF VAR Y AS INT NO-UNDO.
DEF VAR cELNRLst AS CHAR NO-UNDO.

DEF BUFFER bufVPIArtBas FOR VPIArtBas.

FOR EACH EkstVPILev NO-LOCK WHERE
    EkstVPILEv.EkstVPILevNr >= 146 AND
    EkstVPILEv.EkstVPILEvNr <= 146:
     
    /* Skal ikke ha med XONSPORT. */
    IF EkstVPILEv.Kortnavn BEGINS "XON" THEN
        NEXT.
    
    FOR EACH VPIArtBas NO-LOCK WHERE
        VPIArtBas.EkstVPILEvNr = EkstVPILEv.EkstVPILEvNr AND
        VPIArtBas.Sasong = 82 AND
        NOT CAN-FIND(ArtBas WHERE
                     ArtBas.ArtikkelNR = DEC(VPIArtBas.VareNr)):

        IF NOT CAN-DO(cELNRLst,STRING(VPIArtBas.EkstVPILEvNR)) THEN
            cELNRLst = cELNRLst + "," + 
            STRING(VPIArtBas.EkstVPILEvNR).

        X = X + 1.

        IF NOT CAN-FIND(FIRST bufVPIArtBas WHERE
                        bufVPIArtBas.EkstVPILEvNR = 1 AND
                        bufVPIArtBas.VareNr = VPIArtBas.VAreNr) THEN
            Y = Y + 1.

    END.

END.
MESSAGE 
 X Y SKIP
    cELNRLst
VIEW-AS ALERT-BOX INFO BUTTONS OK.

