OUTPUT TO vpialtle.d.
FOR EACH VPIAltLevBas NO-LOCK WHERE VPIAltLevBas.EkstVPILevNr = 1:
    EXPORT VPIAltLevBas.
END.
OUTPUT CLOSE.

OUTPUT TO vpiartba.d.
FOR EACH VPIArtBas NO-LOCK WHERE VPIArtBas.EkstVPILevNr = 1:
    EXPORT VPIArtBas.
END.
OUTPUT CLOSE.

OUTPUT TO artbestp.d.
FOR EACH VPIArtBestPkt NO-LOCK WHERE VPIArtBestPkt.EkstVPILevNr = 1:
    EXPORT VPIArtBestPkt.
END.
OUTPUT CLOSE.

OUTPUT TO vpiartpr.d.
FOR EACH VPIArtPris NO-LOCK WHERE VPIArtPris.EkstVPILevNr = 1:
    EXPORT VPIArtPris.
END.
OUTPUT CLOSE.

OUTPUT TO vpibldda.d.
FOR EACH VPIBildeData NO-LOCK WHERE VPIBildeData.EkstVPILevNr = 1:
    EXPORT VPIBildeData.
END.
OUTPUT CLOSE.

OUTPUT TO vpibldre.d.
FOR EACH VPIBilderegister NO-LOCK WHERE VPIBilderegister.EkstVPILevNr = 1:
    EXPORT VPIBilderegister.
END.
OUTPUT CLOSE.

OUTPUT TO vpidatas.d.
FOR EACH VPIDatasett NO-LOCK WHERE VPIDatasett.EkstVPILevNr = 1:
    EXPORT VPIDatasett.
END.
OUTPUT CLOSE.

OUTPUT TO vpiersta.d.
FOR EACH VPIErstattningsvare NO-LOCK WHERE VPIErstattningsvare.EkstVPILevNr = 1:
    EXPORT VPIErstattningsvare.
END.
OUTPUT CLOSE.

OUTPUT TO vpipakln.d.
FOR EACH VPIPakkeLinje NO-LOCK WHERE VPIPakkeLinje.EkstVPILevNr = 1:
    EXPORT VPIPakkeLinje.
END.
OUTPUT CLOSE.

OUTPUT TO vpistrek.d.
FOR EACH VPIStrekkode NO-LOCK WHERE VPIStrekkode.EkstVPILevNr = 1:
    EXPORT VPIStrekkode.
END.
OUTPUT CLOSE.

