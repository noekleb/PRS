DEF VAR piEkstVPILEvNR AS INT.

ASSIGN
    piEkstVPILEvNr = 1 /* HK */
    .
PUBLISH 'infoDisp' ("Rens VPI Artbas..").
FOR EACH VPIArtBAs WHERE
    VPIArtBAs.EkstVPILEvNR = piEkstVPILEvNr:
    DELETE VPIArtBAs.
END.

PUBLISH 'infoDisp' ("Rens VPI ArtPris..").
FOR EACH VPIArtPris WHERE
    VPIArtPris.EkstVPILEvNR = piEkstVPILEvNr:
    DELETE VPIArtPris.
END.

PUBLISH 'infoDisp' ("Rens VPI Bildedata..").
FOR EACH VPIBildeData WHERE
    VPIBildeData.EkstVPILEvNR = piEkstVPILEvNr:
    DELETE VPIBildeData.
END.

PUBLISH 'infoDisp' ("Rens VPI Bilderegister..").
FOR EACH VPIBilderegister WHERE
    VPIBilderegister.EkstVPILEvNR = piEkstVPILEvNr:
    DELETE VPIBilderegister.
END.

PUBLISH 'infoDisp' ("Rens VPI Datasett..").
FOR EACH VPIDataSett WHERE
    VPIDatasett.EkstVPILEvNR = piEkstVPILEvNr:
    DELETE VPIDatasett.
END.

PUBLISH 'infoDisp' ("Rens VPI Erstattningsvare..").
FOR EACH VPIErstattningsvare WHERE
    VPIErstattningsvare.EkstVPILEvNR = piEkstVPILEvNr:
    DELETE VPIErstattningsvare.
END.

PUBLISH 'infoDisp' ("Rens VPI Pakke..").
FOR EACH VPIPakkelinje WHERE
    VPIPakkeLinje.EkstVPILEvNR = piEkstVPILEvNr:
    DELETE VPIPakkelinje.
END.

PUBLISH 'infoDisp' ("Rens VPI Strekkode..").
FOR EACH VPIStrekkode WHERE
    VPIStrekkode.EkstVPILEvNR = piEkstVPILEvNr:
    DELETE VPIStrekkode.
END.

