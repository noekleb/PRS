FOR EACH vpistrekkode where EkstVPILevNr = 1:
    kode = TRIM(kode).
END.

FOR EACH vpistrekkode WHERE EkstVPILevNr = 1 and num-entries(kode,".") = 3:
    kode = REPLACE("00" + SUBSTR(kode,3),".","").
END.

FOR EACH vpistrekkode WHERE EkstVPILevNr = 1 and num-entries(kode,".") > 1:
    DELETE vpistrekkode.
END.

FOR EACH vpistrekkode WHERE EkstVPILevNr = 1 and LENGTH(kode) > 13:
    Delete vpistrekkode.
END.

FOR EACH vpistrekkode WHERE EkstVPILevNr = 1 and num-entries(kode," ") = 2:
    kode = REPLACE(kode," ","0").
END.

FOR EACH vpistrekkode WHERE EkstVPILevNr = 1 AND LENGTH(kode) < 13:
    kode = FILL("0",13 - LENGTH(kode)) + kode.
END.

