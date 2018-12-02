
OUTPUT TO VALUE("ARTStrekkode_Kodetype.csv").
FOR EACH Strekkode WHERE Strekkode.Kodetype = 0:
    IF LENGTH(Strekkode.Kode) = 13 THEN
    DO:
        PUT UNFORMATTED 
            "ART;"
            Strekkode.ArtikkelNr ";"
            Strekkode.StrKode ";"
            Strekkode.Kode ";"
            Strekkode.KodeType   ";"
            Strekkode.Bestillingsnummer ";" 
            Strekkode.RegistrertDato ";"
            Strekkode.EDato ";"
            SKIP.

        Strekkode.KodeType = 1.
    END.
END.
OUTPUT CLOSE.
OUTPUT TO VALUE("VPIStrekkode_Kodetype.csv").
FOR EACH VPIStrekkode WHERE VPIStrekkode.Kodetype = 0:
    IF LENGTH(VPIStrekkode.Kode) = 13 THEN
    DO:
        PUT UNFORMATTED
            "VPI;"
            VPIStrekkode.VareNr  ";"
            VPIStrekkode.StrKode ";"
            VPIStrekkode.Kode ";"
            VPIStrekkode.KodeType ";"
            VPIStrekkode.Bestillingsnummer ";"
            VPIStrekkode.RegistrertDato ";"
            VPIStrekkode.EDato ";"
            SKIP.

        VPIStrekkode.KodeType = 1.
    END.
END.
OUTPUT CLOSE.
