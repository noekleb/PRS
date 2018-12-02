DEF INPUT PARAMETER iEkstVPILevNr AS INT  NO-UNDO.
DEF INPUT PARAMETER cVareNr       AS CHAR NO-UNDO.

EAN-KODER:
FOR EACH VPIStrekkode WHERE
    VPIStrekkode.EkstVPILevNR = iEkstVPILevNr AND
    VPISTrekkode.VareNr       = cVareNr:
    
    IF LENGTH(VPIStrekkode.Kode) <> 13 THEN
        NEXT.
    /* TN 28/7-09 Også 02 koder fra HK skal importeres lokalt.     
    IF SUBSTRING(VPIStrekkode.Kode,1,2) = "02" THEN
        NEXT.
    */
    IF NOT CAN-FIND(Strekkode WHERE
                    Strekkode.Kode = VPIStrekkode.Kode) THEN
    DO:
        CREATE Strekkode.
        ASSIGN
            Strekkode.ArtikkelNr        = dec(VPIStrekkode.VareNr) 
            Strekkode.Kode              = VPIStrekkode.Kode
            Strekkode.StrKode           = VPIStrekkode.StrKode
            Strekkode.KodeType          = 1
            Strekkode.BestillingsNummer = VPIStrekkode.Bestillingsnummer
            Strekkode.ERPNr             = VPIStrekkode.ERPNr
            .
    END.

END. /* EAN-KODER */
