TRIGGER PROCEDURE FOR DELETE OF VPIArtBas.

/* FIND ELogg WHERE ELogg.TabellNavn = "vpiArtBas" AND                                                      */
/*              ELogg.EksterntSystem = "POS"    AND                                                         */
/*              ELogg.Verdier        = STRING(vpiArtBas.EkstVPILevNr) + CHR(1) + vpiArtBas.VareNr NO-ERROR. */
/* IF NOT AVAIL Elogg THEN DO:                                                                              */
/*     CREATE Elogg.                                                                                        */
/*     ASSIGN ELogg.TabellNavn = "vpiArtBas"                                                                */
/*              ELogg.EksterntSystem = "POS"                                                                */
/*              ELogg.Verdier        = STRING(vpiArtBas.EkstVPILevNr) + CHR(1) + vpiArtBas.VareNr NO-ERROR. */
/* END.                                                                                                     */
/* ASSIGN ELogg.EndringsType = 3                                                                            */
/*        ELogg.Behandlet    = FALSE.                                                                       */
/* RELEASE ELogg.                                                                                           */

/* Her sletter vi alle relaterte poster. */
FOR EACH VPIAltLevBas EXCLUSIVE-LOCK WHERE 
    VPIAltLevBas.EkstVPILevNr = VPIArtBas.EkstVPILevNr AND
    VPIAltLevBas.VareNr       = VPIArtBas.VareNr:
    DELETE VPIAltLevBas.
END.
FOR EACH VPIArtBestPkt EXCLUSIVE-LOCK WHERE 
    VPIArtBestPkt.EkstVPILevNr = VPIArtBas.EkstVPILevNr AND
    VPIArtBestPkt.VareNr       = VPIArtBas.VareNr:
    DELETE VPIArtBestPkt.
END.
FOR EACH VPIArtPris EXCLUSIVE-LOCK WHERE 
    VPIArtPris.EkstVPILevNr = VPIArtBas.EkstVPILevNr AND
    VPIArtPris.VareNr       = VPIArtBas.VareNr:
    DELETE VPIArtPris.
END.
FOR EACH VPIBildeData EXCLUSIVE-LOCK WHERE 
    VPIBildeData.EkstVPILevNr = VPIArtBas.EkstVPILevNr AND
    VPIBildeData.VareNr       = VPIArtBas.VareNr:
    DELETE VPIBildeData.
END.

FOR EACH VPIBilderegister EXCLUSIVE-LOCK WHERE 
    VPIBilderegister.EkstVPILevNr = VPIArtBas.EkstVPILevNr AND
    VPIBilderegister.VareNr       = VPIArtBas.VareNr:
    DELETE VPIBilderegister.
END.

FOR EACH VPIErstattningsvare EXCLUSIVE-LOCK WHERE 
    VPIErstattningsvare.EkstVPILevNr = VPIArtBas.EkstVPILevNr AND
    VPIErstattningsvare.VareNr       = VPIArtBas.VareNr:
    DELETE VPIErstattningsvare.
END.

FOR EACH VPIPakkeLinje EXCLUSIVE-LOCK WHERE 
    VPIPakkeLinje.EkstVPILevNr = VPIArtBas.EkstVPILevNr AND
    VPIPakkeLinje.VareNr       = VPIArtBas.VareNr:
    DELETE VPIPakkeLinje.
END.

FOR EACH VPIStrekkode EXCLUSIVE-LOCK WHERE 
    VPIStrekkode.EkstVPILevNr = VPIArtBas.EkstVPILevNr AND
    VPIStrekkode.VareNr       = VPIArtBas.VareNr:
    DELETE VPIStrekkode.
END.

FOR EACH VPIArtBasKarakteristikk OF VPIArtBas:
    DELETE VPIArtBasKarakteristikk.
END. 