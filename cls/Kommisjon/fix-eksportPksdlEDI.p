
FOR EACH PkSdlHode EXCLUSIVE-LOCK WHERE 
    PkSdlHode.PkSdlStatus = 10 AND 
    PkSdlHode.ButikkNR >= 10100:

    RUN EDIKommisjon.p ( PkSdlHode.PkSdlId, 'pksdllogg').

    DISPLAY
        PkSdlHode.ButikkNr
        PkSdlHode.PkSdlNr
        .

END.
