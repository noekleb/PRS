DEFINE VARIABLE rPakkseddel AS cls.Pakkseddel.Pakkseddel NO-UNDO. 
rPakkseddel  = NEW cls.Pakkseddel.Pakkseddel() NO-ERROR.

FOR EACH PkSdlHode EXCLUSIVE-LOCK WHERE 
    PkSdlHode.PkSdlStatus = 10 AND 
    PkSdlHode.ButikkNR >= 10100:

    rPakkseddel:pksdlKommisjonOppdatering(  INPUT PkSdlHode.PkSdlId ).

    DISPLAY
        PkSdlHode.ButikkNr
        PkSdlHode.PkSdlNr
        .

END.
