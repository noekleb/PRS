OUTPUT TO VALUE("undergrupper.sdv").
    EXPORT DELIMITER ";"
        "Varegruppe"
        "Beskrivelse"
        "Kost%"
        "MvaKode"
        "Mva%"
        "AvdelingNr"
        "Beskrivelse"
        "Hovedgruppe"
        "Beskrivelse"
        .

FOR EACH VarGr NO-LOCK:

    FIND Moms OF VarGr NO-LOCK.
    FIND HuvGr OF VarGr NO-LOCK NO-ERROR.
    FIND Avdeling OF HuvGr NO-LOCK.

    EXPORT DELIMITER ";"
        VarGr.Vg FORMAT ">>>>>9"
        VarGr.VgBeskr
        VarGr.Kost_Proc
        VarGr.Momskod
        Moms.MomsProc
        Avdeling.AvdelingNr
        Avdeling.AvdelingNavn
        HuvGr.Hg FORMAT ">>>9"
        HuvGr.HgBeskr
        .
END.
