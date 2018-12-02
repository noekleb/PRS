DEFINE VARIABLE iHg    AS INTEGER    NO-UNDO.
DEFINE VARIABLE iAvdnr AS INTEGER    NO-UNDO.
FOR EACH VareHierarki:
    RELEASE huvgr.
    RELEASE avdeling.
    FIND vargr WHERE vargr.vg = VareHierarki.vg NO-LOCK NO-ERROR.
    IF AVAIL vargr THEN
        FIND huvgr OF VarGr NO-LOCK NO-ERROR.
    IF AVAIL huvgr THEN
        FIND avdeling OF huvgr NO-LOCK NO-ERROR.
    ASSIGN iHg    = IF AVAIL HuvGr THEN HuvGr.hg ELSE -1
           iAvdnr = IF AVAIL Avdeling THEN Avdeling.avdelingnr ELSE -1.
    IF NOT AVAIL vargr OR VareHierarki.Avdelingnr <> iAvdNr OR VareHierarki.hg <> iHg THEN
        DELETE VareHierarki.
END.
FOR EACH vargr NO-LOCK:
    FIND Huvgr OF vargr NO-LOCK NO-ERROR.
    IF NOT AVAIL HuvGr THEN
        NEXT.
    FIND avdeling OF HuvGr NO-LOCK NO-ERROR.
    IF NOT AVAIL Avdeling THEN
        NEXT.
    FIND Moms OF VarGr NO-LOCK NO-ERROR.
    FIND VareHierarki WHERE VareHierarki.Avdelingnr = Avdeling.avdelingnr AND
                            VareHierarki.Hg         = HuvGr.Hg            AND
                            VareHierarki.Vg         = VarGr.Vg NO-ERROR.
    IF NOT AVAIL VareHierarki THEN DO:
        CREATE VareHierarki.
        ASSIGN VareHierarki.Avdelingnr = Avdeling.Avdelingnr
               VareHierarki.Hg         = HuvGr.hg
               VareHierarki.Vg         = VarGr.Vg.
    END.
    ASSIGN VareHierarki.AvdelingNavn = Avdeling.AvdelingNavn
           VareHierarki.HgBeskr      = HuvGr.HgBeskr
           VareHierarki.VgBeskr      = VarGr.VgBeskr
           VareHierarki.Kost_proc    = VarGr.Kost_Proc
           VareHierarki.MomsKod      = Moms.MomsKod
           VareHierarki.MomsProc     = Moms.MomsProc
           VareHierarki.Beskrivelse  = Moms.Beskrivelse.
END.
QUIT.
