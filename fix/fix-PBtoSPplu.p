DEFINE VARIABLE cReftekst AS CHARACTER  NO-UNDO.
DEFINE TEMP-TABLE TT_MdPLU NO-UNDO
    FIELD plu AS CHAR
    FIELD Beskr AS CHAR
    FIELD SPArtNr AS CHAR
    INDEX plu plu.

FOR EACH TT_MdPLU:
	FIND strekkode WHERE strekkode.kode = SPArtNr NO-LOCK NO-ERROR.
	FIND artbas OF strekkode NO-LOCK NO-ERROR.
    FIND VarGr OF artbas NO-LOCK NO-ERROR.
    FIND HuvGr OF Artbas NO-LOCK NO-ERROR.
	FOR EACH bonglinje WHERE bonglinje.strekkode  = TT_MdPLU.plu AND bonglinje.kassenr = 11 AND 
                             YEAR(bonglinje.dato) = 2004         AND bonglinje.ttid < 13    AND 
                             bonglinje.reftekst = "":
        ASSIGN cReftekst = Bonglinje.strekkode + CHR(1) + bonglinje.bongtekst + CHR(1) + 
                          STRING(bonglinje.varegr) + CHR(1) + bonglinje.artikkelnr + CHR(1) + STRING(bonglinje.hovedgr).
		ASSIGN bonglinje.artikkelnr = STRING(artbas.artikkelnr)
               bonglinje.strekkode  = TT_MdPLU.SPArtnr
		       bonglinje.bongtekst  = TT_MdPLU.Beskr
		       bonglinje.VareGr     = artbas.vg
               BongLinje.VareGruppeNavn     = IF AVAIL VarGr THEN VarGr.vgbeskr ELSE ""
               BongLinje.HovedGr            = artbas.Hg
               BongLinje.HovedGrBeskrivelse = IF AVAIL HuvGr THEN HuvGr.hgbeskr ELSE ""
               bonglinje.reftekst   = cReftekst.
	END.
END.
