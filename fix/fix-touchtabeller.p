FOR EACH KAtegori:
  ASSIGN
    KAtegori.Beskrivelse = " " + KAtegori.Beskrivelse.
  ASSIGN
    KAtegori.Beskrivelse = TRIM(KAtegori.Beskrivelse).
END.

FOR EACH VarGr:
  ASSIGN
    VarGr.vgBEskr = " " + VArGr.vgBeskr.
  ASSIGN
    VarGr.vgBEskr = TRIM(VArGr.vgBEskr).
END.
FOR EACH Avdeling:
  ASSIGN
    Avdeling.AvdelingNAvn = " " + Avdeling.AvdelingNAvn.
  ASSIGN
    Avdeling.AvdelingNAvn = TRIM(Avdeling.AvdelingNAvn).
END.

FOR EACH HuvGr:
  ASSIGN
    HuvGr.HgBeskr = " " + HuvGr.HgBeskr.
  ASSIGN
    HuvGr.HgBeskr = TRIM(HuvGr.HgBeskr).
END.

FOR EACH VgKat:
  ASSIGN
    VgKat.Brukerid = " " + VgKat.Brukerid.
  ASSIGN
    VgKat.Brukerid = TRIM(VgKat.Brukerid).
END.

FOR EACH LevBas:
  ASSIGN
    LevBas.LevNAmn = " " + LevBas.LevNamn.
  ASSIGN
    LevBas.LevNAmn = TRIM(LevBas.LevNAmn).
END.
FOR EACH Sasong:
  ASSIGN
    Sasong.SasBeskr = " " + Sasong.SasBeskr
    Sasong.SasBeskr = TRIM(Sasong.SasBeskr).
END.
FOR EACH StrKonv:
  ASSIGN
    StrKonv.Merknad = " " + StrKonv.Merknad
    StrKonv.Merknad = TRIM(StrKonv.Merknad).
END.

FOR EACH Farg:
  ASSIGN
    Farg.FarBEskr = " " + Farg.FarBEskr
    Farg.FarBEskr = TRIM(Farg.FarBEskr).
END.
FOR EACH Material:
  ASSIGN
    Material.MatBeskr = " " + Material.MatBeskr
    Material.MatBeskr = TRIM(Material.MatBeskr).
END.
FOR EACH Klack:
  ASSIGN
    Klack.Beskrivning = " " + Klack.Beskrivning
    Klack.Beskrivning = TRIM(Klack.Beskrivning).
END.
FOR EACH Innersula:
  ASSIGN
    Innersula.InnerBeskr = " " + Innersula.InnerBeskr
    Innersula.InnerBeskr = TRIM(Innersula.InnerBeskr).
END.
FOR EACH Ovandel:
  ASSIGN
    Ovandel.OvBeskr = " " + Ovandel.OvBeskr
    Ovandel.OvBeskr = TRIM(Ovandel.OvBeskr).
END.
FOR EACH Foder:
  ASSIGN
    Foder.Beskrivning = " " + Foder.Beskrivning
    Foder.Beskrivning = TRIM(Foder.Beskrivning).
END.
FOR EACH Slitsula:
  ASSIGN
    Slitsula.SlitBeskr = " " + Slitsula.SlitBeskr
    Slitsula.SlitBeskr = TRIM(Slitsula.SlitBeskr).
END.
FOR EACH Last-Sko:
  ASSIGN
    Last-Sko.LastBeskr = " " + Last-Sko.LastBeskr
    Last-Sko.LastBeskr = TRIM(Last-Sko.LastBeskr).
END.
FOR EACH Anv-Kod:
  ASSIGN
    Anv-Kod.AnvBeskr = " " + Anv-Kod.AnvBeskr
    Anv-Kod.AnvBeskr = TRIM(Anv-Kod.AnvBeskr).
END.
FOR EACH Produsent:
  ASSIGN
    Produsent.BEskrivelse = " " + Produsent.BEskrivelse
    Produsent.BEskrivelse = TRIM(Produsent.BEskrivelse).
END.
FOR EACH Varemerke:
  ASSIGN
    Varemerke.BEskrivelse = " " + Varemerke.BEskrivelse
    Varemerke.BEskrivelse = TRIM(Varemerke.BEskrivelse).
END.
FOR EACH Behandlingskode:
  ASSIGN
    Behandlingskode.BEskrivelse = " " + Behandlingskode.BEskrivelse
    Behandlingskode.BEskrivelse = TRIM(Behandlingskode.BEskrivelse).
END.
FOR EACH Feilkode:
  ASSIGN
    Feilkode.BEskrivelse = " " + Feilkode.BEskrivelse
    Feilkode.BEskrivelse = TRIM(Feilkode.BEskrivelse).
END.
FOR EACH Handtering:
  ASSIGN
    Handtering.BEskrivelse = " " + Handtering.BEskrivelse
    Handtering.BEskrivelse = TRIM(Handtering.BEskrivelse).
END.
FOR EACH Kravkode:
  ASSIGN
    Kravkode.BEskrivelse = " " + Kravkode.BEskrivelse
    Kravkode.BEskrivelse = TRIM(Kravkode.BEskrivelse).
END.
FOR EACH Prisgruppe:
  ASSIGN
    Prisgruppe.BEskrivelse = " " + Prisgruppe.BEskrivelse
    Prisgruppe.BEskrivelse = TRIM(Prisgruppe.BEskrivelse).
END.
FOR EACH Prisprofil:
  ASSIGN
    Prisprofil.BEskrivelse = " " + Prisprofil.BEskrivelse
    Prisprofil.BEskrivelse = TRIM(Prisprofil.BEskrivelse).
END.
FOR EACH Prov:
  ASSIGN
    Prov.ProvBeskr = " " + Prov.ProvBeskr
    Prov.ProvBeskr = TRIM(Prov.ProvBeskr).
END.
FOR EACH Rabatt:
  ASSIGN
    Rabatt.RabBeskr = " " + Rabatt.RabBeskr
    Rabatt.RabBeskr = TRIM(Rabatt.RabBeskr).
END.
FOR EACH Moms:
  ASSIGN
    Moms.BEskrivelse = " " + Moms.BEskrivelse
    Moms.BEskrivelse = TRIM(Moms.BEskrivelse).
END.
FOR EACH Post:
  ASSIGN
    Post.BEskrivelse = " " + Post.BEskrivelse
    Post.BEskrivelse = TRIM(Post.BEskrivelse).
END.
FOR EACH StrType:
  ASSIGN
    StrType.BEskrivelse = " " + StrType.BEskrivelse
    StrType.BEskrivelse = TRIM(StrType.BEskrivelse).
END.
