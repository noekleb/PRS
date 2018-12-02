  FIELD EkstVPILevNr LIKE EkstVPIFil.EkstVPILevNr VALIDATE ~
  FIELD VPIFilNr LIKE EkstVPIFil.VPIFilNr VALIDATE ~
  FIELD VPIFilTypeNr LIKE EkstVPIFil.VPIFilTypeNr VALIDATE ~
  FIELD VPIFilAktiv LIKE EkstVPIFil.VPIFilAktiv VALIDATE ~
  FIELD VPIFilNavn LIKE EkstVPIFil.VPIFilNavn VALIDATE ~
  FIELD VPIEkst LIKE EkstVPIFil.VPIEkst VALIDATE ~
  FIELD VPIKatalog LIKE EkstVPIFil.VPIKatalog VALIDATE ~
  FIELD VPIInnlesningsrutine LIKE EkstVPIFil.VPIInnlesningsrutine VALIDATE ~
  FIELD VPIOppdateringsrutine LIKE EkstVPIFil.VPIOppdateringsrutine VALIDATE ~
  FIELD VPIUtpakkingsrutine LIKE EkstVPIFil.VPIUtpakkingsrutine VALIDATE ~
  FIELD BrukerID LIKE EkstVPIFil.BrukerID VALIDATE ~
  FIELD RegistrertAv LIKE EkstVPIFil.RegistrertAv VALIDATE ~
  FIELD RegistrertDato LIKE EkstVPIFil.RegistrertDato VALIDATE ~
  FIELD RegistrertTid LIKE EkstVPIFil.RegistrertTid VALIDATE ~
  FIELD EDato LIKE EkstVPIFil.EDato VALIDATE ~
  FIELD ETid LIKE EkstVPIFil.ETid VALIDATE ~
  FIELD VPIFilBeskrivelse LIKE EkstVPIFil.VPIFilBeskrivelse VALIDATE ~
  FIELD fuEndretInfo AS CHARACTER FORMAT "x(40)" LABEL "EndretInfo"~
  FIELD VPIOperator LIKE EkstVPIFil.VPIOperator VALIDATE ~
  FIELD VPIFilMaske LIKE EkstVPIFil.VPIFilMaske VALIDATE ~
  FIELD fuVPIFilTypeKNavn AS CHARACTER FORMAT "x(12)" LABEL "VPIFilTypeKNavn"
