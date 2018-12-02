  FIELD VPIFilTypeNr LIKE VPIFilType.VPIFilTypeNr VALIDATE ~
  FIELD VPIFilTypeKNavn LIKE VPIFilType.VPIFilTypeKNavn VALIDATE ~
  FIELD VPIFilTypeBeskrivelse LIKE VPIFilType.VPIFilTypeBeskrivelse VALIDATE ~
  FIELD RegistrertDato LIKE VPIFilType.RegistrertDato VALIDATE ~
  FIELD RegistrertTid LIKE VPIFilType.RegistrertTid VALIDATE ~
  FIELD RegistrertAv LIKE VPIFilType.RegistrertAv VALIDATE ~
  FIELD EDato LIKE VPIFilType.EDato VALIDATE ~
  FIELD ETid LIKE VPIFilType.ETid VALIDATE ~
  FIELD BrukerID LIKE VPIFilType.BrukerID VALIDATE ~
  FIELD fuEndretInfo AS CHARACTER FORMAT "x(40)" LABEL "EndretInfo"
