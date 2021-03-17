  FIELD EkstVPILevNr LIKE EkstVPILev.EkstVPILevNr VALIDATE ~
  FIELD KortNavn LIKE EkstVPILev.KortNavn VALIDATE  FORMAT "X(15)"~
  FIELD Navn LIKE EkstVPILev.Navn VALIDATE ~
  FIELD EDato LIKE EkstVPILev.EDato VALIDATE ~
  FIELD ETid LIKE EkstVPILev.ETid VALIDATE ~
  FIELD BrukerID LIKE EkstVPILev.BrukerID VALIDATE ~
  FIELD RegistrertAv LIKE EkstVPILev.RegistrertAv VALIDATE ~
  FIELD RegistrertDato LIKE EkstVPILev.RegistrertDato VALIDATE ~
  FIELD RegistrertTid LIKE EkstVPILev.RegistrertTid VALIDATE ~
  FIELD fuEndretInfo AS CHARACTER FORMAT "x(40)" LABEL "EndretInfo"~
  FIELD AktivLev LIKE EkstVPILev.AktivLev VALIDATE ~
  FIELD OppdatMaskeVPI LIKE EkstVPILev.OppdatMaskeVPI VALIDATE ~
  FIELD PrioNr LIKE EkstVPILev.PrioNr VALIDATE ~
  FIELD LevNr LIKE EkstVPILev.LevNr VALIDATE ~
  FIELD EDB-System LIKE EkstVPILev.EDB-System VALIDATE 
