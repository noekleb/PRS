  FIELD BrukerID LIKE OvBunt.BrukerID VALIDATE ~
  FIELD fuEKl AS CHARACTER FORMAT "x(8)" LABEL "Endret kl"~
  FIELD BuntNr LIKE OvBunt.BuntNr VALIDATE ~
  FIELD DatoOppdatert LIKE OvBunt.DatoOppdatert VALIDATE ~
  FIELD EDato LIKE OvBunt.EDato VALIDATE ~
  FIELD fuKlOppdatert AS CHARACTER FORMAT "x(8)" LABEL "Oppdatert kl"~
  FIELD ETid LIKE OvBunt.ETid VALIDATE ~
  FIELD Merknad LIKE OvBunt.Merknad VALIDATE ~
  FIELD OppdatertAv LIKE OvBunt.OppdatertAv VALIDATE ~
  FIELD fuKlRegistrert AS CHARACTER FORMAT "x(8)" LABEL "Reg.kl"~
  FIELD RegistrertAv LIKE OvBunt.RegistrertAv VALIDATE ~
  FIELD RegistrertDato LIKE OvBunt.RegistrertDato VALIDATE  LABEL "Reg.Dato"~
  FIELD RegistrertTid LIKE OvBunt.RegistrertTid VALIDATE ~
  FIELD fuEndretInfo AS CHARACTER FORMAT "x(8)"~
  FIELD TidOppdatert LIKE OvBunt.TidOppdatert VALIDATE ~
  FIELD BatchNr LIKE OvBunt.BatchNr VALIDATE ~
  FIELD BuntStatus LIKE OvBunt.BuntStatus VALIDATE ~
  FIELD opphav LIKE OvBunt.opphav VALIDATE ~
  FIELD Faktura_Id LIKE OvBunt.Faktura_Id VALIDATE ~
  FIELD FakturertAv LIKE OvBunt.FakturertAv VALIDATE ~
  FIELD FakturertDato LIKE OvBunt.FakturertDato VALIDATE ~
  FIELD FakturertTid LIKE OvBunt.FakturertTid VALIDATE ~
  FIELD fuBatchOppdatert AS CHARACTER FORMAT "x(25)" LABEL "Batch oppdatert"~
  FIELD fuFakturaNr AS CHARACTER FORMAT "x(10)" LABEL "Fakturanr"
